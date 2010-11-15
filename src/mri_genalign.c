#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#undef  BIGVAL
#define BIGVAL 1.e+38

/* is a voxel 'good' to use? */

#undef  GOOD
#define GOOD(i) (mask==NULL || mask[i])

/* mark for a good setup */

#undef  SMAGIC
#define SMAGIC 208921148  /* Zip+4 Code for AFNI Group at NIMH */

/* global access to setup parameters */

static GA_setup *gstup = NULL ;
static GA_setup *gstup_bk = NULL ;

float GA_get_warped_overlap_fraction(void) ; /* prototype */

/*---------------------------------------------------------------------------*/
static int mverb = 0 ;
void mri_genalign_verbose(int v){ mverb = v ; }

/*---------------------------------------------------------------------------*/
/* 27 Aug 2008: replace use of drand48 with myunif, for inter-system control */

static const unsigned long long MYa=62003 ;
static const unsigned long long MYb=15485863 ;
static       unsigned long long MYx=15482917 ;
INLINE float myunif(void)
{
  MYx = MYa * MYx + MYb ;
  return ( ((unsigned int)MYx) / 4294967296.0f ) ;
}
static void myunif_reset(unsigned long long x){ MYx = x; return; }

/*---------------------------------------------------------------------------*/

/* Macro to periodically reduce a variable into the range 0..1:
   for example: PRED01(1.2) == 0.8, PRED01(1.8) == 0.2, et cetera;
   graphically
               PRED01(x)|
                        | /\      /\      /\      /\      /\
                        |/  \    /  \    /  \    /  \    /
                        |    \  /    \  /    \  /    \  /
                        |     \/      \/      \/      \/
                        +------------------------------------> x
                          -3  -2  -1   0  +1  +2  +3  +4  +5
*/

#undef  PRED01
#define PRED01(x) fabsf( (x) - 2.0f*floorf(0.5f*((x)+1.0f)) )

/* Max number of points to warp at a time */

#undef  NPER
#define NPER 262144  /* 1 Mbyte per float array */

static int nperval = NPER ;
void GA_set_nperval( int i ){ nperval = (i > 666) ? i : 16777216 ; }

/*--------------------------------------------------------------------*/
/*! Interpolate target image to control points in base image space.
    - Results go into avm, which must be pre-allocated.
    - If mpar==NULL, then warp parameters are all taken from gstup and
      the results are calculated at ALL points in the base image.
----------------------------------------------------------------------*/

void GA_get_warped_values( int nmpar , double *mpar , float *avm )
{
   int    npar, ii,jj,kk,qq,pp,npp,mm,nx,ny,nxy, clip=0, npt, nall, nper ;
   float *wpar, v ;
   float *imf=NULL, *jmf=NULL, *kmf=NULL ;
   float *imw, *jmw, *kmw ;
   MRI_IMAGE *aim ;

ENTRY("GA_get_warped_values") ;

   npar = gstup->wfunc_numpar ;
   wpar = (float *)calloc(sizeof(float),npar) ;
   nper = MAX(nperval,NPER) ;

   /* load ALL the warping parameters, including the fixed values */

   if( mpar != NULL ){                              /* load from input array */
     for( ii=pp=0 ; ii < npar ; ii++ ){
       if( gstup->wfunc_param[ii].fixed ){                    /* fixed param */
         wpar[ii] = gstup->wfunc_param[ii].val_fixed ;
       } else {                                            /* variable param */
         v = (float)mpar[pp++] ;
         wpar[ii] = gstup->wfunc_param[ii].min        /* scale to true range */
                   +gstup->wfunc_param[ii].siz * PRED01(v) ;
       }
     }
   } else {                                     /* load directly from struct */
     for( ii=0 ; ii < gstup->wfunc_numpar ; ii++ )
       wpar[ii] = gstup->wfunc_param[ii].val_out ;
   }

   /* create space for unwarped indexes, if none given */

   if( mpar == NULL || gstup->im == NULL ){
     npt = gstup->bsim->nvox ; nall = MIN(nper,npt) ;
     imf = (float *)calloc(sizeof(float),nall) ;
     jmf = (float *)calloc(sizeof(float),nall) ;
     kmf = (float *)calloc(sizeof(float),nall) ;
   } else {
     npt = gstup->npt_match ; nall = MIN(nper,npt) ;
   }

   /* create space for indexes of warped control points */

   imw = (float *)calloc(sizeof(float),nall) ;
   jmw = (float *)calloc(sizeof(float),nall) ;
   kmw = (float *)calloc(sizeof(float),nall) ;

   nx = gstup->bsim->nx; ny = gstup->bsim->ny; nxy = nx*ny;

   /* send parameters to warping function for its setup */

   gstup->wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /* choose image from which to extract data */

   aim = (gstup->ajims != NULL && mpar != NULL ) ? gstup->ajims /* smoothed */
                                                 : gstup->ajim; /* unsmooth */

   /*--- do (up to) nall points at a time ---*/

   for( pp=0 ; pp < npt ; pp+=nall ){

     npp = MIN( nall , npt-pp ) ;  /* number to do in this iteration */

     if( mpar == NULL || gstup->im == NULL ){  /* do all points */
       for( qq=0 ; qq < npp ; qq++ ){
         mm = pp+qq ;
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         imf[qq] = (float)ii; jmf[qq] = (float)jj; kmf[qq] = (float)kk;
       }
     } else {
       imf = gstup->im->ar + pp ;  /* pointers to control points */
       jmf = gstup->jm->ar + pp ;
       kmf = gstup->km->ar + pp ;
     }

     /****-- warp control points to new locations ---****/
     /**** (warp does index-to-index transformation) ****/

     gstup->wfunc( npar , NULL ,
                   npp  , imf,jmf,kmf , imw,jmw,kmw ) ;

     /* interpolate target image at warped points */

     switch( gstup->interp_code ){
       case MRI_NN:
         GA_interp_NN( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;

       case MRI_LINEAR:
         GA_interp_linear( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;

       case MRI_CUBIC:
         clip = 1 ;
         GA_interp_cubic( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;

       case MRI_VARP1:
         clip = 1 ;
         GA_interp_varp1( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;

       case MRI_WSINC5:
         clip = 1 ;
         GA_interp_wsinc5( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;

       default:        /* for higher order methods not implemented here */
       case MRI_QUINTIC:
         clip = 1 ;
         GA_interp_quintic( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;
     }

   } /* end of loop over matching points */

   /* free the enslaved memory */

   free((void *)kmw); free((void *)jmw); free((void *)imw);
   if( mpar == NULL || gstup->im == NULL ){
     free((void *)kmf); free((void *)jmf); free((void *)imf);
   }
   free((void *)wpar) ;

   /* clip interpolated values to range of target image, if need be */

   if( clip ){
     float bb=gstup->ajbot , tt=gstup->ajtop ;
     for( pp=0 ; pp < npt ; pp++ )
            if( avm[pp] < bb ) avm[pp] = bb ;
       else if( avm[pp] > tt ) avm[pp] = tt ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Stuff for calling a user-supplied function every time the cost
   function is smaller than the previous minimal value (vbest).
   GA_fitter_dotter() is a simple example.
-----------------------------------------------------------------------------*/

static float fit_vbest = BIGVAL ;
static void (*fit_callback)(int,double *) = NULL ;

void GA_reset_fit_callback( void (*fc)(int,double*) )  /* user func is fc */
{
   fit_vbest = BIGVAL ; fit_callback = fc ; return ;
}

void GA_fitter_dotter(int n, double *mpar){ printf("."); fflush(stdout); }

void GA_do_dots(int x){ GA_reset_fit_callback( (x)?GA_fitter_dotter:NULL ); }

/*---------------------------------------------------------------------------*/
void GA_fitter_coster(int n, double *mpar){
  printf(" + Cost=%g\r",fit_vbest); fflush(stdout);
}
void GA_fitter_coster_tab(int n, double *mpar){
  printf(" + Cost=%g\t",fit_vbest); fflush(stdout);
}
void GA_do_cost(int x, byte use_tab){
   if (use_tab) {
      GA_reset_fit_callback( (x)?GA_fitter_coster_tab:NULL );
   } else {
      GA_reset_fit_callback( (x)?GA_fitter_coster:NULL );
   }
}

void GA_fitter_params( int n , double *mpar )
{
  int ii , pp ; double wpar , v ; int npar = gstup->wfunc_numpar ;
  printf(" + Cost=%g Param=",fit_vbest) ;
  for( ii=pp=0 ; ii < npar ; ii++ ){
    if( !gstup->wfunc_param[ii].fixed ){  /* variable param */
      v = mpar[pp++] ;
      wpar = gstup->wfunc_param[ii].min        /* scale to true range */
            +gstup->wfunc_param[ii].siz * PRED01(v) ;
      printf("%g ",wpar) ;
    }
  }
  printf("\n") ; fflush(stdout) ;
}
void GA_do_params( int x ){
   GA_reset_fit_callback( (x)?GA_fitter_params:NULL );
}

/*---------------------------------------------------------------------------*/

void GA_setup_2Dhistogram( float *xar , float *yar )  /* 08 May 2007 */
{
ENTRY("GA_setup_2Dhistogram") ;

   switch( gstup->hist_mode ){

     default:
     case GA_HIST_EQWIDE:
       set_2Dhist_xybin( 0,NULL,NULL ) ;
     break ;

     case GA_HIST_CLEQWD:{
       int nbin=(int)gstup->hist_param , npt=gstup->npt_match ;
       float xbc,xtc , ybc,ytc ;

       if( nbin < 3 ) nbin = 0 ;
       set_2Dhist_hbin( nbin ) ;
       set_2Dhist_xyclip( npt , xar , yar ) ;

       if( mverb > 1 ){
         (void)get_2Dhist_xyclip( &xbc,&xtc , &ybc,&ytc ) ;
         ININFO_message(" - histogram: source clip %g .. %g; base clip %g .. %g",
                        xbc,xtc , ybc,ytc ) ;
         ININFO_message(" - versus source range %g .. %g; base range %g .. %g",
                        gstup->ajbot, gstup->ajclip, gstup->bsbot, gstup->bsclip ) ;
       }
     }
     break ;

     case GA_HIST_EQHIGH:{
       int nbin=(int)gstup->hist_param , npt=gstup->npt_match , ii,dm,mm,nnew ;
       float *xx , *yy ;

       if( npt > 666*nbin ){                /* subsample data to save CPU time */
         dm = GA_find_relprime_fixed( npt ) ;
         mm = 1 ; nnew = (int)(314.1593*nbin) ;
         xx = (float *)malloc(sizeof(float)*nnew) ;
         yy = (float *)malloc(sizeof(float)*nnew) ;
         for( ii=0 ; ii < nnew ; ii++,mm=(mm+dm)%npt ){
           xx[ii] = xar[mm] ; yy[ii] = yar[mm] ;
         }
         npt = nnew ;
       } else {                             /* just use all the data */
         xx = xar ; yy = yar ;
       }

       if( mverb > 1 )
         ININFO_message("- setting up equalized histogram bins with %d pts",npt) ;

       set_2Dhist_xybin_eqhigh( nbin , npt , xx , yy ) ;
       if( xx != xar ){ free(yy); free(xx); }

       if( mverb > 1 ){
         nbin = get_2Dhist_xybin( &xx , &yy ) ;
         ININFO_message("-- %d equalized histogram bins for source follow:",nbin) ;
         fprintf(stderr,"    ") ;
         for( ii=0 ; ii <= nbin ; ii++ ) fprintf(stderr," %g",xx[ii]) ;
         fprintf(stderr,"\n") ;
         ININFO_message("-- %d equalized histogram bins for base follow:",nbin) ;
         fprintf(stderr,"    ") ;
         for( ii=0 ; ii <= nbin ; ii++ ) fprintf(stderr," %g",yy[ii]) ;
         fprintf(stderr,"\n") ;
       }
     }
     break ;
   }

   gstup->need_hist_setup = 0 ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/

#undef  CMAX
#define CMAX 0.9999f

static int lpczz = 0 ;
void GA_pearson_ignore_zero_voxels(int z){ lpczz = z; }  /* 23 Feb 2010 */

/*---------------------------------------------------------------------*/
/*! LPC = Local Pearson Correlation, as described in the famous paper. */

float GA_pearson_local( int npt , float *avm, float *bvm, float *wvm )
{
   GA_BLOK_set *gbs ;
   int nblok , nelm , *elm , dd , ii,jj , nm ;
   float xv,yv,xy,xm,ym,vv,ww,ws,wss , pcor , wt , psum=0.0f ;
   static int uwb=-1 , wsold=0 ;  /* flags set by the environment */

ENTRY("GA_pearson_local") ;

   if( gstup->blokset == NULL ){
     float rad=gstup->blokrad , mrad ; float *ima=NULL,*jma=NULL,*kma=NULL ;
     if( gstup->smooth_code > 0 && gstup->smooth_radius_base > 0.0f )
       rad = sqrt( rad*rad +SQR(gstup->smooth_radius_base) ) ;
     mrad = 1.2345f*(gstup->base_di + gstup->base_dj + gstup->base_dk) ;
     rad  = MAX(rad,mrad) ;
     if( gstup->im != NULL ) ima = gstup->im->ar ;  /* 19 Feb 2010: oopsie */
     if( gstup->jm != NULL ) jma = gstup->jm->ar ;
     if( gstup->km != NULL ) kma = gstup->km->ar ;
     gstup->blokset = create_GA_BLOK_set(  /* cf. mri_genalign_util.c */
                            gstup->bsim->nx, gstup->bsim->ny, gstup->bsim->nz,
                            gstup->base_di , gstup->base_dj , gstup->base_dk ,
                            gstup->npt_match , ima,jma,kma ,
                            gstup->bloktype , rad , gstup->blokmin , 1.0f,mverb ) ;
     if( gstup->blokset == NULL )
       ERROR_exit("Can't create GA_BLOK_set?!?") ;
   }

   gbs   = gstup->blokset ;
   nblok = gbs->num ;
   if( nblok < 1 ) ERROR_exit("LPC: Bad GA_BLOK_set?!") ;

   if( uwb < 0 ){
     uwb   = AFNI_yesenv("AFNI_LPC_UNWTBLOK") ;  /* first time in */
     wsold = AFNI_yesenv("AFNI_LPC_OLDWSUM") ;   /* 02 Mar 2010 */
   }

   for( wss=0.0f,dd=0 ; dd < nblok ; dd++ ){
     nelm = gbs->nelm[dd] ; if( nelm < 9 ) continue ;  /* skip this blok */
     elm = gbs->elm[dd] ;

     if( wvm == NULL ){   /*** unweighted correlation ***/
       xv=yv=xy=xm=ym=0.0f ; ws = 1.0f ;
       for( ii=0 ; ii < nelm ; ii++ ){
         jj = elm[ii] ;
         xm += avm[jj] ; ym += bvm[jj] ;
       }
       xm /= nelm ; ym /= nelm ;
       for( ii=0 ; ii < nelm ; ii++ ){
         jj = elm[ii] ;
         vv = avm[jj]-xm ; ww = bvm[jj]-ym ;
         xv += vv*vv ; yv += ww*ww ; xy += vv*ww ;
       }

     } else {             /*** weighted correlation ***/
       xv=yv=xy=xm=ym=ws=0.0f ;
       for( ii=0 ; ii < nelm ; ii++ ){
         jj = elm[ii] ;
         wt = wvm[jj] ; ws += wt ;
         xm += avm[jj]*wt ; ym += bvm[jj]*wt ;
       }
       xm /= ws ; ym /= ws ;
       for( ii=0 ; ii < nelm ; ii++ ){
         jj = elm[ii] ;
         wt = wvm[jj] ; vv = avm[jj]-xm ; ww = bvm[jj]-ym ;
         xv += wt*vv*vv ; yv += wt*ww*ww ; xy += wt*vv*ww ;
       }
       if( uwb ) ws = 1.0f ;
     }
     if( wsold ) wss += ws ;                /* the olden way was to add first */

     /*** massage results to get final stretched correlation ***/

     if( xv <= 0.0f || yv <= 0.0f ) continue ;      /* skip this blok */
     pcor = xy/sqrtf(xv*yv) ;                       /* correlation */
          if( pcor >  CMAX ) pcor =  CMAX ;         /* limit the range */
     else if( pcor < -CMAX ) pcor = -CMAX ;
     pcor = logf( (1.0f+pcor)/(1.0f-pcor) ) ;       /* 2*arctanh() */
     psum += ws * pcor * fabsf(pcor) ;              /* emphasize large values */
     if( !wsold ) wss += ws ;                       /* moved here 02 Mar 2010 */
   }

   RETURN(0.25f*psum/wss);      /* averaged stretched emphasized correlation */
}                                /* [0.25 to compensate for the 2*arctanh()] */

/*------------------------------------------------------------*/
/*! LSC = Local Spearman Correlation (not described anywhere) */

float GA_spearman_local( int npt , float *avm, float *bvm, float *wvm )
{
   GA_BLOK_set *gbs ;
   int nblok , nelm , *elm , dd , ii,jj ;
   float wss , pcor , psum=0.0f ;
   float *xt=NULL , *yt=NULL ; int nxt=0 ;

   if( gstup->blokset == NULL ){
     float rad=gstup->blokrad , mrad ; float *ima=NULL,*jma=NULL,*kma=NULL ;
     if( gstup->smooth_code > 0 && gstup->smooth_radius_base > 0.0f )
       rad = sqrt( rad*rad +SQR(gstup->smooth_radius_base) ) ;
     mrad = 1.2345f*(gstup->base_di + gstup->base_dj + gstup->base_dk) ;
     rad  = MAX(rad,mrad) ;
     if( gstup->im != NULL ) ima = gstup->im->ar ;  /* 19 Feb 2010: oopsie */
     if( gstup->jm != NULL ) jma = gstup->jm->ar ;
     if( gstup->km != NULL ) kma = gstup->km->ar ;
     gstup->blokset = create_GA_BLOK_set(  /* cf. mri_genalign_util.c */
                            gstup->bsim->nx, gstup->bsim->ny, gstup->bsim->nz,
                            gstup->base_di , gstup->base_dj , gstup->base_dk ,
                            gstup->npt_match , ima,jma,kma ,
                            gstup->bloktype , rad , gstup->blokmin , 1.0f,mverb ) ;
     if( gstup->blokset == NULL )
       ERROR_exit("Can't create GA_BLOK_set?!?") ;
   }

   gbs   = gstup->blokset ;
   nblok = gbs->num ;
   if( nblok < 1 ) ERROR_exit("LPC: Bad GA_BLOK_set?!") ;

   for( wss=0.0001f,dd=0 ; dd < nblok ; dd++ ){
     nelm = gbs->nelm[dd] ; if( nelm < 9 ) continue ;
     elm = gbs->elm[dd] ;
     if( nelm > nxt ){
       xt = (float *)realloc(xt,sizeof(float)*nelm) ;
       yt = (float *)realloc(yt,sizeof(float)*nelm) ; nxt = nelm ;
     }
     for( ii=0 ; ii < nelm ; ii++ ){
       jj = elm[ii] ; xt[ii] = avm[jj] ; yt[ii] = bvm[jj] ;
     }
     pcor = THD_spearman_corr( nelm , xt , yt ) ;
     pcor = 2.0f * sinf( 0.523599f * pcor ) ;  /* 0.523599 = PI/6 */
     if( pcor > CMAX ) pcor = CMAX; else if( pcor < -CMAX ) pcor = -CMAX;
     pcor = logf( (1.0f+pcor)/(1.0f-pcor) ) ;  /* 2*arctanh() */
     psum += pcor * fabsf(pcor) ;              /* emphasize large values */
     wss++ ;
   }
   if( xt != NULL ){ free(yt) ; free(xt) ; }

   return (0.25f*psum/wss);      /* averaged stretched emphasized correlation */
}                                /* [0.25 to compensate for the 2*arctanh()] */

/*---------------------------------------------------------------------------*/

static double micho_mi  = 0.1 ;  /* 24 Feb 2010 */
static double micho_nmi = 0.1 ;
static double micho_crA = 0.5 ;
static double micho_hel = 0.3 ;
static double micho_ov  = 0.0 ;

void GA_setup_micho( double a, double b, double c, double d, double e )
{
   micho_hel = a; micho_mi = b; micho_nmi = c; micho_crA = d; micho_ov = e;
}

/*---------------------------------------------------------------------------*/
/*! Compute a particular fit cost function
    - avm = target image values warped to base
    - bvm = base image values
    - wvm = weight image values
-----------------------------------------------------------------------------*/

double GA_scalar_costfun( int meth , int npt ,
                          float *avm , float *bvm , float *wvm )
{
  double val=0.0f ;

ENTRY("GA_scalar_costfun") ;

  switch( meth ){

    default:
    case GA_MATCH_PEARSON_SCALAR:   /* Pearson correlation coefficient */
      val = (double)THD_pearson_corr_wt( gstup->npt_match, avm, bvm,wvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_PEARSON_SIGNED:
      val = (double)THD_pearson_corr_wt( gstup->npt_match, avm, bvm,wvm ) ;
    break ;

    case GA_MATCH_PEARSON_LOCALS:  /* LPC */
      val = (double)GA_pearson_local( gstup->npt_match, avm, bvm,wvm ) ;
    break ;

    case GA_MATCH_PEARSON_LOCALA:  /* LPA */
      val = (double)GA_pearson_local( gstup->npt_match, avm, bvm,wvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_NCDZLIB:         /* NCD -- 02 Mar 2009 */
      val = (double)THD_ncdfloat_scl( gstup->npt_match ,
                                      gstup->ajbot , gstup->ajclip , avm ,
                                      gstup->bsbot , gstup->bsclip , bvm  ) ;
    break ;

    case GA_MATCH_SPEARMAN_SCALAR:  /* rank-order (Spearman) correlation */
      val = (double)THD_spearman_corr_nd( gstup->npt_match , avm,bvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_KULLBACK_SCALAR:  /* AKA Mutual Information */
      val = -THD_mutual_info_scl( gstup->npt_match ,
                                  gstup->ajbot , gstup->ajclip , avm ,
                                  gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
    break ;

    case GA_MATCH_NORMUTIN_SCALAR:  /* Normalized Mutual Information */
      val = THD_norm_mutinf_scl( gstup->npt_match ,
                                 gstup->ajbot , gstup->ajclip , avm ,
                                 gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
    break ;

    case GA_MATCH_JOINTENT_SCALAR:  /* Joint Entropy */
      val = THD_jointentrop_scl( gstup->npt_match ,
                                 gstup->ajbot , gstup->ajclip , avm ,
                                 gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
    break ;

    case GA_MATCH_CRAT_USYM_SCALAR: /* Correlation ratio (various flavors) */
    case GA_MATCH_CRAT_SADD_SCALAR:
    case GA_MATCH_CORRATIO_SCALAR:
           if( meth==GA_MATCH_CRAT_USYM_SCALAR )THD_corr_ratio_sym_not;
      else if( meth==GA_MATCH_CRAT_SADD_SCALAR )THD_corr_ratio_sym_add;
      else                                      THD_corr_ratio_sym_mul;

#if 0
      if( mverb > 8 )
       INFO_message("THD_corr_ratio_scl(%d,%g,%g,bvm,%g,%g,bvm,wvm)",
                     gstup->npt_match ,
                     gstup->bsbot , gstup->bsclip ,
                     gstup->ajbot , gstup->ajclip  ) ;
#endif

      val = THD_corr_ratio_scl( gstup->npt_match ,
                                gstup->bsbot , gstup->bsclip , bvm ,
                                gstup->ajbot , gstup->ajclip , avm , wvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_HELLINGER_SCALAR: /* Hellinger metric */
      val = -THD_hellinger_scl( gstup->npt_match ,
                                gstup->ajbot , gstup->ajclip , avm ,
                                gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
    break ;

    /* 24 Feb 2010: a combined method, using multiple functionals */

    case GA_MATCH_LPC_MICHO_SCALAR:{
      val = (double)GA_pearson_local( gstup->npt_match, avm, bvm,wvm ) ;

      if( micho_hel != 0.0 || micho_mi  != 0.0 ||
          micho_nmi != 0.0 || micho_crA != 0.0   ){
        float_quad hmc ; float ovv ;
        hmc = THD_helmicra_scl( gstup->npt_match ,
                                gstup->ajbot , gstup->ajclip , avm ,
                                gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
        val += -micho_hel * hmc.a - micho_mi  * hmc.b
               +micho_nmi * hmc.c + micho_crA * (1.0-fabs(hmc.d)) ;

        if( micho_ov != 0.0 && gstup->bsmask != NULL && gstup->ajmask != NULL ){
          ovv = GA_get_warped_overlap_fraction() ;
          ovv = MAX(0.0f,9.0f-10.0f*ovv) ;
          val += micho_ov * ovv*ovv ;
        }
      }
    }
    break ;
  }

  if( !finite(val) ) val = BIGVAL ;
  RETURN( val );
}

/*---------------------------------------------------------------------------*/
/*! Fit metric for matching base and target image value pairs.
    (Smaller is a better match.)  For use as a NEWUOA optimization function.
-----------------------------------------------------------------------------*/

double GA_scalar_fitter( int npar , double *mpar )
{
  double val ;
  float *avm , *bvm , *wvm ;
  static double vsmall=1.e+37 ;

ENTRY("GA_scalar_fitter") ;

  avm = (float *)calloc(gstup->npt_match,sizeof(float)) ; /* target points at */
  GA_get_warped_values( npar , mpar , avm ) ;             /* warped locations */

  bvm = gstup->bvm->ar ;                                  /* base points */
  wvm = (gstup->wvm != NULL) ? gstup->wvm->ar : NULL ;    /* weights */

  if( gstup->need_hist_setup ) GA_setup_2Dhistogram( avm , bvm ) ;

  val = GA_scalar_costfun( gstup->match_code, gstup->npt_match, avm,bvm,wvm ) ;

  free((void *)avm) ;    /* toss the trash */
#if 0
  if( AFNI_yesenv("ALLIN_DEBUG") ){
    char cc = ' ' ;
    if( vsmall > val ){ vsmall = val ; cc = '*' ; }
    ININFO_message(" costfun = %.7g %c",val,cc) ;
  }
#endif
  RETURN(val);
}

/*---------------------------------------------------------------------------*/

void mri_genalign_scalar_clrwght( GA_setup *stup )  /* 18 Oct 2006 */
{
ENTRY("mri_genalign_scalar_clrwght") ;
  if( stup != NULL ){
    if( stup->bwght != NULL ) mri_free(stup->bwght) ;
    if( stup->bmask != NULL ) free((void *)stup->bmask) ;
    stup->nmask = stup->nvox_mask = 0 ;
    stup->bwght = NULL ; stup->bmask = NULL ;
  }
  EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Macro for exiting mri_genalign_scalar_setup() with extreme prejudice.     */

#undef  ERREX
#define ERREX(s) \
 do{ ERROR_message("mri_genalign_scalar_setup: %s",(s)); EXRETURN; } while(0)

/*---------------------------------------------------------------------------*/
/*! Setup for generic alignment of scalar images.
    - If this is a new alignment, images basim and targim must be input;
      wghtim is optional.
    - If this is a continuation of a previously started alignment
      with modified parameters (e.g., smoothing method/radius), then image
      copies are already stored in stup and don't need to be resupplied.
    - cf. 3dAllineate.c for use of this function!
-----------------------------------------------------------------------------*/

void mri_genalign_scalar_setup( MRI_IMAGE *basim  , MRI_IMAGE *wghtim ,
                                MRI_IMAGE *targim , GA_setup  *stup    )

{
   int qq , rr , nx,ny,nz,nxy , mm,ii,jj,kk , qdim ;
   int use_all=0 , need_pts=0 , nmatch ;
   int need_smooth_base , do_smooth_base ;
   int need_smooth_targ , do_smooth_targ , got_new_targ=0 ;
   float *bsar ;

ENTRY("mri_genalign_scalar_setup") ;

   /* basic checks of input for rationality:
      - Must have stup struct (setup parameters)
      - Must have new base image (basim) or have it previously stored in stup
      - Must have new target image (targim) or have previously stored version */

   if( mverb > 1 ) ININFO_message("* Enter alignment setup routine") ;

   if( stup == NULL ) ERREX("stup is NULL") ;
   stup->setup = 0 ;  /* mark stup struct as not being ready yet */

   if( basim  == NULL && stup->bsim == NULL ) ERREX("basim is NULL") ;
   if( targim == NULL && stup->ajim == NULL ) ERREX("targim is NULL") ;

   /* check dimensionality of input images (2D or 3D) */

   if( basim != NULL ){
     qdim = mri_dimensionality(basim) ;
     if( qdim < 2 || qdim > 3 )
       ERREX("basim dimensionality is not 2 or 3") ;
   } else {
     qdim = mri_dimensionality(stup->bsim) ;
   }
   stup->abdim = qdim ;

   if( targim != NULL ){
     if( qdim != mri_dimensionality(targim) )
       ERREX("basim & targim dimensionalities differ") ;
   }

   if( stup->wfunc_numpar < 1 || stup->wfunc==NULL || stup->wfunc_param==NULL )
     ERREX("illegal wfunc parameters") ;

   stup->dim_avec = stup->dim_bvec = 1 ;  /* scalars */

   /** copy new images into setup struct **/

   if( basim != NULL ){
     need_pts = 1 ;              /* will need to extract match points */
     if( stup->bsim != NULL ) mri_free(stup->bsim) ;
     if( mverb > 1 ) ININFO_message("- copying base image") ;
     stup->bsim = mri_to_float(basim) ;
     if( stup->bsim->dx <= 0.0f ) stup->bsim->dx = 1.0f ;
     if( stup->bsim->dy <= 0.0f ) stup->bsim->dy = 1.0f ;
     if( stup->bsim->dz <= 0.0f ) stup->bsim->dz = 1.0f ;

     if( !ISVALID_MAT44(stup->base_cmat) ){
       LOAD_DIAG_MAT44( stup->base_cmat ,
                        stup->bsim->dx , stup->bsim->dy , stup->bsim->dz ) ;
       LOAD_MAT44_VEC( stup->base_cmat ,
                       -(stup->bsim->nx-1)*0.5f*stup->bsim->dx ,
                       -(stup->bsim->ny-1)*0.5f*stup->bsim->dy ,
                       -(stup->bsim->nz-1)*0.5f*stup->bsim->dz  ) ;
     }
     stup->base_imat = MAT44_INV( stup->base_cmat ) ;
     stup->base_di = MAT44_COLNORM(stup->base_cmat,0) ;  /* 22 Aug 2007 */
     stup->base_dj = MAT44_COLNORM(stup->base_cmat,1) ;
     stup->base_dk = MAT44_COLNORM(stup->base_cmat,2) ;
     if( stup->bsims != NULL ){ mri_free(stup->bsims); stup->bsims = NULL; }
   }
   nx = stup->bsim->nx; ny = stup->bsim->ny; nz = stup->bsim->nz; nxy = nx*ny;

   if( targim != NULL ){
     if( stup->ajim != NULL ) mri_free(stup->ajim) ;
     if( mverb > 1 ) ININFO_message("- copying source image") ;
     stup->ajim = mri_to_float(targim) ;
     if( stup->ajim->dx <= 0.0f ) stup->ajim->dx = 1.0f ;
     if( stup->ajim->dy <= 0.0f ) stup->ajim->dy = 1.0f ;
     if( stup->ajim->dz <= 0.0f ) stup->ajim->dz = 1.0f ;

     if( !ISVALID_MAT44(stup->targ_cmat) ){
       LOAD_DIAG_MAT44( stup->targ_cmat ,
                        stup->ajim->dx , stup->ajim->dy , stup->ajim->dz ) ;
       LOAD_MAT44_VEC( stup->targ_cmat ,
                       -(stup->ajim->nx-1)*0.5f*stup->ajim->dx ,
                       -(stup->ajim->ny-1)*0.5f*stup->ajim->dy ,
                       -(stup->ajim->nz-1)*0.5f*stup->ajim->dz  ) ;
     }
     stup->targ_imat = MAT44_INV( stup->targ_cmat ) ;
     stup->targ_di = MAT44_COLNORM(stup->targ_cmat,0) ;  /* 22 Aug 2007 */
     stup->targ_dj = MAT44_COLNORM(stup->targ_cmat,1) ;
     stup->targ_dk = MAT44_COLNORM(stup->targ_cmat,2) ;

     if( stup->ajims  != NULL ){ mri_free(stup->ajims) ; stup->ajims  = NULL; }
     if( stup->ajimor != NULL ){ mri_free(stup->ajimor); stup->ajimor = NULL; }

     if( stup->ajmask != NULL && stup->ajmask->nvox != stup->ajim->nvox ){
       WARNING_message("mri_genalign_scalar_setup: target image/mask mismatch") ;
       mri_free(stup->ajmask) ; stup->ajmask = NULL ;
     }

     if( stup->ajmask != NULL && stup->ajmask_ranfill ){ /* set aj_u??? variables for mask noise */
       float *af, *qf ; byte *mmm ; float_pair quam ; float ubot,usiz , u1,u2;
       MRI_IMAGE *qim ; int nvox,pp ;
       stup->ajimor = mri_copy(stup->ajim) ;  /* backup of original image */
       if( stup->usetemp ) mri_purge( stup->ajimor ) ;  /* save for later */
       af  = MRI_FLOAT_PTR(stup->ajim) ;
       mmm = MRI_BYTE_PTR (stup->ajmask) ;
       qim = mri_new_conforming(stup->ajim,MRI_float);
       qf  = MRI_FLOAT_PTR(qim); nvox = qim->nvox;
       for( ii=pp=0 ; ii < nvox ; ii++ ){ if( mmm[ii] ) qf[pp++] = af[ii] ; }
       qim->nvox = pp; quam = mri_twoquantiles(qim,0.07f,0.93f); mri_free(qim);
       stup->aj_ubot = quam.a ; stup->aj_usiz = (quam.b - quam.a)*0.5f ;
     } else {
       stup->aj_ubot = stup->aj_usiz = 0.0f ;
     }
     got_new_targ = 1 ;
   } /* end of processing input targim */

   /* smooth images if needed
      13 Oct 2006: separate smoothing radii for base and target */

   need_smooth_base = (stup->smooth_code > 0 && stup->smooth_radius_base > 0.0f);
   need_smooth_targ = (stup->smooth_code > 0 && stup->smooth_radius_targ > 0.0f);
   do_smooth_base   = need_smooth_base &&
                     ( stup->smooth_code        != stup->old_sc      ||
                       stup->smooth_radius_base != stup->old_sr_base ||
                       stup->bsims              == NULL                ) ;
   do_smooth_targ   = need_smooth_targ &&
                     ( stup->smooth_code        != stup->old_sc      ||
                       stup->smooth_radius_targ != stup->old_sr_targ ||
                       stup->ajims              == NULL                ) ;
   stup->old_sc      = stup->smooth_code   ;
   stup->old_sr_base = stup->smooth_radius_base ;
   stup->old_sr_targ = stup->smooth_radius_targ ;

   if( !need_smooth_base ){
     if( stup->bsims != NULL ){ mri_free(stup->bsims); stup->bsims = NULL; }
     stup->old_sr_base = -1.0f ;
     mri_unpurge(stup->bsim) ; /* 20 Dec 2006 */
   }
   if( !need_smooth_targ ){
     if( stup->ajims != NULL ){ mri_free(stup->ajims); stup->ajims = NULL; }
     stup->old_sr_targ = -1.0f ;
     mri_unpurge(stup->ajim) ; /* 20 Dec 2006 */
   }
   if( do_smooth_base ){
     if( stup->bsims != NULL ) mri_free(stup->bsims);
     if( mverb > 1 )
       ININFO_message("- Smoothing base; radius=%.2f",stup->smooth_radius_base);
     stup->bsims = GA_smooth( stup->bsim , stup->smooth_code ,
                                           stup->smooth_radius_base ) ;
     if( stup->usetemp ) mri_purge( stup->bsim ) ;  /* 20 Dec 2006 */
   }
   if( do_smooth_targ ){
     if( stup->ajims != NULL ) mri_free(stup->ajims);
     if( mverb > 1 )
       ININFO_message("- Smoothing source; radius=%.2f",stup->smooth_radius_targ);
     stup->ajims = GA_smooth( stup->ajim , stup->smooth_code ,
                                           stup->smooth_radius_targ ) ;
     if( stup->usetemp ) mri_purge( stup->ajim ) ;  /* 20 Dec 2006 */
     got_new_targ = 1 ;
   }

   /* 07 Aug 2007: deal with target mask, if any [moved here 28 Aug 2008] */
   /* that is, add noise to non-mask parts of the image, AFTER smoothing */

   if( got_new_targ && stup->ajmask != NULL && stup->aj_usiz > 0.0f && stup->ajmask_ranfill ){
     float ubot=stup->aj_ubot , usiz=stup->aj_usiz , u1,u2 ;
     float *af ; byte *mmm ; int nvox ;
     if( stup->ajims != NULL ) af = MRI_FLOAT_PTR(stup->ajims) ;
     else                      af = MRI_FLOAT_PTR(stup->ajim ) ;
     mmm = MRI_BYTE_PTR(stup->ajmask) ;
     nvox = stup->ajmask->nvox ;
     if( mverb > 2 ) ININFO_message("!source mask fill: ubot=%g usiz=%g",ubot,usiz);
     myunif_reset(1234567890) ;  /* to get the same numbers every time */
     for( ii=0 ; ii < nvox ; ii++ ){  /* fill non-mask voxels with noise */
       if( !mmm[ii] ){ u1 = myunif(); u2 = myunif(); af[ii] = ubot + usiz*(u1+u2); }
     }
   }

   /* get min and max values in base and target images */

   if( stup->ajims == NULL ){
     stup->ajbot = (float)mri_min(stup->ajim) ;
     stup->ajtop = (float)mri_max(stup->ajim) ;
     if( stup->ajbot >= 0.0f ) stup->ajclip = mri_topclip(stup->ajim) ;
     else                      stup->ajclip = stup->ajtop ;
   } else {
     stup->ajbot = (float)mri_min(stup->ajims) ;
     stup->ajtop = (float)mri_max(stup->ajims) ;
     if( stup->ajbot >= 0.0f ) stup->ajclip = mri_topclip(stup->ajims) ;
     else                      stup->ajclip = stup->ajtop ;
   }

   if( stup->bsims == NULL ){
     stup->bsbot = (float)mri_min(stup->bsim) ;
     stup->bstop = (float)mri_max(stup->bsim) ;
     if( stup->bsbot >= 0.0f ) stup->bsclip = mri_topclip(stup->bsim) ;
     else                      stup->bsclip = stup->bstop ;
   } else {
     stup->bsbot = (float)mri_min(stup->bsims) ;
     stup->bstop = (float)mri_max(stup->bsims) ;
     if( stup->bsbot >= 0.0f ) stup->bsclip = mri_topclip(stup->bsims) ;
     else                      stup->bsclip = stup->bstop ;
   }

   /** load weight and mask arrays **/

   if( wghtim != NULL ){              /*---- have new weight to load ----*/
     MRI_IMAGE *qim ; float *bar , bfac ;

     need_pts = 1 ;
     if( wghtim->nvox != stup->bsim->nvox )
       ERREX("basim and wghtim grids differ!?!") ;

     if( stup->bwght != NULL ) mri_free(stup->bwght) ;
     if( stup->bmask != NULL ) free((void *)stup->bmask) ;

     if( mverb > 1 ) ININFO_message("- copying weight image") ;

     stup->bwght = mri_to_float(wghtim) ; bar = MRI_FLOAT_PTR(stup->bwght) ;
     qim = mri_new_conforming(wghtim,MRI_byte); stup->bmask = MRI_BYTE_PTR(qim);
     bfac = (float)mri_maxabs(stup->bwght) ;
     if( bfac == 0.0f ) ERREX("wghtim is all zero?!?") ;
     bfac = 1.0f / bfac ;
     for( ii=0 ; ii < wghtim->nvox ; ii++ ){   /* scale weight, make mask */
       bar[ii] = fabsf(bar[ii])*bfac ;
       stup->bmask[ii] = (bar[ii] != 0.0f) ;
     }

     mri_fix_data_pointer( NULL , qim ) ; mri_free(qim) ;
     stup->nmask = THD_countmask( wghtim->nvox , stup->bmask ) ;
     if( stup->nmask < 99 ){
       WARNING_message("mri_genalign_scalar: illegal input mask") ;
       free(stup->bmask) ;
       stup->bmask = NULL ; stup->nmask = stup->nvox_mask = 0 ;
       mri_free(stup->bwght) ; stup->bwght = NULL ;
     } else {
       stup->nvox_mask = wghtim->nvox ;
     }

   } else if( stup->nmask > 0 ){  /*---- have old mask to check ----*/

     if( stup->nvox_mask != stup->bsim->nvox ){
       WARNING_message("old mask and new base image differ in size") ;
       if( stup->bwght != NULL ) mri_free(stup->bwght) ;
       if( stup->bmask != NULL ) free((void *)stup->bmask) ;
       stup->nmask = stup->nvox_mask = 0 ;
       stup->bmask = NULL ; stup->bwght = NULL ;
     } else if( mverb > 1 )
       ININFO_message("- retaining old weight image") ;

   } else {                           /*---- have no mask, new or old ----*/
     stup->bmask = NULL ;
     stup->bwght = NULL ;
     stup->nmask = stup->nvox_mask = 0 ;
     if( mverb > 1 ) ININFO_message("- no weight image") ;
   }

   /*-- extract matching points from base image (maybe) --*/

   nmatch = stup->npt_match ;
   if( stup->npt_match <= 9 || stup->npt_match > stup->bsim->nvox ){
     nmatch = stup->bsim->nvox ; use_all = 1 ;
   }
   if( stup->nmask > 0 && stup->npt_match > stup->nmask ){
     nmatch = stup->nmask ; use_all = 2 ;
   }

   if( stup->im == NULL || stup->im->nar != nmatch || stup->npt_match != nmatch )
     need_pts = 1 ;

   if( need_pts ){
     MRI_IMAGE *bim ;

     stup->npt_match = nmatch ;
     if( mverb > 1 ) ININFO_message("- using %d points from base image [use_all=%d]",nmatch,use_all) ;

     if( use_all == 1 ){         /*------------- all points, no mask -----------*/

       if( stup->im != NULL ){
         KILL_floatvec(stup->im) ;
         KILL_floatvec(stup->jm) ;
         KILL_floatvec(stup->km) ;
       }
       stup->im = stup->jm = stup->km = NULL ;

     } else if( use_all == 2 ){  /*------------- all points in mask ------------*/

       int nvox , pp ; byte *mask=stup->bmask ;

       if( stup->im != NULL ){
         KILL_floatvec(stup->im) ;
         KILL_floatvec(stup->jm) ;
         KILL_floatvec(stup->km) ;
       }
       MAKE_floatvec(stup->im,stup->npt_match) ;
       MAKE_floatvec(stup->jm,stup->npt_match) ;
       MAKE_floatvec(stup->km,stup->npt_match) ;

       for( mm=pp=0 ; pp < stup->npt_match ; mm++ ){
         if( GOOD(mm) ){
           ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
           stup->im->ar[pp] = ii; stup->jm->ar[pp] = jj; stup->km->ar[pp] = kk;
           pp++ ;
         }
       }

     } else {  /*--------------------- a subset of points ----------------------*/

       int nvox,pp,dm , *qm ; byte *mask = stup->bmask ;

       nvox = stup->bsim->nvox ;
       dm   = GA_find_relprime_fixed(nvox) ;
       if( stup->im != NULL ){
         KILL_floatvec(stup->im) ;
         KILL_floatvec(stup->jm) ;
         KILL_floatvec(stup->km) ;
       }
       MAKE_floatvec(stup->im,stup->npt_match) ;
       MAKE_floatvec(stup->jm,stup->npt_match) ;
       MAKE_floatvec(stup->km,stup->npt_match) ;

       qm = (int *)calloc(sizeof(int),stup->npt_match) ;
       mm = (nx/2) + (ny/2)*nx + (nz/2)*nxy ;
       for( pp=0 ; pp < stup->npt_match ; mm=(mm+dm)%nvox )
         if( GOOD(mm) ) qm[pp++] = mm ;
       qsort_int( stup->npt_match , qm ) ;

       for( pp=0 ; pp < stup->npt_match ; pp++ ){
         mm = qm[pp] ;
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         stup->im->ar[pp] = ii; stup->jm->ar[pp] = jj; stup->km->ar[pp] = kk;
       }
       free((void *)qm) ;
     }

     /*------------- extract values from base image for matching -------------*/

     KILL_floatvec(stup->bvm) ; KILL_floatvec(stup->wvm) ;
     bim = (stup->bsims != NULL ) ? stup->bsims : stup->bsim ;
     bsar = MRI_FLOAT_PTR(bim) ;
     MAKE_floatvec(stup->bvm,stup->npt_match) ;
     if( stup->bwght == NULL ) stup->wvm = NULL ;
     else                      MAKE_floatvec(stup->wvm,stup->npt_match) ;
     if( stup->im == NULL ){
       memcpy( stup->bvm->ar , bsar , sizeof(float)*stup->npt_match ) ;
       if( stup->wvm != NULL )
         memcpy( stup->wvm->ar , MRI_FLOAT_PTR(stup->bwght) ,
                                 sizeof(float)*stup->npt_match ) ;
     } else {
       for( qq=0 ; qq < stup->npt_match ; qq++ ){
         rr = (int)(stup->im->ar[qq] + stup->jm->ar[qq]*nx + stup->km->ar[qq]*nxy) ;
         stup->bvm->ar[qq] = bsar[rr] ;
       }
       if( stup->bwght != NULL ){
         bsar = MRI_FLOAT_PTR(stup->bwght) ;
         for( qq=0 ; qq < stup->npt_match ; qq++ ){
           rr = (int)(stup->im->ar[qq] + stup->jm->ar[qq]*nx + stup->km->ar[qq]*nxy) ;
           stup->wvm->ar[qq] = bsar[rr] ;
         }
       }
     }
     if( stup->usetemp ){
       mri_purge(stup->bwght) ;  /* 21 Dec 2006 */
       mri_purge(bim) ;          /* 22 Dec 2006 */
     }

     /* do match_code specific pre-processing of the extracted data */

#if 0
     switch( stup->match_code ){
       case GA_MATCH_SPEARMAN_SCALAR:
         STATUS("doing spearman_rank_prepare") ;
         stup->bvstat = spearman_rank_prepare( stup->npt_match, stup->bvm->ar );
       break ;
     }
#endif

   } /* end of if(need_pts) */

   if( stup->blokset != NULL ){                      /* 20 Aug 2007 */
     GA_BLOK_KILL(stup->blokset) ;
     stup->blokset = NULL ;
   }

   stup->need_hist_setup = 1 ;   /* 08 May 2007 */

   if( mverb > 1 ) ININFO_message("* Exit alignment setup routine") ;
   stup->setup = SMAGIC ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Set the byte mask for the target (or unset it).
    Should be done BEFORE mri_genalign_scalar_setup().  [07 Aug 2007] */

void mri_genalign_set_targmask( MRI_IMAGE *im_tmask , GA_setup *stup )
{
   int nmask , nvox ;
ENTRY("mri_genalign_set_targmask") ;
   if( stup == NULL ) EXRETURN ;
   stup->najmask = 0 ;
   if( stup->ajmask != NULL ){ mri_free(stup->ajmask); stup->ajmask = NULL; }
   if( im_tmask != NULL ){
     if( stup->ajim != NULL ){
       if( im_tmask->nvox != stup->ajim->nvox ){
         ERROR_message("mri_genalign_set_targmask: image mismatch!") ;
         EXRETURN ;
       } else {
         WARNING_message("mri_genalign_set_targmask: called after setup()?!") ;
       }
     }
     stup->ajmask = mri_to_byte(im_tmask) ;
     nvox = stup->ajmask->nvox ;
     stup->najmask = nmask = THD_countmask( nvox , MRI_BYTE_PTR(stup->ajmask) ) ;
     if( nmask < 999 ){
       WARNING_message(
        "mri_genalign_set_targmask: mask has %d voxels out of %d total ==> ignored!",
        nmask , nvox ) ;
       mri_free(stup->ajmask) ; stup->ajmask = NULL ; stup->najmask = 0 ;
     } else if( mverb > 2 ) {
       ININFO_message("source mask has %d [out of %d] voxels",nmask,nvox) ;
     }
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Set the byte mask for the base (or unset it).
    Should be done BEFORE mri_genalign_scalar_setup().  [25 Feb 2010] */

void mri_genalign_set_basemask( MRI_IMAGE *im_bmask , GA_setup *stup )
{
   int nmask , nvox ;
ENTRY("mri_genalign_set_basemask") ;
   if( stup == NULL ) EXRETURN ;
   if( stup->bsmask != NULL ){ mri_free(stup->bsmask); stup->bsmask = NULL; }
   stup->nbsmask = 0 ;
   if( im_bmask != NULL ){
     if( stup->ajim != NULL ){
       if( im_bmask->nvox != stup->ajim->nvox ){
         ERROR_message("mri_genalign_set_targmask: image mismatch!") ;
         EXRETURN ;
       } else {
         WARNING_message("mri_genalign_set_targmask: called after setup()?!") ;
       }
     }
     stup->bsmask = mri_to_byte(im_bmask) ;
     nvox = stup->bsmask->nvox ;
     stup->nbsmask = nmask = THD_countmask( nvox , MRI_BYTE_PTR(stup->bsmask) ) ;
     if( nmask < 999 ){
       WARNING_message(
        "mri_genalign_set_basemask: mask has %d voxels out of %d total ==> ignored!",
        nmask , nvox ) ;
       mri_free(stup->bsmask) ; stup->bsmask = NULL ; stup->nbsmask = 0 ;
     } else if( mverb > 2 ) {
       ININFO_message("base mask has %d [out of %d] voxels",nmask,nvox) ;
     }
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Setup parameters for optimizing.
-----------------------------------------------------------------------------*/

void GA_param_setup( GA_setup *stup )
{
   int ii , qq ;

ENTRY("GA_param_setup") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to GA_param_setup()") ;
     EXRETURN ;
   }

   /* count free parameters to optimize over */

   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ )
     if( !stup->wfunc_param[qq].fixed ) ii++ ;

   stup->wfunc_numfree = ii ;
   if( ii == 0 ){
     ERROR_message("No free parameters in GA_param_setup()?"); EXRETURN;
   }
   for( qq=0 ; qq < stup->wfunc_numpar ; qq++ )
     stup->wfunc_param[qq].siz = stup->wfunc_param[qq].max
                                -stup->wfunc_param[qq].min ;

#if 0
   if( mverb ){
     fprintf(stderr," + %d free parameters:\n",stup->wfunc_numfree) ;
     for( qq=0 ; qq < stup->wfunc_numpar ; qq++ )
       fprintf(stderr,"  #%d = %s [%.2f..%.2f] (fixed=%d)\n",
               qq , stup->wfunc_param[qq].name ,
               stup->wfunc_param[qq].min , stup->wfunc_param[qq].max ,
               stup->wfunc_param[qq].fixed ) ;
   }
#endif

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Optimize warping parameters, after doing setup.
    Return value is number of optimization functional calls
    (if it reaches nstep, then final accuracy of rend was not reached).
-----------------------------------------------------------------------------*/

int mri_genalign_scalar_optim( GA_setup *stup ,
                               double rstart, double rend, int nstep )
{
   double *wpar ;
   int ii , qq , nfunc ;

ENTRY("mri_genalign_scalar_optim") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to mri_genalign_scalar_optim()") ;
     RETURN(-1) ;
   }

   GA_param_setup(stup) ;
   if( stup->wfunc_numfree <= 0 ) RETURN(-2) ;

   /* copy initial warp parameters into local array wpar,
      scaling to the range 0..1                          */

   wpar = (double *)calloc(sizeof(double),stup->wfunc_numfree) ;
   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( !stup->wfunc_param[qq].fixed ){
       wpar[ii] = ( stup->wfunc_param[qq].val_init
                   -stup->wfunc_param[qq].min    ) / stup->wfunc_param[qq].siz;
       if( wpar[ii] < 0.0 || wpar[ii] > 1.0 ) wpar[ii] = PRED01(wpar[ii]) ;
       ii++ ;
     }
   }

   gstup = stup ;  /* for global access, in other functions in this file */
   gstup_bk = stup ;

   if( nstep <= 4*stup->wfunc_numfree+5 ) nstep = 6666 ;

        if( rstart >  0.2 ) rstart = 0.2 ;  /* our parameters are */
   else if( rstart <= 0.0 ) rstart = 0.1 ;  /* all in range 0..1 */

   if( rend >= 0.9*rstart || rend <= 0.0 ) rend = 0.0666 * rstart ;

   /*** all the real work takes place now ***/

   nfunc = powell_newuoa( stup->wfunc_numfree , wpar ,
                          rstart , rend , nstep , GA_scalar_fitter ) ;

   stup->vbest = GA_scalar_fitter( stup->wfunc_numfree , wpar ) ;

   /* copy+scale output parameter values back to stup struct */

   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( stup->wfunc_param[qq].fixed ){
       stup->wfunc_param[qq].val_out = stup->wfunc_param[qq].val_fixed ;
     } else {
       stup->wfunc_param[qq].val_out = stup->wfunc_param[qq].min
                                      +stup->wfunc_param[qq].siz
                                       *PRED01(wpar[ii]) ;
       ii++ ;
     }
   }

   free((void *)wpar) ;

   RETURN(nfunc) ;
}

/*---------------------------------------------------------------------------*/
/*! Get the cost function for the given setup. */
/*---------------------------------------------------------------------------*/

float mri_genalign_scalar_cost( GA_setup *stup , float *parm )
{
   double *wpar , val ;
   int ii , qq ;

ENTRY("mri_genalign_scalar_cost") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to mri_genalign_scalar_cost()") ;
     RETURN( BIGVAL );
   }

   GA_param_setup(stup) ;
   if( stup->wfunc_numfree <= 0 ) RETURN( BIGVAL );

   /* copy initial warp parameters into local array wpar,
      scaling to the range 0..1                          */

   wpar = (double *)calloc(sizeof(double),stup->wfunc_numfree) ;

   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( !stup->wfunc_param[qq].fixed ){
       val = (parm == NULL) ? stup->wfunc_param[qq].val_init : parm[qq] ;
       wpar[ii] = (val - stup->wfunc_param[qq].min) / stup->wfunc_param[qq].siz;
       if( wpar[ii] < 0.0 || wpar[ii] > 1.0 ) wpar[ii] = PRED01(wpar[ii]) ;
       ii++ ;
     }
   }

   gstup = stup ;  /* for global access, in other functions in this file */
   gstup_bk = stup ;

   val = GA_scalar_fitter( stup->wfunc_numfree , wpar ) ;

   free((void *)wpar) ; RETURN( (float)val );
}

/*---------------------------------------------------------------------------*/
/*! Return ALL cost functions for a given setup. */
/*---------------------------------------------------------------------------*/

floatvec * mri_genalign_scalar_allcosts( GA_setup *stup , float *parm )
{
   floatvec *costvec ;
   double *wpar , val ;
   float *avm , *bvm , *wvm ;
   int ii , qq , meth ;

ENTRY("mri_genalign_scalar_allcosts") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to mri_genalign_scalar_allcosts()") ;
     RETURN(NULL) ;
   }

   GA_param_setup(stup) ;
   if( stup->wfunc_numfree <= 0 ) RETURN(NULL);

   /* copy initial warp parameters into local array wpar,
      scaling to the range 0..1                          */

   wpar = (double *)calloc(sizeof(double),stup->wfunc_numfree) ;
   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( !stup->wfunc_param[qq].fixed ){
       val = (parm == NULL) ? stup->wfunc_param[qq].val_init : parm[qq] ;
       wpar[ii] = (val - stup->wfunc_param[qq].min) / stup->wfunc_param[qq].siz;
       if( wpar[ii] < 0.0 || wpar[ii] > 1.0 ) wpar[ii] = PRED01(wpar[ii]) ;
       ii++ ;
     }
   }

   gstup = stup ;
   gstup_bk = stup ;

   avm = (float *)calloc(stup->npt_match,sizeof(float)) ; /* target points at */
   GA_get_warped_values( stup->wfunc_numfree,wpar,avm ) ; /* warped locations */

   bvm = stup->bvm->ar ;                                 /* base points */
   wvm = (stup->wvm != NULL) ? stup->wvm->ar : NULL ;    /* weights */

   GA_setup_2Dhistogram( avm , bvm ) ;
   MAKE_floatvec( costvec , GA_MATCH_METHNUM_SCALAR ) ;

   for( meth=1 ; meth <= GA_MATCH_METHNUM_SCALAR ; meth++ )
     costvec->ar[meth-1] = GA_scalar_costfun( meth, stup->npt_match, avm,bvm,wvm ) ;

   free((void *)wpar); free((void *)avm);    /* toss the trash */
   RETURN(costvec) ;
}

/*---------------------------------------------------------------------------*/
/*! Test some random starting points.  Sets val_init values in stup.
-----------------------------------------------------------------------------*/

void mri_genalign_scalar_ransetup( GA_setup *stup , int nrand )
{
   double *wpar, *spar , val , vbest , *bpar , *qpar,*cpar , dist ;
   int ii , qq , twof , ss , nfr , icod , nt=0 , ngood ;
#define NKEEP (2*PARAM_MAXTRIAL+1)
   double *kpar[NKEEP] , kval[NKEEP] , qval[NKEEP] ;
   int nk,kk,jj, ngrid,ngtot ;
   int ival[NKEEP] , rval[NKEEP] ; float fval[NKEEP] ;
   char mrk[6]="*o+-." ;

ENTRY("mri_genalign_scalar_ransetup") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to mri_genalign_scalar_ransetup()") ;
     EXRETURN ;
   }
   if( nrand < NKEEP ) nrand = NKEEP+13 ;

   GA_param_setup(stup) ; gstup = stup ; gstup_bk = stup ;
   if( stup->wfunc_numfree <= 0 ) EXRETURN ;

   nfr = stup->wfunc_numfree ;    /* number of free parameters */
   switch( nfr ){                 /* set ngrid = number of points along */
     case 1: ngrid = 9 ; break ;  /*             parameter axis to scan */
     case 2: ngrid = 5 ; break ;  /*             in the regular grid    */
     case 3: ngrid = 3 ; break ;
     case 4:
     case 5:
     case 6: ngrid = 2 ; break ;
    default: ngrid = 1 ; break ;
   }
   for( ngtot=1,qq=0 ; qq < nfr ; qq++ ) ngtot *= ngrid ; /* ngrid^nfr */

   icod = stup->interp_code ;                     /* save and set interp mode */
   if( icod == MRI_NN || AFNI_noenv("AFNI_TWOPASS_LIN") )
     stup->interp_code = MRI_NN ;                            /* NN if desired */
   else
     stup->interp_code = MRI_LINEAR ;                       /* default=linear */

   wpar = (double *)calloc(sizeof(double),nfr) ;
   spar = (double *)calloc(sizeof(double),nfr) ;
   for( kk=0 ; kk < NKEEP ; kk++ )                     /* keep best NKEEP */
     kpar[kk] = (double *)calloc(sizeof(double),nfr) ; /* parameters sets */

   /* try the middle of the allowed parameter range, and save it */

   for( qq=0 ; qq < nfr ; qq++ ) wpar[qq] = 0.5 ;
   val = GA_scalar_fitter( nfr , wpar ) ;
   memcpy(kpar[0],wpar,sizeof(double)*nfr) ;           /* saved parameters */
   kval[0]  = val ;                                    /* saved cost function */
   rval[0]  = 0 ;                                      /* not random */
   for( kk=1 ; kk < NKEEP ; kk++ ){
     rval[kk] = 0 ; kval[kk] = BIGVAL ;                /* all these are worse */
   }

   /* try some random places, keep the best NKEEP of them */

   twof = 1 << nfr ;  /* 2^nfr */

   if( mverb > 1 ) ININFO_message("- number of free params = %d",nfr) ;
   if( mverb )     fprintf(stderr," + - Testing (%d+%d)*%d params:#",ngtot,nrand,twof) ;

   myunif_reset(3456789) ;  /* 27 Aug 2008 */

   for( ii=0 ; ii < nrand+ngtot ; ii++ ){
     if( ii < ngtot ){                     /* regular grid points */
       val = 0.5/(ngrid+1.0) ; ss = ii ;   /* in parameter space */
       for( qq=0 ; qq < nfr ; qq++ ){      /* ss = number in base ngrid */
         kk = ss % ngrid; ss = ss / ngrid; wpar[qq] = 0.5+(kk+1)*val;
       }
     } else {                              /* pseudo-random */
       if( mverb && ii == ngtot ) fprintf(stderr,"$") ;
       for( qq=0 ; qq < nfr ; qq++ ) wpar[qq] = 0.5*(1.05+0.90*myunif()) ;
     }

     for( ss=0 ; ss < twof ; ss++ ){   /* try divers reflections */
       for( qq=0 ; qq < nfr ; qq++ )
         spar[qq] = (ss & (1<<qq)) ? 1.0-wpar[qq] : wpar[qq] ;

       val = GA_scalar_fitter( nfr , spar ) ;       /* get cost functional */
       for( kk=0 ; kk < NKEEP ; kk++ ){     /* find if this is better than */
         if( val < kval[kk] ){              /* something we've seen so far */
           for( jj=NKEEP-2 ; jj >= kk ; jj-- ){  /* push those above kk up */
             memcpy( kpar[jj+1] , kpar[jj] , sizeof(double)*nfr ) ;
             kval[jj+1] = kval[jj]; rval[jj+1] = rval[jj];
           }
           memcpy( kpar[kk] , spar , sizeof(double)*nfr ) ;   /* save what */
           kval[kk]  = val ;                              /* we just found */
           rval[kk]  = (ii >= ngtot) ;            /* is this a random set? */
           if( mverb && kk < 5 ) fprintf(stderr,"%c",mrk[kk]) ;
           break ;
         }
       }
     } /* end of scan over parameter reflections */
   } /* end of initial scan; should have NKEEP best results in kpar & kval */

   /* 23 Feb 2010: check how many are actually good */

   for( ngood=kk=0 ; kk < NKEEP && kval[kk] < BIGVAL ; kk++,ngood++ ) ; /*nada*/
   if( ngood < 1 ){  /* should never happen */
     ERROR_message("Can't find any good starting locations!?") ;
     free((void *)wpar) ; free((void *)spar) ;
     for( kk=0 ; kk < NKEEP ; kk++ ) free((void *)kpar[kk]) ;
     EXRETURN ;
   }

   for( kk=0 ; kk < ngood ; kk++ ){  /* make sure are in 0..1 range */
     for( ii=0 ; ii < nfr ; ii++ ) kpar[kk][ii] = PRED01(kpar[kk][ii]) ;
   }

   if( mverb ){                    /* print table of results? */
     fprintf(stderr,"\n") ;
     fprintf(stderr," + - best %d costs found:\n",ngood) ;
     for(kk=0;kk<ngood;kk++){
      fprintf(stderr,"   %2d v=% 9.6f:",kk,kval[kk]);
      for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
       if( !stup->wfunc_param[qq].fixed ){
        val = stup->wfunc_param[qq].min+stup->wfunc_param[qq].siz*kpar[kk][ii];
        fprintf(stderr," % 6.2f",val) ; ii++ ;
       }
      }
      fprintf(stderr,"  [%s]" , rval[kk] ? "rand" : "grid" ) ;
      fprintf(stderr,"\n") ;
     }
   }

   /* try a little optimization on each of these parameter sets */

   vbest = BIGVAL ; jj = 0 ; if( icod != MRI_NN ) stup->interp_code = MRI_LINEAR ;
   for( kk=0 ; kk < ngood ; kk++ ){
     if( kval[kk] >= BIGVAL ) continue ;  /* should not happen */
     (void)powell_newuoa( nfr , kpar[kk] ,
                          0.05 , 0.005 , 11*nfr+17 , GA_scalar_fitter ) ;
     kval[kk]  = GA_scalar_fitter( nfr , kpar[kk] ) ;
     if( kval[kk] < vbest ){ vbest = kval[kk]; jj = kk; }
   }
   stup->vbest = vbest ;  /* save for user's edification */

   for( kk=0 ; kk < ngood ; kk++ )  /* make sure are in 0..1 range */
     for( ii=0 ; ii < nfr ; ii++ ) kpar[kk][ii] = PRED01(kpar[kk][ii]) ;

   /* 23 Feb 2010: cast out the bad overlap (coincidence count) guys */

   for( kk=0 ; kk < ngood ; kk++ ) qval[kk] = kval[kk] ;

   /* at this point, smallest error is vbest and best index in kpar is jj */

   if( mverb ){                    /* print out optimized results? */
     fprintf(stderr," + - costs of the above after a little optimization:\n") ;
     for( kk=0 ; kk < ngood ; kk++ ){
      fprintf(stderr,"  %c%2d v=% 9.6f:",(kk==jj)?'*':' ',kk,qval[kk]);
      for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
       if( !stup->wfunc_param[qq].fixed ){
        val = stup->wfunc_param[qq].min+stup->wfunc_param[qq].siz*kpar[kk][ii];
        fprintf(stderr," % 6.2f",val) ; ii++ ;
       }
      }
      fprintf(stderr,"  [%s]" , rval[kk] ? "rand" : "grid" ) ;
      fprintf(stderr,"\n") ;
     }
   }

   /* save best result in the parameter structure */

   bpar = kpar[jj] ;
   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( !stup->wfunc_param[qq].fixed ){
       stup->wfunc_param[qq].val_init = stup->wfunc_param[qq].min
                                       +stup->wfunc_param[qq].siz*bpar[ii];
       ii++ ;
     }
     stup->wfunc_param[qq].val_out = stup->wfunc_param[qq].val_init ;
   }

   /* sort kval, then store a bunch of the best parameters,
      which are not too close to the absolute best set we just saved */

#undef  DTHRESH
#define DTHRESH 0.05
   for( ii=0 ; ii < ngood ; ii++ ){ fval[ii] = kval[ii]; ival[ii] = ii; }
   qsort_floatint( ngood , fval , ival ) ;
   for( qq=0 ; qq < stup->wfunc_numpar ; qq++ ){ /** save best into trial #0 **/
     if( !stup->wfunc_param[qq].fixed )
       stup->wfunc_param[qq].val_trial[0] = stup->wfunc_param[qq].val_init ;
     else
       stup->wfunc_param[qq].val_trial[0] = stup->wfunc_param[qq].val_fixed ;
   }
   if( mverb > 1 ) ININFO_message("- save #%2d for twobest",ival[0]) ;
   nt = 1 ;
   for( jj=1 ; jj < ngood && nt < PARAM_MAXTRIAL ; jj++ ){
     qpar = kpar[ival[jj]] ;                 /* the jj-th best param set */
     for( kk=0 ; kk < jj ; kk++ ){   /* loop over the previous best ones */
       cpar =  kpar[ival[kk]] ;
       for( dist=0.0,ii=0 ; ii < nfr ; ii++ ){ /* compute dist from previous best */
         val = fabs(qpar[ii]-cpar[ii]) ; dist = MAX(dist,val) ;
       }
       if( dist < DTHRESH ){  /* too close to cpar ==> skip */
         if( mverb > 1 )
           ININFO_message("- skip #%2d for twobest: too close to set #%2d",
                           ival[jj], ival[kk] ) ;
         goto NEXT_jj ;
       }
     }
     if( mverb > 1 ) ININFO_message("- save #%2d for twobest",ival[jj]) ;
     for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
       if( !stup->wfunc_param[qq].fixed ){
         stup->wfunc_param[qq].val_trial[nt] = stup->wfunc_param[qq].min
                                              +stup->wfunc_param[qq].siz
                                               *qpar[ii];
         ii++ ;
       } else {
         stup->wfunc_param[qq].val_trial[nt] = stup->wfunc_param[qq].val_fixed ;
       }
     }
     nt++ ; /* 1 more trial set saved */
   NEXT_jj: ;
   }
   stup->wfunc_ntrial = nt ;

   /*** cleanup and exeunt ***/

   free((void *)wpar) ; free((void *)spar) ;
   for( kk=0 ; kk < NKEEP ; kk++ ) free((void *)kpar[kk]) ;

   stup->interp_code = icod ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Warp the entire target image to base coords.  Will be in float format.
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_genalign_scalar_warpim( GA_setup *stup )
{
   MRI_IMAGE *wim ;
   float     *war ;
   float      oot ;

ENTRY("mri_genalign_scalar_warpim") ;

   if( stup       == NULL || stup->setup != SMAGIC ||
       stup->ajim == NULL || stup->bsim  == NULL     ){
     ERROR_message("Illegal call to mri_genalign_scalar_warpim()") ;
     RETURN(NULL) ;
   }
   gstup = stup ; gstup_bk = stup ;

   wim = mri_new_conforming( stup->bsim , MRI_float ) ;
   war = MRI_FLOAT_PTR(wim) ;

   oot = GA_get_outval() ; GA_set_outval(0.0f) ;
   GA_get_warped_values( 0 , NULL , war ) ;
   GA_set_outval(oot) ;

   RETURN(wim) ;
}

/*-------------------------------------------------------------------------*/
/*! Warp an image to base coords, on an nnx X nny X nnz grid.
     - The mapping between ijk and base xyz coords, and
       the mapping between target xyz and ijk coords must have
       been set in mri_genalign_affine_set_befafter() before calling this!
     - The warping between base xyz and target xyz is given by the
       wfunc, which has npar parameters stored in wpar.
     - The interpolation method is in icode.
     - Output is in float format, no matter what input data format was.
     - Generalized from GA_get_warped_values() -- RWCox - 26 Sep 2006.
---------------------------------------------------------------------------*/

MRI_IMAGE * mri_genalign_scalar_warpone( int npar, float *wpar, GA_warpfunc *wfunc,
                                         MRI_IMAGE *imtarg ,
                                         int nnx , int nny , int nnz , int icode )
{
   int   ii,jj,kk,qq,pp,npp,mm,nx,ny,nxy,nz , npt,nall , nper ;
   float x,y,z ;
   float *imf , *jmf , *kmf ;
   float *imw , *jmw , *kmw ;
   MRI_IMAGE *wim , *inim ;
   float     *war , *inar ;
   float oot ;

ENTRY("mri_genalign_scalar_warpone") ;

   if( wfunc == NULL || imtarg == NULL ) RETURN(NULL) ;
   nper = MAX(nperval,NPER) ;

   /* send parameters to warping function, for setup */

   if( mverb > 1 ){
     fprintf(stderr,"++ image warp: parameters =") ;
     for( ii=0 ; ii < npar ; ii++ ) fprintf(stderr," %.4f",wpar[ii]) ;
     fprintf(stderr,"\n") ;
   }

   wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /* create float copy of input image, if needed */

   if( imtarg->kind == MRI_float ) inim = imtarg ;
   else                            inim = mri_to_float(imtarg) ;
   inar = MRI_FLOAT_PTR(inim) ;

   /* dimensions of output image */

   nx = nnx ; ny = nny ; nz = nnz ; nxy = nx*ny ; npt = nxy * nz ;
   wim = mri_new_vol( nx,ny,nz , MRI_float ) ;
   war = MRI_FLOAT_PTR(wim) ;

   /* ijk coordinates in base image to be warped to target ijk */

   nall = MIN(nper,npt) ;

   imf = (float *)calloc(sizeof(float),nall) ;
   jmf = (float *)calloc(sizeof(float),nall) ;
   kmf = (float *)calloc(sizeof(float),nall) ;

   /* ijk coordinates after warping */

   imw = (float *)calloc(sizeof(float),nall) ;
   jmw = (float *)calloc(sizeof(float),nall) ;
   kmw = (float *)calloc(sizeof(float),nall) ;

   oot = GA_get_outval() ; GA_set_outval(0.0f) ;

   /*--- do (up to) nall points at a time ---*/

   for( pp=0 ; pp < npt ; pp+=nall ){
     npp = MIN( nall , npt-pp ) ;      /* number to do */

     /* get base ijk coords */

     for( qq=0 ; qq < npp ; qq++ ){
       mm = pp+qq ;
       ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
       imf[qq] = (float)ii; jmf[qq] = (float)jj; kmf[qq] = (float)kk;
     }

     /**** warp base points to new locations ****/

     wfunc( npar , NULL , npp  , imf,jmf,kmf , imw,jmw,kmw ) ;

     /* interpolate target image at warped points */

     switch( icode ){
       case MRI_NN:
         GA_interp_NN( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;

       case MRI_LINEAR:
         GA_interp_linear( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;

       case MRI_CUBIC:
         GA_interp_cubic( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;

       case MRI_VARP1:
         GA_interp_varp1( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;

       case MRI_WSINC5:
         GA_interp_wsinc5( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;

       default:        /* for higher order methods not implemented here */
       case MRI_QUINTIC:
         GA_interp_quintic( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;
     }
   }

   GA_set_outval(oot) ;

   /* clip interpolated values to range of target image, if need be */

   if( MRI_HIGHORDER(icode) ){
     float bb=inar[0] , tt=inar[0] ; int nin=inim->nvox ;
     for( pp=1 ; pp < nin ; pp++ ) if( inar[pp] < bb ) bb = inar[pp] ;
                              else if( inar[pp] > tt ) tt = inar[pp] ;
     for( pp=0 ; pp < npt ; pp++ ) if( war[pp]  < bb ) war[pp] = bb ;
                              else if( war[pp]  > tt ) war[pp] = tt ;
   }

   /* free the enslaved memory */

   free((void *)kmw); free((void *)jmw); free((void *)imw);
   free((void *)kmf); free((void *)jmf); free((void *)imf);
   if( inim != imtarg ) mri_free(inim) ;

   RETURN(wim) ;
}

/*--------------------------------------------------------------------------*/
/*! - Find the 8 corners of the input dataset (voxel edges, not centers).
    - Warp each one using the provided wfunc().
    - Return the min and max (x,y,z) coordinates of these warped points.
*//*------------------------------------------------------------------------*/

#if 0
static void mri_genalign_warped_bbox( THD_3dim_dataset *inset,
                                      int npar, float *wpar, GA_warpfunc *wfunc,
                                      float *xb , float *xt ,
                                      float *yb , float *yt ,
                                      float *zb , float *zt )
{
   THD_dataxes *daxes ;
   float nx0,ny0,nz0 , nx1,ny1,nz1 ;
   float xx,yy,zz , xbot,ybot,zbot , xtop,ytop,ztop ;

   /* setup stuff */

   if( !ISVALID_DSET(inset) || wfunc == NULL ) return ;

   daxes = inset->daxes ;
   nx0 =           -0.5 ; ny0 =           -0.5 ; nz0 =           -0.5 ;
   nx1 = daxes->nxx-0.5 ; ny1 = daxes->nyy-0.5 ; nz1 = daxes->nzz-0.5 ;

   /* send parameters to warp function, if needed */

   if( npar > 0 && wpar != NULL )
     wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   wfunc() ;
}
#endif


/*==========================================================================*/
/*****------------------------ Warping functions -----------------------*****/
/*--------------------------------------------------------------------------*/

#include "vecmat.h"

/****************************************************************************/
/**************************** General affine warp ***************************/

/***** 04 Oct 2006: Modify to use mat44 instead of vecmat stuff, mostly *****/

/*--------------------------------------------------------------------------*/

#define D2R (PI/180.0)                /* angles are in degrees */

static int matorder = MATORDER_SDU ;  /* cf. mrilib.h */
static int smat     = SMAT_LOWER ;
static int dcode    = DELTA_AFTER  ;  /* cf. 3ddata.h */

void mri_genalign_affine_setup( int mmmm , int dddd , int ssss )
{
   if( mmmm > 0 ) matorder = mmmm ;
   if( dddd > 0 ) dcode    = dddd ;
   if( ssss > 0 ) smat     = ssss ;
   return ;
}

/*--------------------------------------------------------------------------*/

static int   aff_use_before=0 , aff_use_after=0 ;
static mat44 aff_before       , aff_after       , aff_gamijk , aff_gamxyz ;

void mri_genalign_affine_set_befafter( mat44 *ab , mat44 *af )
{
   if( ab == NULL || !ISVALID_MAT44(*ab) ){
     aff_use_before = 0 ;
   } else {
     aff_use_before = 1 ; aff_before = *ab ;
   }

   if( af == NULL || !ISVALID_MAT44(*af) ){
     aff_use_after = 0 ;
   } else {
     aff_use_after = 1 ; aff_after = *af ;
   }
   return ;
}

void mri_genalign_affine_get_befafter( mat44 *ab , mat44 *af )
{
   if( ab != NULL ) *ab = aff_before ;
   if( af != NULL ) *af = aff_after  ;
}

void mri_genalign_affine_get_gammaijk( mat44 *gg )
{
  if( gg != NULL ) *gg = aff_gamijk ;
}

void mri_genalign_affine_get_gammaxyz( mat44 *gg )
{
  if( gg != NULL ) *gg = aff_gamxyz ;
}

/*--------------------------------------------------------------------------*/
/*! Compute a rotation matrix specified by 3 angles:
      Q = R3 R2 R1, where Ri is rotation about axis axi by angle thi.
----------------------------------------------------------------------------*/

static mat44 rot_matrix( int ax1, double th1,
                         int ax2, double th2, int ax3, double th3  )
{
   mat44 q , p ;

   LOAD_ROT_MAT44( q , th1 , ax1 ) ;
   LOAD_ROT_MAT44( p , th2 , ax2 ) ; q = MAT44_MUL( p , q ) ;
   LOAD_ROT_MAT44( p , th3 , ax3 ) ; q = MAT44_MUL( p , q ) ;

   return q ;
}

/*--------------------------------------------------------------------------*/
static int pgmat=0 ;
void mri_genalign_set_pgmat( int p ){ pgmat = p; }

/*--------------------------------------------------------------------------*/

mat44 GA_setup_affine( int npar , float *parvec )
{
   mat44 ss,dd,uu,aa,bb , gam ;
   THD_fvec3 vv ;
   float     a,b,c , p,q,r ;

   if( pgmat ){
     int ii ;
     printf("GA_setup_affine params:") ;
     for( ii=0 ; ii < npar ; ii++ ) printf(" %g",parvec[ii]) ;
     printf("\n") ;
   }

   /* uu = rotation */

   a = b = c = 0.0f ;
   if( npar >= 4 ) a = D2R*parvec[3] ;
   if( npar >= 5 ) b = D2R*parvec[4] ;
   if( npar >= 6 ) c = D2R*parvec[5] ;
   if( a != 0.0f || b != 0.0f || c != 0.0f )
     uu = rot_matrix( 2,a , 0,b , 1,c ) ;
   else
     LOAD_DIAG_MAT44( uu , 1.0f,1.0f,1.0f ) ;

   if( pgmat ) DUMP_MAT44("GA_setup_affine uu",uu) ;

   /* dd = scaling */

   a = b = c = 1.0f ;
   if( npar >= 7 ){ a = parvec[6]; if( a <= 0.10f || a >= 10.0f ) a = 1.0f; }
   if( npar >= 8 ){ b = parvec[7]; if( b <= 0.10f || b >= 10.0f ) b = 1.0f; }
   if( npar >= 9 ){ c = parvec[8]; if( c <= 0.10f || c >= 10.0f ) c = 1.0f; }
   LOAD_DIAG_MAT44( dd , a,b,c ) ;

   if( pgmat ) DUMP_MAT44("GA_setup_affine dd",dd) ;

   /* ss = shear */

   a = b = c = 0.0f ;
   if( npar >= 10 ){ a = parvec[ 9]; if( fabsf(a) > 0.3333f ) a = 0.0f; }
   if( npar >= 11 ){ b = parvec[10]; if( fabsf(b) > 0.3333f ) b = 0.0f; }
   if( npar >= 12 ){ c = parvec[11]; if( fabsf(c) > 0.3333f ) c = 0.0f; }
#if 1
   switch( smat ){
     default:
     case SMAT_LOWER: LOAD_MAT44( ss , 1.0 , 0.0 , 0.0 , 0.0,
                                        a  , 1.0 , 0.0 , 0.0,
                                        b  ,  c  , 1.0 , 0.0 ) ; break ;

     case SMAT_UPPER: LOAD_MAT44( ss , 1.0 ,  a  ,  b , 0.0 ,
                                       0.0 , 1.0 ,  c , 0.0 ,
                                       0.0 , 0.0 , 1.0, 0.0  ) ; break ;

     case SMAT_XXX:   LOAD_MAT44( ss , 1.0 ,  a  ,  b , 0.0 ,
                                       0.0 , 1.0 , 0.0, 0.0 ,
                                       0.0 , 0.0 , 1.0, 0.0  ) ; break ;

     case SMAT_YYY:   LOAD_MAT44( ss , 1.0 , 0.0 , 0.0, 0.0 ,
                                        a  , 1.0 ,  b , 0.0 ,
                                       0.0 , 0.0 , 1.0, 0.0  ) ; break ;

     case SMAT_ZZZ:   LOAD_MAT44( ss , 1.0 , 0.0 , 0.0, 0.0 ,
                                       0.0 , 1.0 , 0.0, 0.0 ,
                                        a  ,  b  , 1.0, 0.0  ) ; break ;
   }
#else
   ksm = (smat % 3) ;           /* decode:  smat = ism*9 + jsm*3 + ksm    */
   ism = (smat / 9) ;           /* (ism,jsm,ksm) = permutation of (0,1,2) */
   jsm = (smat - 9*ism - ksm) ;
   LOAD_DIAG_MAT44( ss , 1.0 , 1.0 , 1.0 ) ;  /* identity */
   ss.m[jsm][ism] = a ;
   ss.m[ksm][ism] = b ;
   ss.m[ksm][jsm] = c ;
#endif

   if( pgmat ) DUMP_MAT44("GA_setup_affine ss",ss) ;

   /* multiply them, as ordered */

   switch( matorder ){
     default:
     case MATORDER_SDU:  aa = MAT44_MUL(ss,dd) ; bb = uu ; break ;
     case MATORDER_SUD:  aa = MAT44_MUL(ss,uu) ; bb = dd ; break ;
     case MATORDER_DSU:  aa = MAT44_MUL(dd,ss) ; bb = uu ; break ;
     case MATORDER_DUS:  aa = MAT44_MUL(dd,uu) ; bb = ss ; break ;
     case MATORDER_USD:  aa = MAT44_MUL(uu,ss) ; bb = dd ; break ;
     case MATORDER_UDS:  aa = MAT44_MUL(uu,dd) ; bb = ss ; break ;
   }
   gam = MAT44_MUL(aa,bb) ;

   /* shifts */

   a = b = c = 0.0f ;
   if( npar >= 1 ) a = parvec[0] ;
   if( npar >= 2 ) b = parvec[1] ;
   if( npar >= 3 ) c = parvec[2] ;

   if( dcode == DELTA_BEFORE ){
     MAT44_VEC( gam , a,b,c , p,q,r ) ;
     a = p ; b = q ; c = r ;
   }
   LOAD_MAT44_VEC( gam , a,b,c ) ;

   if( pgmat ) DUMP_MAT44("GA_setup_affine gam (xyz)",gam) ;

   /* before and after transformations? */

   aff_gamxyz = gam ;

   if( pgmat && aff_use_before ) DUMP_MAT44("GA_setup_affine before",aff_before) ;
   if( pgmat && aff_use_after  ) DUMP_MAT44("GA_setup_affine after ",aff_after ) ;

   if( aff_use_before ) gam = MAT44_MUL( gam , aff_before ) ;
   if( aff_use_after  ) gam = MAT44_MUL( aff_after , gam  ) ;

   aff_gamijk = gam ;

#if 0
   if( mverb > 1 ){
     if( aff_use_before ) DUMP_MAT44("before",aff_before) ;
     if( aff_use_after  ) DUMP_MAT44("after ",aff_after ) ;
                          DUMP_MAT44("gam   ",gam       ) ;
   }
#endif

   if( pgmat ) DUMP_MAT44("GA_setup_affine gam (ijk)",gam) ;

   return gam ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for affine transformations. */
/*--------------------------------------------------------------------------*/

void mri_genalign_affine( int npar, float *wpar ,
                          int npt , float *xi, float *yi, float *zi ,
                                    float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   int ii ;

   /** new parameters ==> setup matrix */

   if( npar > 0 && wpar != NULL ){
     gam = GA_setup_affine( npar , wpar ) ;
     if( pgmat ) DUMP_MAT44("mri_genalign_affine",gam) ;
   }

   /* nothing to transform? */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /* multiply matrix times input vectors */

#pragma omp parallel if( npt > 33333 )
 { int ii ;
  AFNI_OMP_START ;
#pragma omp for
   for( ii=0 ; ii < npt ; ii++ )
     MAT44_VEC( gam , xi[ii],yi[ii],zi[ii] , xo[ii],yo[ii],zo[ii] ) ;
 AFNI_OMP_END ;
 }

   return ;
}

/*--------------------------------------------------------------------------*/
/*! Similar to mri_genalign_affine(), but the 12 parameters are the matrix
    directly, with no physical interpretations such as angles, etc.
----------------------------------------------------------------------------*/

void mri_genalign_mat44( int npar, float *wpar,
                         int npt , float *xi, float *yi, float *zi ,
                                   float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   int ii ;

   /** new parameters ==> setup matrix */

   if( npar >= 12 && wpar != NULL ){
     LOAD_MAT44_AR(gam,wpar) ;
     if( pgmat ) DUMP_MAT44("mri_genalign_mat44",gam) ;
   }

   /* nothing to transform? */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /* multiply matrix times input vectors */

#pragma omp parallel if( npt > 33333 )
 { int ii ;
  AFNI_OMP_START ;
   for( ii=0 ; ii < npt ; ii++ )
     MAT44_VEC( gam , xi[ii],yi[ii],zi[ii] , xo[ii],yo[ii],zo[ii] ) ;
 AFNI_OMP_END ;
 }

   return ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for bilinear warp alignment. */

void mri_genalign_bilinear( int npar, float *wpar ,
                            int npt , float *xi, float *yi, float *zi ,
                                      float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   static float dd_for[3][3][3] , xcen,ycen,zcen,dd_fac ;
   static int ddiag=0 ;  /* is dd matrix diagonal? */

   /** new parameters ==> setup matrix */

   if( npar >= 43 && wpar != NULL ){  /* 39 'real' parameters, 4 'fake' ones */
     float dmag , emag ;

     xcen   = wpar[39] ;              /* the fake (non-varying) parameters */
     ycen   = wpar[40] ;
     zcen   = wpar[41] ;
     dd_fac = wpar[42] ;

     gam = GA_setup_affine( 12 , wpar ) ;

     dd_for[0][0][0] = wpar[12] * dd_fac ;  /* the real parameters */
     dd_for[0][0][1] = wpar[13] * dd_fac ;
     dd_for[0][0][2] = wpar[14] * dd_fac ;
     emag = fabsf(wpar[12])+fabsf(wpar[13])+fabsf(wpar[14]) ;

     dd_for[0][1][0] = wpar[15] * dd_fac ;
     dd_for[0][1][1] = wpar[16] * dd_fac ;
     dd_for[0][1][2] = wpar[17] * dd_fac ;
     ddiag = (wpar[15]==0.0f) && (wpar[16]==0.0f) && (wpar[17]==0.0f) ;
     dmag  = fabsf(wpar[15])+fabsf(wpar[16])+fabsf(wpar[17]) ;

     dd_for[0][2][0] = wpar[18] * dd_fac ;
     dd_for[0][2][1] = wpar[19] * dd_fac ;
     dd_for[0][2][2] = wpar[20] * dd_fac ;
     ddiag = ddiag && (wpar[18]==0.0f) && (wpar[19]==0.0f) && (wpar[20]==0.0f) ;
     dmag += fabsf(wpar[18])+fabsf(wpar[19])+fabsf(wpar[20]) ;

     dd_for[1][0][0] = wpar[21] * dd_fac ;
     dd_for[1][0][1] = wpar[22] * dd_fac ;
     dd_for[1][0][2] = wpar[23] * dd_fac ;
     ddiag = ddiag && (wpar[21]==0.0f) && (wpar[22]==0.0f) && (wpar[23]==0.0f) ;
     dmag += fabsf(wpar[21])+fabsf(wpar[22])+fabsf(wpar[23]) ;

     dd_for[1][1][0] = wpar[24] * dd_fac ;
     dd_for[1][1][1] = wpar[25] * dd_fac ;
     dd_for[1][1][2] = wpar[26] * dd_fac ;
     emag += fabsf(wpar[24])+fabsf(wpar[25])+fabsf(wpar[26]) ;

     dd_for[1][2][0] = wpar[27] * dd_fac ;
     dd_for[1][2][1] = wpar[28] * dd_fac ;
     dd_for[1][2][2] = wpar[29] * dd_fac ;
     ddiag = ddiag && (wpar[27]==0.0f) && (wpar[28]==0.0f) && (wpar[29]==0.0f) ;
     dmag += fabsf(wpar[27])+fabsf(wpar[28])+fabsf(wpar[29]) ;

     dd_for[2][0][0] = wpar[30] * dd_fac ;
     dd_for[2][0][1] = wpar[31] * dd_fac ;
     dd_for[2][0][2] = wpar[32] * dd_fac ;
     ddiag = ddiag && (wpar[30]==0.0f) && (wpar[31]==0.0f) && (wpar[32]==0.0f) ;
     dmag += fabsf(wpar[30])+fabsf(wpar[31])+fabsf(wpar[32]) ;

     dd_for[2][1][0] = wpar[33] * dd_fac ;
     dd_for[2][1][1] = wpar[34] * dd_fac ;
     dd_for[2][1][2] = wpar[35] * dd_fac ;
     ddiag = ddiag && (wpar[33]==0.0f) && (wpar[34]==0.0f) && (wpar[35]==0.0f) ;
     dmag += fabsf(wpar[33])+fabsf(wpar[34])+fabsf(wpar[35]) ;
     dmag /= 18.0f ;

     dd_for[2][2][0] = wpar[36] * dd_fac ;
     dd_for[2][2][1] = wpar[37] * dd_fac ;
     dd_for[2][2][2] = wpar[38] * dd_fac ;
     emag += fabsf(wpar[36])+fabsf(wpar[37])+fabsf(wpar[38]) ;
     emag /= 9.0f ;

#if 0
INFO_message("bilinear warp %s diagonal: %.7g %.7g %.3g",
             ddiag ? "is" : "isn't" , emag , dmag , (emag>0.0f)?(dmag/emag):(-1.0f) ) ;
#endif
   }

   /* nothing to transform? */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /* multiply matrix times input vectors */

#pragma omp parallel if( npt > 22222 )
 { int ii ; THD_mat33 dd,ee ; float aa,bb,cc , uu,vv,ww ;
 AFNI_OMP_START ;
#pragma omp for
   for( ii=0 ; ii < npt ; ii++ ){

     aa = xi[ii] ; bb = yi[ii] ; cc = zi[ii] ;

     if( aff_use_before ){
       MAT44_VEC( aff_before , aa,bb,cc , uu,vv,ww ) ;
     } else {
       uu = aa ; vv = bb ; ww = cc ;
     }
     uu -= xcen; vv -= ycen; ww -= zcen;  /* centered coords */

     /* compute denominator (dd) matrix from coordinates and parameters */

     dd.mat[0][0] = 1.0f + dd_for[0][0][0]*uu + dd_for[0][0][1]*vv + dd_for[0][0][2]*ww;
     dd.mat[1][1] = 1.0f + dd_for[1][1][0]*uu + dd_for[1][1][1]*vv + dd_for[1][1][2]*ww;
     dd.mat[2][2] = 1.0f + dd_for[2][2][0]*uu + dd_for[2][2][1]*vv + dd_for[2][2][2]*ww;
     if( !ddiag ){
       dd.mat[0][1] =      dd_for[0][1][0]*uu + dd_for[0][1][1]*vv + dd_for[0][1][2]*ww;
       dd.mat[0][2] =      dd_for[0][2][0]*uu + dd_for[0][2][1]*vv + dd_for[0][2][2]*ww;
       dd.mat[1][0] =      dd_for[1][0][0]*uu + dd_for[1][0][1]*vv + dd_for[1][0][2]*ww;
       dd.mat[1][2] =      dd_for[1][2][0]*uu + dd_for[1][2][1]*vv + dd_for[1][2][2]*ww;
       dd.mat[2][0] =      dd_for[2][0][0]*uu + dd_for[2][0][1]*vv + dd_for[2][0][2]*ww;
       dd.mat[2][1] =      dd_for[2][1][0]*uu + dd_for[2][1][1]*vv + dd_for[2][1][2]*ww;
       ee = MAT_INV(dd) ;
     } else {
       LOAD_DIAG_MAT( ee , 1.0f / dd.mat[0][0] ,  /* diagonal dd matrix case */
                           1.0f / dd.mat[1][1] ,
                           1.0f / dd.mat[2][2]  ) ;
     }

     MAT44_VEC( gam , aa,bb,cc, uu,vv,ww ) ;  /* affine part of transformation */

     /* apply inverse of denominator matrix to affinely transformed vector */

     if( !ddiag ){
       xo[ii] = ee.mat[0][0] * uu + ee.mat[0][1] * vv + ee.mat[0][2] * ww ;
       yo[ii] = ee.mat[1][0] * uu + ee.mat[1][1] * vv + ee.mat[1][2] * ww ;
       zo[ii] = ee.mat[2][0] * uu + ee.mat[2][1] * vv + ee.mat[2][2] * ww ;
     } else {
       xo[ii] = ee.mat[0][0] * uu ;  /* diagonal dd matrix case */
       yo[ii] = ee.mat[1][1] * vv ;
       zo[ii] = ee.mat[2][2] * ww ;
     }

   } /* end of loop over input points */
 AFNI_OMP_END ;
 }

   return ;
}

/*--------------------------------------------------------------------------*/

/* Legendre polynomials */

#define LP1(x) (x)
#define LP2(x) ((x)*(x)-0.3333333f)                                             /* 2/3    P2 */
#define LP3(x) (((x)*(x)-0.6f)*(x))                                             /* 2/5    P3 */
#define LP4(x) ((x)*(x)*((x)*(x)-0.857143f)+0.0857143f)                         /* 8/35   P4 */
#define LP5(x) (((x)*(x)*((x)*(x)-1.11111f)+0.238095f)*(x))                     /* 8/63   P5 */
#define LP6(x) ((x)*(x)*((x)*(x)*((x)*(x)-1.36364f)+0.454545f)-0.021645f)       /* 16/231 P6 */
#define LP7(x) (((x)*(x)*((x)*(x)*((x)*(x)-1.61538f)+0.734266f)-0.081585f)*(x)) /* 16/429 P7 */

/* 3D product functions of various orders */

#define P2_xx(x,y,z) LP2(x)
#define P2_xy(x,y,z) LP1(x)*LP1(y)
#define P2_xz(x,y,z) LP1(x)*LP1(z)
#define P2_yy(x,y,z) LP2(y)
#define P2_yz(x,y,z) LP1(y)*LP1(z)
#define P2_zz(x,y,z) LP2(z)

#define P3_xxx(x,y,z) LP3(x)
#define P3_xxy(x,y,z) LP2(x)*LP1(y)
#define P3_xxz(x,y,z) LP2(x)*LP1(z)
#define P3_xyy(x,y,z) LP1(x)*LP2(y)
#define P3_xzz(x,y,z) LP1(x)*LP2(z)
#define P3_xyz(x,y,z) LP1(x)*LP1(y)*LP1(z)
#define P3_yyy(x,y,z) LP3(y)
#define P3_yyz(x,y,z) LP2(y)*LP1(z)
#define P3_yzz(x,y,z) LP1(y)*LP2(z)
#define P3_zzz(x,y,z) LP3(z)

#define P4_xxxx(x,y,z) LP4(x)
#define P4_xxxy(x,y,z) LP3(x)*LP1(y)
#define P4_xxxz(x,y,z) LP3(x)*LP1(z)
#define P4_xxyy(x,y,z) LP2(x)*LP2(y)
#define P4_xxzz(x,y,z) LP2(x)*LP2(z)
#define P4_xxyz(x,y,z) LP2(x)*LP1(y)*LP1(z)
#define P4_xyyy(x,y,z) LP1(x)*LP3(y)
#define P4_xyyz(x,y,z) LP1(x)*LP2(y)*LP1(z)
#define P4_xyzz(x,y,z) LP1(x)*LP1(y)*LP2(z)
#define P4_xzzz(x,y,z) LP1(x)*LP3(z)
#define P4_yyyy(x,y,z) LP4(y)
#define P4_yyyz(x,y,z) LP3(y)*LP1(z)
#define P4_yyzz(x,y,z) LP2(y)*LP2(z)
#define P4_yzzz(x,y,z) LP1(y)*LP3(z)
#define P4_zzzz(x,y,z) LP4(z)

#define P5_xxxxx(x,y,z) LP5(x)
#define P5_xxxxy(x,y,z) LP4(x)*LP1(y)
#define P5_xxxxz(x,y,z) LP4(x)*LP1(z)
#define P5_xxxyy(x,y,z) LP3(x)*LP2(y)
#define P5_xxxzz(x,y,z) LP3(x)*LP2(z)
#define P5_xxxyz(x,y,z) LP3(x)*LP1(y)*LP1(z)
#define P5_xxyyy(x,y,z) LP2(x)*LP3(y)
#define P5_xxyyz(x,y,z) LP2(x)*LP2(y)*LP1(z)
#define P5_xxyzz(x,y,z) LP2(x)*LP1(y)*LP2(z)
#define P5_xxzzz(x,y,z) LP2(x)*LP3(z)
#define P5_xyyyy(x,y,z) LP1(x)*LP4(y)
#define P5_xyyyz(x,y,z) LP1(x)*LP3(y)*LP1(z)
#define P5_xyyzz(x,y,z) LP1(x)*LP2(y)*LP2(z)
#define P5_xyzzz(x,y,z) LP1(x)*LP1(y)*LP3(z)
#define P5_xzzzz(x,y,z) LP1(x)*LP4(z)
#define P5_yyyyy(x,y,z) LP5(y)
#define P5_yyyyz(x,y,z) LP4(y)*LP1(z)
#define P5_yyyzz(x,y,z) LP3(y)*LP2(z)
#define P5_yyzzz(x,y,z) LP2(y)*LP3(z)
#define P5_yzzzz(x,y,z) LP1(y)*LP4(z)
#define P5_zzzzz(x,y,z) LP5(z)

#define NPARCUB   16  /* = 6+10       */
#define NPARQUINT 52  /* = 6+10+15+21 */

/*--------------------------------------------------------------------------*/
/*! A wfunc function for cubic polynomials. */

void mri_genalign_cubic( int npar, float *wpar ,
                         int npt , float *xi, float *yi, float *zi ,
                                   float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   static float xcen,ycen,zcen,xyzfac,xyzinv , ppar[3*NPARCUB] ;
   static int puse[NPARCUB] , pall ;

   /** new parameters ==> setup matrix */

   if( npar >= 3*NPARCUB+16 && wpar != NULL ){
     int aa=aff_use_after , ab=aff_use_before , jj ;

     xcen   = wpar[12+3*NPARCUB] ;  /* the fake (non-varying) parameters */
     ycen   = wpar[13+3*NPARCUB] ;
     zcen   = wpar[14+3*NPARCUB] ;
     xyzfac = wpar[15+3*NPARCUB] ; xyzinv = 1.0f / xyzfac ;

     aff_use_before = aff_use_after = 0;
     gam = GA_setup_affine( 12 , wpar ) ;  /* affine param setup */
     aff_use_before = ab; aff_use_after = aa;

     for( jj=0 ; jj < 3*NPARCUB ; jj++ )          /* save polynomial params */
       ppar[jj] = wpar[jj+12] * xyzinv ;
     for( pall=jj=0 ; jj < NPARCUB ; jj++ ){      /* mark which ones to use */
       puse[jj] = (ppar[3*jj  ] != 0.0f) ||
                  (ppar[3*jj+1] != 0.0f) || (ppar[3*jj+2] != 0.0f) ;
       pall += puse[jj] ;
     }
     pall = ( pall >= (int)(0.9f*NPARCUB) ) ;

#if 0
     if( AFNI_yesenv("ALLIN_DEBUG") ){
       fprintf(stderr,"++ cubic params: xyz_cen=%.4g,%.4g,%.4g fac=%.4g:",
               xcen,ycen,zcen,xyzfac) ;
       for( jj=0 ; jj < 60 ; jj++ )
         fprintf(stderr,"%s%.4g",(jj==12||jj==30)?" | ":" ",wpar[jj]) ;
       fprintf(stderr,"\n") ;
     }
#endif

   }

   /* nothing to transform? (a setup call) */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /*--- do some work ---*/

#pragma omp parallel if( npt > 6666 )
 { int ii,jj,kk ; float aa,bb,cc , uu,vv,ww , pv[NPARCUB] ;
 AFNI_OMP_START ;
#pragma omp for
   for( ii=0 ; ii < npt ; ii++ ){

     aa = xi[ii] ; bb = yi[ii] ; cc = zi[ii] ;  /* input indexes/coords */

     if( aff_use_before ){             /* convert to 'real' coordinates */
       MAT44_VEC( aff_before , aa,bb,cc , uu,vv,ww ) ;
     } else {
       uu = aa ; vv = bb ; ww = cc ;
     }
     MAT44_VEC( gam , uu,vv,ww, aa,bb,cc ) ;             /* affine part */

     /* centered and scaled to run from -1..1 */

     uu = (uu-xcen)*xyzfac ; vv = (vv-ycen)*xyzfac ; ww = (ww-zcen)*xyzfac ;

     /* polynomials */

     if( pall ){
       pv[ 0] = P2_xx (uu,vv,ww) ; pv[ 1] = P2_xy (uu,vv,ww) ;
       pv[ 2] = P2_xz (uu,vv,ww) ; pv[ 3] = P2_yy (uu,vv,ww) ;
       pv[ 4] = P2_yz (uu,vv,ww) ; pv[ 5] = P2_zz (uu,vv,ww) ;
       pv[ 6] = P3_xxx(uu,vv,ww) ; pv[ 7] = P3_xxy(uu,vv,ww) ;
       pv[ 8] = P3_xxz(uu,vv,ww) ; pv[ 9] = P3_xyy(uu,vv,ww) ;
       pv[10] = P3_xzz(uu,vv,ww) ; pv[11] = P3_xyz(uu,vv,ww) ;
       pv[12] = P3_yyy(uu,vv,ww) ; pv[13] = P3_yyz(uu,vv,ww) ;
       pv[14] = P3_yzz(uu,vv,ww) ; pv[15] = P3_zzz(uu,vv,ww) ;
       for( kk=jj=0 ; jj < NPARCUB ; jj++,kk+=3 ){
         aa += ppar[kk  ] * pv[jj] ;
         bb += ppar[kk+1] * pv[jj] ;
         cc += ppar[kk+2] * pv[jj] ;
       }
     } else {
       if( puse[ 0] ) pv[ 0] = P2_xx (uu,vv,ww) ;
       if( puse[ 1] ) pv[ 1] = P2_xy (uu,vv,ww) ;
       if( puse[ 2] ) pv[ 2] = P2_xz (uu,vv,ww) ;
       if( puse[ 3] ) pv[ 3] = P2_yy (uu,vv,ww) ;
       if( puse[ 4] ) pv[ 4] = P2_yz (uu,vv,ww) ;
       if( puse[ 5] ) pv[ 5] = P2_zz (uu,vv,ww) ;
       if( puse[ 6] ) pv[ 6] = P3_xxx(uu,vv,ww) ;
       if( puse[ 7] ) pv[ 7] = P3_xxy(uu,vv,ww) ;
       if( puse[ 8] ) pv[ 8] = P3_xxz(uu,vv,ww) ;
       if( puse[ 9] ) pv[ 9] = P3_xyy(uu,vv,ww) ;
       if( puse[10] ) pv[10] = P3_xzz(uu,vv,ww) ;
       if( puse[11] ) pv[11] = P3_xyz(uu,vv,ww) ;
       if( puse[12] ) pv[12] = P3_yyy(uu,vv,ww) ;
       if( puse[13] ) pv[13] = P3_yyz(uu,vv,ww) ;
       if( puse[14] ) pv[14] = P3_yzz(uu,vv,ww) ;
       if( puse[15] ) pv[15] = P3_zzz(uu,vv,ww) ;
       for( kk=jj=0 ; jj < NPARCUB ; jj++,kk+=3 ){
         if( puse[jj] ){
           aa += ppar[kk  ] * pv[jj] ;
           bb += ppar[kk+1] * pv[jj] ;
           cc += ppar[kk+2] * pv[jj] ;
         }
       }
     }

     if( aff_use_after ){                    /* convert back to indexes */
       MAT44_VEC( aff_after , aa,bb,cc , xo[ii],yo[ii],zo[ii] ) ;
     } else {
       xo[ii] = aa ; yo[ii] = bb ; zo[ii] = cc ;
     }

   } /* end of loop over input points */
 AFNI_OMP_END ;
 }

   return ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for quintic polynomials. */

void mri_genalign_quintic( int npar, float *wpar ,
                           int npt , float *xi, float *yi, float *zi ,
                                     float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   static float xcen,ycen,zcen,xyzfac,xyzinv , ppar[3*NPARQUINT] ;
   static int puse[NPARQUINT] , pall ;

   /** new parameters ==> setup matrix */

   if( npar >= 3*NPARQUINT+16 && wpar != NULL ){
     int aa=aff_use_after , ab=aff_use_before , jj ;

     xcen   = wpar[12+3*NPARQUINT] ;  /* the fake (non-varying) parameters */
     ycen   = wpar[13+3*NPARQUINT] ;
     zcen   = wpar[14+3*NPARQUINT] ;
     xyzfac = wpar[15+3*NPARQUINT] ; xyzinv = 1.0f / xyzfac ;

     aff_use_before = aff_use_after = 0;
     gam = GA_setup_affine( 12 , wpar ) ;  /* affine param setup */
     aff_use_before = ab; aff_use_after = aa;

     for( jj=0 ; jj < 3*NPARQUINT ; jj++ )          /* save polynomial params */
       ppar[jj] = wpar[jj+12] * xyzinv ;
     for( pall=jj=0 ; jj < NPARQUINT ; jj++ ){      /* mark which ones to use */
       puse[jj] = (ppar[3*jj  ] != 0.0f) ||
                  (ppar[3*jj+1] != 0.0f) || (ppar[3*jj+2] != 0.0f) ;
       pall += puse[jj] ;
     }
     pall = ( pall >= (int)(0.9f*NPARQUINT) ) ;

#if 0
     if( AFNI_yesenv("ALLIN_DEBUG") ){
       fprintf(stderr,"++ quintic params: xyz_cen=%.4g,%.4g,%.4g fac=%.4g:",
               xcen,ycen,zcen,xyzfac) ;
       for( jj=0 ; jj < 168 ; jj++ )
         fprintf(stderr,"%s%.4g",(jj==12||jj==30||jj==60||jj==105)?" | ":" ",wpar[jj]) ;
       fprintf(stderr,"\n") ;
     }
#endif

   }

   /* nothing to transform? (a setup call) */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /*--- do some work ---*/

#pragma omp parallel if( npt > 6666 )
 { int ii,jj,kk ; float aa,bb,cc , uu,vv,ww , pv[NPARQUINT] ;
 AFNI_OMP_START ;
#pragma omp for
   for( ii=0 ; ii < npt ; ii++ ){

     aa = xi[ii] ; bb = yi[ii] ; cc = zi[ii] ;  /* input indexes/coords */

     if( aff_use_before ){             /* convert to 'real' coordinates */
       MAT44_VEC( aff_before , aa,bb,cc , uu,vv,ww ) ;
     } else {
       uu = aa ; vv = bb ; ww = cc ;
     }
     MAT44_VEC( gam , uu,vv,ww, aa,bb,cc ) ;             /* affine part */

     /* centered and scaled to run from -1..1 */

     uu = (uu-xcen)*xyzfac ; vv = (vv-ycen)*xyzfac ; ww = (ww-zcen)*xyzfac ;

     /* polynomials */

     if( pall ){
       float p1x,p2x,p3x,p4x,p5x , p1y,p2y,p3y,p4y,p5y , p1z,p2z,p3z,p4z,p5z ;
       p1x = LP1(uu); p2x = LP2(uu); p3x = LP3(uu); p4x = LP4(uu); p5x = LP5(uu);
       p1y = LP1(vv); p2y = LP2(vv); p3y = LP3(vv); p4y = LP4(vv); p5y = LP5(vv);
       p1z = LP1(ww); p2z = LP2(ww); p3z = LP3(ww); p4z = LP4(ww); p5z = LP5(ww);

#define q2_xx p2x
#define q2_xy p1x*p1y
#define q2_xz p1x*p1z
#define q2_yy p2y
#define q2_yz p1y*p1z
#define q2_zz p2z
#define q3_xxx p3x
#define q3_xxy p2x*p1y
#define q3_xxz p2x*p1z
#define q3_xyy p1x*p2y
#define q3_xzz p1x*p2z
#define q3_xyz p1x*p1y*p1z
#define q3_yyy p3y
#define q3_yyz p2y*p1z
#define q3_yzz p1y*p2z
#define q3_zzz p3z
#define q4_xxxx p4x
#define q4_xxxy p3x*p1y
#define q4_xxxz p3x*p1z
#define q4_xxyy p2x*p2y
#define q4_xxzz p2x*p2z
#define q4_xxyz p2x*p1y*p1z
#define q4_xyyy p1x*p3y
#define q4_xyyz p1x*p2y*p1z
#define q4_xyzz p1x*p1y*p2z
#define q4_xzzz p1x*p3z
#define q4_yyyy p4y
#define q4_yyyz p3y*p1z
#define q4_yyzz p2y*p2z
#define q4_yzzz p1y*p3z
#define q4_zzzz p4z
#define q5_xxxxx p5x
#define q5_xxxxy p4x*p1y
#define q5_xxxxz p4x*p1z
#define q5_xxxyy p3x*p2y
#define q5_xxxzz p3x*p2z
#define q5_xxxyz p3x*p1y*p1z
#define q5_xxyyy p2x*p3y
#define q5_xxyyz p2x*p2y*p1z
#define q5_xxyzz p2x*p1y*p2z
#define q5_xxzzz p2x*p3z
#define q5_xyyyy p1x*p4y
#define q5_xyyyz p1x*p3y*p1z
#define q5_xyyzz p1x*p2y*p2z
#define q5_xyzzz p1x*p1y*p3z
#define q5_xzzzz p1x*p4z
#define q5_yyyyy p5y
#define q5_yyyyz p4y*p1z
#define q5_yyyzz p3y*p2z
#define q5_yyzzz p2y*p3z
#define q5_yzzzz p1y*p4z
#define q5_zzzzz p5z
       pv[ 0] = q2_xx  ; pv[ 1] = q2_xy  ; pv[ 2] = q2_xz  ; pv[ 3] = q2_yy  ;
       pv[ 4] = q2_yz  ; pv[ 5] = q2_zz  ; pv[ 6] = q3_xxx ; pv[ 7] = q3_xxy ;
       pv[ 8] = q3_xxz ; pv[ 9] = q3_xyy ; pv[10] = q3_xzz ; pv[11] = q3_xyz ;
       pv[12] = q3_yyy ; pv[13] = q3_yyz ; pv[14] = q3_yzz ; pv[15] = q3_zzz ;
       pv[16] = q4_xxxx ; pv[17] = q4_xxxy ; pv[18] = q4_xxxz ; pv[19] = q4_xxyy ;
       pv[20] = q4_xxzz ; pv[21] = q4_xxyz ; pv[22] = q4_xyyy ; pv[23] = q4_xyyz ;
       pv[24] = q4_xyzz ; pv[25] = q4_xzzz ; pv[26] = q4_yyyy ; pv[27] = q4_yyyz ;
       pv[28] = q4_yyzz ; pv[29] = q4_yzzz ; pv[30] = q4_zzzz ; pv[31] = q5_xxxxx ;
       pv[32] = q5_xxxxy ; pv[33] = q5_xxxxz ; pv[34] = q5_xxxyy ; pv[35] = q5_xxxzz ;
       pv[36] = q5_xxxyz ; pv[37] = q5_xxyyy ; pv[38] = q5_xxyyz ; pv[39] = q5_xxyzz ;
       pv[40] = q5_xxzzz ; pv[41] = q5_xyyyy ; pv[42] = q5_xyyyz ; pv[43] = q5_xyyzz ;
       pv[44] = q5_xyzzz ; pv[45] = q5_xzzzz ; pv[46] = q5_yyyyy ; pv[47] = q5_yyyyz ;
       pv[48] = q5_yyyzz ; pv[49] = q5_yyzzz ; pv[50] = q5_yzzzz ; pv[51] = q5_zzzzz ;
       for( kk=jj=0 ; jj < NPARQUINT ; jj++,kk+=3 ){
         aa += ppar[kk  ] * pv[jj] ;
         bb += ppar[kk+1] * pv[jj] ;
         cc += ppar[kk+2] * pv[jj] ;
       }
     } else {
       if( puse[ 0] ) pv[ 0] = P2_xx (uu,vv,ww) ;
       if( puse[ 1] ) pv[ 1] = P2_xy (uu,vv,ww) ;
       if( puse[ 2] ) pv[ 2] = P2_xz (uu,vv,ww) ;
       if( puse[ 3] ) pv[ 3] = P2_yy (uu,vv,ww) ;
       if( puse[ 4] ) pv[ 4] = P2_yz (uu,vv,ww) ;
       if( puse[ 5] ) pv[ 5] = P2_zz (uu,vv,ww) ;
       if( puse[ 6] ) pv[ 6] = P3_xxx(uu,vv,ww) ;
       if( puse[ 7] ) pv[ 7] = P3_xxy(uu,vv,ww) ;
       if( puse[ 8] ) pv[ 8] = P3_xxz(uu,vv,ww) ;
       if( puse[ 9] ) pv[ 9] = P3_xyy(uu,vv,ww) ;
       if( puse[10] ) pv[10] = P3_xzz(uu,vv,ww) ;
       if( puse[11] ) pv[11] = P3_xyz(uu,vv,ww) ;
       if( puse[12] ) pv[12] = P3_yyy(uu,vv,ww) ;
       if( puse[13] ) pv[13] = P3_yyz(uu,vv,ww) ;
       if( puse[14] ) pv[14] = P3_yzz(uu,vv,ww) ;
       if( puse[15] ) pv[15] = P3_zzz(uu,vv,ww) ;
       if( puse[16] ) pv[16] = P4_xxxx(uu,vv,ww) ;
       if( puse[17] ) pv[17] = P4_xxxy(uu,vv,ww) ;
       if( puse[18] ) pv[18] = P4_xxxz(uu,vv,ww) ;
       if( puse[19] ) pv[19] = P4_xxyy(uu,vv,ww) ;
       if( puse[20] ) pv[20] = P4_xxzz(uu,vv,ww) ;
       if( puse[21] ) pv[21] = P4_xxyz(uu,vv,ww) ;
       if( puse[22] ) pv[22] = P4_xyyy(uu,vv,ww) ;
       if( puse[23] ) pv[23] = P4_xyyz(uu,vv,ww) ;
       if( puse[24] ) pv[24] = P4_xyzz(uu,vv,ww) ;
       if( puse[25] ) pv[25] = P4_xzzz(uu,vv,ww) ;
       if( puse[26] ) pv[26] = P4_yyyy(uu,vv,ww) ;
       if( puse[27] ) pv[27] = P4_yyyz(uu,vv,ww) ;
       if( puse[28] ) pv[28] = P4_yyzz(uu,vv,ww) ;
       if( puse[29] ) pv[29] = P4_yzzz(uu,vv,ww) ;
       if( puse[30] ) pv[30] = P4_zzzz(uu,vv,ww) ;
       if( puse[31] ) pv[31] = P5_xxxxx(uu,vv,ww) ;
       if( puse[32] ) pv[32] = P5_xxxxy(uu,vv,ww) ;
       if( puse[33] ) pv[33] = P5_xxxxz(uu,vv,ww) ;
       if( puse[34] ) pv[34] = P5_xxxyy(uu,vv,ww) ;
       if( puse[35] ) pv[35] = P5_xxxzz(uu,vv,ww) ;
       if( puse[36] ) pv[36] = P5_xxxyz(uu,vv,ww) ;
       if( puse[37] ) pv[37] = P5_xxyyy(uu,vv,ww) ;
       if( puse[38] ) pv[38] = P5_xxyyz(uu,vv,ww) ;
       if( puse[39] ) pv[39] = P5_xxyzz(uu,vv,ww) ;
       if( puse[40] ) pv[40] = P5_xxzzz(uu,vv,ww) ;
       if( puse[41] ) pv[41] = P5_xyyyy(uu,vv,ww) ;
       if( puse[42] ) pv[42] = P5_xyyyz(uu,vv,ww) ;
       if( puse[43] ) pv[43] = P5_xyyzz(uu,vv,ww) ;
       if( puse[44] ) pv[44] = P5_xyzzz(uu,vv,ww) ;
       if( puse[45] ) pv[45] = P5_xzzzz(uu,vv,ww) ;
       if( puse[46] ) pv[46] = P5_yyyyy(uu,vv,ww) ;
       if( puse[47] ) pv[47] = P5_yyyyz(uu,vv,ww) ;
       if( puse[48] ) pv[48] = P5_yyyzz(uu,vv,ww) ;
       if( puse[49] ) pv[49] = P5_yyzzz(uu,vv,ww) ;
       if( puse[50] ) pv[50] = P5_yzzzz(uu,vv,ww) ;
       if( puse[51] ) pv[51] = P5_zzzzz(uu,vv,ww) ;
       for( kk=jj=0 ; jj < NPARQUINT ; jj++,kk+=3 ){
         if( puse[jj] ){
           aa += ppar[kk  ] * pv[jj] ;
           bb += ppar[kk+1] * pv[jj] ;
           cc += ppar[kk+2] * pv[jj] ;
         }
       }
     }

     if( aff_use_after ){                    /* convert back to indexes */
       MAT44_VEC( aff_after , aa,bb,cc , xo[ii],yo[ii],zo[ii] ) ;
     } else {
       xo[ii] = aa ; yo[ii] = bb ; zo[ii] = cc ;
     }

   } /* end of loop over input points */
 AFNI_OMP_END ;
 }

   return ;
}

/****************************************************************************/
/****************  Nonlinear Warpfield on top of affine *********************/

static float to_cube_ax=1.0f , to_cube_bx=0.0f ;
static float to_cube_ay=1.0f , to_cube_by=0.0f ;
static float to_cube_az=1.0f , to_cube_bz=0.0f ;
static float fr_cube_ax=1.0f , fr_cube_bx=0.0f ;
static float fr_cube_ay=1.0f , fr_cube_by=0.0f ;
static float fr_cube_az=1.0f , fr_cube_bz=0.0f ;

static mat44 to_cube , fr_cube ;

void mri_genalign_set_boxsize( float xbot, float xtop,
                               float ybot, float ytop, float zbot, float ztop )
{
   float ax=(xtop-xbot) ;
   float ay=(ytop-ybot) ;
   float az=(ztop-zbot) ;

   if( ax == 0.0f ){ to_cube_ax = 1.0f ; to_cube_bx = -xbot ; }
   else            { to_cube_ax = 2.0f/ax; to_cube_bx = -1.0f - xbot*to_cube_ax; }

   if( ay == 0.0f ){ to_cube_ay = 1.0f ; to_cube_by = -ybot ; }
   else            { to_cube_ay = 2.0f/ay; to_cube_by = -1.0f - ybot*to_cube_ay; }

   if( az == 0.0f ){ to_cube_az = 1.0f ; to_cube_bz = -zbot ; }
   else            { to_cube_az = 2.0f/az; to_cube_bz = -1.0f - zbot*to_cube_az; }

   fr_cube_ax = 1.0f / to_cube_ax ; fr_cube_bx = -to_cube_bx * fr_cube_ax ;
   fr_cube_ay = 1.0f / to_cube_ay ; fr_cube_by = -to_cube_by * fr_cube_ay ;
   fr_cube_az = 1.0f / to_cube_az ; fr_cube_bz = -to_cube_bz * fr_cube_az ;

   LOAD_DIAG_MAT44(to_cube,to_cube_ax,to_cube_ay,to_cube_az) ;
   LOAD_MAT44_VEC (to_cube,to_cube_bx,to_cube_by,to_cube_bz) ;

   LOAD_DIAG_MAT44(fr_cube,fr_cube_ax,fr_cube_ay,fr_cube_az) ;
   LOAD_MAT44_VEC (fr_cube,fr_cube_bx,fr_cube_by,fr_cube_bz) ;

   if( mverb )
     ININFO_message("Warpfield boxsize: %.1f..%.1f X %.1f..%.1f X %.1f..%.1f",
                    xbot,xtop , ybot,ytop , zbot,ztop ) ;

   return ;
}

/*--------------------------------------------------------------------------*/
#ifdef USE_OMP
# include "mri_warpfield.c"
#endif
/*--------------------------------------------------------------------------*/

static Warpfield *wfield = NULL ;

Warpfield * mri_genalign_warpfield_setup( int ttt , float ord , int flags )
{
   if( wfield != NULL ){ Warpfield_destroy(wfield); wfield = NULL; }

   wfield = Warpfield_init( ttt , ord , flags , NULL ) ;

   return wfield ;
}

/*--------------------------------------------------------------------------*/

Warpfield * mri_genalign_warpfield_get(void){ return wfield; }

void mri_genalign_warpfield_set(Warpfield *wf){ wfield = wf; }

/*--------------------------------------------------------------------------*/
/*! A wfunc function for nonlinear transformations. */
/*--------------------------------------------------------------------------*/

void mri_genalign_warpfield( int npar, float *wpar ,
                             int npt , float *xi, float *yi, float *zi ,
                                       float *xo, float *yo, float *zo  )
{
   int ii,pp,npp ;
   static float *xii=NULL,*yii,*zii , *xoo,*yoo,*zoo ;

   /* check for criminal inputs */

   if( npar < 12 || wfield == NULL || !ISVALID_MAT44(to_cube) ) return ;

   /** new parameters ==> setup transformation **/

   if( npar > 0 && wpar != NULL ){
     float *wp ;

     if( !WARPFIELD_SKIPAFF(wfield) ){
       mat44 gf , gam=GA_setup_affine(12,wpar) ; /* setup affine matrix */
       gf = MAT44_MUL(gam,fr_cube) ; wfield->aa = MAT44_MUL(to_cube,gf) ;
       wp = wpar+12 ; npp = npar-12 ;
     } else {
       wp = wpar ; npp = npar ;
     }

     npp = npp/3 ; if( npp > wfield->nfun ) npp = wfield->nfun ;
     for( pp=ii=0 ; ii < npp ; ii++ ){
       wfield->cx[ii] = wp[pp++] ;
       wfield->cy[ii] = wp[pp++] ;
       wfield->cz[ii] = wp[pp++] ;
     }
   }

   /* nothing to transform? */

   if( wfield == NULL || npt <= 0 || xi == NULL || xo == NULL ) return ;

   if( xii == NULL ){
     xii = (float *)malloc(sizeof(float)*NPER) ;
     yii = (float *)malloc(sizeof(float)*NPER) ;
     zii = (float *)malloc(sizeof(float)*NPER) ;
     xoo = (float *)malloc(sizeof(float)*NPER) ;
     yoo = (float *)malloc(sizeof(float)*NPER) ;
     zoo = (float *)malloc(sizeof(float)*NPER) ;
   }

   for( pp=0 ; pp < npt ; pp+=NPER ){
     npp = MIN( NPER , npt-pp ) ;  /* number to do in this iteration */

#pragma omp parallel if( npp > 33333 )
{ int ii ;
#pragma omp for
     for( ii=0 ; ii < npp ; ii++ )
       MAT44_VEC( to_cube , xi [ii+pp],yi [ii+pp],zi [ii+pp] ,
                            xii[ii+pp],yii[ii+pp],zii[ii+pp]  ) ;
}

     Warpfield_eval_array( wfield , npp , xii,yii,zii , xoo,yoo,zoo ) ;

#pragma omp parallel if( npp > 33333 )
{ int ii ;
#pragma omp for
     for( ii=0 ; ii < npp ; ii++ )
       MAT44_VEC( fr_cube , xoo[ii+pp],yoo[ii+pp],zoo[ii+pp] ,
                            xo [ii+pp],yo [ii+pp],zo [ii+pp]  ) ;
}

   }

   return ;
}

#if 0
/****************************************************************************/
/************** Nonlinear sum of basis fields stored in arrays **************/

static int wsum_npar = 0 ;

static int wsum_nx=0 , wsum_ny=0 , wsum_nz=0 , wsum_nxy=0 , wsum_nxyz=0 ;
static float wsum_dx , wsum_dy , wsum_dz ;

static float  *wsum_xinit  = NULL ;
static float  *wsum_yinit  = NULL ;
static float  *wsum_zinit  = NULL ;
static float **wsum_xbasis = NULL ;
static float **wsum_ybasis = NULL ;
static float **wsum_zbasis = NULL ;
static float  *wsum_param  = NULL ;

/*--------------------------------------------------------------------------*/

void mri_genalign_warpsum_set_dimen( int   nx, int   ny, int   nz,
                                     float dx, float dy, float dz )
{
   int ii ;

   if( nx < 1 ){ nx = ny = nz = 0 ; }

   wsum_nx = nx ;
   wsum_ny = ny ; wsum_nxy  = nx*ny ;
   wsum_nz = nz ; wsum_nxyz = wsum_nx * nz ;
   wsum_dx = dx ; wsum_dy = dy ; wsum_dz = dz ;

   IFREE(wsum_xinit); IFREE(wsum_yinit); IFREE(wsum_zinit); IFREE(wsum_param);

   if( wsum_xbasis != NULL ){
     for( ii=0 ; ii < wsum_npar ; ii++ ){
       IFREE(wsum_xbasis[ii]); IFREE(wsum_ybasis[ii]); IFREE(wsum_zbasis[ii]);
     }
     IFREE(wsum_xbasis); IFREE(wsum_ybasis); IFREE(wsum_zbasis);
   }

   return ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for nonlinear transformations. */
/*--------------------------------------------------------------------------*/

void mri_genalign_warpsum( int npar, float *wpar ,
                           int npt , float *xi, float *yi, float *zi ,
                                     float *xo, float *yo, float *zo  )
{
   register int ii,jj , pp,qq,rr , pqr ;
   register float xv,yv,zv , *wp ;

   /** new parameters **/

   if( npar > 0 && wpar != NULL ){
     memcpy( wsum_param , wpar , sizeof(float)*npar ) ;
   }
   wp = wsum_param ;

   /* nothing to transform? */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /* multiply matrix times input vectors */

   for( ii=0 ; ii < npt ; ii++ ){
     pp  = (int)xi[ii] ; qq = (int)yi[ii] ; rr = (int)zi[ii] ;
     pqr = pp + qq*wsum_nx + rr*wsum_nxy ;
     xv  = wsum_xinit[pqr] ; yv  = wsum_yinit[pqr] ; zv  = wsum_zinit[pqr] ;
     for( jj=0 ; jj < wsum_npar ; jj++ ){
       xv += wp[jj] * wsum_xbasis[jj][pqr] ;
       yv += wp[jj] * wsum_ybasis[jj][pqr] ;
       zv += wp[jj] * wsum_zbasis[jj][pqr] ;
     }
     xo[ii] = xv ; yo[ii] = yv ; zo[ii] = zv ;
   }

   return ;
}
#endif

/*----------------------------------------------------------------------------*/
/* Note that the setup call to wfunc() must have been made before this call. */

float GA_get_warped_overlap_fraction(void)
{
   int    npar , ii,jj,kk,qq,pp,nqq,mm,nx,nxy , nxt,nxyt , npt,nhit ;
   float *imf, *jmf, *kmf, *imw, *jmw, *kmw , xx,yy,zz,nxh,nyh,nzh , frac ;
   byte *bsar, *tgar ;
#ifdef USE_OMP
   byte *hhh ;
#endif

ENTRY("GA_get_warped_overlap") ;

   if( gstup->bsmask == NULL || gstup->ajmask ==NULL ) RETURN(1.0f) ;
   bsar = MRI_BYTE_PTR(gstup->bsmask) ;
   tgar = MRI_BYTE_PTR(gstup->ajmask) ;

   npar = gstup->wfunc_numpar ;
   npt  = gstup->bsmask->nvox ;  /* number of total voxels in base */
   nqq  = gstup->nbsmask ;       /* number of mask voxels in base */

   nx = gstup->bsmask->nx; nxy = nx * gstup->bsmask->ny;

   nxt = gstup->ajmask->nx ; nxyt = nxt * gstup->ajmask->ny ;
   nxh = nxt-0.501f ; nyh = gstup->ajmask->ny-0.501f ; nzh = gstup->ajmask->nz-0.501f ;

#if 0
   /* send parameters to warping function for its setup */

   gstup->wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;
#endif

   /*--- load 3D indexes to transform from base to target ---*/

   imf = (float *)malloc(sizeof(float)*nqq) ;
   jmf = (float *)malloc(sizeof(float)*nqq) ;
   kmf = (float *)malloc(sizeof(float)*nqq) ;

   for( pp=qq=0 ; pp < npt ; pp++ ){
     if( bsar[pp] ){
       ii = pp % nx ; kk = pp / nxy ; jj = (pp-kk*nxy) / nx ;
       imf[qq] = (float)ii ; jmf[qq] = (float)jj ; kmf[qq] = (float)kk; qq++ ;
     }
   }

   /*--- warp to new locations ---*/

   imw = (float *)malloc(sizeof(float)*nqq) ;
   jmw = (float *)malloc(sizeof(float)*nqq) ;
   kmw = (float *)malloc(sizeof(float)*nqq) ;

   gstup->wfunc( npar , NULL , nqq , imf,jmf,kmf , imw,jmw,kmw ) ;

   free(kmf); free(jmf); free(imf);

   /* check target mask at warped points */

#ifndef USE_OMP
   for( nhit=qq=0 ; qq < nqq ; qq++ ){
     xx = imw[qq] ; if( xx < -0.499f || xx > nxh ) continue ;
     yy = jmw[qq] ; if( yy < -0.499f || yy > nyh ) continue ;
     zz = kmw[qq] ; if( zz < -0.499f || zz > nzh ) continue ;
     ii = (int)(xx+0.5f) ; jj = (int)(yy+0.5f) ; kk = (int)(zz+0.5f) ;
     if( tgar[ii+jj*nxt+kk*nxyt] ) nhit++ ;
   }
#else
   hhh = (byte *)calloc(sizeof(byte),nqq) ;
#pragma omp parallel if( nqq > 33333 )
 { int ii,jj,kk,qq ; float xx,yy,zz ;
 AFNI_OMP_START ;
#pragma omp for
   for( qq=0 ; qq < nqq ; qq++ ){
     xx = imw[qq] ; if( xx < -0.499f || xx > nxh ) continue ;
     yy = jmw[qq] ; if( yy < -0.499f || yy > nyh ) continue ;
     zz = kmw[qq] ; if( zz < -0.499f || zz > nzh ) continue ;
     ii = (int)(xx+0.5f) ; jj = (int)(yy+0.5f) ; kk = (int)(zz+0.5f) ;
     if( tgar[ii+jj*nxt+kk*nxyt] ) hhh[qq] = 1 ;
   }
 AFNI_OMP_END ;
 }
 for( nhit=qq=0 ; qq < nqq ; qq++ ) nhit += hhh[qq] ;
 free(hhh) ;
#endif

   free((void *)kmw); free((void *)jmw); free((void *)imw);

   xx = gstup->nbsmask ;
   yy = gstup->najmask * gstup->ajim->dx * gstup->ajim->dy * gstup->ajim->dz /
                       ( gstup->bsim->dx * gstup->bsim->dy * gstup->bsim->dz ) ;

   frac = nhit / MIN(xx,yy) ; RETURN(frac) ;
}
