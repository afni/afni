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

static int   aff_use_before=0 , aff_use_after=0 ;
static mat44 aff_before       , aff_after       , aff_gamijk , aff_gamxyz ;

/*---------------------------------------------------------------------------*/
static int mverb = 0 , mpr = 0 ;
void mri_genalign_verbose(int v){ mverb = v ; }

/*---------------------------------------------------------------------------*/
/* 27 Aug 2008: replace use of drand48 with myunif, for inter-system control */

static const unsigned long long MYa=62003 ;
static const unsigned long long MYb=15485863 ;
static       unsigned long long MYx=15482917 ;
static INLINE float myunif(void)
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
/* STATUS("copy from mpar") ; */
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
/* STATUS("copy from gstup") ; */
     for( ii=0 ; ii < gstup->wfunc_numpar ; ii++ )
       wpar[ii] = gstup->wfunc_param[ii].val_out ;
   }

   /* create space for unwarped indexes, if none given */

   if( mpar == NULL || gstup->im == NULL ){
/* STATUS("make space") ; */
     npt = gstup->bsim->nvox ; nall = MIN(nper,npt) ;
     imf = (float *)calloc(sizeof(float),nall) ;
     jmf = (float *)calloc(sizeof(float),nall) ;
     kmf = (float *)calloc(sizeof(float),nall) ;
   } else {
     npt = gstup->npt_match ; nall = MIN(nper,npt) ;
   }

   /* create space for indexes of warped control points */

/* STATUS("make more space") ; */
   imw = (float *)calloc(sizeof(float),nall) ;
   jmw = (float *)calloc(sizeof(float),nall) ;
   kmw = (float *)calloc(sizeof(float),nall) ;

   nx = gstup->bsim->nx; ny = gstup->bsim->ny; nxy = nx*ny;

   /* send parameters to warping function for its setup */

/* STATUS("send params to wfunc") ; */
   gstup->wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /* choose image from which to extract data */

   aim = (gstup->ajims != NULL && mpar != NULL ) ? gstup->ajims /* smoothed */
                                                 : gstup->ajim; /* unsmooth */

   /*--- do (up to) nall points at a time ---*/

   for( pp=0 ; pp < npt ; pp+=nall ){

     npp = MIN( nall , npt-pp ) ;  /* number to do in this iteration */

     if( mpar == NULL || gstup->im == NULL ){  /* do all points */
/* STATUS("do all points") ; */
       for( qq=0 ; qq < npp ; qq++ ){
         mm = pp+qq ;
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         imf[qq] = (float)ii; jmf[qq] = (float)jj; kmf[qq] = (float)kk;
       }
     } else {
/* STATUS("do control points") ; */
       imf = gstup->im->ar + pp ;  /* pointers to control points */
       jmf = gstup->jm->ar + pp ;
       kmf = gstup->km->ar + pp ;
     }

     /****-- warp control points to new locations ---****/
     /**** (warp does index-to-index transformation) ****/

/* STATUS("call wfunc for real") ; */
     gstup->wfunc( npar , NULL ,
                   npp  , imf,jmf,kmf , imw,jmw,kmw ) ;

     /* interpolate target image at warped points */

/* STATUS("interpolate") ; */

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

/* STATUS("free stuff") ; */
   free((void *)kmw); free((void *)jmw); free((void *)imw);
   if( mpar == NULL || gstup->im == NULL ){
     free((void *)kmf); free((void *)jmf); free((void *)imf);
   }
   free((void *)wpar) ;

   /* clip interpolated values to range of target image, if need be */

   if( clip ){
/* STATUS("clip") ; */
     float bb=gstup->ajbot , tt=gstup->ajtop ;
     for( pp=0 ; pp < npt ; pp++ )
            if( avm[pp] < bb ) avm[pp] = bb ;
       else if( avm[pp] > tt ) avm[pp] = tt ;
   }

   EXRETURN ;
}

#if 0
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
#endif

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
          ovv = MAX(0.0f,9.95f-10.0f*ovv) ;
          val += micho_ov * ovv*ovv ;
        }
      }
    }
    break ;
  }

  if( !isfinite(val) ) val = BIGVAL ;
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

ENTRY("GA_scalar_fitter") ;

  avm = (float *)calloc(gstup->npt_match,sizeof(float)) ; /* target points at */
  GA_get_warped_values( npar , mpar , avm ) ;             /* warped locations */

  bvm = gstup->bvm->ar ;                                  /* base points */
  wvm = (gstup->wvm != NULL) ? gstup->wvm->ar : NULL ;    /* weights */

  if( gstup->need_hist_setup ) GA_setup_2Dhistogram( avm , bvm ) ;

  val = GA_scalar_costfun( gstup->match_code, gstup->npt_match, avm,bvm,wvm ) ;

  free((void *)avm) ;    /* toss the trash */

#if 1
  if( mverb > 1 ){
    static double vsmall=1.e+37 ; static int ncall=0 ;
    if( vsmall > val ){
      if( ncall > 0 ){
        if( mverb == 2 ) fprintf(stderr,"*") ;
        else             fprintf(stderr,"*[#%d=%.6g] ",ncall,val) ;
        mpr++ ;
      }
      vsmall = val ;
    } else if( mverb > 6 ){
                         fprintf(stderr," [#%d=%.6g] ",ncall,val) ; mpr++ ;
    }
    ncall++ ;
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
STATUS("copy basim") ;
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
STATUS("copy targim") ;
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
STATUS("smooth base") ;
     if( stup->bsims != NULL ) mri_free(stup->bsims);
     if( mverb > 1 )
       ININFO_message("- Smoothing base; radius=%.2f",stup->smooth_radius_base);
     stup->bsims = GA_smooth( stup->bsim , stup->smooth_code ,
                                           stup->smooth_radius_base ) ;
     if( stup->usetemp ) mri_purge( stup->bsim ) ;  /* 20 Dec 2006 */
   }
   if( do_smooth_targ ){
STATUS("smooth targ") ;
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

STATUS("get min/max") ;
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

STATUS("load weight and mask") ;
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

STATUS("need_pts: use_all==1") ;

       if( stup->im != NULL ){
         KILL_floatvec(stup->im) ;
         KILL_floatvec(stup->jm) ;
         KILL_floatvec(stup->km) ;
       }
       stup->im = stup->jm = stup->km = NULL ;

     } else if( use_all == 2 ){  /*------------- all points in mask ------------*/

       int nvox , pp ; byte *mask=stup->bmask ;

STATUS("need_pts: use_all==2") ;

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

STATUS("need_pts: subset") ;

       nvox = stup->bsim->nvox ;
       dm   = GA_find_relprime_fixed(nvox) ;
STATUSi("  :: relprime dm",dm) ;
       if( stup->im != NULL ){
         KILL_floatvec(stup->im) ;
         KILL_floatvec(stup->jm) ;
         KILL_floatvec(stup->km) ;
       }
       MAKE_floatvec(stup->im,stup->npt_match) ;
       MAKE_floatvec(stup->jm,stup->npt_match) ;
       MAKE_floatvec(stup->km,stup->npt_match) ;

       qm = (int *)calloc(sizeof(int),stup->npt_match) ;
       if( qm == NULL ) ERREX("qm malloc fails") ;
       mm = (nx/2) + (ny/2)*nx + (nz/2)*nxy ;
       for( pp=0 ; pp < stup->npt_match ; mm=(mm+dm)%nvox )
         if( GOOD(mm) ) qm[pp++] = mm ;
#if 0
STATUS("  :: sort qm") ;
       qsort_int( stup->npt_match , qm ) ;
#else
       { int cut = (int)(0.5f*sqrtf((float)stup->npt_match)) ;
STATUSi("  :: mostly sort qm; cut",cut) ;
         qsort_int_mostly( stup->npt_match , qm , cut ) ;
       }
#endif

STATUS("  :: load index arrays") ;
       for( pp=0 ; pp < stup->npt_match ; pp++ ){
         mm = qm[pp] ;
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         stup->im->ar[pp] = ii; stup->jm->ar[pp] = jj; stup->km->ar[pp] = kk;
       }
       free((void *)qm) ;
     }

     /*------------- extract values from base image for matching -------------*/

STATUS("extract from base") ;

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
     if( nmask < 999 && nmask/(float)nvox < 0.1f ){
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
     if( nmask < 999 && nmask/(float)nvox < 0.09f ){
       WARNING_message(
        "mri_genalign_set_basemask: mask has %d voxels out of %d total ==> ignored!",
        nmask , nvox ) ;
       mri_free(stup->bsmask); stup->bsmask = NULL; stup->nbsmask = 0;
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

   mpr = 0 ;
   nfunc = powell_newuoa( stup->wfunc_numfree , wpar ,
                          rstart , rend , nstep , GA_scalar_fitter ) ;

   stup->vbest = GA_scalar_fitter( stup->wfunc_numfree , wpar ) ;

#if 1
  if( mpr > 0 && mverb > 1 ) fprintf(stderr,"\n") ;
#endif

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
   mpr = 0 ;
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
   if( mverb )     fprintf(stderr," + - Test (%d+%d)*%d params [top5=%s]:#",ngtot,nrand,twof,mrk) ;

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
     if( mpr > 0 ) fprintf(stderr,"\n") ;
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
   if( mverb ) fprintf(stderr," + - A little optimization:") ;
   for( kk=0 ; kk < ngood ; kk++ ){
     if( kval[kk] >= BIGVAL ) continue ;  /* should not happen */
     (void)powell_newuoa( nfr , kpar[kk] ,
                          0.05 , 0.005 , 11*nfr+17 , GA_scalar_fitter ) ;
     kval[kk]  = GA_scalar_fitter( nfr , kpar[kk] ) ;
     if( kval[kk] < vbest ){ vbest = kval[kk]; jj = kk; }
     if( mverb ) fprintf(stderr,".") ;
   }
   if( mverb ) fprintf(stderr,"\n") ;
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
#if 0
     if( AFNI_yesenv("ALLIN_DEBUG") ){
       if( aff_use_before ) DUMP_MAT44("aff_before",aff_before) ;
       if( aff_use_after  ) DUMP_MAT44("aff_after ",aff_after ) ;
     }
#endif
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

#ifndef HAVE_HEXVOL
#define HAVE_HEXVOL
/*----------------------------------------------------------------------------*/
/* Volume of a hexahedron (distorted cube) given by 8 corners.
   Looking down from the top, the bottom plane points are numbered so:
       2 -- 3
       |    |  and the top plane is similar (add 4 to each index),
       0 -- 1  with point #(i+4) 'above' point #i.
*//*..........................................................................*/

#undef  TRIPROD
#define TRIPROD(ax,ay,az,bx,by,bz,cx,cy,cz) ( (ax)*((by)*(cz)-(bz)*(cy)) \
                                             +(bx)*((cy)*(az)-(cz)*(ay)) \
                                             +(cx)*((ay)*(bz)-(az)*(by))  )
#undef  DA
#undef  DB
#undef  DC
#define DA(p,q) (p.a-q.a)
#define DB(p,q) (p.b-q.b)
#define DC(p,q) (p.c-q.c)

static float hexahedron_volume( float_triple x0 , float_triple x1 ,
                                float_triple x2 , float_triple x3 ,
                                float_triple x4 , float_triple x5 ,
                                float_triple x6 , float_triple x7  )
{
   float xa,ya,za , xb,yb,zb , xc,yc,zc , vol ;

   xa = DA(x7,x1)+DA(x6,x0); ya = DB(x7,x1)+DB(x6,x0); za = DC(x7,x1)+DC(x6,x0);
   xb = DA(x7,x2)          ; yb = DB(x7,x2)          ; zb = DC(x7,x2) ;
   xc = DA(x3,x0)          ; yc = DB(x3,x0)          ; zc = DC(x3,x0) ;
   vol = TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x6,x0)          ; ya = DB(x6,x0)          ; za = DC(x6,x0) ;
   xb = DA(x7,x2)+DA(x5,x0); yb = DB(x7,x2)+DB(x5,x0); zb = DC(x7,x2)+DC(x5,x0);
   xc = DA(x7,x4)          ; yc = DB(x7,x4)          ; zc = DC(x7,x4) ;
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x7,x1)          ; ya = DB(x7,x1)          ; za = DC(x7,x1) ;
   xb = DA(x5,x0)          ; yb = DB(x5,x0)          ; zb = DC(x5,x0) ;
   xc = DA(x7,x4)+DA(x3,x0); yc = DB(x7,x4)+DB(x3,x0); zc = DC(x7,x4)+DC(x3,x0);
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   return (0.08333333f*vol) ;
}
#undef TRIPROD
#undef DA
#undef DB
#undef DC
#endif /* HAVE_HEXVOL */

/*----------------------------------------------------------------------------*/

#undef IJK
#undef C2F
#undef D2F

#define IJK(p,q,r)    ((p)+(q)*nx+(r)*nxy)
#define C2F(p,q,r,xx) MAT44_VEC(cmat,(p),(q),(r),(xx).a,(xx).b,(xx).c)
#define D2F(pqr,xx)   ( (xx).a+=dxar[pqr], (xx).b+=dyar[pqr], (xx).c+=dzar[pqr] )

MRI_IMAGE * mri_genalign_xyzwarp_volmap( MRI_IMARR *dxyzar , mat44 cmat )
{
   int nx,ny,nz,nxy,nxyz ;
   float *dxar, *dyar, *dzar, *var ; MRI_IMAGE *vim ;

ENTRY("mri_genalign_xyzwarp_volmap") ;

   if( dxyzar == NULL || IMARR_COUNT(dxyzar) < 3 ) RETURN(NULL) ;

   nx = IMARR_SUBIM(dxyzar,0)->nx ;
   ny = IMARR_SUBIM(dxyzar,0)->ny ;
   nz = IMARR_SUBIM(dxyzar,0)->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   /* must have at least 2 dimensions bigger than 1 */

   if( nxyz <= nx || nxyz <= ny || nxyz <= nz ) RETURN(NULL) ;

   dxar = MRI_FLOAT_PTR(IMARR_SUBIM(dxyzar,0)) ;
   dyar = MRI_FLOAT_PTR(IMARR_SUBIM(dxyzar,1)) ;
   dzar = MRI_FLOAT_PTR(IMARR_SUBIM(dxyzar,2)) ;

   vim = mri_new_conforming( IMARR_SUBIM(dxyzar,0) , MRI_float ) ;
   var = MRI_FLOAT_PTR(vim) ;

   if( !ISVALID_MAT44(cmat) ) LOAD_DIAG_MAT44(cmat,1,1,1) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 33333 )
 { float_triple x0,x1,x2,x3,x4,x5,x6,x7 ;
   int ii,jj,kk , ip,jp,kp , ijk , qq ;
#pragma omp for
   for( qq=0 ; qq < nxyz ; qq++ ){
     ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
     ip = ii+1 ; jp = jj+1 ; kp = kk+1 ;
     C2F(ii,jj,kk,x0); C2F(ip,jj,kk,x1); C2F(ii,jp,kk,x2); C2F(ip,jp,kk,x3);
     C2F(ii,jj,kp,x4); C2F(ip,jj,kp,x5); C2F(ii,jp,kp,x6); C2F(ip,jp,kp,x7);
     if( ip == nx ) ip-- ; if( jp == ny ) jp-- ; if( kp == nz ) kp-- ;
     ijk = IJK(ip,jj,kk) ; D2F(ijk,x1) ;
     ijk = IJK(ii,jp,kk) ; D2F(ijk,x2) ;
     ijk = IJK(ip,jp,kk) ; D2F(ijk,x3) ;
     ijk = IJK(ii,jj,kp) ; D2F(ijk,x4) ;
     ijk = IJK(ip,jj,kp) ; D2F(ijk,x5) ;
     ijk = IJK(ii,jp,kp) ; D2F(ijk,x6) ;
     ijk = IJK(ip,jp,kp) ; D2F(ijk,x7) ;
     ijk = qq            ; D2F(ijk,x0) ;
     var[qq] = hexahedron_volume(x0,x1,x2,x3,x4,x5,x6,x7) ;
   }
 }
 AFNI_OMP_END ;

   RETURN(vim) ;
}
#undef IJK
#undef C2F
#undef D2F

/*----------------------------------------------------------------------------*/
/* Get the 3 axes (xyz) deltas at each point in the input grid.
   That is, for each grid point (x,y,z), the source dataset should be
   evaluated at (x+dx,y+dy,z+dz) to make it match the base dataset.
*//*--------------------------------------------------------------------------*/

MRI_IMARR * mri_genalign_scalar_xyzwarp( int npar, float *wpar,
                                         GA_warpfunc *wfunc,
                                         int nnx , int nny , int nnz )
{
   MRI_IMAGE *xim , *yim , *zim ;
   float     *xar , *yar , *zar ;
   MRI_IMARR *imar ;
   int   qq,pp,npp,nx,ny,nxy,nz , npt,nall , nper , ab,aa ;
   float x,y,z , *xbb , *ybb , *zbb ;
   mat44 cmat ;

ENTRY("mri_genalign_scalar_xyzwarp") ;

   if( wfunc == NULL ) RETURN(NULL) ;

   /* send parameters to warping function, for setup */

   wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /* dimensions of output images */

   nx = nnx ; ny = nny ; nz = nnz ; nxy = nx*ny ; npt = nxy * nz ;
   xim = mri_new_vol( nx,ny,nz , MRI_float ) ; xar = MRI_FLOAT_PTR(xim) ;
   yim = mri_new_vol( nx,ny,nz , MRI_float ) ; yar = MRI_FLOAT_PTR(yim) ;
   zim = mri_new_vol( nx,ny,nz , MRI_float ) ; zar = MRI_FLOAT_PTR(zim) ;
   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,xim) ; ADDTO_IMARR(imar,yim) ; ADDTO_IMARR(imar,zim) ;

   /* ijk coordinates in base image to be warped to target xyz */

   nper = MAX(nperval,NPER) ; nall = MIN(nper,npt) ;
   xbb  = (float *)calloc(sizeof(float),nall) ;
   ybb  = (float *)calloc(sizeof(float),nall) ;
   zbb  = (float *)calloc(sizeof(float),nall) ;

   /* set wfunc to do xyz -> xyz transform, not ijk -> ijk */

   ab = aff_use_before ; aa = aff_use_after ;
   mri_genalign_affine_use_befafter( 0 , 0 ) ;

   /*--- do (up to) nall points at a time ---*/

   for( pp=0 ; pp < npt ; pp+=nall ){
     npp = MIN( nall , npt-pp ) ;      /* number to do in this group */

     /* get base (unwarped) xyz coords into xbb,ybb,zbb */

 AFNI_OMP_START ;
#pragma omp parallel if( npt > 33333 )
 { int qq , mm , ii,jj,kk ;
#pragma omp for
     for( qq=0 ; qq < npp ; qq++ ){
       mm = pp+qq ;
       ii = mm % nx ; kk = mm / nxy ; jj = (mm-kk*nxy) / nx ;
       if( ab ){
         MAT44_VEC( aff_before , ii,jj,kk , xbb[qq],ybb[qq],zbb[qq] ) ;
       } else {
         xbb[qq] = ii ; ybb[qq] = jj ; zbb[qq] = kk ;
       }
     }
 }
 AFNI_OMP_END ;

     /**** warp base points to new locations, in xar,yar,zar ****/

     wfunc( npar , NULL , npp  , xbb,ybb,zbb , xar+pp,yar+pp,zar+pp ) ;

     /* convert result to shifts rather than absolute coordinates,
        store back into xar,yar,zar (output arrays inside output images) */

     for( qq=0 ; qq < npp ; qq++ ){
       xar[pp+qq] -= xbb[qq] ;
       yar[pp+qq] -= ybb[qq] ;
       zar[pp+qq] -= zbb[qq] ;
     }

   } /* end of loop over groups of points */

   free(zbb) ; free(ybb) ; free(xbb) ;            /* tossola trashola */
   mri_genalign_affine_use_befafter( ab , aa ) ;  /* status quo ante */

   /* 13 Dec 2010: save the cell volumes as well */

   if( ab ) cmat = aff_before ;
   else     INVALIDATE_MAT44(cmat) ;
   zim = mri_genalign_xyzwarp_volmap( imar , cmat ) ;
   if( zim != NULL ) ADDTO_IMARR(imar,zim) ;
   RETURN(imar) ;
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

void mri_genalign_affine_set_befafter( mat44 *ab , mat44 *af )
{
   if( ab == NULL || !ISVALID_MAT44(*ab) ){
     aff_use_before = 0 ; INVALIDATE_MAT44(aff_before) ;
   } else {
     aff_use_before = 1 ; aff_before = *ab ;
   }

   if( af == NULL || !ISVALID_MAT44(*af) ){
     aff_use_after = 0 ; INVALIDATE_MAT44(aff_after) ;
   } else {
     aff_use_after = 1 ; aff_after = *af ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/

void mri_genalign_affine_get_befafter( mat44 *ab , mat44 *af )
{
   if( ab != NULL ) *ab = aff_before ;
   if( af != NULL ) *af = aff_after  ;
}

/*--------------------------------------------------------------------------*/

void mri_genalign_affine_get_gammaijk( mat44 *gg )
{
  if( gg != NULL ) *gg = aff_gamijk ;
}

/*--------------------------------------------------------------------------*/

void mri_genalign_affine_get_gammaxyz( mat44 *gg )
{
  if( gg != NULL ) *gg = aff_gamxyz ;
}

/*--------------------------------------------------------------------------*/

void mri_genalign_affine_use_befafter( int bb , int aa )
{
   aff_use_before = ISVALID_MAT44(aff_before) && bb ;
   aff_use_after  = ISVALID_MAT44(aff_after)  && aa ;
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

 AFNI_OMP_START ;
#pragma omp parallel if( npt > 33333 )
 { int ii ;
#pragma omp for
   for( ii=0 ; ii < npt ; ii++ )
     MAT44_VEC( gam , xi[ii],yi[ii],zi[ii] , xo[ii],yo[ii],zo[ii] ) ;
 }
 AFNI_OMP_END ;

   return ;
}

/*--------------------------------------------------------------------------*/
/*! Similar to mri_genalign_affine(), but the 12 parameters are the matrix
    directly, with no physical interpretations such as angles, etc.
  * That is, this is the index-to-index matrix, not the coord-to-coord
    matrix (befafter stuff doesn't apply here, unlike mri_genalign_affine).
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

 AFNI_OMP_START ;
#pragma omp parallel if( npt > 33333 )
 { int ii ;
   for( ii=0 ; ii < npt ; ii++ )
     MAT44_VEC( gam , xi[ii],yi[ii],zi[ii] , xo[ii],yo[ii],zo[ii] ) ;
 }
 AFNI_OMP_END ;

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

 AFNI_OMP_START ;
#pragma omp parallel if( npt > 22222 )
 { int ii ; THD_mat33 dd,ee ; float aa,bb,cc , uu,vv,ww ;
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
 }
 AFNI_OMP_END ;

   return ;
}

/*--------------------------------------------------------------------------*/

/* Legendre polynomials */

#define LP1(x) (x)
#define LP2(x) ((x)*(x)-0.3333333f)
#define LP3(x) (((x)*(x)-0.6f)*(x))*1.5f
#define LP4(x) ((x)*(x)*((x)*(x)-0.857143f)+0.0857143f)*2.0f
#define LP5(x) (((x)*(x)*((x)*(x)-1.11111f)+0.238095f)*(x))*3.0f
#define LP6(x) ((x)*(x)*((x)*(x)*((x)*(x)-1.36364f)+0.454545f)-0.021645f)*6.0f
#define LP7(x) (((x)*(x)*((x)*(x)*((x)*(x)-1.61538f)+0.734266f)-0.081585f)*(x))*10.0f

#define LP8(x) ( (x)*(x) * \
               ( (x)*(x) * \
               ( (x)*(x) * \
               ( (x)*(x) - 1.86667f ) + 1.07692f ) - 0.195804f ) + 0.0054390f )*18.0f

#define LP9(x) ( ( (x)*(x) * \
                 ( (x)*(x) * \
                 ( (x)*(x) * \
                 ( (x)*(x) - 2.11765f ) + 1.48235f ) - 0.380090f ) + 0.0259153f ) * (x) )*32.0f

/* Gegenbauer (alpha=-0.5) polynomials */

#define GP1(x) (x)
#define GP2(x) (0.1666667f-0.5f*(x)*(x))              /* G2(x)-1/3 : orthogonal to 1 */
#define GP3(x) ((0.3f-0.5f*(x)*(x))*(x))              /* G3(x)-x/5 : orthogonal to x */
#define GP4(x) (-0.125f+(0.75f-0.625f*(x)*(x))*(x)*(x))
#define GP5(x) ((-0.375f+(1.25f-0.875f*(x)*(x))*(x)*(x))*(x))
#define GP6(x) (0.0625f+(-0.9375f+(2.1875f-1.3125f*(x)*(x))*(x)*(x))*(x)*(x))
#define GP7(x) ((0.3125f+(-2.1875f+(3.9375f-2.0625f*(x)*(x))*(x)*(x))*(x)*(x))*(x))
#define GP8(x) (-0.0390625f+(1.09375f+(-4.921875f+(7.21875f-3.3515625f*(x)*(x))*(x)*(x))*(x)*(x))*(x)*(x))
#define GP9(x) ((-0.2734375f+(3.28125f+(-10.828125f+(13.40625f-5.5859375f*(x)*(x))*(x)*(x))*(x)*(x))*(x)*(x))*(x))

/* Hermite polynomials */

#define HP1(x) (x)
#define HP2(x) 0.4f*((x)*(x)-2.0f)
#define HP3(x) 0.2f*((x)*(2.0f*(x)*(x)-3.0f))
#define HP4(x) 0.05f*((4.0f*(x)*(x)-12.0f)*(x)*(x)+3.0f)
#define HP5(x) 0.15f*((x)*((4.0f*(x)*(x)-20.0f)*(x)*(x)+15.0f))
#define HP6(x) 0.2f*(((2.0f*(x)*(x)-15.0f)*(x)*(x)+22.5f)*(x)*(x)-3.75f)
#define HP7(x) 0.06f*(0.5f*(x)*(((8.0f*(x)*(x)-84.0f)*(x)*(x)+210.0f)*(x)*(x)-105.0f))
#define HP8(x) 0.1f*(((((x)*(x)-14.0f)*(x)*(x)+52.5f)*(x)*(x)-52.5f)*(x)*(x)+6.5625f)
#define HP9(x) 0.004f*((x)*((((16.0f*(x)*(x)-288.0f)*(x)*(x)+1512.0f)*(x)*(x)-2520.0f)*(x)*(x)+945.0f))

#define HH1(x) (x)
#define HH2(x) HP2(3.0f*(x))*exp(-2.0f*(x)*(x))
#define HH3(x) HP3(3.0f*(x))*exp(-3.0f*(x)*(x))
#define HH4(x) HP4(3.0f*(x))*exp(-5.0f*(x)*(x))
#define HH5(x) HP5(3.0f*(x))*exp(-6.0f*(x)*(x))
#define HH6(x) HP6(3.0f*(x))*exp(-6.0f*(x)*(x))
#define HH7(x) HP7(3.0f*(x))*exp(-7.0f*(x)*(x))
#define HH8(x) HP8(3.0f*(x))*exp(-7.0f*(x)*(x))
#define HH9(x) HP9(3.0f*(x))*exp(-7.0f*(x)*(x))

#undef USE_GEGEN
#undef USE_HERMITE

/*----------------------------------------------------------------------------*/
#define PP1(x) (x)

static float LEG2(float x){ return LP2(x); }  /* 28 Mar 2013: */
static float LEG3(float x){ return LP3(x); }  /* switch the PPn(x) from */
static float LEG4(float x){ return LP4(x); }  /* being macros to functions */
static float LEG5(float x){ return LP5(x); }
static float LEG6(float x){ return LP6(x); }
static float LEG7(float x){ return LP7(x); }
static float LEG8(float x){ return LP8(x); }
static float LEG9(float x){ return LP9(x); }

static float HER2(float x){ return HH2(x); }
static float HER3(float x){ return HH3(x); }
static float HER4(float x){ return HH4(x); }
static float HER5(float x){ return HH5(x); }
static float HER6(float x){ return HH6(x); }
static float HER7(float x){ return HH7(x); }
static float HER8(float x){ return HH8(x); }
static float HER9(float x){ return HH9(x); }

static float (*PP2)(float) = LEG2 ;
static float (*PP3)(float) = LEG3 ;
static float (*PP4)(float) = LEG4 ;
static float (*PP5)(float) = LEG5 ;
static float (*PP6)(float) = LEG6 ;
static float (*PP7)(float) = LEG7 ;
static float (*PP8)(float) = LEG8 ;
static float (*PP9)(float) = LEG9 ;

void GA_setup_polywarp( int pcode )  /* 28 Mar 2013 */
{
   switch( pcode ){
     default:
       PP2 = LEG2 ; PP3 = LEG3 ; PP4 = LEG4 ; PP5 = LEG5 ;
       PP6 = LEG6 ; PP7 = LEG7 ; PP8 = LEG8 ; PP9 = LEG9 ;
     break ;

     case GA_HERMITE:
       PP2 = HER2 ; PP3 = HER3 ; PP4 = HER4 ; PP5 = HER5 ;
       PP6 = HER6 ; PP7 = HER7 ; PP8 = HER8 ; PP9 = HER9 ;
     break ;
   }
   return ;
}

/*********************************/
#if 0
#ifdef USE_GEGEN
# define PP1 GP1
# define PP2 GP2
# define PP3 GP3
# define PP4 GP4
# define PP5 GP5
# define PP6 GP6
# define PP7 GP7
# define PP8 GP8
# define PP9 GP9
#endif
#ifdef USE_HERMITE
# define PP1 HH1
# define PP2 HH2
# define PP3 HH3
# define PP4 HH4
# define PP5 HH5
# define PP6 HH6
# define PP7 HH7
# define PP8 HH8
# define PP9 HH9
#else
# define PP1 LP1
# define PP2 LP2
# define PP3 LP3
# define PP4 LP4
# define PP5 LP5
# define PP6 LP6
# define PP7 LP7
# define PP8 LP8
# define PP9 LP9
#endif
#endif
/*********************************/

/* 3D product functions of various orders 2..9 */

#define P2_xx(x,y,z) PP2(x)
#define P2_xy(x,y,z) PP1(x)*PP1(y)
#define P2_xz(x,y,z) PP1(x)*PP1(z)
#define P2_yy(x,y,z) PP2(y)
#define P2_yz(x,y,z) PP1(y)*PP1(z)
#define P2_zz(x,y,z) PP2(z)

#define P3_xxx(x,y,z) PP3(x)
#define P3_xxy(x,y,z) PP2(x)*PP1(y)
#define P3_xxz(x,y,z) PP2(x)*PP1(z)
#define P3_xyy(x,y,z) PP1(x)*PP2(y)
#define P3_xzz(x,y,z) PP1(x)*PP2(z)
#define P3_xyz(x,y,z) PP1(x)*PP1(y)*PP1(z)
#define P3_yyy(x,y,z) PP3(y)
#define P3_yyz(x,y,z) PP2(y)*PP1(z)
#define P3_yzz(x,y,z) PP1(y)*PP2(z)
#define P3_zzz(x,y,z) PP3(z)

#define P4_xxxx(x,y,z) PP4(x)
#define P4_xxxy(x,y,z) PP3(x)*PP1(y)
#define P4_xxxz(x,y,z) PP3(x)*PP1(z)
#define P4_xxyy(x,y,z) PP2(x)*PP2(y)
#define P4_xxzz(x,y,z) PP2(x)*PP2(z)
#define P4_xxyz(x,y,z) PP2(x)*PP1(y)*PP1(z)
#define P4_xyyy(x,y,z) PP1(x)*PP3(y)
#define P4_xyyz(x,y,z) PP1(x)*PP2(y)*PP1(z)
#define P4_xyzz(x,y,z) PP1(x)*PP1(y)*PP2(z)
#define P4_xzzz(x,y,z) PP1(x)*PP3(z)
#define P4_yyyy(x,y,z) PP4(y)
#define P4_yyyz(x,y,z) PP3(y)*PP1(z)
#define P4_yyzz(x,y,z) PP2(y)*PP2(z)
#define P4_yzzz(x,y,z) PP1(y)*PP3(z)
#define P4_zzzz(x,y,z) PP4(z)

#define P5_xxxxx(x,y,z) PP5(x)
#define P5_xxxxy(x,y,z) PP4(x)*PP1(y)
#define P5_xxxxz(x,y,z) PP4(x)*PP1(z)
#define P5_xxxyy(x,y,z) PP3(x)*PP2(y)
#define P5_xxxzz(x,y,z) PP3(x)*PP2(z)
#define P5_xxxyz(x,y,z) PP3(x)*PP1(y)*PP1(z)
#define P5_xxyyy(x,y,z) PP2(x)*PP3(y)
#define P5_xxyyz(x,y,z) PP2(x)*PP2(y)*PP1(z)
#define P5_xxyzz(x,y,z) PP2(x)*PP1(y)*PP2(z)
#define P5_xxzzz(x,y,z) PP2(x)*PP3(z)
#define P5_xyyyy(x,y,z) PP1(x)*PP4(y)
#define P5_xyyyz(x,y,z) PP1(x)*PP3(y)*PP1(z)
#define P5_xyyzz(x,y,z) PP1(x)*PP2(y)*PP2(z)
#define P5_xyzzz(x,y,z) PP1(x)*PP1(y)*PP3(z)
#define P5_xzzzz(x,y,z) PP1(x)*PP4(z)
#define P5_yyyyy(x,y,z) PP5(y)
#define P5_yyyyz(x,y,z) PP4(y)*PP1(z)
#define P5_yyyzz(x,y,z) PP3(y)*PP2(z)
#define P5_yyzzz(x,y,z) PP2(y)*PP3(z)
#define P5_yzzzz(x,y,z) PP1(y)*PP4(z)
#define P5_zzzzz(x,y,z) PP5(z)

#define P6_xxxxxx(x,y,z) PP6(x)
#define P6_xxxxxy(x,y,z) PP5(x)*PP1(y)
#define P6_xxxxxz(x,y,z) PP5(x)*PP1(z)
#define P6_xxxxyy(x,y,z) PP4(x)*PP2(y)
#define P6_xxxxzz(x,y,z) PP4(x)*PP2(z)
#define P6_xxxxyz(x,y,z) PP4(x)*PP1(y)*PP1(z)
#define P6_xxxyyy(x,y,z) PP3(x)*PP3(y)
#define P6_xxxyyz(x,y,z) PP3(x)*PP2(y)*PP1(z)
#define P6_xxxyzz(x,y,z) PP3(x)*PP1(y)*PP2(z)
#define P6_xxxzzz(x,y,z) PP3(x)*PP3(z)
#define P6_xxyyyy(x,y,z) PP2(x)*PP4(y)
#define P6_xxyyyz(x,y,z) PP2(x)*PP3(y)*PP1(z)
#define P6_xxyyzz(x,y,z) PP2(x)*PP2(y)*PP2(z)
#define P6_xxyzzz(x,y,z) PP2(x)*PP1(y)*PP3(z)
#define P6_xxzzzz(x,y,z) PP2(x)*PP4(z)
#define P6_xyyyyy(x,y,z) PP1(x)*PP5(y)
#define P6_xyyyyz(x,y,z) PP1(x)*PP4(y)*PP1(z)
#define P6_xyyyzz(x,y,z) PP1(x)*PP3(y)*PP2(z)
#define P6_xyyzzz(x,y,z) PP1(x)*PP2(y)*PP3(z)
#define P6_xyzzzz(x,y,z) PP1(x)*PP1(y)*PP4(z)
#define P6_xzzzzz(x,y,z) PP1(x)*PP5(z)
#define P6_yyyyyy(x,y,z) PP6(y)
#define P6_yyyyyz(x,y,z) PP5(y)*PP1(z)
#define P6_yyyyzz(x,y,z) PP4(y)*PP2(z)
#define P6_yyyzzz(x,y,z) PP3(y)*PP3(z)
#define P6_yyzzzz(x,y,z) PP2(y)*PP4(z)
#define P6_yzzzzz(x,y,z) PP1(y)*PP5(z)
#define P6_zzzzzz(x,y,z) PP6(z)

#define P7_xxxxxxx(x,y,z) PP7(x)
#define P7_xxxxxxy(x,y,z) PP6(x)*PP1(y)
#define P7_xxxxxxz(x,y,z) PP6(x)*PP1(z)
#define P7_xxxxxyy(x,y,z) PP5(x)*PP2(y)
#define P7_xxxxxzz(x,y,z) PP5(x)*PP2(z)
#define P7_xxxxxyz(x,y,z) PP5(x)*PP1(y)*PP1(z)
#define P7_xxxxyyy(x,y,z) PP4(x)*PP3(y)
#define P7_xxxxyyz(x,y,z) PP4(x)*PP2(y)*PP1(z)
#define P7_xxxxyzz(x,y,z) PP4(x)*PP1(y)*PP2(z)
#define P7_xxxxzzz(x,y,z) PP4(x)*PP3(z)
#define P7_xxxyyyy(x,y,z) PP3(x)*PP4(y)
#define P7_xxxyyyz(x,y,z) PP3(x)*PP3(y)*PP1(z)
#define P7_xxxyyzz(x,y,z) PP3(x)*PP2(y)*PP2(z)
#define P7_xxxyzzz(x,y,z) PP3(x)*PP1(y)*PP3(z)
#define P7_xxxzzzz(x,y,z) PP3(x)*PP4(z)
#define P7_xxyyyyy(x,y,z) PP2(x)*PP5(y)
#define P7_xxyyyyz(x,y,z) PP2(x)*PP4(y)*PP1(z)
#define P7_xxyyyzz(x,y,z) PP2(x)*PP3(y)*PP2(z)
#define P7_xxyyzzz(x,y,z) PP2(x)*PP2(y)*PP3(z)
#define P7_xxyzzzz(x,y,z) PP2(x)*PP1(y)*PP4(z)
#define P7_xxzzzzz(x,y,z) PP2(x)*PP5(z)
#define P7_xyyyyyy(x,y,z) PP1(x)*PP6(y)
#define P7_xyyyyyz(x,y,z) PP1(x)*PP5(y)*PP1(z)
#define P7_xyyyyzz(x,y,z) PP1(x)*PP4(y)*PP2(z)
#define P7_xyyyzzz(x,y,z) PP1(x)*PP3(y)*PP3(z)
#define P7_xyyzzzz(x,y,z) PP1(x)*PP2(y)*PP4(z)
#define P7_xyzzzzz(x,y,z) PP1(x)*PP1(y)*PP5(z)
#define P7_xzzzzzz(x,y,z) PP1(x)*PP6(z)
#define P7_yyyyyyy(x,y,z) PP7(y)
#define P7_yyyyyyz(x,y,z) PP6(y)*PP1(z)
#define P7_yyyyyzz(x,y,z) PP5(y)*PP2(z)
#define P7_yyyyzzz(x,y,z) PP4(y)*PP3(z)
#define P7_yyyzzzz(x,y,z) PP3(y)*PP4(z)
#define P7_yyzzzzz(x,y,z) PP2(y)*PP5(z)
#define P7_yzzzzzz(x,y,z) PP1(y)*PP6(z)
#define P7_zzzzzzz(x,y,z) PP7(z)

#define P8_xxxxxxxx(x,y,z) PP8(x)
#define P8_xxxxxxxy(x,y,z) PP7(x)*PP1(y)
#define P8_xxxxxxxz(x,y,z) PP7(x)*PP1(z)
#define P8_xxxxxxyy(x,y,z) PP6(x)*PP2(y)
#define P8_xxxxxxzz(x,y,z) PP6(x)*PP2(z)
#define P8_xxxxxxyz(x,y,z) PP6(x)*PP1(y)*PP1(z)
#define P8_xxxxxyyy(x,y,z) PP5(x)*PP3(y)
#define P8_xxxxxyyz(x,y,z) PP5(x)*PP2(y)*PP1(z)
#define P8_xxxxxyzz(x,y,z) PP5(x)*PP1(y)*PP2(z)
#define P8_xxxxxzzz(x,y,z) PP5(x)*PP3(z)
#define P8_xxxxyyyy(x,y,z) PP4(x)*PP4(y)
#define P8_xxxxyyyz(x,y,z) PP4(x)*PP3(y)*PP1(z)
#define P8_xxxxyyzz(x,y,z) PP4(x)*PP2(y)*PP2(z)
#define P8_xxxxyzzz(x,y,z) PP4(x)*PP1(y)*PP3(z)
#define P8_xxxxzzzz(x,y,z) PP4(x)*PP4(z)
#define P8_xxxyyyyy(x,y,z) PP3(x)*PP5(y)
#define P8_xxxyyyyz(x,y,z) PP3(x)*PP4(y)*PP1(z)
#define P8_xxxyyyzz(x,y,z) PP3(x)*PP3(y)*PP2(z)
#define P8_xxxyyzzz(x,y,z) PP3(x)*PP2(y)*PP3(z)
#define P8_xxxyzzzz(x,y,z) PP3(x)*PP1(y)*PP4(z)
#define P8_xxxzzzzz(x,y,z) PP3(x)*PP5(z)
#define P8_xxyyyyyy(x,y,z) PP2(x)*PP6(y)
#define P8_xxyyyyyz(x,y,z) PP2(x)*PP5(y)*PP1(z)
#define P8_xxyyyyzz(x,y,z) PP2(x)*PP4(y)*PP2(z)
#define P8_xxyyyzzz(x,y,z) PP2(x)*PP3(y)*PP3(z)
#define P8_xxyyzzzz(x,y,z) PP2(x)*PP2(y)*PP4(z)
#define P8_xxyzzzzz(x,y,z) PP2(x)*PP1(y)*PP5(z)
#define P8_xxzzzzzz(x,y,z) PP2(x)*PP6(z)
#define P8_xyyyyyyy(x,y,z) PP1(x)*PP7(y)
#define P8_xyyyyyyz(x,y,z) PP1(x)*PP6(y)*PP1(z)
#define P8_xyyyyyzz(x,y,z) PP1(x)*PP5(y)*PP2(z)
#define P8_xyyyyzzz(x,y,z) PP1(x)*PP4(y)*PP3(z)
#define P8_xyyyzzzz(x,y,z) PP1(x)*PP3(y)*PP4(z)
#define P8_xyyzzzzz(x,y,z) PP1(x)*PP2(y)*PP5(z)
#define P8_xyzzzzzz(x,y,z) PP1(x)*PP1(y)*PP6(z)
#define P8_xzzzzzzz(x,y,z) PP1(x)*PP7(z)
#define P8_yyyyyyyy(x,y,z) PP8(y)
#define P8_yyyyyyyz(x,y,z) PP7(y)*PP1(z)
#define P8_yyyyyyzz(x,y,z) PP6(y)*PP2(z)
#define P8_yyyyyzzz(x,y,z) PP5(y)*PP3(z)
#define P8_yyyyzzzz(x,y,z) PP4(y)*PP4(z)
#define P8_yyyzzzzz(x,y,z) PP3(y)*PP5(z)
#define P8_yyzzzzzz(x,y,z) PP2(y)*PP6(z)
#define P8_yzzzzzzz(x,y,z) PP1(y)*PP7(z)
#define P8_zzzzzzzz(x,y,z) PP8(z)

#define P9_xxxxxxxxx(x,y,z) PP9(x)
#define P9_xxxxxxxxy(x,y,z) PP8(x)*PP1(y)
#define P9_xxxxxxxxz(x,y,z) PP8(x)*PP1(z)
#define P9_xxxxxxxyy(x,y,z) PP7(x)*PP2(y)
#define P9_xxxxxxxzz(x,y,z) PP7(x)*PP2(z)
#define P9_xxxxxxxyz(x,y,z) PP7(x)*PP1(y)*PP1(z)
#define P9_xxxxxxyyy(x,y,z) PP6(x)*PP3(y)
#define P9_xxxxxxyyz(x,y,z) PP6(x)*PP2(y)*PP1(z)
#define P9_xxxxxxyzz(x,y,z) PP6(x)*PP1(y)*PP2(z)
#define P9_xxxxxxzzz(x,y,z) PP6(x)*PP3(z)
#define P9_xxxxxyyyy(x,y,z) PP5(x)*PP4(y)
#define P9_xxxxxyyyz(x,y,z) PP5(x)*PP3(y)*PP1(z)
#define P9_xxxxxyyzz(x,y,z) PP5(x)*PP2(y)*PP2(z)
#define P9_xxxxxyzzz(x,y,z) PP5(x)*PP1(y)*PP3(z)
#define P9_xxxxxzzzz(x,y,z) PP5(x)*PP4(z)
#define P9_xxxxyyyyy(x,y,z) PP4(x)*PP5(y)
#define P9_xxxxyyyyz(x,y,z) PP4(x)*PP4(y)*PP1(z)
#define P9_xxxxyyyzz(x,y,z) PP4(x)*PP3(y)*PP2(z)
#define P9_xxxxyyzzz(x,y,z) PP4(x)*PP2(y)*PP3(z)
#define P9_xxxxyzzzz(x,y,z) PP4(x)*PP1(y)*PP4(z)
#define P9_xxxxzzzzz(x,y,z) PP4(x)*PP5(z)
#define P9_xxxyyyyyy(x,y,z) PP3(x)*PP6(y)
#define P9_xxxyyyyyz(x,y,z) PP3(x)*PP5(y)*PP1(z)
#define P9_xxxyyyyzz(x,y,z) PP3(x)*PP4(y)*PP2(z)
#define P9_xxxyyyzzz(x,y,z) PP3(x)*PP3(y)*PP3(z)
#define P9_xxxyyzzzz(x,y,z) PP3(x)*PP2(y)*PP4(z)
#define P9_xxxyzzzzz(x,y,z) PP3(x)*PP1(y)*PP5(z)
#define P9_xxxzzzzzz(x,y,z) PP3(x)*PP6(z)
#define P9_xxyyyyyyy(x,y,z) PP2(x)*PP7(y)
#define P9_xxyyyyyyz(x,y,z) PP2(x)*PP6(y)*PP1(z)
#define P9_xxyyyyyzz(x,y,z) PP2(x)*PP5(y)*PP2(z)
#define P9_xxyyyyzzz(x,y,z) PP2(x)*PP4(y)*PP3(z)
#define P9_xxyyyzzzz(x,y,z) PP2(x)*PP3(y)*PP4(z)
#define P9_xxyyzzzzz(x,y,z) PP2(x)*PP2(y)*PP5(z)
#define P9_xxyzzzzzz(x,y,z) PP2(x)*PP1(y)*PP6(z)
#define P9_xxzzzzzzz(x,y,z) PP2(x)*PP7(z)
#define P9_xyyyyyyyy(x,y,z) PP1(x)*PP8(y)
#define P9_xyyyyyyyz(x,y,z) PP1(x)*PP7(y)*PP1(z)
#define P9_xyyyyyyzz(x,y,z) PP1(x)*PP6(y)*PP2(z)
#define P9_xyyyyyzzz(x,y,z) PP1(x)*PP5(y)*PP3(z)
#define P9_xyyyyzzzz(x,y,z) PP1(x)*PP4(y)*PP4(z)
#define P9_xyyyzzzzz(x,y,z) PP1(x)*PP3(y)*PP5(z)
#define P9_xyyzzzzzz(x,y,z) PP1(x)*PP2(y)*PP6(z)
#define P9_xyzzzzzzz(x,y,z) PP1(x)*PP1(y)*PP7(z)
#define P9_xzzzzzzzz(x,y,z) PP1(x)*PP8(z)
#define P9_yyyyyyyyy(x,y,z) PP9(y)
#define P9_yyyyyyyyz(x,y,z) PP8(y)*PP1(z)
#define P9_yyyyyyyzz(x,y,z) PP7(y)*PP2(z)
#define P9_yyyyyyzzz(x,y,z) PP6(y)*PP3(z)
#define P9_yyyyyzzzz(x,y,z) PP5(y)*PP4(z)
#define P9_yyyyzzzzz(x,y,z) PP4(y)*PP5(z)
#define P9_yyyzzzzzz(x,y,z) PP3(y)*PP6(z)
#define P9_yyzzzzzzz(x,y,z) PP2(y)*PP7(z)
#define P9_yzzzzzzzz(x,y,z) PP1(y)*PP8(z)
#define P9_zzzzzzzzz(x,y,z) PP9(z)

/* number of polynomials in each set; number of params is times 3 (for x,y,z) */

#define NPOLCUBI   16  /* = 6+10 */
#define NPOLQUIN   52  /* = 6+10+15+21 */
#define NPOLHEPT  116  /* = 6+10+15+21+28+36 */
#define NPOLNONI  216  /* = 6+10+15+21+28+36+45+55 */

/* number of nonlinear terms at order k (in each dimension) */

#define NPOL(k) (((k)+1)*((k)+2)*((k)+3)/6-4)

/* FIXYZ macro makes sure arg is between -1 and 1 */

#define PRAMP(x) ( 0.8f + ((x)-0.8f) / (1.0f + 5.0f*((x)-0.8f)) )
#define NRAMP(x) (-0.8f + ((x)+0.8f) / (1.0f - 5.0f*((x)+0.8f)) )
#define FIXYZ(q) if(q > 0.8f) q=PRAMP(q); else if(q < -0.8f) q=NRAMP(q)


/*--------------------------------------------------------------------------*/
#define CCx   1
#define CCy   2
#define CCz   4
#define CCxy  3
#define CCxz  5
#define CCyz  6
#define CCxyz 7

static byte CCoordCCode[NPOLNONI] = {
  CCx, CCxy, CCxz, CCy, CCyz, CCz, CCx, CCxy, CCxz, CCxy, CCxz, CCxyz,
  CCy, CCyz, CCyz, CCz, CCx, CCxy, CCxz, CCxy, CCxz, CCxyz, CCxy, CCxyz,
  CCxyz, CCxz, CCy, CCyz, CCyz, CCyz, CCz, CCx, CCxy, CCxz, CCxy, CCxz, CCxyz,
  CCxy, CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxz, CCy, CCyz, CCyz, CCyz,
  CCyz, CCz, CCx, CCxy, CCxz, CCxy, CCxz, CCxyz, CCxy, CCxyz, CCxyz, CCxz,
  CCxy, CCxyz, CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxyz, CCxz, CCy,
  CCyz, CCyz, CCyz, CCyz, CCyz, CCz, CCx, CCxy, CCxz, CCxy, CCxz, CCxyz,
  CCxy, CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz,
  CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxyz, CCxyz, CCxz, CCy, CCyz,
  CCyz, CCyz, CCyz, CCyz, CCyz, CCz, CCx, CCxy, CCxz, CCxy, CCxz, CCxyz,
  CCxy, CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz,
  CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxyz, CCxyz, CCxz, CCxy, CCxyz,
  CCxyz, CCxyz, CCxyz, CCxyz, CCxyz, CCxz, CCy, CCyz, CCyz, CCyz, CCyz, CCyz,
  CCyz, CCyz, CCz, CCx, CCxy, CCxz, CCxy, CCxz, CCxyz, CCxy, CCxyz, CCxyz,
  CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxyz, CCxz,
  CCxy, CCxyz, CCxyz, CCxyz, CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxyz,
  CCxyz, CCxyz, CCxz, CCxy, CCxyz, CCxyz, CCxyz, CCxyz, CCxyz, CCxyz, CCxyz, CCxz,
  CCy, CCyz, CCyz, CCyz, CCyz, CCyz, CCyz, CCyz, CCyz, CCz } ;

int GA_polywarp_coordcode( int pnum )
{
   if( pnum < 0 || pnum >= NPOLNONI ) return 0 ;
   return (int)CCoordCCode[pnum] ;
}

static char *PolyFuncName[NPOLNONI] = {
  "x^2" , "x*y" , "x*z" , "y^2" , "y*z" , "z^2" , "x^3" , "x^2*y" ,
  "x^2*z" , "x*y^2" , "x*z^2" , "x*y*z" , "y^3" , "y^2*z" , "y*z^2" , "z^3" ,
  "x^4" , "x^3*y" , "x^3*z" , "x^2*y^2" , "x^2*z^2" , "x^2*y*z" , "x*y^3" , "x*y^2*z" ,
  "x*y*z^2" , "x*z^3" , "y^4" , "y^3*z" , "y^2*z^2" , "y*z^3" , "z^4" , "x^5" , "x^4*y" ,
  "x^4*z" , "x^3*y^2" , "x^3*z^2" , "x^3*y*z" , "x^2*y^3" , "x^2*y^2*z" , "x^2*y*z^2" , "x^2*z^3" ,
  "x*y^4" , "x*y^3*z" , "x*y^2*z^2" , "x*y*z^3" , "x*z^4" , "y^5" , "y^4*z" , "y^3*z^2" ,
  "y^2*z^3" , "y*z^4" , "z^5" , "x^6" , "x^5*y" , "x^5*z" , "x^4*y^2" , "x^4*z^2" ,
  "x^4*y*z" , "x^3*y^3" , "x^3*y^2*z" , "x^3*y*z^2" , "x^3*z^3" , "x^2*y^4" , "x^2*y^3*z" , "x^2*y^2*z^2" ,
  "x^2*y*z^3" , "x^2*z^4" , "x*y^5" , "x*y^4*z" , "x*y^3*z^2" , "x*y^2*z^3" , "x*y*z^4" , "x*z^5" ,
  "y^6" , "y^5*z" , "y^4*z^2" , "y^3*z^3" , "y^2*z^4" , "y*z^5" , "z^6" , "x^7" , "x^6*y" ,
  "x^6*z" , "x^5*y^2" , "x^5*z^2" , "x^5*y*z" , "x^4*y^3" , "x^4*y^2*z" , "x^4*y*z^2" , "x^4*z^3" ,
  "x^3*y^4" , "x^3*y^3*z" , "x^3*y^2*z^2" , "x^3*y*z^3" , "x^3*z^4" , "x^2*y^5" , "x^2*y^4*z" , "x^2*y^3*z^2" ,
  "x^2*y^2*z^3" , "x^2*y*z^4" , "x^2*z^5" , "x*y^6" , "x*y^5*z" , "x*y^4*z^2" , "x*y^3*z^3" , "x*y^2*z^4" ,
  "x*y*z^5" , "x*z^6" , "y^7" , "y^6*z" , "y^5*z^2" , "y^4*z^3" , "y^3*z^4" , "y^2*z^5" ,
  "y*z^6" , "z^7" , "x^8" , "x^7*y" , "x^7*z" , "x^6*y^2" , "x^6*z^2" , "x^6*y*z" ,
  "x^5*y^3" , "x^5*y^2*z" , "x^5*y*z^2" , "x^5*z^3" , "x^4*y^4" , "x^4*y^3*z" , "x^4*y^2*z^2" , "x^4*y*z^3" ,
  "x^4*z^4" , "x^3*y^5" , "x^3*y^4*z" , "x^3*y^3*z^2" , "x^3*y^2*z^3" , "x^3*y*z^4" , "x^3*z^5" , "x^2*y^6" ,
  "x^2*y^5*z" , "x^2*y^4*z^2" , "x^2*y^3*z^3" , "x^2*y^2*z^4" , "x^2*y*z^5" , "x^2*z^6" , "x*y^7" , "x*y^6*z" ,
  "x*y^5*z^2" , "x*y^4*z^3" , "x*y^3*z^4" , "x*y^2*z^5" , "x*y*z^6" , "x*z^7" , "y^8" , "y^7*z" ,
  "y^6*z^2" , "y^5*z^3" , "y^4*z^4" , "y^3*z^5" , "y^2*z^6" , "y*z^7" , "z^8" , "x^9" , "x^8*y" , "x^8*z" ,
  "x^7*y^2" , "x^7*z^2" , "x^7*y*z" , "x^6*y^3" , "x^6*y^2*z" , "x^6*y*z^2" , "x^6*z^3" ,
  "x^5*y^4" , "x^5*y^3*z" , "x^5*y^2*z^2" , "x^5*y*z^3" , "x^5*z^4" , "x^4*y^5" , "x^4*y^4*z" , "x^4*y^3*z^2" ,
  "x^4*y^2*z^3" , "x^4*y*z^4" , "x^4*z^5" , "x^3*y^6" , "x^3*y^5*z" , "x^3*y^4*z^2" ,
  "x^3*y^3*z^3" , "x^3*y^2*z^4" , "x^3*y*z^5" , "x^3*z^6" , "x^2*y^7" , "x^2*y^6*z" , "x^2*y^5*z^2" ,
  "x^2*y^4*z^3" , "x^2*y^3*z^4" , "x^2*y^2*z^5" , "x^2*y*z^6" , "x^2*z^7" , "x*y^8" , "x*y^7*z" ,
  "x*y^6*z^2" , "x*y^5*z^3" , "x*y^4*z^4" , "x*y^3*z^5" , "x*y^2*z^6" , "x*y*z^7" , "x*z^8" ,
  "y^9" , "y^8*z" , "y^7*z^2" , "y^6*z^3" , "y^5*z^4" , "y^4*z^5" , "y^3*z^6" , "y^2*z^7" , "y*z^8" , "z^9" } ;

char * GA_polywarp_funcname( int pnum )
{
   if( pnum < 0 || pnum >= NPOLNONI ) return NULL ;
   return PolyFuncName[pnum] ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for cubic polynomials. */

void mri_genalign_cubic( int npar, float *wpar ,
                         int npt , float *xi, float *yi, float *zi ,
                                   float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   static float xcen,ycen,zcen,xyzfac,xyzinv , ppar[3*NPOLCUBI] ;
   static int puse[NPOLCUBI] , pall ;

   /** new parameters ==> setup matrix */

   if( npar >= 3*NPOLCUBI+16 && wpar != NULL ){
     int aa=aff_use_after , ab=aff_use_before , jj ;

     xcen   = wpar[12+3*NPOLCUBI] ;  /* the fake (non-varying) parameters */
     ycen   = wpar[13+3*NPOLCUBI] ;
     zcen   = wpar[14+3*NPOLCUBI] ;
     xyzfac = wpar[15+3*NPOLCUBI] ; xyzinv = 1.0f / xyzfac ;

     aff_use_before = aff_use_after = 0;
     gam = GA_setup_affine( 12 , wpar ) ;  /* affine param setup */
     aff_use_before = ab; aff_use_after = aa;

     for( jj=0 ; jj < 3*NPOLCUBI ; jj++ )          /* save polynomial params */
       ppar[jj] = wpar[jj+12] * xyzinv ;
     for( pall=jj=0 ; jj < NPOLCUBI ; jj++ ){      /* mark which ones to use */
       puse[jj] = (ppar[3*jj  ] != 0.0f) ||
                  (ppar[3*jj+1] != 0.0f) || (ppar[3*jj+2] != 0.0f) ;
       pall += puse[jj] ;
     }
     pall = ( pall >= (int)(0.9f*NPOLCUBI) ) ;

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

 AFNI_OMP_START ;
#pragma omp parallel if( npt > 6666 )
 { int ii,jj,kk ; float aa,bb,cc , uu,vv,ww , pv[NPOLCUBI] ;
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
     FIXYZ(uu) ; FIXYZ(vv) ; FIXYZ(ww) ;

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
       for( kk=jj=0 ; jj < NPOLCUBI ; jj++,kk+=3 ){
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
       for( kk=jj=0 ; jj < NPOLCUBI ; jj++,kk+=3 ){
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
 }
 AFNI_OMP_END ;

   return ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for quintic polynomials. */

void mri_genalign_quintic( int npar, float *wpar ,
                           int npt , float *xi, float *yi, float *zi ,
                                     float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   static float xcen,ycen,zcen,xyzfac,xyzinv , ppar[3*NPOLQUIN] ;
   static int puse[NPOLQUIN] , pall ;

   /** new parameters ==> setup matrix */

   if( npar >= 3*NPOLQUIN+16 && wpar != NULL ){
     int aa=aff_use_after , ab=aff_use_before , jj ;

     xcen   = wpar[12+3*NPOLQUIN] ;  /* the fake (non-varying) parameters */
     ycen   = wpar[13+3*NPOLQUIN] ;
     zcen   = wpar[14+3*NPOLQUIN] ;
     xyzfac = wpar[15+3*NPOLQUIN] ; xyzinv = 1.0f / xyzfac ;

     aff_use_before = aff_use_after = 0;
     gam = GA_setup_affine( 12 , wpar ) ;  /* affine param setup */
     aff_use_before = ab; aff_use_after = aa;

     for( jj=0 ; jj < 3*NPOLQUIN ; jj++ )          /* save polynomial params */
       ppar[jj] = wpar[jj+12] * xyzinv ;
     for( pall=jj=0 ; jj < NPOLQUIN ; jj++ ){      /* mark which ones to use */
       puse[jj] = (ppar[3*jj  ] != 0.0f) ||
                  (ppar[3*jj+1] != 0.0f) || (ppar[3*jj+2] != 0.0f) ;
       pall += puse[jj] ;
     }
     pall = ( pall >= (int)(0.9f*NPOLQUIN) ) ;

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

 AFNI_OMP_START ;
#pragma omp parallel if( npt > 5555 )
 { int ii,jj,kk ; float aa,bb,cc , uu,vv,ww , pv[NPOLQUIN] ;
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
     FIXYZ(uu) ; FIXYZ(vv) ; FIXYZ(ww) ;

     /* polynomials */

     if( pall ){
       float p1x,p2x,p3x,p4x,p5x , p1y,p2y,p3y,p4y,p5y , p1z,p2z,p3z,p4z,p5z ;
       p1x = PP1(uu); p2x = PP2(uu); p3x = PP3(uu); p4x = PP4(uu); p5x = PP5(uu);
       p1y = PP1(vv); p2y = PP2(vv); p3y = PP3(vv); p4y = PP4(vv); p5y = PP5(vv);
       p1z = PP1(ww); p2z = PP2(ww); p3z = PP3(ww); p4z = PP4(ww); p5z = PP5(ww);

#define Q2_xx p2x
#define Q2_xy p1x*p1y
#define Q2_xz p1x*p1z
#define Q2_yy p2y
#define Q2_yz p1y*p1z
#define Q2_zz p2z
#define Q3_xxx p3x
#define Q3_xxy p2x*p1y
#define Q3_xxz p2x*p1z
#define Q3_xyy p1x*p2y
#define Q3_xzz p1x*p2z
#define Q3_xyz p1x*p1y*p1z
#define Q3_yyy p3y
#define Q3_yyz p2y*p1z
#define Q3_yzz p1y*p2z
#define Q3_zzz p3z
#define Q4_xxxx p4x
#define Q4_xxxy p3x*p1y
#define Q4_xxxz p3x*p1z
#define Q4_xxyy p2x*p2y
#define Q4_xxzz p2x*p2z
#define Q4_xxyz p2x*p1y*p1z
#define Q4_xyyy p1x*p3y
#define Q4_xyyz p1x*p2y*p1z
#define Q4_xyzz p1x*p1y*p2z
#define Q4_xzzz p1x*p3z
#define Q4_yyyy p4y
#define Q4_yyyz p3y*p1z
#define Q4_yyzz p2y*p2z
#define Q4_yzzz p1y*p3z
#define Q4_zzzz p4z
#define Q5_xxxxx p5x
#define Q5_xxxxy p4x*p1y
#define Q5_xxxxz p4x*p1z
#define Q5_xxxyy p3x*p2y
#define Q5_xxxzz p3x*p2z
#define Q5_xxxyz p3x*p1y*p1z
#define Q5_xxyyy p2x*p3y
#define Q5_xxyyz p2x*p2y*p1z
#define Q5_xxyzz p2x*p1y*p2z
#define Q5_xxzzz p2x*p3z
#define Q5_xyyyy p1x*p4y
#define Q5_xyyyz p1x*p3y*p1z
#define Q5_xyyzz p1x*p2y*p2z
#define Q5_xyzzz p1x*p1y*p3z
#define Q5_xzzzz p1x*p4z
#define Q5_yyyyy p5y
#define Q5_yyyyz p4y*p1z
#define Q5_yyyzz p3y*p2z
#define Q5_yyzzz p2y*p3z
#define Q5_yzzzz p1y*p4z
#define Q5_zzzzz p5z
       pv[ 0] = Q2_xx  ; pv[ 1] = Q2_xy  ; pv[ 2] = Q2_xz  ; pv[ 3] = Q2_yy  ;
       pv[ 4] = Q2_yz  ; pv[ 5] = Q2_zz  ; pv[ 6] = Q3_xxx ; pv[ 7] = Q3_xxy ;
       pv[ 8] = Q3_xxz ; pv[ 9] = Q3_xyy ; pv[10] = Q3_xzz ; pv[11] = Q3_xyz ;
       pv[12] = Q3_yyy ; pv[13] = Q3_yyz ; pv[14] = Q3_yzz ; pv[15] = Q3_zzz ;
       pv[16] = Q4_xxxx ; pv[17] = Q4_xxxy ; pv[18] = Q4_xxxz ; pv[19] = Q4_xxyy ;
       pv[20] = Q4_xxzz ; pv[21] = Q4_xxyz ; pv[22] = Q4_xyyy ; pv[23] = Q4_xyyz ;
       pv[24] = Q4_xyzz ; pv[25] = Q4_xzzz ; pv[26] = Q4_yyyy ; pv[27] = Q4_yyyz ;
       pv[28] = Q4_yyzz ; pv[29] = Q4_yzzz ; pv[30] = Q4_zzzz ; pv[31] = Q5_xxxxx ;
       pv[32] = Q5_xxxxy ; pv[33] = Q5_xxxxz ; pv[34] = Q5_xxxyy ; pv[35] = Q5_xxxzz ;
       pv[36] = Q5_xxxyz ; pv[37] = Q5_xxyyy ; pv[38] = Q5_xxyyz ; pv[39] = Q5_xxyzz ;
       pv[40] = Q5_xxzzz ; pv[41] = Q5_xyyyy ; pv[42] = Q5_xyyyz ; pv[43] = Q5_xyyzz ;
       pv[44] = Q5_xyzzz ; pv[45] = Q5_xzzzz ; pv[46] = Q5_yyyyy ; pv[47] = Q5_yyyyz ;
       pv[48] = Q5_yyyzz ; pv[49] = Q5_yyzzz ; pv[50] = Q5_yzzzz ; pv[51] = Q5_zzzzz ;
       for( kk=jj=0 ; jj < NPOLQUIN ; jj++,kk+=3 ){
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
       for( kk=jj=0 ; jj < NPOLQUIN ; jj++,kk+=3 ){
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
 }
 AFNI_OMP_END ;

   return ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for heptic polynomials. */

void mri_genalign_heptic( int npar, float *wpar ,
                          int npt , float *xi, float *yi, float *zi ,
                                    float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   static float xcen,ycen,zcen,xyzfac,xyzinv , ppar[3*NPOLHEPT] ;
   static int puse[NPOLHEPT] , pall ;

   /** new parameters ==> setup matrix */

   if( npar >= 3*NPOLHEPT+16 && wpar != NULL ){
     int aa=aff_use_after , ab=aff_use_before , jj ;

     xcen   = wpar[12+3*NPOLHEPT] ;  /* the fake (non-varying) parameters */
     ycen   = wpar[13+3*NPOLHEPT] ;
     zcen   = wpar[14+3*NPOLHEPT] ;
     xyzfac = wpar[15+3*NPOLHEPT] ; xyzinv = 1.0f / xyzfac ;

     aff_use_before = aff_use_after = 0;
     gam = GA_setup_affine( 12 , wpar ) ;  /* affine param setup */
     aff_use_before = ab; aff_use_after = aa;

     for( jj=0 ; jj < 3*NPOLHEPT ; jj++ )          /* save polynomial params */
       ppar[jj] = wpar[jj+12] * xyzinv ;
     for( pall=jj=0 ; jj < NPOLHEPT ; jj++ ){      /* mark which ones to use */
       puse[jj] = (ppar[3*jj  ] != 0.0f) ||
                  (ppar[3*jj+1] != 0.0f) || (ppar[3*jj+2] != 0.0f) ;
       pall += puse[jj] ;
     }
     pall = ( pall >= (int)(0.9f*NPOLHEPT) ) ;
   }

   /* nothing to transform? (a setup call) */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /*--- do some work ---*/

 AFNI_OMP_START ;
#pragma omp parallel if( npt > 4444 )
 { int ii,jj,kk ; float aa,bb,cc , uu,vv,ww , pv[NPOLHEPT] ;
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
     FIXYZ(uu) ; FIXYZ(vv) ; FIXYZ(ww) ;

     /* polynomials */

     if( pall ){
       float p1x,p2x,p3x,p4x,p5x,p6x,p7x,
             p1y,p2y,p3y,p4y,p5y,p6y,p7y,
             p1z,p2z,p3z,p4z,p5z,p6z,p7z ;
       p1x=PP1(uu); p2x=PP2(uu); p3x=PP3(uu); p4x=PP4(uu); p5x=PP5(uu); p6x=PP6(uu); p7x=PP7(uu);
       p1y=PP1(vv); p2y=PP2(vv); p3y=PP3(vv); p4y=PP4(vv); p5y=PP5(vv); p6y=PP6(vv); p7y=PP7(vv);
       p1z=PP1(ww); p2z=PP2(ww); p3z=PP3(ww); p4z=PP4(ww); p5z=PP5(ww); p6z=PP6(ww); p7z=PP7(ww);

#define Q6_xxxxxx p6x
#define Q6_xxxxxy p5x*p1y
#define Q6_xxxxxz p5x*p1z
#define Q6_xxxxyy p4x*p2y
#define Q6_xxxxzz p4x*p2z
#define Q6_xxxxyz p4x*p1y*p1z
#define Q6_xxxyyy p3x*p3y
#define Q6_xxxyyz p3x*p2y*p1z
#define Q6_xxxyzz p3x*p1y*p2z
#define Q6_xxxzzz p3x*p3z
#define Q6_xxyyyy p2x*p4y
#define Q6_xxyyyz p2x*p3y*p1z
#define Q6_xxyyzz p2x*p2y*p2z
#define Q6_xxyzzz p2x*p1y*p3z
#define Q6_xxzzzz p2x*p4z
#define Q6_xyyyyy p1x*p5y
#define Q6_xyyyyz p1x*p4y*p1z
#define Q6_xyyyzz p1x*p3y*p2z
#define Q6_xyyzzz p1x*p2y*p3z
#define Q6_xyzzzz p1x*p1y*p4z
#define Q6_xzzzzz p1x*p5z
#define Q6_yyyyyy p6y
#define Q6_yyyyyz p5y*p1z
#define Q6_yyyyzz p4y*p2z
#define Q6_yyyzzz p3y*p3z
#define Q6_yyzzzz p2y*p4z
#define Q6_yzzzzz p1y*p5z
#define Q6_zzzzzz p6z
#define Q7_xxxxxxx p7x
#define Q7_xxxxxxy p6x*p1y
#define Q7_xxxxxxz p6x*p1z
#define Q7_xxxxxyy p5x*p2y
#define Q7_xxxxxzz p5x*p2z
#define Q7_xxxxxyz p5x*p1y*p1z
#define Q7_xxxxyyy p4x*p3y
#define Q7_xxxxyyz p4x*p2y*p1z
#define Q7_xxxxyzz p4x*p1y*p2z
#define Q7_xxxxzzz p4x*p3z
#define Q7_xxxyyyy p3x*p4y
#define Q7_xxxyyyz p3x*p3y*p1z
#define Q7_xxxyyzz p3x*p2y*p2z
#define Q7_xxxyzzz p3x*p1y*p3z
#define Q7_xxxzzzz p3x*p4z
#define Q7_xxyyyyy p2x*p5y
#define Q7_xxyyyyz p2x*p4y*p1z
#define Q7_xxyyyzz p2x*p3y*p2z
#define Q7_xxyyzzz p2x*p2y*p3z
#define Q7_xxyzzzz p2x*p1y*p4z
#define Q7_xxzzzzz p2x*p5z
#define Q7_xyyyyyy p1x*p6y
#define Q7_xyyyyyz p1x*p5y*p1z
#define Q7_xyyyyzz p1x*p4y*p2z
#define Q7_xyyyzzz p1x*p3y*p3z
#define Q7_xyyzzzz p1x*p2y*p4z
#define Q7_xyzzzzz p1x*p1y*p5z
#define Q7_xzzzzzz p1x*p6z
#define Q7_yyyyyyy p7y
#define Q7_yyyyyyz p6y*p1z
#define Q7_yyyyyzz p5y*p2z
#define Q7_yyyyzzz p4y*p3z
#define Q7_yyyzzzz p3y*p4z
#define Q7_yyzzzzz p2y*p5z
#define Q7_yzzzzzz p1y*p6z
#define Q7_zzzzzzz p7z

       pv[ 0] = Q2_xx  ; pv[ 1] = Q2_xy  ; pv[ 2] = Q2_xz  ; pv[ 3] = Q2_yy  ;
       pv[ 4] = Q2_yz  ; pv[ 5] = Q2_zz  ; pv[ 6] = Q3_xxx ; pv[ 7] = Q3_xxy ;
       pv[ 8] = Q3_xxz ; pv[ 9] = Q3_xyy ; pv[10] = Q3_xzz ; pv[11] = Q3_xyz ;
       pv[12] = Q3_yyy ; pv[13] = Q3_yyz ; pv[14] = Q3_yzz ; pv[15] = Q3_zzz ;
       pv[16] = Q4_xxxx ; pv[17] = Q4_xxxy ; pv[18] = Q4_xxxz ; pv[19] = Q4_xxyy ;
       pv[20] = Q4_xxzz ; pv[21] = Q4_xxyz ; pv[22] = Q4_xyyy ; pv[23] = Q4_xyyz ;
       pv[24] = Q4_xyzz ; pv[25] = Q4_xzzz ; pv[26] = Q4_yyyy ; pv[27] = Q4_yyyz ;
       pv[28] = Q4_yyzz ; pv[29] = Q4_yzzz ; pv[30] = Q4_zzzz ; pv[31] = Q5_xxxxx ;
       pv[32] = Q5_xxxxy ; pv[33] = Q5_xxxxz ; pv[34] = Q5_xxxyy ; pv[35] = Q5_xxxzz ;
       pv[36] = Q5_xxxyz ; pv[37] = Q5_xxyyy ; pv[38] = Q5_xxyyz ; pv[39] = Q5_xxyzz ;
       pv[40] = Q5_xxzzz ; pv[41] = Q5_xyyyy ; pv[42] = Q5_xyyyz ; pv[43] = Q5_xyyzz ;
       pv[44] = Q5_xyzzz ; pv[45] = Q5_xzzzz ; pv[46] = Q5_yyyyy ; pv[47] = Q5_yyyyz ;
       pv[48] = Q5_yyyzz ; pv[49] = Q5_yyzzz ; pv[50] = Q5_yzzzz ; pv[51] = Q5_zzzzz ;
       kk = 52 ;
       pv[kk++] = Q6_xxxxxx ; pv[kk++] = Q6_xxxxxy ; pv[kk++] = Q6_xxxxxz ;
       pv[kk++] = Q6_xxxxyy ; pv[kk++] = Q6_xxxxzz ; pv[kk++] = Q6_xxxxyz ;
       pv[kk++] = Q6_xxxyyy ; pv[kk++] = Q6_xxxyyz ; pv[kk++] = Q6_xxxyzz ;
       pv[kk++] = Q6_xxxzzz ; pv[kk++] = Q6_xxyyyy ; pv[kk++] = Q6_xxyyyz ;
       pv[kk++] = Q6_xxyyzz ; pv[kk++] = Q6_xxyzzz ; pv[kk++] = Q6_xxzzzz ;
       pv[kk++] = Q6_xyyyyy ; pv[kk++] = Q6_xyyyyz ; pv[kk++] = Q6_xyyyzz ;
       pv[kk++] = Q6_xyyzzz ; pv[kk++] = Q6_xyzzzz ; pv[kk++] = Q6_xzzzzz ;
       pv[kk++] = Q6_yyyyyy ; pv[kk++] = Q6_yyyyyz ; pv[kk++] = Q6_yyyyzz ;
       pv[kk++] = Q6_yyyzzz ; pv[kk++] = Q6_yyzzzz ; pv[kk++] = Q6_yzzzzz ;
       pv[kk++] = Q6_zzzzzz ;
       pv[kk++] = Q7_xxxxxxx ; pv[kk++] = Q7_xxxxxxy ; pv[kk++] = Q7_xxxxxxz ;
       pv[kk++] = Q7_xxxxxyy ; pv[kk++] = Q7_xxxxxzz ; pv[kk++] = Q7_xxxxxyz ;
       pv[kk++] = Q7_xxxxyyy ; pv[kk++] = Q7_xxxxyyz ; pv[kk++] = Q7_xxxxyzz ;
       pv[kk++] = Q7_xxxxzzz ; pv[kk++] = Q7_xxxyyyy ; pv[kk++] = Q7_xxxyyyz ;
       pv[kk++] = Q7_xxxyyzz ; pv[kk++] = Q7_xxxyzzz ; pv[kk++] = Q7_xxxzzzz ;
       pv[kk++] = Q7_xxyyyyy ; pv[kk++] = Q7_xxyyyyz ; pv[kk++] = Q7_xxyyyzz ;
       pv[kk++] = Q7_xxyyzzz ; pv[kk++] = Q7_xxyzzzz ; pv[kk++] = Q7_xxzzzzz ;
       pv[kk++] = Q7_xyyyyyy ; pv[kk++] = Q7_xyyyyyz ; pv[kk++] = Q7_xyyyyzz ;
       pv[kk++] = Q7_xyyyzzz ; pv[kk++] = Q7_xyyzzzz ; pv[kk++] = Q7_xyzzzzz ;
       pv[kk++] = Q7_xzzzzzz ; pv[kk++] = Q7_yyyyyyy ; pv[kk++] = Q7_yyyyyyz ;
       pv[kk++] = Q7_yyyyyzz ; pv[kk++] = Q7_yyyyzzz ; pv[kk++] = Q7_yyyzzzz ;
       pv[kk++] = Q7_yyzzzzz ; pv[kk++] = Q7_yzzzzzz ; pv[kk++] = Q7_zzzzzzz ;

       for( kk=jj=0 ; jj < NPOLHEPT ; jj++,kk+=3 ){
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
       if( puse[51] ) pv[51] = P5_zzzzz(uu,vv,ww) ;      kk = 51 ;
       if( puse[++kk] ) pv[kk] = P6_xxxxxx(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxxxxy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxxxxz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxxxyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxxxzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxxxyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxxyyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxxyyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxxyzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxxzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxyyyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxyyyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxyyzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxyzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xxzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xyyyyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xyyyyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xyyyzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xyyzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xyzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_xzzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_yyyyyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_yyyyyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_yyyyzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_yyyzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_yyzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_yzzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P6_zzzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxxxx(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxxxy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxxxz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxxyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxxzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxxyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxyyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxyyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxyzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxxzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxyyyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxyyyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxyyzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxyzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxxzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxyyyyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxyyyyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxyyyzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxyyzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxyzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xxzzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xyyyyyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xyyyyyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xyyyyzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xyyyzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xyyzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xyzzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_xzzzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_yyyyyyy(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_yyyyyyz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_yyyyyzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_yyyyzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_yyyzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_yyzzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_yzzzzzz(uu,vv,ww) ;
       if( puse[++kk] ) pv[kk] = P7_zzzzzzz(uu,vv,ww) ;
       for( kk=jj=0 ; jj < NPOLHEPT ; jj++,kk+=3 ){
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
 }
 AFNI_OMP_END ;

#if 0
 if( AFNI_yesenv("ALLIN_DEBUG") ){
   int ii ; float dd,dmax=0.0f ;
   static int ncall=0 ;
   for( ii=0 ; ii < npt ; ii++ ){
     dd = fabsf(xo[ii]-xi[ii]) + fabsf(yo[ii]-yi[ii]) + fabsf(zo[ii]-zi[ii]) ;
     if( dd > dmax ) dmax = dd ;
   }
   ncall++ ; ININFO_message("heptic %d: dmax = %.4g",ncall,dmax) ;
 }
#endif

   return ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for nonic (9th order) polynomials. */

void mri_genalign_nonic( int npar, float *wpar ,
                         int npt , float *xi, float *yi, float *zi ,
                                   float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   static float xcen,ycen,zcen,xyzfac,xyzinv , ppar[3*NPOLNONI] ;
   static int puse[NPOLNONI] , pall , plast ;

ENTRY("mri_genalign_nonic") ;

   /** new parameters ==> setup matrix */

   if( npar >= 3*NPOLNONI+16 && wpar != NULL ){
     int aa=aff_use_after , ab=aff_use_before , jj ;

#if 0
STATUS("setup params") ;
#endif

     xcen   = wpar[12+3*NPOLNONI] ;  /* the fake (non-varying) parameters */
     ycen   = wpar[13+3*NPOLNONI] ;
     zcen   = wpar[14+3*NPOLNONI] ;
     xyzfac = wpar[15+3*NPOLNONI] ; xyzinv = 1.0f / xyzfac ;

     aff_use_before = aff_use_after = 0;
     gam = GA_setup_affine( 12 , wpar ) ;  /* affine param setup */
     aff_use_before = ab; aff_use_after = aa;

     for( jj=0 ; jj < 3*NPOLNONI ; jj++ )          /* save polynomial params */
       ppar[jj] = wpar[jj+12] * xyzinv ;
     for( pall=jj=0 ; jj < NPOLNONI ; jj++ ){      /* mark which ones to use */
       puse[jj] = (ppar[3*jj  ] != 0.0f) ||
                  (ppar[3*jj+1] != 0.0f) || (ppar[3*jj+2] != 0.0f) ;
       pall += puse[jj] ; if( puse[jj] ) plast = jj ;
     }
     pall = ( pall >= (int)(0.9f*NPOLNONI) ) ;

#if 0
STATUS("setup finished") ;
#endif
   }

   /* nothing to transform? (a setup call) */

   if( npt <= 0 || xi == NULL || xo == NULL ) EXRETURN ;

   /*--- do some work ---*/

#if 0
   if( pall ){ STATUS("use pall") ; }
   else {
     int jj ;
     STATUS("not pall") ;
     if( PRINT_TRACING ){
       fprintf(stderr,"   ppar:") ;
       for( jj=0 ; jj < 3*NPOLNONI ; jj++ )
         if( ppar[jj] != 0.0f ) fprintf(stderr," [%d]=%.5g",jj,ppar[jj]) ;
       fprintf(stderr,"\n") ;
     }
   }
#endif

 AFNI_OMP_START ;
#pragma omp parallel if( npt > 3333 )
 { int ii,jj,kk ; float aa,bb,cc , uu,vv,ww , pv[NPOLNONI] ;
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
     FIXYZ(uu) ; FIXYZ(vv) ; FIXYZ(ww) ;

     /* polynomials */

     if( pall ){
       float p1x,p2x,p3x,p4x,p5x,p6x,p7x,p8x,p9x,
             p1y,p2y,p3y,p4y,p5y,p6y,p7y,p8y,p9y,
             p1z,p2z,p3z,p4z,p5z,p6z,p7z,p8z,p9z ;
       p1x=PP1(uu); p2x=PP2(uu); p3x=PP3(uu); p4x=PP4(uu); p5x=PP5(uu); p6x=PP6(uu); p7x=PP7(uu); p8x=PP8(uu); p9x=PP9(uu);
       p1y=PP1(vv); p2y=PP2(vv); p3y=PP3(vv); p4y=PP4(vv); p5y=PP5(vv); p6y=PP6(vv); p7y=PP7(vv); p8y=PP8(vv); p9y=PP9(vv);
       p1z=PP1(ww); p2z=PP2(ww); p3z=PP3(ww); p4z=PP4(ww); p5z=PP5(ww); p6z=PP6(ww); p7z=PP7(ww); p8z=PP8(ww); p9z=PP9(ww);

#define Q8_xxxxxxxx p8x
#define Q8_xxxxxxxy p7x*p1y
#define Q8_xxxxxxxz p7x*p1z
#define Q8_xxxxxxyy p6x*p2y
#define Q8_xxxxxxzz p6x*p2z
#define Q8_xxxxxxyz p6x*p1y*p1z
#define Q8_xxxxxyyy p5x*p3y
#define Q8_xxxxxyyz p5x*p2y*p1z
#define Q8_xxxxxyzz p5x*p1y*p2z
#define Q8_xxxxxzzz p5x*p3z
#define Q8_xxxxyyyy p4x*p4y
#define Q8_xxxxyyyz p4x*p3y*p1z
#define Q8_xxxxyyzz p4x*p2y*p2z
#define Q8_xxxxyzzz p4x*p1y*p3z
#define Q8_xxxxzzzz p4x*p4z
#define Q8_xxxyyyyy p3x*p5y
#define Q8_xxxyyyyz p3x*p4y*p1z
#define Q8_xxxyyyzz p3x*p3y*p2z
#define Q8_xxxyyzzz p3x*p2y*p3z
#define Q8_xxxyzzzz p3x*p1y*p4z
#define Q8_xxxzzzzz p3x*p5z
#define Q8_xxyyyyyy p2x*p6y
#define Q8_xxyyyyyz p2x*p5y*p1z
#define Q8_xxyyyyzz p2x*p4y*p2z
#define Q8_xxyyyzzz p2x*p3y*p3z
#define Q8_xxyyzzzz p2x*p2y*p4z
#define Q8_xxyzzzzz p2x*p1y*p5z
#define Q8_xxzzzzzz p2x*p6z
#define Q8_xyyyyyyy p1x*p7y
#define Q8_xyyyyyyz p1x*p6y*p1z
#define Q8_xyyyyyzz p1x*p5y*p2z
#define Q8_xyyyyzzz p1x*p4y*p3z
#define Q8_xyyyzzzz p1x*p3y*p4z
#define Q8_xyyzzzzz p1x*p2y*p5z
#define Q8_xyzzzzzz p1x*p1y*p6z
#define Q8_xzzzzzzz p1x*p7z
#define Q8_yyyyyyyy p8y
#define Q8_yyyyyyyz p7y*p1z
#define Q8_yyyyyyzz p6y*p2z
#define Q8_yyyyyzzz p5y*p3z
#define Q8_yyyyzzzz p4y*p4z
#define Q8_yyyzzzzz p3y*p5z
#define Q8_yyzzzzzz p2y*p6z
#define Q8_yzzzzzzz p1y*p7z
#define Q8_zzzzzzzz p8z
#define Q9_xxxxxxxxx p9x
#define Q9_xxxxxxxxy p8x*p1y
#define Q9_xxxxxxxxz p8x*p1z
#define Q9_xxxxxxxyy p7x*p2y
#define Q9_xxxxxxxzz p7x*p2z
#define Q9_xxxxxxxyz p7x*p1y*p1z
#define Q9_xxxxxxyyy p6x*p3y
#define Q9_xxxxxxyyz p6x*p2y*p1z
#define Q9_xxxxxxyzz p6x*p1y*p2z
#define Q9_xxxxxxzzz p6x*p3z
#define Q9_xxxxxyyyy p5x*p4y
#define Q9_xxxxxyyyz p5x*p3y*p1z
#define Q9_xxxxxyyzz p5x*p2y*p2z
#define Q9_xxxxxyzzz p5x*p1y*p3z
#define Q9_xxxxxzzzz p5x*p4z
#define Q9_xxxxyyyyy p4x*p5y
#define Q9_xxxxyyyyz p4x*p4y*p1z
#define Q9_xxxxyyyzz p4x*p3y*p2z
#define Q9_xxxxyyzzz p4x*p2y*p3z
#define Q9_xxxxyzzzz p4x*p1y*p4z
#define Q9_xxxxzzzzz p4x*p5z
#define Q9_xxxyyyyyy p3x*p6y
#define Q9_xxxyyyyyz p3x*p5y*p1z
#define Q9_xxxyyyyzz p3x*p4y*p2z
#define Q9_xxxyyyzzz p3x*p3y*p3z
#define Q9_xxxyyzzzz p3x*p2y*p4z
#define Q9_xxxyzzzzz p3x*p1y*p5z
#define Q9_xxxzzzzzz p3x*p6z
#define Q9_xxyyyyyyy p2x*p7y
#define Q9_xxyyyyyyz p2x*p6y*p1z
#define Q9_xxyyyyyzz p2x*p5y*p2z
#define Q9_xxyyyyzzz p2x*p4y*p3z
#define Q9_xxyyyzzzz p2x*p3y*p4z
#define Q9_xxyyzzzzz p2x*p2y*p5z
#define Q9_xxyzzzzzz p2x*p1y*p6z
#define Q9_xxzzzzzzz p2x*p7z
#define Q9_xyyyyyyyy p1x*p8y
#define Q9_xyyyyyyyz p1x*p7y*p1z
#define Q9_xyyyyyyzz p1x*p6y*p2z
#define Q9_xyyyyyzzz p1x*p5y*p3z
#define Q9_xyyyyzzzz p1x*p4y*p4z
#define Q9_xyyyzzzzz p1x*p3y*p5z
#define Q9_xyyzzzzzz p1x*p2y*p6z
#define Q9_xyzzzzzzz p1x*p1y*p7z
#define Q9_xzzzzzzzz p1x*p8z
#define Q9_yyyyyyyyy p9y
#define Q9_yyyyyyyyz p8y*p1z
#define Q9_yyyyyyyzz p7y*p2z
#define Q9_yyyyyyzzz p6y*p3z
#define Q9_yyyyyzzzz p5y*p4z
#define Q9_yyyyzzzzz p4y*p5z
#define Q9_yyyzzzzzz p3y*p6z
#define Q9_yyzzzzzzz p2y*p7z
#define Q9_yzzzzzzzz p1y*p8z
#define Q9_zzzzzzzzz p9z

       pv[ 0] = Q2_xx  ; pv[ 1] = Q2_xy  ; pv[ 2] = Q2_xz  ; pv[ 3] = Q2_yy  ;
       pv[ 4] = Q2_yz  ; pv[ 5] = Q2_zz  ; pv[ 6] = Q3_xxx ; pv[ 7] = Q3_xxy ;
       pv[ 8] = Q3_xxz ; pv[ 9] = Q3_xyy ; pv[10] = Q3_xzz ; pv[11] = Q3_xyz ;
       pv[12] = Q3_yyy ; pv[13] = Q3_yyz ; pv[14] = Q3_yzz ; pv[15] = Q3_zzz ;
       pv[16] = Q4_xxxx ; pv[17] = Q4_xxxy ; pv[18] = Q4_xxxz ; pv[19] = Q4_xxyy ;
       pv[20] = Q4_xxzz ; pv[21] = Q4_xxyz ; pv[22] = Q4_xyyy ; pv[23] = Q4_xyyz ;
       pv[24] = Q4_xyzz ; pv[25] = Q4_xzzz ; pv[26] = Q4_yyyy ; pv[27] = Q4_yyyz ;
       pv[28] = Q4_yyzz ; pv[29] = Q4_yzzz ; pv[30] = Q4_zzzz ; pv[31] = Q5_xxxxx ;
       pv[32] = Q5_xxxxy ; pv[33] = Q5_xxxxz ; pv[34] = Q5_xxxyy ; pv[35] = Q5_xxxzz ;
       pv[36] = Q5_xxxyz ; pv[37] = Q5_xxyyy ; pv[38] = Q5_xxyyz ; pv[39] = Q5_xxyzz ;
       pv[40] = Q5_xxzzz ; pv[41] = Q5_xyyyy ; pv[42] = Q5_xyyyz ; pv[43] = Q5_xyyzz ;
       pv[44] = Q5_xyzzz ; pv[45] = Q5_xzzzz ; pv[46] = Q5_yyyyy ; pv[47] = Q5_yyyyz ;
       pv[48] = Q5_yyyzz ; pv[49] = Q5_yyzzz ; pv[50] = Q5_yzzzz ; pv[51] = Q5_zzzzz ;
       kk = 52 ;
       pv[kk++] = Q6_xxxxxx ; pv[kk++] = Q6_xxxxxy ; pv[kk++] = Q6_xxxxxz ;
       pv[kk++] = Q6_xxxxyy ; pv[kk++] = Q6_xxxxzz ; pv[kk++] = Q6_xxxxyz ;
       pv[kk++] = Q6_xxxyyy ; pv[kk++] = Q6_xxxyyz ; pv[kk++] = Q6_xxxyzz ;
       pv[kk++] = Q6_xxxzzz ; pv[kk++] = Q6_xxyyyy ; pv[kk++] = Q6_xxyyyz ;
       pv[kk++] = Q6_xxyyzz ; pv[kk++] = Q6_xxyzzz ; pv[kk++] = Q6_xxzzzz ;
       pv[kk++] = Q6_xyyyyy ; pv[kk++] = Q6_xyyyyz ; pv[kk++] = Q6_xyyyzz ;
       pv[kk++] = Q6_xyyzzz ; pv[kk++] = Q6_xyzzzz ; pv[kk++] = Q6_xzzzzz ;
       pv[kk++] = Q6_yyyyyy ; pv[kk++] = Q6_yyyyyz ; pv[kk++] = Q6_yyyyzz ;
       pv[kk++] = Q6_yyyzzz ; pv[kk++] = Q6_yyzzzz ; pv[kk++] = Q6_yzzzzz ;
       pv[kk++] = Q6_zzzzzz ;
       pv[kk++] = Q7_xxxxxxx ; pv[kk++] = Q7_xxxxxxy ; pv[kk++] = Q7_xxxxxxz ;
       pv[kk++] = Q7_xxxxxyy ; pv[kk++] = Q7_xxxxxzz ; pv[kk++] = Q7_xxxxxyz ;
       pv[kk++] = Q7_xxxxyyy ; pv[kk++] = Q7_xxxxyyz ; pv[kk++] = Q7_xxxxyzz ;
       pv[kk++] = Q7_xxxxzzz ; pv[kk++] = Q7_xxxyyyy ; pv[kk++] = Q7_xxxyyyz ;
       pv[kk++] = Q7_xxxyyzz ; pv[kk++] = Q7_xxxyzzz ; pv[kk++] = Q7_xxxzzzz ;
       pv[kk++] = Q7_xxyyyyy ; pv[kk++] = Q7_xxyyyyz ; pv[kk++] = Q7_xxyyyzz ;
       pv[kk++] = Q7_xxyyzzz ; pv[kk++] = Q7_xxyzzzz ; pv[kk++] = Q7_xxzzzzz ;
       pv[kk++] = Q7_xyyyyyy ; pv[kk++] = Q7_xyyyyyz ; pv[kk++] = Q7_xyyyyzz ;
       pv[kk++] = Q7_xyyyzzz ; pv[kk++] = Q7_xyyzzzz ; pv[kk++] = Q7_xyzzzzz ;
       pv[kk++] = Q7_xzzzzzz ; pv[kk++] = Q7_yyyyyyy ; pv[kk++] = Q7_yyyyyyz ;
       pv[kk++] = Q7_yyyyyzz ; pv[kk++] = Q7_yyyyzzz ; pv[kk++] = Q7_yyyzzzz ;
       pv[kk++] = Q7_yyzzzzz ; pv[kk++] = Q7_yzzzzzz ; pv[kk++] = Q7_zzzzzzz ;
       pv[kk++] = Q8_xxxxxxxx ; pv[kk++] = Q8_xxxxxxxy ; pv[kk++] = Q8_xxxxxxxz ;
       pv[kk++] = Q8_xxxxxxyy ; pv[kk++] = Q8_xxxxxxzz ; pv[kk++] = Q8_xxxxxxyz ;
       pv[kk++] = Q8_xxxxxyyy ; pv[kk++] = Q8_xxxxxyyz ; pv[kk++] = Q8_xxxxxyzz ;
       pv[kk++] = Q8_xxxxxzzz ; pv[kk++] = Q8_xxxxyyyy ; pv[kk++] = Q8_xxxxyyyz ;
       pv[kk++] = Q8_xxxxyyzz ; pv[kk++] = Q8_xxxxyzzz ; pv[kk++] = Q8_xxxxzzzz ;
       pv[kk++] = Q8_xxxyyyyy ; pv[kk++] = Q8_xxxyyyyz ; pv[kk++] = Q8_xxxyyyzz ;
       pv[kk++] = Q8_xxxyyzzz ; pv[kk++] = Q8_xxxyzzzz ; pv[kk++] = Q8_xxxzzzzz ;
       pv[kk++] = Q8_xxyyyyyy ; pv[kk++] = Q8_xxyyyyyz ; pv[kk++] = Q8_xxyyyyzz ;
       pv[kk++] = Q8_xxyyyzzz ; pv[kk++] = Q8_xxyyzzzz ; pv[kk++] = Q8_xxyzzzzz ;
       pv[kk++] = Q8_xxzzzzzz ; pv[kk++] = Q8_xyyyyyyy ; pv[kk++] = Q8_xyyyyyyz ;
       pv[kk++] = Q8_xyyyyyzz ; pv[kk++] = Q8_xyyyyzzz ; pv[kk++] = Q8_xyyyzzzz ;
       pv[kk++] = Q8_xyyzzzzz ; pv[kk++] = Q8_xyzzzzzz ; pv[kk++] = Q8_xzzzzzzz ;
       pv[kk++] = Q8_yyyyyyyy ; pv[kk++] = Q8_yyyyyyyz ; pv[kk++] = Q8_yyyyyyzz ;
       pv[kk++] = Q8_yyyyyzzz ; pv[kk++] = Q8_yyyyzzzz ; pv[kk++] = Q8_yyyzzzzz ;
       pv[kk++] = Q8_yyzzzzzz ; pv[kk++] = Q8_yzzzzzzz ; pv[kk++] = Q8_zzzzzzzz ;
       pv[kk++] = Q9_xxxxxxxxx ; pv[kk++] = Q9_xxxxxxxxy ; pv[kk++] = Q9_xxxxxxxxz ;
       pv[kk++] = Q9_xxxxxxxyy ; pv[kk++] = Q9_xxxxxxxzz ; pv[kk++] = Q9_xxxxxxxyz ;
       pv[kk++] = Q9_xxxxxxyyy ; pv[kk++] = Q9_xxxxxxyyz ; pv[kk++] = Q9_xxxxxxyzz ;
       pv[kk++] = Q9_xxxxxxzzz ; pv[kk++] = Q9_xxxxxyyyy ; pv[kk++] = Q9_xxxxxyyyz ;
       pv[kk++] = Q9_xxxxxyyzz ; pv[kk++] = Q9_xxxxxyzzz ; pv[kk++] = Q9_xxxxxzzzz ;
       pv[kk++] = Q9_xxxxyyyyy ; pv[kk++] = Q9_xxxxyyyyz ; pv[kk++] = Q9_xxxxyyyzz ;
       pv[kk++] = Q9_xxxxyyzzz ; pv[kk++] = Q9_xxxxyzzzz ; pv[kk++] = Q9_xxxxzzzzz ;
       pv[kk++] = Q9_xxxyyyyyy ; pv[kk++] = Q9_xxxyyyyyz ; pv[kk++] = Q9_xxxyyyyzz ;
       pv[kk++] = Q9_xxxyyyzzz ; pv[kk++] = Q9_xxxyyzzzz ; pv[kk++] = Q9_xxxyzzzzz ;
       pv[kk++] = Q9_xxxzzzzzz ; pv[kk++] = Q9_xxyyyyyyy ; pv[kk++] = Q9_xxyyyyyyz ;
       pv[kk++] = Q9_xxyyyyyzz ; pv[kk++] = Q9_xxyyyyzzz ; pv[kk++] = Q9_xxyyyzzzz ;
       pv[kk++] = Q9_xxyyzzzzz ; pv[kk++] = Q9_xxyzzzzzz ; pv[kk++] = Q9_xxzzzzzzz ;
       pv[kk++] = Q9_xyyyyyyyy ; pv[kk++] = Q9_xyyyyyyyz ; pv[kk++] = Q9_xyyyyyyzz ;
       pv[kk++] = Q9_xyyyyyzzz ; pv[kk++] = Q9_xyyyyzzzz ; pv[kk++] = Q9_xyyyzzzzz ;
       pv[kk++] = Q9_xyyzzzzzz ; pv[kk++] = Q9_xyzzzzzzz ; pv[kk++] = Q9_xzzzzzzzz ;
       pv[kk++] = Q9_yyyyyyyyy ; pv[kk++] = Q9_yyyyyyyyz ; pv[kk++] = Q9_yyyyyyyzz ;
       pv[kk++] = Q9_yyyyyyzzz ; pv[kk++] = Q9_yyyyyzzzz ; pv[kk++] = Q9_yyyyzzzzz ;
       pv[kk++] = Q9_yyyzzzzzz ; pv[kk++] = Q9_yyzzzzzzz ; pv[kk++] = Q9_yzzzzzzzz ;
       pv[kk++] = Q9_zzzzzzzzz ;

       for( kk=jj=0 ; jj < NPOLNONI ; jj++,kk+=3 ){
         aa += ppar[kk  ] * pv[jj] ;
         bb += ppar[kk+1] * pv[jj] ;
         cc += ppar[kk+2] * pv[jj] ;
       }
     } else {
       kk = -1 ;
       if( puse[++kk] ){ pv[kk] = P2_xx (uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P2_xy (uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P2_xz (uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P2_yy (uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P2_yz (uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P2_zz (uu,vv,ww) ; if( kk >= plast ) goto PUSE_LOOP ; }
       if( puse[++kk] ){ pv[kk] = P3_xxx(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P3_xxy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P3_xxz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P3_xyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P3_xzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P3_xyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P3_yyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P3_yyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P3_yzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P3_zzz(uu,vv,ww) ; if( kk >= plast ) goto PUSE_LOOP ; }
       if( puse[++kk] ){ pv[kk] = P4_xxxx(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_xxxy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_xxxz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_xxyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_xxzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_xxyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_xyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_xyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_xyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_xzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_yyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_yyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_yyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_yzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P4_zzzz(uu,vv,ww) ; if( kk >= plast ) goto PUSE_LOOP ; }
       if( puse[++kk] ){ pv[kk] = P5_xxxxx(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xxxxy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xxxxz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xxxyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xxxzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xxxyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xxyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xxyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xxyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xxzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_xzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_yyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_yyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_yyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_yyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_yzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P5_zzzzz(uu,vv,ww) ; if( kk >= plast ) goto PUSE_LOOP ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxxxx(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxxxy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxxxz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxxyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxxzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxxyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxxzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xxzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_xzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_yyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_yyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_yyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_yyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_yyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_yzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P6_zzzzzz(uu,vv,ww) ; if( kk >= plast ) goto PUSE_LOOP ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxxxx(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxxxy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxxxz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxxyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxxzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxxyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxxzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxxzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xxzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xyyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xyyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xyyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xyyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xyyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xyzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_xzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_yyyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_yyyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_yyyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_yyyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_yyyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_yyzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_yzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P7_zzzzzzz(uu,vv,ww) ; if( kk >= plast ) goto PUSE_LOOP ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxxxx(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxxxy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxxxz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxxyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxxzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxxyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxxzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxxzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxxzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxyyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxyyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxyyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxyyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxyyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxyzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xxzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xyyyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xyyyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xyyyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xyyyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xyyyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xyyzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xyzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_xzzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_yyyyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_yyyyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_yyyyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_yyyyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_yyyyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_yyyzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_yyzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_yzzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P8_zzzzzzzz(uu,vv,ww) ; if( kk >= plast ) goto PUSE_LOOP ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxxxx(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxxxy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxxxz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxxyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxxzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxxyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxxzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxxzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxxzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxyyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxyyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxyyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxyyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxyyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxyzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxxzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxyyyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxyyyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxyyyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxyyyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxyyyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxyyzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxyzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xxzzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xyyyyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xyyyyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xyyyyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xyyyyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xyyyyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xyyyzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xyyzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xyzzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_xzzzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_yyyyyyyyy(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_yyyyyyyyz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_yyyyyyyzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_yyyyyyzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_yyyyyzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_yyyyzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_yyyzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_yyzzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_yzzzzzzzz(uu,vv,ww) ; }
       if( puse[++kk] ){ pv[kk] = P9_zzzzzzzzz(uu,vv,ww) ; }

PUSE_LOOP:
       for( kk=jj=0 ; jj <= plast ; jj++,kk+=3 ){
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
 }
 AFNI_OMP_END ;

#if 0
 if( AFNI_yesenv("ALLIN_DEBUG") ){
   int ii ; float dd,dmax=0.0f ;
   static int ncall=0 ;
   for( ii=0 ; ii < npt ; ii++ ){
     dd = fabsf(xo[ii]-xi[ii]) + fabsf(yo[ii]-yi[ii]) + fabsf(zo[ii]-zi[ii]) ;
     if( dd > dmax ) dmax = dd ;
   }
   ncall++ ; ININFO_message("nonic %d: dmax = %.4g",ncall,dmax) ;
 }
#endif

   EXRETURN ;
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
   wsum_npar = 0 ;

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
 AFNI_OMP_START ;
#pragma omp parallel if( nqq > 33333 )
 { int ii,jj,kk,qq ; float xx,yy,zz ;
#pragma omp for
   for( qq=0 ; qq < nqq ; qq++ ){
     xx = imw[qq] ; if( xx < -0.499f || xx > nxh ) continue ;
     yy = jmw[qq] ; if( yy < -0.499f || yy > nyh ) continue ;
     zz = kmw[qq] ; if( zz < -0.499f || zz > nzh ) continue ;
     ii = (int)(xx+0.5f) ; jj = (int)(yy+0.5f) ; kk = (int)(zz+0.5f) ;
     if( tgar[ii+jj*nxt+kk*nxyt] ) hhh[qq] = 1 ;
   }
 }
 AFNI_OMP_END ;
 for( nhit=qq=0 ; qq < nqq ; qq++ ) nhit += hhh[qq] ;
 free(hhh) ;
#endif

   free((void *)kmw); free((void *)jmw); free((void *)imw);

   xx = gstup->nbsmask ;
   yy = gstup->najmask * gstup->ajim->dx * gstup->ajim->dy * gstup->ajim->dz /
                       ( gstup->bsim->dx * gstup->bsim->dy * gstup->bsim->dz ) ;

   frac = nhit / MIN(xx,yy) ; RETURN(frac) ;
}
