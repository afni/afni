#include "mrilib.h"

#undef  GOOD
#define GOOD(i) (mask==NULL || mask[i])

static GA_setup *stup = NULL ;

/*---------------------------------------------------------------------------*/
static int gcd( int m , int n )
{
  while( m > 0 ){
    if( n > m ){ int t=m; m=n; n=t; } /* swap */
    m -= n ;
  }
  return n ;
}
static int find_relprime_fixed( int n )
{
   int dj , n5=n/5 ;
   if( n5 < 2 ) return 1 ;
   for( dj=n5 ; gcd(n,dj) > 1 ; dj++ ) ; /*nada*/
   return dj ;
}
static int find_relprime_random( int n )
{
   int dj , n5=n/5 , n2=n/2 ;
   if( n5 < 2 ) return 1 ;
   do{ dj = n5 + lrand48()%n2 ; } while( gcd(n,dj) > 1 ) ;
   return dj ;
}
/*---------------------------------------------------------------------------*/

static MRI_IMAGE * GA_smooth( MRI_IMAGE *im , int meth , float rad )
{
   MRI_IMAGE *om=NULL ;

   ENTRY("GA_smooth") ;

   if( im == NULL || im->kind != MRI_float || rad <= 0.0f ) RETURN(NULL) ;

   switch( meth ){
     default:
     case GA_SMOOTH_GAUSSIAN:
       om = mri_to_float(im) ;
       FIR_blur_volume_3d( om->nx , om->ny , om->nz ,
                           1.0f   , 1.0f   , 1.0f   ,
                           MRI_FLOAT_PTR(om) ,
                           rad , rad , rad           ) ;
     break ;

     case GA_SMOOTH_MEDIAN:
       if( rad < 1.01f ) rad = 1.01f ;
       om = mri_medianfilter( im , rad , NULL , 0 ) ;
     break ;
   }

   RETURN(om) ;
}

/*---------------------------------------------------------------------------*/

#undef  FAR
#define FAR(i,j,k) far[(i)+(j)*nx+(k)*nxy]

static void GA_interp_NN( MRI_IMAGE *fim ,
                          int npp, float *ip, float *jp, float *kp, float *vv )
{
   int nx=fim->nx , ny=fim->ny , nz=fim->nz , nxy=nx*ny , ii,jj,kk , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float *far = MRI_FLOAT_PTR(fim) ;

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=0.0f; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=0.0f; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=0.0f; continue; }

     ii = (int)(xx+0.5f) ; jj = (int)(yy+0.5f) ; kk = (int)(zz+0.5f) ;
     vv[pp] = FAR(ii,jj,kk) ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

#undef  NPER
#define NPER 1024

static void GA_get_warped_values( int nmpar , double *mpar , float *avm )
{
   int    npar , nfree , ii,jj,kk,qq,pp,npp,mm,nx,ny,nxy , *pma ;
   float *wpar ;
   float *imf , *jmf , *kmf ;
   float *imw , *jmw , *kmw ;

   npar  = stup->wfunc_numpar ;
   nfree = stup->wfunc_numfree ;
   pma   = stup->wfunc_pma ;
   wpar  = (float *)malloc(sizeof(float)*npar) ;

   for( ii=pp=0 ; ii < npar ; ii++ ){
     wpar[ii] = ( stup->wfunc_param[ii].fixed )
               ? stup->wfunc_param[ii].val_fixed
               : (float)mpar[pp++] ;
   }

   if( stup->im == NULL ){
     imf = (float *)malloc(sizeof(float)*NPER) ;
     jmf = (float *)malloc(sizeof(float)*NPER) ;
     kmf = (float *)malloc(sizeof(float)*NPER) ;
   }
   imw = (float *)malloc(sizeof(float)*NPER) ;
   jmw = (float *)malloc(sizeof(float)*NPER) ;
   kmw = (float *)malloc(sizeof(float)*NPER) ;

   nx = stup->bsim->nx; ny = stup->bsim->ny; nxy = nx*ny;

   for( pp=0 ; pp < stup->npt_match ; pp+=NPER ){
     npp = MIN( NPER , stup->npt_match-pp ) ;
     if( stup->im == NULL ){
       for( qq=0 ; qq < npp ; qq++ ){
         mm = pp+qq ;
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         imf[qq] = ii; jmf[qq] = jj; kmf[qq] = kk;
       }
     } else {
       imf = stup->im + pp ;
       jmf = stup->jm + pp ;
       kmf = stup->km + pp ;
     }
     stup->wfunc( npar , wpar ,
                  npp  , imf,jmf,kmf , imw,jmw,kmw ) ;

     switch( stup->interp_code ){
       case MRI_NN:
         GA_interp_NN    ( stup->ajim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;
       case MRI_LINEAR:
         GA_interp_linear( stup->ajim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;
       default:
       case MRI_HEPTIC:
         GA_interp_heptic( stup->ajim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;
     }
   }

   free((void *)kmw); free((void *)jmw); free((void *)imw);
   if( stup->im == NULL ){
     free((void *)kmf); free((void *)jmf); free((void *)imf);
   }
   free((void *)wpar) ;

   return ;
}

/*---------------------------------------------------------------------------*/

double GA_scalar_fitter( int npar , double *mpar )
{
  float val=0.0f ;
  float *avm , *bvm ;

  avm = (float *)malloc(stup->npt_match*sizeof(float)) ;
  GA_get_warped_values( npar , mpar , avm ) ;
  bvm = stup->bvm ;

  switch( stup->match_code ){

    default:
    case GA_MATCH_PEARSON_SCALAR:
      val = (double)THD_pearson_corr( stup->npt_match , avm , bvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_SPEARMAN_SCALAR:
      val = (double)spearman_rank_corr( stup->npt_match, avm,
                                        stup->bvstat   , bvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_KULLBACK_SCALAR:   /* not yet implemented */
    break ;
  }

  return (double)val ;
}

/*---------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(s) \
 do{ ERROR_message("mri_genalign_scalar: %s",(s)); RETURN(NULL); } while(0)

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_genalign_scalar( MRI_IMAGE *basim  ,
                                 MRI_IMAGE *maskim ,
                                 MRI_IMAGE *targim ,
                                 GA_parameters *parm    )
{
   int nspad , qq , rr , nx,ny,nz,nxy , mm,ii,jj,kk ;
   int use_all=0 ;
   float *bsar ;

ENTRY("mri_genalign_scalar") ;

   /*-- basic checks of input for rationality --*/

   if( basim  == NULL ) ERREX("basim is NULL") ;
   if( targim == NULL ) ERREX("targim is NULL") ;
   if( parm   == NULL ) ERREX("parm is NULL") ;

   nspad = MRI_DIMENSIONALITY(basim) ;
   if( nspad < 2 || nspad > 3 )
     ERREX("basim dimensionality is not 2 or 3") ;
   if( nspad != MRI_DIMENSIONALITY(targim) )
     ERREX("basim & targim dimensionalities differ") ;
   if( maskim != NULL && maskim->nvox != basim->nvox )
     ERREX("basim and maskim grids differ") ;

   if( parm->wfunc_numpar < 1 || parm->wfunc==NULL || parm->wfunc_param==NULL )
     ERREX("illegal wfunc parameters") ;

   FREE_GA_setup(stup) ;
   stup = (GA_setup *)calloc(1,sizeof(GA_setup)) ;

   stup->match_code    = parm->match_code    ;
   stup->interp_code   = parm->interp_code   ;
   stup->npt_match     = parm->npt_match     ;
   stup->kernel_code   = parm->kernel_code   ;
   stup->kernel_radius = parm->kernel_radius ;
   stup->npt_sum       = parm->npt_sum       ;
   stup->wfunc_numpar  = parm->wfunc_numpar  ;
   stup->wfunc         = parm->wfunc         ;

   stup->dim_avec = stup->dim_bvec = 1 ;  /* scalars */

   /* copy parameter definitions */

   { int nfree=stup->wfunc_numpar , pp , *pma ;

     stup->wfunc_param = (GA_param *)malloc(sizeof(GA_param)*stup->wfunc_numpar);
     for( pp=0 ; pp < stup->wfunc_numpar ; pp++ ){
       stup->wfunc_param[pp] = parm->wfunc_param[pp] ;
       if( stup->wfunc_param[pp].fixed ) nfree-- ;
     }
     if( nfree <= 0 ) ERREX("no free wfunc parameters") ;
     stup->wfunc_numfree = nfree ;

     /** pma[k] = external parameter index for the k-th free parameter **/

     pma = (int *)malloc(sizeof(int) * nfree) ;
     for( pp=ii=0 ; ii < stup->wfunc_numpar ; ii++ )
       if( !stup->wfunc_param[ii].fixed ) pma[pp++] = ii ;
     stup->wfunc_pma = pma ;
   }

   /** load images into setup struct, smoothing if so ordered **/

   stup->bsim = mri_to_float(basim ) ;
   stup->ajim = mri_to_float(targim) ;

   nx = stup->bsim->nx; ny = stup->bsim->ny; nz = stup->bsim->nz; nxy = nx*ny;

   if( parm->smooth_code > 0 && parm->smooth_radius > 0.0f ){
     MRI_IMAGE *qim ;
     qim = GA_smooth( stup->bsim , parm->smooth_code , parm->smooth_radius ) ;
     if( qim != NULL ){ mri_free(stup->bsim) ; stup->bsim = qim ; }
   }

   if( parm->smooth_code > 0 && parm->smooth_radius > 0.0f ){
     MRI_IMAGE *qim ;
     float nxa=stup->ajim->nx, nya=stup->ajim->ny, nza=stup->ajim->nz ;
     float rad=cbrtf(nxa*nya*nza/(nx*ny*nz)) * parm->smooth_radius ;
     qim = GA_smooth( stup->ajim , parm->smooth_code , rad ) ;
     if( qim != NULL ){ mri_free(stup->ajim) ; stup->ajim = qim ; }
   }

   /** load mask array **/

   if( maskim != NULL ){
     MRI_IMAGE *qim = mri_to_byte(maskim) ;
     stup->bmask = MRI_BYTE_PTR(qim) ;
     mri_fix_data_pointer( NULL , qim ) ;
     mri_free(qim) ;
     stup->nmask = THD_countmask( maskim->nvox , stup->bmask ) ;
     if( stup->nmask < 99 ){
       WARNING_message("mri_genalign_scalar: illegal input mask") ;
       free(stup->bmask) ; stup->bmask = NULL ; stup->nmask = 0 ;
     }
   } else {
     stup->bmask = NULL ;
     stup->nmask = 0 ;
   }

   /*-- extract matching points from base image --*/

   if( stup->npt_match <= 9 ){
     stup->npt_match = stup->bsim->nvox ; use_all = 1 ;
   }
   if( stup->nmask > 0 && stup->npt_match > stup->nmask ){
     stup->npt_match = stup->nmask ; use_all = 2 ;
   }

   if( use_all == 1 ){         /*------------- all points, no mask -----------*/

     stup->im = stup->jm = stup->km = NULL ;

   } else if( use_all == 2 ){  /*------------- all points in mask ------------*/

     int nvox , pp ; byte *mask = stup->bmask ;

     stup->im = (float *)malloc(sizeof(float)*stup->npt_match) ;
     stup->jm = (float *)malloc(sizeof(float)*stup->npt_match) ;
     stup->km = (float *)malloc(sizeof(float)*stup->npt_match) ;

     for( mm=pp=0 ; pp < stup->npt_match ; mm++ ){
       if( GOOD(mm) ){
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         stup->im[pp] = ii; stup->jm[pp] = jj; stup->km[pp] = kk;
         pp++ ;
       }
     }

   } else {  /*--------------------- a subset of points ----------------------*/

     int nvox,pp,dm , *qm ; byte *mask = stup->bmask ;

     nvox = stup->bsim->nvox ;
     dm   = find_relprime_fixed(nvox) ;
     stup->im = (float *)malloc(sizeof(float)*stup->npt_match) ;
     stup->jm = (float *)malloc(sizeof(float)*stup->npt_match) ;
     stup->km = (float *)malloc(sizeof(float)*stup->npt_match) ;

     qm = (int *)malloc(sizeof(int)*stup->npt_match) ;
     mm = (nx/2) + (ny/2)*nx + (nz/2)*nxy ;
     for( pp=0 ; pp < stup->npt_match ; mm=(mm+dm)%nvox )
       if( GOOD(mm) ) qm[pp++] = mm ;
     qsort_int( stup->npt_match , qm ) ;

     for( pp=0 ; pp < stup->npt_match ; pp++ ){
       mm = qm[pp] ;
       ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
       stup->im[pp] = ii; stup->jm[pp] = jj; stup->km[pp] = kk;
     }
     free((void *)qm) ;
   }

   /*--- extract values from base image for matching ---*/

   bsar = MRI_FLOAT_PTR(stup->bsim) ;
   stup->bvm = (float *)malloc(sizeof(float)*stup->npt_match) ;
   for( qq=0 ; qq < stup->npt_match ; qq++ ){
     rr = (stup->im != NULL)
          ?  (int)(stup->im[qq] + stup->jm[qq]*nx + stup->km[qq]*nxy) : qq ;
     stup->bvm[qq] = bsar[rr] ;
   }
   if( stup->match_code == GA_MATCH_SPEARMAN_SCALAR )
     stup->bvstat = spearman_rank_prepare( stup->npt_match , stup->bvm ) ;


   RETURN(NULL) ;
}
