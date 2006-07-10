#include "mrilib.h"

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

   if( im == NULL || rad <= 0.0f ) RETURN(NULL) ;

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
       om = mri_medianfilter( im , rad , NULL , 0 ) ;
     break ;
   }

   RETURN(om) ;
}
/*---------------------------------------------------------------------------*/

double GA_scalar_fitter( int npar , double *mpar )
{
  double val=0.0 ;
  return val ;
}

/*---------------------------------------------------------------------------*/

static GA_setup *stup = NULL ;

#undef  GOOD
#define GOOD(i) (mask==NULL || mask[i])

#undef  ERREX
#define ERREX(s) \
 do{ ERROR_message("mri_genalign_scalar: %s",(s)); RETURN(NULL); } while(0)

MRI_IMAGE * mri_genalign_scalar( MRI_IMAGE *basim  ,
                                 MRI_IMAGE *maskim ,
                                 MRI_IMAGE *targim ,
                                 GA_params *parm    )
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

   FREE_GA_setup(stup) ;
   stup = (GA_setup *)calloc(1,sizeof(GA_setup)) ;

   stup->match_code    = parm->match_code    ;
   stup->interp_code   = parm->interp_code   ;
   stup->npt_match     = parm->npt_match     ;
   stup->kernel_code   = parm->kernel_code   ;
   stup->kernel_radius = parm->kernel_radius ;
   stup->npt_sum       = parm->npt_sum       ;

   stup->dim_avec = stup->dim_bvec = 1 ;

   /** load images into setup struct, smoothing if so ordered **/

   stup->bsim = mri_to_float(basim ) ;
   stup->ajim = mri_to_float(targim) ;

   nx = stup->bsim->nx; ny = stup->bsim->ny; nz = stup->bsim->nz; nxy = nx*ny;

   if( parm->smooth_code > 0 ){
     MRI_IMAGE *qim ;
     qim = GA_smooth( stup->bsim , parm->smooth_code , parm->smooth_radius ) ;
     if( qim != NULL ){ mri_free(stup->bsim) ; stup->bsim = qim ; }
   }

   if( parm->smooth_code > 0 ){
     MRI_IMAGE *qim ;
     qim = GA_smooth( stup->ajim , parm->smooth_code , parm->smooth_radius ) ;
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

     stup->im = (int *)malloc(sizeof(int)*stup->npt_match) ;
     stup->jm = (int *)malloc(sizeof(int)*stup->npt_match) ;
     stup->km = (int *)malloc(sizeof(int)*stup->npt_match) ;

     for( mm=pp=0 ; pp < stup->npt_match ; mm++ ){
       if( GOOD(mm) ){
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         stup->im[pp] = ii; stup->jm[pp] = jj; stup->km[pp] = kk;
         pp++ ;
       }
     }

   } else {  /*--------------------- a subset of points ----------------------*/

     int nvox,pp,dm ; byte *mask = stup->bmask ;

     nvox = stup->bsim->nvox ;
     dm   = find_relprime_fixed(nvox) ;
     stup->im = (int *)malloc(sizeof(int)*stup->npt_match) ;
     stup->jm = (int *)malloc(sizeof(int)*stup->npt_match) ;
     stup->km = (int *)malloc(sizeof(int)*stup->npt_match) ;
     mm = (nx/2) + (ny/2)*nx + (nz/2)*nxy ;
     for( pp=0 ; pp < stup->npt_match ; mm=(mm+dm)%nvox ){
       if( GOOD(mm) ){
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         stup->im[pp] = ii; stup->jm[pp] = jj; stup->km[pp] = kk;
         pp++ ;
       }
     }
   }

   /*--- extract values from base image for matching ---*/

   bsar = MRI_FLOAT_PTR(stup->bsim) ;
   stup->bvm = (float *)malloc(sizeof(float)*stup->npt_match) ;
   for( qq=0 ; qq < stup->npt_match ; qq++ ){
     rr = (stup->im != NULL)
          ?  (stup->im[qq] + stup->jm[qq]*nx + stup->km[qq]*nxy) : qq ;
     stup->bvm[qq] = bsar[rr] ;
   }


   RETURN(NULL) ;
}
