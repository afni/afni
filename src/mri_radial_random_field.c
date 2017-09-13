#include "mrilib.h"

/*** This file is intended to be #include-d into another source file, in
     particular into 3dClustSim.c -- for optimization and OpenMP-ization ***/

#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef USE_OMP
# include <omp.h>
#endif

/*** Thread-safe FFT functions in csfft_OMP.c are used ***/

#include "csfft_OMP.c"
#define CFFT csfft_cox_OMP
#define CFFT_setup csfft_cox_OMP_SETUP()

/*---------------------------------------------------------------------------*/
#include "zgaussian.c"  /** Ziggurat Gaussian random number generator **/
/*---------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Gaussian + Exponential function of radius; this is the ACF(r) model. */

static INLINE float rfunc( float r , float *parm )
{
   return       parm[0] * expf(-0.5f*r*r/(parm[1]*parm[1]))
         +(1.0f-parm[0])* expf(-r/parm[2])                 ;
}

/*----------------------------------------------------------------------------*/
/* To help find the inverse of rfunc; cf. rfunc_inv() */

static float rfunc_falsi_step( float *parm , float val , float rlo , float rhi )
{
   float v0,v1,dv ;

   v0 = rfunc(rlo,parm) ;
   v1 = rfunc(rhi,parm) ; dv = v1-v0 ;

   /* not enough variability to continue? */

   if( dv == 0.0f || fabsf(dv) < 0.005f*(fabsf(val-v0)+fabsf(val-v1)) ) return rlo ;

   /* regula falsi = linear inverse interpolation */

   dv = rlo + (rhi-rlo)/dv * (val-v0) ; return dv ;
}

/*----------------------------------------------------------------------------*/
/* Inverse function of rfunc; e.g., FWHM = 2*rfunc_inf(0.5,parm) */

static float rfunc_inv( float val , float *parm )
{
   float rlo,rhi,rtop , vlo,vhi , dr,vv ;
   int ii ;

   if( val >= 1.0f ) return 0.0f ;

   /* range for initial search */

   rtop = 3.0f*parm[1] + 6.0f*parm[2] ;
   if( val <= 0.0001f ) return rtop ;

   rlo = 0.02f*parm[1] ;
   rhi = 0.02f*parm[2] ;
   dr  = MIN(rlo,rhi) ;

   /* bracket val between vlo=rfunc(rlo) and vhi=rfunc(rhi) */

   vlo = 1.0f ; rlo = 0.0f ;
   for( ; rlo < rtop ; ){
     rhi = rlo + dr ;
     vhi = rfunc( rhi , parm ) ;
     if( vhi < val ) break ;
     rlo = rhi ; vlo = vhi ;
   }
   if( rlo >= rtop ) return rtop ;

   /* two regula falsi steps are adequate for our purposes */

   dr = rfunc_falsi_step( parm , val , rlo,rhi ) ;
   vv = rfunc( dr , parm ) ;
   if( vv > val ) rlo = dr ; else rhi = dr ;
   dr = rfunc_falsi_step( parm , val , rlo,rhi ) ;
   return dr ;
}

/*----------------------------------------------------------------------------*/
/* What it says. The dimensions are assumed to be compatible with csfft_cox().
   The array tar, if not NULL, must be at least as long as MAX(ny,nz).
*//*--------------------------------------------------------------------------*/

static void mri_fft3D_inplace( MRI_IMAGE *fim , complex *tar )
{
   complex *far , *qar ;
   int nx,ny,nz,nxy, ii,jj,kk , nn,qq ;

   if( fim == NULL || fim->kind != MRI_complex ) return ;
   far = MRI_COMPLEX_PTR(fim) ;
   if( far == NULL )                             return ;

   nx = fim->nx ; ny = fim->ny ; nz = fim->nz ; nxy = nx*ny ;

   /* x FFTs */

   CFFT_setup ;  /* for the OMP version of csfft,
                    which allows csfft_cox_OMP to be called inside a thread */

   for( qar=far,kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ , qar+=nx ){
      CFFT( -1 , nx , qar ) ;
   }}

   /* y FFTs */

   if( tar == NULL ){  /* create temp space */
     qar = (complex *)malloc(sizeof(complex)*MAX(ny,nz)) ;
   } else {
     qar = tar ;       /* user-supplied temp space */
   }

   for( kk=0 ; kk < nz ; kk++ ){
    for( ii=0 ; ii < nx ; ii++ ){
      qq = ii+kk*nxy ;
      for( jj=0 ; jj < ny ; jj++ ) qar[jj] = far[qq+jj*nx] ;
      CFFT( -1 , ny , qar ) ;
      for( jj=0 ; jj < ny ; jj++ ) far[qq+jj*nx] = qar[jj] ;
   }}

   /* z FFTs */

   for( jj=0 ; jj < ny ; jj++ ){
    for( ii=0 ; ii < nx ; ii++ ){
      qq = ii+jj*nx ;
      for( kk=0 ; kk < nz ; kk++ ) qar[kk] = far[qq+kk*nxy] ;
      CFFT( -1 , nz , qar ) ;
      for( kk=0 ; kk < nz ; kk++ ) far[qq+kk*nxy] = qar[kk] ;
   }}

   if( tar == NULL ) free(qar) ;  /* did we create this? */
   return ;
}

/*----------------------------------------------------------------------------*/
/* Given a noise field size and the kernel function parameters,
   determine the grid size for the FFTs to perform, including
   some buffering for edge effects of the kernel function.
   Output grid sizes are even and compatible with csfft_cox().
*//*--------------------------------------------------------------------------*/

static int_triple get_random_field_size( int   nx, int   ny, int   nz,
                                         float dx, float dy, float dz,
                                         float *parm )
{
   int_triple ijk ;
   float rr ; int qq , xx,yy,zz ;

   rr = rfunc_inv( 0.02f , parm ) ; /* radius of kernel function for buffer */
   INFO_message("Kernel function radius = %.2f mm",rr) ;
   xx = nx + 2*(int)ceilf(rr/dx) ;  /* expand for this buffer */
   yy = ny + 2*(int)ceilf(rr/dy) ;
   zz = nz + 2*(int)ceilf(rr/dz) ;
   if( xx < 16 ) xx = 16 ;
   if( yy < 16 ) yy = 16 ;
   if( zz < 16 ) zz = 16 ;
   xx = csfft_nextup_one35(xx) ;    /* expand for FFT allowable sizes */
   yy = csfft_nextup_one35(yy) ;    /* (even with at most one) */
   zz = csfft_nextup_one35(zz) ;    /* (factor of 3 and/or 5,) */
                                    /* (for efficient FFT-ing) */

   ijk.i = xx ; ijk.j = yy ; ijk.k = zz ; return ijk ;
}

/*----------------------------------------------------------------------------*/
/* Create the kernel smoothing function, then FFT it, and clip/shrink it
   as far as reasonable.  The basic method for simulating the noise random
   field is then
     (1) create an iid N(0,1) Gausian random field in FFT space
     (2) multiply this field by the radial weight function from here
     (3) FFT to real space
     (4) [in 3dClustSim] truncate back to desired 3D grid and normalize
         the standard deviation
   This method relies on the fact that the Fourier transform of white
   noise is also white noise (if this isn't obvious to you, do the algebra).

   Note that nx,ny,nz must be compatible with csfft_cox(), which
   will be easiest if get_random_field_size() is used to set them.
*//*--------------------------------------------------------------------------*/

/* macros for 3D array access */

#undef  WAR
#define WAR(i,j,k) war[(i)+(j)*nx+(k)*nxy]
#undef  QAR
#define QAR(i,j,k) qar[(i)+(j)*nxh+(k)*nxyh]
#undef  FAR
#define FAR(i,j,k) far[(i)+(j)*itop+(k)*ijtop]

static MRI_IMAGE * make_radial_weight( int   nx , int   ny , int   nz ,
                                       float dx , float dy , float dz ,
                                       float *parm )
{
   MRI_IMAGE *wim ,       *fim , *qim ;
   complex   *war ; float *far , *qar ;
   int nxy=nx*ny , ii,jj,kk , nxh=nx/2,nyh=ny/2,nzh=nz/2 , nxyh=nxh*nyh ;
   int itop,jtop,ktop,ijtop ;
   float rr , xx,yy,zz , ftop ;

   /* ACF in real space */

   wim = mri_new_vol( nx,ny,nz , MRI_complex ) ;
   war = MRI_COMPLEX_PTR(wim) ;

   /* fill the array with the ACF in real space */

   for( kk=0 ; kk < nz ; kk++ ){
     zz = (kk < nzh) ? kk*dz : (nz-kk)*dz ;
     for( jj=0 ; jj < ny ; jj++ ){
       yy = (jj < nyh) ? jj*dy : (ny-jj)*dy ;
       for( ii=0 ; ii < nx ; ii++ ){
         if( ii==nxh || jj==nyh || kk==nzh ){  /* Nyquist */
           WAR(ii,jj,kk) = CMPLX(0.0f,0.0f) ;
         } else {
           xx = (ii < nxh) ? ii*dx : (nx-ii)*dx ;
           rr = rfunc( sqrtf(xx*xx+yy*yy+zz*zz) , parm ) ;
           WAR(ii,jj,kk) = CMPLX(rr,0.0f) ;
         }
       }
     }
   }

   /* go to FFT space */

   mri_fft3D_inplace( wim , NULL ) ;

   /* create clipped output */

   qim = mri_new_vol( nxh,nyh,nzh , MRI_float ) ;
   qar = MRI_FLOAT_PTR(qim) ;

   ftop = 0.00001f * fabsf(war[0].r) ;  /* largest value to keep */

   /* half size in each direction since is symmetric */
   /* note we compute sqrt(FFT(wim)) since we are creating the
      weight for the noise
      -- which is effectively squared when the ACF is computed/estimated */

   for( kk=0 ; kk < nzh ; kk++ ){
    for( jj=0 ; jj < nyh ; jj++ ){
     for( ii=0 ; ii < nxh ; ii++ ){
       rr = WAR(ii,jj,kk).r ;       /* imag part should be very small */
       if( rr < ftop ) rr = 0.0f ; else rr = sqrtf(rr) ; /* clip here */
       QAR(ii,jj,kk) = rr ;                            /* load output */
   }}}

   mri_free(wim) ;  /* done with this */

   /* shrink it in space, if possible */

   MRI_autobbox( qim , NULL,&itop , NULL,&jtop , NULL,&ktop ) ;

   if( itop < nxh-1 ) itop++ ;
   if( jtop < nyh-1 ) jtop++ ;
   if( ktop < nzh-1 ) ktop++ ;
   ijtop = itop*jtop ;
   fim = mri_new_vol( itop , jtop , ktop , MRI_float ) ;
   far = MRI_FLOAT_PTR(fim) ;
   ININFO_message("Kernel image dimensions %d x %d x %d",itop,jtop,ktop) ;

   for( kk=0 ; kk < ktop ; kk++ ){
    for( jj=0 ; jj < jtop ; jj++ ){
     for( ii=0 ; ii < itop ; ii++ ){
       FAR(ii,jj,kk) = QAR(ii,jj,kk) ;
   }}}

   mri_free(qim) ; return fim ;
}

#undef WAR
#undef QAR
#undef FAR

/*----------------------------------------------------------------------------*/
/* Create TWO instances of a random field.
   Result has 0 mean but stdev depends on what wtim contains.
     nx,ny,nz = dimensions of output
     wtim     = weight image in FFT space
     tar      = FFT workspace - at least MAX(ny,nz) long (or NULL)
     xran     = random seed vector
   Why TWO? It is easiest to just fill the FFT space with the complex random
   Gaussians, then FFT to real space -- giving us a complex random field
   with the desired ACF(r).  Then in 3dClustSim, we'll just use this pair
   of results alternately.
*//*--------------------------------------------------------------------------*/

/* more macros for 3D array access */

#undef  CXAR
#define CXAR(i,j,k) cxar[(i)+(j)*nx+(k)*nxy]

#undef  WTAR
#define WTAR(i,j,k) wtar[(i)+(j)*nxw+(k)*nxyw]

/* macro to fill the complex FFT value with noise * radial weight */

#undef  CXRAN
#define CXRAN(i,j,k) \
 ( zr=ww*zgaussian_sss(xran), zi=ww*zgaussian_sss(xran), CXAR(i,j,k)=CMPLX(zr,zi) )

static MRI_IMARR * make_radial_random_field( int nx, int ny, int nz ,
                                             MRI_IMAGE *wtim ,
                                             complex *tar ,
                                             unsigned short xran[] )
{
   MRI_IMAGE *cxim ; complex *cxar ;
   MRI_IMARR *outar ;
   float     *wtar ;
   int nxy , nxh,nyh,nzh , nxw,nyw,nzw,nxyw , ii,jj,kk ;
   float ww , zr, zi ;

   cxim = mri_new_vol( nx,ny,nz , MRI_complex ) ;
   cxar = MRI_COMPLEX_PTR(cxim) ;
   wtar = MRI_FLOAT_PTR(wtim) ;
   nxw  = wtim->nx ; nyw = wtim->ny ; nzw = wtim->nz ; nxyw = nxw*nyw ;
   nxy  = nx*ny ;

   /* fill random values */

   for( kk=1 ; kk < nzw ; kk++ ){     /* interior grid */
    for( jj=1 ; jj < nyw ; jj++ ){
     for( ii=1 ; ii < nxw ; ii++ ){
       ww = WTAR(ii,jj,kk) ;
       if( ww != 0.0f ){  /* fill all 8 reflections */
         CXRAN(ii,   jj,   kk) ; CXRAN(nx-ii,   jj,   kk) ;
         CXRAN(ii,ny-jj,   kk) ; CXRAN(nx-ii,ny-jj,   kk) ;
         CXRAN(ii,   jj,nz-kk) ; CXRAN(nx-ii,   jj,nz-kk) ;
         CXRAN(ii,ny-jj,nz-kk) ; CXRAN(nx-ii,ny-jj,nz-kk) ;
       }
   }}}

   for( kk=1 ; kk < nzw ; kk++ ){   /* along ii=0 face */
    for( jj=1 ; jj < nyw ; jj++ ){
      ww = WTAR(0,jj,kk) ;
      if( ww != 0.0f ){   /* just 4 reflections */
        CXRAN(0,jj,   kk) ; CXRAN(0,ny-jj,   kk) ;
        CXRAN(0,jj,nz-kk) ; CXRAN(0,ny-jj,nz-kk) ;
      }
   }}

   for( jj=1 ; jj < nyw ; jj++ ){   /* along kk=0 face */
    for( ii=1 ; ii < nxw ; ii++ ){
      ww = WTAR(ii,jj,0) ;
      if( ww != 0.0f ){   /* just 4 reflections */
        CXRAN(ii,   jj,0) ; CXRAN(nx-ii,   jj,0) ;
        CXRAN(ii,ny-jj,0) ; CXRAN(nx-ii,ny-jj,0) ;
      }
   }}

   for( kk=1 ; kk < nzw ; kk++ ){   /* along jj=0 face */
    for( ii=1 ; ii < nxw ; ii++ ){
      ww = WTAR(ii,0,kk) ;
      if( ww != 0.0f ){   /* just 4 reflections */
        CXRAN(ii,0,   kk) ; CXRAN(nx-ii,0,   kk) ;
        CXRAN(ii,0,nz-kk) ; CXRAN(nx-ii,0,nz-kk) ;
      }
   }}

   for( kk=1 ; kk < nzw ; kk++ ){  /* along ii=jj=0 edge */
     ww = WTAR(0,0,kk) ;
     if( ww != 0.0f ){  /* just 2 reflections */
       CXRAN(0,0,kk) ; CXRAN(0,0,nz-kk) ;
     }
   }

   for( jj=1 ; jj < nyw ; jj++ ){  /* along ii=kk=0 edge */
     ww = WTAR(0,jj,0) ;
     if( ww != 0.0f ){  /* just 2 reflections */
       CXRAN(0,jj,0) ; CXRAN(0,ny-jj,0) ;
     }
   }

   for( ii=1 ; ii < nxw ; ii++ ){  /* along jj=kk=0 edge */
     ww = WTAR(ii,0,0) ;
     if( ww != 0.0f ){  /* just 2 reflections */
       CXRAN(ii,0,0) ; CXRAN(nx-ii,0,0) ;
     }
   }

   CXAR(0,0,0) = CMPLX(0.0f,0.0f) ;   /* the origin ii=jj=kk=0 */
                                      /* set to zero to get zero mean output */
   /* FFT to real space */

   mri_fft3D_inplace( cxim , tar ) ;

   /* create output and exit */

   outar = mri_complex_to_pair( cxim ) ;
   mri_free(cxim) ;

   return outar ;
}

#if 0
/*----------------------------------------------------------------------------*/
/* Main program for testing purposes */

int main( int argc , char *argv[] )
{
   float rr , vv , parm[3] ;
   int_triple ijk ;
   float dxyz=2.0f ; int nxyz=128 ; double ct,wt ;
   MRI_IMAGE *wim ;
   THD_3dim_dataset *dset=NULL ;
   THD_ivec3 nxyz_vec ; THD_fvec3 fxyz_vec , oxyz_vec ;
   unsigned short xran[3] ; unsigned int gseed ; int ithr=1,nbrik=2 ;
   int nthr=1 , ith ;

   parm[0] = 0.66f ;
   parm[1] = 6.62f ;
   parm[2] = 11.09f ;

   if( argc > 1 ){
     nbrik = (int)strtod(argv[1],NULL) ;
     if( nbrik < 2 ) nbrik = 2 ;
   }
   if( nbrik%2 == 1 ) nbrik++ ;

   ijk = get_random_field_size( nxyz , nxyz , nxyz ,
                                dxyz , dxyz , dxyz , parm ) ;

   nxyz = ijk.i ;
   wim = make_radial_weight( nxyz,nxyz,nxyz , dxyz,dxyz,dxyz , parm ) ;

   gseed = ((unsigned int)time(NULL)) + 17*(unsigned int)getpid() ;
   xran[2] = ( gseed        & 0xffff) + (unsigned short)ithr ;
   xran[1] = ((gseed >> 16) & 0xffff) - (unsigned short)ithr ;
   xran[0] = 0x330e                   + (unsigned short)ithr ;

   INFO_message("creating random dataset") ;

   dset = EDIT_empty_copy(NULL) ;
   nxyz_vec.ijk[0] = nxyz ; nxyz_vec.ijk[1] = nxyz ; nxyz_vec.ijk[2] = nxyz ;
   fxyz_vec.xyz[0] = dxyz ; fxyz_vec.xyz[1] = dxyz ; fxyz_vec.xyz[2] = dxyz ;
   oxyz_vec.xyz[0] = 0.0f ; oxyz_vec.xyz[1] = 0.0f ; oxyz_vec.xyz[2] = 0.0f ;

   EDIT_dset_items( dset ,
                      ADN_datum_all  , MRI_float           ,
                      ADN_nxyz       , nxyz_vec            ,
                      ADN_xyzdel     , fxyz_vec            ,
                      ADN_xyzorg     , oxyz_vec            ,
                      ADN_prefix     , "./randomFFT"       ,
                      ADN_nvals      , nbrik               ,
                      ADN_malloc_type, DATABLOCK_MEM_MALLOC,
                      ADN_type       , HEAD_ANAT_TYPE      ,
                      ADN_view_type  , VIEW_ORIGINAL_TYPE  ,
                      ADN_func_type  , ANAT_EPI_TYPE       ,
                    ADN_none ) ;

#ifdef USE_OMP
#pragma omp parallel
 { if( omp_get_thread_num() == 0 ){
     nthr = omp_get_num_threads() ;
     ININFO_message("number of threads = %d",nthr) ;
 }}
#endif

   ct = COX_cpu_time() ; wt = COX_clock_time() ;

AFNI_OMP_START ;
#pragma omp parallel
 { int ib ; MRI_IMARR *abar; MRI_IMAGE *aim,*bim; complex *tar;
   tar = (complex *)malloc(sizeof(complex)*(nxyz+nxyz)) ;
#pragma omp for
   for( ib=0 ; ib < nbrik ; ib+=2 ){
     abar = make_radial_random_field( nxyz,nxyz,nxyz , wim , tar , xran ) ;
     aim = IMARR_SUBIM(abar,0) ; bim = IMARR_SUBIM(abar,1) ;
#pragma omp critical
     { EDIT_substitute_brick( dset , ib   , MRI_float , MRI_FLOAT_PTR(aim) ) ;
       EDIT_substitute_brick( dset , ib+1 , MRI_float , MRI_FLOAT_PTR(bim) ) ;
     }
     FREE_IMARR(abar) ; abar = NULL ;
   }
   free(tar) ;
 }
AFNI_OMP_END ;

   ct = COX_cpu_time()-ct ; wt = COX_clock_time()-wt ;
   ININFO_message("CPU time = %.1f  Clock time = %.1f  Ratio = %.1f",ct,wt,ct/wt) ;

   ININFO_message("writing dataset") ;
   DSET_write(dset) ; WROTE_DSET(dset) ; DSET_delete(dset) ; dset = NULL ;

   exit(0) ;
}
#endif
