#include "mrilib.h"

/*** For inclusion into 3dBallMatch.c ***/

#define MINRAD 7     /* min radius for ball mask (voxels) */
#define USE_PM       /* use +1/-1 instead of +1/0 for masks */
#define USE_FMASK    /* automask the correlation map */
#define USE_SQRTF    /* weight voxel CM by sqrtf(correlation) */

#undef USE_OMP  /* this code doesn't work -- I suspect csfft_OMP.c */
                /* in other words -- do NOT #define USE_OMP here!! */

/*---------------------------------------------------------------------------*/
#ifdef USE_OMP /*** if using OpenMP ***/

 /* prototype */
  static MRI_IMAGE * mri_fft_3Dconvolve_OMP( MRI_IMAGE *aim, MRI_IMAGE *bim ) ;

# define CONVO(a,b)  mri_fft_3Dconvolve_OMP( (a) , (b) )

#else          /*** not using OpenMP ***/

# define CONVO(a,b)  mri_fft_3Dconvolve( (a) , (b) )

#endif
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Make a ball mask, radius brad mm, grid spacings given by dx,dy,dz */
/*---------------------------------------------------------------------------*/

MRI_IMAGE * make_ball_mask_float( float brad, float dx, float dy, float dz )
{
   MRI_IMAGE *bim ; float *bmask ;
   float xq,yq,zq,rq ;
   int ii,jj,kk, irad,jrad,krad, idim,jdim,kdim, ic,jc,kc, ijk ;

   if( brad <= 0.0f ) return NULL ;
   if( dx == 0.0f ) dx = 1.0f ; else dx = fabsf(dx) ;
   if( dy == 0.0f ) dy = 1.0f ; else dy = fabsf(dy) ;
   if( dz == 0.0f ) dz = 1.0f ; else dz = fabsf(dz) ;

   /* radius of ball in voxels */

   irad = (int)rintf(brad/dx) ;
   jrad = (int)rintf(brad/dy) ;
   krad = (int)rintf(brad/dz) ;

   if( irad < MINRAD || jrad < MINRAD || krad < MINRAD ) return NULL ;

   /* size of the box that holds the ball */

   idim = 2*irad+1 ; ic = irad ;
   jdim = 2*jrad+1 ; jc = jrad ;
   kdim = 2*krad+1 ; kc = krad ;
   rq = brad*brad ;

   /* make the box image for the ball mask */

   bim     = mri_new_vol( idim,jdim,kdim , MRI_float ) ;
   bmask   = MRI_FLOAT_PTR(bim) ;
   bim->dx = dx ; bim->dy = dy ; bim->dz = dz ;

   for( ijk=kk=0 ; kk < kdim ; kk++ ){
     zq = (kk-kc)*dz ; zq = zq*zq ;
     for( jj=0 ; jj < jdim ; jj++ ){
       yq = (jj-jc)*dy ; yq = zq + yq*yq ;
       for( ii=0 ; ii < idim ; ii++,ijk++ ){
         xq = (ii-ic)*dx ; xq = yq + xq*xq ;
#ifndef USE_PM
         bmask[ijk] = ( xq <= rq ) ? 1.0f :  0.0f ; /* in=1 out=0 */
#else
         bmask[ijk] = ( xq <= rq ) ? 1.0f : -1.0f ; /* in=1 out=-1 */
#endif
   }}}

   return bim ;
}

/*---------------------------------------------------------------------------*/
/* Make a spheroid (ellipsoid of rotation) mask,
    * semi-axis 1 = arad mm; semi-axes 2,3 = brad mm;
    * grid spacings given by dx,dy,dz ;
    * axis 1 is along vector ax,ay,az (will be normalized to unit length) ;
    * spheroid is prolate if arad > brad ; oblate if arad < brad
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * make_spheroid_mask_float( float arad, float ax, float ay, float az,
                                      float brad, float dx, float dy, float dz )
{
   MRI_IMAGE *bim ; float *bmask ;
   float yq,zq , xqq,aqq,bqq,cqq,crad , xc,yc,zc , arq,brq , yac,zac ;
   int ii,jj,kk, irad,jrad,krad, idim,jdim,kdim, ic,jc,kc, ijk ;

   if( arad <= 0.0f || brad <= 0.0f ) return NULL ;

   if( arad == brad )
     return make_ball_mask_float( arad , dx,dy,dz ) ; /* spherical case */

   arq = 1.0f / (arad*arad) ;
   brq = 1.0f / (brad*brad) ;

   if( dx == 0.0f ) dx = 1.0f ; else dx = fabsf(dx) ;
   if( dy == 0.0f ) dy = 1.0f ; else dy = fabsf(dy) ;
   if( dz == 0.0f ) dz = 1.0f ; else dz = fabsf(dz) ;

   /* normalize (ax,ay,az) to unit length */

   aqq = ax*ax + ay*ay + az*az ;
   if( aqq == 0.0f ){
     ax = 1.0f ; ay = az = 0.0f ;   /* hopeless user */
   } else {
     aqq = sqrtf(aqq) ;             /* normalize (ax,ay,az) vector */
     ax  = ax / aqq ; ay = ay / aqq ; az = az / aqq ;
   }

   /* initial radius of mask in voxels */

   crad = MAX(arad,brad) ;
   irad = (int)rintf(crad/dx) ;
   jrad = (int)rintf(crad/dy) ;
   krad = (int)rintf(crad/dz) ;

   if( irad < MINRAD || jrad < MINRAD || krad < MINRAD ) return NULL ;

   /* size of the box that holds the ball */

   idim = 2*irad+1 ; ic = irad ;
   jdim = 2*jrad+1 ; jc = jrad ;
   kdim = 2*krad+1 ; kc = krad ;

   /* make the box image for the ball mask */

   bim     = mri_new_vol( idim,jdim,kdim , MRI_float ) ;
   bmask   = MRI_FLOAT_PTR(bim) ;
   bim->dx = dx ; bim->dy = dy ; bim->dz = dz ;

   /* loop over (xc,yc,zc) points in the grid;
      compute aqq = length squared of (xc,yc,zc) projected along (ax,ay,az)
      compute bqq = length squared of (xc,yc,zc) projected perp to (ax,ay,az)
                  = length squared of (xc,yc,zc) minus aqq (cf. Pythagoras)
      then, a point is inside the spheroid if aqq/arad^2 + bqq/brad^2 <= 1  */

   for( ijk=kk=0 ; kk < kdim ; kk++ ){
     zc = (kk-kc)*dz ; zq = zc*zc ; zac = zc*az ;
     for( jj=0 ; jj < jdim ; jj++ ){
       yc = (jj-jc)*dy ; yq = yc*yc + zq ; yac = yc*ay + zac ;
       for( ii=0 ; ii < idim ; ii++,ijk++ ){
         xc = (ii-ic)*dx; xqq = xc*xc + yq;  /* xqq = L^2 of (xc,yc,zc) */

         aqq = xc*ax + yac ; aqq = aqq*aqq ; /* aqq = L^2 of (xc,yc,zc)
                                                       along (ax,ay,az) */
         bqq = xqq - aqq ;                   /* bqq = L^2 of (xc,yc,zc)
                                                     perp to (ax,ay,az) */
         cqq = aqq*arq + bqq*brq ;     /* cqq = aqq/arad^2 + bqq/brad^2 */
#ifndef USE_PM
         bmask[ijk] = ( cqq <= 1.0f ) ? 1.0f :  0.0f ; /* in=1 out=0 */
#else
         bmask[ijk] = ( cqq <= 1.0f ) ? 1.0f : -1.0f ; /* in=1 out=-1 */
#endif
   }}}

   /* at this point, could use MRI_autobbox and mri_cut_3D to trim
      image down to exclude any planes of all zeros around the edges */

   return bim ;
}

/*---------------------------------------------------------------------------*/
/* Find the center of mass of the overlapation */
/*---------------------------------------------------------------------------*/

int_triple MRI_mask_overlapation( MRI_IMAGE *aim , float brad )
{
   MRI_IMAGE *bim ;
   MRI_IMAGE *fim ; float *far ; byte *fmask=NULL ;
   int irad,jrad,krad , ii,jj,kk,ijk , nx,ny,nz,nvox ;
   int_triple ijkout = { -666 , -666 , -666 } ;  /* error return */
   float xc,yc,zc,fc,ff,ftop ;
#if 0
   complex minus1 = {-1.0f,0.0f} ;
#endif

   if( aim == NULL || brad <= 0.0f ) return ijkout ;  /* bad inputs */

   /* make a binary mask with a ball of radius brad mm */

   bim = make_ball_mask_float( brad , aim->dx , aim->dy , aim->dz ) ;
   if( bim == NULL ) return ijkout ;

   /* dimensions */

   irad = (bim->nx-1)/2 ;  /* radius of ball in voxels */
   jrad = (bim->ny-1)/2 ;
   krad = (bim->nz-1)/2 ;

   /* convolve ball mask with the input anatomical mask */

#if 0
   EDIT_set_padval( MRI_complex, (void *)&minus1 ) ; /* don't zeropad, -1 pad */
#endif

/* INFO_message("about to CONVO") ; */
   fim = CONVO( aim , bim ) ;  /* see top of file for definition of CONVO */

#if 0
   EDIT_clear_padval ;                               /* back to zeropad */
#endif

/* INFO_message("about to mri_free(bim)") ; */
   mri_free(bim) ;
   if( fim == NULL ) return ijkout ;

   /* find automask of the convolved image? */
#ifdef USE_FMASK
   fmask = mri_automask_image( fim ) ;
   if( fmask == NULL ){ mri_free(fim) ; return ijkout ; } /* bad */
#endif

   far = MRI_FLOAT_PTR(fim) ;
   nx  = fim->nx ; ny = fim->ny ; nz = fim->nz ; nvox = nx*ny*nz ;
   fc  = xc = yc = zc = ftop = 0.0f ;

   /* find the max convolution value */

   for( ijk=0 ; ijk < nvox ; ijk++ ){
#ifdef USE_FMASK
     if( !fmask[ijk]     ) far[ijk] = 0.0f ;     /* use the automask */
#endif
     if( far[ijk] > ftop ) ftop     = far[ijk] ; /* find the max */
   }
#ifdef USE_FMASK
/* INFO_message("about to free(fmask)") ; */
   free(fmask) ;
#endif
   if( ftop == 0.0f ){
     mri_free(fim) ; return ijkout ;   /* bad */
   }
   ftop = 0.09*ftop ;                  /* threshold for using voxel */

   /* compute CM */

   for( ijk=kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++,ijk++ ){
         ff = far[ijk] ;
         if( ff >= ftop ){
#ifdef USE_SQRTF
           ff = sqrtf(ff);   /* downweight */
#endif
           xc += ff*ii; yc += ff*jj; zc += ff*kk; fc += ff;
         }
   }}}
/* INFO_message("about to mri_free(fim)") ; */
   mri_free(fim) ;
   if( fc == 0.0f ) return ijkout ;  /* no positive values? */

   /* subtract off ball radius from CM to get location in original dataset */

   ijkout.i = (int)rintf(xc/fc) - irad ;
   ijkout.j = (int)rintf(yc/fc) - jrad ;
   ijkout.k = (int)rintf(zc/fc) - krad ;

   return ijkout ;
}

/*---------------------------------------------------------------------------*/
/* Find the 'best' overlap with a ball of radius brad mm. */
/*---------------------------------------------------------------------------*/

int_triple THD_mask_overlapation( THD_3dim_dataset *dset , float brad )
{
   MRI_IMAGE *aim ; byte *aar ; int ii ;
   int_triple ijkout = { -666 , -666 , -666 } ;
   float dxyz ; int npeel ;

   if( !ISVALID_DSET(dset) || brad <= 0.0f ) return ijkout ;

   /* autmask the dataset */

   dxyz = cbrtf(fabsf( DSET_DX(dset)*DSET_DY(dset)*DSET_DZ(dset) )) ;
   if( dxyz <= 0.0f ) dxyz = 1.0f ;
   npeel = (int)rintf( 0.266f*brad/dxyz ) ;
   if( npeel <= 0   ) npeel = 1 ;
   THD_automask_set_peelcounts(npeel,17) ;
   THD_automask_set_clipfrac(0.135f) ;
   THD_automask_extclip(1) ;
   aar = THD_automask(dset) ;  /* in=1 out=0 */

   /* stuff automask into an MRI_IMAGE */

#ifndef USE_PM
   /* use the 0-1 mask */
   aim = mri_empty_conforming( DSET_BRICK(dset,0) , MRI_byte ) ;
   mri_set_data_pointer( aim , aar ) ;
#else
   /* convert to a -1/+1 mask */
   { float *nar ; int nvox ;
     aim = mri_new_conforming( DSET_BRICK(dset,0), MRI_float ) ;
     nar = MRI_FLOAT_PTR(aim) ; nvox = aim->nvox ;
     for( ii=0 ; ii < nvox ; ii++ ){
       nar[ii] = ( aar[ii] ) ? +1.0f : -1.0f ;  /* in=+1 out=-1 */
     }
     free(aar) ;
   }
#endif
   aim->dx = DSET_DX(dset); aim->dy = DSET_DY(dset); aim->dz = DSET_DZ(dset);

   /* compute the 'best' mask overlap location and return it */

   ijkout = MRI_mask_overlapation( aim , brad ) ;
   mri_free(aim) ;
   return ijkout ;
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*** Some specialized OpenMP-ized 3D FFT functions [Oct 2020] ***/

/*** DO NOT ENABLE THIS CODE -- I suspect csfft_OMP.c has some problems ***/

#ifdef USE_OMP

#include "csfft_OMP.c"   /* OpenMP compatible FFT functions */
                         /* They don't use OpenMP themselves, */
                         /* but they are thread safe, unlike csfft.c */
/*----------------------------------------------------------------------------*/
/* Replaces mri_fft_3D() in mri_fft_complex.c -- note that alt arg is ignored
   FFT lengths are in Lxx, Lyy, Lzz; however,
     Lxx = 0 ==> no FFT in that direction (etc.).
*//*--------------------------------------------------------------------------*/

static MRI_IMAGE * mri_fft_3D_OMP( int Sign, MRI_IMAGE *inim,
                                   int Lxx,int Lyy,int Lzz, int alt )
{
   MRI_IMAGE *outim ;
   int ii,jj,kk , nx,ny,nxy,nz , nbig , fx,fy,fz,fxy ;
   complex *car , *far ;
   AO_DEFINE_ARRAY(complex,cbig) ;

   if( inim->kind != MRI_complex ) return NULL ;

   /* input data and its dimensions */

   car = MRI_COMPLEX_PTR(inim) ;
   nx = inim->nx ; ny = inim->ny ; nz = inim->nz ; nxy = nx*ny ;

   /* output dimensions and data */

   fx = (Lxx == 0) ? nx : (Lxx > nx) ? csfft_nextup_one35(Lxx) : csfft_nextup_one35(nx);
   fy = (Lyy == 0) ? ny : (Lyy > ny) ? csfft_nextup_one35(Lyy) : csfft_nextup_one35(ny);
   fz = (Lzz == 0) ? nz : (Lzz > nz) ? csfft_nextup_one35(Lzz) : csfft_nextup_one35(nz);
   fxy = fx*fy ;

   outim = mri_new_vol( fx,fy,fz , MRI_complex ) ;  /* zero filled */
   far   = MRI_COMPLEX_PTR(outim) ;

   /* buffer space */

   nbig = MAX(fx,fy) ; nbig = MAX(nbig,fz) ; nbig = 4*nbig + 512 ;
#pragma omp parallel
{ AO_RESIZE_ARRAY(complex,cbig,nbig) ;
  fprintf(stderr,"resize cbig#%d to %d\n",AOth,nbig) ;
}

   /* copy input data into output image */

INFO_message("about to copy data from car to far") ;
   for( kk=0 ; kk < nz ; kk++ )
     for( jj=0 ; jj < ny ; jj++ )
       memcpy( far + jj*fx + kk*fxy, car + jj*nx + kk*nxy, sizeof(complex)*nx );

INFO_message("calling csfft_cox_OMP_SETUP") ;
   csfft_cox_OMP_SETUP() ;

   /* x-direction FFTs */

AFNI_OMP_START ;
   if( Lxx > 1 ){
INFO_message("start x FFTs") ;
#pragma omp parallel
   { int kk,koff , jj,joff , ii ; complex *cbig ;
     cbig = AO_VALUE(cbig) ;
     for( kk=0 ; kk < fz ; kk++ ){
       koff = kk*fxy ;
#pragma omp for
       for( jj=0 ; jj < fy ; jj++ ){
         joff = koff + jj*fx ;
         for( ii=0 ; ii < fx ; ii++ ) cbig[ii] = far[ii+joff] ;
         csfft_cox_OMP( Sign , fx , cbig ) ;
         for( ii=0 ; ii < fx ; ii++ ) far[ii+joff] = cbig[ii] ;
       }
     }
   }
INFO_message("x FFTs(%d) done",fx) ;
   }
AFNI_OMP_END ;

   /* y-direction FFTs */

AFNI_OMP_START ;
   if( Lyy > 1 ){
INFO_message("start y FFTs") ;
#pragma omp parallel
   { int kk,koff , jj,joff , ii ; complex *cbig ;
     cbig = AO_VALUE(cbig) ;
     for( kk=0 ; kk < fz ; kk++ ){
       koff = kk*fxy ;
       for( ii=0 ; ii < fx ; ii++ ){
         joff = koff + ii ;
         for( jj=0 ; jj < fy ; jj++ ) cbig[jj] = far[jj*fx+joff] ; /* copy data */
         csfft_cox_OMP( Sign , fy , cbig ) ;                       /* FFT in buffer */
         for( jj=0 ; jj < fy ; jj++ ) far[jj*fx+joff] = cbig[jj] ; /* copy back */
       }
     }
   }
INFO_message("y FFTs(%d) done",fy) ;
   }
AFNI_OMP_END ;

   /* z-direction FFTs */

AFNI_OMP_START ;
   if( Lzz > 1 ){
INFO_message("start z FFTs") ;
#pragma omp parallel
   { int kk,koff , jj,joff , ii ; complex *cbig ;
     cbig = AO_VALUE(cbig) ;
     for( jj=0 ; jj < fy ; jj++ ){
       joff = jj*fx ;
       for( ii=0 ; ii < fx ; ii++ ){
         koff = joff + ii ;
         for( kk=0 ; kk < fz ; kk++ ) cbig[kk] = far[kk*fxy+koff] ;
         csfft_cox_OMP( Sign , fz , cbig ) ;
         for( kk=0 ; kk < fz ; kk++ ) far[kk*fxy+koff] = cbig[kk] ;
       }
     }
   }
INFO_message("z FFTs(%d) done",fz) ;
   }
AFNI_OMP_END ;

INFO_message("all FFTs done") ;
   MRI_COPY_AUX(outim,inim) ; return outim ;
}

/*----------------------------------------------------------------------------*/
/* Replaces mri_fft_3Dconvolve() in mri_fft_complex.c [Oct 2020]
   Convolve (via FFT) image aim with bim.
   Note output image will be at least as big as than the sum of the two sizes.
*//*--------------------------------------------------------------------------*/

static MRI_IMAGE * mri_fft_3Dconvolve_OMP( MRI_IMAGE *aim , MRI_IMAGE *bim )
{
   MRI_IMAGE *outim=NULL ;
   MRI_IMAGE *paim , *pbim , *faim , *fbim ;
   int nxa,nya,nza , nxb,nyb,nzb , Lxx,Lyy,Lzz , Lxyz,ii ;
   complex  ac   ,  bc   , qc ;
   complex *acar , *bcar ;
   float linv ;

   if( aim == NULL || bim == NULL ) return NULL ;

   /* input dimensions */

   nxa = aim->nx ; nya = aim->ny ; nza = aim->nz ;
   nxb = bim->nx ; nyb = bim->ny ; nzb = bim->nz ;

   /* FFT and output dimensions (sum, bumped up for FFT effiency) */

   Lxx = (nxa > 1 && nxb > 1) ? csfft_nextup_one35(nxa+nxb) : 0 ;
   Lyy = (nya > 1 && nyb > 1) ? csfft_nextup_one35(nya+nyb) : 0 ;
   Lzz = (nza > 1 && nzb > 1) ? csfft_nextup_one35(nza+nzb) : 0 ;

   /* at this time, we don't allow for convolving a 3D image with a 1D
      or 2D image, for example, which is possible but more complicated */

   if( Lxx == 0 || Lyy == 0 || Lzz == 0 ) return NULL ;

   /* 1) convert A image to complex
      2) zero pad it to fit the FFT size
      3) FFT that
      Then repeat these steps for the B image */

   faim = mri_to_complex( aim ) ;                                      /* 1) */
   paim = mri_zeropad_3D( 0,Lxx-nxa , 0,Lyy-nya , 0,Lzz-nza , faim ) ; /* 2) */
INFO_message("about to mri_free(faim)") ;
   mri_free(faim) ;
   faim = mri_fft_3D_OMP( -1 , paim , Lxx,Lyy,Lzz , 0 ) ;              /* 3) */
INFO_message("about to mri_free(paim)") ;
   mri_free(paim) ;
   acar = MRI_COMPLEX_PTR(faim) ;

   fbim = mri_to_complex( bim ) ;                                      /* 1) */
   pbim = mri_zeropad_3D( 0,Lxx-nxb , 0,Lyy-nyb , 0,Lzz-nzb , fbim ) ; /* 2) */
INFO_message("about to mri_free(fbim)") ;
   mri_free(fbim) ;
   fbim = mri_fft_3D_OMP( -1 , pbim , Lxx,Lyy,Lzz , 0 ) ;              /* 3) */
INFO_message("about to mri_free(pbim)") ;
   mri_free(pbim) ;
   bcar = MRI_COMPLEX_PTR(fbim) ;

   /* multiply+scale FFTs, store back in faim/acar */

   Lxyz = Lxx * Lyy * Lzz ;
   linv = 10.f / (float)Lxyz ;       /* scaling for inverse FFT */
   for( ii=0 ; ii < Lxyz ; ii++ ){
     ac = acar[ii] ;
     bc = bcar[ii] ;
     qc.r = (ac.r * bc.r - ac.i * bc.i) * linv ; /* complex */
     qc.i = (ac.r * bc.i + ac.i * bc.r) * linv ; /* multiply */
     acar[ii] = qc ;
   }
INFO_message("about to mri_free(fbim) again") ;
   mri_free(fbim) ;

   /* inverse FFT back to 'real' space */

   fbim = mri_fft_3D_OMP( +1 , faim , Lxx,Lyy,Lzz , 0 ) ;
INFO_message("about to mri_free(faim) again") ;
   mri_free(faim) ;

   /* convert to float-valued image (from complex FFT) and return */

   outim = mri_complex_to_real( fbim ) ;
INFO_message("about to mri_free(faim) yet again") ;
   mri_free(fbim) ;
   return outim ;
}

#endif /* USE_OMP */
