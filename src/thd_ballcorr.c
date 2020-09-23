#include "mrilib.h"

/*** For inclusion into 3dBallMatch.c ***/

#define MINRAD 7     /* min radius for ball mask (voxels) */
#define USE_PM       /* use +1/-1 instead of +1/0 for masks */
#define USE_FMASK    /* automask the correlation map */
#define USE_SQRTF    /* weight voxel CM by sqrtf(correlation) */

/*---------------------------------------------------------------------------*/
/* Make a ball mask, radius brad mm, grid spacings given by dx,dy,dz */
/*---------------------------------------------------------------------------*/

MRI_IMAGE * make_ball_mask_float( float brad, float dx, float dy, float dz )
{
   MRI_IMAGE *bim ; float *bmask ;
   float xq,yq,zq,rq ;
   int ii,jj,kk, irad,jrad,krad, idim,jdim,kdim, ic,jc,kc, ijk ;

   if( brad <= 0.0f ) return NULL ;
   if( dx == 0.0f ) dx = 1.0f ;
   if( dy == 0.0f ) dy = 1.0f ;
   if( dz == 0.0f ) dz = 1.0f ;

   /* radius of ball in voxels */

   irad = (int)rintf(brad/fabsf(dx)) ;
   jrad = (int)rintf(brad/fabsf(dy)) ;
   krad = (int)rintf(brad/fabsf(dz)) ;

   if( irad < MINRAD || jrad < MINRAD || krad < MINRAD ) return NULL ;

   /* size of the box that holds the ball */

   idim = 2*irad+1 ; ic = irad ;
   jdim = 2*jrad+1 ; jc = jrad ;
   kdim = 2*krad+1 ; kc = krad ;
   rq = brad*brad ;

   /* make the box image for the ball mask */

   bim     = mri_new_vol( idim,jdim,kdim , MRI_float ) ;
   bmask   = MRI_FLOAT_PTR(bim) ;
   bim->dx = fabsf(dx) ; bim->dy = fabsf(dy) ; bim->dz = fabsf(dz) ;

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

   fim = mri_fft_3Dconvolve( aim , bim ) ;

#if 0
   EDIT_clear_padval ;                               /* back to zeropad */
#endif

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
   npeel = (int)rintf( 0.123*brad/dxyz ) ;
   if( npeel <= 0   ) npeel = 1 ;
   THD_automask_set_peelcounts(npeel,17) ;
   THD_automask_set_clipfrac(0.333f) ;
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
