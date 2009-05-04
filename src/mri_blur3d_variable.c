#include "mrilib.h"

#undef  INMASK
#define INMASK(i) (mask == NULL || mask[i] != 0)

/*----------------------------------------------------------------------------*/
/*! Blur image, in place, confined to a mask, with blurring factors given
    separately for each dimension xyz.  A NULL blurring factor means don't
    blur in that direction.  Blurring factors can be thought of in 2 ways
      - diffusion equation: fx = 0.5 * Delta_t * D_x / Delta_x**2
      - FWHM blurring:      fx = (Delta_FWHMx / Delta_x)**2 * 0.045084
      - The fx (etc.) factors should be between 0 and 0.05 for stability;
        for increasing the FWHM of the image, this means that the maximum
        change in FWHM is about Delta_x in each step.
      - Not all input fx,fy,fz factors should be NULL (or nothing will happen).
      - Method: 3 point stencil (in each direction) conservative finite
        difference approximation to du/dt = d/dx[ D_x(x,y,z) du/dx ] + ...
      - Points not in the mask are not processed, and will be set to zero
        in the output.
      - The input mask can be NULL.  If you really want speed, a special
        version should be written for the mask=NULL case.  Please send money.
      - Author: Zhark, Emperor of the Galaxy!!!  (Nov 2006)
------------------------------------------------------------------------------*/

void mri_blur3D_variable( MRI_IMAGE *im , byte *mask ,
                          MRI_IMAGE *fx , MRI_IMAGE *fy , MRI_IMAGE *fz )
{
   int nx,ny,nz,nxy,nxyz ;
   float *iar , *fxar , *fyar , *fzar , *qar ;
   int ijk , ii,jj,kk ;
   float vcc , vsub , vout , vf ;

ENTRY("mri_blur3D_variable") ;

   if( im == NULL                             ) EXRETURN ;
   if( fx == NULL && fy == NULL && fz == NULL ) EXRETURN ;

   nx = im->nx; ny = im->ny; nz = im->nz; nxy = nx*ny; nxyz = nxy*nz;

   iar  = MRI_FLOAT_PTR(im) ;
   fxar = MRI_FLOAT_PTR(fx) ;
   fyar = MRI_FLOAT_PTR(fy) ;
   fzar = MRI_FLOAT_PTR(fz) ;
   qar  = (float *)calloc(sizeof(float),nxyz) ;

   for( ijk=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,ijk++ ){
       if( !INMASK(ijk) ) continue ;
       vout = vcc = iar[ijk] ;
       if( fxar != NULL ){    /* distribute (diffuse) in the x-direction */
         vf = fxar[ijk] ;
         if( ii-1 >= 0 && INMASK(ijk-1) ){
           vsub = (fxar[ijk-1]+vf)*vcc; qar[ijk-1] += vsub; vout -= vsub;
         }
         if( ii+1 < nx && INMASK(ijk+1) ){
           vsub = (fxar[ijk+1]+vf)*vcc; qar[ijk+1] += vsub; vout -= vsub;
         }
       }
       if( fyar != NULL ){    /* distribute (diffuse) in the y-direction */
         vf = fyar[ijk] ;
         if( jj-1 >= 0 && INMASK(ijk-nx) ){
           vsub = (fyar[ijk-nx]+vf)*vcc; qar[ijk-nx] += vsub; vout -= vsub;
         }
         if( jj+1 < ny && INMASK(ijk+nx) ){
           vsub = (fyar[ijk+nx]+vf)*vcc; qar[ijk+nx] += vsub; vout -= vsub;
         }
       }
       if( fzar != NULL ){    /* distribute (diffuse) in the z-direction */
         vf = fzar[ijk] ;
         if( kk-1 >= 0 && INMASK(ijk-nxy) ){
           vsub = (fzar[ijk-nxy]+vf)*vcc; qar[ijk-nxy] += vsub; vout -= vsub;
         }
         if( kk+1 < nz && INMASK(ijk+nxy) ){
           vsub = (fzar[ijk+nxy]+vf)*vcc; qar[ijk+nxy] += vsub; vout -= vsub;
         }
       }

       qar[ijk] += vout ;  /* whatever wasn't diffused away from this voxel */
   }}}

   memcpy(iar,qar,sizeof(float)*nxyz) ; free((void *)qar) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! Similar to the above, but with fixed blurring factors in each direction.
    But inside a mask.  Again, 0 <= fx <= 0.05 for stability.  [01 May 2009]
*//*--------------------------------------------------------------------------*/

void mri_blur3D_inmask( MRI_IMAGE *im, byte *mask,
                        float fx,float fy,float fz, int nrep )
{
   int nx,ny,nz,nxy,nxyz ;
   float *iar , *qar ;
   int ijk , ii,jj,kk , nn ;
   register float vcc , vsub , vout , vx,vy,vz ;

ENTRY("mri_blur3D_inmask") ;

   if( im == NULL || nrep <= 0 ) EXRETURN ;

   nx = im->nx; ny = im->ny; nz = im->nz; nxy = nx*ny; nxyz = nxy*nz;

   iar = MRI_FLOAT_PTR(im) ;
   vx  = 2.0f * fx ; if( nx < 2 ) vx = 0.0f ;
   vy  = 2.0f * fy ; if( ny < 2 ) vy = 0.0f ;
   vz  = 2.0f * fz ; if( nz < 2 ) vz = 0.0f ;
   if( vx <= 0.0f && vy <= 0.0f && vz <= 0.0f ) EXRETURN ;

   qar = (float *)calloc(sizeof(float),nxyz) ;

   for( nn=0 ; nn < nrep ; nn++ ){
     for( ijk=kk=0 ; kk < nz ; kk++ ){
      for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++,ijk++ ){
         if( !INMASK(ijk) ) continue ;
         vout = vcc = iar[ijk] ;
         if( vx > 0.0f ){    /* distribute (diffuse) in the x-direction */
           if( ii-1 >= 0 && INMASK(ijk-1) ){
             vsub = vx*vcc; qar[ijk-1] += vsub; vout -= vsub;
           }
           if( ii+1 < nx && INMASK(ijk+1) ){
             vsub = vx*vcc; qar[ijk+1] += vsub; vout -= vsub;
           }
         }
         if( vy > 0.0f ){    /* distribute (diffuse) in the y-direction */
           if( jj-1 >= 0 && INMASK(ijk-nx) ){
             vsub = vy*vcc; qar[ijk-nx] += vsub; vout -= vsub;
           }
           if( jj+1 < ny && INMASK(ijk+nx) ){
             vsub = vy*vcc; qar[ijk+nx] += vsub; vout -= vsub;
           }
         }
         if( vz >= 0.0f ){    /* distribute (diffuse) in the z-direction */
           if( kk-1 >= 0 && INMASK(ijk-nxy) ){
             vsub = vz*vcc; qar[ijk-nxy] += vsub; vout -= vsub;
           }
           if( kk+1 < nz && INMASK(ijk+nxy) ){
             vsub = vz*vcc; qar[ijk+nxy] += vsub; vout -= vsub;
           }
         }

         qar[ijk] += vout ;  /* whatever wasn't diffused away from this voxel */
     }}}
     memcpy(iar,qar,sizeof(float)*nxyz) ;
     if( nn != nrep-1 ) memset(qar,0,sizeof(float)*nxyz) ;
   }

   free((void *)qar) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

#undef  DMAX
#define DMAX 999999.9f

void mri_blur3D_getfac( float fwhm , float dx, float dy, float dz ,
                        int *nrep , float *fx , float *fy , float *fz )
{
   float dm = DMAX ;

   if( fwhm <= 0.0f ) return ;

   if( dx > 0.0f ) dm = dx ;
   if( dy > 0.0f ) dm = MIN(dm,dy) ;
   if( dz > 0.0f ) dm = MIN(dm,dz) ;
   if( dm == DMAX ) return ;

   *nrep = 2 + (int)(fwhm*fwhm/(dm*dm)) ;
   if( dx > 0.0f ) *fx = 0.045084f*(fwhm*fwhm)/(*nrep*dx*dx) ;
   else            *fx = 0.0f ;
   if( dy > 0.0f ) *fy = 0.045084f*(fwhm*fwhm)/(*nrep*dy*dy) ;
   else            *fy = 0.0f ;
   if( dz > 0.0f ) *fz = 0.045084f*(fwhm*fwhm)/(*nrep*dz*dz) ;
   else            *fz = 0.0f ;

   return ;
}

/*----------------------------------------------------------------------------*/

void mri_blur3D_addfwhm( MRI_IMAGE *im , byte *mask , float fwhm )
{
   int nrep=-1 ;
   float fx=-1.0f,fy=-1.0f,fz=-1.0f , dx,dy,dz ;

ENTRY("mri_blur3D_addfwhm") ;

   if( im == NULL || fwhm <= 0.0f ) EXRETURN ;

   dx = im->dx; if( dx == 0.0f ) dx = 1.0f; else if( dx < 0.0f ) dx = -dx;
   dy = im->dy; if( dy == 0.0f ) dy = 1.0f; else if( dy < 0.0f ) dy = -dy;
   dz = im->dz; if( dz == 0.0f ) dz = 1.0f; else if( dz < 0.0f ) dz = -dz;

   mri_blur3D_getfac( fwhm , dx,dy,dz , &nrep,&fx,&fy,&fz ) ;
   if( nrep < 0 || fx < 0.0f || fy < 0.0f || fz < 0.0f ) EXRETURN ;

   if( MRILIB_verb )
     INFO_message("mri_blur3D: #iter=%d fx=%.5f fy=%.5f fz=%.5f",nrep,fx,fy,fz) ;

   mri_blur3D_inmask( im , mask , fx,fy,fz , nrep ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! Blur a vectim, by converting each time point to a 3D image,
    blurring that image, and then putting it back into the vectim.
*//*--------------------------------------------------------------------------*/

void mri_blur3D_vectim( MRI_vectim *vim , float fwhm )
{
   int nrep=-1 , nx,ny,nz , nvox , iv,kk ;
   float fx=-1.0f,fy=-1.0f,fz=-1.0f , dx,dy,dz ;
   MRI_IMAGE *qim ; float *qar , *var ; int *ivar ; byte *mmm ;

ENTRY("mri_blur3d_vectim") ;

   if( vim == NULL || fwhm <= 0.0f ) EXRETURN ;

   dx = vim->dx ; if( dx == 0.0f ) dx = 1.0f; else if( dx < 0.0f ) dx = -dx;
   dy = vim->dy ; if( dy == 0.0f ) dy = 1.0f; else if( dy < 0.0f ) dy = -dy;
   dz = vim->dz ; if( dz == 0.0f ) dz = 1.0f; else if( dz < 0.0f ) dz = -dz;

   nx = vim->nx ; ny = vim->ny ; nz = vim->nz ; nvox = nx*ny*nz ;
   if( nx < 1 || ny < 1 || nz < 1 ) EXRETURN ;

   mri_blur3D_getfac( fwhm , dx,dy,dz , &nrep,&fx,&fy,&fz ) ;
   if( nrep < 0 || fx < 0.0f || fy < 0.0f || fz < 0.0f ) EXRETURN ;

   if( MRILIB_verb )
     INFO_message("mri_blur3D: #iter=%d fx=%.5f fy=%.5f fz=%.5f",nrep,fx,fy,fz) ;

   ivar = vim->ivec ;
   mmm  = (byte *)calloc(sizeof(byte),nvox) ;
   for( kk=0 ; kk < vim->nvec ; kk++ ) mmm[ivar[kk]] = 1 ;

   qim = mri_new_vol( nx,ny,nz , MRI_float ) ;
   qar = MRI_FLOAT_PTR(qim) ;

   for( iv=0 ; iv < vim->nvals ; iv++ ){
     memset( qar , 0 , sizeof(float)*nvox ) ;
     for( kk=0 ; kk < vim->nvec ; kk++ ){
       var = VECTIM_PTR(vim,kk) ; qar[ivar[kk]] = var[iv] ;
     }
     mri_blur3D_inmask( qim , mmm , fx,fy,fz , nrep ) ;
     for( kk=0 ; kk < vim->nvec ; kk++ ){
       var = VECTIM_PTR(vim,kk) ; var[iv] = qar[ivar[kk]] ;
     }
   }

   free(mmm) ; mri_free(qim) ; EXRETURN ;
}
