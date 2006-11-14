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
   fxar = (fx != NULL) ? MRI_FLOAT_PTR(fx) : NULL ;
   fyar = (fy != NULL) ? MRI_FLOAT_PTR(fy) : NULL ;
   fzar = (fz != NULL) ? MRI_FLOAT_PTR(fz) : NULL ;
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
