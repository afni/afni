#include "mrilib.h"

/** not 7D SAFE **/

/*--------------------------------------------------------------------
  12 Nov 2001:
  Get the center of mass of a 2D image;
  store it in the user-supplied locations *xcm, *ycm.
  Note that *xcm and *ycm are the INDEX center of mass coordinates.
----------------------------------------------------------------------*/

void mri_get_cmass_2D( MRI_IMAGE *im , float *xcm , float *ycm )
{
   int ii,jj,joff , nx,ny ;
   double xx , yy , sum , val ; float *far ;
   MRI_IMAGE *flim ;

ENTRY("mri_get_cmass_2D") ;

   if( im == NULL || xcm == NULL || ycm == NULL ) EXRETURN ;

   if( im->kind != MRI_float ) flim = mri_to_float( im ) ;
   else                        flim = im ;

   far = MRI_FLOAT_PTR(flim) ;
   nx  = im->nx ; ny = im->ny ;

   sum = xx = yy = 0.0 ;
   for( jj=0 ; jj < ny ; jj++ ){
      joff = jj * nx ;
      for( ii=0 ; ii < nx ; ii++ ){
         val = fabs(far[ii+joff]) ;
         sum += val ;
         xx  += val * ii ;
         yy  += val * jj ;
      }
   }
   if( flim != im ) mri_free(flim) ;

   if( sum > 0.0 ){ xx /= sum ; yy /= sum ; }
   else           { xx = 0.5*(nx-1); yy=0.5*(ny-1); }

   *xcm = (float)xx ; *ycm = (float)yy ; EXRETURN ;
}

/*--------------------------------------------------------------------*/
/*! Get the center of mass of a 3D image (in index coordinates). */

void mri_get_cmass_3D( MRI_IMAGE *im, float *xcm, float *ycm, float *zcm )
{
   int ii,jj,kk,koff,joff , nx,ny,nz,nxy ;
   double xx,yy,zz , sum , val ; float *far ;
   MRI_IMAGE *flim ;

ENTRY("mri_get_cmass_3D") ;

   if( im == NULL || xcm == NULL || ycm == NULL || zcm == NULL ) EXRETURN ;

   if( im->kind != MRI_float ) flim = mri_to_float( im ) ;
   else                        flim = im ;

   far = MRI_FLOAT_PTR(flim) ;
   nx  = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ;

   sum = xx = yy = zz = 0.0 ;
   for( kk=0 ; kk < nz ; kk++ ){
     koff = kk * nxy ;
     for( jj=0 ; jj < ny ; jj++ ){
       joff = koff + jj * nx ;
       for( ii=0 ; ii < nx ; ii++ ){
         val = fabs(far[ii+joff]) ;
         sum += val ;
         xx  += val * ii ;
         yy  += val * jj ;
         zz  += val * kk ;
       }
     }
   }
   if( flim != im ) mri_free(flim) ;

   if( sum > 0.0 ){ xx /= sum ; yy /= sum ; zz /= sum ; }
   else           { xx = 0.5*(nx-1); yy=0.5*(ny-1); zz=0.5*(nz-1); }

   *xcm = (float)xx ; *ycm = (float)yy ; *zcm = (float)zz ; EXRETURN ;
}
