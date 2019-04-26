#include "mrilib.h"

float mri_spatial_concentration( MRI_IMAGE *im )
{
   float xcen, ycen, zcen , wsum,ww , gval,hval , dxyz  ;
   float *imar ;
   int nx,ny,nz,nxy , nvox , ii,jj,kk,qq , nww ;

   if( im == NULL ) return 0.0f ;
   imar = MRI_FLOAT_PTR(im) ; if( imar == NULL ) return 0.0f ;
   nx = im->nx ; ny = im->ny ; nz = im->nz ; nvox = nx*ny*nz ; nxy = nx*ny ;

   xcen = ycen = zcen = wsum = 0.0f ;
   for( nww=qq=0 ; qq < nvox ; qq++ ){
     ww = fabsf(imar[qq]) ; if( ww == 0.0f ) continue ;
     ii = qq % nx ; kk = qq / nxy ; jj = (qq%nxy)/nx ;
     wsum += ww ; xcen += ww*ii ; ycen += ww*jj ; zcen += ww*kk ; nww++ ;
   }
   if( wsum <= 0.0f || nww < 2 ) return 0.0f ;
   xcen /= wsum ; ycen /= wsum ; zcen /= wsum ;

   gval = hval = 0.0f ;
   for( qq=0 ; qq < nvox ; qq++ ){
     ww = fabsf(imar[qq]) ; if( ww == 0.0f ) continue ;
     ii = qq % nx ; kk = qq / nxy ; jj = (qq%nxy)/nx ;
#if 1
     dxyz = (xcen-ii)*(xcen-ii) + (ycen-jj)*(ycen-jj) + (zcen-kk)*(zcen-kk) ;
#else
     dxyz = fabsf(xcen-ii)+fabsf(ycen-jj)+fabsf(zcen-kk) ;
#endif
     gval += ww*dxyz ; hval += dxyz ;
   }
   gval /= wsum ; hval /= nww ;

   return (hval/gval) ;
}
