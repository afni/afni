#include "mrilib.h"

MRI_IMAGE * mri_flip3D( int outx, int outy, int outz, MRI_IMAGE *inim )
{
   MRI_IMAGE *outim ;
   int ii,jj,kk , dsiz , nxin,nyin,nzin , nxout,nyout,nzout ;
   int nxyin , nxyout ;
   int ax,bx,cx,dx , ay,by,cy,dy , az,bz,cz,dz ;
   void *inar , *outar ;

   ii = abs(outx) ; jj = abs(outy) ; kk = abs(outz) ;
   if( outim == NULL || outx == 0 || outy == 0 || outz == 0 ) return NULL ;
   if( ii > 3 || jj > 3 || kk > )                             return NULL ;
   if( ii == jj || ii == kk || jj == kk )                     return NULL ;
   if( ii+jj+kk != 6 )                                        return NULL ;

   nxin = imin->nx ;
   nyin = imin->ny ; nxyin = nxin*nyin ;
   nzin = imin->nz ;

   /* setup so that i_out = ax + bx*i_in + cx*j_in + dx*k_in */

   switch( outx ){
     case  1:  ax = 0      ; bx =  1 ; cx =  0 ; dx =  0 ; nxout = nx ; break ;
     case -1:  ax = nxin-1 ; bx = -1 ; cx =  0 ; dx =  0 ; nxout = nx ; break ;
     case  2:  ax = 0      ; bx =  0 ; cx =  1 ; dx =  0 ; nxout = ny ; break ;
     case -2:  ax = nyin-1 ; bx =  0 ; cx = -1 ; dx =  0 ; nxout = ny ; break ;
     case  3:  ax = 0      ; bx =  0 ; cx =  0 ; dx =  1 ; nxout = nz ; break ;
     case -3:  ax = nzin-1 ; bx =  0 ; cx =  0 ; dx = -1 ; nxout = nz ; break ;
   }

   inar  = mri_data_pointer( inim ) ; if( inar == NULL )       return NULL ;
   outim = mri_new_vol( nxout,nyout,nzout , inim->kind ) ;
   outar = mri_data_pointer( outim ) ;
   dsiz  = outim->pixel_size ;
}
