#include "afni_warp.h"

int_triple THD_orient_guess( MRI_IMAGE *im )
{
   int nvox , ii , nx,ny,nxy,nz , ix,jy,kz , icm,jcm,kcm ;
   byte *bar , *sar ;
   float xcm , ycm , zcm , ff , dx,dy,dz ;
   float qxx,qyy,qzz , xx,yy,zz ;
   int d_ap , d_lr , d_is , n1,n2,n3 , n12 ;
   float sym_1 , sym_2 ;
   int c1,c2 , c1top,c2top , nc1,nc2 , p,q;

   int_triple it = {-1,-1,-1} ;

   /*-- check for bad input --*/

   if( im == NULL || im->nx < 3 || im->ny < 3 || im->nz < 3 ) return it ;

   nx = im->nx; ny = im->ny; nz = im->nz; nxy = nx*ny; nvox = nx*ny*nz;

   dx = fabs(im->dx) ; if( dx == 0.0 ) dx = 1.0 ;
   dy = fabs(im->dy) ; if( dy == 0.0 ) dy = 1.0 ;
   dz = fabs(im->dz) ; if( dz == 0.0 ) dz = 1.0 ;

   /*-- make binary mask --*/

   clip = THD_cliplevel( im , 0.5 ) ;

   bar = (byte *) malloc( sizeof(byte) * nvox ) ;
   switch( im->kind ){
     case MRI_float:{
       float *ar = MRI_FLOAT_PTR(im) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (ar[ii] >= clip);
     }
     break ;
     case MRI_short:{
       short *ar = MRI_SHORT_PTR(im) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (ar[ii] >= clip);
     }
     break ;
     case MRI_byte:{
       byte *ar = MRI_BYTE_PTR(im) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (ar[ii] >= clip);
     }
     break ;
   }

   ii = THD_countmask(nvox,bar) ; if( ii == 0 ){ free(bar); return it; }

   THD_max_clust( nx,ny,nz , bar ) ;
   THD_mask_fillin_once( nx,ny,nz , bar , 1 ) ;

   ff = THD_countmask(nvox,bar) ;

   /* find center of mass of mask */

   xcm = ycm = zcm = 0.0 ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( bar[ii] ){
       ix = (ii % nx)      ; xx = ix*dx ; xcm += xx ;
       jy = (ii / nx) % ny ; yy = jy*dy ; ycm += yy ;
       kz = (ii /nxy)      ; zz = kz*dz ; zcm += zz ;
     }
   }
   xcm /= ff ; ycm /= ff ; zcm /= ff ;
   icm = rint(xcm/dx) ; jcm = rint(ycm/dy) ; kcm = rint(zcm/dz) ;

   qxx = qyy = qzz = 0.0 ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( bar[ii] ){
       ix = (ii % nx)      ; xx = ix*dx - xcm ;
       jy = (ii / nx) % ny ; yy = jy*dy - ycm ;
       kz = (ii /nxy)      ; zz = kz*dz - zcm ;
       qxx += xx*xx ; qyy += yy*yy ; qzz += zz*zz ;
     }
   }
   qxx /= ff ; qyy /= ff ; qzz /= ff ;

   /* A-P direction is one with q.. largest */

   if( qxx > qyy ){
     if( qxx > qzz ) d_ap = 1 ;
     else            d_ap = 3 ;
   } else {
     if( qyy > qzz ) d_ap = 2 ;
     else            d_ap = 3 ;
   }

   switch( d_ap ){
     case 1: n1 = ny; n2 = nz; n3 = nx ; c1 = jcm; c2 = kcm; break;
     case 2: n1 = nz; n2 = nx; n3 = ny ; c1 = kcm; c2 = icm; break;
     case 3: n1 = nx; n2 = ny; n3 = nz ; c1 = icm; c2 = jcm; break;
   }
   n12 = n1*n2 ;
   c1top = 2*c1 ; if( c1top >= n1 ) c1top = n1-1 ;
   c2top = 2*c2 ; if( c2top >= n2 ) c2top = n2-1 ;
   nc1 = c1top-c1 ; nc2 = c2top-c2 ;

   sar = (byte *) malloc(sizeof(byte)*n1*n2) ;

   sym_1 = sym_2 = 0 ;
   for( kz=0 ; kz < n3 ; kz++ ){
     AFNI_br2sl_byte( nx,ny,nz , d_ap , kz , bar , sar ) ;
     ix = THD_countmask( n12 , sar ) ;
     if( ix < 9 ) continue ;

     for( q=1 ; q <= nc2 ; q++ ){
       jp = c2+q ; jm = c2-q ;
       for( p=1 ; p <= nc1 ; p++ ){
         ip = c1+p ; im = c1-p ;
       }
     }
   }
}
