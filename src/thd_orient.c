#include "afni_warp.h"

int_triple THD_orient_guess( MRI_IMAGE *imm )
{
   int nvox , ii , nx,ny,nxy,nz , ix,jy,kz , icm,jcm,kcm ;
   byte *bar , bp,bm ;
   float xcm , ycm , zcm , ff , dx,dy,dz ;
   float xx,yy,zz ;
   int ni,nj,nk , itop,jtop,ktop , im,ip , jm,jp , km,kp ;
   float axx,ayy,azz , clip  , qx,qy,qz ;
   int d_LR , d_AP , d_IS ;

   int_triple it = {-1,-1,-1} ;

   /*-- check for bad input --*/

   if( imm == NULL || imm->nx < 5 || imm->ny < 5 || imm->nz < 5 ) return it ;

   nx = imm->nx; ny = imm->ny; nz = imm->nz; nxy = nx*ny; nvox = nx*ny*nz;

   dx = fabs(imm->dx) ; if( dx == 0.0 ) dx = 1.0 ;
   dy = fabs(imm->dy) ; if( dy == 0.0 ) dy = 1.0 ;
   dz = fabs(imm->dz) ; if( dz == 0.0 ) dz = 1.0 ;

   /*-- make binary mask --*/

   clip = THD_cliplevel( imm , 0.5 ) ;

   bar = (byte *) malloc( sizeof(byte) * nvox ) ;
   switch( imm->kind ){
     case MRI_float:{
       float *ar = MRI_FLOAT_PTR(imm) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (ar[ii] >= clip);
     }
     break ;
     case MRI_short:{
       short *ar = MRI_SHORT_PTR(imm) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (ar[ii] >= clip);
     }
     break ;
     case MRI_byte:{
       byte *ar = MRI_BYTE_PTR(imm) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (ar[ii] >= clip);
     }
     break ;
   }

   ii = THD_countmask(nvox,bar) ;
   if( ii == 0 ){ free(bar); return it; }

   THD_mask_clust( nx,ny,nz , bar ) ;
   THD_mask_fillin_once( nx,ny,nz , bar , 1 ) ;

   /* find center of mass of mask */

   xcm = ycm = zcm = 0.0 ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( bar[ii] ){
       ix = (ii % nx)      ; xx = ix*dx ; xcm += xx ;
       jy = (ii / nx) % ny ; yy = jy*dy ; ycm += yy ;
       kz = (ii /nxy)      ; zz = kz*dz ; zcm += zz ;
     }
   }
   ff = THD_countmask(nvox,bar) ;
   xcm /= ff ; ycm /= ff ; zcm /= ff ;

   icm = rint(xcm/dx) ;
   itop = 2*icm ; if( itop >= nx ) itop = nx-1 ;
   ni  = itop-icm ;

   jcm = rint(ycm/dy) ;
   jtop = 2*jcm ; if( jtop >= ny ) jtop = ny-1 ;
   nj  = jtop-jcm ;

   kcm = rint(zcm/dz) ;
   ktop = 2*kcm ; if( ktop >= nz ) ktop = nz-1 ;
   nk  = ktop-kcm ;

   printf("Mask count = %d\n"
          "icm = %d  jcm = %d  kcm = %d\n"
          "ni  = %d  nj  = %d  nk  = %d\n",
          (int)ff , icm,jcm,kcm , ni,nj,nk ) ;

   /* compute asymmetry measures about CM voxel */

#define BAR(i,j,k) bar[(i)+(j)*nx+(k)*nxy]

   axx = 0.0 ;
   for( ix=1 ; ix <= ni ; ix++ ){
     im = icm-ix ; ip = icm+ix ;
     for( kz=0 ; kz < nz ; kz++ ){
       for( jy=0 ; jy < ny ; jy++ )
         if( BAR(im,jy,kz) != BAR(ip,jy,kz) ) axx++ ;
     }
   }
   axx /= (itop*ny*nz) ; printf("axx = %g\n",axx) ;

   ayy = 0.0 ;
   for( jy=1 ; jy <= nj ; jy++ ){
     jm = jcm-jy ; jp = jcm+jy ;
     for( kz=0 ; kz < nz ; kz++ ){
       for( ix=0 ; ix < nx ; ix++ )
         if( BAR(ix,jm,kz) != BAR(ix,jp,kz) ) ayy++ ;
     }
   }
   ayy /= (jtop*nx*nz) ; printf("ayy = %g\n",ayy) ;

   azz = 0.0 ;
   for( kz=1 ; kz <= nk ; kz++ ){
     km = kcm-kz ; kp = kcm+kz ;
     for( jy=0 ; jy < ny ; jy++ ){
       for( ix=0 ; ix < nx ; ix++ )
         if( BAR(ix,jy,km) != BAR(ix,jy,kp) ) azz++ ;
     }
   }
   azz /= (ktop*nx*ny) ; printf("azz = %g\n",azz) ;

   if( axx < ayy ){
     if( axx < azz ) d_LR = 1 ;
     else            d_LR = 3 ;
   } else {
     if( ayy < azz ) d_LR = 2 ;
     else            d_LR = 3 ;
   }
   printf("axis %d is L-R\n",d_LR) ;

   qx = qy = qz = 0.0 ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( bar[ii] ){
       ix = (ii % nx)      ; xx = ix*dx-xcm ; qx += xx*xx ;
       jy = (ii / nx) % ny ; yy = jy*dy-ycm ; qy += yy*yy ;
       kz = (ii /nxy)      ; zz = kz*dz-zcm ; qz += zz*zz ;
     }
   }

   switch( d_LR ){
     case 1: if( qy < qz ) d_AP = 3 ;
             else          d_AP = 2 ;
     break ;
     case 2: if( qx < qz ) d_AP = 3 ;
             else          d_AP = 1 ;
     break ;
     case 3: if( qx < qy ) d_AP = 2 ;
             else          d_AP = 1 ;
     break ;
   }
   printf("axis %d is A-P\n",d_AP) ;

   return it ;
}

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   MRI_IMAGE *im ;

   dset = THD_open_dataset(argv[1]) ;
   if( dset == NULL ) exit(1) ;
   DSET_load(dset) ;
   im = DSET_BRICK(dset,0) ;
   im->dx = DSET_DX(dset) ;
   im->dy = DSET_DY(dset) ;
   im->dz = DSET_DZ(dset) ;
   (void) THD_orient_guess( im ) ;
   exit(0) ;
}
