#include "mrilib.h"

/* im must be in floating point */
/* phi is a factor between 0.1 and 0.9 */

void mri_sharpen3D( MRI_IMAGE *im , float phi )
{
   float *imar , *tar ;
   float nphi , omphi , pval , psum , ppp , pbot,ptop ;
   int nx,ny,nz , ii,jj,kk , joff,koff,jkoff,ijk , nx1,ny1,nz1 ;

   if( im == NULL || im->kind != MRI_float ) return ;

   nx = im->nx ; ny = im->ny ; nz = im->nz ;
   if( nx < 3 || ny < 3 || nz < 3          ) return ;
   nx1 = nx-1 ; ny1 = ny-1 ; nz1 = nz-1 ;
   joff = nx ; koff = nx*ny ;

   imar = MRI_FLOAT_PTR(im) ;
   if( imar == NULL                        ) return ;

        if( phi < 0.1f ) phi = 0.1f ;
   else if( phi > 0.9f ) phi = 0.9f ;

   nphi  = phi / 19.0f ;
   omphi = 1.0/(1.0-phi) ;

   tar = (float *)malloc(sizeof(float)*nx*ny*nz) ;
   memcpy( tar , imar , sizeof(float)*nx*ny*nz ) ;

   for( kk=1 ; kk < nz1 ; kk++ ){
    for( jj=1 ; jj < ny1 ; jj++ ){
     jkoff = jj*joff + kk*koff ;
     for( ii=1 ; ii < nx1 ; ii++ ){
       ijk  = ii+jkoff ;
       pbot = ptop = psum = pval = tar[ijk] ; if( pval==0.0f ) continue;
       ppp  = tar[ijk-1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk+1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk-joff] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk+joff] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk-koff] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk+koff] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk-joff-koff] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk-joff+koff] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk+joff-koff] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk+joff+koff] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk-joff+1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk+joff+1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk-koff+1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk+koff+1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk-joff-1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk+joff-1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk-koff-1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }
       ppp  = tar[ijk+koff-1] ;
          if( ppp ==0.0f ) continue;
          else { psum += pval; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); }

       pbot = 2.0f*pbot-pval ;
       ptop = 2.0f*ptop-pval ;
       pval = (pval - phi*psum)*omphi ;
       if( pval < pbot ) pval = pbot ; else if( pval > ptop ) pval = ptop ;
       imar[ijk] = pval ;
   }}}

   free(tar) ; return ;
}
