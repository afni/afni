#include "mrilib.h"

/*-------------------------------------------------------------------*/
/* 3D sharpening filter, for program 3dSharpen3D.
   * Only operates on positive values, and only on those
     which have all 26 NN1, NN2, and NN3 points being positive
     as well (to avoid weird stuff at edges).
   * Uses a simple local difference approach, with NN1 & NN2.
   * im must be in floating point, and is edited in place
   * phi is between 0 and 1 (exclusive).
   ***** RWCox -- 13 Feb 2017 *****
*//*-----------------------------------------------------------------*/

#define RFAC 0.5f

void mri_sharpen3D_pos( MRI_IMAGE *im , float phi )
{
   float *imar , *tar ;
   float nphi , omphi , pval , psum , ppp , pbot,ptop , tbot,ttop ;
   int nx,ny,nz, ii,jj,kk, joff,koff,jkoff,ijk, nx1,ny1,nz1,nxyz , ndon;

   if( im == NULL || im->kind != MRI_float ) return ;
   if( phi <= 0.0f || phi >= 1.0f          ) return ;

   nx = im->nx ; ny = im->ny ; nz = im->nz ;
   if( nx < 3 || ny < 3 || nz < 3          ) return ;
   nx1 = nx-1 ; ny1 = ny-1 ; nz1 = nz-1 ;
   joff = nx ; koff = nx*ny ; nxyz = koff*nz ;

   imar = MRI_FLOAT_PTR(im) ;
   if( imar == NULL                        ) return ;

   nphi  = phi / 19.0f ;
   omphi = 1.0/(1.0-phi) ;

   tar = (float *)malloc(sizeof(float)*nxyz) ;
   memcpy( tar , imar ,  sizeof(float)*nxyz) ;

   tbot = 1.e+37 ; ttop = 0.0f ;
   for( ijk=0 ; ijk < nxyz ; ijk++ ){
     if( tar[ijk] > 0.0f ){
       tbot = MIN(tbot,tar[ijk]) ;
       ttop = MAX(ttop,tar[ijk]) ;
     }
   }
   if( tbot >= ttop ){ free(tar) ; return ; }

#define DOFF(qq)                \
 { ppp = tar[ijk+(qq)] ;        \
   if( ppp <= 0.0f ) continue ; \
   else { psum += ppp; pbot=MIN(pbot,ppp); ptop=MAX(ptop,ppp); } }

#define ZOFF(qq)                \
 { ppp = tar[ijk+(qq)] ; if( ppp <= 0.0f ) continue ; }

   ndon = 0 ;
   for( kk=1 ; kk < nz1 ; kk++ ){
    for( jj=1 ; jj < ny1 ; jj++ ){
     jkoff = jj*joff + kk*koff ;
     for( ii=1 ; ii < nx1 ; ii++ ){
       ijk  = ii+jkoff ;
       pbot = ptop = psum = pval = tar[ijk] ; if( pval <= 0.0f ) continue;
       DOFF(-1)         ; DOFF( 1)         ;  /* operate on the 18 */
       DOFF(-joff)      ; DOFF( joff)      ;  /* NN1 and NN2 points */
       DOFF(-koff)      ; DOFF( koff)      ;
       DOFF(-joff-koff) ; DOFF(-joff+koff) ;
       DOFF( joff-koff) ; DOFF( joff-koff) ;
       DOFF(-joff-1)    ; DOFF(-joff+1)    ;
       DOFF( joff-1)    ; DOFF( joff+1)    ;
       DOFF(-koff-1)    ; DOFF(-koff+1)    ;
       DOFF( koff-1)    ; DOFF( koff+1)    ;
       ZOFF( joff+koff+1); ZOFF( joff+koff-1); /* skip if any of the NN3 */
       ZOFF( joff-koff+1); ZOFF( joff-koff-1); /* points are non-positive */
       ZOFF(-joff+koff+1); ZOFF(-joff+koff-1);
       ZOFF(-joff-koff+1); ZOFF(-joff-koff-1);

       pbot -= RFAC*(pval-pbot) ; if( pbot < tbot ) pbot = tbot ;
       ptop += RFAC*(ptop-pval) ; if( ptop > ttop ) ptop = ttop ;
       pval = (pval - nphi*psum)*omphi ;
       if( pval < pbot ) pval = pbot ; else if( pval > ptop ) pval = ptop ;
       imar[ijk] = pval ; ndon++ ;
   }}}

   free(tar) ; return ;
}
