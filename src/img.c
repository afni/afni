#include "mrilib.h"

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_streakize( MRI_IMAGE *im , MRI_IMAGE *sxim , MRI_IMAGE *syim )
{
   MRI_IMAGE *qim ; byte *qar , *iar ;
   float *sxar , *syar ;
   int nx,ny,nxy , kk,dk , ii,jj,sk, dd,di,dj , ei,ej , ns ;
   float strk , sx,sy , rr,gg,bb ;

   nx = im->nx ; ny = im->ny ; nxy = nx*ny ;

   qim = mri_copy(im) ; qar = MRI_RGB_PTR(qim) ; iar = MRI_RGB_PTR(im) ;
   sxar = MRI_FLOAT_PTR(sxim) ; syar = MRI_FLOAT_PTR(syim) ;

   for( kk=0 ; kk < nxy ; kk++ ){
     sx = sxar[kk] ; sy = syar[kk] ; if( sx == 0.0f && sy == 0.0f ) continue ;
     strk = sqrtf(sx*sx+sy*sy) ;     if( strk < 2.0f              ) continue ;
     sx /= strk ; sy /= strk ;       if( strk > 20.0f ) strk = 20.0f ;
     sk = (int)(strk+0.499f) ;
     rr = iar[3*kk+0] ; gg = iar[3*kk+1] ; bb = iar[3*kk+2] ; ns = 1 ;
     ii = kk % nx ; jj = kk / nx ;
     for( dd=1 ; dd <= sk ; dd++ ){
       di = (int)(dd*sx+0.499f) ; dj = (int)(dd*sy+0.499f) ;
       if( di == 0.0f && dj == 0.0f ) continue ;
       ei = ii+di ; ej = jj+dj ;
       if( ei >= 0 && ei < nx && ej >= 0 && ej < ny ){
         dk = ei + ej*nx ;
         rr += iar[3*dk+0] ; gg += iar[3*dk+1] ; bb += iar[3*dk+2] ; ns++ ;
       }
       ei = ii-di ; ej = jj-dj ;
       if( ei >= 0 && ei < nx && ej >= 0 && ej < ny ){
         dk = ei + ej*nx ;
         rr += iar[3*dk+0] ; gg += iar[3*dk+1] ; bb += iar[3*dk+2] ; ns++ ;
       }
     }
     if( ns > 1 ){
       rr /= ns ; gg /= ns ; bb /= ns ;
       qar[3*kk+0] = BYTEIZE(rr) ; qar[3*kk+1] = BYTEIZE(gg) ; qar[3*kk+2] = BYTEIZE(bb) ;
     }
   }

   return qim ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_vgize( MRI_IMAGE *im )
{
   MRI_IMAGE *blim , *gxim,*gyim , *bxim,*byim ;
   float     *bar , *gxar,*gyar , *bxar,*byar ;
   int nx,ny,nxy , ii,jj,kk,joff ;
   float bsig , bmax , gsiz , blen , bx,by , slen , cc,ss ;

   if( im == NULL || im->kind != MRI_rgb ) return NULL ;

   nx = im->nx ; ny = im->ny ; nxy = nx*ny ;

   bsig = sqrtf(nx*(float)ny) * 0.01f ;

   blim = mri_float_blur2D( bsig , im ) ;
   bar  = MRI_FLOAT_PTR(blim) ;

   gxim = mri_copy(blim) ; gxar = MRI_FLOAT_PTR(gxim) ;
   gyim = mri_copy(blim) ; gyar = MRI_FLOAT_PTR(gyim) ;

   for( jj=0 ; jj < ny ; jj++ ){
    joff = jj*nx ;
    for( ii=0 ; ii < nx ; ii++ ){
      if( jj==0 || jj==ny-1 || ii==0 || ii==nx-1 ){
        gxar[ii+joff] = gyar[ii+joff] = 0.0f ;
        continue ;
      }
      gxar[ii+joff] = bar[ii+joff+1 ] - bar[ii+joff-1 ] ;
      gyar[ii+joff] = bar[ii+joff+nx] - bar[ii+joff-nx] ; }} 
   bxim = mri_float_blur2D(0.5f*bsig,gxim); mri_free(gxim); bxar = MRI_FLOAT_PTR(bxim);
   byim = mri_float_blur2D(0.5f*bsig,gyim); mri_free(gyim); byar = MRI_FLOAT_PTR(byim);

   bmax = 0.0f ;
   for( kk=0 ; kk < nxy ; kk++ ){
     gsiz = bxar[kk]*bxar[kk] + byar[kk]*byar[kk] ;
     if( gsiz > bmax ) bmax = gsiz ;
   }
   bmax = sqrtf(bmax) ;
   if( bmax == 0.0f ){ mri_free(bxim); mri_free(byim) ; return NULL ; }
   bmax = 1.0f / bmax ;
   slen = 0.333f*bsig ;
   for( kk=0 ; kk < nxy ; kk++ ){
     bx = bxar[kk]*bmax ; by = byar[kk]*bmax ; gsiz = sqrtf(bx*bx+by*by) ;
     if( gsiz < 0.1f ){
       bx = by = 0.0f ;
     } else {
       bx *= slen ; by *= slen ;
     }
     bxar[kk] = by ; byar[kk] = -bx ;
     bar[kk]  = (float)(20.0*drand48()-10.0) ;
   }
   gxim = mri_float_blur2D(0.5f*bsig,blim); mri_free(blim); gxar = MRI_FLOAT_PTR(gxim);
   bmax = 0.0f ;
   for( kk=0 ; kk < nxy ; kk++ ){
     gsiz = fabsf(gxar[kk]) ; if( gsiz > bmax ) bmax = gsiz ;
   }
   if( bmax > 0.0f ){
     bmax = (10.0f * PI/180.0f) / bmax ;
     for( kk=0 ; kk < nxy ; kk++ ){
       bx = bxar[kk] ; by = byar[kk] ;
       cc = cosf(bmax*gxar[kk]) ;
       ss = sinf(bmax*gxar[kk]) ;
       bxar[kk] =  cc*bx + ss*by ;
       byar[kk] = -ss*bx + cc*by ;
     }
   }
   mri_free(gxim) ;

   blim = mri_streakize( im , bxim , byim ) ;

   mri_free(bxim) ; mri_free(byim) ;

   return blim ;
}
