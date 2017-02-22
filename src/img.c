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

/* ININFO_message("mri_streakize") ; */

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

static float sigfac = 0.01f ;

MRI_IMAGE * mri_vgize( MRI_IMAGE *im )
{
   MRI_IMAGE *blim , *gxim,*gyim , *bxim,*byim ;
   float     *bar , *gxar,*gyar , *bxar,*byar ;
   int nx,ny,nxy , ii,jj,kk,joff ;
   float bsig , bmax , gsiz , blen , bx,by , slen , cc,ss ;

   if( im == NULL || im->kind != MRI_rgb ) return NULL ;

   nx = im->nx ; ny = im->ny ; nxy = nx*ny ;

   bsig = sqrtf(nx*(float)ny) * sigfac ;
   if( bsig < 1.5f ) bsig = 1.5f ;
/* INFO_message("mri_vgize: nx=%d ny=%d bsig=%.3f",nx,ny,bsig) ; */

   bxim = mri_to_float(im) ;
   blim = mri_float_blur2D( bsig , bxim ) ; mri_free(bxim) ;
   bar  = MRI_FLOAT_PTR(blim) ;

   gxim = mri_copy(blim) ; gxar = MRI_FLOAT_PTR(gxim) ;
   gyim = mri_copy(blim) ; gyar = MRI_FLOAT_PTR(gyim) ;

/* ININFO_message("compute gradients") ; */
   for( jj=0 ; jj < ny ; jj++ ){
    joff = jj*nx ;
    for( ii=0 ; ii < nx ; ii++ ){
      if( jj==0 || jj==ny-1 || ii==0 || ii==nx-1 ){
        gxar[ii+joff] = gyar[ii+joff] = 0.0f ;
      } else {
        gxar[ii+joff] = bar[ii+joff+1 ] - bar[ii+joff-1 ] ;
        gyar[ii+joff] = bar[ii+joff+nx] - bar[ii+joff-nx] ;
      }
   }}
/* ININFO_message("blur gradients") ; */
   bxim = mri_float_blur2D(0.5f*bsig,gxim); mri_free(gxim); bxar = MRI_FLOAT_PTR(bxim);
   byim = mri_float_blur2D(0.5f*bsig,gyim); mri_free(gyim); byar = MRI_FLOAT_PTR(byim);
/* ININFO_message("find gradient max") ; */
   bmax = 0.0f ;
   for( kk=0 ; kk < nxy ; kk++ ){
     gsiz = bxar[kk]*bxar[kk] + byar[kk]*byar[kk] ;
     if( gsiz > bmax ) bmax = gsiz ;
   }
   bmax = sqrtf(bmax) ;
/* ININFO_message("bmax=%g",bmax) ; */
   if( bmax == 0.0f ){ mri_free(bxim); mri_free(byim) ; return NULL ; }
   bmax = 1.0f / bmax ;
   slen = 1.3f * bsig ;
   for( kk=0 ; kk < nxy ; kk++ ){
     bx = bxar[kk]*bmax ; by = byar[kk]*bmax ; gsiz = sqrtf(bx*bx+by*by) ;
     if( gsiz < 0.03f && gsiz > 0.0f ){
       bx *= (0.111f*slen/gsiz) ; by *= (0.111f*slen/gsiz) ;
     } else if( gsiz < 0.30f ){
       bx *= (0.333f*slen/gsiz) ; by *= (0.333f*slen/gsiz) ;
     } else {
       bx *= (slen/gsiz) ; by *= (slen/gsiz) ;
     }
     bxar[kk] = by ; byar[kk] = -bx ;
     bar[kk]  = (float)(30.0*drand48()-15.0) ;
   }
/* ININFO_message("blur angles") ; */
   gxim = mri_float_blur2D(0.5f*bsig,blim); mri_free(blim); gxar = MRI_FLOAT_PTR(gxim);
   bmax = 0.0f ;
   for( kk=0 ; kk < nxy ; kk++ ){
     gsiz = fabsf(gxar[kk]) ; if( gsiz > bmax ) bmax = gsiz ;
   }
/* ININFO_message("max angle=%g",bmax) ; */
   if( bmax > 0.0f ){
     bmax = (40.0f * PI/180.0f) / bmax ;
     for( kk=0 ; kk < nxy ; kk++ ){
       bx = bxar[kk] ; by = byar[kk] ; if( bx==0.0f && by==0.0f ) continue ;
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

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   MRI_IMAGE *inim , *outim ;
   int iarg=1 ;
   char *fname="vgize.png" ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: img [-fac f] inputimage [prefix]\n"
       "\n"
       "* Takes a 2D image and applies a painting effect.\n"
       "* Output is in png format.\n"
       "* '-fac' lets you set the size of the painting effect.\n"
       "  Larger is weirder. The default is 1 = 1%% of image size.\n"
       "* This is just for fun.\n"
       "* Author: Vincent van Zhark (the lesser).\n"
     ) ;
     exit(0) ;
   }

   if( strcasecmp(argv[iarg],"-fac") == 0 ){
     float fac ;
     iarg++ ; if( iarg >= argc ) ERROR_exit("bad fac") ;
     fac = (float)strtod(argv[iarg],NULL) ;
     if( fac >= 1.0f ) fac *= 0.01f ;
     if( fac >  0.2f ) fac  = 0.2f  ;
     sigfac = fac ;
     iarg++ ;
   }

   inim = mri_read(argv[iarg]) ;
   if( inim == NULL || inim->nz > 1 ) ERROR_exit("bad input input") ;

   if( inim->kind != MRI_rgb ){
     MRI_IMAGE *qim = mri_to_rgb(inim) ;
     mri_free(inim) ; inim = qim ;
   }

   outim = mri_vgize(inim) ;

   if( outim == NULL ) ERROR_exit("bad vgize operation") ;

   if( ++iarg < argc ){
     fname = (char *)malloc(sizeof(char)*(strlen(argv[iarg])+16)) ;
     strcpy(fname,argv[iarg]) ;
     if( strcasestr(fname,".png") == NULL ) strcat(fname,".png") ;
   }

   mri_write_png( fname , outim ) ;
   INFO_message("output %s",fname) ;
   exit(0) ;
}
