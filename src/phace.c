#include "mrilib.h"

/*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_fft2D( MRI_IMAGE * im , int mode )
{
   MRI_IMAGE * cxim , * outim ;
   int nx,ny , nxup,nyup , ii,jj ;
   complex * cxar , * outar , * cpt , * opt ;

   if( im == NULL ) return NULL ;

   /* convert input to complex */

   cxim = mri_to_complex(im) ;
   cxar = MRI_COMPLEX_PTR(cxim) ;

   /* compute size of output */

   nx = cxim->nx ; nxup = csfft_nextup_one35(nx) ;
   ny = cxim->ny ; nyup = csfft_nextup_one35(ny) ;

   /* create output array */

   outim = mri_new( nxup , nyup , MRI_complex ) ;
   outar = MRI_COMPLEX_PTR(outim) ;

   /* copy input to output, zero padding along the way */

   opt = outar ;
   cpt = cxar  ;
   for( jj=0 ; jj < ny ; jj++ ){
      for( ii=0 ; ii < nx   ; ii++ ){opt->r=cpt->r; opt->i=cpt->i; opt++; cpt++;}
      for(      ; ii < nxup ; ii++ ){opt->r=opt->i=0.0; opt++;}
   }
   for( ; jj < nyup ; jj++ ){opt->r=opt->i=0.0; opt++;}

   mri_free(cxim) ;

   /* row FFTs */

   for( jj=0 ; jj < ny ; jj++ )
      csfft_cox( mode , nxup , outar+jj*nxup ) ;

   /* column FFTs */

   cxar = (complex *) malloc(sizeof(complex)*nyup) ;

   for( ii=0 ; ii < nxup ; ii++ ){
      for( jj=0 ; jj < nyup ; jj++ ) cxar[jj] = outar[ii+jj*nxup] ;
      csfft_cox( mode , nyup , cxar ) ;
      for( jj=0 ; jj < nyup ; jj++ ) outar[ii+jj*nxup] = cxar[jj] ;
   }

   free(cxar) ; return outim ;
}

/*--------------------------------------------------------------------------*/

MRI_IMAGE * cx_scramble( MRI_IMAGE * ima , MRI_IMAGE * imb ,
                         float alpha     , float beta       )
{
   int ii , npix ;
   double r1,r2 , t1,t2 , rr,tt ;
   complex * ar , * br , * cr ;
   double aa,aa1 , bb,bb1 ;
   MRI_IMAGE * imc ;

   if( ima == NULL        || ima->kind != MRI_complex ||
       imb == NULL        || imb->kind != MRI_complex ||
       ima->nx != imb->nx || ima->ny != imb->ny       ||
       alpha < 0.0        || alpha > 1.0              ||
       beta  < 0.0        || beta  > 1.0                ) return NULL ;

   npix = ima->nvox ;
   ar   = MRI_COMPLEX_PTR(ima) ;
   br   = MRI_COMPLEX_PTR(imb) ;
   imc  = mri_new_conforming( ima , MRI_complex ) ;
   cr   = MRI_COMPLEX_PTR(imc) ;

   aa   = alpha ; aa1 = 1.0 - aa ;
   bb   = beta  ; bb1 = 1.0 - bb ;

   for( ii=0 ; ii < npix ; ii++ ){
      r1 = CABS(ar[ii]) ; r2 = CABS(br[ii]) ; rr = pow(r1,aa)*pow(r2,aa1) ;
      t1 = CARG(ar[ii]) ; t2 = CARG(br[ii]) ; tt = t1-t2 ;
           if( tt < -PI ) t2 -= 2.0*PI ;
      else if( tt >  PI ) t2 += 2.0*PI ;
      tt = bb*t1 + bb1*t2 ;
      cr[ii].r = rr * cos(tt) ; cr[ii].i = rr * sin(tt) ;
   }

   return imc ;
}

/*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_scramble( MRI_IMAGE * ima , MRI_IMAGE * imb ,
                          float alpha     , float beta       )
{
   MRI_IMAGE * cxa, * cxb, * cxc ;
   int nx,ny , nxup,nyup ;

   if( ima == NULL        || imb == NULL        ||
       ima->nx != imb->nx || ima->ny != imb->ny ||
       alpha < 0.0        || alpha > 1.0        ||
       beta  < 0.0        || beta  > 1.0          ) return NULL ;

   cxa = mri_fft2D( ima , -1 ) ;
   cxb = mri_fft2D( imb , -1 ) ;
   cxc = cx_scramble( cxa,cxb,alpha,beta ) ;
   mri_free(cxa) ; mri_free(cxb) ;
   cxa = mri_fft2D( cxc , 1 ) ;
   mri_free(cxc) ;
   cxb = mri_to_mri( ima->kind , cxa ) ;
   mri_free(cxa) ;
   return cxb ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   float alpha,beta ;
   MRI_IMAGE * ima , * imb , * imc , * imd ;
   int nx,ny,nxup,nyup ;

   if( argc < 6 ){
      printf("Usage: phace alpha beta im1 im2 fname\n") ; exit(0) ;
   }

   alpha = strtod(argv[1],NULL) ;
   beta  = strtod(argv[2],NULL) ;
   ima   = mri_read(argv[3]) ;
   imb   = mri_read(argv[4]) ;
   imc   = mri_scramble( ima,imb , alpha,beta )  ;

   nx   = ima->nx ; ny   = ima->ny ;
   nxup = imc->nx ; nyup = imc->ny ;

   if( nx < nxup || ny < nyup ){
      imd = mri_cut_2D( imc , 0,nx-1 , 0,ny-1 ) ;
      mri_free(imc) ;
      imc = imd ;
   }

   mri_free(ima) ; mri_free(imb) ;

   mri_write( argv[5] , imd ) ;
   exit(0) ;
}
