#include "mrilib.h"

/*----- set up an RGB color table, with ngray level of gray,
        and a color "cube" nrr X ngg X nbb color cells on a side;
        total number of cells is ngray+nrr*ngg*nbb-1              -----*/

MRI_IMAGE * mri_colorsetup( int ngray , int nrr , int ngg , int nbb )
{
   MRI_IMAGE *im ;
   rgbyte *ar ;
   int rr,gg,bb , nn ;
   float rac,gac,bac ;

   im = mri_new( ngray + nrr*ngg*nbb , 1 , MRI_rgb ) ;
   ar = (rgbyte *) MRI_RGB_PTR(im) ;

   gac = 255.9f / ngray ; nn = 0 ;
   for( gg=0 ; gg <= ngray ; gg++,nn++ ){
     ar[nn].r = ar[nn].g = ar[nn].b = (byte)(gac*gg) ;
   }

   rac = 255.9f/(nrr-1) ; gac = 255.9f/(ngg-1) ; bac=255.9f / (nbb-1) ;
   for( bb=0 ; bb < nbb ; bb++ ){
    for( gg=0 ; gg < ngg ; gg++ ){
      for( rr=0 ; rr < nrr ; rr++ ){
        if( rr==0     && gg==0     && bb==0     ) continue ;
        if( rr==nrr-1 && gg==ngg-1 && bb==nbb-1 ) continue ;
        ar[nn].r = (byte)(rac*rr) ;
        ar[nn].g = (byte)(gac*gg) ;
        ar[nn].b = (byte)(bac*bb) ; nn++ ;
   } } }

   return im ;
}
