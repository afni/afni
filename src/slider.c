#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE * inim , * outim ;
   byte      * inby , * outby ;
   int ii , nx,ny , npix ;
   byte rr,gg,bb ;

   if( argc < 3 ){
      printf("Usage: slider infile.ppm outfile.ppm\n") ;
      exit(0) ;
   }

   inim = mri_read_ppm( argv[1] ) ;
   if( inim == NULL ){
      fprintf(stderr,"Cannot read input file!\n") ;
      exit(1) ;
   }

   inby = MRI_RGB_PTR(inim) ;
   nx   = inim->nx ; ny = inim->ny ; npix = nx * ny ;

   outim = mri_new( nx , ny , MRI_rgb ) ;
   outby = MRI_RGB_PTR(outim) ;

   for( ii=0 ; ii < 3*npix ; ii+=3 ){

      rr = inby[ii] ; gg = inby[ii+1] ; bb = inby[ii+2] ;

      if( rr==255 && gg==255 && bb==255 ){
         rr = 0 ; gg = 0 ; bb = 111 ;
      } else if( rr==143 && gg==143 && bb > 200 ){
         rr = 255 ; gg = 255 ; bb = 0 ;
      }

      outby[ii] = rr ; outby[ii+1] = gg ; outby[ii+2] = bb ;
   }

   mri_write_pnm( argv[2] , outim ) ;
   exit(0) ;
}
