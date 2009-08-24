/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

MRI_IMAGE * ts_to_ftime( int nwin , MRI_IMAGE *tim )
{
   MRI_IMAGE *ftim ;
   float *ftar , *tar , *xar , *win , val ;
   complex *cxar ;
   int nt , nf , nw2 , nfft, nfft2 , ii,it,kf ;

   if( tim == NULL || tim->kind != MRI_float ||
       tim->ny > 1 || nwin < 16              || tim->nx < nwin ) return NULL ;

   nt = tim->nx ; tar = MRI_FLOAT_PTR(tim) ;
   if( nwin%2 == 0 ) nwin++ ;
   nw2 = nwin/2 ;

   xar = (float *) malloc(sizeof(float)*(nt+2*nw2)) ;
   for( ii=0 ; ii < nw2 ; ii++ ) xar[ii] = xar[nt+nw2+ii] = 0.0 ;
   memcpy( xar+nw2 , tar , sizeof(float)*nt ) ;

   nfft = csfft_nextup_one35( nwin ) ; nfft2 = nfft/2 ;
   ftim = mri_new( nt , nfft2 , MRI_float ) ;
   ftar = MRI_FLOAT_PTR(ftim) ;
   cxar = (complex *) malloc( sizeof(complex) * nfft ) ;

fprintf(stderr,"nfft = %d\n",nfft) ;

   win = mri_setup_taper( nwin , 1.0 ) ;

   for( it=0 ; it < nt ; it++ ){

      for( val=0.0,ii=0 ; ii < nwin ; ii++ ) val += xar[it+ii-nw2] ;
      val /= nwin ;

      for( ii=0 ; ii < nwin ; ii++ ){
         cxar[ii].r = (xar[it+ii-nw2]-val)*win[ii] ; cxar[ii].i = 0.0 ;
      }
      for( ii=nwin ; ii < nfft ; ii++ ) cxar[ii].r = cxar[ii].i = 0.0 ;

      csfft_cox( -1 , nfft , cxar ) ;

      for( kf=0 ; kf < nfft2 ; kf++ )
         ftar[it+nt*kf] = CSQR( cxar[kf] ) ;
   }

   free(win) ; free(xar) ; free(cxar) ;
   return ftim ;
}

int main( int argc , char *argv[] )
{
   MRI_IMAGE *inim , *outim ;
   int nopt,nwin;

   /*-- help? --*/

   if( argc < 4 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dftime nwin infile outfile\n"
            "where infile is an AFNI *.1D file (ASCII list of numbers arranged\n"
            "in columns); outfile will be a similar file, with each column being\n"
            "L2 normalized.\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   nopt = 1 ;
   nwin = strtol( argv[nopt] , NULL , 10 ) ;
   if( nwin < 16 ){
      fprintf(stderr,"** Illegal nwin!\n"); exit(1);
   }

   nopt++ ;
   if( nopt+1 >= argc ){
      fprintf(stderr,"** Need input and output filenames!\n");exit(1);
   }

   if( !THD_filename_ok(argv[nopt+1]) ){
      fprintf(stderr,"** Illegal output filename!\n"); exit(1);
   }
   if( THD_is_file(argv[nopt+1]) ){
      fprintf(stderr,"** Output file already exists!\n"); exit(1);
   }

   /* read input file */

   inim = mri_read_1D( argv[nopt] ) ;
   if( inim == NULL ){
      fprintf(stderr,"** Can't read input file!\n"); exit(1);
   }

   outim = ts_to_ftime( nwin , inim ) ;
   if( outim == NULL ){
      fprintf(stderr,"** Can't compute output!\n"); exit(1);
   }

   mri_write_1D( argv[nopt+1] , outim ) ;
   exit(0) ;
}
