#include <string.h>
#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *im , *flim ;
   MRI_IMARR *imar ;
   int kim ;

   static char * dtype[] = {
      "byte" , "short" , "int" , "float" , "double" , "complex" } ;

   int ii , npix , imk , nzero ;

   float im_min,im_min2 , im_max,im_max2 , im_ave , im_std ;
   float *flar ;

   int nopt=1 , dolabel=TRUE , doquiet = FALSE ;
   char * pix_prefix = NULL ;

   /*-------*/

   if( argc < 2 || strncmp(argv[1],"-h",2) == 0 ){
      printf(
        "Calculation of statistics of one or more images.\n"
        "Usage: imstat [-nolabel] [-pixstat prefix] [-quiet] image_file ...\n"
        "  -nolabel        = don't write labels on each file's summary line\n"
        "  -quiet          = don't print statistics for each file\n"
        "  -pixstat prefix = if more than one image file is given, then\n"
        "                     'prefix.mean' and 'prefix.sdev' will be written\n"
        "                     as the pixel-wise statistics images of the whole\n"
        "                     collection.  These images will be in the 'flim'\n"
        "                     floating point format.  [This option only works\n"
        "                     on 2D images!]\n"
      ) ;
      exit(0) ;
   }

   while( nopt < argc && argv[nopt][0] == '-' ){
      if( strncmp(argv[nopt],"-nolabel",4) == 0 ){
         dolabel = FALSE ;
         nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-quiet",4) == 0 ){
         doquiet = TRUE ;
         nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-pixstat",4) == 0 ){
         pix_prefix = argv[++nopt] ;
         nopt++ ; continue ;
      }
      fprintf(stderr,"Unknown option %s\n",argv[nopt]) ; exit(1) ;
   }
   if( nopt >= argc ){
      fprintf(stderr,"No filename arguments?!\n") ; exit(1) ;
   }

   /*--------*/

   imar = mri_read_many_files( argc-nopt , argv+nopt ) ;
   if( imar == NULL || IMARR_COUNT(imar) < 1 ){
      fprintf(stderr,"Can't read any images!?\n") ; exit(1) ;
   }

   for( kim=0 ; kim < IMARR_COUNT(imar) ; kim++ ){

      im  = IMARR_SUBIMAGE(imar,kim) ;
      imk = im->kind ;

      if( im->kind == MRI_float ){
         flim = im ;
      } else {
         flim = mri_to_float( im ) ; mri_free( im ) ;
      }

      npix = flim->nvox ;
      flar = MRI_FLOAT_PTR( flim ) ;

      im_max = im_min = flar[0] ;
      im_ave = 0.0 ;
      for( ii=0 ; ii < npix ; ii++ ){
              if( flar[ii] > im_max ) im_max  = flar[ii] ;
         else if( flar[ii] < im_min ) im_min  = flar[ii] ;
         im_ave += flar[ii] ;
      }
      im_ave /= npix ;

      im_max2 = 2 * im_min - im_max ;
      im_min2 = 2 * im_max - im_min ;
      for( ii=0 ; ii < npix ; ii++ ){
         if( flar[ii] > im_max2 && flar[ii] < im_max ) im_max2 = flar[ii] ;
         if( flar[ii] < im_min2 && flar[ii] > im_min ) im_min2 = flar[ii] ;
      }

      im_std = 0.0 ;
      nzero  = 0 ;
      for( ii=0 ; ii < npix ; ii++ ){
         im_std += (flar[ii]-im_ave) * (flar[ii]-im_ave) ;
         if( flar[ii] == 0.0 ) nzero++ ;
      }
      im_std = sqrt(im_std/npix) ;

      if( ! doquiet ){
         if( dolabel){
            printf( "\nfile = %s  nx = %d  ny = %d  data type = %s\n" ,
                    flim->name , flim->nx , flim->ny , dtype[imk] ) ;
            printf( "min =%11.4g  next min=%11.4g  max=%11.4g  next max=%11.4g\n" ,
                    im_min,im_min2 , im_max,im_max2 ) ;
            printf( "mean=%11.4g  std.dev.=%11.4g  number of zero pixels = %d\n" ,
                    im_ave,im_std,nzero ) ;
         } else {
            printf( "%d %d %d " , flim->nx , flim->ny , imk ) ;
            printf( "%11.4g %11.4g %11.4g %11.4g %11.4g %11.4g %d\n" ,
                    im_min,im_min2 , im_max,im_max2 , im_ave,im_std,nzero ) ;
         }
      }

      if( pix_prefix != NULL ) (void)mri_stat_seq( flim ) ;

      mri_free(flim) ;
   }

   if( pix_prefix != NULL ){
      char fname[128] ;
      MRI_IMAGE ** sim ;
      float * mar , * sar ;
      float cmax ;

      sim = mri_stat_seq( NULL ) ;
      ii = strlen(pix_prefix) ;

      if( pix_prefix[ii-1] == '.' ) sprintf( fname , "%smean" , pix_prefix ) ;
      else                          sprintf( fname , "%s.mean" , pix_prefix ) ;
      mri_write( fname , sim[0] ) ;
      printf("-- Wrote mean image to %s\n",fname) ;

      if( pix_prefix[ii-1] == '.' ) sprintf( fname , "%ssdev" , pix_prefix ) ;
      else                          sprintf( fname , "%s.sdev" , pix_prefix ) ;
      mri_write( fname , sim[1] ) ;
      printf("-- Wrote standard deviation image to %s\n",fname) ;

      if( pix_prefix[ii-1] == '.' ) sprintf( fname , "%scvar" , pix_prefix ) ;
      else                          sprintf( fname , "%s.cvar" , pix_prefix ) ;
      npix = sim[0]->nx * sim[0]->ny ;
      mar  = MRI_FLOAT_PTR(sim[0]) ;
      sar  = MRI_FLOAT_PTR(sim[1]) ;
      for( ii=0 ; ii < npix ; ii++ )
         if( mar[ii] != 0.0 ) mar[ii] = sar[ii] / fabs(mar[ii]) ;
      cmax = mri_max( sim[0] ) ;
      for( ii=0 ; ii < npix ; ii++ )
         if( mar[ii] == 0.0 ) mar[ii] = cmax ;
      mri_write( fname , sim[0] ) ;
      printf("-- Wrote coefficient of variation image to %s\n",fname) ;
   }

   exit(0) ;
}
