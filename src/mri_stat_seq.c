/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** NOT 7D SAFE ***/

/* Collect stats on sequence of images.
   Usage: call repeatedly, once for each image;  will return void.
          after last image, call with NULL as argument, will return
          pointer to array of two pointers to (float) images, the
          first the mean, the second the stdev.
*/

MRI_IMAGE ** mri_stat_seq( MRI_IMAGE * imin )
{

/* static variables (exist between calls) */

   static MRI_IMAGE * imsum , * imsumq ;
   static int nim = 0 , npix , nx , ny ;
   static double * dbsum , * dbsumq ;

/* local variables */

   register int ii ;
   MRI_IMAGE * imfl , * imsd , ** retval ;
   float     * flar , * sdar ;
   double    scl , vscl , vvv ;

/*** case: set up new problem ***/

   if( nim == 0 ){

      if( imin == NULL ){
         fprintf(stderr,"mri_stat_seq:  NULL argument for initial call!\n") ;
         EXIT(1) ;
      }

      if( ! MRI_IS_2D(imin) ){
         fprintf(stderr,"\n*** mri_stat_seq: only works on 2D images!\n") ;
         EXIT(1) ;
      }

      nx   = imin->nx ;
      ny   = imin->ny ;
      npix = nx * ny ;

      imsum  = mri_new( nx , ny , MRI_double ) ;
      imsumq = mri_new( nx , ny , MRI_double ) ;
      dbsum  = mri_data_pointer( imsum ) ;
      dbsumq = mri_data_pointer( imsumq ) ;

      MRI_COPY_AUX(imsum,imin) ;

      for( ii=0 ; ii < npix ; ii++ ) dbsum[ii] = dbsumq[ii] = 0.0 ;

   }

/*** case: add new image into problem ***/

   if( imin != NULL ){

      if( imin->nx != nx || imin->ny != ny ){
         fprintf(stderr,"mri_stat_seq: input image size mismatch!\n") ;
         EXIT(1) ;
      }

      if( ! MRI_IS_2D(imin) ){
         fprintf(stderr,"\n*** mri_stat_seq: only works on 2D images!\n") ;
         EXIT(1) ;
      }

      if( imin->kind == MRI_float ){
         imfl = imin ;
      } else {
         imfl = mri_to_float( imin ) ;
      }
      flar = mri_data_pointer( imfl ) ;

      for( ii=0 ; ii < npix ; ii++ ){
         dbsum[ii]  += flar[ii] ;
         dbsumq[ii] += flar[ii] * flar[ii] ;
      }

      if( imin != imfl ) mri_free( imfl ) ;
      nim++ ;
      return NULL ;
   }

/*** case: make report on results, and reset static data ***/

   if( nim < 1 ){
      fprintf(stderr,"mri_stat_seq: # images input=%d; too small!\n",nim) ;
      EXIT(1) ;
   }

   imfl = mri_new( nx , ny , MRI_float ) ;
   imsd = mri_new( nx , ny , MRI_float ) ;
   flar = mri_data_pointer( imfl ) ;
   sdar = mri_data_pointer( imsd ) ;

   MRI_COPY_AUX(imfl,imsum) ;

   scl  = 1.0 / nim ;
   vscl = (nim==1) ? 1.0 : sqrt( ((double)(nim))/(nim-1.0) ) ;

   for( ii=0 ; ii < npix ; ii++ ){
      flar[ii] = scl * dbsum[ii] ;
      vvv      = scl * dbsumq[ii] - flar[ii] * flar[ii] ;
      sdar[ii] = (vvv > 0.0) ? (sqrt(vvv)*vscl) : 0.0 ;
   }

   mri_free( imsum ) ; mri_free( imsumq ) ;
   nim = 0 ;

   retval = (MRI_IMAGE **) malloc( 2 * sizeof(MRI_IMAGE *) ) ;
   if( retval == NULL ){
      fprintf(stderr,"mri_stat_seq: malloc error for retval!\n") ;
      EXIT(1) ;
   }

   retval[0] = imfl ;
   retval[1] = imsd ;

   return retval ;
}
