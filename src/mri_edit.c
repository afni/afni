#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/


/*** 7D SAFE ***/

#ifndef MAX
# define MAX(x,y) (((x)>(y))?(x):(y))
#endif

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_edit_image( float pthr , float power , MRI_IMAGE * imin )
{
   int ii , npix , nsum ;
   float val ;
   MRI_IMAGE * imqq ;
   float * flin ;

#ifdef DEBUG
printf("Entry: mri_edit_image\n") ;
#endif

   imqq = mri_to_float( imin ) ;
   flin = mri_data_pointer( imqq ) ;
   npix = imqq->nvox ;

   if( (power==0.0 || power==1.0) && (pthr==0.0) ) return imqq ;

   if( pthr > 0.0 && pthr < 1.0 ){
      register float sum , fa , scl,fmax ;
      register int nsum ;

      fmax = fabs(mri_max(imqq)) ;
      val  = fabs(mri_min(imqq)) ;
      fmax = MAX(fmax,val) ;
      val  = pthr * fmax ;           /* average pixels > pthr * max */
      sum  = 0.0 ;
      nsum = 0 ;

      for( ii=0 ; ii < npix ; ii++ ){
         fa = flin[ii] = fabs(flin[ii]) ;
         if( fa > val ){ sum += fa ; nsum++ ; }
      }
      val = pthr * sum / nsum ;    /* set threshold based on this */

#ifdef HARD_THRESH
      for( ii=0 ; ii < npix ; ii++ ) if(flin[ii] < val) flin[ii] = 0.0 ;
#else
      scl = fmax / (fmax-val) ;
      for( ii=0 ; ii < npix ; ii++ ){
         fa = flin[ii] ;
         flin[ii] = (fa < val) ? (0.0) : (scl*(fa-val)) ;
      }
#endif
   }  /* end of if(pthr) */

   if( power != 0.0 && power != 1.0 ){
      for( ii=0 ; ii < npix ; ii++ ) flin[ii] = pow( fabs(flin[ii]) , power ) ;
   }

   MRI_COPY_AUX(imqq,imin) ;
   return imqq ;
}
