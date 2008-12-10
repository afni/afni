/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

/*-----------------------------------------------------
  Convert input image to the type given by "datum".
-------------------------------------------------------*/

MRI_IMAGE * mri_to_mri( int datum , MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;

ENTRY("mri_to_mri") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   if( oldim->kind == datum ){
     newim = mri_copy(oldim) ; RETURN(newim) ;   /* 10 Dec 2008 */
   }

   if( oldim->kind == MRI_fvect ){               /* 10 Dec 2008 */
     MRI_IMAGE *qim = mri_fvect_subimage(oldim,0) ;
     if( datum == MRI_float ){
       newim = qim ;
     } else {
       newim = mri_to_mri(datum,qim) ; mri_free(qim) ;
     }
     RETURN(newim) ;
   }

   /*-- return to the normal conversion cases --*/

   switch( datum ){
      default:
         fprintf(stderr,
           "\nUnsupported mri_to_mri conversion!\a\n") ;
         newim = NULL ;
      break ;

      case MRI_short:{
         double imtop ;
         imtop = mri_maxabs( oldim ) ;

         if( imtop <= 32767.0 )
            newim = mri_to_short( 1.0 , oldim ) ;
         else
            newim = mri_to_short_scl( 0.0 , 10000.0 , oldim ) ;
      }
      break ;

      case MRI_float:
         newim = mri_to_float( oldim ) ;
      break ;

      case MRI_byte:{
         float immin , immax ;

         if( oldim->kind == MRI_byte ){
            newim = mri_to_byte( oldim ) ;
         } else {
            immin = mri_min( oldim ) ; immax = mri_max( oldim ) ;
            if( immin >= 0 && immax < 256 )
               newim = mri_to_byte_scl( 1.0 , 0.0 , oldim ) ;
            else
               newim = mri_to_byte_scl( 0.0 , 255.0 , oldim ) ;
        }
      }
      break ;

      case MRI_complex:
         newim = mri_to_complex( oldim ) ;
      break ;

      case MRI_rgb:
         newim = mri_to_rgb( oldim ) ; /* 11 Feb 1999 */
      break ;

      case MRI_rgba:
         newim = mri_to_rgba( oldim ) ; /* 20 Mar 2002 */
      break ;

  }
  RETURN( newim );
}

/*-----------------------------------------------------
  Convert input image to the type given by "datum",
  scaled by the given factor.
-------------------------------------------------------*/

MRI_IMAGE * mri_to_mri_scl( int datum , double factor , MRI_IMAGE * oldim )
{
   MRI_IMAGE * newim ;

ENTRY("mri_to_mri_scl") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   switch( datum ){
      default:
         fprintf(stderr,
           "\nUnsupported mri_to_mri conversion!\a\n") ;
         newim = NULL ;
      break ;

      case MRI_short:
         newim = mri_to_short( factor , oldim ) ;
      break ;

      case MRI_float:
         newim = mri_scale_to_float( factor , oldim ) ;
      break ;

      case MRI_byte:
         newim = mri_to_byte_scl( factor , 0.0 , oldim ) ;
      break ;

      case MRI_complex:{
         complex * cxar ; int ii , nvox ;
         newim = mri_to_complex( oldim ) ;
         cxar = MRI_COMPLEX_PTR(newim) ;
         nvox = newim->nvox ;
         for( ii=0 ; ii < nvox ; ii++ ){
            cxar[ii].r *= factor ; cxar[ii].i *= factor ;
         }
      }
      break ;
  }
  RETURN( newim );
}
