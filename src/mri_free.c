/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

/*-------------------------------------------------------------------------*/
/*! Return the pointer to the data array in an MRI_IMAGE struct.
---------------------------------------------------------------------------*/

void *mri_data_pointer( MRI_IMAGE *im )
{
   void *data ;

#ifdef USE_MRI_DELAY
   if( im->fname != NULL && (im->fondisk & INPUT_DELAY) )
      mri_input_delay( im ) ;
#endif

   if( im == NULL ) return NULL ;  /* 27 Jul 2004 */

   switch( im->kind ){
      case MRI_byte:   data = im->im.byte_data   ; break ;
      case MRI_short:  data = im->im.short_data  ; break ;
      case MRI_int:    data = im->im.int_data    ; break ;
      case MRI_float:  data = im->im.float_data  ; break ;
      case MRI_double: data = im->im.double_data ; break ;
      case MRI_complex:data = im->im.complex_data; break ;
      case MRI_rgb:    data = im->im.rgb_data    ; break ;
      case MRI_rgba:   data = im->im.rgba_data   ; break ;
      default:         data = NULL               ; break ;
   }
   return data ;
}

/*-------------------------------------------------------------------------*/
/*! Modify the data pointer in an MRI_IMAGE struct.
---------------------------------------------------------------------------*/

void mri_fix_data_pointer( void * ptr , MRI_IMAGE *im )
{
   if( im == NULL ) return ;
   switch( im->kind ){
      case MRI_byte:   im->im.byte_data   = (byte *)    ptr; break ;
      case MRI_short:  im->im.short_data  = (short *)   ptr; break ;
      case MRI_int:    im->im.int_data    = (int   *)   ptr; break ;
      case MRI_float:  im->im.float_data  = (float *)   ptr; break ;
      case MRI_double: im->im.double_data = (double *)  ptr; break ;
      case MRI_complex:im->im.complex_data= (complex *) ptr; break ;
      case MRI_rgb:    im->im.byte_data   = (byte *)    ptr; break ;
      case MRI_rgba:   im->im.rgba_data   = (rgba *)    ptr; break ;
   }
   return ;
}

/*-------------------------------------------------------------------------*/
/*! Get rid of an MRI_IMAGE struct and all its contents.
---------------------------------------------------------------------------*/

void mri_free( MRI_IMAGE *im )
{
   void * ptr ;

ENTRY("mri_free") ;
   if( im == NULL ) EXRETURN ;
#ifdef USE_MRI_DELAY
   if( im->fname != NULL ){ free(im->fname) ; im->fname = NULL ; }
   im->fondisk = 0 ;
#endif
   if( im->name != NULL ){ free(im->name) ; im->name = NULL ; }
   ptr = mri_data_pointer(im) ;
   if( ptr != NULL ) free(ptr) ;
   free(im) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------*/
/*! Return the size (bytes) of one data element of the given type.
---------------------------------------------------------------------------*/

int mri_datum_size( MRI_TYPE typ )
{
   switch( typ ){
      case MRI_byte:    return sizeof(byte) ;
      case MRI_short:   return sizeof(short) ;
      case MRI_int:     return sizeof(int) ;
      case MRI_float:   return sizeof(float) ;
      case MRI_double:  return sizeof(double) ;
      case MRI_complex: return sizeof(complex) ;
      case MRI_rgb:     return 3*sizeof(byte) ;
      case MRI_rgba:    return sizeof(rgba) ;
      default:          return 0 ;
   }
}

/*-------------------------------------------------------------------------*/
/*! Replace the guts of MRI_IMAGE struct qim with those of zim.
    Afterwards, what's left of zim is mri_free()-ed, so don't ever refer
    to it again. If you want a copy of an image, use mri_copy() instead.
---------------------------------------------------------------------------*/

void mri_move_guts( MRI_IMAGE *qim , MRI_IMAGE *zim )
{
   void *ptr ;

ENTRY("mri_move_guts") ;

   if( qim == NULL || zim == NULL ) EXRETURN ;  /* stupid caller */

   /* destroy the contents inside qim, if any */

#ifdef USE_MRI_DELAY
   if( qim->fname != NULL ) free(qim->fname) ;
#endif
   if( qim->name != NULL ) free(qim->name) ;
   ptr = mri_data_pointer(qim) ;
   if( ptr != NULL ) free(ptr) ;

   /* put the contents of zim in their place */

   *qim = *zim ;

   /* NULL out the contents of zim, then free() it */

   mri_fix_data_pointer( NULL , zim ) ;
   zim->name = NULL ;
#ifdef USE_MRI_DELAY
   zim->fname = NULL ;
#endif
   free(zim) ; EXRETURN ;
}

/*---------------- added fake rint() 12 Feb 2001 ---------------*/

#ifdef NO_RINT
double rint( double x )
{
   int nn ;

   if( x >= 0.0 ) nn = (int)(x+0.49999999) ;
   else           nn = (int)(x-0.49999999) ;
   return (double)nn ;
}
#endif
