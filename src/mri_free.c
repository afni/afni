/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

void *mri_data_pointer( MRI_IMAGE *im )
{
   void *data ;

#ifdef USE_MRI_DELAY
   if( im->fname != NULL && (im->fondisk & INPUT_DELAY) )
      mri_input_delay( im ) ;
#endif

   switch( im->kind ){
      case MRI_byte:   data = im->im.byte_data   ; break ;
      case MRI_short:  data = im->im.short_data  ; break ;
      case MRI_int:    data = im->im.int_data    ; break ;
      case MRI_float:  data = im->im.float_data  ; break ;
      case MRI_double: data = im->im.double_data ; break ;
      case MRI_complex:data = im->im.complex_data; break ;
      case MRI_rgb:    data = im->im.rgb_data    ; break ;
      default:         data = NULL               ; break ;
   }
   return data ;
}

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
   }
   return ;
}

void mri_free( MRI_IMAGE *im )
{
   void * ptr ;
   if( im == NULL ) return ;
#ifdef USE_MRI_DELAY
   if( im->fname != NULL ){ free(im->fname) ; im->fname = NULL ; }
   im->fondisk = 0 ;
#endif
   if( im->name != NULL ){ free(im->name) ; im->name = NULL ; }
   ptr = mri_data_pointer(im) ;
   if( ptr != NULL ) free(ptr) ;
   free(im) ;
   return ;
}

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
      default:          return 0 ;
   }
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
