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

void *mri_data_pointer(const MRI_IMAGE *im )
{
   void *data ;

   if( im == NULL ) return NULL ;  /* 27 Jul 2004 */

   if( im->fname != NULL && (im->fondisk & INPUT_DELAY) )
      mri_input_delay( im ) ;
   else if( MRI_IS_PURGED(im) ) /* 20 Dec 2006 */
      mri_unpurge( im ) ;

   data = im->im ;
   return data ;
}

/*-------------------------------------------------------------------------*/
/*! Modify the data pointer in an MRI_IMAGE struct.
---------------------------------------------------------------------------*/

void mri_fix_data_pointer( void *ptr , MRI_IMAGE *im )
{
   if( im == NULL ) return ;
   im->im = ptr ;
   return ;
}

/*-------------------------------------------------------------------------*/
/*! Get rid of an MRI_IMAGE struct and all its contents.
---------------------------------------------------------------------------*/

void mri_free( MRI_IMAGE *im )
{
   void *ptr ;

ENTRY("mri_free") ;
   if( im == NULL ) EXRETURN ;
   mri_killpurge(im) ;  /* 20 Dec 2006 */
   if( im->fname != NULL ){ free(im->fname) ; im->fname = NULL ; }
   im->fondisk = 0 ;
   if( im->name != NULL ){ free(im->name) ; im->name = NULL ; }
   ptr = mri_data_pointer(im) ;
   if( ptr != NULL ) free(ptr) ;
   free(im) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------*/

void mri_clear( MRI_IMAGE *im )  /* 31 Jan 2007 */
{
  void *ptr ;
  if( im == NULL ) return ;
  mri_killpurge(im) ;
  ptr = mri_data_pointer(im) ;
  if( ptr != NULL ){ free(ptr); mri_fix_data_pointer(NULL,im); }
  return ;
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
/* Replaces former MRI_DIMENSIONALITY macro.  [12 Dec 2007] */

int mri_dimensionality( MRI_IMAGE *im )
{
  if( im == NULL ) return 0 ;
  if( im->nw > 1 ) return 7 ;
  if( im->nv > 1 ) return 6 ;
  if( im->nu > 1 ) return 5 ;
  if( im->nt > 1 ) return 4 ;
  if( im->nz > 1 ) return 3 ;
  if( im->ny > 1 ) return 2 ;
  return 1;
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

   if( qim->fname != NULL ) free(qim->fname) ;
   if( qim->name  != NULL ) free(qim->name) ;
   ptr = mri_data_pointer(qim) ;
   if( ptr != NULL ) free(ptr) ;

   /* put the contents of zim in their place */

   *qim = *zim ;

   /* NULL out the contents of zim that are pointers */

   mri_fix_data_pointer( NULL , zim ) ;
   zim->name  = NULL ;
   zim->fname = NULL ;
   EXRETURN ;
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
