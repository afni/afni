/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <math.h>
#include <stdlib.h>

#if 1
#  ifdef isfinite
#    define IS_GOOD_FLOAT(x) isfinite(x) /* 28 Aug 2003: use C99 macro if exists */
#  else
#    define IS_GOOD_FLOAT(x) finite(x)
#    define isfinite finite
#  endif
#else
#    define IS_GOOD_FLOAT(x) finite(x)
#endif

#if 0
# define IS_GOOD_FLOAT(x) isnan(x)  /* obsolete */
#endif

/*---------------------------------------------------------------------
   Scan an array of floats for illegal values, replacing them with 0.
   Return the number of illegal values found.
-----------------------------------------------------------------------*/

size_t thd_floatscan( size_t nbuf , float *fbuf )
{
   size_t ii , nerr ;

   if( nbuf <= 0 || fbuf == NULL ) return 0 ;

   for( nerr=ii=0 ; ii < nbuf ; ii++ )
     if( !IS_GOOD_FLOAT(fbuf[ii]) ){ fbuf[ii] = 0.0f ; nerr++ ; }

   return nerr ;
}

/*--------------------------------------------------------------------*/

#if 0
typedef struct complex { float r , i ; } complex ;  /* cf. mrilib.h */
#endif

size_t thd_complexscan( size_t nbuf , complex *cbuf )
{
   size_t ii , nerr ;

   if( nbuf <= 0 || cbuf == NULL ) return 0 ;

   for( nerr=ii=0 ; ii < nbuf ; ii++ ){
     if( !IS_GOOD_FLOAT(cbuf[ii].r) ){ cbuf[ii].r = 0.0f ; nerr++ ; }
     if( !IS_GOOD_FLOAT(cbuf[ii].i) ){ cbuf[ii].i = 0.0f ; nerr++ ; }
   }

   return nerr ;
}

/*--------------------------------------------------------------------*/
/* Functions below added 22 Feb 2007 -- RWCox */

size_t mri_floatscan( MRI_IMAGE *im )
{
   if( im == NULL ) return 0 ;
   switch( im->kind ){
     default: break ;
     case MRI_float:
       return thd_floatscan  ( im->nvox , MRI_FLOAT_PTR(im)   ) ;
     case MRI_complex:
       return thd_complexscan( im->nvox , MRI_COMPLEX_PTR(im) ) ;
   }
   return 0 ;
}

/*--------------------------------------------------------------------*/

size_t imarr_floatscan( MRI_IMARR *imar )
{
   size_t ii , nn ;
   if( imar == NULL ) return 0 ;
   for( nn=ii=0 ; ii < IMARR_COUNT(imar) ; ii++ ){
     nn += mri_floatscan( IMARR_SUBIM(imar,ii) ) ;
   }
   return nn ;
}

/*--------------------------------------------------------------------*/

size_t dblk_floatscan( THD_datablock *dblk )
{
   size_t nn ;
   if( !ISVALID_DATABLOCK(dblk) ) return 0 ;
   nn = imarr_floatscan( dblk->brick ) ;
   return nn ;
}

/*--------------------------------------------------------------------*/

size_t dset_floatscan( THD_3dim_dataset *dset )
{
   size_t nn ;
   if( !ISVALID_DSET(dset) ) return 0 ;
   nn = dblk_floatscan( dset->dblk ) ;
   return nn ;
}
