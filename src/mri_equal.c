#include "mrilib.h"

/*! Test if 2 images are equal.
    Returns 1 if they are, 0 if they are not.  [30 Jun 2003] */

int mri_equal( MRI_IMAGE *aim , MRI_IMAGE *bim )
{
   char *aar , *bar ;
   int nn ;

ENTRY("mri_equal") ;

   if( aim == bim )                                RETURN( 1 );
   if( aim == NULL || bim == NULL )                RETURN( 0 );

   if( aim->nvox != bim->nvox )                    RETURN( 0 );
   if( aim->kind != bim->kind )                    RETURN( 0 );

   aar = mri_data_pointer(aim) ; if( aar == NULL ) RETURN( 0 );
   bar = mri_data_pointer(bim) ; if( bar == NULL ) RETURN( 0 );

   nn = memcmp( aar , bar , aim->nvox * aim->pixel_size ) ;
   if( nn != 0 ) nn = 0 ;
   else          nn = 1 ;
                                                   RETURN( nn);
}
