/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <string.h>

/*** 7D SAFE ***/

/*** Copy a string into the image structure;
     The usual use is to remember the input filename ***/

void mri_add_name( char * str , MRI_IMAGE * im )
{
   int ll ;

ENTRY("mri_add_name") ;
   if( im == NULL ) EXRETURN ;  /* 29 Mar 2002 */

   if( im->name != NULL ){ free( im->name ) ; im->name = NULL ; }

   if( str == NULL ) EXRETURN ;

   ll = strlen(str) ; if( ll <= 0 ) EXRETURN ;

   im->name = (char * ) malloc( ll+1 ) ;
   strcpy( im->name , str ) ;
   EXRETURN ;
}

#ifdef USE_MRI_DELAY
void mri_add_fname_delay( char * str , MRI_IMAGE * im )
{
   int ll ;

ENTRY("mri_add_fname_delay") ;

   if( im == NULL ) EXRETURN ;  /* 29 Mar 2002 */

   if( im->fname != NULL ){ free( im->fname ) ; im->fname = NULL ; }

   if( str == NULL ) EXRETURN ;

   ll = strlen(str) ; if( ll <= 0 ) EXRETURN ;

   im->fname = (char * ) malloc( ll+1 ) ;
   strcpy( im->fname , str ) ;
   EXRETURN ;
}
#endif
