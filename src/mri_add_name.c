#include "mrilib.h"
#include <string.h>

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/


/*** 7D SAFE ***/

/*** Copy a string into the image structure;
     The usual use is to remember the input filename ***/

void mri_add_name( char * str , MRI_IMAGE * im )
{
   int ll ;

   if( im->name != NULL ){ free( im->name ) ; im->name = NULL ; }

   if( str == NULL ) return ;

   ll = strlen(str) ; if( ll <= 0 ) return ;

   im->name = (char * ) malloc( ll+1 ) ;
   strcpy( im->name , str ) ;
   return ;
}

#ifdef USE_MRI_DELAY
void mri_add_fname_delay( char * str , MRI_IMAGE * im )
{
   int ll ;

   if( im->fname != NULL ){ free( im->fname ) ; im->fname = NULL ; }

   if( str == NULL ) return ;

   ll = strlen(str) ; if( ll <= 0 ) return ;

   im->fname = (char * ) malloc( ll+1 ) ;
   strcpy( im->fname , str ) ;
   return ;
}
#endif
