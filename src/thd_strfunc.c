#include "mrilib.h"

/*-----------------------------------------------------------------*/
/*! Find if needle is in haystack,
    ignoring case and ignoring any characters in ignore. */

char * ig_strstr( char *haystack , char *needle , char *ignore )
{
   char *hs, *ne , *cp ;
   int ii, jj ;

   if( haystack == NULL || haystack[0] == '\0' ||
       needle   == NULL || needle[0]   == '\0'   ) return NULL ;

   /* make uppercase copy of haystack */

   hs = strdup(haystack) ; jj = strlen(hs) ;
   for( ii=0 ; ii < jj ; ii++ ) hs[ii] = toupper(hs[ii]) ;

   /* replace all ignore characters in hs with a period */

   if( ignore != NULL && ignore[0] != '\0' ){
     for( ii=0 ; ii < jj ; ii++ ){
       if( strchr(ignore,hs[ii]) != NULL ) hs[ii] = '.' ;
     }
   }

   /* make uppercase copy of needle */

   ne = strdup(needle) ; jj = strlen(ne) ;
   for( ii=0 ; ii < jj ; ii++ ) ne[ii] = toupper(ne[ii]) ;

   /* replace all ignore characters in ne with a period */

   if( ignore != NULL && ignore[0] != '\0' ){
     for( ii=0 ; ii < jj ; ii++ ){
       if( strchr(ignore,ne[ii]) != NULL ) ne[ii] = '.' ;
     }
   }

   /* now find if mangled needle is in the mangled haystack */

   cp = strstr( hs , ne ) ;

   /* if it is, then find corresponding location in original haystack */

   if( cp != NULL ){
     jj = cp-hs ;            /* pointer arithmetic */
     cp = haystack + jj ;    /* ditto */
   }

   /* we're outta here */

   free(ne); free(hs); return cp;
}
