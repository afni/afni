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

/*--------------------------------------------------------------------------
  Free up an array of previously malloc()-ed strings
----------------------------------------------------------------------------*/

void freeup_strings( int n , char **sar )
{
   int ii ;
   if( sar == NULL ) return ;
   for( ii=0 ; ii < n ; ii++ )
      if( sar[ii] != NULL ) free(sar[ii]) ;
   free(sar) ;
   return ;
}

/*--------------------------------------------------------------------------
  Break a string into a set of sub-strings.
  Return value is number of sub-strings (< 0 is an error);
  (*stok)[i] is the i-th string.
----------------------------------------------------------------------------*/

#define DQUOT  '"'   /* double quote character */
#define SQUOT  '\''  /* single quote character */
#define NUL    '\0'  /* ASCII NUL character    */

int breakup_string( char *sin , char ***stok )
{
   int n_tok , quote , ll ;
   char **s_tok , *cpt , *sss , qch=NUL ;

   if( stok == NULL || sin == NULL || sin[0] == NUL ) return -1 ;

   n_tok = 0 ;
   s_tok = NULL ;

   cpt = sin ;

   while( *cpt != '\0' ){  /* loop until we use up the input string */

      /* skip whitespace */

      while( isspace(*cpt) ) cpt++ ;
      if( *cpt == NUL ) break ;        /* reached end */

      /* if starts with a quote, note that factoid */

      if( *cpt == SQUOT || *cpt == DQUOT ){
         quote = 1 ; qch = *cpt ; cpt++ ;   /* qch = quote character */
         if( *cpt == NUL ) break ;
      } else {
        quote = 0 ;
      }

      /* scan until end of sub-string (next quote, or next nonblank) */

      sss = cpt ;    /* start of sub-string */
      if( quote ){
         while( *cpt != NUL && *cpt != qch    ) cpt++ ;  /* scan to next quote */
      } else {
         while( *cpt != NUL && !isspace(*cpt) ) cpt++ ;  /* to next non-blank */
      }

      /* cpt now points to character after end of sub-string */

      ll = cpt - sss ;  /* number of characters in sub-string (may be zero) */

      /* make a new entry in the string table */

      s_tok = (char **) realloc( s_tok , sizeof(char *) * (n_tok+1) ) ;
      s_tok[n_tok] = (char *) malloc(ll+4) ;
      if( ll > 0 ) memcpy( s_tok[n_tok] , sss , ll ) ;
      s_tok[n_tok][ll] = NUL ;
      n_tok++ ;

      /* skip the character after the end of sub-string */

      cpt++ ;
   } /* end of loop over input string */

   *stok = s_tok ; return n_tok ;
}
