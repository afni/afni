
/* ------  functions open for use:  (i.e. non-static) ------------------ 
 *
 * r_sprintf_long_to_hex  - convert a long to a hex digit string
 *
 * r_hex_str_to_long      - convert a hex digit string to a long
 *
 * ---------------------------------------------------------------------
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "r_misc.h"

/* -- locals */
static int ulong_size ( unsigned long l );
/* -- end locals */


/*----------------------------------------------------------------------
 * r_hex_str_to_long    - convert hex_digits hex digits to a long
 *
 * Conver up to 8 hex digits into a single unsigned long integer.
 * The input string must consist of hex type characters, without
 * a leading 0x.  i.e. '0'->'9', 'a'->'f' and 'A'->'F'.
 *
 * return the unsigned long integer, or 0 on failure
 *----------------------------------------------------------------------
*/
unsigned long
r_hex_str_to_long ( char * src, int hex_digits )
{
    unsigned long   res = 0;
    char          * cp;
    int             nib, digs;

    if ( hex_digits <= 0 || hex_digits > 8 )
        return 0;

    for ( digs = hex_digits, cp = src; digs > 0; digs--, cp++ )
    {
        if ( (*cp >= '0') && (*cp <= '9') )
            nib = *cp - '0';
        else if ( (*cp >= 'a') && (*cp <= 'f') )
            nib = *cp - 'a' + 10;
        else if ( (*cp >= 'A') && (*cp <= 'F') )
            nib = *cp - 'A' + 10;
        else
        {
            fprintf( stderr, "r_hex_str_to_long: invalid input string <%8s>\n",
                     src );
            return 0;
        }

        res = (res << 4) + (nib & 0xf);
    }

    return res;
}


/*----------------------------------------------------------------------
 * r_sprintf_long_to_hex    - write hex chars to a string
 *
 * Convert the low-order number of bytes from the unsigned long into
 * a null-terminated string hex characters (strlen() is twice the
 * number of converted bytes).
 *
 * We input the option to pad the string with zeros if the number
 * is too small for the requested number of bytes.
 *
 * return number of hex pairs written (if padding, should equal bytes)
 *----------------------------------------------------------------------
*/
int
r_sprintf_long_to_hex
    (
        char          * dest,           /* location of output string     */
        unsigned long   lsrc,           /* number to translate           */
        int             bytes,          /* total bytes (hex pairs)       */
        int             pad             /* pad the result with zeros?    */
    )
{
    static char hexstring[] = "0123456789ABCDEF";

    unsigned char   ub;
    char          * cp = dest;
    int             posn, size, ret;

    if ( (bytes <= 0) || (bytes > 4) )
    {
        *cp = '\0';
        return 0;
    }

    size = ulong_size( lsrc );

    if ( (size < bytes) && !pad )       /* use size if we avoid padding  */
        ret = size;
    else
        ret = bytes;

    for ( posn = ret-1; posn >= 0; posn-- )
    {
        /* write one hex pair for this byte */
        ub = ( lsrc >> (posn << 3) ) & 0xff;            /* current ubyte */
        *cp++ = hexstring[(ub>>4) & 0xf];               /* upper nibble  */
        *cp++ = hexstring[ ub     & 0xf];               /* lower nibble  */
    }

    *cp = '\0';

    return ret;
}

/* return number of bytes needed to represent a long - return at least 1 */
static int
ulong_size ( unsigned long l )
{
    if ( l & 0xff000000 )
        return 4;

    if ( l & 0xff0000 )
        return 3;

    if ( l & 0xff00 )
        return 2;

    return 1;
}

/* allocate and catenate the strings into one long one, using the
 * given separator string                    12 May, 2014 [rickr] */
char * cat_strings(char * slist[], int nstr, char * sepstr)
{
   char * newstr, * localsep = " ";
   int    sind, total, first = 1, seplen = 0;

   if( !slist || nstr <= 0 ) return NULL;

   /* so localsep points to something */
   if( sepstr ) localsep = sepstr;
   seplen = strlen(sepstr);

   /* first compute total space, add 1 for nul 23 Sep, 2014 [rickr] */
   if( slist[0] ) total = strlen(slist[0])+1;
   else           total = 1;

   for( sind = 1; sind < nstr; sind++ )
      if( slist[sind] ) total += strlen(slist[sind]) + seplen;

   /* allocate and dupe... */
   newstr = (char *)calloc(total, sizeof(char));

   /* append the strings */
   for( sind = 0; sind < nstr; sind++ ) {
      if( ! slist[sind] ) continue;
      if( first ) { first = 0; }
      else        { strcat(newstr, localsep); }
      strcat(newstr, slist[sind]);
   }

   return newstr;
}

/* display sequence of slist strings to FILE stream, from bot to top
 * (follow with newline)
 * 
 * example: disp_strings(stderr, "-- prior args: ",
 *                       argc, argv, nopt-3, nopt-1, " ", 1);
 *
 *      fp      : FILE pointer to write to
 *      nstr    : total number of strings in list, just for checking
 *      slist   : array of strings
 *      bot     : bottom index to display
 *      top     : top index to display
 *      sepstr  : separation string, e.g. " ", "\n", "\n   "
 *
 * - require bot >= 0, bot <= top and top < nstr
 *
 */
int disp_strings(FILE * fp, char * mesg, int nstr, char * slist[],
                 int bot, int top, char * sepstr, int newline)
{
   char ** sptr;
   int     first=bot, last=top, ind;

   /* check inputs */
   if( !fp || nstr <= 0 || !slist ) return 1;

   if( mesg ) fputs(mesg, fp);

   /* limit bounds */
   if( first < 0 ) first = 0;
   if( last >= nstr ) last = nstr-1;
   if( first > last ) return 1;

   /* print first, then rest with sepstr */
   ind = first;
   sptr = slist + ind;
   fputs(*sptr ? *sptr : "<NULL>", fp);

   for( ind+=1, sptr+=1; ind <= last; ind++, sptr++ ) {
      if( sepstr ) fputs(sepstr, fp);
      fputs(*sptr ? *sptr : "<NULL>", fp);
   }

   if( newline ) fputc('\n', fp);

   return 0;
}
