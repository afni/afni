
/* ------  functions open for use:  (i.e. non-static) ------------------ 
 *
 * r_sprintf_long_to_hex  - convert a long to a hex digit string
 *
 * r_hex_str_to_long      - convert a hex digit string to a long
 *
 * ---------------------------------------------------------------------
*/


#include <stdio.h>
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

