#include "mrilib.h"

static int  dtable_mode = -1 ;    /* 1=encode, 2=decode, -1=neither */
static byte dtable[256] ;         /* encode/decode table */
static int linelen = 72 ;         /* line length (max 76) */
static int ncrlf   = 1 ;

#define B64_goodchar(c) (dtable[c] != 0x80)  /* for decode only */

#define B64_EOL1 '\r'   /* CR */
#define B64_EOL2 '\n'   /* LF */

/*----------------------------------------------------------------------*/

void B64_set_crlf( int nn )
{
   if( nn >= 1 && nn <= 2 ) ncrlf = nn ;
   return ;
}

/*----------------------------------------------------------------------*/

void B64_set_linelen( int ll )
{
   if( ll >= 16 && ll <= 76 ) linelen = 4*(ll/4) ; /* multiple of 4 */
   else                       linelen = 72 ;
   return ;
}

/*----------------------------------------------------------------------*/

static void load_encode_table(void)
{
    int i ;
    if( dtable_mode == 1 ) return ;
    for (i = 0; i < 26; i++) {
        dtable[i] = 'A' + i;
        dtable[26 + i] = 'a' + i;
    }
    for (i = 0; i < 10; i++) dtable[52 + i] = '0' + i;
    dtable[62] = '+'; dtable[63] = '/'; dtable_mode = 1 ;
    return ;
}

/*----------------------------------------------------------------------*/

static void load_decode_table(void)
{
    int i;
    if( dtable_mode == 2 ) return ;
    for (i = 0  ; i < 255 ; i++) dtable[i] = 0x80;
    for (i = 'A'; i <= 'Z'; i++) dtable[i] = 0 + (i - 'A');
    for (i = 'a'; i <= 'z'; i++) dtable[i] = 26 + (i - 'a');
    for (i = '0'; i <= '9'; i++) dtable[i] = 52 + (i - '0');
    dtable['+'] = 62; dtable['/'] = 63; dtable['='] = 0; dtable_mode = 2 ;
    return ;
}

#define B64_encode3(a,b,c,w,x,y,z)                 \
     ( w = dtable[(a)>>2]                      ,   \
       x = dtable[((a & 3) << 4) | (b >> 4)]   ,   \
       y = dtable[((b & 0xF) << 2) | (c >> 6)] ,   \
       z = dtable[c & 0x3F]                     )

#define B64_encode2(a,b,w,x,y,z)                   \
     ( B64_encode3(a,b,0,w,x,y,z) , z = '=' )

#define B64_encode1(a,w,x,y,z)                     \
     ( B64_encode3(a,0,0,w,x,y,z) , y=z = '=' )

#define B64_decode4(w,x,y,z,a,b,c)                 \
     ( a = (dtable[w] << 2) | (dtable[x] >> 4) ,   \
       b = (dtable[x] << 4) | (dtable[y] >> 2) ,   \
       c = (dtable[y] << 6) | dtable[z]         )

#define B64_decode_count(w,x,y,z)                  \
     ( ((w)=='='||(x)=='=') ? 0                    \
                            : ((y)=='=') ? 1       \
                            : ((z)=='=') ? 2 : 3 )

/*----------------------------------------------------------------------
   Convert base64 encoding to a binary array

   Inputs: nb64 = number of bytes in b64
            b64 = array of base64 encoding bytes
                  values not in the base64 encoding set will be skipped

   Outputs: *nbin = number of binary bytes [*nbin==0 flags an error]
             *bin = pointer to newly malloc()-ed space with bytes
------------------------------------------------------------------------*/

void B64_to_binary( int nb64 , byte * b64 , int * nbin , byte ** bin )
{
   int ii,jj , nn ;
   byte a,b,c , w,x,y,z ;

   /*- sanity checks -*/

   if( nbin == NULL || bin == NULL ) return ;

   if( nb64 < 4 || b64 == NULL ){ *nbin = 0 ; *bin = NULL ; return ; }

   *bin = (byte *) malloc(sizeof(byte)*(2+3*nb64/4)) ;
   if( *bin == NULL ){ *nbin = 0 ; return ; }

   /*- some work -*/

   load_decode_table() ;
   for( ii=jj=0 ; ii < nb64 ; ){  /* scan inputs, skipping bad characters */
      w = b64[ii++] ;
      while( !B64_goodchar(w) && ii < nb64 ) w = b64[ii++] ;
      x = (ii < nb64) ? b64[ii++] : '=' ;
      while( !B64_goodchar(x) && ii < nb64 ) x = b64[ii++] ;
      y = (ii < nb64) ? b64[ii++] : '=' ;
      while( !B64_goodchar(y) && ii < nb64 ) y = b64[ii++] ;
      z = (ii < nb64) ? b64[ii++] : '=' ;
      while( !B64_goodchar(z) && ii < nb64 ) z = b64[ii++] ;

      B64_decode4(w,x,y,z,a,b,c) ;

      if( z == '=' ){                        /* got to the end? */
         nn = B64_decode_count(w,x,y,z) ;    /* see how many to save */
         if( nn > 0 ) (*bin)[jj++] = a ;
         if( nn > 1 ) (*bin)[jj++] = b ;
         break ;
      }

      /* not at the end => save all 3 outputs */

      (*bin)[jj++] = a ; (*bin)[jj++] = b ; (*bin)[jj++] = c ;
   }

   *bin  = (byte *) realloc( *bin , sizeof(byte)*jj ) ;
   *nbin = jj ;
   return ;
}

/*----------------------------------------------------------------------
   Convert binary array to base64 encoding

   Inputs: nbin = number of bytes in bin
            bin = array of binary bytes to encode

   Outputs: *nb64 = number of base64 bytes [*nb64==0 flags an error]
             *b64 = pointer to newly malloc()-ed space with bytes

   The output array (*b64) line length can be set by
      B64_set_linelen(n)
   where n is from 16 to 76.  The default is 72.  Note, however, that
   encoded bytes will always be written out in groups of 4.
   The output array line separator can be the LF character only (Unix)
   or the CR-LF combination (DOS, etc.).  This is controlled by
      B64_set_crlf(n)
   where n=1 for LF, n=2 for CR-LF.  The default is LF.  The output
   array will be terminated with a line separator.  There will be
   no ASCII NUL character at the end of *b64 -- that is, the output
   is not a C string.
------------------------------------------------------------------------*/

void B64_to_base64( int nbin , byte * bin , int * nb64 , byte ** b64 )
{
   int ii,jj , nn,n3 ;
   byte a,b,c , w,x,y,z ;

   /*- sanity checks -*/

   if( nb64 == NULL || b64 == NULL ) return ;
   if( nbin <= 0    || bin == NULL ){ *nb64 = 0 ; *b64 = NULL ; return ; }

   nn   = (4.0*(linelen+ncrlf+1.0)/(3.0*linelen))*nbin + 256 ;
   *b64 = (byte *) malloc(sizeof(byte)*nn) ;
   if( *b64 == NULL ){ *nb64 = 0 ; return ; }

   /*- do blocks of 3 -*/

   load_encode_table() ;
   n3 = (nbin/3)*3 ;
   for( nn=jj=ii=0 ; ii < n3 ; ){
      a = bin[ii++] ; b = bin[ii++] ; c = bin[ii++] ;
      B64_encode3(a,b,c,w,x,y,z) ;
      (*b64)[jj++] = w ;
      (*b64)[jj++] = x ;
      (*b64)[jj++] = y ;
      (*b64)[jj++] = z ;
      nn += 4 ; if( nn >= linelen ){
                   if( ncrlf == 2 ) (*b64)[jj++] = B64_EOL1 ;
                   (*b64)[jj++] = B64_EOL2 ;
                   nn = 0 ;
                }
   }

   /*- do the leftovers, if any (1 or 2 bytes) -*/

   if( ii < nbin ){
      if( ii == nbin-2 )
         B64_encode2(bin[ii],bin[ii+1],w,x,y,z) ;
      else
         B64_encode1(bin[ii],w,x,y,z) ;

      (*b64)[jj++] = w ;
      (*b64)[jj++] = x ;
      (*b64)[jj++] = y ;
      (*b64)[jj++] = z ; nn += 4 ;
   }

   if( nn > 0 ){
      if( ncrlf == 2 ) (*b64)[jj++] = B64_EOL1 ;
      (*b64)[jj++] = B64_EOL2 ;
   }

   *b64  = (byte *) realloc( *b64 , sizeof(byte)*jj ) ;
   *nb64 = jj ;
   return ;
}
