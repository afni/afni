
/**********************************************************************
 * Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All    *
 * rights reserved.                                                   *
 *                                                                    *
 * License to copy and use this software is granted provided that it  *
 * is identified as the "RSA Data Security, Inc. MD5 Message-Digest   *
 * Algorithm" in all material mentioning or referencing this software *
 * or this function.                                                  *
 *                                                                    *
 * License is also granted to make and use derivative works provided  *
 * that such works are identified as "derived from the RSA Data       *
 * Security, Inc. MD5 Message-Digest Algorithm" in all material       *
 * mentioning or referencing the derived work.                        *
 *                                                                    *
 * RSA Data Security, Inc. makes no representations concerning either *
 * the merchantability of this software or the suitability of this    *
 * software for any particular purpose. It is provided "as is"        *
 * without express or implied warranty of any kind.                   *
 *                                                                    *
 * These notices must be retained in any copies of any part of this   *
 * documentation and/or software.                                     *
 **********************************************************************/

/*======= Modified by RWCox for inclusion in the AFNI package ========*/
/*------- These changes are released to the public domain     --------*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <sys/utsname.h>  /* these 4 files are needed by the UNIQ_ */
#include <sys/time.h>     /* functions at the bottom of this file  */
#include <unistd.h>
#include <ctype.h>

typedef unsigned char *POINTER;   /* POINTER defines a generic pointer type */
typedef unsigned short int UINT2; /* UINT2 defines a two byte word */
typedef unsigned long int UINT4;  /* UINT4 defines a four byte word */

/* MD5 context data type */

typedef struct {
  UINT4 state[4];                                        /* state (ABCD) */
  UINT4 count[2];             /* number of bits, modulo 2^64 (lsb first) */
  unsigned char buffer[64];                              /* input buffer */
} MD5_CTX;


/* prototypes for some internal functions */

static void MD5Init (MD5_CTX *);
static void MD5Update (MD5_CTX *, unsigned char *, unsigned int);
static void MD5Final (unsigned char [16], MD5_CTX *);

static void MD5Transform (UINT4 [4], unsigned char [64]);
static void Encode (unsigned char *, UINT4 *, unsigned int);
static void Decode (UINT4 *, unsigned char *, unsigned int);

/* Constants for MD5Transform routine.  */

#define S11 7
#define S12 12
#define S13 17
#define S14 22
#define S21 5
#define S22 9
#define S23 14
#define S24 20
#define S31 4
#define S32 11
#define S33 16
#define S34 23
#define S41 6
#define S42 10
#define S43 15
#define S44 21

static unsigned char PADDING[64] = {
  0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/* F, G, H and I are basic MD5 functions.  */

#define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
#define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

/* ROTATE_LEFT rotates x left n bits.  */

#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32-(n))))

/* FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
   Rotation is separate from addition to prevent recomputation.  */

#define FF(a, b, c, d, x, s, ac) { \
 (a) += F ((b), (c), (d)) + (x) + (UINT4)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

#define GG(a, b, c, d, x, s, ac) { \
 (a) += G ((b), (c), (d)) + (x) + (UINT4)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

#define HH(a, b, c, d, x, s, ac) { \
 (a) += H ((b), (c), (d)) + (x) + (UINT4)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

#undef  II
#define II(a, b, c, d, x, s, ac) { \
 (a) += I ((b), (c), (d)) + (x) + (UINT4)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

/*----------------------------------------------------------------------
   MD5 initialization. Begins an MD5 operation, writing a new context.
------------------------------------------------------------------------*/

static void MD5Init (MD5_CTX * context)
{
  context->count[0] = context->count[1] = 0;

  /* Load magic initialization constants */

  context->state[0] = 0x67452301;
  context->state[1] = 0xefcdab89;
  context->state[2] = 0x98badcfe;
  context->state[3] = 0x10325476;
}

/*----------------------------------------------------------------------
   MD5 block update operation. Continues an MD5 message-digest
   operation, processing another message block, and updating the
   context.
------------------------------------------------------------------------*/

static void MD5Update (MD5_CTX * context, unsigned char * input,
                                          unsigned int inputLen  )
{
  unsigned int i, index, partLen;

  /* Compute number of bytes mod 64 */

  index = (unsigned int)((context->count[0] >> 3) & 0x3F);

  /* Update number of bits */

  if( (context->count[0] += ((UINT4)inputLen << 3)) < ((UINT4)inputLen << 3) )
    context->count[1]++;

  context->count[1] += ((UINT4)inputLen >> 29);

  partLen = 64 - index;

  /* Transform as many times as possible.  */

  if (inputLen >= partLen) {

   memcpy ((POINTER)&context->buffer[index], (POINTER)input, partLen);

   MD5Transform (context->state, context->buffer);

   for (i = partLen; i + 63 < inputLen; i += 64)
     MD5Transform (context->state, &input[i]);

   index = 0;
  }
  else
   i = 0;

  /* Buffer remaining input */

  memcpy ((POINTER)&context->buffer[index], (POINTER)&input[i],
          inputLen-i);
}

/*----------------------------------------------------------------------
   MD5 finalization. Ends an MD5 message-digest operation, writing the
   the message digest and zeroizing the context.
------------------------------------------------------------------------*/

static void MD5Final (unsigned char digest[16], MD5_CTX * context)
{
  unsigned char bits[8];
  unsigned int index, padLen;

  /* Save number of bits */

  Encode (bits, context->count, 8);

  /* Pad out to 56 mod 64.  */

  index = (unsigned int)((context->count[0] >> 3) & 0x3f);
  padLen = (index < 56) ? (56 - index) : (120 - index);
  MD5Update (context, PADDING, padLen);

  /* Append length (before padding) */

  MD5Update (context, bits, 8);

  /* Store state in digest */

  Encode (digest, context->state, 16);

  /* Zeroize sensitive information. */

  memset ((POINTER)context, 0, sizeof (*context));
}

/*----------------------------------------------------------------------
   MD5 basic transformation. Transforms state based on block.
------------------------------------------------------------------------*/

static void MD5Transform (UINT4 state[4], unsigned char block[64])
{
  UINT4 a = state[0], b = state[1], c = state[2], d = state[3], x[16];

  Decode (x, block, 64);

  /* Round 1 */

  FF (a, b, c, d, x[ 0], S11, 0xd76aa478); /* 1 */
  FF (d, a, b, c, x[ 1], S12, 0xe8c7b756); /* 2 */
  FF (c, d, a, b, x[ 2], S13, 0x242070db); /* 3 */
  FF (b, c, d, a, x[ 3], S14, 0xc1bdceee); /* 4 */
  FF (a, b, c, d, x[ 4], S11, 0xf57c0faf); /* 5 */
  FF (d, a, b, c, x[ 5], S12, 0x4787c62a); /* 6 */
  FF (c, d, a, b, x[ 6], S13, 0xa8304613); /* 7 */
  FF (b, c, d, a, x[ 7], S14, 0xfd469501); /* 8 */
  FF (a, b, c, d, x[ 8], S11, 0x698098d8); /* 9 */
  FF (d, a, b, c, x[ 9], S12, 0x8b44f7af); /* 10 */
  FF (c, d, a, b, x[10], S13, 0xffff5bb1); /* 11 */
  FF (b, c, d, a, x[11], S14, 0x895cd7be); /* 12 */
  FF (a, b, c, d, x[12], S11, 0x6b901122); /* 13 */
  FF (d, a, b, c, x[13], S12, 0xfd987193); /* 14 */
  FF (c, d, a, b, x[14], S13, 0xa679438e); /* 15 */
  FF (b, c, d, a, x[15], S14, 0x49b40821); /* 16 */

  /* Round 2 */

  GG (a, b, c, d, x[ 1], S21, 0xf61e2562); /* 17 */
  GG (d, a, b, c, x[ 6], S22, 0xc040b340); /* 18 */
  GG (c, d, a, b, x[11], S23, 0x265e5a51); /* 19 */
  GG (b, c, d, a, x[ 0], S24, 0xe9b6c7aa); /* 20 */
  GG (a, b, c, d, x[ 5], S21, 0xd62f105d); /* 21 */
  GG (d, a, b, c, x[10], S22,  0x2441453); /* 22 */
  GG (c, d, a, b, x[15], S23, 0xd8a1e681); /* 23 */
  GG (b, c, d, a, x[ 4], S24, 0xe7d3fbc8); /* 24 */
  GG (a, b, c, d, x[ 9], S21, 0x21e1cde6); /* 25 */
  GG (d, a, b, c, x[14], S22, 0xc33707d6); /* 26 */
  GG (c, d, a, b, x[ 3], S23, 0xf4d50d87); /* 27 */
  GG (b, c, d, a, x[ 8], S24, 0x455a14ed); /* 28 */
  GG (a, b, c, d, x[13], S21, 0xa9e3e905); /* 29 */
  GG (d, a, b, c, x[ 2], S22, 0xfcefa3f8); /* 30 */
  GG (c, d, a, b, x[ 7], S23, 0x676f02d9); /* 31 */
  GG (b, c, d, a, x[12], S24, 0x8d2a4c8a); /* 32 */

  /* Round 3 */

  HH (a, b, c, d, x[ 5], S31, 0xfffa3942); /* 33 */
  HH (d, a, b, c, x[ 8], S32, 0x8771f681); /* 34 */
  HH (c, d, a, b, x[11], S33, 0x6d9d6122); /* 35 */
  HH (b, c, d, a, x[14], S34, 0xfde5380c); /* 36 */
  HH (a, b, c, d, x[ 1], S31, 0xa4beea44); /* 37 */
  HH (d, a, b, c, x[ 4], S32, 0x4bdecfa9); /* 38 */
  HH (c, d, a, b, x[ 7], S33, 0xf6bb4b60); /* 39 */
  HH (b, c, d, a, x[10], S34, 0xbebfbc70); /* 40 */
  HH (a, b, c, d, x[13], S31, 0x289b7ec6); /* 41 */
  HH (d, a, b, c, x[ 0], S32, 0xeaa127fa); /* 42 */
  HH (c, d, a, b, x[ 3], S33, 0xd4ef3085); /* 43 */
  HH (b, c, d, a, x[ 6], S34,  0x4881d05); /* 44 */
  HH (a, b, c, d, x[ 9], S31, 0xd9d4d039); /* 45 */
  HH (d, a, b, c, x[12], S32, 0xe6db99e5); /* 46 */
  HH (c, d, a, b, x[15], S33, 0x1fa27cf8); /* 47 */
  HH (b, c, d, a, x[ 2], S34, 0xc4ac5665); /* 48 */

  /* Round 4 */

  II (a, b, c, d, x[ 0], S41, 0xf4292244); /* 49 */
  II (d, a, b, c, x[ 7], S42, 0x432aff97); /* 50 */
  II (c, d, a, b, x[14], S43, 0xab9423a7); /* 51 */
  II (b, c, d, a, x[ 5], S44, 0xfc93a039); /* 52 */
  II (a, b, c, d, x[12], S41, 0x655b59c3); /* 53 */
  II (d, a, b, c, x[ 3], S42, 0x8f0ccc92); /* 54 */
  II (c, d, a, b, x[10], S43, 0xffeff47d); /* 55 */
  II (b, c, d, a, x[ 1], S44, 0x85845dd1); /* 56 */
  II (a, b, c, d, x[ 8], S41, 0x6fa87e4f); /* 57 */
  II (d, a, b, c, x[15], S42, 0xfe2ce6e0); /* 58 */
  II (c, d, a, b, x[ 6], S43, 0xa3014314); /* 59 */
  II (b, c, d, a, x[13], S44, 0x4e0811a1); /* 60 */
  II (a, b, c, d, x[ 4], S41, 0xf7537e82); /* 61 */
  II (d, a, b, c, x[11], S42, 0xbd3af235); /* 62 */
  II (c, d, a, b, x[ 2], S43, 0x2ad7d2bb); /* 63 */
  II (b, c, d, a, x[ 9], S44, 0xeb86d391); /* 64 */

  state[0] += a;
  state[1] += b;
  state[2] += c;
  state[3] += d;

  /* Zeroize sensitive information. */

  memset ((POINTER)x, 0, sizeof (x));
}

/*----------------------------------------------------------------------
   Encodes input (UINT4) into output (unsigned char). Assumes len is
   a multiple of 4.
------------------------------------------------------------------------*/

static void Encode (unsigned char *output, UINT4 *input, unsigned int len)
{
  unsigned int i, j;

  for (i = 0, j = 0; j < len; i++, j += 4) {
    output[j] = (unsigned char)(input[i] & 0xff);
    output[j+1] = (unsigned char)((input[i] >> 8) & 0xff);
    output[j+2] = (unsigned char)((input[i] >> 16) & 0xff);
    output[j+3] = (unsigned char)((input[i] >> 24) & 0xff);
  }
}

/*----------------------------------------------------------------------
   Decodes input (unsigned char) into output (UINT4). Assumes len is
   a multiple of 4.
------------------------------------------------------------------------*/

static void Decode (UINT4 *output, unsigned char *input, unsigned int len)
{
  unsigned int i, j;

  for (i = 0, j = 0; j < len; i++, j += 4)
    output[i] = ((UINT4)input[j])          | (((UINT4)input[j+1]) << 8) |
               (((UINT4)input[j+2]) << 16) | (((UINT4)input[j+3]) << 24) ;
}

/*======================================================================
   The stuff below is some interface routines, by RWCox
========================================================================*/

/*----------------------------------------------------------------------
  Function to print the 128 bit digest into a 32 character string
------------------------------------------------------------------------*/

static char * MD5_static_printf( unsigned char digest[16] )
{
  static char st[33] ;

  sprintf(st,
          "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x" ,
          digest[0] , digest[1] , digest[2] , digest[3] , digest[4] ,
          digest[5] , digest[6] , digest[7] , digest[8] , digest[9] ,
          digest[10], digest[11], digest[12], digest[13], digest[14],
          digest[15]
         ) ;

  return st ;
}

/*----------------------------------------------------------------------
  Digest an array and returns the printable string of the result
------------------------------------------------------------------------*/

char * MD5_static_array( int n , char * bytes )
{
   MD5_CTX context;
   unsigned char digest[16];

   if( n < 0 || bytes == NULL ) return NULL ;

   MD5Init( &context ) ;
   MD5Update( &context, bytes, n ) ;
   MD5Final( digest, &context ) ;

   return MD5_static_printf(digest) ;
}

char * MD5_malloc_array( int n , char * bytes )
{
   char *st , *dy ;
   st = MD5_static_array( n , bytes ) ;
   if( st == NULL ) return NULL ;
   dy = (char *) malloc(33) ; strcpy(dy,st) ; return dy ;
}

/*----------------------------------------------------------------------*/

char * MD5_static_string( char * string )
{
   if( string == NULL ) return NULL ;
   return MD5_static_array( strlen(string) , string ) ;
}

char * MD5_malloc_string( char * string )
{
   if( string == NULL ) return NULL ;
   return (char*)MD5_malloc_array( strlen(string) , string ) ;
}

/*----------------------------------------------------------------------*/
/* Digests a file and prints the result.  */
/*----------------------------------------------------------------------*/

char * MD5_static_file(char * filename)
{
  FILE *file;
  MD5_CTX context;
  int len;
  unsigned char buffer[1024] ;
  unsigned char digest[16] ;

  if( (file = fopen (filename, "rb")) == NULL ) return NULL ;

  MD5Init( &context ) ;

  while( len = fread(buffer, 1, 1024, file) )
      MD5Update( &context, buffer, len ) ;

  MD5Final( digest, &context );
  fclose (file);

  return MD5_static_printf( digest ) ;
}

char * MD5_malloc_file(char * filename)
{
   char *st , *dy ;

   st = MD5_static_file( filename ) ;
   if( st == NULL ) return NULL ;
   dy = (char *) malloc(33) ; strcpy(dy,st) ; return dy ;
}

/*============================================================================*/

/*----------------------------------------------------------------------------
   Return the output as a Base64 string, instead of a hex string
   -- strlen(result) is 22 instead of 32
   -- result is malloc()-ed and should be free()-d when appropriate
------------------------------------------------------------------------------*/

extern void B64_to_base64( int, char *, int *, char ** ) ; /* in thd_base64.c */

static char * MD5_to_B64( unsigned char digest[16] )
{
   int nb64=0 ; char *b64=NULL ;

   B64_to_base64( 16 , (char *)digest , &nb64 , &b64 ) ;  /* thd_base64.c */
   if( nb64 <= 0 || b64 == NULL ) return NULL ;
   b64[nb64-3] = '\0' ;                           /* remove trailing "==" */
   return b64 ;
}

char * MD5_B64_array( int n , char * bytes )
{
   MD5_CTX context;
   unsigned char digest[16];

   if( n < 0 || bytes == NULL ) return NULL ;

   MD5Init( &context ) ;
   MD5Update( &context, bytes, n ) ;
   MD5Final( digest, &context ) ;

   return MD5_to_B64( digest ) ;
}

char * MD5_B64_string( char * string )
{
   if( string == NULL ) return NULL ;
   return MD5_B64_array( strlen(string) , string ) ;
}

char * MD5_B64_file(char * filename)
{
  FILE *file;
  MD5_CTX context;
  int len;
  unsigned char buffer[1024] ;
  unsigned char digest[16] ;

  if( (file = fopen (filename, "rb")) == NULL ) return NULL ;

  MD5Init( &context ) ;

  while( len = fread(buffer, 1, 1024, file) )
      MD5Update( &context, buffer, len ) ;

  MD5Final( digest, &context );
  fclose (file);

  return MD5_to_B64( digest ) ;
}

/*-----------------------------------------------------------------------
  Return a globally unique identifier (I hope).  This is a malloc()-ed
  string of length <= 31 (plus the NUL byte; the whole thing will fit
  into a char[32] array).  The output does not contain any '/'s, so
  it could be used as a temporary filename.
  Method: generate a string from the system identfier information and
          the current time of day; MD5 hash this to a 128 byte code;
          Base64 encode this to a 22 byte string; replace '/' with '-'
          and '+' with '_'; add 4 character prefix (1st 3 characters
          of environment variable IDCODE_PREFIX plus '_').
  -- RWCox - 27 Sep 2001
-------------------------------------------------------------------------*/

char * UNIQ_idcode(void)
{
   struct utsname ubuf ;
   struct timeval tv ;
   int    nn , ii ;
   int  nbuf ;
   char *buf , *idc , *eee ;
   static int ncall=0 ;                /* number of times I've been called */

   /* get info about this system */

   nn = uname( &ubuf ) ;               /* get info about this system */
   if( nn == -1 ){                     /* should never happen */
      strcpy( ubuf.nodename , "E" ) ;
      strcpy( ubuf.sysname  , "L" ) ;
      strcpy( ubuf.release  , "V" ) ;
      strcpy( ubuf.version  , "I" ) ;
      strcpy( ubuf.machine  , "S" ) ;
   }

   /* store system info into a string buffer */

   nbuf = strlen(ubuf.nodename)+strlen(ubuf.sysname)
         +strlen(ubuf.release )+strlen(ubuf.version)+strlen(ubuf.machine) ;

   buf = AFMALL(char, nbuf+64) ;      /* include some extra space */
   strcpy(buf,ubuf.nodename) ;
   strcat(buf,ubuf.sysname ) ;
   strcat(buf,ubuf.release ) ;
   strcat(buf,ubuf.version ) ;
   strcat(buf,ubuf.machine ) ;

   idc = AFMALL(char, 32) ;         /* will be output string */

   /* get time and store into buf */

   nn = gettimeofday( &tv , NULL ) ;
   if( nn == -1 ){              /* should never happen */
      tv.tv_sec  = (long) buf ;
      tv.tv_usec = (long) idc ;
   }

   sprintf(buf+nbuf,"%d%d%d%d",
          (int)tv.tv_sec,(int)tv.tv_usec,(int)getpid(),ncall) ;
   ncall++ ;

   /* get prefix for idcode from environment, if present */

   eee = getenv("IDCODE_PREFIX") ;
   if( eee != NULL && isalpha(eee[0]) ){
     for( ii=0 ; ii < 3 && isalnum(eee[ii]) ; ii++ )
       idc[ii] = eee[ii] ;
   } else {
     strcpy(idc,"NIH") ;
   }
   strcat(idc,"_") ;  /* recall idc was calloc()-ed */

   /* MD5+Base64 encode buf to be latter part of the idcode */

   eee = MD5_B64_string( buf ) ;
   if( eee != NULL ){                     /* should always work */
      int nn = strlen(eee) ;
      for( ii=0 ; ii < nn ; ii++ ){
              if( eee[ii] == '/' ) eee[ii] = '-' ;  /* / -> - */
         else if( eee[ii] == '+' ) eee[ii] = '_' ;  /* + -> _ */
      }
      strcat(idc,eee) ;
   } else {                               /* should never happen */
     nn = strlen(idc) ;
     sprintf(idc+nn,"%d_%d",(int)tv.tv_sec,(int)tv.tv_usec) ;
   }

   /* free workspaces and get outta here */

   if( eee != NULL ) free(eee) ;
   free(buf) ; return idc ;
}

/*----------------------------------------------------------------------
   Fill a user-supplied buffer (length at least 32) with an idcode
------------------------------------------------------------------------*/

void UNIQ_idcode_fill( char *idc )
{
   char *bbb ;
   if( idc == NULL ) return ;
   bbb = UNIQ_idcode() ;
   strcpy(idc,bbb) ; free(bbb) ; return ;
}

/*============================================================================*/
