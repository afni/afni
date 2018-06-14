#include "niml_private.h"

/*************************************************************************/
/************************* Stuff for MD5 hashing *************************/
/** [Most are not actually used in NIML, but are here for completeness] **/
/*************************************************************************/

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

/*======= Modified by RWCox for inclusion in the NIML package ========*/
/*------- These changes are released to the public domain     --------*/

/* prototypes for some internal functions */

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

#define II(a, b, c, d, x, s, ac) { \
 (a) += I ((b), (c), (d)) + (x) + (UINT4)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

/*----------------------------------------------------------------------*/
/* #define USE_XOR  
                        ZSS: Apr. 15 2015 
   I have no idea at the moment why USE_XOR ends up causing SUMA labels
   to get written on top of each other in the viewer. This happens on mac
   and linux versions. This definition might also be causing 
   other mysterious crashes in SUMA when opening the surface controller 
   as reported on the message borad. 
   For now, it will stay off until we sort out why this is causing grief.
   Valgrind did not point to anything fishy related to USE_XOR.
                                                                       */
#ifdef  USE_XOR
# define               XOR_NUM 1024
 static int            XOR_iii = -1 ;
 static unsigned char *XOR_bbb = NULL ;
 static int            XOR_use = 1 ;
#endif /* USE_XOR */

void MD5_set_xor_use(int xx){
#ifdef USE_XOR
  XOR_use = xx ;
#endif
}
/*----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/*! MD5 initialization. Begins an MD5 operation, writing a new context.
------------------------------------------------------------------------*/

void MD5Init (MD5_CTX *context)
{
  context->count[0] = context->count[1] = 0;

  /* Load magic initialization constants */

  context->state[0] = 0x67452301;
  context->state[1] = 0xefcdab89;
  context->state[2] = 0x98badcfe;
  context->state[3] = 0x10325476;

#ifdef USE_XOR
  if( XOR_iii < 0 ){
    XOR_bbb = (unsigned char *)malloc(sizeof(unsigned char)*XOR_NUM) ;
    for( XOR_iii=0 ; XOR_iii < XOR_NUM ; XOR_iii++ )
      XOR_bbb[XOR_iii] = (unsigned char)((XOR_iii*557+1021)%255) ;
    XOR_iii = 0 ;
  }
#endif

  return ;
}

/*----------------------------------------------------------------------*/
/*! MD5 block update operation. Continues an MD5 message-digest
   operation, processing another message block, and updating the
   context.
------------------------------------------------------------------------*/

void MD5Update (MD5_CTX *context, unsigned char *input,
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

/*----------------------------------------------------------------------*/
/*! MD5 finalization. Ends an MD5 message-digest operation, writing the
   the message digest and zeroizing the context.
------------------------------------------------------------------------*/

void MD5Final (unsigned char digest[16], MD5_CTX *context)
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

/*----------------------------------------------------------------------*/
/*! MD5 basic transformation. Transforms state based on block.
------------------------------------------------------------------------*/

static void MD5Transform (UINT4 state[4], unsigned char block[64])
{
  UINT4 a = state[0], b = state[1], c = state[2], d = state[3], x[16];

#ifdef USE_XOR
  { register int qq ;
    for( qq=0 ; qq < 64 ; qq++ ){
      block[qq] ^= XOR_bbb[XOR_iii] ; XOR_iii = (XOR_iii+1)%XOR_NUM ;
    }
  }
#endif

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

/*----------------------------------------------------------------------*/
/*! Encodes input (UINT4) into output (unsigned char). Assumes len is
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

/*----------------------------------------------------------------------*/
/*! Decodes input (unsigned char) into output (UINT4). Assumes len is
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
   The stuff below is some MD5 interface routines, by RWCox
========================================================================*/

/*----------------------------------------------------------------------*/
/*! Function to print a 128 bit digest into a static 32 char string.
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

/*----------------------------------------------------------------------*/
/*! Digest an array and returns the printable string of the result,
    stored in a static array (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_static_array( int n , char *bytes )
{
   MD5_CTX context;
   unsigned char digest[16];

   if( n < 0 || bytes == NULL ) return NULL ;

   MD5Init( &context ) ;
   MD5Update( &context, (unsigned char *)bytes, n ) ;
   MD5Final( digest, &context ) ;

   return MD5_static_printf(digest) ;
}

/*----------------------------------------------------------------------*/
/*! Digest an array and returns the printable string of the result,
    stored in a malloc()-ed array (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_malloc_array( int n , char *bytes )
{
   char *st , *dy ;
   st = MD5_static_array( n , bytes ) ;
   if( st == NULL ) return NULL ;
   dy = (char *) malloc(33) ; strcpy(dy,st) ; return dy ;
}

/*----------------------------------------------------------------------*/
/*! Digest a C string and returns the printable string of the result,
    stored in a static array (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_static_string( char *string )
{
   if( string == NULL ) string = "ElvisTheKing" ;
   return MD5_static_array( strlen(string) , string ) ;
}

/*----------------------------------------------------------------------*/
/*! Digest a C string and returns the printable string of the result,
    stored in a malloc()-ed array (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_malloc_string( char *string )
{
   if( string == NULL ) string = "ElvisTheKing" ;
   return MD5_malloc_array( strlen(string)+1 , string ) ;
}

/*----------------------------------------------------------------------*/
/*! Digests a file and prints the result, stored in a static array
    (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_static_file(char *filename)
{
  FILE *file;
  MD5_CTX context;
  int len;
  unsigned char buffer[1024] ;
  unsigned char digest[16] ;

  if( (file = fopen(filename, "rb")) == NULL ) return NULL ;

  MD5Init( &context ) ;

  while( (len = fread(buffer, 1, 1024, file)) )
      MD5Update( &context, buffer, len ) ;

  MD5Final( digest, &context );
  fclose (file);

  return MD5_static_printf( digest ) ;
}

/*----------------------------------------------------------------------*/
/*! Digests a file and prints the result, stored in a malloc()-ed array
    (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_malloc_file(char *filename)
{
   char *st , *dy ;

   st = MD5_static_file( filename ) ;
   if( st == NULL ) return NULL ;
   dy = (char *) malloc(33) ; strcpy(dy,st) ; return dy ;
}

/*----------------------------------------------------------------------------*/
/*! Convert a MD5 hex string to a Base64-ed string.
    * strlen(result) is 22 instead of 32
    * result is malloc()-ed and should be free()-d when appropriate
------------------------------------------------------------------------------*/

static char * MD5_to_B64( unsigned char digest[16] )
{
   int nb64=0 ; byte *b64=NULL ;

   B64_to_base64( 16 , (byte *)digest , &nb64 , &b64 ) ;
   if( nb64 <= 0 || b64 == NULL ) return NULL ;
   b64[nb64-3] = '\0' ;                           /* remove trailing "==" */
   if( isspace(b64[nb64-4]) ) b64[nb64-4]='\0' ;
   return (char *)b64 ;
}

/*----------------------------------------------------------------------------*/
/*! Return the MD5 hash of an array as a Base64 string, instead of a hex
    string.
    * strlen(result) is 22 instead of 32
    * result is malloc()-ed and should be free()-d when appropriate
------------------------------------------------------------------------------*/

char * MD5_B64_array( int n , char *bytes )
{
   MD5_CTX context;
   unsigned char digest[16];

   if( n < 0 || bytes == NULL ) return NULL ;

   MD5Init( &context ) ;
   MD5Update( &context, (unsigned char *)bytes, n ) ;
   MD5Final( digest, &context ) ;

   return MD5_to_B64( digest ) ;
}

/*----------------------------------------------------------------------------*/
/*! Return the MD5 hash of a C string as a Base64 string, instead of a hex
    string.
    * strlen(result) is 22 instead of 32
    * result is malloc()-ed and should be free()-d when appropriate
------------------------------------------------------------------------------*/

char * MD5_B64_string( char *string )
{
   if( string == NULL ) string = "ElvisTheKing" ;
   return MD5_B64_array( strlen(string) , string ) ;
}

/*----------------------------------------------------------------------------*/
/*! Return the MD5 hash of a file as a Base64 string, instead of a hex
    string.
    - strlen(result) is 22 instead of 32
    - result is malloc()-ed and should be free()-d when appropriate
------------------------------------------------------------------------------*/

char * MD5_B64_file(char *filename)
{
  FILE *file;
  MD5_CTX context;
  int len;
  unsigned char buffer[1024] ;
  unsigned char digest[16] ;

  if( (file=fopen (filename, "rb")) == NULL ) return NULL ;

  MD5Init( &context ) ;

  while( (len = fread(buffer, 1, 1024, file)) )
      MD5Update( &context, buffer, len ) ;

  MD5Final( digest, &context );
  fclose (file);

  return MD5_to_B64( digest ) ;
}

/*##########################################################################*/
/*----------------------- SHA-256 Hash Functions ---------------------------*/
/*##########################################################################*/

#ifndef uint8
#define uint8  unsigned char
#endif

#ifndef uint32
#define uint32 unsigned long int
#endif

typedef struct {
    uint32 total[2];
    uint32 state[8];
    uint8 buffer[64];
} sha256_context;

static void sha256_starts( sha256_context *ctx );
static void sha256_update( sha256_context *ctx, uint8 *input, uint32 length );
static void sha256_finish( sha256_context *ctx, uint8 digest[32] );

/*
 *  FIPS-180-2 compliant SHA-256 implementation
 *
 *  Copyright (C) 2001-2003  Christophe Devine
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <string.h>

/*----------------------------------------------------------------------*/

#define GET_UINT32(n,b,i)                       \
{                                               \
    (n) = ( (uint32) (b)[(i)    ] << 24 )       \
        | ( (uint32) (b)[(i) + 1] << 16 )       \
        | ( (uint32) (b)[(i) + 2] <<  8 )       \
        | ( (uint32) (b)[(i) + 3]       );      \
}

/*----------------------------------------------------------------------*/

#define PUT_UINT32(n,b,i)                       \
{                                               \
    (b)[(i)    ] = (uint8) ( (n) >> 24 );       \
    (b)[(i) + 1] = (uint8) ( (n) >> 16 );       \
    (b)[(i) + 2] = (uint8) ( (n) >>  8 );       \
    (b)[(i) + 3] = (uint8) ( (n)       );       \
}

/*----------------------------------------------------------------------*/

static void sha256_starts( sha256_context *ctx )
{
    ctx->total[0] = 0;
    ctx->total[1] = 0;

    ctx->state[0] = 0x6A09E667;
    ctx->state[1] = 0xBB67AE85;
    ctx->state[2] = 0x3C6EF372;
    ctx->state[3] = 0xA54FF53A;
    ctx->state[4] = 0x510E527F;
    ctx->state[5] = 0x9B05688C;
    ctx->state[6] = 0x1F83D9AB;
    ctx->state[7] = 0x5BE0CD19;
}

/*----------------------------------------------------------------------*/

static void sha256_process( sha256_context *ctx, uint8 data[64] )
{
    uint32 temp1, temp2, W[64];
    uint32 A, B, C, D, E, F, G, H;

    GET_UINT32( W[0],  data,  0 );
    GET_UINT32( W[1],  data,  4 );
    GET_UINT32( W[2],  data,  8 );
    GET_UINT32( W[3],  data, 12 );
    GET_UINT32( W[4],  data, 16 );
    GET_UINT32( W[5],  data, 20 );
    GET_UINT32( W[6],  data, 24 );
    GET_UINT32( W[7],  data, 28 );
    GET_UINT32( W[8],  data, 32 );
    GET_UINT32( W[9],  data, 36 );
    GET_UINT32( W[10], data, 40 );
    GET_UINT32( W[11], data, 44 );
    GET_UINT32( W[12], data, 48 );
    GET_UINT32( W[13], data, 52 );
    GET_UINT32( W[14], data, 56 );
    GET_UINT32( W[15], data, 60 );

#define  SHR(x,n) ((x & 0xFFFFFFFF) >> n)
#define ROTR(x,n) (SHR(x,n) | (x << (32 - n)))

#undef  S0
#undef  S1
#define S0(x) (ROTR(x, 7) ^ ROTR(x,18) ^  SHR(x, 3))
#define S1(x) (ROTR(x,17) ^ ROTR(x,19) ^  SHR(x,10))

#undef  S2
#undef  S3
#define S2(x) (ROTR(x, 2) ^ ROTR(x,13) ^ ROTR(x,22))
#define S3(x) (ROTR(x, 6) ^ ROTR(x,11) ^ ROTR(x,25))

#undef  F0
#undef  F1
#define F0(x,y,z) ((x & y) | (z & (x | y)))
#define F1(x,y,z) (z ^ (x & (y ^ z)))

#define R(t)                                    \
(                                               \
    W[t] = S1(W[t -  2]) + W[t -  7] +          \
           S0(W[t - 15]) + W[t - 16]            \
)

#define P(a,b,c,d,e,f,g,h,x,K)                  \
{                                               \
    temp1 = h + S3(e) + F1(e,f,g) + K + x;      \
    temp2 = S2(a) + F0(a,b,c);                  \
    d += temp1; h = temp1 + temp2;              \
}

    A = ctx->state[0];
    B = ctx->state[1];
    C = ctx->state[2];
    D = ctx->state[3];
    E = ctx->state[4];
    F = ctx->state[5];
    G = ctx->state[6];
    H = ctx->state[7];

    P( A, B, C, D, E, F, G, H, W[ 0], 0x428A2F98 );
    P( H, A, B, C, D, E, F, G, W[ 1], 0x71374491 );
    P( G, H, A, B, C, D, E, F, W[ 2], 0xB5C0FBCF );
    P( F, G, H, A, B, C, D, E, W[ 3], 0xE9B5DBA5 );
    P( E, F, G, H, A, B, C, D, W[ 4], 0x3956C25B );
    P( D, E, F, G, H, A, B, C, W[ 5], 0x59F111F1 );
    P( C, D, E, F, G, H, A, B, W[ 6], 0x923F82A4 );
    P( B, C, D, E, F, G, H, A, W[ 7], 0xAB1C5ED5 );
    P( A, B, C, D, E, F, G, H, W[ 8], 0xD807AA98 );
    P( H, A, B, C, D, E, F, G, W[ 9], 0x12835B01 );
    P( G, H, A, B, C, D, E, F, W[10], 0x243185BE );
    P( F, G, H, A, B, C, D, E, W[11], 0x550C7DC3 );
    P( E, F, G, H, A, B, C, D, W[12], 0x72BE5D74 );
    P( D, E, F, G, H, A, B, C, W[13], 0x80DEB1FE );
    P( C, D, E, F, G, H, A, B, W[14], 0x9BDC06A7 );
    P( B, C, D, E, F, G, H, A, W[15], 0xC19BF174 );
    P( A, B, C, D, E, F, G, H, R(16), 0xE49B69C1 );
    P( H, A, B, C, D, E, F, G, R(17), 0xEFBE4786 );
    P( G, H, A, B, C, D, E, F, R(18), 0x0FC19DC6 );
    P( F, G, H, A, B, C, D, E, R(19), 0x240CA1CC );
    P( E, F, G, H, A, B, C, D, R(20), 0x2DE92C6F );
    P( D, E, F, G, H, A, B, C, R(21), 0x4A7484AA );
    P( C, D, E, F, G, H, A, B, R(22), 0x5CB0A9DC );
    P( B, C, D, E, F, G, H, A, R(23), 0x76F988DA );
    P( A, B, C, D, E, F, G, H, R(24), 0x983E5152 );
    P( H, A, B, C, D, E, F, G, R(25), 0xA831C66D );
    P( G, H, A, B, C, D, E, F, R(26), 0xB00327C8 );
    P( F, G, H, A, B, C, D, E, R(27), 0xBF597FC7 );
    P( E, F, G, H, A, B, C, D, R(28), 0xC6E00BF3 );
    P( D, E, F, G, H, A, B, C, R(29), 0xD5A79147 );
    P( C, D, E, F, G, H, A, B, R(30), 0x06CA6351 );
    P( B, C, D, E, F, G, H, A, R(31), 0x14292967 );
    P( A, B, C, D, E, F, G, H, R(32), 0x27B70A85 );
    P( H, A, B, C, D, E, F, G, R(33), 0x2E1B2138 );
    P( G, H, A, B, C, D, E, F, R(34), 0x4D2C6DFC );
    P( F, G, H, A, B, C, D, E, R(35), 0x53380D13 );
    P( E, F, G, H, A, B, C, D, R(36), 0x650A7354 );
    P( D, E, F, G, H, A, B, C, R(37), 0x766A0ABB );
    P( C, D, E, F, G, H, A, B, R(38), 0x81C2C92E );
    P( B, C, D, E, F, G, H, A, R(39), 0x92722C85 );
    P( A, B, C, D, E, F, G, H, R(40), 0xA2BFE8A1 );
    P( H, A, B, C, D, E, F, G, R(41), 0xA81A664B );
    P( G, H, A, B, C, D, E, F, R(42), 0xC24B8B70 );
    P( F, G, H, A, B, C, D, E, R(43), 0xC76C51A3 );
    P( E, F, G, H, A, B, C, D, R(44), 0xD192E819 );
    P( D, E, F, G, H, A, B, C, R(45), 0xD6990624 );
    P( C, D, E, F, G, H, A, B, R(46), 0xF40E3585 );
    P( B, C, D, E, F, G, H, A, R(47), 0x106AA070 );
    P( A, B, C, D, E, F, G, H, R(48), 0x19A4C116 );
    P( H, A, B, C, D, E, F, G, R(49), 0x1E376C08 );
    P( G, H, A, B, C, D, E, F, R(50), 0x2748774C );
    P( F, G, H, A, B, C, D, E, R(51), 0x34B0BCB5 );
    P( E, F, G, H, A, B, C, D, R(52), 0x391C0CB3 );
    P( D, E, F, G, H, A, B, C, R(53), 0x4ED8AA4A );
    P( C, D, E, F, G, H, A, B, R(54), 0x5B9CCA4F );
    P( B, C, D, E, F, G, H, A, R(55), 0x682E6FF3 );
    P( A, B, C, D, E, F, G, H, R(56), 0x748F82EE );
    P( H, A, B, C, D, E, F, G, R(57), 0x78A5636F );
    P( G, H, A, B, C, D, E, F, R(58), 0x84C87814 );
    P( F, G, H, A, B, C, D, E, R(59), 0x8CC70208 );
    P( E, F, G, H, A, B, C, D, R(60), 0x90BEFFFA );
    P( D, E, F, G, H, A, B, C, R(61), 0xA4506CEB );
    P( C, D, E, F, G, H, A, B, R(62), 0xBEF9A3F7 );
    P( B, C, D, E, F, G, H, A, R(63), 0xC67178F2 );

    ctx->state[0] += A;
    ctx->state[1] += B;
    ctx->state[2] += C;
    ctx->state[3] += D;
    ctx->state[4] += E;
    ctx->state[5] += F;
    ctx->state[6] += G;
    ctx->state[7] += H;
}

/*----------------------------------------------------------------------*/

static void sha256_update( sha256_context *ctx, uint8 *input, uint32 length )
{
    uint32 left, fill;

    if( ! length ) return;

    left = ctx->total[0] & 0x3F;
    fill = 64 - left;

    ctx->total[0] += length;
    ctx->total[0] &= 0xFFFFFFFF;

    if( ctx->total[0] < length )
        ctx->total[1]++;

    if( left && length >= fill )
    {
        memcpy( (void *) (ctx->buffer + left),
                (void *) input, fill );
        sha256_process( ctx, ctx->buffer );
        length -= fill;
        input  += fill;
        left = 0;
    }

    while( length >= 64 )
    {
        sha256_process( ctx, input );
        length -= 64;
        input  += 64;
    }

    if( length )
    {
        memcpy( (void *) (ctx->buffer + left),
                (void *) input, length );
    }
}

/*----------------------------------------------------------------------*/

static uint8 sha256_padding[64] =
{
 0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/*----------------------------------------------------------------------*/

static void sha256_finish( sha256_context *ctx, uint8 digest[32] )
{
    uint32 last, padn;
    uint32 high, low;
    uint8 msglen[8];

    high = ( ctx->total[0] >> 29 )
         | ( ctx->total[1] <<  3 );
    low  = ( ctx->total[0] <<  3 );

    PUT_UINT32( high, msglen, 0 );
    PUT_UINT32( low,  msglen, 4 );

    last = ctx->total[0] & 0x3F;
    padn = ( last < 56 ) ? ( 56 - last ) : ( 120 - last );

    sha256_update( ctx, sha256_padding, padn );
    sha256_update( ctx, msglen, 8 );

    PUT_UINT32( ctx->state[0], digest,  0 );
    PUT_UINT32( ctx->state[1], digest,  4 );
    PUT_UINT32( ctx->state[2], digest,  8 );
    PUT_UINT32( ctx->state[3], digest, 12 );
    PUT_UINT32( ctx->state[4], digest, 16 );
    PUT_UINT32( ctx->state[5], digest, 20 );
    PUT_UINT32( ctx->state[6], digest, 24 );
    PUT_UINT32( ctx->state[7], digest, 28 );
}

/*----------------------------------------------------------------------*/

void SHA256_sum( int n , char *bytes , unsigned char *sum )
{
    sha256_context ctx;

    if( bytes == NULL || sum == NULL ) return ;
    if( n < 0 ) n = 0 ;

    sha256_starts( &ctx );
    sha256_update( &ctx , (uint8 *)bytes , (uint32)n ) ;
    sha256_finish( &ctx, sum );
}
