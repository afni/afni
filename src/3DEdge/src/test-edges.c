/*************************************************************************
 * test-edges.c - example program of edges detection
 *
 * $Id$
 *
 * LICENSE:
 * GPL v3.0 (see gpl-3.0.txt for details)
 *
 * DESCRIPTION:
 *
 *
 * AUTHOR:
 * Gregoire Malandain (gregoire.malandain@inria.fr)
 *
 * CREATION DATE:
 * June, 9 1998
 *
 * ADDITIONS, CHANGES
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <fcntl.h> /* open, close */
#include <sys/stat.h> /* open, close */
#include <sys/types.h> /* open, close */
#include <unistd.h> /* write */

#include <time.h>

#include <extrema.h>

int main( int argc, char* argv[] )
{
  void *bufferIn = (void*)NULL;
  void *bufferOut = (void*)NULL;
  int bufferDims[3] = {0,0,0};
  int borderLengths[3] = {0,0,0};
  float filterCoefs[3] = {1.0, 1.0, 1.0};
  recursiveFilterType filterType = ALPHA_DERICHE;
  /* recursiveFilterType filterType = GAUSSIAN_DERICHE; */
  int fd;
  int bufferLength;
  int example;
  int t0, t1, t2, t3;

  t0 = clock( );



  /*
   * 2D image, example of use
   */


  /*
   * allocation of a 64 x 64 image
   */
  example = 1;
  bufferLength = 4096;
  bufferIn = (void*)malloc( bufferLength * sizeof(unsigned char) );
  if ( bufferIn == (void*)NULL ) {
    fprintf( stderr, " example #%d: allocation of first buffer failed.\n", example );
    exit( 1 );
  }
  bufferOut = (void*)malloc( bufferLength * sizeof(unsigned char) );
  if ( bufferOut == (void*)NULL ) {
    fprintf( stderr, " example #%d: allocation of second buffer failed.\n", example );
    exit( 1 );
  }

  /*
   * reading the 2D image (raw data)
   */
  fd = open( "images/square64x64.data", 0 );
  {
    char *b = (char*)bufferIn;
    int toberead = bufferLength;
    int nbread = 0;
    while ( (toberead > 0) && ((nbread = read( fd, b, toberead )) > 0) ) {
      toberead -= nbread;
      b += nbread;
    }
    if ( toberead > 0 ) {
      fprintf( stderr, " example #%d: read %d bytes instead of %d.\n",
	       example, (bufferLength- toberead), bufferLength );
      exit( 1 );
    }
  }
  close( fd );

  /*
   * processing the 2D image
   */
  bufferDims[0] = 64;
  bufferDims[1] = 64;
  bufferDims[2] = 1;
  if ( Extract_Gradient_Maxima_2D( bufferIn, UCHAR,
				   bufferOut, UCHAR,
				   bufferDims,
				   borderLengths,
				   filterCoefs,
				   filterType ) == 0 ) {
    fprintf( stderr, " example #%d: processing failed.\n", example );
    exit( 1 );
  }

  /*
   * writing the 2D image (raw data)
   */
  fd = creat( "images/square64x64.tmp", 0644 );
  if ( write( fd, bufferOut, bufferLength ) != bufferLength ) {
    fprintf( stderr, " example #%d: error in writing.\n", example );
    exit( 1 );
  }
  close( fd );

  /*
   *
   */
  fprintf( stderr, " example #%d: processing is complete.\n", example );
  fprintf( stderr, "             image/square64x64.tmp was written.\n");
  free( bufferIn );
  free( bufferOut );
  t1 = clock( );
  fprintf( stderr, "             processing time was %d microseconds.\n", t1-t0 );



  /*
   * 3D image, example of use
   */


  /*
   * allocation of a 64 x 64 x 64 image
   */
  example = 2;
  bufferLength = 262144;
  bufferIn = (void*)malloc( bufferLength * sizeof(unsigned char) );
  if ( bufferIn == (void*)NULL ) {
    fprintf( stderr, " example #%d: allocation of first buffer failed.\n", example );
    exit( 1 );
  }
  bufferOut = (void*)malloc( bufferLength * sizeof(unsigned char) );
  if ( bufferOut == (void*)NULL ) {
    fprintf( stderr, " example #%d: allocation of second buffer failed.\n", example );
    exit( 1 );
  }

  /*
   * reading the 3D image (raw data)
   */
  fd = open( "images/disk64x64x64.data", 0 );
  {
    char *b = (char*)bufferIn;
    int toberead = bufferLength;
    int nbread = 0;
    while ( (toberead > 0) && ((nbread = read( fd, b, toberead )) > 0) ) {
      toberead -= nbread;
      b += nbread;
    }
    if ( toberead > 0 ) {
      fprintf( stderr, " example #%d: read %d bytes instead of %d.\n",
	       example, (bufferLength- toberead), bufferLength );
      exit( 1 );
    }
  }
  close( fd );

  /*
   * processing the 3D image
   */
  bufferDims[0] = 64;
  bufferDims[1] = 64;
  bufferDims[2] = 64;
  if ( Extract_Gradient_Maxima_3D( bufferIn, UCHAR,
				   bufferOut, UCHAR,
				   bufferDims,
				   borderLengths,
				   filterCoefs,
				   filterType ) == 0 ) {
    fprintf( stderr, " example #%d: processing failed.\n", example );
    exit( 1 );
  }

  /*
   * writing the 3D image (raw data)
   */
  fd = creat( "images/disk64x64x64.tmp", 0644 );
  if ( write( fd, bufferOut, bufferLength ) != bufferLength ) {
    fprintf( stderr, " example #%d: error in writing.\n", example );
    exit( 1 );
  }
  close( fd );

  /*
   *
   */
  fprintf( stderr, " example #%d: processing is complete.\n", example );
  fprintf( stderr, "             image/disk64x64x64.tmp was written.\n");
  free( bufferIn );
  free( bufferOut );
  t2 = clock( );
  fprintf( stderr, "             processing time was %d microseconds.\n", t2-t1 );



  /*
   * 3D image, second example of use
   */


  /*
   * allocation of a 64 x 64 x 64 image
   */
  example = 3;
  bufferLength = 262144;
  bufferIn = (void*)malloc( bufferLength * sizeof(unsigned char) );
  if ( bufferIn == (void*)NULL ) {
    fprintf( stderr, " example #%d: allocation of first buffer failed.\n", example );
    exit( 1 );
  }
  bufferOut = (void*)malloc( bufferLength * sizeof(unsigned char) );
  if ( bufferOut == (void*)NULL ) {
    fprintf( stderr, " example #%d: allocation of second buffer failed.\n", example );
    exit( 1 );
  }

  /*
   * reading the image
   */
  fd = open( "images/mri64x64x64.data", 0 );
  {
    char *b = (char*)bufferIn;
    int toberead = bufferLength;
    int nbread = 0;
    while ( (toberead > 0) && ((nbread = read( fd, b, toberead )) > 0) ) {
      toberead -= nbread;
      b += nbread;
    }
    if ( toberead > 0 ) {
      fprintf( stderr, " example #%d: read %d bytes instead of %d.\n",
	       example, (bufferLength- toberead), bufferLength );
      exit( 1 );
    }
  }
  close( fd );

  /*
   * processing
   */
  bufferDims[0] = 64;
  bufferDims[1] = 64;
  bufferDims[2] = 64;
  /*
   * here it is better (even necessary) to add
   * points at both ends of lines when filetring.
   */
  borderLengths[0] = 10;
  borderLengths[1] = 10;
  borderLengths[2] = 10;
  if ( Extract_Gradient_Maxima_3D( bufferIn, UCHAR,
				   bufferOut, UCHAR,
				   bufferDims,
				   borderLengths,
				   filterCoefs,
				   filterType ) == 0 ) {
    fprintf( stderr, " example #%d: processing failed.\n", example );
    exit( 1 );
  }

  /*
   * writing
   */
  fd = creat( "images/mri64x64x64.tmp", 0644 );
  if ( write( fd, bufferOut, bufferLength ) != bufferLength ) {
    fprintf( stderr, " example #%d: error in writing.\n", example );
    exit( 1 );
  }
  close( fd );

  /*
   *
   */
  fprintf( stderr, " example #%d: processing is complete.\n", example );
  fprintf( stderr, "             image/mri64x64x64.tmp was written.\n");
  free( bufferIn );
  free( bufferOut );
  t3 = clock( );
  fprintf( stderr, "             processing time was %d microseconds.\n", t3-t2 );


  /*
   * ZSS: 3D image, third example of use
   */


  /*
   * allocation of a 256x256x256 image
   */
  example = 4;
  bufferLength = 256*256*256;
  bufferIn = (void*)malloc( bufferLength * sizeof(unsigned char) );
  if ( bufferIn == (void*)NULL ) {
    fprintf( stderr, " example #%d: allocation of first buffer failed.\n", example );
    exit( 1 );
  }
  bufferOut = (void*)malloc( bufferLength * sizeof(unsigned char) );
  if ( bufferOut == (void*)NULL ) {
    fprintf( stderr, " example #%d: allocation of second buffer failed.\n", example );
    exit( 1 );
  }

  /*
   * reading the image
   */
  fd = open( "images/DemoSubj_SurfVol+orig.BRIK", 0 );
  {
    char *b = (char*)bufferIn;
    int toberead = bufferLength;
    int nbread = 0;
    while ( (toberead > 0) && ((nbread = read( fd, b, toberead )) > 0) ) {
      toberead -= nbread;
      b += nbread;
    }
    if ( toberead > 0 ) {
      fprintf( stderr, " example #%d: read %d bytes instead of %d.\n",
	       example, (bufferLength- toberead), bufferLength );
      exit( 1 );
    }
  }
  close( fd );

  /*
   * processing
   */
  bufferDims[0] = 256;
  bufferDims[1] = 256;
  bufferDims[2] = 256;
  /*
   * here it is better (even necessary) to add
   * points at both ends of lines when filetring.
   */
  borderLengths[0] = 50;
  borderLengths[1] = 50;
  borderLengths[2] = 50;
  if ( Extract_Gradient_Maxima_3D( bufferIn, UCHAR,
				   bufferOut, UCHAR,
				   bufferDims,
				   borderLengths,
				   filterCoefs,
				   filterType ) == 0 ) {
    fprintf( stderr, " example #%d: processing failed.\n", example );
    exit( 1 );
  }

  /*
   * writing
   */
  fd = creat( "images/DemoSubj_SurfVol_Edge+orig.BRIK", 0644 );
  if ( write( fd, bufferOut, bufferLength ) != bufferLength ) {
    fprintf( stderr, " example #%d: error in writing.\n", example );
    exit( 1 );
  }
  close( fd );


  /*
   *
   */
  fprintf( stderr, " example #%d: processing is complete.\n", example );
  fprintf( stderr, "             images/DemoSubj_SurfVol_Edge+orig.BRIK was written.\n");
  free( bufferIn );
  free( bufferOut );
  t3 = clock( );
  fprintf( stderr, "             processing time was %d microseconds.\n", t3-t2 );


  /*
   *
   */
  exit( 0 );
}
