/*************************************************************************
 * test-hyster-pnm.c - example program of hystersis thresholding
 *
 * $Id$
 *
 * LICENSE:
 * GPL v3.0 (see gpl-3.0.txt for details)
 *
 * DESCRIPTION:
 *
 * Input must be of PBM/PGM/PPM raw format, output will be the same.
 *
 *
 * AUTHOR:
 * Gregoire Malandain (gregoire.malandain@inria.fr)
 *
 * CREATION DATE:
 * Thu Oct  7 22:56:37 MET DST 1999
 *
 * ADDITIONS, CHANGES
 *
 * * Tue Dec  7 12:06:32 MET 1999 (G. Malandain)
 *   - parameters parsing
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <iopnm.h>
#include <connexe.h>



static char program[256];
static char *usage = "image-in image-out [-lt %lf] [-ht %lf] [-ms %d] [-v] [-help]";
static char *details ="\n\
Hysteresis thresholding\n\
-----------------------\n\
The image is thresholded with the 'low threshold'. A resulting\n\
connected component is said to be valid if it contains at least\n\
one point above the 'high threshold', and at least\n\
' minimal size' points.\n\
\n\
\t -lt | -low-threshold : specifies the low threshold\n\
\t -ht | -high-threshold : specifies the high threshold\n\
\t -ms | -minimal-size : specifies the minimal size\n\
\t -v | -verbose\n\
\t -nv | -no-verbose\n\
\t -h | -help : print this message";

static void ErrorMessage( char *str, int flag )
{
  (void)fprintf(stderr,"Usage: %s %s\n",program, usage);
  if ( flag == 1 ) (void)fprintf(stderr,"%s",details);
  (void)fprintf(stderr,"Error: %s",str);
  exit(0);
}







int main( int argc, char* argv[] )
{
  char nameImageIn[256];
  char nameImageOut[256];
  int nbNames = 0;
  int status, i;

  void *bufferIn = (void*)NULL;
  void *bufferOut = (void*)NULL;
  int bufferDims[3] = {0,0,0};
  int nbytes;
  bufferType TYPE = UCHAR;
  double low = 1.0;
  double high = 1.0;
  int size = 1;

  strcpy( program, argv[0] );

  if ( argc == 1 ) ErrorMessage( "\n", 1 );

  for ( i=1; i<argc; i++ ) {
    if ( argv[i][0] == '-' ) {

      if ( (strcmp ( argv[i], "-help" ) == 0) ||
	   (strcmp ( argv[i], "-h" ) == 0) ) {
	ErrorMessage( "help message\n", 1 );
      }

      else if ( (strcmp ( argv[i], "-verbose" ) == 0) ||
		(strcmp ( argv[i], "-v" ) == 0) ) {
	Connexe_verbose();
      }

      else if ( (strcmp ( argv[i], "-no-verbose" ) == 0) ||
		(strcmp ( argv[i], "-nv" ) == 0) ) {
	Connexe_noverbose();
      }

      else if ( (strcmp ( argv[i], "-low-threshold" ) == 0) ||
		(strcmp ( argv[i], "-lt" ) == 0) ) {
	i += 1;
	if ( i >= argc)    ErrorMessage( "parsing -low-threshold...\n", 0 );
	status = sscanf( argv[i], "%lf", &low );
	if ( status <= 0 ) ErrorMessage( "parsing -low-threshold...\n", 0 );
      }

      else if ( (strcmp ( argv[i], "-high-threshold" ) == 0) ||
		(strcmp ( argv[i], "-ht" ) == 0) ) {
	i += 1;
	if ( i >= argc)    ErrorMessage( "parsing -high-threshold...\n", 0 );
	status = sscanf( argv[i],"%lf", &high );
	if ( status <= 0 ) ErrorMessage( "parsing -high-threshold...\n", 0 );
      }

      else if ( (strcmp ( argv[i], "-minimal-size" ) == 0) ||
		(strcmp ( argv[i], "-ms" ) == 0) ) {
	i += 1;
	if ( i >= argc)    ErrorMessage( "parsing -minimal-size...\n", 0 );
	status = sscanf( argv[i],"%d", &size );
	if ( status <= 0 ) ErrorMessage( "parsing -minimal-size...\n", 0 );
      }

      else {
	sprintf( nameImageIn, "unknown option %s\n", argv[i] );
	ErrorMessage( nameImageIn, 0);
      }
    }

    else if ( argv[i][0] != 0 ) {
      if ( nbNames == 0 ) {
	strcpy( nameImageIn, argv[i] );
      }
      else if ( nbNames == 1 ) {
	strcpy( nameImageOut, argv[i] );
      }
      else {
	sprintf( nameImageIn, "too many image name (%s)\n", argv[i] );
	ErrorMessage( nameImageIn, 0);
      }
      nbNames ++;
    }
  }


  bufferIn = _readPnmImage( nameImageIn, &bufferDims[0], &bufferDims[1], &bufferDims[2], &nbytes );
  if ( nbytes == 2 ) TYPE = USHORT;

  bufferOut = (void*)malloc( bufferDims[0] * bufferDims[1] * bufferDims[2] * sizeof(unsigned char) );


  fprintf( stderr, "%s: processing with thresholds = %f %f\n", argv[0], low, high );
  /*
   * to change the connectivity -> Connexe_SetConnectivity
   * to change the minimal size of the connected components to be kept.
   *                            -> Connexe_SetMinimumSizeOfComponents
   * to change the maximum number of the connected components to be kept.
   *                            -> Connexe_SetMaximumNumberOfComponents
   */
  Connexe_SetMinimumSizeOfComponents( size );
  if ( HysteresisThresholding( bufferIn, TYPE,
			       bufferOut, UCHAR,
			       bufferDims,
			       low, high ) < 0 ) {
    fprintf( stderr, " processing failed.\n" );
    exit( 1 );
  }

  /* there exists a more complete function (with more parameters)
     => HysteresisThresholdingWithAllParams()
     see connexe.h
  */


  _writePnmImage( nameImageOut, bufferDims[0], bufferDims[1], bufferDims[2], 1, bufferOut );

}
