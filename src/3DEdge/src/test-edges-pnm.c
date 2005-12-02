/*************************************************************************
 * test-edges-pnm.c - example program of edges detection
 *
 * $Id$
 *
 * Copyright©INRIA 1999
 * DESCRIPTION: 
 *
 * Input must be of PBM/PGM/PPM raw format, output will be the same.
 *
 *
 * AUTHOR:
 * Gregoire Malandain (greg@sophia.inria.fr)
 * 
 * CREATION DATE: 
 * June, 9 1998
 *
 * Copyright Gregoire Malandain, INRIA
 *
 * ADDITIONS, CHANGES
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <iopnm.h>
#include <extrema.h>
#include <zcross.h>


typedef enum {
  _GRADIENT_EXTREMA_,
  _GRADIENT_MODULUS_,
  _LAPLACIAN_,
  _GRADIENT_LAPLACIAN_,
  _HESSIAN_,
  _GRADIENT_HESSIAN_
} enumOutput;



static char program[256];
static char *usage = "image-in image-out [-a %f] [-s %f] [-v] [-help]";
static char *details ="\n\
Edge detection\n\
--------------\n\
Edges are the maxima of the gradient modulus in the direction\n\
of the gradient.\n\
\n\
\t -a | -alpha : specifies the alpha of Deriche's recursive filters\n\
\t -s | -sigma : specifies the sigma of a recursive approximation\n\
\t               of the gaussian\n\
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
  int borderLengths[3] = {10,10,10};
  float filterCoefs[3] = {1.0, 1.0, 1.0};
  float c = 1.0;
  recursiveFilterType filterType = ALPHA_DERICHE;
  
  enumOutput typeOutput = _GRADIENT_EXTREMA_;

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
	GradientExtrema_verbose();
	ZeroCrossings_verbose ( );
      }

      else if ( (strcmp ( argv[i], "-no-verbose" ) == 0) || 
		(strcmp ( argv[i], "-nv" ) == 0) ) {
	GradientExtrema_noverbose();
      }

      else if ( strcmp ( argv[i], "-neg" ) == 0 ) {
	ZeroCrossings_Are_Negative();
      }
      else if ( strcmp ( argv[i], "-pos" ) == 0 ) {
	ZeroCrossings_Are_Positive();
      }


      else if ( strcmp ( argv[i], "-gradient" ) == 0 ) {
	typeOutput = _GRADIENT_MODULUS_;
      }
      else if ( strcmp ( argv[i], "-laplacian" ) == 0 ) {
	typeOutput = _LAPLACIAN_;
      }
      else if ( strcmp ( argv[i], "-gradient-laplacian" ) == 0 ) {
	typeOutput = _GRADIENT_LAPLACIAN_;
      }
      else if ( strcmp ( argv[i], "-hessian" ) == 0 ) {
	typeOutput = _HESSIAN_;
      }
      else if ( strcmp ( argv[i], "-gradient-hessian" ) == 0 ) {
	typeOutput = _GRADIENT_HESSIAN_;
      }

      else if ( (strcmp ( argv[i], "-alpha" ) == 0) || 
		(strcmp ( argv[i], "-a" ) == 0) ) {
	i += 1;
	if ( i >= argc)    ErrorMessage( "parsing -alpha...\n", 0 );
	status = sscanf( argv[i], "%f", &c );
	if ( status <= 0 ) ErrorMessage( "parsing -alpha...\n", 0 );
	filterCoefs[0] = filterCoefs[1] = filterCoefs[2] = c;
	filterType = ALPHA_DERICHE;
      }

      else if ( (strcmp ( argv[i], "-sigma" ) == 0) || 
		(strcmp ( argv[i], "-s" ) == 0) ) {
	i += 1;
	if ( i >= argc)    ErrorMessage( "parsing -sigma...\n", 0 );
	status = sscanf( argv[i], "%f", &c );
	if ( status <= 0 ) ErrorMessage( "parsing -sigma...\n", 0 );
	filterCoefs[0] = filterCoefs[1] = filterCoefs[2] = c;
	filterType = GAUSSIAN_DERICHE;
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



  bufferIn = _readPnmImage( nameImageIn, &bufferDims[0], &bufferDims[1], &bufferDims[2] );
  
  bufferOut = (void*)malloc( bufferDims[0] * bufferDims[1] * bufferDims[2] * sizeof(unsigned char) );


  fprintf( stderr, "%s: processing with coefficient = %f\n", argv[0], filterCoefs[0] );



  switch( typeOutput ) {
  default :
  case _GRADIENT_EXTREMA_ :
    if ( Extract_Gradient_Maxima_2D( bufferIn, UCHAR,
				     bufferOut, UCHAR,
				     bufferDims,
				     borderLengths,
				     filterCoefs,
				     filterType ) == 0 ) {
      fprintf( stderr, " processing failed.\n" );
      exit( 1 );
    }
    break;
  case _GRADIENT_MODULUS_ :
    if ( GradientModulus( bufferIn, UCHAR, bufferOut, UCHAR,
			  bufferDims, borderLengths, filterCoefs,
			  filterType ) == 0 ) {
      fprintf( stderr, " processing failed.\n" );
      exit( 1 );
    }
    break;
  case _LAPLACIAN_ :
    if ( Laplacian( bufferIn, UCHAR,
		       bufferOut, UCHAR,
		       bufferDims,
		       borderLengths,
		       filterCoefs,
		       filterType ) == 0 ) {
      fprintf( stderr, " processing failed.\n" );
      exit( 1 );
    }
    break;
  case _GRADIENT_LAPLACIAN_ :
    if ( Gradient_On_Laplacian_ZeroCrossings_2D( bufferIn, UCHAR,
		       bufferOut, UCHAR,
		       bufferDims,
		       borderLengths,
		       filterCoefs,
		       filterType ) == 0 ) {
      fprintf( stderr, " processing failed.\n" );
      exit( 1 );
    }
    break;
  case _HESSIAN_ :
    if ( GradientHessianGradient( bufferIn, UCHAR,
		       bufferOut, UCHAR,
		       bufferDims,
		       borderLengths,
		       filterCoefs,
		       filterType ) == 0 ) {
      fprintf( stderr, " processing failed.\n" );
      exit( 1 );
    }
    break;
  case _GRADIENT_HESSIAN_ :
    if ( Gradient_On_GradientHessianGradient_ZeroCrossings_2D( bufferIn, UCHAR,
		       bufferOut, UCHAR,
		       bufferDims,
		       borderLengths,
		       filterCoefs,
		       filterType ) == 0 ) {
      fprintf( stderr, " processing failed.\n" );
      exit( 1 );
    }
    break;
  }

  _writePnmImage( nameImageOut, bufferDims[0], bufferDims[1], bufferDims[2], bufferOut );
  
}
