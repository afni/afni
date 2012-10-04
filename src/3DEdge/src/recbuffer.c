/*************************************************************************
 * recbuffer.c - tools for recursive filtering of 3D and 2D image buffers
 *
 * $Id$
 *
 * LICENSE:
 * GPL v3.0 (see gpl-3.0.txt for details)
 *
 * DESCRIPTION: 
 *
 * recursive filtering of a buffer (a [1,2,3]D array)
 * according that the filtering is separable
 *
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
 * * Jul 6 1999 (Gregoire Malandain)
 *   a bug in RecursiveFilterOnBuffer (*&%^@$^ cut and paste)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>



#include <convert.h>
#include <recbuffer.h>

static int _VERBOSE_ = 0;

#define EXIT_ON_FAILURE 0
#define EXIT_ON_SUCCESS 1










/*
 *
 * Gradient modulus
 *
 *
 */

int GradientModulus( void *bufferIn,
		     bufferType typeIn,
		     void *bufferOut,
		     bufferType typeOut,
		     int *bufferDims,
		     int *borderLengths,
		     float *filterCoefs,
		     recursiveFilterType filterType )
{
  char *proc = "GradientModulus";
  float *auxBuf = NULL;
  float *tmpBuf = NULL, *grdBuf = NULL;
  int sizeAuxBuf = 0;
  int derivatives[3];
  int i;


  sizeAuxBuf = bufferDims[0] * bufferDims[1] * bufferDims[2];
  if ( typeOut != FLOAT || bufferIn == bufferOut )
    sizeAuxBuf *= 2;


  /* allocation des buffers de calcul
   */
  auxBuf = (float*)malloc( sizeAuxBuf * sizeof(float) );
  if ( auxBuf == NULL ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, "%s: unable to allocate auxiliary buffer\n", proc );
    return( EXIT_ON_FAILURE );
  }
  tmpBuf = auxBuf;
  if ( typeOut != FLOAT || bufferIn == bufferOut ) {
    grdBuf  = tmpBuf;
    grdBuf += bufferDims[0] * bufferDims[1] * bufferDims[2];
  } else {
    grdBuf  = (float*)bufferOut;
  }
  
  /* cas 2D
   */
  if ( bufferDims[2] == 1 ) {

    derivatives[0] = DERIVATIVE_1; 
    derivatives[1] = DERIVATIVE_0;
    derivatives[2] = NODERIVATIVE;
    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, (void*)grdBuf, FLOAT,
				  bufferDims, borderLengths, derivatives,
				  filterCoefs, filterType ) != EXIT_ON_SUCCESS ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: unable to compute X derivative (2D)\n", proc );
      free( auxBuf );
      return( EXIT_ON_FAILURE );
    }

    derivatives[0] = DERIVATIVE_0; 
    derivatives[1] = DERIVATIVE_1;
    derivatives[2] = NODERIVATIVE;
    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, (void*)tmpBuf, FLOAT,
				  bufferDims, borderLengths, derivatives,
				  filterCoefs, filterType ) != EXIT_ON_SUCCESS ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: unable to compute Y derivative (2D)\n", proc );
      free( auxBuf );
      return( EXIT_ON_FAILURE );
    }
    
    sizeAuxBuf = bufferDims[0] * bufferDims[1] * bufferDims[2];
    for ( i = 0; i < sizeAuxBuf; i++ )
      grdBuf[i] = (float)sqrt( grdBuf[i]*grdBuf[i] + tmpBuf[i]*tmpBuf[i] );
    
  } else {
    
    derivatives[0] = NODERIVATIVE;
    derivatives[1] = NODERIVATIVE;
    derivatives[2] = DERIVATIVE_0;
    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, (void*)tmpBuf, FLOAT,
				  bufferDims, borderLengths, derivatives,
				  filterCoefs, filterType ) != EXIT_ON_SUCCESS ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: unable to compute Z smoothing (3D)\n", proc );
      free( auxBuf );
      return( EXIT_ON_FAILURE );
    }

    derivatives[0] = DERIVATIVE_1; 
    derivatives[1] = DERIVATIVE_0;
    derivatives[2] = NODERIVATIVE;
    if ( RecursiveFilterOnBuffer( (void*)tmpBuf, FLOAT, (void*)grdBuf, FLOAT,
				  bufferDims, borderLengths, derivatives,
				  filterCoefs, filterType ) != EXIT_ON_SUCCESS ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: unable to compute X derivative (3D)\n", proc );
      free( auxBuf );
      return( EXIT_ON_FAILURE );
    }

    derivatives[0] = DERIVATIVE_0; 
    derivatives[1] = DERIVATIVE_1;
    derivatives[2] = NODERIVATIVE;
    if ( RecursiveFilterOnBuffer( (void*)tmpBuf, FLOAT, (void*)tmpBuf, FLOAT,
				  bufferDims, borderLengths, derivatives,
				  filterCoefs, filterType ) != EXIT_ON_SUCCESS ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: unable to compute Y derivative (3D)\n", proc );
      free( auxBuf );
      return( EXIT_ON_FAILURE );
    }

    sizeAuxBuf = bufferDims[0] * bufferDims[1] * bufferDims[2];
    for ( i = 0; i < sizeAuxBuf; i++ )
      grdBuf[i] = grdBuf[i]*grdBuf[i] + tmpBuf[i]*tmpBuf[i];
    
    derivatives[0] = DERIVATIVE_0;
    derivatives[1] = DERIVATIVE_0;
    derivatives[2] = DERIVATIVE_1;
    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, (void*)tmpBuf, FLOAT,
				  bufferDims, borderLengths, derivatives,
				  filterCoefs, filterType ) != EXIT_ON_SUCCESS ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: unable to compute Z derivative (3D)\n", proc );
      free( auxBuf );
      return( EXIT_ON_FAILURE );
    }
    
    for ( i = 0; i < sizeAuxBuf; i++ )
      grdBuf[i] = (float)sqrt( grdBuf[i] + tmpBuf[i]*tmpBuf[i] );
    
  }

  if ( grdBuf != bufferOut ) 
    ConvertBuffer( grdBuf, FLOAT, bufferOut, typeOut,
		   bufferDims[0]*bufferDims[1]*bufferDims[2] );
  free( auxBuf );
  return( EXIT_ON_SUCCESS );
}
























/*
 *
 * Laplacian
 *
 *
 */
int Laplacian_2D ( void *bufferIn,
		   bufferType typeIn,
		   void *bufferOut,
		   bufferType typeOut,
		   int *bufferDims,
		   int *borderLengths,
		   float *filterCoefs,
		   recursiveFilterType filterType )
{
  char *proc = "Laplacian_2D";
  float *theXX = NULL;
  float *theYY = NULL;

  derivativeOrder XXderiv[3] = { DERIVATIVE_2, SMOOTHING, NODERIVATIVE };
  derivativeOrder YYderiv[3] = { SMOOTHING, DERIVATIVE_2, NODERIVATIVE };
  int sliceDims[3];
  int z, i, dimxXdimy;

  void *sliceOut = NULL;



  /* 
   * We check the buffers' dimensions.
   */
  if ( (bufferDims[0] <= 0) || (bufferDims[1] <= 0) || (bufferDims[2] <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  
  /*
   * test of the coefficients
   */
  if ( (filterCoefs[0] < 0.0) || (filterCoefs[1] < 0.0) ||
       (filterCoefs[2] < 0.0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: negative coefficient's value.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  

  /*
   *
   */
  dimxXdimy = bufferDims[0] * bufferDims[1];
  sliceDims[0] = bufferDims[0];
  sliceDims[1] = bufferDims[1];
  sliceDims[2] = 1;
  

  if ( typeOut == FLOAT ) {
    theXX = (float*)malloc( dimxXdimy * sizeof( float ) );
  } else {
    theXX = (float*)malloc( 2 * dimxXdimy * sizeof( float ) );
  }
  
  if ( theXX == NULL ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to allocate auxiliary buffer.\n" );
    }
    return( EXIT_ON_FAILURE );
  }

  if ( typeOut != FLOAT ) {
    theYY  = theXX;
    theYY += dimxXdimy;
  }
  
  
  
  for ( z=0; z<bufferDims[2]; z++ ) {

    if ( typeOut == FLOAT ) {
      theYY = ((float*)bufferOut) + z * dimxXdimy;
    }
    
    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theXX, FLOAT, 
				  sliceDims, borderLengths,
				  XXderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^2 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theYY, FLOAT, 
				  sliceDims, borderLengths,
				  YYderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y^2 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }
    
    
    for ( i=0; i<dimxXdimy; i++ ) theYY[i] += theXX[i];
    if ( typeOut != FLOAT ) {
      switch ( typeOut ) {
      case UCHAR :
	sliceOut = (((u8*)bufferOut) + z * dimxXdimy);
	break;
      case SCHAR :
	sliceOut = (((s8*)bufferOut) + z * dimxXdimy);
	break;
      case SSHORT :
	sliceOut = (((s16*)bufferOut) + z * dimxXdimy);
	break;
      case DOUBLE :
	sliceOut = (((r64*)bufferOut) + z * dimxXdimy);
	break;
      default :
	if ( _VERBOSE_ > 0 )
	  fprintf( stderr, " Error in %s: such output type not handled.\n", proc );
	free( theXX );
	return( EXIT_ON_FAILURE );
      }
      ConvertBuffer( theYY, FLOAT, sliceOut, typeOut, dimxXdimy );
    }
  }

  return( EXIT_ON_SUCCESS );
}














int Laplacian ( void *bufferIn,
		   bufferType typeIn,
		   void *bufferOut,
		   bufferType typeOut,
		   int *bufferDims,
		   int *borderLengths,
		   float *filterCoefs,
		   recursiveFilterType filterType )
{
  char *proc = "Laplacian";
  float *theSL = NULL;
  float *theZZ = NULL;
  float *theZ0 = NULL;


  derivativeOrder XXderiv[3] = { DERIVATIVE_2, SMOOTHING, NODERIVATIVE };
  derivativeOrder YYderiv[3] = { SMOOTHING, DERIVATIVE_2, NODERIVATIVE };
  derivativeOrder Zsmooth[3] = { NODERIVATIVE, NODERIVATIVE, SMOOTHING };
  derivativeOrder ZZderiv[3] = { SMOOTHING, SMOOTHING, DERIVATIVE_2 };
  
  int sliceDims[3];
  int z, i, j, dimxXdimy;




  /* 
   * We check the buffers' dimensions.
   */
  if ( bufferDims[2] == 1 ) {
    return( Laplacian_2D ( bufferIn, typeIn, bufferOut, typeOut,
			   bufferDims, borderLengths, filterCoefs, filterType ) );
  }

  if ( (bufferDims[0] <= 0) || (bufferDims[1] <= 0) || (bufferDims[2] <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  
  /*
   * test of the coefficients
   */
  if ( (filterCoefs[0] < 0.0) || (filterCoefs[1] < 0.0) ||
       (filterCoefs[2] < 0.0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: negative coefficient's value.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  

  /*
   *
   */
  dimxXdimy = bufferDims[0] * bufferDims[1];
  sliceDims[0] = bufferDims[0];
  sliceDims[1] = bufferDims[1];
  sliceDims[2] = 1;

  

  if ( typeOut == FLOAT ) {
    theSL = (float*)malloc( (1+bufferDims[2]) * dimxXdimy * sizeof( float ) );
  } else {
    theSL = (float*)malloc( (1+2*bufferDims[2]) * dimxXdimy * sizeof( float ) );
  }


  
  if ( theSL == NULL ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to allocate auxiliary buffer.\n" );
    }
    return( EXIT_ON_FAILURE );
  }



  theZ0 = theSL;
  theZ0 += dimxXdimy;


  
  if ( typeOut == FLOAT ) {
    theZZ = bufferOut;
  } else {
    theZZ  = theZ0;
    theZZ += dimxXdimy * bufferDims[2];
  }
  
  
  
  /*
   *
   * 3D filtering / filtering along Z
   *
   */

  if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theZ0, FLOAT, 
				bufferDims, borderLengths,
				Zsmooth, filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Z^0 derivative.\n" );
    }
    free( theSL );
    return( EXIT_ON_FAILURE );
  }
  if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theZZ, FLOAT, 
				bufferDims, borderLengths,
				ZZderiv, filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Z^2 derivative.\n" );
    }
    free( theSL );
    return( EXIT_ON_FAILURE );
  }
  





  for ( z=0; z<bufferDims[2]; z++ ) {

    /*
     *
     * 2D filtering / filtering along X and Y
     *
     */

    if ( RecursiveFilterOnBuffer( theZ0+z*dimxXdimy, FLOAT, theSL, FLOAT, 
				  sliceDims, borderLengths,
				  XXderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^2 derivative.\n" );
      }
      free( theSL );
      return( EXIT_ON_FAILURE );
    }

    for ( j=z*dimxXdimy, i=0; i<dimxXdimy; j++, i++ ) {
      theZZ[j] += theSL[i];
    }

    if ( RecursiveFilterOnBuffer( theZ0+z*dimxXdimy, FLOAT, theSL, FLOAT, 
				  sliceDims, borderLengths,
				  YYderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y^2 derivative.\n" );
      }
      free( theSL );
      return( EXIT_ON_FAILURE );
    }
    
    for ( j=z*dimxXdimy, i=0; i<dimxXdimy; j++, i++ ) {
      theZZ[j] += theSL[i];
    }

  }

  if ( typeOut != FLOAT ) {
    ConvertBuffer( theZZ, FLOAT, bufferOut, typeOut, bufferDims[2]*dimxXdimy );
  }

  return( EXIT_ON_SUCCESS );
}

























/*
 *
 * Gradient . Hessian * Gradient
 *
 *
 */
int GradientHessianGradient_2D ( void *bufferIn,
		   bufferType typeIn,
		   void *bufferOut,
		   bufferType typeOut,
		   int *bufferDims,
		   int *borderLengths,
		   float *filterCoefs,
		   recursiveFilterType filterType )
{
  char *proc = "GradientHessianGradient_2D";
  float *theXX = NULL;
  float *theYY = NULL;
  float *theXY = NULL;
  float *theX  = NULL;
  float *theY  = NULL;

  derivativeOrder Xsmooth[3] = { SMOOTHING,          NODERIVATIVE,       NODERIVATIVE };
  derivativeOrder Yderiv[3]  = { NODERIVATIVE,       DERIVATIVE_1_EDGES, NODERIVATIVE };
  derivativeOrder YYderiv[3] = { NODERIVATIVE,       DERIVATIVE_2,       NODERIVATIVE };

  derivativeOrder Ysmooth[3] = { NODERIVATIVE,       SMOOTHING,          NODERIVATIVE };
  derivativeOrder Xderiv[3]  = { DERIVATIVE_1_EDGES, NODERIVATIVE,       NODERIVATIVE };
  derivativeOrder XXderiv[3] = { DERIVATIVE_2,       NODERIVATIVE,       NODERIVATIVE };

  derivativeOrder XYderiv[3] = { DERIVATIVE_1,       DERIVATIVE_1,       NODERIVATIVE };

  int sliceDims[3];
  int z, i, dimxXdimy;

  void *sliceIn = NULL;
  void *sliceOut = NULL;

  double gx, gy, g;

  /* 
   * We check the buffers' dimensions.
   */
  if ( (bufferDims[0] <= 0) || (bufferDims[1] <= 0) || (bufferDims[2] <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  
  /*
   * test of the coefficients
   */
  if ( (filterCoefs[0] < 0.0) || (filterCoefs[1] < 0.0) ||
       (filterCoefs[2] < 0.0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: negative coefficient's value.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  

  /*
   *
   */
  dimxXdimy = bufferDims[0] * bufferDims[1];
  sliceDims[0] = bufferDims[0];
  sliceDims[1] = bufferDims[1];
  sliceDims[2] = 1;
  

  if ( typeOut == FLOAT ) {
    theXX = (float*)malloc( 4 * dimxXdimy * sizeof( float ) );
  } else {
    theXX = (float*)malloc( 5 * dimxXdimy * sizeof( float ) );
  }
  
  if ( theXX == NULL ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to allocate auxiliary buffer.\n" );
    }
    return( EXIT_ON_FAILURE );
  }
  


  theX = theY = theYY = theXX;
  theYY +=   dimxXdimy;
  theX  += 2*dimxXdimy;
  theY  += 3*dimxXdimy;



  if ( typeOut != FLOAT ) {
    theXY  =   theXX;
    theXY += 4*dimxXdimy;
  }
  
  
  
  for ( z=0; z<bufferDims[2]; z++ ) {

    switch( typeIn ) {
    default :
      break;
    case UCHAR :
    case SCHAR :
      sliceIn = (void*)( ((u8*)bufferIn) + z*dimxXdimy ); break;
    case USHORT :
    case SSHORT :
      sliceIn = (void*)( ((u16*)bufferIn) + z*dimxXdimy ); break;
    case FLOAT :
      sliceIn = (void*)( ((float*)bufferIn) + z*dimxXdimy ); break;
    case DOUBLE :
      sliceIn = (void*)( ((double*)bufferIn) + z*dimxXdimy ); break;
    }
    if ( typeOut == FLOAT ) {
      theXY = ((float*)bufferOut) + z * dimxXdimy;
    }
    
    if ( RecursiveFilterOnBuffer( sliceIn, typeIn, theX, FLOAT, 
				  sliceDims, borderLengths,
				  Ysmooth, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y^0 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( sliceIn, typeIn, theY, FLOAT, 
				  sliceDims, borderLengths,
				  Xsmooth, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^0 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }




    if ( RecursiveFilterOnBuffer( sliceIn, typeIn, theXY, FLOAT, 
				  sliceDims, borderLengths,
				  XYderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^1Y^1 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }




    if ( RecursiveFilterOnBuffer( theX, FLOAT, theXX, FLOAT, 
				  sliceDims, borderLengths,
				  XXderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^2 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( theY, FLOAT, theYY, FLOAT, 
				  sliceDims, borderLengths,
				  YYderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y^2 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }




    if ( RecursiveFilterOnBuffer( theX, FLOAT, theX, FLOAT, 
				  sliceDims, borderLengths,
				  Xderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^1 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( theY, FLOAT, theY, FLOAT, 
				  sliceDims, borderLengths,
				  Yderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y^1 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }


    
    
    for ( i=0; i<dimxXdimy; i++ ) {
      gx = theX[i];
      gy = theY[i];
      g = (gx*gx + gy*gy);
      theXY[i] = (float)(gx * ( theXX[i] * gx + theXY[i] * gy ) +
			 gy * ( theXY[i] * gx + theYY[i] * gy ));
      if ( g > 1e-10 ) theXY[i] = (float)(theXY[i] / g);
    }

    if ( typeOut != FLOAT ) {
      switch ( typeOut ) {
      case UCHAR :
	sliceOut = (((u8*)bufferOut) + z * dimxXdimy);
	break;
      case SCHAR :
	sliceOut = (((s8*)bufferOut) + z * dimxXdimy);
	break;
      case SSHORT :
	sliceOut = (((s16*)bufferOut) + z * dimxXdimy);
	break;
      case DOUBLE :
	sliceOut = (((r64*)bufferOut) + z * dimxXdimy);
	break;
      default :
	if ( _VERBOSE_ > 0 )
	  fprintf( stderr, " Error in %s: such output type not handled.\n", proc );
	free( theXX );
	return( EXIT_ON_FAILURE );
      }
      ConvertBuffer( theXY, FLOAT, sliceOut, typeOut, dimxXdimy );
    }
  }

  return( EXIT_ON_SUCCESS );
}






















int GradientHessianGradient ( void *bufferIn,
		   bufferType typeIn,
		   void *bufferOut,
		   bufferType typeOut,
		   int *bufferDims,
		   int *borderLengths,
		   float *filterCoefs,
		   recursiveFilterType filterType )
{
  char *proc = "GradientHessianGradient";



  float *theZZ = NULL;
  float *theZ  = NULL;
  float *theZ1 = NULL;
  float *theZ0 = NULL;

  float *theXZ = NULL;
  float *theYZ = NULL;

  float *theXX = NULL;
  float *theYY = NULL;
  float *theXY = NULL;

  float *theX  = NULL;
  float *theY  = NULL;


  derivativeOrder ZZderiv[3] = { SMOOTHING,    SMOOTHING,    DERIVATIVE_2 };
  derivativeOrder Zderiv[3]  = { SMOOTHING,    SMOOTHING,    DERIVATIVE_1 };
  derivativeOrder Z1deriv[3] = { NODERIVATIVE, NODERIVATIVE, DERIVATIVE_1 };
  derivativeOrder Z0deriv[3] = { NODERIVATIVE, NODERIVATIVE, SMOOTHING };

  derivativeOrder XZderiv[3] = { DERIVATIVE_1, SMOOTHING,    NODERIVATIVE };
  derivativeOrder YZderiv[3] = { SMOOTHING,    DERIVATIVE_1, NODERIVATIVE };

  derivativeOrder XXderiv[3] = { DERIVATIVE_2, SMOOTHING,    NODERIVATIVE };
  derivativeOrder YYderiv[3] = { SMOOTHING,    DERIVATIVE_2, NODERIVATIVE };
  derivativeOrder XYderiv[3] = { DERIVATIVE_1, DERIVATIVE_1, NODERIVATIVE };

  derivativeOrder Xderiv[3]  = { DERIVATIVE_1, SMOOTHING,    NODERIVATIVE };
  derivativeOrder Yderiv[3]  = { SMOOTHING,    DERIVATIVE_1, NODERIVATIVE };

  int sliceDims[3];
  int z, i, j, dimxXdimy;

  double gx, gy, gz, g;

  /* 
   * We check the buffers' dimensions.
   */
  if ( bufferDims[2] == 1 ) {
    return( GradientHessianGradient_2D ( bufferIn, typeIn, bufferOut, typeOut,
			   bufferDims, borderLengths, filterCoefs, filterType ) );
  }

  if ( (bufferDims[0] <= 0) || (bufferDims[1] <= 0) || (bufferDims[2] <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  
  /*
   * test of the coefficients
   */
  if ( (filterCoefs[0] < 0.0) || (filterCoefs[1] < 0.0) ||
       (filterCoefs[2] < 0.0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: negative coefficient's value.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  

  /*
   *
   */
  dimxXdimy = bufferDims[0] * bufferDims[1];
  sliceDims[0] = bufferDims[0];
  sliceDims[1] = bufferDims[1];
  sliceDims[2] = 1;
  

  if ( typeOut == FLOAT ) {
    theX  = (float*)malloc( (7+3*bufferDims[2]) * dimxXdimy * sizeof( float ) );
  } else {
    theX = (float*)malloc( (7+4*bufferDims[2]) * dimxXdimy * sizeof( float ) );
  }

  
  if ( theX == NULL ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to allocate auxiliary buffer.\n" );
    }
    return( EXIT_ON_FAILURE );
  }
  
  /*
   * BUFFERS
   *
   * slices  : theX  theY  theXY theYY theXX theYZ theXZ
   *
   * volumes : theZ0 theZ1 theZ  theZZ
   *
   */

  theY  = theXX = theXY = theYY = theYZ = theXZ = theX;
  theZ0 = theZ1 = theZ  = theX;

  theY  +=   dimxXdimy;
  theXY += 2*dimxXdimy;
  theYY += 3*dimxXdimy;
  theXX += 4*dimxXdimy;
  theYZ += 5*dimxXdimy;
  theXZ += 6*dimxXdimy;
 
  theZ0 += 7*dimxXdimy;
  theZ1 += 7*dimxXdimy +   bufferDims[2]*dimxXdimy;
  theZ  += 7*dimxXdimy + 2*bufferDims[2]*dimxXdimy;

  if ( typeOut == FLOAT ) {
    theZZ  = (float*)bufferOut;
  } else {
    theZZ  = theX;
    theZZ += 7*dimxXdimy + 3*bufferDims[2]*dimxXdimy;
  }
  
  
  
  /*
   *
   * 3D filtering / filtering along Z
   *
   */

  if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theZ0, FLOAT, 
				bufferDims, borderLengths,
				Z0deriv, filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Z^0 derivative.\n" );
    }
    free( theX );
    return( EXIT_ON_FAILURE );
  }
  
  if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theZ1, FLOAT, 
				bufferDims, borderLengths,
				Z1deriv, filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Z^1 derivative.\n" );
    }
    free( theX );
    return( EXIT_ON_FAILURE );
  }
  
  if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theZ, FLOAT, 
				bufferDims, borderLengths,
				Zderiv, filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Z^1 derivative (edge).\n" );
    }
    free( theX );
    return( EXIT_ON_FAILURE );
  }
  
  if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theZZ, FLOAT, 
				bufferDims, borderLengths,
				ZZderiv, filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Z^2 derivative.\n" );
    }
    free( theX );
    return( EXIT_ON_FAILURE );
  }


  /*
   * theZ0 : smoothed         along Z
   * theZ1 : first derivative along Z
   * theZ  : first derivative along Z, smoothed along X and Y
   * theZZ : second derivative along Z, smoothed along X and Y
   */



  for ( z=0; z<bufferDims[2]; z++ ) {
    fprintf( stderr, "%s: processing slice %3d/%d\r",
	     proc, z, bufferDims[2] );
    /*
     *
     * 2D filtering / filtering along X and Y
     *
     */
    
    if ( RecursiveFilterOnBuffer( theZ1+z*dimxXdimy, FLOAT, theXZ, FLOAT, 
				  sliceDims, borderLengths,
				  XZderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^1Z^1 derivative.\n" );
      }
      free( theX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( theZ1+z*dimxXdimy, FLOAT, theYZ, FLOAT, 
				  sliceDims, borderLengths,
				  YZderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y^1Z^1 derivative.\n" );
      }
      free( theX );
      return( EXIT_ON_FAILURE );
    }




    if ( RecursiveFilterOnBuffer( theZ0+z*dimxXdimy, FLOAT, theXX, FLOAT,
				  sliceDims, borderLengths,
				  XXderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^2 derivative.\n" );
      }
      free( theX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( theZ0+z*dimxXdimy, FLOAT, theYY, FLOAT,
				  sliceDims, borderLengths,
				  YYderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y^2 derivative.\n" );
      }
      free( theX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( theZ0+z*dimxXdimy, FLOAT, theXY, FLOAT,
				  sliceDims, borderLengths,
				  XYderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^1Y^1 derivative.\n" );
      }
      free( theX );
      return( EXIT_ON_FAILURE );
    }



    if ( RecursiveFilterOnBuffer( theZ0+z*dimxXdimy, FLOAT, theX, FLOAT,
				  sliceDims, borderLengths,
				  Xderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^1 derivative (edge).\n" );
      }
      free( theX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( theZ0+z*dimxXdimy, FLOAT, theY, FLOAT,
				  sliceDims, borderLengths,
				  Yderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y^1 derivative (edge).\n" );
      }
      free( theX );
      return( EXIT_ON_FAILURE );
    }

    
    
    for ( j=z*dimxXdimy, i=0; i<dimxXdimy; j++, i++ ) {
      gx = theX[i];
      gy = theY[i];
      gz = theZ[j];
      g = gx*gx + gy*gy + gz*gz;
      theZZ[j] = (float)(gx * ( theXX[i] * gx + theXY[i] * gy  + theXZ[i] * gz ) +
			 gy * ( theXY[i] * gx + theYY[i] * gy  + theYZ[i] * gz ) +
			 gz * ( theXZ[i] * gx + theYZ[i] * gy  + theZZ[j] * gz ));
      if ( g > 1e-10 ) theZZ[j] = (float)(theZZ[j] / g);

    }

  }

  if ( typeOut != FLOAT ) {
    ConvertBuffer( theZZ, FLOAT, bufferOut, typeOut, bufferDims[2]*dimxXdimy );
  }
  
  free( theX );

  return( EXIT_ON_SUCCESS );
}





































/*
 *
 * 
 *
 *
 */

int RecursiveFilterOnBuffer( void *bufferIn,
			     bufferType typeIn,
			     void *bufferOut,
			     bufferType typeOut,
			     int *bufferDims,
			     int *borderLengths,
			     derivativeOrder *derivatives,
			     float *filterCoefs,
			     recursiveFilterType filterType )
{
  char *proc = "RecursiveFilterOnBuffer";
  register int dimx, dimxXdimy;
  int dimy, dimz;
  register int x, y, z;
  /* 
   *obviously, we need to perform the computation 
   * with float or double values. For this reason,
   * we allocate an auxiliary buffer if the output buffer
   * is not of type float or double.
   */
  void *bufferToBeProcessed = (void*)NULL;
  bufferType typeToBeProcessed = TYPE_UNKNOWN;
  void *bufferResult = (void*)NULL;
  bufferType typeResult = TYPE_UNKNOWN;
  /*
   * lines' lengths
   */
  int lengthX = 0;
  int lengthY = 0;
  int lengthZ = 0;
  int maxLengthline = 0;
  int borderXlength = 0;
  int borderYlength = 0;
  int borderZlength = 0;
  /*
   * 1D arrays for computations.
   */
  double *theLine = (double*)NULL;
  double *resLine = (double*)NULL;
  double *tmpLine = (double*)NULL;
  /*
   * pointers for computations;
   */
  register r32 *r32firstPoint = (r32*)NULL;
  register r64 *r64firstPoint = (r64*)NULL;
  register r32 *r32_pt = (r32*)NULL;
  register r64 *r64_pt = (r64*)NULL;
  register double *dbl_pt1 = (double*)NULL;
  register double *dbl_pt2 = (double*)NULL;
  register double dbl_first = 0.0;
  register double dbl_last = 0.0;
  int offsetLastPoint = 0;
  int offsetNextFirstPoint = 0;
  register r32 *r32firstPointResult = (r32*)NULL;
  register r64 *r64firstPointResult = (r64*)NULL;
  double *theLinePlusBorder = (double*)NULL;
  double *resLinePlusBorder = (double*)NULL;

  RFcoefficientType *RFC = NULL;

  /* 
   * We check the buffers' dimensions.
   */
  dimx = bufferDims[0];   dimy = bufferDims[1];   dimz = bufferDims[2];
  dimxXdimy = dimx * dimy;
  if ( (dimx <= 0) || (dimy <= 0) || (dimz <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  /*
   * We check the pointers.
   */
  if ( (bufferIn == (void*)NULL) || (bufferOut == (void*)NULL) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: NULL pointer on buffer.\n", proc );
    return( EXIT_ON_FAILURE );
  }

  /* 
   * May we use the buffer bufferOut as the bufferResult?
   * If its type is FLOAT or DOUBLE, then yes.
   * If not, we have to allocate an auxiliary buffer.
   */
  if ( (typeOut == FLOAT) || (typeOut == DOUBLE) ) {
    bufferResult = bufferOut;
    typeResult = typeOut;
  } else {
    bufferResult = (void*)malloc( (dimx*dimy*dimz) * sizeof(r32) );
    if ( bufferResult == (void*)NULL ) {
      if ( _VERBOSE_ > 0 )
	fprintf( stderr, " Fatal error in %s: unable to allocate auxiliary buffer.\n", proc );
      return( EXIT_ON_FAILURE );
    }
    typeResult = FLOAT;
  }
  
  /* 
   * May we consider the buffer bufferIn as the bufferToBeProcessed?
   * If its type is FLOAT or DOUBLE, then yes.
   * If not, we convert it into the buffer bufferResult, and this
   * last buffer is the bufferToBeProcessed.
   */
  if ( (typeIn == FLOAT) || (typeIn == DOUBLE) ) {
    bufferToBeProcessed = bufferIn;
    typeToBeProcessed = typeIn;
  } else {
    ConvertBuffer( bufferIn, typeIn, bufferResult, typeResult, (dimx*dimy*dimz) );
    bufferToBeProcessed = bufferResult;
    typeToBeProcessed = typeResult;
  }

  /*
   * Estimation of the lines' length along each direction.
   */
  if ( borderLengths != NULL ) {
    borderXlength = borderLengths[0];
    borderYlength = borderLengths[1];
    borderZlength = borderLengths[2];
    if ( borderXlength < 0 ) borderXlength = 0;
    if ( borderYlength < 0 ) borderYlength = 0;
    if ( borderZlength < 0 ) borderZlength = 0;
  }

  /*
   * Tue Jul  6 19:15:15 MET DST 1999 (gregoire.malandainoire Malandain)
   * changes 3 x dimx -> dimx, dimy, dimz
   */
  lengthX = dimx + 2 * borderXlength;
  lengthY = dimy + 2 * borderYlength;
  lengthZ = dimz + 2 * borderZlength;
  maxLengthline = lengthX;
  if ( maxLengthline < lengthY ) maxLengthline = lengthY;
  if ( maxLengthline < lengthZ ) maxLengthline = lengthZ;
  if ( maxLengthline <= 0 ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: unable to deal with dimensions = 0.\n", proc );
    if ( (typeOut != FLOAT) && (typeOut != DOUBLE) )
      free( bufferResult );
    return( EXIT_ON_FAILURE );
  }
  /*
   * Allocations of work arrays. 
   * We will use them to process each line.
   */
  theLine = (double*)malloc( 3 * maxLengthline * sizeof(double) );
  if ( theLine == (double*)NULL ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: unable to allocate auxiliary work arrays.\n", proc );
    if ( (typeOut != FLOAT) && (typeOut != DOUBLE) )
      free( bufferResult );
    return( EXIT_ON_FAILURE );
  }
  resLine = theLine + maxLengthline;
  tmpLine = resLine + maxLengthline;

  /*
   * From now,
   * typeToBeProcessed is either FLOAT or DOUBLE
   * so is typeResult.
   */


  /*
   * Processing along X.
   */
  if ( dimx > 4 )
  if (derivatives[0] != NODERIVATIVE)
  if (filterCoefs[0] > 0.0) {

    if ( _VERBOSE_ != 0 )
      fprintf( stderr, " %s: processing along X.\n", proc );

    RFC = InitRecursiveCoefficients( (double)filterCoefs[0], filterType, derivatives[0] );

    if ( RFC == NULL ) {
      if ( _VERBOSE_ != 0 )
	fprintf( stderr, " %s: unable to allocate coefficients\n", proc );
      if ( (typeOut != FLOAT) && (typeOut != DOUBLE) )
	free( bufferResult );
      return( EXIT_ON_FAILURE );
    }
    
    r64firstPoint = (r64*)bufferToBeProcessed;
    r32firstPoint = (r32*)bufferToBeProcessed;

    r64firstPointResult = (r64*)bufferResult;
    r32firstPointResult = (r32*)bufferResult;

    offsetLastPoint = borderXlength + dimx - 1;

    theLinePlusBorder = theLine + borderXlength;
    resLinePlusBorder = resLine + borderXlength;

    /*
     * There are dimz*dimy X lines to be processed.
     */
    for ( z=0; z<dimz; z++ )
    for ( y=0; y<dimy; y++ ) {
      /*
       * Acquiring a X line.
       */ 
      dbl_pt1 = theLinePlusBorder;
      switch ( typeToBeProcessed ) {
      case DOUBLE :
	(void)memcpy( (void*)dbl_pt1, (void*)r64firstPoint, dimx * sizeof(r64) );
	r64firstPoint += dimx;
	break;
      case FLOAT :
      default :
	for ( x=0; x<dimx; x++, dbl_pt1++, r32firstPoint++ ) *dbl_pt1 = *r32firstPoint;
      }
      /*
       * Adding points at both ends of the line.
       */
      if ( borderXlength > 0 ) {
	dbl_pt1 = theLine + borderXlength;   dbl_first = *dbl_pt1;
	dbl_pt2 = theLine + offsetLastPoint; dbl_last  = *dbl_pt2;
	for ( x=0; x<borderXlength; x++ ) {
	  *--dbl_pt1 = dbl_first;
	  *++dbl_pt2 = dbl_last;
	}
      }
      /*
       * Processing the line.
       */
      if ( RecursiveFilter1D( RFC, theLine, resLine, tmpLine, resLine, lengthX ) == 0 ) {
	if ( _VERBOSE_ != 0 ) 
	  fprintf(stderr," Error in %s: unable to process X line (y=%d,z=%d).\n", proc, y, z);
	if ( (typeOut != FLOAT) && (typeOut != DOUBLE) )
	  free( bufferResult );
	free( (void*)theLine );
	return( EXIT_ON_FAILURE );
      }
      /*
       * Copy the result into the buffer bufferResult.
       */
      dbl_pt1 = resLinePlusBorder;
      switch ( typeResult ) {
      case DOUBLE :
	(void)memcpy( (void*)r64firstPointResult, (void*)dbl_pt1, dimx * sizeof(r64) );
	r64firstPointResult += dimx;
	break;
      case FLOAT :
      default :
	for ( x=0; x<dimx; x++, dbl_pt1++, r32firstPointResult++ )
	  *r32firstPointResult = (r32)(*dbl_pt1);
      }
    }
    
    /*
     * The next buffer to be processed is the buffer
     * bufferResult.
     */
    bufferToBeProcessed = bufferResult;
    typeToBeProcessed = typeResult;
    
    free( RFC );
    RFC = NULL;

  } /* end of Processing along X. */
  
  /*
   * Processing along Y.
   */
  if ( dimy > 4 )
  if (derivatives[1] != NODERIVATIVE)
  if (filterCoefs[1] > 0.0) {

    if ( _VERBOSE_ != 0 )
      fprintf( stderr, " %s: processing along Y.\n", proc );

    RFC = InitRecursiveCoefficients( (double)filterCoefs[1], filterType, derivatives[1] );

    if ( RFC == NULL ) {
      if ( _VERBOSE_ != 0 )
	fprintf( stderr, " %s: unable to allocate coefficients\n", proc );
      if ( (typeOut != FLOAT) && (typeOut != DOUBLE) )
	free( bufferResult );
      return( EXIT_ON_FAILURE );
    }

    r64firstPoint = (r64*)bufferToBeProcessed;
    r32firstPoint = (r32*)bufferToBeProcessed;

    r64firstPointResult = (r64*)bufferResult;
    r32firstPointResult = (r32*)bufferResult;

    offsetLastPoint = borderYlength + dimy - 1;
    offsetNextFirstPoint = dimx * dimy - dimx;

    theLinePlusBorder = theLine + borderYlength;
    resLinePlusBorder = resLine + borderYlength;

    /*
     * There are dimz*dimx Y lines to be processed.
     */
    for ( z=0; z<dimz; z++ ) {
      for ( x=0; x<dimx; x++ ) {
      /*
       * Acquiring a Y line.
       */ 
	dbl_pt1 = theLinePlusBorder;
	switch ( typeToBeProcessed ) {
	case DOUBLE :
	  r64_pt = r64firstPoint;
	  for ( y=0; y<dimy; y++, dbl_pt1++, r64_pt += dimx ) *dbl_pt1 = *r64_pt;
	  /*
	   * Going to the first point of the next Y line
	   */
	  r64firstPoint ++;
	  break;
	case FLOAT :
	default :
	  r32_pt = r32firstPoint;
	  for ( y=0; y<dimy; y++, dbl_pt1++, r32_pt += dimx ) *dbl_pt1 = *r32_pt;
	  r32firstPoint ++;
	}
	/*
	 * Adding points at both ends of the line.
	 */
	if ( borderYlength > 0 ) {
	  dbl_pt1 = theLine + borderYlength;   dbl_first = *dbl_pt1;
	  dbl_pt2 = theLine + offsetLastPoint; dbl_last  = *dbl_pt2;
	  for ( y=0; y<borderYlength; y++ ) {
	    *--dbl_pt1 = dbl_first;
	    *++dbl_pt2 = dbl_last;
	  }
	}
	/*
	 * Processing the line.
	 */
	if ( RecursiveFilter1D( RFC, theLine, resLine, tmpLine, resLine, lengthY ) == 0 ) {
	  if ( _VERBOSE_ != 0 ) 
	    fprintf(stderr," Error in %s: unable to process Y line (x=%d,z=%d).\n", proc, x, z);
	  if ( (typeOut != FLOAT) && (typeOut != DOUBLE) )
	    free( bufferResult );
	  free( (void*)theLine );
	  return( EXIT_ON_FAILURE );
	}
	/*
	 * Copy the result into the buffer bufferResult.
	 */
	dbl_pt1 = resLinePlusBorder;
	switch ( typeResult ) {
	case DOUBLE :
	  r64_pt = r64firstPointResult;
	  for ( y=0; y<dimy; y++, dbl_pt1++, r64_pt += dimx ) *r64_pt = *dbl_pt1;
	  r64firstPointResult ++;
	  break;
	case FLOAT :
	default :
	  r32_pt = r32firstPointResult;
	  for ( y=0; y<dimy; y++, dbl_pt1++, r32_pt += dimx ) 
	    *r32_pt = (float)*dbl_pt1;
	  r32firstPointResult ++;
	}
      }
      /*
       * Going to the first point of the next Y line
       * which is the first Y line of the next slice.
       *
       * The pointer r[32,64]firstPoint[Result] has
       * already been increased by dimx. To reach
       * the first point of the next slice, we
       * have to increase it by (dimx*dimy)-dimx.
       */
      switch ( typeToBeProcessed ) {
      case DOUBLE :
	r64firstPoint += offsetNextFirstPoint;
	break;
      case FLOAT :
      default :
	r32firstPoint += offsetNextFirstPoint;
      }
      switch ( typeResult ) {
      case DOUBLE :
	r64firstPointResult += offsetNextFirstPoint;
	break;
      case FLOAT :
      default :
	r32firstPointResult += offsetNextFirstPoint;
      }
    }
    
    /*
     * The next buffer to be processed is the buffer
     * bufferResult.
     */
    bufferToBeProcessed = bufferResult;
    typeToBeProcessed = typeResult;
  
    free( RFC );
    RFC = NULL;

  } /* end of Processing along Y. */
  

  /*
   * Processing along Z.
   */
  if ( dimz > 4 )
  if (derivatives[2] != NODERIVATIVE)
  if (filterCoefs[2] > 0.0) {

    if ( _VERBOSE_ != 0 )
      fprintf( stderr, " %s: processing along Z.\n", proc );
    
    RFC = InitRecursiveCoefficients( (double)filterCoefs[2], filterType, derivatives[2] );
    
    if ( RFC == NULL ) {
      if ( _VERBOSE_ != 0 )
	fprintf( stderr, " %s: unable to allocate coefficients\n", proc );
      if ( (typeOut != FLOAT) && (typeOut != DOUBLE) )
	free( bufferResult );
      return( EXIT_ON_FAILURE );
    }

    r64firstPoint = (r64*)bufferToBeProcessed;
    r32firstPoint = (r32*)bufferToBeProcessed;

    offsetLastPoint = borderZlength + dimz - 1;

    r64firstPointResult = (r64*)bufferResult;
    r32firstPointResult = (r32*)bufferResult;

    offsetLastPoint = borderZlength + dimz - 1;

    theLinePlusBorder = theLine + borderYlength;
    resLinePlusBorder = resLine + borderYlength;

    /*
     * There are dimy*dimx Z lines to be processed.
     */
    for ( y=0; y<dimy; y++ )
    for ( x=0; x<dimx; x++ ) {
      /*
       * Acquiring a Z line.
       */ 
      dbl_pt1 = theLinePlusBorder;
      switch ( typeToBeProcessed ) {
      case DOUBLE :
	r64_pt = r64firstPoint;
	for ( z=0; z<dimz; z++, dbl_pt1++, r64_pt += dimxXdimy ) *dbl_pt1 = *r64_pt;
	/*
	 * Going to the first point of the next Z line
	 */
	r64firstPoint ++;
	break;
      case FLOAT :
      default :
	r32_pt = r32firstPoint;
	for ( z=0; z<dimz; z++, dbl_pt1++, r32_pt += dimxXdimy ) *dbl_pt1 = *r32_pt;
	r32firstPoint ++;
      }
      /*
       * Adding points at both ends of the line.
       */
      if ( borderZlength > 0 ) {
	dbl_pt1 = theLine + borderZlength;   dbl_first = *dbl_pt1;
	dbl_pt2 = theLine + offsetLastPoint; dbl_last  = *dbl_pt2;
	for ( z=0; z<borderZlength; z++ ) {
	  *--dbl_pt1 = dbl_first;
	  *++dbl_pt2 = dbl_last;
	}
      }
      /*
       * Processing the line.
       */
      if ( RecursiveFilter1D( RFC, theLine, resLine, tmpLine, resLine, lengthZ ) == 0 ) {
	if ( _VERBOSE_ != 0 ) 
	  fprintf(stderr," Error in %s: unable to process Z line (x=%d,y=%d).\n", proc, x, y);
	if ( (typeOut != FLOAT) && (typeOut != DOUBLE) )
	  free( bufferResult );
	free( (void*)theLine );
	return( EXIT_ON_FAILURE );
      }
      
      /*
       * Copy the result into the buffer bufferResult.
       */
      dbl_pt1 = resLinePlusBorder;
      switch ( typeResult ) {
      case DOUBLE :
	r64_pt = r64firstPointResult;
	for ( z=0; z<dimz; z++, dbl_pt1++, r64_pt += dimxXdimy ) 
	  *r64_pt = *dbl_pt1;
	r64firstPointResult ++;
	break;
      case FLOAT :
      default :
	r32_pt = r32firstPointResult;
	for ( z=0; z<dimz; z++, dbl_pt1++, r32_pt += dimxXdimy ) 
	  *r32_pt = (float)*dbl_pt1;
	r32firstPointResult ++;
      }
    }

    free( RFC );
    RFC = NULL;

  } /* end of Processing along Z. */
  



  /*
   * From bufferResult to bufferOut
   */
  ConvertBuffer( bufferResult, typeResult, bufferOut, typeOut, (dimx*dimy*dimz) );

  /*
   * Releasing the buffers.
   */
  if ( (typeOut != FLOAT) && (typeOut != DOUBLE) )
    free( bufferResult );
  free( (void*)theLine );
  
  return( EXIT_ON_SUCCESS );
}






void Recbuffer_verbose ( )
{
  _VERBOSE_ = 1;
  Recline_verbose ( );
}





void Recbuffer_noverbose ( )
{
  _VERBOSE_ = 0;
  Recline_noverbose ( );
}
