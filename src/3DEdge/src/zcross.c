/*************************************************************************
 * zcross.c - zero-crossings
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
 * Tue Nov 28 10:00:36 MET 2000
 *
 * ADDITIONS, CHANGES
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <convert.h>
#include <recbuffer.h>
#include <zcross.h>

static int _VERBOSE_ = 0;


#define EXIT_ON_FAILURE 0
#define EXIT_ON_SUCCESS 1


void ZeroCrossings_verbose ( )
{
  _VERBOSE_ = 1;
  Recbuffer_verbose ( );
}

void ZeroCrossings_noverbose ( )
{
  _VERBOSE_ = 0;
  Recbuffer_noverbose ( );
}





/*
 * `sign' of the zero-crossings
 * if > 0, zero-crossings are chosen in the `positive' region
 *         ie are points M such that I(M) > 0 et I(M+v) <= 0
 * if < 0 zero-crossings are chosen in the `negative' region
 *         ie are points M such that I(M) < 0 et I(M+v) >= 0
 */

static int sign_ZeroCrossing = 1;

void ZeroCrossings_Are_Positive()
{
  sign_ZeroCrossing = 1;
}

void ZeroCrossings_Are_Negative()
{
  sign_ZeroCrossing = -1;
}










int Extract_ZeroCrossing_2D ( void *bufferIn, bufferType typeIn,
			      void *bufferOut, bufferType typeOut, int *bufferDims )
{
  if ( sign_ZeroCrossing > 0 ) 
    return( Extract_PositiveZeroCrossing_2D( bufferIn, typeIn, bufferOut,
					     typeOut, bufferDims ) );
  return( Extract_NegativeZeroCrossing_2D( bufferIn, typeIn, bufferOut,
					   typeOut, bufferDims ) );
}









/*************************************************************
ZERO-CROSSINGS
*/
int Extract_PositiveZeroCrossing_2D ( void *bufferIn,
				       bufferType typeIn,
				       void *bufferOut,
				       bufferType typeOut,
				       int *bufferDims )
{
  /* les passage par zeros sont definis par
     I(M) > 0 et I(M+v) <= 0
     on marque M

     Dans un contexte de detection de contours (ex laplacien)
     cela marque dans les zones sombres (par rapport a clair)
  */
     
  char *proc="Extract_PositiveZeroCrossing_2D";
  int x, y, z, iz, iy;
  int dx  = bufferDims[0];
  int dxy = bufferDims[0] * bufferDims[1];

  /* 
   * We check the buffers' dimensions.
   */
  if ( (bufferDims[0] <= 0) || (bufferDims[1] <= 0) || (bufferDims[2] <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }

  if ( bufferIn == bufferOut ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: input buffer should not be equal to output.\n", proc );
    return( EXIT_ON_FAILURE );
  }

  /*
   * // the (bufferDims[1]-1) first rows
   * for ( y=0, iy=0; y<bufferDims[1]-1; y++, iy+=dx )
   *
   *   // the (dx-1) first points of the row
   *   for ( x=0; x<dx-1; x++ )
   *     // we mark (x,y)   if (x,y) > 0  && ((x+1,y) <= 0 || (x,y+1) <= 0)
   *     // we mark (x+1,y) if (x,y) <= 0 &&  (x+1,y) > 0
   *     // we mark (x,y+1) if (x,y) <= 0 &&  (x,y+1) > 0
   *
   *   // the last point of the row
   *   // we mark (x,y)   if (x,y) > 0  && (x,y+1) <= 0
   *   // we mark (x,y+1) if (x,y) <= 0 && (x,y+1) > 0
   *
   * // the last row
   * for ( x=0; x<dx-1; x++ )
   *   // we mark (x,y)   if (x,y) > 0  && (x+1,y) <= 0
   *   // we mark (x+1,y) if (x,y) <= 0 && (x+1,y) > 0
   *
   */
  
#define _POSITIVE_ZERO_CROSSINGS_( TYPE ) {                     \
  TYPE *resBuf = (TYPE*)bufferOut;                              \
  iz = bufferDims[2]*bufferDims[1]*bufferDims[0];               \
  for ( x=0; x<iz; x++ ) resBuf[x] = 0;                         \
  for ( z=0, iz=0; z<bufferDims[2]; z++, iz+=dxy ) {            \
    for ( y=0, iy=0; y<bufferDims[1]-1; y++, iy+=dx ) {         \
      for ( x=0; x<dx-1; x++ ) {                                \
	if ( theBuf[iz+iy+x] > 0 ) {                            \
	  if ( theBuf[iz+iy+x+1] <= 0 || theBuf[iz+iy+x+dx] <= 0 ) resBuf[iz+iy+x] = 1; \
	} else {                                                \
	  if ( theBuf[iz+iy+x+1] > 0 )  resBuf[iz+iy+x+1] = 1;  \
	  if ( theBuf[iz+iy+x+dx] > 0 ) resBuf[iz+iy+x+dx] = 1; \
	}                                                       \
      }                                                         \
      if ( theBuf[iz+iy+x] > 0 ) {                              \
	if ( theBuf[iz+iy+x+dx] <= 0 ) resBuf[iz+iy+x] = 1;     \
      } else {                                                  \
	if ( theBuf[iz+iy+x+dx] > 0 )  resBuf[iz+iy+x+dx] = 1;  \
      }                                                         \
    }                                                           \
    for ( x=0; x<dx-1; x++ ) {                                  \
      if ( theBuf[iz+iy+x] > 0 ) {                              \
	if ( theBuf[iz+iy+x+1] <= 0 ) resBuf[iz+iy+x] = 1;      \
      } else {                                                  \
	if ( theBuf[iz+iy+x+1] > 0 )  resBuf[iz+iy+x+1] = 1;    \
      }                                                         \
    }                                                           \
  }                                                             \
}

  switch( typeIn ) {
  case FLOAT :
    {
      r32 *theBuf = (r32*)bufferIn;
    
      switch( typeOut ) {

      case UCHAR :
	_POSITIVE_ZERO_CROSSINGS_( u8 )
	break;
	
      case FLOAT :
	_POSITIVE_ZERO_CROSSINGS_( r32 )
	break;
	
      default :
	if ( _VERBOSE_ > 0 )
	  fprintf( stderr, " Error in %s: such output type not handled.\n", proc );
	return( EXIT_ON_FAILURE );
      }

    } /* typeIn == FLOAT */
    break;

  default :
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: such input type not handled.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  return( EXIT_ON_SUCCESS );
}














int Extract_NegativeZeroCrossing_2D ( void *bufferIn,
				      bufferType typeIn,
				      void *bufferOut,
				      bufferType typeOut,
				      int *bufferDims )
{
  /* les passage par zeros sont definis par
     I(M) < 0 et I(M+v) >= 0
     on marque M

     Dans un contexte de detection de contours (ex laplacien)
     cela marque dans les zones sombres (par rapport a clair)
  */
     
  char *proc="Extract_NegativeZeroCrossing_2D";
  int x, y, z, iz, iy;
  int dx  = bufferDims[0];
  int dxy = bufferDims[0] * bufferDims[1];

  /* 
   * We check the buffers' dimensions.
   */
  if ( (bufferDims[0] <= 0) || (bufferDims[1] <= 0) || (bufferDims[2] <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }

  if ( bufferIn == bufferOut ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: input buffer should not be equal to output.\n", proc );
    return( EXIT_ON_FAILURE );
  }


  
  /*
   * // the (bufferDims[1]-1) first rows
   * for ( y=0, iy=0; y<bufferDims[1]-1; y++, iy+=dx )
   *
   *   // the (dx-1) first points of the row
   *   for ( x=0; x<dx-1; x++ )
   *     // we mark (x,y)   if (x,y) < 0  && ((x+1,y) >= 0 || (x,y+1) >= 0)
   *     // we mark (x+1,y) if (x,y) >= 0 &&  (x+1,y) < 0
   *     // we mark (x,y+1) if (x,y) >= 0 &&  (x,y+1) < 0
   *
   *   // the last point of the row
   *   // we mark (x,y)   if (x,y) < 0  && (x,y+1) >= 0
   *   // we mark (x,y+1) if (x,y) >= 0 && (x,y+1) < 0
   *
   * // the last row
   * for ( x=0; x<dx-1; x++ )
   *   // we mark (x,y)   if (x,y) < 0  && (x+1,y) >= 0
   *   // we mark (x+1,y) if (x,y) >= 0 && (x+1,y) < 0
   *
   */
  
#define _NEGATIVE_ZERO_CROSSINGS_( TYPE ) {                     \
  TYPE *resBuf = (TYPE*)bufferOut;                              \
  iz = bufferDims[2]*bufferDims[1]*bufferDims[0];               \
  for ( x=0; x<iz; x++ ) resBuf[x] = 0;                         \
  for ( z=0, iz=0; z<bufferDims[2]; z++, iz+=dxy ) {            \
    for ( y=0, iy=0; y<bufferDims[1]-1; y++, iy+=dx ) {         \
      for ( x=0; x<dx-1; x++ ) {                                \
	if ( theBuf[iz+iy+x] < 0 ) {                            \
	  if ( theBuf[iz+iy+x+1] >= 0 || theBuf[iz+iy+x+dx] >= 0 ) resBuf[iz+iy+x] = 1; \
	} else {                                                \
	  if ( theBuf[iz+iy+x+1] < 0 )  resBuf[iz+iy+x+1] = 1;  \
	  if ( theBuf[iz+iy+x+dx] < 0 ) resBuf[iz+iy+x+dx] = 1; \
	}                                                       \
      }                                                         \
      if ( theBuf[iz+iy+x] < 0 ) {                              \
	if ( theBuf[iz+iy+x+dx] >= 0 ) resBuf[iz+iy+x] = 1;     \
      } else {                                                  \
	if ( theBuf[iz+iy+x+dx] < 0 )  resBuf[iz+iy+x+dx] = 1;  \
      }                                                         \
    }                                                           \
    for ( x=0; x<dx-1; x++ ) {                                  \
      if ( theBuf[iz+iy+x] < 0 ) {                              \
	if ( theBuf[iz+iy+x+1] >= 0 ) resBuf[iz+iy+x] = 1;      \
      } else {                                                  \
	if ( theBuf[iz+iy+x+1] < 0 )  resBuf[iz+iy+x+1] = 1;    \
      }                                                         \
    }                                                           \
  }                                                             \
}


  switch( typeIn ) {
  case FLOAT :
    {
      r32 *theBuf = (r32*)bufferIn;
    
      switch( typeOut ) {
	
      case UCHAR :
	_NEGATIVE_ZERO_CROSSINGS_( u8 )
	break;

      case FLOAT :
	_NEGATIVE_ZERO_CROSSINGS_( r32 )
	break;
	
      default :
	if ( _VERBOSE_ > 0 )
	  fprintf( stderr, " Error in %s: such output type not handled.\n", proc );
	return( EXIT_ON_FAILURE );
      }
      
    }
    break;
    
  default :
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: such input type not handled.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  return( EXIT_ON_SUCCESS );
}





























int Mask_With_Image( void *bufferIn,   bufferType typeIn,
		     void *bufferMask, bufferType typeMask,
		     void *bufferOut,  bufferType typeOut,
		     int *bufferDims ) 
{
  char *proc = "Mask_With_Image";
  int i, v;

  /* 
   * We check the buffers' dimensions.
   */
  if ( (bufferDims[0] <= 0) || (bufferDims[1] <= 0) || (bufferDims[2] <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  v = bufferDims[0] * bufferDims[1] * bufferDims[2];
  
  if ( typeIn != typeOut ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: buffers in and out should have the same type.\n", proc );
    return( EXIT_ON_FAILURE );
  }

  
#define _MASK_( type ) { \
  type *theBuf = (type*)bufferIn;  \
  type *resBuf = (type*)bufferOut; \
  for (i=0; i<v; i++ ) {           \
    resBuf[i] = ( theMask[i] > 0 ) ? theBuf[i] : 0.0; \
  } \
}

  switch( typeMask ) {

  case UCHAR :
    {
      u8 *theMask = (u8*)bufferMask;
      switch( typeIn ) {
      case FLOAT :
	_MASK_( r32 )
	break;
      case DOUBLE :
	_MASK_( r64 )
	break;
      default :
	if ( _VERBOSE_ > 0 )
	  fprintf( stderr, " Error in %s: such output type not handled.\n", proc );
	return( EXIT_ON_FAILURE );
      }
    } /* mask: end of UCHAR */
    break;

  case FLOAT :
    {
      r32 *theMask = (r32*)bufferMask;
      switch( typeIn ) {
      case FLOAT :
	_MASK_( r32 )
	break;
      case DOUBLE :
	_MASK_( r64 )
	break;
      default :
	if ( _VERBOSE_ > 0 )
	  fprintf( stderr, " Error in %s: such output type not handled.\n", proc );
	return( EXIT_ON_FAILURE );
      }
    } /* mask: end of FLOAT */
    break;

  default :
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: such mask type not handled.\n", proc );
    return( EXIT_ON_FAILURE );
  }

  return( EXIT_ON_SUCCESS );
}
		     




















/*
 *
 *
 * Edge detection
 * - laplacian
 * - hessian
 *
 *
 */

static void GradientModulus2D( float *gradient_modulus,
			float *derivative_along_X,
			float *derivative_along_Y,
			int length )
{
  register int i;
  register float *norme = gradient_modulus;
  register float *gx = derivative_along_X;
  register float *gy = derivative_along_Y;
  
  for ( i=0; i<length; i++, norme++, gx++, gy++ )
    *norme = sqrt( (*gx)*(*gx) + (*gy)*(*gy) );
}

static void GradientModulus3D( float *gradient_modulus,
			float *derivative_along_X,
			float *derivative_along_Y,
			float *derivative_along_Z,
			int length )
{
  register int i;
  register float *norme = gradient_modulus;
  register float *gx = derivative_along_X;
  register float *gy = derivative_along_Y;
  register float *gz = derivative_along_Z;
  
  for ( i=0; i<length; i++, norme++, gx++, gy++, gz++ )
    *norme = sqrt( (*gx)*(*gx) + (*gy)*(*gy) + (*gz)*(*gz) );
}




















int Gradient_On_Laplacian_ZeroCrossings_2D ( void *bufferIn, bufferType typeIn,
					     void *bufferOut, bufferType typeOut,
					     int *bufferDims, int *borderLengths,
					     float *filterCoefs, 
					     recursiveFilterType filterType )
{
  char *proc = "Gradient_On_Laplacian_ZeroCrossings_2D";
  float *theGR = NULL;
  float *theXX = NULL;
  float *theYY = NULL;

  derivativeOrder Xderiv[3]  = { DERIVATIVE_1_EDGES, SMOOTHING,          NODERIVATIVE };
  derivativeOrder Yderiv[3]  = { SMOOTHING,          DERIVATIVE_1_EDGES, NODERIVATIVE };
  derivativeOrder XXderiv[3] = { DERIVATIVE_2,       SMOOTHING,          NODERIVATIVE };
  derivativeOrder YYderiv[3] = { SMOOTHING,          DERIVATIVE_2,       NODERIVATIVE };
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
    theXX = (float*)malloc( 2 * dimxXdimy * sizeof( float ) );
  } else {
    theXX = (float*)malloc( 3 * dimxXdimy * sizeof( float ) );
  }
  

  if ( theXX == NULL ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to allocate auxiliary buffer.\n" );
    }
    return( EXIT_ON_FAILURE );
  }

  theGR  = theXX;
  theGR += dimxXdimy;

  if ( typeOut != FLOAT ) {
    theYY  = theGR;
    theYY += dimxXdimy;
  }
  
  
  
  for ( z=0; z<bufferDims[2]; z++ ) {

    if ( typeOut == FLOAT ) {
      theYY = ((float*)bufferOut) + z * dimxXdimy;
    }
    
    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theXX, FLOAT, 
				  sliceDims, borderLengths,
				  Xderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theGR, FLOAT, 
				  sliceDims, borderLengths,
				  Yderiv, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }

    GradientModulus2D( theGR, theGR, theXX, dimxXdimy );

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
    
    
    /* 
     * theYY = laplacian
     */
    for ( i=0; i<dimxXdimy; i++ ) theYY[i] += theXX[i];

    /*
     * theXX = zero-crossings
     */ 
    if ( Extract_ZeroCrossing_2D( theYY, FLOAT, theXX, FLOAT, sliceDims ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute zero crossing.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }

    

    if ( Mask_With_Image( theGR, FLOAT, theXX, FLOAT, theYY, FLOAT, sliceDims ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to mask with zero crossing.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
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
      ConvertBuffer( theYY, FLOAT, sliceOut, typeOut, dimxXdimy );
    }
  }

  return( EXIT_ON_SUCCESS );
}





















int Gradient_On_GradientHessianGradient_ZeroCrossings_2D ( void *bufferIn,
		   bufferType typeIn,
		   void *bufferOut,
		   bufferType typeOut,
		   int *bufferDims,
		   int *borderLengths,
		   float *filterCoefs,
		   recursiveFilterType filterType )
{
  char *proc = "Gradient_On_GradientHessianGradient_ZeroCrossings_2D";
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

  void *sliceOut = NULL;

  double gx, gy;

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

    if ( typeOut == FLOAT ) {
      theXY = ((float*)bufferOut) + z * dimxXdimy;
    }
    
    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theX, FLOAT, 
				  sliceDims, borderLengths,
				  Ysmooth, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y^0 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }

    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theY, FLOAT, 
				  sliceDims, borderLengths,
				  Xsmooth, filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X^0 derivative.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }




    if ( RecursiveFilterOnBuffer( bufferIn, typeIn, theXY, FLOAT, 
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


    
    /*
     * theXY = gradient . Hessian * gradient
     */
    for ( i=0; i<dimxXdimy; i++ ) {
      gx = theX[i];
      gy = theY[i];
      theXY[i] = gx * ( theXX[i] * gx + theXY[i] * gy ) +
	         gy * ( theXY[i] * gx + theYY[i] * gy );
    }
    
    /*
     * theYY = gradient modulus
     */

    GradientModulus2D( theYY, theX, theY, dimxXdimy );

    /*
     * theXX = zero-crossings
     */ 
    if ( Extract_ZeroCrossing_2D( theXY, FLOAT, theXX, FLOAT, sliceDims ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute zero crossing.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
    }



    if ( Mask_With_Image( theYY, FLOAT, theXX, FLOAT, theXY, FLOAT, sliceDims ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to mask with zero crossing.\n" );
      }
      free( theXX );
      return( EXIT_ON_FAILURE );
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






















