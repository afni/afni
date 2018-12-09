/*************************************************************************
 * extrema.c - tools for non-maxima gradient point suppression
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

#include <extrema.h>

static int _VERBOSE_ = 0;

/*
 * epsilon value to select gradient extrema candidates
 */
static double _EPSILON_NORM_ = 0.5;

/*
 * epsilon value to decide of the interpolation type.
 * If one derivative's absolute value is larger than this
 * epsilon (close to one), then we use the nearest value
 * else we perform a [bi,tri]linear interpolation.
 */
static double _EPSILON_DERIVATIVE_ = 0.95;


#define EXIT_ON_FAILURE 0
#define EXIT_ON_SUCCESS 1

/*
 * STATIC FUNCTIONS DEFINITIONS
 */

/* Compute the gradient modulus in 2-D.
 *
 * gradient_modulus = sqrt (
 * derivative_along_X*derivative_along_X +
 * derivative_along_Y*derivative_along_Y ).
 */
static void GradientModulus2D( float *gradient_modulus, /* result buffer */
			       float *derivative_along_X, /* first component */
			       float *derivative_along_Y, /* second component */
			       int length /* buffers' length */
			       );


/* Compute the gradient modulus in 3-D.
 *
 * gradient_modulus = sqrt (
 * derivative_along_X*derivative_along_X +
 * derivative_along_Y*derivative_along_Y +
 * derivative_along_Z*derivative_along_Z ).
 */
static void GradientModulus3D( float *gradient_modulus, /* result buffer */
			       float *derivative_along_X, /* first component */
			       float *derivative_along_Y, /* second component */
			       float *derivative_along_Z, /* third component */
			       int length /* buffers' length */
			       );





int Extract_Gradient_Maxima_2D( void *bufferIn,
				bufferType typeIn,
				void *bufferOut,
				bufferType typeOut,
				int *bufferDims,
				int *borderLengths,
				float *filterCoefs,
				recursiveFilterType filterType )
{
  char *proc="Extract_Gradient_Maxima_2D";
  /*
   * auxiliary buffer
   */
  float *tmpBuffer = (float*)NULL;
  /*
   * Pointers
   */
  float *gx = (float*)NULL;
  float *gy = (float*)NULL;
  float *norme = (float*)NULL;
  void *sliceIn = (void*)NULL;
  void *sliceOut = (void*)NULL;
  /*
   * additional parameters for recursive filtering
   */
  derivativeOrder Xgradient[3] = { DERIVATIVE_1_EDGES, SMOOTHING, NODERIVATIVE };
  derivativeOrder Ygradient[3] = { SMOOTHING, DERIVATIVE_1_EDGES, NODERIVATIVE };
  int sliceDims[3];
  /*
   *
   */
  int z, dimxXdimy;

  /*
   * We check the buffers' dimensions.
   */
  if ( (bufferDims[0] <= 0) || (bufferDims[1] <= 0) || (bufferDims[2] <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  dimxXdimy = bufferDims[0] * bufferDims[1];
  sliceDims[0] = bufferDims[0];
  sliceDims[1] = bufferDims[1];
  sliceDims[2] = 1;

  /*
   * test of the coefficients
   */
  if ( (filterCoefs[0] < 0.0) || (filterCoefs[1] < 0.0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Error in %s: negative coefficient's value.\n", proc );
    return( EXIT_ON_FAILURE );
  }

  /*
   * Allocation of auxiliary buffer.
   * We need a slice buffer for each gradients' component
   * plus one for the modulus.
   */
  tmpBuffer = (float*)malloc( 3 * dimxXdimy * sizeof( float ) );
  if ( tmpBuffer == (float*)NULL ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: unable to allocate auxiliary buffer.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  norme = tmpBuffer;
  gy = norme + dimxXdimy;
  gx = gy + dimxXdimy;

  /*
   * slice by slice processing.
   *
   * For each slice, we compute both the X and Y
   * components of the gradient, its modulus,
   * and we suppress the non-maxima of the
   * gradient. Finally, we put the result
   * in the buffer bufferOut.
   *
   * An other solution may consist in computing
   * the  X and Y components of the gradient
   * for the whole 3D buffer, and performing
   * the non-maxima suppression slice
   * by slice.
   */
  for ( z=0; z<bufferDims[2]; z++ ) {
    if ( (_VERBOSE_ > 0) && (bufferDims[2] > 1) ) {
      fprintf( stderr, " %s: Processing slice #%d.\n", proc, z );
    }
    sliceIn = (void*)NULL;
    /*
     * sliceIn points towards the slice #z of
     * the buffer bufferIn.
     */
    switch( typeIn ) {
    case UCHAR :
      sliceIn = (((u8*)bufferIn) + z * dimxXdimy);
      break;
    case SCHAR :
      sliceIn = (((s8*)bufferIn) + z * dimxXdimy);
      break;
    case USHORT :
      sliceIn = (((u16*)bufferIn) + z * dimxXdimy);
      break;
    case SSHORT :
      sliceIn = (((s16*)bufferIn) + z * dimxXdimy);
      break;
    case INT :
      sliceIn = (((i32*)bufferIn) + z * dimxXdimy);
      break;
    case FLOAT :
      sliceIn = (((r32*)bufferIn) + z * dimxXdimy);
      break;
    case DOUBLE :
      sliceIn = (((r64*)bufferIn) + z * dimxXdimy);
      break;
    default :
      if ( _VERBOSE_ > 0 )
	fprintf( stderr, " Error in %s: such input type not handled.\n", proc );
      free( tmpBuffer );
      return( EXIT_ON_FAILURE );
    }
    /*
     * computing the X and Y component
     * of the gradient.
     */
    if ( RecursiveFilterOnBuffer( sliceIn, typeIn, gx, FLOAT,
				  sliceDims, borderLengths,
				  Xgradient, filterCoefs,
				  filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X gradient for slice #%d.\n", z );
      }
      free( tmpBuffer );
      return( EXIT_ON_FAILURE );
    }
    if ( RecursiveFilterOnBuffer( sliceIn, typeIn, gy, FLOAT,
				  sliceDims, borderLengths,
				  Ygradient, filterCoefs,
				  filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y gradient for slice #%d.\n", z );
      }
      free( tmpBuffer );
      return( EXIT_ON_FAILURE );
    }
    /*
     * Modulus of the gradient
     */
    GradientModulus2D( norme, gx, gy, dimxXdimy );

    /*
     * Suppression of the non maxima of the gradient
     * in the direction of the gradient.
     *
     * If the type of the result buffer bufferOut is
     * FLOAT, then we compute directly the result
     * into the slice #z of the result buffer.
     * Else, we compute the suppression of the
     * non maxima into the gx buffer, and we
     * convert it into the result buffer type.
     */
    if (typeOut == FLOAT ) {
      sliceOut = (((float*)bufferOut) + z * dimxXdimy);
      Remove_Gradient_NonMaxima_Slice_2D( sliceOut, gx ,gy,
					  norme, sliceDims );
    } else {
      Remove_Gradient_NonMaxima_Slice_2D( gx, gx ,gy,
					  norme, sliceDims );
      switch( typeOut ) {
      case UCHAR :
	sliceOut = (((u8*)bufferOut) + z * dimxXdimy);
	break;
      case SCHAR :
	sliceOut = (((s8*)bufferOut) + z * dimxXdimy);
	break;
      case USHORT :
	sliceOut = (((u16*)bufferOut) + z * dimxXdimy);
	break;
      case SSHORT :
	sliceOut = (((s16*)bufferOut) + z * dimxXdimy);
	break;
      case INT :
	sliceOut = (((i32*)bufferOut) + z * dimxXdimy);
	break;
      case DOUBLE :
	sliceOut = (((r64*)bufferOut) + z * dimxXdimy);
	break;
      default :
	if ( _VERBOSE_ > 0 )
	  fprintf( stderr, " Error in %s: such output type not handled.\n", proc );
	free( tmpBuffer );
	return( EXIT_ON_FAILURE );
      }
      ConvertBuffer( gx, FLOAT, sliceOut, typeOut, dimxXdimy);
    }
  }


  free( tmpBuffer );
  return( EXIT_ON_SUCCESS );
}







int Extract_Gradient_Maxima_3D( void *bufferIn,
				bufferType typeIn,
				void *bufferOut,
				bufferType typeOut,
				int *bufferDims,
				int *borderLengths,
				float *filterCoefs,
				recursiveFilterType filterType )
{
  char *proc="Extract_Gradient_Maxima_3D";
  /*
   * auxiliary buffer
   */
  float *tmpBuffer = (float*)NULL;
  float *bufferZsmoothed = (float*)NULL;
  float *bufferZderivated = (float*)NULL;
  /*
   * Pointers
   */
  /*
   * gx[0] points toward the X gradient of the current slice
   * gx[0] points toward the X gradient of the next slice
   */
  float *gx[2] = { (float*)NULL, (float*)NULL };
  /*
   * gy: idem gx but for the Y gradient
   */
  float *gy[2] = { (float*)NULL, (float*)NULL };
  float *gz = (float*)NULL;
  /*
   * norme[0] points toward the gradient modulus of the previous slice
   * norme[1] points toward the gradient modulus of the current slice
   * norme[2] points toward the gradient modulus of the next slice
   */
  float *norme[3] = { (float*)NULL, (float*)NULL, (float*)NULL };
  float *sliceZsmoothed = (float*)NULL;
  float *pt = (float*)NULL;
  /*
   * additional parameters for recursive filtering
   */
  derivativeOrder Xgradient[3] = { DERIVATIVE_1_EDGES, SMOOTHING, NODERIVATIVE };
  derivativeOrder Ygradient[3] = { SMOOTHING, DERIVATIVE_1_EDGES, NODERIVATIVE };
  derivativeOrder Zgradient[3] = { SMOOTHING, SMOOTHING, DERIVATIVE_1_EDGES };
  derivativeOrder Zsmoothing[3] = { NODERIVATIVE, NODERIVATIVE, SMOOTHING };
  int sliceDims[3];
  /*
   *
   */
  int z, dimxXdimy;

  /*
   * We check the buffers' dimensions.
   */
  if ( (bufferDims[0] <= 0) || (bufferDims[1] <= 0) || (bufferDims[2] <= 0) ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Fatal error in %s: improper buffer's dimension.\n", proc );
    return( EXIT_ON_FAILURE );
  }

  /*
   * May we perform a 3D edge detection?
   */
  if ( bufferDims[2] <= 4 ) {
    if ( _VERBOSE_ > 0 )
      fprintf( stderr, " Warning in %s: switch to 2D edge extraction.\n", proc );
    return( Extract_Gradient_Maxima_2D( bufferIn, typeIn,
					bufferOut, typeOut,
					bufferDims, borderLengths,
					filterCoefs, filterType ) );
  }

  /*
   *
   */
  dimxXdimy = bufferDims[0] * bufferDims[1];
  sliceDims[0] = bufferDims[0];
  sliceDims[1] = bufferDims[1];
  sliceDims[2] = 1;

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
   * Allocation of auxiliary buffers.
   *
   * We need a 3D buffer for the Z component of the
   * gradient, plus a 3D buffer for the 3D buffer
   * smoothed along Z, plus 7 2D buffers for the
   * X component of the gradient in both the current
   * and the next slices, idem for the Y component,
   * idem for the modulus plus one 2D buffer for
   * the modulua in the previous slice.
   *
   * If the buffer bufferOut is of type FLOAT,
   * we use it as the Z component of the gradient.
   *
   * This Z component will be used to stored the
   * extrema of the gradient.
   */
  tmpBuffer = (float*)malloc( 7 * dimxXdimy * sizeof( float ) );
  if ( tmpBuffer == (float*)NULL ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to allocate auxiliary buffer.\n" );
    }
    return( EXIT_ON_FAILURE );
  }
  gx[0] = tmpBuffer;
  gx[1] = gx[0] + dimxXdimy;
  gy[0] = gx[1] + dimxXdimy;
  gy[1] = gy[0] + dimxXdimy;
  norme[0] = gy[1] + dimxXdimy;
  norme[1] = norme[0] + dimxXdimy;
  norme[2] = norme[1] + dimxXdimy;

  bufferZsmoothed = (float*)malloc( bufferDims[2] * dimxXdimy * sizeof( float ) );
  if ( bufferZsmoothed == (float*)NULL ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to allocate auxiliary first 3D buffer.\n" );
    }
    free( tmpBuffer );
    return( EXIT_ON_FAILURE );
  }

  if ( typeOut == FLOAT ) {
    bufferZderivated = bufferOut;
  } else {
    bufferZderivated = (float*)malloc( bufferDims[2] * dimxXdimy * sizeof( float ) );
    if ( bufferZderivated == (float*)NULL ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to allocate auxiliary first 3D buffer.\n" );
      }
      free( tmpBuffer );
      free( bufferZsmoothed );
      return( EXIT_ON_FAILURE );
    }
  }

  /*
   * Computation of the Z component of the gradient.
   * Computation of the input buffer smoothed along Z.
   */
  if ( RecursiveFilterOnBuffer( bufferIn, typeIn,
				bufferZderivated, FLOAT,
				bufferDims, borderLengths,
				Zgradient, filterCoefs,
				filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Z gradient.\n" );
    }
    free( tmpBuffer );
    free( bufferZsmoothed );
    if ( typeOut != FLOAT ) free( bufferZderivated );
    return( EXIT_ON_FAILURE );
  }

  if ( RecursiveFilterOnBuffer( bufferIn, typeIn,
				bufferZsmoothed, FLOAT,
				bufferDims, borderLengths,
				Zsmoothing, filterCoefs,
				filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Z gradient.\n" );
    }
    free( tmpBuffer );
    free( bufferZsmoothed );
    if ( typeOut != FLOAT ) free( bufferZderivated );
    return( EXIT_ON_FAILURE );
  }

  /*
   * First slice: extraction of 2D edges.
   *
   * - computation of the X component of the gradient
   *   for that slice
   * - idem for the Y component of the gradient
   * - computation of the modulus
   * - suppression of the 2D non maxima of the gradient
   */
  sliceZsmoothed = bufferZsmoothed;
  gz = bufferZderivated;
  if ( RecursiveFilterOnBuffer( sliceZsmoothed, FLOAT,
				gx[0], FLOAT, sliceDims,
				borderLengths, Xgradient,
				filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute X gradient of the first slice.\n" );
    }
    free( tmpBuffer );
    free( bufferZsmoothed );
    if ( typeOut != FLOAT ) free( bufferZderivated );
    return( EXIT_ON_FAILURE );
  }
  if ( RecursiveFilterOnBuffer( sliceZsmoothed, FLOAT,
				gy[0], FLOAT, sliceDims,
				borderLengths, Ygradient,
				filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Y gradient of the first slice.\n" );
    }
    free( tmpBuffer );
    free( bufferZsmoothed );
    if ( typeOut != FLOAT ) free( bufferZderivated );
    return( EXIT_ON_FAILURE );
  }
  GradientModulus3D( norme[1], gx[0], gy[0], gz, dimxXdimy );
  Remove_Gradient_NonMaxima_Slice_2D( gz, gx[0], gy[0],
				      norme[1], sliceDims );

  /*
   * The first slice is already processed.
   *
   * We prepare the processing of the next slice.
   * - computation of the X component of the gradient
   *   for that slice
   * - idem for the Y component of the gradient
   * - computation of the modulus
   */
  sliceZsmoothed += dimxXdimy;
  gz += dimxXdimy;
  if ( RecursiveFilterOnBuffer( sliceZsmoothed, FLOAT,
				gx[1], FLOAT, sliceDims,
				borderLengths, Xgradient,
				filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute X gradient of the second slice.\n" );
    }
    free( tmpBuffer );
    free( bufferZsmoothed );
    if ( typeOut != FLOAT ) free( bufferZderivated );
    return( EXIT_ON_FAILURE );
  }
  if ( RecursiveFilterOnBuffer( sliceZsmoothed, FLOAT,
				gy[1], FLOAT, sliceDims,
				borderLengths, Ygradient,
				filterCoefs, filterType ) == 0 ) {
    if ( _VERBOSE_ > 0 ) {
      fprintf( stderr, " Fatal error in %s:", proc );
      fprintf( stderr, " unable to compute Y gradient of the second slice.\n" );
    }
    free( tmpBuffer );
    free( bufferZsmoothed );
    if ( typeOut != FLOAT ) free( bufferZderivated );
    return( EXIT_ON_FAILURE );
  }
  GradientModulus3D( norme[2], gx[1], gy[1], gz, dimxXdimy );

  /*
   * slice by slice processing
   */
  for ( z=1; z<bufferDims[2]-1; z++ ) {
    /*
     * slices permutations
     */
    pt = gx[0]; gx[0] = gx[1]; gx[1] = pt;
    pt = gy[0]; gy[0] = gy[1]; gy[1] = pt;
    pt = norme[0]; norme[0] = norme[1];
    norme[1] = norme[2]; norme[2] = pt;
    /*
     * gx[0] and gy[0] are the X and Y components
     * of the gradient of the current slice.
     * gx[1] and gy[1] are the X and Y components
     * of the gradient of the next slice.
     * norme[0] is the gradient modulus of the previous slice,
     * norme[1] is the gradient modulus of the current slice,
     * norme[2] is the gradient modulus of the next slice.
     */
    /*
     * Processing of the next slice.
     * - computation of the X component of the gradient
     *   for that slice
     * - idem for the Y component of the gradient
     * - computation of the modulus
     */
    sliceZsmoothed += dimxXdimy;
    gz += dimxXdimy;
    if ( RecursiveFilterOnBuffer( sliceZsmoothed, FLOAT,
				  gx[1], FLOAT, sliceDims,
				  borderLengths, Xgradient,
				  filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute X gradient of slice #%d.\n", z+1 );
      }
      free( tmpBuffer );
      free( bufferZsmoothed );
      if ( typeOut != FLOAT ) free( bufferZderivated );
      return( EXIT_ON_FAILURE );
    }
    if ( RecursiveFilterOnBuffer( sliceZsmoothed, FLOAT,
				  gy[1], FLOAT, sliceDims,
				  borderLengths, Ygradient,
				  filterCoefs, filterType ) == 0 ) {
      if ( _VERBOSE_ > 0 ) {
	fprintf( stderr, " Fatal error in %s:", proc );
	fprintf( stderr, " unable to compute Y gradient of slice #%d.\n", z+1 );
      }
      free( tmpBuffer );
      free( bufferZsmoothed );
      if ( typeOut != FLOAT ) free( bufferZderivated );
      return( EXIT_ON_FAILURE );
    }
    GradientModulus3D( norme[2], gx[1], gy[1], gz, dimxXdimy );
    /*
     * suppression of the 3D non maxima of the gradient.
     */
    gz -= dimxXdimy;
    Remove_Gradient_NonMaxima_Slice_3D( gz, gx[0], gy[0], gz, norme, sliceDims );
    gz += dimxXdimy;
  }

  /*
   * last slice
   *
   * Components and moduls of the gradient are
   * already computed.
   *
   * - 2D suppression of the non maxima
   */
  Remove_Gradient_NonMaxima_Slice_2D( gz, gx[1], gy[1],
				      norme[2], sliceDims );
  /*
   * conversion of the buffer bufferZderivated of type FLOAT
   * into the buffer bufferOut.
   */

  if (typeOut != FLOAT ) {
    ConvertBuffer( bufferZderivated, FLOAT,
		   bufferOut, typeOut, bufferDims[2]*dimxXdimy);
  }

  free( tmpBuffer );
  free( bufferZsmoothed );
  if ( typeOut != FLOAT ) free( bufferZderivated );
  return( EXIT_ON_SUCCESS );
}







void Remove_Gradient_NonMaxima_Slice_2D( float *maxima,
				 float *gx,
				 float *gy,
				 float *norme,
				 int *bufferDims )
{
  /*
   * the buffer norme[0] contains the gradient modulus of the
   * previous slice, the buffer norme[1] the ones of the
   * slice under study, while norme[2] containes the ones
   * of the next slice.
   */
  /*
   * dimensions
   */
  register int dimx = bufferDims[0];
  int dimy = bufferDims[1];
  int dimxMinusOne = dimx - 1;
  int dimxPlusOne = dimx + 1;
  int dimyMinusOne = dimy - 1;
  /*
   * pointers
   */
  register float *fl_pt1 = (float*)NULL;
  register float *fl_pt2 = (float*)NULL;
  register float *fl_max = (float*)NULL;
  register float *fl_nor = (float*)NULL;
  register float *fl_upper_left = (float*)NULL;
  /*
   * coordinates and vector's components
   */
  register int x, y;
  register double normalized_gx;
  register double normalized_gy;
  register double x_point_to_be_interpolated;
  register double y_point_to_be_interpolated;
  int x_upper_left_corner;
  int y_upper_left_corner;
  /*
   * coefficients
   */
  register double dx, dy, dxdy;
  double c00, c01, c10, c11;
  /*
   * modulus
   */
  double interpolated_norme;

  /*
   * we set the image border to zero.
   * First the borders along X direction,
   * second, the borders along the Y direction.
   */
  fl_pt1 = maxima;
  fl_pt2 = maxima + (dimy-1)*dimx;
  for (x=0; x<dimx; x++, fl_pt1++, fl_pt2++ )
    *fl_pt1 = *fl_pt2 = 0.0;
  fl_pt1 = maxima + dimx;
  fl_pt2 = maxima + dimx + dimx - 1;
  for (y=1; y<dimy-1; y++, fl_pt1+=dimx, fl_pt2+=dimx )
    *fl_pt1 = *fl_pt2 = 0.0;

  /*
   * We investigate the middle of the image.
   */
  /*
   * Pointers are set to the first point
   * to be processed.
   */
  fl_max = maxima + dimx + 1;
  fl_pt1 = gx + dimx + 1;
  fl_pt2 = gy + dimx + 1;
  fl_nor = norme + dimx + 1;
  for ( y=1; y<dimyMinusOne; y++, fl_max+=2, fl_pt1+=2, fl_pt2+=2, fl_nor+=2 )
  for ( x=1; x<dimxMinusOne; x++, fl_max++,  fl_pt1++,  fl_pt2++,  fl_nor++ ) {
    /*
     * If the modulus is too small, go to the next point.
     */
    if ( *fl_nor < _EPSILON_NORM_ ) {
      *fl_max = 0.0;
      continue;
    }
    /*
     * We normalize the vector gradient.
     */
    normalized_gx = *fl_pt1 / *fl_nor;
    normalized_gy = *fl_pt2 / *fl_nor;

    /*
     * May we use the nearest value?
     */
    if ( (-normalized_gx > _EPSILON_DERIVATIVE_) ||
	 (normalized_gx > _EPSILON_DERIVATIVE_) ||
	 (-normalized_gy > _EPSILON_DERIVATIVE_) ||
	 (normalized_gy > _EPSILON_DERIVATIVE_) ) {
      /*
       * First point to be interpolated.
       */
      x_upper_left_corner = (int)( (double)x + normalized_gx + 0.5 );
      y_upper_left_corner = (int)( (double)y + normalized_gy + 0.5 );
      interpolated_norme = *(norme + (x_upper_left_corner + y_upper_left_corner * dimx));
      if ( *fl_nor <= interpolated_norme ) {
	*fl_max = 0.0;
	continue;
      }
      /*
       * Second point to be interpolated.
       */
      x_upper_left_corner = (int)( (double)x - normalized_gx + 0.5 );
      y_upper_left_corner = (int)( (double)y - normalized_gy + 0.5 );
      interpolated_norme = *(norme + (x_upper_left_corner + y_upper_left_corner * dimx));
      if ( *fl_nor < interpolated_norme ) {
	*fl_max = 0.0;
	continue;
      }
      /*
       * We found a gradient extrema.
       */
      *fl_max = *fl_nor;
      continue;
    }


    /*
     * From here we perform a bilinear interpolation
     */

    /*
     * First point to be interpolated.
     * It is the current point + an unitary vector
     * in the direction of the gradient.
     * It must be inside the image.
     */
    x_point_to_be_interpolated = (double)x + normalized_gx;
    y_point_to_be_interpolated = (double)y + normalized_gy;
    if ( (x_point_to_be_interpolated < 0.0) ||
	 (x_point_to_be_interpolated >= dimxMinusOne) ||
	 (y_point_to_be_interpolated < 0.0) ||
	 (y_point_to_be_interpolated >= dimyMinusOne) ) {
      *fl_max = 0.0;
      continue;
    }
    /*
     * Upper left corner,
     * coordinates of the point to be interpolated
     * with respect to this corner.
     */
    x_upper_left_corner = (int)x_point_to_be_interpolated;
    y_upper_left_corner = (int)y_point_to_be_interpolated;
    dx = x_point_to_be_interpolated - (double)x_upper_left_corner;
    dy = y_point_to_be_interpolated - (double)y_upper_left_corner;
    dxdy = dx * dy;
    /*
     * bilinear interpolation of the gradient modulus
     * norme[x_point_to_be_interpolated, y_point_to_be_interpolated] =
     *   norme[0,0] * ( 1 - dx) * ( 1 - dy ) +
     *   norme[1,0] * ( dx ) * ( 1 - dy ) +
     *   norme[0,1] * ( 1 - dx ) * ( dy ) +
     *   norme[1,1] * ( dx ) * ( dy )
     */
    c00 = 1.0 - dx - dy + dxdy;
    c10 = dx - dxdy;
    c01 = dy - dxdy;
    c11 = dxdy;
    fl_upper_left = norme + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme = *(fl_upper_left) * c00 +
      *(fl_upper_left + 1) * c10 +
      *(fl_upper_left + dimx) * c01 +
      *(fl_upper_left + dimxPlusOne) * c11;
    /*
     * We compare the modulus of the point with the
     * interpolated modulus. It must be larger to be
     * still considered as a potential gradient extrema.
     *
     * Here, we consider that it is strictly superior.
     * The next comparison will be superior or equal.
     * This way, the extrema is in the light part of the
     * image.
     * By inverting both tests, we can put it in the
     * dark side of the image.
     */
    if ( *fl_nor <= interpolated_norme ) {
      *fl_max = 0.0;
      continue;
    }
    /*
     * Second point to be interpolated.
     * It is the current point - an unitary vector
     * in the direction of the gradient.
     * It must be inside the image.
     */
    x_point_to_be_interpolated = (double)x - normalized_gx;
    y_point_to_be_interpolated = (double)y - normalized_gy;
    if ( (x_point_to_be_interpolated < 0.0) ||
	 (x_point_to_be_interpolated >= dimxMinusOne) ||
	 (y_point_to_be_interpolated < 0.0) ||
	 (y_point_to_be_interpolated >= dimyMinusOne) ) {
      *fl_max = 0.0;
      continue;
    }
    /*
     * Upper left corner.
     */
    x_upper_left_corner = (int)x_point_to_be_interpolated;
    y_upper_left_corner = (int)y_point_to_be_interpolated;
    /* we do not recompute the coefficients
    dx = x_point_to_be_interpolated - (double)x_upper_left_corner;
    dy = y_point_to_be_interpolated - (double)y_upper_left_corner;
    dxdy = dx * dy;
    */
    /*
     * We may use the previous coefficients.
     * norme[x_point_to_be_interpolated, y_point_to_be_interpolated] =
     *   norme[0,0] * c11 +
     *   norme[1,0] * c01 +
     *   norme[0,1] * c10 +
     *   norme[1,1] * c00
     *
     * WARNING: it works only if the cases where one derivative is close
     *          to -/+ 1 are already be independently processed, else
     *          it may lead to errors.
     */
    /* we do not recompute the coefficients
    c00 = 1.0 - dx - dy + dxdy;
    c10 = dx - dxdy;
    c01 = dy - dxdy;
    c11 = dxdy;
    fl_upper_left = norme + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme = *(fl_upper_left) * c00 +
      *(fl_upper_left + 1) * c10 +
      *(fl_upper_left + dimx) * c01 +
      *(fl_upper_left + dimxPlusOne) * c11;
    */
    fl_upper_left = norme + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme = *(fl_upper_left) * c11 +
      *(fl_upper_left + 1) * c01 +
      *(fl_upper_left + dimx) * c10 +
      *(fl_upper_left + dimxPlusOne) * c00;
    /*
     * Last test to decide whether or not we
     * have an extrema
     */
    if ( *fl_nor < interpolated_norme ) {
      *fl_max = 0.0;
      continue;
    }
    /*
     * We found a gradient extrema.
     */
    *fl_max = *fl_nor;
  }
}






void Remove_Gradient_NonMaxima_Slice_3D( float *maxima,
				 float *gx,
				 float *gy,
				 float *gz,
				 float **norme,
				 int *bufferDims )
{
  /*
   * the buffer norme[0] contains the gradient modulus of the
   * previous slice, the buffer norme[1] the ones of the
   * slice under study, while norme[2] containes the ones
   * of the next slice.
   */
  /*
   * dimensions
   */
  register int dimx = bufferDims[0];
  int dimy = bufferDims[1];
  int dimxMinusOne = dimx - 1;
  int dimxPlusOne = dimx + 1;
  int dimyMinusOne = dimy - 1;
  /*
   * pointers
   */
  register float *fl_pt1 = (float*)NULL;
  register float *fl_pt2 = (float*)NULL;
  register float *fl_pt3 = (float*)NULL;
  register float *fl_max = (float*)NULL;
  register float *fl_nor = (float*)NULL;
  register float *fl_upper_left = (float*)NULL;
  /*
   * coordinates and vector's components
   */
  register int x, y;
  int z;
  register double normalized_gx;
  register double normalized_gy;
  register double normalized_gz;
  register double x_point_to_be_interpolated;
  register double y_point_to_be_interpolated;
  register double z_point_to_be_interpolated;
  int x_upper_left_corner;
  int y_upper_left_corner;
  int z_upper_left_corner;
  /*
   * coefficients
   */
  register double dx, dy, dz;
  register double dxdy, dxdz, dydz;
  double c000, c010, c100, c110;
  double c001, c011, c101, c111;
  /*
   * modulus
   */
  double interpolated_norme;

  /*
   * we set the image border to zero.
   * First the borders along X direction,
   * second, the borders along the Y direction.
   */
  fl_pt1 = maxima;
  fl_pt2 = maxima + (dimy-1)*dimx;
  for (x=0; x<dimx; x++, fl_pt1++, fl_pt2++ )
    *fl_pt1 = *fl_pt2 = 0.0;
  fl_pt1 = maxima + dimx;
  fl_pt2 = maxima + dimx + dimx - 1;
  for (y=1; y<dimy-1; y++, fl_pt1+=dimx, fl_pt2+=dimx )
    *fl_pt1 = *fl_pt2 = 0.0;

  /*
   * We investigate the middle of the image.
   */
  /*
   * Pointers are set to the first point
   * to be processed.
   */
  fl_max = maxima + dimx + 1;
  fl_pt1 = gx + dimx + 1;
  fl_pt2 = gy + dimx + 1;
  fl_pt3 = gz + dimx + 1;
  fl_nor = norme[1] + dimx + 1;
  z = 1;
  for ( y=1; y<dimyMinusOne; y++, fl_max+=2, fl_pt1+=2, fl_pt2+=2, fl_pt3+=2, fl_nor+=2 )
  for ( x=1; x<dimxMinusOne; x++, fl_max++,  fl_pt1++,  fl_pt2++,  fl_pt3++,  fl_nor++ ) {

    /*
     * If the modulus is too small, go to the next point.
     */
    if ( *fl_nor < _EPSILON_NORM_ ) {
      *fl_max = 0.0;
      continue;
    }
    /*
     * We normalize the vector gradient.
     */
    normalized_gx = *fl_pt1 / *fl_nor;
    normalized_gy = *fl_pt2 / *fl_nor;
    normalized_gz = *fl_pt3 / *fl_nor;

    /*
     * May we use the nearest value?
     */
    if ( (-normalized_gx > _EPSILON_DERIVATIVE_) ||
	 (normalized_gx > _EPSILON_DERIVATIVE_) ||
	 (-normalized_gy > _EPSILON_DERIVATIVE_) ||
	 (normalized_gy > _EPSILON_DERIVATIVE_) ||
	 (-normalized_gz > _EPSILON_DERIVATIVE_) ||
	 (normalized_gz > _EPSILON_DERIVATIVE_) ) {
      /*
       * First point to be interpolated.
       */
      x_upper_left_corner = (int)( (double)x + normalized_gx + 0.5 );
      y_upper_left_corner = (int)( (double)y + normalized_gy + 0.5 );
      z_upper_left_corner = (int)( (double)z + normalized_gz + 0.5 );
      interpolated_norme = *(norme[z_upper_left_corner]
			     + (x_upper_left_corner + y_upper_left_corner * dimx));
      if ( *fl_nor <= interpolated_norme ) {
	*fl_max = 0.0;
	continue;
      }
      /*
       * Second point to be interpolated.
       */
      x_upper_left_corner = (int)( (double)x - normalized_gx + 0.5 );
      y_upper_left_corner = (int)( (double)y - normalized_gy + 0.5 );
      z_upper_left_corner = (int)( (double)z - normalized_gz + 0.5 );
      interpolated_norme = *(norme[z_upper_left_corner]
			     + (x_upper_left_corner + y_upper_left_corner * dimx));
      if ( *fl_nor < interpolated_norme ) {
	*fl_max = 0.0;
	continue;
      }
      /*
       * We found a gradient extrema.
       */
      *fl_max = *fl_nor;
      continue;
    }


    /*
     * From here we perform a trilinear interpolation
     */

    /*
     * First point to be interpolated.
     * It is the current point + an unitary vector
     * in the direction of the gradient.
     * It must be inside the image.
     */
    x_point_to_be_interpolated = (double)x + normalized_gx;
    y_point_to_be_interpolated = (double)y + normalized_gy;
    z_point_to_be_interpolated = (double)z + normalized_gz;
    if ( (x_point_to_be_interpolated < 0.0) ||
	 (x_point_to_be_interpolated >= dimxMinusOne) ||
	 (y_point_to_be_interpolated < 0.0) ||
	 (y_point_to_be_interpolated >= dimyMinusOne) ) {
      *fl_max = 0.0;
      continue;
    }

    /*
     * Upper left corner,
     * coordinates of the point to be interpolated
     * with respect to this corner.
     */
    x_upper_left_corner = (int)x_point_to_be_interpolated;
    y_upper_left_corner = (int)y_point_to_be_interpolated;
    z_upper_left_corner = (int)z_point_to_be_interpolated;
    dx = x_point_to_be_interpolated - (double)x_upper_left_corner;
    dy = y_point_to_be_interpolated - (double)y_upper_left_corner;
    dz = z_point_to_be_interpolated - (double)z_upper_left_corner;
    /*
     * trilinear interpolation of the gradient modulus
     * norme[x_point_to_be_interpolated,
     *       y_point_to_be_interpolated,
     *       z_point_to_be_interpolated] =
     *   norme[0,0,0] * ( 1 - dx) * ( 1 - dy ) * ( 1 - dz ) +
     *   norme[1,0,0] * ( dx ) * ( 1 - dy ) * ( 1 - dz ) +
     *   norme[0,1,0] * ( 1 - dx ) * ( dy ) * ( 1 - dz ) +
     *   norme[1,1,0] * ( dx ) * ( dy ) * ( 1 - dz ) +
     *   norme[0,0,1] * ( 1 - dx) * ( 1 - dy ) * ( dz ) +
     *   norme[1,0,1] * ( dx ) * ( 1 - dy ) * ( dz ) +
     *   norme[0,1,1] * ( 1 - dx ) * ( dy ) * ( dz ) +
     *   norme[1,1,1] * ( dx ) * ( dy ) * ( dz )
     */
    dxdy = dx * dy;
    dydz = dy * dz;
    dxdz = dx * dz;
    c111 = dxdy * dz;
    c011 = dydz - c111;
    c101 = dxdz - c111;
    c001 = dz - dxdz - c011;
    c110 = dxdy - c111;
    c010 = dy - dxdy - c011;
    c100 = dx - dxdy - c101;
    c000 = 1.0 - dx - dy + dxdy - c001;
    fl_upper_left = norme[z_upper_left_corner]
      + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme = *(fl_upper_left) * c000 +
      *(fl_upper_left + 1) * c100 +
      *(fl_upper_left + dimx) * c010 +
      *(fl_upper_left + dimxPlusOne) * c110;
    fl_upper_left = norme[z_upper_left_corner+1]
      + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme += *(fl_upper_left) * c001 +
      *(fl_upper_left + 1) * c101 +
      *(fl_upper_left + dimx) * c011 +
      *(fl_upper_left + dimxPlusOne) * c111;
    /*
     * We compare the modulus of the point with the
     * interpolated modulus. It must be larger to be
     * still considered as a potential gradient extrema.
     *
     * Here, we consider that it is strictly superior.
     * The next comparison will be superior or equal.
     * This way, the extrema is in the light part of the
     * image.
     * By inverting both tests, we can put it in the
     * dark side of the image.
     */
    if ( *fl_nor <= interpolated_norme ) {
      *fl_max = 0.0;
      continue;
    }
    /*
     * Second point to be interpolated.
     * It is the current point - an unitary vector
     * in the direction of the gradient.
     * It must be inside the image.
     */
    x_point_to_be_interpolated = (double)x - normalized_gx;
    y_point_to_be_interpolated = (double)y - normalized_gy;
    z_point_to_be_interpolated = (double)z - normalized_gz;
    if ( (x_point_to_be_interpolated < 0.0) ||
	 (x_point_to_be_interpolated >= dimxMinusOne) ||
	 (y_point_to_be_interpolated < 0.0) ||
	 (y_point_to_be_interpolated >= dimyMinusOne) ) {
      *fl_max = 0.0;
      continue;
    }
    /*
     * Upper left corner.
     */
    x_upper_left_corner = (int)x_point_to_be_interpolated;
    y_upper_left_corner = (int)y_point_to_be_interpolated;
    z_upper_left_corner = (int)z_point_to_be_interpolated;
    /* we do not recompute the coefficients
    dx = x_point_to_be_interpolated - (double)x_upper_left_corner;
    dy = y_point_to_be_interpolated - (double)y_upper_left_corner;
    dz = z_point_to_be_interpolated - (double)z_upper_left_corner;
    */
    /*
     * We use the previous coefficients.
     * norme[x_point_to_be_interpolated,
     *       y_point_to_be_interpolated,
     *       z_point_to_be_interpolated] =
     *   norme[0,0,0] * c111 +
     *   norme[1,0,0] * c011 +
     *   norme[0,1,0] * c101 +
     *   norme[1,1,0] * c001 +
     *   norme[0,0,1] * c110 +
     *   norme[1,0,1] * c010 +
     *   norme[0,1,1] * c100 +
     *   norme[1,1,1] * c000
     *

    fl_upper_left = norme[z_upper_left_corner]
      + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme = *(fl_upper_left) * c111 +
      *(fl_upper_left + 1) * c011 +
      *(fl_upper_left + dimx) * c101 +
      *(fl_upper_left + dimxPlusOne) * c001;
    fl_upper_left = norme[z_upper_left_corner+1]
      + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme += *(fl_upper_left) * c110 +
      *(fl_upper_left + 1) * c010 +
      *(fl_upper_left + dimx) * c100 +
      *(fl_upper_left + dimxPlusOne) * c000;

     *
     * WARNING: as in the 2D case it works only if the cases where one
     *          derivative is close to -/+ 1 are already be independently
     *          processed, else it may lead to errors.
     */
    /* we do not recompute the coefficients
    dxdy = dx * dy;
    dydz = dy * dz;
    dxdz = dx * dz;
    c111 = dxdy * dz;
    c011 = dydz - c111;
    c101 = dxdz - c111;
    c001 = dz - dxdz - c011;
    c110 = dxdy - c111;
    c010 = dy - dxdy - c011;
    c100 = dx - dxdy - c101;
    c000 = 1.0 - dx - dy + dxdy - c001;
    fl_upper_left = norme[z_upper_left_corner]
      + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme = *(fl_upper_left) * c000 +
      *(fl_upper_left + 1) * c100 +
      *(fl_upper_left + dimx) * c010 +
      *(fl_upper_left + dimxPlusOne) * c110;
    fl_upper_left = norme[z_upper_left_corner+1]
      + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme += *(fl_upper_left) * c001 +
      *(fl_upper_left + 1) * c101 +
      *(fl_upper_left + dimx) * c011 +
      *(fl_upper_left + dimxPlusOne) * c111;
    */

    fl_upper_left = norme[z_upper_left_corner]
      + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme = *(fl_upper_left) * c111 +
      *(fl_upper_left + 1) * c011 +
      *(fl_upper_left + dimx) * c101 +
      *(fl_upper_left + dimxPlusOne) * c001;
    fl_upper_left = norme[z_upper_left_corner+1]
      + (x_upper_left_corner + y_upper_left_corner * dimx);
    interpolated_norme += *(fl_upper_left) * c110 +
      *(fl_upper_left + 1) * c010 +
      *(fl_upper_left + dimx) * c100 +
      *(fl_upper_left + dimxPlusOne) * c000;

    /*
     * Last test to decide whether or not we
     * have an extrema
     */
    if ( *fl_nor < interpolated_norme ) {
      *fl_max = 0.0;
      continue;
    }
    /*
     * We found a gradient extrema.
     */
    *fl_max = *fl_nor;
  }
}







/* Compute the gradient modulus in 2-D.
 *
 * gradient_modulus = sqrt (
 * derivative_along_X*derivative_along_X +
 * derivative_along_Y*derivative_along_Y ).
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







void SetGradientExtremaEpsilon( double epsilon )
{
  if ( epsilon > 0.0 ) {
    _EPSILON_NORM_ = epsilon;
  }
}

void SetGradientDerivativeEpsilon( double epsilon )
{
  if ( (epsilon > 0.0) && (epsilon < 1.0) ) {
    _EPSILON_DERIVATIVE_ = 1.0 - epsilon;
  }
}





void GradientExtrema_verbose ( )
{
  _VERBOSE_ = 1;
  Recbuffer_verbose ( );
}

void GradientExtrema_noverbose ( )
{
  _VERBOSE_ = 0;
  Recbuffer_noverbose ( );
}
