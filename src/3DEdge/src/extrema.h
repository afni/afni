/*************************************************************************
 * extrema.h - tools for non-maxima gradient point suppression
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


#ifndef _extrema_h_
#define _extrema_h_

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <typedefs.h>
#include <recbuffer.h>
#include <zcross.h>

/* 2D Edge extraction by suppression of the non-maxima of the gradient.
 *
 * DESCRIPTION:
 * Filtering is achieved by using Deriche's filters or recursive
 * approximations of the gaussian (and its derivative).
 *
 * The 3D buffer is processed slice by slice, and a 2D edge
 * detection is performed independently in each slice.
 *
 * PARAMETERS:
 *
 * - bufferDims[0] is the dimension along X,
 *
 *   bufferDims[1] is the dimension along Y,
 *
 *   bufferDims[2] is the dimension along Z.
 *
 * - borderLengths[0] is the number of points to be
 *   added at both ends of each X line (if
 *   positive). The value of each endpoint is
 *   repeated borderLengths[0] times to produce
 *   a line of length
 *   bufferDims[0] + 2 * borderLengths[0].
 *
 *   borderLengths[1] is the number of points to be
 *   added at both ends of each Y line.
 *
 * - filterCoefs[0] is the coefficient of the filter
 *   to be applied along direction X.
 *
 *   filterCoefs[1] is the coefficient of the filter
 *   to be applied along direction Y.
 *
 * - filterType is the type of recursive filter to
 *   be applied to the 3D buffer.
 *
 * RETURN:
 *
 * - 0 in case of error
 *
 * - 1 if successful
 *
 * SEE:
 *
 * - bufferType.
 *
 * - recursiveFilterType.
 *
 * - InitRecursiveCoefficients
 *
 * REFERENCES:
 *
 * - "Optimal edge detection using recursive filtering", R. Deriche,
 *   International Journal of Computer Vision, pp 167-187, 1987.
 *
 * - "Recursive filtering and edge tracking: two primary tools
 *    for 3-D edge detection", O. Monga, R. Deriche,
 *   G. Malandain and J.-P. Cocquerez, Image and Vision
 *   Computing 4:9, pp 203-214, August 1991.
 */
extern int Extract_Gradient_Maxima_2D( void *bufferIn, /* input buffer */
				       bufferType typeIn, /* type of the input buffer */
				       void *bufferOut, /* output buffer */
				       bufferType typeOut, /* type of the output buffer */
				       int *bufferDims, /* buffers' dimensions */
				       int *borderLengths, /* number of points to be added at both ends when filtering a line */
				       float *filterCoefs, /* coefficients of the filters for each direction */
				       recursiveFilterType filterType  /* type of the recursive filter to be used */
				       );

/* 3D Edge extraction by suppression of the non-maxima of the gradient.
 *
 * DESCRIPTION:
 * Filtering is achieved by using Deriche's filters or recursive
 * approximations of the gaussian (and its derivative).
 *
 * If the Z dimension (bufferDims[2]) is not large
 * enough, we perform a 2D edge detection.
 *
 * PARAMETERS:
 *
 * - bufferDims[0] is the dimension along X,
 *
 *   bufferDims[1] is the dimension along Y,
 *
 *   bufferDims[2] is the dimension along Z.
 *
 * - borderLengths[0] is the number of points to be
 *   added at both ends of each X line (if
 *   positive). The value of each endpoint is
 *   repeated borderLengths[0] times to produce
 *   a line of length
 *   bufferDims[0] + 2 * borderLengths[0].
 *
 *   borderLengths[1] is the number of points to be
 *   added at both ends of each Y line.
 *
 *   borderLengths[2] is the number of points to be
 *   added at both ends of each Z line.
 *
 * - filterCoefs[0] is the coefficient of the filter
 *   to be applied along direction X.
 *
 *   filterCoefs[1] is the coefficient of the filter
 *   to be applied along direction Y.
 *
 *   filterCoefs[2] is the coefficient of the filter
 *   to be applied along direction Z.
 *
 * - filterType is the type of recursive filter to
 *   be applied to the 3D buffer.
 *
 * RETURN:
 *
 * - 0 in case of error
 *
 * - 1 if successful
 *
 * SEE:
 *
 * - bufferType.
 *
 * - recursiveFilterType.
 *
 * - InitRecursiveCoefficients
 *
 * REFERENCES:
 *
 * - "Optimal edge detection using recursive filtering", R. Deriche,
 *   International Journal of Computer Vision, pp 167-187, 1987.
 *
 * - "Recursive filtering and edge tracking: two primary tools
 *    for 3-D edge detection", O. Monga, R. Deriche,
 *   G. Malandain and J.-P. Cocquerez, Image and Vision
 *   Computing 4:9, pp 203-214, August 1991.
 */
extern int Extract_Gradient_Maxima_3D( void *bufferIn, /* input buffer */
				       bufferType typeIn, /* type of the input buffer */
				       void *bufferOut, /* output buffer */
				       bufferType typeOut, /* type of the output buffer */
				       int *bufferDims, /* buffers' dimensions */
				       int *borderLengths, /* number of points to be added at both ends when filtering a line */
				       float *filterCoefs, /* coefficients of the filters for each direction */
				       recursiveFilterType filterType  /* type of the recursive filter to be used */
				       );



/* Extract the gradient extrema in 2D.
 *
 * DESCRIPTION:
 * In fact, this procedure suppresses from
 * the modulus buffer the non-maxima of the
 * gradient's modulus in the direction
 * of the direction of the 2D gradient.
 *
 * To decide if a point is a gradient extrema,
 * we look towards +/- the direction of the gradient
 * at a distance of 1. If both bilinear interpolated
 * modulus are smaller than the one of the current
 * point, then we have a gradient extrema.
 *
 * PARAMETERS:
 *
 * - bufferDims[0] is the dimension along X,
 *
 *   bufferDims[1] is the dimension along Y.
 *
 */
extern void Remove_Gradient_NonMaxima_Slice_2D( float *maxima, /* result buffer */
					float *gx, /* first component of the gradient */
					float *gy, /* second component of the gradient */
					float *norme, /* modulus of the gradient */
					int *bufferDims /* buffers' dimensions */
					);



/* Extract the gradient extrema in 3D.
 *
 * DESCRIPTION:
 * In fact, this procedure suppresses from
 * the modulus buffer the non-maxima of the
 * gradient's modulus in the direction
 * of the direction of the 3D gradient.
 *
 * To decide if a point is a gradient extrema,
 * we look towards +/- the direction of the gradient
 * at a distance of 1. If both bilinear interpolated
 * modulus are smaller than the one of the current
 * point, then we have a gradient extrema.
 *
 * PARAMETERS:
 *
 * - bufferDims[0] is the dimension along X,
 *
 *   bufferDims[1] is the dimension along Y.
 *
 * - (*norme[0]) is the buffer of the gradient's
 *   modulus of the previous slice.
 *
 *   (*norme[1]) is the buffer of the gradient's
 *   modulus of the current slice.
 *
 *   (*norme[2]) is the buffer of the gradient's
 *   modulus of the next slice.
 */
extern void Remove_Gradient_NonMaxima_Slice_3D( float *maxima, /* result buffer */
					float *gx, /* first component of the gradient */
					float *gy, /* second component of the gradient */
					float *gz, /* third component of the gradient */
					float **norme, /* modulus of the gradient */
					int *bufferDims /* buffers' dimensions */
					);



/* Set epsilon value for gradient modulus.
 *
 * DESCRIPTION:
 * A point will be considered as a potential gradient
 * extremum if and only if its modulus is greater
 * than epsilon.
 *
 * Only positive value of epsilon are considered.
 *
 * Default value is 0.5.
 *
 * If images are normalized, i.e.,
 * their values range is [0,1], a "good" epsilon
 * value is 0.00001 (because 1/2^16 = .0000152587890625
 * and medical images are usually 16 bits).
 */
extern void SetGradientExtremaEpsilon( double epsilon /* epsilon value */ );



/* Set epsilon value for gradient derivatives.
 *
 * DESCRIPTION:
 * A point M is considered as a gradient extrema
 * if its modulus is larger than the ones of
 * M+G and M-G where G is the normalized gradient
 * (the unit vector in the direction of the gradient).
 *
 * The modulus for both M+G and M-G will be generally
 * estimated with a [bi,tri]linear interpolation.
 * However, the coefficient of this interpolation
 * for M+G are the same (with a permutation) than the
 * ones of M-G. For efficiency, we use them again.
 *
 * However, a probleme may occur when one of the gradient
 * component is equal to one: using the same coefficients
 * with a permutation leads to errors.
 * Thus, if one gradient derivative is larger (in absolute value)
 * than (1.0 - epsilon), we use the nearest modulus value as
 * the estimated value (which makes sense).
 *
 * By default, epsilon is set to 0.01.
 */
extern void SetGradientDerivativeEpsilon( double epsilon /* epsilon value */ );



/* Turn on verbose mode.
 *
 * DESCRIPTION:
 * Some information will be written on stderr when processing.
 */
extern void GradientExtrema_verbose ( );



/* Turn off verbose mode.
 *
 * DESCRIPTION:
 * Nothing will be written on stderr when processing.
 * Exactly the contrary of GradientExtrema_verbose ().
 */
extern void GradientExtrema_noverbose ( );








#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _extrema_h_ */
