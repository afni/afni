/*************************************************************************
 * connexe.h - extraction of connected components
 *
 * $Id$
 *
 * LICENSE:
 * GPL v3.0 (see gpl-3.0.txt for details)
 *
 * AUTHOR:
 * Gregoire Malandain (gregoire.malandain@inria.fr)
 * 
 * CREATION DATE: 
 * July, 8 1999
 *
 * ADDITIONS, CHANGES
 *
 * - Thu Apr 20 10:05:18 MET DST 2000, G. Malandain
 *   add #include <stdlib.h>
 *   it suppresses "warning: cast to pointer from integer of different size"
 *   when compiling with gcc
 *
 *
 */




#ifndef _connexe_h_
#define _connexe_h_

#ifdef __cplusplus
extern "C" {
#endif


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <typedefs.h>


/* OVERALL DESCRIPTION
 *
 There are a few global static variable

 _connectivity_ : the connectivity used to extract the connected components
                  it may be 4, 8 (those are 2D), 6, 10, 18, or 26 (those
		  are 3D). 
                  see Connexe_SetConnectivity()

 _minimum_size_of_components_ : the minimum size (number of points) that a
                  connected component must reach to be validated.
                  see Connexe_SetMinimumSizeOfComponents()

 _maximum_number_of_components_ : the maximal number of connected components
                  If this number is in the range 0...65535, the connected
                  components are sorted with respect to their size, and 
                  only the _maximum_number_of_components_ ones are kept.
                  see Connexe_SetMaximumNumberOfComponents().

 _VERBOSE_ : to write some information on stderr.
             see Connexe_verbose() and Connexe_noverbose().
*/




/* Counts and labels connected components.
 *
 * DESCRIPTION:
 * Extracts connected components from the input buffer. 
 * Only points with values >= 0 are considered.
 * At this point, intermediary computation is performed
 * in an buffer of type (unsigned short int). Some 
 * limitation about the number of intermediary labels
 * may appear (I never see it).
 *
 * Complete description of the method may be found in the 
 * code.
 *
 * There are three main parameters:
 * - the connectivity (see Connexe_SetConnectivity)
 *   4, 8, 6, 10, 18 or 26.
 * - the minimal size of connected components to be 
 *   kept (see Connexe_SetMinimumSizeOfComponents).
 * - the maximal number of connected components to be 
 *   kept (see Connexe_SetMaximumNumberOfComponents),
 *   the largest ones are kept.
 * 
 * It just makes the following function call
 * CountConnectedComponentsWithAllParams( bufferIn, 
 *                                        typeIn,
 *                                        bufferOut,
 *                                        typeOut,
 *                                        bufferDims,
 *                                        (double)1.0, (default threshold)
 *                                        _connectivity_,
 *                                        _minimum_size_of_components_,
 *                                        _maximum_number_of_components_,
 *                                        (int)0 (output is not binary) )
 *
 *
 * KNOWN EFFECTS:
 *
 * - If the maximal number of connected components is larger
 *   to the number of valid connected components (whose size
 *   is larger to the minimal size of connected components),
 *   they are sorted with respect to their size.
 *
 * - If the type of the output buffer is (unsigned char)
 *   and if the number of connected components exceeds
 *   255, some of them will have the same labels.
 *   (a warning should be print in verbose mode).
 *
 *
 * PARAMETERS:
 *
 * - bufferDims[0] is the dimension along X,
 *
 *   bufferDims[1] is the dimension along Y,
 *
 *   bufferDims[2] is the dimension along Y.
 *
 *
 * RETURN:
 *
 * - -1 in case of error
 *
 * - the number of selected connected components if successful (may be 0)
 *
 *
 * SEE ALSO:
 *
 * - Connexe_SetConnectivity
 *
 * - Connexe_SetMinimumSizeOfComponents
 *
 * - Connexe_SetMaximumNumberOfComponents
 *
 * - Connexe_verbose
 *
 * - Connexe_noverbose
 *
 */

extern int CountConnectedComponents( void *bufferIn, /* input buffer */
				     bufferType typeIn,  /* type of the input buffer */
				     void *bufferOut, /* output buffer */
				     bufferType typeOut, /* type of the output buffer */
				     int *bufferDims /* buffers' dimensions */
				     ); 

/* Counts and labels connected components (without global parameters)
 *
 *
 * It is quite the same function than CountConnectedComponents(), except
 * that there are more parameters, and that no global parameters will be
 * used.
 *
 * Main differences with CountConnectedComponents()
 *
 * - the input image is binarized with the parameter 'threshold'
 *   points with value greater or equal to the threshold are the 
 *   foreground (whom components will be extracted), while the others
 *   are the foreground
 *
 * - by specifying a binary output (with a non null value of 'outputIsBinary')
 *   one eliminates in a binary image the 'small' components. The binary value
 *   is the maximal value with respect to the output type (e.g. 255 for unsigned char).
 */
 
extern int CountConnectedComponentsWithAllParams( void *bufferIn, /* input buffer */
				     bufferType typeIn,  /* type of the input buffer */
				     void *bufferOut, /* output buffer */
				     bufferType typeOut, /* type of the output buffer */
				     int *bufferDims, /* buffers' dimensions */
				     double threshold, /* threshold used to binarize the input image */
				     int connectivity, /* connectivity */
				     int minNumberOfPts, /* minimal number of points that a connected
							    component must reach to be validated */
				     int maxNumberOfConnectedComponent, /* maximal number of connected
									   component, only the largest
									   ones (among the valid ones)
									   are kept */
				     int outputIsBinary /* the result is a two-valued image:
							   background and foreground */ ); 





/* Performs hysteresis thresholding.
 *
 * DESCRIPTION:
 * Extracts connected components from the input buffer. 
 * These connected components is made of points of 
 * value >= lowThreshold and contain at least one
 * point of value >= highThreshold.
 *
 * For integer types, the floating values are converted
 * to the nearest integers.
 *
 * Only points with values >= 0 are considered.
 * At this point, intermediary computation is performed
 * in an buffer of type (unsigned short int). Some 
 * limitation about the number of intermediary labels
 * may appear (I never see it).
 *
 * Complete description of the method may be found in the 
 * code.
 *
 * There are three main parameters:
 * - the connectivity (see Connexe_SetConnectivity)
 *   4, 8, 6, 10, 18 or 26.
 * - the minimal size of connected components to be 
 *   kept (see Connexe_SetMinimumSizeOfComponents).
 * - the maximal number of connected components to be 
 *   kept (see Connexe_SetMaximumNumberOfComponents),
 *   the largest ones are kept.
 *
 * It just makes the following function call
 * HysteresisThresholdingWithAllParams( bufferIn, 
 *                                      typeIn,
 *                                      bufferOut,
 *                                      typeOut,
 *                                      bufferDims,
 *                                      lowThreshold, 
 *                                      highThreshold,
 *                                      _connectivity_,
 *                                      _minimum_size_of_components_,
 *                                      (int)1,
 *                                      _maximum_number_of_components_,
 *                                      (int)1 (output is binary) )
 *
 *
 * PARAMETERS:
 *
 * - bufferDims[0] is the dimension along X,
 *
 *   bufferDims[1] is the dimension along Y,
 *
 *   bufferDims[2] is the dimension along Y.
 *
 *
 * RETURN:
 *
 * - -1 in case of error
 *
 * - the number of selected connected components if successful (may be 0)
 *
 *
 * SEE ALSO:
 *
 * - Connexe_SetConnectivity
 *
 * - Connexe_SetMinimumSizeOfComponents
 *
 * - Connexe_SetMaximumNumberOfComponents
 *
 * - Connexe_verbose
 *
 * - Connexe_noverbose
 *
 */

extern int HysteresisThresholding( void *bufferIn, /* input buffer */
				   bufferType typeIn,  /* type of the input buffer */
				   void *bufferOut, /* output buffer */
				   bufferType typeOut, /* type of the output buffer */
				   int *bufferDims, /* buffers' dimensions */
				   double lowThreshold, /* low threshold */
				   double highThreshold  /* high threshold */
				   );




/* Performs hysteresis thresholding (without global parameters)
 *
 *
 * It is quite the same function than HysteresisThresholding() except
 * that there are more parameters, and that no global parameters will be
 * used.
 *
 * Main differences with HysteresisThresholding()
 *
 * - a connected component is valid when its size is greater or equal
 *   to 'minNumberOfPtsAboveLow' (there are more than 'minNumberOfPtsAboveLow'
 *   points which have a value greater or equal to 'lowThreshold') 
 *   AND when it contains at least 'minNumberOfPtsAboveHigh' 
 *   points which have a value greater or equal to 'highThreshold'.
 *   Here, one can tune 'minNumberOfPtsAboveHigh'.
 *
 * - by specifying a non-binary output (with a null value of 'outputIsBinary')
 *   One obtains the labeled connected components.
 */
 
extern int HysteresisThresholdingWithAllParams( void *bufferIn, /* input buffer */
				     bufferType typeIn,  /* type of the input buffer */
				     void *bufferOut, /* output buffer */
				     bufferType typeOut, /* type of the output buffer */
				     int *bufferDims, /* buffers' dimensions */
				     double lowThreshold, /* low threshold */
				     double highThreshold,  /* high threshold */
				     int connectivity, /* connectivity */
				     int minNumberOfPtsAboveLow, /* requested minimum number
								    of points above the low
								    value to validate the connected
								    component */
				     int minNumberOfPtsAboveHigh, /* requested minimum number
								     of points above the high
								     value to validate the connected
								     component */
				     int maxNumberOfConnectedComponent, /* maximal number of connected
									   component, only the largest
									   ones (among the valid ones)
									   are kept */
				     int outputIsBinary /* the result is a two-valued image */ ); 





/* Changes the labels of connected components to sort them by decreasing size
 *
 * DESCRIPTION:
 * The connected components are sorted by deceasing size
 * so that the new label of the largest connected component
 * will be 1, of the second largest connected component 2,
 * ... and the new label of the smallest connected component
 * will be equal to the num,be of connected components
 *
 * RETURN:
 *  1: success
 * -1: error
 */

extern int RelabelConnectedComponentsByDecreasingSize( void *inputBuf,
						       bufferType typeIn,
						       int *theDim );



/* Set the connectivity for the extraction of connected components.
 *
 * DESCRIPTION:
 * Admitted values are 4, 8, 6, 10, 18, and 26.
 * The 2 first are 2D connectivities.
 * The 4 last are 3D connectivities.
 * The 10-connectivity is an anisotropic connectivity,
 * a mixed-up of the 2D 8-connectivity and the 3D 
 * 6-connectivity.
 *
 */
extern void Connexe_SetConnectivity( int c );

  
/* Set the minimal size of the connected components to be kept.
 *
 * DESCRIPTION:
 * The extracted components which contains less than
 * this number of points are discarded.
 */
extern void Connexe_SetMinimumSizeOfComponents( int c );


/* Set the maximum number of the connected components to be kept.
 *
 * DESCRIPTION:
 * If this number is <= 0, all the valid connected components
 * are kept.
 * If this number is > 0, the connected components are sorted
 * with respect to their size (number of points), and only 
 * the c largest components are kept. 
 *
 * A side effect is (for CountConnectedComponents()): 
 * if c is larger than the number of valid connected 
 * connected, all the components are kept, but the labels are sorted,
 * i.e. the largest connected component has label 1,
 * the second largest connected component has label 2, etc.
 *
 */
extern void Connexe_SetMaximumNumberOfComponents( int c );




/* Turn on verbose mode.
 *
 * DESCRIPTION:
 * Some information will be written on stderr when processing.
 */
extern void Connexe_verbose ( );

/* Turn off verbose mode.
 *
 * DESCRIPTION:
 * Nothing will be written on stderr when processing.
 * Exactly the contrary of Connexe_verbose().
 * It will turn off the verbose mode of recline too.
 */
extern void Connexe_noverbose ( );


#ifdef __cplusplus
}
#endif

#endif
