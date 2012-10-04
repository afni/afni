/*************************************************************************
 * zcross.h - zero-crossings
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
 * Tue Nov 28 10:06:22 MET 2000
 *
 * ADDITIONS, CHANGES
 *
 */

#ifndef _zcross_h_
#define _zcross_h_

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#include <typedefs.h>



extern void ZeroCrossings_verbose ( );
extern void ZeroCrossings_noverbose ( );

extern void ZeroCrossings_Are_Positive( );
extern void ZeroCrossings_Are_Negative( );




extern int Extract_ZeroCrossing_2D ( void *bufferIn, bufferType typeIn,
				     void *bufferOut, bufferType typeOut, 
				     int *bufferDims );




extern int Extract_PositiveZeroCrossing_2D ( void *bufferIn,
					     bufferType typeIn,
					     void *bufferOut,
					     bufferType typeOut,
					     int *bufferDims );
extern int Extract_NegativeZeroCrossing_2D ( void *bufferIn,
					     bufferType typeIn,
					     void *bufferOut,
					     bufferType typeOut,
					     int *bufferDims );



extern int Mask_With_Image( void *bufferIn,   bufferType typeIn,
		     void *bufferMask, bufferType typeMask,
		     void *bufferOut,  bufferType typeOut,
		     int *bufferDims );








extern int Gradient_On_Laplacian_ZeroCrossings_2D ( void *bufferIn,  bufferType typeIn,
						    void *bufferOut, bufferType typeOut,
						    int *bufferDims, int *borderLengths,
						    float *filterCoefs,
						    recursiveFilterType filterType );


extern int Gradient_On_GradientHessianGradient_ZeroCrossings_2D( void *bufferIn,  
								 bufferType typeIn,
						    void *bufferOut, bufferType typeOut,
						    int *bufferDims, int *borderLengths,
						    float *filterCoefs,
						    recursiveFilterType filterType );




#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _zcross_h_ */
