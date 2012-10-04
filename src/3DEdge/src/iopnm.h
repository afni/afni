/*************************************************************************
 * iopnm.h - homemade I/O procedures for PBM/PGM/PPM raw images
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
 * July, 6 1999
 *
 * ADDITIONS, CHANGES
 *
 * * Jul 15 1999 (Gregoire Malandain)
 *   add P6 (color) images. They are considered as 3D images with z=3.
 *
 */

#ifndef _iopnm_h_
#define _iopnm_h_

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#include <stdlib.h>
#include <stdio.h>

#include <fcntl.h> /* open, close */
#include <sys/stat.h> /* open, close */
#include <sys/types.h> /* open, close */
#include <string.h>


extern void *_readPnmImage( char *name, int *dimx, int *dimy, int *dimz, int *bytes );


extern void _writePnmImage( char *name, int x, int y, int z, int b, void *buf );


extern void IoPnm_verbose ( );
extern void IoPnm_noverbose ( );

extern void IoPnm_WriteGreyAsColor();
extern void IoPnm_DontWriteGreyAsColor();

extern void IoPnm_SetMaxGreyValueTo255();
extern void IoPnm_DontSetMaxGreyValueTo255();

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _iopnm_h_ */
