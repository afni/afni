/*! Header for functions in SUMA_Isosurface.c

NOTE: MarchingCube code was translated from Thomas Lewiner's C++
implementation of the paper:
Efficient Implementation of Marching Cubes´ Cases with Topological Guarantees
by Thomas Lewiner, Hélio Lopes, Antônio Wilson Vieira and Geovan Tavares 
in Journal of Graphics Tools. 
http://www-sop.inria.fr/prisme/personnel/Thomas.Lewiner/JGT.pdf
*/

#ifndef SUMA_ISOSURFACE_INCLUDED
#define SUMA_ISOSURFACE_INCLUDED

typedef enum { SUMA_ISO_UNDEFINED, SUMA_ISO_VAL, 
               SUMA_ISO_RANGE, SUMA_ISO_CMASK, SUMA_ISO_ROIS } SUMA_ISO_OPTIONS;
typedef enum { SUMA_ISO_XFORM_UNDEFINED, SUMA_ISO_XFORM_NONE, SUMA_ISO_XFORM_SHIFT, SUMA_ISO_XFORM_MASK } SUMA_ISO_XFORMS;


SUMA_Boolean SUMA_Get_isosurface_datasets (
                                    SUMA_GENERIC_PROG_OPTIONS_STRUCT * Opt);
SUMA_SurfaceObject *SUMA_MarchingCubesSurface(
                                    SUMA_GENERIC_PROG_OPTIONS_STRUCT * Opt);
char *SUMA_Decode_ROI_IsoSurfacesLabels(char *SOlabel, int *key);
SUMA_SurfaceObject **SUMA_THD_ROI_IsoSurfaces(THD_3dim_dataset *in_volu, int isb,
                                              int *valmask, int *N_valmask,
                                              byte *cmask, byte cropit, 
                                              int debug);
SUMA_SurfaceObject *SUMA_THD_IsoSurface(THD_3dim_dataset *in_vol,
                       float v0, float v1,
                       byte cropit, int debug); 
#endif
