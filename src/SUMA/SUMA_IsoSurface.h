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

typedef enum { SUMA_ISO_UNDEFINED, SUMA_ISO_VAL, SUMA_ISO_RANGE, SUMA_ISO_CMASK } SUMA_ISO_OPTIONS;
typedef enum { SUMA_ISO_XFORM_UNDEFINED, SUMA_ISO_XFORM_NONE, SUMA_ISO_XFORM_SHIFT, SUMA_ISO_XFORM_MASK } SUMA_ISO_XFORMS;

/* structures to be used by surface clusters functions */
#define ISOSURFACE_MAX_SURF 10  /*!< Maximum number of surfaces allowed for SurfClust*/
typedef struct {
   SUMA_SO_File_Type iType;
   char *sv_name;
   char *surf_names[ISOSURFACE_MAX_SURF];
   int N_surf;
   char *spec_file;
   char *in_name;
   char *surftype;
   char *out_prefix;   /* this one's dynamically allocated so you'll have to free it yourself */
   int MaskMode;
   char *cmask;
   THD_3dim_dataset *in_vol;
   double *mcdatav; /* the dataset that is passed to the marching cube algorithm */
   int debug;
   int ninmask;
   float v0;
   float v1;
   int nvox;
   double *dvec;
   int obj_type;
   int obj_type_res;
   SUMA_ISO_XFORMS xform;
   SUMA_SO_File_Format SurfFileFormat;
   SUMA_SO_File_Type SurfFileType;
   /* following fields are intended for use in ConvexHull only */
   char *in_1D;  /* name of 1D file containing XYZ coords */
   float *XYZ; /* a 3*N_XYZ vector of XYZ coordinates. This vector should be freed at the end*/
   int N_XYZ;  /* number of points in XYZ */
} SUMA_ISOSURFACE_OPTIONS;

SUMA_Boolean SUMA_Get_isosurface_datasets (SUMA_ISOSURFACE_OPTIONS * Opt);
SUMA_SurfaceObject *SUMA_MarchingCubesSurface(SUMA_ISOSURFACE_OPTIONS * Opt);

#endif
