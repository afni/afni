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
   int nmaskv;
   int *maskv;
   int debug;
   int ninmask;
   float v0;
   float v1;
   int nvox;
   double *dvec;
   SUMA_SO_File_Format SurfFileFormat;
   SUMA_SO_File_Type SurfFileType;
} SUMA_ISOSURFACE_OPTIONS;

SUMA_Boolean SUMA_Get_isosurface_datasets (SUMA_ISOSURFACE_OPTIONS * Opt);

#endif
