#ifndef SUMA__INCLUDED
#define SUMA__INCLUDED
SUMA_Boolean SUMA_ParseLHS_RHS (char *s, char *lhs, char *rhs);
SUMA_Boolean SUMA_Read_SpecFile (char *f_name, SUMA_SurfSpecFile * Spec);
SUMA_Boolean SUMA_LoadSpec (SUMA_SurfSpecFile *Spec, SUMA_DO *dov, int *N_dov, char *VolParName);
SUMA_Boolean SUMA_SurfaceMetrics (SUMA_SurfaceObject *SO, const char *Metrics, SUMA_SurfaceObject *SOinh);
SUMA_Boolean SUMA_Save_Surface_Object (void *F_name, SUMA_SurfaceObject *SO, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF);
char * SUMA_SurfaceFileName (SUMA_SurfaceObject * SO, SUMA_Boolean MitPath);


#endif
