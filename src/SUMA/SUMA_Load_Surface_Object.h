#ifndef SUMA__INCLUDED
#define SUMA__INCLUDED

SUMA_SurfaceObject * SUMA_Load_Spec_Surf(SUMA_SurfSpecFile *Spec, int i, char *tmpVolParName, int debug);
SUMA_SurfaceObject * SUMA_Load_Surface_Object (void *SO_FileName, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *VolParName);
SUMA_SurfaceObject * SUMA_Load_Surface_Object_eng (void *SO_FileName, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *VolParName, int debug);
SUMA_Boolean SUMA_ParseLHS_RHS (char *s, char *lhs, char *rhs);
SUMA_Boolean SUMA_ShowSpecStruct (SUMA_SurfSpecFile *Spec, FILE *Out, int detail);
char* SUMA_SpecStructInfo (SUMA_SurfSpecFile *Spec, int detail);
SUMA_Boolean SUMA_Read_SpecFile (char *f_name, SUMA_SurfSpecFile * Spec);
SUMA_Boolean SUMA_CheckOnSpecFile (SUMA_SurfSpecFile *Spec);
SUMA_Boolean SUMA_LoadSpec (SUMA_SurfSpecFile *Spec, SUMA_DO *dov, int *N_dov, char *VolParName);
SUMA_Boolean SUMA_LoadSpec_eng (SUMA_SurfSpecFile *Spec, SUMA_DO *dov, int *N_dov, char *VolParName, int debug, DList *DsetList);
SUMA_Boolean SUMA_SurfaceMetrics (SUMA_SurfaceObject *SO, const char *Metrics, SUMA_SurfaceObject *SOinh);
SUMA_Boolean SUMA_SurfaceMetrics_eng (SUMA_SurfaceObject *SO, const char *Metrics, SUMA_SurfaceObject *SOinh, int debug, DList *DsetList);
SUMA_Boolean SUMA_Save_Surface_Object (void *F_name, SUMA_SurfaceObject *SO, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF);
char * SUMA_SurfaceFileName (SUMA_SurfaceObject * SO, SUMA_Boolean MitPath);
SUMA_SO_SIDE SUMA_GuessSide(SUMA_SurfaceObject *SO);
char SUMA_GuessAnatCorrect(SUMA_SurfaceObject *SO);
int    SUMA_spec_select_surfs ( SUMA_SurfSpecFile * spec, char ** names,
			        int nnames, int debug );
int    SUMA_spec_set_map_refs ( SUMA_SurfSpecFile * spec, int debug );
char * SUMA_coord_file        ( SUMA_SurfSpecFile * spec, int index );
int    SUMA_swap_spec_entries (SUMA_SurfSpecFile *spec,int i0,int i1,int debug);
int    SUMA_unique_name_ind   ( SUMA_SurfSpecFile * spec, char * sname );
int    swap_strings           ( char * s0, char * s1, char * save, int len );

#endif
