#ifndef SUMA__INCLUDED
#define SUMA__INCLUDED


SUMA_Boolean SUMA_AllocSpecFields (SUMA_SurfSpecFile *Spec);
SUMA_Boolean SUMA_FreeSpecFields (SUMA_SurfSpecFile *Spec);
SUMA_SurfaceObject * SUMA_Load_Spec_Surf(SUMA_SurfSpecFile *Spec, int i, char *tmpVolParName, int debug);
SUMA_Boolean SUMA_PrepAddmappableSO(SUMA_SurfaceObject *SO, SUMA_DO *dov, int *N_dov, int debug, DList *DsetList);
SUMA_Boolean SUMA_PrepSO_GeomProp_GL(SUMA_SurfaceObject *SO);
SUMA_SurfaceObject * SUMA_Load_Surface_Object (void *SO_FileName, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *VolParName);
SUMA_SurfaceObject * SUMA_Load_Surface_Object_eng (void *SO_FileName, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *VolParName, int debug);
SUMA_SurfaceObject *SUMA_Load_Surface_Object_Wrapper ( char *if_name1, char *if_name2, char *vp_name, 
                                                   SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *sv_name, int debug);
SUMA_Boolean SUMA_ParseLHS_RHS (char *s, char *lhs, char *rhs);
SUMA_Boolean SUMA_ShowSpecStruct (SUMA_SurfSpecFile *Spec, FILE *Out, int detail);
char* SUMA_SpecStructInfo (SUMA_SurfSpecFile *Spec, int detail);
SUMA_Boolean SUMA_Read_SpecFile (char *f_name, SUMA_SurfSpecFile * Spec);
SUMA_Boolean SUMA_Write_SpecFile ( SUMA_SurfSpecFile * Spec, 
                           char *specFileNm, 
                           char *program, 
                           char *histnote);
SUMA_Boolean SUMA_CheckOnSpecFile (SUMA_SurfSpecFile *Spec);
SUMA_Boolean SUMA_LoadSpec (SUMA_SurfSpecFile *Spec, SUMA_DO *dov, int *N_dov, char *VolParName);
SUMA_Boolean SUMA_LoadSpec_eng (SUMA_SurfSpecFile *Spec, SUMA_DO *dov, int *N_dov, char *VolParName, int debug, DList *DsetList);
SUMA_Boolean SUMA_SurfaceMetrics (SUMA_SurfaceObject *SO, const char *Metrics, SUMA_SurfaceObject *SOinh);
SUMA_Boolean SUMA_SurfaceMetrics_eng (SUMA_SurfaceObject *SO, const char *Metrics, SUMA_SurfaceObject *SOinh, int debug, DList *DsetList);
SUMA_Boolean SUMA_Save_Surface_Object (void *F_name, SUMA_SurfaceObject *SO, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, void*someparam);
void * SUMA_Save_Surface_Object_Wrap ( char *surf_name, char *topo_name,
                                       SUMA_SurfaceObject *SO, 
                                       SUMA_SO_File_Type SO_FT, 
                                       SUMA_SO_File_Format SO_FF, 
                                       void *someparam);
char * SUMA_SurfaceFileName (SUMA_SurfaceObject * SO, SUMA_Boolean MitPath);
SUMA_SO_SIDE SUMA_GuessSide(SUMA_SurfaceObject *SO);
int SUMA_SetSphereParams(SUMA_SurfaceObject *SO, float tol);
char SUMA_GuessAnatCorrect(SUMA_SurfaceObject *SO);
int    SUMA_spec_select_surfs ( SUMA_SurfSpecFile * spec, char ** names,
			        int nnames, int debug );
int    SUMA_spec_set_map_refs ( SUMA_SurfSpecFile * spec, int debug );
char * SUMA_coord_file        ( SUMA_SurfSpecFile * spec, int index );
int    SUMA_swap_spec_entries (SUMA_SurfSpecFile *spec,int i0,int i1,int debug);
int    SUMA_copy_spec_entries( SUMA_SurfSpecFile * spec0, 
                               SUMA_SurfSpecFile *spec1,
                               int i0, int i1, int debug);
int    SUMA_unique_name_ind   ( SUMA_SurfSpecFile * spec, char * sname );
int    swap_strings           ( char * s0, char * s1, char * save, int len );
void SUMA_Show_IO_args(SUMA_GENERIC_ARGV_PARSE *ps);
SUMA_SurfSpecFile *SUMA_SOGroup_2_Spec(SUMA_SurfaceObject **SOv, int N_SOv);
SUMA_SurfSpecFile *SUMA_IO_args_2_spec(SUMA_GENERIC_ARGV_PARSE *ps, int *nspec);
void SetLoadPacify(int k);
int  GetLoadPacify(void);

#endif
