#ifndef SUMA_DOMANIP_INCLUDED
#define SUMA_DOMANIP_INCLUDED

float ** SUMA_IV_XYZextract (char *IV_filename, int *N_NodeList, int IncludeIndex);
int **SUMA_IV_FaceSetsextract (char *IV_filename, int *N_FaceSetList);
SUMA_SURF_NORM SUMA_SurfNorm (float **NodeList, int N_NodeList, int **FaceSetList, int N_FaceSetList );
SUMA_SurfaceObject *SUMA_Alloc_SurfObject_Struct(int N);
SUMA_SurfaceObject * SUMA_Load_Surface_Object (void *SO_FileName, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *VolParName);
SUMA_Boolean SUMA_Free_Surface_Object (SUMA_SurfaceObject *SO);
void SUMA_Print_Surface_Object(SUMA_SurfaceObject *SO, FILE *Out);
void SUMA_CreateMesh(SUMA_SurfaceObject *SurfObj);
SUMA_Boolean SUMA_Free_Displayable_Object (SUMA_DO *dov);
SUMA_DO *SUMA_Alloc_DisplayObject_Struct (int N);
SUMA_Boolean SUMA_AddDO(SUMA_DO *dov, int *N_dov, void *op, SUMA_DO_Types DO_Type, SUMA_DO_CoordType DO_CoordType);
SUMA_Boolean SUMA_UnRegisterDO(int dov_id, SUMA_SurfaceViewer *cSV);
SUMA_Boolean SUMA_RegisterDO(int dov_id, SUMA_SurfaceViewer *cSV);
void SUMA_Show_DOv (SUMA_DO *dov, int N_dov, FILE *Out);
SUMA_Boolean SUMA_CreateAxis (SUMA_Axis* Ax);
void SUMA_MeshAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceObject *cso);
void SUMA_EyeAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *csv);
SUMA_Axis* SUMA_Alloc_Axis (const char *Name);
void SUMA_Free_Axis (SUMA_Axis *Ax);
int * SUMA_GetDO_Type(SUMA_DO *dov, int N_dov, SUMA_DO_Types DO_Type, int *N);
SUMA_Boolean SUMA_Free_Displayable_Object_Vect (SUMA_DO *dov, int N);
SUMA_MEMBER_FACE_SETS *SUMA_MemberFaceSets (int Nind, int ** FaceSetList, int nFr , int FaceDim);
SUMA_Boolean SUMA_Free_MemberFaceSets (SUMA_MEMBER_FACE_SETS *MF);
void SUMA_Free_CrossHair (SUMA_CrossHair *Ch);
SUMA_CrossHair* SUMA_Alloc_CrossHair (void);
SUMA_Boolean SUMA_CreateCrossHair (SUMA_CrossHair* Ch);
void SUMA_Free_SphereMarker (SUMA_SphereMarker *SM);
SUMA_SphereMarker* SUMA_Alloc_SphereMarker (void);
SUMA_Boolean SUMA_CreateFaceSetMarker (SUMA_FaceSetMarker* FM);
SUMA_FaceSetMarker* SUMA_Alloc_FaceSetMarker (void);
void SUMA_Free_FaceSetMarker (SUMA_FaceSetMarker* FM);
SUMA_VOLPAR *SUMA_VolPar_Attr (char *volparent_name);
SUMA_Boolean SUMA_Free_VolPar (SUMA_VOLPAR *VP);
SUMA_VOLPAR *SUMA_Alloc_VolPar (void);
void SUMA_Show_VolPar(SUMA_VOLPAR *VP, FILE *Out);
SUMA_Boolean SUMA_Align_to_VolPar (SUMA_SurfaceObject *SO, void* S_struct);
SUMA_Boolean SUMA_Read_SureFit_Param (char *f_name, SUMA_SureFit_struct *SF);
int SUMA_ReleaseLink (SUMA_INODE * IN);
int SUMA_AddLink (SUMA_INODE *IN);
SUMA_INODE *SUMA_CreateInode (void *data, char *ID);
SUMA_INODE *SUMA_CreateInodeLink (SUMA_INODE * FromIN, SUMA_INODE *ToIN);
SUMA_Boolean SUMA_isInodeLink (SUMA_INODE *IN, const char *HolderIDcode);
SUMA_INODE * SUMA_BreakInodeLink (SUMA_INODE *IN, const char *HolderIDcode);
SUMA_Boolean SUMA_SurfaceMetrics (SUMA_SurfaceObject *SO, const char *Metrics, SUMA_SurfaceObject *SOinh);
char * SUMA_SurfaceFileName (SUMA_SurfaceObject * SO, SUMA_Boolean MitPath);



#endif
