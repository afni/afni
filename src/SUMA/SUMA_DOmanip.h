#ifndef SUMA_DOMANIP_INCLUDED
#define SUMA_DOMANIP_INCLUDED

SUMA_SurfaceObject * SUMA_Load_Surface_Object (void *SO_FileName, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *VolParName);
float * SUMA_IV_XYZextract (char *IV_filename, int *N_NodeList, int IncludeIndex);
int *SUMA_IV_FaceSetsextract (char *IV_filename, int *N_FaceSetList);
SUMA_SURF_NORM SUMA_SurfNorm (float *NodeList, int N_NodeList, int *FaceSetList, int N_FaceSetList );
SUMA_Boolean SUMA_Free_Displayable_Object (SUMA_DO *dov);
SUMA_DO *SUMA_Alloc_DisplayObject_Struct (int N);
SUMA_Boolean SUMA_AddDO(SUMA_DO *dov, int *N_dov, void *op, SUMA_DO_Types DO_Type, SUMA_DO_CoordType DO_CoordType);
SUMA_Boolean SUMA_UnRegisterDO(int dov_id, SUMA_SurfaceViewer *cSV);
SUMA_Boolean SUMA_RegisterDO(int dov_id, SUMA_SurfaceViewer *cSV);
void SUMA_Show_DOv (SUMA_DO *dov, int N_dov, FILE *Out);
int * SUMA_GetDO_Type(SUMA_DO *dov, int N_dov, SUMA_DO_Types DO_Type, int *N);
SUMA_Boolean SUMA_Free_Displayable_Object_Vect (SUMA_DO *dov, int N);
SUMA_MEMBER_FACE_SETS *SUMA_MemberFaceSets (int Nind, int * FaceSetList, int nFr , int FaceDim);
SUMA_Boolean SUMA_Free_MemberFaceSets (SUMA_MEMBER_FACE_SETS *MF);
SUMA_VOLPAR *SUMA_VolPar_Attr (char *volparent_name);
SUMA_Boolean SUMA_Free_VolPar (SUMA_VOLPAR *VP);
SUMA_VOLPAR *SUMA_Alloc_VolPar (void);
void SUMA_Show_VolPar(SUMA_VOLPAR *VP, FILE *Out);
char *SUMA_VolPar_Info (SUMA_VOLPAR *VP);
SUMA_Boolean SUMA_Align_to_VolPar (SUMA_SurfaceObject *SO, void* S_struct);
SUMA_Boolean SUMA_Apply_VolReg_Trans (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_Read_SureFit_Param (char *f_name, SUMA_SureFit_struct *SF);
int SUMA_ReleaseLink (SUMA_INODE * IN);
int SUMA_AddLink (SUMA_INODE *IN);
SUMA_INODE *SUMA_CreateInode (void *data, char *ID);
SUMA_INODE *SUMA_CreateInodeLink (SUMA_INODE * FromIN, SUMA_INODE *ToIN);
SUMA_Boolean SUMA_isInodeLink (SUMA_INODE *IN, const char *HolderIDcode);
SUMA_INODE * SUMA_BreakInodeLink (SUMA_INODE *IN, const char *HolderIDcode);
SUMA_Boolean SUMA_existDO(char *idcode, SUMA_DO *dov, int N_dov);
int SUMA_findSO_inDOv(char *idcode, SUMA_DO *dov, int N_dov);
SUMA_SurfaceObject * SUMA_findSOp_inDOv(char *idcode, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_ismappable (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isINHmappable (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isSO (SUMA_DO DO); 
SUMA_Boolean SUMA_isRelated (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2);
SUMA_Boolean SUMA_isdROIrelated (SUMA_DRAWN_ROI *ROI, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isROIrelated (SUMA_ROI *ROI, SUMA_SurfaceObject *SO);
SUMA_DRAWN_ROI * SUMA_FetchROI_InCreation (SUMA_SurfaceObject *SO, SUMA_DO * dov, int N_dov); 





#endif
