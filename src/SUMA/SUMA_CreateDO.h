#ifndef SUMA_CREATEDO_INCLUDED
#define SUMA_CREAREDO_INCLUDED

SUMA_Axis* SUMA_Alloc_Axis (const char *Name);
void SUMA_Free_Axis (SUMA_Axis *Ax);
SUMA_Boolean SUMA_CreateAxis (SUMA_Axis* Ax);
void SUMA_MeshAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceObject *cso);
void SUMA_EyeAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *csv);
void SUMA_Free_CrossHair (SUMA_CrossHair *Ch);
SUMA_CrossHair* SUMA_Alloc_CrossHair (void);
SUMA_Boolean SUMA_CreateCrossHair (SUMA_CrossHair* Ch);
void SUMA_Free_SphereMarker (SUMA_SphereMarker *SM);
SUMA_SphereMarker* SUMA_Alloc_SphereMarker (void);
SUMA_Boolean SUMA_CreateFaceSetMarker (SUMA_FaceSetMarker* FM);
SUMA_FaceSetMarker* SUMA_Alloc_FaceSetMarker (void);
void SUMA_Free_FaceSetMarker (SUMA_FaceSetMarker* FM);
void SUMA_CreateMesh(SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *csv);
SUMA_Boolean SUMA_Free_Surface_Object (SUMA_SurfaceObject *SO);
void SUMA_Print_Surface_Object(SUMA_SurfaceObject *SO, FILE *Out);
SUMA_SurfaceObject *SUMA_Alloc_SurfObject_Struct(int N);
SUMA_DRAWN_ROI * SUMA_AllocateDrawnROI (char *Parent_idcode_str, SUMA_ROI_DRAWING_STATUS DrawStatus, SUMA_ROI_DRAWING_TYPE Type, char * label);
SUMA_ROI * SUMA_AllocateROI (char *Parent_idcode_str, SUMA_ROI_DRAWING_TYPE Type, char * label, int N_ElInd, int *ElInd);
SUMA_Boolean SUMA_freeDrawnROI (SUMA_DRAWN_ROI *D_ROI); 
SUMA_Boolean SUMA_freeROI (SUMA_ROI *ROI); 
SUMA_Boolean SUMA_Draw_SO_ROI (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov);
 



#endif
