#ifndef SUMA_SVMANIP_INCLUDED
#define SUMA_SVMANIP_INCLUDED

#define UPDATE_NO_VIEWING_PARAMS_MASK (1<<1)
#define UPDATE_ROT_MASK (1<<2)
#define UPDATE_VIEW_POINT_MASK (1<<3)
#define UPDATE_EYE_AXIS_STD_MASK (1<<4)
#define UPDATE_STANDARD_VIEW_MASK (1<<5)
#define UPDATE_ALL_VIEWING_PARAMS_MASK ( UPDATE_ROT_MASK | UPDATE_VIEW_POINT_MASK | UPDATE_EYE_AXIS_STD_MASK | UPDATE_STANDARD_VIEW_MASK )

/*! functions defined in SUMA_SVmanip.c */
SUMA_Boolean SUMA_LockEnum_LockType (SUMA_LINK_TYPES i, char *Name);
SUMA_SurfaceViewer *SUMA_Alloc_SurfaceViewer_Struct (int N);
void SUMA_Show_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV, FILE *Out, int detail);
char *SUMA_SurfaceViewer_StructInfo (SUMA_SurfaceViewer *SV, int detail);
SUMA_Boolean SUMA_SetViewerLightsForSO(SUMA_SurfaceViewer *cSV, SUMA_SurfaceObject *SO);

SUMA_Boolean SUMA_UpdateRotaCenter (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_UpdateViewPoint (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_Free_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV);
SUMA_Boolean SUMA_Free_SurfaceViewer_Struct_Vect (SUMA_SurfaceViewer *SVv, int N);
SUMA_Boolean SUMA_Free_ViewState (SUMA_ViewState *vs);
SUMA_ViewState *SUMA_Alloc_ViewState (int N);
SUMA_Boolean SUMA_New_ViewState (SUMA_SurfaceViewer *csv);
SUMA_Boolean SUMA_Free_ViewState_Hist (SUMA_ViewState_Hist *vsh);
SUMA_ViewState_Hist *SUMA_Alloc_ViewState_Hist (void);
SUMA_Boolean SUMA_Show_ViewState(SUMA_ViewState *VS, FILE *Out, int detail); 
char *SUMA_ViewStateInfo(SUMA_ViewState *VS, int detail);
SUMA_Boolean SUMA_AdoptSurfGroup(SUMA_SurfaceViewer *csv, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_RegisterSpecSO (SUMA_SurfSpecFile *Spec, SUMA_SurfaceViewer *csv, SUMA_DO* dov, int N_dov, int viewopt);
int SUMA_WhichState (char *state, SUMA_SurfaceViewer *csv, char *ForceGroup);
SUMA_Boolean SUMA_Free_CommonFields (SUMA_CommonFields *cf);
SUMA_CommonFields * SUMA_Create_CommonFields (void);
void SUMA_Show_CommonFields (SUMA_CommonFields *cf, FILE *out);
char * SUMA_CommonFieldsInfo (SUMA_CommonFields *cf, int detail);
SUMA_STANDARD_VIEWS SUMA_BestStandardView (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_SetupSVforDOs (SUMA_SurfSpecFile Spec, SUMA_DO *DOv, int N_DOv, SUMA_SurfaceViewer *cSV, int viewopt);
SUMA_Boolean SUMA_FillColorList (SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_EmptyColorList (SUMA_SurfaceViewer *sv, char *DO_idstr);
GLfloat * SUMA_GetColorList (SUMA_SurfaceViewer *sv, char *DO_idstr);
SUMA_Boolean SUMA_SetRemixFlag (char *SO_idcode_str, SUMA_SurfaceViewer *SVv, int N_SVv);
SUMA_Boolean SUMA_SetShownLocalRemixFlag (SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SetLocalRemixFlag (char *SO_idcode_str, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SetAllRemixFlag (SUMA_SurfaceViewer *SVv, int N_SVv);
int SUMA_WhichSV (SUMA_SurfaceViewer *sv, SUMA_SurfaceViewer *SVv, int N_SVv);
SUMA_X_SumaCont *SUMA_CreateSumaContStruct (void);
void *SUMA_FreeSumaContStruct (SUMA_X_SumaCont *SumaCont);
SUMA_X_ViewCont *SUMA_CreateViewContStruct (void);
void *SUMA_FreeViewContStruct (SUMA_X_ViewCont *ViewCont);
SUMA_X_SurfCont *SUMA_CreateSurfContStruct (char *idcode_str);
void *SUMA_FreeSurfContStruct (SUMA_X_SurfCont *SurfCont);
SUMA_rb_group *SUMA_CreateLock_rb (int N_rb_group, int N_but);
void * SUMA_FreeLock_rb (SUMA_rb_group *Lock_rb);
SUMA_X_DrawROI *SUMA_CreateDrawROIStruct (void);
void *SUMA_FreeDrawROIStruct (SUMA_X_DrawROI *DrawROI);
void SUMA_UpdateViewerTitle(SUMA_SurfaceViewer *sv); 
void SUMA_UpdateAllViewerCursor(void); 
void SUMA_UpdateViewerCursor(SUMA_SurfaceViewer *sv); 
int SUMA_WhichViewerInMomentum(SUMA_SurfaceViewer *SVv, int N_SV, SUMA_SurfaceViewer *sv);
int SUMA_WhichGroup (SUMA_CommonFields *cf, char *nm);
SUMA_Boolean SUMA_RegisterGroup (SUMA_CommonFields *cf, SUMA_SurfSpecFile *spec);
SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleGroupList (SUMA_SurfaceViewer *sv); 
SUMA_Boolean SUMA_SwitchGroups (SUMA_SurfaceViewer *sv, char *group);
SUMA_Boolean SUMA_AdoptGroup(SUMA_SurfaceViewer *csv, char *group);
const char * SUMA_Clip_Type_to_Clip_Name (SUMA_CLIP_PLANE_TYPES tp);
char * SUMA_Show_Clip_Planes_Info (SUMA_CommonFields *cf);
void SUMA_Show_Clip_Planes (SUMA_CommonFields *cf, FILE *out);
float SUMA_sv_fov_original(SUMA_SurfaceViewer *sv);
SUMA_SurfaceViewer *SUMA_OneViewerWithSOinFocus(
                              SUMA_SurfaceObject *curSO);
SUMA_SurfaceViewer *SUMA_OneViewerWithSOVisible(
                              SUMA_SurfaceObject *curSO);
SUMA_SurfaceViewer *SUMA_OneViewerWithSORegistered(
                              SUMA_SurfaceObject *curSO);
SUMA_SurfaceViewer *SUMA_BestViewerForSO(
                              SUMA_SurfaceObject *curSO);





#endif
