#ifndef SUMA_SVMANIP_INCLUDED
#define SUMA_SVMANIP_INCLUDED

/*! functions defined in SUMA_SVmanip.c */
SUMA_Boolean SUMA_LockEnum_LockType (SUMA_LINK_TYPES i, char *Name);
SUMA_SurfaceViewer *SUMA_Alloc_SurfaceViewer_Struct (int N);
void SUMA_Show_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV, FILE *Out);
SUMA_Boolean SUMA_UpdateRotaCenter (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_UpdateViewPoint (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_Free_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV);
SUMA_Boolean SUMA_Free_SurfaceViewer_Struct_Vect (SUMA_SurfaceViewer *SVv, int N);
SUMA_Boolean SUMA_Free_ViewState (SUMA_ViewState *vs);
SUMA_ViewState *SUMA_Alloc_ViewState (int N);
SUMA_Boolean SUMA_Free_ViewState_Hist (SUMA_ViewState_Hist *vsh);
SUMA_ViewState_Hist *SUMA_Alloc_ViewState_Hist (void);
SUMA_Boolean SUMA_Show_ViewState(SUMA_ViewState *VS, FILE *Out); 
SUMA_Boolean SUMA_RegisterSpecSO (SUMA_SurfSpecFile *Spec, SUMA_SurfaceViewer *csv, SUMA_DO* dov, int N_dov);
int SUMA_WhichState (char *state, SUMA_SurfaceViewer *csv);
SUMA_Boolean SUMA_Assign_AfniHostName (SUMA_CommonFields *cf, char *AfniHostName);
SUMA_Boolean SUMA_Free_CommonFields (SUMA_CommonFields *cf);
SUMA_CommonFields * SUMA_Create_CommonFields (void);
void SUMA_Show_CommonFields (SUMA_CommonFields *cf);
SUMA_STANDARD_VIEWS SUMA_BestStandardView (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_SetupSVforDOs (SUMA_SurfSpecFile Spec, SUMA_DO *DOv, int N_DOv, SUMA_SurfaceViewer *cSV);
SUMA_Boolean SUMA_FillColorList (SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_EmptyColorList (SUMA_SurfaceViewer *sv, char *DO_idstr);
GLfloat * SUMA_GetColorList (SUMA_SurfaceViewer *sv, char *DO_idstr);
SUMA_Boolean SUMA_SetRemixFlag (char *SO_idcode_str, SUMA_SurfaceViewer *SVv, int N_SVv);
SUMA_Boolean SUMA_SetShownLocalRemixFlag (SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SetLocalRemixFlag (char *SO_idcode_str, SUMA_SurfaceViewer *sv);
int SUMA_WhichSV (SUMA_SurfaceViewer *sv, SUMA_SurfaceViewer *SVv, int N_SVv);
SUMA_X_SumaCont *SUMA_CreateSumaContStruct (void);
void *SUMA_FreeSumaContStruct (SUMA_X_SumaCont *SumaCont);
SUMA_X_ViewCont *SUMA_CreateViewContStruct (void);
void *SUMA_FreeViewContStruct (SUMA_X_ViewCont *ViewCont);
SUMA_X_SurfCont *SUMA_CreateSurfContStruct (void);
void *SUMA_FreeSurfContStruct (SUMA_X_SurfCont *SurfCont);
SUMA_rb_group *SUMA_CreateLock_rb (int N_rb_group, int N_but);
void * SUMA_FreeLock_rb (SUMA_rb_group *Lock_rb);
SUMA_X_DrawROI *SUMA_CreateDrawROIStruct (void);
void *SUMA_FreeDrawROIStruct (SUMA_X_DrawROI *DrawROI);
void SUMA_UpdateViewerTitle(SUMA_SurfaceViewer *sv); 
void SUMA_UpdateAllViewerCursor(void); 
void SUMA_UpdateViewerCursor(SUMA_SurfaceViewer *sv); 
 




#endif
