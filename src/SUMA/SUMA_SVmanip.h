#ifndef SUMA_SVMANIP_INCLUDED
#define SUMA_SVMANIP_INCLUDED

/*! functions defined in SUMA_SVmanip.c */
SUMA_SurfaceViewer *SUMA_Alloc_SurfaceViewer_Struct (int N);
void Show_SUMA_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV, FILE *Out);
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




#endif
