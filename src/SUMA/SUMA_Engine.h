#ifndef SUMA_ENGINE_INCLUDED
#define SUMA_ENGINE_INCLUDED

SUMA_Boolean SUMA_MakeMeDo (char *scom, int method);
SUMA_Boolean SUMA_Engine (DList **listp);
char *SUMA_AfniOverlayLabel(SUMA_ALL_DO *ado, int num);
SUMA_Boolean SUMA_process_NIML_data( void *nini , SUMA_SurfaceViewer *sv );
int SUMA_RegisteredSOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs);
int SUMA_VisibleSOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs, 
                     int forpicking);
int SUMA_VisibleMDOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *MDO_IDs);
int SUMA_Selectable_ADOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs);
int SUMA_is_iDO_Selectable(int dov_id);
int SUMA_ADOs_WithSurfCont (SUMA_DO *dov, int N_dov, int *dov_IDs);
int SUMA_NextSO (SUMA_DO *dov, int n_dov, char *idcode, 
                 SUMA_SurfaceObject *SOnxt);
int SUMA_FirstGoodState(SUMA_SurfaceViewer *sv);
int SUMA_FirstGoodAnatCorrState(SUMA_SurfaceViewer *sv);
int SUMA_NextState(SUMA_SurfaceViewer *sv);
int SUMA_PrevState(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_GetOverlaysFromParent(SUMA_SurfaceObject *SO_nxt, 
                                        SUMA_SurfaceObject *SO_prec);
SUMA_Boolean SUMA_SwitchState (SUMA_DO *dov, int N_dov, SUMA_SurfaceViewer *sv, 
                               int nxtstateID, char *nxtgroup);
SUMA_Boolean SUMA_OpenGLStateReset (SUMA_DO *dov, int N_dov, 
                                    SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SwitchSO (SUMA_DO *dov, int N_dov, int SOcurID, int SOnxtID, 
                            SUMA_SurfaceViewer *sv);
int SUMA_GetEyeAxis (SUMA_SurfaceViewer *sv, SUMA_DO *dov);
float * SUMA_XYZ_XYZmap (float *XYZ, SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                         int N_dov, int *I_C, int LDP_only);
float * SUMA_XYZmap_XYZ (float *XYZmap, SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                         int N_dov, int *I_C, int LDP_only);
int SUMA_MapRefRelative (int cur_id, int *prec_list, int N_prec_list, 
                         SUMA_DO *dov);
SUMA_Boolean SUMA_NewGeometryInViewer (SUMA_DO *dov, int N_dov, 
                                       SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_isVisibleDO (SUMA_SurfaceViewer *sv, 
                               SUMA_DO *dov, SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_isRegisteredSO (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                  SUMA_SurfaceObject *curSO);
SUMA_Boolean SUMA_isRegisteredDO (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                  SUMA_ALL_DO *curDO);
SUMA_Boolean SUMA_ADO_isRegistered(SUMA_SurfaceViewer *sv, SUMA_ALL_DO *ado);
int *SUMA_FormSOListToSendToAFNI(SUMA_DO *dov, int N_dov, int *N_Send) ;
void *SUMA_nimlEngine2Engine(NI_group *ngr); 

#endif
