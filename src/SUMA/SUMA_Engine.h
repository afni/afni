#ifndef SUMA_ENGINE_INCLUDED
#define SUMA_ENGINE_INCLUDED

SUMA_Boolean SUMA_Engine (char *Command, SUMA_EngineData *EngineData, SUMA_SurfaceViewer *sv);
int SUMA_EngineFieldCode(char *Scom);
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *MTI);
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *MTI, char *Location);
SUMA_Boolean SUMA_InitializeEngineData (SUMA_EngineData *MTI);
SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *MTI, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer);
int SUMA_EngineSourceCode (char *Scom);
void SUMA_EngineSourceString (char *Scom, int ses_code);
SUMA_Boolean SUMA_process_NIML_data( void *nini , SUMA_SurfaceViewer *sv );
int SUMA_ShownSOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs);
int SUMA_NextSO (SUMA_DO *dov, int n_dov, char *idcode, SUMA_SurfaceObject *SOnxt);
int SUMA_NextState(SUMA_SurfaceViewer *sv);
int SUMA_PrevState(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SwitchState (SUMA_DO *dov, int N_dov, SUMA_SurfaceViewer *sv, int nxtstateID);
SUMA_Boolean SUMA_SwitchSO (SUMA_DO *dov, int N_dov, int SOcurID, int SOnxtID, SUMA_SurfaceViewer *sv);
int SUMA_GetEyeAxis (SUMA_SurfaceViewer *sv, SUMA_DO *dov);
float * SUMA_XYZ_XYZmap (float *XYZ, SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, int *I_C);
float * SUMA_XYZmap_XYZ (float *XYZmap, SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, int *I_C);
int SUMA_MapRefRelative (int cur_id, int *prec_list, int N_prec_list, SUMA_DO *dov);
 

#endif
