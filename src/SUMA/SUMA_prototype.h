#ifndef SUMA_PROTOTYPE_INCLUDED
#define SUMA_PROTOTYPE_INCLUDED

/* functions that have yet to be prototyped in a particular spot */
void SUMA_postRedisplay(void);
void SUMA_display(SUMA_SurfaceViewer *csv, SUMA_DO *dov);
void SUMA_momentum(XtPointer clientData, XtIntervalId *id);
Boolean SUMA_spin(XtPointer clientData);
void SUMA_mapStateChanged(Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
Colormap SUMA_getShareableColormap(SUMA_SurfaceViewer * csv);
void SUMA_graphicsInit(Widget w, XtPointer clientData, XtPointer call);
void SUMA_expose(Widget w, XtPointer clientData, XtPointer call);
void SUMA_resize(Widget w, XtPointer clientData, XtPointer call);
void SUMA_input(Widget w, XtPointer clientData, XtPointer callData);
void SUMA_help(void);
void SUMA_help_message(FILE *Out);
void SUMA_VolSurf_help (FILE *Out);
void SUMA_Version (FILE *Out);
SUMA_Boolean SUMA_X_SurfaceViewer_Create (SUMA_SurfaceViewer *csv, int argc,char *argv[]);
void SUMA_SetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
SUMA_Boolean  SUMA_RegisterCommand(char *S, char d, char term, char *Scom, SUMA_Boolean Prepend);
int  SUMA_GetNextCommand (char *S, char d, char term, char *Scom);
SUMA_Boolean SUMA_Engine (char *Command, SUMA_EngineData *EngineData);
int SUMA_CommandCode(char *Scom);
SUMA_Boolean SUMA_SureFit_Read_Coord (char * f_name, SUMA_SureFit_struct *SF);
SUMA_Boolean SUMA_SureFit_Read_Topo (char * f_name, SUMA_SureFit_struct *SF);
void SUMA_Show_SureFit (SUMA_SureFit_struct *SF, FILE *Out);
SUMA_Boolean SUMA_Free_SureFit (SUMA_SureFit_struct *SF);
SUMA_Boolean SUMA_FreeSurfer_Read (char * f_name, SUMA_FreeSurfer_struct *FS);
SUMA_Boolean SUMA_Free_FreeSurfer (SUMA_FreeSurfer_struct *FS);
void SUMA_Show_FreeSurfer (SUMA_FreeSurfer_struct *FS, FILE *Out);
int SUMA_EngineFieldCode(char *Scom);
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *MTI);
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *MTI, char *Location);
SUMA_Boolean SUMA_InitializeEngineData (SUMA_EngineData *MTI);
SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *MTI, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer);
NI_element * SUMA_makeNI_SurfIXYZ (SUMA_SurfaceObject *SO);
NI_element * SUMA_makeNI_SurfIJK (SUMA_SurfaceObject *SO);
Boolean SUMA_niml_workproc( XtPointer thereiselvis );
SUMA_Boolean SUMA_process_NIML_data( void *nini );
Boolean SUMA_workprocess( XtPointer fred );
void SUMA_remove_workproc( XtWorkProc func );
void SUMA_register_workproc( XtWorkProc func , XtPointer data );
int SUMA_EngineSourceCode (char *Scom);
void SUMA_EngineSourceString (char *Scom, int ses_code);
NI_element * SUMA_makeNI_CrossHair (SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_nel_stdout (NI_element *nel); 
SUMA_Boolean SUMA_Read_SpecFile (char *f_name, SUMA_SurfSpecFile * Spec);
SUMA_Boolean SUMA_ParseLHS_RHS (char *s, char *lhs, char *rhs);
SUMA_Boolean SUMA_existDO(char *idcode, SUMA_DO *dov, int N_dov);
int SUMA_findDO(char *idcode, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_LoadSpec (SUMA_SurfSpecFile *Spec, SUMA_DO *dov, int *N_dov, char *VolParName);
SUMA_Boolean SUMA_ismappable (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isINHmappable (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isSO (SUMA_DO DO); 
SUMA_Boolean SUMA_CanTalkToAfni (SUMA_SurfaceViewer *sv, SUMA_DO *dov);
int SUMA_ShownSOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs);
int SUMA_NextSO (SUMA_DO *dov, int n_dov, char *idcode, SUMA_SurfaceObject *SOnxt);
SUMA_Boolean SUMA_SwitchSO (SUMA_DO *dov, int N_dov, int SOcurID, int SOnxtID, SUMA_SurfaceViewer *sv);
int SUMA_GetEyeAxis (SUMA_SurfaceViewer *sv, SUMA_DO *dov);
float * SUMA_XYZ_XYZmap (float *XYZ, SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, int *I_C);
float * SUMA_XYZmap_XYZ (float *XYZmap, SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, int *I_C);
int SUMA_NextState(SUMA_SurfaceViewer *sv);
int SUMA_PrevState(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SwitchState (SUMA_DO *dov, int N_dov, SUMA_SurfaceViewer *sv, int nxtstateID);
int SUMA_MapRefRelative (int cur_id, int *prec_list, int N_prec_list, SUMA_DO *dov);
SUMA_Boolean SUMA_RenderToPixMap (SUMA_SurfaceViewer *csv, SUMA_DO* dov);
void SUMA_context_Init(void);
 













#endif



