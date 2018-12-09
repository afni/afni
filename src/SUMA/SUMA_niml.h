#ifndef SUMA_NIML_DEFINED
#define SUMA_NIML_DEFINED

typedef struct {
   THD_3dim_dataset *dset;
   int at_sb;
} SUMA_SEND_2AFNI;

int SUMA_init_ports_assignments(SUMA_CommonFields *cf);
NI_element * SUMA_makeNI_SurfIXYZ (SUMA_SurfaceObject *SO);
int SUMA_offset_NI_SurfIXYZ (NI_element *nel, float *delta);
NI_element * SUMA_makeNI_SurfIJK (SUMA_SurfaceObject *SO);
NI_element * SUMA_makeNI_SurfINORM (SUMA_SurfaceObject *SO);
Boolean SUMA_niml_workproc( XtPointer thereiselvis );
Boolean SUMA_workprocess( XtPointer fred );
void SUMA_remove_workproc( XtWorkProc func );
void SUMA_register_workproc( XtWorkProc func , XtPointer data );
NI_element * SUMA_makeNI_CrossHair (SUMA_SurfaceViewer *sv);
NI_group * SUMA_makeNI_InstaTract_Query (SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_nel_stdout (NI_element *nel);
void SUMA_remove_workproc2( XtWorkProc func , XtPointer data );
SUMA_Boolean SUMA_CanTalkToAfni (SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_Write_DrawnROI_NIML (SUMA_DRAWN_ROI **ROIv, int N_ROI, char *filename, int Format);
SUMA_NIML_DRAWN_ROI * SUMA_Free_NIMLDrawROI (SUMA_NIML_DRAWN_ROI *nimlROI);
SUMA_NIML_DRAWN_ROI * SUMA_DrawnROI_to_NIMLDrawnROI (SUMA_DRAWN_ROI *ROI);
SUMA_DRAWN_ROI * SUMA_NIMLDrawnROI_to_DrawnROI (SUMA_NIML_DRAWN_ROI * nimlROI, SUMA_Boolean ForDisplay);
void SUMA_FakeIt (int Solo);
SUMA_Boolean SUMA_niml_call (SUMA_CommonFields *cf, int si, SUMA_Boolean fromSUMA);
SUMA_Boolean SUMA_niml_hangup (SUMA_CommonFields *cf, char *nel_stream_name, SUMA_Boolean fromSUMA, SUMA_Boolean killit);
int SUMA_which_stream_index (SUMA_CommonFields *cf, char *nel_stream_name);
SUMA_Boolean SUMA_SendSumaNewSurface(SUMA_SurfaceObject *SO, SUMA_COMM_STRUCT *cs);
SUMA_COMM_STRUCT *SUMA_Create_CommSrtuct(void);
SUMA_COMM_STRUCT *SUMA_Free_CommSrtuct(SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_SendToSuma (SUMA_SurfaceObject *SO, SUMA_COMM_STRUCT *cs, void *data, SUMA_DSET_TYPE dtype, int action);
SUMA_Boolean SUMA_SendToAfni (SUMA_COMM_STRUCT *cs, void *data, int action);
NI_element * SUMA_NodeVal2irgba_nel (SUMA_SurfaceObject *SO, float *val, char *instanceID, int cleanup);
NI_element * SUMA_Mesh_IJK2Mesh_IJK_nel (SUMA_SurfaceObject *SO, int *val, SUMA_Boolean cleanup, SUMA_DSET_TYPE dtype);
SUMA_Boolean SUMA_Mesh_IJK_nel2Mesh_IJK(SUMA_SurfaceObject *SO, NI_element *nel);
NI_element * SUMA_NodeXYZ2NodeXYZ_nel (SUMA_SurfaceObject *SO, float *val, SUMA_Boolean cleanup, SUMA_DSET_TYPE dtype);
SUMA_Boolean SUMA_NodeXYZ_nel2NodeXYZ (SUMA_SurfaceObject *SO, NI_element *nel);
SUMA_Boolean SUMA_Assign_HostName (SUMA_CommonFields *cf, char *HostName, int istream);
SUMA_Boolean SUMA_SendDset_Afni( NI_stream ns,  SUMA_SEND_2AFNI *SS2A, int all);
NI_element *SUMA_SOVolPar2VolPar_nel (SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, SUMA_DSET_TYPE dtype);
SUMA_Boolean SUMA_VolPar_nel2SOVolPar(SUMA_SurfaceObject *SO, NI_element *nel);
void SUMA_Wait_Till_Stream_Goes_Bad(SUMA_COMM_STRUCT *cs, int slp, int WaitMax, int verb);

#endif
