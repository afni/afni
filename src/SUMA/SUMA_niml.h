#ifndef SUMA_NIML_DEFINED
#define SUMA_NIML_DEFINED

NI_element * SUMA_makeNI_SurfIXYZ (SUMA_SurfaceObject *SO);
NI_element * SUMA_makeNI_SurfIJK (SUMA_SurfaceObject *SO);
Boolean SUMA_niml_workproc( XtPointer thereiselvis );
Boolean SUMA_workprocess( XtPointer fred );
void SUMA_remove_workproc( XtWorkProc func );
void SUMA_register_workproc( XtWorkProc func , XtPointer data );
NI_element * SUMA_makeNI_CrossHair (SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_nel_stdout (NI_element *nel); 
void SUMA_remove_workproc2( XtWorkProc func , XtPointer data );
SUMA_Boolean SUMA_CanTalkToAfni (SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_Write_DrawnROI_NIML (SUMA_DRAWN_ROI **ROIv, int N_ROI, char *filename, int Format); 
SUMA_NIML_DRAWN_ROI * SUMA_Free_NIMLDrawROI (SUMA_NIML_DRAWN_ROI *nimlROI);
SUMA_NIML_DRAWN_ROI * SUMA_DrawnROI_to_NIMLDrawnROI (SUMA_DRAWN_ROI *ROI);
SUMA_DRAWN_ROI * SUMA_NIMLDrawnROI_to_DrawnROI (SUMA_NIML_DRAWN_ROI * nimlROI, SUMA_Boolean ForDisplay);
void SUMA_FakeIt (int Solo);

#endif
