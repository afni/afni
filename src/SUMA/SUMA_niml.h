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
SUMA_Boolean SUMA_Save_DrawnROI_NIML (SUMA_DRAWN_ROI *ROI, char *filename); 
void my_free( void *p );
void * my_realloc( void *p , size_t n );
void * my_malloc( size_t n );
SUMA_NIML_DRAWN_ROI * SUMA_Free_NIMLDrawROI (SUMA_NIML_DRAWN_ROI *nimlROI);
SUMA_NIML_DRAWN_ROI * SUMA_DrawnROI_to_NIMLDrawROI (SUMA_DRAWN_ROI *ROI);

#endif
