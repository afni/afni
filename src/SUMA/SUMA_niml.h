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

#endif
