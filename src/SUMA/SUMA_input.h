#ifndef SUMA_INPUT_INCLUDED
#define SUMA_INPUT_INCLUDED

#define SUMA_BRUSH_BLOCK 500

void SUMA_input(Widget w, XtPointer clientData, XtPointer callData);
int SUMA_MarkLineSurfaceIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov);
void SUMA_momentum(XtPointer clientData, XtIntervalId *id);
SUMA_Boolean  SUMA_AddToBrushStroke (SUMA_SurfaceViewer *sv, int x, int y, SUMA_Boolean Show);
SUMA_Boolean  SUMA_CreateBrushStroke (SUMA_SurfaceViewer *sv);
void  SUMA_ClearBrushStroke (SUMA_SurfaceViewer *sv);
void SUMA_ShowBrushStroke (SUMA_SurfaceViewer *sv, FILE *out);
void SUMA_DrawBrushStroke (SUMA_SurfaceViewer *sv, SUMA_Boolean Incremental);
void SUMA_SetSVForegroundColor (SUMA_SurfaceViewer *sv, const char *Color);


#endif
