#ifndef SUMA_DISPLAY_INCLUDED
#define SUMA_DISPLAY_INCLUDED

Boolean SUMA_handleRedisplay (XtPointer w);
void SUMA_postRedisplay(Widget w, XtPointer clientData, XtPointer call);
void SUMA_display(SUMA_SurfaceViewer *csv, SUMA_DO *dov);
Colormap SUMA_getShareableColormap(SUMA_SurfaceViewer * csv);
void SUMA_graphicsInit(Widget w, XtPointer clientData, XtPointer call);
void SUMA_expose(Widget w, XtPointer clientData, XtPointer call);
void SUMA_resize(Widget w, XtPointer clientData, XtPointer call);
SUMA_Boolean SUMA_X_SurfaceViewer_Create (void);
void SUMA_ButtOpen_pushed (Widget w, XtPointer cd1, XtPointer cd2);
void SUMA_ButtClose_pushed (Widget w, XtPointer cd1, XtPointer cd2);
SUMA_Boolean SUMA_RenderToPixMap (SUMA_SurfaceViewer *csv, SUMA_DO* dov);
void SUMA_context_Init(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_GetSelectionLine (SUMA_SurfaceViewer *sv, int x, int y);
void SUMA_cb_quit(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_view_surface_controller(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_view_viewer_controller(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_toggle_crosshair(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_toggle_node_in_focus(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_toggle_selected_faceset(Widget w, XtPointer data, XtPointer callData);


#endif
