#ifndef SUMA_COLOR_INCLUDED
#define SUMA_COLOR_INCLUDED

SUMA_COLOR_MAP * SUMA_MakeColorMap (float **Fiducials, int Nfid, int Ncols, SUMA_Boolean SkipLast, char *Name);
void SUMA_Free_ColorMap (SUMA_COLOR_MAP* SM);
int r_ulong_size ( unsigned long l );
int r_sprintf_long_to_hex (char  * dest, unsigned long  lsrc,	int bytes, int	pad);
SUMA_SCALE_TO_MAP_OPT * SUMA_ScaleToMapOptInit(void);
void SUMA_Free_ColorScaledVect (SUMA_COLOR_SCALED_VECT * S);
SUMA_COLOR_SCALED_VECT * SUMA_Create_ColorScaledVect(int N_Node);
SUMA_Boolean SUMA_ScaleToMap (float *V, int N_V, float Vmin, float Vmax, SUMA_COLOR_MAP *ColMap, SUMA_SCALE_TO_MAP_OPT *Opt, SUMA_COLOR_SCALED_VECT * SV);
SUMA_COLOR_MAP * SUMA_GetStandardMap (SUMA_STANDARD_CMAP mapname);
float * SUMA_PercRange (float *V, float *Vsort, int N_V, float *PercRange, float *PercRangeVal);
SUMA_COLOR_MAP* SUMA_MakeColorMap_v2 (float **Fiducials, int Nfid, int *Nint, SUMA_Boolean SkipLast, char *Name);
SUMA_OVERLAYS * SUMA_CreateOverlayPointer (int N_Nodes, const char *Name);
SUMA_Boolean SUMA_FreeOverlayPointer (SUMA_OVERLAYS * Sover);
SUMA_Boolean SUMA_Overlays_2_GLCOLAR4(SUMA_OVERLAYS ** Overlays, int N_Overlays, GLfloat *glcolar, int N_Node, float Back_ModFact, SUMA_Boolean ShowBack, SUMA_Boolean ShowFore);
SUMA_OVERLAYS * SUMA_Fetch_OverlayPointer (SUMA_OVERLAYS **Overlays, int N_Overlays, const char * Name, int * OverInd);
SUMA_Boolean SUMA_Show_ColorOverlayPlanes (SUMA_OVERLAYS **Overlays, int N_Overlays);
char *SUMA_ColorOverlayPlane_Info (SUMA_OVERLAYS **Overlays, int N_Overlays); 
SUMA_Boolean SUMA_MixOverlays (SUMA_OVERLAYS ** Overlays, int N_Overlays, int *ShowOvelays, int N_ShowOverlays, GLfloat *glcolar, int N_Node, SUMA_Boolean *isColored, SUMA_Boolean FILL);
SUMA_Boolean SUMA_MixColors (SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_iRGB_to_OverlayPointer (SUMA_SurfaceObject *SO, char *Name, SUMA_OVERLAY_PLANE_DATA *sopd, int *PlaneInd, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_CompactOverlaysOrder (SUMA_SurfaceObject *SO);
void SUMA_FreeOverlayListDatum (void *OLDv);
SUMA_Boolean SUMA_AddNewPlane (SUMA_SurfaceObject *SO, SUMA_OVERLAYS *Overlay, SUMA_INODE *Overlay_Inode);
SUMA_Boolean SUMA_isOverlayOfSO (SUMA_SurfaceObject *SO, SUMA_OVERLAYS *Plane);
int SUMA_GetSmallestForegroundOrder (DList *listop);
int SUMA_GetLargestBackroundOrder (DList *listop);
DList * SUMA_OverlaysToOrderedList (SUMA_SurfaceObject *SO, int Opt);
SUMA_Boolean SUMA_ReleaseOverlay (SUMA_OVERLAYS * Overlays, SUMA_INODE *Overlays_Inode);
char * SUMA_PlaneOrder_Info(SUMA_SurfaceObject *SO);
void SUMA_Print_PlaneOrder (SUMA_SurfaceObject *SO, FILE *Out);
SUMA_Boolean SUMA_ListOrderToPlaneOrder (DList *listop); 
SUMA_Boolean SUMA_MovePlaneUp (SUMA_SurfaceObject *SO, char *Name);
SUMA_Boolean SUMA_MovePlaneDown (SUMA_SurfaceObject *SO, char *Name);
void SUMA_LoadColorPlaneFile (char *filename, void *data);
SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleColorPlaneList (SUMA_SurfaceObject *SO); 
void SUMA_RefreshColorPlaneList (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_FlushPlaneNotInUse (char *PlaneName, SUMA_SurfaceObject *SO, SUMA_DO *dov, int N_dov);








#endif
