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
SUMA_Boolean SUMA_Overlays_2_GLCOLAR4(SUMA_OVERLAYS ** Overlays, int N_Overlays, GLfloat *glcolar, int N_Node, float Back_ModFact);
SUMA_OVERLAYS * SUMA_Fetch_OverlayPointer (SUMA_OVERLAYS **Overlays, int N_Overlays, const char * Name, int * OverInd);
SUMA_Boolean SUMA_Show_ColorOverlayPlanes (SUMA_OVERLAYS **Overlays, int N_Overlays);
SUMA_Boolean SUMA_SetPlaneOrder (SUMA_OVERLAYS **Overlays, int N_Overlays, const char* Name, int NewOrder);
SUMA_Boolean SUMA_MixOverlays (SUMA_OVERLAYS ** Overlays, int N_Overlays, int *ShowOvelays, int N_ShowOverlays, GLfloat *glcolar, int N_Node, SUMA_Boolean *isColored, SUMA_Boolean FILL);




#endif
