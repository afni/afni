#ifndef SUMA_COLOR_INCLUDED
#define SUMA_COLOR_INCLUDED

#define SUMA_ADD_COORD_BIAS_VECT(SO, ovr, BiasDim, BiasVect) {   \
   int m_i, m_i3; \
   switch (BiasDim) {   \
      case SW_CoordBias_X: \
         /* Add X bias */  \
         for (m_i=0; m_i < ovr->N_NodeDef; ++m_i) {   \
            m_i3 = 3*ovr->NodeDef[m_i]; \
            SO->NodeList[m_i3] += BiasVect[m_i];   \
         }  \
         break;   \
      case SW_CoordBias_Y: \
         /* Add Y bias */  \
         for (m_i=0; m_i < ovr->N_NodeDef; ++m_i) {   \
            m_i3 = 3*ovr->NodeDef[m_i]+1;  \
            SO->NodeList[m_i3] += BiasVect[m_i];   \
         }  \
         break;   \
      case SW_CoordBias_Z: \
         /* Add Z bias */  \
         for (m_i=0; m_i < ovr->N_NodeDef; ++m_i) {   \
            m_i3 = 3*ovr->NodeDef[m_i]+2;  \
            SO->NodeList[m_i3] += BiasVect[m_i];   \
         }  \
         break;   \
      case SW_CoordBias_N: \
         /* Add Normal bias */   \
         for (m_i=0; m_i < ovr->N_NodeDef; ++m_i) {   \
            m_i3 = 3*ovr->NodeDef[m_i]; \
            SO->NodeList[m_i3] += BiasVect[m_i] * SO->NodeNormList[m_i3]; ++m_i3;    \
            SO->NodeList[m_i3] += BiasVect[m_i] * SO->NodeNormList[m_i3]; ++m_i3;    \
            SO->NodeList[m_i3] += BiasVect[m_i] * SO->NodeNormList[m_i3];          \
         }  \
         break;   \
      default: \
         SUMA_SL_Err("This should not be.\nWhy, oh why ?"); \
   }  \
}  \

SUMA_COLOR_MAP * SUMA_MakeColorMap (float **Fiducials, int Nfid, int Ncols, SUMA_Boolean SkipLast, char *Name);
void SUMA_Free_ColorMap (SUMA_COLOR_MAP* SM);
int r_ulong_size ( unsigned long l );
int r_sprintf_long_to_hex (char  * dest, unsigned long  lsrc,	int bytes, int	pad);
SUMA_SCALE_TO_MAP_OPT * SUMA_ScaleToMapOptInit(void);
void SUMA_Free_ColorScaledVect (SUMA_COLOR_SCALED_VECT * S);
SUMA_COLOR_SCALED_VECT * SUMA_Create_ColorScaledVect(int N_Node);
SUMA_Boolean SUMA_ScaleToMap (float *V, int N_V, float Vmin, float Vmax, SUMA_COLOR_MAP *ColMap, SUMA_SCALE_TO_MAP_OPT *Opt, SUMA_COLOR_SCALED_VECT * SV);
SUMA_COLOR_MAP * SUMA_GetStandardMap (SUMA_STANDARD_CMAP mapname);
float * SUMA_PercRange (float *V, float *Vsort, int N_V, float *PercRange, float *PercRangeVal, int *iPercRange);
double * SUMA_dPercRange (double *V, double *Vsort, int N_V, double *PercRange, double *PercRangeVal, int *iPercRangeVal);
SUMA_COLOR_MAP* SUMA_MakeColorMap_v2 (float **Fiducials, int Nfid, int *Nint, SUMA_Boolean SkipLast, char *Name);
SUMA_OVERLAYS * SUMA_CreateOverlayPointer (int N_Nodes, const char *Name, SUMA_DSET *dset, char *owner_id);
SUMA_Boolean SUMA_FreeOverlayPointer (SUMA_OVERLAYS * Sover);
SUMA_Boolean SUMA_Overlays_2_GLCOLAR4(SUMA_SurfaceObject *SO, SUMA_SurfaceViewer *sv, GLfloat *glcolar);
SUMA_OVERLAYS * SUMA_Fetch_OverlayPointer (SUMA_OVERLAYS **Overlays, int N_Overlays, const char * Name, int * OverInd);
SUMA_Boolean SUMA_Show_ColorOverlayPlanes (SUMA_OVERLAYS **Overlays, int N_Overlays, int detail);
char *SUMA_ColorOverlayPlane_Info (SUMA_OVERLAYS **Overlays, int N_Overlays, int detail); 
SUMA_Boolean SUMA_MixOverlays (SUMA_OVERLAYS ** Overlays, int N_Overlays, int *ShowOvelays, int N_ShowOverlays, GLfloat *glcolar, int N_Node, SUMA_Boolean *isColored, SUMA_Boolean FILL);
SUMA_Boolean SUMA_MixColors (SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_iRGB_to_OverlayPointer (SUMA_SurfaceObject *SO, char *Name, 
                                          SUMA_OVERLAY_PLANE_DATA *sopd, 
                                          int *PlaneInd, SUMA_DO *dov, 
                                          int N_dov, DList *DsetList);
SUMA_Boolean SUMA_CompactOverlaysOrder (SUMA_SurfaceObject *SO);
void SUMA_FreeOverlayListDatum (void *OLDv);
SUMA_Boolean SUMA_AddNewPlane (SUMA_SurfaceObject *SO, SUMA_OVERLAYS *Overlay, SUMA_DO *dov, int N_dov, int DupFlag);
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
void SUMA_LoadDsetFile (char *filename, void *data);
void SUMA_LoadColorPlaneFile (char *filename, void *data);
SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleColorPlaneList (SUMA_SurfaceObject *SO); 
void SUMA_RefreshDsetList (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_FlushPlaneNotInUse (char *PlaneName, SUMA_SurfaceObject *SO, SUMA_DO *dov, int N_dov);
SUMA_STANDARD_CMAP SUMA_StandardMapCode (char *Name);
char *SUMA_StandardMapName (SUMA_STANDARD_CMAP mapcode, int *N_col);
SUMA_COLOR_MAP *SUMA_Linearize_Color_Map (SUMA_COLOR_MAP* SM, int N_lin);
SUMA_COLOR_MAP *SUMA_Read_Color_Map_1D (char *Name);
SUMA_Boolean SUMA_ScaleToMap_alaAFNI ( float *V, int N_V, 
                                       float range, SUMA_COLOR_MAP *ColMap, 
                                       SUMA_SCALE_TO_MAP_OPT *Opt, 
                                       SUMA_COLOR_SCALED_VECT * SV);
SUMA_AFNI_COLORS * SUMA_Get_AFNI_Default_Color_Maps ();
SUMA_COLOR_MAP ** SUMA_Add_ColorMap (SUMA_COLOR_MAP *CM, SUMA_COLOR_MAP **OldCMv, int *N_maps); 
SUMA_RGB_NAME * SUMA_Add_Color (char *Name, float r, float g, float b, float a, SUMA_RGB_NAME *oCv, int *N_cols);
char *SUMA_ColorMapVec_Info (SUMA_COLOR_MAP **CMv, int N_maps, int detail);
char *SUMA_ColorVec_Info (SUMA_RGB_NAME *Cv, int N_cols); 
void SUMA_Show_ColorMapVec (SUMA_COLOR_MAP **CMv, int N_maps, FILE *Out, int detail);
void SUMA_Show_ColorVec (SUMA_RGB_NAME *CMv, int N_maps, FILE *Out); 
int SUMA_Find_ColorMap ( char *Name, SUMA_COLOR_MAP **CMv, int N_maps, int sgn);
int SUMA_Find_Color ( char *Name, SUMA_RGB_NAME *Cv, int N_cols); 
SUMA_AFNI_COLORS *SUMA_DestroyAfniColors (SUMA_AFNI_COLORS *SAC);
SUMA_Boolean SUMA_Interpret_AFNIColor (char *Name, float RGB[3]);
int SUMA_AFNI_Extract_Colors ( char *fname, SUMA_AFNI_COLORS *SAC );
void SUMA_Flip_Color_Map (SUMA_COLOR_MAP *CM);
int SUMA_ColorizePlane (SUMA_OVERLAYS *cp);
SUMA_AFNI_COLORS *SUMA_Build_Color_maps(void);
char *SUMA_ScaleToMapOpt_Info (SUMA_SCALE_TO_MAP_OPT *OptScl, int detail);
SUMA_Boolean SUMA_ShowScaleToMapOpt(SUMA_SCALE_TO_MAP_OPT *OptScl, FILE *Out, int detail);
SUMA_Boolean SUMA_SetConvexityPlaneDefaults(SUMA_SurfaceObject *SO, DList *DsetList);
SUMA_COLOR_MAP *SUMA_CmapOfPlane (SUMA_OVERLAYS *Sover );
SUMA_Boolean SUMA_SetSO_CoordBias(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *ovr, float *NewBias, SUMA_WIDGET_INDEX_COORDBIAS BiasDim);
SUMA_Boolean SUMA_SetCoordBias(SUMA_OVERLAYS *ovr, float *NewBias, SUMA_WIDGET_INDEX_COORDBIAS BiasDim);
SUMA_Boolean SUMA_RemoveSO_CoordBias(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *ovr);
SUMA_Boolean SUMA_RemoveCoordBias(SUMA_OVERLAYS *ovr); 
SUMA_Boolean SUMA_TransferSO_CoordBias(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *ovr, SUMA_WIDGET_INDEX_COORDBIAS BiasDim);
SUMA_Boolean SUMA_TransferCoordBias(SUMA_OVERLAYS *ovr, SUMA_WIDGET_INDEX_COORDBIAS BiasDim);
SUMA_Boolean SUMA_NewSurfaceGeometry(SUMA_SurfaceObject *SO);
int SUMA_GetNodeOverInd (SUMA_OVERLAYS *Sover, int node);
SUMA_Boolean SUMA_isColumn_inferred(NI_element *nel, int icol);
SUMA_Boolean SUMA_AddNodeIndexColumn(SUMA_DSET *dset, SUMA_SurfaceObject *SO); 
SUMA_Boolean SUMA_OKassign(SUMA_DSET *dset, SUMA_SurfaceObject *SO);










#endif
