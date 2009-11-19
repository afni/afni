#ifndef SUMA_CREATEDO_INCLUDED
#define SUMA_CREATEDO_INCLUDED
typedef struct {
   char *idcode_str; /* copied by value */
   char *LocalDomainParent; /* copied by value */
   char *LocalDomainParentID;  /* copied by value */
   SUMA_SO_File_Format FileFormat; /*defaults to  SUMA_ASCII*/
   SUMA_SO_File_Type FileType; /*defaults to SUMA_FT_NOT_SPECIFIED*/
   byte DoNormals; /* calculate normals ?*/
   byte DoMetrics; /* calculate metrics? */
   byte DoCenter; /* calculate center ? */
   float LargestBoxSize;
} SUMA_NEW_SO_OPT; 

SUMA_NEW_SO_OPT *SUMA_NewNewSOOpt(void);
SUMA_NEW_SO_OPT *SUMA_FreeNewSOOpt(SUMA_NEW_SO_OPT *nsopt);
SUMA_SurfaceObject *SUMA_NewSO(float **NodeList, int N_Node, int **FaceSetList, int N_FaceSet, SUMA_NEW_SO_OPT *nsooptu);
SUMA_SurfaceObject *SUMA_CreateChildSO(SUMA_SurfaceObject * SO, 
                                       float *NodeList, int N_Node, 
                                       int *FaceSetList, int N_FaceSet,
                                       SUMA_Boolean replace);
SUMA_Axis* SUMA_Alloc_Axis (const char *Name, SUMA_DO_Types type);
void SUMA_Free_Axis (SUMA_Axis *Ax);
SUMA_Boolean SUMA_DrawAxis (SUMA_Axis* Ax, SUMA_SurfaceViewer *csv);
void SUMA_MeshAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceObject *cso);
void SUMA_EyeAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *csv);
void SUMA_Free_CrossHair (SUMA_CrossHair *Ch);
SUMA_CrossHair* SUMA_Alloc_CrossHair (void);
SUMA_Boolean SUMA_DrawCrossHair (SUMA_SurfaceViewer *csv);
void SUMA_Free_SphereMarker (SUMA_SphereMarker *SM);
SUMA_SphereMarker* SUMA_Alloc_SphereMarker (void);
SUMA_Boolean SUMA_DrawFaceSetMarker (SUMA_FaceSetMarker* FM, SUMA_SurfaceViewer *sv);
SUMA_FaceSetMarker* SUMA_Alloc_FaceSetMarker (void);
void SUMA_Free_FaceSetMarker (SUMA_FaceSetMarker* FM);
void SUMA_DrawMesh(SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *csv);
char *SUMA_SO_AnatomicalStructurePrimary(SUMA_SurfaceObject *SO);
char *SUMA_SO_GeometricType(SUMA_SurfaceObject *SO);
char *SUMA_SO_AnatomicalStructureSecondary(SUMA_SurfaceObject *SO);
char *SUMA_SO_TopologicalType(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_MergeAfniSO_In_SumaSO(NI_group **aSOp,
                                        SUMA_SurfaceObject *SO);
NI_group *SUMA_ExtractAfniSO_FromSumaSO( SUMA_SurfaceObject *SO, 
                                                   int CopyData);

SUMA_Boolean SUMA_Free_Surface_Object (SUMA_SurfaceObject *SO);
void SUMA_Print_Surface_Object(SUMA_SurfaceObject *SO, FILE *Out);
char *SUMA_SurfaceObject_Info (SUMA_SurfaceObject *SO, DList *DsetList);
SUMA_SurfaceObject *SUMA_Alloc_SurfObject_Struct(int N);
SUMA_DRAWN_ROI * SUMA_AllocateDrawnROI (char *Parent_idcode_str, SUMA_ROI_DRAWING_STATUS DrawStatus, 
                                       SUMA_ROI_DRAWING_TYPE Type, char * label, int ilabel);
SUMA_ROI * SUMA_AllocateROI (char *Parent_idcode_str, SUMA_ROI_TYPE Type, char * label, int N_ElInd, int *ElInd);
SUMA_Boolean SUMA_freeDrawnROI (SUMA_DRAWN_ROI *D_ROI); 
SUMA_Boolean SUMA_freeROI (SUMA_ROI *ROI); 
SUMA_Boolean SUMA_Draw_SO_ROI (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, 
                               SUMA_SurfaceViewer *csv);
SUMA_Boolean SUMA_Draw_SO_Dset_Contours(SUMA_SurfaceObject *SO, 
                               SUMA_SurfaceViewer *sv);
SUMA_DO_Types SUMA_Guess_DO_Type(char *s);
SUMA_NIDO * SUMA_Alloc_NIDO (char *idcode_str, char *Label, 
                             char *Parent_idcode_str);
SUMA_NIDO *SUMA_free_NIDO(SUMA_NIDO *TDO) ;
SUMA_SegmentDO * SUMA_Alloc_SegmentDO (int N_n, char *Label, int oriented, 
                                       char *parent_idcode, int NodeBased,
                                       SUMA_DO_Types type);
void SUMA_free_SegmentDO (SUMA_SegmentDO * SDO);
SUMA_Boolean SUMA_DrawSegmentDO (SUMA_SegmentDO *SDO, SUMA_SurfaceViewer *sv);
SUMA_SphereDO * SUMA_Alloc_SphereDO (int N_n, char *Label, char *parent_idcode, SUMA_DO_Types type);
SUMA_PlaneDO * SUMA_Alloc_PlaneDO (int N_n, char *Label, SUMA_DO_Types type);
void SUMA_free_SphereDO (SUMA_SphereDO * SDO);
void SUMA_free_PlaneDO (SUMA_PlaneDO * SDO);
SUMA_Boolean SUMA_DrawSphereDO (SUMA_SphereDO *SDO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawPlaneDO (SUMA_PlaneDO *SDO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_isROIdequal (SUMA_ROI_DATUM *ROId1, SUMA_ROI_DATUM *ROId2);
void SUMA_FreeROIDatum (void * data);
SUMA_ROI_DATUM * SUMA_AllocROIDatum (void);
SUMA_Boolean SUMA_PrependToROIdatum (SUMA_ROI_DATUM *ROId1, SUMA_ROI_DATUM *ROId2);
void SUMA_ShowDrawnROI (SUMA_DRAWN_ROI *D_ROI, FILE *out, SUMA_Boolean ShortVersion);
void SUMA_ShowDrawnROIDatum (SUMA_ROI_DATUM *ROId, FILE *out, SUMA_Boolean ShortVersion);
SUMA_Boolean SUMA_AppendToROIdatum (SUMA_ROI_DATUM *ROId1, SUMA_ROI_DATUM *ROId2);
SUMA_ROI_DATUM * SUMA_FillToMask(SUMA_SurfaceObject *SO, int *ROI_Mask, int FirstSurfNode);
void SUMA_FillToMask_Engine (SUMA_NODE_FIRST_NEIGHB *FN, int *Visited, int *ROI_mask, int seed, int *N_Visited, int N_Node);
void SUMA_FillToMask_Engine_old (SUMA_NODE_FIRST_NEIGHB *FN, int *Visited, int *ROI_mask, int seed, int *N_Visited);
SUMA_DRAWN_ROI **SUMA_Find_ROIrelatedtoSO (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_do, int *N_ROI);
SUMA_DRAWN_ROI **SUMA_Find_ROIonSO (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_do, int *N_ROI);
SUMA_Boolean SUMA_Paint_SO_ROIplanes (SUMA_SurfaceObject *SO, 
                                       SUMA_DO* dov, int N_do, 
                                       SUMA_Boolean *MakeNel,
                                       NI_element ***nelvp, int *N_nelv);
SUMA_Boolean SUMA_Paint_SO_ROIplanes_w (SUMA_SurfaceObject *SO, 
                                       SUMA_DO* dov, int N_do);
void SUMA_Free_ROI_PlaneData (void *da);
DList * SUMA_Addto_ROIplane_List (DList *ROIplaneListIn, SUMA_DO *dov, int idov);
int * SUMA_NodesInROI (SUMA_DRAWN_ROI *D_ROI, int *N_Nodes, SUMA_Boolean Unique);
SUMA_DRAWN_ROI * SUMA_1DROI_to_DrawnROI ( int *Node, int N_Node, int Value, char *Parent_idcode_str, 
                                          char *Label, char *ColPlaneName, 
                                          float *FillColor, float *EdgeColor, int EdgeThickness , 
                                          SUMA_DO *dov, int N_dov, SUMA_Boolean ForDisplay);
SUMA_SegmentDO * SUMA_ReadNBVecDO (char *s, int oriented, char *parent_SO_id);
SUMA_SphereDO * SUMA_ReadNBSphDO (char *s, char *parent_SO_id);
SUMA_SegmentDO * SUMA_ReadSegDO (char *s, int oriented, char *soid);
SUMA_SegmentDO * SUMA_ReadNBSegDO (char *s, int oriented, char *soid);
SUMA_SphereDO * SUMA_ReadSphDO (char *s);
SUMA_PlaneDO * SUMA_ReadPlaneDO (char *s);
SUMA_NIDO *SUMA_ReadNIDO(char *s, char *soid);
SUMA_NIDO *SUMA_BlankNIDO (char *idcode_str, char *Label, 
                           char *parent_so_id, char *coord_type,
                           char *font_name);
SUMA_SegmentDO *SUMA_CreateSegmentDO(  int N_n, int oriented, int NodeBased, 
                                       int Stipple,
                                       char *Label, char *idcode_str, 
                                       char *Parent_idcode_str,
                                       float LineWidth, float *LineCol,
                                       int *NodeId, int *NodeId1,
                                       float *n0, float *n1,
                                       float *colv, float *thickv 
                                       );
SUMA_SurfaceObject *SUMA_Cmap_To_SO (SUMA_COLOR_MAP *Cmap, float orig[3], float topright[3], int verb);
SUMA_Boolean SUMA_PrepForNIDOnelPlacement (  SUMA_SurfaceViewer *sv,
                                             NI_element *nel, 
                                             SUMA_SurfaceObject *default_SO,
                                             float *txloc, float *txcoord,
                                             int *sz, 
                                             int *orthoreset,
                                             SUMA_DO_CoordUnits coord_units);
SUMA_Boolean SUMA_DrawImageNIDOnel( NI_element *nel, 
                                    SUMA_SurfaceObject *SO,
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font,
                                    SUMA_SurfaceViewer *sv) ;
SUMA_Boolean SUMA_DrawTextNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font,
                                    SUMA_SurfaceViewer *sv) ;
SUMA_Boolean SUMA_DrawSphereNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    SUMA_SurfaceViewer *sv) ;
SUMA_Boolean SUMA_DrawNIDO (SUMA_NIDO *SDO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawLineAxis ( SUMA_AxisSegmentInfo *ASIp, SUMA_Axis *Ax, SUMA_Boolean AddText);
DList *SUMA_SortedAxisSegmentList ( SUMA_SurfaceViewer *sv, SUMA_Axis *Ax, 
                                    SUMA_SORT_BOX_AXIS_OPTION opt);
void SUMA_WorldAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_AxisText(SUMA_AxisSegmentInfo *ASIp, double *Ps);
SUMA_Boolean SUMA_DrawText(char *txt, float *Ps);
void SUMA_ReportDrawnROIDatumLength(SUMA_SurfaceObject *SO, SUMA_ROI_DATUM *ROId, FILE *out, SUMA_WIDGET_INDEX_DRAWROI_WHATDIST option);
SUMA_SurfaceObject *SUMA_HJS_Surface(int ipart);
SUMA_SurfaceObject *SUMA_head_01_surface(void);
NI_group *SUMA_SDO2niSDO(SUMA_SegmentDO *SDO);
SUMA_SegmentDO *SUMA_niSDO2SDO(NI_group *ngr); 
SUMA_Boolean SUMA_isSODimInitialized(SUMA_SurfaceObject *SO) ;
SUMA_Boolean SUMA_SetSODims(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_MinMaxNodesInROI (SUMA_DRAWN_ROI *D_ROI, 
                                    int MinMax[]);
SUMA_Boolean SUMA_TextBoxSize(char *txt, int *w, int *h, int *nl, void *font);
int SUMA_glutBitmapFontHeight(void *font) ;

static void *FontPointerList[] = 
         {  GLUT_BITMAP_8_BY_13, GLUT_BITMAP_9_BY_15, 
            GLUT_BITMAP_TIMES_ROMAN_10 , GLUT_BITMAP_TIMES_ROMAN_24,
            GLUT_BITMAP_HELVETICA_10, GLUT_BITMAP_HELVETICA_12,
            GLUT_BITMAP_HELVETICA_18, NULL };

void * SUMA_glutBitmapFont(char *fontname);
char * SUMA_glutBitmapFontName(void * font);
char *SUMA_CoordTypeName (SUMA_DO_CoordType tp);
SUMA_DO_CoordType SUMA_CoordType (char *atr);
int SUMA_NodeRange_DrawnROI (SUMA_DRAWN_ROI *ROI, int *min, int *max);
int SUMA_NIDO_TexEnvMode(NI_element *nel, int def);
/*!
   NO Guarantee that certain nodes might 
   get counted twice !
*/
#define SUMA_ROI_CRUDE_COUNT_NODES(m_D_ROI, m_N_max)  \
{  \
   DListElmt *m_NextElm = NULL;  \
   SUMA_ROI_DATUM *m_ROId = NULL; \
   int m_LastOfPreSeg = -1; \
   \
   m_N_max = 0;\
   m_NextElm = NULL;\
   m_LastOfPreSeg = -1;  \
   do {  \
      if (!m_NextElm) m_NextElm = dlist_head(m_D_ROI->ROIstrokelist);   \
      else m_NextElm = dlist_next(m_NextElm);   \
      m_ROId = (SUMA_ROI_DATUM *)m_NextElm->data; \
      if (m_ROId->nPath[0] == m_LastOfPreSeg) {  \
         m_N_max += m_ROId->N_n - 1; \
      }  else {   \
         m_N_max += m_ROId->N_n; \
      }  \
      m_LastOfPreSeg = m_ROId->nPath[m_ROId->N_n - 1];   \
   }while (m_NextElm != dlist_tail(m_D_ROI->ROIstrokelist));   \
   \
}  

#endif


