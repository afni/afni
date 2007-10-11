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
SUMA_Boolean SUMA_Free_Surface_Object (SUMA_SurfaceObject *SO);
void SUMA_Print_Surface_Object(SUMA_SurfaceObject *SO, FILE *Out);
char *SUMA_SurfaceObject_Info (SUMA_SurfaceObject *SO, DList *DsetList);
SUMA_SurfaceObject *SUMA_Alloc_SurfObject_Struct(int N);
SUMA_DRAWN_ROI * SUMA_AllocateDrawnROI (char *Parent_idcode_str, SUMA_ROI_DRAWING_STATUS DrawStatus, 
                                       SUMA_ROI_DRAWING_TYPE Type, char * label, int ilabel);
SUMA_ROI * SUMA_AllocateROI (char *Parent_idcode_str, SUMA_ROI_TYPE Type, char * label, int N_ElInd, int *ElInd);
SUMA_Boolean SUMA_freeDrawnROI (SUMA_DRAWN_ROI *D_ROI); 
SUMA_Boolean SUMA_freeROI (SUMA_ROI *ROI); 
SUMA_Boolean SUMA_Draw_SO_ROI (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, SUMA_SurfaceViewer *csv);
SUMA_DO_Types SUMA_Guess_DO_Type(char *s);
SUMA_SegmentDO * SUMA_Alloc_SegmentDO (int N_n, char *Label, int oriented, char *parent_idcode, SUMA_DO_Types type);
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
SUMA_SphereDO * SUMA_ReadSphDO (char *s);
SUMA_PlaneDO * SUMA_ReadPlaneDO (char *s);
SUMA_SegmentDO *SUMA_CreateSegmentDO(  int N_n, int oriented, int NodeBased, int Stipple,
                                       char *Label, char *idcode_str, char *Parent_idcode_str,
                                       float LineWidth, float *LineCol,
                                       int *NodeId, float *n0, float *n1,
                                       float *colv, float *thickv 
                                       );
SUMA_SurfaceObject *SUMA_Cmap_To_SO (SUMA_COLOR_MAP *Cmap, float orig[3], float topright[3], int verb);
SUMA_Boolean SUMA_DrawLineAxis ( SUMA_AxisSegmentInfo *ASIp, SUMA_Axis *Ax, SUMA_Boolean AddText);
DList *SUMA_SortedAxisSegmentList ( SUMA_SurfaceViewer *sv, SUMA_Axis *Ax, 
                                    SUMA_SORT_BOX_AXIS_OPTION opt);
void SUMA_WorldAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_AxisText(SUMA_AxisSegmentInfo *ASIp, double *Ps);
void SUMA_ReportDrawnROIDatumLength(SUMA_SurfaceObject *SO, SUMA_ROI_DATUM *ROId, FILE *out, SUMA_WIDGET_INDEX_DRAWROI_WHATDIST option);
SUMA_SurfaceObject *SUMA_HJS_Surface(int ipart);
SUMA_SurfaceObject *SUMA_head_01_surface(void);
NI_group *SUMA_SDO2niSDO(SUMA_SegmentDO *SDO);
SUMA_SegmentDO *SUMA_niSDO2SDO(NI_group *ngr); 
SUMA_Boolean SUMA_isSODimInitialized(SUMA_SurfaceObject *SO) ;
SUMA_Boolean SUMA_SetSODims(SUMA_SurfaceObject *SO);

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

