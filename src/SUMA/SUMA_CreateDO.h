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

typedef struct {
   char state_s[32]; /*!< state name */
   char now_s[16]; /*!< current value in string format*/
   char init_s[16]; /*!< initial value in string format*/
   int now_i;
   int init_i;
   float now_f4[4];
   float init_f4[4];
   char whodunit[32]; /*!< Who wanted this state tracked */
} SUMA_GL_STEL;

typedef struct {
   char *idcode_str;
   float *NodeList;
   int N_Node;
   int *NodeIndex;
   
   float AvgLe;
   byte err;
} SUMA_DUMB_DO;   /* A structure to hold pointer copies for use by drawing
                     functions. Nothing in here should be allocated for.
                     All use of this structure should be temporary to the
                     drawing function */

/* A structure to create a hash table for locating NIML elements
   for a certain edge in a graph dataset */
typedef struct {
    int id;    /* keep it named 'id' to facilitate use of convenience
                  macros in uthash . The integer id of an edge: its index*/
    int ngrindex; /* the index into ngr->part of the ni element in question.
                     ngr is the AFNI_dataset group element */
    UT_hash_handle hh;  /* keep it named 'hh' for same reasons  */
}  SUMA_NGR_INDEX_HASH_DATUM;

/*! Graph dataset Auxiliary structure for SUMA's use */
typedef struct {
   DList *DisplayUpdates;
   SUMA_SegmentDO *SDO;
   SUMA_NIDO *nido;
   SUMA_SurfaceObject *FrameSO; /*!< Matrix's holder surface object */
   SUMA_OVERLAYS *Overlay;
   SUMA_X_SurfCont *DOCont;/*!< Displayable object controller */
   SUMA_PICK_RESULT *PR;
   SUMA_Boolean *isColored; /*!< is the datum receiving color? Not masked say 
                                 by thresholds etc. */
   NI_group *net; /*!< A network group holding tract indexed in thd. 
                       if net is NULL, then thd indexes into dset->ngr*/
   SUMA_NGR_INDEX_HASH_DATUM *thd; /*! A hash table pointing to niml element
                                       containing tract to represent path
                                       between two points */
   SUMA_Boolean ShowBundles; /*!< Show bundles instead of edge if possible */
   SUMA_Boolean ShowUncon; /*!< Show graph points (nodes) even if not 
                                  connected */
   SUMA_Boolean IgnoreSelection; /*!< Ignore selection mode when displaying
                                      Currently used to show all graph, even
                                      when one node is selected */
   float *Center_G3D; /* Geometric center of all points in 3D variant*/
   float *Range_G3D;  /* Min Max of X, Y, and Z of all points in 3D variant*/
   float *Center_GMATRIX; /* Geometric center of all points in MATRIX 
                             variant*/
   float *Range_GMATRIX;  /* Min Max of X, Y, and Z of all points in MATRIX 
                             variant*/
} SUMA_GRAPH_SAUX;

/*! A Tract object's Auxiliary structure for SUMA's use */
typedef struct {
   DList *DisplayUpdates;
   SUMA_X_SurfCont *DOCont;/*!< Displayable object controller */
   SUMA_PICK_RESULT *PR;
   SUMA_OVERLAYS **Overlays;
   int N_Overlays;
   SUMA_Boolean *isColored; /*!< is the datum receiving color? Not masked say 
                                 by thresholds etc. */
   int TractMask;
   float MaskGray;
   float *tract_lengths;
   
   float *Center; /* Geometric center of all points */
   float *Range;  /* Min Max of X, Y, and Z of all points */
} SUMA_TRACT_SAUX;

/*! A CIFTI object's Auxiliary structure for SUMA's use */
typedef struct {
   DList *DisplayUpdates;
   SUMA_X_SurfCont *DOCont;/*!< Displayable object controller */
   SUMA_PICK_RESULT *PR;
   SUMA_OVERLAYS **Overlays;
   int N_Overlays;
   SUMA_Boolean *isColored; /*!< is the datum receiving color? Not masked say 
                                 by thresholds etc. */
   
   float *Center; /* Geometric center of all points */
   float *Range;  /* Min Max of X, Y, and Z of all points */
} SUMA_CIFTI_SAUX;

/*! A Mask object's Auxiliary structure for SUMA's use */
typedef struct {
   DList *DisplayUpdates;
   SUMA_X_SurfCont *DOCont;/*!< Displayable object controller */
   SUMA_PICK_RESULT *PR;
   SUMA_OVERLAYS **Overlays;
   int N_Overlays;
   SUMA_Boolean *isColored; /*!< is the datum receiving color? Not masked say 
                                 by thresholds etc. */
} SUMA_MASK_SAUX;


/*! A Surface object's Auxiliary structure for SUMA's use */
typedef struct {
   #if 0 /* Not in use yet*/
   DList *DisplayUpdates;
   SUMA_X_SurfCont *DOCont;/*!< Displayable object controller */
   #endif 
   SUMA_PICK_RESULT *PR;
   #if 0 /* Not in use yet*/
   SUMA_OVERLAYS **Overlays;
   int N_Overlays;
   SUMA_Boolean *isColored; /*!< is the datum receiving color? Not masked say 
                                 by thresholds etc. */
   #endif
} SUMA_SURF_SAUX;

typedef struct {
   float Eq[4];
   int slc_num;
   char variant[16];
} SUMA_RENDERED_SLICE; /*!< Information about a rendered slice */

/*! A volume object's Aux structure for SUMA's use */
typedef struct {
   DList *DisplayUpdates;
   SUMA_X_SurfCont *DOCont;/*!< Displayable object controller */
   SUMA_PICK_RESULT *PR;
   SUMA_PICK_RESULT *PRc;
   SUMA_OVERLAYS **Overlays;
   int N_Overlays;
   SUMA_Boolean *isColored; /*!< is the datum receiving color? Not masked say 
                                 by thresholds etc. */
   DList *slcl; /* Rendered slices, top slice rendered last */
   DList *vrslcl;
   
   char *State; /* Normally the state of a volume is ANY_ANATOMICAL,
      	           but when it is a domain of a CIFTI object, we will
		   change its state to reflect that fact. If we don't
		   do that, the volume itself would get registered
		   (and therefore displayed) in any anatomically correct state 
		   with complete disregard for its lineage. We want to
		   display the volume only when the CIFTI object is being
		   displayed. */ 
   int ShowAxSlc;
   int ShowSaSlc;
   int ShowCoSlc;
   int ShowVrSlc;
   int VrSelect;
   
   int SlicesAtCrosshair; /* Make three slices jump to location of crosshair */
   SUMA_ATRANS_MODES TransMode; /*!< polygon transparency  */
} SUMA_VOL_SAUX;

#define SDSET_GSAUX(dset) ( ( (dset) && (dset)->Aux && (dset)->Aux->Saux &&   \
                               SUMA_isGraphDset(dset) ) ? \
                           (SUMA_GRAPH_SAUX *)((dset)->Aux->Saux):NULL )

#define SDSET_GOVERLAY(dset) (( (dset) && (dset)->Aux && (dset)->Aux->Saux &&   \
                               SUMA_isGraphDset(dset) ) ? \
                  ((SUMA_GRAPH_SAUX *)(dset)->Aux->Saux)->Overlay:NULL )
#define SDSET_COVERLAY(dset) (( (dset) && (dset)->Aux && (dset)->Aux->Saux &&   \
                               SUMA_isCIFTIDset(dset) ) ? \
                  ((SUMA_CIFTI_SAUX *)(dset)->Aux->Saux)->Overlay:NULL )
                  
#define SDSET_GMATSO(dset) (( (dset) && (dset)->Aux && (dset)->Aux->Saux &&   \
                               SUMA_isGraphDset(dset) ) ? \
                  ((SUMA_GRAPH_SAUX *)(dset)->Aux->Saux)->FrameSO:NULL )
                  
#define TDO_HAS_GRID(tdo) ( ((tdo) && (tdo)->net && (tdo)->net->grid) ? \
                              (tdo)->net->grid : NULL )

#define TDO_N_BUNDLES(tdo) ( ((tdo) && (tdo)->net ) ? (tdo)->net->N_tbv : -1 )
#define TDO_N_TRACTS(tdo) ( ((tdo) ) ? Network_N_tracts((tdo)->net, 0) : -1 )

#define TDO_BUNDLE(tdo, tbi) ( ((tdo) && (tdo)->net && (tdo)->net->tbv && \
                                 tbi >= 0 && tbi < (tdo)->net->N_tbv) ? \
                                                   (tdo)->net->tbv[tbi] : NULL )
#define TDO_TSAUX(tdo) ( (tdo) ? (SUMA_TRACT_SAUX *)(tdo)->Saux:NULL )
#define CDO_CSAUX(cdo) ( (cdo) ? (SUMA_CIFTI_SAUX *)(cdo)->Saux:NULL )
#define VDO_VSAUX(vo) ( (vo) ? (SUMA_VOL_SAUX *)(vo)->Saux:NULL )
#define SDO_SSAUX(so) ( (so) ? (SUMA_SURF_SAUX *)(so)->Saux:NULL )
#define MDO_MSAUX(mo) ( (mo) ? (SUMA_MASK_SAUX *)(mo)->Saux:NULL )

#define VE_NX(ve) SUMA_VE_Ni(&ve, 0)
#define VE_NY(ve) SUMA_VE_Nj(&ve, 0)
#define VE_NZ(ve) SUMA_VE_Nk(&ve, 0)
#define VE_NXY(ve) SUMA_VE_Niy(&ve, 0)
#define VE_NVOX(ve) SUMA_VE_Nvox(&ve, 0)

#define VO_NI(vo) ( (vo) ?  \
                        SUMA_VE_Ni((vo)->VE, 0) : -1 )
#define VO_NIJ(vo) ( (vo) ?  \
                        SUMA_VE_Nij((vo)->VE, 0) : -1 )
#define VO_NJ(vo) ( (vo) ?  \
                        SUMA_VE_Nj((vo)->VE, 0) : -1 )
#define VO_NK(vo) ( (vo) ?  \
                        SUMA_VE_Nk((vo)->VE, 0) : -1 )
#define VO_NVOX(vo) ( (vo)  ?  \
                        SUMA_VE_Nvox((vo)->VE, 0) : -1 )
#define VO_N_VOLS(vo) ( SUMA_VO_NumVE(vo) )

#define MDO_IS_SHADOW(MDO) ( ((MDO) && (MDO)->mtype[0] == 'C' && \
                                       (MDO)->mtype[1] == 'A' && \
                                       (MDO)->mtype[2] == 'S' && \
                                       (MDO)->mtype[3] == 'P' && \
                                       (MDO)->mtype[4] == 'E' && \
                                       (MDO)->mtype[5] == 'R' && \
                                       (MDO)->mtype[6] == '\0' ) ? 1:0 )
#define MDO_IS_BOX(MDO) ( ((MDO) && (!strcasecmp((MDO)->mtype,"box") || \
                                     !strcasecmp((MDO)->mtype,"cube"))) ? 1:0 )
#define MDO_IS_SPH(MDO) ( ((MDO) && (!strcasecmp((MDO)->mtype,"ball") || \
                                     !strcasecmp((MDO)->mtype,"sphere"))) ? 1:0 )
#define MDO_IS_SURF(MDO) ( ((MDO) && (!strcasecmp((MDO)->mtype,"surf"))) ? 1:0 )

SUMA_Boolean SUMA_DrawDO_UL_FullMonty(DList *dl);
SUMA_Boolean SUMA_ADO_UL_Add(SUMA_ALL_DO *ado, char *com, int replace);
SUMA_Boolean SUMA_DrawDO_UL_Add(DList *dl, char *com, int replace); 
DListElmt *SUMA_DrawDO_UL_Find(DList *dl, char *com); 
SUMA_Boolean SUMA_DrawDO_UL_EmptyList(DList *dl, DListElmt *del);
SUMA_Boolean SUMA_DestroyNgrHashDatum(SUMA_NGR_INDEX_HASH_DATUM *thd);
void SUMA_Free_GSaux(void *vSaux);
void SUMA_Free_SSaux(void *vSaux);
void SUMA_Free_TSaux(void *vSaux);
void SUMA_Free_MSaux(void *vSaux);
void SUMA_Free_VSaux(void *vSaux);
void SUMA_Free_CSaux(void *vSaux);
void SUMA_Free_Saux_DisplayUpdates_datum(void *ddd);
SUMA_Boolean SUMA_AddTractSaux(SUMA_TractDO *tdo);
SUMA_Boolean SUMA_AddCIFTISaux(SUMA_CIFTI_DO *cdo);
float SUMA_TDO_tract_length(SUMA_TractDO *tdo, int tt);
SUMA_Boolean SUMA_AddVolSaux(SUMA_VolumeObject *vo);
void SUMA_Free_SliceListDatum(void *data);
SUMA_Boolean SUMA_AddMaskSaux(SUMA_MaskDO *mdo);
SUMA_Boolean SUMA_AddDsetSaux(SUMA_DSET *dset);
SUMA_Boolean SUMA_Load_Dumb_DO(SUMA_ALL_DO *ado, SUMA_DUMB_DO *DDO);
SUMA_Boolean SUMA_SetDrawVariant(SUMA_DSET *dset, char *variant);
char *SUMA_GetDrawVariant(SUMA_DSET *dset);
SUMA_Boolean SUMA_UnSetDrawVariant(SUMA_DSET *dset);
SUMA_Boolean SUMA_SetDrawVariant(SUMA_DSET *dset, char *variant);
SUMA_Boolean SUMA_isDrawVariant(SUMA_DSET *dset, char *variant);
int SUMA_GDSET_edgeij_to_GMATRIX_XYZ(SUMA_DSET *dset, 
                                        int ei, int ej, float *XYZ, int FC);
int SUMA_GDSET_GMATRIX_CellPixSize(SUMA_DSET *dset, SUMA_SurfaceViewer *sv,
                                   float *Sz);
float *SUMA_GDSET_NodeList(SUMA_DSET *dset, int *N_Node, int recompute,    
                           int **ind, char *thisvariant); 
float *SUMA_CDOM_NodeList(SUMA_CIFTI_DO *CO, int *N_Node, int recompute, 
                           int **ind);
NI_element * SUMA_SO_NIDO_Node_Texture (  SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                                          int N_do, SUMA_SurfaceViewer *sv );
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
void SUMA_MeshAxisStandard (SUMA_Axis* Ax, SUMA_ALL_DO *cso);
void SUMA_EyeAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *csv);
void SUMA_Free_CrossHair (SUMA_CrossHair *Ch);
SUMA_CrossHair* SUMA_Alloc_CrossHair (void);
SUMA_Boolean SUMA_DrawCrossHair (SUMA_SurfaceViewer *csv);
void SUMA_Free_SphereMarker (SUMA_SphereMarker *SM);
SUMA_SphereMarker* SUMA_Alloc_SphereMarker (void);
SUMA_Boolean SUMA_DrawFaceSetMarker (SUMA_FaceSetMarker* FM, 
                                     SUMA_SurfaceViewer *sv);
SUMA_FaceSetMarker* SUMA_Alloc_FaceSetMarker (void);
void SUMA_Free_FaceSetMarker (SUMA_FaceSetMarker* FM);
int SUMA_NodeMask_to_FaceMask(SUMA_SurfaceObject *SO, byte *nodemask,
                              int N_nz_nodemask,
                              int *triblock, byte **facemask, 
                              int minhits);
int SUMA_Prep_SO_DrawPatches(SUMA_SurfaceObject *SO, SUMA_SurfaceViewer *sv);
SUMA_DrawPatch *SUMA_New_DrawPatchDatum(SUMA_SurfaceObject *SO, int *triblock,
                                        int N_Faces, byte *facemask);
void SUMA_Free_DrawPatchDatum(void *data);
int SUMA_ComplimentaryPatches(SUMA_SurfaceObject *SO, int *triblock, 
                              int N_Faces, byte *facemask, 
                              SUMA_DrawPatch **ptch0, 
                              SUMA_DrawPatch **ptch1);
void SUMA_DrawMesh_mask(SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *sv);
void SUMA_DrawMesh(SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *csv);
void SUMA_SimpleDrawMesh(SUMA_SurfaceObject *SurfObj, 
                         GLfloat *colp, SUMA_SurfaceViewer *sv);
SUMA_VIS_XFORM_DATUM *SUMA_NewVisXdatum(char *label);
void SUMA_FreeVisXdatum (void *vxd);

int SUMA_EmptyVisXform(SUMA_VIS_XFORM *vx);
int SUMA_ApplyVisXform(SUMA_SurfaceObject *SO, char *which, 
       SUMA_VISX_XFORM_DIRECTIONS direction, int recompute_norm);
float *SUMA_VisX_CoordPointer(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_VisX_Pointers4Display(SUMA_SurfaceObject *SO, int fordisp);
int SUMA_AllowPrying(SUMA_SurfaceViewer *sv, int *RegSO);
SUMA_Boolean SUMA_ResetPrying(SUMA_SurfaceViewer *svu);
SUMA_Boolean SUMA_ApplyPrying(SUMA_SurfaceViewer *sv, float val[3], char *units,
                              int recompute_norm);
SUMA_Boolean SUMA_RecomputeNormsPrying(SUMA_SurfaceViewer *svu);
int SUMA_LeftShownOnLeft(SUMA_SurfaceViewer *sv, 
                         SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2,
                         int useParents, int applyViewingXform);
int SUMA_ComputeVisX(SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2, 
                     SUMA_SurfaceViewer *csv, char *which, int recompute_norm );
char *SUMA_SO_AnatomicalStructurePrimary(SUMA_SurfaceObject *SO);
char *SUMA_SO_GeometricType(SUMA_SurfaceObject *SO);
char *SUMA_SO_AnatomicalStructureSecondary(SUMA_SurfaceObject *SO);
char *SUMA_SO_TopologicalType(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_MergeAfniSO_In_SumaSO(NI_group **aSOp,
                                        SUMA_SurfaceObject *SO);
NI_group *SUMA_ExtractAfniSO_FromSumaSO( SUMA_SurfaceObject *SO, 
                                                   int CopyData);
SUMA_Boolean SUMA_Blank_AfniSO_Coord_System(NI_group *aSO);
char *SUMA_SideName(SUMA_SO_SIDE ss);
SUMA_SO_SIDE SUMA_SideType(char *s);
SUMA_Boolean SUMA_Free_Surface_Object (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_FreeDrawMasks(SUMA_DRAW_MASKS * DW);
SUMA_Boolean SUMA_EmptyDrawMasks(SUMA_DRAW_MASKS * DW);
SUMA_VolumeObject *SUMA_FreeVolumeObject(SUMA_VolumeObject *VO);
SUMA_CIFTI_DO *SUMA_FreeCIFTIObject(SUMA_CIFTI_DO *CO); 
SUMA_CIFTI_DO *SUMA_CreateCIFTIObject(char *Label);
void SUMA_Print_Surface_Object(SUMA_SurfaceObject *SO, FILE *Out);
char *SUMA_VisX_XformType2Name(SUMA_VISX_XFORM_TYPE tt);
char *SUMA_VisX_Info(SUMA_VIS_XFORM VisX, int N_Node, char *mumble);
char *SUMA_SurfaceObject_Info (SUMA_SurfaceObject *SO, DList *DsetList);
char *SUMA_ADO_Info(SUMA_ALL_DO *ado, DList *DsetList, int detail);
SUMA_SurfaceObject *SUMA_Alloc_SurfObject_Struct(int N);
int SUMA_VO_NumVE(SUMA_VolumeObject *VO);
SUMA_DSET *SUMA_VO_dset(SUMA_VolumeObject *VO);
SUMA_DSET *SUMA_VE_dset(SUMA_VolumeElement **VE, int idset);
SUMA_VolumeObject *SUMA_CreateVolumeObject(char *label);
SUMA_Boolean SUMA_AddDsetVolumeObject( SUMA_VolumeObject *VO, 
                                       THD_3dim_dataset **dsetp);
SUMA_DRAWN_ROI * SUMA_AllocateDrawnROI (char *Parent_idcode_str, 
                           SUMA_ROI_DRAWING_STATUS DrawStatus, 
                           SUMA_ROI_DRAWING_TYPE Type, char * label, int ilabel);
SUMA_ROI * SUMA_AllocateROI (char *Parent_idcode_str, SUMA_ROI_TYPE Type, 
                             char * label, int N_ElInd, int *ElInd);
SUMA_Boolean SUMA_freeDrawnROI (SUMA_DRAWN_ROI *D_ROI); 
SUMA_Boolean SUMA_freeROI (SUMA_ROI *ROI); 
SUMA_Boolean SUMA_Draw_SO_ROI (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, 
                               SUMA_SurfaceViewer *csv);
SUMA_Boolean SUMA_Draw_SO_Dset_Contours(SUMA_SurfaceObject *SO, 
                               SUMA_SurfaceViewer *sv);
SUMA_DO_Types SUMA_Guess_DO_Type(char *s);
SUMA_MaskDO *SUMA_SymMaskDO(char *s, char *mtype, char *hid, byte mtypeonly);
SUMA_Boolean SUMA_MDO_OkVarName(char *this);
SUMA_Boolean SUMA_MDO_SetVarName(SUMA_MaskDO *mdo, char *this);
SUMA_MaskDO *SUMA_MDO_GetVar(char *vn);
SUMA_Boolean SUMA_AccessorizeMDO(SUMA_MaskDO *MDO);
SUMA_Boolean SUMA_isSymMaskDO(char *s, char *mtype);
SUMA_Boolean SUMA_Ok_Sym_MaskDO_Type(char *mtype);
SUMA_Boolean SUMA_Guess_Str_MaskDO_Type(char *s, char *mtype);
SUMA_Boolean SUMA_Set_MaskDO_Type(SUMA_MaskDO *mdo, char *mtype);
SUMA_Boolean SUMA_Set_MaskDO_Cen(SUMA_MaskDO *mdo, float *cen);
SUMA_Boolean SUMA_Set_MaskDO_Dim(SUMA_MaskDO *mdo, float *dim);
SUMA_Boolean SUMA_Set_MaskDO_Trans(SUMA_MaskDO *mdo, SUMA_TRANS_MODES T);
SUMA_Boolean SUMA_Set_MaskDO_Alpha(SUMA_MaskDO *mdo, float alpha);
SUMA_Boolean SUMA_Set_MaskDO_Color(SUMA_MaskDO *mdo, float *col, float dim);
SUMA_Boolean SUMA_Set_MaskDO_Label(SUMA_MaskDO *mdo, char *lab);
#define SUMA_MDO_New_Cen(mdo, cen) \
            SUMA_MDO_New_Params((mdo), (cen), NULL, NULL, NULL, NULL, \
                                 -1, STM_N_TransModes, -1)
#define SUMA_MDO_New_Type(mdo, ttype) \
            SUMA_MDO_New_Params((mdo), NULL, NULL, NULL, NULL, (ttype), \
                                 -1, STM_N_TransModes, -1)
#define SUMA_MDO_New_Label(mdo, ttype) \
            SUMA_MDO_New_Params((mdo), NULL, NULL, NULL, (ttype), NULL, \
                                 -1, STM_N_TransModes, -1)
#define SUMA_MDO_New_Dim(mdo, dim) \
            SUMA_MDO_New_Params((mdo), NULL, (dim), NULL, NULL, NULL, \
                                 -1, STM_N_TransModes, -1)
#define SUMA_MDO_New_Color(mdo, col) \
            SUMA_MDO_New_Params((mdo), NULL, NULL, (col), NULL, NULL, \
                                 -1, STM_N_TransModes, -1)
#define SUMA_MDO_New_Trans(mdo, tran) \
            SUMA_MDO_New_Params((mdo), NULL, NULL, NULL, NULL, NULL, \
                                 -1, tran, -1)
#define SUMA_MDO_New_Alpha(mdo, alpha) \
            SUMA_MDO_New_Params((mdo), NULL, NULL, NULL, NULL, NULL, \
                                 alpha, STM_N_TransModes, -1)
#define SUMA_MDO_New_CDim(mdo, cdim) \
            SUMA_MDO_New_Params((mdo), NULL, NULL, NULL, NULL, NULL, \
                                 -1, STM_N_TransModes, cdim)
int SUMA_MDO_New_Doppel(SUMA_MaskDO *mdo, float *xyz);
int SUMA_MDO_New_parent(SUMA_MaskDO *mdo, char *parent_id, int parent_datum_id);
int SUMA_MDO_New_Params(SUMA_MaskDO *mdo, float *cen, float *dim, 
                        float *col, char *Label, char *Type,
                        float alpha, SUMA_TRANS_MODES tran, float cdim);
SUMA_NIDO * SUMA_Alloc_NIDO (char *idcode_str, char *Label, 
                             char *Parent_idcode_str);
SUMA_NIDO *SUMA_free_NIDO(SUMA_NIDO *NIDO); 
int SUMA_ProcessDODrawMask(SUMA_SurfaceViewer *sv,          
                                    SUMA_SurfaceObject *SO,
                                    byte **mask, int *ncross);
SUMA_SegmentDO * SUMA_Alloc_SegmentDO (int N_n, char *Label, int oriented, 
                                      char *parent_idcode,
                                      int NodeBased, SUMA_DO_Types type, 
                                    SUMA_DO_Types P_type, char *DrawnDO_variant);
void SUMA_free_SegmentDO (SUMA_SegmentDO * SDO);
SUMA_MaskDO *SUMA_Alloc_MaskDO ( int N_obj, char *Label, char *hash_label,
                                 char *parent_ADO_id, 
                                 int withcol);
void SUMA_free_MaskDO (SUMA_MaskDO * MDO);
int SUMA_Set_N_SegNodes_SegmentDO(SUMA_SegmentDO * SDO, int N);
int SUMA_Set_N_AllNodes_SegmentDO(SUMA_SegmentDO * SDO, int N);
SUMA_Boolean SUMA_DrawSegmentDO (SUMA_SegmentDO *SDO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawGSegmentDO (SUMA_GRAPH_SAUX *GSaux, 
                                  SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawTractDO (SUMA_TractDO *TDO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawMaskDO (SUMA_MaskDO *MDO, SUMA_SurfaceViewer *sv);
float *SUMA_ADO_Center(SUMA_ALL_DO *ado, float *here);
float *SUMA_ADO_Range(SUMA_ALL_DO *ado, float *here);
float *SUMA_TDO_Grid_Center(SUMA_TractDO *tdo, float *here);
float *SUMA_MDO_Center(SUMA_MaskDO *MDO, float *here);
float *SUMA_VO_Grid_Center(SUMA_VolumeObject *vo, float *here);
int SUMA_TDO_N_tracts(SUMA_TractDO *tdo);
int SUMA_TDO_Max_N_tracts(SUMA_TractDO *tdo); 
int SUMA_VE_Nk(SUMA_VolumeElement **VE, int ivo); 
int SUMA_VE_Nj(SUMA_VolumeElement **VE, int ivo);
int SUMA_VE_Ni(SUMA_VolumeElement **VE, int ivo);
int SUMA_VE_Niy(SUMA_VolumeElement **VE, int ivo);
int SUMA_VE_Nvox(SUMA_VolumeElement **VE, int ivo);
int SUMA_VO_N_Slices(SUMA_VolumeObject *VO, char *variant);
int SUMA_VE_N_Slices(SUMA_VolumeElement **VE, int ivo, char *variant);
char * SUMA_VO_orcode(SUMA_VolumeObject *VO);
char *SUMA_VE_orcode(SUMA_VolumeElement **VE, int ivo);
char * SUMA_VE_Headname(SUMA_VolumeElement **VE, int ivo);
SUMA_Boolean SUMA_VE_Set_Dims(SUMA_VolumeElement **VE, int ive);
float *SUMA_TDO_Points_Center(SUMA_TractDO *tdo, float *here);
float *SUMA_TDO_XYZ_Range(SUMA_TractDO *tdo, float *here);
float *SUMA_SDO_XYZ_Range(SUMA_SurfaceObject *so, float *here);
float *SUMA_CIFTI_DO_XYZ_Range(SUMA_CIFTI_DO *co, float *here);
float *SUMA_VO_XYZ_Range(SUMA_VolumeObject *VO, float *here);
float *SUMA_MDO_XYZ_Range(SUMA_MaskDO *MDO, float *here);
SUMA_SphereDO * SUMA_Alloc_SphereDO (int N_n, char *Label, char *parent_idcode, 
                                     SUMA_DO_Types type);
SUMA_TractDO * SUMA_Alloc_TractDO (char *Label, char *parent_idcode);
SUMA_PlaneDO * SUMA_Alloc_PlaneDO (int N_n, char *Label, SUMA_DO_Types type);
SUMA_Boolean SUMA_DrawGraphDO_G3D (SUMA_GraphLinkDO *gldo, 
                                   SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_BordFrac_to_GB(int BF, int *G, int *B);
SUMA_NIDO * SUMA_GDSET_matrix_nido(SUMA_DSET *dset);
SUMA_Boolean SUMA_GDSET_clear_matrix_nido(SUMA_DSET *dset, int clear_SO);
SUMA_Boolean SUMA_GDSET_refresh_matrix_nido(SUMA_DSET *dset, int also_SO);
SUMA_Boolean SUMA_DrawGraphDO_GMATRIX (SUMA_GraphLinkDO *gldo, 
                                       SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawGraphDO_GRELIEF (SUMA_GraphLinkDO *gldo, 
                                       SUMA_SurfaceViewer *sv);
SUMA_SurfaceObject *SUMA_Surface_Of_NIDO_Matrix(SUMA_NIDO *nido);
void SUMA_free_SphereDO (SUMA_SphereDO * SDO);
void SUMA_free_TractDO (SUMA_TractDO * SDO);
void SUMA_free_PlaneDO (SUMA_PlaneDO * SDO);
void SUMA_free_GraphLinkDO (SUMA_GraphLinkDO * GLDO);
SUMA_GraphLinkDO * SUMA_Alloc_GraphLinkDO (  char *variant, 
                                             SUMA_DSET *ParentGraph);
SUMA_GraphLinkDO *SUMA_find_Dset_GLDO(SUMA_DSET *dset, char *variant, 
                                      int *ifound);
SUMA_DSET *SUMA_find_GLDO_Dset(SUMA_GraphLinkDO *GLDO);
SUMA_Boolean SUMA_Remove_From_Pick_Colid_list(SUMA_SurfaceViewer *sv, 
                                              char *idcode_str);
DListElmt * SUMA_Find_In_Pick_Colid_list(SUMA_SurfaceViewer *sv, 
                                         char *idcode_str, char *primitive);
void *SUMA_Picked_reference_object(SUMA_COLID_OFFSET_DATUM *cod, 
                               SUMA_DO_Types *do_type);
int SUMA_Picked_DO_ID(SUMA_COLID_OFFSET_DATUM *cod);
SUMA_Boolean SUMA_CreateGraphDOs(SUMA_DSET *dset);
SUMA_Boolean SUMA_RegisterGraphDOs(SUMA_DSET *dset, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawGraphDO (SUMA_GraphLinkDO *gldo, SUMA_SurfaceViewer *sv,
                               char *variant);
SUMA_Boolean SUMA_DrawGraphLinkDO (SUMA_GraphLinkDO *GLDO, 
                                   SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_isGLDO_AnatCorrect(SUMA_GraphLinkDO *GLDO);
char *SUMA_iDO_state(int i);
char *SUMA_DO_state(SUMA_DO *DO);
char *SUMA_iDO_group(int i);
char *SUMA_DO_group(SUMA_DO *DO);
int SUMA_isDO_AnatCorrect(SUMA_DO *DO);
int  SUMA_is_iDO_AnatCorrect(int dov_id);
SUMA_Boolean SUMA_DrawSphereDO (SUMA_SphereDO *SDO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawPointDO (SUMA_SphereDO *SDO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawPlaneDO (SUMA_PlaneDO *SDO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawPlanes( float **PlEq, float **cen, float *sz, 
                              int N_pl, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_isROIdequal (SUMA_ROI_DATUM *ROId1, SUMA_ROI_DATUM *ROId2);
void SUMA_FreeROIDatum (void * data);
SUMA_ROI_DATUM * SUMA_AllocROIDatum (void);
SUMA_Boolean SUMA_PrependToROIdatum (SUMA_ROI_DATUM *ROId1, 
                                     SUMA_ROI_DATUM *ROId2);
void SUMA_ShowDrawnROI (SUMA_DRAWN_ROI *D_ROI, FILE *out, 
                        SUMA_Boolean ShortVersion);
void SUMA_ShowDrawnROIDatum (SUMA_ROI_DATUM *ROId, FILE *out, 
                             SUMA_Boolean ShortVersion);
SUMA_Boolean SUMA_AppendToROIdatum (SUMA_ROI_DATUM *ROId1,
                                    SUMA_ROI_DATUM *ROId2);
SUMA_ROI_DATUM * SUMA_FillToMask(SUMA_SurfaceObject *SO, int *ROI_Mask, 
                                 int FirstSurfNode);
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
SUMA_DRAWN_ROI * SUMA_1DROI_to_DrawnROI ( int *Node, int N_Node, int Value, 
                     char *Parent_idcode_str, 
                     char *Label, char *ColPlaneName, 
                     float *FillColor, float *EdgeColor, int EdgeThickness , 
                     SUMA_DO *dov, int N_dov, SUMA_Boolean ForDisplay);
SUMA_SegmentDO * SUMA_ReadNBVecDO (char *s, int oriented, char *parent_SO_id);
SUMA_SphereDO * SUMA_ReadNBSphDO (char *s, char *parent_SO_id);
SUMA_SegmentDO * SUMA_ReadDirDO (char *s, int oriented, char *parent_SO_id);
SUMA_TractDO *SUMA_ReadTractDO(char *s, char *parent_SO_id);
SUMA_TractDO *SUMA_Net2TractDO(TAYLOR_NETWORK *net, char *Label, 
                               char *parent_SO_id);
GLushort SUMA_int_to_stipplemask(int i); 
GLushort SUMA_int_to_stipplemask_cont(int i);
SUMA_SegmentDO * SUMA_ReadSegDO (char *s, int oriented, char *soid);
SUMA_SegmentDO * SUMA_ReadNBSegDO (char *s, int oriented, char *soid);
SUMA_SphereDO * SUMA_ReadSphDO (char *s);
SUMA_SphereDO * SUMA_ReadPntDO (char *s);
SUMA_PlaneDO * SUMA_ReadPlaneDO (char *s);
SUMA_MaskDO * SUMA_ReadMaskDO (char *s, char *parent_ADO_id);
SUMA_NIDO *SUMA_ReadNIDO(char *s, char *soid);
SUMA_NIDO *SUMA_BlankNIDO (char *idcode_str, char *Label, 
                           char *parent_so_id, char *coord_type,
                           char *font_name);
SUMA_SegmentDO *SUMA_CreateSegmentDO(  int N_n, int oriented, int NodeBased, 
                                       int Stipple,
                                       char *Label, char *idcode_str, 
                           char *Parent_idcode_str, SUMA_DO_Types Parent_do_type,
                                       char *DrawnDO_variant,
                                       float LineWidth, float *LineCol,
                                       int *NodeId, int *NodeId1,
                                       float *n0, float *n1,
                                       float *colv, float *thickv 
                                       );
SUMA_DO * SUMA_Multiply_NodeObjects ( SUMA_SurfaceObject *SO, 
                                      SUMA_DO *DO );
SUMA_NIDO ** SUMA_Multiply_NodeNIDOObjects ( SUMA_SurfaceObject *SO, 
                                      SUMA_DO *DO, int *NodeID, int N_Node);
SUMA_Boolean SUMA_ApplyDataToNodeObjects(
            SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *sv);
SUMA_SurfaceObject *SUMA_Cmap_To_SO (SUMA_COLOR_MAP *Cmap, float orig[3], 
                                       float topright[3], int verb);
SUMA_Boolean SUMA_PrepForNIDOnelPlacement (  SUMA_SurfaceViewer *sv,
                                             NI_element *nel, 
                                             SUMA_SurfaceObject *default_SO,
                                             int default_node,
                                             float *txloc, float *txcoord,
                                             int *sz, 
                                             int *orthoreset,
                                             SUMA_DO_CoordUnits coord_units,
                                             float *xyzoffset, int *jw);
SUMA_Boolean SUMA_DrawImageNIDOnel( NI_element *nel, 
                                    SUMA_SurfaceObject *SO,
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font, int default_node,
                                    SUMA_SurfaceViewer *sv) ;
SUMA_Boolean SUMA_DrawTextNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font, int default_node,
                                    SUMA_SurfaceViewer *sv) ;
SUMA_Boolean SUMA_DrawSphereNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, int default_node, 
                                    SUMA_SurfaceViewer *sv) ;
SUMA_Boolean SUMA_DrawNIDO (SUMA_NIDO *SDO, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_DrawLineAxis ( SUMA_AxisSegmentInfo *ASIp, SUMA_Axis *Ax, 
                                 SUMA_Boolean AddText);
DList *SUMA_SortedAxisSegmentList ( SUMA_SurfaceViewer *sv, SUMA_Axis *Ax, 
                                    SUMA_SORT_BOX_AXIS_OPTION opt);
void SUMA_WorldAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_AxisText(SUMA_AxisSegmentInfo *ASIp, double *Ps);
SUMA_Boolean SUMA_DrawText(char *txt, float *Ps);
void SUMA_ReportDrawnROIDatumLength(SUMA_SurfaceObject *SO, SUMA_ROI_DATUM *ROId, FILE *out, SUMA_WIDGET_INDEX_DRAWROI_WHATDIST option);
SUMA_SurfaceObject *SUMA_HJS_Surface(int ipart);
SUMA_SurfaceObject *SUMA_head_01_surface(void);
SUMA_SurfaceObject *SUMA_cube_surface(float sz, float *cen);
SUMA_SurfaceObject *SUMA_box_surface(float *sz3, float *cen, 
                                     float *col, int n_obj);
SUMA_SurfaceObject *SUMA_ball_surface(float *hd3, float *cen, float *col, 
                                     int n_obj);
NI_group *SUMA_SDO2niSDO(SUMA_SegmentDO *SDO);
SUMA_SegmentDO *SUMA_niSDO2SDO(NI_group *ngr); 
SUMA_Boolean SUMA_isSODimInitialized(SUMA_SurfaceObject *SO) ;
SUMA_Boolean SUMA_nixSODim(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_SetSODims(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_MinMaxNodesInROI (SUMA_DRAWN_ROI *D_ROI, 
                                    int MinMax[]);
SUMA_Boolean SUMA_TextBoxSize(char *txt, int *w, int *h, int *nl, void *font);
byte *SUMA_WordOverlapMask(int Nwidth, int Nheight, int N_n, char **names, 
                           void *fontGL, float *xyz, float maxoverlap, 
                           byte *usethesewords);
int SUMA_WordBoxSize (char **txt, int N_txt, int *w, void *font);
int SUMA_glutBitmapFontHeight(void *font) ;
int *SUMA_NIDOtext_LineWidth(char *string, void *font, int *N_lines);

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
const GLubyte *SUMA_StippleMask(int transp);
const GLubyte *SUMA_StippleMask_rand(int transp);
const GLubyte *SUMA_StippleMask_shift(int transp, int btp);
GLushort SUMA_StippleLineMask_rand(int transp, int chunkwidth, int rseed);
void SUMA_StippleMaskResest(void);
SUMA_GL_STEL *SUMA_FindStateTrackEl(char *state, DList *stu);
void SUMA_FreeStateTrackEl(void *stel);
SUMA_GL_STEL *SUMA_NewStateTrackEl(char *state, char *progenitor);
int SUMA_SetTrackElVal(SUMA_GL_STEL *stel, void *val, char *act);
char *SUMA_EnumToGLstate(int glpar);
int SUMA_GLstateToEnum(char *state);
int SUMA_GLStateTrack(char *action, DList **stu, char *progenitor,
                      char *state, void *val);
void SUMA_free_colid_offset_datum (void *vv);
GLubyte *SUMA_New_colid(SUMA_SurfaceViewer *sv, 
                char *Label, char *idcode_str, char *primitive, char *variant,
                char *reference_idcode_str, SUMA_DO_Types ref_do_type,
                int N_n);
GLubyte *SUMA_DO_get_pick_colid(SUMA_ALL_DO *DO, char *idcode_str,
                           char *primitive, char *variant,
                           char *ref_idcode_str,SUMA_DO_Types ref_do_type,
                                   SUMA_SurfaceViewer *sv);
NI_element *SUMA_GDSET_Edge_Bundle(SUMA_DSET *gset, SUMA_GRAPH_SAUX *GSaux, 
                                   int edge_id, int alt_edge_id);
NI_group *SUMA_MDO_to_NIMDO(SUMA_MaskDO *mdo, NI_group *cont);
SUMA_MaskDO *SUMA_NIMDO_to_MDO(NI_group *);
     
/*! Following DO_ macros are for setting states up for colorid picking
    and restoring states at exit.
    Note that some of these are overkill. They were added in a attempt
    to remedy what looked like antialiasing in the rendered image.
    It turned out the problem was caused by the visual not having an
    alpha size of 0. */
#define DO_PICK_VARS int pgllst=0, pgllsm=0, pgllight=0, pglblend=0, \
                         pgldith=0, pgltex2=0, pgltex3=0

#define DO_PICK_RESTORE {  \
   if (pgllst) glEnable(GL_LINE_STIPPLE);  \
   if (pgllsm) glEnable(GL_LINE_SMOOTH);   \
   if (pgllight) glEnable(GL_LIGHTING);    \
   if (pglblend) glEnable(GL_BLEND);       \
   if (pgldith)  glEnable(GL_DITHER);      \
   if (pgltex2)  glEnable(GL_TEXTURE_2D);  \
   if (pgltex3)  glEnable(GL_TEXTURE_3D);  \
}

#define DO_PICK_DISABLES {   \
   if ((pgllst = glIsEnabled(GL_LINE_STIPPLE))) glDisable(GL_LINE_STIPPLE); \
   if ((pgllsm = glIsEnabled(GL_LINE_SMOOTH)) ) glDisable(GL_LINE_SMOOTH);  \
   if ((pgllight = glIsEnabled(GL_LIGHTING))) glDisable(GL_LIGHTING);       \
   if ((pglblend = glIsEnabled(GL_BLEND))) glDisable(GL_BLEND);             \
   if ((pgldith = glIsEnabled(GL_DITHER))) glDisable(GL_DITHER);            \
   if ((pgltex2 = glIsEnabled(GL_TEXTURE_2D))) glDisable(GL_TEXTURE_2D);    \
   if ((pgltex3 = glIsEnabled(GL_TEXTURE_3D))) glDisable(GL_TEXTURE_3D);    \
}


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


