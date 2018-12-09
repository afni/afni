#ifndef SUMA_SVMANIP_INCLUDED
#define SUMA_SVMANIP_INCLUDED

#define UPDATE_NO_VIEWING_PARAMS_MASK (1<<1)
#define UPDATE_ROT_MASK (1<<2)
#define UPDATE_VIEW_POINT_MASK (1<<3)
#define UPDATE_EYE_AXIS_STD_MASK (1<<4)
#define UPDATE_STANDARD_VIEW_MASK (1<<5)
#define UPDATE_ALL_VIEWING_PARAMS_MASK ( UPDATE_ROT_MASK | UPDATE_VIEW_POINT_MASK | UPDATE_EYE_AXIS_STD_MASK | UPDATE_STANDARD_VIEW_MASK )

#define ROI_MODE(sv) ( SUMAg_CF->ROI_mode )
#define MASK_MANIP_MODE(sv) ( sv->MouseMode == SUMA_MASK_MANIP_MMODE )
typedef enum {
   SUMA_DEF_MMODE=0,
   SUMA_ROI_MMODE,
   SUMA_MASK_MANIP_MMODE,

   SUMA_N_MMODES
} SUMA_MOUSE_MODES;

/*! functions defined in SUMA_SVmanip.c */
SUMA_Boolean SUMA_LockEnum_LockType (SUMA_LINK_TYPES i, char *Name);
SUMA_Boolean SUMA_DiffGeomViewStruct(SUMA_GEOMVIEW_STRUCT gvs1,
                                     SUMA_GEOMVIEW_STRUCT gvs2,
                                     int level);
SUMA_Boolean SUMA_CopyGeomViewStruct(SUMA_GEOMVIEW_STRUCT *gvs1,
                                     SUMA_GEOMVIEW_STRUCT *gvs2);
SUMA_SurfaceViewer *SUMA_Alloc_SurfaceViewer_Struct (int N);
SUMA_PICK_RESULT *SUMA_Get_From_PickResult_List(SUMA_SurfaceViewer *sv,
                              SUMA_ALL_DO *ado, char *variant);
SUMA_Boolean SUMA_Add_To_PickResult_List(SUMA_SurfaceViewer *sv,
                         SUMA_ALL_DO *ado, char *variant, SUMA_PICK_RESULT **PR);
void SUMA_Show_PickList(DList *SelAdo, int detail, char *headstring, FILE *out);
char *SUMA_PickList_Info(DList *SelAdo, int detail);
SUMA_Boolean SUMA_Process_Selected_ADO(SUMA_SurfaceViewer *sv, int deepfirst);
void SUMA_Free_SelectedDO_Datum(void *data);
void SUMA_Show_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV, FILE *Out,
                                     int detail);
SUMA_Boolean SUMA_isViewerStateAnatomical(SUMA_SurfaceViewer *sv);
char *SUMA_SurfaceViewer_StructInfo (SUMA_SurfaceViewer *SV, int detail);
SUMA_Boolean SUMA_SetViewerLightsForSO(SUMA_SurfaceViewer *cSV,
                                       SUMA_SurfaceObject *SO);

SUMA_Boolean SUMA_UpdateRotaCenter (SUMA_SurfaceViewer *SV,
                                    SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_Apply_VisX_Chain(float *xyz, int N, DList *dl, int inv);
DListElmt *SUMA_Fetch_VisX_Element(char *label, DList *dl);
SUMA_VIS_XFORM_DATUM *SUMA_Fetch_VisX_Datum (char *label, DList *dl,
                                  SUMA_VISX_ADD_POSITIONS add, char *ref_label);
SUMA_Boolean SUMA_UpdateViewPoint (SUMA_SurfaceViewer *SV,
                                    SUMA_DO *dov, int N_dov, byte keepzoom);
SUMA_Boolean SUMA_UpdateViewPoint_RegisteredADO(SUMA_ALL_DO *ado, byte keepzoom);
SUMA_Boolean SUMA_SetGLHome(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_Free_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV);
SUMA_Boolean SUMA_Free_SurfaceViewer_Struct_Vect (SUMA_SurfaceViewer *SVv,
                                                  int N);
SUMA_Boolean SUMA_Free_ViewState (SUMA_ViewState *vs);
SUMA_ViewState *SUMA_Alloc_ViewState (int N);
SUMA_Boolean SUMA_New_ViewState (SUMA_SurfaceViewer *csv);
SUMA_Boolean SUMA_Free_ViewState_Hist (SUMA_ViewState_Hist *vsh);
SUMA_ViewState_Hist *SUMA_Alloc_ViewState_Hist (void);
SUMA_Boolean SUMA_Show_ViewState(SUMA_ViewState *VS, FILE *Out, int detail);
SUMA_Boolean SUMA_ViewState_MembsRefresh(SUMA_ViewState *VS);
SUMA_Boolean SUMA_AllViewState_MembsRefresh(void);
int *SUMA_ViewState_Membs(SUMA_ViewState *VS, SUMA_DO_Types *tt,
                          int *uN_MembSOs);
char *SUMA_ViewStateInfo(SUMA_ViewState *VS, int detail);
int SUMA_SV_GetShowSelectedDatum(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SV_SetShowSelectedDatum(SUMA_SurfaceViewer *sv,
                                          int act, int callback);
int SUMA_SV_GetShowSelectedFaceSet(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SV_SetShowSelectedFaceSet(SUMA_SurfaceViewer *sv,
                                          int act, int callback);
SUMA_Boolean SUMA_AdoptSurfGroup(SUMA_SurfaceViewer *csv,
                                 SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_RegisterSpecSO (SUMA_SurfSpecFile *Spec,
                                  SUMA_SurfaceViewer *csv,
                                  SUMA_DO* dov, int N_dov, int viewopt);
int SUMA_WhichState (char *state, SUMA_SurfaceViewer *csv, char *ForceGroup);
int SUMA_Which_iDO_State(int dov_id, SUMA_SurfaceViewer *cSV, int addifmissing);
SUMA_Boolean SUMA_Free_CommonFields (SUMA_CommonFields *cf);
SUMA_CommonFields * SUMA_Create_CommonFields (void);
void SUMA_Show_CommonFields (SUMA_CommonFields *cf, FILE *out);
char * SUMA_CommonFieldsInfo (SUMA_CommonFields *cf, int detail);
SUMA_STANDARD_VIEWS SUMA_BestStandardView (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_SetupSVforDOs (SUMA_SurfSpecFile *Spec, SUMA_DO *DOv,
                           int N_DOv, SUMA_SurfaceViewer *cSV, int viewopt);
SUMA_Boolean SUMA_ADO_FillColorList_Params(SUMA_ALL_DO *ADO,
                                     int *N_points, char **idcode);
SUMA_Boolean SUMA_FillColorList (SUMA_SurfaceViewer *sv, SUMA_ALL_DO *SO);
SUMA_Boolean SUMA_Free_ColorList (SUMA_COLORLIST_STRUCT *cl);
SUMA_Boolean SUMA_EmptyColorList (SUMA_SurfaceViewer *sv, char *DO_idstr);
GLfloat * SUMA_GetColorList (SUMA_SurfaceViewer *sv, char *DO_idstr);
GLfloat * SUMA_GetColorListPtr (SUMA_COLORLIST_STRUCT *cl);
SUMA_Boolean SUMA_SetRemixFlag (char *SO_idcode_str, SUMA_SurfaceViewer *SVv,
                                int N_SVv);
SUMA_COLORLIST_STRUCT * SUMA_GetColorListStruct (SUMA_SurfaceViewer *sv,
                                                 char *DO_idstr);
SUMA_Boolean SUMA_BlankColorListStruct(SUMA_COLORLIST_STRUCT *cl);
SUMA_Boolean SUMA_SetShownLocalRemixFlag (SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SetLocalRemixFlag(char *SO_idcode_str, SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SetAllRemixFlag (SUMA_SurfaceViewer *SVv, int N_SVv);
int SUMA_WhichSVg(SUMA_SurfaceViewer *sv);
int SUMA_WhichSV (SUMA_SurfaceViewer *sv, SUMA_SurfaceViewer *SVv, int N_SVv);
char SUMA_WhichSVc(SUMA_SurfaceViewer *sv, SUMA_SurfaceViewer *SVv, int N_SVv);
SUMA_X_SumaCont *SUMA_CreateSumaContStruct (void);
void *SUMA_FreeSumaContStruct (SUMA_X_SumaCont *SumaCont);
SUMA_X_ViewCont *SUMA_CreateViewContStruct (void);
void *SUMA_FreeViewContStruct (SUMA_X_ViewCont *ViewCont);
SUMA_X_SurfCont *SUMA_CreateSurfContStruct (char *idcode_str, SUMA_DO_Types dd);
void *SUMA_FreeSurfContStruct (SUMA_X_SurfCont *SurfCont);
SUMA_X_SurfCont *SUMA_GlobalMaskContStruct(char *idcode);
SUMA_MENU_WIDGET *SUMA_Free_Menu_Widget(SUMA_MENU_WIDGET *smw);
SUMA_MENU_WIDGET *SUMA_Alloc_Menu_Widget(int nw);
SUMA_rb_group *SUMA_CreateLock_rb (int N_rb_group, int N_but);
void * SUMA_FreeLock_rb (SUMA_rb_group *Lock_rb);
SUMA_X_DrawROI *SUMA_CreateDrawROIStruct (void);
void *SUMA_FreeDrawROIStruct (SUMA_X_DrawROI *DrawROI);
void SUMA_UpdateViewerTitle(SUMA_SurfaceViewer *sv);
void SUMA_UpdateAllViewerCursor(void);
void SUMA_UpdateViewerCursor(SUMA_SurfaceViewer *sv);
float SUMA_DimSclFac(char *units, char *specie);
int SUMA_WhichViewerInMomentum(SUMA_SurfaceViewer *SVv,
                               int N_SV, SUMA_SurfaceViewer *sv);
int SUMA_WhichGroup (SUMA_CommonFields *cf, char *nm);
SUMA_Boolean SUMA_RegisterGroup (SUMA_CommonFields *cf, char *gname);
SUMA_Boolean SUMA_RegisterSpecGroup (SUMA_CommonFields *cf,
                                     SUMA_SurfSpecFile *spec);
SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleGroupList (SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SwitchGroups (SUMA_SurfaceViewer *sv, char *group);
SUMA_Boolean SUMA_AdoptGroup(SUMA_SurfaceViewer *csv, char *group);
const char * SUMA_Clip_Type_to_Clip_Name (SUMA_CLIP_PLANE_TYPES tp);
char * SUMA_Show_Clip_Planes_Info (SUMA_CommonFields *cf);
void SUMA_Show_Clip_Planes (SUMA_CommonFields *cf, FILE *out);
float SUMA_sv_auto_fov(SUMA_SurfaceViewer *sv);
SUMA_SurfaceViewer *SUMA_OneViewerWithADORegistered(SUMA_ALL_DO *ADO);
SUMA_SurfaceViewer *SUMA_OneViewerWithADOVisible(SUMA_ALL_DO *ADO);
SUMA_SurfaceViewer *SUMA_OneViewerWithADOinFocus(SUMA_ALL_DO *ADO);
SUMA_SurfaceViewer *SUMA_OneViewerWithSOinFocus(
                              SUMA_SurfaceObject *curSO);
SUMA_SurfaceViewer *SUMA_OneViewerWithSOVisible(
                              SUMA_SurfaceObject *curSO);
SUMA_SurfaceViewer *SUMA_OneViewerWithSORegistered(
                              SUMA_SurfaceObject *curSO);
int SUMA_UpdateCrossHairNodeLabelFieldForDO(SUMA_ALL_DO *ado);
SUMA_SurfaceViewer *SUMA_BestViewerForADO(SUMA_ALL_DO *ado);
SUMA_PARSED_NAME *SUMA_SetAutoRecord(char *pref);
SUMA_SurfaceObject *SUMA_SV_Focus_SO(SUMA_SurfaceViewer *sv);
SUMA_SurfaceObject *SUMA_SV_Focus_any_SO(SUMA_SurfaceViewer *sv, int *dov_id);
SUMA_ALL_DO *SUMA_SV_Focus_ADO(SUMA_SurfaceViewer *sv);
SUMA_ALL_DO *SUMA_findanyFocusable_ADO(int *dov_id);
SUMA_ALL_DO *SUMA_SV_Focus_any_ADO(SUMA_SurfaceViewer *sv, int *dov_id);
SUMA_ALL_DO *SUMA_findany_ADO_WithSurfContWidget(int *dov_id,
                                                 SUMA_DO_Types thisdotp);
SUMA_ALL_DO *SUMA_SV_any_ADO_WithSurfContWidget(SUMA_SurfaceViewer *sv,
                                          int *dov_id, SUMA_DO_Types thisdotp);
SUMA_Boolean SUMA_SurfCont_SetcurDOp(SUMA_X_SurfCont *SurfCont,
                                     SUMA_ALL_DO *ado);
SUMA_ALL_DO *SUMA_SurfCont_GetcurDOp(SUMA_X_SurfCont *SurfCont);
SUMA_Boolean SUMA_SetMouseMode(SUMA_SurfaceViewer *sv,
                               SUMA_MOUSE_MODES mmode, void *val);
char *SUMA_ADO_ContName(SUMA_ALL_DO *ado);
int SUMA_SetObjectDisplayOrder(char *ord, int *otseq);
int SUMA_VerifyRenderOrder(char *ord, void *unused);


#endif
