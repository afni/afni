#ifndef SUMA_XCOLBAR_INCLUDED
#define SUMA_XCOLBAR_INCLUDED

#define SUMA_CMAP_WIDTH    20
#define SUMA_CMAP_HEIGHT   300
#define SUMA_SCALE_SLIDER_WIDTH 18
#define SUMA_SCALE_WIDTH 70
#define SUMA_SCALE_HEIGHT  SUMA_CMAP_HEIGHT   
#define SUMA_CMAP_ORIGIN   0.0,  0.0,     0.0
#define SUMA_CMAP_TOPLEFT  SUMA_CMAP_WIDTH, SUMA_CMAP_HEIGHT,   0.0
#define SUMA_CMAP_VIEW_FROM (2 * SUMA_CMAP_HEIGHT)
#define SUMA_CMAP_FOV_INITIAL 28.07249 /*!< 2 * atan((double)SUMA_CMAP_HEIGHT/2.0/(double)SUMA_CMAP_VIEW_FROM) * 180 * SUMA_PI , see labbook page 3 */
#if 0 /* the old one */
   #define SUMA_RANGE_STRING(m_nel, m_i, m_str_min, m_str_max, m_range){  \
      int m_loc[2];  \
      if (SUMA_GetColRange(m_nel, m_i, m_range, m_loc)) {   \
         sprintf(m_str_min, "%.2f %d", m_range[0], m_loc[0]);   \
         sprintf(m_str_max, "%.2f %d", m_range[1], m_loc[1]);   \
      } else { \
         sprintf(m_str_min, "??? ???");   \
         sprintf(m_str_max, "??? ???");   \
      }  \
   }
#else
   #define SUMA_RANGE_STRING(m_dset, m_i, m_str_min, m_str_max, m_str_minloc, m_str_maxloc, m_range){  \
      int m_loc[2];  \
      if (SUMA_GetDsetColRange(m_dset, m_i, m_range, m_loc)) {   \
         SUMA_SurfaceObject *m_SOp = NULL;   \
         char *m_idcode_str = NULL; \
         int m_N_Node=-1;  \
         /* number of nodes of parent surface */   \
         m_idcode_str = NI_get_attribute(m_dset->ngr, "MeshParent_idcode"); /* obsolete */\
         if (!m_idcode_str) m_idcode_str = NI_get_attribute(m_dset->ngr, "domain_parent_idcode"); \
         if (m_idcode_str) { \
            m_SOp = SUMA_findSOp_inDOv(m_idcode_str, SUMAg_DOv, SUMAg_N_DOv);  \
            if (m_SOp) {  \
               m_N_Node = m_SOp->N_Node;   \
            }  \
         }  \
         sprintf(m_str_min, "%s", MV_format_fval2(m_range[0], 7));   \
         sprintf(m_str_max, "%s", MV_format_fval2(m_range[1], 7));   \
         sprintf(m_str_minloc, "%d", SUMA_GetNodeIndex_FromNodeRow_s(m_dset, m_loc[0], m_N_Node));   \
         sprintf(m_str_maxloc, "%d", SUMA_GetNodeIndex_FromNodeRow_s(m_dset, m_loc[1], m_N_Node));   \
      } else { \
         sprintf(m_str_min, "???");   \
         sprintf(m_str_max, "???");   \
         sprintf(m_str_minloc, "???");   \
         sprintf(m_str_maxloc, "???");   \
      }  \
   }
#endif

#define SUMA_XHAIR_STRING(v, str)   {\
   sprintf(str,"%s, ", MV_format_fval2(v[0], 7)); \
   strcat(str, MV_format_fval2(v[1], 7)); \
   strcat(str, ", ");   \
   strcat(str,MV_format_fval2(v[2], 7)); \
}

#define SUMA_INSERT_CELL_STRING(TF, i, j, strng)   {  \
   if (TF->str_value) { \
      SUMA_STRING_REPLACE(TF->str_value[j*TF->Ni+i], strng);\
   }  \
   XtVaSetValues (TF->cells[j*TF->Ni+i], XmNvalue, strng, NULL);  \
}

/* modifies a cell's value and marks it as modified
No callback is made*/
#define SUMA_MODIFY_CELL_VALUE(TF, i, j, val)   {  \
   if (TF->type == SUMA_int || TF->type == SUMA_float) { \
      TF->cell_modified = j*TF->Ni+i;  \
      TF->num_value[TF->cell_modified] = val;  \
      SUMA_TableF_SetString(TF);\
   }  else {   \
      SUMA_SL_Err("Macro for numerical tables only"); \
   }  \
}

/* Like SUMA_MODIFY_CELL_VALUE, but cell_modified is reset to -1 */
#define SUMA_INSERT_CELL_VALUE(TF, i, j, val)   {  \
   if (TF->type == SUMA_int || TF->type == SUMA_float) { \
      SUMA_MODIFY_CELL_VALUE(TF, i, j, val); \
      TF->cell_modified = -1; \
   }  else {   \
      SUMA_SL_Err("Macro also for numerical tables only"); \
   }  \
}



/*!
   \brief retrieves the cell index using the cell's widget
*/
#define SUMA_WHICH_CELL(TF, w, Found)  {  \
   int m_nmx, m_i=0; \
   m_nmx = TF->Ni*TF->Nj;  \
   Found = -1; \
   while (m_i<m_nmx) {  \
      if (TF->cells[m_i] == w) { Found = m_i; m_i = m_nmx; }      \
      ++m_i;   \
   }  \
}


/* scale size gets messed up, see afni_widg.c and afni.h's
FIX_SCALE_SIZE*/
#define SUMA_FORCE_SCALE_HEIGHT(SurfCont) {\
  XtVaSetValues(  SurfCont->thr_sc, XmNheight,  SUMA_CMAP_HEIGHT-40, NULL ) ; \
}

#define SUMA_UPDATE_ALL_NODE_GUI_FIELDS(ado) {\
      SUMA_UpdateNodeNodeField(ado); \
      /* Now get the data values at that node */   \
      SUMA_UpdateNodeValField(ado);  \
      /* now find that node in the colored list */ \
      SUMA_UpdateNodeLblField(ado);  \
}    

/*! structure for holding table data for range setting */
typedef struct {
   SUMA_ALL_DO *ado;
   SUMA_OVERLAYS *colp;
} SUMA_SRV_DATA;

SUMA_Boolean SUMA_isTopColPlane(SUMA_OVERLAYS *cp, SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_isCurColPlane(SUMA_OVERLAYS *cp, SUMA_ALL_DO *ado);
SUMA_X_SurfCont *SUMA_ADO_Cont(SUMA_ALL_DO *ado);
SUMA_ALL_DO *SUMA_Cont_ADO(SUMA_X_SurfCont *SurfCont);
SUMA_SurfaceObject *SUMA_Cont_SO(SUMA_X_SurfCont *SurfCont);
SUMA_OVERLAYS * SUMA_ADO_CurColPlane(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_isADO_Cont_Realized(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_isADO_Cont_Created(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_isTopColPlane(SUMA_OVERLAYS *cp, SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_isCurColPlane(SUMA_OVERLAYS *cp, SUMA_ALL_DO *ado);
float *SUMA_ADO_DatumXYZ(SUMA_ALL_DO *ado, int isel, char *variant);
float *SUMA_GDSET_EdgeXYZ(SUMA_DSET *dset, int isel, char *variant, float *here);
SUMA_Boolean SUMA_GDSET_EdgeXYZ_eng(SUMA_DSET *dset, int isel, 
                                    char *variant, float *here);
SUMA_SurfaceObject *SUMA_GDSET_FrameSO(SUMA_DSET *dset);
SUMA_Boolean SUMA_GDSET_GMATRIX_Aff(SUMA_DSET *dset, double Aff[4][4], int I2X);
float *SUMA_GDSET_XYZ_Range(SUMA_DSET *dset,  char *variant, float *here);
float *SUMA_GDSET_XYZ_Center(SUMA_DSET *dset,  char *variant, float *here);
float *SUMA_GDSET_NodeXYZ(SUMA_DSET *dset, int node, char *variant, float *here);
SUMA_Boolean SUMA_GDSET_NodeXYZ_eng(SUMA_DSET *dset, int node, 
                                    char *variant, float *here);
char *SUMA_ADO_LDP(SUMA_ALL_DO *ado);
char * SUMA_ADO_Label(SUMA_ALL_DO *ado);
char * SUMA_ADO_idcode(SUMA_ALL_DO *ado);
char * SUMA_ADO_Parent_idcode(SUMA_ALL_DO *ado);
SUMA_GRAPH_SAUX *SUMA_ADO_GSaux(SUMA_ALL_DO *ado);
int SUMA_ADO_N_Datum(SUMA_ALL_DO *ado);
int SUMA_ADO_Max_Datum_Index(SUMA_ALL_DO *ado);
char * SUMA_ADO_variant(SUMA_ALL_DO *ado);
int SUMA_ADO_SelectedDatum(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_is_ADO_Datum_Primitive(SUMA_ALL_DO *ado,
                                          SUMA_COLID_OFFSET_DATUM *codf);
SUMA_Boolean SUMA_ADO_Set_SelectedDatum(SUMA_ALL_DO *ado, int sel);
int SUMA_ADO_N_Overlays(SUMA_ALL_DO *ado);
SUMA_OVERLAYS * SUMA_ADO_Overlay0(SUMA_ALL_DO *ado);
SUMA_OVERLAYS * SUMA_ADO_Overlay(SUMA_ALL_DO *ado, int i);
SUMA_OVERLAYS * SUMA_ADO_CurColPlane(SUMA_ALL_DO *ado);
void SUMA_ShowMeTheChildren(Widget w);
void SUMA_UnmanageTheChildren(Widget w);
void SUMA_ManageTheChildren(Widget w);
void SUMA_DoForTheChildren(Widget w, int i, int lvl, int rec);
Widget SUMA_FindChildWidgetNamed(Widget w, char *name);
XImage *SUMA_cmap_to_XImage (Widget wid, SUMA_COLOR_MAP *cm);
void SUMA_DrawCmap(SUMA_COLOR_MAP *Cmap);
void SUMA_cmap_wid_display(SUMA_ALL_DO *ado);
void SUMA_cmap_context_Init(SUMA_ALL_DO *ado);
void SUMA_cmap_wid_graphicsInit (Widget w, XtPointer clientData, XtPointer call);
Boolean SUMA_cmap_wid_handleRedisplay(XtPointer clientData);
void SUMA_cmap_wid_postRedisplay(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cmap_wid_expose(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cmap_wid_resize(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cmap_wid_input(Widget w, XtPointer clientData, XtPointer call);
unsigned char *SUMA_read_ppm(char *fname, int *width, int *height, int verb);
void SUMA_CreateCmapWidgets(Widget parent, SUMA_ALL_DO *ado);
void SUMA_cb_ColMap_Switch(Widget w, XtPointer clientData, XtPointer call);
int SUMA_SwitchCmap(SUMA_ALL_DO *ado, SUMA_COLOR_MAP *CM, int setmenu);
int SUMA_SwitchCmap_one(SUMA_ALL_DO *ado, SUMA_COLOR_MAP *CM, int setmenu);
int SUMA_SelectSwitchCmap_one( SUMA_ALL_DO *ado, SUMA_LIST_WIDGET *LW,
                               int ichoice, SUMA_Boolean CloseShop, int setmen);
int SUMA_SelectSwitchCmap( SUMA_ALL_DO *ado, SUMA_LIST_WIDGET *LW,
                           int ichoice, SUMA_Boolean CloseShop, int setmen);
int SUMA_SwitchColPlaneBrightness_one(
         SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
         int ind, int setmen);
int SUMA_SwitchColPlaneBrightness(
         SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
         int ind, int setmen);
void SUMA_cb_SwitchBrightness(Widget w, XtPointer clientData, XtPointer call);
int SUMA_SwitchColPlaneThreshold(
         SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
         int ind, int setmen);
int SUMA_SwitchColPlaneThreshold_one(
         SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
         int ind, int setmen);
void SUMA_cb_SwitchThreshold(Widget w, XtPointer clientData, XtPointer call);
int SUMA_SwitchColPlaneIntensity(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                                 int ind, int setmen);
int SUMA_SwitchColPlaneIntensity_one (
         SUMA_ALL_DO *ado, 
         SUMA_OVERLAYS *colp, 
         int ind, int setmen);
void SUMA_cb_SwitchIntensity(Widget w, XtPointer clientData, XtPointer call);
SUMA_MenuItem *SUMA_FreeMenuVector(SUMA_MenuItem *menu, int Nels);
SUMA_MenuItem *SUMA_FormSwitchColMenuVector(SUMA_ALL_DO *ado, 
                                            int what, int *N_items);
void SUMA_set_cmap_options(SUMA_ALL_DO *ado, 
                           SUMA_Boolean NewDset, SUMA_Boolean NewMap);
void SUMA_set_cmap_options_SO(SUMA_ALL_DO *ado, SUMA_Boolean NewDset,
                           SUMA_Boolean NewMap);

void SUMA_cb_SwitchCmap(Widget w, XtPointer client_data, XtPointer call);
SUMA_MenuItem *SUMA_FormSwitchCmapMenuVector(SUMA_COLOR_MAP **CMv, int N_maps);
void SUMA_cb_SelectSwitchCmap (Widget w, XtPointer client_data, XtPointer call);
void SUMA_cb_CloseSwitchCmap (Widget w, XtPointer client_data, XtPointer call);
SUMA_Boolean SUMA_CmapSelectList(SUMA_ALL_DO *ado, int type, int bringup);
SUMA_Boolean SUMA_SwitchColPlaneCmap(SUMA_ALL_DO *ado, SUMA_COLOR_MAP *CM);
SUMA_Boolean SUMA_SetCmapMenuChoice(SUMA_ALL_DO *ado, char *str);
int SUMA_GetListIchoice(XmListCallbackStruct *cbs, 
                        SUMA_LIST_WIDGET *LW,
                        SUMA_Boolean *CloseShop);
void SUMA_cb_SelectSwitchInt (
         Widget w, XtPointer client_data, 
         XtPointer call_data);
void SUMA_cb_SelectSwitchThr (
         Widget w, XtPointer client_data, 
         XtPointer call_data);
void SUMA_cb_SelectSwitchBrt (
         Widget w, XtPointer client_data, 
         XtPointer call_data);
int SUMA_SelectSwitchDsetCol(
         SUMA_ALL_DO *ado, 
         SUMA_LIST_WIDGET *LW, 
         int block,
         int ichoice);
void SUMA_cb_CloseSwitchLst (Widget w, XtPointer client_data, XtPointer call);
void SUMA_SetScaleRange(SUMA_ALL_DO *ado, double range[2]);  
int SUMA_set_threshold_one(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                           float *val);
int SUMA_set_threshold(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                           float *val);
void SUMA_cb_set_threshold(Widget w, XtPointer clientData, XtPointer call);
int SUMA_set_threshold_label(SUMA_ALL_DO *ado, float val);
void SUMA_optmenu_EV( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch );
void SUMA_cb_SetCoordBias(Widget widget, XtPointer client_data, 
                          XtPointer call_data);
SUMA_Boolean SUMA_RedisplayAllShowing(char *SO_idcode_str, 
                                      SUMA_SurfaceViewer *SVv, int N_SVv);
void SUMA_CreateTable(  Widget parent,
            int Ni, int Nj, 
            char **row_tit, char **col_tit, 
            char **row_hint, char **col_hint,
            char **row_help, char **col_help, 
            int *cwidth, SUMA_Boolean editable, SUMA_VARTYPE type, 
            void (*NewValueCallback)(void * data), void *cb_data,
            void (*TitLabelEVHandler)(Widget w , XtPointer cd , 
                                      XEvent *ev , Boolean *ctd), 
            void *TitLabelEVHandlerData,
            void (*CellEVHandler)(Widget w , XtPointer cd , 
                                  XEvent *ev , Boolean *ctd), 
            void *CellEVHandlerData,
                        SUMA_TABLE_FIELD *TF);
void SUMA_TableF_cb_label_Modify (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_TableF_SetString (SUMA_TABLE_FIELD * AF);
void SUMA_TableF_cb_label_change (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_leave_TableField( Widget w , XtPointer client_data ,
                           XEvent * ev , Boolean * continue_to_dispatch );
#if 0 /* Kill after a while. ZSS July 2012 */
void SUMA_SetRangeValueOld (void *data);
void SUMA_SetRangeValueOld_one (void *data);
#endif
int SUMA_SetRangeValueNew(SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset);
int SUMA_SetRangeValueNew_one(SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset);
void SUMA_cb_SetRangeValue (void *data);
int SUMA_SetClustValue(SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset);
int SUMA_SetClustValue_one(SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset);
void SUMA_cb_SetClustValue (void *data);
SUMA_Boolean SUMA_SetClustTableTit_one (SUMA_ALL_DO *ado, 
                        SUMA_OVERLAYS *colp, int i, int j, int Button); 
SUMA_Boolean SUMA_SetClustTableTit (SUMA_ALL_DO *ado, 
                        SUMA_OVERLAYS *colp, int i, int j, int Button);
void SUMA_SetClustTableTit_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch ); 
SUMA_Boolean SUMA_SetTableTitleButton1(SUMA_TABLE_FIELD *TF, int i, int j, 
                                       byte flag);
SUMA_TABLE_FIELD * SUMA_AllocTableField(void);
SUMA_TABLE_FIELD * SUMA_FreeTableField(SUMA_TABLE_FIELD *TF);
SUMA_CELL_VARIETY SUMA_cellvariety (SUMA_TABLE_FIELD *TF, int n);
SUMA_Boolean SUMA_InitRangeTable(SUMA_ALL_DO *ado, int what);
SUMA_Boolean SUMA_InitClustTable(SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets(Widget parent, SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets_SO(Widget parent, SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets_GLDO(Widget parent, SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_UpdateXhairField(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_UpdateCrossHairNodeLabelField(SUMA_SurfaceViewer *sv);
void SUMA_XhairInput (void* data);
void SUMA_NodeInput (void* data);
void SUMA_GNodeInput (void *data);
void  SUMA_SetCellEditMode(SUMA_TABLE_FIELD *TF, int i, int j, int Mode);
void SUMA_TriInput (void* data);
SUMA_Boolean SUMA_UpdateTriField(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_UpdateNodeLblField(SUMA_ALL_DO *ADO);
SUMA_Boolean SUMA_UpdateNodeLblField_ADO(SUMA_ALL_DO *ADO);
char **SUMA_FormNodeValFieldStrings(SUMA_ALL_DO *ado, 
                                 SUMA_DSET *dset, int Node,
                                 int find, int tind, int bind, int dec);
SUMA_Boolean SUMA_UpdateNodeValField(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_UpdateNodeNodeField(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_Init_SurfCont_CrossHair(SUMA_ALL_DO *ado);
void SUMA_cb_AbsThresh_tb_toggled (Widget w, XtPointer data, 
                                    XtPointer client_data);
void SUMA_cb_SymIrange_tb_toggled (Widget w, XtPointer data, 
                                    XtPointer client_data);
void SUMA_cb_ShowZero_tb_toggled (Widget w, XtPointer data, 
                                    XtPointer client_data);
void SUMA_cb_SetCmapMode(Widget widget, XtPointer client_data, 
                                    XtPointer call_data);
SUMA_Boolean SUMA_SetCmapMode(SUMA_ALL_DO *ado, int imenu);
void SUMA_cb_SetLinkMode(Widget widget, XtPointer client_data, 
                                    XtPointer call_data);
void SUMA_set_cmap_options_GLDO(SUMA_ALL_DO *ado, SUMA_Boolean NewDset,
                                SUMA_Boolean NewMap);
void SUMA_cb_Cmap_Load(Widget w, XtPointer data, XtPointer client_data);
SUMA_COLOR_MAP *SUMA_LoadCmapFile_eng(char *filename);
void SUMA_LoadCmapFile (char *filename, void *data);
SUMA_Boolean  SUMA_Insert_Cmap_of_Dset(SUMA_DSET *dset);
void SUMA_CreateUpdatableCmapMenu(SUMA_ALL_DO *ado);
int SUMA_ThreshVal2ScalePos(SUMA_ALL_DO *ado, float *val);
void SUMA_cb_SetScaleThr(void *data);
int SUMA_SetScaleThr_one(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                          float *val, int setmen, int redisplay);
int SUMA_SetScaleThr(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                          float *val, int setmen, int redisplay);
SUMA_Boolean SUMA_DsetColSelectList(
         SUMA_ALL_DO *ado, int type, 
         int refresh, int bringup);
SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleDsetColList(SUMA_DSET *dset); 
void SUMA_UpdatePvalueField (SUMA_ALL_DO *ado, float thresh);
double SUMA_Pval2ThreshVal (SUMA_ALL_DO *ado, double pval);
SUMA_Boolean SUMA_UpdatePointField(SUMA_ALL_DO*ado);
SUMA_Boolean SUMA_UpdateNodeField(SUMA_ALL_DO *ado);
char *SUMA_GetLabelsAtNode(SUMA_ALL_DO *ado, int node);
char *SUMA_GetLabelsAtNode_ADO(SUMA_ALL_DO *ado, int node);
SUMA_Boolean SUMA_SetCmodeMenuChoice(SUMA_ALL_DO *ado, char *str);
SUMA_NIDO *SUMA_NodeLabelToTextNIDO (char *lbls, SUMA_ALL_DO *ado, 
                                     SUMA_SurfaceViewer *sv);
/* the help strings */

/* Surface Properties Block */
#define  SUMA_SurfContHelp_more  \
   "Opens a dialog with detailed\n" \
   "information about the surface\n"\
   "object."

#define  SUMA_SurfContHelp_RenderMode  \
   "Choose the rendering mode for this surface.\n" \
   "   Viewer: Surface's rendering mode is set\n"  \
   "           by the viewer's setting which can\n"   \
   "           be changed with the 'p' option.\n"  \
   "   Fill:   Shaded rendering mode.\n"  \
   "   Line:   Mesh rendering mode.\n"    \
   "   Points: Points rendering mode."   

#define  SUMA_SurfContHelp_TransMode  \
   "Choose the transparency for this surface.\n" \
   "   Viewer: Surface's transparency is set\n"  \
   "           by the viewer's setting which can\n"   \
   "           be changed with the 'o', 'O' options.\n"  \
   "   0 :   No transparency, opaque.\n"  \
   "   ...\n"    \
   "   16:   Maximum transparency, invisibile"   

#define  SUMA_SurfContHelp_DsetViewMode  \
   "Choose the viewing mode for this dataset.\n" \
   "   Col: Colours, only.\n"  \
   "   Con: Contours (slower), only.\n"  \
   "   C&C: Colours and Contours (slower), only.\n"    \
   "   XXX: Unfortunately nothing, only.\n"  \
   " There is one contour created for each color \n"  \
   " in the colormap. You'd want to use colormaps with \n"  \
   " few colors to get a contour of use.\n" \
   " Contours are not created if colormap has panes\n"   \
   " of unequal sizes.\n"   

#define SUMA_SurfContHelp_Dsets  \
   "Show/Hide Dataset (previously Color Plane) controllers"

#define SUMA_SurfContHelp_Xhr \
   "Crosshair coordinates on\n"   \
   "this controller's surface.\n"   \
   "Entering new coordinates \n"   \
   "makes the crosshair jump\n"   \
   "to that location (like 'ctrl+j').\n"   \
   "Use 'alt+l' to center the\n"   \
   "cross hair in your viewer."    

#define SUMA_SurfContHelp_Node   \
   "Index of node in\n"   \
   "focus on this controller's\n"   \
   "surface. Nodes in focus are\n"   \
   "highlighted by the blue sphere\n"   \
   "in the crosshair.\n"   \
   "Entering a new node's index\n"   \
   "will put that node in focus\n"   \
   "and send the crosshair to its\n"   \
   "location (like 'j').\n"   \
   "Use 'alt+l' to center the\n"   \
   "cross hair in your viewer."

 #define SUMA_SurfContHelp_GNode   \
   "FILL ME"
        
#define SUMA_SurfContHelp_Tri   \
   "1- Triangle (faceset) index of\n"   \
   "triangle in focus on this \n"   \
   "on this controller's surface.\n"   \
   "Triangle in focus is highlighted\n"   \
   "in gray. Entering a new triangle's\n"   \
   "index will set a new triangle in\n"   \
   "focus (like 'J').\n"   \
   "2- Nodes forming triangle."

#define SUMA_SurfContHelp_GEdge \
   "FILL ME"
   
#define SUMA_SurfContHelp_NodeValTblr0 \
   "Data Values at node in focus"
#define SUMA_SurfContHelp_GEdgeValTblr0 \
   "Data Values at edge in focus"
   
#define SUMA_SurfContHelp_NodeValTblc0 \
   "Data Values at node in focus"
#define SUMA_SurfContHelp_GEdgeValTblc0 \
   "Data Values at edge in focus"
   
#define SUMA_SurfContHelp_NodeValTblc1 \
   "Intensity (I) value"
#define    SUMA_SurfContHelp_GEdgeValTblc1\
   "Intensity (I) value" 

#define SUMA_SurfContHelp_NodeValTblc2 \
   "Threshold (T) value"
#define SUMA_SurfContHelp_GEdgeValTblc2\
   "Threshold (T) value"

#define SUMA_SurfContHelp_NodeValTblc3 \
   "Brightness modulation (B) value"
#define SUMA_SurfContHelp_GEdgeValTblc3 \
   "Brightness modulation (B) value"

#define SUMA_SurfContHelp_NodeLabelTblr0 \
   "Labels available the selected Dset\n" \
   "at the node in focus.\n"   \
   "If nothing is available, node color\n"   \
   "is displayed."

#define SUMA_SurfContHelp_GEdgeLabelTblr0\
   "Color from the selected Dset\n" \
   "at the edge in focus.\n"   \
   "If nothing is available, node color\n"   \
   "is displayed."
   
#define SUMA_SurfContHelp_DsetLblTblr0 \
  "Label of Dset." 
  
#define SUMA_SurfContHelp_DsetLblTblr1 \
   "Parent surface of Dset."

#define SUMA_SurfContHelp_DsetOrd \
   "Order of Dset's colorplane.\n"  \
   "Dset with highest number is \n"   \
   "on top of the stack. Separate \n"  \
   "stacks exits for foreground (fg:)\n" \
   "and background planes (bg:)."
   
#define SUMA_SurfContHelp_ArrowFieldMenu \
   "For datasets with sub-bricks exceeding what you have\n" \
   "set in environment variable SUMA_ArrowFieldSelectorTrigger\n"\
   "the menu selection switches to this format."
   
#define SUMA_SurfContHelp_DsetOpa \
   "Opacity of Dset's colorplane.\n"  \
   "Opaque planes have an opacity\n"   \
   "of 1, transparent planes have\n"  \
   "an opacity of 0. \n"   \
   "Opacities are used when mixing\n" \
   "planes within the same group  \n"  \
   "foreground (fg:) or background(bg:).\n"   \
   "\n"  \
   "Opacity values are not applied\n"  \
   "to the first plane in a group.\n"   \
   "Consequently, if you have just\n"   \
   "one plane to work with, opacity \n"   \
   "value is meaningless.\n"  \
   "\n"  \
   "Color mixing can be done in two \n"  \
   "ways, use F7 to toggle between \n" \
   "mixing modes.\n"
   
#define SUMA_SurfContHelp_DsetDim  \
   "Dimming factor to apply to colormap\n" \
   "before mapping the intensity (I) data.\n" \
   "The colormap, if displayed on the right,\n"   \
   "is not visibly affected by Dim but the\n"   \
   "colors mapped onto the surface are.\n"   \
   "For RGB Dsets (.col files), Dim is\n" \
   "applied to the RGB colors directly"
   
#define SUMA_SurfContHelp_DsetView  \
   "View (ON)/Hide Dset node colors."

#define SUMA_SurfContHelp_DsetViewOne  \
   "If ON, view only the selected\n"\
   "Dset's colors. No mixing of colors in the\n"\
   "foreground stack is done.\n"   \
   "\n"  \
   "If OFF, then mix the color planes\n"  \
   "in the foreground stack.\n"  \
   "\n"  \
   "This option makes it easy to view \n" \
   "one Dset's colors at a time without\n"   \
   "having to worry about color mixing,\n"   \
   "opacity, and stacking order.\n" \
   "\n"  \
   "Needless to say, options such as\n" \
   "'Ord:' and 'Opa:' in this panel are \n" \
   "of little use when this button is ON."

#define SUMA_SurfContHelp_DsetSwitch   \
   "Switch between datasets."

#define SUMA_SurfContHelp_SetThreshTblr0   \
   "Set the threshold.\n"  \
   "For statistical parameters, you can \n"  \
   "append a 'p' to set by the p value.\n" \
   "For example 0.001p\n"
   
#define SUMA_SurfContHelp_DsetLoad  \
   "Load a new dataset (Dset).\n"   \
   "Datasets can be of 2 formats:\n"   \
   "1- NIML (.niml.dset)\n"   \
   "     This format is internal \n"   \
   "     to AFNI/SUMA. \n"   \
   "2- 1D   (.1D.dset)\n"   \
   "     Simple ASCII tabular format\n"   \
   "     supporting numerical values\n"   \
   "     only.\n"   \
   "     Each row i contains Nj data\n"   \
   "     values per node.\n"   \
   "     Since this format has no header\n"   \
   "     associated with it, it makes\n"   \
   "     some assumption about the data\n"   \
   "     in the columns. \n"   \
   "   You can choose from 3 options:\n"   \
   "     (see below for nomenclature)\n"   \
   "   - Each column has Ni values where\n"   \
   "     Ni = N_Node \n"   \
   "     In this case, it is assumed that\n"   \
   "     row i has values for node i on\n"   \
   "     the surface.\n"   \
   "   - If Ni is not equal to N_Node then\n"   \
   "     SUMA will check to see if column 0\n"   \
   "     (Col_0) is all integers with values\n"   \
   "     v satisfying:  0 <= v < N_Node .\n"   \
   "     If that is the case then column 0\n"   \
   "     contains the node indices. The values\n"   \
   "     in row j of Dset are for the node\n"   \
   "     indexed Col_0[j].\n"   \
   "     In the Sample 1D Dset shown below\n"   \
   "     assuming N_Node > 58, SUMA\n"   \
   "     will consider the 1st column to \n"   \
   "     contain node indices. In that case\n"   \
   "     the values -12.1 and 0.9 are for \n"   \
   "     node 58 on the surface.\n"   \
   "   - Lastly, if Col_0 fails the node index\n"   \
   "     test, then SUMA considers the data\n"   \
   "     in row i to be associated with node i.\n"   \
   "\n"   \
   "   If you're confused, try creating some\n"   \
   "   toy datasets like the one below and \n"   \
   "   load them into SUMA.\n"   \
   "\n"   \
   "   Sample 1D Dset (Call it pickle.1D.dset):\n"   \
   "     25    22.7     1.2   \n"   \
   "     58    -12.1    0.9   \n"   \
   "\n"   \
   "   Nomenclature and conventions:\n"   \
   "     - N_Node is the number of nodes\n"   \
   "       forming the surface.\n"   \
   "     - Indexing always starts at 0.\n"   \
   "       In the example, value v at \n"   \
   "       row 0, column 1 is v = 22.7 .\n"   \
   "     - A Dset has Ni rows and Nj columns.\n"   \
   "       In other terms, Ni is the number\n"   \
   "       of values per node and Nj is the\n"   \
   "       number of nodes for which data are\n"   \
   "       specified in Dset.\n"   \
   "       Ni = 2, Nj = 3 in the example."
   
#define SUMA_SurfContHelp_DsetLoadCol  \
   "Load a new color plane.\n"   \
   "A color plane is a 1D text file with \n" \
   "each row formatted as such:"  \
   "  n  r g b\n" \
   "where n is the node index, \n"  \
   "r, g, and b are the red, green and blue\n"  \
   "color values, respectively. \n"  \
   "Color values must be between 0 and 1.0. \n" \
   "A sample file would be: test.1D.col\n"   \
   "   0    0.1 0.2 1   \n"   \
   "   1    0   1   0.8 \n"   \
   "   4    1   1   1   \n"   \
   "   7    1   0   1   \n"   \
   "   14   0.7 0.3 0   "


#define SUMA_SurfContHelp_SelInt \
   "Select Intensity (I) column.\n"   \
   "Use this menu to select\n"   \
   "which column in the\n"   \
   "dataset (Dset) should be \n"   \
   "used for an Intensity (I)\n"   \
   "measure.\n"   \
   "\n"   \
   "Right click on 'I' to get a \n" \
   "list widget, which is better \n" \
   "when you have many columns \n"   \
   "from which to choose.\n"\
   "\n"  \
   "I values are the ones that \n"   \
   "get colored by the colormap.\n" \
   "\n"   \
   "No coloring is done if the\n"   \
   "'v' button on the right is\n"   \
   "turned off.\n"   \
   "\n"   \
   "I value for the selected node\n"   \
   "is shown in the 'Val' table\n"   \
   "of the 'Xhair Info' section \n"   \
   "on the left." 

#define SUMA_SurfContHelp_SelThr \
   "Select Threshold (T) column.\n"   \
   "Use this menu to select\n"   \
   "which column in the\n"   \
   "dataset (Dset) should be \n"   \
   "used for a Threshold (T)\n"   \
   "measure.\n"   \
   "\n"   \
   "Right click on 'T' to get a \n" \
   "list widget, which is better \n" \
   "when you have many columns \n"   \
   "from which to choose.\n"\
   "\n"  \
   "T values are the ones used \n"   \
   "to determine if a node \n"   \
   "gets colored based on its\n"   \
   "I value.\n"   \
   "\n"   \
   "A node n is not colored if:\n"   \
   "    T(n)   < Tscale   \n"   \
   "or if '|T|' option below\n"   \
   "is turned ON.\n"   \
   "  | T(n) | < Tscale .\n"  \
   "\n"   \
   "Thresholding is not applied\n"   \
   "when the 'v' button on the \n"   \
   "right is turned off.\n"   \
   "\n"   \
   "T(n) for the selected node n\n"   \
   "is shown in the 'Val'\n"   \
   "table of the 'Xhair Info'\n"   \
   "section on the left."   

#define SUMA_SurfContHelp_SelBrt \
   "Select Brightness (B) column.\n"   \
   "Use this menu to select\n"   \
   "which column in the\n"   \
   "dataset (Dset) should be \n"   \
   "used for color Brightness (B)\n"   \
   "modulation.\n"   \
   "\n"   \
   "Right click on 'B' to get a \n" \
   "list widget which is better \n" \
   "when you have many columns \n"   \
   "from which to choose.\n"\
   "\n"  \
   "B values are the ones used \n"   \
   "to control the brightness of\n"   \
   "a node's color.\n"  \
   "\n"   \
   "Brightness modulation is\n"   \
   "controlled by ranges in the\n"   \
   "'B' cells of the table below.\n"   \
   "\n"   \
   "Brightness modulation is not\n"   \
   "applied when the 'v' button on \n"   \
   "the right is turned off.\n"  \
   "\n"   \
   "B(n) for the selected node n\n"   \
   "is shown in the 'Val'\n"   \
   "table of the 'Xhair Info'\n"   \
   "section on the left."

#define SUMA_SurfContHelp_SelIntTgl \
   "View (ON)/Hide Dset node colors."

#define SUMA_SurfContHelp_SelThrTgl \
   "Apply (ON)/Ignore thresholding"
   
#define SUMA_SurfContHelp_SelBrtTgl \
   "View (ON)/Ignore brightness modulation"

#define SUMA_SurfContHelp_SetRngTbl_r0 \
   "Used for setting the clipping ranges."   \
   "Clipping is only done for \n"   \
   "color mapping. Actual data \n"   \
   "values do not change."

#define SUMA_SurfContHelp_SetRngTbl_r1 \
   "Intensity clipping range.\n" \
   "Values in the intensity data \n"   \
   "that are less than Min are colored\n"   \
   "by the first (bottom) color of the \n"   \
   "colormap. \n"   \
   "Values larger than Max are mapped \n"   \
   "to the top color.\n"   \
   "\n"   \
   "Left click locks ranges\n"   \
   "from automatic resetting.\n"   \
   "Locked range is applied to\n"   \
   "current Dset only.\n"   \
   "\n"   \
   "Right click resets values\n"   \
   "to full range in data."  

#define SUMA_SurfContHelp_SetRngTbl_r2 \
   "Brightness modulation clipping range.\n"   \
   "Values in the brightness data are\n"   \
   "clipped to the Min to Max range before\n"   \
   "calculating their modulation factor\n"   \
   "(see next table row).\n"   \
   "\n"   \
   "Left click locks ranges\n"   \
   "from automatic resetting.\n"   \
   "Locked range is applied to\n"   \
   "current Dset only.\n"  \
   "\n"   \
   "Right click resets values\n"   \
   "to full range in data."   

#define SUMA_SurfContHelp_SetRngTbl_r3 \
   "Brightness modulation factor range.\n"   \
   "Brightness modulation values, after\n"   \
   "clipping per the values in the row above,\n"   \
   "are scaled to fit the range specified\n"   \
   "here."
   
#define SUMA_SurfContHelp_SetRngTbl_r4 \
   "Coordinate bias range.\n"   \
   "Coordinates of nodes that are mapped\n"   \
   "to the colormap can have a bias added\n"   \
   "to their coordinates. \n"   \
   "\n"   \
   "Nodes mapped to the first color of \n"   \
   "the map receive the minimum bias and\n"   \
   "nodes mapped to the last color receive\n"   \
   "the maximum bias. \n"   \
   "\n"   \
   "Nodes not colored, because of \n"   \
   "thresholding for example, will \n"   \
   "have no bias applied."

#define SUMA_SurfContHelp_SetRngTbl_c1 \
   "Minimum clip value.\n" \
   "Clips values (v) in the Dset\n" \
   "less than Minimum (min):\n"  \
   "  if v < min then v = min "

#define SUMA_SurfContHelp_SetRngTbl_c2 \
   "Maximum clip value.\n" \
   "Clips values (v) in the Dset\n" \
   "larger than Maximum (max):\n"  \
   "  if v > max then v = max "

#define SUMA_SurfContHelp_SetClustTbl_r0 \
   "Used for setting the clustering parameters."  

#define SUMA_SurfContHelp_SetClustTbl_r1 \
   "Clusterizing.\n" \
   "\n"   \
   "Left click toggles clusterizing ON/OFF"   
   
#define SUMA_SurfContHelp_SetClustTbl_c1 \
   "Minimum distance between nodes.\n" \
   "Nodes closer than the minimum distance are in\n"\
   "same cluster. If you want to distance to be in\n"\
   "number of edges (N) separating nodes, set the minimum\n"\
   "distance to -N. This parameter is the same as -rmm in\n"\
   "the program SurfClust"

#define SUMA_SurfContHelp_SetClustTbl_c2 \
   "Minimum cluster area\n" \
   "A cluster whose area is less than the specified minimum\n"\
   "will not be displayed. Instead of areas, you can specify\n"\
   "that clusters less than K nodes be masked by setting\n"\
   "the Minimum cluster area to -K\n"  \
   "This parameter covers options -amm2 and -n in\n"\
   "the program SurfClust"
   
#define SUMA_SurfContHelp_Col \
   "Switch between color mapping modes.\n"   \
   "Int: Interpolate linearly between\n"   \
   "     colors in colormap\n"   \
   "NN : Use the nearest color in the\n"   \
   "     colormap. The index into the colormap\n"\
   "     of Ncol colors is given by:\n"\
   "     icol=floor((V-Vmin)/Vrange * Ncol)\n"   \
   "     with icol clipped to the range 0 to Ncol-1\n"   \
   "Dir: Use intensity values as indices\n"   \
   "     into the colormap.\n"   \
   "     In Dir mode, the intensity \n"   \
   "     clipping range is of no use.\n" \
   "     icol=floor(V) with clipping to the range 0 to Ncol-1"

#define SUMA_SurfContHelp_Link \
   "Switch between I, T selectors linking modes.\n"   \
   "None: Do nothing.\n"   \
   "Stat: Swith T selector to match an I selection with \n"\
   "      an obvious statistic. Matching is based on labels.\n"\
   "You can set your preference using environment variable\n"\
   "   SUMA_IxT_LinkMode\n"

#define SUMA_SurfContHelp_Bias \
   "Coordinate bias direction.\n"   \
   "   -: No bias thank you\n"   \
   "   x: X coord bias\n"   \
   "   y: Y coord bias\n"   \
   "   z: Z coord bias\n"   \
   "   n: bias along node's normal\n"  \
   "\n"   \
   "See more info in Bhelp for\n"   \
   "'C' table entry above.\n" \
   "\n"   \
   "This option will produce\n"   \
   "'Extremely Cool'[1] images.\n"   \
   "[1] Chuck E. Weiss (Slow River/\n" \
   "    Rykodisc) 1999."

#define SUMA_SurfContHelp_Cmp \
   "Switch between available color maps.\n"   \
   "If the number of colormaps is too large\n"   \
   "for the menu button, right click over\n"   \
   "the 'Cmp' label and a chooser with a \n"   \
   "slider bar will appear.\n"   \
   "\n"   \
   "More help is available via\n"   \
   "ctrl+h while mouse is over the\n"   \
   "colormap."   
   
#define SUMA_SurfContHelp_CmpNew \
   "Load new colormap.\n"   \
   "Loaded map will replace a\n"   \
   "pre-existing one with the\n"   \
   "same name.\n"   \
   "\n"   \
   "See ScaleToMap -help for \n"   \
   "details on the format of \n"   \
   "colormap file. The formats\n"   \
   "are described in the section\n"   \
   "for the option -cmapfile.\n"   \
   "\n"   \
   "A sample colormap would be:\n"   \
   " 0 0 1\n"   \
   " 1 1 1\n"   \
   " 1 0 0\n"   \
   "saved into a cmap file called\n"   \
   "cmap_test.1D.cmap"

#define  SUMA_SurfContHelp_AbsThr   \
   "Toggle Absolute thresholding.\n"   \
   "OFF: Mask node color for\n"   \
   "     nodes that have:  \n"   \
   "     T(n) < Tscale\n"   \
   "ON:  Mask node color for\n"   \
   "     nodes that have:\n"   \
   "     | T(n) | < Tscale\n"   \
   "where:\n"   \
   "Tscale is the value set by\n"   \
   "       the threshold scale.\n"   \
   "T(n) is the node value in the \n"   \
   "     selected threshold column (T).\n"   \
   "     this value is seen in the \n"   \
   "     second cell of the 'Value'\n"   \
   "     table on the left side."  

#define  SUMA_SurfContHelp_Isym   \
   "Toggle Intensity range symmetry\n"   \
   "about 0. \n"   \
   "ON : Intensity clipping range\n"   \
   "     is forced to go from \n"   \
   "     -val to val\n"   \
   "     This allows you to mimic\n"   \
   "     AFNI's ranging mode.\n"   \
   "OFF: Intensity clipping range\n"   \
   "     can be set to your liking."

#define  SUMA_SurfContHelp_Shw0   \
   "Toggle color masking of nodes \n"   \
   "with intensity = 0 \n"   \
   "ON : 0 intensities are mapped\n"   \
   "     to the colormap as any\n"   \
   "     other values.\n"   \
   "OFF: 0 intensities are masked,\n"   \
   "     a la AFNI"

#define  SUMA_SurfContHelp_RangeTbl_c0 \
   "Full range of values in Dset."
           
#define SUMA_SurfContHelp_RangeTbl_c1 \
   "Minimum value in Dset column."

#define SUMA_SurfContHelp_RangeTbl_c2 \
   "Node index at minimum.\n"   \
   "Right click in cell to\n"   \
   "have crosshair jump to\n"   \
   "node's index.\n"   \
   "Same as 'ctrl+j' or\n"   \
   "an entry in the 'Node' cell\n"   \
   "under Xhair Info block."  

#define SUMA_SurfContHelp_RangeTbl_c3 \
   "Maximum value in Dset column."

#define SUMA_SurfContHelp_RangeTbl_c4  \
   "Node index at maximum.\n" \
   "Right click in cell to\n" \
   "have crosshair jump to\n" \
   "node's index.\n"   \
   "Same as 'ctrl+j' or\n"   \
   "an entry in the 'Node' cell\n"   \
   "under Xhair Info block."

#define SUMA_SurfContHelp_RangeTbl_r1  \
   "Range of values in intensity (I) column."

#define SUMA_SurfContHelp_RangeTbl_r2  \
   "Range of values in threshold (T) column."

#define SUMA_SurfContHelp_RangeTbl_r3  \
   "Range of values in brightness (B) column."

/* this one's based on AFNI's func->thr_pval_label help */
#define SUMA_SurfContHelp_ThreshStats  \
   "Shows the estimated significance\n"   \
   "(p-value) of the threshold above,\n"  \
   "if possible.\n"  \
   "* If not possible, will display as\n" \
   "   '[N/A]' instead.\n" \
   "* p's that display as 1.2-7 should\n" \
   "   be interpreted as 1.2 x 10^(-7).\n"   \
   "* p-value here is significance PER NODE.\n" \
   "* If FDR curves are pre-computed in\n"   \
   "   the dataset header, then the False\n" \
   "   Discovery Rate q-value will also\n"   \
   "   be shown.\n"     \
   "* You can add FDR curves to a dataset\n" \
   "   with '3drefit -addFDR'.\n"   
   
#endif
