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

#define SUMA_GET_CELL_VALUE(TF, i, j, val)   {  \
   if (TF->type == SUMA_int || TF->type == SUMA_float) { \
      val = TF->num_value[j*TF->Ni+i];  \
   }  else {   \
      SUMA_SL_Err("Macro for numerical tables only"); \
      val = 0.0;  \
   }  \
}

#define SUMA_SET_CELL_VALUE(TF, i, j, val)   {  \
   if (TF->type == SUMA_int) {\
      TF->num_value[j*TF->Ni+i] = (int)val;  \
   } else if (TF->type == SUMA_float) { \
      TF->num_value[j*TF->Ni+i] = (float)val;  \
   }  else {   \
      SUMA_SL_Err("Macro for numerical tables only"); \
      val = 0.0;  \
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

#define SUMA_CELL_ROW_COL_2_1D(m_TF, m_row, m_col) \
   ( ((m_TF) && (m_row) >= 0 && (m_row) < (m_TF)->Ni && \
                (m_col) >= 0 && (m_col) < (m_TF)->Nj) \
         ? (m_row)+(m_TF)->Ni*(m_col):-1 )

#define SUMA_CELL_1D_2_ROW_COL(m_TF, m_n, m_row, m_col) {\
   (m_row) = (m_col) = -1;   \
   if (((m_TF)) && (m_n) >= 0) {   \
      (m_row) = (m_n) % (m_TF)->Ni; \
      (m_col) = (m_n) / (m_TF)->Ni; \
      if ((m_row) >= (m_TF)->Ni || (m_col) >= (m_TF)->Nj) {\
         (m_row) = (m_col) = -1;   \
      }\
   }  \
}

/* scale size gets messed up, see afni_widg.c and afni.h's
FIX_SCALE_SIZE*/
#define SUMA_FORCE_SCALE_HEIGHT(SurfCont) {\
  XtVaSetValues(  SurfCont->thr_sc, XmNheight,  SUMA_CMAP_HEIGHT-40, NULL ) ; \
}

#define SUMA_FORCE_SLICE_SCALE_WIDTH(SurfCont) {\
  /* Not needed, if you have problems make it mirror SUMA_FORCE_SCALE_HEIGHT \
  for scales inside SurfCont->Ax_slc, Sa_slc, and Co_slc */ \
}

#define SUMA_UPDATE_ALL_NODE_GUI_FIELDS(ado) {\
      SUMA_UpdateNodeNodeField(ado); \
      /* Now get the data values at that node */   \
      SUMA_UpdateNodeValField(ado);  \
      /* now find that node in the colored list */ \
      SUMA_UpdateNodeLblField(ado);  \
}    

/* Don't attempt to break up this string over multiple lines */
#define SUMA_SHPINX_BREAK  ".. container:: clearer\n\n   .. image:: media/blank.jpg\n\n   .. Preceding block is a trick to keep upcoming text from wrapping around the figures.\n\n"

/*! structure for holding table data for range setting */
typedef struct {
   SUMA_ALL_DO *ado;
   SUMA_OVERLAYS *colp;
} SUMA_SRV_DATA;

SUMA_Boolean SUMA_isTopColPlane(SUMA_OVERLAYS *cp, SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_isCurColPlane(SUMA_OVERLAYS *cp, SUMA_ALL_DO *ado);
SUMA_X_SurfCont *SUMA_ADO_Cont(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_ADO_ShowCurForeOnly(SUMA_ALL_DO *ado);
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
SUMA_Boolean SUMA_TDO_PointXYZ_eng(SUMA_TractDO *tdo, int point, 
                                   int *BTP, float *here);
float *SUMA_TDO_PointXYZ(SUMA_TractDO *tdo, int point, int *BTP, float *here);
SUMA_Boolean SUMA_VO_PointXYZ_eng(SUMA_VolumeObject *vo, int point, 
                                   int *IJK, float *here);
float *SUMA_VO_PointXYZ(SUMA_VolumeObject *vo, int point, int *IJK, float *here);
SUMA_Boolean SUMA_MDO_PointXYZ_eng(SUMA_MaskDO *mo, int point, 
                                   int *IJK, float *here);
float *SUMA_MDO_PointXYZ(SUMA_MaskDO *mdo, int point, int *BTP, float *here);
char *SUMA_ADO_LDP(SUMA_ALL_DO *ado);
char * SUMA_ADO_Label(SUMA_ALL_DO *ado);
char * SUMA_ADO_CropLabel(SUMA_ALL_DO *ado, int len);
SUMA_Boolean SUMA_ADO_isLabel(SUMA_ALL_DO *ado, char *lbl);
char *SUMA_ADO_sLabel(SUMA_ALL_DO *ado);
char * SUMA_ADO_idcode(SUMA_ALL_DO *ado);
char * SUMA_ADO_Parent_idcode(SUMA_ALL_DO *ado);
SUMA_CIFTI_SAUX *SUMA_ADO_CSaux(SUMA_ALL_DO *ado);
SUMA_GRAPH_SAUX *SUMA_ADO_GSaux(SUMA_ALL_DO *ado);
SUMA_TRACT_SAUX *SUMA_ADO_TSaux(SUMA_ALL_DO *ado);
SUMA_MASK_SAUX *SUMA_ADO_MSaux(SUMA_ALL_DO *ado);
SUMA_SURF_SAUX *SUMA_ADO_SSaux(SUMA_ALL_DO *ado);
SUMA_VOL_SAUX *SUMA_ADO_VSaux(SUMA_ALL_DO *ado);
void *SUMA_ADO_Saux(SUMA_ALL_DO *ado);
void SUMA_cb_ShowCoSlice_toggled(Widget w, XtPointer data,XtPointer client_data);
int SUMA_SetShowSlice(SUMA_VolumeObject *vdo, char *variant, int val);
void SUMA_cb_ShowSaSlice_toggled(Widget w, XtPointer data,XtPointer client_data);
void SUMA_cb_ShowAxSlice_toggled(Widget w, XtPointer data,XtPointer client_data);
SUMA_DSET *SUMA_ADO_Dset(SUMA_ALL_DO *ado);
int SUMA_Anatomical_DOs(SUMA_DO *dov, int N_dov, int *rdov);
int SUMA_ADO_N_Datum(SUMA_ALL_DO *ado);
int SUMA_ADO_N_Datum_Lev(SUMA_ALL_DO *ado, SUMA_DATUM_LEVEL dtlvl);
int SUMA_ADO_Max_Datum_Index(SUMA_ALL_DO *ado);
int SUMA_ADO_Max_Datum_Index_Lev(SUMA_ALL_DO *ado, SUMA_DATUM_LEVEL dtlvl);
char * SUMA_ADO_variant(SUMA_ALL_DO *ado);
int SUMA_ADO_ColPlane_SelectedDatum(SUMA_ALL_DO *ado, SUMA_OVERLAYS *Sover);
int SUMA_ADO_SelectedDatum(SUMA_ALL_DO *ado, void *extra, void *extra2);
int SUMA_ADO_SelectedSecondary(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_is_ADO_Datum_Primitive(SUMA_ALL_DO *ado,
                                          SUMA_COLID_OFFSET_DATUM *codf);
SUMA_Boolean SUMA_ADO_Set_SelectedDatum(SUMA_ALL_DO *ado, int sel, 
                                        void *extra, void *extra2);
int SUMA_ADO_N_Overlays(SUMA_ALL_DO *ado);
SUMA_OVERLAYS * SUMA_ADO_Overlay0(SUMA_ALL_DO *ado);
SUMA_OVERLAYS * SUMA_ADO_Overlay(SUMA_ALL_DO *ado, int i);
SUMA_OVERLAYS * SUMA_ADO_CurColPlane(SUMA_ALL_DO *ado);
SUMA_OVERLAYS **  SUMA_ADO_Overlays(SUMA_ALL_DO *ado, int *N_over);
SUMA_Boolean SUMA_ADO_Append_Overlay(SUMA_ALL_DO *ado, SUMA_OVERLAYS **over);
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
int SUMA_set_threshold_label(SUMA_ALL_DO *ado, float val, float val2);
void SUMA_optmenu_EV( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch );
void SUMA_cb_SetCoordBias(Widget widget, XtPointer client_data, 
                          XtPointer call_data);
SUMA_Boolean SUMA_RedisplayAllShowing(char *SO_idcode_str, 
                                      SUMA_SurfaceViewer *SVv, int N_SVv);
void SUMA_CreateSliceFields(  Widget parent,
                        char *tit, char *hint, char *help, 
                        int Nslc, char *var, SUMA_ALL_DO *ado,
                        void (*NewValueCallback)(void * data), void *cb_data,
                        SUMA_SLICE_FIELD *SF);
void SUMA_CreateTable(  Widget parent,
            int Ni, int Nj, 
            char *wname,
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
void SUMA_TableF_cb_label_Modify (Widget w, XtPointer client_data, 
                                  XtPointer call_data);
void SUMA_TableF_SetString (SUMA_TABLE_FIELD * AF);
void SUMA_TableF_cb_label_change (Widget w, XtPointer client_data, 
                                  XtPointer call_data);
void SUMA_leave_TableField( Widget w , XtPointer client_data ,
                           XEvent * ev , Boolean * continue_to_dispatch );
void SUMA_SliceF_cb_mont_change (  Widget w, XtPointer client_data, 
                                    XtPointer call_data);
void SUMA_leave_SliceField( Widget w , XtPointer client_data ,
                           XEvent * ev , Boolean * continue_to_dispatch );
void SUMA_leave_MontField( Widget w , XtPointer client_data ,
                            XEvent * ev , Boolean * continue_to_dispatch );
int SUMA_SetRangeValueNew(SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset,
                          SUMA_NUMERICAL_UNITS num_units);
int SUMA_SetRangeValueNew_one(SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset,
                          SUMA_NUMERICAL_UNITS num_units);
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
SUMA_TABLE_FIELD * SUMA_AllocTableField(char *wname);
SUMA_TABLE_FIELD * SUMA_FreeTableField(SUMA_TABLE_FIELD *TF);
SUMA_SLICE_FIELD * SUMA_AllocSliceField(char *wname);
SUMA_SLICE_FIELD * SUMA_FreeSliceField(SUMA_SLICE_FIELD *SF);
SUMA_VR_FIELD * SUMA_AllocVRField(char *wname);
SUMA_VR_FIELD * SUMA_FreeVRField(SUMA_VR_FIELD *VrF);
int SUMA_set_slice_label(SUMA_ALL_DO *ado, char *variant, float val);
int SUMA_set_slice_scale(SUMA_ALL_DO *ado, char *variant, float val);
void SUMA_cb_set_Ax_slice_label(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cb_set_Sa_slice_label(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cb_set_Co_slice_label(Widget w, XtPointer clientData, XtPointer call);
void SUMA_SliceF_SetString (SUMA_SLICE_FIELD * SF);
void SUMA_SliceF_cb_label_change (  Widget w, XtPointer client_data, 
                                    XtPointer call_data);
int SUMA_set_slice(SUMA_ALL_DO *ado, char *variant, float *valp, 
                   char *caller, int redisp);
int SUMA_set_mont(SUMA_ALL_DO *ado, char *variant, 
                  float *val1p, float *val2p,
                  char *caller, int redisp);
void SUMA_cb_set_Co_slice(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cb_set_Sa_slice(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cb_set_Ax_slice(Widget w, XtPointer clientData, XtPointer call);
SUMA_CELL_VARIETY SUMA_cellvariety (SUMA_TABLE_FIELD *TF, int n);
int SUMA_RowTitCell(SUMA_TABLE_FIELD *TF, int r);
int SUMA_ColTitCell(SUMA_TABLE_FIELD *TF, int c);
int SUMA_ObjectID_Row(SUMA_TABLE_FIELD *TF, char *id);
SUMA_Boolean SUMA_InitRangeTable(SUMA_ALL_DO *ado, int what);
SUMA_Boolean SUMA_InitClustTable(SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets(Widget parent, SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets_SO(Widget parent, SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets_GLDO(Widget parent, SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets_TDO(Widget parent, SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets_CO(Widget parent, SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets_VO(Widget parent, SUMA_ALL_DO *ado);
void SUMA_CreateXhairWidgets_MDO(Widget parent, SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_UpdateXhairField(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_UpdateCrossHairNodeLabelField(SUMA_SurfaceViewer *sv);
void SUMA_XhairInput (void* data);
void SUMA_NodeInput (void* data);
void SUMA_GNodeInput (void *data);
void  SUMA_SetCellEditMode(SUMA_TABLE_FIELD *TF, int i, int j, int Mode);
void SUMA_TriInput (void* data);
void SUMA_TpointInput(void*data);
void SUMA_IJKInput(void*data);
SUMA_Boolean SUMA_UpdateTriField(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_UpdateNodeLblField(SUMA_ALL_DO *ADO);
SUMA_Boolean SUMA_UpdateNodeLblField_ADO(SUMA_ALL_DO *ADO);
char **SUMA_FormNodeValFieldStrings(SUMA_ALL_DO *ado, 
                                 SUMA_DSET *dset, int Node,
                                 int find, int tind, int bind, int dec,
                                 double *I, double *T, double *B);
SUMA_Boolean SUMA_GetNodeValsAtSelection(SUMA_ALL_DO *ado, 
               SUMA_DSET *dset, int Node,
               int find, int tind, int bind,
               double *I, double *T, double *B) ;
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
void SUMA_set_cmap_options_VO(SUMA_ALL_DO *ado, SUMA_Boolean NewDset,
                                SUMA_Boolean NewMap);
void SUMA_set_cmap_options_CO(SUMA_ALL_DO *ado, SUMA_Boolean NewDset,
                           SUMA_Boolean NewMap);
void SUMA_cb_Cmap_Load(Widget w, XtPointer data, XtPointer client_data);
SUMA_COLOR_MAP *SUMA_LoadCmapFile_eng(char *filename);
void SUMA_LoadCmapFile (char *filename, void *data);
SUMA_Boolean  SUMA_Insert_Cmap_of_Dset(SUMA_DSET *dset);
void SUMA_CreateUpdatableCmapMenu(SUMA_ALL_DO *ado);
int SUMA_ThreshVal2ScalePos(SUMA_ALL_DO *ado, float *val);
int  SUMA_SliceVal2ScalePos (SUMA_ALL_DO *ado, char *variant, float *val);
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
char *SUMA_GetLabelsAtSelection(SUMA_ALL_DO *ado, int node, int sec);
char *SUMA_GetLabelsAtSelection_ADO(SUMA_ALL_DO *ado, int node, int sec);
SUMA_Boolean SUMA_GetValuesAtSelection(SUMA_ALL_DO *ado, int fromtable,
                                       float *I, float *T, float *B);
SUMA_Boolean SUMA_SetCmodeMenuChoice(SUMA_ALL_DO *ado, char *str);
SUMA_NIDO *SUMA_NodeLabelToTextNIDO (char *lbls, SUMA_ALL_DO *ado, 
                                     SUMA_SurfaceViewer *sv);
XmFontList SUMA_AppendToFontList(XmFontList fontlisti, Widget w, 
                                 char *fontname, char *tag);

/* the help strings */

/* Surface Properties Block */
#define  SUMA_SurfContHelp_more  \
   "Opens a dialog with detailed " \
   "information about the object in geek speak."

   #define  SUMA_SurfContHelp_RenderMode  \
"Choose the rendering mode for this surface.:LR:\n" \
"   Viewer: Surface's rendering mode is set "  \
":         :by the viewer's setting which can "   \
":         :be changed with the :ref:`'p'<LC_p>` option.:LR:\n"  \
"   Fill:   Shaded rendering mode.:LR:\n"  \
"   Line:   Mesh rendering mode.:LR:\n"    \
"   Points: Points rendering mode.:LR:\n"   

   #define  SUMA_SurfContHelp_VTransMode  \
"Set the transparency level for this set of slices to one of the " \
"following options::LR:\n" \
"   Viewer: Surface's transparency is set "  \
"           by the viewer's setting which can "   \
"           be changed with the :ref:`o<LC_o>`, :ref:`O<UC_O>` options.:LR:\n"\
"           Only Cheesecloth transparency is allowed in "\
"           this setting.:LR:\n"\
"   A :   Alpha blending. May look good, but not always "\
"         accurate.:LR:\n"  \
"   0 :   No transparency, opaque.:LR:\n"  \
"   ...:LR:\n"    \
"   16:   Maximum transparency, invisibile:LR:\n"   

   #define  SUMA_SurfContHelp_TransMode  \
"Set the transparency for this surface to one of the following "   \
"options.:LR:\n" \
"   Viewer: Surface's transparency is set "  \
"           by the viewer's setting which can "   \
"           be changed with the :ref:`o<LC_o>`, :ref:`O<UC_O>`  options.:LR:\n" \
"   0 :   No transparency, opaque.:LR:\n"  \
"   ...:LR:\n"    \
"   16:   Maximum transparency, invisibile:LR:\n"   

#define  SUMA_SurfContHelp_DsetViewMode  \
   "Choose the viewing mode for this dataset.:LR:\n" \
   "   Col: Colours, only.:LR:\n"  \
   "   Con: Contours (slower), only.:LR:\n"  \
   "   C&C: Colours and Contours (slower), only.:LR:\n"    \
   "   XXX: Unfortunately nothing, only.:LR:\n"  \
   " There is one contour created for each color "  \
   " in the colormap. You'd want to use colormaps with "  \
   " few colors to get a contour of use. " \
   " Contours are not created if colormap has panes "   \
   " of unequal sizes.\n"   

#define  SUMA_SurfContHelp_DsetFont  \
   "Choose the font size for labels of nodes.:LR:\n" \
   "   8: 8x13.:LR:\n"  \
   "   9: 9x15.:LR:\n"  \
   "   TR10: Times New Roman 10.:LR:\n"    \
   "   HE10: Helvetica 10.:LR:\n"    \
   "   HE12: Helvetica 12.:LR:\n"    \
   "   HE18: Helvetica 18.:LR:\n"    \
   "   TR24: Times New Roman 24.:LR:\n"    \
   "   XXX: Show no text.:LR:\n"  

#define  SUMA_SurfContHelp_DsetNodeRad  \
   "Choose the radius sizing for nodes of this graph dataset.:LR:\n" \
   "   Const: All nodes have a radius of 1 x Gain.:LR:\n"  \
   "   Val: Nodes size equals its dset value x Gain. A node's dset"\
   "value is that of the edge connecting the node to itself:LR:\n"  \
   "   XXX: Show no balls.:LR:\n"  

#define  SUMA_SurfContHelp_DsetThrough  \
   "When a node, rather than an edge is :ref:`selected<Selecting_Objects>`, "\
   "choose how connections to it are displayed.:LR:\n" \
   "   Edg: Show connections to selected node with edges, either straight "\
   "lines or with bundles.:LR:\n"  \
   "   Col: Show connections to selected node by changing the colors of "\
   "the connecting nodes, based on edge value. Edges are not displayed. "\
   "the idea here is to reduce the clutter of the display, while still "\
   "allowing you to visualize connection strength to one node at a time.:LR:\n"\
  "   Rad: Show connections to selected node by changing the  radius of the "\
  "connecting nodes, based on edge value. Edges are not displayed in this "\
  "mode also.:LR:\n"\
   "   CaR: Both Col and Rad:LR:\n"\
   "   XXX: Do nothing special, keep showing whole graph, even when "\
   "selecting a graph node.:LR:\n"  

#define  SUMA_SurfContHelp_DsetEdgeThick  \
   "Choose the thickness for edges of this graph dataset.:LR:\n" \
   "   Const: All nodes have a radius of 1 x Gain.:LR:\n"  \
   "   Val: Edge size equals its dset value x Gain:LR:\n"  

#define  SUMA_SurfContHelp_TractStyle  \
   "Choose the line drawing style.:LR:\n" \
   "   Digits specify number of pixels to mask out of each 16 pixels:LR:\n"\
   "   1 :   One pixel/16 off, almost solid:LR:\n"  \
   "   ...:LR:\n"    \
   "   15:   15/16 pixels off, almost invisible:LR:\n"   \
   "   HDE: Hide all the tracts:LR:\n"  \
   "   SLD: No stippling, solid line.:LR:\n"  

#define  SUMA_SurfContHelp_DsetEdgeStip  \
   "Choose the stippling for edges of this graph dataset.:LR:\n" \
   "   1 :   One pixel/16 off, almost solid:LR:\n"  \
   "   ...:LR:\n"    \
   "   15:   15/16 pixels off, almost invisible:LR:\n"   \
   "   Val: Set stippling based on the dset value:LR:\n"  \
   "   XXX: No stippling, solid line.:LR:\n"  

   #define  SUMA_SurfContHelp_DsetAlphaVal  \
"Choose the method for assigning an alpha value (A) to a voxel's color.:LR:\n" \
"   Avg :  A = average of R, G, B values:LR:\n"  \
"   Max :  A = maximum of R, G, B values:LR:\n"    \
"   Min :  A = minimum of R, G, B values:LR:\n"    \
"   I :  A is based on I selection. I range parameters apply :LR:\n"   \
"   T :  A is based on T selection. Full range is used.:LR:\n"  \
"   B :  A is based on B selection. B range parameters apply:LR:\n" \
"   XXX: A is set to 0, nothing will show.:LR:\n"  

   #define  SUMA_SurfContHelp_TractMask  \
"That's not the name of the button, but its default value. "\
"This menu controls how tracts that fall outside of the masks are "\
"displayed::LR:\n"\
"   Hde:   Hide 'em masked tracts:LR:\n"  \
"   Gry:   Gray 'em masked tracts (gray color set by"\
" :ref:`Gry<TractCont->Coloring_Controls->Gry>` arrow field):LR:\n"\
"   One:   A coding mistake that ended up looking cool. Each tract not in "\
"the mask is colored by one color extracted from the set of colors for the"\
" whole network.:LR:\n"\
"   Ign:   Ignore 'em good for nothing masks, show tracts in all their"\
" unabashed glory:LR:\n"
      
#define  SUMA_SurfContHelp_TractMaskGray  \
   "Set the gray level for masked tracts. 0 for black, 100 for white\n" \
   "   This arrow field only has an effect when 'Msk' menu is set to 'Gry'\n"   \
      
#define  SUMA_SurfContHelp_DsetNodeCol  \
   "Choose the colorization method for nodes of this dataset.:LR:\n" \
   "   White: Alle weiss.:LR:\n"  \
   "   Black: Tutti nero.:LR:\n"  \
   "   Red: Sve crveno.:LR:\n"  \
   "   Green: Killon akhdar.:LR:\n" \
   "   Blue: Tous bleu.:LR:\n"   \
   "   Yellow: Todos amarillo.:LR:\n"   \
   "   Gray50: Not there yet.:LR:\n" \
   "   Val: Nodes color is based its dset value and the chosen colormap:LR:\n"

#define  SUMA_SurfContHelp_DsetTxtShad  \
   "Choose the variants for how labels are handled.:LR:\n" \
   "   T : Text shown unless more than 50% occluded.:LR:\n"  \
   "   Ts: Foreground text bright, occluded text shaded:LR:\n"  \
   "   B : Text shown with background box unless more than 50% occluded:LR:\n"  \
   "   Bs: Foreground text with background, occluded text shaded:LR:\n"   \
   "   Ta: All text shown, occlusions be damned.:LR:\n"\
   "   Ba: All text shown with background, damn the torpedoes.:LR:\n"

#define  SUMA_SurfContHelp_DsetGmatBord  \
   "Choose the partition ratio of the matrix border. This option only applies to the matrix display of the graph.:LR:\n" \
   "   XX: No partition.:LR:\n"  \
   "   5: Border is 1/5 of cell width.:LR:\n"  \
   "   10: Border is 1/10 of cell width.:LR:\n"  \
   "   20: Border is 1/20 of cell width.:LR:\n" \
   "   30: Border is 1/30 of cell width.:LR:\n"   \
   "   40: Border is 1/40 of cell width.:LR:\n"

#define SUMA_SurfContHelp_Dsets  \
   "Show/Hide Dataset (previously Color Plane) controllers"

#define SUMA_SurfContHelp_Xhr \
   "Set/Get crosshair location in mm :term:`RAI` on\n"   \
   "this controller's selected object.\n"   \
   "Entering new coordinates \n"   \
   "makes the crosshair jump\n"   \
   "to that location (like :ref:`'ctrl+j' <LC_Ctrl+j>`).\n"   \
   "Use :ref:`'alt+l'<LC_Alt+l>` to center the\n"   \
   "cross hair in your viewer."    

#define SUMA_SurfContHelp_Node   \
   "Index of node in focus (1) and :term:`RAI` coordinates "   \
   "of that node (2).:LR:\n"   \
   "1- The index is of the node in focus on this controller's "   \
   "surface. Nodes in focus are "   \
   "highlighted by the blue sphere "   \
   "in the crosshair.\n"   \
   "This cell is editable; manually entering a new node's index "   \
   "will put that node in focus "   \
   "and send the crosshair to its "   \
   "location (like :ref:`'j' <LC_j>`). "   \
   "Use :ref:`'alt+l' <LC_Alt+l>` to center the "   \
   "cross hair in your viewer.:LR:\n" \
   "2- The :term:`RAI` coordinates are those of the surface node after all spatial transformations have been applied to the surface. Those transformations do not include visualization transformations applied in the viewer"

#define SUMA_SurfContHelp_GNode   \
   "Index of the node closest to the selection location on the edge's "  \
   "representation.:LR:\n"\
   "*NOTE* that a node is also an edge that starts and ends at the same"\
   "node. Think diagonal elements of a connectivity matrix."
        
#define SUMA_TractContHelp_I   \
   "Set/Get the :term:`1D index` of the selected elementary tract datum: "\
   "the infinitesimal point.\n" 

#define SUMA_SurfContHelp_I   \
   "Set/Get the :term:`1D index` of the selected elementary surface datum: "\
   "the node.\n" 

#define SUMA_GraphContHelp_I   \
   "Set/Get the :term:`1D index` of the selected elementary surface datum: "\
   "the edge.\n" 

#define SUMA_VolContHelp_I   \
   "Set/Get the :term:`1D index` of the selected elementary surface datum: "\
   "the voxel.\n" 

   #define SUMA_SurfContHelp_BTP   \
"Set/Get the triplet of indices for the selection on the displayed tracts.\n"\
"   The 1st index is that of the selected :term:`bundle` in the network:LR:\n"\
"   The second is for the selected :term:`tract` in that bundle:LR:\n"  \
"   The third is the index of the :term:`point` selected along that tract.:LR:\n"

#define SUMA_SurfContHelp_IJK   \
   "Triplet of indices (I) of selected voxel.\n"   \
   "The mm RAI coordinate X = M I with M being the matrix transforming\n"  \
   "voxel indices to voxel coordinates.\n" 


#define SUMA_SurfContHelp_Tri   \
   "1- Triangle (faceset) index of "   \
   "triangle in focus on this  "   \
   "on this controller's surface.\n"   \
   "Triangle in focus is highlighted "   \
   "in gray, and entering a new triangle's "   \
   "index will set a new triangle in "   \
   "focus (like :ref:`'J'<UC_j>`).:LR:\n"   \
   "2- Indices of nodes forming triangle.:LR:\n"

   #define SUMA_SurfContHelp_GEdge \
"1- Edge/Cell Index:  Get/Set index of :term:`edge`/:term:`cell` in focus on "\
"this controller's graph. This number is the :term:`1D index` of the edge/cell "\
"in the graph/matrix. Consider it the equivalent of a voxel 1D index in "\
"a volume, or a node in a surface dataset. \n" \
"Entering a new edge's index will put that edge  in focus and send the "\
"crosshair to its center (like :ref:`j<LC_j>`). "\
"Use :ref:`alt+l<LC_Alt+l>` to center the cross hair in your viewer.:LR:\n" \
"Note that an edge can be formed by a pair of identical nodes - think "\
"matrix diagonal.:LR:\n"\
"2- Nodes Forming Directed Edge/Cell: For a cell, this would its pair of "\
"row and column indices into the matrix. For a graph, this would be the "\
"indices of the :term:`nodes` forming the directed edge."  
   
#define SUMA_SurfContHelp_NodeValTblr0 \
   "Data values at node in focus"
#define SUMA_SurfContHelp_GEdgeValTblr0 \
   "Data values at edge in focus. :term:`Intensity`, "\
":term:`Threshold`, and :term:`Brightness` show the triplets of values "\
" at the selected edge that correspond to the graph/matrix  choices." \
"in :ref:`I<VolCont->Dset_Mapping->I>`, :ref:`T<VolCont->Dset_Mapping->T>`, and :ref:`B<VolCont->Dset_Mapping->B>` selectors."
   
   #define SUMA_SurfContHelp_NodeValTblc0 \
"Data values at node in focus. :term:`Intensity`, "\
":term:`Threshold`, and :term:`Brightness` show the triplets of values "\
" at the selected node that correspond to the dataset column choices " \
"in :ref:`I<SurfCont->Dset_Mapping->I>`, :ref:`T<SurfCont->Dset_Mapping->T>`, and :ref:`B<SurfCont->Dset_Mapping->B>` selectors."
   
   
   #define SUMA_TractContHelp_NodeValTblc0 \
"Data values at point in focus. At the moment, :term:`Intensity`, "\
":term:`Threshold`, and :term:`Brightness` show the RGB values for the point "\
"selected. Eventually, they would represent the triplets of values at the point"\
" that correspond to the dataset column choices in :term:`I`, :term:`T`,"\
" :term:`B`."

   #define SUMA_VolContHelp_NodeValTblc0 \
"Data values at voxel in focus. :term:`Intensity`, "\
":term:`Threshold`, and :term:`Brightness` show the triplets of values "\
" at the selected voxel that correspond to the volume column choices " \
"in :ref:`I<VolCont->Dset_Mapping->I>`, :ref:`T<VolCont->Dset_Mapping->T>`, and :ref:`B<VolCont->Dset_Mapping->B>` selectors."

#define SUMA_SurfContHelp_GEdgeValTblc0 \
   SUMA_SurfContHelp_GEdgeValTblr0
   
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
   "Labels available at the selected datum.\n" \
   "If nothing is available, datum color\n"   \
   "is displayed."

#define SUMA_TractContHelp_NodeLabelTblr0 \
   "Labels at selected point. For now, nothing more than a regurgitation " \
   "of :ref:`BTP<TractCont->Xhair_Info->BTP.r00>`"

#define SUMA_SurfContHelp_GEdgeLabelTblr0\
   "Labels from the selected graph dataset\n" \
   "at the edge in focus.\n"   \
   "If no labels are available, edge color\n"   \
   "is displayed."
   
#define SUMA_SurfContHelp_DsetLblTblr0 \
  "Label of dataset currently selected. Note that for some objects, "\
  "like surfaces, what you're viewing "\
  "at any moment maybe a blend of multiple datasets. See "\
  ":ref:`color mixing<ColorMixing>` for details." 
  
#define SUMA_SurfContHelp_DsetLblTblr1 \
   "Parent surface of Dset."

#define SUMA_SurfContHelp_DsetOrd \
   "Order of Dset's colorplane in the stack of all colorplanes of the parent surface.\n"  \
   "The datset with highest order number is \n"   \
   "on top of the stack. Separate \n"  \
   ":ref:`stacks<Plane_Layering>` exist for foreground (fg:)\n" \
   "and background planes (bg:).:LR:\n"   \
   ":SPX:See :ref:`Color Mixing<ColorMixing>` for details on how colors "\
   "are merged.:SPX:"

#define SUMA_TractContHelp_DsetOrd \
   "Order of this tract's dataset colorplane in the stack of all colorplanes available.\n"  \
   "The datset with highest order number is \n"   \
   "on top of the stack.\n"   \
   ":SPX:See :ref:`color plane grouping <Color_Plane_Grouping>` for details "\
   "on how colors are merged.:SPX:"
 
#define SUMA_SurfContHelp_DsetAlphaThresh \
   "Alpha threshold of Dset's rendered slices.\n"  \
   "When datasets' voxels get colored, they get an Alpha (A) value\n"\
   "in addition to the R, G, B values. A is computed based on\n"\
   "the setting of the 'Avl' menu.\n"\
   "Voxels (or more precisely, their openGL realization) \n"   \
   "with Alpha lower than this value will not get rendered.\n"  \
   "This is another way to 'threshold' a rendered volume, and \n" \
   "is comparable to thresholding with the slider bar if using a\n"\
   "monochromatic increasingly monotonic colormap with 'Avl' set to\n"\
   "one of Max, Min, or Avg.\n"  \
   "Note that thresholding with the slider bar sets A for thresholded \n" \
   "voxels to 0.0 regardless of the setting for 'Avl'.\n"   \
   "Thresholding with Ath is faster than using the slider bar because \n" \
   "it does not require recreating the whole texture."
  
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
   "planes within the same :ref:`group<Color_Plane_Grouping>`  \n"  \
   "foreground (fg:) or background(bg:).\n"   \
   "\n"  \
   "Opacity values are not applied\n"  \
   "to the first plane in a group.\n"   \
   "Consequently, if you have just\n"   \
   "one plane to work with, opacity \n"   \
   "value is meaningless.\n"  \
   "\n"  \
   "Color mixing can be done in two \n"  \
   "ways, use :ref:`F7<F7>` to toggle between \n" \
   "mixing modes.\n"
   
#define SUMA_TractContHelp_DsetOpa \
   "Opacity of Dset's colorplane.\n"  \
   "Opaque planes have an opacity\n"   \
   "of 1, transparent planes have\n"  \
   "an opacity of 0. \n"   \
   "\n"\
   "Opacity values are not applied\n"  \
   "to the first plane in a group.\n"   \
   "Consequently, if you have just\n"   \
   "one plane to work with, or you have "\
   ":ref:`1<TractCont->Coloring_Controls->1>` ON, \n"\
   "the opacity  value is meaningless.\n"  \
   "\n"  \
   "Color mixing can be done in two \n"  \
   "ways, use :ref:`F7<F7>` to toggle between \n" \
   "mixing modes.\n"
   
#define SUMA_SurfContHelp_DsetDim  \
   "Dimming factor to apply to colormap\n" \
   "before mapping the intensity (I) data.\n" \
   "The colormap, if displayed on the right,\n"   \
   "is not visibly affected by Dim but the\n"   \
   "colors mapped onto the surface, voxel grid, tracts, etc. are.\n"   \
   "For RGB Dsets (e.g. .col files, or tract colors), Dim is\n" \
   "applied to the RGB colors directly.:LR:\n"  \
   "Decreasing Dim is useful when the colors are too saturated\n" \
   "for lighting to reflect the object terrain.\n" \
   "When in doubt, just press the button and see what happens."   \

#define SUMA_SurfContHelp_DsetNodeRadGain  \
   "Gain to apply to node radius.\n" \
   "This multiplier is always applied to whatever\n" \
   "radius value the node gets, whether is it constant\n"   \
   "or data derived.\n"   
   
#define SUMA_SurfContHelp_DsetEdgeThickGain  \
   "Gain to apply to edge thickness.\n" \
   "This multiplier is always applied to whatever\n" \
   "thickness value the edge gets, whether is it constant\n"   \
   "or data derived.\n"   
   
#define SUMA_SurfContHelp_DsetView  \
   "View (ON)/Hide Dset node colors"

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

#define SUMA_TractContHelp_DsetViewOne  \
   "If ON, view only the selected\n"\
   "Dset's colors.\n"   \
   "\n"  \
   "If OFF, then mix the color planes\n"  \
   "in the datasets stack.\n"  \
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

#define SUMA_TractContHelp_DsetSwitch   \
"Select the dataset to which the Coloring Controls are being applied. For now "\
"you have three free RGB datasets per network that are created by SUMA. In the"\
" first one each node of a tract is colored based on the local orientation, "\
"with red, green, and blue values reflecting the X,Y, and Z components of the"\
" unit direction vector. In the second dataset all nodes of a tract are "\
"assigned the color of the middle node of that tract. In the third dataset, all"\
" nodes of a tract are colored based on the bundle in which that tract"\
" resides.The number of colors in such a dataset depend on the total number of"\
" bundles in the entire network."

#define SUMA_SurfContHelp_SetThreshTblr0   \
"Set/Get the threshold value.\n"  \
"When statistical parameters are set under "\
":ref:`T <VolCont->Dset_Mapping->T>`, you can \n"  \
"append a 'p' to set by the p value, as in 0.001p.:LR:\n" \
"For percentile thresholding, append a '%' to "\
"the value, such as 25%\n"

   #define SUMA_SurfContHelp_MasksLoad  \
"Load a set of masks previously saved by my neighboring button. The save "\
"operation also preserves the :ref:`Mask Eval<MaskCont->Masks->Mask_Eval.r00>` "\
"expression.:LR:\nReloading a mask file will replace current masks."

#define SUMA_SurfContHelp_MasksSave  \
   "Save current set of masks for future loading by my neighborly button.\n"   
   
#define SUMA_SurfContHelp_DsetLoad  \
   "Load a new dataset (Dset).\n"   \
   "Datasets can be of 3 formats::LR:\n"   \
   "1- NIML (.niml.dset): "   \
   ":   :This format is internal "   \
   ":   :to AFNI/SUMA. :LR:\n"   \
   "2- GIFTI (.gii.dset):"\
   ":   :The format to end all formats.:LR:\n"  \
   "3- 1D   (.1D.dset): "   \
   ":   :Simple ASCII tabular format "   \
   ":   :supporting numerical values "   \
   ":   :only.\n"   \
   "     Each row i contains Nj data "   \
   ":   :values per node."   \
   ":   :Since this format has no header"   \
   ":   :associated with it, it makes"   \
   ":   :some assumption about the data"   \
   ":   :in the columns. :LR:\n"   \
   "   You can choose from 3 options: \n"   \
   ":   :(see below for nomenclature):LR:\n"   \
   "   - Each column has Ni values where\n"   \
   ":   :Ni = N_Node. \n"   \
   ":   :In this case, it is assumed that\n"   \
   ":   :row i has values for node i on\n"   \
   ":   :the surface.\n"   \
   "   - If Ni is not equal to N_Node then\n"   \
   ":   :SUMA will check to see if column 0\n"   \
   ":   :(Col_0) is all integers with values\n"   \
   ":   :v satisfying:  0 <= v < N_Node .\n"   \
   "     If that is the case then column 0\n"   \
   ":   :contains the node indices. The values\n"   \
   ":   :in row j of Dset are for the node\n"   \
   ":   :indexed Col_0[j].\n"   \
   "     In the sample :term:`1D` Dset shown below\n"   \
   ":   :assuming N_Node > 58, SUMA\n"   \
   ":   :will consider the 1st column to \n"   \
   ":   :contain node indices. In that case\n"   \
   ":   :the values -12.1 and 0.9 are for \n"   \
   ":   :node 58 on the surface.\n"   \
   "   - Lastly, if Col_0 fails the node index\n"   \
   ":   :test, then SUMA considers the data\n"   \
   ":   :in row i to be associated with node i.\n"   \
   "\n"   \
   "   If you're confused, try creating some\n"   \
   ":   :toy datasets like the one below and \n"   \
   ":   :load them into SUMA.\n"   \
   "\n"   \
   "   Sample 1D Dset (Call it pickle.1D.dset):\n"   \
   "     25    22.7     1.2   \n"   \
   "     58    -12.1    0.9   \n"   \
   "\n"   \
   "   Nomenclature and conventions:\n"   \
   "     - N_Node is the number of nodes\n"   \
   ":     :forming the surface.\n"   \
   "     - Indexing always starts at 0.\n"   \
   ":     :In the example, value v at \n"   \
   ":     :row 0, column 1 is v = 22.7 .\n"   \
   "     - A Dset has Ni rows and Nj columns.\n"   \
   ":     :In other terms, Ni is the number\n"   \
   ":     :of values per node and Nj is the\n"   \
   ":     :number of nodes for which data are\n"   \
   ":     :specified in Dset.\n"   \
   ":     :Ni = 2, Nj = 3 in the example."
   
#define SUMA_SurfContHelp_DsetLoadCol  \
   "Load a new color plane.\n"   \
   "A color plane is a :term:`1D` text file with \n" \
   "each row formatted as such:LR:\n"  \
   "  n  r g b:LR:\n" \
   "where n is the node index, \n"  \
   "r, g, and b are the red, green and blue\n"  \
   "color values, respectively. \n"  \
   "Color values must be between 0 and 1.0. \n" \
   "A sample file would be: test.1D.col with content:LIT:\n"   \
   "   0    0.1 0.2 1   \n"   \
   "   1    0   1   0.8 \n"   \
   "   4    1   1   1   \n"   \
   "   7    1   0   1   \n"   \
   "   14   0.7 0.3 0   "


#define SUMA_SurfContHelp_SelInt \
"Use this menu to select which column (:term:`sub-brick`) in the "   \
"dataset (Dset) should be used for an Intensity (I)"   \
"measure.\n"   \
"\n"   \
"Values in (I) are the ones that get colored by the colormap," \
"however, no coloring is done if the :ref:`'v'<SurfCont->Dset_Mapping->I->v>` button on the right is"   \
"turned off.\n"   \
"\n"   \
"The (I) value for the selected :term:`datum` (n) is shown in the :ref:`'Val'<SurfCont->Xhair_Info->Val.c00>` table"\
"of the :ref:`'Xhair Info' <SurfCont->Xhair_Info>` section on the left.\n"\
"The value is also shown in the SUMA viewer\n"\
"\n"   \
"You can use a different type of selector to set (I). "  \
"A right-click on 'I' opens a list widget, which is better " \
"when you have many columns from which to choose.\n" \
"\n"  \
"The style of this selector can also change depending on the number"\
"of sub-bricks (columns) you have in your dataset. If the number"\
"exceeds a threshold specified by the environment variable "\
":ref:`SUMA_ArrowFieldSelectorTrigger<SUMA_ArrowFieldSelectorTrigger>`\n"

#define SUMA_SurfContHelp_SelThr \
"Use this menu to select which column (:term:`sub-brick`) in the\n"   \
"dataset (Dset) should be used for a Threshold (T) "   \
"measure.\n"   \
"\n"   \
"T values are the ones used to determine if a :term:`datum` "   \
"gets colored based on its (I) value.\n"   \
"\n"   \
"A :term:`datum` n is not colored if::LR:\n"   \
"    T(n)   < Tscale   :LR:\n"   \
"or if :ref:`'\\|T\\|'<SurfCont->Dset_Mapping->abs_T>` option below "   \
"is turned ON: :LR:\n"   \
"  | T(n) | < Tscale .\n"  \
"\n"   \
"Thresholding is not applied when the :ref:`'v'<SurfCont->Dset_Mapping->T->v>` button on the right is turned off.\n" \
"\n"   \
"The (T) value for the selected :term:`datum` (n) is shown in the :ref:`'Val'<SurfCont->Xhair_Info->Val.c00>` table"\
"of the :ref:`'Xhair Info' <SurfCont->Xhair_Info>` section on the left.\n"\
"The value is also shown in the SUMA viewer\n"\
"\n"   \
"You can use a different type of selector to set (T). "  \
"A right-click on 'T' opens a list widget, which is better " \
"when you have many columns from which to choose.\n" \
"\n" \
"The style of this selector can also change depending on the number "\
"of sub-bricks (columns) you have in your dataset. If the number "\
"exceeds a threshold specified by the environment variable "\
":ref:`SUMA_ArrowFieldSelectorTrigger<SUMA_ArrowFieldSelectorTrigger>`\n"

   #define SUMA_SurfContHelp_SelBrt \
"Use this menu to select which column (:term:`sub-brick`) in the "\
"dataset (Dset) should be used for color Brightness (B) modulation.\n"   \
"\n"   \
"The (B) values are the ones used to control the brightness of a :term:`datum's<datum>` color.\n"  \
"\n"   \
"Brightness modulation is controlled by ranges in the 'B' cells of the "\
"table below.\n"   \
"\n"   \
"Brightness modulation is not applied when the :ref:`'v'<SurfCont->Dset_Mapping->B->v>` button on \n"   \
"the right is turned off.\n"  \
"\n"   \
"The (B) value for the selected :term:`datum` (n) is shown in the :ref:`'Val'<SurfCont->Xhair_Info->Val.c00>` table"\
"of the :ref:`'Xhair Info' <SurfCont->Xhair_Info>` section on the left.\n"\
"The value is also shown in the SUMA viewer\n"\
"\n"   \
"You can use a different type of selector to set (B). "  \
"A right-click on 'B' opens a list widget, which is better " \
"when you have many columns from which to choose.\n" \
"\n" \
"The style of this selector can also change depending on the number"\
"of sub-bricks (columns) you have in your dataset. If the number"\
"exceeds a threshold specified by the environment variable "\
":ref:`SUMA_ArrowFieldSelectorTrigger<SUMA_ArrowFieldSelectorTrigger>`\n"

#define SUMA_SurfContHelp_SelIntTgl \
   "View (ON)/Hide Dset node colors"

#define SUMA_VolContHelp_SelIntTgl \
   "View (ON)/Hide Dset voxel colors"

#define SUMA_GraphContHelp_SelIntTgl \
   "View (ON)/Hide Dset edge colors"

#define SUMA_SurfContHelp_SelThrTgl \
   "Apply (ON)/Ignore thresholding"
   
#define SUMA_SurfContHelp_SelBrtTgl \
   "View (ON)/Ignore brightness modulation"

#define SUMA_SurfContHelp_ShowSliceTgl \
   "View (ON)/Hide slice"
#define SUMA_SurfContHelp_ShowVrFTgl \
   "View (ON)/Hide Volume Rendering"
#define SUMA_SurfContHelp_VrSelectTgl \
   "When ON, allow voxel selection on volume rendering."

#define SUMA_SurfContHelp_SetRngTbl_r0 \
   "Used for setting the clipping ranges. "   \
   "Clipping is only done for \n"   \
   "color mapping. Actual data \n"   \
   "values do not change."

#define SUMA_SurfContHelp_SetRngTbl_r1 \
"Intensity clipping range rules::LR:\n" \
"   Values in the intensity data "   \
"that are less than Min are colored "   \
"by the first (bottom) color of the "   \
"colormap. :LR:\n"   \
"   Values larger than Max are mapped "   \
"to the top color.:LR:\n"   \
"   Intermediate values are mapped according to the :ref:`'Col'<SurfCont->Dset_Mapping->Col>` menu below.\n"\
"\n"\
"You can set the range as a percentile of the dataset's values by appending "\
" '%' to the percentile for Min and/or Max such as 5% or 90%. Note that "\
"the percentile always gets replaced by the actual value in the dataset.\n"  \
"\n"   \
"A left-click on 'I' locks ranges from automatic resetting, and the locked "\
"range applies to the current Dset only. A locked range is indicated with the "\
"reverse video mode.\n"   \
"\n"   \
"A right-click resets values to the default range (usually 2% to 98%) in the dataset."

#define SUMA_SurfContHelp_SetRngTbl_r2 \
"Values in the brightness (B) :ref:`column<SurfCont->Dset_Mapping->B>` "\
"are clipped to the Min to Max range in this row before calculating "\
"their modulation factor per the values in the next table row.\n"   \
"\n"   \
"You can set the range as a percentile of the dataset's values by appending "\
" '%' to the percentile for Min and/or Max such as 8% or 75%. Note that "\
"the percentile always gets replaced by the actual value in the dataset.\n"  \
"\n"   \
"A left-click locks ranges in this row from automatic resetting, "   \
"and a locked range is applied to the current Dset only. A locked "\
"range is indicated with the reverse video mode.\n" \
"\n"   \
"A right-click resets values to the default range (usually 2% to 98%) for the dataset."   

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
   "  if v < min then v = min \n\n"\
"You can also set the range as a percentile of the dataset's values by  "\
"appending '%' to the percentile such as 5% or 90%. Note that "\
"the percentile always gets replaced by the actual value in the dataset."

#define SUMA_SurfContHelp_SetRngTbl_c2 \
   "Maximum clip value.\n" \
   "Clips values (v) in the Dset\n" \
   "larger than Maximum (max):\n"  \
   "  if v > max then v = max \n\n" \
"You can also set the range as a percentile of the dataset's values by  "\
"appending '%' to the percentile such as 5% or 90%. Note that "\
"the percentile always gets replaced by the actual value in the dataset."


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
   "Switch between modes for mapping values to the color map.:LR:\n"   \
   "The bottom color of the map C0 maps to the minimum value in the "\
   ":ref:`I range<SurfCont->Dset_Mapping->SetRangeTable.r01>` row, "\
   "and the top color to the maximum value. Colors for values in between "\
   "the minimum and maximum of "\
   ":ref:`I range<SurfCont->Dset_Mapping->SetRangeTable.r01>`, the following "\
   "methods apply:LR:\n"\
   "Int: Interpolate linearly between\n"   \
   ":   :colors in colormap to find color at:LR:\n"\
   ":   :   icol=((V-Vmin)/Vrange * Ncol) :LR:\n"   \
   "NN : Use the nearest color in the\n"   \
   ":   :colormap. The index into the colormap\n"\
   ":   :of Ncol colors is given by :LR:\n"\
   ":   :   icol=floor((V-Vmin)/Vrange * Ncol) :LR:\n"   \
   ":   :with icol clipped to the range 0 to Ncol-1:LR:\n"   \
   "Dir: Use intensity values as indices\n"   \
   ":   :into the colormap.\n"   \
   ":   :In Dir mode, the intensity \n"   \
   ":   :clipping range is of no use.:LR:\n" \
   ":   :   icol=floor(V) with clipping to the range 0 to Ncol-1"

#define SUMA_SurfContHelp_Link \
"Switch between methods for the automatic linking of I, T selectors.:LR:\n"   \
"  None: Do nothing.:LR:\n"   \
"  Same: Set the T selector to match the I selection.:LR:\n"  \
"  Stat: Switch T selector to match an I selection with \n"\
":       :an obvious statistic. Matching is based on labels.:LR:\n"\
"You can set your preference using environment variable\n"\
"   SUMA_IxT_LinkMode\n"

#define SUMA_SurfContHelp_Bias \
   "Coordinate bias direction.:LR:\n"   \
   ": :-: No bias thank you:LR:\n"   \
   ": :x: X coord bias:LR:\n"   \
   ": :y: Y coord bias:LR:\n"   \
   ": :z: Z coord bias:LR:\n"   \
   ": :n: bias along node's normal:LR:\n"  \
   "\n"   \
   "See more info in Bhelp for\n"   \
   "'C' table entry above.\n" \
   "\n"   \
   "This option will produce\n"   \
   "'Extremely Cool'[1] images.\n"   \
   "[1] Chuck E. Weiss (Slow River/" \
   "Rykodisc) 1999."

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
   "A sample colormap would be:LIT:\n"   \
   " 0 0 1\n"   \
   " 1 1 1\n"   \
   " 1 0 0:LR:\n"   \
   "saved into a cmap file called\n"   \
   "cmap_test.1D.cmap" \
   "\n"  \
   "See also envs :ref:`SUMA_CmapsDir<SUMA_CmapsDir>`, "\
   ":ref:`SUMA_RetinoAngle_DsetColorMap<SUMA_RetinoAngle_DsetColorMap>` "\
   "and :ref:`SUMA_VFR_DsetColorMap<SUMA_VFR_DsetColorMap>`"
   

   #define  SUMA_SurfContHelp_AbsThr   \
"Toggle Absolute thresholding.:LR:\n"   \
"   OFF: Mask color for\n"   \
":      ::term:`datum` (nodes, edges, voxels, etc.) that have::LR:\n"   \
"      T(n) < Tscale:LR:\n"   \
"   ON:  Mask color for\n"   \
":      ::term:`datum` that have::LR:\n"   \
"      | T(n) | < Tscale\n\n"   \
"where::LR:\n"   \
"     Tscale is the value set by the threshold scale.:LR:\n"   \
"     T(n) is the :term:`datum` value in the selected threshold column (T).\n"  \
":         :This value is seen in the second cell of the 'Value'\n"   \
":         :table on the left side."  

#define  SUMA_SurfContHelp_Isym   \
   "Toggle Intensity range symmetry "   \
   "about 0.:LR:\n"   \
   "   ON : Intensity clipping range\n"   \
   ":     :is forced to go from \n"   \
   ":     :-val to val.\n"   \
   ":     :This allows you to mimic\n"   \
   ":     :AFNI's ranging mode.:LR:\n"   \
   "  OFF: Intensity clipping range\n"   \
   ":     :can be set to your liking."

#define  SUMA_SurfContHelp_Shw0   \
   "Toggle color masking of nodes "   \
   "with intensity = 0 :LR:\n"   \
   "  ON : 0 intensities are mapped\n"   \
   ":     :to the colormap as any\n"   \
   ":     :other values.:LR:\n"   \
   "  OFF: 0 intensities are masked,\n"   \
   ":     :a la AFNI"

#define  SUMA_SurfContHelp_RangeTbl_c0 \
   "Full range of values in Dset"
           
#define SUMA_SurfContHelp_RangeTbl_c1 \
   "Minimum value in Dset column"

#define SUMA_SurfContHelp_RangeTbl_c2 \
   "Node index at minimum.\n"   \
   "Right click in cell to\n"   \
   "have crosshair jump to\n"   \
   "node's index.\n"   \
   "Same as 'ctrl+j' or\n"   \
   "an entry in the 'Node' cell\n"   \
   "under Xhair Info block."  

#define SUMA_GraphContHelp_RangeTbl_c2 \
   "Edge index at minimum.\n"   \
   "Right click in cell to\n"   \
   "have crosshair jump to\n"   \
   "edge's index.\n"   \
   "Same as 'ctrl+j' or\n"   \
   "an entry in the 'Edge' cell\n"   \
   "under Xhair Info block."  

#define SUMA_SurfContHelp_RangeTbl_c3 \
   "Maximum value in Dset column"

#define SUMA_SurfContHelp_RangeTbl_c4  \
   "Node index at maximum.\n" \
   "Right click in cell to\n" \
   "have crosshair jump to\n" \
   "node's index.\n"   \
   "Same as 'ctrl+j' or\n"   \
   "an entry in the 'Node' cell\n"   \
   "under Xhair Info block."

#define SUMA_GraphContHelp_RangeTbl_c4  \
   "Edge index at maximum.\n" \
   "Right click in cell to\n" \
   "have crosshair jump to\n" \
   "edge's index.\n"   \
   "Same as 'ctrl+j' or\n"   \
   "an entry in the 'Edge' cell\n"   \
   "under Xhair Info block."
  
#define SUMA_SurfContHelp_RangeTbl_r1  \
   "Range of values in intensity (I) column"

#define SUMA_SurfContHelp_RangeTbl_r2  \
   "Range of values in threshold (T) column"

#define SUMA_SurfContHelp_RangeTbl_r3  \
   "Range of values in brightness (B) column"

#define  SUMA_SurfContHelp_MaskTypeTbl_c0 \
   "Add one more row for a mask ROI"
           
#define SUMA_SurfContHelp_MaskTypeTbl_c1 \
   "String label of ROI"

#define SUMA_SurfContHelp_MaskTypeTbl_c05 \
   "Variable symbol. Choose from 'a' to 'z'"

   #define SUMA_SurfContHelp_MaskTypeTbl_c2 \
"Type of mask. For the moment, this string can only be one of 'box' or 'sphere'."

   #define SUMA_SurfContHelp_MaskTypeTbl_c3 \
"Set/Get coordinates in mm :term:`RAI` of the center of the mask :LR:\n"   \
"You can right click in cell to get back to the original center.:LR:\n" \
"You can also reposition the mask interactively in the SUMA viewer "\
"by :ref:`selecting <Button_3-Press>` something, if you are in "\
":ref:`Mask Manipulation Mode<Mask_Manipulation_Mode>`."
   
#define SUMA_SurfContHelp_MaskTypeTbl_c4 \
"Set/Get size along three dimensions of mask. You can enter a single value "\
"if the all three dimensions are equal. :LR:\n" \
"Right click in cell to get back to the original size.:LR:\n" \
"Resizing in SUMA viewer can be done with :ref:`Ctrl+scroll<Ctrl+Scroll>` "\
"if you are in :ref:`Mask Manipulation Mode<Mask_Manipulation_Mode>`.:LR:\n"\
"You can also change values by scrolling with mouse pointer over the cell.\n" 

   #define SUMA_Switch_Cont_BHelp   \
"Switch to controllers of other objects. You can use the arrows, or set the "\
"controller's index directly."

   #define SUMA_SurfContHelp_AllObjs   \
"Initialize controllers for all objects that have one. "\
"This is particularly useful when a particular may not be visible under "\
"the default settings."

   #define SUMA_SurfContHelp_MaskTypeTbl_c5  \
"Color of mask in RGB triplets between 0 and 1.0. You can also specify "\
"colors using the shorthands of::LR:\n"\
"   'b' or 'blue':LR:\n"   \
"   'g' or 'green':LR:\n"  \
"   'p' or 'pink':LR:\n"   \
"   'r' or 'red':LR:\n" \
"   'w' or 'white':LR:\n"  \
"   'y' or 'yellow':LR:\n" \
"The final color depends also on the dim factor 'D'"

   #define SUMA_SurfContHelp_MaskTypeTbl_c6  \
"Alpha of mask color. The Alpha value controls the contribution of an ROI's"\
"color to the tracts that pass through it. This tinting process is only "\
"used when 'Mask Eval' is in use, and when A is > 0. See the help for "\
"'Mask Eval' for information on how tinting works.:LR:\n"\
"You can also change values by scrolling with mouse pointer over the cell.\n" 

   #define SUMA_SurfContHelp_MaskTypeTbl_c7  \
"Transparency of mask. A value of 0 renders a mask opaque. Consider using "\
"lower D values to avoid color saturation of rendered masks.:LR:\n"\
"You can also change values by scrolling with mouse pointer over the cell.\n" 
 

#define SUMA_SurfContHelp_MaskTypeTbl_c8  \
   "Dimming factor for color. Saturated colors may not look nice when rendered, so consider using the D parameter to dim a color's brightness without having to so directly in the color column. Setting D to 6 for example will scale a color by a factor of 6/9, so a saturated red of [1 0 0] becomes [0.67 0 0 ]. This makes masks render better when not in transparent mode T = 0.:LR:\n"\
"You can also change values by scrolling with mouse pointer over the cell.\n" 

   #define SUMA_SurfContHelp_MaskTypeTbl_r1  \
"Delete row of mask ROI. You have to click twice to get rid of a row, so there "\
"is no undo for you. After the 1st click, the 'x' turn big 'X' and a new click "\
"on big 'X' deletes the row. If you fail to click on big 'X' or simply change "\
"your mind, the operation is canceled and big 'X' is little 'x' again. YAY!"

   #define SUMA_SurfContHelp_EvalMaskExpr_r0 \
"A boolean expression evaluated per tract to determine whether or not a tract "\
"should be displayed. Each mask is assigned a letter from 'a' to 'z' and has "\
"an entry in the table below. Symbols for the OR operator are '|' or '+' "\
"while those for AND are '&' or '*'. The '|' is for the NOT operation. By "\
"default, the expression is blank, as indicated by '-', and the operation is "\
"an OR of all the masks.\n\n" \
":SPX:.. _Tract_Tinting:\n\n"\
":NOF:Tract Tinting:\n"\
":NOF:^^^^^^^^^^^^^^\n\n"\
":DEF:"\
"Tract_Tinting:\n"\
"^^^^^^^^^^^^^^\n"\
":SPX:"\
"Tracts that go through any of the masks are displayed and they keep their own color:SPX:, as shown in the figure below to the left:SPX:.\n\n"\
"Say we now want to show tracts that go through both masks b and c or through "\
"mask a. The expression to evaluate at each tract would be: '( b & c ) | a'. "\
"Note that for the expression to take effect, you need to have the "\
":ref:`v button<MaskCont->Masks->Mask_Eval->v>` selected.\n\n"\
":SPX:\n\n"\
"   .. figure:: media/MaskedTracts.01.jpg\n"\
"      :align: left\n"\
"      :figwidth: 30%\n"\
"      :name: media/MaskedTracts.01.jpg\n\n"\
"      :ref:`Tracts going through any of the three masks.<media/MaskedTracts.01.jpg>`\n\n"\
"   .. figure:: media/MaskedTracts.02.jpg\n"\
"      :align: right\n"\
"      :figwidth: 30%\n"\
"      :name: media/MaskedTracts.02.jpg\n\n"\
"      Tracts evaluating to true per expression: '( b & c ) | a'. :ref:`(link)<media/MaskedTracts.02.jpg>`\n\n"\
"   .. figure:: media/MaskController.02.jpg\n"\
"      :align: center\n"\
"      :name: media/MaskController.02.jpg\n"\
"      :figwidth: 30%\n\n"\
"      :ref:`Mask Controller.<media/MaskController.02.jpg>`\n\n"\
SUMA_SHPINX_BREAK \
":SPX:"  \
"When using the the Mask Eval expression, the color of tracts that go though a set of regions is equal to the alpha weighted average of the colors of those regions.:SPX: This can be seen in the figure on the right side above.\n\n"\
"The colors of a tract is given by:LIT:\n"\
"   Ct = sum(AiCi)/sum(Ai)\n\n"\
"for all ROIs i the tract intersects.\n\n"   \
"For example, say a tract goes through a blue region of color [0 0 1] with alpha of 0.5 (A ~ 5 in column A), and a red region of color [1 0 0] (alpha is 1.0, or in the table = 9). The tracts that go through both ROIs will be colored (1.0*([1 0 0]+0.5*([0 0 1])/1.5, which is purple. Similar averaging goes on if tracts go through more than 2 regions. Tracts that go though one region will get that region's color.\n\n"\
"Now, if you set alpha to 0 for a certain ROI, then that ROI does add to the "\
"tint of tracts that go thourough it at all. And for a tract that goes through that region only, it retains its original colors.:SPX: See image on the right side.:SPX:\n\n"\
":SPX:\n\n"\
"  .. figure:: media/Masks.02.jpg\n"\
"     :align: left\n"\
"     :name: media/Masks.02.jpg\n"\
"     :figwidth: 30%\n\n"\
"     Tracts going through any of 2 masks 'a|b', with 'Mask Eval' ON. :ref:`(link)<media/Masks.02.jpg>`\n\n"\
"  .. figure:: media/Masks.03.jpg\n"\
"     :align: right\n"\
"     :name: media/Masks.03.jpg\n"\
"     :figwidth: 30%\n\n"\
"     Tracts going through 'a|b' but with alpha of ROI 'a' - the blue one - set to 0. Tracts going through the blue ROI are not tinted by it at all. :ref:`(link)<media/Masks.03.jpg>`\n\n"\
"  .. figure:: media/Masks.00.jpg\n"\
"     :align: center\n"\
"     :name: media/Masks.00.jpg\n"\
"     :figwidth: 30%\n\n"\
"     Mask Controller settings for image to the left. :ref:`(link)<media/Masks.00.jpg>`\n\n"\
SUMA_SHPINX_BREAK \
":SPX:"  \

   #define SUMA_SurfContHelp_DistMask_r0 \
"Set Min Max length for tract masking. Use can scroll (mouse wheel) in Min "\
"and Max cells to change the value. The 'v' button must be selected for "\
"masking to take effect."
   
#define SUMA_SurfContHelp_GDSET_ViewBundles \
   "Show bundles instead of edges between nodes if \n"\
   "the graph dataset contains such information. For\n"\
   "the moment, only 3dProbTrackID creates such data."\
":SPX:\n\n"\
".. figure:: media/Graph3D.jpg\n"\
"   :align: left\n"\
"   :name: media/Graph3D.jpg\n"\
"   :figwidth: 30%\n\n"\
"   :ref:`Graph shown in 3D. <media/Graph3D.jpg>` Edges represented by straight lines.\n\n"\
".. figure:: media/Graph3D_Bundles.jpg\n"\
"   :align: right\n"\
"   :name: media/Graph3D_Bundles.jpg\n"\
"   :figwidth: 30%\n\n"\
"   Graph shown in 3D. :ref:`Edges represented by bundles <media/Graph3D_Bundles.jpg>` derived from.\n"\
"   tractography with 3dTrackID. See :ref:`FATCAT_DEMO` for details.\n\n"\
SUMA_SHPINX_BREAK \
"Figures were generated using :ref:`FATCAT_DEMO` output with::\n\n"\
"   suma -vol mprage+orig. -gdset DTI/o.NETS_AND_000.niml.dset &\n\n"\
":SPX:\n\n"\
"Bundle colors reflect the value of the edge connecting the two nodes\n\n"\
"Selection is identical to when edges are represented by straight lines.\n\n"
 
#define SUMA_SurfContHelp_GDSET_ViewUncon \
   "Show graph nodes even if unconnected to other nodes.\n"

#define  SUMA_SurfContHelp_Mask  \
"Opens controller for masks.\n" \
"At the first click, this button creates a new interactive tract mask and activates menu items such as :ref:`Gry<TractCont->Coloring_Controls->Gry>`. A ball of a mask is added to the interface, and only tracts that go through it are displayed. \n"\
":SPX:"\
"\n"\
".. figure:: media/MaskButtonInController.jpg\n"\
"   :align: center\n"\
"   :name: media/MaskButtonInController.jpg\n"\
"\n"\
"   :ref:`(link)<media/MaskButtonInController.jpg>`\n\n"\
":SPX:"\
"Clicking on Masks after the initialization brings up the "\
":ref:`Mask Controller<MaskCont>`. See :ref:`mask manipulation"\
" mode<Mask_Manipulation_Mode>` for details on how to move the mask around.\n"

/* this one's based on AFNI's func->thr_pval_label help */
#define SUMA_SurfContHelp_ThreshStats  \
   "Shows the estimated significance\n"   \
   "(p-value) of the threshold above,\n"  \
   "if possible.:LR:\n"  \
   "   * If not possible, will display as '[N/A]' instead.:LR:\n" \
   "   * p's that display as 1.2-7 should be interpreted as 1.2 x 10^(-7):LR:\n"\
   "   * p-value here is significance PER NODE/VOXEL/etc.:LR:\n" \
   "* If FDR curves are pre-computed in the dataset's header, then the False "\
   "Discovery Rate q-value will also be shown.:LR:\n"     \
   "* You can add FDR curves to a dataset with '3drefit -addFDR'.\n"   
   
   #define SUMA_SurfContHelp_ColorBar  \
"Colorbar used for colorizing values in 'I' sub-brick.\n"   \
"Colorization depends on the settings under the "\
":ref:`I<SurfCont->Dset_Mapping->I>`, :ref:`Range "\
"Setting<SurfCont->Dset_Mapping->SetRangeTable.r01>`, among other things. "\
"Threshold settings determine whether or not a certain value will get "\
"displayed at all.:LR:\n"  \
"Use :SPX: *ctrl+h over the colorbar* :DEF: ctrl+h over the colorbar :SPX:"\
"for help on :ref:`manipulating the displayed "\
"map<Colormap_Keyboard_Controls>`.\n"    
   
#define SUMA_SurfContHelp_ThrScale  \
   "Set threshold value to determine which nodes/voxels/edges will get colored"\
   "Voxels for which the value in the 'T' sub-brick is below that of the "\
   "threshold will not get colored."
   
#endif
