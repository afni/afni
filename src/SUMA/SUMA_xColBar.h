#ifndef SUMA_XCOLBAR_INCLUDED
#define SUMA_XCOLBAR_INCLUDED

#define SUMA_SCALE_HEIGHT 310    
#define SUMA_SCALE_WIDTH 70
#define SUMA_CMAP_WIDTH    20
#define SUMA_CMAP_HEIGHT   300
#define SUMA_CMAP_ORIGIN   0.0,  0.0,     0.0
#define SUMA_CMAP_TOPLEFT  SUMA_CMAP_WIDTH, SUMA_CMAP_HEIGHT,   0.0
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

#define SUMA_INSERT_CELL_STRING(TF, i, j, strng)   {  \
   XtVaSetValues (TF->cells[j*TF->Ni+i], XmNvalue, strng, NULL);  \
}

#define SUMA_INSERT_CELL_VALUE(TF, i, j, val)   {  \
   if (TF->type == SUMA_int || TF->type == SUMA_float) { \
      TF->cell_modified = j*TF->Ni+i;  \
      TF->value = val;  \
      SUMA_TableF_SetString(TF);\
      TF->cell_modified = -1; \
   }  else {   \
      SUMA_SL_Err("Macro for numerical tables only"); \
   }  \
}


/* scale size gets messed up, see afni_widg.c and afni.h's
FIX_SCALE_SIZE*/
#define SUMA_FORCE_SCALE_HEIGHT(SO) {\
  XtVaSetValues(  SO->SurfCont->thr_sc, XmNheight,  SUMA_CMAP_HEIGHT, NULL ) ;   \
}

void SUMA_ShowMeTheChildren(Widget w);
XImage *SUMA_cmap_to_XImage (Widget wid, SUMA_COLOR_MAP *cm);
void SUMA_DrawCmap(SUMA_COLOR_MAP *Cmap);
void SUMA_cmap_wid_display(SUMA_SurfaceObject *SO);
void SUMA_cmap_context_Init(SUMA_SurfaceObject *SO);
void SUMA_cmap_wid_graphicsInit (Widget w, XtPointer clientData, XtPointer call);
Boolean SUMA_cmap_wid_handleRedisplay(XtPointer clientData);
void SUMA_cmap_wid_postRedisplay(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cmap_wid_expose(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cmap_wid_resize(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cmap_wid_input(Widget w, XtPointer clientData, XtPointer call);
unsigned char *SUMA_read_ppm(char *fname, int *width, int *height, int verb);
void SUMA_CreateCmapWidgets(Widget parent, SUMA_SurfaceObject *SO);
void SUMA_cb_ColMap_Switch(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cb_SwitchBrightness(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cb_SwitchThreshold(Widget w, XtPointer clientData, XtPointer call);
void SUMA_cb_SwitchIntensity(Widget w, XtPointer clientData, XtPointer call);
SUMA_MenuItem *SUMA_FreeMenuVector(SUMA_MenuItem *menu, int Nels);
SUMA_MenuItem *SUMA_FormSwitchColMenuVector(SUMA_SurfaceObject *SO, int what, int *N_items);
void SUMA_set_cmap_options(SUMA_SurfaceObject *SO, SUMA_Boolean NewDset, SUMA_Boolean NewMap);
void SUMA_cb_SwitchCmap(Widget w, XtPointer client_data, XtPointer call);
SUMA_MenuItem *SUMA_FormSwitchCmapMenuVector(SUMA_COLOR_MAP **CMv, int N_maps);
void SUMA_cb_SelectSwitchCmap (Widget w, XtPointer client_data, XtPointer call);
void SUMA_cb_CloseSwitchCmap (Widget w, XtPointer client_data, XtPointer call);
SUMA_Boolean SUMA_CmapSelectList(SUMA_SurfaceObject *SO, int type);
SUMA_Boolean SUMA_SwitchColPlaneCmap(SUMA_SurfaceObject *SO, SUMA_COLOR_MAP *CM);
SUMA_Boolean SUMA_SetCmapMenuChoice(SUMA_SurfaceObject *SO, char *str);
void SUMA_SetScaleRange(Widget w, float range[2]);  
void SUMA_cb_set_threshold_label(Widget w, XtPointer clientData, XtPointer call);
void SUMA_optmenu_EV( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch );
void SUMA_cb_SetCoordBias(Widget widget, XtPointer client_data, XtPointer call_data);
SUMA_Boolean SUMA_RedisplayAllShowing(char *SO_idcode_str, SUMA_SurfaceViewer *SVv, int N_SVv);
void SUMA_CreateTable(  Widget parent,
                        int Ni, int Nj, 
                        char **row_tit, char **col_tit, 
                        int cwidth, SUMA_Boolean editable, SUMA_VARTYPE type, 
                        void (*NewValueCallback)(void * data), void *cb_data,
                        void (*TitLabelCallback)(Widget w , XtPointer cd , XEvent *ev , Boolean *ctd), void *TitLabelCallbackData,
                        SUMA_TABLE_FIELD *TF);
void SUMA_TableF_cb_label_Modify (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_TableF_SetString (SUMA_TABLE_FIELD * AF);
void SUMA_TableF_cb_label_change (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_leave_TableField( Widget w , XtPointer client_data ,
                           XEvent * ev , Boolean * continue_to_dispatch );
void SUMA_SetRangeValue (void *data);
SUMA_TABLE_FIELD * SUMA_AllocTableField(void);
SUMA_TABLE_FIELD * SUMA_FreeTableField(SUMA_TABLE_FIELD *TF);
SUMA_CELL_VARIETY SUMA_cellvariety (SUMA_TABLE_FIELD *TF, int n);
SUMA_Boolean SUMA_InitRangeTable(SUMA_SurfaceObject *SO);


         
         
         

#endif
