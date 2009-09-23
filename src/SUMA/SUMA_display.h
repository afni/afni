#ifndef SUMA_DISPLAY_INCLUDED
#define SUMA_DISPLAY_INCLUDED

/* 

   Historical Note from Fri Jan  3 10:21:52 EST 2003:
   the method for hiding a surface viewer (and other controllers), used to have     three options prior to Fri Jan  3 10:21:52 EST 2003
   Now only SUMA_WITHDRAW and NOT SUMA_DESTROY should be used.

   As of Wed Sep 23 14:45:59 EDT 2009
   ----------------------------------
   On Mac OS X 10.5, and at least early 10.6, using XWithdrawWindow,
   followed by XMapRaised, SUMA would crash with a GLX_BadDrawable error.
   One way around this is to go back to XtUnrealizeWidget/XtRealizeWidget pair.
   It appears that GLX implementation is full of uninitialzation errors, 
   according to Valgrind. 
   
   So now one can choose between three closing modes. For widgers with GLX drawables (Viewers, and SurfaceControllers) use SUMA_UNREALIZE.
   For other, stick with SUMA_WITHDRAW as previously done.
*/

#define SUMA_DESTROY    1
#define SUMA_WITHDRAW   2
#define SUMA_UNREALIZE  3

#define SUMA_CLOSE_MODE       SUMA_WITHDRAW
#define SUMA_GL_CLOSE_MODE    SUMA_UNREALIZE

#define NO_FLUSH        1  /* 1 = Avoid flushing/Buffer swapping, at close time 
                                  Part of the effort to keep suma from crashing
                                  on OS X 10.5 , when closing a widget with an 
                                  OpenGL drawable */
                                  
typedef struct suma_menu_item {
    char        *label;         /*!< the label for the item */
    WidgetClass *class;         /*!< pushbutton, label, separator... */
    char         mnemonic;      /*!< mnemonic; NULL if none */
    char        *accelerator;   /*!< accelerator; NULL if none */
    char        *accel_text;    /*!< to be converted to compound string */
    void       (*callback)();   /*!< routine to call; NULL if none */
    XtPointer    callback_data; /*!< client_data for callback(). 
                                    This ends up being the index
                                    of the widget in the menu, in addition
                                    to being the callback value. 
                                    Specify it even if callback is NULL 
                                    because it is needed to index into 
                                    the vector of widgets. The callback value
                                    is actually a bit more complicated than what 
                                    is stored in callback_data. The call back ends
                                    up receiving a pointer to a structure of the 
                                    type SUMA_MenuCallBackData. That structure contains
                                    callback_data in a field of the same name, in addition
                                    to an identifier of the controller containing that menu.
                                    This way you can tell from which controller the menu
                                    had been activated.*/
    struct suma_menu_item *subitems; /*!< pullright menu items, if not NULL */
} SUMA_MenuItem; /*!< Structure to hold descriptions of menu items. 
                  This structure is mostly based on the build_option.c example 
                  of the "Motif Programming Manual". */

typedef struct {
   XtPointer callback_data; /*!< usually the item number in the menu */
   void *ContID; /*!< some identifier of the controller */
} SUMA_MenuCallBackData;/*!< a structure to help in the creation of menus. The main problem is that I may have the same 
menu item in different controllers and one needs a way to know from which controller the menu was
activated.
*/

/*!
   macro that converts a callback data from a File Menu widget into the 
   index of the viewer containing it and the widget's SUMA_WIDGET_INDEX_FILE type
*/
#define SUMA_VIEWER_FROM_FILEMENU_CALLBACK(data, isv, widtype) {\
         SUMA_MenuCallBackData *datap; \
         datap = (SUMA_MenuCallBackData *)data;  \
         isv = (int)datap->ContID; \
         widtype = (int)datap->callback_data; }
         

#define SUMA_VIEWER_FROM_VIEWMENU_CALLBACK(data, isv, widtype) {\
         SUMA_MenuCallBackData *datap; \
         datap = (SUMA_MenuCallBackData *)data;  \
         isv = (int)datap->ContID; \
         widtype = (int)datap->callback_data; }
/*!
sets the select color of the widget to its foreground color */         
#define SUMA_SET_SELECT_COLOR(m_w) {\
      Pixel m_fg_pix=0;  \
      XtVaGetValues (m_w, XmNforeground, &m_fg_pix, NULL);  \
      XtVaSetValues (m_w, XmNselectColor, m_fg_pix, NULL);  \
}

/*! set the string of a label , sa: SUMA_SET_LABEL_MAX*/
#define SUMA_SET_LABEL(m_w, m_s) {\
   /* XmStringCreateLocalized does not work well with \n chars */ \
   /*XmString m_str = XmStringCreateLocalized(m_s); */\
   XmString m_str = XmStringCreateLtoR (m_s, XmSTRING_DEFAULT_CHARSET); \
   XtVaSetValues (m_w, XmNlabelString, m_str, NULL); \
   XmStringFree (m_str);   \
}

/*! set the string of a label, use a maximum of m_max chars*/
#define SUMA_SET_LABEL_MAX(m_w, m_s, m_max) {\
   char m_tmp = '\0'; \
   XmString m_str ;   \
   if (strlen(m_s) >= m_max) { m_tmp = m_s[m_max-1]; m_s[m_max-1] = '\0'; } \
   /* XmStringCreateLocalized does not work well with \n chars */ \
   /* m_str = XmStringCreateLocalized(m_s); */ \
   m_str = XmStringCreateLtoR (m_s, XmSTRING_DEFAULT_CHARSET); \
   XtVaSetValues (m_w, XmNlabelString, m_str, NULL); \
   XmStringFree (m_str);   \
   if (m_tmp != '\0') m_s[m_max-1] = m_tmp;  \
}

#define SUMA_SET_TEXT_FIELD(m_w, m_s) {\
   XtVaSetValues (m_w, XmNvalue, m_s, NULL); \
}

/*! m_s is a char *. Do not allocate space for it, do not free it afterwards 
*/
#define SUMA_GET_TEXT_FIELD(m_w, m_s) {\
   void *n=NULL; \
   XtVaGetValues (m_w, XmNvalue, &n, NULL); \
   m_s = (char *)n;  \
}

#define SUMA_SET_GL_PROJECTION(csv, ortho) {  \
   if (!ortho) { \
      if (LocalHead) \
         fprintf (SUMA_STDOUT,\
                  "%s: Setting up matrix mode and perspective ...\n", \
                  FuncName); \
      glMatrixMode (GL_PROJECTION); \
      glLoadIdentity ();   \
      gluPerspective((GLdouble)csv->FOV[csv->iState], csv->Aspect, \
                     SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); \
                     /*lower angle is larger zoom,*/   \
   }  else { \
      GLdouble m_sz = \
         0.5 *tan(SUMA_PI * csv->FOV[csv->iState] / 180.0) * \
         csv->GVS[csv->StdView].ViewFrom[2];  \
      GLdouble m_szx = m_sz * csv->Aspect;   \
      GLdouble m_szy = m_sz ;   \
      if (LocalHead) \
         fprintf (SUMA_STDOUT,\
                  "%s: Setting up matrix mode and orthographic projection "\
                  "(m_szx = %g, m_szy=%g)...\n", FuncName, m_szx, m_szy); \
      glMatrixMode (GL_PROJECTION); \
      glLoadIdentity ();   \
      glOrtho( -m_szx, m_szx, \
               -m_szy, m_szy, \
               SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); /*lower angle is  \
                                                               larger zoom,*/   \
   }  \
}

#define SUMA_SET_GL_MODELVIEW(csv) {   \
   if (LocalHead) {  \
      int m_i; \
      fprintf(stdout,"Translation Vector: %f %f\n", \
         csv->GVS[csv->StdView].translateVec[0], \
         csv->GVS[csv->StdView].translateVec[1]); \
      fprintf(stdout,"Rotation Matrix:\n");  \
      for (m_i=0; m_i<4; ++m_i){ fprintf(stdout, "%f\t%f\t%f\t%f\n",   \
         rotationMatrix[m_i][0], rotationMatrix[m_i][1], \
         rotationMatrix[m_i][2], rotationMatrix[m_i][3]); }   \
   }  \
   glMatrixMode(GL_MODELVIEW);   \
   glPushMatrix();   \
   glTranslatef ( csv->GVS[csv->StdView].translateVec[0], \
                  csv->GVS[csv->StdView].translateVec[1], 0.0);   \
   glTranslatef ( csv->GVS[csv->StdView].RotaCenter[0], \
                  csv->GVS[csv->StdView].RotaCenter[1], \
                  csv->GVS[csv->StdView].RotaCenter[2]); \
   glMultMatrixf(&rotationMatrix[0][0]);  \
   glTranslatef (-csv->GVS[csv->StdView].RotaCenter[0], \
                  -csv->GVS[csv->StdView].RotaCenter[1], \
                  -csv->GVS[csv->StdView].RotaCenter[2]); \
}   


#define SUMA_MARGIN  1

String *SUMA_get_fallbackResources ();         
void SUMA_CullOption(SUMA_SurfaceViewer *, const char *action);
Boolean SUMA_handleRedisplay (XtPointer w);
void SUMA_postRedisplay(Widget w, XtPointer clientData, XtPointer call);
GLenum SUMA_index_to_clip_plane(int iplane) ;
void SUMA_display(SUMA_SurfaceViewer *csv, SUMA_DO *dov);
Colormap SUMA_getShareableColormap_Eng(XVisualInfo * vi, Display *dpy);
Colormap SUMA_getShareableColormap(SUMA_SurfaceViewer * csv);
void SUMA_graphicsInit(Widget w, XtPointer clientData, XtPointer call);
void SUMA_expose(Widget w, XtPointer clientData, XtPointer call);
void SUMA_resize(Widget w, XtPointer clientData, XtPointer call);
SUMA_Boolean SUMA_X_SurfaceViewer_Create (void);
void SUMA_ButtOpen_pushed (Widget w, XtPointer cd1, XtPointer cd2);
void SUMA_ButtClose_pushed (Widget w, XtPointer cd1, XtPointer cd2);
int SUMA_generateEPS(char *filename, int inColor, unsigned int width, unsigned int height);
GLvoid *SUMA_grabPixels(int inColor, unsigned int width, unsigned int height);
SUMA_Boolean SUMA_RenderToPixMap (SUMA_SurfaceViewer *csv, SUMA_DO* dov);
void SUMA_context_Init(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_NormScreenToWorld(SUMA_SurfaceViewer *sv, 
                                    double xn, double yn, 
                                    GLdouble *pfront, GLdouble *pback);
SUMA_Boolean SUMA_GetSelectionLine (SUMA_SurfaceViewer *sv, int x, int y, 
                                    GLdouble *Pick0, GLdouble *Pick1, 
                                    int N_List, int *xList, int *yList, 
                                    GLdouble *Pick0List);
int SUMA_viewSurfaceCont(Widget w, SUMA_SurfaceObject *SO, SUMA_SurfaceViewer *sv);
void SUMA_cb_viewSurfaceCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_viewViewerCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_toggle_crosshair(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_toggle_node_in_focus(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_toggle_selected_faceset(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_viewSumaCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_createSumaCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_closeSumaCont(Widget w, XtPointer data, XtPointer callData);
Widget SUMA_GetTopShell(Widget w);
void SUMA_cb_createViewerCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_closeViewerCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_XHlock_toggled(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_XHviewlock_toggled(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_closeSurfaceCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_createSurfaceCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_newSumaCont(Widget w, XtPointer client_data, XtPointer callData);
void  SUMA_cb_doneSumaCont(Widget wcall, XtPointer cd1, XtPointer cbs);
void SUMA_quit_timeout_CB( XtPointer client_data , XtIntervalId * id );
void SUMA_set_Lock_rb (SUMA_rb_group * Lock_rbg, int irb, int but);
void SUMA_set_Lock_arb (SUMA_rb_group * Lock_rbg);   
void SUMA_cb_XHaviewlock_toggled (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_XHalock_toggled (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_set_LockView_atb (void);
int SUMA_BuildMenu(Widget parent, int menu_type, char *menu_title, char menu_mnemonic, \
                     SUMA_Boolean tear_off, SUMA_MenuItem *items, void *ContID, 
                     char *hint, char *help,
                     Widget *MenuWidgets);
void SUMA_cb_FileOpenSpec (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileOpenSurf (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileClose (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileSaveView (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileLoadView (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_moreSumaInfo (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_moreSurfInfo (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_moreViewerInfo (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_ViewerInfo_destroyed (void *p);
void SUMA_ViewerInfo_open (void *p);
void SUMA_SumaInfo_destroyed (void *p);
void SUMA_SumaInfo_open (void *p);
SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTextShellStruct (void (*opencallback)(void *data), void *opendata, 
                                                            void (*closecallback)(void*data), void *closedata);
SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTextShell (char *s, char *title, SUMA_CREATE_TEXT_SHELL_STRUCT *TextShellStruct);
void SUMA_cb_search_text(Widget widget, XtPointer client_data, XtPointer call_data);
void SUMA_DestroyTextShell (Widget w, XtPointer ud, XtPointer cd);
void SUMA_SurfInfo_open (void *SO);
void SUMA_SurfInfo_destroyed (void *SO);
void SUMA_cb_ToggleCaseSearch (Widget widget, XtPointer client_data, XtPointer call_data);
void SUMA_cb_helpUsage (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpIO_notify(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpMemTrace(Widget w, XtPointer data, XtPointer callData);
char * SUMA_FormatMessage (SUMA_MessageData *MD);
void SUMA_PopUpMessage (SUMA_MessageData *MD);
void SUMA_cb_helpMessageLog (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpSUMAGlobal (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpViewerStruct (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpSurfaceStruct (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_SetRenderMode(Widget widget, XtPointer client_data, XtPointer call_data);
void SUMA_cb_ToolsDrawROI (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_cb_CloseDrawROIWindow(Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_CreateDrawROIWindow(void);
SUMA_Boolean SUMA_InitializeDrawROIWindow (SUMA_DRAWN_ROI *DrawnROI);
SUMA_Boolean SUMA_OpenDrawROIWindow (SUMA_DRAWN_ROI *DrawnROI);
SUMA_Boolean SUMA_OpenDrawROIController(SUMA_SurfaceViewer *sv);
void SUMA_cb_DrawROImode_toggled (Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_ContROImode_toggled (Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_DrawROIPen_toggled (Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_AfniLink_toggled (Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_DrawROI_Undo (Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_DrawROI_Redo (Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_DrawROI_Save (Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_DrawROI_Load (Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_DrawROI_setlabel (Widget w, XtPointer data, XtPointer client_data);
void SUMA_CreateArrowField ( Widget pw, char *label,
                              float value, float vmin, float vmax, float vstep,
                              int cwidth, SUMA_VARTYPE type,
                              SUMA_Boolean wrap,
                              void (*NewValueCallback)(void * data), void *cb_data,
                              char *hint, char *help,
                              SUMA_ARROW_TEXT_FIELD *AF);
void SUMA_CreateTextField ( Widget pw, char *label,
                              int cwidth, 
                              void (*NewValueCallback)(void *data),
                              char *hint, char *help,
                              SUMA_ARROW_TEXT_FIELD *AF);
void SUMA_DrawROI_NewLabel (void * data);
void SUMA_ATF_change_value (XtPointer client_data, XtIntervalId *id);
void SUMA_ATF_start_stop (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_DrawROI_NewValue (void * data);
void SUMA_ATF_cb_label_change (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_ATF_SetString (SUMA_ARROW_TEXT_FIELD * AF);
void SUMA_ATF_SetValue (SUMA_ARROW_TEXT_FIELD * AF);
void SUMA_ATF_cb_label_Modify (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_leave_EV( Widget w , XtPointer client_data ,
                  XEvent * ev , Boolean * continue_to_dispatch );
void SUMA_ATF_cb_label_Focus (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_PositionWindowRelative (Widget New, Widget Ref, SUMA_WINDOW_POSITION Loc);
void SUMA_cb_DrawROI_Finish (Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_DrawROI_Join (Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_DrawROI_SwitchROI (Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_DrawROI_Delete(Widget wcall, XtPointer cd1, XtPointer cbs);
void SUMA_delete_timeout_CB( XtPointer client_data , XtIntervalId * id );
SUMA_LIST_WIDGET * SUMA_FreeScrolledList (SUMA_LIST_WIDGET *LW);
SUMA_LIST_WIDGET * SUMA_AllocateScrolledList (
                     char *Label, int SelectPolicy, 
                     SUMA_Boolean RemoveDups, 
                     SUMA_Boolean ShowSorted,
                     Widget PosRef,
                     SUMA_WINDOW_POSITION Pos,
                     int width, 
                     void (*Default_cb)(  Widget w, XtPointer data, 
                                          XtPointer calldata), 
                     void *DefaultData,
                     void (*Select_cb)(   Widget w, XtPointer data, 
                                          XtPointer calldata), 
                     void *SelectData,
                     void (*CloseList_cb)(Widget w, XtPointer data, 
                                          XtPointer calldata), 
                     void *CloseListData);
SUMA_Boolean SUMA_UpdateScrolledListData(SUMA_LIST_WIDGET *LW, void *Default_Data, void *Select_Data, void *CloseList_Data); 
void SUMA_CreateScrolledList (    char **clist, int N_clist, SUMA_Boolean Partial, 
                                  SUMA_LIST_WIDGET *LW);
void SUMA_cb_CloseSwitchROI(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_SelectSwitchROI(Widget w, XtPointer data, XtPointer call_data);
void SUMA_FileSelection_popdown_cb (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_FileSelection_file_select_cb(Widget dialog, XtPointer client_data, XtPointer call_data);
SUMA_SELECTION_DIALOG_STRUCT *SUMA_CreateFileSelectionDialog (char *title, SUMA_SELECTION_DIALOG_STRUCT **dlg);
SUMA_SELECTION_DIALOG_STRUCT *SUMA_CreateFileSelectionDialogStruct (Widget daddy, SUMA_FILE_SELECT_MODE Mode, SUMA_Boolean preserve,
                                                                  void (*SelectCallback)(char *filename, void *data), void *SelectData,
                                                                  void (*CancelCallback)(void *data), void *CancelData,
                                                                  char *FilePattern,
                                                                  SUMA_SELECTION_DIALOG_STRUCT *dlg);
void SUMA_FileSelection_Unmap_cb (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_FreeFileSelectionDialogStruct(SUMA_SELECTION_DIALOG_STRUCT *dlg);
SUMA_PROMPT_DIALOG_STRUCT *SUMA_CreatePromptDialogStruct (SUMA_PROMPT_MODE Mode, char *TextFieldLabel, 
                                                         char *init_selection, 
                                                         Widget daddy, SUMA_Boolean preserve,
                                                         SUMA_PROMPT_BUTTONS Return_button,
                                                         void(*SelectCallback)(char *selection, void *data), void *SelectData,
                                                         void(*CancelCallback)(void *data), void *CancelData,
                                                         void(*HelpCallback)(void *data), void *HelpData,
                                                         int(*VerifyFunction)(char *selection, void *data), void *VerifyData,
                                                         SUMA_PROMPT_DIALOG_STRUCT *oprmpt);
SUMA_PROMPT_DIALOG_STRUCT *SUMA_CreatePromptDialog(char *title_extension, SUMA_PROMPT_DIALOG_STRUCT *prmpt);
const char * SUMA_PromptButtonLabel(SUMA_PROMPT_BUTTONS code);
SUMA_Boolean SUMA_CreatePromptActionArea (SUMA_PROMPT_DIALOG_STRUCT *prmpt);
void SUMA_PromptOk_cb (Widget w, XtPointer data, XtPointer calldata);
void SUMA_PromptClear_cb (Widget w, XtPointer data, XtPointer calldata);
void SUMA_PromptApply_cb (Widget w, XtPointer data, XtPointer calldata);
void SUMA_PromptCancel_cb (Widget w, XtPointer data, XtPointer calldata);
void SUMA_PromptHelp_cb (Widget w, XtPointer data, XtPointer calldata);
void SUMA_PromptActivate_cb (Widget w, XtPointer data, XtPointer calldata);
void SUMA_PromptUnmap_cb (Widget w, XtPointer data, XtPointer calldata);
void SUMA_FreePromptDialogStruct(SUMA_PROMPT_DIALOG_STRUCT *prmpt);
void  SUMA_cb_ToggleManagementColPlaneWidget(Widget w, XtPointer data, XtPointer client_data);
void SUMA_ColPlane_NewOrder (void *data);
void SUMA_ColPlane_NewOpacity (void *data);
void SUMA_ColPlane_NewDimFact (void *data);
void SUMA_cb_ColPlaneShow_toggled (Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_ColPlaneShowOne_toggled (Widget w, XtPointer data, XtPointer client_data);
int SUMA_ColPlaneShowOne_Set (SUMA_SurfaceObject *SO, SUMA_Boolean state);
void SUMA_cb_ColPlane_Delete(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_ColPlane_Load(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_Dset_Load(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_SurfCont_SwitchColPlane(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_CloseSwitchColPlane(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_SelectSwitchColPlane(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_ViewerCont_SwitchState (Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_ViewerCont_SwitchGroup (Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_SelectSwitchGroup(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_CloseSwitchGroup(Widget w, XtPointer data, XtPointer call_data);
SUMA_Boolean SUMA_InitializeColPlaneShell(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *ColPlane);
SUMA_Boolean SUMA_UpdateColPlaneShellAsNeeded(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_RemixRedisplay (SUMA_SurfaceObject *SO);
void SUMA_cb_SetDrawROI_SaveMode(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_SetDrawROI_SaveWhat(Widget w, XtPointer data, XtPointer call_data);
void SUMA_response(Widget widget, XtPointer client_data, XtPointer call_data);
int SUMA_PauseForUser(Widget parent, char *question, 
                      SUMA_WINDOW_POSITION pos, XtAppContext    *app,
                      int withpause);
int SUMA_ForceUser_YesNo(Widget parent, char *question, int default_ans, SUMA_WINDOW_POSITION pos);
int AskUser(Widget parent, char *question, char *ans1, char *ans2, int default_ans);
char * SUMA_ClassOf(int c);
char * SUMA_Format(int n, int w);
void SUMA_ShowAllVisuals (void); 
int SUMA_ShowVisual (Display *dpy, XVisualInfo *vi, SUMA_Boolean ShowHead);
int SUMA_AskUser_File_replace(Widget parent, char *question, int default_ans);
void SUMA_WidgetResize (Widget New, int width, int height);
void SUMA_LoadVisualState(char *fname, void *csvp);
int SUMA_ApplyVisualState(NI_element *nel, SUMA_SurfaceViewer *csv);
void SUMA_SaveVisualState(char *fname, void *csvp);
void SUMA_LoadSegDO (char *s, void *csvp);
void SUMA_SiSi_I_Insist(void);
void SUMA_BuildMenuReset(int nchar);
SUMA_Boolean SUMA_Init_SurfCont_SurfParam(SUMA_SurfaceObject *SO);
int SUMA_NodeNeighborAlongScreenDirection(SUMA_SurfaceViewer *sv,
                                          SUMA_SurfaceObject *SO,
                                          int inode, double *dd);
SUMA_Boolean SUMA_World2ScreenCoords (SUMA_SurfaceViewer *sv, int N_List, double *WorldList, 
                                       double *ScreenList, int *Quad, SUMA_Boolean ApplyXform);
SUMA_Boolean SUMA_DrawWindowLine(SUMA_SurfaceViewer *sv, int x0, int y0, int x1, int y1, int meth);
void SUMA_cb_SetDrawROI_WhatDist(Widget widget, XtPointer client_data, XtPointer call_data);
SUMA_Boolean SUMA_UpdateColPlaneShellAsNeeded(SUMA_SurfaceObject *SO);
void SUMA_cb_createSurfaceCont(Widget w, XtPointer data, XtPointer callData);
SUMA_Boolean SUMA_display_edge_striplist(DList *striplist, SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO, char *DispOptions);
Widget SUMA_CloseBhelp_Frame( Widget parent,
                              XtCallbackProc close_callback, 
                              XtPointer close_data,
                              char *close_hint,
                              char *close_help);
void SUMA_cb_XformPreProc_Save (Widget w, XtPointer data, 
                             XtPointer client_data);
void SUMA_cb_XformOpts_Apply (Widget w, XtPointer data, 
                             XtPointer client_data);

#define SUMA_MAX_XFCB_OBJS 32       /*!< Max number of callbacks or xforms 
                                         that may act on dsets or SOs */

#define SUMA_SPECT_AXIS(TR,NSAMP,fs, fmax,fstep) {\
   fs=1.0/(TR); fmax = fs/2.0; fstep=fs/(float)(NSAMP);   \
}

typedef struct {
   
   Widget AppShell;
   Widget Active_tb;
   
   Widget ParentLabel_lb;

   SUMA_ARROW_TEXT_FIELD *AF0;
   SUMA_ARROW_TEXT_FIELD *AF1;
   SUMA_ARROW_TEXT_FIELD *AF2;
   
   Widget SaveOpts_pb;
   Widget SavePreProc_pb;
   Widget ShowPreProc_tb;
   Widget ApplyOpts_pb;
   Widget LoadOrtFile_pb;
   Widget OrtFileLabel_lb;
   
} SUMA_GENERIC_XFORM_INTERFACE;

typedef struct {
   char name[128];
   char idcode_str[SUMA_IDCODE_LENGTH]; /*!< A unique identifier for xform */
   char parents[SUMA_MAX_XFCB_OBJS][SUMA_IDCODE_LENGTH]; /*!< IDs of parents
                     upon which the xform is applied. 
                     These could be SOs or DSETS*/ 
   char parents_domain[SUMA_MAX_XFCB_OBJS][SUMA_IDCODE_LENGTH]; /*!< IDs of SO
                   defining the domain of the parent. This is meaningful when
                   the parent is a dset */
   int  N_parents;
   char children[SUMA_MAX_XFCB_OBJS][SUMA_IDCODE_LENGTH]; /*!< IDs of children
                  created by application of xform.
                  These could be SOs or DSETS*/  
   int N_children;
   int active;
   int ShowPreProc;
   NI_group *XformOpts;
      
   SUMA_GENERIC_XFORM_INTERFACE *gui;
} SUMA_XFORM;  /*!< See Comments in ZSS labbook NIH-5, pp30-... */ 

void SUMA_cb_CloseXformInterface(Widget w, XtPointer data, XtPointer call_data);
SUMA_Boolean SUMA_InitializeXformInterface (SUMA_XFORM *xf);
void SUMA_CreateXformInterface(SUMA_XFORM *xf);
void SUMA_DotXform_NewOrtName(  SUMA_XFORM *xf,
                               char * ortname, 
                               int fromgui);
void SUMA_OpenXformOrtFile (char *filename, void *data);


#define SUMA_XformOrtFile_Load_help   \
   "Load an ort file"

#define SUMA_XformPreProc_Save_help \
   "Save preprocessed dsets to disk"
   
#define SUMA_XformOpts_Apply_help   \
   "Apply changes to transform options now"
   
#define SUMA_XformOpts_Save_help \
   "Save options structure to disk"
   
#define SUMA_ShowPreProcXform_help  \
   "Show in SUMA pre-processed time series" 

#define SUMA_ActivateXform_help  \
   "Activate/Suspend xform" 

#define SUMA_DotXform_AF0_hint \
   "Bottom pass frequency in Hz." 

#define SUMA_DotXform_AF0_help   \
   "Bottom pass frequency in Hz."
   
#define SUMA_DotXform_AF1_hint \
   "Top pass frequency in Hz." 

#define SUMA_DotXform_AF1_help   \
   "Top pass frequency in Hz."

#define SUMA_DotXform_AF2_hint \
   "Baseline model polynomial degree." 

#define SUMA_DotXform_AF2_help   \
   "Baseline model polynomial degree\n"   \
   "-1 for no baseline model.\n"

#define SUMA_DotXform_ParentLabel_help  \
   "Label of time series dsets transformed." 

#define SUMA_DrawROI_ParentLabel_help  \
   "Label of the ROI's parent surface." 

#define SUMA_DrawROI_DrawROIMode_help\
   "Toggles ROI drawing mode.\n" \
   "If turned on, then drawing is enabled \n"   \
   "and the cursor changes to a target. \n"  \
   "To draw, use the right mouse button. \n" \
   "If you want to pick a node without causing \n" \
   "a drawing action, use shift+right button."
   
#define SUMA_DrawROI_ContROIMode_help\
   "Toggles ROI contour drawing \n" \
   "If turned on, then contours are drawn around filled ROIs\n" 
   
#define SUMA_DrawROI_PenMode_help\
   "Toggles Pen drawing mode\n"\
   "If turned on, the cursor changes shape to a pen. \n" \
   "In the pen mode, drawing is done with button 1. \n"  \
   "This is for coherence with AFNI's pen drawing mode, \n" \
   "which is meant to work pleasantly with a stylus directly \n"  \
   "on the screen. In pen mode, you draw with the left mouse \n"  \
   "button and move the surface with the right button. \n"  \
   "To pick a node, use shift+left button. \n"  \
   "Pen mode only works when Draw Mode is enabled."

#define SUMA_DrawROI_AfniLink_help\
   "Toggles Afni Link for ROI drawing.\n" \
   "If turned on, then ROIs drawn on the surface are\n" \
   "sent to AFNI. \n"   \
   "Surface ROIs that are sent to AFNI are turned\n"  \
   "into volume ROIs (VOIs) on the fly and displayed \n" \
   "in a functional volume with the same colormap used in SUMA.\n"   \
   "The mapping from the surface domain (ROI) to the volume \n"   \
   "domain (VOI) is done by intersection of the first with \n" \
   "the latter. The volume used for the VOI has the same \n"   \
   "resolution (grid) as the Surface Volume (-sv option) \n"   \
   "used when launching SUMA. The color map used for ROIs \n"  \
   "is set by the environment variable SUMA_ROIColorMap."
   
#define SUMA_DrawROI_Label_help  \
   "Label of ROI being drawn.\n" \
   "It is very advisable that you use \n" \
   "different labels for different ROIs. \n"  \
   "If you don't, you won't be able to \n"   \
   "differentiate between them afterwards."

#define SUMA_DrawROI_Value_help  \
   "Integer value associated with ROI.\n" \
   "This value controls the color of the \n" \
   "ROI per the ROI colormap."
   
#define SUMA_DrawROI_Undo_help   \
   "Undo the last action on the stack." 
   
#define SUMA_DrawROI_Redo_help   \
   "Redo the last undone action."

#define SUMA_DrawROI_Join_help   \
   "Join the first node of the ROI to the last,\n"   \
   "thereby creating a close contour ROI.\n" \
   "This is a necessary step before the filling\n" \
   "operation. Joining is done by cutting the surface\n"   \
   "with a plane formed by the two nodes\n"  \
   "and the projection of the center of your window.\n"  \
   "You could double click at the last node, if you don't\n"   \
   "want to use the 'Join' button." 

#define SUMA_DrawROI_Finish_help \
   "Mark ROI as finished.\n" \
   "Allows you to start drawing a new one.\n"   \
   "Once marked as finished, an ROI's label\n" \
   "and value can no longer be changed.\n"   \
   "To do so, you will need to 'Undo' the \n"   \
   "finish action."
   
#define SUMA_DrawROI_SwitchROI_help \
   "Allows you to switch between ROIs.\n"   \
   "This is where you'll suffer if ROIs \n"  \
   "on topologically isomorphic surfaces \n" \
   "share identical labels."
   
#define SUMA_DrawROI_Load_help   \
   "Load a Drawn ROI.\n"   \
   "See BHelp for 'Save' below."

#define SUMA_DrawROI_DeleteROI_help   \
   "Delete a drawn ROI.\n" \
   "This operation is not reversible,\n"  \
   "(no Undo here) so you'll have to click twice."
   
#define SUMA_DrawROI_SaveFormat_help   \
   "File format for saving ROI:\n"  \
   "Format options are 1D and NIML. \n"   \
   "   The 1D format is the same one used in AFNI. \n"   \
   "It is an ASCII file with 2 values per line. The first \n"  \
   "value is a node index, the second is the node's value. \n" \
   "Needless, to say, this format does not support the storage \n"   \
   "of ROI auxiliary information such as Label and \n"   \
   "Parent Surface, etc., nor does it preserve the order in which \n"   \
   "nodes are traversed during a tracing. For that you'll have to use NIML.\n" \
   "   NIML is a whole different story which will be documented \n"  \
   "(if necessary) in the future. Suffice it to say that in NIML \n" \
   "format you can store all the auxiliary information about \n"  \
   "each ROI, unlike with the .1D format. \n"   \
   "But more importantly, the NIML format allows you to preserve\n"  \
   "--------------------  the order in which you traced the ROI. \n" \
   "This information can be later used for the purpose of sampling \n"  \
   "cortical activity along a particular path. This would be accomplished \n" \
   "with the aid of ROI2dataset's -nodelist* options, along with \n" \
   "ConvertDset's -node_select_1D option."
   
#define SUMA_DrawROI_SaveWhat_help  \
   "Which ROIs to save?\n" \
   "   This: saves the current ROI. \n"   \
   "   All: saves all ROIs on surfaces related to the Parent \n"  \
   "        surface of the current ROI."
   
#define SUMA_DrawROI_WhatDist_help  \
   "Report length of drawn segments?\n" \
   "   -----: No distance calculations. \n"   \
   "   trace: Calculate distance along last\n"  \
   "          traced segment.\n" \
   "   all:   In addition to output from\n"  \
   "          'trace', calculate the shortest\n"   \
   "          distance between the first and \n"   \
   "          last node of the trace.\n"  \
   "   The results are output to the Message Log \n"  \
   "   window (Help --> Message Log) with the following\n" \
   "   information:\n"  \
   "   n0, n1: Indices of first and last node forming\n" \
   "           the traced path.\n"  \
   "   N_n:    Number of nodes forming the trace.\n"  \
   "   lt:     Trace length calculated as the sum\n"  \
   "           of the distances from node to node.\n"   \
   "           This length is a slight overestimation\n"  \
   "           of the geodesic length.\n" \
   "           Units for all distances is the same as\n" \
   "           the units for surface coordinates. Usually\n"   \
   "           and hopefully in mm.\n" \
   "   lt_c:   Trace length corrected by a scaling factor\n"  \
   "           from [1] to better approximate geodesic \n"  \
   "           distances. Factor is 2/(1+sqrt(2)).\n" \
   "           Do not use this factor when N_n is small. \n"\
   "           Think of the extreme case when N_n is 2.\n"   \
   "   sd:     Shortest distance on the mesh (graph) \n" \
   "           between n0 and n1 using Dijkstra's algorithm.\n"   \
   "   sd_c:   Corrected shortest distance as for lt_c.\n"  \
   "\n"  \
   "   Note 1: sd and sd_c take some time to compute. That is \n"  \
   "           why they are only calculated when you select 'all'.\n"   \
   "   Note 2: The output is formatted to be cut and pasted into\n"  \
   "           a .1D file for ease of processing.\n"  \
   "           You can include all the comment lines that\n"   \
   "           start with '#'. But you cannot combine entries\n"  \
   "           from the output obtained using 'all' option with \n" \
   "           those from 'trace' since they produce different \n"  \
   "           numbers of values.\n"  \
   "\n"  \
   "   [1] Fischl et al, Neuroimage 9, 195-207 1999, \n" \
   "       Cortical Surface-Based Analysis."
   
#define SUMA_DrawROI_Save_help \
   "Save the Drawn ROI to disk.\n"  \
   "Choose the file format and what is to be\n"   \
   "saved from the two menus ahead.\n"  \
   "\n"  \
   SUMA_DrawROI_SaveFormat_help  \
   "\n"  \
   SUMA_DrawROI_SaveWhat_help

#define SUMA_closeDrawROI_help  \
   "Close Draw ROI window.\n" \
   "Current settings are preserved for the \n"   \
   "next time you reopen this window."
 


#define SUMA_help_help \
   "Click the hand\n"   \
   "on any button or \n"\
   "label, menu, etc. to\n"  \
   "get a little help."
   
#define SUMA_closeSumaCont_help \
   "Close SUMA controller window.\n"   \
   "Current settings are preserved\n"\
   "when controller is reopened."

#define SUMA_LockSumaCont_help   \
   "Set the Cross Hair lock \n"  \
   "between viewers.\n" \
   "- No Lock\n"  \
   "i Node index Lock\n"   \
   "c Coordinate Lock"
   
#define SUMA_LockViewSumaCont_help  \
   "Lock the view point of \n"   \
   "all viewers."
   
#define SUMA_viewerSumaCont_help   \
   "Opens a new Surface viewer window."  

#define SUMA_closeSurfaceCont_help   \
   "Close Surface controller window.\n"   \
   "Current settings are preserved\n"\
   "when controller is reopened.\n"

#define SUMA_closeXformCont_help   \
   "Close Xform controller window.\n"   \
   "Current settings are preserved\n"\
   "when controller is reopened.\n"

#define SUMA_closeViewerCont_help   \
   "Close Viewer controller window.\n"   \
   "Current settings are preserved\n"\
   "when controller is reopened.\n"
  

#define  SUMA_moreViewerInfo_help  \
   "Opens a dialog with detailed\n" \
   "information about the surface\n"\
   "viewer.\n"

#define SUMA_SurfCont_ColPlaneDim_hint \
   "Dimming factor to apply to colormap." \

#define SUMA_SurfCont_ColPlaneOrder_hint \
   "Order of Dset's colorplane." \

#define SUMA_SurfCont_ColPlaneOpacity_hint \
   "Opacity of Dset's colorplane." \


   
#endif
