#ifndef SUMA_DISPLAY_INCLUDED
#define SUMA_DISPLAY_INCLUDED

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
      Pixel m_fg_pix;  \
      XtVaGetValues (m_w, XmNforeground, &m_fg_pix, NULL);  \
      XtVaSetValues (m_w, XmNselectColor, m_fg_pix, NULL);  \
}

/*! set the string of a label */
#define SUMA_SET_LABEL(m_w, m_s) {\
   XmString m_str = XmStringCreateLocalized(m_s); \
   XtVaSetValues (m_w, XmNlabelString, m_str, NULL); \
   XmStringFree (m_str);   \
}

#define SUMA_SET_TEXT_FIELD(m_w, m_s) {\
   XtVaSetValues (m_w, XmNvalue, m_s, NULL); \
}

/*! m_s is a char *. Do not allocate space for it, do not free it afterwards 
*/
#define SUMA_GET_TEXT_FIELD(m_w, m_s) {\
   void *n; \
   XtVaGetValues (m_w, XmNvalue, &n, NULL); \
   m_s = (char *)n;  \
}

#define SUMA_MARGIN  1

String *SUMA_get_fallbackResources ();         
Boolean SUMA_handleRedisplay (XtPointer w);
void SUMA_postRedisplay(Widget w, XtPointer clientData, XtPointer call);
void SUMA_display(SUMA_SurfaceViewer *csv, SUMA_DO *dov);
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
SUMA_Boolean SUMA_GetSelectionLine (SUMA_SurfaceViewer *sv, int x, int y);
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
Widget SUMA_BuildMenu(Widget parent, int menu_type, char *menu_title, char menu_mnemonic, \
                     SUMA_Boolean tear_off, SUMA_MenuItem *items, void *ContID, 
                     Widget *MenuWidgets);
void SUMA_cb_FileOpenSpec (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileOpenSurf (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileClose (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_moreSurfInfo (Widget w, XtPointer client_data, XtPointer callData);
SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTestShellStruct (void (*opencallback)(void *data), void *opendata, 
                                                            void (*closecallback)(void*data), void *closedata);
SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTextShell (char *s, char *title, SUMA_CREATE_TEXT_SHELL_STRUCT *TextShellStruct);
void SUMA_cb_search_text(Widget widget, XtPointer client_data, XtPointer call_data);
void SUMA_DestroyTextShell (Widget w, XtPointer ud, XtPointer cd);
void SUMA_SurfInfo_open (void *SO);
void SUMA_SurfInfo_destroyed (void *SO);
void SUMA_cb_ToggleCaseSearch (Widget widget, XtPointer client_data, XtPointer call_data);
void SUMA_cb_helpViewer (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpIO_notify(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpMemTrace(Widget w, XtPointer data, XtPointer callData);
char * SUMA_FormatMessage (SUMA_MessageData *MD);
void SUMA_PopUpMessage (SUMA_MessageData *MD);
void SUMA_cb_helpMessageLog (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_SetRenderMode(Widget widget, XtPointer client_data, XtPointer call_data);
void SUMA_cb_ToolsDrawROI (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_cb_CloseDrawROIWindow(Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_CreateDrawROIWindow(void);
SUMA_Boolean SUMA_InitializeDrawROIWindow (SUMA_DRAWN_ROI *DrawnROI);
SUMA_Boolean SUMA_OpenDrawROIWindow (SUMA_DRAWN_ROI *DrawnROI);
void SUMA_cb_DrawROImode_toggled (Widget w, XtPointer data, XtPointer call_data);
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
                              SUMA_ARROW_TEXT_FIELD *AF);
void SUMA_CreateTextField ( Widget pw, char *label,
                              int cwidth, 
                              void (*NewValueCallback)(void *data),
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
SUMA_LIST_WIDGET * SUMA_AllocateScrolledList (char *Label, int SelectPolicy, 
                                                SUMA_Boolean RemoveDups, SUMA_Boolean ShowSorted,
                                                Widget PosRef, SUMA_WINDOW_POSITION Pos,
                                                void (*Default_cb)(Widget w, XtPointer data, XtPointer calldata), void *DefaultData,
                                                void (*Select_cb)(Widget w, XtPointer data, XtPointer calldata), void *SelectData,
                                                void (*CloseList_cb)(Widget w, XtPointer data, XtPointer calldata), void *CloseListData);
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
                                                         SUMA_Boolean(*VerifyFunction)(char *selection, void *data), void *VerifyData,
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
void  SUMA_cb_UnmanageWidget(Widget w, XtPointer data, XtPointer client_data);
void SUMA_ColPlane_NewOrder (void *data);
void SUMA_ColPlane_NewOpacity (void *data);
void SUMA_cb_ColPlaneShow_toggled (Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_ColPlane_Delete(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_ColPlane_Load(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_SurfCont_SwitchColPlane(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_CloseSwitchColPlane(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_SelectSwitchColPlane(Widget w, XtPointer data, XtPointer call_data);
SUMA_Boolean SUMA_InitializeColPlaneShell(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *ColPlane);
SUMA_Boolean SUMA_UpdateColPlaneShellAsNeeded(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_RemixRedisplay (SUMA_SurfaceObject *SO);
void SUMA_cb_SetDrawROI_SaveMode(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_SetDrawROI_SaveWhat(Widget w, XtPointer data, XtPointer call_data);
void SUMA_response(Widget widget, XtPointer client_data, XtPointer call_data);
int SUMA_AskUser_ROI_replace(Widget parent, char *question, int default_ans);
int AskUser(Widget parent, char *question, char *ans1, char *ans2, int default_ans);
char * SUMA_ClassOf(int c);
char * SUMA_Format(int n, int w);
void SUMA_ShowAllVisuals (void); 
int SUMA_ShowVisual (Display *dpy, XVisualInfo *vi, SUMA_Boolean ShowHead);
int SUMA_AskUser_File_replace(Widget parent, char *question, int default_ans);





#define SUMA_DrawROI_ParentLabel_help  \
   "Label of the ROI's parent surface." 

#define SUMA_DrawROI_DrawROIMode_help\
   "Toggles ROI drawing mode"

#define SUMA_DrawROI_AfniLink_help\
   "Toggles Afni Link for ROI drawing"

#define SUMA_DrawROI_Save_help \
   "Save the Drawn ROI to disk.\n"  \
   "Choose the file format\n"   \
   "and what is to be saved\n"  \
   "from the two menus ahead."  \

#define SUMA_DrawROI_Load_help   \
   "Load a Drawn ROI"

#define SUMA_closeDrawROI_help  \
   "Close Draw ROI window"
 
#define SUMA_DrawROI_Redo_help   \
   "Redo the last undone action"

#define SUMA_DrawROI_Undo_help   \
   "Undo the last action on the stack"

#define SUMA_DrawROI_Join_help   \
   "Join the first node of the path to the last.\n"   \
   "This is done by cutting the surface\n"   \
   "with a plane formed by the two nodes\n"  \
   "and the projection of the center of \n"  \
   "your window." 

#define SUMA_DrawROI_Finish_help \
   "Label ROI as finished.\n" \
   "Allows you to start \n"   \
   "drawing a new one."

#define SUMA_DrawROI_SwitchROI_help \
   "Switch between ROIs.\n"   \

#define SUMA_help_help \
   "Click the hand\n"   \
   "on any button to\n"  \
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
   
#define  SUMA_moreSurfInfo_help  \
   "Opens a dialog with detailed\n" \
   "information about the surface\n"\
   "object.\n"

#define SUMA_DrawROI_ColPlaneShow_help   \
   "Shows/Hides the colorplane."
   
#endif
