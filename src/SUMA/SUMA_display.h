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
         
#define SUMA_MARGIN  3

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



 


#define SUMA_help_help \
   "Click the hand\n"   \
   "on any button to\n"  \
   "get a little help."
   
#define SUMA_closeSumaCont_help \
   "Close SUMA controller window.\n"   \
   "Current settings are preserved\n"\
   "when controller is reopened.\n"

#define SUMA_LockSumaCont_help   \
   "Set the Cross Hair lock \n"  \
   "between viewers.\n" \
   "- No Lock\n"  \
   "i Node index Lock\n"   \
   "c Coordinate Lock\n"

#define SUMA_viewerSumaCont_help   \
   "Opens a new Surface viewer window.\n"  

#define SUMA_closeSurfaceCont_help   \
   "Close Surface controller window.\n"   \
   "Current settings are preserved\n"\
   "when controller is reopened.\n"
   
#define  SUMA_moreSurfInfo_help  \
   "Opens a dialog with detailed\n" \
   "information about the surface\n"\
   "object.\n"
   
#endif
