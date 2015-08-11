#ifndef SUMA_DISPLAY_INCLUDED
#define SUMA_DISPLAY_INCLUDED
 
/* 

   Historical Note from Fri Jan  3 10:21:52 EST 2003:
   the method for hiding a surface viewer (and other controllers), used to have      three options prior to Fri Jan  3 10:21:52 EST 2003
   Now only SUMA_WITHDRAW and NOT SUMA_DESTROY should be used.

   As of Wed Sep 23 14:45:59 EDT 2009
   ----------------------------------
   On Mac OS X 10.5, and at least early 10.6, using XWithdrawWindow,
   followed by XMapRaised, SUMA would crash with a GLX_BadDrawable error.
   One way around this is to go back to XtUnrealizeWidget/XtRealizeWidget pair.
   It appears that GLX implementation is full of uninitialzation errors, 
   according to Valgrind. 
   
   So now one can choose between three closing modes. For widgets with GLX
   drawables (Viewers, and SurfaceControllers) use SUMA_UNREALIZE.
   For other, stick with SUMA_WITHDRAW as previously done.
*/

#define SUMA_DESTROY    1
#define SUMA_WITHDRAW   2
#define SUMA_UNREALIZE  3

/* Flags to move sub-bricks up or down using 
   same functions to set a particular index 
   Values must be < 0 and != -1 */
#define SUMA_FORWARD_ONE_SUBBRICK    -444
#define SUMA_BACK_ONE_SUBBRICK       -555 

#define SUMA_XmArrowFieldMenu -123

#define SUMA_CLOSE_MODE       SUMA_WITHDRAW
#define SUMA_GL_CLOSE_MODE    SUMA_UNREALIZE

#define SUMA_HOLD_IT  { \
   SUMA_S_Note("Waiting...");\
   glXWaitGL();glXWaitX(); glFinish();\
   SUMA_S_Note("Done.");}  

#define SUMA_CHECK_GL_ERROR(str)                                           \
{                                                                  \
    GLenum error;                                                  \
    while((error = glGetError()) != GL_NO_ERROR)                   \
       fprintf(stderr,"**************GL Error: %s (%s)\n", \
         gluErrorString(error), str);  \
}



                                  
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
   void       (*callback)();/*!< routine to call; This is only used when
                                 and arrow field is usurping a menu's job */
   void *ContID; /*!< some identifier of the controller */
   SUMA_MENU_WIDGET *SMW; /*!< This is needed for handling arrow fields */
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
         isv = (INT_CAST)datap->ContID; \
         widtype = (INT_CAST)datap->callback_data; }
         

#define SUMA_VIEWER_FROM_VIEWMENU_CALLBACK(data, isv, widtype) {\
         SUMA_MenuCallBackData *datap; \
         datap = (SUMA_MenuCallBackData *)data;  \
         isv = (INT_CAST)datap->ContID; \
         widtype = (INT_CAST)datap->callback_data; }
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
      SUMA_LH("Setting up matrix mode and perspective\n"\
              "iState=%d, Apsect=%f, FOV=%p...\n",\
              csv->iState, csv->Aspect, csv->FOV); \
      if (csv->FOV[csv->iState] < 0.00001) { \
         /* This can happen when only Non-SO objects are loaded
            Just do nothing  without fanfare */ \
         SUMA_LHv("Fov (%f) seems messed up, ignoring call\n",\
            csv->FOV[csv->iState]);   \
      }  else {\
         glMatrixMode (GL_PROJECTION); \
         glLoadIdentity ();   \
         gluPerspective((GLdouble)csv->FOV[csv->iState], csv->Aspect, \
                        SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); \
                        /*lower angle is larger zoom,*/   \
      }  \
   }  else { \
      GLdouble m_sz = \
         0.5 *tan(SUMA_PI * csv->FOV[csv->iState] / 180.0) * \
         csv->GVS[csv->StdView].ViewFrom[2];  \
      GLdouble m_szx = m_sz * csv->Aspect;   \
      GLdouble m_szy = m_sz ;   \
      SUMA_LH("Setting up matrix mode and orthographic projection "\
              "(m_szx = %g, m_szy=%g)...\n", m_szx, m_szy); \
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

#define SUMA_GL_MAT_SHOW(mm,str) {\
   int m_i; double m_dmatrix[16];  \
   glGetDoublev(mm, m_dmatrix); \
   SUMA_S_Note(str); \
   for (m_i=0; m_i<4; ++m_i) {   \
      fprintf(stderr,"\t");  \
      fprintf(stderr,"%+3.3f\t%+3.3f\t%+3.3f\t%+3.3f\t", \
         m_dmatrix[m_i],m_dmatrix[4+m_i],m_dmatrix[8+m_i],m_dmatrix[12+m_i]); \
      fprintf(stderr,"\n");  \
   }\
}


#define SUMA_SET_AS_NEEDED_2D_VIEW_ANGLE(sv) {  \
   int m_j = SUMA_BestStandardView(sv, SUMAg_DOv, SUMAg_N_DOv); \
   float m_a[3]; \
   if (m_j == SUMA_2D_Z0) {   \
      m_a[0] = 1.0; m_a[1] = 0.0; m_a[2] = 0.0; \
      axis_to_quat(m_a, 0, sv->GVS[m_j].currentQuat); \
   } else if (m_j == SUMA_2D_Z0L) { \
      m_a[0] = 0.0; m_a[1] = 0.0; m_a[2] = 1.0; \
      axis_to_quat(m_a, SUMA_PI, sv->GVS[m_j].currentQuat); \
   } else if (m_j == SUMA_3D_Z0) { \
      SUMA_HOME_QUAT(SUMA_3D_Z0, sv->GVS[m_j].currentQuat); \
   } \
}

#define SUMA_MARGIN  1

/* return the character for a viewer struct */
#define SUMA_SV_CHAR(csv) \
   (char)( csv ? (65+SUMA_WhichSV((csv), SUMAg_SVv, SUMA_MAX_SURF_VIEWERS)):'-' )

#define SV_IN_PRYING(sv) (((sv) && \
                           (((sv)->GVS[(sv)->StdView].vLHpry[0] != 0.0f ) || \
                            ((sv)->GVS[(sv)->StdView].vLHpry[1] != 0.0f ) || \
                            ((sv)->GVS[(sv)->StdView].vLHpry[2] != 0.0f )) ) \
                           ? 1:0)

/* Make sure recording path is legit */
#define SUMA_VALIDATE_RECORD_PATH(autorecord) {\
   if (!THD_mkdir((autorecord)->Path)) {  \
      SUMA_PARSED_NAME *pn2=NULL;   \
      SUMA_S_Errv(   \
   "Failed to create directory %s, resorting to local directory.\n", \
         (autorecord)->Path); \
      pn2 = SUMA_ParseFname((autorecord)->FileName, NULL);  \
      SUMA_Free_Parsed_Name((autorecord)); \
      (autorecord) = pn2; pn2=NULL; \
   }  \
}

/* Get the box corner points from an Axis's range values */
#define SUMA_BOX_CORNER_POINTS_FROM_AXIS(Ax, P) {   \
   P[0][0] = Ax->BR[0][0]; P[0][1] = Ax->BR[1][0]; P[0][2] = Ax->BR[2][0];      \
                                                         /*xmin, ymin, zmin */  \
   P[1][0] = Ax->BR[0][1]; P[1][1] = Ax->BR[1][0]; P[1][2] = Ax->BR[2][0];      \
                                                          /*xmax, ymin, zmin */ \
   P[2][0] = Ax->BR[0][0]; P[2][1] = Ax->BR[1][1]; P[2][2] = Ax->BR[2][0];      \
                                                          /*xmin, ymax, zmin */ \
   P[3][0] = Ax->BR[0][1]; P[3][1] = Ax->BR[1][1]; P[3][2] = Ax->BR[2][0];      \
                                                          /*xmax, ymax, zmin */ \
   P[4][0] = Ax->BR[0][0]; P[4][1] = Ax->BR[1][0]; P[4][2] = Ax->BR[2][1];      \
                                                          /*xmin, ymin, zmax */ \
   P[5][0] = Ax->BR[0][1]; P[5][1] = Ax->BR[1][0]; P[5][2] = Ax->BR[2][1];      \
                                                          /*xmax, ymin, zmax */ \
   P[6][0] = Ax->BR[0][0]; P[6][1] = Ax->BR[1][1]; P[6][2] = Ax->BR[2][1];      \
                                                          /*xmin, ymax, zmax */ \
   P[7][0] = Ax->BR[0][1]; P[7][1] = Ax->BR[1][1]; P[7][2] = Ax->BR[2][1];      \
                                                          /*xmax, ymax, zmax */ \
}

String *SUMA_get_fallbackResources ();         
void SUMA_CullOption(SUMA_SurfaceViewer *, const char *action);
SUMA_Boolean SUMA_glXMakeCurrent(Display *dpy, Window wdw, GLXContext cont,
      char *fname, char *wlab, int force);
Boolean SUMA_handleRedisplay (XtPointer w);
void SUMA_postRedisplay(Widget w, XtPointer clientData, XtPointer call);
GLenum SUMA_index_to_clip_plane(int iplane) ;
int SUMA_PixelsToDisk(SUMA_SurfaceViewer *csv, int w, int h, GLvoid *pixels, 
                      int colordepth, int verb, char *ufname, 
                      int autoname, int over); 
int SUMA_SnapToDisk(SUMA_SurfaceViewer *csv, int verb, int getback);
SUMA_DO_LOCATOR *SUMA_SV_SortedRegistDO(SUMA_SurfaceViewer *csv, int *N_regs,
                                        SUMA_DO *dov);
void SUMA_display(SUMA_SurfaceViewer *csv, SUMA_DO *dov);
void SUMA_display_one(SUMA_SurfaceViewer *csv, SUMA_DO *dov);
Colormap SUMA_getShareableColormap_Eng(XVisualInfo * vi, Display *dpy);
Colormap SUMA_getShareableColormap(SUMA_SurfaceViewer * csv);
void SUMA_graphicsInit(Widget w, XtPointer clientData, XtPointer call);
void SUMA_expose(Widget w, XtPointer clientData, XtPointer call);
void SUMA_resize(Widget w, XtPointer clientData, XtPointer call);
SUMA_Boolean SUMA_SV_DrawAreaDims_From_WindDims(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SV_WindDims_From_DrawAreaDims(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_SV_InitDrawAreaOffset(SUMA_SurfaceViewer *sv);
int SUMA_XErrHandler( Display *d , XErrorEvent *x );
SUMA_Boolean SUMA_X_SurfaceViewer_Create (void);
void SUMA_ButtOpen_pushed (Widget w, XtPointer cd1, XtPointer cd2);
void SUMA_ButtClose_pushed (Widget w, XtPointer cd1, XtPointer cd2);
int SUMA_generateEPS(char *filename, int inColor, 
                     unsigned int width, unsigned int height);
GLvoid *SUMA_grabPixels(int ColorDepth, unsigned int width, unsigned int height);
GLvoid *SUMA_grabRenderedPixels(SUMA_SurfaceViewer *sv,
           int ColorDepth, unsigned int width, unsigned int height, int getback);
SUMA_Boolean SUMA_RenderToPixMap (SUMA_SurfaceViewer *csv, SUMA_DO* dov);
void SUMA_context_Init(SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_NormScreenToWorld(SUMA_SurfaceViewer *sv, 
                                    double xn, double yn, 
                                    GLdouble *pfront, GLdouble *pback, 
                                    int xform);
SUMA_Boolean SUMA_GetSelectionLine (SUMA_SurfaceViewer *sv, int x, int y, 
                                    GLdouble *Pick0, GLdouble *Pick1, 
                                    int N_List, int *xList, int *yList, 
                                    GLdouble *Pick0List);
SUMA_Boolean SUMA_isSurfContWidgetCreated(SUMA_X_SurfCont  *SurfCont);
int SUMA_OpenCloseSurfaceCont(Widget w, 
                              SUMA_ALL_DO *ado, 
                              SUMA_SurfaceViewer *sv);
int SUMA_OpenSurfCont_if_other(Widget w, 
                              SUMA_ALL_DO *ado, 
                              SUMA_SurfaceViewer *sv);
int SUMA_viewSurfaceCont(Widget w, SUMA_ALL_DO *SO, 
                         SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_MarkSurfContOpen(int Open, SUMA_ALL_DO *SO);
SUMA_ALL_DO **SUMA_DOsInSurfContNotebook(Widget NB);
void SUMA_cb_viewSurfaceCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_viewViewerCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_toggle_crosshair(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_toggle_node_in_focus(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_toggle_selected_faceset(Widget w, 
                                     XtPointer data, XtPointer callData);
int SUMA_viewSumaCont(int flag);
void SUMA_cb_viewSumaCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_createSumaCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_closeSumaCont(Widget w, XtPointer data, XtPointer callData);
Widget SUMA_GetTopShell(Widget w);
void SUMA_cb_createViewerCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_closeViewerCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_XHlock_toggled(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_XHviewlock_toggled(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_closeSurfaceCont(Widget w, XtPointer data, XtPointer callData);
SUMA_Boolean SUMA_WriteCont_Help(SUMA_DO_Types do_type, TFORM targ, char *fname);
SUMA_Boolean SUMA_Snap_AllCont(SUMA_DO_Types do_type, char *fname);
void SUMA_cb_createSurfaceCont(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_createSurfaceCont_SO(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_createSurfaceCont_TDO(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_createSurfaceCont_VO(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_createSurfaceCont_CO(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_createSurfaceCont_GLDO(Widget w, XtPointer data, 
                                     XtPointer callData);
void SUMA_cb_createSurfaceCont_MDO(Widget w, XtPointer data, 
                                     XtPointer callData);
void SUMA_cb_newSumaCont(Widget w, XtPointer client_data, XtPointer callData);
void  SUMA_cb_doneSumaCont(Widget wcall, XtPointer cd1, XtPointer cbs);
void SUMA_quit_timeout_CB( XtPointer client_data , XtIntervalId * id );
void SUMA_set_Lock_rb (SUMA_rb_group * Lock_rbg, int irb, int but);
void SUMA_set_Lock_arb (SUMA_rb_group * Lock_rbg);   
void SUMA_cb_XHaviewlock_toggled (Widget w, XtPointer client_data, 
                                  XtPointer callData);
void SUMA_cb_XHalock_toggled (Widget w, XtPointer client_data, 
                              XtPointer callData);
void SUMA_set_LockView_atb (void);
int SUMA_BuildMenu(Widget parent, int menu_type, char *menu_title, 
                   char menu_mnemonic,  SUMA_Boolean tear_off, 
                   SUMA_MenuItem *items, void *ContID, 
                   char *wname, char *hint, char *help,
                   SUMA_MENU_WIDGET *SMW);
void SUMA_cb_FileOpenSpec (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileOpenSurf (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileClose (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileSaveView (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_FileLoadView (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_moreSumaInfo (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_moreSurfInfo (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_moreViewerInfo (Widget w, XtPointer client_data, 
                                                            XtPointer callData);
void SUMA_ViewerInfo_destroyed (void *p);
void SUMA_ViewerInfo_open (void *p);
void SUMA_SumaInfo_destroyed (void *p);
void SUMA_SumaInfo_open (void *p);
SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTextShellStruct (
   void (*opencallback)(void *data), void *opendata, char *opendatatype, 
   void (*closecallback)(void*data), void *closedata, char *weblink);
SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTextShell (char *s, char *title, SUMA_CREATE_TEXT_SHELL_STRUCT *TextShellStruct);
void SUMA_cb_search_text(Widget widget, XtPointer client_data, 
                         XtPointer call_data);
char * SUMA_WriteStringToFile(char *fname, char *s, int, int);
void SUMA_SaveTextShell(Widget w, XtPointer ud, XtPointer cd);
void SUMA_RefreshTextShell(Widget w, XtPointer ud, XtPointer cd);
void SUMA_DestroyTextShell (Widget w, XtPointer ud, XtPointer cd);
void SUMA_WebTextShell(Widget w, XtPointer ud, XtPointer cd);
void SUMA_SurfInfo_open (void *SO);
void SUMA_SurfInfo_destroyed (void *SO);
void SUMA_cb_ToggleCaseSearch (Widget widget, XtPointer client_data, 
                               XtPointer call_data);
void SUMA_cb_Mask (Widget w, XtPointer client_data, XtPointer callData);
void SUMA_cb_helpUsage (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpWeb (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpIO_notify(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpEchoKeyPress(Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpMemTrace(Widget w, XtPointer data, XtPointer callData);
char * SUMA_FormatMessage (SUMA_MessageData *MD);
void SUMA_PopUpMessage (SUMA_MessageData *MD);
void SUMA_cb_helpMessageLog (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpSUMAGlobal (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpViewerStruct (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_helpSurfaceStruct (Widget w, XtPointer data, XtPointer callData);
void SUMA_cb_SetRenderMode(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
void SUMA_cb_SetTransMode(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
void SUMA_cb_SetATransMode(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
int SUMA_SetDsetViewMode(SUMA_ALL_DO *ado, int imenu, int update_menu) ;
int SUMA_SetDsetFont(SUMA_ALL_DO *ado, int imenu, int updatemenu);
int SUMA_SetDsetNodeRad(SUMA_ALL_DO *ado, int imenu, int updatemenu);
int SUMA_SetDsetThrough(SUMA_ALL_DO *ado, int imenu, int updatemenu);
int SUMA_SetDsetNodeCol(SUMA_ALL_DO *ado, int imenu, int updatemenu);
void SUMA_cb_SetDsetViewMode(Widget widget, XtPointer client_data, 
                              XtPointer call_data);
void SUMA_cb_SetDsetFont(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
void SUMA_cb_SetDsetNodeCol(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
void SUMA_cb_SetDsetNodeRad(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
void SUMA_cb_SetDsetThrough(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
void SUMA_cb_SetDsetGmatBord(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
int SUMA_SetDsetGmatBord(SUMA_ALL_DO *ado, int imenu, int updatemenu);
int SUMA_SetDsetTxtShad(SUMA_ALL_DO *ado, int imenu, int updatemenu); 
void SUMA_cb_SetDsetTxtShad(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
void SUMA_cb_ToolsDrawROI (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_cb_CloseDrawROIWindow(Widget w, XtPointer client_data, 
                                XtPointer call_data);
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
int SUMA_AllowArrowFieldMenus(int N, char *t);
void SUMA_CreateArrowField ( Widget pw, char *label,
                              float value, float vmin, float vmax, float vstep,
                              int cwidth, SUMA_VARTYPE type,
                              SUMA_Boolean wrap,
                              void (*NewValueCallback)(void * data), 
                              void *cb_data,
                              char *wname, char *hint, char *help,
                              SUMA_ARROW_TEXT_FIELD *AF);
void SUMA_CreateTextField ( Widget pw, char *label,
                              int cwidth, 
                              void (*NewValueCallback)(void *data),
                              char *wname, char *hint, char *help,
                              SUMA_ARROW_TEXT_FIELD *AF);
void SUMA_DrawROI_NewLabel (void * data);
void SUMA_ATF_change_value (XtPointer client_data, XtIntervalId *id);
void SUMA_ATF_start_stop (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_DrawROI_NewValue (void * data);
void SUMA_ATF_cb_label_change (Widget w, XtPointer client_data, 
                               XtPointer call_data);
void SUMA_ATF_SetString (SUMA_ARROW_TEXT_FIELD * AF);
void SUMA_ATF_GetString (SUMA_ARROW_TEXT_FIELD * AF, char *sbuf);
void SUMA_ATF_SetValue (SUMA_ARROW_TEXT_FIELD * AF);
void SUMA_ATF_cb_label_Modify (Widget w, XtPointer client_data, XtPointer call_data);
void SUMA_leave_EV( Widget w , XtPointer client_data ,
                  XEvent * ev , Boolean * continue_to_dispatch );
void SUMA_press_EV( Widget w , XtPointer client_data ,
                    XEvent * ev , Boolean * continue_to_dispatch );
void SUMA_ATF_cb_label_Focus (Widget w, XtPointer client_data, 
                    XtPointer call_data);
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
void SUMA_CreateScrolledList (char **clist, int N_clist, SUMA_Boolean Partial, 
                              SUMA_LIST_WIDGET *LW);
void SUMA_cb_CloseSwitchROI(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_SelectSwitchROI(Widget w, XtPointer data, XtPointer call_data);
void SUMA_FileSelection_popdown_cb (Widget w, XtPointer client_data, 
                                    XtPointer call_data);
void SUMA_FileSelection_file_select_cb(Widget dialog, XtPointer client_data, 
                                    XtPointer call_data);
SUMA_SELECTION_DIALOG_STRUCT *SUMA_CreateFileSelectionDialog (char *title, 
                                          SUMA_SELECTION_DIALOG_STRUCT **dlg);
SUMA_SELECTION_DIALOG_STRUCT *SUMA_CreateFileSelectionDialogStruct (
   Widget daddy, SUMA_FILE_SELECT_MODE Mode, SUMA_Boolean preserve,
   void (*SelectCallback)(char *filename, void *data), void *SelectData,
   void (*CancelCallback)(void *data), void *CancelData,
   char *FilePattern,
   SUMA_SELECTION_DIALOG_STRUCT *dlg);
void SUMA_FileSelection_Unmap_cb (Widget w, XtPointer client_data, 
                                  XtPointer call_data);
void SUMA_FreeFileSelectionDialogStruct(SUMA_SELECTION_DIALOG_STRUCT *dlg);
SUMA_PROMPT_DIALOG_STRUCT *SUMA_CreatePromptDialogStruct (
   SUMA_PROMPT_MODE Mode, char *TextFieldLabel, 
   char *init_selection, 
   Widget daddy, SUMA_Boolean preserve,
   SUMA_PROMPT_BUTTONS Return_button,
   void(*SelectCallback)(char *selection, void *data), void *SelectData,
   void(*CancelCallback)(void *data), void *CancelData,
   void(*HelpCallback)(void *data), void *HelpData,
   int(*VerifyFunction)(char *selection, void *data), void *VerifyData,
   SUMA_PROMPT_DIALOG_STRUCT *oprmpt);
SUMA_PROMPT_DIALOG_STRUCT *SUMA_CreatePromptDialog(
               char *title_extension, SUMA_PROMPT_DIALOG_STRUCT *prmpt);
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
void  SUMA_cb_ToggleManagementColPlaneWidget(Widget w, XtPointer data, 
                                                      XtPointer client_data);
void SUMA_cb_AllConts(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_SurfCont_SwitchPage (void *data);
void SUMA_cb_ColPlane_NewOrder (void *data);
int SUMA_ColPlane_NewOrder     (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                               int neworder, int cb_direct);
int SUMA_ColPlane_NewOrder_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                               int neworder, int cb_direct);
void SUMA_cb_ColPlane_NewOpacity (void *data);
int SUMA_ColPlane_NewOpacity     (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                                 float newopa, int cb_direct);
int SUMA_ColPlane_NewOpacity_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                                 float newopa, int cb_direct);
int SUMA_Tract_NewGray (SUMA_ALL_DO *ado, 
                           float newgray, int cb_direct );
void SUMA_cb_Tract_NewGray(void *data);
int SUMA_ColPlane_NewAlphaThresh_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                                  float newAlphaThresh, int cb_direct);
int SUMA_ColPlane_NewAlphaThresh (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                              float newAlphaThresh, int cb_direct);
void SUMA_cb_ColPlane_NewAlphaThresh (void *data);
void SUMA_cb_ColPlane_NewDimFact (void *data);
int SUMA_ColPlane_NewDimFact     (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                                 float newdimfact, int cb_direct);
int SUMA_ColPlane_NewDimFact_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                                 float newdimfact, int cb_direct);
int SUMA_ColPlane_NewNodeRadGain_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                                       float newdimfact, int cb_direct);
int SUMA_ColPlane_NewNodeRadGain (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                              float newdimfact, int cb_direct);
void SUMA_cb_ColPlane_NewNodeRadGain (void *data);

void SUMA_cb_ColPlaneShow_toggled (Widget w, XtPointer data, 
                                   XtPointer client_data);
void SUMA_cb_ColPlaneShowOneFore_toggled (Widget w, XtPointer data, 
                                          XtPointer client_data);
int SUMA_ColPlaneShowOneFore_Set (SUMA_ALL_DO *ado, SUMA_Boolean state, 
                                  int direct);
int SUMA_ColPlaneShowOneFore_Set_one (SUMA_ALL_DO *ado, SUMA_Boolean state,
                                      int direct);
void SUMA_cb_ColPlane_Delete(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_ColPlane_Load(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_Dset_Load(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_SurfCont_SwitchColPlane(Widget w, XtPointer data, 
                                                         XtPointer client_data);
void SUMA_cb_CloseSwitchColPlane(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_SelectSwitchColPlane(Widget w, XtPointer data, XtPointer call_data);
int SUMA_SelectSwitchColPlane(SUMA_ALL_DO *ado, 
                                  SUMA_LIST_WIDGET *LW, 
                                  int ichoice, SUMA_Boolean CloseShop, 
                                  int setmen);
int SUMA_SelectSwitchColPlane_one(SUMA_ALL_DO *ado, 
                                  SUMA_LIST_WIDGET *LW, 
                                  int ichoice, SUMA_Boolean CloseShop, 
                                  int setmen);
void SUMA_cb_ViewerCont_SwitchState (Widget w, XtPointer data, 
                                     XtPointer call_data);
void SUMA_cb_ViewerCont_SwitchGroup (Widget w, XtPointer data, 
                                     XtPointer call_data);
void SUMA_cb_SelectSwitchGroup(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_CloseSwitchGroup(Widget w, XtPointer data, XtPointer call_data);
SUMA_Boolean SUMA_InitializeColPlaneShell(SUMA_ALL_DO *SO, 
                                          SUMA_OVERLAYS *ColPlane);
SUMA_Boolean SUMA_InitializeColPlaneShell_SO(SUMA_SurfaceObject *SO, 
                                             SUMA_OVERLAYS *ColPlane);
SUMA_Boolean SUMA_InitializeColPlaneShell_GLDO(SUMA_ALL_DO *ado,
                                             SUMA_OVERLAYS *ColPlane);
SUMA_Boolean SUMA_InitializeColPlaneShell_TDO(SUMA_ALL_DO *ado,
                                             SUMA_OVERLAYS *ColPlane);
SUMA_Boolean SUMA_InitializeColPlaneShell_VO(SUMA_ALL_DO *ado,
                                             SUMA_OVERLAYS *ColPlane);
SUMA_Boolean SUMA_InitializeColPlaneShell_CO (
                  SUMA_ALL_DO *ado, 
                  SUMA_OVERLAYS *ColPlane);
SUMA_Boolean SUMA_InitializeColPlaneShell_MDO (SUMA_ALL_DO *ado, 
                                               SUMA_OVERLAYS *ColPlane);
SUMA_Boolean SUMA_UpdateColPlaneShellAsNeeded(SUMA_ALL_DO *SO);
SUMA_Boolean SUMA_Remixedisplay (SUMA_ALL_DO *ado);
void SUMA_cb_SetDrawROI_SaveMode(Widget w, XtPointer data, XtPointer call_data);
void SUMA_cb_SetDrawROI_SaveWhat(Widget w, XtPointer data, XtPointer call_data);
void SUMA_response(Widget widget, XtPointer client_data, XtPointer call_data);
int SUMA_PauseForUser(Widget parent, char *question, 
                      SUMA_WINDOW_POSITION pos, XtAppContext    *app,
                      int withpause, float timeoutsec);
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
SUMA_Boolean SUMA_LoadVolDO (char *fname, 
                        SUMA_DO_CoordUnits coord_type, SUMA_VolumeObject **VOp,
			byte PutVOinList);
int SUMA_Set_VO_Slice_Params(char *params, SUMA_VolumeObject *VO);
void SUMA_SiSi_I_Insist(void);
void SUMA_BuildMenuReset(int nchar);
void SUMA_MenuArrowFieldCallback (void *CB);
int SUMA_PageWidgetToNumber(Widget NB, Widget page);
#define SUMA_IS_CONTPAGE_ON_TOP(SC) ( (SC) && (SC)->Page \
                                      &&  SUMAg_CF->X->UseSameSurfCont \
                && SUMA_isCurrentContPage(SUMAg_CF->X->SC_Notebook,(SC)->Page) )
int SUMA_isCurrentContPage(Widget NB, Widget page);
SUMA_Boolean SUMA_SetSurfContPageNumber(Widget NB, int i);
int SUMA_NotebookLastPageNumber(Widget NB);
SUMA_Boolean SUMA_Init_SurfCont_SurfParam(SUMA_ALL_DO *SO);
SUMA_Boolean SUMA_Init_SurfCont_SurfParam_SO(SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_Init_SurfCont_SurfParam_GLDO(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_Init_SurfCont_SurfParam_TDO(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_Init_SurfCont_SurfParam_VO(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_Init_SurfCont_SurfParam_CO(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_Init_SurfCont_SurfParam_MDO(SUMA_ALL_DO *ado);
SUMA_Boolean SUMA_Init_SurfCont_SurfParam_ADO(SUMA_ALL_DO *ado);
int SUMA_NodeNeighborAlongScreenDirection(SUMA_SurfaceViewer *sv,
                                          SUMA_SurfaceObject *SO,
                                          int inode, double *dd);
SUMA_Boolean SUMA_World2ScreenCoords (SUMA_SurfaceViewer *sv, int N_List, 
                              double *WorldList, double *ScreenList, int *Quad, 
                              SUMA_Boolean ApplyXform, SUMA_Boolean ScreenY);
SUMA_Boolean SUMA_World2ScreenCoordsF (
                     SUMA_SurfaceViewer *sv, int N_List, float *WorldList, 
                     float *ScreenList, int *Quad, 
                     SUMA_Boolean ApplyXform, SUMA_Boolean ScreenY);
int * SUMA_DepthSort(float *NodeList, int N_Node, char **names, 
                     int cpxform, float *scrxyz);
SUMA_Boolean SUMA_DrawWindowLine(SUMA_SurfaceViewer *sv, int x0, int y0, 
                                                int x1, int y1, int meth);
void SUMA_cb_SetDrawROI_WhatDist(Widget widget, XtPointer client_data, 
                                 XtPointer call_data);
SUMA_Boolean SUMA_display_edge_striplist(DList *striplist, 
            SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO, char *DispOptions);
Widget SUMA_CloseBhelp_Frame( Widget parent,
                              XtCallbackProc close_callback, 
                              XtPointer close_data,
                              char *wname,
                              char *close_hint,
                              char *close_help,
                              XtCallbackProc help_callback,
                              XtPointer help_data,
                              char *help_hint,
                              char *help_help);
void SUMA_cb_XformPreProc_Save (Widget w, XtPointer data, 
                             XtPointer client_data);
void SUMA_cb_XformOpts_Apply (Widget w, XtPointer data, 
                             XtPointer client_data);
void SUMA_setIO_notify(int val);
int SUMA_RenderMode2RenderModeMenuItem(int Mode);
int SUMA_TransMode2TransModeMenuItem(int Mode);
SUMA_TRANS_MODES SUMA_ATransMode2TransMode(SUMA_ATRANS_MODES ii);
SUMA_ATRANS_MODES SUMA_TransMode2ATransMode(SUMA_TRANS_MODES ii);
int SUMA_ATransMode2ATransModeMenuItem(int Mode);
int SUMA_ShowMode2ShowModeMenuItem(int Mode);
int SUMA_ShowModeStr2ShowModeMenuItem(char *str); 
int SUMA_Font2FontMenuItem(int Mode);
int SUMA_FontStr2FontMenuItem(char *str); 
void * SUMA_Font2GLFont(int Mode);
int SUMA_Through2ThroughMenuItem(int Mode);
int SUMA_ThroughStr2ThroughMenuItem(char *str); 
int SUMA_NodeRad2NodeRadMenuItem(int Mode);
int SUMA_NodeRadStr2NodeRadMenuItem(char *str);
int SUMA_NodeCol2NodeColMenuItem(int Mode);
int SUMA_NodeColStr2NodeColMenuItem(char *str);
float *SUMA_NodeCol2Col(int Mode, float *here);
void SUMA_cb_SetDsetEdgeStip(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
int SUMA_SetDsetEdgeStip(SUMA_ALL_DO *ado, int imenu, int updatemenu); 
void SUMA_cb_SetDsetAlphaVal(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
int SUMA_SetDsetAlphaVal(SUMA_ALL_DO *ado, int imenu, int updatemenu); 
void SUMA_cb_SetTractMask(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
int SUMA_SetTractMask(SUMA_ALL_DO *ado, int imenu, int updatemenu);
void SUMA_cb_SetDsetEdgeThick(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
int SUMA_SetDsetEdgeThick(SUMA_ALL_DO *ado, int imenu, int updatemenu);
int SUMA_ColPlane_NewEdgeThickGain_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                                       float newdimfact, int cb_direct);
int SUMA_ColPlane_NewEdgeThickGain (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                              float newdimfact, int cb_direct);
void SUMA_cb_ColPlane_NewEdgeThickGain (void *data);
int SUMA_GDSET_ShowBundles ( SUMA_ALL_DO *ado, 
                                SUMA_Boolean state, int cb_direct);
void SUMA_cb_GDSET_ShowBundles_toggled (Widget w, XtPointer data, 
                                          XtPointer client_data);
int SUMA_GDSET_ShowUncon ( SUMA_ALL_DO *ado, 
                                SUMA_Boolean state, int cb_direct);
void SUMA_cb_GDSET_ShowUncon_toggled (Widget w, XtPointer data, 
                                          XtPointer client_data);
int SUMA_FlushPickBufferForDO(SUMA_ALL_DO *curDO);
SUMA_TRANS_MODES SUMA_1dig_to_T(int i);
int SUMA_T_to_1dig(SUMA_TRANS_MODES stm);
float SUMA_1dig_to_A(int i);
int SUMA_A_to_1dig(float v);
SUMA_Boolean SUMA_DispExpr_To_EvalExpr(char *expr, char *evale, char *tight);


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
SUMA_Boolean SUMA_WildcardChoice(int filetype, 
                  SUMA_SurfaceObject *SO, char wild[]); 
SUMA_Boolean SUMA_Set_Menu_Widget(SUMA_MENU_WIDGET *men, int i);
void SUMA_MaskTableCell_EV ( Widget w , XtPointer cd ,
                XEvent *ev , Boolean *continue_to_dispatch );
void SUMA_cb_createSurfaceCont_MDO(Widget w, XtPointer data, 
                                     XtPointer callData);
void SUMA_cb_SetMaskTableValue (void *data);
void SUMA_MaskTableLabel_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch );
void SUMA_MaskTableCell_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch );
void SUMA_MaskEvalTableCell_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch );
void SUMA_MaskEvalTableLabel_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch );
int SUMA_SetMaskEvalTableValueNew(  int row, int col,
                                char *s1, 
                                int setmen, 
                                int redisplay, 
                                SUMA_NUMERICAL_UNITS num_units);
char *SUMA_GetMaskEvalExpr(void);
void SUMA_cb_SetMaskEvalTableValue (void *data);
void SUMA_cb_UseMaskEval_toggled(Widget w, XtPointer data, 
                                 XtPointer client_data);
SUMA_Boolean SUMA_Set_UseMaskEval(int v, int redisp, int setmen);
void SUMA_MaskLenTableCell_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch );
void SUMA_MaskLenTableLabel_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch );
int SUMA_SetMaskLenTableValueNew(  int row, int col,
                                float, 
                                int setmen, 
                                int redisplay, 
                                SUMA_NUMERICAL_UNITS num_units);
void SUMA_cb_SetMaskLenTableValue (void *data);
void SUMA_cb_UseMaskLen_toggled(Widget w, XtPointer data, 
                                 XtPointer client_data);
SUMA_Boolean SUMA_Set_UseMaskLen(int v, int redisp, int setmen);
SUMA_Boolean SUMA_ModifyTable(SUMA_TABLE_FIELD *TF, int Nrows);
void SUMA_delete_mask_timeout_CB( XtPointer client_data , XtIntervalId * id);
void SUMA_cb_Mask_Delete(Widget wcall, XtPointer cd1, XtPointer cbs);
SUMA_Boolean SUMA_DeleteMask(char *ado_id);
SUMA_Boolean SUMA_DeleteAllMasks(char *labeled, SUMA_DO *dov, int N_dov);
SUMA_MaskDO * SUMA_findanyMDOp_inDOv(SUMA_DO *dov, int N_dov, int *dov_id);
SUMA_MaskDO * SUMA_findanyMDOp(int *dov_id);
DList *SUMA_AssembleMasksList(int withShadow);
DList *SUMA_AssembleMasksList_inDOv(SUMA_DO *dov, int N_dov, int withShadow);
SUMA_Boolean  SUMA_InitMasksTable(SUMA_X_SurfCont *SurfCont);
SUMA_Boolean  SUMA_InitMasksTable_row(SUMA_X_SurfCont *SurfCont, 
                                      SUMA_MaskDO *mdo, int row);
int SUMA_NewSymMaskDO(SUMA_ALL_DO *ado); 
int SUMA_ShadowMaskDO(SUMA_MaskDO **mdop);
int SUMA_SetTractStyle(SUMA_ALL_DO *ado, int imenu, int updatemenu);
void SUMA_cb_SetTractStyle(Widget widget, XtPointer client_data, 
                           XtPointer call_data);
void SUMA_CreateVrFields(  Widget parent,
                        char *tit, char *hint, char *help, 
                        int Nslc, SUMA_ALL_DO *ado,
                        void (*NewValueCallback)(void * data), void *cb_data,
                        SUMA_VR_FIELD *VrF);
void SUMA_cb_ShowVrF_toggled(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_VrSelect_toggled(Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_VSliceAtXYZ_toggled(Widget w, XtPointer data,XtPointer client_data);
void SUMA_leave_NslcField( Widget w , XtPointer client_data ,
                            XEvent * ev , Boolean * continue_to_dispatch );
void SUMA_VrF_cb_N_slc_change (  Widget w, XtPointer client_data, 
                                    XtPointer call_data);
void SUMA_VrF_SetNslcString(SUMA_VR_FIELD * VrF);
int SUMA_SetMaskTableValueNew(int row, int col,
                              char *s1,
                              int setmen, 
                              int redisplay,
                              SUMA_NUMERICAL_UNITS num_units);
SUMA_Boolean SUMA_Set_ADO_TransMode(SUMA_ALL_DO *ado, int i, 
                                    int delta, int update_widgets);
SUMA_Boolean SUMA_Set_ADO_RenderMode(SUMA_ALL_DO *ado, int i, int delta,
                                    int update_widgets );
int SUMA_Get_ADO_TransMode(SUMA_ALL_DO *ado);
void SUMA_cb_Masks_Save (Widget w, XtPointer data, XtPointer client_data);
void SUMA_cb_Masks_Load(Widget w, XtPointer data, XtPointer client_data);
SUMA_Boolean SUMA_LoadMultiMasks_eng (char *filename, 
                              int SetupOverlay, 
                              int LaunchDisplay);
void SUMA_LoadMultiMasks (char *filename, void *data);
void SUMA_SaveMultiMasks (char *filename, void *data);
SUMA_Boolean SUMA_SaveMultiMasks_eng (char *filename);
SUMA_Boolean SUMA_wait_till_visible(Widget w, int maxms);

/* 
   *************** Convolution utilities *************** 
   based on example in glut's convolve.c by  
   Tom McReynolds, SGI 
   *****************************************************
*/
void SUMA_C_identity(SUMA_C_FILTER *mat);
SUMA_C_FILTER * SUMA_C_newfilter(int rows, int cols);
void SUMA_C_free(SUMA_C_FILTER *mat);
void SUMA_C_resize(SUMA_C_FILTER *mat, int rows, int cols);
void SUMA_C_box(SUMA_C_FILTER *mat);
void SUMA_C_sobel(SUMA_C_FILTER *mat);
void SUMA_C_laplace(SUMA_C_FILTER *mat);
void SUMA_C_convolve(SUMA_SurfaceViewer *csv, SUMA_DO *dov, SUMA_C_FILTER *mat);

/* *************** End Convolution utilities *************** */
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

#define SUMA_DrawROI_DrawROIMode_help \
   "Toggles ROI drawing mode.\n" \
   "If turned on, then drawing is enabled \n"   \
   "and the cursor changes to a target. \n"  \
   "To draw, use the right mouse button. \n" \
   "If you want to pick a node without causing \n" \
   "a drawing action, use shift+right button.\n\n"\
   "After the draw ROI window is open, you can toggle \n"\
   "this button via :ref:`ctrl+d<LC_Ctrl+d>` also."
   
#define SUMA_DrawROI_ContROIMode_help \
   "Toggles ROI contour drawing \n" \
   "If turned on, then contours are drawn around filled ROIs.\n" \
   "Contours will *float* over other displayed datasets\n" 
   
#define SUMA_DrawROI_PenMode_help \
   "Toggles Pen drawing mode\n"\
   "If turned on, the cursor changes shape to a pen. \n" \
   "In the pen mode, drawing is done with button 1. \n"  \
   "This is for coherence with AFNI's pen drawing mode, \n" \
   "which is meant to work pleasantly with a stylus directly \n"  \
   "on the screen. In pen mode, you draw with the left mouse \n"  \
   "button and move the surface with the right button. \n"  \
   "To pick a node, use shift+left button. \n"  \
   "Pen mode only works when Draw Mode is enabled."

   #define SUMA_DrawROI_AfniLink_help \
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
"is set by the environment variable :ref:`SUMA_ROIColorMap<SUMA_ROIColorMap>`."
   
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
   
#define SUMA_SurfCont_DeleteMask_help   \
   "Deletes a Mask.\n" \
   "This operation is not reversible,\n"  \
   "(no Undo here) so you'll have to click twice."
   
#define SUMA_DrawROI_SaveFormat_help   \
   "File format for saving ROI:\n"  \
   "Format options are 1D and NIML.:LR:\n"   \
   "   The 1D format is the same one used in AFNI.:LR:\n"   \
   "It is an ASCII file with 2 values per line. The first \n"  \
   "value is a node index, the second is the node's value. \n" \
   "Needless, to say, this format does not support the storage \n"   \
   "of ROI auxiliary information such as Label and \n"   \
   "Parent Surface, etc., nor does it preserve the order in which \n"   \
   "nodes are traversed during a tracing. "\
   "For that you'll have to use :term:NIML.:LR:\n" \
   ": :NIML is a whole different story which will be documented \n"  \
   "(if necessary) in the future. Suffice it to say that in NIML \n" \
   "format you can store all the auxiliary information about \n"  \
   "each ROI, unlike with the .1D format.:LR:\n"   \
   ":SPX:"\
   "**But more importantly**, the NIML format allows you to preserve "  \
   "the order in which you traced the ROI. You can actually use \n" \
   ":ref:`Undo<ROICont->ROI->Undo>`/ref:`Undo<ROICont->ROI->Redo>` on "\
   "ROIs save in NIML format."\
   ":DEF:"\
   "But more importantly, the NIML format allows you to preserve\n"  \
   "--------------------  the order in which you traced the ROI. \n" \
   ":SPX:"\
   "This information can be later used for the purpose of sampling \n"  \
   "cortical activity along a particular path. This would be accomplished \n" \
   "with the aid of ROI2dataset's -nodelist:SPX:\\:SPX:* options, along with \n"\
   "ConvertDset's -node_select_1D option.:LR:\n"
   
#define SUMA_DrawROI_SaveWhat_help  \
   "Which ROIs to save?:LR:\n" \
   "   This: saves the current ROI. \n"   \
   "   All: saves all ROIs on surfaces related to the Parent \n"  \
   ":       :surface of the current ROI."
   
#define SUMA_DrawROI_WhatDist_help  \
   "Report length of drawn segments?:LR:\n" \
   "   -----: No distance calculations.:LR:\n"   \
   "   trace: Calculate distance along last \n"  \
   ":        :traced segment.\n" \
   "   all:   In addition to output from \n"  \
   ":        :'trace', calculate the shortest \n"   \
   ":        :distance between the first and \n"   \
   ":        :last node of the trace.:LR:\n"  \
   "   The results are output to the Message Log \n"  \
   ": :window (Help --> Message Log) with the following\n" \
   ": :information::LR:\n"  \
   "   n0, n1: Indices of first and last node forming \n" \
   ":         :the traced path.:LR:\n"  \
   "   N_n:    Number of nodes forming the trace.:LR:\n"  \
   "   lt:     Trace length calculated as the sum \n"  \
   ":         :of the distances from node to node.\n"   \
   ":         :This length is a slight overestimation \n"  \
   ":         :of the geodesic length. \n" \
   ":         :Units for all distances is the same as \n" \
   ":         :the units for surface coordinates. Usually \n"   \
   ":         :and hopefully in mm.:LR:\n" \
   "   lt_c:   Trace length corrected by a scaling factor\n"  \
   ":         :from [1] to better approximate geodesic \n"  \
   ":         :distances. Factor is 2/(1+sqrt(2)). \n" \
   ":         :Do not use this factor when N_n is small. \n"\
   ":         :Think of the extreme case when N_n is 2.:LR:\n"   \
   "   sd:     Shortest distance on the mesh (graph) \n" \
   ":         :between n0 and n1 using Dijkstra's algorithm.:LR:\n"   \
   "   sd_c:   Corrected shortest distance as for lt_c.:LR:\n"  \
   "\n"  \
   "   Note 1: sd and sd_c take some time to compute. That is \n"  \
   ":         :why they are only calculated when you select 'all'.:LR:\n"   \
   "   Note 2: The output is formatted to be cut and pasted into \n"  \
   ":         :a .1D file for ease of processing. \n"  \
   ":         :You can include all the comment lines that \n"   \
   ":         :start with '#'. But you cannot combine entries \n"  \
   ":         :from the output obtained using 'all' option with \n" \
   ":         :those from 'trace' since they produce different \n"  \
   ":         :numbers of values.\n"  \
   "\n"  \
   "   [1] Fischl et al, Neuroimage 9, 195-207 1999, \n" \
   ":     :Cortical Surface-Based Analysis."
   
#define SUMA_DrawROI_Save_help \
   "Save the Drawn ROI to disk.\n"  \
   "Choose the file format and what is to be\n"   \
   "saved from the two menus ahead.\n"  \
   ":LR:\n"  \
   SUMA_DrawROI_SaveFormat_help  \
   ":LR:\n"  \
   SUMA_DrawROI_SaveWhat_help

#define SUMA_closeDrawROI_help  \
   "Close Draw ROI window.\n" \
   "Current settings are preserved for the \n"   \
   "next time you reopen this window."
 


#define SUMA_help_help \
   "Click the hand\n"   \
   "on any button or \n"\
   "label, menu, etc. to\n"  \
   "get a little help. See also WHelp!"
   
#define SUMA_webhelp_help \
   "Click the coffee cup on any button \n"   \
   "label, menu, etc. to go to the corresponding online help.\n"  \
   "Clicking on table cells might take you to the help for the\n" \
   "entire table or the GUI section the table is in. You might\n" \
   "get a more focused result by clicking on the table's headings.\n"   \
   "At the moment, this button will not deliver any puppies."
    
   
#define SUMA_closeSumaCont_help \
   "Close SUMA controller window.\n"   \
   "Current settings are preserved\n"\
   "when controller is reopened."

   #define SUMA_LockSumaCont_help   \
"Set the crosshair lock \n"  \
"between viewers.:LR:\n" \
"   **-** No Lock: Crosshair only moves in viewer where you clicked.:LR:\n"  \
"   **i** Node index Lock: Crosshair jumps to the same node index on related "\
"surfaces (or objects) in other viewers. "\
"Linking in this case is topology based.:LR:\n"   \
"  **c** Coordinate Lock: Crosshair jumps to the same XYZ mm coordinate in "\
"other viewers. Linking in this case is geometry based)."
   
#define SUMA_LockViewSumaCont_help  \
   "Lock the view point of all viewers. Depress toggle button to link view "   \
   "point across viewers.:LR:\n" \
   "   * Surface rotation and translation in one viewer is reflected in all " \
   "linked viewers.:LR:\n"\
   "   * Liking is NOT done across viewers that are displaying objects of "\
   "different :ref:`embedding dimensions<Spec_EmbedDimension>` such as 3D "\
   "and 2D surfaces.\n" 
   
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

#define SUMA_helpXformCont_help   \
   "Open a searchable help window\n"   \
   "about using this interface.\n"

#define SUMA_closeViewerCont_help   \
   "Close Viewer controller window. "   \
   "Current settings are preserved "\
   "when controller is reopened."
  

#define  SUMA_moreViewerInfo_help  \
   "Opens a dialog with detailed " \
   "information about the surface "\
   "viewer."

#define SUMA_SurfCont_ColPlaneDim_hint \
   "Dimming factor to apply to colormap or color datasets." \

#define SUMA_SurfCont_NodeRadGain_hint \
   "Gain factor to apply to node radius." \

#define SUMA_SurfCont_EdgeThickGain_hint \
   "Gain factor to apply to edge thickness." \

#define SUMA_SurfCont_TractMask_hint \
   "Gray level (0--100) of tracts outside of mask (only for Msk --> Gry)" \

#define SUMA_SurfCont_ColPlaneOrder_hint \
   "Order of Dset's colorplane." \

#define SUMA_SurfCont_ColPlaneAlphaThresh_hint \
   "Threshold for voxel alpha value." \

#define SUMA_SurfCont_ColPlaneOpacity_hint \
   "Opacity of Dset's colorplane." \

   #define SUMA_VR_help \
"Set the number of slices used to render the volume. "\
"Volume rendering is done by slicing the volume from the far end along "\
"your vieweing direction to the front, blending successive images along "\
"the way. The more slices you use the better the result, something comparable "\
"to the maximum number of voxels in any of the directions would be a good "\
"start. Of course, the more slices, the slower the rendering.:LR:\n"\
"Blending is affected by :ref:`Avl<VolCont->Dset_Controls->Avl>` and "\
":ref:`Ath<VolCont->Dset_Controls->Ath>` settings."


#define SUMA_VR_hint \
   "Volume Rendering Settings (use BHelp for details)"

#define SUMA_SliceSelect_axial_help \
   "Select axial slice(s) to render.\n"\
   "If the dataset is oblique, that would be the slice that is closest\n"\
   "to the axial plane.\n"\
   "Move slider bar or enter slice number directly in adjoining field.\n"\
   "To show a stack of axial slices set the second text field to N:S\n"\
   "where N is the number of slices in the stack and S is the spacing\n"\
   "in number of slices between consecutive slices. The stack is centered\n"   \
   "on the chosen slice number. So when N > 1 and even, the 'selected' slice\n" \
   "is not rendered. In that case, set N to the next odd number to see it.\n"   \
   "To hide/show all displayed axial slices, use right-side toggle button."

#define SUMA_SliceSelect_axial_hint \
   "Select axial slice(s) to render (use BHelp for details)"
   
#define SUMA_SliceSelect_sagittal_help \
   "Select sagittal slice(s) to render.\n"\
   "If the dataset is oblique, that would be the slice that is closest\n"\
   "to the sagittal plane.\n"\
   "Move slider bar or enter slice number directly in adjoining field.\n"\
   "To show a stack of sagittal slices set the second text field to N:S\n"\
   "where N is the number of slices in the stack and S is the spacing\n"\
   "in number of slices between consecutive slices. The stack is centered\n"   \
   "on the chosen slice number. So when N > 1 and even, the 'selected' slice\n" \
   "is not rendered. In that case, set N to the next odd number to see it.\n"  \
   "To hide/show all displayed sagittal slices, use right-side toggle button."

#define SUMA_SliceSelect_sagittal_hint \
   "Select sagittal slice(s) to render (use BHelp for details)"

#define SUMA_SliceSelect_coronal_help \
   "Select coronal slice(s) to render.\n"\
   "If the dataset is oblique, that would be the slice that is closest\n"\
   "to the coronal plane.\n"\
   "Move slider bar or enter slice number directly in adjoining field.\n"\
   "To show a stack of coronal slices set the second text field to N:S\n"\
   "where N is the number of slices in the stack and S is the spacing\n"\
   "in number of slices between consecutive slices. The stack is centered\n"   \
   "on the chosen slice number. So when N > 1 and even, the 'selected' slice\n" \
   "is not rendered. In that case, set N to the next odd number to see it.\n"   \
   "To hide/show all displayed coronal slices, use right-side toggle button."

#define SUMA_SliceSelect_coronal_hint \
   "Select coronal slice(s) to render (use BHelp for details)"
   
#endif
