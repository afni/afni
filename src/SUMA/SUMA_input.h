#ifndef SUMA_INPUT_INCLUDED
#define SUMA_INPUT_INCLUDED

#define SUMA_BRUSH_BLOCK 500

#define SUMA_APPLE_AltOptMask		(1<<13)     /* empirically determined,
                                               no sign for it in X.h
                                               Would not work for catching
                                               alt/option+character because
                                               on mac, this combo maps to
                                               new characters. See XK_e
                                               for sample case */

#define SUMA_APPLE_KEY(s) ( (SUMA_iswordin_ci(s,"apl")==1) ? 1:0 )
#define SUMA_CTRL_KEY(s) ( (SUMA_iswordin_ci(s,"ctrl")==1) ? 1:0 )
#define SUMA_ALT_KEY(s) ( (SUMA_iswordin_ci(s,"alt")==1) ? 1:0 )
#define SUMA_AALT_KEY(s) ( (SUMA_APPLE_KEY(s) || SUMA_ALT_KEY(s) ) )
#define SUMA_SHIFT_KEY(s) ( (SUMA_iswordin_ci(s,"shift")==1) ? 1:0 )
#define SUMA_GL_ERRS {  \
   int m_error, m_cnt = 0;  \
   fprintf (SUMA_STDERR, \
            "%s (via SUMA_GL_ERRS): Looking for OpenGL errors ...\n", \
            FuncName); \
   \
   while ((m_error = glGetError()) != GL_NO_ERROR) { \
      ++m_cnt;   \
     fprintf ( SUMA_STDERR, "GL error %d: %s\n", \
               m_cnt, gluErrorString(m_error));  \
   }  \
   if (!m_cnt) {  \
      fprintf (SUMA_STDERR, "%s: No errors found.\n", FuncName);  \
   }\
}

typedef struct {
   char *identifier;
   char *prefix;
   char *type;
} SUMA_SAVE_LIST_EL;

typedef struct sVector3
{
    float x, y, z;
} Vector3;


int SUMA_KeyPress(char *key, char *keyname);
int SUMA_bracketleft_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_bracketright_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_space_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_period_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_comma_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F1_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F2_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F3_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F4_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F5_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F6_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F7_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F8_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F9_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_F10_Key(SUMA_SurfaceViewer *sv, char *key,char *callmode,char *strgval);
int SUMA_F11_Key(SUMA_SurfaceViewer *sv,char *key,char *callmode, char *strgval);
int SUMA_F12_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_A_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_B_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_D_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_G_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_J_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode, char *strgval);
int SUMA_L_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode, char *strgval);
int SUMA_M_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_N_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_O_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_P_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_R_Key(SUMA_SurfaceViewer *sv, char *key, char *caller);
int SUMA_T_Key(SUMA_SurfaceViewer *sv, char *key, char *caller);
int SUMA_U_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode);
int SUMA_W_Key(SUMA_SurfaceViewer *sv, char *key, char *caller);
int SUMA_Z_Key(SUMA_SurfaceViewer *sv, char *key, char *caller);
int SUMA_Up_Key(SUMA_SurfaceViewer *sv, char *key, char *caller);
int SUMA_Down_Key(SUMA_SurfaceViewer *sv, char *key, char *caller);
int SUMA_Left_Key(SUMA_SurfaceViewer *sv, char *key, char *caller);
int SUMA_Right_Key(SUMA_SurfaceViewer *sv, char *key, char *caller);
SUMA_EVENT *SUMA_RecordEvent( XEvent *event, SUMA_EVENT *ev);
void SUMA_ShowEvent(SUMA_EVENT *ev, int opt, char *pre);
char *SUMA_KeyType2String(int kt);
char *SUMA_Butts2String(SUMA_EVENT *ev);
int SUMA_Plain_Event(SUMA_EVENT *ev);
int SUMA_Alt_Event(SUMA_EVENT *ev);
int SUMA_ShftAlt_Event(SUMA_EVENT *ev);
int SUMA_ContAlt_Event(SUMA_EVENT *ev);
int SUMA_ShftContAlt_Event(SUMA_EVENT *ev);
int SUMA_Shft_Event(SUMA_EVENT *ev);
int SUMA_Cont_Event(SUMA_EVENT *ev);
int SUMA_ShftCont_Event(SUMA_EVENT *ev);
void SUMA_input(Widget w, XtPointer clientData, XtPointer callData) ;
void SUMA_Show_SaveList(DList *SL, char *head);
int SUMA_Add_to_SaveList(DList **SLp, char *type, char *ident, char *prefix);
void SUMA_free_Save_List_El(void *selu);
int SUMA_Apply_PR(SUMA_SurfaceViewer *sv, SUMA_PICK_RESULT **PR);
int SUMA_ComputeLineSurfaceIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov,
                                      int IgnoreSameNode, SUMA_ALL_DO **ado);
int SUMA_ComputeLineMaskIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov,
                                      int IgnoreSameNode, SUMA_ALL_DO **pado);
int SUMA_Apply_PR_SO(SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO,
                     SUMA_PICK_RESULT **PRi);
int SUMA_MarkLineSurfaceIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov,
                                    int IgnoreSameNode);
int SUMA_MarkLineCutplaneIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov,
                                    int IgnoreSameNode);
int SUMA_ComputeLineVOslicesIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov,
                                       int IgnoreSameNode, SUMA_ALL_DO **pado);
int SUMA_ComputeLineVOvrIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov,
                                       int IgnoreSameNode, SUMA_ALL_DO **pado);
int SUMA_Apply_PR_VO(SUMA_SurfaceViewer *sv, SUMA_VolumeObject *VO,
                     SUMA_PICK_RESULT **PRi);
int SUMA_MarkLineVOslicesIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov,
                                    int IgnoreSameNode);
int SUMA_ComputeLineDOsIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov,
                                  int IgnoreSameNode, SUMA_ALL_DO **pado);
int SUMA_Apply_PR_DO(SUMA_SurfaceViewer *sv, SUMA_ALL_DO *DO,
                     SUMA_PICK_RESULT **PRi);
int SUMA_MarkLineDOsIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov,
                               int IgnoreSameNode);
SUMA_Boolean SUMA_PickBuffer(SUMA_SurfaceViewer *sv, int action, SUMA_DO *dov);
SUMA_Boolean SUMA_ADO_Flush_Pick_Buffer(SUMA_ALL_DO *ado,SUMA_SurfaceViewer *sv);
SUMA_Boolean SUMA_Bundle_Pick_Intersect(void *p, char ptype, int Tmask,
                     SUMA_SurfaceViewer *sv,
                     float *scpx, int crude,
                     int *tmin, int *pmin, float *frmin, float *mindist);
SUMA_PICK_RESULT *SUMA_WhatWasPicked(SUMA_SurfaceViewer *sv, GLubyte *colid,
                                SUMA_COLID_OFFSET_DATUM **ucodf, int i, int j,
                                SUMA_PICK_RESULT *PR);
SUMA_PICK_RESULT *SUMA_WhatWasPicked_FrameSO(SUMA_SurfaceViewer *sv, int ido);
SUMA_PICK_RESULT *SUMA_free_PickResult(SUMA_PICK_RESULT *PR);
SUMA_PICK_RESULT *SUMA_New_Pick_Result(SUMA_PICK_RESULT *PR);
SUMA_Boolean SUMA_ADO_StorePickResult(SUMA_ALL_DO *ado, SUMA_PICK_RESULT **PRP);
SUMA_PICK_RESULT * SUMA_ADO_GetPickResult(SUMA_ALL_DO *ado, char *variant);
void SUMA_Show_Pick_Colid_List(DList *pick_colid_list, FILE *fout);
char *SUMA_Pick_Colid_List_Info (DList *pick_colid_list);
SUMA_Boolean SUMA_MarkPickInBuffer4(SUMA_SurfaceViewer *sv, int InViewer,
                                    char *OnDisk);
SUMA_Boolean SUMA_GetColidInPickBuffer4(GLubyte *pix, int Ni, int Nj,
                                        int *ii, int *ji,
                                        int maxlay, GLubyte *colid);
void SUMA_momentum(XtPointer clientData, XtIntervalId *id);
SUMA_Boolean  SUMA_AddToBrushStroke (SUMA_SurfaceViewer *sv, int x, int y, GLdouble *NP, GLdouble *FP, SUMA_Boolean Show);
SUMA_Boolean  SUMA_CreateBrushStroke (SUMA_SurfaceViewer *sv);
void  SUMA_ClearBrushStroke (SUMA_SurfaceViewer *sv);
void SUMA_ShowBrushStroke (SUMA_SurfaceViewer *sv, FILE *out);
void SUMA_DrawBrushStroke (SUMA_SurfaceViewer *sv, SUMA_Boolean Incremental);
void SUMA_SetSVForegroundColor (SUMA_SurfaceViewer *sv, const char *Color);
SUMA_DRAWN_ROI * SUMA_ProcessBrushStroke (SUMA_SurfaceViewer *sv, SUMA_BRUSH_STROKE_ACTION BsA);
SUMA_Boolean SUMA_BrushStrokeToNodeStroke (SUMA_SurfaceViewer *sv);
SUMA_ROI_DATUM *SUMA_NodeStrokeToConnectedNodes (SUMA_SurfaceViewer *sv);
SUMA_ROI_DATUM *SUMA_LinkTailNodeToNodeStroke (SUMA_SurfaceViewer *sv, SUMA_DRAWN_ROI *DrawnROI);
DListElmt * SUMA_PushActionStack (  DList *ActionStack, DListElmt *StackPos,
                                    SUMA_ACTION_RESULT (*ActionFunction)(void *ActionData, SUMA_ACTION_POLARITY Pol),
                                    void *ActionData,
                                    void (*ActionDataDestructor)(void *Actiondata));
SUMA_ACTION_RESULT SUMA_AddToTailROIDatum (void *data, SUMA_ACTION_POLARITY Pol);
SUMA_ACTION_RESULT SUMA_AddToTailJunctionROIDatum (void *data, SUMA_ACTION_POLARITY Pol);
void SUMA_DestroyROIActionData (void *data);
SUMA_ROI_DATUM *SUMA_LinkThisNodeToNodeInStroke (SUMA_SurfaceViewer *sv, int NonSurf, DListElmt *El);
DListElmt * SUMA_UndoAction (DList *ActionStack, DListElmt *StackPos);
DListElmt * SUMA_RedoAction (DList *ActionStack, DListElmt *StackPos);
void SUMA_FreeBSDatum (void *bsd);
SUMA_BRUSH_STROKE_DATUM * SUMA_CreateBSDatum(void);
SUMA_ACTION_RESULT SUMA_AddFillROIDatum (void *data, SUMA_ACTION_POLARITY Pol);
SUMA_ACTION_RESULT SUMA_FinishedROI (void *data, SUMA_ACTION_POLARITY Pol);
void SUMA_LookAtCoordinates (char *s, void *data);
void SUMA_SetScreenClip (char *s, void *data);
void SUMA_SetObjectClip (char *s, void *data);
void SUMA_SetClip (char *s, SUMA_SurfaceViewer *data, SUMA_CLIP_PLANE_TYPES tp);
void SUMA_SetLight0 (char *s, void *data);
void SUMA_SV_SetRenderOrder(char *s, void *data);
void SUMA_JumpIndex (char *s, void *data);
void SUMA_JumpIndex_SO (char *s, SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO);
void SUMA_JumpIndex_GDSET (char *s, SUMA_SurfaceViewer *sv,
                           SUMA_DSET *dset, char *variant);
void SUMA_JumpIndex_TDO (char *s, SUMA_SurfaceViewer *sv,
                           SUMA_TractDO *tdo);
void SUMA_JumpIndex_CO (char *s, SUMA_SurfaceViewer *sv,
                        SUMA_CIFTI_DO *co);
void SUMA_JumpIndex_VO (char *s, SUMA_SurfaceViewer *sv,
                        SUMA_VolumeObject *vo);
void SUMA_JumpIndex_MDO (char *s, SUMA_SurfaceViewer *sv, SUMA_MaskDO *mo);
void SUMA_JumpXYZ (char *s, void *data);
void SUMA_JumpFocusNode (char *s, void *data);
void SUMA_JumpFocusFace (char *s, void *data);
void SUMA_HighlightBox (char *s, void *data);
void SUMA_SetNumForeSmoothing (char *s, void *data);
void SUMA_SetNumFinalSmoothing (char *s, void *data);
void SUMA_SetRotCenter (char *s, void *data);

// PDL
void clipPlaneTransform(int deltaTheta, int deltaPhi, int deltaPlaneD, Bool flip, int activePlane,
    Bool toggleOffOn);
void drawClipPlane(float planeA, float planeB, float planeC, float planeD, Widget w,
    SUMA_SurfaceViewer *sv, int isv);
void getFourCoordsJustInsideClipPlane(float *plane, float points[4][3]);

// static int clipPlaneIndex;
static Bool clipPlaneIdentificationMode;

/*!
   \brief Macro to retrieve the first node and first triangle intersected by a brushstroke
   Since not all elements of brushstroke have a surface node, the macro continues searching until
   a non-negative SurfNode is found. Note that SurfTri is not monitored.

   \param bs (DList *) containing brushstroke
   \param fn (int) the first SurfNode (>= 0) of the list
   \param ft (int) the value of SurfTri corresponding to fn
   \param NE (DList_Elmt *) pointer to list element where the first SurfNode was found
*/
#define SUMA_BS_FIRST_SURF_NODE(bs, fn, ft, NE)   \
   {  \
      SUMA_BRUSH_STROKE_DATUM *m_bsd=NULL;  \
      \
      if (!dlist_size(bs)) {  \
         fn = -1; \
         ft = -1; \
      } else { \
         do {  \
            if (!NE) NE = dlist_head(bs);   \
            else NE = NE->next; \
            m_bsd = (SUMA_BRUSH_STROKE_DATUM *)NE->data;  \
            fn = m_bsd->SurfNode;   \
            ft = m_bsd->SurfTri; \
         } while ( (NE != dlist_tail(bs)) && fn < 0);   \
      }  \
   }

/*!
   \brief Find the next element in bs that contains a non-negative value for SurfNode
   returns NULL in El if failed to find such an element
*/
#define SUMA_BS_NEXT_SURF_NODE(bs, oEl, El) \
   {  \
      SUMA_BRUSH_STROKE_DATUM *m_bsd=NULL;  \
      \
      if (oEl == dlist_tail (bs))   {\
         El = NULL;  \
      }  else {   \
         El = oEl;   \
         do {  \
            El = El->next;   \
            m_bsd = (SUMA_BRUSH_STROKE_DATUM *)El->data; \
         }  while (El != dlist_tail(bs) && m_bsd->SurfNode < 0);  \
         if (m_bsd->SurfNode < 0) El = NULL; \
      }  \
   }

#define SUMA_BS_COUNT_SURF_NODES(bs, N_SurfNode)  \
   {  \
      SUMA_BRUSH_STROKE_DATUM *m_bsd=NULL;  \
      DListElmt *m_El=NULL;  \
      \
      N_SurfNode = 0;   \
      do {  \
            if (!m_El) m_El = dlist_head(bs);   \
            else m_El = m_El->next;   \
            m_bsd = (SUMA_BRUSH_STROKE_DATUM *)m_El->data; \
            if (m_bsd->SurfNode >= 0) ++N_SurfNode;   \
         } while (m_El != dlist_tail(bs));   \
   }

#define SUMA_REMOVE_NEXT_NON_DECIMATED(bs, Eli, Eln)   \
   {  \
      SUMA_BRUSH_STROKE_DATUM *m_bsd=NULL;  \
      SUMA_Boolean m_wasDecimated;  \
      do {  \
         Eln = Eli->next;   \
         m_bsd = (SUMA_BRUSH_STROKE_DATUM *)Eln->data; \
         if (m_bsd->Decimated) { \
            m_wasDecimated = YUP;   \
            dlist_remove (bs, Eln, (void *)(&m_bsd)); \
            SUMA_FreeBSDatum (m_bsd);  \
         } else {  \
            m_wasDecimated = NOPE;   \
         }  \
      } while (Eln != dlist_tail(bs) && m_wasDecimated);   \
      if (m_bsd->Decimated) Eln = NULL;   \
   }

#endif


