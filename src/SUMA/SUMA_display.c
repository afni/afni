#include "SUMA_suma.h"
#include "coxplot.h"
#include "SUMA_plot.h"
 
/* the method for hiding a surface viewer (and other controllers), used to have three options prior to Fri Jan  3 10:21:52 EST 2003
Now only SUMA_USE_WITHDRAW and NOT SUMA_USE_DESTROY should be used*/
#define SUMA_USE_WITHDRAW

extern SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
extern int SUMAg_N_SVv; /*!< Number of SVs realized by X  */
/* extern SUMA_SurfaceViewer *SUMAg_cSV; */ /* This variable is no longer used in this file Tue Aug 13 15:27:53 EDT 2002*/ 
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_CommonFields *SUMAg_CF; 

/*! Parts of the code in this file are based on code from the motif programming manual.
    This fact is mentioned at relevant spots in the code but the complete copyright 
    notice is only copied here for brevity:
       * Written by Dan Heller and Paula Ferguson.  
       * Copyright 1994, O'Reilly & Associates, Inc.
       * Permission to use, copy, and modify this program without
       * restriction is hereby granted, as long as this copyright
       * notice appears in each copy of the program source code.
       * This program is freely distributable without licensing fees and
       * is provided without guarantee or warrantee expressed or implied.
       * This program is -not- in the public domain.
*/

/*! Widget initialization */
static int snglBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 16,
   GLX_RED_SIZE, 1, GLX_BLUE_SIZE, 1, GLX_GREEN_SIZE, 1,  None};
static int dblBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 16,
  GLX_RED_SIZE, 1, GLX_BLUE_SIZE, 1, GLX_GREEN_SIZE, 1,
  GLX_DOUBLEBUFFER,
  None};

static String fallbackResources_default[] = {
   "*glxarea*width: 300", "*glxarea*height: 300",
   "*frame*x: 20", "*frame*y: 20",
   "*frame*topOffset: 20", "*frame*bottomOffset: 20",
   "*frame*rightOffset: 20", "*frame*leftOffset: 20",
   "*frame*shadowType: SHADOW_IN", 
   "*fontList:              9x15bold=charset1"    ,
   "*pbar*fontList:         6x10=charset1"        ,
   "*imseq*fontList:        7x13=charset1"        ,
   "*font8*fontList:        8x13=charset1"        ,
   "*font7*fontList:        7x13=charset1"        ,
   "*font6*fontList:        6x10=charset1"        ,
   "*table*fontList:        9x15bold=charset1"    ,
   "*background:            gray50"               ,
   "*menu*background:       gray30"               ,
   "*borderColor:           gray30"               ,
   "*foreground:            black"               ,
   "*borderWidth:           0"                    ,
   "*troughColor:           green"                ,
   "*XmLabel.translations:  #override<Btn2Down>:" , /* Motif 2.0 bug */
   "*help*background:       black"                ,
   "*help*foreground:       yellow"               ,
   "*help*helpborder:       False"                ,
   "*help*waitPeriod:       1066"                 ,
   "*help*fontList:         9x15bold=charset1"    ,
   "*cluefont:              9x15bold"             ,
   "*help*cancelWaitPeriod: 50"                   ,
   "*XmList.translations: #override"                /* 24 Feb 2007 */
        "<Btn4Down>: ListPrevItem()\\n"
        "<Btn5Down>: ListNextItem()"                  ,
   "*XmText.translations: #override"
        "<Btn4Down>: previous-line()\\n"
        "<Btn5Down>: next-line()"                     ,
   "*XmScrollBar.translations: #override"
        "<Btn4Down>: IncrementUpOrLeft(0) IncrementUpOrLeft(1)\\n"
        "<Btn5Down>: IncrementDownOrRight(1) IncrementDownOrRight(0)" ,
  NULL
}; /* if you change default width and height, make sure you change SV->X->WIDTH & SV->X->HEIGHT in SUMA_SVmanip */

static String fallbackResources_AFNI[] = {
   "*glxarea*width: 300", "*glxarea*height: 300",
   "*frame*x: 20", "*frame*y: 20",
   "*frame*topOffset: 20", "*frame*bottomOffset: 20",
   "*frame*rightOffset: 20", "*frame*leftOffset: 20",
   "*frame*shadowType: SHADOW_IN", 
   "*fontList:              9x15bold=charset1"    ,
   "*pbar*fontList:         6x10=charset1"        ,
   "*imseq*fontList:        7x13=charset1"        ,
   "*font8*fontList:        8x13=charset1"        ,
   "*font7*fontList:        7x13=charset1"        ,
   "*font6*fontList:        6x10=charset1"        ,
   "*background:            gray30"               ,
   "*menu*background:       gray30"               ,
   "*borderColor:           gray30"               ,
   "*foreground:            yellow"               ,
   "*borderWidth:           0"                    ,
   "*troughColor:           green"                ,
   "*XmLabel.translations:  #override<Btn2Down>:" , /* Motif 2.0 bug */
   "*help*background:       black"                ,
   "*help*foreground:       yellow"               ,
   "*help*helpborder:       False"                ,
   "*help*waitPeriod:       1066"                 ,
   "*help*fontList:         9x15bold=charset1"    ,
   "*cluefont:              9x15bold"             ,
   "*help*cancelWaitPeriod: 50"                   ,
   "*XmList.translations: #override"                /* 24 Feb 2007 */
        "<Btn4Down>: ListPrevItem()\\n"
        "<Btn5Down>: ListNextItem()"                  ,
   "*XmText.translations: #override"
        "<Btn4Down>: previous-line()\\n"
        "<Btn5Down>: next-line()"                     ,
   "*XmScrollBar.translations: #override"
        "<Btn4Down>: IncrementUpOrLeft(0) IncrementUpOrLeft(1)\\n"
        "<Btn5Down>: IncrementDownOrRight(1) IncrementDownOrRight(0)" ,
  NULL
}; /* if you change default width and height, make sure you change SV->X->WIDTH & SV->X->HEIGHT in SUMA_SVmanip */

static String fallbackResources_EURO[] = {
   "*glxarea*width: 300", "*glxarea*height: 300",
   "*frame*x: 20", "*frame*y: 20",
   "*frame*topOffset: 20", "*frame*bottomOffset: 20",
   "*frame*rightOffset: 20", "*frame*leftOffset: 20",
   "*frame*shadowType: SHADOW_IN", 
   "*fontList:              9x15=charset1"    ,
   "*font8*fontList:        8x13=charset1"        ,
   "*font7*fontList:        7x13=charset1"        ,
   "*font6*fontList:        6x10=charset1"        ,
   "*table*fontList:        9x15bold=charset1"    ,
   "*pbar*fontList:         6x10=charset1"        ,
   "*imseq*fontList:        7x13=charset1"        ,
   "*background:            black"               ,
   "*menu*background:       gray70"               ,
   "*borderColor:           gray70"               ,
   "*foreground:            white"               ,
   "*borderWidth:           0"                    ,
   "*troughColor:           LightCyan2"                ,
   "*XmLabel.translations:  #override<Btn2Down>:" , /* Motif 2.0 bug */
   "*help*background:       black"                ,
   "*help*foreground:       yellow"               ,
   "*help*helpborder:       False"                ,
   "*help*waitPeriod:       1066"                 ,
   "*help*fontList:         9x15=charset1"    ,
   "*cluefont:              9x15"             ,
   "*help*cancelWaitPeriod: 50"                   ,
   "*hotcolor:              blue2"               , 
   "*XmList.translations: #override"                /* 24 Feb 2007 */
        "<Btn4Down>: ListPrevItem()\\n"
        "<Btn5Down>: ListNextItem()"                  ,
   "*XmText.translations: #override"
        "<Btn4Down>: previous-line()\\n"
        "<Btn5Down>: next-line()"                     ,
   "*XmScrollBar.translations: #override"
        "<Btn4Down>: IncrementUpOrLeft(0) IncrementUpOrLeft(1)\\n"
        "<Btn5Down>: IncrementDownOrRight(1) IncrementDownOrRight(0)" ,
  NULL
}; /* if you change default width and height, make sure you change SV->X->WIDTH & SV->X->HEIGHT in SUMA_SVmanip */

static String fallbackResources_Bonaire[] = {
   "*glxarea*width: 300", "*glxarea*height: 300",
   "*frame*x: 20", "*frame*y: 20",
   "*frame*topOffset: 20", "*frame*bottomOffset: 20",
   "*frame*rightOffset: 20", "*frame*leftOffset: 20",
   "*frame*shadowType: SHADOW_IN", 
   "*fontList:              9x15bold=charset1"    ,
   "*font8*fontList:        8x13=charset1"        ,
   "*font7*fontList:        7x13=charset1"        ,
   "*font6*fontList:        6x10=charset1"        ,
   "*table*fontList:        9x15bold=charset1"    ,
   "*pbar*fontList:         6x10=charset1"        ,
   "*imseq*fontList:        7x13=charset1"        ,
   "*background:            navy"               ,
   "*menu*background:       gray30"               ,
   "*borderColor:           gray30"               ,
   "*foreground:            LightCyan2"               ,
   "*borderWidth:           0"                    ,
   "*troughColor:           green"                ,
   "*XmLabel.translations:  #override<Btn2Down>:" , /* Motif 2.0 bug */
   "*help*background:       black"                ,
   "*help*foreground:       yellow"               ,
   "*help*helpborder:       False"                ,
   "*help*waitPeriod:       1066"                 ,
   "*help*fontList:         9x15bold=charset1"    ,
   "*cluefont:              9x15bold"             ,
   "*help*cancelWaitPeriod: 50"                   ,
   "*hotcolor:              azure"               , 
   "*XmList.translations: #override"                /* 24 Feb 2007 */
        "<Btn4Down>: ListPrevItem()\\n"
        "<Btn5Down>: ListNextItem()"                  ,
   "*XmText.translations: #override"
        "<Btn4Down>: previous-line()\\n"
        "<Btn5Down>: next-line()"                     ,
   "*XmScrollBar.translations: #override"
        "<Btn4Down>: IncrementUpOrLeft(0) IncrementUpOrLeft(1)\\n"
        "<Btn5Down>: IncrementDownOrRight(1) IncrementDownOrRight(0)" ,
  NULL
}; /* if you change default width and height, make sure you change SV->X->WIDTH & SV->X->HEIGHT in SUMA_SVmanip */

/* DO NOT USE THESE, keep here for record */
static char SUMA_TEXT_WIDGET_TRANSLATIONS[] = 
   "  <Btn4Down>: previous-line()   \n\
      <Btn5Down>: next-line()       ";
static char SUMA_SCR_LIST_WIDGET_TRANSLATIONS[] = 
   "  <Btn4Down>: ListPrevItem() \n\
      <Btn5Down>: ListNextItem() ";

      

/*!

- use matlab script readXcol to choose different color settings
*/

String *SUMA_get_fallbackResources ()
{
   static char FuncName[]={"SUMA_get_fallbackResources"};
   
   SUMA_ENTRY;

   switch (SUMAg_CF->X->X_Resources) {
      case SXR_Afni:
         SUMA_RETURN (fallbackResources_AFNI);
         break;
      case SXR_Euro:
         SUMA_RETURN (fallbackResources_EURO);
         break;
      case SXR_Bonaire:
         SUMA_RETURN (fallbackResources_Bonaire);
         break;
      case SXR_default:
      default:
         SUMA_RETURN (fallbackResources_default);
   }

}

static SUMA_Boolean PleaseDoMakeCurrent;

void SUMA_SiSi_I_Insist(void)
{
   PleaseDoMakeCurrent = YUP;
}

Boolean
SUMA_handleRedisplay(XtPointer closure)
{
   static char FuncName[]={"SUMA_handleRedisplay"};
   static int Last_isv = -1;
   int isv;
   SUMA_SurfaceViewer *sv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (LocalHead) {
      SUMA_REPORT_WICH_WIDGET_SV ((Widget)closure);
   }
   
   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV((Widget)closure, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   
   if (Last_isv >= 0) { /* first time function is called, no use for this variable yet */
      if (isv != Last_isv) {/* need to call glXMakeCurrent */
         if (!sv->Open) {
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Redisplay request for a closed window. Skipping.\n", FuncName);
            SUMA_RETURN(NOPE);
         }else {
            PleaseDoMakeCurrent = YUP;
         }
      }
   } 
   
   if (PleaseDoMakeCurrent) {
      /* An OpenGL rendering context is a port through which all OpenGL commands pass. */
      /* Before rendering, a rendering context must be bound to the desired drawable using glXMakeCurrent. OpenGL rendering commands implicitly use the current bound rendering context and one drawable. Just as a
         program can create multiple windows, a program can create multiple OpenGL rendering contexts. But a thread can only be bound to one rendering context and drawable at a time. Once bound, OpenGL rendering can begin.
         glXMakeCurrent can be called again to bind to a different window and/or rendering context. */
      if (!glXMakeCurrent (sv->X->DPY, XtWindow((Widget)closure), sv->X->GLXCONTEXT)) {
               fprintf (SUMA_STDERR, "Error %s: Failed in glXMakeCurrent.\n \tContinuing ...\n", FuncName);
      }
      PleaseDoMakeCurrent = NOPE;
   }
   
   Last_isv = isv; /* store last surface viewer to call display */
   /* call display for the proper surface viewer*/
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Calling SUMA_display with SV[%d], Pointer %p.\n", FuncName, isv, sv); 
   SUMA_display(sv, SUMAg_DOv);
   sv->X->REDISPLAYPENDING = 0;
   
   if (SUMAg_N_SVv > 1) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Forcing display to finish.\n", FuncName);
      /* When multiple viewers are open, the picking does not work at times if you click around rapidly.
      The problem seems to be caused by OpenGL being in a state corresponding to that of the last viewer 
      visited before coming to the current viewer. Forcing gl to render after a redisplay pending for a 
      certain viewer is placed seems to reduce this problem significantly so this fix will be adopted
      until a better one comes along. This call does reduce the apparent speed of the display and might
      cause momentum motion to be more blocky but the overload is minimal for regular use.*/
      glFinish();
   }

   SUMA_RETURN(YUP);
}

/*!
   \brief, set culling for viewer:
   action takes one of: "Apply" or "Restore": Settings based on sv->BF_Cull
                                             With Apply, you get a notice on the screen,
                                             use it sparingly
                        "Off" or "Hold": Turn off culling, regardless of sv->BF_Cull
                        "Front": Turn on front face culling, regardless of sv->BF_Cull
                        "Back": Turn on back face culling, regardless of sv->BF_Cull
*/
void SUMA_CullOption(SUMA_SurfaceViewer *sv, const char *action)
{
   static char FuncName[]={"SUMA_CullOption"};
   char ac;
   
   SUMA_ENTRY;
   
   if (!action) {
      SUMA_S_Err("NULL action!");
      SUMA_RETURNe;
   }
   
   ac = SUMA_TO_LOWER_C(action[0]);
   
   if (ac == 'o' || ac == 'h') {
      glDisable(GL_CULL_FACE);   
   } else if ( ac == 'b') {
      glCullFace (GL_BACK);
      glEnable (GL_CULL_FACE);
   } else if ( ac == 'f') {
      glCullFace (GL_FRONT);
      glEnable (GL_CULL_FACE);
   } else if ( ac == 'a' || ac == 'r') {   
      switch (sv->BF_Cull) {
         case 0:
            glDisable(GL_CULL_FACE);
            if (ac == 'a') { SUMA_SLP_Note ("Culling disabled."); }
            break;
         case 1:
            glCullFace (GL_BACK);
            glEnable (GL_CULL_FACE);
            if (ac == 'a') { SUMA_SLP_Note ("BackFace Culling enabled."); }
            break;
         case 2:
            glCullFace (GL_FRONT);
            glEnable (GL_CULL_FACE);
            if (ac == 'a') { SUMA_SLP_Note ("FrontFace Culling enabled."); }
            break;
      }
   }

   SUMA_RETURNe;
}
/*!

Only w is used consistently, the other input varaibles may be null at times
always send GLXAREA widget in w otherwise you won't know what pointer to use with 
SUMA_remove_workproc2's data
\sa SUMA_remove_workproc2
*/
void
SUMA_postRedisplay(Widget w,
  XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_postRedisplay"};
   static XtPointer elvis;
   int isv;
   SUMA_SurfaceViewer *sv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   } else {
      if (LocalHead) 
         if (sv->X->REDISPLAYPENDING) {
            fprintf (SUMA_STDERR, 
                     "%s: Redisplay Pending. "
                     "No new request registered for viewer %d.\n",
                     FuncName, isv);
         } else {
            fprintf (SUMA_STDERR, 
                     "%s: Redisplay Pending registered for viewer %d.\n",
                     FuncName, isv);
         }
   }

   if(!sv->X->REDISPLAYPENDING) {
    /*sv->X->REDISPLAYID = XtAppAddWorkProc(sv->X->APP, handleRedisplay, 0);*/
    SUMA_register_workproc( SUMA_handleRedisplay , (XtPointer)sv->X->GLXAREA );
    sv->X->REDISPLAYPENDING = 1;
   }
   
   SUMA_RETURNe;
}

/*!
   A function for displaying the results from a plane/surface intersection after it has been rendered into a list of connected
   strips.
  A sequence would be:
    Eq = SUMA_Plane_Equation (...);
    SPI = SUMA_Surf_Plane_Intersect (SO, Eq);
    striplist = SUMA_SPI_to_EdgeStrips(SO, SPI);
    SUMA_display_edge_striplist(striplist, &(SUMAg_SVv[0]), SO, "ShowEdges, ShowConnectedPoints, ShowPoints");
    
   "ShowEdges" Means to show the intersected edges
   "ShowConnectedPoints" Means to show the intersection points with a segment joining them. These segments are
                         not edges. They are meant to show how the intersected points form a strip(s)
   "ShowPoints" Just show the intersection points (which lie on intersected edges).
*/
SUMA_Boolean SUMA_display_edge_striplist(DList *striplist, SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO, char *DispOptions)
{
   static char FuncName[]={"SUMA_display_edge_striplist"};
   /* no need to worry about freeing the DOs. They go in SUMAg_DOv */
   SUMA_SegmentDO *SDO = NULL, *SEDO=NULL;
   SUMA_SphereDO *SPDO = NULL;
   DListElmt *elm = NULL, *elmlist=NULL, *elmn=NULL;
   SUMA_STRIP *strip=NULL;
   int N_allEpath=0, N_allPpath=0, kstrip, ke=0, j, N_Epath, N_Ppath, isclosed;
   float col_first[4], col_last[4], col_middle[4];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (dlist_size(striplist)) {
      do {
         if (!elm) elm = dlist_head(striplist);
         else elm  = dlist_next(elm);
         strip = (SUMA_STRIP *)elm->data;
         N_allEpath += dlist_size(strip->Edges);
         N_allPpath += dlist_size(strip->Points); 
      } while (elm != dlist_tail(striplist));
   }
   
   
   if (SUMA_iswordin_ci(DispOptions, "ShowEdges") == 1) {
      if (N_allEpath) {
         SUMA_LHv("Building SDO of %d edges from %d strips\n", N_allEpath, dlist_size(striplist));
         if ((SDO = SUMA_Alloc_SegmentDO (N_allEpath, "SUMA_SPI_to_EdgeStrips_segs", 0, NULL, 0, LS_type))) {
            SDO->do_type = LS_type;
            SDO->colv = (GLfloat *)SUMA_malloc(4*sizeof(GLfloat)*SDO->N_n);
            SDO->LineWidth = 4;
            elmlist=NULL; kstrip = 0;
            ke = 0;
            do {
               if (!elmlist) elmlist = dlist_head(striplist);
               else elmlist  = dlist_next(elmlist);
               strip = (SUMA_STRIP *)elmlist->data;
               N_Epath = dlist_size(strip->Edges);
               isclosed = SUMA_isEdgeStripClosed(strip->Edges, SO);
               if (isclosed) { 
                  col_first[0] = col_first[3] = 1.0; col_first[1] = col_first[2] = 0.0;
                  col_middle[1] = col_middle[3] = 1.0; col_middle[0] = col_middle[2] = 0.0;
                  col_last[0] = col_last[1] = 0.0;  col_last[2] = col_last[3] = 1.0;
               } else {
                  col_first[0] = col_first[3] = 1.0; col_first[1] = col_first[2] = 0.0;
                  col_middle[0] = col_middle[1] = col_middle[3] = 1.0; col_middle[2] = 0.0;
                  col_last[0] = col_last[1] = 0.0;  col_last[2] = col_last[3] = 1.0;
               }
               SUMA_LHv("   SDO's strip #%d of %d edges (isclosed = %d)\n", kstrip, N_Epath, isclosed);
               elm=NULL;
               do{
                  if (!elm) elm = dlist_head(strip->Edges);
                  else elm = dlist_next(elm);
                  if (elm==dlist_head(strip->Edges)) { SUMA_COPY_VEC(col_first, &(SDO->colv[4*ke]), 4, float, GLfloat);  }
                  else if (elm==dlist_tail(strip->Edges)) { SUMA_COPY_VEC(col_last, &(SDO->colv[4*ke]), 4, float, GLfloat); }
                  else { SUMA_COPY_VEC(col_middle, &(SDO->colv[4*ke]), 4, float, GLfloat); }
                  for (j=0; j<3;++j) {
                     SDO->n0[3*ke+j] = SO->NodeList[3*SO->EL->EL[(int)elm->data][0]+j];
                     SDO->n1[3*ke+j] = SO->NodeList[3*SO->EL->EL[(int)elm->data][1]+j]; 
                  }
                  ++ke;
               } while (elm != dlist_tail(strip->Edges));
               ++kstrip;
            }  while (elmlist != dlist_tail(striplist)); 
            /* addDO */
            if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)SDO, LS_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
               SUMA_RETURNe;
            }
            /* register DO with viewer */
            if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
               SUMA_RETURNe;
            }
         }
      }
   }
       
   if (SUMA_iswordin_ci(DispOptions, "ShowConnectedPoints") == 1) {
      if (N_allEpath) {
         SUMA_LHv("Building SPDO of %d points from %d strips\n", 
                  N_allEpath, dlist_size(striplist));
         if ((SEDO = SUMA_Alloc_SegmentDO (N_allEpath, 
                        "SUMA_SPI_to_EdgeStrips_pointsegs", 1, 
                        NULL, 0, OLS_type))) {
            SEDO->do_type = LS_type;
            SEDO->colv = (GLfloat *)SUMA_malloc(4*sizeof(GLfloat)*SEDO->N_n);
            SEDO->LineWidth = 2;
            elmlist=NULL; kstrip = 0;
            ke = 0;
            do {
               if (!elmlist) elmlist = dlist_head(striplist);
               else elmlist  = dlist_next(elmlist);
               strip = (SUMA_STRIP *)elmlist->data;
               N_Ppath = dlist_size(strip->Points);
               isclosed = SUMA_isEdgeStripClosed(strip->Edges, SO);
               if (isclosed) { 
                  col_first[0] = col_first[3] = 1.0; 
                  col_first[1] = col_first[2] = 0.0;
                  col_middle[1] = col_middle[3] = 1.0; 
                  col_middle[0] = col_middle[2] = 0.0;
                  col_last[0] = col_last[1] = 0.0;  
                  col_last[2] = col_last[3] = 1.0;
               } else {
                  col_first[0] = col_first[3] = 1.0; 
                  col_first[1] = col_first[2] = 0.0;
                  col_middle[0] = col_middle[1] = 
                  col_middle[3] = 1.0; col_middle[2] = 0.0;
                  col_last[0] = col_last[1] = 0.0;  
                  col_last[2] = col_last[3] = 1.0;
               }
               SUMA_LHv("   SEDO's strip #%d of %d edges (isclosed = %d)\n", 
                           kstrip, N_Ppath, isclosed);
               elm=NULL;
               do{
                  if (!elm) elm = dlist_head(strip->Points);
                  else elm = dlist_next(elm);
                  if (elm==dlist_head(strip->Points)) { SUMA_COPY_VEC(col_first, &(SEDO->colv[4*ke]), 4, float, GLfloat);  }
                  else if (elm==dlist_tail(strip->Points)) { SUMA_COPY_VEC(col_last, &(SEDO->colv[4*ke]), 4, float, GLfloat); }
                  else { SUMA_COPY_VEC(col_middle, &(SEDO->colv[4*ke]), 4, float, GLfloat); }
                  if (elm != dlist_tail(strip->Points)) elmn = dlist_next(elm);
                  else {
                     if (isclosed) elmn = dlist_head(strip->Points);
                     else elmn = elm;
                  }  
                  for (j=0; j<3;++j) {
                     SEDO->n0[3*ke+j] = ((float*)elm->data)[j];
                     SEDO->n1[3*ke+j] = ((float*)elmn->data)[j]; 
                  }
                  ++ke;
               } while (elm != dlist_tail(strip->Points));
               ++kstrip;
            }  while (elmlist != dlist_tail(striplist)); 
            /* addDO */
            if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)SEDO, OLS_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
               SUMA_RETURNe;
            }
            /* register DO with viewer */
            if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
               SUMA_RETURNe;
            }
         }
      }
   }
   
   if (SUMA_iswordin_ci(DispOptions, "ShowPoints") == 1) {
      if (N_allPpath) {
         SUMA_LHv("Building SPDO of %d points from %d strips\n", N_allPpath, dlist_size(striplist));
         if ((SPDO = SUMA_Alloc_SphereDO (N_allPpath, "SUMA_SPI_to_EdgeStrips_points", NULL, SP_type))) {
            SPDO->do_type = SP_type;
            SPDO->CommonRad = SO->EL->AvgLe/6.0;
            SPDO->colv = (GLfloat *)SUMA_malloc(4*sizeof(GLfloat)*SPDO->N_n);

            elmlist=NULL; kstrip = 0;
            ke = 0;
            do {
               if (!elmlist) elmlist = dlist_head(striplist);
               else elmlist  = dlist_next(elmlist);
               strip = (SUMA_STRIP *)elmlist->data;
               N_Ppath = dlist_size(strip->Points);
               isclosed = SUMA_isEdgeStripClosed(strip->Edges, SO);
               if (isclosed) { 
                  col_first[0] = col_first[3] = 1.0; col_first[1] = col_first[2] = 0.0;
                  col_middle[0] = col_middle[1] = col_middle[2] = col_middle[3] = 1.0;
                  col_last[0] = col_last[1] = 0.0;  col_last[2] = col_last[3] = 1.0;
               } else {
                  col_first[0] = col_first[3] = 1.0; col_first[1] = col_first[2] = 0.0;
                  col_middle[0] = col_middle[2] = col_middle[3] = 1.0; col_middle[1] = 0.0;
                  col_last[0] = col_last[1] = 0.0;  col_last[2] = col_last[3] = 1.0;
               }
               SUMA_LHv("   SPDO's strip #%d of %d points (isclosed=%d)\n", kstrip, N_Ppath, isclosed);
               elm=NULL;
               do{
                  if (!elm) elm = dlist_head(strip->Points);
                  else elm = dlist_next(elm);
                  if (elm==dlist_head(strip->Points)) { SUMA_COPY_VEC(col_first, &(SPDO->colv[4*ke]), 4, float, GLfloat);}
                  else if (elm==dlist_tail(strip->Points)) { SUMA_COPY_VEC(col_last, &(SPDO->colv[4*ke]), 4, float, GLfloat);  }
                  else {SUMA_COPY_VEC(col_middle, &(SPDO->colv[4*ke]), 4, float, GLfloat);  }
                  SPDO->cxyz[3*ke  ] = ((float*)elm->data)[0];
                  SPDO->cxyz[3*ke+1] = ((float*)elm->data)[1];
                  SPDO->cxyz[3*ke+2] = ((float*)elm->data)[2]; 
                  ++ke;
               } while(elm != dlist_tail(strip->Points));
               ++kstrip;
            } while (elmlist != dlist_tail(striplist)); 
            /* addDO */
            if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)SPDO, SP_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
               SUMA_RETURNe;
            }

            /* register DO with viewer */
            if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
               SUMA_RETURNe;
            }

         }

      }   
   }

     

   if (SDO || SPDO || SEDO) {
      /* redisplay curent only*/
      sv->ResetGLStateVariables = YUP;
      SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
   }
   SUMA_RETURN(YUP); 
}


void SUMA_LoadSegDO (char *s, void *csvp )
{
   static char FuncName[]={"SUMA_LoadSegDO"};
   SUMA_SegmentDO *SDO = NULL;
   SUMA_SurfaceViewer *sv;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_DO_Types dotp=no_type;
   SUMA_DO_CoordType coord_type=SUMA_WORLD;
   void *VDO = NULL;
   
   SUMA_ENTRY;
   
   sv = (SUMA_SurfaceViewer *)csvp;
   
   if (!s) { SUMA_RETURNe; }
   
   /* what type are we dealing with ? */
   dotp = SUMA_Guess_DO_Type(s);
   if (dotp == no_type) {
      /* assume segments */
      dotp = LS_type;
   }
   coord_type = SUMA_WORLD;
   switch (dotp) {
      case ONBV_type:
      case NBV_type:
         if (!(SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP)) {
            SUMA_SL_Err("No surface in focus to which the vector would be attached.\n");
            SUMA_RETURNe;
         }
         if (dotp == NBV_type) {
            if (!(SDO = SUMA_ReadNBVecDO(s, 0, SO->idcode_str))) {
               SUMA_SL_Err("Failed to read segments file.\n");
               SUMA_RETURNe;
            }
         } else {
            if (!(SDO = SUMA_ReadNBVecDO(s, 1, SO->idcode_str))) {
               SUMA_SL_Err("Failed to read segments file.\n");
               SUMA_RETURNe;
            }
         }
         SDO->do_type = dotp;
         VDO = (void *)SDO;
         break;
      case NBSP_type:
         if (!(SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP)) {
            SUMA_SL_Err("No surface in focus to which "
                        "the spheres would be attached.\n");
            SUMA_RETURNe;
         }
         if (!(VDO = (void*)SUMA_ReadNBSphDO(s, SO->idcode_str))) {
            SUMA_SL_Err("Failed to read spheres file.\n");
            SUMA_RETURNe;
         }
         ((SUMA_SphereDO * )VDO)->do_type = dotp;
         break;
      case OLS_type:
         if (!(SDO = SUMA_ReadSegDO(s, 1, NULL))) {
            SUMA_SL_Err("Failed to read segments file.\n");
            SUMA_RETURNe;
         }
         SDO->do_type = dotp;
         VDO = (void *)SDO;
         break;
      case LS_type:
         if (!(SDO = SUMA_ReadSegDO(s, 0, NULL))) {
            SUMA_SL_Err("Failed to read segments file.\n");
            SUMA_RETURNe;
         }
         SDO->do_type = dotp;
         VDO = (void *)SDO;
         break;
      case NBOLS_type:
         if (!(SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP)) {
            SUMA_SL_Err("No surface in focus to which "
                        "the spheres would be attached.\n");
            SUMA_RETURNe;
         }
         if (!(SDO = SUMA_ReadNBSegDO(s, 1, SO->idcode_str))) {
            SUMA_SL_Err("Failed to read segments file.\n");
            SUMA_RETURNe;
         }
         SDO->do_type = dotp;
         VDO = (void *)SDO;
         break;
      case NBLS_type:
         if (!(SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP)) {
            SUMA_SL_Err("No surface in focus to which "
                        "the spheres would be attached.\n");
            SUMA_RETURNe;
         }
         if (!(SDO = SUMA_ReadNBSegDO(s, 0, SO->idcode_str))) {
            SUMA_SL_Err("Failed to read segments file.\n");
            SUMA_RETURNe;
         }
         SDO->do_type = dotp;
         VDO = (void *)SDO;
         break;
      
      case SP_type:
         if (!(VDO = (void *)SUMA_ReadSphDO(s))) {
            SUMA_SL_Err("Failed to read spheres file.\n");
            SUMA_RETURNe;
         }
         ((SUMA_SphereDO * )VDO)->do_type = dotp;
         break;
      case PL_type:
         if (!(VDO = (void *)SUMA_ReadPlaneDO(s))) {
            SUMA_SL_Err("Failed to read spheres file.\n");
            SUMA_RETURNe;
         }
         ((SUMA_SphereDO * )VDO)->do_type = dotp;
         break;
      case NIDO_type:
         if (sv->Focus_SO_ID >=0) 
            SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
         else
            SO = NULL;
         if (!(VDO = (void *)SUMA_ReadNIDO(s, SO->idcode_str))) {
            SUMA_SL_Err("Failed to read spheres file.\n");
            SUMA_RETURNe;
         }
         ((SUMA_NIDO * )VDO)->do_type = dotp;
         coord_type = SUMA_CoordType(
                  NI_get_attribute(((SUMA_NIDO * )VDO)->ngr,
                                    "coord_type"));
         if (!coord_type) {
            SUMA_SL_Err("Bad coord_type");
            SUMA_RETURNe;
         }   
         break;
      default:
         SUMA_SL_Err("Should not get here");
         SUMA_RETURNe;
         break;
   }
   
   #if 0 
   { /* a test for changing SDO formats */
      if (SDO) {
         NI_group *ngr = SUMA_SDO2niSDO(SDO);
         int suc;
         NEL_WRITE_TX(ngr, "file:mess.niml.SDO", suc);
         SUMA_S_Note("Wrote mess to disk");
         /* Now free SDO and reload from disk */
         SUMA_free_SegmentDO(SDO);
         SDO = SUMA_niSDO2SDO(ngr);
         NI_free(ngr); ngr = NULL;
         /* now repeat xformation and see what you get */
         ngr = SUMA_SDO2niSDO(SDO);
         NEL_WRITE_TX(ngr, "file:mess2.niml.SDO", suc);
         SUMA_S_Note("Wrote mess2 to disk");
         NI_free(ngr); ngr = NULL;
         VDO = (void *)SDO;
      }
   }
   #endif
   
   /* addDO */
   if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, VDO, dotp, coord_type)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
      SUMA_RETURNe;
   }

   /* register DO with viewer */
   if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
      SUMA_RETURNe;
   }

   /* redisplay curent only*/
   sv->ResetGLStateVariables = YUP;
   SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
               
   SUMA_RETURNe;
}

/*!
   \brief, retrieves an vector attribute
   and decodes it into m_fv (or m_dv for double). It is your 
   job to make sure m_fv (or m_dv) is big enough 
   for the endeavour 
   m_fail is 1 if no such attribute is found
            2 if the number of values read is not = m_n
*/
static int shutup;

#define SUMA_S2FV_ATTR(m_nel, m_attr, m_fv, m_n, m_fail) {\
   char *m_atmp = NULL; \
   int m_nr = 0; \
   m_fail = 0; \
   m_atmp = NI_get_attribute(m_nel, m_attr); \
   if (!m_atmp) {   \
      if (LocalHead) \
         fprintf( SUMA_STDERR,\
                  "Error %s:\nNo such attribute (%s).", FuncName, m_attr);  \
      m_fail = 1; \
   }  \
   m_nr = SUMA_StringToNum(m_atmp, (void*)m_fv, m_n,1);  \
   if (m_nr != m_n) {  \
      if (LocalHead) \
         fprintf( SUMA_STDERR,\
                  "Error %s:\nBad attribute (%s) length.\n"\
                  "Expected %d, found %d\n",   \
                           FuncName, m_attr, m_n, m_nr);  \
      m_fail = 2; \
   }  \
}

#define SUMA_S2DV_ATTR(m_nel, m_attr, m_dv, m_n, m_fail) {\
   char *m_atmp = NULL; \
   int m_nr = 0; \
   m_fail = 0; \
   m_atmp = NI_get_attribute(m_nel, m_attr); \
   if (!m_atmp) {   \
      if (LocalHead) \
         fprintf( SUMA_STDERR,\
                  "Error %s:\nNo such attribute (%s).", FuncName, m_attr);  \
      m_fail = 1; \
   }  \
   m_nr = SUMA_StringToNum(m_atmp, (void*)m_dv, m_n,2);  \
   if (m_nr != m_n) {  \
      if (LocalHead) \
         fprintf( SUMA_STDERR,\
                  "Error %s:\nBad attribute (%s) length.\n"\
                  "Expected %d, found %d\n",   \
                           FuncName, m_attr, m_n, m_nr);  \
      m_fail = 2; \
   }  \
}

#define SUMA_FV2S_ATTR_TMP_STR   3000
/*!
   \brief turns a vector into a string of floats
   The macro will fail if your vector ends up
   being more than SUMA_FV2S_ATTR_TMP_STR characters long.
   This macro is slow and stupid, don't use it for
   long vectors.
*/
#define SUMA_FV2S_ATTR(m_nel, m_attr, m_fv, m_n, m_fail) {\
   int m_i; \
   char m_stmp[SUMA_FV2S_ATTR_TMP_STR], m_val[50];   \
   m_fail = 0; \
   m_i = 0; \
   m_stmp[0] = '\0'; \
   while (m_i < m_n && !(m_fail)) { \
      sprintf(m_val,"%f ", m_fv[m_i]); \
      if (strlen(m_stmp) + strlen(m_val) >= SUMA_FV2S_ATTR_TMP_STR -1) {   \
         fprintf(SUMA_STDERR,"Error %s\nVector exceeded buffer length.\n", FuncName);  \
         m_fail = 1; \
      } else { \
         strcat (m_stmp, m_val); \
         ++ m_i;  \
      }\
   }  \
   NI_set_attribute (m_nel, m_attr, m_stmp);      \
}
      
#define SUMA_DV2S_ATTR(m_nel, m_attr, m_dv, m_n, m_fail) {\
   int m_i; \
   char m_stmp[SUMA_FV2S_ATTR_TMP_STR], m_val[50];   \
   m_fail = 0; \
   m_i = 0; \
   m_stmp[0] = '\0'; \
   while (m_i < m_n && !(m_fail)) { \
      sprintf(m_val,"%f ", m_dv[m_i]); \
      if (strlen(m_stmp) + strlen(m_val) >= SUMA_FV2S_ATTR_TMP_STR -1) {   \
         fprintf(SUMA_STDERR,"Error %s\nVector exceeded buffer length.\n", FuncName);  \
         m_fail = 1; \
      } else { \
         strcat (m_stmp, m_val); \
         ++ m_i;  \
      }\
   }  \
   NI_set_attribute (m_nel, m_attr, m_stmp);      \
}

/*!
   \brief   A function to save the settings of a surface viewer
            for later recall 
*/
void SUMA_SaveVisualState(char *fname, void *csvp )
{
   static char FuncName[]={"SUMA_SaveVisualState"};
   NI_element *nel = NULL;
   NI_stream nstdout;
   char stmp[500];
   char *fnamestmp=NULL, *fnamestmp2=NULL;
   int feyl;
   SUMA_SurfaceViewer *csv;
   Position X=0, Y=0;
   Dimension ScrW, ScrH;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   csv = (SUMA_SurfaceViewer *)csvp;
   
   if (!csv) { SUMA_RETURNe; }
   
   if (csv->StdView < 0 || csv->iState < 0 ) { SUMA_RETURNe; }
   
   SUMA_allow_nel_use(1);
   nel = SUMA_NewNel (  SUMA_VIEWER_SETTING, /* one of SUMA_DSET_TYPE */
                        NULL, /* idcode of Domain Parent */
                        NULL, /* idcode of geometry parent, not useful here*/
                        0,
                        NULL,
                        NULL); /* Number of elements */
   if (!nel) {
      SUMA_SL_Err("Failed to create nel.");
      SUMA_RETURNe;
   }
   
   /* Save the relevant parameters */
   SUMA_FV2S_ATTR(nel, "currentQuat", csv->GVS[csv->StdView].currentQuat, 4, feyl); if (feyl) { SUMA_RETURNe; }
   SUMA_FV2S_ATTR(nel, "translateVec", csv->GVS[csv->StdView].translateVec, 2, feyl); if (feyl) { SUMA_RETURNe; }
   SUMA_FV2S_ATTR(nel, "clear_color", csv->clear_color, 4, feyl); if (feyl) { SUMA_RETURNe; }
   sprintf(stmp, "%f", csv->FOV[csv->iState]);
   NI_set_attribute (nel, "FOV", stmp);
   sprintf(stmp, "%f", csv->Aspect);
   NI_set_attribute (nel, "Aspect", stmp);
   sprintf(stmp, "%d", csv->WindWidth);
   NI_set_attribute (nel, "WindWidth", stmp);
   sprintf(stmp, "%d", csv->WindHeight);
   NI_set_attribute (nel, "WindHeight", stmp);
   if (csv->X && csv->X->TOPLEVEL) {            
      ScrW = WidthOfScreen (XtScreen(csv->X->TOPLEVEL));
      ScrH = HeightOfScreen (XtScreen(csv->X->TOPLEVEL));
      XtVaGetValues (csv->X->TOPLEVEL, /* Get the positions of New */
         XmNx, &X,
         XmNy, &Y,
         NULL);
      if ((int)X >= 0 && (int)X < ScrW ) { 
         NI_SET_INT (nel, "WindX", (int)X);
      } else {
         SUMA_S_Warnv("X position is %d, outside of [0,%d[\n",
                      X, ScrW);
      }
      if ((int)Y >= 0 && (int)Y < ScrH ) { 
         NI_SET_INT (nel, "WindY", (int)Y);
      } else {
         SUMA_S_Warnv("Y position is %d, outside of [0,%d[\n",
                      Y, ScrH);
      }
   }
   sprintf(stmp, "%d", (int)csv->BF_Cull);
   NI_set_attribute (nel, "BF_Cull", stmp);
   sprintf(stmp, "%f", csv->Back_Modfact);
   NI_set_attribute (nel, "Back_Modfact", stmp);
   sprintf(stmp, "%d", (int)csv->PolyMode);
   NI_set_attribute (nel, "PolyMode", stmp);
   sprintf(stmp, "%d", csv->ShowEyeAxis);
   NI_set_attribute (nel, "ShowEyeAxis", stmp);
   sprintf(stmp, "%d", csv->ShowMeshAxis);
   NI_set_attribute (nel, "ShowMeshAxis", stmp);
   sprintf(stmp, "%d", csv->ShowWorldAxis);
   NI_set_attribute (nel, "ShowWorldAxis", stmp);
   sprintf(stmp, "%d", csv->ShowCrossHair);
   NI_set_attribute (nel, "ShowCrossHair", stmp);
   sprintf(stmp, "%d", (int)csv->ShowForeground);
   NI_set_attribute (nel, "ShowForeground", stmp);
   sprintf(stmp, "%d", (int)csv->ShowBackground);
   NI_set_attribute (nel, "ShowBackground", stmp);
   
   /* write element */
   if (!fname) {
      nstdout = NI_stream_open( "file:test.niml.vvs","w");
   } else {
      fnamestmp = SUMA_append_string("file:", fname);
      fnamestmp2 = SUMA_Extension(fnamestmp, ".niml.vvs", NOPE);
      nstdout = NI_stream_open( fnamestmp2,"w");
      SUMA_free(fnamestmp); fnamestmp = NULL;
      SUMA_free(fnamestmp2); fnamestmp2 = NULL;
   }
   if( nstdout == NULL ){ 
      fprintf(stderr,"%s: Can't open file\n", FuncName); 
      SUMA_RETURNe; 
   }
   NI_write_element( nstdout , nel , NI_TEXT_MODE | NI_HEADERSHARP_FLAG) ;               
   NI_stream_close(nstdout);
   NI_free_element(nel) ; nel = NULL;
    
   SUMA_RETURNe;
}

int SUMA_ApplyVisualState(NI_element *nel, SUMA_SurfaceViewer *csv)
{
   static char FuncName[]={"SUMA_ApplyVisualState"};
   int feyl;
   char *fnamestmp=NULL, *fnamestmp2=NULL;
   float quat[4], Aspect[1], FOV[1], tran[2],
         WindWidth[1], WindHeight[1], clear_color[4], 
         BF_Cull[1], Back_Modfact[1], PolyMode[1], ShowEyeAxis[1], 
         ShowWorldAxis[1],
         ShowMeshAxis[1], ShowCrossHair[1], ShowForeground[1], 
         ShowBackground[1], WindX[1], WindY[1];
   Dimension ScrW, ScrH;   
   char *atmp;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nel || !csv) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(0);
   }
   
   /* don't crash if you fail here and there, try your best ...*/
   SUMA_S2FV_ATTR(nel, "currentQuat", quat, 4, feyl); 
      if (!feyl) {
         SUMA_COPY_VEC( quat, csv->GVS[csv->StdView].currentQuat, 4, 
                        float, float);
      }
   SUMA_S2FV_ATTR(nel, "translateVec", tran, 2, feyl); 
      if (!feyl) {
         SUMA_COPY_VEC( tran, csv->GVS[csv->StdView].translateVec, 2, 
                        float, float);
      }
   SUMA_S2FV_ATTR(nel, "FOV", FOV, 1, feyl); 
      if (!feyl) {   
         csv->FOV[csv->iState] = FOV[0];
      }
   SUMA_S2FV_ATTR(nel, "Aspect", Aspect, 1, feyl); 
      if (!feyl) {   
         csv->Aspect = Aspect[0]; /* gets set when SUMA_resize is called */
      }
   SUMA_S2FV_ATTR(nel, "WindWidth", WindWidth, 1, feyl); 
      if (!feyl) {
         csv->WindWidth = (int)WindWidth[0];
            /* gets set when SUMA_resize is called */
      }
   SUMA_S2FV_ATTR(nel, "WindHeight", WindHeight, 1, feyl); 
      if (!feyl) {
         csv->WindHeight = (int)WindHeight[0]; 
            /* That gets recalculated when SUMA_resize is called */
      }
   if (csv->X && csv->X->TOPLEVEL) {
      Position Xi, Yi;            
      ScrW = WidthOfScreen (XtScreen(csv->X->TOPLEVEL));
      ScrH = HeightOfScreen (XtScreen(csv->X->TOPLEVEL));
      SUMA_S2FV_ATTR(nel, "WindX", WindX, 1, feyl); 
      if (!feyl) {
         if ((int)WindX[0] < 0 && (int)WindX[0] >= ScrW ) { 
            SUMA_S_Warnv("X position is %d, outside of [0,%d[\n",
                   (int)WindX[0], ScrW);
            WindX[0] = -1.0;
         }
      } else WindX[0] = -1.0;
      SUMA_S2FV_ATTR(nel, "WindY", WindY, 1, feyl); 
      if (!feyl) {
         if ((int)WindY[0] < 0 && (int)WindY[0] >= ScrW ) { 
            SUMA_S_Warnv("Y position is %d, outside of [0,%d[\n",
                   (int)WindY[0], ScrW);
            WindY[0] = -1.0;
         }
      } else WindY[0] = -1.0;
      
      if (WindY[0] >= 0 && WindY[0] >= 0) {
         XtVaSetValues (csv->X->TOPLEVEL, /* Get the positions of New */
            XmNx, (Position)((int)WindX[0]),
            XmNy, (Position)((int)WindY[0]),
            NULL);
       }
   }

   SUMA_S2FV_ATTR(nel, "clear_color", clear_color, 4, feyl); 
      if (!feyl) {
         SUMA_COPY_VEC(clear_color, csv->clear_color, 4, float, float);
      }
   SUMA_S2FV_ATTR(nel, "BF_Cull", BF_Cull, 1, feyl); 
      if (!feyl) {
         csv->BF_Cull = (SUMA_Boolean)BF_Cull[0];
      }
   SUMA_S2FV_ATTR(nel, "Back_Modfact", Back_Modfact, 1, feyl); 
      if (!feyl) {
         csv->Back_Modfact = Back_Modfact[0];
      }
   SUMA_S2FV_ATTR(nel, "PolyMode", PolyMode, 1, feyl); 
      if (!feyl) {
         csv->PolyMode = (SUMA_RENDER_MODES)PolyMode[0];
      }
   SUMA_S2FV_ATTR(nel, "ShowEyeAxis", ShowEyeAxis, 1, feyl); 
      if (!feyl) {
         csv->ShowEyeAxis = (int)ShowEyeAxis[0];
      }
   SUMA_S2FV_ATTR(nel, "ShowMeshAxis", ShowMeshAxis, 1, feyl); 
      if (!feyl) {
         csv->ShowMeshAxis = (int)ShowMeshAxis[0];
      }
   SUMA_S2FV_ATTR(nel, "ShowWorldAxis", ShowWorldAxis, 1, feyl); 
      if (!feyl) {
         csv->ShowWorldAxis = (int)ShowWorldAxis[0];
      }
   SUMA_S2FV_ATTR(nel, "ShowCrossHair", ShowCrossHair, 1, feyl); 
      if (!feyl) {
         csv->ShowCrossHair = (int)ShowCrossHair[0];
      }
   SUMA_S2FV_ATTR(nel, "ShowForeground", ShowForeground, 1, feyl); 
      if (!feyl) {
         csv->ShowForeground = (SUMA_Boolean)ShowForeground[0];
      }
   SUMA_S2FV_ATTR(nel, "ShowBackground", ShowBackground, 1, feyl); 
      if (!feyl) {
         csv->ShowForeground = (SUMA_Boolean)ShowForeground[0];
      }
   
   /* do a resize (does not matter if dimensions did not change, call is simple.
   This call will also generate a SUMA_resize call */
   SUMA_WidgetResize (csv->X->TOPLEVEL , csv->WindWidth, csv->WindHeight); 

   
   SUMA_RETURN(1);   
   
}
/*!
   \brief function to load a viewer's visual state
*/
void SUMA_LoadVisualState(char *fname, void *csvp)
{
   static char FuncName[]={"SUMA_LoadVisualState"};
   NI_element *nel = NULL;
   int feyl;
   char *fnamestmp=NULL, *fnamestmp2=NULL;
   float quat[4], Aspect[1], FOV[1], tran[2],
         WindWidth[1], WindHeight[1], clear_color[4], 
         BF_Cull[1], Back_Modfact[1], PolyMode[1], 
         ShowEyeAxis[1], ShowWorldAxis[1],
         ShowMeshAxis[1], ShowCrossHair[1], ShowForeground[1], 
         ShowBackground[1];   char *atmp;
   NI_stream nstdin;
   SUMA_SurfaceViewer *csv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   csv = (SUMA_SurfaceViewer *)csvp;
   if (!csv) { SUMA_RETURNe; }
   
   if (!fname) {
      nstdin = NI_stream_open( "file:test.niml.vvs","r");
   } else {
      fnamestmp = SUMA_append_string("file:", fname);
      fnamestmp2 = SUMA_Extension(fnamestmp, ".niml.vvs", NOPE);
      nstdin = NI_stream_open( fnamestmp2,"r");
      SUMA_free(fnamestmp); fnamestmp = NULL;
      SUMA_free(fnamestmp2); fnamestmp2 = NULL;
   }
   if( nstdin == NULL ){ 
      fprintf(stderr,"%s: Can't open file\n", FuncName); 
      SUMA_RETURNe; 
   }
   nel = NI_read_element (nstdin, 1);
   if (!nel) {
      SUMA_SL_Err("Failed to read nel.\n");
      SUMA_RETURNe;
   }
   
   if (!SUMA_ApplyVisualState(nel, csv)) {
      SUMA_S_Err("Failed to apply state");
      SUMA_RETURNe;
   }
   
   NI_free_element(nel); nel = NULL;
   NI_stream_close(nstdin); 
   

   SUMA_RETURNe;
}

GLenum SUMA_index_to_clip_plane(int iplane) 
{
   static char FuncName[]={"SUMA_index_to_clip_plane"};
   
   switch(iplane) {
      case 0:
         return(GL_CLIP_PLANE0);
         break;
      case 1:
         return(GL_CLIP_PLANE1);
         break;
      case 2:
         return(GL_CLIP_PLANE2);
         break;
      case 3:
         return(GL_CLIP_PLANE3);
         break;
      case 4:
         return(GL_CLIP_PLANE4);
         break;
      case 5:
         return(GL_CLIP_PLANE5);
         break;
      default:
         SUMA_SLP_Err("You are not to have more than 6 planes!!!");
         return(GL_CLIP_PLANE0);
         break;
   }
}
void SUMA_display(SUMA_SurfaceViewer *csv, SUMA_DO *dov)
{   
   int i;
   static int xList[1], yList[1];
   SUMA_SurfaceObject *SO=NULL;
   GLfloat rotationMatrix[4][4];
   static char FuncName[]={"SUMA_display"};
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
    
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_DUMP_TRACE("Trace At display call");
   }

   
   /* now you need to set the clear_color since 
      it can be changed per viewer Thu Dec 12 2002 */
   glClearColor ( csv->clear_color[0], csv->clear_color[1], 
                  csv->clear_color[2], csv->clear_color[3]);
   
   if (csv->NewGeom) { 
      /* This function makes calls that are repeated in SUMA_OpenGLStateReset */
      SUMA_NewGeometryInViewer(SUMAg_DOv, SUMAg_N_DOv, csv);
      csv->NewGeom = NOPE;
      csv->ResetGLStateVariables = NOPE; /*  SUMA_NewGeometryInViewer contains 
                                             SUMA_OpenGLStateReset
                                             stuff and lots more ...*/
   } else {
      /* You cannot just rely on csv->ResetGLStateVariables because 
         it is hard to set for all conditions. 
         For example, if you have multiple viewers open and you have surfaces 
         moving on momentum in all viewers, then you will have to call 
         SUMA_OpenGLStateReset before each display otherwise the openGL 
         settings for one of them will affect the others.
         At any rate, that function is not costly to run so there's no harm in 
         running it anytime you have a display call and more than one viewer 
         open */ 

      if (SUMAg_N_SVv > 1 || csv->ResetGLStateVariables) {
         if (LocalHead) 
            fprintf( SUMA_STDERR, 
                     "%s: Calling SUMA_OpenGLStateReset.\n", FuncName);
         SUMA_OpenGLStateReset (SUMAg_DOv, SUMAg_N_DOv, csv);
         csv->ResetGLStateVariables = NOPE;
      }
   }
   
   /* calculate Pcenter_close for the axis positioning */
   if (csv->ShowMeshAxis || csv->ShowWorldAxis) {
      yList[0] = csv->WindHeight;
      xList[0] = 0;
      SUMA_GetSelectionLine ( csv, csv->WindWidth/2, csv->WindHeight/2, 
                              csv->Pcenter_close, csv->Pcenter_far, 1, 
                              xList, yList, csv->Plist_close);
   }
   /* decide on color mixing needs */
   if (!SUMA_MixColors (csv)) {
      fprintf( SUMA_STDERR,
               "Error %s: Failed in SUMA_MixColors. Aborting.\n", FuncName);
      exit(1);
   }
   
   if (LocalHead) 
      fprintf (SUMA_STDOUT,"%s: Building Rotation matrix ...\n", FuncName);
   SUMA_build_rotmatrix(rotationMatrix, csv->GVS[csv->StdView].currentQuat);
    
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: performing glClear ...\n", FuncName);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); /* clear the Color Buffer 
                                                         and the depth buffer */
   
   SUMA_SET_GL_PROJECTION(csv, csv->ortho);
   
   if (SUMAg_CF->N_ClipPlanes) { /* clipping parts in fixed (screen)  
                                    coordinate space */
      for (i=0; i<SUMAg_CF->N_ClipPlanes; ++i) {
         if (SUMAg_CF->ClipPlaneType[i] == SUMA_SCREEN_CLIP) {
            glClipPlane(SUMA_index_to_clip_plane(i), 
                        &(SUMAg_CF->ClipPlanes[4*i]));
            glEnable(SUMA_index_to_clip_plane(i));
         }
      }
   }

   /* cycle through csv->RegisteredDO and display 
      those things that have a fixed CoordType*/
   if (LocalHead) 
      fprintf (SUMA_STDOUT,
               "%s: Creating objects with fixed coordinates ...\n", 
               FuncName);
   #if 0
      {
         float Ps[3]={0.0, 0.0, 0.0};
         /* To ponder:
         How to define coordinates of location
            You could have centered text on center of screen axis,
         but then what do you do you're off center and you zoom in and out...
            Do you want coordinate relative to window size or do you want them
         in mm units?
         You probably want two loops on the display. 
            One for DOs in the foreground and one for DOs in the background.
            It gets complicated but you typically want text on the front and
            axis on the back even though both may be SUMA_SCREEN CoordType
         
         Last but not least, you need a respectable DO for text on screen
         
         See SUMA_DrawWindowLine and SUMA_GetSelectionLine for relating 
         window coords to 3d coords.
         
         */ 
      }
   #endif
   i = 0;
   while (i < csv->N_DO) {
      if (dov[csv->RegisteredDO[i]].CoordType == SUMA_SCREEN) {
         switch (dov[csv->RegisteredDO[i]].ObjectType) {
            case type_not_set:
            case no_type:
               SUMA_SL_Err("Should not be doing this buidness");
               break;
            case NBSP_type:
            case SP_type:
               SUMA_SL_Warn("Not ready yet!");
               break;
            case SO_type:
               break;
            case AO_type:
               if (csv->ShowEyeAxis){
                  if (!SUMA_DrawAxis (
                        (SUMA_Axis*)dov[csv->RegisteredDO[i]].OP, csv)) {
                     fprintf( SUMA_STDERR,
                              "Error %s: Could not display EYE AXIS\n", 
                              FuncName);
                  }
               }
               break;
            case GO_type:
               break;
            case ROIdO_type:
               /* those are drawn by SUMA_DrawMesh */
               break;
            case ROIO_type:
               /* those are drawn by SUMA_DrawMesh */
               break;
            case ONBV_type:
            case NBV_type:
            case OLS_type:
            case LS_type:
            case NBOLS_type:
            case NBLS_type:
               if (!SUMA_DrawSegmentDO (
                     (SUMA_SegmentDO *)dov[csv->RegisteredDO[i]].OP, 
                     csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawSegmentDO.\n", 
                           FuncName);
               }
               break;
            case PL_type:
               SUMA_SL_Warn("Not ready yet!");
               break;
            case NIDO_type:
               SUMA_LH("Doing Screen NIDO");
               if (!SUMA_DrawNIDO ((SUMA_NIDO*)dov[csv->RegisteredDO[i]].OP, 
                     csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawNIDO.\n", 
                           FuncName);
               }
               break;
         }
      }
      ++i;
   }
   
   SUMA_SET_GL_MODELVIEW(csv);

   /* cycle through csv->RegisteredDO and display 
      those things that have a Local CoordType*/
   if (LocalHead) 
      fprintf (SUMA_STDOUT,
               "%s: Creating objects with local coordinates ...\n", FuncName);

   /* cuting plane for all? */
   if (SUMAg_CF->N_ClipPlanes) { /* clipping parts in object coordinate space */
      for (i=0; i<SUMAg_CF->N_ClipPlanes; ++i) {
         if (SUMAg_CF->ClipPlaneType[i] == SUMA_ALL_OBJECT_CLIP) {
            glClipPlane(SUMA_index_to_clip_plane(i), 
                        &(SUMAg_CF->ClipPlanes[4*i]));
            glEnable(SUMA_index_to_clip_plane(i));
         }
      }
   }
   
   /* Show the World Axis if required,
   Do this before DOs because with Depth test enabled (as it should
   for axis drawing, you can have a large texture volume, with many outer voxels
   of zeros obscuring the axis because of the depth test. 
   One could deactivate the 
   depth test and play with color blending but that is a pain. */
   if (csv->ShowWorldAxis) {
      /* fprintf(SUMA_STDOUT,"Showing World Axis \n");  */
      if (!SUMA_DrawAxis (csv->WAx, csv)) {
         fprintf(stderr,"display error: Failed to Create WAx\n");
      }
   }


   i = 0;
   while (i < csv->N_DO) {
      if (dov[csv->RegisteredDO[i]].CoordType == SUMA_WORLD) {
         switch (dov[csv->RegisteredDO[i]].ObjectType) {
            case SO_type:
               SO = (SUMA_SurfaceObject *)dov[csv->RegisteredDO[i]].OP;
               if (SO->Show) {
                  if (  (SO->Side == SUMA_LEFT && csv->ShowLeft) || 
                        (SO->Side == SUMA_RIGHT && csv->ShowRight) ||
                        SO->Side == SUMA_NO_SIDE || SO->Side == SUMA_LR) {
                        SUMA_DrawMesh(SO, csv); /* create the surface */
                  }
               }
               break;
            case AO_type:
               if (csv->ShowMeshAxis) {
                  if (!SUMA_DrawAxis ((SUMA_Axis*)dov[csv->RegisteredDO[i]].OP, 
                                       csv)) {
                     fprintf( stderr,
                              "display error: Could not display Mesh AXIS\n");
                  }
               }
               break;
            case GO_type:
               break;
            case ROIdO_type:
               /* those are drawn by SUMA_DrawMesh */
               break;
            case ROIO_type:
               /* those are drawn by SUMA_DrawMesh */
               break;
            case ONBV_type:
            case NBV_type:
            case NBLS_type:
            case NBOLS_type:
               /* those are drawn by SUMA_DrawMesh */
               break;
            case OLS_type:
            case LS_type:
               if (!SUMA_DrawSegmentDO (
                     (SUMA_SegmentDO *)dov[csv->RegisteredDO[i]].OP, csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawSegmentDO.\n", 
                           FuncName);
               }
               break;
            case NBSP_type:
               /* those are drawn by SUMA_DrawMesh */
               break;
            case SP_type:
               if (!SUMA_DrawSphereDO (
                     (SUMA_SphereDO *)dov[csv->RegisteredDO[i]].OP, csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawSphereDO.\n", FuncName);
               }
               break;
            case PL_type:
               if (!SUMA_DrawPlaneDO (
                     (SUMA_PlaneDO *)dov[csv->RegisteredDO[i]].OP, csv)) {
                  fprintf(SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawPlaneDO.\n", FuncName);
               }
               break;
            case NIDO_type:
               if (SUMA_isNIDO_SurfBased(
                     (SUMA_NIDO *)dov[csv->RegisteredDO[i]].OP)) { 
                  /* this is done in SUMA_DrawMesh */
               } else {
                  if (!SUMA_DrawNIDO (
                        (SUMA_NIDO *)dov[csv->RegisteredDO[i]].OP, csv)) {
                     fprintf(SUMA_STDERR, 
                              "Error %s: Failed in SUMA_DrawNIDO.\n", FuncName);
                  }
               }
               break;
            case type_not_set:
            case no_type:
               SUMA_SL_Err("What's cracking?");
               break;
         }
      }
      ++i;
   }
   
   /* Show the Cross Hair, if required leave this at the end because
   it does not care about depth tests which could be a problem when doing
   texture volumes*/
   if (csv->ShowCrossHair) {
      /*fprintf(SUMA_STDOUT,"Showing Cross Hair \n");*/
      if (!SUMA_DrawCrossHair (csv)) {
         fprintf(stderr,"display error: Failed to Create Cross Hair\n");
      }
   }
   

   #if 0
   /* Show the pick line, you may want place this as a DO later on */
    {
      static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
      static GLfloat LineColor[] = {1.0, 0.0, 1.0, 0.0};
      glLineWidth(1.0);
      glDisable(GL_DEPTH_TEST);
      glEnable(GL_LINE_STIPPLE);
      glLineStipple (1, 0x1C47); /* dashed, see OpenGL Prog guide, page 55 */
      glBegin(GL_LINES);
      glMaterialfv(GL_FRONT, GL_EMISSION, LineColor); /*turn on emissivity for axis*/
      glVertex3f(csv->Pick0[0], csv->Pick0[1], csv->Pick0[2]);
      glVertex3f(csv->Pick1[0], csv->Pick1[1], csv->Pick1[2]);
      glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity for axis*/
      glEnd();
      glDisable(GL_LINE_STIPPLE);
   }
   #endif
 
   
   
   FLUSH_AND_OUT:
      
   glPopMatrix();   
   
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Flushing or swapping ...\n", FuncName);
   if (csv->X->DOUBLEBUFFER)
    glXSwapBuffers(csv->X->DPY, XtWindow(csv->X->GLXAREA));
   else
    glFlush();

  /* Avoid indirect rendering latency from queuing. */
  if (!glXIsDirect(csv->X->DPY, csv->X->GLXCONTEXT))
    glFinish();
   
  /* if recording, take a snap */
  if (csv->Record) {
      if (csv->rdc < SUMA_RDC_X_START || csv->rdc > SUMA_RDC_X_END) {
         /*
         Combination below helps partial coverage 
         problem under linux when recording.
         But it does not fix the coverage problem 
         entirely.
         
         SUMA_S_Note("Raising the dead");
         XtPopup (csv->X->TOPLEVEL, XtGrabExclusive);
         XRaiseWindow(XtDisplay(csv->X->TOPLEVEL), XtWindow(csv->X->TOPLEVEL));
       */  
         glFinish();
         glXWaitX();
      #ifdef DARWIN
         { GLvoid *pixels;
           pixels = SUMA_grabPixels(1, csv->X->WIDTH, csv->X->HEIGHT);
           if (pixels) {
             ISQ_snapsave( csv->X->WIDTH, -csv->X->HEIGHT,
                           (unsigned char *)pixels, csv->X->GLXAREA );
             SUMA_free(pixels);
           }
         }
      #else
         ISQ_snapshot ( csv->X->GLXAREA );
      #endif
      }
  }
  
  /* reset rdc, if it is the last thing you'll ever do */
  csv->rdc = SUMA_RDC_NOT_SET;
  
   SUMA_RETURNe;
}

void
SUMA_graphicsInit(Widget w, XtPointer clientData, XtPointer call)
{
   
   XVisualInfo *SUMAg_cVISINFO=NULL;
   static char FuncName[]={"SUMA_graphicsInit"};
   int isv;
   SUMA_SurfaceViewer *sv;
   
   SUMA_ENTRY;

   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV((Widget)w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }

   /* Create OpenGL rendering context. */
   XtVaGetValues(w, GLwNvisualInfo, &SUMAg_cVISINFO, NULL);
   sv->X->GLXCONTEXT = glXCreateContext(XtDisplay(w), SUMAg_cVISINFO,
    0,                  /* No sharing. */
    True);              /* Direct rendering if possible. */

   /* Setup OpenGL state. */
   if (!glXMakeCurrent(XtDisplay(w), XtWindow(w), sv->X->GLXCONTEXT)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in glXMakeCurrent.\n \tContinuing ...\n", FuncName);
   }
   
   /* call context_Init to setup colors and lighting */   
   SUMA_context_Init(sv);

   SUMA_RETURNe;
   
}

void 
SUMA_context_Init(SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_context_Init"};
   GLfloat mat_specular[] = { SUMA_MAT_SPECULAR_INIT};
   GLfloat mat_shininess[] = { SUMA_MAT_SHININESS_INIT };
   GLfloat mat_ambient[] = { SUMA_MAT_AMBIENT_INIT};
   GLfloat mat_diffuse[] = { SUMA_MAT_DIFFUSE_INIT };
   GLfloat mat_emission[] = { SUMA_MAT_EMISSION_INIT  };
   
   /*GLfloat green_light[] = { 0.0, 1.0, 0.0, 1.0};*/
   
   SUMA_ENTRY;
   if (sv->PolyMode == SRM_Hide) { SUMA_SL_Note("sv->PolyMode reset to SRM_Fill"); sv->PolyMode = SRM_Fill; }
   glClearColor (sv->clear_color[0], sv->clear_color[1], sv->clear_color[2], sv->clear_color[3]);
   glShadeModel (GL_SMOOTH);

   SUMA_SET_GL_RENDER_MODE(sv->PolyMode); 
   
      
   /* Set the material properties*/
   glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
   glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
   glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
   glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);
   glMaterialfv(GL_FRONT, GL_EMISSION, mat_emission);
   
   /* set the directional light properties */
   glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);
   glLightfv(GL_LIGHT0, GL_DIFFUSE, sv->light0_color);
   glLightfv(GL_LIGHT0, GL_SPECULAR, sv->light0_color);

   /*glLightfv(GL_LIGHT1, GL_POSITION, sv->light1_position);
   glLightfv(GL_LIGHT1, GL_DIFFUSE, green_light);
   glLightfv(GL_LIGHT1, GL_SPECULAR, green_light);*/
   
   /* set the ambient light */
   glLightModelfv(GL_LIGHT_MODEL_AMBIENT, sv->lmodel_ambient);
 
   glEnable(GL_LIGHTING); /* prepare GL to perform lighting calculations */
   glEnable(GL_LIGHT0); /*Turn lights ON */
   /*glEnable(GL_LIGHT1); */
   glEnable(GL_DEPTH_TEST);
   /* back face culling does not work with SureFit surfaces since facesets are not all defined clockwise or counter clockwise */
   /* also back face culling won't light up the interior of a surface, which is nice to have at times. Like occip patches for example */
   if (sv->BF_Cull) {
      glCullFace (GL_BACK);
      glEnable (GL_CULL_FACE);
   }
   
   /*setup the view point and then setup the lights. Those lights will remain in place regardless of the rotations/translations
   done on the surface */
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   gluLookAt (   sv->GVS[sv->StdView].ViewFrom[0], sv->GVS[sv->StdView].ViewFrom[1], 
               sv->GVS[sv->StdView].ViewFrom[2], sv->GVS[sv->StdView].ViewCenter[0], 
               sv->GVS[sv->StdView].ViewCenter[1], sv->GVS[sv->StdView].ViewCenter[2], 
               sv->GVS[sv->StdView].ViewCamUp[0], sv->GVS[sv->StdView].ViewCamUp[1], 
               sv->GVS[sv->StdView].ViewCamUp[2] );

   /*glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);*/
   /*glLightfv(GL_LIGHT1, GL_POSITION, sv->light1_position);*/

   SUMA_RETURNe;

}

   
void
SUMA_resize(Widget w,
  XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_resize"};
   GLwDrawingAreaCallbackStruct *callData;
   SUMA_SurfaceViewer *sv;
   int isv;
   
   SUMA_ENTRY;

   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }

   /*   fprintf(stdout, "Resizn'...\n");*/
   callData = (GLwDrawingAreaCallbackStruct *) call;
   if (!glXMakeCurrent(XtDisplay(w), XtWindow(w), sv->X->GLXCONTEXT)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in glXMakeCurrent.\n \tContinuing ...\n", FuncName);
   }

   glXWaitX();
   sv->X->WIDTH = callData->width;
   sv->X->HEIGHT = callData->height;
   glViewport(0, 0, callData->width, callData->height);

   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   gluLookAt (   sv->GVS[sv->StdView].ViewFrom[0], sv->GVS[sv->StdView].ViewFrom[1], 
               sv->GVS[sv->StdView].ViewFrom[2], sv->GVS[sv->StdView].ViewCenter[0], 
               sv->GVS[sv->StdView].ViewCenter[1], sv->GVS[sv->StdView].ViewCenter[2], 
               sv->GVS[sv->StdView].ViewCamUp[0], sv->GVS[sv->StdView].ViewCamUp[1], 
               sv->GVS[sv->StdView].ViewCamUp[2]);
   sv->Aspect = (GLfloat) callData->width/(GLfloat) callData->height;
   sv->WindWidth = callData->width; sv->WindHeight = callData->height;
   
   sv->rdc = SUMA_RDC_X_RESIZE;
   SUMA_postRedisplay(w, clientData, call);
   
   SUMA_RETURNe;
}


void
SUMA_expose(Widget w,
  XtPointer clientData, XtPointer call)
{
  static char FuncName[]={"SUMA_expose"};
  int isv;
  SUMA_SurfaceViewer *sv;
  SUMA_Boolean LocalHead = NOPE;
  
  SUMA_ENTRY;
  
   SUMA_LH("Called");
  /*glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);*/ /* No need for that, done in display */
  
  
   /* get the viewer just entered. */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }
   
   sv->rdc = SUMA_RDC_X_EXPOSE;
   
   /* When using multiple viewers, you must reset the OpenGL state variables or risk having abrupt changes with the first click */
   sv->ResetGLStateVariables = YUP;
   SUMA_postRedisplay(w, NULL, NULL);
   
   SUMA_RETURNe;
}

void
SUMA_mapStateChanged(Widget w, XtPointer clientData,
  XEvent * event, Boolean * cont)
{

   static char FuncName[]={"SUMA_mapStateChanged"};
   int isv;
   SUMA_SurfaceViewer *sv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }

   sv->rdc = SUMA_RDC_X_MAPSTATE;
   
   /* When using multiple viewers, you must reset the OpenGL state variables or risk having abrupt changes with the first click */
   sv->ResetGLStateVariables = YUP;

   /*fprintf(stdout, "widget window being mapped/unmapped\n");*/
   switch (event->type) {
   case MapNotify:
      sv->isShaded = NOPE;
      if (sv->GVS[sv->StdView].ApplyMomentum)
         sv->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_CF->X->App, 1, SUMA_momentum, (XtPointer)w);
      break;
   case UnmapNotify:
      sv->isShaded = YUP;
      if (sv->GVS[sv->StdView].ApplyMomentum) {
         if (sv->X->MOMENTUMID) XtRemoveTimeOut(sv->X->MOMENTUMID);
         sv->X->MOMENTUMID = 0;
      }
      break;
   }
  
  SUMA_postRedisplay(w, clientData, NULL);
  
  SUMA_RETURNe;
}

/*! 

   \param ContID (void *) This parameters is stored in the callback structure that the menu items
                        send to the callback. You can pass an integer for a controller's index or 
                        any other structure like a surface object's ID for example.  
   The callback structure value associated with each menu widget (cv) is:
   CBp->ContID = ContID;
   CBp->callback_data = callback_data (that is passed in items);
   This allows you to create multiple versions 
   of the same menu and still be able to dissociate between them.
   \param hint, help (char *) strings for hints and bhelps...(NULL for nothing)
   \param MenuWidgets (Widget *) pointer to a vector that will contain widgets created.
         MenuWidgets[0] is the menu or cascade widgets, MenuWidgets[1]..MenuWidgets[N_wid-1]
         would contain the button and separator widgets specified in items
   \return N_wid (int) number of widgets in MenuWidgets. Obviously you'll need
         to know that ahead of time to allocate for MenuWidgets ... 

-  This function is largely based on BuildMenu in the "Motif Programming Manual"
  Build popup, option and pulldown menus, depending on the menu_type.
  It may be XmMENU_PULLDOWN, XmMENU_OPTION or  XmMENU_POPUP.  Pulldowns
  return the CascadeButton that pops up the menu.  Popups return the menu.
  Option menus are created, but the RowColumn that acts as the option
  "area" is returned unmanaged. (The user must manage it.)
  Pulldown menus are built from cascade buttons, so this function
  also builds pullright menus.  The function also adds the right
  callback for PushButton or ToggleButton menu items.
   
 *** SIGNIFICANT CHANGE to BuildMenu ZSS Apr 23 04   
 *** MUST CALL SUMA_BuildMenuReset BEFORE calling SUMA_BuildMenu
 */
static int i_wid = 0;
static int N_wid = 0;
static int nchar = 0;

void SUMA_BuildMenuReset(int n_max)
{
   static char FuncName[]={"SUMA_BuildMenuReset"};
   SUMA_ENTRY;
   i_wid = 0;
   N_wid = 0;
   nchar = n_max;
   SUMA_RETURNe;
}

int SUMA_BuildMenu(  Widget parent, int menu_type, char *menu_title, 
                     char menu_mnemonic, SUMA_Boolean tear_off, 
                     SUMA_MenuItem *items, 
                     void *ContID, 
                     char *hint, char *help,
                     Widget *MenuWidgets )
{
   static char FuncName[]={"SUMA_BuildMenu"};
   char nlabel[300]="\0";
   Widget menu = NULL, cascade = NULL;
   XmString str=NULL;
   int i=-1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!menu_title) {
      SUMA_S_Warn("menu_title is NULL, alert Rick Reynolds!");
      menu_title = "";
   }
   
   SUMA_LHv("Entered. menu_title %s.\n", menu_title);
      
   if (menu_type == XmMENU_PULLDOWN || menu_type == XmMENU_OPTION)
     menu = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
   else if (menu_type == XmMENU_POPUP)
     menu = XmCreatePopupMenu (parent, "_popup", NULL, 0);
   else {
     XtWarning ("Invalid menu type passed to BuildMenu()");
     SUMA_RETURN(-1);
   }
   

   if (tear_off)
     XtVaSetValues (menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);

   /* Pulldown menus require a cascade button to be made */
   if (menu_type == XmMENU_PULLDOWN) {
     str = XmStringCreateLocalized (menu_title);
     cascade = XtVaCreateManagedWidget (menu_title,
         xmCascadeButtonWidgetClass, parent,
         XmNsubMenuId,   menu,
         XmNlabelString, str,
         XmNmnemonic,    menu_mnemonic,
         XmNmarginHeight, 0,
         XmNmarginTop, 0,
         XmNmarginBottom, 0,
         NULL);
     XmStringFree (str);
   } 
   else if (menu_type == XmMENU_OPTION) {
     /* Option menus are a special case, but not hard to handle */
     Arg args[10];
     int n = 0;
     SUMA_LHv("XmMENU_OPTION %s", menu_title);
     str = XmStringCreateLocalized (menu_title);
     XtSetArg (args[n], XmNsubMenuId, menu); n++;
     XtSetArg (args[n], XmNlabelString, str); n++;
     XtSetArg (args[n], XmNmarginHeight, 0); n++;
     XtSetArg (args[n], XmNmarginTop, 0 ); n++;
     XtSetArg (args[n], XmNmarginBottom, 0 ); n++;
     /* Following three settings have no effect ...
     XtSetArg (args[n], XmNpacking, XmPACK_COLUMN); n++;
     XtSetArg (args[n], XmNnumColumns, 2); n++;
     XtSetArg (args[n], XmNorientation, XmHORIZONTAL); n++;*/
     
     /* This really isn't a cascade, but this is the widget handle
      * we're going to return at the end of the function.
      */
     cascade = XmCreateOptionMenu (parent, menu_title, args, n);
     XmStringFree (str);
   }
   
   /* hide your jewel */
   if (menu_type == XmMENU_POPUP) {  MenuWidgets[i_wid] = menu; }
   else { MenuWidgets[i_wid] = cascade; } 
   
   if (hint) MCW_register_hint(MenuWidgets[i_wid], hint);
   if (help) MCW_reghelp_children(MenuWidgets[i_wid], help);
   
   ++i_wid;
   
   /* Now add the menu items */
   for (i = 0; items[i].label != NULL; i++) {
      if (LocalHead) 
         fprintf (SUMA_STDERR, 
                  "%s: Adding label # %d - %s\n", 
                  FuncName, i, items[i].label);
     /* If subitems exist, create the pull-right menu by calling this
      * function recursively.  Since the function returns a cascade
      * button, the widget returned is used..
      */
     if (items[i].subitems)
         if (menu_type == XmMENU_OPTION) {
             XtWarning ("You can't have submenus from option menu items.");
             continue;
         } 
         else {
             if (LocalHead) 
               fprintf (SUMA_STDERR, "%s: Going for sub-menu.\n", FuncName);
             SUMA_BuildMenu ( menu, XmMENU_PULLDOWN, items[i].label, 
                              items[i].mnemonic, tear_off, 
                              items[i].subitems, ContID, 
                              hint, help, MenuWidgets);
         }
     else {
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Creating widgets MenuWidgets[%d]\n", 
                     FuncName, (int)items[i].callback_data);
         if (nchar > 0) {
            snprintf(nlabel, nchar*sizeof(char), "%s", items[i].label);
            MenuWidgets[i_wid] = XtVaCreateManagedWidget (nlabel,
                *items[i].class, menu,
                NULL);
         } else {
            MenuWidgets[i_wid] = XtVaCreateManagedWidget (items[i].label,
                *items[i].class, menu,
                NULL);
         }
      }

      
      /* Whether the item is a real item or a cascade button with a
      * menu, it can still have a mnemonic.
      */
      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: Setting Mnemonic ...\n", FuncName);
      if (items[i].mnemonic)
         XtVaSetValues (MenuWidgets[i_wid], XmNmnemonic, 
                        items[i].mnemonic, NULL);

      /* any item can have an accelerator, except cascade menus. But,
      * we don't worry about that; we know better in our declarations.
      */

      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: Setting accelerator ...\n", FuncName);
      if (items[i].accelerator) {
         str = XmStringCreateLocalized (items[i].accel_text);
         XtVaSetValues (MenuWidgets[i_wid],
             XmNaccelerator, items[i].accelerator,
             XmNacceleratorText, str,
             NULL);
         XmStringFree (str);
      }

      if (items[i].class == &xmToggleButtonWidgetClass ||
              items[i].class == &xmToggleButtonWidgetClass) {
         Pixel fg_pix=0;
         XtVaGetValues (MenuWidgets[i_wid], XmNforeground, &fg_pix, NULL);
         XtVaSetValues (MenuWidgets[i_wid], XmNselectColor, fg_pix, NULL); 
          
      }
     
      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: Setting callback ...\n", FuncName);
      if (items[i].callback) {
         SUMA_MenuCallBackData *CBp=NULL;
         CBp = (SUMA_MenuCallBackData *)calloc(1,sizeof(SUMA_MenuCallBackData));                      /* There is no freeing of this pointer in SUMA. 
                     Once created, a widget is only destroyed when 
                     SUMA is killed */
         /* prepare the callback pointer */
         CBp->callback_data = (XtPointer) items[i].callback_data;
         CBp->ContID = ContID;
         XtAddCallback (MenuWidgets[i_wid],
             (items[i].class == &xmToggleButtonWidgetClass ||
              items[i].class == &xmToggleButtonWidgetClass) ?
                 XmNvalueChangedCallback : /* ToggleButton class */
                 XmNactivateCallback,      /* PushButton class */
             items[i].callback, (XtPointer)CBp);
      }
      ++i_wid;
   }

   #if 0
    {
      SUMA_LH( "Adding event handler." 
               "Crashes when dealing with File: cascade buttons ....");
      if (menu_type == XmMENU_POPUP) {
         SUMA_ShowMeTheChildren(menu);
      } else {
         SUMA_ShowMeTheChildren(cascade);
      }
      /* add yourself an even handler to deal with long menus a la AFNI 
      see bbox.c */
      if (0) {
      SUMA_ShowMeTheChildren(menu);
      XtInsertEventHandler(   cascade ,      /* handle events in optmenu */
                           ButtonPressMask ,  /* button presses */
                           FALSE ,            /* nonmaskable events? */
                           SUMA_optmenu_EV ,  /* handler */
                           (XtPointer) cascade ,   /* client data */
                           XtListTail ) ;     /* last in queue */
     } 

   }
   #endif
   /* for popup menus, just return the menu; pulldown menus, return
   * the cascade button; option menus, return the thing returned
   * from XmCreateOptionMenu().  This isn't a menu, or a cascade button!
   */
   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: Returning %d widgets created.\n", FuncName, i_wid);
   SUMA_RETURN (i_wid);
}

Widget mainw, menubar, menupane, btn, sep, cascade, frame;
Arg menuPaneArgs[1], args[1];

SUMA_MenuItem FileOpen_menu[] = {
   {  "OpenSpec", &xmPushButtonWidgetClass, \
      'p', "Ctrl<Key>p", "Ctrl+p", \
      SUMA_cb_FileOpenSpec, (XtPointer) SW_FileOpenSpec, NULL},
   
   {  "OpenSurf", &xmPushButtonWidgetClass, \
      'o', "Ctrl<Key>o", "Ctrl+o", \
      SUMA_cb_FileOpenSurf, (XtPointer) SW_FileOpenSurf, NULL},
   
   {NULL} ,
};

SUMA_MenuItem File_menu[] = {
   /*{  "Open", &xmPushButtonWidgetClass, \
      '\0', NULL, NULL, \
      NULL,  (XtPointer) SW_FileOpen, (SUMA_MenuItem *) FileOpen_menu },
   */
   {  "Save View", &xmPushButtonWidgetClass, \
      '\0', NULL, NULL, \
      SUMA_cb_FileSaveView, (XtPointer) SW_FileSaveView, NULL},
   
   {  "Load View", &xmPushButtonWidgetClass, \
      '\0', NULL, NULL, \
      SUMA_cb_FileLoadView, (XtPointer) SW_FileLoadView, NULL},
      
   
   {  "Close", &xmPushButtonWidgetClass, \
      'C', NULL, "Esc", \
      SUMA_cb_FileClose, (XtPointer) SW_FileClose, NULL},
   
   {NULL},
};
 

/* 
SUMA_MenuItem Edit_menu[] = {
   {  "Draw ROI", &xmPushButtonWidgetClass, \
      'D', "Ctrl <Key>d", "Ctrl+D", \
      SUMA_cb_ToolsDrawROI, (XtPointer) SW_ToolsDrawROI, NULL },
   
   {NULL},

};
*/

/* can use combo like: "Ctrl Shift<Key>d", "Ctrl+D"*/
SUMA_MenuItem View_menu[] = {
   {  "SUMA Controller", &xmPushButtonWidgetClass, \
      'U', "Ctrl<Key>u", "Ctrl+u", \
      SUMA_cb_viewSumaCont, (XtPointer) SW_ViewSumaCont, NULL },
   
   {  "Surface Controller", &xmPushButtonWidgetClass, \
      'S', "Ctrl<Key>s", "Ctrl+s", \
      SUMA_cb_viewSurfaceCont, (XtPointer) SW_ViewSurfCont, NULL },
   
   {  "Viewer Controller", &xmPushButtonWidgetClass, \
      'V', "Ctrl<Key>v", "Ctrl+v", \
      SUMA_cb_viewViewerCont, (XtPointer) SW_ViewViewCont, NULL },
   
   {  "Separator 1", &xmSeparatorWidgetClass, \
      '\0', NULL, NULL, \
      NULL, (XtPointer) SW_ViewSep1, NULL },
   
   {  "Cross Hair", &xmToggleButtonWidgetClass, \
      'C', "<Key>F3", "F3",  \
      SUMA_cb_toggle_crosshair, (XtPointer) SW_ViewCrossHair, NULL },
   
   {  "Node in Focus", &xmToggleButtonWidgetClass, \
      'N', "<Key>F4", "F4", \
      SUMA_cb_toggle_node_in_focus, (XtPointer) SW_ViewNodeInFocus, NULL },
      
   {  "Selected Faceset", &xmToggleButtonWidgetClass, \
      'F', "<Key>F5", "F5", \
      SUMA_cb_toggle_selected_faceset, (XtPointer) SW_ViewSelectedFaceset, NULL },
      
   {NULL},
};

SUMA_MenuItem Tools_menu[] = {
   {  "Draw ROI", &xmPushButtonWidgetClass, \
      'D', "Ctrl <Key>d", "Ctrl+d", \
      SUMA_cb_ToolsDrawROI, (XtPointer) SW_ToolsDrawROI, NULL },
   
   {NULL},

};


SUMA_MenuItem Help_menu[] = {
   {  "Usage", &xmPushButtonWidgetClass, \
      'U', "Ctrl <Key>h", "Ctrl+h", \
      SUMA_cb_helpUsage, (XtPointer) SW_HelpUsage, NULL},
      
   {  "Message Log", &xmPushButtonWidgetClass, \
      'L', NULL, NULL, \
      SUMA_cb_helpMessageLog, (XtPointer) SW_HelpMessageLog, NULL},
      
   {  "Separator 1", &xmSeparatorWidgetClass, \
      '\0', NULL, NULL, \
      NULL, (XtPointer) SW_HelpSep1, NULL }, 
   
   {  "SUMA Global", &xmPushButtonWidgetClass, \
      'G', NULL, NULL, \
      SUMA_cb_helpSUMAGlobal, (XtPointer) SW_HelpSUMAGlobal, NULL},
   
   {  "Viewer struct", &xmPushButtonWidgetClass, \
      'V', NULL, NULL, \
      SUMA_cb_helpViewerStruct, (XtPointer) SW_HelpViewerStruct, NULL},
   
   {  "Surface struct", &xmPushButtonWidgetClass, \
      'S', NULL, NULL, \
      SUMA_cb_helpSurfaceStruct, (XtPointer) SW_HelpSurfaceStruct, NULL},
   
   {  "Separator 2", &xmSeparatorWidgetClass, \
      '\0', NULL, NULL, \
      NULL, (XtPointer) SW_HelpSep2, NULL },    
   
   {  "InOut Notify", &xmToggleButtonWidgetClass, \
      'I', NULL, NULL, \
      SUMA_cb_helpIO_notify, (XtPointer) SW_HelpIONotify, NULL},
      
   {  "MemTrace", &xmToggleButtonWidgetClass, \
      'M', NULL, NULL, \
      SUMA_cb_helpMemTrace, (XtPointer) SW_HelpMemTrace, NULL},
   {NULL},
};

SUMA_MenuItem RenderMode_Menu[] = {
   {  "Viewer", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetRenderMode, (XtPointer) SW_SurfCont_RenderViewerDefault, NULL},
      
   {  "Fill", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetRenderMode, (XtPointer) SW_SurfCont_RenderFill, NULL},
   
   {  "Line", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetRenderMode, (XtPointer) SW_SurfCont_RenderLine, NULL},
    
   {  "Points", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetRenderMode, (XtPointer) SW_SurfCont_RenderPoints, NULL},

   {  "Hide", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetRenderMode, (XtPointer) SW_SurfCont_RenderHide, NULL},
        
   {NULL},
};

SUMA_MenuItem DrawROI_SaveMode_Menu[]= {
   {  "1D", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_SaveMode, (XtPointer) SW_DrawROI_SaveMode1D, NULL},
   
   {  "NIML", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_SaveMode, (XtPointer) SW_DrawROI_SaveModeNIML, NULL},
   
   {NULL},
};

SUMA_MenuItem DrawROI_SaveWhat_Menu[]= {
   {  "This", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_SaveWhat, (XtPointer) SW_DrawROI_SaveWhatThis, NULL},
   
   {  "All", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_SaveWhat, (XtPointer) SW_DrawROI_SaveWhatRelated, NULL},
         
   {NULL},
};

SUMA_MenuItem DrawROI_WhatDist_Menu[]= {
   {  "----", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_WhatDist, (XtPointer) SW_DrawROI_WhatDistNothing, NULL},
   
   {  "trace", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_WhatDist, (XtPointer) SW_DrawROI_WhatDistTrace, NULL},
   
   {  "all", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_WhatDist, (XtPointer) SW_DrawROI_WhatDistAll, NULL},
         
   {NULL},
};      

SUMA_Boolean SUMA_X_SurfaceViewer_Create (void)
{
   static char FuncName[]={"SUMA_X_SurfaceViewer_Create"};
   static int CallNum = 0;
   int ic = 0, icr=0;
   char *vargv[1]={ "[A] SUMA" };
   int cargc = 1;
   SUMA_Boolean NewCreation = NOPE, Found=NOPE, Inherit = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   char slabel[20]="\0"; 
       
   SUMA_ENTRY;

   /* Step 1. */
   if (CallNum == 0) { /* first call, initialize App */
      SUMAg_CF->N_OpenSV = 0;
      /*
         SUMAg_SVv[ic].X->TOPLEVEL = XtAppInitialize(&SUMAg_CF->X->App, "SUMA", NULL, 0, &cargc, vargv,
       SUMA_get_fallbackResources(), NULL, 0); Superseded by XtOpenApplication
      */
      SUMAg_SVv[ic].X->TOPLEVEL = 
         XtOpenApplication(&SUMAg_CF->X->App, "SUMA", 
                           NULL, 0, &cargc, vargv,
                           SUMA_get_fallbackResources(), 
                           topLevelShellWidgetClass, NULL, 0); 
      SUMAg_SVv[ic].X->DPY = XtDisplay(SUMAg_SVv[ic].X->TOPLEVEL);
      /* save DPY for first controller opened */
      SUMAg_CF->X->DPY_controller1 = SUMAg_SVv[ic].X->DPY;
      NewCreation = YUP;
      Inherit = NOPE;
   } else {/* not the first call, new controller is required */
      ic = 0;
      Found = NOPE;
      while (ic < SUMA_MAX_SURF_VIEWERS && !Found) {
         if (!SUMAg_SVv[ic].Open) {
            Found = YUP;
         } else {
            ++ic;
         }
      }
      if (!Found) { /* no unopen windows left to open */
         fprintf (SUMA_STDERR,
                  "Error %s: Cannot open more than %d viewers.\n", 
                  FuncName, SUMA_MAX_SURF_VIEWERS);
         SUMA_RETURN (NOPE);
      }
      
      /* an unopen window was found, check its top level widget */
      if (SUMAg_SVv[ic].X->TOPLEVEL == NULL) {
         /* Unopen window found, needs a shell */
         SUMAg_SVv[ic].X->DPY = SUMAg_CF->X->DPY_controller1;
         SUMAg_SVv[ic].X->TOPLEVEL = XtVaAppCreateShell("SUMA" , "Suma" ,
                   topLevelShellWidgetClass , SUMAg_SVv[ic].X->DPY ,
                   XmNinitialResourcesPersistent , False ,
                   NULL ) ;
         NewCreation = YUP;
         Inherit = YUP;
      } else { /* Unopen window found, has a shell already. */
         NewCreation = NOPE;
      }
   }

   if (NewCreation) { /* create widgets, add call backs etc ,,, */
      /* Step 2. */
      XtAddEventHandler(SUMAg_SVv[ic].X->TOPLEVEL, StructureNotifyMask,
       False, SUMA_mapStateChanged, NULL);
      XtAddEventHandler(SUMAg_SVv[ic].X->TOPLEVEL, EnterWindowMask,
       False, SUMA_SetcSV, NULL);
      XtAddEventHandler(SUMAg_SVv[ic].X->TOPLEVEL, LeaveWindowMask,
       False, SUMA_unSetcSV, NULL); 

      /* Step 3 */
      if (!Inherit) {
         if (LocalHead) 
            fprintf(stdout, "trying for cool double buffer visual\n");
         SUMAg_SVv[ic].X->VISINFO = 
            glXChooseVisual(  SUMAg_SVv[ic].X->DPY, 
                              DefaultScreen(SUMAg_SVv[ic].X->DPY), dblBuf);
         if (SUMAg_SVv[ic].X->VISINFO == NULL) {
            fprintf(stdout, "trying lame single buffer visual\n");
            XtAppWarning(SUMAg_CF->X->App, "trying lame single buffer visual");
            SUMAg_SVv[ic].X->VISINFO = 
               glXChooseVisual(  SUMAg_SVv[ic].X->DPY, 
                                 DefaultScreen(SUMAg_SVv[ic].X->DPY), snglBuf);
          if (SUMAg_SVv[ic].X->VISINFO == NULL) {
            XtAppError(SUMAg_CF->X->App, "no good visual");
            SUMA_RETURN (NOPE);
          }
          SUMAg_SVv[ic].X->DOUBLEBUFFER = False;
         }
      } else {
         SUMA_LH("This is new. Inheriting");
         SUMAg_SVv[ic].X->VISINFO = SUMAg_SVv[0].X->VISINFO;
         SUMAg_SVv[ic].X->DOUBLEBUFFER = SUMAg_SVv[0].X->DOUBLEBUFFER;
      }
		
      #ifdef USING_LESSTIF 
         SUMA_LH("Using LessTif Libraries.");
      #else
         SUMA_LH("Using Open Motif libraries");
      #endif

      /* Step 3.5 Wed Dec 18 14:49:25 EST 2002 - The GUI*/
         /* see Kilgard's OpenGL Programming for the X window system */
         /* create main window */
         SUMA_LH("Creating Main Window");
	 /* Next call Causes Seg. fault on Fedora Core 4. Not much I can do. Same happens with demo code paperplane.c by Kilgard */
         mainw = XmCreateMainWindow (SUMAg_SVv[ic].X->TOPLEVEL, "mainw", NULL, 0); 
         SUMA_LH("Managing Main Window");
         XtManageChild (mainw);      
         /* create menu bar */
         menubar = XmCreateMenuBar (mainw, "menubar", NULL, 0);
         XtManageChild (menubar);
         
         /* create File Menu */
         SUMA_BuildMenuReset(0);
         SUMA_BuildMenu(menubar, XmMENU_PULLDOWN, 
                                 "File", 'F', YUP, File_menu, 
                                 (void *)ic, NULL, NULL,  
                                 SUMAg_SVv[ic].X->FileMenu );
         
         /* create View Menu */
         SUMA_BuildMenuReset(0);
         SUMA_BuildMenu(menubar, XmMENU_PULLDOWN, 
                                 "View", 'V', YUP, View_menu, 
                                 (void *)ic, NULL, NULL,  
                                 SUMAg_SVv[ic].X->ViewMenu );
         
         /* create Tools Menu */
         SUMA_BuildMenuReset(0);
         SUMA_BuildMenu(menubar, XmMENU_PULLDOWN, 
                                 "Tools", 'T', YUP, Tools_menu, 
                                 (void *)ic, NULL, NULL,  
                                 SUMAg_SVv[ic].X->ToolsMenu );
         
         /* create Help Menu */
         SUMA_BuildMenuReset(0);
         SUMA_BuildMenu(menubar, XmMENU_PULLDOWN, 
                                 "Help", 'H', YUP, Help_menu,
                                 (void *)ic, NULL, NULL,  
                                 SUMAg_SVv[ic].X->HelpMenu );
         
         XtVaSetValues (menubar, XmNmenuHelpWidget, SUMAg_SVv[ic].X->HelpMenu[SW_Help], NULL);
                                 
         /* set states of the some view menu widgets */
         XmToggleButtonSetState (SUMAg_SVv[ic].X->ViewMenu[SW_ViewCrossHair], 
            SUMAg_SVv[ic].ShowCrossHair, NOPE);
         
         XmToggleButtonSetState (SUMAg_SVv[ic].X->HelpMenu[SW_HelpMemTrace], 
            SUMAg_CF->MemTrace, NOPE);
         if (SUMAg_CF->MemTrace) {  XtSetSensitive (SUMAg_SVv[ic].X->HelpMenu[SW_HelpMemTrace], 0); }

         XmToggleButtonSetState (SUMAg_SVv[ic].X->HelpMenu[SW_HelpIONotify], 
            SUMAg_CF->InOut_Notify, NOPE);
         
 
         
        SUMAg_SVv[ic].X->CMAP = SUMA_getShareableColormap(&(SUMAg_SVv[ic]));

        /* create a frame to put glxarea in */
        SUMAg_SVv[ic].X->FRAME = XmCreateFrame (mainw, "frame", NULL, 0);
        XtManageChild(SUMAg_SVv[ic].X->FRAME);
      
      #ifdef SUMA_MOTIF_GLXAREA
        SUMA_LH("MOTIF Drawing Area");
        /* Step 6. */
         /* glwMDrawingAreaWidgetClass requires libMesaGLwM.a */
         SUMAg_SVv[ic].X->GLXAREA = XtVaCreateManagedWidget("glxarea",
          glwMDrawingAreaWidgetClass, SUMAg_SVv[ic].X->FRAME,
          GLwNvisualInfo, SUMAg_SVv[ic].X->VISINFO,
          XtNcolormap, SUMAg_SVv[ic].X->CMAP,
          NULL);
      #else
        SUMA_LH("GL Drawing Area");
         /* glwDrawingAreaWidgetClass requires libMesaGLw.a */
         SUMAg_SVv[ic].X->GLXAREA = XtVaCreateManagedWidget("glxarea",
          glwDrawingAreaWidgetClass, SUMAg_SVv[ic].X->FRAME,
          GLwNvisualInfo, SUMAg_SVv[ic].X->VISINFO,
          XtNcolormap, SUMAg_SVv[ic].X->CMAP,
          NULL);
      #endif

          
      /* Step 7. */
      XtAddCallback(SUMAg_SVv[ic].X->GLXAREA, GLwNginitCallback, SUMA_graphicsInit, NULL);
      XtAddCallback(SUMAg_SVv[ic].X->GLXAREA, GLwNexposeCallback, SUMA_expose, NULL);
      XtAddCallback(SUMAg_SVv[ic].X->GLXAREA, GLwNresizeCallback, SUMA_resize, NULL);
      XtAddCallback(SUMAg_SVv[ic].X->GLXAREA, GLwNinputCallback, SUMA_input, NULL);

      /* trap for window kill */
      
      /* turn off default delete response. If you do not do that, you will suffer.*/
       XtVaSetValues( SUMAg_SVv[ic].X->TOPLEVEL,
           XmNdeleteResponse, XmDO_NOTHING,
           NULL);      
         
      XmAddWMProtocolCallback(           /* make "Close" window menu work */
           SUMAg_SVv[ic].X->TOPLEVEL,
           XmInternAtom( SUMAg_SVv[ic].X->DPY , "WM_DELETE_WINDOW" , False ) ,
           SUMA_ButtClose_pushed , NULL ) ;
           
      /* Step 8. */
      XtRealizeWidget(SUMAg_SVv[ic].X->TOPLEVEL);
      
      /* I will need a Graphics Context variable to draw into the window */
      {  
         XGCValues gcv; /* see program drawing.c in Motif Programming Manual, Ch. 10 */
         gcv.foreground = BlackPixelOfScreen (XtScreen (SUMAg_SVv[ic].X->GLXAREA));
         SUMAg_SVv[ic].X->gc = XCreateGC (SUMAg_SVv[ic].X->DPY,
                                          XtWindow (SUMAg_SVv[ic].X->GLXAREA), 
                                          GCForeground, &gcv);
         SUMA_SetSVForegroundColor (&SUMAg_SVv[ic], "Green");

      }
      /* keep track of count */
      SUMAg_N_SVv += 1;
      /* position window next to the previous one open */
      {
         Found = NOPE;
         icr = SUMA_MAX_SURF_VIEWERS - 1;
         while (icr >= 0 && !Found) {
            if (icr != ic && SUMAg_SVv[icr].Open) {
               Found = YUP;
            } else {
               --icr;
            }
         }
         if (Found) {
            SUMA_PositionWindowRelative(SUMAg_SVv[ic].X->TOPLEVEL,
                                        SUMAg_SVv[icr].X->TOPLEVEL,
                                        SWP_TOP_RIGHT);
         } else {
            if (SUMA_isEnv("SUMA_StartUpLocation", "POINTER"))
               SUMA_PositionWindowRelative(SUMAg_SVv[ic].X->TOPLEVEL,
                                        NULL,
                                        SWP_POINTER_OFF);
            else {
               /*default, do nothing */
            }
         }
      
      }       
   } else { /* widget already set up, just undo whatever was done in SUMA_ButtClose_pushed */
      
      #ifdef SUMA_USE_WITHDRAW
         XMapRaised(SUMAg_SVv[ic].X->DPY, XtWindow(SUMAg_SVv[ic].X->TOPLEVEL));      
      #endif
      
      /* add the workprocess again */
      SUMA_register_workproc( SUMA_handleRedisplay, SUMAg_SVv[ic].X->GLXAREA );
      SUMAg_SVv[ic].X->REDISPLAYPENDING = 0;
   }

   SUMAg_SVv[ic].Open = YUP;
   ++SUMAg_CF->N_OpenSV;
   ++CallNum;
   
   SUMA_UpdateViewerCursor (&(SUMAg_SVv[ic]));
   SUMA_UpdateViewerTitle (&(SUMAg_SVv[ic]));

   SUMA_RETURN (YUP);
}

void SUMA_ButtOpen_pushed (Widget w, XtPointer cd1, XtPointer cd2)
{
   static char FuncName[]={"SUMA_ButtOpen_pushed"};
   
   SUMA_ENTRY;

   if (!SUMA_X_SurfaceViewer_Create ()) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_X_SurfaceViewer_Create.\n", FuncName);
   } 
   SUMA_RETURNe;
}

void SUMA_cb_FileOpenSpec (Widget w, XtPointer data, XtPointer calldata) 
{
   static char FuncName[]={"SUMA_cb_FileOpenSpec"};
   
   SUMA_ENTRY;
   
   fprintf (SUMA_STDERR, "%s: called.\n", FuncName);
   SUMA_RETURNe;
}

void SUMA_cb_FileOpenSurf (Widget w, XtPointer data, XtPointer calldata) 
{
   static char FuncName[]={"SUMA_cb_FileOpenSurf"};
   
   SUMA_ENTRY;
   
   fprintf (SUMA_STDERR, "%s: called.\n", FuncName);
   SUMA_RETURNe;
}

void SUMA_cb_FileSaveView (Widget w, XtPointer data, XtPointer calldata) 
{
   static char FuncName[]={"SUMA_cb_FileSaveView"};
   int isv, widtype;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   SUMA_SurfaceViewer *sv;
   
   SUMA_ENTRY;
   
   /* find the index of the viewer closed */
   SUMA_VIEWER_FROM_FILEMENU_CALLBACK(data, isv, widtype);
   if (widtype != SW_FileSaveView) {
      fprintf (SUMA_STDERR, "Error %s: Something really bad has happened.\n", FuncName);
      SUMA_RETURNe;
   }   

   sv = &SUMAg_SVv[isv];
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_SaveViewFileSelection);
   if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, sv->X->TOPLEVEL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
   }
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
   /*
   if (!SUMA_SaveVisualState(NULL, (void*)sv)) {
      SUMA_SLP_Err("Failed to save view.");
      SUMA_RETURNe;
   }
   */
   
   SUMA_RETURNe;
}

void SUMA_cb_FileLoadView (Widget w, XtPointer data, XtPointer calldata) 
{
   static char FuncName[]={"SUMA_cb_FileLoadView"};
   int isv, widtype;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   SUMA_SurfaceViewer *sv;
   
   SUMA_ENTRY;
   
   /* find the index of the viewer closed */
   SUMA_VIEWER_FROM_FILEMENU_CALLBACK(data, isv, widtype);
   if (widtype != SW_FileLoadView) {
      fprintf (SUMA_STDERR, "Error %s: Something really bad has happened.\n", FuncName);
      SUMA_RETURNe;
   }   

   sv = &SUMAg_SVv[isv];
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_LoadViewFileSelection);
   if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, sv->X->TOPLEVEL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
   }
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
   /*
   if (!SUMA_LoadVisualState(NULL, (void*)sv)) {
      SUMA_SLP_Err("Failed to load view.");
      SUMA_RETURNe;
   }
   */
   
   SUMA_RETURNe;
}


void SUMA_cb_FileClose (Widget w, XtPointer data, XtPointer calldata) 
{
   static char FuncName[]={"SUMA_cb_FileClose"};
   int isv, widtype;
   SUMA_SurfaceViewer *sv;
   
   SUMA_ENTRY;
   
   /* find the index of the viewer closed */
   SUMA_VIEWER_FROM_FILEMENU_CALLBACK(data, isv, widtype);
   if (widtype != SW_FileClose) {
      fprintf (SUMA_STDERR, "Error %s: Something really bad has happened.\n", FuncName);
      SUMA_RETURNe;
   }   

   sv = &SUMAg_SVv[isv];
   SUMA_ButtClose_pushed (sv->X->GLXAREA, data, calldata);
      
   SUMA_RETURNe;
}


/*!
  \brief Close the viewer. Exit if it is last viewer to be closed. 
*/
void SUMA_ButtClose_pushed (Widget w, XtPointer cd1, XtPointer cd2)
{
   static char FuncName[]={"SUMA_ButtClose_pushed"};
   int ic, Found;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_LH("Called");
   ic = 0;
   Found = 0;
   while (ic < SUMA_MAX_SURF_VIEWERS && !Found) {
      #if 0 
      /*use once you have a close button with its widget*/
      if (SUMAg_SVv[ic].X->ButtClose == w) {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Close order from button.\n", FuncName);
         Found = 1;
      }
      #endif
      if (SUMAg_SVv[ic].X->TOPLEVEL == w) {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Close order from window manager.\n", FuncName);
         Found = 1;
      }else if (SUMAg_SVv[ic].X->GLXAREA == w) { 
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Close order from GLX area.\n", FuncName);
         Found = 1;
      }
      
      if (!Found) ++ic;
   }
   
   if (Found) {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Widget Found\n", FuncName);
         
         /* Must turn off all workprocesses and timeouts for this surface viewer */
         
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Turning off workprocesses and timeouts ...\n", FuncName);
         if (SUMAg_SVv[ic].GVS[SUMAg_SVv[ic].StdView].ApplyMomentum) {
            if (SUMAg_SVv[ic].X->MOMENTUMID) XtRemoveTimeOut(SUMAg_SVv[ic].X->MOMENTUMID); 
            SUMAg_SVv[ic].X->MOMENTUMID = 0;
         }
         
         /* remove Redisplay workprocess*/
         SUMA_remove_workproc2( SUMA_handleRedisplay, SUMAg_SVv[ic].X->GLXAREA );
         
         /* flush display */
         if (SUMAg_SVv[ic].X->DOUBLEBUFFER)
             glXSwapBuffers(SUMAg_SVv[ic].X->DPY, XtWindow(SUMAg_SVv[ic].X->GLXAREA));
          else
            glFlush();
         
         /* done cleaning up, deal with windows ... */
         
         /** Fri Jan  3 09:51:35 EST 2003
             XtUnrealizeWidget is not used anymore because it destroys windows associated with a widget and its descendants.
            There's no need for that here. 
            Also, destroying widgets should not be used either because that would automatically destroy the SUMA controller which is a 
            child of one of the viewers. The code for destroy is left for historical reasons.*/
         #ifdef SUMA_USE_WITHDRAW 
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing it.\n", FuncName);
            XWithdrawWindow(SUMAg_SVv[ic].X->DPY, 
               XtWindow(SUMAg_SVv[ic].X->TOPLEVEL), 
               XScreenNumberOfScreen(XtScreen(SUMAg_SVv[ic].X->TOPLEVEL)));
            if (SUMAg_SVv[ic].X->ViewCont->TopLevelShell) {
               XWithdrawWindow(SUMAg_SVv[ic].X->DPY, 
               XtWindow(SUMAg_SVv[ic].X->ViewCont->TopLevelShell),
               XScreenNumberOfScreen(XtScreen(SUMAg_SVv[ic].X->ViewCont->TopLevelShell)));
            }
         #endif
         #ifdef SUMA_USE_DESTROY 
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying it.\n", FuncName);
            XtDestroyWidget(SUMAg_SVv[ic].X->TOPLEVEL);
            SUMAg_SVv[ic].X->TOPLEVEL = NULL;      
            
            /* no need to destroy viewer controller */
            SUMAg_SVv[ic].X->ViewCont->TopLevelShell = NULL;
            
            /* update the count */
            SUMAg_N_SVv -= 1;

         #endif

         SUMAg_SVv[ic].Open = NOPE;
         --SUMAg_CF->N_OpenSV;
         if (SUMAg_CF->N_OpenSV == 0) {
            if (LocalHead) fprintf (SUMA_STDERR,"%s: No more viewers, exiting.\n", FuncName);
            /* not quite necessary but for completeness */
            if (SUMAg_CF->X->SumaCont->AppShell) {
               XtDestroyWidget(SUMAg_CF->X->SumaCont->AppShell);
            }
            exit(0);
         }
   } else {
      fprintf (SUMA_STDERR,"Error %s: Widget not Found!.\n", FuncName);
   }
   
    SUMA_RETURNe;
}

Colormap SUMA_getShareableColormap_Eng (XVisualInfo * vi, Display *dpy) 
{
   Status status;
   XStandardColormap *standardCmaps;
   int i, numCmaps;
   Colormap cmap;
   SUMA_Boolean LocalHead = NOPE;
   static char FuncName[]={"SUMA_getShareableColormap_Eng"};
   
   SUMA_ENTRY;

   /* Be lazy; using DirectColor too involved for this example. */
#if defined(__cplusplus) || defined(c_plusplus)
   if (vi->c_class != TrueColor) {
      SUMA_S_Crit("SUMA has no support for non-TrueColor visual");
      exit(1);
   }
#else 
   if (vi->class != TrueColor) {
      SUMA_S_Crit("SUMA has no no support for non-TrueColor visual");
      exit(1);
   }
#endif

   /* If no standard colormap but TrueColor, just make an
     unshared one. */
   status = XmuLookupStandardColormap(dpy, vi->screen, vi->visualid,
    vi->depth, XA_RGB_DEFAULT_MAP,
    False,              /* Replace. */
    True);              /* Retain. */
   if (status == 1) {
    status = XGetRGBColormaps(dpy, RootWindow(dpy, vi->screen),
      &standardCmaps, &numCmaps, XA_RGB_DEFAULT_MAP);
    if (status == 1)
      for (i = 0; i < numCmaps; i++)
        if (standardCmaps[i].visualid == vi->visualid) {
          cmap = standardCmaps[i].colormap;
          XFree(standardCmaps);
          SUMA_RETURN(cmap);
        }
   }
  cmap = XCreateColormap(dpy, RootWindow(dpy, vi->screen), vi->visual, AllocNone);

  SUMA_RETURN(cmap);

}
Colormap
SUMA_getShareableColormap(SUMA_SurfaceViewer *csv)
{
   SUMA_Boolean LocalHead = NOPE;
   static char FuncName[]={"SUMA_getShareableColormap"};
   
   SUMA_ENTRY;

   SUMA_RETURN(SUMA_getShareableColormap_Eng(csv->X->VISINFO, csv->X->DPY));
}

void SUMA_SetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont)
{
   static char FuncName[]={"SUMA_SetcSV"};
   SUMA_SurfaceViewer *sv;
   int isv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Called, w = %p\n", FuncName, w);
   

   /* When using multiple viewers, you must reset the OpenGL state variables or risk having abrupt changes with the first click */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }

   #ifdef DARWIN
      /* Set the focus manually.
      If you're not using motif widgets, window focus is not managed.
      You can manage it yourself with XSetInputFocus when the EnterWindowEvent is captured.
      You don't need to do that however if you link (for some reason) to -lXm.
      But on the macosx10, -lXm does not help, so we manage the foucs ourselves */
      /* The downside is that this call seems to be related to a crash on the mac,
      reported as a complaint coming from X_SetInputFocus.
      The crash happened when multiple viewers were open and one of them was closed
      AND suma was set to prompt the user with "Close this viewer?"
      I tried resetting the focus to a viewer that is not being closed before
      closing the reviewer as requested by the user but that did not help.
      It turns out that the problem does not occur if the prompt window does not
      appear on top of the viewer to be closed. More precisely, if the pointer ends
      up on top of the viewer just after the prompt window disapears SUMA crashes with the 
      message: X Error of failed request: BadMatch (invalid parameter attributes) ...
      The solution is simple, on DARWIN, make sure prompt window appears to the right
      of the viewer. If the user moves it back over the viewer AND then presses YES to close 
      the viewer then they will suffer the crash. June 15 04*/

      XSetInputFocus(sv->X->DPY, XtWindow(w), RevertToPointerRoot, CurrentTime);
   #endif

   sv->rdc = SUMA_RDC_X_ENTER_WINDOW;
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: in Surface Viewer #%d.\n", FuncName, isv);
   sv->ResetGLStateVariables = YUP;  

   SUMA_postRedisplay(w, clientData, NULL);

   
   SUMA_RETURNe;
}

void SUMA_unSetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont)
{
   static char FuncName[]={"SUMA_unSetcSV"};
   
   SUMA_ENTRY;
   SUMA_RETURNe;
}

/* ------------------------------------------------------------------------------------------------------------*/
/*! 
 
 functions SUMA_generateEPS, SUMA_grabPixels, SUMA_RenderToPixMap are straight from pixmap2eps.c

 COPYRIGHT NOTICE FROM pixmap2eps.c
 Copyright (c) Mark J. Kilgard, 1996. 

 This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. 

 \sa OpenGl, Programming for the X Window System, pp 94, 95
 
*/

int
SUMA_generateEPS(char *filename, int inColor, unsigned int width, unsigned int height)
{
   FILE *fp;
   GLvoid *pixels;
   unsigned char *curpix;
   int components, pos, i;
   static char FuncName[]={"SUMA_generateEPS"};
   
   SUMA_ENTRY;

   pixels = SUMA_grabPixels(inColor, width, height);
   
   if (pixels == NULL)
    SUMA_RETURN (1);
   if (inColor)
    components = 3;     /* Red, green, blue. */
   else
    components = 1;     /* Luminance. */

   fp = fopen(filename, "w");
   if (fp == NULL) {
    SUMA_RETURN (2);
   }
   fprintf(fp, "%%!PS-Adobe-2.0 EPSF-1.2\n");
   fprintf(fp, "%%%%Creator: OpenGL pixmap render output\n");
   fprintf(fp, "%%%%BoundingBox: 0 0 %d %d\n", width, height);
   fprintf(fp, "%%%%EndComments\n");
   fprintf(fp, "gsave\n");
   fprintf(fp, "/bwproc {\n");
   fprintf(fp, "    rgbproc\n");
   fprintf(fp, "    dup length 3 idiv string 0 3 0\n");
   fprintf(fp, "    5 -1 roll {\n");
   fprintf(fp, "    add 2 1 roll 1 sub dup 0 eq\n");
   fprintf(fp, "    { pop 3 idiv 3 -1 roll dup 4 -1 roll dup\n");
   fprintf(fp, "        3 1 roll 5 -1 roll put 1 add 3 0 }\n");
   fprintf(fp, "    { 2 1 roll } ifelse\n");
   fprintf(fp, "    } forall\n");
   fprintf(fp, "    pop pop pop\n");
   fprintf(fp, "} def\n");
   fprintf(fp, "systemdict /colorimage known not {\n");
   fprintf(fp, "    /colorimage {\n");
   fprintf(fp, "        pop\n");
   fprintf(fp, "        pop\n");
   fprintf(fp, "        /rgbproc exch def\n");
   fprintf(fp, "        { bwproc } image\n");
   fprintf(fp, "    } def\n");
   fprintf(fp, "} if\n");
   fprintf(fp, "/picstr %d string def\n", width * components);
   fprintf(fp, "%d %d scale\n", width, height);
   fprintf(fp, "%d %d %d\n", width, height, 8);
   fprintf(fp, "[%d 0 0 %d 0 0]\n", width, height);
   fprintf(fp, "{currentfile picstr readhexstring pop}\n");
   fprintf(fp, "false %d\n", components);
   fprintf(fp, "colorimage\n");

   curpix = (unsigned char *) pixels;
   pos = 0;
   for (i = width * height * components; i > 0; i--) {
    fprintf(fp, "%02hx", *curpix++);
    if (++pos >= 32) {
      fprintf(fp, "\n");
      pos = 0;
    }
   }
   if (pos)
    fprintf(fp, "\n");

   fprintf(fp, "grestore\n");
   SUMA_free(pixels);
   fclose(fp);
   SUMA_RETURN (0);
}

GLvoid *
SUMA_grabPixels(int inColor, unsigned int width, unsigned int height)
{
   GLvoid *buffer;
   GLint swapbytes, lsbfirst, rowlength;
   GLint skiprows, skippixels, alignment;
   GLenum format;
   unsigned int size;
   static char FuncName[]={"SUMA_grabPixels"};

   SUMA_ENTRY;
   
   if (inColor) {
    format = GL_RGB;
    size = width * height * 3;
   } else {
    format = GL_LUMINANCE;
    size = width * height * 1;
   }

   buffer = (GLvoid *) SUMA_malloc(size);
   if (buffer == NULL)
    SUMA_RETURN (buffer);

   /* Save current modes. */
   glGetIntegerv(GL_PACK_SWAP_BYTES, &swapbytes);
   glGetIntegerv(GL_PACK_LSB_FIRST, &lsbfirst);
   glGetIntegerv(GL_PACK_ROW_LENGTH, &rowlength);
   glGetIntegerv(GL_PACK_SKIP_ROWS, &skiprows);
   glGetIntegerv(GL_PACK_SKIP_PIXELS, &skippixels);
   glGetIntegerv(GL_PACK_ALIGNMENT, &alignment);
   /* Little endian machines (DEC Alpha for example) could
     benefit from setting GL_PACK_LSB_FIRST to GL_TRUE
     instead of GL_FALSE, but this would require changing the
     generated bitmaps too. */
   glPixelStorei(GL_PACK_SWAP_BYTES, GL_TRUE);
   glPixelStorei(GL_PACK_LSB_FIRST, GL_TRUE);
   glPixelStorei(GL_PACK_ROW_LENGTH, 0);
   glPixelStorei(GL_PACK_SKIP_ROWS, 0);
   glPixelStorei(GL_PACK_SKIP_PIXELS, 0);
   glPixelStorei(GL_PACK_ALIGNMENT, 1);

   /* Actually read the pixels. */
   glReadPixels(0, 0, width, height, format,
    GL_UNSIGNED_BYTE, (GLvoid *) buffer);

   /* Restore saved modes. */
   glPixelStorei(GL_PACK_SWAP_BYTES, swapbytes);
   glPixelStorei(GL_PACK_LSB_FIRST, lsbfirst);
   glPixelStorei(GL_PACK_ROW_LENGTH, rowlength);
   glPixelStorei(GL_PACK_SKIP_ROWS, skiprows);
   glPixelStorei(GL_PACK_SKIP_PIXELS, skippixels);
   glPixelStorei(GL_PACK_ALIGNMENT, alignment);
   SUMA_RETURN (buffer);
}
 

SUMA_Boolean SUMA_RenderToPixMap (SUMA_SurfaceViewer *csv, SUMA_DO *dov) 
{
   static int configuration[] = { GLX_DOUBLEBUFFER, GLX_RGBA, GLX_DEPTH_SIZE, 16,
   GLX_RED_SIZE, 1, GLX_GREEN_SIZE, 1, GLX_BLUE_SIZE, 1, None};
   Display *dpy;
   XVisualInfo *vi;
   GLXContext cx;
   Pixmap pmap;
   GLXPixmap glxpmap;
   static char FuncName[]={"SUMA_RenderToPixMap"};

   SUMA_ENTRY;
   SUMA_S_Note("CALLED!");
   dpy = XOpenDisplay(NULL);
   if (dpy == NULL)
    fprintf(SUMA_STDERR,"Error %s: could not open display", FuncName);

   if (!glXQueryExtension(dpy, NULL, NULL))
    fprintf(SUMA_STDERR,"Error %s: X server has no OpenGL GLX extension", FuncName);

   /* find an OpenGL-capable RGB visual with depth buffer */
   #if 1  /* use screen rendering Xvisual */
   vi = glXChooseVisual(dpy, DefaultScreen(dpy), &configuration[1]);
   if (vi == NULL) {
   /*fprintf(SUMA_STDERR,"%s: Trying to use useless double buffering configuration.\n", FuncName);*/
    vi = glXChooseVisual(dpy, DefaultScreen(dpy), &configuration[0]);
    if (vi == NULL) {
      fprintf(SUMA_STDERR,"Error %s: no appropriate RGB visual with depth buffer", FuncName);
    }
   }
   #else
   vi = csv->X->VISINFO;
   #endif


   /* create an OpenGL rendering context */
   cx = glXCreateContext(dpy, vi,
    NULL,               /* no sharing of display lists */
    False);             /* direct rendering if possible */
   if (cx == NULL)
    fprintf(SUMA_STDERR,"Error %s: could not create rendering context", FuncName);

   pmap = XCreatePixmap(dpy, RootWindow(dpy, vi->screen),
    csv->X->WIDTH, csv->X->HEIGHT, vi->depth);
   glxpmap = glXCreateGLXPixmap(dpy, vi, pmap);
   if (!glXMakeCurrent(dpy, glxpmap, cx)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in glXMakeCurrent.\n \tContinuing ...\n", FuncName);
   }

   SUMA_context_Init(csv);
   glViewport(0, 0, csv->X->WIDTH, csv->X->HEIGHT);
   SUMA_display(csv, dov);

   glFinish (); /* make sure you wait until rendering is over */

   /* find out the next best name and write it*/
   {
        char tmpprfx[100], *padprfx, *padname;
      int cntindx=0;
      SUMA_SurfaceObject *SO;
      SUMA_Boolean OKname = NOPE;
      
      /* get the SO in focus, use it's label for output filename */
      if (csv->Focus_SO_ID >= 0) {
         SO = (SUMA_SurfaceObject *)(SUMAg_DOv[csv->Focus_SO_ID].OP);
      }else {
         SO = NULL;
      }
      
      if (!SO){
         padname = (char *)SUMA_calloc(100, sizeof(char));
      }else {
         if (!SO->Label) { /* nothing set, proceed with default */
            padname = (char *)SUMA_calloc(100, sizeof(char));
         } else {
            padname = (char *)SUMA_calloc(strlen(SO->Label)+10, sizeof(char));
         }
      }
      while (!OKname) {
         sprintf (tmpprfx, "%d", cntindx);
         padprfx = SUMA_pad_str (tmpprfx, '0', 4, 0);
         if (!SO) {
            sprintf(padname,"suma_img%s.eps", padprfx);
         }else {
            sprintf(padname,"%s_%s.eps", SO->Label, padprfx);
         }
         if (SUMA_filexists(padname)) {
            ++cntindx;
         } else { OKname = YUP; }

         SUMA_free(padprfx);
      }

     fprintf (SUMA_STDOUT,"%s: Writing image to %s ...", FuncName, padname);
     SUMA_generateEPS(padname, /* color */ 1, csv->X->WIDTH, csv->X->HEIGHT);
     fprintf (SUMA_STDOUT,"Done.\n");
     SUMA_free(padname);
   }

   /* render to original context */
   if (!glXMakeCurrent(XtDisplay(csv->X->GLXAREA), XtWindow(csv->X->GLXAREA),  csv->X->GLXCONTEXT)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in glXMakeCurrent.\n \tContinuing ...\n", FuncName);   
   }

   SUMA_RETURN (YUP);
}

/* ------------------------------------------------------------------------------------------------------------*/
/*! Returns the index of the node that neighbors inode in a direction
that most closely follows the direction in dd expressed in screen coordinate 
space 
   \param sv
   \param SO
   \param inode (int) node in question for which we seek a neighbor along dd
   \param dd (2x1 double) direction in screen units along which we wish to find 
                          a neighbor of inode. Make sure dd's norm is 1
   \return inodenext (int) -2 in case of error
                           -1 in case of no plausible move along dd.
                              the function allows moves as long as the 
                              dot product is > 0
                           the node neighboring inode such that the screen 
                           projection of vector inode-->inodenext is the closest
                           in direction to dd
*/
int SUMA_NodeNeighborAlongScreenDirection(SUMA_SurfaceViewer *sv,
                                          SUMA_SurfaceObject *SO,
                                          int inode, double *dd)
{
   static char FuncName[]={"SUMA_NodeNeighborAlongScreenDirection"};
   int ii, jj, ineighb=0, inodenext = -2, idd=-1;
   double *p=NULL;
   double *s=NULL, dot=0.0, dotmax=0.0;
   int *q=NULL;
   double dir[3]={0.0,0.0,0.0}, norm=0.0;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !sv || !dd) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(-2);
   }
   /* get all neighbors */
   p = (double *)SUMA_calloc( SO->NodeDim*SO->FN->N_Neighb_max+1, 
                              sizeof(double));
   s = (double *)SUMA_calloc( SO->NodeDim*SO->FN->N_Neighb_max+1, 
                              sizeof(double));
   q = (int *)SUMA_calloc( SO->FN->N_Neighb_max+1, sizeof(int));
   for (jj=0; jj<SO->NodeDim; ++jj) {
      p[jj] = SO->NodeList[SO->NodeDim*inode+jj];
   }
   for (ii=0; ii<SO->FN->N_Neighb[inode]; ++ii) {
      for (jj=0; jj<SO->NodeDim; ++jj) {
         ineighb = SO->FN->FirstNeighb[inode][ii];
         p[SO->NodeDim*(ii+1)+jj] =
               SO->NodeList[SO->NodeDim*ineighb+jj];
      }
   }
   /* find screen projection of neighbors */
   if (!SUMA_World2ScreenCoords( sv, SO->FN->N_Neighb[inode]+1,
                                 p , s, q, 0)) {
      SUMA_S_Err("The world has failed me");                                            SUMA_RETURN(-2);
   }

   if (LocalHead) {
      fprintf(SUMA_STDERR," S = [\n");
      for (ii=0; ii<SO->FN->N_Neighb[inode]+1; ++ii) {
         fprintf(SUMA_STDERR, "%.3f  %.3f  %.3f\n", 
                              s[3*ii], s[3*ii+1], s[3*ii+2]);
      }
      fprintf(SUMA_STDERR,"];\n");
   }

   /* find closest to desired direction */
   for (ii=0; ii<SO->FN->N_Neighb[inode]; ++ii) { 
         /* for each neighbor*/        
      /* direction on screen (only x y needed)*/
      for (jj=0; jj<2; ++jj) {
         dir[jj] = s[(ii+1)*SO->NodeDim+jj] - s[jj]; 
      }
      SUMA_NORM_VEC(dir,2,norm);
      /* calculate dot product*/
      dot = dir[0]*dd[0]/norm + dir[1]*dd[1]/norm;
      if (ii==0) { dotmax = dot; idd=ii; }
      else {
         if (dot > dotmax) {
            dotmax = dot; idd=ii;
         }
      }
   }
   if (dotmax > 0) {
      inodenext = SO->FN->FirstNeighb[inode][idd];
      SUMA_LHv("Next node should be %d\n", inodenext);
   } else {
      SUMA_LH("No good direction");
      inodenext = -1; 
   }

   if (p) SUMA_free(p); p = NULL;
   if (q) SUMA_free(q); q = NULL;
   if (s) SUMA_free(s); s = NULL;
   
   SUMA_RETURN(inodenext);
}   

/*!
   Purpose: Takes a the world x,y,z coordinates and turns them into screen coordinates
   Set the last param to 0 (or NOPE) if you are calling this function after the projection 
   and other viewing matrices have been set. This happens when this function is called as
   a child of SUMA_display
   \sa SUMA_GetSelectionLine
*/
SUMA_Boolean SUMA_World2ScreenCoords (
                     SUMA_SurfaceViewer *sv, int N_List, double *WorldList, 
                     double *ScreenList, int *Quad, SUMA_Boolean ApplyXform)
{
   static char FuncName[]={"SUMA_World2ScreenCoords"};
   GLfloat rotationMatrix[4][4];
   GLint viewport[4];
   GLdouble mvmatrix[16], projmatrix[16];
   int i, i3;
   char CommString[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf (SUMA_STDERR, 
               "%s: Current Quat: %.4f, %.4f, %.4f, %.4f.\n", 
               FuncName, sv->GVS[sv->StdView].currentQuat[0],           
               sv->GVS[sv->StdView].currentQuat[1], 
               sv->GVS[sv->StdView].currentQuat[2],
               sv->GVS[sv->StdView].currentQuat[3]);
      fprintf (SUMA_STDERR, 
               "%s: Translation Vector of view #%d: %.4f, %.4f, %.4f\n", 
               FuncName, sv->StdView, sv->GVS[sv->StdView].translateVec[0], 
               sv->GVS[sv->StdView].translateVec[1], 
               sv->GVS[sv->StdView].translateVec[2]);
      fprintf (SUMA_STDERR, 
               "%s: RotaCenter of view #%d: %.4f, %.4f, %.4f\n", 
               FuncName, sv->StdView, sv->GVS[sv->StdView].RotaCenter[0], 
               sv->GVS[sv->StdView].RotaCenter[1], 
               sv->GVS[sv->StdView].RotaCenter[2]);
   }
      
   
   if (ApplyXform) {
      /* go through the ModelView transforms as you would 
         in display since the modelview matrix is popped
         after each display call */
      SUMA_build_rotmatrix(rotationMatrix, sv->GVS[sv->StdView].currentQuat);
      glMatrixMode(GL_MODELVIEW);
      /* The next line appears to fix some bug with GL_MODELVIEW's matrix. 
         When you clicked button3 for the first time in a viewer, 
         the chosen point was off. The next click in the identical position would          select the correct point and subsequent clicks are OK.
         None of the parameters used for the selection would change between the  
         first click and the next but it appears that going from one
         viewer to the next caused GL_MODELVIEW to change (sometimes) slightly.            Putting the line glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
         to check (and debug) what was happening to GL_MODELVIEW matrix between 
         one viewer and the next fixed the clicking problem. So, we keep
         it here as a fix until a better one comes along. PS: This was also the 
         source of the Z (blue) eye axis showing up when it should not. */  
         glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
         if (LocalHead) {
            int itmp = 0;
            fprintf (SUMA_STDERR, "%s: Initial Modelview:\nMV=[ ", FuncName);
            while (itmp < 16) { 
               fprintf (SUMA_STDERR, "%.4f, ", mvmatrix[itmp]); ++itmp;}
            fprintf (SUMA_STDERR, "]\n");
         }
      glPushMatrix();
      glTranslatef ( sv->GVS[sv->StdView].translateVec[0], 
                     sv->GVS[sv->StdView].translateVec[1], 0.0);
      glTranslatef ( sv->GVS[sv->StdView].RotaCenter[0], 
                     sv->GVS[sv->StdView].RotaCenter[1], 
                     sv->GVS[sv->StdView].RotaCenter[2]);
      glMultMatrixf(&rotationMatrix[0][0]);
      glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
         if (LocalHead) {
            int itmp = 0;
            fprintf (SUMA_STDERR, 
                     "%s: Modelview After Translation & Rotation:\nMVtr=[ ", 
                     FuncName);
            while (itmp < 16) { 
               fprintf (SUMA_STDERR, "%.4f, ", mvmatrix[itmp]); ++itmp;}
            fprintf (SUMA_STDERR, "]\n");
         }
      glTranslatef ( -sv->GVS[sv->StdView].RotaCenter[0], 
                     -sv->GVS[sv->StdView].RotaCenter[1], 
                     -sv->GVS[sv->StdView].RotaCenter[2]);
   } 
   glGetIntegerv(GL_VIEWPORT, viewport);
   glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
   glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);
   
   for (i=0;i<N_List; ++i) {
      i3 = 3*i;
      gluProject( (GLdouble)WorldList[i3], 
                  (GLdouble)WorldList[i3+1], (GLdouble)WorldList[i3+2],  
                  mvmatrix, projmatrix, viewport, 
                  (GLdouble*)(&(ScreenList[i3])), 
                  (GLdouble*)(&(ScreenList[i3+1])), 
                  (GLdouble*)(&(ScreenList[i3+2])) );
      ScreenList[i3+1] = viewport[3] - ScreenList[i3+1] - 1; /* change from 
                                                   OpenGL's y to screen's y */
      if (ScreenList[i3] < sv->WindWidth/2) {
         if (ScreenList[i3+1] > sv->WindHeight/2) 
            Quad[i] = SUMA_LOWER_LEFT_SCREEN;
         else Quad[i] = SUMA_UPPER_LEFT_SCREEN;
      } else {
         if (ScreenList[i3+1] > sv->WindHeight/2) 
            Quad[i] = SUMA_LOWER_RIGHT_SCREEN;
         else Quad[i] = SUMA_UPPER_RIGHT_SCREEN;
      }
      if (LocalHead) 
         fprintf (SUMA_STDOUT, 
                  "%s: World: [%.2f %.2f %.2f] \t "
                  "Screen [%.2f %.2f %.2f] \t Quad %d\n", 
                  FuncName, 
                  WorldList[i3],WorldList[i3+1], WorldList[i3+2], 
                  ScreenList[i3], ScreenList[i3+1],ScreenList[i3+2], Quad[i]);
   
   }

   if (ApplyXform) glPopMatrix();

   SUMA_RETURN (YUP);
}

/* Take normalized x,y screen corrdinates and turn them to world coordinates 
Based on code from SUMA_GetSelectionLine 
ASSUMES GL_MODELVIEW_MATRIX and GL_PROJECTION_MATRIX are current! */
SUMA_Boolean SUMA_NormScreenToWorld(SUMA_SurfaceViewer *sv, 
                                    double xn, double yn, 
                                    GLdouble *pfront, GLdouble *pback)
{
   static char FuncName[]={"SUMA_NormScreenToWorld"};
   GLdouble ox, oy;
   GLint viewport[4];
   GLdouble mvmatrix[16], projmatrix[16];
   
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
      
   glGetIntegerv(GL_VIEWPORT, viewport);
   glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
   glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);
   
   /* get OpenGL screen coords */
   ox = xn*(double)viewport[2];
   oy = yn*(double)viewport[3];
   
   /* unproject to world */
   if (pfront) {
      gluUnProject(  ox, oy, 0.0,
                     mvmatrix, projmatrix, viewport, 
                     pfront, pfront+1, pfront+2);
   }
   if (pback) {
      gluUnProject(  ox, oy, 1.0,
                     mvmatrix, projmatrix, viewport, 
                     pback, pback+1, pback+2);
   }
   
   SUMA_LHv("Normalized screen coords: [%.4f %.4f]\n"
            "OpenGl screen coords: [%.4f %.4f]\n"
            "pfront (%p): [%.4f %.4f %.4f]\n"
            "pback  (%p): [%.4f %.4f %.4f]\n",
            xn, yn, ox, oy,
            pfront, pfront ? pfront[0] : 0 , 
                    pfront ? pfront[1] : 0 , 
                    pfront ? pfront[2] : 0 ,
            pback,  pback  ? pback[0] : 0 , 
                    pback  ? pback[1] : 0 , 
                    pback  ? pback[2] : 0 );
      
   SUMA_RETURN(YUP);
}
/*!
   Purpose: Takes a the x,y positions of the cursor and sets 
            the Pick0 and Pick1 values (usually sv's) 
   \param sv (*SUMA_SurfaceViewer)
   \param x (int) mouse coordinate
   \param y (int)
   \param Pick0 (GLdouble *) vector of 3 elements (usually, pass sv->Pick0)
   \param Pick1 (GLdouble *) vector of 3 elements (usually, pass sv->Pick1)
   \param N_List (int) if > 0, it indicates that there are N_list 
                       other mouse coordinates to consider 
                       in addition to the y and y above.
   \param xList (int *) a vector of N_list x values
   \param yList (int *) a vector of N_list y values
   \param PickList (Gldouble *) a N_list x 3 vector containing the equivalent 
                                of Pick0 for the values in xList and yList
   \return YUP/NOPE
   \sa SUMA_input, button3 pick
   \sa SUMA_NormScreenToWorld
*/
SUMA_Boolean SUMA_GetSelectionLine (SUMA_SurfaceViewer *sv, int x, int y, 
                                    GLdouble *Pick0, GLdouble *Pick1, 
                                    int N_List, int *xList, int *yList, 
                                    GLdouble *Pick0List)
{
   static char FuncName[]={"SUMA_GetSelectionLine"};
   GLfloat rotationMatrix[4][4];
   GLint viewport[4];
   GLdouble mvmatrix[16], projmatrix[16];
   GLint realy; /* OpenGL y coordinate position */
   char CommString[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   
   
   if (LocalHead) {
      fprintf (SUMA_STDERR, 
               "%s: Selection x, y=[%d, %d]\n",
               FuncName, x, y);                 
      fprintf (SUMA_STDERR, 
               "%s: Current Quat: %.4f, %.4f, %.4f, %.4f.\n", 
                FuncName, 
                sv->GVS[sv->StdView].currentQuat[0], 
                sv->GVS[sv->StdView].currentQuat[1],  
                sv->GVS[sv->StdView].currentQuat[2],
                sv->GVS[sv->StdView].currentQuat[3]);
      fprintf (SUMA_STDERR, 
               "%s: Translation Vector of view #%d: %.4f, %.4f, %.4f\n",
               FuncName, sv->StdView, sv->GVS[sv->StdView].translateVec[0], 
               sv->GVS[sv->StdView].translateVec[1], 
               sv->GVS[sv->StdView].translateVec[2]);
      fprintf (SUMA_STDERR, 
               "%s: RotaCenter of view #%d: %.4f, %.4f, %.4f\n", 
               FuncName, sv->StdView, sv->GVS[sv->StdView].RotaCenter[0], 
               sv->GVS[sv->StdView].RotaCenter[1], 
               sv->GVS[sv->StdView].RotaCenter[2]);
   }
      
   
   /* go through the ModelView transforms as you would in 
      display since the modelview matrix is popped
      after each display call */
   SUMA_build_rotmatrix(rotationMatrix, sv->GVS[sv->StdView].currentQuat);
   glMatrixMode(GL_MODELVIEW);
   /* The next line appears to fix some bug with GL_MODELVIEW's matrix. 
      When you clicked button3 for the first time in a viewer, 
      the chosen point was off. The next click in the identical position would 
      select the correct point and subsequent clicks are OK.
      None of the parameters used for the selection would change between the
      first click and the next but it appears that going from one
      viewer to the next caused GL_MODELVIEW to change (sometimes) slightly.  
      Putting the line glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
      to check (and debug) what was happening to GL_MODELVIEW matrix between one 
      viewer and the next fixed the clicking problem. So, we keep
      it here as a fix until a better one comes along. 
      PS: This was also the source of the Z (blue) eye axis showing up when 
      it should have not. */  
      glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
      if (LocalHead) {
         int itmp = 0;
         fprintf (SUMA_STDERR, 
                  "%s: Initial Modelview:\nMV=[ ", FuncName);
         while (itmp < 16) { 
            fprintf (SUMA_STDERR, "%.4f, ", mvmatrix[itmp]); ++itmp;}
         fprintf (SUMA_STDERR, "]\n");
      }
   glPushMatrix();
   glTranslatef ( sv->GVS[sv->StdView].translateVec[0], 
                  sv->GVS[sv->StdView].translateVec[1], 0.0);
   glTranslatef ( sv->GVS[sv->StdView].RotaCenter[0], 
                  sv->GVS[sv->StdView].RotaCenter[1], 
                  sv->GVS[sv->StdView].RotaCenter[2]);
   glMultMatrixf(&rotationMatrix[0][0]);
      glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
      if (LocalHead) {
         int itmp = 0;
         fprintf (SUMA_STDERR, 
                  "%s: Modelview After Translation & Rotation:\nMVtr=[ ", 
                  FuncName);
         while (itmp < 16) { 
            fprintf (SUMA_STDERR, "%.4f, ", mvmatrix[itmp]); ++itmp;}
         fprintf (SUMA_STDERR, "]\n");
      }
   glTranslatef ( -sv->GVS[sv->StdView].RotaCenter[0], 
                  -sv->GVS[sv->StdView].RotaCenter[1], 
                  -sv->GVS[sv->StdView].RotaCenter[2]);

   glGetIntegerv(GL_VIEWPORT, viewport);
   glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
   glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);
   /* viewport[3] is height of window in pixels */
   realy = viewport[3] - (GLint)y -1;

   if (LocalHead) 
      fprintf (SUMA_STDOUT, 
               "%s: Coordinates at cursor are (%4d, %4d)\n"
               "Viewport is %d %d %d %d\n"
               "Normalized coords: (%.3f %.3f)\n", 
               FuncName, x, realy, 
               viewport[0], viewport[1], viewport[2], viewport[3],
               (double)x/(double)viewport[2], (double)y/viewport[3]);

   /* set the pick points at both ends of the clip planes */
   if (Pick0) {
      gluUnProject(  (GLdouble)x, (GLdouble)realy, 0.0,
                     mvmatrix, projmatrix, viewport, 
                     &(Pick0[0]), &(Pick0[1]), &(Pick0[2]));
      if (LocalHead) 
         fprintf (SUMA_STDOUT, 
                  "World Coords at z=0.0 (near clip plane) are (%f, %f, %f)\n",
                 (Pick0[0]), (Pick0[1]), (Pick0[2]));
   }
   if (Pick1) {
      gluUnProject(  (GLdouble)x, (GLdouble)realy, 1.0,
                     mvmatrix, projmatrix, viewport, 
                     &(Pick1[0]), &(Pick1[1]), &(Pick1[2]));
      if (LocalHead) 
         fprintf (SUMA_STDOUT, 
                  "World Coords at z=1.0 (far clip plane) are (%f, %f, %f)\n",
                  (Pick1[0]), (Pick1[1]), (Pick1[2]));
   }

   if (N_List > 0) {
      SUMA_LH("Doing the list thing");
      if (!Pick0List || !xList || !yList) { 
         SUMA_S_Err( "Null Pick0List or xlist or ylist with non 0 N_List.\n"
                     "PickList ignored."); }
      else {
         int i, i3;
         for (i=0; i<N_List; ++i) {
            i3 = 3*i;
            realy = viewport[3] - (GLint)yList[i] -1;
            gluUnProject((GLdouble)xList[i], (GLdouble)realy, 0.0,
                           mvmatrix, projmatrix, viewport, 
                           &(Pick0List[i3+0]), &(Pick0List[i3+1]), 
                           &(Pick0List[i3+2]));
         }
      }  
   }
   glPopMatrix();

   SUMA_RETURN (YUP);
}

/*!
   \brief Draws a line between screen (window) coordinates 
*/
SUMA_Boolean SUMA_DrawWindowLine(SUMA_SurfaceViewer *sv, 
                                 int x0, int y0, 
                                 int x1, int y1, int meth)
{
   static char FuncName[]={"SUMA_DrawWindowLine"};
   GLfloat rotationMatrix[4][4];
   static GLfloat LineCol[]={ SUMA_RED_GL };
   static int xlist[2], ylist[2];
   GLdouble Pick0[3], Pick1[3], PickList[6];
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};   
   SUMA_Boolean LocalHead  = NOPE;
   SUMA_ENTRY;

   switch (meth) {
      case 0: /* does not work on OSX */
         XDrawLine (sv->X->DPY, XtWindow(sv->X->GLXAREA), sv->X->gc, 
           (int)x0, (int)y0,
           (int)x1, (int)y1);
         break;
      case 1:
         SUMA_build_rotmatrix(rotationMatrix, sv->GVS[sv->StdView].currentQuat);
         xlist[0] = x0; xlist[1] = x1;
         ylist[0] = y0; ylist[1] = y1;
         SUMA_GetSelectionLine ( sv, x0, y0, 
                                 NULL, NULL, 
                                 2, 
                                 xlist, ylist, 
                                 PickList);
         SUMA_SET_GL_PROJECTION(sv, sv->ortho);
         SUMA_SET_GL_MODELVIEW(sv);
         glMaterialfv(GL_FRONT, GL_EMISSION, LineCol);
         glLineWidth(SUMA_CROSS_HAIR_LINE_WIDTH);
         if (LocalHead) {
            fprintf( SUMA_STDERR,
                     "%s:PickList\n"
                     "[%.3f %.3f %.3f\n %.3f %.3f %.3f]\n", 
                     FuncName,
                     PickList[0],PickList[1],PickList[2],
                     PickList[3], PickList[4],PickList[5] );
         }
         glBegin(GL_LINES);
         glVertex3d(PickList[0], PickList[1], PickList[2]-0.001); 
                  /* something to do with clipping ...*/
         glVertex3d(PickList[3], PickList[4], PickList[5]-0.001);
         glVertex3d(PickList[0], PickList[1], PickList[2]+0.001); 
                  /* something to do with clipping ...*/
         glVertex3d(PickList[3], PickList[4], PickList[5]+0.001);
         glEnd();
         glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);
         glPopMatrix();   
         if (sv->X->DOUBLEBUFFER)
             glXSwapBuffers(sv->X->DPY, XtWindow(sv->X->GLXAREA));
          else
            glFlush();         
         break;
      default:
         break;
   }

   SUMA_RETURN(YUP);
}


/*!
   \brief A call back to open the help window 
   No input parameters needed
*/
void SUMA_cb_helpUsage (Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpUsage"};
   DList *list = NULL;
   
   SUMA_ENTRY;
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Help, SES_Suma, NULL); 
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
   SUMA_RETURNe;

}

/*!
   \brief A call back to open the Message Log window 
   No input parameters needed
*/
void SUMA_cb_helpMessageLog (Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpMessageLog"};
   DList *list = NULL;
   
   SUMA_ENTRY;
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Log, SES_Suma, NULL); 
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
   SUMA_RETURNe;

}

/*!
   \brief A call back to open the viewer Info window 
   Exepcts the index of the viewer in SUMAg_SVv in data->ContID
*/
void SUMA_cb_helpViewerStruct (Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpViewerStruct"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceViewer *sv = NULL;
   
   SUMA_ENTRY;
   
   datap = (SUMA_MenuCallBackData *)data;
   sv = &(SUMAg_SVv[(int)datap->ContID]);
   
   if (!sv->X->ViewCont->TopLevelShell) { /* see comments on similar section in SUMA_cb_helpSurfaceStruct */
      SUMA_cb_createViewerCont( w, (XtPointer)sv, callData);
      SUMA_cb_closeViewerCont ( w, (XtPointer)sv, callData); 
   }
   /* Now do the info thingy */
   SUMA_cb_moreViewerInfo (w, (XtPointer)sv, callData);

   
   SUMA_RETURNe;

}

/*!
   \brief A call back to open the surface Info window 
   Exepcts the index of the viewer in SUMAg_SVv in data->ContID
*/
void SUMA_cb_helpSurfaceStruct (Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpSurfaceStruct"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceViewer *sv = NULL;
   SUMA_SurfaceObject *SO = NULL;
   
   SUMA_ENTRY;
   
   datap = (SUMA_MenuCallBackData *)data;
   sv = &(SUMAg_SVv[(int)datap->ContID]);
   if (sv->Focus_SO_ID >= 0) {
      SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   }else {
      SUMA_SLP_Err("No surface object in focus.\n");
      SUMA_RETURNe;
   }

   if (!SO->SurfCont->TopLevelShell) {
      /* Before you open the surface info widget, you'll need to open
      the surface controller to initialize the Surface Controller first */
      SUMA_cb_createSurfaceCont( w, (XtPointer)SO, callData);
      /* NOW CLOSE IT, user need not use it.
      Could have been a bit more elegant here
      but that's good enough*/
      SUMA_cb_closeSurfaceCont ( w, (XtPointer)SO, callData); 
   }
   
   /* Now do the info thingy */
   SUMA_cb_moreSurfInfo (w, (XtPointer)SO->SurfCont->curSOp, callData);

   SUMA_RETURNe;

}

void SUMA_cb_helpSUMAGlobal (Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpSUMAGlobal"};
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->X->SumaCont->AppShell) { /* create */
      SUMA_cb_createSumaCont( w, data, callData);
      SUMA_cb_closeSumaCont ( w, data, callData);
   }
   
   /* Now open the info thingy */
   SUMA_cb_moreSumaInfo (w, data, callData);
   
   SUMA_RETURNe;
}

/*!
 function to toggle the IOnotify debugging flag
 - expects nothing
*/  
void SUMA_cb_helpIO_notify(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpIO_notify"};
   int ii;
   
   SUMA_ENTRY;
   
   SUMA_INOUT_NOTIFY_TOGGLE;
   
   /* must update the state of toggle buttons in otherviewers */
   for (ii=0; ii<SUMAg_N_SVv; ++ii) {
      if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
         /* you must check for both conditions because by default 
         all viewers are initialized to isShaded = NOPE, even before they are ever opened */
         if (w != SUMAg_SVv[ii].X->HelpMenu[SW_HelpIONotify]) {
            XmToggleButtonSetState (SUMAg_SVv[ii].X->HelpMenu[SW_HelpIONotify], 
               SUMAg_CF->InOut_Notify, NOPE);
         }
      }
   }
   
    
   SUMA_RETURNe; 
}

/*!
 function to toggle the Memtrace debugging flag
 - expects nothing
*/  
void SUMA_cb_helpMemTrace(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpIO_notify"};
   int ii;
   
   SUMA_ENTRY;
   
   SUMA_MEMTRACE_TOGGLE;
   
   /* must update the state of toggle buttons in otherviewers */
   for (ii=0; ii<SUMAg_N_SVv; ++ii) {
      if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
         /* you must check for both conditions because by default 
         all viewers are initialized to isShaded = NOPE, even before they are ever opened */
         XmToggleButtonSetState (SUMAg_SVv[ii].X->HelpMenu[SW_HelpMemTrace], 
            SUMAg_CF->MemTrace, NOPE);
         if (SUMAg_CF->MemTrace) {
            /* can't turn it off */
             XtSetSensitive (SUMAg_SVv[ii].X->HelpMenu[SW_HelpMemTrace], 0);
         }
      }
   }
   
   SUMA_RETURNe; 
}

/*!
   \brief callback to open SUMA 's Controller
   No input parameters needed
*/
void SUMA_cb_viewSumaCont(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_viewSumaCont"};
   Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->X->SumaCont->AppShell) { /* create */
      if (LocalHead) fprintf (SUMA_STDERR,"%s: creating controller \n", FuncName);
      SUMA_cb_createSumaCont( w, data, callData);
   }else {
      /* controller already created, need to bring it up again */
      #ifdef SUMA_USE_WITHDRAW
         if (LocalHead) fprintf (SUMA_STDERR,"%s: raising SUMA controller \n", FuncName);
         XMapRaised(SUMAg_CF->X->DPY_controller1, XtWindow(SUMAg_CF->X->SumaCont->AppShell));
      #endif
   }

   SUMA_RETURNe;
}

/*! if calling this function from outside interface, set w to NULL 
*/
int SUMA_viewSurfaceCont(Widget w, SUMA_SurfaceObject *SO, SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_viewSurfaceCont"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->SurfCont) {
      SUMA_RETURN(0);
   }
   
   if (!SO->SurfCont->TopLevelShell) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_cb_createSurfaceCont.\n", FuncName);
      if (w) SUMA_cb_createSurfaceCont( w, (XtPointer)SO, NULL);
      else SUMA_cb_createSurfaceCont( sv->X->TOPLEVEL, (XtPointer)SO, NULL);
   }else {
      /* controller already created, need to bring it up again */
      #ifdef SUMA_USE_WITHDRAW
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Controller already created, Raising it.\n", FuncName);
         XMapRaised(SUMAg_CF->X->DPY_controller1, XtWindow(SO->SurfCont->TopLevelShell));      
      #endif

   }
   
   SUMA_Init_SurfCont_SurfParam(SO);
   SUMA_Init_SurfCont_CrossHair(SO);
   
   if (SO->SurfCont->PosRef != sv->X->TOPLEVEL) {
      SO->SurfCont->PosRef = sv->X->TOPLEVEL;
      SUMA_PositionWindowRelative (SO->SurfCont->TopLevelShell, SO->SurfCont->PosRef, SWP_TOP_RIGHT);   
   }
   
   SUMA_RETURN(1); 
}

/*!
   \brief SUMA_cb_viewSurfaceCont(Widget w, XtPointer data, XtPointer callData);
   opens the surface controller for the surface in focus. 
   \param data (XtPointer) index of widget into sv->X->ViewMenu  
   It is the controller for the surface in focus that will be open.
   
*/ 
void SUMA_cb_viewSurfaceCont(Widget w, XtPointer data, XtPointer callData)
{
   SUMA_SurfaceObject *SO;
   SUMA_SurfaceViewer *sv;
   int isv, widtype;
   static char FuncName[] = {"SUMA_cb_viewSurfaceCont"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_VIEWER_FROM_VIEWMENU_CALLBACK (data, isv, widtype);
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: A call from viewer %d, widget %d.\n", FuncName, isv, widtype);
   
   sv = &SUMAg_SVv[isv];
   if (sv->Focus_SO_ID >= 0) {
    SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   }else {
      fprintf (SUMA_STDERR,"%s: No surface object in focus.\n", FuncName);
      SUMA_RETURNe;
   }
   
   if (!SUMA_viewSurfaceCont(w, SO, sv)) {
      SUMA_S_Err("Failed in SUMA_viewSurfaceCont");
      SUMA_RETURNe;
   }

   SUMA_RETURNe;
}

/*! \brief SUMA_cb_viewViewerCont(Widget w, XtPointer data, XtPointer callData)
      opens the viewer controller. 
      \param data index of widget into sv->X->ViewMenu 
*/
void SUMA_cb_viewViewerCont(Widget w, XtPointer data, XtPointer callData)
{
   int isv, widtype;
   SUMA_SurfaceViewer *sv;
   SUMA_Boolean LocalHead = NOPE;
   static char FuncName[] = {"SUMA_cb_viewViewerCont"};
   
   SUMA_ENTRY;
   
   SUMA_VIEWER_FROM_VIEWMENU_CALLBACK (data, isv, widtype);
   
   sv = &SUMAg_SVv[isv];

   if (!sv->X->ViewCont->TopLevelShell) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_cb_createViewerCont.\n", FuncName);
      SUMA_cb_createViewerCont( w, sv, callData);
   }else {
      /* controller already created, need to bring it up again */
      
      #ifdef SUMA_USE_WITHDRAW
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Controller already created, Raising it.\n", FuncName);
         XMapRaised(sv->X->DPY, XtWindow(sv->X->ViewCont->TopLevelShell));      
      #endif

   }
   
   SUMA_RETURNe;
}



/*!<
 the function expects the index of widget into sv->X->ViewMenu in data */
void SUMA_cb_toggle_crosshair(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_toggle_crosshair"};
   int isv, widtype;
   DList *list = NULL;
   SUMA_SurfaceViewer *sv;
      
   SUMA_ENTRY;
   
   SUMA_VIEWER_FROM_VIEWMENU_CALLBACK (data, isv, widtype);
   
   sv = &SUMAg_SVv[isv];
      
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleCrossHair, SES_SumaWidget, sv);
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_SumaWidget, sv);

   if (!SUMA_Engine (&list)) {
      fprintf(stderr,"Error %s: Failed SUMA_Engine\n", FuncName);
   }
   
   SUMA_RETURNe;
}
 
void SUMA_cb_toggle_node_in_focus(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_toggle_node_in_focus"};
   int isv, widtype;
   DList *list = NULL;
   SUMA_SurfaceViewer *sv;
   
   SUMA_ENTRY;
   
   SUMA_VIEWER_FROM_VIEWMENU_CALLBACK (data, isv, widtype);
   
   sv = &SUMAg_SVv[isv];
      
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleShowSelectedNode, SES_SumaWidget, sv);
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_SumaWidget, sv);

   if (!SUMA_Engine (&list)) {
      fprintf(stderr,"Error %s: Failed SUMA_Engine\n", FuncName);
   }

   SUMA_RETURNe;
}

void SUMA_cb_toggle_selected_faceset(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_toggle_selected_faceset"};
   int isv, widtype;
   DList *list = NULL;
   SUMA_SurfaceViewer *sv;
   
   SUMA_ENTRY;
   
   SUMA_VIEWER_FROM_VIEWMENU_CALLBACK (data, isv, widtype);
   
   sv = &SUMAg_SVv[isv];
      
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleShowSelectedFaceSet, SES_SumaWidget, sv);
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_SumaWidget, sv);

   if (!SUMA_Engine (&list)) {
      fprintf(stderr,"Error %s: Failed SUMA_Engine\n", FuncName);
   }

   
   SUMA_RETURNe;
}         

/*! Creates the dialog shell of the viewer controller.
*/
#define SUMA_CONTROLLER_AS_DIALOG 0 /* controller widgets as dialog (1) or toplevelshells (0) 
                                    Stick with toplevelshells or window managers might force 
                                    you to keep them atop the surface viewers. Downside is that
                                    it is managed such that if the viewer is minimized, the controller is not.
                                    But that is not necessarily a bad thing. */
/*!
\brief Creates the viewer controller, expects sv in data 
*/
void SUMA_cb_createViewerCont(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createViewerCont"};
   Widget   tl, rc, pb, ViewerFrame, SwitchFrame, 
            QuitFrame, rc_left, rc_right, rc_mamma;
   Display *dpy;
   SUMA_SurfaceViewer *sv;
   int isv;    
   char slabel[100]; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   sv = (SUMA_SurfaceViewer *)data;
   isv = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
   
   if (sv->X->ViewCont->TopLevelShell) {
      fprintf (SUMA_STDERR,
               "Error %s: sv->X->ViewCont->TopLevelShell!=NULL. \n"
               "Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   tl = SUMA_GetTopShell(w); /* top level widget */
   dpy = XtDisplay(tl);
   
   sprintf(slabel,"[%c] Viewer Controller", 65+isv);
   
   
   #if SUMA_CONTROLLER_AS_DIALOG 
      /*xmDialogShellWidgetClass, topLevelShellWidgetClass*/
   SUMA_LH("Create a popup");
   sv->X->ViewCont->TopLevelShell = XtVaCreatePopupShell (slabel,
      xmDialogShellWidgetClass, tl,
      XmNallowShellResize, True, /* let code resize shell */
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);    
   #else
   SUMA_LH("Create an App");
   /** Feb 03/03: I was using XtVaCreatePopupShell to create a 
      topLevelShellWidgetClass. 
      XtVaCreatePopupShell is used to create dialog shells not 
      toplevel or appshells */
   sv->X->ViewCont->TopLevelShell = XtVaAppCreateShell (slabel, "Suma",
      topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);   
   #endif
   
   /* allow for code to resize the shell */
   XtVaSetValues (sv->X->ViewCont->TopLevelShell, 
         XmNresizePolicy , XmRESIZE_NONE , /* allow (?) childrent to resize */
         XmNallowShellResize , True ,       /* let code resize shell */
         NULL);
   
   /* handle the close button from window manager */
   XmAddWMProtocolCallback(/* make "Close" window menu work */
      sv->X->ViewCont->TopLevelShell,
      XmInternAtom( dpy , "WM_DELETE_WINDOW" , False ) ,
      SUMA_cb_closeViewerCont, (XtPointer) sv) ;
   
   /* create a form widget, manage it at the end ...*/
   sv->X->ViewCont->Mainform = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, sv->X->ViewCont->TopLevelShell,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   rc_mamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, sv->X->ViewCont->Mainform,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            XmNleftAttachment , XmATTACH_FORM ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNrightAttachment , XmATTACH_FORM ,
            NULL);
            
   rc_left = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_mamma,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);
   
   rc_right = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_mamma,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL); 
   
   {/*s group, state and info */ 
      Widget rc, pb, label;
      
      /* put a frame */
      ViewerFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
         
      /* row column Lock rowcolumns */
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, ViewerFrame,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);

      /*put a label containing the surface name, 
         number of nodes and number of facesets */
      snprintf(slabel, 40*sizeof(char), 
               "Group: %s, State: %s", sv->CurGroupName, sv->State);
      sv->X->ViewCont->Info_lb = XtVaCreateManagedWidget (slabel, 
               xmLabelWidgetClass, rc,
               NULL);
               
      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, rc, 
                                 XmNorientation, XmVERTICAL,NULL);

      sv->X->ViewCont->ViewerInfo_pb = XtVaCreateWidget ("more", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (sv->X->ViewCont->ViewerInfo_pb, XmNactivateCallback, 
                     SUMA_cb_moreViewerInfo, (XtPointer) sv);
      XtVaSetValues (sv->X->ViewCont->ViewerInfo_pb, 
                     XmNuserData, (XtPointer)sv, NULL); 
                     /* store sv in userData
                     I think it is more convenient than as data
                     in the call back structure. This way it will
                     be easy to change the sv that this same button
                     might refer to. 
                     This is only for testing purposes, the pb_close
                     button still expects sv in clientData
                     Feb 23 04: UserData works well, but other 
                     functions don't use it much so also store sv in clientData*/
      MCW_register_hint( sv->X->ViewCont->ViewerInfo_pb , 
                           "More info on Viewer" ) ;
      MCW_register_help( sv->X->ViewCont->ViewerInfo_pb , 
                           SUMA_moreViewerInfo_help ) ;
      XtManageChild (sv->X->ViewCont->ViewerInfo_pb); 

      XtManageChild (rc);
      
      XtManageChild (ViewerFrame);
   
   }
   { /* switch group and state frames */
      Widget rc, pb, label;/* put a frame */
      
      SwitchFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
         
      /* row column for switching groups and state */
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SwitchFrame,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);

      /*put a label for Switch */
      snprintf(slabel, 40*sizeof(char), "Switch:");
      label = XtVaCreateManagedWidget (slabel, 
               xmLabelWidgetClass, rc,
               NULL);

      
      /* put a button for swiching groups */
      snprintf(slabel, 40*sizeof(char), "[%c] Switch Group", 65 + isv);
      sv->X->ViewCont->SwitchGrouplst = 
            SUMA_AllocateScrolledList (slabel, SUMA_LSP_SINGLE, 
                              NOPE, YUP,
                              sv->X->ViewCont->TopLevelShell, SWP_TOP_LEFT,
                              125,
                              SUMA_cb_SelectSwitchGroup, (void *)sv,
                              SUMA_cb_SelectSwitchGroup, (void *)sv,
                              SUMA_cb_CloseSwitchGroup, (void *)sv);
    

      pb = XtVaCreateWidget ("Group", 
         xmPushButtonWidgetClass, rc, 
         NULL);
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ViewerCont_SwitchGroup, (XtPointer) sv);
      MCW_register_hint( pb , "Switch Group" ) ;
      MCW_register_help( pb , "Switch Group" ) ;
      XtManageChild (pb);

      /* put a button for swiching states */
      pb = XtVaCreateWidget ("State", 
         xmPushButtonWidgetClass, rc, 
         NULL);
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ViewerCont_SwitchState, (XtPointer) sv);
      MCW_register_hint( pb , "Switch State" ) ;
      MCW_register_help( pb , "Switch State" ) ;
      XtManageChild (pb);
      
      
      /* now start managing the row column widget */
      XtManageChild (rc);

      /* manage the frame and the fslabelorm */
      XtManageChild (SwitchFrame);
   }               
   
   { /*s close and help buttons */
      Widget rc, pb_close, pb_bhelp;
      
      /* put up a frame to group the display controls */
      QuitFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, sv->X->ViewCont->Mainform,
         XmNleftAttachment , XmATTACH_FORM ,
         XmNtopAttachment  , XmATTACH_WIDGET ,
         XmNtopWidget, SwitchFrame,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 

         
      #if 0
      /* ugly, useless */
      /* this one requires Motif 1.2 or newer */
      XtVaCreateManagedWidget ("Disp. Cont.",
            xmLabelWidgetClass, QuitFrame, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      #endif
      
      /* row column Lock rowcolumns */
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, QuitFrame,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);

      pb_close = XtVaCreateWidget ("Close", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb_close, XmNactivateCallback, 
                     SUMA_cb_closeViewerCont, (XtPointer) sv);
      MCW_register_hint( pb_close , "Close Viewer controller" ) ;
      MCW_register_help( pb_close , SUMA_closeViewerCont_help ) ;
      XtManageChild (pb_close); 

      pb_bhelp = XtVaCreateWidget ("BHelp", 
         xmPushButtonWidgetClass, rc, 
         NULL);
      XtAddCallback (pb_bhelp, XmNactivateCallback, MCW_click_help_CB, NULL);
      MCW_register_help(pb_bhelp , SUMA_help_help ) ;
      MCW_register_hint(pb_bhelp  , 
                        "Press this button then click on a "
                        "button/label/menu for more help." ) ;

      XtManageChild (pb_bhelp); 



      /* now start managing the row column widget */
      XtManageChild (rc);

      /* manage the frame and the fslabelorm */
      XtManageChild (QuitFrame);
   }
      
   XtManageChild (rc_right);
   XtManageChild (rc_left);
   XtManageChild (rc_mamma);
   XtManageChild (sv->X->ViewCont->Mainform);
   
   #if SUMA_CONTROLLER_AS_DIALOG    
   #else
   /** Feb 03/03: pop it up if it is a topLevelShellWidgetClass, 
   you should do the popping after all the widgets have been created.
   Otherwise, the window does not size itself correctly when open */
   XtPopup(sv->X->ViewCont->TopLevelShell, XtGrabNone);
   #endif
   
   /* realize the widget */
   XtRealizeWidget (sv->X->ViewCont->TopLevelShell);
   
   SUMA_RETURNe;
}

/*!
   \brief Closes a viewer controller, expects sv in data
*/
void SUMA_cb_closeViewerCont(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_closeViewerCont"};
   SUMA_SurfaceViewer *sv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   sv = (SUMA_SurfaceViewer *)data;
   
   if (!sv->X->ViewCont->TopLevelShell) SUMA_RETURNe;

   #ifdef SUMA_USE_WITHDRAW 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing Viewer Controller...\n", FuncName);
      
      XWithdrawWindow(sv->X->DPY, 
         XtWindow(sv->X->ViewCont->TopLevelShell),
         XScreenNumberOfScreen(XtScreen(sv->X->ViewCont->TopLevelShell)));
   #endif
   #ifdef SUMA_USE_DESTROY 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying Viewer Controller...\n", FuncName);
      XtDestroyWidget(sv->X->ViewCont->TopLevelShell);
      sv->X->ViewCont->TopLevelShell = NULL;
   #endif

    
   SUMA_RETURNe;

}

/*!
   \brief SUMA_cb_createSurfaceCont(Widget w, XtPointer data, XtPointer callData);
   \param data (XtPointer) to SO (NOT sv)

*/
void SUMA_cb_createSurfaceCont(Widget w, XtPointer data, XtPointer callData)
{
   Widget tl, pb, form, DispFrame, SurfFrame, rc_left, rc_right, rc_mamma;
   Display *dpy;
   SUMA_SurfaceObject *SO;
   char *slabel, *lbl30, *sss=NULL;
   XmString xmstmp; 
   SUMA_Boolean LocalHead = NOPE;
   static char FuncName[] = {"SUMA_cb_createSurfaceCont"};
   
   SUMA_ENTRY;
   
   SO = (SUMA_SurfaceObject *)data;
   *(SO->SurfCont->curSOp) = (void *)SO;
   
   if (SO->SurfCont->TopLevelShell) {
      fprintf (SUMA_STDERR,
               "Error %s: SO->SurfCont->TopLevelShell!=NULL.\n"
               "Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   tl = SUMA_GetTopShell(w); /* top level widget */
   dpy = XtDisplay(tl);
   
   slabel = (char *)SUMA_malloc (sizeof(char) * (strlen(SO->Label) + 100));
   if (strlen(SO->Label) > 40) {
      char *tmpstr=NULL;
      tmpstr = SUMA_truncate_string(SO->Label, 40);
      if (tmpstr) { 
         sprintf(slabel,"[%s] Surface Controller", tmpstr);
         free(tmpstr); tmpstr=NULL;
      }
   } else {
      sprintf(slabel,"[%s] Surface Controller", SO->Label);
   }
   
   /* March 12 08: Made font8 default for surface controller */
   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }

   #if SUMA_CONTROLLER_AS_DIALOG /* xmDialogShellWidgetClass, 
                                    topLevelShellWidgetClass*/
   if (LocalHead) 
      fprintf(SUMA_STDERR, "%s: Creating dialog shell.\n", FuncName);
   
   SO->SurfCont->TopLevelShell = XtVaCreatePopupShell (sss,
      XmNtitle, slabel,
      xmDialogShellWidgetClass, tl,
      XmNallowShellResize, True, /* let code resize shell */
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);    
   #else
   if (LocalHead) 
      fprintf(SUMA_STDERR, "%s: Creating toplevel shell.\n", FuncName);
   /** Feb 03/03:    I was using XtVaCreatePopupShell to create a 
                     topLevelShellWidgetClass. 
                     XtVaCreatePopupShell is used to create dialog 
                     shells not toplevel or appshells. 
                     Of course, it made no difference! */
   SO->SurfCont->TopLevelShell = XtVaAppCreateShell (sss, "Suma",
      topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
      XmNtitle, slabel,
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);   
   #endif
   
   /* allow for code to resize the shell */
   XtVaSetValues (SO->SurfCont->TopLevelShell, 
         XmNresizePolicy , XmRESIZE_NONE , 
         XmNallowShellResize , True ,       /* let code resize shell */
         NULL);
    
   /* handle the close button from window manager */
   XmAddWMProtocolCallback(/* make "Close" window menu work */
      SO->SurfCont->TopLevelShell,
      XmInternAtom( dpy , "WM_DELETE_WINDOW" , False ) ,
      SUMA_cb_closeSurfaceCont, (XtPointer) SO) ;
   
   /* create a form widget, manage it at the end ...*/
   SO->SurfCont->Mainform = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SO->SurfCont->TopLevelShell,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   rc_mamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->Mainform,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            XmNleftAttachment , XmATTACH_FORM ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNrightAttachment , XmATTACH_FORM ,
            NULL);
            
   rc_left = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_mamma,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
/*            XmNwidth, 317,
            XmNresizeWidth, False, */
            NULL);
   
   rc_right = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_mamma,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL); 
                    
   
   {/*surface properties */ 
      Widget rc, label, rc_SurfProp, pb;
     
      /* put a frame */
      SurfFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      XtVaCreateManagedWidget ("Surface Properties",
            xmLabelWidgetClass, SurfFrame, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      
      rc_SurfProp = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfFrame,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL); 
      
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_SurfProp,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /*put a label containing the surface name, number of nodes 
      and number of facesets */
      lbl30 = SUMA_set_string_length(SO->Label, ' ', 27);
      if (lbl30) {
         sprintf( slabel,"%s\n%d nodes: %d tri.", 
                  lbl30, SO->N_Node, SO->N_FaceSet); 
         SUMA_free(lbl30); lbl30 = NULL;
      } else {
         sprintf(slabel,"???\n%d nodes: %d tri.", SO->N_Node, SO->N_FaceSet); 
      }
      xmstmp = XmStringCreateLtoR (slabel, XmSTRING_DEFAULT_CHARSET);
         /* XmStringCreateLocalized(slabel) does not reliably 
            handle newline characters q*/
      SO->SurfCont->SurfInfo_label = XtVaCreateManagedWidget ("dingel", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
      XmStringFree (xmstmp);
      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, rc, 
                                 XmNorientation, XmVERTICAL, NULL );

      SO->SurfCont->SurfInfo_pb = XtVaCreateWidget ("more", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (SO->SurfCont->SurfInfo_pb, XmNactivateCallback, 
                     SUMA_cb_moreSurfInfo, (XtPointer)SO->SurfCont->curSOp);
      XtVaSetValues (SO->SurfCont->SurfInfo_pb, XmNuserData, 
                     (XtPointer)SO->SurfCont->curSOp, NULL); /* 
            Feb 23 04: XmNuserData is not used anymore.
            See notes for SUMA_cb_moreViewerInfo
            call for reasons why this was done...

            store the surface object SO in userData
            I think it is more convenient than as data
            in the call back structure. This way it will
            be easy to change the SO that this same button
            might refer to. 
            This is only for testing purposes, the pb_close
            button still expects SO in clientData*/
      MCW_register_hint( SO->SurfCont->SurfInfo_pb , "More info on Surface" ) ;
      MCW_register_help( SO->SurfCont->SurfInfo_pb , SUMA_SurfContHelp_more ) ;
      XtManageChild (SO->SurfCont->SurfInfo_pb); 

      XtManageChild (rc);
      
      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, rc_SurfProp, 
                                 XmNorientation, XmHORIZONTAL,NULL);

      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_SurfProp,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);

      /* rendering menu option */
      SUMA_BuildMenuReset(0);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                                 "RenderMode", '\0', YUP, RenderMode_Menu, 
                                 (void *)(SO->SurfCont->curSOp), 
                                 "Choose the rendering mode for this surface.",
                                 SUMA_SurfContHelp_RenderMode, 
                                 SO->SurfCont->RenderModeMenu );
      XtManageChild (SO->SurfCont->RenderModeMenu[SW_SurfCont_Render]);
      
      pb = XtVaCreateWidget ("Dsets", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ToggleManagementColPlaneWidget, 
                     (XtPointer) SO->SurfCont->curSOp);
      MCW_register_hint( pb , 
                        "Show/Hide Dataset (previously Color Plane) "
                        "controllers" ) ;
      MCW_register_help( pb , SUMA_SurfContHelp_Dsets ) ;
      XtManageChild (pb);
      
      XtManageChild (rc);

      XtManageChild (rc_SurfProp);
      XtManageChild (SurfFrame);
   }  
   
   {  /* Xhair Controls */
      Widget rcv;
      /* put a frame */
      SO->SurfCont->Xhair_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      XtVaCreateManagedWidget ("Xhair Info",
            xmLabelWidgetClass, SO->SurfCont->Xhair_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
            
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->Xhair_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateXhairWidgets(rcv, SO);

      
      XtManageChild(rcv);
      XtManageChild(SO->SurfCont->Xhair_fr);
   }  /* Xhair Controls */
    
   {  /* Dset Mapping */
      Widget rcv;
      /* put a frame */
      SO->SurfCont->DsetMap_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_right,
         XmNrightAttachment , XmATTACH_FORM ,
         XmNleftAttachment, XmATTACH_WIDGET,
         XmNleftWidget, SurfFrame,
         XmNtopAttachment  , XmATTACH_FORM ,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      XtVaCreateManagedWidget ("Dset Mapping",
            xmLabelWidgetClass, SO->SurfCont->DsetMap_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
            
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->DsetMap_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateCmapWidgets(rcv, SO);

      XtManageChild(rcv);

      XtManageChild(SO->SurfCont->DsetMap_fr);
   }

   /* Dset Controls */
   {
       Widget rc, rcv, pb;
     
      
      /* put a frame */
      SO->SurfCont->ColPlane_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      XtVaCreateManagedWidget ("Dset Controls",
            xmLabelWidgetClass, SO->SurfCont->ColPlane_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
            
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->ColPlane_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
            
      /* row column for label*/
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rcv,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      
      /*put a label containing the surface name, 
         number of nodes and number of facesets */
      {
         char *Dset_tit[]  =  {  "Lbl", "Par", NULL };
         char *Dset_hint[] =  {  "Label of Dset", 
                                 "Parent surface of Dset", NULL };
         char *Dset_help[] =  {  SUMA_SurfContHelp_DsetLblTblr0, 
                                 SUMA_SurfContHelp_DsetLblTblr1, NULL };
         int colw[]={ 3, 27};
         SUMA_CreateTable(rc, 
            2, 2,
            Dset_tit, NULL,
            Dset_hint, NULL,
            Dset_help, NULL,
            colw, NOPE, SUMA_string,
            NULL, NULL,
            NULL, NULL,
            NULL, NULL, 
            SO->SurfCont->ColPlaneLabelTable);
      }
      XtManageChild (rc);
      
      /* add a rc for the colorplane order and opacity */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
   
      SUMA_CreateArrowField ( rc, "Ord:",
                           1, 0, 20, 1,
                           2, SUMA_int,
                           NOPE,
                           SUMA_ColPlane_NewOrder, (void *)SO,
                           SUMA_SurfCont_ColPlaneOrder_hint, 
                           SUMA_SurfContHelp_DsetOrd,
                           SO->SurfCont->ColPlaneOrder);
                             
      SUMA_CreateArrowField ( rc, "Opa:",
                           1, 0.0, 1.0, 0.1,
                           3, SUMA_float,
                           NOPE,
                           SUMA_ColPlane_NewOpacity, (void *)SO,
                           SUMA_SurfCont_ColPlaneOpacity_hint,
                           SUMA_SurfContHelp_DsetOpa,
                           SO->SurfCont->ColPlaneOpacity);

      /* manage  rc */
      XtManageChild (rc);
      
      /* add a rc for the colorplane brightness factor and visibility */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
   
      SUMA_CreateArrowField ( rc, "Dim:",
                           1, 0.1, 1, 0.1,
                           3, SUMA_float,
                           YUP,
                           SUMA_ColPlane_NewDimFact, (void *)SO, 
                           SUMA_SurfCont_ColPlaneDim_hint, 
                           SUMA_SurfContHelp_DsetDim,
                           SO->SurfCont->ColPlaneDimFact);
           
      SO->SurfCont->ColPlaneShow_tb = XtVaCreateManagedWidget("view", 
            xmToggleButtonWidgetClass, rc, NULL);
      XmToggleButtonSetState (SO->SurfCont->ColPlaneShow_tb, YUP, NOPE);
      XtAddCallback (SO->SurfCont->ColPlaneShow_tb, 
                  XmNvalueChangedCallback, SUMA_cb_ColPlaneShow_toggled, SO);
                  
      MCW_register_help(SO->SurfCont->ColPlaneShow_tb , 
                        SUMA_SurfContHelp_DsetView ) ;
      MCW_register_hint(SO->SurfCont->ColPlaneShow_tb , "Shows/Hides Dset." ) ;
      SUMA_SET_SELECT_COLOR(SO->SurfCont->ColPlaneShow_tb);
      
      SO->SurfCont->ColPlaneShowOne_tb = 
         XtVaCreateManagedWidget("1 Only", 
                                 xmToggleButtonWidgetClass, rc, NULL);
      XmToggleButtonSetState (SO->SurfCont->ColPlaneShowOne_tb, 
                              SO->SurfCont->ShowCurOnly, NOPE);
      XtAddCallback (SO->SurfCont->ColPlaneShowOne_tb, 
                     XmNvalueChangedCallback, 
                     SUMA_cb_ColPlaneShowOne_toggled, SO);
                  
      MCW_register_help(SO->SurfCont->ColPlaneShowOne_tb , 
                        SUMA_SurfContHelp_DsetViewOne ) ;
      MCW_register_hint(SO->SurfCont->ColPlaneShowOne_tb , 
                        "Shows ONLY selected Dset from foreground stack." ) ;
      SUMA_SET_SELECT_COLOR(SO->SurfCont->ColPlaneShowOne_tb);
           
      /* manage  rc */
      XtManageChild (rc);
      
      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, rcv, 
                                 XmNorientation, XmHORIZONTAL,NULL);

      /* row column for Switch, Load, Delete */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
         
      /* put a push button to switch between color planes */
      SO->SurfCont->SwitchDsetlst = 
            SUMA_AllocateScrolledList ("Switch Dset", 
                     SUMA_LSP_SINGLE, 
                     NOPE, NOPE, /* duplicate deletion, no sorting */ 
                     SO->SurfCont->TopLevelShell, SWP_POINTER,
                     125,
                     SUMA_cb_SelectSwitchColPlane, (void *)SO,
                     SUMA_cb_SelectSwitchColPlane, (void *)SO,
                     SUMA_cb_CloseSwitchColPlane, NULL);


      pb = XtVaCreateWidget ("Switch Dset", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_SurfCont_SwitchColPlane, (XtPointer)SO);
      MCW_register_hint(pb , "Switch between datasets" ) ;
      MCW_register_help(pb , SUMA_SurfContHelp_DsetSwitch ) ;
      XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Load Dset", 
            xmPushButtonWidgetClass, rc, 
            NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_Dset_Load, (XtPointer) SO);
         MCW_register_hint(pb ,  
                  "Load a new dataset (much more with BHelp)" ) ;
         MCW_register_help(pb ,  SUMA_SurfContHelp_DsetLoad ) ;
         XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Delete", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ColPlane_Delete, (XtPointer) SO);
      /* XtManageChild (pb); */ /* Not ready for this one yet */

      pb = XtVaCreateWidget ("Load Col", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ColPlane_Load, (XtPointer) SO);
      MCW_register_hint(pb , "Load a new color plane (more with BHelp)" ) ;
      MCW_register_help(pb , SUMA_SurfContHelp_DsetLoadCol  ) ;
      XtManageChild (pb);
      
       
      XtManageChild (rc);
      
      /* manage vertical row column */
      XtManageChild (rcv);
      
      XtManageChild (SO->SurfCont->ColPlane_fr);
   }
   
   
   if (1){ /*s close and help buttons */
      Widget rc, pb_close, pb_bhelp;
      
      /* put up a frame to group the display controls */
      DispFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNleftAttachment , XmATTACH_FORM ,
         XmNbottomAttachment  , XmATTACH_WIDGET ,
         XmNbottomWidget, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 

         
      /* this one requires Motif 1.2 or newer */
         XtVaCreateManagedWidget ("Disp. Cont.",
            xmLabelWidgetClass, DispFrame, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);

      /* row column Lock rowcolumns */
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, DispFrame,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);

      pb_close = XtVaCreateWidget ("Close", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb_close, XmNactivateCallback, 
                     SUMA_cb_closeSurfaceCont, (XtPointer) SO);
      MCW_register_hint( pb_close , "Close Surface controller" ) ;
      MCW_register_help( pb_close , SUMA_closeSurfaceCont_help ) ;
      XtManageChild (pb_close); 

      pb_bhelp = XtVaCreateWidget ("BHelp", 
         xmPushButtonWidgetClass, rc, 
         NULL);
      XtAddCallback (pb_bhelp, XmNactivateCallback, MCW_click_help_CB, NULL);
      MCW_register_help(pb_bhelp , SUMA_help_help ) ;
      MCW_register_hint(pb_bhelp  , 
                        "Press this button then click on a "
                        "button/label/menu for more help." ) ;

      XtManageChild (pb_bhelp); 



      /* now start managing the row column widget */
      XtManageChild (rc);

      /* manage the frame and the fslabelorm */
      XtManageChild (DispFrame);
   }
   
   XtManageChild (rc_right);
   XtManageChild (rc_left);
   XtManageChild (rc_mamma);
   XtManageChild (SO->SurfCont->Mainform);

   #if SUMA_CONTROLLER_AS_DIALOG    
   #else
   /** Feb 03/03: pop it up if it is a topLevelShellWidgetClass, you should do the popping after all the widgets have been created.
   Otherwise, the window does not size itself correctly when open */
   XtPopup(SO->SurfCont->TopLevelShell, XtGrabNone);
   #endif
   
   /* realize the widget */
   XtRealizeWidget (SO->SurfCont->TopLevelShell);
   
   SUMA_free (slabel);
   
   /* initialize the left side 
      (no need here, that's done in SUMA_cb_viewSurfaceCont)*/
   /* SUMA_Init_SurfCont_SurfParam(SO); */
   
   /* initialize the ColorPlane frame if possible 
   Do it here rather than above because scale goes crazy 
   when parent widgets are being resized*/
   if (!SO->Overlays[0]) {
      SUMA_SurfaceObject *SOp=NULL;
      if (LocalHead) fprintf(SUMA_STDERR,"%s:\n"
                           "NO Overlays yet for this surface\n", FuncName);
      /* happens in very few instances when both child and parent 
      are in the first view and the child is selected before the surface
      controller for that family is ever opened! */
      SOp = SUMA_findSOp_inDOv(SO->LocalDomainParentID, SUMAg_DOv, SUMAg_N_DOv);
      if (!SOp) {
         SUMA_SL_Err("Failed to find parent, should not be.");
      }

      if (!SUMA_GetOverlaysFromParent(SO, SOp)) {
         SUMA_SL_Err("Failed to copy overlays!");
         SUMA_RETURNe;
      }
   }
   
   if (SO->N_Overlays) {
      SUMA_InitializeColPlaneShell(SO, SO->Overlays[0]);
      #ifdef DARWIN
      /* Sometimes color maps do not show up on mac when you first 
         open the surface controller */
      SUMA_SwitchColPlaneCmap(SO, SUMA_CmapOfPlane(SO->SurfCont->curColPlane));
      #endif
   }

   #if USING_LESSTIF
   /* A quick fix to ensure Dset_Mapping 
      gets displayed properly the first time */
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&SO), NULL);
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&SO), NULL);
   #endif

   SUMA_RETURNe;
}

/*!
   \brief close surface controller, expects SO in data
*/
void SUMA_cb_closeSurfaceCont(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_closeSurfaceCont"};
   SUMA_SurfaceObject *SO;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->TopLevelShell) SUMA_RETURNe;

   #ifdef SUMA_USE_WITHDRAW 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing Surface Controller...\n", FuncName);
      
      XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
         XtWindow(SO->SurfCont->TopLevelShell),
         XScreenNumberOfScreen(XtScreen(SO->SurfCont->TopLevelShell)));
   #endif
   #ifdef SUMA_USE_DESTROY 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying Surface Controller...\n", FuncName);
      XtDestroyWidget(SO->SurfCont->TopLevelShell);
      SO->SurfCont->TopLevelShell = NULL;
   #endif

    
   SUMA_RETURNe;

}

/*!
   \brief updates the left side of Surface Controller
*/
SUMA_Boolean SUMA_Init_SurfCont_SurfParam(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_Init_SurfCont_SurfParam"};
   char *slabel = NULL, *Name, *lbl30 = NULL;
   int i, imenu;
   Widget *w=NULL, whist=NULL;
   XmString string;
   SUMA_SurfaceObject *oSO;
   SUMA_Boolean SameSurface = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   oSO = *(SO->SurfCont->curSOp);
   if (oSO == SO) {
      SameSurface = YUP;
   } else {
      SameSurface = NOPE;
   }
    
   /* set the new current surface pointer */
   *(SO->SurfCont->curSOp) = (void *)SO;
   
   if (!SameSurface) {
      /* initialize the title of the window */
      slabel = (char *)SUMA_malloc (sizeof(char) * (strlen(SO->Label) + 100));
      if (strlen(SO->Label) > 40) {
         char *tmpstr=NULL;
         tmpstr = SUMA_truncate_string(SO->Label, 40);
         if (tmpstr) { 
            sprintf(slabel,"[%s] Surface Controller", tmpstr);
            free(tmpstr); tmpstr=NULL;
         }
      } else {
         sprintf(slabel,"[%s] Surface Controller", SO->Label);
      }
      SUMA_LH("Setting title");
      XtVaSetValues(SO->SurfCont->TopLevelShell, XtNtitle, slabel, NULL);

      /* initialize the string before the more button */
         /*put a label containing the surface name, number of nodes 
            and number of facesets */
         lbl30 = SUMA_set_string_length(SO->Label, ' ', 27);
         if (lbl30) {
            sprintf( slabel,"%s\n%d nodes: %d tri.", 
                     lbl30, SO->N_Node, SO->N_FaceSet); 
            SUMA_free(lbl30); lbl30 = NULL;
         } else {
            sprintf(slabel,"???\n%d nodes: %d tri.", SO->N_Node, SO->N_FaceSet); 
         }
         SUMA_LHv("Setting label to\n>>%s<<\n", slabel);
         string = XmStringCreateLtoR (slabel, XmSTRING_DEFAULT_CHARSET);
         XtVaSetValues( SO->SurfCont->SurfInfo_label, 
                        XmNlabelString, string, NULL);
         XmStringFree (string);

      if (slabel) SUMA_free(slabel); slabel = NULL;
      /* Can't do much with the SurfInfo button,
      You can only have on Info shell/LocalDomainParent
      at a time */

      SUMA_LH("Setting RenderMode");
      /* set the correct RenderMode for that surface */
      imenu = -1;
      switch (SO->PolyMode) {
         case SRM_ViewerDefault:
            imenu = SW_SurfCont_RenderViewerDefault;
            break;
         case SRM_Fill:
            imenu = SW_SurfCont_RenderFill;
            break;
         case SRM_Line:
            imenu = SW_SurfCont_RenderLine;
            break;
         case SRM_Points:
            imenu = SW_SurfCont_RenderPoints;
            break;
         case SRM_Hide:
            imenu = SW_SurfCont_RenderHide;
            break;
         default: 
            fprintf (SUMA_STDERR, "Error %s: Unexpected something.\n", FuncName);
            break;
      }
      /* look for name of widget with imenu for call data. This is overkill but its fun */
      i = 0;
      Name = NULL;
      while (&(RenderMode_Menu[i])) {
         if ((int)RenderMode_Menu[i].callback_data == imenu) {
            Name = RenderMode_Menu[i].label;
            if (LocalHead) fprintf (SUMA_STDERR,"Looking for %s\n", Name);
            /* now we know what the name of the button needed is, look for it*/
            w = SO->SurfCont->RenderModeMenu;
            for (i=0; i< SW_N_SurfCont_Render; ++i) {
               if (LocalHead) fprintf (SUMA_STDERR,"I have %s\n", XtName(w[i]));
               if (strcmp(Name, XtName(w[i])) == 0) {
                  SUMA_LH("Match!");
                  XtVaSetValues(  w[0], XmNmenuHistory , w[i] , NULL ) ;  
                  SUMA_RETURN(YUP);
              }
            }
         }
         ++i;
      }
   } 
   
   /* do even if this is the old surface */ 
    
   SUMA_RETURN(YUP);
}

/*!
   \brief creates/raises the DrawROI window
   
   \param DrawnROI (SUMA_DRAWN_ROI *) a drawn ROI that is currently being drawn. NULL if there are none
   
*/
SUMA_Boolean SUMA_OpenDrawROIWindow (SUMA_DRAWN_ROI *DrawnROI)
{
   static char FuncName[] = {"SUMA_OpenDrawROIWindow"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->X->DrawROI->AppShell) { /* need to create window */
      SUMA_CreateDrawROIWindow ();
   } else {/* just needs raising */
      /* controller already created, need to bring it up again */
      #ifdef SUMA_USE_WITHDRAW
         if (LocalHead) fprintf (SUMA_STDERR,"%s: raising DrawROI window \n", FuncName);
         XMapRaised(SUMAg_CF->X->DPY_controller1, XtWindow(SUMAg_CF->X->DrawROI->AppShell));
      #endif
   }
   
   if (DrawnROI) {
      /* initialize the window */
      SUMA_InitializeDrawROIWindow (DrawnROI);
   } 
   
   SUMA_RETURN(YUP);
}
 
/*!
   \brief Sets the widgets in the DrawROI window based on the DrawnROI structure
*/
SUMA_Boolean SUMA_InitializeDrawROIWindow (SUMA_DRAWN_ROI *DrawnROI)
{
   static char FuncName[] = {"SUMA_InitializeDrawROIWindow"};
   SUMA_SurfaceObject *SOp = NULL;
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!DrawnROI) {
      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: Initializing with NULL.\n", FuncName);
      SUMA_SET_LABEL(SUMAg_CF->X->DrawROI->ParentLabel_lb, "Parent: -");
      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIlbl->textfield, "-");
      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIval->textfield, "0");
   }else {
      if (LocalHead) 
         fprintf (SUMA_STDERR, 
                  "%s: Initializing with %p.\n", FuncName, DrawnROI);
      SOp = SUMA_findSOp_inDOv(DrawnROI->Parent_idcode_str, 
                               SUMAg_DOv, SUMAg_N_DOv);
      if (SOp) {
         sprintf (sbuf, "Parent: %s", SOp->Label);
         SUMA_SET_LABEL(SUMAg_CF->X->DrawROI->ParentLabel_lb, sbuf);
      } else {
         SUMA_SET_LABEL(SUMAg_CF->X->DrawROI->ParentLabel_lb, 
                        "Parent: Not Found");
      }  

      SUMAg_CF->X->DrawROI->curDrawnROI = DrawnROI; 
            /* set the currently drawnROI */

      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIlbl->textfield, 
                           DrawnROI->Label);
      
      SUMAg_CF->X->DrawROI->ROIval->value = DrawnROI->iLabel;
      sprintf(sbuf,"%d", DrawnROI->iLabel);
      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIval->textfield, sbuf);
   }
   SUMA_RETURN (YUP);
}

/*!
   \brief Initializes the widgets in the color plane shell window based on the SUMA_OVERLAYS structue
*/
SUMA_Boolean SUMA_InitializeColPlaneShell (
                  SUMA_SurfaceObject *SO, 
                  SUMA_OVERLAYS *ColPlane)
{
   static char FuncName[] = {"SUMA_InitializeColPlaneShell"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   double range[2];
   int loc[2], i;
   SUMA_SurfaceObject *SOpar=NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!SO->SurfCont->ColPlane_fr) {
      /* just set the curColPlane before returning ZSS  March 25 08*/
      if (ColPlane) SO->SurfCont->curColPlane = ColPlane;
      SUMA_RETURN(YUP);
   }
   
   if (!ColPlane) {
      SUMA_LH("Initializing with NULL");
      SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 0, 1, "-");
      SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 1, 1, "-");
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneOrder->textfield, "-");
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneOpacity->textfield,"-");
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneDimFact->textfield,"-");
      SUMA_RETURN (YUP);
   }else {
      SUMA_LH("Initializing for real");
      if (ColPlane->dset_link) { /* get the parent surface of the colorplane */
         if (ColPlane->dset_link->ngr) {
            SOpar = SUMA_findSOp_inDOv(
                              NI_get_attribute(ColPlane->dset_link->ngr,
                                                "domain_parent_idcode"), 
                              SUMAg_DOv, SUMAg_N_DOv);
         }
      }
      if (!SOpar) {
         SUMA_SL_Warn(  "No parent for dset found.\n"
                        "Proceeding with next best option.");
         SOpar = SO;
      }
      
      if (  strlen(ColPlane->Label) + 
            strlen(SOpar->Label) +  25   > 
            SUMA_MAX_LABEL_LENGTH) {
         SUMA_SL_Warn("Surface Labels too long!");
         SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 
                                 0, 1, "Surface Labels too long!");
         SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 
                                 1, 1, "Surface Labels too long!");
      } else {
         if (strlen(SOpar->Label) > 40) {
            char *tmpstr=NULL;
            tmpstr = SUMA_truncate_string(SOpar->Label, 40);
            if (tmpstr) { 
               sprintf (sbuf, "Label: %s\nParent: %s", ColPlane->Label, tmpstr);
               free(tmpstr); tmpstr = NULL;
            }
            SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 
                                    0, 1, ColPlane->Label);
            SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 
                                    1, 1, tmpstr);
         } else {
            sprintf (sbuf, "Label: %s\nParent: %s", 
                     ColPlane->Label, SOpar->Label);
         }
         SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 
                                 0, 1, ColPlane->Label);
         SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 
                                 1, 1, SOpar->Label);
      }
      
      SO->SurfCont->ColPlaneOrder->value = ColPlane->PlaneOrder;
      sprintf(sbuf,"%d", ColPlane->PlaneOrder);
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneOrder->textfield, sbuf);
      
      SO->SurfCont->ColPlaneOpacity->value = ColPlane->GlobalOpacity;
      sprintf(sbuf,"%.1f", ColPlane->GlobalOpacity);
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneOpacity->textfield, sbuf);
      
      if (ColPlane->OptScl) 
         SO->SurfCont->ColPlaneDimFact->value = ColPlane->OptScl->BrightFact;
      else SO->SurfCont->ColPlaneDimFact->value = ColPlane->DimFact;
      sprintf(sbuf,"%.1f", SO->SurfCont->ColPlaneDimFact->value);
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneDimFact->textfield, sbuf);
      
   }
   
   XmToggleButtonSetState (SO->SurfCont->ColPlaneShow_tb, ColPlane->Show, NOPE);

   SO->SurfCont->curColPlane = ColPlane;
      
   /* update the cross hair group */
   SUMA_Init_SurfCont_CrossHair(SO);
   
   /* set the colormap */
   if (SO->SurfCont->cmp_ren->cmap_context) {
      if (strcmp(SO->SurfCont->curColPlane->cmapname, "explicit") == 0) {
         if (XtIsManaged(SO->SurfCont->DsetMap_fr)) {
            SUMA_LH("An RGB dset, no surface controls to be seen");
            XtUnmanageChild(SO->SurfCont->DsetMap_fr);
            XtUnmanageChild(XtParent(SO->SurfCont->DsetMap_fr));
         }
      } else {
         if (!XtIsManaged(SO->SurfCont->DsetMap_fr)) {
            SUMA_LH( "A non RGB dset, surface controls need to be seen\n"
                     "But only when ColPlane_fr is also shown \n"
                     "(frame may be hidden by Dsets button action)");
            if (XtIsManaged(SO->SurfCont->ColPlane_fr)) {
               XtManageChild(XtParent(SO->SurfCont->DsetMap_fr));
               XtManageChild(SO->SurfCont->DsetMap_fr);
            }
         }
         SUMA_cmap_wid_handleRedisplay((XtPointer) SO); 

         /* set the widgets for dems mapping options */
         SUMA_set_cmap_options(SO, YUP, NOPE);

         /* set the menu to show the colormap used */
         SUMA_SetCmapMenuChoice(SO, ColPlane->cmapname);

         /* set the values for the threshold bar */
         if (SUMA_GetDsetColRange(  SO->SurfCont->curColPlane->dset_link, 
                                    SO->SurfCont->curColPlane->OptScl->tind, 
                                    range, loc)) {   
            SUMA_SetScaleRange(SO, range );
         }
      }
   } else {
      SUMA_LH("cmap_context was NULL");
   }
   
   
   SUMA_RETURN (YUP);
}

/*!
   \brief Updates color plane editing windows of surfaces related to SO, 
   if the color planes are open and displaying the same plane
   
*/
SUMA_Boolean SUMA_UpdateColPlaneShellAsNeeded(SUMA_SurfaceObject *SO)
{
   static char FuncName[] = {"SUMA_UpdateColPlaneShellAsNeeded"};
   int i=-1;
   SUMA_SurfaceObject *SOi=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO->SurfCont) SUMA_RETURN(NOPE);
   
   SUMA_LH("Called");
   /* find out which surfaces are related to SO */
   for (i=0; i<SUMAg_N_DOv; ++i) {
      if (SUMA_isSO(SUMAg_DOv[i])) {
         SOi = (SUMA_SurfaceObject *)SUMAg_DOv[i].OP;
         if (SOi != SO && SUMA_isRelated (SOi, SO, 1)) { 
            /* do this for kins of the 1st order */
            if (SOi->SurfCont) {
               if (  SOi->SurfCont != SO->SurfCont && 
                     SOi->SurfCont->ColPlane_fr && 
                     SOi->SurfCont->curColPlane == SO->SurfCont->curColPlane) {
                  SUMA_InitializeColPlaneShell(SOi, SOi->SurfCont->curColPlane);
               }
            }
         }
      }
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Creates the widgets for the DrawROI window
*/
void SUMA_CreateDrawROIWindow(void)
{
   static char FuncName[] = {"SUMA_CreateDrawROIWindow"};
   Widget form, frame, rc, pb, rc_ur, rcv, rc_switch, rc_save;
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (SUMAg_CF->X->DrawROI->AppShell) {
      fprintf (SUMA_STDERR,"Error %s: SUMAg_CF->X->DrawROI->AppShell!=NULL. Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
    
   /* create as a separate application shell, you do not want a parent to this controller that
   can be closed or withdrawn temporarily */
   SUMAg_CF->X->DrawROI->AppShell = XtVaAppCreateShell("Draw ROI" , "Suma" ,
      topLevelShellWidgetClass , SUMAg_CF->X->DPY_controller1 ,
      NULL ) ;
   
   /* turn off default delete response. If you do not do that, you will suffer.*/
   XtVaSetValues( SUMAg_CF->X->DrawROI->AppShell,
           XmNdeleteResponse, XmDO_NOTHING,
           NULL);  
             
   /* handle the close button from window manager */
   XmAddWMProtocolCallback(/* make "Close" window menu work */
      SUMAg_CF->X->DrawROI->AppShell,
      XmInternAtom( SUMAg_CF->X->DPY_controller1 , "WM_DELETE_WINDOW" , False ) ,
      SUMA_cb_CloseDrawROIWindow, NULL) ;
   
   /* create a form widget, manage it at the end ...*/
   form = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SUMAg_CF->X->DrawROI->AppShell,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   /* a frame to put stuff in */
   frame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, form,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_FORM ,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL); 
   
   XtVaCreateManagedWidget ("ROI",
      xmLabelWidgetClass, frame, 
      XmNchildType, XmFRAME_TITLE_CHILD,
      XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
      NULL);
   
   
   /* vertical row column to stack horizontal rcs in */
   rcv = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, frame,
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
         
   /* row column for the parent surface name */
   rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
   
   /*put a label containing the ROI's parent surface name */
   SUMAg_CF->X->DrawROI->ParentLabel_lb = XtVaCreateManagedWidget ("Parent: N/A", 
            xmLabelWidgetClass, rc,
            NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->ParentLabel_lb , SUMA_DrawROI_ParentLabel_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->ParentLabel_lb , "Label of the ROI's parent surface" ) ;
   
   XtManageChild(rc);
   
   /* row column for the surface labels and the toggle DrawROI buttons */
   rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);

   /*put a toggle button for the DrawROI more */
   /* Turn on the ROI drawing mode, since that is what the users want to do the first time they open this window */
   SUMAg_CF->ROI_mode = YUP;
   /* make a call to change the cursor */
   SUMA_UpdateAllViewerCursor(); 
   SUMAg_CF->X->DrawROI->DrawROImode_tb = XtVaCreateManagedWidget("Draw Mode", 
      xmToggleButtonWidgetClass, rc, NULL);
   XmToggleButtonSetState (SUMAg_CF->X->DrawROI->DrawROImode_tb, SUMAg_CF->ROI_mode, NOPE);
   XtAddCallback (SUMAg_CF->X->DrawROI->DrawROImode_tb, 
                  XmNvalueChangedCallback, SUMA_cb_DrawROImode_toggled, 
                  NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->DrawROImode_tb , SUMA_DrawROI_DrawROIMode_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->DrawROImode_tb , "Toggles ROI drawing mode" ) ;

   /* set the toggle button's select color */
   SUMA_SET_SELECT_COLOR(SUMAg_CF->X->DrawROI->DrawROImode_tb);
   
   /*put a toggle button for the Pen mode */
   SUMAg_CF->X->DrawROI->Penmode_tb = XtVaCreateManagedWidget("Pen", 
      xmToggleButtonWidgetClass, rc, NULL);
   XmToggleButtonSetState (SUMAg_CF->X->DrawROI->Penmode_tb, SUMAg_CF->Pen_mode, NOPE);
   XtAddCallback (SUMAg_CF->X->DrawROI->Penmode_tb, 
                  XmNvalueChangedCallback, SUMA_cb_DrawROIPen_toggled, 
                  NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->Penmode_tb , SUMA_DrawROI_PenMode_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->Penmode_tb , "Toggles Pen drawing mode" ) ;

   /* set the toggle button's select color */
   SUMA_SET_SELECT_COLOR(SUMAg_CF->X->DrawROI->Penmode_tb);
   
   /* set sensitivity of Pen button */
   if (SUMAg_CF->ROI_mode) XtSetSensitive (SUMAg_CF->X->DrawROI->Penmode_tb, 1);
   else XtSetSensitive (SUMAg_CF->X->DrawROI->Penmode_tb, 0);
   
   /* Put a toggle button for real time communication with AFNI */
   SUMAg_CF->X->DrawROI->AfniLink_tb = XtVaCreateManagedWidget("Afni", 
      xmToggleButtonWidgetClass, rc, NULL);
   
   #if 0
   /* can the link be on ? */
   if (SUMAg_CF->Connected) SUMAg_CF->ROI2afni = YUP;
   else SUMAg_CF->ROI2afni = NOPE;
   #endif
   SUMAg_CF->ROI2afni = NOPE; /* keep link off when starting, 
                                 otherwise  it is confusing */
   
   XmToggleButtonSetState (SUMAg_CF->X->DrawROI->AfniLink_tb, 
                           SUMAg_CF->ROI2afni, NOPE);
   XtAddCallback (SUMAg_CF->X->DrawROI->AfniLink_tb, 
                  XmNvalueChangedCallback, SUMA_cb_AfniLink_toggled, 
                  NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->AfniLink_tb , 
                     SUMA_DrawROI_AfniLink_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->AfniLink_tb , 
                     "Toggles Link to Afni" ) ;

   /* set the toggle button's select color */
   SUMA_SET_SELECT_COLOR(SUMAg_CF->X->DrawROI->AfniLink_tb);
   
   /* put a menu for writing distance info */
   SUMA_BuildMenuReset(0);
   SUMA_BuildMenu (rc, XmMENU_OPTION, 
                   "Dist", '\0', YUP, DrawROI_WhatDist_Menu, 
                   "DoDist", 
                   "Report length of drawn segments? (BHelp for more)", 
                   SUMA_DrawROI_WhatDist_help,   
                   SUMAg_CF->X->DrawROI->WhatDistMenu);
   XtManageChild (SUMAg_CF->X->DrawROI->WhatDistMenu[SW_DrawROI_WhatDist]);
   
    /* manage rc */
   XtManageChild (rc);
   
   /* add a rc for the ROI label and the ROI node value */
   rc = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, rcv,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight, 0 ,
      XmNmarginWidth , 0 ,
      NULL);
  
   
   SUMA_CreateTextField ( rc, "Label:",
                           6, SUMA_DrawROI_NewLabel,
                           "Label of ROI being drawn", SUMA_DrawROI_Label_help,
                           SUMAg_CF->X->DrawROI->ROIlbl);
                        
   SUMA_CreateArrowField ( rc, "Value:",
                           1, 0, 999, 1,
                           3, SUMA_int,
                           NOPE,
                           SUMA_DrawROI_NewValue, NULL,
                           "Integer value associated with ROI", 
                           SUMA_DrawROI_Value_help,
                           SUMAg_CF->X->DrawROI->ROIval);
   /* manage  rc */
   XtManageChild (rc);
   
   
   /* a separator */
   XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rcv, NULL);
   
   /* add rc for undo, redo buttons */
   rc_ur = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, rcv,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight, SUMA_MARGIN ,
      XmNmarginWidth , SUMA_MARGIN ,
      NULL);

   SUMAg_CF->X->DrawROI->Undo_pb = XtVaCreateWidget ("Undo", 
      xmPushButtonWidgetClass, rc_ur, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Undo_pb, XmNactivateCallback, SUMA_cb_DrawROI_Undo, NULL);   
   MCW_register_help(SUMAg_CF->X->DrawROI->Undo_pb , SUMA_DrawROI_Undo_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->Undo_pb , "Undo the last action on the stack" ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Undo_pb);
   
   SUMAg_CF->X->DrawROI->Redo_pb = XtVaCreateWidget ("Redo", 
      xmPushButtonWidgetClass, rc_ur, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Redo_pb, XmNactivateCallback, SUMA_cb_DrawROI_Redo, NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->Redo_pb , SUMA_DrawROI_Redo_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->Redo_pb , "Redo the last undone action" ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Redo_pb);
   
   XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc_ur, 
                              XmNorientation, XmVERTICAL,NULL);
   
   SUMAg_CF->X->DrawROI->Join_pb = XtVaCreateWidget ("Join", 
      xmPushButtonWidgetClass, rc_ur, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Join_pb, XmNactivateCallback, SUMA_cb_DrawROI_Join, NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->Join_pb , SUMA_DrawROI_Join_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->Join_pb , "Join the first node of the path to the last" ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Join_pb);
                                 
   SUMAg_CF->X->DrawROI->Finish_pb = XtVaCreateWidget ("Finish", 
      xmPushButtonWidgetClass, rc_ur, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Finish_pb, XmNactivateCallback, SUMA_cb_DrawROI_Finish, NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->Finish_pb , SUMA_DrawROI_Finish_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->Finish_pb , "Label ROI as finished." ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Finish_pb);
                                 
   /* a separator */
   XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rcv, NULL);

   /* manage rc_ur */
   XtManageChild (rc_ur);
  
   /* add rc for switchin */
   rc_switch = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, rcv,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight, SUMA_MARGIN ,
      XmNmarginWidth , SUMA_MARGIN ,
      NULL);

   
   
   /* put a push button to switch between ROIs */
   SUMAg_CF->X->DrawROI->SwitchROIlst = 
      SUMA_AllocateScrolledList ("Switch ROI", SUMA_LSP_SINGLE, 
                                 NOPE, YUP,
                                 SUMAg_CF->X->DrawROI->AppShell, SWP_TOP_LEFT,
                                 125,
                                 SUMA_cb_SelectSwitchROI, NULL,
                                 SUMA_cb_SelectSwitchROI, NULL,
                                 SUMA_cb_CloseSwitchROI, NULL);
    
   pb = XtVaCreateWidget ( "Switch ROI", xmPushButtonWidgetClass, 
                           rc_switch, NULL);
   XtAddCallback (pb, XmNactivateCallback, SUMA_cb_DrawROI_SwitchROI, 
                  SUMAg_CF->X->DrawROI->SwitchROIlst);
   MCW_register_help(pb , SUMA_DrawROI_SwitchROI_help ) ;
   MCW_register_hint(pb , "Switch between ROIs." ) ;
   XtManageChild (pb);
   
   SUMAg_CF->X->DrawROI->Load_pb = XtVaCreateWidget ("Load", 
      xmPushButtonWidgetClass, rc_switch, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Load_pb, 
                  XmNactivateCallback, SUMA_cb_DrawROI_Load, NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->Load_pb , SUMA_DrawROI_Load_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->Load_pb , "Load a Drawn ROI" ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Load_pb);
   
   XtVaCreateManagedWidget (  "sep", 
                           xmSeparatorWidgetClass, rc_switch, 
                           XmNorientation, XmVERTICAL,NULL);
                                                            
   SUMAg_CF->X->DrawROI->Delete_pb = XtVaCreateWidget ("delete ROI", 
      xmPushButtonWidgetClass, rc_switch, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Delete_pb, XmNactivateCallback, SUMA_cb_DrawROI_Delete, NULL);
   MCW_register_hint( SUMAg_CF->X->DrawROI->Delete_pb , "Click twice in 5 seconds to delete ROI. No Undo for this action." ) ;
   MCW_register_help( SUMAg_CF->X->DrawROI->Delete_pb , SUMA_DrawROI_DeleteROI_help);
   MCW_set_widget_bg( SUMAg_CF->X->DrawROI->Delete_pb , MCW_hotcolor(SUMAg_CF->X->DrawROI->Delete_pb) , 0 ) ;

   XtManageChild (SUMAg_CF->X->DrawROI->Delete_pb); 


   /* manage rc_switch */
   XtManageChild (rc_switch);
   
   /* a separator */
   XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rcv, NULL);
   
   /* add rc for savin */
   rc_save = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, rcv,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight, SUMA_MARGIN ,
      XmNmarginWidth , SUMA_MARGIN ,
      NULL);

   SUMAg_CF->X->DrawROI->Save_pb = XtVaCreateWidget ("Save", 
      xmPushButtonWidgetClass, rc_save, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Save_pb, XmNactivateCallback, SUMA_cb_DrawROI_Save, NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->Save_pb , SUMA_DrawROI_Save_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->Save_pb , "Save the Drawn ROI to disk." ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Save_pb);

   /* Saving Mode */
   SUMA_BuildMenuReset(0);
   SUMA_BuildMenu (rc_save, XmMENU_OPTION, 
                               "", '\0', YUP, DrawROI_SaveMode_Menu, 
                               "Frm.", "File format for saving ROI", SUMA_DrawROI_SaveFormat_help, 
                               SUMAg_CF->X->DrawROI->SaveModeMenu);
   XtManageChild (SUMAg_CF->X->DrawROI->SaveModeMenu[SW_DrawROI_SaveMode]);
      
   /* Saving what ? */
   SUMA_BuildMenuReset(0);
   SUMA_BuildMenu (rc_save, XmMENU_OPTION, 
                               "", '\0', YUP, DrawROI_SaveWhat_Menu, 
                               "What", "Which ROIs to save?", SUMA_DrawROI_SaveWhat_help,   
                               SUMAg_CF->X->DrawROI->SaveWhatMenu);
   XtManageChild (SUMAg_CF->X->DrawROI->SaveWhatMenu[SW_DrawROI_SaveWhat]);
      

   XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc_save, 
                              XmNorientation, XmVERTICAL,NULL);

   pb = XtVaCreateWidget ("BHelp", 
      xmPushButtonWidgetClass, rc_save, 
      NULL);
   XtAddCallback (pb, XmNactivateCallback, MCW_click_help_CB, NULL);  
   MCW_register_help(pb , SUMA_help_help ) ;
   MCW_register_hint(pb , "Press this button then click on a button/label/menu for more help." ) ;
   XtManageChild (pb);
    
   SUMAg_CF->X->DrawROI->Close_pb = XtVaCreateWidget ("Close", 
      xmPushButtonWidgetClass, rc_save, 
      NULL);   
   XtAddCallback (SUMAg_CF->X->DrawROI->Close_pb, XmNactivateCallback, SUMA_cb_CloseDrawROIWindow, NULL);
   MCW_register_hint(SUMAg_CF->X->DrawROI->Close_pb  , "Close Draw ROI window" ) ;
   MCW_register_help(SUMAg_CF->X->DrawROI->Close_pb  , SUMA_closeDrawROI_help ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Close_pb);  
   
   /* manage rc_save */
   XtManageChild (rc_save);

   /* manage vertical rc */
   XtManageChild (rcv);
   
   /* manage frame */
   XtManageChild (frame);
   
   /* manage form */
   XtManageChild (form);

   /* position the widget relative to the first open viewer */
   i=0;
   while (i < SUMAg_N_SVv && !SUMAg_SVv[i].X->ViewCont->TopLevelShell && SUMAg_SVv[i].isShaded) ++i; 

   if (i < SUMAg_N_SVv) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: i = %d\n", FuncName, i);
      SUMA_PositionWindowRelative (SUMAg_CF->X->DrawROI->AppShell, SUMAg_SVv[i].X->TOPLEVEL, SWP_TOP_RIGHT);
   }

   /* realize the widget */
   XtRealizeWidget (SUMAg_CF->X->DrawROI->AppShell);
   
   
   SUMA_RETURNe;
}

/*!
   \brief: Creates and initializes the structure for a scrolled list
   
   \param Label (char *) Label of window containing list. 
         Label should be freed (if necessary) after this function returns.
   \param SelectPolicy (SUMA_ListSelectPolicy) list selection policy
   \param RemoveDups (SUMA_Boolean) YUP: Allow duplicate entries, NOPE: Purge duplicate entries
   \param ShowSorted (SUMA_Boolean) YUP: Sort list entries
   \param PosRef (Widget) Widget to position list relative to
   \param Pos (SUMA_WINDOW_POSITION) position of list relative to PosRef
   \param Default_cb pointer to default selection callack function
   \param Default_Data (void *) pointer to default callback data, If you specify NULL then 
         the SUMA_LIST_WIDGET * is sent in data.
   \param Select_cb pointer to selection callback function. That
         function should be ready to deal with te SelectionPolicy you have set.
         See Motif Programming Manual, section 12.5.
         Typically, you will use the same function for Select_cb and Default_cb
   \param Select_Data (void *) pointer to select callback data, If you specify NULL then 
         the SUMA_LIST_WIDGET * is sent in data.
   \param CloseList_cb pointer to close callback function
   \param CloseList_Data (void *) pointer to close callback data, If you specify NULL then 
         the SUMA_LIST_WIDGET * is sent in data.
   \return LW (SUMA_LIST_WIDGET *) allocate and initialized List Widget structure
   
   \sa SUMA_FreeScrolledList
   
*/   
SUMA_LIST_WIDGET * SUMA_AllocateScrolledList (
      char *Label, int SelectPolicy, 
      SUMA_Boolean RemoveDups, SUMA_Boolean ShowSorted,
      Widget PosRef, SUMA_WINDOW_POSITION Pos,
      int width,
      void (*Default_cb)
            (Widget w, XtPointer data, XtPointer calldata), 
      void *Default_Data,
      void (*Select_cb)
            (Widget w, XtPointer data, XtPointer calldata), 
      void *Select_Data,
      void (*CloseList_cb)
            (Widget w, XtPointer data, XtPointer calldata), 
      void *CloseList_Data)
{
   static char FuncName[]={"SUMA_AllocateScrolledList"};
   SUMA_LIST_WIDGET *LW = NULL;

   SUMA_ENTRY;
   

   if (!Label) {
      SUMA_SLP_Err("Null Label");
      SUMA_RETURN(LW);
   }
   
   LW = (SUMA_LIST_WIDGET *) SUMA_malloc(sizeof(SUMA_LIST_WIDGET));
   memset(LW, 0, sizeof(SUMA_LIST_WIDGET));
   LW->toplevel = NULL;
   LW->list = NULL;      
   LW->rc = NULL;  
   LW->RemoveDups = RemoveDups;
   LW->SelectPolicy = SelectPolicy;
   LW->ShowSorted = ShowSorted;
   LW->Label = (char *)SUMA_calloc(strlen(Label)+1, sizeof(char));
   LW->Label = strcpy (LW->Label, Label);
   LW->PosRef = PosRef;
   LW->Pos = Pos;
   LW->CloseList_cb = CloseList_cb;
   LW->CloseList_Data = CloseList_Data;
   LW->Default_cb = Default_cb;
   LW->Default_Data = Default_Data;
   LW->Select_cb = Select_cb;
   LW->Select_Data = Select_Data;
   LW->ALS = NULL;
   LW->isShaded = YUP;
   LW->lastitempos = -1;
   if (width > 0) LW->width = width;
   else LW->width = 125;
   
   SUMA_RETURN(LW);
   
}

/*!
   \brief Frees the SUMA_LIST_WIDGET *
*/
SUMA_LIST_WIDGET * SUMA_FreeScrolledList (SUMA_LIST_WIDGET *LW)
{
   static char FuncName[]={"SUMA_FreeScrolledList"};
   
   SUMA_ENTRY;
   if (LW->Label) SUMA_free(LW->Label);
   if (LW->ALS) SUMA_FreeAssembleListStruct(LW->ALS);
   if (LW) SUMA_free(LW);
   
   SUMA_RETURN(NULL);
   
}

/*!
   \brief changes the Default_Data Select_Data  callback data for a list widget.
   Does not change callbacks although that can be arranged
   - Do not change callback data outside of this function
   - Make sure you remove callbacks properly or you'll end
   up with multiple callbacks
   \sa SUMA_CreateScrolledList
*/
SUMA_Boolean SUMA_UpdateScrolledListData(SUMA_LIST_WIDGET *LW, void *Default_Data, void *Select_Data, void *CloseList_Data) 
{
   static char FuncName[]={"SUMA_UpdateScrolledListData"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!LW) SUMA_RETURN(NOPE);
   if (!LW->toplevel) { /* no callbacks yet, just assign the data values to their positions in LW */
      LW->Default_Data = Default_Data;
      LW->Select_Data = Select_Data;
      LW->CloseList_Data = CloseList_Data; 
      SUMA_RETURN(YUP);
   }

   /* need to remove old callbacks before adding new ones SEE ALSO SUMA_CreateScrolledList*/
   if (LW->Default_Data !=  Default_Data) {
      SUMA_LH("Doing Default Data..."); 
      if (!LW->Default_Data) {
         XtRemoveCallback(LW->list, XmNdefaultActionCallback, LW->Default_cb, (XtPointer)LW);
      } else {
         XtRemoveCallback (LW->list, XmNdefaultActionCallback, LW->Default_cb, (XtPointer)LW->Default_Data);
      }
      if (!Default_Data) {
         XtAddCallback (LW->list, XmNdefaultActionCallback, LW->Default_cb, (XtPointer)LW);
      } else {
         XtAddCallback (LW->list, XmNdefaultActionCallback, LW->Default_cb, (XtPointer)Default_Data);
      }
      LW->Default_Data =  Default_Data;
   }

   if (LW->Select_Data !=  Select_Data) { 
      SUMA_LH("Doing Select Data..."); 
      switch (LW->SelectPolicy){
         case SUMA_LSP_SINGLE:
            if (!LW->Select_Data) 
               XtRemoveCallback (LW->list, XmNsingleSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtRemoveCallback (LW->list, XmNsingleSelectionCallback, LW->Select_cb, (XtPointer)LW->Select_Data); 
            break;
         case SUMA_LSP_BROWSE:
            if (!LW->Select_Data) 
               XtRemoveCallback (LW->list, XmNbrowseSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtRemoveCallback (LW->list, XmNbrowseSelectionCallback, LW->Select_cb, (XtPointer)LW->Select_Data); 

            break;
         case SUMA_LSP_MULTIPLE:
            if (!LW->Select_Data) 
               XtRemoveCallback (LW->list, XmNmultipleSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtRemoveCallback (LW->list, XmNmultipleSelectionCallback, LW->Select_cb, (XtPointer)LW->Select_Data); 

            break;
         case SUMA_LSP_EXTENDED:
            if (!LW->Select_Data) 
               XtRemoveCallback (LW->list, XmNextendedSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtRemoveCallback (LW->list, XmNextendedSelectionCallback, LW->Select_cb, (XtPointer)LW->Select_Data); 

            break;
         default:
            SUMA_SL_Err("Bad selection policy");
            SUMA_RETURN(NOPE);
            break;
      }

      switch (LW->SelectPolicy){
         case SUMA_LSP_SINGLE:
            if (!Select_Data) 
               XtAddCallback (LW->list, XmNsingleSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNsingleSelectionCallback, LW->Select_cb, (XtPointer)Select_Data); 
            break;
         case SUMA_LSP_BROWSE:
            if (!Select_Data) 
               XtAddCallback (LW->list, XmNbrowseSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNbrowseSelectionCallback, LW->Select_cb, (XtPointer)Select_Data); 

            break;
         case SUMA_LSP_MULTIPLE:
            if (!Select_Data) 
               XtAddCallback (LW->list, XmNmultipleSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNmultipleSelectionCallback, LW->Select_cb, (XtPointer)Select_Data); 

            break;
         case SUMA_LSP_EXTENDED:
            if (!Select_Data) 
               XtAddCallback (LW->list, XmNextendedSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNextendedSelectionCallback, LW->Select_cb, (XtPointer)Select_Data); 

            break;
         default:
            SUMA_SL_Err("Bad selection policy");
            SUMA_RETURN(NOPE);
            break;
      }

      LW->Select_Data =  Select_Data;

   }

   if (LW->CloseList_Data !=  CloseList_Data) {
      SUMA_LH("Doing CloseList Data..."); 
      if (!LW->CloseList_Data) {
         XmRemoveWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW) ;
      } else {
         XmRemoveWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW->CloseList_Data) ;
      }
      if (!CloseList_Data) {
         XmAddWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW) ;
      } else {
         XmAddWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)CloseList_Data) ;
      }

      LW->CloseList_Data =  CloseList_Data;         
   } 
   SUMA_RETURN(YUP);
}
/*!
   \brief creates a scrolled list window 
   SUMA_CreateScrolledList (  clist, N_clist, Partial, LW);
   
   \param clist (char **) list of strings. You should free it after you call the function
   \param N_clist (int) number of elements in clist
   \param Partial (SUMA_Boolean) YUP: add to existing list, NOPE: Replace it. 
         If !Partial and !LW->RemoveDups then the previous selection position is lost.
   \param LW (SUMA_LIST_WIDGET *) initialized list widget structure.
   
   \sa SUMA_AllocateScrolledList
   \sa SUMA_UpdateScrolledListData
   - If LW->toplevel = NULL then a new widget is created, otherwise only the list is updated
   
                           
*/
void SUMA_CreateScrolledList (    
      char **clist, int N_clist, 
      SUMA_Boolean Partial, SUMA_LIST_WIDGET *LW)
{
   static char FuncName[]={"SUMA_CreateScrolledList"};
   XmString  str, *strlist = NULL;
   char *text;
   int i = -1, iclist, u_bound = 0, l_bound = 0, n;
   Arg args[20];
   SUMA_Boolean New = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   
   SUMA_ENTRY;
   
   if (!LW) { /* Never been created */
      SUMA_SL_Err ("Null LW!");
      SUMA_RETURNe;
   }
   
   if (N_clist <= 0) {
      SUMA_SLP_Note ("No ROIs found");
      SUMA_RETURNe;
   }
   
   if (!LW->toplevel) { 
      /* widget has never been created 
         or had been destroyed, create it anew */  
      /* create the widget */ 
      LW->toplevel = XtVaAppCreateShell(LW->Label , "Suma" ,
                     topLevelShellWidgetClass , SUMAg_CF->X->DPY_controller1 ,
                     NULL ) ;
      
      /* cancel the kill button's effect */
      XtVaSetValues( LW->toplevel,
           XmNdeleteResponse, XmDO_NOTHING,
           NULL);  
             
      /* handle the close button from window manager  
         SEE ALSO  SUMA_UpdateScrolledListData */
      if (!LW->CloseList_Data) {
         XmAddWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom(  SUMAg_CF->X->DPY_controller1  , 
                           "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW) ;
      } else {
         XmAddWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom(  SUMAg_CF->X->DPY_controller1  , 
                           "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW->CloseList_Data) ;
      }   
      

      LW->rc = XtVaCreateWidget( "Tonka", 
                                 xmRowColumnWidgetClass, LW->toplevel, NULL);
      n = 0;
      XtSetArg (args[n], XmNitemCount,      0); n++;
      XtSetArg (args[n], XmNlistSizePolicy,   XmCONSTANT   ); n++;
      XtSetArg (args[n], XmNwidth, LW->width); n++;
      LW->list = XmCreateScrolledList (LW->rc, "Tonka", args, n);
      
      
      /* add the default selection callback  
         SEE ALSO  SUMA_UpdateScrolledListData */
      if (!LW->Default_Data) {
         XtAddCallback (LW->list, 
                        XmNdefaultActionCallback, 
                        LW->Default_cb, (XtPointer)LW);
      } else {
         XtAddCallback (LW->list, 
                        XmNdefaultActionCallback, 
                        LW->Default_cb, (XtPointer)LW->Default_Data);
      }        

     /* set the selection policy SEE ALSO  SUMA_UpdateScrolledListData */
      switch (LW->SelectPolicy){
         case SUMA_LSP_SINGLE:
            XtVaSetValues( LW->list, XmNselectionPolicy, XmSINGLE_SELECT, NULL);
            if (!LW->Select_Data) 
               XtAddCallback (LW->list, 
                              XmNsingleSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNsingleSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW->Select_Data); 
            break;
         case SUMA_LSP_BROWSE:
            XtVaSetValues( LW->list, XmNselectionPolicy, XmBROWSE_SELECT, NULL);
            if (!LW->Select_Data) 
               XtAddCallback (LW->list, 
                              XmNbrowseSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, 
                              XmNbrowseSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW->Select_Data); 
            
            break;
         case SUMA_LSP_MULTIPLE:
            if (!LW->Select_Data) 
               XtAddCallback (LW->list, 
                              XmNmultipleSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, 
                              XmNmultipleSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW->Select_Data); 
            
            XtVaSetValues( LW->list, 
                           XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
            break;
         case SUMA_LSP_EXTENDED:
            if (!LW->Select_Data) 
               XtAddCallback (LW->list, 
                              XmNextendedSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, 
                              XmNextendedSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW->Select_Data); 
            
            XtVaSetValues( LW->list, 
                           XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
            break;
         default:
            SUMA_SL_Err("Bad selection policy");
            SUMA_RETURNe;
            break;
      }
       
       /* manage it */
      if (LocalHead) fprintf(SUMA_STDERR, "%s: Managing ..\n", FuncName);
      XtManageChild (LW->list);
      XtManageChild (LW->rc);


      SUMA_PositionWindowRelative (LW->toplevel, LW->PosRef, LW->Pos);   

      LW->isShaded = NOPE;
      New = YUP;
      
   } else {
      /*map and raise the baby */
      XMapRaised(SUMAg_CF->X->DPY_controller1, XtWindow(LW->toplevel));
      LW->isShaded = NOPE;
   }  

   
   /* now cycle through the elements in clist and add them, 
      if they are new, in alphabetical order */
   if (!Partial && !LW->RemoveDups) {
      if (LocalHead) 
         fprintf (SUMA_STDERR, 
                  "%s: New full list, deleting old entries. \n", FuncName);
      XmListDeleteAllItems(LW->list);
   }else {
      if (LocalHead) 
         fprintf (SUMA_STDERR, 
                  "%s: Partial list, will add.\n", FuncName);
   }
   for (iclist=0; iclist < N_clist; iclist++)     {
      SUMA_LH(clist[iclist]);
      if (LW->ShowSorted) {
         l_bound = 0;
         /* get the current entries (and number of entries) from the List */
         XtVaGetValues (LW->list,
                        XmNitemCount, &u_bound,
                        XmNitems,     &strlist,
                        NULL);
         u_bound--;
         /* perform binary search */
         while (u_bound >= l_bound) {
            int i = l_bound + (u_bound - l_bound) / 2;
            /* convert the compound string into a regular C string */
            if (!XmStringGetLtoR (strlist[i], XmFONTLIST_DEFAULT_TAG, &text))
               break;
            if (strcmp (text, clist[iclist]) > 0)
               u_bound = i - 1; /* newtext comes before item */
            else
               l_bound = i + 1; /* newtext comes after item */
            XtFree (text); /* XmStringGetLtoR() allocates memory ... yuk */
         }
      } else { /* Not sorted, add to bottom*/
        l_bound = 0; 
      }
      
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Adding %s...\n", FuncName, clist[iclist]);
      str = XmStringCreateLocalized (clist[iclist]); 

      /* positions indexes start at 1, so increment accordingly */
      if (LW->RemoveDups) { 
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: removing duplicates\n", FuncName);
         if (!XmListItemExists(LW->list, str)) 
            XmListAddItemUnselected (LW->list, str, l_bound+1);
      } else { 
         if (LocalHead) fprintf (SUMA_STDERR,"%s: with duplicates\n", FuncName);
         XmListAddItemUnselected (LW->list, str, l_bound+1);
      }
      XmStringFree (str);

   }


   XtVaSetValues( LW->list,                  
                  XmNvisibleItemCount, 10,
                  NULL);
   

   
   if (New) {
      XmListSetPos(LW->list,1);
      XmListSelectPos(LW->list,1, False); 
      /* realize the widget */
      XtRealizeWidget (LW->toplevel);
   } else {
      XmListSetPos(LW->list,SUMA_MAX_PAIR(1,LW->lastitempos-3));
      XmListSelectPos(LW->list,SUMA_MAX_PAIR(1,LW->lastitempos), False); 
   }
   
   SUMA_RETURNe;
}



/*!
   \brief adds arrow fields
   SUMA_CreateArrowField (    pw, label, 
                              value,  vmin,  vmax,  vstep,
                              cwidth, type,
                              wrap,
                              NewValueCallback,
                              cb_data ,
                              AF);
                              
   \param pw (Widget)   Parent widget
   \param label (char *) label (NULL for nothing)
   \param value (float) current value
   \param vmin (float) minimum value
   \param vmax (float) maximum value
   \param vstep (float) arrow increment
   \param cwidth (int) number of columns for text field
   \param type (SUMA_VARTYPE) SUMA_int or SUMA_float
   \param wrap (SUMA_Boolean) YUP=wrap values, NOPE=clip values
   \param NewValueCallback(void *data) (void *) Function to call when there is a new value in town. 
   \param cb_data (void *) data to send to callback.
                           if NULL data is actually the AF structure pointer itself.
   \param hint (char *) if NULL, no hint
   \param help (char *) if NULL no help
   \param AF (SUMA_ARROW_TEXT_FIELD *) structure defining the arrow field.                        
   - AF must be pre-allocated, of course. Its fields are initialized by the values passed to the function
*/
void SUMA_CreateArrowField ( Widget pw, char *label,
                              float value, float vmin, float vmax, float vstep,
                              int cwidth, SUMA_VARTYPE type,
                              SUMA_Boolean wrap,
                              void (*NewValueCallback)(void *data), void *cb_data,
                              char *hint, char *help,
                              SUMA_ARROW_TEXT_FIELD *AF)
{
   static char FuncName[]={"SUMA_CreateArrowField"};
   
   SUMA_ENTRY;
   
   if (!AF) {
      SUMA_RegisterMessage (SUMAg_CF->MessageList, "Bad value in text field", FuncName, SMT_Error, SMA_Log);
      SUMA_RETURNe;  
   }
   
   AF->step = vstep;
   AF->value = value;
   AF->min = vmin;
   AF->max = vmax;
   AF->cwidth = cwidth;
   AF->type = type;
   AF->NewValueCallback = NewValueCallback;
   AF->NewValueCallbackData = cb_data;
   AF->modified = NOPE;
   AF->wrap = wrap;
   AF->rc = XtVaCreateManagedWidget ("Container", 
      xmRowColumnWidgetClass, pw,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      NULL);
   if (hint) MCW_register_hint( AF->rc , hint);
   if (help) MCW_register_help( AF->rc , help);
   
   if (label) {
      AF->label =  XtVaCreateManagedWidget (label,
         xmLabelWidgetClass, AF->rc, 
         XmNmarginHeight, 0,
         XmNmarginTop, 0,
         XmNmarginBottom, 0,
         NULL);
         if (hint) MCW_register_help( AF->label , hint);
         if (help) MCW_register_help( AF->label , help);
   }else {
      AF->label = NULL;
   }

   AF->up = XtVaCreateManagedWidget ("arrow_up",
         xmArrowButtonWidgetClass, AF->rc,
         XmNarrowDirection,   XmARROW_UP,
         XmNmarginHeight, 0,
         XmNmarginTop, 0,
         XmNmarginBottom, 0,
         NULL);
   if (hint) MCW_register_help( AF->up , hint);
   if (help) MCW_register_help( AF->up , help);

   XtVaSetValues (AF->up, XmNuserData, (XtPointer)AF, NULL);
   XtAddCallback (AF->up, XmNarmCallback, SUMA_ATF_start_stop, (XtPointer)1);
   XtAddCallback (AF->up, XmNdisarmCallback, SUMA_ATF_start_stop, (XtPointer)1);

   AF->down = XtVaCreateManagedWidget ("arrow_dn",
      xmArrowButtonWidgetClass, AF->rc,
      XmNarrowDirection,   XmARROW_DOWN,
      XmNmarginHeight, 0,
      XmNmarginTop, 0,
      XmNmarginBottom, 0,
      NULL);
   if (hint) MCW_register_help( AF->down , hint);
   if (help) MCW_register_help( AF->down , help);
   XtVaSetValues (AF->down, XmNuserData, (XtPointer)AF, NULL);
   XtAddCallback (AF->down, XmNarmCallback, SUMA_ATF_start_stop, (XtPointer)-1);
   XtAddCallback (AF->down, XmNdisarmCallback, SUMA_ATF_start_stop, (XtPointer)-1);

   AF->textfield = XtVaCreateManagedWidget ("label",
      xmTextFieldWidgetClass, AF->rc,
      XmNuserData, (XtPointer)AF,
      XmNvalue, "-",
      XmNcolumns, AF->cwidth,
      XmNmarginHeight, 0,
      XmNmarginTop, 0,
      XmNmarginBottom, 0,
      NULL);
   if (hint) MCW_register_hint( AF->textfield , hint);
   if (help) MCW_register_help( AF->textfield , help);
   
   XtAddCallback (AF->textfield, XmNactivateCallback, SUMA_ATF_cb_label_change, (XtPointer)AF);
   XtAddCallback (AF->textfield, XmNmodifyVerifyCallback, SUMA_ATF_cb_label_Modify, (XtPointer)AF);
   
   /* add event handler to nitify when widget was left */
   XtInsertEventHandler( AF->textfield ,        /* notify when */
                         LeaveWindowMask ,  /* pointer leaves */
                         FALSE ,            /* this window */
                         SUMA_leave_EV,
                         (XtPointer) AF ,
                         XtListTail ) ;     /* last in queue */      
   XtManageChild (AF->rc);
   SUMA_RETURNe;
}

/*! 
   creates a text field.
   
   \sa SUMA_CreateArrowField 
*/
void SUMA_CreateTextField ( Widget pw, char *label,
                              int cwidth, 
                              void (*NewValueCallback)(void *data),
                              char *hint, char *help,
                              SUMA_ARROW_TEXT_FIELD *AF)
{
   static char FuncName[]={"SUMA_CreateTextField"};

   SUMA_ENTRY;

   /* techincally, one should have a structure that 
      is only for text but that is not necessary, I think */
   
   AF->up = AF->down = NULL;
   AF->step = AF->value = AF->min = AF->max = AF->wrap = 0;
   
   AF->type = SUMA_string;
   AF->NewValueCallback = NewValueCallback;
   AF->NewValueCallbackData = NULL;
   AF->arrow_action = NOPE;
   AF->cwidth = cwidth;
   AF->modified = NOPE;

   AF->rc = XtVaCreateManagedWidget ("Container", 
      xmRowColumnWidgetClass, pw,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      NULL);
   if (hint) MCW_register_hint( AF->rc , hint);

   if (label) {
      AF->label =  XtVaCreateManagedWidget (label,
         xmLabelWidgetClass, AF->rc, 
         XmNmarginHeight, 0,
         XmNmarginTop, 0,
         XmNmarginBottom, 0,
         NULL);
      if (hint) MCW_register_help( AF->label , hint);
      if (help) MCW_register_help( AF->label , help);
   }else {
      AF->label = NULL;
   }
   
   AF->textfield = XtVaCreateManagedWidget ("label",
      xmTextFieldWidgetClass, AF->rc,
      XmNuserData, (XtPointer)AF,
      XmNvalue, "0",
      XmNcolumns, AF->cwidth,
      XmNmarginHeight, 0,
      XmNmarginTop, 0,
      XmNmarginBottom, 0,
      NULL);
   if (hint) MCW_register_hint( AF->textfield , hint);
   if (help) MCW_register_help( AF->textfield , help);
   
   XtAddCallback (AF->textfield, XmNactivateCallback, SUMA_ATF_cb_label_change, (XtPointer)AF);
   XtAddCallback (AF->textfield, XmNmodifyVerifyCallback, SUMA_ATF_cb_label_Modify, (XtPointer)AF);
   
   /* add event handler to nitify when widget was left */
   XtInsertEventHandler( AF->textfield ,        /* notify when */
                         LeaveWindowMask ,  /* pointer leaves */
                         FALSE ,            /* this window */
                         SUMA_leave_EV,
                         (XtPointer) AF ,
                         XtListTail ) ;     /* last in queue */      
   XtManageChild (AF->rc);   
   SUMA_RETURNe;
} 

/*!
   \brief This function is called when label field has been modified by user keyboard input.
   All it does is set AF->modified to YUP
   
*/
void SUMA_ATF_cb_label_Modify (Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_ATF_cb_label_Modify"};
   SUMA_ARROW_TEXT_FIELD *AF=NULL;
   
   SUMA_ENTRY;
   
   AF = (SUMA_ARROW_TEXT_FIELD *)client_data ;
   if (!AF->arrow_action) AF->modified = YUP;
   
   SUMA_RETURNe;
}

/*!
   \brief This function is called when mouse pointer leaves label field
   It only acts if  AF->modified 
*/
void SUMA_leave_EV( Widget w , XtPointer client_data ,
                  XEvent * ev , Boolean * continue_to_dispatch )
{
   SUMA_ARROW_TEXT_FIELD *AF=NULL; 
   XLeaveWindowEvent * lev = (XLeaveWindowEvent *) ev ;
   XmAnyCallbackStruct cbs ;
   static char FuncName[]={"SUMA_leave_EV"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   AF = (SUMA_ARROW_TEXT_FIELD *)client_data ;
   if( lev->type != LeaveNotify || !AF->modified ) SUMA_RETURNe; 
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Leave notification.\n", FuncName);
   SUMA_ATF_cb_label_change( AF->textfield , (XtPointer) AF , NULL ) ;
   
   SUMA_RETURNe;
}

/*!
   \brief This function is called when the label field is activated by the user
   
*/
void SUMA_ATF_cb_label_change (Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_ATF_cb_label_change"};
   SUMA_ARROW_TEXT_FIELD *AF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* make call to NewValue callback */
   AF = (SUMA_ARROW_TEXT_FIELD *)client_data;
   
   if (AF->type == SUMA_int || AF->type == SUMA_float) SUMA_ATF_SetValue (AF);
   
   if (!AF->NewValueCallbackData) {
      SUMA_LH("No Callback data.");
      AF->NewValueCallback((void*)AF);
   } else {
      SUMA_LH("Callback data.");
      AF->NewValueCallback(AF->NewValueCallbackData);
   }
   
   AF->modified = NOPE;
   SUMA_RETURNe;
}

/*!
   \brief function to handle the pressed buttons of the arrow keys
   
   -Based on code from Motif Programming Manual: arrow_timer.c
   
 - start_stop is used to start or stop the incremental changes to
 * the label's value.  When the button goes down, the reason is
 * XmCR_ARM and the timer starts.  XmCR_DISARM disables the timer.
 */
void SUMA_ATF_start_stop (Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_ATF_start_stop"};
   int incr = (int) client_data;
   SUMA_ARROW_TEXT_FIELD *AF = NULL;
   void *n = NULL;
   XmArrowButtonCallbackStruct *cbs = 
        (XmArrowButtonCallbackStruct *) call_data;
   
   
   SUMA_ENTRY;
   
   if (!w) SUMA_RETURNe;
   
   XtVaGetValues(w, XmNuserData, &n, NULL);
   if (!n) SUMA_RETURNe;
   AF = (SUMA_ARROW_TEXT_FIELD *)n;
   AF->direction = incr;

   if (cbs->reason == XmCR_ARM) {
     AF->arrow_action = YUP;
     SUMA_ATF_change_value (AF, (XtIntervalId *)1 );
   } else if (cbs->reason == XmCR_DISARM) {
     if (AF->arrow_timer_id) XtRemoveTimeOut (AF->arrow_timer_id);
     /* make call to NewValue callback */
     if (!AF->NewValueCallbackData) 
         AF->NewValueCallback((void*)AF);
     else 
         AF->NewValueCallback(AF->NewValueCallbackData);
          
     AF->arrow_action = NOPE;
 
   }     

   SUMA_RETURNe;
}

/*!
   \brief A function that is called when the DrawROI value arrow field is set.
   
   \param data (void *) a typecast of a pointer to a SUMA_ARROW_TEXT_FIELD structure
*/
void SUMA_DrawROI_NewValue (void *data)
{
   static char FuncName[]={"SUMA_DrawROI_NewValue"};
   SUMA_ARROW_TEXT_FIELD *AF=NULL;
   SUMA_DRAWN_ROI *DrawnROI=NULL;
   static int ErrCnt=0;
   DList *list=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   AF = (SUMA_ARROW_TEXT_FIELD *)data;
   DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
   
   if (!DrawnROI) SUMA_RETURNe;
   
   if (AF->value == DrawnROI->iLabel) SUMA_RETURNe;
   
   if (!DrawnROI->DrawStatus == SUMA_ROI_Finished) {
      if (LocalHead) 
         fprintf (SUMA_STDERR, 
                  "%s: Changing ROI value from %d to %d\n", 
                  FuncName, DrawnROI->iLabel, (int)AF->value);   

      DrawnROI->iLabel = (int)AF->value;
      ErrCnt = 0;
   } else {
      if (!ErrCnt) SUMA_SLP_Err ("ROI is marked as finished.\nNew value will not be applied.\n");
      ++ErrCnt;
      AF->value = (float)DrawnROI->iLabel;
      SUMA_ATF_SetString (AF); 
   }
   
   /* if your colors are based on the label, you've got work to do*/
   if (DrawnROI->ColorByLabel) {
      SUMA_SurfaceObject *SO=NULL;
      /* Now update the Paint job on the ROI plane */
      SO = SUMA_findSOp_inDOv (DrawnROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
      if (!SO) {
         SUMA_SLP_Err(  "Failed to find parent surface\n"
                        "No color for you!");
         SUMA_RETURNe;
      }
      if (!SUMA_Paint_SO_ROIplanes_w (SO, SUMAg_DOv, SUMAg_N_DOv)) {
         SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
         SUMA_RETURNe;
      }

      if (!list) list = SUMA_CreateList ();
      SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_SumaWidget, NULL);
      if (!SUMA_Engine (&list)) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
         SUMA_RETURNe;    
      }
   }   
   SUMA_RETURNe;
}

/*!
   \brief Function to update the order of a colorplane 

   -expects SO in data
*/
void SUMA_ColPlane_NewOrder (void *data)
{
   static char FuncName[]={"SUMA_ColPlane_NewOrder"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   SUMA_SurfaceObject *SO=NULL;
   int Old_Order = -1, i, iMove, NetMove;
   SUMA_Boolean Shaded, Decent; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SO = (SUMA_SurfaceObject *)data;
   
   /* make sure a new order is in order */
   if (SO->SurfCont->curColPlane->PlaneOrder == (int)SO->SurfCont->ColPlaneOrder->value) SUMA_RETURNe;
   
   /* Now show the new order */
   if (LocalHead) SUMA_Print_PlaneOrder(SO, NULL);
   

   /* Now figure out the direction of the arrow presses */
   NetMove = (int)SO->SurfCont->ColPlaneOrder->value - SO->SurfCont->curColPlane->PlaneOrder ; 
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s:  Net move %d\n", FuncName, NetMove);
   iMove = 0;
   Decent = YUP;
   if (NetMove > 0) {   
      do {
         Old_Order = SO->SurfCont->curColPlane->PlaneOrder;
         if (!SUMA_MovePlaneUp(SO, SO->SurfCont->curColPlane->Name)) {
            SUMA_L_Err("Error in SUMA_MovePlaneUp.");
            SUMA_RETURNe;
         }
         
         if (SO->SurfCont->curColPlane->PlaneOrder == Old_Order) {
            SUMA_LH("Nothing can be done");
            Decent = NOPE;
         } else {
            ++iMove;
         } 
      } while (iMove < NetMove && Decent);
   } else if (NetMove < 0) {
      do {
         Old_Order = SO->SurfCont->curColPlane->PlaneOrder;
         if (!SUMA_MovePlaneDown(SO, SO->SurfCont->curColPlane->Name)) {
            SUMA_L_Err("Error in SUMA_MovePlaneDown.");
            SUMA_RETURNe;
         }
         if (SO->SurfCont->curColPlane->PlaneOrder == Old_Order) {
            SUMA_LH("Enough");
            Decent = NOPE;
         } else {
            ++iMove;
         } 
      } while (iMove < -NetMove && Decent);   
   } else {
      SUMA_LH("Hmmmmm");
      Decent = NOPE;
   }
   
   SUMA_LH("Out");
   /* Now show the new order */
   if(LocalHead) SUMA_Print_PlaneOrder(SO, NULL);
   
   /* refresh the switch list */
   SUMA_IS_SWITCH_COL_PLANE_SHADED(SO, Shaded);
   if (!Shaded) {
      SUMA_LH("Refreshing Col Plane List");
      SUMA_RefreshDsetList (SO);
   }

   if (!Decent) {
      /* reset order value in widget to its last acceptable value. */
      sprintf(sbuf,"%d", SO->SurfCont->curColPlane->PlaneOrder);
      SO->SurfCont->ColPlaneOrder->value = SO->SurfCont->curColPlane->PlaneOrder;
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneOrder->textfield, sbuf); 
   }
   
   SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */

   if (iMove > 0) { /* something decent was done, act on it */
      /* a good remix and redisplay */
      SUMA_LH("Remix and redisplay");
      SUMA_RemixRedisplay (SO);
   }
   
   
   SUMA_RETURNe;
   
}

/*!
   \brief Function to update the opacity of a colorplane 
   
   -expects SO in data
*/
void SUMA_ColPlane_NewOpacity (void *data)
{
   static char FuncName[]={"SUMA_ColPlane_NewOpacity"};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;

   /* change the value of the global opacity */
   SO->SurfCont->curColPlane->GlobalOpacity = SO->SurfCont->ColPlaneOpacity->value;   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: GlobalOpacity of %s set to %f.\n", 
         FuncName, SO->SurfCont->curColPlane->Name, SO->SurfCont->curColPlane->GlobalOpacity);
   
   SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */

   /* a good remix and redisplay */
   SUMA_RemixRedisplay (SO);
   
   SUMA_RETURNe;
}

/*!
   \brief Function to update the DimFact of a colorplane 
   DimFact is the same as BrightFact which is not defined
   for explicitly colored planes 
   -expects SO in data
*/
void SUMA_ColPlane_NewDimFact (void *data)
{
   static char FuncName[]={"SUMA_ColPlane_NewDimFact"};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   if (!SO->SurfCont || !SO->SurfCont->curColPlane ) SUMA_RETURNe;
   
   /* change the value of the dimfact */
   SO->SurfCont->curColPlane->DimFact = SO->SurfCont->ColPlaneDimFact->value; 
   if (SO->SurfCont->curColPlane->OptScl) 
      SO->SurfCont->curColPlane->OptScl->BrightFact = SO->SurfCont->curColPlane->DimFact;
      
   if (LocalHead) fprintf(SUMA_STDERR,"%s: DimFact of %s set to %f.\n", 
         FuncName, SO->SurfCont->curColPlane->Name, SO->SurfCont->curColPlane->DimFact);
   
   SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */

   /* need to colorize plane */
   SUMA_ColorizePlane(SO->SurfCont->curColPlane);
   
   /* a good remix and redisplay */
   SUMA_RemixRedisplay (SO);
   
   /* update color label */
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURNe;
}
/*!
   \brief Function to set the color remix flag for surface SO and call a redisplay for relevant viewers 
*/
SUMA_Boolean SUMA_RemixRedisplay (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_RemixRedisplay"};
   DList *list=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   /* remix colors for all viewers displaying related surfaces */
   if (!SUMA_SetRemixFlag(SO->idcode_str, SUMAg_SVv, SUMAg_N_SVv)) {
      SUMA_SLP_Err("Failed in SUMA_SetRemixFlag.\n");
      SUMA_RETURN(NOPE);
   }

   /* redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_RedisplayNow_AllVisible, SES_Suma, NULL); 
   if (!SUMA_Engine(&list)) {
      SUMA_SLP_Err("Failed to redisplay.");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief callback to deal with show/hide colorplane toggle
   
   -expects SO in data
*/
void SUMA_cb_ColPlaneShow_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ColPlaneShow_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO || !SO->SurfCont) SUMA_RETURNe;
   if (!SO->SurfCont->curColPlane || !SO->SurfCont->ColPlaneShow_tb) SUMA_RETURNe;

   SUMA_LH("Getting State");
   SO->SurfCont->curColPlane->Show = XmToggleButtonGetState (SO->SurfCont->ColPlaneShow_tb);
   /* set the duplicate button next to int */
   SUMA_LH("Setting State of duplicate button");
   XmToggleButtonSetState (SO->SurfCont->Int_tb, SO->SurfCont->curColPlane->Show, NOPE);
   SUMA_LH("Updating color plane shells");
   SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */

   SUMA_RemixRedisplay(SO);
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURNe;
}

/*!
   \brief callback to deal with show only one colorplane toggle
   
   -expects SO in data
*/
void SUMA_cb_ColPlaneShowOne_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ColPlaneShowOne_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;

   SO->SurfCont->ShowCurOnly = XmToggleButtonGetState (SO->SurfCont->ColPlaneShowOne_tb);
   
   SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */

   SUMA_RemixRedisplay(SO);
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURNe;
}

int SUMA_ColPlaneShowOne_Set (SUMA_SurfaceObject *SO, SUMA_Boolean state) 
{
   static char FuncName[]={"SUMA_ColPlaneShowOne_Set"};
   
   SUMA_ENTRY;

   if (!SO->SurfCont) SUMA_RETURN(0);
   if (!SO->SurfCont->TopLevelShell) SUMA_RETURN(0);
   
   SO->SurfCont->ShowCurOnly = state;
   XmToggleButtonSetState (SO->SurfCont->ColPlaneShowOne_tb, SO->SurfCont->ShowCurOnly, NOPE);   
   
   SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */

   SUMA_RemixRedisplay(SO);
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURN(1);
   
}
/*!
 \brief Function based on arrow_time.c program from Motif Programing Manual
 
 change_value is called each time the timer expires.  This function
 * is also used to initiate the timer.  The "id" represents that timer
 * ID returned from the last call to XtAppAddTimeOut().  If id == 1,
 * the function was called from start_stop(), not a timeout.  If the value 
 * has reached its maximum or minimum, don't restart timer, just return.
 * If id == 1, this is the first timeout so make it be longer to allow
 * the user to release the button and avoid getting into the "speedy"
 * part of the timeouts.
 */
void SUMA_ATF_change_value(XtPointer client_data, XtIntervalId *id)
{
   static char FuncName[]={"SUMA_ATF_change_value"};
   float ArrowTolerance = 0.0001; /* roundoff and truncation headaches ... */
   int incr;
   SUMA_ARROW_TEXT_FIELD * AF= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   AF = (SUMA_ARROW_TEXT_FIELD *)client_data;
   
   if (!AF->wrap) {
      if (AF->value + AF->direction * AF->step > (AF->max + ArrowTolerance) ||
        AF->value + AF->direction * AF->step < (AF->min - ArrowTolerance) ) {
           SUMA_RETURNe;
      }
   }
   
   AF->value += AF->direction * AF->step;
   
   if (AF->wrap) SUMA_WRAP_VALUE(AF->value, AF->min, AF->max);
   
   /* round to the tolerance */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Pre Tolerance %f\n", FuncName, AF->value);
   /* if no negs allowed, take absolute value. Round off errors can cause AF->value to show -0.00000001 or something ugly like that*/
   if (AF->min >= 0.0 && AF->value < 0.0) AF->value = 0.0;
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Post Tolerance %f\n", FuncName, AF->value);

   SUMA_ATF_SetString (AF);

   AF->arrow_timer_id =
     XtAppAddTimeOut (SUMAg_CF->X->App, (int)id==1? 500 : 100, SUMA_ATF_change_value, (XtPointer)AF);
   
   /* turn off the modified field because it should only be on when the user edits the field */
   SUMA_RETURNe;
}

/*!
   \brief updates string based on ROI value in the Arrowfield 
*/
void SUMA_ATF_SetString (SUMA_ARROW_TEXT_FIELD * AF)
{
   static char FuncName[]={"SUMA_ATF_SetString"};
   char buf[36];
   
   SUMA_ENTRY;
   
   if (AF->type == SUMA_int) {
      sprintf (buf, "%-4d", (int)AF->value);
   }else if (AF->type == SUMA_float) {
      sprintf (buf, "%-4.4f", AF->value);
   }else {
      /* fair enough, must be stringy */
   }
   XtVaSetValues (AF->textfield, XmNvalue, buf, NULL);
   
   SUMA_RETURNe;
}

/*!
   \brief sets the value of Arrowfield based on string
*/
void SUMA_ATF_SetValue (SUMA_ARROW_TEXT_FIELD * AF)
{
   static char FuncName[]={"SUMA_ATF_SetValue"};
   float val;
   void *n = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
  
   XtVaGetValues (AF->textfield, XmNvalue, &n, NULL);
   /* YOU DO NOT WANT TO FREE n because n is not a 
      copy of the string in the widget! 
      Later in time:
      Hmmmm, maybe you do, maybe you do. Must abide by 
      upper case message. Must have crashed somwhere */
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Read %s\n", FuncName, (char *)n);
   
   val = strtod ((char *)n, NULL);
   if (errno) {
      /* bad syntax, reset value*/
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Bad syntax.\n", FuncName);
      SUMA_RegisterMessage (SUMAg_CF->MessageList, "Bad value in text field", FuncName, SMT_Error, SMA_Log);
      SUMA_ATF_SetString (AF);
   }else { 
      if (AF->type == SUMA_int) {
         AF->value = (int)val;    
         if (AF->wrap) {
            SUMA_WRAP_VALUE(AF->value, AF->min, AF->max);
         } else {
            SUMA_CLIP_VALUE(AF->value, AF->min, AF->max);
         }

         /* now call set string just to be sure users did not enter floats */
         SUMA_ATF_SetString (AF);
      } else {
         AF->value = val;
         if (AF->wrap) {
            SUMA_WRAP_VALUE(AF->value, AF->min, AF->max);
         } else {
            SUMA_CLIP_VALUE(AF->value, AF->min, AF->max);
         }
         /* It is still nice to call SetString because it puts the cursor at the beginning of the field */
         SUMA_ATF_SetString (AF);
         
      }
   }
   
   SUMA_RETURNe;
}

/*!
   \brief Callback for Group button
   -Expects sv in data
*/
void SUMA_cb_ViewerCont_SwitchGroup (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_ViewerCont_SwitchGroup"};
   SUMA_SurfaceViewer *sv=NULL;
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   sv = (SUMA_SurfaceViewer *)data;
   
   LW = sv->X->ViewCont->SwitchGrouplst;
   
   if (LW->ALS) {
      /* free that old hag */
      if (LocalHead) SUMA_S_Err("Freeing the hag.");
      LW->ALS = SUMA_FreeAssembleListStruct(LW->ALS);
   }
   
   /* assemble the ROI list */
   LW->ALS = SUMA_AssembleGroupList (sv);
  
   if (!LW->ALS) {
      SUMA_SLP_Err("Error assembling list.");
      SUMA_RETURNe;
   }
   
   if (LW->ALS->N_clist < 0) {
      SUMA_SL_Err("Failed in SUMA_AssembleGroupList");
      SUMA_RETURNe;
   }
   
   if (!LW->ALS->N_clist) {
      SUMA_SLP_Note ("No Groups to choose from.");
      SUMA_RETURNe;
   }
   
   SUMA_CreateScrolledList ( LW->ALS->clist, LW->ALS->N_clist, NOPE,
                             LW);
                             
   SUMA_RETURNe;
}
/*!
   \brief Callback for Group button
   -Expects sv in data
*/
void SUMA_cb_ViewerCont_SwitchState (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_ViewerCont_SwitchState"};
   SUMA_SurfaceViewer *sv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   sv = (SUMA_SurfaceViewer *)data;
   
   SUMA_SLP_Warn( "Not implemented yet.\n"
                  "Use ',' and '.' keys\n");
                  
   SUMA_RETURNe;
}

/*!
   \brief Callback for Switch Col Plane button
   -Expects SO in data
*/
void SUMA_cb_SurfCont_SwitchColPlane (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SurfCont_SwitchColPlane"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_SurfaceObject *SO = NULL;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SO = (SUMA_SurfaceObject *)data;
   
   SUMA_RefreshDsetList (SO);
                                                   
   SUMA_RETURNe;
}
/*!
   \brief Callback for Switch ROI button 
   
   -Expects LW in data
*/
void SUMA_cb_DrawROI_SwitchROI (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_DrawROI_SwitchROI"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_LIST_WIDGET *LW = NULL;
   
   SUMA_ENTRY;
   
   LW = (SUMA_LIST_WIDGET *)data;
   
   if (LW->ALS) {
      /* free that old hag */
      if (LocalHead) SUMA_S_Err("Freeing the hag.");
      LW->ALS = SUMA_FreeAssembleListStruct(LW->ALS);
   }
   /* assemble the ROI list */
   LW->ALS = SUMA_AssembleAllROIList (SUMAg_DOv, SUMAg_N_DOv, YUP);
  
   if (!LW->ALS) {
      SUMA_SLP_Err("Error assembling list.");
      SUMA_RETURNe;
   }
   
   if (LW->ALS->N_clist < 0) {
      SUMA_SL_Err("Failed in SUMA_AssembleAllROIList");
      SUMA_RETURNe;
   }
   
   if (!LW->ALS->N_clist) {
      SUMA_SLP_Note ("No ROIs to choose from.");
      SUMA_RETURNe;
   }
   SUMA_CreateScrolledList ( LW->ALS->clist, LW->ALS->N_clist, NOPE,
                             LW);
                                                
   SUMA_RETURNe;
}

/*!
   \brief Toggles the draw ROI mode
*/
void SUMA_cb_DrawROImode_toggled (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_DrawROImode_toggled"};
   
   SUMA_ENTRY;
   
   SUMAg_CF->ROI_mode = !SUMAg_CF->ROI_mode;
   
   /* take care of sensitivity of Pen button */
   if (!SUMAg_CF->ROI_mode) XtSetSensitive (SUMAg_CF->X->DrawROI->Penmode_tb, 0);
   else XtSetSensitive (SUMAg_CF->X->DrawROI->Penmode_tb, 1);

   SUMA_UpdateAllViewerCursor();
   
   SUMA_RETURNe;

}

/*!
   \brief Toggles the pen mode
*/

void SUMA_cb_DrawROIPen_toggled (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_DrawROIPen_toggled"};
   
   SUMA_ENTRY;
   
   SUMAg_CF->Pen_mode = !SUMAg_CF->Pen_mode;
   
   SUMA_UpdateAllViewerCursor();
   
   SUMA_RETURNe;

}


/*!
   \brief Toggles the Afni link mode
*/
void SUMA_cb_AfniLink_toggled (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_AfniLink_toggled"};
   DList *list=NULL;
   SUMA_STANDARD_CMAP cmap;
   SUMA_EngineData *ED = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMAg_CF->ROI2afni = !SUMAg_CF->ROI2afni;
   
   /* make sure that is OK */
   /* Same here, need to handle SUMA_TO_MATLAB_STREAM_INDEX */
   if (SUMAg_CF->ROI2afni && !SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX]) {
      SUMAg_CF->ROI2afni = NOPE;
      
      SUMA_SLP_Err(  "Cannot link to Afni.\n"
                     "No connection found.");
      XmToggleButtonSetState (SUMAg_CF->X->DrawROI->AfniLink_tb, SUMAg_CF->ROI2afni, NOPE);
   }
   
   if (SUMAg_CF->ROI2afni) {
      if (SUMAg_CF->ROI_CM) {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Sending cmap (%s)\n",
            FuncName,  SUMAg_CF->ROI_CM->Name);
            SUMA_LH("Sending colormap to afni ...");
         /* send the color map for ROI to afni */
         cmap = SUMA_StandardMapCode (SUMAg_CF->ROI_CM->Name);
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Sending cmap %d (%s)\n",
            FuncName, cmap, SUMAg_CF->ROI_CM->Name);
         list = SUMA_CreateList();
         ED = SUMA_InitializeEngineListData (SE_SendColorMapToAfni);
         if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                SEF_i, (void*)&cmap, 
                                                SES_SumaWidget, NULL, NOPE, 
                                                SEI_Head, NULL )) {
            fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
            SUMA_RETURNe;
         }
         SUMA_LH("NOW!");
         if (!SUMA_Engine (&list)) {
            fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         }   
      }
   }
   SUMA_RETURNe;

}



/*! 
   \brief handles a selection from switch ColPlane 
   
   -expect SO in data
*/
void SUMA_cb_SelectSwitchColPlane(Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_SelectSwitchColPlane"};
   SUMA_LIST_WIDGET *LW = NULL;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   char *choice=NULL, *choice_trimmed=NULL;
   SUMA_Boolean CloseShop = NOPE, Found = NOPE;
   int ichoice = -1;
   SUMA_OVERLAYS *ColPlane = NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   SO = (SUMA_SurfaceObject *)data;
   LW = SO->SurfCont->SwitchDsetlst;
   
   if (!LW) {
      SUMA_S_Err("NULL LW!");
      SUMA_RETURNe;
   }

   #if 0
   if (cbs->reason == XmCR_SINGLE_SELECT) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Single selection, list widget %s... \n", FuncName, LW->Label);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Default selection, list widget %s... \n", FuncName, LW->Label);
      /*double click or enter on that one, close shop after selection */
      CloseShop = YUP;
   }

   XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Selected item: %s {%s} (%d)\n", FuncName, choice, choice, cbs->item_position);
   LW->lastitempos = cbs->item_position;   /* store for next opening */
   
   /* because of sorting, choice cannot be used as an index into clist and oplist in ALS */
   Found = NOPE;
   ichoice = 0;
   do {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Comparing:\t>%s<\t>%s<", FuncName, LW->ALS->clist[ichoice], choice);
      if (strncmp(LW->ALS->clist[ichoice], choice, strlen(LW->ALS->clist[ichoice])) == 0) Found = YUP; 
      else ++ichoice;
   } while (ichoice < LW->ALS->N_clist && !Found);
   
   if (!Found) {
      SUMA_SLP_Err("Choice not found.");
      SUMA_RETURNe;
   }
   
   XtFree (choice);
   
   #else
   /* the new way */
   ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop);
   #endif
   /* now retrieve that choice from the SUMA_ASSEMBLE_LIST_STRUCT structure and initialize the drawing window */
   if (LW->ALS) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: N_clist = %d\n", FuncName, LW->ALS->N_clist); 
      if (LW->ALS->N_clist > ichoice) {
         ColPlane = (SUMA_OVERLAYS *)LW->ALS->oplist[ichoice];
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Retrieved ColPlane named %s\n", FuncName, ColPlane->Name);
         SUMA_InitializeColPlaneShell(SO, ColPlane);
         SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */
         /* If you're viewing one plane at a time, do a remix */
         if (SO->SurfCont->ShowCurOnly) SUMA_RemixRedisplay(SO);

      }
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: NULL ALS\n", FuncName); 
   }

   if (CloseShop) {
      SUMA_cb_CloseSwitchColPlane( w,  (XtPointer)SO->SurfCont->SwitchDsetlst,  call_data);
   }  
   
   
   SUMA_RETURNe;
}

/*!
   \brief Closes the DrawROI window 
   
   -expects SUMA_LIST_WIDGET * in client_data
*/
void SUMA_cb_CloseSwitchColPlane(Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_CloseSwitchColPlane"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_LIST_WIDGET *LW = NULL;
   
   SUMA_ENTRY;

   LW = (SUMA_LIST_WIDGET *)data;
   
   #if defined SUMA_USE_WITHDRAW 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing list widget %s...\n", FuncName, LW->Label);
      
      XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
         XtWindow(LW->toplevel),
         XScreenNumberOfScreen(XtScreen(LW->toplevel)));
   #elif defined SUMA_USE_DESTROY 
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying list widget %s...\n", FuncName, LW->Label);
         XtDestroyWidget(LW->toplevel);
         LW->toplevel = NULL;
   #endif
   
   LW->isShaded = YUP; 

   SUMA_RETURNe;
}

/*!
   \brief default selection action, handles single selection mode
   - expects sv in data
*/
void SUMA_cb_SelectSwitchGroup(Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_SelectSwitchGroup"};
   SUMA_LIST_WIDGET *LW = NULL;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   char *choice=NULL;
   SUMA_Boolean CloseShop = NOPE, Found = NOPE;
   int ichoice = -1;
   char *strn=NULL;
   SUMA_SurfaceViewer *sv=NULL;
   SUMA_Boolean LocalHead = NOPE;
 
   SUMA_ENTRY;
   
   sv = (SUMA_SurfaceViewer *)data;
   
   LW = sv->X->ViewCont->SwitchGrouplst;
   
   if (!LW) {
      SUMA_S_Err("NULL LW!");
      SUMA_RETURNe;
   }

   #if 0
   if (cbs->reason == XmCR_SINGLE_SELECT) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Single selection, list widget %s... \n", FuncName, LW->Label);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Default selection, list widget %s... \n", FuncName, LW->Label);
      /*double click or enter on that one, close shop after selection */
      CloseShop = YUP;
   }
   
   XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Selected item: %s (%d)\n", FuncName, choice, cbs->item_position);
   LW->lastitempos = cbs->item_position;   /* store for next opening */
 
   /* because of sorting, choice cannot be used as an index into clist and oplist in ALS */
   Found = NOPE;
   ichoice = 0;
   do {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Comparing:\t>%s<\t>%s<", FuncName, LW->ALS->clist[ichoice], choice);
      if (strncmp(LW->ALS->clist[ichoice], choice, strlen(LW->ALS->clist[ichoice])) == 0) Found = YUP; 
      else ++ichoice;
   } while (ichoice < LW->ALS->N_clist && !Found);
   
   if (!Found) {
      SUMA_SLP_Err("Choice not found.");
      SUMA_RETURNe;
   }
   
   XtFree (choice);
   #else
   /* the new way */
   ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop);
   #endif

   /* now retrieve that choice from the SUMA_ASSEMBLE_LIST_STRUCT structure and initialize the drawing window */
   if (LW->ALS) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: N_clist = %d\n", FuncName, LW->ALS->N_clist); 
      if (LW->ALS->N_clist > ichoice) {
         strn = (char *)LW->ALS->clist[ichoice];
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Retrieved group labeled %s\n", FuncName, strn);
         /* Now we know what group the user wants so go switch groups */
         if (!SUMA_SwitchGroups(sv, strn)) { 
            SUMA_SLP_Err("Failed to switch groups");
         }
      }
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: NULL ALS\n", FuncName); 
   }

   /* Now make the viewer switch group */
   
   if (CloseShop) {
      SUMA_LH("Closing Shop");
      SUMA_cb_CloseSwitchGroup( w,  data,  call_data);
   }  
   
   SUMA_RETURNe;
}

/*!
   \brief Closes the SwitchGroup window 
   
   -expects sv in client_data
*/
void SUMA_cb_CloseSwitchGroup(Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_CloseSwitchGroup"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_SurfaceViewer *sv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   sv = (SUMA_SurfaceViewer *)data;
   
   LW = sv->X->ViewCont->SwitchGrouplst;
   
   #if defined SUMA_USE_WITHDRAW 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing list widget %s...\n", FuncName, LW->Label);
      
      XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
         XtWindow(LW->toplevel),
         XScreenNumberOfScreen(XtScreen(LW->toplevel)));
   #elif defined SUMA_USE_DESTROY 
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying list widget %s...\n", FuncName, LW->Label);
         XtDestroyWidget(LW->toplevel);
         LW->toplevel = NULL;
   #endif
   
   LW->isShaded = YUP; 

   SUMA_RETURNe;
}

/*!
   \brief default selection action, handles single selection mode
   
   -code snipets from Motif Programming Manual
*/
void SUMA_cb_SelectSwitchROI(Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_SelectSwitchROI"};
   SUMA_LIST_WIDGET *LW = NULL;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   char *choice=NULL;
   SUMA_Boolean CloseShop = NOPE, Found = NOPE;
   int ichoice = -1;
   SUMA_DRAWN_ROI *DrawnROI = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   LW = (SUMA_LIST_WIDGET *)data;
   
   if (!LW) {
      SUMA_S_Err("NULL LW!");
      SUMA_RETURNe;
   }

   #if 0   
   if (cbs->reason == XmCR_SINGLE_SELECT) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Single selection, list widget %s... \n", FuncName, LW->Label);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Default selection, list widget %s... \n", FuncName, LW->Label);
      /*double click or enter on that one, close shop after selection */
      CloseShop = YUP;
   }

   XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Selected item: %s (%d)\n", FuncName, choice, cbs->item_position);
   LW->lastitempos = cbs->item_position;   /* store for next opening */
 
   /* because of sorting, choice cannot be used as an index into clist and oplist in ALS */
   Found = NOPE;
   ichoice = 0;
   do {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Comparing:\t>%s<\t>%s<", FuncName, LW->ALS->clist[ichoice], choice);
      if (strncmp(LW->ALS->clist[ichoice], choice, strlen(LW->ALS->clist[ichoice])) == 0) Found = YUP; 
      else ++ichoice;
   } while (ichoice < LW->ALS->N_clist && !Found);
   
   if (!Found) {
      SUMA_SLP_Err("Choice not found.");
      SUMA_RETURNe;
   }
   
   XtFree (choice);
   #else
   /* the new way */
   ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop);
   #endif

   /* now retrieve that choice from the SUMA_ASSEMBLE_LIST_STRUCT structure and initialize the drawing window */
   if (LW->ALS) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: N_clist = %d\n", FuncName, LW->ALS->N_clist); 
      if (LW->ALS->N_clist > ichoice) {
         DrawnROI = (SUMA_DRAWN_ROI *)LW->ALS->oplist[ichoice];
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Retrieved DrawnROI labeled %s\n", FuncName, DrawnROI->Label);
         SUMA_InitializeDrawROIWindow(DrawnROI);
      }
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: NULL ALS\n", FuncName); 
   }

   if (CloseShop) {
      SUMA_cb_CloseSwitchROI( w,  data,  call_data);
   }  
   
   SUMA_RETURNe;
}

/*!
   \brief Closes the SwitchROI window 
   
   -expects SUMA_LIST_WIDGET * in client_data
*/
void SUMA_cb_CloseSwitchROI(Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_CloseSwitchROI"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_LIST_WIDGET *LW = NULL;
   
   SUMA_ENTRY;

   LW = (SUMA_LIST_WIDGET *)data;
   
   #if defined SUMA_USE_WITHDRAW 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing list widget %s...\n", FuncName, LW->Label);
      
      XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
         XtWindow(LW->toplevel),
         XScreenNumberOfScreen(XtScreen(LW->toplevel)));
   #elif defined SUMA_USE_DESTROY 
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying list widget %s...\n", FuncName, LW->Label);
         XtDestroyWidget(LW->toplevel);
         LW->toplevel = NULL;
   #endif
   
   LW->isShaded = YUP; 

   SUMA_RETURNe;
}
/*!
   \brief Closes the DrawROI window
*/
void SUMA_cb_CloseDrawROIWindow(Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_CloseDrawROIWindow"};
   SUMA_Boolean Shaded = NOPE, LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->X->DrawROI->AppShell) SUMA_RETURNe;
   
   /* if the ROI selection list is open, close it */
   /* Close the ROIlist window if it is open */
   SUMA_IS_DRAW_ROI_SWITCH_ROI_SHADED(Shaded);
   if (!Shaded) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Closing switch ROI window ...\n", FuncName);
      SUMA_cb_CloseSwitchROI(NULL, (XtPointer) SUMAg_CF->X->DrawROI->SwitchROIlst, NULL);
   }
   
   #if defined SUMA_USE_WITHDRAW 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing DrawROI window...\n", FuncName);
      
      XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
         XtWindow(SUMAg_CF->X->DrawROI->AppShell),
         XScreenNumberOfScreen(XtScreen(SUMAg_CF->X->DrawROI->AppShell)));
   #elif defined SUMA_USE_DESTROY 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying DrawROI window...\n", FuncName);
      XtDestroyWidget(SUMAg_CF->X->DrawROI->AppShell);
      SUMAg_CF->X->DrawROI->AppShell = NULL;
   #endif
   
   SUMA_RETURNe;
}
/*!
   \brief creates the SUMA controller window. Expects sv  input
*/
void SUMA_cb_createSumaCont(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSumaCont"};
   Widget rc, pb_close, pb_new, pb_done, pb_bhelp, LockFrame, AppFrame, form, tb, rb, rc_m;
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SUMAg_CF->X->SumaCont->AppShell) {
      fprintf (SUMA_STDERR,"Error %s: SUMAg_CF->X->SumaCont->AppShell!=NULL. Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }

   /* create as a separate application shell, you do not want a parent to this controller that
   can be closed or withdrawn temporarily */
   SUMAg_CF->X->SumaCont->AppShell = XtVaAppCreateShell("Suma Controller" , "Suma" ,
      topLevelShellWidgetClass , SUMAg_CF->X->DPY_controller1 ,
      NULL ) ;
   
  
   /* turn off default delete response. If you do not do that, you will suffer.*/
   XtVaSetValues( SUMAg_CF->X->SumaCont->AppShell,
           XmNdeleteResponse, XmDO_NOTHING,
           NULL);  
             
   /* handle the close button from window manager */
   XmAddWMProtocolCallback(/* make "Close" window menu work */
      SUMAg_CF->X->SumaCont->AppShell,
      XmInternAtom( SUMAg_CF->X->DPY_controller1 , "WM_DELETE_WINDOW" , False ) ,
      SUMA_cb_closeSumaCont, NULL) ;
   
   /* create a form widget, manage it at the end ...*/
   form = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SUMAg_CF->X->SumaCont->AppShell,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
      
   /* a LockFrame to put the lockstuff in */
   LockFrame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, form,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_FORM ,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL); 
   
      /* this one requires Motif 1.2 or newer */
      XtVaCreateManagedWidget ("Lock",
         xmLabelWidgetClass, LockFrame, 
         XmNchildType, XmFRAME_TITLE_CHILD,
         XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
         NULL);
   
   /* row column Lock rowcolumns */
   rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, LockFrame,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight, SUMA_MARGIN ,
         XmNmarginWidth , SUMA_MARGIN ,
         NULL);

   for (i = 0; i < SUMA_MAX_SURF_VIEWERS; i++) {
      char stmp[3];
      int tmpfac;
      
      rc_m = XtVaCreateManagedWidget ("rowcolumn",
         xmRowColumnWidgetClass, rc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmVERTICAL ,
         NULL);
         
      sprintf(stmp,"%c", 65+i);
      w = XtVaCreateManagedWidget (stmp,
         xmLabelWidgetClass, rc_m,
         NULL);

      SUMAg_CF->X->SumaCont->Lock_rbg->rb[i] = XtVaCreateWidget("radiobox",
         xmRowColumnWidgetClass, rc_m,
         XmNorientation , XmVERTICAL ,
         XmNpacking,      XmPACK_TIGHT,
         XmNradioBehavior, True,
         XmNnumColumns,   1,
         NULL); 
      
      tmpfac = SUMAg_CF->X->SumaCont->Lock_rbg->N_but;
       
      SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i] = XtVaCreateManagedWidget("-", 
      xmToggleButtonWidgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i], 
                     XmNvalueChangedCallback, SUMA_cb_XHlock_toggled, 
                     (XtPointer)(tmpfac*i));
       
      SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+1] = XtVaCreateManagedWidget("i", 
      xmToggleButtonWidgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+1], 
                     XmNvalueChangedCallback, SUMA_cb_XHlock_toggled,  
                     (XtPointer)(tmpfac*i+1));
      
      SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+2] = XtVaCreateManagedWidget("c", 
      xmToggleButtonWidgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+2], 
                     XmNvalueChangedCallback, SUMA_cb_XHlock_toggled,  
                     (XtPointer)(tmpfac*i+2));
      
      XtManageChild (SUMAg_CF->X->SumaCont->Lock_rbg->rb[i]);
      
      /* put some help on the radiobox and its children*/
      MCW_reghelp_children( SUMAg_CF->X->SumaCont->Lock_rbg->rb[i] , SUMA_LockSumaCont_help );
      
      /* initialize radio button created */
      SUMA_set_Lock_rb (SUMAg_CF->X->SumaCont->Lock_rbg, i, SUMAg_CF->Locked[i]);
      
      XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rc_m, NULL);
      
      SUMAg_CF->X->SumaCont->LockView_tbg[i] = XtVaCreateManagedWidget("v", 
         xmToggleButtonWidgetClass, rc_m, NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->LockView_tbg[i], XmNvalueChangedCallback, SUMA_cb_XHviewlock_toggled, (XtPointer) i);

      /* put some help on this buton*/
      MCW_reghelp_children( rc_m , SUMA_LockViewSumaCont_help );
            
   }  
   XtManageChild (rc);
   XtManageChild (LockFrame);
      
   
   /* a vertical separator */
   XtVaCreateManagedWidget ("sep", 
                           xmSeparatorGadgetClass, rc, 
                           XmNorientation, XmVERTICAL,
                           NULL);

   /* a radio box for the all buttons */
   rc_m = XtVaCreateManagedWidget ("rowcolumn",
         xmRowColumnWidgetClass, rc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmVERTICAL ,
         NULL);
         
   w = XtVaCreateManagedWidget ("All",
      xmLabelWidgetClass, rc_m,
      NULL);

   SUMAg_CF->X->SumaCont->Lock_rbg->arb = XtVaCreateWidget("radiobox",
      xmRowColumnWidgetClass, rc_m,
      XmNorientation , XmVERTICAL ,
      XmNpacking,      XmPACK_TIGHT,
      XmNradioBehavior, True,
      XmNnumColumns,   1,
      NULL);

   SUMAg_CF->X->SumaCont->Lock_rbg->atb[0] = XtVaCreateManagedWidget("-", 
   xmToggleButtonWidgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->arb, NULL);
   XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->atb[0], 
                  XmNvalueChangedCallback, SUMA_cb_XHalock_toggled, 
                  (XtPointer)(0));

   SUMAg_CF->X->SumaCont->Lock_rbg->atb[1] = XtVaCreateManagedWidget("i", 
   xmToggleButtonWidgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->arb, NULL);
   XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->atb[1], 
                  XmNvalueChangedCallback, SUMA_cb_XHalock_toggled,  
                  (XtPointer)(1));

   SUMAg_CF->X->SumaCont->Lock_rbg->atb[2] = XtVaCreateManagedWidget("c", 
   xmToggleButtonWidgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->arb, NULL);
   XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->atb[2], 
                  XmNvalueChangedCallback, SUMA_cb_XHalock_toggled,  
                  (XtPointer)(2));

   XtManageChild (SUMAg_CF->X->SumaCont->Lock_rbg->arb);

   /* put some help on the radiobox and its children*/
   MCW_reghelp_children( SUMAg_CF->X->SumaCont->Lock_rbg->arb , SUMA_LockSumaCont_help );

   /* initialize radio button created */
   SUMA_set_Lock_arb (SUMAg_CF->X->SumaCont->Lock_rbg);   
         
   XtVaCreateManagedWidget ("sep", xmSeparatorGadgetClass, rc_m, NULL);
      
   SUMAg_CF->X->SumaCont->LockAllView_tb = XtVaCreateManagedWidget("v", 
      xmToggleButtonWidgetClass, rc_m, NULL);
   XtAddCallback (SUMAg_CF->X->SumaCont->LockAllView_tb, XmNvalueChangedCallback, SUMA_cb_XHaviewlock_toggled, NULL);
   
   /* a frame to put the Close button in */
   AppFrame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, form,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_WIDGET ,
      XmNtopWidget, LockFrame,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL); 
   
   
   rc = XtVaCreateManagedWidget ("rowcolumn",
         xmRowColumnWidgetClass, AppFrame,
         XmNpacking, XmPACK_COLUMN, 
         XmNorientation , XmVERTICAL ,
         XmNnumColumns, 2, 
         NULL);
         
   pb_new = XtVaCreateWidget ("Viewer", 
      xmPushButtonWidgetClass, rc, 
      NULL);
   XtAddCallback (pb_new, XmNactivateCallback, SUMA_cb_newSumaCont, NULL);
   MCW_register_hint( pb_new , "Opens a new viewer" ) ;
   MCW_register_help( pb_new , SUMA_viewerSumaCont_help );
   XtManageChild (pb_new); 

   pb_close = XtVaCreateWidget ("Close", 
      xmPushButtonWidgetClass, rc, 
      NULL);   
   XtAddCallback (pb_close, XmNactivateCallback, SUMA_cb_closeSumaCont, NULL);
   MCW_register_hint( pb_close , "Close SUMA controller" ) ;
   MCW_register_help( pb_close , SUMA_closeSumaCont_help ) ;
   XtManageChild (pb_close); 
   
   pb_bhelp = XtVaCreateWidget ("BHelp", 
      xmPushButtonWidgetClass, rc, 
      NULL);
   XtAddCallback (pb_bhelp, XmNactivateCallback, MCW_click_help_CB, NULL);
   MCW_register_help(pb_bhelp , SUMA_help_help ) ;
   MCW_register_hint(pb_bhelp  , "Press this button then click on a button/label/menu for more help." ) ;
   
   XtManageChild (pb_bhelp); 
   
   SUMAg_CF->X->SumaCont->quit_pb = XtVaCreateWidget ("done", 
      xmPushButtonWidgetClass, rc, 
      NULL);
   XtAddCallback (SUMAg_CF->X->SumaCont->quit_pb, XmNactivateCallback, SUMA_cb_doneSumaCont, NULL);
   MCW_register_hint( SUMAg_CF->X->SumaCont->quit_pb , "Click twice in 5 seconds to quit application." ) ;
   MCW_set_widget_bg( SUMAg_CF->X->SumaCont->quit_pb , MCW_hotcolor(SUMAg_CF->X->SumaCont->quit_pb) , 0 ) ;

   XtManageChild (SUMAg_CF->X->SumaCont->quit_pb); 
  
   XtManageChild (AppFrame);
   
   /* manage the remaing widgets */
   XtManageChild (form);
   
   /* realize the widget */
   XtRealizeWidget (SUMAg_CF->X->SumaCont->AppShell);
   
   SUMA_RETURNe;
}

/*!
   \brief Close all viewers and exit SUMA
   
   based on afni's AFNI_quit_CB
*/

void  SUMA_cb_doneSumaCont(Widget wcall, XtPointer cd1, XtPointer cbs)
{
   static char FuncName[] = {"SUMA_cb_doneSumaCont"};
   XmPushButtonCallbackStruct * pbcbs = (XmPushButtonCallbackStruct *) cbs ;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* NULL widget --> reset button to lowercase */
   if( wcall == NULL ){
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Resetting button.\n", FuncName); 
      if( SUMAg_CF->X->SumaCont->quit_first == NOPE ){
         MCW_set_widget_label( SUMAg_CF->X->SumaCont->quit_pb , "done " ) ;
         SUMAg_CF->X->SumaCont->quit_first = YUP ;
      }
      SUMA_RETURNe ;
   }
   
   /* Press of button with Shift or Control key pressed --> Death Now */
   if( pbcbs != NULL                       &&
       pbcbs->event != NULL                &&
       pbcbs->event->type == ButtonRelease &&
       ((XButtonEvent *)(pbcbs->event))->state &  /* note single & here! */
       (ShiftMask|ControlMask|Button2Mask|Button3Mask) ){

      if (LocalHead) fprintf (SUMA_STDERR, "%s: Closing display.\n", FuncName); 
      XtCloseDisplay( SUMAg_CF->X->DPY_controller1 ) ;
      exit(0) ;
   }
   
   /* First press --> just change button label */

   if( SUMAg_CF->X->SumaCont->quit_first ){
      MCW_set_widget_label( SUMAg_CF->X->SumaCont->quit_pb , "DONE " ) ;
      SUMAg_CF->X->SumaCont->quit_first = NOPE ;

      /* if not re-pressed in 5 seconds, will reset to lowercase */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: First Press, adding time out.\n", FuncName);
      (void) XtAppAddTimeOut(
               XtWidgetToApplicationContext(SUMAg_CF->X->SumaCont->quit_pb) ,
               5000 , SUMA_quit_timeout_CB , NULL ) ;

       SUMA_RETURNe;
   }
   
   /* close up */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Closing shop...\n", FuncName);
   XtCloseDisplay( SUMAg_CF->X->DPY_controller1 ) ;
   exit(0) ;

   SUMA_RETURNe;
}
void SUMA_quit_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   static char FuncName[] = {"SUMA_quit_timeout_CB"};

   SUMA_ENTRY;

   SUMA_cb_doneSumaCont(NULL, NULL, NULL);

   SUMA_RETURNe; 
}

void SUMA_cb_XHlock_toggled(Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_XHlock_toggled"};
   SUMA_Boolean LocalHead = NOPE;
   int cd, i, j;
   
   cd = (int) client_data;
   
   SUMA_ENTRY;

   i = cd / SUMAg_CF->X->SumaCont->Lock_rbg->N_but;
   j = cd % SUMAg_CF->X->SumaCont->Lock_rbg->N_but;
   fprintf (SUMA_STDERR, "%s: Viewer %c Lock=%d.\n", FuncName, 65+i, j);
   SUMAg_CF->Locked[i] = j;
   
   /* now call the function to set the All lock buttons */
   SUMA_set_Lock_arb (SUMAg_CF->X->SumaCont->Lock_rbg);

   SUMA_RETURNe;
}

void SUMA_cb_XHalock_toggled (Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_XHalock_toggled"};
   int i;
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   
   SUMA_ENTRY;
 
   i = (int) client_data;
   

   list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_SetLockAllCrossHair);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&i, 
                                          SES_SumaWidget, NULL, NOPE, 
                                          SEI_Head, NULL )) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      SUMA_RETURNe;
   }
   
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
                  
   SUMA_RETURNe;
}

void SUMA_cb_XHaviewlock_toggled (Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_XHaviewlock_toggled"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;

   SUMA_ENTRY;
   
   list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_ToggleLockAllViews);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_Empty, NULL, 
                                          SES_SumaWidget, NULL, NOPE, 
                                          SEI_Head, NULL )) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      SUMA_RETURNe;
   }
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
  
   
   SUMA_RETURNe;   
   
}

void SUMA_cb_XHviewlock_toggled(Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_XHviewlock_toggled"};
   SUMA_Boolean LocalHead = NOPE;
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   int i = (int) client_data;
   
   SUMA_ENTRY;
   
   list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_ToggleLockView);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&i, 
                                          SES_SumaWidget, NULL, NOPE, 
                                          SEI_Head, NULL )) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      SUMA_RETURNe;
   }
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
   SUMA_RETURNe;
}

/*!
   \brief open a new viewer, expects nothing in data
*/

void SUMA_cb_newSumaCont(Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_newSumaCont"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   fprintf(SUMA_STDOUT, "%s: Opening a new controller...\n", FuncName);
   /* open a new controller */
   if (!SUMA_X_SurfaceViewer_Create ()) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_X_SurfaceViewer_Create.\n", FuncName);
      SUMA_RETURNe;
   } 
   
   SUMA_RETURNe;
}
/*!
   \brief Close the suma controller, expects nothing in data
*/
void SUMA_cb_closeSumaCont(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_closeSumaCont"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->X->SumaCont->AppShell) SUMA_RETURNe;
   
   #ifdef SUMA_USE_WITHDRAW 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing Suma Controller...\n", FuncName);
      
      XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
         XtWindow(SUMAg_CF->X->SumaCont->AppShell),
         XScreenNumberOfScreen(XtScreen(SUMAg_CF->X->SumaCont->AppShell)));
   #endif
   #ifdef SUMA_USE_DESTROY 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying Suma Controller...\n", FuncName);
      XtDestroyWidget(SUMAg_CF->X->SumaCont->AppShell);
      SUMAg_CF->X->SumaCont->AppShell = NULL;
   #endif
   
   SUMA_RETURNe;

}

/*! 

   \brief climb widget tree until we get to the top.  Return the Shell 
   tw = SUMA_GetTopShell(w);
   
   \param w (Widget) widget for which the top widget is sought
   \return tw (Widget) top widget
   
 * Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * see full notice in the beginning of this file
   
*/
Widget SUMA_GetTopShell(Widget w)
{
    while (w && !XtIsWMShell (w))
        w = XtParent (w);
    return w;
}

/*!
   \brief Sets a button on a radio box
*/

void SUMA_set_Lock_rb (SUMA_rb_group * Lock_rbg, int irb, int but)
{
   static char FuncName[] = {"SUMA_set_Lock_rb"};
   SUMA_Boolean LocalHead = NOPE;
   Widget w;
   int i, itb, ifb;
   
   SUMA_ENTRY;
      
   ifb = irb*Lock_rbg->N_but; /* index of first button in radio box irb */
   itb = ifb+but; /* index of button to modify */
   
   i = 0;
   while (i<Lock_rbg->N_but) {
      /* get the widget of the button in question */
      w = Lock_rbg->tb[ifb+i];
      if (!w) SUMA_RETURNe; /* this happens before opening the SUMA controller */
      if ( (ifb + i) == itb) XmToggleButtonSetState (w, YUP, NOPE);
       else XmToggleButtonSetState (w, NOPE, NOPE);
      ++i;
   }
   
   SUMA_RETURNe;   

}  
      
void SUMA_set_Lock_arb (SUMA_rb_group * Lock_rbg)
{
   static char FuncName[] = {"SUMA_set_Lock_arb"};
   int i, sumlock;
   
   SUMA_ENTRY;
   
   if (!Lock_rbg->atb[0]) SUMA_RETURNe;
   /* find out if all buttons are set to the same value */
   sumlock = 0;
   for (i=0; i < SUMA_MAX_SURF_VIEWERS; ++i) {
      sumlock += SUMAg_CF->Locked[i];
   } 
   
   if (sumlock == SUMA_MAX_SURF_VIEWERS * SUMA_No_Lock) { /* all no lock */
         XmToggleButtonSetState (Lock_rbg->atb[0], YUP, NOPE);
         XmToggleButtonSetState (Lock_rbg->atb[1], NOPE, NOPE);
         XmToggleButtonSetState (Lock_rbg->atb[2], NOPE, NOPE);
   }else if (sumlock == SUMA_MAX_SURF_VIEWERS * SUMA_I_Lock) {
         XmToggleButtonSetState (Lock_rbg->atb[0], NOPE, NOPE);
         XmToggleButtonSetState (Lock_rbg->atb[1], YUP, NOPE);
         XmToggleButtonSetState (Lock_rbg->atb[2], NOPE, NOPE);
   }else if (sumlock == SUMA_MAX_SURF_VIEWERS * SUMA_XYZ_Lock) {
         XmToggleButtonSetState (Lock_rbg->atb[0], NOPE, NOPE);
         XmToggleButtonSetState (Lock_rbg->atb[1], NOPE, NOPE);
         XmToggleButtonSetState (Lock_rbg->atb[2], YUP, NOPE);
   }else {
         XmToggleButtonSetState (Lock_rbg->atb[0], NOPE, NOPE);
         XmToggleButtonSetState (Lock_rbg->atb[1], NOPE, NOPE);
         XmToggleButtonSetState (Lock_rbg->atb[2], NOPE, NOPE);
   }
   
   SUMA_RETURNe;   

}

void SUMA_set_LockView_atb (void)
{
   static char FuncName[] = {"SUMA_set_LockView_atb"};
   int i, sumlock;
   
   SUMA_ENTRY;
   
   /* find out if all buttons are set to the same value */
   sumlock = 0;
   for (i=0; i < SUMA_MAX_SURF_VIEWERS; ++i) {
      sumlock += SUMAg_CF->ViewLocked[i];
   } 

   if (sumlock == SUMA_MAX_SURF_VIEWERS) { /* all locked */
      XmToggleButtonSetState (SUMAg_CF->X->SumaCont->LockAllView_tb, YUP, NOPE);
   }else if (sumlock == NOPE) { /* none locked */
      XmToggleButtonSetState (SUMAg_CF->X->SumaCont->LockAllView_tb, NOPE, NOPE);
   }else {/* a mix and match */
      /* do nothing for now */
   }
   
   SUMA_RETURNe;   
}

/*!
   \brief opens a text window with information about the surface viewer  
   -expects sv pointer in userdata
   Feb 23 04: Now requiring sv in client_data. Other widgets will be calling
   this function too so w should not be used
*/
void SUMA_cb_moreViewerInfo (Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_moreViewerInfo"};
   SUMA_SurfaceViewer *sv=NULL;
   void *n=NULL;
   char *s = NULL, stmp[100];
   SUMA_Boolean LocalHead = NOPE;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell = NULL;
   int isv;
   
   SUMA_ENTRY;
   
   #if 0
   /* The userdata way , 
   not setup for all widgets calling this function .... */
   XtVaGetValues (w,
                  XmNuserData, &n,
                  NULL);
   sv = (SUMA_SurfaceViewer *)n;
   #else
   /* the good old way, Feb 23 04 */          
   sv = (SUMA_SurfaceViewer *)client_data;
   #endif
   
   isv = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);

   /* check to see if window is already open, if it is, just raise it */
   if (sv->X->ViewCont->ViewerInfo_TextShell) {
      XRaiseWindow (SUMAg_CF->X->DPY_controller1, XtWindow(sv->X->ViewCont->ViewerInfo_TextShell->toplevel));
      SUMA_RETURNe;
   }
   
   /* for the string of the surface info */
   s = SUMA_SurfaceViewer_StructInfo(sv, 1);
   
   #if 1
   if (s) {
      TextShell =  SUMA_CreateTextShellStruct (
                                 SUMA_ViewerInfo_open, 
                                 (void *)sv, 
                                 SUMA_ViewerInfo_destroyed, 
                                 (void *)sv);
      if (!TextShell) {
         fprintf (SUMA_STDERR, 
                  "Error %s: Failed in SUMA_CreateTextShellStruct.\n",
                   FuncName);
         SUMA_RETURNe;
      }
      sprintf(stmp, "[%c] Viewer Info", 65+isv);
      sv->X->ViewCont->ViewerInfo_TextShell = 
            SUMA_CreateTextShell(s, stmp, TextShell);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_SurfaceViewer_StructInfo.\n",
               FuncName);
   }   
   #else 
      fprintf (SUMA_STDERR, "%s\n", s);
      SUMA_free(s); 
   #endif

    
   SUMA_RETURNe;
}
/*!
   \brief Function called when Surface Info window is open
*/
void SUMA_ViewerInfo_open (void *p) 
{
   static char FuncName[] = {"SUMA_ViewerInfo_open"};
   SUMA_SurfaceViewer *sv= NULL;
  
   SUMA_ENTRY;
   
   sv = (SUMA_SurfaceViewer *)p;
   MCW_invert_widget (sv->X->ViewCont->ViewerInfo_pb);
   
   
   SUMA_RETURNe;
}

/*!
   \brief Function called when Viewer Info window is destroyed
*/
void SUMA_ViewerInfo_destroyed (void *p) 
{
   static char FuncName[] = {"SUMA_ViewerInfo_destroyed"};
   SUMA_SurfaceViewer *sv= NULL;
   
   SUMA_ENTRY;

   sv = (SUMA_SurfaceViewer *)p;
   MCW_invert_widget (sv->X->ViewCont->ViewerInfo_pb);
   
   sv->X->ViewCont->ViewerInfo_TextShell = NULL;
   SUMA_RETURNe;
}


/*!
   \brief Opens a text viewer with SUMA's structure info
   - expects nothing for input 
*/
void SUMA_cb_moreSumaInfo (Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_moreSumaInfo"};
   SUMA_SurfaceObject *SO=NULL;
   void *n=NULL;
   char *s = NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell = NULL;
   
   SUMA_ENTRY;
   
   /* check to see if window is already open, if it is, just raise it */
   if (SUMAg_CF->X->SumaCont->SumaInfo_TextShell) {
      XRaiseWindow (SUMAg_CF->X->DPY_controller1, XtWindow(SUMAg_CF->X->SumaCont->SumaInfo_TextShell->toplevel));
      SUMA_RETURNe;
   }
   
   /* for the string of the surface info */
   s = SUMA_CommonFieldsInfo (SUMAg_CF, 1);
   
   if (s) {
      TextShell =  SUMA_CreateTextShellStruct (SUMA_SumaInfo_open, NULL, 
                                               SUMA_SumaInfo_destroyed, NULL);
      if (!TextShell) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateTextShellStruct.\n", FuncName);
         SUMA_RETURNe;
      }
      SUMAg_CF->X->SumaCont->SumaInfo_TextShell = SUMA_CreateTextShell(s, "SUMA", TextShell);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CommonFieldsInfo.\n", FuncName);
   }   

    
   SUMA_RETURNe;
}

/*!
   \brief Function called when Suma Info window is open
*/
void SUMA_SumaInfo_open (void *p) 
{
   static char FuncName[] = {"SUMA_SumaInfo_open"};
  
   SUMA_ENTRY;
   
   /* nothing to do here ... */
   
   SUMA_RETURNe;
}

/*!
   \brief Function called when Suma Info window is destroyed
*/
void SUMA_SumaInfo_destroyed (void *p) 
{
   static char FuncName[] = {"SUMA_SumaInfo_destroyed"};
   SUMA_SurfaceObject *SO= NULL;
   
   SUMA_ENTRY;

   SUMAg_CF->X->SumaCont->SumaInfo_TextShell = NULL;
   SUMA_RETURNe;
}

/*! 
   \brief opens a text window with information about the surface object in focus 
   -expects SO pointer in userdata
   Feb 23 04: Now requiring sv in client_data. Other widgets will be calling
   this function too, so w should not be used
*/
void SUMA_cb_moreSurfInfo (Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_moreSurfInfo"};
   SUMA_SurfaceObject *SO=NULL;
   void *n=NULL;
   char *s = NULL;
   void **curSOp;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell = NULL;
   
   SUMA_ENTRY;
   #if 0
   XtVaGetValues (w,
                  XmNuserData, &n,
                  NULL);
                  
   SO = (SUMA_SurfaceObject *)n;
   #else
   curSOp = (void **)client_data;
   SO = (SUMA_SurfaceObject *)(*curSOp);
   #endif
  
   /* check to see if window is already open, if it is, just raise it */
   if (SO->SurfCont->SurfInfo_TextShell) {
      XRaiseWindow (SUMAg_CF->X->DPY_controller1, XtWindow(SO->SurfCont->SurfInfo_TextShell->toplevel));
      SUMA_RETURNe;
   }
   
   /* for the string of the surface info */
   s = SUMA_SurfaceObject_Info (SO, SUMAg_CF->DsetList);
   
   if (s) {
      TextShell =  SUMA_CreateTextShellStruct (SUMA_SurfInfo_open, (void *)SO, 
                                               SUMA_SurfInfo_destroyed, (void *)SO);
      if (!TextShell) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateTextShellStruct.\n", FuncName);
         SUMA_RETURNe;
      }
      SO->SurfCont->SurfInfo_TextShell = SUMA_CreateTextShell(s, SO->Label, TextShell);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_SurfaceObject_Info.\n", FuncName);
   }   

    
   SUMA_RETURNe;
}

/*!
   \brief Function called when Surface Info window is open
*/
void SUMA_SurfInfo_open (void *p) 
{
   static char FuncName[] = {"SUMA_SurfInfo_open"};
   SUMA_SurfaceObject *SO= NULL;
  
   SUMA_ENTRY;
   
   SO = (SUMA_SurfaceObject *)p;
   MCW_invert_widget (SO->SurfCont->SurfInfo_pb);
   
   
   SUMA_RETURNe;
}

/*!
   \brief Function called when Surface Info window is destroyed
*/
void SUMA_SurfInfo_destroyed (void *p) 
{
   static char FuncName[] = {"SUMA_SurfInfo_destroyed"};
   SUMA_SurfaceObject *SO= NULL;
   
   SUMA_ENTRY;

   SO = (SUMA_SurfaceObject *)p;
   MCW_invert_widget (SO->SurfCont->SurfInfo_pb);
   
   SO->SurfCont->SurfInfo_TextShell = NULL;
   SUMA_RETURNe;
}

/*!
   \brief calls XtDestroyWidget on to top level shell of w and frees the TextShell pointer in clientdata. 
*/
void SUMA_DestroyTextShell (Widget w, XtPointer ud, XtPointer cd) 
{
   static char FuncName[] = {"SUMA_DestroyTextShell"};
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell=NULL;
   
   SUMA_ENTRY;
   
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *)ud;
   
   if (TextShell->DestroyCallBack) {
      /* call destroy callback */
      TextShell->DestroyCallBack(TextShell->DestroyData);
   }
   if (TextShell) SUMA_free(TextShell);
   
   XtDestroyWidget(SUMA_GetTopShell(w));

   SUMA_RETURNe;
}


/*!
   \brief Creates the structure used to pass widget and options back and forth from SUMA_CreateTextShell
   TextShellStruct = SUMA_CreateTextShellStruct (void (*opencallback)(void *data), void *opendata, 
                                                            void (*closecallback)(void*data), void *closedata);
                                                            
   - callbacks and their data are stored in their respective fields in TextShellStruct
   - All widgets are set to NULL
   
*/

SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTextShellStruct (
                                    void (*opencallback)(void *data), 
                                    void *opendata,
                                    void (*closecallback)(void*data), 
                                    void *closedata)
{
   static char FuncName[] = {"SUMA_CreateTextShellStruct"};
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell=NULL;
   
   SUMA_ENTRY;
   
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *) 
               SUMA_calloc(1,sizeof(SUMA_CREATE_TEXT_SHELL_STRUCT));
   if (!TextShell) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed to allocate for TextShell.\n", 
               FuncName);
      SUMA_RETURN (NULL);
   }
   TextShell->text_w =  TextShell->search_w = 
                        TextShell->text_output = 
                        TextShell->toplevel = NULL;
   TextShell->case_sensitive = NOPE;
   TextShell->allow_edit = NOPE;
   TextShell->OpenCallBack = opencallback;
   TextShell->OpenData = opendata;
   TextShell->DestroyCallBack = closecallback;
   TextShell->DestroyData = closedata;
   TextShell->CursorAtBottom = NOPE;
   
   SUMA_RETURN (TextShell);
}  

/*!
   \brief Opens a window with text information in it. 
   
   \param s (char *) string to display, must be null terminated.
   \param title (char *) title of window
   \param TextShell (SUMA_CreateTextShell *) containing options for SUMA_CreateTextShell
      if TextShell->toplevel then only the log message is updated, otherwise the window is created.
   \return TextShell (SUMA_CreateTextShell *) same structure sent to function but with widgets fields filled. 
   
   \sa SUMA_CreateTextShellStruct
   
   - based on example SUMA_search_text from "Motif Programming Manual"
   see copyright notice in beginning of SUMA_display.c
*/   
SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTextShell (
                                 char *s, 
                                 char *title, 
                                 SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell)
{
   static char FuncName[] = {"SUMA_CreateTextShell"};
   Widget rowcol_v, rowcol_h, close_w, form, frame, toggle_case_w;
   int n;
   SUMA_Boolean LocalHead = NOPE;
   Pixel fg_pix = 0;
   Arg args[30];
   static XmTextScanType sarray[] = {  XmSELECT_POSITION, 
                                       XmSELECT_WORD, 
                                       XmSELECT_LINE, 
                                       XmSELECT_ALL };
   SUMA_ENTRY;

   if (TextShell->OpenCallBack) { /* do the opening callback */
      if (LocalHead) fprintf (SUMA_STDERR, 
                              "%s: Calling OpenCallBack.\n", 
                              FuncName);
      TextShell->OpenCallBack(TextShell->OpenData);
   }
   
   if (!TextShell->toplevel) { /* need to create window */
      if (LocalHead) fprintf (SUMA_STDERR, 
                              "%s: Creating new text shell window.\n",
                              FuncName);
      TextShell->toplevel = XtVaAppCreateShell (title, "Suma",
         topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,        
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);  

      XmAddWMProtocolCallback(/* make "Close" window menu work */
         TextShell->toplevel,
         XmInternAtom(  SUMAg_CF->X->DPY_controller1 , 
                        "WM_DELETE_WINDOW" , 
                        False ) ,
         SUMA_DestroyTextShell, TextShell) ;

      form = XtVaCreateWidget ("textoutput",
        xmFormWidgetClass, TextShell->toplevel, NULL);


      rowcol_v = XtVaCreateWidget ("rowcol_v",
        xmRowColumnWidgetClass, form, NULL);

      rowcol_h = XtVaCreateWidget ("rowcol_h",
        xmRowColumnWidgetClass, rowcol_v,
        XmNorientation,  XmHORIZONTAL,
        NULL);
      XtVaCreateManagedWidget ("Search Pattern:",
        xmLabelWidgetClass, rowcol_h, NULL);

      TextShell->search_w = XtVaCreateManagedWidget ("SUMA_search_text",
        xmTextFieldWidgetClass, rowcol_h, NULL);

      XtVaGetValues (TextShell->search_w, XmNforeground, &fg_pix, NULL);
      toggle_case_w = XtVaCreateManagedWidget ("Case Sensitive",
         xmToggleButtonWidgetClass, rowcol_h,
         XmNset, TextShell->case_sensitive,
         XmNselectColor, fg_pix, 
         NULL);
      XtAddCallback (toggle_case_w,
                     XmNvalueChangedCallback,
                     SUMA_cb_ToggleCaseSearch, 
                     TextShell);

      close_w = XtVaCreateManagedWidget (
                     "Close", 
                     xmPushButtonWidgetClass, 
                     rowcol_h, NULL);
      XtAddCallback (close_w, 
                     XmNactivateCallback, 
                     SUMA_DestroyTextShell, 
                     TextShell);    

      XtManageChild (rowcol_h);
      
      TextShell->text_output = XtVaCreateManagedWidget ("text_output",
        xmTextWidgetClass, rowcol_v,
        XmNeditable, False,
        XmNcursorPositionVisible, False,
        XmNshadowThickness,       0,
        XmNhighlightThickness,    0,
        NULL);

      XtManageChild (rowcol_v);

      n = 0;
      XtSetArg (args[n], XmNrows,      10); n++;
      XtSetArg (args[n], XmNcolumns,   80); n++;
      XtSetArg (args[n], XmNeditMode,  XmMULTI_LINE_EDIT); n++;
      XtSetArg (args[n], XmNeditable, TextShell->allow_edit); n++;
      XtSetArg (args[n], XmNscrollHorizontal,  False); n++;
      XtSetArg (args[n], XmNwordWrap,  True); n++;
      XtSetArg (args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg (args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg (args[n], XmNtopWidget, rowcol_v); n++;
      /* XtSetArg (args[n], XmNselectionArray, sarray); n++;
      XtSetArg (args[n], XmNselectionArrayCount, 4); n++;
      XtSetArg (args[n], XmNselectThreshold, 1); n++;  leave it to defaults*/
      
      TextShell->text_w = XmCreateScrolledText (form, "text_w", args, n);
                                  
      if (!s) {
         XmTextSetString (TextShell->text_w, "No Messages.\n---------------\n");
      } else {
         XmTextSetString (TextShell->text_w, s);
      }   
      XtManageChild (TextShell->text_w);
      
      XtAddCallback (TextShell->search_w, 
                     XmNactivateCallback, SUMA_cb_search_text, 
                     TextShell);
      
      /* Setting XmNtranslations gives warning 
         if done inside XmCreateScrolledText ror text_w 
      
      Not needed anymore, see #override in fallback resources
      If you keep this here, you can no longer select from the text window!
      
      XtVaSetValues  (  TextShell->text_w, 
                        XmNtranslations,
                        XtParseTranslationTable(SUMA_TEXT_WIDGET_TRANSLATIONS),
                        NULL); 
      */

      XtManageChild (form);

      /* pop it up if it is a topLevelShellWidgetClass */
      XtPopup(TextShell->toplevel, XtGrabNone);   

      XtRealizeWidget (TextShell->toplevel);
   } else { /* already created, just replace text and perhaps title (in the future)*/
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Setting string in previously created text shell window.\n", FuncName);
      if (!s) XmTextSetString (TextShell->text_w, "No Messages.\n---------------\n");
      else XmTextSetString (TextShell->text_w, s);
      if (TextShell->CursorAtBottom) {
         XmTextSetInsertionPosition(TextShell->text_w, XmTextGetLastPosition (TextShell->text_w));
      }
   }
   SUMA_RETURN(TextShell);
}

/*! \brief toggles case sensitive search 
   - Expects a SUMA_CREATE_TEXT_SHELL_STRUCT pointer in clientdata:
*/

void SUMA_cb_ToggleCaseSearch (Widget widget, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_ToggleCaseSearch"};
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell;
   
   SUMA_ENTRY;

   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *)client_data;
   TextShell->case_sensitive = !TextShell->case_sensitive;
   
   SUMA_RETURNe;   
}
/*!
    \brief searches text in a text widget for a string specified in a textfield widget and
    writes the results in a text_output text widget.

   - Expects a structure SUMA_CREATE_TEXT_SHELL_STRUCT pointer in clientdata:
      text_w (Widget)
      search_w (Widget)
      text_output (Widget)
      
   - Based on search_text() from "Motif Programming Manual"
*/
void SUMA_cb_search_text(Widget widget, XtPointer client_data, XtPointer call_data)
{
   char *search_pat, *p, *string, buf[32];
   XmTextPosition pos;
   int len, i;
   Boolean found = False;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell;
   static char FuncName[]={"SUMA_cb_search_text"};
   
   SUMA_ENTRY;
   
   
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *)client_data;
   if (!TextShell) { SUMA_SL_Err("Unexpected NULL TextShell"); SUMA_RETURNe; }
   
   /* get the text that is about to be searched */
   if (!(string = XmTextGetString (TextShell->text_w)) || !*string) {
     XmTextSetString (TextShell->text_output, "No text to search.");
     if (string) XtFree (string); /* may have been ""; free it */
     SUMA_RETURNe;
   }
   len = strlen(string);
   if (!TextShell->case_sensitive) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Case insensitive search.\n", FuncName);
      /* turn string to lowercase */
      for (i=0; i < len; ++i) string[i] = tolower(string[i]);   
   }
   
   /* get the pattern we're going to search for in the text. */
   if (!(search_pat = XmTextGetString (TextShell->search_w)) || !*search_pat) {
     XmTextSetString (TextShell->text_output, "Specify a search pattern.");
     XtFree (string); /* this we know is a string; free it */
     if (search_pat) XtFree (search_pat); /* this may be "", XtFree() checks.. */
     SUMA_RETURNe;
   }
   len = strlen (search_pat);
   
   if (!TextShell->case_sensitive) {
      /* turn search_pat to lowercase */
      for (i=0; i < len; ++i) search_pat[i] = tolower(search_pat[i]);  
   }
   /* start searching at current cursor position + 1 to find
   * the -next- occurrance of string.  we may be sitting on it.
   */
   pos = XmTextGetCursorPosition (TextShell->text_w);
   for (p = &string[pos+1]; (p = index (p, *search_pat)); p++)
     if (!strncmp (p, search_pat, len)) {
         found = True;
         break;
     }
   if (!found) { /* didn't find pattern? */
     /* search from beginning till we've passed "pos" */
     for (p = string;
             (p = index (p, *search_pat)) && p - string <= pos; p++)
         if (!strncmp (p, search_pat, len)) {
             found = True;
             break;
         }
   }
   if (!found)
     XmTextSetString (TextShell->text_output, "Pattern not found.");
   else {
     pos = (XmTextPosition)(p - string);
     sprintf (buf, "Pattern found at position %ld.", pos);
     XmTextSetString (TextShell->text_output, buf);
     XmTextSetInsertionPosition (TextShell->text_w, pos);
     XmTextSetHighlight(TextShell->text_w, pos, pos+len, XmHIGHLIGHT_SELECTED);
   }
   if (string) XtFree (string);
   if (search_pat) XtFree (search_pat);
   
   SUMA_RETURNe;
}

/*!
   \brief sets the saving mode 
   - expects a SUMA_MenuCallBackData * in  client_data
   Nothing in client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetDrawROI_SaveMode(Widget widget, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDrawROI_SaveMode"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   datap = (SUMA_MenuCallBackData *)client_data;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Setting SaveMode to %d\n", FuncName, (int)datap->callback_data);
   SUMAg_CF->X->DrawROI->SaveMode = (int)datap->callback_data; 
   
   SUMA_RETURNe;
}


/*!
   \brief sets the "saving what" parameter
   - expects a SUMA_MenuCallBackData * in  client_data
   Nothing in client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetDrawROI_WhatDist(Widget widget, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDrawROI_WhatDist"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   datap = (SUMA_MenuCallBackData *)client_data;

   if (LocalHead) fprintf(SUMA_STDERR,"%s: Setting WhatDist to %d\n", FuncName, (int)datap->callback_data);
   SUMAg_CF->X->DrawROI->WhatDist = (int)datap->callback_data; 
   
   SUMA_RETURNe;
} 

/*!
   \brief sets the " what distance" parameter
   - expects a SUMA_MenuCallBackData * in  client_data
   Nothing in client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetDrawROI_SaveWhat(Widget widget, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDrawROI_SaveWhat"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   datap = (SUMA_MenuCallBackData *)client_data;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Setting SaveWhat to %d\n", FuncName, (int)datap->callback_data);
   SUMAg_CF->X->DrawROI->SaveWhat = (int)datap->callback_data; 
   
   SUMA_RETURNe;
}
   
/*!
   \brief sets the rendering mode of a surface 
   
   - expects a SUMA_MenuCallBackData * in  client_data
   with SO as client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetRenderMode(Widget widget, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetRenderMode"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceObject *SO = NULL;
   void **curSOp;
   int imenu = 0;
   
   SUMA_ENTRY;

   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   curSOp = (void **)datap->ContID;
   SO = (SUMA_SurfaceObject *)(*curSOp);
   imenu = (int)datap->callback_data; 
   
   switch (imenu) {
      case SW_SurfCont_RenderViewerDefault:
         imenu = SRM_ViewerDefault;
         break;
      case SW_SurfCont_RenderFill:
         imenu = SRM_Fill;
         break;
      case SW_SurfCont_RenderLine:
         imenu = SRM_Line;
         break;
      case SW_SurfCont_RenderPoints:
         imenu = SRM_Points;
         break;
      case SW_SurfCont_RenderHide:
         imenu = SRM_Hide;
         break;
      default: 
         fprintf (SUMA_STDERR, "Error %s: Unexpected widget index.\n", FuncName);
         break;
   }
   
   
   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_SumaWidget, NULL);   
   ED = SUMA_InitializeEngineListData (SE_SetRenderMode);
   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)SO,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", FuncName);
      SUMA_RETURNe;                                     
   }
   
   
   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURNe;    
   }
   
   SUMA_RETURNe;
}
/*!
   \brief pops a SUMA message
*/
void SUMA_PopUpMessage (SUMA_MessageData *MD)
{
   static char FuncName[]={"SUMA_PopUpMessage"};
   Widget Parent_w=NULL, wmsg = NULL;
   int ii;
   char *sf=NULL; 
   SUMA_ENTRY;
   
   if (!SUMAg_N_SVv) {
      /* no graphics here, get out */
      SUMA_RETURNe;
   }
   
   /* find a decent popup message parent */
   ii=0;
   while (  (  SUMAg_SVv[ii].isShaded || 
               !SUMAg_SVv[ii].X->TOPLEVEL) && 
            (ii < SUMAg_N_SVv)) {
      ++ii;   
   }
   
   
   if (ii < SUMAg_N_SVv)
      Parent_w = SUMAg_SVv[ii].X->TOPLEVEL;
   else { 
      /* try again but with one that could be shaded */
      ii=0;
      while (!SUMAg_SVv[ii].X->TOPLEVEL && (ii < SUMAg_N_SVv)) {
         ++ii;   
      }
      if (ii >= SUMAg_N_SVv) {
         fprintf (SUMA_STDERR, 
            "Error %s: This should not be happening.\n", FuncName);
         SUMA_RETURNe;  
      }else Parent_w = SUMAg_SVv[ii].X->TOPLEVEL;
   }
   
   if (MD->Action ==  SMA_LogAndPopup) {
      wmsg = NULL;
      sf = SUMA_FormatMessage (MD);
      switch (MD->Type) {
         case SMT_Notice:
            wmsg = MCW_popup_message(Parent_w, sf, 
                                     MCW_USER_KILL | MCW_TIMER_KILL);
            break;
         case SMT_Warning:
            wmsg = MCW_popup_message(Parent_w, sf, 
                                     MCW_USER_KILL | MCW_TIMER_KILL);
            break;
         case SMT_Error:
            wmsg = MCW_popup_message(Parent_w, sf, 
                                     MCW_USER_KILL);
            break;
         case SMT_Critical:
            wmsg = MCW_popup_message(Parent_w, sf, 
                                     MCW_CALLER_KILL);
            break;
         case SMT_Text:
            wmsg = MCW_popup_message(Parent_w, sf,  
                                     MCW_CALLER_KILL | MCW_TIMER_KILL);
            break;
         default:
            break;
      }
      if (sf)SUMA_free(sf); sf = NULL;
      if (wmsg) {
         SUMA_PositionWindowRelative (wmsg, NULL, SWP_POINTER_OFF);
      }
   }
   
   SUMA_RETURNe;   

}

/*!
   \brief forms the message string.
*/
char * SUMA_FormatMessage (SUMA_MessageData *MD) 
{
   static char FuncName[]={"SUMA_FormatMessage"};
   char *s=NULL;

   SUMA_ENTRY;

   s = (char *)SUMA_calloc (strlen(MD->Message)+strlen(MD->Source)+100, sizeof(char));
   if (!s) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   switch (MD->Type) {
      case SMT_Notice:
         sprintf(s,"Notice %s:\n%s\n", MD->Source, MD->Message);
         break;
      case SMT_Warning:
         sprintf(s,"Warning %s:\n%s\n", MD->Source, MD->Message);
         break;
      case SMT_Error:
         sprintf(s,"Error %s:\n%s\n", MD->Source, MD->Message);
         break;
      case SMT_Critical:
         sprintf(s,"Critical Error %s:\n%s\n", MD->Source, MD->Message);
         break;
      case SMT_Text:
         sprintf(s,"%s", MD->Message);
         break;
      default:
         sprintf(s,"BAD MESSAGE.\n");
         break;
   }
   
   SUMA_RETURN (s);
}


/*!
   \brief opens the DRAW ROI window 
   
   - expects a SUMA_MenuCallBackData * in  client_data
   with sv index as client_data->ContID 
*/
void SUMA_cb_ToolsDrawROI (Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_ToolsDrawROI"};
   int isv;
   DList *list = NULL;
   SUMA_MenuCallBackData * datap=NULL;
   
   SUMA_ENTRY;

   /* get the surface viewer that the command was made in */
   datap = (SUMA_MenuCallBackData *)client_data;
   isv = (int)datap->ContID;

   /* register a call to open the ROI editor */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_OpenDrawROI, SES_SumaWidget, (void*)&SUMAg_SVv[isv]); 
   if (!SUMA_Engine (&list)) {
      SUMA_RegisterMessage (SUMAg_CF->MessageList, "Failed to open DrawROI", FuncName, SMT_Error, SMA_LogAndPopup);
   }  
   SUMA_RETURNe;
}

/*!
   \brief Undo an action on the stack
   
   - expects nothing
*/
void SUMA_cb_DrawROI_Undo (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_DrawROI_Undo"};
   DList *list = NULL;
   DListElmt *tmp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->X->DrawROI->curDrawnROI) SUMA_RETURNe;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Calling SUMA_UndoAction...\n", FuncName);
   
   if (!SUMAg_CF->X->DrawROI->curDrawnROI->StackPos) {
      SUMA_SLP_Err("Nothing to Undo.");
      SUMA_RETURNe;
   }
   
   tmp = SUMA_UndoAction (SUMAg_CF->X->DrawROI->curDrawnROI->ActionStack, SUMAg_CF->X->DrawROI->curDrawnROI->StackPos);
   if (!tmp) {
      SUMA_S_Err("Failed to Undo.");
      SUMA_RETURNe;
   }else if (tmp == SUMAg_CF->X->DrawROI->curDrawnROI->StackPos) {
      /* reached bottom */
      SUMAg_CF->X->DrawROI->curDrawnROI->StackPos = NULL;
   }else {
      SUMAg_CF->X->DrawROI->curDrawnROI->StackPos = tmp;
   }

   if (dlist_size(SUMAg_CF->X->DrawROI->curDrawnROI->ROIstrokelist)) {
      SUMA_LH("Not empty ROIstrokelist");
   }else {
      SUMA_LH(" empty ROIstrokelist");
   }
   
   /* do the paint thing */
   /* Now update the Paint job on the ROI plane */
   if (!SUMA_Paint_SO_ROIplanes_w (SUMA_findSOp_inDOv(SUMAg_CF->X->DrawROI->curDrawnROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv), SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURNe;
   } 

   /* place a call to redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_Suma, NULL);
   if (!SUMA_Engine (&list)) {
      SUMA_SL_Err("Failed calling SUMA_Engine.");
   }
   
   
   SUMA_RETURNe;
}

void SUMA_cb_DrawROI_Redo (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_DrawROI_Redo"};
   DList *list = NULL;
   DListElmt *tmp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->X->DrawROI->curDrawnROI) SUMA_RETURNe;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Calling SUMA_RedoAction...\n", FuncName);
   
   if (SUMAg_CF->X->DrawROI->curDrawnROI->StackPos == dlist_tail(SUMAg_CF->X->DrawROI->curDrawnROI->ActionStack)) {
      SUMA_SLP_Err("Nothing to Redo.");
      SUMA_RETURNe;
   }
   
   tmp = SUMA_RedoAction (SUMAg_CF->X->DrawROI->curDrawnROI->ActionStack, SUMAg_CF->X->DrawROI->curDrawnROI->StackPos);
   if (!tmp) {
      SUMA_S_Err("Failed to Redo.");
      SUMA_RETURNe;
   }else {
      SUMAg_CF->X->DrawROI->curDrawnROI->StackPos = tmp;
   }
   

   /* do the paint thing */
   /* Now update the Paint job on the ROI plane */
   if (!SUMA_Paint_SO_ROIplanes_w (SUMA_findSOp_inDOv(SUMAg_CF->X->DrawROI->curDrawnROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv), SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURNe;
   } 

   /* place a call to redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_Suma, NULL);
   if (!SUMA_Engine (&list)) {
      SUMA_SL_Err("Failed calling SUMA_Engine.");
   }
   
   SUMA_RETURNe;
}

void SUMA_cb_DrawROI_Join (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_DrawROI_Join"};
   SUMA_Boolean LocalHead = NOPE;
   int HeadNode=-1, TailNode=-1;
   float ThirdNode_v[3];
   SUMA_DRAWN_ROI *DrawnROI=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_ROI_DATUM *ROIstroke = NULL;
   SUMA_ROI_ACTION_STRUCT *ROIA;
   DListElmt *tmpStackPos=NULL;
   DList *list=NULL;
   
   SUMA_ENTRY;
   
   DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
   
   if (!DrawnROI) {
      SUMA_SLP_Err ("NO ROI to close.");
      SUMA_RETURNe;
   }
   
   if (DrawnROI->DrawStatus == SUMA_ROI_Finished) {
      SUMA_SLP_Err ("Cannot edit Finished ROI.");
      SUMA_RETURNe;
   }
   
   if (DrawnROI->Type != SUMA_ROI_OpenPath) {
      SUMA_SLP_Err ("You can only close an open path.");
      SUMA_RETURNe; 
   }
   
   SUMA_DRAWN_ROI_HEAD_NODE(DrawnROI,HeadNode);
   SUMA_DRAWN_ROI_TAIL_NODE(DrawnROI,TailNode);
   
   /* get the third node, assuming it is along the normal of the TailNode */
   SO = SUMA_findSOp_inDOv(DrawnROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv); 
   ThirdNode_v[0] = SO->NodeList[3*TailNode] + 20 *SO->NodeNormList[3*TailNode];
   ThirdNode_v[1] = SO->NodeList[3*TailNode+1] + 20 *SO->NodeNormList[3*TailNode+1];
   ThirdNode_v[2] = SO->NodeList[3*TailNode+2] + 20 *SO->NodeNormList[3*TailNode+2];
   
   /* No do the intersection */
   ROIstroke = SUMA_Surf_Plane_Intersect_ROI (SO, TailNode, HeadNode, ThirdNode_v);
   
   if (!ROIstroke) {
      SUMA_SLP_Err ("Failed to close path.\nTry closing with mouse.");
      SUMA_RETURNe;
   }
   
   /* what is the last node of ROIstroke ? 
   It is possible that the returned ROIstroke 
   was not a successful closure (a partial success), investigate*/
   if (LocalHead) fprintf(SUMA_STDERR, "%s: Last node of ROIstroke is %d\n", FuncName, ROIstroke->nPath[ROIstroke->N_n-1]); 
   if (ROIstroke->nPath[ROIstroke->N_n-1] != HeadNode) {
      /* Can't accept partials */
      SUMA_SLP_Err ("Failed to close path.\nTry closing with mouse.");
      SUMA_RETURNe;
   }
   
   /* looking good, add the thing */
   ROIstroke->action = SUMA_BSA_JoinEnds;
   ROIA = (SUMA_ROI_ACTION_STRUCT *)
            SUMA_calloc(1,sizeof(SUMA_ROI_ACTION_STRUCT)); /* this structure is freed in SUMA_DestroyROIActionData */
   ROIA->DrawnROI = DrawnROI;
   ROIA->ROId = ROIstroke;
   tmpStackPos = SUMA_PushActionStack (DrawnROI->ActionStack, DrawnROI->StackPos, SUMA_AddToTailJunctionROIDatum, (void *)ROIA, SUMA_DestroyROIActionData);
   if (tmpStackPos) DrawnROI->StackPos = tmpStackPos;
   else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_PushActionStack.\n", FuncName);
      SUMA_RETURNe;
   }
   
   /* redisplay all others */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_RedisplayNow_AllVisible, SES_SumaWidget, NULL);
   SUMA_Engine (&list);

   SUMA_RETURNe;
}

void SUMA_cb_DrawROI_Finish (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_DrawROI_Finish"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_DRAWN_ROI *DrawnROI=NULL;
   SUMA_ROI_ACTION_STRUCT *ROIA;
   SUMA_SurfaceObject *SO = NULL;
   DListElmt *tmpStackPos=NULL;
   DList *list=NULL;
   
   SUMA_ENTRY;
   
   DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
   
   if (!DrawnROI) {
      SUMA_SLP_Err ("NO ROI to finish.");
      SUMA_RETURNe;
   }
   
   if (DrawnROI->DrawStatus == SUMA_ROI_Finished) {
      /* nothing to do */
      SUMA_RETURNe;
   }
   
   /* looking good, add the thing */
   ROIA = (SUMA_ROI_ACTION_STRUCT *)
             SUMA_calloc(1,sizeof(SUMA_ROI_ACTION_STRUCT)); /* this structure is freed in SUMA_DestroyROIActionData */
   ROIA->DrawnROI = DrawnROI;
   ROIA->ROId = NULL;
   tmpStackPos = SUMA_PushActionStack (DrawnROI->ActionStack, DrawnROI->StackPos, SUMA_FinishedROI, (void *)ROIA, SUMA_DestroyROIActionData);
   if (tmpStackPos) DrawnROI->StackPos = tmpStackPos;
   else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_PushActionStack.\n", FuncName);
      SUMA_RETURNe;
   }
   
   SO = SUMA_findSOp_inDOv(DrawnROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   /* Now update the Paint job on the ROI plane */
   if (!SUMA_Paint_SO_ROIplanes_w (SO, SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURNe;
   }
   
   /* redisplay all others */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_SumaWidget, NULL);
   SUMA_Engine (&list);

   SUMA_RETURNe;
}

/*! 
   \brief Delete a drawn ROI 
   
*/
void SUMA_cb_DrawROI_Delete(Widget wcall, XtPointer cd1, XtPointer cbs)
{
   static char *PlaneName=NULL, FuncName[] = {"SUMA_cb_DrawROI_Delete"};
   XmPushButtonCallbackStruct * pbcbs = (XmPushButtonCallbackStruct *) cbs ;
   static int ErrCnt =0;
   DList *list=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SUMAg_CF->X->DrawROI->curDrawnROI && wcall) { 
      /* remember, you'll always get the callback from the time out function
      the use of wcall in the condition above is to tell the difference between
      the timeout call and a delete request with a null curDrawnROI ...
      NULL curDrawnROI occurs normally such as when there are no ROI left */
      if (!ErrCnt) SUMA_SLP_Note ("No ROI to delete");
      wcall = NULL; /* use this to turn key back to lower case ahead */
      ErrCnt ++;
   }
   
   /* NULL widget --> reset button to lowercase */
   if( wcall == NULL ){
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Resetting button.\n", FuncName); 
      if( SUMAg_CF->X->DrawROI->Delete_first == NOPE ){
         MCW_set_widget_label( SUMAg_CF->X->DrawROI->Delete_pb , "delete ROI" ) ;
         SUMAg_CF->X->DrawROI->Delete_first = YUP ;
      }
      SUMA_RETURNe ;
   }
   
   /* First press --> just change button label */

   if( SUMAg_CF->X->DrawROI->Delete_first ){
      MCW_set_widget_label( SUMAg_CF->X->DrawROI->Delete_pb  , "DELETE ROI" ) ;
      SUMAg_CF->X->DrawROI->Delete_first = NOPE ;

      /* if not re-pressed in 5 seconds, will reset to lowercase */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: First Press, adding time out.\n", FuncName);
      (void) XtAppAddTimeOut(
               XtWidgetToApplicationContext(SUMAg_CF->X->DrawROI->Delete_pb) ,
               5000 , SUMA_delete_timeout_CB , NULL ) ;

       SUMA_RETURNe;
   }
   
   /* delete ROI */
   ErrCnt = 0;
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Should be deleting ROI %s here ...\n", FuncName, SUMAg_CF->X->DrawROI->curDrawnROI->Label);
   
   /* preserve some info about ROI to be deleted */
   SO = SUMA_findSOp_inDOv(SUMAg_CF->X->DrawROI->curDrawnROI->Parent_idcode_str , SUMAg_DOv, SUMAg_N_DOv);
   PlaneName = SUMA_copy_string(SUMAg_CF->X->DrawROI->curDrawnROI->ColPlaneName);
   
   if (!SUMA_DeleteROI (SUMAg_CF->X->DrawROI->curDrawnROI)) {
      SUMA_SLP_Err("Failed to delete ROI");
      SUMA_RETURNe; 
   }
   
   /* If no other ROIs remain on the same plane as the deleted ROI, flush that plane's colors */
   SUMA_FlushPlaneNotInUse (PlaneName, SO, SUMAg_DOv, SUMAg_N_DOv);
   if (PlaneName) SUMA_free(PlaneName);
   
   /* Now update the Paint job on the ROI plane */
   if (!SUMA_Paint_SO_ROIplanes_w (
         SO, SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURNe;
   }

   /* redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_Suma, NULL); 
   if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
   
   /* reset the Delete button settings */
   MCW_set_widget_label( SUMAg_CF->X->DrawROI->Delete_pb , "delete ROI" ) ;   
   SUMA_RETURNe;
}

void SUMA_delete_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   static char FuncName[] = {"SUMA_delete_timeout_CB"};

   SUMA_ENTRY;

   SUMA_cb_DrawROI_Delete(NULL, NULL, NULL);

   SUMA_RETURNe; 
}

/*!
   \brief saving the current ROI (stored in SUMAg_CF->X->DrawROI->curDrawnROI) to a niml format
*/
void SUMA_cb_DrawROI_Save (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_DrawROI_Save"};
   SUMA_DRAWN_ROI *dROI=NULL;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   dROI = SUMAg_CF->X->DrawROI->curDrawnROI;
   
   if (!dROI) {
      SUMA_LH("NULL ROI");
      SUMA_RETURNe;
   }
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_SaveDrawnROIFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, NULL,
                                          SES_Suma, NULL, NOPE,
                                          SEI_Head, NULL))) {
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
   }
   if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, (int *)w,
                                          SES_Suma, NULL, NOPE,
                                          SEI_In, NextElm)) {
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
   }
   
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }

   SUMA_RETURNe;
}

void SUMA_cb_DrawROI_Load (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_DrawROI_Load"};
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_OpenDrawnROIFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, NULL,
                                          SES_Suma, NULL, NOPE,
                                          SEI_Head, NULL))) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed to register command.\n", FuncName);
   }
   if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, (int *)w,
                                          SES_Suma, NULL, NOPE,
                                          SEI_In, NextElm)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed to register command.\n", FuncName);
   }
   
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }

   SUMA_RETURNe;
}

void SUMA_DrawROI_NewLabel (void *data)
{
   static char FuncName[]={"SUMA_DrawROI_NewLabel"};
   SUMA_DRAWN_ROI *DrawnROI=NULL;
   SUMA_ARROW_TEXT_FIELD * AF=NULL;
   void *n=NULL;
   static int ErrCnt=0;
   SUMA_Boolean Shaded = YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   AF = (SUMA_ARROW_TEXT_FIELD *)data;
   DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
   if (!DrawnROI) {
      SUMA_LH("Null DrawnROI");
      SUMA_RETURNe;
   }
   
   XtVaGetValues (AF->textfield, XmNvalue, &n, NULL); 
   /* return if no change has been made */
   if (!strcmp((char *)n, DrawnROI->Label)) {
      SUMA_LH("No change");
      SUMA_LH((char *)n);
      SUMA_LH(DrawnROI->Label);
      SUMA_RETURNe;
   }
   
   if (DrawnROI->DrawStatus != SUMA_ROI_Finished) {
      SUMA_LH("unFinished");
      /* YOU DO NOT WANT TO FREE n because n is not a copy of the string in the widget! */
      if (DrawnROI->Label) {
         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: Changing ROI label from %s to %s\n", FuncName, DrawnROI->Label, (char *)n);         
         DrawnROI->Label = (char *)SUMA_realloc(DrawnROI->Label, sizeof(char)*(strlen((char *)n)+1));
      }  else {
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Setting ROI label to %s\n", FuncName, (char *)n);
         DrawnROI->Label = (char *)SUMA_malloc(sizeof(char) * (strlen((char *)n)+1));
      }
      DrawnROI->Label = strcpy(DrawnROI->Label, (char *)n);   
      ErrCnt = 0;
      
      /* check if list window is open and update it if need be */
      SUMA_IS_DRAW_ROI_SWITCH_ROI_SHADED(Shaded);
      if (!Shaded) {
         if (LocalHead) fprintf (SUMA_STDERR, "%s: updating switch ROI window ...\n", FuncName);
         SUMA_cb_DrawROI_SwitchROI(NULL, (XtPointer) SUMAg_CF->X->DrawROI->SwitchROIlst, NULL);
      }
   } else {
      SUMA_LH("Finished");
      if (!ErrCnt) SUMA_SLP_Err("ROI maked as finished.\nNew label cannot be applied.");
      ++ErrCnt;
      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIlbl->textfield, DrawnROI->Label);
   }
   
   SUMA_RETURNe;
}

void SUMA_WidgetResize (Widget New, int width, int height)
{
   static char FuncName[]={"SUMA_WidgetResize"};
   
   SUMA_ENTRY;
   
   XtVaSetValues (New,
      XmNwidth, width,
      XmNheight, height,
      NULL);
   SUMA_RETURNe;
}
/*!
   \brief Positions a new widget relative to a reference widget 
   SUMA_PositionWindowRelative ( New,  Ref,  Loc);
   
   \param New (Widget) the widget to place
   \param Ref (Widget) the widget relative to which New is placed (could pass NULL if positioning relative to pointer)
   
   \param Loc (SUMA_WINDOW_POSITION) the position of New relative to Ref
*/
void SUMA_PositionWindowRelative (  Widget New, Widget Ref, 
                                    SUMA_WINDOW_POSITION Loc)
{
   static char FuncName[]={"SUMA_PositionWindowRelative"};
   Position RefX=0, RefY=0, NewX=0, NewY=0, Dx=5, RootX=0, RootY=0;
   Dimension RefW=0, RefH=0, ScrW=0, ScrH=0, NewW=0, NewH=0;
   
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!New) { SUMA_RETURNe; }
   
   ScrW = WidthOfScreen (XtScreen(New));
   ScrH = HeightOfScreen (XtScreen(New));
   
   SUMA_LH("Getting New Positions");
   XtVaGetValues (New,           /* get the positions of New */
         XmNwidth, &NewW,
         XmNheight, &NewH,
         XmNx, &NewX,
         XmNy, &NewY,
         NULL);

   if (Ref) { /* get the positions of Ref */
      XtVaGetValues (Ref,
         XmNwidth, &RefW,
         XmNheight, &RefH,
         XmNx, &RefX,
         XmNy, &RefY,
         NULL);
      if (0) {
         /* stupid gig to figure out window manager position offsets,
         Does not work ..... */
         XtVaSetValues (Ref,  /* set the positions to where WM told you it is */
            XmNx, RefX+20,
            XmNy, RefY+12,
            NULL);
         /* Now query again */
         XtVaGetValues (Ref,   /* get the positions after you've just set them */
            XmNx, &RootX,
            XmNy, &RootY,
            NULL);
         SUMA_LHv( "Nothing changed.....:\n"
                  "Asked for: %d %d\n"
                  "Got      : %d %d\n",
                  RefX+20, RefY+12, RootX, RootY);
      }

   /* These positions do not seem to properly account for window positioning.
    According to documentation, XmNx and Xmny are the coordinates relative to 
    the shell's parent window, usually the window manager's frame window. 
    (tip from ftp://ftp.x.org/contrib/faqs/FAQ-Xt). 
    Use XtTranslateCoords() to translate to root coordinate space. But that does
    not work on my mac (maybe two displays...) and that makes the problem 
    worse. So I won't be using it. Other folksy remedies include setting
    some resource to False but that did nothing. There are also indications
    that this is a quartz problem... Will live with this for now...*/
      /* Get the root position*/
      XtTranslateCoords(Ref, RefX, RefY, &RootX, &RootY);
      SUMA_LHv("Got Ref Positions,  %d %d, %d %d\n"
               "    Root Positions, %d %d\n",
                  RefX, RefY, RefW, RefH, 
                  RootX, RootY);
      /* RefX = RootX; RefY=RootY; Does not work, at least on macs... */
   } else {
      if (LocalHead) fprintf(SUMA_STDERR, "%s: NULL Ref.\n", FuncName);
      RefX = 10;
      RefY = 10;
      RefW = 0;
      RefH = 0;
   }
   
   switch (Loc) {
      case SWP_BOTTOM_RIGHT_CORNER:
         NewX = RefW + RefX;
         NewY = RefH + RefY;
         break; 
      case SWP_TOP_RIGHT:
         NewX = RefW + RefX + Dx;
         NewY = RefY;
         break;
      case SWP_TOP_LEFT:
         NewX = RefW + Dx;
         NewY = RefY;
         break;
      case SWP_STEP_DOWN_RIGHT:
         NewY = RefY + 10;
         NewX = RefX + 10;
         break;
      case SWP_POINTER:
         {
            Window root, child, wind;
            int root_x, root_y, win_x, win_y;
            unsigned int keys_buttons;
            SUMA_LH("Pointer Query 1");
            if (!XtIsRealized(New)) {
               SUMA_LH("Need new wind");
               if (!XtIsRealized(SUMAg_SVv[0].X->GLXAREA)) {
                  SUMA_SL_Err("Nothing to work with here!");
                  SUMA_RETURNe;
               }
               wind = XtWindow(SUMAg_SVv[0].X->GLXAREA);
            } else {
               wind = XtWindow(New);
            }
            XQueryPointer( XtDisplay(New), wind, &root, &child, 
                           &root_x, &root_y, &win_x, &win_y, &keys_buttons);
            NewX = root_x;
            NewY = root_y;
         }
         break;
      case SWP_POINTER_OFF:
         {
            Window root, child, wind;
            int root_x, root_y, win_x, win_y;
            unsigned int keys_buttons;
            SUMA_LH("Pointer Query 2");
            if (!XtIsRealized(New)) {
               SUMA_LH("Need new wind");
               if (!XtIsRealized(SUMAg_SVv[0].X->GLXAREA)) {
                  SUMA_SL_Err("Nothing to work with here!");
                  SUMA_RETURNe;
               }
               wind = XtWindow(SUMAg_SVv[0].X->GLXAREA);
            } else {
               wind = XtWindow(New);
            }
            XQueryPointer( XtDisplay(New), wind, &root, &child, 
                           &root_x, &root_y, &win_x, &win_y, &keys_buttons);
            NewX = root_x - (int)NewW/2;
            NewY = root_y - (int)NewH + Dx;
         }
         break;
            
      default:
         fprintf (SUMA_STDERR, "Error %s: Option not known.\n", FuncName);
         SUMA_RETURNe;
         break;
   }

   
   if (NewX >= ScrW || NewX < 0) NewX = 50;
   if (NewY >= ScrH || NewY < 0) NewY = 50;
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Positioning window at %d %d\n", FuncName, NewX, NewY);
   XtVaSetValues (New,
      XmNx, NewX,
      XmNy, NewY,
      NULL);
      
   SUMA_RETURNe;
}

/*** Functions to follow taken from editor_dnd.c example code 
 * Written by Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */
 
/*!
   \brief function to allocate and initialize a prompt dialogue structure
   
   \param Mode (SUMA_PROMPT_MODE) type of action area buttons desired
   \param init_selection (char *) the original value to set the text_field to
   \param daddy (Widget) the parent widget of the dialog
   \param preserve (SUMA_Boolean) if YUP then do not destroy widget and structure after use
   \param Return_button (SUMA_PROMPT_BUTTONS) make return (enter) in the text field
      mimick button Return_button (usually SUMA_OK_BUTTON OR SUMA_APPLY_BUTTON)
   \param SelectCallback 
   \param SelectData
   \param CancelCallback
   \param CancelData
   \param HelpCallback
   \param HelpData
   \param oprmpt (SUMA_PROMPT_DIALOG_STRUCT *) the structure to reuse. 
                  Use in conjuction with preserve=YUP
                  
   \sa warnings in SUMA_CreateFileSelectionDialogStruct
*/
SUMA_PROMPT_DIALOG_STRUCT *SUMA_CreatePromptDialogStruct (SUMA_PROMPT_MODE Mode, char *TextFieldLabel, 
                                                         char *init_selection, 
                                                         Widget daddy, SUMA_Boolean preserve,
                                                         SUMA_PROMPT_BUTTONS Return_button,
                                                         void(*SelectCallback)(char *selection, void *data), void *SelectData,
                                                         void(*CancelCallback)(void *data), void *CancelData,
                                                         void(*HelpCallback)(void *data), void *HelpData,
                                                         int(*VerifyFunction)(char *selection, void *data), void *VerifyData,
                                                         SUMA_PROMPT_DIALOG_STRUCT *oprmpt)
{
   static char FuncName[]={"SUMA_CreatePromptDialogStruct"};
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   if (!oprmpt) {
      SUMA_LH ("New prompt structure");
      prmpt = (SUMA_PROMPT_DIALOG_STRUCT *)
                  SUMA_calloc(1,sizeof(SUMA_PROMPT_DIALOG_STRUCT));
      if (!prmpt) {
         SUMA_SLP_Crit("Failed to allocate for prmpt");
         SUMA_RETURN(prmpt);
      }
      prmpt->daddy = daddy;
      prmpt->dialog = NULL;
      prmpt->pane = NULL;
      prmpt->text_w = NULL;
      prmpt->Mode = Mode;
   } else {
      SUMA_LH("Refitting old prompt structure.");
      prmpt = oprmpt;
      if (!preserve) SUMA_SLP_Warn("You should not be reusing\na prmpt structure along with\nthe Preserve flag on.");
      if (Mode != prmpt->Mode) SUMA_SLP_Warn("You cannot be reusing\na prmpt structure and change its mode.");
      if (prmpt->selection) SUMA_free(prmpt->selection);
      if (prmpt->label) SUMA_free(prmpt->label);
   }   
   
   prmpt->SelectCallback = SelectCallback;
   prmpt->SelectData = SelectData;
   prmpt->CancelCallback = CancelCallback;
   prmpt->CancelData = CancelData;
   prmpt->HelpCallback = HelpCallback;
   prmpt->HelpData = HelpData;
   prmpt->default_button = Return_button;
   prmpt->VerifyFunction = VerifyFunction;
   prmpt->VerifyData = VerifyData;
   
   if (init_selection) {
      prmpt->selection = (char *)SUMA_calloc(strlen(init_selection)+1, sizeof(char));
      prmpt->selection = strcpy(prmpt->selection, init_selection);
   }else {
      prmpt->selection = NULL;
   }
   if (TextFieldLabel) {
      prmpt->label = (char *)SUMA_calloc(strlen(TextFieldLabel)+1, sizeof(char));
      prmpt->label = strcpy(prmpt->label, TextFieldLabel);
   }else {
      prmpt->label = NULL;
   }
   prmpt->preserve = preserve;
   
   SUMA_RETURN(prmpt);
}

/*!
   \brief function to open a prompt 
*/
SUMA_PROMPT_DIALOG_STRUCT *SUMA_CreatePromptDialog(char *title_extension, SUMA_PROMPT_DIALOG_STRUCT *prmpt)
{
   static char FuncName[]={"SUMA_CreatePromptDialog"};
   Widget rc;
   XmString string;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!prmpt->dialog) {
      SUMA_LH ("Creating new prompt dialog.");
      /* The DialogShell is the Shell for this dialog.  Set it up so
      * that the "Close" button in the window manager's system menu
      * destroys the shell (it only unmaps it by default).
      */
      prmpt->dialog= XtVaCreatePopupShell ("dialog",
        xmDialogShellWidgetClass, prmpt->daddy,
        XmNtitle,  title_extension,     /* give arbitrary title in wm */
        XmNdeleteResponse, XmDO_NOTHING,  /* Unmap is the default and it is the best, 
                                             by I can't get an unmap callback for
                                             the stupid dialog shell. 
                                          */
        NULL);
      
      /* handle the close button from window manager */
      XmAddWMProtocolCallback(/* make "Close" window menu work */
         prmpt->dialog,
         XmInternAtom( XtDisplay(prmpt->dialog) , "WM_DELETE_WINDOW" , False ) ,
         SUMA_PromptUnmap_cb, (XtPointer) prmpt) ;
      
      
      /* Create the paned window as a child of the dialog.  This will
      * contain the control area and the action area
      * (created by CreateActionArea() using the action_items above).
      */
      prmpt->pane = XtVaCreateWidget ("pane", xmPanedWindowWidgetClass, prmpt->dialog,
        XmNsashWidth,  1,
        XmNsashHeight, 1,
        NULL);

      /* create the control area which contains a
      * Label gadget and a TextField widget.
      */
      rc = XtVaCreateWidget ("control_area", xmRowColumnWidgetClass, prmpt->pane, NULL);
      string = XmStringCreateLocalized (prmpt->label);
      XtVaCreateManagedWidget ("label", xmLabelWidgetClass, rc,
        XmNlabelString,    string,
        NULL);
      XmStringFree (string);

      prmpt->text_w = XtVaCreateManagedWidget ("text-field",
        xmTextFieldWidgetClass, rc, 
        NULL);
      
      if (prmpt->selection) {
         XtVaSetValues(prmpt->text_w, 
            XmNvalue, prmpt->selection,
            NULL);
      }

      /* add a callback for the return in the text-field widget */
      XtAddCallback (prmpt->text_w, XmNactivateCallback, SUMA_PromptActivate_cb, (XtPointer)prmpt);
      
      /* RowColumn is full -- now manage */
      XtManageChild (rc);
      
      
      /* Now create the action area */
      if (!SUMA_CreatePromptActionArea (prmpt)){
         SUMA_SLP_Crit("Failed to create action area.");
         SUMA_RETURN(NULL);
      }
      
      
       XtManageChild (prmpt->actionarea);
       XtManageChild (prmpt->pane);
       XtPopup (prmpt->dialog, XtGrabNone);
   }else {
      SUMA_LH ("bringing back old prompt dialog.");
      XtManageChild (prmpt->dialog);
      /* make sure that dialog is raised to top of window stack */
      /* 
         For some reason, the next line fails after opening the prompt more
         than twice!
         
         XMapRaised (XtDisplay (prmpt->dialog), XtWindow (XtParent (prmpt->dialog)));
         
         For some other reason, the following line works although it should be done by default
         when a widget is managed. ZSS May 14 03*/
      XtMapWidget (prmpt->dialog);
   }
   
   SUMA_RETURN(prmpt);
}

/*!
   \brief function to create the action area of the prompt 
*/
#define TIGHTNESS 20
const char * SUMA_PromptButtonLabel(SUMA_PROMPT_BUTTONS code)
{
   static char FuncName[]={"SUMA_CommandString"};
   
   SUMA_ENTRY;
   
   switch (code) {
      case SUMA_OK_BUTTON:
         SUMA_RETURN("OK");
      case SUMA_CLEAR_BUTTON:
         SUMA_RETURN("Clear");
      case SUMA_CANCEL_BUTTON:
         SUMA_RETURN("Cancel");
      case SUMA_HELP_BUTTON:
         SUMA_RETURN("Help");
      case SUMA_APPLY_BUTTON:
         SUMA_RETURN("Apply");
      default:
         SUMA_RETURN("BAD BAD BAD.");
   }
   SUMA_RETURN("This cannot be.");
}

SUMA_Boolean SUMA_CreatePromptActionArea (SUMA_PROMPT_DIALOG_STRUCT *prmpt)
{
   static char FuncName[]={"SUMA_CreatePromptActionArea"};
   int i, num_actions;
   Widget widget=NULL;
   SUMA_Boolean DoButt[SUMA_N_PROMPT_BUTTONS];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_LH ("Called");
   /* initialize DoButt */
   for (i=0; i < SUMA_N_PROMPT_BUTTONS; ++i) DoButt[i]=NOPE;
   
   /* Now set the flags for building the action area */
   num_actions = 0;
   switch (prmpt->Mode) {
      case SUMA_OK:
         DoButt[SUMA_OK_BUTTON] = YUP;
         num_actions = 1;
         break;
      case SUMA_OK_HELP:
         DoButt[SUMA_OK_BUTTON] = DoButt[SUMA_HELP_BUTTON] = YUP;
         num_actions = 2;
         break;
      case SUMA_OK_CANCEL:
         DoButt[SUMA_OK_BUTTON] = DoButt[SUMA_CANCEL_BUTTON] = YUP;
         num_actions = 2;
         break;
      case SUMA_OK_CANCEL_HELP:
         DoButt[SUMA_OK_BUTTON] = DoButt[SUMA_CANCEL_BUTTON] = 
         DoButt[SUMA_HELP_BUTTON] = YUP;
         num_actions = 3;
         break;               
      case SUMA_OK_CLEAR_CANCEL:
         DoButt[SUMA_OK_BUTTON] = DoButt[SUMA_CANCEL_BUTTON] = 
         DoButt[SUMA_CLEAR_BUTTON] = YUP;
         num_actions = 3;
         break;
      case SUMA_OK_CLEAR_CANCEL_HELP:
         DoButt[SUMA_OK_BUTTON] = DoButt[SUMA_CANCEL_BUTTON] = 
         DoButt[SUMA_CLEAR_BUTTON] = DoButt[SUMA_HELP_BUTTON] = YUP;
         num_actions = 4;
         break;
      case SUMA_OK_APPLY_CANCEL:
         DoButt[SUMA_OK_BUTTON] = DoButt[SUMA_CANCEL_BUTTON] = 
         DoButt[SUMA_APPLY_BUTTON] = YUP;
         num_actions = 3;
         break;
      case SUMA_OK_APPLY_CANCEL_HELP:
         DoButt[SUMA_OK_BUTTON] = DoButt[SUMA_CANCEL_BUTTON] = 
         DoButt[SUMA_APPLY_BUTTON] = DoButt[SUMA_HELP_BUTTON] = YUP;
         num_actions = 4;
         break;
      case SUMA_OK_APPLY_CLEAR_CANCEL:
         DoButt[SUMA_OK_BUTTON] = DoButt[SUMA_CANCEL_BUTTON] = 
         DoButt[SUMA_APPLY_BUTTON] = DoButt[SUMA_CLEAR_BUTTON] = YUP;
         num_actions = 4;
         break;
      case SUMA_OK_APPLY_CLEAR_CANCEL_HELP:
         DoButt[SUMA_OK_BUTTON] = DoButt[SUMA_CANCEL_BUTTON] = 
         DoButt[SUMA_APPLY_BUTTON] = DoButt[SUMA_CLEAR_BUTTON] = 
         DoButt[SUMA_HELP_BUTTON] = YUP;
         num_actions = 5;
         break;
      default:
         SUMA_SL_Err("Bad prompt mode.");
         SUMA_RETURN(NOPE);
         break;
   }

   prmpt->actionarea = XtVaCreateWidget ("action_area", xmFormWidgetClass, prmpt->pane,
        XmNfractionBase, TIGHTNESS*num_actions - 1,
        XmNleftOffset,   10,
        XmNrightOffset,  10,
        NULL);
        
   /* create the buttons */
   for (i=0; i< SUMA_N_PROMPT_BUTTONS; ++i) {
      if (DoButt[i]) {
         widget = XtVaCreateManagedWidget (SUMA_PromptButtonLabel(i),
            xmPushButtonWidgetClass, prmpt->actionarea,
            XmNleftAttachment,       i? XmATTACH_POSITION : XmATTACH_FORM,
            XmNleftPosition,         TIGHTNESS*i,
            XmNtopAttachment,        XmATTACH_FORM,
            XmNbottomAttachment,     XmATTACH_FORM,
            XmNrightAttachment,
                i != num_actions - 1 ? XmATTACH_POSITION : XmATTACH_FORM,
            XmNrightPosition,        TIGHTNESS * i + (TIGHTNESS - 1),
            XmNshowAsDefault,        i == 0, 
            XmNdefaultButtonShadowThickness, 1, 
            NULL);      
      }
      if (i == prmpt->default_button) {
         /* Set the action_area's default button  Also, set the
          * pane window constraint for max and min heights so this
          * particular pane in the PanedWindow is not resizable.
          */
         Dimension height=0, h=0;
         XtVaGetValues (prmpt->actionarea, XmNmarginHeight, &h, NULL);
         XtVaGetValues (widget, XmNheight, &height, NULL);
         height += 2 * h;
         XtVaSetValues (prmpt->actionarea,
             XmNdefaultButton, widget,
             XmNpaneMaximum,   height,
             XmNpaneMinimum,   height,
             NULL);
      }

      /* Now set the callbacks */
      switch (i) {
         case SUMA_OK_BUTTON:
            XtAddCallback (widget, XmNactivateCallback, SUMA_PromptOk_cb, (XtPointer)prmpt);
            break;
         case SUMA_CLEAR_BUTTON:
            XtAddCallback (widget, XmNactivateCallback, SUMA_PromptClear_cb, (XtPointer)prmpt);
            break;
         case SUMA_CANCEL_BUTTON:
            XtAddCallback (widget, XmNactivateCallback, SUMA_PromptCancel_cb, (XtPointer)prmpt);
            break;
         case SUMA_APPLY_BUTTON:
            XtAddCallback (widget, XmNactivateCallback, SUMA_PromptApply_cb, (XtPointer)prmpt);
            break;
         case SUMA_HELP_BUTTON:
            XtAddCallback (widget, XmNactivateCallback, SUMA_PromptHelp_cb, (XtPointer)prmpt);
            break;
         default:
            SUMA_SL_Err("Bad action area button label");
            break;
      }   
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Called when prompt dialog is being unmapped.
   This happens when users enter a selection, hit cancel or hit the kill button on the window 

   -expects a SUMA_PROMPT_DIALOG_STRUCT *in data

*/
void SUMA_PromptUnmap_cb (Widget w, XtPointer data, XtPointer calldata)
{
   static char FuncName[]={"SUMA_PromptUnmap_cb"};
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   prmpt = (SUMA_PROMPT_DIALOG_STRUCT *)data;
   
   /* if preservation is not required, kill the widget and free dlg */
   if (!prmpt->preserve) {
      if (prmpt->dialog) {
         SUMA_LH("Destroying prompt");
         XtDestroyWidget(prmpt->dialog); 
      }else {
         SUMA_SL_Warn("prmpt->dialog is null.\nThis should not be.");
      }
      
      /* now free the structure */
      SUMA_FreePromptDialogStruct(prmpt);
      
   }else {
      SUMA_LH("Preserving prompt");
      XtUnmapWidget (prmpt->dialog); 
   }   
   
   SUMA_RETURNe;
}

/*!
   \brief Call from Activate button in prompt dialog
   
   -expects a SUMA_PROMPT_DIALOG_STRUCT *in data
*/
void SUMA_PromptActivate_cb (Widget w, XtPointer data, XtPointer calldata)
{
   static char FuncName[]={"SUMA_PromptActivate_cb"};
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) calldata;
   Widget dflt = NULL;
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   prmpt = (SUMA_PROMPT_DIALOG_STRUCT *)data;
   
   /* get the "default button" from the action area... */
    XtVaGetValues (prmpt->actionarea, XmNdefaultButton, &dflt, NULL);
    if (dflt) /* sanity check -- this better work */
        /* make the default button think it got pushed using
         * XtCallActionProc().  This function causes the button
         * to appear to be activated as if the user pressed it.
         */
        XtCallActionProc (dflt, "ArmAndActivate", cbs->event, NULL, 0);
   
   
   SUMA_RETURNe;
}

/*!
   \brief Call from Ok button in prompt dialog
   
   -expects a SUMA_PROMPT_DIALOG_STRUCT *in data
*/
void SUMA_PromptOk_cb (Widget w, XtPointer data, XtPointer calldata)
{
   static char FuncName[]={"SUMA_PromptOk_cb"};
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Called");

   prmpt = (SUMA_PROMPT_DIALOG_STRUCT *)data;
   
   /* apply first */
   SUMA_PromptApply_cb (w, data, calldata);

   /* close window */
   SUMA_PromptUnmap_cb (w, data, calldata);
   
   SUMA_RETURNe;
}

/*!
   \brief Call from Clear button in prompt dialog
   
   -expects a SUMA_PROMPT_DIALOG_STRUCT *in data
*/
void SUMA_PromptClear_cb (Widget w, XtPointer data, XtPointer calldata)
{
   static char FuncName[]={"SUMA_PromptClear_cb"};
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   prmpt = (SUMA_PROMPT_DIALOG_STRUCT *)data;
  
   XmTextFieldSetString (prmpt->text_w, "");
   
   SUMA_RETURNe;
}

/*!
   \brief Call from Apply button in prompt dialog
   
   -expects a SUMA_PROMPT_DIALOG_STRUCT *in data
*/
void SUMA_PromptApply_cb (Widget w, XtPointer data, XtPointer calldata)
{
   static char FuncName[]={"SUMA_PromptApply_cb"};
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   char *text=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   prmpt = (SUMA_PROMPT_DIALOG_STRUCT *)data;
   
   text = XmTextFieldGetString (prmpt->text_w);
   
   if (prmpt->selection) SUMA_free(prmpt->selection);
   if (text[0]) { 
      prmpt->selection = (char *)SUMA_calloc(strlen(text)+1,sizeof(char));
      prmpt->selection = strcpy(prmpt->selection, text);
   }else {
      prmpt->selection = NULL;
   }
   XtFree (text);
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Read %s\n", FuncName, prmpt->selection);

   /* verify the input */
   if (prmpt->VerifyFunction) {
      if (!prmpt->VerifyFunction(prmpt->selection, prmpt->VerifyData)) {
         SUMA_SLP_Err("Gibberish! try again.\nSyntax error or wrong\nnumber/type of arguments.");
         SUMA_RETURNe;
      }
   }
   
   /* do your selectcallback */
   if (prmpt->SelectCallback) {
      prmpt->SelectCallback (prmpt->selection, prmpt->SelectData);
   }
   
   
   SUMA_RETURNe;
}

/*!
   \brief Call from Cancel button in prompt dialog
   
   -expects a SUMA_PROMPT_DIALOG_STRUCT *in data
*/
void SUMA_PromptCancel_cb (Widget w, XtPointer data, XtPointer calldata)
{
   static char FuncName[]={"SUMA_PromptCancel_cb"};
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   prmpt = (SUMA_PROMPT_DIALOG_STRUCT *)data;
   
   /* do your cancelcallback */
   if (prmpt->CancelCallback) {
      prmpt->CancelCallback (prmpt->CancelData);
   }
   
   /* close window */
   SUMA_PromptUnmap_cb (w, data, calldata);

   SUMA_RETURNe;
}

/*!
   \brief Call from Help button in prompt dialog
   
   -expects a SUMA_PROMPT_DIALOG_STRUCT *in data
*/
void SUMA_PromptHelp_cb (Widget w, XtPointer data, XtPointer calldata)
{
   static char FuncName[]={"SUMA_PromptHelp_cb"};
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   prmpt = (SUMA_PROMPT_DIALOG_STRUCT *)data;
   
   /* do your helpcallback */
   if (prmpt->HelpCallback) {
      prmpt->HelpCallback (prmpt->HelpData);
   }
   
   SUMA_RETURNe;
}

/*!
   \brief frees prompt dialog structure. 
   It does not destroy the widget for the dialog, that should be done
   before this function is called.
   
*/
void SUMA_FreePromptDialogStruct(SUMA_PROMPT_DIALOG_STRUCT *prmpt)
{
   static char FuncName[]={"SUMA_FreePromptDialogStruct"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   SUMA_LH("Called");
   
   if (!prmpt) SUMA_RETURNe;
   
   /* now free structure */
   if (prmpt->selection) SUMA_free(prmpt->selection);
   if (prmpt->label) SUMA_free(prmpt->label); 
   SUMA_free(prmpt);
   
   SUMA_RETURNe;
}

/*!
   \brief function to allocate and initialize a file selection dialogue structure
   
   \param parent (Widget) parent widget of dialog
   \param Mode (SUMA_FILE_SELECT_MODE) : SUMA_OPEN_FILE, SUMA_SAVE_FILE
   \param preserve (SUMA_Boolean) YUP: keep dialog widget and structure after closing.
                                    see notes in structure's definition for more help.
   \param SelectCallback(char *filename, void *data) (void*): Function to call when selection is made
   \param SelectData (void *) data to pass SelectCallback
   \param CancelCallback(void *data) (void*): Function to call when cancel is pressed
   \param CancelData (void *) data to pass CancelCallback
   \param FilePattern (char *) pattern to use as initial file filter.
            FilePattern is copied so you should handle freeing it if necessary.
            If FilePattern = NULL then the one that was used last is preserved.
   \param odlg (SUMA_SELECTION_DIALOG_STRUCT *) if not null, then instead of 
            creating a new structure, the function will update the fields in 
            odlg. This is useful if you want to reuse a dialog's widget (preserve = YUP)
            but with different callbacks and calldata.
            NOTE: parent cannot be changed if you are updating odlg.
            NOTE: DO NOT USE preserve = YUP along with non-NULL odlg. It does not make sense
            and I can't easily make this fool proof.
   \return dlg_strct (SUMA_SELECTION_DIALOG_STRUCT *) an allocated and initialized dialogue struct
               or an updated odlg if specified. 
    
   -You might have to add a trap for the kill button, if possible, else you need to trap for the kill with
   a callback to set the widget to null.
   
   See SUMA_DestroyTextShell
*/
SUMA_SELECTION_DIALOG_STRUCT *SUMA_CreateFileSelectionDialogStruct (
   Widget daddy, 
   SUMA_FILE_SELECT_MODE Mode, 
   SUMA_Boolean preserve,
   void (*SelectCallback)(char *filename, void *data), 
   void *SelectData,
   void (*CancelCallback)(void *data), 
   void *CancelData,
   char *FilePattern,
   SUMA_SELECTION_DIALOG_STRUCT *odlg)
{
   static char FuncName[]={"SUMA_CreateFileSelectionDialogStruct"};
   SUMA_SELECTION_DIALOG_STRUCT * dlg = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!odlg) { /* new structure */
      SUMA_LH("A new structure ");    
      dlg = (SUMA_SELECTION_DIALOG_STRUCT *)
                  SUMA_calloc(1,sizeof(SUMA_SELECTION_DIALOG_STRUCT));
      if (!dlg) {
         fprintf (SUMA_STDERR, 
                  "Error %s: Failed to allocate for TextShell.\n", 
                  FuncName);
         SUMA_RETURN (NULL);
      }
      dlg->dlg_w = NULL;
      dlg->FilePattern = NULL;
   }else {
      SUMA_LH("Refitting old one. ");
      if (!preserve) 
         SUMA_SLP_Warn( "You should not be reusing\n"
                        "a dlg structure along with\n"
                        "the Preserve flag on.");
      dlg = odlg;
      if (dlg->filename) {
         SUMA_LH("Freeing dlg->filename");
         SUMA_free(dlg->filename);
      }
   }
   
   dlg->daddy = daddy; 
   dlg->filename = NULL;
   dlg->Mode = Mode;
   dlg->SelectCallback = SelectCallback;
   dlg->SelectData = SelectData;
   dlg->CancelCallback = CancelCallback;
   dlg->CancelData = CancelData;
   dlg->preserve = preserve;
   
   if (FilePattern) {
      /* new one specified, destroy the old one */
      if (dlg->FilePattern) {
         SUMA_LHv("Restting dlg->FilePattern to %s\n", FilePattern);
         SUMA_free(dlg->FilePattern);
      }   
      dlg->FilePattern = SUMA_copy_string (FilePattern);
   }
   
   SUMA_RETURN(dlg);
}

/*!
   \brief, opens a file selection dialogue
   
   \param title (char *) title of window
   \param dlg (SUMA_SELECTION_DIALOG_STRUCT **) pointer to structure created and initialized by SUMA_CreateFileSelectionDialogStruct
*/                                                            
SUMA_SELECTION_DIALOG_STRUCT *SUMA_CreateFileSelectionDialog (
      char *title_extension, 
      SUMA_SELECTION_DIALOG_STRUCT **dlgp)
{
   static char FuncName[]={"SUMA_CreateFileSelectionDialog"};
   static char *last_title_extension=NULL;
   SUMA_Boolean LocalHead = NOPE, same;
   SUMA_SELECTION_DIALOG_STRUCT *dlg = NULL;
   XmString button, title, pattern=NULL;

   SUMA_ENTRY;
   
   dlg = *dlgp;   
   if (!dlg->dlg_w) {/* need to create it for the first time */
      SUMA_LH ("Creating new file selection window.");
      dlg->dlg_w = XmCreateFileSelectionDialog (dlg->daddy, "Files", NULL, 0);
      
      
      XtVaSetValues (dlg->dlg_w,
         XmNdeleteResponse, XmUNMAP,  /* system menu "Close" action */
        NULL);
        
      /* you can't cancel the kill button's effect, 
      the way you do for toplevel shells. 
      But it does appear that the kill button is just 
      unmanaging the widget, which is fine.
      see my modified action_area.c file
      */
   } else { 
      SUMA_LH ("Updating");
      /* update and raise dialogue, 
        that is done next. 
        For the moment, remove pre-existing callbacks*/
      XtRemoveAllCallbacks (dlg->dlg_w, XmNcancelCallback);
      XtRemoveAllCallbacks (dlg->dlg_w, XmNokCallback);
      XtRemoveAllCallbacks (dlg->dlg_w, XmNunmapCallback);
   }
      
   same = NOPE;
   if (title_extension) {
      if (last_title_extension) {
   SUMA_LHv("last: %s, this: %s\n", last_title_extension, title_extension);
         if (strcmp(last_title_extension, title_extension) == 0) 
            { same = YUP; }
         SUMA_free(last_title_extension); last_title_extension=NULL;
      }
      last_title_extension = SUMA_copy_string( title_extension);
   }
   if (dlg->FilePattern ) { 
      if (!same) {
         pattern = XmStringCreateLocalized (dlg->FilePattern);
         XtVaSetValues (dlg->dlg_w,
            XmNpattern, pattern,
            NULL);
         XmStringFree (pattern);
      } else { 
         XtVaGetValues(dlg->dlg_w, XmNpattern, &pattern, NULL);
         if (pattern) {
            XmFileSelectionDoSearch(dlg->dlg_w, pattern); 
            XmStringFree (pattern); 
         }
      }
   }
      
   XtAddCallback (dlg->dlg_w, 
                  XmNcancelCallback, SUMA_FileSelection_popdown_cb,
                  (XtPointer)dlg);
   XtAddCallback (dlg->dlg_w, 
                  XmNokCallback, SUMA_FileSelection_file_select_cb, 
                  (XtPointer)dlg);
   XtAddCallback (dlg->dlg_w, 
                  XmNunmapCallback, SUMA_FileSelection_Unmap_cb,
                  (XtPointer)dlgp);

   if (dlg->Mode == SUMA_FILE_OPEN) {
      button = XmStringCreateLocalized ("Open");
      title = XmStringCreateLocalized (title_extension);
   } 
   else { /* dlg->Mode == SUMA_FILE_SAVE */
     button = XmStringCreateLocalized ("Save");
     title = XmStringCreateLocalized (title_extension);
   }
   XtVaSetValues (dlg->dlg_w,
     XmNokLabelString, button,
     XmNdialogTitle,   title,
     NULL);

   XmStringFree (button);
   XmStringFree (title);

   XtManageChild (dlg->dlg_w);
      
   /* make sure that dialog is raised to top of window stack */
   XMapRaised (XtDisplay (dlg->dlg_w), XtWindow (XtParent (dlg->dlg_w)));      
   
   SUMA_RETURN(dlg);
}
/*!
 \brief sample callback routine for "Cancel" button in FileSelectionDialogs 
 SUMA_SELECTION_DIALOG_STRUCT *
*/
void SUMA_FileSelection_popdown_cb (Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_FileSelection_popdown_cb"};
   SUMA_SELECTION_DIALOG_STRUCT *dlg;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   dlg = (SUMA_SELECTION_DIALOG_STRUCT *)client_data;
   

   /* do the callback for the cancel */
   if (dlg->CancelCallback) {
      dlg->CancelCallback(dlg->CancelData);
   }
   
   XtUnmanageChild (dlg->dlg_w);
   
   SUMA_RETURNe;
}
/*!
 \brief sample callback routine for killing window in FileSelectionDialogs.
 That happens when users hit the X on the dialog
 This function destroys the widget and frees the structure if no preservation is needed
 
 -expect SUMA_SELECTION_DIALOG_STRUCT ** in client_data
*/
void SUMA_FileSelection_Unmap_cb (
   Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_FileSelection_Unmap_cb"};
   SUMA_SELECTION_DIALOG_STRUCT *dlg;
   SUMA_SELECTION_DIALOG_STRUCT **dlgp;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   dlgp = (SUMA_SELECTION_DIALOG_STRUCT **)client_data;
   dlg = *dlgp;
   
   /* if preservation is not required, kill the widget and free dlg */
   if (!dlg->preserve) {
      if (dlg->dlg_w) {
         SUMA_LH("Destroying dlg");
         XtDestroyWidget(dlg->dlg_w); 
      }else {
         SUMA_SL_Warn("dlg_w is null.\nThis should not be.");
      }
      
      /* now free the structure */
      SUMA_FreeFileSelectionDialogStruct(dlg);
      *dlgp = NULL;
      
   }
   
   SUMA_RETURNe;
}

/*!
   \brief Free the structure for creating a file selection dialog
   
*/
void SUMA_FreeFileSelectionDialogStruct(SUMA_SELECTION_DIALOG_STRUCT *dlg)
{
   static char FuncName[]={"SUMA_FreeFileSelectionDialogStruct"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   SUMA_LH("Called");
   
   
   if (!dlg) SUMA_RETURNe;
   
   /* now free structure */
   if (dlg->filename) SUMA_free(dlg->filename);
   if (dlg->FilePattern) SUMA_free(dlg->FilePattern);
   SUMA_free(dlg);
   
   SUMA_RETURNe;
}

/*! 
callback routine for "OK" button in FileSelectionDialogs 
*/
void SUMA_FileSelection_file_select_cb(Widget dialog, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_FileSelection_file_select_cb"};
   char buf[256], *filename;
   struct stat statb;
   FILE *fp=NULL;
   SUMA_SELECTION_DIALOG_STRUCT *dlg;
   XmFileSelectionBoxCallbackStruct *cbs =
     (XmFileSelectionBoxCallbackStruct *) call_data;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   SUMA_LH("Called");
   
   dlg = (SUMA_SELECTION_DIALOG_STRUCT *)client_data;

   /* clear old filename */
   if (dlg->filename) {
      SUMA_free(dlg->filename); 
      dlg->filename = NULL;
   }

   if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename))
      SUMA_RETURNe; /* must have been an internal error */

   if (filename[0] == '\0') {
      XtFree (filename);
      XBell (XtDisplay (dlg->daddy), 50);
      SUMA_RETURNe; /* nothing typed */
   }

   if (dlg->Mode == SUMA_FILE_SAVE) {
      /* here you could do some tests on the file given the
      options that would be specified in dlg */
      /* Do not carry out tests here as filename might change once proper extensions are added */
      #if 0
      if (!(fp = fopen (filename, "w"))) {
         perror (filename);
         sprintf (buf, "Can't save to %s.", filename);
         SUMA_SLP_Err(buf);
         XtFree(filename);
         SUMA_RETURNe;
      }
      #endif
      
   } 
   else { /* reason == FILE_OPEN */
     /* here you could do some tests on the file given the
     options that would be specified in dlg */
     /* make sure the file is a regular text file and open it */
     if (stat (filename, &statb) == -1 ||
             (statb.st_mode & S_IFMT) != S_IFREG ||
             !(fp = fopen (filename, "r"))) {
         perror (filename);
         sprintf (buf, "Can't read %s.", filename);
         SUMA_SLP_Err(buf);
         XtFree(filename);
         SUMA_RETURNe;
     }
   }

   /* store the filename */
   if (filename) {
      dlg->filename = (char *)SUMA_calloc(strlen(filename)+1, sizeof(char));
      dlg->filename = strcpy(dlg->filename, filename);
   }

   /* free all allocated space. */
   XtFree (filename);
   if (fp) fclose (fp);

   /* Now do the SelectCallback */
   if (dlg->SelectCallback) {
      dlg->SelectCallback(dlg->filename, dlg->SelectData);
   } 

   XtUnmanageChild (dlg->dlg_w); /* this function will call the unmap callback which will 
                                    do the destruction if dialog is not to be preserved */

   SUMA_RETURNe;
}

/* This function is for hiding the color plane frame */
void  SUMA_cb_ToggleManagementColPlaneWidget(Widget w, XtPointer data, 
                                             XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ToggleManagementColPlaneWidget"};
   static int ncall=1;
   SUMA_SurfaceObject *SO = NULL;
   void **curSOp;
   int xx, yy, notest = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   curSOp = (void **)data;
   SO = (SUMA_SurfaceObject *)(*curSOp);
   
#ifdef USING_LESSTIF
      notest = 1;
#endif

   if (ncall > 0) {
      if (XtIsManaged(SO->SurfCont->ColPlane_fr)) {
         SUMA_LHv("UnM.ch: ColPlane_fr, ncall %d\n", ncall);
         XtUnmanageChild(SO->SurfCont->ColPlane_fr);
      }
      if( notest ) {
         SUMA_LHv("UnM.ch: DsetMap_fr, ncall %d\n", ncall);
         XtUnmanageChild(SO->SurfCont->DsetMap_fr); /* needed */
      }   
      /* if nothing else remains in the parent of ColPlane, 
         then unmanage its parent (rc_right) too. 
         *** Parent of that frame is now rc_left    May 25 04*/
      /* XtUnmanageChild(XtParent(SO->SurfCont->ColPlane_fr)); May 25 04*/
      if (XtIsManaged(SO->SurfCont->DsetMap_fr)) {
         SUMA_LHv("UnM.ch: DsetMap_fr, par(DsetMap_fr), ncall %d\n", ncall);
         XtUnmanageChild(SO->SurfCont->DsetMap_fr);
         XtUnmanageChild(XtParent(SO->SurfCont->DsetMap_fr));
      }
   } else {
      /* XtManageChild(XtParent(SO->SurfCont->ColPlane_fr)); May 25 04*/
      if (strcmp(SO->SurfCont->curColPlane->cmapname, "explicit") != 0) { 
         SUMA_LHv("Not explicit, ncall = %d\n", ncall);
         /* not an RGB dset */
         if (!XtIsManaged(SO->SurfCont->DsetMap_fr)) {
            SUMA_LHv("M.ch: par(DsetMap_fr) DsetMap_fr, ncall = %d\n", ncall);
            XtManageChild(XtParent(SO->SurfCont->DsetMap_fr));
            XtManageChild((Widget)SO->SurfCont->DsetMap_fr);
         }
      }

      SUMA_LHv("M.ch: ColPlane_fr, ncall = %d\n", ncall);
      XtManageChild((Widget)SO->SurfCont->ColPlane_fr);
      
      SUMA_LHv("Map.R: ColPlane_fr, ncall = %d\n", ncall);
      XMapRaised (XtDisplay(SO->SurfCont->ColPlane_fr), 
                  XtWindow(SO->SurfCont->TopLevelShell));
   }
   
   ncall *= -1;
   SUMA_RETURNe;
}


/*!
   Load colorplane
   
   expects SO in data and a calling widget in w 
*/
void SUMA_cb_ColPlane_Load(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ColPlane_Load"};
   SUMA_LIST_WIDGET *LW=NULL;
   SUMA_SurfaceObject *SO=NULL;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_OpenColFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, (void *)data,
                                          SES_Suma, NULL, NOPE,
                                          SEI_Head, NULL))) {
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
   }
   if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, (int *)w,
                                          SES_Suma, NULL, NOPE,
                                          SEI_In, NextElm)) {
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
   }
   
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
   SUMA_RETURNe;
}

/*!
   Load Dset
   
   expects SO in data and a calling widget in w 
*/
void SUMA_cb_Dset_Load(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_Dset_Load"};
   SUMA_LIST_WIDGET *LW=NULL;
   SUMA_SurfaceObject *SO=NULL;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_OpenDsetFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, (void *)data,
                                          SES_Suma, NULL, NOPE,
                                          SEI_Head, NULL))) {
      fprintf (SUMA_STDERR, 
         "Error %s: Failed to register command.\n", FuncName);
   }
   if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, (int *)w,
                                          SES_Suma, NULL, NOPE,
                                          SEI_In, NextElm)) {
      fprintf (SUMA_STDERR, 
         "Error %s: Failed to register command.\n", FuncName);
   }
   
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, 
         "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
   SUMA_RETURNe;
}

/*!
   Delete colorplane
   
   expects SO in data 
*/
void SUMA_cb_ColPlane_Delete(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ColPlane_Delete"};
   SUMA_LIST_WIDGET *LW=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SUMA_RETURNe;
   
   #if 0
   SO = (SUMA_SurfaceObject *)data;
   
   LW is not set yet ...
   
   /*close the list widget if open */
   if (!LW->isShaded) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Closing switch Color plane window ...\n", FuncName);
      SUMA_cb_CloseSwitchColPlane( w,  (XtPointer)SO->SurfCont->SwitchDsetlst,  client_data);
   }  
   #endif       
   SUMA_RETURNe;
}

/*!
   \brief create a forced answer dialog for replacing files
   \return SUMA_YES SUMA_NO SUMA_YES_ALL or SUMA_NO_ALL
   
*/
int SUMA_AskUser_File_replace(Widget parent, char *question, int default_ans)
{
    static char FuncName[]={"SUMA_AskUser_File_replace"};
    static Widget dialog; /* static to avoid multiple creation */
    Widget YesWid, NoWid, HelpWid;
    XmString text, yes, no;
    static int answer;

   SUMA_ENTRY;

   if (!dialog) {
     dialog = XmCreateQuestionDialog (parent, "dialog", NULL, 0);
     XtVaSetValues (dialog,
         XmNdialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
         NULL);
     XtSetSensitive (
         XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON),
         False);
     XtAddCallback (dialog, XmNokCallback, SUMA_response, &answer);
     XtAddCallback (dialog, XmNcancelCallback, SUMA_response, &answer);
    /* Now add a special extra cute little button */
    {
       XmString NewButt;
       Widget NewButt_button = NULL;
      
       NewButt= XmStringCreateLocalized ("Yes All");
       NewButt_button = XtVaCreateManagedWidget("Yes All", 
         xmPushButtonWidgetClass, dialog,
         XmNlabelString, NewButt,
         NULL);
       XtVaSetValues(NewButt_button, XmNuserData, SUMA_YES_ALL, NULL);
       XtAddCallback (NewButt_button, XmNactivateCallback, SUMA_response, &answer);
       XmStringFree (NewButt);
       

       NewButt= XmStringCreateLocalized ("No");
       NewButt_button = XtVaCreateManagedWidget("No", 
         xmPushButtonWidgetClass, dialog,
         XmNlabelString, NewButt,
         NULL);
       XtVaSetValues(NewButt_button, XmNuserData, SUMA_NO, NULL);
       XtAddCallback (NewButt_button, XmNactivateCallback, SUMA_response, &answer);
       XmStringFree (NewButt);
          
    }

    }
   answer = SUMA_NO_ANSWER;
   text = XmStringCreateLocalized (question);
   yes = XmStringCreateLocalized ("Yes");
   no = XmStringCreateLocalized ("No All");
   XtVaSetValues (dialog,
     XmNmessageString,      text,
     XmNokLabelString,      yes,
     XmNcancelLabelString,  no,
     XmNdefaultButtonType,  default_ans == SUMA_YES ?
         XmDIALOG_OK_BUTTON : XmDIALOG_CANCEL_BUTTON,
     NULL);
   XmStringFree (text);
   XmStringFree (yes);
   XmStringFree (no);

   /* set the values of the standrard buttons */
   YesWid = XmMessageBoxGetChild(dialog, XmDIALOG_OK_BUTTON);
   XtVaSetValues(YesWid, XmNuserData, SUMA_YES, NULL);
   NoWid = XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON);
   XtVaSetValues(NoWid, XmNuserData, SUMA_NO_ALL, NULL);
   HelpWid = XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON);
   XtVaSetValues(HelpWid, XmNuserData, SUMA_HELP, NULL);

   /* unmanage the Help button because I am not using it here */
   XtUnmanageChild(HelpWid);
   
   XtManageChild (dialog);
   XtPopup (XtParent (dialog), XtGrabNone);

   while (answer == SUMA_NO_ANSWER)
     XtAppProcessEvent (SUMAg_CF->X->App, XtIMAll);

   XtPopdown (XtParent (dialog));
   /* make sure the dialog goes away before returning. Sync with server
   * and update the display.
   */
   XSync (XtDisplay (dialog), 0);
   XmUpdateDisplay (parent);

   SUMA_RETURN(answer);
}


/*!
  Supposed to behave like ForceUser function below but only for pausing.
*/
int SUMA_PauseForUser(Widget parent, char *question, SUMA_WINDOW_POSITION pos)
{
   static char FuncName[]={"SUMA_PauseForUser"};
   static Widget dialog; /* static to avoid multiple creation */
   Widget YesWid;
   int ii;
   XmString text, yes;
   static int answer;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!parent) {
      /* look for the first non-null sv->X->TOPLEVEL */
      ii = 0;
      while (ii<SUMAg_N_SVv && !(parent=SUMAg_SVv[ii].X->TOPLEVEL)) {
         ++ii;
      }
   }
   if (!parent) { /* no widgets, go command line */
      SUMA_PAUSE_PROMPT(question);
      SUMA_RETURN(SUMA_YES);
   }
   
   if (!dialog) {
     SUMA_LH("Creating Dialog");
     dialog = XmCreateQuestionDialog (parent, "dialog", NULL, 0);
     XtVaSetValues (dialog,
         XmNdialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
         NULL);
     /* Don't need help and cancel buttons */
     XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
     XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
     
     XtAddCallback (dialog, XmNokCallback, SUMA_response, &answer); 
    } else {
      SUMA_LH("Reusing Dialog (SLOW SLOW SLOW)");
    }
    
   answer = SUMA_NO_ANSWER;
   text = XmStringCreateLocalized (question);
   yes = XmStringCreateLocalized ("Yes");
   XtVaSetValues (dialog,
     XmNmessageString,      text,
     XmNokLabelString,      yes,
     XmNdefaultButtonType,  XmDIALOG_OK_BUTTON,
     NULL);
   XmStringFree (text);
   XmStringFree (yes);

   /* set the values of the standard buttons */
   YesWid = XmMessageBoxGetChild(dialog, XmDIALOG_OK_BUTTON);
   XtVaSetValues(YesWid, XmNuserData, SUMA_YES, NULL);

   XtManageChild (dialog);
   
   XtPopup (XtParent (dialog), XtGrabNone);
   
   if (pos != SWP_DONT_CARE) SUMA_PositionWindowRelative(dialog, parent, pos);
   
   while (answer == SUMA_NO_ANSWER)
     XtAppProcessEvent (SUMAg_CF->X->App, XtIMAll);
      
   #if 1
      XtDestroyWidget(dialog); 
      dialog = NULL;
   #else /* bad, takes for ever to come back up. 
               Same for repeated calls of ForceUser if created for the first time from DriveSuma and
               not from the interface with, say 'shft+Esc' 
               See bit of illustration code in SUMA_Engine where PauseForUser is called*/ 
      XtUnmanageChild(dialog);
   #endif
   SUMA_RETURN(answer);
}

/*!
   \brief create a forced answer dialog YES/NO 
   
   set pos to SWP_DONT_CARE (recommended) if you don't want to specify where
   the prompt goes
   \return SUMA_YES SUMA_NO 
*/
int SUMA_ForceUser_YesNo(Widget parent, char *question, int default_ans, SUMA_WINDOW_POSITION pos)
{
    static char FuncName[]={"SUMA_ForceUser_YesNo"};
    static Widget dialog; /* static to avoid multiple creation */
    Widget YesWid, NoWid, HelpWid;
    XmString text, yes, no;
    static int answer;

   SUMA_ENTRY;

   if (!dialog) {
     dialog = XmCreateQuestionDialog (parent, "dialog", NULL, 0);
     XtVaSetValues (dialog,
         XmNdialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
         NULL);
     XtSetSensitive (
         XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON),
         False);
     XtAddCallback (dialog, XmNokCallback, SUMA_response, &answer);
     XtAddCallback (dialog, XmNcancelCallback, SUMA_response, &answer);

    }
   answer = SUMA_NO_ANSWER;
   text = XmStringCreateLocalized (question);
   yes = XmStringCreateLocalized ("Yes");
   no = XmStringCreateLocalized ("No");
   XtVaSetValues (dialog,
     XmNmessageString,      text,
     XmNokLabelString,      yes,
     XmNcancelLabelString,  no,
     XmNdefaultButtonType,  default_ans == SUMA_YES ?
         XmDIALOG_OK_BUTTON : XmDIALOG_CANCEL_BUTTON,
     NULL);
   XmStringFree (text);
   XmStringFree (yes);
   XmStringFree (no);

   /* set the values of the standard buttons */
   YesWid = XmMessageBoxGetChild(dialog, XmDIALOG_OK_BUTTON);
   XtVaSetValues(YesWid, XmNuserData, SUMA_YES, NULL);
   NoWid = XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON);
   XtVaSetValues(NoWid, XmNuserData, SUMA_NO_ALL, NULL);
   HelpWid = XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON);
   XtVaSetValues(HelpWid, XmNuserData, SUMA_HELP, NULL);

   /* unmanage the buttons I am not using it here */
   XtUnmanageChild(HelpWid);
   
   
   XtManageChild (dialog);
   XtPopup (XtParent (dialog), XtGrabNone);
   
   if (pos != SWP_DONT_CARE) SUMA_PositionWindowRelative(dialog, parent, pos);
   
   while (answer == SUMA_NO_ANSWER)
     XtAppProcessEvent (SUMAg_CF->X->App, XtIMAll);

   XtPopdown (XtParent (dialog));
   /* make sure the dialog goes away before returning. Sync with server
   * and update the display.
   */
   XSync (XtDisplay (dialog), 0);
   XmUpdateDisplay (parent);

   SUMA_RETURN(answer);
}

 
/*
 * AskUser() -- a generalized routine that asks the user a question
 * and returns a response.  Parameters are: the question, the labels
 * for the "Yes" and "No" buttons, and the default selection to use.
 */
int AskUser(Widget parent, char *question, char *ans1, char *ans2, int default_ans)
{
    static Widget dialog; /* static to avoid multiple creation */
    XmString text, yes, no;
    static int answer;

    if (!dialog) {
        dialog = XmCreateQuestionDialog (parent, "dialog", NULL, 0);
        XtVaSetValues (dialog,
            XmNdialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
            NULL);
        XtSetSensitive (
            XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON),
            False);
        XtAddCallback (dialog, XmNokCallback, SUMA_response, &answer);
        XtAddCallback (dialog, XmNcancelCallback, SUMA_response, &answer);
       /* Now add a special extra cute little button */
       {
          /* To do here:
          - Make all other buttons use UserData for uniformity
          - use SUMA_NO, SUMA_YES etc....
          - deal with recreation issues (you'll have to keep track of which new buttons are used, their new labels and whether they are to appear or not.)
            You'll probably want different kinds of static dialog widgets for the various types you envision using ....
          */
          XmString All = XmStringCreateLocalized ("All");
          Widget All_button = NULL;

          All_button = XtVaCreateManagedWidget("All", 
            xmPushButtonWidgetClass, dialog,
            XmNlabelString, All,
            NULL);
          XtVaSetValues(All_button, XmNuserData, SUMA_YES_ALL, NULL);
          XtAddCallback (All_button, XmNactivateCallback, SUMA_response, &answer);
          XmStringFree (All);   
       }

    }
    answer = SUMA_NO_ANSWER;
    text = XmStringCreateLocalized (question);
    yes = XmStringCreateLocalized (ans1);
    no = XmStringCreateLocalized (ans2);
    XtVaSetValues (dialog,
        XmNmessageString,      text,
        XmNokLabelString,      yes,
        XmNcancelLabelString,  no,
        XmNdefaultButtonType,  default_ans == SUMA_YES ?
            XmDIALOG_OK_BUTTON : XmDIALOG_CANCEL_BUTTON,
        NULL);
    XmStringFree (text);
    XmStringFree (yes);
    XmStringFree (no);
        
    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);

    while (answer == SUMA_NO_ANSWER)
        XtAppProcessEvent (SUMAg_CF->X->App, XtIMAll);

    XtPopdown (XtParent (dialog));
    /* make sure the dialog goes away before returning. Sync with server
     * and update the display.
     */
    XSync (XtDisplay (dialog), 0);
    XmUpdateDisplay (parent);

    return answer;
}

/*!
   \brief sets the answer value to a question dialog created by functions 
   like SUMA_AskUser.... 
   -Based largely on example in Motif Programming Manual chapters 5 and 7
   
   - For the three standard dialog buttons: 
   XmDIALOG_OK_BUTTON, XmDIALOG_CANCEL_BUTTON and XmDIALOG_HELP_BUTTON,
   the widget appears to be the dialog widget
   - For the other buttons (XmCR_ACTIVATE) the widget appears to be the
   button itself.
 */
void SUMA_response(Widget widget, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_response"};
   int *answer = (int *) client_data;
   int ud=0;
   Widget YesWid, NoWid, HelpWid;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   switch (cbs->reason) {
   case XmCR_OK:
      YesWid = XmMessageBoxGetChild(widget, XmDIALOG_OK_BUTTON);
      XtVaGetValues(YesWid, XmNuserData, &ud, NULL);       
      *answer = ud;
      break;
   case XmCR_CANCEL:
      NoWid = XmMessageBoxGetChild(widget, XmDIALOG_CANCEL_BUTTON);
      XtVaGetValues(NoWid, XmNuserData, &ud, NULL);      
      *answer = ud;
      break;
   case XmCR_HELP:
      HelpWid = XmMessageBoxGetChild(widget, XmDIALOG_HELP_BUTTON);
      XtVaGetValues(HelpWid, XmNuserData, &ud, NULL);      
      *answer = ud;
      break;
   case XmCR_ACTIVATE:
      XtVaGetValues(widget, XmNuserData, &ud, NULL); 
      *answer = ud;
      break;
   default:
      *answer = -1;
      break;
   }
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Answer %d\n", FuncName, *answer); 
   SUMA_RETURNe;        
}
/*!
   \brief spits out stats about available visuals 
   
   - copied from program glxvisuals.c by Mark Kilgard
   
   \sa SUMA_ShowVisual
*/
void SUMA_ShowAllVisuals (void) 
{
   static char FuncName[]={"SUMA_ShowAllVisuals"};
   Display *dpy;
   XVisualInfo match, *visualList=NULL, *vi=NULL, *visualToTry=NULL;
   int errorBase, eventBase, major, minor, found, glxcapable;
   Widget TopLevel;
   XtAppContext App;
   char *vargv[1]={ "[A] SUMA" };
   int cargc = 1;


   
   SUMA_ENTRY;

   dpy = XOpenDisplay(NULL);
   if (!dpy) {
      fprintf(SUMA_STDERR, "Error %s: Could not connect to %s.\n", FuncName, XDisplayName(NULL));
      SUMA_RETURNe;
   }
   if (glXQueryExtension(dpy, &errorBase, &eventBase) == False) {
      fprintf(SUMA_STDERR, "Error %s: OpenGL not supported by X server.\n" ,FuncName);
      SUMA_RETURNe;
   }

   glXQueryVersion(dpy, &major, &minor);
   fprintf(SUMA_STDERR, "display: %s\n", XDisplayName(NULL));
   fprintf(SUMA_STDERR, "using GLX version: %d.%d\n\n", major, minor);

   match.screen = DefaultScreen(dpy);
   visualList = XGetVisualInfo(dpy, VisualScreenMask, &match, &found);
   
   visualToTry = NULL;
   for(vi = visualList; found > 0; found--, vi++) {
      if (vi == visualList) glxcapable = SUMA_ShowVisual(dpy, vi, YUP);
      else glxcapable = SUMA_ShowVisual(dpy, vi, NOPE);
      
      if (glxcapable) visualToTry = vi;
   }

   if (visualToTry) {
      GLXContext context;
      Window window;
      Colormap colormap;
      XSetWindowAttributes swa;

      context = glXCreateContext(dpy, visualToTry, 0, GL_TRUE);
      colormap = XCreateColormap(dpy,
      RootWindow(dpy, visualToTry->screen),
      visualToTry->visual, AllocNone);
      swa.colormap = colormap;
      swa.border_pixel = 0;
      window = XCreateWindow(dpy, RootWindow(dpy, visualToTry->screen), 0, 0, 100, 100,
      0, visualToTry->depth, InputOutput, visualToTry->visual,
      CWBorderPixel | CWColormap, &swa);
      glXMakeCurrent(dpy, window, context);
      fprintf(SUMA_STDERR, "\n");
      fprintf(SUMA_STDERR, "OpenGL vendor string: %s\n", glGetString(GL_VENDOR));
      fprintf(SUMA_STDERR, "OpenGL renderer string: %s\n", glGetString(GL_RENDERER));
      fprintf(SUMA_STDERR, "OpenGL version string: %s\n", glGetString(GL_VERSION));
      if (glXIsDirect(dpy, context))
         fprintf(SUMA_STDERR, "direct rendering: supported\n");
   } else fprintf(SUMA_STDERR, "No GLX-capable visuals!\n");
   
   if (visualList) XFree(visualList);

   /* which visual will be chosen by SUMA ? (based on Step 3 in SUMA_X_SurfaceViewer_Create) */
   /* TopLevel = XtAppInitialize(&App, "SUMA", NULL, 0, &cargc, vargv,
                              SUMA_get_fallbackResources(), NULL, 0); Superseded by XtOpenApplication*/
   TopLevel = XtOpenApplication(&App, "SUMA", NULL, 0, &cargc, vargv,
                              SUMA_get_fallbackResources(), topLevelShellWidgetClass, NULL, 0); 
   dpy = XtDisplay(TopLevel);

   vi = glXChooseVisual(dpy, DefaultScreen(dpy), dblBuf);
   if (vi == NULL) {
      fprintf(stdout, "trying lame single buffer visual\n");
      XtAppWarning(App, "trying lame single buffer visual");
      vi = glXChooseVisual(dpy, DefaultScreen(dpy), snglBuf);
    if (vi == NULL) {
      XtAppError(App, "no good visual");
      }
   }
   fprintf (SUMA_STDERR,"************************************\n"); 
   fprintf (SUMA_STDERR,"%s: Visual chosen by SUMA:\n", FuncName);
   SUMA_ShowVisual(dpy, vi, YUP);
   if (vi->class != TrueColor) {
      fprintf (SUMA_STDERR,"%s: Visual is not TrueColor.\n", FuncName); 
      fprintf (SUMA_STDERR," SUMA NO LIKE.\n");
   }
   /* Might cause trouble on Fedora Core 4. Don't know what to do with it */
   XtDestroyWidget(TopLevel);
   XtDestroyApplicationContext(App);
   SUMA_RETURNe;
}

/*!
   \brief Show the properties of a visual.
   \sa OpenGL Programming for the X Window System by Mark J. Kilgard
             pp. 75..81
*/
int SUMA_ShowVisual (Display *dpy, XVisualInfo *vi, SUMA_Boolean ShowHead)
{
   static char FuncName[]={"SUMA_ShowVisual"};
   int glxCapable, bufferSize, level, renderType, doubleBuffer, stereo,
      auxBuffers, redSize, greenSize, blueSize, alphaSize, depthSize,
      stencilSize, acRedSize, acGreenSize, acBlueSize, acAlphaSize;

   SUMA_ENTRY;
   
   if (ShowHead) {
      fprintf(SUMA_STDERR, "\n");
      fprintf(SUMA_STDERR, "   visual     bf lv rg d st  r  g  b a   ax dp st accum buffs\n");
      fprintf(SUMA_STDERR, " id dep cl    sz l  ci b ro sz sz sz sz  bf th cl  r  g  b  a\n");
      fprintf(SUMA_STDERR, "-------------------------------------------------------------\n");
   }
   
   glXGetConfig(dpy, vi, GLX_USE_GL, &glxCapable);
   if (glxCapable) {
      fprintf(SUMA_STDERR, "0x%x %2d %s",(unsigned int)vi->visualid, vi->depth, SUMA_ClassOf(vi->class));
      glXGetConfig(dpy, vi, GLX_BUFFER_SIZE, &bufferSize);
      glXGetConfig(dpy, vi, GLX_LEVEL, &level);
      glXGetConfig(dpy, vi, GLX_RGBA, &renderType);
      glXGetConfig(dpy, vi, GLX_DOUBLEBUFFER, &doubleBuffer);
      glXGetConfig(dpy, vi, GLX_STEREO, &stereo);
      glXGetConfig(dpy, vi, GLX_AUX_BUFFERS, &auxBuffers);
      glXGetConfig(dpy, vi, GLX_RED_SIZE, &redSize);
      glXGetConfig(dpy, vi, GLX_GREEN_SIZE, &greenSize);
      glXGetConfig(dpy, vi, GLX_BLUE_SIZE, &blueSize);
      glXGetConfig(dpy, vi, GLX_ALPHA_SIZE, &alphaSize);
      glXGetConfig(dpy, vi, GLX_DEPTH_SIZE, &depthSize);
      glXGetConfig(dpy, vi, GLX_STENCIL_SIZE, &stencilSize);
      glXGetConfig(dpy, vi, GLX_ACCUM_RED_SIZE, &acRedSize);
      glXGetConfig(dpy, vi, GLX_ACCUM_GREEN_SIZE, &acGreenSize);
      glXGetConfig(dpy, vi, GLX_ACCUM_BLUE_SIZE, &acBlueSize);
      glXGetConfig(dpy, vi, GLX_ACCUM_ALPHA_SIZE, &acAlphaSize);
      fprintf(SUMA_STDERR, "    %2s %2s %1s  %1s  %1s ",
        SUMA_Format(bufferSize, 2), SUMA_Format(level, 2),
        renderType ? "r" : "c",
         doubleBuffer ? "y" : ".", 
         stereo ? "y" : ".");
      fprintf(SUMA_STDERR, "%2s %2s %2s %2s ",
         SUMA_Format(redSize, 2), SUMA_Format(greenSize, 2),
         SUMA_Format(blueSize, 2), SUMA_Format(alphaSize, 2));
      fprintf(SUMA_STDERR, "%2s %2s %2s %2s %2s %2s %2s",
        SUMA_Format(auxBuffers, 2), SUMA_Format(depthSize, 2), SUMA_Format(stencilSize, 2),
        SUMA_Format(acRedSize, 2), SUMA_Format(acGreenSize, 2),
        SUMA_Format(acBlueSize, 2), SUMA_Format(acAlphaSize, 2));
      fprintf(SUMA_STDERR, "\n");
   }
   
   SUMA_RETURN(glxCapable); 
} 

char * SUMA_ClassOf(int c)
{
   static char FuncName[]={"SUMA_ClassOf"};
  
   SUMA_ENTRY;

   switch (c) {
      case StaticGray:   SUMA_RETURN("sg");
      case GrayScale:    SUMA_RETURN("gs");
      case StaticColor:  SUMA_RETURN("sc");
      case PseudoColor:  SUMA_RETURN("pc");
      case TrueColor:    SUMA_RETURN("tc");
      case DirectColor:  SUMA_RETURN("dc");
      default:           SUMA_RETURN("??");
   }
}

char * SUMA_Format(int n, int w)
{
   static char FuncName[]={"SUMA_Format"};
   static char buffer[256];
   static int bufptr;
   char *buf;

   SUMA_ENTRY;

   if (bufptr >= sizeof(buffer) - w)
      bufptr = 0;
   
   buf = buffer + bufptr;
   
   if (n == 0)
      sprintf(buf, "%*s", w, ".");
   else
      sprintf(buf, "%*d", w, n);
   
   bufptr += w + 1;

   SUMA_RETURN(buf);
}


/*
void  (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={""};
   
   SUMA_ENTRY;
   
   SUMA_RETURNe;
}

*/
