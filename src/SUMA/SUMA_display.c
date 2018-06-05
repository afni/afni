#include "SUMA_suma.h"
#include "coxplot.h"
#include "SUMA_plot.h"
 
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
/* May 2013: Added GLX_ALPHA_SIZE, 1 for visual to get color picking to work.
             Without it, there was always what looked like antialiasing that
             resulted in more colors rendered than specified, no matter 
             what I disabled                                            */

#define MULTICONV 0        /* Obsolete */
#define MULTISAMPLE 0      /* Does not make any difference relative to
                              current defaults */

#if MULTICONV
/* No more GLX_ACCUM business, no longer supported GL3.0+  */
static int dblBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 16,
  GLX_RED_SIZE, 1, GLX_BLUE_SIZE, 1, GLX_GREEN_SIZE, 1, GLX_ALPHA_SIZE, 1, 
  GLX_DOUBLEBUFFER, GLX_STENCIL_SIZE, 1, 
  GLX_ACCUM_RED_SIZE, 1,   GLX_ACCUM_GREEN_SIZE, 1,   
  GLX_ACCUM_BLUE_SIZE, 1,   GLX_ACCUM_ALPHA_SIZE, 1,      
  None};
#elif MULTISAMPLE
/* Makes no difference to regular situation. I need blurring, not 
   just multisampling. For that, I'll need to use GLSL */
static int dblBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 16,
  GLX_RED_SIZE, 1, GLX_BLUE_SIZE, 1, GLX_GREEN_SIZE, 1, GLX_ALPHA_SIZE, 1, 
  GLX_DOUBLEBUFFER, GLX_STENCIL_SIZE, 1, 
  GLX_SAMPLE_BUFFERS, 1, GLX_SAMPLES, 8,      
  None};
#else
static int dblBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 16,
  GLX_RED_SIZE, 1, GLX_BLUE_SIZE, 1, GLX_GREEN_SIZE, 1, GLX_ALPHA_SIZE, 1, 
  GLX_DOUBLEBUFFER, GLX_STENCIL_SIZE, 1,     
  None};
#endif

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
   "*hotcolor:              blue2"               , 
   "*buthighlight:          gray60"               ,
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
}; /* if you change default width and height, make sure you change SV->X->aWIDTH & SV->X->aHEIGHT in SUMA_SVmanip */

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
   "*hotcolor:              blue2"               , 
   "*buthighlight:          gray60"               ,
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
}; /* if you change default width and height, make sure you change SV->X->aWIDTH & SV->X->aHEIGHT in SUMA_SVmanip */

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
   "*buthighlight:          gray60"               ,
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
}; /* if you change default width and height, make sure you change SV->X->aWIDTH & SV->X->aHEIGHT in SUMA_SVmanip */

static String fallbackResources_PRINT[] = {
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
   "*background:            white"               ,
   "*menu*background:       gray10"               ,
   "*borderColor:           gray10"               ,
   "*foreground:            black"               ,
   "*borderWidth:           0"                    ,
   "*troughColor:           LightCyan2"                ,
   "*XmLabel.translations:  #override<Btn2Down>:" , /* Motif 2.0 bug */
   "*help*background:       white"                ,
   "*help*foreground:       gray70"               ,
   "*help*helpborder:       False"                ,
   "*help*waitPeriod:       1066"                 ,
   "*help*fontList:         9x15=charset1"    ,
   "*cluefont:              9x15"             ,
   "*help*cancelWaitPeriod: 50"                   ,
   "*hotcolor:              blue2"               , 
   "*buthighlight:          gray20"               ,
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
}; /* if you change default width and height, make sure you change SV->X->aWIDTH & SV->X->aHEIGHT in SUMA_SVmanip */

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
   "*buthighlight:          gray20"               ,
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
}; /* if you change default width and height, make sure you change SV->X->aWIDTH & SV->X->aHEIGHT in SUMA_SVmanip */

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
      case SXR_Print:
         SUMA_RETURN (fallbackResources_PRINT);
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

/* A function that can guess at to whether or not a call to 
glXMakeCurrent is needed. Eventually, I should no longer use
SUMA_SiSi_I_Insist() */
SUMA_Boolean SUMA_glXMakeCurrent(Display *dpy, Window wdw, GLXContext cont,
      char *fname, char *wlab, int force)
{
   static char FuncName[]={"SUMA_glXMakeCurrent"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
      /* An OpenGL rendering context is a port through which all OpenGL 
         commands pass. */
      /* Before rendering, a rendering context must be bound to the desired 
      drawable using glXMakeCurrent. OpenGL rendering commands implicitly use the
      current bound rendering context and one drawable. Just as a
      program can create multiple windows, a program can create multiple OpenGL 
      rendering contexts. But a thread can only be bound to one rendering context
      and drawable at a time. Once bound, OpenGL rendering can begin.
      glXMakeCurrent can be called again to bind to a different window and/or 
      rendering context. */
      
   if (force || cont != SUMAg_CF->X->Cr->last_context ||
                dpy != SUMAg_CF->X->Cr->last_context_DPY ||
                wdw != SUMAg_CF->X->Cr->last_context_WDW ) {
   
      SUMAg_CF->X->Cr->last_context_DPY = NULL; /* This will be a clear
                                                 record of failure to
                                                 set last context */
      snprintf(SUMAg_CF->X->Cr->setting_function, 62,"%s",
               fname ? fname : "NOT_SET");
      snprintf(SUMAg_CF->X->Cr->widget_label, 62,"%s",
               wlab ? wlab : "NOT_SET");
      SUMA_LH("About to make current %s, %s (dpy %p %ld %ld, force %d)", 
               fname, wlab, dpy, (long int)wdw, (long int)cont, force);
      if (!glXMakeCurrent (dpy, wdw, cont)) {
         SUMA_S_Err("Failed in glXMakeCurrent.\n");
         SUMA_GL_ERRS;
         SUMA_EDUMP_TRACE("Called from %s", FuncName);
         SUMA_RETURN(NOPE);
      }
      
      SUMAg_CF->X->Cr->last_context = cont;
      SUMAg_CF->X->Cr->last_context_DPY = dpy;
      SUMAg_CF->X->Cr->last_context_WDW = wdw;
   } else {
      SUMA_LHv("No current making needed from %s, label %s\n", 
               fname?fname:"NULL", wlab?wlab:"NULL");
   }
   SUMA_RETURN(YUP);
}

Boolean SUMA_handleRedisplay(XtPointer closure)
{
   static char FuncName[]={"SUMA_handleRedisplay"};
   static int Last_isv = -1;
   int isv;
   char buf[32];
   SUMA_SurfaceViewer *sv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   if (LocalHead) {
      SUMA_REPORT_WICH_WIDGET_SV ((Widget)closure);
   }
   
   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV((Widget)closure, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   
   /* NEED TO DO WITHOUT THIS SILLY LOGIC HERE.
      SUMA_glXMakeCurrent() below should be able to figure out
      on its own if a call to glXMakeCurrent is needed... */
   if (Last_isv >= 0) {    /* first time function is called, 
                              no use for this variable yet */
      if (isv != Last_isv) {/* need to call glXMakeCurrent */
         if (!sv->Open) {
            if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: Redisplay request for a closed window. Skipping.\n",
                        FuncName);
            SUMA_RETURN(NOPE);
         }else {
            PleaseDoMakeCurrent = YUP;
         }
      }
   } else {
      if (!sv->Open) {
            if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: Redisplay request for a closed window. \n"
                        "Not sure that is ever needed. Skipping.\n",
                        FuncName);
            SUMA_RETURN(NOPE);
      }
   } 
   #if 0 /* trying to fix horrible freeze on 10.5 
            Actually this is also needed on 10.7, turning this option 
            on fixes a crash which happens under the following scenario:
            from suma_demo/afni, launch afni and run_suma with both hemis.,
            't', the switch to inflated view, rotate colormap in AFNI,
            'ctrl+s', load dset v2s.TS.lh.niml.dset, CRASH
            For the crash to happen the surface controller, when first
            opened needed to be initialized for FuncAfni dataset, that
            is why I did the switching to the inflated view and did
            one colormap rotation in AFNI. Otherwise it would initialize
            with the convexity dset and it won't crash.
            In the end I put in a judicious call to SUMA_SiSi_I_Insist()
            in SUMA_LoadDsetOntoSO() to selectively set the rendering 
            context instead of doing it at each redraw.*/
   SUMA_S_Note("Forcing Current Making");
   PleaseDoMakeCurrent = YUP;
   #endif
   
   sprintf(buf,"GLXAREA of sv %d", isv);
   if (!SUMA_glXMakeCurrent (sv->X->DPY, XtWindow((Widget)closure), 
                        sv->X->GLXCONTEXT, FuncName, buf, PleaseDoMakeCurrent)) {
      SUMA_S_Err("Failed in SUMA_glXMakeCurrent.\n");
      SUMA_RETURN(NOPE);
   }
   PleaseDoMakeCurrent = NOPE;
   
   Last_isv = isv; /* store last surface viewer to call display */
   /* call display for the proper surface viewer*/
   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: Calling SUMA_display with SV[%d], Pointer %p.\n", 
               FuncName, isv, sv); 
   SUMA_display(sv, SUMAg_DOv);
   sv->X->REDISPLAYPENDING = 0;
   
   if (SUMAg_N_SVv > 1) {
      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: Forcing display to finish.\n", FuncName);
      /* When multiple viewers are open, the picking does not work at times 
         if you click around rapidly.
         The problem seems to be caused by OpenGL being in a state corresponding 
         to that of the last viewer visited before coming to the current viewer. 
         Forcing gl to render after a redisplay pending for a 
         certain viewer is placed seems to reduce this problem significantly so
         this fix will be adopted until a better one comes along. 
         This call does reduce the apparent speed of the display and might
         cause momentum motion to be more blocky but the overload is minimal for 
         regular use.*/
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
      if (LocalHead) {
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
SUMA_Boolean SUMA_display_edge_striplist(DList *striplist, 
               SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO, char *DispOptions)
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
         SUMA_LHv("Building SDO of %d edges from %d strips\n", 
                  N_allEpath, dlist_size(striplist));
         if ((SDO = SUMA_Alloc_SegmentDO (N_allEpath, 
                        "SUMA_SPI_to_EdgeStrips_segs", 0, 
                        SO->idcode_str, 0, LS_type, SO_type, NULL))) {
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
                  col_first[0] = col_first[3] = 1.0; 
                     col_first[1] = col_first[2] = 0.0;
                  col_middle[1] = col_middle[3] = 1.0; 
                     col_middle[0] = col_middle[2] = 0.0;
                  col_last[0] = col_last[1] = 0.0;  
                     col_last[2] = col_last[3] = 1.0;
               } else {
                  col_first[0] = col_first[3] = 1.0; 
                     col_first[1] = col_first[2] = 0.0;
                  col_middle[0] = col_middle[1] = col_middle[3] = 1.0; 
                     col_middle[2] = 0.0;
                  col_last[0] = col_last[1] = 0.0;  
                     col_last[2] = col_last[3] = 1.0;
               }
               SUMA_LHv("   SDO's strip #%d of %d edges (isclosed = %d)\n", 
                        kstrip, N_Epath, isclosed);
               elm=NULL;
               do{
                  if (!elm) elm = dlist_head(strip->Edges);
                  else elm = dlist_next(elm);
                  if (elm==dlist_head(strip->Edges)) { 
                     SUMA_COPY_VEC( col_first, 
                                    &(SDO->colv[4*ke]), 4, float, GLfloat);  
                  } else if (elm==dlist_tail(strip->Edges)) { 
                     SUMA_COPY_VEC( col_last, 
                                    &(SDO->colv[4*ke]), 4, float, GLfloat); 
                  } else { 
                     SUMA_COPY_VEC( col_middle, 
                                    &(SDO->colv[4*ke]), 4, float, GLfloat); }
                  for (j=0; j<3;++j) {
                     SDO->n0[3*ke+j] = 
                           SO->NodeList[3*SO->EL->EL[(INT_CAST)elm->data][0]+j];
                     SDO->n1[3*ke+j] = 
                           SO->NodeList[3*SO->EL->EL[(INT_CAST)elm->data][1]+j]; 
                  }
                  ++ke;
               } while (elm != dlist_tail(strip->Edges));
               ++kstrip;
            }  while (elmlist != dlist_tail(striplist)); 
            /* addDO */
            if (!SUMA_AddDO(  SUMAg_DOv, &SUMAg_N_DOv, 
                              (void *)SDO, LS_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,
                        "Error %s: Failed in SUMA_AddDO.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            /* register DO with viewer */
            if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
               fprintf(SUMA_STDERR,
                        "Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
               SUMA_RETURN(NOPE);
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
                        SO->idcode_str, 0, OLS_type, SO_type, NULL))) {
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
                  if (elm==dlist_head(strip->Points)) { 
                     SUMA_COPY_VEC(col_first, &(SEDO->colv[4*ke]), 
                                    4, float, GLfloat);  
                  } else if (elm==dlist_tail(strip->Points)) { 
                        SUMA_COPY_VEC(col_last, &(SEDO->colv[4*ke]), 
                                       4, float, GLfloat); 
                  } else { SUMA_COPY_VEC(col_middle, &(SEDO->colv[4*ke]), 
                                       4, float, GLfloat); }
                  if (elm != dlist_tail(strip->Points)) {
                     elmn = dlist_next(elm);
                  } else {
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
            if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, 
                            (void *)SEDO, OLS_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,
                        "Error %s: Failed in SUMA_AddDO.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            /* register DO with viewer */
            if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
               fprintf(SUMA_STDERR,
                        "Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
         }
      }
   }
   
   if (SUMA_iswordin_ci(DispOptions, "ShowPoints") == 1) {
      if (N_allPpath) {
         SUMA_LHv("Building SPDO of %d points from %d strips\n", 
                  N_allPpath, dlist_size(striplist));
         if ((SPDO = SUMA_Alloc_SphereDO (N_allPpath, 
                        "SUMA_SPI_to_EdgeStrips_points", NULL, SP_type))) {
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
                  col_first[0] = col_first[3] = 1.0; 
                  col_first[1] = col_first[2] = 0.0;
                  col_middle[0] = col_middle[1] = 
                     col_middle[2] = col_middle[3] = 1.0;
                  col_last[0] = col_last[1] = 0.0;  
                  col_last[2] = col_last[3] = 1.0;
               } else {
                  col_first[0] = col_first[3] = 1.0; 
                  col_first[1] = col_first[2] = 0.0;
                  col_middle[0] = col_middle[2] = 
                     col_middle[3] = 1.0; col_middle[1] = 0.0;
                  col_last[0] = col_last[1] = 0.0;  
                  col_last[2] = col_last[3] = 1.0;
               }
               SUMA_LHv("   SPDO's strip #%d of %d points (isclosed=%d)\n", 
                        kstrip, N_Ppath, isclosed);
               elm=NULL;
               do{
                  if (!elm) elm = dlist_head(strip->Points);
                  else elm = dlist_next(elm);
                  if (elm==dlist_head(strip->Points)) { 
                     SUMA_COPY_VEC(col_first, &(SPDO->colv[4*ke]), 
                                    4, float, GLfloat);
                  } else if (elm==dlist_tail(strip->Points)) { 
                     SUMA_COPY_VEC(col_last, &(SPDO->colv[4*ke]), 
                                    4, float, GLfloat);  
                  } else {
                     SUMA_COPY_VEC(col_middle, &(SPDO->colv[4*ke]), 
                                    4, float, GLfloat);  }
                  SPDO->cxyz[3*ke  ] = ((float*)elm->data)[0];
                  SPDO->cxyz[3*ke+1] = ((float*)elm->data)[1];
                  SPDO->cxyz[3*ke+2] = ((float*)elm->data)[2]; 
                  ++ke;
               } while(elm != dlist_tail(strip->Points));
               ++kstrip;
            } while (elmlist != dlist_tail(striplist)); 
            /* addDO */
            if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, 
                           (void *)SPDO, SP_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,
                        "Error %s: Failed in SUMA_AddDO.\n", FuncName);
               SUMA_RETURN(NOPE);
            }

            /* register DO with viewer */
            if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
               fprintf(SUMA_STDERR,
                        "Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
               SUMA_RETURN(NOPE);
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

void SUMA_LoadMaskDO (char *s, void *csvp )
{
   static char FuncName[]={"SUMA_LoadMaskDO"};
   SUMA_MaskDO *MDO = NULL;
   SUMA_SurfaceViewer *sv;
   SUMA_DO_Types dotp=not_DO_type;
   SUMA_DO_CoordType coord_type=SUMA_WORLD;
   void *VDO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Loading %s", s);
   sv = (SUMA_SurfaceViewer *)csvp;
   
   if (!s) { SUMA_RETURNe; }
   
   /* what type are we dealing with ? */
   dotp = SUMA_Guess_DO_Type(s);
   if (dotp == not_DO_type) {
      SUMA_S_Warnv("Could not determine DO type of %s, assuming MASK.",
                     s);
      /* assume mask */
      dotp = MASK_type;
   }
   coord_type = SUMA_WORLD;
   switch (dotp) {
      case MASK_type: {
         SUMA_MaskDO *MDO = NULL;
         if (!(VDO = (void *)SUMA_ReadMaskDO(s, NULL))) {
            SUMA_SL_Err("Failed to read tracts file.\n");
            SUMA_RETURNe;
         }
         SUMA_NEW_MASKSTATE();
         SUMA_LH("Mask read");
         ((SUMA_MaskDO*)VDO)->do_type = dotp;
         MDO = (SUMA_MaskDO *)VDO;
         if (!SUMA_AccessorizeMDO(MDO)) {
            SUMA_SL_Err("Failed to get lipstick on that pig.\n");
            SUMA_RETURNe;
         }
         break; }
      case NIDO_type:
         if (!(VDO = (void *)SUMA_ReadNIDO(s, NULL))) {
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
   
   /* addDO */
   SUMA_LH("Adding DO");
   if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, VDO, dotp, coord_type)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
      SUMA_RETURNe;
   }

   /* register DO with viewer */
   SUMA_LH("Registrar");
   if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
      SUMA_RETURNe;
   }

   SUMA_LH("Outa here");
   /* redisplay curent only*/
   if (sv) {
      sv->ResetGLStateVariables = YUP;
      SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
   }
               
   SUMA_RETURNe;
}

void SUMA_LoadSegDO (char *s, void *csvp )
{
   static char FuncName[]={"SUMA_LoadSegDO"};
   SUMA_SegmentDO *SDO = NULL;
   SUMA_SurfaceViewer *sv;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_DO_Types dotp=not_DO_type;
   SUMA_MaskDO *MDO = NULL;
   SUMA_DO_CoordType coord_type=SUMA_WORLD;
   void *VDO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Loading %s", s);
   sv = (SUMA_SurfaceViewer *)csvp;
   
   if (!s) { SUMA_RETURNe; }
   
   /* what type are we dealing with ? */
   dotp = SUMA_Guess_DO_Type(s);
   if (dotp == not_DO_type) {
      SUMA_S_Warnv("Could not determine DO type of %s, assuming segments.",
                     s);
      /* assume segments */
      dotp = LS_type;
   }
   coord_type = SUMA_WORLD;
   switch (dotp) {
      case TRACT_type:
         if (!sv || !(SO = SUMA_SV_Focus_SO(sv))) {
            SUMA_LH("No surface in focus to which the tracts "
                        "would be attached. That's OK.\n");
         }
         if (!(VDO = (void *)SUMA_ReadTractDO(s, SO?SO->idcode_str:NULL))) {
               SUMA_SL_Err("Failed to read tracts file.\n");
               SUMA_RETURNe;
         }
         SUMA_LH("Tract read");
         ((SUMA_TractDO*)VDO)->do_type = dotp;
         break;
      case ONBV_type:
      case NBV_type:
         if (!(SO = SUMA_SV_Focus_SO(sv))) {
            SUMA_SL_Err("No surface in focus to which the vector "
                        "would be attached.\n");
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
         if (!(SO = SUMA_SV_Focus_SO(sv))) {
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
     case ODIR_type:
         if (!(SDO = SUMA_ReadDirDO(s, 1, NULL))) {
            SUMA_SL_Err("Failed to read directions file.\n");
            SUMA_RETURNe;
         }
         SDO->do_type = dotp;
         VDO = (void *)SDO;
         break;
     case DIR_type:
         if (!(SDO = SUMA_ReadDirDO(s, 0, NULL))) {
            SUMA_SL_Err("Failed to read directions file.\n");
            SUMA_RETURNe;
         }
         SDO->do_type = dotp;
         VDO = (void *)SDO;
         break;
      case NBOLS_type:
         if (!(SO = SUMA_SV_Focus_SO(sv))) {
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
         if (!(SO = SUMA_SV_Focus_SO(sv))) {
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
      case PNT_type:
         if (!(VDO = (void *)SUMA_ReadPntDO(s))) {
            SUMA_SL_Err("Failed to read points file.\n");
            SUMA_RETURNe;
         }
         ((SUMA_SphereDO * )VDO)->do_type = dotp;
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
         SO = SUMA_SV_Focus_SO(sv);
         if (!(VDO = (void *)SUMA_ReadNIDO(s, SO ? SO->idcode_str:NULL))) {
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
      case MASK_type: {
         SUMA_S_Warn("Why the duplication with SUMA_LoadMaskDO?...");
         if (!(VDO = (void *)SUMA_ReadMaskDO(s, NULL))) {
            SUMA_SL_Err("Failed to read masks file.\n");
            SUMA_RETURNe;
         }
         SUMA_LH("Mask read");
         ((SUMA_MaskDO*)VDO)->do_type = dotp;
         MDO = (SUMA_MaskDO *)VDO;
         if (MDO_IS_BOX(MDO)) {
            SUMA_LH("Forming SO for box");
            if (!(MDO->SO = SUMA_box_surface(MDO->hdim, MDO->cen, 
                                             MDO->dcolv, MDO->N_obj))) {
               SUMA_S_Err("Failed to create box SO!");
               SUMA_RETURNe;
            }
         } else if (MDO_IS_SPH(MDO)) {
            SUMA_S_Warn("Not ready for multi obj, or spheroidal objects");
            if (!(MDO->SO = SUMA_CreateIcosahedron(MDO->hdim[0], 2, 
                     MDO->cen, "n", 1))) {
               SUMA_S_Err("Failed to create sphere SO!");
               SUMA_RETURNe;
            }
         } else {
            SUMA_S_Err("Not ready for prime time");
            SUMA_RETURNe;
         }
         break; }
      default:
         SUMA_EDUMP_TRACE("Should not get here, type %d", dotp);
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
         NI_free_element(ngr); ngr = NULL;
         /* now repeat xformation and see what you get */
         ngr = SUMA_SDO2niSDO(SDO);
         NEL_WRITE_TX(ngr, "file:mess2.niml.SDO", suc);
         SUMA_S_Note("Wrote mess2 to disk");
         NI_free_element(ngr); ngr = NULL;
         VDO = (void *)SDO;
      }
   }
   #endif
   
   /* addDO */
   SUMA_LH("Adding DO");
   if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, VDO, dotp, coord_type)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
      SUMA_RETURNe;
   }

   /* register DO with viewer */
   SUMA_LH("Registrar");
   if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
      SUMA_RETURNe;
   }

   SUMA_LH("Outa here");
   /* redisplay curent only*/
   if (sv) {
      sv->ResetGLStateVariables = YUP;
      SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
   }
               
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
         SUMA_strncat (m_stmp, m_val, SUMA_FV2S_ATTR_TMP_STR-1); \
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
         SUMA_strncat (m_stmp, m_val, SUMA_FV2S_ATTR_TMP_STR-1); \
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
   SUMA_FV2S_ATTR(nel, "currentQuat", 
                  csv->GVS[csv->StdView].currentQuat, 4, feyl); 
      if (feyl) { SUMA_RETURNe; }
   SUMA_FV2S_ATTR(nel, "translateVec", 
                  csv->GVS[csv->StdView].translateVec, 2, feyl); 
      if (feyl) { SUMA_RETURNe; }
   SUMA_FV2S_ATTR(nel, "clear_color", csv->clear_color, 4, feyl); 
      if (feyl) { SUMA_RETURNe; }
   sprintf(stmp, "%f", csv->FOV[csv->iState]);
   NI_set_attribute (nel, "FOV", stmp);
   sprintf(stmp, "%f", csv->Aspect);
   NI_set_attribute (nel, "Aspect", stmp);
   sprintf(stmp, "%d", csv->wWindWidth);
   NI_set_attribute (nel, "WindWidth", stmp);
   sprintf(stmp, "%d", csv->wWindHeight);
   NI_set_attribute (nel, "WindHeight", stmp);
   if (csv->X && csv->X->TOPLEVEL) {            
      SUMA_ALL_DO *ado=NULL;
      SUMA_X_SurfCont *SurfCont=NULL;
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
      if ((ado = SUMA_SV_any_ADO_WithSurfContWidget(csv,NULL,NOT_SET_type))) {
         SurfCont = SUMA_ADO_Cont(ado);
         XtVaGetValues (SurfCont->TLS, /* Get the positions of New */
            XmNx, &X,
            XmNy, &Y,
            NULL);
         if ((int)X >= 0 && (int)X < ScrW ) { 
            NI_SET_INT (nel, "ContX", (int)X);
         } else {
            SUMA_S_Warnv("X position is %d, outside of [0,%d[\n",
                         X, ScrW);
         }
         if ((int)Y >= 0 && (int)Y < ScrH ) { 
            NI_SET_INT (nel, "ContY", (int)Y);
         } else {
            SUMA_S_Warnv("Y position is %d, outside of [0,%d[\n",
                         Y, ScrH);
         }
      }
   }
   sprintf(stmp, "%d", (int)csv->BF_Cull);
   NI_set_attribute (nel, "BF_Cull", stmp);
   sprintf(stmp, "%f", csv->Back_Modfact);
   NI_set_attribute (nel, "Back_Modfact", stmp);
   sprintf(stmp, "%d", (int)csv->PolyMode);
   NI_set_attribute (nel, "PolyMode", stmp);
   sprintf(stmp, "%d", (int)csv->DO_DrawMask);
   NI_set_attribute (nel, "DO_DrawMask", stmp);
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
         ShowWorldAxis[1], DO_DrawMask[1],
         ShowMeshAxis[1], ShowCrossHair[1], ShowForeground[1], 
         ShowBackground[1], WindX[1], WindY[1], ContX[1], ContY[1];
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
         csv->wWindWidth = (int)WindWidth[0];
            /* gets set when SUMA_resize is called */
      }
   SUMA_S2FV_ATTR(nel, "WindHeight", WindHeight, 1, feyl); 
      if (!feyl) {
         csv->wWindHeight = (int)WindHeight[0]; 
            /* That gets recalculated when SUMA_resize is called */
      }
   if (csv->X && csv->X->TOPLEVEL) {
      SUMA_ALL_DO *ado=NULL;
      SUMA_X_SurfCont *SurfCont=NULL;
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
      
      if (WindX[0] >= 0 && WindY[0] >= 0) {
         SUMA_LH("Setting viewer window at positions %d %d", 
                 (int)WindX[0], (int)WindY[0]);
         XtVaSetValues (csv->X->TOPLEVEL, /* Set the positions of New */
            XmNx, (Position)((int)WindX[0]),
            XmNy, (Position)((int)WindY[0]),
            NULL);
       }
      if ((ado = SUMA_SV_Focus_any_ADO(csv, NULL))) {
         /* possible to set things for controller */
         SUMA_S2FV_ATTR(nel, "ContX", ContX, 1, feyl); 
         if (!feyl) {
            if ((int)ContX[0] < 0 && (int)ContX[0] >= ScrW ) { 
               SUMA_S_Warnv("X position is %d, outside of [0,%d[\n",
                      (int)ContX[0], ScrW);
               ContX[0] = -1.0;
            }
         } else ContX[0] = -1.0;
         SUMA_S2FV_ATTR(nel, "ContY", ContY, 1, feyl); 
         if (!feyl) {
            if ((int)ContY[0] < 0 && (int)ContY[0] >= ScrW ) { 
               SUMA_S_Warnv("Y position is %d, outside of [0,%d[\n",
                      (int)ContY[0], ScrW);
               ContY[0] = -1.0;
            }
         } else ContY[0] = -1.0;

         if (ContX[0] >= 0 && ContY[0] >= 0 && 
             (SurfCont = SUMA_ADO_Cont(ado))&& 
             SUMA_viewSurfaceCont(NULL, ado, csv)) {
            SUMA_LH("Setting controller window at positions %d %d", 
                 (int)ContX[0], (int)ContY[0]);
            XtVaSetValues (SurfCont->TLS, /* Set the positions of New */
               XmNx, (Position)((int)ContX[0]),
               XmNy, (Position)((int)ContY[0]),
               NULL);
          }
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
   SUMA_S2FV_ATTR(nel, "DO_DrawMask", DO_DrawMask, 1, feyl); 
      if (!feyl) {
         csv->DO_DrawMask = (SUMA_DO_DRAW_MASK)DO_DrawMask[0];
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
   This call will also generate a SUMA_resize call, which will provide adjusted 
   values to reflect size of the GLXAREA */
   SUMA_S_Note("Resizing main window to %d x %d",
            csv->wWindWidth, csv->wWindHeight);
   SUMA_WidgetResize (csv->X->TOPLEVEL , csv->wWindWidth, csv->wWindHeight); 
   
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
         ShowEyeAxis[1], ShowWorldAxis[1], DO_DrawMask[1],
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

int SUMA_PixelsToDisk(SUMA_SurfaceViewer *csv, int w, int h, GLvoid *vpixels, 
                      int colordepth, int verb, char *ufname, int autoname, 
                      int overwrite) 
{
   static char FuncName[]={"SUMA_PixelsToDisk"};  
   MRI_IMAGE *tim=NULL;
   static char fname[512];
   int undoover=0;
   GLubyte *pixels=NULL;
   GLfloat *fpixels=NULL;
   SUMA_PARSED_NAME *pn=NULL;
   
   SUMA_ENTRY;
   
   if (!vpixels || w < 1 || SUMA_ABS(h) < 1) SUMA_RETURN(0);

   switch (colordepth) {
      case SUMA_b:
      case 1: {
         unsigned char *pp3=NULL;
         int ii, ii3;
         pixels = (GLubyte *)vpixels;
         if (!(pp3=(unsigned char *)SUMA_malloc(3*w*h*sizeof(unsigned char)))) {
            SUMA_S_Crit("malloc failed"); SUMA_RETURN(0);
         }
         ii3 = 0;
         for (ii=0; ii<w*h; ++ii) {
            pp3[ii3++] = pixels[ii];
            pp3[ii3++] = pixels[ii];
            pp3[ii3++] = pixels[ii];
         }
         tim = ISQ_snap_to_mri_image (w, h, (unsigned char *)pp3 );
         SUMA_free(pp3);
         break; }
      case SUMA_F:
      case 5: {
         unsigned char *pp3=NULL, pt;
         float fmax = 1.0;
         int ii, ii3;
         fpixels = (GLfloat *)vpixels;
         if (!(pp3=(unsigned char *)SUMA_malloc(3*w*h*sizeof(unsigned char)))) {
            SUMA_S_Crit("malloc failed"); SUMA_RETURN(0);
         }
         fmax = fpixels[0];
         for (ii=1; ii<w*h; ++ii) {
            if (fmax < fpixels[ii]) fmax = fpixels[ii];
         }
         if (fmax == 0.0f) fmax = 1.0;
         if (verb) {
            SUMA_S_Note("Buffer float max of %f", fmax);
         }
         ii3 = 0;
         for (ii=0; ii<w*h; ++ii) {
            pt = (unsigned char)((fpixels[ii]/fmax)*255);
            pp3[ii3++] = pt;
            pp3[ii3++] = pt;
            pp3[ii3++] = pt;
         }
         tim = ISQ_snap_to_mri_image (w, h, (unsigned char *)pp3 );
         SUMA_free(pp3);
         break; }
      case SUMA_bbb:
      case 3:
         pixels = (GLubyte *)vpixels;
         tim = ISQ_snap_to_mri_image (w, h, (unsigned char *)pixels );
         break;
      case SUMA_bbbb:
      case 4:
         pixels = (GLubyte *)vpixels;
         tim = ISQ_snap4_to_mri_image (w, h, (unsigned char *)pixels );
         break;
      default:
         SUMA_S_Errv("Bad color depth or GL tuple of %d\n", colordepth);
         SUMA_RETURN(0);
         break;
   }
   
   if (!tim) {
      SUMA_S_Err("Failed to get image"); 
      SUMA_RETURN(0); 
   }
   
   if (!ufname) {
      SUMA_VALIDATE_RECORD_PATH(SUMAg_CF->autorecord);
      pn = SUMAg_CF->autorecord;
      autoname = 1;
   } else {
      if (!(pn = SUMA_ParseFname(ufname, NULL))) {
         SUMA_S_Errv("Failed to parse %s\n", ufname);
         SUMA_RETURN(0);
      }
   }
   if (autoname) {
      if (!strcasecmp(pn->Ext,".jpg") ||
          !strcasecmp(pn->Ext,".ppm") ||
          !strcasecmp(pn->Ext,".1D") ){
            snprintf(fname,510*sizeof(char),
                     "%s/%s.%c.%s%s", 
                     pn->Path, 
                     pn->FileName_NoExt,
                     SUMA_SV_CHAR(csv),
                     SUMA_time_stamp(),
                     pn->Ext); 
      } else {
            snprintf(fname,510*sizeof(char),
                     "%s/%s.%c.%s%s", 
                     pn->Path, 
                     pn->FileName,
                     SUMA_SV_CHAR(csv),
                     SUMA_time_stamp(),
                     ".jpg"); 
      }
   } else {
      if (!strcasecmp(pn->Ext,".jpg") ||
          !strcasecmp(pn->Ext,".ppm") ||
          !strcasecmp(pn->Ext,".1D") ){
            snprintf(fname,510*sizeof(char),
                     "%s/%s%s", 
                     pn->Path, 
                     pn->FileName_NoExt,
                     pn->Ext); 
      } else {
            snprintf(fname,510*sizeof(char),
                     "%s/%s%s", 
                     pn->Path, 
                     pn->FileName,
                     ".jpg"); 
      }
   }
   
   if (overwrite && !THD_ok_overwrite()) {
      THD_force_ok_overwrite(1);
      undoover=1;
   }  
   if (strcasecmp(pn->Ext,".1D")) mri_write(fname,tim); 
   else mri_write_1D(fname,tim);
   mri_free(tim); tim=NULL; 
   if (verb) SUMA_S_Notev("Wrote image to %s\n",fname);
   if (ufname) pn = SUMA_Free_Parsed_Name(pn);
   if (undoover) THD_force_ok_overwrite(0);
   SUMA_RETURN(1);
}  

int SUMA_SnapToDisk(SUMA_SurfaceViewer *csv, int verb, int getback) 
{
   static char FuncName[]={"SUMA_SnapToDisk"};
   GLvoid *pixels=NULL;
   int holdrdc = 0;
   
   SUMA_ENTRY;
   
   if (!csv) SUMA_RETURN(0);
   
   holdrdc = csv->rdc;
   #if 0  /* problem should be fixed by SUMA_grabRenderedPixels
                              Throw section out if no new problems arise.
                              Search for KILL_DOUBLE_RENDERING to locate
                              other chunks for removal 
                                       ZSS Feb 2012 */
      glFinish();
      glXWaitX();
      #ifdef DARWIN
         csv->rdc = SUMA_RDC_X_EXPOSE; /* any thing that avoids a record
                                         operation ... */
         SUMA_handleRedisplay((XtPointer)csv->X->GLXAREA);
         csv->rdc=holdrdc;
      #endif
   #endif
   glXWaitX();
   pixels = SUMA_grabRenderedPixels(csv, 3, csv->X->aWIDTH, csv->X->aHEIGHT,
                                    getback);
   if (pixels) {
      if (!SUMA_PixelsToDisk(csv, csv->X->aWIDTH, -csv->X->aHEIGHT,
                             pixels, 3, verb, NULL, 1, 0)) {
         SUMA_S_Err("Failed to write pix to disk");
         SUMA_free(pixels);
         SUMA_RETURN(0);
      }
      SUMA_free(pixels);
   }
   
   SUMA_RETURN(1);
}

/* Some of these constants may not be defined in older openGLs */
#define NO_GL_CHECK_FRAME_BUFFER 0 /* expect glCheckFramebufferStatus() */

#ifndef  GL_FRAMEBUFFER
   #define GL_FRAMEBUFFER 0
   #define NO_GL_CHECK_FRAME_BUFFER 1/* No glCheckFramebufferStatus() */
#endif

#ifndef  GL_FRAMEBUFFER_COMPLETE
   #define GL_FRAMEBUFFER_COMPLETE -1
   #define NO_GL_CHECK_FRAME_BUFFER 1
#endif

#ifndef  GL_FRAMEBUFFER_UNDEFINED
   #define GL_FRAMEBUFFER_UNDEFINED -2
   #define NO_GL_CHECK_FRAME_BUFFER 1
#endif

#ifndef  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
   #define GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE -3
#endif

#ifndef  GL_FRAMEBUFFER_UNSUPPORTED
   #define GL_FRAMEBUFFER_UNSUPPORTED -4
#endif

#ifndef GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT 
   #define GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT -5
#endif

#ifndef GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
   #define GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS -6
#endif

#if NO_GL_CHECK_FRAME_BUFFER
   #define glCheckFramebufferStatus DUMMY_glCheckFramebufferStatus
#endif
#ifdef SUMA_GL_NO_CHECK_FRAME_BUFFER
   #define glCheckFramebufferStatus DUMMY_glCheckFramebufferStatus
#endif

/* For older openGL libs */
GLenum DUMMY_glCheckFramebufferStatus(GLenum dumdum) {
   return(GL_FRAMEBUFFER_COMPLETE); 
}

SUMA_DO_LOCATOR *SUMA_SV_SortedRegistDO(SUMA_SurfaceViewer *csv, int *N_regs,
                                        SUMA_DO *dov)
{
   static char FuncName[]={"SUMA_SV_SortedRegistDO"};
   SUMA_DO_LOCATOR *sRegistDO=NULL;
   int i, j, k, ct, ot, ctseq[2],
       ncheck=0, N_ctseq=-1, iotseq=-1;
   
   SUMA_ENTRY;
   
   *N_regs = -1;
   if (!csv || csv->N_DO <= 0 || !csv->RegistDO || !N_regs || !dov) {
      SUMA_S_Err("NULL or no DOs in input");
      SUMA_RETURN(sRegistDO);
   }
   
   /* count number of total objects.
      For now it will be the same, as N_DO, but this
      might allow me to render something twice, perhaps, 
      in the future, if I allow special directives to go 
      back with the DO_LOCATOR... 
      For now, same as csv->N_DO*/
      
   *N_regs = csv->N_DO;
   sRegistDO = (SUMA_DO_LOCATOR *)
                  SUMA_calloc(*N_regs, sizeof(SUMA_DO_LOCATOR));
   
   
   /* Sequence of coordinate types */
   ctseq[0] = SUMA_SCREEN;
   ctseq[1] = SUMA_WORLD;
   N_ctseq = 2;
   
   ncheck=0;
   for (j=0; j<N_ctseq; ++j) {
      i = 0; 
      while (i < csv->N_DO) {
         ct = dov[csv->RegistDO[i].dov_ind].CoordType;
         if (ct == ctseq[j]) {
            ot = dov[csv->RegistDO[i].dov_ind].ObjectType;
            iotseq = 
               SUMA_FindFirst_inIntVect(csv->otseq, csv->otseq+csv->N_otseq, ot);
            if (iotseq < 0) { /* not found, take it */
               sRegistDO[ncheck].dov_ind = csv->RegistDO[i].dov_ind;
               strcpy(sRegistDO[ncheck].idcode_str,
                      csv->RegistDO[i].idcode_str);
               ++ncheck;
            }
         }
         ++i;
      }
      /* Now get those in csv->otseq, in order */
      for (k=0; k<csv->N_otseq; ++k) {
         i = 0;
         while (i < csv->N_DO) {
            ct = dov[csv->RegistDO[i].dov_ind].CoordType;
            if (ct == ctseq[j]) {
               ot = dov[csv->RegistDO[i].dov_ind].ObjectType;
               if (ot == csv->otseq[k]) { /* its time has come */
                  sRegistDO[ncheck].dov_ind = csv->RegistDO[i].dov_ind;
                  strcpy(sRegistDO[ncheck].idcode_str,
                         csv->RegistDO[i].idcode_str);
                  ++ncheck;
               }
            }
            ++i;
         }
      }
   }
   if (ncheck != *N_regs) {
      SUMA_S_Err("Mismatch, %d and %d. Adopting smaller number", 
                  ncheck, *N_regs);
      if (ncheck < *N_regs) *N_regs=ncheck;
   }
   SUMA_RETURN(sRegistDO);
}

void SUMA_display(SUMA_SurfaceViewer *csv, SUMA_DO *dov)
{
   static char FuncName[]={"SUMA_display"};
   static SUMA_CONV_MODES l_C_mode=SUMA_CONV_NONE;
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
    
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_DUMP_TRACE("Trace At display call");
   }
   
   if (!csv->Open) {
      SUMA_S_Errv("Very weird to be here with Open flag = %d\n", csv->Open);
      SUMA_RETURNe;
   }
   
   if (SUMAg_CF->Dev) {
      #if MULTICONV 
         /* Obsolete! No longer works on newer machines. Need to use GLSL 
            to do perform such trickery. This is here for historical reasons */
      csv->C_mode = SUMA_CONV_BOX_5X5;
      if (l_C_mode != csv->C_mode) {
         switch(csv->C_mode) {
          case SUMA_CONV_NONE:
            SUMA_C_resize(csv->C_filter, 1,1);
            SUMA_C_identity(csv->C_filter);
            break;
          case SUMA_CONV_BOX_3X3:
            SUMA_C_resize(csv->C_filter, 3, 3);
            SUMA_C_box(csv->C_filter);
            break;
          case SUMA_CONV_BOX_5X5:
            SUMA_C_resize(csv->C_filter, 5, 5);
            SUMA_C_box(csv->C_filter);
            break;
          case SUMA_CONV_SOBEL_X:
            SUMA_C_sobel(csv->C_filter);
            break;
          case SUMA_CONV_LAPLACE:
            SUMA_C_laplace(csv->C_filter);
            break;
         }
         l_C_mode = csv->C_mode;
      }
      switch (csv->C_mode) {
         case SUMA_CONV_NONE:
            SUMA_display_one(csv, dov);
            break;
         default:
            glClearAccum(csv->C_filter->bias,
                    csv->C_filter->bias,
                    csv->C_filter->bias,
                    1.0);

            glClear(GL_ACCUM_BUFFER_BIT);

            SUMA_C_convolve(csv, dov, csv->C_filter);

            glAccum(GL_RETURN, csv->C_filter->scale);
            break;
      }
      #elif MULTISAMPLE
      /* Does not do much, left here to prove it does
      not do much at all. */
      glEnable(GL_MULTISAMPLE);
      glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
      if (0) {
         GLint iMultiSample=0;
         GLint iNumSamples = 0;
         glGetIntegerv(GL_SAMPLE_BUFFERS, &iMultiSample);
         glGetIntegerv(GL_SAMPLES, &iNumSamples);
         SUMA_LH("HERE %d %d", iMultiSample, iNumSamples);
      }
      SUMA_display_one(csv, dov);
      #else
      SUMA_display_one(csv, dov);
      #endif
   } else {
      SUMA_display_one(csv, dov);
   }
   /* FLUSH_AND_OUT: */
   SUMA_LHv("Flushing or swapping on %p\n", csv->X->GLXAREA);
   /* SUMA_HOLD_IT; Not used anymore */
   
   SUMA_GLX_BUF_SWAP(csv);
   SUMA_LHv("Done Flushing or swapping on %p\n", csv->X->GLXAREA);

  /* Avoid indirect rendering latency from queuing. */
  if (!glXIsDirect(csv->X->DPY, csv->X->GLXCONTEXT))
    glFinish();
   
  /*  if recording, take a snap 
      Note that the buffer swap has been done, it might have
      been better to snap before then.
      This way I could have used SUMA_grabPixels
      instead of SUMA_grabRenderedPixels. 
      Consider this in the future. ZSS Feb 2012*/
  if (csv->Record == 1 && !csv->DO_PickMode) {
      if (csv->rdc < SUMA_RDC_X_START || csv->rdc > SUMA_RDC_X_END) {
         if (0) {/* problem should be fixed by SUMA_grabRenderedPixels
                              Throw section out if no new problems arise.
                              Search for KILL_DOUBLE_RENDERING to locate
                              other chunks for removal 
                                       ZSS Feb 2012 */
           /*
            Combination below helps partial coverage 
            problem under linux when recording.
            But it does not fix the coverage problem 
            entirely.

            SUMA_S_Note("Raising the dead");
            XtPopup (csv->X->TOPLEVEL, XtGrabExclusive);
            XRaiseWindow(XtDisplay(csv->X->TOPLEVEL), 
                        XtWindow(csv->X->TOPLEVEL));
          */  
            glFinish();
            glXWaitX();
         #ifdef DARWIN

            { GLvoid *pixels;
              int holdrdc = csv->rdc;
              /* see justification for this SUMA_handleRedisplay in function
                  SUMA_R_Key(). You need to change rdc to avoid getting into
                  recorder when SUMA_handleRedisplay ends up calling this
                  function again. */
              csv->rdc = SUMA_RDC_X_EXPOSE; /* any thing that avoids a record
                                               operation ... */
              SUMA_handleRedisplay((XtPointer)csv->X->GLXAREA);
              csv->rdc=holdrdc;
              pixels = SUMA_grabPixels(3, csv->X->aWIDTH, csv->X->aHEIGHT);
              if (pixels) {
                ISQ_snapsave( csv->X->aWIDTH, -csv->X->aHEIGHT,
                              (unsigned char *)pixels, csv->X->GLXAREA );
                SUMA_free(pixels);
              }
            }
         #else
            ISQ_snapshot ( csv->X->GLXAREA );
         #endif
         } else { /* better approach after fixing buffer swaping bug Feb 2012*/
            GLvoid *pixels=NULL;
            pixels = SUMA_grabRenderedPixels(csv, 3, 
                                          csv->X->aWIDTH, csv->X->aHEIGHT, 0);
            if (pixels) {
              ISQ_snapsave( csv->X->aWIDTH, -csv->X->aHEIGHT,
                           (unsigned char *)pixels, csv->X->GLXAREA );
              SUMA_free(pixels);
            }
         }
      }
   } else if (csv->Record == 2 && !csv->DO_PickMode) {
      if (csv->rdc < SUMA_RDC_X_START || csv->rdc > SUMA_RDC_X_END) {
         SUMA_SnapToDisk(csv,0,0);
      }
   }
   /* reset rdc, if it is the last thing you'll ever do */
   csv->rdc = SUMA_RDC_NOT_SET;
   
   if (SUMAg_CF->Dev) {
      #ifdef MULTISAMP
      glDisable(GL_MULTISAMPLE);
      #endif
   } 
   SUMA_RETURNe;
}

void SUMA_display_one(SUMA_SurfaceViewer *csv, SUMA_DO *dov)
{   
   static char FuncName[]={"SUMA_display_one"};
   SUMA_DO_LOCATOR *sRegistDO = NULL;
   int i, N_sReg, iflush=1000;
   static int xList[1], yList[1];
   SUMA_SurfaceObject *SO=NULL;
   SUMA_VolumeObject *VO=NULL;
   GLenum fbs;
   GLfloat rotationMatrix[4][4];
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
    
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_DUMP_TRACE("Trace At display_one call");
   }
   if (csv->iState < 0 || !csv->FOV) {
      /* This can happen when loading multiple surfaces 
         at the command line such as with -i -i -i ...*/
      SUMA_LH("Negative state and/or NULL FOV? (%d,%p)\n",
                  csv->iState, csv->FOV);
      if (LocalHead) SUMA_DUMP_TRACE("Weird state/FOV");
      SUMA_RETURNe;
   }
   
   if (!csv->Open) {
      SUMA_S_Errv("Very weird to be here with Open flag = %d\n", csv->Open);
      SUMA_RETURNe;
   }
   
   if (csv->DO_PickMode && !csv->pick_colid_list) { /* get ready */
      csv->pick_colid_list = (DList *)SUMA_calloc(1,sizeof(DList));
      dlist_init(csv->pick_colid_list, SUMA_free_colid_offset_datum);
   }
   
   /* reset stippling masks */
   SUMA_StippleMaskResest(); 

   
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
      yList[0] = csv->X->aHEIGHT;
      xList[0] = 0;
      SUMA_GetSelectionLine ( csv, csv->X->aWIDTH/2, csv->X->aHEIGHT/2, 
                              csv->Pcenter_close, csv->Pcenter_far, 1, 
                              xList, yList, csv->Plist_close);
   }
   /* decide on color mixing needs */
   if (!csv->DO_PickMode && !SUMA_MixColors (csv)) {
      fprintf( SUMA_STDERR,
               "Error %s: Failed in SUMA_MixColors. Aborting.\n", FuncName);
      SUMA_RETURNe;
   }
   
   if (LocalHead) 
      fprintf (SUMA_STDOUT,"%s: Building Rotation matrix ...\n", FuncName);
   SUMA_build_rotmatrix(rotationMatrix, csv->GVS[csv->StdView].currentQuat);
   
   if (LocalHead) {
      SUMA_LH("performing glClear ...");
      SUMA_CHECK_GL_ERROR("OpenGL Error Pre clear");
   }
   fbs = glCheckFramebufferStatus(GL_FRAMEBUFFER);
   switch (fbs) {
      case 0:
         SUMA_CHECK_GL_ERROR("OpenGL Error glCheckFramebufferStatus");
         SUMA_RETURNe;
      case GL_FRAMEBUFFER_COMPLETE:
         SUMA_LH("Frame buffer complete (%d)", fbs);
         break;
      case GL_FRAMEBUFFER_UNDEFINED:
         SUMA_S_Err("Frame buffer undefined(%d)", fbs);
         SUMA_RETURNe;
      case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
         SUMA_S_Err("Frame buffer incomplete attachment(%d)", fbs);
         SUMA_RETURNe;
      case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS:
         SUMA_S_Err("Frame buffer incomplete layer targets(%d)", fbs);
         SUMA_RETURNe;
      case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
         SUMA_S_Err("Frame buffer incomplete multisample(%d)", fbs);
         SUMA_RETURNe;
      case GL_FRAMEBUFFER_UNSUPPORTED:
         SUMA_S_Err("Frame buffer unsupported(%d)", fbs);
         SUMA_RETURNe;
      default:
         SUMA_S_Err("Frame buffer %d?", fbs);
         SUMA_RETURNe;
   }
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); /* clear the Color Buffer 
                                                         and the depth buffer */
   if (LocalHead) {
      SUMA_CHECK_GL_ERROR("OpenGL Error Post clear");   
      SUMA_LH("Clearing done ...");
   }
   
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

   #if 1
   SUMA_LH("Sorting DOs");
   if (!(sRegistDO = SUMA_SV_SortedRegistDO(csv, &N_sReg, dov))) {
      SUMA_S_Err("Failed to create sorted registered DO.\n"
                 "Falling back on default");
      sRegistDO = csv->RegistDO;
      N_sReg = csv->N_DO;
   }
   #else
      SUMA_S_Warn("No sorting!");
      sRegistDO = csv->RegistDO;
      N_sReg = csv->N_DO;
   #endif
   
   /* cycle through csv->RegisteredDO and display 
      those things that have a fixed CoordType*/
   if (LocalHead) 
      fprintf (SUMA_STDOUT,
               "%s: Creating objects with fixed coordinates ...\n", 
               FuncName);
   i = 0;
   while (i < N_sReg) {
      if (dov[sRegistDO[i].dov_ind].CoordType == SUMA_SCREEN) {
         switch (dov[sRegistDO[i].dov_ind].ObjectType) {
            case N_DO_TYPES:
               SUMA_S_Err("This is reserved for the number of types");
               break;
            case NOT_SET_type:
               SUMA_SL_Err("Should not be doing this buidness");
               break;
            case not_DO_type: /* nothing to see here */
               break;
            case NBSP_type:
            case SP_type:
            case PNT_type:
               SUMA_SL_Warn("Not ready yet!");
               break;
            case SO_type:
               break;
            case ANY_DSET_type:
            case MD_DSET_type:
            case GDSET_type: /* Should not be in DO list */
               SUMA_S_Warn("Should not have such objects as registrered DOs");
               break;
            case CDOM_type:
               SUMA_S_Err("Needs implementation");
               break;
            case AO_type:
               if (csv->ShowEyeAxis){
                  if (!SUMA_DrawAxis (
                        (SUMA_Axis*)dov[sRegistDO[i].dov_ind].OP, csv)) {
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
            case NBT_type:
            case SBT_type:
            case DBT_type:
               /*  those types are not used  */
               break;
            case ONBV_type:
            case NBV_type:
            case OLS_type:
            case LS_type:
            case DIR_type:
            case ODIR_type:
            case NBOLS_type:
            case NBLS_type:
               if (!SUMA_DrawSegmentDO (
                     (SUMA_SegmentDO *)dov[sRegistDO[i].dov_ind].OP, 
                     csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawSegmentDO.\n", 
                           FuncName);
               }
               break;
            case PL_type:
               SUMA_SL_Warn("Not ready yet!");
               break;
            case VO_type:
               if (!SUMA_DrawVolumeDO (
                     (SUMA_VolumeObject *)dov[sRegistDO[i].dov_ind].OP, 
                        csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawVolumeDO.\n", 
                           FuncName);
               }
               break;
            case NIDO_type:
               SUMA_LH("Doing Screen NIDO");
               if (!SUMA_DrawNIDO ((SUMA_NIDO*)dov[sRegistDO[i].dov_ind].OP, 
                     csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawNIDO.\n", 
                           FuncName);
               }
               break;
            case TRACT_type: /* should not be fixed */
               break;
            case MASK_type: /* Would we ever need this to be fixed? */
               break;
            case GRAPH_LINK_type: /* don't think we have these fixed either */
               break;
         }
      }
      ++i;
   }
   
   SUMA_SET_GL_MODELVIEW(csv);

   /* cycle through sRegistDO and display 
      those things that have a Local CoordType*/
   if (LocalHead) 
      fprintf (SUMA_STDERR,
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


   i = 0; iflush = N_sReg;
   while (i < N_sReg) {
      if (dov[sRegistDO[i].dov_ind].CoordType == SUMA_WORLD) {
         switch (dov[sRegistDO[i].dov_ind].ObjectType) {
            case N_DO_TYPES:
               SUMA_S_Err("N_DO_TYPES should not come up here");
               break;
            case SO_type:
               SO = (SUMA_SurfaceObject *)dov[sRegistDO[i].dov_ind].OP;
               if (SO->Show && SO->PolyMode != SRM_Hide) {
                  if (  (SO->Side == SUMA_LEFT && csv->ShowLeft) || 
                        (SO->Side == SUMA_RIGHT && csv->ShowRight) ||
                        SO->Side == SUMA_NO_SIDE || SO->Side == SUMA_LR) {
                        if (SUMAg_CF->Dev && 
                            (SUMA_EnvVal("SUMA_TEMP_NODE_CMASK_EXPR"))) {
                           /* Secret option, for testing only, search for 
                           env above for example */
                           SUMA_DrawMesh_mask(SO, csv); /* create the surface */
                        } else {
                           SUMA_DrawMesh(SO, csv); /* create the surface */
                        }
                  }
               }
               break;
            case VO_type:
               VO = (SUMA_VolumeObject *)dov[sRegistDO[i].dov_ind].OP;
               if (VO->Show) {
                  if (!SUMA_DrawVolumeDO (VO, csv)) {
                     SUMA_S_Err("Failed in SUMA_DrawVolumeDO.");
                  } 
               }
               break;
            case AO_type:
               if (csv->ShowMeshAxis) {
                  if (!SUMA_DrawAxis (
                                 (SUMA_Axis*)dov[sRegistDO[i].dov_ind].OP, 
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
            case NBT_type:
            case SBT_type:
            case DBT_type:
               /* those types are not used */
               break;
            case ANY_DSET_type:
            case MD_DSET_type:
            case GDSET_type:
               SUMA_S_Warn("Should not have type in DO list to be rendered");
               break;
            case CDOM_type:
               #if 0
                  /* A CIFTI DO is not drawn directly, not any more */
               if (!SUMA_Draw_CIFTI_DO (
                     (SUMA_CIFTI_DO *)dov[sRegistDO[i].dov_ind].OP, csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_Draw_CIFTI_DO.\n", 
                           FuncName);
               }
               #endif
               break;
            case MASK_type:
               if (!SUMA_DrawMaskDO (
                     (SUMA_MaskDO *)dov[sRegistDO[i].dov_ind].OP, csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawMaskDO.\n", 
                           FuncName);
               }
               break;
            case TRACT_type:
               if (!SUMA_DrawTractDO (
                     (SUMA_TractDO *)dov[sRegistDO[i].dov_ind].OP, csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawTractDO.\n", 
                           FuncName);
               }
               break;
            case OLS_type:
            case LS_type:
            case DIR_type:
            case ODIR_type:
               if (!SUMA_DrawSegmentDO (
                     (SUMA_SegmentDO *)dov[sRegistDO[i].dov_ind].OP, csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawSegmentDO.\n", 
                           FuncName);
               }
               break;
            case NBSP_type:
               /* those are drawn by SUMA_DrawMesh */
               break;
            case PNT_type:
               if (!SUMA_DrawPointDO (
                     (SUMA_SphereDO *)dov[sRegistDO[i].dov_ind].OP, csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawPntDO.\n", FuncName);
               }
               break;
            case SP_type:
               if (!SUMA_DrawSphereDO (
                     (SUMA_SphereDO *)dov[sRegistDO[i].dov_ind].OP, csv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawSphereDO.\n", FuncName);
               }
               break;
            case PL_type:
               if (!SUMA_DrawPlaneDO (
                     (SUMA_PlaneDO *)dov[sRegistDO[i].dov_ind].OP, csv)) {
                  fprintf(SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawPlaneDO.\n", FuncName);
               }
               break;
            case NIDO_type:
               if (SUMA_isNIDO_SurfBased(
                     (SUMA_NIDO *)dov[sRegistDO[i].dov_ind].OP)) { 
                  /* this is done in SUMA_DrawMesh */
               } else {
                  if (!SUMA_DrawNIDO (
                        (SUMA_NIDO *)dov[sRegistDO[i].dov_ind].OP, csv)) {
                     fprintf(SUMA_STDERR, 
                              "Error %s: Failed in SUMA_DrawNIDO.\n", FuncName);
                  }
               }
               break;
            case GRAPH_LINK_type:
               if (SUMAg_CF->Dev && SUMA_isEnv("JAVIER_DEPTH_SPECIAL","YES") &&
                   i < iflush) {
                  SUMA_LH("Flushing depth buffer");
                  /* This trick, and a dirty one it is, allows Javier to see
                  all the connections, unimpeded by any other objects that might
                  obscure it. This simple trick will only work if graphs
                  are rendered at the very end of all other objects.
                  Not sure what the implications, if any, will be for 
                  the matrix display...*/
                  glClear(GL_DEPTH_BUFFER_BIT);
                  iflush = i;
               }
               /* find the real DO this baby points to and render that */
               if (!SUMA_DrawGraphLinkDO (
                    (SUMA_GraphLinkDO *)dov[sRegistDO[i].dov_ind].OP, csv)) {
                     fprintf(SUMA_STDERR, 
                              "Error %s: Failed in SUMA_DrawGraphLinkDO.\n", 
                              FuncName);
               }
               break;
            case NOT_SET_type:
            case not_DO_type:
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
      SUMA_LH("Showing Cross Hair");
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
      glMaterialfv(GL_FRONT, GL_EMISSION, LineColor); 
         /*turn on emissivity for axis*/
      glVertex3f(csv->Pick0[0], csv->Pick0[1], csv->Pick0[2]);
      glVertex3f(csv->Pick1[0], csv->Pick1[1], csv->Pick1[2]);
      glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
         /*turn off emissivity for axis*/
      glEnd();
      glDisable(GL_LINE_STIPPLE);
   }
   #endif
 
   /* Pop matrix and scrimooche */   
   glPopMatrix();   
   
   if (sRegistDO && sRegistDO != csv->RegistDO) {
     SUMA_free(sRegistDO);   
   }
   sRegistDO = NULL;

   SUMA_RETURNe;
}

void
SUMA_graphicsInit(Widget w, XtPointer clientData, XtPointer call)
{
   
   XVisualInfo *SUMAg_cVISINFO=NULL;
   static char FuncName[]={"SUMA_graphicsInit"};
   int isv;
   char buf[32];
   SUMA_SurfaceViewer *sv;
   
   SUMA_ENTRY;

   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV((Widget)w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }

   /* Create OpenGL rendering context. */
   XtVaGetValues(w, GLwNvisualInfo, &SUMAg_cVISINFO, NULL);
   sv->X->GLXCONTEXT = 
      glXCreateContext(XtDisplay(w), SUMAg_cVISINFO,
               0,                  /* No sharing. */
               True);              /* Direct rendering if possible. */

   /* Setup OpenGL state. */
   /* SUMA_HOLD_IT; Not used anymore */
   sprintf(buf,"Init of sv %d", isv);
   if (!SUMA_glXMakeCurrent(XtDisplay(w), XtWindow(w), sv->X->GLXCONTEXT,
                  FuncName, buf, 1)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_glXMakeCurrent.\n \tContinuing ...\n", 
               FuncName);
      SUMA_RETURNe;
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
   if (sv->PolyMode == SRM_Hide) { 
      SUMA_SL_Note("sv->PolyMode reset to SRM_Fill"); 
      sv->PolyMode = SRM_Fill; 
   }
   glClearColor (sv->clear_color[0], sv->clear_color[1], 
                 sv->clear_color[2], sv->clear_color[3]);
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

SUMA_Boolean SUMA_SV_WindDims_From_DrawAreaDims(SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_SV_WindDims_From_DrawAreaDims"};
   
   SUMA_ENTRY;
   
   if (!sv || !sv->X) {
      SUMA_S_Err("sv or sv->X is NULL");
      SUMA_RETURN(NOPE);
   }
   
   if (sv->DrawAreaWidthOffset < 0 || sv->DrawAreaHeightOffset < 0) {
      if (!(SUMA_SV_InitDrawAreaOffset(sv))) {
         SUMA_S_Err("Offset not initialized (%d %d)!\n", 
                   sv->DrawAreaWidthOffset, sv->DrawAreaHeightOffset);
         SUMA_RETURN(NOPE);
      }
   }
   sv->wWindWidth = sv->DrawAreaWidthOffset+sv->X->aWIDTH;
   sv->wWindHeight = sv->DrawAreaHeightOffset+sv->X->aHEIGHT;
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_SV_InitDrawAreaOffset(SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_SV_WindDims_From_DrawAreaDims"};
   Dimension awidth, aheight;
   Dimension wwidth, wheight;
   int dd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv || !sv->X || !sv->X->TOPLEVEL || !sv->X->GLXAREA) {
      SUMA_S_Err("sv or sv->X or widgets is/are NULL");
      SUMA_RETURN(NOPE);
   }
         
   XtVaGetValues (sv->X->GLXAREA,
      XmNwidth, &awidth,
      XmNheight, &aheight,
      NULL);
   SUMA_LH("GLXAREA dims: %d %d", awidth, aheight);
   XtVaGetValues (sv->X->TOPLEVEL,
      XmNwidth, &wwidth,
      XmNheight, &wheight,
      NULL);
   SUMA_LH("Window dims: %d %d", wwidth, wheight);
   dd = (wwidth - awidth);
   if (dd > 0 && dd < 100) { /* sometimes values returned are crraazy, 
                                so just to be sure */
      sv->DrawAreaWidthOffset = dd;
   }
   dd = (wheight - aheight);
   if (dd > 0 && dd < 100) { /* sometimes values returned are crraazy, 
                                so just to be sure */
      sv->DrawAreaHeightOffset = dd;
   }
   if (sv->DrawAreaWidthOffset > 0 && sv->DrawAreaHeightOffset > 0) {
      SUMA_RETURN(YUP);
   } else {
      SUMA_RETURN(NOPE);
   }
}

SUMA_Boolean SUMA_SV_DrawAreaDims_From_WindDims(SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_SV_DrawAreaDims_From_WindDims"};
   
   SUMA_ENTRY;
   
   if (!sv || !sv->X) {
      SUMA_S_Err("sv or sv->X is NULL");
      SUMA_RETURN(NOPE);
   }
   if (sv->DrawAreaWidthOffset < 0 || sv->DrawAreaHeightOffset < 0) {
      if (!(SUMA_SV_InitDrawAreaOffset(sv))) {
         SUMA_S_Err("Offset not initialized (%d %d)!\n", 
                     sv->DrawAreaWidthOffset, sv->DrawAreaHeightOffset);
         SUMA_RETURN(NOPE);
      }
   }
   sv->X->aWIDTH  = sv->wWindWidth - sv->DrawAreaWidthOffset;
   sv->X->aHEIGHT = sv->wWindHeight - sv->DrawAreaHeightOffset;
   SUMA_RETURN(YUP);
}
   
void
SUMA_resize(Widget w,
  XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_resize"};
   GLwDrawingAreaCallbackStruct *callData;
   SUMA_SurfaceViewer *sv;
   char buf[32];
   int isv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }

   callData = (GLwDrawingAreaCallbackStruct *) call;
   SUMA_LH("Resizing sv %d to %dx%d from %dx%d", isv,
                  callData->width, callData->height,
               sv->X->aWIDTH, sv->X->aHEIGHT);
   /* SUMA_HOLD_IT; Not used anymore */
   sprintf(buf,"Resize sv %d", isv);
   if (!SUMA_glXMakeCurrent(XtDisplay(w), XtWindow(w), sv->X->GLXCONTEXT,
                           FuncName, buf, 1)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_glXMakeCurrent.\n \tContinuing ...\n", 
               FuncName);
      SUMA_GL_ERRS;

      SUMA_RETURNe;
   }
   SUMA_LH("Context made current");
   /* SUMA_HOLD_IT; Not used anymore */
   
   glXWaitX();
   sv->X->aWIDTH = callData->width;
   sv->X->aHEIGHT = callData->height;
   glViewport(0, 0, callData->width, callData->height);

   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   gluLookAt ( sv->GVS[sv->StdView].ViewFrom[0], 
               sv->GVS[sv->StdView].ViewFrom[1], 
               sv->GVS[sv->StdView].ViewFrom[2], 
               sv->GVS[sv->StdView].ViewCenter[0], 
               sv->GVS[sv->StdView].ViewCenter[1], 
               sv->GVS[sv->StdView].ViewCenter[2], 
               sv->GVS[sv->StdView].ViewCamUp[0], 
               sv->GVS[sv->StdView].ViewCamUp[1], 
               sv->GVS[sv->StdView].ViewCamUp[2]);
   sv->Aspect = (GLfloat) callData->width/(GLfloat) callData->height;
   if (!(SUMA_SV_WindDims_From_DrawAreaDims(sv))) {
      SUMA_S_Err("Failed to set window dims. This should not happen");
      SUMA_DUMP_TRACE("How did you get here?");
      SUMA_S_Warn("Assuming offset is 0, but that's not cool");
      sv->wWindWidth = sv->X->aWIDTH;
      sv->wWindHeight = sv->X->aHEIGHT;
   }
   
   sv->rdc = SUMA_RDC_X_RESIZE;
   SUMA_postRedisplay(w, clientData, call);
   /* flush pick buffer */
   SUMA_PickBuffer(sv, 0, NULL);
   
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
  /*glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);*/ 
                              /* No need for that, done in display */
  
  
   /* get the viewer just entered. */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      SUMA_S_Err("Failed in macro SUMA_ANY_WIDGET2SV.");
      SUMA_RETURNe;
   }
   
   sv->rdc = SUMA_RDC_X_EXPOSE;
   
   /* When using multiple viewers, you must reset the OpenGL state 
      variables or risk having abrupt changes with the first click */
   sv->ResetGLStateVariables = YUP;
   SUMA_postRedisplay(w, NULL, NULL);
   
   if (SUMAg_CF->N_dcom) {
      for (isv = 0; isv < SUMAg_CF->N_dcom; ++isv) {
         SUMA_S_Note("Executing driver command %d: %s\n", 
                     isv, SUMAg_CF->dcom[isv]);
         SUMA_MakeMeDo(SUMAg_CF->dcom[isv], 0);
         SUMA_ifree(SUMAg_CF->dcom[isv]);
      }
      SUMA_ifree(SUMAg_CF->dcom); SUMAg_CF->N_dcom = 0;
   }

   

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

/* can't allow any menu to do arrow fields. For example 
Cmp will choke in function SUMA_SetCmapMenuChoice so you'll
have to fix that case if you want to allow arrow fields for
colormaps. But why on earth would that be needed? 
Only allow arrow fields for I, T, and B
*/
int SUMA_AllowArrowFieldMenus(int N_items, char *menu_title) 
{
   if (!menu_title || 
        N_items < SUMA_floatEnv("SUMA_ArrowFieldSelectorTrigger", 200)) {
      return(0);    
   }
   if (  !strcmp(menu_title,"I") ||
         !strcmp(menu_title,"T") ||   
         !strcmp(menu_title,"B") ) return(1);
   return(0); 
}

int SUMA_BuildMenu(  Widget parent, int menu_type, char *menu_title, 
                     char menu_mnemonic, SUMA_Boolean tear_off,
                     SUMA_MenuItem *items, 
                     void *ContID, 
                     char *wname, char *hint, char *help,
                     SUMA_MENU_WIDGET *SMW )
{
   static char FuncName[]={"SUMA_BuildMenu"};
   char nlabel[300]="\0";
   Widget menu = NULL, cascade = NULL;
   XmString str=NULL;
   int i=-1; int N_items=0, ok_tear_off=1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!menu_title) {
      SUMA_S_Warn("menu_title is NULL, alert Rick Reynolds!");
      menu_title = "";
   }

   if (items) {
      while(items[N_items].label) {
         if (N_items < 4) {
            SUMA_LHv("item %d %s callback %p\n", 
               N_items, items[N_items].label, items[N_items].callback);
         } else {
            if (LocalHead) {fprintf(stderr,".");}
         }   
         ++N_items;
     }
     if (LocalHead && N_items > 4) {fprintf(stderr,"\n");}
   } 
   SUMA_LHv("Entered. menu_title %s, type %d, %d items.\n", 
               menu_title, menu_type, N_items);
   
   if (SUMA_AllowArrowFieldMenus(N_items, menu_title)) {
      menu_type = SUMA_XmArrowFieldMenu;
      SUMA_LHv("Arrow field triggered menu_title %s, type %d, %d items.\n", 
               menu_title, menu_type, N_items);
   }
   if (menu_type == XmMENU_PULLDOWN || menu_type == XmMENU_OPTION)
     menu = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
   else if (menu_type == XmMENU_POPUP)
     menu = XmCreatePopupMenu (parent, "_popup", NULL, 0);
   else if (menu_type == SUMA_XmArrowFieldMenu) { /* an arrow field */
     menu = XtVaCreateWidget ("rowcolumn",
                  xmRowColumnWidgetClass, parent,
                  XmNpacking, XmPACK_TIGHT,
                  XmNleftAttachment,XmATTACH_FORM ,  
                  XmNorientation , XmHORIZONTAL ,
                  XmNmarginHeight , 0 ,
                  XmNmarginWidth  , 0 ,
                     NULL);
      ok_tear_off = 0; 
   }
   else {
     XtWarning ("Invalid menu type passed to BuildMenu()");
     SUMA_RETURN(-1);
   }
   

   if (tear_off && ok_tear_off)
     XtVaSetValues (menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);

   /* Pulldown menus require a cascade button to be made */
   if (menu_type == XmMENU_PULLDOWN) {
     str = XmStringCreateLocalized (menu_title);
     cascade = XtVaCreateManagedWidget (menu_title,
         xmCascadeButtonWidgetClass, parent,
         XmNlabelString, str,
     /*     XmNmnemonic,    menu_mnemonic, this line causes a warning on 
                                          OS X 10.6. Can't tell why...
                           It also causes plenty of valgrind conditional 
                           jumps warnings.
                           To hell with it.*/
         XmNsubMenuId,   menu,
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
     SUMA_LHv("XmMENU_OPTION %s\n", menu_title);
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
   else if (menu_type == SUMA_XmArrowFieldMenu) {
      SUMA_LHv("XmMENU_OPTION %s\n", "ArrowFieldMode");
   }
   
   /* save the type */
   SMW->menu_type = menu_type;
   
   /* hide your jewel */
   if (menu_type == XmMENU_POPUP || menu_type == SUMA_XmArrowFieldMenu) {  
      SMW->mw[i_wid] = menu; }
   else { SMW->mw[i_wid] = cascade; } 
   
   if (hint || help) {
      SUMA_LH("Registering %s with \n%s\n and \n%s\n\n", wname, hint, help); 
      SUMA_Register_Widget_Children_Help(SMW->mw[i_wid], 1, wname, 
                                hint, help);
   }
   
   ++i_wid;
   
   if (menu_type != SUMA_XmArrowFieldMenu) {
      /* Now add the menu items */
      for (i = 0; items[i].label != NULL; i++) {
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Adding label # %d - %s (CB: %p)\n", 
                     FuncName, i, items[i].label, items[i].callback);
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
                                 wname, hint, help, SMW);
            }
        else {
            if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: Creating widgets MenuWidgets[%d]\n", 
                        FuncName, (INT_CAST)items[i].callback_data);
            if (nchar > 0) {
               snprintf(nlabel, nchar*sizeof(char), "%s", items[i].label);
               SMW->mw[i_wid] = XtVaCreateManagedWidget (nlabel,
                   *items[i].class, menu,
                   NULL);
            } else {
               SMW->mw[i_wid] = 
                  XtVaCreateManagedWidget (items[i].label,
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
            XtVaSetValues (SMW->mw[i_wid], XmNmnemonic, 
                           items[i].mnemonic, NULL);

         /* any item can have an accelerator, except cascade menus. But,
         * we don't worry about that; we know better in our declarations.
         */

         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: Setting accelerator ...\n", FuncName);
         if (items[i].accelerator) {
            str = XmStringCreateLocalized (items[i].accel_text);
            XtVaSetValues (SMW->mw[i_wid],
                XmNaccelerator, items[i].accelerator,
                XmNacceleratorText, str,
                NULL);
            XmStringFree (str);
         }

         if (items[i].class == &xmToggleButtonWidgetClass ||
                 items[i].class == &xmToggleButtonWidgetClass) {
            Pixel fg_pix=0;
            XtVaGetValues (SMW->mw[i_wid], 
                              XmNforeground, &fg_pix, NULL);
            XtVaSetValues (SMW->mw[i_wid], 
                              XmNselectColor, fg_pix, NULL); 

         }

         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: Setting callback ...\n", FuncName);
         if (items[i].callback) {
            SUMA_MenuCallBackData *CBp=NULL;
            CBp = (SUMA_MenuCallBackData *)
                        calloc(1,sizeof(SUMA_MenuCallBackData));
            /* There is no freeing of this pointer in SUMA. 
                        Once created, a widget is only destroyed when 
                        SUMA is killed */
            /* prepare the callback pointer */
            CBp->callback_data = (XtPointer) items[i].callback_data;
            CBp->ContID = ContID;
            XtAddCallback (SMW->mw[i_wid],
                (items[i].class == &xmToggleButtonWidgetClass ||
                 items[i].class == &xmToggleButtonWidgetClass) ?
                    XmNvalueChangedCallback : /* ToggleButton class */
                    XmNactivateCallback,      /* PushButton class */
                items[i].callback, (XtPointer)CBp);
         }
         ++i_wid;
      }
   } else { /* special arrow field case */
      SUMA_MenuCallBackData *CBp=NULL;
      i = 0; /* all callbacks will be the same */
      /* An arrow field */
      if (!SMW->af) {
         SMW->af = (SUMA_ARROW_TEXT_FIELD*)
                           calloc(1,sizeof(SUMA_ARROW_TEXT_FIELD));
      }
      CBp = (SUMA_MenuCallBackData *)
                  calloc(1,sizeof(SUMA_MenuCallBackData));
      CBp->SMW = SMW;
      CBp->callback = items[i].callback;
      CBp->callback_data = NULL; /* this will be set in 
                                  SUMA_MenuArrowFieldCallback*/
      CBp->ContID = ContID;      
      SUMA_LHv("Building an arrow field, i=%d\n", i);
      SUMA_LHv("callback %p data %p\n", 
                   items[i].callback, items[i].callback_data);
      SUMA_CreateArrowField ( menu, menu_title,
                     0, 0, N_items-1, 1,
                     8, SUMA_int,
                     YUP,
                     SUMA_MenuArrowFieldCallback, (void *)CBp,
                     wname,
                     "A way to switch sub-bricks in dsets with lots of them.",
                     SUMA_SurfContHelp_ArrowFieldMenu,
                     SMW->af);
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

/* This function is a wrapper for calling menu callbacks when the
Arrow field is used to setup the menu */
void SUMA_MenuArrowFieldCallback (void *CB) 
{
   static char FuncName[]={"SUMA_MenuArrowFieldCallback"};
   SUMA_MenuCallBackData *CBp = (SUMA_MenuCallBackData *)CB;
   
   SUMA_ENTRY;
   
   if (!CBp) {
      SUMA_S_Err("Bad setup, NULL CB"); SUMA_RETURNe;
   }
   if (!CBp->callback) {
      SUMA_S_Err("Bad setup, NULL CB->callback"); SUMA_RETURNe;
   }
   if (!CBp->SMW) {
      SUMA_S_Err("Need menu structure for arrofields"); SUMA_RETURNe;
   }

   CBp->callback_data = (XTP_CAST)((int)CBp->SMW->af->value+1);
   CBp->callback(NULL, (XtPointer)CBp, NULL);
   SUMA_RETURNe;
}


Widget Gmainw, Gmenubar;

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
   
   {  "Object Controller", &xmPushButtonWidgetClass, \
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
   {  "Web help", &xmPushButtonWidgetClass, \
      'W', NULL, NULL, \
      SUMA_cb_helpWeb, (XtPointer) SW_HelpWeb, NULL},
      
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
      
   {  "Echo Keypresses", &xmToggleButtonWidgetClass, \
      'K', NULL, NULL, \
      SUMA_cb_helpEchoKeyPress, (XtPointer) SW_HelpEchoKeyPress, NULL},
      
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
   
SUMA_MenuItem TransMode_Menu[] = {
   {  "Vwr", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_TransViewerDefault, NULL},
      
   {  "0", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans0, NULL},
   
   {  "1", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans1, NULL},
    
   {  "2", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans2, NULL},
    
   {  "3", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans3, NULL},
    
   {  "4", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans4, NULL},
    
   {  "5", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans5, NULL},
    
   {  "6", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans6, NULL},
    
   {  "7", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans7, NULL},
    
   {  "8", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans8, NULL},
    
   {  "9", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans9, NULL},
    
   {  "10", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans10, NULL},
    
   {  "11", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans11, NULL},
    
   {  "12", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans12, NULL},
    
   {  "13", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans13, NULL},
    
   {  "14", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans14, NULL},
    
   {  "15", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans15, NULL},
    
   {  "16", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_Trans16, NULL},
    
   {NULL},
};

SUMA_MenuItem VTransMode_Menu[] = {
   {  "Vwr", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATransViewerDefault, NULL},

   {  "A", &xmPushButtonWidgetClass, 
      'A', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_Alpha, NULL},
   
   {  "0", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans0, NULL},
   
   {  "1", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans1, NULL},
    
   {  "2", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans2, NULL},
    
   {  "3", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans3, NULL},
    
   {  "4", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans4, NULL},
    
   {  "5", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans5, NULL},
    
   {  "6", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans6, NULL},
    
   {  "7", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_ATrans7, NULL},
    
   {  "8", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans8, NULL},
    
   {  "9", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_ATrans9, NULL},
    
   {  "10", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans10, NULL},
    
   {  "11", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans11, NULL},
    
   {  "12", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans12, NULL},
    
   {  "13", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans13, NULL},
    
   {  "14", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_ATrans14, NULL},
    
   {  "15", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTransMode, (XtPointer) SW_SurfCont_ATrans15, NULL},
    
   {  "16", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetATransMode, (XtPointer) SW_SurfCont_ATrans16, NULL},
    
   {NULL},
};


SUMA_MenuItem DsetViewMode_Menu[] = {
   {  "Col", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetViewMode, 
      (XtPointer) SW_SurfCont_DsetViewCol, NULL},
      
   {  "Con", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetViewMode, (XtPointer) SW_SurfCont_DsetViewCon, NULL},
   
   {  "C&C", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetViewMode, (XtPointer) SW_SurfCont_DsetViewCaC, NULL},
    
   {  "XXX", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetViewMode, (XtPointer) SW_SurfCont_DsetViewXXX, NULL},
        
   {NULL},
};

SUMA_MenuItem DsetFont_Menu[] = {
   {  "8", &xmPushButtonWidgetClass, 
      '8', NULL, NULL, 
      SUMA_cb_SetDsetFont, 
      (XtPointer) SW_SurfCont_DsetFont8, NULL},
      
   {  "9", &xmPushButtonWidgetClass, 
      '9', NULL, NULL, 
      SUMA_cb_SetDsetFont, (XtPointer) SW_SurfCont_DsetFont9, NULL},
   
   {  "T10", &xmPushButtonWidgetClass, 
      't', NULL, NULL, 
      SUMA_cb_SetDsetFont, (XtPointer) SW_SurfCont_DsetFontTR10, NULL},
    
   {  "H10", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetFont, (XtPointer) SW_SurfCont_DsetFontHE10, NULL},
    
   {  "H12", &xmPushButtonWidgetClass, 
      'h', NULL, NULL, 
      SUMA_cb_SetDsetFont, (XtPointer) SW_SurfCont_DsetFontHE12, NULL},
    
   {  "H18", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetFont, (XtPointer) SW_SurfCont_DsetFontHE18, NULL},
    
   {  "T24", &xmPushButtonWidgetClass, 
      'T', NULL, NULL, 
      SUMA_cb_SetDsetFont, (XtPointer) SW_SurfCont_DsetFontTR24, NULL},
    
   {  "XXX", &xmPushButtonWidgetClass, 
      'x', NULL, NULL, 
      SUMA_cb_SetDsetFont, (XtPointer) SW_SurfCont_DsetFontXXX, NULL},
        
   {NULL},
};

SUMA_MenuItem DsetNodeCol_Menu[] = {
   {  "Wht", &xmPushButtonWidgetClass, 
      'W', NULL, NULL, 
      SUMA_cb_SetDsetNodeCol, 
      (XtPointer) SW_SurfCont_DsetNodeColWhite, NULL},
      
   {  "Blk", &xmPushButtonWidgetClass, 
      'B', NULL, NULL, 
      SUMA_cb_SetDsetNodeCol, (XtPointer) SW_SurfCont_DsetNodeColBlack, NULL},
   
   {  "Red", &xmPushButtonWidgetClass, 
      'R', NULL, NULL, 
      SUMA_cb_SetDsetNodeCol, (XtPointer) SW_SurfCont_DsetNodeColRed, NULL},
    
   {  "Grn", &xmPushButtonWidgetClass, 
      'G', NULL, NULL, 
      SUMA_cb_SetDsetNodeCol, (XtPointer) SW_SurfCont_DsetNodeColGreen, NULL},
    
   {  "Blu", &xmPushButtonWidgetClass, 
      'B', NULL, NULL, 
      SUMA_cb_SetDsetNodeCol, (XtPointer) SW_SurfCont_DsetNodeColBlue, NULL},
    
   {  "Yel", &xmPushButtonWidgetClass, 
      'Y', NULL, NULL, 
      SUMA_cb_SetDsetNodeCol, (XtPointer) SW_SurfCont_DsetNodeColYellow, NULL},
    
   {  "G50", &xmPushButtonWidgetClass, 
      '5', NULL, NULL, 
      SUMA_cb_SetDsetNodeCol, (XtPointer) SW_SurfCont_DsetNodeColGray50, NULL},
    
   {  "Val", &xmPushButtonWidgetClass, 
      'V', NULL, NULL, 
      SUMA_cb_SetDsetNodeCol, (XtPointer) SW_SurfCont_DsetNodeColVal, NULL},
        
   {  "Grp", &xmPushButtonWidgetClass, 
      'G', NULL, NULL, 
      SUMA_cb_SetDsetNodeCol, (XtPointer) SW_SurfCont_DsetNodeColGrp, NULL},
        
   {NULL},
};

SUMA_MenuItem DsetTxtShad_Menu[] = {
   {  "T", &xmPushButtonWidgetClass, 
      '1', NULL, NULL, 
      SUMA_cb_SetDsetTxtShad, 
      (XtPointer) SW_SurfCont_DsetTxtShad1, NULL},
      
   {  "Ts", &xmPushButtonWidgetClass, 
      '2', NULL, NULL, 
      SUMA_cb_SetDsetTxtShad, 
      (XtPointer) SW_SurfCont_DsetTxtShad2, NULL},
      
   {  "B", &xmPushButtonWidgetClass, 
      '3', NULL, NULL, 
      SUMA_cb_SetDsetTxtShad, (XtPointer) SW_SurfCont_DsetTxtShad3, NULL},
   
   {  "Bs", &xmPushButtonWidgetClass, 
      '4', NULL, NULL, 
      SUMA_cb_SetDsetTxtShad, (XtPointer) SW_SurfCont_DsetTxtShad4, NULL},
            
   {  "Ta", &xmPushButtonWidgetClass, 
      '5', NULL, NULL, 
      SUMA_cb_SetDsetTxtShad, (XtPointer) SW_SurfCont_DsetTxtShad5, NULL},
            
   {  "Ba", &xmPushButtonWidgetClass, 
      '6', NULL, NULL, 
      SUMA_cb_SetDsetTxtShad, (XtPointer) SW_SurfCont_DsetTxtShad6, NULL},
            
   {NULL},
};

SUMA_MenuItem DsetGmatBord_Menu[] = {
   {  "XX", &xmPushButtonWidgetClass, 
      'X', NULL, NULL, 
      SUMA_cb_SetDsetGmatBord, 
      (XtPointer) SW_SurfCont_DsetGmatBord0, NULL},
      
   {  "5", &xmPushButtonWidgetClass, 
      '5', NULL, NULL, 
      SUMA_cb_SetDsetGmatBord, (XtPointer) SW_SurfCont_DsetGmatBord5, NULL},
   
   {  "10", &xmPushButtonWidgetClass, 
      '1', NULL, NULL, 
      SUMA_cb_SetDsetGmatBord, (XtPointer) SW_SurfCont_DsetGmatBord10, NULL},
    
   {  "20", &xmPushButtonWidgetClass, 
      '2', NULL, NULL, 
      SUMA_cb_SetDsetGmatBord, (XtPointer) SW_SurfCont_DsetGmatBord20, NULL},
    
   {  "40", &xmPushButtonWidgetClass, 
      'B', NULL, NULL, 
      SUMA_cb_SetDsetGmatBord, (XtPointer) SW_SurfCont_DsetGmatBord40, NULL},
            
   {NULL},
};

SUMA_MenuItem DsetNodeRad_Menu[] = {
   { "Cst", &xmPushButtonWidgetClass, 
      'C', NULL, NULL, 
      SUMA_cb_SetDsetNodeRad, (XtPointer) SW_SurfCont_DsetNodeRadConst, NULL},
   {  "Val", &xmPushButtonWidgetClass, 
      'V', NULL, NULL, 
      SUMA_cb_SetDsetNodeRad, (XtPointer) SW_SurfCont_DsetNodeRadVal, NULL},
   { "XXX", &xmPushButtonWidgetClass, 
      'X', NULL, NULL, 
      SUMA_cb_SetDsetNodeRad, (XtPointer) SW_SurfCont_DsetNodeRadXXX, NULL},    
   {NULL},
};

SUMA_MenuItem DsetThrough_Menu[] = {
   { "Edg", &xmPushButtonWidgetClass, 
      'E', NULL, NULL, 
      SUMA_cb_SetDsetThrough, (XtPointer) SW_SurfCont_DsetThroughEdge, NULL},
   {  "Col", &xmPushButtonWidgetClass, 
      'C', NULL, NULL, 
      SUMA_cb_SetDsetThrough, (XtPointer) SW_SurfCont_DsetThroughCol, NULL},
   { "Rad", &xmPushButtonWidgetClass, 
      'R', NULL, NULL, 
      SUMA_cb_SetDsetThrough, (XtPointer) SW_SurfCont_DsetThroughRad, NULL},    
   { "C&R", &xmPushButtonWidgetClass, 
      'B', NULL, NULL, 
      SUMA_cb_SetDsetThrough, (XtPointer) SW_SurfCont_DsetThroughCaR, NULL},    
   { "XXX", &xmPushButtonWidgetClass, 
      'X', NULL, NULL, 
      SUMA_cb_SetDsetThrough, (XtPointer) SW_SurfCont_DsetThroughXXX, NULL},    
   
   {NULL},
};

SUMA_MenuItem DsetEdgeThick_Menu[] = {
   { "Cst", &xmPushButtonWidgetClass, 
      'C', NULL, NULL, 
      SUMA_cb_SetDsetEdgeThick, (XtPointer)SW_SurfCont_DsetEdgeThickConst, NULL},
   {  "Val", &xmPushButtonWidgetClass, 
      'V', NULL, NULL, 
      SUMA_cb_SetDsetEdgeThick, (XtPointer) SW_SurfCont_DsetEdgeThickVal, NULL},
   {NULL},
};

SUMA_MenuItem DsetEdgeStip_Menu[] = {
   { "XXX", &xmPushButtonWidgetClass, 
      'X', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStipXXX, NULL},
   {  "Val", &xmPushButtonWidgetClass, 
      'V', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStipVal, NULL},
   { "s01", &xmPushButtonWidgetClass, 
      '1', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip1, NULL},
   { "s02", &xmPushButtonWidgetClass, 
      '2', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip2, NULL},
   { "s03", &xmPushButtonWidgetClass, 
      '3', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip3, NULL},
   { "s04", &xmPushButtonWidgetClass, 
      '4', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip4, NULL},
   { "s05", &xmPushButtonWidgetClass, 
      '5', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip5, NULL},
   { "s06", &xmPushButtonWidgetClass, 
      '6', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip6, NULL},
   { "s07", &xmPushButtonWidgetClass, 
      '7', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip7, NULL},
   { "s08", &xmPushButtonWidgetClass, 
      '8', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip8, NULL},
   { "s09", &xmPushButtonWidgetClass, 
      '9', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip9, NULL},
   { "s10", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip10, NULL},
   { "s11", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip11, NULL},
   { "s12", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip12, NULL},
   { "s13", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip13, NULL},
   { "s14", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip14, NULL},
   { "s15", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDsetEdgeStip, (XtPointer) SW_SurfCont_DsetEdgeStip15, NULL},
   {NULL},
};

SUMA_MenuItem TractStyle_Menu[] = {
   { "SLD", &xmPushButtonWidgetClass, 
      'X', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleSOLID, NULL},
   {  "HDE", &xmPushButtonWidgetClass, 
      'V', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleHIDE, NULL},
   { "s01", &xmPushButtonWidgetClass, 
      '1', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST1, NULL},
   { "s02", &xmPushButtonWidgetClass, 
      '2', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST2, NULL},
   { "s03", &xmPushButtonWidgetClass, 
      '3', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST3, NULL},
   { "s04", &xmPushButtonWidgetClass, 
      '4', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST4, NULL},
   { "s05", &xmPushButtonWidgetClass, 
      '5', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST5, NULL},
   { "s06", &xmPushButtonWidgetClass, 
      '6', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST6, NULL},
   { "s07", &xmPushButtonWidgetClass, 
      '7', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST7, NULL},
   { "s08", &xmPushButtonWidgetClass, 
      '8', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST8, NULL},
   { "s09", &xmPushButtonWidgetClass, 
      '9', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST9, NULL},
   { "s10", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST10, NULL},
   { "s11", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST11, NULL},
   { "s12", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST12, NULL},
   { "s13", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST13, NULL},
   { "s14", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST14, NULL},
   { "s15", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetTractStyle, (XtPointer) SW_SurfCont_TractStyleST15, NULL},
   {NULL},
};

SUMA_MenuItem DsetAlphaVal_Menu[] = {
   { "Max", &xmPushButtonWidgetClass, 
      'M', NULL, NULL, 
      SUMA_cb_SetDsetAlphaVal, (XtPointer) SW_SurfCont_DsetAlphaVal_Max, NULL},
   {  "Avg", &xmPushButtonWidgetClass, 
      'A', NULL, NULL, 
      SUMA_cb_SetDsetAlphaVal, (XtPointer) SW_SurfCont_DsetAlphaVal_Avg, NULL},
   { "Min", &xmPushButtonWidgetClass, 
      'm', NULL, NULL, 
      SUMA_cb_SetDsetAlphaVal, (XtPointer) SW_SurfCont_DsetAlphaVal_Min, NULL},
   { "I", &xmPushButtonWidgetClass, 
      'I', NULL, NULL, 
      SUMA_cb_SetDsetAlphaVal, (XtPointer) SW_SurfCont_DsetAlphaVal_I, NULL},
   { "T", &xmPushButtonWidgetClass, 
      'T', NULL, NULL, 
      SUMA_cb_SetDsetAlphaVal, (XtPointer) SW_SurfCont_DsetAlphaVal_T, NULL},
   { "B", &xmPushButtonWidgetClass, 
      'B', NULL, NULL, 
      SUMA_cb_SetDsetAlphaVal, (XtPointer) SW_SurfCont_DsetAlphaVal_B, NULL},
   { "XXX", &xmPushButtonWidgetClass, 
      'X', NULL, NULL, 
      SUMA_cb_SetDsetAlphaVal, (XtPointer) SW_SurfCont_DsetAlphaVal_XXX, NULL},
   {NULL},
};

SUMA_MenuItem TractMask_Menu[] = {
   {  "Hde", &xmPushButtonWidgetClass, 
      'H', NULL, NULL, 
      SUMA_cb_SetTractMask, (XtPointer)SW_SurfCont_TractMaskHide, NULL},
   {  "Gry", &xmPushButtonWidgetClass, 
      'G', NULL, NULL, 
      SUMA_cb_SetTractMask, (XtPointer) SW_SurfCont_TractMaskGray, NULL},
   {  "Dim", &xmPushButtonWidgetClass, 
      'D', NULL, NULL, 
      SUMA_cb_SetTractMask, (XtPointer) SW_SurfCont_TractMaskDim, NULL},
   {  "One", &xmPushButtonWidgetClass, 
      'O', NULL, NULL, 
      SUMA_cb_SetTractMask, (XtPointer) SW_SurfCont_TractMaskHair, NULL},
   {  "Ign", &xmPushButtonWidgetClass, 
      'I', NULL, NULL, 
      SUMA_cb_SetTractMask, (XtPointer) SW_SurfCont_TractMaskIgnore, NULL},
  {NULL},
};


SUMA_MenuItem DrawROI_SaveMode_Menu[]= {
   {  "NIML", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_SaveMode, (XtPointer) SW_DrawROI_SaveModeNIML, NULL},
   
   {  "1D", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_SaveMode, (XtPointer) SW_DrawROI_SaveMode1D, NULL},
   
   {NULL},
};

SUMA_MenuItem DrawROI_SaveWhat_Menu[]= {
   {  "All", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_SaveWhat, (XtPointer) SW_DrawROI_SaveWhatRelated, NULL},
         
   {  "This", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetDrawROI_SaveWhat, (XtPointer) SW_DrawROI_SaveWhatThis, NULL},
   
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

/* capture warnings from X and save them in Log
Had to add it to deal with the stupid   
   Illegal mnemonic character;  Could not convert X KEYSYM to a keycode
that seem to have no effect...
*/
void SUMA_XtWarn_handler(char *msg){
   static char FuncName[]={"SUMA_XtWarn_handler"};
   static long unsigned int warn_count=0;
   static int wrnstep=10;
   char *wrn=NULL;
   static char ibuf[256]={""};
   sprintf(ibuf,"  X11 Warning %ld:", warn_count+1);
   wrn = SUMA_append_string(ibuf,msg);
   if (! ((warn_count) % wrnstep) ) {
      SUMA_S_Notev("%s\n"
                   "  Have %ld X11 warnings so far, "
                   "see Help-->Message Log if curious.\n"
                   "  This notice is shown once for each additional "
                   "%d warnings.\n"
                   "\n", 
                   wrn, warn_count+1, wrnstep);
   }
   SUMA_L_Warn("%s",wrn);
   SUMA_free(wrn); wrn = NULL;
   ++warn_count;
   return ;
}

/* deal with fatal errors. Not used yet. */
int SUMA_XtErr_handler( Display *d , XErrorEvent *x ){
    static char FuncName[]={"SUMA_XtErr_handler"};
    char buf[256] = "(null)" ;
  
    if( x != NULL && d != NULL ) {
      XGetErrorText( d,x->error_code , buf,255 ) ;
    }
    SUMA_S_Errv( "Intercepted fatal X11 error: %s\n",buf) ;
   
   if( x != NULL) return (x->error_code);
   else return (-1);
}


/* Report error but don't die */
int SUMA_XErrHandler( Display *d , XErrorEvent *x )
{
   static char FuncName[]={"SUMA_XErrHandler"};
   char buf[256] = "(null)" ;
   
   SUMA_ENTRY;
   
   if( x != NULL && d != NULL ) XGetErrorText( d,x->error_code , buf,255 ) ;
   SUMA_S_Warn( "Intercepted X11 error: %s\n"                
                "Will attempt to proceed but trouble might ensue.",buf) ;
   SUMA_DUMP_TRACE("Trace At Xerr");
  
   SUMA_RETURN(0) ;
}


SUMA_Boolean SUMA_X_SurfaceViewer_Create (void)
{
   static char FuncName[]={"SUMA_X_SurfaceViewer_Create"};
   static int CallNum = 0;
   int ic = 0, icr=0;
   char *vargv[1]={ "[A] SUMA" };
   int cargc = 1, repos[5]={0,0,0,0,0};
   SUMA_Boolean NewCreation = NOPE, Found=NOPE, Inherit = NOPE;
   char slabel[20]="\0", *eee=NULL; 
   SUMA_Boolean LocalHead = NOPE;
       
   SUMA_ENTRY;
   /* Step 1. */
   if (CallNum == 0) { /* first call, initialize App */
      SUMA_LH("Entered 1st viewer creation");
      SUMAg_CF->N_OpenSV = 0;
      /*
         SUMAg_SVv[ic].X->TOPLEVEL = XtAppInitialize(&SUMAg_CF->X->App, "SUMA", 
                                                      NULL, 0, &cargc, vargv,
       SUMA_get_fallbackResources(), NULL, 0); Superseded by XtOpenApplication
      */
      SUMAg_SVv[ic].X->TOPLEVEL = 
         XtOpenApplication(&(SUMAg_CF->X->App), "SUMA", 
                           NULL, 0, &cargc, vargv,
                           SUMA_get_fallbackResources(), 
                           topLevelShellWidgetClass, NULL, 0); 
      SUMAg_SVv[ic].X->DPY = XtDisplay(SUMAg_SVv[ic].X->TOPLEVEL);
      
      if (!ic && (eee=SUMA_EnvVal("SUMA_Position_Original")) 
              && strcmp(eee, "TopLeft")) {
         int nv=0;
         double dv[6];
         if (SUMA_EnvEquals("SUMA_Position_Original", "RightOffset",1,NULL)){
            repos[0]=2; repos[1]=437; repos[2]=44;
         } else if ((nv = SUMA_StringToNum(eee,
                                     (void *)dv, 4, 2))==2 || nv == 4) {
            if (nv == 2) {
               repos[0]=2; repos[1]=(int)dv[0]; repos[2]=(int)dv[1];
            } else if (nv == 4) {
               repos[0]=4; repos[1]=(int)dv[0]; repos[2]=(int)dv[1];
                           repos[3]=(int)dv[2]; repos[4]=(int)dv[3];
            } 
         } else {
            SUMA_S_Warnv(
               "Ignored ill formatted env 'SUMA_Position_Original=%s'\n",
                  eee);
         }
         if (repos[1] < 0) repos[1] = 0;
         if (repos[2] < 0) repos[2] = 0;
      }
      /* catch some warnings that are not important */
      XtAppSetWarningHandler(SUMAg_CF->X->App,SUMA_XtWarn_handler) ;
      
      /* save DPY for first controller opened */
      SUMAg_CF->X->DPY_controller1 = SUMAg_SVv[ic].X->DPY;
      NewCreation = YUP;
      Inherit = NOPE;
      
      (void)XSetErrorHandler(SUMA_XErrHandler);
      
   } else {/* not the first call, new controller is required */
      SUMA_LH("Entered subsequent viewers creation");
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
      SUMA_LH("New creation, Inherit=%d", Inherit);
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
           /* Next call Causes Seg. fault on Fedora Core 4. 
         Not much I can do. Same happens with demo code paperplane.c 
         by Kilgard */
         Gmainw = XmCreateMainWindow (  SUMAg_SVv[ic].X->TOPLEVEL, 
                                       "mainw", NULL, 0); 
         SUMA_LH("Managing Main Window");
         XtManageChild (Gmainw);      
         /* create menu bar */
         Gmenubar = XmCreateMenuBar (Gmainw, "menubar", NULL, 0);
         XtManageChild (Gmenubar);
         
         /* create File Menu */
         SUMA_BuildMenuReset(0);
         SUMA_BuildMenu(Gmenubar, XmMENU_PULLDOWN, 
                                 "File", 'F', YUP, File_menu, 
                                 (VOID_CAST)ic, NULL, NULL, NULL,  
                                 SUMAg_SVv[ic].X->FileMenu );
         
         /* create View Menu */
         SUMA_BuildMenuReset(0);
         SUMA_BuildMenu(Gmenubar, XmMENU_PULLDOWN, 
                                 "View", 'V', YUP, View_menu, 
                                 (VOID_CAST)ic, NULL, NULL, NULL,  
                                 SUMAg_SVv[ic].X->ViewMenu );
         
         /* create Tools Menu */
         SUMA_BuildMenuReset(0);
         SUMA_BuildMenu(Gmenubar, XmMENU_PULLDOWN, 
                                 "Tools", 'T', YUP, Tools_menu, 
                                 (VOID_CAST)ic, NULL, NULL, NULL,  
                                 SUMAg_SVv[ic].X->ToolsMenu );
         
         /* create Help Menu */
         SUMA_BuildMenuReset(0);
         SUMA_BuildMenu(Gmenubar, XmMENU_PULLDOWN, 
                                 "Help", 'H', YUP, Help_menu,
                                 (VOID_CAST)ic, NULL, NULL, NULL,  
                                 SUMAg_SVv[ic].X->HelpMenu );
         
         XtVaSetValues (Gmenubar, XmNmenuHelpWidget, 
                        SUMAg_SVv[ic].X->HelpMenu->mw[SW_Help], NULL);
                                 
         /* set states of the some view menu widgets */
         XmToggleButtonSetState (
            SUMAg_SVv[ic].X->ViewMenu->mw[SW_ViewCrossHair], 
            SUMAg_SVv[ic].ShowCrossHair, NOPE);
         XmToggleButtonSetState (
            SUMAg_SVv[ic].X->ViewMenu->mw[SW_ViewSelectedFaceset], 
            SUMAg_SVv[ic].ShowSelectedFaceSet, NOPE);
         XmToggleButtonSetState (
            SUMAg_SVv[ic].X->ViewMenu->mw[SW_ViewNodeInFocus], 
            SUMAg_SVv[ic].ShowSelectedDatum, NOPE);
            
         XmToggleButtonSetState (
            SUMAg_SVv[ic].X->HelpMenu->mw[SW_HelpMemTrace], 
            SUMAg_CF->MemTrace, NOPE);
         if (SUMAg_CF->MemTrace) {  
            XtSetSensitive(SUMAg_SVv[ic].X->HelpMenu->mw[SW_HelpMemTrace], 0); }

         XmToggleButtonSetState (SUMAg_SVv[ic].X->HelpMenu->mw[SW_HelpIONotify],
            SUMAg_CF->InOut_Notify, NOPE);
         XmToggleButtonSetState (
            SUMAg_SVv[ic].X->HelpMenu->mw[SW_HelpEchoKeyPress],
            SUMAg_CF->Echo_KeyPress, NOPE);
 
         
        SUMAg_SVv[ic].X->CMAP = SUMA_getShareableColormap(&(SUMAg_SVv[ic]));

        /* create a frame to put glxarea in */
        SUMAg_SVv[ic].X->FRAME = XmCreateFrame (Gmainw, "frame", NULL, 0);
        XtManageChild(SUMAg_SVv[ic].X->FRAME);
      
      #ifdef SUMA_MOTIF_GLXAREA
        SUMA_LH("MOTIF Drawing Area");
        /* Step 6. */
         /* glwMDrawingAreaWidgetClass requires libMesaGLwM.a */

        if( glwMDrawingAreaWidgetClass == NULL ) {
            fprintf(stderr,"** ERROR: glwMDrawingAreaWidgetClass is NULL\n"
            "   This might be an error in GLwDrawA.h where the class is\n"
            "   not referenced using 'extern'.  An alternative is to use\n"
            "   the local build of libGLws.a.\n");
            exit(1);
        }

        SUMAg_SVv[ic].X->GLXAREA = XtVaCreateManagedWidget("glxarea",
          glwMDrawingAreaWidgetClass, SUMAg_SVv[ic].X->FRAME,
          GLwNvisualInfo, SUMAg_SVv[ic].X->VISINFO,
          XtNcolormap, SUMAg_SVv[ic].X->CMAP,
          NULL);
      #else
        SUMA_LH("GL Drawing Area");

        /* glwDrawingAreaWidgetClass requires libMesaGLw.a */

        /* -------------------------------------------------------------------
         * check for NULL glwDrawingAreaWidgetClass        23 May 2017 [rickr]
         *
         * If glwDrawingAreaWidgetClass was declared without extern, we might
         * be seeing a local version of it (with a NULL value), rather than a
         * reference to the existing library verison (if the library is loaded
         * dynamically, the variable might not be seen at compile time).
         *
         * see https://www.cygwin.com/ml/cygwin-xfree/2014-10/msg00040.html
         * ------------------------------------------------------------------ */
        if( glwDrawingAreaWidgetClass == NULL ) {
            fprintf(stderr,"** ERROR: glwDrawingAreaWidgetClass is NULL\n"
            "   This might be an error in GLwDrawA.h where the class is\n"
            "   not referenced using 'extern'.  An alternative is to use\n"
            "   the local build of libGLws.a.\n");
            exit(1);
        }

        SUMAg_SVv[ic].X->GLXAREA = XtVaCreateManagedWidget("glxarea",
          glwDrawingAreaWidgetClass, SUMAg_SVv[ic].X->FRAME,
          GLwNvisualInfo, SUMAg_SVv[ic].X->VISINFO,
          XtNcolormap, SUMAg_SVv[ic].X->CMAP,
          NULL);
      #endif

          
      /* Step 7. */
      SUMA_LH("Callbacks");
      XtAddCallback( SUMAg_SVv[ic].X->GLXAREA, GLwNginitCallback, 
                     SUMA_graphicsInit, NULL);
      XtAddCallback( SUMAg_SVv[ic].X->GLXAREA, GLwNexposeCallback, 
                     SUMA_expose, NULL);
      XtAddCallback( SUMAg_SVv[ic].X->GLXAREA, GLwNresizeCallback, 
                     SUMA_resize, NULL);
      XtAddCallback( SUMAg_SVv[ic].X->GLXAREA, GLwNinputCallback, 
                     SUMA_input, NULL);

      /* trap for window kill */
      
      /* turn off default delete response. If you do not do that, 
         you will suffer.*/
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
      SUMA_LH("Getting a grahics context");
      if (0){ /* This has caused me lots of grief with XQuartz, on 10.8 systems
                 and possibly before. If this block is executed, I get undefined
                 buffer errors (right before glClear) and failures to set 
                 the current graphic context with glXCreateContext().
                 The problems are intermittent however and at times tricky to
                 reproduce and they only occur whenver a new controller
                 is opened with 'ctrl+n' the first time. You have better odds
                 of causing the problem if you resize the very first window
                 right before opening the new controller.
                 In any case, this block is not needed at all anymore since 
                 X->gc is no longer used anywhere. */
         XGCValues gcv; /* see program drawing.c in Motif Programming Manual, 
                           Ch. 10 */
         gcv.foreground = 
            BlackPixelOfScreen (XtScreen (SUMAg_SVv[ic].X->GLXAREA));
         SUMAg_SVv[ic].X->gc = XCreateGC (SUMAg_SVv[ic].X->DPY,
                                          XtWindow (SUMAg_SVv[ic].X->GLXAREA), 
                                          GCForeground, &gcv);
         SUMA_SetSVForegroundColor (&SUMAg_SVv[ic], "Green");
      }
      
      
      /* keep track of count */
      SUMAg_N_SVv += 1;
      SUMA_LH("Repos");
      /* initial repositioning, just for 1st creation */
      if (1) {
      switch (repos[0]) {
         case 4:
            XtVaSetValues (SUMAg_SVv[ic].X->TOPLEVEL, 
               XmNx, (Position)(repos[1]),
               XmNy, (Position)(repos[2]),
               XmNwidth, (repos[3]),
               XmNheight, (repos[4]),
               NULL);
            break;
         case 2:
            XtVaSetValues (SUMAg_SVv[ic].X->TOPLEVEL, 
               XmNx, (Position)(repos[1]),
               XmNy, (Position)(repos[2]),
               NULL);
            break;
         default:
            break;
      }
      }
      
      /* position window next to the previous one open */
      if (1) {
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
      SUMA_SV_InitDrawAreaOffset(SUMAg_SVv+ic); 

      SUMA_LH("Done with new window setup");  
   } else {    /* widget already set up, just undo whatever 
                  was done in SUMA_ButtClose_pushed */
      
      switch (SUMA_GL_CLOSE_MODE) {
         case SUMA_WITHDRAW:
            XMapRaised( SUMAg_SVv[ic].X->DPY, 
                        XtWindow(SUMAg_SVv[ic].X->TOPLEVEL)); 
            break;
         case SUMA_UNREALIZE:
            SUMA_LH("Realizing");
            XtRealizeWidget(SUMAg_SVv[ic].X->TOPLEVEL);
            XSync(SUMAg_SVv[ic].X->DPY, 0);  /* Don't know if this helps for sure
                                                Part of the OS X 10.5 GLX crash 
                                                from hell */
            break;
         default:
            SUMA_S_Err("Not set up for this closing mode");
            SUMA_RETURN(NOPE);
            break;
      }
      /* add the workprocess again */
      SUMA_register_workproc( SUMA_handleRedisplay, SUMAg_SVv[ic].X->GLXAREA );
      SUMAg_SVv[ic].X->REDISPLAYPENDING = 0;
   }

   SUMAg_SVv[ic].Open = YUP;
   ++SUMAg_CF->N_OpenSV; 
   ++CallNum;
         
   SUMA_LH("Updates");  
   SUMA_UpdateViewerCursor (&(SUMAg_SVv[ic]));
   SUMA_UpdateViewerTitle (&(SUMAg_SVv[ic]));

   SUMA_LH("Returning");
   SUMA_RETURN (YUP);
}

void SUMA_ButtOpen_pushed (Widget w, XtPointer cd1, XtPointer cd2)
{
   static char FuncName[]={"SUMA_ButtOpen_pushed"};
   
   SUMA_ENTRY;

   if (!SUMA_X_SurfaceViewer_Create ()) {
      fprintf (SUMA_STDERR,
               "Error %s: Failed in SUMA_X_SurfaceViewer_Create.\n", FuncName);
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
      fprintf (SUMA_STDERR, 
         "Error %s: Something really bad has happened.\n", FuncName);
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
      fprintf (SUMA_STDERR, 
               "Error %s: Something really bad has happened.\n", FuncName);
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
   int ic, Found, isv = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_LH("Called");
   ic = 0;
   Found = 0;
   while (ic < SUMA_MAX_SURF_VIEWERS && !Found) {
      #if 0 
      /*use once you have a close button with its widget*/
      if (SUMAg_SVv[ic].X->ButtClose == w) {
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: Close order from button.\n", FuncName);
         Found = 1;
      }
      #endif
      if (SUMAg_SVv[ic].X->TOPLEVEL == w) {
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Close order from window manager for viewer %d.\n", 
                     FuncName, ic);
         Found = 1;
      }else if (SUMAg_SVv[ic].X->GLXAREA == w) { 
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Close order from GLX area for viewer %d.\n", 
                     FuncName, ic);
         Found = 1;
      }
      
      if (!Found) ++ic;
   }
   
   if (Found) {
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: Widget Found, ic = %d\n", FuncName, ic);
         
         /* Must turn off all workprocesses and timeouts for 
            this surface viewer */
         
         if (!SUMAg_SVv[ic].Open) {
            SUMA_S_Errv("Viewer %d already closed (%d)\n", 
                       ic, SUMAg_SVv[ic].Open);
         }
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Turning off workprocesses and timeouts ...\n", 
                     FuncName);
         if (SUMAg_SVv[ic].GVS[SUMAg_SVv[ic].StdView].ApplyMomentum) {
            if (SUMAg_SVv[ic].X->MOMENTUMID) 
               XtRemoveTimeOut(SUMAg_SVv[ic].X->MOMENTUMID); 
            SUMAg_SVv[ic].X->MOMENTUMID = 0;
         }
         
         /* remove Redisplay workprocess*/
         SUMA_remove_workproc2( SUMA_handleRedisplay, SUMAg_SVv[ic].X->GLXAREA );
         
                  
         /* done cleaning up, deal with windows ... */
         
         /** 
            SEE UPDATED NOTE IN SUMA_display.h
            The comment below is obsolete. 
            ==================================
         Fri Jan  3 09:51:35 EST 2003
             XtUnrealizeWidget is not used anymore because it destroys 
             windows associated with a widget and its descendants.
            There's no need for that here. 
            Also, destroying widgets should not be used either because that would
            automatically destroy the SUMA controller which is a 
            child of one of the viewers. The code for destroy is left for
            historical reasons.*/
         switch (SUMA_GL_CLOSE_MODE) {
            case SUMA_WITHDRAW: 
               if (LocalHead) 
                  fprintf (SUMA_STDERR,"%s: Withdrawing it.\n", FuncName);

               XWithdrawWindow(SUMAg_SVv[ic].X->DPY, 
                  XtWindow(SUMAg_SVv[ic].X->TOPLEVEL), 
                  XScreenNumberOfScreen(XtScreen(SUMAg_SVv[ic].X->TOPLEVEL)));
               if (SUMAg_SVv[ic].X->ViewCont->TopLevelShell) {
                  XWithdrawWindow(SUMAg_SVv[ic].X->DPY, 
                  XtWindow(SUMAg_SVv[ic].X->ViewCont->TopLevelShell),
                  XScreenNumberOfScreen(XtScreen(
                                    SUMAg_SVv[ic].X->ViewCont->TopLevelShell)));
               }
               break;
            case SUMA_UNREALIZE:
               SUMA_LHv("Unrealizing viewer %d\n", ic);
               /* Unrealizing on OS X 10.6 does not crash as it might on 10.5
               However, you will get something like the following messages
               right after unrealizing the widget. 
               Don't know what to make of this at this time. ZSS: Oct 05 2009 
               
Mon Oct  5 17:48:12 eomer.nimh.nih.gov suma[4538] <Error>: kCGErrorIllegalArgument: CGSGetSurfaceBounds
Mon Oct  5 17:48:12 eomer.nimh.nih.gov suma[4538] <Error>: kCGErrorIllegalArgument: CGSBindSurface: Invalid window 0xd1
Mon Oct  5 17:48:12 eomer.nimh.nih.gov suma[4538] <Error>: kCGErrorIllegalArgument: CGSBindSurface: Invalid window 0xd1
Mon Oct  5 17:48:12 eomer.nimh.nih.gov suma[4538] <Error>: kCGErrorIllegalArgument: CGSBindSurface: Invalid window 0xd1
Mon Oct  5 17:48:12 eomer.nimh.nih.gov suma[4538] <Error>: kCGErrorIllegalArgument: CGSBindSurface: Invalid window 0xd1
Mon Oct  5 17:48:12 eomer.nimh.nih.gov suma[4538] <Error>: unknown error code: invalid drawable    
               */           
               XtUnrealizeWidget(SUMAg_SVv[ic].X->TOPLEVEL);
               break;
            case SUMA_DESTROY: 
               if (LocalHead) 
                  fprintf (SUMA_STDERR,"%s: Destroying it.\n", FuncName);
               XtDestroyWidget(SUMAg_SVv[ic].X->TOPLEVEL);
               SUMAg_SVv[ic].X->TOPLEVEL = NULL;      
            
               /* no need to destroy viewer controller */
               SUMAg_SVv[ic].X->ViewCont->TopLevelShell = NULL;
            
               /* update the count */
               SUMAg_N_SVv -= 1;
               break;
            default:
               SUMA_S_Err("Not set to deal with this closing mode");
               SUMA_RETURNe;
         }

         /* Textures displayed before closing a viewer no longer show up after
         the viewer is reopened. Reloading the texture with SUMA_VE_LoadTexture()
         brings it back, but that's a slow call that should only be done when
         necessary. This next call resets the records of which textures have 
         been loaded already so that at the next drawing operation the loading
         is rerun and the texture marked as loaded */
         if (!SUMA_SV_Mark_Textures_Status(SUMAg_SVv+ic, "unloaded_all", 
                                             NULL, 0, 0)){
            SUMA_S_Err("Failed to mark all textures as unloaded");
            SUMA_RETURNe;
         }

         SUMAg_SVv[ic].Open = NOPE;
         --SUMAg_CF->N_OpenSV;
         if (SUMAg_CF->N_OpenSV == 0) {
            if (LocalHead) 
               fprintf (SUMA_STDERR,"%s: No more viewers, exiting.\n", FuncName);
            /* not quite necessary but for completeness */
            if (SUMAg_CF->X->SumaCont->AppShell) {
               XtDestroyWidget(SUMAg_CF->X->SumaCont->AppShell);
            }
            selenium_close();/* close selenium opened browser windows if open */
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
   

   /* When using multiple viewers, you must reset the 
      OpenGL state variables or risk having abrupt changes with the 
      first click */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }
   SUMAg_CF->PointerLastInViewer = isv;
   
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
   
   if (LocalHead) 
      fprintf (SUMA_STDERR, "%s: in Surface Viewer #%d.\n", FuncName, isv);
   sv->ResetGLStateVariables = YUP;  

   SUMA_postRedisplay(w, clientData, NULL);

   
   SUMA_RETURNe;
}

void SUMA_unSetcSV (Widget w, XtPointer clientData, 
                     XEvent * event, Boolean * cont)
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
SUMA_generateEPS( char *filename, int inColor, 
                  unsigned int width, unsigned int height)
{
   FILE *fp;
   GLvoid *pixels;
   unsigned char *curpix;
   int components, pos, i;
   static char FuncName[]={"SUMA_generateEPS"};
   
   SUMA_ENTRY;

   pixels = SUMA_grabPixels(inColor ? 3:1, width, height);
   
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

GLvoid *SUMA_grabRenderedPixels(SUMA_SurfaceViewer *sv, int ColorDepth, 
                        unsigned int width, unsigned int height, int getback)
{
   GLvoid *buffer=NULL;
   if (!sv || !sv->X) return(buffer);
   if (!getback) SUMA_GLX_BUF_SWAP(sv); /* return to last rendered buffer */
   buffer = SUMA_grabPixels(ColorDepth, width, height);
   if (!getback) SUMA_GLX_BUF_SWAP(sv);  /* return to current buffer */
   return(buffer);
}

GLvoid *SUMA_grabPixels(int ColorDepth, 
                        unsigned int width, unsigned int height)
{
   static char FuncName[]={"SUMA_grabPixels"};
   GLvoid *buffer;
   GLint swapbytes, lsbfirst, rowlength;
   GLint skiprows, skippixels, alignment;
   GLenum format;
   int tp, saverestore;
   unsigned int size;

   SUMA_ENTRY;
   
   if (ColorDepth == 0) ColorDepth = 1;   /* for backward compatibility */
   
   saverestore = 1;      
   switch (ColorDepth) {
      case GL_LUMINANCE:
      case 1:
         format = GL_LUMINANCE;
         tp = GL_UNSIGNED_BYTE;
         size = width * height * 1;
         break;
      case GL_RGB:
      case 3:
         format = GL_RGB;
         tp = GL_UNSIGNED_BYTE;
         size = width * height * 3;
         break;
      case GL_RGBA:
      case 4:
         format = GL_RGBA;
         tp = GL_UNSIGNED_BYTE;
         size = width * height * 4;
         break;
      case GL_DEPTH_COMPONENT:
      case 5:
         saverestore = 0;
         format = GL_DEPTH_COMPONENT;
         tp = GL_FLOAT;
         size = width * height * 1 * sizeof(GLfloat);
         break;
      case GL_STENCIL_INDEX:
      case 6:
         saverestore = 0;
         format = GL_STENCIL_INDEX;
         tp = GL_FLOAT; /* Unsigned byte did not work */
         size = width * height * 1 * sizeof(GLfloat);
         break;
      default:
         SUMA_S_Errv("Bad ColorDepth or format of %d\n", ColorDepth);
         SUMA_RETURN(NULL);
   }

   buffer = (GLvoid *) SUMA_malloc(size);
   if (buffer == NULL)
    SUMA_RETURN (buffer);

   
   if (saverestore) { /* Using this makes depth and stencil index reads fail,
                         at least on mac os x         ZSS Dec 31 2013*/
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
   }
   
   /* Actually read the pixels. */
   glReadPixels(0, 0, width, height, format,
                tp, (GLvoid *) buffer);

   if (saverestore) {
      /* Restore saved modes. */
      glPixelStorei(GL_PACK_SWAP_BYTES, swapbytes);
      glPixelStorei(GL_PACK_LSB_FIRST, lsbfirst);
      glPixelStorei(GL_PACK_ROW_LENGTH, rowlength);
      glPixelStorei(GL_PACK_SKIP_ROWS, skiprows);
      glPixelStorei(GL_PACK_SKIP_PIXELS, skippixels);
      glPixelStorei(GL_PACK_ALIGNMENT, alignment);
   }
   
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
   int isv=-1;
   char buf[32];
   static char FuncName[]={"SUMA_RenderToPixMap"};

   SUMA_ENTRY;
   SUMA_S_Note("CALLED!");
   dpy = XOpenDisplay(NULL);
   if (dpy == NULL)
    fprintf(SUMA_STDERR,"Error %s: could not open display", FuncName);

   if (!glXQueryExtension(dpy, NULL, NULL))
    fprintf(SUMA_STDERR,
            "Error %s: X server has no OpenGL GLX extension", FuncName);

   /* find an OpenGL-capable RGB visual with depth buffer */
   #if 1  /* use screen rendering Xvisual */
   vi = glXChooseVisual(dpy, DefaultScreen(dpy), &configuration[1]);
   if (vi == NULL) {
   /*fprintf(SUMA_STDERR,"%s: Trying to use useless double 
               buffering configuration.\n", FuncName);*/
    vi = glXChooseVisual(dpy, DefaultScreen(dpy), &configuration[0]);
    if (vi == NULL) {
      fprintf( SUMA_STDERR,
               "Error %s: no appropriate RGB visual with depth buffer", 
               FuncName);
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
    fprintf(SUMA_STDERR,
            "Error %s: could not create rendering context", FuncName);

   pmap = XCreatePixmap(dpy, RootWindow(dpy, vi->screen),
    csv->X->aWIDTH, csv->X->aHEIGHT, vi->depth);
   glxpmap = glXCreateGLXPixmap(dpy, vi, pmap);
   isv = SUMA_WhichSV(csv, SUMAg_SVv, SUMAg_N_SVv);
   sprintf(buf,"pixmap of sv %d", isv);
   if (!SUMA_glXMakeCurrent(dpy, glxpmap, cx, FuncName, buf, 1)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_glXMakeCurrent.\n \tContinuing ...\n", 
               FuncName);
      SUMA_GL_ERRS;

      SUMA_RETURN(NOPE);
   }

   SUMA_context_Init(csv);
   glViewport(0, 0, csv->X->aWIDTH, csv->X->aHEIGHT);
   SUMA_display(csv, dov);

   glFinish (); /* make sure you wait until rendering is over */

   /* find out the next best name and write it*/
   {
      char tmpprfx[100], *padprfx, *padname;
      int cntindx=0;
      SUMA_SurfaceObject *SO;
      SUMA_Boolean OKname = NOPE;
      
      /* get the SO in focus, use it's label for output filename */
      SO = SUMA_SV_Focus_SO(csv);
      
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
     SUMA_generateEPS(padname, /* color */ 1, csv->X->aWIDTH, csv->X->aHEIGHT);
     fprintf (SUMA_STDOUT,"Done.\n");
     SUMA_free(padname);
   }

   /* render to original context */
   /* SUMA_HOLD_IT; Not used anymore */
   sprintf(buf,"GLXAREA of sv %d",  isv);
   if (!SUMA_glXMakeCurrent( XtDisplay(csv->X->GLXAREA), 
             XtWindow(csv->X->GLXAREA),  csv->X->GLXCONTEXT,
             FuncName, buf, 1)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_glXMakeCurrent.\n \tContinuing ...\n", 
               FuncName);
      SUMA_GL_ERRS;

      SUMA_RETURN (NOPE);   
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
   int lay=-1, N_neighbs=0, *neighbs=NULL;
   double *p=NULL;
   double *s=NULL, dot=0.0, dotmax=0.0;
   int *q=NULL;
   static int offset_N_Node=-1;
   static SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   double dir[3]={0.0,0.0,0.0}, norm=0.0;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !sv || !dd) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(-2);
   }
   
   lay = sv->KeyNodeJump;
   
   if (sv->KeyNodeJump > 1) {
         if (offset_N_Node != SO->N_Node) {
            /* need to reinitialize */
            if (OffS) SUMA_Free_getoffsets(OffS); OffS=NULL;
            OffS = SUMA_Initialize_getoffsets (SO->N_Node);
            offset_N_Node = SO->N_Node;
         } else {
            SUMA_Recycle_getoffsets(OffS);
         }
         if (!SUMA_getoffsets2(inode, SO, -(sv->KeyNodeJump+1),
                               OffS, NULL, 0)) {
            SUMA_S_Err("Failed to get offsets");
            SUMA_RETURN(-2);
         }   
   }
   
   do {
      if (lay == 1) { 
         neighbs = SO->FN->FirstNeighb[inode];
         N_neighbs = SO->FN->N_Neighb[inode];
      } else {
         neighbs = OffS->layers[lay].NodesInLayer;
         N_neighbs = OffS->layers[lay].N_NodesInLayer;
      }
      /* Put the neighbors in p*/
      p = (double *)SUMA_calloc( SO->NodeDim*(N_neighbs+1), 
                                 sizeof(double));
      s = (double *)SUMA_calloc( SO->NodeDim*(N_neighbs+1), 
                                 sizeof(double));
      q = (int *)SUMA_calloc( N_neighbs+1, sizeof(int));
      for (jj=0; jj<SO->NodeDim; ++jj) {
         p[jj] = SO->NodeList[SO->NodeDim*inode+jj];
      }
      for (ii=0; ii<N_neighbs; ++ii) {
         for (jj=0; jj<SO->NodeDim; ++jj) {
            ineighb = neighbs[ii];
            p[SO->NodeDim*(ii+1)+jj] =
                  SO->NodeList[SO->NodeDim*ineighb+jj];
         }
      }
      /* find screen projection of neighbors */
      if (!SUMA_World2ScreenCoords( sv, N_neighbs+1,
                                    p , s, q, YUP, YUP)) {
         SUMA_S_Err("The world has failed me");                                           SUMA_RETURN(-2);
      }

      if (LocalHead) {
         fprintf(SUMA_STDERR," S = [\n");
         for (ii=0; ii<N_neighbs; ++ii) {
            fprintf(SUMA_STDERR, "%.3f  %.3f  %.3f\n", 
                                 s[3*ii], s[3*ii+1], s[3*ii+2]);
         }
         fprintf(SUMA_STDERR,"];\n");
      }

      /* find closest to desired direction */
      for (ii=0; ii<N_neighbs; ++ii) { 
                                 /* for each neighbor*/        
         /* direction on screen (only x y needed)*/
         for (jj=0; jj<2; ++jj) {
            dir[jj] = s[(ii+1)*SO->NodeDim+jj] - s[jj]; 
         }
         SUMA_NORM_VEC(dir,2,norm);
         /* calculate dot product*/
         dot = dir[0]*dd[0]/norm + dir[1]*dd[1]/norm;
         
         if (ii==0) { 
            dotmax = dot; idd=neighbs[ii]; 
         } else {
            if (dot > dotmax) {
               dotmax = dot; idd=neighbs[ii];
            }
         }
      }
      if (dotmax > 0) {
         inodenext = idd;
         SUMA_LHv("Next node should be %d\n", inodenext);
      } else {
         SUMA_LH("No good direction");
         inodenext = -1; 
      }

      if (p) SUMA_free(p); p = NULL;
      if (q) SUMA_free(q); q = NULL;
      if (s) SUMA_free(s); s = NULL;
      
      lay = lay -1;
   } while (lay > 0 && inodenext < 0);
   
   
   SUMA_RETURN(inodenext);
}   

/*!
   Purpose: Takes a the world x,y,z coordinates and turns them into 
            screen coordinates
   Set the ApplyXform to 0 (or NOPE) if you are calling this function 
   after the projection  and other viewing matrices have been set. 
   This happens when this function is called as
   a child of SUMA_display
   \sa SUMA_GetSelectionLine
   \sa SUMA_World2ScreenCoordsF, sync modifications
*/
SUMA_Boolean SUMA_World2ScreenCoords (
                     SUMA_SurfaceViewer *sv, int N_List, double *WorldList, 
                     double *ScreenList, int *Quad, SUMA_Boolean ApplyXform,
                     SUMA_Boolean ScreenY)
{
   static char FuncName[]={"SUMA_World2ScreenCoords"};
   GLfloat rotationMatrix[4][4];
   GLint viewport[4];
   GLdouble mvmatrix[16], projmatrix[16];
   int i, i3;
   char CommString[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv && (Quad || ApplyXform)) {
      SUMA_S_Err("NULL sv with Quad or Xform. I need sv for that");
      SUMA_RETURN(NOPE);
   }
   if (LocalHead && sv) {
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
      if (ScreenY) { 
         ScreenList[i3+1] = viewport[3] - ScreenList[i3+1] - 1; /* change from 
                                                   OpenGL's y to screen's y */
      }

      if (Quad) {
         if (ScreenList[i3] < sv->X->aWIDTH/2) {
            if (ScreenList[i3+1] > sv->X->aHEIGHT/2) 
               Quad[i] = SUMA_LOWER_LEFT_SCREEN;
            else Quad[i] = SUMA_UPPER_LEFT_SCREEN;
         } else {
            if (ScreenList[i3+1] > sv->X->aHEIGHT/2) 
               Quad[i] = SUMA_LOWER_RIGHT_SCREEN;
            else Quad[i] = SUMA_UPPER_RIGHT_SCREEN;
         }
      }
      if (LocalHead) 
         fprintf (SUMA_STDOUT, 
                  "%s: World: [%.2f %.2f %.2f] \t "
                  "Screen [%.2f %.2f %.2f] \t Quad %d\n", 
                  FuncName, 
                  WorldList[i3],WorldList[i3+1], WorldList[i3+2], 
                  ScreenList[i3], ScreenList[i3+1],ScreenList[i3+2], 
                  Quad?Quad[i]:-999);
   }

   if (ApplyXform) glPopMatrix();

   SUMA_RETURN (YUP);
}

/* Floating point precision version of SUMA_World2ScreenCoords.
   Keep in sync with SUMA_World2ScreenCoords */
SUMA_Boolean SUMA_World2ScreenCoordsF (
                     SUMA_SurfaceViewer *sv, int N_List, float *WorldList, 
                     float *ScreenList, int *Quad, SUMA_Boolean ApplyXform,
                     SUMA_Boolean ScreenY)
{
   static char FuncName[]={"SUMA_World2ScreenCoordsF"};
   GLfloat rotationMatrix[4][4];
   GLint viewport[4];
   GLdouble mvmatrix[16], projmatrix[16], scd[3];
   int i, i3;
   char CommString[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!sv && (Quad || ApplyXform)) {
      SUMA_S_Err("NULL sv with Quad or Xform. I need sv for that");
      SUMA_RETURN(NOPE);
   }
   if (LocalHead && sv) {
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
   /* initialize because if gluProject fails without touching scd, and
      then copying scd's contents without any initialization causes
      valgrind to gripe, and rightly so. */
   scd[0] = scd[1] = scd[2] = 0.0;
   for (i=0;i<N_List; ++i) {
      i3 = 3*i;
      if (!(gluProject( (GLdouble)WorldList[i3], 
                        (GLdouble)WorldList[i3+1], (GLdouble)WorldList[i3+2],  
                         mvmatrix, projmatrix, viewport, 
                        (GLdouble*)(scd  ), 
                        (GLdouble*)(scd+1), 
                        (GLdouble*)(scd+2) ))) {
         if (LocalHead) {
            int iim;
            SUMA_S_Warn(
               "gluProject Failed, GL_MODELVIEW_MATRIX likely all zeros."
               "This might happen if the rendering context is not set.");
            SUMA_GL_ERRS;
            SUMA_DUMP_TRACE("Trace at gluProject failure");
            fprintf(SUMA_STDERR,"ModelView Matrix: (col major)\n");
            iim = 0; while (iim < 16) {
               fprintf(SUMA_STDERR,"%.3f ", mvmatrix[iim]); ++iim;
            }
            fprintf(SUMA_STDERR,"\nProjection Matrix: (col major)\n");
            iim = 0; while (iim < 16) {
               fprintf(SUMA_STDERR,"%.3f ", projmatrix[iim]); ++iim;
            }
         }
         /* Set all output to zero and proceed */
         SUMA_LH("Failed in gluProject, skipping");
         memset(ScreenList, 0, 3*N_List*sizeof(float));
         break;               
      }
      ScreenList[i3] = scd[0]; 
      if (ScreenY) {
         ScreenList[i3+1] = viewport[3] - scd[1] - 1; /* change from 
                                                   OpenGL's y to screen's y */
      } else {
         ScreenList[i3+1] = scd[1];
      }
      ScreenList[i3+2] = scd[2];
       
      if (Quad) {
         if (ScreenList[i3] < sv->X->aWIDTH/2) {
            if (ScreenList[i3+1] > sv->X->aHEIGHT/2) 
               Quad[i] = SUMA_LOWER_LEFT_SCREEN;
            else Quad[i] = SUMA_UPPER_LEFT_SCREEN;
         } else {
            if (ScreenList[i3+1] > sv->X->aHEIGHT/2) 
               Quad[i] = SUMA_LOWER_RIGHT_SCREEN;
            else Quad[i] = SUMA_UPPER_RIGHT_SCREEN;
         }
      }
      if (LocalHead) 
         fprintf (SUMA_STDOUT, 
                  "%s: World: [%.2f %.2f %.2f] \t "
                  "Screen [%.2f %.2f %.2f] \t Quad %d\n", 
                  FuncName, 
                  WorldList[i3],WorldList[i3+1], WorldList[i3+2], 
                  ScreenList[i3], ScreenList[i3+1],ScreenList[i3+2], 
                  Quad?Quad[i]:-999);
   
   }

   if (ApplyXform) glPopMatrix();

   SUMA_RETURN (YUP);
}

/* 
   Depth sorting of locations in NodeList
   
   All viewing matrices should be applied before this function is called 
   Retruns sorting map, isrt (int *), such that NodeList[3*isrt[0]] is 
   the location farthest from the eyeball. 
   
   if (xform_NodeList) then NodeList's content is replaced with the
   screen projection coords.
   
   scrxyz (float *): Pointer to hold screen corrds version of
                     NodeList before any sorting. Pass NULL
                     to have the function use its static array.
                     
*/
int * SUMA_DepthSort(float *NodeList, int N_Node, char **names, 
                     int xform_NodeList, float *uscrxyz)
{
   static char FuncName[]={"SUMA_DepthSort"};
   static float *scrxyz_loc=NULL;
   float *scrxyzR=NULL, *scrz=NULL, *scrxyz=NULL;
   static int N_alloc=-1;
   int ii, *isrt=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (N_Node == -1) { /* cleanup call */
      SUMA_ifree(scrxyz_loc); N_alloc = -1;
      SUMA_RETURN(NULL);
   }
   
   if (!uscrxyz) {
      if (!scrxyz_loc || N_alloc != N_Node) {
         SUMA_ifree(scrxyz_loc);
         if (!(scrxyz_loc = (float *)SUMA_malloc(3*N_Node *sizeof(float)))) {
            SUMA_S_Critv("Failed to allocate for %d node XYZ vals\n", N_Node);
            SUMA_RETURN(NULL);
         }
         N_alloc = N_Node;
      }
      scrxyz = scrxyz_loc;
   } else {
      scrxyz = uscrxyz;
   }
   
   SUMA_World2ScreenCoordsF(NULL, N_Node, NodeList, scrxyz, NULL, NOPE, YUP);
   if (xform_NodeList) memcpy(NodeList, scrxyz, 3*N_Node*sizeof(float));
   
   if (LocalHead) { /* slower way for debugging */
      if (!(scrz = (float *)SUMA_malloc(N_Node *sizeof(float)))) {
         SUMA_S_Critv("Failed to allocate for %d node XYZ vals\n", N_Node);
         SUMA_RETURN(NULL);
      }
      for (ii=0; ii<N_Node; ++ii) {
         scrz[ii] = -scrxyz[3*ii+2];
      }
      isrt = SUMA_z_qsort (scrz, N_Node);
   
      scrxyzR = SUMA_freorder_triplets(scrxyz, isrt, N_Node);

      fprintf(stderr,"\nSorted %d objects, farthest first, closest last\n",
             N_Node);
      for (ii=0; ii<N_Node; ++ii) {
         fprintf(stderr,"Obj. %d, name %s, scr coords [%f %f %f], "
                        "orig coords [%f %f %f]\n",
               isrt[ii], names?names[isrt[ii]]:"NULL", 
               scrxyzR[3*ii], scrxyzR[3*ii+1], scrxyzR[3*ii+2],
               NodeList[3*ii], NodeList[3*ii+1],NodeList[3*ii+2]);
      }
      SUMA_ifree(scrxyzR); 
      SUMA_ifree(scrz);
   } else {
      if (uscrxyz) {/* don't ruin the screen array */
         if (!(scrz = (float *)SUMA_malloc(N_Node *sizeof(float)))) {
            SUMA_S_Critv("Failed to allocate for %d node XYZ vals\n", N_Node);
            SUMA_RETURN(NULL);
         }
         for (ii=0; ii<N_Node; ++ii) {
            scrz[ii] = -scrxyz[3*ii+2];
         }
         isrt = SUMA_z_qsort (scrz, N_Node);
         SUMA_ifree(scrz);
      } else {
         /* reuse scrxyz, only 1/3 will be used*/
         for (ii=0; ii<N_Node; ++ii) {
            scrxyz[ii] = -scrxyz[3*ii+2];
         }
         isrt = SUMA_z_qsort (scrxyz, N_Node);
      }
   }
   
   
   SUMA_RETURN(isrt);
}  

/* Take normalized x,y screen corrdinates and turn them to world coordinates 
Based on code from SUMA_GetSelectionLine.
If you need to set GL_MODELVIEW_MATRIX set ApplyXform to 1 and make sure 
sv is not NULL (otherwise sv is not needed.) See SUMA_World2ScreenCoords()
for parallels.

if ApplyXform = 0 function ASSUMES GL_MODELVIEW_MATRIX is current.

GL_PROJECTION_MATRIX  is assumed set all the time.
*/

SUMA_Boolean SUMA_NormScreenToWorld(SUMA_SurfaceViewer *sv, 
                                    double xn, double yn, 
                                    GLdouble *pfront, GLdouble *pback,
                                    int ApplyXform)
{
   static char FuncName[]={"SUMA_NormScreenToWorld"};
   GLfloat rotationMatrix[4][4];
   GLdouble ox, oy;
   GLint viewport[4];
   GLdouble mvmatrix[16], projmatrix[16];
   
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
  
   if (ApplyXform) {
      if (!sv) SUMA_S_Err("Need sv with ApplyXform");
      SUMA_LH("Forcing setting of GL_MODELVIEW_MATRIX and GL_PROJECTION_MATRIX");
      /* go through the ModelView transforms as you would 
         in display since the modelview matrix is popped
         after each display call */
      SUMA_build_rotmatrix(rotationMatrix, sv->GVS[sv->StdView].currentQuat);
      glMatrixMode(GL_MODELVIEW);
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
   
   if (ApplyXform)  glPopMatrix();
   SUMA_RETURN(YUP);
}
/*!
   Purpose: Takes a the x,y positions of the cursor and sets 
            the Pick0 and Pick1 values (usually sv's) 
   \param sv (*SUMA_SurfaceViewer)
   \param x (int) mouse coordinate
   \param y (int) (y will get set to: viewport[3] - (GLint)y -1; 
                   inside of function)
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
      Also, if N_List <= 0, sv->PickPix is set before returning
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
   int isv=-1;
   char buf[32];
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
   /* BEFORE April 2013 
      The next line (which was glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix)) 
      appears to fix some bug with GL_MODELVIEW's matrix. 
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
      it should have not. 
         glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
      
      AFTER April 2013,
      The glGetDoublev is now replaced with the call to glXMakeCurrent
      I was having a similar first selection problem (the  GL_MODELVIEW)
      was getting screwed up as I clicked right after the surface controller
      notebook page was changed. For some reason, the context was no longer
      current or correct when the window manager had to manage widgets
      outside of the GLX area.   
      
      AFTER Nov 2013,
      Now calling SUMA_glXMakeCurrent with force context setting
   */  
      isv =  SUMA_WhichSV (sv, SUMAg_SVv, SUMAg_N_SVv);
      sprintf(buf,"GLXAREA of sv %d", isv); 
      if (!SUMA_glXMakeCurrent (sv->X->DPY, XtWindow(sv->X->GLXAREA), 
                           sv->X->GLXCONTEXT, FuncName, buf, 1)) {
               fprintf (SUMA_STDERR, 
                        "Error %s: Failed in SUMA_glXMakeCurrent.\n"
                        " \tContinuing ...\n", FuncName);
               SUMA_GL_ERRS;
      }
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
   
   if (N_List <=0 ) {
      /* store selection location in pixel indices into frame buffer */
      sv->PickPix[0] = (int)x; sv->PickPix[1]=(int)realy;
   }
   
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

   if (meth == 0) {
      static int nwarn=0;
      if (!nwarn) {
         SUMA_S_Warn("meth = 0 no longer allowed because of"
                     "troubles with X->gc, on macs.");
         ++nwarn;
      }
      meth = 1;
   }
   switch (meth) {
      case 0: /* does not work on OSX !!!*/
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
         SUMA_GLX_BUF_SWAP(sv);
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

void SUMA_cb_helpWeb (Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpWeb"};
   
   SUMA_ENTRY;
    whereami_browser("https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/SUMA/Viewer.html");
    //whereami_browser("https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/sumatoc1.html");

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
   sv = &(SUMAg_SVv[(INT_CAST)datap->ContID]);
   
   if (!sv->X->ViewCont->TopLevelShell) { /* see comments on similar section in 
                                             SUMA_cb_helpSurfaceStruct */
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
   SUMA_ALL_DO *ado = NULL;
   
   SUMA_ENTRY;
   
   datap = (SUMA_MenuCallBackData *)data;
   sv = &(SUMAg_SVv[(INT_CAST)datap->ContID]);
   if (!(ado = SUMA_SV_Focus_ADO(sv))) {
      SUMA_SLP_Err("No object in focus.\n");
      SUMA_RETURNe;
   }

   if (!SUMA_isADO_Cont_Realized(ado)) {
      SUMA_OpenCloseSurfaceCont(NULL, ado, NULL);
   }
   
   /* Now do the info thingy */
   SUMA_cb_moreSurfInfo (w, 
         (XtPointer)SUMA_Cont_ADO(SUMA_ADO_Cont(ado)), callData);

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
         all viewers are initialized to isShaded = NOPE, 
         even before they are ever opened */
         if (w != SUMAg_SVv[ii].X->HelpMenu->mw[SW_HelpIONotify]) {
            XmToggleButtonSetState (
               SUMAg_SVv[ii].X->HelpMenu->mw[SW_HelpIONotify], 
               SUMAg_CF->InOut_Notify, NOPE);
         }
      }
   }
   
    
   SUMA_RETURNe; 
}

void SUMA_setIO_notify(int val)
{
   static char FuncName[] = {"SUMA_setIO_notify"};
   int ii;
   
   SUMA_ENTRY;
   
   if (val) {SUMA_INOUT_NOTIFY_ON;}
   else { SUMA_INOUT_NOTIFY_OFF;} 
   
   /* must update the state of toggle buttons in otherviewers */
   for (ii=0; ii<SUMAg_N_SVv; ++ii) {
      if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
         /* you must check for both conditions because by default 
         all viewers are initialized to isShaded = NOPE, 
         even before they are ever opened */
         {
            XmToggleButtonSetState (
               SUMAg_SVv[ii].X->HelpMenu->mw[SW_HelpIONotify], 
               SUMAg_CF->InOut_Notify, NOPE);
         }
      }
   }
   
   SUMA_RETURNe;
}

/*!
 function to echo key presses that reach SUMA
 - expects nothing
*/  
void SUMA_cb_helpEchoKeyPress(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpEchoKeyPress"};
   int ii;
   
   SUMA_ENTRY;
   
   SUMA_ECHO_KEYPRESS_TOGGLE;
   
   /* must update the state of toggle buttons in otherviewers */
   for (ii=0; ii<SUMAg_N_SVv; ++ii) {
      if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
         /* you must check for both conditions because by default 
         all viewers are initialized to isShaded = NOPE, 
         even before they are ever opened */
         if (w != SUMAg_SVv[ii].X->HelpMenu->mw[SW_HelpEchoKeyPress]) {
            XmToggleButtonSetState (
               SUMAg_SVv[ii].X->HelpMenu->mw[SW_HelpEchoKeyPress], 
               SUMAg_CF->Echo_KeyPress, NOPE);
         }
      }
   }
   
    
   SUMA_RETURNe; 
}

void SUMA_setEcho_KeyPress(int val)
{
   static char FuncName[] = {"SUMA_setEcho_KeyPress"};
   int ii;
   
   SUMA_ENTRY;
   
   if (val) {SUMA_ECHO_KEYPRESS_ON;}
   else { SUMA_ECHO_KEYPRESS_OFF;} 
   
   /* must update the state of toggle buttons in otherviewers */
   for (ii=0; ii<SUMAg_N_SVv; ++ii) {
      if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
         /* you must check for both conditions because by default 
         all viewers are initialized to isShaded = NOPE, 
         even before they are ever opened */
         {
      XmToggleButtonSetState (
            SUMAg_SVv[ii].X->HelpMenu->mw[SW_HelpEchoKeyPress], 
               SUMAg_CF->Echo_KeyPress, NOPE);
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
         all viewers are initialized to isShaded = NOPE, 
         even before they are ever opened */
         XmToggleButtonSetState (
            SUMAg_SVv[ii].X->HelpMenu->mw[SW_HelpMemTrace], 
            SUMAg_CF->MemTrace, NOPE);
         if (SUMAg_CF->MemTrace) {
            /* can't turn it off */
             XtSetSensitive (SUMAg_SVv[ii].X->HelpMenu->mw[SW_HelpMemTrace], 0);
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
   
   SUMA_viewSumaCont(1);

   SUMA_RETURNe;
}

int SUMA_viewSumaCont(int flag)
{
   static char FuncName[] = {"SUMA_viewSumaCont"};
   Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (flag == 1) { /* want it visible */
      SUMA_LH("Visibilizing");
      if (!SUMAg_CF->X->SumaCont->AppShell) { /* create */
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: creating controller \n", FuncName);
         SUMA_cb_createSumaCont( NULL, NULL, NULL);
      }else {
         /* controller already created, need to bring it up again */
         switch (SUMA_CLOSE_MODE)   {/* No open GL drawables in this widget*/
            case SUMA_WITHDRAW:
               if (LocalHead) 
                  fprintf (SUMA_STDERR,
                           "%s: raising SUMA controller \n", FuncName);
               XMapRaised(SUMAg_CF->X->DPY_controller1, 
                           XtWindow(SUMAg_CF->X->SumaCont->AppShell));
               break;
            default:
               SUMA_S_Err("Not ready to deal with this closing mode");
               SUMA_RETURN(0);
               break;
         }
      }

      SUMA_PositionWindowRelative ( SUMAg_CF->X->SumaCont->AppShell, 
                                    SUMAg_SVv[0].X->TOPLEVEL, SWP_TOP_RIGHT);
   } else {
      /* want it minimized */
      SUMA_cb_closeSumaCont ( NULL, NULL, NULL);
   }
   
   SUMA_RETURN(1);
}


SUMA_Boolean SUMA_isSurfContWidgetCreated(SUMA_X_SurfCont  *SurfCont)
{
   static char FuncName[]={"SUMA_isSurfContWidgetCreated"};
   if (!SurfCont || !SurfCont->TLS) return(0);
   return(1);
}

/* Open the surface controller if other had been open already
   This is only for the single window mode */
int SUMA_OpenSurfCont_if_other(Widget w, 
                              SUMA_ALL_DO *ado, 
                              SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_OpenSurfCont_if_other"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->X->UseSameSurfCont) { SUMA_RETURN(1); /* only in same mode */ }
   if (!(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0); /* bad */
   if (!SUMAg_CF->X->SameSurfContOpen) { /* window not on screen, do nothing */
      SUMA_RETURN(0);
   }
   /* OK, bring it up if needed */
   if (SUMA_isADO_Cont_Realized(ado)) SUMA_RETURN(1); /* nothing to do */
   
   if (0 && w) {/* no need, using  SameSurfContOpen instead*/
      XWindowAttributes winattr;
      XGetWindowAttributes(XtDisplay(w), XtWindow(w), &winattr);
      SUMA_LHv("Window map_state: %d\n", winattr.map_state);
   }
   
   if (!SUMA_viewSurfaceCont(w, ado, sv)) {
      SUMA_S_Err("Failed to view surface cont");
      SUMA_RETURN(0);
   }

   SUMA_RETURN(1);
}

/* Many times you need the surface controller created but not for 
display, create it if needed, then minimize it immediately afterwards*/
int SUMA_OpenCloseSurfaceCont(Widget w, 
                              SUMA_ALL_DO *ado, 
                              SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_OpenCloseSurfaceCont"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   
   if (SUMA_isADO_Cont_Realized(ado)) SUMA_RETURN(1); /* nothing to do */
   
   if (w) {
      SUMA_LH("nism");
      SUMA_cb_createSurfaceCont( w, (XtPointer)ado, NULL);
   } else {
      if (!sv) {
         if (!(sv = SUMA_BestViewerForADO(ado)) ||
             !sv->X->TOPLEVEL) {
            SUMA_LH("NULLity");
            SUMA_RETURN(0);
         }
      }
      if (!SUMA_isADO_Cont_Created(ado)) {
        SUMA_LH("Creationism");
        SUMA_cb_createSurfaceCont( sv->X->TOPLEVEL, (XtPointer)ado, NULL); 
      } else {
        /* must have been closed, open it */
        if (!SUMA_viewSurfaceCont( sv->X->TOPLEVEL, ado, sv)) {
           SUMA_S_Err("Failed to open surf cont anew");
           SUMA_RETURN(0);
        }
      }
   }
   SUMA_LH("Initializing ColPaneShell");
   SUMA_InitializeColPlaneShell(ado, SUMA_ADO_CurColPlane(ado));


   /* Now close it quick. Maybe should put a delayed closing for nicer effect */
   if (!SUMAg_CF->X->UseSameSurfCont) { /* Don't minimize when using one surfcont
                                          it is more annoying than useful */
      SUMA_LH("Closism")
      #if 0 /* Not a good idea to do this because widgets get unrealized and
           The main reason for calling this function is to be sure that
           the widgets are alive and well for setting and queries ZSS Nov 9 2012 */
      SUMA_cb_closeSurfaceCont(NULL, (XtPointer) ado, NULL);
      #else
      XIconifyWindow(SUMAg_CF->X->DPY_controller1, XtWindow(SurfCont->TLS), 0);
      #endif
   }
   
   SUMA_LH("Returnism")
   SUMA_RETURN(1);
}

/*!
   Get a listing of all surfaces in the surface controller notebook pages
   You'll have to change the returned pointer someday to allow 
   for other objects that may have a page in Notebook... 
   
   I think this function needs to be updated to include tracts, volumes, and CIFTI datasets.
*/
SUMA_ALL_DO **SUMA_DOsInSurfContNotebook(Widget NB)
{
   static char FuncName[]={"SUMA_DOsInSurfContNotebook"};
   int i, lp, j, iso;
   static SUMA_ALL_DO *DOv[SUMA_MAX_SURF_ON_COMMAND];
   SUMA_ALL_DO *DOt=NULL, *curDO=NULL;
   XmNotebookPageStatus ns;
   XmNotebookPageInfo pi;
   char *allids = NULL;
   SUMA_X_SurfCont *SurfCont=NULL, *curSurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   iso=0;
   DOv[0]=NULL;
   if (!NB) SUMA_RETURN(DOv);
   
   XtVaGetValues(NB, XmNlastPageNumber, &lp, NULL);
   for (i=0; i<lp; ++i) {
      ns = XmNotebookGetPageInfo(NB, i+1, &pi);
      if (ns != XmPAGE_FOUND) {
         SUMA_LH("Could not find page!");
         SUMA_RETURN(DOv);
      }
      SUMA_LHv("Page %d, widget %p, page name=%s\n",
                   i+1, pi.page_widget,
                   XtName(pi.page_widget));
      
      for (j=0; j<SUMAg_N_DOv; ++j) {
         if (SUMAg_DOv[j].ObjectType == SO_type ||
             SUMAg_DOv[j].ObjectType == GDSET_type ||
             SUMAg_DOv[j].ObjectType == TRACT_type ||
             SUMAg_DOv[j].ObjectType == VO_type) {
            DOt = (SUMA_ALL_DO *)SUMAg_DOv[j].OP;
            SurfCont = SUMA_ADO_Cont(DOt);
            if (DOt && SurfCont) {
               curDO = SUMA_Cont_ADO(SurfCont);
               if (curDO) {
                  if (!allids || !strstr(allids, SUMA_ADO_idcode(curDO))) {
                     curSurfCont = SUMA_ADO_Cont(curDO);
                     if (pi.page_widget == curSurfCont->Page) {
                        if (iso < SUMA_MAX_SURF_ON_COMMAND-1) {
                           DOv[iso] = curDO; ++iso; DOv[iso]=NULL;
                           allids = SUMA_append_replace_string(allids,
                                                SUMA_ADO_idcode(curDO),"**",1);
                           SUMA_LHv("Got object %s\n", SUMA_ADO_Label(curDO));
                        } else {
                           SUMA_S_Errv("Too many surface controllers (%d)\n"
                                       "Will need to increase limit\n",
                                       iso);
                           if (allids) SUMA_free(allids); allids=NULL;
                           SUMA_RETURN(DOv);
                        }
                     }
                  } else {
                     /* surface sharing controller */
                     if (curDO != DOt) {
                        SUMA_LHv(
                       "Surface/DO %s should be sharing a controller with %s\n",
                           SUMA_ADO_Label(DOt), SUMA_ADO_Label(curDO));
                     }
                  }
               } else {
                  SUMA_LHv("Surface %s has no current DO set in its controller\n"
                        "This should mean its controller has never been open\n",
                         SUMA_ADO_Label(DOt));
               }
            } else {
               SUMA_LHv("Surface %s has no controller open yet\n", 
                        DOt?SUMA_ADO_Label(DOt):"NULL");
            }
         } else {
            switch(SUMAg_DOv[j].ObjectType) {
               case CDOM_type:
                  SUMA_LH("%s objects are no longer to get their own controller",
                    SUMA_ObjectTypeCode2ObjectTypeName(SUMAg_DOv[j].ObjectType));
                  break;
               default:
                  break;
            }
         }
      }
   }

   if (allids) SUMA_free(allids); allids=NULL;
   SUMA_RETURN(DOv);
}

SUMA_Boolean SUMA_MarkSurfContOpen(int Open, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_MarkSurfContOpen"};
   int i;
   SUMA_ALL_DO **DOv=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   
   SUMA_ENTRY;
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SUMAg_CF->X->SameSurfContOpen = Open;
      SurfCont = SUMA_ADO_Cont(ado);
      SurfCont->Open = Open;
      /* apply to all other surfaces */
      DOv = SUMA_DOsInSurfContNotebook(SUMAg_CF->X->SC_Notebook);      
      i = 0;
      while (DOv[i]) {
         SurfCont = SUMA_ADO_Cont(DOv[i]);
         SurfCont->Open=Open;
         ++i;
      }
   } else {
      if (ado) {
         SurfCont = SUMA_ADO_Cont(ado);
         SurfCont->Open = Open;
      }
   }
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_BringUpSurfContTLS(Widget TLS) 
{
   static char FuncName[]={"SUMA_BringUpSurfContTLS"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!TLS) SUMA_RETURN(NOPE);

   /* controller already created, need to bring it up again */
   switch (SUMA_GL_CLOSE_MODE)   {/* open GL drawables in this widget*/
      case SUMA_WITHDRAW:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Controller already created, Raising it.\n", 
                     FuncName);
         XMapRaised( SUMAg_CF->X->DPY_controller1, 
                     XtWindow(TLS));
         break;
      case SUMA_UNREALIZE:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Controller already created, realizing it.\n", 
                     FuncName);
         XtRealizeWidget(TLS);
         XSync(SUMAg_CF->X->DPY_controller1, 0);
         /* now do Raise, so that we can see the damned thing if it 
            behind other windows, and hope it does not crash*/
         SUMA_LH("Rise and do not crash");
         XMapRaised( SUMAg_CF->X->DPY_controller1, 
                     XtWindow(TLS)); 

         break;
      default:
         SUMA_S_Err("No setup for this close mode");
         SUMA_RETURN(NOPE);
         break;     
   }
   SUMA_RETURN(YUP);
}

 
/*! 
   if calling this function from outside interface, set w to NULL 
*/
int SUMA_viewSurfaceCont(Widget w, SUMA_ALL_DO *ado, 
                         SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_viewSurfaceCont"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) {
      SUMA_RETURN(0);
   }
   
   if (ado->do_type == CDOM_type) {
      SUMA_LH("I thought we decided to not have a separate "
              "controller for CIFTI_DO!");
      SUMA_RETURN(0);
   }
   if (!sv) {
      SUMA_LHv("Got to get me an sv for %s\n", SUMA_ADO_Label(ado));
      if (!(sv = SUMA_BestViewerForADO(ado))) {
         SUMA_RETURN(0);
      }
   }
   SUMA_LHv("Working object %s,\n sv %p,\n w  %p\n", 
            SUMA_ADO_Label(ado), w, sv);
      
   if (!SUMA_isADO_Cont_Created(ado)) {
      if (SUMAg_CF->X->UseSameSurfCont && 
          SUMAg_CF->X->CommonSurfContTLW && 
          !SUMAg_CF->X->SameSurfContOpen) {
         /* If you don't do this, then bad things will happen when:
         Launch SUMA with 2 types of objects (say one vol and one surf).
         Select the surf, open the surface controller. 
         Close the controller.
         Select the vol, open the surface controller and you'll get some 
         variant on:
         
         oo     Warning SUMA_XErrHandler (SUMA_display.c:3896):
         Intercepted X11 error: BadWindow (invalid Window parameter)
         Will attempt to proceed but trouble might ensue.
         ++     Notice SUMA_XErrHandler (SUMA_display.c:3897 @19:57:15):
         Trace At Xerr
                SUMA_XErrHandler
               SUMA_cb_createSurfaceCont_VO
              SUMA_cb_createSurfaceCont
             SUMA_viewSurfaceCont
            SUMA_cb_viewSurfaceCont
           suma
          Bottom of Debug Stack
         
         The error handler catches it and you can recover from the crash,
         however the close (x) button no longer works on that controller
         and that's just not cool. 
         The fix is in the call below. So all is well for now. Apr. 16 2014 */
         SUMA_LH("Must bring controller parent up first");
         SUMA_BringUpSurfContTLS(SUMAg_CF->X->CommonSurfContTLW);
      }
      
      if (LocalHead) 
         SUMA_LH("Calling SUMA_cb_createSurfaceCont.");
      if (w) SUMA_cb_createSurfaceCont( w, (XtPointer)ado, NULL);
      else SUMA_cb_createSurfaceCont( sv->X->TOPLEVEL, (XtPointer)ado, NULL);
   } else {
      if (!SUMA_BringUpSurfContTLS(SurfCont->TLS)) {
         SUMA_S_Err("Failed to raise the roof");
         SUMA_RETURN(NOPE);
      }
   }
   
   if (!SUMA_ADO_Cont(ado)) { /* This can happen when attempting to open a
                                 controller for an object which should have
                                 none. Return without kvetching */
      SUMA_RETURN(YUP);
   }
   
   SUMA_LH("Init Position %p %p", SurfCont->PosRef, sv->X->TOPLEVEL);
   if (SurfCont->PosRef != sv->X->TOPLEVEL) {
      SurfCont->PosRef = sv->X->TOPLEVEL;
      if (!SUMAg_CF->X->UseSameSurfCont ||
          SUMA_NotebookLastPageNumber(SUMAg_CF->X->SC_Notebook) == 1) { 
         /* Only reposition if not using same surf cont, or if
         using same surf cont but have only 1 page in it */
         SUMA_PositionWindowRelative ( SurfCont->TLS, 
                                       SurfCont->PosRef, SWP_TOP_BEST);    
      }
   }
   
   SUMA_MarkSurfContOpen(1,ado); 

   SUMA_LH("Init SurfParam");
   SUMA_Init_SurfCont_SurfParam(ado);
   SUMA_LH("Init CrossHair");
   SUMA_Init_SurfCont_CrossHair(ado);
   SUMA_LH("Dset goodies");
   SUMA_InitializeColPlaneShell(ado, SurfCont->curColPlane);
   
   
   SUMA_LH("Returning");
   
   /* insist on a glXMakeCurrent for surface viewer,
   Otherwise you get a crash on OS X 10.5 if
   you do the following:
      Open the surface controller with View-->Surface Controller
      Close the controller
      Open it again in the same way and then you get one of these:
         X Error of failed request:  GLXBadCurrentWindow
         Major opcode of failed request:  149 (GLX)
         Minor opcode of failed request:  11 (X_GLXSwapBuffers)
         Serial number of failed request:  7046
         Current serial number in output stream:  7046

      */
   SUMA_LH("Making sv's GLXAREA current\n");
   SUMA_SiSi_I_Insist();
   
   #if 0
      SUMA_S_Warn("Block Me!");
      /* Toying with automated saving of controllers to help automate help
         The problem with simple command below is that the OpenGL parts will
         not get captured with XGetImage. So object controllers do not show
         the colormap, and the viewer windows will not show the rendering.
         
         One approach might be to grab the openGL rendered pixels, and replace
         the GLX areas with a widget displaying the grabbed image. I don't know
         ahead of time how much of a pain it will be to be sure the newly added
         widget does not cause resizing problems with the interface. What a pain.
         
         See functions SUMA_Snap*Widgets, for automatic widget snapping. Still 
         need to write something to render colormaps a la PBAR_bigexpose_CB()
         which would then work for auto snapping of controllers.
      */   
         
      ISQ_snapfile ( SurfCont->Mainform );
   #endif
        
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
   SUMA_ALL_DO *ado;
   SUMA_SurfaceViewer *sv;
   int isv, widtype;
   static char FuncName[] = {"SUMA_cb_viewSurfaceCont"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_VIEWER_FROM_VIEWMENU_CALLBACK (data, isv, widtype);
   
   if (LocalHead) 
      fprintf (SUMA_STDERR,
               "%s: A call from viewer %d, widget %d.\n", 
               FuncName, isv, widtype);
   
   sv = &SUMAg_SVv[isv];
   if (sv->Focus_DO_ID >= 0) {
    ado = (SUMA_ALL_DO *)SUMAg_DOv[sv->Focus_DO_ID].OP;
   }else {
      fprintf (SUMA_STDERR,"%s: No displayable objects in focus.\n", FuncName);
      SUMA_RETURNe;
   }
   
   if (!SUMA_viewSurfaceCont(w, ado, sv)) {
      SUMA_S_Err("Failed in SUMA_viewSurfaceCont ADO %s", ADO_LABEL(ado));
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
      if (LocalHead) 
         fprintf (SUMA_STDERR,
                  "%s: Calling SUMA_cb_createViewerCont.\n", FuncName);
      SUMA_cb_createViewerCont( w, sv, callData);
   }else {
      /* controller already created, need to bring it up again */
      
      switch (SUMA_CLOSE_MODE)   {/* No open GL drawables in this widget*/
         case SUMA_WITHDRAW:
            if (LocalHead) 
               fprintf (SUMA_STDERR,
                        "%s: Controller already created, Raising it.\n", 
                        FuncName);
            XMapRaised(sv->X->DPY, XtWindow(sv->X->ViewCont->TopLevelShell));                 break;
         default:
            SUMA_S_Err("Not set to deal with this close mode");
            SUMA_RETURNe;
            break;
      }

   }
   
   SUMA_RETURNe;
}



/*!<
 the function expects the index of widget into sv->X->ViewMenu in data 
*/
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
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleCrossHair, 
                                       SES_SumaWidget, sv);
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
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleShowSelectedNode, 
                                       SES_SumaWidget, sv);
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_SumaWidget, sv);

   if (!SUMA_Engine (&list)) {
      fprintf(stderr,"Error %s: Failed SUMA_Engine\n", FuncName);
   }

   SUMA_RETURNe;
}

void SUMA_cb_toggle_selected_faceset(Widget w, XtPointer data, 
                                     XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_toggle_selected_faceset"};
   int isv, widtype;
   DList *list = NULL;
   SUMA_SurfaceViewer *sv;
   
   SUMA_ENTRY;
   
   SUMA_VIEWER_FROM_VIEWMENU_CALLBACK (data, isv, widtype);
   
   sv = &SUMAg_SVv[isv];
      
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleShowSelectedFaceSet, 
                                      SES_SumaWidget, sv);
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
   char slabel[100], *sss; 
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
   
   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }

   #if SUMA_CONTROLLER_AS_DIALOG 
      /*xmDialogShellWidgetClass, topLevelShellWidgetClass*/
   SUMA_LH("Create a popup");
   sv->X->ViewCont->TopLevelShell = XtVaCreatePopupShell (sss,
      xmDialogShellWidgetClass, tl,
      XmNallowShellResize, True, /* let code resize shell */
      XmNdeleteResponse, XmDO_NOTHING,
      XmNtitle, slabel,
      NULL);    
   #else
   SUMA_LH("Create an App");
   /** Feb 03/03: I was using XtVaCreatePopupShell to create a 
      topLevelShellWidgetClass. 
      XtVaCreatePopupShell is used to create dialog shells not 
      toplevel or appshells */
   sv->X->ViewCont->TopLevelShell = XtVaAppCreateShell (sss, "Suma",
      topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
      XmNdeleteResponse, XmDO_NOTHING,
      XmNtitle, slabel,
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
      SUMA_Register_Widget_Help(sv->X->ViewCont->ViewerInfo_pb , 1,
                           "ViewerCont->more",
                           "More info on Viewer", SUMA_moreViewerInfo_help);
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
      SUMA_Register_Widget_Help(pb , 1,
                           "ViewerCont->Switch->Group",
                           "Switch Group", "Switch Group");
      XtManageChild (pb);

      /* put a button for swiching states */
      pb = XtVaCreateWidget ("State", 
         xmPushButtonWidgetClass, rc, 
         NULL);
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ViewerCont_SwitchState, (XtPointer) sv);
      SUMA_Register_Widget_Help(pb , 1, 
                           "ViewerCont->Switch->State",
                           "Switch State", "Switch State");
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
      SUMA_Register_Widget_Help(pb_close , 1,
                           "ViewerCont->Close",
                           "Close Viewer controller", SUMA_closeViewerCont_help);
      XtManageChild (pb_close); 

      pb_bhelp = XtVaCreateWidget ("BHelp", 
         xmPushButtonWidgetClass, rc, 
         NULL);
      XtAddCallback (pb_bhelp, XmNactivateCallback, MCW_click_help_CB, NULL);
      SUMA_Register_Widget_Help(pb_bhelp , 1,
                           "ViewerCont->BHelp",
                           "Press this button then click on a "
                           "button/label/menu for more help.",
                           SUMA_help_help);
      XtManageChild (pb_bhelp); 


      pb_bhelp = XtVaCreateWidget ("WHelp", 
         xmPushButtonWidgetClass, rc, 
         NULL);
      XtAddCallback (pb_bhelp, XmNactivateCallback, 
                     SUMA_click_webhelp_CB, "ViewerCont->WHelp");
      SUMA_Register_Widget_Help(pb_bhelp , 1,
                           "ViewerCont->WHelp",
                           "Press this button then click on a "
                           "button/label/menu for online help.",
                           SUMA_webhelp_help);
      MCW_set_widget_bg( pb_bhelp , MCW_buthighlight(pb_bhelp) , 0 ) ;
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

   switch (SUMA_CLOSE_MODE)   {/* No open GL drawables in this widget*/
      case SUMA_WITHDRAW:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Withdrawing Viewer Controller...\n", FuncName);
         XWithdrawWindow(sv->X->DPY, 
            XtWindow(sv->X->ViewCont->TopLevelShell),
            XScreenNumberOfScreen(XtScreen(sv->X->ViewCont->TopLevelShell)));
         break;
      case SUMA_DESTROY:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Destroying Viewer Controller...\n", FuncName);
         XtDestroyWidget(sv->X->ViewCont->TopLevelShell);
         sv->X->ViewCont->TopLevelShell = NULL;
         break;
      default:
         SUMA_S_Err("Not ready to deal with this closing mode\n");
         break;
         SUMA_RETURNe;
   }

   SUMA_RETURNe;

}

/* 
   creates a managed widget with the common close, bhelp buttons 
*/
Widget SUMA_CloseBhelp_Frame( Widget parent,
                              XtCallbackProc close_callback, 
                              XtPointer close_data,
                              char *wname,
                              char *close_hint,
                              char *close_help,
                              XtCallbackProc help_callback,
                              XtPointer help_data,
                              char *help_hint,
                              char *help_help)
{
   static char FuncName[]={"SUMA_CloseBhelp_Frame"};
   Widget rc, pb_close, pb_bhelp, DispFrame, pb_help;
   char ss[64];
   
   SUMA_ENTRY;
      
   /* put up a frame to group the display controls */
   DispFrame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, parent,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNbottomAttachment  , XmATTACH_WIDGET ,
      XmNbottomWidget, parent,
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
   rc = XtVaCreateWidget ("rowcolumnCBF",
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
                  close_callback, close_data);
   snprintf(ss, 63, "%s->Close", wname);
   SUMA_Register_Widget_Help(pb_close , 1,
                        ss,
                        close_hint, close_help);
   XtManageChild (pb_close); 

   pb_bhelp = XtVaCreateWidget ("BHelp", 
      xmPushButtonWidgetClass, rc, 
      NULL);
   XtAddCallback (pb_bhelp, XmNactivateCallback, MCW_click_help_CB, NULL);
   snprintf(ss, 63, "%s->BHelp", wname);
   SUMA_Register_Widget_Help(pb_bhelp , 1,
                        ss,
                        "Press this button then click on a "
                        "button/label/menu for more help.", SUMA_help_help);
   XtManageChild (pb_bhelp); 

   pb_bhelp = XtVaCreateWidget ("WHelp", 
      xmPushButtonWidgetClass, rc, 
      NULL);
   snprintf(ss, 63, "%s->WHelp", wname);
   XtAddCallback (pb_bhelp, XmNactivateCallback, SUMA_click_webhelp_CB,
                  SUMA_copy_string(ss));
   MCW_set_widget_bg( pb_bhelp , MCW_buthighlight(pb_bhelp) , 0 ) ;
   SUMA_Register_Widget_Help(pb_bhelp , 1,
                        ss,
                        "Press this button then click on a "
                        "button/label/menu for online help.", SUMA_webhelp_help);

   XtManageChild (pb_bhelp); 


   if (help_callback) {
      XtVaCreateManagedWidget ("sep", 
                        xmSeparatorGadgetClass, rc, 
                        XmNorientation, XmVERTICAL,
                        NULL);
      pb_help = XtVaCreateWidget ("Help", 
         xmPushButtonWidgetClass, rc, 
         NULL);
      XtAddCallback (pb_help, XmNactivateCallback, help_callback, help_data);
      snprintf(ss, 63, "%s->Help", wname);
      SUMA_Register_Widget_Help(pb_help, 1,ss, help_hint ? 
            help_hint : "Press this button to get help about this interface",
                                             help_help ? 
            help_help : "Help about this interface" );                      
      XtManageChild (pb_help); 
   }

   /* now start managing the row column widget */
   XtManageChild (rc);

   /* manage the frame and the fslabelorm */
   XtManageChild (DispFrame);
   
   SUMA_RETURN(DispFrame);
}

void SUMA_cb_createSurfaceCont(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSurfaceCont"};
   Display *dpy;
   SUMA_ALL_DO *ado;
   char *slabel, *lbl30, *sss=NULL;
   XmString xmstmp; 
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL, *over0=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (!(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Errv("Failed to get Controller for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   SUMA_LH("Creating controller for %s", ADO_LABEL(ado));
   switch (ado->do_type) {
      case SO_type:
         SUMA_cb_createSurfaceCont_SO(w, data, callData);
         break; 
      case CDOM_type:
         SUMA_LH("No longer planning on separate controllers for CIFTI");
         #if 0
         SUMA_cb_createSurfaceCont_CO(w, (XtPointer)ado,  callData);
         #endif
         break;
      case GDSET_type:
         SUMA_S_Err("Cannot create a controller for a dataset"
                    "with no rendering variant");
         SUMA_RETURNe;
         break;
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_cb_createSurfaceCont_GLDO(w, (XtPointer)ado,  callData);
         break; }
      case TRACT_type: {
         SUMA_cb_createSurfaceCont_TDO(w, (XtPointer)ado,  callData);
         break; }
      case MASK_type: {
         SUMA_cb_createSurfaceCont_MDO(w, (XtPointer)ado,  callData);
         break; }
      case VO_type: {
         SUMA_cb_createSurfaceCont_VO(w, (XtPointer)ado,  callData);
         break; }      
      default:
         SUMA_S_Errv("No controller for type %d (%s)\n",
               ado->do_type, SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         SUMA_RETURNe;
         break;
   }

   SUMA_RETURNe;
}

SUMA_Boolean SUMA_WriteCont_Help(SUMA_DO_Types do_type, TFORM targ, char *fname)
{
   static char FuncName[]={"SUMA_WriteCont_Help"};
   FILE *fout=NULL;
   char *s=NULL;
   
   SUMA_ENTRY;
   
   if (!fname) {
      switch (targ) {
         case WEB:
         case NO_FORMAT:
         case TXT:
            fname = "SurfCont_help.txt";
            break;
         case ASPX:
         case SPX:
            fname = "SurfCont_help.rst";
            break;
         default:
            SUMA_S_Warn("Unknown format of %d. Going with .txt",targ);
            fname = "SurfCont_help.txt";
            break;
      }    
   }
   
   if (!(fout = fopen(fname,"w"))) {
      SUMA_S_Err("Failed to open help file %s in write mode", fname);
      SUMA_RETURN(NOPE);
   }
   
   switch(do_type) {
      case not_DO_type:
         s = SUMA_Help_AllSumaCont(targ);
         break;
      case SO_type:
         s = SUMA_Help_AllSurfCont(targ);
         break;
      case GRAPH_LINK_type:
         s = SUMA_Help_AllGraphCont(targ);
         break;
      case TRACT_type:
         s = SUMA_Help_AllTractCont(targ);
         break;
      case MASK_type:
         s = SUMA_Help_AllMaskCont(targ);
         break;
      case VO_type:
         s = SUMA_Help_AllVolCont(targ);
         break;
      case ROIdO_type:
         s = SUMA_Help_AllROICont(targ);
         break;
      default:
         SUMA_S_Err("Nothing for this controller");
         SUMA_RETURN(NOPE);
   }
       
   fprintf(fout,"%s\n", s);
   SUMA_ifree(s);
   fclose(fout); fout=NULL;
   
   SUMA_S_Note("Appended controller help to %s", fname); 
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_Snap_AllCont(SUMA_DO_Types do_type, char *fname)
{
   static char FuncName[]={"SUMA_Snap_AllCont"};
   FILE *fout=NULL;
   char *s=NULL;
   SUMA_ALL_DO *ado=NULL;
   
   SUMA_ENTRY;
   
   if (!fname) {
      fname = "SurfCont";
   }
   
   switch(do_type) {
      case not_DO_type:
         SUMA_Snap_AllSumaCont(fname);
         break;
      case ROIdO_type:
         SUMA_Snap_AllROICont(fname);
         break;
      case SO_type:
         SUMA_Snap_AllSurfCont(fname);
         break;
      case GRAPH_LINK_type:
         SUMA_Snap_AllGraphCont(fname);
         break;
      case TRACT_type:
         if ((ado = (SUMA_ALL_DO*)SUMA_findanyTDOp_inDOv(
                                       SUMAg_DOv, SUMAg_N_DOv,NULL))) {
            SUMA_cb_SurfCont_SwitchPage(ado);
         }
         SUMA_Snap_AllTractCont(fname);
         break;
      case MASK_type:
         if ((ado = (SUMA_ALL_DO*)SUMA_findanyMDOp_inDOv(
                                       SUMAg_DOv, SUMAg_N_DOv,NULL))) {
            SUMA_cb_SurfCont_SwitchPage(ado);
         }
         SUMA_Snap_AllMaskCont(fname);
         break;
      case VO_type:
         SUMA_Snap_AllVolCont(fname);
         break;
      default:
         SUMA_S_Err("Nothing for this controller");
         SUMA_RETURN(NOPE);
   }
       
   SUMA_S_Note("Saved widgets for controller to root %s", fname); 
   
   SUMA_RETURN(YUP);
}

/*!
   \brief SUMA_cb_createSurfaceCont_SO(Widget w, XtPointer data, 
                                    XtPointer callData);
   \param data (XtPointer) to SO (NOT sv)

*/
void SUMA_cb_createSurfaceCont_SO(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSurfaceCont_SO"};
   Widget tl, pb, form,
          rc_left, rc_right, rc_mamma, rc_gmamma, tls=NULL;
   Display *dpy;
   SUMA_ALL_DO *ado;
   char *slabel, *lbl30, *sss=NULL;
   XmString xmstmp; 
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL, *over0=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (!(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Errv("Failed to get Controller for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   if (!SUMA_SurfCont_SetcurDOp(SurfCont, ado)) {/* First set owner of GUI */
      SUMA_S_Err("Failed to set curDOp");
      SUMA_RETURNe;
   }
   if (!(curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_S_Errv("Failed to get current col plane for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   SUMA_LH("Cur Col Plane = %s", curColPlane->Name);
   if (SurfCont->TLS) {
      fprintf (SUMA_STDERR,
               "Error %s: SurfCont->TopLevelShell!=NULL.\n"
               "Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   
   tl = SUMA_GetTopShell(w); /* top level widget */
   dpy = XtDisplay(tl);
   
   slabel = (char *)SUMA_malloc(sizeof(char) * 
                                 (strlen(SUMA_ADO_Label(ado)) + 100));
   if (strlen(SUMA_ADO_Label(ado)) > 40) {
      char *tmpstr=NULL;
      tmpstr = SUMA_truncate_string(SUMA_ADO_Label(ado), 40);
      if (tmpstr) { 
         sprintf(slabel,"[%s] Surface Controller", tmpstr);
         free(tmpstr); tmpstr=NULL;
      }
   } else {
      sprintf(slabel,"[%s] Surface Controller", SUMA_ADO_Label(ado));
   }
   
   /* March 12 08: Made font8 default for surface controller */
   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }

   SUMA_LH("Creating dialog shell.");
   if (!SUMAg_CF->X->UseSameSurfCont || 
       !SUMAg_CF->X->CommonSurfContTLW) { /* need a new one */
      #if SUMA_CONTROLLER_AS_DIALOG /* xmDialogShellWidgetClass, 
                                       topLevelShellWidgetClass*/
      tls = XtVaCreatePopupShell (sss,
         XmNtitle, slabel,
         xmDialogShellWidgetClass, tl,
         XmNallowShellResize, True, /* let code resize shell */
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);    
      #else
      SUMA_LH("Creating toplevel shell.");
      /** Feb 03/03:    I was using XtVaCreatePopupShell to create a 
                        topLevelShellWidgetClass. 
                        XtVaCreatePopupShell is used to create dialog 
                        shells not toplevel or appshells. 
                        Of course, it made no difference! */
      tls = XtVaAppCreateShell (sss, "Suma",
         topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
         XmNtitle, slabel,
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);   
      #endif

      /* allow for code to resize the shell */
      XtVaSetValues (tls, 
            XmNresizePolicy , XmRESIZE_NONE , 
            XmNallowShellResize , True ,       /* let code resize shell */
            NULL);

      /* handle the close button from window manager */
      XmAddWMProtocolCallback(/* make "Close" window menu work */
         tls,
         XmInternAtom( dpy , "WM_DELETE_WINDOW" , False ) ,
         SUMA_cb_closeSurfaceCont, (XtPointer) ado) ;
      
      if (SUMAg_CF->X->UseSameSurfCont) {
         Widget scroller;
         SUMAg_CF->X->CommonSurfContTLW = tls;
         SUMAg_CF->X->SC_Notebook = 
            XtVaCreateWidget("ControllerBook", xmNotebookWidgetClass,
                             SUMAg_CF->X->CommonSurfContTLW, 
                             XmNbindingWidth, XmNONE,
                             XmNbackPageNumber, 0, 
                             XmNmajorTabSpacing, 0, 
                             XmNminorTabSpacing, 0, 
                             NULL);
            /*XmCreateNotebook (SUMAg_CF->X->CommonSurfContTLW, "ControllerBook",
                              NULL, 0);
              XtVaSetValues(SUMAg_CF->X->SC_Notebook,
                          XmNbindingWidth, XmNONE,
                          NULL); */
            
         
         /* Kill the scroller from hell otherwise no keyboard input
            gets to the baby widgets. Better write my own scroller
            if need be in the future */
         scroller = XtNameToWidget (SUMAg_CF->X->SC_Notebook, "PageScroller");
         XtUnmanageChild (scroller);
      }
      
   }
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SurfCont->TLS = SUMAg_CF->X->CommonSurfContTLW;
   } else {
      SurfCont->TLS = tls;
   }
   
   if (!SurfCont->TLS) {
      SUMA_S_Err("Bad logic");
      SUMA_RETURNe;
   }
   
   SUMA_LH("Widgets...");
   if (SUMAg_CF->X->UseSameSurfCont) {
      Arg args[20];
      /* add the page */
      XtSetArg (args[0], XmNnotebookChildType, XmPAGE);
      SurfCont->Page = 
         XmCreateRowColumn (SUMAg_CF->X->SC_Notebook,
                     SUMA_ADO_Label(ado)?SUMA_ADO_Label(ado):"page",
                                              args, 1);
   }
   
   /* create a form widget, manage it at the end ...*/
   SurfCont->Mainform = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SurfCont->Page ? 
                              SurfCont->Page:SurfCont->TLS,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   rc_gmamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Mainform,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);
            

   SurfCont->DispFrame = SUMA_CloseBhelp_Frame(rc_gmamma,
                     SUMA_cb_closeSurfaceCont, (XtPointer) ado,
                     "SurfCont", 
                     "Close Surface controller", SUMA_closeSurfaceCont_help,
                     NULL, NULL, NULL, NULL);
                     
   SUMA_Register_Widget_Help( SurfCont->DispFrame , 0,
                                 "SurfCont",
                                 "Surface Cont.",
"The surface controller is for controlling the way surfaces and datasets "
"defined over them are displayed. The same controller is shared by a "
":SPX::term:`family of surfaces <Family of surfaces>`:DEF:"
"family of surfaces:SPX:"
" and all the datasets displayed on them. Left and Right "
"surfaces have separate controllers though in most cases actions on one "
"hemisphere's controller are automatically mirrored on the contralateral "
"side. The surface controller is initialized by the currently selected "
"surface - the one said to be in focus.\n"
":SPX:"
"You can launch the :ref:`Surface Controller <SurfCont>` with:"
" :ref:`ctrl+s <LC_Ctrl+s>` or :menuselection:`View-->Object Controller`\n"
"\n"
".. figure:: media/SurfCont.auto.ALL.jpg\n"
"   :align: center\n"
"   :name: media/SurfCont.auto.ALL.jpg\n"
"\n"
"   :ref:`(link)<media/SurfCont.auto.ALL.jpg>`\n"
"\n\n"
"   ..\n\n"
":DEF:"
"You can launch the Surface Controller with:"
"\n'ctrl+s' or 'View-->Object Controller'\n"
":SPX:"
"\n") ;


   XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rc_gmamma , NULL);
   
   rc_mamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_gmamma,
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
      Widget rc, label, rc_SurfProp, pb, www;
     
      /* put a frame */
      SurfCont->SurfFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Surface Properties",
            xmLabelWidgetClass, SurfCont->SurfFrame, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "SurfCont->Surface_Properties",
                                 "Surface Properties",
            "Block providing information about selected surface."
            ":SPX:\n\n"
            ".. figure:: media/SurfCont.auto.Surface_Properties.jpg\n"
            "   :align: right\n"
            "   :name: media/SurfCont.auto.Surface_Properties.jpg\n"
            "\n"
            "   :ref:`(link)<media/SurfCont.auto.Surface_Properties.jpg>`\n"
            "   ..\n\n"
                  ":SPX:") ;

      rc_SurfProp = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->SurfFrame,
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
      lbl30 = SUMA_set_string_length(SUMA_ADO_Label(ado), ' ', 27);
      if (ado->do_type == SO_type) {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         if (lbl30) {
            sprintf( slabel,"%s\n%d nodes: %d tri.", 
                     lbl30, SO->N_Node, SO->N_FaceSet); 
            SUMA_free(lbl30); lbl30 = NULL;
         } else {
            sprintf(slabel,"???\n%d nodes: %d tri.", SO->N_Node, SO->N_FaceSet); 
         }
      } else {
         SUMA_S_Err("This function is only for SOs");
         SUMA_RETURNe;
      }
      xmstmp = XmStringCreateLtoR (slabel, XmSTRING_DEFAULT_CHARSET);
         /* XmStringCreateLocalized(slabel) does not reliably 
            handle newline characters q*/
      SurfCont->SurfInfo_label = XtVaCreateManagedWidget ("dingel-1", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
      XmStringFree (xmstmp);
      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, rc, 
                                 XmNorientation, XmVERTICAL, NULL );
      SUMA_Register_Widget_Help( SurfCont->SurfInfo_label, 1,
                                 "SurfCont->Surface_Properties->label",
                                 "Summary object information",
                                 "Summary object information" ) ;

      SurfCont->SurfInfo_pb = XtVaCreateWidget ("more", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (SurfCont->SurfInfo_pb, XmNactivateCallback, 
                     SUMA_cb_moreSurfInfo, 
                        (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont));
      XtVaSetValues (SurfCont->SurfInfo_pb, XmNuserData, 
                     (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont), NULL); /* 
            Feb 23 04: XmNuserData is not used anymore.
            See notes for SUMA_cb_moreViewerInfo
            call for reasons why this was done...

            store the surface object SO/DO in userData
            I think it is more convenient than as data
            in the call back structure. This way it will
            be easy to change the SO that this same button
            might refer to. 
            This is only for testing purposes, the pb_close
            button still expects SO in clientData*/
      SUMA_Register_Widget_Help( SurfCont->SurfInfo_pb , 1,
                                 "SurfCont->Surface_Properties->more",
                                 "More info on Surface",
                                 SUMA_SurfContHelp_more ) ;
      XtManageChild (SurfCont->SurfInfo_pb); 

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
      SurfCont->RenderModeMenu = 
            SUMA_Alloc_Menu_Widget(SW_N_SurfCont_Render);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                                 "Drw", '\0', YUP, RenderMode_Menu, 
                                 (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                                 "SurfCont->Surface_Properties->Drw",
                        "Choose the rendering (drawing) mode for this surface.",
                                 SUMA_SurfContHelp_RenderMode, 
                                 SurfCont->RenderModeMenu );
      XtManageChild (SurfCont->RenderModeMenu->mw[SW_SurfCont_Render]);

      SUMA_BuildMenuReset(0);
      SurfCont->TransModeMenu = 
            SUMA_Alloc_Menu_Widget(SW_N_SurfCont_Trans);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                                 "Trn", '\0', YUP, TransMode_Menu, 
                                 (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                                 "SurfCont->Surface_Properties->Trn",
                        "Choose the transparency for this surface.",
                                 SUMA_SurfContHelp_TransMode, 
                                 SurfCont->TransModeMenu );
      XtManageChild (SurfCont->TransModeMenu->mw[SW_SurfCont_Trans]);
      
      pb = XtVaCreateWidget ("Dsets", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ToggleManagementColPlaneWidget, 
                     (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont));
      SUMA_Register_Widget_Help( pb, 1, "SurfCont->Surface_Properties->Dsets", 
                                  "Show/Hide Dataset (previously Color Plane) "
                                  "controllers",
                                  SUMA_SurfContHelp_Dsets ) ;
      XtManageChild (pb);
      
      XtManageChild (rc);

      XtManageChild (rc_SurfProp);
      XtManageChild (SurfCont->SurfFrame);
   }  
   
   SUMA_LH("Xhair business");
   {  /* Xhair Controls */
      Widget rcv, www;
      /* put a frame */
      SurfCont->Xhair_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Xhair Info",
            xmLabelWidgetClass, SurfCont->Xhair_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "SurfCont->Xhair_Info",
                      "Crosshair Information",
                  ":SPX:\n\n"
                  ".. figure:: media/SurfCont.auto.Xhair_Info.jpg\n"
                  "   :align: right\n"
                  "   :name: media/SurfCont.auto.Xhair_Info.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/SurfCont.auto.Xhair_Info.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Xhair_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateXhairWidgets(rcv, ado);

      
      XtManageChild(rcv);
      XtManageChild(SurfCont->Xhair_fr);
   }  /* Xhair Controls */

   SUMA_LHv("\nDset Mapping, surface %s\n", SUMA_ADO_Label(ado));
   {  /* Dset Mapping */
      Widget rcv, www;
      /* put a frame */
      SurfCont->DsetMap_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_right,
         XmNrightAttachment , XmATTACH_FORM ,
         XmNleftAttachment, XmATTACH_WIDGET,
         XmNleftWidget, SurfCont->SurfFrame,
         XmNtopAttachment  , XmATTACH_FORM ,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Dset Mapping",
            xmLabelWidgetClass, SurfCont->DsetMap_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "SurfCont->Dset_Mapping",
                                 "Dset Color Mapping",
                  ":SPX:\n\n"
                  ".. figure:: media/SurfCont.auto.Dset_Mapping.jpg\n"
                  "   :align: right\n"
                  "   :name: media/SurfCont.auto.Dset_Mapping.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/SurfCont.auto.Dset_Mapping.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
      
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->DsetMap_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateCmapWidgets(rcv, ado);

      XtManageChild(rcv);

      XtManageChild(SurfCont->DsetMap_fr);
   }

   SUMA_LH("Dset Controls");
   /* Dset Controls */
   {
       Widget rc, rcv, pb, www;
     
      
      /* put a frame */
      SurfCont->ColPlane_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Dset Controls",
            xmLabelWidgetClass, SurfCont->ColPlane_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "SurfCont->Dset_Controls",
                                 "Dset Controls", 
                  ":SPX:\n\n"
                  ".. figure:: media/SurfCont.auto.Dset_Controls.jpg\n"
                  "   :align: right\n"
                  "   :name: media/SurfCont.auto.Dset_Controls.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/SurfCont.auto.Dset_Controls.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->ColPlane_fr,
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
            "SurfCont->Dset_Controls->Lbl+Par",
            Dset_tit, NULL,
            Dset_hint, NULL,
            Dset_help, NULL,
            colw, NOPE, SUMA_string,
            NULL, NULL,
            NULL, NULL,
            NULL, NULL, 
            SurfCont->ColPlaneLabelTable);
      }
      XtManageChild (rc);
      
      /* add a rc for the colorplane order and opacity */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
   
      SUMA_CreateArrowField ( rc, "Ord",
                           1, 0, 20, 1,
                           2, SUMA_int,
                           NOPE,
                           SUMA_cb_ColPlane_NewOrder, (void *)ado,
                           "SurfCont->Dset_Controls->Ord",
                           SUMA_SurfCont_ColPlaneOrder_hint, 
                           SUMA_SurfContHelp_DsetOrd,
                           SurfCont->ColPlaneOrder);
                             
      SUMA_CreateArrowField ( rc, "Opa",
                           1, 0.0, 1.0, 0.1,
                           3, SUMA_float,
                           NOPE,
                           SUMA_cb_ColPlane_NewOpacity, (void *)ado,
                           "SurfCont->Dset_Controls->Opa",
                           SUMA_SurfCont_ColPlaneOpacity_hint,
                           SUMA_SurfContHelp_DsetOpa,
                           SurfCont->ColPlaneOpacity);

      /* manage  rc */
      XtManageChild (rc);
      
      /* add a rc for the colorplane brightness factor and visibility */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
   
      SUMA_CreateArrowField ( rc, "Dim",
                           1, 0.1, 2, 0.1,
                           3, SUMA_float,
                           YUP,
                           SUMA_cb_ColPlane_NewDimFact, (void *)ado, 
                           "SurfCont->Dset_Controls->Dim",
                           SUMA_SurfCont_ColPlaneDim_hint, 
                           SUMA_SurfContHelp_DsetDim,
                           SurfCont->ColPlaneDimFact);
           
      #if 0
      SurfCont->ColPlaneShow_tb = XtVaCreateManagedWidget("view", 
            xmToggleButtonWidgetClass, rc, NULL);
      XmToggleButtonSetState (SurfCont->ColPlaneShow_tb, YUP, NOPE);
      XtAddCallback (SurfCont->ColPlaneShow_tb, 
                  XmNvalueChangedCallback, SUMA_cb_ColPlaneShow_toggled, ado);
      SUMA_Register_Widget_Help(SurfCont->ColPlaneShow_tb , 1,
                                "SurfCont->Dset_Controls->view",
                                "Shows/Hides Dset.",
                                SUMA_SurfContHelp_DsetView ) ;
      SUMA_SET_SELECT_COLOR(SurfCont->ColPlaneShow_tb);
     #else
      SUMA_BuildMenuReset(0);
      SurfCont->DsetViewModeMenu = 
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetView);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                      "Dsp", '\0', YUP, DsetViewMode_Menu, 
                      (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                      "SurfCont->Dset_Controls->Dsp",
                      "Choose the rendering mode for this dataset.",
                      SUMA_SurfContHelp_DsetViewMode, 
                      SurfCont->DsetViewModeMenu );
      XtManageChild (SurfCont->DsetViewModeMenu->mw[SW_SurfCont_DsetView]);
      SUMA_Set_Menu_Widget(SurfCont->DsetViewModeMenu,
                    SUMA_ShowMode2ShowModeMenuItem(
                                 curColPlane->ShowMode));

      #endif            
      SurfCont->ColPlaneShowOneFore_tb = 
         XtVaCreateManagedWidget("1", 
                                 xmToggleButtonWidgetClass, rc, NULL);
      XmToggleButtonSetState (SurfCont->ColPlaneShowOneFore_tb, 
                              SurfCont->ShowCurForeOnly, NOPE);
      XtAddCallback (SurfCont->ColPlaneShowOneFore_tb, 
                     XmNvalueChangedCallback, 
                     SUMA_cb_ColPlaneShowOneFore_toggled, ado);
                  
      SUMA_Register_Widget_Help(SurfCont->ColPlaneShowOneFore_tb , 1,
                                "SurfCont->Dset_Controls->1",
             "Show ONLY ONE selected Dset. Foreground only. (BHelp for more)",
                                 SUMA_SurfContHelp_DsetViewOne ) ;
      SUMA_SET_SELECT_COLOR(SurfCont->ColPlaneShowOneFore_tb);
           
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
      SurfCont->SwitchDsetlst = 
            SUMA_AllocateScrolledList ("Switch Dset", 
                     SUMA_LSP_SINGLE, 
                     NOPE, NOPE, /* duplicate deletion, no sorting */ 
                     SurfCont->TLS, SWP_POINTER,
                     125,
                     SUMA_cb_SelectSwitchColPlane, (void *)ado,
                     SUMA_cb_SelectSwitchColPlane, (void *)ado,
                     SUMA_cb_CloseSwitchColPlane, NULL);


      pb = XtVaCreateWidget ("Switch Dset", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_SurfCont_SwitchColPlane, (XtPointer)ado);
      SUMA_Register_Widget_Help(pb, 1, "SurfCont->Dset_Controls->Switch_Dset",
                                "Switch between datasets", 
                                SUMA_SurfContHelp_DsetSwitch ) ;
      XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Load Dset", 
            xmPushButtonWidgetClass, rc, 
            NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_Dset_Load, (XtPointer) ado);
         SUMA_Register_Widget_Help(pb, 1, "SurfCont->Dset_Controls->Load_Dset",
                                   "Load a new dataset (much more with BHelp)",
                                   SUMA_SurfContHelp_DsetLoad ) ;
         XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Delete", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ColPlane_Delete, (XtPointer) ado);
      /* XtManageChild (pb); */ /* Not ready for this one yet */

      pb = XtVaCreateWidget ("Load Col", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ColPlane_Load, (XtPointer) ado);
      SUMA_Register_Widget_Help(pb, 1, "SurfCont->Dset_Controls->Load_Col",
                                "Load a new color plane (more with BHelp)",
                                SUMA_SurfContHelp_DsetLoadCol) ;
      XtManageChild (pb);
      
       
      XtManageChild (rc);
      /* manage vertical row column */
      XtManageChild (rcv);
      
      XtManageChild (SurfCont->ColPlane_fr);
   }
   
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      Widget rc=NULL;
      /* put something to cycle through objects */
      if ((rc = SUMA_FindChildWidgetNamed(SurfCont->DispFrame, "rowcolumnCBF"))){
         XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc, 
                              XmNorientation, XmVERTICAL,NULL);
         
         pb = XtVaCreateWidget ("All Objs.", 
                  xmPushButtonWidgetClass, rc, 
                  NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_AllConts, NULL);
         SUMA_Register_Widget_Help(pb, 1, "SurfCont->Disp_Cont->AllObjs",
                                "Initialize Controllers for All Objects",
                                SUMA_SurfContHelp_AllObjs) ;
         XtManageChild (pb);
         SUMA_CreateArrowField ( rc, "Switch",
                           1, 1, 20, 1,
                           2, SUMA_int,
                           YUP,
                           SUMA_cb_SurfCont_SwitchPage, (void *)ado,
                           "SurfCont->Disp_Cont->Switch",
                           "Switch to other object controller", 
                           SUMA_Switch_Cont_BHelp,
                           SurfCont->SurfContPage);
         xmstmp = XmStringCreateLtoR (SUMA_ADO_CropLabel(ado, 
                                          SUMA_SURF_CONT_SWITCH_LABEL_LENGTH), 
                                      XmSTRING_DEFAULT_CHARSET);
         SurfCont->SurfContPage_label = XtVaCreateManagedWidget ("dingel-2", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
         XmStringFree (xmstmp);
      }
   }
   SUMA_LHv("Management ...%p %p %p %p %p\n",
            rc_right, rc_left, rc_mamma, SurfCont->Mainform, SurfCont->Page);

   XtManageChild (rc_right);
   XtManageChild (rc_left);
   XtManageChild (rc_mamma);
   XtManageChild (rc_gmamma);
   XtManageChild (SurfCont->Mainform);
   if (SUMAg_CF->X->UseSameSurfCont) XtManageChild (SurfCont->Page);
   
   #if SUMA_CONTROLLER_AS_DIALOG    
   #else
   /** Feb 03/03: pop it up if it is a topLevelShellWidgetClass, 
   you should do the popping after all the widgets have been created.
   Otherwise, the window does not size itself correctly when open */
   XtPopup(SurfCont->TLS, XtGrabNone);
   #endif
   
   /* realize the widget */
   if (SUMAg_CF->X->UseSameSurfCont) XtManageChild (SUMAg_CF->X->SC_Notebook);
   XtRealizeWidget (SurfCont->TLS);
   
   SUMA_LH("%s",slabel);
   SUMA_free (slabel);

   /* Mark as open */
   SUMA_MarkSurfContOpen(1,ado);
   
   SUMA_LHv("Marked %s's controller as open.\n", SUMA_ADO_Label(ado));
   
   /* initialize the left side 
      (no need here, that's done in SUMA_cb_viewSurfaceCont)*/
   /* SUMA_Init_SurfCont_SurfParam(ado); */
   
   /* initialize the ColorPlane frame if possible 
   Do it here rather than above because scale goes crazy 
   when parent widgets are being resized*/
   
   if (!(over0 = SUMA_ADO_Overlay0(ado))) {
      SUMA_SurfaceObject *SOp=NULL, *SO=(SUMA_SurfaceObject *)ado;
      SUMA_LH("NO Overlays yet for this surface\n");
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
   
   if (SUMA_ADO_N_Overlays(ado)>0) {
      SUMA_LH("Initializing ColPlaneShell");
      SUMA_InitializeColPlaneShell(ado, curColPlane); /* Used to use Over0, 
                              But that is not good when Over0 is not
                              the current color Plane. This
                              function no longer gets down to here without
                           cutColPlane, so we should be good to go May 4th 2015*/
      #ifdef NONONO /* It looks like I do not need this anymore 
                       Used to be #idef DARWIN */
      /* Sometimes color maps do not show up on mac when you first 
         open the surface controller */
      SUMA_LH("Darwining");
      SUMA_SwitchColPlaneCmap(ado, SUMA_CmapOfPlane(curColPlane));
      #endif
   }

   #if USING_LESSTIF
   SUMA_LH("Less tif fix");
   /* A quick fix to ensure Dset_Mapping 
      gets displayed properly the first time */
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   #endif

   SUMA_LH("going home.");

   SUMA_MarkSurfContOpen(1, ado);

   SUMA_RETURNe;
}

void SUMA_cb_createSurfaceCont_GLDO(Widget w, XtPointer data, 
                                     XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSurfaceCont_GLDO"};
   Widget tl, pb, form, 
          rc_left, rc_right, rc_mamma, rc_gmamma, tls=NULL;
   Display *dpy;
   SUMA_ALL_DO *ado;
   char *slabel, *lbl30, *sss=NULL;
   XmString xmstmp; 
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL, *over0=NULL;
   SUMA_GRAPH_SAUX *GSaux = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (ado->do_type != GRAPH_LINK_type) {
      SUMA_S_Errv("Calling me with (%s) other than SDSET_GRAPH_Link type,\n" 
                  "I don't like that, call me with GLDO",
                  SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
      SUMA_RETURNe;
   }
   SUMA_LHv("Creating SurfaceCont for type %s, label %s\n",
            ADO_TNAME(ado), SUMA_ADO_Label(ado));
   if (LocalHead) {
      SUMA_DUMP_TRACE("Creating SurfaceCont");
   }
   if (!(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Errv("Failed to get Controller for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   if (!(GSaux = SUMA_ADO_GSaux(ado))) {
      SUMA_S_Errv("No GSaux for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   if (!SUMA_SurfCont_SetcurDOp(SurfCont, ado)) {
      SUMA_S_Errv("Failed to Set curDOp for %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   if (!(curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_S_Errv("Failed to get current col plane for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   if (SurfCont->TLS) {
      fprintf (SUMA_STDERR,
               "Error %s: SurfCont->TopLevelShell!=NULL.\n"
               "Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   
   tl = SUMA_GetTopShell(w); /* top level widget */
   dpy = XtDisplay(tl);
   
   slabel = (char *)SUMA_malloc(sizeof(char) * 
                                 (strlen(SUMA_ADO_Label(ado)) + 100));
   if (strlen(SUMA_ADO_Label(ado)) > 40) {
      char *tmpstr=NULL;
      tmpstr = SUMA_truncate_string(SUMA_ADO_Label(ado), 40);
      if (tmpstr) { 
         sprintf(slabel,"[%s] %s Dset Controller", 
                        tmpstr, SUMA_ADO_ContName(ado));
         free(tmpstr); tmpstr=NULL;
      }
   } else {
      sprintf(slabel,"[%s] %s Dset Controller", 
                     SUMA_ADO_Label(ado), SUMA_ADO_ContName(ado));
   }
   
   /* March 12 08: Made font8 default for surface controller */
   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }

   SUMA_LH("Creating dialog shell.");
   if (!SUMAg_CF->X->UseSameSurfCont || 
       !SUMAg_CF->X->CommonSurfContTLW) { /* need a new one */
      #if SUMA_CONTROLLER_AS_DIALOG /* xmDialogShellWidgetClass, 
                                       topLevelShellWidgetClass*/
      tls = XtVaCreatePopupShell (sss,
         XmNtitle, slabel,
         xmDialogShellWidgetClass, tl,
         XmNallowShellResize, True, /* let code resize shell */
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);    
      #else
      SUMA_LH("Creating toplevel shell.");
      /** Feb 03/03:    I was using XtVaCreatePopupShell to create a 
                        topLevelShellWidgetClass. 
                        XtVaCreatePopupShell is used to create dialog 
                        shells not toplevel or appshells. 
                        Of course, it made no difference! */
      tls = XtVaAppCreateShell (sss, "Suma",
         topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
         XmNtitle, slabel,
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);   
      #endif

      /* allow for code to resize the shell */
      XtVaSetValues (tls, 
            XmNresizePolicy , XmRESIZE_NONE , 
            XmNallowShellResize , True ,       /* let code resize shell */
            NULL);

      /* handle the close button from window manager */
      XmAddWMProtocolCallback(/* make "Close" window menu work */
         tls,
         XmInternAtom( dpy , "WM_DELETE_WINDOW" , False ) ,
         SUMA_cb_closeSurfaceCont, (XtPointer) ado) ;
      
      if (SUMAg_CF->X->UseSameSurfCont) {
         Widget scroller;
         SUMAg_CF->X->CommonSurfContTLW = tls;
         SUMAg_CF->X->SC_Notebook = 
            XtVaCreateWidget("ControllerBook", xmNotebookWidgetClass,
                             SUMAg_CF->X->CommonSurfContTLW, 
                             XmNbindingWidth, XmNONE,
                             XmNbackPageNumber, 0, 
                             XmNmajorTabSpacing, 0, 
                             XmNminorTabSpacing, 0, 
                             NULL);
            /*XmCreateNotebook (SUMAg_CF->X->CommonSurfContTLW, "ControllerBook",
                              NULL, 0);
              XtVaSetValues(SUMAg_CF->X->SC_Notebook,
                          XmNbindingWidth, XmNONE,
                          NULL); */
            
         
         /* Kill the scroller from hell otherwise no keyboard input
            gets to the baby widgets. Better write my own scroller
            if need be in the future */
         scroller = XtNameToWidget (SUMAg_CF->X->SC_Notebook, "PageScroller");
         XtUnmanageChild (scroller);
      }
   }
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SurfCont->TLS = SUMAg_CF->X->CommonSurfContTLW;
   } else {
      SurfCont->TLS = tls;
   }
   
   if (!SurfCont->TLS) {
      SUMA_S_Err("Bad logic");
      SUMA_RETURNe;
   }
   
   SUMA_LH("Widgets...");
   if (SUMAg_CF->X->UseSameSurfCont) {
      Arg args[20];
      /* add the page */
      XtSetArg (args[0], XmNnotebookChildType, XmPAGE);
      SurfCont->Page = 
         XmCreateRowColumn (SUMAg_CF->X->SC_Notebook,
                     SUMA_ADO_Label(ado)?SUMA_ADO_Label(ado):"page",
                                              args, 1);
   }
   
   /* create a form widget, manage it at the end ...*/
   SurfCont->Mainform = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SurfCont->Page ? 
                              SurfCont->Page:SurfCont->TLS,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   rc_gmamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Mainform,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNbottomAttachment  , XmATTACH_FORM ,
            NULL);
   

   SurfCont->DispFrame = SUMA_CloseBhelp_Frame(rc_gmamma,
                     SUMA_cb_closeSurfaceCont, (XtPointer) ado,
                     "GraphCont",
                     "Close Graph controller", SUMA_closeSurfaceCont_help,
                     NULL, NULL, NULL, NULL);
   
   SUMA_Register_Widget_Help( SurfCont->DispFrame , 0,
                              "GraphCont",
                              "Graph Cont.",
"The graph controller is for controlling the way graphs (matrices) are rendered.  Each graph gets its own controller. You can use the switch button above to switch between them. The graph controller is initialized by the graph of the last selected edge/cell.\n After you have selected an edge, "
":SPX:"
"you can launch the :ref:`Graph Controller <VolCont>` with:"
" :ref:`ctrl+s <LC_Ctrl+s>` or :menuselection:`View-->Object Controller`\n"
"\n"
".. figure:: media/GraphCont.auto.ALL.jpg\n"
"   :align: center\n"
"   :name: media/GraphCont.auto.ALL.jpg\n"
"\n"
"   :ref:`(link)<media/GraphCont.auto.ALL.jpg>`\n"
"\n\n"
"   ..\n\n"
":DEF:"
"you can launch the Graph Controller with:"
"\n'ctrl+s' or 'View-->Object Controller'\n"
":SPX:"
"\n") ;

   XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rc_gmamma , NULL);
         
   rc_mamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_gmamma,
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
      Widget rc, label, rc_SurfProp, pb, www;
     
      /* put a frame */
      SurfCont->SurfFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Graph Dset Properties",
            xmLabelWidgetClass, SurfCont->SurfFrame, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                              "GraphCont->Graph_Dset_Properties",
                              "Properties of graph dset",
            ":SPX:\n\n"
            ".. figure:: media/GraphCont.auto.Graph_Dset_Properties.jpg\n"
            "   :align: right\n"
            "   :name: media/GraphCont.auto.Graph_Dset_Properties.jpg\n"
            "\n"
            "   :ref:`(link)<media/GraphCont.auto.Graph_Dset_Properties.jpg>`\n"
            "   ..\n\n"
            ":SPX:") ;
      
      rc_SurfProp = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->SurfFrame,
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
      lbl30 = SUMA_set_string_length(
            SDSET_LABEL(SUMA_find_GLDO_Dset((SUMA_GraphLinkDO*)ado)), ' ', 27);
      if (lbl30) { 
         sprintf(slabel,"%s \n%d edges, variant %s.",
                     lbl30, SUMA_ADO_N_Datum(ado), SUMA_ADO_variant(ado)); 
         SUMA_free(lbl30); lbl30 = NULL;
      } else {
         sprintf(slabel,"???\n%d edges.",SUMA_ADO_N_Datum(ado)); 
      }
      
      xmstmp = XmStringCreateLtoR (slabel, XmSTRING_DEFAULT_CHARSET);
         /* XmStringCreateLocalized(slabel) does not reliably 
            handle newline characters q*/
      SurfCont->SurfInfo_label = XtVaCreateManagedWidget ("dingel-3", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
      XmStringFree (xmstmp);
      SUMA_Register_Widget_Help( SurfCont->SurfInfo_label, 1,
                                 "GraphCont->Graph_Dset_Properties->label",
                                 "Summary object information",
                                 "Summary object information" ) ;

      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, rc, 
                                 XmNorientation, XmVERTICAL, NULL );

      SurfCont->SurfInfo_pb = XtVaCreateWidget ("more", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (SurfCont->SurfInfo_pb, XmNactivateCallback, 
                     SUMA_cb_moreSurfInfo, 
                        (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont));
      XtVaSetValues (SurfCont->SurfInfo_pb, XmNuserData, 
                     (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont), NULL); 

      SUMA_Register_Widget_Help( SurfCont->SurfInfo_pb , 1,
                                 "GraphCont->Graph_Dset_Properties->more",
                                 "More info on Graph Dset" , 
                                 SUMA_SurfContHelp_more ) ;
      XtManageChild (SurfCont->SurfInfo_pb); 

      XtManageChild (rc);
      
      #if 0 /* This section is for widgets that have no use yet 
               for graph dsets. If they don't get revived soon,
               kill them all in the future.     May 2013 */
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
      SurfCont->RenderModeMenu = 
            SUMA_Alloc_Menu_Widget(SW_N_SurfCont_Render);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                                 "Drw", '\0', YUP, RenderMode_Menu, 
                                 (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                                 "GraphCont->NOT_USED_YET->Drw",
                        "Choose the rendering (drawing) mode for this graph.",
                                 SUMA_SurfContHelp_RenderMode, 
                                 SurfCont->RenderModeMenu );
      XtManageChild (SurfCont->RenderModeMenu->mw[SW_SurfCont_Render]);

      SUMA_BuildMenuReset(0);
      SurfCont->TransModeMenu = 
            SUMA_Alloc_Menu_Widget(SW_N_SurfCont_Trans);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                                 "Trn", '\0', YUP, TransMode_Menu, 
                                 (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                                 "GraphCont->NOT_USED_YET->Trn",
                        "Choose the transparency for this graph.",
                                 SUMA_SurfContHelp_TransMode, 
                                 SurfCont->TransModeMenu );
      XtManageChild (SurfCont->TransModeMenu->mw[SW_SurfCont_Trans]);
      
      pb = XtVaCreateWidget ("Dsets", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ToggleManagementColPlaneWidget, 
                     (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont));
      SUMA_Register_Widget_Help( pb, 1,
                                 "GraphCont->NOT_USED_YET->Dsets",
                                 "Show/Hide Dataset (previously Color Plane) "
                                 "controllers", 
                                 SUMA_SurfContHelp_Dsets ) ;
      XtManageChild (pb);
      
      XtManageChild (rc);
      #endif
      
      XtManageChild (rc_SurfProp);
      XtManageChild (SurfCont->SurfFrame);
   }  
   
   SUMA_LH("Xhair business");
   {  /* Xhair Controls */
      Widget rcv, www;
      /* put a frame */
      SurfCont->Xhair_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Xhair Info",
            xmLabelWidgetClass, SurfCont->Xhair_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "GraphCont->Xhair_Info",
                      "Crosshair Information",
                  ":SPX:\n\n"
                  ".. figure:: media/GraphCont.auto.Xhair_Info.jpg\n"
                  "   :align: right\n"
                  "   :name: media/GraphCont.auto.Xhair_Info.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/GraphCont.auto.Xhair_Info.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Xhair_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateXhairWidgets(rcv, ado);
      
      XtManageChild(rcv);
      XtManageChild(SurfCont->Xhair_fr);
   }  /* Xhair Controls */

   SUMA_LHv("\nGDset Mapping, graph %s\n", SUMA_ADO_Label(ado));
   {  /* Dset Mapping */
      Widget rcv, www;
      /* put a frame */
      SurfCont->DsetMap_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_right,
         XmNrightAttachment , XmATTACH_FORM ,
         XmNleftAttachment, XmATTACH_WIDGET,
         XmNleftWidget, SurfCont->SurfFrame,
         XmNtopAttachment  , XmATTACH_FORM ,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("GDset Mapping",
            xmLabelWidgetClass, SurfCont->DsetMap_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                              "GraphCont->GDset_Mapping",
                              "Control mapping of edge/cell values to color map",
                  ":SPX:\n\n"
                  ".. figure:: media/GraphCont.auto.GDset_Mapping.jpg\n"
                  "   :align: right\n"
                  "   :name: media/GraphCont.auto.GDset_Mapping.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/GraphCont.auto.GDset_Mapping.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
       
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->DsetMap_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateCmapWidgets(rcv, ado);

      XtManageChild(rcv);

      XtManageChild(SurfCont->DsetMap_fr);
   }

   SUMA_LH("GDset Controls");
   /* Dset Controls */
   {
       Widget rc, rcv, pb, www;
     
      /* put a frame */
      SurfCont->ColPlane_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("GDset Controls",
            xmLabelWidgetClass, SurfCont->ColPlane_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      
      SUMA_Register_Widget_Help( www , 0,
                        "GraphCont->GDset_Controls",
                        "Control appearance of 3D graphs and matrices",
                  ":SPX:\n\n"
                  ".. figure:: media/GraphCont.auto.GDset_Controls.jpg\n"
                  "   :align: right\n"
                  "   :name: media/GraphCont.auto.GDset_Controls.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/GraphCont.auto.GDset_Controls.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
      
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->ColPlane_fr,
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
         char *Dset_tit[]  =  {  "Lbl", NULL };
         char *Dset_hint[] =  {  "Label of Graph Dset",  NULL };
         char *Dset_help[] =  {  SUMA_SurfContHelp_DsetLblTblr0, NULL };
         int colw[]={ 3, 27};
         SUMA_CreateTable(rc, 
            1, 2,
            "GraphCont->GDset_Controls->Lbl",
            Dset_tit, NULL,
            Dset_hint, NULL,
            Dset_help, NULL,
            colw, NOPE, SUMA_string,
            NULL, NULL,
            NULL, NULL,
            NULL, NULL, 
            SurfCont->ColPlaneLabelTable);
      }
      XtManageChild (rc);
            
      /* add a rc for the colorplane brightness and perhaps visibility? */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
   
      SUMA_CreateArrowField ( rc, "Dim",
                           1, 0.1, 2, 0.1,
                           3, SUMA_float,
                           YUP,
                           SUMA_cb_ColPlane_NewDimFact, (void *)ado, 
                           "GraphCont->GDset_Controls->Dim",
                           SUMA_SurfCont_ColPlaneDim_hint, 
                           SUMA_SurfContHelp_DsetDim,
                           SurfCont->ColPlaneDimFact);
      
          
      
      SurfCont->GDSET_ShowBundles_tb = 
         XtVaCreateManagedWidget("Bundles", 
                                 xmToggleButtonWidgetClass, rc, NULL);
      XmToggleButtonSetState (SurfCont->GDSET_ShowBundles_tb, 
                              GSaux->ShowBundles, NOPE);
      XtAddCallback (SurfCont->GDSET_ShowBundles_tb, 
                     XmNvalueChangedCallback, 
                     SUMA_cb_GDSET_ShowBundles_toggled, ado);
                  
      SUMA_Register_Widget_Help(SurfCont->GDSET_ShowBundles_tb , 1,
                        "GraphCont->GDset_Controls->Bundles",
                        "Show bundles instead of edges if possible.",
                        SUMA_SurfContHelp_GDSET_ViewBundles ) ;
      SUMA_SET_SELECT_COLOR(SurfCont->GDSET_ShowBundles_tb);
      
      SUMA_BuildMenuReset(0);
      SurfCont->DsetThroughMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetThrough);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "CN", '\0', YUP, DsetThrough_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "GraphCont->GDset_Controls->CN",
                "How to display connection to selected graph node.",
                SUMA_SurfContHelp_DsetThrough, 
                SurfCont->DsetThroughMenu );
      XtManageChild (SurfCont->DsetThroughMenu->mw[SW_SurfCont_DsetThrough]);
      SUMA_Set_Menu_Widget(SurfCont->DsetThroughMenu, 
                           SUMA_Through2ThroughMenuItem(curColPlane->Through));
           
      /* manage  rc */
      XtManageChild (rc);
      
      /* add a rc for the node radius, gain, and colors */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
         
      SUMA_BuildMenuReset(0);
      SurfCont->DsetNodeRadMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetNodeRad);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "Rd", '\0', YUP, DsetNodeRad_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "GraphCont->GDset_Controls->Rd",
                "Choose the sizing option for graph nodes.",
                SUMA_SurfContHelp_DsetNodeRad, 
                SurfCont->DsetNodeRadMenu );
      XtManageChild (SurfCont->DsetNodeRadMenu->mw[SW_SurfCont_DsetNodeRad]);
      
      SUMA_CreateArrowField ( rc, "Gn",
                           1.0, 0.0, 200.0, 1.0,
                           3, SUMA_float,
                           YUP,
                           SUMA_cb_ColPlane_NewNodeRadGain, (void *)ado, 
                           "GraphCont->GDset_Controls->Rd->Gn",
                           SUMA_SurfCont_NodeRadGain_hint, 
                           SUMA_SurfContHelp_DsetNodeRadGain,
                           SurfCont->NodeRadGainAF);
     
      SUMA_BuildMenuReset(0);
      SurfCont->DsetGmatBordMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetGmatBord);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "Br", '\0', YUP, DsetGmatBord_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "GraphCont->GDset_Controls->Br",
                "Choose the partition ratio of matrix.",
                SUMA_SurfContHelp_DsetGmatBord, 
                SurfCont->DsetGmatBordMenu );
      XtManageChild (SurfCont->DsetGmatBordMenu->mw[SW_SurfCont_DsetGmatBord]);
      
      XtManageChild (rc);
      
      /* add a rc for the node font and colorization */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);

      SUMA_BuildMenuReset(0);
      SurfCont->DsetFontMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetFont);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "Fo", '\0', YUP, DsetFont_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "GraphCont->GDset_Controls->Fo",
                "Choose the font for graph node labels.",
                SUMA_SurfContHelp_DsetFont, 
                SurfCont->DsetFontMenu );
      XtManageChild (SurfCont->DsetFontMenu->mw[SW_SurfCont_DsetFont]);
      SUMA_Set_Menu_Widget(SurfCont->DsetFontMenu, 
                           SUMA_Font2FontMenuItem(curColPlane->Font));
      
      SUMA_BuildMenuReset(0);
      SurfCont->DsetNodeColMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetNodeCol);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "Cl", '\0', YUP, DsetNodeCol_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "GraphCont->GDset_Controls->Cl",
                "Choose the coloring option for graph nodes.",
                SUMA_SurfContHelp_DsetNodeCol, 
                SurfCont->DsetNodeColMenu );
      XtManageChild (SurfCont->DsetNodeColMenu->mw[SW_SurfCont_DsetNodeCol]);
      SUMA_Set_Menu_Widget( SurfCont->DsetNodeColMenu, curColPlane->NodeCol);
      
      SUMA_BuildMenuReset(0);
      SurfCont->DsetTxtShadMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetTxtShad);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "Sh", '\0', YUP, DsetTxtShad_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "GraphCont->GDset_Controls->Sh",
                "Choose the shading options for node labels.",
                SUMA_SurfContHelp_DsetTxtShad, 
                SurfCont->DsetTxtShadMenu );
      XtManageChild (SurfCont->DsetTxtShadMenu->mw[SW_SurfCont_DsetTxtShad]);
      
      SurfCont->GDSET_ShowUncon_tb = 
         XtVaCreateManagedWidget("U", 
                                 xmToggleButtonWidgetClass, rc, NULL);
      XmToggleButtonSetState (SurfCont->GDSET_ShowUncon_tb, 
                              GSaux->ShowUncon, NOPE);
      XtAddCallback (SurfCont->GDSET_ShowUncon_tb, 
                     XmNvalueChangedCallback, 
                     SUMA_cb_GDSET_ShowUncon_toggled, ado);
      SUMA_Register_Widget_Help(SurfCont->GDSET_ShowUncon_tb , 1,
                        "GraphCont->GDset_Controls->U",
                        "Show Unconnected graph nodes.",
                        SUMA_SurfContHelp_GDSET_ViewUncon) ;            
      SUMA_SET_SELECT_COLOR(SurfCont->GDSET_ShowUncon_tb);
           

      
      /* manage  rc */
      XtManageChild (rc);
      
      /* add a rc for the edge size, stippling and colorization */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
      
      SUMA_BuildMenuReset(0);
      SurfCont->DsetEdgeThickMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetEdgeThick);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "Th", '\0', YUP, DsetEdgeThick_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "GraphCont->GDset_Controls->Th",
                "Choose the thickness option for graph edges.",
                SUMA_SurfContHelp_DsetEdgeThick, 
                SurfCont->DsetEdgeThickMenu );
      XtManageChild (SurfCont->DsetEdgeThickMenu->mw[SW_SurfCont_DsetEdgeThick]);
      
      SUMA_CreateArrowField ( rc, "Gn",
                           1.0, 0.0, 200.0, 1.0,
                           3, SUMA_float,
                           YUP,
                           SUMA_cb_ColPlane_NewEdgeThickGain, (void *)ado, 
                           "GraphCont->GDset_Controls->Th->Gn",
                           SUMA_SurfCont_EdgeThickGain_hint, 
                           SUMA_SurfContHelp_DsetEdgeThickGain,
                           SurfCont->EdgeThickGainAF);

      
      SUMA_BuildMenuReset(0);
      SurfCont->DsetEdgeStipMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetEdgeStip);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "St", '\0', YUP, DsetEdgeStip_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "GraphCont->GDset_Controls->St",
                "Choose the stippling option for graph edges.",
                SUMA_SurfContHelp_DsetEdgeStip, 
                SurfCont->DsetEdgeStipMenu );
      XtManageChild (SurfCont->DsetEdgeStipMenu->mw[SW_SurfCont_DsetEdgeStip]);
      
      XtManageChild (rc);
     
      
      #if 0 
         /* Not sure this is all that useful here.
            Makes no sense to load a new dataset onto an existing one.
            Switching certainly makes no sense... */
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
      SurfCont->SwitchDsetlst = 
            SUMA_AllocateScrolledList ("Switch Dset", 
                     SUMA_LSP_SINGLE, 
                     NOPE, NOPE, /* duplicate deletion, no sorting */ 
                     SurfCont->TLS, SWP_POINTER,
                     125,
                     SUMA_cb_SelectSwitchColPlane, (void *)ado,
                     SUMA_cb_SelectSwitchColPlane, (void *)ado,
                     SUMA_cb_CloseSwitchColPlane, NULL);


      pb = XtVaCreateWidget ("Switch Dset", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_SurfCont_SwitchColPlane, (XtPointer)ado);
      SUMA_Register_Widget_Help(pb, 1, "GraphCont->GDset_Controls->Switch_Dset",
                                "Switch between datasets", 
                                SUMA_SurfContHelp_DsetSwitch ) ;
      XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Load Dset", 
            xmPushButtonWidgetClass, rc, 
            NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_Dset_Load, (XtPointer) ado);
         SUMA_Register_Widget_Help(pb, 1, "GraphCont->GDset_Controls->Load_Dset",
                                   "Load a new dataset (much more with BHelp)",
                                   SUMA_SurfContHelp_DsetLoad ) ;
         XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Delete", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ColPlane_Delete, (XtPointer) ado);
      /* XtManageChild (pb); */ /* Not ready for this one yet */

      
       
      XtManageChild (rc);
      #endif
      
      /* manage vertical row column */
      XtManageChild (rcv);
      
      XtManageChild (SurfCont->ColPlane_fr);
   }
   
   
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      Widget rc=NULL;
      /* put something to cycle through objects */
      if ((rc = SUMA_FindChildWidgetNamed(SurfCont->DispFrame,"rowcolumnCBF"))) {
         XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc, 
                              XmNorientation, XmVERTICAL,NULL);
         pb = XtVaCreateWidget ("All Objs.", 
                  xmPushButtonWidgetClass, rc, 
                  NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_AllConts, NULL);
         SUMA_Register_Widget_Help(pb, 1, "GraphCont->Disp_Cont->AllObjs",
                                "Initialize Controllers for All Objects",
                                SUMA_SurfContHelp_AllObjs) ;
         XtManageChild (pb);

         SUMA_CreateArrowField ( rc, "Switch",
                           1, 1, 20, 1,
                           2, SUMA_int,
                           YUP,
                           SUMA_cb_SurfCont_SwitchPage, (void *)ado,
                           "GraphCont->Disp_Cont->Switch",
                           "Switch to other object controller", 
                           SUMA_Switch_Cont_BHelp,
                           SurfCont->SurfContPage);
         xmstmp = XmStringCreateLtoR (SUMA_ADO_CropLabel(ado, 
                                       SUMA_SURF_CONT_SWITCH_LABEL_LENGTH), 
                                      XmSTRING_DEFAULT_CHARSET);
         SurfCont->SurfContPage_label = XtVaCreateManagedWidget ("dingel-4", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
         XmStringFree (xmstmp);
      }
   }
   
   SUMA_LHv("Management ...%p %p %p %p %p\n",
            rc_right, rc_left, rc_mamma, SurfCont->Mainform, SurfCont->Page);

   XtManageChild (rc_right);
   XtManageChild (rc_left);
   XtManageChild (rc_mamma);
   XtManageChild (rc_gmamma);
   XtManageChild (SurfCont->Mainform);
   if (SUMAg_CF->X->UseSameSurfCont) XtManageChild (SurfCont->Page);
   
   #if SUMA_CONTROLLER_AS_DIALOG    
   #else
   /** Feb 03/03: pop it up if it is a topLevelShellWidgetClass, 
   you should do the popping after all the widgets have been created.
   Otherwise, the window does not size itself correctly when open */
   XtPopup(SurfCont->TLS, XtGrabNone);
   #endif
   
   /* realize the widget */
   if (SUMAg_CF->X->UseSameSurfCont) XtManageChild (SUMAg_CF->X->SC_Notebook);
   XtRealizeWidget (SurfCont->TLS);
   
   SUMA_LH("%s",slabel);
   SUMA_free (slabel);

   /* Mark as open */
   SUMA_MarkSurfContOpen(1,ado);
   
   SUMA_LHv("Marked %s's controller as open.\n", SUMA_ADO_Label(ado));
   
   /* initialize the left side 
      (no need here, that's done in SUMA_cb_viewSurfaceCont)*/
   /* SUMA_Init_SurfCont_SurfParam(ado); */
   
   /* initialize the ColorPlane frame if possible 
   Do it here rather than above because scale goes crazy 
   when parent widgets are being resized*/
   
   if (ado->do_type == SO_type && !(over0 = SUMA_ADO_Overlay0(ado))) {
      SUMA_SurfaceObject *SOp=NULL, *SO=(SUMA_SurfaceObject *)ado;
      SUMA_LH("NO Overlays yet for this surface\n");
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
   
   if (SUMA_ADO_N_Overlays(ado)>0) {
      SUMA_LH("Initializing ColPlaneShell");
      SUMA_InitializeColPlaneShell(ado, curColPlane);
      #ifdef NONONO /* It looks like I do not need this anymore 
                       Used to be #idef DARWIN */
      /* Sometimes color maps do not show up on mac when you first 
         open the surface controller */
      SUMA_LH("Darwining");
      SUMA_SwitchColPlaneCmap(ado, SUMA_CmapOfPlane(curColPlane));
      #endif
   }

   #if USING_LESSTIF
   SUMA_LH("Less tif fix");
   /* A quick fix to ensure Dset_Mapping 
      gets displayed properly the first time */
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   #endif

   SUMA_LH("going home.");

   SUMA_MarkSurfContOpen(1, ado);

   SUMA_RETURNe;
}

void SUMA_cb_createSurfaceCont_TDO(Widget w, XtPointer data, 
                                     XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSurfaceCont_TDO"};
   Widget tl, pb, form, 
          rc_left, rc_right, rc_mamma, rc_gmamma, tls=NULL;
   Display *dpy;
   SUMA_ALL_DO *ado;
   SUMA_TractDO *tdo;
   char *slabel, *lbl30, *sss=NULL;
   XmString xmstmp; 
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_TRACT_SAUX *TSaux=NULL;
   SUMA_OVERLAYS *curColPlane=NULL, *over0=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (ado->do_type != TRACT_type) {
      SUMA_S_Errv("Calling me with (%s) other than TRACT_type type,\n" 
                  "I don't like that, call me with TDO",
                  SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
      SUMA_RETURNe;
   }
   tdo = (SUMA_TractDO *)ado;
   
   if (LocalHead) {
      SUMA_LHv("Creating SurfaceCont for type %s, label %s\n",
            ADO_TNAME(ado), SUMA_ADO_Label(ado));
      SUMA_DUMP_TRACE("Creating SurfaceCont");
   }
   if (!(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Errv("Failed to get Controller for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   if (!SUMA_SurfCont_SetcurDOp(SurfCont, ado)) {
      SUMA_S_Errv("Failed to Set curDOp for %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   if (!(curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_S_Errv("Failed to get current col plane for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   SUMA_LH("CurColPlane %p for ado %s\n", curColPlane, SUMA_ADO_Label(ado));
   if (SurfCont->TLS) {
      fprintf (SUMA_STDERR,
               "Error %s: SurfCont->TopLevelShell!=NULL.\n"
               "Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   if (!(TSaux = SUMA_ADO_TSaux(ado))) {
      SUMA_S_Err("Failed to get TSaux");
      SUMA_RETURNe;
   }
   tl = SUMA_GetTopShell(w); /* top level widget */
   dpy = XtDisplay(tl);
   
   slabel = (char *)SUMA_malloc(sizeof(char) * 
                                 (strlen(SUMA_ADO_Label(ado)) + 100));
   if (strlen(SUMA_ADO_Label(ado)) > 40) {
      char *tmpstr=NULL;
      tmpstr = SUMA_truncate_string(SUMA_ADO_Label(ado), 40);
      if (tmpstr) { 
         sprintf(slabel,"[%s] Tract Controller", tmpstr);
         free(tmpstr); tmpstr=NULL;
      }
   } else {
      sprintf(slabel,"[%s] Tract Controller", SUMA_ADO_Label(ado));
   }
   
   /* March 12 08: Made font8 default for surface controller */
   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }

   SUMA_LH("Creating dialog shell.");
   if (!SUMAg_CF->X->UseSameSurfCont || 
       !SUMAg_CF->X->CommonSurfContTLW) { /* need a new one */
      #if SUMA_CONTROLLER_AS_DIALOG /* xmDialogShellWidgetClass, 
                                       topLevelShellWidgetClass*/
      tls = XtVaCreatePopupShell (sss,
         XmNtitle, slabel,
         xmDialogShellWidgetClass, tl,
         XmNallowShellResize, True, /* let code resize shell */
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);    
      #else
      SUMA_LH("Creating toplevel shell.");
      /** Feb 03/03:    I was using XtVaCreatePopupShell to create a 
                        topLevelShellWidgetClass. 
                        XtVaCreatePopupShell is used to create dialog 
                        shells not toplevel or appshells. 
                        Of course, it made no difference! */
      tls = XtVaAppCreateShell (sss, "Suma",
         topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
         XmNtitle, slabel,
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);   
      #endif

      /* allow for code to resize the shell */
      XtVaSetValues (tls, 
            XmNresizePolicy , XmRESIZE_NONE , 
            XmNallowShellResize , True ,       /* let code resize shell */
            NULL);

      /* handle the close button from window manager */
      XmAddWMProtocolCallback(/* make "Close" window menu work */
         tls,
         XmInternAtom( dpy , "WM_DELETE_WINDOW" , False ) ,
         SUMA_cb_closeSurfaceCont, (XtPointer) ado) ;
      
      if (SUMAg_CF->X->UseSameSurfCont) {
         Widget scroller;
         SUMAg_CF->X->CommonSurfContTLW = tls;
         SUMAg_CF->X->SC_Notebook = 
            XtVaCreateWidget("ControllerBook", xmNotebookWidgetClass,
                             SUMAg_CF->X->CommonSurfContTLW, 
                             XmNbindingWidth, XmNONE,
                             XmNbackPageNumber, 0, 
                             XmNmajorTabSpacing, 0, 
                             XmNminorTabSpacing, 0, 
                             NULL);
            /*XmCreateNotebook (SUMAg_CF->X->CommonSurfContTLW, "ControllerBook",
                              NULL, 0);
              XtVaSetValues(SUMAg_CF->X->SC_Notebook,
                          XmNbindingWidth, XmNONE,
                          NULL); */
            
         
         /* Kill the scroller from hell otherwise no keyboard input
            gets to the baby widgets. Better write my own scroller
            if need be in the future */
         scroller = XtNameToWidget (SUMAg_CF->X->SC_Notebook, "PageScroller");
         XtUnmanageChild (scroller);
      }

   }
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SurfCont->TLS = SUMAg_CF->X->CommonSurfContTLW;
   } else {
      SurfCont->TLS = tls;
   }
   
   if (!SurfCont->TLS) {
      SUMA_S_Err("Bad logic");
      SUMA_RETURNe;
   }
   
   SUMA_LH("Widgets...");
   if (SUMAg_CF->X->UseSameSurfCont) {
      Arg args[20];
      /* add the page */
      XtSetArg (args[0], XmNnotebookChildType, XmPAGE);
      SurfCont->Page = 
         XmCreateRowColumn (SUMAg_CF->X->SC_Notebook,
                     SUMA_ADO_Label(ado)?SUMA_ADO_Label(ado):"page",
                                              args, 1);
   }
   
   /* create a form widget, manage it at the end ...*/
   SurfCont->Mainform = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SurfCont->Page ? 
                              SurfCont->Page:SurfCont->TLS,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   rc_gmamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Mainform,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNbottomAttachment  , XmATTACH_FORM ,
            NULL);
   

   SurfCont->DispFrame = SUMA_CloseBhelp_Frame(rc_gmamma,
                     SUMA_cb_closeSurfaceCont, (XtPointer) ado,
                     "TractCont",
                     "Close Surface controller", SUMA_closeSurfaceCont_help,
                     NULL, NULL, NULL, NULL);
   SUMA_Register_Widget_Help( SurfCont->DispFrame , 0,
                              "TractCont",
                              "Network/Tracts Cont.",
"The tract controller is for controlling the way tracts and values "
"defined over them are displayed. "
"Each network of tracts gets its own controller. "
":SPX:"
"You can launch the :ref:`Tract Controller <TractCont>` with:"
" :ref:`ctrl+s <LC_Ctrl+s>` or :menuselection:`View-->Object Controller`\n"
"\n"
".. figure:: media/TractCont.auto.ALL.jpg\n"
"   :align: center\n"
"   :name: media/TractCont.auto.ALL.jpg\n"
"\n"
"   :ref:`(link)<media/TractCont.auto.ALL.jpg>`\n"
"\n\n"
"   ..\n\n"
":DEF:"
"You can launch the Tract Controller with:"
"\n'ctrl+s' or 'View-->Tract Controller'\n"
":SPX:"
"\n") ;
   
   XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rc_gmamma , NULL);
         
   rc_mamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_gmamma,
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
      Widget rc, label, rc_SurfProp, pb, www;
     
      /* put a frame */
      SurfCont->SurfFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Tract Properties",
            xmLabelWidgetClass, SurfCont->SurfFrame, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "TractCont->Tract_Properties",
                                 "Tract Properties",
               "Name and number of bundles, tracts, and points making up the "
               " selected network of tracts. "
                  ":SPX:\n\n"
                  ".. figure:: media/TractCont.auto.Tract_Properties.jpg\n"
                  "   :align: right\n"
                  "   :name: media/TractCont.auto.Tract_Properties.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/TractCont.auto.Tract_Properties.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
      
      rc_SurfProp = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->SurfFrame,
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

      /*put a label containing the Object name, number of bundles, tracts, 
        points */
      lbl30 = SUMA_set_string_length(SUMA_ADO_Label(ado), ' ', 27);
      if (lbl30) { 
         sprintf(slabel,"%s \n%d bnd. %d trc. %d pts.",
                     lbl30, TDO_N_BUNDLES(tdo), TDO_N_TRACTS(tdo),
                     SUMA_ADO_N_Datum(ado)); 
         SUMA_free(lbl30); lbl30 = NULL;
      } else {
         sprintf(slabel,"???\n%d bnd. %d trc. %d pts.",
                     TDO_N_BUNDLES(tdo), 
                     TDO_N_TRACTS(tdo), SUMA_ADO_N_Datum(ado)); 
      }
      xmstmp = XmStringCreateLtoR (slabel, XmSTRING_DEFAULT_CHARSET);
         /* XmStringCreateLocalized(slabel) does not reliably 
            handle newline characters q*/
      SurfCont->SurfInfo_label = XtVaCreateManagedWidget ("hagel", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
      XmStringFree (xmstmp);
      SUMA_Register_Widget_Help( SurfCont->SurfInfo_label, 1,
                                 "TractCont->Tract_Properties->label",
                                 "Summary object information" , 
                                 "Summary object information" ) ;
      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, rc, 
                                 XmNorientation, XmVERTICAL, NULL );

      SurfCont->SurfInfo_pb = XtVaCreateWidget ("more", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (SurfCont->SurfInfo_pb, XmNactivateCallback, 
                     SUMA_cb_moreSurfInfo, 
                        (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont));
      XtVaSetValues (SurfCont->SurfInfo_pb, XmNuserData, 
                     (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont), NULL); 
      SUMA_Register_Widget_Help( SurfCont->SurfInfo_pb, 1,
                                 "TractCont->Tract_Properties->more",
                                 "More info on network of tracts" , 
                                 SUMA_SurfContHelp_more ) ;
      XtManageChild (SurfCont->SurfInfo_pb); 

      XtManageChild (rc);
            
      XtManageChild (rc_SurfProp);
      XtManageChild (SurfCont->SurfFrame);
   }  
   
   SUMA_LH("Xhair business");
   {  /* Xhair Controls */
      Widget rcv, www;
      /* put a frame */
      SurfCont->Xhair_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Xhair Info",
            xmLabelWidgetClass, SurfCont->Xhair_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "TractCont->Xhair_Info",
                                 "Information at crosshair",
                  ":SPX:\n\n"
                  ".. figure:: media/TractCont.auto.Xhair_Info.jpg\n"
                  "   :align: right\n\n"
                  "   :name: media/TractCont.auto.Xhair_Info.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/TractCont.auto.Xhair_Info.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;            
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Xhair_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateXhairWidgets(rcv, ado);
      
      XtManageChild(rcv);
      XtManageChild(SurfCont->Xhair_fr);
   }  /* Xhair Controls */

   SUMA_LHv("\nDset Mapping, tract %s\n", SUMA_ADO_Label(ado));
   {  /* Dset Mapping */
      Widget rcv, www;
      /* put a frame */
      SurfCont->DsetMap_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_right,
         XmNrightAttachment , XmATTACH_FORM ,
         XmNleftAttachment, XmATTACH_WIDGET,
         XmNleftWidget, SurfCont->SurfFrame,
         XmNtopAttachment  , XmATTACH_FORM ,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Dset Mapping",
            xmLabelWidgetClass, SurfCont->DsetMap_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "TractCont->Dset_Mapping",
                                 "Tract Dset Color Mapping",
                  ":SPX:\n\n"
                  ".. figure:: media/TractCont.auto.Dset_Mapping.jpg\n"
                  "   :align: right\n"
                  "   :name: media/TractCont.auto.Dset_Mapping.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/TractCont.auto.Dset_Mapping.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;            
            
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->DsetMap_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateCmapWidgets(rcv, ado);

      XtManageChild(rcv);

      XtManageChild(SurfCont->DsetMap_fr);
   }

   SUMA_LH("Dset Controls");
   /* Dset Controls */
   {
       Widget rc, rcv, pb, www;
     
      
      /* put a frame */
      SurfCont->ColPlane_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Coloring Controls",
            xmLabelWidgetClass, SurfCont->ColPlane_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
            
      SUMA_Register_Widget_Help( www , 0,
                                 "TractCont->Coloring_Controls",
                                 "Coloring Controls",
"Controls the final coloration of the tracts based on the tract datasets"
" available. What's a tract dataset you say? It is a dataset defined over the"
" collection of points that define the tracts of a network. And where do we get"
" these sets? Nowhere at the moment. For now they are generated internally and"
" they are only of the RGB variety. This will change in the future, when you"
" would be able to drive a flying car and have arbitrary sets much like on"
" surfaces or volumes."
":SPX:\n\n"
".. figure:: media/TractCont.auto.Coloring_Controls.jpg\n"
"   :align: right\n"
"   :name: media/TractCont.auto.Coloring_Controls.jpg\n"
"\n"
"   :ref:`(link)<media/TractCont.auto.Coloring_Controls.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;            
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->ColPlane_fr,
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
         char *Dset_tit[]  =  {  "Lbl", NULL };
         char *Dset_hint[] =  {  "Label of dataset displayed on tracts",  NULL };
         char *Dset_help[] =  {  SUMA_SurfContHelp_DsetLblTblr0, NULL };
         int colw[]={ 3, 27};
         SUMA_CreateTable(rc, 
            1, 2,
            "TractCont->Coloring_Controls->Lbl",
            Dset_tit, NULL,
            Dset_hint, NULL,
            Dset_help, NULL,
            colw, NOPE, SUMA_string,
            NULL, NULL,
            NULL, NULL,
            NULL, NULL, 
            SurfCont->ColPlaneLabelTable);
      }
      XtManageChild (rc);
            
      /* add a rc for the colorplane brightness and perhaps visibility? */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
   
      SUMA_CreateArrowField ( rc, "Dim",
                           1, 0.1, 2, 0.1,
                           3, SUMA_float,
                           YUP,
                           SUMA_cb_ColPlane_NewDimFact, (void *)ado, 
                           "TractCont->Coloring_Controls->Dim",
                           SUMA_SurfCont_ColPlaneDim_hint, 
                           SUMA_SurfContHelp_DsetDim,
                           SurfCont->ColPlaneDimFact);
      
      SUMA_CreateArrowField ( rc, "Ord",
                           1, 0, 20, 1,
                           2, SUMA_int,
                           NOPE,
                           SUMA_cb_ColPlane_NewOrder, (void *)ado,
                           "TractCont->Coloring_Controls->Ord",
                           SUMA_SurfCont_ColPlaneOrder_hint, 
                           SUMA_TractContHelp_DsetOrd,
                           SurfCont->ColPlaneOrder);
           
      SurfCont->ColPlaneShowOneFore_tb = 
         XtVaCreateManagedWidget("1", 
                                 xmToggleButtonWidgetClass, rc, NULL);
      XmToggleButtonSetState (SurfCont->ColPlaneShowOneFore_tb, 
                              SurfCont->ShowCurForeOnly, NOPE);
      XtAddCallback (SurfCont->ColPlaneShowOneFore_tb, 
                     XmNvalueChangedCallback, 
                     SUMA_cb_ColPlaneShowOneFore_toggled, ado);
                  
      SUMA_Register_Widget_Help(SurfCont->ColPlaneShowOneFore_tb , 1,
                                "TractCont->Coloring_Controls->1",
             "Show ONLY selected set. (BHelp for more)",
                                SUMA_TractContHelp_DsetViewOne ) ;
      SUMA_SET_SELECT_COLOR(SurfCont->ColPlaneShowOneFore_tb);
           
      /* manage  rc */
      XtManageChild (rc);
      
      /* another row*/
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
         
      SUMA_BuildMenuReset(0);
                             
      SUMA_CreateArrowField ( rc, "Opa",
                           1, 0.0, 1.0, 0.1,
                           3, SUMA_float,
                           NOPE,
                           SUMA_cb_ColPlane_NewOpacity, (void *)ado,
                           "TractCont->Coloring_Controls->Opa",
                           SUMA_SurfCont_ColPlaneOpacity_hint,
                           SUMA_TractContHelp_DsetOpa,
                           SurfCont->ColPlaneOpacity);

      SUMA_BuildMenuReset(0);
      SurfCont->TractStyleMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_TractStyle);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "Ln", '\0', YUP, TractStyle_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "TractCont->Coloring_Controls->Ln",
                "Line drawing style",
                SUMA_SurfContHelp_TractStyle, 
                SurfCont->TractStyleMenu );
      XtManageChild (SurfCont->TractStyleMenu->mw[SW_SurfCont_TractStyle]);
      
      XtManageChild (rc);
     
      /* Masking opts*/
      rc = XtVaCreateWidget ("rowcolumn",
          xmRowColumnWidgetClass, rcv,
          XmNpacking, XmPACK_TIGHT, 
          XmNorientation , XmHORIZONTAL ,
          NULL);
             
      SurfCont->Mask_pb = XtVaCreateWidget ("Masks", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (SurfCont->Mask_pb, XmNactivateCallback, 
                     SUMA_cb_Mask, 
                        (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont));
      XtVaSetValues (SurfCont->Mask_pb, XmNuserData, 
                     (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont), NULL); 
      SUMA_Register_Widget_Help( SurfCont->Mask_pb , 1,
                                 "TractCont->Coloring_Controls->Masks",
                                 "Create/Switch to Masks controller",
                                 SUMA_SurfContHelp_Mask ) ;
      XtManageChild (SurfCont->Mask_pb); 
 
      SUMA_BuildMenuReset(0);
      SurfCont->TractMaskMenu =
          SUMA_Alloc_Menu_Widget(SW_N_SurfCont_TractMask);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "", '\0', YUP, TractMask_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "TractCont->Coloring_Controls->Hde",
                "Choose how masked tracts are displayed.",
                SUMA_SurfContHelp_TractMask, 
                SurfCont->TractMaskMenu );
      XtManageChild (SurfCont->TractMaskMenu->mw[SW_SurfCont_TractMask]);
      if (!SUMA_findanyMDOp(NULL)) 
         XtSetSensitive(SurfCont->TractMaskMenu->mw[SW_SurfCont_TractMask], 0);
      
      SUMA_BuildMenuReset(0);
      SUMA_CreateArrowField ( rc, "Gry",
                           TSaux->MaskGray, 0.0, 100, 5,
                           2, SUMA_int,
                           NOPE,
                           SUMA_cb_Tract_NewGray, (void *)ado,
                           "TractCont->Coloring_Controls->Gry",
                           SUMA_SurfCont_TractMask_hint,
                           SUMA_SurfContHelp_TractMaskGray,
                           SurfCont->TractMaskGray);
      if (!SUMA_findanyMDOp(NULL)) 
         XtSetSensitive(SurfCont->TractMaskGray->rc, 0);
         
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
      SurfCont->SwitchDsetlst = 
            SUMA_AllocateScrolledList ("Switch Coloring", 
                     SUMA_LSP_SINGLE, 
                     NOPE, NOPE, /* duplicate deletion, no sorting */ 
                     SurfCont->TLS, SWP_POINTER,
                     125,
                     SUMA_cb_SelectSwitchColPlane, (void *)ado,
                     SUMA_cb_SelectSwitchColPlane, (void *)ado,
                     SUMA_cb_CloseSwitchColPlane, NULL);


      pb = XtVaCreateWidget ("Switch Dset", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_SurfCont_SwitchColPlane, (XtPointer)ado);
      SUMA_Register_Widget_Help(pb, 1, 
                           "TractCont->Coloring_Controls->Switch_Dset",
                                "Switch between datasets",
                                SUMA_TractContHelp_DsetSwitch ) ;
      XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Load Dset", 
            xmPushButtonWidgetClass, rc, 
            NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_Dset_Load, (XtPointer) ado);
      SUMA_Register_Widget_Help(pb, 1,"TractCont->Coloring_Controls->Load_Dset",
                                "Load a new dataset (much more with BHelp)",
                                SUMA_SurfContHelp_DsetLoad ) ;
      /* XtManageChild (pb); Not ready just yet */
      
      pb = XtVaCreateWidget ("Delete", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ColPlane_Delete, (XtPointer) ado);
      /* XtManageChild (pb); */ /* Not ready for this one yet */

      
       
      XtManageChild (rc);
      /* manage vertical row column */
      XtManageChild (rcv);
      
      XtManageChild (SurfCont->ColPlane_fr);
   }
   
   
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      Widget rc=NULL;
      /* put something to cycle through objects */
      if ((rc = SUMA_FindChildWidgetNamed(SurfCont->DispFrame, "rowcolumnCBF"))){
         XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc, 
                              XmNorientation, XmVERTICAL,NULL);
         pb = XtVaCreateWidget ("All Objs.", 
                  xmPushButtonWidgetClass, rc, 
                  NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_AllConts, NULL);
         SUMA_Register_Widget_Help(pb, 1,"TractCont->Disp_Cont->AllObjs",
                                "Initialize Controllers for All Objects",
                                SUMA_SurfContHelp_AllObjs) ;
         XtManageChild (pb);

         SUMA_CreateArrowField ( rc, "Switch",
                           1, 1, 20, 1,
                           2, SUMA_int,
                           YUP,
                           SUMA_cb_SurfCont_SwitchPage, (void *)ado,
                           "TractCont->Disp_Cont->Switch",
                           "Switch to other object controller", 
                           SUMA_Switch_Cont_BHelp,
                           SurfCont->SurfContPage);
         xmstmp = XmStringCreateLtoR (SUMA_ADO_CropLabel(ado, 
                                          SUMA_SURF_CONT_SWITCH_LABEL_LENGTH), 
                                      XmSTRING_DEFAULT_CHARSET);
         SurfCont->SurfContPage_label = XtVaCreateManagedWidget ("dingel-5", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
         XmStringFree (xmstmp);
      }
   }
   
   SUMA_LHv("Management ...%p %p %p %p %p\n",
            rc_right, rc_left, rc_mamma, SurfCont->Mainform, SurfCont->Page);

   XtManageChild (rc_right);
   XtManageChild (rc_left);
   XtManageChild (rc_mamma);
   XtManageChild (rc_gmamma);
   XtManageChild (SurfCont->Mainform);
   if (SUMAg_CF->X->UseSameSurfCont) XtManageChild (SurfCont->Page);
   
   #if SUMA_CONTROLLER_AS_DIALOG    
   #else
   /** Feb 03/03: pop it up if it is a topLevelShellWidgetClass, 
   you should do the popping after all the widgets have been created.
   Otherwise, the window does not size itself correctly when open */
   XtPopup(SurfCont->TLS, XtGrabNone);
   #endif
   
   /* realize the widget */
   if (SUMAg_CF->X->UseSameSurfCont) XtManageChild (SUMAg_CF->X->SC_Notebook);
   XtRealizeWidget (SurfCont->TLS);
   
   SUMA_LH("%s",slabel);
   SUMA_free (slabel);

   /* Mark as open */
   SUMA_MarkSurfContOpen(1,ado);
   
   SUMA_LHv("Marked %s's controller as open.\n", SUMA_ADO_Label(ado));
   
   /* initialize the left side 
      (no need here, that's done in SUMA_cb_viewSurfaceCont)*/
   /* SUMA_Init_SurfCont_SurfParam(ado); */
   
   /* initialize the ColorPlane frame if possible 
   Do it here rather than above because scale goes crazy 
   when parent widgets are being resized*/
   
  
   if (SUMA_ADO_N_Overlays(ado)>0) {
      SUMA_LH("Initializing ColPlaneShell");
      SUMA_InitializeColPlaneShell(ado, curColPlane);
      #ifdef NONONO /* It looks like I do not need this anymore 
                       Used to be #idef DARWIN */
      /* Sometimes color maps do not show up on mac when you first 
         open the surface controller */
      SUMA_LH("Darwining");
      SUMA_SwitchColPlaneCmap(ado, SUMA_CmapOfPlane(curColPlane));
      #endif
   }

   #if USING_LESSTIF
   SUMA_LH("Less tif fix");
   /* A quick fix to ensure Dset_Mapping 
      gets displayed properly the first time */
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   #endif

   SUMA_LH("going home.");

   SUMA_MarkSurfContOpen(1, ado);

   SUMA_RETURNe;
}

void SUMA_cb_createSurfaceCont_CO(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSurfaceCont_CO"};
   Widget tl, pb, form, 
          rc_left, rc_right, rc_mamma, rc_gmamma, tls=NULL;
   Display *dpy;
   SUMA_ALL_DO *ado;
   SUMA_CIFTI_DO *co;
   char *slabel, *lbl30, *sss=NULL;
   XmString xmstmp; 
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL, *over0=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (ado->do_type != CDOM_type) {
      SUMA_S_Errv("Calling me with (%s) other than VO_type type,\n" 
                  "I don't like that, call me with VO",
                  SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
      SUMA_RETURNe;
   }
   
   SUMA_S_Err("CIFTI objects do not have their own controller");
   
   SUMA_RETURNe;  
}

void SUMA_cb_createSurfaceCont_VO(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSurfaceCont_VO"};
   Widget tl, pb, form, 
          rc_left, rc_right, rc_mamma, rc_gmamma, tls=NULL;
   Display *dpy;
   SUMA_ALL_DO *ado;
   SUMA_VolumeObject *vo;
   char *slabel, *lbl30, *sss=NULL;
   XmString xmstmp; 
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL, *over0=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (ado->do_type != VO_type) {
      SUMA_S_Errv("Calling me with (%s) other than VO_type type,\n" 
                  "I don't like that, call me with VO",
                  SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
      SUMA_RETURNe;
   }
   vo = (SUMA_VolumeObject *)ado;
   
   SUMA_LHv("Creating SurfaceCont for type %s, label %s\n",
            ADO_TNAME(ado), SUMA_ADO_Label(ado));
   
   if (!(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Errv("Failed to get Controller for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   SUMA_LH("SurfCont = %p, %p %p %p", 
            SurfCont, SurfCont->Ax_slc, SurfCont->Sa_slc, SurfCont->Co_slc);
   if (!SUMA_SurfCont_SetcurDOp(SurfCont, ado)) {
      SUMA_S_Errv("Failed to Set curDOp for %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   if (!(curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_S_Errv("Failed to get current col plane for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   if (SurfCont->TLS) {
      fprintf (SUMA_STDERR,
               "Error %s: SurfCont->TopLevelShell!=NULL.\n"
               "Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   
   SUMA_LH("Getting TL of %p", w);
   tl = SUMA_GetTopShell(w); /* top level widget */
   SUMA_LH("Got tl = %p", tl);
   dpy = XtDisplay(tl);
   SUMA_LH("Got dpy %p", dpy);
   
   slabel = (char *)SUMA_malloc(sizeof(char) * 
                                 (strlen(SUMA_ADO_Label(ado)) + 100));
   if (strlen(SUMA_ADO_Label(ado)) > 40) {
      char *tmpstr=NULL;
      tmpstr = SUMA_truncate_string(SUMA_ADO_Label(ado), 40);
      if (tmpstr) { 
         sprintf(slabel,"[%s] Volume Controller", tmpstr);
         free(tmpstr); tmpstr=NULL;
      }
   } else {
      sprintf(slabel,"[%s] Volume Controller", SUMA_ADO_Label(ado));
   }
   
   /* March 12 08: Made font8 default for surface controller */
   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }

   SUMA_LH("Creating dialog shell.");
   if (!SUMAg_CF->X->UseSameSurfCont || 
       !SUMAg_CF->X->CommonSurfContTLW) { /* need a new one */
      #if SUMA_CONTROLLER_AS_DIALOG /* xmDialogShellWidgetClass, 
                                       topLevelShellWidgetClass*/
      tls = XtVaCreatePopupShell (sss,
         XmNtitle, slabel,
         xmDialogShellWidgetClass, tl,
         XmNallowShellResize, True, /* let code resize shell */
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);    
      #else
      SUMA_LH("Creating toplevel shell.");
      /** Feb 03/03:    I was using XtVaCreatePopupShell to create a 
                        topLevelShellWidgetClass. 
                        XtVaCreatePopupShell is used to create dialog 
                        shells not toplevel or appshells. 
                        Of course, it made no difference! */
      tls = XtVaAppCreateShell (sss, "Suma",
         topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
         XmNtitle, slabel,
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);   
      #endif

      /* allow for code to resize the shell */
      XtVaSetValues (tls, 
            XmNresizePolicy , XmRESIZE_NONE , 
            XmNallowShellResize , True ,       /* let code resize shell */
            NULL);

      /* handle the close button from window manager */
      XmAddWMProtocolCallback(/* make "Close" window menu work */
         tls,
         XmInternAtom( dpy , "WM_DELETE_WINDOW" , False ) ,
         SUMA_cb_closeSurfaceCont, (XtPointer) ado) ;
      
      if (SUMAg_CF->X->UseSameSurfCont) {
         Widget scroller;
         SUMAg_CF->X->CommonSurfContTLW = tls;
         SUMAg_CF->X->SC_Notebook = 
            XtVaCreateWidget("ControllerBook", xmNotebookWidgetClass,
                             SUMAg_CF->X->CommonSurfContTLW, 
                             XmNbindingWidth, XmNONE,
                             XmNbackPageNumber, 0, 
                             XmNmajorTabSpacing, 0, 
                             XmNminorTabSpacing, 0, 
                             NULL);
            /*XmCreateNotebook (SUMAg_CF->X->CommonSurfContTLW, "ControllerBook",
                              NULL, 0);
              XtVaSetValues(SUMAg_CF->X->SC_Notebook,
                          XmNbindingWidth, XmNONE,
                          NULL); */
            
         
         /* Kill the scroller from hell otherwise no keyboard input
            gets to the baby widgets. Better write my own scroller
            if need be in the future */
         scroller = XtNameToWidget (SUMAg_CF->X->SC_Notebook, "PageScroller");
         XtUnmanageChild (scroller);
      }
   }
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SurfCont->TLS = SUMAg_CF->X->CommonSurfContTLW;
   } else {
      SurfCont->TLS = tls;
   }
   
   if (!SurfCont->TLS) {
      SUMA_S_Err("Bad logic");
      SUMA_RETURNe;
   }
   
   SUMA_LH("Widgets...");
   if (SUMAg_CF->X->UseSameSurfCont) {
      Arg args[20];
      /* add the page */
      XtSetArg (args[0], XmNnotebookChildType, XmPAGE);
      SurfCont->Page = 
         XmCreateRowColumn (SUMAg_CF->X->SC_Notebook,
                     SUMA_ADO_Label(ado)?SUMA_ADO_Label(ado):"page",
                                              args, 1);
   }
   
   /* create a form widget, manage it at the end ...*/
   SurfCont->Mainform = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SurfCont->Page ? 
                              SurfCont->Page:SurfCont->TLS,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   rc_gmamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Mainform,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNbottomAttachment  , XmATTACH_FORM ,
            NULL);
   

   SurfCont->DispFrame = SUMA_CloseBhelp_Frame(rc_gmamma,
                     SUMA_cb_closeSurfaceCont, (XtPointer) ado,
                     "VolCont",
                     "Close Surface controller", SUMA_closeSurfaceCont_help,
                     NULL, NULL, NULL, NULL);
   
   SUMA_Register_Widget_Help( SurfCont->DispFrame , 0,
                                 "VolCont",
                                 "Volume Cont.",
"The volume controller is for controlling the way volumes are rendered. Each volume gets its own controller. You can use the switch button above to switch between them. The volume controller is initialized by the volume of the last selected voxel.\n After you have selected a voxel, "
":SPX:"
"you can launch the :ref:`Volume Controller <VolCont>` with:"
" :ref:`ctrl+s <LC_Ctrl+s>` or :menuselection:`View-->Object Controller`\n"
"\n"
".. figure:: media/VolCont.auto.ALL.jpg\n"
"   :align: center\n"
"   :name: media/VolCont.auto.ALL.jpg\n"
"\n"
"   :ref:`(link)<media/VolCont.auto.ALL.jpg>`\n"
"   ..\n\n"
":DEF:"
"you can launch the Volume Controller with:"
"\n'ctrl+s' or 'View-->Object Controller'\n"
":SPX:"
"\n") ;

   XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rc_gmamma , NULL);
         
   rc_mamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_gmamma,
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
                    
   
   {/*surface properties */ 
      Widget rc, label, rc_SurfProp, pb, www;
     
      /* put a frame */
      SurfCont->SurfFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Volume Properties",
            xmLabelWidgetClass, SurfCont->SurfFrame, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0, 
                                 "VolCont->Volume_Properties",
                                 "Volume Properties",
                  "Block providing information about selected volume."
                  ":SPX:\n\n"
                  ".. figure:: media/VolCont.auto.Volume_Properties.jpg\n"
                  "   :align: right\n"
                  "   :name: media/VolCont.auto.Volume_Properties.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/VolCont.auto.Volume_Properties.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
      
      rc_SurfProp = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->SurfFrame,
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

      /*put a label containing the Object name, number of bundles, tracts, 
        points */
      
      lbl30 = SUMA_set_string_length(SUMA_ADO_Label(ado), ' ', 27);
      if (lbl30) { 
         sprintf(slabel,"%s\n%d x %d x %d (%d vox)",
                     lbl30, VO_NI(vo), VO_NJ(vo), VO_NK(vo), 
                     SUMA_ADO_N_Datum(ado)); 
         SUMA_free(lbl30); lbl30 = NULL;
      } else {
         sprintf(slabel,"???\n%d x %d x %d = %d voxels.",
                     VO_NI(vo), VO_NJ(vo), VO_NK(vo),
                     SUMA_ADO_N_Datum(ado)); 
      }
      xmstmp = XmStringCreateLtoR (slabel, XmSTRING_DEFAULT_CHARSET);
         /* XmStringCreateLocalized(slabel) does not reliably 
            handle newline characters q*/
      SurfCont->SurfInfo_label = XtVaCreateManagedWidget ("shagel", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
      XmStringFree (xmstmp);
      SUMA_Register_Widget_Help( SurfCont->SurfInfo_label, 1,
                                 "VolCont->Volume_Properties->label",
                                 "Summary object information" , 
                                 "Summary object information" ) ;
      
      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, rc, 
                                 XmNorientation, XmVERTICAL, NULL );

      SurfCont->SurfInfo_pb = XtVaCreateWidget ("more", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (SurfCont->SurfInfo_pb, XmNactivateCallback, 
                     SUMA_cb_moreSurfInfo, 
                        (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont));
      XtVaSetValues (SurfCont->SurfInfo_pb, XmNuserData, 
                     (XtPointer)SUMA_SurfCont_GetcurDOp(SurfCont), NULL); 
      SUMA_Register_Widget_Help( SurfCont->SurfInfo_pb , 1,
                                 "VolCont->Volume_Properties->more",
                                 "More info on Volume",
                                 SUMA_SurfContHelp_more ) ;
      XtManageChild (SurfCont->SurfInfo_pb); 

      XtManageChild (rc);
            
      XtManageChild (rc_SurfProp);
      XtManageChild (SurfCont->SurfFrame);
   }  
   
   SUMA_LH("Xhair business");
   {  /* Xhair Controls */
      Widget rcv, www;
      
      /* put a frame */
      SurfCont->Xhair_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Xhair Info",
            xmLabelWidgetClass, SurfCont->Xhair_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "VolCont->Xhair_Info",
                                 "Information at crosshair",
                                 ":SPX:\n\n"
                  ".. figure:: media/VolCont.auto.Xhair_Info.jpg\n"
                  "   :align: right\n"
                  "   :name: media/VolCont.auto.Xhair_Info.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/VolCont.auto.Xhair_Info.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
     
      
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Xhair_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateXhairWidgets(rcv, ado);
      
      XtManageChild(rcv);
      XtManageChild(SurfCont->Xhair_fr);
   }  /* Xhair Controls */

   SUMA_LHv("\nDset Mapping, volume %s\n", SUMA_ADO_Label(ado));
   {  /* Dset Mapping */
      Widget rcv, www;
      /* put a frame */
      SurfCont->DsetMap_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_right,
         XmNrightAttachment , XmATTACH_FORM ,
         XmNleftAttachment, XmATTACH_WIDGET,
         XmNleftWidget, SurfCont->SurfFrame,
         XmNtopAttachment  , XmATTACH_FORM ,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Dset Mapping",
            xmLabelWidgetClass, SurfCont->DsetMap_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "VolCont->Dset_Mapping",
                                 "Dset Color Mapping",
                  ":SPX:\n\n"
                  ".. figure:: media/VolCont.auto.Dset_Mapping.jpg\n"
                  "   :align: right\n"
                  "   :name: media/VolCont.auto.Dset_Mapping.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/VolCont.auto.Dset_Mapping.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;
            
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->DsetMap_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      /* create the widgets for the colormap stuff */
      SUMA_CreateCmapWidgets(rcv, ado);

      XtManageChild(rcv);

      XtManageChild(SurfCont->DsetMap_fr);
   }

   SUMA_LH("Slice Controls");
   {
      Widget rc, rcv, pb, rch, www;
      
      /* put a frame */
      SurfCont->Slice_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Slice Controls",
            xmLabelWidgetClass, SurfCont->Slice_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "VolCont->Slice_Controls",
                             "Set up which and how many slices are displayed",
                  ":SPX:\n\n"
                  ".. figure:: media/VolCont.auto.Slice_Controls.jpg\n"
                  "   :align: right\n"
                  "   :name: media/VolCont.auto.Slice_Controls.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/VolCont.auto.Slice_Controls.jpg>`\n"
                  "\n"
                  ":SPX:") ;

      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Slice_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      
      /* Slice toys */       
      SUMA_CreateSliceFields( rcv, "Ax", 
                  SUMA_SliceSelect_axial_hint, SUMA_SliceSelect_axial_help,
                  SUMA_VO_N_Slices((SUMA_VolumeObject *)ado, "Ax"), "Ax", ado,
                           NULL, (void *)ado, 
                           SurfCont->Ax_slc);
      SUMA_CreateSliceFields( rcv, "Sa", 
                  SUMA_SliceSelect_sagittal_hint, SUMA_SliceSelect_sagittal_help,
                  SUMA_VO_N_Slices((SUMA_VolumeObject *)ado, "Sa"), "Sa", ado,
                           NULL, (void *)ado, 
                           SurfCont->Sa_slc);
      SUMA_CreateSliceFields( rcv, "Co", 
                  SUMA_SliceSelect_coronal_hint, SUMA_SliceSelect_coronal_help,
                  SUMA_VO_N_Slices((SUMA_VolumeObject *)ado, "Co"), "Co", ado,
                           NULL, (void *)ado, 
                           SurfCont->Co_slc);
      
      rch = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rcv,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      SUMA_BuildMenuReset(0);
      SurfCont->VTransModeMenu = SUMA_Alloc_Menu_Widget(SW_N_SurfCont_ATrans);
      SUMA_BuildMenu (  rch, XmMENU_OPTION, 
                        "Trn", '\0', YUP, VTransMode_Menu, 
                        (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                        "VolCont->Slice_Controls->Trn",
                        "Choose the transparency for these slices.",
                        SUMA_SurfContHelp_VTransMode, 
                        SurfCont->VTransModeMenu );
      XtManageChild (SurfCont->VTransModeMenu->mw[SW_SurfCont_ATrans]);
      
      /* Now for the toggle button */
      SurfCont->VSliceAtXYZ_tb = XtVaCreateManagedWidget("Slices At +", 
                        xmToggleButtonWidgetClass, rch, NULL);
      XtAddCallback (SurfCont->VSliceAtXYZ_tb, 
                  XmNvalueChangedCallback, SUMA_cb_VSliceAtXYZ_toggled, ado);
      SUMA_Register_Widget_Help(SurfCont->VSliceAtXYZ_tb, 1,
                                "VolCont->Slice_Controls->Slices_At_+",
                                "Make slices jump to crosshair location",
                                "Make slices jump to crosshair location");

      SUMA_SET_SELECT_COLOR(SurfCont->VSliceAtXYZ_tb);
      XmToggleButtonSetState (SurfCont->VSliceAtXYZ_tb, 
                  SUMA_VO_SlicesAtCrosshair((SUMA_VolumeObject *)ado) , NOPE);
      
      XtManageChild (rch);
      XtManageChild (rcv);
      XtManageChild (SurfCont->Slice_fr);
   }

   SUMA_LH("Volume Rendering Controls");
   {
      Widget rc, rcv, pb, www;
      
      /* put a frame */
      SurfCont->VR_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Volume Rendering Controls",
            xmLabelWidgetClass, SurfCont->VR_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help(www, 0,
                                "VolCont->Volume_Rendering_Controls",
                                "Set the parameters for 3D rendering",
         ":SPX:\n\n"
         ".. figure:: media/VolCont.auto.Volume_Rendering_Controls.jpg\n"
         "   :align: right\n"
         "   :name: media/VolCont.auto.Volume_Rendering_Controls.jpg\n"
         "\n"
         "   :ref:`(link)<media/VolCont.auto.Volume_Rendering_Controls.jpg>`\n"
         "\n"
               ":SPX:"      );
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->VR_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      
      /* Slice toys */       
      SUMA_CreateVrFields( rcv,  "Ns",
                  SUMA_VR_hint, SUMA_VR_help,
                  SUMA_VO_N_Slices((SUMA_VolumeObject *)ado, "Mx"),  ado,
                           NULL, (void *)ado, 
                           SurfCont->VR_fld);
      XtManageChild(rcv);
      XtManageChild (SurfCont->VR_fr);
   }
   
   SUMA_LH("Dset Controls");
   /* Dset Controls */
   {
       Widget rc, rcv, pb, www;
      
      /* put a frame */
      SurfCont->ColPlane_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Dset Controls",
            xmLabelWidgetClass, SurfCont->ColPlane_fr, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "VolCont->Dset_Controls",
                                 "Dset Controls",
                  ":SPX:\n\n"
                  ".. figure:: media/VolCont.auto.Dset_Controls.jpg\n"
                  "   :align: right\n"
                  "   :name: media/VolCont.auto.Dset_Controls.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/VolCont.auto.Dset_Controls.jpg>`\n"
                  "\n"
                  ":SPX:") ;
      
      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->ColPlane_fr,
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
         char *Dset_tit[]  =  {  "Lbl", NULL };
         char *Dset_hint[] =  {  "Label of Volume Dset",  NULL };
         char *Dset_help[] =  {  SUMA_SurfContHelp_DsetLblTblr0, NULL };
         int colw[]={ 3, 27};
         SUMA_CreateTable(rc, 
            1, 2,
            "VolCont->Dset_Controls->Lbl",
            Dset_tit, NULL,
            Dset_hint, NULL,
            Dset_help, NULL,
            colw, NOPE, SUMA_string,
            NULL, NULL,
            NULL, NULL,
            NULL, NULL, 
            SurfCont->ColPlaneLabelTable);
      }
      XtManageChild (rc);
            
      /* add a rc for the colorplane brightness and perhaps visibility? */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
   
      SUMA_CreateArrowField ( rc, "Dim",
                           1, 0.1, 2, 0.1,
                           3, SUMA_float,
                           YUP,
                           SUMA_cb_ColPlane_NewDimFact, (void *)ado, 
                           "VolCont->Dset_Controls->Dim",
                           SUMA_SurfCont_ColPlaneDim_hint, 
                           SUMA_SurfContHelp_DsetDim,
                           SurfCont->ColPlaneDimFact);
      
      #if 0 /* Nothing for this yet */
      SUMA_CreateArrowField ( rc, "Ord",
                           1, 0, 20, 1,
                           2, SUMA_int,
                           NOPE,
                           SUMA_cb_ColPlane_NewOrder, (void *)ado,
                           "VolCont->Dset_Controls->Ord",
                           SUMA_SurfCont_ColPlaneOrder_hint, 
                           SUMA_SurfContHelp_DsetOrd,
                           SurfCont->ColPlaneOrder);
      #endif     
           
      /* manage  rc */
      XtManageChild (rc);
      
      /* another row*/
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);

      SUMA_BuildMenuReset(0);
      SurfCont->DsetAlphaValMenu =
         SUMA_Alloc_Menu_Widget(SW_N_SurfCont_DsetAlphaVal);
      SUMA_BuildMenu (rc, XmMENU_OPTION, 
                "Avl", '\0', YUP, DsetAlphaVal_Menu,
                (void *)SUMA_SurfCont_GetcurDOp(SurfCont), 
                "VolCont->Dset_Controls->Avl",
                "Choose method for computing color alpha value for voxels.",
                SUMA_SurfContHelp_DsetAlphaVal, 
                SurfCont->DsetAlphaValMenu );
      XtManageChild (SurfCont->DsetAlphaValMenu->mw[SW_SurfCont_DsetAlphaVal]);
      
      SUMA_CreateArrowField ( rc, "Ath",
                           1, 0.0, 1, 0.1,
                           3, SUMA_float,
                           YUP,
                           SUMA_cb_ColPlane_NewAlphaThresh, (void *)ado, 
                           "VolCont->Dset_Controls->Ath",
                           SUMA_SurfCont_ColPlaneAlphaThresh_hint, 
                           SUMA_SurfContHelp_DsetAlphaThresh,
                           SurfCont->ColPlaneAlphaThresh);
      
      
      #if 0  /* Not ready for this yet, no multiple vols used */
      SurfCont->ColPlaneShowOneFore_tb = 
         XtVaCreateManagedWidget("1", 
                                 xmToggleButtonWidgetClass, rc, NULL);
      XmToggleButtonSetState (SurfCont->ColPlaneShowOneFore_tb, 
                              SurfCont->ShowCurForeOnly, NOPE);
      XtAddCallback (SurfCont->ColPlaneShowOneFore_tb, 
                     XmNvalueChangedCallback, 
                     SUMA_cb_ColPlaneShowOneFore_toggled, ado);
                  
      SUMA_Register_Widget_Help(SurfCont->ColPlaneShowOneFore_tb ,  1,
                                "VolCont->Dset_Controls->1",
             "Show ONLY selected set. Foreground only. (BHelp for more)\n",
                                SUMA_SurfContHelp_DsetViewOne ) ;
      SUMA_SET_SELECT_COLOR(SurfCont->ColPlaneShowOneFore_tb);
      #endif
      
      XtManageChild (rc);
      
      #if 0 /* Nothing for these yet */
      SUMA_BuildMenuReset(0);
      SUMA_CreateArrowField ( rc, "Opa",
                           1, 0.0, 1.0, 0.1,
                           3, SUMA_float,
                           NOPE,
                           SUMA_cb_ColPlane_NewOpacity, (void *)ado,
                           "VolCont->Dset_Controls->Opa",
                           SUMA_SurfCont_ColPlaneOpacity_hint,
                           SUMA_SurfContHelp_DsetOpa,
                           SurfCont->ColPlaneOpacity);

     
      
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
      SurfCont->SwitchDsetlst = 
            SUMA_AllocateScrolledList ("Switch Dset", 
                     SUMA_LSP_SINGLE, 
                     NOPE, NOPE, /* duplicate deletion, no sorting */ 
                     SurfCont->TLS, SWP_POINTER,
                     125,
                     SUMA_cb_SelectSwitchColPlane, (void *)ado,
                     SUMA_cb_SelectSwitchColPlane, (void *)ado,
                     SUMA_cb_CloseSwitchColPlane, NULL);


      pb = XtVaCreateWidget ("Switch Dset", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_SurfCont_SwitchColPlane, (XtPointer)ado);
      SUMA_Register_Widget_Help(pb, 1,
                                "VolCont->Dset_Controls->Switch_Dset",
                                "Switch between datasets",
                                SUMA_SurfContHelp_DsetSwitch ) ;
      XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Load Dset", 
            xmPushButtonWidgetClass, rc, 
            NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_Dset_Load, (XtPointer) ado);
      SUMA_Register_Widget_Help(pb, 1,
                                "VolCont->Dset_Controls->Load_Dset",
                                "Load a new dataset (much more with BHelp)",
                                SUMA_SurfContHelp_DsetLoad ) ;
      XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Delete", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, 
                     SUMA_cb_ColPlane_Delete, (XtPointer) ado);
      /* XtManageChild (pb); */ /* Not ready for this one yet */

      XtManageChild (rc);
      #endif /* Nothing for switching or loading Dsets now ... */
      
      /* manage vertical row column */
      XtManageChild (rcv);
      
      XtManageChild (SurfCont->ColPlane_fr);
   }
   
   
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      Widget rc=NULL;
      /* put something to cycle through objects */
      if ((rc = SUMA_FindChildWidgetNamed(SurfCont->DispFrame, "rowcolumnCBF"))){
         XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc, 
                              XmNorientation, XmVERTICAL,NULL);
         pb = XtVaCreateWidget ("All Objs.", 
                  xmPushButtonWidgetClass, rc, 
                  NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_AllConts, NULL);
         SUMA_Register_Widget_Help(pb, 1, "VolCont->Disp_Cont->AllObjs",
                                "Initialize Controllers for All Objects",
                                SUMA_SurfContHelp_AllObjs) ;
         XtManageChild (pb);

         SUMA_CreateArrowField ( rc, "Switch",
                           1, 1, 20, 1,
                           2, SUMA_int,
                           YUP,
                           SUMA_cb_SurfCont_SwitchPage, (void *)ado,
                           "VolCont->Disp_Cont->Switch",
                           "Switch to other object controller", 
                           SUMA_Switch_Cont_BHelp,
                           SurfCont->SurfContPage);
         xmstmp = XmStringCreateLtoR (SUMA_ADO_CropLabel(ado, 
                                       SUMA_SURF_CONT_SWITCH_LABEL_LENGTH), 
                                      XmSTRING_DEFAULT_CHARSET);
         SurfCont->SurfContPage_label = XtVaCreateManagedWidget ("dingel-6", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
         XmStringFree (xmstmp);
      }
   }
   
   SUMA_LHv("Management ...%p %p %p %p %p\n",
            rc_right, rc_left, rc_mamma, SurfCont->Mainform, SurfCont->Page);

   XtManageChild (rc_right);
   XtManageChild (rc_left);
   XtManageChild (rc_mamma);
   XtManageChild (rc_gmamma);
   XtManageChild (SurfCont->Mainform);
   if (SUMAg_CF->X->UseSameSurfCont) XtManageChild (SurfCont->Page);
   
   
   SUMA_LH("Popup maybe and Notebook management");
   #if SUMA_CONTROLLER_AS_DIALOG    
   #else
   /** Feb 03/03: pop it up if it is a topLevelShellWidgetClass, 
   you should do the popping after all the widgets have been created.
   Otherwise, the window does not size itself correctly when open */
   XtPopup(SurfCont->TLS, XtGrabNone);
   #endif
   
   /* realize the widget */
   if (SUMAg_CF->X->UseSameSurfCont) {
      XtManageChild (SUMAg_CF->X->SC_Notebook);
   }
   SUMA_LH("Realize TLS widget %p, closed %d", 
            SurfCont->TLS, !SUMAg_CF->X->SameSurfContOpen);
   XtRealizeWidget (SurfCont->TLS);
   
   SUMA_LH("%s",slabel);
   SUMA_free (slabel);

   
   /* initialize the left side 
      (no need here, that's done in SUMA_cb_viewSurfaceCont)*/
   /* SUMA_Init_SurfCont_SurfParam(ado); */
   
   /* initialize the ColorPlane frame if possible 
   Do it here rather than above because scale goes crazy 
   when parent widgets are being resized*/
   
   if (SUMA_ADO_N_Overlays(ado)>0) {
      SUMA_LH("Initializing ColPlaneShell");
      SUMA_InitializeColPlaneShell(ado, curColPlane);
      #ifdef NONONO /* It looks like I do not need this anymore 
                       Used to be #idef DARWIN */
      /* Sometimes color maps do not show up on mac when you first 
         open the surface controller */
      SUMA_LH("Darwining");
      SUMA_SwitchColPlaneCmap(ado, SUMA_CmapOfPlane(curColPlane));
      #endif
   } else {
      SUMA_LH("No overlays to work with");
   }

   #if USING_LESSTIF
   SUMA_LH("Less tif fix");
   /* A quick fix to ensure Dset_Mapping 
      gets displayed properly the first time */
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   #endif

   SUMA_LH("going home.");

   SUMA_MarkSurfContOpen(1, ado);
   SUMA_LHv("Marked %s's controller as open.\n", SUMA_ADO_Label(ado));

   SUMA_RETURNe;
}

/*!
   \brief close surface controller, expects SO in data
*/
void SUMA_cb_closeSurfaceCont(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_closeSurfaceCont"};
   SUMA_ALL_DO *ado;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   SurfCont = SUMA_ADO_Cont(ado);
   SUMA_LHv("Have TLS %p Open %d Same %d SameOpen %d\n",
            SurfCont->TLS, SurfCont->Open,
            SUMAg_CF->X->UseSameSurfCont, SUMAg_CF->X->SameSurfContOpen) 
   if (!SurfCont->TLS || !SurfCont->Open ||
       (SUMAg_CF->X->UseSameSurfCont && !SUMAg_CF->X->SameSurfContOpen) )
      SUMA_RETURNe;

   switch (SUMA_GL_CLOSE_MODE)   {/* No open GL drawables in this widget*/
      case SUMA_WITHDRAW:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Withdrawing Surface Controller...\n", FuncName);
         XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
            XtWindow(SurfCont->TLS),
            XScreenNumberOfScreen(XtScreen(SurfCont->TLS)));
         break;
      case SUMA_DESTROY: 
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Destroying Surface Controller...\n", FuncName);
         if (!SUMAg_CF->X->CommonSurfContTLW) { /* not in common mode */
               XtDestroyWidget(SurfCont->TLS);
         }
         SurfCont->TLS = NULL;
         break;
      case SUMA_UNREALIZE:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Unrealizing Surface Controller...\n", FuncName);
         XtUnrealizeWidget(SurfCont->TLS);
         break;
      default: 
         SUMA_S_Err("Not set up to deal with this closure mode");
         SUMA_RETURNe;
         break;
   }
   
   SUMA_MarkSurfContOpen(0,ado);
   SUMA_RETURNe;
}

/*! 
   Find number for page widget in notebook 
   0 for bad news
*/
int SUMA_PageWidgetToNumber(Widget NB, Widget page)
{
   static char FuncName[]={"SUMA_PageWidgetToNumber"};
   int i, lp;
   XmNotebookPageStatus ns;
   XmNotebookPageInfo pi;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!NB || !page) SUMA_RETURN(0);
   
   XtVaGetValues(NB, XmNlastPageNumber, &lp, NULL);
   for (i=0; i<lp; ++i) {
      ns = XmNotebookGetPageInfo(NB, i+1, &pi);
      if (ns != XmPAGE_FOUND) {
         SUMA_LH("Could not find page!");
         SUMA_RETURN(0);
      }
      SUMA_LHv("Page %d, widget %p, have page %p, match=%d, "
                   "page name=%s\n",
                   i+1, pi.page_widget, page,
                   (pi.page_widget==page)?1:0,
                   XtName(pi.page_widget));
      if (pi.page_widget==page) SUMA_RETURN(i+1);  
   }
   SUMA_RETURN(0);
}

int SUMA_isCurrentContPage(Widget NB, Widget page)
{
   static char FuncName[]={"SUMA_isCurrentContPage"};
   int lp;
   XmNotebookPageStatus ns;
   XmNotebookPageInfo pi;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!NB) SUMA_RETURN(0);
   
   XtVaGetValues(NB, XmNcurrentPageNumber, &lp, NULL);
   ns = XmNotebookGetPageInfo(NB, lp, &pi);
   if (ns != XmPAGE_FOUND) {
         SUMA_LH("Could not find current page!");
         SUMA_RETURN(0);
   }
   if (pi.page_widget==page) SUMA_RETURN(1);
   SUMA_RETURN(0);
}

/*!
   \brief updates the left side of Surface Controller
*/
SUMA_Boolean SUMA_Init_SurfCont_SurfParam(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_Init_SurfCont_SurfParam"};
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURN(NOPE);  
   
   switch (ado->do_type) {
      case SO_type:
         SUMA_RETURN(SUMA_Init_SurfCont_SurfParam_SO((SUMA_SurfaceObject *)ado));
         break;
      case CDOM_type:
         SUMA_RETURN(SUMA_Init_SurfCont_SurfParam_CO(ado));
         break;
      case GDSET_type:
         SUMA_S_Err("Should not send me DOs that can't be displayed \n"
                    "without variant info");
         SUMA_RETURN(NOPE);
         break;
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            SUMA_RETURN(NOPE);
         }
         SUMA_RETURN(SUMA_Init_SurfCont_SurfParam_GLDO(ado));
         break; }
      case TRACT_type: {
         SUMA_RETURN(SUMA_Init_SurfCont_SurfParam_TDO(ado));
         break; }
      case MASK_type: {
         SUMA_RETURN(SUMA_Init_SurfCont_SurfParam_MDO(ado));
         break; }
      case VO_type: {
         SUMA_RETURN(SUMA_Init_SurfCont_SurfParam_VO(ado));
         break; }
      default:
         SUMA_S_Errv("Rien de rien for %s\n",
            SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         break;
   }
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_Init_SurfCont_SurfParam_SO(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_Init_SurfCont_SurfParam_SO"};
   char *slabel = NULL, *Name, *lbl30 = NULL;
   int i, imenu;
   Widget *w=NULL, whist=NULL;
   XmString string;
   SUMA_SurfaceObject *oSO;
   SUMA_ALL_DO *oado=NULL;
   SUMA_Boolean SameSurface = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!(oado = SUMA_SurfCont_GetcurDOp(SO->SurfCont))) {
      SUMA_S_Err("Failed to retrieve DO");
      SUMA_RETURN(NOPE);
   }
   if (oado->do_type != SO_type) {
      SUMA_S_Err("Did not expect type mismatch here.");
      SUMA_RETURN(NOPE);
   } else {
      oSO = (SUMA_SurfaceObject *)oado;
   }
   if (oSO == SO) {
      SameSurface = YUP;
   } else {
      SameSurface = NOPE;
   }
    
   SUMA_LHv("Called for %s, current %s, buttdown = %d\n", 
            SO->Label, oSO->Label, SUMAg_CF->X->ButtonDown);
   /* set the new current surface pointer */
   if (!SUMA_SurfCont_SetcurDOp(SO->SurfCont, (SUMA_ALL_DO *)SO)) {
      SUMA_S_Err("Failed to set current pointer");
      SUMA_RETURN(NOPE);  
   }
   if (!SameSurface || 
       ( SUMAg_CF->X->UseSameSurfCont && 
         !SUMA_isCurrentContPage(SUMAg_CF->X->SC_Notebook, 
                                 SO->SurfCont->Page))) {
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
      XtVaSetValues(SO->SurfCont->TLS, XtNtitle, slabel, NULL);

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
      /* look for name of widget with imenu for call data. 
         This is overkill but its fun */
      i = 0;
      Name = NULL;
      while (&(RenderMode_Menu[i])) {
         if ((INT_CAST)RenderMode_Menu[i].callback_data == imenu) {
            Name = RenderMode_Menu[i].label;
            if (LocalHead) fprintf (SUMA_STDERR,"Looking for %s\n", Name);
            /* now we know what the name of the button needed is, look for it*/
            w = SO->SurfCont->RenderModeMenu->mw;
            for (i=0; i< SW_N_SurfCont_Render; ++i) {
               if (LocalHead) fprintf (SUMA_STDERR,"I have %s\n", XtName(w[i]));
               if (strcmp(Name, XtName(w[i])) == 0) {
                  SUMA_LH("Match!");
                  XtVaSetValues(  w[0], XmNmenuHistory , w[i] , NULL ) ;  
                  goto RAISE;
              }
            }
         }
         ++i;
      }
      
      SUMA_LH("Setting TransMode");
      /* set the transparency for that surface */
      imenu = -1;
      switch (SO->TransMode) {
         case STM_ViewerDefault:
            imenu = SW_SurfCont_TransViewerDefault;
            break;
         case STM_0:
            imenu = SW_SurfCont_Trans0;
            break;
         case STM_1:
            imenu = SW_SurfCont_Trans1;
            break;
         case STM_2:
            imenu = SW_SurfCont_Trans2;
            break;
         case STM_3:
            imenu = SW_SurfCont_Trans3;
            break;
         case STM_4:
            imenu = SW_SurfCont_Trans4;
            break;
         case STM_5:
            imenu = SW_SurfCont_Trans5;
            break;
         case STM_6:
            imenu = SW_SurfCont_Trans6;
            break;
         case STM_7:
            imenu = SW_SurfCont_Trans7;
            break;
         case STM_8:
            imenu = SW_SurfCont_Trans8;
            break;
         case STM_9:
            imenu = SW_SurfCont_Trans9;
            break;
         case STM_10:
            imenu = SW_SurfCont_Trans10;
            break;
         case STM_11:
            imenu = SW_SurfCont_Trans11;
            break;
         case STM_12:
            imenu = SW_SurfCont_Trans12;
            break;
         case STM_13:
            imenu = SW_SurfCont_Trans13;
            break;
         case STM_14:
            imenu = SW_SurfCont_Trans14;
            break;
         case STM_15:
            imenu = SW_SurfCont_Trans15;
            break;
         case STM_16:
            imenu = SW_SurfCont_Trans16;
            break;
         default: 
            fprintf (SUMA_STDERR, "Error %s: Unexpected something.\n", FuncName);
            break;
      }
      /* look for name of widget with imenu for call data. 
         This is overkill but its fun */
      i = 0;
      Name = NULL;
      while (&(TransMode_Menu[i])) {
         if ((INT_CAST)TransMode_Menu[i].callback_data == imenu) {
            Name = TransMode_Menu[i].label;
            if (LocalHead) fprintf (SUMA_STDERR,"Looking for %s\n", Name);
            /* now we know what the name of the button needed is, look for it*/
            w = SO->SurfCont->TransModeMenu->mw;
            for (i=0; i< SW_N_SurfCont_Trans; ++i) {
               if (LocalHead) fprintf (SUMA_STDERR,"I have %s\n", XtName(w[i]));
               if (strcmp(Name, XtName(w[i])) == 0) {
                  SUMA_LH("Match!");
                  XtVaSetValues(  w[0], XmNmenuHistory , w[i] , NULL ) ;  
                  goto RAISE;
              }
            }
         }
         ++i;
      }

      RAISE:
      if (SUMAg_CF->X->UseSameSurfCont) { 
         if (!(i = SUMA_PageWidgetToNumber(SUMAg_CF->X->SC_Notebook, 
                                          SO->SurfCont->Page))) {
            SUMA_S_Errv("Failed to find controller page for surface %s\n",
                        CHECK_NULL_STR(SO->Label));
         } else {
            SUMA_LHv("Setting notebook page to page %d, button down is %d\n",
                     i,SUMAg_CF->X->ButtonDown);
            if (LocalHead) SUMA_DUMP_TRACE("You rang?");
            if (!SUMAg_CF->X->ButtonDown) {
               SUMA_SetSurfContPageNumber(SUMAg_CF->X->SC_Notebook, i);
            }   
         }
      }
   } 
   
   /* do even if this is the old surface */ 
    
   SUMA_RETURN(YUP);
}

int SUMA_NotebookLastPageNumber(Widget NB) 
{
   static char FuncName[]={"SUMA_NotebookLastPageNumber"};
   int imax=0;
   if (NB) XtVaGetValues(NB, XmNlastPageNumber, &imax, NULL);
   return(imax);
}

SUMA_Boolean SUMA_SetSurfContPageNumber(Widget NB, int i)
{
   static char FuncName[]={"SUMA_SetSurfContPageNumber"};
   int k, N_adolist=0, adolist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   SUMA_X_SurfCont *SurfCont = NULL;
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   XmString string;
   int imax;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!NB || i < 1) {
      SUMA_S_Errv("NULL widget or bad page number %d\n", i);
      SUMA_RETURN(NOPE);
   }
   XtVaGetValues(NB, XmNlastPageNumber, &imax, NULL);
   if (i > imax) {
      SUMA_S_Errv("Request to switch to page %d, but have %d pages total.\n", 
                  i, imax);
      SUMA_RETURN(NOPE);
   }
   SUMA_LHv("Setting page to %d\n", i);
   XtVaSetValues(NB, XmNcurrentPageNumber, i, NULL);
   
   /* And force set all arrow fields, they are supposed to act like one */
   N_adolist = SUMA_ADOs_WithSurfCont (SUMAg_DOv, SUMAg_N_DOv, adolist);
   SUMA_LHv("Force setting %d surfconts to page %d, max %d\n", 
               N_adolist, i, imax);
   for (k=0; k<N_adolist; ++k) {
      /* Note that many objects in this list maybe intimately
         related (they share the same parent graph link). So
         beware that you may be setting the same arrow head 
         multiple times. But that is not a big deal, just redundant
         work */
      SUMA_LHv("   %d for %s\n",
               k, SUMA_ADO_Label((SUMA_ALL_DO *)SUMAg_DOv[adolist[k]].OP));
      SurfCont = SUMA_ADO_Cont((SUMA_ALL_DO *)SUMAg_DOv[adolist[k]].OP);
      if (SurfCont && SurfCont->SurfContPage && SurfCont->SurfContPage->rc) {
         SurfCont->SurfContPage->value = i;
         SurfCont->SurfContPage->max = (float)imax;
         
         sprintf(sbuf,"%d",(int)SurfCont->SurfContPage->value);
         SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, sbuf);
         
         string = XmStringCreateLtoR (
               SUMA_ADO_CropLabel((SUMA_ALL_DO *)SUMAg_DOv[adolist[k]].OP, 
                                  SUMA_SURF_CONT_SWITCH_LABEL_LENGTH), 
                     XmSTRING_DEFAULT_CHARSET);
         XtVaSetValues( SurfCont->SurfContPage_label, 
                        XmNlabelString, string, NULL);
         XmStringFree (string);
      }
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_Init_SurfCont_SurfParam_GLDO(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_Init_SurfCont_SurfParam_GLDO"};
   return(SUMA_Init_SurfCont_SurfParam_ADO(ado));
}

SUMA_Boolean SUMA_Init_SurfCont_SurfParam_TDO(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_Init_SurfCont_SurfParam_TDO"};
   return(SUMA_Init_SurfCont_SurfParam_ADO(ado));
}

SUMA_Boolean SUMA_Init_SurfCont_SurfParam_VO(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_Init_SurfCont_SurfParam_VO"};
   return(SUMA_Init_SurfCont_SurfParam_ADO(ado));
}

SUMA_Boolean SUMA_Init_SurfCont_SurfParam_CO(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_Init_SurfCont_SurfParam_CO"};
   return(SUMA_Init_SurfCont_SurfParam_ADO(ado));
}

SUMA_Boolean SUMA_Init_SurfCont_SurfParam_MDO(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_Init_SurfCont_SurfParam_MDO"};
   return(SUMA_Init_SurfCont_SurfParam_ADO(ado));
}

SUMA_Boolean SUMA_Init_SurfCont_SurfParam_ADO(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_Init_SurfCont_SurfParam_ADO"};
   char *slabel = NULL, *Name, *lbl30 = NULL;
   int i, imenu;
   Widget *w=NULL, whist=NULL;
   XmString string;
   SUMA_ALL_DO *oado=NULL;
   SUMA_X_SurfCont *SurfCont = NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean SameObject = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado)) || !SurfCont->TLS) {
      SUMA_S_Err("NULL input on ADO %s type %s (%p, TLS %p)",
                  ADO_LABEL(ado), ADO_TNAME(ado), 
                  SurfCont, SurfCont?SurfCont->TLS:NULL); 
      SUMA_DUMP_TRACE("NULL input on ADO");
      SUMA_RETURN(NOPE);
   }
   
   if (!(oado = SUMA_SurfCont_GetcurDOp(SurfCont))) {
      SUMA_S_Err("Failed to retrieve DO"); SUMA_RETURN(NOPE);
   }
   
   if (oado->do_type != ado->do_type) {
      SUMA_S_Errv("Type mismatch: oado is %d (%s), ado is %d (%s)\n",
                  oado->do_type, 
                  SUMA_ObjectTypeCode2ObjectTypeName(oado->do_type),
                  ado->do_type, 
                  SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
      SUMA_DUMP_TRACE("DO type mismatch");
      SUMA_RETURN(NOPE);
   } 
   if (oado == ado) {
      SameObject = YUP;
   } else {
      SameObject = NOPE;
   }
    
   SUMA_LHv("Called for %s, current %s, buttdown = %d\n", 
            SUMA_ADO_Label(ado), SUMA_ADO_Label(oado), SUMAg_CF->X->ButtonDown);
   /* set the new current surface pointer. 
      Here you must decide whether or not to get the particular DO variant 
      that was clicked on...  */
   if (!SUMA_SurfCont_SetcurDOp(SurfCont,ado)) {
      SUMA_S_Err("Failed to set curDOp");
      SUMA_RETURN(NOPE);
   }
                
   if (!SameObject || 
       ( SUMAg_CF->X->UseSameSurfCont && 
         !SUMA_isCurrentContPage(SUMAg_CF->X->SC_Notebook, 
                                 SurfCont->Page))) {      
      /* initialize the title of the window */
      slabel = (char *)SUMA_malloc (sizeof(char) * 
                           (strlen(SUMA_ADO_Label(ado)) + 100));
      
      if (strlen(SUMA_ADO_Label(ado)) > 40) {
         char *tmpstr=NULL;
         tmpstr = SUMA_truncate_string(SUMA_ADO_Label(ado), 40);
         if (tmpstr) { 
            sprintf(slabel,"[%s] %s Controller", tmpstr, SUMA_ADO_ContName(ado));
            free(tmpstr); tmpstr=NULL;
         }
      } else {
         sprintf(slabel,"[%s] %s Controller", 
                        SUMA_ADO_Label(ado), SUMA_ADO_ContName(ado));
      }
      SUMA_LH("Setting title");
      XtVaSetValues(SurfCont->TLS, XtNtitle, slabel, NULL);

      /* initialize the string before the more button */
         /*put a label containing the surface name, number of nodes 
            and number of facesets */
         lbl30 = SUMA_set_string_length(SUMA_ADO_Label(ado), ' ', 27);
         switch (ado->do_type) {
            case GRAPH_LINK_type:
               if (lbl30) { 
                  sprintf(slabel,"%s\n%d edges, variant %s.",
                           lbl30, SUMA_ADO_N_Datum(ado), SUMA_ADO_variant(ado)); 
                  SUMA_free(lbl30); lbl30 = NULL;
               } else {
                  sprintf(slabel,"???\n%d edges.",SUMA_ADO_N_Datum(ado)); 
               }
               break;
            case TRACT_type: {
               SUMA_TractDO *tdo = (SUMA_TractDO *)ado;
               if (lbl30) { 
                  sprintf(slabel,"%s \n%d bnd. %d trc. %d pts.",
                              lbl30, TDO_N_BUNDLES(tdo), TDO_N_TRACTS(tdo),
                              SUMA_ADO_N_Datum(ado)); 
                  SUMA_free(lbl30); lbl30 = NULL;
               } else {
                  sprintf(slabel,"???\n%d bnd. %d trc. %d pts.",
                              TDO_N_BUNDLES(tdo), 
                              TDO_N_TRACTS(tdo), SUMA_ADO_N_Datum(ado)); 
               }
               break; }
            case MASK_type: { /* Won't be used anymore, see below */
               SUMA_MaskDO *mdo = (SUMA_MaskDO *)ado;
               if (lbl30) { 
                  sprintf(slabel,"Current: %s\n ", lbl30); 
                  SUMA_free(lbl30); lbl30 = NULL;
               } else {
                  sprintf(slabel,"Current: ???\n "); 
               }
               break; }
            default:
               if (lbl30) { 
                  sprintf(slabel,"%s\n%d data vals.",
                           lbl30, SUMA_ADO_N_Datum(ado)); 
                  SUMA_free(lbl30); lbl30 = NULL;
               } else {
                  sprintf(slabel,"???\n%d data vals.",SUMA_ADO_N_Datum(ado)); 
               }
               break;
         }
         if (SurfCont->SurfInfo_label) { /* Not used for MASK_type */
            SUMA_LHv("Setting label to\n>>%s<<\n", slabel);
            string = XmStringCreateLtoR (slabel, XmSTRING_DEFAULT_CHARSET);
            XtVaSetValues( SurfCont->SurfInfo_label, 
                           XmNlabelString, string, NULL);
            XmStringFree (string);
         }

      if (slabel) SUMA_free(slabel); slabel = NULL;
      /* Can't do much with the SurfInfo button,
      You can only have on Info shell/LocalDomainParent
      at a time */
      

      /* RAISE */
      if (SUMAg_CF->X->UseSameSurfCont) { 
         if (!(i = SUMA_PageWidgetToNumber(SUMAg_CF->X->SC_Notebook, 
                                           SurfCont->Page))) {
            SUMA_S_Errv("Failed to find controller page for surface %s\n",
                        CHECK_NULL_STR(SUMA_ADO_Label(ado)));
         } else {
            SUMA_LHv("Setting notebook page to page %d, button down is %d\n",
                     i,SUMAg_CF->X->ButtonDown);
            if (LocalHead) SUMA_DUMP_TRACE("You rang?");
            if (!SUMAg_CF->X->ButtonDown) {
               SUMA_SetSurfContPageNumber(SUMAg_CF->X->SC_Notebook, i);
            }
         }
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
      switch (SUMA_CLOSE_MODE)   {/* No open GL drawables in this widget*/
         case SUMA_WITHDRAW:
            if (LocalHead) 
               fprintf (SUMA_STDERR,"%s: raising DrawROI window \n", FuncName);
            XMapRaised(SUMAg_CF->X->DPY_controller1, 
                        XtWindow(SUMAg_CF->X->DrawROI->AppShell));
            break;
         default:
            SUMA_S_Err("Not setup for this close mode");
            SUMA_RETURN(NOPE);
            break;
      }
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
SUMA_Boolean SUMA_InitializeColPlaneShell(
                  SUMA_ALL_DO *ado, 
                  SUMA_OVERLAYS *colPlane)
{
   static char FuncName[] = {"SUMA_InitializeColPlaneShell"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   
   if (!ado) {
      SUMA_S_Err("NULL input, what gives?");
      SUMA_RETURN(NOPE);
   }
   if (LocalHead) {
      SUMA_LH("Called with colPlane %p, ado %s", colPlane, ADO_LABEL(ado));
      SUMA_DUMP_TRACE("And who called that one?");
   }
   switch(ado->do_type) {
      case SO_type:
         SUMA_RETURN(SUMA_InitializeColPlaneShell_SO(
                              (SUMA_SurfaceObject *)ado, colPlane));
         break;
      case GDSET_type:
         SUMA_S_Err("No init for a DO that cannot be dispalyed\n"
                    "without variant");
         SUMA_RETURN(NOPE);
         break;
      case CDOM_type:
         SUMA_RETURN(SUMA_InitializeColPlaneShell_CO(ado, colPlane));
         break;
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            SUMA_RETURN(NOPE);
         }
         SUMA_RETURN(SUMA_InitializeColPlaneShell_GLDO(ado, colPlane));
         break; }
      case TRACT_type: {
         SUMA_RETURN(SUMA_InitializeColPlaneShell_TDO(ado, colPlane));
         break; }
      case MASK_type: {
         SUMA_RETURN(SUMA_InitializeColPlaneShell_MDO(ado, colPlane));
         break; }
      case VO_type: {
         SUMA_RETURN(SUMA_InitializeColPlaneShell_VO(ado, colPlane));
         break; }
      default:
         SUMA_S_Errv("Not ready for %s types\n",
            SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_InitializeColPlaneShell_SO (
                  SUMA_SurfaceObject *SO, 
                  SUMA_OVERLAYS *ColPlane)
{
   static char FuncName[] = {"SUMA_InitializeColPlaneShell_SO"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   double range[2];
   int loc[2], i;
   SUMA_SurfaceObject *SOpar=NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!SO || !SO->SurfCont) {
      SUMA_S_Err("NULL input, what gives?");
      SUMA_RETURN(NOPE);
   }
   
   if (!SO->SurfCont->ColPlane_fr) {
      /* just set the curColPlane before returning ZSS  March 25 08*/
      if (ColPlane) SO->SurfCont->curColPlane = ColPlane;
      SUMA_RETURN(YUP);
   }
   
   if (!ColPlane) {
      SUMA_LH("Initializing with NULL");
      SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 0, 1, "-");
      SUMA_INSERT_CELL_STRING(SO->SurfCont->ColPlaneLabelTable, 1, 1, "-");
      SUMA_SET_TEXT_FIELD(SO->SurfCont->SurfContPage->textfield, "-");
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
         SUMA_SL_Warn(  "No domain parent for dset %s found.\n"
                        "Proceeding with next best option.", 
                        SDSET_LABEL(ColPlane->dset_link));
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
      
      if (SUMAg_CF->X->UseSameSurfCont) {
         SO->SurfCont->SurfContPage->value = 
            SUMA_PageWidgetToNumber(SUMAg_CF->X->SC_Notebook, 
                                    SO->SurfCont->Page);
         SUMA_LHv("Setting PPpage to %d\n",
                      (int)SO->SurfCont->SurfContPage->value);
         sprintf(sbuf,"%d",(int)SO->SurfCont->SurfContPage->value);
         SUMA_SET_TEXT_FIELD(SO->SurfCont->SurfContPage->textfield, sbuf);
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
   

   SO->SurfCont->curColPlane = ColPlane;
   
   SUMA_LHv("Have ShowMode for %s of %d (widget %d)\n", 
                SO->SurfCont->curColPlane->Label,
                SO->SurfCont->curColPlane->ShowMode,
                SUMA_ShowMode2ShowModeMenuItem(
                              SO->SurfCont->curColPlane->ShowMode));
   SUMA_Set_Menu_Widget(SO->SurfCont->DsetViewModeMenu,
                 SUMA_ShowMode2ShowModeMenuItem(
                              SO->SurfCont->curColPlane->ShowMode));
   /* obsolete 
   XmToggleButtonSetState (SO->SurfCont->ColPlaneShow_tb, ColPlane->Show, NOPE);
   */
   
   /* Set 1 only sensitivity */
   if (SO->SurfCont->ColPlaneShowOneFore_tb) {
      if (SO->SurfCont->curColPlane->isBackGrnd) {
         XtSetSensitive (SO->SurfCont->ColPlaneShowOneFore_tb, 0);
      }  else {
         XtSetSensitive (SO->SurfCont->ColPlaneShowOneFore_tb, 1);
      }   
   }
   

      
   /* update the cross hair group */
   SUMA_Init_SurfCont_CrossHair((SUMA_ALL_DO *)SO);
   
   /* set the colormap */
   SUMA_LH("Cmap time");
   if (SO->SurfCont->cmp_ren->cmap_context) {
      SUMA_LH("Rendering context not null");
      if (strcmp(SO->SurfCont->curColPlane->cmapname, "explicit") == 0 ||
          SUMA_is_Label_dset(SO->SurfCont->curColPlane->dset_link, NULL)) {
         SUMA_LH("Checking management");
         if (XtIsManaged(SO->SurfCont->DsetMap_fr)) {
            SUMA_LH("An RGB dset, or label dset no surface controls to be seen");
            XtUnmanageChild(SO->SurfCont->DsetMap_fr);
            XtUnmanageChild(XtParent(SO->SurfCont->DsetMap_fr));
         }else {
            SUMA_LH("An RGB dset, or label dset and no controls managed");
         }
      } else {
         if (!XtIsManaged(SO->SurfCont->DsetMap_fr)) {
            SUMA_LH( "A non RGB, and non label, dset \n"
                     "surface controls need to be seen\n"
                     "But only when ColPlane_fr is also shown \n"
                     "(frame may be hidden by Dsets button action)");
            if (XtIsManaged(SO->SurfCont->ColPlane_fr)) {
               XtManageChild(XtParent(SO->SurfCont->DsetMap_fr));
               XtManageChild(SO->SurfCont->DsetMap_fr);
            }
         }
         SUMA_LH( "Handling redisplay");
         SUMA_cmap_wid_handleRedisplay((XtPointer) SO); 

         SUMA_LH( "cmap options");
         /* set the widgets for dems mapping options */
         SUMA_set_cmap_options((SUMA_ALL_DO *)SO, YUP, NOPE);

         SUMA_LH( "cmap menu choice");
         /* set the menu to show the colormap used */
         SUMA_SetCmapMenuChoice((SUMA_ALL_DO *)SO, ColPlane->cmapname);

         SUMA_LH( "Dset col range");
         /* set the values for the threshold bar */
         if (SUMA_GetDsetColRange(  SO->SurfCont->curColPlane->dset_link, 
                                    SO->SurfCont->curColPlane->OptScl->tind, 
                                    range, loc)) {   
            SUMA_SetScaleRange((SUMA_ALL_DO *)SO, range );
         }
      }
   } else {
      SUMA_LH("cmap_context was NULL");
   }
   
   SUMA_LH("Returning");
   SUMA_RETURN (YUP);
}

/*!
   This function mirrors SUMA_InitializeColPlaneShell_SO
   but it is for graph datasets 
*/
SUMA_Boolean SUMA_InitializeColPlaneShell_GLDO (
                  SUMA_ALL_DO *ado, 
                  SUMA_OVERLAYS *ColPlane)
{
   static char FuncName[] = {"SUMA_InitializeColPlaneShell_GLDO"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   double range[2];
   int loc[2], i;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (!ado || !SurfCont) {
      SUMA_S_Err("NULL input, what gives?");
      SUMA_RETURN(NOPE);
   }
   
   if (!SurfCont->ColPlane_fr) {
      /* just set the curColPlane before returning ZSS  March 25 08*/
      if (ColPlane) SurfCont->curColPlane = ColPlane;
      SUMA_RETURN(YUP);
   }
   
   if (!ColPlane) {
      SUMA_LH("Initializing with NULL");
      SUMA_INSERT_CELL_STRING(SurfCont->ColPlaneLabelTable, 0, 1, "-");
      SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, "-");
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneDimFact->textfield,"-");
      SUMA_RETURN (YUP);
   } else {
      SUMA_LH("Initializing for real");
      
      SUMA_INSERT_CELL_STRING(SurfCont->ColPlaneLabelTable, 
                                 0, 1, ColPlane->Label);
   }
   
   SurfCont->curColPlane = ColPlane;   
   
   
   /* update the cross hair group */
   SUMA_Init_SurfCont_CrossHair(ado);
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SurfCont->SurfContPage->value = 
         SUMA_PageWidgetToNumber(SUMAg_CF->X->SC_Notebook, 
                                 SurfCont->Page); 
      SUMA_LHv("Setting PPpage to %d\n",
                    (int)SurfCont->SurfContPage->value);

      sprintf(sbuf,"%d",(int)SurfCont->SurfContPage->value);
      SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, sbuf);
   }
      
   /* set the colormap */
   SUMA_LH("Cmap time");
   if (SurfCont->cmp_ren->cmap_context) {
      SUMA_LH("Rendering context not null");
      if (strcmp(curColPlane->cmapname, "explicit") == 0 ||
          SUMA_is_Label_dset(curColPlane->dset_link, NULL)) {
         SUMA_LH("Checking management");
         if (XtIsManaged(SurfCont->DsetMap_fr)) {
            SUMA_LH("An RGB dset, or label dset no surface controls to be seen");
            XtUnmanageChild(SurfCont->DsetMap_fr);
            XtUnmanageChild(XtParent(SurfCont->DsetMap_fr));
         }else {
            SUMA_LH("An RGB dset, or label dset and no controls managed");
         }
      } else {
         if (!XtIsManaged(SurfCont->DsetMap_fr)) {
            SUMA_LH( "A non RGB, and non label, dset \n"
                     "surface controls need to be seen\n"
                     "But only when ColPlane_fr is also shown \n"
                     "(frame may be hidden by Dsets button action)");
            if (XtIsManaged(SurfCont->ColPlane_fr)) {
               XtManageChild(XtParent(SurfCont->DsetMap_fr));
               XtManageChild(SurfCont->DsetMap_fr);
            }
         }
         SUMA_LH( "Handling redisplay"); 
         SUMA_cmap_wid_handleRedisplay((XtPointer)ado); 

         SUMA_LH( "cmap options");
         /* set the widgets for dems mapping options */
         SUMA_set_cmap_options(ado, YUP, NOPE);

         SUMA_LH( "cmap menu choice");
         /* set the menu to show the colormap used */
         SUMA_SetCmapMenuChoice(ado, ColPlane->cmapname);

         /* set the values for the threshold bar */
         SUMA_LH( "dset range");
         if (SUMA_GetDsetColRange(  curColPlane->dset_link, 
                                    curColPlane->OptScl->tind, 
                                    range, loc)) {   
            SUMA_SetScaleRange(ado, range );
         }
      }
   } else {
      SUMA_LH("cmap_context was NULL");
   }
   
   SUMA_LH("Returning");
   SUMA_RETURN (YUP);
}

/*!
   This function mirrors SUMA_InitializeColPlaneShell_SO
   but it is for tracts  
*/
SUMA_Boolean SUMA_InitializeColPlaneShell_TDO (
                  SUMA_ALL_DO *ado, 
                  SUMA_OVERLAYS *ColPlane)
{
   static char FuncName[] = {"SUMA_InitializeColPlaneShell_TDO"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   double range[2];
   int loc[2], i;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_LH("Called with ColPlane %p", ColPlane);
      SUMA_DUMP_TRACE("Who called this?");
   }
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (!ado || !SurfCont) {
      SUMA_S_Err("NULL input, what gives?");
      SUMA_RETURN(NOPE);
   }
   
   if (!SurfCont->ColPlane_fr) {
      /* just set the curColPlane before returning ZSS  March 25 08*/
      if (ColPlane) SurfCont->curColPlane = ColPlane;
      SUMA_RETURN(YUP);
   }
   
   if (!ColPlane) {
      SUMA_LH("Initializing with NULL");
      SUMA_INSERT_CELL_STRING(SurfCont->ColPlaneLabelTable, 0, 1, "-");
      SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, "-");
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneDimFact->textfield,"-");
      SUMA_RETURN (YUP);
   } else {
      SUMA_LH("Initializing for real");
      
      SUMA_INSERT_CELL_STRING(SurfCont->ColPlaneLabelTable, 
                                 0, 1, ColPlane->Label);
   }
   
   SurfCont->curColPlane = ColPlane;   
   
   
   /* update the cross hair group */
   SUMA_Init_SurfCont_CrossHair(ado);
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SurfCont->SurfContPage->value = 
         SUMA_PageWidgetToNumber(SUMAg_CF->X->SC_Notebook, 
                                 SurfCont->Page); 
      SUMA_LHv("Setting PPpage to %d\n",
                    (int)SurfCont->SurfContPage->value);

      sprintf(sbuf,"%d",(int)SurfCont->SurfContPage->value);
      SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, sbuf);
   }
      
   /* set the colormap */
   SUMA_LH("Cmap time");
   if (SurfCont->cmp_ren->cmap_context) {
      SUMA_LH("Rendering context not null");
      if (strcmp(curColPlane->cmapname, "explicit") == 0 ||
          SUMA_is_Label_dset(curColPlane->dset_link, NULL)) {
         SUMA_LH("Checking management");
         if (XtIsManaged(SurfCont->DsetMap_fr)) {
            SUMA_LH("An RGB dset, or label dset no surface controls to be seen");
            XtUnmanageChild(SurfCont->DsetMap_fr);
            XtUnmanageChild(XtParent(SurfCont->DsetMap_fr));
         }else {
            SUMA_LH("An RGB dset, or label dset and no controls managed");
         }
      } else {
         if (!XtIsManaged(SurfCont->DsetMap_fr)) {
            SUMA_LH( "A non RGB, and non label, dset \n"
                     "surface controls need to be seen\n"
                     "But only when ColPlane_fr is also shown \n"
                     "(frame may be hidden by Dsets button action)");
            if (XtIsManaged(SurfCont->ColPlane_fr)) {
               XtManageChild(XtParent(SurfCont->DsetMap_fr));
               XtManageChild(SurfCont->DsetMap_fr);
            }
         }
         SUMA_LH( "Handling redisplay"); 
         SUMA_cmap_wid_handleRedisplay((XtPointer)ado); 

         SUMA_LH( "cmap options");
         /* set the widgets for dems mapping options */
         SUMA_set_cmap_options(ado, YUP, NOPE);

         SUMA_LH( "cmap menu choice");
         /* set the menu to show the colormap used */
         SUMA_SetCmapMenuChoice(ado, ColPlane->cmapname);

         /* set the values for the threshold bar */
         SUMA_LH( "dset range");
         if (SUMA_GetDsetColRange(  curColPlane->dset_link, 
                                    curColPlane->OptScl->tind, 
                                    range, loc)) {   
            SUMA_SetScaleRange(ado, range );
         }
      }
   } else {
      SUMA_LH("cmap_context was NULL");
   }
   
   SUMA_LH("Returning");
   SUMA_RETURN (YUP);
}

/*!
   This function mirrors SUMA_InitializeColPlaneShell_SO or _VO
   but it is for CIFTI objects  
*/
SUMA_Boolean SUMA_InitializeColPlaneShell_CO (
                  SUMA_ALL_DO *ado, 
                  SUMA_OVERLAYS *ColPlane)
{
   static char FuncName[] = {"SUMA_InitializeColPlaneShell_CO"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   double range[2];
   int loc[2], i;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called with ColPlane %p", ColPlane);
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (!ado || !SurfCont) {
      SUMA_S_Err("NULL input, what gives?");
      SUMA_RETURN(NOPE);
   }

   SUMA_S_Err("Place Holder, nothing yet. See comparable functions for "
              "_VO and _SO");
              
   SUMA_RETURN(NOPE);
}


/*!
   This function mirrors SUMA_InitializeColPlaneShell_SO
   but it is for volume objects  
*/
SUMA_Boolean SUMA_InitializeColPlaneShell_VO (
                  SUMA_ALL_DO *ado, 
                  SUMA_OVERLAYS *ColPlane)
{
   static char FuncName[] = {"SUMA_InitializeColPlaneShell_VO"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   double range[2];
   int loc[2], i;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called with ColPlane %p", ColPlane);
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (!ado || !SurfCont) {
      SUMA_S_Err("NULL input, what gives?");
      SUMA_RETURN(NOPE);
   }
   
   if (!SurfCont->ColPlane_fr) {
      /* just set the curColPlane before returning ZSS  March 25 08*/
      if (ColPlane) SurfCont->curColPlane = ColPlane;
      SUMA_RETURN(YUP);
   }
   
   if (!ColPlane) {
      SUMA_LH("Initializing with NULL");
      SUMA_INSERT_CELL_STRING(SurfCont->ColPlaneLabelTable, 0, 1, "-");
      SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, "-");
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneAlphaThresh->textfield,"-");
      SUMA_RETURN (YUP);
   } else {
      SUMA_LH("Initializing for real");
      
      SUMA_INSERT_CELL_STRING(SurfCont->ColPlaneLabelTable, 
                                 0, 1, ColPlane->Label);
      
      SurfCont->ColPlaneAlphaThresh->value = ColPlane->AlphaThresh;
      sprintf(sbuf,"%.1f", SurfCont->ColPlaneAlphaThresh->value);
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneAlphaThresh->textfield, sbuf);
   }
   
   SurfCont->curColPlane = ColPlane;   
   
   
   /* update the cross hair group */
   SUMA_Init_SurfCont_CrossHair(ado);
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SurfCont->SurfContPage->value = 
         SUMA_PageWidgetToNumber(SUMAg_CF->X->SC_Notebook, 
                                 SurfCont->Page); 
      SUMA_LHv("Setting PPpage to %d\n",
                    (int)SurfCont->SurfContPage->value);

      sprintf(sbuf,"%d",(int)SurfCont->SurfContPage->value);
      SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, sbuf);
   }
      
   /* set the colormap */
   SUMA_LH("Cmap time");
   if (SurfCont->cmp_ren->cmap_context) {
      SUMA_LH("Rendering context not null");
      if (strcmp(curColPlane->cmapname, "explicit") == 0 ||
          SUMA_is_Label_dset(curColPlane->dset_link, NULL)) {
         SUMA_LH("Checking management");
         if (XtIsManaged(SurfCont->DsetMap_fr)) {
            SUMA_LH("An RGB dset, or label dset no surface controls to be seen");
            XtUnmanageChild(SurfCont->DsetMap_fr);
            XtUnmanageChild(XtParent(SurfCont->DsetMap_fr));
         }else {
            SUMA_LH("An RGB dset, or label dset and no controls managed");
         }
      } else {
         if (!XtIsManaged(SurfCont->DsetMap_fr)) {
            SUMA_LH( "A non RGB, and non label, dset \n"
                     "surface controls need to be seen\n"
                     "But only when ColPlane_fr is also shown \n"
                     "(frame may be hidden by Dsets button action)");
            if (XtIsManaged(SurfCont->ColPlane_fr)) {
               XtManageChild(XtParent(SurfCont->DsetMap_fr));
               XtManageChild(SurfCont->DsetMap_fr);
            }
         }
         SUMA_LH( "Handling redisplay"); 
         SUMA_cmap_wid_handleRedisplay((XtPointer)ado); 

         SUMA_LH( "cmap options");
         /* set the widgets for dems mapping options */
         SUMA_set_cmap_options(ado, YUP, NOPE);

         SUMA_LH( "cmap menu choice");
         /* set the menu to show the colormap used */
         SUMA_SetCmapMenuChoice(ado, ColPlane->cmapname);

         /* set the values for the threshold bar */
         SUMA_LH( "dset range");
         if (SUMA_GetDsetColRange(  curColPlane->dset_link, 
                                    curColPlane->OptScl->tind, 
                                    range, loc)) {   
            SUMA_SetScaleRange(ado, range );
         }
      }
   } else {
      SUMA_LH("cmap_context was NULL");
   }
   
   SUMA_LH("Returning");
   SUMA_RETURN (YUP);
}

/*!
   This function mirrors SUMA_InitializeColPlaneShell_SO
   but it is for volume objects  
*/
SUMA_Boolean SUMA_InitializeColPlaneShell_MDO (
                  SUMA_ALL_DO *ado, 
                  SUMA_OVERLAYS *ColPlane)
{
   static char FuncName[] = {"SUMA_InitializeColPlaneShell_MDO"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   double range[2];
   int loc[2], i;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called with ColPlane %p", ColPlane);
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (!ado || !SurfCont) {
      SUMA_S_Err("NULL input, what gives?");
      SUMA_RETURN(NOPE);
   }
   
   if (!SurfCont->ColPlane_fr) {
      /* just set the curColPlane before returning ZSS  March 25 08*/
      if (ColPlane) SurfCont->curColPlane = ColPlane;
      SUMA_RETURN(YUP);
   }
   
   if (!ColPlane) {
      SUMA_LH("Initializing with NULL");
      SUMA_INSERT_CELL_STRING(SurfCont->ColPlaneLabelTable, 0, 1, "-");
      SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, "-");
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneDimFact->textfield,"-");
      SUMA_RETURN (YUP);
   } else {
      SUMA_LH("Initializing for real");
      
      SUMA_INSERT_CELL_STRING(SurfCont->ColPlaneLabelTable, 
                                 0, 1, ColPlane->Label);
   }
   
   SurfCont->curColPlane = ColPlane;   
   
   
   /* update the cross hair group */
   SUMA_Init_SurfCont_CrossHair(ado);
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SurfCont->SurfContPage->value = 
         SUMA_PageWidgetToNumber(SUMAg_CF->X->SC_Notebook, 
                                 SurfCont->Page); 
      SUMA_LHv("Setting PPpage to %d\n",
                    (int)SurfCont->SurfContPage->value);

      sprintf(sbuf,"%d",(int)SurfCont->SurfContPage->value);
      SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, sbuf);
   }
      
   /* set the colormap */
   SUMA_LH("Cmap time");
   if (SurfCont->cmp_ren->cmap_context) {
      SUMA_LH("Rendering context not null");
      if (strcmp(curColPlane->cmapname, "explicit") == 0 ||
          SUMA_is_Label_dset(curColPlane->dset_link, NULL)) {
         SUMA_LH("Checking management");
         if (XtIsManaged(SurfCont->DsetMap_fr)) {
            SUMA_LH("An RGB dset, or label dset no surface controls to be seen");
            XtUnmanageChild(SurfCont->DsetMap_fr);
            XtUnmanageChild(XtParent(SurfCont->DsetMap_fr));
         }else {
            SUMA_LH("An RGB dset, or label dset and no controls managed");
         }
      } else {
         if (!XtIsManaged(SurfCont->DsetMap_fr)) {
            SUMA_LH( "A non RGB, and non label, dset \n"
                     "surface controls need to be seen\n"
                     "But only when ColPlane_fr is also shown \n"
                     "(frame may be hidden by Dsets button action)");
            if (XtIsManaged(SurfCont->ColPlane_fr)) {
               XtManageChild(XtParent(SurfCont->DsetMap_fr));
               XtManageChild(SurfCont->DsetMap_fr);
            }
         }
         SUMA_LH( "Handling redisplay"); 
         SUMA_cmap_wid_handleRedisplay((XtPointer)ado); 

         SUMA_LH( "cmap options");
         /* set the widgets for dems mapping options */
         SUMA_set_cmap_options(ado, YUP, NOPE);

         SUMA_LH( "cmap menu choice");
         /* set the menu to show the colormap used */
         SUMA_SetCmapMenuChoice(ado, ColPlane->cmapname);

         /* set the values for the threshold bar */
         SUMA_LH( "dset range");
         if (SUMA_GetDsetColRange(  curColPlane->dset_link, 
                                    curColPlane->OptScl->tind, 
                                    range, loc)) {   
            SUMA_SetScaleRange(ado, range );
         }
      }
   } else {
      SUMA_LH("cmap_context was NULL");
   }
   
   SUMA_LH("Returning");
   SUMA_RETURN (YUP);
}

/*!
   \brief Updates color plane editing windows of surfaces related to SO, 
   if the color planes are open and displaying the same plane
   
*/
SUMA_Boolean SUMA_UpdateColPlaneShellAsNeeded(SUMA_ALL_DO *ado)
{
   static char FuncName[] = {"SUMA_UpdateColPlaneShellAsNeeded"};
   int i=-1;
   SUMA_SurfaceObject *SOi=NULL, *SO=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SurfCont = SUMA_ADO_Cont(ado);
   if (!SurfCont) SUMA_RETURN(NOPE);
   
   SUMA_LH("Called");
   switch (ado->do_type) {
      case SO_type:
         /* find out which surfaces are related to SO */
         SO = (SUMA_SurfaceObject *)ado;
         for (i=0; i<SUMAg_N_DOv; ++i) {
            if (SUMA_isSO(SUMAg_DOv[i])) {
               SOi = (SUMA_SurfaceObject *)SUMAg_DOv[i].OP;
               if (SOi != SO && SUMA_isRelated_SO (SOi, SO, 1)) { 
                  /* do this for kins of the 1st order */
                  if (SOi->SurfCont) {
                     if (  SOi->SurfCont != SO->SurfCont && 
                           SOi->SurfCont->ColPlane_fr && 
                           SOi->SurfCont->curColPlane == 
                                       SO->SurfCont->curColPlane) {
                        SUMA_InitializeColPlaneShell((SUMA_ALL_DO *)SOi, 
                                                     SOi->SurfCont->curColPlane);
                     }
                  }
               }
            }
         }
         break;
      case GDSET_type:
         SUMA_S_Warn("This should not happen in this modern day and age");
         break;
      case CDOM_type:
      case VO_type:
      case TRACT_type:
      case MASK_type:
      case GRAPH_LINK_type:
         /* Nothing should be done here because these objects don't
            have the types of kinship that surfaces have. Otherwise
            You end up resetting things like I range when you change
            the Dim value of tracts, graphs, or volumes */
         #if 0
         SUMA_InitializeColPlaneShell(ado, SurfCont->curColPlane);
         #endif
         break;
      default:
         SUMA_S_Errv("Nothing to do with %s\n",
               SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Creates the widgets for the DrawROI window
*/ 
void SUMA_CreateDrawROIWindow(void)
{
   static char FuncName[] = {"SUMA_CreateDrawROIWindow"};
   Widget rc, pb, rc_ur, rcv, rc_switch, rc_save, www;
   int i;
   char *sss, slabel[]={"Draw ROI"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (SUMAg_CF->X->DrawROI->AppShell) {
      SUMA_S_Err("SUMAg_CF->X->DrawROI->AppShell!=NULL. Should not be here.\n");
      SUMA_RETURNe;
   }
   
   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }
 
   SUMA_LH("Create AppShell");
   /* create as a separate application shell, 
   you do not want a parent to this controller that
   can be closed or withdrawn temporarily */
   SUMAg_CF->X->DrawROI->AppShell = XtVaAppCreateShell(sss , "Suma" ,
      topLevelShellWidgetClass , SUMAg_CF->X->DPY_controller1 ,
      XmNtitle, slabel,
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
   
SUMA_Register_Widget_Help( SUMAg_CF->X->DrawROI->AppShell , 0,
                                 "ROICont",
                                 "ROI Cont.",
"The ROI controller is for drawing ROIs on surfaces.:LR:\n"
":SPX:"
"You can launch the :ref:`Draw ROI Controller <ROICont>` with:"
" :ref:`ctrl+d <LC_Ctrl+d>` or :menuselection:`Tools-->Draw ROI`\n"
"\n"
".. figure:: media/ROICont.auto.ALL.jpg\n"
"   :align: center\n"
"   :name: media/ROICont.auto.ALL.jpg\n"
"\n"
"   :ref:`(link)<media/ROICont.auto.ALL.jpg>`\n"
"\n"
":DEF:"
"You can launch the Draw ROI Controller with:"
"\n'ctrl+d' or 'Tools-->Draw ROI'\n"
":SPX:"
"\n") ;

   /* create a form widget, manage it at the end ...*/
   SUMAg_CF->X->DrawROI->form = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SUMAg_CF->X->DrawROI->AppShell,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   /* a frame to put stuff in */
   SUMAg_CF->X->DrawROI->frame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, SUMAg_CF->X->DrawROI->form,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_FORM ,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL); 
   
   www = XtVaCreateManagedWidget ("ROI",
      xmLabelWidgetClass, SUMAg_CF->X->DrawROI->frame, 
      XmNchildType, XmFRAME_TITLE_CHILD,
      XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
      NULL);
   
   SUMA_Register_Widget_Help( www , 0,
                              "ROICont->ROI",
                              "ROI",
                  "Controls for drawing ROIs."
                  ":SPX:\n\n"
                  ".. figure:: media/ROICont.auto.ROI.jpg\n"
                  "   :align: right\n"
                  "   :name: media/ROICont.auto.ROI.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/ROICont.auto.ROI.jpg>`\n"
                  "\n"
                  ":SPX:") ;

   /* vertical row column to stack horizontal rcs in */
   rcv = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SUMAg_CF->X->DrawROI->frame,
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
   SUMAg_CF->X->DrawROI->ParentLabel_lb = 
      XtVaCreateManagedWidget ("Parent: N/A", 
            xmLabelWidgetClass, rc,
            NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->ParentLabel_lb, 1,
                             "ROICont->ROI->Parent",
                             "Label of the ROI's parent surface",
                             SUMA_DrawROI_ParentLabel_help ) ;   
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
   /* Turn on the ROI drawing mode, since that is what 
   the users want to do the first time they open this window */
   SUMAg_CF->ROI_mode = YUP; SUMA_ResetPrying(NULL);
   /* make a call to change the cursor */
   SUMA_UpdateAllViewerCursor(); 
   SUMAg_CF->X->DrawROI->DrawROImode_tb = XtVaCreateManagedWidget("Draw", 
      xmToggleButtonWidgetClass, rc, NULL);
   XmToggleButtonSetState (SUMAg_CF->X->DrawROI->DrawROImode_tb, 
                           SUMAg_CF->ROI_mode, NOPE);
   XtAddCallback (SUMAg_CF->X->DrawROI->DrawROImode_tb, 
                  XmNvalueChangedCallback, SUMA_cb_DrawROImode_toggled, 
                  NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->DrawROImode_tb , 1,
                     "ROICont->ROI->Draw",
                     "Toggles ROI drawing mode",
                     SUMA_DrawROI_DrawROIMode_help ) ;

   /* set the toggle button's select color */
   SUMA_SET_SELECT_COLOR(SUMAg_CF->X->DrawROI->DrawROImode_tb);
   
   
   /* put a toggle button for showing/hiding contours */
   SUMAg_CF->X->DrawROI->ContROImode_tb = 
               XtVaCreateManagedWidget("Cont.", 
                                       xmToggleButtonWidgetClass, rc, NULL);
   XmToggleButtonSetState (SUMAg_CF->X->DrawROI->ContROImode_tb,  
                           SUMAg_CF->ROI_contmode, NOPE);
   XtAddCallback (SUMAg_CF->X->DrawROI->ContROImode_tb, 
                  XmNvalueChangedCallback, SUMA_cb_ContROImode_toggled, 
                  NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->ContROImode_tb , 1,
                     "ROICont->ROI->Cont.",
                     "Toggles showing ROI contours",
                     SUMA_DrawROI_ContROIMode_help ) ;

   /* set the toggle button's select color */
   SUMA_SET_SELECT_COLOR(SUMAg_CF->X->DrawROI->ContROImode_tb);

   
   /*put a toggle button for the Pen mode */
   SUMAg_CF->X->DrawROI->Penmode_tb = 
      XtVaCreateManagedWidget("Pen", 
                              xmToggleButtonWidgetClass, rc, NULL);
   XmToggleButtonSetState (SUMAg_CF->X->DrawROI->Penmode_tb, 
                           SUMAg_CF->Pen_mode, NOPE);
   XtAddCallback (SUMAg_CF->X->DrawROI->Penmode_tb, 
                  XmNvalueChangedCallback, SUMA_cb_DrawROIPen_toggled, 
                  NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->Penmode_tb , 1,
                     "ROICont->ROI->Pen",
                     "Toggles Pen drawing mode",
                     SUMA_DrawROI_PenMode_help ) ;

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
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->AfniLink_tb , 1,
                     "ROICont->ROI->Afni",
                     "Toggles Link to Afni",
                     SUMA_DrawROI_AfniLink_help ) ;

   /* set the toggle button's select color */
   SUMA_SET_SELECT_COLOR(SUMAg_CF->X->DrawROI->AfniLink_tb);
   
   SUMA_LH("Building menu");
   /* put a menu for writing distance info */
   SUMA_BuildMenuReset(0);
   SUMAg_CF->X->DrawROI->WhatDistMenu = 
      SUMA_Alloc_Menu_Widget(SW_N_DrawROI_WhatDist);
   SUMA_BuildMenu (rc, XmMENU_OPTION, 
                   "Dist", '\0', YUP, DrawROI_WhatDist_Menu, 
                   "DoDist", 
                   "ROICont->ROI->Dist",
                   "Report length of drawn segments? (BHelp for more)", 
                   SUMA_DrawROI_WhatDist_help,   
                   SUMAg_CF->X->DrawROI->WhatDistMenu);
   XtManageChild (SUMAg_CF->X->DrawROI->WhatDistMenu->mw[SW_DrawROI_WhatDist]);
   
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
                           "ROICont->ROI->Label",
                           "Label of ROI being drawn", SUMA_DrawROI_Label_help,
                           SUMAg_CF->X->DrawROI->ROIlbl);
                        
   SUMA_CreateArrowField ( rc, "Value:",
                           1, 0, 999, 1,
                           3, SUMA_int,
                           NOPE,
                           SUMA_DrawROI_NewValue, NULL,
                           "ROICont->ROI->Value",
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
   XtAddCallback (SUMAg_CF->X->DrawROI->Undo_pb, 
                  XmNactivateCallback, SUMA_cb_DrawROI_Undo, NULL);   
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->Undo_pb, 1,
                             "ROICont->ROI->Undo",
                             "Undo the last action on the stack",
                             SUMA_DrawROI_Undo_help ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Undo_pb);
   
   SUMAg_CF->X->DrawROI->Redo_pb = XtVaCreateWidget ("Redo", 
      xmPushButtonWidgetClass, rc_ur, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Redo_pb, 
                  XmNactivateCallback, SUMA_cb_DrawROI_Redo, NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->Redo_pb , 1,
                             "ROICont->ROI->Redo",
                             "Redo the last undone action",
                             SUMA_DrawROI_Redo_help ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Redo_pb);
   
   XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc_ur, 
                              XmNorientation, XmVERTICAL,NULL);
   
   SUMAg_CF->X->DrawROI->Join_pb = XtVaCreateWidget ("Join", 
      xmPushButtonWidgetClass, rc_ur, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Join_pb, 
                  XmNactivateCallback, SUMA_cb_DrawROI_Join, NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->Join_pb , 1,
                             "ROICont->ROI->Join",
                             "Join the first node of the path to the last",
                             SUMA_DrawROI_Join_help ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Join_pb);
                                 
   SUMAg_CF->X->DrawROI->Finish_pb = XtVaCreateWidget ("Finish", 
      xmPushButtonWidgetClass, rc_ur, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Finish_pb, 
                  XmNactivateCallback, SUMA_cb_DrawROI_Finish, NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->Finish_pb , 1,
                             "ROICont->ROI->Finish",
                             "Label ROI as finished.",
                             SUMA_DrawROI_Finish_help ) ;
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
   SUMA_Register_Widget_Help(pb, 1,
                             "ROICont->ROI->Switch_ROI",
                             "Switch between ROIs.",
                             SUMA_DrawROI_SwitchROI_help ) ;
   XtManageChild (pb);
   
   SUMAg_CF->X->DrawROI->Load_pb = XtVaCreateWidget ("Load", 
      xmPushButtonWidgetClass, rc_switch, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Load_pb, 
                  XmNactivateCallback, SUMA_cb_DrawROI_Load, NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->Load_pb , 1,
                             "ROICont->ROI->Load",
                             "Load a Drawn ROI",
                             SUMA_DrawROI_Load_help ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Load_pb);
   
   XtVaCreateManagedWidget (  "sep", 
                           xmSeparatorWidgetClass, rc_switch, 
                           XmNorientation, XmVERTICAL,NULL);
                                                            
   SUMAg_CF->X->DrawROI->Delete_pb = XtVaCreateWidget ("delete ROI", 
      xmPushButtonWidgetClass, rc_switch, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Delete_pb, 
                  XmNactivateCallback, SUMA_cb_DrawROI_Delete, NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->Delete_pb , 1,
                             "ROICont->ROI->delete_ROI",
                             "Click twice in 5 seconds to delete ROI. "
                             "No Undo for this action.",
                             SUMA_DrawROI_DeleteROI_help);
   MCW_set_widget_bg( SUMAg_CF->X->DrawROI->Delete_pb , 
                      MCW_hotcolor(SUMAg_CF->X->DrawROI->Delete_pb) , 0 ) ;

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
   XtAddCallback (SUMAg_CF->X->DrawROI->Save_pb, 
                  XmNactivateCallback, SUMA_cb_DrawROI_Save, NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->Save_pb , 1,
                             "ROICont->ROI->Save",
                             "Save the Drawn ROI to disk.",
                             SUMA_DrawROI_Save_help ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Save_pb);

   /* Saving Mode */
   SUMA_BuildMenuReset(0);
   SUMAg_CF->X->DrawROI->SaveModeMenu = 
         SUMA_Alloc_Menu_Widget(SW_N_DrawROI_SaveMode);
   SUMA_BuildMenu (rc_save, XmMENU_OPTION, 
                               "", '\0', YUP, DrawROI_SaveMode_Menu, 
                               "Frm.",
                               "ROICont->ROI->Save->NIML",
                               "Format for saving ROI, "
                               "use NIML to preserve tracing order. "
                               "(BHelp or WHelp for more)", 
                               SUMA_DrawROI_SaveFormat_help, 
                               SUMAg_CF->X->DrawROI->SaveModeMenu);
   XtManageChild (SUMAg_CF->X->DrawROI->SaveModeMenu->mw[SW_DrawROI_SaveMode]);
      
   /* Saving what ? */
   SUMA_BuildMenuReset(0);
   SUMAg_CF->X->DrawROI->SaveWhatMenu = 
          SUMA_Alloc_Menu_Widget(SW_N_DrawROI_SaveWhat);
   SUMA_BuildMenu (rc_save, XmMENU_OPTION, 
                               "", '\0', YUP, DrawROI_SaveWhat_Menu, 
                               "What", 
                               "ROICont->ROI->Save->All",
                               "Which ROIs to save?", 
                               SUMA_DrawROI_SaveWhat_help,   
                               SUMAg_CF->X->DrawROI->SaveWhatMenu);
   XtManageChild (SUMAg_CF->X->DrawROI->SaveWhatMenu->mw[SW_DrawROI_SaveWhat]);
      

   XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc_save, 
                              XmNorientation, XmVERTICAL,NULL);

   pb = XtVaCreateWidget ("BHelp", 
      xmPushButtonWidgetClass, rc_save, 
      NULL);
   XtAddCallback (pb, XmNactivateCallback, MCW_click_help_CB, NULL);  
   SUMA_Register_Widget_Help(pb, 1, "ROICont->ROI->BHelp", 
         "Press this button then click on a button/label/menu for more help.",
                             SUMA_help_help ) ;
   XtManageChild (pb);
    
   pb = XtVaCreateWidget ("WHelp", 
      xmPushButtonWidgetClass, rc_save, 
      NULL);
   XtAddCallback (pb, XmNactivateCallback, SUMA_click_webhelp_CB, 
                  "ROICont->ROI->WHelp");  
   MCW_set_widget_bg( pb , MCW_buthighlight(pb) , 0 ) ;
   SUMA_Register_Widget_Help(pb, 1, "ROICont->ROI->WHelp",
         "Press this button then click on a button/label/menu for online help.",
                             SUMA_webhelp_help ) ;
   XtManageChild (pb);
    
   SUMAg_CF->X->DrawROI->Close_pb = XtVaCreateWidget ("Close", 
      xmPushButtonWidgetClass, rc_save, 
      NULL);   
   XtAddCallback (SUMAg_CF->X->DrawROI->Close_pb, XmNactivateCallback, 
                  SUMA_cb_CloseDrawROIWindow, NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->DrawROI->Close_pb, 1,
                             "ROICont->ROI->Close",
                             "Close Draw ROI window",
                             SUMA_closeDrawROI_help ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Close_pb);  
   
   /* manage rc_save */
   XtManageChild (rc_save);

   /* manage vertical rc */
   XtManageChild (rcv);
   
   /* manage frame */
   XtManageChild (SUMAg_CF->X->DrawROI->frame);
   
   /* manage form */
   XtManageChild (SUMAg_CF->X->DrawROI->form);

   /* position the widget relative to the first open viewer */
   SUMA_LHv("Position widgets (%d viewers)\n", SUMAg_N_SVv);
   i=0;
   while (i < SUMAg_N_SVv && 
         !SUMAg_SVv[i].X->ViewCont->TopLevelShell && SUMAg_SVv[i].isShaded) ++i; 

   if (i < SUMAg_N_SVv) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: i = %d\n", FuncName, i);
      SUMA_PositionWindowRelative ( SUMAg_CF->X->DrawROI->AppShell, 
                                    SUMAg_SVv[i].X->TOPLEVEL, SWP_TOP_RIGHT);
   }

   /* realize the widget */
   XtRealizeWidget (SUMAg_CF->X->DrawROI->AppShell);
   
   SUMA_LH("All done");
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
SUMA_Boolean SUMA_UpdateScrolledListData(SUMA_LIST_WIDGET *LW, 
            void *Default_Data, void *Select_Data, void *CloseList_Data) 
{
   static char FuncName[]={"SUMA_UpdateScrolledListData"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!LW) SUMA_RETURN(NOPE);
   if (!LW->toplevel) { /* no callbacks yet, just assign the data 
                           values to their positions in LW */
      LW->Default_Data = Default_Data;
      LW->Select_Data = Select_Data;
      LW->CloseList_Data = CloseList_Data; 
      SUMA_RETURN(YUP);
   }

   /* need to remove old callbacks before adding new ones 
      SEE ALSO SUMA_CreateScrolledList*/
   if (LW->Default_Data !=  Default_Data) {
      SUMA_LH("Doing Default Data..."); 
      if (!LW->Default_Data) {
         XtRemoveCallback(LW->list, XmNdefaultActionCallback, 
                           LW->Default_cb, (XtPointer)LW);
      } else {
         XtRemoveCallback (LW->list, XmNdefaultActionCallback, 
                           LW->Default_cb, (XtPointer)LW->Default_Data);
      }
      if (!Default_Data) {
         XtAddCallback (LW->list, XmNdefaultActionCallback, 
                        LW->Default_cb, (XtPointer)LW);
      } else {
         XtAddCallback (LW->list, XmNdefaultActionCallback, 
                        LW->Default_cb, (XtPointer)Default_Data);
      }
      LW->Default_Data =  Default_Data;
   }

   if (LW->Select_Data !=  Select_Data) { 
      SUMA_LH("Doing Select Data..."); 
      switch (LW->SelectPolicy){
         case SUMA_LSP_SINGLE:
            if (!LW->Select_Data) 
               XtRemoveCallback (LW->list, XmNsingleSelectionCallback, 
                                 LW->Select_cb, (XtPointer)LW);
            else
               XtRemoveCallback (LW->list, XmNsingleSelectionCallback,
                                 LW->Select_cb, (XtPointer)LW->Select_Data); 
            break;
         case SUMA_LSP_BROWSE:
            if (!LW->Select_Data) 
               XtRemoveCallback (LW->list, XmNbrowseSelectionCallback, 
                                 LW->Select_cb, (XtPointer)LW);
            else
               XtRemoveCallback (LW->list, XmNbrowseSelectionCallback, 
                                 LW->Select_cb, (XtPointer)LW->Select_Data); 

            break;
         case SUMA_LSP_MULTIPLE:
            if (!LW->Select_Data) 
               XtRemoveCallback (LW->list, XmNmultipleSelectionCallback, 
                                 LW->Select_cb, (XtPointer)LW);
            else
               XtRemoveCallback (LW->list, XmNmultipleSelectionCallback, 
                                 LW->Select_cb, (XtPointer)LW->Select_Data); 

            break;
         case SUMA_LSP_EXTENDED:
            if (!LW->Select_Data) 
               XtRemoveCallback (LW->list, XmNextendedSelectionCallback, 
                                 LW->Select_cb, (XtPointer)LW);
            else
               XtRemoveCallback (LW->list, XmNextendedSelectionCallback, 
                                 LW->Select_cb, (XtPointer)LW->Select_Data); 

            break;
         default:
            SUMA_SL_Err("Bad selection policy");
            SUMA_RETURN(NOPE);
            break;
      }

      switch (LW->SelectPolicy){
         case SUMA_LSP_SINGLE:
            if (!Select_Data) 
               XtAddCallback (LW->list, XmNsingleSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNsingleSelectionCallback, 
                              LW->Select_cb, (XtPointer)Select_Data); 
            break;
         case SUMA_LSP_BROWSE:
            if (!Select_Data) 
               XtAddCallback (LW->list, XmNbrowseSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNbrowseSelectionCallback, 
                              LW->Select_cb, (XtPointer)Select_Data); 

            break;
         case SUMA_LSP_MULTIPLE:
            if (!Select_Data) 
               XtAddCallback (LW->list, XmNmultipleSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNmultipleSelectionCallback, 
                              LW->Select_cb, (XtPointer)Select_Data); 

            break;
         case SUMA_LSP_EXTENDED:
            if (!Select_Data) 
               XtAddCallback (LW->list, XmNextendedSelectionCallback, 
                              LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNextendedSelectionCallback, 
                              LW->Select_cb, (XtPointer)Select_Data); 

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
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , 
                           "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW) ;
      } else {
         XmRemoveWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , 
                           "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW->CloseList_Data) ;
      }
      if (!CloseList_Data) {
         XmAddWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , 
                           "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW) ;
      } else {
         XmAddWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , 
                           "WM_DELETE_WINDOW" , False ) ,
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
      SUMA_SLP_Note ("No elements in list (such as ROIs) found");
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
      SUMA_LH("%s",clist[iclist]);
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


   /* set the vertical size of the list */
   XtVaSetValues( LW->list,                  
                  XmNvisibleItemCount, 10,
                  NULL);

   
   if (New) {
      XmListSetPos(LW->list,1);
      XmListSelectPos(LW->list,1, False); 
      /* realize the widget */
      XtRealizeWidget (LW->toplevel);
      /* To allow user to adjust list height, I would have to put in a callback 
      on the rc widget to reset XmNvisibleItemCount manually.
      Attempts to set  XmNresizeHeight for rc or toplevel did not work*/
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
void SUMA_CreateArrowField (  Widget pw, char *label,
                              float value, float vmin, 
                              float vmax, float vstep,
                              int cwidth, SUMA_VARTYPE type,
                              SUMA_Boolean wrap,
                              void (*NewValueCallback)(void *data), 
                              void *cb_data,
                              char *wname, char *hint, char *help,
                              SUMA_ARROW_TEXT_FIELD *AF)
{
   static char FuncName[]={"SUMA_CreateArrowField"};
   char sbuf[64], sss[256]={""};
   char *sh=NULL;
   
   SUMA_ENTRY;
   
   if (!AF) {
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                            "Bad value in text field", 
                            FuncName, SMT_Error, SMA_Log);
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
   
   if (hint || help) {
      SUMA_Register_Widget_Help( AF->rc , 1, wname, hint, help);
   }
      
   if (label) {
      AF->label =  XtVaCreateManagedWidget (label,
         xmLabelWidgetClass, AF->rc, 
         XmNmarginHeight, 0,
         XmNmarginTop, 0,
         XmNmarginBottom, 0,
         NULL);
      if (hint || help) {
         snprintf(sss, 255,"%s->label", wname);
         if (AF->type == SUMA_int || AF->type == SUMA_float) {
            sh = SUMA_append_replace_string(help,
              "For numeric arrow fields, you can also change values by scrolling"
              "with the pointer over the text field.", ":LR:\n",0);
         } else {
            sh = help;
         }
         SUMA_Register_Widget_Help( AF->label , 1, sss, NULL, help?help:hint);
         if (sh != help) { SUMA_ifree(sh); }
      }
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
   if (hint || help) {
      snprintf(sss, 255,"%s->up", wname);
      if (AF->type == SUMA_int || AF->type == SUMA_float) {
         sh = SUMA_append_replace_string(help,
            "For numeric arrow fields, you can also change values by scrolling"
            "with the pointer over the text field.", ":LR:\n",0);
      } else {
         sh = help;
      }
      SUMA_Register_Widget_Help( AF->up , 1, sss, NULL, help?help:hint);
      if (sh != help) { SUMA_ifree(sh); }
   }

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
   if (hint || help) {
      snprintf(sss, 255,"%s->down", wname);
      if (AF->type == SUMA_int || AF->type == SUMA_float) {
         sh = SUMA_append_replace_string(help,
            "For numeric arrow fields, you can also change values by scrolling"
            "with the pointer over the text field.", ":LR:\n",0);
      } else {
         sh = help;
      }
      SUMA_Register_Widget_Help( AF->down , 1, sss, NULL, help?help:hint);
      if (sh != help) { SUMA_ifree(sh); }
   }
   XtVaSetValues (AF->down, XmNuserData, (XtPointer)AF, NULL);
   XtAddCallback (AF->down, XmNarmCallback, SUMA_ATF_start_stop, (XtPointer)-1);
   XtAddCallback (AF->down, XmNdisarmCallback, 
                              SUMA_ATF_start_stop, (XtPointer)-1);

   SUMA_ATF_GetString(AF, sbuf);
   AF->textfield = XtVaCreateManagedWidget ("label",
      xmTextFieldWidgetClass, AF->rc,
      XmNuserData, (XtPointer)AF,
      XmNvalue, sbuf,
      XmNcolumns, AF->cwidth,
      XmNmarginHeight, 0,
      XmNmarginTop, 0,
      XmNmarginBottom, 0,
      NULL);
   
   if (hint || help) {
      snprintf(sss, 255,"%s->val", wname);
      if (AF->type == SUMA_int || AF->type == SUMA_float) {
         sh = SUMA_append_replace_string(help,
            "For numeric arrow fields, you can also change values by scrolling"
            "with the pointer over the text field.", ":LR:\n",0);
      } else {
         sh = help;
      }
      SUMA_Register_Widget_Help( AF->textfield , 1, sss, NULL, sh?sh:hint);
      if (sh != help) { SUMA_ifree(sh); }
   }
   
   XtAddCallback (AF->textfield, XmNactivateCallback, 
                                    SUMA_ATF_cb_label_change, (XtPointer)AF);
   XtAddCallback (AF->textfield, XmNmodifyVerifyCallback, 
                                    SUMA_ATF_cb_label_Modify, (XtPointer)AF);
   
   /* add event handler to nitify when widget was left */
   XtInsertEventHandler( AF->textfield ,        /* notify when */
                         LeaveWindowMask ,  /* pointer leaves */
                         FALSE ,            /* this window */
                         SUMA_leave_EV,
                         (XtPointer) AF ,
                         XtListTail ) ;     /* last in queue */      
   XtInsertEventHandler( AF->textfield  ,      /* handle events in cell */
                         ButtonPressMask ,  /* button presses */
                         FALSE ,            /* nonmaskable events? */
                         SUMA_press_EV,  /* handler */
                         (XtPointer) AF ,   /* client data */
                         XtListTail ) ;
   XtManageChild (AF->rc);
   SUMA_RETURNe;
}


/*! \brief Sets the GUI menu selection to the ith selection
           for a menu created by SUMA_BuildMenu. i starts at 1 */
SUMA_Boolean SUMA_Set_Menu_Widget(SUMA_MENU_WIDGET *men, int i)
{
   static char FuncName[]={"SUMA_Set_Menu_Widget"};
   char stmp[64]={""};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (i<1) { SUMA_S_Err("i must be >=1"); SUMA_RETURN(NOPE);   }
   if (!men) { SUMA_DUMP_TRACE("NULL widget struct"); SUMA_RETURN(NOPE); }
   if(men->menu_type == SUMA_XmArrowFieldMenu) {
      if (!men->af || !men->af->textfield) {
         SUMA_S_Err("Null AF for arrow field menu!");  SUMA_RETURN(NOPE);
      }
      sprintf(stmp,"%d", i-1);
      SUMA_LHv("Arrows: stmp=%s, textfield=%p\n", stmp, men->af->textfield);
      XtVaSetValues( men->af->textfield, XmNvalue, stmp, NULL); 
      men->af->value = i-1;
   } else {
      SUMA_LHv("Non arrows, %p, men->N_mw %d, i=%d\n", men->mw, men->N_mw, i);
      if (!men->mw || i>=men->N_mw || !men->mw[i]) {
         /* you can get here with i>=men->N_mw if controller has not been 
           open yet, so return without complaints. */
         /* SUMA_S_Err("Null menu widgets or bad index for menu!");  
         SUMA_RETURN(NOPE); */
         SUMA_RETURN(YUP);
      } 
      XtVaSetValues(  men->mw[0], XmNmenuHistory ,  men->mw[i], NULL);
   } 
   
   SUMA_RETURN(YUP);
}

/*! 
   creates a text field.
   
   \sa SUMA_CreateArrowField 
*/
void SUMA_CreateTextField ( Widget pw, char *label,
                              int cwidth, 
                              void (*NewValueCallback)(void *data),
                              char *wname, char *hint, char *help,
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
   
   if (hint || help) {
      SUMA_Register_Widget_Help( AF->rc , 1, wname, hint, help);
   }
      

   if (label) {
      AF->label =  XtVaCreateManagedWidget (label,
         xmLabelWidgetClass, AF->rc, 
         XmNmarginHeight, 0,
         XmNmarginTop, 0,
         XmNmarginBottom, 0,
         NULL);
      if (hint || help) {
         SUMA_Register_Widget_Help( AF->label , 1, wname, hint, help);
      }
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
   if (hint || help) {
      SUMA_Register_Widget_Help( AF->textfield , 1, wname, hint, help);
   }
   
   XtAddCallback (AF->textfield, XmNactivateCallback, 
                  SUMA_ATF_cb_label_change, (XtPointer)AF);
   XtAddCallback (AF->textfield, XmNmodifyVerifyCallback, 
                  SUMA_ATF_cb_label_Modify, (XtPointer)AF);
   
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

   SUMA_LH("Called");
   AF = (SUMA_ARROW_TEXT_FIELD *)client_data ;
   if( lev->type != LeaveNotify || !AF->modified ) SUMA_RETURNe; 
   
   SUMA_LH("Leave notification");
   SUMA_ATF_cb_label_change( AF->textfield , (XtPointer) AF , NULL ) ;
   
   SUMA_RETURNe;
}

/*!
   \brief This function is called when button presses happen in arrow text field 
*/
void SUMA_press_EV( Widget w , XtPointer client_data ,
                    XEvent * ev , Boolean * continue_to_dispatch )
{
   static char FuncName[]={"SUMA_press_EV"};
   SUMA_ARROW_TEXT_FIELD *AF=NULL; 
   XButtonEvent * bev = (XButtonEvent *) ev ;
   XmAnyCallbackStruct cbs ;
   int incr=0, ival=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_LH("Called");
   AF = (SUMA_ARROW_TEXT_FIELD *)client_data ;

   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ) {
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || AF == NULL ) { SUMA_RETURNe ; }
      
   incr = 0;
   switch (bev->button) {
      case Button1:
         SUMA_LH("Button 1");
         break;
      case Button2:
         SUMA_LH("Button 2");
         break;
      case Button3:
         SUMA_LH("Button 3");
         break;
      case Button4:
      case 6:  /* This is shift and wheel on mac, Button6 is not in X.h ! */
         SUMA_LH("Button 4/6 %d", bev->button);
         incr = -1;
         break;
      case Button5:
      case 7: 
         SUMA_LH("Button 5/7 %d", bev->button);
         incr = 1;
         break;
      default:
         SUMA_RETURNe;
   }
      
   if (incr) {
      AF->direction = incr;
      ival = AF->value;
      SUMA_ATF_change_value (AF, NULL );
      if (ival != AF->value) {
         if (!AF->NewValueCallbackData) 
               AF->NewValueCallback((void*)AF);
         else 
               AF->NewValueCallback(AF->NewValueCallbackData);
      }
   }
    
   SUMA_RETURNe;
}


/*!
   \brief This function is called when the label field is activated by the user
   
*/
void SUMA_ATF_cb_label_change (Widget w, XtPointer client_data, 
                               XtPointer call_data)
{
   static char FuncName[]={"SUMA_ATF_cb_label_change"};
   SUMA_ARROW_TEXT_FIELD *AF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* make call to NewValue callback */
   AF = (SUMA_ARROW_TEXT_FIELD *)client_data;
   SUMA_LHv("Type %d (%d int, %d float\n", AF->type, SUMA_int, SUMA_float);
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
   int incr = (INT_CAST) client_data;
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
      if (!ErrCnt) 
         SUMA_SLP_Err ( "ROI is marked as finished.\n"
                        "New value will not be applied.\n");
      ++ErrCnt;
      AF->value = (float)DrawnROI->iLabel;
      SUMA_ATF_SetString (AF); 
   }
   
   /* if your colors are based on the label, you've got work to do*/
   if (DrawnROI->ColorByLabel) {
      SUMA_SurfaceObject *SO=NULL;
      /* Now update the Paint job on the ROI plane */
      SO = SUMA_findSOp_inDOv (DrawnROI->Parent_idcode_str, 
                               SUMAg_DOv, SUMAg_N_DOv);
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
      SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                          SES_SumaWidget, NULL);
      if (!SUMA_Engine (&list)) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
         SUMA_RETURNe;    
      }
   }   
   SUMA_RETURNe;
}

int SUMA_ColPlane_NewOrder(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                               int neworder, int cb_direct) 
{
   static char FuncName[]={"SUMA_ColPlane_NewOrder"};
   SUMA_SurfaceObject *SOC=NULL, *SO=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err("Will need to switch current plane. Not ready for this");
      SUMA_RETURN(0);      
   }
   
   if (!SUMA_ColPlane_NewOrder_one(ado, colp, neworder, cb_direct)) {
      SUMA_S_Err("Returning on a sad note");
      SUMA_RETURN(0);
   }
   
   if (ado->do_type == SO_type) {
      SO = (SUMA_SurfaceObject *)ado;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (SOC->SurfCont->curColPlane != colpC) {
            SUMA_S_Err("Don't have contralateral as cur colplane.\n"
                       "This should not happen under L/R yoked conditions.");
            SUMA_RETURN(0);
         }
         if (!SUMA_ColPlane_NewOrder_one((SUMA_ALL_DO *)SOC, colpC, 
                                         neworder, 0)) {
            SUMA_S_Warn("Failed in contralateralization");
         }
      } else {
         SUMA_LHv("Found NO contralateral equivalent to:\n"
                      " %s and %s in\n",
                      SO->Label, CHECK_NULL_STR(colp->Label));
      }
   }

   
   SUMA_RETURN(1);
}

int SUMA_ColPlane_NewOrder_one(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                               int neworder, int cb_direct) 
{
   static char FuncName[]={"SUMA_ColPlane_NewOrder_one"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   int Old_Order = -1, i, iMove, NetMove;
   SUMA_Boolean Shaded, Decent; 
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   
   curColPlane = SUMA_ADO_CurColPlane(ado);

   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err("Will need to switch current plane. Not ready for this");
      SUMA_RETURN(0);      
   }
   
   /* Now show the new order */
   if (LocalHead) SUMA_Print_PlaneOrder(ado, NULL);
   

   /* Now figure out the direction of the arrow presses */
   NetMove = (int)neworder - 
                  curColPlane->PlaneOrder ; 
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s:  Net move %d\n", FuncName, NetMove);
   iMove = 0;
   Decent = YUP;
   if (NetMove > 0) {   
      do {
         Old_Order = curColPlane->PlaneOrder;
         if (!SUMA_MovePlaneUp(ado, curColPlane->Name)) {
            SUMA_L_Err("Error in SUMA_MovePlaneUp.");
            SUMA_RETURN(0);
         }
         
         if (curColPlane->PlaneOrder == Old_Order) {
            SUMA_LH("Nothing can be done");
            Decent = NOPE;
         } else {
            ++iMove;
         } 
      } while (iMove < NetMove && Decent);
   } else if (NetMove < 0) {
      do {
         Old_Order = curColPlane->PlaneOrder;
         if (!SUMA_MovePlaneDown(ado, curColPlane->Name)) {
            SUMA_L_Err("Error in SUMA_MovePlaneDown.");
            SUMA_RETURN(0);
         }
         if (curColPlane->PlaneOrder == Old_Order) {
            SUMA_LH("Enough");
            Decent = NOPE;
         } else {
            ++iMove;
         } 
      } while (iMove < -NetMove && Decent);   
   } else {
      Decent = NOPE;
   }
   
   SUMA_LH("Out");
   /* Now show the new order */
   if(LocalHead) SUMA_Print_PlaneOrder(ado, NULL);
   
   /* refresh the switch list */
   SUMA_IS_SWITCH_COL_PLANE_SHADED(ado, Shaded);
   if (!Shaded) {
      SUMA_LH("Refreshing Col Plane List");
      SUMA_RefreshDsetList (ado);
   }

   if (!Decent) {
      /* reset order value in widget to its last acceptable value. */
      sprintf(sbuf,"%d", curColPlane->PlaneOrder);
      neworder = curColPlane->PlaneOrder;
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneOrder->textfield, sbuf); 
   }
   
   SUMA_UpdateColPlaneShellAsNeeded(ado); /* update other open ColPlaneShells */

   if (iMove > 0) { /* something decent was done, act on it */
      /* a good remix and redisplay */
      SUMA_LH("Remix and redisplay");
      SUMA_Remixedisplay (ado);
   }
   
   if (!cb_direct && neworder != (int)SurfCont->ColPlaneOrder->value) {
      /* force gui match */
      sprintf(sbuf,"%d", neworder);
      SurfCont->ColPlaneOrder->value = neworder;
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneOrder->textfield, sbuf); 
   }
   
   SUMA_RETURN(1);
}

/*!
   \brief Function to update the order of a colorplane 

   -expects ADO in data
*/
void SUMA_cb_ColPlane_NewOrder (void *data)
{
   static char FuncName[]={"SUMA_cb_ColPlane_NewOrder"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))
            || !SurfCont->ColPlaneOrder) SUMA_RETURNe;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
      
   /* make sure a new order is in order */
   if (curColPlane->PlaneOrder == 
         (int)SurfCont->ColPlaneOrder->value) SUMA_RETURNe;
   
   SUMA_ColPlane_NewOrder(ado, NULL, 
                              (int)SurfCont->ColPlaneOrder->value, 1);
   
   SUMA_RETURNe;
}

/*!
   \brief Function to change pages in notebook 

   -expects ADO in data
*/
void SUMA_cb_SurfCont_SwitchPage (void *data)
{
   static char FuncName[]={"SUMA_cb_SurfCont_SwitchPage"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))
            || !SurfCont->SurfContPage) SUMA_RETURNe;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   SUMA_LHv("About to change page to %d\n", (int)SurfCont->SurfContPage->value);
   if (!(SUMA_SetSurfContPageNumber(SUMAg_CF->X->SC_Notebook, 
                                    SurfCont->SurfContPage->value))) {
      /* revert to good value */
      SurfCont->SurfContPage->value = 
               SUMA_PageWidgetToNumber(SUMAg_CF->X->SC_Notebook, SurfCont->Page);
      sprintf(sbuf,"%d",(int)SurfCont->SurfContPage->value);
      SUMA_SET_TEXT_FIELD(SurfCont->SurfContPage->textfield, sbuf);
      SUMA_LHv("Problem, reverting to %d\n",
               (int)SurfCont->SurfContPage->value);
   } 
   
   SUMA_RETURNe;
}

/*!
   \brief Initialize controllers for all objects that would have one
   
   -expects nothing
*/
void SUMA_cb_AllConts(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_AllConts"};
   SUMA_ALL_DO *ado=NULL;
   int ido, new = 0;
   
   SUMA_ENTRY;
   /* For a large number of objects, say 90, this function
      can take a very long (2-3 mins) time to finsh. The reason
      for this latency is that a very large number of events end up
      in the queue with each new controller opened. The process takes longer
      and longer with each new controller opened. 
      The best way around this is to force X11 to discard all the 
      generated events and just refresh the display of the last 
      controller opened. The update will then take place once
      you switch the the controller of interest.
      
      Note that MCW_invert_widget was also calling XSync() and XmUpdatedisplay()
      so I have had to call a variant that does not force these calls.
      
      Note again, the delay still increases with increasing objects but
      for now this remaining slowing down is not worth pursuing.
       
                                       ZSS Snowed in, Feb. 2015 */
   XSync( XtDisplay(w) , False ) ; /* Be nice and tidy up before plunge 
                                      We will drop all remaining events later*/
   for (ido=0; ido<SUMAg_N_DOv; ++ido) {
      ado = (SUMA_ALL_DO *)SUMAg_DOv[ido].OP;
      if (SUMA_ADO_Cont(ado) && !SUMA_isADO_Cont_Realized(ado)) {
         ++new;
         SUMA_viewSurfaceCont(NULL, ado, NULL);
      }
   }
   if (new > 10) { /* don't bother unless we have had too many newbies */
      XSync( XtDisplay(w) , True ) ; /* get rid of all pending events */
      /* Now repeat call to last ado's viewer to update its widgets */
      if (new) SUMA_SetSurfContPageNumber(SUMAg_CF->X->SC_Notebook, 1);   
   }
   SUMA_RETURNe;
}

/*!
   \brief Function to update the opacity of a colorplane 
   
   -expects SO in data
*/
void SUMA_cb_ColPlane_NewOpacity(void *data)
{
   static char FuncName[]={"SUMA_cb_ColPlane_NewOpacity"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))
            || !SurfCont->ColPlaneOpacity) SUMA_RETURNe;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (SurfCont->ColPlaneOpacity->value == 
       curColPlane->GlobalOpacity) SUMA_RETURNe;
   
   SUMA_ColPlane_NewOpacity(ado, NULL, SurfCont->ColPlaneOpacity->value, 1);
   
   SUMA_RETURNe;
}

int SUMA_ColPlane_NewOpacity (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                               float newopacity, int cb_direct )
{
   static char FuncName[]={"SUMA_ColPlane_NewOpacity"};
   SUMA_SurfaceObject *SOC=NULL, *SO=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err( "Will need to switch to current plane first. "
                  "Not ready for this");
      SUMA_RETURN(0);      
   }
   
   if (!SUMA_ColPlane_NewOpacity_one(ado, colp, newopacity, cb_direct)) {
      SUMA_S_Err("Returning on a sad note");
      SUMA_RETURN(0);
   }
   
   if (ado->do_type == SO_type) {
      SO = (SUMA_SurfaceObject *)ado;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (SOC->SurfCont->curColPlane != colpC) {
            SUMA_S_Err("Don't have contralateral as cur colplane.\n"
                       "This should not happen under L/R yoked conditions.");
            SUMA_RETURN(0);
         }
         if (!SUMA_ColPlane_NewOpacity_one((SUMA_ALL_DO *)SOC, colpC, 
                                         newopacity, 0)) {
            SUMA_S_Warn("Failed in contralateralization");
         }
      } else {
         SUMA_LHv("Found NO contralateral equivalent to:\n"
                      " %s and %s in\n",
                      SO->Label, CHECK_NULL_STR(colp->Label));
      }
   }
   
   SUMA_RETURN(1);
}

int SUMA_ColPlane_NewOpacity_one(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                                 float newopacity, int cb_direct) 
{
   static char FuncName[]={"SUMA_ColPlane_NewOpacity_one"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err("Will need to switch current plane. Not ready for this");
      SUMA_RETURN(0);      
   }
   
   /* change the value of the global opacity */
   curColPlane->GlobalOpacity = newopacity;   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: GlobalOpacity of %s set to %f.\n", 
         FuncName, curColPlane->Name, 
         curColPlane->GlobalOpacity);
   
   SUMA_UpdateColPlaneShellAsNeeded(ado); /* update other open ColPlaneShells */

   /* a good remix and redisplay */
   SUMA_Remixedisplay (ado);
   
   if (!cb_direct && newopacity != SurfCont->ColPlaneOpacity->value) {
      /* force gui match */
      sprintf(sbuf,"%.2f", newopacity);
      SurfCont->ColPlaneOpacity->value = newopacity;
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneOpacity->textfield, sbuf); 
   }
   
   SUMA_RETURN(1);
}

void SUMA_cb_Tract_NewGray(void *data)
{
   static char FuncName[]={"SUMA_cb_Tract_NewGray"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_TRACT_SAUX *TSaux=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(TSaux=SUMA_ADO_TSaux(ado)) || 
       !(SurfCont = SUMA_ADO_Cont(ado))) SUMA_RETURNe;
   
   if (SurfCont->TractMaskGray->value == 
       TSaux->MaskGray) SUMA_RETURNe;
   
   SUMA_Tract_NewGray(ado, SurfCont->TractMaskGray->value, 1);
   
   SUMA_RETURNe;
}

int SUMA_Tract_NewGray (SUMA_ALL_DO *ado, 
                           float newgray, int cb_direct )
{
   static char FuncName[]={"SUMA_Tract_NewGray"};
   SUMA_TractDO *tdo = (SUMA_TractDO*)ado;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_TRACT_SAUX *TSaux=NULL;
   char sbuf[32];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(TSaux = SUMA_ADO_TSaux(ado)) ||
       !(SurfCont = SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
      
   /* change the value of the masking mode */
   TSaux->MaskGray = newgray;   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Newgray of %s set to %f.\n", 
         FuncName, ADO_LABEL(ado), 
         TSaux->MaskGray);
   
   /* a good remix and redisplay */
   SUMA_Remixedisplay (ado);
   
   if (!cb_direct && newgray != SurfCont->TractMaskGray->value) {
      /* force gui match */
      sprintf(sbuf,"%.2f", newgray);
      SurfCont->TractMaskGray->value = newgray;
      SUMA_SET_TEXT_FIELD(SurfCont->TractMaskGray->textfield, sbuf); 
   }
   
   SUMA_RETURN(1);
}

/*!
   \brief Function to update the DimFact of a colorplane 
   DimFact is the same as BrightFact which is not defined
   for explicitly colored planes 
   -expects SO in data
*/
void SUMA_cb_ColPlane_NewDimFact (void *data)
{
   static char FuncName[]={"SUMA_cb_ColPlane_NewDimFact"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado)) 
            || !SurfCont->curColPlane ) SUMA_RETURNe;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (curColPlane->DimFact == 
       SurfCont->ColPlaneDimFact->value) SUMA_RETURNe;
   
   SUMA_ColPlane_NewDimFact(ado, curColPlane, 
                            SurfCont->ColPlaneDimFact->value, 1);
   SUMA_RETURNe;                            
}

int SUMA_ColPlane_NewDimFact (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                              float newdimfact, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlane_NewDimFact"};
   SUMA_SurfaceObject *SOC=NULL, *SO=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err( "Will need to switch to current plane first. "
                  "Not ready for this");
      SUMA_RETURN(0);      
   }
   
   if (!SUMA_ColPlane_NewDimFact_one(ado, colp, newdimfact, cb_direct)) {
      SUMA_S_Err("Non son pagliaccio");
      SUMA_RETURN(0);
   }
   
   if (ado->do_type == SO_type) {
      SO = (SUMA_SurfaceObject *)ado;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (SOC->SurfCont->curColPlane != colpC) {
            SUMA_S_Err("Don't have contralateral as cur colplane.\n"
                       "This should not happen under L/R yoked conditions.");
            SUMA_RETURN(0);
         }
         if (!SUMA_ColPlane_NewDimFact_one((SUMA_ALL_DO *)SOC, colpC, 
                                           newdimfact, 0)) {
            SUMA_S_Warn("Failed in contralateralization");
         }
      } else {
         SUMA_LHv("Found NO contralateral equivalent to:\n"
                      " %s and %s in\n",
                      SO->Label, CHECK_NULL_STR(colp->Label));
      }
   }
   SUMA_RETURN(1);
}

int SUMA_ColPlane_NewDimFact_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                                  float newdimfact, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlane_NewDimFact_one"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);   
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err("Will need to switch current plane. Not ready for this");
      SUMA_RETURN(0);      
   }

   /* change the value of the dimfact */
   curColPlane->DimFact = newdimfact; 
   if (curColPlane->OptScl) 
      curColPlane->OptScl->BrightFact = 
                                 curColPlane->DimFact;
      
   if (LocalHead) fprintf(SUMA_STDERR,"%s: DimFact of %s set to %f.\n", 
         FuncName, curColPlane->Name, 
         curColPlane->DimFact);
   
   SUMA_UpdateColPlaneShellAsNeeded(ado); /* update other open ColPlaneShells */

   /* need to colorize plane */
   SUMA_ColorizePlane(curColPlane);
   
   /* a good remix and redisplay */
   SUMA_Remixedisplay (ado);
   
   /* update color label */
   SUMA_UpdateNodeLblField(ado);
   
   if (!cb_direct && newdimfact != SurfCont->ColPlaneDimFact->value) {
      /* force gui match */
      sprintf(sbuf,"%.2f", newdimfact);
      SurfCont->ColPlaneDimFact->value = newdimfact;
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneDimFact->textfield, sbuf); 
   }
  
   SUMA_RETURN(1);
}
/*!
   \brief Function to update the AlphaThresh of a colorplane 
   AlphaThresh is only valued for volume objects, at the moment 
   -expects VO in data
*/
void SUMA_cb_ColPlane_NewAlphaThresh (void *data)
{
   static char FuncName[]={"SUMA_cb_ColPlane_NewAlphaThresh"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado)) 
            || !SurfCont->curColPlane ) SUMA_RETURNe;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (curColPlane->AlphaThresh == 
       SurfCont->ColPlaneAlphaThresh->value) SUMA_RETURNe;
   
   SUMA_ColPlane_NewAlphaThresh(ado, curColPlane, 
                            SurfCont->ColPlaneAlphaThresh->value, 1);
   SUMA_RETURNe;                            
}

int SUMA_ColPlane_NewAlphaThresh (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                              float newAlphaThresh, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlane_NewAlphaThresh"};
   SUMA_SurfaceObject *SOC=NULL, *SO=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err( "Will need to switch to current plane first. "
                  "Not ready for this");
      SUMA_RETURN(0);      
   }
   
   if (!SUMA_ColPlane_NewAlphaThresh_one(ado, colp, newAlphaThresh, cb_direct)) {
      SUMA_S_Err("Non son pagliaccio");
      SUMA_RETURN(0);
   }
   
   if (ado->do_type == SO_type) {
      SO = (SUMA_SurfaceObject *)ado;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (SOC->SurfCont->curColPlane != colpC) {
            SUMA_S_Err("Don't have contralateral as cur colplane.\n"
                       "This should not happen under L/R yoked conditions.");
            SUMA_RETURN(0);
         }
         if (!SUMA_ColPlane_NewAlphaThresh_one((SUMA_ALL_DO *)SOC, colpC, 
                                           newAlphaThresh, 0)) {
            SUMA_S_Warn("Failed in contralateralization");
         }
      } else {
         SUMA_LHv("Found NO contralateral equivalent to:\n"
                      " %s and %s in\n",
                      SO->Label, CHECK_NULL_STR(colp->Label));
      }
   }
   SUMA_RETURN(1);
}

int SUMA_ColPlane_NewAlphaThresh_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                                  float newAlphaThresh, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlane_NewAlphaThresh_one"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);   
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err("Will need to switch current plane. Not ready for this");
      SUMA_RETURN(0);      
   }

   /* change the value of the AlphaThresh */
   curColPlane->AlphaThresh = newAlphaThresh; 
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: AlphaThresh of %s set to %f.\n", 
         FuncName, curColPlane->Name, 
         curColPlane->AlphaThresh);
   
   SUMA_UpdateColPlaneShellAsNeeded(ado); /* update other open ColPlaneShells */

   /* NO need to colorize plane. That is the whole reason for using AlphaThresh
      rather than the threshold slider ! 
      Colorizing the volume is taxing because one has to reload the volume
      into the texture memory.... */
   /* SUMA_ColorizePlane(curColPlane); */
   
   /* a good remix and redisplay */
   SUMA_Remixedisplay (ado);
   
   /* update color label */
   SUMA_UpdateNodeLblField(ado);
   
   if (!cb_direct && newAlphaThresh != SurfCont->ColPlaneAlphaThresh->value) {
      /* force gui match */
      sprintf(sbuf,"%.2f", newAlphaThresh);
      SurfCont->ColPlaneAlphaThresh->value = newAlphaThresh;
      SUMA_SET_TEXT_FIELD(SurfCont->ColPlaneAlphaThresh->textfield, sbuf); 
   }
  
   SUMA_RETURN(1);
}

void SUMA_cb_ColPlane_NewNodeRadGain (void *data)
{
   static char FuncName[]={"SUMA_cb_ColPlane_NewNodeRadGain"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado)) 
            || !SurfCont->curColPlane ) SUMA_RETURNe;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (curColPlane->NodeRadGain == 
       SurfCont->NodeRadGainAF->value) SUMA_RETURNe;
   
   SUMA_ColPlane_NewNodeRadGain(ado, curColPlane, 
                            SurfCont->NodeRadGainAF->value, 1);
   SUMA_RETURNe;                            
}

int SUMA_ColPlane_NewNodeRadGain (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                              float newdimfact, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlane_NewNodeRadGain"};
   SUMA_SurfaceObject *SOC=NULL, *SO=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err( "Will need to switch to current plane first. "
                  "Not ready for this");
      SUMA_RETURN(0);      
   }
   
   if (!SUMA_ColPlane_NewNodeRadGain_one(ado, colp, newdimfact, cb_direct)) {
      SUMA_S_Err("Non son pagliaccio");
      SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}

int SUMA_ColPlane_NewNodeRadGain_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                                       float newdimfact, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlane_NewNodeRadGain_one"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);   
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err("Will need to switch current plane. Not ready for this");
      SUMA_RETURN(0);      
   }

   /* change the value of the dimfact */
   curColPlane->NodeRadGain = newdimfact; 
      
   if (LocalHead) fprintf(SUMA_STDERR,"%s: NodeRadGain of %s set to %f.\n", 
         FuncName, curColPlane->Name, 
         curColPlane->NodeRadGain);
   
   SUMA_UpdateColPlaneShellAsNeeded(ado); /* update other open ColPlaneShells */

   SUMA_ADO_Flush_Pick_Buffer(ado, NULL);
   
   /* need to colorize plane */
   SUMA_ColorizePlane(curColPlane);
   
   /* a good remix and redisplay */
   SUMA_Remixedisplay (ado);
   
   /* update color label */
   SUMA_UpdateNodeLblField(ado);
   
   if (!cb_direct && newdimfact != SurfCont->NodeRadGainAF->value) {
      /* force gui match */
      sprintf(sbuf,"%.2f", newdimfact);
      SurfCont->NodeRadGainAF->value = newdimfact;
      SUMA_SET_TEXT_FIELD(SurfCont->NodeRadGainAF->textfield, sbuf); 
   }
  
   SUMA_RETURN(1);
}

void SUMA_cb_ColPlane_NewEdgeThickGain (void *data)
{
   static char FuncName[]={"SUMA_cb_ColPlane_NewEdgeThickGain"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado)) 
            || !SurfCont->curColPlane ) SUMA_RETURNe;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (curColPlane->EdgeThickGain == 
       SurfCont->EdgeThickGainAF->value) SUMA_RETURNe;
   
   SUMA_ColPlane_NewEdgeThickGain(ado, curColPlane, 
                            SurfCont->EdgeThickGainAF->value, 1);
   SUMA_RETURNe;                            
}


int SUMA_ColPlane_NewEdgeThickGain (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                              float newdimfact, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlane_NewEdgeThickGain"};
   SUMA_SurfaceObject *SOC=NULL, *SO=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err( "Will need to switch to current plane first. "
                  "Not ready for this");
      SUMA_RETURN(0);      
   }
   
   if (!SUMA_ColPlane_NewEdgeThickGain_one(ado, colp, newdimfact, cb_direct)) {
      SUMA_S_Err("Non son pagliaccio");
      SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}

int SUMA_ColPlane_NewEdgeThickGain_one (SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                                       float newdimfact, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlane_NewEdgeThickGain_one"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);   
   if (!colp) colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (colp != curColPlane) {
      SUMA_S_Err("Will need to switch current plane. Not ready for this");
      SUMA_RETURN(0);      
   }

   /* change the value of the dimfact */
   curColPlane->EdgeThickGain = newdimfact; 
      
   if (LocalHead) fprintf(SUMA_STDERR,"%s: EdgeThickGain of %s set to %f.\n", 
         FuncName, curColPlane->Name, 
         curColPlane->EdgeThickGain);
   
   SUMA_UpdateColPlaneShellAsNeeded(ado); /* update other open ColPlaneShells */

   /* need to colorize plane */
   SUMA_ColorizePlane(curColPlane);
   
   SUMA_ADO_Flush_Pick_Buffer(ado, NULL);

   /* a good remix and redisplay */
   SUMA_Remixedisplay (ado);
   
   /* update color label */
   SUMA_UpdateNodeLblField(ado);
   
   if (!cb_direct && newdimfact != SurfCont->EdgeThickGainAF->value) {
      /* force gui match */
      sprintf(sbuf,"%.2f", newdimfact);
      SurfCont->EdgeThickGainAF->value = newdimfact;
      SUMA_SET_TEXT_FIELD(SurfCont->EdgeThickGainAF->textfield, sbuf); 
   }
  
   SUMA_RETURN(1);
}

/*!
   \brief Function to set the color remix flag for DOs and call 
      a redisplay for relevant viewers 
*/
SUMA_Boolean SUMA_Remixedisplay (SUMA_ALL_DO *ADO)
{
   static char FuncName[]={"SUMA_Remixedisplay"};
   DList *list=NULL;
   char *idcode=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LHv("Called with ado=%p, ado->do_type=%d, ado->idcode_str=%s\n",
      ADO, ADO?ADO->do_type:-1, SUMA_CHECK_NULL_STR(SUMA_ADO_idcode(ADO)));

   /* remix colors for all viewers displaying related surfaces */
   switch (ADO->do_type) {
      case SO_type:
      case VO_type:
      case MASK_type:
      case TRACT_type:
      case CDOM_type:
      case GDSET_type:
         idcode = SUMA_ADO_idcode(ADO);
         break;
      case GRAPH_LINK_type: {
         SUMA_DSET *dset = SUMA_find_GLDO_Dset((SUMA_GraphLinkDO *)ADO);
         idcode = SUMA_ADO_idcode((SUMA_ALL_DO *)dset);
         break; }
      default:
         SUMA_S_Errv("Not ready for type %s\n", ADO_TNAME(ADO));
         SUMA_RETURN(NOPE);
         break;
   }
         
   
   if (!SUMA_SetRemixFlag(idcode, SUMAg_SVv, SUMAg_N_SVv)) {
      SUMA_SLP_Err("Failed in SUMA_SetRemixFlag.");
      SUMA_RETURN(NOPE);
   }

   /* redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA( list, SE_RedisplayNow_AllVisible, 
                                       SES_Suma, NULL); 
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
void SUMA_cb_ColPlaneShow_toggled ( Widget w, XtPointer data, 
                                    XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ColPlaneShow_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SUMA_S_Warn("Obsolete, do not call me!");
   SUMA_RETURNe;
   
   #if 0
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO || !SO->SurfCont) SUMA_RETURNe;
   if (!SO->SurfCont->curColPlane || 
       !SO->SurfCont->ColPlaneShow_tb) SUMA_RETURNe;

   SUMA_LH("Getting State");
   SO->SurfCont->curColPlane->Show = 
      XmToggleButtonGetState (SO->SurfCont->ColPlaneShow_tb);
   if (SO->SurfCont->Int_tb) {
      /* set the duplicate button next to int */
      SUMA_LH("Setting State of duplicate button");
      XmToggleButtonSetState (SO->SurfCont->Int_tb, 
                              SO->SurfCont->curColPlane->Show, NOPE);
   }
   SUMA_LH("Updating color plane shells");
   SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */

   SUMA_Remixedisplay((SUMA_ALL_DO*)SO);
   SUMA_UpdateNodeLblField(NULL,SO);
   
   SUMA_RETURNe;
   #endif
}

/*!
   \brief callback to deal with show only one colorplane toggle
   
   -expects SO in data
*/
void SUMA_cb_ColPlaneShowOneFore_toggled (Widget w, XtPointer data, 
                                          XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ColPlaneShowOneFore_toggled"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!curColPlane) SUMA_RETURNe;

   SUMA_ColPlaneShowOneFore_Set(ado,
               XmToggleButtonGetState (SurfCont->ColPlaneShowOneFore_tb), 1);
      
   SUMA_RETURNe;
}

int SUMA_ColPlaneShowOneFore_Set_one ( SUMA_ALL_DO *ado, 
                                       SUMA_Boolean state, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlaneShowOneFore_Set_one"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   
   if (!(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   if (!SUMA_isADO_Cont_Realized(ado)) SUMA_RETURN(0);
   
   if (SurfCont->ShowCurForeOnly == state) SUMA_RETURN(1);
   
   SurfCont->ShowCurForeOnly = state;
   XmToggleButtonSetState (SurfCont->ColPlaneShowOneFore_tb, 
                           SurfCont->ShowCurForeOnly, NOPE);   
   
   SUMA_UpdateColPlaneShellAsNeeded(ado); /* update other open ColPlaneShells */

   SUMA_Remixedisplay(ado);
   SUMA_UpdateNodeLblField(ado);
   
   SUMA_RETURN(1);
}

int SUMA_ColPlaneShowOneFore_Set ( SUMA_ALL_DO *ado, 
                                   SUMA_Boolean state, int cb_direct)
{
   static char FuncName[]={"SUMA_ColPlaneShowOneFore_Set"};
   SUMA_SurfaceObject *SOC=NULL, *SO=NULL;
   SUMA_OVERLAYS *colpC=NULL, *colp=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   if (!SUMA_isADO_Cont_Realized(ado)) SUMA_RETURN(0);
   
   
   if (!SUMA_ColPlaneShowOneFore_Set_one (ado, state, cb_direct)) {
      SUMA_S_Err("Returning on an angry note");
      SUMA_RETURN(0);
   }
   
   if (ado->do_type == SO_type) {
      SO = (SUMA_SurfaceObject *)ado;
      colp = SO->SurfCont->curColPlane;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (SOC->SurfCont->curColPlane != colpC) {
            SUMA_S_Err("Don't have contralateral as cur colplane.\n"
                       "This should not happen under L/R yoked conditions.");
            SUMA_RETURN(0);
         }
         if (!SUMA_ColPlaneShowOneFore_Set_one ((SUMA_ALL_DO *)SOC, state, 0)) {
            SUMA_S_Err("Returning on an cranky note");
            SUMA_RETURN(0);
         }
      } else {
         SUMA_LHv("Found NO contralateral equivalent to:\n"
                      " %s and %s in\n",
                      SO->Label, CHECK_NULL_STR(colp->Label));
      }
   }
   
   SUMA_RETURN(1);
}

void SUMA_cb_GDSET_ShowBundles_toggled (Widget w, XtPointer data, 
                                          XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_GDSET_ShowBundles_toggled"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   
   SurfCont = SUMA_ADO_Cont(ado);
   if (!SurfCont) SUMA_RETURNe;

   SUMA_GDSET_ShowBundles(ado,
               XmToggleButtonGetState (SurfCont->GDSET_ShowBundles_tb), 1);
      
   SUMA_RETURNe;
}

void SUMA_cb_GDSET_ShowUncon_toggled (Widget w, XtPointer data, 
                                          XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_GDSET_ShowUncon_toggled"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   
   SurfCont = SUMA_ADO_Cont(ado);
   if (!SurfCont) SUMA_RETURNe;

   SUMA_GDSET_ShowUncon(ado,
               XmToggleButtonGetState (SurfCont->GDSET_ShowUncon_tb), 1);
      
   SUMA_RETURNe;
}

int SUMA_FlushPickBufferForDO(SUMA_ALL_DO *curDO) 
{
   static char FuncName[]={"SUMA_FlushPickBufferForDO"};
   int i=0, iup=0;
   SUMA_SurfaceViewer *sv=NULL;
   
   SUMA_ENTRY;
   
   if (!curDO) SUMA_RETURN(0);
   
   /* update any viewer that is showing this 
      surface */
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
         /* is this viewer showing curSO ? */
         if (SUMA_isRegisteredDO(&(SUMAg_SVv[i]), SUMAg_DOv, curDO)) {
            sv = &(SUMAg_SVv[i]);
            SUMA_PickBuffer(sv, 0, NULL); /* flush pick buffer */
            ++iup;
         }
      }
   }

   SUMA_RETURN(iup);
}


int SUMA_GDSET_ShowBundles ( SUMA_ALL_DO *ado, 
                                SUMA_Boolean state, int cb_direct)
{
   static char FuncName[]={"SUMA_GDSET_ShowBundles"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_GRAPH_SAUX *GSaux = NULL;
   SUMA_ENTRY;

   
   if (!(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   if (!SUMA_isADO_Cont_Realized(ado)) SUMA_RETURN(0);
   if (!(GSaux = SUMA_ADO_GSaux(ado))) SUMA_RETURN(0);
   
   if (GSaux->ShowBundles == state) SUMA_RETURN(1);
   
   GSaux->ShowBundles = state;
   XmToggleButtonSetState (SurfCont->GDSET_ShowBundles_tb, 
                           GSaux->ShowBundles, NOPE);   
   /* flush pick buffer */
   SUMA_FlushPickBufferForDO(ado);

   SUMA_Remixedisplay(ado);
   
   SUMA_RETURN(1);
}

int SUMA_GDSET_ShowUncon ( SUMA_ALL_DO *ado, 
                                SUMA_Boolean state, int cb_direct)
{
   static char FuncName[]={"SUMA_GDSET_ShowUncon"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_GRAPH_SAUX *GSaux = NULL;
   SUMA_ENTRY;

   
   if (!(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURN(0);
   if (!SUMA_isADO_Cont_Realized(ado)) SUMA_RETURN(0);
   if (!(GSaux = SUMA_ADO_GSaux(ado))) SUMA_RETURN(0);
   
   if (GSaux->ShowUncon == state) SUMA_RETURN(1);
   
   GSaux->ShowUncon = state;
   XmToggleButtonSetState (SurfCont->GDSET_ShowUncon_tb, 
                           GSaux->ShowUncon, NOPE);   
   /* flush pick buffer */
   SUMA_FlushPickBufferForDO(ado);

   SUMA_Remixedisplay(ado);
   
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
   if (LocalHead) 
      fprintf (SUMA_STDERR, "%s: Pre Tolerance %f\n", FuncName, AF->value);
   /* if no negs allowed, take absolute value. 
      Round off errors can cause AF->value to show -0.00000001 
      or something ugly like that*/
   if (AF->min >= 0.0 && AF->value < 0.0) AF->value = 0.0;
   if (LocalHead) 
      fprintf (SUMA_STDERR, "%s: Post Tolerance %f\n", FuncName, AF->value);

   SUMA_ATF_SetString (AF);

   if (id) {
      AF->arrow_timer_id =
         XtAppAddTimeOut (SUMAg_CF->X->App, (INT_CAST)id==1? 500 : 100, 
                           SUMA_ATF_change_value, (XtPointer)AF);
   }
   /* turn off the modified field because it should only be on 
      when the user edits the field */
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
      /* fair enough, this is a text field, nothing to be done for value*/
      SUMA_RETURNe;
   }
   XtVaSetValues (AF->textfield, XmNvalue, buf, NULL);
   
   SUMA_RETURNe;
}

void SUMA_ATF_GetString (SUMA_ARROW_TEXT_FIELD * AF, char *sbuf)
{
   static char FuncName[]={"SUMA_ATF_GetString"};
   
   SUMA_ENTRY;
   
   sbuf[0]='\0';
   
   if (AF->type == SUMA_int) {
      snprintf (sbuf, 62, "%-4d", (int)AF->value);
   }else if (AF->type == SUMA_float) {
      snprintf (sbuf, 62, "%-4.4f", AF->value);
   }else if (AF->type == SUMA_string) { /* This AF is a text field, should 
                                           not have a 'value' */
      snprintf (sbuf, 62, "Text-Field");
   } else {
      snprintf(sbuf, 62, "UnGettable");
   }
   
   SUMA_RETURNe;
}

/*!
   \brief sets the value of Arrowfield based on string
*/
void SUMA_ATF_SetValue (SUMA_ARROW_TEXT_FIELD * AF)
{
   static char FuncName[]={"SUMA_ATF_SetValue"};
   double val;
   void *n = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
  
   XtVaGetValues (AF->textfield, XmNvalue, &n, NULL);
   /* YOU DO NOT WANT TO FREE n because n is not a 
      copy of the string in the widget! 
      Later in time:
      Hmmmm, maybe you do, maybe you do. Must abide by 
      upper case message. Must have crashed somwhere */
   
   SUMA_LHv("Read %s\n", (char *)n);
   
   if (!SUMA_strtod((char*)n, &val)){
      /* bad syntax, reset value*/
      SUMA_LHv("Bad syntax, got %f.\n", val);
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
            "Bad value in text field", FuncName, SMT_Error, SMA_Log);
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
         /* It is still nice to call SetString because it puts the 
            cursor at the beginning of the field */
         SUMA_ATF_SetString (AF);
         
      }
   }
   
   SUMA_RETURNe;
}

/*!
   \brief Callback for Group button
   -Expects sv in data
*/
void SUMA_cb_ViewerCont_SwitchGroup (  Widget w, XtPointer data, 
                                       XtPointer call_data)
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
void SUMA_cb_ViewerCont_SwitchState (Widget w, XtPointer data, 
                                     XtPointer call_data)
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
void SUMA_cb_SurfCont_SwitchColPlane (Widget w, XtPointer data, 
                                      XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SurfCont_SwitchColPlane"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ALL_DO *ado = NULL;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   ado = (SUMA_ALL_DO *)data;
   
   SUMA_RefreshDsetList (ado);
                                                   
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
   if (SUMAg_CF->ROI_mode) SUMA_ResetPrying(NULL);
   /* take care of sensitivity of Pen button */
   if (!SUMAg_CF->ROI_mode) XtSetSensitive (SUMAg_CF->X->DrawROI->Penmode_tb, 0);
   else XtSetSensitive (SUMAg_CF->X->DrawROI->Penmode_tb, 1);

   SUMA_UpdateAllViewerCursor();
   
   SUMA_RETURNe;

}
/*!
   \brief Toggles the contour ROI mode
*/
void SUMA_cb_ContROImode_toggled (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_ContROImode_toggled"};
   DList *list=NULL;
   SUMA_ENTRY;
   
   SUMAg_CF->ROI_contmode = !SUMAg_CF->ROI_contmode;
   

   /* redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, 
                                      SE_RedisplayNow_AllVisible, 
                                      SES_Suma, NULL); 
   if (!SUMA_Engine(&list)) {
      SUMA_SLP_Err("Failed to redisplay.");
      SUMA_RETURNe;
   }
   
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
      XmToggleButtonSetState (SUMAg_CF->X->DrawROI->AfniLink_tb, 
                              SUMAg_CF->ROI2afni, NOPE);
   }
   
   if (SUMAg_CF->ROI2afni) {
      if (SUMAg_CF->ROI_CM) {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Sending cmap (%s)\n",
            FuncName,  SUMAg_CF->ROI_CM->Name);
            SUMA_LH("Sending colormap to afni ...");
         /* send the color map for ROI to afni */
         cmap = SUMA_StandardMapIndex (SUMAg_CF->ROI_CM->Name);
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Sending cmap %d (%s)\n",
            FuncName, cmap, SUMAg_CF->ROI_CM->Name);
         list = SUMA_CreateList();
         ED = SUMA_InitializeEngineListData (SE_SendColorMapToAfni);
         if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                SEF_i, (void*)&cmap, 
                                                SES_SumaWidget, NULL, NOPE, 
                                                SEI_Head, NULL )) {
            fprintf( SUMA_STDERR,
                     "Error %s: Failed to register command\n", FuncName);
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

int SUMA_SelectSwitchColPlane_one(SUMA_ALL_DO *ado, 
                                  SUMA_LIST_WIDGET *LW, 
                                  int ichoice, SUMA_Boolean CloseShop, 
                                  int setmen)
{
   static char FuncName[]={"SUMA_SelectSwitchColPlane_one"};
   SUMA_OVERLAYS *ColPlane=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !LW) SUMA_RETURN(0);
   SurfCont = SUMA_ADO_Cont(ado);
   
   /* retrieve that choice from the SUMA_ASSEMBLE_LIST_STRUCT 
      structure and initialize the drawing window */
   if (LW->ALS) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: N_clist = %d\n", FuncName, LW->ALS->N_clist); 
      if (LW->ALS->N_clist > ichoice) {
         ColPlane = (SUMA_OVERLAYS *)LW->ALS->oplist[ichoice];
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: Retrieved ColPlane named %s\n", 
                     FuncName, ColPlane->Name);
         SUMA_InitializeColPlaneShell(ado, ColPlane);
         SUMA_UpdateColPlaneShellAsNeeded(ado); /* update other open 
                                                   ColPlaneShells */
         SUMA_UpdateNodeField(ado);
         SUMA_UpdateCrossHairNodeLabelFieldForDO(ado);
         /* If you're viewing one plane at a time, do a remix */
         if (SurfCont->ShowCurForeOnly)
            SUMA_Remixedisplay(ado);
      }
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: NULL ALS\n", FuncName); 
   }

   if (CloseShop) {
      SUMA_cb_CloseSwitchColPlane( NULL,  
                        (XtPointer)SurfCont->SwitchDsetlst,  NULL);
   }  
   
   SUMA_RETURN(1);
}

int SUMA_SelectSwitchColPlane(SUMA_ALL_DO *ado, 
                                  SUMA_LIST_WIDGET *LW, 
                                  int ichoice, SUMA_Boolean CloseShop, 
                                  int setmen)
{
   static char FuncName[]={"SUMA_SelectSwitchColPlane"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !LW) SUMA_RETURN(0);

   if (!SUMA_SelectSwitchColPlane_one(ado, LW, ichoice, CloseShop, setmen)) {
      SUMA_RETURN(0);
   }
   
   if (ado->do_type == SO_type) {
      SUMA_OVERLAYS *colp=NULL, *colpC=NULL;
      SUMA_SurfaceObject *SOC=NULL, *SO=(SUMA_SurfaceObject *)ado;
      /* do we have a contralateral SO and overlay? */
      colp = SUMA_ADO_CurColPlane(ado);
      colpC = SUMA_Contralateral_overlay(colp, (SUMA_SurfaceObject *)ado, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (!SOC->SurfCont->SwitchDsetlst->ALS)
                        SUMA_RefreshDsetList ((SUMA_ALL_DO *)SOC);
         if (!SUMA_SelectSwitchColPlane_one((SUMA_ALL_DO *)SOC, 
                                       SOC->SurfCont->SwitchDsetlst, 
                                            ichoice, 0, 1)) {
            SUMA_S_Warn("Failed in contralateralization");
         }
      }
   }

   SUMA_RETURN(1);
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
   SUMA_Boolean CloseShop = NOPE, Found = NOPE;
   int ichoice = -1;
   SUMA_OVERLAYS *ColPlane = NULL;
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   
   SurfCont = SUMA_ADO_Cont(ado);
   LW = SurfCont->SwitchDsetlst;
   
   if (!LW) {
      SUMA_S_Err("NULL LW!");
      SUMA_RETURNe;
   }

   ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop);

   if (!SUMA_SelectSwitchColPlane(ado, LW, ichoice, CloseShop, 1)) {
      SUMA_S_Err("I guess failure was an option.");
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
   
   switch (SUMA_CLOSE_MODE)   {/* No open GL drawables in this widget*/
      case SUMA_WITHDRAW:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Withdrawing list widget %s...\n", 
                     FuncName, LW->Label);
         XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
            XtWindow(LW->toplevel),
            XScreenNumberOfScreen(XtScreen(LW->toplevel)));
         break;
      case SUMA_DESTROY: 
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Destroying list widget %s...\n", FuncName, LW->Label);
         XtDestroyWidget(LW->toplevel);
         LW->toplevel = NULL;
         break;
      default:
         SUMA_S_Err("Not setup to deal with this closing mode");
         SUMA_RETURNe;
         break;
   }
   
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

   ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop);

   /* now retrieve that choice from the SUMA_ASSEMBLE_LIST_STRUCT structure 
      and initialize the drawing window */
   if (LW->ALS) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: N_clist = %d\n", 
                                             FuncName, LW->ALS->N_clist); 
      if (LW->ALS->N_clist > ichoice) {
         strn = (char *)LW->ALS->clist[ichoice];
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Retrieved group labeled %s\n", 
                                    FuncName, strn);
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
   
   switch (SUMA_CLOSE_MODE)   {/* No open GL drawables in this widget*/
      case SUMA_WITHDRAW:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Withdrawing list widget %s...\n", FuncName, LW->Label);
         XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
            XtWindow(LW->toplevel),
            XScreenNumberOfScreen(XtScreen(LW->toplevel)));
         break;
      case SUMA_DESTROY: 
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Destroying list widget %s...\n", FuncName, LW->Label);
         XtDestroyWidget(LW->toplevel);
         LW->toplevel = NULL;
         break;
      default:
         SUMA_S_Err("Not setup to deal with this closing mode");
         SUMA_RETURNe;
         break;
   }
   
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

   ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop);

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
   
   switch (SUMA_CLOSE_MODE)   {/* No open GL drawables in this widget*/
      case SUMA_WITHDRAW:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Withdrawing list widget %s...\n", FuncName, LW->Label);
         XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
            XtWindow(LW->toplevel),
            XScreenNumberOfScreen(XtScreen(LW->toplevel)));
         break;
      case SUMA_DESTROY:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Destroying list widget %s...\n", FuncName, LW->Label);
         XtDestroyWidget(LW->toplevel);
         LW->toplevel = NULL;
         break;
      default:
         SUMA_S_Err("Not setup to deal with this mode");
         SUMA_RETURNe;
         break;
   }
   
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
      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: Closing switch ROI window ...\n", FuncName);
      SUMA_cb_CloseSwitchROI(NULL, 
                           (XtPointer) SUMAg_CF->X->DrawROI->SwitchROIlst, NULL);
   }
   
   /* Turn off Draw Mode, if it is ON */
   if (SUMAg_CF->ROI_mode) {
      XmToggleButtonSetState (SUMAg_CF->X->DrawROI->DrawROImode_tb, 
                              NOPE, YUP);
   }
   switch (SUMA_CLOSE_MODE)   {/* No open GL drawables in this widget*/
      case SUMA_WITHDRAW:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Withdrawing DrawROI window...\n", FuncName);
         XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
            XtWindow(SUMAg_CF->X->DrawROI->AppShell),
            XScreenNumberOfScreen(XtScreen(SUMAg_CF->X->DrawROI->AppShell)));
         break;
      case SUMA_DESTROY :
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: Destroying DrawROI window...\n", FuncName);
         XtDestroyWidget(SUMAg_CF->X->DrawROI->AppShell);
         SUMAg_CF->X->DrawROI->AppShell = NULL;
         break;
      default:
         SUMA_S_Err("Not setup to deal with this mode of closure");
         SUMA_RETURNe;
         break;
   }
   
   SUMA_RETURNe;
}
/*!
   \brief creates the SUMA controller window. Expects nothing  in input
*/
void SUMA_cb_createSumaCont(Widget ww, XtPointer ddata, XtPointer ccallData)
{
   static char FuncName[] = {"SUMA_cb_createSumaCont"};
   Widget w, rc, pb_close, pb_new, pb_done, pb_bhelp, AppFrame, 
          tb, rb, rc_m;
   int i;
   char *sss;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SUMAg_CF->X->SumaCont->AppShell) {
      SUMA_S_Err("SUMAg_CF->X->SumaCont->AppShell!=NULL. Should not be here.");
      SUMA_RETURNe;
   }

   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }

   /* create as a separate application shell, 
      you do not want a parent to this controller that
      can be closed or withdrawn temporarily */
   SUMAg_CF->X->SumaCont->AppShell = XtVaAppCreateShell( sss, "Suma" ,
      topLevelShellWidgetClass , SUMAg_CF->X->DPY_controller1 ,
      XmNtitle, "Suma Controller", 
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
   SUMAg_CF->X->SumaCont->form = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SUMAg_CF->X->SumaCont->AppShell,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
      
   SUMA_Register_Widget_Help( SUMAg_CF->X->SumaCont->form , 0,
                                 "SumaCont",
                                 "Suma Controller",
"The suma controller is for controlling parameters common to across viewers and objects."
":SPX:"
"You can launch the :ref:`Suma Controller <SumaCont>` with:"
" :ref:`ctrl+u <LC_Ctrl+u>` or :menuselection:`View-->Suma Controller`\n"
"\n"
".. figure:: media/SumaCont.auto.ALL.jpg\n"
"   :align: center\n"
"   :name: media/SumaCont.auto.ALL.jpg\n"
"\n"
"   :ref:`(link)<media/SumaCont.auto.ALL.jpg>`\n"
"\n\n"
"   ..\n\n"
":DEF:"
"You can launch the Suma Controller with:"
"\n'ctrl+u' or 'View-->Suma Controller'\n"
":SPX:"
"\n") ;


   /* a LockFrame to put the lockstuff in */
   SUMAg_CF->X->SumaCont->LockFrame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, SUMAg_CF->X->SumaCont->form,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_FORM ,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL); 
   
      /* this one requires Motif 1.2 or newer */
      XtVaCreateManagedWidget ("Lock",
         xmLabelWidgetClass, SUMAg_CF->X->SumaCont->LockFrame, 
         XmNchildType, XmFRAME_TITLE_CHILD,
         XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
         NULL);
   
   /* row column Lock rowcolumns */
   rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SUMAg_CF->X->SumaCont->LockFrame,
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
       
      SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i] = 
         XtVaCreateManagedWidget("-", 
            xmToggleButtonWidgetClass, 
            SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i], 
                     XmNvalueChangedCallback, SUMA_cb_XHlock_toggled, 
                     (XTP_CAST)(tmpfac*i));
       
      SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+1] = 
         XtVaCreateManagedWidget("i", 
            xmToggleButtonWidgetClass, 
            SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+1], 
                     XmNvalueChangedCallback, SUMA_cb_XHlock_toggled,  
                     (XTP_CAST)(tmpfac*i+1));
      
      SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+2] = 
         XtVaCreateManagedWidget("c", 
            xmToggleButtonWidgetClass, 
            SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+2], 
                     XmNvalueChangedCallback, SUMA_cb_XHlock_toggled,  
                     (XTP_CAST)(tmpfac*i+2));
      
      XtManageChild (SUMAg_CF->X->SumaCont->Lock_rbg->rb[i]);
      
      /* put some help on the radiobox and its children*/
      SUMA_Register_Widget_Children_Help(SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], 
                           1, "SumaCont->Lock", NULL, SUMA_LockSumaCont_help );
      
      /* initialize radio button created */
      SUMA_set_Lock_rb (SUMAg_CF->X->SumaCont->Lock_rbg, i, SUMAg_CF->Locked[i]);
      
      XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rc_m, NULL);
      
      SUMAg_CF->X->SumaCont->LockView_tbg[i] = XtVaCreateManagedWidget("v", 
         xmToggleButtonWidgetClass, rc_m, NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->LockView_tbg[i], 
                     XmNvalueChangedCallback, SUMA_cb_XHviewlock_toggled, 
                     (XTP_CAST) i);
      XmToggleButtonSetState (SUMAg_CF->X->SumaCont->LockView_tbg[i], 
                              SUMAg_CF->ViewLocked[i], NOPE);
      /* put some help on the view lock*/
      SUMA_Register_Widget_Children_Help(rc_m , 1,
                           "SumaCont->Lock->View", 
                           NULL, SUMA_LockViewSumaCont_help );
            
   }  
   XtManageChild (rc);
   XtManageChild (SUMAg_CF->X->SumaCont->LockFrame);
      
   
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
   SUMA_Register_Widget_Children_Help(SUMAg_CF->X->SumaCont->Lock_rbg->arb , 1,
                         "SumaCont->Lock->All", NULL,
                         SUMA_LockSumaCont_help );

   /* initialize radio button created */
   SUMA_set_Lock_arb (SUMAg_CF->X->SumaCont->Lock_rbg);   
         
   XtVaCreateManagedWidget ("sep", xmSeparatorGadgetClass, rc_m, NULL);
      
   SUMAg_CF->X->SumaCont->LockAllView_tb = 
      XtVaCreateManagedWidget("v",xmToggleButtonWidgetClass, rc_m, NULL);
   XtAddCallback (SUMAg_CF->X->SumaCont->LockAllView_tb, 
                  XmNvalueChangedCallback, SUMA_cb_XHaviewlock_toggled, NULL);
   SUMA_set_LockView_atb();
   
   /* a frame to put the Close button in */
   SUMAg_CF->X->SumaCont->AppFrame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, SUMAg_CF->X->SumaCont->form,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_WIDGET ,
      XmNtopWidget, SUMAg_CF->X->SumaCont->LockFrame,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL); 
   
   
   rc = XtVaCreateManagedWidget ("rowcolumn",
         xmRowColumnWidgetClass, SUMAg_CF->X->SumaCont->AppFrame,
         XmNpacking, XmPACK_COLUMN, 
         XmNorientation , XmVERTICAL ,
         XmNnumColumns, 2, 
         NULL);
         
   pb_new = XtVaCreateWidget ("Viewer", 
      xmPushButtonWidgetClass, rc, 
      NULL);
   XtAddCallback (pb_new, XmNactivateCallback, SUMA_cb_newSumaCont, NULL);
   SUMA_Register_Widget_Help(pb_new , 1, "SumaCont->Viewer",
                             "Opens a new viewer", SUMA_viewerSumaCont_help );
   XtManageChild (pb_new); 

   pb_close = XtVaCreateWidget ("Close", 
      xmPushButtonWidgetClass, rc, 
      NULL);   
   XtAddCallback (pb_close, XmNactivateCallback, SUMA_cb_closeSumaCont, NULL);
   SUMA_Register_Widget_Help(pb_close , 1, "SumaCont->Close",
                             "Close SUMA controller", SUMA_closeSumaCont_help ) ;
   XtManageChild (pb_close); 
   
   pb_bhelp = XtVaCreateWidget ("BHelp", 
      xmPushButtonWidgetClass, rc, 
      NULL);
   XtAddCallback (pb_bhelp, XmNactivateCallback, MCW_click_help_CB, NULL);
   SUMA_Register_Widget_Help(pb_bhelp, 1, "SumaCont->BHelp",
           "Press this button then click on a button/label/menu for more help.",
                             SUMA_help_help ) ; 
   
   XtManageChild (pb_bhelp); 
   
   pb_bhelp = XtVaCreateWidget ("WHelp", 
      xmPushButtonWidgetClass, rc, 
      NULL);
   XtAddCallback (pb_bhelp, XmNactivateCallback, SUMA_click_webhelp_CB, 
                  "SumaCont->WHelp");
   MCW_set_widget_bg( pb_bhelp , MCW_buthighlight(pb_bhelp) , 0 ) ;
   SUMA_Register_Widget_Help(pb_bhelp, 1, "SumaCont->WHelp",
          "Press this button then click on a button/label/menu for online help.",
                             SUMA_webhelp_help ) ; 
   
   XtManageChild (pb_bhelp); 
   
   SUMAg_CF->X->SumaCont->quit_pb = XtVaCreateWidget ("done", 
      xmPushButtonWidgetClass, rc, 
      NULL);
   XtAddCallback (SUMAg_CF->X->SumaCont->quit_pb, XmNactivateCallback, 
                  SUMA_cb_doneSumaCont, NULL);
   SUMA_Register_Widget_Help(SUMAg_CF->X->SumaCont->quit_pb, 1,
                             "SumaCont->done",
                  "Click twice in 5 seconds to close everything and quit SUMA.",
                  "Click twice in 5 seconds to quit application. "
                  "All viewer windows will be closed, nothing is saved, "
                  "SUMA will terminate, and "
                  "there maybe no one left at this computer.");
   MCW_set_widget_bg( SUMAg_CF->X->SumaCont->quit_pb , 
                      MCW_hotcolor(SUMAg_CF->X->SumaCont->quit_pb) , 0 ) ;

   XtManageChild (SUMAg_CF->X->SumaCont->quit_pb); 
  
   XtManageChild (SUMAg_CF->X->SumaCont->AppFrame);
   
   /* manage the remaing widgets */
   XtManageChild (SUMAg_CF->X->SumaCont->form);
   
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
   
   selenium_close(); /* close any selenium opened browser windows if open */
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
   
   cd = (INT_CAST) client_data;
   
   SUMA_ENTRY;

   i = cd / SUMAg_CF->X->SumaCont->Lock_rbg->N_but;
   j = cd % SUMAg_CF->X->SumaCont->Lock_rbg->N_but;
   fprintf (SUMA_STDERR, "%s: Viewer %c Lock=%d.\n", FuncName, 65+i, j);
   SUMAg_CF->Locked[i] = j;
   
   /* now call the function to set the All lock buttons */
   SUMA_set_Lock_arb (SUMAg_CF->X->SumaCont->Lock_rbg);

   SUMA_RETURNe;
}

void SUMA_cb_XHalock_toggled (Widget w, XtPointer client_data, 
                              XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_XHalock_toggled"};
   int i;
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   
   SUMA_ENTRY;
 
   i = (INT_CAST) client_data;
   

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

void SUMA_cb_XHaviewlock_toggled (Widget w, XtPointer client_data, 
                                  XtPointer callData)
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

void SUMA_cb_XHviewlock_toggled(Widget w, XtPointer client_data, 
                                 XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_XHviewlock_toggled"};
   SUMA_Boolean LocalHead = NOPE;
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   int i = (INT_CAST) client_data;
   
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
      SUMA_S_Err("Failed in SUMA_X_SurfaceViewer_Create.");
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
   
   switch (SUMA_CLOSE_MODE)   {/* NO open GL drawables in this widget*/
      case SUMA_WITHDRAW:
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Withdrawing Suma Controller...\n", FuncName);
         XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
            XtWindow(SUMAg_CF->X->SumaCont->AppShell),
            XScreenNumberOfScreen(XtScreen(SUMAg_CF->X->SumaCont->AppShell)));
         break;
      case SUMA_DESTROY: 
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Destroying Suma Controller...\n", FuncName);
         XtDestroyWidget(SUMAg_CF->X->SumaCont->AppShell);
         SUMAg_CF->X->SumaCont->AppShell = NULL;
         break;
      default:
         SUMA_S_Err("Not setup to deal with this closing mode");
         SUMA_RETURNe;
         break;
   }
   
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
   /* form the string of the surface info */
   if (!(s = SUMA_SurfaceViewer_StructInfo(sv, 1))) {
      SUMA_S_Err("Failed in SUMA_SurfaceViewer_StructInfo.");
      SUMA_RETURNe;
   }
   sprintf(stmp, "[%c] Viewer Info", 65+isv);

   /* check to see if window is already open, if it is, just raise it */
   if (sv->X->ViewCont->ViewerInfo_TextShell) {
     sv->X->ViewCont->ViewerInfo_TextShell = 
         SUMA_CreateTextShell(s, stmp, sv->X->ViewCont->ViewerInfo_TextShell);
     SUMA_ifree(s); 
     XRaiseWindow (SUMAg_CF->X->DPY_controller1, 
         XtWindow(sv->X->ViewCont->ViewerInfo_TextShell->toplevel));
     SUMA_RETURNe;
   }
   
   
   if (s) {
      TextShell =  SUMA_CreateTextShellStruct (
                                 SUMA_ViewerInfo_open, 
                                 (void *)sv, "SurfaceViewer",
                                 SUMA_ViewerInfo_destroyed, 
                                 (void *)sv, NULL);
      if (!TextShell) {
         fprintf (SUMA_STDERR, 
                  "Error %s: Failed in SUMA_CreateTextShellStruct.\n",
                   FuncName);
         SUMA_RETURNe;
      }
      sv->X->ViewCont->ViewerInfo_TextShell = 
            SUMA_CreateTextShell(s, stmp, TextShell);
      SUMA_ifree(s);
      /* invert the widget to indicate window is open */
      if (sv->X->ViewCont && sv->X->ViewCont->ViewerInfo_pb) 
               MCW_invert_widget (sv->X->ViewCont->ViewerInfo_pb);
   }   

    
   SUMA_RETURNe;
}

/*!
   \brief Function called when Viewer Info window is open
*/
void SUMA_ViewerInfo_open (void *p) 
{
   static char FuncName[] = {"SUMA_ViewerInfo_open"};
   SUMA_SurfaceViewer *sv= NULL;
  
   SUMA_ENTRY;
   
   sv = (SUMA_SurfaceViewer *)p;
   
   
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
   
   /* form the string of the surface info */
   if (!(s = SUMA_CommonFieldsInfo (SUMAg_CF, 1))) {
      SUMA_S_Err("Failed in SUMA_CommonFieldsInfo.");
      SUMA_RETURNe;
   }
   
   /* check to see if window is already open, if it is, just raise it */
   if (SUMAg_CF->X->SumaCont->SumaInfo_TextShell) {
      SUMAg_CF->X->SumaCont->SumaInfo_TextShell = 
               SUMA_CreateTextShell(s, "SUMA", 
                           SUMAg_CF->X->SumaCont->SumaInfo_TextShell);
      SUMA_ifree(s); 
      XRaiseWindow (SUMAg_CF->X->DPY_controller1, 
                  XtWindow(SUMAg_CF->X->SumaCont->SumaInfo_TextShell->toplevel));
      SUMA_RETURNe;
   }
   
   if (s) {
      TextShell =  SUMA_CreateTextShellStruct (SUMA_SumaInfo_open, NULL, NULL, 
                                               SUMA_SumaInfo_destroyed, NULL, 
                                               NULL);
      if (!TextShell) {
         fprintf (SUMA_STDERR, 
                  "Error %s: Failed in SUMA_CreateTextShellStruct.\n", FuncName);
         SUMA_RETURNe;
      }
      SUMAg_CF->X->SumaCont->SumaInfo_TextShell = 
                        SUMA_CreateTextShell(s, "SUMA", TextShell);
      SUMA_ifree(s);
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
   Feb 23 04: Now requiring ado in client_data. Other widgets will be calling
   this function too, so w should not be used
*/
void SUMA_cb_moreSurfInfo (Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_moreSurfInfo"};
   SUMA_ALL_DO *ado=NULL;
   void *n=NULL;
   char *s = NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell = NULL;
   
   SUMA_ENTRY;

   ado = (SUMA_ALL_DO *)client_data;
   SurfCont = SUMA_ADO_Cont(ado);
   
   if (!(s = SUMA_ADO_Info (ado, SUMAg_CF->DsetList, 0))) {
      SUMA_S_Err("Failed in SUMA_SurfaceObject_Info.");
      SUMA_RETURNe;
   }
   
   /* check to see if window is already open, if it is, just raise it */
   if (SurfCont->SurfInfo_TextShell) {
      SurfCont->SurfInfo_TextShell = 
         SUMA_CreateTextShell(s, SUMA_ADO_Label(ado), 
                              SurfCont->SurfInfo_TextShell);
      SUMA_ifree(s); 
      XRaiseWindow (SUMAg_CF->X->DPY_controller1, 
            XtWindow(SurfCont->SurfInfo_TextShell->toplevel));
      SUMA_RETURNe;
   }
   
   if (s) {
      TextShell =  
         SUMA_CreateTextShellStruct (SUMA_SurfInfo_open, (void *)ado, "ADO",
                                     SUMA_SurfInfo_destroyed, (void *)ado,
                                     NULL);
      if (!TextShell) {
         SUMA_S_Err("Failed in SUMA_CreateTextShellStruct.");
         SUMA_RETURNe;
      }
      SurfCont->SurfInfo_TextShell = 
         SUMA_CreateTextShell(s, SUMA_ADO_Label(ado), TextShell);
      SUMA_ifree(s);
      /* invert the widget to indicate window is open */
      MCW_invert_widget (SurfCont->SurfInfo_pb);
   }   

    
   SUMA_RETURNe;
}

/*!
   \brief Function called when Surface Info window is open
*/
void SUMA_SurfInfo_open (void *p) 
{
   static char FuncName[] = {"SUMA_SurfInfo_open"};
   SUMA_ALL_DO *ado= NULL;
  
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)p;
   
   
   SUMA_RETURNe;
}

/*!
   \brief Function called when Surface Info window is destroyed
*/
void SUMA_SurfInfo_destroyed (void *p) 
{
   static char FuncName[] = {"SUMA_SurfInfo_destroyed"};
   SUMA_ALL_DO *ado= NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   
   SUMA_ENTRY;
   if (!(ado = (SUMA_ALL_DO *)p) || !(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Err("Real bad cheese");
      SUMA_RETURNe;
   }
   MCW_invert_widget (SurfCont->SurfInfo_pb);
   
   SurfCont->SurfInfo_TextShell = NULL;
   SUMA_RETURNe;
}

#define NO_SPACE(str) {\
   int m_i; \
   for (m_i=0; str[m_i]!='\0';++m_i) { \
      if (isspace(str[m_i])) { str[m_i]='_'; } \
   }  \
}

 
char * SUMA_WriteStringToFile(char *fname, char *s, int over, int view) 
{
   static char FuncName[]={"SUMA_WriteStringToFile"};
   FILE *fout=NULL;
   char *fused=NULL, *viewer=NULL;
   int i=0;
   char sbuf[128], cmd[256];
   
   SUMA_ENTRY;
   
   if (!fname) fname = FuncName;
   if (!s) SUMA_RETURN(NULL);
   
   fused = SUMA_copy_string(fname);SUMA_NICEATE_FILENAME(fused, '_');
   if (!over) {
      i = 0;
      while (i < 10000 && SUMA_filexists(fused)) {
         SUMA_free(fused);fused = NULL;
         sprintf(sbuf,".%03d", i);
         fused = SUMA_append_replace_string(fname,sbuf,"", 0); NO_SPACE(fused);
         SUMA_NICEATE_FILENAME(fused, '_');
         ++i;
      }
      if (i >= 10000) {
         SUMA_S_Errv("Cannot find available name for %s\n"
                     "I am giving up.\n", fname);
         SUMA_free(fused); fused = NULL;
         SUMA_RETURN(NULL);
      }
   }  
   
   if ((fout = fopen(fused,"w"))) {
      fprintf(fout,"%s", s);
      fclose(fout);
   } else {
      SUMA_S_Errv("Failed to write to %s.\n", fused);
      SUMA_free(fused);
      SUMA_RETURN(NULL);
   }
   
   if (view) {
      if (!(viewer = GetAfniTextEditor())) {
         SUMA_S_Err("No GUI editor defined, and guessing game failed.\n"
              "Set AFNI_GUI_EDITOR in your .afnirc for this option to work.\n"); 
         SUMA_free(fused);
         SUMA_RETURN(NULL);
      }
      snprintf(cmd,250*sizeof(char),"%s %s &", viewer, fused);
      system(cmd);
   }
   
   SUMA_RETURN(fused);
}

/*!
   View text content of shell in editor  
*/
void SUMA_ViewTextShellInEditor(Widget w, XtPointer ud, XtPointer cd) 
{
   static char FuncName[] = {"SUMA_ViewTextShellInEditor"};
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell=NULL;
   char *string=NULL, *fused=NULL;
   char sbuf[128];
   
   SUMA_ENTRY;
   
   if (!GetAfniTextEditor()) {
      SUMA_SLP_Err("No GUI editor defined, and guessing game failed.\n"
              "Set AFNI_GUI_EDITOR in your .afnirc for this option to work.");
      SUMA_RETURNe;
   }
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *)ud;
   
   if (!(string = XmTextGetString (TextShell->text_w)) || !*string) {
      SUMA_SLP_Warn("Nothing to save");
      SUMA_RETURNe;
   }
   
   snprintf(sbuf, 120*sizeof(char),"/tmp/VTSIE.%s.txt",TextShell->title);
   if (!(fused = SUMA_WriteStringToFile(sbuf, string, 0, 1))) {
      SUMA_SLP_Err("Failed to write text.");
   } else {
      SUMA_free(fused); fused=NULL;
   }
   
   XtFree(string); string=NULL;

   SUMA_RETURNe;
}

/*!
   Save text content of shell to a file 
*/
void SUMA_SaveTextShell(Widget w, XtPointer ud, XtPointer cd) 
{
   static char FuncName[] = {"SUMA_SaveTextShell"};
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell=NULL;
   char *string=NULL, *fused=NULL;
   char sbuf[128];
   
   SUMA_ENTRY;
   
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *)ud;
   
   if (!(string = XmTextGetString (TextShell->text_w)) || !*string) {
      SUMA_SLP_Warn("Nothing to save");
      SUMA_RETURNe;
   }
   
   if (!(fused = SUMA_WriteStringToFile(TextShell->title, string, 0, 0))) {
      SUMA_SLP_Err("Failed to write text.");
   } else {
      snprintf(sbuf,127*sizeof(char),
                   "Wrote window content to %s", fused);
      SUMA_free(fused); fused=NULL;
      SUMA_SLP_Note("%s",sbuf);
   }
   
   XtFree(string); string=NULL;

   SUMA_RETURNe;
}


/*!
   Refresh content of text shell  
*/
void SUMA_RefreshTextShell(Widget w, XtPointer ud, XtPointer cd) 
{
   static char FuncName[] = {"SUMA_RefreshTextShell"};
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell=NULL;
   char *string=NULL, *fused=NULL;
   char sbuf[128];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *)ud;
   
   if (!TextShell->OpenDataType) { /* not refreshable for sure */
      SUMA_RETURNe;
   }
   
   /* find out where this text shell comes from based on the OpenDataType */
   if (strstr(TextShell->OpenDataType, "SurfaceViewer")) {
      SUMA_cb_moreViewerInfo(w, TextShell->OpenData, cd);
   } else if (strstr(TextShell->OpenDataType, "ADO")) {
      SUMA_cb_moreSurfInfo(w, TextShell->OpenData, cd);
   } else {
      /* nothing know as updatable */
      SUMA_LHv("Nothing done for >%s<%s>\n",
                  TextShell->title, TextShell->OpenDataType);
   }
   
   SUMA_RETURNe;
}

/*!
   Open weblink of text shell  
*/
void SUMA_WebTextShell(Widget w, XtPointer ud, XtPointer cd) 
{
   static char FuncName[] = {"SUMA_WebTextShell"};
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell=NULL;
   char *string=NULL, *fused=NULL;
   char sbuf[128];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *)ud;
   
   if (!TextShell->weblink) { /* nothing to do for sure */
      SUMA_RETURNe;
   }
   
   SUMA_LH("Opening %s", TextShell->weblink);
   whereami_browser(TextShell->weblink);
   
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
   if (TextShell) {
      if (TextShell->DestroyCallBack) {
         /* call destroy callback */
         TextShell->DestroyCallBack(TextShell->DestroyData);
      }
      SUMA_ifree(TextShell->title);
      SUMA_ifree(TextShell->OpenDataType);
      SUMA_ifree(TextShell->weblink);
      SUMA_free(TextShell);
   }
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
                                    void *opendata, char *odtype,
                                    void (*closecallback)(void*data), 
                                    void *closedata,
                                    char *weblink)
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
   if (!odtype) odtype = "NotSet";
   TextShell->text_w =  TextShell->search_w = 
                        TextShell->text_output = 
                        TextShell->toplevel = NULL;
   TextShell->case_sensitive = NOPE;
   TextShell->allow_edit = NOPE;
   TextShell->OpenCallBack = opencallback;
   TextShell->OpenData = opendata;
   TextShell->OpenDataType = SUMA_copy_string(odtype);
   TextShell->DestroyCallBack = closecallback;
   TextShell->DestroyData = closedata;
   TextShell->CursorAtBottom = NOPE;
   TextShell->title = NULL;
   TextShell->weblink = NULL;
   if (weblink) TextShell->weblink = SUMA_copy_string(weblink);
   SUMA_RETURN (TextShell);
}  

/*!
   \brief Opens a window with text information in it. 
   
   \param s (char *) string to display, must be null terminated.
   \param title (char *) title of window
   \param TextShell (SUMA_CreateTextShell *) containing options 
                                                for SUMA_CreateTextShell
      if TextShell->toplevel then only the log message is updated, otherwise 
                                                the window is created.
   \return TextShell (SUMA_CreateTextShell *) same structure sent to function 
                                              but with widgets fields filled. 
   
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
   Widget rowcol_v, rowcol_h, close_w, save_w, view_w, 
          form, frame, toggle_case_w, refresh_w, web_w;
   int n;
   Pixel fg_pix = 0;
   Arg args[30];
   static XmTextScanType sarray[] = {  XmSELECT_POSITION, 
                                       XmSELECT_WORD, 
                                       XmSELECT_LINE, 
                                       XmSELECT_ALL };
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!title) title = "NO_Title";
   if (!TextShell) {
      SUMA_S_Err("Need Text Shell first");
      SUMA_RETURN(TextShell);
   }
   if (TextShell->title) SUMA_free(TextShell->title);
   TextShell->title = SUMA_copy_string(title);

   if (TextShell->OpenCallBack) { /* do the opening callback */
      SUMA_LH("Calling OpenCallBack.\n");
      TextShell->OpenCallBack(TextShell->OpenData);
   }
   
   if (!TextShell->toplevel) { /* need to create window */
      SUMA_LH("Creating new text shell window.\n");
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
      refresh_w = XtVaCreateManagedWidget (
                     "Refresh",
                     xmPushButtonWidgetClass, 
                     rowcol_h, NULL);
      XtAddCallback (refresh_w, 
                     XmNactivateCallback, 
                     SUMA_RefreshTextShell, 
                     TextShell);    
      save_w = XtVaCreateManagedWidget (
                     "Save",
                     xmPushButtonWidgetClass, 
                     rowcol_h, NULL);
      XtAddCallback (save_w, 
                     XmNactivateCallback, 
                     SUMA_SaveTextShell, 
                     TextShell);    
      view_w = XtVaCreateManagedWidget (
                     "View",
                     xmPushButtonWidgetClass, 
                     rowcol_h, NULL);
      XtAddCallback (view_w, 
                     XmNactivateCallback, 
                     SUMA_ViewTextShellInEditor, 
                     TextShell); 
      if (TextShell->weblink) {
         web_w = XtVaCreateManagedWidget (
                     "WHelp",
                     xmPushButtonWidgetClass, 
                     rowcol_h, NULL);
         XtAddCallback (web_w, 
                        XmNactivateCallback, 
                        SUMA_WebTextShell, 
                        TextShell); 
         MCW_set_widget_bg( web_w , MCW_buthighlight(web_w) , 0 ) ;
      }
      close_w = XtVaCreateManagedWidget (
                     "Close", 
                     xmPushButtonWidgetClass, 
                     rowcol_h, NULL);
      XtAddCallback (close_w, 
                     XmNactivateCallback, 
                     SUMA_DestroyTextShell, 
                     TextShell);    

      XtManageChild (rowcol_h);
      
      SUMA_LH("Text output widget\n");
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
                                  
      SUMA_LHv("String %s\n",s?s:"NULL");
      if (!s) {
         XmTextSetString (TextShell->text_w, "No Messages.\n---------------\n");
      } else {
         XmTextSetString (TextShell->text_w, s);
      }   
      XtManageChild (TextShell->text_w);
      
      SUMA_LH("Adding callback\n");
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
   } else { /* already created, just replace text and perhaps title in title bar 
               (in the future)*/
      XmTextPosition nlpos, otpos;
      
      otpos = XmTextGetInsertionPosition(TextShell->text_w);/*current position*/
      SUMA_LH("Setting string in previously created text shell window.\n");
      
      if (!s) XmTextSetString (TextShell->text_w, 
                               "No Messages.\n---------------\n");
      else XmTextSetString (TextShell->text_w, s);
      nlpos = XmTextGetLastPosition (TextShell->text_w); /*new last position*/
      if (TextShell->CursorAtBottom) {
         XmTextSetInsertionPosition(TextShell->text_w, nlpos);
      } else {
         n = 1;
         XtVaGetValues (TextShell->text_w, XmNrows, &n, NULL);
         SUMA_LHv("previous pos %d, current last pos %d, num_rows %d\n",
                        (int)otpos, (int)nlpos, n);
         /* for some reason sometimes n is undefined, put restrictions */
         if (n<0) n = 10;  /* Bug should not happen */
         if (n>40) n = 40; /* Guarding for too big a shift
                              Might not be necessary */
         /* try to put the cursor in a comparable position */
         if ( (int)otpos < (int)nlpos ) {
            XmTextSetInsertionPosition(TextShell->text_w, otpos);
         } else {
            XmTextSetInsertionPosition(TextShell->text_w, nlpos);
         }
         if (n) XmTextScroll(TextShell->text_w, n/2);
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
void SUMA_cb_search_text(Widget widget, 
                  XtPointer client_data, XtPointer call_data)
{
   char *search_pat, *p, *string, buf[65];
   XmTextPosition pos;
   int len, i;
   Boolean found = False;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell;
   static char FuncName[]={"SUMA_cb_search_text"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *)client_data;
   if (!TextShell) { SUMA_SL_Err("Unexpected NULL TextShell"); SUMA_RETURNe; }
   
   /* get the text that is about to be searched */
   if (!(string = XmTextGetString (TextShell->text_w)) || !*string) {
     XmTextSetString (TextShell->text_output, "No text to search.");
     if (string) XtFree (string); /* may have been ""; free it */
     SUMA_RETURNe;
   }
   SUMA_LHv("Looking for %s\n", string);
   len = strlen(string);
   if (!TextShell->case_sensitive) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Case insensitive search.\n", FuncName);
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
   SUMA_LHv("Pat %s\n", search_pat);
   
   if (!TextShell->case_sensitive) {
      /* turn search_pat to lowercase */
      for (i=0; i < len; ++i) search_pat[i] = tolower(search_pat[i]);  
   }
   /* start searching at current cursor position + 1 to find
   * the -next- occurrance of string.  we may be sitting on it.
   */
   SUMA_LH("Looking down");
   pos = XmTextGetCursorPosition (TextShell->text_w);
   for (p = &string[pos+1]; (p = index (p, *search_pat)); p++)
     if (!strncmp (p, search_pat, len)) {
         found = True;
         break;
     }
   if (!found) { /* didn't find pattern? */
     /* search from beginning till we've passed "pos" */
      SUMA_LH("Looking from top");
     for (p = string;
             (p = index (p, *search_pat)) && p - string <= pos; p++)
         if (!strncmp (p, search_pat, len)) {
             found = True;
             break;
         }
   }
   
   if (!found) {
      SUMA_LH("Got nothing");
     XmTextSetString (TextShell->text_output, "Pattern not found.");
   } else {
     pos = (XmTextPosition)(p - string);
     snprintf (buf,sizeof(char)*64, "Pattern found at position %ld.", pos);
     SUMA_LH("%s",buf);
     XmTextSetString (TextShell->text_output, buf);
     SUMA_LH("insertion");
     XmTextSetInsertionPosition (TextShell->text_w, pos);
     SUMA_LH("highlight");
     XmTextSetHighlight(TextShell->text_w, pos, pos+len, XmHIGHLIGHT_SELECTED);
     SUMA_LH("freeatlast");
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
void SUMA_cb_SetDrawROI_SaveMode(Widget widget, XtPointer client_data, 
                                 XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDrawROI_SaveMode"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   datap = (SUMA_MenuCallBackData *)client_data;
   
   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s: Setting SaveMode to %d\n", 
               FuncName, (INT_CAST)datap->callback_data);
   SUMAg_CF->X->DrawROI->SaveMode = (INT_CAST)datap->callback_data; 
   
   SUMA_RETURNe;
}


/*!
   \brief sets the " what distance" parameter
   - expects a SUMA_MenuCallBackData * in  client_data
   Nothing in client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetDrawROI_WhatDist(Widget widget, XtPointer client_data, 
                                 XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDrawROI_WhatDist"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   datap = (SUMA_MenuCallBackData *)client_data;

   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s: Setting WhatDist to %d\n", 
               FuncName, (INT_CAST)datap->callback_data);
   SUMAg_CF->X->DrawROI->WhatDist = (INT_CAST)datap->callback_data; 
   
   SUMA_RETURNe;
} 

/*!
   \brief sets the "saving what" parameter
   - expects a SUMA_MenuCallBackData * in  client_data
   Nothing in client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetDrawROI_SaveWhat(Widget widget, XtPointer client_data, 
                                    XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDrawROI_SaveWhat"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   datap = (SUMA_MenuCallBackData *)client_data;
   
   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s: Setting SaveWhat to %d\n", 
               FuncName, (INT_CAST)datap->callback_data);
   SUMAg_CF->X->DrawROI->SaveWhat = (INT_CAST)datap->callback_data; 
   
   SUMA_RETURNe;
}
   
/*!
   \brief sets the rendering mode of a surface 
   
   - expects a SUMA_MenuCallBackData * in  client_data
   with SO as client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetRenderMode(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetRenderMode"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceObject *SO = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
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
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                       SES_SumaWidget, NULL);   
   ED = SUMA_InitializeEngineListData (SE_SetRenderMode);
   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)SO,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURNe;                                     
   }
   
   
   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURNe;    
   }
   
   SUMA_RETURNe;
}

int SUMA_RenderMode2RenderModeMenuItem(int Mode)
{
   static char FuncName[]={"SUMA_RenderMode2RenderModeMenuItem"};
   
   SUMA_ENTRY;
   
   if ((Mode) >= SW_N_SurfCont_Render || 
       (Mode) <=  SW_SurfCont_Render) {
      SUMA_S_Err("Bad mode, returning FILL");    
      SUMA_RETURN(SW_SurfCont_RenderFill);
   }
   
   SUMA_RETURN(Mode);
}      

/*!
   \brief sets the transparency mode of a surface 
   
   - expects a SUMA_MenuCallBackData * in  client_data
   with SO as client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetTransMode(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetTransMode"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceObject *SO = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   switch (imenu) {
      case SW_SurfCont_TransViewerDefault:
         imenu = STM_ViewerDefault;
         break;
      case SW_SurfCont_Trans0:
         imenu = STM_0;
         break;
      case SW_SurfCont_Trans1:
         imenu = STM_1;
         break;
      case SW_SurfCont_Trans2:
         imenu = STM_2;
         break;
      case SW_SurfCont_Trans3:
         imenu = STM_3;
         break;
      case SW_SurfCont_Trans4:
         imenu = STM_4;
         break;
      case SW_SurfCont_Trans5:
         imenu = STM_5;
         break;
      case SW_SurfCont_Trans6:
         imenu = STM_6;
         break;
      case SW_SurfCont_Trans7:
         imenu = STM_7;
         break;
      case SW_SurfCont_Trans8:
         imenu = STM_8;
         break;
      case SW_SurfCont_Trans9:
         imenu = STM_9;
         break;
      case SW_SurfCont_Trans10:
         imenu = STM_10;
         break;
      case SW_SurfCont_Trans11:
         imenu = STM_11;
         break;
      case SW_SurfCont_Trans12:
         imenu = STM_12;
         break;
      case SW_SurfCont_Trans13:
         imenu = STM_13;
         break;
      case SW_SurfCont_Trans14:
         imenu = STM_14;
         break;
      case SW_SurfCont_Trans15:
         imenu = STM_15;
         break;
      case SW_SurfCont_Trans16:
         imenu = STM_16;
         break;
      default: 
         fprintf (SUMA_STDERR, "Error %s: Unexpected widget index.\n", FuncName);
         break;
   }
   
   
   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                       SES_SumaWidget, NULL);   
   ED = SUMA_InitializeEngineListData (SE_SetTransMode);
   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)SO,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURNe;                                     
   }
   
   
   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURNe;    
   }
   
   SUMA_RETURNe;
}

/*!
   \brief sets the transparency mode of a volume/slices 
   
   - expects a SUMA_MenuCallBackData * in  client_data
   with ado as client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetATransMode(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetATransMode"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   switch (imenu) {
      case SW_SurfCont_ATransViewerDefault:
         imenu = SATM_ViewerDefault;
         break;
      case SW_SurfCont_Alpha:
         imenu = SATM_ALPHA;
         break;
      case SW_SurfCont_ATrans0:
         imenu = SATM_0;
         break;
      case SW_SurfCont_ATrans1:
         imenu = SATM_1;
         break;
      case SW_SurfCont_ATrans2:
         imenu = SATM_2;
         break;
      case SW_SurfCont_ATrans3:
         imenu = SATM_3;
         break;
      case SW_SurfCont_ATrans4:
         imenu = SATM_4;
         break;
      case SW_SurfCont_ATrans5:
         imenu = SATM_5;
         break;
      case SW_SurfCont_ATrans6:
         imenu = SATM_6;
         break;
      case SW_SurfCont_ATrans7:
         imenu = SATM_7;
         break;
      case SW_SurfCont_ATrans8:
         imenu = SATM_8;
         break;
      case SW_SurfCont_ATrans9:
         imenu = SATM_9;
         break;
      case SW_SurfCont_ATrans10:
         imenu = SATM_10;
         break;
      case SW_SurfCont_ATrans11:
         imenu = SATM_11;
         break;
      case SW_SurfCont_ATrans12:
         imenu = SATM_12;
         break;
      case SW_SurfCont_ATrans13:
         imenu = SATM_13;
         break;
      case SW_SurfCont_ATrans14:
         imenu = SATM_14;
         break;
      case SW_SurfCont_ATrans15:
         imenu = SATM_15;
         break;
      case SW_SurfCont_ATrans16:
         imenu = SATM_16;
         break;
      default: 
         fprintf (SUMA_STDERR, "Error %s: Unexpected widget index.\n", FuncName);
         break;
   }
   
   
   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                       SES_SumaWidget, NULL);   
   ED = SUMA_InitializeEngineListData (SE_SetATransMode);
   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURNe;                                     
   }
   
   
   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURNe;    
   }
   
   SUMA_RETURNe;
}

int SUMA_TransMode2TransModeMenuItem(int Mode)
{
   static char FuncName[]={"SUMA_TransMode2TransModeMenuItem"};
   
   SUMA_ENTRY;
   
   if ((Mode) >= SW_N_SurfCont_Trans || 
       (Mode) <=  SW_SurfCont_Trans) {
      SUMA_S_Err("Bad mode, returning 0");    
      SUMA_RETURN(SW_SurfCont_Trans0);
   }
   
   SUMA_RETURN(Mode);
}      

int SUMA_ATransMode2ATransModeMenuItem(int Mode)
{
   static char FuncName[]={"SUMA_ATransMode2ATransModeMenuItem"};
   
   SUMA_ENTRY;
   
   if ((Mode) >= SW_N_SurfCont_ATrans || 
       (Mode) <=  SW_SurfCont_ATrans) {
      SUMA_S_Err("Bad mode, returning 0");    
      SUMA_RETURN(SW_SurfCont_ATrans0);
   }
   
   SUMA_RETURN(Mode);
}

  

int SUMA_ShowMode2ShowModeMenuItem(int Mode)
{
   static char FuncName[]={"SUMA_ShowMode2ShowModeMenuItem"};
   
   SUMA_ENTRY;
   
   if (SUMA_ABS(Mode) >= SW_N_SurfCont_DsetView || 
       SUMA_ABS(Mode) <= SW_SurfCont_DsetView ) {
      SUMA_S_Err("Bad mode, returning FULL");    
      SUMA_RETURN(SW_SurfCont_DsetViewCol);
   }
   if (Mode < 0) {
      SUMA_RETURN(SW_SurfCont_DsetViewXXX);
   } else {
      SUMA_RETURN(Mode);
   }
}      


int SUMA_ShowModeStr2ShowModeMenuItem(char *str) 
{
   static char FuncName[]={"SUMA_ShowModeStr2ShowModeMenuItem"};
   
   SUMA_ENTRY;
   if (!str) {
      SUMA_S_Err("NULL str, returning view color");    
      SUMA_RETURN(SW_SurfCont_DsetViewCol);
   }
   if (!strcasecmp(str,"xxx")) 
       SUMA_RETURN(SW_SurfCont_DsetViewXXX);
   else if (!strcasecmp(str,"col")) 
       SUMA_RETURN(SW_SurfCont_DsetViewCol);
   else if (!strcasecmp(str,"con")) 
       SUMA_RETURN(SW_SurfCont_DsetViewCon);
   else if (!strcasecmp(str,"c&c")) 
       SUMA_RETURN(SW_SurfCont_DsetViewCaC);
   else {
      SUMA_S_Errv("'%s' is not a valid show mode, returning view col", str);
      SUMA_RETURN(SW_SurfCont_DsetViewCol);
   }
      
}

int SUMA_NodeRad2NodeRadMenuItem(int Mode)
{
   static char FuncName[]={"SUMA_NodeRad2NodeRadMenuItem"};
   
   SUMA_ENTRY;
   
   if (SUMA_ABS(Mode) >= SW_N_SurfCont_DsetNodeRad || 
       SUMA_ABS(Mode) <= SW_SurfCont_DsetNodeRad ) {
      SUMA_S_Err("Bad mode, returning Const");    
      SUMA_RETURN(SW_SurfCont_DsetNodeRadConst);
   }
   if (Mode < 0) {
      SUMA_RETURN(SW_SurfCont_DsetNodeRadXXX);
   } else {
      SUMA_RETURN(Mode);
   }
}      

int SUMA_NodeRadStr2NodeRadMenuItem(char *str) 
{
   static char FuncName[]={"SUMA_NodeRadStr2NodeRadMenuItem"};
   
   SUMA_ENTRY;
   if (!str) {
      SUMA_S_Err("NULL str, returning Const");    
      SUMA_RETURN(SW_SurfCont_DsetNodeRadConst);
   }
   if (!strcasecmp(str,"xxx")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeRadXXX);
   else if (!strcasecmp(str,"cst") || !strcasecmp(str,"const")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeRadConst);
   else if (!strcasecmp(str,"val")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeRadVal);
   else {
      SUMA_S_Errv("'%s' is not a valid show mode, returning Const", str);
      SUMA_RETURN(SW_SurfCont_DsetNodeRadConst);
   }
      
}

int SUMA_Through2ThroughMenuItem(int Mode)
{
   static char FuncName[]={"SUMA_Through2ThroughMenuItem"};
   
   SUMA_ENTRY;
   
   if (SUMA_ABS(Mode) >= SW_N_SurfCont_DsetThrough || 
       SUMA_ABS(Mode) <= SW_SurfCont_DsetThrough ) {
      SUMA_S_Err("Bad mode, returning Edge");    
      SUMA_RETURN(SW_SurfCont_DsetThroughEdge);
   }
   if (Mode < 0) {
      SUMA_RETURN(SW_SurfCont_DsetThroughXXX);
   } else {
      SUMA_RETURN(Mode);
   }
}      

int SUMA_ThroughStr2ThroughMenuItem(char *str) 
{
   static char FuncName[]={"SUMA_ThroughStr2ThroughMenuItem"};
   
   SUMA_ENTRY;
   if (!str) {
      SUMA_S_Err("NULL str, returning Edge");    
      SUMA_RETURN(SW_SurfCont_DsetThroughEdge);
   }
   if (!strcasecmp(str,"xxx")) 
       SUMA_RETURN(SW_SurfCont_DsetThroughXXX);
   else if (!strcasecmp(str,"edg") || !strcasecmp(str,"edge")) 
       SUMA_RETURN(SW_SurfCont_DsetThroughEdge);
   else if (!strcasecmp(str,"col") || !strcasecmp(str,"color")) 
       SUMA_RETURN(SW_SurfCont_DsetThroughCol);
   else if (!strcasecmp(str,"rad") || !strcasecmp(str,"radius")) 
       SUMA_RETURN(SW_SurfCont_DsetThroughRad);
   else if (!strcasecmp(str,"c&r")) 
       SUMA_RETURN(SW_SurfCont_DsetThroughCaR);
   else {
      SUMA_S_Errv("'%s' is not a valid show mode, returning Edge", str);
      SUMA_RETURN(SW_SurfCont_DsetThroughEdge);
   }
      
}

int SUMA_NodeCol2NodeColMenuItem(int Mode)
{
   static char FuncName[]={"SUMA_NodeCol2NodeColMenuItem"};
   
   SUMA_ENTRY;
   
   if (SUMA_ABS(Mode) >= SW_N_SurfCont_DsetNodeCol || 
       SUMA_ABS(Mode) <= SW_SurfCont_DsetNodeCol ) {
      SUMA_S_Err("Bad mode, returning Const");    
      SUMA_RETURN(SW_SurfCont_DsetNodeColWhite);
   }
   if (Mode < 0) {
      SUMA_S_Err("No hide mode for color, returning Const");
      SUMA_RETURN(SW_SurfCont_DsetNodeColWhite);
   } else {
      SUMA_RETURN(Mode);
   }
}      

int SUMA_NodeColStr2NodeColMenuItem(char *str) 
{
   static char FuncName[]={"SUMA_NodeColStr2NodeColMenuItem"};
   
   SUMA_ENTRY;
   if (!str) {
      SUMA_S_Err("NULL str, returning White");    
      SUMA_RETURN(SW_SurfCont_DsetNodeColWhite);
   }
   if (!strcasecmp(str,"wht") || !strcasecmp(str,"white")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeColWhite);
   else if (!strcasecmp(str,"blk") || !strcasecmp(str,"black")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeColBlack);
   else if (!strcasecmp(str,"red")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeColRed);
   else if (!strcasecmp(str,"grn") || !strcasecmp(str,"green")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeColGreen);
   else if (!strcasecmp(str,"blu") || !strcasecmp(str,"blue")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeColBlue);
   else if (!strcasecmp(str,"yel") ||!strcasecmp(str,"yellow")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeColYellow);
   else if (!strcasecmp(str,"g50") || !strcasecmp(str,"gray50")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeColGray50);
   else if (!strcasecmp(str,"val")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeColVal);
   else if (!strcasecmp(str,"grp")) 
       SUMA_RETURN(SW_SurfCont_DsetNodeColGrp);
   else {
      SUMA_S_Errv("'%s' is not a valid node color, returning White", str);
      SUMA_RETURN(SW_SurfCont_DsetNodeColWhite);
   }
      
}

float * SUMA_NodeCol2Col(int Mode, float *here) 
{
   static char FuncName[]={"SUMA_NodeCol2Col"};
   static int ncall=0;
   static float fv[10][4];
   
   SUMA_ENTRY;
   
   if (!here) {
      ncall++; if (ncall > 9) ncall=0;
      here = fv[ncall];
   }
   here[3] = 1.0;
   switch (Mode){
      default:
      case SW_SurfCont_DsetNodeColWhite:
         here[0] = here[1] = here[2] = 1.0;
         break;
      case SW_SurfCont_DsetNodeColBlack:
         here[0] = here[1] = here[2] = 0.0;
         break;
      case SW_SurfCont_DsetNodeColRed:
         here[0] = 1.0; here[1] = here[2] = 0.0;
         break;
      case SW_SurfCont_DsetNodeColGreen:
         here[0] = 0.0; here[1] = 1.0; here[2] = 0.0;
         break;
      case SW_SurfCont_DsetNodeColBlue:
         here[0] = 0.0; here[1] = 0.0; here[2] = 1.0;
         break;
      case SW_SurfCont_DsetNodeColYellow:
         here[0] = 1.0; here[1] = 1.0; here[2] = 0.0;
         break;
      case SW_SurfCont_DsetNodeColGray50:
         here[0] = 0.5; here[1] = 0.5; here[2] = 0.5;
         break;
   }
   
   SUMA_RETURN(here);
}

int SUMA_Font2FontMenuItem(int Mode)
{
   static char FuncName[]={"SUMA_Font2FontMenuItem"};
   
   SUMA_ENTRY;
   
   if (SUMA_ABS(Mode) >= SW_N_SurfCont_DsetFont || 
       SUMA_ABS(Mode) <= SW_SurfCont_DsetFont ) {
      SUMA_S_Err("Bad mode, returning Font 9");    
      SUMA_RETURN(SW_SurfCont_DsetFont9);
   }
   if (Mode < 0) {
      SUMA_RETURN(SW_SurfCont_DsetFontXXX);
   } else {
      SUMA_RETURN(Mode);
   }
}      

void * SUMA_Font2GLFont(int Mode)
{
   static char FuncName[]={"SUMA_Font2GLFont"};
   
   SUMA_ENTRY;
   
   if (SUMA_ABS(Mode) >= SW_N_SurfCont_DsetFont || 
       SUMA_ABS(Mode) <= SW_SurfCont_DsetFont ) {
      SUMA_S_Errv("Bad mode %d, returning Font 9", Mode);    
      SUMA_RETURN(GLUT_BITMAP_9_BY_15);
   }
   if (Mode < 0) {
      SUMA_RETURN(NULL);
   } else {
      switch(Mode) {
         case SW_SurfCont_DsetFont8:
            SUMA_RETURN(GLUT_BITMAP_8_BY_13);
         case SW_SurfCont_DsetFont9:
            SUMA_RETURN(GLUT_BITMAP_9_BY_15);
         case SW_SurfCont_DsetFontTR10:
            SUMA_RETURN(GLUT_BITMAP_TIMES_ROMAN_10);
         case SW_SurfCont_DsetFontTR24:
            SUMA_RETURN(GLUT_BITMAP_TIMES_ROMAN_24);
         case SW_SurfCont_DsetFontHE10:
            SUMA_RETURN(GLUT_BITMAP_HELVETICA_10);
         case SW_SurfCont_DsetFontHE12:
            SUMA_RETURN(GLUT_BITMAP_HELVETICA_12);
         case SW_SurfCont_DsetFontHE18:
            SUMA_RETURN(GLUT_BITMAP_HELVETICA_18);
         default:
            SUMA_RETURN(GLUT_BITMAP_9_BY_15);
      }
   }
   SUMA_RETURN(NULL);
}

int SUMA_FontStr2FontMenuItem(char *str) 
{
   static char FuncName[]={"SUMA_FontStr2FontMenuItem"};
   
   SUMA_ENTRY;
   if (!str) {
      SUMA_S_Err("NULL str, returning Font 9");    
      SUMA_RETURN(SW_SurfCont_DsetFont9);
   }
   if (!strcasecmp(str,"xxx")) 
       SUMA_RETURN(SW_SurfCont_DsetFontXXX);
   else if (!strcasecmp(str,"8") || !strcasecmp(str,"f8")) 
       SUMA_RETURN(SW_SurfCont_DsetFont8);
   else if (!strcasecmp(str,"9") || !strcasecmp(str,"f9")) 
       SUMA_RETURN(SW_SurfCont_DsetFont9);
   else if (!strcasecmp(str,"T10") || !strcasecmp(str,"TR10") ) 
       SUMA_RETURN(SW_SurfCont_DsetFontTR10);
   else if (!strcasecmp(str,"T24") || !strcasecmp(str,"TR24")) 
       SUMA_RETURN(SW_SurfCont_DsetFontTR24);
   else if (!strcasecmp(str,"H10") || !strcasecmp(str,"He10")) 
       SUMA_RETURN(SW_SurfCont_DsetFontHE10);
   else if (!strcasecmp(str,"H12") || !strcasecmp(str,"He12")) 
       SUMA_RETURN(SW_SurfCont_DsetFontHE12);
   else if (!strcasecmp(str,"H18") || !strcasecmp(str,"He18")) 
       SUMA_RETURN(SW_SurfCont_DsetFontHE18);
   else {
      SUMA_S_Errv("'%s' is not a valid Font, returning Font 9", str);
      SUMA_RETURN(SW_SurfCont_DsetFont9);
   }
}
/*!
   \brief sets the dataset viewing mode of the current dset  on SO
   
*/
int SUMA_SetDsetViewMode(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetViewMode"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetViewMode);
   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetViewModeMenu,
                            curColPlane->ShowMode);
   }

   SUMA_RETURN(YUP);
}

/*!
   \brief sets the dataset viewing mode of the current dset  
   
   - expects a SUMA_MenuCallBackData * in  client_data
   with SO as client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetDsetViewMode(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetViewMode"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetViewMode(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}

/*!
   \brief sets the dataset viewing mode of the current dset  on SO
   
*/
int SUMA_SetDsetFont(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetFont"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetFont);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetFontMenu,
                            curColPlane->Font);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetDsetFont(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetFont"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetFont(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}

int SUMA_SetDsetNodeRad(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetNodeRad"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetNodeRad);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetNodeRadMenu,
                            curColPlane->NodeRad);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetDsetNodeRad(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetNodeRad"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetNodeRad(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}

int SUMA_SetDsetThrough(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetThrough"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetThrough);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetThroughMenu,
                            curColPlane->Through);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetDsetThrough(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetThrough"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetThrough(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}

int SUMA_SetDsetEdgeThick(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetEdgeThick"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetEdgeThick);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetEdgeThickMenu,
                            curColPlane->EdgeThick);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetDsetEdgeThick(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetEdgeThick"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetEdgeThick(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}

int SUMA_SetDsetAlphaVal(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetAlphaVal"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetAlphaVal);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetAlphaValMenu,
                            curColPlane->AlphaVal);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetDsetAlphaVal(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetAlphaVal"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetAlphaVal(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}

int SUMA_SetDsetEdgeStip(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetEdgeStip"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetEdgeStip);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetEdgeStipMenu,
                            curColPlane->EdgeStip);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetDsetEdgeStip(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetEdgeStip"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetEdgeStip(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}

int SUMA_SetTractStyle(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetTractStyle"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetTractStyle);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->TractStyleMenu,
                            curColPlane->EdgeStip);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetTractStyle(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetTractStyle"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetTractStyle(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}


int SUMA_SetTractMask(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetTractMask"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_TRACT_SAUX *TSaux=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetTractMask);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (TSaux = SUMA_ADO_TSaux(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->TractMaskMenu,
                            TSaux->TractMask);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetTractMask(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetTractMask"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetTractMask(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}

int SUMA_SetDsetNodeCol(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetNodeCol"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetNodeCol);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetNodeColMenu,
                            curColPlane->NodeCol);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetDsetNodeCol(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetNodeCol"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetNodeCol(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}

int SUMA_SetDsetTxtShad(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetTxtShad"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetTxtShad);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetTxtShadMenu,
                            curColPlane->TxtShad);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetDsetTxtShad(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetTxtShad"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetTxtShad(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
      SUMA_RETURNe;
   }
      
   
   SUMA_RETURNe;
}


int SUMA_SetDsetGmatBord(SUMA_ALL_DO *ado, int imenu, int updatemenu) 
{
   static char FuncName[]={"SUMA_SetDsetGmatBord"};
   DList *list = NULL;
   DListElmt *Elmnt = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_ENTRY;

   /* make a call to SUMA_Engine */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetDsetGmatBord);

   Elmnt = SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_i, (void *)&imenu,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL);
   if (!SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ado,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_In, Elmnt)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_RegisterEngineListCommand.\n", 
               FuncName);
      SUMA_RETURN(NOPE);                                     
   }


   if (!SUMA_Engine (&list)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Engine.\n", FuncName);
      SUMA_RETURN(NOPE);    
   }

   if (updatemenu && 
       (SurfCont = SUMA_ADO_Cont(ado)) &&
       (curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_Set_Menu_Widget( SurfCont->DsetGmatBordMenu,
                            curColPlane->BordFrac);
   }

   SUMA_RETURN(YUP);
}

void SUMA_cb_SetDsetGmatBord(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDsetGmatBord"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   int imenu = 0;
   
   SUMA_ENTRY;

   
   /* get the  object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (!SUMA_SetDsetGmatBord(ado, imenu, 0)) {
      SUMA_S_Err("Failed to set view mode");
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

   s = (char *)SUMA_calloc (strlen(MD->Message)+strlen(MD->Source)+100, 
                            sizeof(char));
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

SUMA_Boolean SUMA_OpenDrawROIController(SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_OpenDrawROIController"};
   DList *list = NULL;
   
   SUMA_ENTRY;
   
   if (!sv) {
      sv = (void*)&SUMAg_SVv[0];
   }
   if (!sv) SUMA_RETURN(NOPE);
   
   /* register a call to open the ROI editor */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, 
                                       SE_OpenDrawROI, SES_SumaWidget, 
                                       (void*)sv); 
   if (!SUMA_Engine (&list)) {
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                            "Failed to open DrawROI", 
                            FuncName, SMT_Error, SMA_LogAndPopup);
      SUMA_RETURN(NOPE);
   }  
   
   SUMA_RETURN(YUP);
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
   SUMA_MenuCallBackData * datap=NULL;
   
   SUMA_ENTRY;

   /* get the surface viewer that the command was made in */
   datap = (SUMA_MenuCallBackData *)client_data;
   isv = (INT_CAST)datap->ContID;
   if (isv < 0 || isv >= SUMAg_N_SVv) {
      SUMA_S_Err("Bad baby");
      SUMA_RETURNe;
   }
   
   SUMA_OpenDrawROIController((&SUMAg_SVv[isv]));
   /* Turn on Draw Mode, if it is OFF */
   if (!SUMAg_CF->ROI_mode) {
      XmToggleButtonSetState (SUMAg_CF->X->DrawROI->DrawROImode_tb, 
                              YUP, YUP);
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
   
   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s: Calling SUMA_UndoAction...\n", FuncName);
   
   if (!SUMAg_CF->X->DrawROI->curDrawnROI->StackPos) {
      SUMA_SLP_Err("Nothing to Undo.");
      SUMA_RETURNe;
   }
   
   tmp = SUMA_UndoAction (SUMAg_CF->X->DrawROI->curDrawnROI->ActionStack, 
                           SUMAg_CF->X->DrawROI->curDrawnROI->StackPos);
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
   if (!SUMA_Paint_SO_ROIplanes_w ( 
         SUMA_findSOp_inDOv(  
              SUMAg_CF->X->DrawROI->curDrawnROI->Parent_idcode_str, 
              SUMAg_DOv, SUMAg_N_DOv), 
         SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURNe;
   } 

   /* place a call to redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                       SES_Suma, NULL);
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
   
   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s: Calling SUMA_RedoAction...\n", FuncName);
   
   if (SUMAg_CF->X->DrawROI->curDrawnROI->StackPos == 
         dlist_tail(SUMAg_CF->X->DrawROI->curDrawnROI->ActionStack)) {
      SUMA_SLP_Err("Nothing to Redo.");
      SUMA_RETURNe;
   }
   
   tmp = SUMA_RedoAction (SUMAg_CF->X->DrawROI->curDrawnROI->ActionStack, 
                           SUMAg_CF->X->DrawROI->curDrawnROI->StackPos);
   if (!tmp) {
      SUMA_S_Err("Failed to Redo.");
      SUMA_RETURNe;
   }else {
      SUMAg_CF->X->DrawROI->curDrawnROI->StackPos = tmp;
   }
   

   /* do the paint thing */
   /* Now update the Paint job on the ROI plane */
   if (!SUMA_Paint_SO_ROIplanes_w ( 
         SUMA_findSOp_inDOv(
               SUMAg_CF->X->DrawROI->curDrawnROI->Parent_idcode_str, 
               SUMAg_DOv, SUMAg_N_DOv), 
         SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURNe;
   } 

   /* place a call to redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                      SES_Suma, NULL);
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
      SUMA_LH("First Press, adding time out.");
      (void) XtAppAddTimeOut(
               XtWidgetToApplicationContext(SUMAg_CF->X->DrawROI->Delete_pb) ,
               5000 , SUMA_delete_timeout_CB , NULL ) ;

       SUMA_RETURNe;
   }
   
   /* delete ROI */
   ErrCnt = 0;
   SUMA_LHv("Should be deleting ROI %s here ...\n",
            SUMAg_CF->X->DrawROI->curDrawnROI->Label);
   
   /* preserve some info about ROI to be deleted */
   SO = SUMA_findSOp_inDOv(SUMAg_CF->X->DrawROI->curDrawnROI->Parent_idcode_str ,
                           SUMAg_DOv, SUMAg_N_DOv);
   PlaneName = SUMA_copy_string(SUMAg_CF->X->DrawROI->curDrawnROI->ColPlaneName);
   
   if (!SUMA_DeleteROI (SUMAg_CF->X->DrawROI->curDrawnROI)) {
      SUMA_SLP_Err("Failed to delete ROI");
      SUMA_RETURNe; 
   }
   
   /* If no other ROIs remain on the same plane as the deleted ROI, 
      flush that plane's colors */
   SUMA_FlushPlaneNotInUse (PlaneName, (SUMA_ALL_DO *)SO, 
                            SUMAg_DOv, SUMAg_N_DOv);
   if (PlaneName) SUMA_free(PlaneName);
   
   /* Now update the Paint job on the ROI plane */
   if (!SUMA_Paint_SO_ROIplanes_w (
         SO, SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURNe;
   }

   /* redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                      SES_Suma, NULL); 
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
      SUMA_RETURNe;
   }
   
   if (DrawnROI->DrawStatus != SUMA_ROI_Finished) {
      SUMA_LH("unFinished");
      /* YOU DO NOT WANT TO FREE n because n is not a copy of the string 
         in the widget! */
      if (DrawnROI->Label) {
         SUMA_LH("Changing ROI label from %s to %s\n", 
                 DrawnROI->Label, (char *)n);         
         DrawnROI->Label = (char *)SUMA_realloc(DrawnROI->Label, 
                                          sizeof(char)*(strlen((char *)n)+1));
      }  else {
         SUMA_LH("Setting ROI label to %s\n", (char *)n);
         DrawnROI->Label = (char *)SUMA_malloc(sizeof(char) * 
                                                (strlen((char *)n)+1));
      }
      DrawnROI->Label = strcpy(DrawnROI->Label, (char *)n);   
      ErrCnt = 0;
      
      /* check if list window is open and update it if need be */
      SUMA_IS_DRAW_ROI_SWITCH_ROI_SHADED(Shaded);
      if (!Shaded) {
         SUMA_LH("updating switch ROI window ...\n");
         SUMA_cb_DrawROI_SwitchROI(NULL, (XtPointer) 
                                 SUMAg_CF->X->DrawROI->SwitchROIlst, NULL);
      }
   } else {
      SUMA_LH("Finished");
      if (!ErrCnt) SUMA_SLP_Err("ROI maked as finished.\n"
                                "New label cannot be applied.");
      ++ErrCnt;
      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIlbl->textfield, 
                        DrawnROI->Label);
   }
   
   SUMA_RETURNe;
}

/* 
Children of New are always called with a resize if they want one.
When  New is sv->X->TOPLEVEL a resize  callback is initiated on 
the GLXAREA with adjusted width and height values. 
*/
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
   
   This function is a working fallback function in case the mysterious
   <Error>: kCGErrorIllegalArgument: CGSBindSurface: Invalid window 0x2fa
   start coming back with the new function on DARWIN
   
   See SUMA_PositionWindowRelative_current()
*/
void SUMA_PositionWindowRelative_orig (  Widget New, Widget Ref, 
                                    SUMA_WINDOW_POSITION Loc)
{
   static char FuncName[]={"SUMA_PositionWindowRelative_orig"};
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
            int root_x, root_y, win_x, win_y, isv;
            unsigned int keys_buttons;
            SUMA_LH("Pointer Query 1");
            if (!XtIsRealized(New)) {
               SUMA_LH("Need new wind");
               isv = 0;
               while (isv < SUMAg_N_SVv &&
                      !XtIsRealized(SUMAg_SVv[isv].X->GLXAREA)) ++isv;
               if (isv < SUMAg_N_SVv) {
                  wind = XtWindow(SUMAg_SVv[isv].X->GLXAREA);
               } else {
                  SUMA_SL_Err("Nothing to work with here!");
                  SUMA_RETURNe;
               }
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
         case SWP_POINTER_LEFT_BOTTOM:
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
            NewX = root_x - (int)Dx*2;
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

/*!
   \brief Positions a new widget relative to a reference widget 
   SUMA_PositionWindowRelative ( New,  Ref,  Loc);
   
   \param New (Widget) the widget to place
   \param Ref (Widget) the widget relative to which New is placed (could pass NULL if positioning relative to pointer)
   
   \param Loc (SUMA_WINDOW_POSITION) the position of New relative to Ref
*/
void SUMA_PositionWindowRelative_current (  Widget New, Widget Ref, 
                                    SUMA_WINDOW_POSITION Loc)
{
   static char FuncName[]={"SUMA_PositionWindowRelative_current"};
   Position RefX=0, RefY=0, NewX=0, NewY=0, Dx=5, RootX=0, RootY=0;
   Dimension RefW=0, RefH=0, ScrW=0, ScrH=0, NewW=0, NewH=0;
   Position MinOK=150;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_DUMP_TRACE("Who Called SUMA_PositionWindowRelative_current");
   }
   
   if (!New) { SUMA_RETURNe; }
   
   ScrW = WidthOfScreen (XtScreen(New));
   ScrH = HeightOfScreen (XtScreen(New));
   
   SUMA_LH("Getting New Positions: screen w%d, h%d", ScrW, ScrH);
   XtVaGetValues (New,           /* get the positions of New */
         XmNwidth, &NewW,
         XmNheight, &NewH,
         XmNx, &NewX,
         XmNy, &NewY,
         NULL);
   /* For some reason, all the New parameters are 0 or close to 0 
   when the  controller is open in the absence of surfaces. Not
   sure why that is the case yet, but going down that route might
   provide a clue */ 
   SUMA_LH("Current New Position: x%d, y%d, w%d, h%d", NewX, NewY, NewW, NewH);
   
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
      SUMA_LHv("Got Ref Positions,  x%d y%d, W%d H%d\n"
               "    Root Positions, x%d y%d\n",
                  RefX, RefY, RefW, RefH, 
                  RootX, RootY);
      /* RefX = RootX; RefY=RootY; Does not work, at least on macs... */
      #ifdef DARWIN
      if (SUMAg_CF->X->roffx == -1) {
         /* For some insane reason the block below causes weird errors 
            on OSX 10.8 (and possibly older). Basically a call
            to  XGetWindowAttributes(SUMAg_CF->X->DPY_controller1, 
                               XtWindow(SUMAg_SVv[0].X->GLXAREA), &wa);
            or variants on it generated a boat load of :
          kCGErrorIllegalArgument: CGSSetWindowSendExposed: Invalid window 0x8a0:
          and other variants.  The slew of errors ends with something like:
          error: xp_attach_gl_context returned: 2
          which triggers:
          Error SUMA_handleRedisplay: Failed in glXMakeCurrent.
                  Continuing ...
          SUMA_handleRedisplay (via SUMA_GL_ERRS): Looking for OpenGL errors ...
          GL error 1: invalid framebuffer operation

          This can result in messed up rendering buffers.
          
          Why this happens I have no idea but I have wasted enough of my life 
          on this already and then some.
         */
         SUMAg_CF->X->roffx = 0;
         SUMAg_CF->X->roffy = 22;
      }
      #else
      if (SUMAg_CF->X->roffx == -1) {
         XWindowAttributes wa;
         Display *dd=NULL;
         Window ww;
         dd = XtDisplay(Ref);
         ww = XtWindow(Ref);
         if (dd && ww && XGetWindowAttributes(dd, ww, &wa)) {
            if (wa.y > 0) { /* I have had XGetWindowAttributes
                             fail at times and return 0 0 for offsets. */
               SUMA_LHv("Setting offsets to %d %d\n", wa.x, wa.y);
               SUMAg_CF->X->roffx = wa.x;
               SUMAg_CF->X->roffy = wa.y;
            } else {
               SUMA_LH("Failed to get reasonable offset.");
               SUMAg_CF->X->roffx = 0; SUMAg_CF->X->roffy = 0;
            }
         } else {
            SUMA_LH("Failed to get window attributes, approximating");
            SUMAg_CF->X->roffx = 0;
            SUMAg_CF->X->roffy = 22;
         } 
      }
      #endif
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
         NewX = (Position)(RefW + RefX + Dx - SUMAg_CF->X->roffx);
         NewY = (Position)(RefY - SUMAg_CF->X->roffy);
         break;
      case SWP_TOP_LEFT:
         NewX = (Position)(RefW + Dx - SUMAg_CF->X->roffx);
         NewY = (Position)(RefY - SUMAg_CF->X->roffy);
         break;
      case SWP_TOP_BEST:
         /* aim for TOP_RIGHT */
         NewX = (Position)(RefW + RefX + Dx - SUMAg_CF->X->roffx);
         NewY = (Position)(RefY - SUMAg_CF->X->roffy);
         SUMA_LH("NewX  %d", NewX);
         if (NewX >= ScrW-NewW) { /* Try TOP_LEFT side with no overlap */
            NewX = (Position)(RefX + Dx - SUMAg_CF->X->roffx - NewW);
         }
         SUMA_LH("NewX now %d", NewX);
         if (NewX < 0) NewX = MinOK;
         if (NewY >= ScrH ) NewY = ScrH-MinOK;
         if (NewY < 0) NewY = MinOK;
         break;
      case SWP_STEP_DOWN_RIGHT:
         NewY = RefY + 10;
         NewX = RefX + 10;
         break;
      case SWP_POINTER:
         {
            Window root, child, wind;
            int root_x, root_y, win_x, win_y, isv;
            unsigned int keys_buttons;
            SUMA_LH("Pointer Query 1");
            if (!XtIsRealized(New)) {
               SUMA_LH("Need new wind");
               isv = 0;
               while (isv < SUMAg_N_SVv &&
                      !XtIsRealized(SUMAg_SVv[isv].X->GLXAREA)) ++isv;
               if (isv < SUMAg_N_SVv) {
                  wind = XtWindow(SUMAg_SVv[isv].X->GLXAREA);
               } else {
                  SUMA_SL_Err("Nothing to work with here!");
                  SUMA_RETURNe;
               }
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
         case SWP_POINTER_LEFT_BOTTOM:
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
            NewX = root_x - (int)Dx*13;
            NewY = root_y - (int)NewH + Dx;
         }
         break;
            
      default:
         fprintf (SUMA_STDERR, "Error %s: Option not known.\n", FuncName);
         SUMA_RETURNe;
         break;
   }

   
   if (NewX >= ScrW-MinOK) NewX = ScrW-MinOK;
   if (NewX < 0) NewX = MinOK;
   if (NewY >= ScrH ) NewY = ScrH-MinOK;
   if (NewY < 0) NewY = MinOK;
   
   SUMA_LH("Positioning window at %d %d\n", NewX, NewY);
   XtVaSetValues (New,
      XmNx, NewX,
      XmNy, NewY,
      NULL);
      
   SUMA_RETURNe;
}

void SUMA_PositionWindowRelative (  Widget New, Widget Ref, 
                                    SUMA_WINDOW_POSITION Loc)
{                              
   SUMA_PositionWindowRelative_current(New, Ref, Loc);
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
SUMA_PROMPT_DIALOG_STRUCT *SUMA_CreatePromptDialogStruct (
      SUMA_PROMPT_MODE Mode, char *TextFieldLabel, 
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
      prmpt->pane = XtVaCreateWidget ("pane", xmPanedWindowWidgetClass, 
                                      prmpt->dialog,
        XmNsashWidth,  1,
        XmNsashHeight, 1,
        NULL);

      /* create the control area which contains a
      * Label gadget and a TextField widget.
      */
      rc = XtVaCreateWidget ("control_area", xmRowColumnWidgetClass, 
                             prmpt->pane, NULL);
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
      XtAddCallback (prmpt->text_w, XmNactivateCallback, 
                     SUMA_PromptActivate_cb, (XtPointer)prmpt);
      
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
   
   SUMA_LH("Read %s\n", prmpt->selection);

   /* verify the input */
   if (prmpt->VerifyFunction) {
      if (!prmpt->VerifyFunction(prmpt->selection, prmpt->VerifyData)) {
         SUMA_SLP_Err("Gibberish! try again.\n"
                      "Syntax error or wrong\n"
                      "number/type of arguments.\n"
                      "See command line for more specific errors.");
         SUMA_RETURNe;
      }
   }
   
   /* do your selectcallback */
   if (prmpt->SelectCallback) {
      prmpt->SelectCallback(prmpt->selection, prmpt->SelectData);
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
         SUMA_LHv("Reseting dlg->FilePattern to %s\n", FilePattern);
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
      /* Do not carry out tests here as filename might change
         once proper extensions are added */
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
         SUMA_SLP_Err("Can't read %s.", filename);
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

   XtUnmanageChild (dlg->dlg_w); /* this function will call the unmap 
                                    callback which will do the destruction 
                                    if dialog is not to be preserved */

   SUMA_RETURNe;
}

/* This function is for hiding the color plane frame */
void  SUMA_cb_ToggleManagementColPlaneWidget(Widget w, XtPointer data, 
                                             XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ToggleManagementColPlaneWidget"};
   static int ncall=1;
   SUMA_ALL_DO *ado = NULL;
   int xx, yy, notest = 0;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
#ifdef USING_LESSTIF
      notest = 1;
#endif

   if (ncall > 0) {
      if (XtIsManaged(SurfCont->ColPlane_fr)) {
         SUMA_LHv("UnM.ch: ColPlane_fr, ncall %d\n", ncall);
         XtUnmanageChild(SurfCont->ColPlane_fr);
      }
      if( notest ) {
         SUMA_LHv("UnM.ch: DsetMap_fr, ncall %d\n", ncall);
         XtUnmanageChild(SurfCont->DsetMap_fr); /* needed */
      }   
      /* if nothing else remains in the parent of ColPlane, 
         then unmanage its parent (rc_right) too. 
         *** Parent of that frame is now rc_left    May 25 04*/
      /* XtUnmanageChild(XtParent(SurfCont->ColPlane_fr)); May 25 04*/
      if (XtIsManaged(SurfCont->DsetMap_fr)) {
         SUMA_LHv("UnM.ch: DsetMap_fr, par(DsetMap_fr), ncall %d\n", ncall);
         XtUnmanageChild(SurfCont->DsetMap_fr);
         XtUnmanageChild(XtParent(SurfCont->DsetMap_fr));
      }
   } else {
      /* XtManageChild(XtParent(SurfCont->ColPlane_fr)); May 25 04*/
      if ((strcmp(curColPlane->cmapname, "explicit") != 0) &&
          !SUMA_is_Label_dset(curColPlane->dset_link, NULL)) { 
         SUMA_LHv("Not explicit, ncall = %d\n", ncall);
         /* not an RGB dset */
         if (!XtIsManaged(SurfCont->DsetMap_fr)) {
            SUMA_LHv("M.ch: par(DsetMap_fr) DsetMap_fr, ncall = %d\n", ncall);
            XtManageChild(XtParent(SurfCont->DsetMap_fr));
            XtManageChild((Widget)SurfCont->DsetMap_fr);
         }
      }

      SUMA_LHv("M.ch: ColPlane_fr, ncall = %d\n", ncall);
      XtManageChild((Widget)SurfCont->ColPlane_fr);
      
      SUMA_LHv("Map.R: ColPlane_fr, ncall = %d\n", ncall);
      XMapRaised (XtDisplay(SurfCont->ColPlane_fr), 
                  XtWindow(SurfCont->TLS));
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
int SUMA_PauseForUser(  Widget parent, char *question, SUMA_WINDOW_POSITION pos, 
                        XtAppContext *app, int withcancel, float timeout)
{
   static char FuncName[]={"SUMA_PauseForUser"};
   static Widget dialog; /* static to avoid multiple creation */
   Widget YesWid;
   int ii;
   XmString text, yes;
   struct  timeval  tt;
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
      SUMA_PAUSE_PROMPT_STDIN(question);
      SUMA_RETURN(SUMA_YES);
   }
   
   if (!dialog) {
     SUMA_LH("Creating Dialog");
     dialog = XmCreateQuestionDialog (parent, "dialog", NULL, 0);
     XtVaSetValues (dialog,
         XmNdialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
         NULL);
     /* Don't need help and cancel buttons*/
     XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
     if (!withcancel) {
      XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON)); 
     }else {
      XtAddCallback (dialog, XmNcancelCallback, SUMA_response, &answer); 
     }
     
     XtAddCallback (dialog, XmNokCallback, SUMA_response, &answer); 
    } else {
      SUMA_LH("Reusing Dialog (SLOW SLOW SLOW)");
    }
   
   SUMA_etime(&tt, 0);
   answer = SUMA_NO_ANSWER;
   text = XmStringCreateLocalized (question);
   yes = XmStringCreateLocalized ("OK");
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
   

   if (!app) app = &(SUMAg_CF->X->App);
   if (timeout < 0.0) { /* no timer */
      while ( (answer == SUMA_NO_ANSWER && XtIsManaged(dialog)) ) {
          XtAppProcessEvent (*app, XtIMAll);
      } 
   } else {
      while ( (answer == SUMA_NO_ANSWER && XtIsManaged(dialog)) ) {
         if (timeout < 0.0 || SUMA_etime(&tt,1) < timeout) {
            if (XtAppPending(*app)) { XtAppProcessEvent (*app, XtIMAll); }
         } else {
            XtVaGetValues(YesWid, XmNuserData, &answer, NULL);       
            break;
         }
      }
   }
   #if 1
      SUMA_LH("destroying dialog");
      
      XtDestroyWidget(dialog);   /* This won't get the widget off of the screen
                                    unless there is an XtAppMainLoop running.
                                    When that is not the case, you need to 
                                    trigger an event processing call, see 
                                    SUMA_prompt_user.c for an example */
                                    
      dialog = NULL;
   #else /* bad, takes for ever to come back up. 
            Same for repeated calls of ForceUser if created for the first 
            time from DriveSuma and not from the interface with, say 'shft+Esc' 
            See bit of illustration code in SUMA_Engine where PauseForUser 
            is called*/ 
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
      fprintf( SUMA_STDERR, 
               "Error %s: Could not connect to %s.\n", 
               FuncName, XDisplayName(NULL));
      SUMA_RETURNe;
   }
   if (glXQueryExtension(dpy, &errorBase, &eventBase) == False) {
      fprintf(SUMA_STDERR, 
              "Error %s: OpenGL not supported by X server.\n" ,FuncName);
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
      window = XCreateWindow( dpy, RootWindow(dpy, visualToTry->screen), 
                              0, 0, 100, 100, 0, visualToTry->depth, 
                              InputOutput, visualToTry->visual,
                              CWBorderPixel | CWColormap, &swa);
      SUMA_glXMakeCurrent(dpy, window, context, FuncName, "showall", 1);
      fprintf(SUMA_STDERR, "\n");
      fprintf(SUMA_STDERR, "OpenGL vendor string: %s\n", glGetString(GL_VENDOR));
      fprintf(SUMA_STDERR, "OpenGL renderer string: %s\n", 
                           glGetString(GL_RENDERER));
      fprintf(SUMA_STDERR, "OpenGL version string: %s\n", 
                           glGetString(GL_VERSION));
      if (glXIsDirect(dpy, context))
         fprintf(SUMA_STDERR, "direct rendering: supported\n");
   } else fprintf(SUMA_STDERR, "No GLX-capable visuals!\n");
   
   if (visualList) XFree(visualList);

   /* which visual will be chosen by SUMA ? 
      (based on Step 3 in SUMA_X_SurfaceViewer_Create) */
   /* TopLevel = XtAppInitialize(&App, "SUMA", NULL, 0, &cargc, vargv,
                              SUMA_get_fallbackResources(), NULL, 0); 
               Superseded by XtOpenApplication*/
   TopLevel = XtOpenApplication(&App, "SUMA", NULL, 0, &cargc, vargv,
                                 SUMA_get_fallbackResources(), 
                                 topLevelShellWidgetClass, NULL, 0); 
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
      fprintf(SUMA_STDERR, "0x%x %2d %s",(unsigned int)vi->visualid, 
                           vi->depth, SUMA_ClassOf(vi->class));
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
        SUMA_Format(auxBuffers, 2), SUMA_Format(depthSize, 2), 
        SUMA_Format(stencilSize, 2),
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


void SUMA_cb_CloseXformInterface(Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_CloseXformInterface"};
   SUMA_XFORM *xf=(SUMA_XFORM *)data;
   SUMA_Boolean Shaded = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!xf->gui->AppShell) SUMA_RETURNe;
   
   
   if (LocalHead) 
      fprintf (SUMA_STDERR,"%s: Withdrawing xf window...\n", FuncName);

   XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
      XtWindow(xf->gui->AppShell),
      XScreenNumberOfScreen(XtScreen(xf->gui->AppShell)));
   
   SUMA_RETURNe;
}

void SUMA_cb_helpXformInterface (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[] = {"SUMA_cb_helpXformInterface"};
   SUMA_XFORM *xf=(SUMA_XFORM *)data;
   SUMA_Boolean Shaded = NOPE;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   if (LocalHead) 
      fprintf (SUMA_STDERR,"%s: help for xf window...\n", FuncName);

   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_Help_Xform);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, (void *)xf,
                                          SES_Suma, NULL, NOPE,
                                          SEI_Tail, NULL))) {
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
   }
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }

   SUMA_RETURNe;
}

/*!
   \brief Sets the widgets in the DrawROI window based on the DrawnROI structure
*/
SUMA_Boolean SUMA_InitializeXformInterface (SUMA_XFORM *xf)
{
   static char FuncName[] = {"SUMA_InitializeXformInterface"};
   char sbuf[12*SUMA_MAX_LABEL_LENGTH+12];
   int ii=0;
   SUMA_DSET *in_dset=NULL;
   NI_element *dotopts=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) SUMA_DUMP_TRACE("who's calling?");
   
   if (!xf) {
      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: Initializing with NULL.\n", FuncName);
   }else {
      if (LocalHead) 
         fprintf (SUMA_STDERR, 
                  "%s: Initializing with %p.\n", FuncName, xf);
      
      /* generic stuff */
      XmToggleButtonSetState( xf->gui->Active_tb, xf->active, NOPE);
      
      /* particulars */
      if (!strcmp(xf->name, "Dot")) { 
         for (ii=0; ii<xf->N_parents; ++ii) {
            if (!SUMA_is_ID_4_DSET(xf->parents[ii], &in_dset)) {
               SUMA_S_Err("You've really done it this time!");
               SUMA_RETURN(NOPE);
            }         
            if (ii==0) snprintf (sbuf, sizeof(char)*3*SUMA_MAX_LABEL_LENGTH,
                                 "TS Parents:\n"
                                 "%s", SUMA_sdset_label(in_dset));
            else if (ii<10)      {
               strcat(sbuf,"\n");
               strcat(sbuf,SUMA_sdset_label(in_dset));
            }  else {
               SUMA_S_Err("Fatten sbuf");
            }
         }
         SUMA_SET_LABEL(xf->gui->ParentLabel_lb, sbuf);
         
         if (LocalHead) SUMA_ShowNel(xf->XformOpts);
         
         if ((dotopts = SUMA_FindNgrNamedElement(xf->XformOpts, "dotopts"))) {
            NI_GET_FLOAT(dotopts, "filter_below", (xf->gui->AF0->value));
            sprintf(sbuf, "%.3f", (xf->gui->AF0->value));
            SUMA_SET_TEXT_FIELD(xf->gui->AF0->textfield,sbuf); 
            NI_GET_FLOAT(dotopts, "filter_above", (xf->gui->AF1->value));
            sprintf(sbuf, "%.3f", xf->gui->AF1->value);
            SUMA_SET_TEXT_FIELD(xf->gui->AF1->textfield,sbuf); 
            NI_GET_INT(dotopts, "polort", (xf->gui->AF2->value));
            sprintf(sbuf, "%d", (int)(xf->gui->AF2->value));
            SUMA_SET_TEXT_FIELD(xf->gui->AF2->textfield,sbuf); 
         } else {
            SUMA_DUMP_TRACE("No dotopts");
            SUMA_S_Err("No dotopts!");
            SUMA_RETURN(NOPE);
         }
         if (xf->gui->ShowPreProc_tb) 
            XmToggleButtonSetState( xf->gui->ShowPreProc_tb, 
                                    xf->ShowPreProc, NOPE);
      } else {
         SUMA_S_Errv("Don't know how to initialize %s\n", xf->name);   
      }
   }
   SUMA_RETURN (YUP);
}

void SUMA_cb_XformActive_toggled(Widget w, XtPointer data, 
                                 XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_XformActive_toggled"};
   SUMA_XFORM *xf=(SUMA_XFORM*)data;
   
   SUMA_ENTRY;
   
   SUMA_SetXformActive(xf, !xf->active, 1);
   
   SUMA_RETURNe;
}

void SUMA_cb_XformShowPreProc_toggled( Widget w, XtPointer data, 
                                       XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_XformShowPreProc_toggled"};
   SUMA_XFORM *xf=(SUMA_XFORM*)data;
   
   SUMA_ENTRY;
   
   SUMA_SetXformShowPreProc(xf, !xf->ShowPreProc, 1);
   
   SUMA_RETURNe;
}

void SUMA_CreateXformXformInterface(SUMA_XFORM *xf, Widget parent_frame)
{
   static char FuncName[]={"SUMA_CreateXformXformInterface"};
   Widget rc, rcv;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   /* the vertical rcv */
   rcv = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, parent_frame,
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
   /* row column to hold active button and maybe more */
   rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
   xf->gui->Active_tb = XtVaCreateManagedWidget("Active", 
      xmToggleButtonWidgetClass, rc, NULL);
   XmToggleButtonSetState (xf->gui->Active_tb, xf->active, NOPE);
   XtAddCallback (xf->gui->Active_tb, 
                  XmNvalueChangedCallback, SUMA_cb_XformActive_toggled, 
                  (XtPointer)xf);
   SUMA_Register_Widget_Help(xf->gui->Active_tb , 1,
                             "Xform->Active",
                             "Activate/Suspend xform",
                             SUMA_ActivateXform_help ) ; 

   /* set the toggle button's select color */
   SUMA_SET_SELECT_COLOR(xf->gui->Active_tb);
   
   XtManageChild(rc);

   XtManageChild(rcv);
   SUMA_RETURNe;
}

void SUMA_CreateXformParentsInterface(SUMA_XFORM *xf, Widget parent_frame)
{
   static char FuncName[]={"SUMA_CreateXformParentsInterface"};
   Widget rc, rcv;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   /* the vertical rcv */
   rcv = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, parent_frame,
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
   
   /* row column to hold parent name and maybe more*/
   rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
   if ( !strcmp(xf->name,"Dot") ) { /* Dot xform */
      xf->gui->ParentLabel_lb = 
         XtVaCreateManagedWidget ("TS Parents:      N/A \n"
                                  "                 N/A \n", 
            xmLabelWidgetClass, rc,
            NULL);
      SUMA_Register_Widget_Help(xf->gui->ParentLabel_lb , 1,
                                "Dot->Xform->datasets->TS_Parents",
                                "Label of time series dsets transformed",
                                SUMA_DotXform_ParentLabel_help ) ;
   } else {
      SUMA_S_Errv("Don't know how to build xform parent interface for %s\n",
                  xf->name);
      SUMA_RETURNe;
   }
   XtManageChild(rc);
   
   if (!strcmp(xf->name,"Dot")) {
      /* Show intermediate dsets? */
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rcv,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      XtVaCreateManagedWidget ("Preprocessed Dsets:", 
               xmLabelWidgetClass, rc,
               NULL);
      
      /* a save button for preprocessed dsets */
      xf->gui->SavePreProc_pb = XtVaCreateWidget ("Save", 
                                           xmPushButtonWidgetClass, rc, 
                                           NULL);
      XtAddCallback (xf->gui->SavePreProc_pb, 
                     XmNactivateCallback, 
                     SUMA_cb_XformPreProc_Save, (XtPointer)xf);
      SUMA_Register_Widget_Help(xf->gui->SavePreProc_pb ,1, 
                                "Dot->Xform->datasets->Save",
                        "Save the preprocessed dsets to disk.",
                                SUMA_XformPreProc_Save_help ) ;
      XtManageChild (xf->gui->SavePreProc_pb);

      #if 0 /* complicated, see comment in SUMA_SetXformShowPreProc*/
      xf->gui->ShowPreProc_tb = XtVaCreateManagedWidget("Show Intermediate", 
         xmToggleButtonWidgetClass, rc, NULL);
      XmToggleButtonSetState (xf->gui->ShowPreProc_tb, xf->ShowPreProc, NOPE);
      XtAddCallback (xf->gui->ShowPreProc_tb, 
                     XmNvalueChangedCallback, SUMA_cb_XformShowPreProc_toggled, 
                     (XtPointer)xf);
      SUMA_Register_Widget_Help(xf->gui->ShowPreProc_tb , 1,
                                "Dot->Xform->UNUSED->Show_Intermediate",
                                "Make visible preprocessed dsets",
                                SUMA_ShowPreProcXform_help ) ;

      /* set the toggle button's select color */
      SUMA_SET_SELECT_COLOR(xf->gui->ShowPreProc_tb);
      #endif
      
      XtManageChild(rc);

   }else {
      SUMA_S_Errv("Don't know how to build xform parent interface for %s\n",
                  xf->name);
      SUMA_RETURNe;
   }
   XtManageChild(rc);
   
   
   
   XtManageChild(rcv);
   SUMA_RETURNe;
}


void SUMA_DotXform_NewBandPass(  SUMA_XFORM *xf,
                                 float lf, float uf, 
                                 int fromgui)
{
   static char FuncName[]={"SUMA_DotXform_NewBandPass"};
   NI_element *dotopts=NULL;
   char sbuf[256];
   int ii;
   float ouf=0.0, olf=0.0;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   if (lf > uf) {
      SUMA_S_Err("Bad range");
      SUMA_RETURNe;
   }
   
   if (!(dotopts = SUMA_FindNgrNamedElement(xf->XformOpts, "dotopts"))) {
      SUMA_S_Err("Failed to find dotopts");
      SUMA_RETURNe;
   }
   
   NI_GET_FLOAT(dotopts,"filter_below", olf);
   NI_GET_FLOAT(dotopts,"filter_above", ouf);
   
   if (olf == lf && ouf == uf) {
      SUMA_LH("Nothing to do");
      SUMA_RETURNe;
   } else {
      SUMA_LHv("%f %f\n%f %f\n",
                     olf, lf, ouf, uf);
   }
   
   NI_SET_FLOAT(dotopts,"filter_below", lf);
   NI_SET_FLOAT(dotopts,"filter_above", uf);

   if (!fromgui && xf->gui) {
      xf->gui->AF0->value = lf;
      sprintf(sbuf, "%.3f", lf);
      SUMA_SET_TEXT_FIELD(xf->gui->AF0->textfield,sbuf); 
      xf->gui->AF1->value = uf;
      sprintf(sbuf, "%.3f", uf);
      SUMA_SET_TEXT_FIELD(xf->gui->AF1->textfield,sbuf); 
   } 
   
   for (ii=0; ii<xf->N_parents; ++ii)
      SUMA_DotXform_SetPending (dotopts, 1, xf->parents[ii]);
   
     
   SUMA_RETURNe;
}

void SUMA_OpenXformOrtFile (char *filename, void *data)
{
   static char FuncName[]={"SUMA_OpenXformOrtFile"};
   SUMA_XFORM *xf=(SUMA_XFORM *)data;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!xf) {
      SUMA_S_Err("NULL input");
   }
   if (!strcmp(xf->name,"Dot")) {
      SUMA_DotXform_NewOrtName( xf,
                               filename, 
                               1 );
   } else {
      SUMA_S_Err("Dunno what to do");
   }
 
 
   SUMA_RETURNe;
}

void SUMA_DotXform_NewPolort(  SUMA_XFORM *xf,
                               int polort, 
                               int fromgui)
{
   static char FuncName[]={"SUMA_DotXform_NewPolort"};
   NI_element *dotopts=NULL;
   char sbuf[256];
   int ii=0, opolort=0;
   SUMA_DSET *in_dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   if (polort < -1) {
      SUMA_S_Err("Bad val");
      SUMA_RETURNe;
   }
   
   if (!(dotopts = SUMA_FindNgrNamedElement(xf->XformOpts, "dotopts"))) {
      SUMA_S_Err("Failed to find dotopts");
      SUMA_RETURNe;
   }
   
   NI_GET_INT(dotopts,"polort", opolort);
   if (polort == opolort) { /* nothing to do */
      SUMA_LH("Nothing to do");
      SUMA_RETURNe;
   } else {
      SUMA_LHv("%d %d\n", opolort, polort);
   }
   
   NI_SET_INT(dotopts,"polort", polort);

   if (!fromgui && xf->gui) {
      xf->gui->AF2->value = polort;
      sprintf(sbuf, "%d", polort);
      SUMA_SET_TEXT_FIELD(xf->gui->AF2->textfield,sbuf); 
   } 
   
   /* recalculate polorts */
   if (!(SUMA_is_ID_4_DSET(xf->parents[0], &in_dset))) {
      SUMA_S_Err("Could not find ts dset");
      SUMA_RETURNe;
   }
   if (!SUMA_DotXform_MakeOrts( dotopts, SDSET_VECNUM(in_dset), 
                                polort, NI_get_attribute(dotopts,"ortname"))){
      SUMA_S_Err("Failed to make orts");
      SUMA_RETURNe;
   }

   
   for (ii=0; ii<xf->N_parents; ++ii)
      SUMA_DotXform_SetPending (dotopts, 1, xf->parents[ii]);

     
   SUMA_RETURNe;
}

void SUMA_DotXform_NewOrtName(  SUMA_XFORM *xf,
                               char * ortname, 
                               int fromgui)
{
   static char FuncName[]={"SUMA_DotXform_NewOrtName"};
   NI_element *dotopts=NULL;
   char sbuf[256];
   int ii=0, polort=-3;
   SUMA_DSET *in_dset=NULL;
   SUMA_Boolean LocalHead=NOPE;
      
   SUMA_ENTRY;
      
   if (!(dotopts = SUMA_FindNgrNamedElement(xf->XformOpts, "dotopts"))) {
      SUMA_S_Err("Failed to find dotopts");
      SUMA_RETURNe;
   }
   
   SUMA_LHv("Setting ortname to %s\n", ortname);
   NI_set_attribute(dotopts,"ortname", ortname);

   if (xf->gui) {
      if (ortname) {
         SUMA_PARSED_NAME *pn = SUMA_ParseFname(ortname, SUMAg_CF->cwd);
         SUMA_SET_LABEL(xf->gui->OrtFileLabel_lb, pn->FileName);
         SUMA_Free_Parsed_Name(pn); pn = NULL;
      } else {
         SUMA_SET_LABEL(xf->gui->OrtFileLabel_lb, "--"); 
      }
   } 
   
   /* recalculate polorts */
   if (!(SUMA_is_ID_4_DSET(xf->parents[0], &in_dset))) {
      SUMA_S_Err("Could not find ts dset");
      SUMA_RETURNe;
   }
   NI_GET_INT(dotopts,"polort", polort);
   if (!SUMA_DotXform_MakeOrts( dotopts, SDSET_VECNUM(in_dset), 
                                polort, ortname)){
      SUMA_S_Err("Failed to make orts");
      SUMA_RETURNe;
   }

   
   for (ii=0; ii<xf->N_parents; ++ii)
      SUMA_DotXform_SetPending (dotopts, 1, xf->parents[ii]);

     
   SUMA_RETURNe;
}

void SUMA_Xform_NewAF0 (void *data) 
{
   static char FuncName[]={"SUMA_Xform_NewAF0"};
   SUMA_XFORM *xf=(SUMA_XFORM *)data;
   char sbuf[128];
   
   SUMA_ENTRY;
   
   if (!strcmp(xf->name,"Dot")) { 
      if (xf->gui->AF0->value > xf->gui->AF1->value) {/* bad range*/
         xf->gui->AF0->value = xf->gui->AF1->value;
         sprintf(sbuf, "%.3f", xf->gui->AF0->value);
         SUMA_SET_TEXT_FIELD(xf->gui->AF0->textfield,sbuf); 
         SUMA_RETURNe;
      }

      SUMA_DotXform_NewBandPass( xf, xf->gui->AF0->value, 
                                 xf->gui->AF1->value , 1);      
   } else {
      SUMA_S_Errv("Don't know how to process xform %s\n", xf->name);
   }
   
   SUMA_RETURNe;
}

void SUMA_Xform_NewAF1 (void *data) 
{
   static char FuncName[]={"SUMA_Xform_NewAF1"};
   SUMA_XFORM *xf=(SUMA_XFORM *)data;
   char sbuf[128];
   
   SUMA_ENTRY;
   
   if (!strcmp(xf->name,"Dot")) { 
      if (xf->gui->AF1->value < xf->gui->AF0->value) {/* bad range*/
         xf->gui->AF1->value = xf->gui->AF0->value;
         sprintf(sbuf, "%.3f", xf->gui->AF1->value);
         SUMA_SET_TEXT_FIELD(xf->gui->AF1->textfield,sbuf); 
         SUMA_RETURNe;
      }
      SUMA_DotXform_NewBandPass( xf, xf->gui->AF0->value, 
                                 xf->gui->AF1->value , 1);      
   } else {
      SUMA_S_Errv("Don't know how to process xform %s\n", xf->name);
   }
   
   SUMA_RETURNe;
}

void SUMA_Xform_NewAF2 (void *data) 
{
   static char FuncName[]={"SUMA_Xform_NewAF2"};
   SUMA_XFORM *xf=(SUMA_XFORM *)data;
   char sbuf[128];
   
   SUMA_ENTRY;
   
   if (!strcmp(xf->name,"Dot")) { 
      SUMA_DotXform_NewPolort( xf, xf->gui->AF2->value, 1);      
   } else {
      SUMA_S_Errv("Don't know how to process xform %s\n", xf->name);
   }
   
   SUMA_RETURNe;
}


void SUMA_cb_XformOpts_Apply (Widget w, XtPointer data, 
                             XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_XformOpts_Apply"};
   SUMA_XFORM *xf=(SUMA_XFORM*)data;
   NI_element *nelpars=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!strcmp(xf->name,"Dot")) {
      SUMA_CALLBACK *cb=NULL;
      DListElmt *el=NULL;
      DList *lst = SUMAg_CF->callbacks;
      if (!lst) SUMA_RETURNe;
      el = dlist_head(lst);
      while (el && !cb) {
         cb = (SUMA_CALLBACK *)el->data;
         if (!strcmp(cb->creator_xform, xf->idcode_str)) {
            SUMA_SetCallbackPending(cb, 1, SES_Suma);
            if (( nelpars=
                   SUMA_FindNgrNamedElement(
                     cb->FunctionInput, "event_parameters"))) {
               /* put the last events back in */
               SUMA_XFORM_RETRIEVE_LAST_EVENT(nelpars);
               if (!SUMA_ExecuteCallback(cb, 1, NULL, 1)) {
                  SUMA_S_Err("Failed executing callback");
               }
            }
         }
         el = dlist_next(el);
      }  
   } else {
      SUMA_S_Errv("Don't know what to do for this %s xform", xf->name);
      SUMA_RETURNe;
   }
   
   SUMA_RETURNe;
}

void SUMA_cb_XformOpts_Save (Widget w, XtPointer data, 
                             XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_XformOpts_Save"};
   SUMA_XFORM *xf=(SUMA_XFORM*)data;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_SaveXformOptsFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, (void *)xf,
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

SUMA_Boolean SUMA_SaveXformPreProcDsets (SUMA_XFORM *xf, char *prefix)
{
   static char FuncName[]={"SUMA_SaveXformPreProcDsets"};
   char *fn=NULL, *fno=NULL, *oid=NULL, *ofn=NULL;
   int ii;
   NI_element *dotopt=NULL;
   SUMA_DSET *in_dset=NULL, *pp_dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!strcmp(xf->name,"Dot")) {   
      if (!(dotopt = SUMA_FindNgrNamedElement(xf->XformOpts, "dotopts"))) {
         SUMA_S_Err("dotopt not found");
         SUMA_RETURN(NOPE);
      }
      for (ii=0; ii<xf->N_parents; ++ii) {
         if (!SUMA_is_ID_4_DSET(xf->parents[ii], &in_dset)) {
            SUMA_S_Err("No parent");
            SUMA_RETURN(NOPE);
         }
         if (!(pp_dset = SUMA_GetDotPreprocessedDset(in_dset, dotopt))){
            SUMA_S_Err("PreProcParent not found");
            SUMA_RETURN(NOPE);
         }
         fn = SUMA_append_replace_string(prefix,SDSET_LABEL(in_dset),
                                         "", 0);       
         fno = SUMA_WriteDset_PreserveID(fn, pp_dset, SUMA_BINARY_NIML,1,1);   
         if (fno) fprintf(stderr,"Saved %s\n", fno);
         else fprintf(stderr,"Failed to save\n");
         
         SUMA_free(fn); fn=NULL;
         SUMA_free(fno); fno=NULL; 
      }   
   } else {
      SUMA_S_Errv("Can't do %s\n", xf->name);
      SUMA_RETURN(NOPE);
   }
   SUMA_RETURN(YUP);
}

void SUMA_cb_XformPreProc_Save (Widget w, XtPointer data, 
                             XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_XformPreProc_Save"};
   SUMA_XFORM *xf=(SUMA_XFORM*)data;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_SaveXformPreProcDsets(xf, "pp");
      
   SUMA_RETURNe;
}

void SUMA_cb_XformOrtFile_Load(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_XformOrtFile_Load"};
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_XFORM *xf=(SUMA_XFORM*)data;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!xf) { SUMA_S_Err("what gives?"); SUMA_RETURNe;}
   SUMA_LHv("Loading ort for %s\n", xf->name);
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_OpenXformOrtFileFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, (XtPointer)xf,
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

void SUMA_CreateXformOptionsInterface(SUMA_XFORM *xf, Widget parent_frame)
{
   static char FuncName[]={"SUMA_CreateXformOptionsInterface"};
   Widget rc, rcv;
   float fs, fmax, fstep;
   double TR;    
   int N_TS;
   XmString xmstmp; 
   SUMA_DSET *in_dset=NULL;
     
   SUMA_ENTRY;
   
   /* the vertical rcv */
   rcv = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, parent_frame,
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
         
   if ( !strcmp(xf->name,"Dot") ) { /* Dot xform */
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rcv,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      /* put a label in this rc*/ 
      XtVaCreateManagedWidget ("Band Pass Range (Hz.)", 
               xmLabelWidgetClass, rc,
               NULL);
      XtManageChild(rc);
      
      /* row column to hold bandpass options  and maybe more */
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rcv,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      
               
      /* find some params from parent dsets */
      if (!(SUMA_is_ID_4_DSET(xf->parents[0], 
                              &in_dset))) {
         SUMA_S_Err("Could not find parent dset");
         SUMA_RETURNe;
      }
      if (!(SUMA_is_TimeSeries_dset(in_dset, &TR))) {
         TR = 0.0;
      } 

      SUMA_SPECT_AXIS(TR,SDSET_VECNUM(in_dset),  fs, fmax,fstep);
      
      SUMA_CreateArrowField ( rc, "LF",
                        0.01, 0.0, fmax, fstep, 
                        6, SUMA_float,
                        NOPE,
                        SUMA_Xform_NewAF0, (void *)xf, 
                        "Dot->options->LF",
                        SUMA_DotXform_AF0_hint, 
                        SUMA_DotXform_AF0_help,
                        xf->gui->AF0);
      SUMA_CreateArrowField ( rc, "HF",
                        fmax > 0.1 ? 0.1:fmax, 0.0, 1.0, fstep,  
                        6, SUMA_float,
                        NOPE,
                        SUMA_Xform_NewAF1, (void *)xf, 
                        "Dot->options->HF",
                        SUMA_DotXform_AF1_hint, 
                        SUMA_DotXform_AF1_help,
                        xf->gui->AF1);

      XtManageChild(rc);

      /* row column to hold polort options and maybe more*/
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rcv,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);

      SUMA_CreateArrowField ( rc, "polort",
                        /* 0, -1, SDSET_VECNUM(in_dset)/2,  */
                        2,2,2, /* for now keep it frozen, till you figure out 
                                  how to turn off bandpassing when polort is   
                                  used.  */
                        1, 
                        3, SUMA_int,
                        NOPE,
                        SUMA_Xform_NewAF2, (void *)xf, 
                        "Dot->options->polort",
                        SUMA_DotXform_AF2_hint, 
                        SUMA_DotXform_AF2_help,
                        xf->gui->AF2);
      XtManageChild(rc);
      XtSetSensitive(rc, 0);
      
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rcv,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      
      xf->gui->LoadOrtFile_pb = XtVaCreateWidget ("OrtFile", 
                                    xmPushButtonWidgetClass, rc, 
                                    NULL);
      XtAddCallback (xf->gui->LoadOrtFile_pb, 
                     XmNactivateCallback, SUMA_cb_XformOrtFile_Load, 
                     (XtPointer)xf);
      SUMA_Register_Widget_Help(xf->gui->LoadOrtFile_pb , 1,
                                "Dot->options->OrtFile",
                                "Load an ort file",
                                SUMA_XformOrtFile_Load_help ) ;
      XtManageChild (xf->gui->LoadOrtFile_pb);

      xmstmp = XmStringCreateLtoR ("no ort file loaded", 
                                    XmSTRING_DEFAULT_CHARSET);
      xf->gui->OrtFileLabel_lb = XtVaCreateManagedWidget (
               "orti", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
      XmStringFree (xmstmp); xmstmp=NULL;
      
      XtManageChild(rc);
      
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rcv,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      
      XtVaCreateManagedWidget ("Options:", 
               xmLabelWidgetClass, rc,
               NULL);
               
      xf->gui->SaveOpts_pb = XtVaCreateWidget ("Save", 
                                           xmPushButtonWidgetClass, rc, 
                                           NULL);
      XtAddCallback (xf->gui->SaveOpts_pb, 
                     XmNactivateCallback, 
                     SUMA_cb_XformOpts_Save, (XtPointer)xf);
      SUMA_Register_Widget_Help(xf->gui->SaveOpts_pb , 1,
                                "Dot->options->Options->Save",
                                "Save the options to disk.",
                                SUMA_XformOpts_Save_help ) ;
      XtManageChild (xf->gui->SaveOpts_pb);
      
      xf->gui->ApplyOpts_pb = XtVaCreateWidget ("Apply", 
                                       xmPushButtonWidgetClass, rc, 
                                       NULL);
      XtAddCallback (xf->gui->ApplyOpts_pb, 
                     XmNactivateCallback, 
                     SUMA_cb_XformOpts_Apply, (XtPointer)xf);
      SUMA_Register_Widget_Help(xf->gui->ApplyOpts_pb, 1,
                                "Dot->options->Options->Apply",
                                "Apply the options immediately",
                                SUMA_XformOpts_Apply_help ) ;
      XtManageChild (xf->gui->ApplyOpts_pb);

      XtManageChild(rc);
      
      
   } else {
      SUMA_S_Errv("Don't know how to build xform parent interface for %s\n",
                  xf->name);
      SUMA_RETURNe;
   }
   
   XtManageChild(rcv);
   
   SUMA_RETURNe;
}

void SUMA_CreateXformInterface(SUMA_XFORM *xf)
{
   static char FuncName[]={"SUMA_CreateXformInterface"};
   Widget cb_frame, opts_frame, frame_rcv, form, parent_frame, xform_frame;
   int i;
   char *sss;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (xf->gui) SUMA_RETURNe;
   
   xf->gui = SUMA_NewXformInterface(NULL);

   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }

   /* generic parts */
   xf->gui->AppShell = XtVaAppCreateShell(sss , "Suma" ,
                                          topLevelShellWidgetClass , 
                                          SUMAg_CF->X->DPY_controller1 , 
                                          XmNtitle, xf->name,
                                          NULL ) ;
   
   /* turn off default delete response. 
      If you do not do that, you will suffer.*/
   XtVaSetValues( xf->gui->AppShell,
           XmNdeleteResponse, XmDO_NOTHING,
           NULL);  
             
   /* handle the close button from window manager */
   XmAddWMProtocolCallback(/* make "Close" window menu work */
      xf->gui->AppShell,
      XmInternAtom( SUMAg_CF->X->DPY_controller1, "WM_DELETE_WINDOW" , False ) ,
      SUMA_cb_CloseXformInterface, (XtPointer)xf) ;
   
   /* create a form widget, manage it at the end ...*/
   form = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, xf->gui->AppShell,
      XmNborderWidth , 2 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   /* a vertical rc to put multiple frames in */
   frame_rcv = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, form,
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
   
   
   /* a frame for xform general buttons */
   xform_frame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, frame_rcv,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_FORM ,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL);      
   XtVaCreateManagedWidget ("xform",
      xmLabelWidgetClass, xform_frame, 
      XmNchildType, XmFRAME_TITLE_CHILD,
      XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
      NULL);
    
   /* populate the xform frame based on the type of xform */
   SUMA_CreateXformXformInterface(xf,xform_frame);
  
   XtManageChild (xform_frame);
   
   /* a frame to put parent datasets stuff in */
   parent_frame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, frame_rcv,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_FORM ,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL);
   
   XtVaCreateManagedWidget ("datasets",
      xmLabelWidgetClass, parent_frame, 
      XmNchildType, XmFRAME_TITLE_CHILD,
      XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
      NULL);
   
   /* populate the datasets frame based on the type of xform */
   SUMA_CreateXformParentsInterface(xf,parent_frame);
   
   XtManageChild (parent_frame);
      
   /* a frame to put options in */
   opts_frame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, frame_rcv,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_FORM ,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL);
   XtVaCreateManagedWidget ("options",
      xmLabelWidgetClass, opts_frame, 
      XmNchildType, XmFRAME_TITLE_CHILD,
      XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
      NULL);
   
   /* populate the opts frame based on the type of xform */
   
   SUMA_CreateXformOptionsInterface(xf,opts_frame);
      
   XtManageChild (opts_frame);
   
   /* the close, bhelp deal */
   cb_frame = SUMA_CloseBhelp_Frame( frame_rcv,
                              SUMA_cb_CloseXformInterface, 
                              (XtPointer) xf,
                              "Dot",
                              "Close Xform controller",
                              SUMA_closeXformCont_help,
                              SUMA_cb_helpXformInterface, 
                              (XtPointer) xf,
                              "Help on using this transform's interface",
                              SUMA_helpXformCont_help);
   
   /* manage the frame rcv */
   XtManageChild (frame_rcv);
   
   /* manage the form */
   XtManageChild (form);
   
   /* position the widget relative to the first open viewer */
   i=0;
   while (  i < SUMAg_N_SVv && 
            !SUMAg_SVv[i].X->ViewCont->TopLevelShell && 
            SUMAg_SVv[i].isShaded) ++i; 

   if (i < SUMAg_N_SVv) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: i = %d\n", FuncName, i);
      SUMA_PositionWindowRelative ( xf->gui->AppShell, 
                                    SUMAg_SVv[i].X->TOPLEVEL, SWP_TOP_RIGHT);
   }

   /* realize the widget */
   XtRealizeWidget (xf->gui->AppShell);
   
   
   SUMA_RETURNe;

}
   
/*!
   \brief Supposed to suggest a good wildcard string
   \param filetype: Not used now. It should be used to 
                    tell whether the wildcard is for a dataset
                    or a surface, etc.
   \param SO
   \param wild[]: a char string to contain the wildcard
   Function needs lots of work
*/
SUMA_Boolean SUMA_WildcardChoice(int filetype, 
                  SUMA_SurfaceObject *SO, char wild[]) 
{
   static char FuncName[]={"SUMA_WildcardChoice"};
   char *eeel = NULL;
   char *eeer = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) SUMA_RETURN(NOPE);
   
   /* default */
   switch (filetype) {
      case 1: /* dsets */
         eeel = getenv("SUMA_LEFT_FILE_DSET_IDENTIFIER");
         eeer = getenv("SUMA_RIGHT_FILE_DSET_IDENTIFIER");
         snprintf(wild, sizeof(char)*64, "*.dset");
         if (SO->Side == SUMA_LEFT ) {
            if (eeel) snprintf(wild, sizeof(char)*64,"%s", eeel);
         } else if (SO->Side == SUMA_RIGHT) {
            if (eeer) snprintf(wild, sizeof(char)*64,"%s", eeer);
         }
         break;
      case 2: /* rois */
         eeel = getenv("SUMA_LEFT_FILE_ROI_IDENTIFIER");
         eeer = getenv("SUMA_RIGHT_FILE_ROI_IDENTIFIER");
         snprintf(wild, sizeof(char)*64,"*.roi");
         if (SO->Side == SUMA_LEFT ) {
            if (eeel) snprintf(wild, sizeof(char)*64,"%s", eeel);
         } else if (SO->Side == SUMA_RIGHT) {
            if (eeer) snprintf(wild, sizeof(char)*64,"%s", eeer);
         }
         break;
      default: /* anything bloke */
         eeel = getenv("SUMA_LEFT_FILE_OTHER_IDENTIFIER");
         eeer = getenv("SUMA_RIGHT_FILE_OTHER_IDENTIFIER");
         snprintf(wild, sizeof(char)*64,"*");
         if (SO->Side == SUMA_LEFT ) {
            if (eeel) snprintf(wild, sizeof(char)*64,"%s", eeel);
         } else if (SO->Side == SUMA_RIGHT) {
            if (eeer) snprintf(wild, sizeof(char)*64,"%s", eeer);
         }
         break;
   }
   
   SUMA_RETURN(YUP); 
}


/*
void  (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={""};
   
   SUMA_ENTRY;
   
   SUMA_RETURNe;
}

*/
