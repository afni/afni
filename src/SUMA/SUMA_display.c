#include "SUMA_suma.h"

/* the method for hiding a surface viewer (and other controllers), used to have three options prior to Fri Jan  3 10:21:52 EST 2003
Now only SUMA_USE_WITHDRAW and NOT SUMA_USE_DESTROY should be used*/
#define SUMA_USE_WITHDRAW


extern SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
extern int SUMAg_N_SVv; /*!< Number of SVs stored in SVv */
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
static int snglBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 12,
  GLX_RED_SIZE, 1, None};
static int dblBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 12,
  GLX_DOUBLEBUFFER, GLX_RED_SIZE, 1, None};
static String fallbackResources[] = {
  "*glxarea*width: 300", "*glxarea*height: 300",
  "*frame*x: 20", "*frame*y: 20",
  "*frame*topOffset: 20", "*frame*bottomOffset: 20",
  "*frame*rightOffset: 20", "*frame*leftOffset: 20",
  "*frame*shadowType: SHADOW_IN", NULL
}; /* if you change default width and height, make sure you change SV->X->WIDTH & SV->X->HEIGHT in SUMA_SVmanip */

Boolean
SUMA_handleRedisplay(XtPointer closure)
{
   static char FuncName[]={"SUMA_handleRedisplay"};
   static int Last_isv = -1;
   int isv;
   SUMA_SurfaceViewer *sv;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
            /* An OpenGL rendering context is a port through which all OpenGL commands pass. */
            /* Before rendering, a rendering context must be bound to the desired drawable using glXMakeCurrent. OpenGL rendering commands implicitly use the current bound rendering context and one drawable. Just as a
               program can create multiple windows, a program can create multiple OpenGL rendering contexts. But a thread can only be bound to one rendering context and drawable at a time. Once bound, OpenGL rendering can begin.
               glXMakeCurrent can be called again to bind to a different window and/or rendering context. */
            if (!glXMakeCurrent (sv->X->DPY, XtWindow((Widget)closure), sv->X->GLXCONTEXT)) {
                     fprintf (SUMA_STDERR, "Error %s: Failed in glXMakeCurrent.\n \tContinuing ...\n", FuncName);
            }
         }
      }
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   } else {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Redisplay Pending registered for viewer %d.\n", FuncName, isv);
   }

   if(!sv->X->REDISPLAYPENDING) {
    /*sv->X->REDISPLAYID = XtAppAddWorkProc(sv->X->APP, handleRedisplay, 0);*/
    SUMA_register_workproc( SUMA_handleRedisplay , (XtPointer)sv->X->GLXAREA );
    sv->X->REDISPLAYPENDING = 1;
   }
   
   SUMA_RETURNe;
}


void SUMA_display(SUMA_SurfaceViewer *csv, SUMA_DO *dov)
{   
   int i;
   GLfloat rotationMatrix[4][4];
   static char FuncName[]={"SUMA_display"};
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
    
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);


   /* now you need to set the clear_color since it can be changed per viewer Thu Dec 12 2002 */
   glClearColor (csv->clear_color[0], csv->clear_color[1], csv->clear_color[2], csv->clear_color[3]);
   
   /* You cannot just rely on csv->ResetGLStateVariables because it is hard to set 
   for all conditions. For example, if you have multiple viewers open and you have surfaces 
   moving on momentum in all viewers, then you will have to call SUMA_OpenGLStateReset before
   each display otherwise the openGL settings for one of them will affect the others.
   At any rate, that function is not costly to run so there's no harm in running it anytime
   you have a display call and more than one viewer open */ 
   
   if (SUMAg_N_SVv > 1 || csv->ResetGLStateVariables) {
      if (LocalHead) fprintf(SUMA_STDERR, "%s: Calling SUMA_OpenGLStateReset.\n", FuncName);
      SUMA_OpenGLStateReset (SUMAg_DOv, SUMAg_N_DOv, csv);
      csv->ResetGLStateVariables = NOPE;
   }
   
   /* decide on color mixing needs */
   if (!SUMA_MixColors (csv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_MixColors. Aborting.\n", FuncName);
      exit(1);
   }
   
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Building Rotation matrix ...\n", FuncName);
   SUMA_build_rotmatrix(rotationMatrix, csv->GVS[csv->StdView].currentQuat);
    
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: performing glClear ...\n", FuncName);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); /* clear the Color Buffer and the depth buffer */
   
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Setting up matrix mode and perspective ...\n", FuncName);
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluPerspective((GLdouble)csv->FOV[csv->iState], csv->Aspect, SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); /*lower angle is larger zoom,*/

   /* cycle through csv->ShowDO and display those things that have a fixed CoordType*/
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Creating objects with fixed coordinates ...\n", FuncName);
   i = 0;
   while (i < csv->N_DO) {
      if (dov[csv->ShowDO[i]].CoordType == SUMA_SCREEN) {
         switch (dov[csv->ShowDO[i]].ObjectType) {
            case SO_type:
               break;
            case AO_type:
               if (csv->ShowEyeAxis){
                  if (!SUMA_CreateAxis ((SUMA_Axis*)dov[csv->ShowDO[i]].OP)) {
                     fprintf(SUMA_STDERR,"Error %s: Could not display EYE AXIS\n", FuncName);
                  }
               }
               break;
            case GO_type:
               break;
            case ROIdO_type:
               /* those are drawn by SUMA_CreateMesh */
               break;
            case ROIO_type:
               /* those are drawn by SUMA_CreateMesh */
               break;
            case LS_type:
               if (!SUMA_CreateSegmentDO ((SUMA_SegmentDO *)dov[csv->ShowDO[i]].OP)) {
                  fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_CreateSegmentDO.\n", FuncName);
               }
               break;
         }
      }
      ++i;
   }
   
   
   /*
    fprintf(stdout,"Translation Vector: %f %f\n", csv->GVS[csv->StdView].translateVec[0], csv->GVS[csv->StdView].translateVec[1]);
   fprintf(stdout,"Rotation Matrix:\n");
   for (i=0; i<4; ++i){ fprintf(stdout, "%f\t%f\t%f\t%f\n",\
    rotationMatrix[i][0], rotationMatrix[i][1], rotationMatrix[i][2], rotationMatrix[i][3]); }
    
    */
   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glTranslatef (csv->GVS[csv->StdView].translateVec[0], csv->GVS[csv->StdView].translateVec[1], 0.0);
   glTranslatef (csv->GVS[csv->StdView].RotaCenter[0], csv->GVS[csv->StdView].RotaCenter[1], csv->GVS[csv->StdView].RotaCenter[2]);
   glMultMatrixf(&rotationMatrix[0][0]);
   glTranslatef (-csv->GVS[csv->StdView].RotaCenter[0], -csv->GVS[csv->StdView].RotaCenter[1], -csv->GVS[csv->StdView].RotaCenter[2]);

   /* cycle through csv->ShowDO and display those things that have a Local CoordType*/
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Creating objects with local coordinates ...\n", FuncName);
   i = 0;
   while (i < csv->N_DO) {
      if (dov[csv->ShowDO[i]].CoordType == SUMA_LOCAL) {
         switch (dov[csv->ShowDO[i]].ObjectType) {
            case SO_type:
               SUMA_CreateMesh((SUMA_SurfaceObject *)dov[csv->ShowDO[i]].OP, csv); /* create the surface */
               break;
            case AO_type:
               if (csv->ShowMeshAxis) {
                  if (!SUMA_CreateAxis ((SUMA_Axis*)dov[csv->ShowDO[i]].OP)) {
                     fprintf(stderr,"display error: Could not display Mesh AXIS\n");
                  }
               }
               break;
            case GO_type:
               break;
            case ROIdO_type:
               /* those are drawn by SUMA_CreateMesh */
               break;
            case ROIO_type:
               /* those are drawn by SUMA_CreateMesh */
               break;
            case LS_type:
               if (!SUMA_CreateSegmentDO ((SUMA_SegmentDO *)dov[csv->ShowDO[i]].OP)) {
                  fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_CreateSegmentDO.\n", FuncName);
               }
               break;
         }
      }
      ++i;
   }
   
   /* Show the Cross Hair, if required */
   if (csv->ShowCrossHair) {
      /*fprintf(SUMA_STDOUT,"Showing Cross Hair \n");*/
      if (!SUMA_CreateCrossHair (csv->Ch)) {
         fprintf(stderr,"display error: Failed to Create Cross Hair\n");
      }
   }
   
   #if 0
   /* Show the pick line, you may want place this as a DO later on */
    {
      static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
      static GLfloat LineColor[] = {1.0, 0.0, 1.0, 0.0};
      glLineWidth(1.0);
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
      
   glPopMatrix();   

   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Flushing or swapping ...\n", FuncName);
   if (csv->X->DOUBLEBUFFER)
    glXSwapBuffers(csv->X->DPY, XtWindow(csv->X->GLXAREA));
   else
    glFlush();

  /* Avoid indirect rendering latency from queuing. */
  if (!glXIsDirect(csv->X->DPY, csv->X->GLXCONTEXT))
    glFinish();
   
   SUMA_RETURNe;
}

void
SUMA_graphicsInit(Widget w, XtPointer clientData, XtPointer call)
{
   
   XVisualInfo *SUMAg_cVISINFO;
   static char FuncName[]={"SUMA_graphicsInit"};
   int isv;
   SUMA_SurfaceViewer *sv;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   GLfloat light0_color[] = { SUMA_LIGHT0_COLOR_INIT};
   /*GLfloat green_light[] = { 0.0, 1.0, 0.0, 1.0};*/
   
   GLfloat lmodel_ambient[] = {SUMA_LMODEL_AMBIENT};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   glClearColor (sv->clear_color[0], sv->clear_color[1],sv->clear_color[2],sv->clear_color[3]);
   glShadeModel (GL_SMOOTH);

   switch (sv->PolyMode) {
      case 0:
         glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
         break;
      case 1:
         glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
         break;
      case 2:
         glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
         break;
   }
   
      
   /* Set the material properties*/
   glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
   glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
   glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
   glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);
   glMaterialfv(GL_FRONT, GL_EMISSION, mat_emission);
   
   /* set the directional light properties */
   glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);
   glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_color);
   glLightfv(GL_LIGHT0, GL_SPECULAR, light0_color);

   /*glLightfv(GL_LIGHT1, GL_POSITION, sv->light1_position);
   glLightfv(GL_LIGHT1, GL_DIFFUSE, green_light);
   glLightfv(GL_LIGHT1, GL_SPECULAR, green_light);*/
   
   /* set the ambient light */
   glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);
 
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
  
  /*glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);*/ /* No need for that, done in display */
  
  
   /* get the viewer just entered. */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }
   
   /* When using multiple viewers, you must reset the OpenGL state variables or risk having abrupt changes with the first click */
   sv->ResetGLStateVariables = YUP;
   SUMA_postRedisplay(w, NULL, NULL);

}

void
SUMA_mapStateChanged(Widget w, XtPointer clientData,
  XEvent * event, Boolean * cont)
{

   static char FuncName[]={"SUMA_mapStateChanged"};
   int isv;
   SUMA_SurfaceViewer *sv;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* determine the surface viewer that the widget belongs to */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }

   /* When using multiple viewers, you must reset the OpenGL state variables or risk having abrupt changes with the first click */
   sv->ResetGLStateVariables = YUP;

  /*fprintf(stdout, "widget window being mapped/unmapped\n");*/
  switch (event->type) {
  case MapNotify:
   sv->isShaded = NOPE;
    if (sv->X->MOMENTUMID)
      sv->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_CF->X->App, 1, SUMA_momentum, (XtPointer)w);
    break;
  case UnmapNotify:
    sv->isShaded = YUP;
    if (sv->X->MOMENTUMID)
      XtRemoveTimeOut(sv->X->MOMENTUMID);
    break;
  }
  
  SUMA_postRedisplay(w, clientData, NULL);
  
  SUMA_RETURNe;
}

Widget mainw, menubar, menupane, btn, sep, cascade, frame;
Arg menuPaneArgs[1], args[1];

SUMA_Boolean SUMA_X_SurfaceViewer_Create (void)
{
   static char FuncName[]={"SUMA_X_SurfaceViewer_Create"};
   static int CallNum = 0;
   int ic = 0;
   char *vargv[1]={ "[A] SUMA" };
   int cargc = 1;
   SUMA_Boolean NewCreation = NOPE, Found;
   char slabel[20]; 
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* Step 1. */
   if (CallNum == 0) { /* first call, initialize App */
      SUMAg_CF->N_OpenSV = 0;
      SUMAg_SVv[ic].X->TOPLEVEL = XtAppInitialize(&SUMAg_CF->X->App, "Suma", NULL, 0, &cargc, vargv,
       fallbackResources, NULL, 0);
      SUMAg_SVv[ic].X->DPY = XtDisplay(SUMAg_SVv[ic].X->TOPLEVEL);
      /* save DPY for first controller opened */
      SUMAg_CF->X->DPY_controller1 = SUMAg_SVv[ic].X->DPY;
      NewCreation = YUP;
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
         fprintf (SUMA_STDERR,"Error %s: Cannot open more than %d viewers.\n", FuncName, SUMA_MAX_SURF_VIEWERS);
         SUMA_RETURN (NOPE);
      }
      
      /* an unopen window was found, check its top level widget */
      if (SUMAg_SVv[ic].X->TOPLEVEL == NULL) {
         /* Unopen window found, needs a shell */
         sprintf(slabel,"[%c] SUMA", 65+ic);
         SUMAg_SVv[ic].X->DPY = SUMAg_CF->X->DPY_controller1;
         SUMAg_SVv[ic].X->TOPLEVEL = XtVaAppCreateShell(slabel , "Suma" ,
                   topLevelShellWidgetClass , SUMAg_SVv[ic].X->DPY ,
                   XmNinitialResourcesPersistent , False ,
                   NULL ) ;
         NewCreation = YUP;
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

      /* Step 3. */
      /*fprintf(stdout, "trying for cool double buffer visual\n");*/
      SUMAg_SVv[ic].X->VISINFO = glXChooseVisual(SUMAg_SVv[ic].X->DPY, DefaultScreen(SUMAg_SVv[ic].X->DPY), dblBuf);
      if (SUMAg_SVv[ic].X->VISINFO == NULL) {
      fprintf(stdout, "trying lame single buffer visual\n");
       XtAppWarning(SUMAg_CF->X->App, "trying lame single buffer visual");
       SUMAg_SVv[ic].X->VISINFO = glXChooseVisual(SUMAg_SVv[ic].X->DPY, DefaultScreen(SUMAg_SVv[ic].X->DPY), snglBuf);
       if (SUMAg_SVv[ic].X->VISINFO == NULL) {
         XtAppError(SUMAg_CF->X->App, "no good visual");
         SUMA_RETURN (NOPE);
         }
       SUMAg_SVv[ic].X->DOUBLEBUFFER = False;
      }

      /* Step 3.5 Wed Dec 18 14:49:25 EST 2002 - The GUI*/
         /* see Kilgard's OpenGL Programming for the X window system */
         /* create main window */
         mainw = XmCreateMainWindow (SUMAg_SVv[ic].X->TOPLEVEL, "mainw", NULL, 0);
         XtManageChild (mainw);      
         /* create menu bar */
         menubar = XmCreateMenuBar (mainw, "menubar", NULL, 0);
         XtManageChild (menubar);
         /* create menu pane */
         menupane = XmCreatePulldownMenu (menubar, "menupane", NULL, 0);
         
         /* build File Cascade button */
         btn = XmCreatePushButton (menupane, "Close", NULL, 0);
         XtAddCallback (btn, XmNactivateCallback, SUMA_cb_close, (XtPointer)&(SUMAg_SVv[ic]));
         XtManageChild (btn);
         XtSetArg(args[0], XmNsubMenuId, menupane);
         cascade = XmCreateCascadeButton(menubar, "File", args, 1);
         XtManageChild(cascade);

         /* build View Cascade button */
        /* create menu pane */
         menupane = XmCreatePulldownMenu (menubar, "menupane", NULL, 0);
         XtVaSetValues(menupane,
            XmNtearOffModel, XmTEAR_OFF_ENABLED);
         
         btn = XmCreatePushButton (menupane, "SUMA Controller", NULL, 0);
         XtAddCallback (btn, XmNactivateCallback, SUMA_cb_view_suma_controller, NULL);
         XtManageChild (btn);
         
         btn = XmCreatePushButton (menupane, "Surface Controller", NULL, 0);
         XtAddCallback (btn, XmNactivateCallback, SUMA_cb_view_surface_controller, (XtPointer)&(SUMAg_SVv[ic]));
         XtManageChild (btn);
         
         btn = XmCreatePushButton (menupane, "Viewer Controller", NULL, 0);
         XtAddCallback (btn, XmNactivateCallback, SUMA_cb_view_viewer_controller, (XtPointer)&(SUMAg_SVv[ic]));
         XtManageChild (btn);
         
         sep = XmCreateSeparator (menupane, "", NULL, 0);
         XtManageChild (sep);
         
         SUMAg_SVv[ic].X->ToggleCrossHair_View_tglbtn = XmCreateToggleButton(menupane, "Cross Hair", NULL, 0);
         XtAddCallback(SUMAg_SVv[ic].X->ToggleCrossHair_View_tglbtn, XmNvalueChangedCallback,
          (XtCallbackProc) SUMA_cb_toggle_crosshair, (XtPointer)&(SUMAg_SVv[ic]));
         XtManageChild(SUMAg_SVv[ic].X->ToggleCrossHair_View_tglbtn);
         XmToggleButtonSetState (SUMAg_SVv[ic].X->ToggleCrossHair_View_tglbtn, 
            SUMAg_SVv[ic].ShowCrossHair, NOPE);
            
         btn = XmCreateToggleButton(menupane, "Node in Focus", NULL, 0);
         XtAddCallback(btn, XmNvalueChangedCallback,
          (XtCallbackProc) SUMA_cb_toggle_node_in_focus, NULL);
         XtManageChild(btn);
         
         btn = XmCreateToggleButton(menupane, "Selected Faceset", NULL, 0);
         XtAddCallback(btn, XmNvalueChangedCallback,
          (XtCallbackProc) SUMA_cb_toggle_selected_faceset, NULL);
         XtManageChild(btn);
         
         XtSetArg(args[0], XmNsubMenuId, menupane);
         cascade = XmCreateCascadeButton(menubar, "View", args, 1);
         XtManageChild(cascade);

         /* build Edit Cascade button */
         
      #ifdef SUMA_MOTIF_GLXAREA
        /* Step 4. */
        SUMAg_SVv[ic].X->FORM = XmCreateForm(SUMAg_SVv[ic].X->TOPLEVEL, "form", NULL, 0);
        XtManageChild(SUMAg_SVv[ic].X->FORM);
        SUMAg_SVv[ic].X->FRAME = XmCreateFrame(SUMAg_SVv[ic].X->FORM, "frame", NULL, 0);
        XtVaSetValues(SUMAg_SVv[ic].X->FRAME,
          XmNbottomAttachment, XmATTACH_FORM,
          XmNtopAttachment, XmATTACH_FORM,
          XmNleftAttachment, XmATTACH_FORM,
          XmNrightAttachment, XmATTACH_FORM,
          NULL);
        XtManageChild(SUMAg_SVv[ic].X->FRAME);

        /* Step 5. */
        SUMAg_SVv[ic].X->CMAP = SUMA_getShareableColormap(csv);

        /* Step 6. */
         /* glwMDrawingAreaWidgetClass requires libMesaGLwM.a */
         SUMAg_SVv[ic].X->GLXAREA = XtVaCreateManagedWidget("glxarea",
          glwMDrawingAreaWidgetClass, SUMAg_SVv[ic].X->FRAME,
          GLwNvisualInfo, SUMAg_SVv[ic].X->VISINFO,
          XtNcolormap, SUMAg_SVv[ic].X->CMAP,
          NULL);
      #else
      /* Step 4-6. */
         SUMAg_SVv[ic].X->CMAP = SUMA_getShareableColormap(&(SUMAg_SVv[ic]));

         /* create a frame to put glxarea in */
         SUMAg_SVv[ic].X->FRAME  = XmCreateFrame (mainw, "frame", NULL, 0);
         XtManageChild(SUMAg_SVv[ic].X->FRAME);

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
      
      /* keep track of count */
      SUMAg_N_SVv += 1;
     

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
   SUMA_RETURN (YUP);
}

void SUMA_ButtOpen_pushed (Widget w, XtPointer cd1, XtPointer cd2)
{
   static char FuncName[]={"SUMA_ButtOpen_pushed"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!SUMA_X_SurfaceViewer_Create ()) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_X_SurfaceViewer_Create.\n", FuncName);
   } 
   SUMA_RETURNe;
}

void SUMA_ButtClose_pushed (Widget w, XtPointer cd1, XtPointer cd2)
{
   static char FuncName[]={"SUMA_ButtClose_pushed"};
   int ic, Found;
   SUMA_Boolean LocalHead = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
         if (SUMAg_SVv[ic].X->MOMENTUMID) XtRemoveTimeOut(SUMAg_SVv[ic].X->MOMENTUMID);
         
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
            if (SUMAg_SVv[ic].X->ViewCont) {
               XWithdrawWindow(SUMAg_SVv[ic].X->DPY, 
               XtWindow(SUMAg_SVv[ic].X->ViewCont),
               XScreenNumberOfScreen(XtScreen(SUMAg_SVv[ic].X->ViewCont)));
            }
         #endif
         #ifdef SUMA_USE_DESTROY 
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying it.\n", FuncName);
            XtDestroyWidget(SUMAg_SVv[ic].X->TOPLEVEL);
            SUMAg_SVv[ic].X->TOPLEVEL = NULL;      
            
            /* no need to destroy viewer controller */
            SUMAg_SVv[ic].X->ViewCont = NULL;
            
            /* update the count */
            SUMAg_N_SVv -= 1;

         #endif

         SUMAg_SVv[ic].Open = NOPE;
         --SUMAg_CF->N_OpenSV;
         if (SUMAg_CF->N_OpenSV == 0) {
            if (LocalHead) fprintf (SUMA_STDERR,"%s: No more viewers, exiting.\n", FuncName);
            /* not quite necessary but for completeness */
            if (SUMAg_CF->X->SumaCont) {
               XtDestroyWidget(SUMAg_CF->X->SumaCont);
            }
            exit(0);
         }
   } else {
      fprintf (SUMA_STDERR,"Error %s: Widget not Found!.\n", FuncName);
   }
   
    SUMA_RETURNe;
}

Colormap
SUMA_getShareableColormap(SUMA_SurfaceViewer *csv)
{
   Status status;
   XStandardColormap *standardCmaps;
   Colormap cmap;
   int i, numCmaps;
   XVisualInfo * vi;
   static char FuncName[]={"SUMA_getShareableColormap"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   vi = csv->X->VISINFO;
   
   /* Be lazy; using DirectColor too involved for this example. */
#if defined(__cplusplus) || defined(c_plusplus)
   if (vi->c_class != TrueColor)
    XtAppError(SUMAg_CF->X->App, "no support for non-TrueColor visual");
#else 
   if (vi->class != TrueColor)
    XtAppError(SUMAg_CF->X->App, "no support for non-TrueColor visual");
#endif

   /* If no standard colormap but TrueColor, just make an
     unshared one. */
   status = XmuLookupStandardColormap(csv->X->DPY, vi->screen, vi->visualid,
    vi->depth, XA_RGB_DEFAULT_MAP,
    False,              /* Replace. */
    True);              /* Retain. */
   if (status == 1) {
    status = XGetRGBColormaps(csv->X->DPY, RootWindow(csv->X->DPY, vi->screen),
      &standardCmaps, &numCmaps, XA_RGB_DEFAULT_MAP);
    if (status == 1)
      for (i = 0; i < numCmaps; i++)
        if (standardCmaps[i].visualid == vi->visualid) {
          cmap = standardCmaps[i].colormap;
          XFree(standardCmaps);
          SUMA_RETURN(cmap);
        }
   }
   cmap = XCreateColormap(csv->X->DPY, RootWindow(csv->X->DPY, vi->screen), vi->visual, AllocNone);

  SUMA_RETURN(cmap);
}

void SUMA_SetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont)
{
   static char FuncName[]={"SUMA_SetcSV"};
   SUMA_SurfaceViewer *sv;
   int isv;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   #ifdef DARWIN
      /* Set the focus manually.
      If you're not using motif widgets, window focus is not managed.
      You can manage it yourself with XSetInputFocus when the EnterWindowEvent is captured.
      You don't need to do that however if you link (for some reason) to -lXm.
      But on the macosx10, -lXm does not help, so we manage the foucs ourselves */
      XSetInputFocus(XtDisplay(w), XtWindow(w), RevertToPointerRoot, CurrentTime);
   #endif
   

   /* When using multiple viewers, you must reset the OpenGL state variables or risk having abrupt changes with the first click */
   SUMA_ANY_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }

   if (LocalHead) fprintf (SUMA_STDERR, "%s: in Surface Viewer #%d.\n", FuncName, isv);
   sv->ResetGLStateVariables = YUP;  

   SUMA_postRedisplay(w, clientData, NULL);

   
   SUMA_RETURNe;
}

void SUMA_unSetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont)
{
   static char FuncName[]={"SUMA_unSetcSV"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
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
int SUMA_generateEPS(char *filename, int inColor, unsigned int width, unsigned int height);
GLvoid *SUMA_grabPixels(int inColor, unsigned int width, unsigned int height);

int
SUMA_generateEPS(char *filename, int inColor, unsigned int width, unsigned int height)
{
   FILE *fp;
   GLvoid *pixels;
   unsigned char *curpix;
   int components, pos, i;
   static char FuncName[]={"SUMA_generateEPS"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

/*!
   Purpose: Takes a the x,y positions of the cursor and sets the Pick0 and Pick1 values in sv 
   \param sv (*SUMA_SurfaceViewer)
   \param x (int) mouse coordinate
   \param y (int)
   \return YUP/NOPE
   \sa SUMA_input, button3 pick
*/
SUMA_Boolean SUMA_GetSelectionLine (SUMA_SurfaceViewer *sv, int x, int y)
{
   static char FuncName[]={"SUMA_GetSelectionLine"};
   GLfloat rotationMatrix[4][4];
   GLint viewport[4];
   GLdouble mvmatrix[16], projmatrix[16];
   GLint realy; /* OpenGL y coordinate position */
   char CommString[100];
   SUMA_EngineData ED;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   
   
   if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: Current Quat: %.4f, %.4f, %.4f, %.4f.\n", \
       FuncName, sv->GVS[sv->StdView].currentQuat[0], sv->GVS[sv->StdView].currentQuat[1], \
       sv->GVS[sv->StdView].currentQuat[2],sv->GVS[sv->StdView].currentQuat[3]);
      fprintf (SUMA_STDERR, "%s: Translation Vector of view #%d: %.4f, %.4f, %.4f\n", \
         FuncName, sv->StdView, sv->GVS[sv->StdView].translateVec[0], sv->GVS[sv->StdView].translateVec[1], \
         sv->GVS[sv->StdView].translateVec[2]);
      fprintf (SUMA_STDERR, "%s: RotaCenter of view #%d: %.4f, %.4f, %.4f\n", \
         FuncName, sv->StdView, sv->GVS[sv->StdView].RotaCenter[0], sv->GVS[sv->StdView].RotaCenter[1], \
         sv->GVS[sv->StdView].RotaCenter[2]);
   }
      
   
   /* go through the ModelView transforms as you would in display since the modelview matrix is popped
   after each display call */
   SUMA_build_rotmatrix(rotationMatrix, sv->GVS[sv->StdView].currentQuat);
   glMatrixMode(GL_MODELVIEW);
   /* The next line appears to fix some bug with GL_MODELVIEW's matrix. When you clicked button3 for the first time in a viewer, 
   the chosen point was off. The next click in the identical position would select the correct point and subsequent clicks are OK.
   None of the parameters used for the selection would change between the first click and the next but it appears that going from one
   viewer to the next caused GL_MODELVIEW to change (sometimes) slightly. Putting the line glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
   to check (and debug) what was happening to GL_MODELVIEW matrix between one viewer and the next fixed the clicking problem. So, we keep
   it here as a fix until a better one comes along. PS: This was also the source of the Z (blue) eye axis showing up when it should not. */  
      glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
      if (LocalHead) {
         int itmp = 0;
         fprintf (SUMA_STDERR, "%s: Initial Modelview:\nMV=[ ", FuncName);
         while (itmp < 16) { fprintf (SUMA_STDERR, "%.4f, ", mvmatrix[itmp]); ++itmp;}
         fprintf (SUMA_STDERR, "]\n");
      }
   glPushMatrix();
   glTranslatef (sv->GVS[sv->StdView].translateVec[0], sv->GVS[sv->StdView].translateVec[1], 0.0);
   glTranslatef (sv->GVS[sv->StdView].RotaCenter[0], sv->GVS[sv->StdView].RotaCenter[1], sv->GVS[sv->StdView].RotaCenter[2]);
   glMultMatrixf(&rotationMatrix[0][0]);
      glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
      if (LocalHead) {
         int itmp = 0;
         fprintf (SUMA_STDERR, "%s: Modelview After Translation & Rotation:\nMVtr=[ ", FuncName);
         while (itmp < 16) { fprintf (SUMA_STDERR, "%.4f, ", mvmatrix[itmp]); ++itmp;}
         fprintf (SUMA_STDERR, "]\n");
      }
   glTranslatef (-sv->GVS[sv->StdView].RotaCenter[0], -sv->GVS[sv->StdView].RotaCenter[1], -sv->GVS[sv->StdView].RotaCenter[2]);

   glGetIntegerv(GL_VIEWPORT, viewport);
   glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
   glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);
   /* viewport[3] is height of window in pixels */
   realy = viewport[3] - (GLint)y -1;

   if (LocalHead) fprintf (SUMA_STDOUT, "%s: Coordinates at cursor are (%4d, %4d)\n", FuncName, x, realy);

   /* set the pick points at both ends of the clip planes */
   gluUnProject((GLdouble)x, (GLdouble)realy, 0.0,\
      mvmatrix, projmatrix, viewport, \
      &(sv->Pick0[0]), &(sv->Pick0[1]), &(sv->Pick0[2]));
   if (LocalHead) fprintf (SUMA_STDOUT, "World Coords at z=0.0 (near clip plane) are (%f, %f, %f)\n",\
      (sv->Pick0[0]), (sv->Pick0[1]), (sv->Pick0[2]));

   gluUnProject((GLdouble)x, (GLdouble)realy, 1.0,\
      mvmatrix, projmatrix, viewport, \
      &(sv->Pick1[0]), &(sv->Pick1[1]), &(sv->Pick1[2]));
   if (LocalHead) fprintf (SUMA_STDOUT, "World Coords at z=1.0 (far clip plane) are (%f, %f, %f)\n",\
      (sv->Pick1[0]), (sv->Pick1[1]), (sv->Pick1[2]));

   glPopMatrix();

   SUMA_RETURN (YUP);
}

void SUMA_cb_close(Widget w, XtPointer data, XtPointer callData)
{
   SUMA_SurfaceViewer *sv;
   static char FuncName[] = {"SUMA_cb_close"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   sv = (SUMA_SurfaceViewer *)data;
   SUMA_ButtClose_pushed (sv->X->GLXAREA, data, callData);
   
   SUMA_RETURNe;
}

/*!
   \brief callback to open SUMA 's Controller
*/
void SUMA_cb_view_suma_controller(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_view_suma_controller"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SUMAg_CF->X->SumaCont) { /* create */
      SUMA_cb_createSumaController( w, data, callData);
   }else {
      /* controller already created, need to bring it up again */
      #ifdef SUMA_USE_WITHDRAW
         XMapRaised(SUMAg_CF->X->DPY_controller1, XtWindow(SUMAg_CF->X->SumaCont));      
      #endif
   }

   SUMA_RETURNe;
}
void SUMA_cb_view_surface_controller(Widget w, XtPointer data, XtPointer callData)
{
   SUMA_SurfaceViewer *sv;
   static char FuncName[] = {"SUMA_cb_view_surface_controller"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   sv = (SUMA_SurfaceViewer *)data;
/* if (!Created) {
      XtVaAppCreateShell (NULL, "Class", 
         topLevelShellWidgetClass, dpy, 
         XtNtitle, "Dialog Shell Title",
         NULL); pp 229
   }
   
   * map to screen *
   XtPopup
   
   * raise to top *
   XMapRaised
   if open then raise
   else open
   */   
   
   fprintf (SUMA_STDERR,"%s: Not setup yet.\n", FuncName);
   
   SUMA_RETURNe;
}

void SUMA_cb_view_viewer_controller(Widget w, XtPointer data, XtPointer callData)
{
   SUMA_SurfaceViewer *sv;
   SUMA_Boolean LocalHead = YUP;
   static char FuncName[] = {"SUMA_cb_view_viewer_controller"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   sv = (SUMA_SurfaceViewer *)data;
   if (!sv->X->ViewCont) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_cb_createViewerController.\n", FuncName);
      SUMA_cb_createViewerController( w, data, callData);
   }else {
      /* controller already created, need to bring it up again */
      
      #ifdef SUMA_USE_WITHDRAW
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Controller already created, Raising it.\n", FuncName);
         XMapRaised(sv->X->DPY, XtWindow(sv->X->ViewCont));      
      #endif

   }
   
   SUMA_RETURNe;
}


/* the function expects the surface viewer pointer in data */
void SUMA_cb_toggle_crosshair(Widget w, XtPointer data, XtPointer callData)
{
   char CommString[SUMA_MAX_COMMAND_LENGTH];
   static char FuncName[] = {"SUMA_cb_toggle_crosshair"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
      
   sprintf(CommString, "Redisplay|ToggleCrossHair~");
   if (!SUMA_Engine (CommString, NULL, (SUMA_SurfaceViewer *)data)) {
      fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
   }
   
   SUMA_RETURNe;
}
 
void SUMA_cb_toggle_node_in_focus(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_toggle_node_in_focus"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_RETURNe;
}

void SUMA_cb_toggle_selected_faceset(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_toggle_selected_faceset"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_RETURNe;
}         

/*! Creates the dialog shell of the viewer controller.
*/
#define SUMA_CONTROLLER_AS_DIALOG 0 /* controller widgets as dialog (1) or toplevelshells (0) 
                                    Stick with toplevelshells or window managers might force 
                                    you to keep them atop the surface viewers. Downside is that
                                    it is managed such that if the viewer is minimized, the controller is not.
                                    But that is not necessarily a bad thing. */
void SUMA_cb_createViewerController(Widget w, XtPointer data, XtPointer callData)
{
   Widget tl, rc, pb;
   Display *dpy;
   SUMA_SurfaceViewer *sv;
   int isv;    
   char slabel[100]; 

   static char FuncName[] = {"SUMA_cb_createViewerController"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   sv = (SUMA_SurfaceViewer *)data;
   isv = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
   
   if (sv->X->ViewCont) {
      fprintf (SUMA_STDERR,"Error %s: sv->X->ViewCont!=NULL. Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   tl = SUMA_GetTopShell(w); /* top level widget */
   dpy = XtDisplay(tl);
   
   sprintf(slabel,"[%c] Viewer Controller", 65+isv);
   
   #if SUMA_CONTROLLER_AS_DIALOG /*xmDialogShellWidgetClass, topLevelShellWidgetClass*/
   sv->X->ViewCont = XtVaCreatePopupShell (slabel,
      xmDialogShellWidgetClass, tl,
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);    
   #else
   sv->X->ViewCont = XtVaCreatePopupShell (slabel,
      topLevelShellWidgetClass, tl,
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);   
      /* pop it up if it is a topLevelShellWidgetClass */
      XtPopup(sv->X->ViewCont, XtGrabNone);
   #endif
   
   /* handle the close button from window manager */
   XmAddWMProtocolCallback(/* make "Close" window menu work */
      sv->X->ViewCont,
      XmInternAtom( dpy , "WM_DELETE_WINDOW" , False ) ,
      SUMA_cb_closeViewerController, (XtPointer) sv) ;
   
   rc = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, sv->X->ViewCont,
      NULL);
   
   /* create some button */
   pb = XmCreatePushButton (rc, "Close", NULL, 0);
   XtAddCallback (pb, XmNactivateCallback, SUMA_cb_closeViewerController, (XtPointer) sv);
   XtManageChild (pb);     
   
   /* now start managing the row column widget */
   XtManageChild (rc);
   
   /* realize the widget */
   XtRealizeWidget (tl);
   
   SUMA_RETURNe;
}

void SUMA_cb_closeViewerController(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_closeViewerController"};
   SUMA_SurfaceViewer *sv;
   SUMA_Boolean LocalHead = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   sv = (SUMA_SurfaceViewer *)data;
   
   if (!sv->X->ViewCont) SUMA_RETURNe;

   #ifdef SUMA_USE_WITHDRAW 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing Viewer Controller...\n", FuncName);
      
      XWithdrawWindow(sv->X->DPY, 
         XtWindow(sv->X->ViewCont),
         XScreenNumberOfScreen(XtScreen(sv->X->ViewCont)));
   #endif
   #ifdef SUMA_USE_DESTROY 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying Viewer Controller...\n", FuncName);
      XtDestroyWidget(sv->X->ViewCont);
      sv->X->ViewCont = NULL;
   #endif

    
   SUMA_RETURNe;

}

#define SUMA_MARGIN  3

void SUMA_cb_createSumaController(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSumaController"};
   SUMA_SurfaceViewer *sv;
   Widget rc, pb, frame, form;
   SUMA_Boolean LocalHead = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (SUMAg_CF->X->SumaCont) {
      fprintf (SUMA_STDERR,"Error %s: SUMAg_CF->X->SumaCont!=NULL. Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }

   sv = (SUMA_SurfaceViewer *)data;

   /* create as a separate application shell, you do not want a parent to this controller that
   can be closed or withdrawn temporarily */
   SUMAg_CF->X->SumaCont = XtVaAppCreateShell("Suma Controller" , "Suma" ,
      topLevelShellWidgetClass , SUMAg_CF->X->DPY_controller1 ,
      XmNinitialResourcesPersistent , False , /* I am not planning on destroying this widget but perhaps someday */
      NULL ) ;
   
   /* handle the close button from window manager */
   XmAddWMProtocolCallback(/* make "Close" window menu work */
      SUMAg_CF->X->SumaCont,
      XmInternAtom( SUMAg_CF->X->DPY_controller1 , "WM_DELETE_WINDOW" , False ) ,
      SUMA_cb_closeSumaController, NULL) ;
   
   /* create a form widget, manage it at the end ...*/
   form = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SUMAg_CF->X->SumaCont,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
      
   /* a smaller frame to put the lockstuff in */
   frame = XtVaCreateWidget ("dialog",
      xmFrameWidgetClass, form,
      XmNleftAttachment , XmATTACH_FORM ,
      XmNtopAttachment  , XmATTACH_FORM ,
      XmNshadowType , XmSHADOW_ETCHED_IN ,
      XmNshadowThickness , 5 ,
      XmNtraversalOn , False ,
      NULL); 
   
   #ifndef MOTIF_1_2
      XtVaCreateManagedWidget ("Lock",
         xmLabelGadgetClass, frame, 
         XmNchildType, XmFRAME_TITLE_CHILD,
         XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
         NULL);
   #endif   
   
   rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, frame,
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, SUMA_MARGIN ,
         XmNmarginWidth , SUMA_MARGIN ,
         XmNrightAttachment, XmATTACH_SELF,     
         NULL);
   
   /* create some button */
   pb = XtVaCreateManagedWidget("open",
      xmPushButtonWidgetClass, rc, 
      XmNrightAttachment, XmATTACH_SELF, 
      NULL);
   XtAddCallback (pb, XmNactivateCallback, SUMA_cb_closeSumaController, NULL);
   
   pb = XmCreatePushButton (rc, "Close", NULL, 0);
   XtAddCallback (pb, XmNactivateCallback, SUMA_cb_closeSumaController, NULL);
   XtManageChild (pb);  
   
   /* now start managing the widgets */
   XtManageChild (rc);
   XtManageChild (frame);
   XtManageChild (form);
   
   /* realize the widget */
   XtRealizeWidget (SUMAg_CF->X->SumaCont);
   SUMA_RETURNe;
}

void SUMA_cb_closeSumaController(Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_closeSumaController"};
   SUMA_Boolean LocalHead = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SUMAg_CF->X->SumaCont) SUMA_RETURNe;
   
   #ifdef SUMA_USE_WITHDRAW 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Withdrawing Suma Controller...\n", FuncName);
      
      XWithdrawWindow(SUMAg_CF->X->DPY_controller1, 
         XtWindow(SUMAg_CF->X->SumaCont),
         XScreenNumberOfScreen(XtScreen(SUMAg_CF->X->SumaCont)));
   #endif
   #ifdef SUMA_USE_DESTROY 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Destroying Suma Controller...\n", FuncName);
      XtDestroyWidget(->X->SumaCont);
      SUMAg_CF->X->SumaCont = NULL;
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
