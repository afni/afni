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
static int snglBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 16,
   GLX_RED_SIZE, 1, GLX_BLUE_SIZE, 1, GLX_GREEN_SIZE, 1,  None};
static int dblBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 16,
  GLX_RED_SIZE, 1, GLX_BLUE_SIZE, 1, GLX_GREEN_SIZE, 1, GLX_DOUBLEBUFFER, None};

static String fallbackResources_default[] = {
   "*glxarea*width: 300", "*glxarea*height: 300",
   "*frame*x: 20", "*frame*y: 20",
   "*frame*topOffset: 20", "*frame*bottomOffset: 20",
   "*frame*rightOffset: 20", "*frame*leftOffset: 20",
   "*frame*shadowType: SHADOW_IN", 
   "*fontList:              9x15bold=charset1"    ,
   "*pbar*fontList:         6x10=charset1"        ,
   "*imseq*fontList:        7x13=charset1"        ,
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
   "*background:            green"               ,
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
  NULL
}; /* if you change default width and height, make sure you change SV->X->WIDTH & SV->X->HEIGHT in SUMA_SVmanip */

static String fallbackResources_NP[] = {
   "*glxarea*width: 300", "*glxarea*height: 300",
   "*frame*x: 20", "*frame*y: 20",
   "*frame*topOffset: 20", "*frame*bottomOffset: 20",
   "*frame*rightOffset: 20", "*frame*leftOffset: 20",
   "*frame*shadowType: SHADOW_IN", 
   "*fontList:              9x15=charset1"    ,
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
  NULL
}; /* if you change default width and height, make sure you change SV->X->WIDTH & SV->X->HEIGHT in SUMA_SVmanip */

static String fallbackResources_Bonaire[] = {
   "*glxarea*width: 300", "*glxarea*height: 300",
   "*frame*x: 20", "*frame*y: 20",
   "*frame*topOffset: 20", "*frame*bottomOffset: 20",
   "*frame*rightOffset: 20", "*frame*leftOffset: 20",
   "*frame*shadowType: SHADOW_IN", 
   "*fontList:              9x15bold=charset1"    ,
   "*pbar*fontList:         6x10=charset1"        ,
   "*imseq*fontList:        7x13=charset1"        ,
   "*background:            blue2"               ,
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
  NULL
}; /* if you change default width and height, make sure you change SV->X->WIDTH & SV->X->HEIGHT in SUMA_SVmanip */

/*!

- use matlab script readXcol to choose different color settings
*/

String *SUMA_get_fallbackResources ()
{
   static char FuncName[]={"SUMA_get_fallbackResources"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   switch (SUMAg_CF->X->X_Resources) {
      case SXR_Afni:
         SUMA_RETURN (fallbackResources_AFNI);
         break;
      case SXR_NP:
         SUMA_RETURN (fallbackResources_NP);
         break;
      case SXR_Bonaire:
         SUMA_RETURN (fallbackResources_Bonaire);
         break;
      case SXR_default:
      default:
         SUMA_RETURN (fallbackResources_default);
   }

}

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
   SUMA_SurfaceObject *SO=NULL;
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

   /* cycle through csv->RegisteredDO and display those things that have a fixed CoordType*/
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Creating objects with fixed coordinates ...\n", FuncName);
   i = 0;
   while (i < csv->N_DO) {
      if (dov[csv->RegisteredDO[i]].CoordType == SUMA_SCREEN) {
         switch (dov[csv->RegisteredDO[i]].ObjectType) {
            case SO_type:
               break;
            case AO_type:
               if (csv->ShowEyeAxis){
                  if (!SUMA_DrawAxis ((SUMA_Axis*)dov[csv->RegisteredDO[i]].OP)) {
                     fprintf(SUMA_STDERR,"Error %s: Could not display EYE AXIS\n", FuncName);
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
            case LS_type:
               if (!SUMA_DrawSegmentDO ((SUMA_SegmentDO *)dov[csv->RegisteredDO[i]].OP)) {
                  fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_DrawSegmentDO.\n", FuncName);
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

   /* cycle through csv->RegisteredDO and display those things that have a Local CoordType*/
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Creating objects with local coordinates ...\n", FuncName);
   i = 0;
   while (i < csv->N_DO) {
      if (dov[csv->RegisteredDO[i]].CoordType == SUMA_LOCAL) {
         switch (dov[csv->RegisteredDO[i]].ObjectType) {
            case SO_type:
               SO = (SUMA_SurfaceObject *)dov[csv->RegisteredDO[i]].OP;
               if (SO->Show) {
                  if (  (SO->Side == SUMA_LEFT && csv->ShowLeft) || 
                        (SO->Side == SUMA_RIGHT && csv->ShowRight) ||
                        SO->Side == SUMA_NO_SIDE) {
                        SUMA_DrawMesh(SO, csv); /* create the surface */
                  }
               }
               break;
            case AO_type:
               if (csv->ShowMeshAxis) {
                  if (!SUMA_DrawAxis ((SUMA_Axis*)dov[csv->RegisteredDO[i]].OP)) {
                     fprintf(stderr,"display error: Could not display Mesh AXIS\n");
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
            case LS_type:
               if (!SUMA_DrawSegmentDO ((SUMA_SegmentDO *)dov[csv->RegisteredDO[i]].OP)) {
                  fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_DrawSegmentDO.\n", FuncName);
               }
               break;
         }
      }
      ++i;
   }
   
   /* Show the Cross Hair, if required */
   if (csv->ShowCrossHair) {
      /*fprintf(SUMA_STDOUT,"Showing Cross Hair \n");*/
      if (!SUMA_DrawCrossHair (csv->Ch)) {
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
  
  /* if recording, take a snap */
  if (csv->Record) {
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
  
   SUMA_LH("Called");
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
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_LH("Called");
   
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
   where ocv is the original callback value. This allows you to create multiple versions 
   of the same menu and still be able to dissociate between them.
   \param MenuWidgets (Widget *) pointer to a vector that will contain widgets created.
    
-  This function is largely based on BuildMenu in the "Motif Programming Manual"
  Build popup, option and pulldown menus, depending on the menu_type.
  It may be XmMENU_PULLDOWN, XmMENU_OPTION or  XmMENU_POPUP.  Pulldowns
  return the CascadeButton that pops up the menu.  Popups return the menu.
  Option menus are created, but the RowColumn that acts as the option
  "area" is returned unmanaged. (The user must manage it.)
  Pulldown menus are built from cascade buttons, so this function
  also builds pullright menus.  The function also adds the right
  callback for PushButton or ToggleButton menu items.
  
 */
Widget SUMA_BuildMenu(Widget parent, int menu_type, char *menu_title, char menu_mnemonic, SUMA_Boolean tear_off, SUMA_MenuItem *items, void *ContID, Widget *MenuWidgets )
{
   static char FuncName[]={"SUMA_BuildMenu"};
   Widget menu = NULL, cascade = NULL;
   int i;
   XmString str;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Here.\n", FuncName);
   
   if (menu_type == XmMENU_PULLDOWN || menu_type == XmMENU_OPTION)
     menu = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
   else if (menu_type == XmMENU_POPUP)
     menu = XmCreatePopupMenu (parent, "_popup", NULL, 0);
   else {
     XtWarning ("Invalid menu type passed to BuildMenu()");
     SUMA_RETURN(NULL);
   }
   if (tear_off)
     XtVaSetValues (menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);

   /* Pulldown menus require a cascade button to be made */
   if (menu_type == XmMENU_PULLDOWN) {
     str = XmStringCreateLocalized (menu_title);
     cascade = XtVaCreateManagedWidget (menu_title,
         xmCascadeButtonGadgetClass, parent,
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
     str = XmStringCreateLocalized (menu_title);
     XtSetArg (args[n], XmNsubMenuId, menu); n++;
     XtSetArg (args[n], XmNlabelString, str); n++;
     XtSetArg (args[n], XmNmarginHeight, 0); n++;
     XtSetArg (args[n], XmNmarginTop, 0 ); n++;
     XtSetArg (args[n], XmNmarginBottom, 0 ); n++;
     
     /* This really isn't a cascade, but this is the widget handle
      * we're going to return at the end of the function.
      */
     cascade = XmCreateOptionMenu (parent, menu_title, args, n);
     XmStringFree (str);
   }

   /* Now add the menu items */
   for (i = 0; items[i].label != NULL; i++) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Adding label # %d - %s\n", FuncName, i, items[i].label);
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
             if (LocalHead) fprintf (SUMA_STDERR, "%s: Going for sub-menu.\n", FuncName);
             MenuWidgets[(int)items[i].callback_data] = SUMA_BuildMenu (menu, XmMENU_PULLDOWN, items[i].label, 
                 items[i].mnemonic, tear_off, items[i].subitems, ContID, MenuWidgets);
         }
     else {
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Creating widgets MenuWidgets[%d]\n", FuncName, (int)items[i].callback_data);
         MenuWidgets[(int)items[i].callback_data] = XtVaCreateManagedWidget (items[i].label,
             *items[i].class, menu,
             NULL);
      }

      
      /* Whether the item is a real item or a cascade button with a
      * menu, it can still have a mnemonic.
      */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Setting Mnemonic ...\n", FuncName);
      if (items[i].mnemonic)
         XtVaSetValues (MenuWidgets[(int)items[i].callback_data], XmNmnemonic, items[i].mnemonic, NULL);

      /* any item can have an accelerator, except cascade menus. But,
      * we don't worry about that; we know better in our declarations.
      */

      if (LocalHead) fprintf (SUMA_STDERR, "%s: Setting accelerator ...\n", FuncName);
      if (items[i].accelerator) {
         str = XmStringCreateLocalized (items[i].accel_text);
         XtVaSetValues (MenuWidgets[(int)items[i].callback_data],
             XmNaccelerator, items[i].accelerator,
             XmNacceleratorText, str,
             NULL);
         XmStringFree (str);
      }

      if (items[i].class == &xmToggleButtonWidgetClass ||
              items[i].class == &xmToggleButtonGadgetClass) {
         Pixel fg_pix;
         XtVaGetValues (MenuWidgets[(int)items[i].callback_data], XmNforeground, &fg_pix, NULL);
         XtVaSetValues (MenuWidgets[(int)items[i].callback_data], XmNselectColor, fg_pix, NULL); 
          
      }
     
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Setting callback ...\n", FuncName);
      if (items[i].callback) {
         SUMA_MenuCallBackData *CBp=NULL;
         CBp = (SUMA_MenuCallBackData *)malloc (sizeof(SUMA_MenuCallBackData)); /* There is no freeing of this pointer in SUMA. Once created, a widget is only destroyed when SUMA is killed */
         /* prepare the callback pointer */
         CBp->callback_data = (XtPointer) items[i].callback_data;
         CBp->ContID = ContID;
         XtAddCallback (MenuWidgets[(int)items[i].callback_data],
             (items[i].class == &xmToggleButtonWidgetClass ||
              items[i].class == &xmToggleButtonGadgetClass) ?
                 XmNvalueChangedCallback : /* ToggleButton class */
                 XmNactivateCallback,      /* PushButton class */
             items[i].callback, (XtPointer)CBp);
      }
   }

   /* for popup menus, just return the menu; pulldown menus, return
   * the cascade button; option menus, return the thing returned
   * from XmCreateOptionMenu().  This isn't a menu, or a cascade button!
   */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Returning.\n", FuncName);
   SUMA_RETURN (menu_type == XmMENU_POPUP ? menu : cascade);
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
   {  "Open", &xmPushButtonWidgetClass, \
      '\0', NULL, NULL, \
      NULL,  (XtPointer) SW_FileOpen, (SUMA_MenuItem *) FileOpen_menu },
   
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
   {  "Viewer Usage", &xmPushButtonWidgetClass, \
      'V', "Ctrl <Key>h", "Ctrl+h", \
      SUMA_cb_helpViewer, (XtPointer) SW_HelpViewer, NULL},
      
   {  "Message Log", &xmPushButtonWidgetClass, \
      'L', NULL, NULL, \
      SUMA_cb_helpMessageLog, (XtPointer) SW_HelpMessageLog, NULL},
      
   {  "Separator 1", &xmSeparatorWidgetClass, \
      '\0', NULL, NULL, \
      NULL, (XtPointer) SW_HelpSep1, NULL }, 
      
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

      
SUMA_Boolean SUMA_X_SurfaceViewer_Create (void)
{
   static char FuncName[]={"SUMA_X_SurfaceViewer_Create"};
   static int CallNum = 0;
   int ic = 0;
   char *vargv[1]={ "[A] SUMA" };
   int cargc = 1;
   SUMA_Boolean NewCreation = NOPE, Found;
   SUMA_Boolean LocalHead = NOPE;
   char slabel[20]; 
       
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* Step 1. */
   if (CallNum == 0) { /* first call, initialize App */
      SUMAg_CF->N_OpenSV = 0;
      SUMAg_SVv[ic].X->TOPLEVEL = XtAppInitialize(&SUMAg_CF->X->App, "SUMA", NULL, 0, &cargc, vargv,
       SUMA_get_fallbackResources(), NULL, 0);
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
         SUMAg_SVv[ic].X->DPY = SUMAg_CF->X->DPY_controller1;
         SUMAg_SVv[ic].X->TOPLEVEL = XtVaAppCreateShell("Not Yet" , "Suma" ,
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
      if (LocalHead) fprintf(stdout, "trying for cool double buffer visual\n");
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
         
         /* create File Menu */
         SUMAg_SVv[ic].X->FileMenu[SW_File] = SUMA_BuildMenu(menubar, XmMENU_PULLDOWN, \
                                 "File", 'F', YUP, File_menu, \
                                 (void *)ic, SUMAg_SVv[ic].X->FileMenu );
         
         /* create View Menu */
         SUMAg_SVv[ic].X->ViewMenu[SW_View] = SUMA_BuildMenu(menubar, XmMENU_PULLDOWN, \
                                 "View", 'V', YUP, View_menu, \
                                 (void *)ic, SUMAg_SVv[ic].X->ViewMenu );
         
         /* create Tools Menu */
         SUMAg_SVv[ic].X->ToolsMenu[SW_Tools] = SUMA_BuildMenu(menubar, XmMENU_PULLDOWN, \
                                 "Tools", 'T', YUP, Tools_menu, \
                                 (void *)ic, SUMAg_SVv[ic].X->ToolsMenu );
         
         /* create Help Menu */
         SUMAg_SVv[ic].X->HelpMenu[SW_Help] = SUMA_BuildMenu(menubar, XmMENU_PULLDOWN, \
                                 "Help", 'H', YUP, Help_menu, \
                                 (void *)ic, SUMAg_SVv[ic].X->HelpMenu );
         
         XtVaSetValues (menubar, XmNmenuHelpWidget, SUMAg_SVv[ic].X->HelpMenu[SW_Help], NULL);
                                 
         /* set states of the some view menu widgets */
         XmToggleButtonSetState (SUMAg_SVv[ic].X->ViewMenu[SW_ViewCrossHair], 
            SUMAg_SVv[ic].ShowCrossHair, NOPE);
         
         XmToggleButtonSetState (SUMAg_SVv[ic].X->HelpMenu[SW_HelpMemTrace], 
            SUMAg_CF->MemTrace, NOPE);
         XmToggleButtonSetState (SUMAg_SVv[ic].X->HelpMenu[SW_HelpIONotify], 
            SUMAg_CF->InOut_Notify, NOPE);
         
 
         
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
        SUMAg_SVv[ic].X->CMAP = SUMA_getShareableColormap(&(SUMAg_SVv[ic]));

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
   
   SUMA_UpdateViewerTitle (&(SUMAg_SVv[ic]));

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

void SUMA_cb_FileOpenSpec (Widget w, XtPointer data, XtPointer calldata) 
{
   static char FuncName[]={"SUMA_cb_FileOpenSpec"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   fprintf (SUMA_STDERR, "%s: called.\n", FuncName);
   SUMA_RETURNe;
}

void SUMA_cb_FileOpenSurf (Widget w, XtPointer data, XtPointer calldata) 
{
   static char FuncName[]={"SUMA_cb_FileOpenSurf"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   fprintf (SUMA_STDERR, "%s: called.\n", FuncName);
   SUMA_RETURNe;
}

void SUMA_cb_FileClose (Widget w, XtPointer data, XtPointer calldata) 
{
   static char FuncName[]={"SUMA_cb_FileClose"};
   int isv, widtype;
   SUMA_SurfaceViewer *sv;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

Colormap
SUMA_getShareableColormap(SUMA_SurfaceViewer *csv)
{
   Status status;
   XStandardColormap *standardCmaps;
   Colormap cmap;
   int i, numCmaps;
   XVisualInfo * vi;
   SUMA_Boolean LocalHead = NOPE;
   static char FuncName[]={"SUMA_getShareableColormap"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   vi = csv->X->VISINFO;
   
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


/*!
   \brief A call back to open the help window 
   No input parameters needed
*/
void SUMA_cb_helpViewer (Widget w, XtPointer data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_helpViewer"};
   DList *list = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Log, SES_Suma, NULL); 
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMAg_CF->InOut_Notify = !SUMAg_CF->InOut_Notify;

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMAg_CF->MemTrace = !SUMAg_CF->MemTrace;
   
   /* must update the state of toggle buttons in otherviewers */
   for (ii=0; ii<SUMAg_N_SVv; ++ii) {
      if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
         /* you must check for both conditions because by default 
         all viewers are initialized to isShaded = NOPE, even before they are ever opened */
         if (w != SUMAg_SVv[ii].X->HelpMenu[SW_HelpMemTrace]) {
            XmToggleButtonSetState (SUMAg_SVv[ii].X->HelpMenu[SW_HelpMemTrace], 
               SUMAg_CF->MemTrace, NOPE);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_VIEWER_FROM_VIEWMENU_CALLBACK (data, isv, widtype);
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: A call from viewer %d, widget %d.\n", FuncName, isv, widtype);
   
   sv = &SUMAg_SVv[isv];
   if (sv->Focus_SO_ID >= 0) {
    SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   }else {
      fprintf (SUMA_STDERR,"%s: No surface object in focus.\n", FuncName);
      SUMA_RETURNe;
   }
   
   
   if (!SO->SurfCont->TopLevelShell) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_cb_createSurfaceCont.\n", FuncName);
      SUMA_cb_createSurfaceCont( w, (XtPointer)SO, callData);
   }else {
      /* controller already created, need to bring it up again */
      #ifdef SUMA_USE_WITHDRAW
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Controller already created, Raising it.\n", FuncName);
         XMapRaised(SUMAg_CF->X->DPY_controller1, XtWindow(SO->SurfCont->TopLevelShell));      
      #endif

   }
   
   if (SO->SurfCont->PosRef != sv->X->TOPLEVEL) {
      SO->SurfCont->PosRef = sv->X->TOPLEVEL;
      SUMA_PositionWindowRelative (SO->SurfCont->TopLevelShell, SO->SurfCont->PosRef, SWP_TOP_RIGHT);   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   Widget tl, rc, pb;
   Display *dpy;
   SUMA_SurfaceViewer *sv;
   int isv;    
   char slabel[100]; 

   static char FuncName[] = {"SUMA_cb_createViewerCont"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   sv = (SUMA_SurfaceViewer *)data;
   isv = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
   
   if (sv->X->ViewCont->TopLevelShell) {
      fprintf (SUMA_STDERR,"Error %s: sv->X->ViewCont->TopLevelShell!=NULL. Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   tl = SUMA_GetTopShell(w); /* top level widget */
   dpy = XtDisplay(tl);
   
   sprintf(slabel,"[%c] Viewer Controller", 65+isv);
   
   #if SUMA_CONTROLLER_AS_DIALOG /*xmDialogShellWidgetClass, topLevelShellWidgetClass*/
   sv->X->ViewCont->TopLevelShell = XtVaCreatePopupShell (slabel,
      xmDialogShellWidgetClass, tl,
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);    
   #else
   /** Feb 03/03: I was using XtVaCreatePopupShell to create a topLevelShellWidgetClass. 
   XtVaCreatePopupShell is used to create dialog shells not toplevel or appshells */
   sv->X->ViewCont->TopLevelShell = XtVaAppCreateShell (slabel, "Suma",
      topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);   
      /* pop it up if it is a topLevelShellWidgetClass */
      XtPopup(sv->X->ViewCont->TopLevelShell, XtGrabNone);
   #endif
   
   /* handle the close button from window manager */
   XmAddWMProtocolCallback(/* make "Close" window menu work */
      sv->X->ViewCont->TopLevelShell,
      XmInternAtom( dpy , "WM_DELETE_WINDOW" , False ) ,
      SUMA_cb_closeViewerCont, (XtPointer) sv) ;
   
   rc = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, sv->X->ViewCont->TopLevelShell,
      NULL);
   
   /* create some button */
   pb = XmCreatePushButton (rc, "Close", NULL, 0);
   XtAddCallback (pb, XmNactivateCallback, SUMA_cb_closeViewerCont, (XtPointer) sv);
   XtManageChild (pb);     
   
   /* now start managing the row column widget */
   XtManageChild (rc);
   
   /* realize the widget */
   XtRealizeWidget (tl);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   Widget tl, pb, form, DispFrame, SurfFrame, RenderSetFrame, rc_left, rc_right, rc_mamma;
   Display *dpy;
   SUMA_SurfaceObject *SO;
   char *slabel; 
   SUMA_Boolean LocalHead = NOPE;
   static char FuncName[] = {"SUMA_cb_createSurfaceCont"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (SO->SurfCont->TopLevelShell) {
      fprintf (SUMA_STDERR,"Error %s: SO->SurfCont->TopLevelShell!=NULL. Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   tl = SUMA_GetTopShell(w); /* top level widget */
   dpy = XtDisplay(tl);
   
   slabel = (char *)SUMA_malloc (sizeof(char) * (strlen(SO->Label) + 100));
   sprintf(slabel,"[%s] Surface Controller", SO->Label);
   
   #if SUMA_CONTROLLER_AS_DIALOG /*xmDialogShellWidgetClass, topLevelShellWidgetClass*/
   if (LocalHead) fprintf(SUMA_STDERR, "%s: Creating dialog shell.\n", FuncName);
   SO->SurfCont->TopLevelShell = XtVaCreatePopupShell (slabel,
      xmDialogShellWidgetClass, tl,
      XmNallowShellResize, True, /* let code resize shell */
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);    
   #else
   if (LocalHead) fprintf(SUMA_STDERR, "%s: Creating toplevel shell.\n", FuncName);
   /** Feb 03/03: I was using XtVaCreatePopupShell to create a topLevelShellWidgetClass. 
   XtVaCreatePopupShell is used to create dialog shells not toplevel or appshells. 
   Of course, it made no difference! */
   SO->SurfCont->TopLevelShell = XtVaAppCreateShell (slabel, "Suma",
      topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
      XmNdeleteResponse, XmDO_NOTHING,
      NULL);   
   #endif
   
   /* allow for code to resize the shell */
   XtVaSetValues (SO->SurfCont->TopLevelShell, 
         XmNresizePolicy , XmRESIZE_NONE , /* allow (?) childrent to resize */
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
            NULL);
   
   rc_right = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_mamma,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL); 
                    
   {/*s surface label and info */ 
      Widget rc, label;
     
      /* put a frame */
      SurfFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 

      /* row column Lock rowcolumns */
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfFrame,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);

      /*put a label containing the surface name, number of nodes and number of facesets */
      sprintf(slabel,"%s\n%d nodes: %d facesets", SO->Label, SO->N_Node, SO->N_FaceSet); 
      label = XtVaCreateManagedWidget (slabel, 
               xmLabelWidgetClass, rc,
               NULL);

      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, rc, 
                                 XmNorientation, XmVERTICAL,NULL);

      SO->SurfCont->SurfInfo_pb = XtVaCreateWidget ("more", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (SO->SurfCont->SurfInfo_pb, XmNactivateCallback, SUMA_cb_moreSurfInfo, NULL);
      XtVaSetValues (SO->SurfCont->SurfInfo_pb, XmNuserData, (XtPointer)SO, NULL); /* store the surface object SO in userData
                                                                  I think it is more convenient than as data
                                                                  in the call back structure. This way it will
                                                                  be easy to change the SO that this same button
                                                                  might refer to. 
                                                                  This is only for testing purposes, the pb_close
                                                                  button still expects SO in clientData*/
      MCW_register_hint( SO->SurfCont->SurfInfo_pb , "More info on Surface" ) ;
      MCW_register_help( SO->SurfCont->SurfInfo_pb , SUMA_moreSurfInfo_help ) ;
      XtManageChild (SO->SurfCont->SurfInfo_pb); 

      XtManageChild (rc);
      
      XtManageChild (SurfFrame);
   }  
   
   /* put the colorplane frame */
   {
       Widget rc, rcv, pb;
     
      
      /* put a frame */
      SO->SurfCont->ColPlane_fr = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_right,
         XmNrightAttachment , XmATTACH_FORM ,
         XmNleftAttachment, XmATTACH_WIDGET,
         XmNleftWidget, SurfFrame,
         XmNtopAttachment  , XmATTACH_FORM ,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 

      /* vertical row column */
      rcv = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->ColPlane_fr,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);
            
      /* row column for label*/
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rcv,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);
      
      /*put a label containing the surface name, number of nodes and number of facesets */
      sprintf(slabel,"Label:-\nParent:-"); 
      SO->SurfCont->ColPlaneLabel_Parent_lb = XtVaCreateManagedWidget (slabel, 
               xmLabelWidgetClass, rcv,
               NULL);
      XtManageChild (rc);
      
      /* add a rc for the colorplane label and the ROI node value */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
   
      SUMA_CreateArrowField ( rc, "Order:",
                           1, 0, 20, 1,
                           2, SUMA_int,
                           NOPE,
                           SUMA_ColPlane_NewOrder, (void *)SO,
                           SO->SurfCont->ColPlaneOrder);
                             
      SUMA_CreateArrowField ( rc, "Opacity:",
                           1, 0, 1.1, 0.1,
                           3, SUMA_float,
                           NOPE,
                           SUMA_ColPlane_NewOpacity, (void *)SO,
                           SO->SurfCont->ColPlaneOpacity);
      
      SO->SurfCont->ColPlaneShow_tb = XtVaCreateManagedWidget("Show", 
            xmToggleButtonGadgetClass, rc, NULL);
      XmToggleButtonSetState (SO->SurfCont->ColPlaneShow_tb, YUP, NOPE);
      XtAddCallback (SO->SurfCont->ColPlaneShow_tb, 
                  XmNvalueChangedCallback, SUMA_cb_ColPlaneShow_toggled, SO);
                  
      MCW_register_help(SO->SurfCont->ColPlaneShow_tb , SUMA_DrawROI_ColPlaneShow_help ) ;
      MCW_register_hint(SO->SurfCont->ColPlaneShow_tb , "Hides the colorplane." ) ;
      SUMA_SET_SELECT_COLOR(SO->SurfCont->ColPlaneShow_tb);
           
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
         
      /* put a push button to switch between ROIs */
      SO->SurfCont->SwitchColPlanelst = SUMA_AllocateScrolledList ("Switch Color Plane", SUMA_LSP_SINGLE, 
                                 NOPE, NOPE, /* duplicate deletion, no sorting */ 
                                 SO->SurfCont->TopLevelShell, SWP_POINTER,
                                 SUMA_cb_SelectSwitchColPlane, (void *)SO,
                                 SUMA_cb_SelectSwitchColPlane, (void *)SO,
                                 SUMA_cb_CloseSwitchColPlane, NULL);


      pb = XtVaCreateWidget ("Switch Color Plane", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, SUMA_cb_SurfCont_SwitchColPlane, (XtPointer)SO);
      MCW_register_hint(pb , "Switch between color planes." ) ;
      XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Load", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, SUMA_cb_ColPlane_Load, (XtPointer) SO);
      XtManageChild (pb);
      
      pb = XtVaCreateWidget ("Delete", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, SUMA_cb_ColPlane_Delete, (XtPointer) SO);
      XtManageChild (pb);
      
      XtManageChild (rc);
      
      
      /* manage vertical row column */
      XtManageChild (rcv);

      /* initialize the ColorPlane frame if possible */
      if (SO->N_Overlays) {
         SUMA_InitializeColPlaneShell(SO, SO->Overlays[0]);
      }
      
      XtManageChild (SO->SurfCont->ColPlane_fr);
   
   }
   
   { /* rendering mode and transparency level */
      Widget rc, label, pb;
     
      /* put a frame */
      RenderSetFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNleftAttachment , XmATTACH_FORM ,
         XmNtopAttachment  , XmATTACH_WIDGET ,
         XmNtopWidget, SurfFrame,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      /* row column  */
      rc = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, RenderSetFrame,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);

      /* rendering menu option */
      SO->SurfCont->RenderModeMenu[SW_SurfCont_Render] = SUMA_BuildMenu (rc, XmMENU_OPTION, 
                                 "RenderMode", '\0', YUP, RenderMode_Menu, 
                                 (void *)SO, SO->SurfCont->RenderModeMenu );
      XtManageChild (SO->SurfCont->RenderModeMenu[SW_SurfCont_Render]);
      
      pb = XtVaCreateWidget ("Col.Plane", 
         xmPushButtonWidgetClass, rc, 
         NULL);   
      XtAddCallback (pb, XmNactivateCallback, SUMA_cb_UnmanageWidget, (XtPointer) SO);
      XtManageChild (pb);
      
      XtManageChild (rc);
      XtManageChild (RenderSetFrame);
   }
   
   { /*s close and help buttons */
      Widget rc, pb_close, pb_bhelp;
      
      /* put up a frame to group the display controls */
      DispFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, SO->SurfCont->Mainform,
         XmNleftAttachment , XmATTACH_FORM ,
         XmNtopAttachment  , XmATTACH_WIDGET ,
         XmNtopWidget, RenderSetFrame,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 

      /* this one requires Motif 1.2 or newer */
         XtVaCreateManagedWidget ("Disp. Cont.",
            xmLabelGadgetClass, DispFrame, 
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
      XtAddCallback (pb_close, XmNactivateCallback, SUMA_cb_closeSurfaceCont, (XtPointer) SO);
      MCW_register_hint( pb_close , "Close Surface controller" ) ;
      MCW_register_help( pb_close , SUMA_closeSurfaceCont_help ) ;
      XtManageChild (pb_close); 

      pb_bhelp = XtVaCreateWidget ("BHelp", 
         xmPushButtonWidgetClass, rc, 
         NULL);
      XtAddCallback (pb_bhelp, XmNactivateCallback, MCW_click_help_CB, NULL);
      MCW_register_help(pb_bhelp , SUMA_help_help ) ;
      MCW_register_hint(pb_bhelp  , "Coddles the weak." ) ;

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   \brief creates/raises the DrawROI window
   
   \param DrawnROI (SUMA_DRAWN_ROI *) a drawn ROI that is currently being drawn. NULL if there are none
   
*/
SUMA_Boolean SUMA_OpenDrawROIWindow (SUMA_DRAWN_ROI *DrawnROI)
{
   static char FuncName[] = {"SUMA_OpenDrawROIWindow"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!DrawnROI) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Initializing with NULL.\n", FuncName);
      SUMA_SET_LABEL(SUMAg_CF->X->DrawROI->ParentLabel_lb, "Parent: -");
      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIlbl->textfield, "-");
      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIval->textfield, "0");
   }else {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Initializing with %p.\n", FuncName, DrawnROI);
      SOp = SUMA_findSOp_inDOv(DrawnROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
      sprintf (sbuf, "Parent: %s", SOp->Label);
      if (SOp) {
         SUMA_SET_LABEL(SUMAg_CF->X->DrawROI->ParentLabel_lb, sbuf);
      } else {
         SUMA_SET_LABEL(SUMAg_CF->X->DrawROI->ParentLabel_lb, "Parent: Not Found");
      }  

      SUMAg_CF->X->DrawROI->curDrawnROI = DrawnROI; /* set the currently drawnROI */

      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIlbl->textfield, DrawnROI->Label);
      
      SUMAg_CF->X->DrawROI->ROIval->value = DrawnROI->iLabel;
      sprintf(sbuf,"%d", DrawnROI->iLabel);
      SUMA_SET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIval->textfield, sbuf);
   }
   SUMA_RETURN (YUP);
}

/*!
   \brief Initializes the widgets in the DrawROI window based on the SUMA_OVERLAYS structue
*/
SUMA_Boolean SUMA_InitializeColPlaneShell(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *ColPlane)
{
   static char FuncName[] = {"SUMA_InitializeColPlaneShell"};
   char sbuf[SUMA_MAX_LABEL_LENGTH];
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_LH("Called");
   if (!SO->SurfCont->ColPlane_fr) SUMA_RETURN(YUP);
   
   if (!ColPlane) {
      SUMA_LH("Initializing with NULL");
      SUMA_SET_LABEL(SO->SurfCont->ColPlaneLabel_Parent_lb, "Label: -\nParent: -");
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneOrder->textfield, "-");
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneOpacity->textfield,"-");
   }else {
      SUMA_LH("Initializing for real");
      sprintf (sbuf, "Label: %s\nParent: %s", ColPlane->Label, SO->Label);
      SUMA_SET_LABEL(SO->SurfCont->ColPlaneLabel_Parent_lb,sbuf);
      
      SO->SurfCont->ColPlaneOrder->value = ColPlane->PlaneOrder;
      sprintf(sbuf,"%d", ColPlane->PlaneOrder);
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneOrder->textfield, sbuf);
      
      SO->SurfCont->ColPlaneOpacity->value = ColPlane->GlobalOpacity;
      sprintf(sbuf,"%.1f", ColPlane->GlobalOpacity);
      SUMA_SET_TEXT_FIELD(SO->SurfCont->ColPlaneOpacity->textfield, sbuf);
      
   }
   
   XmToggleButtonSetState (SO->SurfCont->ColPlaneShow_tb, ColPlane->Show, NOPE);

   SO->SurfCont->curColPlane = ColPlane;

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* find out which surfaces are related to SO */
   for (i=0; i<SUMAg_N_DOv; ++i) {
      if (SUMA_isSO(SUMAg_DOv[i])) {
         SOi = (SUMA_SurfaceObject *)SUMAg_DOv[i].OP;
         if (SOi != SO && SUMA_isRelated (SOi, SO)) {
            if (SOi->SurfCont) {
               if (SOi->SurfCont->ColPlane_fr && SOi->SurfCont->curColPlane == SO->SurfCont->curColPlane) {
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
      xmLabelGadgetClass, frame, 
      XmNchildType, XmFRAME_TITLE_CHILD,
      XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
      NULL);
   
   
   /* vertical row column to stack horizontal rcs in */
   rcv = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, frame,
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, SUMA_MARGIN ,
         XmNmarginWidth , SUMA_MARGIN ,
         NULL);
         
   /* row column for the parent surface name */
   rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcv,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight, SUMA_MARGIN ,
         XmNmarginWidth , SUMA_MARGIN ,
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
         XmNmarginHeight, SUMA_MARGIN ,
         XmNmarginWidth , SUMA_MARGIN ,
         NULL);

   /*put a toggle button for the DrawROI more */
   /* Turn on the ROI drawing mode, since that is what the users want to do the first time they open this window */
   SUMAg_CF->ROI_mode = YUP;
   SUMAg_CF->X->DrawROI->DrawROImode_tb = XtVaCreateManagedWidget("Draw Mode", 
      xmToggleButtonGadgetClass, rc, NULL);
   XmToggleButtonSetState (SUMAg_CF->X->DrawROI->DrawROImode_tb, SUMAg_CF->ROI_mode, NOPE);
   XtAddCallback (SUMAg_CF->X->DrawROI->DrawROImode_tb, 
                  XmNvalueChangedCallback, SUMA_cb_DrawROImode_toggled, 
                  NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->DrawROImode_tb , SUMA_DrawROI_DrawROIMode_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->DrawROImode_tb , "Toggles ROI drawing mode" ) ;

   /* set the toggle button's select color */
   SUMA_SET_SELECT_COLOR(SUMAg_CF->X->DrawROI->DrawROImode_tb);
   
   /* Put a toggle button for real time communication with AFNI */
   SUMAg_CF->X->DrawROI->AfniLink_tb = XtVaCreateManagedWidget("Afni Link", 
      xmToggleButtonGadgetClass, rc, NULL);
   /* can the link be on ? */
   if (SUMAg_CF->Connected) SUMAg_CF->ROI2afni = YUP;
   else SUMAg_CF->ROI2afni = NOPE;
   XmToggleButtonSetState (SUMAg_CF->X->DrawROI->AfniLink_tb, SUMAg_CF->ROI2afni, NOPE);
   XtAddCallback (SUMAg_CF->X->DrawROI->AfniLink_tb, 
                  XmNvalueChangedCallback, SUMA_cb_AfniLink_toggled, 
                  NULL);
   MCW_register_help(SUMAg_CF->X->DrawROI->AfniLink_tb , SUMA_DrawROI_AfniLink_help ) ;
   MCW_register_hint(SUMAg_CF->X->DrawROI->AfniLink_tb , "Toggles Link to Afni" ) ;

   /* set the toggle button's select color */
   SUMA_SET_SELECT_COLOR(SUMAg_CF->X->DrawROI->AfniLink_tb);
   
   /* manage rc */
   XtManageChild (rc);
   
   /* add a rc for the ROI label and the ROI node value */
   rc = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, rcv,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      NULL);
   
   
   SUMA_CreateTextField ( rc, "Label:",
                           6, SUMA_DrawROI_NewLabel,
                           SUMAg_CF->X->DrawROI->ROIlbl);
                             
   SUMA_CreateArrowField ( rc, "Value:",
                           1, 0, 100, 1,
                           3, SUMA_int,
                           NOPE,
                           SUMA_DrawROI_NewValue, NULL,
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
   SUMAg_CF->X->DrawROI->SwitchROIlst = SUMA_AllocateScrolledList ("Switch ROI", SUMA_LSP_SINGLE, 
                              NOPE, YUP,
                              SUMAg_CF->X->DrawROI->AppShell, SWP_TOP_LEFT,
                              SUMA_cb_SelectSwitchROI, NULL,
                              SUMA_cb_SelectSwitchROI, NULL,
                              SUMA_cb_CloseSwitchROI, NULL);
    
   pb = XtVaCreateWidget ("Switch ROI", xmPushButtonWidgetClass, rc_switch, NULL);
   XtAddCallback (pb, XmNactivateCallback, SUMA_cb_DrawROI_SwitchROI, SUMAg_CF->X->DrawROI->SwitchROIlst);
   MCW_register_help(pb , SUMA_DrawROI_SwitchROI_help ) ;
   MCW_register_hint(pb , "Switch between ROIs." ) ;
   XtManageChild (pb);
   
   SUMAg_CF->X->DrawROI->Load_pb = XtVaCreateWidget ("Load", 
      xmPushButtonWidgetClass, rc_switch, 
      NULL);
   XtAddCallback (SUMAg_CF->X->DrawROI->Load_pb, XmNactivateCallback, SUMA_cb_DrawROI_Load, NULL);
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
   MCW_register_hint( SUMAg_CF->X->DrawROI->Delete_pb , "Click twice in 5 seconds to delete ROI." ) ;
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
   MCW_register_hint(SUMAg_CF->X->DrawROI->Save_pb , "Save the Drawn ROI" ) ;
   XtManageChild (SUMAg_CF->X->DrawROI->Save_pb);

   /* Saving Mode */
   SUMAg_CF->X->DrawROI->SaveModeMenu[SW_DrawROI_SaveMode] = SUMA_BuildMenu (rc_save, XmMENU_OPTION, 
                               NULL, '\0', YUP, DrawROI_SaveMode_Menu, 
                               "Frm.",  SUMAg_CF->X->DrawROI->SaveModeMenu);
   XtManageChild (SUMAg_CF->X->DrawROI->SaveModeMenu[SW_DrawROI_SaveMode]);
      
   /* Saving what ? */
   SUMAg_CF->X->DrawROI->SaveWhatMenu[SW_DrawROI_SaveWhat] = SUMA_BuildMenu (rc_save, XmMENU_OPTION, 
                               NULL, '\0', YUP, DrawROI_SaveWhat_Menu, 
                               "What",  SUMAg_CF->X->DrawROI->SaveWhatMenu);
   XtManageChild (SUMAg_CF->X->DrawROI->SaveWhatMenu[SW_DrawROI_SaveWhat]);
      

   XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc_save, 
                              XmNorientation, XmVERTICAL,NULL);

   pb = XtVaCreateWidget ("BHelp", 
      xmPushButtonWidgetClass, rc_save, 
      NULL);
   XtAddCallback (pb, XmNactivateCallback, MCW_click_help_CB, NULL);  
   MCW_register_help(pb , SUMA_help_help ) ;
   MCW_register_hint(pb , "Coddles the weak." ) ;
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
SUMA_LIST_WIDGET * SUMA_AllocateScrolledList (char *Label, int SelectPolicy, 
                                                SUMA_Boolean RemoveDups, SUMA_Boolean ShowSorted,
                                                Widget PosRef, SUMA_WINDOW_POSITION Pos,
                                                void (*Default_cb)(Widget w, XtPointer data, XtPointer calldata), void *Default_Data,
                                                void (*Select_cb)(Widget w, XtPointer data, XtPointer calldata), void *Select_Data,
                                                void (*CloseList_cb)(Widget w, XtPointer data, XtPointer calldata), void *CloseList_Data)
{
   static char FuncName[]={"SUMA_AllocateScrolledList"};
   SUMA_LIST_WIDGET *LW = NULL;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   

   if (!Label) {
      SUMA_SLP_Err("Null Label");
      SUMA_RETURN(LW);
   }
   
   LW = (SUMA_LIST_WIDGET *) SUMA_malloc(sizeof(SUMA_LIST_WIDGET));
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
   SUMA_RETURN(LW);
   
}

/*!
   \brief Frees the SUMA_LIST_WIDGET *
*/
SUMA_LIST_WIDGET * SUMA_FreeScrolledList (SUMA_LIST_WIDGET *LW)
{
   static char FuncName[]={"SUMA_FreeScrolledList"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   if (LW->Label) SUMA_free(LW->Label);
   if (LW->ALS) SUMA_FreeAssembleListStruct(LW->ALS);
   if (LW) SUMA_free(LW);
   
   SUMA_RETURN(NULL);
   
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
   
   - If LW->toplevel = NULL then a new widget is created, otherwise only the list is updated
                           
*/
void SUMA_CreateScrolledList (    char **clist, int N_clist, SUMA_Boolean Partial,  
                                  SUMA_LIST_WIDGET *LW)
{
   static char FuncName[]={"SUMA_CreateScrolledList"};
   XmString  str, *strlist;
   char *text;
   int i = -1, iclist, u_bound, l_bound = 0;
   SUMA_Boolean New = NOPE, LocalHead=NOPE;
   
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!LW) { /* Never been created */
      SUMA_SL_Err ("Null LW!");
      SUMA_RETURNe;
   }
   
   if (N_clist <= 0) {
      SUMA_SLP_Note ("No ROIs found");
      SUMA_RETURNe;
   }
   
   if (!LW->toplevel) { /* widget has never been created or had been destroyed, create it anew */  
      /* create the widget */ 
      LW->toplevel = XtVaAppCreateShell(LW->Label , "Suma" ,
                     topLevelShellWidgetClass , SUMAg_CF->X->DPY_controller1 ,
                     NULL ) ;
      
      /* cancel the kill button's effect */
      XtVaSetValues( LW->toplevel,
           XmNdeleteResponse, XmDO_NOTHING,
           NULL);  
             
      /* handle the close button from window manager */
      if (!LW->CloseList_Data) {
         XmAddWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW) ;
      } else {
         XmAddWMProtocolCallback(/* make "Close" window menu work */
            LW->toplevel,
            XmInternAtom( SUMAg_CF->X->DPY_controller1  , "WM_DELETE_WINDOW" , False ) ,
            LW->CloseList_cb, (XtPointer)LW->CloseList_Data) ;
      }   
      

      LW->rc = XtVaCreateWidget("Tonka", xmRowColumnWidgetClass, LW->toplevel, NULL);
      LW->list = XmCreateScrolledList (LW->rc, "Tonka", NULL, 0);
      XtVaSetValues( LW->list, 
                     XmNitemCount, 0,
                     NULL);
      
      
      /* add the default selection callback */
      if (!LW->Default_Data) {
         XtAddCallback (LW->list, XmNdefaultActionCallback, LW->Default_cb, (XtPointer)LW);
      } else {
         XtAddCallback (LW->list, XmNdefaultActionCallback, LW->Default_cb, (XtPointer)LW->Default_Data);
      }        

      /* set the selection policy */
      switch (LW->SelectPolicy){
         case SUMA_LSP_SINGLE:
            XtVaSetValues( LW->list, XmNselectionPolicy, XmSINGLE_SELECT, NULL);
            if (!LW->Select_Data) 
               XtAddCallback (LW->list, XmNsingleSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNsingleSelectionCallback, LW->Select_cb, (XtPointer)LW->Select_Data); 
            break;
         case SUMA_LSP_BROWSE:
            if (!LW->Select_Data) 
               XtAddCallback (LW->list, XmNbrowseSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNbrowseSelectionCallback, LW->Select_cb, (XtPointer)LW->Select_Data); 
            
            XtVaSetValues( LW->list, XmNselectionPolicy, XmBROWSE_SELECT, NULL);
            break;
         case SUMA_LSP_MULTIPLE:
            if (!LW->Select_Data) 
               XtAddCallback (LW->list, XmNmultipleSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNmultipleSelectionCallback, LW->Select_cb, (XtPointer)LW->Select_Data); 
            
            XtVaSetValues( LW->list, XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
            break;
         case SUMA_LSP_EXTENDED:
            if (!LW->Select_Data) 
               XtAddCallback (LW->list, XmNextendedSelectionCallback, LW->Select_cb, (XtPointer)LW);
            else
               XtAddCallback (LW->list, XmNextendedSelectionCallback, LW->Select_cb, (XtPointer)LW->Select_Data); 
            
            XtVaSetValues( LW->list, XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
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

   
   /* now cycle through the elements in clist and add them, if they are new, in alphabetical order */
   if (!Partial && !LW->RemoveDups) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: New full list, deleting old entries. \n", FuncName);
      XmListDeleteAllItems(LW->list);
   }else {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Partial list, will add.\n", FuncName);
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
      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Adding %s...\n", FuncName, clist[iclist]);
      str = XmStringCreateLocalized (clist[iclist]); 

      /* positions indexes start at 1, so increment accordingly */
      if (LW->RemoveDups) { 
         if (LocalHead) fprintf (SUMA_STDERR,"%s: removing duplicates\n", FuncName);
         if (!XmListItemExists(LW->list, str)) XmListAddItemUnselected (LW->list, str, l_bound+1);
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
      /* realize the widget */
      XtRealizeWidget (LW->toplevel);
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
   \param AF (SUMA_ARROW_TEXT_FIELD *) structure defining the arrow field.                        
   - AF must be pre-allocated, of course. Its fields are initialized by the values passed to the function
*/
void SUMA_CreateArrowField ( Widget pw, char *label,
                              float value, float vmin, float vmax, float vstep,
                              int cwidth, SUMA_VARTYPE type,
                              SUMA_Boolean wrap,
                              void (*NewValueCallback)(void *data), void *cb_data,
                              SUMA_ARROW_TEXT_FIELD *AF)
{
   static char FuncName[]={"SUMA_CreateArrowField"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (label) {
      AF->label =  XtVaCreateManagedWidget (label,
         xmLabelGadgetClass, AF->rc, 
         XmNmarginTop, 0,
         XmNmarginBottom, 0,
         NULL);
   }else {
      AF->label = NULL;
   }

   AF->up = XtVaCreateManagedWidget ("arrow_up",
         xmArrowButtonGadgetClass, AF->rc,
         XmNarrowDirection,   XmARROW_UP,
         XmNmarginTop, 0,
         XmNmarginBottom, 0,
         NULL);
   XtVaSetValues (AF->up, XmNuserData, (XtPointer)AF, NULL);
   XtAddCallback (AF->up, XmNarmCallback, SUMA_ATF_start_stop, (XtPointer)1);
   XtAddCallback (AF->up, XmNdisarmCallback, SUMA_ATF_start_stop, (XtPointer)1);

   AF->down = XtVaCreateManagedWidget ("arrow_dn",
      xmArrowButtonGadgetClass, AF->rc,
      XmNarrowDirection,   XmARROW_DOWN,
      XmNmarginTop, 0,
      XmNmarginBottom, 0,
      NULL);
   XtVaSetValues (AF->down, XmNuserData, (XtPointer)AF, NULL);
   XtAddCallback (AF->down, XmNarmCallback, SUMA_ATF_start_stop, (XtPointer)-1);
   XtAddCallback (AF->down, XmNdisarmCallback, SUMA_ATF_start_stop, (XtPointer)-1);

   AF->textfield = XtVaCreateManagedWidget ("label",
      xmTextFieldWidgetClass, AF->rc,
      XmNuserData, (XtPointer)AF,
      XmNvalue, "-",
      XmNcolumns, AF->cwidth,
      XmNmarginTop, 0,
      XmNmarginBottom, 0,
      NULL);
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
                              SUMA_ARROW_TEXT_FIELD *AF)
{
   static char FuncName[]={"SUMA_ATF_cb_label_Modify"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* techincally, one should have a structure that is only for text but that is not necessary, I think */
   
   AF->up = AF->down = NULL;
   AF->step = AF->value = AF->min = AF->max = AF->wrap = 0;
   
   AF->type = SUMA_string;
   AF->NewValueCallback = NewValueCallback;
   AF->cwidth = cwidth;
   AF->modified = NOPE;

   AF->rc = XtVaCreateManagedWidget ("Container", 
      xmRowColumnWidgetClass, pw,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      NULL);

   if (label) {
      AF->label =  XtVaCreateManagedWidget (label,
         xmLabelGadgetClass, AF->rc, 
         XmNmarginHeight, 0,
         XmNmarginTop, 0,
         XmNmarginBottom, 0,
         NULL);
   }else {
      AF->label = NULL;
   }
   
   AF->textfield = XtVaCreateManagedWidget ("label",
      xmTextFieldWidgetClass, AF->rc,
      XmNuserData, (XtPointer)AF,
      XmNvalue, "0",
      XmNcolumns, AF->cwidth,
      XmNmarginTop, 0,
      XmNmarginBottom, 0,
      NULL);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* make call to NewValue callback */
   AF = (SUMA_ARROW_TEXT_FIELD *)client_data;
   
   if (AF->type == SUMA_int || AF->type == SUMA_float) SUMA_ATF_SetValue (AF);
   
   if (!AF->NewValueCallbackData) {
      AF->NewValueCallback((void*)AF);
   } else {
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
   void *n;
   XmArrowButtonCallbackStruct *cbs = 
        (XmArrowButtonCallbackStruct *) call_data;
   
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   XtVaGetValues(w, XmNuserData, &n, NULL);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   AF = (SUMA_ARROW_TEXT_FIELD *)data;
   DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
   
   if (!DrawnROI) SUMA_RETURNe;
   
   if (AF->value == DrawnROI->iLabel) SUMA_RETURNe;
   
   if (!DrawnROI->DrawStatus == SUMA_ROI_Finished) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Changing ROI value from %d to %d\n", 
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
   SUMA_Boolean Shaded, Decent, LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SO = (SUMA_SurfaceObject *)data;
   
   /* make sure a new order is in order */
   if (SO->SurfCont->curColPlane->PlaneOrder == (int)SO->SurfCont->ColPlaneOrder->value) SUMA_RETURNe;
   
   /* Now show the new order */
   SUMA_Print_PlaneOrder(SO, NULL);
   

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
   SUMA_Print_PlaneOrder(SO, NULL);
   
   /* refresh the switch list */
   SUMA_IS_SWITCH_COL_PLANE_SHADED(SO, Shaded);
   if (!Shaded) {
      SUMA_LH("Refreshing Col Plane List");
      SUMA_RefreshColorPlaneList (SO);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   \brief Function to set the color remix flag for surface SO and call a redisplay for relevant viewers 
*/
SUMA_Boolean SUMA_RemixRedisplay (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_RemixRedisplay"};
   DList *list=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_LH("Called");
   /* remix colors for all viewers displaying related surfaces */
   if (!SUMA_SetRemixFlag(SO->idcode_str, SUMAg_SVv, SUMAg_N_SVv)) {
      SUMA_SLP_Err("Failed in SUMA_SetRemixFlag.\n");
      SUMA_RETURN(NOPE);
   }

   /* redisplay */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_Suma, NULL); 
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;

   SO->SurfCont->curColPlane->Show = XmToggleButtonGetState (SO->SurfCont->ColPlaneShow_tb);
   
   SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */

   SUMA_RemixRedisplay(SO);
   
   SUMA_RETURNe;
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
   int incr;
   SUMA_ARROW_TEXT_FIELD * AF= NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   AF = (SUMA_ARROW_TEXT_FIELD *)client_data;
   
   if (!AF->wrap) {
      if (AF->value + AF->direction * AF->step> AF->max ||
        AF->value + AF->direction * AF->step< AF->min)
        SUMA_RETURNe;
   }
   
   AF->value += AF->direction * AF->step;
   
   if (AF->wrap) SUMA_WRAP_VALUE(AF->value, AF->min, AF->max);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   void *n;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
  
   XtVaGetValues (AF->textfield, XmNvalue, &n, NULL);
   /* YOU DO NOT WANT TO FREE n because n is not a copy of the string in the widget! */
   
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
   \brief Callback for Switch Col Plane button
   -Expects SO in data
*/
void SUMA_cb_SurfCont_SwitchColPlane (Widget w, XtPointer data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SurfCont_SwitchColPlane"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_SurfaceObject *SO = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_LH("Called");
   SO = (SUMA_SurfaceObject *)data;
   
   SUMA_RefreshColorPlaneList (SO);
                                                   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMAg_CF->ROI_mode = !SUMAg_CF->ROI_mode;
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMAg_CF->ROI2afni = !SUMAg_CF->ROI2afni;
   
   /* make sure that is OK */
   if (SUMAg_CF->ROI2afni && !SUMAg_CF->Connected) {
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SO = (SUMA_SurfaceObject *)data;
   LW = SO->SurfCont->SwitchColPlanelst;
   
   if (!LW) {
      SUMA_S_Err("NULL LW!");
      SUMA_RETURNe;
   }
   
   
   if (cbs->reason == XmCR_SINGLE_SELECT) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Single selection, list widget %s... \n", FuncName, LW->Label);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Default selection, list widget %s... \n", FuncName, LW->Label);
      /*double click or enter on that one, close shop after selection */
      CloseShop = YUP;
   }

   XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Selected item: %s {%s} (%d)\n", FuncName, choice, choice, cbs->item_position);
   /* because of sorting, choice cannot be used as an index into clist and oplist in ALS */
   Found = NOPE;
   ichoice = 0;
   do {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Comparing:\n%s\n%s", FuncName, LW->ALS->clist[ichoice], choice);
      if (strncmp(LW->ALS->clist[ichoice], choice, strlen(LW->ALS->clist[ichoice])) == 0) Found = YUP; 
      else ++ichoice;
   } while (ichoice < LW->ALS->N_clist && !Found);
   
   if (!Found) {
      SUMA_SLP_Err("Choice not found.");
      SUMA_RETURNe;
   }
   
   XtFree (choice);
   
   /* now retrieve that choice from the SUMA_ASSEMBLE_LIST_STRUCT structure and initialize the drawing window */
   if (LW->ALS) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: N_clist = %d\n", FuncName, LW->ALS->N_clist); 
      if (LW->ALS->N_clist > ichoice) {
         ColPlane = (SUMA_OVERLAYS *)LW->ALS->oplist[ichoice];
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Retrieved ColPlane named %s\n", FuncName, ColPlane->Name);
         SUMA_InitializeColPlaneShell(SO, ColPlane);
         SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open ColPlaneShells */

      }
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: NULL ALS\n", FuncName); 
   }

   if (CloseShop) {
      SUMA_cb_CloseSwitchColPlane( w,  (XtPointer)SO->SurfCont->SwitchColPlanelst,  call_data);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName); 
   
   LW = (SUMA_LIST_WIDGET *)data;
   
   if (!LW) {
      SUMA_S_Err("NULL LW!");
      SUMA_RETURNe;
   }
   
   
   if (cbs->reason == XmCR_SINGLE_SELECT) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Single selection, list widget %s... \n", FuncName, LW->Label);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Default selection, list widget %s... \n", FuncName, LW->Label);
      /*double click or enter on that one, close shop after selection */
      CloseShop = YUP;
   }

   XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Selected item: %s (%d)\n", FuncName, choice, cbs->item_position);
 
   /* because of sorting, choice cannot be used as an index into clist and oplist in ALS */
   Found = NOPE;
   ichoice = 0;
   do {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Comparing:\n%s\n%s", FuncName, LW->ALS->clist[ichoice], choice);
      if (strncmp(LW->ALS->clist[ichoice], choice, strlen(LW->ALS->clist[ichoice])) == 0) Found = YUP; 
      else ++ichoice;
   } while (ichoice < LW->ALS->N_clist && !Found);
   
   if (!Found) {
      SUMA_SLP_Err("Choice not found.");
      SUMA_RETURNe;
   }
   
   XtFree (choice);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
         xmLabelGadgetClass, LockFrame, 
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
         xmLabelGadgetClass, rc_m,
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
      xmToggleButtonGadgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i], 
                     XmNvalueChangedCallback, SUMA_cb_XHlock_toggled, 
                     (XtPointer)(tmpfac*i));
       
      SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+1] = XtVaCreateManagedWidget("i", 
      xmToggleButtonGadgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+1], 
                     XmNvalueChangedCallback, SUMA_cb_XHlock_toggled,  
                     (XtPointer)(tmpfac*i+1));
      
      SUMAg_CF->X->SumaCont->Lock_rbg->tb[tmpfac*i+2] = XtVaCreateManagedWidget("c", 
      xmToggleButtonGadgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->rb[i], NULL);
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
         xmToggleButtonGadgetClass, rc_m, NULL);
      XtAddCallback (SUMAg_CF->X->SumaCont->LockView_tbg[i], XmNvalueChangedCallback, SUMA_cb_XHviewlock_toggled, (XtPointer) i);
            
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
      xmLabelGadgetClass, rc_m,
      NULL);

   SUMAg_CF->X->SumaCont->Lock_rbg->arb = XtVaCreateWidget("radiobox",
      xmRowColumnWidgetClass, rc_m,
      XmNorientation , XmVERTICAL ,
      XmNpacking,      XmPACK_TIGHT,
      XmNradioBehavior, True,
      XmNnumColumns,   1,
      NULL);

   SUMAg_CF->X->SumaCont->Lock_rbg->atb[0] = XtVaCreateManagedWidget("-", 
   xmToggleButtonGadgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->arb, NULL);
   XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->atb[0], 
                  XmNvalueChangedCallback, SUMA_cb_XHalock_toggled, 
                  (XtPointer)(0));

   SUMAg_CF->X->SumaCont->Lock_rbg->atb[1] = XtVaCreateManagedWidget("i", 
   xmToggleButtonGadgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->arb, NULL);
   XtAddCallback (SUMAg_CF->X->SumaCont->Lock_rbg->atb[1], 
                  XmNvalueChangedCallback, SUMA_cb_XHalock_toggled,  
                  (XtPointer)(1));

   SUMAg_CF->X->SumaCont->Lock_rbg->atb[2] = XtVaCreateManagedWidget("c", 
   xmToggleButtonGadgetClass, SUMAg_CF->X->SumaCont->Lock_rbg->arb, NULL);
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
      xmToggleButtonGadgetClass, rc_m, NULL);
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
   MCW_register_hint( pb_new , SUMA_viewerSumaCont_help );
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
   MCW_register_hint(pb_bhelp  , "Coddles the weak." ) ;
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SUMA_cb_doneSumaCont(NULL, NULL, NULL);

   SUMA_RETURNe; 
}

void SUMA_cb_XHlock_toggled(Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_XHlock_toggled"};
   SUMA_Boolean LocalHead = NOPE;
   int cd, i, j;
   
   cd = (int) client_data;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
 
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
      
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   \brief opens a text window with information about the surface object in focus 
   -expects SO pointer in userdata
*/
void SUMA_cb_moreSurfInfo (Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_moreSurfInfo"};
   SUMA_SurfaceObject *SO=NULL;
   void *n=NULL;
   char *s = NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   XtVaGetValues (w,
                  XmNuserData, &n,
                  NULL);
                  
   SO = (SUMA_SurfaceObject *)n;
   
   /* check to see if window is already open, if it is, just raise it */
   if (SO->SurfCont->SurfInfo_TextShell) {
      XRaiseWindow (SUMAg_CF->X->DPY_controller1, XtWindow(SO->SurfCont->SurfInfo_TextShell->toplevel));
      SUMA_RETURNe;
   }
   
   /* for the string of the surface info */
   s = SUMA_SurfaceObject_Info (SO);
   
   if (s) {
      TextShell =  SUMA_CreateTestShellStruct (SUMA_SurfInfo_open, (void *)SO, 
                                               SUMA_SurfInfo_destroyed, (void *)SO);
      if (!TextShell) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateTestShellStruct.\n", FuncName);
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
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   TextShellStruct = SUMA_CreateTestShellStruct (void (*opencallback)(void *data), void *opendata, 
                                                            void (*closecallback)(void*data), void *closedata);
                                                            
   - callbacks and their data are stored in their respective fields in TextShellStruct
   - All widgets are set to NULL
   
*/

SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTestShellStruct (void (*opencallback)(void *data), void *opendata, 
                                                            void (*closecallback)(void*data), void *closedata)
{
   static char FuncName[] = {"SUMA_CreateTestShellStruct"};
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *) SUMA_malloc (sizeof(SUMA_CREATE_TEXT_SHELL_STRUCT));
   if (!TextShell) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for TextShell.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   TextShell->text_w =  TextShell->search_w = TextShell->text_output = TextShell->toplevel = NULL;
   TextShell->case_sensitive = YUP;
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
   
   \sa SUMA_CreateTestShellStruct
   
   - based on example SUMA_search_text from "Motif Programming Manual"
   see copyright notice in beginning of SUMA_display.c
*/   
SUMA_CREATE_TEXT_SHELL_STRUCT * SUMA_CreateTextShell (char *s, char *title, SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell)
{
   static char FuncName[] = {"SUMA_CreateTextShell"};
   Widget rowcol_v, rowcol_h, close_w, form, frame, toggle_case_w;
   int n;
   SUMA_Boolean LocalHead = NOPE;
   Pixel fg_pix;
   Arg args[20];
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (TextShell->OpenCallBack) { /* do the opening callback */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Calling OpenCallBack.\n", FuncName);
      TextShell->OpenCallBack(TextShell->OpenData);
   }
   
   if (!TextShell->toplevel) { /* need to create window */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Creating new text shell window.\n", FuncName);
      TextShell->toplevel = XtVaAppCreateShell (title, "Suma",
         topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);  

      XmAddWMProtocolCallback(/* make "Close" window menu work */
         TextShell->toplevel,
         XmInternAtom( SUMAg_CF->X->DPY_controller1 , "WM_DELETE_WINDOW" , False ) ,
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
        xmLabelGadgetClass, rowcol_h, NULL);

      TextShell->search_w = XtVaCreateManagedWidget ("SUMA_search_text",
        xmTextFieldWidgetClass, rowcol_h, NULL);

      XtVaGetValues (TextShell->search_w, XmNforeground, &fg_pix, NULL);
      toggle_case_w = XtVaCreateManagedWidget ("Case Sensitive",
         xmToggleButtonWidgetClass, rowcol_h,
         XmNset, TextShell->case_sensitive,
         XmNselectColor, fg_pix, 
         NULL);
      XtAddCallback (toggle_case_w, XmNvalueChangedCallback,SUMA_cb_ToggleCaseSearch, TextShell);

      close_w = XtVaCreateManagedWidget ("Close", 
         xmPushButtonWidgetClass, rowcol_h, NULL);
      XtAddCallback (close_w, XmNactivateCallback, SUMA_DestroyTextShell, TextShell);    

      XtManageChild (rowcol_h);

      TextShell->text_output = XtVaCreateManagedWidget ("text_output",
        xmTextWidgetClass, rowcol_v,
        XmNeditable,              False,
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

      TextShell->text_w = XmCreateScrolledText (form, "text_w", args, n);
      if (!s) {
         XmTextSetString (TextShell->text_w, "No Messages.\n---------------\n");
      } else {
         XmTextSetString (TextShell->text_w, s);
      }   
      XtManageChild (TextShell->text_w);

      XtAddCallback (TextShell->search_w, XmNactivateCallback, SUMA_cb_search_text, TextShell);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   static char FuncName[]={"SUMA_search_text"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   
   TextShell = (SUMA_CREATE_TEXT_SHELL_STRUCT *)client_data;
   
   /* get the text that is about to be searched */
   if (!(string = XmTextGetString (TextShell->text_w)) || !*string) {
     XmTextSetString (TextShell->text_output, "No text to search.");
     XtFree (string); /* may have been ""; free it */
     return;
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
     XtFree (search_pat); /* this may be "", XtFree() checks.. */
     return;
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
   }
   XtFree (string);
   XtFree (search_pat);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
void SUMA_cb_SetDrawROI_SaveWhat(Widget widget, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetDrawROI_SaveWhat"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   int imenu = 0;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
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
   Widget Parent_w=NULL;
   int ii;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SUMAg_N_SVv) {
      /* no graphics here, get out */
      SUMA_RETURNe;
   }
   
   /* find a decent popup message parent */
   ii=0;
   while ((SUMAg_SVv[ii].isShaded || !SUMAg_SVv[ii].X->TOPLEVEL) && (ii < SUMAg_N_SVv)) {
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
         fprintf (SUMA_STDERR, "Error %s: This should not be happening.\n", FuncName);
         SUMA_RETURNe;  
      }else Parent_w = SUMAg_SVv[ii].X->TOPLEVEL;
   }
   
   if (MD->Action ==  SMA_LogAndPopup) {
      switch (MD->Type) {
         case SMT_Notice:
            (void)MCW_popup_message(Parent_w, SUMA_FormatMessage (MD), MCW_USER_KILL | MCW_TIMER_KILL);
            break;
         case SMT_Warning:
            (void)MCW_popup_message(Parent_w, SUMA_FormatMessage (MD), MCW_USER_KILL | MCW_TIMER_KILL);
            break;
         case SMT_Error:
            (void)MCW_popup_message(Parent_w, SUMA_FormatMessage (MD), MCW_USER_KILL);
            break;
         case SMT_Critical:
            (void)MCW_popup_message(Parent_w, SUMA_FormatMessage (MD), MCW_CALLER_KILL);
            break;
         default:
            break;
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   ROIA = (SUMA_ROI_ACTION_STRUCT *) SUMA_malloc (sizeof(SUMA_ROI_ACTION_STRUCT *)); /* this structure is freed in SUMA_DestroyROIActionData */
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   ROIA = (SUMA_ROI_ACTION_STRUCT *) SUMA_malloc (sizeof(SUMA_ROI_ACTION_STRUCT *)); /* this structure is freed in SUMA_DestroyROIActionData */
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
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_RedisplayNow_AllVisible, SES_SumaWidget, NULL);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_OpenDrawnROIFileSelection);
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

void SUMA_DrawROI_NewLabel (void *data)
{
   static char FuncName[]={"SUMA_DrawROI_NewLabel"};
   SUMA_DRAWN_ROI *DrawnROI=NULL;
   SUMA_ARROW_TEXT_FIELD * AF=NULL;
   void *n=NULL;
   static int ErrCnt=0;
   SUMA_Boolean Shaded = YUP, LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   AF = (SUMA_ARROW_TEXT_FIELD *)data;
   DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
   if (!DrawnROI) {
      SUMA_LH("Null DrawnROI");
      SUMA_RETURNe;
   }
   
   XtVaGetValues (AF->textfield, XmNvalue, &n, NULL);
   /* return if not change has been made */
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
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Changing ROI label from %s to %s\n", FuncName, DrawnROI->Label, (char *)n);         
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

/*!
   \brief Positions a new widget relative to a reference widget 
   SUMA_PositionWindowRelative ( New,  Ref,  Loc);
   
   \param New (Widget) the widget to place
   \param Ref (Widget) the widget relative to which New is placed (NULL if you a reposition relative to the pointer)
   \param Loc (SUMA_WINDOW_POSITION) the position of New relative to Ref
*/
void SUMA_PositionWindowRelative (Widget New, Widget Ref, SUMA_WINDOW_POSITION Loc)
{
   static char FuncName[]={"SUMA_PositionWindowRelative"};
   Position RefX, RefY, NewX, NewY, Dx=5;
   Dimension RefW, RefH, ScrW, ScrH;
   SUMA_Boolean LocalHead=NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   ScrW = WidthOfScreen (XtScreen(New));
   ScrH = HeightOfScreen (XtScreen(New));
   if (Ref) { /* get the positions of Ref */
      XtVaGetValues (Ref,
         XmNwidth, &RefW,
         XmNheight, &RefH,
         XmNx, &RefX,
         XmNy, &RefY,
         NULL);
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
      case SWP_POINTER:
         {
            Window root, child;
            int root_x, root_y, win_x, win_y;
            unsigned int keys_buttons;
            XQueryPointer(XtDisplay(Ref), XtWindow(Ref), &root, &child, &root_x, &root_y, &win_x, &win_y, &keys_buttons);
            NewX = root_x;
            NewY = root_y;
         }
         break;
      default:
         fprintf (SUMA_STDERR, "Error %s: Option not known.\n", FuncName);
         SUMA_RETURNe;
         break;
   }

   
   if (NewX >= ScrW) NewX = 50;
   if (NewY >= ScrH) NewY = 50;
   
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
                                                         SUMA_Boolean(*VerifyFunction)(char *selection, void *data), void *VerifyData,
                                                         SUMA_PROMPT_DIALOG_STRUCT *oprmpt)
{
   static char FuncName[]={"SUMA_CreatePromptDialogStruct"};
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!oprmpt) {
      SUMA_LH ("New prompt structure");
      prmpt = (SUMA_PROMPT_DIALOG_STRUCT *)SUMA_malloc(sizeof(SUMA_PROMPT_DIALOG_STRUCT));
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
      XtVaCreateManagedWidget ("label", xmLabelGadgetClass, rc,
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
         Dimension height, h;
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   Widget dflt;
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
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
SUMA_SELECTION_DIALOG_STRUCT *SUMA_CreateFileSelectionDialogStruct (Widget daddy, SUMA_FILE_SELECT_MODE Mode, SUMA_Boolean preserve,
                                                                  void (*SelectCallback)(char *filename, void *data), void *SelectData,
                                                                  void (*CancelCallback)(void *data), void *CancelData,
                                                                  char *FilePattern,
                                                                  SUMA_SELECTION_DIALOG_STRUCT *odlg)
{
   static char FuncName[]={"SUMA_CreateFileSelectionDialogStruct"};
   SUMA_SELECTION_DIALOG_STRUCT * dlg = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!odlg) { /* new structure */
      SUMA_LH("A new structure ");    
      dlg = (SUMA_SELECTION_DIALOG_STRUCT *)SUMA_malloc(sizeof(SUMA_SELECTION_DIALOG_STRUCT));
      if (!dlg) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate for TextShell.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      dlg->dlg_w = NULL;
      dlg->FilePattern = NULL;
   }else {
      SUMA_LH("Refitting old one. ");
      if (!preserve) 
         SUMA_SLP_Warn("You should not be reusing\na dlg structure along with\nthe Preserve flag on.");
      dlg = odlg;
      if (dlg->filename) SUMA_free(dlg->filename);
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
      if (dlg->FilePattern) SUMA_free(dlg->FilePattern);
      dlg->FilePattern = SUMA_copy_string (FilePattern);
   }
   
   SUMA_RETURN(dlg);
}

/*!
   \brief, opens a file selection dialogue
   
   \param title (char *) title of window
   \param dlg (SUMA_SELECTION_DIALOG_STRUCT **) pointer to structure created and initialized by SUMA_CreateFileSelectionDialogStruct
*/                                                            
SUMA_SELECTION_DIALOG_STRUCT *SUMA_CreateFileSelectionDialog (char *title_extension, SUMA_SELECTION_DIALOG_STRUCT **dlgp)
{
   static char FuncName[]={"SUMA_CreateFileSelectionDialog"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_SELECTION_DIALOG_STRUCT *dlg = NULL;
   XmString button, title, pattern;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   dlg = *dlgp;   
   if (!dlg->dlg_w) {/* need to create it for the first time */
      SUMA_LH ("Creating new file selection window.");
      dlg->dlg_w = XmCreateFileSelectionDialog (dlg->daddy, "Files", NULL, 0);
      
      XtVaSetValues (dlg->dlg_w,
         XmNdeleteResponse, XmUNMAP,  /* system menu "Close" action */
        NULL);
        
      /* you can't cancel the kill button's effect, the way you do for toplevel shells. 
      But it does appear that the kill button is just unmanaging the widget, which is fine.
      see my modified action_area.c file
      */
      
   } else { 
      SUMA_LH ("Updating");
      /* update and raise dialogue, that is done next, for the moment, remove pre-existing callbacks*/
      XtRemoveAllCallbacks (dlg->dlg_w, XmNcancelCallback);
      XtRemoveAllCallbacks (dlg->dlg_w, XmNokCallback);
      XtRemoveAllCallbacks (dlg->dlg_w, XmNunmapCallback);
   }
      
      if (dlg->FilePattern) {
         pattern = XmStringCreateLocalized (dlg->FilePattern);
         XtVaSetValues (dlg->dlg_w,
            XmNpattern, pattern,
            NULL);
         XmStringFree (pattern);
      }
      
      XtAddCallback (dlg->dlg_w, XmNcancelCallback, SUMA_FileSelection_popdown_cb, (XtPointer)dlg);
      XtAddCallback (dlg->dlg_w, XmNokCallback, SUMA_FileSelection_file_select_cb, (XtPointer)dlg);
      XtAddCallback (dlg->dlg_w, XmNunmapCallback, SUMA_FileSelection_Unmap_cb, (XtPointer)dlgp);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
void SUMA_FileSelection_Unmap_cb (Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_FileSelection_Unmap_cb"};
   SUMA_SELECTION_DIALOG_STRUCT *dlg;
   SUMA_SELECTION_DIALOG_STRUCT **dlgp;
   SUMA_Boolean LocalHead = NOPE;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
void  SUMA_cb_UnmanageWidget(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_UnmanageWidget"};
   static int ncall=1;
   SUMA_SurfaceObject *SO = NULL;
   int xx, yy;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (ncall > 0) {
      XtUnmanageChild(SO->SurfCont->ColPlane_fr);
      /* if nothing else remains in the parent of ColPlane, then unmanage its parent (rc_right) too. */
      XtUnmanageChild(XtParent(SO->SurfCont->ColPlane_fr));
   } else {
      XtManageChild(XtParent(SO->SurfCont->ColPlane_fr));
      XtManageChild((Widget)SO->SurfCont->ColPlane_fr);
      XMapRaised (XtDisplay(SO->SurfCont->ColPlane_fr), XtWindow(SO->SurfCont->TopLevelShell));
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
    
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   Delete colorplane
   
   expects SO in data 
*/
void SUMA_cb_ColPlane_Delete(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ColPlane_Delete"};
   SUMA_LIST_WIDGET *LW=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_LH("Called");
   SUMA_RETURNe;
   
   SO = (SUMA_SurfaceObject *)data;

   /*close the list widget if open */
   if (!LW->isShaded) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Closing switch Color plane window ...\n", FuncName);
      SUMA_cb_CloseSwitchColPlane( w,  (XtPointer)SO->SurfCont->SwitchColPlanelst,  client_data);
   }  
          
   SUMA_RETURNe;
}

/*!
   \brief create a forced answer dialog for replacing ROIs
   
   \return SUMA_YES SUMA_NO SUMA_YES_ALL or SUMA_NO_ALL
*/
int SUMA_AskUser_ROI_replace(Widget parent, char *question, int default_ans)
{
    static char FuncName[]={"SUMA_AskUser_ROI_replace"};
    static Widget dialog; /* static to avoid multiple creation */
    Widget YesWid, NoWid, HelpWid;
    XmString text, yes, no;
    static int answer;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   int ud;
   Widget YesWid, NoWid, HelpWid;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   XVisualInfo match, *visualList, *vi, *visualToTry;
   int errorBase, eventBase, major, minor, found, glxcapable;
   Widget TopLevel;
   XtAppContext App;
   char *vargv[1]={ "[A] SUMA" };
   int cargc = 1;


   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   XFree(visualList);

   /* which visual will be chosen by SUMA ? (based on Step 3 in SUMA_X_SurfaceViewer_Create) */
   TopLevel = XtAppInitialize(&App, "SUMA", NULL, 0, &cargc, vargv,
                              SUMA_get_fallbackResources(), NULL, 0);
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
   XtDestroyWidget(TopLevel);
   
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (ShowHead) {
      fprintf(SUMA_STDERR, "\n");
      fprintf(SUMA_STDERR, "   visual     bf lv rg d st  r  g  b a   ax dp st accum buffs\n");
      fprintf(SUMA_STDERR, " id dep cl    sz l  ci b ro sz sz sz sz  bf th cl  r  g  b  a\n");
      fprintf(SUMA_STDERR, "-------------------------------------------------------------\n");
   }
   
   glXGetConfig(dpy, vi, GLX_USE_GL, &glxCapable);
   if (glxCapable) {
      fprintf(SUMA_STDERR, "0x%x %2d %s", vi->visualid, vi->depth, SUMA_ClassOf(vi->class));
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
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_RETURNe;
}

*/
