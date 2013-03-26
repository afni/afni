/*! File to contain functions for creating interface
for mapping data to color maps. Lots of functions from
xim.c display.c and pbar.c*/

#include "SUMA_suma.h"
#include "SUMA_plot.h"
 
/*!
   \brief Reads a ppm image and turns i into an rgba vector for ease of
   use with OpenGL. 
   Depends heavily on afni's mri_read_ppm
*/
unsigned char *SUMA_read_ppm(char *fname, int *width, int *height, int verb)
{
   static char FuncName[]={"SUMA_read_ppm"};
   char stmp[500];
   unsigned char *imar = NULL;
   byte * rgb , *cp=NULL;
   float alf = 0;
   MRI_IMAGE * im=NULL;
   int ir, ic, i1d, i1df, imx, i1d3, i1d4;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!fname) { if (verb) SUMA_SL_Err("NULL fname");  SUMA_RETURN(imar); }
   im = mri_read_ppm( fname ) ;
   if (!im) { 
      if (verb) { 
         snprintf(stmp, 500 * sizeof(char), "Failed to read %s", fname); 
         SUMA_SL_Err(stmp); }
      SUMA_RETURN(imar); 
   }
   
   rgb = MRI_RGB_PTR(im) ;
   *height = im->ny ;
   *width = im->nx ;
   imx = im->ny * im->nx;
   
   if (LocalHead) 
      fprintf (SUMA_STDERR,"%s:\nNx (width) = %d, Ny (height) = %d\n", 
                  FuncName, im->nx, im->ny);
   
   imar = (unsigned char *) 
            SUMA_malloc(sizeof(unsigned char) * im->nx * im->ny * 4);
   if (!imar) {
      SUMA_SL_Crit("Failed to allocate.");
      mri_free(im) ;
      SUMA_RETURN(imar); 
   }
   
   for (ir = 0; ir < im->ny; ++ir) {
      for (ic = 0; ic < im->nx; ++ic) {
         i1d = ic + ir * im->nx; /* equiv. 1d index into row major image data */
         i1df = ic + (im->ny - ir - 1) * im->nx; /* column flipped index */
         i1d4 = 4 * i1d; i1d3 = 3*i1df; 
         imar[i1d4] = (unsigned char)rgb[i1d3]; 
            alf  = (float)imar[i1d4];   ++i1d3; ++i1d4; 
         imar[i1d4] = (unsigned char)rgb[i1d3]; 
            alf += (float)imar[i1d4];   ++i1d3; ++i1d4; 
         imar[i1d4] = (unsigned char)rgb[i1d3]; 
            alf += (float)imar[i1d4];            ++i1d4; 
         imar[i1d4] = (unsigned char)(alf/3.0); 
      }
   } 

   mri_free(im) ; im = NULL;
   
   SUMA_RETURN(imar); 
}

void SUMA_cmap_wid_graphicsInit (Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cmap_wid_graphicsInit"};
   XVisualInfo *SUMAg_cVISINFO=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   
   XtVaGetValues(w, GLwNvisualInfo, &SUMAg_cVISINFO, NULL);
   SO->SurfCont->cmp_ren->cmap_context = 
      glXCreateContext( XtDisplay(w), SUMAg_cVISINFO,
                        0,                  /* No sharing. */
                        True);              /* Direct rendering if possible. */
   
   /* Setup OpenGL state. */
   if (!glXMakeCurrent( XtDisplay(w), XtWindow(w), 
                        SO->SurfCont->cmp_ren->cmap_context)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in glXMakeCurrent.\n \tContinuing ...\n", 
               FuncName);
      SUMA_GL_ERRS;
      SUMA_RETURNe;
   }
   
   /* call context_Init to setup colors and lighting */   
   SUMA_cmap_context_Init(SO);

   SUMA_RETURNe;
}

/*!
   Originally intended to setup for an Ortho2D mode for an image display
   But we don't like 2D. So this ends up being very much like 
   SUMA_context_Init, lookt here for reference if need be.
*/
void SUMA_cmap_context_Init(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_cmap_context_Init"};
   GLfloat mat_specular[] = { 0.0, 0.0, 0.0, 1.0};
   GLfloat mat_shininess[] = { 0 };
   GLfloat mat_ambient[] = { 0.0, 0.0, 0.0, 1.0};
   GLfloat mat_diffuse[] = { 1.0, 1.0, 1.0, 1.0 };
   GLfloat mat_emission[] = { SUMA_MAT_EMISSION_INIT  };
   GLfloat clear_color[] = { 0.0, 0.0, 0.0, 0.0};
   GLfloat light0_color[] = { 1.0, 1.0, 1.0, 1.0};
   GLfloat lmodel_ambient[] = {1.0, 1.0, 1.0, 1.0};
   GLfloat light0_position[] = {0.0, 0.0, -1.0, 0.0};
   GLfloat ViewFrom[]={0.0, 0.0, 300};
   GLfloat ViewCenter[]={0.0, 0.0, 0.0};
   GLfloat ViewCamUp[]={0.0, 1.0, 0.0};
   GLfloat CmapOrig[]={SUMA_CMAP_ORIGIN};
   GLfloat CmapTL[]={SUMA_CMAP_TOPLEFT};
   int i;
   
   SUMA_ENTRY;

   glClearColor (clear_color[0], clear_color[1], clear_color[2], clear_color[3]);
   glShadeModel (GL_SMOOTH);

   SUMA_SET_GL_RENDER_MODE(SRM_Fill); 
   
      
   /* Set the material properties*/
   glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
   glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
   glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
   glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);
   glMaterialfv(GL_FRONT, GL_EMISSION, mat_emission);
    
   /* set the directional light properties */
   glLightfv(GL_LIGHT0, GL_POSITION, light0_position);
   glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_color);
   glLightfv(GL_LIGHT0, GL_SPECULAR, light0_color);

   /* set the ambient light */
   glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);
 
   glEnable(GL_LIGHTING); /* prepare GL to perform lighting calculations */
   glEnable(GL_LIGHT0); /*Turn lights ON */
   glEnable(GL_DEPTH_TEST);
   
   /*setup the view point and then setup the lights. Those lights will remain in place regardless of the rotations/translations
   done on the surface */
   for (i=0; i<2; ++i) { ViewCenter[i] = (CmapTL[i] - CmapOrig[i]) / 2.0;  }
   ViewFrom[0] = ViewCenter[0]; ViewFrom[1] = ViewCenter[1]; ViewFrom[2] = SUMA_CMAP_VIEW_FROM;
    
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   gluLookAt ( ViewFrom[0], ViewFrom[1], ViewFrom[2],  
               ViewCenter[0], ViewCenter[1], ViewCenter[2],
               ViewCamUp[0], ViewCamUp[1], ViewCamUp[2] );
   
   
   SUMA_RETURNe;
   
}

void SUMA_DrawCmap(SUMA_COLOR_MAP *Cmap)
{
   static char FuncName[]={"SUMA_DrawCmap"};
   float orig[3]={ SUMA_CMAP_ORIGIN };
   float topright[3] = { SUMA_CMAP_TOPLEFT };
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");

   if (!Cmap->SO) {
      SUMA_LH("Creating Cmap's SO");
      Cmap->SO = SUMA_Cmap_To_SO(Cmap, orig, topright, 0);
      if (!Cmap->SO) { SUMA_SL_Err("Failed to create SO"); }
   }
   
   /* initialize the context to be safe; 
   sometimes there is conflict with the viewer's context 
   and that causes the colormaps to be absent...   ZSS Nov. 28 06
   But that is too radical and kills translation toys ZSS Mar. 06 08*/
   /* Turned off, may no longer cause trouble...  ZSS Mar. 07 08*/
   /* SUMA_cmap_context_Init(Cmap->SO); */
 
   /* This allows each node to follow the color specified when it was drawn */ 
   glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE); 
   glEnable(GL_COLOR_MATERIAL);

   /*Now setup various pointers*/
   glEnableClientState (GL_COLOR_ARRAY);
   glEnableClientState (GL_VERTEX_ARRAY);
   glEnableClientState (GL_NORMAL_ARRAY);
   glColorPointer (4, GL_FLOAT, 0, Cmap->SO->PermCol);
   glVertexPointer (3, GL_FLOAT, 0, Cmap->SO->glar_NodeList);
   glNormalPointer (GL_FLOAT, 0, Cmap->SO->glar_NodeNormList);

   SUMA_SET_GL_RENDER_MODE(SRM_Fill); 
   glDrawElements (GL_TRIANGLES, (GLsizei)Cmap->SO->N_FaceSet*3, 
                   GL_UNSIGNED_INT, Cmap->SO->glar_FaceSetList);
   
   /* Here you could draw a contour around the color cells. 
   But you'll need to raise the contour over the filled polygons 
   because they'll get
   covered otherwise */ 
   #if 0
   { 
      GLfloat *LineCol=NULL;   
      LineCol = (GLfloat *)SUMA_calloc(Cmap->SO->N_Node*4, sizeof(GLfloat));
      for (i=0; i<Cmap->SO->N_Node; ++i) { 
         LineCol[4*i] = LineCol[4*i+1] = LineCol[4*i+2] = 0.1; 
         LineCol[4*i+3] = 1.0; }
      glColorPointer (4, GL_FLOAT, 0, LineCol);
      SUMA_SET_GL_RENDER_MODE(SRM_Line); 
      glDrawElements (  GL_TRIANGLES, (GLsizei)Cmap->SO->N_FaceSet*3, 
                        GL_UNSIGNED_INT, Cmap->SO->glar_FaceSetList);
      SUMA_free(LineCol); LineCol = NULL;
   }
   #endif
   
   SUMA_RETURNe;
}

void SUMA_cmap_wid_display(SUMA_SurfaceObject *SO)
{   
   static char FuncName[]={"SUMA_cmap_wid_display"};
   int i;
   GLfloat rotationMatrix[4][4];
   float currentQuat[]={0.0, 0.0, 0.0, 1.0};
   GLfloat clear_color[] = { 0.8, 0.8, 0.8, 0.0};
   GLfloat RotaCenter[]={0.0, 0.0, 0.0};
   SUMA_COLOR_MAP *Cmap = NULL;
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
    
   SUMA_ENTRY;
   
   SUMA_LH("in, lots of inefficiencies here, make sure you revisit");
   
   /* is surface controller closed? */
   if (!SO->SurfCont->Open) {
      SUMA_LHv("SurfCont closed for %s, trying to open it\n", SO->Label);
      if (!SUMA_viewSurfaceCont(NULL, SO, SUMA_BestViewerForSO(SO))) {
         SUMA_S_Warn("No SurfCont");
         SUMA_DUMP_TRACE("No SurfCont");
         SUMA_RETURNe;
      }
   }
   /* now you need to set the clear_color since it can be 
      changed per viewer Thu Dec 12 2002 */
   glClearColor (clear_color[0], clear_color[1],clear_color[2],clear_color[3]);
      
   if (LocalHead) 
      fprintf (SUMA_STDOUT,"%s: Building Rotation matrix ...\n", FuncName);
   SUMA_build_rotmatrix(rotationMatrix, currentQuat);
    
   if (LocalHead) 
      fprintf (SUMA_STDOUT,"%s: performing glClear ...\n", FuncName);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); /* clear the Color Buffer                                                          and the depth buffer */
   
   /* careful here, you might want to turn 
      the next block into a macro like SUMA_SET_GL_PROJECTION */
   if (LocalHead) 
      fprintf (SUMA_STDOUT,
               "%s: Setting up matrix mode and perspective ...\nFOV=%f\n"
               "Translation is %f %f %f\n", 
               FuncName, SUMA_CMAP_FOV_INITIAL,
               SO->SurfCont->cmp_ren->translateVec[0],
               SO->SurfCont->cmp_ren->translateVec[1], 
               SO->SurfCont->cmp_ren->translateVec[2] );
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluPerspective(SO->SurfCont->cmp_ren->FOV, 
                  (double)SUMA_CMAP_WIDTH/SUMA_CMAP_HEIGHT, 
                  SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); 
                  /*lower angle is larger zoom,*/

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glTranslatef ( SO->SurfCont->cmp_ren->translateVec[0],
                  SO->SurfCont->cmp_ren->translateVec[1], 
                  SO->SurfCont->cmp_ren->translateVec[2] );
   if (0){
   SUMA_SL_Note("no need for shananigans\n"
                  "But to illustrate ...\n");
   glTranslatef (RotaCenter[0], RotaCenter[1], RotaCenter[2]);
   glMultMatrixf(&rotationMatrix[0][0]);
   glTranslatef (-RotaCenter[0], -RotaCenter[1], -RotaCenter[2]);
   }
   
   
   /* find out what colormap is to be displayed */
   if (SO->SurfCont->curColPlane) {
      /* what's the Cmap for that plane ? */
      Cmap = SUMA_CmapOfPlane (SO->SurfCont->curColPlane );
      if (Cmap) SUMA_DrawCmap(Cmap); /* create the colormap */
   } else {
      SUMA_SL_Err("NULL SO->SurfCont->curColPlane");
   }   
   glPopMatrix();   

   if (LocalHead) 
      fprintf (SUMA_STDERR,
               "%s: Flushing or swapping ...\n"
               "cmp_ren %p, cmap_wid %p\n", 
               FuncName,
               SO->SurfCont ? SO->SurfCont->cmp_ren : NULL, 
               (SO->SurfCont && SO->SurfCont->cmp_ren ) ? 
                  SO->SurfCont->cmp_ren->cmap_wid : NULL );
   
   if (SUMAg_SVv[0].X->DOUBLEBUFFER)
      glXSwapBuffers(XtDisplay(SO->SurfCont->cmp_ren->cmap_wid), 
                     XtWindow(SO->SurfCont->cmp_ren->cmap_wid));
   else  
      glFlush();

   /* Avoid indirect rendering latency from queuing. */
   
   if (!glXIsDirect( XtDisplay(SO->SurfCont->cmp_ren->cmap_wid), 
                     SO->SurfCont->cmp_ren->cmap_context)) {
      SUMA_LH("GLfinish time");
      glFinish();
   } else {
      SUMA_LH("No finish call");
   }

   SUMA_RETURNe;
}

Boolean SUMA_cmap_wid_handleRedisplay(XtPointer clientData)
{
   static char FuncName[]={"SUMA_cmap_wid_handleRedisplay"};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
  
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN(NOPE); }
   
   if (SO->SurfCont->Open) { /* Otherwise it causes a crash on linux 
                                 when yoking L/R stuff */
      SUMA_LHv("Making cmap_wid current %p %p\n", 
               SO->SurfCont->cmp_ren->cmap_wid, 
               SO->SurfCont->cmp_ren->cmap_context);
      if (!glXMakeCurrent( XtDisplay(SO->SurfCont->cmp_ren->cmap_wid), 
                           XtWindow(SO->SurfCont->cmp_ren->cmap_wid), 
                           SO->SurfCont->cmp_ren->cmap_context)) {
         SUMA_GL_ERRS;
         SUMA_S_Err("Failed in glXMakeCurrent.\n \tContinuing ...");
      }
      SUMA_LH("Calling wid display");
      SUMA_cmap_wid_display(SO);
      glFinish();

      /* insist on a glXMakeCurrent for surface viewer */
      SUMA_LH("Making sv's GLXAREA current\n");
      SUMA_SiSi_I_Insist();
   }
   
   SUMA_RETURN(YUP);
}

void SUMA_cmap_wid_postRedisplay(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cmap_wid_postRedisplay"};
   static XtPointer elvis;
   int isv;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("cold");
   
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }

   SUMA_register_workproc( SUMA_cmap_wid_handleRedisplay , (XtPointer)SO );
   
   SUMA_RETURNe;
}

void SUMA_cmap_wid_expose(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cmap_wid_expose"};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   
   SUMA_cmap_wid_postRedisplay(w, (XtPointer)SO, NULL);

   SUMA_RETURNe;
}

void SUMA_cmap_wid_resize(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cmap_wid_resize"};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   
   SUMA_RETURNe;
}

void SUMA_cmap_wid_input(Widget w, XtPointer clientData, XtPointer callData)
{
   static char FuncName[]={"SUMA_cmap_wid_input"};
   GLwDrawingAreaCallbackStruct *cd;
   KeySym keysym;
   XKeyEvent Kev;
   XButtonEvent Bev;
   XMotionEvent Mev;
   char buffer[10], cbuf = '\0', cbuf2='\0';
   SUMA_SurfaceObject *SO=NULL;
   int xls;
   static Time B1time = 0;
   static int pButton, mButton, rButton;
   static SUMA_Boolean DoubleClick = NOPE;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL; 
   static float height_two_col, width;
   int ncol;
   SUMA_COLOR_MAP *ColMap = NULL;
   static float fov_lim = -1.0;
   static SUMA_SurfaceObject *SOcmap=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   SUMA_LH("called");
   SO = (SUMA_SurfaceObject *)clientData;             
      /* THIS SO is for the main surface, NOT THE colormap's */
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   
   ColMap = SUMA_CmapOfPlane (SO->SurfCont->curColPlane );
   if (!ColMap) { SUMA_SL_Err("No Cmap"); SUMA_RETURNe; };
   if (ColMap->SO != SOcmap) {
      SUMA_LH("New colormap SO");
      /* calculate FOV limit for zooming in */
      SOcmap = ColMap->SO;
      ncol = SOcmap->N_FaceSet / 2;
      height_two_col =  (SOcmap->MaxDims[1] - SOcmap->MinDims[1]) / 
                        (float)ncol * 2.0; 
                        /* no need to show more than 2 cols */
      width = (SOcmap->MaxDims[0] - SOcmap->MinDims[0]);
      fov_lim = 2.0 * atan( (double)height_two_col / 
               ( 2.0 * (double)SUMA_CMAP_VIEW_FROM ) ) * 180 / SUMA_PI; 
      if (LocalHead) {
         SUMA_Print_Surface_Object(SOcmap, NULL);
         fprintf( SUMA_STDERR,
                  "%s: ncol=%d, height = %f, height of 2 col =%f\n"
                  ", width=%f, d = %d, fov_lim = %f\n", 
                  FuncName, ncol, (SOcmap->MaxDims[1] - SOcmap->MinDims[1]), 
                  height_two_col,  width, SUMA_CMAP_VIEW_FROM, fov_lim);
      }
   }  

   /* make sure the color map is the current context */
   if (!glXMakeCurrent( XtDisplay(w), 
                        XtWindow(w), 
                        SO->SurfCont->cmp_ren->cmap_context)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in glXMakeCurrent.\n ", FuncName);
      SUMA_RETURNe;
   }

   /* get the callData pointer */
   cd = (GLwDrawingAreaCallbackStruct *) callData;

   Kev = *(XKeyEvent *) &cd->event->xkey; /* RickR's suggestion to comply with 
                                             ANSI C, no type casting of
                                             structures July 04*/
   Bev = *(XButtonEvent *) &cd->event->xbutton;
   Mev = *(XMotionEvent *) &cd->event->xmotion;
   
   switch (Kev.type) { /* switch event type */
   case KeyPress:
      xls = XLookupString((XKeyEvent *) cd->event, buffer, 8, &keysym, NULL);
      
      /* XK_* are found in keysymdef.h */ 
      switch (keysym) { /* keysym */
         case XK_h:
            if (Kev.state & ControlMask){
              if (!list) list = SUMA_CreateList();
              ED = SUMA_InitializeEngineListData (SE_Help_Cmap);
              SUMA_RegisterEngineListCommand ( list, ED,
                                         SEF_vp, (void *)ColMap,
                                         SES_SumaWidget, NULL, NOPE,
                                         SEI_Head, NULL); 
              if (!SUMA_Engine (&list)) {
                  fprintf(stderr, 
                           "Error %s: SUMA_Engine call failed.\n", FuncName);
              }    
            }
            break;
         case XK_f:
            {
               if (1) { /* needs work, don't feel like it for now */
                  SUMA_LH("Flipping colormap");
                  SUMA_Flip_Color_Map(
                     SUMA_CmapOfPlane(SO->SurfCont->curColPlane));
                  SUMA_LH("Switching colormap");
                  SUMA_SwitchColPlaneCmap(SO,
                     SUMA_CmapOfPlane(SO->SurfCont->curColPlane));
                  #if SUMA_SEPARATE_SURF_CONTROLLERS
                     SUMA_LH("Updating shells");
                     SUMA_UpdateColPlaneShellAsNeeded(SO);
                  #endif

                  /* update Lbl fields */
                  SUMA_LH("Updating NodeLblFields");
                  SUMA_UpdateNodeLblField(SO);
               }
            }
            break;
         case XK_r:
            {
               GLvoid *pixels;
               SUMA_LH("Recording");
               if (SUMAg_SVv[0].X->DOUBLEBUFFER)
                  glXSwapBuffers(XtDisplay(SO->SurfCont->cmp_ren->cmap_wid), 
                                 XtWindow(SO->SurfCont->cmp_ren->cmap_wid));
               pixels = SUMA_grabPixels(1, SUMA_CMAP_WIDTH, SUMA_CMAP_HEIGHT);
               if (SUMAg_SVv[0].X->DOUBLEBUFFER)
                  glXSwapBuffers(XtDisplay(SO->SurfCont->cmp_ren->cmap_wid), 
                                 XtWindow(SO->SurfCont->cmp_ren->cmap_wid));
               if (pixels) {
                 ISQ_snapsave (SUMA_CMAP_WIDTH, -SUMA_CMAP_HEIGHT, 
                              (unsigned char *)pixels,
                              SO->SurfCont->cmp_ren->cmap_wid ); 
                 SUMA_free(pixels);
               }else {
                  SUMA_SLP_Err("Failed to record image.");
               }
            }
            break;
         case XK_w:
            {
               char *sss=NULL;
               SUMA_COLOR_MAP * ColMap = 
                     SUMA_CmapOfPlane(SO->SurfCont->curColPlane);
               if (LocalHead) {
                  if ((sss = SUMA_ColorMapVec_Info(&ColMap, 1, 1))) {
                     SUMA_LHv("To be written:\n%s\n", sss);
                     SUMA_free(sss); sss = NULL;
                  }
               }
               if (!SUMA_Write_Color_Map_1D(ColMap, NULL)) {
                  SUMA_S_Errv("Failed to write colmap %s\n", 
                           SO->SurfCont->curColPlane->Name);
               }else {
                  SUMA_S_Notev("Wrote colormap %s to file.\n",
                           SO->SurfCont->curColPlane->Name);
               }
            }
            break;
         case XK_Z:
            {
               static SUMA_Boolean BeepedAlready = NOPE;
               SO->SurfCont->cmp_ren->FOV /= FOV_IN_FACT; 
               if (SO->SurfCont->cmp_ren->FOV < fov_lim) { 
                  if (!BeepedAlready) {
                     SUMA_BEEP; BeepedAlready = YUP;
                  }
                  SO->SurfCont->cmp_ren->FOV = fov_lim; 
               } else BeepedAlready = NOPE;
               if (LocalHead) 
                  fprintf(SUMA_STDERR,
                           "%s: Zoom in FOV = %f\n", 
                           FuncName, SO->SurfCont->cmp_ren->FOV);
               SUMA_cmap_wid_postRedisplay(w, (XtPointer)SO, NULL);
            }
            break;

         case XK_z:
            {
               static SUMA_Boolean BeepedAlready = NOPE;
               SO->SurfCont->cmp_ren->FOV /= FOV_OUT_FACT; 
               if (SO->SurfCont->cmp_ren->FOV > SUMA_CMAP_FOV_INITIAL) { 
                  if (!BeepedAlready) {
                     SUMA_BEEP; BeepedAlready = YUP;
                  }
                  SO->SurfCont->cmp_ren->FOV = SUMA_CMAP_FOV_INITIAL; 
               } else BeepedAlready = NOPE;
               if (LocalHead) 
                  fprintf( SUMA_STDERR,
                           "%s: Zoom out FOV = %f\n", 
                           FuncName, SO->SurfCont->cmp_ren->FOV);
               SUMA_cmap_wid_postRedisplay(w, (XtPointer)SO, NULL);
            }
            break;
         case XK_Home:   
            SO->SurfCont->cmp_ren->FOV = SUMA_CMAP_FOV_INITIAL;
            SO->SurfCont->cmp_ren->translateVec[0] = 
            SO->SurfCont->cmp_ren->translateVec[1] = 
            SO->SurfCont->cmp_ren->translateVec[2] = 0.0;
            {
               SUMA_COLOR_MAP *CM = SUMA_CmapOfPlane(SO->SurfCont->curColPlane);
               if (SUMA_Rotate_Color_Map(CM, 0) % CM->N_M[0]) { 
                  SUMA_LH("Got color map modification to do");
                  SUMA_SwitchColPlaneCmap(SO,
                     SUMA_CmapOfPlane(SO->SurfCont->curColPlane));
                  #if SUMA_SEPARATE_SURF_CONTROLLERS
                     SUMA_UpdateColPlaneShellAsNeeded(SO);
                  #endif

                  /* update Lbl fields */
                  SUMA_UpdateNodeLblField(SO);
               } else {
                  SUMA_cmap_wid_postRedisplay(w, (XtPointer)SO, NULL);
               }
            }
            break;
         case XK_Up:   /*KEY_UP:*/
            {
               if (Kev.state & ShiftMask){
                  static SUMA_Boolean BeepedAlready = NOPE;   
                  float tstep = height_two_col / 2.0 * 
                                 SO->SurfCont->cmp_ren->FOV /
                                 (float)SUMA_CMAP_FOV_INITIAL; 
                  SO->SurfCont->cmp_ren->translateVec[1] += tstep ;
                  if (LocalHead) 
                     fprintf(SUMA_STDERR,
                              "%s: translateVec[1] = %f (%d)\n", 
                              FuncName, 
                              SO->SurfCont->cmp_ren->translateVec[1],
                              SUMA_CMAP_HEIGHT - 20);
                  if (  SO->SurfCont->cmp_ren->translateVec[1] >  
                        SUMA_CMAP_HEIGHT - 20) {
                     if (!BeepedAlready) {
                        SUMA_BEEP; BeepedAlready = YUP;
                     }
                        SO->SurfCont->cmp_ren->translateVec[1] -= tstep; 
                  } else BeepedAlready = NOPE;
                  SUMA_cmap_wid_postRedisplay(w, (XtPointer)SO, NULL);
               } else { 
                  float frac = 0.0;
                  if (Kev.state & ControlMask) {
                     frac = 1;
                  }else {
                     frac = SUMAg_CF->CmapRotaFrac;
                  }
                  SUMA_LH("Rotating colormap");
                  SUMA_Rotate_Color_Map(
                     SUMA_CmapOfPlane(SO->SurfCont->curColPlane), frac);
                  SUMA_LH("Switching colormap");
                  SUMA_SwitchColPlaneCmap(SO,
                     SUMA_CmapOfPlane(SO->SurfCont->curColPlane));
                  #if SUMA_SEPARATE_SURF_CONTROLLERS
                     SUMA_LH("Updating shells");
                     SUMA_UpdateColPlaneShellAsNeeded(SO);
                  #endif

                  /* update Lbl fields */
                  SUMA_LH("Updating NodeLblFields");
                  SUMA_UpdateNodeLblField(SO);
               }
            }
            break;
         case XK_Down:   /*KEY_DOWN:*/
            {
               if (Kev.state & ShiftMask){
                  static SUMA_Boolean BeepedAlready = NOPE;  
                  float tstep =  height_two_col / 2.0 * 
                                 SO->SurfCont->cmp_ren->FOV / 
                                 (float)SUMA_CMAP_FOV_INITIAL; 
                  SO->SurfCont->cmp_ren->translateVec[1] -=  tstep;
                  if (  SO->SurfCont->cmp_ren->translateVec[1] <  
                        -SUMA_CMAP_HEIGHT + 20) {
                     if (!BeepedAlready) {
                        SUMA_BEEP; BeepedAlready = YUP;
                     }
                        SO->SurfCont->cmp_ren->translateVec[1] += tstep; 
                  } else BeepedAlready = NOPE;
                  SUMA_cmap_wid_postRedisplay(w, (XtPointer)SO, NULL);
               } else {
                  float frac = 0.0;
                  if (Kev.state & ControlMask) {
                     frac = 1;
                  }else {
                     frac = SUMAg_CF->CmapRotaFrac;
                  }
                  SUMA_Rotate_Color_Map(
                     SUMA_CmapOfPlane(SO->SurfCont->curColPlane), -frac);
                  SUMA_SwitchColPlaneCmap(SO,
                     SUMA_CmapOfPlane(SO->SurfCont->curColPlane));
                  #if SUMA_SEPARATE_SURF_CONTROLLERS
                     SUMA_UpdateColPlaneShellAsNeeded(SO);
                  #endif

                  /* update Lbl fields */
                  SUMA_UpdateNodeLblField(SO);
               }
            }
            break;
         
      } /* keysym */
   break;
   
   case ButtonPress:
      if (LocalHead) fprintf(stdout,"In ButtonPress\n");      
      pButton = Bev.button;
      if (SUMAg_CF->SwapButtons_1_3 || 
          (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
         if (pButton == Button1) pButton = Button3;
         else if (pButton == Button3) pButton = Button1;
      }
     
     /* trap for double click */
      if (Bev.time - B1time < SUMA_DOUBLE_CLICK_MAX_DELAY) {
         if (LocalHead) fprintf(SUMA_STDERR, "%s: Double click.\n", FuncName);
         DoubleClick = YUP;
      } else {
         DoubleClick = NOPE;
      }
      B1time = Bev.time; 
            
      switch (pButton) { /* switch type of button Press */
         case Button1:
            break;
         default:
            break;
      } /* switch type of button Press */
      break;
      
   case ButtonRelease:
      if (LocalHead) fprintf(SUMA_STDERR,"%s: In ButtonRelease\n", FuncName); 
      rButton = Bev.button;
      if (SUMAg_CF->SwapButtons_1_3 || 
          (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode) ) {
         if (rButton == Button1) rButton = Button3;
         else if (rButton == Button3) rButton = Button1;
      }
      switch (rButton) { /* switch type of button Press */
         case Button3:
            break;
         default:
            break;
      } /* switch type of button Press */
      break;
      
   case MotionNotify:
      if (LocalHead) fprintf(stdout,"In MotionNotify\n"); 
      if (SUMAg_CF->SwapButtons_1_3 || 
          (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
        if (((Mev.state & Button3MotionMask) && (Mev.state & Button2MotionMask)) 
         || ((Mev.state & Button2MotionMask) && (Mev.state & ShiftMask))) {
            mButton = SUMA_Button_12_Motion;
         } else if(Mev.state & Button3MotionMask) {
            mButton = SUMA_Button_1_Motion;
         }else if(Mev.state & Button2MotionMask) { 
            mButton = SUMA_Button_2_Motion;
         }else if(Mev.state & Button1MotionMask) { 
            mButton = SUMA_Button_3_Motion;
         }else {
            break;
         } 
      } else {
         if (((Mev.state & Button1MotionMask) && (Mev.state & Button2MotionMask)) || ((Mev.state & Button2MotionMask) && (Mev.state & ShiftMask))) {
            mButton = SUMA_Button_12_Motion;
         } else if(Mev.state & Button1MotionMask) {
            mButton = SUMA_Button_1_Motion;
         }else if(Mev.state & Button2MotionMask) { 
            mButton = SUMA_Button_2_Motion;
         } else if(Mev.state & Button3MotionMask) { 
            mButton = SUMA_Button_3_Motion;
         }else {
            break;
         }
      }
      
      switch (mButton) {
         case SUMA_Button_12_Motion:
         case SUMA_Button_2_Shift_Motion:
            break;
         default:
            break;
      }
      
      
      break;
  }/* switch event type */

  /* set up flag to make sure sv regains context */
  SUMA_SiSi_I_Insist();
   
  SUMA_RETURNe;
}

int SUMA_set_threshold_label(SUMA_SurfaceObject *SO, float val)
{
   static char FuncName[]={"SUMA_set_threshold_label"};
   char slabel[100];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN(0); }
   
   
   if (SO->SurfCont->curColPlane->OptScl->ThrMode != SUMA_ABS_LESS_THAN) 
      sprintf(slabel, "%5s", MV_format_fval(val)); 
   else {
      /* used to use this:
      sprintf(slabel, "|%5s|", .... 
      but that does not work in the editable field ... */
      sprintf(slabel, "%5s", MV_format_fval(val)); 
   }
   /* SUMA_SET_LABEL(SO->SurfCont->thr_lb,  slabel);*/
      SUMA_INSERT_CELL_STRING(SO->SurfCont->SetThrScaleTable, 0,0,slabel); 

   
   /* You must use the line below if you are calling this function on the fly */
   /* SUMA_FORCE_SCALE_HEIGHT(SO);  */
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   SUMA_UpdatePvalueField (SO, val); 
   
   SUMA_RETURN(1);  
}

void SUMA_cb_set_threshold_label(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_threshold_label"};
   SUMA_SurfaceObject *SO=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff ;
   int dec=0;
   char slabel[100];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   
   SUMA_set_threshold_label(SO, fff);
   
   SUMA_RETURNe;
}

int SUMA_set_threshold(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *colp,
                           float *val)
{
   static char FuncName[]={"SUMA_set_threshold"};
   SUMA_SurfaceObject *SOC=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_set_threshold_one(SO, colp, val)) SUMA_RETURN(0);
   if (!colp) colp = SO->SurfCont->curColPlane;
   if (!colp) SUMA_RETURN(0);
   
    /* do we have a contralateral SO and overlay? */
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      if (!SUMA_SetScaleThr_one(SOC, colpC, val, 1, 1)) {
         SUMA_S_Warn("Failed in contralateral");
         SUMA_RETURN(0);
      }
   } 
   SUMA_RETURN(1);  
}

int SUMA_set_threshold_one(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *colp,
                           float *val)
{
   static char FuncName[]={"SUMA_set_threshold_one"};
   float oval;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) SUMA_RETURN(0);
   if (!colp) colp = SO->SurfCont->curColPlane;
   if (!colp) SUMA_RETURN(0);
   
   oval = colp->OptScl->ThreshRange[0];
   colp->OptScl->ThreshRange[0] = *val;
 
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s:\nThreshold set to %f\n",
               FuncName, 
               colp->OptScl->ThreshRange[0]); 
   }
   
   if (  colp->OptScl->UseThr &&
         colp->OptScl->tind >=0) {   
      if (oval != colp->OptScl->ThreshRange[0] &&
          colp->OptScl->Clusterize) {
         /* Need a new clusterizing effort*/
         colp->OptScl->RecomputeClust = 1;
      }
      SUMA_ColorizePlane(colp);
      SUMA_RemixRedisplay(SO);
   }

   /* call this one since it is not being called as the slider is dragged. */
   SUMA_set_threshold_label(SO, *val);   

   /* sad as it is */
   SUMA_FORCE_SCALE_HEIGHT(SO); 

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   SUMA_UpdateNodeValField(SO);
   SUMA_UpdateNodeLblField(SO);
   SUMA_UpdatePvalueField (SO,
                           colp->OptScl->ThreshRange[0]);  
 
   SUMA_RETURN(1);
}

void SUMA_cb_set_threshold(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_threshold"};
   SUMA_SurfaceObject *SO=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff=0.0;
   int dec=-1;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   SUMA_LHv("Have %f\n", fff);
   SUMA_set_threshold(SO, NULL, &fff);
   
   SUMA_RETURNe;
}

int SUMA_SwitchColPlaneIntensity(
         SUMA_SurfaceObject *SO, 
         SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneIntensity"};
   char srange[500];
   double range[2];
   int loc[2];
   SUMA_DSET *dset=NULL;
   SUMA_SurfaceObject *SOC=NULL; 
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_SwitchColPlaneIntensity_one(SO, colp, ind, setmen)) {
      SUMA_S_Err("Failed in _one");
      SUMA_RETURN(0);
   }

   /* do we have a contralateral SO and overlay? */
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      if (!SUMA_SwitchColPlaneIntensity_one(SOC, colpC, ind, 1)) {
         SUMA_S_Warn("Failed in contralateral");
      }
   }
   
   SUMA_RETURN(1);
}
  
/* changes you do here should be reflected under SE_SetSurfCont in SUMA_Engine*/
int SUMA_SwitchColPlaneIntensity_one (
         SUMA_SurfaceObject *SO, 
         SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneIntensity_one"};
   char srange[500];
   double range[2];
   int loc[2];
   SUMA_DSET *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (  !SO || !SO->SurfCont || 
         !SO->SurfCont->curColPlane || 
         !colp || ind < 0 || !colp->dset_link) { SUMA_RETURN(0); }
   if (LocalHead) {
      fprintf( SUMA_STDERR, 
               "%s:\n request to switch intensity to col. %d\n", 
               FuncName, ind);
      fprintf(SUMA_STDERR, "SO->Label = %s\n", SO->Label);
   }
   
   if (SDSET_TYPE(colp->dset_link) == SUMA_NODE_RGB) {
      SUMA_S_Err("This is a NODE_RGB dataset, cannot switch columns.\n");
      SUMA_RETURN(0);
   }
   if (ind >= SDSET_VECNUM(colp->dset_link)) {
      SUMA_S_Errv("Col. Index of %d exceeds maximum of %d for this dset.\n", 
                   ind, SDSET_VECNUM(colp->dset_link)-1);
      SUMA_RETURN(0);
   }
   if (ind != colp->OptScl->find &&
          colp->OptScl->Clusterize) {
         /* Need a new clusterizing effort*/
         colp->OptScl->RecomputeClust = 1;
   }
   colp->OptScl->find = ind;
   if (setmen && colp == SO->SurfCont->curColPlane ) {
      SUMA_LHv("Setting menu values, %d\n", colp->OptScl->find+1);
      SUMA_Set_Menu_Widget(SO->SurfCont->SwitchIntMenu, colp->OptScl->find+1);
   }
   
   
   dset=colp->dset_link;
   switch(SO->SurfCont->curColPlane->LinkMode) {/* corresponding threshold sb */
      case SW_LinkMode_Stat:
            {
         char *lab=SUMA_DsetColLabelCopy(dset, ind, 0), *lab2=NULL, *ext=NULL;
         int ind2=-1, ipair=0;
         static char *exta[50]={ "_Coef",    "_Tstat",   /* 3dDeconvolve */
                                 "_mean",    "_Tstat",   /* 3dttest++ */
                                 ":Mean",    ":t-stat",  /* 3dANOVA */
                                 ":Contr",   ":t-stat",  /* 3dANOVA */
                                 ":Inten",   ":F-stat",  /* 3dANOVA */
                                 ":Diff",    ":t-stat",  /* 3dANOVA */
                                 ":b",       ":t",       /* 3dMEMA */
                                 "_mean",    "_Zscr",    /* GICOR */
                                 "_meanNC",    "_ZscrNC",    /* GICOR */
                                 NULL, NULL }; /* leave always at the bottom*/
         SUMA_LHv("Looking for decent match for %s\n", lab);
         if (lab) {
            ipair=0;
            while(ind2 < 0 && (ext = exta[ipair*2])) {
               if (STRING_HAS_SUFFIX(lab,ext)) {
                  lab[strlen(lab)-strlen(ext)]='\0';
                  lab2 = SUMA_append_string(lab,exta[ipair*2+1]);
                  SUMA_LHv("  Looking for %s\n", lab2);
                  if ((ind2 = SUMA_FindDsetColLabeled(dset, lab2)) >= 0) {
                     SUMA_LHv("Sub-brick %s%s has %s go with it.\n",
                                 lab, ext, lab2);
                     colp->OptScl->tind = ind2;
                     if (colp == SO->SurfCont->curColPlane ) {/* must set this
                                             regardless of setmen */
                        SUMA_LH("Setting threshold values");
                        SUMA_Set_Menu_Widget(SO->SurfCont->SwitchThrMenu, 
                                      colp->OptScl->tind+1);
                        if (SUMA_GetDsetColRange(colp->dset_link, 
                                             colp->OptScl->tind, range, loc)) {  
                           SUMA_SetScaleRange(SO, range );
                           SUMA_InitRangeTable(SO, -1) ;
                           SUMA_UpdateNodeValField(SO);
                        }else {
                           SUMA_S_Err("Failed to get range");
                        }
                     }
                  } SUMA_free(lab2); lab2=NULL;
                  /* put lab back together */
                  lab[strlen(lab)]=ext[0];
               }
               ++ipair;
            }
         }
            }
         break;
      case SW_LinkMode_Same:
         colp->OptScl->tind = ind;
         if (colp == SO->SurfCont->curColPlane ) {/* must set this
                                 regardless of setmen */
            SUMA_LH("Setting threshold values");
            SUMA_Set_Menu_Widget(SO->SurfCont->SwitchThrMenu, 
                          colp->OptScl->tind+1) ;
            if (SUMA_GetDsetColRange(colp->dset_link, 
                                 colp->OptScl->tind, range, loc)) {  
               SUMA_SetScaleRange(SO, range );
               SUMA_InitRangeTable(SO, -1) ;
               SUMA_UpdateNodeValField(SO);
            }else {
               SUMA_S_Err("Failed to get range");
            }
         }
         break;
      case SW_LinkMode_Pls1:
            {
         if (ind+1 >= SDSET_VECNUM(dset)) break; 
         colp->OptScl->tind = ind+1;
         if (colp == SO->SurfCont->curColPlane ) {/* must set this
                                 regardless of setmen */
            SUMA_LH("Setting threshold values");
            SUMA_Set_Menu_Widget(SO->SurfCont->SwitchThrMenu,
                          colp->OptScl->tind+1) ;
            if (SUMA_GetDsetColRange(colp->dset_link, 
                                 colp->OptScl->tind, range, loc)) {  
               SUMA_SetScaleRange(SO, range );
               SUMA_InitRangeTable(SO, -1) ;
               SUMA_UpdateNodeValField(SO);
            }else {
               SUMA_S_Err("Failed to get range");
            }
         }
            }
         break;
      default:
         break;
   }
   
   
   SUMA_LH("Setting Range, Intensity change");
   SUMA_InitRangeTable(SO, 0) ;
   SUMA_UpdateCrossHairNodeLabelFieldForSO(SO);

   if (colp->ShowMode < 0) { SUMA_RETURN(1); } /* nothing else to do */
   
   
   if (!SUMA_ColorizePlane (colp)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(0);
   }
   
   
   SUMA_RemixRedisplay(SO);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
 
   SUMA_UpdateNodeValField(SO);
   SUMA_UpdateNodeLblField(SO);
      
   SUMA_RETURN(1);
}

void SUMA_cb_SwitchIntensity(Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_SwitchIntensity"};
   int imenu = 0;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (imenu-1 == SO->SurfCont->curColPlane->OptScl->find) {
      SUMA_RETURNe; /* nothing to be done */
   }

   SUMA_SwitchColPlaneIntensity(SO, SO->SurfCont->curColPlane, imenu -1, 0);
   
   SUMA_RETURNe;
}

int SUMA_SwitchColPlaneThreshold(
         SUMA_SurfaceObject *SO, 
         SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneThreshold"};
   char srange[500];
   double range[2];
   int loc[2];
   SUMA_DSET *dset=NULL;
   SUMA_SurfaceObject *SOC=NULL; 
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_SwitchColPlaneThreshold_one(SO, colp, ind, setmen)) {
      SUMA_S_Err("Failed in _one");
      SUMA_RETURN(0);
   }

   /* do we have a contralateral SO and overlay? */
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      if (!SUMA_SwitchColPlaneThreshold_one(SOC, colpC, ind, 1)) {
         SUMA_S_Warn("Failed in contralateral");
      }
   }
   
   SUMA_RETURN(1);
}

int SUMA_SwitchColPlaneThreshold_one(
                                 SUMA_SurfaceObject *SO, SUMA_OVERLAYS *colp, 
                                 int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneThreshold_one"};
   char srange[500];
   double range[2]; int loc[2];  
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->SurfCont || !SO->SurfCont->curColPlane || 
       !colp || ind < -1 || !colp->dset_link) { SUMA_RETURN(0); }
   
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n request to switch threshold to col. %d\n", 
                  FuncName, ind);
   }
   if (ind < 0) {
      /* turn threshold off */
      XmToggleButtonSetState (SO->SurfCont->Thr_tb, NOPE, YUP);
      SUMA_RETURN(1);
   } 
   
   if (ind >= SDSET_VECNUM(colp->dset_link)) {
      SUMA_S_Errv("Col. Index of %d exceeds maximum of %d for this dset.\n", 
            ind, SDSET_VECNUM(colp->dset_link)-1);
      SUMA_RETURN(0);
   }
   if (ind != colp->OptScl->tind &&
          colp->OptScl->Clusterize) {
         /* Need a new clusterizing effort*/
         colp->OptScl->RecomputeClust = 1;
   }
   colp->OptScl->tind = ind;

   /* make sure threshold is on if command is not from the interface*/
   if (setmen && !colp->OptScl->UseThr && colp->OptScl->tind >= 0) {
      colp->OptScl->UseThr = YUP;
      XmToggleButtonSetState (SO->SurfCont->Thr_tb, YUP, NOPE);
   }
   
   if (setmen && colp == SO->SurfCont->curColPlane) {
      SUMA_Set_Menu_Widget(SO->SurfCont->SwitchThrMenu,
                    colp->OptScl->tind+1) ; 
   }
   
   if (SUMA_GetDsetColRange(colp->dset_link, colp->OptScl->tind, range, loc)) {  
      SUMA_SetScaleRange(SO, range );
   }else {
      SUMA_SLP_Err("Failed to get range");
      SUMA_RETURN(0);
   }
   
   /* threshold sub-brick change */ 
   SUMA_InitRangeTable(SO, -1) ;
   SUMA_UpdateCrossHairNodeLabelFieldForSO(SO);
   SUMA_UpdateNodeValField(SO);
   
   if (!colp->OptScl->UseThr) { SUMA_RETURN(1); } /* nothing else to do */

   if (!SUMA_ColorizePlane (colp)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(0);
   }
   
   SUMA_RemixRedisplay(SO);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURN(1);
}

void SUMA_cb_SwitchThreshold(Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_SwitchThreshold"};
   int imenu = 0;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (imenu-1 == SO->SurfCont->curColPlane->OptScl->tind) {
      SUMA_RETURNe; /* nothing to be done */
   }

   SUMA_SwitchColPlaneThreshold(SO, SO->SurfCont->curColPlane, imenu -1, 0);
   SUMA_RETURNe;
}

int SUMA_SwitchColPlaneBrightness(
         SUMA_SurfaceObject *SO, 
         SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneBrightness"};
   char srange[500];
   double range[2];
   int loc[2];
   SUMA_DSET *dset=NULL;
   SUMA_SurfaceObject *SOC=NULL; 
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch brightness to col. %d\n", FuncName, ind);
   }
   
   if (ind == colp->OptScl->bind) {
      SUMA_RETURN(0); /* nothing to be done */
   }
   
   if (!SUMA_SwitchColPlaneBrightness_one(SO, colp, ind, setmen)) {
      SUMA_S_Err("Failed in _one");
      SUMA_RETURN(0);
   }

   /* do we have a contralateral SO and overlay? */
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      if (!SUMA_SwitchColPlaneBrightness_one(SOC, colpC, ind, 1)) {
         SUMA_S_Warn("Failed in contralateral");
      }
   }
   
   SUMA_RETURN(1);
}

int SUMA_SwitchColPlaneBrightness_one(
         SUMA_SurfaceObject *SO, SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneBrightness_one"};
   char srange[500];
   double range[2]; int loc[2];  
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (  !SO || !SO->SurfCont || !SO->SurfCont->curColPlane || 
         !colp || ind < -1 || !colp->dset_link) { 
         SUMA_S_Err("NULLity");
         SUMA_RETURN(0); 
   }
   
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch brightness to col. %d, currently %d\n", 
            FuncName, ind, colp->OptScl->bind);
   }

   if (ind < 0) {
      /* turn brightness off */
      XmToggleButtonSetState (SO->SurfCont->Brt_tb, NOPE, YUP);
      SUMA_RETURN(1);
   } 
   
   if (ind >= SDSET_VECNUM(colp->dset_link)) {
      SUMA_S_Errv("Col. Index of %d exceeds maximum of %d for this dset.\n",
                  ind, SDSET_VECNUM(colp->dset_link)-1);
      SUMA_RETURN(0);
   }
   colp->OptScl->bind = ind;

   /* make sure brightness is on if command is not from the interface*/
   if (setmen && !colp->OptScl->UseBrt && colp->OptScl->bind >= 0) {
      colp->OptScl->UseBrt = YUP;
      XmToggleButtonSetState (SO->SurfCont->Brt_tb, YUP, NOPE);
   }
   
   if (setmen && colp == SO->SurfCont->curColPlane) {
      SUMA_Set_Menu_Widget(SO->SurfCont->SwitchBrtMenu, 
                    colp->OptScl->bind+1); 
   }
   
   if (SUMA_GetDsetColRange(colp->dset_link, colp->OptScl->bind, range, loc)) {   
      SUMA_SetScaleRange(SO, range );
   }else {
      SUMA_SLP_Err("Failed to get range");
      SUMA_RETURN(0);
   }
   
   /* brightness change */ 
   SUMA_InitRangeTable(SO, -1) ;
   SUMA_UpdateCrossHairNodeLabelFieldForSO(SO);
   SUMA_UpdateNodeValField(SO);
   
   if (!colp->OptScl->UseBrt) { SUMA_RETURN(1); } /* nothing else to do */

   if (!SUMA_ColorizePlane (colp)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(0);
   }
   
   SUMA_RemixRedisplay(SO);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURN(1);
}


void SUMA_cb_SwitchBrightness(Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_SwitchBrightness"};
   int imenu = 0;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   if (imenu-1 == SO->SurfCont->curColPlane->OptScl->bind) {
      SUMA_RETURNe; /* nothing to be done */
   }
   SUMA_SwitchColPlaneBrightness(SO, SO->SurfCont->curColPlane, imenu -1, 0);
   
   #if 0 /* Obsolete, now all handled in SUMA_SwitchColPlaneBrightness above*/
   if (LocalHead) {
      fprintf( SUMA_STDERR, 
               "%s:\n request to switch brightness to col. %d\n", 
               FuncName, imenu - 1);
   }
   
   SO->SurfCont->curColPlane->OptScl->bind = imenu - 1;

   SUMA_InitRangeTable(SO, 1) ;

   SUMA_UpdateNodeValField(SO);
   if (!SO->SurfCont->curColPlane->OptScl->UseBrt) { 
      SUMA_RETURNe; 
   } /* nothing else to do */
   
   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   

   SUMA_RemixRedisplay(SO);
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   SUMA_UpdateNodeLblField(SO);
   #endif
   
   SUMA_RETURNe;
}

int SUMA_SwitchCmap_one(SUMA_SurfaceObject *SO,
                         SUMA_COLOR_MAP *CM, int setmenu)
{
   static char FuncName[]={"SUMA_SwitchCmap_one"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !CM) SUMA_RETURN(0);
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n request to switch colormap to  (%s)\n", 
         FuncName, CM->Name);
   }
   
   if (setmenu) {
      if (!SUMA_SetCmapMenuChoice (SO, CM->Name)) {
             SUMA_SL_Err("Failed in SUMA_SetCmapMenuChoice");
      }
   }  
   if (!SUMA_SwitchColPlaneCmap(SO, CM)) {
      SUMA_SL_Err("Failed in SUMA_SwitchColPlaneCmap");
   }
   
   /* Now you'll need to close the list widget if a choice has been made */
   if (SUMAg_CF->X->SwitchCmapLst) {
      if (!SUMAg_CF->X->SwitchCmapLst->isShaded) 
         SUMA_cb_CloseSwitchCmap( NULL,  (XtPointer)SUMAg_CF->X->SwitchCmapLst,  
                                  NULL);
   }
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURN(1);
}

int SUMA_SwitchCmap(SUMA_SurfaceObject *SO,
                    SUMA_COLOR_MAP *CM, int setmenu)
{
   static char FuncName[]={"SUMA_SwitchCmap"};
   SUMA_SurfaceObject *SOC=NULL;
   SUMA_OVERLAYS *colp=NULL, *colpC=NULL;                     
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   if (!SO || !CM) SUMA_RETURN(0);
   
   if (!SUMA_SwitchCmap_one(SO, CM, setmenu)) SUMA_RETURN(0);
   
   /* do we have a contralateral SO and overlay? */
   colp = SO->SurfCont->curColPlane;
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      if (!SUMA_SwitchCmap_one(SOC, CM, 1)) {
         SUMA_S_Warn("Failed in contralateralination");
      }
   }
   
   SUMA_RETURN(1);
}

/*! 
   \brief function that handlges switching colormap from the menu widget
   \sa SUMA_cb_SelectSwitchCmap 
*/ 
void SUMA_cb_SwitchCmap(Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_SwitchCmap"};
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_COLOR_MAP *CM = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   CM = (SUMA_COLOR_MAP *)datap->callback_data; 
   
   SUMA_SwitchCmap(SO, CM, 0);
   
   SUMA_RETURNe;
}

void SUMA_cb_ShowZero_tb_toggled (Widget w, XtPointer data, 
                                  XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ShowZero_tb_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO || !SO->SurfCont) { SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
   if (  !SO->SurfCont->curColPlane || 
         !SO->SurfCont->curColPlane->OptScl )  { 
      SUMA_S_Warn("NULL input 2"); SUMA_RETURNe; 
   }
   
   SO->SurfCont->curColPlane->OptScl->MaskZero = 
      !SO->SurfCont->curColPlane->OptScl->MaskZero;
   
   if (!SO->SurfCont->curColPlane->ShowMode < 0) { 
      /* nothing else to do */ 
      SUMA_RETURNe;
   } 
   
   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_RemixRedisplay(SO);
 
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURNe;
}
 

void SUMA_cb_SymIrange_tb_toggled (Widget w, XtPointer data, 
                                   XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SymIrange_tb_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO || !SO->SurfCont) { SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
   if (!SO->SurfCont->curColPlane) { SUMA_S_Warn("NULL input 2"); SUMA_RETURNe; }
   
   SO->SurfCont->curColPlane->SymIrange = !SO->SurfCont->curColPlane->SymIrange;
   
   if (SO->SurfCont->curColPlane->SymIrange) {
      /* manual setting of range. 
         DO NOT Call SUMA_InitRangeTable because it will 
         automatically update the I range under certain conditions*/
      TF = SO->SurfCont->SetRangeTable;
      SO->SurfCont->curColPlane->OptScl->IntRange[1] = 
         SUMA_LARG_ABS(SO->SurfCont->curColPlane->OptScl->IntRange[0], 
         SO->SurfCont->curColPlane->OptScl->IntRange[1]);
      SO->SurfCont->curColPlane->OptScl->IntRange[0] = 
         -SO->SurfCont->curColPlane->OptScl->IntRange[1];
      SUMA_INSERT_CELL_VALUE(TF, 1, 1, 
                  SO->SurfCont->curColPlane->OptScl->IntRange[0]);
      SUMA_INSERT_CELL_VALUE(TF, 1, 2, 
                  SO->SurfCont->curColPlane->OptScl->IntRange[1]);
   }
   
   if (!SO->SurfCont->curColPlane->ShowMode < 0) { SUMA_RETURNe; } 
      /* nothing else to do */
   
   
   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_RemixRedisplay(SO);
 
   SUMA_UpdateNodeValField(SO);
   SUMA_UpdateNodeLblField(SO);
   

   SUMA_RETURNe;
}

void SUMA_cb_AbsThresh_tb_toggled (Widget w, XtPointer data, 
                                   XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_AbsThresh_tb_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   char slabel[100];
   double range[2]; int loc[2];  
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;

   if (SO->SurfCont->curColPlane->OptScl->ThrMode != SUMA_ABS_LESS_THAN) {
      SO->SurfCont->curColPlane->OptScl->ThrMode = SUMA_ABS_LESS_THAN;
      /* used to use this:
      sprintf(slabel, "|%5s|", .... 
      but that does not work in the editable field ... */
      sprintf(slabel, "%5s", MV_format_fval(
               fabs(SO->SurfCont->curColPlane->OptScl->ThreshRange[0])));
   } else {
      SO->SurfCont->curColPlane->OptScl->ThrMode = SUMA_LESS_THAN;
      sprintf(slabel, "%5s", MV_format_fval(
               SO->SurfCont->curColPlane->OptScl->ThreshRange[0]));
   }
   /* SUMA_SET_LABEL(SO->SurfCont->thr_lb,  slabel); */
   SUMA_INSERT_CELL_STRING(SO->SurfCont->SetThrScaleTable, 0,0,slabel); 
   if (SUMA_GetDsetColRange(SO->SurfCont->curColPlane->dset_link, 
                     SO->SurfCont->curColPlane->OptScl->tind, range, loc)) {   
      SUMA_SetScaleRange(SO, range );
   }else {
      SUMA_SLP_Err("Failed to get range");
      SUMA_RETURNe;
   }
      
   if (!SO->SurfCont->curColPlane->OptScl->UseThr) { SUMA_RETURNe; } 
                                                /* nothing else to do */

   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_RemixRedisplay(SO);

   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
}

void SUMA_cb_SwitchInt_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SwitchInt_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;

   /* make sure ok to turn on */
   if (SO->SurfCont->curColPlane->OptScl->find < 0) {
      SUMA_BEEP;
      SUMA_SLP_Note("no intensity column set");
      XmToggleButtonSetState (SO->SurfCont->Int_tb, NOPE, NOPE);
      SUMA_RETURNe;
   }
      
   /* this button's the same as the Show button */
   if (XmToggleButtonGetState (SO->SurfCont->Int_tb)) { 
      SO->SurfCont->curColPlane->ShowMode = 
         SUMA_ABS(SO->SurfCont->curColPlane->ShowMode);
   } else {
      SO->SurfCont->curColPlane->ShowMode = 
         -SUMA_ABS(SO->SurfCont->curColPlane->ShowMode);
   }
   
   SUMA_Set_Menu_Widget(SO->SurfCont->DsetViewModeMenu,
                 SUMA_ShowMode2ShowModeMenuItem(
                                 SO->SurfCont->curColPlane->ShowMode));
      
   SUMA_ColorizePlane(SO->SurfCont->curColPlane);
   SUMA_RemixRedisplay(SO);
   SUMA_UpdateNodeLblField(SO);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   SUMA_RETURNe;
}

void SUMA_cb_SwitchThr_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SwitchThr_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;
   
   /* make sure ok to turn on */
   if (SO->SurfCont->curColPlane->OptScl->tind < 0) {
      SUMA_BEEP;
      SUMA_SLP_Note("no threshold column set");
      XmToggleButtonSetState (SO->SurfCont->Thr_tb, NOPE, NOPE);
      SUMA_RETURNe;
   }
      
   SO->SurfCont->curColPlane->OptScl->UseThr = 
         XmToggleButtonGetState (SO->SurfCont->Thr_tb);
      
   SUMA_ColorizePlane(SO->SurfCont->curColPlane);
   SUMA_RemixRedisplay(SO);
   
   SUMA_UpdateNodeLblField(SO);
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   SUMA_RETURNe;
}

void SUMA_cb_SwitchBrt_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SwitchBrt_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;
   
   /* make sure ok to turn on */
   if (SO->SurfCont->curColPlane->OptScl->bind < 0) {
      SUMA_BEEP;
      SUMA_SLP_Note("no brightness column set");
      XmToggleButtonSetState (SO->SurfCont->Brt_tb, NOPE, NOPE);
      SUMA_RETURNe;
   }
   
   SO->SurfCont->curColPlane->OptScl->UseBrt = 
                     XmToggleButtonGetState (SO->SurfCont->Brt_tb);
   
   SUMA_ColorizePlane(SO->SurfCont->curColPlane);
   SUMA_RemixRedisplay(SO);
   SUMA_UpdateNodeLblField(SO);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   SUMA_RETURNe;
}

SUMA_MenuItem CoordBias_Menu[] = {
   {  "-", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCoordBias, (XtPointer) SW_CoordBias_None, NULL},
      
   {  "x", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCoordBias, (XtPointer) SW_CoordBias_X, NULL},
   
   {  "y", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCoordBias, (XtPointer) SW_CoordBias_Y, NULL},
    
   {  "z", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCoordBias, (XtPointer) SW_CoordBias_Z, NULL},
        
   {  "n", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCoordBias, (XtPointer) SW_CoordBias_N, NULL},
   
   {NULL},
};

SUMA_MenuItem CmapMode_Menu[] = {
   {  "Dir", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCmapMode, (XtPointer) SW_Direct, NULL},
    
   {  "NN", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCmapMode, (XtPointer) SW_NN, NULL},
   
   {  "Int", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCmapMode, (XtPointer) SW_Interp, NULL},
      
   {NULL},
};

SUMA_MenuItem LinkMode_Menu[] = {
   {  "None", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetLinkMode, (XtPointer) SW_LinkMode_None, NULL},
    
   {  "Pls1", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetLinkMode, (XtPointer) SW_LinkMode_Pls1, NULL},
   
   {  "Same", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetLinkMode, (XtPointer) SW_LinkMode_Same, NULL},
   
   {  "Stat", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetLinkMode, (XtPointer) SW_LinkMode_Stat, NULL},

   {NULL},
};


/*!
   \brief sets the colormap interpolation mode
   - expects a SUMA_MenuCallBackData * in  client_data
   with SO as client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetCmapMode(Widget widget, XtPointer client_data, 
                         XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetCmapMode"};
   SUMA_MenuCallBackData *datap=NULL;
   int imenu;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   SUMA_SetCmapMode(SO, imenu);
      
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_SetCmapMode(SUMA_SurfaceObject *SO, int imenu)
{
   static char FuncName[]={"SUMA_SetCmapMode"};
   SUMA_Boolean NewDisp = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->SurfCont || 
       !SO->SurfCont->curColPlane || imenu < 1) SUMA_RETURN(NOPE);
   
   /* get the surface object that the setting belongs to */
   NewDisp = NOPE;
   switch (imenu) {
      case SW_Interp:
         if (SO->SurfCont->curColPlane->OptScl->interpmode != SUMA_INTERP) {
            SO->SurfCont->curColPlane->OptScl->interpmode = SUMA_INTERP;
            NewDisp = YUP;
         }
         break;
      case SW_NN:
         if (SO->SurfCont->curColPlane->OptScl->interpmode != SUMA_NO_INTERP) {
            SO->SurfCont->curColPlane->OptScl->interpmode = SUMA_NO_INTERP;
            NewDisp = YUP;
         }
         break;
      case SW_Direct:
         if (SO->SurfCont->curColPlane->OptScl->interpmode != SUMA_DIRECT) {
            SO->SurfCont->curColPlane->OptScl->interpmode = SUMA_DIRECT;
            NewDisp = YUP;
         }
         break;
      default: 
         fprintf (SUMA_STDERR, 
                  "Error %s: Unexpected widget index.\n", FuncName);
         SUMA_RETURN(NOPE);
         break;
      
   }
   
   /* redisplay all viewers showing SO*/
   if (NewDisp) {
      SUMA_ColorizePlane(SO->SurfCont->curColPlane);
      SUMA_RemixRedisplay(SO);
   }
   
   SUMA_UpdateNodeNodeField(SO);
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURN(YUP);
}

/*!
   \brief sets the linking between I & T mode
   - expects a SUMA_MenuCallBackData * in  client_data
   with SO as client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetLinkMode(Widget widget, XtPointer client_data, 
                         XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetLinkMode"};
   SUMA_MenuCallBackData *datap=NULL;
   int imenu;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean NewDisp = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   NewDisp = NOPE;
   switch (imenu) { /* There is really no need for this 
                        switching block... */
      case SW_LinkMode_None:
         if (SO->SurfCont->curColPlane->LinkMode != imenu) {
             SO->SurfCont->curColPlane->LinkMode = imenu;
            NewDisp = YUP;
         }
         break;
      case SW_LinkMode_Pls1:
         if (SO->SurfCont->curColPlane->LinkMode != imenu) {
             SO->SurfCont->curColPlane->LinkMode = imenu;
            NewDisp = YUP;
         }
         break;
      case SW_LinkMode_Same:
         if (SO->SurfCont->curColPlane->LinkMode != imenu) {
             SO->SurfCont->curColPlane->LinkMode = imenu;
            NewDisp = YUP;
         }
         break;
      case SW_LinkMode_Stat:
         if (SO->SurfCont->curColPlane->LinkMode != imenu) {
             SO->SurfCont->curColPlane->LinkMode = imenu;
            NewDisp = YUP;
         }
         break;
      default: 
         fprintf (SUMA_STDERR, "Error %s: Unexpected widget index %d.\n", 
                                 FuncName, imenu);
         break;
   }
   SUMA_LHv("LinkMode now %d\n", SO->SurfCont->curColPlane->LinkMode);
   /* redisplay all viewers showing SO*/
   if (NewDisp) {
      SUMA_ColorizePlane(SO->SurfCont->curColPlane);
      SUMA_RemixRedisplay(SO);
   }
   
   SUMA_UpdateNodeNodeField(SO);
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURNe;
}

/*!
   \brief sets the coordinate bias mode
   - expects a SUMA_MenuCallBackData * in  client_data
   with SO as client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetCoordBias(Widget widget, XtPointer client_data, 
                           XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetCoordBias"};
   SUMA_MenuCallBackData *datap=NULL;
   int imenu;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_VIS_XFORM_DATUM *x0=NULL;
   SUMA_Boolean NewDisp = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 

   /* if CoordBias is to be added, it should be before Prying */
   x0 = SUMA_Fetch_VisX_Datum ("CoordBias", SO->VisX.Xchain, 
                               ADD_BEFORE, "Prying");

   NewDisp = NOPE;
   switch (imenu) {
      case SW_CoordBias_None:
         if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_None) {
            if (SO->SurfCont->curColPlane->OptScl->BiasVect) {
               SUMA_RemoveCoordBias(SO->SurfCont->curColPlane);
            }
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_X:
         if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_X) { 
               /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to 
                  new dimension*/
               SUMA_TransferCoordBias(SO->SurfCont->curColPlane, SW_CoordBias_X);
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_Y:
         if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_Y) { 
               /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to 
               new dimension*/
               SUMA_TransferCoordBias(SO->SurfCont->curColPlane, SW_CoordBias_Y);
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_Z:
         if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_Z) { 
               /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to 
                  new dimension*/
               SUMA_TransferCoordBias(SO->SurfCont->curColPlane, SW_CoordBias_Z);
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_N:
         if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_N) { 
               /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to 
                  new dimension*/
               SUMA_TransferCoordBias(SO->SurfCont->curColPlane, SW_CoordBias_N);
            NewDisp = YUP;
         }
         break;
      default: 
         fprintf (SUMA_STDERR, "Error %s: Unexpected widget index.\n", FuncName);
         break;
   }
   
   /* redisplay all viewers showing SO*/
   if (NewDisp) {
      SUMA_ColorizePlane(SO->SurfCont->curColPlane);
      SUMA_RemixRedisplay(SO);
   }
   
   SUMA_UpdateNodeNodeField(SO);
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   SUMA_RETURNe;
}

/*!
   \brief Function to call a redisplay of all viewers showing SO 
*/
SUMA_Boolean SUMA_RedisplayAllShowing(char *SO_idcode_str, SUMA_SurfaceViewer *SVv, int N_SVv)
{
   static char FuncName[]={"SUMA_RedisplayAllShowing"};
   SUMA_SurfaceViewer *sv;
   SUMA_SurfaceObject *SO1 = NULL, *SO2 = NULL;
   int i, k, dov_id;   
   DList *list=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO_idcode_str || !SVv) {
      fprintf (SUMA_STDERR,"Error %s: NULL SVv or SO_idcode_str. BAD\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   dov_id = SUMA_findSO_inDOv (SO_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   if (dov_id < 0) {
      fprintf (SUMA_STDERR,"Error %s: Failed to find object with idcode %s.\n", FuncName, SO_idcode_str);
      SUMA_RETURN (NOPE);
   }
   SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[dov_id].OP;

   /* search all viewers */
   for (i=0; i < N_SVv; ++i) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Searching viewer %d.\n", FuncName, i);
      sv = &(SVv[i]);
      /* search for SO in RegisteredDO */
      for (k=0; k < sv->N_DO; ++k) {
         if (SUMA_isSO(SUMAg_DOv[sv->RegisteredDO[k]])) {
            SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[sv->RegisteredDO[k]].OP;
            if (SUMA_WhatAreYouToMe(SO1, SO2) == SUMA_SO1_is_SO2) { /* same surface */
               /* Get a redisplay for that puppy */
               if (!list) list = SUMA_CreateList ();
               SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_SumaWidget, sv);
            }
         }  
      } 
   }
   
   if (!SUMA_Engine(&list)) {
      SUMA_SLP_Err("Failed to redisplay.");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(YUP);
}

SUMA_TABLE_FIELD * SUMA_AllocTableField(void)
{
   static char FuncName[]={"SUMA_AllocTableField"};
   SUMA_TABLE_FIELD *TF = NULL;

   SUMA_ENTRY;
   TF = (SUMA_TABLE_FIELD *)SUMA_calloc(1,sizeof(SUMA_TABLE_FIELD));
   if (!TF) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(TF);
   }
   TF->Ni = -1;
   TF->Nj = -1;
   TF->rc = NULL;
   TF->cells = NULL;
   TF->cwidth = NULL;
   TF->editable = NOPE;
   TF->type = SUMA_string;
   TF->NewValueCallback = NULL;
   TF->NewValueCallbackData = NULL;
   TF->TitLabelEVHandler = NULL;
   TF->TitLabelEVHandlerData = NULL;
   TF->CellEVHandler = NULL;
   TF->CellEVHandlerData = NULL;
   TF->cell_modified = -1;
   TF->num_value = NULL;
   TF->str_value = NULL;
   SUMA_RETURN(TF);
}

/*!
   Called when user clicks on range table cell
   Expect SO in cd
*/
void SUMA_RangeTableCell_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_RangeTableCell_EV"};
   SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)cd;
   SUMA_SurfaceObject *curSO = *(SO->SurfCont->curSOp);
   SUMA_TABLE_FIELD *TF = SO->SurfCont->RangeTable;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  i, j, n, Found;
   void *cv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ) {
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || TF == NULL || SO == NULL ) { SUMA_RETURNe ; }

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
      default:
         SUMA_RETURNe;
   }
   
   /* which cell is calling? */
   n = 0;
   Found = -1;
   while (n<TF->Nj*TF->Ni && Found == -1) {
      if (TF->cells[n] == w) {
         Found = n;
      } else ++n;
   }
   
   if (Found <0) {
      SUMA_SL_Err("Widget not found ????");
      SUMA_RETURNe;
   }
   
   /* find out widget's place in table*/
   i = Found % TF->Ni; j = Found / TF->Ni ;
   n = Found; 
   
   switch (j) {
      case 0:
      case 1:
      case 3:
         break;
      case 2:
      case 4:
         SUMA_LH("Call to jump to a node");
         XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s, node = %d\n", 
                  FuncName, i, j, (char *)cv, atoi((char *)cv));
         }

         /* look for a viewer that is showing this surface 
            and has this surface in focus*/
         for (i=0; i<SUMAg_N_SVv; ++i) {
            SUMA_LHv("Checking viewer %d.\n", i);
            if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
               /* is this viewer showing curSO ? */
               if (SUMA_isVisibleSO(&(SUMAg_SVv[i]), SUMAg_DOv, curSO)) {
                  if ((SUMAg_DOv[SUMAg_SVv[i].Focus_SO_ID].OP) == curSO) {
                        SUMA_JumpIndex((char *)cv, (void *)(&(SUMAg_SVv[i])));
                  } else {
                     SUMAg_SVv[i].Focus_SO_ID = 
                        SUMA_findSO_inDOv (curSO->idcode_str, 
                                           SUMAg_DOv, SUMAg_N_DOv);
                     if (SUMA_SURFCONT_REALIZED(curSO)) {   
                        SUMA_Init_SurfCont_SurfParam(curSO);
                     } else {
                        SUMA_S_Err("How did this happen?");
                     }
                     SUMA_JumpIndex((char *)cv, (void *)(&(SUMAg_SVv[i])));
                     SUMA_UpdateViewerTitle(SUMAg_SVv+i);
                  }
               }
            }
         }

         break;
      default:
         SUMA_SL_Err("Did not know you had so many");
         break;
   }
   
   SUMA_RETURNe;
}
/*!
   Called when user clicks on table title 
   Expects SUMA_SRV_DATA in TF->NewValueCallbackData
*/
void SUMA_SetRangeTableTit_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_SetRangeTableTit_EV"};
   Dimension lw ;
   Widget * children , wl = NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  num_children , i, j, Found, AutoHist;
   SUMA_TABLE_FIELD *TF = (SUMA_TABLE_FIELD *)cd;
   SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)TF->NewValueCallbackData;
   SUMA_SurfaceObject *SO = srvd->SO;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ){
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || TF == NULL ) SUMA_RETURNe ;

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
      default:
         SUMA_RETURNe;
   }
   
   /* which column title (i == 0) widget is calling ? */
   /* check the first column */
   i = 0; j = 0;
   Found = 0;
   while (j<TF->Nj && !Found) {
      if (TF->cells[j*TF->Ni+i] == w) {
         Found = 1;
      } else ++j;
   }
   
   if (!Found) { /* maybe it is a row title */
      i = 0; j = 0;
      Found = 0;
      while (i<TF->Ni && !Found) {
         if (TF->cells[j*TF->Ni+i] == w) {
            Found = 1;
         } else ++i;
      }
   }
   
   if (Found >= 0) {
      SUMA_LHv("Click on cell [%d %d]\n", i, j);
   } else {
      SUMA_SL_Err("CEll not found!");
      SUMA_RETURNe;
   }
   if (!SO->SurfCont->curColPlane) {
      SUMA_SL_Err("No curColPlane!");
      SUMA_RETURNe;
   }
   
   /* Now do something */
   if (j == 0) { /* clicked on one of the row's titles */
      switch (i) {
         case 1:
            if (bev->button == Button1) { /* toggle lock */
               SO->SurfCont->curColPlane->OptScl->AutoIntRange = 
                           !SO->SurfCont->curColPlane->OptScl->AutoIntRange;
               SO->SurfCont->IntRangeLocked = !SO->SurfCont->IntRangeLocked;
               MCW_invert_widget(w);
            }else if (bev->button == Button3) { /* reset to autorange values */
               AutoHist = SO->SurfCont->curColPlane->OptScl->AutoIntRange; 
               SO->SurfCont->curColPlane->OptScl->AutoIntRange = 1;
               SUMA_InitRangeTable(SO, 0); /* overkill but little overhead */
               SUMA_ColorizePlane(SO->SurfCont->curColPlane);
               SUMA_RemixRedisplay(SO);
               SO->SurfCont->curColPlane->OptScl->AutoIntRange = AutoHist; 
            }
            break;
         case 2:
            if (bev->button == Button1) { /* toggle lock */
               SO->SurfCont->curColPlane->OptScl->AutoBrtRange = 
                           !SO->SurfCont->curColPlane->OptScl->AutoBrtRange;
               SO->SurfCont->BrtRangeLocked = !SO->SurfCont->BrtRangeLocked;
               MCW_invert_widget(w);   
            }else if (bev->button == Button3) { /* reset to autorange values */
               AutoHist = SO->SurfCont->curColPlane->OptScl->AutoBrtRange; 
               SO->SurfCont->curColPlane->OptScl->AutoBrtRange = 1;
               SUMA_InitRangeTable(SO, 1); /* overkill but little overhead */
               SUMA_ColorizePlane(SO->SurfCont->curColPlane);
               SUMA_RemixRedisplay(SO);
               SO->SurfCont->curColPlane->OptScl->AutoBrtRange = AutoHist; 
            }
            break;
         case 3:
            break;
         default:
            break;
      }
   }
   if (i == 0) { /* clicked on one of the column's titles */
      switch (j) {
         case 1:
            break;
         case 2:
            break;
         case 3:
            break;
         default:
            break;
      }
   }
   
   /* update the Xhair Info block */
   if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_None) {
      SUMA_UpdateNodeNodeField(SO);    
   }
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;

}

SUMA_Boolean SUMA_SetClustTableTit_one (SUMA_SurfaceObject *SO, 
                        SUMA_OVERLAYS *colp, int i, int j, int Button) 
{
   static char FuncName[]={"SUMA_SetClustTableTit_one"};
   SUMA_TABLE_FIELD *TF = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");

   if (!SO) SUMA_RETURN(0);
   if (colp && colp != SO->SurfCont->curColPlane) SUMA_RETURN(0);
   colp = SO->SurfCont->curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (!(TF = SO->SurfCont->SetClustTable)) SUMA_RETURN(0);
   
   /* Now do something */
   if (j == 0) { /* clicked on one of the row's titles */
      switch (i) {
         case 1:
            if (Button == Button1) { /* toggle lock */
               TF->but_flag[j*TF->Ni+i] = !TF->but_flag[j*TF->Ni+i];
               MCW_invert_widget(TF->cells[j*TF->Ni+i]);
               colp->OptScl->Clusterize = TF->but_flag[j*TF->Ni+i];
               colp->OptScl->RecomputeClust = YUP;
               SUMA_ColorizePlane(colp);
               SUMA_RemixRedisplay(SO);
            }else if (Button == Button3) { /* nothing to do */
               
            }
            break;
         default:
            break;
      }
   }
   if (i == 0) { /* clicked on one of the column's titles */
      switch (j) {
         case 1:
            break;
         case 2:
            break;
         case 3:
            break;
         default:
            break;
      }
   }
   
   /* update the Xhair Info block */
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURN(YUP);
}   

/* Set the button flag for a table cell (usually for titles only),
based on flag. Inverts widget if need be. */
SUMA_Boolean SUMA_SetTableTitleButton1(SUMA_TABLE_FIELD *TF, int i, int j, 
                                       byte flag)
{
   static char FuncName[]={"SUMA_SetTableTitleButton1"};
   
   SUMA_ENTRY;
   
   if (!TF) SUMA_RETURN(NOPE);
   
   if (flag == TF->but_flag[j*TF->Ni+i]) {
      /* Nothing to do, return*/
   } else {
      TF->but_flag[j*TF->Ni+i] = !TF->but_flag[j*TF->Ni+i];
               MCW_invert_widget(TF->cells[j*TF->Ni+i]);
   }
   
   SUMA_RETURN(YUP);   
}

SUMA_Boolean SUMA_SetClustTableTit (SUMA_SurfaceObject *SO, 
                        SUMA_OVERLAYS *colp, int i, int j, int Button) 
{
   static char FuncName[]={"SUMA_SetClustTableTit"};
   SUMA_SurfaceObject *SOC=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!SO) SUMA_RETURN(0);
   if (colp && colp != SO->SurfCont->curColPlane) SUMA_RETURN(0);
   colp = SO->SurfCont->curColPlane;
   if (!colp) SUMA_RETURN(0);
   
   if (!SUMA_SetClustTableTit_one (SO, colp, i, j, Button)) SUMA_RETURN(0);
      
   /* do we have a contralateral SO and overlay? */
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      if (!SUMA_SetClustTableTit_one (SOC, colpC, i, j, Button)) 
                                                         SUMA_RETURN(0);
   }


   SUMA_RETURN(YUP);
}   

/*!
   Called when user clicks on table title 
   Expects SUMA_SRV_DATA in TF->NewValueCallbackData
*/
void SUMA_SetClustTableTit_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_SetClustTableTit_EV"};
   Dimension lw ;
   Widget * children , wl = NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  num_children , i, j, Found;
   SUMA_TABLE_FIELD *TF = (SUMA_TABLE_FIELD *)cd;
   SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)TF->NewValueCallbackData;
   SUMA_SurfaceObject *SO = srvd->SO;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ){
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || TF == NULL ) SUMA_RETURNe ;

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
      default:
         SUMA_RETURNe;
   }
   
   /* which column title (i == 0) widget is calling ? */
   /* check the first column */
   i = 0; j = 0;
   Found = 0;
   while (j<TF->Nj && !Found) {
      if (TF->cells[j*TF->Ni+i] == w) {
         Found = 1;
      } else ++j;
   }
   
   if (!Found) { /* maybe it is a row title */
      i = 0; j = 0;
      Found = 0;
      while (i<TF->Ni && !Found) {
         if (TF->cells[j*TF->Ni+i] == w) {
            Found = 1;
         } else ++i;
      }
   }
   
   if (Found >= 0) {
      SUMA_LHv("Click on cell [%d %d]\n", i, j);
   } else {
      SUMA_SL_Err("CEll not found!");
      SUMA_RETURNe;
   }
   if (!SO->SurfCont->curColPlane) {
      SUMA_SL_Err("No curColPlane!");
      SUMA_RETURNe;
   }


   if (!SUMA_SetClustTableTit(SO, SO->SurfCont->curColPlane, i, j, bev->button)){
      SUMA_S_Err("Failed, weird");
   }
   
   SUMA_RETURNe;

}

SUMA_TABLE_FIELD * SUMA_FreeTableField(SUMA_TABLE_FIELD *TF)
{
   static char FuncName[]={"SUMA_FreeTableField"};
   int i;
   
   SUMA_ENTRY;

   if (!TF) SUMA_RETURN(NULL);

   if (TF->cells) SUMA_free(TF->cells);
   if (TF->cwidth) SUMA_free(TF->cwidth);
   if (TF->num_value) SUMA_free(TF->num_value);
   if (TF->but_flag) SUMA_free(TF->but_flag);
   if (TF->str_value) { 
      for (i=0; i<TF->Nj*TF->Ni; ++i) 
         if (TF->str_value[i]) SUMA_free(TF->str_value[i]); 
      SUMA_free(TF->str_value);
   }
   SUMA_free(TF);

   SUMA_RETURN(NULL);

}

/*!
   \brief sets a cell entry widget to be in Edit or No Edit modes
   \param TF
   \param i (int) the row index
   \param j (int) the col index
   \param Mode (int) 0 no edit, 1 yes edit
*/
void  SUMA_SetCellEditMode(SUMA_TABLE_FIELD *TF, int i, int j, int Mode)
{
   static char FuncName[]={"SUMA_SetCellEditMode"};
   int n;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;                        

   if (!TF) { SUMA_SL_Err("NULL TF"); SUMA_RETURNe; }
   n = j * TF->Ni + i;
   
   /* remove calls anyway */
   XtRemoveCallback (TF->cells[n], XmNactivateCallback, 
                     SUMA_TableF_cb_label_change, (XtPointer)TF);
   XtRemoveCallback (TF->cells[n], XmNmodifyVerifyCallback, 
                     SUMA_TableF_cb_label_Modify, (XtPointer)TF);
   /* remove event handlers */
   XtRemoveEventHandler( TF->cells[n] ,        
                         LeaveWindowMask ,  
                         FALSE ,           
                         SUMA_leave_TableField,
                         (XtPointer) TF);
   switch (Mode) {
      case 0:
         /* non edit */
         XtVaSetValues(TF->cells[n],
                       XmNeditable, False, 
                       XmNshadowThickness , 1,          /* hide the border */
                       XmNcursorPositionVisible, False, /* hide the cursor */
                       NULL);
         break;
      case 1:
         /* si edit */
         XtVaSetValues(TF->cells[n],
                       XmNeditable, True, 
                       XmNshadowThickness , 2,         
                       XmNcursorPositionVisible, True, 
                       NULL);
         
         XtAddCallback (TF->cells[n], XmNactivateCallback, 
                        SUMA_TableF_cb_label_change, (XtPointer)TF);
         XtAddCallback (TF->cells[n], XmNmodifyVerifyCallback, 
                        SUMA_TableF_cb_label_Modify, (XtPointer)TF);
         /* add event handler to notify when widget was left */
         XtInsertEventHandler( TF->cells[n] ,        /* notify when */
                                  LeaveWindowMask ,  /* pointer leaves */
                                  FALSE ,            /* this window */
                                  SUMA_leave_TableField,
                                  (XtPointer) TF ,
                                  XtListTail ) ;     /* last in queue */
         break;
      default:
         SUMA_SL_Err("What?");
         break;
   }
   SUMA_RETURNe;
}

/* 
Appends a font of a some name to a fontlist. 
You'll almost always want to call the function with the same 1st argument and
the returned variable. The function will free the old one and return a new one.
Read function for more detail.
 
   fontlist = SUMA_AppendToFontList(fontlist, w, "6x10", NULL);

Font lesson from http://www-h.eng.cam.ac.uk/help/tpl/graphics/Motif/mt3 :
-------------------------------------------------------------------------
"""Begin quote 
[How do you find out which font names are valid on your system? Generally,
type the command:

	xlsfonts -fn "*" > out,

and then look at "out". All the font names will be in the file. Some of 
them are short, like "6x10" used above, but some are monsters, as shown 
in the fragment of my "out" file copied below:

-adobe-times-bold-i-normal--24-240-75-75-p-128-iso8859-1
-adobe-times-bold-i-normal--25-180-100-100-p-128-iso8859-1
-adobe-times-bold-i-normal--34-240-100-100-p-170-iso8859-1

To use one of these fonts, I can say:
	
	 namestring="*times*-25-*";

in the above code. This will get a 25 point times font. If I want a specific
times font I can be more specific, like "*times*bold*-25-*". The "*" is
a wildcard like it is in a file name.]
"""End quote

*/
XmFontList SUMA_AppendToFontList(XmFontList fontlisti, Widget w, 
                                 char *fontname, char *tag)
{
   static char FuncName[]={"SUMA_AppendToFontList"};
   XFontStruct *font = NULL;
   XmFontList fontlist = NULL;
   XmFontListEntry entry = NULL;
   
   SUMA_ENTRY;
   
   if (!tag) tag = XmFONTLIST_DEFAULT_TAG;
   
   if (!(font = XLoadQueryFont(XtDisplay(w), fontname))) {
      SUMA_S_Errv("Failed to get font named %s\n", fontname);
      SUMA_RETURN(fontlist);
   }
   #if 0
      /* OBSOLETE, do not use. If you use it, you'll also need to take care 
      of clean up */
   fontlist = XmFontListCreate(font, XmSTRING_DEFAULT_CHARSET);
   #else
      entry = XmFontListEntryCreate(tag, 
                                    XmFONT_IS_FONT, font);
      /* Do not free font after this call. 
         Unless all other lists referencing it are feed */
      fontlist = XmFontListAppendEntry(fontlisti, entry);
         /* fontlisti is taken care of inside XmFontListAppendEntry */
      XmFontListEntryFree(&entry); entry = NULL;
   #endif
   
   SUMA_RETURN(fontlist);
} 

/*!
   \brief create a table widget
   \param parent (Widget) 
   \param Ni (int) number of entries in column (including title, if any)
   \param Nj (int) number of entries in row (including title, if any)
   \param col_tit (char **) if not NULL then this should be Nj strings
                            to appear as titles above each column
   \param row_tit (char **) if not NULL then this should be Ni strings
                            to appear as titles before each row
   \param cwidth (int *) number of characters to allow for each column. No auto sizing allowed
   \param editable (SUMA_Boolean) if YUP then fields are editable by user
   \param NewValueCallback(void * data) (void) function called when user changes value
   \param cb_data (void *) pointer to data sent back to call back
   \param TF (SUMA_TABLE_FIELD *) structure containing table info and the index
                                 for the newly modified field.
*/
void SUMA_CreateTable(  Widget parent,
                        int Ni, int Nj, 
                        char **row_tit, char **col_tit,
                        char **row_hint, char **col_hint,
                        char **row_help, char **col_help, 
                        int *cwidth, SUMA_Boolean editable, SUMA_VARTYPE type,
                        void (*NewValueCallback)(void * data), void *cb_data,
                        void (*TitLabelEVHandler)
                           (  Widget w , XtPointer cd , XEvent *ev , 
                              Boolean *ctd   ), 
                        void *TitLabelEVHandlerData,
                        void (*CellEVHandler)
                           (  Widget w , XtPointer cd , XEvent *ev , 
                              Boolean *ctd), 
                        void *CellEVHandlerData,
                        SUMA_TABLE_FIELD *TF) 
{
   static char FuncName[]={"SUMA_CreateTable"};
   int i, j, n, titw, xmw, shad;
   char *tmp;
   Widget rco, rcc;
   XtPointer cd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;                        

   if (!parent) { SUMA_SL_Err("NULL parent"); SUMA_RETURNe; }
   
   /* initialize font list if need be */
   if (!SUMAg_CF->X->TableTextFontList) {
      if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
         SUMAg_CF->X->TableTextFontList = 
               SUMA_AppendToFontList( SUMAg_CF->X->TableTextFontList, 
                                       parent, "*9x15*", NULL);
      } else {
         SUMAg_CF->X->TableTextFontList = 
               SUMA_AppendToFontList( SUMAg_CF->X->TableTextFontList, 
                                       parent, "*8x13*", NULL);
      }    
   }
   
   if (!TF) { SUMA_SL_Err("NULL TF"); SUMA_RETURNe; }
   TF->Ni = Ni; TF->Nj = Nj; TF->editable = editable; 
   TF->cwidth = (int *)SUMA_calloc(TF->Nj, sizeof(int)); 
   for (j=0; j<TF->Nj; ++j) TF->cwidth[j] = cwidth[j];
   if(col_tit) TF->HasColTit = YUP; else TF->HasColTit = NOPE;
   if(row_tit) TF->HasRowTit = YUP; else TF->HasRowTit = NOPE;
   TF->cells = (Widget *)SUMA_calloc(TF->Ni*TF->Nj,sizeof(Widget));
   TF->but_flag = (byte *)SUMA_calloc(TF->Ni*TF->Nj,sizeof(byte));
   if (!TF->cells) {  SUMA_SL_Crit("Failed to allocate"); SUMA_RETURNe; }
   TF->NewValueCallback = NewValueCallback;
   TF->NewValueCallbackData = cb_data;
   TF->TitLabelEVHandler = TitLabelEVHandler;
   TF->TitLabelEVHandlerData = TitLabelEVHandlerData;
   TF->CellEVHandler = CellEVHandler;
   TF->CellEVHandlerData = CellEVHandlerData;
   TF->type = type;
   TF->num_units = SUMA_NO_NUM_UNITS;
   switch (TF->type) {
      case SUMA_int:
      case SUMA_float:
         TF->num_value= (float *)SUMA_calloc(TF->Nj*TF->Ni, sizeof(float));
         break;
      case SUMA_string:
         TF->str_value= (char **)SUMA_malloc(TF->Nj*TF->Ni * sizeof(char *));
         for (i=0; i<TF->Nj*TF->Ni; ++i) TF->str_value[i] = NULL;
         break;
      default:
         SUMA_SL_Err("Comme tu es bete!");
         SUMA_RETURNe;
         break;  
   }
   /* An outer row column to keep the inner one from resizing with parent 
   YOU COULD HAVE SET XmNadjustLast to False, instead ....*/
   TF->rc = XtVaCreateManagedWidget ("rowcolumn",
      xmRowColumnWidgetClass, parent,
      XmNorientation , XmHORIZONTAL ,
      XmNpacking, XmPACK_TIGHT,
      XmNmarginHeight, 0,
      XmNmarginWidth, 0,
      NULL);
               
    
   /* Le row column to contain the table's columns*/
   rco = XtVaCreateManagedWidget ("rowcolumn",
      xmRowColumnWidgetClass, TF->rc,
      XmNorientation , XmVERTICAL ,
      XmNpacking, XmPACK_TIGHT,
      XmNnumColumns, 1,
      XmNmarginHeight, 0,
      XmNmarginWidth, 0, 
      NULL);

   /* must fill up row by row, each column is a separate bloody rc to allow for alignments */
   for (i=0; i<TF->Ni; ++i) {   /* for each row */
      rcc = XtVaCreateManagedWidget ("rowcolumn",
         xmRowColumnWidgetClass, rco,
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight, 0,
         XmNmarginHeight, 0,
         XmNmarginWidth, 0, 
         NULL);
      
      if (i == 0 && TF->HasColTit) { 
         /* This is left here to show that I 
            tried using different packing methods
            to get the title to appear centered with the cell entries below.
            That did nothing. Setting packing to tight in all cases and padding
            the titles to fit the cell entry widths works fine */
         XtVaSetValues (rcc, XmNpacking, XmPACK_TIGHT, NULL); 
      } else {
         XtVaSetValues (rcc, XmNpacking, XmPACK_TIGHT, NULL); 
      }
      
      for (j=0; j<TF->Nj; ++j) { /* for each column */
         n = j * TF->Ni + i;
         switch (SUMA_cellvariety(TF, n)) {
            case SUMA_ROW_TIT_CELL: /* row's title */
               if (LocalHead) 
                  fprintf( SUMA_STDERR,
                           "%s:\nAdding [%d %d] (%d) %s\n", 
                           FuncName, i, j, n, row_tit[i]);
               #if 0
                  TF->cells[n] = XtVaCreateManagedWidget(row_tit[i],  
                                                xmLabelWidgetClass, rcc, 
                                                NULL);
               #else
                  TF->cells[n] = 
                     XtVaCreateManagedWidget("table",   
                           xmTextFieldWidgetClass, rcc,
                           XmNvalue, row_tit[i],
                           XmNmarginHeight, 0,
                           XmNmarginWidth, 0,
                           XmNmarginTop, 0,
                           XmNmarginBottom, 0,
                           XmNmarginLeft, 0,
                           XmNmarginRight, 0,
                           XmNeditable, False, 
                           XmNshadowThickness , 0,          /* hide the border */
                           XmNcursorPositionVisible, False, /* hide the cursor */
                           XmNcolumns, strlen(row_tit[i]), 
                           NULL);
                  XtVaSetValues( TF->cells[n], 
                                 XmNfontList, 
                                 SUMAg_CF->X->TableTextFontList, NULL);

               #endif
               if (!TF->TitLabelEVHandlerData) 
                  cd = (XtPointer) TF; 
               else cd = (XtPointer)TF->TitLabelEVHandlerData;
               if (TF->TitLabelEVHandler) {
                  /* insert handler to catch clicks on titles */
                  XtInsertEventHandler( 
                     TF->cells[n] ,      /* handle events in title cell */
                     ButtonPressMask ,  /* button presses */
                     FALSE ,            /* nonmaskable events? */
                     TF->TitLabelEVHandler,  /* handler */
                     cd ,   /* client data */
                     XtListTail ) ; 
               }
               if (row_hint)  MCW_register_hint( TF->cells[n], row_hint[i] );
               if (row_help)  MCW_register_help( TF->cells[n], row_help[i] ) ;
               break;
               
            case SUMA_COL_TIT_CELL: /* column's title */
               if (LocalHead) 
                  fprintf( SUMA_STDERR,
                           "%s:\nAdding [%d %d] (%d) %s\n", 
                           FuncName, i, j, n, col_tit[j]);
               /* padd to fit cell entry fields*/
               if (i == 0 && j != 0 && TF->HasColTit) { 
                  titw = TF->cwidth[j]; 
                  /* set the margins to meet those of cell entries */
                  xmw = 5;
                  shad = 1;
               } else {
                  titw = TF->cwidth[j];
                  /* set the margins to meet those of Labels */
                  xmw = 0;
                  shad = 0;
               }
               #if 0
                  TF->cells[n] = XtVaCreateManagedWidget(tmp,  
                                                xmLabelWidgetClass, rcc,
                                                NULL);
               #else 
                  TF->cells[n] = 
                     XtVaCreateManagedWidget("table",   
                           xmTextFieldWidgetClass, rcc,
                           XmNvalue, col_tit[j],
                           XmNmarginHeight, 0,
                           XmNmarginWidth, xmw+shad,/*include shadow size 
                                                     of text entry cells*/
                           XmNmarginTop, 0,
                           XmNmarginBottom, 0,
                           XmNmarginLeft, 0,
                           XmNmarginRight, 0,
                           XmNeditable, False, 
                           XmNshadowThickness , 0,       /* hide the border */
                           XmNcursorPositionVisible, False, /* hide the cursor */
                              /* Keep these two out: See warning below ...
                              XmNfontList, SUMAg_CF->X->TableTextFontList,
                              XmNcolumns, titw,
                              */ 
                           NULL);
                  
                  /* WARNING: At least with LESSTIF:
                     The order in which one sets XmNfontList
                     and XmNcolumns matters. For example, adding:
                        XmNfontList, SUMAg_CF->X->TableTextFontList,
                        XmNcolumns, titw,
                     to XtVaCreateManagedWidget above does not 
                     work. You need to call XtVaSetValues for these
                     variables separately. 
                     
                     Also, simply adding the XtVaSetValues lines
                     below WITHOUT removing the two lines:
                        XmNfontList, SUMAg_CF->X->TableTextFontList,
                        XmNcolumns, titw,
                     from XtVaCreateManagedWidget does not work quite well.
                     
                     Also, note that the shadow adds to the size of the 
                     TextFieldWdiget, it does not eat 'into' it. So to
                     improve title alignment, I added the shadow thickness
                     into the margin width. */
                  
                  XtVaSetValues( TF->cells[n], 
                                 XmNfontList, 
                                 SUMAg_CF->X->TableTextFontList, NULL);
                  XtVaSetValues( TF->cells[n], XmNcolumns, titw, 
                           NULL);
                        
               #endif
               if (i == 0 && j != 0) { 
                  XtVaSetValues( TF->cells[n], 
                                 XmNalignment, XmALIGNMENT_BEGINNING, NULL);
               }
               
               /* insert handler to catch clicks on titles */
               if (!TF->TitLabelEVHandlerData) 
                  cd = (XtPointer) TF; 
               else cd = (XtPointer)TF->TitLabelEVHandlerData;
               if (TF->TitLabelEVHandler) {
                  /* insert handler to catch clicks on titles */
                  XtInsertEventHandler( 
                     TF->cells[n] ,      /* handle events in title cell */
                     ButtonPressMask ,  /* button presses */
                     FALSE ,            /* nonmaskable events? */
                     TF->TitLabelEVHandler,  /* handler */
                     cd ,   /* client data */
                     XtListTail ) ; 
               }                 
               if (col_hint)  MCW_register_hint( TF->cells[n], col_hint[j] );
               if (col_help)  MCW_register_help( TF->cells[n], col_help[j] ) ;
               break;
            case SUMA_ENTRY_CELL: /* entry cell */
               if (LocalHead) 
                  fprintf( SUMA_STDERR,
                           "%s:\nAdding [%d %d] (%d) entry cell\n", 
                           FuncName, i, j, n);
               TF->cells[n] = XtVaCreateManagedWidget(
                              "entry",  
                              xmTextFieldWidgetClass, rcc,
                              XmNuserData, (XTP_CAST)n,
                              XmNvalue, "-",
                              XmNmarginHeight, 0,
                              XmNmarginTop, 0,
                              XmNmarginBottom, 0,
                              XmNmarginWidth, 5, 
                              NULL);
                  XtVaSetValues( TF->cells[n], 
                                 XmNfontList, 
                                 SUMAg_CF->X->TableTextFontList, NULL);
               if (col_help || col_hint || row_help || row_hint)  {
                  if (!row_tit && !col_tit && Ni == 1 && Nj == 1) {
                     if (col_help) 
                        MCW_register_help( TF->cells[n], col_help[0]) ;
                     else if (row_help) 
                        MCW_register_help( TF->cells[n], row_help[0]) ;
                     if (col_hint) 
                        MCW_register_hint( TF->cells[n], col_hint[0]) ;
                     else if (row_hint) 
                        MCW_register_hint( TF->cells[n], row_hint[0]) ;
                  } else {
                     MCW_register_help( TF->cells[n], "Hints and help messages\n"
                                                      "are attached to table's\n"
                                                      "column and row titles.") ;
                     MCW_register_hint( TF->cells[n], "Hints and help messages\n"
                                                      "are attached to table's\n"
                                                      "column and row titles.") ;
                  }
               } 
               if (TF->cwidth[j] > 0) {  
                  XtVaSetValues(TF->cells[n], XmNcolumns, TF->cwidth[j], NULL); 
               }
               if (!TF->editable) { 
                  SUMA_SetCellEditMode(TF, i, j, 0);
               } else {
                  SUMA_SetCellEditMode(TF, i, j, 1);
               }

               /* insert handlers if any */
               if (!TF->CellEVHandlerData) cd = (XtPointer) TF; 
                  else cd = (XtPointer)TF->CellEVHandlerData;
               if (TF->CellEVHandler) {
                  /* insert handler to catch clicks on cells */
                  XtInsertEventHandler( 
                              TF->cells[n] ,      /* handle events in cell */
                              ButtonPressMask ,  /* button presses */
                              FALSE ,            /* nonmaskable events? */
                              TF->CellEVHandler,  /* handler */
                              cd ,   /* client data */
                              XtListTail ) ; 
               }                 
               break;
            default:
               SUMA_SL_Err("Bad cell type");
               SUMA_RETURNe;
               break;
         }     
      } /* for i */
   } /* for j */
   
   SUMA_RETURNe;
}

SUMA_CELL_VARIETY SUMA_cellvariety (SUMA_TABLE_FIELD *TF, int n)
{
   static char FuncName[]={"SUMA_cellvariety"};
   int i, j;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!TF) SUMA_RETURN(SUMA_ERROR_CELL);
   i = n % TF->Ni;
   j = n / TF->Ni;
   if (TF->HasColTit && i == 0) SUMA_RETURN(SUMA_COL_TIT_CELL);
   if (TF->HasRowTit && j == 0) SUMA_RETURN(SUMA_ROW_TIT_CELL);
   SUMA_RETURN(SUMA_ENTRY_CELL);
}

/*!
   \brief This function is called when mouse pointer leaves label field
   It only acts if  TF->modified 
*/
void SUMA_leave_TableField( Widget w , XtPointer client_data ,
                           XEvent * ev , Boolean * continue_to_dispatch )
{
   static char FuncName[]={"SUMA_leave_TableField"};
   SUMA_TABLE_FIELD *TF=NULL; 
   XLeaveWindowEvent * lev = (XLeaveWindowEvent *) ev ;
   XmAnyCallbackStruct cbs ;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   TF = (SUMA_TABLE_FIELD *)client_data ;
   if( lev->type != LeaveNotify || TF->cell_modified < 0) SUMA_RETURNe; 

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Leave notification.\n", FuncName);
   
   SUMA_TableF_cb_label_change( w , (XtPointer)TF , NULL ) ;

   SUMA_RETURNe;
}

/*!
   \brief This function is called when the label field is activated by the user

*/
void SUMA_TableF_cb_label_change (  Widget w, XtPointer client_data, 
                                    XtPointer call_data)
{
   static char FuncName[]={"SUMA_TableF_cb_label_change"};
   SUMA_TABLE_FIELD *TF=NULL;
   float val;
   int N_words = 0;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
   void *n=NULL;
   char *cs=NULL;
   int unt = SUMA_NO_NUM_UNITS;
   SUMA_Boolean DoCallBacks;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   /* make call to NewValue callback */
   TF = (SUMA_TABLE_FIELD *)client_data;
   
   DoCallBacks = NOPE;
   if (call_data) { 
      /* do the call backs if carriage return even if nothing is modified */
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: cbs->reason = %d (CR=%d)\n", 
                              FuncName, cbs->reason, XmCR_ACTIVATE);
      if (cbs->reason == XmCR_ACTIVATE) { 
         DoCallBacks = YUP;
      }
   }
   
   if (TF->cell_modified >= 0) {
      DoCallBacks = YUP;   /* do the callbacks even if no carriage return ... */
      if (TF->type == SUMA_int || TF->type == SUMA_float) {
         /* Check if the string is numerical, and get unit */
         XtVaGetValues (w, XmNvalue, &n, NULL);
         cs = (char *)n;
         if (!cs || !strlen(cs)) {/* empty cell, leave it alone */ 
            TF->cell_modified = -1;
            SUMA_LHv("empty %s", cs);
            SUMA_RETURNe;
         } else  {
            SUMA_COUNT_WORDS(cs, NULL, N_words);
            if (!N_words) { /* no harm, go back */ 
               TF->cell_modified = -1;
               SUMA_LHv("spacy %s", cs);
               SUMA_RETURNe; 
            }
         }
         unt = SUMA_NumStringUnits(cs, 0);
         if (SUMA_StringToNum(cs, (void *)&val, 1, 1) != 1) {
            SUMA_BEEP;
            /* bad syntax, reset value*/
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Bad syntax.\n", FuncName);
            SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                  "Bad value in text field", FuncName, 
                                  SMT_Error, SMA_Log);
            SUMA_TableF_SetString (TF);
         }else {
            if (TF->type == SUMA_int) {
               if (TF->num_value[TF->cell_modified] == (int)val &&
                   TF->num_units == unt ) { 
                  SUMA_LH("Same value"); 
                  TF->cell_modified = -1;
                  SUMA_RETURNe; 
               }
               TF->num_value[TF->cell_modified] = (int)val;
            } else if (TF->type == SUMA_float) {
               if (TF->num_value[TF->cell_modified] == val &&
                   TF->num_units == unt) { 
                  SUMA_LH("Same value"); 
                  TF->cell_modified = -1;
                  SUMA_RETURNe; 
               }
               TF->num_value[TF->cell_modified] = val;
            }
            SUMA_LH("Going to set string");
            TF->num_units = unt;
            SUMA_TableF_SetString (TF);
         }
      }
   } else {
      SUMA_LH("no cells modified");
   }

   if (DoCallBacks) {
      SUMA_LH("CallBacks ...");
      if (TF->cell_modified < 0) {
         /* figure out which one, user insists on value */
         SUMA_WHICH_CELL(TF, w, TF->cell_modified);
      }
      if (!TF->NewValueCallbackData) {
         SUMA_LH("No Callback data.");
         if (TF->NewValueCallback) TF->NewValueCallback((void*)TF);
      } else {
         SUMA_LH("Callback data.");
         if (TF->NewValueCallback) 
            TF->NewValueCallback(TF->NewValueCallbackData);
      }
   }
   
   TF->cell_modified = -1;
   SUMA_RETURNe;
}

/*!
   threshold value to scale value (position)
   NOTE val's value might change if it is outside the slider range
   or if it is negative when the slider is in |T| mode
*/
int SUMA_ThreshVal2ScalePos(SUMA_SurfaceObject *SO, float *val)
{
   static char FuncName[]={"SUMA_ThreshVal2ScalePos"};
   int min_v=0, max_v=0, cv=0, scl=0, dec=0;
   float ftmp;
   Widget w = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!SO) { SUMA_SL_Err("Null SO"); SUMA_RETURN(0); }
   w = SO->SurfCont->thr_sc;
   if (!w) { SUMA_SL_Err("Null widget"); SUMA_RETURN(0); }
   
   if (XtIsRealized(w)) {
      XtVaGetValues(w, XmNuserData, &dec, NULL);
      XtVaGetValues( w,
                     XmNmaximum, &max_v,
                     XmNminimum, &min_v,
                     XmNvalue, &cv,
                     XmNscaleMultiple, &scl,  
                     NULL);
   } else {
      SUMA_S_Note("Slider widget not realized"); SUMA_RETURN(0); 
   }
   if (*val < 0 && 
       SO->SurfCont->curColPlane->OptScl->ThrMode == SUMA_ABS_LESS_THAN) {
      *val = -*val;
   } 
   SUMA_LHv("min %d max %d scalemult %d decimals %d\nCurrent scale value %d\n", 
            min_v, max_v, scl, dec, cv);  

   /* what is the new slider value to be ?*/
   ftmp = *val * pow(10.0, dec);
   if (ftmp > 0) cv = (int) (ftmp+0.5);
   else cv = (int) (ftmp-0.5);              

   /* Now check on the new cv */
   if (cv < min_v) {
      cv = min_v;
      /* must update threshold value in options structure*/
      *val = (float)cv / pow(10.0, dec); 
   } else if (cv > max_v) {
      cv = max_v;
      *val = (float)cv / pow(10.0, dec); 
   }

   SUMA_RETURN(cv);
}

double SUMA_Pval2ThreshVal (SUMA_SurfaceObject *SO, double pval) {
   static char FuncName[]={"SUMA_Pval2ThreshVal"};
   float p[3], zval = -1.0;
   int statcode;
   double val = 0.0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) { 
      SUMA_SL_Err("NULL SO");
      SUMA_RETURN(val); 
   }  
   if (!SO->SurfCont || 
       !SO->SurfCont->thr_sc ||
       !SO->SurfCont->curColPlane ||
       !SO->SurfCont->curColPlane->dset_link) { 
      SUMA_SL_Err("NULL SurfCont");
      SUMA_RETURN(val); 
   }
     
   /* see if you can get the stat codes */
   if (!SUMA_GetDsetColStatAttr(  
            SO->SurfCont->curColPlane->dset_link, 
            SO->SurfCont->curColPlane->OptScl->tind, 
            &statcode,
            p, (p+1), (p+2))) {
      SUMA_LH("Error");        
   }else if (statcode) {
      SUMA_LHv("Have stats at sb %d\n"
               "statcode %d: %f %f %f\n", 
               SO->SurfCont->curColPlane->OptScl->tind,
               statcode, p[0], p[1], p[2]);
      SO->SurfCont->curColPlane->OptScl->ThreshStats[0] = pval;
      val = THD_pval_to_stat( pval , statcode , p  ) ;
      SUMA_LHv("Have pval of %f\n"
               "      val of %f\n", 
               SO->SurfCont->curColPlane->OptScl->ThreshStats[0],val);
   } else {
      /* no stats */
      SO->SurfCont->curColPlane->OptScl->ThreshStats[0] = -1.0;
      SO->SurfCont->curColPlane->OptScl->ThreshStats[1] = -1.0;
   }

   SUMA_RETURN(val);
}


/*!
   set the threshold bar when a new value is entered

	Crash note for suma in macosx_10.7_Intel_64 binaries.

With gcc 4.7.0 and 4.7.1, SUMA crashed when two hemispheres are loaded
and the threshold level is changed for one hemisphere. Because of LR yoking
the contralateral hemisphere is subject to the threshold change too.
This works fine on all other binaries but with binaries from SARUMAN which use
gcc 4.7.1, rather than the older gcc 4.2.1. The stack at the crash reliably pointed
to SUMA_SetScaleThr_one(), right after the call to SUMA_ThreshVal2ScalePos(). 
Debugging messages show that SUMA_ThreshVal2ScalePos() returns OK, but not even
a simple statement can get printed right afterwards. Valgrind showed no relevant
errors. 
	To reproduce the problem, use the toy surfaces under std10_toy. Something like
 suma -spec std.10both.mini.spec , followed by ctrl+s and a few threshold adjustments
 is enough to cause the crash.
 
*/
int SUMA_SetScaleThr_one(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *colp,
                          float *val, int setmen, int redisplay) 
{
   static char FuncName[]={"SUMA_SetScaleThr_one"};
   SUMA_SurfaceObject *curSO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int cv=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   
   if (!SO || !SO->SurfCont || !SO->SurfCont->SetThrScaleTable) SUMA_RETURN(0);
   if (colp && colp != SO->SurfCont->curColPlane) SUMA_RETURN(0);
   
   curSO = *(SO->SurfCont->curSOp);
   TF = SO->SurfCont->SetThrScaleTable;
   
   switch (TF->num_units) {
      case SUMA_P_VALUE_UNITS:
         if (LocalHead) 
               fprintf( SUMA_STDERR,
                        "%s:\nUnits in p value, transforming %f\n",
                        FuncName, *val);
         /* transform value from P to threshold value */
         *val = (float)SUMA_Pval2ThreshVal (SO, (double)*val);
         if (LocalHead) 
               fprintf( SUMA_STDERR,
                        "   to %f\n",
                        *val);
         /* reset the units of the table to reflect new value, 
            string containing new val is reset later on*/
         TF->num_units = SUMA_NO_NUM_UNITS;
         break;
      default:
         break;
   }
   
   cv = SUMA_ThreshVal2ScalePos (SO, val );

   if (LocalHead) 
      fprintf(SUMA_STDERR,
              "%s:\nChecksums, new value is %f, cv to be set to %d\n"
              "val now %f\n", 
              FuncName, TF->num_value[0], cv, *val);   

   /* TF->cell_modifed is not good when the call is made
   as a result of LR controller yoking. So don't bother using it.
   We only have one cell to be modified anyway. ZSS Sept 11 2012 */
   
   /* check on value */
   if (TF->num_value[0] != *val) { 
      /* a change in value (plateau effect) */
      TF->num_value[0] = *val;
      if (!setmen) setmen = 1;
   }
   
   if (setmen) {
      SUMA_INSERT_CELL_VALUE(TF, 0, 0, *val);
   }
   
   if (LocalHead) 
      fprintf( SUMA_STDERR,
               "%s:\nSet thresholdiation, new value is %f\n", 
               FuncName, *val);
   /* if value OK, set threshold bar*/
   SO->SurfCont->curColPlane->OptScl->ThreshRange[0] = *val;
   XtVaSetValues(SO->SurfCont->thr_sc,  
            XmNvalue, cv, 
            NULL);   

   SUMA_LHv("Colorize if necessary, refisplay=%d\n", redisplay);
   /* colorize if necessary */
   if ( redisplay == 0 ||
        (redisplay == 1 && !SO->SurfCont->curColPlane->OptScl->UseThr) ) { 
      SUMA_RETURN(0); 
   } /* nothing else to do */

   SUMA_LH("Colorize");
   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(0);
   }
   SUMA_LH("Remix redisplay");
   SUMA_RemixRedisplay(SO);

   SUMA_UpdateNodeLblField(SO);
   SUMA_UpdatePvalueField( SO,
                           SO->SurfCont->curColPlane->OptScl->ThreshRange[0]);
   SUMA_RETURN(1);  
}

int SUMA_SetScaleThr(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *colp,
                          float *val, int setmen, int redisplay) 
{
   static char FuncName[]={"SUMA_SetScaleThr"};
   SUMA_SurfaceObject *SOC=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   
   if (!SO) SUMA_RETURN(0);
   if (colp && colp != SO->SurfCont->curColPlane) SUMA_RETURN(0);
   colp = SO->SurfCont->curColPlane;
   
   if (!SUMA_SetScaleThr_one(SO, colp, val, setmen, redisplay)) SUMA_RETURN(0);
      
   /* do we have a contralateral SO and overlay? */
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      if (!SUMA_SetScaleThr_one(SOC, colpC, val, 1, redisplay)) SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}

void SUMA_cb_SetScaleThr(void *data) 
{
   static char FuncName[]={"SUMA_cb_SetScaleThr"};
   SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)data, *curSO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int cv, max_v, min_v, cell_mod = -1;
   float val;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   if (!SO) SUMA_RETURNe;
   curSO = *(SO->SurfCont->curSOp);
   TF = SO->SurfCont->SetThrScaleTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   val = TF->num_value[TF->cell_modified];
   
   SUMA_SetScaleThr(SO, NULL, &val, 0, 1);
   
   SUMA_RETURNe;
}

/*!
   \brief Sends the Focus triangle  when new value is entered
*/
void SUMA_TriInput (void *data)
{
   static char FuncName[]={"SUMA_TriInput"};
   SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)data, *curSO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   float fv3[3];
   char str[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   curSO = *(SO->SurfCont->curSOp);
   TF = SO->SurfCont->FaceTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   i = n % TF->Ni;
   j = n / TF->Ni;
   
   if ((int)TF->num_value[n] < 0 || (int)TF->num_value[n] >= curSO->N_FaceSet) {
      SUMA_SLP_Err("Triangle index n must be positive\n"
                   "and less than the number of nodes \n"
                   "forming the surface.\n");
      TF->num_value[n] = SO->SelectedFaceSet;
      SUMA_TableF_SetString (TF);
      TF->cell_modified = -1;
      SUMA_RETURNe;
   }

   switch (j) {
      case 1:
         XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s, Tri = %d\n", 
                  FuncName, i, j, (char *)cv, (int)TF->num_value[n]);
         }

         /* look for a viewer that is showing this surface and has this surface in focus*/
         for (i=0; i<SUMAg_N_SVv; ++i) {
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, i);
            if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
               /* is this viewer showing curSO ? */
               if (SUMA_isVisibleSO(&(SUMAg_SVv[i]), SUMAg_DOv, curSO)) {
                  if ((SUMAg_DOv[SUMAg_SVv[i].Focus_SO_ID].OP) == curSO) {
                     SUMA_JumpFocusFace((char *)cv, (void *)(&(SUMAg_SVv[i])));
                  }
               }
            }
         }
         break;
      default:
         SUMA_SL_Err("Should not get this input");
         break;
   }
   SUMA_RETURNe;  
}
/*!
   \brief Sends the node flying when new value is entered
*/
void SUMA_NodeInput (void *data)
{
   static char FuncName[]={"SUMA_NodeInput"};
   SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)data, *curSO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   float fv3[3];
   char str[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   curSO = *(SO->SurfCont->curSOp);
   TF = SO->SurfCont->NodeTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   i = n % TF->Ni;
   j = n / TF->Ni;
   
   if ((int)TF->num_value[n] < 0 || (int)TF->num_value[n] >= curSO->N_Node) {
      SUMA_SLP_Err("Node index must be positive and \n"
                   "less than the number of nodes \n"
                   "forming the surface.\n");
      TF->num_value[n] = SO->SelectedNode;
      SUMA_TableF_SetString (TF);
      TF->cell_modified = -1;
      SUMA_RETURNe;
   }
   
   switch (j) {
      case 1:
         XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s, node = %d\n", 
                  FuncName, i, j, (char *)cv, (int)TF->num_value[n]);
         }

         /* look for a viewer that is showing this surface and has this surface in focus*/
         for (i=0; i<SUMAg_N_SVv; ++i) {
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, i);
            if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
               /* is this viewer showing curSO ? */
               if (SUMA_isVisibleSO(&(SUMAg_SVv[i]), SUMAg_DOv, curSO)) {
                  if ((SUMAg_DOv[SUMAg_SVv[i].Focus_SO_ID].OP) == curSO) {
                        SUMA_JumpIndex((char *)cv, (void *)(&(SUMAg_SVv[i])));
                  }
               }
            }
         }
         break;
      default:
         SUMA_SL_Err("Should not get this input");
         break;
   }
   SUMA_RETURNe;  
}

/*!
   \brief Sends the cross hair flying when new value is entered
*/
void SUMA_XhairInput (void* data)
{
   static char FuncName[]={"SUMA_XhairInput"};
   SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)data, *curSO = NULL;
   SUMA_SurfaceViewer *sv=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   float fv3[3];
   char str[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   curSO = *(SO->SurfCont->curSOp);
   TF = SO->SurfCont->XhairTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   SUMA_LH("Cell modified, modifying ...");
   n = TF->cell_modified;
   i = n % TF->Ni;
   j = n / TF->Ni;
   XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s\n", 
               FuncName, i, j, (char *)cv);
   }
   /* Now parse that string into 3 numbers */
   if (SUMA_StringToNum ((char *)cv, (void *)fv3, 3,1) != 3) {
      SUMA_BEEP;
      str[0]='\0';
   } else {
      SUMA_XHAIR_STRING(fv3, str);
   }
   XtVaSetValues(TF->cells[n], XmNvalue, str, NULL);
   
   /* look for a viewer that is showing this surface */
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, i);
      if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
         /* is this viewer showing curSO ? */
         sv = &(SUMAg_SVv[i]);
         if (SUMA_isVisibleSO(sv, SUMAg_DOv, curSO)) {
            /* is this a new coordinate? 
               Avoid calls due to cosmetic text changes */
            if (sv->Ch->c[0] != fv3[0] || 
                sv->Ch->c[1] != fv3[1] || sv->Ch->c[2] != fv3[2]) {
               if (LocalHead) fprintf(SUMA_STDERR, 
                                      "%s: Calling for jump to %s\n", 
                                      FuncName, str);
               SUMA_JumpXYZ(str, (void *)(&(SUMAg_SVv[i])));
            }
         }
      }
   }
   SUMA_RETURNe;
}

#if 0       /* Kill after a while. ZSS: July 11 2012 */
/*!
   \brief Sets the range values when new value is input to the table 
*/
void SUMA_SetRangeValueOld_one (void *data)
{
   static char FuncName[]={"SUMA_SetRangeValueOld_one"};
   SUMA_SRV_DATA *srvd=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   SUMA_OVERLAYS *ColPlane = NULL;
   SUMA_Boolean NewDisp = NOPE, isCur = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   srvd = (SUMA_SRV_DATA *)data;
   SO = srvd->SO;
   TF = SO->SurfCont->SetRangeTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   
   if (!srvd->colp && !SO->SurfCont->curColPlane) SUMA_RETURNe;
   if (!srvd->colp) {
      ColPlane = SO->SurfCont->curColPlane;
   } else {
      ColPlane = srvd->colp;
   }
   if (ColPlane == SO->SurfCont->curColPlane) {
      isCur = 1;
   } else isCur = 0;
   
   n = TF->cell_modified;
   i = n % TF->Ni;
   j = n / TF->Ni;
   XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s\n", 
                           FuncName, i, j, (char *)cv);
   }
   NewDisp = NOPE;
   if (1) {
   /* What are we dealing with ? */
   switch (i) {
      case 1:  /* That's the Int. range */
         SUMA_LHv("Setting Int. Range, isCur=%d\n", isCur);
         if (j == 1) {
            if (ColPlane->SymIrange) {
               ColPlane->OptScl->IntRange[0] = -fabs((double)TF->num_value[n]);
               ColPlane->OptScl->IntRange[1] = -ColPlane->OptScl->IntRange[0];
               if (isCur) {
                  SUMA_INSERT_CELL_VALUE(TF, 1, 1, 
                                         ColPlane->OptScl->IntRange[0]);
                  SUMA_INSERT_CELL_VALUE(TF, 1, 2, 
                                         ColPlane->OptScl->IntRange[1]);
               }
            } else {
               if (TF->num_value[n] > ColPlane->OptScl->IntRange[1]) {
                  SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->IntRange[0];
                  SUMA_SLP_Err("Lower bound > Upper bound!");
                  if (isCur) SUMA_TableF_SetString(TF);
               } else {
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,"%s: IntRange[0] was %f, will be %f\n",
                              FuncName, ColPlane->OptScl->IntRange[0], 
                              TF->num_value[n]);
                  ColPlane->OptScl->IntRange[0] = TF->num_value[n];
               }
            }
         } else if (j==2) {
             if (ColPlane->SymIrange) {
               ColPlane->OptScl->IntRange[1] = fabs((double)TF->num_value[n]);
               ColPlane->OptScl->IntRange[0] = -ColPlane->OptScl->IntRange[1];
               if (isCur) {
                  SUMA_INSERT_CELL_VALUE(TF, 1, 1, 
                                         ColPlane->OptScl->IntRange[0]);
                  SUMA_INSERT_CELL_VALUE(TF, 1, 2, 
                                         ColPlane->OptScl->IntRange[1]);
               }
             } else {
               if (TF->num_value[n] < ColPlane->OptScl->IntRange[0]) {
                  SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->IntRange[1];
                  SUMA_SLP_Err("Upper bound < Lower bound!");
                  if (isCur) SUMA_TableF_SetString(TF);
               } else {
                  ColPlane->OptScl->IntRange[1] = TF->num_value[n];
               }
            }
         } else { SUMA_SL_Err("What's going on John ?"); }
         if (isCur && ColPlane->ShowMode > 0) NewDisp = YUP;
         break;
      case 2:  /* That's the Brt.. range */
         SUMA_LH("Setting Brt. Range");
         if (j == 1) {
            if (TF->num_value[n] > ColPlane->OptScl->BrightRange[1]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightRange[0];
               SUMA_SLP_Err("Lower bound > Upper bound!");
               if (isCur) SUMA_TableF_SetString(TF);
            } else {
               ColPlane->OptScl->BrightRange[0] = TF->num_value[n];
            }
         } else if (j==2) {
            if (TF->num_value[n] < ColPlane->OptScl->BrightRange[0]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightRange[1];
               SUMA_SLP_Err("Upper bound < Lower bound!");
               if (isCur) SUMA_TableF_SetString(TF);
            } else {
               ColPlane->OptScl->BrightRange[1] = TF->num_value[n];
            }
         } else { SUMA_SL_Err("What's going on Ron ?"); }
         if (isCur && ColPlane->OptScl->UseBrt) NewDisp = YUP;
         break;
      case 3:  /* That's the Brt. Map Range */
         SUMA_LH("Setting BrtMap. Range");
         if (j == 1) {
            if (TF->num_value[n] > ColPlane->OptScl->BrightMap[1]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightMap[0];
               SUMA_SLP_Err("Lower bound > Upper bound!");
               if (isCur) SUMA_TableF_SetString(TF);
            } else if (TF->num_value[n] < 0) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightMap[0];
               SUMA_SLP_Err("Value must be >= 0");
               if (isCur) SUMA_TableF_SetString(TF);
            } else {
               ColPlane->OptScl->BrightMap[0] = TF->num_value[n];
            }
         } else if (j==2) {
            if (TF->num_value[n] < ColPlane->OptScl->BrightMap[0]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightMap[1];
               SUMA_SLP_Err("Upper bound < Lower bound!");
               if (isCur) SUMA_TableF_SetString(TF);
            } else {
               ColPlane->OptScl->BrightMap[1] = TF->num_value[n];
            }
         } else { SUMA_SL_Err("What's going on Mon ?"); }
         if (isCur && ColPlane->OptScl->UseBrt) NewDisp = YUP;
         break;
      case 4:  /* That's the coordinate bias Range */
         SUMA_LH("Setting CoordBias. Range");
         if (j == 1) {
            if (TF->num_value[n] > ColPlane->OptScl->CoordBiasRange[1]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->CoordBiasRange[0];
               SUMA_SLP_Err("Lower bound > Upper bound!");
               if (isCur) SUMA_TableF_SetString(TF);
            } else { /* OK */
               ColPlane->OptScl->CoordBiasRange[0] = TF->num_value[n];
            }
         } else if (j==2) {
            if (TF->num_value[n] < ColPlane->OptScl->CoordBiasRange[0]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->CoordBiasRange[1];
               SUMA_SLP_Err("Upper bound < Lower bound!");
               if (isCur) SUMA_TableF_SetString(TF);
            } else { /* OK */
               ColPlane->OptScl->CoordBiasRange[1] = TF->num_value[n];
            }
         } else { SUMA_SL_Err("What's going on Hon ?"); }
         if (isCur) NewDisp = YUP; 
               /* You might want to disable this feature if the ColPlane 
                  is not shown */
         break;
      default:
         SUMA_SL_Err("You make me sick");
         break;
   }
   }
   
   /* Now, you need to redraw the deal */
   if (NewDisp) {
      SUMA_ColorizePlane(SO->SurfCont->curColPlane);
      SUMA_RemixRedisplay(SO);
   }   
   
   /* update the Xhair Info block */
   if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_None) {
      SUMA_UpdateNodeNodeField(SO);    
   }
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
}

void SUMA_SetRangeValueOld (void *data)
{
   static char FuncName[]={"SUMA_SetRangeValueOld"};
   SUMA_SRV_DATA srvdC, *srvd=NULL;
   SUMA_SurfaceObject *SO=NULL, *SOC=NULL;
   SUMA_OVERLAYS *colpC=NULL, *colp=NULL;
   int n=-1,i=-1,j=-1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch range \n", FuncName);
   }
   if (!(srvd = (SUMA_SRV_DATA *)data)) SUMA_RETURNe;
   SO = srvd->SO; colp = srvd->colp;
   if (!colp) colp = SO->SurfCont->curColPlane;
   /* save which cell was modified because that will get reset
      after 1st call to SUMA_SetRangeValueOld_one */
   n = SO->SurfCont->SetRangeTable->cell_modified;
   SUMA_LHv("Initial n %d\n", n);
   SUMA_SetRangeValueOld_one (data);
   
   
   /* do we have a contralateral SO and overlay? */
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n"
                   "cell modified: n = %d\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label),
                   n);
      srvdC.SO = SOC; srvdC.colp = colpC;
      
      
      if (colpC == SOC->SurfCont->curColPlane) {
         if (n >= 0) {
            i = n % SO->SurfCont->SetRangeTable->Ni;
            j = n / SO->SurfCont->SetRangeTable->Ni; 
            SUMA_LHv("Mode 1, n=%d, i=%d, j=%d\n", n, i, j);
            /* modify the cells in question */
            switch(i) {
               case 1:
                  SUMA_INSERT_CELL_VALUE(SOC->SurfCont->SetRangeTable, i, 1, 
                                SO->SurfCont->curColPlane->OptScl->IntRange[0]);
                  SUMA_INSERT_CELL_VALUE(SOC->SurfCont->SetRangeTable, i, 2, 
                                SO->SurfCont->curColPlane->OptScl->IntRange[1]);
                  break;
               case 2:
                  SUMA_INSERT_CELL_VALUE(SOC->SurfCont->SetRangeTable, i, 1, 
                              SO->SurfCont->curColPlane->OptScl->BrightRange[0]);
                  SUMA_INSERT_CELL_VALUE(SOC->SurfCont->SetRangeTable, i, 2, 
                              SO->SurfCont->curColPlane->OptScl->BrightRange[1]);
                  break;
               case 3:
                  SUMA_INSERT_CELL_VALUE(SOC->SurfCont->SetRangeTable, i, 1, 
                              SO->SurfCont->curColPlane->OptScl->BrightMap[0]);
                  SUMA_INSERT_CELL_VALUE(SOC->SurfCont->SetRangeTable, i, 2, 
                              SO->SurfCont->curColPlane->OptScl->BrightMap[1]);
                  break;
               case 4:
                  SUMA_INSERT_CELL_VALUE(SOC->SurfCont->SetRangeTable, i, 1, 
                           SO->SurfCont->curColPlane->OptScl->CoordBiasRange[0]);
                  SUMA_INSERT_CELL_VALUE(SOC->SurfCont->SetRangeTable, i, 2, 
                           SO->SurfCont->curColPlane->OptScl->CoordBiasRange[1]);
                  break; 
            }
            SOC->SurfCont->SetRangeTable->cell_modified = n; 
            SUMA_SetRangeValueOld_one (&srvdC);
         } else {
            SUMA_S_Warn("Don't know why this can be");
         }
      } else {
         /* just update values in contralateral baby */
         if (n > 0) {
            i = n % SO->SurfCont->SetRangeTable->Ni;
            j = n / SO->SurfCont->SetRangeTable->Ni; 
            SUMA_LHv("Mode 2, n=%d, i=%d, j=%d\n", n, i, j);
            switch(i) {
               case 1:
                  colpC->OptScl->IntRange[0] = 
                     SO->SurfCont->curColPlane->OptScl->IntRange[0];
                  colpC->OptScl->IntRange[1] = 
                     SO->SurfCont->curColPlane->OptScl->IntRange[1];
                  break;
               case 2:
                  colpC->OptScl->BrightRange[0] = 
                     SO->SurfCont->curColPlane->OptScl->BrightRange[0];
                  colpC->OptScl->BrightRange[1] = 
                     SO->SurfCont->curColPlane->OptScl->BrightRange[1];
                  break;
               case 3:
                  colpC->OptScl->BrightMap[0] = 
                     SO->SurfCont->curColPlane->OptScl->BrightMap[0];
                  colpC->OptScl->BrightMap[1] = 
                     SO->SurfCont->curColPlane->OptScl->BrightMap[1];
                  break;
               case 4:
                  colpC->OptScl->CoordBiasRange[0] = 
                     SO->SurfCont->curColPlane->OptScl->CoordBiasRange[0];
                  colpC->OptScl->CoordBiasRange[1] = 
                     SO->SurfCont->curColPlane->OptScl->CoordBiasRange[1];
                  break;
            }
         } else {
            SUMA_LHv("Mode 3, n=%d\n", n);
            /* Just equate all of them */
            colpC->OptScl->IntRange[0] = 
               SO->SurfCont->curColPlane->OptScl->IntRange[0];
            colpC->OptScl->IntRange[1] = 
               SO->SurfCont->curColPlane->OptScl->IntRange[1];
            colpC->OptScl->BrightRange[0] = 
               SO->SurfCont->curColPlane->OptScl->BrightRange[0];
            colpC->OptScl->BrightRange[1] = 
               SO->SurfCont->curColPlane->OptScl->BrightRange[1];
            colpC->OptScl->BrightMap[0] = 
               SO->SurfCont->curColPlane->OptScl->BrightMap[0];
            colpC->OptScl->BrightMap[1] = 
               SO->SurfCont->curColPlane->OptScl->BrightMap[1];
            colpC->OptScl->CoordBiasRange[0] = 
               SO->SurfCont->curColPlane->OptScl->CoordBiasRange[0];
            colpC->OptScl->CoordBiasRange[1] = 
               SO->SurfCont->curColPlane->OptScl->CoordBiasRange[1];
         }
      }
   }
   
   SUMA_RETURNe; 
}
#endif

#define SETMEN (setmen > 1 || (setmen && isCur))
#define REDISP (redisplay > 1 || (redisplay && NewDisp))

int SUMA_SetRangeValueNew_one(SUMA_SurfaceObject *SO, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset) 
{
   static char FuncName[]={"SUMA_SetRangeValueNew_one"};
   int NewDisp=0, isCur=0;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_TABLE_FIELD *TF=NULL;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->SurfCont || 
       !SO->SurfCont->SetRangeTable || !reset) {
      SUMA_RETURN(0);
   }
   if (!colp) colp = SO->SurfCont->curColPlane;
   
   if (colp && colp == SO->SurfCont->curColPlane) {
      isCur=YUP;
   } else {
      isCur=NOPE;
   }
   
   if (!colp) {
      SUMA_RETURN(0); /* not much to do */
   }
   
   if (colp->SymIrange) { /* must set table fields in this case */
      if (!setmen) setmen = 1;
   }
   
   TF = SO->SurfCont->SetRangeTable;
   if (!TF) setmen = 0; /* can't set nothing */
   
   
   NewDisp = NOPE;
   /* What are we dealing with ? */
   switch (row) {
      case 1:  /* That's the Int. range */
         SUMA_LHv("Setting Int. Range, isCur=%d [%d %d] sym %d\n", 
                   isCur, row, col, colp->SymIrange);
         if (col == 1) { /* the min value */
            if (colp->SymIrange) {
               colp->OptScl->IntRange[0] = -fabs((double)v1);
               colp->OptScl->IntRange[1] = -colp->OptScl->IntRange[0];
               if (SETMEN) {
                  SUMA_LHv("Inserting cell values %f and %f\n", 
                           colp->OptScl->IntRange[0], colp->OptScl->IntRange[1]);
                  SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                         colp->OptScl->IntRange[0]);
                  SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                         colp->OptScl->IntRange[1]);
               }
            } else {
               if (v1 > colp->OptScl->IntRange[1]) {
                  *reset = colp->OptScl->IntRange[0];
                  SUMA_RETURN(-1); 
               } else {
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,"%s: IntRange[0] was %f, will be %f\n",
                              FuncName, colp->OptScl->IntRange[0], 
                              v1);
                  colp->OptScl->IntRange[0] = v1;
                  if (SETMEN) {
                     SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                         colp->OptScl->IntRange[0]);
                  }  
               }
            }
         } else if (col==2) {
             if (colp->SymIrange) {
               colp->OptScl->IntRange[1] = fabs((double)v1);
               colp->OptScl->IntRange[0] = -colp->OptScl->IntRange[1];
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                         colp->OptScl->IntRange[0]);
                  SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                         colp->OptScl->IntRange[1]);
               }
             } else {
               if (v1 < colp->OptScl->IntRange[0]) {
                  *reset = colp->OptScl->IntRange[1];
                  SUMA_RETURN(-2); 
               } else {
                  colp->OptScl->IntRange[1] = v1;
                  if (SETMEN) {
                     SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                         colp->OptScl->IntRange[1]);
                  }
               }
            }
         } else if (col==-1) {
            if (v1 > v2) {
               SUMA_S_Errv("Bad range %f %f\n", v1, v2);
               SUMA_RETURN(-3);
            } else {
               colp->OptScl->IntRange[0] = v1;
               colp->OptScl->IntRange[1] = v2;
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                         colp->OptScl->IntRange[0]);
                  SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                         colp->OptScl->IntRange[1]);
               }
            }  
         } else { SUMA_SL_Err("What's going on John ?"); }
         if (isCur && colp->ShowMode > 0) NewDisp = YUP;
         break;
      case 2:  /* That's the Brt.. range */
         SUMA_LH("Setting Brt. Range");
         if (col == 1) {
            if (v1 > colp->OptScl->BrightRange[1]) {
               *reset = colp->OptScl->BrightRange[0];
               SUMA_RETURN(-1); 
            } else {
               colp->OptScl->BrightRange[0] = v1;
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                      colp->OptScl->BrightRange[0]);
               }
            }
         } else if (col==2) {
            if (v1 < colp->OptScl->BrightRange[0]) {
               *reset = colp->OptScl->BrightRange[1];
               SUMA_RETURN(-2); 
            } else {
               colp->OptScl->BrightRange[1] = v1;
               if (SETMEN) {
                     SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                         colp->OptScl->BrightRange[1]);
               }
            }
         } else if (col == -1) {
            if (v1 > v2) {
               SUMA_S_Errv("Bad range %f %f\n", v1, v2);
               SUMA_RETURN(-3);
            } else {
               colp->OptScl->BrightRange[0] = v1;
               colp->OptScl->BrightRange[1] = v2;
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                         colp->OptScl->BrightRange[0]);
                  SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                         colp->OptScl->BrightRange[1]);
               }
            }  
         } else { SUMA_SL_Err("What's going on Ron ?"); }
         if (isCur && colp->OptScl->UseBrt) NewDisp = YUP;
         break;
      case 3:  /* That's the Brt. Map Range */
         SUMA_LH("Setting BrtMap. Range");
         if (col == 1) {
            if (v1 > colp->OptScl->BrightMap[1]) {
               *reset = colp->OptScl->BrightMap[0];
               SUMA_RETURN(-1); 
            } else if (v1 < 0) {
               *reset = colp->OptScl->BrightMap[0];
               SUMA_RETURN(-1);
            } else {
               colp->OptScl->BrightMap[0] = v1;
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                      colp->OptScl->BrightMap[0]);
               }
            }
         } else if (col==2) {
            if (v1 < colp->OptScl->BrightMap[0]) {
               *reset = colp->OptScl->BrightMap[1]; 
               SUMA_RETURN(-2); 
            } else {
               colp->OptScl->BrightMap[1] = v1;
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                      colp->OptScl->BrightMap[1]);
               }
            }
         } else if (col == -1) {
            if (v1 > v2 || v1 < 0) {
               SUMA_S_Errv("Bad range %f %f\n", v1, v2);
               SUMA_RETURN(-3);
            } else {
               colp->OptScl->BrightMap[0] = v1;
               colp->OptScl->BrightMap[1] = v2;
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                         colp->OptScl->BrightMap[0]);
                  SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                         colp->OptScl->BrightMap[1]);
               }
            }  
         } else { SUMA_SL_Err("What's going on Mon ?"); }
         if (isCur && colp->OptScl->UseBrt) NewDisp = YUP;
         break;
      case 4:  /* That's the coordinate bias Range */
         SUMA_LH("Setting CoordBias. Range");
         if (col == 1) {
            if (v1 > colp->OptScl->CoordBiasRange[1]) {
               *reset = colp->OptScl->CoordBiasRange[0]; 
               SUMA_RETURN(-1);
            } else { /* OK */
               colp->OptScl->CoordBiasRange[0] = v1;
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                      colp->OptScl->CoordBiasRange[0]);
               }
            }
         } else if (col==2) {
            if (v1 < colp->OptScl->CoordBiasRange[0]) {
                *reset = colp->OptScl->CoordBiasRange[1]; 
               SUMA_RETURN(-2);
            } else { /* OK */
               colp->OptScl->CoordBiasRange[1] = v1;
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                      colp->OptScl->CoordBiasRange[1]);
               }
            }
         } else if (col == -1) {
            if (v1 > v2) {
               SUMA_S_Errv("Bad range %f %f\n", v1, v2);
               SUMA_RETURN(-3);
            } else {
               colp->OptScl->CoordBiasRange[0] = v1;
               colp->OptScl->CoordBiasRange[1] = v2;
               if (SETMEN) {
                  SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                         colp->OptScl->CoordBiasRange[0]);
                  SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                         colp->OptScl->CoordBiasRange[1]);
               }
            }  
         } else { SUMA_SL_Err("What's going on Hon ?"); }
         if (isCur) NewDisp = YUP; 
               /* You might want to disable this feature if the colp 
                  is not shown */
         break;
      default:
         SUMA_SL_Err("You make me sick");
         break;
   }
      
   /* Now, you need to redraw the deal */
   if (REDISP) {
      SUMA_ColorizePlane(SO->SurfCont->curColPlane);
      SUMA_RemixRedisplay(SO);
   }   
   
   /* update the Xhair Info block */
   if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_None) {
      SUMA_UpdateNodeNodeField(SO);    
   }
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURN(1);
}

int SUMA_SetRangeValueNew (SUMA_SurfaceObject *SO, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset)
{
   static char FuncName[]={"SUMA_SetRangeValueNew"};
   SUMA_SurfaceObject *SOC=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   int NewDisp=0, an=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch range \n", FuncName);
   }
   
   an = SUMA_SetRangeValueNew_one(SO, colp, row, col, 
                                  v1, v2, setmen, 
                                  redisplay, reset);
   if (an <= 0) SUMA_RETURN(an);
   
   /* do we have a contralateral SO and overlay? */
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      an = SUMA_SetRangeValueNew_one(SOC, colpC, row, col, 
                                     v1, v2, 1, 
                                     redisplay, reset);
   }
   
   SUMA_RETURN(an);
}


void SUMA_cb_SetRangeValue (void *data) 
{
   static char FuncName[]={"SUMA_cb_SetRangeValue"};
   SUMA_SRV_DATA srvdC, *srvd=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_OVERLAYS *colp=NULL;
   int n=-1,row=-1,col=-1, an=0;
   float reset = 0.0;
   void *cv=NULL; 
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch range \n", FuncName);
   }
   if (!(srvd = (SUMA_SRV_DATA *)data)) SUMA_RETURNe;
   SO = srvd->SO; colp = srvd->colp;
   if (!SO) SUMA_RETURNe;
   if (!colp) colp = SO->SurfCont->curColPlane;
   
   TF = SO->SurfCont->SetRangeTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   row = n % TF->Ni;
   col = n / TF->Ni;
   XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s\n", 
                           FuncName, row, col, (char *)cv);
   }
   
   an = SUMA_SetRangeValueNew(SO, colp, row, col,
                          TF->num_value[n], 0.0,
                          0, 1, &reset);
   if (an < 0) {
      if (an == -1 || an == -2) {
         SUMA_BEEP; 
         TF->num_value[n] = reset;
         SUMA_TableF_SetString(TF);      
         if (an == -1) { SUMA_SLP_Err("Lower bound > Upper bound!"); }
         else { SUMA_SLP_Err("Upper bound < Lower bound!"); }
      } else {
         SUMA_S_Err("Erriosity");
      }
   }
   
   SUMA_RETURNe;
}

int SUMA_SetClustValue_one(SUMA_SurfaceObject *SO, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset) 
{
   static char FuncName[]={"SUMA_SetClustValue_one"};
   int NewDisp=0, isCur=0;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_TABLE_FIELD *TF=NULL;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->SurfCont || 
       !SO->SurfCont->SetClustTable || !reset) {
      SUMA_RETURN(0);
   }
   if (!colp) colp = SO->SurfCont->curColPlane;
   
   if (colp && colp == SO->SurfCont->curColPlane) {
      isCur=YUP;
   } else {
      isCur=NOPE;
   }
   
   if (!colp) {
      SUMA_RETURN(0); /* not much to do */
   }
   
   
   TF = SO->SurfCont->SetClustTable;
   if (!TF) setmen = 0; /* can't set nothing */
      
   NewDisp = NOPE;
   /* What are we dealing with ? */
   switch (row) {
      case 1:  /* That's the Int. range */
         SUMA_LHv("Setting Clust params isCur=%d [%d %d]\n", 
                   isCur, row, col);
         if (col == 1) { /* the DistLim value */
            if (colp->OptScl->Clusterize &&
                colp->OptScl->ClustOpt->DistLim != v1) {
               colp->OptScl->RecomputeClust = 1;   
            }
            colp->OptScl->ClustOpt->DistLim = v1;
            if (SETMEN) {
               SUMA_LHv("Inserting cell value %f \n", 
                        colp->OptScl->ClustOpt->DistLim);
               SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                      colp->OptScl->ClustOpt->DistLim);
            }
         } else if (col==2) {
            if (colp->OptScl->Clusterize &&
                colp->OptScl->ClustOpt->AreaLim != v1) {
               colp->OptScl->RecomputeClust = 1;   
            }
            colp->OptScl->ClustOpt->AreaLim = v1;
            if (SETMEN) {
               SUMA_LHv("Inserting cell value %f \n", 
                        colp->OptScl->ClustOpt->AreaLim);
               SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                      colp->OptScl->ClustOpt->AreaLim);
            }
         } else if (col==-1) {
            if (colp->OptScl->Clusterize &&
                (colp->OptScl->ClustOpt->DistLim != v1 ||
                 colp->OptScl->ClustOpt->AreaLim != v2)) {
               colp->OptScl->RecomputeClust = 1;   
            }
            colp->OptScl->ClustOpt->DistLim = v1;
            colp->OptScl->ClustOpt->AreaLim = v2;
            if (SETMEN) {
               SUMA_LHv("Inserting cell values %f %f\n", 
                        colp->OptScl->ClustOpt->DistLim,
                        colp->OptScl->ClustOpt->AreaLim);
               SUMA_INSERT_CELL_VALUE(TF, row, 1, 
                                      colp->OptScl->ClustOpt->DistLim);
               SUMA_INSERT_CELL_VALUE(TF, row, 2, 
                                      colp->OptScl->ClustOpt->AreaLim);
            }
         } else { SUMA_SL_Err("What's going on Jane ?"); }
         if (isCur && colp->ShowMode > 0 && colp->OptScl->Clusterize) 
                                                            NewDisp = YUP;
         break;
      default:
         SUMA_SL_Err("You make me sick. What's that you're eating?");
         break;
   }
      
   /* Now, you need to redraw the deal */
   if (REDISP) {
      SUMA_ColorizePlane(SO->SurfCont->curColPlane);
      SUMA_RemixRedisplay(SO);
   }   
   
   /* update the Xhair Info block */
   if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_None) {
      SUMA_UpdateNodeNodeField(SO);    
   }
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURN(1);
}

int SUMA_SetClustValue (SUMA_SurfaceObject *SO, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset)
{
   static char FuncName[]={"SUMA_SetClustValue"};
   SUMA_SurfaceObject *SOC=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   int NewDisp=0, an=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch clust params \n", FuncName);
   }
   
   an = SUMA_SetClustValue_one(SO, colp, row, col, 
                                  v1, v2, setmen, 
                                  redisplay, reset);
   if (an <= 0) SUMA_RETURN(an);
   
   /* do we have a contralateral SO and overlay? */
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      an = SUMA_SetClustValue_one(SOC, colpC, row, col, 
                                     v1, v2, 1, 
                                     redisplay, reset);
   }
   
   SUMA_RETURN(an);
}


void SUMA_cb_SetClustValue (void *data) 
{
   static char FuncName[]={"SUMA_cb_SetClustValue"};
   SUMA_SRV_DATA srvdC, *srvd=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_OVERLAYS *colp=NULL;
   int n=-1,row=-1,col=-1, an=0;
   float reset = 0.0;
   void *cv=NULL; 
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch range \n", FuncName);
   }
   if (!(srvd = (SUMA_SRV_DATA *)data)) SUMA_RETURNe;
   SO = srvd->SO; colp = srvd->colp;
   if (!SO) SUMA_RETURNe;
   if (!colp) colp = SO->SurfCont->curColPlane;
   
   TF = SO->SurfCont->SetClustTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   row = n % TF->Ni;
   col = n / TF->Ni;
   XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s\n", 
                           FuncName, row, col, (char *)cv);
   }
   
   an = SUMA_SetClustValue(SO, colp, row, col,
                          TF->num_value[n], 0.0,
                          0, 1, &reset);
   if (an < 0) {
      SUMA_S_Warn("Error checking not handled yet.\n"
                  "This upcoming code chunk is from\n"
                  "sister function: SUMA_cb_SetRangeValueNew\n");
      if (an == -1 || an == -2) {
         SUMA_BEEP; 
         TF->num_value[n] = reset;
         SUMA_TableF_SetString(TF);      
         if (an == -1) { SUMA_SLP_Err("Doh"); }
         else { SUMA_SLP_Err("Duh"); }
      } else {
         SUMA_S_Err("Erriositation");
      }
   }
   
   SUMA_RETURNe;
}

/*!
   \brief updates string based on value entered in table field. 
   Nothing is done unless field is numeric
*/
void SUMA_TableF_SetString (SUMA_TABLE_FIELD * TF)
{
   static char FuncName[]={"SUMA_TableF_SetString"};
   char buf[36];

   SUMA_ENTRY;

   if (TF->cell_modified < 0) { 
      /* nothing to do, user hit enter in field without modification */
      SUMA_RETURNe;
   }
   if (TF->type == SUMA_int) {
      sprintf (buf, "%-4d", (int)TF->num_value[TF->cell_modified]);
   }else if (TF->type == SUMA_float) {
      sprintf (buf, "%s", 
               MV_format_fval2(  TF->num_value[TF->cell_modified], 
                                 TF->cwidth[TF->cell_modified / TF->Ni]));
   }else {
      /* fair enough, must be stringy */
   }
   
   XtVaSetValues (TF->cells[TF->cell_modified], XmNvalue, buf, NULL);
   SUMA_RETURNe;
}

/*!
   \brief This function is called when label field has been modified by user keyboard input.
   All it does is set TF->cell_modified to the 1D index of that cell

*/
void SUMA_TableF_cb_label_Modify (Widget w, XtPointer client_data, 
                                  XtPointer call_data)
{
   static char FuncName[]={"SUMA_TableF_cb_label_Modify"};
   SUMA_TABLE_FIELD *TF=NULL;
   int ud=0;
   static int CurrentCell = -1; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   SUMA_LH("Called");
   TF = (SUMA_TABLE_FIELD *)client_data ;
   
   if (!TF->editable) { /* this does not apply */
      SUMA_RETURNe;
   }
   if (TF->cell_modified != -1) { 
      /* make sure it is the last one you'd been working on 
      This check fails when I am dealing with mutliple tables
      If you need it, store a value for each cell and 
      check them individually*/
      if (0 && CurrentCell >= 0  && TF->cell_modified != CurrentCell) {
         SUMA_SL_Err("cell_modified not reset.");
         SUMA_RETURNe;
      }
   } 
   XtVaGetValues(w, XmNuserData, &ud, NULL);
   if (TF->cell_modified == -1) {
      /* fresh start, keep track */
      CurrentCell = ud;
   } 
   TF->cell_modified = ud;

   SUMA_RETURNe;
}


/*!
   \brief This function sets the color mapping options including the switch datasets and what have you
   It is called each time one loads a new data set and is meant to be called if the user adds a new
   colormap (not supported at the moment).
   \param NewMap --> New ColorMap was added.
   \param NewDset --> New Dataset was added.
   DO NOT CALL THIS FUNCTION if: 
   You are switching color maps, you are switching Intensity/threshold and so on
   DO call this function if you load or switch between dsets
   Do call this function if you load a new color map
   
*/
void SUMA_set_cmap_options(SUMA_SurfaceObject *SO, SUMA_Boolean NewDset,
                           SUMA_Boolean NewMap)
{
   static char FuncName[]={"SUMA_set_cmap_options"};
   SUMA_MenuItem  *SwitchInt_Menu = NULL, *SwitchThr_Menu = NULL, 
                  *SwitchBrt_Menu = NULL;
   int N_items, FirstTime;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) SUMA_RETURNe;
   if (!SO->SurfCont) SUMA_RETURNe;
   if (!SO->SurfCont->opts_form || !SO->SurfCont->opts_rc) SUMA_RETURNe;
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;
   if (!NewDset && !NewMap && SO->SurfCont->rcvo && SO->SurfCont->rccm) {
      SUMA_SL_Err("Nothing to do");
      SUMA_RETURNe;
   }
   /* row column to contain all switching stuffs */
   if (!SO->SurfCont->rcvo){
      SO->SurfCont->rcvo = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SO->SurfCont->opts_rc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
      NewDset = YUP; /* The first time around */
   } else {
      /* (no need ..) */
      /* XtUnmanageChild(SO->SurfCont->rcvo);  */
   }  
   
   if (NewDset) { /* The intensity / threshold / Brightness block*/
      /* link mode */
      if (!SO->SurfCont->LinkModeMenu->mw[SW_LinkMode]) {
               Widget rc = NULL; /* one pass through this block ONLY */
               rc = XtVaCreateWidget ("rowcolumn",
                  xmRowColumnWidgetClass, SO->SurfCont->rcvo,
                  XmNpacking, XmPACK_TIGHT, 
                  XmNorientation , XmHORIZONTAL ,
                  XmNmarginHeight, 0 ,
                  XmNmarginWidth , 0 ,
                  NULL);
               
               SUMA_LH("Forming map mode menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "IxT", '\0', YUP, LinkMode_Menu, 
                               (void *)SO,  
                               "Set I, T selection linking modes.", 
                               SUMA_SurfContHelp_Link,
                               SO->SurfCont->LinkModeMenu);
               XtManageChild (SO->SurfCont->LinkModeMenu->mw[SW_LinkMode]);
               
               XtManageChild(rc);
         }

      if (!SO->SurfCont->rcsw) {
         SO->SurfCont->rcsw = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->rcvo,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNheight, 105,         /* don't let that change dynamically,  */
            XmNresizeHeight, False, /* it messes up the frame size, when you 
                                       switch dsets*/
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
       } else {
         /* (no need ..) */
         /*XtUnmanageChild(SO->SurfCont->rcsw); */
       }
      if (!SO->SurfCont->rcsw_v1) {
         SO->SurfCont->rcsw_v1 = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->rcsw,
            XmNpacking, XmPACK_COLUMN, 
            XmNorientation , XmVERTICAL ,
            XmNnumColumns, 1,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      }
      if (!SO->SurfCont->rcsw_v2) {
         SO->SurfCont->rcsw_v2 = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->rcsw,
            XmNpacking, XmPACK_COLUMN, 
            XmNorientation , XmVERTICAL ,
            XmNnumColumns, 1,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      }
      
      /* Switching triplets */
      SwitchInt_Menu = SUMA_FormSwitchColMenuVector(SO, 0, &N_items);      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: %d items.\n", FuncName, N_items);
      if (SwitchInt_Menu || !N_items) {
         SO->SurfCont->SwitchIntMenu = 
               SUMA_Free_Menu_Widget(SO->SurfCont->SwitchIntMenu);
         SO->SurfCont->SwitchIntMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SO->SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "I", '\0', YUP, SwitchInt_Menu, 
                           (void *)SO, 
                           "Select Intensity (I) column (BHelp for more)", 
                           SUMA_SurfContHelp_SelInt,
                           SO->SurfCont->SwitchIntMenu );
         XtInsertEventHandler( SO->SurfCont->SwitchIntMenu->mw[0] , 
                                             /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) SO ,   /* client data */
                        XtListTail ) ;
         if (LocalHead) 
            SUMA_ShowMeTheChildren(SO->SurfCont->SwitchIntMenu->mw[0]);
         XtManageChild (SO->SurfCont->SwitchIntMenu->mw[0]);
         /* Now destroy the SwitchInt_Menu */
         SwitchInt_Menu = SUMA_FreeMenuVector(SwitchInt_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SO->SurfCont->SwitchIntMenu,
                       SO->SurfCont->curColPlane->OptScl->find+1) ; 
      } else {
         SUMA_SL_Err("NULL SwitchInt_Menu");
      }
      
      SwitchThr_Menu = SUMA_FormSwitchColMenuVector(SO, 1, &N_items);
      if (SwitchThr_Menu || !N_items) {
         SO->SurfCont->SwitchThrMenu = 
               SUMA_Free_Menu_Widget(SO->SurfCont->SwitchThrMenu);
         SO->SurfCont->SwitchThrMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);  
         SUMA_BuildMenuReset(13);         
         SUMA_BuildMenu (SO->SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "T", '\0', YUP, SwitchThr_Menu, 
                           (void *)SO,  
                           "Select Threshold (T) column (BHelp for more)", 
                           SUMA_SurfContHelp_SelThr ,    
                           SO->SurfCont->SwitchThrMenu );
         XtInsertEventHandler( SO->SurfCont->SwitchThrMenu->mw[0] ,      
                                       /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) SO ,   /* client data */
                        XtListTail ) ;
         XtManageChild (SO->SurfCont->SwitchThrMenu->mw[0]);
         /* Now destroy the SwitchThr_Menu */
         SwitchThr_Menu = SUMA_FreeMenuVector(SwitchThr_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SO->SurfCont->SwitchThrMenu,
                       SO->SurfCont->curColPlane->OptScl->tind+1); 
      } else {
         SUMA_SL_Err("NULL SwitchThr_Menu");
      }

      SwitchBrt_Menu = SUMA_FormSwitchColMenuVector(SO, 2, &N_items);
      if (SwitchBrt_Menu || !N_items) {
         SO->SurfCont->SwitchBrtMenu = 
               SUMA_Free_Menu_Widget(SO->SurfCont->SwitchBrtMenu);
         SO->SurfCont->SwitchBrtMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SO->SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "B", '\0', YUP, SwitchBrt_Menu, 
                           (void *)SO,  
                           "Select Brightness (B) column (BHelp for more)", 
                           SUMA_SurfContHelp_SelBrt,
                           SO->SurfCont->SwitchBrtMenu );
         XtInsertEventHandler( SO->SurfCont->SwitchBrtMenu->mw[0] ,      
                                                /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) SO ,   /* client data */
                        XtListTail ) ;

         XtManageChild (SO->SurfCont->SwitchBrtMenu->mw[0]);
         /* Now destroy the SwitchBrt_Menu */
         SwitchBrt_Menu = SUMA_FreeMenuVector(SwitchBrt_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SO->SurfCont->SwitchBrtMenu,
                       SO->SurfCont->curColPlane->OptScl->bind+1);
      } else {
         SUMA_SL_Err("NULL SwitchBrt_Menu");
      }
      
     if (1) {
     /* put the toggle buttons */
         if (!SO->SurfCont->Int_tb) {
            SO->SurfCont->Int_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SO->SurfCont->rcsw_v2, NULL);
            XtAddCallback (SO->SurfCont->Int_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwitchInt_toggled, SO);
            MCW_register_hint(SO->SurfCont->Int_tb,   
                              "View (ON)/Hide Dset node colors");
            MCW_register_help(SO->SurfCont->Int_tb,   
                              SUMA_SurfContHelp_SelIntTgl);

            SUMA_SET_SELECT_COLOR(SO->SurfCont->Int_tb);
         } 
         XmToggleButtonSetState (SO->SurfCont->Int_tb,      
                    SO->SurfCont->curColPlane->ShowMode > 0 ? 1:0 , NOPE);
         
         if (!SO->SurfCont->Thr_tb) {
            SO->SurfCont->Thr_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SO->SurfCont->rcsw_v2, NULL);
            XtAddCallback (SO->SurfCont->Thr_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwitchThr_toggled, SO);
            SUMA_SET_SELECT_COLOR(SO->SurfCont->Thr_tb);
            MCW_register_hint(SO->SurfCont->Thr_tb,   
                              "Apply (ON)/Ignore thresholding");
            MCW_register_help(SO->SurfCont->Thr_tb,   
                              SUMA_SurfContHelp_SelThrTgl);
         }
         if (SO->SurfCont->curColPlane->OptScl->tind >=0) {
            XmToggleButtonSetState (SO->SurfCont->Thr_tb, 
                              SO->SurfCont->curColPlane->OptScl->UseThr, NOPE);
         }else {
            XmToggleButtonSetState (SO->SurfCont->Thr_tb, NOPE, NOPE);
         }
         
         if (!SO->SurfCont->Brt_tb) {
            SO->SurfCont->Brt_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SO->SurfCont->rcsw_v2, NULL);
            XtAddCallback (SO->SurfCont->Brt_tb, 
                     XmNvalueChangedCallback, SUMA_cb_SwitchBrt_toggled, SO);
            SUMA_SET_SELECT_COLOR(SO->SurfCont->Brt_tb);
            MCW_register_hint(SO->SurfCont->Brt_tb,   
                     "View (ON)/Ignore brightness modulation");
            MCW_register_help(SO->SurfCont->Brt_tb,   
                              SUMA_SurfContHelp_SelBrtTgl);
         }
         if (SO->SurfCont->curColPlane->OptScl->bind >=0) {
            XmToggleButtonSetState (SO->SurfCont->Brt_tb, 
                     SO->SurfCont->curColPlane->OptScl->UseBrt, NOPE);
         } else {
            XmToggleButtonSetState (SO->SurfCont->Brt_tb, NOPE, NOPE);
         }
      }
      if (!XtIsManaged(SO->SurfCont->rcsw_v1)) 
         XtManageChild (SO->SurfCont->rcsw_v1);
      if (!XtIsManaged(SO->SurfCont->rcsw_v2)) 
         XtManageChild (SO->SurfCont->rcsw_v2);
      if (!XtIsManaged(SO->SurfCont->rcsw)) XtManageChild (SO->SurfCont->rcsw);
   } /* The intensity / threshold / Brightness block */
   
   
   {/*  The Color map range and selector block */
      char *col_tit[]=  {  " ", "Min", "Max", NULL};
      char *col_hint[]= {  "Clipping ranges", 
                           "Minimum clip value", 
                           "Maximum clip value" , NULL};
      char *col_help[]= {  SUMA_SurfContHelp_SetRngTbl_r0, 
                           SUMA_SurfContHelp_SetRngTbl_c1, 
                           SUMA_SurfContHelp_SetRngTbl_c2 , NULL};
      char *row_tit[]=  {  " ", "I", "B", " " , "C", NULL};
      char *row_hint[]= {  
         "Clipping ranges ", 
         "Intensity clipping range (much more with BHelp)", 
         "Brightness modulation clipping range (much more with BHelp)", 
         "Brightness modulation factor range (much more with BHelp)" , 
         "Coordinate bias range (much more with BHelp)", NULL};
      char *row_help[]= {  SUMA_SurfContHelp_SetRngTbl_r0, 
                           SUMA_SurfContHelp_SetRngTbl_r1,
                           SUMA_SurfContHelp_SetRngTbl_r2, 
                           SUMA_SurfContHelp_SetRngTbl_r3, 
                           SUMA_SurfContHelp_SetRngTbl_r4, NULL};
      if (!SO->SurfCont->rccm) {
         SO->SurfCont->rccm = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->rcvo,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0,
            XmNmarginWidth, 0,
            NULL);
         NewMap = YUP; /* the first time around */
      }
       
      if (NewMap) {/* new colormaps */
         SUMA_LH("NewMap set");
         if (!SO->SurfCont->SetRangeTable->cells) {
            int colw[3] = { 1, 8, 8 };
            SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)calloc(1,sizeof(SUMA_SRV_DATA));
            srvd->SO = SO; srvd->colp = NULL;               
            /* create the widgets for the range table */
            SUMA_LH("Creating table");
            SUMA_CreateTable( SO->SurfCont->rccm,
                           5, 3, 
                           row_tit, col_tit,  
                           row_hint, col_hint,  
                           row_help, col_help,  
                           colw, YUP, SUMA_float, 
                           SUMA_cb_SetRangeValue, (void *)srvd,
                           SUMA_SetRangeTableTit_EV, NULL,
                           NULL, NULL,  
                           SO->SurfCont->SetRangeTable);
         }
         if (!SO->SurfCont->CoordBiasMenu->mw[SW_CoordBias]) {
               Widget rc = NULL; /* one pass through this block ONLY */
               rc = XtVaCreateWidget ("rowcolumn",
                  xmRowColumnWidgetClass, SO->SurfCont->rccm,
                  XmNpacking, XmPACK_TIGHT, 
                  XmNorientation , XmHORIZONTAL ,
                  XmNmarginHeight, 0 ,
                  XmNmarginWidth , 0 ,
                  NULL);
               
               SUMA_LH("Forming map mode menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "Col", '\0', YUP, CmapMode_Menu, 
                               (void *)SO,  
                               "Switch between color mapping modes.", 
                               SUMA_SurfContHelp_Col,
                               SO->SurfCont->CmapModeMenu);
               XtManageChild (SO->SurfCont->CmapModeMenu->mw[SW_CmapMode]);
               
               SUMA_LH("Forming new bias menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "Bias", '\0', YUP, CoordBias_Menu, 
                               (void *)SO, 
                               "Coordinate bias direction", 
                               SUMA_SurfContHelp_Bias, 
                               SO->SurfCont->CoordBiasMenu);
               XtManageChild (SO->SurfCont->CoordBiasMenu->mw[SW_CoordBias]);
               
               XtManageChild(rc);
         }
            
         if (!SO->SurfCont->rccm_swcmap) {
            SUMA_LH("Creating rccm_swcmap");
            SO->SurfCont->rccm_swcmap =  XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SO->SurfCont->rccm,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0 ,
               XmNmarginWidth , 0 ,
               NULL);
         }

         {
            SUMA_CreateUpdatableCmapMenu(SO); 

            #if 0
               /* Not any more, menu is now stuck in its own rc */
               /* the loader, needs to be recreated with colormap menu  */
               if (SO->SurfCont->CmapLoad_pb) { 
                  XtDestroyWidget(SO->SurfCont->CmapLoad_pb); 
                  SO->SurfCont->CmapLoad_pb = NULL;
               }
            #endif
            if (!SO->SurfCont->CmapLoad_pb) { 
               SUMA_LH("Forming CmapLoad button");
               SO->SurfCont->CmapLoad_pb = XtVaCreateManagedWidget ("New", 
                                 xmPushButtonWidgetClass, 
                                 SO->SurfCont->rccm_swcmap, 
                                 NULL);
               XtAddCallback (SO->SurfCont->CmapLoad_pb, XmNactivateCallback, 
                              SUMA_cb_Cmap_Load, (XtPointer) SO);
               MCW_register_hint(SO->SurfCont->CmapLoad_pb , 
                                 "Load new colormap");
               MCW_register_help(SO->SurfCont->CmapLoad_pb ,  
                                 SUMA_SurfContHelp_CmpNew);
            }
         } /* new colormaps */
         if (!XtIsManaged(SO->SurfCont->rccm_swcmap)) 
                     XtManageChild (SO->SurfCont->rccm_swcmap); 
      }
      
      /* Set the CoordBias's menu history to reflect current setting */
      SUMA_LH("Updating Link Mode History");
      SUMA_Set_Menu_Widget( SO->SurfCont->LinkModeMenu,
                     SO->SurfCont->curColPlane->LinkMode); 
      
      SUMA_LH("Working the lock stuff ...");
      /* You'll need to fix the table's locking widget colors */
      if ( SO->SurfCont->IntRangeLocked == 
               SO->SurfCont->curColPlane->OptScl->AutoIntRange) {
         SUMA_LH("   Do the Int");
         /* need to put things in sync */
         SO->SurfCont->IntRangeLocked = !SO->SurfCont->IntRangeLocked;
         MCW_invert_widget(SO->SurfCont->SetRangeTable->cells[1]);
      }
      if ( SO->SurfCont->BrtRangeLocked == 
               SO->SurfCont->curColPlane->OptScl->AutoBrtRange) {
         SUMA_LH("   Do the Brt");
         /* need to put things in sync */
         SO->SurfCont->BrtRangeLocked = !SO->SurfCont->BrtRangeLocked;
         MCW_invert_widget(SO->SurfCont->SetRangeTable->cells[2]);
      } 

      /* Set the CoordBias's menu history to reflect current setting */
      SUMA_LH("Updating CoorBias chooser History");
      SUMA_Set_Menu_Widget( SO->SurfCont->CoordBiasMenu,
                     SO->SurfCont->curColPlane->OptScl->DoBias); 
 
      /* Set the Col's menu history to reflect current setting */
      SUMA_LH("Updating Col chooser History");
      SUMA_Set_Menu_Widget( SO->SurfCont->CmapModeMenu,
                     SO->SurfCont->curColPlane->OptScl->interpmode); 
 
      /* add the selectors for symmetric range and absolute threshold */
      if (!SO->SurfCont->AbsThresh_tb) {
         Widget rc;
         rc = XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SO->SurfCont->rccm,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0 ,
               XmNmarginWidth , 0 ,
               NULL);
         /* create the absolute threshold toggle button */
         SO->SurfCont->AbsThresh_tb = XtVaCreateManagedWidget("|T|", 
               xmToggleButtonWidgetClass, rc, 
               NULL);
         XtAddCallback (SO->SurfCont->AbsThresh_tb, 
               XmNvalueChangedCallback, SUMA_cb_AbsThresh_tb_toggled, SO);
         MCW_register_hint(SO->SurfCont->AbsThresh_tb , 
                           "Absolute threshold ON/OFF");
         MCW_register_help(SO->SurfCont->AbsThresh_tb ,  
                           SUMA_SurfContHelp_AbsThr );
         
         SUMA_SET_SELECT_COLOR(SO->SurfCont->AbsThresh_tb);
         
         /* create the symmetric range toggle button */
         SO->SurfCont->SymIrange_tb = XtVaCreateManagedWidget("sym I", 
               xmToggleButtonWidgetClass, rc, NULL);
         XtAddCallback (SO->SurfCont->SymIrange_tb, 
               XmNvalueChangedCallback, SUMA_cb_SymIrange_tb_toggled, SO);
         MCW_register_hint(SO->SurfCont->SymIrange_tb, 
                           "Intensity range symmetry about 0 ");
         MCW_register_help(SO->SurfCont->SymIrange_tb,  SUMA_SurfContHelp_Isym);
         SUMA_SET_SELECT_COLOR(SO->SurfCont->SymIrange_tb);
         
         /* add a button for zero masking */
         SO->SurfCont->ShowZero_tb = XtVaCreateManagedWidget("shw 0", 
               xmToggleButtonWidgetClass, rc, NULL);
         XtAddCallback (SO->SurfCont->ShowZero_tb, 
               XmNvalueChangedCallback, SUMA_cb_ShowZero_tb_toggled, SO);
         MCW_register_hint(SO->SurfCont->ShowZero_tb,   
               "Color masking of nodes with intensity = 0 ");
         MCW_register_help(SO->SurfCont->ShowZero_tb,   SUMA_SurfContHelp_Shw0);
         SUMA_SET_SELECT_COLOR(SO->SurfCont->ShowZero_tb);
         XtManageChild (rc);
      }
      
      {/* The clustering options */
         char *col_tit[]= { " ", "Conn", "Area", NULL };
         char *col_hint[]={ "Clusterizing options", 
                            "Connectedness criterion",
                            "Cluster Area Threshold", NULL };
         char *col_help[]={   SUMA_SurfContHelp_SetClustTbl_r0,
                              SUMA_SurfContHelp_SetClustTbl_c1,
                              SUMA_SurfContHelp_SetClustTbl_c2, NULL
                           };
         char *row_tit[]={ " ", "Clst", NULL};
         char *row_hint[]={ "Clusterizing options", "Clust on/off", NULL};
         char *row_help[]={ SUMA_SurfContHelp_SetClustTbl_r0,
                            SUMA_SurfContHelp_SetClustTbl_r1, NULL
                           };

         SUMA_LH("Receptacle");
         if (!SO->SurfCont->rcclust) {
            SO->SurfCont->rcclust = XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SO->SurfCont->opts_form,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0,
               XmNmarginWidth, 0,
               XmNrightAttachment, XmATTACH_FORM , 
               XmNleftAttachment,  XmATTACH_NONE,
               XmNtopAttachment, XmATTACH_WIDGET ,
               XmNtopWidget, SO->SurfCont->opts_rc,
               NULL);
            FirstTime = YUP; /* the first time around */
         } else {
            FirstTime = NOPE;
         }

         if (FirstTime) {/* new clust */
            SUMA_LH("FirstTime set");
            if (!SO->SurfCont->SetClustTable->cells) {
               int colw[3] = { 4, 6, 6 };
               SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)
                                 calloc(1,sizeof(SUMA_SRV_DATA));
               srvd->SO = SO; srvd->colp = NULL;               
               /* create the widgets for the range table */
               SUMA_LH("Creating table");
               SUMA_CreateTable( SO->SurfCont->rcclust,
                              2, 3, 
                              row_tit, col_tit,  
                              row_hint, col_hint,  
                              row_help, col_help,  
                              colw, YUP, SUMA_float, 
                              SUMA_cb_SetClustValue, (void *)srvd,
                              SUMA_SetClustTableTit_EV, NULL,
                              NULL, NULL,  
                              SO->SurfCont->SetClustTable);
            }
            
            if (SO->SurfCont->curColPlane->OptScl) {
                SUMA_SetTableTitleButton1(SO->SurfCont->SetClustTable, 1,0,                                       SO->SurfCont->curColPlane->OptScl->Clusterize);
            }
         }
         SUMA_LH("Managerial");

         if (!XtIsManaged(SO->SurfCont->rcclust)) 
                  XtManageChild (SO->SurfCont->rcclust);

      }/* The clustering options */

      /* do the initialization */
      SUMA_InitClustTable(SO); /* init the clust table values*/
      
      if (SO->SurfCont->curColPlane->OptScl->ThrMode == SUMA_ABS_LESS_THAN) {
         XmToggleButtonSetState( SO->SurfCont->AbsThresh_tb, True, NOPE);
      } else {
         XmToggleButtonSetState( SO->SurfCont->AbsThresh_tb, False, NOPE);
      }
      if (!SO->SurfCont->curColPlane->SymIrange) {
         XmToggleButtonSetState( SO->SurfCont->SymIrange_tb, False, NOPE);
      } else {
         XmToggleButtonSetState( SO->SurfCont->SymIrange_tb, True, NOPE);
      }
      if (!SO->SurfCont->curColPlane->OptScl->MaskZero) {
         XmToggleButtonSetState( SO->SurfCont->ShowZero_tb, True, NOPE);
      } else {
         XmToggleButtonSetState( SO->SurfCont->ShowZero_tb, False, NOPE);
      }
      
      if (!XtIsManaged(SO->SurfCont->rccm)) XtManageChild (SO->SurfCont->rccm);
   
   }/*  The Color map range and selector block */
   
   if (1){ /* The Range values block*/
      char *col_tit[]=  {  " ", "Min", "Node", "Max", "Node", NULL};
      char *col_hint[]= {  "Full range in Dset", 
                           "Minimum value in Dset column", 
                           "Node index at minimum", 
                           "Maximum value in Dset column", 
                           "Node index at maximum", NULL};
      char *col_help[]= {  SUMA_SurfContHelp_RangeTbl_c0, 
                           SUMA_SurfContHelp_RangeTbl_c1,
                           SUMA_SurfContHelp_RangeTbl_c2, 
                           SUMA_SurfContHelp_RangeTbl_c3, 
                           SUMA_SurfContHelp_RangeTbl_c4, NULL};
      char *row_tit[]=  {  " ", "I", "T", "B", NULL};
      char *row_hint[]= {  "Full range in Dset", 
                           "Range of values in intensity (I) column", 
                           "Range of values in threshold (T) column", 
                           "Range of values in brightness (B) column", NULL};
      char *row_help[]= {  SUMA_SurfContHelp_RangeTbl_c0, 
                           SUMA_SurfContHelp_RangeTbl_r1, 
                           SUMA_SurfContHelp_RangeTbl_r2, 
                           SUMA_SurfContHelp_RangeTbl_r3, NULL};
      if (!SO->SurfCont->rcswr) {
         SO->SurfCont->rcswr = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->opts_form,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNrightAttachment, XmATTACH_FORM , 
            XmNleftAttachment,  XmATTACH_NONE,
            XmNtopAttachment, XmATTACH_WIDGET ,
            XmNtopWidget, SO->SurfCont->rcclust,
            NULL);
      }
      
      if (!SO->SurfCont->RangeTable->cells) {
         int colw[5] = { 1, 6, 6, 6, 6 };
         /* create the widgets for the range table */
         SUMA_CreateTable( SO->SurfCont->rcswr,
                           4, 5, 
                           row_tit, col_tit,  
                           row_hint, col_hint,  
                           row_help, col_help,  
                           colw, NOPE, SUMA_string, 
                           NULL, NULL,
                           NULL, NULL,  
                           SUMA_RangeTableCell_EV, (void *)SO, 
                           SO->SurfCont->RangeTable);
      }

      if (!XtIsManaged(SO->SurfCont->rcswr)) XtManageChild (SO->SurfCont->rcswr);
   } /* The Range values block */
         
   if (NewDset) {
      /* initialize tables of range values */
      SUMA_InitRangeTable(SO, 2);
   }
   
   if (!XtIsManaged(SO->SurfCont->rcvo)) XtManageChild (SO->SurfCont->rcvo);
   SUMA_FORCE_SCALE_HEIGHT(SO); /* Unfortunately, you need to resize after managing */

   SUMA_RETURNe;
}

/*!
   A function to create the cmap selection menu
   in a manner that can be recreated if the menu
   contents change. You can call this function
   repeatedly whenever menu contents change 
*/
void SUMA_CreateUpdatableCmapMenu(SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_CreateUpdatableCmapMenu"};
   SUMA_MenuItem *SwitchCmap_Menu = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SUMAg_CF->scm) {   
      SUMAg_CF->scm = SUMA_Build_Color_maps();
      if (!SUMAg_CF->scm) {
         SUMA_SL_Err("Failed to build color maps.\n");
         SUMA_RETURNe;
      }
   }
   
   if (!SO->SurfCont->rc_CmapCont) { /* first pass, create placement container */
      SO->SurfCont->rc_CmapCont = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, SO->SurfCont->rccm_swcmap,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight, 0 ,
      XmNmarginWidth , 0 ,
      NULL);   
   }

   SUMA_LH("Forming CmapMenu");
   SwitchCmap_Menu = SUMA_FormSwitchCmapMenuVector(SUMAg_CF->scm->CMv, 
                                                   SUMAg_CF->scm->N_maps);
   if (SwitchCmap_Menu) {
      SO->SurfCont->SwitchCmapMenu =
         SUMA_Free_Menu_Widget(SO->SurfCont->SwitchCmapMenu);
      SO->SurfCont->SwitchCmapMenu = 
               SUMA_Alloc_Menu_Widget(SUMAg_CF->scm->N_maps+1);
      SUMA_BuildMenuReset(10);
      SUMA_BuildMenu (  SO->SurfCont->rc_CmapCont, 
                           XmMENU_OPTION, /* populate it */
                           "Cmp", '\0', YUP, SwitchCmap_Menu, 
                           (void *)SO,  
                           "Switch between available color maps."
                           " (BHelp for more)", 
                           SUMA_SurfContHelp_Cmp, 
                           SO->SurfCont->SwitchCmapMenu );
      XtInsertEventHandler( SO->SurfCont->SwitchCmapMenu->mw[0] ,      
                                               /* handle events in optmenu */
                            ButtonPressMask ,  /* button presses */
                            FALSE ,            /* nonmaskable events? */
                            SUMA_optmenu_EV ,  /* handler */
                            (XtPointer) SO ,   /* client data */
                            XtListTail ) ;
      XtManageChild (SO->SurfCont->SwitchCmapMenu->mw[0]);
      /* Now destroy the SwitchCmap_Menu */
      SwitchCmap_Menu = SUMA_FreeMenuVector(SwitchCmap_Menu, 
                                          SUMAg_CF->scm->N_maps);
   }

   XtManageChild(SO->SurfCont->rc_CmapCont);

   SUMA_RETURNe;
}

SUMA_Boolean SUMA_InitClustTable(SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_InitClustTable"};
   SUMA_TABLE_FIELD *TFs;
   SUMA_Boolean ColorizeBaby;
   SUMA_SCALE_TO_MAP_OPT *OptScl;
   
   SUMA_ENTRY;

   if (!SO->SurfCont) SUMA_RETURN(NOPE);
   TFs = SO->SurfCont->SetClustTable; 
   if (!TFs) SUMA_RETURN(NOPE);
   OptScl = SO->SurfCont->curColPlane->OptScl;
   
   ColorizeBaby = NOPE; /* Not sure if I'll need this one here */
   
   SUMA_INSERT_CELL_VALUE(TFs, 1, 1, OptScl->ClustOpt->DistLim);
   SUMA_INSERT_CELL_VALUE(TFs, 1, 2, OptScl->ClustOpt->AreaLim);
   SUMA_SetTableTitleButton1(TFs, 1,0, OptScl->Clusterize);
   
   if (ColorizeBaby) {
      if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(NOPE);
      }
   }
   
   SUMA_RETURN(YUP);
}
/*!
   \brief updates table with data value range
            and the table where user sets mapping
            range (that last one depends on what)
   SO: You know what
   what: (int)   -1: Do NOTHING with user accessible mapping ranges
                  0: intensity only
                  1: brightness modulation only
                  2: Set all user accessible mapping ranges
*/
SUMA_Boolean SUMA_InitRangeTable(SUMA_SurfaceObject *SO, int what) 
{
   static char FuncName[]={"SUMA_InitRangeTable"};
   char srange_min[50], srange_max[50], srange_minloc[50], srange_maxloc[50];
   SUMA_TABLE_FIELD *TF, *TFs;
   int i, j, i1D, fi, bi, ti;
   double range[2];
   NI_element *nel;
   SUMA_SCALE_TO_MAP_OPT *OptScl;
   SUMA_Boolean DoIs = NOPE, DoBs = NOPE, ColorizeBaby;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO->SurfCont) SUMA_RETURN(NOPE);
   TF = SO->SurfCont->RangeTable; 
   TFs = SO->SurfCont->SetRangeTable; 
   if (!TF || !TFs) SUMA_RETURN(NOPE);
   OptScl = SO->SurfCont->curColPlane->OptScl;
   fi = OptScl->find;
   ti = OptScl->tind;
   bi = OptScl->bind;
   ColorizeBaby = NOPE;
   
   switch (what) {
      case -1:
         DoIs = NOPE;
         DoBs = NOPE;
         break;
      case 2:
         DoIs = YUP;
         DoBs = YUP;
         break;
      case 0:
         DoIs = YUP;
         break;
      case 1:
         DoBs = YUP;
         break;
      default:
         SUMA_SL_Err("That's stupid Joe!");
         SUMA_RETURN(NOPE);
         break;
   }
   
   /* TF Range table Int*/
   SUMA_LH("Setting Int.");
   SUMA_RANGE_STRING(SO->SurfCont->curColPlane->dset_link, 
                     fi, srange_min, srange_max, 
                     srange_minloc, srange_maxloc, range); 
   SUMA_INSERT_CELL_STRING(TF, 1, 1, srange_min);/* min */
   SUMA_INSERT_CELL_STRING(TF, 1, 2, srange_minloc);/* minloc */
   SUMA_INSERT_CELL_STRING(TF, 1, 3, srange_max);/* max */
   SUMA_INSERT_CELL_STRING(TF, 1, 4, srange_maxloc);/* maxloc */
   /* TFs Range table Int*/
   if (DoIs) {
      if (SO->SurfCont->curColPlane->OptScl->AutoIntRange) { 
         if (!SO->SurfCont->curColPlane->ForceIntRange[0] && 
             !SO->SurfCont->curColPlane->ForceIntRange[1]) {
            if (  OptScl->IntRange[0] != range[0] ||
                  OptScl->IntRange[1] != range[1] ) {
               ColorizeBaby = YUP;      
               OptScl->IntRange[0] = range[0]; OptScl->IntRange[1] = range[1]; 
            }
         } else {
            SUMA_LH("Using ForceIntRange");
            if (  OptScl->IntRange[0] != 
                     SO->SurfCont->curColPlane->ForceIntRange[0] ||
                  OptScl->IntRange[1] != 
                     SO->SurfCont->curColPlane->ForceIntRange[1] ) {
               ColorizeBaby = YUP;      
               OptScl->IntRange[0] = SO->SurfCont->curColPlane->ForceIntRange[0];
               OptScl->IntRange[1] = SO->SurfCont->curColPlane->ForceIntRange[1];
            }
         }

         /* enforce the SymIrange option */
         if (SO->SurfCont->curColPlane->SymIrange) {
            if (  OptScl->IntRange[1] != 
                     SUMA_LARG_ABS(OptScl->IntRange[0], OptScl->IntRange[1]) ||
                  OptScl->IntRange[0] != -OptScl->IntRange[1]) {
               ColorizeBaby = YUP;   
               OptScl->IntRange[1] = 
                     SUMA_LARG_ABS(OptScl->IntRange[0], OptScl->IntRange[1]);
               OptScl->IntRange[0] = -OptScl->IntRange[1];
            }
         }

         SUMA_INSERT_CELL_VALUE(TFs, 1, 1, OptScl->IntRange[0]);/* min */ 
         SUMA_INSERT_CELL_VALUE(TFs, 1, 2, OptScl->IntRange[1]);/* max */
         if (SO->SurfCont->curColPlane->OptScl->AutoIntRange < 0) {
            /* snap out of the initialization and go to user's default */
            SO->SurfCont->curColPlane->OptScl->AutoIntRange = 
                     SUMA_isEnv("SUMA_Auto_I_Range","YES") ? 1:0;
            SUMA_LHv("AutoIntRange now %d\n",
                     SO->SurfCont->curColPlane->OptScl->AutoIntRange);
         }
      }else {
         /* Make sure viewer is showing same values as in OptScl */
         /* enforce the SymIrange option */
         if (SO->SurfCont->curColPlane->SymIrange) {
            if (  OptScl->IntRange[1] != 
                     SUMA_LARG_ABS(OptScl->IntRange[0], OptScl->IntRange[1]) ||
                  OptScl->IntRange[0] != -OptScl->IntRange[1]) {
               ColorizeBaby = YUP;   
               OptScl->IntRange[1] = 
                     SUMA_LARG_ABS(OptScl->IntRange[0], OptScl->IntRange[1]);
               OptScl->IntRange[0] = -OptScl->IntRange[1];
            }
         }
         SUMA_LH("Imposing...");
         if (  OptScl->IntRange[0] != TFs->num_value[1*TFs->Ni+1] ||
               OptScl->IntRange[1] != TFs->num_value[2*TFs->Ni+1] ) {
            SUMA_INSERT_CELL_VALUE(TFs, 1, 1, OptScl->IntRange[0]);/* min */ 
            SUMA_INSERT_CELL_VALUE(TFs, 1, 2, OptScl->IntRange[1]);/* max */
         }
      }
   } 
   /* TF Range table Thr*/
   SUMA_LH("Setting Thr.");
   SUMA_RANGE_STRING(SO->SurfCont->curColPlane->dset_link, ti, 
                     srange_min, srange_max, 
                     srange_minloc, srange_maxloc, range); 
   SUMA_INSERT_CELL_STRING(TF, 2, 1, srange_min);/* min */
   SUMA_INSERT_CELL_STRING(TF, 2, 2, srange_minloc);/* minloc */
   SUMA_INSERT_CELL_STRING(TF, 2, 3, srange_max);/* max */
   SUMA_INSERT_CELL_STRING(TF, 2, 4, srange_maxloc);/* maxloc */
  
   /* TF Range table Brt*/
   SUMA_LH("Setting Brt.");
   SUMA_RANGE_STRING(SO->SurfCont->curColPlane->dset_link, bi, 
                     srange_min, srange_max, 
                     srange_minloc, srange_maxloc, range); 
   SUMA_INSERT_CELL_STRING(TF, 3, 1, srange_min);/* min */
   SUMA_INSERT_CELL_STRING(TF, 3, 2, srange_minloc);/* minloc */
   SUMA_INSERT_CELL_STRING(TF, 3, 3, srange_max);/* max */
   SUMA_INSERT_CELL_STRING(TF, 3, 4, srange_maxloc);/* maxloc */
   /* TFs Range table Brt*/
   if (DoBs) {
      if (SO->SurfCont->curColPlane->OptScl->AutoBrtRange) { 
         if (  OptScl->BrightRange[0] != range[0] ||
               OptScl->BrightRange[1] != range[1] ) {
            ColorizeBaby = YUP;      
            OptScl->BrightRange[0] = range[0]; OptScl->BrightRange[1] = range[1]; 
         }
         SUMA_INSERT_CELL_VALUE(TFs, 2, 1, OptScl->BrightRange[0]);/* min */
         SUMA_INSERT_CELL_VALUE(TFs, 2, 2, OptScl->BrightRange[1]);/* max */
         /* TFs Range table BrtMap*/
         SUMA_INSERT_CELL_VALUE(TFs, 3, 1, OptScl->BrightMap[0]);/* min */
         SUMA_INSERT_CELL_VALUE(TFs, 3, 2, OptScl->BrightMap[1]);/* max */
         if (SO->SurfCont->curColPlane->OptScl->AutoBrtRange < 0) {
            /* snap out of the initialization and go to user's default */
            SO->SurfCont->curColPlane->OptScl->AutoBrtRange = 
                     SUMA_isEnv("SUMA_Auto_B_Range","YES") ? 1:0;
            SUMA_LHv("AutoBrtRange now %d\n",
                     SO->SurfCont->curColPlane->OptScl->AutoBrtRange);
         }
      } else {
         /* Make sure viewer is showing same values as in OptScl */
         if (  OptScl->BrightRange[0] != TFs->num_value[1*TFs->Ni+2] ||
               OptScl->BrightRange[1] != TFs->num_value[2*TFs->Ni+2] ||
               OptScl->BrightMap[0]   != TFs->num_value[1*TFs->Ni+3] ||
               OptScl->BrightMap[1]   != TFs->num_value[2*TFs->Ni+3]   ) {
            SUMA_INSERT_CELL_VALUE(TFs, 2, 1, OptScl->BrightRange[0]);/* min */
            SUMA_INSERT_CELL_VALUE(TFs, 2, 2, OptScl->BrightRange[1]);/* max */
            /* TFs Range table BrtMap*/
            SUMA_INSERT_CELL_VALUE(TFs, 3, 1, OptScl->BrightMap[0]);/* min */
            SUMA_INSERT_CELL_VALUE(TFs, 3, 2, OptScl->BrightMap[1]);/* max */
         }
      } 
   }
   
   /* TFs Range table CoordBias*/
   SUMA_INSERT_CELL_VALUE(TFs, 4, 1, OptScl->CoordBiasRange[0]);/* min */
   SUMA_INSERT_CELL_VALUE(TFs, 4, 2, OptScl->CoordBiasRange[1]);/* max */
   
   if (ColorizeBaby) {
      if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(NOPE);
      }
   }
   SUMA_RETURN(YUP);
}

SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleCmapList(SUMA_COLOR_MAP **CMv, 
                                                  int N_maps) 
{
   static char FuncName[]={"SUMA_AssembleCmapList"};
   SUMA_ASSEMBLE_LIST_STRUCT *clist_str = NULL;  
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   clist_str = SUMA_CreateAssembleListStruct();
   clist_str->clist = (char **)SUMA_calloc(N_maps, sizeof(char *));
   clist_str->oplist = (void **)SUMA_calloc(N_maps, sizeof(void *));
   clist_str->N_clist = N_maps;
   
   for (i=0; i<N_maps; ++i) {
      clist_str->clist[i] = SUMA_copy_string(CMv[i]->Name);
      clist_str->oplist[i] = (void*)CMv[i];
   }
   
   SUMA_RETURN(clist_str);
}

SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleDsetColList(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_AssembleDsetColList"};
   SUMA_ASSEMBLE_LIST_STRUCT *clist_str = NULL;  
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SDSET_VECNUM(dset) < 1) SUMA_RETURN(clist_str);
   
   clist_str = SUMA_CreateAssembleListStruct();
   clist_str->clist = (char **)SUMA_calloc(SDSET_VECNUM(dset), sizeof(char *));
   clist_str->oplist = (void **)SUMA_calloc(SDSET_VECNUM(dset), sizeof(void *));
   clist_str->N_clist = SDSET_VECNUM(dset);
   clist_str->content_id = SUMA_copy_string(SDSET_ID(dset));
   
   for (i=0; i<SDSET_VECNUM(dset); ++i) {
      clist_str->clist[SDSET_VECNUM(dset)-1-i] = 
                  SUMA_DsetColLabelCopy(dset,i, 1);
      clist_str->oplist[SDSET_VECNUM(dset)-1-i] = (XTP_CAST)i;
   }
   
   SUMA_RETURN(clist_str);
}

/*!
   \brief opens a list selection for choosing a Dset column 
*/
SUMA_Boolean SUMA_DsetColSelectList(
         SUMA_SurfaceObject *SO, int type, 
         int refresh, int bringup)
{
   static char FuncName[]={"SUMA_DsetColSelectList"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!SO || !SO->SurfCont) SUMA_RETURN(NOPE);
   
   /* Widget is common to a surface controller. */
   switch (type) {
      case 0:
         LW = SO->SurfCont->SwitchIntMenu->lw;
         if (!LW) {
            SUMA_LH("Allocating widget");
            /* need to create widget */
            LW = SUMA_AllocateScrolledList   (  
                  "Switch Intensity", SUMA_LSP_BROWSE,
                  NOPE,          NOPE,
                  SO->SurfCont->TLS, SWP_POINTER_OFF,
                  150,
                  SUMA_cb_SelectSwitchInt, (void *)SO,
                  SUMA_cb_SelectSwitchInt, (void *)SO,
                  SUMA_cb_CloseSwitchLst, NULL);

            SO->SurfCont->SwitchIntMenu->lw = LW;
            refresh = 1; /* no doubt aboot it */
         } else {
            if (  (void *)SO != LW->Default_Data || 
                  (void *)SO != LW->Select_Data) {
               /* just update the callback data info in LW */
               SUMA_UpdateScrolledListData(LW, (void *)SO, (void *)SO, NULL);
            }
            
         }
         break;
      case 1:
         LW = SO->SurfCont->SwitchThrMenu->lw;
         if (!LW) {
            SUMA_LH("Allocating widget");
            /* need to create widget */
            LW = SUMA_AllocateScrolledList   (  
                  "Switch Threshold", SUMA_LSP_BROWSE,
                  NOPE,          NOPE,
                  SO->SurfCont->TLS, SWP_POINTER_OFF,
                  150,
                  SUMA_cb_SelectSwitchThr, (void *)SO,
                  SUMA_cb_SelectSwitchThr, (void *)SO,
                  SUMA_cb_CloseSwitchLst, NULL);

            SO->SurfCont->SwitchThrMenu->lw = LW;
            refresh = 1; /* no doubt aboot it */
         } else {
            if (  (void *)SO != LW->Default_Data || 
                  (void *)SO != LW->Select_Data) {
               /* just update the callback data info in LW */
               SUMA_UpdateScrolledListData(LW, (void *)SO, (void *)SO, NULL);
            }
            
         }
         break;
      case 2:
         LW = SO->SurfCont->SwitchBrtMenu->lw;
         if (!LW) {
            SUMA_LH("Allocating widget");
            /* need to create widget */
            LW = SUMA_AllocateScrolledList   (  
                  "Switch Brightness", SUMA_LSP_BROWSE,
                  NOPE,          NOPE,
                  SO->SurfCont->TLS, SWP_POINTER_OFF,
                  150, 
                  SUMA_cb_SelectSwitchBrt, (void *)SO,
                  SUMA_cb_SelectSwitchBrt, (void *)SO,
                  SUMA_cb_CloseSwitchLst, NULL);

            SO->SurfCont->SwitchBrtMenu->lw = LW;
            refresh = 1; /* no doubt aboot it */
         } else {
            if (  (void *)SO != LW->Default_Data || 
                  (void *)SO != LW->Select_Data) {
               /* just update the callback data info in LW */
               SUMA_UpdateScrolledListData(LW, (void *)SO, (void *)SO, NULL);
            }
            
         }
         break;
      default:
         SUMA_SL_Err("Unexpected type");
         SUMA_RETURN(NOPE);
   }
            
   /* Refresh if LW exists, but request is for a new data set */
   if (!refresh &&
        strcmp(LW->ALS->content_id, 
               SDSET_ID(SO->SurfCont->curColPlane->dset_link))) {
      refresh=1;
   } 
  
  if (refresh) {
      /* Now creating list*/
      if (LW->ALS) {
         if (LocalHead) SUMA_S_Err("Freeing the hag.");
         LW->ALS = SUMA_FreeAssembleListStruct(LW->ALS);
      }
      SUMA_LH("Assembling");
      LW->ALS = SUMA_AssembleDsetColList(SO->SurfCont->curColPlane->dset_link);
      if (!LW->ALS) {
         SUMA_SL_Err("Failed to assemble list");
         SUMA_RETURN(NOPE);
      }
      if (LW->ALS->N_clist < 0) {
         SUMA_SL_Err("Failed in SUMA_AssembleDsetColList");
         SUMA_RETURN(NOPE);
      }
      if (!LW->ALS->N_clist) {
         SUMA_SLP_Note ("No Dset Cols to choose from.");
         SUMA_RETURN(NOPE);
      }
   }
   
   if (bringup) 
      SUMA_CreateScrolledList ( LW->ALS->clist, LW->ALS->N_clist, NOPE, LW);
   
   SUMA_RETURN(YUP);
}


int SUMA_GetListIchoice(XmListCallbackStruct *cbs, 
                        SUMA_LIST_WIDGET *LW, 
                        SUMA_Boolean *CloseShop)
{
   static char FuncName[]={"SUMA_GetListIchoice"};
   int ichoice;
   char *choice=NULL;
   SUMA_Boolean Found = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *CloseShop = NOPE;
   ichoice = -1;
   if (!LW) {
      SUMA_S_Err("NULL LW!");
      SUMA_RETURN(ichoice);
   }


   if (  cbs->reason == XmCR_SINGLE_SELECT || 
         cbs->reason == XmCR_BROWSE_SELECT) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,
                  "%s: Single selection (reason %d, (%d, %d)),\n"
                  "list widget %s... \n", 
               FuncName, cbs->reason, 
               XmCR_SINGLE_SELECT, XmCR_BROWSE_SELECT , LW->Label);
   } else {
      if (LocalHead) 
         fprintf (SUMA_STDERR,
                  "%s: Default selection (reason %d, (%d, %d)),\n"
                  "list widget %s... \n", 
                  FuncName, cbs->reason, 
                  XmCR_SINGLE_SELECT, XmCR_BROWSE_SELECT, LW->Label);
      /*double click or enter on that one, close shop after selection */
      *CloseShop = YUP;
   }

   XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);

   if (LocalHead) 
      fprintf (SUMA_STDERR,
               "%s: Selected item: %s {%s} (%d)\n", 
               FuncName, choice, choice, cbs->item_position);
   LW->lastitempos = cbs->item_position;   /* store for next opening */
   /* because of sorting, choice cannot be used 
      as an index into clist and oplist in ALS */
   Found = NOPE;
   ichoice = 0;
   do {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Comparing:\t>%s<\t>%s<\n", 
                  FuncName, LW->ALS->clist[ichoice], choice);
      if (strcmp(LW->ALS->clist[ichoice], 
                  choice) == 0) Found = YUP; 
      else ++ichoice;
   } while (ichoice < LW->ALS->N_clist && !Found);
      
   if (!Found) { /* do older search, with strncmp dunno why it 
                     was like that
                     but I worry about backward compatibility */
      ichoice = 0;
      do {
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: Comparing:\t>%s<\t>%s<\n", 
                     FuncName, LW->ALS->clist[ichoice], choice);
         if (strncmp(LW->ALS->clist[ichoice], 
                     choice, strlen(choice)) == 0) Found = YUP; 
         else ++ichoice;
      } while (ichoice < LW->ALS->N_clist && !Found);
   }
   
   if (!Found) {
      SUMA_SLP_Err("Choice not found.");
      SUMA_RETURN(-1);
   }

   XtFree (choice);
   SUMA_RETURN(ichoice);
}
/*!
   \brief function that handles switching intensity from the list widget 
   \sa SUMA_cb_SwitchIntensity
*/
void SUMA_cb_SelectSwitchInt (
         Widget w, XtPointer client_data, 
         XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SelectSwitchInt"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_LIST_WIDGET *LW = NULL;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   int ichoice;
   SUMA_Boolean CloseShop = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SO = (SUMA_SurfaceObject *)client_data;
   LW = SO->SurfCont->SwitchIntMenu->lw;
   
   if ((ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop))==-1) {
      SUMA_RETURNe;
   }
   

   if (!SUMA_SelectSwitchDsetCol(SO, LW, 0, ichoice)) {
      SUMA_S_Err("Failed to SelectSwitchDsetCol");
      SUMA_RETURNe;
   }
   
   if (CloseShop) {
      SUMA_cb_CloseSwitchLst( w,  (XtPointer)LW,  call_data);
   }  
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
}
void SUMA_cb_SelectSwitchThr (
         Widget w, XtPointer client_data, 
         XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SelectSwitchThr"};
   SUMA_SurfaceObject *SO = NULL;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   SUMA_Boolean CloseShop = NOPE;
   SUMA_LIST_WIDGET *LW = NULL;
   int ichoice;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SO = (SUMA_SurfaceObject *)client_data;
   LW = SO->SurfCont->SwitchThrMenu->lw;
   
   if ((ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop))==-1) {
      SUMA_RETURNe;
   }
   
   if (!SUMA_SelectSwitchDsetCol(SO, LW, 1, ichoice)) {
      SUMA_S_Err("Failed to SelectSwitchDsetCol");
      SUMA_RETURNe;
   }

   if (CloseShop) {
      SUMA_cb_CloseSwitchLst( w,  (XtPointer)LW,  call_data);
   }  
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
}
void SUMA_cb_SelectSwitchBrt (
         Widget w, XtPointer client_data, 
         XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SelectSwitchBrt"};
   SUMA_SurfaceObject *SO = NULL;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   SUMA_Boolean CloseShop = NOPE;
   SUMA_LIST_WIDGET *LW = NULL;
   int ichoice;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SO = (SUMA_SurfaceObject *)client_data;
   LW = SO->SurfCont->SwitchBrtMenu->lw;
   
   if ((ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop))==-1) {
      SUMA_RETURNe;
   }

   if (!SUMA_SelectSwitchDsetCol(SO, LW, 2, ichoice)) {
      SUMA_S_Err("Failed to SelectSwitchDsetCol");
      SUMA_RETURNe;
   }
   
   if (CloseShop) {
      SUMA_cb_CloseSwitchLst( w,  (XtPointer)LW,  call_data);
   }  
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
}

   
int SUMA_SelectSwitchDsetCol(
         SUMA_SurfaceObject *SO, 
         SUMA_LIST_WIDGET *LW, 
         int block,
         int ichoice)
{
   static char FuncName[]={"SUMA_SelectSwitchDsetCol"};
   SUMA_MenuCallBackData data;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   if (!SO || !LW || block < 0 || block > 2 || ichoice < 0) SUMA_RETURN(0);
   
   /* now retrieve that choice from the 
      SUMA_ASSEMBLE_LIST_STRUCT structure and initialize  */
   if (LW->ALS) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,
                  "%s: N_clist = %d\n", 
                  FuncName, LW->ALS->N_clist); 
      if (LW->ALS->N_clist > ichoice) {
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Retrieved Column indexed %d\n", 
                     FuncName, (INT_CAST)LW->ALS->oplist[ichoice]);
         
         switch (block){
            case 0:
               if (!SUMA_SwitchColPlaneIntensity
                     (SO, SO->SurfCont->curColPlane, 
                      (INT_CAST)LW->ALS->oplist[ichoice], 1)) {
                  SUMA_SL_Err("Failed in SUMA_SwitchColPlaneIntensity");
               }
               break;
            case 1:
               if (!SUMA_SwitchColPlaneThreshold
                     (SO, SO->SurfCont->curColPlane, 
                      (INT_CAST)LW->ALS->oplist[ichoice], 1)) {
                  SUMA_SL_Err("Failed in SUMA_SwitchColPlaneThreshold");
               }
               break;
            case 2:
               if (!SUMA_SwitchColPlaneBrightness
                     (SO, SO->SurfCont->curColPlane, 
                      (INT_CAST)LW->ALS->oplist[ichoice], 1)) {
                  SUMA_SL_Err("Failed in SUMA_SwitchColPlaneBrightness");
               }
               break;
            default:
               SUMA_S_Err("Ah NON!");
               SUMA_RETURN(0);
               break;
         }
      }
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: NULL ALS\n", FuncName); 
   }

   SUMA_RETURN(1);
}

/*!
   \brief function that handles closing switch * list widget 
   expects LW in client_data
*/
void SUMA_cb_CloseSwitchLst (Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_CloseSwitchLst"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");

   LW = (SUMA_LIST_WIDGET *)client_data;
   
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
                     "%s: Destroying list widget %s...\n", 
                     FuncName, LW->Label);
         XtDestroyWidget(LW->toplevel);
         LW->toplevel = NULL;
         break;
      default:
         SUMA_S_Err("Not ready for this type of closing");
         SUMA_RETURNe;
         break;
   }
   
   LW->isShaded = YUP; 
   
   
   
   SUMA_RETURNe;
}


/*!
   \brief opens a list selection for choosing a color map 
*/
SUMA_Boolean SUMA_CmapSelectList(SUMA_SurfaceObject *SO, int refresh, 
                                 int bringup)
{
   static char FuncName[]={"SUMA_CmapSelectList"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SUMAg_CF->scm) {   
      SUMAg_CF->scm = SUMA_Build_Color_maps();
      if (!SUMAg_CF->scm) {
         SUMA_SL_Err("Failed to build color maps.\n");
         SUMA_RETURN(NOPE);
      }
   }
      
   /* Widget is common to all SUMA */
   LW = SUMAg_CF->X->SwitchCmapLst;
   
   if (!LW) {
      SUMA_LH("Allocating widget");
      /* need to create widget */
      LW = SUMA_AllocateScrolledList   (  
            "Switch Cmap", SUMA_LSP_BROWSE,
            NOPE,          NOPE,
            SO->SurfCont->TLS, SWP_POINTER_OFF,
            125,
            SUMA_cb_SelectSwitchCmap, (void *)SO,
            SUMA_cb_SelectSwitchCmap, (void *)SO,
            SUMA_cb_CloseSwitchCmap, NULL);

      SUMAg_CF->X->SwitchCmapLst = LW;
      refresh = 1; /* no doubt aboot it */
   } else {
      if ((void *)SO != LW->Default_Data || (void *)SO != LW->Select_Data) {
         /* just update the callback data info in LW */
         SUMA_UpdateScrolledListData(LW, (void *)SO, (void *)SO, NULL);
      }
   }

   if (refresh) {
      /* Now creating list*/
      if (LW->ALS) {
         if (LocalHead) SUMA_S_Err("Freeing the hag.");
         LW->ALS = SUMA_FreeAssembleListStruct(LW->ALS);
      }
      SUMA_LH("Assembling");
      LW->ALS = SUMA_AssembleCmapList(SUMAg_CF->scm->CMv, SUMAg_CF->scm->N_maps);
      if (!LW->ALS) {
         SUMA_SL_Err("Failed to assemble list");
         SUMA_RETURN(NOPE);
      }
      if (LW->ALS->N_clist < 0) {
         SUMA_SL_Err("Failed in SUMA_AssembleCmapList");
         SUMA_RETURN(NOPE);
      }
      if (!LW->ALS->N_clist) {
         SUMA_SLP_Note ("No cmaps to choose from.");
         SUMA_RETURN(NOPE);
      }
   }
   
   if (bringup) 
      SUMA_CreateScrolledList ( LW->ALS->clist, LW->ALS->N_clist, NOPE, LW);
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_SetCmodeMenuChoice(SUMA_SurfaceObject *SO, char *str)
{
   static char FuncName[]={"SUMA_SetCmodeMenuChoice"};
   int i, Nbutt = 0, nstr=0, nf=0;
   Widget whist = NULL, *w = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LHv("So(%p), SurfCont(%p), CmapModeMenu(%p)\n", 
            SO, SO->SurfCont, SO->SurfCont->CmapModeMenu);
   w = SO->SurfCont->CmapModeMenu->mw;
   if (!w) {
      SUMA_LH("NULL w");
      SUMA_RETURN(NOPE);
   }
   if (!str) {
      SUMA_S_Err("NULL str");
      SUMA_RETURN(NOPE);
   }
   /* what's your history joe ? */
   XtVaGetValues(  w[0], XmNmenuHistory , &whist , NULL ) ;  
   if (!whist) {
      SUMA_SL_Err("NULL whist!");
      SUMA_RETURN(NOPE);
   }

   if (LocalHead) { 
      fprintf (SUMA_STDERR,"%s: The history is NAMED: %s (%d buttons total)\n", 
               FuncName, XtName(whist), Nbutt);
   } 
   if (!strcasecmp(XtName(whist), str)) {
      SUMA_LHv("Current setting of %s same as %s, nothing to do.\n",
               XtName(whist),str);
      SUMA_RETURN(YUP);
   }   
   nstr = strlen(str);
   
   /* Now search the widgets in w for a widget labeled str */
   for (i=0; i< SW_N_CmapMode; ++i) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"I have %s, want %s\n", XtName(w[i]), str);
      nf = strcasecmp(str, XtName(w[i]));
      if (nf == 0) {
         SUMA_LH("Match!");
         XtVaSetValues(  w[0], XmNmenuHistory , w[i] , NULL ) ;  
         SUMA_SetCmapMode(SO, i);
         SUMA_RETURN(YUP);
     }
   }
   
   SUMA_RETURN(NOPE);
}

/*!
   This function will fail if the strings have been trunctated 
   Consider writing SetMenuChoiceUserData
*/ 
SUMA_Boolean SUMA_SetCmapMenuChoice(SUMA_SurfaceObject *SO, char *str)
{
   static char FuncName[]={"SUMA_SetCmapMenuChoice"};
   int i, Nbutt = 0, nstr=0, nf=0;
   Widget whist = NULL, *w = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LHv("So(%p), SurfCont(%p), SwitchCmapMenu(%p)\n", 
            SO, SO->SurfCont, SO->SurfCont->SwitchCmapMenu);
   w = SO->SurfCont->SwitchCmapMenu->mw;
   if (!w) {
      SUMA_LH("NULL w");
      SUMA_RETURN(NOPE);
   }
   if (!str) {
      SUMA_S_Err("NULL str");
      SUMA_RETURN(NOPE);
   }
   /* what's your history joe ? */
   XtVaGetValues(  w[0], XmNmenuHistory , &whist , NULL ) ;  
   if (!whist) {
      SUMA_SL_Err("NULL whist!");
      SUMA_S_Notev("So(%p), SurfCont(%p), SwitchCmapMenu(%p), %s\n", 
            SO, SO->SurfCont, SO->SurfCont->SwitchCmapMenu, str);
      SUMA_RETURN(NOPE);
   }

   if (LocalHead) { 
      fprintf (SUMA_STDERR,"%s: The history is NAMED: %s (%d buttons total)\n", 
               FuncName, XtName(whist), Nbutt);
   } 
      
   nstr = strlen(str);
   /* Now search the widgets in w for a widget labeled str */
   for (i=0; i< SO->SurfCont->SwitchCmapMenu->N_mw; ++i) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"I have %s, want %s\n", XtName(w[i]), str);
      if (nstr > strlen(XtName(w[i]))) { /* name in list got trunctated ...*/
         nf = strncmp(str, XtName(w[i]), strlen(XtName(w[i])));
      } else {
         nf = strcmp(str, XtName(w[i]));
      }
      if (nf == 0) {
         SUMA_LH("Match!");
         XtVaSetValues(  w[0], XmNmenuHistory , w[i] , NULL ) ;  
         SUMA_RETURN(YUP);
     }
   }
   
   SUMA_RETURN(NOPE);
}

int SUMA_SelectSwitchCmap_one( SUMA_SurfaceObject *SO, SUMA_LIST_WIDGET *LW,
                               int ichoice, SUMA_Boolean CloseShop, int setmen)
{
   static char FuncName[]={"SUMA_SelectSwitchCmap_one"};
   SUMA_COLOR_MAP *CM = NULL;
   char *choice=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !LW) SUMA_RETURN(0);
   
   /*  retrieve that choice from the SUMA_ASSEMBLE_LIST_STRUCT structure 
   and initialize the drawing window */
   if (LW->ALS) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: N_clist = %d\n", 
                     FuncName, LW->ALS->N_clist); 
      if (LW->ALS->N_clist > ichoice) {
         CM = (SUMA_COLOR_MAP *)LW->ALS->oplist[ichoice];
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: Retrieved Colmap named %s\n", 
                     FuncName, CM->Name);
         /* Now you need to set the button menu to reflect the choice made */
         if (!SUMA_SetCmapMenuChoice (SO, LW->ALS->clist[ichoice])) {
            SUMA_SL_Err("Failed in SUMA_SetCmapMenuChoice");
         }
         if (!SUMA_SwitchColPlaneCmap(SO, CM)) {
            SUMA_SL_Err("Failed in SUMA_SwitchColPlaneCmap");
         }
      }
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: NULL ALS\n", FuncName); 
   }

   if (CloseShop) {
      SUMA_cb_CloseSwitchCmap( NULL,  (XtPointer)LW,  NULL);
   }  
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(SO);
   
   
   SUMA_RETURN(1);
} 

int SUMA_SelectSwitchCmap( SUMA_SurfaceObject *SO, SUMA_LIST_WIDGET *LW,
                           int ichoice, SUMA_Boolean CloseShop, int setmen)
{
   static char FuncName[]={"SUMA_SelectSwitchCmap"};
   SUMA_SurfaceObject *SOC=NULL;
   SUMA_OVERLAYS *colpC=NULL, *colp = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !LW) SUMA_RETURN(0);
   
   if (!SUMA_SelectSwitchCmap_one(SO, LW, ichoice, CloseShop, setmen)) {
      SUMA_RETURN(0);
   }
   
   colp = SO->SurfCont->curColPlane;
   colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
   if (colpC && SOC) {
      SUMA_LHv("Found contralateral equivalent to:\n"
                   " %s and %s in\n"
                   " %s and %s\n",
                   SO->Label, CHECK_NULL_STR(colp->Label),
                   SOC->Label, CHECK_NULL_STR(colpC->Label));
      if (!SUMA_SelectSwitchCmap_one(SOC, LW, ichoice, 0, 1)) {
         SUMA_S_Warn("Failed in contralaterality");
         SUMA_RETURN(0);
      }
   } 
   
   SUMA_RETURN(1);
} 


/*!
   \brief function that handles switching colormap from the list widget 
   \sa SUMA_cb_SwitchCmap
*/
void SUMA_cb_SelectSwitchCmap (Widget w, XtPointer client_data, 
                               XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SelectSwitchCmap"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean CloseShop = NOPE;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   int ichoice = -1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SO = (SUMA_SurfaceObject *)client_data;
   LW = SUMAg_CF->X->SwitchCmapLst;
   
   ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop);

   if (!SUMA_SelectSwitchCmap(SO, LW, ichoice, CloseShop, 1)) {
      SUMA_S_Err("glitch");
      SUMA_RETURNe;
   }

   SUMA_RETURNe;
}

SUMA_Boolean SUMA_SwitchColPlaneCmap(SUMA_SurfaceObject *SO, SUMA_COLOR_MAP *CM)
{
   static char FuncName[]={"SUMA_SwitchColPlaneCmap"};
   SUMA_OVERLAYS *over = NULL;
   SUMA_Boolean LocalHead = NOPE;
   static int nwarn=0;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   if (!SO || !CM) { SUMA_RETURN(NOPE); }
   if (!SO->SurfCont) { SUMA_RETURN(NOPE); }
   
   over = SO->SurfCont->curColPlane;
   if (!over) { SUMA_RETURN(NOPE); }
   
   if (over->ShowMode == SW_SurfCont_DsetViewCon ||
       over->ShowMode == SW_SurfCont_DsetViewCaC ) { /* wants contours */
      if (SUMA_NeedsLinearizing(CM)) {
         if (!nwarn) {
            SUMA_SLP_Note("Cannot do contouring with colormaps\n"
                          "that panes of unequal sizes.\n"
                          "Contouring turned off.\n"
                          "Notice shown once per session.");
            ++nwarn;
         }
         over->ShowMode = SW_SurfCont_DsetViewCol;
         SUMA_Set_Menu_Widget( SO->SurfCont->DsetViewModeMenu,
                        SUMA_ShowMode2ShowModeMenuItem(over->ShowMode));
         /* kill current contours */
         SUMA_KillOverlayContours(over);
      } 
   } 
   
   SUMA_STRING_REPLACE(over->cmapname, CM->Name);
   if (!SUMA_ColorizePlane (over)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(NOPE);
   }
   
   /* reset zoom and translation vectors */
   SO->SurfCont->cmp_ren->FOV = SUMA_CMAP_FOV_INITIAL;
   SO->SurfCont->cmp_ren->translateVec[0] = 
   SO->SurfCont->cmp_ren->translateVec[1] = 
   SO->SurfCont->cmp_ren->translateVec[2] = 0.0;

   /* update the color map display NOW, no workprocess crap. ZSS Mar. 7 08*/
   #if 0
   /* With this, the next call to SUMA_RemixRedisplay,
   causes an error: glXSwapBuffers: no context for this drawable
   because SUMA_cmap_wid_handleRedisplay is still to be processed
   as SUMA_cmap_wid_postRedisplay puts it in as a workprocess.
   You need to force the immediate execution of 
   SUMA_cmap_wid_handleRedisplay which resets the context before 
   returning */
   SUMA_LH("Calling SUMA_cmap_wid_postRedisplay");
   SUMA_cmap_wid_postRedisplay(NULL, (XtPointer)SO, NULL);
   #else
   SUMA_cmap_wid_handleRedisplay((XtPointer)SO);
   #endif
   
   SUMA_LH("Calling SUMA_RemixRedisplay ");          
   SUMA_RemixRedisplay(SO);

   SUMA_LH("Returning");
   SUMA_RETURN(YUP);
}
/*!
   \brief function that handles closing switch colormap list widget 
   expects LW in client_data
*/
void SUMA_cb_CloseSwitchCmap (Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_CloseSwitchCmap"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SUMA_cb_CloseSwitchLst(w, client_data, call);   
      /* Now calling standard lst closing function       May 2009*/
   SUMA_RETURNe;  /* get out */
   
   /* Pre May 2009 */
   LW = (SUMA_LIST_WIDGET *)client_data;
   
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
         SUMA_S_Err("Not setup to deal with this closing type");
         SUMA_RETURNe;
         break;
   }
   
   LW->isShaded = YUP; 
   
   
   
   SUMA_RETURNe;
}

/* based on bbox.c's optmenu_EV */
void SUMA_optmenu_EV( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_optmenu_EV"};
   Dimension lw=0 ;
   Widget * children , wl = NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  num_children , ic ;
   SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)cd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ){
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || SO == NULL ) SUMA_RETURNe ;

   if( bev->button != Button3 ) SUMA_RETURNe ;
   
   if (LocalHead) {
      SUMA_LH("Les enfants de w");
      SUMA_ShowMeTheChildren(w);
   }
   
   /* get the widget named "OptionLabel" */
   wl = XtNameToWidget(w, "OptionLabel");
   if (!wl) { 
      SUMA_SL_Err("Failed to find la widget"); /* do continue */
   } else {  /* confine yourself to the label, young man */
      XtVaGetValues( wl , XmNwidth, &lw , NULL ) ;
      if( bev->x > lw ) SUMA_RETURNe ;
   }
   
   /* Need to create a list */
   SUMA_LHv("Now creating list XtName(w)=%s \n", XtName(w));
   if (strcmp(XtName(w), "I") == 0) {
      if (!SUMA_DsetColSelectList(SO, 0, 0, 1)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else if (strcmp(XtName(w), "T") == 0){
      if (!SUMA_DsetColSelectList(SO, 1, 0, 1)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else if (strcmp(XtName(w), "B") == 0){
      if (!SUMA_DsetColSelectList(SO, 2, 0, 1)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else if (strcmp(XtName(w), "Cmp") == 0){
      if (!SUMA_CmapSelectList(SO, 0, 1)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else {
      SUMA_SLP_Err("wahtchyoutalkinaboutwillis?");
      SUMA_RETURNe;
   }
   SUMA_RETURNe;

}


void SUMA_CreateXhairWidgets(Widget parent, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_CreateXhairWidgets"};
   char *Xhair_tit[]=   {  "Xhr ", NULL};
   char *Xhair_hint[]=  {  "Crosshair coordinates.", NULL};
   char *Xhair_help[]=  {  SUMA_SurfContHelp_Xhr , NULL};
   char *Node_tit[]=    {  "Node"   , NULL};
   char *Node_hint[]=   {  "Node index", NULL};
   char *Node_help[]=   {  SUMA_SurfContHelp_Node , NULL};
   char *Face_tit[]=    {  "Tri ", NULL};
   char *Face_hint[]=   {  "1- Triangle index, 2- Nodes forming tiangle", NULL};
   char *Face_help[]=   {  SUMA_SurfContHelp_Tri , NULL};
   char *Data_tit[]=    {  "    ", "Intens", "Thresh", "Bright" , NULL};
   char *Data_colhint[]=      {  "Data Values at node in focus", 
                                 "Intensity (I) value", 
                                 "Threshold (T) value", 
                                 "Brightness modulation (B) value" , NULL};
   char *Data_colhelp[]=      {  SUMA_SurfContHelp_NodeValTblc0, 
                                 SUMA_SurfContHelp_NodeValTblc1, 
                                 SUMA_SurfContHelp_NodeValTblc2, 
                                 SUMA_SurfContHelp_NodeValTblc3 , NULL}; 
   
   char *Data_rtit[]=      {  "    ", "Val " , NULL};
   char *Data_rowhint[]=   {  "Data Values at node in focus", 
                              "Data Values at node in focus" , NULL};
   char *Data_rowhelp[]=   {  SUMA_SurfContHelp_NodeValTblr0, 
                              SUMA_SurfContHelp_NodeValTblr0 , NULL};
   
   char *Label_tit[]=   {  "Lbl ", NULL};
   char *Label_hint[]=  {  "Color at node in focus", NULL};
   char *Label_help[]=  {  SUMA_SurfContHelp_NodeLabelTblr0 , NULL};
   
   Widget rcc;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* a row column to contain them all */
   rcc = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, parent,
      XmNpacking, XmPACK_TIGHT,
      XmNleftAttachment,XmATTACH_FORM ,  
      XmNorientation , XmVERTICAL ,
      XmNmarginHeight , 0 ,
      XmNmarginWidth  , 0 ,
      NULL);
   
   /* a simple table with the xhair coordinate */
   SUMA_LH("Creating Xhair coordinates table");
   {
      int colw[] = { 4, 27 };
      SUMA_CreateTable(rcc, 
         1, 2,
         Xhair_tit, NULL,
         Xhair_hint, NULL,
         Xhair_help, NULL,
         colw, YUP, SUMA_string,
         SUMA_XhairInput, (void*)SO,
         NULL, NULL, 
         NULL, NULL,  
         SO->SurfCont->XhairTable);
   }
   /* a table for a node's index */      
   SUMA_LH("Creating node table");
   {
      int colw[]={4, 6, 19};
      SUMA_CreateTable(rcc, 
         1, 3,
         Node_tit, NULL,
         Node_hint, NULL,
         Node_help, NULL,
         colw, YUP, SUMA_int,
         SUMA_NodeInput, (void*)SO,
         NULL, NULL, 
         NULL, NULL,  
         SO->SurfCont->NodeTable);
      /* disable the 3rd entry cell */
      SUMA_SetCellEditMode(SO->SurfCont->NodeTable, 0, 2, 0);
   }
   /* a table for the triangle in focus */      
   SUMA_LH("Creating Face  table");
   {
      int colw[]={4, 6, 19}  ; 
      SUMA_CreateTable(rcc, 
         1, 3,
         Face_tit, NULL,
         Face_hint, NULL,
         Face_help, NULL,
         colw, YUP, SUMA_int,
         SUMA_TriInput, (void*)SO,
         NULL, NULL, 
         NULL, NULL,  
         SO->SurfCont->FaceTable);
      /* disable the 3rd entry cell */
      SUMA_SetCellEditMode(SO->SurfCont->FaceTable, 0, 2, 0);   
   }
   /* a table for the Dset values at node in focus */      
   SUMA_LH("Creating Dset Val  table");
   {
      int colw[]={ 4, 7, 7, 7};
      SUMA_CreateTable(rcc, 
         2, 4,
         Data_rtit, Data_tit,
         Data_rowhint, Data_colhint,
         Data_rowhelp, Data_colhelp,
         colw, NOPE, SUMA_float,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SO->SurfCont->DataTable);
   }
   /* a table for a node's label*/      
   SUMA_LH("Creating label  table");
   {
      int colw[]={4, 26};
      SUMA_CreateTable(rcc, 
         1, 2,
         Label_tit, NULL,
         Label_hint, NULL,
         Label_help, NULL, 
         colw, NOPE, SUMA_string,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SO->SurfCont->LabelTable);
   }      
   XtManageChild(rcc);
   SUMA_RETURNe;
   
}

void SUMA_CreateCmapWidgets(Widget parent, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_CreateCmapWidgets"};
   char slabel[100];
   Widget rct, rcc, rco;
   XtVarArgsList arglist=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (SO->SurfCont->opts_rc) {
      SUMA_SL_Err("Non null opts_rc\nThis should not be.");
      SUMA_RETURNe;
   }
   
   SO->SurfCont->opts_form = XtVaCreateWidget ("form",
      xmFormWidgetClass, parent,
      NULL);
   
   SO->SurfCont->opts_rc = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, SO->SurfCont->opts_form,
      XmNpacking, XmPACK_TIGHT,
      XmNleftAttachment,XmATTACH_FORM ,  
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight , 0 ,
      XmNmarginWidth  , 0 ,
      NULL);

   { /* the threshold bar */
      rct = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SO->SurfCont->opts_rc,
         XmNpacking, XmPACK_TIGHT, 
         XmNresizeHeight, False, /* important that this rc is not to be resized
                                    automatically,
                                    otherwise, the fix SUMA_FORCE_SCALE_HEIGHT
                                    will fail 
                                   */
         XmNresizeWidth, False,
         XmNwidth, SUMA_SCALE_WIDTH,
         XmNheight,  SUMA_SCALE_HEIGHT, 
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight , 0 ,
         XmNmarginWidth  , 0 ,
         NULL);
      /* convenient common arguments for scales */
      arglist = XtVaCreateArgsList( NULL,
                                    XmNshowValue, True,
                                    XmNmaximum, 255,
                                    XmNscaleMultiple, 5,
                                    XmNheight,  SUMA_SCALE_HEIGHT,
                                    XmNuserData, (XtPointer)0,
                                    NULL);
                  
      /* put a string on top of the scale
      Can use XmNtitleString but it is placed on the side. 
      Too much waisted space */
      #if 0
      sprintf(slabel,"Thr.");
      SO->SurfCont->thr_lb = XtVaCreateManagedWidget (slabel, 
                                          xmLabelWidgetClass, rct,
                                          XmNwidth, SUMA_SCALE_WIDTH,
                                          XmNrecomputeSize, False,   
                                             /* don't let it change size, 
                                             it messes up the slider */ 
                                          NULL);
      #else
      { 
         int colw[]={6};
         char *lhint[]={ "Threshold Value (append p to set by p value)", NULL};
         char *lhelp[]={ SUMA_SurfContHelp_SetThreshTblr0, NULL};
         if (!SO->SurfCont->SetThrScaleTable->cells) {
            SUMA_CreateTable( rct,
                              1, 1, 
                              NULL, NULL,  
                              lhint, NULL,  
                              lhelp, NULL,  
                              colw, YUP, SUMA_float, 
                              SUMA_cb_SetScaleThr, (void *)SO,
                              NULL, NULL,
                              NULL, NULL,  
                              SO->SurfCont->SetThrScaleTable);                                     
         }
      }
      #endif
      /* add a vertical scale for the intensity */
      SO->SurfCont->thr_sc = XtVaCreateManagedWidget("Thr.",
                                          xmScaleWidgetClass, rct,
                                          XtVaNestedList, arglist,
                                          NULL);
#ifdef USING_LESSTIF
   if (LocalHead) fprintf(stderr,"\n========= setting width to %d\n",
                                 SUMA_SCALE_SLIDER_WIDTH);
   XtVaSetValues( SO->SurfCont->thr_sc, 
                  XmNscaleWidth, SUMA_SCALE_SLIDER_WIDTH , NULL ) ;
#endif


      XtAddCallback (SO->SurfCont->thr_sc, 
                     XmNvalueChangedCallback, 
                     SUMA_cb_set_threshold, 
                     (XtPointer) SO);
      
      XtAddCallback (SO->SurfCont->thr_sc, 
                     XmNdragCallback, 
                     SUMA_cb_set_threshold_label, 
                     (XtPointer) SO); 
      
      /* put a string for the pvalue */
      sprintf(slabel,"p [N/A]\nq [N/A]");
      SO->SurfCont->thrstat_lb = XtVaCreateManagedWidget ("font8", 
                                          xmLabelWidgetClass, rct,
                                          XmNwidth, SUMA_SCALE_WIDTH,
                                          XmNrecomputeSize, False,
                                          LABEL_ARG(slabel),
                                          XmNinitialResourcesPersistent, False ,
                                          NULL);
      MCW_register_help( SO->SurfCont->thrstat_lb ,
                         SUMA_SurfContHelp_ThreshStats);
      MCW_register_hint( SO->SurfCont->thrstat_lb , 
                         "Nominal p-value per node; FDR q-value" ) ;

      XtManageChild (rct);
   }/* the threshold bar */
                     
   if (arglist) XtFree(arglist); arglist = NULL;
   
   {/* the color bar */
      Widget rcc2;
      rcc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SO->SurfCont->opts_rc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight , 0 ,
         XmNmarginWidth  , 0 ,
         NULL);
      
      sprintf(slabel,"   ");
      SO->SurfCont->cmaptit_lb = XtVaCreateManagedWidget (slabel, 
                                          xmLabelWidgetClass, rcc,
                                          NULL);
      /* need another rc for the cmap to avoid having 
      the glxarea resized by cmaptit_lb
      and SwitchCmapMenu */
      rcc2 = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight , 0 ,
         XmNmarginWidth  , 0 ,
         NULL);
      
      /* open me a glxarea */
      SUMA_LH("Forming glxarea");
      #ifdef SUMA_MOTIF_GLXAREA
         SO->SurfCont->cmp_ren->cmap_wid = XtVaCreateManagedWidget("glxarea",
             glwMDrawingAreaWidgetClass, rcc2,
             GLwNvisualInfo, SUMAg_SVv[0].X->VISINFO,
             XtNcolormap, SUMAg_SVv[0].X->CMAP,
             XmNwidth,   SUMA_CMAP_WIDTH,
             XmNheight,  SUMA_CMAP_HEIGHT,
             NULL);
      #else
         SO->SurfCont->cmp_ren->cmap_wid = 
               XtVaCreateManagedWidget("glxarea",
                                       glwDrawingAreaWidgetClass, rcc2,
                                       GLwNvisualInfo, SUMAg_SVv[0].X->VISINFO,
                                       XtNcolormap, SUMAg_SVv[0].X->CMAP,
                                       XmNwidth,   SUMA_CMAP_WIDTH,
                                       XmNheight,  SUMA_CMAP_HEIGHT,
                                       NULL);
      #endif
      XtManageChild (rcc2);
      
      SUMA_LH("Callbacks on glxarea");
      /* add me some callbacks */
      XtAddCallback( SO->SurfCont->cmp_ren->cmap_wid, 
                     GLwNginitCallback, SUMA_cmap_wid_graphicsInit, 
                     (XtPointer )SO);
      XtAddCallback( SO->SurfCont->cmp_ren->cmap_wid, 
                     GLwNexposeCallback, SUMA_cmap_wid_expose, 
                     (XtPointer )SO);
      XtAddCallback( SO->SurfCont->cmp_ren->cmap_wid, 
                     GLwNresizeCallback, SUMA_cmap_wid_resize, 
                     (XtPointer )SO);
      XtAddCallback( SO->SurfCont->cmp_ren->cmap_wid, 
                     GLwNinputCallback, SUMA_cmap_wid_input, 
                     (XtPointer )SO);
      
      XtManageChild (rcc);
   }  /* the colorbar */
   
   /* The options will be created as needed, when colorplanes are switched.
   see SUMA_InitializeColPlaneShell */
   
   XtManageChild (SO->SurfCont->opts_rc);
   XtManageChild (SO->SurfCont->opts_form);
   SUMA_RETURNe;
} 

SUMA_MenuItem *SUMA_FreeMenuVector(SUMA_MenuItem *menu, int Nels)
{
   static char FuncName[]={"SUMA_FreeMenuVector"};
   int i;
   
   SUMA_ENTRY;
   
   if (!menu) {SUMA_RETURN(NULL);}
   if (Nels <= 0) {SUMA_RETURN(NULL);}
   
   for (i=0; i<Nels; ++i) {
      if (menu[i].label) SUMA_free(menu[i].label);
      if (menu[i].accelerator) SUMA_free(menu[i].accelerator);
      if (menu[i].accel_text) SUMA_free(menu[i].accel_text);
      if (menu[i].subitems) { SUMA_SL_Err("Don't know how to free subitems yet."); }
   }
   SUMA_free(menu);
   
   SUMA_RETURN(NULL);
}

SUMA_MenuItem *SUMA_FormSwitchColMenuVector(SUMA_SurfaceObject *SO, int what, int *N_items)
{
   static char FuncName[]={"SUMA_FormSwitchColMenuVector"};
   SUMA_MenuItem *menu = NULL;
   int i, isarrow;
   void (*callback)();
   NI_element *nel = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN (menu);}
   if (!SO->SurfCont) { SUMA_SL_Err("NULL SO->SurfCont"); SUMA_RETURN (menu);}
   if (!SO->SurfCont->curColPlane) { SUMA_SL_Err("NULL SO->SurfCont->curColPlane"); SUMA_RETURN (menu);}
   if (!SO->SurfCont->curColPlane->dset_link) { SUMA_SL_Err("NULL SO->SurfCont->curColPlane->dset_link"); SUMA_RETURN (menu);}

   nel = SO->SurfCont->curColPlane->dset_link->dnel;
   if (!nel) { SUMA_SL_Err("NULL nel"); SUMA_RETURN (menu);}
   if (!nel->vec_num) { SUMA_SL_Err("no vecs"); SUMA_RETURN (menu);}
   
   /* decide on callback */
   switch (what) {
      case 0: 
         callback = SUMA_cb_SwitchIntensity;
         break;
      case 1:
         callback = SUMA_cb_SwitchThreshold;
         break;
      case 2:
         callback = SUMA_cb_SwitchBrightness;
         break;
      default:
         SUMA_SL_Err("No such what");
         SUMA_RETURN(menu);
   }
   
   /* Allocate for menu */
   menu = (SUMA_MenuItem *)SUMA_calloc((nel->vec_num+1), sizeof(SUMA_MenuItem));
   isarrow = SUMA_AllowArrowFieldMenus((nel->vec_num+1), "I");
                                          /* "I", or "B", or "T" OK */   

   /* fillup menu */
   for (i=0; i < nel->vec_num; ++i) {
      if (!isarrow) {
         menu[i].label = 
            SUMA_DsetColLabelCopy(SO->SurfCont->curColPlane->dset_link, i, 1);
      } else {
         /* SUMA_DsetColLabelCopy is slow as a dog for very large numbers
            of sub-bricks. In any case, sub-brick labels are not that 
            important here, this should be improved someday*/
         menu[i].label = (char*)malloc(13*sizeof(char));
         snprintf(menu[i].label,11*sizeof(char), "sb%d", i-1);
      }
      menu[i].class = &xmPushButtonWidgetClass;
      menu[i].mnemonic = '\0';
      menu[i].accelerator = NULL;
      menu[i].accel_text = NULL;
      menu[i].callback = callback;
      menu[i].callback_data = (XTP_CAST)(i+1); /* DO NOT USE THE zeroth item */
      menu[i].subitems = NULL;
   }
   
   /* add the stop sign. That's what SUMA_BuildMenu uses to figure out the 
      number of elements */
   menu[nel->vec_num].label = NULL;
   
   *N_items = nel->vec_num;
      
   SUMA_RETURN (menu);
}

void SUMA_ShowMeTheChildren(Widget w) {
   SUMA_DoForTheChildren(w,0, 0, 0);
}

void SUMA_UnmanageTheChildren(Widget w) {
   SUMA_DoForTheChildren(w, -1, 0, 0);
}
void SUMA_ManageTheChildren(Widget w) {
   SUMA_DoForTheChildren(w, 1, 0, 0);
}

/*!
   Perform operation on the children of a widget
   i = 0: Write out the names of the widgets
       1: Manage widgets
      -1: Unmanage widgets
*/ 
void SUMA_DoForTheChildren(Widget w, int i, int lvl, int rec)
{
   static char FuncName[]={"SUMA_DoForTheChildren"};
   Widget * children = NULL;
   int  num_children=0, num_children2=0, ic , Nbutt=0, kk=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   XtVaGetValues( w ,         XmNchildren    , &children ,
                              XmNnumChildren , &num_children , 
                              XmNbuttonCount, &Nbutt,
                              NULL ) ;
   for( ic=0 ; ic < num_children ; ic++ ){
      if (rec) { /* recursive, but ugly output */
         XtVaGetValues( children[ic] , XmNnumChildren , &num_children2, NULL);
         if (num_children2) SUMA_DoForTheChildren(children[ic],i, lvl+1, rec);
      }
      if (LocalHead) {
         /* what's the name jane ? */
         XtVaGetValues (children[ic], XmNbuttonCount, &Nbutt, NULL);
         for (kk=0; kk<lvl; ++kk) fprintf (SUMA_STDERR,"  "); 
         fprintf (SUMA_STDERR,   "%d.%d: %s (%d N_butts)\n", 
                                 lvl, ic, XtName(children[ic]), Nbutt);
      }
      switch (i) {
         case 1: 
            XtManageChild(children[ic]);
            break;
         case -1:
            XtUnmanageChild(children[ic]);
            break;
         case 0:
            /* what's the name jane ? */
            XtVaGetValues (children[ic], XmNbuttonCount, &Nbutt, NULL);
            for (kk=0; kk<lvl; ++kk) fprintf (SUMA_STDERR,"  "); 
            fprintf (SUMA_STDERR,   "%d.%d: %s (%d N_butts)\n", 
                                     lvl,  ic, XtName(children[ic]), Nbutt);
            break;
         default:
            SUMA_S_Err("Action %d unknown");
            SUMA_RETURNe;
      }
   }
   
   if (i==0 || LocalHead) {
      for (kk=0; kk<lvl; ++kk) fprintf (SUMA_STDERR,"  "); 
      fprintf (SUMA_STDERR, 
            "%s: Widget '%s' (lvl %d) has (%d) children (%d N_butts):\n", 
            FuncName, XtName(w), lvl, num_children, Nbutt);
   }
   SUMA_RETURNe;
}


SUMA_MenuItem *SUMA_FormSwitchCmapMenuVector(SUMA_COLOR_MAP **CMv, int N_maps)
{
   static char FuncName[]={"SUMA_FormSwitchCmapMenuVector"};
   SUMA_MenuItem *menu = NULL;
   int i;
   void (*callback)();
   NI_element *nel = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!CMv) { SUMA_SL_Err("NULL CMv"); SUMA_RETURN (menu);}
   if (N_maps <=0) { SUMA_SL_Err("N_maps <=0"); SUMA_RETURN (menu);}

   callback = SUMA_cb_SwitchCmap;
   
   /* Allocate for menu */
   menu = (SUMA_MenuItem *)SUMA_calloc((N_maps+1),sizeof(SUMA_MenuItem));
   
   /* fillup menu */
   for (i=0; i < N_maps; ++i) {
      menu[i].label = SUMA_copy_string(CMv[i]->Name);
      menu[i].class = &xmPushButtonWidgetClass;
      menu[i].mnemonic = '\0';
      menu[i].accelerator = NULL;
      menu[i].accel_text = NULL;
      menu[i].callback = callback;
      menu[i].callback_data = (XtPointer)CMv[i]; 
                        /* (used to be i+1)DO NOT USE THE 0 for the first button.                            0th index is reserved for the rc widget */
      menu[i].subitems = NULL;
   }
   
   /* add the stop sign. That's what SUMA_BuildMenu uses to figure out the number of elements */
   menu[N_maps].label = NULL;
      
   SUMA_RETURN (menu);
}

/* This one here, recalculates the p and q value for a new threshold
and displays the results on the widget
*/
void SUMA_UpdatePvalueField (SUMA_SurfaceObject *SO, float thresh)   
{/* set the pvalue */ 
   static char FuncName[]={"SUMA_UpdatePvalueField"};
   float p[3], zval = -1.0;
   int statcode;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) { 
      SUMA_SL_Err("NULL SO");
      SUMA_RETURNe; 
   }  
   if (!SO->SurfCont || 
       !SO->SurfCont->thr_sc ||
       !SO->SurfCont->curColPlane ||
       !SO->SurfCont->curColPlane->dset_link) { 
      SUMA_SL_Err("NULL SurfCont");
      SUMA_RETURNe; 
   }
     
   /* see if you can get the stat codes */
   if (!SUMA_GetDsetColStatAttr(  
            SO->SurfCont->curColPlane->dset_link, 
            SO->SurfCont->curColPlane->OptScl->tind, 
            &statcode,
            p, (p+1), (p+2))) {
      SUMA_LH("Error");        
   }else if (statcode) {
      SUMA_LHv("Have stats at sb %d\n"
               "statcode %d: %f %f %f\n", 
               SO->SurfCont->curColPlane->OptScl->tind,
               statcode, p[0], p[1], p[2]);
      SO->SurfCont->curColPlane->OptScl->ThreshStats[0] = 
            THD_stat_to_pval( thresh , statcode , p  ) ;
      
      SUMA_LHv("Have pval of %f\n", 
               SO->SurfCont->curColPlane->OptScl->ThreshStats[0]);
      if( SO->SurfCont->curColPlane->OptScl->ThreshStats[0] >= 0.0 ){
         SUMA_LH("zvaling ...\n")
         zval = SUMA_fdrcurve_zval( 
                           SO->SurfCont->curColPlane->dset_link, 
                           SO->SurfCont->curColPlane->OptScl->tind, 
                           thresh) ;
         if( zval > 0.0f ){
            SO->SurfCont->curColPlane->OptScl->ThreshStats[1] = 
                     2.0*qg(zval) ;         /* convert z back to FDR q */
         }
      } 
   } else {
      /* no stats */
      SO->SurfCont->curColPlane->OptScl->ThreshStats[0] = -1.0;
      SO->SurfCont->curColPlane->OptScl->ThreshStats[1] = -1.0;
   }
   SUMA_LHv("statcode %d: %f %f %f\n"
            "Thresh %f, p %f, q %f\n", 
            statcode, p[0], p[1], p[2],
            thresh, 
            SO->SurfCont->curColPlane->OptScl->ThreshStats[0], 
            SO->SurfCont->curColPlane->OptScl->ThreshStats[1]);
   
   
   { /* form the text, a la afni */
      char buf[100]={"Rien"};
      float pval = SO->SurfCont->curColPlane->OptScl->ThreshStats[0];
      float qval = SO->SurfCont->curColPlane->OptScl->ThreshStats[1];
      if( pval < 0.0 ){
        strcpy( buf , "p=N/A") ;
      } else {
        if( pval == 0.0 ){
          strcpy( buf , "p=0" ) ;
        } else if( pval >= 0.9999 ){
          strcpy( buf , "p=1" ) ;
        } else if( pval >= 0.0010 ){
          char qbuf[16] ;
          sprintf( qbuf , "%5.4f" , pval ) ;
          strcpy(buf,"p=") ; strcat( buf , qbuf+1 ) ;/*qbuf+1 skips leading 0*/
        } else {
          int dec = (int)(0.999 - log10(pval)) ;
          zval = pval * pow( 10.0 , (double) dec ) ;  /* between 1 and 10 */
          if( dec < 10 ) sprintf( buf , "p=%3.1f-%1d" ,           zval , dec ) ;
          else           sprintf( buf , "p=%1d.-%2d"  , (int)rint(zval), dec ) ;
        }
      }
      if( qval > 0.0f && qval < 0.9999 ){
         char qbuf[16] ;
         if( qval >= 0.0010 ) sprintf(qbuf,"%5.4f",qval) ;
         else {
           int dec = (int)(0.999 - log10(qval)) ;
           zval = qval * pow( 10.0 , (double)dec ) ;  /* between 1 and 10 */
           if( dec < 10 ) sprintf( qbuf, " %3.1f-%1d",            zval, dec );
           else           sprintf( qbuf, " %1d.-%2d" , (int)rint(zval), dec );
         }
         strcat(buf,"\nq=") ; strcat(buf,qbuf+1) ;
      } else {
         strcat(buf,"\nq=N/A") ;
      }
     
      MCW_set_widget_label( SO->SurfCont->thrstat_lb, buf );
   }
   
   SUMA_RETURNe;
}
         

void SUMA_SetScaleRange(SUMA_SurfaceObject *SO, double range[2])   
{
   static char FuncName[]={"SUMA_SetScaleRange"};
   int min_v, max_v, scl, dec, cv=0;
   Widget w ;
   double dtmp;
   char slabel[100];
   static int nwarn = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) { 
      SUMA_SL_Err("NULL SO");
      SUMA_RETURNe; 
   }  
   if (!SO->SurfCont->thr_sc) { 
      SUMA_SL_Err("NULL widget");
      SUMA_RETURNe; 
   }
   
   w = SO->SurfCont->thr_sc;
   
   if (range[1] <= range[0]) range[1] = range[0] + 1;
   
   if (SO->SurfCont->curColPlane->OptScl->ThrMode == SUMA_ABS_LESS_THAN) {
      SUMA_LH("Absolutizing Threshold Range");
      if (fabs((double)range[0]) > fabs((double)range[1])) {
         range[1] = fabs((double)range[0]); range[0] = 0.0;
      } else {
         range[1] = fabs((double)range[1]); range[0] = 0.0;
      }
   }
   if (range[1] - range[0] > pow(10.0,SUMAg_CF->SUMA_ThrScalePowerBias)) { 
      /* no need for power */
      dec = 0;
      if (range[0] > MRI_maxint) min_v=MRI_maxint;
      else if (range[0] < -MRI_maxint) min_v=-MRI_maxint;
      else min_v = (int)(range[0] ); 
      if (range[1] > MRI_maxint) max_v=MRI_maxint;
      else if (range[1] < -MRI_maxint) max_v=-MRI_maxint;
      else max_v = (int)(range[1] ); 
      dtmp = 2.0*(max_v-min_v)/MRI_maxint;
      if (dtmp >= 0.499) { 
         /* See comment for similar block below */
         if (!nwarn) 
            SUMA_S_Warn( "Data range too big for threshold scale.\n"
                         "Range will be trimmed.\n"
                         "Similar messages will be muted");
          ++nwarn;
         /* Range cannot be too big. 
               X11 Warning 1:
               Name: Thr.
               Class: XmScale
               (Maximum - minimum) cannot be greater than INT_MAX / 2;
               minimum has been set to zero, 
               maximum may have been set to (INT_MAX/2).
         */
         max_v /= (dtmp+0.01);
         min_v /= (dtmp+0.01);
      }
      scl = (max_v -min_v) / 10; 
   } else {
      /* what power of 10 is needed (based on Bob's settings in afni_wid.c)? */
      dec = (int)ceil( log((double)(range[1] - range[0] + 0.001)) / log (10) );
      /* Add the scale bias, so that dec is at least = bias*/
      if (dec < SUMAg_CF->SUMA_ThrScalePowerBias) 
         dec = SUMAg_CF->SUMA_ThrScalePowerBias;
      dtmp = (range[0] * pow(10.0, dec));
      if (dtmp > MRI_maxint) min_v=MRI_maxint;
      else if (dtmp < -MRI_maxint) min_v=-MRI_maxint;
      else min_v = (int)(dtmp); 
      dtmp = range[1] * pow(10.0, dec);
      if (dtmp > MRI_maxint) max_v=MRI_maxint;
      else if (dtmp < -MRI_maxint) max_v=-MRI_maxint;
      else max_v = (int)(dtmp + 0.001); 
      dtmp = 2.0*(max_v-min_v)/MRI_maxint;
      if (dtmp >= 0.499) { 
         /* Perhaps I should include a scale value multiplier = 1/dtmp 
            so that users can still get the full range.
            Problem is that users will not see the proper value 
            on the silder bar, unless I start setting it explicitly
            in the widget. Best to wait and see who'd need it. */
         if (!nwarn) 
            SUMA_S_Warn( "Data range too big for threshold scale.\n"
                         "Range will be trimmed\n"
                         "Similar messages will be muted");
          ++nwarn;         
          /* Range cannot be too big. 
               X11 Warning 1:
               Name: Thr.
               Class: XmScale
               (Maximum - minimum) cannot be greater than INT_MAX / 2;
               minimum has been set to zero, 
               maximum may have been set to (INT_MAX/2).
         */
         max_v /= (dtmp+0.01);
         min_v /= (dtmp+0.01);
      }

      scl = (max_v -min_v) / 10; 
   }
     
   if (max_v <= min_v || scl < 0) { 
      /* happens when max_v is so large that you get trash when typecast to int.
         That's the case when you're using the number of nodes in a surface 
         for example and you set dec to something demented like 6 ! 
         Need a clever function here */
      SUMA_SLP_Note("Bad auto scaling \nparameters for threshold bar.\n"
                    "Using defaults"); 
      min_v = (int)(range[0]); 
      max_v = (int)(range[1])+1; 
      scl = (max_v - min_v) / 10;
      dec = 1;     
   }
   
   #if 0
   /* make sure the current value is not less than the min or greater 
      than the max */
   XtVaGetValues(w, XmNvalue, &cv, NULL);
   #else
   /* what was the slider's previous value in this dset ?*/
   dtmp = SO->SurfCont->curColPlane->OptScl->ThreshRange[0] * pow(10.0, dec);
   if (dtmp > 0) cv = (int) (dtmp+0.5);
   else cv = (int) (dtmp-0.5);
   #endif

   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s:\n"
               " min %d max %d scalemult %d decimals %d\n"
               "Current scale value %d\n", 
                  FuncName, min_v, max_v, scl, dec, cv);  
   if (cv < min_v) {
      cv = min_v;
      /* must update threshold value in options structure*/
      SO->SurfCont->curColPlane->OptScl->ThreshRange[0] = 
         (float)cv / pow(10.0, dec); 
   } else if (cv > max_v) {
      cv = max_v;
      SO->SurfCont->curColPlane->OptScl->ThreshRange[0] = 
         (float)cv / pow(10.0, dec); 
   }
   /* set the slider bar */
   XtVaSetValues(w,  
            XmNmaximum, max_v, 
            XmNminimum, min_v,
            XmNvalue, cv, 
            XmNscaleMultiple, scl,  
            XmNdecimalPoints , dec,
            XmNuserData, (XTP_CAST)dec,   
            NULL);   
            
   /* set the label on top */
   if (SO->SurfCont->curColPlane->OptScl->ThrMode != SUMA_ABS_LESS_THAN) 
      sprintf(slabel, "%5s", MV_format_fval((float)cv / pow(10.0, dec))); 
   else {
      /* used to use this:
      sprintf(slabel, "|%5s|", .... 
      but that does not work in the editable field ... */
      sprintf(slabel, "%5s", MV_format_fval((float)cv / pow(10.0, dec))); 
   }
   /* SUMA_SET_LABEL(SO->SurfCont->thr_lb,  slabel);*/
      SUMA_INSERT_CELL_STRING(SO->SurfCont->SetThrScaleTable, 0,0,slabel); 
   
   SUMA_UpdatePvalueField (SO, 
                           SO->SurfCont->curColPlane->OptScl->ThreshRange[0]);
   
   SUMA_RETURNe;
}


/*!
   \brief A function to update the field showing
   Xhair coordinates.
   Xhair is a property of the viewer rather than
   the surface but it makes more sense to display
   the coordinates, in certain cases, in the 
   surface controllers
*/
SUMA_Boolean SUMA_UpdateXhairField(SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_UpdateXhairField"};
   int i=0, N_SOlist=0, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   SUMA_DO *dov = SUMAg_DOv;
   SUMA_SurfaceObject *SO=NULL, *curSO=NULL;
   char str[100]={""};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (!sv) SUMA_RETURN(NOPE);
   
   /* Which surfaces are visible in this SV ? */
   N_SOlist = SUMA_VisibleSOs(sv, dov, SOlist);
   for (i=0; i<N_SOlist; ++i) {
      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: working %d/%d shown surfaces ...\n", 
                                          FuncName, i, N_SOlist);
      SO = (SUMA_SurfaceObject *)dov[SOlist[i]].OP;
      if (SO->SurfCont) { /* does this surface have surface controller ? */
         /* is this controller, displaying information for that surface ? */
         curSO = *(SO->SurfCont->curSOp);
         if (curSO == SO) {
            /* OK, show the coordinate */
            if (LocalHead) 
               fprintf(SUMA_STDERR, "%s: Setting cross hair at %f %f %f\n", 
                                    FuncName, 
                                    sv->Ch->c[0],sv->Ch->c[1],sv->Ch->c[2]);
            SUMA_XHAIR_STRING(sv->Ch->c, str);
            SUMA_LH(str);
            XtVaSetValues(SO->SurfCont->XhairTable->cells[1], 
                           XmNvalue, str, NULL);
            SUMA_UpdateCrossHairNodeLabelField(sv);
         }
      }
      
   }
   
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_UpdateNodeField(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_UpdateNodeField"};
   int i=0;
   SUMA_OVERLAYS *Sover=NULL, *targetSover=NULL;
   NI_group *ngr=NULL;
   DListElmt *el=NULL;
   SUMA_SurfaceObject *curSO=NULL, *targetSO=NULL;
   SUMA_CALLBACK *cb=NULL;
   char *targetSO_idcode=NULL, *targetSover_name=NULL, *lbls=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (!SO || !SO->SurfCont) SUMA_RETURN(NOPE);
   
   curSO = *(SO->SurfCont->curSOp);

   Sover = SO->SurfCont->curColPlane; 
   
   /* Do we have click callbacks pending? */
   if (SUMAg_CF->callbacks && !SUMAg_CF->HoldClickCallbacks) {
      el = dlist_head(SUMAg_CF->callbacks);
      while (el) {
         cb = (SUMA_CALLBACK *)el->data;
         if (  cb->event == SUMA_NEW_NODE_ACTIVATE_EVENT && 
               cb->active > 0 && 
               cb->pending) {
            SUMA_LHv("Calling active pending click callback %s\n", 
                     cb->FunctionName);
            if (!SUMA_ExecuteCallback(cb, 1, SO, 0)) {
               SUMA_S_Err("Failed to execute callback");
               break;
            }
         } else {
            if (cb->active == -1) {
               SUMA_LH("Callback inactive");
            }
         }
         el = dlist_next(el);
      }
   } 

   if (SUMA_isRelated (SO, curSO, 1)) {
      SUMA_LH( "Updating GUI Node Fields, "
               "whereami is handled in SUMA_UpdateNodeLblField");
      SUMA_UPDATE_ALL_NODE_GUI_FIELDS(SO);
   } else {
      SUMA_LH("No GUI Node Field Updates, but may use a whereami update");
      if (SUMAg_CF->X->Whereami_TextShell) {
         lbls = SUMA_GetLabelsAtNode(SO, SO->SelectedNode);
         if (lbls) SUMA_free(lbls); lbls = NULL;
      }
   }

   if (  !SO->SurfCont->ShowCurForeOnly || 
         SO->SurfCont->GraphHidden) {   /* graph updating can be done 
                                          for all planes */
      for (i=0; i<SO->N_Overlays; ++i) {
         Sover = SO->Overlays[i];
         if (     Sover 
               && Sover->dset_link 
               && Sover->rowgraph_mtd ) {
            SUMA_OverlayGraphAtNode(Sover, 
                                    SO, 
                                    SO->SelectedNode);
         }
      }
   } else {
      Sover = SO->SurfCont->curColPlane;
      if (     Sover 
               && Sover->dset_link 
               && Sover->rowgraph_mtd ) {
            SUMA_OverlayGraphAtNode(Sover, 
                                    SO, 
                                    SO->SelectedNode);
         }
   }

         
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_UpdateNodeNodeField(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_UpdateNodeNodeField"};
   char str[100];

   SUMA_ENTRY; 

   if (!SO || !SO->SurfCont || !SO->SurfCont->NodeTable) SUMA_RETURN(NOPE);
   if (SO->SelectedNode < 0 || SO->SelectedNode >= SO->N_Node) SUMA_RETURN(NOPE);
   
   sprintf(str, "%d", SO->SelectedNode);
   SO->SurfCont->NodeTable->num_value[1] = SO->SelectedNode;
   XtVaSetValues(SO->SurfCont->NodeTable->cells[1], XmNvalue, str, NULL);
   sprintf(str, "%s, ", 
               MV_format_fval2(SO->NodeList[3*SO->SelectedNode], 7));
   strcat(str, MV_format_fval2(SO->NodeList[3*SO->SelectedNode+1], 7));
   strcat(str, ", ");
   strcat(str, MV_format_fval2(SO->NodeList[3*SO->SelectedNode+2], 7) );
   XtVaSetValues(SO->SurfCont->NodeTable->cells[2], XmNvalue, str, NULL);

   SUMA_RETURN(YUP);
}

char **SUMA_FormNodeValFieldStrings(SUMA_SurfaceObject *SO, 
                                 SUMA_DSET *dset, int Node,
                                 int find, int tind, int bind,
                                 int dec) 
{
   static char FuncName[]={"SUMA_FormNodeValFieldStrings"};
   char **sar=NULL;
   double dval;
   int Found = -1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !dset) SUMA_RETURN(sar);
   
   /* 1- Where is this node in the data set ? */
   if (Node > -1) {
      Found = SUMA_GetNodeRow_FromNodeIndex_s(  dset, Node, SO->N_Node );
   } else {
      Found = -1;
   }
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: Node index %d is at row %d in dset %p "
               "(label %s, filename %s).\n", 
               FuncName, Node, Found, 
               dset, 
               SDSET_LABEL(dset),
               SDSET_FILENAME(dset));
   }
   
   if (Found >= 0) {   
      sar = (char **)SUMA_calloc(3, sizeof(char *));
      /* 2- What is the value of the intensity */
      if ((sar[0] = SUMA_GetDsetValInCol(dset, find, Found, &dval))) {
         if (dec > 0) {
            SUMA_free(sar[0]); 
            sar[0] = SUMA_copy_string(MV_format_fval2(dval, dec));
         } 
         SUMA_LHv("str_int=%s, dval = %f\n",sar[0], dval);
      } else {
         sar[0] = SUMA_copy_string("X");
         SUMA_SL_Err("Failed to get str_int");
      }
      if ((sar[1] = SUMA_GetDsetValInCol(dset, tind, Found, &dval))) {
         if (dec > 0) {
            SUMA_free(sar[1]); 
            sar[1] = SUMA_copy_string(MV_format_fval2(dval, dec));
         }
         SUMA_LHv("str_thr=%s, dval = %f\n",sar[1], dval);
      } else {
         sar[1] = SUMA_copy_string("X");
         SUMA_SL_Err("Failed to get str_thr");
      }
      if ((sar[2] = SUMA_GetDsetValInCol(dset, bind, Found, &dval))) {
         if (dec > 0) {
            SUMA_free(sar[2]); 
            sar[2] = SUMA_copy_string(MV_format_fval2(dval, dec));
         }
         SUMA_LHv("str_brt=%s, dval = %f\n",sar[2], dval);
      } else {
         SUMA_SL_Err("Failed to get str_brt");
         sar[2] = SUMA_copy_string("X");
      }
   } 
      
   SUMA_RETURN(sar);
}

SUMA_Boolean SUMA_UpdateNodeValField(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_UpdateNodeValField"};
   char **sar=NULL;
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SO) {
      SUMA_LH("Null SO");
      SUMA_RETURN(NOPE);
   }
   Sover = SO->SurfCont->curColPlane;
   
   if (!Sover) {
      SUMA_LH("Null Sover");
      SUMA_RETURN(NOPE);
   }
   

   if (!(sar = SUMA_FormNodeValFieldStrings(SO, Sover->dset_link, 
                           SO->SelectedNode, 
                           Sover->OptScl->find, 
                           Sover->OptScl->tind,
                           Sover->OptScl->bind, 0))) {
       SUMA_LH("Failed to get strings");
   }
   if (sar && sar[0]) {
      SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 1, sar[0]);
      SUMA_free(sar[0]); 
   } else {
      SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 1, "Err");
   }
   if (sar && sar[1]) {
      SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 2, sar[1]);
      SUMA_free(sar[1]); 
   } else {
      SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 2, "Err");
   }
   if (sar && sar[2]) {
      SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 3, sar[2]);
      SUMA_free(sar[2]); 
   } else {
      SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 3, "Err");
   }
   if (sar) SUMA_free(sar); sar = NULL;

   SUMA_RETURN(YUP);
}

/* Do we have label dsets for this surface ? */
char *SUMA_GetLabelsAtNode(SUMA_SurfaceObject *SO, int node) 
{
   static char FuncName[]={"SUMA_GetLabelsAtNode"};
   char *lbls=NULL;
   int key = -1, OverInd=-1, i0, i, sp;
   SUMA_DSET *cdset=NULL, *dd=NULL;
   SUMA_OVERLAYS *colplane=NULL, *Sover=NULL;
   SUMA_COLOR_MAP *CM=NULL;
   DListElmt *el=NULL, *NextElm=NULL;
   DList *list=NULL;
   SUMA_EngineData *ED=NULL;
   char **sar=NULL, stmp[64]={""};
   char *seps[3]={"I=", " T=", " B="};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   if (!SO) SUMA_RETURN(NOPE);
   
   el = dlist_head(SUMAg_CF->DsetList);
   while (el) {
      dd = (SUMA_DSET*)el->data;
      if (SUMA_isDsetRelated(dd, SO)) {
         SUMA_LHv("Have Dset %s related to SO\n", SDSET_LABEL(dd));
         /* is dd a LabelDset ? */
         if (  SUMA_is_Label_dset(dd, NULL) && node >= 0 ) {
            SUMA_LHv("dset %s will work with SO", SDSET_LABEL(dd));
            key = SUMA_GetDsetNodeValInCol2( dd, 0, 
                                             node, -1);
            /* get the overlay for that dset */
            if (key >= 0 &&
                (colplane = SUMA_Fetch_OverlayPointerByDset (
                     SO->Overlays, SO->N_Overlays, dd, &OverInd))) {
               /* get the colormap for that colplane */
               if ((CM = SUMA_FindNamedColMap (colplane->cmapname)) &&
                   CM->cname) {
                  if ((i0 = SUMA_ColMapKeyIndex(key, CM)) >= 0) {
                     if (!lbls) {
                        lbls = SUMA_copy_string(CM->cname[i0]);
                     } else {   
                        lbls = SUMA_append_replace_string(lbls,CM->cname[i0],
                                                       "|", 1); 
                     }
                  }
               }         
            }
         }
      } 
      el = dlist_next(el);
   }
   
   if ((Sover = SO->SurfCont->curColPlane) && 
        SDSET_TYPE(Sover->dset_link) != SUMA_NODE_RGB ) {
      if (!(sar = SUMA_FormNodeValFieldStrings(SO,       
                              Sover->dset_link, 
                              node, 
                              Sover->OptScl->find, 
                              Sover->OptScl->tind,
                              Sover->OptScl->bind, 5))) {
      } else if (1) { /* include sub-brick labels */
         if (lbls) lbls = SUMA_append_replace_string(lbls,"\n","",1);
         if (sar[0] && sar[1] && sar[2]) {
            if (!strcmp(sar[0],sar[1]) && !strcmp(sar[2],sar[1])) {
               lbls = SUMA_append_replace_string(lbls, "(I,T,B)", "",1);
               lbls = SUMA_append_replace_string(lbls, 
                        SUMA_DsetColLabel(Sover->dset_link, Sover->OptScl->find),
                                                 "",1);
               lbls = SUMA_append_replace_string(lbls, sar[0], "=",1);
            } else if (!strcmp(sar[0],sar[1])) {
               lbls = SUMA_append_replace_string(lbls, "(I,T)", "",1);
               lbls = SUMA_append_replace_string(lbls, 
                        SUMA_DsetColLabel(Sover->dset_link, Sover->OptScl->find),
                                                 "",1);
               lbls = SUMA_append_replace_string(lbls, sar[0], "=",1);

               lbls = SUMA_append_replace_string(lbls, " (B)", "",1);
               lbls = SUMA_append_replace_string(lbls, 
                        SUMA_DsetColLabel(Sover->dset_link, Sover->OptScl->bind),
                                                 "",1);                  
               lbls = SUMA_append_replace_string(lbls, sar[2], "=", 1);
            } else if (!strcmp(sar[1],sar[2])) {
               lbls = SUMA_append_replace_string(lbls, "(I)", "",1);
               lbls = SUMA_append_replace_string(lbls, 
                        SUMA_DsetColLabel(Sover->dset_link, Sover->OptScl->find),
                                                 "",1);
               lbls = SUMA_append_replace_string(lbls, sar[0], "=",1);

               lbls = SUMA_append_replace_string(lbls, " (B,T)", "",1);
               lbls = SUMA_append_replace_string(lbls, 
                        SUMA_DsetColLabel(Sover->dset_link, Sover->OptScl->bind),
                                                 "",1);                  
               lbls = SUMA_append_replace_string(lbls, sar[2], "=", 1);
            } else if (!strcmp(sar[0],sar[2])) {
               lbls = SUMA_append_replace_string(lbls, "(I,B)", "",1);
               lbls = SUMA_append_replace_string(lbls, 
                        SUMA_DsetColLabel(Sover->dset_link, Sover->OptScl->find),
                                                 "",1);
               lbls = SUMA_append_replace_string(lbls, sar[0], "=",1);

               lbls = SUMA_append_replace_string(lbls, " (T)", "",1);
               lbls = SUMA_append_replace_string(lbls, 
                        SUMA_DsetColLabel(Sover->dset_link, Sover->OptScl->tind),
                                                 "",1);                  
               lbls = SUMA_append_replace_string(lbls, sar[1], "=", 1);
            } else {
               for (sp=0,i=0;i<3;++i) {
                  if (sar[i]) {
                     if (sp) lbls = SUMA_append_replace_string(lbls," ","", 1);
                     lbls = SUMA_append_replace_string(lbls, 
                        SUMA_DsetColLabel(Sover->dset_link, 
                                             i==0 ? Sover->OptScl->find : 
                                             ( i == 1 ?  Sover->OptScl->tind:
                                                         Sover->OptScl->bind)),
                                                 "",1); 
                     lbls = SUMA_append_replace_string(lbls,sar[i],"=", 1);
                     sp=1;
                     SUMA_free(sar[i]); sar[i]=NULL;
                  }
               }
            }
         } else {
               for (sp=0,i=0;i<3;++i) {
                  if (sar[i]) {
                     if (sp) lbls = SUMA_append_replace_string(lbls," ","", 1);
                     lbls = SUMA_append_replace_string(lbls, 
                        SUMA_DsetColLabel(Sover->dset_link, 
                                             i==0 ? Sover->OptScl->find : 
                                             ( i == 1 ?  Sover->OptScl->tind:
                                                         Sover->OptScl->bind)),
                                                 "",1); 
                     lbls = SUMA_append_replace_string(lbls,sar[i],"=", 1);
                     sp=1;
                     SUMA_free(sar[i]); sar[i]=NULL;
                  }
               }

         }
         SUMA_free(sar); sar=NULL;
         /* Any clusters? */
         if ((sp=SUMA_NodeClustNumber(Sover, node, SO, NULL))) {
            sprintf(stmp,"\nIn cluster %d", sp);
            if (lbls) lbls = SUMA_append_replace_string(lbls,stmp,"",1);
         }
      } else { /* old approach to labeling, I,T, business only */
         if (lbls) lbls = SUMA_append_replace_string(lbls,"\n","",1);
         if (sar[0] && sar[1] && sar[2]) {
            if (!strcmp(sar[0],sar[1]) && !strcmp(sar[2],sar[1])) {
               lbls = SUMA_append_replace_string(lbls, sar[0], "I,T,B=",1);
            } else if (!strcmp(sar[0],sar[1])) {
               lbls = SUMA_append_replace_string(lbls, sar[0], "I,T=",1);
               lbls = SUMA_append_replace_string(lbls, sar[2], " B=", 1);
            } else if (!strcmp(sar[1],sar[2])) {
               lbls = SUMA_append_replace_string(lbls, sar[0], "I=",1);
               lbls = SUMA_append_replace_string(lbls, sar[1], " B,T=", 1);
            } else {
               for (i=0;i<3;++i) {
                  if (sar[i]) {
                     lbls = SUMA_append_replace_string(lbls,sar[i],
                                                    seps[i], 1); 
                     SUMA_free(sar[i]); sar[i]=NULL;
                  }
               }
            }
         } else {
            for (i=0;i<3;++i) {
               if (sar[i]) {
                  lbls = SUMA_append_replace_string(lbls,sar[i],
                                                 seps[i], 1); 
                  SUMA_free(sar[i]); sar[i]=NULL;
               }
            }
         }
         SUMA_free(sar); sar=NULL;
      }
   }
      
   /* Do we need to tell Santa? */
   if (lbls && SUMAg_CF->X->Whereami_TextShell) {
      if (!list) list = SUMA_CreateList();
      ED = SUMA_InitializeEngineListData (SE_Whereami);
      if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                    SEF_vp, (void *)SO,
                                    SES_Suma, NULL, NOPE,
                                    SEI_Head, NULL))) {
         fprintf (SUMA_STDERR, 
                  "Error %s: Failed to register command.\n", 
                  FuncName);
      }
      if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                    SEF_s, (void *)lbls, /* Copy by value */
                                    SES_Suma, NULL, NOPE,
                                    SEI_In, NextElm))) {
         fprintf (SUMA_STDERR, 
                  "Error %s: Failed to add data.\n", 
                  FuncName);
      }
                  
      if (!SUMA_Engine (&list)) {
         fprintf(stderr, 
                  "Error %s: SUMA_Engine call failed.\n", FuncName);
      }
      SUMA_free(lbls); lbls = NULL; 
   }
   SUMA_RETURN(lbls);
}

/* transform string to a TextNIDO,
   If sv is not NULL, 
      then NIDO is added to SUMAg_DOv and registered with sv
      Do not free what it returned because it is added to SUMA's DOv
   else
      the returned nido is all yours to manage
   
*/
SUMA_NIDO *SUMA_NodeLabelToTextNIDO (char *lbls, SUMA_SurfaceObject *SO, 
                                     SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_StringToTextNIDO"};
   static void * default_font=GLUT_BITMAP_9_BY_15;
   int i=0;
   float txcol[4] = {0.2, 0.5, 1, 1.0};
   float default_color[4] = {0.2, 0.5, 1, 1.0}, *v;
   float topscr[3]={ 0.5, 1.0, 0.0 };
   SUMA_DO_CoordType coord_type = SUMA_WORLD;
   SUMA_DO_CoordUnits default_coord_units = SUMA_WORLD_UNIT;
   SUMA_NIDO *nido=NULL;
   NI_element *nini = NULL;
   
   SUMA_ENTRY;
   
   
   if (0) { /* on crosshair, does not look so nice. Keep it for the record*/
      nido = SUMA_BlankNIDO(NULL, "AHorseWithNoName",
                         SO->LocalDomainParentID, NULL, NULL);
      nini = NI_new_data_element("T", 0);
   
      v = &(SO->NodeList[3*SO->SelectedNode]);
      coord_type = SUMA_WORLD;
   } else {/* fixed on screen */
      nido = SUMA_BlankNIDO(NULL, "AHorseWithNoName",
                         NULL, "fixed", NULL);
      nini = NI_new_data_element("T", 0);
      coord_type = SUMA_SCREEN;
      NI_set_attribute(nini,"v_align", "top");
      NI_set_attribute(nini,"h_align", "center");
      v = topscr;
   }
   NI_SET_FLOATv( nini, "coord", v, 3);
   NI_set_attribute(nini,"text", lbls);
   NI_set_attribute(nini, "font", SUMA_EnvVal("SUMA_CrossHairLabelFont"));
   for (i=0;i<3;++i) default_color[i] = 1.0 - sv->clear_color[i];
   NI_SET_FLOATv( nini, "col", default_color,3);
   
   NI_add_to_group(nido->ngr, nini);
   
   if (sv) {
      /* addDO (repeated nidos will get replaced)
         As long as its ID remains the same, then 
         SUMA_AddDO, will free its precursor and 
         replace with the new one*/
      if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)nido, 
                      NIDO_type, coord_type)) {
         fprintf( SUMA_STDERR,"Error %s: Failed in SUMA_AddDO. (leak)\n", 
                  FuncName);
         SUMA_RETURN(NULL);
      }

      /* register DO with viewer */
      if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO. (leak)\n", 
                 FuncName);
         SUMA_RETURN(NULL);
      }
   }
   
   SUMA_RETURN(nido);
}


/*! 
   Updates places where a node's value is shown 
*/
SUMA_Boolean SUMA_UpdateNodeLblField(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_UpdateNodeLblField"};
   int Found = -1;
   char *lbls=NULL, *ltmp=NULL;
   char str_col[100];
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
      
   if (!SO || (!SO->SurfCont && !SUMAg_CF->X->Whereami_TextShell)) {
      /* absolutely nothing to do */
      SUMA_RETURN(NOPE);
   } 
   
   /* get labels from Label Datasets, and update whereami window if needed*/
   lbls = SUMA_GetLabelsAtNode(SO, SO->SelectedNode);
   SUMA_LHv("Label Dsets: %s\n", lbls);
   
   if (SO->SurfCont) {
      Sover = SO->SurfCont->curColPlane;
      if (!Sover) {
         SUMA_RETURN(NOPE);
      }

      if (!Sover->ShowMode > 0) {
         SUMA_LH("Col plane hidden");
         sprintf(str_col,"hidden color overlay");
         if (lbls) lbls = SUMA_append_replace_string(lbls, str_col, ",", 1);
         else lbls = SUMA_copy_string(str_col);
         SUMA_INSERT_CELL_STRING(SO->SurfCont->LabelTable, 0, 1, lbls);
         if (lbls) SUMA_free(lbls); lbls = NULL;
         SUMA_RETURN(YUP);
      }

      Found = SUMA_GetNodeOverInd(Sover, SO->SelectedNode);

      if (Found < 0) {
         SUMA_Boolean Reasoned = NOPE;
         SUMA_LH("Node not found.\nLikely masked by threshold");
         sprintf(str_col,"masked");
         /* try to find out why it is masked */
         if (!Reasoned && SO->SurfCont->DataTable) {
            SUMA_LH("Checking if there is no data for this node");
            {
               void *n=NULL;
               XtVaGetValues(
               SO->SurfCont->DataTable->cells[1*SO->SurfCont->DataTable->Ni+1], 
                  XmNvalue, &n, NULL);
               if (strcmp((char *)n, "NoData") == 0) {
                  /* no data at all */
                  sprintf(str_col,"no data for this node");
                  Reasoned = YUP;
               }
            }
         }
         if (!Reasoned && SO->SurfCont->DataTable) { /* is the value 0 ? */
            SUMA_LH("Checking if node value is zero & zero is not being shown");
            if (Sover->OptScl->MaskZero && 
           SO->SurfCont->DataTable->num_value[1*SO->SurfCont->DataTable->Ni+1]) {
               sprintf(str_col,"masked by zero value");  
            }   
         }
      } else {
         {

            /* Now we know what the index of this node 
               is in the overlay plane (and the data) */
            sprintf(str_col,"%s",              
                            MV_format_fval2(Sover->ColVec[3*Found],5));
            strcat( str_col,", ");
            strcat( str_col, MV_format_fval2(Sover->ColVec[3*Found+1],5)); 
            strcat( str_col,", ");
            strcat( str_col, MV_format_fval2(Sover->ColVec[3*Found+2],5));
            SUMA_LH(str_col);
         }
      }
      if (lbls) ltmp = SUMA_append_replace_string(lbls, str_col, ",", 0);
      else ltmp = SUMA_copy_string(str_col);
      SUMA_INSERT_CELL_STRING(SO->SurfCont->LabelTable, 0, 1, ltmp);
      SUMA_free(ltmp); ltmp=NULL;
   } 
   
   if (lbls) SUMA_free(lbls); lbls = NULL;
   
   SUMA_RETURN(YUP);
}

 
SUMA_Boolean SUMA_UpdateCrossHairNodeLabelField(SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_UpdateCrossHairNodeLabelField"};
   char *lbls=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
      
   if (!sv || !sv->Ch || sv->Ch->SurfaceID < 0) {
      /* nothing to do */
      SUMA_RETURN(NOPE);
   } 
   
   if (!(SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Ch->SurfaceID].OP))) {
      SUMA_RETURN(NOPE);
   }
   
   if ( sv->ShowLabelAtXhair &&
       (lbls = SUMA_GetLabelsAtNode(SO, sv->Ch->NodeID))) {
      SUMA_NodeLabelToTextNIDO (lbls, SO, sv);
      SUMA_free(lbls); lbls = NULL;
   } else {
      SUMA_NodeLabelToTextNIDO ("", SO, sv);
   }
   
   SUMA_RETURN(YUP);
}

int SUMA_UpdateCrossHairNodeLabelFieldForSO(SUMA_SurfaceObject *curSO) 
{
   static char FuncName[]={"SUMA_UpdateCrossHairNodeLabelFieldForSO"};
   int i=0, iup=0;
   SUMA_SurfaceViewer *sv=NULL;
   
   SUMA_ENTRY;
   
   if (!curSO) SUMA_RETURN(0);
   
   /* update any viewer that is showing this 
      surface */
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
         /* is this viewer showing curSO ? */
         if (SUMA_isRegisteredSO(&(SUMAg_SVv[i]), SUMAg_DOv, curSO)) {
            sv = &(SUMAg_SVv[i]);
            SUMA_UpdateCrossHairNodeLabelField(sv);
            ++iup;
         }
      }
   }

   SUMA_RETURN(iup);
}


SUMA_Boolean SUMA_UpdateTriField(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_UpdateTriField"};
   SUMA_SurfaceObject *curSO=NULL;
   char str[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (!SO) SUMA_RETURN(NOPE);
   
   if (SO->SurfCont) {
      curSO = *(SO->SurfCont->curSOp);
      if (curSO == SO) {
         if (SO->SelectedFaceSet >= 0) {
            sprintf(str, "%d", SO->SelectedFaceSet);
            SO->SurfCont->FaceTable->num_value[1] = SO->SelectedFaceSet;
            XtVaSetValues(SO->SurfCont->FaceTable->cells[1], 
                          XmNvalue, str, NULL);
            sprintf(str, "%d, %d, %d", 
               SO->FaceSetList[3*SO->SelectedFaceSet], 
               SO->FaceSetList[3*SO->SelectedFaceSet+1],
               SO->FaceSetList[3*SO->SelectedFaceSet+2]);
            XtVaSetValues(SO->SurfCont->FaceTable->cells[2], 
                          XmNvalue, str, NULL);
         } else {
            XtVaSetValues(SO->SurfCont->FaceTable->cells[1], 
                          XmNvalue, "-1", NULL);
            SO->SurfCont->FaceTable->num_value[1] = -1;
            XtVaSetValues(SO->SurfCont->FaceTable->cells[2], 
                          XmNvalue, "x, x, x", NULL);
         }
      }
   }
         
   SUMA_RETURN(YUP);
   
}

/*!
   \brief set the values of the cross-hair group in the surface controller
   
   \sa SUMA_Init_SurfCont_SurfParam
   \sa SUMA_InitializeColPlaneShell
   \sa the set of SUMA_UpdateNode*Field functions
*/
SUMA_Boolean SUMA_Init_SurfCont_CrossHair(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_Init_SurfCont_CrossHair"};
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SO) SUMA_RETURN(YUP);

   /* set the cross hair and related fields */
   SUMA_UpdateTriField(SO);
   SUMA_UpdateNodeField(SO);
   /* look for a viewer that is showing this surface and has 
      this surface in focus*/
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if (LocalHead) fprintf (SUMA_STDERR,
                              "%s: Checking viewer %d.\n", FuncName, i);
      if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
         /* is this viewer showing SO ? */
         if (SUMA_isVisibleSO(&(SUMAg_SVv[i]), SUMAg_DOv, SO)) {
            if ((SUMAg_DOv[SUMAg_SVv[i].Focus_SO_ID].OP) == SO) {
               SUMA_UpdateXhairField(&(SUMAg_SVv[i]));
            }
         }
      }
   }

   SUMA_RETURN(YUP);
}

/*!
   Load Cmap callback 
   Expect SO in data
*/
void SUMA_cb_Cmap_Load(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_Cmap_Load"};
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
   ED = SUMA_InitializeEngineListData (SE_OpenCmapFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, (void *)SO,
                                          SES_Suma, NULL, NOPE,
                                          SEI_Head, NULL))) {
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
   }
   SUMA_RegisterEngineListCommand (  list, ED,
                                     SEF_ip, (void *)SO->SurfCont->TLS,
                                     SES_Suma, NULL, NOPE,
                                     SEI_In, NextElm);
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
   SUMA_RETURNe;
}

SUMA_COLOR_MAP *SUMA_LoadCmapFile_eng(char *filename)
{
   static char FuncName[]={"SUMA_LoadCmapFile_eng"};
   SUMA_COLOR_MAP *Cmap=NULL;
   SUMA_DSET_FORMAT form;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   /* find out if file exists and how many values it contains */
   if (!SUMA_filexists(filename)) {
      SUMA_S_Err("File not found");
      SUMA_RETURN(NULL);
   }

   /* take a stab at the format */
   form = SUMA_GuessFormatFromExtension(filename, NULL);
   
   /* load the baby */
   Cmap = NULL;
   switch (form) {
      case  SUMA_1D:
         Cmap = SUMA_Read_Color_Map_1D (filename);
         if (Cmap == NULL) {
            SUMA_S_Err("Could not load colormap.");
            SUMA_RETURN(NULL); 
         }
         break;
      case SUMA_ASCII_NIML:
      case SUMA_BINARY_NIML:
      case SUMA_NIML:
         Cmap = SUMA_Read_Color_Map_NIML(filename);
         break;
      default:
         SUMA_S_Err(  "Format not recognized.\n"
                       "I won't try to guess.\n"
                       "Do use the proper extension.");
         break;
   }
   
   SUMA_RETURN(Cmap);
}

/*! Loads a colormap file and adds it to the list of colormaps */
void SUMA_LoadCmapFile (char *filename, void *data)
{
   static char FuncName[]={"SUMA_LoadCmapFile"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_OVERLAY_PLANE_DATA sopd;
   SUMA_IRGB *irgb=NULL;
   int OverInd = -1, lnp=-1, loc[2];
   char *np=NULL;
   int bringup = 0;
   SUMA_DSET_FORMAT form;
   DList *list=NULL;
   SUMA_LIST_WIDGET *LW=NULL;
   SUMA_COLOR_MAP *Cmap=NULL;
   SUMA_PARSED_NAME * pn=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   if (!SUMAg_CF->scm) {   
      SUMAg_CF->scm = SUMA_Build_Color_maps();
      if (!SUMAg_CF->scm) {
         SUMA_SL_Err("Failed to build color maps.\n");
         SUMA_RETURNe;
      }
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,
               "%s: Received request to load %s \n", 
               FuncName, filename);
   }

   if (!SUMA_filexists(filename)) {
      SUMA_SLP_Err("File not found");
      SUMA_RETURNe;
   }
   
   if (!(Cmap=SUMA_LoadCmapFile_eng(filename))) {
      SUMA_SLP_Err("Failed to load cmap. File format may not have been\n"
                   "recognized from filename extension.\n"
                   "Use either .1D.cmap or .niml.cmap for name extension.\n"); 
      SUMA_RETURNe;
   }
   /* have Cmap, add to dbase */

   /* remove path from name for pretty purposes */
   pn = SUMA_ParseFname(Cmap->Name, NULL);
   SUMA_STRING_REPLACE(Cmap->Name, pn->FileName_NoExt);
   SUMA_Free_Parsed_Name(pn); pn = NULL;
   SUMAg_CF->scm->CMv = SUMA_Add_ColorMap (Cmap, SUMAg_CF->scm->CMv, 
                                             &(SUMAg_CF->scm->N_maps)); 
   
   /* Now you need to close any pre-existing switch Cmap window */
   bringup = 0;
   if (SUMAg_CF->X->SwitchCmapLst) {
      if (SUMAg_CF->X->SwitchCmapLst->toplevel && 
         !SUMAg_CF->X->SwitchCmapLst->isShaded) {
         /* close that baby */
         SUMA_cb_CloseSwitchCmap( NULL,  (XtPointer)SUMAg_CF->X->SwitchCmapLst,  
                                  NULL);
         bringup = 1;
      }
   }

   if (data) {
      SO = (SUMA_SurfaceObject *)data;

      if (LocalHead) {
         fprintf (SUMA_STDERR,
                  "%s: bonding colormap %s to surface %s.\n", 
                  FuncName, filename, SO->Label);
      }

      /* refresh the list */
      SUMA_CmapSelectList(SO, 1, bringup);

      /* update the menu buttons */
      SUMA_CreateUpdatableCmapMenu(SO);

      /* Set the menu button to the current choice */
      if (!SUMA_SetCmapMenuChoice (SO, Cmap->Name)) {
         SUMA_SL_Err("Failed in SUMA_SetCmapMenuChoice");
      }

      /* switch to the recently loaded  cmap */
      if (!SUMA_SwitchColPlaneCmap(SO, Cmap)) {
         SUMA_SL_Err("Failed in SUMA_SwitchColPlaneCmap");
      }

      /* update Lbl fields */
      SUMA_UpdateNodeLblField(SO);
   }
   

   SUMA_RETURNe;
}

/* Insert a color map from a dataset into the database.
   See function LoadCmapFile for details */   
SUMA_Boolean  SUMA_Insert_Cmap_of_Dset(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_Insert_Cmap_of_Dset"};
   NI_group *ngr=NULL;
   SUMA_COLOR_MAP *Cmap=NULL;
   SUMA_PARSED_NAME * pn=NULL;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(NOPE);
   if (!SUMAg_CF->scm) {   
      SUMAg_CF->scm = SUMA_Build_Color_maps();
      if (!SUMAg_CF->scm) {
         SUMA_SL_Err("Failed to build color maps.\n");
         SUMA_RETURN(NOPE);
      }
   }

   if (!(ngr = SUMA_NI_Cmap_of_Dset(dset))) {
      SUMA_RETURN(YUP); /* OK, nothing to do */
   }
   
   /* turn cmap into SUMA's structure */
   if (!(Cmap = SUMA_NICmapToCmap(ngr))) {
      SUMA_S_Err("Failed in translation");
      SUMA_RETURN(NOPE);
   }
   
   /* remove path from name for pretty purposes */
   pn = SUMA_ParseFname(Cmap->Name, NULL);
   SUMA_STRING_REPLACE(Cmap->Name, pn->FileName_NoExt);
   SUMA_Free_Parsed_Name(pn); pn = NULL;
   SUMAg_CF->scm->CMv = SUMA_Add_ColorMap (Cmap, SUMAg_CF->scm->CMv, 
                                             &(SUMAg_CF->scm->N_maps)); 

   /* Set cmap name to go with dset SRT stands for SUMA_RUN_TIME attribute
   This might be a way to kill attributes before writing out dataset */
   NI_set_attribute(dset->ngr, "SRT_use_this_cmap", Cmap->Name);
   
   SUMA_RETURN(YUP);
}
     
     
