/*! File to contain functions for creating interface
for mapping data to color maps. Lots of functions from
xim.c display.c and pbar.c*/

#include "SUMA_suma.h"
 
#undef STAND_ALONE

#if defined SUMA_SHOW_CMAP_STAND_ALONE
#define STAND_ALONE
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */
#else
extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  
#endif

/* the method for hiding a surface viewer (and other controllers), used to have three options prior to Fri Jan  3 10:21:52 EST 2003
Now only SUMA_USE_WITHDRAW and NOT SUMA_USE_DESTROY should be used*/
#define SUMA_USE_WITHDRAW

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
   
   if (!fname) { if (verb) SUMA_SL_Err("NULL fname");  SUMA_RETURN(imar); }
   im = mri_read_ppm( fname ) ;
   if (!im) { 
      if (verb) { snprintf(stmp, 500 * sizeof(char), "Failed to read %s", fname); SUMA_SL_Err(stmp); }
      SUMA_RETURN(imar); 
   }
   
   rgb = MRI_RGB_PTR(im) ;
   *height = im->ny ;
   *width = im->nx ;
   imx = im->ny * im->nx;
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s:\nNx (width) = %d, Ny (height) = %d\n", FuncName, im->nx, im->ny);
   
   imar = (unsigned char *) SUMA_malloc(sizeof(unsigned char) * im->nx * im->ny * 4);
   if (!imar) {
      SUMA_SL_Crit("Failed to allocate.");
      mri_free(im) ;
      SUMA_RETURN(imar); 
   }
   
   for (ir = 0; ir < im->ny; ++ir) {
      for (ic = 0; ic < im->nx; ++ic) {
         i1d = ic + ir * im->nx; /* equivalent 1d index into row major image data */
         i1df = ic + (im->ny - ir - 1) * im->nx; /* column flipped index */
         i1d4 = 4 * i1d; i1d3 = 3*i1df; 
         imar[i1d4] = (unsigned char)rgb[i1d3]; alf  = (float)imar[i1d4];   ++i1d3; ++i1d4; 
         imar[i1d4] = (unsigned char)rgb[i1d3]; alf += (float)imar[i1d4];   ++i1d3; ++i1d4; 
         imar[i1d4] = (unsigned char)rgb[i1d3]; alf += (float)imar[i1d4];            ++i1d4; 
         imar[i1d4] = (unsigned char)(alf/3.0);
      }
   } 

   mri_free(im) ; im = NULL;
   
   SUMA_RETURN(imar); 
}

void SUMA_cmap_wid_graphicsInit (Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cmap_wid_graphicsInit"};
   XVisualInfo *SUMAg_cVISINFO;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   
   XtVaGetValues(w, GLwNvisualInfo, &SUMAg_cVISINFO, NULL);
   SO->SurfCont->cmap_context = glXCreateContext(XtDisplay(w), SUMAg_cVISINFO,
            0,                  /* No sharing. */
            True);              /* Direct rendering if possible. */
   
   /* Setup OpenGL state. */
   if (!glXMakeCurrent(XtDisplay(w), XtWindow(w), SO->SurfCont->cmap_context)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in glXMakeCurrent.\n \tContinuing ...\n", FuncName);
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
   int i;
   float topright[3] = { SUMA_CMAP_TOPLEFT };
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");

   if (!Cmap->SO) {
      SUMA_LH("Creating Cmap's SO");
      Cmap->SO = SUMA_Cmap_To_SO(Cmap, orig, topright, 0);
      if (!Cmap->SO) { SUMA_SL_Err("Failed to create SO"); }
   }
    
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
   glDrawElements (GL_TRIANGLES, (GLsizei)Cmap->SO->N_FaceSet*3, GL_UNSIGNED_INT, Cmap->SO->glar_FaceSetList);
   
   /* Here you could draw a contour around the color cells. 
   But you'll need to raise the contour over the filled polygons because they'll get
   covered otherwise */ 
   #if 0
   { 
      GLfloat *LineCol=NULL;   
      LineCol = (GLfloat *)SUMA_calloc(Cmap->SO->N_Node*4, sizeof(GLfloat));
      for (i=0; i<Cmap->SO->N_Node; ++i) { LineCol[4*i] = LineCol[4*i+1] = LineCol[4*i+2] = 0.1; LineCol[4*i+3] = 1.0; }
      glColorPointer (4, GL_FLOAT, 0, LineCol);
      SUMA_SET_GL_RENDER_MODE(SRM_Line); 
      glDrawElements (GL_TRIANGLES, (GLsizei)Cmap->SO->N_FaceSet*3, GL_UNSIGNED_INT, Cmap->SO->glar_FaceSetList);
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
   GLfloat translateVec[]={0.0, 0.0, 0.0};
   GLfloat RotaCenter[]={0.0, 0.0, 0.0};
   SUMA_COLOR_MAP *Cmap = NULL;
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
    
   SUMA_ENTRY;
   
   SUMA_LH("in, lots of inefficiencies here, make sure you revisit");
   
   /* now you need to set the clear_color since it can be changed per viewer Thu Dec 12 2002 */
   glClearColor (clear_color[0], clear_color[1],clear_color[2],clear_color[3]);
      
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Building Rotation matrix ...\n", FuncName);
   SUMA_build_rotmatrix(rotationMatrix, currentQuat);
    
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: performing glClear ...\n", FuncName);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); /* clear the Color Buffer and the depth buffer */
   
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Setting up matrix mode and perspective ...\nFOV_initial=%f\n", FuncName, SUMA_CMAP_FOV_INITIAL);
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluPerspective(SUMA_CMAP_FOV_INITIAL, (double)SUMA_CMAP_WIDTH/SUMA_CMAP_HEIGHT, SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); /*lower angle is larger zoom,*/

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   if (0){
   SUMA_SL_Note("no need for shananigans\n"
                  "But to illustrate ...\n");
   glTranslatef (translateVec[0], translateVec[1], translateVec[2] );
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

   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Flushing or swapping ...\n", FuncName);
   
   if (SUMAg_SVv[0].X->DOUBLEBUFFER)
      glXSwapBuffers(XtDisplay(SO->SurfCont->cmap_wid), XtWindow(SO->SurfCont->cmap_wid));
   else  
      glFlush();

   /* Avoid indirect rendering latency from queuing. */
   if (!glXIsDirect(XtDisplay(SO->SurfCont->cmap_wid), SO->SurfCont->cmap_context))
      glFinish();

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
   
   SUMA_LH("Making cmap_wid current");
   if (!glXMakeCurrent(XtDisplay(SO->SurfCont->cmap_wid), XtWindow(SO->SurfCont->cmap_wid), SO->SurfCont->cmap_context)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in glXMakeCurrent.\n \tContinuing ...\n", FuncName);
   }
   
   SUMA_cmap_wid_display(SO);
   glFinish();
   
   /* insist on a glXMakeCurrent for surface viewer */
   SUMA_LH("Making sv's GLXAREA current");
   SUMA_SiSi_I_Insist();
   
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

void SUMA_cmap_wid_input(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cmap_wid_input"};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   
   SUMA_RETURNe;
}

void SUMA_cb_set_threshold_label(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_threshold_label"};
   SUMA_SurfaceObject *SO=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff ;
   int dec;
   char slabel[100];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   if (SO->SurfCont->curColPlane->OptScl->ThrMode != SUMA_ABS_LESS_THAN) 
      sprintf(slabel, "%5s", MV_format_fval((float)cbs->value / pow(10.0, dec))); 
   else 
      sprintf(slabel, "|%5s|", MV_format_fval((float)cbs->value / pow(10.0, dec))); 
   SUMA_SET_LABEL(SO->SurfCont->thr_lb,  slabel);
   
   /* You must use the line below if you are calling this function on the fly */
   /* SUMA_FORCE_SCALE_HEIGHT(SO);  */
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   SUMA_RETURNe;
}

void SUMA_cb_set_threshold(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_threshold"};
   SUMA_SurfaceObject *SO=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff ;
   int dec;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   SO = (SUMA_SurfaceObject *)clientData;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURNe; }
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   SO->SurfCont->curColPlane->OptScl->ThreshRange[0] = (float)cbs->value / pow(10.0, dec); 
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nThreshold set to %f\n",FuncName, SO->SurfCont->curColPlane->OptScl->ThreshRange[0]); 
   }
   
   if (SO->SurfCont->curColPlane->OptScl->UseThr && SO->SurfCont->curColPlane->OptScl->tind >=0) {
      SUMA_ColorizePlane(SO->SurfCont->curColPlane);
      SUMA_RemixRedisplay(SO);
   }

   /* call this one since it is not being called as the slider is dragged. */
   SUMA_cb_set_threshold_label(w, clientData, call);   

   /* sad as it is */
   SUMA_FORCE_SCALE_HEIGHT(SO); 

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   SUMA_UpdateNodeValField(SO);
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURNe;

}

void SUMA_cb_SwitchIntensity(Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_SwitchIntensity"};
   int imenu = 0;
   char srange[500];
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (int)datap->callback_data; 
   
   if (!SO->SurfCont->curColPlane) { SUMA_RETURNe; }
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n request to switch intensity to col. %d\n", FuncName, imenu - 1);
      fprintf(SUMA_STDERR, "SO->Label = %s\n", SO->Label);
   }
   
   SO->SurfCont->curColPlane->OptScl->find = imenu - 1;

   SUMA_InitRangeTable(SO) ;

   if (!SO->SurfCont->curColPlane->Show) { SUMA_RETURNe; } /* nothing else to do */
   
   
   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   
   SUMA_RemixRedisplay(SO);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
 
   SUMA_UpdateNodeValField(SO);
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
}

void SUMA_cb_SwitchThreshold(Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_SwitchThreshold"};
   int imenu = 0;
   char srange[500];
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_SurfaceObject *SO = NULL;
   float range[2]; int loc[2];  
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (int)datap->callback_data; 
   
   if (!SO->SurfCont->curColPlane) { SUMA_RETURNe; }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n request to switch threshold to col. %d\n", FuncName, imenu -1);
   }
   SO->SurfCont->curColPlane->OptScl->tind = imenu - 1;
   
   if (SUMA_GetColRange(SO->SurfCont->curColPlane->dset_link->nel, SO->SurfCont->curColPlane->OptScl->tind, range, loc)) {   
      SUMA_SetScaleRange(SO, range );
   }else {
      SUMA_SLP_Err("Failed to get range");
      SUMA_RETURNe;
   }
    
   SUMA_InitRangeTable(SO) ;
   
   SUMA_UpdateNodeValField(SO);
   
   if (!SO->SurfCont->curColPlane->OptScl->UseThr) { SUMA_RETURNe; } /* nothing else to do */

   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_RemixRedisplay(SO);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   SUMA_UpdateNodeLblField(SO);
   
   SUMA_RETURNe;
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
   imenu = (int)datap->callback_data; 
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n request to switch brightness to col. %d\n", FuncName, imenu - 1);
   }
   
   SO->SurfCont->curColPlane->OptScl->bind = imenu - 1;

   SUMA_InitRangeTable(SO) ;

   SUMA_UpdateNodeValField(SO);
   if (!SO->SurfCont->curColPlane->OptScl->UseBrt) { SUMA_RETURNe; } /* nothing else to do */
   
   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   

   SUMA_RemixRedisplay(SO);
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
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
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n request to switch colormap to  (%s)\n", 
         FuncName, CM->Name);
   }
   
   if (!SUMA_SwitchColPlaneCmap(SO, CM)) {
      SUMA_SL_Err("Failed in SUMA_SwitchColPlaneCmap");
   }
   
   /* Now you'll need to close the list widget if a choice has been made */
   if (SUMAg_CF->X->SwitchCmapLst) {
      if (!SUMAg_CF->X->SwitchCmapLst->isShaded) 
         SUMA_cb_CloseSwitchCmap( w,  (XtPointer)SUMAg_CF->X->SwitchCmapLst,  call);
   }
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
}

void SUMA_cb_ShowZero_tb_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SymIrange_tb_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;
   
   SO->SurfCont->curColPlane->OptScl->MaskZero = !SO->SurfCont->curColPlane->OptScl->MaskZero;
   
   if (!SO->SurfCont->curColPlane->Show) { SUMA_RETURNe; } /* nothing else to do */
   
   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_RemixRedisplay(SO);
 
   SUMA_UpdateNodeLblField(SO);
   

}
 

void SUMA_cb_SymIrange_tb_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SymIrange_tb_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;
   
   SO->SurfCont->curColPlane->SymIrange = !SO->SurfCont->curColPlane->SymIrange;
   
   if (SO->SurfCont->curColPlane->SymIrange) {
      /* manual setting of range. DO NOT Call SUMA_InitRangeTable because it will automatically update the I range under certain conditions*/
      TF = SO->SurfCont->SetRangeTable;
      SO->SurfCont->curColPlane->OptScl->IntRange[1] = 
         SUMA_LARG_ABS(SO->SurfCont->curColPlane->OptScl->IntRange[0], SO->SurfCont->curColPlane->OptScl->IntRange[1]);
      SO->SurfCont->curColPlane->OptScl->IntRange[0] = -SO->SurfCont->curColPlane->OptScl->IntRange[1];
      SUMA_INSERT_CELL_VALUE(TF, 1, 1, SO->SurfCont->curColPlane->OptScl->IntRange[0]);
      SUMA_INSERT_CELL_VALUE(TF, 1, 2, SO->SurfCont->curColPlane->OptScl->IntRange[1]);
   }
   
   if (!SO->SurfCont->curColPlane->Show) { SUMA_RETURNe; } /* nothing else to do */
   
   
   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_RemixRedisplay(SO);
 
   SUMA_UpdateNodeValField(SO);
   SUMA_UpdateNodeLblField(SO);
   

   SUMA_RETURNe;
}

void SUMA_cb_AbsThresh_tb_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_AbsThresh_tb_toggled"};
   SUMA_SurfaceObject *SO = NULL;
   char slabel[100];
   float range[2]; int loc[2];  
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SO->SurfCont->curColPlane) SUMA_RETURNe;

   if (SO->SurfCont->curColPlane->OptScl->ThrMode != SUMA_ABS_LESS_THAN) {
      SO->SurfCont->curColPlane->OptScl->ThrMode = SUMA_ABS_LESS_THAN;
      sprintf(slabel, "|%5s|", MV_format_fval(fabs(SO->SurfCont->curColPlane->OptScl->ThreshRange[0])));
   } else {
      SO->SurfCont->curColPlane->OptScl->ThrMode = SUMA_LESS_THAN;
      sprintf(slabel, "%5s", MV_format_fval(SO->SurfCont->curColPlane->OptScl->ThreshRange[0]));
   }
   SUMA_SET_LABEL(SO->SurfCont->thr_lb,  slabel);
   
   if (SUMA_GetColRange(SO->SurfCont->curColPlane->dset_link->nel, SO->SurfCont->curColPlane->OptScl->tind, range, loc)) {   
      SUMA_SetScaleRange(SO, range );
   }else {
      SUMA_SLP_Err("Failed to get range");
      SUMA_RETURNe;
   }
      
   if (!SO->SurfCont->curColPlane->OptScl->UseThr) { SUMA_RETURNe; } /* nothing else to do */

   if (!SUMA_ColorizePlane (SO->SurfCont->curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_RemixRedisplay(SO);

   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
}

void SUMA_cb_SwithInt_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SwithInt_toggled"};
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
   SO->SurfCont->curColPlane->Show = XmToggleButtonGetState (SO->SurfCont->Int_tb);
   XmToggleButtonSetState (SO->SurfCont->ColPlaneShow_tb, SO->SurfCont->curColPlane->Show, NOPE);
   
   SUMA_ColorizePlane(SO->SurfCont->curColPlane);
   SUMA_RemixRedisplay(SO);
   SUMA_UpdateNodeLblField(SO);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   SUMA_RETURNe;
}

void SUMA_cb_SwithThr_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SwithThr_toggled"};
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
      
   SO->SurfCont->curColPlane->OptScl->UseThr = XmToggleButtonGetState (SO->SurfCont->Thr_tb);
      
   SUMA_ColorizePlane(SO->SurfCont->curColPlane);
   SUMA_RemixRedisplay(SO);
   
   SUMA_UpdateNodeLblField(SO);
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(SO);
   #endif
   SUMA_RETURNe;
}

void SUMA_cb_SwithBrt_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SwithBrt_toggled"};
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
   
   SO->SurfCont->curColPlane->OptScl->UseBrt = XmToggleButtonGetState (SO->SurfCont->Brt_tb);
   
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
   {  "Int", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCmapMode, (XtPointer) SW_Interp, NULL},
      
   {  "NN", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCmapMode, (XtPointer) SW_NN, NULL},
   
   {  "Dir", &xmPushButtonWidgetClass, 
      '\0', NULL, NULL, 
      SUMA_cb_SetCmapMode, (XtPointer) SW_Direct, NULL},
    
   {NULL},
};

/*!
   \brief sets the colormap interpolation mode
   - expects a SUMA_MenuCallBackData * in  client_data
   with SO as client_data->ContID and Menubutton in client_data->callback_data
*/
void SUMA_cb_SetCmapMode(Widget widget, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetCmapMode"};
   SUMA_MenuCallBackData *datap=NULL;
   int imenu;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean NewDisp = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (int)datap->callback_data; 
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
         fprintf (SUMA_STDERR, "Error %s: Unexpected widget index.\n", FuncName);
         break;
   }
   
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
void SUMA_cb_SetCoordBias(Widget widget, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SetCoordBias"};
   SUMA_MenuCallBackData *datap=NULL;
   int imenu;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean NewDisp = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   SO = (SUMA_SurfaceObject *)datap->ContID;
   imenu = (int)datap->callback_data; 
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
         if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_X) { /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to new dimension*/
               SUMA_TransferCoordBias(SO->SurfCont->curColPlane, SW_CoordBias_X);
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_Y:
         if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_Y) { /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to new dimension*/
               SUMA_TransferCoordBias(SO->SurfCont->curColPlane, SW_CoordBias_Y);
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_Z:
         if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_Z) { /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to new dimension*/
               SUMA_TransferCoordBias(SO->SurfCont->curColPlane, SW_CoordBias_Z);
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_N:
         if (SO->SurfCont->curColPlane->OptScl->DoBias != SW_CoordBias_N) { /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to new dimension*/
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
   TF = (SUMA_TABLE_FIELD *)SUMA_malloc(sizeof(SUMA_TABLE_FIELD));
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
   TF->TitLabelCallback = NULL;
   TF->TitLabelCallbackData = NULL;
   TF->cell_modified = -1;
   TF->num_value = NULL;
   TF->str_value = NULL;
   SUMA_RETURN(TF);
}

/*!
   Called when user clicks on table title 
   Expects SO in TF->NewValueCallbackData
*/
void SUMA_RangeTableTit_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_RangeTableTit_EV"};
   Dimension lw ;
   Widget * children , wl = NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  num_children , i, j, Found, AutoHist;
   SUMA_TABLE_FIELD *TF = (SUMA_TABLE_FIELD *)cd;
   SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)TF->NewValueCallbackData;
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
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Click on cell [%d %d]\n", FuncName, i, j);
   } else {
      SUMA_SL_Err("CEll not found!");
      SUMA_RETURNe;
   }
   
   /* Now do something */
   if (j == 0) { /* clicked on one of the row's titles */
      switch (i) {
         case 1:
            if (bev->button == Button1) { /* toggle lock */
               SO->SurfCont->AutoIntRange = !SO->SurfCont->AutoIntRange;
               MCW_invert_widget(w);
            }else if (bev->button == Button3) { /* reset to autorange values */
               AutoHist = SO->SurfCont->AutoIntRange; 
               SO->SurfCont->AutoIntRange = 1;
               SUMA_InitRangeTable(SO); /* overkill but little overhead */
               SUMA_ColorizePlane(SO->SurfCont->curColPlane);
               SUMA_RemixRedisplay(SO);
               SO->SurfCont->AutoIntRange = AutoHist; 
            }
            break;
         case 2:
            if (bev->button == Button1) { /* toggle lock */
               SO->SurfCont->AutoBrtRange = !SO->SurfCont->AutoBrtRange;
               MCW_invert_widget(w);   
            }else if (bev->button == Button3) { /* reset to autorange values */
               AutoHist = SO->SurfCont->AutoBrtRange; 
               SO->SurfCont->AutoBrtRange = 1;
               SUMA_InitRangeTable(SO); /* overkill but little overhead */
               SUMA_ColorizePlane(SO->SurfCont->curColPlane);
               SUMA_RemixRedisplay(SO);
               SO->SurfCont->AutoBrtRange = AutoHist; 
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


SUMA_TABLE_FIELD * SUMA_FreeTableField(SUMA_TABLE_FIELD *TF)
{
   static char FuncName[]={"SUMA_FreeTableField"};
   int i;
   
   SUMA_ENTRY;

   if (!TF) SUMA_RETURN(NULL);

   if (TF->cells) SUMA_free(TF->cells);
   if (TF->cwidth) SUMA_free(TF->cwidth);
   if (TF->num_value) SUMA_free(TF->num_value);
   if (TF->str_value) { 
      for (i=0; i<TF->Nj*TF->Ni; ++i) if (TF->str_value[i]) SUMA_free(TF->str_value[i]); 
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
   XtRemoveCallback (TF->cells[n], XmNactivateCallback, SUMA_TableF_cb_label_change, (XtPointer)TF);
   XtRemoveCallback (TF->cells[n], XmNmodifyVerifyCallback, SUMA_TableF_cb_label_Modify, (XtPointer)TF);
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
         
         XtAddCallback (TF->cells[n], XmNactivateCallback, SUMA_TableF_cb_label_change, (XtPointer)TF);
         XtAddCallback (TF->cells[n], XmNmodifyVerifyCallback, SUMA_TableF_cb_label_Modify, (XtPointer)TF);
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
                        void (*TitLabelCallback)(Widget w , XtPointer cd , XEvent *ev , Boolean *ctd), void *TitLabelCallbackData,
                        SUMA_TABLE_FIELD *TF) 
{
   static char FuncName[]={"SUMA_CreateTable"};
   int i, j, n, titw, xmw;
   char *tmp;
   Widget rco, rcc;
   XtPointer cd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;                        

   if (!TF) { SUMA_SL_Err("NULL TF"); SUMA_RETURNe; }
   TF->Ni = Ni; TF->Nj = Nj; TF->editable = editable; 
   TF->cwidth = (int *)SUMA_calloc(TF->Nj, sizeof(int)); for (j=0; j<TF->Nj; ++j) TF->cwidth[j] = cwidth[j];
   if(col_tit) TF->HasColTit = YUP; else TF->HasColTit = NOPE;
   if(row_tit) TF->HasRowTit = YUP; else TF->HasRowTit = NOPE;
   TF->cells = (Widget *)SUMA_malloc(sizeof(Widget)*TF->Ni*TF->Nj);
   if (!TF->cells) {  SUMA_SL_Crit("Failed to allocate"); SUMA_RETURNe; }
   TF->NewValueCallback = NewValueCallback;
   TF->NewValueCallbackData = cb_data;
   TF->TitLabelCallback = TitLabelCallback;
   TF->TitLabelCallbackData = TitLabelCallbackData;
   TF->type = type;
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
      
      if (i == 0 && TF->HasColTit) { /* This is left here to show that I tried using different packing methods
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
               if (LocalHead) fprintf(SUMA_STDERR,"%s:\nAdding [%d %d] (%d) %s\n", FuncName, i, j, n, row_tit[i]);
               #if 0
                  TF->cells[n] = XtVaCreateManagedWidget(row_tit[i],  
                                                xmLabelWidgetClass, rcc, 
                                                NULL);
               #else
                  TF->cells[n] = XtVaCreateManagedWidget("column title",   
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
               #endif
               if (!TF->TitLabelCallbackData) cd = (XtPointer) TF; else cd = (XtPointer)TF->TitLabelCallbackData;
               if (TF->TitLabelCallback) {
                  /* insert handler to catch clicks on titles */
                  XtInsertEventHandler( TF->cells[n] ,      /* handle events in title cell */
                              ButtonPressMask ,  /* button presses */
                              FALSE ,            /* nonmaskable events? */
                              TF->TitLabelCallback,  /* handler */
                              cd ,   /* client data */
                              XtListTail ) ; 
               }
               if (row_hint)  MCW_register_hint( TF->cells[n], row_hint[i] );
               if (row_help)  MCW_register_help( TF->cells[n], row_help[i] ) ;
               break;
               
            case SUMA_COL_TIT_CELL: /* column's title */
               if (LocalHead) fprintf(SUMA_STDERR,"%s:\nAdding [%d %d] (%d) %s\n", FuncName, i, j, n, col_tit[j]);
               /* padd to fit cell entry fields*/
               if (i == 0 && j != 0 && TF->HasColTit) { 
                  titw = TF->cwidth[j]; 
                  /* set the margins to meet those of cell entries */
                  xmw = 5;
               } else {
                  titw = TF->cwidth[j];
                  /* set the margins to meet those of Labels */
                  xmw = 0;
               }
               #if 0
                  TF->cells[n] = XtVaCreateManagedWidget(tmp,  
                                                xmLabelWidgetClass, rcc,
                                                NULL);
               #else 
                  TF->cells[n] = XtVaCreateManagedWidget("column title",   
                                                   xmTextFieldWidgetClass, rcc,
                                                   XmNvalue, col_tit[j],
                                                   XmNmarginHeight, 0,
                                                   XmNmarginWidth, xmw,
                                                   XmNmarginTop, 0,
                                                   XmNmarginBottom, 0,
                                                   XmNmarginLeft, 0,
                                                   XmNmarginRight, 0,
                                                   XmNeditable, False, 
                                                   XmNshadowThickness , 0,          /* hide the border */
                                                   XmNcursorPositionVisible, False, /* hide the cursor */
                                                   XmNcolumns, titw, 
                                                   NULL);
               #endif
               if (i == 0 && j != 0) { 
                  XtVaSetValues( TF->cells[n], XmNalignment, XmALIGNMENT_BEGINNING, NULL);
               }
               
               /* insert handler to catch clicks on titles */
               if (!TF->TitLabelCallbackData) cd = (XtPointer) TF; else cd = (XtPointer)TF->TitLabelCallbackData;
               if (TF->TitLabelCallback) {
                  /* insert handler to catch clicks on titles */
                  XtInsertEventHandler( TF->cells[n] ,      /* handle events in title cell */
                              ButtonPressMask ,  /* button presses */
                              FALSE ,            /* nonmaskable events? */
                              TF->TitLabelCallback,  /* handler */
                              cd ,   /* client data */
                              XtListTail ) ; 
               }                 
               if (col_hint)  MCW_register_hint( TF->cells[n], col_hint[j] );
               if (col_help)  MCW_register_help( TF->cells[n], col_help[j] ) ;
               break;
            case SUMA_ENTRY_CELL: /* entry cell */
               if (LocalHead) fprintf(SUMA_STDERR,"%s:\nAdding [%d %d] (%d) entry cell\n", FuncName, i, j, n);
               TF->cells[n] = XtVaCreateManagedWidget("entry",  
                                                   xmTextFieldWidgetClass, rcc,
                                                   XmNuserData, (XtPointer)n,
                                                   XmNvalue, "-",
                                                   XmNmarginHeight, 0,
                                                   XmNmarginTop, 0,
                                                   XmNmarginBottom, 0, 
                                                   NULL);

               if (TF->cwidth[j] > 0) {  XtVaSetValues(TF->cells[n], XmNcolumns, TF->cwidth[j], NULL); }
               if (!TF->editable) { 
                  SUMA_SetCellEditMode(TF, i, j, 0);
               } else {
                  SUMA_SetCellEditMode(TF, i, j, 1);
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
void SUMA_TableF_cb_label_change (Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_TableF_cb_label_change"};
   SUMA_TABLE_FIELD *TF=NULL;
   float val;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
   void *n;
   SUMA_Boolean DoCallBacks;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   /* make call to NewValue callback */
   TF = (SUMA_TABLE_FIELD *)client_data;
   
   DoCallBacks = NOPE;
   if (call_data) { /* do the call backs if carriage return even if nothing is modified */
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: cbs->reason = %d (CR=%d)\n", FuncName, cbs->reason, XmCR_ACTIVATE);
      if (cbs->reason == XmCR_ACTIVATE) { 
         DoCallBacks = YUP;
      }
   }
   
   if (TF->cell_modified >= 0) {
      DoCallBacks = YUP;   /* do the callbacks even if no carriage return ... */
      if (TF->type == SUMA_int || TF->type == SUMA_float) {
         /* Check if the string is numerical */
         XtVaGetValues (w, XmNvalue, &n, NULL);
         #if 0
         /* Don't use that, returns 0 if bad syntax is used */ 
         val = strtod ((char *)n, NULL); 
         if (errno) {
            /* bad syntax, reset value*/
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Bad syntax.\n", FuncName);
            SUMA_RegisterMessage (SUMAg_CF->MessageList, "Bad value in text field", FuncName, SMT_Error, SMA_Log);
            SUMA_TableF_SetString (TF);
         }else {
            if (TF->type == SUMA_int) {
               if (TF->num_value[TF->cell_modified] == (int)val) { 
                  SUMA_LH("Same value"); 
                  TF->cell_modified = -1;
                  SUMA_RETURNe; 
               }
               TF->num_value[TF->cell_modified] = (int)val;
            } else if (TF->type == SUMA_float) {
               if (TF->num_value[TF->cell_modified] == val) { 
                  SUMA_LH("Same value"); 
                  TF->cell_modified = -1;
                  SUMA_RETURNe; 
               }
               TF->num_value[TF->cell_modified] = val;
            }
            SUMA_TableF_SetString (TF);
         }
         #else
         if (SUMA_StringToNum((char *)n, &val, 1) != 1) {
            SUMA_BEEP;
            /* bad syntax, reset value*/
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Bad syntax.\n", FuncName);
            SUMA_RegisterMessage (SUMAg_CF->MessageList, "Bad value in text field", FuncName, SMT_Error, SMA_Log);
            SUMA_TableF_SetString (TF);
         }else {
            if (TF->type == SUMA_int) {
               if (TF->num_value[TF->cell_modified] == (int)val) { 
                  SUMA_LH("Same value"); 
                  TF->cell_modified = -1;
                  SUMA_RETURNe; 
               }
               TF->num_value[TF->cell_modified] = (int)val;
            } else if (TF->type == SUMA_float) {
               if (TF->num_value[TF->cell_modified] == val) { 
                  SUMA_LH("Same value"); 
                  TF->cell_modified = -1;
                  SUMA_RETURNe; 
               }
               TF->num_value[TF->cell_modified] = val;
            }
            SUMA_TableF_SetString (TF);
         }
         #endif  
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
         if (TF->NewValueCallback) TF->NewValueCallback(TF->NewValueCallbackData);
      }
   }
   
   TF->cell_modified = -1;
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
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s\n", FuncName, i, j, (char *)cv);
   }
   /* Now parse that string into 3 numbers */
   if (SUMA_StringToNum ((char *)cv, fv3, 3) != 3) {
      SUMA_BEEP;
      str[0]='\0';
   } else {
      SUMA_XHAIR_STRING(fv3, str);
   }
   XtVaSetValues(TF->cells[n], XmNvalue, str, NULL);
   
   /* look for a viewer that is showing this surface */
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, i);
      if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
         /* is this viewer showing curSO ? */
         sv = &(SUMAg_SVv[i]);
         if (SUMA_isVisibleSO(sv, SUMAg_DOv, curSO)) {
            /* is this a new coordinate? Avoid calls due to cosmetic text changes */
            if (sv->Ch->c[0] != fv3[0] || sv->Ch->c[1] != fv3[1] || sv->Ch->c[2] != fv3[2]) {
               if (LocalHead) fprintf(SUMA_STDERR, "%s: Calling for jump to %s\n", FuncName, str);
               SUMA_JumpXYZ(str, (void *)(&(SUMAg_SVv[i])));
            }
         }
      }
   }
   SUMA_RETURNe;
}

/*!
   \brief Sets the range values when new value is input to the table 
*/
void SUMA_SetRangeValue (void *data)
{
   static char FuncName[]={"SUMA_SetRangeValue"};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   SUMA_OVERLAYS *ColPlane = NULL;
   SUMA_Boolean NewDisp = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SO = (SUMA_SurfaceObject *)data;
   TF = SO->SurfCont->SetRangeTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   
   ColPlane = SO->SurfCont->curColPlane;
   
   n = TF->cell_modified;
   i = n % TF->Ni;
   j = n / TF->Ni;
   XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s\n", FuncName, i, j, (char *)cv);
   }
   NewDisp = NOPE;
   if (1) {
   /* What are we dealing with ? */
   switch (i) {
      case 1:  /* That's the Int. range */
         SUMA_LH("Setting Int. Range");
         if (j == 1) {
            if (ColPlane->SymIrange) {
               ColPlane->OptScl->IntRange[0] = -fabs((double)TF->num_value[n]);
               ColPlane->OptScl->IntRange[1] = -ColPlane->OptScl->IntRange[0];
               SUMA_INSERT_CELL_VALUE(TF, 1, 1, ColPlane->OptScl->IntRange[0]);
               SUMA_INSERT_CELL_VALUE(TF, 1, 2, ColPlane->OptScl->IntRange[1]);
            } else {
               if (TF->num_value[n] > ColPlane->OptScl->IntRange[1]) {
                  SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->IntRange[0];
                  SUMA_SLP_Err("Lower bound > Upper bound!");
                  SUMA_TableF_SetString(TF);
               } else {
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: IntRange[0] was %f, will be %f\n", FuncName, ColPlane->OptScl->IntRange[0], TF->num_value[n]);
                  ColPlane->OptScl->IntRange[0] = TF->num_value[n];
               }
            }
         } else if (j==2) {
             if (ColPlane->SymIrange) {
               ColPlane->OptScl->IntRange[1] = fabs((double)TF->num_value[n]);
               ColPlane->OptScl->IntRange[0] = -ColPlane->OptScl->IntRange[1];
               SUMA_INSERT_CELL_VALUE(TF, 1, 1, ColPlane->OptScl->IntRange[0]);
               SUMA_INSERT_CELL_VALUE(TF, 1, 2, ColPlane->OptScl->IntRange[1]);
             } else {
               if (TF->num_value[n] < ColPlane->OptScl->IntRange[0]) {
                  SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->IntRange[1];
                  SUMA_SLP_Err("Upper bound < Lower bound!");
                  SUMA_TableF_SetString(TF);
               } else {
                  ColPlane->OptScl->IntRange[1] = TF->num_value[n];
               }
            }
         } else { SUMA_SL_Err("What's going on John ?"); }
         if (ColPlane->Show) NewDisp = YUP;
         break;
      case 2:  /* That's the Brt.. range */
         SUMA_LH("Setting Brt. Range");
         if (j == 1) {
            if (TF->num_value[n] > ColPlane->OptScl->BrightRange[1]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightRange[0];
               SUMA_SLP_Err("Lower bound > Upper bound!");
               SUMA_TableF_SetString(TF);
            } else {
               ColPlane->OptScl->BrightRange[0] = TF->num_value[n];
            }
         } else if (j==2) {
            if (TF->num_value[n] < ColPlane->OptScl->BrightRange[0]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightRange[1];
               SUMA_SLP_Err("Upper bound < Lower bound!");
               SUMA_TableF_SetString(TF);
            } else {
               ColPlane->OptScl->BrightRange[1] = TF->num_value[n];
            }
         } else { SUMA_SL_Err("What's going on Ron ?"); }
         if (ColPlane->OptScl->UseBrt) NewDisp = YUP;
         break;
      case 3:  /* That's the Brt. Map Range */
         SUMA_LH("Setting BrtMap. Range");
         if (j == 1) {
            if (TF->num_value[n] > ColPlane->OptScl->BrightMap[1]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightMap[0];
               SUMA_SLP_Err("Lower bound > Upper bound!");
               SUMA_TableF_SetString(TF);
            } else if (TF->num_value[n] < 0) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightMap[0];
               SUMA_SLP_Err("Value must be >= 0");
               SUMA_TableF_SetString(TF);
            } else {
               ColPlane->OptScl->BrightMap[0] = TF->num_value[n];
            }
         } else if (j==2) {
            if (TF->num_value[n] < ColPlane->OptScl->BrightMap[0]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->BrightMap[1];
               SUMA_SLP_Err("Upper bound < Lower bound!");
               SUMA_TableF_SetString(TF);
            } else {
               ColPlane->OptScl->BrightMap[1] = TF->num_value[n];
            }
         } else { SUMA_SL_Err("What's going on Mon ?"); }
         if (ColPlane->OptScl->UseBrt) NewDisp = YUP;
         break;
      case 4:  /* That's the coordinate bias Range */
         SUMA_LH("Setting CoordBias. Range");
         if (j == 1) {
            if (TF->num_value[n] > ColPlane->OptScl->CoordBiasRange[1]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->CoordBiasRange[0];
               SUMA_SLP_Err("Lower bound > Upper bound!");
               SUMA_TableF_SetString(TF);
            } else { /* OK */
               ColPlane->OptScl->CoordBiasRange[0] = TF->num_value[n];
            }
         } else if (j==2) {
            if (TF->num_value[n] < ColPlane->OptScl->CoordBiasRange[0]) {
               SUMA_BEEP; TF->num_value[n] = ColPlane->OptScl->CoordBiasRange[1];
               SUMA_SLP_Err("Upper bound < Lower bound!");
               SUMA_TableF_SetString(TF);
            } else { /* OK */
               ColPlane->OptScl->CoordBiasRange[1] = TF->num_value[n];
            }
         } else { SUMA_SL_Err("What's going on Hon ?"); }
         NewDisp = YUP; /* You might want to disable this feature if the ColPlane is not shown */
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
         MV_format_fval2(TF->num_value[TF->cell_modified], TF->cwidth[TF->cell_modified / TF->Ni]));
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
void SUMA_TableF_cb_label_Modify (Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_TableF_cb_label_Modify"};
   SUMA_TABLE_FIELD *TF=NULL;
   int ud;
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
void SUMA_set_cmap_options(SUMA_SurfaceObject *SO, SUMA_Boolean NewDset, SUMA_Boolean NewMap)
{
   static char FuncName[]={"SUMA_set_cmap_options"};
   SUMA_MenuItem *SwitchInt_Menu = NULL, *SwitchThr_Menu = NULL, *SwitchBrt_Menu = NULL;
   int N_items;
   SUMA_MenuItem *SwitchCmap_Menu = NULL;
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
      if (!SO->SurfCont->rcsw) {
         SO->SurfCont->rcsw = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->rcvo,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNheight, 105,               /* don't let that change dynamically,  */
            XmNresizeHeight, False,       /* it messes up the frame size, when you switch dsets*/
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
      SwitchInt_Menu = SUMA_FormSwitchColMenuVector(SO, 0, &N_items);      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: %d items.\n", FuncName, N_items);
      if (SwitchInt_Menu || !N_items) {
         if (SO->SurfCont->SwitchIntMenu) {
            SUMA_LH("Freeing old menu");
            XtDestroyWidget(SO->SurfCont->SwitchIntMenu[0]); /*kill the menu widget */
            SUMA_free(SO->SurfCont->SwitchIntMenu);   /* free the vector */
         }
         /* create a new one allocate for one more spot for the parent widget. 
            (more additions for sub-menus, see how SUMA_BuildMenu works )*/
         SO->SurfCont->SwitchIntMenu = (Widget *)SUMA_malloc(sizeof(Widget)*(N_items+1));  
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SO->SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "I", '\0', YUP, SwitchInt_Menu, 
                           (void *)SO, SO->SurfCont->SwitchIntMenu );
         if (LocalHead) SUMA_ShowMeTheChildren(SO->SurfCont->SwitchIntMenu[0]);
         XtManageChild (SO->SurfCont->SwitchIntMenu[0]);
         /* Now destroy the SwitchInt_Menu */
         SwitchInt_Menu = SUMA_FreeMenuVector(SwitchInt_Menu, N_items);
      } else {
         SUMA_SL_Err("NULL SwitchInt_Menu");
      }
      
      SwitchThr_Menu = SUMA_FormSwitchColMenuVector(SO, 1, &N_items);
      if (SwitchThr_Menu || !N_items) {
         if (SO->SurfCont->SwitchThrMenu) {
            SUMA_LH("Freeing old menu");
            XtDestroyWidget(SO->SurfCont->SwitchThrMenu[0]); /*kill the menu widget */
            SUMA_free(SO->SurfCont->SwitchThrMenu);   /* free the vector */
         }
         /* create a new one allocate for one more spot for the parent widget. 
            (more additions for sub-menus, see how SUMA_BuildMenu works )*/
         SO->SurfCont->SwitchThrMenu = (Widget *)SUMA_malloc(sizeof(Widget)*(N_items+1));  
         SUMA_BuildMenuReset(13);         
         SUMA_BuildMenu (SO->SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "T", '\0', YUP, SwitchThr_Menu, 
                           (void *)SO, SO->SurfCont->SwitchThrMenu );
         XtManageChild (SO->SurfCont->SwitchThrMenu[0]);
         /* Now destroy the SwitchThr_Menu */
         SwitchThr_Menu = SUMA_FreeMenuVector(SwitchThr_Menu, N_items);
      } else {
         SUMA_SL_Err("NULL SwitchThr_Menu");
      }

      SwitchBrt_Menu = SUMA_FormSwitchColMenuVector(SO, 2, &N_items);
      if (SwitchBrt_Menu || !N_items) {
         if (SO->SurfCont->SwitchBrtMenu) {
            SUMA_LH("Freeing old menu");
            XtDestroyWidget(SO->SurfCont->SwitchBrtMenu[0]); /*kill the menu widget */
            SUMA_free(SO->SurfCont->SwitchBrtMenu);   /* free the vector */
         }
         /* create a new one allocate for one more spot for the parent widget. 
            (more additions for sub-menus, see how SUMA_BuildMenu works )*/         
         SO->SurfCont->SwitchBrtMenu = (Widget *)SUMA_malloc(sizeof(Widget)*(N_items+1));  
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SO->SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "B", '\0', YUP, SwitchBrt_Menu, 
                           (void *)SO, SO->SurfCont->SwitchBrtMenu );
         XtManageChild (SO->SurfCont->SwitchBrtMenu[0]);
         /* Now destroy the SwitchBrt_Menu */
         SwitchBrt_Menu = SUMA_FreeMenuVector(SwitchBrt_Menu, N_items);
      } else {
         SUMA_SL_Err("NULL SwitchBrt_Menu");
      }
      
     if (1) {
     /* put the toggle buttons */
         if (!SO->SurfCont->Int_tb) {
            SO->SurfCont->Int_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonGadgetClass, SO->SurfCont->rcsw_v2, NULL);
            XtAddCallback (SO->SurfCont->Int_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwithInt_toggled, SO);
            SUMA_SET_SELECT_COLOR(SO->SurfCont->Int_tb);
         } 
         XmToggleButtonSetState (SO->SurfCont->Int_tb, SO->SurfCont->curColPlane->Show, NOPE);
         
         if (!SO->SurfCont->Thr_tb) {
            SO->SurfCont->Thr_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonGadgetClass, SO->SurfCont->rcsw_v2, NULL);
            XtAddCallback (SO->SurfCont->Thr_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwithThr_toggled, SO);
            SUMA_SET_SELECT_COLOR(SO->SurfCont->Thr_tb);
         }
         if (SO->SurfCont->curColPlane->OptScl->tind >=0) {
            XmToggleButtonSetState (SO->SurfCont->Thr_tb, SO->SurfCont->curColPlane->OptScl->UseThr, NOPE);
         }else {
            XmToggleButtonSetState (SO->SurfCont->Thr_tb, NOPE, NOPE);
         }
         
         if (!SO->SurfCont->Brt_tb) {
            SO->SurfCont->Brt_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonGadgetClass, SO->SurfCont->rcsw_v2, NULL);
            XtAddCallback (SO->SurfCont->Brt_tb, 
                     XmNvalueChangedCallback, SUMA_cb_SwithBrt_toggled, SO);
            SUMA_SET_SELECT_COLOR(SO->SurfCont->Brt_tb);
         }
         if (SO->SurfCont->curColPlane->OptScl->bind >=0) {
            XmToggleButtonSetState (SO->SurfCont->Brt_tb, SO->SurfCont->curColPlane->OptScl->UseBrt, NOPE);
         } else {
            XmToggleButtonSetState (SO->SurfCont->Brt_tb, NOPE, NOPE);
         }
      }
      if (!XtIsManaged(SO->SurfCont->rcsw_v1)) XtManageChild (SO->SurfCont->rcsw_v1);
      if (!XtIsManaged(SO->SurfCont->rcsw_v2)) XtManageChild (SO->SurfCont->rcsw_v2);
      if (!XtIsManaged(SO->SurfCont->rcsw)) XtManageChild (SO->SurfCont->rcsw);
   } /* The intensity / threshold / Brightness block */
   
   {/*  The Color map range and selector block */
      char *col_tit[]=  {  " ", "Min", "Max", NULL};
      char *col_hint[]= {  "Clipping ranges", 
                           "Minimum clip value", 
                           "Maximum clip value" , NULL};
      char *col_help[]= {  "Clipping ranges", 
                           "Minimum clip value", 
                           "Maximum clip value" , NULL};
      char *row_tit[]=  {  " ", "I", "B", " " , "C", NULL};
      char *row_hint[]= {  "Clipping ranges ", 
                           "Intensity clipping range (much more with BHelp)", 
                           "Brightness modulation clipping range (much more with BHelp)", 
                           "Brightness modulation factor range (much more with BHelp)" , 
                           "Coordinate bias range (much more with BHelp)", NULL};
      char *row_help[]= {  "Clipping ranges ", 
                           "Intensity clipping range.\n"
                           "Values in the intensity data \n"
                           "that are less than Min are colored\n"
                           "by the first (bottom) color of the \n"
                           "colormap. Values larger than Max \n"
                           "are mapped to the top color.\n"
                           "***Note: Left click locks ranges\n"
                           "         from automatic resetting.\n"
                           "         Right click resets values\n"
                           "         to full range in data.\n",
                           "Brightness modulation clipping range.\n"
                           "Values in the brightness data are\n"
                           "clipped to the Min to Max range before\n"
                           "calculating their modulation factor\n"
                           "(see next row).\n"
                           "***Note: Left click locks ranges\n"
                           "         from automatic resetting.\n"
                           "         Right click resets values\n"
                           "         to full range in data.\n", 
                           "Brightness modulation factor range.\n"
                           "Brightness modulation values, after\n"
                           "clipping per the values in the row above,\n"
                           "are scaled to fit the range specified\n"
                           "here.\n", 
                           "Coordinate bias range.\n"
                           "Coordinates of nodes that are mapped\n"
                           "to the colormap can have a bias added\n"
                           "to their coordinates. Nodes mapped to\n"
                           "the first color of the map have the\n"
                           "minimum bias value, nodes mapped to \n"
                           "the last color have recieve the \n"
                           "maximum bias. Nodes not mapped will \n"
                           "have no bias applied.", NULL};
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
            /* create the widgets for the range table */
            SUMA_LH("Creating table");
            SUMA_CreateTable( SO->SurfCont->rccm,
                           5, 3, 
                           row_tit, col_tit,  
                           row_hint, col_hint,  
                           row_help, col_help,  
                           colw, YUP, SUMA_float, 
                           SUMA_SetRangeValue, (void *)SO,
                           SUMA_RangeTableTit_EV, NULL, 
                           SO->SurfCont->SetRangeTable);
         }
       
         if (!SO->SurfCont->CoordBiasMenu[SW_CoordBias]) {
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
                               (void *)SO,  SO->SurfCont->CmapModeMenu);
               XtManageChild (SO->SurfCont->CmapModeMenu[SW_CmapMode]);
               
               SUMA_LH("Forming new bias menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "Bias", '\0', YUP, CoordBias_Menu, 
                               (void *)SO,  SO->SurfCont->CoordBiasMenu);
               XtManageChild (SO->SurfCont->CoordBiasMenu[SW_CoordBias]);
               
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
            SUMA_LH("Forming CmapMenu");
            SwitchCmap_Menu = SUMA_FormSwitchCmapMenuVector(SUMAg_CF->scm->CMv, SUMAg_CF->scm->N_maps);
            if (SwitchCmap_Menu) {
               /*SO->SurfCont->cmapswtch_pb = XtVaCreateManagedWidget ("Switch", 
                                 xmPushButtonWidgetClass, rcc, 
                                 NULL);
               XtAddCallback (SO->SurfCont->cmapswtch_pb, XmNactivateCallback, SUMA_cb_ColMap_Switch, (XtPointer) SO);
               */
               if (SO->SurfCont->SwitchCmapMenu) {
                  SUMA_LH("Freeing old menu");
                  XtDestroyWidget(SO->SurfCont->SwitchCmapMenu[0]); /*kill the menu widget */
                  SUMA_free(SO->SurfCont->SwitchCmapMenu);   /* free the vector */
               }
               /* create a new one allocate for one more spot for the parent widget. 
                  (more additions for sub-menus, see how SUMA_BuildMenu works )*/
               SO->SurfCont->SwitchCmapMenu = (Widget *)SUMA_malloc(sizeof(Widget)*(SUMAg_CF->scm->N_maps+1));  
               SUMA_BuildMenuReset(10);
               SO->SurfCont->N_CmapMenu = SUMA_BuildMenu (SO->SurfCont->rccm_swcmap, XmMENU_OPTION, /* populate it */
                                 "Cmp", '\0', YUP, SwitchCmap_Menu, 
                                 (void *)SO, SO->SurfCont->SwitchCmapMenu );
               XtInsertEventHandler( SO->SurfCont->SwitchCmapMenu[0] ,      /* handle events in optmenu */
                                 ButtonPressMask ,  /* button presses */
                                 FALSE ,            /* nonmaskable events? */
                                 SUMA_optmenu_EV ,  /* handler */
                                 (XtPointer) SO ,   /* client data */
                                 XtListTail ) ;
               XtManageChild (SO->SurfCont->SwitchCmapMenu[0]);
               /* Now destroy the SwitchInt_Menu */
               SwitchCmap_Menu = SUMA_FreeMenuVector(SwitchCmap_Menu, SUMAg_CF->scm->N_maps);
            }

            #if 0
            /* the bias chooser, need to be recreated with new map, not a big deal*/
            if (SO->SurfCont->CoordBiasMenu[SW_CoordBias]) {
               XtDestroyWidget(SO->SurfCont->CoordBiasMenu[SW_CoordBias]); 
               SO->SurfCont->CoordBiasMenu[SW_CoordBias] = NULL;
            }
            if (!SO->SurfCont->CoordBiasMenu[SW_CoordBias]) {
               SUMA_LH("Forming new bias menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu (SO->SurfCont->rccm_swcmap, XmMENU_OPTION, 
                               NULL, '\0', YUP, CoordBias_Menu, 
                               (void *)SO,  SO->SurfCont->CoordBiasMenu);
               XtManageChild (SO->SurfCont->CoordBiasMenu[SW_CoordBias]);
            }
            #endif
         } /* new colormaps */
         if (!XtIsManaged(SO->SurfCont->rccm_swcmap)) XtManageChild (SO->SurfCont->rccm_swcmap); 
      }
      
      /* Set the CoordBias's menu history to reflect current setting */
      SUMA_LH("Updating CoorBias chooser History");
      XtVaSetValues(  SO->SurfCont->CoordBiasMenu[0], XmNmenuHistory , 
                      SO->SurfCont->CoordBiasMenu[SO->SurfCont->curColPlane->OptScl->DoBias] , NULL ) ; 
 
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
               xmToggleButtonGadgetClass, rc, 
               NULL);
         XtAddCallback (SO->SurfCont->AbsThresh_tb, 
               XmNvalueChangedCallback, SUMA_cb_AbsThresh_tb_toggled, SO);
         SUMA_SET_SELECT_COLOR(SO->SurfCont->AbsThresh_tb);
         
         /* create the symmetric range toggle button */
         SO->SurfCont->SymIrange_tb = XtVaCreateManagedWidget("sym I", 
               xmToggleButtonGadgetClass, rc, NULL);
         XtAddCallback (SO->SurfCont->SymIrange_tb, 
               XmNvalueChangedCallback, SUMA_cb_SymIrange_tb_toggled, SO);
         
         SUMA_SET_SELECT_COLOR(SO->SurfCont->SymIrange_tb);
         
         /* add a button for zero masking */
         SO->SurfCont->ShowZero_tb = XtVaCreateManagedWidget("shw 0", 
               xmToggleButtonGadgetClass, rc, NULL);
         XtAddCallback (SO->SurfCont->ShowZero_tb, 
               XmNvalueChangedCallback, SUMA_cb_ShowZero_tb_toggled, SO);
         
         SUMA_SET_SELECT_COLOR(SO->SurfCont->ShowZero_tb);
         XtManageChild (rc);
      }
      
      /* do the initialization */
      
      if (SO->SurfCont->curColPlane->OptScl->ThrMode != SUMA_ABS_LESS_THAN) {
         XmToggleButtonSetState( SO->SurfCont->AbsThresh_tb, False, NOPE);
      } else {
         XmToggleButtonSetState( SO->SurfCont->AbsThresh_tb, True, NOPE);
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
      char *col_hint[]= {  "Full range in data", 
                           "Minimum value in data", 
                           "Node index at minimum", 
                           "Maximum value in data", 
                           "Node index at maximum", NULL};
      char *col_help[]= {  "Full range in data", 
                           "Minimum value in data",
                           "Node index at minimum", 
                           "Maximum value in data", 
                           "Node index at maximum", NULL};
      char *row_tit[]=  {  " ", "I", "T", "B", NULL};
      char *row_hint[]= {  "Full range in data", 
                           "Range of values in intensity data", 
                           "Range of values in threshold data", 
                           "Range of values in brightness data", NULL};
      char *row_help[]= {  "Full range in data", 
                           "Range of values in intensity data", 
                           "Range of values in threshold data", 
                           "Range of values in brightness data", NULL};
      if (!SO->SurfCont->rcswr) {
         SO->SurfCont->rcswr = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SO->SurfCont->opts_form,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNrightAttachment, XmATTACH_FORM , 
            XmNleftAttachment,  XmATTACH_NONE,
            XmNtopAttachment, XmATTACH_WIDGET ,
            XmNtopWidget, SO->SurfCont->opts_rc,
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
                           SO->SurfCont->RangeTable);
      }

      if (!XtIsManaged(SO->SurfCont->rcswr)) XtManageChild (SO->SurfCont->rcswr);
   } /* The Range values block */
         
   if (NewDset) {
      /* initialize tables of range values */
      SUMA_InitRangeTable(SO);
   }
   
   if (!XtIsManaged(SO->SurfCont->rcvo)) XtManageChild (SO->SurfCont->rcvo);
   SUMA_FORCE_SCALE_HEIGHT(SO); /* Unfortunately, you need to resize after managing */

   SUMA_RETURNe;
}

SUMA_Boolean SUMA_InitRangeTable(SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_InitRangeTable"};
   char srange_min[50], srange_max[50], srange_minloc[50], srange_maxloc[50];
   SUMA_TABLE_FIELD *TF, *TFs;
   int i, j, i1D, fi, bi, ti;
   float range[2];
   NI_element *nel;
   SUMA_SCALE_TO_MAP_OPT *OptScl;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO->SurfCont) SUMA_RETURN(NOPE);
   TF = SO->SurfCont->RangeTable; 
   TFs = SO->SurfCont->SetRangeTable; 
   if (!TF || !TFs) SUMA_RETURN(NOPE);
   OptScl = SO->SurfCont->curColPlane->OptScl;
   nel = SO->SurfCont->curColPlane->dset_link->nel;
   fi = OptScl->find;
   ti = OptScl->tind;
   bi = OptScl->bind;
   
   /* TF Range table Int*/
   SUMA_LH("Setting Int.");
   SUMA_RANGE_STRING(nel, fi, srange_min, srange_max, srange_minloc, srange_maxloc, range); 
   SUMA_INSERT_CELL_STRING(TF, 1, 1, srange_min);/* min */
   SUMA_INSERT_CELL_STRING(TF, 1, 2, srange_minloc);/* minloc */
   SUMA_INSERT_CELL_STRING(TF, 1, 3, srange_max);/* max */
   SUMA_INSERT_CELL_STRING(TF, 1, 4, srange_maxloc);/* maxloc */
   /* TFs Range table Int*/
   if (SO->SurfCont->AutoIntRange) { 
      if (!SO->SurfCont->curColPlane->ForceIntRange[0] && !SO->SurfCont->curColPlane->ForceIntRange[1]) {
         OptScl->IntRange[0] = range[0]; OptScl->IntRange[1] = range[1]; 
      } else {
         SUMA_LH("Using ForceIntRange");
         OptScl->IntRange[0] = SO->SurfCont->curColPlane->ForceIntRange[0];
         OptScl->IntRange[1] = SO->SurfCont->curColPlane->ForceIntRange[1];
      }
      
      /* enforce the SymIrange option */
      if (SO->SurfCont->curColPlane->SymIrange) {
         OptScl->IntRange[1] = SUMA_LARG_ABS(OptScl->IntRange[0], OptScl->IntRange[1]);
         OptScl->IntRange[0] = -OptScl->IntRange[1];
      }
      
      SUMA_INSERT_CELL_VALUE(TFs, 1, 1, OptScl->IntRange[0]);/* min */ 
      SUMA_INSERT_CELL_VALUE(TFs, 1, 2, OptScl->IntRange[1]);/* max */
   } 
   /* TF Range table Thr*/
   SUMA_LH("Setting Thr.");
   SUMA_RANGE_STRING(nel, ti, srange_min, srange_max, srange_minloc, srange_maxloc, range); 
   SUMA_INSERT_CELL_STRING(TF, 2, 1, srange_min);/* min */
   SUMA_INSERT_CELL_STRING(TF, 2, 2, srange_minloc);/* minloc */
   SUMA_INSERT_CELL_STRING(TF, 2, 3, srange_max);/* max */
   SUMA_INSERT_CELL_STRING(TF, 2, 4, srange_maxloc);/* maxloc */
  
   /* TF Range table Brt*/
   SUMA_LH("Setting Brt.");
   SUMA_RANGE_STRING(nel, bi, srange_min, srange_max, srange_minloc, srange_maxloc, range); 
   SUMA_INSERT_CELL_STRING(TF, 3, 1, srange_min);/* min */
   SUMA_INSERT_CELL_STRING(TF, 3, 2, srange_minloc);/* minloc */
   SUMA_INSERT_CELL_STRING(TF, 3, 3, srange_max);/* max */
   SUMA_INSERT_CELL_STRING(TF, 3, 4, srange_maxloc);/* maxloc */
   /* TFs Range table Brt*/
   if (SO->SurfCont->AutoBrtRange) { 
      OptScl->BrightRange[0] = range[0]; OptScl->BrightRange[1] = range[1]; 
      SUMA_INSERT_CELL_VALUE(TFs, 2, 1, OptScl->BrightRange[0]);/* min */
      SUMA_INSERT_CELL_VALUE(TFs, 2, 2, OptScl->BrightRange[1]);/* max */
      /* TFs Range table BrtMap*/
      SUMA_INSERT_CELL_VALUE(TFs, 3, 1, OptScl->BrightMap[0]);/* min */
      SUMA_INSERT_CELL_VALUE(TFs, 3, 2, OptScl->BrightMap[1]);/* max */
   } 
   
   /* TFs Range table CoordBias*/
   SUMA_INSERT_CELL_VALUE(TFs, 4, 1, OptScl->CoordBiasRange[0]);/* min */
   SUMA_INSERT_CELL_VALUE(TFs, 4, 2, OptScl->CoordBiasRange[1]);/* max */
   
   
   SUMA_RETURN(YUP);
}

SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleCmapList(SUMA_COLOR_MAP **CMv, int N_maps) 
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

/*!
   \brief opens a list selection for choosing a Dset column 
*/
SUMA_Boolean SUMA_DsetColSelectList(SUMA_SurfaceObject *SO, int type)
{
   static char FuncName[]={"SUMA_DsetColSelectList"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SUMA_RETURN(YUP);
}
/*!
   \brief opens a list selection for choosing a color map 
*/
SUMA_Boolean SUMA_CmapSelectList(SUMA_SurfaceObject *SO, int refresh)
{
   static char FuncName[]={"SUMA_CmapSelectList"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* Widget is common to all SUMA */
   LW = SUMAg_CF->X->SwitchCmapLst;
   
   if (!LW) {
      SUMA_LH("Allocating widget");
      /* need to create widget */
      LW = SUMA_AllocateScrolledList   (  "Switch Cmap", SUMA_LSP_SINGLE,
                                          NOPE,          NOPE,
                                          SO->SurfCont->TopLevelShell, SWP_POINTER_OFF,
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
   
   SUMA_CreateScrolledList ( LW->ALS->clist, LW->ALS->N_clist, NOPE,
                             LW);
   
   SUMA_RETURN(YUP);
}

/*!
   This function will fail if the strings have been trunctated 
   Consider writing SetMenuChoiceUserData
*/ 
SUMA_Boolean SUMA_SetCmapMenuChoice(SUMA_SurfaceObject *SO, char *str)
{
   static char FuncName[]={"SUMA_SetCmapMenuChoice"};
   int i, Nbutt = 0;
   Widget whist, *w = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   w = SO->SurfCont->SwitchCmapMenu;
   if (!w) SUMA_RETURN(NOPE);
   
   /* what's your history joe ? */
   XtVaGetValues(  w[0], XmNmenuHistory , &whist , NULL ) ;  
   if (!whist) {
      SUMA_SL_Err("NULL whist!");
      SUMA_RETURN(NOPE);
   }

   if (LocalHead) { 
      fprintf (SUMA_STDERR,"%s: The history is NAMED: %s (%d buttons total)\n", FuncName, XtName(whist), Nbutt);
   } 
      

   /* Now search the widgets in w for a widget labeled str */
   for (i=0; i< SO->SurfCont->N_CmapMenu; ++i) {
      if (LocalHead) fprintf (SUMA_STDERR,"I have %s\n", XtName(w[i]));
      if (strcmp(str, XtName(w[i])) == 0) {
         SUMA_LH("Match!");
         XtVaSetValues(  w[0], XmNmenuHistory , w[i] , NULL ) ;  
         SUMA_RETURN(YUP);
     }
   }
   
   SUMA_RETURN(NOPE);
}

/*!
   \brief function that handles switching colormap from the list widget 
   \sa SUMA_cb_SwitchCmap
*/
void SUMA_cb_SelectSwitchCmap (Widget w, XtPointer client_data, XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SelectSwitchCmap"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_MenuCallBackData data;
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean CloseShop = NOPE, Found = NOPE;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   char *choice=NULL;
   int ichoice = -1;
   SUMA_COLOR_MAP *CM = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SO = (SUMA_SurfaceObject *)client_data;
   LW = SUMAg_CF->X->SwitchCmapLst;
   
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
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Comparing:\n%s\n%s\n", FuncName, LW->ALS->clist[ichoice], choice);
      if (strncmp(LW->ALS->clist[ichoice], choice, strlen(choice)) == 0) Found = YUP; 
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
         CM = (SUMA_COLOR_MAP *)LW->ALS->oplist[ichoice];
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Retrieved Colmap named %s\n", FuncName, CM->Name);
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
      SUMA_cb_CloseSwitchCmap( w,  (XtPointer)LW,  call_data);
   }  
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(SO);

   SUMA_RETURNe;
}

SUMA_Boolean SUMA_SwitchColPlaneCmap(SUMA_SurfaceObject *SO, SUMA_COLOR_MAP *CM)
{
   static char FuncName[]={"SUMA_SwitchColPlaneCmap"};
   SUMA_OVERLAYS *over = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   if (!SO || !CM) { SUMA_RETURN(NOPE); }
   if (!SO->SurfCont) { SUMA_RETURN(NOPE); }
   
   over = SO->SurfCont->curColPlane;
   if (!over) { SUMA_RETURN(NOPE); }
   
   SUMA_STRING_REPLACE(over->cmapname, CM->Name);
   if (!SUMA_ColorizePlane (over)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(NOPE);
   }
   
   /* update the color map display */
   SUMA_cmap_wid_postRedisplay(NULL, (XtPointer)SO, NULL);
               
   SUMA_RemixRedisplay(SO);

   
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
   
   LW = (SUMA_LIST_WIDGET *)client_data;
   
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

/* based on bbox.c's optmenu_EV */
void SUMA_optmenu_EV( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_optmenu_EV"};
   Dimension lw ;
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
   SUMA_LH("Now creating list ");
   if (strcmp(XtName(w), "I") == 0) {
      if (!SUMA_DsetColSelectList(SO, 0)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else if (strcmp(XtName(w), "T") == 0){
      if (!SUMA_DsetColSelectList(SO, 1)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else if (strcmp(XtName(w), "B") == 0){
      if (!SUMA_DsetColSelectList(SO, 2)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else if (strcmp(XtName(w), "Cmp") == 0){
      if (!SUMA_CmapSelectList(SO, 0)) {
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
   char *Xhair_help[]=  {  "Crosshair coordinates on\n"
                           "this controller's surface.\n"
                           "Entering new coordinates \n"
                           "makes the crosshair jump\n"
                           "to that location (like 'ctrl+j').\n"
                           "Use 'alt+l' to center the\n"
                           "cross hair in your viewer." , NULL};
   char *Node_tit[]=    {  "Node"   , NULL};
   char *Node_hint[]=   {  "Node index", NULL};
   char *Node_help[]=   {  "Node index of node in\n"
                           "focus on this controller's\n"
                           "surface. Nodes in focus are\n"
                           "highlighted by the blue sphere\n"
                           "in the crosshair.\n"
                           "Entering a new node's index\n"
                           "will put that node in focus\n"
                           "and send the crosshair to its\n"
                           "location (like 'j').\n"
                           "Use 'alt+l' to center the\n"
                           "cross hair in your viewer." , NULL};
   char *Face_tit[]=    {  "Tri ", NULL};
   char *Face_hint[]=   {  "1- Triangle index, 2- Nodes forming tiangle", NULL};
   char *Face_help[]=   {  "1- Triangle (faceset) index of\n"
                           "triangle in focus on this \n"
                           "on this controller's surface.\n"
                           "Triangle in focus is highlighted\n"
                           "in gray. Entering a new triangle's\n"
                           "index will set a new triangle in\n"
                           "focus (like 'J').\n"
                           "2- Nodes forming triangle." , NULL};
   char *Data_tit[]=    {  "    ", "Intens", "Thresh", "Bright" , NULL};
   char *Data_hint[]=   {  "Data Values at node in focus", 
                           "Intensity (I) value", 
                           "Threshold (T) value", 
                           "Brightness modulation (B) value" , NULL};
   char *Data_help[]=   {  "Data Values at node in focus", 
                           "Intensity (I) value", 
                           "Threshold (T) value", 
                           "Brightness modulation (B) value" , NULL}; 
   
   char *Data_rtit[]=   {  "    ", "Val " , NULL};
   char *Data_rhint[]=  {  "Data Values at node in focus", 
                           "Data Values at node in focus" , NULL};
   char *Data_rhelp[]=  {  "Data Values at node in focus", 
                           "Data Values at node in focus" , NULL};
   
   char *Label_tit[]=   {  "Lbl ", NULL};
   char *Label_hint[]=  {  "Color at node in focus", NULL};
   char *Label_help[]=  {  "Color at node in focus.\n"
                           "For the moment, only color\n"
                           "is displayed. The plan is\n"
                           "to display labels of various\n"
                           "sorts here.\n" , NULL};
   
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
         Data_rhint, Data_hint,
         Data_rhelp, Data_help,
         colw, NOPE, SUMA_float,
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
         XmNresizeHeight, False, /* important that this rc is not to be resized automatically,
                                    otherwise, the fix SUMA_FORCE_SCALE_HEIGHT will fail */
         XmNresizeWidth, False,
         XmNwidth, SUMA_SCALE_WIDTH, 
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
      sprintf(slabel,"Thr.");
      SO->SurfCont->thr_lb = XtVaCreateManagedWidget (slabel, 
                                          xmLabelWidgetClass, rct,
                                          XmNwidth, SUMA_SCALE_WIDTH,
                                          XmNrecomputeSize, False,   /* don't let it change size, it messes up the slider */ 
                                          NULL);
                                           
      /* add a vertical scale for the intensity */
      SO->SurfCont->thr_sc = XtVaCreateManagedWidget("Thr.",
                                          xmScaleWidgetClass, rct,
                                          XtVaNestedList, arglist,
                                          NULL);

      XtAddCallback (SO->SurfCont->thr_sc, XmNvalueChangedCallback, SUMA_cb_set_threshold, (XtPointer) SO);
      
      XtAddCallback (SO->SurfCont->thr_sc, XmNdragCallback, SUMA_cb_set_threshold_label, (XtPointer) SO); 
      
      #if 0
      /* put a string for the pvalue */
      sprintf(slabel,"[N/A]");
      SO->SurfCont->thrstat_lb = XtVaCreateManagedWidget (slabel, 
                                          xmLabelWidgetClass, rct,
                                          NULL);
      #endif
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
      /* need another rc for the cmap to avoid having the glxarea resized by cmaptit_lb
      and SwitchCmapMenu */
      rcc2 = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, rcc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight , 0 ,
         XmNmarginWidth  , 0 ,
         NULL);
      
      /* open me a glxarea */
      SO->SurfCont->cmap_wid = XtVaCreateManagedWidget("glxarea",
                                          glwDrawingAreaWidgetClass, rcc2,
                                          GLwNvisualInfo, SUMAg_SVv[0].X->VISINFO,
                                          XtNcolormap, SUMAg_SVv[0].X->CMAP,
                                          XmNwidth,   SUMA_CMAP_WIDTH,
                                          XmNheight,  SUMA_CMAP_HEIGHT,
                                          NULL);

      XtManageChild (rcc2);
      
      /* add me some callbacks */
      XtAddCallback(SO->SurfCont->cmap_wid, GLwNginitCallback, SUMA_cmap_wid_graphicsInit, (XtPointer )SO);
      XtAddCallback(SO->SurfCont->cmap_wid, GLwNexposeCallback, SUMA_cmap_wid_expose, (XtPointer )SO);
      XtAddCallback(SO->SurfCont->cmap_wid, GLwNresizeCallback, SUMA_cmap_wid_resize, (XtPointer )SO);
      XtAddCallback(SO->SurfCont->cmap_wid, GLwNinputCallback, SUMA_cmap_wid_input, (XtPointer )SO);
      
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
   int i;
   void (*callback)();
   NI_element *nel = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN (menu);}
   if (!SO->SurfCont) { SUMA_SL_Err("NULL SO->SurfCont"); SUMA_RETURN (menu);}
   if (!SO->SurfCont->curColPlane) { SUMA_SL_Err("NULL SO->SurfCont->curColPlane"); SUMA_RETURN (menu);}
   if (!SO->SurfCont->curColPlane->dset_link) { SUMA_SL_Err("NULL SO->SurfCont->curColPlane->dset_link"); SUMA_RETURN (menu);}

   nel = SO->SurfCont->curColPlane->dset_link->nel;
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
   menu = (SUMA_MenuItem *)SUMA_malloc(sizeof(SUMA_MenuItem)*(nel->vec_num+1));
   

   /* fillup menu */
   for (i=0; i < nel->vec_num; ++i) {
      menu[i].label = SUMA_ColLabelCopy(nel, i);
      menu[i].class = &xmPushButtonWidgetClass;
      menu[i].mnemonic = '\0';
      menu[i].accelerator = NULL;
      menu[i].accel_text = NULL;
      menu[i].callback = callback;
      menu[i].callback_data = (XtPointer)i+1; /* DO NOT USE THE zeroth item */
      menu[i].subitems = NULL;
   }
   
   /* add the stop sign. That's what SUMA_BuildMenu uses to figure out the number of elements */
   menu[nel->vec_num].label = NULL;
   
   *N_items = nel->vec_num;
      
   SUMA_RETURN (menu);
}

void SUMA_ShowMeTheChildren(Widget w)
{
   static char FuncName[]={"SUMA_ShowMeTheChildren"};
   Widget * children ;
   int  num_children , ic , Nbutt=0;
   
   SUMA_ENTRY;
   
   XtVaGetValues( w ,         XmNchildren    , &children ,
                              XmNnumChildren , &num_children , 
                              XmNbuttonCount, &Nbutt,
                              NULL ) ;
   
   fprintf (SUMA_STDERR, "%s: The (%d) children of %s (%d N_butts):\n", FuncName, num_children, XtName(w), Nbutt);
   for( ic=0 ; ic < num_children ; ic++ ){
      /* what's the name jane ? */
      XtVaGetValues (children[ic], XmNbuttonCount, &Nbutt, NULL);
      fprintf (SUMA_STDERR, "%d: %s (%d N_butts)\n", ic, XtName(children[ic]), Nbutt);
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
   menu = (SUMA_MenuItem *)SUMA_malloc(sizeof(SUMA_MenuItem)*(N_maps+1));
   
   /* fillup menu */
   for (i=0; i < N_maps; ++i) {
      menu[i].label = SUMA_copy_string(CMv[i]->Name);
      menu[i].class = &xmPushButtonWidgetClass;
      menu[i].mnemonic = '\0';
      menu[i].accelerator = NULL;
      menu[i].accel_text = NULL;
      menu[i].callback = callback;
      menu[i].callback_data = (XtPointer)CMv[i]; /* (used to be i+1)DO NOT USE THE 0 for the first button. 0th index is reserved for the rc widget */
      menu[i].subitems = NULL;
   }
   
   /* add the stop sign. That's what SUMA_BuildMenu uses to figure out the number of elements */
   menu[N_maps].label = NULL;
      
   SUMA_RETURN (menu);
}

void SUMA_SetScaleRange(SUMA_SurfaceObject *SO, float range[2])   
{
   static char FuncName[]={"SUMA_SetScaleRange"};
   int min_v, max_v, scl, dec, cv;
   Widget w ;
   char slabel[100];
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
    
   if (range[1] - range[0] > pow(10.0,SUMA_SCL_POW_BIAS)) { /* no need for power */
      dec = 0;
      min_v = (int)(range[0] ); 
      max_v = (int)(range[1] ); 
      scl = max_v / 1000; 
   } else {
      /* what power of 10 is needed (based on Bob's settings in afni_wid.c)? */
      dec = (int)ceil( log((double)(range[1] - range[0] + 0.001)) / log (10) );
      /* Add the scale bias, so that dec is at least = bias*/
      if (dec < SUMA_SCL_POW_BIAS) dec = SUMA_SCL_POW_BIAS;
      min_v = (int)(range[0] * pow(10.0, dec)); 
      max_v = (int)(range[1] * pow(10.0, dec) + 0.001); 
      scl = max_v / 1000; 
      
   }  
   if (max_v <= min_v || scl < 0) { /* happens when max_v is so large that you get trash when typecast to int.
                           That's the case when you're using the number of nodes in a surface for example
                           and you set dec to something demented like 6 ! Need a clever function here */
      SUMA_SLP_Note("Bad auto scaling \nparameters for threshold bar.\nUsing defaults"); 
      min_v = (int)(range[0]); 
      max_v = (int)(range[1])+1; 
      scl = max_v / 1000;
      dec = 1;     
   }
   /* make sure the current value is not less than the min or greater than the max */
   XtVaGetValues(w, XmNvalue, &cv, NULL);
   if (LocalHead) fprintf (SUMA_STDERR, "%s:\n min %d max %d scalemult %d decimals %d\nCurrent scale value %d\n", 
                  FuncName, min_v, max_v, scl, dec, cv);  
   if (cv < min_v) cv = min_v;
   else if (cv > max_v) cv = max_v;
   
   /* set the slider bar */
   XtVaSetValues(w,  
            XmNmaximum, max_v, 
            XmNminimum, min_v,
            XmNvalue, cv, 
            XmNscaleMultiple, scl,  
            XmNdecimalPoints , dec,
            XmNuserData, (XtPointer)dec,   
            NULL);   
   
   /* set the label on top */
   if (SO->SurfCont->curColPlane->OptScl->ThrMode != SUMA_ABS_LESS_THAN) 
      sprintf(slabel, "%5s", MV_format_fval((float)cv / pow(10.0, dec))); 
   else
      sprintf(slabel, "|%5s|", MV_format_fval((float)cv / pow(10.0, dec))); 
   
   SUMA_SET_LABEL(SO->SurfCont->thr_lb,  slabel);
   
            
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
   int i, N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   SUMA_DO *dov = SUMAg_DOv;
   SUMA_SurfaceObject *SO=NULL, *curSO=NULL;
   char str[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (!sv) SUMA_RETURN(NOPE);
   
   /* Which surfaces are visible in this SV ? */
   N_SOlist = SUMA_VisibleSOs(sv, dov, SOlist);
   for (i=0; i<N_SOlist; ++i) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: working %d/%d shown surfaces ...\n", 
                                          FuncName, i, N_SOlist);
      SO = (SUMA_SurfaceObject *)dov[SOlist[i]].OP;
      if (SO->SurfCont) { /* does this surface have surface controller ? */
         /* is this controller, displaying information for that surface ? */
         curSO = *(SO->SurfCont->curSOp);
         if (curSO == SO) {
            /* OK, show the coordinate */
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Setting cross hair at %f %f %f\n", 
               FuncName, sv->Ch->c[0],sv->Ch->c[1],sv->Ch->c[2]);
            SUMA_XHAIR_STRING(sv->Ch->c, str);
            SUMA_LH(str);
            XtVaSetValues(SO->SurfCont->XhairTable->cells[1], XmNvalue, str, NULL);
         }
      }
      
   }
   
   SUMA_RETURN(YUP);
   
}
 
SUMA_Boolean SUMA_UpdateNodeField(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_UpdateNodeField"};
   SUMA_SurfaceObject *curSO=NULL;
   
   SUMA_ENTRY; 
   
   if (!SO) SUMA_RETURN(NOPE);
   
   if (SO->SurfCont) {
      curSO = *(SO->SurfCont->curSOp);
      if (curSO == SO) {
         SUMA_UpdateNodeNodeField(SO);
         /* Now get the data values at that node */
         SUMA_UpdateNodeValField(SO);
         /* now find that node in the colored list */
         SUMA_UpdateNodeLblField(SO);
                  
      }
   }
         
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_UpdateNodeNodeField(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_UpdateNodeNodeField"};
   char str[100];

   SUMA_ENTRY; 

   if (!SO) SUMA_RETURN(NOPE);

   sprintf(str, "%d", SO->SelectedNode);
   SO->SurfCont->NodeTable->num_value[1] = SO->SelectedNode;
   XtVaSetValues(SO->SurfCont->NodeTable->cells[1], XmNvalue, str, NULL);
   sprintf(str, "%s",          MV_format_fval2(SO->NodeList[3*SO->SelectedNode], 7));
   sprintf(str, "%s, %s", str, MV_format_fval2(SO->NodeList[3*SO->SelectedNode+1], 7));
   sprintf(str, "%s, %s", str, MV_format_fval2(SO->NodeList[3*SO->SelectedNode+2], 7) );
   XtVaSetValues(SO->SurfCont->NodeTable->cells[2], XmNvalue, str, NULL);

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_UpdateNodeValField(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_UpdateNodeValField"};
   SUMA_OVERLAYS *Sover=NULL;
   char *str_int=NULL, *str_thr=NULL, *str_brt=NULL;
   double dval;
   int Found = -1;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SO) SUMA_RETURN(NOPE);
   Sover = SO->SurfCont->curColPlane;

   /* 1- Where is this node in the data set ? */
   Found = SUMA_GetNodeColIndex(Sover->dset_link->nel, SO->SelectedNode);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Node index %d is at row %d in dset.", FuncName, SO->SelectedNode, Found);
   }

   if (Found >= 0) {
      /* 2- What is the value of the intensity */
      str_int = SUMA_GetValInCol(Sover->dset_link->nel, Sover->OptScl->find, Found, &dval);
      if (str_int) {
         SUMA_LH(str_int);
         SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 1, str_int);
      } else {
         SUMA_SL_Err("Failed to get str_int");
      }
      str_thr = SUMA_GetValInCol(Sover->dset_link->nel, Sover->OptScl->tind, Found, &dval);
      if (str_thr) {
         SUMA_LH(str_thr);
         SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 2, str_thr);
      } else {
         SUMA_SL_Err("Failed to get str_thr");
      }
      str_brt = SUMA_GetValInCol(Sover->dset_link->nel, Sover->OptScl->bind, Found, &dval);
      if (str_brt) {
         SUMA_LH(str_brt);
         SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 3, str_brt);
      } else {
         SUMA_SL_Err("Failed to get str_brt");
      }
      if (str_int) SUMA_free(str_int); str_int = NULL;
      if (str_thr) SUMA_free(str_thr); str_thr = NULL;
      if (str_brt) SUMA_free(str_brt); str_brt = NULL;
   } else {
      SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 1, "NoData");
      SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 2, "NoData");
      SUMA_INSERT_CELL_STRING(SO->SurfCont->DataTable, 1, 3, "NoData");
   }
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_UpdateNodeLblField(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_UpdateNodeLblField"};
   int Found = -1;
   char str_col[100];
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SO) SUMA_RETURN(NOPE);

   Sover = SO->SurfCont->curColPlane;
   
   if (!Sover->Show) {
      SUMA_LH("Col plane hidden");
      sprintf(str_col,"hidden color overlay");
      SUMA_INSERT_CELL_STRING(SO->SurfCont->LabelTable, 0, 1, str_col);
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
            XtVaGetValues(SO->SurfCont->DataTable->cells[1*SO->SurfCont->DataTable->Ni+1], XmNvalue, &n, NULL);
            if (strcmp((char *)n, "NoData") == 0) {
               /* no data at all */
               sprintf(str_col,"no data for this node");
               Reasoned = YUP;
            }
         }
      }
      if (!Reasoned && SO->SurfCont->DataTable) { /* is the value 0 ? */
         SUMA_LH("Checking if node value is zero & zero is not being shown");
         if (Sover->OptScl->MaskZero && SO->SurfCont->DataTable->num_value[1*SO->SurfCont->DataTable->Ni+1]) {
            sprintf(str_col,"masked by zero value");  
         }   
      }
      SUMA_INSERT_CELL_STRING(SO->SurfCont->LabelTable, 0, 1, str_col);
   } else {
      /* Now we know what the index of this node is in the overlay plane (and the data) */
      sprintf(str_col,"%s",              MV_format_fval2(Sover->ColVec[3*Found],5));
      sprintf(str_col,"%s, %s", str_col, MV_format_fval2(Sover->ColVec[3*Found+1],5)); 
      sprintf(str_col,"%s, %s", str_col, MV_format_fval2(Sover->ColVec[3*Found+2],5));
      SUMA_LH(str_col);
      SUMA_INSERT_CELL_STRING(SO->SurfCont->LabelTable, 0, 1, str_col);
   }

   SUMA_RETURN(YUP);
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
            XtVaSetValues(SO->SurfCont->FaceTable->cells[1], XmNvalue, str, NULL);
            sprintf(str, "%d, %d, %d", 
               SO->FaceSetList[3*SO->SelectedFaceSet], SO->FaceSetList[3*SO->SelectedFaceSet+1],
               SO->FaceSetList[3*SO->SelectedFaceSet+2]);
            XtVaSetValues(SO->SurfCont->FaceTable->cells[2], XmNvalue, str, NULL);
         } else {
            XtVaSetValues(SO->SurfCont->FaceTable->cells[1], XmNvalue, "-1", NULL);
            SO->SurfCont->FaceTable->num_value[1] = -1;
            XtVaSetValues(SO->SurfCont->FaceTable->cells[2], XmNvalue, "x, x, x", NULL);
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
   /* look for a viewer that is showing this surface and has this surface in focus*/
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, i);
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


#ifdef SUMA_SHOW_CMAP_STAND_ALONE

/* This was to test the X way of displaying a colormap. 
TO HELL WITH IT, used OpenGL instead
*/

static MCW_DC *cmapdc = NULL;
static char * vcl[] =  { "StaticGray"  , "GrayScale" , "StaticColor" ,
                         "PseudoColor" , "TrueColor" , "DirectColor"  } ;

static XImage * xim    = NULL ;
static int      xim_ww = 0 ;
static int      xim_hh = 0 ;


/*-------------------------------------------------------------------*/
#ifdef __GNUC__
# define INLINE inline
#else
# define INLINE /*nada*/
#endif
/*-------------------------------------------------------------------*/
/*! Local copy of function from display.c (and from xim.c)
---------------------------------------------------------------------*/
static INLINE Pixel SUMA_tc_rgb_to_pixel( MCW_DC * dc, byte rr, byte gg, byte bb )
{
   static MCW_DC * dcold=NULL ;
   DC_colordef * cd = dc->cdef ;
   static unsigned long pold=0 ;
   static byte rold=0 , gold=0 , bold=0 ;
   unsigned long r , g , b ;

   if( cd == NULL ){ reload_DC_colordef(dc) ; cd = dc->cdef ; }

   if( rr == 0   && gg == 0   && bb == 0   ) return 0 ;          /* common */
   if( rr == 255 && gg == 255 && bb == 255 ) return cd->whpix ;  /* cases  */

   if( dc == dcold && rr == rold && gg == gold && bb == bold ) /* Remembrance of Things Past? */
      return (Pixel) pold ;

   rold = rr ; gold = gg ; bold = bb ; dcold = dc ;            /* OK, remember for next time */

   r = (cd->rrshift<0) ? (rr<<(-cd->rrshift))
                       : (rr>>cd->rrshift)   ; r = r & cd->rrmask ;

   g = (cd->ggshift<0) ? (gg<<(-cd->ggshift))
                       : (gg>>cd->ggshift)   ; g = g & cd->ggmask ;

   b = (cd->bbshift<0) ? (bb<<(-cd->bbshift))
                       : (bb>>cd->bbshift)   ; b = b & cd->bbmask ;

   pold = r | g | b ;  /* assemble color from components */
   return (Pixel) pold ;
}

XImage *SUMA_cmap_to_XImage (Widget wid, SUMA_COLOR_MAP *cm)
{
   static char FuncName[]={"SUMA_cmap_to_XImage"};
   Pixel * par ;
   int ii;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!cm) { SUMA_SL_Err("NULL cm"); SUMA_RETURN(xim); }
   if (!wid) { SUMA_SL_Err("NULL wid"); SUMA_RETURN(xim); }
   
   if (!cmapdc) {
      SUMA_LH("Initializing cmapdc");
      cmapdc = MCW_new_DC( wid, 128, 0, NULL,NULL, 1.0, 0 );
      SUMA_LH("Initialization done");
      if (!cmapdc) { SUMA_SL_Err("Failed in MCW_new_DC"); SUMA_RETURN(xim); }
      switch( cmapdc->visual_class ){
         case TrueColor:   break ;
         case PseudoColor: 
            SUMA_SL_Err("No PseudoColor support here.");
            SUMA_RETURN(xim);
      }   
   } 
   
   /* create XImage*/
   par = (Pixel *) malloc(sizeof(Pixel)*cm->N_Col); 
   if( par == NULL ) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(NULL) ;
   }
   SUMA_LH("rgb to pixel");
   for( ii=0 ; ii < cm->N_Col ; ii++ ) {
     par[ii] = SUMA_tc_rgb_to_pixel(  cmapdc , 
                                 (byte)(cm->M[ii][0]/255.0),
                                 (byte)(cm->M[ii][1]/255.0),
                                 (byte)(cm->M[ii][2]/255.0) ) ;

   }
   
   SUMA_LH("pixar_to_XImage");
   xim = pixar_to_XImage( cmapdc , cm->N_Col , 1 , par ) ;
   
   free(par);   
   SUMA_RETURN(xim);
}

void usage_ShowCmap()
{
   static char FuncName[]={"usage_ShowCmap"};
   char * s = NULL;
      s = SUMA_help_basics();
      printf ( "\nUsage:  \n"
               "%s\n"
               "\n", s); SUMA_free(s); s = NULL;
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
     printf("      Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \n\t\t Tue Dec 30\n"
            "\n");   
}
void fred_CB( Widget w , XtPointer cd , XtPointer cb ){ exit(0); }
void elvis_CB( Widget w , XtPointer cd , XtPointer cb )
{
   static int needGC = 1 ;
   static  GC myGC ;
   XmDrawingAreaCallbackStruct * cbs = (XmDrawingAreaCallbackStruct *) cb ;
   XExposeEvent * ev = (XExposeEvent *) cbs->event ;
   Dimension nx , ny ;
   int ii , jj ;

   if( cbs->reason != XmCR_EXPOSE || ev->count > 0 ) return ;

   if( needGC ){
     XGCValues  gcv;
     gcv.function = GXcopy ;
     myGC  = XCreateGC( XtDisplay(w) , XtWindow(w) , GCFunction , &gcv ) ;
     needGC = 0 ;
   }

   XtVaGetValues( w , XmNwidth  , &nx , XmNheight , &ny , NULL ) ;

   ii = 0 ;
   do{
      jj = 0 ;
      do{
         XPutImage( XtDisplay(w),XtWindow(w),myGC,xim,0,0,ii,jj,xim_ww,xim_hh) ;
         jj += xim_hh + 4 ;
      } while( jj < ny ) ;
      ii += xim_ww ;
   } while( ii < nx ) ;

   return ;
}


int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ShowCmap"};
   int kar, i;
   SUMA_Boolean brk;
   SUMA_COLOR_MAP *cmap;
   int icmap;

   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_mainENTRY;
   
   SUMA_STANDALONE_INIT;
   
   icmap = SUMA_Find_ColorMap ("bgyr64", SUMAg_CF->scm->CMv, SUMAg_CF->scm->N_maps, -2 );
   if (icmap < 0) { SUMA_SL_Err("Failed to find ColMap"); SUMA_RETURN(0); }
   cmap = SUMAg_CF->scm->CMv[icmap];
   
   #ifdef Xway
   {
      XtAppContext    app;            /* the application context */
      Widget          top;            /* toplevel widget */
      Display         *dpy;           /* display */
      Colormap        colormap;       /* created colormap */
      XVisualInfo     vinfo;          /* template for find visual */
      Visual          *vis ;          /* the Visual itself */
      XVisualInfo     *vinfo_list;    /* returned list of visuals */
      int             count;          /* number of matchs (only 1?) */
      int             vid , stat ;
      Widget          fred , fff ;
      
      SUMA_S_Note("XtVaCreateWidget");
      top = XtVaAppInitialize( &app , "test" , NULL , 0 , &argc , argv , NULL , NULL ) ;
      dpy = XtDisplay (top);
      SUMA_S_Note("XGetVisualInfo");

      vinfo.screen = DefaultScreen(dpy);
      vinfo_list = XGetVisualInfo (dpy, VisualScreenMask, &vinfo, &count);
      if( count == 0 || vinfo_list == NULL ){fprintf(stderr,"no match\n");exit(1);}
      vinfo = vinfo_list[0] ;
      vid = vinfo.visualid ;
      vis = vinfo.visual ;

      SUMA_S_Note("XCreateColormap");
      colormap = XCreateColormap( dpy, RootWindowOfScreen(XtScreen(top)) ,
                                 vis , AllocNone ) ;

      XtVaSetValues( top ,
                       XtNborderColor , 0 ,
                       XtNbackground  , 0 ,
                       XtNdepth       , vinfo.depth ,
                       XtNcolormap    , colormap ,
                       XtNvisual      , vis ,
                    NULL ) ;

      fff = XtVaCreateWidget( "dialog" , xmFormWidgetClass , top ,
                              XmNborderWidth , 0 ,
                              NULL ) ;

      #ifndef LABEL_ARG
      #define LABEL_ARG(str) \
        XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
      #endif

      fred = XtVaCreateManagedWidget( "dialog" , xmPushButtonWidgetClass , fff ,
                                       LABEL_ARG("Jumpback") ,
                                       XmNtopAttachment    , XmATTACH_FORM ,
                                       XmNleftAttachment   , XmATTACH_FORM ,
                                       XmNrightAttachment  , XmATTACH_FORM ,
                                      NULL ) ;
      XtAddCallback( fred , XmNactivateCallback , fred_CB , NULL ) ;

      fred = XtVaCreateManagedWidget( "dialog" , xmDrawingAreaWidgetClass , fff ,
                                       XmNtopAttachment    , XmATTACH_WIDGET ,
                                       XmNtopWidget        , fred ,
                                       XmNleftAttachment   , XmATTACH_FORM ,
                                       XmNrightAttachment  , XmATTACH_FORM ,
                                       XmNbottomAttachment , XmATTACH_FORM ,
                                     NULL ) ;

      XtAddCallback( fred , XmNexposeCallback , elvis_CB , NULL ) ;

      SUMA_SL_Note("SUMA_cmap_to_XImage");
      xim = SUMA_cmap_to_XImage(fred, cmap);

      SUMA_SL_Note("XPutImage");
      xim_ww = cmap->N_Col;
      xim_hh = 20;

      XtVaSetValues( top ,
                      XmNwidth , xim_ww ,
                      XmNheight , xim_hh+40 ,
                    NULL ) ;

      XtManageChild(fff) ;
      XtRealizeWidget(top);
      XtAppMainLoop(app);
   }
   #else 
   {  /* do the deed with OPENGL */
      
   
   }
   #endif
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   SUMA_RETURN (0);
}

#endif   /* end of SUMA_SHOW_CMAP_STAND_ALONE */
