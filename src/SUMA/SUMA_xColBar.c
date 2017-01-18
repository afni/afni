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
         SUMA_SL_Err("Failed to read %s", fname); }
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
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }
   if (!(SurfCont=SUMA_ADO_Cont(ado))) {
      SUMA_S_Err("NULL Cont!!!"); SUMA_RETURNe;
   }
   
   XtVaGetValues(w, GLwNvisualInfo, &SUMAg_cVISINFO, NULL);
   SurfCont->cmp_ren->cmap_context = 
      glXCreateContext( XtDisplay(w), SUMAg_cVISINFO,
                        0,                  /* No sharing. */
                        True);              /* Direct rendering if possible. */
   
   /* Setup OpenGL state. */
   if (!SUMA_glXMakeCurrent( XtDisplay(w), XtWindow(w), 
                        SurfCont->cmp_ren->cmap_context, 
                        FuncName, "some cmap init", 1)) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_glXMakeCurrent.\n \tContinuing ...\n", 
               FuncName);
      SUMA_GL_ERRS;
      SUMA_RETURNe;
   }
   
   /* call context_Init to setup colors and lighting */   
   SUMA_cmap_context_Init(ado);

   SUMA_RETURNe;
}

/*!
   Originally intended to setup for an Ortho2D mode for an image display
   But we don't like 2D. So this ends up being very much like 
   SUMA_context_Init, lookt here for reference if need be.
*/
void SUMA_cmap_context_Init(SUMA_ALL_DO *ado)
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
   
   /*setup the view point and then setup the lights. 
   Those lights will remain in place regardless of the rotations/translations
   done on the surface */
   for (i=0; i<2; ++i) { ViewCenter[i] = (CmapTL[i] - CmapOrig[i]) / 2.0;  }
   ViewFrom[0] = ViewCenter[0]; 
   ViewFrom[1] = ViewCenter[1]; ViewFrom[2] = 
   SUMA_CMAP_VIEW_FROM;
    
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
   /* SUMA_cmap_context_Init((SUMA_ALL_DO*)Cmap->SO); */
 
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

void SUMA_cmap_wid_display(SUMA_ALL_DO *ado)
{   
   static char FuncName[]={"SUMA_cmap_wid_display"};
   int i;
   GLfloat rotationMatrix[4][4];
   float currentQuat[]={0.0, 0.0, 0.0, 1.0};
   GLfloat clear_color[] = { 0.8, 0.8, 0.8, 0.0};
   GLfloat RotaCenter[]={0.0, 0.0, 0.0};
   SUMA_COLOR_MAP *Cmap = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
    
   SUMA_ENTRY;
   
   SUMA_LH("in, lots of inefficiencies here, make sure you revisit");
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
  
   /* is surface controller closed? */
   if (!SurfCont->Open) {
      SUMA_LHv("SurfCont closed for %s, trying to open it\n", 
               SUMA_ADO_Label(ado));
      if (!SUMA_viewSurfaceCont(NULL, ado, SUMA_BestViewerForADO(ado))) {
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
               SurfCont->cmp_ren->translateVec[0],
               SurfCont->cmp_ren->translateVec[1], 
               SurfCont->cmp_ren->translateVec[2] );
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluPerspective(SurfCont->cmp_ren->FOV, 
                  (double)SUMA_CMAP_WIDTH/SUMA_CMAP_HEIGHT, 
                  SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); 
                  /*lower angle is larger zoom,*/

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glTranslatef ( SurfCont->cmp_ren->translateVec[0],
                  SurfCont->cmp_ren->translateVec[1], 
                  SurfCont->cmp_ren->translateVec[2] );
   if (0){
   SUMA_SL_Note("no need for shananigans\n"
                  "But to illustrate ...\n");
   glTranslatef (RotaCenter[0], RotaCenter[1], RotaCenter[2]);
   glMultMatrixf(&rotationMatrix[0][0]);
   glTranslatef (-RotaCenter[0], -RotaCenter[1], -RotaCenter[2]);
   }
   
   
   /* find out what colormap is to be displayed */
   if (curColPlane) {
      /* what's the Cmap for that plane ? */
      Cmap = SUMA_CmapOfPlane (curColPlane );
      if (Cmap) SUMA_DrawCmap(Cmap); /* create the colormap */
   } else {
      SUMA_SL_Err("NULL curColPlane");
   }   
   glPopMatrix();   

   if (LocalHead) 
      fprintf (SUMA_STDERR,
               "%s: Flushing or swapping ...\n"
               "cmp_ren %p, cmap_wid %p\n", 
               FuncName,
               SurfCont ? SurfCont->cmp_ren : NULL, 
               (SurfCont && SurfCont->cmp_ren ) ? 
                  SurfCont->cmp_ren->cmap_wid : NULL );
   
   if (SUMAg_SVv[0].X->DOUBLEBUFFER)
      glXSwapBuffers(XtDisplay(SurfCont->cmp_ren->cmap_wid), 
                     XtWindow(SurfCont->cmp_ren->cmap_wid));
   else  
      glFlush();

   /* Avoid indirect rendering latency from queuing. */
   
   if (!glXIsDirect( XtDisplay(SurfCont->cmp_ren->cmap_wid), 
                     SurfCont->cmp_ren->cmap_context)) {
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
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
  
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL DO"); SUMA_RETURN(NOPE); }
   
   SurfCont = SUMA_ADO_Cont(ado);
   SUMA_LHv("Called with ado %s, SurfCont->Open: %d\n", 
            SUMA_ADO_Label(ado), SurfCont->Open);
   if (SurfCont->Open) { /* Otherwise it causes a crash on linux 
                                 when yoking L/R stuff */
      SUMA_LHv("Making cmap_wid current %p %p\n", 
               SurfCont->cmp_ren->cmap_wid, 
               SurfCont->cmp_ren->cmap_context);
      if (!SUMA_glXMakeCurrent( XtDisplay(SurfCont->cmp_ren->cmap_wid), 
                           XtWindow(SurfCont->cmp_ren->cmap_wid), 
            SurfCont->cmp_ren->cmap_context, FuncName, "some cmap resize", 1)) {
         SUMA_S_Err("Failed in SUMA_glXMakeCurrent.\n \tContinuing ...");
      }
      SUMA_LH("Calling wid display");
      SUMA_cmap_wid_display(ado);
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
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("cold");
   
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL DO"); SUMA_RETURNe; }

   SUMA_register_workproc(SUMA_cmap_wid_handleRedisplay , (XtPointer)ado );
   
   SUMA_RETURNe;
}

void SUMA_cmap_wid_expose(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cmap_wid_expose"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL DO"); SUMA_RETURNe; }
   
   SUMA_cmap_wid_postRedisplay(w, (XtPointer)ado, NULL);

   SUMA_RETURNe;
}

/* An attempt to render colormap using X11 and Motif rather than openGL.
This is only for the purpose of taking an autosnapshot of the whole widget 
Based on AFNI's PBAR_bigexpose_CB()*/
void SUMA_PBAR_bigexpose_CB(Widget wiw, XtPointer clientData, XtPointer calliw)
{
   static char FuncName[]={"SUMA_PBAR_bigexpose_CB"};
   SUMA_ALL_DO *ado=NULL;
   XVisualInfo *SUMAg_cVISINFO=NULL;
   static MCW_DC *dc = NULL;
   static XImage *bigxim = NULL;
   SUMA_X_SurfCont *SurfCont = NULL;
   int  ww = SUMA_CMAP_WIDTH, hh = SUMA_CMAP_HEIGHT;
   int ii , jj , kk;
   MRI_IMAGE *cim, *dim;
   byte      *car , r,g,b ;
   static SUMA_COLOR_MAP *CMd=NULL;
   SUMA_COLOR_MAP *CM=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) { 
      SUMA_SL_Err("NULL DO or Cont"); SUMA_RETURNe; 
   }
   
   /* Get colormap of plane, if possible */
   if ((curColPlane = SUMA_ADO_CurColPlane(ado))) {
      CM = SUMA_CmapOfPlane (curColPlane );
   }
   /* get some default */
   if (!CM) {
      if (!(CM = SUMA_FindNamedColMap("byr64"))) {
         SUMA_S_Err("Failed to get byr64");
         SUMA_RETURNe;
      }   
   }
   
   if (!CMd || CM != CMd) {
      SUMA_LH("Setting CMd");
      CMd = CM;
      if (bigxim) {
         SUMA_LH("Killing bigxim");
         MCW_kill_XImage( bigxim ); bigxim = NULL;
      }
   }
   
   if (!bigxim) {
      float fac; int mm;
      cim = mri_new( ww, 256 , MRI_rgb ) ;
      car = MRI_RGB_PTR(cim) ;
      fac = (float)CM->N_M[0]/256.0;
      for( kk=ii=0 ; ii < 256 ; ii++ ){
         mm = CM->N_M[0]-1-(int)((float)ii*fac);
         if (mm>=CM->N_M[0]) mm = CM->N_M[0]-1;
         if (mm<0) mm = 0;
         r=(byte)(CM->M[mm][0]*255.0); 
         g=(byte)(CM->M[mm][1]*255.0);   
         b=(byte)(CM->M[mm][2]*255.0);
         if( CM->M[mm][0] >= 0 || CM->M[mm][1] >= 0 || CM->M[mm][2] >= 0 ){
            for( jj=0 ; jj < ww ; jj++ ){
              car[kk++] = r; car[kk++] = g; car[kk++] = b;
            }
         } else {
            for( jj=0 ; jj < ww ; jj++ ){
              car[kk++]=128; car[kk++]=128; car[kk++]=128;
            }
         }
      }
      dim = mri_resize( cim , ww,hh ) ;
      if (!dc) {
         dc = MCW_new_DC( SurfCont->Fake_pbar, 4,0, NULL,NULL, 1.0,0 );
      }
      bigxim = mri_to_XImage( dc , dim ) ;
      mri_free(dim) ;
   }
   
   /* actually show the image to the window pane */
   if( XtIsManaged(SurfCont->Fake_pbar) )
     XPutImage( SUMAg_CF->X->DPY_controller1 , 
                XtWindow(SurfCont->Fake_pbar) ,
                dc->origGC , bigxim , 0,0,0,0 ,
                ww , hh ) ;

   SUMA_RETURNe;
}

void SUMA_PBAR_biginput_CB(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_PBAR_biginput_CB"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) { 
      SUMA_SL_Err("NULL DO or Cont"); SUMA_RETURNe; 
   }
   SUMA_RETURNe;
}

void SUMA_PBAR_bigresize_CB(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_PBAR_bigresize_CB"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) { 
      SUMA_SL_Err("NULL DO or Cont"); SUMA_RETURNe; 
   }
   SUMA_RETURNe;
}


void SUMA_cmap_wid_resize(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cmap_wid_resize"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL DO"); SUMA_RETURNe; }
   
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
   SUMA_ALL_DO *ado=NULL;
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
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;             
      /* THIS ado is for the main surface/DO, NOT THE colormap's */
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }

   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   ColMap = SUMA_CmapOfPlane (curColPlane );
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
   if (!SUMA_glXMakeCurrent( XtDisplay(w), 
                        XtWindow(w), 
            SurfCont->cmp_ren->cmap_context, FuncName, "some cmap input", 1)) {
      SUMA_S_Err("Failed in SUMA_glXMakeCurrent.\n ");
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
               if (1) { 
                  SUMA_LH("Flipping colormap");
                  SUMA_Flip_Color_Map(
                     SUMA_CmapOfPlane(curColPlane));
                  SUMA_LH("Switching colormap");
                  SUMA_SwitchCmap(ado,
                         SUMA_CmapOfPlane(curColPlane), 0);
               }
            }
            break;
         case XK_r:
            {
               GLvoid *pixels;
               SUMA_LH("Recording");
               if (SUMAg_SVv[0].X->DOUBLEBUFFER)
                  glXSwapBuffers(XtDisplay(SurfCont->cmp_ren->cmap_wid), 
                                 XtWindow(SurfCont->cmp_ren->cmap_wid));
               pixels = SUMA_grabPixels(3, SUMA_CMAP_WIDTH, SUMA_CMAP_HEIGHT);
               if (SUMAg_SVv[0].X->DOUBLEBUFFER)
                  glXSwapBuffers(XtDisplay(SurfCont->cmp_ren->cmap_wid), 
                                 XtWindow(SurfCont->cmp_ren->cmap_wid));
               if (pixels) {
                 ISQ_snapsave (SUMA_CMAP_WIDTH, -SUMA_CMAP_HEIGHT, 
                              (unsigned char *)pixels,
                              SurfCont->cmp_ren->cmap_wid ); 
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
                     SUMA_CmapOfPlane(curColPlane);
               if (LocalHead) {
                  if ((sss = SUMA_ColorMapVec_Info(&ColMap, 1, 1))) {
                     SUMA_LHv("To be written:\n%s\n", sss);
                     SUMA_free(sss); sss = NULL;
                  }
               }
               if (!SUMA_Write_Color_Map_1D(ColMap, NULL)) {
                  SUMA_S_Errv("Failed to write colmap %s\n", 
                           curColPlane->Name);
               }else {
                  SUMA_S_Notev("Wrote colormap %s to file.\n",
                           curColPlane->Name);
               }
            }
            break;
         case XK_Z:
            {
               static SUMA_Boolean BeepedAlready = NOPE;
               SurfCont->cmp_ren->FOV /= FOV_IN_FACT; 
               if (SurfCont->cmp_ren->FOV < fov_lim) { 
                  if (!BeepedAlready) {
                     SUMA_BEEP; BeepedAlready = YUP;
                  }
                  SurfCont->cmp_ren->FOV = fov_lim; 
               } else BeepedAlready = NOPE;
               if (LocalHead) 
                  fprintf(SUMA_STDERR,
                           "%s: Zoom in FOV = %f\n", 
                           FuncName, SurfCont->cmp_ren->FOV);
               SUMA_cmap_wid_postRedisplay(w, (XtPointer)ado, NULL);
            }
            break;

         case XK_z:
            {
               static SUMA_Boolean BeepedAlready = NOPE;
               SurfCont->cmp_ren->FOV /= FOV_OUT_FACT; 
               if (SurfCont->cmp_ren->FOV > SUMA_CMAP_FOV_INITIAL) { 
                  if (!BeepedAlready) {
                     SUMA_BEEP; BeepedAlready = YUP;
                  }
                  SurfCont->cmp_ren->FOV = SUMA_CMAP_FOV_INITIAL; 
               } else BeepedAlready = NOPE;
               if (LocalHead) 
                  fprintf( SUMA_STDERR,
                           "%s: Zoom out FOV = %f\n", 
                           FuncName, SurfCont->cmp_ren->FOV);
               SUMA_cmap_wid_postRedisplay(w, (XtPointer)ado, NULL);
            }
            break;
         case XK_Home:   
            SurfCont->cmp_ren->FOV = SUMA_CMAP_FOV_INITIAL;
            SurfCont->cmp_ren->translateVec[0] = 
            SurfCont->cmp_ren->translateVec[1] = 
            SurfCont->cmp_ren->translateVec[2] = 0.0;
            {
               SUMA_COLOR_MAP *CM = SUMA_CmapOfPlane(curColPlane);
               if (SUMA_Rotate_Color_Map(CM, 0) % CM->N_M[0]) { 
                  SUMA_SwitchCmap(ado,
                         SUMA_CmapOfPlane(curColPlane), 0);
               } else {
                  SUMA_cmap_wid_postRedisplay(w, (XtPointer)ado, NULL);
               }
            }
            break;
         case XK_Up:   /*KEY_UP:*/
            {
               if (Kev.state & ShiftMask){
                  static SUMA_Boolean BeepedAlready = NOPE;   
                  float tstep = height_two_col / 2.0 * 
                                 SurfCont->cmp_ren->FOV /
                                 (float)SUMA_CMAP_FOV_INITIAL; 
                  SurfCont->cmp_ren->translateVec[1] += tstep ;
                  if (LocalHead) 
                     fprintf(SUMA_STDERR,
                              "%s: translateVec[1] = %f (%d)\n", 
                              FuncName, 
                              SurfCont->cmp_ren->translateVec[1],
                              SUMA_CMAP_HEIGHT - 20);
                  if (  SurfCont->cmp_ren->translateVec[1] >  
                        SUMA_CMAP_HEIGHT - 20) {
                     if (!BeepedAlready) {
                        SUMA_BEEP; BeepedAlready = YUP;
                     }
                        SurfCont->cmp_ren->translateVec[1] -= tstep; 
                  } else BeepedAlready = NOPE;
                  SUMA_cmap_wid_postRedisplay(w, (XtPointer)ado, NULL);
               } else { 
                  float frac = 0.0;
                  if (Kev.state & ControlMask) {
                     frac = 1;
                  }else {
                     frac = SUMAg_CF->CmapRotaFrac;
                  }
                  SUMA_LH("Rotating colormap");
                  SUMA_Rotate_Color_Map(
                     SUMA_CmapOfPlane(curColPlane), frac);
                  SUMA_LH("Switching colormap");
                  SUMA_SwitchCmap(ado,
                         SUMA_CmapOfPlane(curColPlane), 0);
               }
            }
            break;
         case XK_Down:   /*KEY_DOWN:*/
            {
               if (Kev.state & ShiftMask){
                  static SUMA_Boolean BeepedAlready = NOPE;  
                  float tstep =  height_two_col / 2.0 * 
                                 SurfCont->cmp_ren->FOV / 
                                 (float)SUMA_CMAP_FOV_INITIAL; 
                  SurfCont->cmp_ren->translateVec[1] -=  tstep;
                  if (  SurfCont->cmp_ren->translateVec[1] <  
                        -SUMA_CMAP_HEIGHT + 20) {
                     if (!BeepedAlready) {
                        SUMA_BEEP; BeepedAlready = YUP;
                     }
                        SurfCont->cmp_ren->translateVec[1] += tstep; 
                  } else BeepedAlready = NOPE;
                  SUMA_cmap_wid_postRedisplay(w, (XtPointer)ado, NULL);
               } else {
                  float frac = 0.0;
                  if (Kev.state & ControlMask) {
                     frac = 1;
                  }else {
                     frac = SUMAg_CF->CmapRotaFrac;
                  }
                  SUMA_Rotate_Color_Map(
                     SUMA_CmapOfPlane(curColPlane), -frac);
                  SUMA_SwitchCmap(ado,
                         SUMA_CmapOfPlane(curColPlane), 0);
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

int SUMA_set_threshold_label(SUMA_ALL_DO *ado, float val, float val2)
{
   static char FuncName[]={"SUMA_set_threshold_label"};
   char slabel[100];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURN(0); }
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   switch (curColPlane->OptScl->ThrMode) {
      case SUMA_LESS_THAN: 
         sprintf(slabel, "%5s", MV_format_fval(val));
         break;
      case SUMA_ABS_LESS_THAN:
         /* used to use this:
         sprintf(slabel, "|%5s|", .... 
         but that does not work in the editable field ... */
         sprintf(slabel, "%5s", MV_format_fval(val));
         break;
      case SUMA_THRESH_INSIDE_RANGE:
         /* This is just a place holder for now */
         sprintf(slabel, "<%5s..%5s>", 
                       MV_format_fval(val), MV_format_fval(val2));
         break;
      case SUMA_THRESH_OUTSIDE_RANGE:
         /* This is just a place holder for now */
         sprintf(slabel, ">%5s..%5s<", 
                       MV_format_fval(val), MV_format_fval(val2));
         break;
      case SUMA_NO_THRESH:
         break;
      default:
         /* This is just a place holder for now */
         sprintf(slabel, "?%5s??%5s?<", 
                       MV_format_fval(val), MV_format_fval(val2));
         break;
   }
   /* SUMA_SET_LABEL(SurfCont->thr_lb,  slabel);*/
      SUMA_INSERT_CELL_STRING(SurfCont->SetThrScaleTable, 0,0,slabel); 

   
   /* You must use the line below if you are calling this function on the fly */
   /* SUMA_FORCE_SCALE_HEIGHT(SUMA_ADO_Cont(ado));  */
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   
   /* Will need to ponder Pvalue field when I start using
      SUMA_THRESH_INSIDE_RANGE and SUMA_THRESH_OUTSIDE_RANGE.
      Also, should also consider one tail versus two tail...*/
   SUMA_UpdatePvalueField (ado, val); 
   
   SUMA_RETURN(1);  
}

void SUMA_cb_set_threshold_label(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_threshold_label"};
   SUMA_ALL_DO *ado=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff ;
   int dec=0;
   char slabel[100];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }
   
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   
   SUMA_set_threshold_label(ado, fff, 0.0);
   
   SUMA_RETURNe;
}

int SUMA_set_threshold(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                           float *val)
{
   static char FuncName[]={"SUMA_set_threshold"};
   SUMA_SurfaceObject *SOC=NULL, *SO=NULL;
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_set_threshold_one(ado, colp, val)) SUMA_RETURN(0);
   if (!colp) colp = SUMA_ADO_CurColPlane(ado);
   if (!colp) SUMA_RETURN(0);
   
   if (ado->do_type == SO_type) {
      /* do we have a contralateral SO and overlay? */
      SO = (SUMA_SurfaceObject *)ado;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (!SUMA_SetScaleThr_one((SUMA_ALL_DO *)SOC, colpC, val, 1, 1)) {
            SUMA_S_Warn("Failed in contralateral");
            SUMA_RETURN(0);
         }
      } 
   }
   
   /* CIFTI yoking outline 
   
   if (colp->dset_link) is CIFTI subdomain (see a. below)
      Look up the parent cifti dset (cdset) by using MD_parent_ID 
      
      Look up also the CIFTI DO (CO) that goes with cdset using
      SUMA_Fetch_OverlayPointerByDset()
      
      For each subdomain in CO and each corresponding dset and color plane
      	 
	 If the subdomain and the colorplane are different from ado , adoC (that
	 would be (SUMA_ALL_DO *)SOC above), colp and colpC  
	 
	    call  SUMA_SetScaleThr_one() as per above 
   
   
   
   a. One way to find out if a dset is a subdomain would be to look for one of the MD_* attribute of dset->ngr that are created in SUMA_CIFTI_2_edset()
   
   */
   
   SUMA_RETURN(1);  
}

int SUMA_set_threshold_one(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                           float *valp)
{
   static char FuncName[]={"SUMA_set_threshold_one"};
   float oval, val;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURN(0);
   SurfCont = SUMA_ADO_Cont(ado);
   if (!colp) colp = SUMA_ADO_CurColPlane(ado);
   if (!colp) SUMA_RETURN(0);
   
   oval = colp->OptScl->ThreshRange[0];
   if (!valp) val = oval; /* a dirty trick to force scale height */
   else val = *valp;
   colp->OptScl->ThreshRange[0] = val;
 
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
      SUMA_Remixedisplay(ado);
   }

   /* call this one since it is not being called as the slider is dragged. */
   SUMA_set_threshold_label(ado, val, 0.0);   

   /* sad as it is */
   SUMA_FORCE_SCALE_HEIGHT(SUMA_ADO_Cont(ado)); 

   SUMA_ADO_Flush_Pick_Buffer(ado, NULL);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   
   SUMA_UpdateNodeValField(ado);
   SUMA_UpdateNodeLblField(ado);
   SUMA_UpdatePvalueField (ado, colp->OptScl->ThreshRange[0]);  
 
   SUMA_RETURN(1);
}

void SUMA_cb_set_threshold(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_threshold"};
   SUMA_ALL_DO *ado=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff=0.0;
   int dec=-1;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   SUMA_LHv("Have %f\n", fff);
   SUMA_set_threshold(ado, NULL, &fff);
   
   SUMA_RETURNe;
}

int SUMA_SwitchColPlaneIntensity(
         SUMA_ALL_DO *ado, 
         SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneIntensity"};
   char srange[500];
   double range[2];
   int loc[2];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_SwitchColPlaneIntensity_one(ado, colp, ind, setmen)) {
      SUMA_S_Err("Failed in _one");
      SUMA_RETURN(0);
   }

   if (ado->do_type == SO_type) {
      SUMA_SurfaceObject *SOC=NULL, *SO=NULL; 
      SUMA_OVERLAYS *colpC=NULL;
      /* do we have a contralateral SO and overlay? */
      SO = (SUMA_SurfaceObject *)ado;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (!SUMA_SwitchColPlaneIntensity_one(
                           (SUMA_ALL_DO *)SOC, colpC, ind, 1)) {
            SUMA_S_Warn("Failed in contralateral");
         }
      }
   }
   
   SUMA_RETURN(1);
}


/* changes you do here should be reflected under SE_SetSurfCont in SUMA_Engine*/
int SUMA_SwitchColPlaneIntensity_one (
         SUMA_ALL_DO *ado, 
         SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneIntensity_one"};
   char srange[500];
   double range[2];
   int loc[2];
   SUMA_DSET *dset=NULL;
   char *Label=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   Label = SUMA_ADO_Label(ado);
   
   if (  !ado || !SurfCont || 
         !curColPlane || 
         !colp || !colp->dset_link || !colp->OptScl) { SUMA_RETURN(0); }
   
   if (ind < 0) {
      if (ind == SUMA_BACK_ONE_SUBBRICK) {/* --1 */
         ind = colp->OptScl->find-1;
         if (ind < 0 || ind >= SDSET_VECNUM(colp->dset_link)) {
            SUMA_LH("Reached end"); 
            SUMA_BEEP;
            SUMA_RETURN(1);
         }
      } else if (ind == SUMA_FORWARD_ONE_SUBBRICK) {/* ++1 */
         ind = colp->OptScl->find+1;
         if (ind < 0 || ind >= SDSET_VECNUM(colp->dset_link)) {
            SUMA_LH("Reached end, return without complaint"); 
            SUMA_BEEP;
            SUMA_RETURN(1);
         }
      } else { SUMA_RETURN(0); }
   }
   
   if (LocalHead) {
      fprintf( SUMA_STDERR, 
               "%s:\n request to switch intensity to col. %d\n", 
               FuncName, ind);
      fprintf(SUMA_STDERR, "ADO->Label = %s\n", Label);
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
   if (setmen && colp == curColPlane && SurfCont->SwitchIntMenu) {
      SUMA_LHv("Setting menu values, %d\n", colp->OptScl->find+1);
      SUMA_Set_Menu_Widget(SurfCont->SwitchIntMenu, colp->OptScl->find+1);
   }
   
   
   dset=colp->dset_link;
   switch(curColPlane->LinkMode) {/* corresponding threshold sb */
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
                     if (colp == curColPlane && SurfCont->SwitchThrMenu){
                                          /* must set this
                                             regardless of setmen, but not if
                                       SwitchThrMenu has not be been set yet */
                        SUMA_LH("Setting threshold values");
                        SUMA_Set_Menu_Widget(SurfCont->SwitchThrMenu, 
                                      colp->OptScl->tind+1);
                        if (SUMA_GetDsetColRange(colp->dset_link, 
                                             colp->OptScl->tind, range, loc)) {  
                           SUMA_SetScaleRange(ado, range );
                           SUMA_InitRangeTable(ado, -1) ;
                           SUMA_UpdateNodeValField(ado);
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
         if (colp == curColPlane ) {/* must set this
                                 regardless of setmen */
            SUMA_LH("Setting threshold values");
            SUMA_Set_Menu_Widget(SurfCont->SwitchThrMenu, 
                          colp->OptScl->tind+1) ;
            if (SUMA_GetDsetColRange(colp->dset_link, 
                                 colp->OptScl->tind, range, loc)) {  
               SUMA_SetScaleRange(ado, range );
               SUMA_InitRangeTable(ado, -1) ;
               SUMA_UpdateNodeValField(ado);
            }else {
               SUMA_S_Err("Failed to get range");
            }
         }
         break;
      case SW_LinkMode_Pls1:
            {
         if (ind+1 >= SDSET_VECNUM(dset)) break; 
         colp->OptScl->tind = ind+1;
         if (colp == curColPlane ) {/* must set this
                                 regardless of setmen */
            SUMA_LH("Setting threshold values");
            SUMA_Set_Menu_Widget(SurfCont->SwitchThrMenu,
                          colp->OptScl->tind+1) ;
            if (SUMA_GetDsetColRange(colp->dset_link, 
                                 colp->OptScl->tind, range, loc)) {  
               SUMA_SetScaleRange(ado, range );
               SUMA_InitRangeTable(ado, -1) ;
               SUMA_UpdateNodeValField(ado);
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
   SUMA_InitRangeTable(ado, 0) ;
   SUMA_UpdateCrossHairNodeLabelFieldForDO(ado);

   if (colp->ShowMode < 0) { SUMA_RETURN(1); } /* nothing else to do */
   
   SUMA_ADO_Flush_Pick_Buffer(ado, NULL);

   if (!SUMA_ColorizePlane (colp)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(0);
   }
   
   
   SUMA_Remixedisplay(ado);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
 
   SUMA_UpdateNodeValField(ado);
   SUMA_UpdateNodeLblField(ado);
      
   SUMA_RETURN(1);
}

void SUMA_cb_SwitchIntensity(Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_SwitchIntensity"};
   int imenu = 0;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   imenu = (INT_CAST)datap->callback_data; 
   
   if (imenu-1 == curColPlane->OptScl->find) {
      SUMA_RETURNe; /* nothing to be done */
   }

   SUMA_SwitchColPlaneIntensity(ado, curColPlane, imenu -1, 0);
   
   SUMA_RETURNe;
}

int SUMA_SwitchColPlaneThreshold(
         SUMA_ALL_DO *ado, 
         SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneThreshold"};
   char srange[500];
   double range[2];
   int loc[2];
   SUMA_DSET *dset=NULL;
   SUMA_SurfaceObject *SOC=NULL, *SO=NULL; 
   SUMA_OVERLAYS *colpC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_SwitchColPlaneThreshold_one(ado, colp, ind, setmen)) {
      SUMA_S_Err("Failed in _one");
      SUMA_RETURN(0);
   }

   if (ado->do_type == SO_type) {
      SO = (SUMA_SurfaceObject *)ado;
      /* do we have a contralateral SO and overlay? */
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (!SUMA_SwitchColPlaneThreshold_one(ado, colpC, ind, 1)) {
            SUMA_S_Warn("Failed in contralateral");
         }
      }
   }
   
   SUMA_RETURN(1);
}

int SUMA_SwitchColPlaneThreshold_one(
                                 SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
                                 int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneThreshold_one"};
   char srange[500];
   double range[2]; int loc[2];  
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!ado || !SurfCont || !curColPlane || 
       !colp || ind < -1 || !colp->dset_link) { SUMA_RETURN(0); }
   
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n request to switch threshold to col. %d\n", 
                  FuncName, ind);
   }
   if (ind < 0) {
      /* turn threshold off */
      XmToggleButtonSetState (SurfCont->Thr_tb, NOPE, YUP);
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
      XmToggleButtonSetState (SurfCont->Thr_tb, YUP, NOPE);
   }
   
   if (setmen && colp == curColPlane) {
      SUMA_Set_Menu_Widget(SurfCont->SwitchThrMenu,
                    colp->OptScl->tind+1) ; 
   }
   
   if (SUMA_GetDsetColRange(colp->dset_link, colp->OptScl->tind, range, loc)) {  
      SUMA_SetScaleRange(ado, range );
   }else {
      SUMA_SLP_Err("Failed to get range");
      SUMA_RETURN(0);
   }
   
   /* threshold sub-brick change */ 
   SUMA_InitRangeTable(ado, -1) ;
   SUMA_UpdateCrossHairNodeLabelFieldForDO(ado);
   SUMA_UpdateNodeValField(ado);
   
   if (!colp->OptScl->UseThr) { SUMA_RETURN(1); } /* nothing else to do */

   SUMA_ADO_Flush_Pick_Buffer(ado, NULL);
   
   if (!SUMA_ColorizePlane (colp)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(0);
   }
   
   SUMA_Remixedisplay(ado);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   
   SUMA_UpdateNodeLblField(ado);

   SUMA_RETURN(1);
}

void SUMA_cb_SwitchThreshold(Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_SwitchThreshold"};
   int imenu = 0;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (imenu-1 == curColPlane->OptScl->tind) {
      SUMA_RETURNe; /* nothing to be done */
   }

   SUMA_SwitchColPlaneThreshold(ado, curColPlane, imenu -1, 0);
   SUMA_RETURNe;
}

int SUMA_SwitchColPlaneBrightness(
         SUMA_ALL_DO *ado, 
         SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneBrightness"};
   char srange[500];
   double range[2];
   int loc[2];
   SUMA_DSET *dset=NULL;
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
   
   if (!SUMA_SwitchColPlaneBrightness_one(ado, colp, ind, setmen)) {
      SUMA_S_Err("Failed in _one");
      SUMA_RETURN(0);
   }

   if (ado->do_type == SO_type) {
      SUMA_SurfaceObject *SOC=NULL; 
      SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
      /* do we have a contralateral SO and overlay? */
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (!SUMA_SwitchColPlaneBrightness_one((SUMA_ALL_DO *)SOC, 
                                                 colpC, ind, 1)) {
            SUMA_S_Warn("Failed in contralateral");
         }
      }
   }   
   SUMA_RETURN(1);
}

int SUMA_SwitchColPlaneBrightness_one(
         SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp, 
         int ind, int setmen)
{
   static char FuncName[]={"SUMA_SwitchColPlaneBrightness_one"};
   char srange[500];
   double range[2]; int loc[2];  
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) { 
      SUMA_SL_Err("NULL ado");
      SUMA_RETURN(0); 
   }
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (  !SurfCont || !curColPlane || 
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
      XmToggleButtonSetState (SurfCont->Brt_tb, NOPE, YUP);
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
      XmToggleButtonSetState (SurfCont->Brt_tb, YUP, NOPE);
   }
   
   if (setmen && colp == curColPlane) {
      SUMA_Set_Menu_Widget(SurfCont->SwitchBrtMenu, 
                    colp->OptScl->bind+1); 
   }
   
   if (SUMA_GetDsetColRange(colp->dset_link, colp->OptScl->bind, range, loc)) {   
      SUMA_SetScaleRange(ado, range );
   }else {
      SUMA_SLP_Err("Failed to get range");
      SUMA_RETURN(0);
   }
   
   /* brightness change */ 
   SUMA_InitRangeTable(ado, -1) ;
   SUMA_UpdateCrossHairNodeLabelFieldForDO(ado);
   SUMA_UpdateNodeValField(ado);
   
   if (!colp->OptScl->UseBrt) { SUMA_RETURN(1); } /* nothing else to do */

   if (!SUMA_ColorizePlane (colp)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(0);
   }
   
   SUMA_Remixedisplay(ado);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   
   SUMA_UpdateNodeLblField(ado);

   SUMA_RETURN(1);
}


void SUMA_cb_SwitchBrightness(Widget w, XtPointer client_data, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_SwitchBrightness"};
   int imenu = 0;
   SUMA_MenuCallBackData *datap=NULL;
   SUMA_ALL_DO *ado = NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   imenu = (INT_CAST)datap->callback_data; 
   
   if (imenu-1 == curColPlane->OptScl->bind) {
      SUMA_RETURNe; /* nothing to be done */
   }
   SUMA_SwitchColPlaneBrightness(ado, curColPlane, imenu -1, 0);
   
   #if 0 /* Obsolete, now all handled in SUMA_SwitchColPlaneBrightness above*/
   if (LocalHead) {
      fprintf( SUMA_STDERR, 
               "%s:\n request to switch brightness to col. %d\n", 
               FuncName, imenu - 1);
   }
   
   curColPlane->OptScl->bind = imenu - 1;

   SUMA_InitRangeTable(ado, 1) ;

   SUMA_UpdateNodeValField(ado);
   if (!curColPlane->OptScl->UseBrt) { 
      SUMA_RETURNe; 
   } /* nothing else to do */
   
   if (!SUMA_ColorizePlane (curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   

   SUMA_Remixedisplay(ado);
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   
   SUMA_UpdateNodeLblField(ado);
   #endif
   
   SUMA_RETURNe;
}

int SUMA_SwitchCmap_one(SUMA_ALL_DO *ado,
                         SUMA_COLOR_MAP *CM, int setmenu)
{
   static char FuncName[]={"SUMA_SwitchCmap_one"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !CM) SUMA_RETURN(0);
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n request to switch colormap to  (%s)\n", 
         FuncName, CM->Name);
   }
   
   if (setmenu) {
      if (!SUMA_SetCmapMenuChoice (ado, CM->Name)) {
             SUMA_SL_Err("Failed in SUMA_SetCmapMenuChoice");
      }
   }  

   if (!SUMA_SwitchColPlaneCmap(ado, CM)) {
      SUMA_SL_Err("Failed in SUMA_SwitchColPlaneCmap");
   }
   
   /* Now you'll need to close the list widget if a choice has been made */
   if (SUMAg_CF->X->SwitchCmapLst) {
      if (!SUMAg_CF->X->SwitchCmapLst->isShaded) 
         SUMA_cb_CloseSwitchCmap( NULL,  (XtPointer)SUMAg_CF->X->SwitchCmapLst,  
                                  NULL);
   }
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(ado);

   if (SUMAg_CF->Fake_Cmap) {
      SUMA_LH("Attempting to keep up appearances");
      SUMA_PBAR_bigexpose_CB(NULL, (XtPointer)ado, NULL);
   }
   
   SUMA_RETURN(1);
}

int SUMA_SwitchCmap(SUMA_ALL_DO *ado,
                    SUMA_COLOR_MAP *CM, int setmenu)
{
   static char FuncName[]={"SUMA_SwitchCmap"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   if (!ado || !CM) SUMA_RETURN(0);
   
   if (!SUMA_SwitchCmap_one(ado, CM, setmenu)) SUMA_RETURN(0);
   
   if (ado->do_type == SO_type) {
      SUMA_SurfaceObject *SOC=NULL, *SO=(SUMA_SurfaceObject *)ado;
      SUMA_OVERLAYS *colp=NULL, *colpC=NULL;                     
      /* do we have a contralateral SO and overlay? */
      colp = SO->SurfCont->curColPlane;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (!SUMA_SwitchCmap_one((SUMA_ALL_DO *)SOC, CM, 1)) {
            SUMA_S_Warn("Failed in contralateralination");
         }
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
   SUMA_ALL_DO *ado = NULL;
   SUMA_COLOR_MAP *CM = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   CM = (SUMA_COLOR_MAP *)datap->callback_data; 
   
   SUMA_SwitchCmap(ado, CM, 0);
   
   SUMA_RETURNe;
}

void SUMA_cb_ShowZero_tb_toggled (Widget w, XtPointer data, 
                                  XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ShowZero_tb_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (  !curColPlane || 
         !curColPlane->OptScl )  { 
      SUMA_S_Warn("NULL input 2"); SUMA_RETURNe; 
   }
   
   curColPlane->OptScl->MaskZero = 
      !curColPlane->OptScl->MaskZero;
   
   if (!curColPlane->ShowMode < 0) { 
      /* nothing else to do */ 
      SUMA_RETURNe;
   } 
   
   SUMA_ADO_Flush_Pick_Buffer(ado, NULL);
   
   if (!SUMA_ColorizePlane (curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_Remixedisplay(ado);
 
   SUMA_UpdateNodeLblField(ado);
   
   SUMA_RETURNe;
}
 

void SUMA_cb_SymIrange_tb_toggled (Widget w, XtPointer data, 
                                   XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SymIrange_tb_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if ( !curColPlane )  { 
      SUMA_S_Warn("NULL input 2"); SUMA_RETURNe; 
   }
   
   curColPlane->SymIrange = !curColPlane->SymIrange;
   
   if (curColPlane->SymIrange) {
      /* manual setting of range. 
         DO NOT Call SUMA_InitRangeTable because it will 
         automatically update the I range under certain conditions*/
      TF = SurfCont->SetRangeTable;
      curColPlane->OptScl->IntRange[1] = 
         SUMA_LARG_ABS(curColPlane->OptScl->IntRange[0], 
         curColPlane->OptScl->IntRange[1]);
      curColPlane->OptScl->IntRange[0] = 
         -curColPlane->OptScl->IntRange[1];
      SUMA_INSERT_CELL_VALUE(TF, 1, 1, 
                  curColPlane->OptScl->IntRange[0]);
      SUMA_INSERT_CELL_VALUE(TF, 1, 2, 
                  curColPlane->OptScl->IntRange[1]);
   }
   
   if (!curColPlane->ShowMode < 0) { SUMA_RETURNe; } 
      /* nothing else to do */
   
   
   if (!SUMA_ColorizePlane (curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_Remixedisplay(ado);
 
   SUMA_UpdateNodeValField(ado);
   SUMA_UpdateNodeLblField(ado);
   

   SUMA_RETURNe;
}

void SUMA_cb_AbsThresh_tb_toggled (Widget w, XtPointer data, 
                                   XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_AbsThresh_tb_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   char slabel[100];
   double range[2]; int loc[2];  
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if ( !curColPlane )  { 
      SUMA_S_Warn("NULL input 2"); SUMA_RETURNe; 
   }

   if (curColPlane->OptScl->ThrMode == SUMA_LESS_THAN) {
      curColPlane->OptScl->ThrMode = SUMA_ABS_LESS_THAN;
   } else if (curColPlane->OptScl->ThrMode == SUMA_ABS_LESS_THAN){
      curColPlane->OptScl->ThrMode = SUMA_LESS_THAN;
   } else if (curColPlane->OptScl->ThrMode == SUMA_THRESH_OUTSIDE_RANGE){
      curColPlane->OptScl->ThrMode = SUMA_THRESH_INSIDE_RANGE;
   } else if (curColPlane->OptScl->ThrMode == SUMA_THRESH_INSIDE_RANGE){
      curColPlane->OptScl->ThrMode = SUMA_THRESH_OUTSIDE_RANGE;
   } else {
      SUMA_S_Err("Not ready for this situation %d...", 
                 curColPlane->OptScl->ThrMode);
   }
   switch (curColPlane->OptScl->ThrMode) {
      case SUMA_LESS_THAN: 
         sprintf(slabel, "%5s", 
            MV_format_fval(curColPlane->OptScl->ThreshRange[0]));
         break;
      case SUMA_ABS_LESS_THAN:
         /* used to use this:
         sprintf(slabel, "|%5s|", .... 
         but that does not work in the editable field ... */
         sprintf(slabel, "%5s", 
               MV_format_fval(fabs(curColPlane->OptScl->ThreshRange[0])));
         break;
      case SUMA_THRESH_INSIDE_RANGE:
         /* This is just a place holder for now */
         sprintf(slabel, "<%5s..%5s>", 
                       MV_format_fval(curColPlane->OptScl->ThreshRange[0]), 
                       MV_format_fval(curColPlane->OptScl->ThreshRange[1]));
         break;
      case SUMA_THRESH_OUTSIDE_RANGE:
         /* This is just a place holder for now */
         sprintf(slabel, ">%5s..%5s<", 
                       MV_format_fval(curColPlane->OptScl->ThreshRange[0]), 
                       MV_format_fval(curColPlane->OptScl->ThreshRange[1]));
         break;
      case SUMA_NO_THRESH:
         break;
      default:
         /* This is just a place holder for now */
         sprintf(slabel, "?%5s??%5s?<", 
                       MV_format_fval(curColPlane->OptScl->ThreshRange[0]), 
                       MV_format_fval(curColPlane->OptScl->ThreshRange[1]));
         break;
   }
   
   /* SUMA_SET_LABEL(SurfCont->thr_lb,  slabel); */
   SUMA_INSERT_CELL_STRING(SurfCont->SetThrScaleTable, 0,0,slabel); 
   if (SUMA_GetDsetColRange(curColPlane->dset_link, 
                     curColPlane->OptScl->tind, range, loc)) {   
      SUMA_SetScaleRange(ado, range );
   }else {
      SUMA_SLP_Err("Failed to get range");
      SUMA_RETURNe;
   }
      
   if (!curColPlane->OptScl->UseThr) { SUMA_RETURNe; } 
                                                /* nothing else to do */

   SUMA_ADO_Flush_Pick_Buffer(ado, NULL);
   
   if (!SUMA_ColorizePlane (curColPlane)) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURNe;
   }
   
   SUMA_Remixedisplay(ado);

   SUMA_UpdateNodeLblField(ado);

   SUMA_RETURNe;
}

void SUMA_cb_SwitchInt_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SwitchInt_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if ( !curColPlane )  { 
      SUMA_S_Warn("NULL input 2"); SUMA_RETURNe; 
   }

   /* make sure ok to turn on */
   if (curColPlane->OptScl->find < 0) {
      SUMA_BEEP;
      SUMA_SLP_Note("no intensity column set");
      XmToggleButtonSetState (SurfCont->Int_tb, NOPE, NOPE);
      SUMA_RETURNe;
   }
      
   /* this button's the same as the Show button */
   if (XmToggleButtonGetState (SurfCont->Int_tb)) { 
      curColPlane->ShowMode = 
         SUMA_ABS(curColPlane->ShowMode);
   } else {
      curColPlane->ShowMode = 
         -SUMA_ABS(curColPlane->ShowMode);
   }
   if (SurfCont->DsetViewModeMenu) {
      SUMA_Set_Menu_Widget(SurfCont->DsetViewModeMenu,
                           SUMA_ShowMode2ShowModeMenuItem(
                                                curColPlane->ShowMode));
   }
      
   SUMA_ColorizePlane(curColPlane);
   SUMA_Remixedisplay(ado);
   SUMA_UpdateNodeLblField(ado);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   SUMA_RETURNe;
}

void SUMA_cb_SwitchThr_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SwitchThr_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");

   ado = (SUMA_ALL_DO *)data;
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if ( !curColPlane )  { 
      SUMA_S_Warn("NULL input 2"); SUMA_RETURNe; 
   }


   /* make sure ok to turn on */
   if (curColPlane->OptScl->tind < 0) {
      SUMA_BEEP;
      SUMA_SLP_Note("no threshold column set");
      XmToggleButtonSetState (SurfCont->Thr_tb, NOPE, NOPE);
      SUMA_RETURNe;
   }
      
   curColPlane->OptScl->UseThr = 
         XmToggleButtonGetState (SurfCont->Thr_tb);
      
   SUMA_ColorizePlane(curColPlane);
   SUMA_Remixedisplay(ado);
   
   SUMA_UpdateNodeLblField(ado);
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   SUMA_RETURNe;
}

void SUMA_cb_SwitchBrt_toggled (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_SwitchBrt_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if ( !curColPlane )  { 
      SUMA_S_Warn("NULL input 2"); SUMA_RETURNe; 
   }
   
   /* make sure ok to turn on */
   if (curColPlane->OptScl->bind < 0) {
      SUMA_BEEP;
      SUMA_SLP_Note("no brightness column set");
      XmToggleButtonSetState (SurfCont->Brt_tb, NOPE, NOPE);
      SUMA_RETURNe;
   }
   
   curColPlane->OptScl->UseBrt = 
                     XmToggleButtonGetState (SurfCont->Brt_tb);
   
   SUMA_ColorizePlane(curColPlane);
   SUMA_Remixedisplay(ado);
   SUMA_UpdateNodeLblField(ado);

   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
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
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   
   SUMA_SetCmapMode(ado, imenu);
      
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_SetCmapMode(SUMA_ALL_DO *ado, int imenu)
{
   static char FuncName[]={"SUMA_SetCmapMode"};
   SUMA_Boolean NewDisp = NOPE;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado)) || 
       !(curColPlane = SUMA_ADO_CurColPlane(ado)) || 
       imenu < 1) SUMA_RETURN(NOPE);
   
   /* get the surface object that the setting belongs to */
   NewDisp = NOPE;
   switch (imenu) {
      case SW_Interp:
         if (curColPlane->OptScl->interpmode != SUMA_INTERP) {
            curColPlane->OptScl->interpmode = SUMA_INTERP;
            NewDisp = YUP;
         }
         break;
      case SW_NN:
         if (curColPlane->OptScl->interpmode != SUMA_NO_INTERP) {
            curColPlane->OptScl->interpmode = SUMA_NO_INTERP;
            NewDisp = YUP;
         }
         break;
      case SW_Direct:
         if (curColPlane->OptScl->interpmode != SUMA_DIRECT) {
            curColPlane->OptScl->interpmode = SUMA_DIRECT;
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
      SUMA_ColorizePlane(curColPlane);
      SUMA_Remixedisplay(ado);
   }
   
   SUMA_UpdateNodeNodeField(ado);
   SUMA_UpdateNodeLblField(ado);
   
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
   SUMA_ALL_DO *ado = NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean NewDisp = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   curColPlane = SUMA_ADO_CurColPlane(ado);
   imenu = (INT_CAST)datap->callback_data; 
   NewDisp = NOPE;
   switch (imenu) { /* There is really no need for this 
                        switching block... */
      case SW_LinkMode_None:
         if (curColPlane->LinkMode != imenu) {
             curColPlane->LinkMode = imenu;
            NewDisp = YUP;
         }
         break;
      case SW_LinkMode_Pls1:
         if (curColPlane->LinkMode != imenu) {
             curColPlane->LinkMode = imenu;
            NewDisp = YUP;
         }
         break;
      case SW_LinkMode_Same:
         if (curColPlane->LinkMode != imenu) {
             curColPlane->LinkMode = imenu;
            NewDisp = YUP;
         }
         break;
      case SW_LinkMode_Stat:
         if (curColPlane->LinkMode != imenu) {
             curColPlane->LinkMode = imenu;
            NewDisp = YUP;
         }
         break;
      default: 
         fprintf (SUMA_STDERR, "Error %s: Unexpected widget index %d.\n", 
                                 FuncName, imenu);
         break;
   }
   SUMA_LHv("LinkMode now %d\n", curColPlane->LinkMode);
   /* redisplay all viewers showing SO*/
   if (NewDisp) {
      SUMA_ColorizePlane(curColPlane);
      SUMA_Remixedisplay(ado);
   }
   
   SUMA_UpdateNodeNodeField(ado);
   SUMA_UpdateNodeLblField(ado);
   
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
   SUMA_ALL_DO *ado = NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean NewDisp = NOPE;
   SUMA_VIS_XFORM_DATUM *x0=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   /* get the surface object that the setting belongs to */
   datap = (SUMA_MenuCallBackData *)client_data;
   ado = (SUMA_ALL_DO *)datap->ContID;
   imenu = (INT_CAST)datap->callback_data; 
   curColPlane = SUMA_ADO_CurColPlane(ado);
   SurfCont = SUMA_ADO_Cont(ado);
   
   /* if CoordBias is to be added, it should be before Prying */
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         x0 = SUMA_Fetch_VisX_Datum ("CoordBias", SO->VisX.Xchain, 
                                              ADD_BEFORE, "Prying");
         break; }
      case GDSET_type: {
         SUMA_S_Warn("Not sure what to do here");
         break; }
      case CDOM_type: {
         SUMA_S_Warn("Not sure what to do here");
         break; }
      default:
         SUMA_S_Errv("Not ready for type %s\n",
            SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         break; 
   }
   
   NewDisp = NOPE;
   switch (imenu) {
      case SW_CoordBias_None:
         if (curColPlane->OptScl->DoBias != SW_CoordBias_None) {
            if (curColPlane->OptScl->BiasVect) {
               SUMA_RemoveCoordBias(curColPlane);
            }
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_X:
         if (curColPlane->OptScl->DoBias != SW_CoordBias_X) { 
               /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to 
                  new dimension*/
               SUMA_TransferCoordBias(curColPlane, SW_CoordBias_X);
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_Y:
         if (curColPlane->OptScl->DoBias != SW_CoordBias_Y) { 
               /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to 
               new dimension*/
               SUMA_TransferCoordBias(curColPlane, SW_CoordBias_Y);
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_Z:
         if (curColPlane->OptScl->DoBias != SW_CoordBias_Z) { 
               /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to 
                  new dimension*/
               SUMA_TransferCoordBias(curColPlane, SW_CoordBias_Z);
            NewDisp = YUP;
         }
         break;
      case SW_CoordBias_N:
         if (curColPlane->OptScl->DoBias != SW_CoordBias_N) { 
               /* something needs to be done */
               /* bias other than on other dimension exists, transfer it to 
                  new dimension*/
               SUMA_TransferCoordBias(curColPlane, SW_CoordBias_N);
            NewDisp = YUP;
         }
         break;
      default: 
         fprintf (SUMA_STDERR, "Error %s: Unexpected widget index.\n", FuncName);
         break;
   }
   
   /* redisplay all viewers showing DO*/
   if (NewDisp) {
      SUMA_ColorizePlane(curColPlane);
      SUMA_Remixedisplay(ado);
   }
   
   SUMA_UpdateNodeNodeField(ado);
   
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   SUMA_RETURNe;
}

/*!
   \brief Function to call a redisplay of all viewers showing SO 
*/
SUMA_Boolean SUMA_RedisplayAllShowing(char *SO_idcode_str, 
                                      SUMA_SurfaceViewer *SVv, int N_SVv)
{
   static char FuncName[]={"SUMA_RedisplayAllShowing"};
   SUMA_SurfaceViewer *sv;
   void *pp=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_DO_Types tp;
   SUMA_SurfaceObject *SO1 = NULL, *SO2 = NULL;
   SUMA_DSET *dset=NULL;
   int i, k;   
   DList *list=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SVv) {
      SVv = SUMAg_SVv;
      N_SVv = SUMAg_N_SVv;
   }
   if (!SO_idcode_str || !SVv) {
      SUMA_S_Err("NULL SVv or SO_idcode_str. BAD");
      SUMA_RETURN (NOPE);
   }
   if (!(pp = SUMA_find_any_object(SO_idcode_str, &tp))) {
      SUMA_S_Errv("Failed to find object with idcode %s.\n", SO_idcode_str);
      SUMA_RETURN(NOPE);
   }
   ado = (SUMA_ALL_DO*)pp;
   switch (tp) {
      case SO_type:
         SO1 = (SUMA_SurfaceObject *)pp;
         /* search all viewers */
         for (i=0; i < N_SVv; ++i) {
            SUMA_LHv("Searching viewer %d.\n", i);
            sv = &(SVv[i]);
            /* search for SO in RegisteredDO */
            for (k=0; k < sv->N_DO; ++k) {
               if (SUMA_isSO(SUMAg_DOv[sv->RegistDO[k].dov_ind])) {
                  SO2 = (SUMA_SurfaceObject *)
                                 SUMAg_DOv[sv->RegistDO[k].dov_ind].OP;
                  if (SUMA_WhatAreYouToMe(SO1, SO2) == SUMA_SO1_is_SO2) { 
                     /* Get a redisplay for that puppy */
                     if (!list) list = SUMA_CreateList ();
                     SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay,
                                                  SES_SumaWidget, sv);
                  }
               }  
            } 
         }       
         break;
      case MASK_type:
      case GDSET_type:
      case CDOM_type:
      case VO_type:
      case TRACT_type:
      case GRAPH_LINK_type:
         for (i=0; i < N_SVv; ++i) {
            sv = &(SVv[i]);
            if (SUMA_ADO_isRegistered(sv, ado)) {
               if (!list) list = SUMA_CreateList ();
                        SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay,
                                                     SES_SumaWidget, sv);
            }
         }
         break;
      default:
         SUMA_S_Errv("Type %d (%s) is not welcome here\n", 
                     tp, SUMA_ObjectTypeCode2ObjectTypeName(tp));
         SUMA_RETURN(NOPE); 
   }   
      
   if (!SUMA_Engine(&list)) {
      SUMA_SLP_Err("Failed to redisplay.");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(YUP);
}

SUMA_TABLE_FIELD * SUMA_AllocTableField(char *wname)
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
   TF->rowobject_id = NULL;
   if (wname) snprintf(TF->wname,63,"%s", wname);
   else snprintf(TF->wname,63,"UNNAMED");
   SUMA_RETURN(TF);
}

SUMA_SLICE_FIELD * SUMA_AllocSliceField(char *wname)
{
   static char FuncName[]={"SUMA_AllocSliceField"};
   SUMA_SLICE_FIELD *SF = NULL;

   SUMA_ENTRY;
   SF = (SUMA_SLICE_FIELD *)SUMA_calloc(1,sizeof(SUMA_SLICE_FIELD));
   if (!SF) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(SF);
   }
   SF->Nslc = -1;
   SF->sl = NULL;
   SF->tb = NULL;
   SF->text = NULL;
   SF->mont = NULL;
   SF->NewValueCallback = NULL;
   SF->NewValueCallbackData = NULL;
   SF->slice_num_str = NULL;
   SF->mont_str = NULL;
   SF->slice_units = SUMA_NO_NUM_UNITS;
   SF->mont_units = SUMA_NO_NUM_UNITS;
   SF->slice_num = 0;
   SF->mont_num = 1;
   SF->mont_inc = 10;
   SF->variant = NULL;
   if (wname) snprintf(SF->wname,63,"%s", wname);
   else snprintf(SF->wname,63,"UNNAMED");
   SUMA_RETURN(SF);
}

SUMA_SLICE_FIELD * SUMA_FreeSliceField(SUMA_SLICE_FIELD *SF)
{
   static char FuncName[]={"SUMA_FreeSliceField"};
   int i;
   
   SUMA_ENTRY;

   if (!SF) SUMA_RETURN(NULL);

   if (SF->slice_num_str) SUMA_free(SF->slice_num_str);
   if (SF->mont_str) SUMA_free(SF->mont_str);
   if (SF->variant) SUMA_free(SF->variant);
   SUMA_free(SF);

   SUMA_RETURN(NULL);

}

SUMA_VR_FIELD * SUMA_AllocVRField(char *wname)
{
   static char FuncName[]={"SUMA_AllocVRField"};
   SUMA_VR_FIELD *VrF = NULL;

   SUMA_ENTRY;

   VrF = (SUMA_VR_FIELD *)SUMA_calloc(1,sizeof(SUMA_VR_FIELD));
   if (!VrF) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(VrF);
   }
   VrF->Nslc = -1;
   VrF->tb = NULL;
   VrF->text = NULL;
   VrF->NewValueCallback = NULL;
   VrF->NewValueCallbackData = NULL;
   VrF->N_slice_num_str = NULL;
   VrF->N_slice_units = SUMA_NO_NUM_UNITS;
   VrF->N_slice_num = -1;
   if (wname) snprintf(VrF->wname,63,"%s", wname);
   else snprintf(VrF->wname,63,"UNNAMED");
   SUMA_RETURN(VrF);
}

SUMA_VR_FIELD * SUMA_FreeVRField(SUMA_VR_FIELD *VrF)
{
   static char FuncName[]={"SUMA_FreeVRField"};
   int i;
   
   SUMA_ENTRY;

   if (!VrF) SUMA_RETURN(NULL);

   if (VrF->N_slice_num_str) SUMA_free(VrF->N_slice_num_str);
   SUMA_free(VrF);

   SUMA_RETURN(NULL);

}

/*!
   Called when user clicks on range table cell
   Expect SO in cd
*/
void SUMA_RangeTableCell_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_RangeTableCell_EV"};
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)cd, *curDO=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  i, j, n, Found;
   void *cv=NULL;
   SUMA_TABLE_FIELD *TF = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   curDO = SUMA_Cont_ADO(SurfCont);
   TF = SurfCont->RangeTable;
   
   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ) {
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || TF == NULL || ado == NULL ) { SUMA_RETURNe ; }

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
               /* is this viewer showing curDO ? */
               if (SUMA_isVisibleDO(&(SUMAg_SVv[i]), SUMAg_DOv,
                                    curDO)) {
                  if (SUMA_SV_Focus_ADO(&(SUMAg_SVv[i])) == curDO) {
                        SUMA_JumpIndex((char *)cv, (void *)(&(SUMAg_SVv[i])));
                  } else {
                     SUMA_LHv("Seeking DO, %p, %s\n", 
                              curDO,
                              SUMA_CHECK_NULL_STR(SUMA_ADO_idcode(curDO)));
                     SUMAg_SVv[i].Focus_DO_ID = 
                        SUMA_whichDO(SUMA_ADO_idcode(curDO), 
                                     SUMAg_DOv, SUMAg_N_DOv );
                     if (SUMA_isADO_Cont_Realized(curDO)) {   
                        SUMA_Init_SurfCont_SurfParam(curDO);
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
   SUMA_ALL_DO *ado = srvd->ado;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
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
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (!ado || !SurfCont || !curColPlane) {
      SUMA_SL_Err("No curColPlane!");
      SUMA_RETURNe;
   }
   
   /* Now do something */
   if (j == 0) { /* clicked on one of the row's titles */
      switch (i) {
         case 1:
            if (bev->button == Button1) { /* toggle lock */
               curColPlane->OptScl->AutoIntRange = 
                           !curColPlane->OptScl->AutoIntRange;
               SurfCont->IntRangeLocked = !SurfCont->IntRangeLocked;
               MCW_invert_widget(w);
            }else if (bev->button == Button3) { /* reset to autorange values */
               AutoHist = curColPlane->OptScl->AutoIntRange; 
               curColPlane->OptScl->AutoIntRange = 1;
               SUMA_InitRangeTable(ado, 0); /* overkill but little overhead */
               SUMA_ColorizePlane(curColPlane);
               SUMA_Remixedisplay(ado);
               curColPlane->OptScl->AutoIntRange = AutoHist; 
            }
            break;
         case 2:
            if (bev->button == Button1) { /* toggle lock */
               curColPlane->OptScl->AutoBrtRange = 
                           !curColPlane->OptScl->AutoBrtRange;
               SurfCont->BrtRangeLocked = !SurfCont->BrtRangeLocked;
               MCW_invert_widget(w);   
            }else if (bev->button == Button3) { /* reset to autorange values */
               AutoHist = curColPlane->OptScl->AutoBrtRange; 
               curColPlane->OptScl->AutoBrtRange = 1;
               SUMA_InitRangeTable(ado, 1); /* overkill but little overhead */
               SUMA_ColorizePlane(curColPlane);
               SUMA_Remixedisplay(ado);
               curColPlane->OptScl->AutoBrtRange = AutoHist; 
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
   if (curColPlane->OptScl->DoBias != SW_CoordBias_None) {
      SUMA_UpdateNodeNodeField(ado);    
   }
   SUMA_UpdateNodeLblField(ado);

   SUMA_RETURNe;

}

SUMA_Boolean SUMA_SetClustTableTit_one (SUMA_ALL_DO *ado, 
                        SUMA_OVERLAYS *colp, int i, int j, int Button) 
{
   static char FuncName[]={"SUMA_SetClustTableTit_one"};
   SUMA_TABLE_FIELD *TF = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");

   if (!ado) SUMA_RETURN(0);
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);

   if (colp && colp != curColPlane) SUMA_RETURN(0);
   colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   if (!(TF = SurfCont->SetClustTable)) SUMA_RETURN(0);
   
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
               SUMA_Remixedisplay((SUMA_ALL_DO*)ado);
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
   SUMA_UpdateNodeLblField(ado);

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

SUMA_Boolean SUMA_SetClustTableTit (SUMA_ALL_DO *ado, 
                        SUMA_OVERLAYS *colp, int i, int j, int Button) 
{
   static char FuncName[]={"SUMA_SetClustTableTit"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!ado) SUMA_RETURN(0);
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (colp && colp != curColPlane) SUMA_RETURN(0);
   colp = curColPlane;
   if (!colp) SUMA_RETURN(0);
   
   if (!SUMA_SetClustTableTit_one (ado, colp, i, j, Button)) SUMA_RETURN(0);
   
   if (ado->do_type == SO_type) {   
      SUMA_SurfaceObject *SOC=NULL, *SO = (SUMA_SurfaceObject *)ado;
      SUMA_OVERLAYS *colpC=NULL;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (!SUMA_SetClustTableTit_one ((SUMA_ALL_DO *)SOC, 
                                          colpC, i, j, Button)) SUMA_RETURN(0);
      }
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
   SUMA_ALL_DO *ado = srvd->ado;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);

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
   if (!curColPlane) {
      SUMA_SL_Err("No curColPlane!");
      SUMA_RETURNe;
   }


   if (!SUMA_SetClustTableTit(ado, curColPlane, i, j, bev->button)){
      SUMA_S_Err("Failed, weird");
   }
   
   SUMA_RETURNe;

}

void SUMA_SetClustTableCell_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_SetClustTableCell_EV"};
   Dimension lw ;
   Widget * children , wl = NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  num_children , i, j, Found, incr=0, an, n;
   float reset, newv;
   SUMA_TABLE_FIELD *TF = (SUMA_TABLE_FIELD *)cd;
   SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)TF->NewValueCallbackData;
   SUMA_ALL_DO *ado = srvd->ado;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   DList *list = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called Button %d", bev->button);
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);

   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ){
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || TF == NULL ) SUMA_RETURNe ;

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
         break;
      case 1:/* radius */
      case 2: /* area */
         if (incr) {
                 if (TF->num_value[n]>1000) incr = incr*100;
            else if (TF->num_value[n]>100) incr = incr*10;
            else if (TF->num_value[n]>50) incr = incr*5;
            newv = TF->num_value[n]+incr;
            SUMA_MODIFY_CELL_VALUE(TF, i, j, newv);
            an = SUMA_SetClustValue(ado, curColPlane, i, j,
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
                  SUMA_RETURNe;
               } else {
                  SUMA_S_Err("Erriositation");
                  SUMA_RETURNe;
               }
            }
            /* redisplay */
            if (!list) list = SUMA_CreateList ();
            SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                               SES_Suma, NULL); 
            if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");   
         }
         break;
      default:
         SUMA_SL_Err("Did not know you had so many");
         break;
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
   if (TF->rowobject_id) { 
      for (i=0; i<TF->Ni; ++i) 
         if (TF->rowobject_id[i]) SUMA_free(TF->rowobject_id[i]); 
      SUMA_free(TF->rowobject_id);
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
                        char *iwname,
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
   char *tmp, wname[64]={"NeedToSetMe"};
   Widget rcc;
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
   if (wname) { /* override useless default */
      snprintf(TF->wname,63,"%s", iwname);
   }
   TF->Ni = Ni; TF->Nj = Nj; TF->editable = editable; 
   TF->cwidth = (int *)SUMA_calloc(TF->Nj, sizeof(int));
   TF->rowobject_id = (char **)SUMA_calloc(TF->Ni, sizeof(char *));
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
   TF->rco = XtVaCreateManagedWidget ("rowcolumn",
      xmRowColumnWidgetClass, TF->rc,
      XmNorientation , XmVERTICAL ,
      XmNpacking, XmPACK_TIGHT,
      XmNnumColumns, 1,
      XmNmarginHeight, 0,
      XmNmarginWidth, 0, 
      NULL);

   /* must fill up row by row, each column is a separate bloody rc 
      to allow for alignments */
   for (i=0; i<TF->Ni; ++i) {   /* for each row */
      rcc = XtVaCreateManagedWidget ("rowcolumn",
         xmRowColumnWidgetClass, TF->rco,
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
      
      /* Create what would become a table URI in the html help */
      snprintf(wname, 63, "%s", TF->wname);
      SUMA_Register_Widget_Help(NULL, 2, wname, NULL, NULL);
      
      for (j=0; j<TF->Nj; ++j) { /* for each column */
         n = j * TF->Ni + i;
         switch (SUMA_cellvariety(TF, n)) {
            case SUMA_ROW_TIT_CELL: /* row's title */
               if (LocalHead) 
                  fprintf( SUMA_STDERR,
                           "%s:\nAdding RT [%d %d] (%d) %s\n", 
                           FuncName, i, j, n, row_tit[i] );
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
               if (0 && TF->HasRowTit &&  row_tit[i]) { /* Much better name, but 
                    Cannot use it before updating
                    help generating functions which now call for help on .c00, 
                    .c01, or .r00 .r01, etc. */
                  snprintf(wname, 63, "%s.%s", TF->wname, row_tit[i]);
               } else { 
                  snprintf(wname, 63, "%s.r%02d", TF->wname, i);
               }
               SUMA_Register_Widget_Help(TF->cells[n], 1, wname, 
                                         row_hint?row_hint[i]:NULL, 
                                         row_help?row_help[i]:NULL ) ;
               break;
               
            case SUMA_COL_TIT_CELL: /* column's title */
               if (LocalHead) 
                  fprintf( SUMA_STDERR,
                           "%s:\nAdding CT [%d %d] (%d) %s\n", 
                           FuncName, i, j, n, col_tit[j]);
               /* padd to fit cell entry fields*/
               if (i == 0 && j != 0 && TF->HasColTit) { 
                  titw = TF->cwidth[j]; 
                  /* set the margins to meet those of cell entries */
                  xmw = 6;
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
               if (0 && TF->HasColTit &&  col_tit[j]) {/* Much better name, but 
                    Cannot use it before updating
                    help generating functions which now call for help on .c00, 
                    .c01, or .r00 .r01, etc. */
                  snprintf(wname, 63, "%s.%s", TF->wname, col_tit[j]);
               } else {
                  snprintf(wname, 63, "%s.c%02d", TF->wname, j);
               }
               SUMA_Register_Widget_Help(TF->cells[n], 1, wname,
                                         col_hint?col_hint[j]:NULL, 
                                         col_help?col_help[j]:NULL ) ;  
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
                  if (TF->Ni>1) {
                     snprintf(wname, 63, "%s[%d,%d]", TF->wname, i, j);
                  } else {
                     snprintf(wname, 63, "%s[%d]", TF->wname, n);
                  }
                  if (!row_tit && !col_tit && TF->Ni == 1 && TF->Nj == 1) {
                     char *shh=NULL, *sii=NULL;
                     if (col_help) shh =  col_help[0] ;
                     else if (row_help) shh =  row_help[0] ;
                     if (col_hint) sii = col_hint[0] ;
                     else if (row_hint) sii =  row_hint[0] ;
                     if (shh || sii) {
                        SUMA_Register_Widget_Help(TF->cells[n],1,wname,sii, shh);
                     }
                  } else {
                     SUMA_Register_Widget_Help(TF->cells[n], 1, wname, 
                                                      NULL,
                                    "Use BHelp on table's column and row titles"
                                    "for usage information.") ;
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
      } /* for j */
   } /* for i */
   
   SUMA_RETURNe;
}

int SUMA_set_slice_label(SUMA_ALL_DO *ado, char *variant, float val)
{
   static char FuncName[]={"SUMA_set_slice_label"};
   char slabel[100];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   
   SurfCont = SUMA_ADO_Cont(ado);
   if (!ado || !variant || !SurfCont) { 
      SUMA_SL_Err("NULL input"); SUMA_RETURN(0); }
   
   if (!SurfCont->Ax_slc || !SurfCont->Ax_slc->text ||
       !SurfCont->Sa_slc || !SurfCont->Sa_slc->text ||
       !SurfCont->Co_slc || !SurfCont->Co_slc->text) {
      SUMA_LH("No GUI yet"); SUMA_RETURN(1);
   }

   sprintf(slabel, "%3s", MV_format_fval(val)); 
          if (variant[0] == 'A') {
      SUMA_STRING_REPLACE(SurfCont->Ax_slc->slice_num_str, slabel);
      XtVaSetValues (SurfCont->Ax_slc->text, XmNvalue, slabel, NULL);
   } else if (variant[0] == 'S') {
      SUMA_STRING_REPLACE(SurfCont->Sa_slc->slice_num_str, slabel);
      XtVaSetValues (SurfCont->Sa_slc->text, XmNvalue, slabel, NULL);
   } else if (variant[0] == 'C') {
      SUMA_STRING_REPLACE(SurfCont->Co_slc->slice_num_str, slabel);
      XtVaSetValues (SurfCont->Co_slc->text, XmNvalue, slabel, NULL);
   }

   SUMA_RETURN(1);  
}

int SUMA_set_slice_scale(SUMA_ALL_DO *ado, char *variant, float val)
{
   static char FuncName[]={"SUMA_set_slice_scale"};
   char slabel[100];
   int cv;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   
   SurfCont = SUMA_ADO_Cont(ado);
   if (!ado || !variant || !SurfCont) { 
      SUMA_SL_Err("NULL input"); SUMA_RETURN(0); }
   
   if (!SurfCont->Ax_slc || !SurfCont->Ax_slc->sl ||
       !SurfCont->Sa_slc || !SurfCont->Sa_slc->sl ||
       !SurfCont->Co_slc || !SurfCont->Co_slc->sl) {
      SUMA_LH("No GUI yet"); SUMA_RETURN(1);
   }
   
   cv = SUMA_SliceVal2ScalePos (ado, variant, &val );

          if (variant[0] == 'A') {
      XtVaSetValues(SurfCont->Ax_slc->sl,  XmNvalue, cv, NULL);   
   } else if (variant[0] == 'S') {
      XtVaSetValues(SurfCont->Sa_slc->sl,  XmNvalue, cv, NULL);   
   } else if (variant[0] == 'C') {
      XtVaSetValues(SurfCont->Co_slc->sl,  XmNvalue, cv, NULL);   
   }

   SUMA_RETURN(1);  
}

void SUMA_cb_set_Ax_slice_label(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_Ax_slice_label"};
   SUMA_ALL_DO *ado=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff ;
   int dec=0;
   char slabel[100];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }
   
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   
   SUMA_set_slice(ado, "Ax", &fff, "text_field", 1);
   
   SUMA_RETURNe;
}

void SUMA_cb_set_Sa_slice_label(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_Sa_slice_label"};
   SUMA_ALL_DO *ado=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff ;
   int dec=0;
   char slabel[100];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }
   
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   
   SUMA_set_slice(ado, "Sa", &fff, "text_field", 1);
   
   SUMA_RETURNe;
}

void SUMA_cb_set_Co_slice_label(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_Co_slice_label"};
   SUMA_ALL_DO *ado=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff ;
   int dec=0;
   char slabel[100];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("called");
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }
   
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   
   SUMA_set_slice(ado, "Co", &fff, "text_field", 1);
   
   SUMA_RETURNe;
}

void SUMA_cb_set_Ax_slice(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_Ax_slice"};
   SUMA_ALL_DO *ado=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff=0.0;
   int dec=-1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;  
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   SUMA_LHv("Have %f\n", fff);
   SUMA_set_slice(ado, "Ax", &fff, "scale", 1);
   
   SUMA_RETURNe;
}

void SUMA_cb_set_Sa_slice(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_Sa_slice"};
   SUMA_ALL_DO *ado=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff=0.0;
   int dec=-1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;  
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   SUMA_LHv("Have %f\n", fff);
   SUMA_set_slice(ado, "Sa", &fff, "scale", 1);
   
   SUMA_RETURNe;
}

void SUMA_cb_set_Co_slice(Widget w, XtPointer clientData, XtPointer call)
{
   static char FuncName[]={"SUMA_cb_set_Co_slice"};
   SUMA_ALL_DO *ado=NULL;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call ;
   float fff=0.0;
   int dec=-1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;  
   ado = (SUMA_ALL_DO *)clientData;
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURNe; }
   XtVaGetValues(w, XmNuserData, &dec, NULL);
   fff = (float)cbs->value / pow(10.0, dec);
   SUMA_LHv("Have %f\n", fff);
   SUMA_set_slice(ado, "Co", &fff, "scale", 1);
   
   SUMA_RETURNe;
}

int SUMA_set_slice(SUMA_ALL_DO *ado, char *variant, float *valp, 
                   char *caller, int redisp)
{
   static char FuncName[]={"SUMA_set_slice"};
   float oval, val;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_VolumeObject *VO=(SUMA_VolumeObject *)ado;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !variant) SUMA_RETURN(0);
   if (!caller) caller = "XXX"; /* Don't update other input sources */
   
   SurfCont = SUMA_ADO_Cont(ado);
   
   if (!valp) {
      val = SUMA_VO_N_Slices(VO, variant);/* dirty trick to force scale height */
   } else val = *valp;
 
   SUMA_LH("Slice %s set to %f", variant, val); 
   
   if (caller[0] == 'i') { /* incrementor */
      /* calculate val to reflect increment */
             if (variant[0] == 'A') {
            val += SurfCont->Ax_slc->slice_num; 
      } else if (variant[0] == 'S') {
            val += SurfCont->Sa_slc->slice_num;   
      } else if (variant[0] == 'C') {
            val += SurfCont->Co_slc->slice_num;     
      } else if (variant[0] == 'V') {
            val += SurfCont->VR_fld->N_slice_num;  
      }
   }
   
   if (val < 0) val = 0;
   else if (val >= SUMA_VO_N_Slices(VO, variant) &&
                   variant[0] != 'V') 
                     val = SUMA_VO_N_Slices(VO, variant)-1;  

   /* call this one since it is not being called as the slider is dragged. */
   if (caller[0] != 'X' && variant[0] != 'V') { /* Update equivalent */
      if (caller[0] != 't') { /* Caller not from text field so set that*/
         SUMA_set_slice_label(ado, variant, val);   
      }
      if (caller[0] != 's') { /* Caller not from slider, so update that too */
         SUMA_set_slice_scale(ado, variant, val);
      }
   }
   
          if (variant[0] == 'A') {
         SurfCont->Ax_slc->slice_num = val;   
   } else if (variant[0] == 'S') {
         SurfCont->Sa_slc->slice_num = val;   
   } else if (variant[0] == 'C') {
         SurfCont->Co_slc->slice_num = val;      
   } else if (variant[0] == 'V') {
         SurfCont->VR_fld->N_slice_num = val;      
   }

   if (redisp) SUMA_Remixedisplay(ado);

   /* sad as it is */
   if (variant[0] != 'V') SUMA_FORCE_SLICE_SCALE_WIDTH(SUMA_ADO_Cont(ado)); 
   
   #if 0 /* I don't think this will be necessary */
   SUMA_ADO_Flush_Pick_Buffer(ado, NULL);
   #endif
   
   /* And this? 
   #if SUMA_SEPARATE_SURF_CONTROLLERS
      SUMA_UpdateColPlaneShellAsNeeded(ado);
   #endif
   */
   
   if (0) {
      static int ncnt=0;
      /* Do nothing about selection. Not sure what would be of use 
      One could supposedly move the selection - if the selection
      was on the slice being moved. Something to ponder for the
      future 
         Remember selection could be on any of the slices in the
         montage, or even elsewhere in space....
      */
      if (!ncnt) {
         SUMA_S_Warn("What to do about selected voxels here? Move them along?");
         ++ncnt;
      }
      SUMA_UpdateNodeValField(ado);
      SUMA_UpdateNodeLblField(ado);
   }
 
   SUMA_RETURN(1);   
}

int SUMA_set_mont(SUMA_ALL_DO *ado, char *variant, 
                  float *val1p, float *val2p,
                  char *caller, int redisp)
{
   static char FuncName[]={"SUMA_set_mont"};
   float oval, val1, val2;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_VolumeObject *VO=(SUMA_VolumeObject *)ado;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !variant) SUMA_RETURN(0);
   if (!caller) caller = "XXX"; /* Don't update other input sources */
   
   SurfCont = SUMA_ADO_Cont(ado);
   
   if (!val1p) {
      val1 = 1;
   } else val1 = *val1p;
   if (!val2p) {
      val2 = 1;
   } else val2 = *val2p;
 
      if (val1 < 1) val1 = 1;
   else if (val1 > SUMA_VO_N_Slices(VO, variant)) 
                     val1 = SUMA_VO_N_Slices(VO, variant);
      if (val2 < 1) val2 = 1;
   else if (val2 > SUMA_VO_N_Slices(VO, variant)) 
                     val2 = SUMA_VO_N_Slices(VO, variant);
   
   val1 = (int)val1; val2 = (int)val2;
   SUMA_LH("Slice %s mont set to %d %d", variant, (int)val1, (int)val2); 
   

          if (variant[0] == 'A') {
         SurfCont->Ax_slc->mont_num = val1;
         SurfCont->Ax_slc->mont_inc = val2;   
   } else if (variant[0] == 'S') {
         SurfCont->Sa_slc->mont_num = val1;
         SurfCont->Sa_slc->mont_inc = val2;   
   } else if (variant[0] == 'C') {
         SurfCont->Co_slc->mont_num = val1;
         SurfCont->Co_slc->mont_inc = val2;      
   }

   if (redisp) SUMA_Remixedisplay(ado);
 
   SUMA_RETURN(1);   
}

/*!
   \brief create slice selection widgets
*/
void SUMA_CreateSliceFields(  Widget parent,
                        char *tit, char *hint, char *help, 
                        int Nslc, char *var, SUMA_ALL_DO *ado,
                        void (*NewValueCallback)(void * data), void *cb_data,
                        SUMA_SLICE_FIELD *SF) 
{
   static char FuncName[]={"SUMA_CreateSliceFields"};
   int i, j, n, titw, xmw, shad, mult;
   char *tmp, sbuf[12], wname[64]={"SetMePlease"};
   XtPointer cd;
   XtVarArgsList arglist=NULL;
   XtCallbackProc slcb, sllbcb, slcactcb, shwslccb;
   int shw_init = 0;
   SUMA_VOL_SAUX *VSaux=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;                        

   if (!parent) { SUMA_SL_Err("NULL parent"); SUMA_RETURNe; }
   if (!ado) { SUMA_S_Err("NULL ado"); SUMA_RETURNe; }
   if (!(VSaux = SUMA_ADO_VSaux(ado))) { SUMA_S_Err("No VSaux"); SUMA_RETURNe; }
   if (!tit) tit = var;
   
   if (LocalHead) {
      SUMA_S_Warn("Are NewValueCallback and its data needed at all?");
   }
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
   
   if (!SF) { SUMA_SL_Err("NULL SF"); SUMA_RETURNe; }
   if (!var){ SUMA_S_Err("Bad var format"); SUMA_RETURNe; }
   SUMA_LH("Init");
   if (!strcmp(var,"Ax")) {
      slcb = SUMA_cb_set_Ax_slice;
      sllbcb = SUMA_cb_set_Ax_slice_label;
      slcactcb = SUMA_SliceF_cb_label_change;
      shwslccb = SUMA_cb_ShowAxSlice_toggled;
      shw_init = VSaux->ShowAxSlc;
   } else if (!strcmp(var,"Sa")) {
      slcb = SUMA_cb_set_Sa_slice;
      sllbcb = SUMA_cb_set_Sa_slice_label; 
      slcactcb = SUMA_SliceF_cb_label_change;
      shwslccb = SUMA_cb_ShowSaSlice_toggled;
      shw_init = VSaux->ShowSaSlc;
   } else if (!strcmp(var,"Co")) {   
      slcb = SUMA_cb_set_Co_slice;
      sllbcb = SUMA_cb_set_Co_slice_label; 
      slcactcb = SUMA_SliceF_cb_label_change;
      shwslccb = SUMA_cb_ShowCoSlice_toggled;
      shw_init = VSaux->ShowCoSlc;
   } else {
      SUMA_S_Err("Bad var %s", var);
      SUMA_RETURNe;
   }
   SF->variant = SUMA_copy_string(var);
   SF->Nslc = Nslc;
   SF->NewValueCallback = NewValueCallback;
   SF->NewValueCallbackData = cb_data;
   /* An outer row column to keep the inner one from resizing with parent 
   YOU COULD HAVE SET XmNadjustLast to False, instead ....*/
   SF->rc = XtVaCreateManagedWidget ("rowcolumn",
      xmRowColumnWidgetClass, parent,
      XmNorientation , XmHORIZONTAL ,
      XmNpacking, XmPACK_TIGHT,
      XmNmarginHeight, 0,
      XmNmarginWidth, 0,
      NULL);
   
   SUMA_LH("Widgets, var %s, tit %s, slice num = %d, Nslc = %d, wname = %s", 
            var, tit, (int)SF->slice_num, SF->Nslc, SF->wname);
   SF->lab = XtVaCreateManagedWidget(tit, xmLabelWidgetClass, SF->rc, 
                           XmNfontList, SUMAg_CF->X->TableTextFontList, 
                                     NULL);
   if (hint || help) {
      snprintf(wname,63, "%s->%s", SF->wname, tit);
      SUMA_Register_Widget_Help( SF->lab, 1, wname, hint, help);
   }
   mult = ((int)(SF->Nslc/20.0)/5)*5; if (mult < 1) mult = 1;
   arglist = XtVaCreateArgsList( NULL,
                           XmNshowValue, False,
                           XmNmaximum, SF->Nslc,
                           XmNvalue, (int)SF->slice_num,
                           XmNscaleMultiple, mult,
                           XmNheight,  SUMA_SCALE_HEIGHT,
                           XmNorientation, XmHORIZONTAL,
                           XmNuserData, (XtPointer)0,
                           NULL);

   SF->sl =  XtVaCreateManagedWidget(var,
                                 xmScaleWidgetClass, SF->rc,
                                 XtVaNestedList, arglist,
                                 NULL);             
   XtAddCallback (SF->sl, 
                  XmNvalueChangedCallback, 
                  slcb, 
                  (XtPointer) ado);

   XtAddCallback (SF->sl, 
                  XmNdragCallback, 
                  sllbcb, 
                  (XtPointer) ado); 

   if (arglist) XtFree(arglist); arglist = NULL;
   
   sprintf(sbuf,"%-3d", (int)SF->slice_num);
   SF->text = XtVaCreateManagedWidget(
                     "slice",  
                     xmTextFieldWidgetClass, SF->rc,
                     XmNuserData, (XTP_CAST)ado,
                     XmNvalue, sbuf,
                     XmNmarginHeight, 0,
                     XmNmarginTop, 0,
                     XmNmarginBottom, 0,
                     XmNmarginWidth, 5, 
                     NULL);
   XtVaSetValues( SF->text, XmNfontList, 
                  SUMAg_CF->X->TableTextFontList, NULL);

   if (hint || help) {
      snprintf(wname, 63, "%s->%s_text", SF->wname, tit);
      SUMA_Register_Widget_Help( SF->text, 1, wname, hint, help);
   }
   XtVaSetValues(SF->text, XmNcolumns, 3, NULL); 
   XtVaSetValues(SF->text, XmNeditable, True, 
                 XmNshadowThickness , 2,         
                 XmNcursorPositionVisible, True, 
                 NULL);

   XtAddCallback (SF->text, XmNactivateCallback, 
               SUMA_SliceF_cb_label_change, (XtPointer)SF);
   /* add event handler to notify when widget was left */
   XtInsertEventHandler( SF->text ,        /* notify when */
                         LeaveWindowMask ,  /* pointer leaves */
                         FALSE ,            /* this window */
                         SUMA_leave_SliceField,
                         (XtPointer) SF ,
                         XtListTail ) ;     /* last in queue */

   sprintf(sbuf,"%d:%d", (int)SF->mont_num, (int)SF->mont_inc);
   SF->mont = XtVaCreateManagedWidget(
                     "mont",  
                     xmTextFieldWidgetClass, SF->rc,
                     XmNuserData, (XTP_CAST)ado,
                     XmNvalue, sbuf,
                     XmNmarginHeight, 0,
                     XmNmarginTop, 0,
                     XmNmarginBottom, 0,
                     XmNmarginWidth, 5, 
                     NULL);
   XtVaSetValues( SF->mont, XmNfontList, 
                  SUMAg_CF->X->TableTextFontList, NULL);

   if (hint || help) {
      snprintf(wname, 63, "%s->mont", SF->wname);
      SUMA_Register_Widget_Help( SF->mont, 1, wname, hint, help);
   }
   XtVaSetValues(SF->mont, XmNcolumns, 5, NULL); 
   XtVaSetValues(SF->mont, XmNeditable, True, 
                 XmNshadowThickness , 2,         
                 XmNcursorPositionVisible, True, 
                 NULL);

   XtAddCallback (SF->mont, XmNactivateCallback, 
               SUMA_SliceF_cb_mont_change, (XtPointer)SF);
   /* add event handler to notify when widget was left */
   XtInsertEventHandler( SF->mont ,        /* notify when */
                         LeaveWindowMask ,  /* pointer leaves */
                         FALSE ,            /* this window */
                         SUMA_leave_MontField,
                         (XtPointer) SF ,
                         XtListTail ) ;     /* last in queue */


   /* Now for the toggle button */
   SF->tb = XtVaCreateManagedWidget("v", 
      xmToggleButtonWidgetClass, SF->rc, NULL);
   XtAddCallback (SF->tb, 
         XmNvalueChangedCallback, shwslccb, ado);
   snprintf(wname, 63, "%s->tb", SF->wname);
   SUMA_Register_Widget_Help(SF->tb, 1, wname, 
                     "View (ON)/Hide Slice(s)",
                     SUMA_SurfContHelp_ShowSliceTgl);

   SUMA_SET_SELECT_COLOR(SF->tb);
   XmToggleButtonSetState (SF->tb, shw_init , NOPE);
   
   SUMA_RETURNe;
}

void SUMA_cb_ShowAxSlice_toggled(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ShowAxSlice_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
      
   SUMA_SetShowSlice((SUMA_VolumeObject *)ado, "Ax", 
                      XmToggleButtonGetState (SurfCont->Ax_slc->tb));
   SUMA_RETURNe;
}


void SUMA_cb_ShowSaSlice_toggled(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ShowSaSlice_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
      
   SUMA_SetShowSlice((SUMA_VolumeObject *)ado, "Sa", 
                      XmToggleButtonGetState (SurfCont->Sa_slc->tb));
   SUMA_RETURNe;
}

void SUMA_cb_ShowCoSlice_toggled(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ShowCoSlice_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
      
   SUMA_SetShowSlice((SUMA_VolumeObject *)ado, "Co", 
                      XmToggleButtonGetState (SurfCont->Co_slc->tb));
   SUMA_RETURNe;
}

int SUMA_SetShowSlice(SUMA_VolumeObject *vdo, char *variant, int val)
{
   static char FuncName[]={"SUMA_SetShowSlice"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_VOL_SAUX *VSaux = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called variant %s, val %d", variant?variant:"NULL", val);
   
   ado = (SUMA_ALL_DO *)vdo;
   VSaux = SUMA_ADO_VSaux(ado);
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado)) || !VSaux || !variant) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURN(0); }
   
   if (!strcmp(variant, "Ax")) {
      if (VSaux->ShowAxSlc != val) {
         VSaux->ShowAxSlc = val;
         SUMA_Remixedisplay(ado);
         #if SUMA_SEPARATE_SURF_CONTROLLERS
            SUMA_UpdateColPlaneShellAsNeeded(ado);
         #endif        
      }
   } else if (!strcmp(variant, "Sa")) {
      if (VSaux->ShowSaSlc != val) {
         VSaux->ShowSaSlc = val;
         SUMA_Remixedisplay(ado);
         #if SUMA_SEPARATE_SURF_CONTROLLERS
            SUMA_UpdateColPlaneShellAsNeeded(ado);
         #endif        
      }
   } else if (!strcmp(variant, "Co")) {
      if (VSaux->ShowCoSlc != val) {
         VSaux->ShowCoSlc = val;
         SUMA_Remixedisplay(ado);
         #if SUMA_SEPARATE_SURF_CONTROLLERS
            SUMA_UpdateColPlaneShellAsNeeded(ado);
         #endif        
      }
   } else if (!strcmp(variant, "Vr")) {
      if (VSaux->ShowVrSlc != val) {
         VSaux->ShowVrSlc = val;
         SUMA_Remixedisplay(ado);
         #if SUMA_SEPARATE_SURF_CONTROLLERS
            SUMA_UpdateColPlaneShellAsNeeded(ado);
         #endif        
      }
      SUMA_LH("ShowVrSlc now %d", VSaux->ShowVrSlc);
   } else if (!strcmp(variant, "AtXYZ")) {
      SUMA_SurfaceViewer *sv=NULL;
      if (VSaux->SlicesAtCrosshair != val) {
         VSaux->SlicesAtCrosshair = val;
         if (VSaux->SlicesAtCrosshair && (sv=SUMA_OneViewerWithADOVisible(ado))
             && sv->Ch){
            SUMA_VO_set_slices_XYZ(vdo, sv->Ch->c_noVisX);
         }
         SUMA_Remixedisplay(ado);
         #if SUMA_SEPARATE_SURF_CONTROLLERS
            SUMA_UpdateColPlaneShellAsNeeded(ado);
         #endif        
      }
      SUMA_LH("SlicesAtCrosshair now %d", VSaux->SlicesAtCrosshair);
   } else if (!strcmp(variant, "Sel")) {
      if (VSaux->VrSelect != val) {
         VSaux->VrSelect = val;
      }
      SUMA_LH("VrSelect now %d", VSaux->VrSelect);
   } else {
      SUMA_S_Err("And what is variant %s for?", variant);
      SUMA_RETURN(NOPE);
   }
   SUMA_RETURN(1);
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

int SUMA_RowTitCell(SUMA_TABLE_FIELD *TF, int r)
{
   static char FuncName[]={"SUMA_RowTitCell"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!TF || !TF->HasRowTit || r < 0 || r >= TF->Ni) SUMA_RETURN(-1);
   
   SUMA_RETURN(r);
}

int SUMA_ColTitCell(SUMA_TABLE_FIELD *TF, int c)
{
   static char FuncName[]={"SUMA_ColTitCell"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!TF || !TF->HasColTit || c < 0 || c >= TF->Nj) SUMA_RETURN(-1);
   
   SUMA_RETURN(c*TF->Ni);
}

int SUMA_ObjectID_Row(SUMA_TABLE_FIELD *TF, char *id)
{
   static char FuncName[]={"SUMA_ObjectID_Row"};
   int found = -1, ii=-1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!TF || !TF->rowobject_id || !id) SUMA_RETURN(-1);

   if (LocalHead) {
      fprintf(SUMA_STDERR, "Seeking %s\n"
                           "in      ", id);
      for (ii=0; ii<TF->Ni; ++ii) {
         fprintf(SUMA_STDERR,"%s\n        ", 
                 TF->rowobject_id[ii]?TF->rowobject_id[ii]:"NULL");
      }
      fprintf(SUMA_STDERR, "\n");
   }
   found = -1; ii=0;
   while (found < 0 && ii < TF->Ni) {
      if (TF->rowobject_id[ii] &&
          !strcmp(id, TF->rowobject_id[ii])) {
         found = ii;
      }
      ++ii;
   }
   
   SUMA_RETURN(found);
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
   \brief This function is called when mouse pointer leaves montage field
*/
void SUMA_leave_MontField( Widget w , XtPointer client_data ,
                            XEvent * ev , Boolean * continue_to_dispatch )
{
   static char FuncName[]={"SUMA_leave_MontField"};
   SUMA_SLICE_FIELD *SF=NULL; 
   XLeaveWindowEvent * lev = (XLeaveWindowEvent *) ev ;
   XmAnyCallbackStruct cbs ;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SF = (SUMA_SLICE_FIELD *)client_data ;
   if( lev->type != LeaveNotify) SUMA_RETURNe; 

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Leave notification.\n", FuncName);
   
   SUMA_SliceF_cb_mont_change( w , (XtPointer)SF , NULL ) ;

   SUMA_RETURNe;
}

/*!
   \brief This function is called when mouse pointer leaves slice field
*/
void SUMA_leave_SliceField( Widget w , XtPointer client_data ,
                            XEvent * ev , Boolean * continue_to_dispatch )
{
   static char FuncName[]={"SUMA_leave_SliceField"};
   SUMA_SLICE_FIELD *SF=NULL; 
   XLeaveWindowEvent * lev = (XLeaveWindowEvent *) ev ;
   XmAnyCallbackStruct cbs ;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SF = (SUMA_SLICE_FIELD *)client_data ;
   if( lev->type != LeaveNotify) SUMA_RETURNe; 

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Leave notification.\n", FuncName);
   
   SUMA_SliceF_cb_label_change( w , (XtPointer)SF , NULL ) ;

   SUMA_RETURNe;
}

/*!
   \brief User entered new slice value
*/
void SUMA_SliceF_cb_label_change (  Widget w, XtPointer client_data, 
                                    XtPointer call_data)
{
   static char FuncName[]={"SUMA_SliceF_cb_label_change"};
   SUMA_SLICE_FIELD *SF=NULL;
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
   SF = (SUMA_SLICE_FIELD *)client_data;
   
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
   
   DoCallBacks = YUP;   /* do the callbacks even if no carriage return ... */
   /* Check if the string is numerical, and get unit */
   XtVaGetValues (w, XmNvalue, &n, NULL);
   cs = (char *)n;
   if (!cs || !strlen(cs)) {/* empty cell, leave it alone */ 
      SUMA_LHv("empty %s", cs);
      SUMA_RETURNe;
   } else  {
      SUMA_COUNT_WORDS(cs, NULL, N_words);
      if (!N_words) { /* no harm, go back */ 
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
      SUMA_SliceF_SetString (SF);
   }else {
      if (SF->slice_num == val &&
          SF->slice_units == unt) { 
            SUMA_LH("Same value"); 
            SUMA_RETURNe; 
      }
      SUMA_LH("A new beast? %f, %f, %d %d",
              SF->slice_num, val, SF->slice_units, unt);
      SF->slice_num = val;
      SF->slice_units = unt;
      SUMA_SliceF_SetString (SF);
   }

   if (DoCallBacks) {
      SUMA_set_slice((SUMA_ALL_DO *)SF->NewValueCallbackData, SF->variant, &val,
                     "text_field", 1);
   }
   
   SUMA_RETURNe;
}

/*!
   \brief User entered new montage string
*/
void SUMA_SliceF_cb_mont_change (  Widget w, XtPointer client_data, 
                                    XtPointer call_data)
{
   static char FuncName[]={"SUMA_SliceF_cb_mont_change"};
   SUMA_SLICE_FIELD *SF=NULL;
   float val1, val2;
   int N_words = 0;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
   void *n=NULL;
   char *cs=NULL, *ss=NULL;
   int unt = SUMA_NO_NUM_UNITS;
   SUMA_Boolean DoCallBacks;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Nothing done yet, build based on SUMA_SliceF_cb_label_change");
   /* make call to NewValue callback */
   SF = (SUMA_SLICE_FIELD *)client_data;
   
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
   
   DoCallBacks = YUP;   /* do the callbacks even if no carriage return ... */
   /* Check if the string is numerical, and get unit */
   XtVaGetValues (w, XmNvalue, &n, NULL);
   cs = (char *)n;
   if (!cs || !strlen(cs)) {/* empty cell, leave it alone */ 
      SUMA_LHv("empty %s", cs);
      SUMA_RETURNe;
   } else  {
      SUMA_COUNT_WORDS(cs, NULL, N_words);
      if (!N_words) { /* no harm, go back */ 
         SUMA_LHv("spacy %s", cs);
         SUMA_RETURNe; 
      }
   }
   val1 = -1;
   val2 = -1;
   unt = SUMA_NumStringUnits(cs, 0);
   /* Split based on the presence of ':' */
   if (!(ss = SUMA_NI_get_ith_string(cs, ":", 0))) {
      SUMA_LH("Nothing in cs?");
      SUMA_RETURNe;
   }
   if (SUMA_StringToNum(ss, (void *)&val1, 1, 1) != 1) {
      SUMA_BEEP;
      /* bad syntax, reset value*/
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Bad syntax.\n", FuncName);
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                            "Bad value in text field", FuncName, 
                            SMT_Error, SMA_Log);
      SUMA_SliceF_SetString (SF);
      val1 = SF->mont_num;
   } else {
      SUMA_ifree(ss);
      if (!(ss = SUMA_NI_get_ith_string(cs, ":", 1))) {
         SUMA_LH("No second number, assume 1");
         val2 = 1;
      } else if (SUMA_StringToNum(ss, (void *)&val2, 1, 1) != 1) {
         SUMA_BEEP;
         /* bad syntax, reset value*/
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Bad syntax.\n", FuncName);
         SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                               "Bad value in text field", FuncName, 
                               SMT_Error, SMA_Log);
         SUMA_SliceF_SetString (SF);
         val2 = SF->mont_inc;
      }
   }
   SUMA_ifree(ss);
   
   if (val1 >= 0.0 && val2 >= 0.0) {
      if (SF->mont_num == val1 &&
          SF->mont_inc == val2) { 
            SUMA_LH("Same value"); 
            SUMA_RETURNe; 
      }
      SF->mont_num = val1;
      SF->mont_inc = val2;
      SUMA_SliceF_SetString (SF);
   }

   if (DoCallBacks) {
      SUMA_set_mont((SUMA_ALL_DO *)SF->NewValueCallbackData, SF->variant, 
                     &val1, &val2, "text_field", 1);
   }
   
   SUMA_RETURNe;
}

void SUMA_SliceF_SetString (SUMA_SLICE_FIELD * SF)
{
   static char FuncName[]={"SUMA_SliceF_SetString"};
   char buf[36];

   SUMA_ENTRY;

   if (SF->slice_units == SUMA_NO_NUM_UNITS) {
      sprintf (buf, "%-4d", (int)SF->slice_num);
   }else if (SF->slice_units == SUMA_MM_UNITS) {
      sprintf (buf, "%s", 
               MV_format_fval2(  SF->slice_num, 3));
   }else {
      /* fair enough, must be stringy */
   }
   
   XtVaSetValues (SF->text, XmNvalue, buf, NULL);
   SUMA_RETURNe;
}


/*!
   \brief This function is called when the label field is activated by the user
\*/
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
      } else if (TF->type == SUMA_string) {
         XtVaGetValues (w, XmNvalue, &n, NULL);
         cs = (char *)n;
         if ( TF->str_value && TF->str_value[TF->cell_modified] &&
             !strcmp(TF->str_value[TF->cell_modified],cs)) {
               SUMA_LH("Same string"); 
               TF->cell_modified = -1;
               SUMA_RETURNe;    
         }
         SUMA_LH("Here now, modified %d \n", TF->cell_modified);
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

int  SUMA_SliceVal2ScalePos (SUMA_ALL_DO *ado, char *variant, float *val)
{
   static char FuncName[]={"SUMA_SliceVal2ScalePos"};
   int min_v=0, max_v=0, cv=0, scl=0, dec=0;
   float ftmp;
   Widget w = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_VOL_SAUX *VSaux=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))
       || !(VSaux = SUMA_ADO_VSaux(ado)) || !variant) { 
      SUMA_SL_Err("Null ado or no SurfCont"); SUMA_RETURN(0); 
   }
   if (variant[0] == 'A') {
      w = SurfCont->Ax_slc->sl;
   } else if (variant[0] == 'S') {
      w = SurfCont->Sa_slc->sl;
   } else if (variant[0] == 'C') {
      w = SurfCont->Co_slc->sl;
   }  
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
   
   if (*val < 0) { /* ignore sign */
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


/*!
   threshold value to scale value (position)
   NOTE val's value might change if it is outside the slider range
   or if it is negative when the slider is in |T| mode
*/
int SUMA_ThreshVal2ScalePos(SUMA_ALL_DO *ado, float *val)
{
   static char FuncName[]={"SUMA_ThreshVal2ScalePos"};
   int min_v=0, max_v=0, cv=0, scl=0, dec=0;
   float ftmp;
   Widget w = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) { 
      SUMA_SL_Err("Null ado or no SurfCont"); SUMA_RETURN(0); 
   }
   curColPlane = SUMA_ADO_CurColPlane(ado);
   w = SurfCont->thr_sc;
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
       curColPlane->OptScl->ThrMode == SUMA_ABS_LESS_THAN) {
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

double SUMA_Pval2ThreshVal (SUMA_ALL_DO *ado, double pval) 
{
   static char FuncName[]={"SUMA_Pval2ThreshVal"};
   float p[3], zval = -1.0;
   int statcode;
   double val = 0.0;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) { 
      SUMA_SL_Err("Null ado or no SurfCont"); SUMA_RETURN(val); 
   }
   curColPlane = SUMA_ADO_CurColPlane(ado);

   if (!SurfCont || 
       !SurfCont->thr_sc ||
       !curColPlane ||
       !curColPlane->dset_link) { 
      SUMA_SL_Err("NULL SurfCont or other things");
      SUMA_RETURN(val); 
   }
     
   /* see if you can get the stat codes */
   if (!SUMA_GetDsetColStatAttr(  
            curColPlane->dset_link, 
            curColPlane->OptScl->tind, 
            &statcode,
            p, (p+1), (p+2))) {
      SUMA_LH("Error");        
   }else if (statcode) {
      SUMA_LHv("Have stats at sb %d\n"
               "statcode %d: %f %f %f\n", 
               curColPlane->OptScl->tind,
               statcode, p[0], p[1], p[2]);
      curColPlane->OptScl->ThreshStats[0] = pval;
      val = THD_pval_to_stat( pval , statcode , p  ) ;
      SUMA_LHv("Have pval of %f\n"
               "      val of %f\n", 
               curColPlane->OptScl->ThreshStats[0],val);
   } else {
      /* no stats */
      curColPlane->OptScl->ThreshStats[0] = -1.0;
      curColPlane->OptScl->ThreshStats[1] = -1.0;
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
int SUMA_SetScaleThr_one(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                          float *val, int setmen, int redisplay) 
{
   static char FuncName[]={"SUMA_SetScaleThr_one"};
   SUMA_ALL_DO *curDO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int cv=0;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   
   if (!(SurfCont=SUMA_ADO_Cont(ado)) || 
       !SurfCont->SetThrScaleTable) SUMA_RETURN(0);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (colp && colp != curColPlane) SUMA_RETURN(0);
   
   if (!(curDO = SUMA_SurfCont_GetcurDOp(SurfCont))) {
      SUMA_S_Err("Failed to get curDOp");
      SUMA_RETURN(0);
   }
   TF = SurfCont->SetThrScaleTable;
   
   switch (TF->num_units) {
      case SUMA_P_VALUE_UNITS:
         if (LocalHead) 
               fprintf( SUMA_STDERR,
                        "%s:\nUnits in p value, transforming %f\n",
                        FuncName, *val);
         /* transform value from P to threshold value */
         *val = (float)SUMA_Pval2ThreshVal (ado, (double)*val);
         if (LocalHead) 
               fprintf( SUMA_STDERR,
                        "   to %f\n",
                        *val);
         /* reset the units of the table to reflect new value, 
            string containing new val is reset later on*/
         TF->num_units = SUMA_NO_NUM_UNITS;
         break;
      case SUMA_PERC_VALUE_UNITS:
         SUMA_LH("Units in percentile value, transforming %f\n", *val);
         *val = SUMA_OverlayPercentile(colp, 'T', *val);
         TF->num_units = SUMA_NO_NUM_UNITS;
         break;
      default:
         break;
   }
   
   cv = SUMA_ThreshVal2ScalePos (ado, val );

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
   curColPlane->OptScl->ThreshRange[0] = *val;
   XtVaSetValues(SurfCont->thr_sc,  
            XmNvalue, cv, 
            NULL);   

   SUMA_LHv("Colorize if necessary, redisplay=%d\n", redisplay);
   /* colorize if necessary */
   if ( redisplay == 0 ||
        (redisplay == 1 && !curColPlane->OptScl->UseThr) ) { 
      SUMA_RETURN(0); 
   } /* nothing else to do */


   SUMA_ADO_Flush_Pick_Buffer(ado, NULL);

   SUMA_LH("Colorize");
   if (!SUMA_ColorizePlane (curColPlane)) {
      SUMA_SLP_Err("Failed to colorize plane.\n");
      SUMA_RETURN(0);
   }
   
   SUMA_LH("Remix redisplay");
   SUMA_Remixedisplay(ado);

   SUMA_UpdateNodeLblField(ado);
   SUMA_UpdatePvalueField( ado,
                           curColPlane->OptScl->ThreshRange[0]);
   SUMA_RETURN(1);  
}

int SUMA_SetScaleThr(SUMA_ALL_DO *ado, SUMA_OVERLAYS *colp,
                          float *val, int setmen, int redisplay) 
{
   static char FuncName[]={"SUMA_SetScaleThr"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!ado || !SurfCont || !curColPlane) SUMA_RETURN(0);
   
   if (colp && colp != curColPlane) SUMA_RETURN(0);
   colp = curColPlane;
   
   if (!SUMA_SetScaleThr_one(ado, colp, val, setmen, redisplay)) SUMA_RETURN(0);
      
   if (ado->do_type == SO_type) {
      SUMA_SurfaceObject *SOC=NULL, *SO = (SUMA_SurfaceObject *)ado;
      SUMA_OVERLAYS *colpC=NULL;
      /* do we have a contralateral SO and overlay? */
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (!SUMA_SetScaleThr_one((SUMA_ALL_DO *)SOC, 
                                    colpC, val, 1, redisplay)) SUMA_RETURN(0);
      }
   }
   SUMA_RETURN(1);
}

void SUMA_cb_SetScaleThr(void *data) 
{
   static char FuncName[]={"SUMA_cb_SetScaleThr"};
   SUMA_ALL_DO *ado=(SUMA_ALL_DO *)data, *curDO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int cv, max_v, min_v, cell_mod = -1;
   float val;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   if (!ado) SUMA_RETURNe;
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!(curDO = SUMA_SurfCont_GetcurDOp(SurfCont))) {
      SUMA_S_Err("Failed to get curDOp");
      SUMA_RETURNe;
   }
   TF = SurfCont->SetThrScaleTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   val = TF->num_value[TF->cell_modified];
   
   SUMA_SetScaleThr(ado, NULL, &val, 0, 1);
   
   SUMA_RETURNe;
}

/*!
   \brief Sends the Focus triangle  when new value is entered
*/
void SUMA_TriInput (void *data)
{
   static char FuncName[]={"SUMA_TriInput"};
   SUMA_ALL_DO *ado=(SUMA_ALL_DO *)data, *curDO=NULL;
   SUMA_SurfaceObject *SO=NULL, *curSO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   float fv3[3];
   char str[100];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   SurfCont = SUMA_ADO_Cont(ado);
   if (!(curDO = SUMA_SurfCont_GetcurDOp(SurfCont))) {
      SUMA_S_Err("Failed to get curDOp");
      SUMA_RETURNe;
   }

   if (ado->do_type != SO_type || curDO->do_type != SO_type) {
      SUMA_S_Err("Should not call this");
      SUMA_RETURNe;
   }
   SO = (SUMA_SurfaceObject *)ado;
   curSO = (SUMA_SurfaceObject *)curDO;
   
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

         /* look for a viewer that is showing this surface and 
            has this surface in focus*/
         for (i=0; i<SUMAg_N_SVv; ++i) {
            SUMA_LHv("Checking viewer %d.\n", i);
            if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
               /* is this viewer showing curSO ? */
               if (SUMA_isVisibleDO(&(SUMAg_SVv[i]), SUMAg_DOv, 
                                    (SUMA_ALL_DO *)curSO)) {
                  if (SUMA_SV_Focus_SO(&(SUMAg_SVv[i])) == curSO) {
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
   \brief Sends the node/datum flying when new value is entered
*/
void SUMA_NodeInput (void *data)
{
   static char FuncName[]={"SUMA_NodeInput"};
   SUMA_ALL_DO *ado=(SUMA_ALL_DO *)data, *curDO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   float fv3[3];
   char str[100];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   
   SurfCont = SUMA_ADO_Cont(ado);

   if (!(curDO = SUMA_SurfCont_GetcurDOp(SurfCont))) {
      SUMA_S_Err("Failed to get curDOp");
      SUMA_RETURNe;
   }
   TF = SurfCont->NodeTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   i = n % TF->Ni;
   j = n / TF->Ni;
   
   if ((int)TF->num_value[n] < 0 || 
       (int)TF->num_value[n] > SUMA_ADO_Max_Datum_Index(ado)) {
      SUMA_SLP_Err("Node/Voxel/etc index must be positive and \n"
                   "less than the number of nodes \n"
                   "forming the surface.\n");
      TF->num_value[n] = SUMA_ADO_SelectedDatum(ado, NULL, NULL);
      SUMA_TableF_SetString (TF);
      TF->cell_modified = -1;
      SUMA_RETURNe;
   }
   if (ado->do_type == MASK_type) {
      SUMA_S_Warn("Have not dealt with masks yet");
      SUMA_RETURNe;
   }
   switch (j) {
      case 1:
         XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s, node = %d\n", 
                  FuncName, i, j, (char *)cv, (int)TF->num_value[n]);
         }

         /* look for a viewer that is showing this surface and 
            has this surface in focus*/
         for (i=0; i<SUMAg_N_SVv; ++i) {
            SUMA_LHv("Checking viewer %d.\n", i);
            if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
               /* is this viewer showing curSO ? */
               if (SUMA_isVisibleDO(&(SUMAg_SVv[i]), SUMAg_DOv,
                                    curDO)) {
                  if (SUMA_SV_Focus_ADO(&(SUMAg_SVv[i])) == curDO) {
                        SUMA_JumpIndex((char *)cv, (void *)(&(SUMAg_SVv[i])));
                  }
               }
            }
         }
         break;
      case 2:
         XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s, node = %d\n", 
                  FuncName, i, j, (char *)cv, (int)TF->num_value[n]);
         }
         switch (ado->do_type) {
            case TRACT_type:
               for (i=0; i<SUMAg_N_SVv; ++i) {
                  SUMA_LHv("Checking viewer %d.\n", i);
                  if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
                     /* is this viewer showing curDO ? */
                     if (SUMA_isVisibleDO(&(SUMAg_SVv[i]), SUMAg_DOv,
                                          curDO)) {
                        if (SUMA_SV_Focus_ADO(&(SUMAg_SVv[i])) == curDO) {
                                 SUMA_JumpIndex((char *)cv, 
                                                (void *)(&(SUMAg_SVv[i])));
                        }
                     }
                  }
               }
               break;
            default: 
               SUMA_LH("Nothing to do here");
               break;
         }
         break;
      default:
         SUMA_SL_Err("Should not get this input");
         break;
   }
   SUMA_RETURNe;  
}


void SUMA_IJKInput(void *data) {
   SUMA_TpointInput (data);
   return;
}
/*!
   \brief Sends the node/datum flying when new value is entered
*/
void SUMA_TpointInput (void *data)
{
   static char FuncName[]={"SUMA_TpointInput"};
   SUMA_ALL_DO *ado=(SUMA_ALL_DO *)data, *curDO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   float fv3[3];
   char str[100];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   
   SurfCont = SUMA_ADO_Cont(ado);

   if (!(curDO = SUMA_SurfCont_GetcurDOp(SurfCont))) {
      SUMA_S_Err("Failed to get curDOp");
      SUMA_RETURNe;
   }
   TF = SurfCont->FaceTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   i = n % TF->Ni;
   j = n / TF->Ni;
   
   switch (j) {
      case 1:
         XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s\n", 
                  FuncName, i, j, (char *)cv);
         }

         /* look for a viewer that is showing this surface and 
            has this surface in focus*/
         for (i=0; i<SUMAg_N_SVv; ++i) {
            SUMA_LHv("Checking viewer %d.\n", i);
            if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
               /* is this viewer showing curSO ? */
               if (SUMA_isVisibleDO(&(SUMAg_SVv[i]), SUMAg_DOv,
                                    curDO)) {
                  if (SUMA_SV_Focus_ADO(&(SUMAg_SVv[i])) == curDO) {
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

/* What happens when you select a graph's node? 
THis is parallel to SUMA_TriInput */
void SUMA_GNodeInput (void *data)
{
   static char FuncName[]={"SUMA_GNodeInput"};
   SUMA_ALL_DO *ado=(SUMA_ALL_DO *)data, *curDO = NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   float fv3[3];
   char str[100];
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_GRAPH_SAUX *GSaux=NULL;
   SUMA_DSET *dset = NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
    
   SUMA_LH("Called");
   
   if (!ado || ado->do_type != GRAPH_LINK_type) {
      SUMA_S_Err("NULL/bad input");
      SUMA_RETURNe;
   }
   dset = SUMA_find_GLDO_Dset((SUMA_GraphLinkDO*)ado);
   GSaux = SDSET_GSAUX(dset);

   SurfCont = SUMA_ADO_Cont(ado);

   if (!(curDO = SUMA_SurfCont_GetcurDOp(SurfCont))) {
      SUMA_S_Err("Failed to get curDOp");
      SUMA_RETURNe;
   }
   TF = SurfCont->FaceTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   i = n % TF->Ni;
   j = n / TF->Ni;
   
   if ((int)TF->num_value[n] < 0 || 
       (int)TF->num_value[n] < dset->Aux->range_node_index[0] ||
       (int)TF->num_value[n] > dset->Aux->range_node_index[1]) {
      SUMA_SLP_Err("Node index must be positive and \n"
                   "less than the number of nodes \n"
                   "forming the surface.\n");
      TF->num_value[n] = GSaux->PR->iAltSel[SUMA_ENODE_0];
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

         for (i=0; i<SUMAg_N_SVv; ++i) {
            SUMA_LHv("Checking viewer %d.\n", i);
            if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
               /* is this viewer showing curSO ? */
               if (SUMA_isVisibleDO(&(SUMAg_SVv[i]), SUMAg_DOv,
                                    curDO)) {
                  if (SUMA_SV_Focus_ADO(&(SUMAg_SVv[i])) == curDO) {
                        /* FocusFace works for this one too */
                        SUMA_JumpFocusFace((char *)cv, 
                                            (void *)(&(SUMAg_SVv[i])));
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
   SUMA_ALL_DO *ado=(SUMA_ALL_DO *)data, *curDO = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_SurfaceViewer *sv=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   int i, n, j;
   void *cv=NULL;
   float fv3[3];
   char str[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   SurfCont = SUMA_ADO_Cont(ado);

   if (!(curDO = SUMA_SurfCont_GetcurDOp(SurfCont))) {
      SUMA_S_Err("Failed to get curDOp");
      SUMA_RETURNe;
   }
   TF = SurfCont->XhairTable;
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
         /* is this viewer showing curDO ? */
         sv = &(SUMAg_SVv[i]);
         if (SUMA_isVisibleDO(sv, SUMAg_DOv, curDO)) {
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


#define SETMEN (setmen > 1 || (setmen && isCur))
#define REDISP (redisplay > 1 || (redisplay && NewDisp))

int SUMA_SetRangeValueNew_one(SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset,
                          SUMA_NUMERICAL_UNITS num_units) 
{
   static char FuncName[]={"SUMA_SetRangeValueNew_one"};
   int NewDisp=0, isCur=0;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_DUMP_TRACE("Who called SUMA_SetRangeValueNew_one?");
   }
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado)) || 
       !SurfCont->SetRangeTable || !reset) {
      SUMA_RETURN(0);
   }
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   
   if (colp && colp == curColPlane) {
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
   
   TF = SurfCont->SetRangeTable;
   if (!TF) setmen = 0; /* can't set nothing */
   
   if (num_units == SUMA_PERC_VALUE_UNITS) {
      float pr[2], prv[2], *Vsort=NULL;
      
      /* Update, because for colorization (I) column, both colp->V 
         and colp->Vperc get clamped by range */
      if (!SUMA_SetOverlay_Vecs(colp, 'V', colp->OptScl->find, 
                                "reset", 1)) {
         SUMA_S_Err("Failed to set overlay vecs");
         SUMA_RETURN(0); 
      }
      
      if (v2 < v1) v2 = v1;
      pr[0] = v1; pr[1] = v2;
         /* some safety checks */
      if (pr[1]<pr[0]) pr[1] = pr[0]+1; 
      if (pr[1] > 100.0) pr[1] = 100.0;
      if (pr[0] < 0.0) pr[0] = 0.0;
      
      #if 0 /* old but works */
      if (!colp->V) {
         SUMA_S_Err("Cannot do percentiles without colp->V");
         SUMA_RETURN(0);
      }
      if (!(Vsort = SUMA_PercRangeVol(colp->V, NULL, colp->N_V, pr, 2, prv, 
                                      NULL, colp->OptScl->MaskZero, NULL))) {
         SUMA_S_Err("Failed to get perc. range");
         SUMA_RETURN(0);                               
      }
      SUMA_ifree(Vsort);
      #else
      prv[0] = SUMA_OverlayPercentile (colp, 'V', pr[0]);
      prv[1] = SUMA_OverlayPercentile (colp, 'V', pr[1]);
      #endif
      
      SUMA_LH("Computed percentiles %f %f-->%f %f", 
               pr[0], pr[1], prv[0], prv[1]);
      v1 = prv[0]; v2 = prv[1];
      
      /* Need to update the value in the table struct */
      if (TF) ++setmen;
      
      /* Remove percentile units */
      if (TF) TF->num_units = SUMA_NO_NUM_UNITS;
   }
   
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
      SUMA_ColorizePlane(curColPlane);
      SUMA_Remixedisplay(ado);
   }   
   
   /* update the Xhair Info block */
   if (curColPlane->OptScl->DoBias != SW_CoordBias_None) {
      SUMA_UpdateNodeNodeField(ado);    
   }
   SUMA_UpdateNodeLblField(ado);
   
   SUMA_RETURN(1);
}

int SUMA_SetRangeValueNew (SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset,
                          SUMA_NUMERICAL_UNITS num_units)
{
   static char FuncName[]={"SUMA_SetRangeValueNew"};
   int NewDisp=0, an=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch range \n", FuncName);
   }
   
   an = SUMA_SetRangeValueNew_one(ado, colp, row, col, 
                                  v1, v2, setmen, 
                                  redisplay, reset, num_units);
   if (an <= 0) SUMA_RETURN(an);
   
   if (ado->do_type == SO_type) {
      SUMA_SurfaceObject *SOC=NULL, *SO = (SUMA_SurfaceObject *)ado;
      SUMA_OVERLAYS *colpC=NULL;
      /* do we have a contralateral SO and overlay? */
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         an = SUMA_SetRangeValueNew_one((SUMA_ALL_DO *)SOC, colpC, row, col, 
                                        v1, v2, 1, 
                                        redisplay, reset, num_units);
      }
   }
   
   SUMA_RETURN(an);
}


void SUMA_cb_SetRangeValue (void *data) 
{
   static char FuncName[]={"SUMA_cb_SetRangeValue"};
   SUMA_SRV_DATA srvdC, *srvd=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_OVERLAYS *colp=NULL;
   int n=-1,row=-1,col=-1, an=0;
   float reset = 0.0;
   void *cv=NULL; 
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch range \n", FuncName);
   }
   if (!(srvd = (SUMA_SRV_DATA *)data)) SUMA_RETURNe;
   ado = srvd->ado; colp = srvd->colp;
   if (!ado) SUMA_RETURNe;
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);

   if (!colp) colp = curColPlane;
   
   TF = SurfCont->SetRangeTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   row = n % TF->Ni;
   col = n / TF->Ni;
   XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s\n", 
                           FuncName, row, col, (char *)cv);
   }

   an = SUMA_SetRangeValueNew(ado, colp, row, col,
                          TF->num_value[n], 0.0,
                          0, 1, &reset, TF->num_units);
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

int SUMA_SetClustValue_one(SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset) 
{
   static char FuncName[]={"SUMA_SetClustValue_one"};
   int NewDisp=0, isCur=0;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_TABLE_FIELD *TF=NULL;
   
   SUMA_ENTRY;
   
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado)) || 
       !SurfCont->SetClustTable || !reset) {
      SUMA_RETURN(0);
   }
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   
   if (colp && colp == curColPlane) {
      isCur=YUP;
   } else {
      isCur=NOPE;
   }
   
   if (!colp) {
      SUMA_RETURN(0); /* not much to do */
   }
   
   
   TF = SurfCont->SetClustTable;
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
         if (isCur && 
              colp->ShowMode > 0 && colp->ShowMode < SW_SurfCont_DsetViewXXX &&
              colp->OptScl->Clusterize) 
                                                            NewDisp = YUP;
         break;
      default:
         SUMA_SL_Err("You make me sick. What's that you're eating?");
         break;
   }
      
   /* Now, you need to redraw the deal */
   if (REDISP) {
      SUMA_ColorizePlane(curColPlane);
      SUMA_Remixedisplay(ado);
   }   
   
   /* update the Xhair Info block */
   if (curColPlane->OptScl->DoBias != SW_CoordBias_None) {
      SUMA_UpdateNodeNodeField(ado);    
   }
   SUMA_UpdateNodeLblField(ado);
   
   SUMA_RETURN(1);
}

int SUMA_SetClustValue (SUMA_ALL_DO *ado, 
                          SUMA_OVERLAYS *colp,
                          int row, int col,
                          float v1, float v2,
                          int setmen, 
                          int redisplay, float *reset)
{
   static char FuncName[]={"SUMA_SetClustValue"};
   int NewDisp=0, an=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n request to switch clust params \n", FuncName);
   }
   
   an = SUMA_SetClustValue_one(ado, colp, row, col, 
                                  v1, v2, setmen, 
                                  redisplay, reset);
   if (an <= 0) SUMA_RETURN(an);
   
   if (ado->do_type == SO_type) {
      SUMA_SurfaceObject *SOC=NULL, *SO = (SUMA_SurfaceObject *)ado;
      SUMA_OVERLAYS *colpC=NULL;
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         an = SUMA_SetClustValue_one((SUMA_ALL_DO *)SOC, colpC, row, col, 
                                        v1, v2, 1, 
                                        redisplay, reset);
      }
   }
   SUMA_RETURN(an);
}


void SUMA_cb_SetClustValue (void *data) 
{
   static char FuncName[]={"SUMA_cb_SetClustValue"};
   SUMA_SRV_DATA srvdC, *srvd=NULL;
   SUMA_OVERLAYS *colp=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
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
   ado = srvd->ado; colp = srvd->colp;
   if (!ado) SUMA_RETURNe;
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!colp) colp = curColPlane;
   
   TF = SurfCont->SetClustTable;
   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   row = n % TF->Ni;
   col = n / TF->Ni;
   XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s\n", 
                           FuncName, row, col, (char *)cv);
   }
   
   an = SUMA_SetClustValue(ado, colp, row, col,
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
   SUMA_LH("ud %d, cell modified %d", ud, TF->cell_modified);
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

void SUMA_set_cmap_options(SUMA_ALL_DO *ado, SUMA_Boolean NewDset,
                           SUMA_Boolean NewMap)
{
   static char FuncName[]={"SUMA_set_cmap_options"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURNe;
   switch (ado->do_type) {
      case SO_type:
         SUMA_set_cmap_options_SO(ado, NewDset,  NewMap);
         SUMA_RETURNe;
         break;
      case GDSET_type:
         SUMA_S_Err("No init for a DO that cannot be dispalyed\n"
                    "without variant");
         SUMA_RETURNe;
      case CDOM_type:
         SUMA_set_cmap_options_CO(ado, NewDset,  NewMap);
         SUMA_RETURNe;
      case GRAPH_LINK_type:
         SUMA_set_cmap_options_GLDO(ado, NewDset, NewMap);
         SUMA_RETURNe;
         break;
      case VO_type:
         SUMA_set_cmap_options_VO(ado, NewDset,  NewMap);
         SUMA_RETURNe;
         break;
      default:
         SUMA_S_Errv("Nothing for type %s\n",
            SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         SUMA_RETURNe;
         break;
   }
   
   SUMA_RETURNe;
}

void SUMA_set_cmap_options_SO(SUMA_ALL_DO *ado, SUMA_Boolean NewDset,
                           SUMA_Boolean NewMap)
{
   static char FuncName[]={"SUMA_set_cmap_options_SO"};
   SUMA_MenuItem  *SwitchInt_Menu = NULL, *SwitchThr_Menu = NULL, 
                  *SwitchBrt_Menu = NULL;
   int N_items, FirstTime;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURNe;
   if (ado->do_type != SO_type) {
      SUMA_S_Err("Should not be here");
      SUMA_RETURNe;
   }
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);

   if (!SurfCont) SUMA_RETURNe;
   if (!SurfCont->opts_form || !SurfCont->opts_rc) SUMA_RETURNe;
   if (!curColPlane) SUMA_RETURNe;
   if (!NewDset && !NewMap && SurfCont->rcvo && SurfCont->rccm) {
      SUMA_SL_Err("Nothing to do");
      SUMA_RETURNe;
   }
   /* row column to contain all switching stuffs */
   if (!SurfCont->rcvo){
      SurfCont->rcvo = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SurfCont->opts_rc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
      NewDset = YUP; /* The first time around */
   } else {
      /* (no need ..) */
      /* XtUnmanageChild(SurfCont->rcvo);  */
   }  
   
   if (NewDset) { /* The intensity / threshold / Brightness block*/
      /* link mode */
      if (!SurfCont->LinkModeMenu->mw[SW_LinkMode]) {
               Widget rc = NULL; /* one pass through this block ONLY */
               rc = XtVaCreateWidget ("rowcolumn",
                  xmRowColumnWidgetClass, SurfCont->rcvo,
                  XmNpacking, XmPACK_TIGHT, 
                  XmNorientation , XmHORIZONTAL ,
                  XmNmarginHeight, 0 ,
                  XmNmarginWidth , 0 ,
                  NULL);
               
               SUMA_LH("Forming map mode menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "IxT", '\0', YUP, LinkMode_Menu, 
                               (void *)ado,  
                               "SurfCont->Dset_Mapping->IxT",
                               "Set I, T selection linking modes.", 
                               SUMA_SurfContHelp_Link,
                               SurfCont->LinkModeMenu);
               XtManageChild (SurfCont->LinkModeMenu->mw[SW_LinkMode]);
               
               XtManageChild(rc);
         }

      if (!SurfCont->rcsw) {
         SurfCont->rcsw = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcvo,
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
         /*XtUnmanageChild(SurfCont->rcsw); */
       }
      if (!SurfCont->rcsw_v1) {
         SurfCont->rcsw_v1 = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcsw,
            XmNpacking, XmPACK_COLUMN, 
            XmNorientation , XmVERTICAL ,
            XmNnumColumns, 1,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      }
      if (!SurfCont->rcsw_v2) {
         SurfCont->rcsw_v2 = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcsw,
            XmNpacking, XmPACK_COLUMN, 
            XmNorientation , XmVERTICAL ,
            XmNnumColumns, 1,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      }
      
      /* Switching triplets */
      SwitchInt_Menu = SUMA_FormSwitchColMenuVector(ado, 0, &N_items);      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: %d items.\n", FuncName, N_items);
      if (SwitchInt_Menu || !N_items) {
         SurfCont->SwitchIntMenu = 
               SUMA_Free_Menu_Widget(SurfCont->SwitchIntMenu);
         SurfCont->SwitchIntMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "I", '\0', YUP, SwitchInt_Menu, 
                           (void *)ado, 
                           "SurfCont->Dset_Mapping->I",
                  "Select Intensity (I) column, aka sub-brick. (BHelp for more)",
                           SUMA_SurfContHelp_SelInt,
                           SurfCont->SwitchIntMenu );
         XtInsertEventHandler( SurfCont->SwitchIntMenu->mw[0] , 
                                             /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) ado ,   /* client data */
                        XtListTail ) ;
         if (LocalHead) 
            SUMA_ShowMeTheChildren(SurfCont->SwitchIntMenu->mw[0]);
         XtManageChild (SurfCont->SwitchIntMenu->mw[0]);
         /* Now destroy the SwitchInt_Menu */
         SwitchInt_Menu = SUMA_FreeMenuVector(SwitchInt_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SurfCont->SwitchIntMenu,
                       curColPlane->OptScl->find+1) ; 
      } else {
         SUMA_SL_Err("NULL SwitchInt_Menu");
      }
      
      SwitchThr_Menu = SUMA_FormSwitchColMenuVector(ado, 1, &N_items);
      if (SwitchThr_Menu || !N_items) {
         SurfCont->SwitchThrMenu = 
               SUMA_Free_Menu_Widget(SurfCont->SwitchThrMenu);
         SurfCont->SwitchThrMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);  
         SUMA_BuildMenuReset(13);         
         SUMA_BuildMenu (SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "T", '\0', YUP, SwitchThr_Menu, 
                           (void *)ado,  
                           "SurfCont->Dset_Mapping->T",
                  "Select Threshold (T) column, aka sub-brick. (BHelp for more)",
                           SUMA_SurfContHelp_SelThr ,    
                           SurfCont->SwitchThrMenu );
         XtInsertEventHandler( SurfCont->SwitchThrMenu->mw[0] ,      
                                       /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) ado ,   /* client data */
                        XtListTail ) ;
         XtManageChild (SurfCont->SwitchThrMenu->mw[0]);
         /* Now destroy the SwitchThr_Menu */
         SwitchThr_Menu = SUMA_FreeMenuVector(SwitchThr_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SurfCont->SwitchThrMenu,
                       curColPlane->OptScl->tind+1); 
      } else {
         SUMA_SL_Err("NULL SwitchThr_Menu");
      }

      SwitchBrt_Menu = SUMA_FormSwitchColMenuVector(ado, 2, &N_items);
      if (SwitchBrt_Menu || !N_items) {
         SurfCont->SwitchBrtMenu = 
               SUMA_Free_Menu_Widget(SurfCont->SwitchBrtMenu);
         SurfCont->SwitchBrtMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "B", '\0', YUP, SwitchBrt_Menu, 
                           (void *)ado,  
                           "SurfCont->Dset_Mapping->B",
               "Select Brightness (B) column, aka sub-brick. (BHelp for more)",
                           SUMA_SurfContHelp_SelBrt,
                           SurfCont->SwitchBrtMenu );
         XtInsertEventHandler( SurfCont->SwitchBrtMenu->mw[0] ,      
                                                /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) ado ,   /* client data */
                        XtListTail ) ;

         XtManageChild (SurfCont->SwitchBrtMenu->mw[0]);
         /* Now destroy the SwitchBrt_Menu */
         SwitchBrt_Menu = SUMA_FreeMenuVector(SwitchBrt_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SurfCont->SwitchBrtMenu,
                       curColPlane->OptScl->bind+1);
      } else {
         SUMA_SL_Err("NULL SwitchBrt_Menu");
      }
      
     if (1) {
     /* put the toggle buttons */
         if (!SurfCont->Int_tb) {
            SurfCont->Int_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SurfCont->rcsw_v2, NULL);
            XtAddCallback (SurfCont->Int_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwitchInt_toggled, ado);
            SUMA_Register_Widget_Help(SurfCont->Int_tb, 1,
                                   "SurfCont->Dset_Mapping->I->v",
                                   "View (ON)/Hide Dset node colors",
                                   SUMA_SurfContHelp_SelIntTgl);
            SUMA_SET_SELECT_COLOR(SurfCont->Int_tb);
         } 
         XmToggleButtonSetState (SurfCont->Int_tb,      
                    curColPlane->ShowMode > 0 ? 1:0 , NOPE);
         
         if (!SurfCont->Thr_tb) {
            SurfCont->Thr_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SurfCont->rcsw_v2, NULL);
            XtAddCallback (SurfCont->Thr_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwitchThr_toggled, ado);
            SUMA_SET_SELECT_COLOR(SurfCont->Thr_tb);
            SUMA_Register_Widget_Help(SurfCont->Thr_tb, 1,
                                   "SurfCont->Dset_Mapping->T->v",  
                                   "Apply (ON)/Ignore thresholding",     
                                   SUMA_SurfContHelp_SelThrTgl);
         }
         if (curColPlane->OptScl->tind >=0) {
            XmToggleButtonSetState (SurfCont->Thr_tb, 
                              curColPlane->OptScl->UseThr, NOPE);
         }else {
            XmToggleButtonSetState (SurfCont->Thr_tb, NOPE, NOPE);
         }
         
         if (!SurfCont->Brt_tb) {
            SurfCont->Brt_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SurfCont->rcsw_v2, NULL);
            XtAddCallback (SurfCont->Brt_tb, 
                     XmNvalueChangedCallback, SUMA_cb_SwitchBrt_toggled, ado);
            SUMA_SET_SELECT_COLOR(SurfCont->Brt_tb);
            SUMA_Register_Widget_Help(SurfCont->Brt_tb, 1,
                                      "SurfCont->Dset_Mapping->B->v", 
                                      "View (ON)/Ignore brightness modulation",
                                      SUMA_SurfContHelp_SelBrtTgl);
         }
         if (curColPlane->OptScl->bind >=0) {
            XmToggleButtonSetState (SurfCont->Brt_tb, 
                     curColPlane->OptScl->UseBrt, NOPE);
         } else {
            XmToggleButtonSetState (SurfCont->Brt_tb, NOPE, NOPE);
         }
      }
      if (!XtIsManaged(SurfCont->rcsw_v1)) 
         XtManageChild (SurfCont->rcsw_v1);
      if (!XtIsManaged(SurfCont->rcsw_v2)) 
         XtManageChild (SurfCont->rcsw_v2);
      if (!XtIsManaged(SurfCont->rcsw)) XtManageChild (SurfCont->rcsw);
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
         "Intensity clipping range (append '%' for percentiles, see BHelp)", 
         "Brightness modulation clipping range (much more with BHelp)", 
         "Brightness modulation factor range (much more with BHelp)" , 
         "Coordinate bias range (much more with BHelp)", NULL};
      char *row_help[]= {  SUMA_SurfContHelp_SetRngTbl_r0, 
                           SUMA_SurfContHelp_SetRngTbl_r1,
                           SUMA_SurfContHelp_SetRngTbl_r2, 
                           SUMA_SurfContHelp_SetRngTbl_r3, 
                           SUMA_SurfContHelp_SetRngTbl_r4, NULL};
      if (!SurfCont->rccm) {
         SurfCont->rccm = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcvo,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0,
            XmNmarginWidth, 0,
            NULL);
         NewMap = YUP; /* the first time around */
      }
       
      if (NewMap) {/* new colormaps */
         SUMA_Register_Widget_Help(SurfCont->rccm, 0,
                        "SurfContCont->Dset_Mapping->SetRangeTable",
                        "Node colorization parameters",
                        "Set parameters for mapping node data onto color scale");
         SUMA_LH("NewMap set");
         if (!SurfCont->SetRangeTable->cells) {
            int colw[3] = { 1, 8, 8 };
            SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)calloc(1,sizeof(SUMA_SRV_DATA));
            srvd->ado = ado; srvd->colp = NULL;               
            /* create the widgets for the range table */
            SUMA_LH("Creating table");
            SUMA_CreateTable( SurfCont->rccm,
                           5, 3, 
                           "SurfCont->Dset_Mapping->SetRangeTable",
                           row_tit, col_tit,  
                           row_hint, col_hint,  
                           row_help, col_help,  
                           colw, YUP, SUMA_float, 
                           SUMA_cb_SetRangeValue, (void *)srvd,
                           SUMA_SetRangeTableTit_EV, NULL,
                           NULL, NULL,  
                           SurfCont->SetRangeTable);
         }
         if (!SurfCont->CoordBiasMenu->mw[SW_CoordBias]) {
               Widget rc = NULL; /* one pass through this block ONLY */
               rc = XtVaCreateWidget ("rowcolumn",
                  xmRowColumnWidgetClass, SurfCont->rccm,
                  XmNpacking, XmPACK_TIGHT, 
                  XmNorientation , XmHORIZONTAL ,
                  XmNmarginHeight, 0 ,
                  XmNmarginWidth , 0 ,
                  NULL);
               
               SUMA_LH("Forming map mode menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "Col", '\0', YUP, CmapMode_Menu, 
                               (void *)ado,  
                               "SurfCont->Dset_Mapping->Col",  
                               "Switch between color mapping modes.", 
                               SUMA_SurfContHelp_Col,
                               SurfCont->CmapModeMenu);
               XtManageChild (SurfCont->CmapModeMenu->mw[SW_CmapMode]);
               
               SUMA_LH("Forming new bias menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "Bias", '\0', YUP, CoordBias_Menu, 
                               (void *)ado, 
                               "SurfCont->Dset_Mapping->Bias",  
                               "Coordinate bias direction", 
                               SUMA_SurfContHelp_Bias, 
                               SurfCont->CoordBiasMenu);
               XtManageChild (SurfCont->CoordBiasMenu->mw[SW_CoordBias]);
               
               XtManageChild(rc);
         }
            
         if (!SurfCont->rccm_swcmap) {
            SUMA_LH("Creating rccm_swcmap");
            SurfCont->rccm_swcmap =  XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SurfCont->rccm,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0 ,
               XmNmarginWidth , 0 ,
               NULL);
         }

         {
            SUMA_CreateUpdatableCmapMenu(ado); 

            #if 0
               /* Not any more, menu is now stuck in its own rc */
               /* the loader, needs to be recreated with colormap menu  */
               if (SurfCont->CmapLoad_pb) { 
                  XtDestroyWidget(SurfCont->CmapLoad_pb); 
                  SurfCont->CmapLoad_pb = NULL;
               }
            #endif
            if (!SurfCont->CmapLoad_pb) { 
               SUMA_LH("Forming CmapLoad button");
               SurfCont->CmapLoad_pb = XtVaCreateManagedWidget ("New", 
                                 xmPushButtonWidgetClass, 
                                 SurfCont->rccm_swcmap, 
                                 NULL);
               XtAddCallback (SurfCont->CmapLoad_pb, XmNactivateCallback, 
                              SUMA_cb_Cmap_Load, (XtPointer) ado);
               SUMA_Register_Widget_Help(SurfCont->CmapLoad_pb , 1,
                                         "SurfCont->Dset_Mapping->Cmp->New",
                                         "Load new colormap",
                                         SUMA_SurfContHelp_CmpNew);
            }
         } /* new colormaps */
         if (!XtIsManaged(SurfCont->rccm_swcmap)) 
                     XtManageChild (SurfCont->rccm_swcmap); 
      }
      
      /* Set the CoordBias's menu history to reflect current setting */
      SUMA_LH("Updating Link Mode History");
      SUMA_Set_Menu_Widget( SurfCont->LinkModeMenu,
                     curColPlane->LinkMode); 
      
      SUMA_LH("Working the lock stuff ...");
      /* You'll need to fix the table's locking widget colors */
      if ( SurfCont->IntRangeLocked == 
               curColPlane->OptScl->AutoIntRange) {
         SUMA_LH("   Do the Int");
         /* need to put things in sync */
         SurfCont->IntRangeLocked = !SurfCont->IntRangeLocked;
         MCW_invert_widget_sync(SurfCont->SetRangeTable->cells[1],0);
      }
      if ( SurfCont->BrtRangeLocked == 
               curColPlane->OptScl->AutoBrtRange) {
         SUMA_LH("   Do the Brt");
         /* need to put things in sync */
         SurfCont->BrtRangeLocked = !SurfCont->BrtRangeLocked;
         MCW_invert_widget_sync(SurfCont->SetRangeTable->cells[2],0);
      } 

      /* Set the CoordBias's menu history to reflect current setting */
      SUMA_LH("Updating CoorBias chooser History");
      SUMA_Set_Menu_Widget( SurfCont->CoordBiasMenu,
                     curColPlane->OptScl->DoBias); 
 
      /* Set the Col's menu history to reflect current setting */
      SUMA_LH("Updating Col chooser History");
      SUMA_Set_Menu_Widget( SurfCont->CmapModeMenu,
                     curColPlane->OptScl->interpmode); 
 
      /* add the selectors for symmetric range and absolute threshold */
      if (!SurfCont->AbsThresh_tb) {
         Widget rc;
         rc = XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SurfCont->rccm,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0 ,
               XmNmarginWidth , 0 ,
               NULL);
         /* create the absolute threshold toggle button */
         SurfCont->AbsThresh_tb = XtVaCreateManagedWidget("|T|", 
               xmToggleButtonWidgetClass, rc, 
               NULL);
         XtAddCallback (SurfCont->AbsThresh_tb, 
               XmNvalueChangedCallback, SUMA_cb_AbsThresh_tb_toggled, ado);
         SUMA_Register_Widget_Help(SurfCont->AbsThresh_tb , 1,
                                   "SurfCont->Dset_Mapping->abs_T",
                                   "Absolute threshold ON/OFF",
                                   SUMA_SurfContHelp_AbsThr );
         
         SUMA_SET_SELECT_COLOR(SurfCont->AbsThresh_tb);
         
         /* create the symmetric range toggle button */
         SurfCont->SymIrange_tb = XtVaCreateManagedWidget("sym I", 
               xmToggleButtonWidgetClass, rc, NULL);
         XtAddCallback (SurfCont->SymIrange_tb, 
               XmNvalueChangedCallback, SUMA_cb_SymIrange_tb_toggled, ado);
         SUMA_Register_Widget_Help(SurfCont->SymIrange_tb, 1,
                                   "SurfCont->Dset_Mapping->sym_I",
                                   "Intensity range symmetry about 0 ",
                                   SUMA_SurfContHelp_Isym);
         SUMA_SET_SELECT_COLOR(SurfCont->SymIrange_tb);
         
         /* add a button for zero masking */
         SurfCont->ShowZero_tb = XtVaCreateManagedWidget("shw 0", 
               xmToggleButtonWidgetClass, rc, NULL);
         XtAddCallback (SurfCont->ShowZero_tb, 
               XmNvalueChangedCallback, SUMA_cb_ShowZero_tb_toggled, ado);
         SUMA_Register_Widget_Help(SurfCont->ShowZero_tb, 1, 
                                   "SurfCont->Dset_Mapping->shw_0",
                                   "Color masking of nodes with intensity = 0 ",
                                   SUMA_SurfContHelp_Shw0);
         SUMA_SET_SELECT_COLOR(SurfCont->ShowZero_tb);
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
         if (!SurfCont->rcclust) {
            SurfCont->rcclust = XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SurfCont->opts_form,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0,
               XmNmarginWidth, 0,
               XmNrightAttachment, XmATTACH_FORM , 
               XmNleftAttachment,  XmATTACH_NONE,
               XmNtopAttachment, XmATTACH_WIDGET ,
               XmNtopWidget, SurfCont->opts_rc,
               NULL);
            FirstTime = YUP; /* the first time around */
         } else {
            FirstTime = NOPE;
         }

         if (FirstTime) {/* new clust */
            SUMA_LH("FirstTime set");
            if (!SurfCont->SetClustTable->cells) {
               int colw[3] = { 4, 6, 6 };
               SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)
                                 calloc(1,sizeof(SUMA_SRV_DATA));
               srvd->ado = ado; srvd->colp = NULL;               
               /* create the widgets for the range table */
               SUMA_LH("Creating table");
               SUMA_CreateTable( SurfCont->rcclust,
                              2, 3, 
                              "SurfCont->Dset_Mapping->Clst",
                              row_tit, col_tit,  
                              row_hint, col_hint,  
                              row_help, col_help,  
                              colw, YUP, SUMA_float, 
                              SUMA_cb_SetClustValue, (void *)srvd,
                              SUMA_SetClustTableTit_EV, NULL,
                              SUMA_SetClustTableCell_EV, NULL,  
                              SurfCont->SetClustTable);
            }
            
            if (curColPlane->OptScl) {
                SUMA_SetTableTitleButton1(SurfCont->SetClustTable, 1,0,                                       curColPlane->OptScl->Clusterize);
            }
         }
         SUMA_LH("Managerial");

         if (!XtIsManaged(SurfCont->rcclust)) 
                  XtManageChild (SurfCont->rcclust);

      }/* The clustering options */

      /* do the initialization */
      SUMA_InitClustTable(ado); /* init the clust table values*/
      
      /* This button is NOT appropriate for anything other than 
         SUMA_ABS_LESS_THAN and SUMA_LESS_THAN
         Probably need separate button ormenu system for other
         thresholding modes */
      if (curColPlane->OptScl->ThrMode == SUMA_ABS_LESS_THAN) {
         XmToggleButtonSetState( SurfCont->AbsThresh_tb, True, NOPE);
      } else if (curColPlane->OptScl->ThrMode == SUMA_LESS_THAN) {
         XmToggleButtonSetState( SurfCont->AbsThresh_tb, False, NOPE);
      } else {
         SUMA_S_Err("Not ready to handle ThrModeR of %d yet", 
                     curColPlane->OptScl->ThrMode);
      }
      if (!curColPlane->SymIrange) {
         XmToggleButtonSetState( SurfCont->SymIrange_tb, False, NOPE);
      } else {
         XmToggleButtonSetState( SurfCont->SymIrange_tb, True, NOPE);
      }
      if (!curColPlane->OptScl->MaskZero) {
         XmToggleButtonSetState( SurfCont->ShowZero_tb, True, NOPE);
      } else {
         XmToggleButtonSetState( SurfCont->ShowZero_tb, False, NOPE);
      }
      
      if (!XtIsManaged(SurfCont->rccm)) XtManageChild (SurfCont->rccm);
   
   }/*  The Color map range and selector block */
   
   if (1){ /* The Range values block*/
      char *col_tit[]=  {  " ", "Min", "Node", "Max", "Node", NULL};
      char *col_hint[]= {  "Full range of values in Dset", 
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
      char *row_hint[]= {  "Full range of values in Dset", 
                           "Range of values in intensity (I) column", 
                           "Range of values in threshold (T) column", 
                           "Range of values in brightness (B) column", NULL};
      char *row_help[]= {  SUMA_SurfContHelp_RangeTbl_c0, 
                           SUMA_SurfContHelp_RangeTbl_r1, 
                           SUMA_SurfContHelp_RangeTbl_r2, 
                           SUMA_SurfContHelp_RangeTbl_r3, NULL};
      if (!SurfCont->rcswr) {
         SurfCont->rcswr = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->opts_form,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNrightAttachment, XmATTACH_FORM , 
            XmNleftAttachment,  XmATTACH_NONE,
            XmNtopAttachment, XmATTACH_WIDGET ,
            XmNtopWidget, SurfCont->rcclust,
            NULL);
      }
      
      if (!SurfCont->RangeTable->cells) {
         int colw[5] = { 1, 6, 6, 6, 6 };
         /* create the widgets for the range table */
         SUMA_CreateTable( SurfCont->rcswr,
                           4, 5, 
                           "SurfCont->Dset_Mapping->RangeTable",
                           row_tit, col_tit,  
                           row_hint, col_hint,  
                           row_help, col_help,  
                           colw, NOPE, SUMA_string, 
                           NULL, NULL,
                           NULL, NULL,  
                           SUMA_RangeTableCell_EV, (void *)ado, 
                           SurfCont->RangeTable);
      }

      if (!XtIsManaged(SurfCont->rcswr)) XtManageChild (SurfCont->rcswr);
   } /* The Range values block */
         
   if (NewDset) {
      /* initialize tables of range values */
      SUMA_InitRangeTable(ado, 2);
   }
   
   if (!XtIsManaged(SurfCont->rcvo)) XtManageChild (SurfCont->rcvo);
   SUMA_FORCE_SCALE_HEIGHT(SUMA_ADO_Cont(ado)); 

   SUMA_RETURNe;
}

void SUMA_set_cmap_options_CO(SUMA_ALL_DO *ado, SUMA_Boolean NewDset,
                           SUMA_Boolean NewMap)
{
   static char FuncName[]={"SUMA_set_cmap_options_CO"};
   SUMA_MenuItem  *SwitchInt_Menu = NULL, *SwitchThr_Menu = NULL, 
                  *SwitchBrt_Menu = NULL;
   int N_items, FirstTime;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURNe;
   if (ado->do_type != CDOM_type) {
      SUMA_S_Err("Should not be here");
      SUMA_RETURNe;
   }
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);

   if (!SurfCont) SUMA_RETURNe;
   
   SUMA_S_Err("Nothing here still");
     
   SUMA_RETURNe;
}

void SUMA_set_cmap_options_VO(SUMA_ALL_DO *ado, SUMA_Boolean NewDset,
                           SUMA_Boolean NewMap)
{
   static char FuncName[]={"SUMA_set_cmap_options_VO"};
   SUMA_MenuItem  *SwitchInt_Menu = NULL, *SwitchThr_Menu = NULL, 
                  *SwitchBrt_Menu = NULL;
   int N_items, FirstTime;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURNe;
   if (ado->do_type != VO_type) {
      SUMA_S_Err("Should not be here");
      SUMA_RETURNe;
   }
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);

   if (!SurfCont) SUMA_RETURNe;
   if (!SurfCont->opts_form || !SurfCont->opts_rc) SUMA_RETURNe;
   if (!curColPlane) SUMA_RETURNe;
   if (!NewDset && !NewMap && SurfCont->rcvo && SurfCont->rccm) {
      SUMA_SL_Err("Nothing to do");
      SUMA_RETURNe;
   }
   /* row column to contain all switching stuffs */
   if (!SurfCont->rcvo){
      SurfCont->rcvo = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SurfCont->opts_rc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
      NewDset = YUP; /* The first time around */
   } else {
      /* (no need ..) */
      /* XtUnmanageChild(SurfCont->rcvo);  */
   }  
   
   if (NewDset) { /* The intensity / threshold / Brightness block*/
      /* link mode */
      if (!SurfCont->LinkModeMenu->mw[SW_LinkMode]) {
               Widget rc = NULL; /* one pass through this block ONLY */
               rc = XtVaCreateWidget ("rowcolumn",
                  xmRowColumnWidgetClass, SurfCont->rcvo,
                  XmNpacking, XmPACK_TIGHT, 
                  XmNorientation , XmHORIZONTAL ,
                  XmNmarginHeight, 0 ,
                  XmNmarginWidth , 0 ,
                  NULL);
               
               SUMA_LH("Forming map mode menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "IxT", '\0', YUP, LinkMode_Menu, 
                               (void *)ado,  
                               "VolCont->Dset_Mapping->IxT",
                               "Set I, T selection linking modes.", 
                               SUMA_SurfContHelp_Link,
                               SurfCont->LinkModeMenu);
               XtManageChild (SurfCont->LinkModeMenu->mw[SW_LinkMode]);
               
               XtManageChild(rc);
         }

      if (!SurfCont->rcsw) {
         SurfCont->rcsw = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcvo,
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
         /*XtUnmanageChild(SurfCont->rcsw); */
       }
      if (!SurfCont->rcsw_v1) {
         SurfCont->rcsw_v1 = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcsw,
            XmNpacking, XmPACK_COLUMN, 
            XmNorientation , XmVERTICAL ,
            XmNnumColumns, 1,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      }
      if (!SurfCont->rcsw_v2) {
         SurfCont->rcsw_v2 = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcsw,
            XmNpacking, XmPACK_COLUMN, 
            XmNorientation , XmVERTICAL ,
            XmNnumColumns, 1,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      }
      
      /* Switching triplets */
      SwitchInt_Menu = SUMA_FormSwitchColMenuVector(ado, 0, &N_items);      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: %d items.\n", FuncName, N_items);
      if (SwitchInt_Menu || !N_items) {
         SurfCont->SwitchIntMenu = 
               SUMA_Free_Menu_Widget(SurfCont->SwitchIntMenu);
         SurfCont->SwitchIntMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "I", '\0', YUP, SwitchInt_Menu, 
                           (void *)ado, 
                           "VolCont->Dset_Mapping->I",
                  "Select Intensity (I) column, aka sub-brick. (BHelp for more)",
                           SUMA_SurfContHelp_SelInt,
                           SurfCont->SwitchIntMenu );
         XtInsertEventHandler( SurfCont->SwitchIntMenu->mw[0] , 
                                             /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) ado ,   /* client data */
                        XtListTail ) ;
         if (LocalHead) 
            SUMA_ShowMeTheChildren(SurfCont->SwitchIntMenu->mw[0]);
         XtManageChild (SurfCont->SwitchIntMenu->mw[0]);
         /* Now destroy the SwitchInt_Menu */
         SwitchInt_Menu = SUMA_FreeMenuVector(SwitchInt_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SurfCont->SwitchIntMenu,
                       curColPlane->OptScl->find+1) ; 
      } else {
         SUMA_SL_Err("NULL SwitchInt_Menu");
      }
      
      SwitchThr_Menu = SUMA_FormSwitchColMenuVector(ado, 1, &N_items);
      if (SwitchThr_Menu || !N_items) {
         SurfCont->SwitchThrMenu = 
               SUMA_Free_Menu_Widget(SurfCont->SwitchThrMenu);
         SurfCont->SwitchThrMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);  
         SUMA_BuildMenuReset(13);         
         SUMA_BuildMenu (SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "T", '\0', YUP, SwitchThr_Menu, 
                           (void *)ado,  
                           "VolCont->Dset_Mapping->T",
               "Select Threshold (T) column, aka sub-brick.  (BHelp for more)",
                           SUMA_SurfContHelp_SelThr ,    
                           SurfCont->SwitchThrMenu );
         XtInsertEventHandler( SurfCont->SwitchThrMenu->mw[0] ,      
                                       /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) ado ,   /* client data */
                        XtListTail ) ;
         XtManageChild (SurfCont->SwitchThrMenu->mw[0]);
         /* Now destroy the SwitchThr_Menu */
         SwitchThr_Menu = SUMA_FreeMenuVector(SwitchThr_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SurfCont->SwitchThrMenu,
                       curColPlane->OptScl->tind+1); 
      } else {
         SUMA_SL_Err("NULL SwitchThr_Menu");
      }

      SwitchBrt_Menu = SUMA_FormSwitchColMenuVector(ado, 2, &N_items);
      if (SwitchBrt_Menu || !N_items) {
         SurfCont->SwitchBrtMenu = 
               SUMA_Free_Menu_Widget(SurfCont->SwitchBrtMenu);
         SurfCont->SwitchBrtMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "B", '\0', YUP, SwitchBrt_Menu, 
                           (void *)ado,  
                           "VolCont->Dset_Mapping->B",
               "Select Brightness (B) column, aka sub-brick. (BHelp for more)",
                           SUMA_SurfContHelp_SelBrt,
                           SurfCont->SwitchBrtMenu );
         XtInsertEventHandler( SurfCont->SwitchBrtMenu->mw[0] ,      
                                                /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) ado ,   /* client data */
                        XtListTail ) ;

         XtManageChild (SurfCont->SwitchBrtMenu->mw[0]);
         /* Now destroy the SwitchBrt_Menu */
         SwitchBrt_Menu = SUMA_FreeMenuVector(SwitchBrt_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SurfCont->SwitchBrtMenu,
                       curColPlane->OptScl->bind+1);
      } else {
         SUMA_SL_Err("NULL SwitchBrt_Menu");
      }
      
     if (1) {
     /* put the toggle buttons */
         if (!SurfCont->Int_tb) {
            SurfCont->Int_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SurfCont->rcsw_v2, NULL);
            XtAddCallback (SurfCont->Int_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwitchInt_toggled, ado);
            SUMA_Register_Widget_Help(SurfCont->Int_tb, 1,
                              "VolCont->Dset_Mapping->I->v",
                              "View (ON)/Hide volume voxel colors",
                              SUMA_VolContHelp_SelIntTgl);

            SUMA_SET_SELECT_COLOR(SurfCont->Int_tb);
         } 
         XmToggleButtonSetState (SurfCont->Int_tb,      
                    curColPlane->ShowMode > 0 ? 1:0 , NOPE);
         
         if (!SurfCont->Thr_tb) {
            SurfCont->Thr_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SurfCont->rcsw_v2, NULL);
            XtAddCallback (SurfCont->Thr_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwitchThr_toggled, ado);
            SUMA_SET_SELECT_COLOR(SurfCont->Thr_tb);
            SUMA_Register_Widget_Help(SurfCont->Thr_tb, 1,
                              "VolCont->Dset_Mapping->T->v",  
                              "Apply (ON)/Ignore thresholding",
                              SUMA_SurfContHelp_SelThrTgl);
         }
         if (curColPlane->OptScl->tind >=0) {
            XmToggleButtonSetState (SurfCont->Thr_tb, 
                              curColPlane->OptScl->UseThr, NOPE);
         }else {
            XmToggleButtonSetState (SurfCont->Thr_tb, NOPE, NOPE);
         }
         
         if (!SurfCont->Brt_tb) {
            SurfCont->Brt_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SurfCont->rcsw_v2, NULL);
            XtAddCallback (SurfCont->Brt_tb, 
                     XmNvalueChangedCallback, SUMA_cb_SwitchBrt_toggled, ado);
            SUMA_SET_SELECT_COLOR(SurfCont->Brt_tb);
            SUMA_Register_Widget_Help(SurfCont->Brt_tb, 1,
                     "VolCont->Dset_Mapping->B->v",   
                     "View (ON)/Ignore brightness modulation",
                     SUMA_SurfContHelp_SelBrtTgl);
         }
         if (curColPlane->OptScl->bind >=0) {
            XmToggleButtonSetState (SurfCont->Brt_tb, 
                     curColPlane->OptScl->UseBrt, NOPE);
         } else {
            XmToggleButtonSetState (SurfCont->Brt_tb, NOPE, NOPE);
         }
      }
      if (!XtIsManaged(SurfCont->rcsw_v1)) 
         XtManageChild (SurfCont->rcsw_v1);
      if (!XtIsManaged(SurfCont->rcsw_v2)) 
         XtManageChild (SurfCont->rcsw_v2);
      if (!XtIsManaged(SurfCont->rcsw)) XtManageChild (SurfCont->rcsw);
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
         "Intensity clipping range (append '%' for percentiles, see BHelp)", 
         "Brightness modulation clipping range (much more with BHelp)", 
         "Brightness modulation factor range (much more with BHelp)" , 
         "Coordinate bias range (much more with BHelp)", NULL};
      char *row_help[]= {  SUMA_SurfContHelp_SetRngTbl_r0, 
                           SUMA_SurfContHelp_SetRngTbl_r1,
                           SUMA_SurfContHelp_SetRngTbl_r2, 
                           SUMA_SurfContHelp_SetRngTbl_r3, 
                           SUMA_SurfContHelp_SetRngTbl_r4, NULL};
      if (!SurfCont->rccm) {
         SurfCont->rccm = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcvo,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0,
            XmNmarginWidth, 0,
            NULL);
         NewMap = YUP; /* the first time around */
      }
       
      if (NewMap) {/* new colormaps */
         SUMA_LH("NewMap set");
         SUMA_Register_Widget_Help(SurfCont->rccm, 1, 
                           "VolCont->Dset_Mapping->SetRangeTable",
                           "Voxel colorization parameters",
                           "Set parameters for mapping data onto color scale");
         if (!SurfCont->SetRangeTable->cells) {
            int colw[3] = { 1, 8, 8 };
            SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)calloc(1,sizeof(SUMA_SRV_DATA));
            srvd->ado = ado; srvd->colp = NULL;               
            /* create the widgets for the range table */
            SUMA_LH("Creating table");
            SUMA_CreateTable( SurfCont->rccm,
                           5, 3, 
                           "VolCont->Dset_Mapping->SetRangeTable",
                           row_tit, col_tit,  
                           row_hint, col_hint,  
                           row_help, col_help,  
                           colw, YUP, SUMA_float, 
                           SUMA_cb_SetRangeValue, (void *)srvd,
                           SUMA_SetRangeTableTit_EV, NULL,
                           NULL, NULL,  
                           SurfCont->SetRangeTable);
         }
         if (!SurfCont->CoordBiasMenu->mw[SW_CoordBias]) {
               Widget rc = NULL; /* one pass through this block ONLY */
               rc = XtVaCreateWidget ("rowcolumn",
                  xmRowColumnWidgetClass, SurfCont->rccm,
                  XmNpacking, XmPACK_TIGHT, 
                  XmNorientation , XmHORIZONTAL ,
                  XmNmarginHeight, 0 ,
                  XmNmarginWidth , 0 ,
                  NULL);
               
               SUMA_LH("Forming map mode menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "Col", '\0', YUP, CmapMode_Menu, 
                               (void *)ado,  
                               "VolCont->Dset_Mapping->Col",
                               "Switch between color mapping modes.", 
                               SUMA_SurfContHelp_Col,
                               SurfCont->CmapModeMenu);
               XtManageChild (SurfCont->CmapModeMenu->mw[SW_CmapMode]);
               
               #if 0
               SUMA_LH("Forming new bias menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "Bias", '\0', YUP, CoordBias_Menu, 
                               (void *)ado, 
                               "VolCont->Dset_Mapping->Bias",
                               "Coordinate bias direction", 
                               SUMA_SurfContHelp_Bias, 
                               SurfCont->CoordBiasMenu);
               XtManageChild (SurfCont->CoordBiasMenu->mw[SW_CoordBias]);
               #endif
               XtManageChild(rc);
         }
            
         if (!SurfCont->rccm_swcmap) {
            SUMA_LH("Creating rccm_swcmap");
            SurfCont->rccm_swcmap =  XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SurfCont->rccm,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0 ,
               XmNmarginWidth , 0 ,
               NULL);
         }

         {
            SUMA_CreateUpdatableCmapMenu(ado); 

            #if 0
               /* Not any more, menu is now stuck in its own rc */
               /* the loader, needs to be recreated with colormap menu  */
               if (SurfCont->CmapLoad_pb) { 
                  XtDestroyWidget(SurfCont->CmapLoad_pb); 
                  SurfCont->CmapLoad_pb = NULL;
               }
            #endif
            if (!SurfCont->CmapLoad_pb) { 
               SUMA_LH("Forming CmapLoad button");
               SurfCont->CmapLoad_pb = XtVaCreateManagedWidget ("New", 
                                 xmPushButtonWidgetClass, 
                                 SurfCont->rccm_swcmap, 
                                 NULL);
               XtAddCallback (SurfCont->CmapLoad_pb, XmNactivateCallback, 
                              SUMA_cb_Cmap_Load, (XtPointer) ado);
               SUMA_Register_Widget_Help(SurfCont->CmapLoad_pb , 1,
                                 "VolCont->Dset_Mapping->Cmp->New",
                                 "Load new colormap",
                                 SUMA_SurfContHelp_CmpNew);
            }
         } /* new colormaps */
         if (!XtIsManaged(SurfCont->rccm_swcmap)) 
                     XtManageChild (SurfCont->rccm_swcmap); 
      }
      
      /* Set the CoordBias's menu history to reflect current setting */
      SUMA_LH("Updating Link Mode History");
      SUMA_Set_Menu_Widget( SurfCont->LinkModeMenu,
                     curColPlane->LinkMode); 
      
      SUMA_LH("Working the lock stuff ...");
      /* You'll need to fix the table's locking widget colors */
      if ( SurfCont->IntRangeLocked == 
               curColPlane->OptScl->AutoIntRange) {
         SUMA_LH("   Do the Int");
         /* need to put things in sync */
         SurfCont->IntRangeLocked = !SurfCont->IntRangeLocked;
         MCW_invert_widget_sync(SurfCont->SetRangeTable->cells[1], 0);
      }
      if ( SurfCont->BrtRangeLocked == 
               curColPlane->OptScl->AutoBrtRange) {
         SUMA_LH("   Do the Brt");
         /* need to put things in sync */
         SurfCont->BrtRangeLocked = !SurfCont->BrtRangeLocked;
         MCW_invert_widget_sync(SurfCont->SetRangeTable->cells[2], 0);
      } 

      /* Set the CoordBias's menu history to reflect current setting */
      SUMA_LH("Updating CoorBias chooser History");
      SUMA_Set_Menu_Widget( SurfCont->CoordBiasMenu,
                     curColPlane->OptScl->DoBias); 
 
      /* Set the Col's menu history to reflect current setting */
      SUMA_LH("Updating Col chooser History");
      SUMA_Set_Menu_Widget( SurfCont->CmapModeMenu,
                     curColPlane->OptScl->interpmode); 
 
      /* add the selectors for symmetric range and absolute threshold */
      if (!SurfCont->AbsThresh_tb) {
         Widget rc;
         rc = XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SurfCont->rccm,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0 ,
               XmNmarginWidth , 0 ,
               NULL);
         /* create the absolute threshold toggle button */
         SurfCont->AbsThresh_tb = XtVaCreateManagedWidget("|T|", 
               xmToggleButtonWidgetClass, rc, 
               NULL);
         XtAddCallback (SurfCont->AbsThresh_tb, 
               XmNvalueChangedCallback, SUMA_cb_AbsThresh_tb_toggled, ado);
         SUMA_Register_Widget_Help(SurfCont->AbsThresh_tb , 1,
                           "VolCont->Dset_Mapping->abs_T",
                           "Absolute threshold ON/OFF",
                           SUMA_SurfContHelp_AbsThr );
         
         SUMA_SET_SELECT_COLOR(SurfCont->AbsThresh_tb);
         
         /* create the symmetric range toggle button */
         SurfCont->SymIrange_tb = XtVaCreateManagedWidget("sym I", 
               xmToggleButtonWidgetClass, rc, NULL);
         XtAddCallback (SurfCont->SymIrange_tb, 
               XmNvalueChangedCallback, SUMA_cb_SymIrange_tb_toggled, ado);
         SUMA_Register_Widget_Help(SurfCont->SymIrange_tb, 1,
                           "VolCont->Dset_Mapping->sym_I",
                           "Intensity range symmetry about 0 ",
                           SUMA_SurfContHelp_Isym);
         SUMA_SET_SELECT_COLOR(SurfCont->SymIrange_tb);
         
         /* add a button for zero masking */
         SurfCont->ShowZero_tb = XtVaCreateManagedWidget("shw 0", 
               xmToggleButtonWidgetClass, rc, NULL);
         XtAddCallback (SurfCont->ShowZero_tb, 
               XmNvalueChangedCallback, SUMA_cb_ShowZero_tb_toggled, ado);
         SUMA_Register_Widget_Help(SurfCont->ShowZero_tb, 1,
               "VolCont->Dset_Mapping->shw_0",
               "Color masking of nodes with intensity = 0 ",
               SUMA_SurfContHelp_Shw0);
         SUMA_SET_SELECT_COLOR(SurfCont->ShowZero_tb);
         XtManageChild (rc);
      }
      
      if (0) {/* The clustering options - NOT YET IMPLEMENTED FOR VOLUMES - 
                 Interface OK, but callbacks do not handle volumes yet */
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
         if (!SurfCont->rcclust) {
            SurfCont->rcclust = XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SurfCont->opts_form,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0,
               XmNmarginWidth, 0,
               XmNrightAttachment, XmATTACH_FORM , 
               XmNleftAttachment,  XmATTACH_NONE,
               XmNtopAttachment, XmATTACH_WIDGET ,
               XmNtopWidget, SurfCont->opts_rc,
               NULL);
            FirstTime = YUP; /* the first time around */
         } else {
            FirstTime = NOPE;
         }

         if (FirstTime) {/* new clust */
            SUMA_LH("FirstTime set");
            if (!SurfCont->SetClustTable->cells) {
               int colw[3] = { 4, 6, 6 };
               SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)
                                 calloc(1,sizeof(SUMA_SRV_DATA));
               srvd->ado = ado; srvd->colp = NULL;               
               /* create the widgets for the range table */
               SUMA_LH("Creating table");
               SUMA_CreateTable( SurfCont->rcclust,
                              2, 3, 
                              "VolCont->Dset_Mapping->Clst",
                              row_tit, col_tit,  
                              row_hint, col_hint,  
                              row_help, col_help,  
                              colw, YUP, SUMA_float, 
                              SUMA_cb_SetClustValue, (void *)srvd,
                              SUMA_SetClustTableTit_EV, NULL,
                              SUMA_SetClustTableCell_EV, NULL,  
                              SurfCont->SetClustTable);
            }
            
            if (curColPlane->OptScl) {
                SUMA_SetTableTitleButton1(SurfCont->SetClustTable, 1,0,                                       curColPlane->OptScl->Clusterize);
            }
         }
         SUMA_LH("Managerial");

         if (!XtIsManaged(SurfCont->rcclust)) 
                  XtManageChild (SurfCont->rcclust);

         /* do the initialization */
         SUMA_InitClustTable(ado); /* init the clust table values*/
      }/* The clustering options */

      
      if (curColPlane->OptScl->ThrMode == SUMA_ABS_LESS_THAN) {
         XmToggleButtonSetState( SurfCont->AbsThresh_tb, True, NOPE);
      } else if (curColPlane->OptScl->ThrMode == SUMA_LESS_THAN){
         XmToggleButtonSetState( SurfCont->AbsThresh_tb, False, NOPE);
      } else {
         SUMA_S_Err("Not ready to handle ThrModeR of %d yet", 
                     curColPlane->OptScl->ThrMode);
      }
      if (!curColPlane->SymIrange) {
         XmToggleButtonSetState( SurfCont->SymIrange_tb, False, NOPE);
      } else {
         XmToggleButtonSetState( SurfCont->SymIrange_tb, True, NOPE);
      }
      if (!curColPlane->OptScl->MaskZero) {
         XmToggleButtonSetState( SurfCont->ShowZero_tb, True, NOPE);
      } else {
         XmToggleButtonSetState( SurfCont->ShowZero_tb, False, NOPE);
      }
      
      if (!XtIsManaged(SurfCont->rccm)) XtManageChild (SurfCont->rccm);
   
   }/*  The Color map range and selector block */
   
   { /* The Range values block*/
      char *col_tit[]=  {  " ", "Min", "Vox", "Max", "Vox", NULL};
      char *col_hint[]= {  "Full range of values in Dset", 
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
      char *row_hint[]= {  "Full range of values in Dset", 
                           "Range of values in intensity (I) column", 
                           "Range of values in threshold (T) column", 
                           "Range of values in brightness (B) column", NULL};
      char *row_help[]= {  SUMA_SurfContHelp_RangeTbl_c0, 
                           SUMA_SurfContHelp_RangeTbl_r1, 
                           SUMA_SurfContHelp_RangeTbl_r2, 
                           SUMA_SurfContHelp_RangeTbl_r3, NULL};
      if (!SurfCont->rcswr) {
         SurfCont->rcswr = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->opts_form,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNrightAttachment, XmATTACH_FORM , 
            XmNleftAttachment,  XmATTACH_NONE,
            XmNtopAttachment, XmATTACH_WIDGET ,
            XmNtopWidget, SurfCont->rcclust?SurfCont->rcclust:SurfCont->opts_rc,
            NULL);
      }
      
      if (!SurfCont->RangeTable->cells) {
         int colw[5] = { 1, 6, 6, 6, 6 };
         /* create the widgets for the range table */
         SUMA_CreateTable( SurfCont->rcswr,
                           4, 5, 
                           "VolCont->Dset_Mapping->RangeTable",
                           row_tit, col_tit,  
                           row_hint, col_hint,  
                           row_help, col_help,  
                           colw, NOPE, SUMA_string, 
                           NULL, NULL,
                           NULL, NULL,  
                           SUMA_RangeTableCell_EV, (void *)ado, 
                           SurfCont->RangeTable);
      }

      if (!XtIsManaged(SurfCont->rcswr)) XtManageChild (SurfCont->rcswr);
   } /* The Range values block */
         
   if (NewDset) {
      /* initialize tables of range values */
      SUMA_InitRangeTable(ado, 2);
   }
   
   if (!XtIsManaged(SurfCont->rcvo)) XtManageChild (SurfCont->rcvo);
   SUMA_FORCE_SCALE_HEIGHT(SUMA_ADO_Cont(ado)); 

   SUMA_RETURNe;
}

void SUMA_set_cmap_options_GLDO(SUMA_ALL_DO *ado, SUMA_Boolean NewDset,
                           SUMA_Boolean NewMap)
{
   static char FuncName[]={"SUMA_set_cmap_options_GLDO"};
   SUMA_MenuItem  *SwitchInt_Menu = NULL, *SwitchThr_Menu = NULL, 
                  *SwitchBrt_Menu = NULL;
   int N_items, FirstTime;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;

   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURNe;
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   if (!SurfCont) SUMA_RETURNe;
   if (!SurfCont->opts_form || !SurfCont->opts_rc) SUMA_RETURNe;
   if (!curColPlane) SUMA_RETURNe;
   if (!NewDset && !NewMap && SurfCont->rcvo && SurfCont->rccm) {
      SUMA_SL_Err("Nothing to do");
      SUMA_RETURNe;
   }
   /* row column to contain all switching stuffs */
   if (!SurfCont->rcvo){
      SurfCont->rcvo = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SurfCont->opts_rc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);
      NewDset = YUP; /* The first time around */
   } else {
      /* (no need ..) */
      /* XtUnmanageChild(SurfCont->rcvo);  */
   }  
   
   if (NewDset) { /* The intensity / threshold / Brightness block*/
      /* link mode */
      if (!SurfCont->LinkModeMenu->mw[SW_LinkMode]) {
               Widget rc = NULL; /* one pass through this block ONLY */
               rc = XtVaCreateWidget ("rowcolumn",
                  xmRowColumnWidgetClass, SurfCont->rcvo,
                  XmNpacking, XmPACK_TIGHT, 
                  XmNorientation , XmHORIZONTAL ,
                  XmNmarginHeight, 0 ,
                  XmNmarginWidth , 0 ,
                  NULL);
               
               SUMA_LH("Forming map mode menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "IxT", '\0', YUP, LinkMode_Menu, 
                               (void *)ado,  
                               "GraphCont->GDset_Mapping->IxT",
                               "Set I, T selection linking modes.", 
                               SUMA_SurfContHelp_Link,
                               SurfCont->LinkModeMenu);
               XtManageChild (SurfCont->LinkModeMenu->mw[SW_LinkMode]);
               
               XtManageChild(rc);
         }

      if (!SurfCont->rcsw) {
         SurfCont->rcsw = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcvo,
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
         /*XtUnmanageChild(SurfCont->rcsw); */
       }
      if (!SurfCont->rcsw_v1) {
         SurfCont->rcsw_v1 = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcsw,
            XmNpacking, XmPACK_COLUMN, 
            XmNorientation , XmVERTICAL ,
            XmNnumColumns, 1,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      }
      if (!SurfCont->rcsw_v2) {
         SurfCont->rcsw_v2 = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcsw,
            XmNpacking, XmPACK_COLUMN, 
            XmNorientation , XmVERTICAL ,
            XmNnumColumns, 1,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL);
      }
      
      /* Switching triplets */
      SwitchInt_Menu = 
         SUMA_FormSwitchColMenuVector(ado, 0, &N_items);      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: %d items.\n", FuncName, N_items);
      if (SwitchInt_Menu || !N_items) {
         SurfCont->SwitchIntMenu = 
               SUMA_Free_Menu_Widget(SurfCont->SwitchIntMenu);
         SurfCont->SwitchIntMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "I", '\0', YUP, SwitchInt_Menu, 
                           (void *)ado, 
                           "GraphCont->GDset_Mapping->I",
                  "Select Intensity (I) column, aka sub-brick. (BHelp for more)",
                           SUMA_SurfContHelp_SelInt,
                           SurfCont->SwitchIntMenu );
         XtInsertEventHandler( SurfCont->SwitchIntMenu->mw[0] , 
                                             /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) ado ,   /* client data */
                        XtListTail ) ;
         if (LocalHead) 
            SUMA_ShowMeTheChildren(SurfCont->SwitchIntMenu->mw[0]);
         XtManageChild (SurfCont->SwitchIntMenu->mw[0]);
         /* Now destroy the SwitchInt_Menu */
         SwitchInt_Menu = SUMA_FreeMenuVector(SwitchInt_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SurfCont->SwitchIntMenu,
                       curColPlane->OptScl->find+1) ; 
      } else {
         SUMA_SL_Err("NULL SwitchInt_Menu");
      }
      
      SwitchThr_Menu = 
         SUMA_FormSwitchColMenuVector(ado, 1, &N_items);
      if (SwitchThr_Menu || !N_items) {
         SurfCont->SwitchThrMenu = 
               SUMA_Free_Menu_Widget(SurfCont->SwitchThrMenu);
         SurfCont->SwitchThrMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);  
         SUMA_BuildMenuReset(13);         
         SUMA_BuildMenu (SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "T", '\0', YUP, SwitchThr_Menu, 
                           (void *)ado,  
                           "GraphCont->GDset_Mapping->T",
               "Select Threshold (T) column, aka sub-brick. (BHelp for more)",
                           SUMA_SurfContHelp_SelThr ,    
                           SurfCont->SwitchThrMenu );
         XtInsertEventHandler( SurfCont->SwitchThrMenu->mw[0] ,      
                                       /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) ado ,   /* client data */
                        XtListTail ) ;
         XtManageChild (SurfCont->SwitchThrMenu->mw[0]);
         /* Now destroy the SwitchThr_Menu */
         SwitchThr_Menu = SUMA_FreeMenuVector(SwitchThr_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SurfCont->SwitchThrMenu,
                       curColPlane->OptScl->tind+1); 
      } else {
         SUMA_SL_Err("NULL SwitchThr_Menu");
      }

      SwitchBrt_Menu = 
         SUMA_FormSwitchColMenuVector(ado, 2, &N_items);
      if (SwitchBrt_Menu || !N_items) {
         SurfCont->SwitchBrtMenu = 
               SUMA_Free_Menu_Widget(SurfCont->SwitchBrtMenu);
         SurfCont->SwitchBrtMenu = 
               SUMA_Alloc_Menu_Widget(N_items+1);
         SUMA_BuildMenuReset(13);
         SUMA_BuildMenu (SurfCont->rcsw_v1, XmMENU_OPTION, /* populate it */
                           "B", '\0', YUP, SwitchBrt_Menu, 
                           (void *)ado,  
                           "GraphCont->GDset_Mapping->B",
               "Select Brightness (B) column, aka sub-brick. (BHelp for more)",
                           SUMA_SurfContHelp_SelBrt,
                           SurfCont->SwitchBrtMenu );
         XtInsertEventHandler( SurfCont->SwitchBrtMenu->mw[0] ,      
                                                /* handle events in optmenu */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        SUMA_optmenu_EV ,  /* handler */
                        (XtPointer) ado ,   /* client data */
                        XtListTail ) ;

         XtManageChild (SurfCont->SwitchBrtMenu->mw[0]);
         /* Now destroy the SwitchBrt_Menu */
         SwitchBrt_Menu = SUMA_FreeMenuVector(SwitchBrt_Menu, N_items);
         /* setup the history to the proper widget */
         SUMA_Set_Menu_Widget(SurfCont->SwitchBrtMenu,
                       curColPlane->OptScl->bind+1);
      } else {
         SUMA_SL_Err("NULL SwitchBrt_Menu");
      }
      
     if (1) {
     /* put the toggle buttons */
         if (!SurfCont->Int_tb) {
            SurfCont->Int_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SurfCont->rcsw_v2, NULL);
            XtAddCallback (SurfCont->Int_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwitchInt_toggled, ado);
            SUMA_Register_Widget_Help(SurfCont->Int_tb, 1,
                              "GraphCont->GDset_Mapping->I->v",
                              "View (ON)/Hide graph edge colors",
                              SUMA_GraphContHelp_SelIntTgl);
            SUMA_SET_SELECT_COLOR(SurfCont->Int_tb);
         } 
         XmToggleButtonSetState (SurfCont->Int_tb,      
                    curColPlane->ShowMode > 0 ? 1:0 , NOPE);
         
         if (!SurfCont->Thr_tb) {
            SurfCont->Thr_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SurfCont->rcsw_v2, NULL);
            XtAddCallback (SurfCont->Thr_tb, 
                  XmNvalueChangedCallback, SUMA_cb_SwitchThr_toggled, ado);
            SUMA_SET_SELECT_COLOR(SurfCont->Thr_tb);
            SUMA_Register_Widget_Help(SurfCont->Thr_tb, 1,
                              "GraphCont->GDset_Mapping->T->v",
                              "Apply (ON)/Ignore thresholding",
                              SUMA_SurfContHelp_SelThrTgl);
         }
         if (curColPlane->OptScl->tind >=0) {
            XmToggleButtonSetState (SurfCont->Thr_tb, 
                              curColPlane->OptScl->UseThr, NOPE);
         }else {
            XmToggleButtonSetState (SurfCont->Thr_tb, NOPE, NOPE);
         }
         
         if (!SurfCont->Brt_tb) {
            SurfCont->Brt_tb = XtVaCreateManagedWidget("v", 
               xmToggleButtonWidgetClass, SurfCont->rcsw_v2, NULL);
            XtAddCallback (SurfCont->Brt_tb, 
                     XmNvalueChangedCallback, SUMA_cb_SwitchBrt_toggled, ado);
            SUMA_SET_SELECT_COLOR(SurfCont->Brt_tb);
            SUMA_Register_Widget_Help(SurfCont->Brt_tb, 1,
                     "GraphCont->GDset_Mapping->B->v",
                     "View (ON)/Ignore brightness modulation",
                     SUMA_SurfContHelp_SelBrtTgl);
         }
         if (curColPlane->OptScl->bind >=0) {
            XmToggleButtonSetState (SurfCont->Brt_tb, 
                     curColPlane->OptScl->UseBrt, NOPE);
         } else {
            XmToggleButtonSetState (SurfCont->Brt_tb, NOPE, NOPE);
         }
      }
      if (!XtIsManaged(SurfCont->rcsw_v1)) 
         XtManageChild (SurfCont->rcsw_v1);
      if (!XtIsManaged(SurfCont->rcsw_v2)) 
         XtManageChild (SurfCont->rcsw_v2);
      if (!XtIsManaged(SurfCont->rcsw)) XtManageChild (SurfCont->rcsw);
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
         "Intensity clipping range (append '%' for percentiles, see BHelp)", 
         "Brightness modulation clipping range (much more with BHelp)", 
         "Brightness modulation factor range (much more with BHelp)" , 
         "Coordinate bias range (much more with BHelp)", NULL};
      char *row_help[]= {  SUMA_SurfContHelp_SetRngTbl_r0, 
                           SUMA_SurfContHelp_SetRngTbl_r1,
                           SUMA_SurfContHelp_SetRngTbl_r2, 
                           SUMA_SurfContHelp_SetRngTbl_r3, 
                           SUMA_SurfContHelp_SetRngTbl_r4, NULL};
      if (!SurfCont->rccm) {
         SurfCont->rccm = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->rcvo,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0,
            XmNmarginWidth, 0,
            NULL);
         NewMap = YUP; /* the first time around */
      }
       
      if (NewMap) {/* new colormaps */
         SUMA_LH("NewMap set");
         SUMA_Register_Widget_Help(SurfCont->rccm, 0,
                        "GraphCont->GDset_Mapping->SetRangeTable",
                        "Edge colorization parameters",
                        "Set parameters for mapping edge data onto color scale");
         if (!SurfCont->SetRangeTable->cells) {
            int colw[3] = { 1, 8, 8 };
            SUMA_SRV_DATA *srvd=(SUMA_SRV_DATA *)calloc(1,sizeof(SUMA_SRV_DATA));
            srvd->ado = ado; srvd->colp = NULL;               
            /* create the widgets for the range table */
            SUMA_LH("Creating table");
            SUMA_CreateTable( SurfCont->rccm,
                           5, 3, 
                           "GraphCont->GDset_Mapping->SetRangeTable",
                           row_tit, col_tit,  
                           row_hint, col_hint,  
                           row_help, col_help,  
                           colw, YUP, SUMA_float, 
                           SUMA_cb_SetRangeValue, (void *)srvd,
                           SUMA_SetRangeTableTit_EV, NULL,
                           NULL, NULL,  
                           SurfCont->SetRangeTable);
         }
         if (!SurfCont->CoordBiasMenu->mw[SW_CoordBias]) {
               Widget rc = NULL; /* one pass through this block ONLY */
               rc = XtVaCreateWidget ("rowcolumn",
                  xmRowColumnWidgetClass, SurfCont->rccm,
                  XmNpacking, XmPACK_TIGHT, 
                  XmNorientation , XmHORIZONTAL ,
                  XmNmarginHeight, 0 ,
                  XmNmarginWidth , 0 ,
                  NULL);
               
               SUMA_LH("Forming map mode menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "Col", '\0', YUP, CmapMode_Menu, 
                               (void *)ado,  
                               "GraphCont->GDset_Mapping->Col",
                               "Switch between color mapping modes.", 
                               SUMA_SurfContHelp_Col,
                               SurfCont->CmapModeMenu);
               XtManageChild (SurfCont->CmapModeMenu->mw[SW_CmapMode]);
               
               #if 0
               SUMA_LH("Forming new bias menu");
               SUMA_BuildMenuReset(0);
               SUMA_BuildMenu ( rc, XmMENU_OPTION, 
                               "Bias", '\0', YUP, CoordBias_Menu, 
                               (void *)ado, 
                               "GraphCont->GDset_Mapping->Bias",
                               "Coordinate bias direction", 
                               SUMA_SurfContHelp_Bias, 
                               SurfCont->CoordBiasMenu);
               XtManageChild (SurfCont->CoordBiasMenu->mw[SW_CoordBias]);
               #endif
               
               XtManageChild(rc);
         }
            
         if (!SurfCont->rccm_swcmap) {
            SUMA_LH("Creating rccm_swcmap");
            SurfCont->rccm_swcmap =  XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SurfCont->rccm,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0 ,
               XmNmarginWidth , 0 ,
               NULL);
         }

         {
            SUMA_CreateUpdatableCmapMenu((SUMA_ALL_DO *)ado); 

            #if 0
               /* Not any more, menu is now stuck in its own rc */
               /* the loader, needs to be recreated with colormap menu  */
               if (SurfCont->CmapLoad_pb) { 
                  XtDestroyWidget(SurfCont->CmapLoad_pb); 
                  SurfCont->CmapLoad_pb = NULL;
               }
            #endif
            if (!SurfCont->CmapLoad_pb) { 
               SUMA_LH("Forming CmapLoad button");
               SurfCont->CmapLoad_pb = XtVaCreateManagedWidget ("New", 
                                 xmPushButtonWidgetClass, 
                                 SurfCont->rccm_swcmap, 
                                 NULL);
               XtAddCallback (SurfCont->CmapLoad_pb, XmNactivateCallback, 
                              SUMA_cb_Cmap_Load, (XtPointer) ado);
               SUMA_Register_Widget_Help(SurfCont->CmapLoad_pb , 1,
                              "GraphCont->GDset_Mapping->Cmp->New",
                              "Load new colormap",
                              SUMA_SurfContHelp_CmpNew);
            }
         } /* new colormaps */
         if (!XtIsManaged(SurfCont->rccm_swcmap)) 
                     XtManageChild (SurfCont->rccm_swcmap); 
      }
      
      /* Set the CoordBias's menu history to reflect current setting */
      SUMA_LH("Updating Link Mode History");
      SUMA_Set_Menu_Widget( SurfCont->LinkModeMenu,
                     curColPlane->LinkMode); 
      
      SUMA_LH("Working the lock stuff ...");
      /* You'll need to fix the table's locking widget colors */
      if ( SurfCont->IntRangeLocked == 
               curColPlane->OptScl->AutoIntRange) {
         SUMA_LH("   Do the Int");
         /* need to put things in sync */
         SurfCont->IntRangeLocked = !SurfCont->IntRangeLocked;
         MCW_invert_widget_sync(SurfCont->SetRangeTable->cells[1], 0);
      }
      if ( SurfCont->BrtRangeLocked == 
               curColPlane->OptScl->AutoBrtRange) {
         SUMA_LH("   Do the Brt");
         /* need to put things in sync */
         SurfCont->BrtRangeLocked = !SurfCont->BrtRangeLocked;
         MCW_invert_widget_sync(SurfCont->SetRangeTable->cells[2], 0);
      } 

      /* Set the CoordBias's menu history to reflect current setting */
      SUMA_LH("Updating CoorBias chooser History");
      SUMA_Set_Menu_Widget( SurfCont->CoordBiasMenu,
                     curColPlane->OptScl->DoBias); 
 
      /* Set the Col's menu history to reflect current setting */
      SUMA_LH("Updating Col chooser History");
      SUMA_Set_Menu_Widget( SurfCont->CmapModeMenu,
                     curColPlane->OptScl->interpmode); 
 
      /* add the selectors for symmetric range and absolute threshold */
      if (!SurfCont->AbsThresh_tb) {
         Widget rc;
         rc = XtVaCreateWidget ("rowcolumn",
               xmRowColumnWidgetClass, SurfCont->rccm,
               XmNpacking, XmPACK_TIGHT, 
               XmNorientation , XmHORIZONTAL ,
               XmNmarginHeight, 0 ,
               XmNmarginWidth , 0 ,
               NULL);
         /* create the absolute threshold toggle button */
         SurfCont->AbsThresh_tb = XtVaCreateManagedWidget("|T|", 
               xmToggleButtonWidgetClass, rc, 
               NULL);
         XtAddCallback (SurfCont->AbsThresh_tb, 
               XmNvalueChangedCallback, SUMA_cb_AbsThresh_tb_toggled, ado);
         SUMA_Register_Widget_Help(SurfCont->AbsThresh_tb , 1,
                           "GraphCont->GDset_Mapping->abs_T",
                           "Absolute threshold ON/OFF",
                           SUMA_SurfContHelp_AbsThr );
         
         SUMA_SET_SELECT_COLOR(SurfCont->AbsThresh_tb);
         
         /* create the symmetric range toggle button */
         SurfCont->SymIrange_tb = XtVaCreateManagedWidget("sym I", 
               xmToggleButtonWidgetClass, rc, NULL);
         XtAddCallback (SurfCont->SymIrange_tb, 
               XmNvalueChangedCallback, SUMA_cb_SymIrange_tb_toggled, ado);
         SUMA_Register_Widget_Help(SurfCont->SymIrange_tb, 1,
                           "GraphCont->GDset_Mapping->sym_I",
                           "Intensity range symmetry about 0 ",
                           SUMA_SurfContHelp_Isym );
         SUMA_SET_SELECT_COLOR(SurfCont->SymIrange_tb);
         
         /* add a button for zero masking */
         SurfCont->ShowZero_tb = XtVaCreateManagedWidget("shw 0", 
               xmToggleButtonWidgetClass, rc, NULL);
         XtAddCallback (SurfCont->ShowZero_tb, 
               XmNvalueChangedCallback, SUMA_cb_ShowZero_tb_toggled, ado);
         SUMA_Register_Widget_Help(SurfCont->ShowZero_tb,  1, 
                     "GraphCont->GDset_Mapping->shw_0",
                     "Color masking of nodes with intensity = 0 ",
                     SUMA_SurfContHelp_Shw0);
         SUMA_SET_SELECT_COLOR(SurfCont->ShowZero_tb);
         XtManageChild (rc);
      }
            
      if (curColPlane->OptScl->ThrMode == SUMA_ABS_LESS_THAN) {
         XmToggleButtonSetState( SurfCont->AbsThresh_tb, True, NOPE);
      } else if (curColPlane->OptScl->ThrMode == SUMA_LESS_THAN) {
         XmToggleButtonSetState( SurfCont->AbsThresh_tb, False, NOPE);
      } else {
         SUMA_S_Err("Not ready to handle ThrModeR of %d yet", 
                     curColPlane->OptScl->ThrMode);
      }
      if (!curColPlane->SymIrange) {
         XmToggleButtonSetState( SurfCont->SymIrange_tb, False, NOPE);
      } else {
         XmToggleButtonSetState( SurfCont->SymIrange_tb, True, NOPE);
      }
      if (!curColPlane->OptScl->MaskZero) {
         XmToggleButtonSetState( SurfCont->ShowZero_tb, True, NOPE);
      } else {
         XmToggleButtonSetState( SurfCont->ShowZero_tb, False, NOPE);
      }
      
      if (!XtIsManaged(SurfCont->rccm)) XtManageChild (SurfCont->rccm);
   
   }/*  The Color map range and selector block */
   
   if (1){ /* The Range values block*/
      char *col_tit[]=  {  " ", "Min", "Edge", "Max", "Edge", NULL};
      char *col_hint[]= {  "Full range of values in Dset", 
                           "Minimum value in Dset column", 
                           "Edge index at minimum", 
                           "Maximum value in Dset column", 
                           "Edge index at maximum", NULL};
      char *col_help[]= {  SUMA_SurfContHelp_RangeTbl_c0, 
                           SUMA_SurfContHelp_RangeTbl_c1,
                           SUMA_GraphContHelp_RangeTbl_c2, 
                           SUMA_SurfContHelp_RangeTbl_c3, 
                           SUMA_GraphContHelp_RangeTbl_c4, NULL};
      char *row_tit[]=  {  " ", "I", "T", "B", NULL};
      char *row_hint[]= {  "Full range of values in Dset", 
                           "Range of values in intensity (I) column", 
                           "Range of values in threshold (T) column", 
                           "Range of values in brightness (B) column", NULL};
      char *row_help[]= {  SUMA_SurfContHelp_RangeTbl_c0, 
                           SUMA_SurfContHelp_RangeTbl_r1, 
                           SUMA_SurfContHelp_RangeTbl_r2, 
                           SUMA_SurfContHelp_RangeTbl_r3, NULL};
      if (!SurfCont->rcswr) {
         SurfCont->rcswr = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->opts_form,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNrightAttachment, XmATTACH_FORM , 
            XmNleftAttachment,  XmATTACH_NONE,
            XmNtopAttachment, XmATTACH_WIDGET ,
            XmNtopWidget, SurfCont->opts_rc,
            NULL);
      }
      
      if (!SurfCont->RangeTable->cells) {
         int colw[5] = { 1, 6, 6, 6, 6 };
         /* create the widgets for the range table */
         SUMA_CreateTable( SurfCont->rcswr,
                           4, 5, 
                           "GraphCont->GDset_Mapping->RangeTable",
                           row_tit, col_tit,  
                           row_hint, col_hint,  
                           row_help, col_help,  
                           colw, NOPE, SUMA_string, 
                           NULL, NULL,
                           NULL, NULL,  
                           SUMA_RangeTableCell_EV, (void *)ado, 
                           SurfCont->RangeTable);
      }

      if (!XtIsManaged(SurfCont->rcswr)) XtManageChild (SurfCont->rcswr);
   } /* The Range values block */
         
   if (NewDset) {
      /* initialize tables of range values */
      SUMA_InitRangeTable(ado, 2);
   }
   
   if (!XtIsManaged(SurfCont->rcvo)) XtManageChild (SurfCont->rcvo);
   SUMA_FORCE_SCALE_HEIGHT(SUMA_ADO_Cont(ado)); 

   SUMA_RETURNe;
}

/*!
   A function to create the cmap selection menu
   in a manner that can be recreated if the menu
   contents change. You can call this function
   repeatedly whenever menu contents change 
*/
void SUMA_CreateUpdatableCmapMenu(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_CreateUpdatableCmapMenu"};
   SUMA_MenuItem *SwitchCmap_Menu = NULL;
   SUMA_X_SurfCont *SurfCont=NULL; 
   char *wname; 
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SUMAg_CF->scm) {   
      SUMAg_CF->scm = SUMA_Build_Color_maps();
      if (!SUMAg_CF->scm) {
         SUMA_SL_Err("Failed to build color maps.\n");
         SUMA_RETURNe;
      }
   }
   
   SurfCont = SUMA_ADO_Cont(ado);
   if (!SurfCont->rc_CmapCont) { /* first pass, create placement container */
      SurfCont->rc_CmapCont = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, SurfCont->rccm_swcmap,
      XmNpacking, XmPACK_TIGHT, 
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight, 0 ,
      XmNmarginWidth , 0 ,
      NULL);   
   }

   SUMA_LH("Forming CmapMenu");
   SwitchCmap_Menu = SUMA_FormSwitchCmapMenuVector(SUMAg_CF->scm->CMv, 
                                                   SUMAg_CF->scm->N_maps);
   switch(ado->do_type) {
      case SO_type:
         wname = "SurfCont->Dset_Mapping->Cmp";
         break;
      case VO_type:
         wname = "VolCont->Dset_Mapping->Cmp";
         break;
      case GRAPH_LINK_type:
         wname = "GraphCont->GDset_Mapping->Cmp";
         break;
      default:
         wname = "WhatIsThisFor->Cmp";
         break;
   }
   
   if (SwitchCmap_Menu) {
      SurfCont->SwitchCmapMenu =
         SUMA_Free_Menu_Widget(SurfCont->SwitchCmapMenu);
      SurfCont->SwitchCmapMenu = 
               SUMA_Alloc_Menu_Widget(SUMAg_CF->scm->N_maps+1);
      SUMA_BuildMenuReset(10);
      SUMA_BuildMenu (  SurfCont->rc_CmapCont, 
                           XmMENU_OPTION, /* populate it */
                           "Cmp", '\0', YUP, SwitchCmap_Menu, 
                           (void *)ado,  
                           wname,
                           "Switch between available color maps."
                           " (BHelp for more)", 
                           SUMA_SurfContHelp_Cmp, 
                           SurfCont->SwitchCmapMenu );
      XtInsertEventHandler( SurfCont->SwitchCmapMenu->mw[0] ,      
                                               /* handle events in optmenu */
                            ButtonPressMask ,  /* button presses */
                            FALSE ,            /* nonmaskable events? */
                            SUMA_optmenu_EV ,  /* handler */
                            (XtPointer)ado ,   /* client data */
                            XtListTail ) ;
      XtManageChild (SurfCont->SwitchCmapMenu->mw[0]);
      /* Now destroy the SwitchCmap_Menu */
      SwitchCmap_Menu = SUMA_FreeMenuVector(SwitchCmap_Menu, 
                                          SUMAg_CF->scm->N_maps);
   }

   XtManageChild(SurfCont->rc_CmapCont);

   SUMA_RETURNe;
}

SUMA_Boolean SUMA_InitClustTable(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_InitClustTable"};
   SUMA_TABLE_FIELD *TFs;
   SUMA_Boolean ColorizeBaby;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;   
   SUMA_SCALE_TO_MAP_OPT *OptScl;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);

   if (!SurfCont) SUMA_RETURN(NOPE);
   if (ado->do_type == VO_type) {
      /* At the moment GUI is for interactive clustering is turned off, but
      to be safe, return without complaints */
      SUMA_LH("Nothing to do here until you implement clustering for volumes");
      SUMA_RETURN(YUP);
   }
   TFs = SurfCont->SetClustTable; 
   if (!TFs) SUMA_RETURN(NOPE);
   OptScl = curColPlane->OptScl;
   
   ColorizeBaby = NOPE; /* Not sure if I'll need this one here */
   
   SUMA_INSERT_CELL_VALUE(TFs, 1, 1, OptScl->ClustOpt->DistLim);
   SUMA_INSERT_CELL_VALUE(TFs, 1, 2, OptScl->ClustOpt->AreaLim);
   SUMA_SetTableTitleButton1(TFs, 1,0, OptScl->Clusterize);
   
   if (ColorizeBaby) {
      if (!SUMA_ColorizePlane (curColPlane)) {
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
SUMA_Boolean SUMA_InitRangeTable(SUMA_ALL_DO *ado, int what) 
{
   static char FuncName[]={"SUMA_InitRangeTable"};
   char srange_min[50], srange_max[50], srange_minloc[50], srange_maxloc[50];
   SUMA_TABLE_FIELD *TF, *TFs;
   int i, j, i1D, fi, bi, ti;
   double range[2];
   NI_element *nel;
   SUMA_SCALE_TO_MAP_OPT *OptScl;
   SUMA_Boolean DoIs = NOPE, DoBs = NOPE, ColorizeBaby;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (LocalHead) {
      SUMA_DUMP_TRACE("Who just called SUMA_InitRangeTable?");
   }
   if (!ado) { 
      SUMA_SL_Err("NULL ado");
      SUMA_RETURN(NOPE);
   }
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);

   if (!SurfCont || !curColPlane) SUMA_RETURN(NOPE);
   TF = SurfCont->RangeTable; 
   TFs = SurfCont->SetRangeTable; 
   if (!TF || !TFs || !TF->cells || !TFs->cells) SUMA_RETURN(NOPE);
   OptScl = curColPlane->OptScl;
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
   SUMA_LHv("Setting Int. fi=%d\n", fi);
   SUMA_RANGE_STRING(curColPlane->dset_link, 
                     fi, srange_min, srange_max, 
                     srange_minloc, srange_maxloc, range); 
   SUMA_INSERT_CELL_STRING(TF, 1, 1, srange_min);/* min */
   SUMA_INSERT_CELL_STRING(TF, 1, 2, srange_minloc);/* minloc */
   SUMA_INSERT_CELL_STRING(TF, 1, 3, srange_max);/* max */
   SUMA_INSERT_CELL_STRING(TF, 1, 4, srange_maxloc);/* maxloc */
   
   /* TFs Range table Int*/
   if (DoIs) {
      if (curColPlane->OptScl->AutoIntRange) {
         if (!curColPlane->ForceIntRange[0] && 
             !curColPlane->ForceIntRange[1]) {
            if (  OptScl->IntRange[0] != range[0] ||
                  OptScl->IntRange[1] != range[1] ) {
               ColorizeBaby = YUP;      
               OptScl->IntRange[0] = range[0]; OptScl->IntRange[1] = range[1]; 
            }
         } else {
            SUMA_LH("Using ForceIntRange");
            if (  OptScl->IntRange[0] != 
                     curColPlane->ForceIntRange[0] ||
                  OptScl->IntRange[1] != 
                     curColPlane->ForceIntRange[1] ) {
               ColorizeBaby = YUP;      
               OptScl->IntRange[0] = curColPlane->ForceIntRange[0];
               OptScl->IntRange[1] = curColPlane->ForceIntRange[1];
            }
         }

         /* enforce the SymIrange option */
         if (curColPlane->SymIrange) {
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
         if (curColPlane->OptScl->AutoIntRange < 0) {
            /* snap out of the initialization and go to user's default */
            curColPlane->OptScl->AutoIntRange = 
                     SUMA_isEnv("SUMA_Auto_I_Range","YES") ? 1:0;
            SUMA_LHv("AutoIntRange now %d\n",
                     curColPlane->OptScl->AutoIntRange);
         }
      }else {
         /* Make sure viewer is showing same values as in OptScl */
         /* enforce the SymIrange option */
         if (curColPlane->SymIrange) {
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
   SUMA_RANGE_STRING(curColPlane->dset_link, ti, 
                     srange_min, srange_max, 
                     srange_minloc, srange_maxloc, range); 
   SUMA_INSERT_CELL_STRING(TF, 2, 1, srange_min);/* min */
   SUMA_INSERT_CELL_STRING(TF, 2, 2, srange_minloc);/* minloc */
   SUMA_INSERT_CELL_STRING(TF, 2, 3, srange_max);/* max */
   SUMA_INSERT_CELL_STRING(TF, 2, 4, srange_maxloc);/* maxloc */
  
   /* TF Range table Brt*/
   SUMA_LH("Setting Brt.");
   SUMA_RANGE_STRING(curColPlane->dset_link, bi, 
                     srange_min, srange_max, 
                     srange_minloc, srange_maxloc, range); 
   SUMA_INSERT_CELL_STRING(TF, 3, 1, srange_min);/* min */
   SUMA_INSERT_CELL_STRING(TF, 3, 2, srange_minloc);/* minloc */
   SUMA_INSERT_CELL_STRING(TF, 3, 3, srange_max);/* max */
   SUMA_INSERT_CELL_STRING(TF, 3, 4, srange_maxloc);/* maxloc */
   /* TFs Range table Brt*/
   if (DoBs) {
      if (curColPlane->OptScl->AutoBrtRange) { 
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
         if (curColPlane->OptScl->AutoBrtRange < 0) {
            /* snap out of the initialization and go to user's default */
            curColPlane->OptScl->AutoBrtRange = 
                     SUMA_isEnv("SUMA_Auto_B_Range","YES") ? 1:0;
            SUMA_LHv("AutoBrtRange now %d\n",
                     curColPlane->OptScl->AutoBrtRange);
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
      if (!SUMA_ColorizePlane (curColPlane)) {
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
         SUMA_ALL_DO *ado, int type, 
         int refresh, int bringup)
{
   static char FuncName[]={"SUMA_DsetColSelectList"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   if (!ado || !SurfCont) SUMA_RETURN(NOPE);
   
   /* Widget is common to a surface controller. */
   switch (type) {
      case 0:
         LW = SurfCont->SwitchIntMenu->lw;
         if (!LW) {
            SUMA_LH("Allocating widget");
            /* need to create widget */
            LW = SUMA_AllocateScrolledList   (  
                  "Switch Intensity", SUMA_LSP_BROWSE,
                  NOPE,          NOPE,
                  SurfCont->TLS, SWP_POINTER_OFF,
                  150,
                  SUMA_cb_SelectSwitchInt, (void *)ado,
                  SUMA_cb_SelectSwitchInt, (void *)ado,
                  SUMA_cb_CloseSwitchLst, NULL);

            SurfCont->SwitchIntMenu->lw = LW;
            refresh = 1; /* no doubt aboot it */
         } else {
            if (  (void *)ado != LW->Default_Data || 
                  (void *)ado != LW->Select_Data) {
               /* just update the callback data info in LW */
               SUMA_UpdateScrolledListData(LW, (void *)ado, (void *)ado, NULL);
            }
            
         }
         break;
      case 1:
         LW = SurfCont->SwitchThrMenu->lw;
         if (!LW) {
            SUMA_LH("Allocating widget");
            /* need to create widget */
            LW = SUMA_AllocateScrolledList   (  
                  "Switch Threshold", SUMA_LSP_BROWSE,
                  NOPE,          NOPE,
                  SurfCont->TLS, SWP_POINTER_OFF,
                  150,
                  SUMA_cb_SelectSwitchThr, (void *)ado,
                  SUMA_cb_SelectSwitchThr, (void *)ado,
                  SUMA_cb_CloseSwitchLst, NULL);

            SurfCont->SwitchThrMenu->lw = LW;
            refresh = 1; /* no doubt aboot it */
         } else {
            if (  (void *)ado != LW->Default_Data || 
                  (void *)ado != LW->Select_Data) {
               /* just update the callback data info in LW */
               SUMA_UpdateScrolledListData(LW, (void *)ado, (void *)ado, NULL);
            }
            
         }
         break;
      case 2:
         LW = SurfCont->SwitchBrtMenu->lw;
         if (!LW) {
            SUMA_LH("Allocating widget");
            /* need to create widget */
            LW = SUMA_AllocateScrolledList   (  
                  "Switch Brightness", SUMA_LSP_BROWSE,
                  NOPE,          NOPE,
                  SurfCont->TLS, SWP_POINTER_OFF,
                  150, 
                  SUMA_cb_SelectSwitchBrt, (void *)ado,
                  SUMA_cb_SelectSwitchBrt, (void *)ado,
                  SUMA_cb_CloseSwitchLst, NULL);

            SurfCont->SwitchBrtMenu->lw = LW;
            refresh = 1; /* no doubt aboot it */
         } else {
            if (  (void *)ado != LW->Default_Data || 
                  (void *)ado != LW->Select_Data) {
               /* just update the callback data info in LW */
               SUMA_UpdateScrolledListData(LW, (void *)ado, (void *)ado, NULL);
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
               SDSET_ID(curColPlane->dset_link))) {
      refresh=1;
   } 
  
  if (refresh) {
      /* Now creating list*/
      if (LW->ALS) {
         if (LocalHead) SUMA_S_Err("Freeing the hag.");
         LW->ALS = SUMA_FreeAssembleListStruct(LW->ALS);
      }
      SUMA_LH("Assembling");
      LW->ALS = SUMA_AssembleDsetColList(curColPlane->dset_link);
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
   SUMA_ALL_DO *ado = NULL;
   SUMA_LIST_WIDGET *LW = NULL;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   int ichoice;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean CloseShop = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   ado = (SUMA_ALL_DO *)client_data;
   SurfCont = SUMA_ADO_Cont(ado);
   LW = SurfCont->SwitchIntMenu->lw;
   
   if ((ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop))==-1) {
      SUMA_RETURNe;
   }
   

   if (!SUMA_SelectSwitchDsetCol(ado, LW, 0, ichoice)) {
      SUMA_S_Err("Failed to SelectSwitchDsetCol");
      SUMA_RETURNe;
   }
   
   if (CloseShop) {
      SUMA_cb_CloseSwitchLst( w,  (XtPointer)LW,  call_data);
   }  
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(ado);

   SUMA_RETURNe;
}

void SUMA_cb_SelectSwitchThr (
         Widget w, XtPointer client_data, 
         XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SelectSwitchThr"};
   SUMA_ALL_DO *ado = NULL;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   SUMA_Boolean CloseShop = NOPE;
   SUMA_LIST_WIDGET *LW = NULL;
   int ichoice;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   ado = (SUMA_ALL_DO *)client_data;
   SurfCont = SUMA_ADO_Cont(ado);
   LW = SurfCont->SwitchThrMenu->lw;
   
   if ((ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop))==-1) {
      SUMA_RETURNe;
   }
   
   if (!SUMA_SelectSwitchDsetCol(ado, LW, 1, ichoice)) {
      SUMA_S_Err("Failed to SelectSwitchDsetCol");
      SUMA_RETURNe;
   }

   if (CloseShop) {
      SUMA_cb_CloseSwitchLst( w,  (XtPointer)LW,  call_data);
   }  
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(ado);

   SUMA_RETURNe;
}

void SUMA_cb_SelectSwitchBrt (
         Widget w, XtPointer client_data, 
         XtPointer call_data)
{
   static char FuncName[]={"SUMA_cb_SelectSwitchBrt"};
   SUMA_ALL_DO *ado = NULL;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   SUMA_Boolean CloseShop = NOPE;
   SUMA_LIST_WIDGET *LW = NULL;
   int ichoice;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   ado = (SUMA_ALL_DO *)client_data;
   SurfCont = SUMA_ADO_Cont(ado);
   LW = SurfCont->SwitchBrtMenu->lw;
   
   if ((ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop))==-1) {
      SUMA_RETURNe;
   }

   if (!SUMA_SelectSwitchDsetCol(ado, LW, 2, ichoice)) {
      SUMA_S_Err("Failed to SelectSwitchDsetCol");
      SUMA_RETURNe;
   }
   
   if (CloseShop) {
      SUMA_cb_CloseSwitchLst( w,  (XtPointer)LW,  call_data);
   }  
   
   /* update Lbl fields */
   SUMA_UpdateNodeLblField(ado);

   SUMA_RETURNe;
}

   
int SUMA_SelectSwitchDsetCol(
         SUMA_ALL_DO *ado, 
         SUMA_LIST_WIDGET *LW, 
         int block,
         int ichoice)
{
   static char FuncName[]={"SUMA_SelectSwitchDsetCol"};
   SUMA_MenuCallBackData data;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   if (!ado || !LW || block < 0 || block > 2 || ichoice < 0) SUMA_RETURN(0);
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   
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
                     (ado, curColPlane, 
                      (INT_CAST)LW->ALS->oplist[ichoice], 1)) {
                  SUMA_SL_Err("Failed in SUMA_SwitchColPlaneIntensity");
               }
               break;
            case 1:
               if (!SUMA_SwitchColPlaneThreshold
                     (ado, curColPlane, 
                      (INT_CAST)LW->ALS->oplist[ichoice], 1)) {
                  SUMA_SL_Err("Failed in SUMA_SwitchColPlaneThreshold");
               }
               break;
            case 2:
               if (!SUMA_SwitchColPlaneBrightness
                     (ado, curColPlane, 
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
SUMA_Boolean SUMA_CmapSelectList(SUMA_ALL_DO *ado, int refresh, 
                                 int bringup)
{
   static char FuncName[]={"SUMA_CmapSelectList"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
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
   SurfCont = SUMA_ADO_Cont(ado);
   
   if (!LW) {
      SUMA_LH("Allocating widget");
      /* need to create widget */
      LW = SUMA_AllocateScrolledList   (  
            "Switch Cmap", SUMA_LSP_BROWSE,
            NOPE,          NOPE,
            SurfCont->TLS, SWP_POINTER_OFF,
            125,
            SUMA_cb_SelectSwitchCmap, (void *)ado,
            SUMA_cb_SelectSwitchCmap, (void *)ado,
            SUMA_cb_CloseSwitchCmap, NULL);

      SUMAg_CF->X->SwitchCmapLst = LW;
      refresh = 1; /* no doubt aboot it */
   } else {
      if ((void *)ado != LW->Default_Data || (void *)ado != LW->Select_Data) {
         /* just update the callback data info in LW */
         SUMA_UpdateScrolledListData(LW, (void *)ado, (void *)ado, NULL);
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

SUMA_Boolean SUMA_SetCmodeMenuChoice(SUMA_ALL_DO *ado, char *str)
{
   static char FuncName[]={"SUMA_SetCmodeMenuChoice"};
   int i, Nbutt = 0, nstr=0, nf=0;
   Widget whist = NULL, *w = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SurfCont = SUMA_ADO_Cont(ado);
   SUMA_LHv("So(%p), SurfCont(%p), CmapModeMenu(%p)\n", 
            ado, SurfCont, SurfCont->CmapModeMenu);
   w = SurfCont->CmapModeMenu->mw;
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
         SUMA_SetCmapMode(ado, i);
         SUMA_RETURN(YUP);
     }
   }
   
   SUMA_RETURN(NOPE);
}

/*!
   This function will fail if the strings have been trunctated 
   Consider writing SetMenuChoiceUserData
*/ 
SUMA_Boolean SUMA_SetCmapMenuChoice(SUMA_ALL_DO *ado, char *str)
{
   static char FuncName[]={"SUMA_SetCmapMenuChoice"};
   int i, Nbutt = 0, nstr=0, nf=0;
   Widget whist = NULL, *w = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   SUMA_LHv("DO(%p), SurfCont(%p), SwitchCmapMenu(%p)\n", 
            ado, SurfCont, SurfCont?SurfCont->SwitchCmapMenu:NULL);
            
   if (!ado || !SurfCont || !SurfCont->SwitchCmapMenu) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   
   w = SurfCont->SwitchCmapMenu->mw;
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
      SUMA_S_Notev("ado(%p), SurfCont(%p), SwitchCmapMenu(%p), %s\n", 
            ado, SurfCont, SurfCont->SwitchCmapMenu, str);
      SUMA_RETURN(NOPE);
   }

   if (LocalHead) { 
      fprintf (SUMA_STDERR,"%s: The history is NAMED: %s (%d buttons total)\n", 
               FuncName, XtName(whist), Nbutt);
   } 
      
   nstr = strlen(str);
   /* Now search the widgets in w for a widget labeled str */
   for (i=0; i< SurfCont->SwitchCmapMenu->N_mw; ++i) {
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

int SUMA_SelectSwitchCmap_one( SUMA_ALL_DO *ado, SUMA_LIST_WIDGET *LW,
                               int ichoice, SUMA_Boolean CloseShop, int setmen)
{
   static char FuncName[]={"SUMA_SelectSwitchCmap_one"};
   SUMA_COLOR_MAP *CM = NULL;
   char *choice=NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !LW) SUMA_RETURN(0);
   
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
         if (!SUMA_SetCmapMenuChoice (ado, LW->ALS->clist[ichoice])) {
            SUMA_SL_Err("Failed in SUMA_SetCmapMenuChoice");
         }
         if (!SUMA_SwitchColPlaneCmap(ado, CM)) {
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
   SUMA_UpdateNodeLblField(ado);
   
   
   SUMA_RETURN(1);
} 

int SUMA_SelectSwitchCmap( SUMA_ALL_DO *ado, SUMA_LIST_WIDGET *LW,
                           int ichoice, SUMA_Boolean CloseShop, int setmen)
{
   static char FuncName[]={"SUMA_SelectSwitchCmap"};
   SUMA_SurfaceObject *SO=NULL, *SOC=NULL;
   SUMA_OVERLAYS *colpC=NULL, *colp = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !LW) SUMA_RETURN(0);
   
   if (!SUMA_SelectSwitchCmap_one(ado, LW, ichoice, CloseShop, setmen)) {
      SUMA_RETURN(0);
   }
   
   if (ado->do_type == SO_type) {
      SO = (SUMA_SurfaceObject *)ado;
      colp = SUMA_ADO_CurColPlane(ado);
      colpC = SUMA_Contralateral_overlay(colp, SO, &SOC);
      if (colpC && SOC) {
         SUMA_LHv("Found contralateral equivalent to:\n"
                      " %s and %s in\n"
                      " %s and %s\n",
                      SO->Label, CHECK_NULL_STR(colp->Label),
                      SOC->Label, CHECK_NULL_STR(colpC->Label));
         if (!SUMA_SelectSwitchCmap_one((SUMA_ALL_DO *)SOC, LW, ichoice, 0, 1)) {
            SUMA_S_Warn("Failed in contralaterality");
            SUMA_RETURN(0);
         }
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
   SUMA_ALL_DO *ado = NULL;
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean CloseShop = NOPE;
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
   int ichoice = -1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   ado = (SUMA_ALL_DO *)client_data;
   LW = SUMAg_CF->X->SwitchCmapLst;
   
   ichoice = SUMA_GetListIchoice(cbs, LW, &CloseShop);

   if (!SUMA_SelectSwitchCmap(ado, LW, ichoice, CloseShop, 1)) {
      SUMA_S_Err("glitch");
      SUMA_RETURNe;
   }

   SUMA_RETURNe;
}

SUMA_Boolean SUMA_SwitchColPlaneCmap(SUMA_ALL_DO *ado, SUMA_COLOR_MAP *CM)
{
   static char FuncName[]={"SUMA_SwitchColPlaneCmap"};
   SUMA_OVERLAYS *over = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   static int nwarn=0;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   if (!ado || !CM) { SUMA_RETURN(NOPE); }
   
   SurfCont = SUMA_ADO_Cont(ado);

   if (!SurfCont) { SUMA_RETURN(NOPE); }
   
   over = SUMA_ADO_CurColPlane(ado);
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
         SUMA_Set_Menu_Widget( SurfCont->DsetViewModeMenu,
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
   SurfCont->cmp_ren->FOV = SUMA_CMAP_FOV_INITIAL;
   SurfCont->cmp_ren->translateVec[0] = 
   SurfCont->cmp_ren->translateVec[1] = 
   SurfCont->cmp_ren->translateVec[2] = 0.0;

   /* update the color map display NOW, no workprocess crap. ZSS Mar. 7 08*/
   #if 0
   /* With this, the next call to SUMA_Remixedisplay,
   causes an error: glXSwapBuffers: no context for this drawable
   because SUMA_cmap_wid_handleRedisplay is still to be processed
   as SUMA_cmap_wid_postRedisplay puts it in as a workprocess.
   You need to force the immediate execution of 
   SUMA_cmap_wid_handleRedisplay which resets the context before 
   returning */
   SUMA_LH("Calling SUMA_cmap_wid_postRedisplay");
   SUMA_cmap_wid_postRedisplay(NULL, (XtPointer)ado, NULL);
   #else
   SUMA_cmap_wid_handleRedisplay((XtPointer)ado);
   #endif
   
   SUMA_LH("Calling SUMA_Remixedisplay on %s", ADO_LABEL(ado));          
   SUMA_Remixedisplay(ado);

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
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)cd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ){
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || ado == NULL ) SUMA_RETURNe ;

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
      if (!SUMA_DsetColSelectList(ado, 0, 0, 1)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else if (strcmp(XtName(w), "T") == 0){
      if (!SUMA_DsetColSelectList(ado, 1, 0, 1)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else if (strcmp(XtName(w), "B") == 0){
      if (!SUMA_DsetColSelectList(ado, 2, 0, 1)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else if (strcmp(XtName(w), "Cmp") == 0){
      if (!SUMA_CmapSelectList(ado, 0, 1)) {
         SUMA_SLP_Err("Failed to create DsetList");
         SUMA_RETURNe;
      }
   } else {
      SUMA_SLP_Err("wahtchyoutalkinaboutwillis?");
      SUMA_RETURNe;
   }
   SUMA_RETURNe;

}

void SUMA_CreateXhairWidgets(Widget parent, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_CreateXhairWidgets"};
   SUMA_ENTRY;
   
   if (!ado) {
      SUMA_RETURNe;
   }
   switch (ado->do_type) {
      case SO_type:
         SUMA_CreateXhairWidgets_SO(parent, ado);
         break;
      case GDSET_type:
         SUMA_S_Err("Should not create widgets for a DO that "
                    "can't be displayed without variant");
         SUMA_RETURNe;
      case CDOM_type:
         SUMA_CreateXhairWidgets_CO(parent, ado);
         SUMA_RETURNe;
         break;
      case GRAPH_LINK_type:
         SUMA_CreateXhairWidgets_GLDO(parent, ado);
         break;
      case TRACT_type:
         SUMA_CreateXhairWidgets_TDO(parent, ado);
         break;
      case MASK_type:
         SUMA_CreateXhairWidgets_MDO(parent, ado);
         break;
      case VO_type:
         SUMA_CreateXhairWidgets_VO(parent, ado);
         break;
      default:
         SUMA_S_Errv("Not ready for this beast %d (%s)\n",
            ado->do_type, SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         break;
   }
   SUMA_RETURNe;
}

void SUMA_CreateXhairWidgets_SO(Widget parent, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_CreateXhairWidgets_SO"};
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
   SUMA_X_SurfCont *SurfCont=NULL;
   Widget rcc;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || ado->do_type != SO_type || !(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_RETURNe;
   }
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
         "SurfCont->Xhair_Info->Xhr",
         Xhair_tit, NULL,
         Xhair_hint, NULL,
         Xhair_help, NULL,
         colw, YUP, SUMA_string,
         SUMA_XhairInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->XhairTable);
   }
   /* a table for a node's index */      
   SUMA_LH("Creating node table");
   {
      int colw[]={4, 6, 19};
      SUMA_CreateTable(rcc, 
         1, 3,
         "SurfCont->Xhair_Info->Node",
         Node_tit, NULL,
         Node_hint, NULL,
         Node_help, NULL,
         colw, YUP, SUMA_int,
         SUMA_NodeInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->NodeTable);
      /* disable the 3rd entry cell */
      SUMA_SetCellEditMode(SurfCont->NodeTable, 0, 2, 0);
   }
   /* a table for the triangle in focus */      
   SUMA_LH("Creating Face  table");
   {
      int colw[]={4, 6, 19}  ; 
      SUMA_CreateTable(rcc, 
         1, 3,
         "SurfCont->Xhair_Info->Tri",
         Face_tit, NULL,
         Face_hint, NULL,
         Face_help, NULL,
         colw, YUP, SUMA_int,
         SUMA_TriInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->FaceTable);
      /* disable the 3rd entry cell */
      SUMA_SetCellEditMode(SurfCont->FaceTable, 0, 2, 0);   
   }
   /* a table for the Dset values at node in focus */      
   SUMA_LH("Creating Dset Val  table");
   {
      int colw[]={ 4, 7, 7, 7};
      SUMA_CreateTable(rcc, 
         2, 4,
         "SurfCont->Xhair_Info->Val",
         Data_rtit, Data_tit,
         Data_rowhint, Data_colhint,
         Data_rowhelp, Data_colhelp,
         colw, NOPE, SUMA_float,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->DataTable);
   }
   /* a table for a node's label*/      
   SUMA_LH("Creating label  table");
   {
      int colw[]={4, 26};
      SUMA_CreateTable(rcc, 
         1, 2,
         "SurfCont->Xhair_Info->Lbl",
         Label_tit, NULL,
         Label_hint, NULL,
         Label_help, NULL, 
         colw, NOPE, SUMA_string,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->LabelTable);
   }      
   XtManageChild(rcc);
   SUMA_RETURNe;
   
}

void SUMA_CreateXhairWidgets_GLDO(Widget parent, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_CreateXhairWidgets_GLDO"};
   char *Xhair_tit[]=   {  "Xhr ", NULL};
   char *Xhair_hint[]=  {  "Crosshair coordinates.", NULL};
   char *Xhair_help[]=  {  SUMA_SurfContHelp_Xhr , NULL};
   char *Node_tit[]=    {  "Node"   , NULL};
   char *Node_hint[]=   {  "Closest Node index", NULL};
   char *Node_help[]=   {  SUMA_SurfContHelp_GNode , NULL};
   char *Edge_tit[]=    {  "Edge", NULL};
   char *Edge_hint[]=   {  "1- Edge index, 2- Nodes forming (directed) edge", 
                                    NULL};
   char *Edge_help[]=   {  SUMA_SurfContHelp_GEdge , NULL};
   char *Data_tit[]=    {  "    ", "Intens", "Thresh", "Bright" , NULL};
   char *Data_colhint[]=      {  "Data Values at Edge in Focus", 
                                 "Intensity (I) value", 
                                 "Threshold (T) value", 
                                 "Brightness modulation (B) value" , NULL};
   char *Data_colhelp[]=      {  SUMA_SurfContHelp_GEdgeValTblc0, 
                                 SUMA_SurfContHelp_GEdgeValTblc1, 
                                 SUMA_SurfContHelp_GEdgeValTblc2, 
                                 SUMA_SurfContHelp_GEdgeValTblc3 , NULL}; 
   
   char *Data_rtit[]=      {  "    ", "Val " , NULL};
   char *Data_rowhint[]=   {  "Data Values at Edge in Focus", 
                              "Data Values at Edge in Focus" , NULL};
   char *Data_rowhelp[]=   {  SUMA_SurfContHelp_GEdgeValTblr0, 
                              SUMA_SurfContHelp_GEdgeValTblr0 , NULL};
   
   char *Label_tit[]=   {  "Lbl ", NULL};
   char *Label_hint[]=  {  "Label of edge in focus", NULL};
   char *Label_help[]=  {  SUMA_SurfContHelp_GEdgeLabelTblr0 , NULL};
   SUMA_X_SurfCont *SurfCont=NULL;
   Widget rcc;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || ado->do_type != GRAPH_LINK_type || 
       !(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Errv("Something's amiss %p, type %d (%s) (must be GRAPH_LINK)\n",
                   ado, ado?ado->do_type:0, ADO_TNAME(ado));
      SUMA_RETURNe;
   }
   
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
         "GraphCont->Xhair_Info->Xhr",
         Xhair_tit, NULL,
         Xhair_hint, NULL,
         Xhair_help, NULL,
         colw, YUP, SUMA_string,
         SUMA_XhairInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->XhairTable);
   }
   /* a table for the edge in focus */      
   SUMA_LH("Creating Edge  table");
   {
      int colw[]={4, 6, 19}  ; 
      SUMA_CreateTable(rcc, 
         1, 3,
         "GraphCont->Xhair_Info->Edge",
         Edge_tit, NULL,
         Edge_hint, NULL,
         Edge_help, NULL,
         colw, YUP, SUMA_int,
         SUMA_NodeInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->NodeTable);
      /* disable the 3rd entry cell */
      SUMA_SetCellEditMode(SurfCont->NodeTable, 0, 2, 0);   
   }
   /* a table for a node's index */      
   SUMA_LH("Creating graph node table");
   {
      int colw[]={4, 6, 19};
      SUMA_CreateTable(rcc, 
         1, 3,
         "GraphCont->Xhair_Info->Node",
         Node_tit, NULL,
         Node_hint, NULL,
         Node_help, NULL,
         colw, YUP, SUMA_int,
         SUMA_GNodeInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->FaceTable);
      /* disable the 3rd entry cell */
      SUMA_SetCellEditMode(SurfCont->FaceTable, 0, 2, 0);
   }
   /* a table for the Dset values at node in focus */      
   SUMA_LH("Creating Dset Val  table");
   {
      int colw[]={ 4, 7, 7, 7};
      SUMA_CreateTable(rcc, 
         2, 4,
         "GraphCont->Xhair_Info->Val",
         Data_rtit, Data_tit,
         Data_rowhint, Data_colhint,
         Data_rowhelp, Data_colhelp,
         colw, NOPE, SUMA_float,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->DataTable);
   }
   /* a table for a node's label*/      
   SUMA_LH("Creating label  table");
   {
      int colw[]={4, 26};
      SUMA_CreateTable(rcc, 
         1, 2,
         "GraphCont->Xhair_Info->Lbl",
         Label_tit, NULL,
         Label_hint, NULL,
         Label_help, NULL, 
         colw, NOPE, SUMA_string,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->LabelTable);
   }      
   XtManageChild(rcc);
   SUMA_RETURNe;
   
}

void SUMA_CreateXhairWidgets_TDO(Widget parent, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_CreateXhairWidgets_TDO"};
   char *Xhair_tit[]=   {  "Xhr ", NULL};
   char *Xhair_hint[]=  {  "Crosshair coordinates.", NULL};
   char *Xhair_help[]=  {  SUMA_SurfContHelp_Xhr , NULL};
   char *I_tit[]=      { "Ind ", NULL };
   char *I_hint[] =    { "Point index in whole network", NULL };
   char *I_help[] =    { SUMA_TractContHelp_I, NULL };
   char *BTP_tit[]=    {  "BTP"   , NULL};
   char *BTP_hint[]=   {  "Bundle index in network, Tract index in bundle, "
                          "Point index in tract",
                           NULL};
   char *BTP_help[]=   {  SUMA_SurfContHelp_BTP , NULL};
   char *Data_tit[]=    {  "    ", "Intens", "Thresh", "Bright" , NULL};
   char *Data_colhint[]=      {  "Data values at tract point in focus", 
                                 "Intensity (I) value", 
                                 "Threshold (T) value", 
                                 "Brightness modulation (B) value" , NULL};
   char *Data_colhelp[]=      {  SUMA_TractContHelp_NodeValTblc0, 
                                 SUMA_SurfContHelp_NodeValTblc1, 
                                 SUMA_SurfContHelp_NodeValTblc2, 
                                 SUMA_SurfContHelp_NodeValTblc3 , NULL}; 
   
   char *Data_rtit[]=      {  "    ", "Val " , NULL};
   char *Data_rowhint[]=   {  "Data Values at point in focus", 
                              "Data Values at point in focus" , NULL};
   char *Data_rowhelp[]=   {  SUMA_SurfContHelp_NodeValTblr0, 
                              SUMA_SurfContHelp_NodeValTblr0 , NULL};
   
   char *Label_tit[]=   {  "Lbl ", NULL};
   char *Label_hint[]=  {  "Label at selected point", NULL};
   char *Label_help[]=  {  SUMA_TractContHelp_NodeLabelTblr0 , NULL};
   SUMA_X_SurfCont *SurfCont=NULL;
   Widget rcc, rcch;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || ado->do_type != TRACT_type || !(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_RETURNe;
   }
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
         "TractCont->Xhair_Info->Xhr",
         Xhair_tit, NULL,
         Xhair_hint, NULL,
         Xhair_help, NULL,
         colw, YUP, SUMA_string,
         SUMA_XhairInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->XhairTable);
   }
   rcch = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, rcc,
      XmNpacking, XmPACK_TIGHT,
      XmNleftAttachment,XmATTACH_FORM ,  
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight , 0 ,
      XmNmarginWidth  , 0 ,
      NULL);
   /* a table for a point's index */      
   SUMA_LH("Creating point table");
   {
      int colw[]={3, 5};
      SUMA_CreateTable(rcch, 
         1, 2,
         "TractCont->Xhair_Info->Ind",
         I_tit, NULL,
         I_hint, NULL,
         I_help, NULL,
         colw, YUP, SUMA_int,
         SUMA_NodeInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->NodeTable);
   }
   /* a table for a point's BTP index */      
   SUMA_LH("Creating BTP table");
   {
      int colw[]={4, 15};
      SUMA_CreateTable(rcch, 
         1, 2,
         "TractCont->Xhair_Info->BTP",
         BTP_tit, NULL,
         BTP_hint, NULL,
         BTP_help, NULL,
         colw, YUP, SUMA_string,
         SUMA_TpointInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->FaceTable);
   }
   XtManageChild(rcch);
   
   
   /* a table for the Dset values at node in focus */      
   SUMA_LH("Creating Dset Val  table");
   {
      int colw[]={ 4, 7, 7, 7};
      SUMA_CreateTable(rcc, 
         2, 4,
         "TractCont->Xhair_Info->Val",
         Data_rtit, Data_tit,
         Data_rowhint, Data_colhint,
         Data_rowhelp, Data_colhelp,
         colw, NOPE, SUMA_float,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->DataTable);
   }
   /* a table for a node's label*/      
   SUMA_LH("Creating label  table");
   {
      int colw[]={4, 26};
      SUMA_CreateTable(rcc, 
         1, 2,
         "TractCont->Xhair_Info->Lbl",
         Label_tit, NULL,
         Label_hint, NULL,
         Label_help, NULL, 
         colw, NOPE, SUMA_string,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->LabelTable);
   }      
   XtManageChild(rcc);
   SUMA_RETURNe;
}

void SUMA_CreateXhairWidgets_MDO(Widget parent, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_CreateXhairWidgets_MDO"};
   char *Xhair_tit[]=   {  "Xhr ", NULL};
   char *Xhair_hint[]=  {  "Crosshair coordinates.", NULL};
   char *Xhair_help[]=  {  SUMA_SurfContHelp_Xhr , NULL};
   char *I_tit[]=      { "Ind ", NULL };
   char *I_hint[] =    { "Point index in whole network", NULL };
   char *I_help[] =    { SUMA_TractContHelp_I, NULL };
   char *BTP_tit[]=    {  "BTP"   , NULL};
   char *BTP_hint[]=   {  "Bundle index in network, Tract index in bundle, "
                          "Point index in tract",
                           NULL};
   char *BTP_help[]=   {  SUMA_SurfContHelp_BTP , NULL};
   char *Data_tit[]=    {  "    ", "Intens", "Thresh", "Bright" , NULL};
   char *Data_colhint[]=      {  "Data Values at tract point in focus", 
                                 "Intensity (I) value", 
                                 "Threshold (T) value", 
                                 "Brightness modulation (B) value" , NULL};
   char *Data_colhelp[]=      {  SUMA_SurfContHelp_NodeValTblc0, 
                                 SUMA_SurfContHelp_NodeValTblc1, 
                                 SUMA_SurfContHelp_NodeValTblc2, 
                                 SUMA_SurfContHelp_NodeValTblc3 , NULL}; 
   
   char *Data_rtit[]=      {  "    ", "Val " , NULL};
   char *Data_rowhint[]=   {  "Data Values at point in focus", 
                              "Data Values at point in focus" , NULL};
   char *Data_rowhelp[]=   {  SUMA_SurfContHelp_NodeValTblr0, 
                              SUMA_SurfContHelp_NodeValTblr0 , NULL};
   
   char *Label_tit[]=   {  "Lbl ", NULL};
   char *Label_hint[]=  {  "Label at node in focus", NULL};
   char *Label_help[]=  {  SUMA_SurfContHelp_NodeLabelTblr0 , NULL};
   SUMA_X_SurfCont *SurfCont=NULL;
   Widget rcc, rcch;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || ado->do_type != MASK_type || !(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_RETURNe;
   }
   
   SUMA_S_Warn("Nothing done below");
   SUMA_RETURNe;
   
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
         "MaskCont->NOT_DONE->Xhr",
         Xhair_tit, NULL,
         Xhair_hint, NULL,
         Xhair_help, NULL,
         colw, YUP, SUMA_string,
         SUMA_XhairInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->XhairTable);
   }
   rcch = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, rcc,
      XmNpacking, XmPACK_TIGHT,
      XmNleftAttachment,XmATTACH_FORM ,  
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight , 0 ,
      XmNmarginWidth  , 0 ,
      NULL);
   /* a table for a point's index */      
   SUMA_LH("Creating point table");
   {
      int colw[]={3, 5};
      SUMA_CreateTable(rcch, 
         1, 2,
         "MaskCont->NOT_DONE->I",
         I_tit, NULL,
         I_hint, NULL,
         I_help, NULL,
         colw, YUP, SUMA_int,
         SUMA_NodeInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->NodeTable);
   }
   /* a table for a point's BTP index */      
   SUMA_LH("Creating BTP table");
   {
      int colw[]={4, 15};
      SUMA_CreateTable(rcch, 
         1, 2,
         "MaskCont->NOT_DONE->BTP",
         BTP_tit, NULL,
         BTP_hint, NULL,
         BTP_help, NULL,
         colw, YUP, SUMA_string,
         SUMA_TpointInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->FaceTable);
   }
   XtManageChild(rcch);
   
   
   /* a table for the Dset values at node in focus */      
   SUMA_LH("Creating Dset Val  table");
   {
      int colw[]={ 4, 7, 7, 7};
      SUMA_CreateTable(rcc, 
         2, 4,
         "MaskCont->NOT_DONE->Val",
         Data_rtit, Data_tit,
         Data_rowhint, Data_colhint,
         Data_rowhelp, Data_colhelp,
         colw, NOPE, SUMA_float,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->DataTable);
   }
   /* a table for a node's label*/      
   SUMA_LH("Creating label  table");
   {
      int colw[]={4, 26};
      SUMA_CreateTable(rcc, 
         1, 2,
         "MaskCont->NOT_DONE->Lbl",
         Label_tit, NULL,
         Label_hint, NULL,
         Label_help, NULL, 
         colw, NOPE, SUMA_string,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->LabelTable);
   }      
   XtManageChild(rcc);
   SUMA_RETURNe;
   
}

void SUMA_CreateXhairWidgets_VO(Widget parent, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_CreateXhairWidgets_VO"};
   char *Xhair_tit[]=   {  "Xhr ", NULL};
   char *Xhair_hint[]=  {  "Crosshair coordinates.", NULL};
   char *Xhair_help[]=  {  SUMA_SurfContHelp_Xhr , NULL};
   char *I_tit[]=      { "Ind ", NULL };
   char *I_hint[] =    { "Voxel 1D index in volume", NULL };
   char *I_help[] =    { SUMA_VolContHelp_I, NULL };
   char *BTP_tit[]=    {  "IJK"   , NULL};
   char *BTP_hint[]=   {  "Voxel 3D indices in volume",
                           NULL};
   char *BTP_help[]=   {  SUMA_SurfContHelp_IJK , NULL};
   char *Data_tit[]=    {  "    ", "Intens", "Thresh", "Bright" , NULL};
   char *Data_colhint[]=      {  "Data Values at voxel in focus", 
                                 "Intensity (I) value", 
                                 "Threshold (T) value", 
                                 "Brightness modulation (B) value" , NULL};
   char *Data_colhelp[]=      {  SUMA_VolContHelp_NodeValTblc0, 
                                 SUMA_SurfContHelp_NodeValTblc1, 
                                 SUMA_SurfContHelp_NodeValTblc2, 
                                 SUMA_SurfContHelp_NodeValTblc3 , NULL}; 
   
   char *Data_rtit[]=      {  "    ", "Val " , NULL};
   char *Data_rowhint[]=   {  "Data values at voxel in focus", 
                              "Data values at voxel in focus" , NULL};
   char *Data_rowhelp[]=   {  SUMA_SurfContHelp_NodeValTblr0, 
                              SUMA_SurfContHelp_NodeValTblr0 , NULL};
   
   char *Label_tit[]=   {  "Lbl ", NULL};
   char *Label_hint[]=  {  "Label at voxel in focus", NULL};
   char *Label_help[]=  {  SUMA_SurfContHelp_NodeLabelTblr0 , NULL};
   SUMA_X_SurfCont *SurfCont=NULL;
   Widget rcc, rcch;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || ado->do_type != VO_type || !(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_RETURNe;
   }
   
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
         "VolCont->Xhair_Info->Xhr",
         Xhair_tit, NULL,
         Xhair_hint, NULL,
         Xhair_help, NULL,
         colw, YUP, SUMA_string,
         SUMA_XhairInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->XhairTable);
   }
   rcch = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, rcc,
      XmNpacking, XmPACK_TIGHT,
      XmNleftAttachment,XmATTACH_FORM ,  
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight , 0 ,
      XmNmarginWidth  , 0 ,
      NULL);
   /* a table for a point's index */      
   SUMA_LH("Creating point table");
   {
      int colw[]={3, 6};
      SUMA_CreateTable(rcch, 
         1, 2,
         "VolCont->Xhair_Info->Ind",
         I_tit, NULL,
         I_hint, NULL,
         I_help, NULL,
         colw, YUP, SUMA_int,
         SUMA_NodeInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->NodeTable);
   }
   /* a table for a point's BTP index */      
   SUMA_LH("Creating IJK table");
   {
      int colw[]={4, 15};
      SUMA_CreateTable(rcch, 
         1, 2,
         "VolCont->Xhair_Info->IJK",
         BTP_tit, NULL,
         BTP_hint, NULL,
         BTP_help, NULL,
         colw, YUP, SUMA_string,
         SUMA_IJKInput, (void*)ado,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->FaceTable);
   }
   XtManageChild(rcch);
   
   
   /* a table for the Dset values at node in focus */      
   SUMA_LH("Creating Dset Val  table");
   {
      int colw[]={ 4, 7, 7, 7};
      SUMA_CreateTable(rcc, 
         2, 4,
         "VolCont->Xhair_Info->Val",
         Data_rtit, Data_tit,
         Data_rowhint, Data_colhint,
         Data_rowhelp, Data_colhelp,
         colw, NOPE, SUMA_float,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->DataTable);
   }
   /* a table for a voxel's label*/      
   SUMA_LH("Creating label  table");
   {
      int colw[]={4, 26};
      SUMA_CreateTable(rcc, 
         1, 2,
         "VolCont->Xhair_Info->Lbl",
         Label_tit, NULL,
         Label_hint, NULL,
         Label_help, NULL, 
         colw, NOPE, SUMA_string,
         NULL, NULL,
         NULL, NULL, 
         NULL, NULL,  
         SurfCont->LabelTable);
   }      
   XtManageChild(rcc);
   SUMA_RETURNe;
   
}

/* I suspect this will be a mixture of _VO and _SO */
void SUMA_CreateXhairWidgets_CO(Widget parent, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_CreateXhairWidgets_CO"};
   char *Xhair_tit[]=   {  "Xhr ", NULL};
   char *Xhair_hint[]=  {  "Crosshair coordinates.", NULL};
   char *Xhair_help[]=  {  SUMA_SurfContHelp_Xhr , NULL};
   char *I_tit[]=      { "Ind ", NULL };
   char *I_hint[] =    { "Voxel 1D index in volume", NULL };
   char *I_help[] =    { SUMA_VolContHelp_I, NULL };
   char *BTP_tit[]=    {  "IJK"   , NULL};
   char *BTP_hint[]=   {  "Voxel 3D indices in volume",
                           NULL};
   char *BTP_help[]=   {  SUMA_SurfContHelp_IJK , NULL};
   char *Data_tit[]=    {  "    ", "Intens", "Thresh", "Bright" , NULL};
   char *Data_colhint[]=      {  "Data Values at voxel in focus", 
                                 "Intensity (I) value", 
                                 "Threshold (T) value", 
                                 "Brightness modulation (B) value" , NULL};
   char *Data_colhelp[]=      {  SUMA_VolContHelp_NodeValTblc0, 
                                 SUMA_SurfContHelp_NodeValTblc1, 
                                 SUMA_SurfContHelp_NodeValTblc2, 
                                 SUMA_SurfContHelp_NodeValTblc3 , NULL}; 
   
   char *Data_rtit[]=      {  "    ", "Val " , NULL};
   char *Data_rowhint[]=   {  "Data values at voxel in focus", 
                              "Data values at voxel in focus" , NULL};
   char *Data_rowhelp[]=   {  SUMA_SurfContHelp_NodeValTblr0, 
                              SUMA_SurfContHelp_NodeValTblr0 , NULL};
   
   char *Label_tit[]=   {  "Lbl ", NULL};
   char *Label_hint[]=  {  "Label at voxel in focus", NULL};
   char *Label_help[]=  {  SUMA_SurfContHelp_NodeLabelTblr0 , NULL};
   SUMA_X_SurfCont *SurfCont=NULL;
   Widget rcc, rcch;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || ado->do_type != CDOM_type || !(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_RETURNe;
   }
   
   SUMA_S_Err("Fill me up scotty, premium dude, premium");

   SUMA_RETURNe;
}

void SUMA_CreateCmapWidgets(Widget parent, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_CreateCmapWidgets"};
   char slabel[100], wname[64]={"NoTsEt"}, *blk=NULL;
   Widget rct, rcc, rco;
   XtVarArgsList arglist=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Err("NULL input");
      SUMA_RETURNe;
   }  
   
   if (SurfCont->opts_rc) {
      SUMA_SL_Err("Non null opts_rc\nThis should not be.");
      SUMA_RETURNe;
   }
   
   if (ado->do_type == GRAPH_LINK_type) {
      blk = "GDset_Mapping";
   } else {
      blk = "Dset_Mapping";
   }
      
   SurfCont->opts_form = XtVaCreateWidget ("form",
      xmFormWidgetClass, parent,
      NULL);
   
   SurfCont->opts_rc = XtVaCreateWidget ("rowcolumn",
      xmRowColumnWidgetClass, SurfCont->opts_form,
      XmNpacking, XmPACK_TIGHT,
      XmNleftAttachment,XmATTACH_FORM ,  
      XmNorientation , XmHORIZONTAL ,
      XmNmarginHeight , 0 ,
      XmNmarginWidth  , 0 ,
      NULL);
   
   SUMA_LH("Creating the threshold bar widgets");
   { /* the threshold bar */
      rct = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SurfCont->opts_rc,
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
      SurfCont->thr_lb = XtVaCreateManagedWidget (slabel, 
                                          xmLabelWidgetClass, rct,
                                          XmNwidth, SUMA_SCALE_WIDTH,
                                          XmNrecomputeSize, False,   
                                             /* don't let it change size, 
                                             it messes up the slider */ 
                                          NULL);
      #else
      { 
         int colw[]={6};
         char *lhint[]={ "Threshold Value (append 'p' to set by p value, "
                         "'%' to set by percentile)", NULL};
         char *lhelp[]={ SUMA_SurfContHelp_SetThreshTblr0, NULL};
         if (!SurfCont->SetThrScaleTable->cells) {
            snprintf(wname, 63, "%s->%s->ThrVal", 
                     SUMA_do_type_2_contwname(SurfCont->do_type), blk);
            SUMA_CreateTable( rct,
                              1, 1, 
                              wname,
                              NULL, NULL,  
                              lhint, NULL,  
                              lhelp, NULL,  
                              colw, YUP, SUMA_float, 
                              SUMA_cb_SetScaleThr, (void *)ado,
                              NULL, NULL,
                              NULL, NULL,  
                              SurfCont->SetThrScaleTable);                                     
         }
      }
      #endif
      /* add a vertical scale for the intensity */
      SurfCont->thr_sc = XtVaCreateManagedWidget("Thr.",
                                          xmScaleWidgetClass, rct,
                                          XtVaNestedList, arglist,
                                          NULL);
#ifdef USING_LESSTIF
   if (LocalHead) fprintf(stderr,"\n========= setting width to %d\n",
                                 SUMA_SCALE_SLIDER_WIDTH);
   XtVaSetValues( SurfCont->thr_sc, 
                  XmNscaleWidth, SUMA_SCALE_SLIDER_WIDTH , NULL ) ;
#endif


      XtAddCallback (SurfCont->thr_sc, 
                     XmNvalueChangedCallback, 
                     SUMA_cb_set_threshold, 
                     (XtPointer) ado);
      
      XtAddCallback (SurfCont->thr_sc, 
                     XmNdragCallback, 
                     SUMA_cb_set_threshold_label, 
                     (XtPointer) ado); 
      snprintf(wname, 63, "%s->%s->Cmap->scale", 
               SUMA_do_type_2_contwname(SurfCont->do_type), blk);
      SUMA_Register_Widget_Help(SurfCont->thr_sc , 1,
                                wname,
                                "Set the threshold for 'T' values",
                                SUMA_SurfContHelp_ThrScale);
      
      /* put a string for the pvalue */
      sprintf(slabel,"p [N/A]\nq [N/A]");
      SurfCont->thrstat_lb = XtVaCreateManagedWidget ("font8", 
                                          xmLabelWidgetClass, rct,
                                          XmNwidth, SUMA_SCALE_WIDTH,
                                          XmNrecomputeSize, False,
                                          LABEL_ARG(slabel),
                                          XmNinitialResourcesPersistent, False ,
                                          NULL);
      
      snprintf(wname, 63, "%s->%s->Cmap->pval", 
               SUMA_do_type_2_contwname(SurfCont->do_type), blk);
      SUMA_Register_Widget_Help(SurfCont->thrstat_lb , 1,
                                wname,
                                "Nominal p-value per node; FDR q-value",
                                SUMA_SurfContHelp_ThreshStats);
      XtManageChild (rct);
   }/* the threshold bar */
                     
   if (arglist) XtFree(arglist); arglist = NULL;
   
   SUMA_LH("The colorbar");
   {/* the color bar */
      Widget rcc2;
      rcc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SurfCont->opts_rc,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmVERTICAL ,
         XmNmarginHeight , 0 ,
         XmNmarginWidth  , 0 ,
         NULL);
      
      sprintf(slabel,"   ");
      SurfCont->cmaptit_lb = XtVaCreateManagedWidget (slabel, 
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
      {
         #ifdef SUMA_MOTIF_GLXAREA
            SurfCont->cmp_ren->cmap_wid = XtVaCreateManagedWidget("glxarea",
                glwMDrawingAreaWidgetClass, rcc2,
                GLwNvisualInfo, SUMAg_SVv[0].X->VISINFO,
                XtNcolormap, SUMAg_SVv[0].X->CMAP,
                XmNwidth,   SUMA_CMAP_WIDTH,
                XmNheight,  SUMA_CMAP_HEIGHT,
                NULL);
         #else
            SurfCont->cmp_ren->cmap_wid = 
                  XtVaCreateManagedWidget("glxarea",
                                          glwDrawingAreaWidgetClass, rcc2,
                                       GLwNvisualInfo, SUMAg_SVv[0].X->VISINFO,
                                          XtNcolormap, SUMAg_SVv[0].X->CMAP,
                                          XmNwidth,   SUMA_CMAP_WIDTH,
                                          XmNheight,  SUMA_CMAP_HEIGHT,
                                          NULL);
         #endif
         snprintf(wname, 63, "%s->%s->Cmap->bar", 
                  SUMA_do_type_2_contwname(SurfCont->do_type), blk);
         SUMA_Register_Widget_Help(SurfCont->cmp_ren->cmap_wid ,1,
                                   wname,
                                   "Colorbar for 'I' values",
                                   SUMA_SurfContHelp_ColorBar);
         XtManageChild (rcc2);

         SUMA_LH("Callbacks on glxarea");
         /* add me some callbacks */
         XtAddCallback( SurfCont->cmp_ren->cmap_wid, 
                        GLwNginitCallback, SUMA_cmap_wid_graphicsInit, 
                        (XtPointer )ado);
         XtAddCallback( SurfCont->cmp_ren->cmap_wid, 
                        GLwNexposeCallback, SUMA_cmap_wid_expose, 
                        (XtPointer )ado);
         XtAddCallback( SurfCont->cmp_ren->cmap_wid, 
                        GLwNresizeCallback, SUMA_cmap_wid_resize, 
                        (XtPointer )ado);
         XtAddCallback( SurfCont->cmp_ren->cmap_wid, 
                        GLwNinputCallback, SUMA_cmap_wid_input, 
                        (XtPointer )ado);
      }  
      
      if (SUMAg_CF->Fake_Cmap) {
         #define NPANE_MIN        2
         #define NPANE_MAX       20
         #define PANE_WIDTH      15
         #define PANE_MIN_HEIGHT  5
         #define PANE_LOFF        6
         #define PANE_SPACING     2

         #define PANE_MAXMODE     2
         #define SASH_HNO         1
         Widget frm, pw;
         SUMA_S_Warn("Creating X11 cmap for snapshot taking only!");
         
         /*
         frm = XtVaCreateManagedWidget( "pbar" , xmFrameWidgetClass , rcc2 ,
                                     XmNshadowType , XmSHADOW_ETCHED_IN ,
                                  NULL ) ;

         pw = XtVaCreateManagedWidget( "pbar" , xmPanedWindowWidgetClass , frm ,
                                      XmNsashWidth , PANE_WIDTH-2*PANE_SPACING,
                                      XmNsashIndent , PANE_SPACING ,
                                      XmNsashHeight , SASH_HNO ,
                                      XmNmarginHeight , 0 ,
                                      XmNmarginWidth , 0 ,
                                      XmNspacing , PANE_SPACING ,
                                      XmNx , 0 , XmNy , 0 ,
                                      XmNtraversalOn, True  ,
                                      XmNinitialResourcesPersistent , False ,
                                   NULL ) ;
         SurfCont->Fake_pbar = XtVaCreateWidget(
                          "pbar" , xmDrawnButtonWidgetClass , pw ,
                              XmNpaneMinimum , PANE_MIN_HEIGHT ,
                              XmNallowResize , True ,
                              XmNheight , SUMA_CMAP_HEIGHT ,
                              XmNwidth , PANE_WIDTH,
                              XmNborderWidth , 0 ,
                              XmNmarginWidth , 0 ,
                              XmNmarginHeight , 0 ,
                              XmNhighlightThickness , 0 ,
                              XmNpushButtonEnabled , True ,
                              XmNuserData , (XtPointer)ado ,
                              XmNtraversalOn , True ,
                              XmNinitialResourcesPersistent , False ,
                            NULL ) ;
         */
         SurfCont->Fake_pbar = XmCreateDrawingArea(rcc2, "pbar", NULL, 0);
         XtVaSetValues(SurfCont->Fake_pbar, XmNheight , SUMA_CMAP_HEIGHT ,
                              XmNwidth , SUMA_CMAP_WIDTH,
                              XmNallowResize , False ,
                              NULL);
         XtManageChild (SurfCont->Fake_pbar);
         XtManageChild (rcc2);
         XtAddCallback( SurfCont->Fake_pbar, XmNexposeCallback, 
                        SUMA_PBAR_bigexpose_CB, (XtPointer )ado ) ;
         /* The following commands were part of a failed attempt
         at getting the rest of the widgets - sub-brick selectors
         etc. to appear when the glxarea drawing widget was not
         created. For some reason, little other than the X11 colormap
         would show if I did not create SurfCont->cmp_ren->cmap_wid
         above. Interestingly, commenting out the SurfCont->cmp_ren->cmap_wid
         callbacks above also had the same effect. 
         So the solution is to render both and make one super thin. It is 
         rendered in black anyway when the picture is snapped so it makes
         little difference in the end.                ZSS Nov 2014 */
         XtAddCallback( SurfCont->Fake_pbar, 
                        XmNresizeCallback, SUMA_PBAR_bigresize_CB, 
                        (XtPointer )ado);
         XtAddCallback( SurfCont->Fake_pbar, 
                        XmNinputCallback, SUMA_PBAR_biginput_CB, 
                        (XtPointer )ado);
         XtVaSetValues( SurfCont->cmp_ren->cmap_wid,
                        XmNheight , 1,
                        XmNwidth , 1,
                        NULL);
      }
      
      XtManageChild (rcc);
   }  /* the colorbar */
   
   /* The options will be created as needed, when colorplanes are switched.
   see SUMA_InitializeColPlaneShell */
   
   XtManageChild (SurfCont->opts_rc);
   XtManageChild (SurfCont->opts_form);
   
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
      if (menu[i].subitems) { 
         SUMA_SL_Err("Don't know how to free subitems yet."); }
   }
   SUMA_free(menu);
   
   SUMA_RETURN(NULL);
}

SUMA_MenuItem *SUMA_FormSwitchColMenuVector(SUMA_ALL_DO *ado, 
                                            int what, int *N_items)
{
   static char FuncName[]={"SUMA_FormSwitchColMenuVector"};
   SUMA_MenuItem *menu = NULL;
   int i, isarrow;
   void (*callback)();
   NI_element *nel = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!ado) { SUMA_SL_Err("NULL ado"); SUMA_RETURN (menu);}
   
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   
   if (!SurfCont) { SUMA_SL_Err("NULL SurfCont"); SUMA_RETURN (menu);}
   if (!curColPlane) { 
      SUMA_SL_Err("NULL curColPlane"); SUMA_RETURN (menu);}
   if (!curColPlane->dset_link) { 
      SUMA_SL_Err("NULL curColPlane->dset_link"); 
      SUMA_RETURN (menu);}

   nel = curColPlane->dset_link->dnel;
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
            SUMA_DsetColLabelCopy(curColPlane->dset_link, i, 1);
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
            SUMA_S_Err("Action %d unknown", i);
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

Widget SUMA_FindChildWidgetNamed(Widget w, char *name)
{
   static char FuncName[]={"SUMA_FindChildWidgetNamed"};
   Widget * children = NULL;
   int  num_children=0, num_children2=0, ic , Nbutt=0, kk=0;
   char *wn;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!w || !name) SUMA_RETURN(NULL);
   XtVaGetValues( w ,         XmNchildren    , &children ,
                              XmNnumChildren , &num_children , 
                              XmNbuttonCount, &Nbutt,
                              NULL ) ;
   for( ic=0 ; ic < num_children ; ic++ ){
      if ((wn = XtName(children[ic]))) {
         if (!strcmp(wn,name)) SUMA_RETURN(children[ic]);
      }
   }
   SUMA_RETURN(NULL);
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
   
   /* add the stop sign. That's what SUMA_BuildMenu uses to figure out \
   the number of elements */
   menu[N_maps].label = NULL;
      
   SUMA_RETURN (menu);
}

/* This one here, recalculates the p and q value for a new threshold
and displays the results on the widget
*/
void SUMA_UpdatePvalueField (SUMA_ALL_DO *ado, float thresh)   
{/* set the pvalue */ 
   static char FuncName[]={"SUMA_UpdatePvalueField"};
   float p[3], zval = -1.0;
   int statcode;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;  
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) { 
      SUMA_SL_Err("NULL ado");
      SUMA_RETURNe; 
   }
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
     
   if (!SurfCont || 
       !SurfCont->thr_sc ||
       !curColPlane ||
       !curColPlane->dset_link) { 
      SUMA_SL_Err("NULL SurfCont");
      SUMA_RETURNe; 
   }
     
   /* see if you can get the stat codes */
   if (!SUMA_GetDsetColStatAttr(  
            curColPlane->dset_link, 
            curColPlane->OptScl->tind, 
            &statcode,
            p, (p+1), (p+2))) {
      SUMA_LH("Error");        
   }else if (statcode) {
      SUMA_LHv("Have stats at sb %d\n"
               "statcode %d: %f %f %f\n", 
               curColPlane->OptScl->tind,
               statcode, p[0], p[1], p[2]);
      curColPlane->OptScl->ThreshStats[0] = 
            THD_stat_to_pval( thresh , statcode , p  ) ;
      
      SUMA_LHv("Have pval of %f\n", 
               curColPlane->OptScl->ThreshStats[0]);
      if( curColPlane->OptScl->ThreshStats[0] >= 0.0 ){
         SUMA_LH("zvaling ...\n")
         zval = SUMA_fdrcurve_zval( 
                           curColPlane->dset_link, 
                           curColPlane->OptScl->tind, 
                           thresh) ;
         if( zval > 0.0f ){
            curColPlane->OptScl->ThreshStats[1] = 
                     2.0*qg(zval) ;         /* convert z back to FDR q */
         }
      } 
   } else {
      /* no stats */
      curColPlane->OptScl->ThreshStats[0] = -1.0;
      curColPlane->OptScl->ThreshStats[1] = -1.0;
   }
   SUMA_LHv("statcode %d: %f %f %f\n"
            "Thresh %f, p %f, q %f\n", 
            statcode, p[0], p[1], p[2],
            thresh, 
            curColPlane->OptScl->ThreshStats[0], 
            curColPlane->OptScl->ThreshStats[1]);
   
   
   { /* form the text, a la afni */
      char buf[100]={"Rien"};
      float pval = curColPlane->OptScl->ThreshStats[0];
      float qval = curColPlane->OptScl->ThreshStats[1];
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
          strcpy(buf,"p=");
          SUMA_strncat(buf,qbuf+1,99);/*qbuf+1 skips leading 0*/
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
         strcat(buf,"\nq=") ; SUMA_strncat(buf,qbuf+1,99) ;
      } else {
         SUMA_strncat(buf,"\nq=N/A",99) ;
      }
     
      MCW_set_widget_label( SurfCont->thrstat_lb, buf );
   }
   
   SUMA_RETURNe;
}
         

void SUMA_SetScaleRange(SUMA_ALL_DO *ado, double range[2])   
{
   static char FuncName[]={"SUMA_SetScaleRange"};
   int min_v, max_v, scl, dec, cv=0;
   Widget w ;
   double dtmp, rmult;
   char slabel[100];
   static int nwarn = 0;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) { 
      SUMA_SL_Err("NULL ado");
      SUMA_RETURNe; 
   }
   SurfCont = SUMA_ADO_Cont(ado);
   curColPlane = SUMA_ADO_CurColPlane(ado);
   
   rmult = (double)SUMA_floatEnv("SUMA_Range_Multiplier", 0.0) ;
   if (rmult > 0.0f && rmult != 1.0f) {
      SUMA_LHv("Applying range multipler of %f\n", rmult);
      range[0] = range[0]*rmult;
      range[1] = range[1]*rmult;
   } 

   if (!SurfCont->thr_sc) { 
      SUMA_SL_Err("NULL widget");
      SUMA_RETURNe; 
   }
   
   w = SurfCont->thr_sc;
   
   if (range[1] <= range[0]) range[1] = range[0] + 1;
   
   if (curColPlane->OptScl->ThrMode == SUMA_ABS_LESS_THAN) {
      SUMA_LH("Absolutizing Threshold Range");
      if (fabs((double)range[0]) > fabs((double)range[1])) {
         range[1] = fabs((double)range[0]); range[0] = 0.0;
      } else {
         range[1] = fabs((double)range[1]); range[0] = 0.0;
      }
   } else if (curColPlane->OptScl->ThrMode == SUMA_LESS_THAN) {
   
   } else {
      SUMA_S_Err("Not ready for ThrModeR == %d", curColPlane->OptScl->ThrMode);
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
   dtmp = curColPlane->OptScl->ThreshRange[0] * pow(10.0, dec);
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
      curColPlane->OptScl->ThreshRange[0] = 
         (float)cv / pow(10.0, dec); 
   } else if (cv > max_v) {
      cv = max_v;
      curColPlane->OptScl->ThreshRange[0] = 
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
   if (curColPlane->OptScl->ThrMode != SUMA_ABS_LESS_THAN) {
      sprintf(slabel, "%5s", MV_format_fval((float)cv / pow(10.0, dec))); 
   } else if (curColPlane->OptScl->ThrMode != SUMA_LESS_THAN){
      /* used to use this:
      sprintf(slabel, "|%5s|", .... 
      but that does not work in the editable field ... */
      sprintf(slabel, "%5s", MV_format_fval((float)cv / pow(10.0, dec))); 
   } else {
         SUMA_S_Err("Not ready to handle ThrModeR of %d yet", 
                     curColPlane->OptScl->ThrMode);
   }
   /* SUMA_SET_LABEL(SurfCont->thr_lb,  slabel);*/
      SUMA_INSERT_CELL_STRING(SurfCont->SetThrScaleTable, 0,0,slabel); 
   
   SUMA_UpdatePvalueField (ado, 
                           curColPlane->OptScl->ThreshRange[0]);
   
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
   SUMA_ALL_DO *ado=NULL, *curDO=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   char str[100]={""};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (!sv) SUMA_RETURN(NOPE);
   
   /* Which abjects are visible in this SV ? */
   N_SOlist = SUMA_Selectable_ADOs(sv, dov, SOlist);
   for (i=0; i<N_SOlist; ++i) {
      SUMA_LHv("working %d/%d shown surfaces/DOs ...\n", i, N_SOlist);
      ado = (SUMA_ALL_DO *)dov[SOlist[i]].OP;
      SurfCont = SUMA_ADO_Cont(ado);
      curColPlane = SUMA_ADO_CurColPlane(ado);
      if (ado->do_type == MASK_type) {/* No cross hair tricks */
         SUMA_RETURN(YUP);
      }
      if (SurfCont) { /* does this surface have surface controller ? */
         /* is this controller, displaying information for that surface ? */
         SUMA_LHv("Working controller for %s\n", SUMA_ADO_Label(ado));
         if (!(curDO = SUMA_SurfCont_GetcurDOp(SurfCont))) {
            SUMA_LH("No curDOp"); /* that's OK, happens when controller not
                                     yet created for this DO */
            continue;
         }
         if (curDO == ado) {
           /* OK, show the coordinate */
            SUMA_LHv("Setting cross hair at %f %f %f\n", 
                                    sv->Ch->c[0],sv->Ch->c[1],sv->Ch->c[2]);
            SUMA_XHAIR_STRING(sv->Ch->c, str);
            SUMA_LH("%s",str);
            XtVaSetValues(SurfCont->XhairTable->cells[1], 
                           XmNvalue, str, NULL);
            SUMA_UpdateCrossHairNodeLabelField(sv);
         }
      }
      
   }
   
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_UpdateNodeField(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_UpdateNodeField"};
   int i=0;
   SUMA_OVERLAYS *Sover=NULL, *targetSover=NULL;
   NI_group *ngr=NULL;
   DListElmt *el=NULL;
   SUMA_CALLBACK *cb=NULL;
   char *targetSO_idcode=NULL, *targetSover_name=NULL, *lbls=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) SUMA_RETURN(NOPE);
   SUMA_LHv("Entry, SurfCont=%p, ado %p\n",
            ado,   SurfCont);
   if (LocalHead) SUMA_DUMP_TRACE("Who Called Me?"); 
   if (!SUMA_isSurfContWidgetCreated(SurfCont)) {
      /* No controller created yet, return quietly */
      SUMA_RETURN(YUP);
   }
   Sover = SUMA_ADO_CurColPlane(ado); 
   SUMA_LHv("Sover %p, ado type %d %s, callbacks %p, HoldClickCallbacks %d\n", 
         Sover, ado->do_type, SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type),
         SUMAg_CF->callbacks, SUMAg_CF->HoldClickCallbacks); 
   switch (ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *curSO=NULL, *targetSO=NULL, *SO=NULL;
         SO = (SUMA_SurfaceObject *)ado;
         if (!(curSO =(SUMA_SurfaceObject *)SUMA_SurfCont_GetcurDOp(SurfCont))) {
            SUMA_S_Err("Failed to get curDOp");
            SUMA_RETURN(NOPE);
         }
   
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
                  if (!SUMA_ExecuteCallback(cb, 1, ado, 0)) {
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

         if (SUMA_isRelated_SO(SO, curSO, 1)) {
            SUMA_LH( "Updating GUI Node Fields, "
                     "whereami is handled in SUMA_UpdateNodeLblField");
            SUMA_UPDATE_ALL_NODE_GUI_FIELDS(ado);
         } else {
            SUMA_LH("No GUI Node Field Updates, but may use a whereami update");
            if (SUMAg_CF->X->Whereami_TextShell) {
               lbls = SUMA_GetLabelsAtSelection(ado, SO->SelectedNode, -1);
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
                                          (SUMA_ALL_DO *)SO, 
                                          SO->SelectedNode);
               }
            }
         } else {
            Sover = SO->SurfCont->curColPlane;
            if (     Sover 
                     && Sover->dset_link 
                     && Sover->rowgraph_mtd ) {
                  SUMA_OverlayGraphAtNode(Sover, 
                                          (SUMA_ALL_DO *)SO, 
                                          SO->SelectedNode);
               }
         }
         break; }
      case GDSET_type: {
         SUMA_S_Err("Should not call with this type DO because without\n"
                    "a variant it cannot be displayed");
         SUMA_RETURN(NOPE);
         break; }
      case CDOM_type:
      case VO_type:
      case TRACT_type:
      case GRAPH_LINK_type:
         {
         SUMA_ALL_DO *curado=NULL;
         if (LocalHead && ado->do_type ==  GRAPH_LINK_type) {
            SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
            SUMA_DSET *dset=NULL;
            if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
               SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                           SUMA_ADO_Label(ado));
               SUMA_RETURN(NOPE);
            }
         }
         
         if (!(curado = SUMA_SurfCont_GetcurDOp(SurfCont))) {
            SUMA_S_Err("Failed to get curDOp");
            break;
         }
         SUMA_LHv("Working %s, curado %s on label %s\n", 
                  ADO_TNAME(ado), ADO_TNAME(curado), ADO_LABEL(ado));
   
         /* Do we have click callbacks pending? */
         if (SUMAg_CF->callbacks && !SUMAg_CF->HoldClickCallbacks) {
            el = dlist_head(SUMAg_CF->callbacks);
            while (el) {
               cb = (SUMA_CALLBACK *)el->data;
               if (  cb->event == SUMA_NEW_NODE_ACTIVATE_EVENT && 
                     cb->active > 0 && 
                     cb->pending) {
                  SUMA_S_Warnv("Click callback %s disabled on graphs\n", 
                               cb->FunctionName);
                  #if 0 /* will need to deal with this when the need arises */
                  if (!SUMA_ExecuteCallback(cb, 1, ado, 0)) {
                     SUMA_S_Err("Failed to execute callback");
                     break;
                  }
                  #endif
               } else {
                  if (cb->active == -1) {
                     SUMA_LH("Callback inactive");
                  }
               }
               el = dlist_next(el);
            }
         } 

         if (SUMA_isRelated(ado, curado, 1)) {
            SUMA_LH( "Updating GUI Node Fields, "
                     "whereami is handled in SUMA_UpdateNodeLblField");
            SUMA_UPDATE_ALL_NODE_GUI_FIELDS(ado);
         } else {
            SUMA_LH("No GUI Node Field Updates, but may use a whereami update");
            if (SUMAg_CF->X->Whereami_TextShell) {
               lbls = SUMA_GetLabelsAtSelection(ado,
                                         SUMA_ADO_SelectedDatum(ado, NULL, NULL),
                                             SUMA_ADO_SelectedSecondary(ado));
               if (lbls) SUMA_free(lbls); lbls = NULL;
            }
         }
         
         SUMA_LH("On to graphing");
         if (  !SurfCont->ShowCurForeOnly || 
                        SurfCont->GraphHidden) {   /* graph updating can be done 
                                                      for all planes */
            for (i=0; i<SUMA_ADO_N_Overlays(ado); ++i) {
               Sover = SUMA_ADO_Overlay(ado,i);
               if (     Sover 
                     && Sover->dset_link 
                     && Sover->rowgraph_mtd ) {
                  SUMA_OverlayGraphAtNode(Sover, 
                                          ado, 
                                     SUMA_ADO_SelectedDatum(ado, NULL, NULL));
               }
            }
         } else {
            Sover = SurfCont->curColPlane;
            if (     Sover 
                     && Sover->dset_link 
                     && Sover->rowgraph_mtd ) {
                  SUMA_OverlayGraphAtNode(Sover, 
                                          ado, 
                                     SUMA_ADO_SelectedDatum(ado, NULL, NULL));
               }
         }
         SUMA_RETURN(YUP);
         break; }
      case MASK_type:
         SUMA_S_Warn("Anything to be done here for masks?");
         SUMA_RETURN(YUP);   
      default:
         SUMA_S_Errv("Nothing to do with %s\n",
               SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         SUMA_RETURN(NOPE);
   }
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_UpdatePointField(SUMA_ALL_DO*ado)
{
   static char FuncName[]={"SUMA_UpdatePointField"};
   int i=0;
   SUMA_OVERLAYS *Sover=NULL;
   char str[100];
   float *fv=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (!ado || !(SurfCont = SUMA_ADO_Cont(ado))) SUMA_RETURN(NOPE);
   SUMA_LHv("Entry, SurfCont=%p, ado %p\n",
            ado,   SurfCont); 
   if (!SUMA_isSurfContWidgetCreated(SurfCont)) {
      /* No controller created yet, return quietly */
      SUMA_RETURN(YUP);
   }
   Sover = SUMA_ADO_CurColPlane(ado); 
   SUMA_LHv("Sover %p, ado type %d %s\n", 
         Sover, ado->do_type, SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type)); 
   switch (ado->do_type) {
      case SO_type: {
         SUMA_S_Err("Not for SOs this!");
         break; }
      
      case GDSET_type: {
         SUMA_S_Err("Should not call with this type DO because without\n"
                    "a variant it cannot be displayed");
         SUMA_RETURN(NOPE);
         break; }
      case GRAPH_LINK_type:
         {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         SUMA_GRAPH_SAUX *GSaux=NULL;
         SUMA_ALL_DO *curado=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            SUMA_RETURN(NOPE);
         }
          if (!(curado = SUMA_SurfCont_GetcurDOp(SurfCont))) {
            SUMA_S_Err("Failed to get curDOp");
            break;
         }
         GSaux = SDSET_GSAUX(dset);
         SUMA_LHv("Working %s (%s), curado %s (%s) on dset %s\n", 
                  ADO_TNAME(ado), SUMA_ADO_Label(ado),
                  ADO_TNAME(curado), SUMA_ADO_Label(curado), 
                  SDSET_LABEL(dset));
   
         if (!SurfCont->FaceTable||
             !SurfCont->FaceTable->num_value) { /* table widgets not set yet ?*/
            SUMA_RETURN(NOPE);
         }

         if (SUMA_isRelated(ado, curado, 1)) {
            SUMA_LH( "Updating GUI Point Fields, ");
            if (curado == ado) {
               if (GSaux->PR->iAltSel[SUMA_ENODE_0] >= 0) {
                  sprintf(str, "%ld", GSaux->PR->iAltSel[SUMA_ENODE_0]);
                  SurfCont->FaceTable->num_value[1] = 
                        GSaux->PR->iAltSel[SUMA_ENODE_0];
                  XtVaSetValues(SurfCont->FaceTable->cells[1], 
                                 XmNvalue, str, NULL);
                  fv = SUMA_GDSET_NodeXYZ(dset, GSaux->PR->iAltSel[SUMA_ENODE_0],
                                          SUMA_ADO_variant(ado),NULL);
                  sprintf(str, "%f, %f, %f", fv[0], fv[1], fv[2]); 
               
                  XtVaSetValues(SurfCont->FaceTable->cells[2], 
                                 XmNvalue, str, NULL);
               } else {
                  XtVaSetValues(SurfCont->FaceTable->cells[1], 
                                 XmNvalue, "-1", NULL);
                  SurfCont->FaceTable->num_value[1] = -1;
                  XtVaSetValues(SurfCont->FaceTable->cells[2], 
                          XmNvalue, "x, x, x", NULL);
               }
            }
         }

         SUMA_RETURN(YUP);
         break; }
      
      default:
         SUMA_S_Errv("Nothing to do with %s\n",
               SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         SUMA_RETURN(NOPE);
   }
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_UpdateNodeNodeField(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_UpdateNodeNodeField"};
   char str[101];
   SUMA_X_SurfCont *SurfCont=NULL;
   int SelectedNode, ivsel[SUMA_N_IALTSEL_TYPES];
   float *fv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 

   if (!ado) { 
      SUMA_SL_Err("NULL ado");
      SUMA_RETURN(NOPE); 
   }
   SurfCont = SUMA_ADO_Cont(ado);
   SelectedNode = SUMA_ADO_SelectedDatum(ado, NULL, NULL);
   SUMA_LH("Working %d on %s\n", SelectedNode, ADO_LABEL(ado));
   if (!SurfCont || !SurfCont->NodeTable) SUMA_RETURN(NOPE);
   if (SelectedNode < 0 || SelectedNode > SUMA_ADO_Max_Datum_Index(ado)) 
                                          SUMA_RETURN(NOPE);
   if (!SurfCont->NodeTable->num_value) { /* Table widgets not created yet? */
      SUMA_RETURN(NOPE);
   }
   sprintf(str, "%d", SelectedNode);
   SurfCont->NodeTable->num_value[1] = SelectedNode;
   XtVaSetValues(SurfCont->NodeTable->cells[1], XmNvalue, str, NULL);
   switch (ado->do_type) {
      case SO_type:
         fv = SUMA_ADO_DatumXYZ(ado,SelectedNode, NULL);
         sprintf(str, "%s, ", 
                     MV_format_fval2(fv[0], 7));
         SUMA_strncat(str, MV_format_fval2(fv[1], 7), 100);
         SUMA_strncat(str, ", ", 100);
         SUMA_strncat(str, MV_format_fval2(fv[2], 7), 100 );
         XtVaSetValues(SurfCont->NodeTable->cells[2], XmNvalue, str, NULL);
         break;
      case GRAPH_LINK_type: {
         int i0, i1;
         SUMA_DSET *dset=SUMA_find_GLDO_Dset((SUMA_GraphLinkDO*)ado);
         SUMA_GRAPH_SAUX *GSaux=SDSET_GSAUX(dset);
         if (SUMA_GDSET_SegIndexToPoints(dset, SelectedNode,
                                          &i0, &i1, NULL)) {
            sprintf(str, "%d, %d", i0, i1);
         } else {
            sprintf(str, "-,-");
         }
         XtVaSetValues(SurfCont->NodeTable->cells[2], XmNvalue, str, NULL);
         break; }
      case TRACT_type: {
         if (!SurfCont->FaceTable) SUMA_RETURN(NOPE); 
         SUMA_ADO_SelectedDatum(ado, (void *)ivsel, NULL);
         sprintf(str, "%d, %d, %d", 
                 ivsel[SUMA_NET_BUN], ivsel[SUMA_BUN_TRC], ivsel[SUMA_TRC_PNT]);
         XtVaSetValues(SurfCont->FaceTable->cells[1], XmNvalue, str, NULL);
         break; }
      case MASK_type: {
         SUMA_S_Warn("Anything for this?");
         break; }
      case VO_type: {
         if (!SurfCont->FaceTable) SUMA_RETURN(NOPE); 
         SUMA_ADO_SelectedDatum(ado, (void *)ivsel, NULL);
         sprintf(str, "%d, %d, %d", 
                 ivsel[SUMA_VOL_I], ivsel[SUMA_VOL_J], ivsel[SUMA_VOL_K]);
         XtVaSetValues(SurfCont->FaceTable->cells[1], XmNvalue, str, NULL);
         break; }
      default:
         SUMA_S_Errv("No love for %s\n", ADO_TNAME(ado));
         break;
   }

   SUMA_RETURN(YUP);
}

/* A lazy wrapper for SUMA_FormNodeValFieldStrings to return
   values but not the strings.
   This function is not very efficient, since all the strings
   are formed and then discarded.
*/
SUMA_Boolean SUMA_GetNodeValsAtSelection(SUMA_ALL_DO *ado, 
               SUMA_DSET *dset, int Node,
               int find, int tind, int bind,
               double *I, double *T, double *B) 
{
   static char FuncName[] = {"SUMA_GetNodeValsAtSelection"};
   char **sar=NULL;
   int i;
   
   SUMA_ENTRY;
   
   sar = SUMA_FormNodeValFieldStrings(ado, dset, Node, find, tind, bind,
                                      1, I, T, B);
   if (!sar) SUMA_RETURN(NOPE);
   for (i=0; i<3; ++i) SUMA_ifree(sar[i]);
   SUMA_ifree(sar);
   
   SUMA_RETURN(YUP);                  
}

char **SUMA_FormNodeValFieldStrings(SUMA_ALL_DO *ado, 
                                 SUMA_DSET *dset, int Node,
                                 int find, int tind, int bind,
                                 int dec, double *I, double *T, double *B) 
{
   static char FuncName[]={"SUMA_FormNodeValFieldStrings"};
   char **sar=NULL;
   double dval;
   int Found = -1;
   SUMA_DATUM_LEVEL lev = 0;
   int Max_Node_Index = -1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !dset) SUMA_RETURN(sar);
   if (I) *I=-1.0;
   if (T) *T=-1.0;
   if (B) *B=-1.0;
   Max_Node_Index = SUMA_ADO_Max_Datum_Index(ado);
   
   /* What datum level do we have here ? */
   switch ((lev = SUMA_sdset_datum_level(dset))) {
      case SUMA_ELEM_DAT:
         SUMA_LH("Datum %d is expected to refer to an elementary datum level",
                 Node);
         break;
      case SUMA_LEV1_DAT:
         SUMA_LH("Datum %d is expected to refer to a level1 datum", Node); 
         break;
      case SUMA_LEV2_DAT:
         SUMA_LH("Datum %d is expected to refer to a level2 datum", Node);
         break;
      default:
         SUMA_S_Err("You're not on the level %d", lev);
         break;
   }
   
   /* 1- Where is this node in the data set ? */
   if (Node > -1) {
      Found = SUMA_GetNodeRow_FromNodeIndex_s(  dset, Node, Max_Node_Index );
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
         if (I) *I = dval;
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
         if (T) *T = dval;
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
         if (B) *B = dval;
      } else {
         SUMA_SL_Err("Failed to get str_brt");
         sar[2] = SUMA_copy_string("X");
      }
   } 
      
   SUMA_RETURN(sar);
}

/*!
   Like SUMA_ADO_SelectedDatum, except that 
if the overlay plane is for a non-elementary 
datum, the datum selection is adjusted properly
   Set Sover to NULL if you want to use
   the current overlay
*/
int SUMA_ADO_ColPlane_SelectedDatum(SUMA_ALL_DO *ado, SUMA_OVERLAYS *Sover)
{
   static char FuncName[]={"SUMA_ADO_ColPlane_SelectedDatum"};
   int SelectedNode = -1, ivsel[SUMA_N_IALTSEL_TYPES];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!ado) {
      SUMA_LH("Null ado");
      SUMA_RETURN(-1);
   }
   
   if (!Sover) Sover = SUMA_ADO_CurColPlane(ado);
   if (!Sover) {
      SUMA_LH("Null Sover and no current overlay plane");
      SUMA_RETURN(-1);
   }
   
   SelectedNode = SUMA_ADO_SelectedDatum(ado, (void *)ivsel, NULL);
   SUMA_LH("Selection: %d -- %d %d %d %d",
               SelectedNode, ivsel[0], ivsel[1], ivsel[2], ivsel[3]);
   if (Sover->dtlvl != SUMA_ELEM_DAT) {
      switch (ado->do_type) {
         case TRACT_type:
            if (Sover->dtlvl == SUMA_LEV1_DAT) 
                     SelectedNode = ivsel[SUMA_NET_TRC];
            else if (Sover->dtlvl == SUMA_LEV2_DAT) 
                                 SelectedNode = ivsel[SUMA_NET_BUN];
            break;
         default:
            SUMA_S_Err("Not ready for non-elementary datum for type %s\n",
                       ADO_TNAME(ado));
            break;
      }
   }
   
   SUMA_RETURN(SelectedNode);
}

SUMA_Boolean SUMA_UpdateNodeValField(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_UpdateNodeValField"};
   char **sar=NULL;
   double I, T, B;
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   int SelectedNode = -1;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!ado) {
      SUMA_LH("Null ado");
      SUMA_RETURN(NOPE);
   }

   Sover = SUMA_ADO_CurColPlane(ado);
   if (!Sover) {
      SUMA_LH("Null Sover");
      SUMA_RETURN(NOPE);
   }
   SurfCont = SUMA_ADO_Cont(ado);
   if (!SurfCont || !SurfCont->DataTable || 
       !SurfCont->DataTable->cells) SUMA_RETURN(NOPE);
   
   SelectedNode = SUMA_ADO_ColPlane_SelectedDatum(ado, Sover);
   if (!(sar = SUMA_FormNodeValFieldStrings(ado, Sover->dset_link, 
                           SelectedNode, 
                           Sover->OptScl->find, 
                           Sover->OptScl->tind,
                           Sover->OptScl->bind, 0, &I, &T, &B))) {
       SUMA_LH("Failed to get strings");
   } else {
      SUMA_LH("Got strings");
   }
   if (sar && sar[0]) {
      SUMA_INSERT_CELL_STRING(SurfCont->DataTable, 1, 1, sar[0]);
      SUMA_free(sar[0]); 
   } else {
      SUMA_INSERT_CELL_STRING(SurfCont->DataTable, 1, 1, "Err");
   }
   /* Set the table num values too, otherwise you can't read out
      the numerical values if you wish and that's just a shame 
      for numerical tables such as this one. */
   SUMA_SET_CELL_VALUE(SurfCont->DataTable, 1, 1, I);
   if (sar && sar[1]) {
      SUMA_INSERT_CELL_STRING(SurfCont->DataTable, 1, 2, sar[1]);
      SUMA_free(sar[1]); 
   } else {
      SUMA_INSERT_CELL_STRING(SurfCont->DataTable, 1, 2, "Err");
   }
   SUMA_SET_CELL_VALUE(SurfCont->DataTable, 1, 1, T);
   if (sar && sar[2]) {
      SUMA_INSERT_CELL_STRING(SurfCont->DataTable, 1, 3, sar[2]);
      SUMA_free(sar[2]); 
   } else {
      SUMA_INSERT_CELL_STRING(SurfCont->DataTable, 1, 3, "Err");
   }
   SUMA_SET_CELL_VALUE(SurfCont->DataTable, 1, 1, B);
   if (sar) SUMA_free(sar); sar = NULL;

   SUMA_RETURN(YUP);
}

/*!< Get values at current selection on current plane
     if (fromtable) then read the values from the 
     displayed table structure.
*/
SUMA_Boolean SUMA_GetValuesAtSelection(SUMA_ALL_DO *ado, int fromtable,
                                       float *I, float *T, float *B)
{
   static char FuncName[]={"SUMA_GetValuesAtSelection"};
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   int SelectedNode = -1;
   double II=0.0, TT=0.0, BB=0.0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) {
      SUMA_LH("Null ado");
      SUMA_RETURN(NOPE);
   }

   Sover = SUMA_ADO_CurColPlane(ado);
   if (!Sover) {
      SUMA_LH("Null Sover");
      SUMA_RETURN(NOPE);
   }
   SurfCont = SUMA_ADO_Cont(ado);
   if (!SurfCont) SUMA_RETURN(NOPE);
   
   SelectedNode = SUMA_ADO_ColPlane_SelectedDatum(ado, Sover);
   
   if (fromtable && (!SurfCont->DataTable || 
          !SurfCont->DataTable->cells)) {
          SUMA_LH("Controller not initialized, "
                  "cannot retrieve values from table, trying without");
      fromtable = 0;
   }
   if (fromtable) {
      if (!SurfCont->DataTable || 
          !SurfCont->DataTable->cells) {
            SUMA_S_Err("Controller not initialized, "
                       "cannot retrieve values from table");
            SUMA_RETURN(NOPE);
      }
      /* Check that SelectedNode matches that in the node index table too */
      if (SelectedNode != SurfCont->NodeTable->num_value[1]) { 
         /* table not updated yet perhaps  */
         SUMA_S_Note("Forced update of value fields (%d) (%f)to be safe", 
                     SelectedNode, SurfCont->NodeTable->num_value[1]);
         SUMA_UpdateNodeValField(ado);
      }
      SUMA_LH("Fetching from table");
      if (I) SUMA_GET_CELL_VALUE(SurfCont->DataTable, 1, 1, *I);
      if (T) SUMA_GET_CELL_VALUE(SurfCont->DataTable, 1, 2, *T);
      if (B) SUMA_GET_CELL_VALUE(SurfCont->DataTable, 1, 3, *B);
   } else {
      if (!SUMA_GetNodeValsAtSelection(ado, Sover->dset_link, 
                           SelectedNode, 
                           Sover->OptScl->find, 
                           Sover->OptScl->tind,
                           Sover->OptScl->bind,
                           &II, &TT, &BB)) {
             
         SUMA_S_Err("Failed to get sel values");
         SUMA_RETURN(NOPE);
      }
      if (I) *I = II;
      if (T) *T = TT;
      if (B) *B = BB;
   }
   SUMA_RETURN(YUP);
}

char *SUMA_GetLabelsAtSelection(SUMA_ALL_DO *ado, int node, int sec) 
{
   static char FuncName[]={"SUMA_GetLabelsAtSelection"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURN(NULL);
   
   switch (ado->do_type) {
      case SO_type:
         SUMA_RETURN(SUMA_GetLabelsAtSelection_ADO(ado,node, sec));
         break;
      case GDSET_type:
         SUMA_S_Warn("Not ready to return labels for dsets, and should I be?");
         break;
      case CDOM_type:
      case VO_type:
      case MASK_type:
      case TRACT_type:
      case GRAPH_LINK_type:
         SUMA_RETURN(SUMA_GetLabelsAtSelection_ADO(ado,node, sec));
         break;
      default:
         SUMA_LHv("No labels ready for type %s\n",
            SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         break;
   }
   
   SUMA_RETURN(NULL);
}

/* Do we have labels at this location ? 
   node is the 'datum' index, primary selection - think node on surface,
                                                  or edge on graph
   sec is the secondary selection index - think FaceSet on surface,
                                                  or node on graph*/
char *SUMA_GetLabelsAtSelection_ADO(SUMA_ALL_DO *ado, int node, int sec) 
{
   static char FuncName[]={"SUMA_GetLabelsAtSelection_ADO"};
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
   
   if (!ado) SUMA_RETURN(NULL);
   
   /* Any clusters? */
   SUMA_LHv("on ADO %s, looking for labels for selection %d and secondary %d\n", 
            SUMA_ADO_Label(ado), node, sec);
   
   if (ado->do_type == SO_type) {
      SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)ado;
      el = dlist_head(SUMAg_CF->DsetList);
      while (el) {
         dd = (SUMA_DSET*)el->data;
         if (SUMA_isDsetRelated(dd, SO) &&
             (colplane = SUMA_Fetch_OverlayPointerByDset (
                                 (SUMA_ALL_DO*)SO, dd, &OverInd)) &&
             colplane->OptScl) {
            SUMA_LHv("Have Dset %s related to SO\n", SDSET_LABEL(dd));
            /* is dd a LabelDset ? */
            if (  (SUMA_is_Label_dset(dd, NULL) || 
                   SUMA_is_Label_dset_col(dd,colplane->OptScl->find)) 
                && node >= 0 ) {
               SUMA_LHv("dset %s will work with SO", SDSET_LABEL(dd));
               key = SUMA_GetDsetNodeValInCol2( dd, colplane->OptScl->find, 
                                                node, -1);
               /* get the overlay for that dset */
               if (key >= 0) {
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
   }
   
  
   if ((Sover = SUMA_ADO_CurColPlane(ado))) {
      if (SDSET_TYPE(Sover->dset_link) != SUMA_NODE_RGB ) {/* Have labels */
         if (!(sar = SUMA_FormNodeValFieldStrings(ado,       
                                 Sover->dset_link, 
                                 node, 
                                 Sover->OptScl->find, 
                                 Sover->OptScl->tind,
                                 Sover->OptScl->bind, 5,
                                 NULL, NULL, NULL))) {
            SUMA_LH("No Sar");
         } else if (1) { /* include sub-brick labels */
            SUMA_LH("Sar: %s %s %s", 
                   SUMA_CHECK_NULL_STR(sar[0]),SUMA_CHECK_NULL_STR(sar[1]),
                   SUMA_CHECK_NULL_STR(sar[2]));
            if (lbls) lbls = SUMA_append_replace_string(lbls,"\n","",1);
            if (sar[0] && sar[1] && sar[2]) {
               if (!strcmp(sar[0],sar[1]) && !strcmp(sar[2],sar[1])) {
                  lbls = SUMA_append_replace_string(lbls, "(I,T,B)", "",1);
                  lbls = SUMA_append_replace_string(lbls, 
                           SUMA_DsetColLabel(Sover->dset_link, 
                                             Sover->OptScl->find),
                                                    "",1);
                  lbls = SUMA_append_replace_string(lbls, sar[0], "=",1);
               } else if (!strcmp(sar[0],sar[1])) {
                  lbls = SUMA_append_replace_string(lbls, "(I,T)", "",1);
                  lbls = SUMA_append_replace_string(lbls, 
                           SUMA_DsetColLabel(Sover->dset_link, 
                                             Sover->OptScl->find),
                                                    "",1);
                  lbls = SUMA_append_replace_string(lbls, sar[0], "=",1);

                  lbls = SUMA_append_replace_string(lbls, " (B)", "",1);
                  lbls = SUMA_append_replace_string(lbls, 
                           SUMA_DsetColLabel(Sover->dset_link, 
                                             Sover->OptScl->bind),
                                                    "",1);                  
                  lbls = SUMA_append_replace_string(lbls, sar[2], "=", 1);
               } else if (!strcmp(sar[1],sar[2])) {
                  lbls = SUMA_append_replace_string(lbls, "(I)", "",1);
                  lbls = SUMA_append_replace_string(lbls, 
                           SUMA_DsetColLabel(Sover->dset_link, 
                                             Sover->OptScl->find),
                                                    "",1);
                  lbls = SUMA_append_replace_string(lbls, sar[0], "=",1);

                  lbls = SUMA_append_replace_string(lbls, " (B,T)", "",1);
                  lbls = SUMA_append_replace_string(lbls, 
                           SUMA_DsetColLabel(Sover->dset_link, 
                                             Sover->OptScl->bind),
                                                    "",1);                  
                  lbls = SUMA_append_replace_string(lbls, sar[2], "=", 1);
               } else if (!strcmp(sar[0],sar[2])) {
                  lbls = SUMA_append_replace_string(lbls, "(I,B)", "",1);
                  lbls = SUMA_append_replace_string(lbls, 
                           SUMA_DsetColLabel(Sover->dset_link, 
                                             Sover->OptScl->find),
                                                    "",1);
                  lbls = SUMA_append_replace_string(lbls, sar[0], "=",1);

                  lbls = SUMA_append_replace_string(lbls, " (T)", "",1);
                  lbls = SUMA_append_replace_string(lbls, 
                           SUMA_DsetColLabel(Sover->dset_link, 
                                             Sover->OptScl->tind),
                                                    "",1);                  
                  lbls = SUMA_append_replace_string(lbls, sar[1], "=", 1);
               } else {
                  for (sp=0,i=0;i<3;++i) {
                     if (sar[i]) {
                        if (sp) 
                           lbls = SUMA_append_replace_string(lbls," ","", 1);
                        lbls = SUMA_append_replace_string(lbls, 
                           SUMA_DsetColLabel(Sover->dset_link, 
                                                i==0 ? Sover->OptScl->find : 
                                                ( i == 1 ? Sover->OptScl->tind:
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
                        if (sp) 
                           lbls = SUMA_append_replace_string(lbls," ","", 1);
                        lbls = SUMA_append_replace_string(lbls, 
                           SUMA_DsetColLabel(Sover->dset_link, 
                                                i==0 ? Sover->OptScl->find : 
                                                ( i == 1 ? Sover->OptScl->tind:
                                                           Sover->OptScl->bind)),
                                                    "",1); 
                        lbls = SUMA_append_replace_string(lbls,sar[i],"=", 1);
                        sp=1;
                        SUMA_free(sar[i]); sar[i]=NULL;
                     }
                  }

            }
            SUMA_free(sar); sar=NULL;
            if (ado->do_type == SO_type) {
               if ((sp=SUMA_NodeClustNumber(Sover, node, 
                                           (SUMA_SurfaceObject *)ado, NULL))) {
                  sprintf(stmp,"\nIn cluster %d", sp);
                  if (lbls) lbls = SUMA_append_replace_string(lbls,stmp,"",1);
               }
            }
        } else { /* old approach to labeling, I,T, business only */
            SUMA_LH("Old");
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
      }/* Have labels */
      
      /* Some coord info for select few ? */

      if (ado->do_type == GRAPH_LINK_type) {
         SUMA_DSET *gset=NULL;
         char *stmp=NULL, *pref=NULL;
         gset =  SUMA_find_GLDO_Dset((SUMA_GraphLinkDO*)ado);
         if (node >= 0) { /* an edge */
            pref = "\nGpair "; /* I was using "Cell" and "Edge"
                                 depending on the variant, but
                                 the label is shared across viewers
                                 so that does not work well */
            if ((stmp=SUMA_GDSET_Edge_Label(gset, node, pref, NULL))) {
               SUMA_LHv("Adding edge label %s\n", stmp);
               lbls = SUMA_append_replace_string(lbls,stmp,"", 1);
               SUMA_ifree(stmp);
            }
         } else {
            if (sec >= 0) { /* all from some node */
               if ((stmp=SUMA_GDSET_Node_Label(gset, sec))) {
                  SUMA_LHv("Got graph node label %s\n", stmp);
                  stmp = SUMA_append_replace_string("\nGnode ", stmp, "", 0);
                  lbls = SUMA_append_replace_string(lbls,stmp,"", 1);
                  SUMA_ifree(stmp);
               }
            }
         }
      }

      /* Bundles, tracts? */
      if (ado->do_type == TRACT_type) {
         SUMA_TractDO *tdo=(SUMA_TractDO *)ado;
         SUMA_TRACT_SAUX *TSaux = SUMA_ADO_TSaux(ado);
         TAYLOR_BUNDLE *tb=NULL;
         char stmp[256], *be=NULL;
         if (node >= 0) { /* spell out where you are */
            if (TSaux->PR->iAltSel[SUMA_NET_BUN] >= 0 &&
                TSaux->PR->iAltSel[SUMA_BUN_TRC] >= 0 &&
                TSaux->PR->iAltSel[SUMA_TRC_PNT] >= 0 ) {
                if ( (tb = TDO_BUNDLE(tdo, TSaux->PR->iAltSel[SUMA_NET_BUN])) &&
                     tb->bundle_ends ) {
                   be = tb->bundle_ends; 
                }
                snprintf(stmp, 256,
                     "%s%s%cPnt %ld, trct %ld, bnd %ld",
                       lbls?"\n":"", be ? be:"", be ? '\n':'\0',
                       TSaux->PR->iAltSel[SUMA_TRC_PNT], 
                       TSaux->PR->iAltSel[SUMA_BUN_TRC], 
                       TSaux->PR->iAltSel[SUMA_NET_BUN]);
               lbls = SUMA_append_replace_string(lbls,stmp,"", 0);
            }
         }
      }

      /* Volume object */
      if (ado->do_type == VO_type) {
         SUMA_VolumeObject *vo=(SUMA_VolumeObject *)ado;
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         char stmp[256];
         SUMA_LH("Vol consideration with %s and node %d",
                  lbls, node);
         if (node >= 0) { /* spell out where you are */
            if (VSaux->PR->iAltSel[SUMA_VOL_I] >= 0 &&
                VSaux->PR->iAltSel[SUMA_VOL_J] >= 0 &&
                VSaux->PR->iAltSel[SUMA_VOL_K] >= 0 ) {
                snprintf(stmp, 256,
                  "%sVoxel [%ld,%ld,%ld] at [%.2f,%.2f,%.2f]",
                       lbls?"\n":"",
                       VSaux->PR->iAltSel[SUMA_VOL_I], 
                       VSaux->PR->iAltSel[SUMA_VOL_J], 
                       VSaux->PR->iAltSel[SUMA_VOL_K],
                       VSaux->PR->PickXYZ[0], 
                       VSaux->PR->PickXYZ[1],
                       VSaux->PR->PickXYZ[2]);
               lbls = SUMA_append_replace_string(lbls,stmp,"", 0);
               if (LocalHead) { /* Just for fun */
                  char variant[8];
                  SUMA_dset_gui_slice_from_tex_slice_d(vo->VE, 0, 
                                 VSaux->PR->dAltSel+SUMA_VOL_SLC_EQ0, 
                                                      0, variant,NULL);
                  SUMA_LH("%s, slice %.2f,%.2f,%.2f, %.2f: %s\n", stmp, 
                     VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ0], 
                     VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ1], 
                     VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ2], 
                     VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ3],
                     variant);
               }
            }
         } 
      }

      /* Mask object */
      if (ado->do_type == MASK_type) {
         char stmp[256];
         snprintf(stmp, 256,
                  "%sMask selected",
                  lbls?"\n":"");
         lbls = SUMA_append_replace_string(lbls,stmp,"", 0);
      }
   }
      
   /* Do we need to tell Santa? */
   if (ado->do_type == SO_type &&
      lbls && SUMAg_CF->X->Whereami_TextShell) {
      if (!list) list = SUMA_CreateList();
      ED = SUMA_InitializeEngineListData (SE_Whereami);
      if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                    SEF_vp, (void *)ado,
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
   }
   SUMA_LH("Returning %s", lbls);
   SUMA_RETURN(lbls);
}

/* transform string to a TextNIDO,
   If sv is not NULL, 
      then NIDO is added to SUMAg_DOv and registered with sv
      Do not free what it returned because it is added to SUMA's DOv
   else
      the returned nido is all yours to manage
   
*/
SUMA_NIDO *SUMA_NodeLabelToTextNIDO (char *lbls, SUMA_ALL_DO *ado, 
                                     SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_NodeLabelToTextNIDO"};
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
                         SUMA_ADO_LDP(ado), NULL, NULL);
      nini = NI_new_data_element("T", 0);
   
      v = SUMA_ADO_DatumXYZ(ado, SUMA_ADO_SelectedDatum(ado, NULL, NULL), NULL);
      coord_type = SUMA_WORLD;
   } else {/* fixed on screen */
      nido = SUMA_BlankNIDO(NULL, "AHorseWithNoName",
                         NULL, "fixed", NULL);
      nini = NI_new_data_element("T", 0);
      coord_type = SUMA_SCREEN;
      NI_set_attribute(nini,"v_align", "top");
      NI_set_attribute(nini,"h_align", "center");
      NI_set_attribute(nini,"shadow","yes"); /* Not ready for prime time */
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

SUMA_Boolean SUMA_UpdateNodeLblField(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_UpdateNodeLblField"};
   
   if (!ado) return(NOPE);
   switch(ado->do_type) {
      case SO_type: {
         return(SUMA_UpdateNodeLblField_ADO(ado));
         break; }
      case GDSET_type: {
         SUMA_S_Warn("Should I be updating this guy and not it GLDO?");
         return(YUP);
         break; }
      case CDOM_type:
      case VO_type:
      case TRACT_type:
      case MASK_type:
      case GRAPH_LINK_type: {
         return(SUMA_UpdateNodeLblField_ADO(ado));
         break; }
      default:
         SUMA_S_Errv("Bad type %s for this function\n",
                  SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         return(NOPE);
   }
   return(NOPE);
}

/*! 
   Updates places where a node's value is shown 
*/
SUMA_Boolean SUMA_UpdateNodeLblField_ADO(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_UpdateNodeLblField_ADO"};
   int Found = -1;
   char *lbls=NULL, *ltmp=NULL;
   char str_col[101];
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
      
   if (!ado || (!(SurfCont=SUMA_ADO_Cont(ado)) 
                  && !SUMAg_CF->X->Whereami_TextShell)) {
      /* absolutely nothing to do */
      SUMA_RETURN(NOPE);
   } 
   
   switch(ado->do_type) {
      case CDOM_type:
      case VO_type:
      case GRAPH_LINK_type:
      case MASK_type:
      case TRACT_type:
      case SO_type: 
         /* get labels from Label Datasets, and update whereami 
            window if needed*/
         lbls = SUMA_GetLabelsAtSelection_ADO(ado,
                                       SUMA_ADO_SelectedDatum(ado, NULL, NULL),
                                              SUMA_ADO_SelectedSecondary(ado));
         SUMA_LHv("Label Dsets: %s\n", lbls);
         break;
      case GDSET_type:
         SUMA_S_Warn("Should be using GRAPH_LINK_type instead");
         break;
      default:
         break;
   }
         
   
   if (SurfCont && SurfCont->LabelTable && SurfCont->LabelTable->cells) {
      Sover = SUMA_ADO_CurColPlane(ado);
      if (!Sover) {
         SUMA_RETURN(NOPE);
      }

      if (!Sover->ShowMode > 0) {
         SUMA_LH("Col plane hidden");
         sprintf(str_col,"hidden color overlay");
         if (lbls) lbls = SUMA_append_replace_string(lbls, str_col, "; col=", 1);
         else lbls = SUMA_copy_string(str_col);
         SUMA_INSERT_CELL_STRING(SurfCont->LabelTable, 0, 1, lbls);
         if (lbls) SUMA_free(lbls); lbls = NULL;
         SUMA_RETURN(YUP);
      }

      Found = SUMA_GetNodeOverInd(Sover, 
                                  SUMA_ADO_ColPlane_SelectedDatum(ado, Sover));

      if (Found < 0) {
         SUMA_Boolean Reasoned = NOPE;
         SUMA_LH("Node not found.\nLikely masked by threshold");
         sprintf(str_col,"masked");
         /* try to find out why it is masked */
         if (!Reasoned && SurfCont->DataTable) {
            SUMA_LH("Checking if there is no data for this node");
            {
               void *n=NULL;
               XtVaGetValues(
               SurfCont->DataTable->cells[1*SurfCont->DataTable->Ni+1], 
                  XmNvalue, &n, NULL);
               if (strcmp((char *)n, "NoData") == 0) {
                  /* no data at all */
                  sprintf(str_col,"no data for this node");
                  Reasoned = YUP;
               }
            }
         }
         if (!Reasoned && SurfCont->DataTable) { /* is the value 0 ? */
            SUMA_LH("Checking if node value is zero & zero is not being shown");
            if (Sover->OptScl->MaskZero && 
                  SurfCont->DataTable->num_value[1*SurfCont->DataTable->Ni+1]) {
               sprintf(str_col,"masked by zero value");  
            }   
         }
      } else {
         {
            /* Now we know what the index of this node 
               is in the overlay plane (and the data) */
            sprintf(str_col,"%s",              
                            MV_format_fval2(Sover->ColVec[3*Found],5));
            SUMA_strncat( str_col,", ", 100);
            SUMA_strncat( str_col, 
                           MV_format_fval2(Sover->ColVec[3*Found+1],5), 100); 
            SUMA_strncat( str_col,", ", 100);
            SUMA_strncat( str_col, 
                           MV_format_fval2(Sover->ColVec[3*Found+2],5), 100);
            SUMA_LH("%s", str_col);
         }
      }
      if (lbls) ltmp = SUMA_append_replace_string(lbls, str_col, "; col=", 0);
      else ltmp = SUMA_copy_string(str_col);
      SUMA_INSERT_CELL_STRING(SurfCont->LabelTable, 0, 1, ltmp);
      SUMA_free(ltmp); ltmp=NULL;
   } 
   
   if (lbls) SUMA_free(lbls); lbls = NULL;
   
   SUMA_RETURN(YUP);
}

 
SUMA_Boolean SUMA_UpdateCrossHairNodeLabelField(SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_UpdateCrossHairNodeLabelField"};
   char *lbls=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
      
   if (!sv || !sv->Ch || sv->Ch->adoID < 0) {
      /* nothing to do */
      SUMA_RETURN(NOPE);
   } 
   
   if (!(ado = (SUMA_ALL_DO *)(SUMAg_DOv[sv->Ch->adoID].OP))) {
      SUMA_RETURN(NOPE);
   }
   if ( sv->ShowLabelAtXhair &&
       (lbls = SUMA_GetLabelsAtSelection(ado, sv->Ch->datumID, sv->Ch->secID))) {
      SUMA_LH("Got %s (%d)",lbls, sv->ShowLabelAtXhair);
      SUMA_NodeLabelToTextNIDO (lbls, ado, sv);
      SUMA_free(lbls); lbls = NULL;
   } else {
      SUMA_LH("Got NOTHING or %d", sv->ShowLabelAtXhair);
      SUMA_NodeLabelToTextNIDO ("", ado, sv);
   }
   
   SUMA_RETURN(YUP);
}

int SUMA_UpdateCrossHairNodeLabelFieldForDO(SUMA_ALL_DO *curDO) 
{
   static char FuncName[]={"SUMA_UpdateCrossHairNodeLabelFieldForDO"};
   int i=0, iup=0;
   SUMA_SurfaceViewer *sv=NULL;
   
   SUMA_ENTRY;
   
   if (!curDO) SUMA_RETURN(0);
   
   /* update any viewer that is showing this 
      surface */
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
         /* is this viewer showing curDO ? */
         if (SUMA_isRegisteredDO(&(SUMAg_SVv[i]), SUMAg_DOv, curDO)) {
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
      if (!SO->SurfCont->FaceTable||
          !SO->SurfCont->FaceTable->num_value) { /* table widgets not set yet ?*/
         SUMA_RETURN(NOPE);
      }

      if (!(curSO=(SUMA_SurfaceObject *)SUMA_SurfCont_GetcurDOp(SO->SurfCont))) {
         SUMA_S_Err("Failed to get curDOp");
         SUMA_RETURN(NOPE);
      }
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
SUMA_Boolean SUMA_Init_SurfCont_CrossHair(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_Init_SurfCont_CrossHair"};
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!ado) SUMA_RETURN(YUP);

   /* set the cross hair and related fields */
   switch (ado->do_type) {
      case SO_type:
         SUMA_UpdateTriField((SUMA_SurfaceObject *)ado);
         SUMA_UpdateNodeField(ado);
         break;
      case GDSET_type:
         SUMA_S_Err("No init for a DO that cannot be dispalyed\n"
                    "without variant");
         SUMA_RETURN(NOPE);
         break;
      case CDOM_type:
         SUMA_S_Err("So much to do, so little time"
                    "Probably something like SUMA_UpdateNodeField");
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
         SUMA_UpdatePointField(ado);
         SUMA_UpdateNodeField(ado);
         break; }
      case TRACT_type: {
         SUMA_UpdateNodeField(ado);
         break; }
      case MASK_type: {
         static int ncnt;
         if (!ncnt) {
            ++ncnt;
            SUMA_LH("Nothing to be done for masks here");
         }
         break; }
      case VO_type: {
         SUMA_UpdateNodeField(ado);
         break; }
      default:
         SUMA_S_Errv("Nothing to do with %s\n",
               SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         SUMA_RETURN(NOPE);
   }
   
   /* look for a viewer that is showing this surface and has 
      this surface in focus*/
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if (LocalHead) fprintf (SUMA_STDERR,
                              "%s: Checking viewer %d.\n", FuncName, i);
      if (!SUMAg_SVv[i].isShaded && SUMAg_SVv[i].X->TOPLEVEL) {
         /* is this viewer showing DO ? */
         if (SUMA_isVisibleDO(&(SUMAg_SVv[i]), SUMAg_DOv, ado)) {
            if (SUMA_SV_Focus_ADO(&(SUMAg_SVv[i])) == ado) {
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
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!(ado = (SUMA_ALL_DO *)data) || !(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Err("NULL input");
      SUMA_RETURNe;
   }
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_OpenCmapFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, (void *)ado,
                                          SES_Suma, NULL, NOPE,
                                          SEI_Head, NULL))) {
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
   }
   SUMA_RegisterEngineListCommand (  list, ED,
                                     SEF_ip, (void *)SurfCont->TLS,
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
   SUMA_ALL_DO *ado = NULL;
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
      ado = (SUMA_ALL_DO *)data;

      if (LocalHead) {
         fprintf (SUMA_STDERR,
                  "%s: bonding colormap %s to surface %s.\n", 
                  FuncName, filename, SUMA_ADO_Label(ado));
      }

      /* refresh the list */
      SUMA_CmapSelectList(ado, 1, bringup);

      /* update the menu buttons */
      SUMA_CreateUpdatableCmapMenu(ado);

      /* Set the menu button to the current choice */
      if (!SUMA_SetCmapMenuChoice (ado, Cmap->Name)) {
         SUMA_SL_Err("Failed in SUMA_SetCmapMenuChoice");
      }

      /* switch to the recently loaded  cmap */
      if (!SUMA_SwitchColPlaneCmap(ado, Cmap)) {
         SUMA_SL_Err("Failed in SUMA_SwitchColPlaneCmap");
      }

      /* update Lbl fields */
      SUMA_UpdateNodeLblField(ado);
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
     
     
/*! 
   SUMA_ADO_CurColPlane: Return the current col plane for any DO
   This is meant to replace all of the old SO->SurfCont->curColPlane
   
   For some DO with a DOCont, such as DSET_type, the answer 
   will be GSaux->Overlay 
   For the rest the answer is NULL
*/
SUMA_OVERLAYS * SUMA_ADO_CurColPlane(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_ADO_CurColPlane"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (!ado) return(NULL);
   SUMA_LHv("Have %d\n", ado->do_type);
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         if (!SO->SurfCont) return(NULL);
         return(SO->SurfCont->curColPlane);
         break; }
      case CDOM_type: {
         SUMA_CIFTI_SAUX *CSaux = SUMA_ADO_CSaux(ado);
         if (!CSaux || !CSaux->DOCont) return(NULL);
         return(CSaux->DOCont->curColPlane);
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *GSaux = SDSET_GSAUX(dset);
         if (!GSaux) return(NULL);
         return(GSaux->Overlay);
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            return(NULL);
         }
         return(SUMA_ADO_CurColPlane((SUMA_ALL_DO *)dset));
         break; }
      case MASK_type: {
         SUMA_MASK_SAUX *MSaux = SUMA_ADO_MSaux(ado);
         if (!MSaux || !MSaux->DOCont) return(NULL);
         return(MSaux->DOCont->curColPlane);
         break; }
      case TRACT_type: {
         SUMA_TRACT_SAUX *TSaux = SUMA_ADO_TSaux(ado);
         if (!TSaux || !TSaux->DOCont) return(NULL);
         return(TSaux->DOCont->curColPlane);
         break; }
      case VO_type: {
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         if (!VSaux || !VSaux->DOCont) return(NULL);
         return(VSaux->DOCont->curColPlane);
         break; }
      default:
         return(NULL);
   }
   return(NULL);
}

SUMA_OVERLAYS * SUMA_ADO_Overlay0(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_ADO_Overlay0"};
   return(SUMA_ADO_Overlay(ado, 0));
}

SUMA_OVERLAYS * SUMA_ADO_Overlay(SUMA_ALL_DO *ado, int i)
{
   static char FuncName[]={"SUMA_ADO_Overlay"};
   SUMA_OVERLAYS **overlays=NULL;
   int N_over=0;
   
   if (!ado || i<0) return(NULL);
   if ((overlays = SUMA_ADO_Overlays(ado, &N_over))) { 
      if (i < N_over) return(overlays[i]);
   }
   return(NULL);
}

SUMA_Boolean SUMA_ADO_Append_Overlay(SUMA_ALL_DO *ado, SUMA_OVERLAYS **over)
{
   static char FuncName[]={"SUMA_ADO_Append_Overlay"};
   SUMA_OVERLAYS ** overlays = NULL;
   int N_over = -1;
   
   SUMA_ENTRY;
   
   if (!over || !*over) { 
      SUMA_S_Err("No overlay to add !");
      SUMA_RETURN (NOPE);
   }
   overlays = SUMA_ADO_Overlays(ado, &N_over);
   if (!overlays || N_over < 0) {
      SUMA_S_Err("No overlays (%p) or N_over (%d)", overlays, N_over);
      SUMA_RETURN (NOPE);
   }  
   
   if (N_over+1 >= SUMA_MAX_OVERLAYS) {
      SUMA_SL_Crit("Too many color overlays.");
      SUMA_RETURN (NOPE);
   }
   
   switch (ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)ado;
         SUMA_S_Warn("Case not tested yet");
         overlays[N_over] = *over;
         *over = NULL;
         ++SO->N_Overlays;
         break; }
      case TRACT_type: {
         SUMA_TractDO *tdo = (SUMA_TractDO *)ado;
         SUMA_TRACT_SAUX *TSaux = SUMA_ADO_TSaux(ado);
         overlays[N_over] = *over;
         *over = NULL;
         ++TSaux->N_Overlays;
         break; }
      case MASK_type: {
         SUMA_MaskDO *mdo = (SUMA_MaskDO *)ado;
         SUMA_MASK_SAUX *MSaux = SUMA_ADO_MSaux(ado);
         overlays[N_over] = *over;
         *over = NULL;
         ++MSaux->N_Overlays;
         break; }
      case VO_type: {
         SUMA_VolumeObject *vo = (SUMA_VolumeObject *)ado;
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         overlays[N_over] = *over;
         *over = NULL;
         ++VSaux->N_Overlays;
         break; }
      case CDOM_type: {
         SUMA_CIFTI_SAUX *CSaux = SUMA_ADO_CSaux(ado);
         overlays[N_over] = *over;
         *over = NULL;
         ++CSaux->N_Overlays;
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *GSaux = SDSET_GSAUX(dset);
         SUMA_S_Warn("Case not tested yet");
         if (!GSaux) {
            SUMA_S_Err("NULL GSaux");
            SUMA_RETURN(NOPE);
         }
         if (N_over > 0) {
            SUMA_S_Err("Not ready or shouldn't add more than one overlay \n"
                       "for dsets");
            SUMA_RETURN(NOPE);
         }
         GSaux->Overlay = *over;
         *over = NULL;
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         SUMA_Boolean ans = NOPE;
         SUMA_S_Warn("Case not tested yet");
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            SUMA_RETURN(NOPE);
         }
         ans = SUMA_ADO_Append_Overlay((SUMA_ALL_DO *)dset, over);
         if (!ans) SUMA_RETURN(NOPE);
         *over = NULL;
         break; }
      default:
         SUMA_S_Err("Not ready for %d\n", ado->do_type);
         SUMA_RETURN(NOPE);
         break;
   }
   
   SUMA_RETURN(YUP);
}

SUMA_OVERLAYS **  SUMA_ADO_Overlays(SUMA_ALL_DO *ado, int *N_over)
{
   static char FuncName[]={"SUMA_ADO_Overlays"};
   
   if (!ado) return(NULL);
   
   if (N_over) *N_over = -1;
   
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         if (N_over) *N_over = SO->N_Overlays;
         return(SO->Overlays);
         break; }
      case CDOM_type: {
         SUMA_CIFTI_SAUX *CSaux = SUMA_ADO_CSaux(ado);
         if (!CSaux) return(NULL);
         if (N_over) *N_over = CSaux->N_Overlays;
         return(CSaux->Overlays);
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *GSaux = SDSET_GSAUX(dset);
         if (!GSaux || !GSaux->Overlay) return(NULL);
         if (N_over) *N_over = 1;
         return(&(GSaux->Overlay));
         break; }
      case GRAPH_LINK_type: {
         int N_n;
         SUMA_OVERLAYS **oo=NULL;
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            return(NULL);
         }
         if ((oo = SUMA_ADO_Overlays((SUMA_ALL_DO *)dset, &N_n))){
            if (N_over) *N_over = N_n;
            return(oo);
         }
         break; }
      case TRACT_type: {
         SUMA_TRACT_SAUX *TSaux = SUMA_ADO_TSaux(ado);
         if (!TSaux) return(NULL);
         if (N_over) *N_over = TSaux->N_Overlays;
         return(TSaux->Overlays);
         break; }
      case MASK_type: {
         SUMA_MASK_SAUX *MSaux = SUMA_ADO_MSaux(ado);
         if (!MSaux) return(NULL);
         if (N_over) *N_over = MSaux->N_Overlays;
         return(MSaux->Overlays);
         break; }
      case VO_type: {
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         if (!VSaux) return(NULL);
         if (N_over) *N_over = VSaux->N_Overlays;
         return(VSaux->Overlays);
         break; }
      default:
         return(NULL);
   }
   return(NULL);
}


int SUMA_ADO_N_Overlays(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_ADO_N_Overlays"};
   if (!ado) return(-1);
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         return(SO->N_Overlays);
         break; }
      case CDOM_type: {
         SUMA_CIFTI_SAUX *CSaux = SUMA_ADO_CSaux(ado);
         if (!CSaux) return(-1);
         return(CSaux->N_Overlays);
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *GSaux = SDSET_GSAUX(dset);
         if (!GSaux) return(-1);
         return(1);
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            return(-1);
         }
         return(SUMA_ADO_N_Overlays((SUMA_ALL_DO *)dset));
         break; }
      case TRACT_type: {
         SUMA_TRACT_SAUX *TSaux = SUMA_ADO_TSaux(ado);
         if (!TSaux) return(-1);
         return(TSaux->N_Overlays);
         break; }
      case MASK_type: {
         SUMA_MASK_SAUX *MSaux = SUMA_ADO_MSaux(ado);
         if (!MSaux) return(-1);
         return(MSaux->N_Overlays);
         break; }
      case VO_type: {
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         if (!VSaux) return(-1);
         return(VSaux->N_Overlays);
         break; }

      default:
         return(-1);
   }
   return(-1);
}

/*!
   This function returns a selected datum for a DO
   For surfaces, this is always SurfCont->SelectedNode
   For SDSETs, it is GSaux->PR->datum_index
*/
int SUMA_ADO_SelectedDatum(SUMA_ALL_DO *ado, void *extra, void *extra2)
{
   static char FuncName[]={"SUMA_ADO_SelectedDatum"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (!ado) return(-1);
   SUMA_LHv("Here with %d (%s), %s\n", 
             ado->do_type, ADO_TNAME(ado), SUMA_ADO_Label(ado));
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         return(SO->SelectedNode);
         break; }
      case CDOM_type: {
         SUMA_CIFTI_SAUX *CSaux = SUMA_ADO_CSaux(ado);
         if (!CSaux) return(-1);
         /* Plenty of room to return extras, once we figure those out.
            Selections could be on surfaces or volumes so what needs 
            to be filled out in the extras will depend on the type of
            domain over which the selection was made. */
         return(CSaux->PR->datum_index);
         break; }   
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *GSaux = SDSET_GSAUX(dset);
         if (!GSaux) return(-1);
         return(GSaux->PR->datum_index);
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            return(-1);
         }
         return(SUMA_ADO_SelectedDatum((SUMA_ALL_DO *)dset, extra, extra2));
         break; }
      case TRACT_type: {
         SUMA_TRACT_SAUX *TSaux = SUMA_ADO_TSaux(ado);
         if (!TSaux) return(-1);
         if (extra) {
            int *ivsel = (int *)extra;
            ivsel[SUMA_NET_BUN] = TSaux->PR->iAltSel[SUMA_NET_BUN];
            ivsel[SUMA_BUN_TRC] = TSaux->PR->iAltSel[SUMA_BUN_TRC];
            ivsel[SUMA_TRC_PNT] = TSaux->PR->iAltSel[SUMA_TRC_PNT];
            ivsel[SUMA_NET_TRC] = TSaux->PR->iAltSel[SUMA_NET_TRC];
         }
         return(TSaux->PR->datum_index);
         break; }
      case VO_type: {
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         if (!VSaux) return(-1);
         if (extra) {
            int *ivsel = (int *)extra;
            ivsel[SUMA_VOL_I] = VSaux->PR->iAltSel[SUMA_VOL_I];
            ivsel[SUMA_VOL_J] = VSaux->PR->iAltSel[SUMA_VOL_J];
            ivsel[SUMA_VOL_K] = VSaux->PR->iAltSel[SUMA_VOL_K];
            ivsel[SUMA_VOL_IJK] = VSaux->PR->iAltSel[SUMA_VOL_IJK];
         }
         if (extra2) {
            float *fvsel = (float *)extra2;
            fvsel[SUMA_VOL_SLC_EQ0] = VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ0];
            fvsel[SUMA_VOL_SLC_EQ1] = VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ1];
            fvsel[SUMA_VOL_SLC_EQ2] = VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ2];
            fvsel[SUMA_VOL_SLC_EQ3] = VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ3];
         }
            
         return(VSaux->PR->datum_index);
         break; }
      case MASK_type: {
         static int ncnt=0;
         if (!ncnt) {
            SUMA_S_Warn("Not ready for mask objects, returning 0"); ++ncnt;
         }
         return(0);
         break; }
      default:
         return(-1);
   }
   return(-1);
}

/*!
   This function returns secondary selections a DO
   For surfaces, this is always SurfCont->SelectedFaceSet
   For SDSETs, it is GSaux->PR->iAltSel[SUMA_ENODE_0]
*/
int SUMA_ADO_SelectedSecondary(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_ADO_SelectedSecondary"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (!ado) return(-1);
   SUMA_LHv("Here with %d (%s), %s\n", 
             ado->do_type, ADO_TNAME(ado), SUMA_ADO_Label(ado));
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         return(SO->SelectedFaceSet);
         break; }
      case CDOM_type: {
         SUMA_S_Err("What gets set will depend on  PR->primitive. \n"
                    "Consider SUMA_ADO_SelectedSecondary() and ponder away.");
         return(-1);
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *GSaux = SDSET_GSAUX(dset);
         if (!GSaux) return(-1);
         return(GSaux->PR->iAltSel[SUMA_ENODE_0]);
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            return(-1);
         }
         return(SUMA_ADO_SelectedSecondary((SUMA_ALL_DO *)dset));
         break; }
      case TRACT_type: {
         SUMA_LH("No secondary selections on tracts.");
         return(-1);
         break; }
      case MASK_type: {
         SUMA_LH("No secondary selections on masks.");
         return(-1);
         break; }
      case VO_type: {
         SUMA_LH("No secondary selections on volumes.");
         return(-1);
         break; }
      default:
         return(-1);
   }
   return(-1);
}

/* Was what was selected a primitive of the DO that carries
   data ? 
   A selected edge on a graph would return YES
   A selected node on a graph would return NOPE
*/
SUMA_Boolean SUMA_is_ADO_Datum_Primitive(SUMA_ALL_DO *ado,
                                          SUMA_COLID_OFFSET_DATUM *codf) 
{
   static char FuncName[]={"SUMA_is_ADO_Datum_Primitive"};
      
   if (!ado || !codf) return(NOPE);
   
   switch (ado->do_type) {
      case VO_type:
      case CDOM_type:
      case SO_type:
         SUMA_S_Err("Function not ready to handle colid selection modes"
                    "on surfaces or volumes");
         break;
      case GDSET_type: 
      case GRAPH_LINK_type:
         if (codf->primitive && !strcmp(codf->primitive,"segments"))
            return(YUP);
         break;
      default:
         SUMA_S_Errv("Not ready to deal with type %s\n",
              SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         break;
   }
   
   return(NOPE);
}

SUMA_Boolean SUMA_ADO_Set_SelectedDatum(SUMA_ALL_DO *ado, int sel, 
                                        void *extra, void *extra2)
{
   static char FuncName[]={"SUMA_ADO_Set_SelectedDatum"};
   if (!ado) return(0);
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         if (!SO->SurfCont) return(0);
         SO->SelectedNode = sel;
         return(1);
         break; }
      case CDOM_type: {
         SUMA_CIFTI_DO *co = (SUMA_CIFTI_DO *)ado;
         SUMA_CIFTI_SAUX *CSaux = SUMA_ADO_CSaux(ado);
         int it, ip, ib, l1, *iv = (int *)extra;
         CSaux->PR->datum_index = sel;
         if (extra) {
            SUMA_S_Err("Not ready for extra");
         } else {
            /* Will need to setup other PR fields once we decide on extras */
         }   
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *GSaux = SDSET_GSAUX(dset);
         if (!GSaux) return(0);
         GSaux->PR->datum_index = sel;
         return(1);
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            return(0);
         }
         return(SUMA_ADO_Set_SelectedDatum((SUMA_ALL_DO *)dset, sel, 
                                           extra, extra2));
         break; }
      case TRACT_type: {
         SUMA_TractDO *tdo = (SUMA_TractDO *)ado;
         SUMA_TRACT_SAUX *TSaux = SUMA_ADO_TSaux(ado);
         int it, ip, ib, l1, *iv = (int *)extra;
         TSaux->PR->datum_index = sel;
         if (extra) {
            TSaux->PR->iAltSel[SUMA_NET_BUN] = iv[SUMA_NET_BUN];
            TSaux->PR->iAltSel[SUMA_BUN_TRC] = iv[SUMA_BUN_TRC];
            TSaux->PR->iAltSel[SUMA_TRC_PNT] = iv[SUMA_TRC_PNT];
            TSaux->PR->iAltSel[SUMA_NET_TRC] = iv[SUMA_NET_TRC];
         } else {
            TSaux->PR->iAltSel[SUMA_NET_BUN] = TSaux->PR->iAltSel[SUMA_BUN_TRC] =
            TSaux->PR->iAltSel[SUMA_TRC_PNT] = 
            TSaux->PR->iAltSel[SUMA_NET_TRC] = -1;
            if (tdo && tdo->net) {
               if (Network_1P_to_PTB(tdo->net, sel, &ip, &it, &ib, &l1)) {
                  TSaux->PR->iAltSel[SUMA_NET_BUN] = ib;
                  TSaux->PR->iAltSel[SUMA_BUN_TRC] = it;
                  TSaux->PR->iAltSel[SUMA_TRC_PNT] = ip;
                  TSaux->PR->iAltSel[SUMA_NET_TRC] = l1;
               }
            }
         }   
         break; }
      case VO_type: {
         SUMA_VolumeObject *vo = (SUMA_VolumeObject *)ado;
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         SUMA_DSET *dset=NULL;
         int *iv = (int *)extra, *dims=NULL, iv4[4];
         int iv3[3];
         VSaux->PR->datum_index = sel;
         if (extra) {
            VSaux->PR->iAltSel[SUMA_VOL_I] = iv[SUMA_VOL_I];
            VSaux->PR->iAltSel[SUMA_VOL_J] = iv[SUMA_VOL_J];
            VSaux->PR->iAltSel[SUMA_VOL_K] = iv[SUMA_VOL_K];
            VSaux->PR->iAltSel[SUMA_VOL_IJK] = iv[SUMA_VOL_IJK];
            if (extra2) {
               float *fv = (float *)extra2;
               if (SUMA_PLANE_NOT_SET(fv+SUMA_VOL_SLC_EQ0)) {/* a sign that there
                                               is no good plane in input
                                              Try updating current plane */
                  if (vo && 
                      SUMA_PLANE_IS_SET(VSaux->PR->dAltSel+SUMA_VOL_SLC_EQ0)) {
                      iv3[0] = (int)VSaux->PR->iAltSel[SUMA_VOL_I];
                      iv3[1] = (int)VSaux->PR->iAltSel[SUMA_VOL_J];
                      iv3[2] = (int)VSaux->PR->iAltSel[SUMA_VOL_K];
                      SUMA_SHIFT_PLANE_TO_P(
                                 (VSaux->PR->dAltSel+SUMA_VOL_SLC_EQ0),
                                 SUMA_VO_PointXYZ(vo, -1, iv3, NULL));
                  } else { 
                     /* Do nothing: Nothing set in the first place */
                  }
               } else {
                  VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ0] = fv[SUMA_VOL_SLC_EQ0];
                  VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ1] = fv[SUMA_VOL_SLC_EQ1];
                  VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ2] = fv[SUMA_VOL_SLC_EQ2];
                  VSaux->PR->dAltSel[SUMA_VOL_SLC_EQ3] = fv[SUMA_VOL_SLC_EQ3];
               }
            }
         } else {
            VSaux->PR->iAltSel[SUMA_VOL_I] = VSaux->PR->iAltSel[SUMA_VOL_J] =
            VSaux->PR->iAltSel[SUMA_VOL_K] = 
            VSaux->PR->iAltSel[SUMA_VOL_IJK] = -1;
            SUMA_MARK_PLANE_NOT_SET(VSaux->PR->dAltSel+SUMA_VOL_SLC_EQ0);
            if (vo && (dset = SUMA_VO_dset(vo)) &&
                      (dims = SUMA_GetDatasetDimensions(dset)) ) {
               Vox1D2Vox3D(sel, dims[0], dims[0]*dims[1], iv4);
                  VSaux->PR->iAltSel[SUMA_VOL_I] = iv4[0];
                  VSaux->PR->iAltSel[SUMA_VOL_J] = iv4[1];
                  VSaux->PR->iAltSel[SUMA_VOL_K] = iv4[2];
                  VSaux->PR->iAltSel[SUMA_VOL_IJK] = sel;
               if (SUMA_PLANE_IS_SET(VSaux->PR->dAltSel+SUMA_VOL_SLC_EQ0)) {
                  iv3[0] = (int)VSaux->PR->iAltSel[SUMA_VOL_I];
                  iv3[1] = (int)VSaux->PR->iAltSel[SUMA_VOL_J];
                  iv3[2] = (int)VSaux->PR->iAltSel[SUMA_VOL_K];
                  SUMA_SHIFT_PLANE_TO_P((VSaux->PR->dAltSel+SUMA_VOL_SLC_EQ0),
                                        SUMA_VO_PointXYZ (vo, -1, iv3, NULL));
               }
            }
         }   
         return(0);
         break; }
      case MASK_type: {
         SUMA_S_Warn("Not ready for mask type");
         return(0);
         break; }
      default:
         return(0);
   }
   return(0);
}

int SUMA_ADO_N_Datum(SUMA_ALL_DO *ado)
{
   return(SUMA_ADO_N_Datum_Lev(ado, SUMA_ELEM_DAT));
}

int SUMA_ADO_N_Datum_Lev(SUMA_ALL_DO *ado, SUMA_DATUM_LEVEL dtlvl)
{
   static char FuncName[]={"SUMA_ADO_N_Datum_Lev"};
   if (!ado) return(-1);
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         return(SO->N_Node);
         break; }
      case VO_type: {
         SUMA_VolumeObject *VO = (SUMA_VolumeObject *)ado;
         SUMA_DSET *dset = SUMA_VE_dset(VO->VE, 0);
         return(dset ? SDSET_NVOX(dset):-1);
         break; }
      case MASK_type: {
         SUMA_MaskDO *MDO = (SUMA_MaskDO *)ado;
         if (MDO->SO && 
             (MDO_IS_SURF(MDO) || MDO_IS_BOX(MDO) || MDO_IS_SPH(MDO)) ) {
            return(MDO->SO->N_Node);
         } else if (MDO_IS_BOX(MDO)) {
            return(MDO->N_obj*8);
         } else if (MDO_IS_SPH(MDO)) {
            SUMA_S_Err("No SO on spheres mask. Need to create your surfs");
            return(-1);
         } else if (MDO_IS_SHADOW(MDO)){
            return(0);
         } else {
            SUMA_S_Err("Not ready for this combo type >%s<", MDO->mtype);
            return(-1);
         }
         return(-1);
         break; }
      case CDOM_type: {
         SUMA_CIFTI_DO *CO=(SUMA_CIFTI_DO *)ado;
	 switch(dtlvl) {
	    int i, nn;
	    case SUMA_ELEM_DAT: /* The maximum possible number of data
	             	      	   assuming all domains are filled to 
				   the max */
	       nn = 0;
	       for (i=0; i<CO->N_subdoms; ++i) {
	          nn += SUMA_ADO_N_Datum(SUMA_CIFTI_subdom_ado(CO,i));
	       }
	       return(nn);
	       break;
	    default:
	       SUMA_S_Err("Should not be here, not yet at least (dtlvl = %d)", 
	             	  dtlvl);
	       return(-1);
	       break;
	 }
         return(-1);
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         return(SDSET_VECLEN(dset));
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            return(-1);
         }
         return(SUMA_ADO_N_Datum((SUMA_ALL_DO *)dset));
         break; }
      case TRACT_type: {
         SUMA_TractDO *tdo=(SUMA_TractDO *)ado;
         if (tdo->N_datum == -2) { /* initialize */
            tdo->N_datum = Network_N_points(tdo->net, 0);
         }
         switch(dtlvl) {
            default:
            case SUMA_ELEM_DAT:
               return(tdo->N_datum);
               break;
            case SUMA_LEV1_DAT:
               return(TDO_N_TRACTS(tdo));
               break;
            case SUMA_LEV2_DAT:
               return(TDO_N_BUNDLES(tdo));
               break;
         }
         break; }
      default:
         return(-1);
   }
   return(-1);
}

int SUMA_ADO_Max_Datum_Index(SUMA_ALL_DO *ado) 
{
   if (!ado) return(-1);
   return(SUMA_ADO_Max_Datum_Index_Lev(ado, SUMA_ELEM_DAT));
}

/*! This function needs revisiting for all objects.
    Its intention should be to return the highest index
    of a datum, but in a sparse set, this max could very
    well be different from the number of elements. 
    Check use in code and replace with the maximum of the
    NodeDef element if change is needed */
int SUMA_ADO_Max_Datum_Index_Lev(SUMA_ALL_DO *ado, SUMA_DATUM_LEVEL dtlvl)
{
   static char FuncName[]={"SUMA_ADO_Max_Datum_Index_Lev"};
   if (!ado) return(-1);
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         return(SO->N_Node-1);
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         if (SUMA_isGraphDset(dset)) {
            int mm;
            GDSET_MAX_EDGE_INDEX(dset,mm);
            return(mm);
         } else {
            return(SDSET_VECLEN(dset)-1);
         }
         break; }
      case CDOM_type: {
         SUMA_S_Err("Riddle me this");
         return(-1);
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            return(-1);
         }
         return(SUMA_ADO_Max_Datum_Index_Lev((SUMA_ALL_DO *)dset, dtlvl));
         break; }
      case TRACT_type: {
         SUMA_TractDO *tdo=(SUMA_TractDO *)ado;
         if (tdo->N_datum == -2) { /* initialize */
            tdo->N_datum = Network_N_points(tdo->net, 0);
         }
         switch(dtlvl) {
            default:
            case SUMA_ELEM_DAT:
               return(tdo->N_datum-1);
               break;
            case SUMA_LEV1_DAT:
               return(TDO_N_TRACTS(tdo)-1);
               break;
            case SUMA_LEV2_DAT:
               return(TDO_N_BUNDLES(tdo)-1);
               break;
         }
         break; }
      case VO_type: {
         SUMA_VolumeObject *VO = (SUMA_VolumeObject *)ado;
         SUMA_DSET *dset = SUMA_VE_dset(VO->VE, 0);
         return(dset ? SDSET_NVOX(dset)-1:-1);
         }
      case MASK_type: {
         SUMA_MaskDO *MDO = (SUMA_MaskDO *)ado;
         if (MDO_IS_SURF(MDO) || MDO_IS_BOX(MDO) || MDO_IS_SPH(MDO)) {
            if (!MDO->SO) {
               SUMA_S_Err("Need my SO baby");
               return(-1);
            }
            return(MDO->SO->N_Node-1);
         } else {
            SUMA_S_Warn("Not ready");
            return(-1);
         }
         }
      default:
         return(-1);
   }
   return(-1);
   
}


char * SUMA_ADO_variant(SUMA_ALL_DO *ado) {
   static char FuncName[]={"SUMA_ADO_variant"};
   if (!ado) return("");
   switch(ado->do_type) {
      default: {
         return("");
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         if (gldo->variant) {
            return(gldo->variant);
         }
         return("");
         break; }
   }
   return("");   
}

char * SUMA_ADO_Label(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_ADO_Label"};
   if (!ado) return(NULL);
   switch(ado->do_type) {
      default: {
         return(ado->private_Label);
         break; }
      case GDSET_type:
      case MD_DSET_type:
      case ANY_DSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         return(SDSET_LABEL(dset));
         break; }
   }
   return(NULL);
}

/* Like ADO_Label, but return empty string in err */
char *SUMA_ADO_sLabel(SUMA_ALL_DO *ado) {
   static char FuncName[]={"SUMA_ADO_sLabel"};
   char *cc = SUMA_ADO_Label(ado);
   if (!cc) return("");
   else return(cc);
}

char * SUMA_ADO_CropLabel(SUMA_ALL_DO *ado, int len)
{
   static char FuncName[]={"SUMA_ADO_CropLabel"};
   static char s[10][130];
   static int icall=0;
   char *str=NULL;
   
   ++icall;
   if (icall > 9) icall = 0;
   s[icall][0]='\0';
   
   if (!ado) { SUMA_S_Err("NULL input"); return(s[icall]); }
   if (len > 127) {
      SUMA_S_Warn("Label max length is 128, will truncate");
      len = 128;
   }
   
   str = SUMA_truncate_string(SUMA_ADO_Label(ado), len);
   if (!str) return(s[icall]);
   
   strcpy(s[icall], str);
   SUMA_ifree(str);
   
   return(s[icall]);
}

/* compare labels, NULL=NULL --> OK */
SUMA_Boolean SUMA_ADO_isLabel(SUMA_ALL_DO *ado, char *lbl) 
{
   static char FuncName[]={"SUMA_ADO_isLabelSUMA_ALL_DO"};
   char *cc=NULL;
   
   if (!ado) return(NOPE);
   cc = SUMA_ADO_Label(ado);
   if (!cc) {
      if (!lbl) return(YUP);
   } else {
      if (!strcmp(cc, lbl)) return(YUP);
   }
   return(NOPE);
}

/* Because of the damned SUMA_DSET * object, which needs
the Linked Pointer fields on the top, never access
ado->idcode_str directly, but use this function
instead*/
char * SUMA_ADO_idcode(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_ADO_idcode"};
   if (!ado) return(NULL);
   switch(ado->do_type) {
      default: {
         return(ado->private_idcode_str);
         break; }
      case ANY_DSET_type:
      case MD_DSET_type:
      case GDSET_type: {/* The special beast */
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         return(SDSET_ID(dset));
         break; }
   }
   return(NULL);
}

char * SUMA_ADO_Parent_idcode(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_ADO_Parent_idcode"};
   if (!ado) return(NULL);
   switch(ado->do_type) {
      case ANY_DSET_type:
      case MD_DSET_type:
      case GDSET_type: {/* The special beast, as a DO, it is its own parent*/
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         return(SDSET_ID(dset));
         break; }
      case GRAPH_LINK_type:
         return(((SUMA_GraphLinkDO*)ado)->Parent_idcode_str);
         break;
      case SP_type:
         return(((SUMA_SphereDO*)ado)->Parent_idcode_str);
         break;
      case TRACT_type:
         return(((SUMA_TractDO*)ado)->Parent_idcode_str);
         break;
      case MASK_type:
         return(((SUMA_MaskDO*)ado)->Parent_idcode_str);
         break;
      case NBLS_type:
      case NBOLS_type:
      case NBV_type:
      case ONBV_type:
      case NBSP_type:
      case NBT_type:
         return(((SUMA_NB_DO*)ado)->Parent_idcode_str);
         break;
      case NIDO_type:
         if (((SUMA_NIDO*)ado)->ngr) {
            return(NI_get_attribute(
                     ((SUMA_NIDO*)ado)->ngr, "Parent_idcode_str"));
         } else return(NULL);
         break;
      case SO_type:
         return(((SUMA_SurfaceObject*)ado)->LocalDomainParentID);
         break;
      case ROIdO_type:
         return(((SUMA_DRAWN_ROI*)ado)->Parent_idcode_str);
         break;
      case AO_type: /* those are their own parents */
      case PL_type: 
      case CDOM_type:
      case VO_type:
         return(ado->private_idcode_str);
         break;
      default: {
         SUMA_S_Errv("Not ready for parent of %s\n",
               SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         return(NULL);
         break; }
      
   }
   return(NULL);
}

/*!
   Return the "SurfaceController" for any DO 
*/
SUMA_X_SurfCont *SUMA_ADO_Cont(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_ADO_Cont"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (!ado) return(NULL);
   SUMA_LHv("Here with %d (%s), %s\n", 
             ado->do_type, ADO_TNAME(ado), SUMA_ADO_Label(ado));
   
   switch(ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         return(SO->SurfCont);
         break; }
      case CDOM_type: {
         SUMA_CIFTI_SAUX * CSaux = (SUMA_CIFTI_SAUX *)SUMA_ADO_Saux(ado);
         if (CSaux) return(CSaux->DOCont);
         else return(NULL);
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *GSaux = SDSET_GSAUX(dset);
         if (!GSaux) return(NULL);
         return(GSaux->DOCont);
         break; }
      case GRAPH_LINK_type: {
         SUMA_DSET *dset=NULL;
         /* get the graph dset, which is the pointer holding
            the controller pointer */
         if (!(dset = SUMA_find_GLDO_Dset((SUMA_GraphLinkDO *)ado))) {
            SUMA_S_Err("No graph dset for GLDO???");
            return(NULL);
         }
         return(SUMA_ADO_Cont((SUMA_ALL_DO *)dset));
         break; }
      case TRACT_type: {
         SUMA_TRACT_SAUX * TSaux = (SUMA_TRACT_SAUX *)SUMA_ADO_Saux(ado);
         if (TSaux) return(TSaux->DOCont);
         else return(NULL);
         break; }
      case MASK_type: {
         SUMA_MASK_SAUX * MSaux = (SUMA_MASK_SAUX *)SUMA_ADO_Saux(ado);
         if (MSaux) return(MSaux->DOCont);
         else return(NULL);
         break; }
      case VO_type: {
         SUMA_VOL_SAUX * VSaux = (SUMA_VOL_SAUX *)SUMA_ADO_Saux(ado);
         SUMA_LH("Have %p", VSaux);
         if (VSaux) return(VSaux->DOCont);
         else return(NULL);
         break; }
      default:
         return(NULL);
   }
   return(NULL);
}

SUMA_Boolean SUMA_ADO_ShowCurForeOnly(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_ADO_ShowCurForeOnly"};
   SUMA_X_SurfCont *sc=NULL;
   
   if (!ado || !(sc = SUMA_ADO_Cont(ado))) return(NOPE);
   
   return(sc->ShowCurForeOnly);
}

SUMA_ALL_DO *SUMA_Cont_ADO(SUMA_X_SurfCont *SurfCont)
{
   static char FuncName[]={"SUMA_Cont_ADO"};
   if (!SurfCont) return(NULL);
   return(SUMA_SurfCont_GetcurDOp(SurfCont));
}

SUMA_SurfaceObject *SUMA_Cont_SO(SUMA_X_SurfCont *SurfCont)
{
   static char FuncName[]={"SUMA_Cont_SO"};
   SUMA_ALL_DO *ado=NULL;
   if (!SurfCont) return(NULL);
   ado = SUMA_SurfCont_GetcurDOp(SurfCont);
   if (ado->do_type == SO_type) 
      return((SUMA_SurfaceObject *)ado);
   return(NULL);
}

/*!
   SUMA_isCurColPlane: Is this color plane the current one ? 
   This is meant to replace if (colp == SO->SurfCont->curColPlane)
   so that it would work for any DO that would have a colorplane.
   This query will always return yes for a DSET_type DO
   
   You might want to consider the alternative SUMA_isTopColPlane()
   which is identical to  SUMA_isCurColPlane() when using the old style
   multiple surface controller windows, versus the modern multi-page controller
*/
SUMA_Boolean SUMA_isCurColPlane(SUMA_OVERLAYS *cp, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_isCurColPlane"};
   if (!cp || !ado) return(NOPE);
   if (cp == SUMA_ADO_CurColPlane(ado)) return(YUP);
   return(NOPE);
}

SUMA_Boolean SUMA_isTopColPlane(SUMA_OVERLAYS *cp, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_isTopColPlane"};
   SUMA_X_SurfCont *SurfCont=NULL;
   if (!SUMAg_CF->X->UseSameSurfCont) return(SUMA_isCurColPlane(cp, ado));
   else if (SUMA_isCurColPlane(cp, ado) && (SurfCont = SUMA_ADO_Cont(ado))) {
      /* OK, so that plane is current in some fashion, 
      is it the top page of the uber controller ? */
      return(SUMA_isCurrentContPage(SUMAg_CF->X->SC_Notebook, SurfCont->Page));
   } 
   return(NOPE);
}

/* Used to be MACRO  SUMA_SURFCONT_CREATED */
SUMA_Boolean SUMA_isADO_Cont_Created(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_isADO_Cont_Created"};
   SUMA_X_SurfCont *SurfCont=NULL;
   
   if ((SurfCont = SUMA_ADO_Cont(ado)) && SurfCont->TLS ) return(1);
   
   return(0); 
}
    
/* Used to be MACRO  SUMA_SURFCONT_REALIZED */
SUMA_Boolean SUMA_isADO_Cont_Realized(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_isADO_Cont_Realized"};
   SUMA_X_SurfCont *SurfCont=NULL;
   
   if ((SurfCont = SUMA_ADO_Cont(ado)) && SurfCont->TLS 
       && XtIsRealized(SurfCont->TLS)) return(1);
   
   return(0); 
}

#define NVALS_XYZ_NODE 3				      
float *SUMA_GDSET_NodeXYZ(SUMA_DSET *dset, int node, char *variant, float *here) 
{
   static char FuncName[]={"SUMA_GDSET_NodeXYZ"};
   static int icall=0;
   static float fv[10][NVALS_XYZ_NODE];
   int N_Node=-1, *Node_Ind=NULL, cinode = -1;
   float *ff=NULL, *NodeList = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   
   SUMA_GDSET_NodeXYZ_eng(dset, node, variant, here);
      
   SUMA_RETURN(here);
}

SUMA_Boolean SUMA_GDSET_NodeXYZ_eng(SUMA_DSET *dset, int node, 
                                    char *variant, float *here) 
{
   static char FuncName[]={"SUMA_GDSET_NodeXYZ_eng"};
   int N_Node=-1, *Node_Ind=NULL, cinode = -1;
   float *ff=NULL, *NodeList = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      SUMA_S_Err("Need output pointer");
      SUMA_RETURN(NOPE);
   }
   here[0] = here[1] = here[2] =  0.0;
   
   if (!dset || node < 0) SUMA_RETURN(NOPE);
   
   if (!(NodeList = SUMA_GDSET_NodeList(dset, &N_Node, 0, &Node_Ind, variant))) 
                                                      SUMA_RETURN(NOPE);
   
   /* get position of node n in NodeList */
   cinode =  SUMA_NodeIndex_To_Index(Node_Ind, N_Node, node);      
   SUMA_LHv("Node %d is in row %d of the nodelist of %d nodes\n",
            node, cinode, N_Node);
   
   if (cinode >= 0 && cinode < N_Node) {
      ff = NodeList+3*cinode;
      here[0] = *ff; ++ff; 
      here[1] = *ff; ++ff; 
      here[2] = *ff;
      SUMA_RETURN(YUP);
   } else {
      SUMA_LHv("Node %d not found in node list\n", node);
      SUMA_RETURN(NOPE);
   } 
   
   SUMA_RETURN(NOPE);
}

float *SUMA_TDO_PointXYZ(SUMA_TractDO *tdo, int point, int *BTP, float *here) 
{
   static char FuncName[]={"SUMA_TDO_PointXYZ"};
   static int icall=0;
   static float fv[10][NVALS_XYZ_NODE];
   int N_Node=-1, *Node_Ind=NULL, cinode = -1;
   float *ff=NULL, *NodeList = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   
   SUMA_TDO_PointXYZ_eng(tdo, point, BTP, here);
      
   SUMA_RETURN(here);
}

SUMA_Boolean SUMA_TDO_PointXYZ_eng(SUMA_TractDO *tdo, int point, 
                                   int *BTP, float *here) 
{
   static char FuncName[]={"SUMA_TDO_PointXYZ_eng"};
   int N_Node=-1, *Node_Ind=NULL, cinode = -1, iv3[3], nn3;
   float *ff=NULL, *NodeList = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      SUMA_S_Err("Need output pointer");
      SUMA_RETURN(NOPE);
   }
   here[0] = here[1] = here[2] =  0.0;
   
   if (!tdo || !tdo->net || point < 0) SUMA_RETURN(NOPE);
   
   if (BTP && BTP[0] >= 0 && BTP[1] >= 0 && BTP[2] >=0) {
      iv3[SUMA_NET_BUN] = BTP[SUMA_NET_BUN];
      iv3[SUMA_BUN_TRC] = BTP[SUMA_BUN_TRC];
      iv3[SUMA_TRC_PNT] = BTP[SUMA_TRC_PNT];
   } else if (point >= 0) {
      if (!Network_1P_to_PTB(tdo->net, point, 
               iv3+SUMA_TRC_PNT, iv3+SUMA_BUN_TRC, iv3+SUMA_NET_BUN, NULL)) {
         SUMA_S_Err("Bad point index");
         SUMA_RETURN(NOPE);
      }
   }
   
   if (iv3[SUMA_NET_BUN] < TDO_N_BUNDLES(tdo)) {
      if (iv3[SUMA_BUN_TRC] < tdo->net->tbv[iv3[SUMA_NET_BUN]]->N_tracts) {
         nn3 = iv3[SUMA_TRC_PNT]*3;
         if (nn3 < 
             tdo->net->tbv[iv3[SUMA_NET_BUN]]->tracts[iv3[SUMA_BUN_TRC]].N_pts3){
            ff = 
             tdo->net->tbv[iv3[SUMA_NET_BUN]]->tracts[iv3[SUMA_BUN_TRC]].pts+nn3;
            here[0] = ff[0];
            here[1] = ff[1];
            here[2] = ff[2];
            SUMA_RETURN(YUP);
         }
      }
   } else {
      SUMA_LHv("Point %d (B%d T%d P%d) not found in node list\n", 
         point, iv3[SUMA_NET_BUN], iv3[SUMA_BUN_TRC], iv3[SUMA_TRC_PNT]);
      SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(NOPE);
}

float *SUMA_VO_PointXYZ(SUMA_VolumeObject *vo, int point, int *IJK, float *here) 
{
   static char FuncName[]={"SUMA_VO_PointXYZ"};
   static int icall=0;
   static float fv[10][NVALS_XYZ_NODE];
   int N_Node=-1, *Node_Ind=NULL, cinode = -1;
   float *ff=NULL, *NodeList = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   
   SUMA_VO_PointXYZ_eng(vo, point, IJK, here);
      
   SUMA_RETURN(here);
}

SUMA_Boolean SUMA_VO_PointXYZ_eng(SUMA_VolumeObject *vo, int point, 
                                   int *IJK, float *here) 
{
   static char FuncName[]={"SUMA_VO_PointXYZ_eng"};
   int N_Node=-1, *Node_Ind=NULL, cinode = -1, iv3[3]={0,0,0}, nn3;
   float *ff=NULL, *NodeList = NULL;
   float I[3];
   int *dims;
   SUMA_DSET *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;

    
   SUMA_ENTRY;
   
   
   if (!here) {
      SUMA_S_Err("Need output pointer");
      SUMA_RETURN(NOPE);
   }
   here[0] = here[1] = here[2] =  0.0;
   
   if (!vo || 
      (point < 0 && !(IJK && IJK[0] >= 0 && IJK[1] >= 0 && IJK[2] >=0) )) 
      SUMA_RETURN(NOPE);
   if (!(dset = SUMA_VO_dset(vo)) || 
       !(dims = SUMA_GetDatasetDimensions(dset)) ||
       !vo->VE || !vo->VE[0]) {
      SUMA_S_Err("no valid ijk_to_dicom_real") ;
      SUMA_RETURN(NOPE);
   }
   
   
   if (IJK && IJK[0] >= 0 && IJK[1] >= 0 && IJK[2] >=0) {
      iv3[SUMA_VOL_I] = IJK[SUMA_VOL_I];
      iv3[SUMA_VOL_J] = IJK[SUMA_VOL_J];
      iv3[SUMA_VOL_K] = IJK[SUMA_VOL_K];
   } else if (point >= 0 && point < SDSET_NVOX(dset)) {
      Vox1D2Vox3D(point, dims[0], dims[0]*dims[1], (iv3+SUMA_VOL_I));
   }
   
   if (iv3[SUMA_VOL_I] < dims[0]) {
      if (iv3[SUMA_VOL_J] < dims[1]) {
         if (iv3[SUMA_VOL_K] < dims[2] ) {
            AFF44_MULT_I(here, vo->VE[0]->I2X, (iv3+SUMA_VOL_I));
            SUMA_RETURN(YUP);
         }
      }
   } else {
      SUMA_LHv("Point %d (I%d J%d K%d) not found in grid\n", 
         point, iv3[SUMA_VOL_I], iv3[SUMA_VOL_J], iv3[SUMA_VOL_K]);
      SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(NOPE);
}

float *SUMA_MDO_PointXYZ(SUMA_MaskDO *mo, int point, int *IJK, float *here)
{
   static char FuncName[]={"SUMA_MDO_PointXYZ"};
   static int icall=0;
   static float fv[10][NVALS_XYZ_NODE];
   int N_Node=-1, *Node_Ind=NULL, cinode = -1;
   float *ff=NULL, *NodeList = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   
   SUMA_MDO_PointXYZ_eng(mo, point, IJK, here);
      
   SUMA_RETURN(here);
}

SUMA_Boolean SUMA_MDO_PointXYZ_eng(SUMA_MaskDO *mo, int point, 
                                   int *IJK, float *here) 
{
   static char FuncName[]={"SUMA_MDO_PointXYZ_eng"};
   int N_Node=-1, *Node_Ind=NULL, cinode = -1, iv3[3]={0,0,0}, nn3;
   float *ff=NULL, *NodeList = NULL;
   float I[3];
   int *dims;
   SUMA_Boolean LocalHead = NOPE;

    
   SUMA_ENTRY;
   
   if (!here) {
      SUMA_S_Err("Need output pointer");
      SUMA_RETURN(NOPE);
   }
   here[0] = here[1] = here[2] =  0.0;
   
   if (!mo || point < 0) SUMA_RETURN(NOPE);
   
   if (MDO_IS_BOX(mo) || MDO_IS_SPH(mo)) {
      if (IJK && IJK[0] >= 0 && IJK[1] >= 0) { /* obj index and point within */
         point = 3*IJK[0]+IJK[1];
      }
   }

   if (point < 0 || point >= SUMA_ADO_N_Datum((SUMA_ALL_DO *)mo)) 
      SUMA_RETURN(NOPE);
         
   if (!mo->SO) {
      SUMA_S_Err("SO no formed yet");
      SUMA_RETURN(NOPE);
   }   
   
   here[0] = mo->SO->NodeList[3*point];
   here[1] = mo->SO->NodeList[3*point+1];
   here[2] = mo->SO->NodeList[3*point+2];
   
   SUMA_RETURN(NOPE);
}


float *SUMA_GDSET_XYZ_Range(SUMA_DSET *dset,  char *variant, float *here) 
{
   static char FuncName[]={"SUMA_GDSET_XYZ_Range"};
   static int icall=0;
   static float fv[10][6];
   float *X, *Y, *Z;
   int *I;
   double nums[6];
   int iicoord;
   NI_element *nelxyz = NULL;
   char *ctmp=NULL, *rs=NULL, *cs=NULL, sbuf[24];   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   here[0] = here[1] = here[2] =  -10.0;
   here[0] = here[1] = here[2] =  10.0;

   if (!dset || !variant) SUMA_RETURN(here);
      
   if (!strcmp(variant,"G3D")) {
      if (!(nelxyz = SUMA_FindGDsetNodeListElement(dset))) {
         SUMA_S_Errv("Failed to find Dset %s's NodeListElement\n", 
                           SDSET_LABEL(dset));
         SUMA_RETURN(here);
      }
      if (!(cs = NI_get_attribute(nelxyz,"COLMS_LABS"))) {
         SUMA_S_Err("What can I say?");
         SUMA_RETURN(here);
      }
      if (!(rs = NI_get_attribute(nelxyz,"COLMS_RANGE"))) {
         SUMA_S_Err("Zut alors!");
         SUMA_RETURN(here);
      }
      
      /* Get the X range */
      if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode X"))<0) {
         SUMA_S_Err("Failed to find X"); 
         SUMA_RETURN(here);
      }
      ctmp = SUMA_Get_Sub_String(rs, SUMA_NI_CSS, iicoord);
      if (SUMA_StringToNum(ctmp, (void *)nums, 4, 2) != 4) { 
         SUMA_SL_Err("Failed to read 6 nums from range."); 
         SUMA_RETURN(here); 
      }
      here[0]=nums[0]; here[1]=nums[1];
      
      /* Get the Y range */
      if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode Y"))<0) {
         SUMA_S_Err("Failed to find Y"); 
         SUMA_RETURN(here);
      }
      ctmp = SUMA_Get_Sub_String(rs, SUMA_NI_CSS, iicoord);
      if (SUMA_StringToNum(ctmp, (void *)nums, 4, 2) != 4) { 
         SUMA_SL_Err("Failed to read 6 nums from range."); 
         SUMA_RETURN(here); 
      }
      here[2]=nums[0]; here[3]=nums[1];
      
      /* Get the Z range */
      if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode Z"))<0) {
         SUMA_S_Err("Failed to find Y"); 
         SUMA_RETURN(here);
      }
      ctmp = SUMA_Get_Sub_String(rs, SUMA_NI_CSS, iicoord);
      if (SUMA_StringToNum(ctmp, (void *)nums, 4, 2) != 4) { 
         SUMA_SL_Err("Failed to read 6 nums from range."); 
         SUMA_RETURN(here); 
      }
      here[4]=nums[0]; here[5]=nums[1];
   } else if (!strcmp(variant,"GMATRIX")) {
      /* This would be the range of the FrameSO */
      SUMA_SurfaceObject *SO = SUMA_GDSET_FrameSO(dset);
      if (SO) {
         SUMA_LHv("%f -- %f, %f -- %f, %f -- %f\n",
                  SO->MinDims[0], SO->MaxDims[0],
                  SO->MinDims[1], SO->MaxDims[1],
                  SO->MinDims[2], SO->MaxDims[2]);
         here[0] = SO->MinDims[0]; here[1] = SO->MaxDims[0];
         here[2] = SO->MinDims[1]; here[3] = SO->MaxDims[2];         
         here[4] = SO->MinDims[2]; here[5] = SO->MaxDims[2];         
      }
      SUMA_RETURN(here);
   } else if (!strcmp(variant,"GRELIEF")) {   
      SUMA_S_Err("Not ready yet for GRELIEF");
      SUMA_RETURN(here);
   } else if (!strncmp(variant,"TheShadow", 9)) {
      SUMA_LH("Who knows what evil lurks in the hearts of men?");
      SUMA_RETURN(here);
   } else {
      SUMA_S_Errv("Bad draw variant >%s< for %s\n", 
                  variant, SDSET_LABEL(dset));
      SUMA_DUMP_TRACE("Bad draw variant");
      SUMA_RETURN(here);
   }
   
   SUMA_RETURN(here);
}

float *SUMA_GDSET_XYZ_Center(SUMA_DSET *dset,  char *variant, float *here) 
{
   static char FuncName[]={"SUMA_GDSET_XYZ_Center"};
   static int icall=0;
   static float fv[10][3];
   float *X, *Y, *Z, meanI;
   double nums[6];
   int iicoord, *I;
   NI_element *nelxyz = NULL;
   char *cen=NULL, *ctmp=NULL, *rs=NULL, *cs=NULL, *stmp=NULL, sbuf[24];   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   here[0] = here[1] = here[2] =  0.0;
   
   if (!dset || !variant) SUMA_RETURN(here);
   
   if (!strcmp(variant,"G3D")) {
      if (!(nelxyz = SUMA_FindGDsetNodeListElement(dset))) {
         SUMA_S_Errv("Failed to find Dset %s's NodeListElement\n", 
                           SDSET_LABEL(dset));
         SUMA_RETURN(here);
      }
      /* create and store the thing */
      if (!(cs = NI_get_attribute(nelxyz,"COLMS_LABS"))) {
         SUMA_S_Err("What can I say?");
         SUMA_RETURN(here);
      }
      if (!(cen = NI_get_attribute(nelxyz,"COLMS_AVG"))) {
        /* Stupid but have to do it for COLMS_AVG */   
         /* Get the I col */
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, 
                                              "Gnode Index"))<0) {
            SUMA_S_Err("Failed to find I"); 
            SUMA_RETURN(here);
         }
         I = (int *)nelxyz->vec[iicoord];
         SUMA_MEAN_VEC(I, nelxyz->vec_len, meanI, 0); 
         sprintf(sbuf,"%f", meanI); 
         if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, iicoord,sbuf)) {
            SUMA_LHv("Failed to set substring to %s\n", stmp);
            SUMA_RETURN(here);
         }
            
         /* Get the X col */
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode X"))<0) {
            SUMA_S_Err("Failed to find X"); 
            SUMA_RETURN(here);
         }
         X = (float *)nelxyz->vec[iicoord];
         SUMA_MEAN_VEC(X, nelxyz->vec_len, here[0], 0);
         sprintf(sbuf,"%f", here[0]); 
         if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, iicoord,sbuf)) {
            SUMA_LHv("Failed to set substring to %s\n", stmp);
            SUMA_RETURN(here);
         }
            
         /* Get the Y col */
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode Y"))<0) {
            SUMA_S_Err("Failed to find Y"); 
            SUMA_RETURN(here);
         }
         Y = (float *)nelxyz->vec[iicoord];
         SUMA_MEAN_VEC(Y, nelxyz->vec_len, here[1], 0);
         sprintf(sbuf,"%f", here[1]); 
         if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, iicoord,sbuf)) {
            SUMA_LHv("Failed to set substring to %s\n", stmp);
            SUMA_RETURN(here);
         }
         
         /* Get the Z col */
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode Z"))<0) {
            SUMA_S_Err("Failed to find Z"); 
            SUMA_RETURN(here);
         }
         Z = (float *)nelxyz->vec[iicoord];
         SUMA_MEAN_VEC(Z, nelxyz->vec_len, here[2], 0);
         sprintf(sbuf,"%f", here[2]); 
         if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, iicoord,sbuf)) {
            SUMA_LHv("Failed to set substring to %s\n", stmp);
            SUMA_RETURN(here);
         }
        
         NI_set_attribute(nelxyz,"COLMS_AVG", stmp);
         SUMA_LHv("Computed for %s to be: %f %f %f: (stored in %s)\n", 
                  variant, here[0], here[1], here[2], stmp);
         SUMA_ifree(stmp);
      } else {
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode X"))<0) {
            SUMA_S_Err("Failed to find X"); 
            SUMA_RETURN(here);
         }
         ctmp = SUMA_Get_Sub_String(cen, SUMA_NI_CSS, iicoord);
         if (SUMA_StringToNum(ctmp, (void *)nums, 1, 2) != 1) { 
            SUMA_SL_Err("Failed to read 1 num from range."); 
            SUMA_RETURN(here); 
         }
         here[0]=nums[0];
         
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode Y"))<0) {
            SUMA_S_Err("Failed to find Y"); 
            SUMA_RETURN(here);
         }
         ctmp = SUMA_Get_Sub_String(cen, SUMA_NI_CSS, iicoord);
         if (SUMA_StringToNum(ctmp, (void *)nums, 1, 2) != 1) { 
            SUMA_SL_Err("Failed to read 1 num from range."); 
            SUMA_RETURN(here); 
         }
         here[1]=nums[0];
         
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode Z"))<0) {
            SUMA_S_Err("Failed to find Z"); 
            SUMA_RETURN(here);
         }
         ctmp = SUMA_Get_Sub_String(cen, SUMA_NI_CSS, iicoord);
         if (SUMA_StringToNum(ctmp, (void *)nums, 1, 2) != 1) { 
            SUMA_SL_Err("Failed to read 1 num from range."); 
            SUMA_RETURN(here); 
         }
         here[2]=nums[0];
         SUMA_LHv("Retrieved (%s) from header for %s to be: %f %f %f:\n", 
                     cen, variant, here[0], here[1], here[2]);
      }
   } else if (!strcmp(variant,"GMATRIX")) {
      SUMA_SurfaceObject *SO=SUMA_GDSET_FrameSO(dset);
      if (!SO) {
         SUMA_S_Warnv("No Frame SO for %s?\n", SDSET_LABEL(dset));
         SUMA_RETURN(here);
      }
      here[0] = SO->Center[0]; here[1] = SO->Center[1]; here[2] = SO->Center[2];
      SUMA_LHv("From SO for %s : %f %f %f:\n", 
                  variant, here[0], here[1], here[2]);
      SUMA_RETURN(here);
   } else if (!strcmp(variant,"GRELIEF")) {   
      SUMA_S_Err("Not ready yet for GRELIEF");
      SUMA_RETURN(here);
   } else if (!strncmp(variant,"TheShadow", 9)) {
      SUMA_LH("Who knows what evil lurks in the hearts of men?");
      SUMA_RETURN(here);
   } else {
      SUMA_S_Errv("Bad draw variant >%s< for %s\n", 
                  variant, SDSET_LABEL(dset));
      SUMA_DUMP_TRACE("Bad draw variant");
      SUMA_RETURN(here);
   }
   
   SUMA_RETURN(here);
}

#define NVALS_XYZ_EDGE 6	
float *SUMA_GDSET_EdgeXYZ(SUMA_DSET *dset, int isel, char *variant, float *here) 
{
   static char FuncName[]={"SUMA_GDSET_EdgeXYZ"};
   static int icall=0;
   static float fv[10][NVALS_XYZ_EDGE];
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
      
   SUMA_GDSET_EdgeXYZ_eng(dset, isel, variant, here);
            
   SUMA_RETURN(here);
}

SUMA_Boolean SUMA_GDSET_EdgeXYZ_eng(SUMA_DSET *dset, int isel, 
                                    char *variant, float *here) 
{
   static char FuncName[]={"SUMA_GDSET_EdgeXYZ_eng"};
   int N_Node=-1, *ind0=NULL, *ind1=NULL, *inde=NULL, i1=-1, i2=0;
   SUMA_GraphLinkDO *gldo=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      SUMA_S_Err("Must give me a pointer for results");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_LHv("Looking for XYZ of edge ID %d, variant %s on dset %s\n",
            isel, variant, SDSET_LABEL(dset));
            
   here[0] = here[1] = here[2] = 
   here[3] = here[4] = here[5] = 0.0;
   
   if (!dset || isel < 0) SUMA_RETURN(NOPE);
   if (!variant) {
      SUMA_S_Err("No XYZ on dset without variant");
      SUMA_RETURN(NOPE);  
   }
   
   if (isel <= SUMA_GDSET_Max_Edge_Index(dset)) {
      SDSET_EDGE_NODE_INDEX_COLS(dset, inde, ind0, ind1);
      if (!ind0 || !ind1 || !inde) {
         SUMA_LH("No explicit node idexing information");
         SUMA_RETURN(NOPE);
      }
      if (inde[isel] != isel) {
         SUMA_LHv("Hard way for segment index %d: i1=%d, i2 = %d\n",
                        isel, i1, i2);         
         /* Fetch the indices of the nodes forming the edge */
         if (!SUMA_GDSET_SegIndexToPoints(dset, isel, &i1, &i2, NULL)) {
            SUMA_S_Errv("Failed to locate nodes of edge %d on dset %s\n",
                        isel, SDSET_LABEL(dset));
            SUMA_RETURN(NOPE);
         }
      } else { /* the easy way */
         SUMA_LHv("Easy way: inde[%d]=%d [%d %d]\n",
                  isel, inde[isel], ind0[isel], ind1[isel]);
         i1 = ind0[isel];
         i2 = ind1[isel];
      }
      
      if (i1 < 0 || i2 < 0) SUMA_RETURN(NOPE);
      
      if (!strcmp(variant,"GMATRIX")) {
         SUMA_LHv("Here looking for XYZ of %d %d\n", i1, i2);
         /* Get an edge coord based on the matrix being displayed */
         if (SUMA_GDSET_edgeij_to_GMATRIX_XYZ(dset, i1, i2, here, 0)) {
            /* duplicate the 1st three vals to make results consistent
               with G3D case */
            here[3] = here[0]; here[4]=here[1]; here[5]=here[2];
         } else {
            SUMA_S_Err("Should this happen ?");
            SUMA_RETURN(NOPE);
         }
      } else { /* the 3D one */
         if (!(SUMA_GDSET_NodeXYZ_eng(dset, i1, variant, here))) 
               SUMA_RETURN(NOPE);
         if (!(SUMA_GDSET_NodeXYZ(dset, i2, variant, here+3)))
               SUMA_RETURN(NOPE);
      }
      
      SUMA_LHv("Selection request for edge %d [%d %d] variant %s\n"
                   "here=[%f %f %f %f %f %f]\n", 
                   isel, i1, i2, variant, 
                   here[0], here[1], here[2], here[3], here[4], here[5]);
   } else {
      SUMA_LHv("isel=%d, veclen=%d, max edge index %d\n", 
               isel, SDSET_VECLEN(dset), SUMA_GDSET_Max_Edge_Index(dset));
   }
         
   SUMA_RETURN(YUP);
}

SUMA_SurfaceObject *SUMA_GDSET_FrameSO(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GDSET_FrameSO"};
   SUMA_GRAPH_SAUX *GSaux = NULL;
   
   SUMA_ENTRY;
   
   if (!(GSaux = SDSET_GSAUX(dset))) {
      SUMA_S_Err("Cannot create an SO this early, or dset is not graph");
      SUMA_RETURN(NULL);
   }
   if (!GSaux->nido && !(GSaux->nido = SUMA_GDSET_matrix_nido(dset))) {
      SUMA_S_Err("No milk!");
      SUMA_DUMP_TRACE("%s", FuncName);
      SUMA_RETURN(NULL);
   }
   
   if (!GSaux->FrameSO) {
      /* need to make one */
      GSaux->FrameSO = SUMA_Surface_Of_NIDO_Matrix(GSaux->nido);
   }
   
   SUMA_RETURN(GSaux->FrameSO);
}

SUMA_Boolean SUMA_GDSET_GMATRIX_Aff(SUMA_DSET *dset, double Aff[4][4], int I2X)
{
   static char FuncName[]={"SUMA_GDSET_GMATRIX_Aff"};
   SUMA_GRAPH_SAUX *GSaux=NULL;
   double V[12];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!(GSaux = SDSET_GSAUX(dset)) || !GSaux->nido) SUMA_RETURN(NOPE);
   
   if (I2X) {
      NI_GET_DOUBLEv(GSaux->nido->ngr, "ijk_to_dicom_real", V, 12, LocalHead);
      if (!NI_GOT) {
         SUMA_S_Err("No ijk_to_dicom_real");
         SUMA_RETURN(NOPE);
      }   
      V12_TO_AFF44(Aff, V);
   } else {
      NI_GET_DOUBLEv(GSaux->nido->ngr, "dicom_real_to_ijk", V, 12, LocalHead);
      if (!NI_GOT) {
         SUMA_S_Err("No dicom_real_to_ijk");
         SUMA_RETURN(NOPE);
      }   
      V12_TO_AFF44(Aff, V);
   }
   
   SUMA_RETURN(YUP);
}
/* Return coordinates of a datum, failure returns 0 0 0 
   Note that what a datum represents will differ
   depending on the object*/
#define NVALS_XYZ_DATUM 6
float *SUMA_ADO_DatumXYZ(SUMA_ALL_DO *ado, int isel, char *variant) 
{
   static char FuncName[]={"SUMA_ADO_DatumXYZ"};
   static int icall=0;
   static float fv[10][NVALS_XYZ_DATUM];
   
   SUMA_ENTRY;
   
   ++icall; if (icall > 9) icall = 0;
   fv[icall][0] = fv[icall][1] = fv[icall][2] = 
   fv[icall][3] = fv[icall][4] = fv[icall][5] = 0.0;
   
   if (!ado || isel < 0) SUMA_RETURN(fv[icall]);
   switch (ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         if (SO->NodeList && isel < SO->N_Node) {
            float *ff = SO->NodeList+SO->NodeDim*isel;
            fv[icall][0] = *ff;++ff;
            if (SO->NodeDim > 1) fv[icall][1] = *ff;++ff;
            if (SO->NodeDim > 2) fv[icall][2] = *ff;++ff;
         }
         break; }
      case CDOM_type: {
         SUMA_S_Err("Not ready: 1- find domain from index.\n"
                    "           2- find index on domain\n"
                    "           3- return XYZ\n");
         return(NULL);
         break;
         }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         if (!variant) {
            SUMA_S_Err("No XYZ without variant on dsets");
         } else {
            return(SUMA_GDSET_EdgeXYZ(dset, isel, variant, 
                                      (float *)(&fv[icall])));
         }
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            SUMA_RETURN(fv[icall]);
         }
         if (!variant) variant = SUMA_ADO_variant(ado);
         return(SUMA_GDSET_EdgeXYZ(dset, isel, variant, (float *)(&fv[icall])));
         break; }
      default:
         /* no coords */
         break;
   }
   
   SUMA_RETURN(fv[icall]);
}

/*! Return the local domain parent for an ado */
char *SUMA_ADO_LDP(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_ADO_LDP"};
   
   if (!ado) return(NULL);
   switch (ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         return(SO->LocalDomainParentID);
         break; }
      case ANY_DSET_type:
      case MD_DSET_type:
      case GDSET_type: {
         SUMA_DSET *dset = (SUMA_DSET *)ado;
         return(SDSET_ID(dset)); /* itself */
         break; }
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            return(NULL);
         }
         return(SUMA_ADO_LDP((SUMA_ALL_DO *)dset));
         break; }
      case CDOM_type: {
         SUMA_S_Warn("Not sure if this will apply yet.");
         return(NULL);
         break; }
      default:
         return(NULL);
   }
   return(NULL);
}

SUMA_CIFTI_SAUX *SUMA_ADO_CSaux(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_ADO_CSaux"};
   
   if (!ado) return(NULL);
   switch (ado->do_type) {
      case CDOM_type:
         return((SUMA_CIFTI_SAUX *)SUMA_ADO_Saux(ado));
         break;
      default:
         return(NULL);
   }
   return(NULL);
}

SUMA_GRAPH_SAUX *SUMA_ADO_GSaux(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_ADO_GSaux"};
   
   if (!ado) return(NULL);
   switch (ado->do_type) {
      case GDSET_type:
      case GRAPH_LINK_type:   
         return((SUMA_GRAPH_SAUX *)SUMA_ADO_Saux(ado));
         break;
      default:
         return(NULL);
   }
   return(NULL);
}

SUMA_TRACT_SAUX *SUMA_ADO_TSaux(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_ADO_TSaux"};
   
   if (!ado) return(NULL);
   switch (ado->do_type) {
      case TRACT_type:
         return((SUMA_TRACT_SAUX *)SUMA_ADO_Saux(ado));
         break;
      default:
         return(NULL);
   }
   return(NULL);
}

SUMA_MASK_SAUX *SUMA_ADO_MSaux(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_ADO_MSaux"};
   
   if (!ado) return(NULL);
   switch (ado->do_type) {
      case MASK_type:
         return((SUMA_MASK_SAUX *)SUMA_ADO_Saux(ado));
         break;
      default:
         return(NULL);
   }
   return(NULL);
}

SUMA_SURF_SAUX *SUMA_ADO_SSaux(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_SURF_SAUX"};
   
   if (!ado) return(NULL);
   switch (ado->do_type) {
      case SO_type:
         return((SUMA_SURF_SAUX *)SUMA_ADO_Saux(ado));
         break;
      default:
         return(NULL);
   }
   return(NULL);
}

SUMA_VOL_SAUX *SUMA_ADO_VSaux(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_ADO_VSaux"};
   
   if (!ado) return(NULL);
   switch (ado->do_type) {
      case VO_type:
         return((SUMA_VOL_SAUX *)SUMA_ADO_Saux(ado));
         break;
      default:
         return(NULL);
   }
   return(NULL);
}

void *SUMA_ADO_Saux(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_ADO_Saux"};
   
   if (!ado) return(NULL);
   switch (ado->do_type) {
      case SO_type: 
         return((void *)SDO_SSAUX((SUMA_SurfaceObject *)ado));
         break;
      case CDOM_type:
         return((void *)CDO_CSAUX((SUMA_CIFTI_DO *)ado));
         break;
      case GDSET_type:
         return((void *)SDSET_GSAUX((SUMA_DSET *)ado));
         break;
      case GRAPH_LINK_type: {
         SUMA_DSET *dset = SUMA_find_GLDO_Dset((SUMA_GraphLinkDO *)ado);
         return((void *)SUMA_ADO_Saux((SUMA_ALL_DO *)dset));
         break; }
      case TRACT_type: {
         return((void *)TDO_TSAUX((SUMA_TractDO *)ado));
         break; }
      case MASK_type: {
         return((void *)MDO_MSAUX((SUMA_MaskDO *)ado));
         break; }
      case VO_type: {
         return((void *)VDO_VSAUX((SUMA_VolumeObject *)ado));
         break; }
      default:
         return(NULL);
   }
   return(NULL);
}

SUMA_DSET *SUMA_ADO_Dset(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_ADO_Dset"};
   
   if (!ado) return(NULL);
   switch (ado->do_type) {
      case SO_type: 
         return(NULL);
         break;
      case CDOM_type:
         SUMA_S_Note("Decide what should be done here. A CDOM is created from a "
                     "certain CIFTI dataset that one could return. However it "
                     "is envisioned that multiple CIFTI datasets can share the "
                     "same CIFTI domain so then which dset to return in that "
                     "instance. For now, let us return NULL");
         return(NULL);
         break; 
      case ANY_DSET_type:
      case MD_DSET_type:
      case GDSET_type:
         return((SUMA_DSET *)ado);
         break;
      case GRAPH_LINK_type: {
         return(SUMA_find_GLDO_Dset((SUMA_GraphLinkDO *)ado));
         break; }
      default:
         return(NULL);
   }
   return(NULL);
}

/*!
   How many and which DOs are anatomically correct ?
*/
int SUMA_Anatomical_DOs(SUMA_DO *dov, int N_dov, int *rdov)
{
   static char FuncName[]={"SUMA_Anatomical_DOs"};
   SUMA_ALL_DO *ado=NULL;
   int ii, N=0;
   
   if (!dov) {
      dov = SUMAg_DOv;
      N_dov = SUMAg_N_DOv;
   }
   for (ii=0; ii<N_dov; ++ii) {
      ado = (SUMA_ALL_DO *)dov[ii].OP;
      if (SUMA_ADO_is_AnatCorrect(ado)) {
         ++N;
         if (rdov) rdov[N-1] = ii;
      }
   }
   
   return(N);
   
}
