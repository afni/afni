#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <math.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <GL/GLwDrawA.h>  /* Motif OpenGL drawing area. */

#include "SUMA_suma.h"

extern SUMA_SurfaceViewer *SUMAg_cSV;
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;

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
handleRedisplay(XtPointer closure)
{
	display(SUMAg_cSV, SUMAg_DOv);
  SUMAg_cSV->X->REDISPLAYPENDING = 0;
  return True;
}

void
postRedisplay(void)
{
	static XtPointer elvis;
  if(!SUMAg_cSV->X->REDISPLAYPENDING) {
    /*SUMAg_cSV->X->REDISPLAYID = XtAppAddWorkProc(SUMAg_cSV->X->APP, handleRedisplay, 0);*/
	 SUMA_register_workproc( handleRedisplay , elvis );
    SUMAg_cSV->X->REDISPLAYPENDING = 1;
  }
}


void display(SUMA_SurfaceViewer *csv, SUMA_DO *dov)
{	
	int i;
   GLfloat rotationMatrix[4][4];
	build_rotmatrix(rotationMatrix, csv->GVS[csv->StdView].currentQuat);
	 
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); /* clear the Color Buffer and the depth buffer */
	
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluPerspective((GLdouble)csv->FOV[csv->iState], csv->Aspect, SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); /*lower angle is larger zoom,*/

	/* cycle through csv->ShowDO and display those things that have a fixed CoordType*/
	i = 0;
	while (i < csv->N_DO) {
		if (dov[csv->ShowDO[i]].CoordType == SUMA_SCREEN) {
			switch (dov[csv->ShowDO[i]].ObjectType) {
				case SO_type:
					break;
				case AO_type:
					if (csv->ShowEyeAxis){
						if (!SUMA_CreateAxis ((SUMA_Axis*)dov[csv->ShowDO[i]].OP)) {
							fprintf(stderr,"display error: Could not display EYE AXIS\n");
						}
					}
					break;
				case GO_type:
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
	i = 0;
	while (i < csv->N_DO) {
		if (dov[csv->ShowDO[i]].CoordType == SUMA_LOCAL) {
			switch (dov[csv->ShowDO[i]].ObjectType) {
				case SO_type:
					CreateMesh((SUMA_SurfaceObject *)dov[csv->ShowDO[i]].OP); /* create the surface */
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

   if (SUMAg_cSV->X->DOUBLEBUFFER)
    glXSwapBuffers(SUMAg_cSV->X->DPY, XtWindow(SUMAg_cSV->X->GLXAREA));
  else
    glFlush();

  /* Avoid indirect rendering latency from queuing. */
  if (!glXIsDirect(SUMAg_cSV->X->DPY, SUMAg_cSV->X->GLXCONTEXT))
    glFinish();

}

void
graphicsInit(Widget w, XtPointer clientData, XtPointer call)
{
	
	XVisualInfo *SUMAg_cVISINFO;

	/* Create OpenGL rendering context. */
	SUMAg_cSV->X->Wd = w; /* store the window widget */
	XtVaGetValues(w, GLwNvisualInfo, &SUMAg_cVISINFO, NULL);
	SUMAg_cSV->X->GLXCONTEXT = glXCreateContext(XtDisplay(w), SUMAg_cVISINFO,
	 0,                  /* No sharing. */
	 True);              /* Direct rendering if possible. */

	/* Setup OpenGL state. */
	glXMakeCurrent(XtDisplay(w), XtWindow(w), SUMAg_cSV->X->GLXCONTEXT);
	
	/* call context_Init to setup colors and lighting */	
	SUMA_context_Init();
}

void 
SUMA_context_Init(void)
{
	GLfloat mat_specular[] = { SUMA_MAT_SPECULAR_INIT};
   GLfloat mat_shininess[] = { SUMA_MAT_SHININESS_INIT };
	GLfloat mat_ambient[] = { SUMA_MAT_AMBIENT_INIT};
	GLfloat mat_diffuse[] = { SUMA_MAT_DIFFUSE_INIT };
   GLfloat mat_emission[] = { SUMA_MAT_EMISSION_INIT  };
	
	GLfloat light0_color[] = { SUMA_LIGHT0_COLOR_INIT};
   /*GLfloat green_light[] = { 0.0, 1.0, 0.0, 1.0};*/
	
	GLfloat lmodel_ambient[] = {SUMA_LMODEL_AMBIENT};

	glClearColor (SUMA_CLEAR_COLOR);
   glShadeModel (GL_SMOOTH);

	switch (SUMAg_cSV->PolyMode) {
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
	glLightfv(GL_LIGHT0, GL_POSITION, SUMAg_cSV->light0_position);
   glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_color);
	glLightfv(GL_LIGHT0, GL_SPECULAR, light0_color);

	/*glLightfv(GL_LIGHT1, GL_POSITION, SUMAg_cSV->light1_position);
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
	if (SUMAg_cSV->BF_Cull) {
		glCullFace (GL_BACK);
   	glEnable (GL_CULL_FACE);
	}
	
   /*setup the view point and then setup the lights. Those lights will remain in place regardless of the rotations/translations
	done on the surface */
	glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
	gluLookAt (SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewFrom[0], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewFrom[1], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewFrom[2], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCenter[0], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCenter[1], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCenter[2], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCamUp[0], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCamUp[1], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCamUp[2]);

	/*glLightfv(GL_LIGHT0, GL_POSITION, SUMAg_cSV->light0_position);*/
   /*glLightfv(GL_LIGHT1, GL_POSITION, SUMAg_cSV->light1_position);*/


}

	
void
resize(Widget w,
  XtPointer clientData, XtPointer call)
{
  GLwDrawingAreaCallbackStruct *callData;

/*	fprintf(stdout, "Resizn'...\n");*/
  callData = (GLwDrawingAreaCallbackStruct *) call;
  glXMakeCurrent(XtDisplay(w), XtWindow(w), SUMAg_cSV->X->GLXCONTEXT);
  glXWaitX();
  SUMAg_cSV->X->WIDTH = callData->width;
  SUMAg_cSV->X->HEIGHT = callData->height;
  glViewport(0, 0, callData->width, callData->height);
	
	glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   gluLookAt (SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewFrom[0], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewFrom[1], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewFrom[2], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCenter[0], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCenter[1], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCenter[2], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCamUp[0], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCamUp[1], SUMAg_cSV->GVS[SUMAg_cSV->StdView].ViewCamUp[2]);
	SUMAg_cSV->Aspect = (GLfloat) callData->width/(GLfloat) callData->height;
	SUMAg_cSV->WindWidth = callData->width; SUMAg_cSV->WindHeight = callData->height;

}


void
expose(Widget w,
  XtPointer clientData, XtPointer call)
{
  /*fprintf(stdout,"Ahhhhhhhh, exposed! You Pervert!\n");*/
  /*glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);*/ /* No need for that, done in display */
	postRedisplay();

}

void
SUMA_mapStateChanged(Widget w, XtPointer clientData,
  XEvent * event, Boolean * cont)
{
  /*fprintf(stdout, "widget window being mapped/unmapped\n");*/
  switch (event->type) {
  case MapNotify:
    if (SUMAg_cSV->X->MOMENTUMID)
      SUMAg_cSV->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_cSV->X->APP, 1, momentum, 0);
    break;
  case UnmapNotify:
    if (SUMAg_cSV->X->MOMENTUMID)
      XtRemoveTimeOut(SUMAg_cSV->X->MOMENTUMID);
    break;
  }
}

SUMA_Boolean SUMA_X_SurfaceViewer_Create (SUMA_SurfaceViewer *csv, int argc,char *argv[])
{
  /* Step 1. */
  csv->X->TOPLEVEL = XtAppInitialize(&csv->X->APP, "SUMA", NULL, 0, &argc, argv,
    fallbackResources, NULL, 0);

  /* Step 2. */
  XtAddEventHandler(csv->X->TOPLEVEL, StructureNotifyMask,
    False, SUMA_mapStateChanged, NULL);
  XtAddEventHandler(csv->X->TOPLEVEL, EnterWindowMask,
    False, SUMA_SetcSV, NULL);
 	
  /* Step 3. */
  csv->X->DPY = XtDisplay(csv->X->TOPLEVEL);
  /*fprintf(stdout, "trying for cool double buffer visual\n");*/
  csv->X->VISINFO = glXChooseVisual(csv->X->DPY, DefaultScreen(csv->X->DPY), dblBuf);
  if (csv->X->VISINFO == NULL) {
  	fprintf(stdout, "trying lame single buffer visual\n");
    XtAppWarning(csv->X->APP, "trying lame single buffer visual");
	 csv->X->VISINFO = glXChooseVisual(csv->X->DPY, DefaultScreen(csv->X->DPY), snglBuf);
    if (csv->X->VISINFO == NULL) {
      XtAppError(csv->X->APP, "no good visual");
		return (NOPE);
		}
    csv->X->DOUBLEBUFFER = False;
  }
  
  /* Step 4. */
  csv->X->FORM = XmCreateForm(csv->X->TOPLEVEL, "form", NULL, 0);
  XtManageChild(csv->X->FORM);
  csv->X->FRAME = XmCreateFrame(csv->X->FORM, "frame", NULL, 0);
  XtVaSetValues(csv->X->FRAME,
    XmNbottomAttachment, XmATTACH_FORM,
    XmNtopAttachment, XmATTACH_FORM,
    XmNleftAttachment, XmATTACH_FORM,
    XmNrightAttachment, XmATTACH_FORM,
    NULL);
  XtManageChild(csv->X->FRAME);

  /* Step 5. */
  csv->X->CMAP = SUMA_getShareableColormap(csv);

  /* Step 6. */
  /* glwMDrawingAreaWidgetClass requires libMesaGLwM.a */
  /* glwDrawingAreaWidgetClass requires libMesaGLw.a */
  csv->X->GLXAREA = XtVaCreateManagedWidget("glxarea",
    glwDrawingAreaWidgetClass, csv->X->FRAME,
    GLwNvisualInfo, csv->X->VISINFO,
    XtNcolormap, csv->X->CMAP,
    NULL);

  /* Step 7. */
  XtAddCallback(csv->X->GLXAREA, GLwNginitCallback, graphicsInit, NULL);
  XtAddCallback(csv->X->GLXAREA, GLwNexposeCallback, expose, NULL);
  XtAddCallback(csv->X->GLXAREA, GLwNresizeCallback, resize, NULL);
  XtAddCallback(csv->X->GLXAREA, GLwNinputCallback, input, NULL);

  /* Step 8. */
  XtRealizeWidget(csv->X->TOPLEVEL);

return (YUP);
}

Colormap
SUMA_getShareableColormap(SUMA_SurfaceViewer *csv)
{
  Status status;
  XStandardColormap *standardCmaps;
  Colormap cmap;
  int i, numCmaps;
	XVisualInfo * vi;
	
	vi = csv->X->VISINFO;
	
  /* Be lazy; using DirectColor too involved for this example. */
  if (vi->class != TrueColor)
    XtAppError(csv->X->APP, "no support for non-TrueColor visual");
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
          return cmap;
        }
  }
  cmap = XCreateColormap(csv->X->DPY, RootWindow(csv->X->DPY, vi->screen), vi->visual, AllocNone);
  return cmap;
}

void SUMA_SetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont)
{
	/*fprintf(stdout,"\nEntering Window\n");*/
	return;
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

  pixels = SUMA_grabPixels(inColor, width, height);
  if (pixels == NULL)
    return 1;
  if (inColor)
    components = 3;     /* Red, green, blue. */
  else
    components = 1;     /* Luminance. */

  fp = fopen(filename, "w");
  if (fp == NULL) {
    return 2;
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
  free(pixels);
  fclose(fp);
  return 0;
}

GLvoid *
SUMA_grabPixels(int inColor, unsigned int width, unsigned int height)
{
  GLvoid *buffer;
  GLint swapbytes, lsbfirst, rowlength;
  GLint skiprows, skippixels, alignment;
  GLenum format;
  unsigned int size;

  if (inColor) {
    format = GL_RGB;
    size = width * height * 3;
  } else {
    format = GL_LUMINANCE;
    size = width * height * 1;
  }

  buffer = (GLvoid *) malloc(size);
  if (buffer == NULL)
    return NULL;

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
  return buffer;
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
	
  dpy = XOpenDisplay(NULL);
  if (dpy == NULL)
    fprintf(SUMA_STDERR,"Error %s: could not open display", FuncName);

  if (!glXQueryExtension(dpy, NULL, NULL))
    fprintf(SUMA_STDERR,"Error %s: X server has no OpenGL GLX extension", FuncName);

  /* find an OpenGL-capable RGB visual with depth buffer */
  #if 1  /* use screen rendering Xvisual */
  vi = glXChooseVisual(dpy, DefaultScreen(dpy), &configuration[1]);
  if (vi == NULL) {
  	fprintf(SUMA_STDERR,"%s: Trying to use useless double buffering configuration.\n", FuncName);
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
  glXMakeCurrent(dpy, glxpmap, cx);

  SUMA_context_Init();
  glViewport(0, 0, csv->X->WIDTH, csv->X->HEIGHT);
  display(csv, dov);

	glFinish (); /* make sure you wait until rendering is over */
	
  /* find out the next best name and write it*/
  {
  		char tmpprfx[100], *padprfx, padname[100];
		int cntindx=0;
		SUMA_Boolean OKname = NOPE;
		while (!OKname) {
			sprintf (tmpprfx, "%d", cntindx);
			padprfx = SUMA_pad_str (tmpprfx, '0', 4, 0);
			sprintf(padname,"suma.rgb%s.eps", padprfx);
			if (SUMA_filexists(padname)) {
				++cntindx;
			} else { OKname = YUP; }
			
			free (padprfx);
		}
	  
	  fprintf (SUMA_STDOUT,"%s: Writing image to %s ...", FuncName, padname);
	  SUMA_generateEPS(padname, /* color */ 1, csv->X->WIDTH, csv->X->HEIGHT);
	  fprintf (SUMA_STDOUT,"Done.\n");
  }
	
	/* render to original context */
	glXMakeCurrent(XtDisplay(csv->X->Wd), XtWindow(csv->X->Wd),  csv->X->GLXCONTEXT); 
	
	return (YUP);
}

/* ------------------------------------------------------------------------------------------------------------*/
