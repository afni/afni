/*! Functions to create Displayable Objects */
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
#include <GL/GLwMDrawA.h>  /* Motif OpenGL drawing area. */

#include "SUMA_suma.h"

/*! Allocate for a axis object */
SUMA_Axis* SUMA_Alloc_Axis (const char *Name)
{	
	static char FuncName[]={"SUMA_Alloc_Axis"};
	SUMA_Axis* Ax;
	Ax = malloc (sizeof (SUMA_Axis));
	if (Ax == NULL) {
		fprintf(stderr,"SUMA_Alloc_Axis Error: Failed to allocate Ax\n");
		return (NULL);
	}
	
	/* setup some default values */
	Ax->XaxisColor[0] = 1.0;
	Ax->XaxisColor[1] = 0.0;
	Ax->XaxisColor[2] = 0.0;
	Ax->XaxisColor[3] = 0.0;
	
	Ax->YaxisColor[0] = 0.0;
	Ax->YaxisColor[1] = 1.0;
	Ax->YaxisColor[2] = 0.0;
	Ax->YaxisColor[3] = 0.0;
	
	Ax->ZaxisColor[0] = 0.0;
	Ax->ZaxisColor[1] = 0.0;
	Ax->ZaxisColor[2] = 1.0;
	Ax->ZaxisColor[3] = 0.0;
	
	Ax->LineWidth = 1.0;
	Ax->Stipple = SUMA_SOLID_LINE;
	Ax->XYZspan[0] = Ax->XYZspan[1] = Ax->XYZspan[2] = 800;
	
	Ax->Center[0] = Ax->Center[1] = Ax->Center[2] = 0.0;
	
	if (Name != NULL) {
		if (strlen(Name) > SUMA_MAX_LABEL_LENGTH-1) {
			fprintf(SUMA_STDERR, "Error %s: Name too long (> %d).\n",\
				FuncName, SUMA_MAX_LABEL_LENGTH);
			Ax->Name = NULL;
			Ax->idcode_str = NULL;
		} else {
			Ax->Name = (char *)calloc (strlen(Name)+1, sizeof(char));
			Ax->idcode_str = (char *)calloc (SUMA_IDCODE_LENGTH, sizeof(char));
			if (Ax->Name == NULL) {
				fprintf(SUMA_STDERR,"Error %s: Failed to allocate for Ax->Name.\n", \
					FuncName);
			}
			sprintf(Ax->Name, "%s", Name);
			UNIQ_idcode_fill(Ax->idcode_str); 
		}
		
	}
	return (Ax);
}
void SUMA_Free_Axis (SUMA_Axis *Ax)
{
	if (Ax->Name != NULL) free (Ax->Name);
	if (Ax->idcode_str != NULL) free (Ax->idcode_str);
	if (Ax) free(Ax);
	return;
}

void SUMA_EyeAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *csv)
{
	Ax->Stipple = SUMA_DASHED_LINE;
	Ax->XYZspan[0] = Ax->XYZspan[1] = Ax->XYZspan[2] = 1000.0;
	Ax->Center[0] = csv->GVS[csv->StdView].ViewCenter[0];
	Ax->Center[1] = csv->GVS[csv->StdView].ViewCenter[1];
	Ax->Center[2] = csv->GVS[csv->StdView].ViewCenter[2];
	return;
}

void SUMA_MeshAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceObject *cso)
{
	Ax->Stipple = SUMA_SOLID_LINE;
	Ax->XYZspan[0]= Ax->XYZspan[1]= Ax->XYZspan[2]= 100.0;
	Ax->Center[0] = cso->Center[0];
	Ax->Center[1] = cso->Center[1];
	Ax->Center[2] = cso->Center[2];
	return;
}

SUMA_Boolean SUMA_CreateAxis (SUMA_Axis* Ax)
{ static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
	glLineWidth(Ax->LineWidth);
	switch (Ax->Stipple) {
		case SUMA_DASHED_LINE:
			glEnable(GL_LINE_STIPPLE);
			glLineStipple (1, 0x00FF); /* dashed, see OpenGL Prog guide, page 55 */
			break;
		case SUMA_SOLID_LINE:
			break;
		default:
			fprintf(stderr,"Error SUMA_CreateAxis: Unrecognized Stipple option\n");
			return(NOPE);
	}
	glBegin(GL_LINES);
   glMaterialfv(GL_FRONT, GL_EMISSION, Ax->XaxisColor); /*turn on emissivity for axis*/
	glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); /* turn off ambient and diffuse components */
	glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
	
	glVertex3f(-Ax->XYZspan[0]+Ax->Center[0], Ax->Center[1], Ax->Center[2]);
	glVertex3f(Ax->XYZspan[0]+Ax->Center[0], Ax->Center[1], Ax->Center[2]); 
	
   glMaterialfv(GL_FRONT, GL_EMISSION, Ax->YaxisColor); /*turn on emissivity for axis*/
	glVertex3f(Ax->Center[0], -Ax->XYZspan[1]+Ax->Center[1], Ax->Center[2]);
	glVertex3f(Ax->Center[0], +Ax->XYZspan[1]+Ax->Center[1], Ax->Center[2]); 
	
   glMaterialfv(GL_FRONT, GL_EMISSION, Ax->ZaxisColor); /*turn on emissivity for axis*/
	glVertex3f(Ax->Center[0], Ax->Center[1], -Ax->XYZspan[2]+Ax->Center[2]);
	glVertex3f(Ax->Center[0], Ax->Center[1], Ax->XYZspan[2]+Ax->Center[2]); 
	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, NoColor); /*turn off emissivity for axis*/

	glEnd();
	switch (Ax->Stipple) {
		case SUMA_DASHED_LINE:
			glDisable(GL_LINE_STIPPLE);
			break;
		case SUMA_SOLID_LINE:
			break;
	}
	return (YUP);
}

/*! Create the cross hair */
SUMA_Boolean SUMA_CreateCrossHair (SUMA_CrossHair* Ch)
{
	static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
	glLineWidth(Ch->LineWidth);
	/*fprintf(SUMA_STDOUT, "Center: %f, %f, %f. Gap %f, Radius: %f\n",\
		Ch->c[0], Ch->c[2], Ch->c[2], Ch->g, Ch->r);*/
	glBegin(GL_LINES);
		glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); /* turn off ambient and diffuse components */
		glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
		if (Ch->g) { /* gap */
   		glMaterialfv(GL_FRONT, GL_EMISSION, Ch->XaxisColor); /*turn on emissivity for axis*/
			glVertex3f(Ch->c[0] - Ch->r, Ch->c[1], Ch->c[2]);
			glVertex3f(Ch->c[0] - Ch->g, Ch->c[1], Ch->c[2]);
			glVertex3f(Ch->c[0] + Ch->r, Ch->c[1], Ch->c[2]);
			glVertex3f(Ch->c[0] + Ch->g, Ch->c[1], Ch->c[2]);

			glMaterialfv(GL_FRONT, GL_EMISSION, Ch->YaxisColor); /*turn on emissivity for axis*/
			glVertex3f(Ch->c[0], Ch->c[1] - Ch->r, Ch->c[2]);
			glVertex3f(Ch->c[0], Ch->c[1] - Ch->g, Ch->c[2]);
			glVertex3f(Ch->c[0], Ch->c[1] + Ch->r, Ch->c[2]);
			glVertex3f(Ch->c[0], Ch->c[1] + Ch->g, Ch->c[2]);

			glMaterialfv(GL_FRONT, GL_EMISSION, Ch->ZaxisColor); /*turn on emissivity for axis*/
			glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - Ch->r);
			glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - Ch->g);
			glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + Ch->r);
			glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + Ch->g);

		}/*gap */ else {/*no gap */
			glMaterialfv(GL_FRONT, GL_EMISSION, Ch->XaxisColor); /*turn on emissivity for axis*/
			glVertex3f(Ch->c[0] - Ch->r, Ch->c[1], Ch->c[2]);
			glVertex3f(Ch->c[0] + Ch->r, Ch->c[1], Ch->c[2]);
			
			glMaterialfv(GL_FRONT, GL_EMISSION, Ch->YaxisColor); /*turn on emissivity for axis*/
			glVertex3f(Ch->c[0], Ch->c[1] - Ch->r, Ch->c[2]);
			glVertex3f(Ch->c[0], Ch->c[1] + Ch->r, Ch->c[2]);

			glMaterialfv(GL_FRONT, GL_EMISSION, Ch->ZaxisColor); /*turn on emissivity for axis*/
			glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - Ch->r);
			glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + Ch->r);
		}
		glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity for axis*/

	glEnd();  
	
	if (Ch->ShowSphere) {
		/*fprintf(SUMA_STDOUT, "SHOWING SPHERE\n");*/
		glMaterialfv(GL_FRONT, GL_EMISSION, Ch->sphcol); /*turn on emissivity for sphere */
		glTranslatef (Ch->c[0], Ch->c[1],Ch->c[2]);
		gluSphere(Ch->sphobj, Ch->sphrad, Ch->slices, Ch->stacks);
		glTranslatef (-Ch->c[0], -Ch->c[1],-Ch->c[2]);
		glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity for axis*/
	}
	
	return (YUP);
}

/* Allocate for a CrossHair object */
SUMA_CrossHair* SUMA_Alloc_CrossHair (void)
{	
	SUMA_CrossHair* Ch;
	Ch = malloc (sizeof (SUMA_CrossHair));
	if (Ch == NULL) {
		fprintf(stderr,"SUMA_Alloc_CrossHair Error: Failed to allocate Ch\n");
		return (NULL);
	}
	
	/* setup some default values */
	Ch->XaxisColor[0] = 1.0;
	Ch->XaxisColor[1] = 0.0;
	Ch->XaxisColor[2] = 0.0;
	Ch->XaxisColor[3] = 0.0;
	
	Ch->YaxisColor[0] = 0.0;
	Ch->YaxisColor[1] = 1.0;
	Ch->YaxisColor[2] = 0.0;
	Ch->YaxisColor[3] = 0.0;
	
	Ch->ZaxisColor[0] = 0.0;
	Ch->ZaxisColor[1] = 0.0;
	Ch->ZaxisColor[2] = 1.0;
	Ch->ZaxisColor[3] = 0.0;
	
	Ch->LineWidth = SUMA_CROSS_HAIR_LINE_WIDTH;
	Ch->Stipple = SUMA_SOLID_LINE;
	Ch->c[0] = Ch->c[1] = Ch->c[2] = 0.0;
	
	Ch->g = SUMA_CROSS_HAIR_GAP; 
	Ch->r = SUMA_CROSS_HAIR_RADIUS; 
	
	/* create the ball object*/
	Ch->ShowSphere	= YUP;
	Ch->sphobj = gluNewQuadric();
	/* for wire frame  use GLU_LINE with GLU_NONE */
	/* for solid, use GLU_FILL and GLU_SMOOTH */
	#ifdef SUMA_SOLID_LOCAL
		gluQuadricDrawStyle (Ch->sphobj, GLU_FILL); 
		gluQuadricNormals (Ch->sphobj , GLU_SMOOTH);
	#else
		gluQuadricDrawStyle (Ch->sphobj, GLU_LINE);
		gluQuadricNormals (Ch->sphobj , GLU_NONE);
	#endif
	
	Ch->sphcol[0] = 1.0; Ch->sphcol[1] = 1.0; Ch->sphcol[2] = 0.0;
	Ch->sphrad = SUMA_CROSS_HAIR_SPHERE_RADIUS;
	Ch->slices = 10;
	Ch->stacks = 10;
	
	Ch->SurfaceID = -1;
	Ch->NodeID = -1;
	return (Ch);
}

/*! Free a CrossHair object */
void SUMA_Free_CrossHair (SUMA_CrossHair *Ch)
{
	if (Ch->sphobj) gluDeleteQuadric(Ch->sphobj);
	if (Ch) free(Ch);
	return;
}


/* Allocate for a SphereMarker object */
SUMA_SphereMarker* SUMA_Alloc_SphereMarker (void)
{	
	SUMA_SphereMarker* SM;
	SM = malloc (sizeof (SUMA_SphereMarker));
	if (SM == NULL) {
		fprintf(stderr,"SUMA_Alloc_SphereMarker Error: Failed to allocate SM\n");
		return (NULL);
	}
	
	/* create the ball object*/
	SM->sphobj = gluNewQuadric();
	/* for wire frame  use GLU_LINE with GLU_NONE */
	/* for solid, use GLU_FILL and GLU_SMOOTH */
	#ifdef SUMA_SOLID_LOCAL
		gluQuadricDrawStyle (SM->sphobj, GLU_FILL); 
		gluQuadricNormals (SM->sphobj , GLU_SMOOTH);
	#else
		gluQuadricDrawStyle (SM->sphobj, GLU_LINE);
		gluQuadricNormals (SM->sphobj , GLU_NONE);
	#endif
	SM->sphcol[0] = 0.50; SM->sphcol[1] = 0.5; SM->sphcol[2] = 1.0;
	SM->sphrad = SUMA_SELECTED_NODE_SPHERE_RADIUS;
	SM->slices = 10;
	SM->stacks = 10;
	SM->c[0] = SM->c[1] = SM->c[2] = 0.0; 
	
	return (SM);
}

/*! Free a SphereMarker object */
void SUMA_Free_SphereMarker (SUMA_SphereMarker *SM)
{
	if (SM->sphobj) gluDeleteQuadric(SM->sphobj);
	if (SM) free(SM);
	return;
}

/*! Create the highlighted faceset  marker */
SUMA_Boolean SUMA_CreateFaceSetMarker (SUMA_FaceSetMarker* FM)
{	static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0}, dx, dy, dz;
	dx = SUMA_SELECTED_FACESET_OFFSET_FACTOR * FM->NormVect[0];
	dy = SUMA_SELECTED_FACESET_OFFSET_FACTOR * FM->NormVect[1];
	dy = SUMA_SELECTED_FACESET_OFFSET_FACTOR * FM->NormVect[2];
	 
	glLineWidth(FM->LineWidth);
	glDisable(GL_LINE_STIPPLE);

	glMaterialfv(GL_FRONT, GL_EMISSION, FM->LineCol); /*turn on emissivity for triangle*/
	glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); /* turn off ambient and diffuse components */
	glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);

	glBegin(GL_LINE_LOOP);
		glVertex3f(FM->n0[0]+dx, FM->n0[1]+dy, FM->n0[2]+dz);
		glVertex3f(FM->n1[0]+dx, FM->n1[1]+dy, FM->n1[2]+dz);
		glVertex3f(FM->n2[0]+dx, FM->n2[1]+dy, FM->n2[2]+dz);
	glEnd();
	glBegin(GL_LINE_LOOP);
		glVertex3f(FM->n0[0]-dx, FM->n0[1]-dy, FM->n0[2]-dz);
		glVertex3f(FM->n1[0]-dx, FM->n1[1]-dy, FM->n1[2]-dz);
		glVertex3f(FM->n2[0]-dx, FM->n2[1]-dy, FM->n2[2]-dz);
	glEnd();
	glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);
	return (YUP);
}	

/* Allocate for a faceset mrker */
SUMA_FaceSetMarker* SUMA_Alloc_FaceSetMarker (void)
{
	SUMA_FaceSetMarker* FM;
	FM = malloc (sizeof (SUMA_FaceSetMarker));
	if (FM == NULL) {
		fprintf(stderr,"SUMA_Alloc_FaceSetMarker Error: Failed to allocate FM\n");
		return (NULL);
	}
	 
	/* setup some default values */
	FM->LineWidth = SUMA_SELECTED_FACESET_LINE_WIDTH;
	FM->LineCol[0] = FM->LineCol[1] = FM->LineCol[2] = SUMA_SELECTED_FACESET_LINE_INTENSITY;
	
	return (FM);
}
/*! Free a FaceSetMarker object */
void SUMA_Free_FaceSetMarker (SUMA_FaceSetMarker* FM)
{
	if (FM) free(FM);
	return;
}

/*! Create a tesselated mesh */
void CreateMesh(SUMA_SurfaceObject *SurfObj)
{  static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
	int i;
	switch (DRAW_METHOD) { 
   	case STRAIGHT:
			switch (RENDER_METHOD) {
				case TRIANGLES:
					glBegin (GL_TRIANGLES);
					break;
				case POINTS:
					glPointSize(4.0); /* keep outside of glBegin */
					glBegin (GL_POINTS);
					break;
				} /* switch RENDER_METHOD */
      		glColor4f(NODE_COLOR_R, NODE_COLOR_G, NODE_COLOR_B, SUMA_NODE_ALPHA);
      		for (i=0; i < SurfObj->N_FaceSet; i++)
				{	
					glNormal3fv(&SurfObj->NodeNormList[SurfObj->FaceSetList[i][0]][0]);
					glVertex3fv(&SurfObj->NodeList[SurfObj->FaceSetList[i][0]][0]); /* glVertex3f(0.1, 0.9, 0.0); */
      			glNormal3fv(&SurfObj->NodeNormList[SurfObj->FaceSetList[i][1]][0]);
					glVertex3fv(&SurfObj->NodeList[SurfObj->FaceSetList[i][1]][0]);/* glVertex3f(0.1, 0.1, 0.0); */
					glNormal3fv(&SurfObj->NodeNormList[SurfObj->FaceSetList[i][2]][0]);
      			glVertex3fv(&SurfObj->NodeList[SurfObj->FaceSetList[i][2]][0]);/* glVertex3f(0.7, 0.5, 0.0); */
				}
   		glEnd();
			break;
		
		case ARRAY:
			/* Draw Axis */
			
			if (SurfObj->MeshAxis && SurfObj->ShowMeshAxis)	{
				if (!SUMA_CreateAxis (SurfObj->MeshAxis)) {
					fprintf(stderr,"Error SUMA_CreateAxis: Unrecognized Stipple option\n");
				}
			}
			/* Draw Selected Node Highlight */
			if (SurfObj->ShowSelectedNode && SurfObj->SelectedNode >= 0) {
				/*fprintf(SUMA_STDOUT,"Drawing Node Selection \n");*/
				glMaterialfv(GL_FRONT, GL_EMISSION, SurfObj->NodeMarker->sphcol); /*turn on emissivity for sphere */
				glTranslatef (SurfObj->NodeList[SurfObj->SelectedNode][0], SurfObj->NodeList[SurfObj->SelectedNode][1],SurfObj->NodeList[SurfObj->SelectedNode][2]);
				gluSphere(SurfObj->NodeMarker->sphobj, SurfObj->NodeMarker->sphrad, SurfObj->NodeMarker->slices, SurfObj->NodeMarker->stacks);
				glTranslatef (-SurfObj->NodeList[SurfObj->SelectedNode][0], -SurfObj->NodeList[SurfObj->SelectedNode][1],-SurfObj->NodeList[SurfObj->SelectedNode][2]);
				glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity for axis*/
			}
			
			/* Draw Selected FaceSet Highlight */
			if (SurfObj->ShowSelectedFaceSet && SurfObj->SelectedFaceSet >= 0) {
				/*fprintf(SUMA_STDOUT,"Drawing FaceSet Selection \n");				*/
				if (!SUMA_CreateFaceSetMarker (SurfObj->FaceSetMarker)) {
					fprintf(SUMA_STDERR,"Error CreateMesh: Failed in SUMA_CreateFaceSetMarker\b");
				}
			} 
			/* This allows each node to follow the color specified when it was drawn */ 
			glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE); 
			glEnable(GL_COLOR_MATERIAL);
			
			/*Now setup various pointers*/
   		glEnableClientState (GL_COLOR_ARRAY);
			glEnableClientState (GL_VERTEX_ARRAY);
			glEnableClientState (GL_NORMAL_ARRAY);
	  		glColorPointer (4, GL_FLOAT, 0, SurfObj->glar_ColorList);
   		glVertexPointer (3, GL_FLOAT, 0, SurfObj->glar_NodeList);
			glNormalPointer (GL_FLOAT, 0, SurfObj->glar_NodeNormList);
			/*fprintf(stdout, "Ready to draw Elements %d\n", SurfObj->N_FaceSet);*/
   		switch (RENDER_METHOD) {
				case TRIANGLES:
					glDrawElements (GL_TRIANGLES, (GLsizei)SurfObj->N_FaceSet*3, GL_UNSIGNED_INT, SurfObj->glar_FaceSetList);
					break;
				case POINTS:
					glPointSize(4.0); /* keep outside of glBegin */
					/* it is inefficient to draw points using the glar_FaceSetList because nodes are listed more 
					than once. You are better off creating an index vector into glar_NodeList to place all the points, just once*/ 
					glDrawElements (GL_POINTS, (GLsizei)SurfObj->N_FaceSet*3, GL_UNSIGNED_INT, SurfObj->glar_FaceSetList);
					break;
				} /* switch RENDER_METHOD */

   		/*fprintf(stdout, "Disabling clients\n");*/
   		glDisableClientState (GL_COLOR_ARRAY);	
			glDisableClientState (GL_VERTEX_ARRAY);
   		glDisableClientState (GL_NORMAL_ARRAY);	
			/*fprintf(stdout, "Out CreateMesh, ARRAY mode\n");*/
			
			glDisable(GL_COLOR_MATERIAL);
			
			break;

	} /* switch DRAW_METHOD */

} /* CreateMesh */
