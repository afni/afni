/*! Functions to create Displayable Objects */
#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_DO *SUMAg_DOv;
extern int SUMAg_N_DOv;

/*! Allocate for a axis object */
SUMA_Axis* SUMA_Alloc_Axis (const char *Name)
{   
   static char FuncName[]={"SUMA_Alloc_Axis"};
   SUMA_Axis* Ax;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   Ax = SUMA_malloc (sizeof (SUMA_Axis));
   if (Ax == NULL) {
      fprintf(stderr,"SUMA_Alloc_Axis Error: Failed to allocate Ax\n");
      SUMA_RETURN (Ax);
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
         Ax->Name = (char *)SUMA_calloc (strlen(Name)+1, sizeof(char));
         Ax->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));
         if (Ax->Name == NULL) {
            fprintf(SUMA_STDERR,"Error %s: Failed to allocate for Ax->Name.\n", \
               FuncName);
         }
         sprintf(Ax->Name, "%s", Name);
         UNIQ_idcode_fill(Ax->idcode_str); 
      }
      
   }
   SUMA_RETURN (Ax);
}
void SUMA_Free_Axis (SUMA_Axis *Ax)
{
   static char FuncName[]={"SUMA_Free_Axis"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (Ax->Name != NULL) SUMA_free(Ax->Name);
   if (Ax->idcode_str != NULL) SUMA_free(Ax->idcode_str);
   if (Ax) SUMA_free(Ax);
   SUMA_RETURNe;
}

void SUMA_EyeAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *csv)
{
   static char FuncName[]={"SUMA_EyeAxisStandard"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   Ax->Stipple = SUMA_DASHED_LINE;
   Ax->XYZspan[0] = Ax->XYZspan[1] = Ax->XYZspan[2] = 1000.0;
   Ax->Center[0] = csv->GVS[csv->StdView].ViewCenter[0];
   Ax->Center[1] = csv->GVS[csv->StdView].ViewCenter[1];
   Ax->Center[2] = csv->GVS[csv->StdView].ViewCenter[2];
   SUMA_RETURNe;
}

void SUMA_MeshAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceObject *cso)
{
   static char FuncName[]={"SUMA_EyeAxisStandard"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   Ax->Stipple = SUMA_SOLID_LINE;
   Ax->XYZspan[0]= Ax->XYZspan[1]= Ax->XYZspan[2]= 100.0;
   Ax->Center[0] = cso->Center[0];
   Ax->Center[1] = cso->Center[1];
   Ax->Center[2] = cso->Center[2];
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_CreateAxis (SUMA_Axis* Ax)
{ 
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   static char FuncName[]={"SUMA_CreateAxis"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
         SUMA_RETURN(NOPE);
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
   SUMA_RETURN (YUP);
}

/*! Create the ROIs for a particular surface */
SUMA_Boolean SUMA_Draw_SO_ROI (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_do)
{
   static char FuncName[]={"SUMA_Draw_SO_ROI"};
   GLfloat ROI_SphCol[] = {1.0, 0.0, 0.0, 1.0};
   GLfloat ROI_SphCol_frst[] = {1.0, 0.0, 0.0, 1.0};
   GLfloat ROI_FaceGroup[] = {0.8, 0.3, 1.0, 1.0 };
   GLfloat ROI_NodeGroup[] = {0.8, 0.3, 0.5, 1.0 };
   GLfloat ROI_EdgeGroup[] = {0.8, 0.8, 0.1, 1.0 };
   GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int i, id, ii, id1,id2, id3, EdgeIndex, FaceIndex, Node1, Node2, Node3;
   float dx, dy, dz = 0.0;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   SUMA_ROI *ROI = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   for (i=0; i < N_do; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case ROIdO_type:
            D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
            if (SUMA_isdROIrelated (D_ROI, SO)) { /* draw it */
               if (LocalHead) fprintf(SUMA_STDERR, "%s: Drawing Drawn ROI %s (Status %d)\n", FuncName, D_ROI->Label, D_ROI->DrawStatus);
               switch (D_ROI->DrawStatus) {
                  case SUMA_ROI_InCreation:
                     ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 0.0; ROI_SphCol_frst[2] = 0.0; ROI_SphCol_frst[3] = 1.0;     
                     ROI_SphCol[0] = 0.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 0.0; ROI_SphCol[3] = 1.0;     
                     break;   
                  case SUMA_ROI_Finished:
                     ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 1.0; ROI_SphCol_frst[2] = 0.0; ROI_SphCol_frst[3] = 1.0;     
                     ROI_SphCol[0] = 0.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 1.0; ROI_SphCol[3] = 1.0;  
                     break;   
                  case SUMA_ROI_InEdit:
                     ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 0.0; ROI_SphCol_frst[2] = 1.0; ROI_SphCol_frst[3] = 1.0;     
                     ROI_SphCol[0] = 1.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 0.0; ROI_SphCol[3] = 1.0;  
                     break;   
                  default:
                     ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 0.3; ROI_SphCol_frst[2] = 1.0; ROI_SphCol_frst[3] = 1.0;     
                     ROI_SphCol[0] = 1.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 0.0; ROI_SphCol[3] = 1.0;     
                     break;

               }


               /* draw 1st sphere */
               glMaterialfv(GL_FRONT, GL_EMISSION, ROI_SphCol_frst);
               id = 3 * D_ROI->CtrlNodev[0];
               glTranslatef (SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]);
               gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
               glTranslatef (-SO->NodeList[id], -SO->NodeList[id+1], -SO->NodeList[id+2]);

               glLineWidth(1);
               glMaterialfv(GL_FRONT, GL_EMISSION, ROI_SphCol);
               for (ii = 1; ii < D_ROI->N_CtrlNodev; ++ii) {
                  id = 3 * D_ROI->CtrlNodev[ii];
                  id2 = 3 * D_ROI->CtrlNodev[ii-1];

                  /* draw lines connecting spheres */
                  glBegin(GL_LINES);
                  glVertex3f(SO->NodeList[id2], SO->NodeList[id2+1], SO->NodeList[id2+2]);
                  glVertex3f(SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]); 
                  glEnd();

                  glTranslatef (SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]);
                  gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
                  glTranslatef (-SO->NodeList[id], -SO->NodeList[id+1], -SO->NodeList[id+2]);
               }

               /* close the loop if necessary */
               if (D_ROI->DrawStatus == SUMA_ROI_Finished) {
                 id = 3 * D_ROI->CtrlNodev[D_ROI->N_CtrlNodev - 1];
                 id2 = 3 * D_ROI->CtrlNodev[0];

                 /* draw lines connecting spheres */
                  glBegin(GL_LINES);
                  glVertex3f(SO->NodeList[id2], SO->NodeList[id2+1], SO->NodeList[id2+2]);
                  glVertex3f(SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]); 
                  glEnd(); 
               }
               glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);
            }
            break;
         case ROIO_type:
            ROI = (SUMA_ROI *)dov[i].OP;
            if (SUMA_isROIrelated (ROI, SO)) { /* draw it */
               if (LocalHead) fprintf(SUMA_STDERR, "%s: Drawing ROI %s \n", FuncName, ROI->Label);
               switch (ROI->Type) { /* ROI types */
                  case SUMA_ROI_EdgeGroup:
                     glMaterialfv(GL_FRONT, GL_EMISSION, ROI_EdgeGroup);
                     for (ii=0; ii < ROI->N_ElInd; ++ii) {
                        EdgeIndex = ROI->ElInd[ii];
                        Node1 = SO->EL->EL[EdgeIndex][0];
                        Node2 = SO->EL->EL[EdgeIndex][1];
                        id = 3 * Node1;
                        id2 = 3 * Node2;
                        
                        glLineWidth(3);
                        
                        glBegin(GL_LINES);
                        glVertex3f(SO->NodeList[id2], SO->NodeList[id2+1], SO->NodeList[id2+2]);
                        glVertex3f(SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]); 
                        glEnd();
                     }
                     glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);
                     break;
                  case SUMA_ROI_NodeGroup:
                     glMaterialfv(GL_FRONT, GL_EMISSION, ROI_NodeGroup);
                     for (ii=0; ii < ROI->N_ElInd; ++ii) {
                        id = 3 * ROI->ElInd[ii];
                        glTranslatef (SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]);
                        gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
                        glTranslatef (-SO->NodeList[id], -SO->NodeList[id+1], -SO->NodeList[id+2]);
                     }
                     glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);
                     break;
                  case SUMA_ROI_FaceGroup:   
                     glMaterialfv(GL_FRONT, GL_EMISSION, ROI_FaceGroup);
                     for (ii=0; ii < ROI->N_ElInd; ++ii) {
                           FaceIndex = ROI->ElInd[ii];
                           id = FaceIndex * 3;
                         
                           Node1 = SO->FaceSetList[id];
                           Node2 = SO->FaceSetList[id+1];
                           Node3 = SO->FaceSetList[id+2];
                           
                           id1 = 3 * Node1;
                           id2 = 3 * Node2;
                           id3 = 3 * Node3;
                           
                           glLineWidth(6);
                           
                           #if 0 /* no need for that one, most likely */
                              
                              dx = SUMA_SELECTED_FACESET_OFFSET_FACTOR * SO->FaceNormList[id];
                              dy = SUMA_SELECTED_FACESET_OFFSET_FACTOR * SO->FaceNormList[id+1];
                              dz = SUMA_SELECTED_FACESET_OFFSET_FACTOR * SO->FaceNormList[id+2];


                              glBegin(GL_LINE_LOOP);
                                 glVertex3f(SO->NodeList[id1]+dx, SO->NodeList[id1+1]+dy, SO->NodeList[id1+2]+dz);
                                 glVertex3f(SO->NodeList[id2]+dx, SO->NodeList[id2+1]+dy, SO->NodeList[id2+2]+dz);
                                 glVertex3f(SO->NodeList[id3]+dx, SO->NodeList[id3+1]+dy, SO->NodeList[id3+2]+dz);
                              glEnd();


                              glBegin(GL_LINE_LOOP);
                                 glVertex3f(SO->NodeList[id1]-dx, SO->NodeList[id1+1]-dy, SO->NodeList[id1+2]-dz);
                                 glVertex3f(SO->NodeList[id2]-dx, SO->NodeList[id2+1]-dy, SO->NodeList[id2+2]-dz);
                                 glVertex3f(SO->NodeList[id3]-dx, SO->NodeList[id3+1]-dy, SO->NodeList[id3+2]-dz);
                              glEnd();
                           #endif
                           
                           glBegin(GL_LINE_LOOP);
                              glVertex3f(SO->NodeList[id1], SO->NodeList[id1+1], SO->NodeList[id1+2]);
                              glVertex3f(SO->NodeList[id2], SO->NodeList[id2+1], SO->NodeList[id2+2]);
                              glVertex3f(SO->NodeList[id3], SO->NodeList[id3+1], SO->NodeList[id3+2]);
                           glEnd();

                     }
                     glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);
                     break;
                  default:
                     fprintf(SUMA_STDERR, "Error %s: Not ready to drawn this type of ROI.\n", FuncName);
                     break;
               } /* ROI types */
            } /* draw it */
            break;
         default:
            /* not an ROI */
            break;
      }/* case Object Type */
   }

   SUMA_RETURN (YUP);
}         

/*! Create the cross hair */
SUMA_Boolean SUMA_CreateCrossHair (SUMA_CrossHair* Ch)
{
   static char FuncName[]={"SUMA_CreateCrossHair"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   SUMA_RETURN (YUP);
}

/* Allocate for a CrossHair object */
SUMA_CrossHair* SUMA_Alloc_CrossHair (void)
{   
   static char FuncName[]={"SUMA_Alloc_CrossHair"};
   SUMA_CrossHair* Ch;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   Ch = SUMA_malloc (sizeof (SUMA_CrossHair));
   if (Ch == NULL) {
      fprintf(stderr,"SUMA_Alloc_CrossHair Error: Failed to allocate Ch\n");
      SUMA_RETURN (NULL);
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
   Ch->ShowSphere   = YUP;
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
   
   Ch->sphcol[0] = 1.0; Ch->sphcol[1] = 1.0; Ch->sphcol[2] = 0.0; Ch->sphcol[3] = 0.0;
   Ch->sphrad = SUMA_CROSS_HAIR_SPHERE_RADIUS;
   Ch->slices = 10;
   Ch->stacks = 10;
   
   Ch->SurfaceID = -1;
   Ch->NodeID = -1;
   SUMA_RETURN (Ch);
}

/*! Free a CrossHair object */
void SUMA_Free_CrossHair (SUMA_CrossHair *Ch)
{
   static char FuncName[]={"SUMA_Free_CrossHair"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (Ch->sphobj) gluDeleteQuadric(Ch->sphobj);
   if (Ch) SUMA_free(Ch);
   SUMA_RETURNe;
}


/* Allocate for a SphereMarker object */
SUMA_SphereMarker* SUMA_Alloc_SphereMarker (void)
{   
   static char FuncName[]={"SUMA_SphereMarker"};
   SUMA_SphereMarker* SM;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SM = SUMA_malloc (sizeof (SUMA_SphereMarker));
   if (SM == NULL) {
      fprintf(stderr,"SUMA_Alloc_SphereMarker Error: Failed to allocate SM\n");
      SUMA_RETURN (NULL);
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
   SM->sphcol[0] = 0.50; SM->sphcol[1] = 0.5; SM->sphcol[2] = 1.0; SM->sphcol[3] = 1.0;
   SM->sphrad = SUMA_SELECTED_NODE_SPHERE_RADIUS;
   SM->slices = 10;
   SM->stacks = 10;
   SM->c[0] = SM->c[1] = SM->c[2] = 0.0; 
   
   SUMA_RETURN (SM);
}

/*! Free a SphereMarker object */
void SUMA_Free_SphereMarker (SUMA_SphereMarker *SM)
{
   static char FuncName[]={"SUMA_Free_SphereMarker"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (SM->sphobj) gluDeleteQuadric(SM->sphobj);
   if (SM) SUMA_free(SM);
   SUMA_RETURNe;
}

/*! Create the highlighted faceset  marker */
SUMA_Boolean SUMA_CreateFaceSetMarker (SUMA_FaceSetMarker* FM)
{   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0}, dx, dy, dz;
   static char FuncName[]={"SUMA_CreateFaceSetMarker"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   dx = SUMA_SELECTED_FACESET_OFFSET_FACTOR * FM->NormVect[0];
   dy = SUMA_SELECTED_FACESET_OFFSET_FACTOR * FM->NormVect[1];
   dz = SUMA_SELECTED_FACESET_OFFSET_FACTOR * FM->NormVect[2];
    
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
   SUMA_RETURN (YUP);
}   

/* Allocate for a faceset mrker */
SUMA_FaceSetMarker* SUMA_Alloc_FaceSetMarker (void)
{
   SUMA_FaceSetMarker* FM;
   static char FuncName[]={"SUMA_Alloc_FaceSetMarker"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   FM = SUMA_malloc (sizeof (SUMA_FaceSetMarker));
   if (FM == NULL) {
      fprintf(stderr,"SUMA_Alloc_FaceSetMarker Error: Failed to allocate FM\n");
      SUMA_RETURN (NULL);
   }
    
   /* setup some default values */
   FM->LineWidth = SUMA_SELECTED_FACESET_LINE_WIDTH;
   FM->LineCol[0] = FM->LineCol[1] = FM->LineCol[2] = SUMA_SELECTED_FACESET_LINE_INTENSITY; FM->LineCol[3] = 1;
   
   SUMA_RETURN (FM);
}
/*! Free a FaceSetMarker object */
void SUMA_Free_FaceSetMarker (SUMA_FaceSetMarker* FM)
{
   static char FuncName[]={"SUMA_Free_FaceSetMarker"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (FM) SUMA_free(FM);
   SUMA_RETURNe;
}

/*! Create a tesselated mesh */
void SUMA_CreateMesh(SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *sv)
{  static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int i, ii, ND, id, ip, NP;
   static char FuncName[]={"SUMA_CreateMesh"};
   SUMA_DRAWN_ROI *DrawnROI = NULL;
   SUMA_Boolean LocalHead = YUP;
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ND = SurfObj->NodeDim;
   NP = SurfObj->FaceSetDim;
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
               ip = NP * i;
               id = ND * SurfObj->FaceSetList[ip];
               glNormal3fv(&SurfObj->NodeNormList[id]);
               glVertex3fv(&SurfObj->NodeList[id]); /* glVertex3f(0.1, 0.9, 0.0); */
               
               id = ND * SurfObj->FaceSetList[ip+1];
               glNormal3fv(&SurfObj->NodeNormList[id]);
               glVertex3fv(&SurfObj->NodeList[id]);/* glVertex3f(0.1, 0.1, 0.0); */
               
               id = ND * SurfObj->FaceSetList[ip+2];
               glNormal3fv(&SurfObj->NodeNormList[id]);
               glVertex3fv(&SurfObj->NodeList[id]);/* glVertex3f(0.7, 0.5, 0.0); */
            }
         glEnd();
         break;
      
      case ARRAY:
         if (!SUMA_Draw_SO_ROI (SurfObj, SUMAg_DOv, SUMAg_N_DOv)) {
            fprintf (SUMA_STDERR, "Error %s: Failed in drawing ROI objects.\n", FuncName);
         }
         
         /* Draw Axis */
         if (SurfObj->MeshAxis && SurfObj->ShowMeshAxis)   {
            if (!SUMA_CreateAxis (SurfObj->MeshAxis)) {
               fprintf(stderr,"Error SUMA_CreateAxis: Unrecognized Stipple option\n");
            }
         }
         /* Draw Selected Node Highlight */
         if (SurfObj->ShowSelectedNode && SurfObj->SelectedNode >= 0) {
            /*fprintf(SUMA_STDOUT,"Drawing Node Selection \n");*/
            id = ND * SurfObj->SelectedNode;
            glMaterialfv(GL_FRONT, GL_EMISSION, SurfObj->NodeMarker->sphcol); /*turn on emissidity for sphere */
            glTranslatef (SurfObj->NodeList[id], SurfObj->NodeList[id+1],SurfObj->NodeList[id+2]);
            gluSphere(SurfObj->NodeMarker->sphobj, SurfObj->NodeMarker->sphrad, SurfObj->NodeMarker->slices, SurfObj->NodeMarker->stacks);
            glTranslatef (-SurfObj->NodeList[id], -SurfObj->NodeList[id+1],-SurfObj->NodeList[id+2]);
            glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissidity for axis*/
         }
         
         /* Draw Selected FaceSet Highlight */
         if (SurfObj->ShowSelectedFaceSet && SurfObj->SelectedFaceSet >= 0) {
            /*fprintf(SUMA_STDOUT,"Drawing FaceSet Selection \n");            */
            if (!SUMA_CreateFaceSetMarker (SurfObj->FaceSetMarker)) {
               fprintf(SUMA_STDERR,"Error SUMA_CreateMesh: Failed in SUMA_CreateFaceSetMarker\b");
            }
         } 
         /* This allows each node to follow the color specified when it was drawn */ 
         glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE); 
         glEnable(GL_COLOR_MATERIAL);
         
         /*Now setup various pointers*/
         glEnableClientState (GL_COLOR_ARRAY);
         glEnableClientState (GL_VERTEX_ARRAY);
         glEnableClientState (GL_NORMAL_ARRAY);
           glColorPointer (4, GL_FLOAT, 0, SUMA_GetColorList (sv, SurfObj->idcode_str));
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
         /*fprintf(stdout, "Out SUMA_CreateMesh, ARRAY mode\n");*/
         
         glDisable(GL_COLOR_MATERIAL);
         
         break;

   } /* switch DRAW_METHOD */
   SUMA_RETURNe;
} /* SUMA_CreateMesh */

/*!**
File : SUMA_Load_Surface_Object.c
\author Ziad Saad
Date : Wed Jan 23 15:18:12 EST 2002
   
Purpose : 
   
   
   
Usage : 
    Ans = SUMA_Free_Surface_Object ( SO)
   
   
Input paramters : 
\param   SO (SUMA_SurfaceObject *) Surface Object pointer
   
Returns : 
\return  Ans (SUMA_Boolean) 

\sa SUMA_Load_Surface_Object        
***/
SUMA_Boolean SUMA_Free_Surface_Object (SUMA_SurfaceObject *SO)
{   
   static char FuncName[]={"SUMA_Free_Surface_Object"};
   int i;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /*fprintf (stdout, "freeing:\n");*/
   /* Start with the big ones and down*/
   /* From SUMA 1.2 and on, some glar_ pointers are copies of others and should not be freed */ 
   SO->glar_FaceSetList = NULL;
   SO->glar_NodeList = NULL;
   SO->glar_NodeNormList = NULL;
   SO->glar_FaceNormList = NULL;
   
   /*fprintf (stdout, "SO->NodeList... ");*/
   if (SO->NodeList)   SUMA_free(SO->NodeList);
   /*fprintf (stdout, "SO->NodeList... ");*/
   if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
   /*fprintf (stdout, "SO->FaceSetList... ");*/
   if (SO->NodeNormList) SUMA_free(SO->NodeNormList);
   /*fprintf (stdout, "SO->NodeNormList... ");*/
   if (SO->FaceNormList) SUMA_free(SO->FaceNormList);
   /*fprintf (stdout, "SO->FaceNormList... ");*/
   if (SO->Name_NodeParent) SUMA_free(SO->Name_NodeParent);
   /*fprintf (stdout, "SO->Name.FileName... ");*/
   if (SO->Name.FileName) SUMA_free(SO->Name.FileName);
   /*fprintf (stdout, "SO->Name.Path... ");*/
   if (SO->Name.Path) SUMA_free(SO->Name.Path);
   if (SO->MeshAxis) SUMA_Free_Axis (SO->MeshAxis);
   if (SO->MF) {
      if (!SUMA_Free_MemberFaceSets (SO->MF)) {
            fprintf(SUMA_STDERR,"Error SUMA_Free_Surface_Object : Failed to free SO->MF");
         }
   }
   if (SO->NodeMarker) SUMA_Free_SphereMarker (SO->NodeMarker);
   if (SO->FaceSetMarker) SUMA_Free_FaceSetMarker(SO->FaceSetMarker);
   /*fprintf (stdout, "SO... ");*/
   if (SO->idcode_str) SUMA_free(SO->idcode_str);
   if (SO->MapRef_idcode_str) SUMA_free(SO->MapRef_idcode_str);
   if (SO->Group) SUMA_free(SO->Group);
   if (SO->State) SUMA_free(SO->State);
   if (SO->PolyArea) SUMA_free(SO->PolyArea);
   if (SO->SC) {
      SUMA_Free_SURFACE_CURVATURE(SO->SC);
   }
   
   /* freeing Cx,  make sure that there are no links to Cx*/
   if (SO->Cx || SO->Cx_Inode) { /* there should be no case where only one of two is null but if such a case existed, you'll get notified below. */
      if (SUMA_ReleaseLink(SO->Cx_Inode)) { 
         /* some links are left, do not free memory */
      } else {
         if (SO->Cx) SUMA_free(SO->Cx);
         /* now free SO->Cx_Inode */
         SUMA_free(SO->Cx_Inode);
      }
      SO->Cx = NULL;
      SO->Cx_Inode = NULL;
   } 
   
   /* freeing overlays */
   if (SO->N_Overlays) {
      /* freeing color overlays */
      fprintf (SUMA_STDERR,"%s: Freeing Overlays.\n", FuncName);
      for (i=0; i <    SO->N_Overlays; ++i) {
         if (SO->Overlays_Inode[i] || SO->Overlays[i]) { /* there should be no case where only one of two is null but if such a case existed, you'll get notified below. */
            if (SUMA_ReleaseLink(SO->Overlays_Inode[i])) { 
               /* some links are left, do not free memory */
            } else {
               fprintf (SUMA_STDERR,"%s: Overlays[%d] is free of links, freeing allocated memory ...\n", FuncName, i);
               if (SO->Overlays[i]) SUMA_FreeOverlayPointer (SO->Overlays[i]);
               SUMA_free(SO->Overlays_Inode[i]); 
            }
         }
         SO->Overlays[i] = NULL;
         SO->Overlays_Inode[i] = NULL;
      }
      SO->N_Overlays = 0;
   }
   /*Now free the vector of pointers */
   SUMA_free(SO->Overlays);
   SUMA_free(SO->Overlays_Inode);
   
   /* freeing FN,  make sure that there are no links to FN*/
   if (SO->FN_Inode || SO->FN) { /* there should be no case where only one of two is null but if such a case existed, you'll get notified below. */
      if (SUMA_ReleaseLink(SO->FN_Inode)) { 
         /* some links are left, do not free memory */
      } else {
         if (SO->FN) {
            if (!SUMA_Free_FirstNeighb (SO->FN)) {
               fprintf(SUMA_STDERR,"Error SUMA_Free_Surface_Object : Failed to free SO->FN");
            }
         }
         /* now free SO->FN_Inode */
         SUMA_free(SO->FN_Inode);
      }
   }
   SO->FN = NULL;
   SO->FN_Inode = NULL;
   /* freeing Label */
   if (SO->Label) SUMA_free(SO->Label);
   
   /* freeing EL,  make sure that there are no links to EL*/
   if (SO->EL_Inode || SO->EL){ /* there should be no case where only one of two is null but if such a case existed, you'll get notified below. */
      if (SUMA_ReleaseLink(SO->EL_Inode)) { 
         /* some links are left, do not free memory */
      } else {
         if (SO->EL) SUMA_free_Edge_List (SO->EL);
         /* now free SO->EL_Inode */
         SUMA_free(SO->EL_Inode);
      }
   }
   SO->EL = NULL;
   SO->EL_Inode = NULL;
   
   if (SO) SUMA_free(SO);
   /*fprintf (stdout, "Done\n");*/
   SUMA_RETURN (YUP);
}   

/*!**
File : SUMA_Load_Surface_Object.c
\author Ziad Saad
Date : Fri Jan 25  2002
   
Purpose : 
   Print the contents of a Surface Object
   
   
Usage : 
    SUMA_Print_Surface_Object ( SO, Out)
   
   
Input paramters : 
\param   SO (SUMA_SurfaceObject *) Surface Object pointer
\param   Out (FILE *) stream pointer. (can use stdout or stderr)
         If you pass a file pointer, make sure it is open before 
         making the function call. Also, make sure you close it
         afterwards. You can pass a NULL pointer and the output 
         will default to stdout.
         
\sa SUMA_Load_Surface_Object        
***/
   
void SUMA_Print_Surface_Object (SUMA_SurfaceObject *SO, FILE *Out)
{   
   static char FuncName[]={"SUMA_Print_Surface_Object"};
   int MaxShow = 5, i,j, ND, NP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (Out == NULL) Out = stdout;
      
   if (SO == NULL) {
      fprintf (Out, "NULL Surface Object Pointer\n");
      SUMA_RETURNe;
   }

   ND = SO->NodeDim;
   NP = SO->FaceSetDim;
   fprintf (Out,"\n---------------------------------\n");
   if (SO->Label == NULL)
      fprintf (Out,"Label is NULL\n");
   else   {
      fprintf (Out,"Label: %s\n", SO->Label);
   }

   if (SO->FileType != SUMA_SUREFIT) {
      fprintf (Out,"FileName: %s\n", SO->Name.FileName);
      fprintf (Out,"Path: %s\n", SO->Name.Path);
   } else {
      fprintf (Out,"Coord FileName: %s \n", SO->Name_coord.FileName);
      fprintf (Out,"Coord Path: %s \n", SO->Name_coord.Path);
      fprintf (Out,"Topo FileName: %s \n", SO->Name_topo.FileName);
      fprintf (Out,"Topo Path: %s \n", SO->Name_topo.Path);
   }   
   fprintf (Out,"FileType: %d\t FileFormat: %d\n", SO->FileType, SO->FileFormat);
   
   fprintf (Out,"IDcode: %s\n", SO->idcode_str);
   if (SO->MapRef_idcode_str == NULL) {
      fprintf (Out,"MapRef_idcode_str is NULL\n");
   } else {
      fprintf (Out,"MapRef_idcode_str: %s\n", SO->MapRef_idcode_str);
   }
   
   fprintf (Out,"Group: %s\tState: %s\n", SO->Group, SO->State);
   
   if (SUMA_ismappable(SO)) {
      if (SUMA_isINHmappable(SO)) {
         fprintf (Out,"Surface is Inherently Mappable.\n");
      } else {
         fprintf (Out,"Surface is Mappable.\n");
      }
   } else {
      fprintf (Out,"Surface is NOT Mappable.\n");
   }
   
   
   if (SO->Name_NodeParent == NULL)
      fprintf (Out,"Name_NodeParent is NULL\n");
   else   {
      fprintf (Out,"Name_NodeParent: %s\n", SO->Name_NodeParent);
   }
   
   if (SO->MeshAxis) fprintf (Out,"ShowMeshAxis: %d\t MeshAxis Defined\n", SO->ShowMeshAxis);
      else fprintf (Out,"ShowMeshAxis: %d\t MeshAxis Undefined\n", SO->ShowMeshAxis);
   
   fprintf (Out,"N_Node: %d\t NodeDim: %d, EmbedDim: %d\n", \
      SO->N_Node, SO->NodeDim, SO->EmbedDim);
   fprintf (Out,"RotationWeight: %d, ViewCenterWeight %d\n", SO->RotationWeight, SO->ViewCenterWeight);
   fprintf (Out,"N_FaceSet: %d, FaceSetDim %d\n\n", SO->N_FaceSet, SO->FaceSetDim);
   
   fprintf (Out,"Center: [%.3f\t%.3f\t%.3f]\n", SO->Center[0], SO->Center[1],SO->Center[2]);
   
   fprintf (Out,"Maximum: [%.3f\t%.3f\t%.3f]\t (aMax %.3f)\n", SO->MaxDims[0], SO->MaxDims[1],SO->MaxDims[2], SO->aMaxDims);
   
   fprintf (Out,"Minimum: [%.3f\t%.3f\t%.3f]\t (aMin %.3f)\n\n", SO->MinDims[0], SO->MinDims[1],SO->MinDims[2], SO->aMinDims);
   fprintf (Out,"SUMA_VolPar_Aligned: %d\n", SO->SUMA_VolPar_Aligned);
   fprintf (Out,"VOLREG_APPLIED: %d\n", SO->VOLREG_APPLIED);
   fprintf (Out,"ShowSelecetedNode: %d\tSelectedNode %d\n",\
      SO->ShowSelectedNode, SO->SelectedNode);
   
   fprintf (Out,"ShowSelecetedFaceSet: %d\tSelectedFaceSet %d\n\n",\
      SO->ShowSelectedFaceSet, SO->SelectedFaceSet);
 
   if (Out == stdout) {
      fprintf (Out,"Strrrrrike return to look at more details ...");
      i = getchar();
      fprintf (Out,"\n");
   }
   
   SUMA_Show_VolPar(SO->VolPar, Out);
   
   if (Out == stdout) {
      fprintf (Out,"Strrrrrike return to look at more details ...");
      i = getchar();
      fprintf (Out,"\n");
   }
   if (SO->NodeList == NULL)
      fprintf (Out,"NodeList is NULL\n\n");
   else {
      if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
      fprintf (Out, "NodeList (showing %d out of %d elements):\n", MaxShow, SO->N_Node);
      for (i=0; i < MaxShow; ++i)   {
         for (j=0; j < SO->NodeDim; ++j) fprintf (Out, "\t%.3f", SO->NodeList[ND * i + j]);
         fprintf (Out, "\n\n");
      }
   }

   if (SO->NodeNormList == NULL)
      fprintf (Out,"NodeNormList is NULL\n\n");
   else {
      if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
      fprintf (Out, "NodeNormList (showing %d out of %d elements):\n", MaxShow, SO->N_Node);
      for (i=0; i < MaxShow; ++i)   {
         for (j=0; j < 3; ++j) fprintf (Out, "\t%.3f", SO->NodeNormList[ND * i + j]);
         fprintf (Out, "\n");
      }
      fprintf (Out, "\n");
   }


   if (SO->FaceSetList == NULL)
      fprintf (Out,"FaceSetList is NULL\n\n");
   else {
      if (MaxShow > SO->N_FaceSet) MaxShow = SO->N_FaceSet; 
      fprintf (Out, "FaceSetList: (showing %d out of %d elements):\n", MaxShow, SO->N_FaceSet);
      for (i=0; i < MaxShow; ++i)   {
         for (j=0; j < SO->FaceSetDim; ++j) fprintf (Out, "\t%d", SO->FaceSetList[NP * i + j]);
         fprintf (Out, "\n");
      }
      fprintf (Out, "\n");
   }
   
   if (SO->FaceNormList == NULL)
      fprintf (Out,"FaceNormList is NULL\n\n");
   else {
      if (MaxShow > SO->N_FaceSet) MaxShow = SO->N_FaceSet; 
      fprintf (Out, "FaceNormList (showing %d out of %d elements):\n", MaxShow, SO->N_FaceSet);
      for (i=0; i < MaxShow; ++i)   {
         for (j=0; j < 3; ++j) fprintf (Out, "\t%.3f", SO->FaceNormList[NP * i + j]);
         fprintf (Out, "\n");
      }
      fprintf (Out, "\n");
   }
      
   
   if (SO->MF == NULL)
      fprintf (Out,"SO->MF = NULL\n\n") ;
   else {
      if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
      fprintf (Out, "SO->MF (showing %d out of %d elements):\n", MaxShow, SO->N_Node);
      for (i=0; i < MaxShow ; ++i)   {
         fprintf (Out,"\tNode %d: Member of %d FaceSets: ", i, SO->MF->N_Memb[i]);
         for (j=0; j < SO->MF->N_Memb[i]; ++j) fprintf (Out,"%d, ", SO->MF->NodeMemberOfFaceSet[i][j]);
         fprintf (Out,"\n");
      }
      fprintf (Out, "\n");
   }
   
   if (SO->FN == NULL)
      fprintf (Out,"SO->FN = NULL\n\n") ;
   else {
      if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
      fprintf (Out, "SO->FN, Max. Neighbs of %d (showing %d out of %d elements):\n", SO->FN->N_Neighb_max, MaxShow, SO->N_Node);
      for (i=0; i < MaxShow ; ++i)   {
         fprintf (Out,"\tNode %d: %d Neighbors:\t", i, SO->FN->N_Neighb[i]);
          for (j=0; j< SO->FN->N_Neighb[i]; ++j) fprintf (Out,"%d, ", SO->FN->FirstNeighb[i][j]);
         fprintf (Out,"\n");
      }
      fprintf (Out, "\n");
   }
   
   if (SO->EL == NULL)
      fprintf (Out,"SO->EL = NULL\n\n") ;
   else {
      if (MaxShow > SO->EL->N_EL) MaxShow = SO->EL->N_EL; 
      fprintf (Out, "SO->EL, %d edges, max_Hosts %d, min_Hosts %d (showing %d out of %d elements):\n", \
            SO->EL->N_EL, SO->EL->max_N_Hosts, SO->EL->min_N_Hosts, MaxShow, SO->EL->N_EL);
      for (i=0; i < MaxShow ; ++i)   {
         fprintf (Out,"\tEdge %d: %d %d\tFlip %d Tri %d N_tri %d",\
             i, SO->EL->EL[i][0], SO->EL->EL[i][1], SO->EL->ELps[i][0], SO->EL->ELps[i][1],SO->EL->ELps[i][2]);
         fprintf (Out,"\n");
      }
      if (MaxShow > SO->N_FaceSet) MaxShow = SO->N_FaceSet; 
      fprintf (Out, "Triangle Limbs, (showing %d out of %d elements):\n", MaxShow, SO->N_FaceSet);
      for (i=0; i < MaxShow ; ++i)   {
         fprintf (Out,"\tTri_limb[%d][:] = %d %d %d\n", \
         i, SO->EL->Tri_limb[i][0], SO->EL->Tri_limb[i][1],SO->EL->Tri_limb[i][2]);
      } 
      fprintf (Out, "\n");
   }
   
   if (SO->PolyArea == NULL)
      fprintf (Out,"SO->PolyArea = NULL\n\n") ;
   else {
      if (MaxShow > SO->N_FaceSet) MaxShow = SO->N_FaceSet;
      fprintf (Out, "SO->PolyArea, showing %d out of %d elements:\n", MaxShow, SO->N_FaceSet);
      for (i=0; i < MaxShow ; ++i)   {
         fprintf (Out,"\tFaceSet %d: Area = %f\n", i, SO->PolyArea[i]);
      }
   }
   fprintf (Out,"\n");
   
   if (SO->Cx == NULL)
      fprintf (Out,"SO->Cx = NULL\n\n") ;
   else {
      if (MaxShow > SO->N_Node) MaxShow = SO->N_Node;
      fprintf (Out, "SO->Cx, showing %d out of %d elements:\n", MaxShow, SO->N_Node);
      for (i=0; i < MaxShow ; ++i)   {
         fprintf (Out,"\t SO->Cx[%d] = %f\n", i, SO->Cx[i]);
      }
   }
   
   fprintf (Out,"\n");
      
   fprintf (Out,"---------------------------------\n\n");
   
   SUMA_RETURNe;
}   

/*!
Create a Surface Object data structure 
*/

SUMA_SurfaceObject *SUMA_Alloc_SurfObject_Struct(int N)
{
   static char FuncName[]={"SUMA_Alloc_SurfObject_Struct"};
   SUMA_SurfaceObject *SO;
   int i, j;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SO = (SUMA_SurfaceObject *)SUMA_malloc(sizeof(SUMA_SurfaceObject)*N);
   if (SO == NULL) {
      SUMA_alloc_problem("SUMA_Alloc_SurfObject_Struct: could not allocate memory for SO");
   }
   
   for (i=0; i< N; ++i) {
      SO[i].Name_NodeParent = NULL;
      SO[i].Label = NULL;
      SO[i].EmbedDim = 3;
      SO[i].MF = NULL;
      SO[i].FN = NULL;
      SO[i].FN_Inode = NULL;
      SO[i].EL = NULL;
      SO[i].EL_Inode = NULL;
      SO[i].PolyArea = NULL;
      SO[i].SC = NULL;
      SO[i].Cx = NULL;
      SO[i].Cx_Inode = NULL;
      SO[i].VolPar = NULL;
      SO[i].glar_NodeList = NULL; 
      SO[i].glar_FaceSetList = NULL; 
      SO[i].glar_FaceNormList = NULL; 
      SO[i].glar_NodeNormList = NULL; 
      /* create vector of pointers */
      SO[i].Overlays = (SUMA_OVERLAYS **) SUMA_malloc(sizeof(SUMA_OVERLAYS *) * SUMA_MAX_OVERLAYS);
      SO[i].Overlays_Inode = (SUMA_INODE **) SUMA_malloc(sizeof(SUMA_INODE *) * SUMA_MAX_OVERLAYS); 
      /* fill pointers with NULL */
      for (j=0; j < SUMA_MAX_OVERLAYS; ++j) {
         SO[i].Overlays[j] = NULL;
         SO[i].Overlays_Inode[j] = NULL;
      }
      SO[i].N_Overlays = 0;
      SO[i].SentToAfni = NOPE;
      
      SO[i].MeshAxis = NULL;
      SO[i].State = NULL;
      SO[i].Group = NULL;
      SO[i].FaceSetMarker = NULL;
      
      
      
     }
   SUMA_RETURN(SO);
}/* SUMA_Alloc_SurfObject_Struct */

/*! 
   \brief function for freeing a SUMA_ROI structure.
   ans = SUMA_freeROI (ROI);
   \param ROI (SUMA_ROI *) pointer to an ROI structure
   \return YUP/NOPE
   
   \sa SUMA_AllocateROI
*/
SUMA_Boolean SUMA_freeROI (SUMA_ROI *ROI) 
{
   static char FuncName[]={"SUMA_freeROI"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
      
   if (ROI->Parent_idcode_str) SUMA_free(ROI->Parent_idcode_str);
   if (ROI->idcode_str) SUMA_free(ROI->idcode_str);
   if (ROI->Label) SUMA_free(ROI->Label);
   if (ROI->ElInd) SUMA_free(ROI->ElInd);
   if (ROI) SUMA_free(ROI);
   
   SUMA_RETURN (YUP);
}

/*! 
   \brief function for freeing a SUMA_DRAWN_ROI structure.
   ans = SUMA_freeDrawnROI (D_ROI);
   \param D_ROI (SUMA_DRAWN_ROI *) pointer to a drawn ROI structure
   \return YUP/NOPE
   
   \sa SUMA_AllocateDrawnROI
*/
SUMA_Boolean SUMA_freeDrawnROI (SUMA_DRAWN_ROI *D_ROI) 
{
   static char FuncName[]={"SUMA_freeDrawnROI"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   D_ROI->N_CtrlNodev = 0;
   
   if (D_ROI->Parent_idcode_str) SUMA_free(D_ROI->Parent_idcode_str);
   if (D_ROI->idcode_str) SUMA_free(D_ROI->idcode_str);
   if (D_ROI->Label) SUMA_free(D_ROI->Label);
   if (D_ROI) SUMA_free(D_ROI);
   
   SUMA_RETURN (YUP);
}

/*! 
   \brief function for creating (allocating and initializing) the contents of a SUMA_ROI structure.
   ROI = SUMA_AllocateROI (Parent_idcode_str, Type, label, int N_ElInd, int *ElInd) 
   \param Parent_idcode_str (char *) idcode of parent surface
   \param Type (SUMA_ROI_TYPE) type of ROI 
   \param label  (char *) label ascii label to label ROI. If you pass NULL, a number is assigned to the ROI automatically
   \param N_ElInd (int) number of elements in ElInd to allocate for.
   \param ElInd (int *) vector of indices to initialize the ROI with, pass NULL for no initialization.
                        Values in ElInd are copied into ROI->ElInd.
   \return (SUMA_ROI *) ROI pointer to ROI object created
               an idcode_str is assigned to ROI
   \sa SUMA_freeROI             
*/
SUMA_ROI *SUMA_AllocateROI (char *Parent_idcode_str, SUMA_ROI_TYPE Type, char *label, int N_ElInd, int *ElInd) 
{
   SUMA_ROI *ROI = NULL;
   static int ROI_index = 0;
   int i = 0;
   static char FuncName[]={"SUMA_AllocateROI"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   ROI = (SUMA_ROI *) SUMA_malloc (sizeof(SUMA_ROI));
   ROI->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));
   ROI->Parent_idcode_str = (char *)SUMA_calloc (strlen(Parent_idcode_str)+1, sizeof (char));
   if (label) ROI->Label = (char *)SUMA_calloc (strlen(label)+1, sizeof(char));
   else ROI->Label = (char *)SUMA_calloc (20, sizeof(char));
   ROI->ElInd = (int *)SUMA_calloc (N_ElInd, sizeof (int));
   
   if (!ROI || !ROI->idcode_str || !ROI->Parent_idcode_str || !ROI->Label || !ROI->ElInd) {
      fprintf (SUMA_STDERR, "Error %s: Failed allocating.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   ROI->N_ElInd = N_ElInd;
   
   if (ElInd) {
      for (i=0; i<N_ElInd; ++i)
         ROI->ElInd[i] = ElInd[i];
   }
   
   UNIQ_idcode_fill(ROI->idcode_str);   
   
   ROI->Parent_idcode_str = strcpy (ROI->Parent_idcode_str, Parent_idcode_str);
   if (label) ROI->Label = strcpy (ROI->Label, label);
   else sprintf (ROI->Label, "auto label %d", ROI_index);
   
   ROI->Type = Type;
   
   ++ROI_index;
   SUMA_RETURN (ROI);
}


/*! 
   \brief function for creating (allocating and initializing) the contents of a SUMA_DRAWN_ROI structure.
   D_ROI = SUMA_AllocateDrawnROI (Parent_idcode_str, DrawStatus, Type, label ) 
   \param Parent_idcode_str (char *) idcode of parent surface
   \param DrawStatus (SUMA_ROI_DRAWING_STATUS) status of ROI being drawn
   \param Type (SUMA_ROI_DRAWING_TYPE) type of ROI being drawn
   \param label (char *) label ascii label to label ROI. If you pass NULL, a number is assigned to the ROI automatically
   \return (SUMA_DRAWN_ROI *) D_ROI pointer to ROI object created
*/
SUMA_DRAWN_ROI *SUMA_AllocateDrawnROI (char *Parent_idcode_str, SUMA_ROI_DRAWING_STATUS DrawStatus, SUMA_ROI_DRAWING_TYPE Type, char *label) 
{
   SUMA_DRAWN_ROI *D_ROI = NULL;
   static int ROI_index = 0;
   static char FuncName[]={"SUMA_AllocateDrawnROI"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   D_ROI = (SUMA_DRAWN_ROI *) SUMA_malloc (sizeof(SUMA_DRAWN_ROI));
   D_ROI->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));
   D_ROI->Parent_idcode_str = (char *)SUMA_calloc (strlen(Parent_idcode_str)+1, sizeof (char));
   if (label) D_ROI->Label = (char *)SUMA_calloc (strlen(label)+1, sizeof(char));
   else D_ROI->Label = (char *)SUMA_calloc (20, sizeof(char));
   
   if (!D_ROI || !D_ROI->idcode_str || !D_ROI->Parent_idcode_str || !D_ROI->Label) {
      fprintf (SUMA_STDERR, "Error %s: Failed allocating.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   D_ROI->N_CtrlNodev = 0;
   
   UNIQ_idcode_fill(D_ROI->idcode_str);   
   
   D_ROI->Parent_idcode_str = strcpy (D_ROI->Parent_idcode_str, Parent_idcode_str);
   if (label) D_ROI->Label = strcpy (D_ROI->Label, label);
   else sprintf (D_ROI->Label, "auto label %d", ROI_index);
   
   D_ROI->DrawStatus = DrawStatus;
   D_ROI->Type = Type;
   
   ++ROI_index;
   SUMA_RETURN (D_ROI);
}

