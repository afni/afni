/*! Functions to create Displayable Objects */
#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_DO *SUMAg_DOv;
extern int SUMAg_N_DOv;

/*!
 allocate for a segment DO 
   SDO = SUMA_Alloc_SegmentDO ( N_n, Label)

   \param N_n (int) number of nodes to allocate for n0 and n1. 
      if N_n > 0  SDO->n0 and n1 (GLfloat *) vector to 3*N_n elements to contain the XYZ of nodes n0 and n1.
      else SDO->n0 and n1 = NULL
   \param Label (char *) label of segment DO. Pass NULL for no labels

   \returns SDO (SUMA_SegmentDO *) 
     
*/
SUMA_SegmentDO * SUMA_Alloc_SegmentDO (int N_n, char *Label)
{
   static char FuncName[]={"SUMA_Alloc_Segment"};
   SUMA_SegmentDO * SDO= NULL;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (N_n > 0) {
      SDO = (SUMA_SegmentDO *) SUMA_malloc (sizeof (SUMA_SegmentDO));
      if (!SDO) {
         fprintf(stderr,"Error %s: Failed to allocate for SDO\n", FuncName);
         SUMA_RETURN (SDO);
      }
      SDO->n0 = (GLfloat *) SUMA_calloc (3*N_n, sizeof(GLfloat));
      SDO->n1 = (GLfloat *) SUMA_calloc (3*N_n, sizeof(GLfloat));
   
      if (!SDO->n0 || !SDO->n1) {
         fprintf(stderr,"Error %s: Failed to allocate for SDO-n1 or SDO->n0\n", FuncName);
         if (SDO->n0) SUMA_free (SDO->n0);
         if (SDO->n1) SUMA_free (SDO->n1);
         if (SDO) SUMA_free (SDO);
         SUMA_RETURN (NULL);
      }
   } else {
      SDO->n0 = NULL;
      SDO->n1 = NULL;
      SDO->N_n = 0;
   }
   
   SDO->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));
   UNIQ_idcode_fill(SDO->idcode_str);
   
   
   if (Label) {
      SDO->Label = (char *)SUMA_calloc (strlen (SDO->Label)+1, sizeof(char));
      SDO->Label = strcpy (SDO->Label, Label);
   } else {
      SDO->Label = NULL;
   }
   
   SDO->N_n = N_n;
   SDO->Stipple = SUMA_SOLID_LINE;
   /* setup some default values */
   SDO->LineWidth = 4.0;
   SDO->LineCol[0] = 1.0; SDO->LineCol[1] = 0.3; SDO->LineCol[2] = 1.0; SDO->LineCol[3] = 1.0; 
   
   SUMA_RETURN (SDO);
}

void SUMA_free_SegmentDO (SUMA_SegmentDO * SDO)
{
   static char FuncName[]={"SUMA_free_SegmentDO"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SDO) SUMA_RETURNe;
   
   if (SDO->n0) SUMA_free(SDO->n0);
   if (SDO->n1) SUMA_free(SDO->n1);
   if (SDO->idcode_str) SUMA_free(SDO->idcode_str);
   if (SDO->Label) SUMA_free(SDO->Label);
   if (SDO) SUMA_free(SDO);
   
   SUMA_RETURNe;
}

/*! Allocate for a axis object */
SUMA_Axis* SUMA_Alloc_Axis (const char *Name)
{   
   static char FuncName[]={"SUMA_Alloc_Axis"};
   SUMA_Axis* Ax;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   Ax = (SUMA_Axis *) SUMA_malloc (sizeof (SUMA_Axis));
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

SUMA_Boolean SUMA_DrawSegmentDO (SUMA_SegmentDO *SDO)
{
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int i, N_n3;
   static char FuncName[]={"SUMA_DrawSegmentDO"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SDO) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   glLineWidth(SDO->LineWidth);
   
   switch (SDO->Stipple) {
      case SUMA_DASHED_LINE:
         glEnable(GL_LINE_STIPPLE);
         glLineStipple (1, 0x00FF); /* dashed, see OpenGL Prog guide, page 55 */
         break;
      case SUMA_SOLID_LINE:
         break;
      default:
         fprintf(stderr,"Error %s: Unrecognized Stipple option\n", FuncName);
         SUMA_RETURN(NOPE);
   }
   
   glBegin(GL_LINES);
   glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol); /*turn on emissivity for axis*/
   glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); /* turn off ambient and diffuse components */
   glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
   
   i = 0;
   N_n3 = 3*SDO->N_n;
   while (i < N_n3) {
      glVertex3f(SDO->n0[i], SDO->n0[i+1], SDO->n0[i+2]);
      glVertex3f(SDO->n1[i], SDO->n1[i+1], SDO->n1[i+2]); 
      i += 3;
   }
   
   glEnd();
      switch (SDO->Stipple) {
      case SUMA_DASHED_LINE:
         glDisable(GL_LINE_STIPPLE);
         break;
      case SUMA_SOLID_LINE:
         break;
   }
   
   SUMA_RETURN (YUP);
   
}
SUMA_Boolean SUMA_DrawAxis (SUMA_Axis* Ax)
{ 
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   static char FuncName[]={"SUMA_DrawAxis"};
   
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
         fprintf(stderr,"Error SUMA_DrawAxis: Unrecognized Stipple option\n");
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

/*!
   \brief find ROIs created on SO. ROIs created on a relative of SO will not be 
   returned. 
   \param SO (SUMA_SurfaceObject *)SO
   \param dov (SUMA_DO*) displayable objects vector
   \param N_do (int) number of displayable objects
   \param N_ROI (int *) to hold the number of DrawnROIs found
   \return ROIv (SUMA_DRAWN_ROI **) vector of SUMA_DRAWN_ROI * 
      such that ROIv[i]->Parent_idcode_str = SO->idcode_str
    
   - free  ROIv with SUMA_free(ROIv);
     
   \sa SUMA_Find_ROIrelatedtoSO 
*/
SUMA_DRAWN_ROI **SUMA_Find_ROIonSO (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_do, int *N_ROI)
{
   static char FuncName[]={"SUMA_Find_ROIonSO"};
   SUMA_DRAWN_ROI **ROIv=NULL;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   int i, roi_cnt=0;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   *N_ROI = -1;
   
   /* allocate for maximum */
   ROIv = (SUMA_DRAWN_ROI **)SUMA_malloc(sizeof(SUMA_DRAWN_ROI *)*N_do);
   if (!ROIv) {
      SUMA_SL_Crit("Failed to allocate for ROIv");
      SUMA_RETURN(NULL);
   }
   
   roi_cnt=0;
   for (i=0; i < N_do; ++i) {
      if (dov[i].ObjectType == ROIdO_type) {
         D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
         if (!strncmp(D_ROI->Parent_idcode_str, SO->idcode_str, strlen(SO->idcode_str))) {
            SUMA_LH("Found an ROI");
            ROIv[roi_cnt] = D_ROI;
            ++roi_cnt;
         }
      }
      if (dov[i].ObjectType == ROIO_type) {
         SUMA_SL_Warn("ROIO_types are being ignored.");
      }
   }
   
   /* realloc */
   ROIv = (SUMA_DRAWN_ROI **)SUMA_realloc(ROIv, sizeof(SUMA_DRAWN_ROI *)*roi_cnt);
   if (!ROIv) {
      SUMA_SL_Crit("Failed to reallocate for ROIv");
      SUMA_RETURN(NULL);
   }
   *N_ROI = roi_cnt;
   
   SUMA_RETURN(ROIv);
}

/*!
   \brief find ROIs related to SO. ROIs created on a relative of SO will be returned. 
   \param SO (SUMA_SurfaceObject *)SO
   \param dov (SUMA_DO*) displayable objects vector
   \param N_do (int) number of displayable objects
   \param N_ROI (int *) to hold the number of DrawnROIs found
   \return ROIv (SUMA_DRAWN_ROI **) vector of SUMA_DRAWN_ROI * 
      such that ROIv[i]->Parent_idcode_str = SO->idcode_str
    
   - free  ROIv with SUMA_free(ROIv);

   \sa SUMA_Find_ROIonSO
*/
SUMA_DRAWN_ROI **SUMA_Find_ROIrelatedtoSO (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_do, int *N_ROI)
{
   static char FuncName[]={"SUMA_Find_ROIrelatedtoSO"};
   SUMA_DRAWN_ROI **ROIv=NULL;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   int i, roi_cnt=0;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   *N_ROI = -1;
   
   /* allocate for maximum */
   ROIv = (SUMA_DRAWN_ROI **)SUMA_malloc(sizeof(SUMA_DRAWN_ROI *)*N_do);
   if (!ROIv) {
      SUMA_SL_Crit("Failed to allocate for ROIv");
      SUMA_RETURN(NULL);
   }
   
   roi_cnt=0;
   for (i=0; i < N_do; ++i) {
      if (dov[i].ObjectType == ROIdO_type) {
         D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
         if (SUMA_isdROIrelated (D_ROI, SO)) {
            SUMA_LH("Found an ROI");
            ROIv[roi_cnt] = D_ROI;
            ++roi_cnt;
         }
      }
      if (dov[i].ObjectType == ROIO_type) {
         SUMA_SL_Warn("ROIO_types are being ignored.");
      }
   }
   
   /* realloc */
   ROIv = (SUMA_DRAWN_ROI **)SUMA_realloc(ROIv, sizeof(SUMA_DRAWN_ROI *)*roi_cnt);
   if (!ROIv) {
      SUMA_SL_Crit("Failed to reallocate for ROIv");
      SUMA_RETURN(NULL);
   }
   *N_ROI = roi_cnt;
   
   SUMA_RETURN(ROIv);
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
   int i, id, ii, id1,id2, id3, EdgeIndex, FaceIndex, Node1, Node2, Node3, N_ROId=0, idFirst=0;
   float dx, dy, dz = 0.0;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   SUMA_ROI_DATUM *ROId=NULL;
   SUMA_ROI *ROI = NULL;
   DListElmt *NextElm=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   glEnable(GL_COLOR_MATERIAL);
   glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE); 
   for (i=0; i < N_do; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case ROIdO_type:
            D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
            if (!D_ROI->ROIstrokelist) {
               fprintf (SUMA_STDERR, "Error %s: NULL ROIstrokeList.\n", FuncName);
               SUMA_RETURN (NOPE);
            }else if (!dlist_size(D_ROI->ROIstrokelist)) {
               if (LocalHead) fprintf (SUMA_STDERR, "%s: Empty ROIstrokelist.\n", FuncName);
               break;
            }
            if (SUMA_isdROIrelated (D_ROI, SO)) { /* draw it */
               if (LocalHead) fprintf(SUMA_STDERR, "%s: Drawing Drawn ROI %s (Status %d)\n", FuncName, D_ROI->Label, D_ROI->DrawStatus);
               if (D_ROI->DrawStatus == SUMA_ROI_InCreation) {
                  switch (D_ROI->Type) {
                     case SUMA_ROI_OpenPath:
                        ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 0.0; ROI_SphCol_frst[2] = 0.0; ROI_SphCol_frst[3] = 1.0;     
                        ROI_SphCol[0] = 0.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 0.0; ROI_SphCol[3] = 1.0;     
                        break;   
                     case SUMA_ROI_ClosedPath:
                        ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 1.0; ROI_SphCol_frst[2] = 0.0; ROI_SphCol_frst[3] = 1.0;     
                        ROI_SphCol[0] = 0.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 1.0; ROI_SphCol[3] = 1.0;  
                        break;   
                     case SUMA_ROI_FilledArea:
                        ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 0.0; ROI_SphCol_frst[2] = 1.0; ROI_SphCol_frst[3] = 1.0;     
                        ROI_SphCol[0] = 1.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 0.0; ROI_SphCol[3] = 1.0;  
                        break;   
                     default:
                        ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 0.3; ROI_SphCol_frst[2] = 1.0; ROI_SphCol_frst[3] = 1.0;     
                        ROI_SphCol[0] = 1.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 0.0; ROI_SphCol[3] = 1.0;     
                        break;

                  }
               } else {
                  /* finished, mark it as such */
                  ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 0.3; ROI_SphCol_frst[2] = 1.0; ROI_SphCol_frst[3] = 1.0;     
                  ROI_SphCol[0] = 1.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 0.0; ROI_SphCol[3] = 1.0;
               }

               /* start with the first element */
               NextElm = NULL;
               N_ROId = 0;
               do {
                  if (!NextElm) {
                     NextElm = dlist_head(D_ROI->ROIstrokelist);
                  }else {
                     NextElm = dlist_next(NextElm);
                  }
                  ROId = (SUMA_ROI_DATUM *)NextElm->data;
                  if (ROId->Type == SUMA_ROI_NodeSegment) { 
                     if (ROId->N_n) {
                        if (!N_ROId) {
                           /* draw 1st sphere */
                           glMaterialfv(GL_FRONT, GL_EMISSION, ROI_SphCol_frst);
                           idFirst = 3 * ROId->nPath[0];
                           glTranslatef (SO->NodeList[idFirst], SO->NodeList[idFirst+1], SO->NodeList[idFirst+2]);
                           gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
                           glTranslatef (-SO->NodeList[idFirst], -SO->NodeList[idFirst+1], -SO->NodeList[idFirst+2]);
                        } 

                        glLineWidth(6);
                        glMaterialfv(GL_FRONT, GL_EMISSION, ROI_SphCol);
                        /* always start at 1 since the 0th node was draw at the end of the previous ROId */
                        for (ii = 1; ii < ROId->N_n; ++ii) {
                           id = 3 * ROId->nPath[ii];
                           id2 = 3 * ROId->nPath[ii-1];

                           /* draw lines connecting spheres */
                           glBegin(GL_LINES);
                           glVertex3f(SO->NodeList[id2], SO->NodeList[id2+1], SO->NodeList[id2+2]);
                           glVertex3f(SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]); 
                           glEnd();

                           glTranslatef (SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]);
                           gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
                           glTranslatef (-SO->NodeList[id], -SO->NodeList[id+1], -SO->NodeList[id+2]);
                        }

                        
                        glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);
                        ++N_ROId;
                     }
                  } else { /* non segment type Drawn ROI */
                        glMaterialfv(GL_FRONT, GL_EMISSION, ROI_NodeGroup);
                        for (ii=0; ii < ROId->N_n; ++ii) {
                           id = 3 * ROId->nPath[ii];
                           glTranslatef (SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]);
                           gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
                           glTranslatef (-SO->NodeList[id], -SO->NodeList[id+1], -SO->NodeList[id+2]);
                        }
                        glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);
                  
                  }
               } while (NextElm != dlist_tail(D_ROI->ROIstrokelist));
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
   glDisable(GL_COLOR_MATERIAL);


   SUMA_RETURN (YUP);
}         

/*! Create the cross hair */
SUMA_Boolean SUMA_DrawCrossHair (SUMA_CrossHair* Ch)
{
   static char FuncName[]={"SUMA_DrawCrossHair"};
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
SUMA_Boolean SUMA_DrawFaceSetMarker (SUMA_FaceSetMarker* FM)
{   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0}, dx, dy, dz;
   static char FuncName[]={"SUMA_DrawFaceSetMarker"};
   
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
void SUMA_DrawMesh(SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *sv)
{  static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int i, ii, ND, id, ip, NP, PolyMode;
   static char FuncName[]={"SUMA_DrawMesh"};
   SUMA_DRAWN_ROI *DrawnROI = NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* check on rendering mode */
   if (SurfObj->PolyMode != SRM_ViewerDefault) {
     /* not the default, do the deed */
     SUMA_SET_GL_RENDER_MODE(SurfObj->PolyMode); 
   }
   
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
         /* Draw Axis */
         if (SurfObj->MeshAxis && SurfObj->ShowMeshAxis)   {
            if (!SUMA_DrawAxis (SurfObj->MeshAxis)) {
               fprintf(stderr,"Error SUMA_DrawAxis: Unrecognized Stipple option\n");
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
            if (!SUMA_DrawFaceSetMarker (SurfObj->FaceSetMarker)) {
               fprintf(SUMA_STDERR,"Error SUMA_DrawMesh: Failed in SUMA_DrawFaceSetMarker\b");
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
         /*fprintf(stdout, "Out SUMA_DrawMesh, ARRAY mode\n");*/
         
         glDisable(GL_COLOR_MATERIAL);
         
         if (!SUMA_Draw_SO_ROI (SurfObj, SUMAg_DOv, SUMAg_N_DOv)) {
            fprintf (SUMA_STDERR, "Error %s: Failed in drawing ROI objects.\n", FuncName);
         }
         
         break;

   } /* switch DRAW_METHOD */
   
   /* reset viewer default rendering modes */
   if (SurfObj->PolyMode != SRM_ViewerDefault) {
     /* not the default, do the deed */
     SUMA_SET_GL_RENDER_MODE(sv->PolyMode); 
   }
   
   SUMA_RETURNe;
} /* SUMA_DrawMesh */

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
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO\n", FuncName);
   /* Start with the big ones and down*/
   /* From SUMA 1.2 and on, some glar_ pointers are copies of others and should not be freed */ 
   SO->glar_FaceSetList = NULL;
   SO->glar_NodeList = NULL;
   SO->glar_NodeNormList = NULL;
   SO->glar_FaceNormList = NULL;
   
   /*fprintf (stdout, "SO->NodeList... ");*/
   if (SO->NodeList)   SUMA_free(SO->NodeList);
   /*fprintf (stdout, "SO->FaceSetList... ");*/
   if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
   /*fprintf (stdout, "SO->FaceSetList... ");*/
   if (SO->NodeNormList) SUMA_free(SO->NodeNormList);
   /*fprintf (stdout, "SO->NodeNormList... ");*/
   if (SO->FaceNormList) SUMA_free(SO->FaceNormList);
   /*fprintf (stdout, "SO->FaceNormList... ");*/
   if (SO->Name_NodeParent) SUMA_free(SO->Name_NodeParent);
   /*fprintf (stdout, "SO->Name.FileName... ");*/
   if (SO->Name.FileName) SUMA_free(SO->Name.FileName);
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->Name.Path\n", FuncName);
   if (SO->Name.Path) SUMA_free(SO->Name.Path);
   if (SO->MeshAxis) SUMA_Free_Axis (SO->MeshAxis);
   if (SO->MF) {
      if (!SUMA_Free_MemberFaceSets (SO->MF)) {
            fprintf(SUMA_STDERR,"Error SUMA_Free_Surface_Object : Failed to free SO->MF");
         }
   }
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->NodeMarker\n", FuncName);
   if (SO->NodeMarker) SUMA_Free_SphereMarker (SO->NodeMarker);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->FaceSetMarker\n", FuncName);
   if (SO->FaceSetMarker) SUMA_Free_FaceSetMarker(SO->FaceSetMarker);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->idcode_str\n", FuncName);
   if (SO->idcode_str) free(SO->idcode_str); /* DO NOT use SUMA_free because this pointer is created by UNIQ_hashcode which uses afni's calloc 
                                                If you do so, you'll get a nasty warning from SUMA_free*/
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->MapRef_idcode_str\n", FuncName);
   if (SO->MapRef_idcode_str) SUMA_free(SO->MapRef_idcode_str);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->Group\n", FuncName);
   if (SO->Group) SUMA_free(SO->Group);
   if (SO->State) SUMA_free(SO->State);
   if (SO->PolyArea) SUMA_free(SO->PolyArea);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->SC\n", FuncName);
   if (SO->SC) {
      SUMA_Free_SURFACE_CURVATURE(SO->SC);
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing Cx\n", FuncName);

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
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing overlays\n", FuncName);
   
   /* freeing overlays */
   if (SO->N_Overlays) {
      /* freeing color overlays */
      for (i=0; i <    SO->N_Overlays; ++i) {
         SUMA_ReleaseOverlay(SO->Overlays[i] , SO->Overlays_Inode[i]);
         SO->Overlays[i] = NULL;
         SO->Overlays_Inode[i] = NULL;
      }
      SO->N_Overlays = 0;
   }
   /*Now free the vector of pointers */
   SUMA_free(SO->Overlays);
   SUMA_free(SO->Overlays_Inode);
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing FN\n", FuncName);

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

   if (SO->SurfCont) SUMA_FreeSurfContStruct(SO->SurfCont);
   
   if (SO) SUMA_free(SO);
   
   if (LocalHead) fprintf (stdout, "Done\n");
   SUMA_RETURN (YUP);
}   


/*!
   \brief Creates a string containing information about the surface
   
   \param SO (SUMA_SurfaceObject *) pointer to surface object structure
   \return s (char *) pointer to NULL terminated string containing surface info.
   It is your responsability to free it.
   \sa SUMA_Print_Surface_Object
   
*/
char *SUMA_SurfaceObject_Info (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_SurfaceObject_Info"};
   int MaxShow = 5, i,j, ND = 0, NP = 0, N_max = 10000;
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SS = SUMA_StringAppend (NULL, NULL);
      
   if (SO) {
      ND = SO->NodeDim;
      NP = SO->FaceSetDim;
      
      if (SO->Label == NULL)
         SS = SUMA_StringAppend (SS,"Label is NULL.\n");
      else   {
         sprintf (stmp,"Label: %s\n", SO->Label);
         SS = SUMA_StringAppend (SS,stmp);
      }

      switch (SO->FileType) {
         case SUMA_SUREFIT:
            sprintf (stmp,"SureFit surface.\n");
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"Coord FileName: %s \n", SO->Name_coord.FileName);
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"Coord Path: %s \n", SO->Name_coord.Path);
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"Topo FileName: %s \n", SO->Name_topo.FileName);
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"Topo Path: %s \n", SO->Name_topo.Path);
            SS = SUMA_StringAppend (SS,stmp);
            break;
         case SUMA_VEC:
            sprintf (stmp,"VEC surface.\n");
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"NodeList FileName: %s \n", SO->Name_coord.FileName);
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"NodeList Path: %s \n", SO->Name_coord.Path);
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"FaceSetList FileName: %s \n", SO->Name_topo.FileName);
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"FaceSetList Path: %s \n", SO->Name_topo.Path);
            SS = SUMA_StringAppend (SS,stmp);
            break;
         case SUMA_FREE_SURFER:
            sprintf (stmp,"FreeSurfer surface.\n");
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"Path: %s\n", SO->Name.Path);
            SS = SUMA_StringAppend (SS,stmp);
            break;
         case SUMA_INVENTOR_GENERIC:
            sprintf (stmp,"Inventor generic surface.\n");
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"Path: %s\n", SO->Name.Path);
            SS = SUMA_StringAppend (SS,stmp);
            break;
         case SUMA_PLY: 
            sprintf (stmp,"PLY surface.\n");
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend (SS,stmp);
            sprintf (stmp,"Path: %s\n", SO->Name.Path);
            SS = SUMA_StringAppend (SS,stmp);
            break;
         case SUMA_FT_NOT_SPECIFIED:
            sprintf (stmp,"File Type not specified.\n");
            SS = SUMA_StringAppend (SS,stmp);
            break;
         default:
            sprintf (stmp,"Unknown surface type.\n");
            SS = SUMA_StringAppend (SS,stmp);
            break;
      }

      sprintf (stmp,"FileType: %d\t FileFormat: %d\n", SO->FileType, SO->FileFormat);
      SS = SUMA_StringAppend (SS,stmp);

      sprintf (stmp,"IDcode: %s\n", SO->idcode_str);
      SS = SUMA_StringAppend (SS,stmp);
      
      if (SO->MapRef_idcode_str == NULL) {
         sprintf (stmp,"MapRef_idcode_str is NULL\n");
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         sprintf (stmp,"MapRef_idcode_str: %s\n", SO->MapRef_idcode_str);
         SS = SUMA_StringAppend (SS,stmp);
      }

      sprintf (stmp,"Group: %s\tState: %s\n", SO->Group, SO->State);
      SS = SUMA_StringAppend (SS,stmp);

      if (SUMA_ismappable(SO)) {
         if (SUMA_isINHmappable(SO)) {
            sprintf (stmp,"Surface is Inherently Mappable.\n");
            SS = SUMA_StringAppend (SS,stmp);
         } else {
            sprintf (stmp,"Surface is Mappable.\n");
           SS = SUMA_StringAppend (SS,stmp);
         }
      } else {
         sprintf (stmp,"Surface is NOT Mappable.\n");
         SS = SUMA_StringAppend (SS,stmp);
      }


      if (SO->Name_NodeParent == NULL) {
         sprintf (stmp,"Name_NodeParent is NULL\n");
         SS = SUMA_StringAppend (SS,stmp);
      } else   {
         sprintf (stmp,"Name_NodeParent: %s\n", SO->Name_NodeParent);
         SS = SUMA_StringAppend (SS,stmp);
      }

      if (SO->MeshAxis) {
         sprintf (stmp,"ShowMeshAxis: %d\t MeshAxis Defined\n", SO->ShowMeshAxis);
         SS = SUMA_StringAppend (SS,stmp);
      }   else {
         sprintf (stmp,"ShowMeshAxis: %d\t MeshAxis Undefined\n", SO->ShowMeshAxis);
         SS = SUMA_StringAppend (SS,stmp);
      }  
      
      sprintf (stmp,"RenderMode: %d\n", SO->PolyMode);
      SS = SUMA_StringAppend (SS,stmp);
      
      sprintf (stmp,"N_Node: %d\t NodeDim: %d, EmbedDim: %d\n", \
         SO->N_Node, SO->NodeDim, SO->EmbedDim);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"RotationWeight: %d, ViewCenterWeight %d\n", SO->RotationWeight, SO->ViewCenterWeight);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"N_FaceSet: %d, FaceSetDim %d\n\n", SO->N_FaceSet, SO->FaceSetDim);
      SS = SUMA_StringAppend (SS,stmp);

      sprintf (stmp,"Center: [%.3f\t%.3f\t%.3f]\n", SO->Center[0], SO->Center[1],SO->Center[2]);
      SS = SUMA_StringAppend (SS,stmp);

      sprintf (stmp,"Maximum: [%.3f\t%.3f\t%.3f]\t (aMax %.3f)\n", SO->MaxDims[0], SO->MaxDims[1],SO->MaxDims[2], SO->aMaxDims);
      SS = SUMA_StringAppend (SS,stmp);

      sprintf (stmp,"Minimum: [%.3f\t%.3f\t%.3f]\t (aMin %.3f)\n\n", SO->MinDims[0], SO->MinDims[1],SO->MinDims[2], SO->aMinDims);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"SUMA_VolPar_Aligned: %d\n", SO->SUMA_VolPar_Aligned);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"VOLREG_APPLIED: %d\n", SO->VOLREG_APPLIED);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"ShowSelecetedNode: %d\tSelectedNode %d\n",\
         SO->ShowSelectedNode, SO->SelectedNode);
      SS = SUMA_StringAppend (SS,stmp);

      sprintf (stmp,"ShowSelecetedFaceSet: %d\tSelectedFaceSet %d\n\n",\
         SO->ShowSelectedFaceSet, SO->SelectedFaceSet);
      SS = SUMA_StringAppend (SS,stmp);

      SS = SUMA_StringAppend (SS, SUMA_VolPar_Info(SO->VolPar));

      if (SO->NodeList == NULL) {
         sprintf (stmp,"NodeList is NULL\n\n");
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
         sprintf (stmp, "NodeList (showing %d out of %d elements):\n", MaxShow, SO->N_Node);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow; ++i)   {
            for (j=0; j < SO->NodeDim; ++j) {
               sprintf (stmp, "\t%.3f", SO->NodeList[ND * i + j]);
               SS = SUMA_StringAppend (SS,stmp);
            }
            sprintf (stmp, "\n\n");
            SS = SUMA_StringAppend (SS,stmp);
         }
      }

      if (SO->NodeNormList == NULL) {
         sprintf (stmp,"NodeNormList is NULL\n\n");
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
         sprintf (stmp, "NodeNormList (showing %d out of %d elements):\n", MaxShow, SO->N_Node);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow; ++i)   {
            for (j=0; j < 3; ++j) {
               sprintf (stmp, "\t%.3f", SO->NodeNormList[ND * i + j]);
               SS = SUMA_StringAppend (SS,stmp);
            }
            sprintf (stmp, "\n");
            SS = SUMA_StringAppend (SS,stmp);
         }
         sprintf (stmp, "\n");
         SS = SUMA_StringAppend (SS,stmp);
      }


      if (SO->FaceSetList == NULL) {
         sprintf (stmp,"FaceSetList is NULL\n\n");
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_FaceSet) MaxShow = SO->N_FaceSet; 
         sprintf (stmp, "FaceSetList: (showing %d out of %d elements):\n", MaxShow, SO->N_FaceSet);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow; ++i)   {
            for (j=0; j < SO->FaceSetDim; ++j) {
               sprintf (stmp, "\t%d", SO->FaceSetList[NP * i + j]);
               SS = SUMA_StringAppend (SS,stmp);
            }
            sprintf (stmp, "\n");
            SS = SUMA_StringAppend (SS,stmp);
         }
         sprintf (stmp, "\n");
         SS = SUMA_StringAppend (SS,stmp);
      }

      if (SO->FaceNormList == NULL) {
         sprintf (stmp,"FaceNormList is NULL\n\n");
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_FaceSet) MaxShow = SO->N_FaceSet; 
         sprintf (stmp, "FaceNormList (showing %d out of %d elements):\n", MaxShow, SO->N_FaceSet);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow; ++i)   {
            for (j=0; j < 3; ++j) {
               sprintf (stmp, "\t%.3f", SO->FaceNormList[NP * i + j]);
               SS = SUMA_StringAppend (SS,stmp);
            }
            sprintf (stmp, "\n");
            SS = SUMA_StringAppend (SS,stmp);
         }
         sprintf (stmp, "\n");
         SS = SUMA_StringAppend (SS,stmp);
      }


      if (SO->MF == NULL) {
         sprintf (stmp,"SO->MF = NULL\n\n") ;
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
         sprintf (stmp, "SO->MF (showing %d out of %d elements):\n", MaxShow, SO->N_Node);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\tNode %d: Member of %d FaceSets: ", i, SO->MF->N_Memb[i]);
            SS = SUMA_StringAppend (SS,stmp);
            for (j=0; j < SO->MF->N_Memb[i]; ++j) {
               sprintf (stmp,"%d, ", SO->MF->NodeMemberOfFaceSet[i][j]);
               SS = SUMA_StringAppend (SS,stmp);
            }
            sprintf (stmp,"\n");
            SS = SUMA_StringAppend (SS,stmp);
         }
         sprintf (stmp, "\n");
         SS = SUMA_StringAppend (SS,stmp);
      }

      if (SO->FN == NULL) {
         sprintf (stmp,"SO->FN = NULL\n\n") ;
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
         sprintf (stmp, "SO->FN, Max. Neighbs of %d (showing %d out of %d elements):\n", SO->FN->N_Neighb_max, MaxShow, SO->N_Node);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\tNode %d: %d Neighbors:\t", i, SO->FN->N_Neighb[i]);
            SS = SUMA_StringAppend (SS,stmp);
             for (j=0; j< SO->FN->N_Neighb[i]; ++j) {
               sprintf (stmp,"%d, ", SO->FN->FirstNeighb[i][j]);
               SS = SUMA_StringAppend (SS,stmp);
            }
            sprintf (stmp,"\n");
            SS = SUMA_StringAppend (SS,stmp);
         }
         sprintf (stmp, "\n");
         SS = SUMA_StringAppend (SS,stmp);
      }

      if (SO->EL == NULL) {
         sprintf (stmp,"SO->EL = NULL\n\n") ;
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->EL->N_EL) MaxShow = SO->EL->N_EL; 
         sprintf (stmp, "SO->EL, %d edges, max_Hosts %d, min_Hosts %d (showing %d out of %d elements):\n", \
               SO->EL->N_EL, SO->EL->max_N_Hosts, SO->EL->min_N_Hosts, MaxShow, SO->EL->N_EL);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\tEdge %d: %d %d\tFlip %d Tri %d N_tri %d\n",\
                i, SO->EL->EL[i][0], SO->EL->EL[i][1], SO->EL->ELps[i][0], SO->EL->ELps[i][1],SO->EL->ELps[i][2]);
            SS = SUMA_StringAppend (SS,stmp);   
         }
         sprintf (stmp,"\n");
         SS = SUMA_StringAppend (SS,stmp);
         
         if (MaxShow > SO->N_FaceSet) MaxShow = SO->N_FaceSet; 
         sprintf (stmp, "Triangle Limbs, (showing %d out of %d elements):\n", MaxShow, SO->N_FaceSet);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\tTri_limb[%d][:] = %d %d %d\n", \
            i, SO->EL->Tri_limb[i][0], SO->EL->Tri_limb[i][1],SO->EL->Tri_limb[i][2]);
            SS = SUMA_StringAppend (SS,stmp);
         } 
         sprintf (stmp, "\n");
         SS = SUMA_StringAppend (SS,stmp);
      }

      if (SO->PolyArea == NULL) {
         sprintf (stmp,"SO->PolyArea = NULL\n\n") ;
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_FaceSet) MaxShow = SO->N_FaceSet;
         sprintf (stmp, "SO->PolyArea, showing %d out of %d elements:\n", MaxShow, SO->N_FaceSet);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\tFaceSet %d: Area = %f\n", i, SO->PolyArea[i]);
            SS = SUMA_StringAppend (SS,stmp);
         }
      }
      sprintf (stmp,"\n");
      SS = SUMA_StringAppend (SS,stmp);

      if (SO->Cx == NULL) {
         sprintf (stmp,"SO->Cx = NULL\n\n") ;
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_Node) MaxShow = SO->N_Node;
         sprintf (stmp, "SO->Cx, showing %d out of %d elements:\n", MaxShow, SO->N_Node);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\t SO->Cx[%d] = %f\n", i, SO->Cx[i]);
            SS = SUMA_StringAppend (SS,stmp);
         }
      }

      if (SO->N_Overlays) {
         sprintf (stmp,"%d Overlay planes.\n", SO->N_Overlays);
         SS = SUMA_StringAppend (SS,stmp);
         s = SUMA_ColorOverlayPlane_Info(SO->Overlays, SO->N_Overlays);
         if (s) {
            SS = SUMA_StringAppend (SS,s);
            SUMA_free(s);
            s = NULL;
         }
         
      }else {
         sprintf (stmp,"No overlay planes.\n");
         SS = SUMA_StringAppend (SS,stmp);
      }
      sprintf (stmp,"\n");
      SS = SUMA_StringAppend (SS,stmp);

      
   } else {
      sprintf (stmp, "NULL Surface Object Pointer.");
      SS = SUMA_StringAppend (SS, stmp);
   }   
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (s);
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
\sa SUMA_SurfaceObject_Info      
***/
   
void SUMA_Print_Surface_Object (SUMA_SurfaceObject *SO, FILE *Out)
{   
   static char FuncName[]={"SUMA_Print_Surface_Object"};
   char *s;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (Out == NULL) Out = stdout;
      
   s = SUMA_SurfaceObject_Info (SO);
   
   if (s) {
      fprintf (Out, "%s", s);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_SurfaceObject_Info.\n", FuncName);
   }   
   
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
      SO[i].idcode_str = NULL;
      SO[i].MapRef_idcode_str = NULL;
      SO[i].Name.Path = NULL;
      SO[i].Name.FileName = NULL;
      SO[i].Name_coord.Path = NULL;
      SO[i].Name_coord.FileName = NULL;
      SO[i].Name_topo.Path = NULL;
      SO[i].Name_topo.FileName = NULL;
      SO[i].SUMA_VolPar_Aligned = NOPE;
      SO[i].VOLREG_APPLIED = NOPE;
      SO[i].SurfCont = SUMA_CreateSurfContStruct();
      SO[i].PolyMode = SRM_ViewerDefault;
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
   
   
   if (D_ROI->Parent_idcode_str) SUMA_free(D_ROI->Parent_idcode_str);
   if (D_ROI->idcode_str) SUMA_free(D_ROI->idcode_str);
   if (D_ROI->Label) SUMA_free(D_ROI->Label);
   if (D_ROI->ROIstrokelist) SUMA_EmptyDestroyList(D_ROI->ROIstrokelist);
   if (D_ROI->ActionStack) SUMA_EmptyDestroyActionStack(D_ROI->ActionStack);
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
   ROI->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH+1, sizeof(char));
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
   D_ROI = SUMA_AllocateDrawnROI (Parent_idcode_str, DrawStatus, Type, label , ilabel) 
   \param Parent_idcode_str (char *) idcode of parent surface
   \param DrawStatus (SUMA_ROI_DRAWING_STATUS) status of ROI being drawn
   \param Type (SUMA_ROI_DRAWING_TYPE) type of ROI being drawn
   \param label (char *) label ascii label to label ROI. If you pass NULL, a number is assigned to the ROI automatically
   \param ilabel (int) integer label (or value)
   \return (SUMA_DRAWN_ROI *) D_ROI pointer to ROI object created
*/
SUMA_DRAWN_ROI *SUMA_AllocateDrawnROI (char *Parent_idcode_str, SUMA_ROI_DRAWING_STATUS DrawStatus, 
                                       SUMA_ROI_DRAWING_TYPE Type, char *label, int ilabel) 
{
   SUMA_DRAWN_ROI *D_ROI = NULL;
   static int ROI_index = 1;
   static char FuncName[]={"SUMA_AllocateDrawnROI"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   D_ROI = (SUMA_DRAWN_ROI *) SUMA_malloc (sizeof(SUMA_DRAWN_ROI));
   D_ROI->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));
   D_ROI->Parent_idcode_str = (char *)SUMA_calloc (strlen(Parent_idcode_str)+1, sizeof (char));
   D_ROI->ROIstrokelist = (DList *)SUMA_malloc (sizeof(DList));
   dlist_init(D_ROI->ROIstrokelist, SUMA_FreeROIDatum);
   
   if (label) D_ROI->Label = (char *)SUMA_calloc (strlen(label)+1, sizeof(char));
   else D_ROI->Label = (char *)SUMA_calloc (20, sizeof(char));
   
   if (!D_ROI || !D_ROI->idcode_str || !D_ROI->Parent_idcode_str || !D_ROI->Label) {
      fprintf (SUMA_STDERR, "Error %s: Failed allocating.\n", FuncName);
      SUMA_RETURN (NULL);
   }
      
   UNIQ_idcode_fill(D_ROI->idcode_str);   
   
   D_ROI->Parent_idcode_str = strcpy (D_ROI->Parent_idcode_str, Parent_idcode_str);
   if (label) D_ROI->Label = strcpy (D_ROI->Label, label);
   else sprintf (D_ROI->Label, "auto label %d", ROI_index);
   
   D_ROI->DrawStatus = DrawStatus;
   D_ROI->Type = Type;
   
   D_ROI->ActionStack = SUMA_CreateActionStack ();
   D_ROI->StackPos = NULL;
   
   D_ROI->iLabel = ilabel;
   
   ++ROI_index;
   SUMA_RETURN (D_ROI);
}

/*!
   A destructor for SUMA_ROI_DATUM *
*/
void SUMA_FreeROIDatum (void * data) 
{
   static char FuncName[]={"SUMA_FreeROIDatum"};
   SUMA_ROI_DATUM *ROId=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   ROId = (SUMA_ROI_DATUM *)data;
   
   if (!ROId) {
      SUMA_RETURNe;
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Freeing nPath\n", FuncName);
   if (ROId->nPath) SUMA_free(ROId->nPath);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Freeing tPath\n", FuncName);
   if (ROId->tPath) SUMA_free(ROId->tPath);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Freeing ROId\n", FuncName);
   SUMA_free(ROId);
   
   SUMA_RETURNe;
}

/*!
   A constructor for SUMA_ROI_DATUM *
*/
SUMA_ROI_DATUM * SUMA_AllocROIDatum (void) 
{
   static char FuncName[]={"SUMA_AllocROIDatum"};
   SUMA_ROI_DATUM *ROId=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   ROId = (SUMA_ROI_DATUM *) SUMA_malloc (sizeof(SUMA_ROI_DATUM));
   
   if (!ROId) {
      SUMA_RETURN (NULL);
   }
   
   ROId->nPath = ROId->tPath = NULL;
   ROId->N_n = ROId->N_t = 0;
   ROId->nDistance = ROId->tDistance = 0.0;
   ROId->Type = SUMA_ROI_Undefined;
   ROId->action = SUMA_BSA_Undefined;
   SUMA_RETURN (ROId);
}

/*!
   \brief Determine if nPath in ROId1 and ROId2 are identical (same nodes).
   
   - Comparison will fail if either datum is null
   
   - Function not tested 
   
*/
SUMA_Boolean SUMA_isROIdequal (SUMA_ROI_DATUM *ROId1, SUMA_ROI_DATUM *ROId2)
{
   static char FuncName[]={"SUMA_isROIdequal"};
   int i;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!ROId1 || !ROId2) SUMA_RETURN(NOPE);
   if (ROId1->N_n != ROId2->N_n) SUMA_RETURN(NOPE);
   if (!ROId1->nPath || !ROId2->nPath) SUMA_RETURN(NOPE);
   i = 0;
   do {
      if (ROId1->nPath[i] != ROId2->nPath[i]) SUMA_RETURN(NOPE);
      ++i;
   }while (i < ROId2->N_n);
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Merges two ROIdatum together. 
   ans = SUMA_AppendToROIdatum (ROIlink, ROId);
   ROId = [ROId ROIlink]
   
   \param ROIlink (SUMA_ROI_DATUM *)
   \param ROId (SUMA_ROI_DATUM *)
   \return ans YUP/NOPE
   
   - ROId becomes ROId followed by ROIlink 
   - ROIlink is not freed
   - It is required that the last node of ROId be the first node of ROIlink.
   - This is not required for the tPath (the triangle path). By the same token,
   it is not guaranteed that the resultant tPath is contiguous (not yet).
   
   \sa SUMA_PrependToROIdatum
*/
SUMA_Boolean SUMA_AppendToROIdatum (SUMA_ROI_DATUM *ROId1, SUMA_ROI_DATUM *ROId2)
{
   static char FuncName[]={"SUMA_AppendToROIdatum"};
   int i, N_nNew=-1, N_tNew=-1, *tPathNew=NULL, *nPathNew=NULL;
   SUMA_Boolean CommonTip = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!ROId1) SUMA_RETURN(YUP);
   if (!ROId1->N_n)  SUMA_RETURN(YUP);
   if (!ROId2) {
      fprintf (SUMA_STDERR, "Error %s: NULL ROId2.\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   /* make sure the last node of ROId2 and the first node of ROId1 match */
   if (ROId2->N_n) {
      if (ROId1->nPath[0] != ROId2->nPath[ROId2->N_n-1]) {
         fprintf (SUMA_STDERR, "Error %s: Last node of ROId2 is not the same as the first node of ROId1.\n", FuncName);
         SUMA_RETURN(NOPE);
      }
   }
   /* now merge the two */
   
   /* FIRST the nodes */
   /* figure out the new N_n */
   N_nNew = ROId1->N_n + ROId2->N_n -1;
   
   /* create a new nPath pointer */
   nPathNew = (int *)SUMA_calloc (N_nNew, sizeof (int));
   if (!nPathNew) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate. \n", FuncName);
      SUMA_RETURN(NOPE);
   }
   
   for (i=0; i<ROId2->N_n; ++i) nPathNew[i] = ROId2->nPath[i];
   for (i=1; i<ROId1->N_n; ++i) nPathNew[ROId2->N_n+i-1] = ROId1->nPath[i];
   SUMA_free(ROId2->nPath);
   ROId2->nPath = nPathNew;
   ROId2->N_n = N_nNew;
   
   /* SECOND THE triangles */
   CommonTip = NOPE;
   if (!ROId1->tPath || !ROId1->N_t) {
      /* nothing to do */
      ROId2->tPath = NULL;
      ROId2->N_t = 0;
      SUMA_RETURN(YUP);
   }else{
      /* do the strips have a common triangle at the end ? */
      if (ROId2->N_t) {
         if (ROId1->tPath[0] == ROId2->tPath[ROId2->N_t-1]) CommonTip = YUP;
      }
   }
   if (CommonTip) {
      /* figure out the new N_n */
      N_tNew = ROId1->N_t + ROId2->N_t -1;

      /* create a new tPath pointer */
      tPathNew = (int *)SUMA_calloc (N_tNew, sizeof (int));
      if (!tPathNew) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate. \n", FuncName);
         SUMA_RETURN(NOPE);
      }
      for (i=0; i<ROId2->N_t; ++i) tPathNew[i] = ROId2->tPath[i];
      for (i=1; i<ROId1->N_t; ++i) tPathNew[ROId2->N_t+i-1] = ROId1->tPath[i];
      SUMA_free(ROId2->tPath);
   }else {
      /* figure out the new N_n */
      N_tNew = ROId1->N_t + ROId2->N_t;

      /* create a new tPath pointer */
      tPathNew = (int *)SUMA_calloc (N_tNew, sizeof (int));
      if (!tPathNew) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate. \n", FuncName);
         SUMA_RETURN(NOPE);
      }
      for (i=0; i<ROId2->N_t; ++i) tPathNew[i] = ROId2->tPath[i];
      for (i=0; i<ROId1->N_t; ++i) tPathNew[ROId2->N_t+i] = ROId1->tPath[i];
      SUMA_free(ROId2->tPath);
   }
   ROId2->tPath = tPathNew;
   ROId2->N_t = N_tNew;

   
   SUMA_RETURN(YUP);
}

/*!
   \brief Merges two ROIdatum together. 
   ans = SUMA_PrependToROIdatum (ROIlink, ROId);
   ROId = [ROIlink ROId]
   
   \param ROIlink (SUMA_ROI_DATUM *)
   \param ROId (SUMA_ROI_DATUM *)
   \return ans YUP/NOPE
   
   - ROId becomes ROIlink followed by ROId
   - ROIlink is not freed
   - It is required that the last node of ROIlink be the first node of ROId.
   - This is not required for the tPath (the triangle path). By the same token,
   it is not guaranteed that the resultant tPath is contiguous (not yet).
   
   \sa SUMA_AppendToROIdatum
*/
SUMA_Boolean SUMA_PrependToROIdatum (SUMA_ROI_DATUM *ROId1, SUMA_ROI_DATUM *ROId2)
{
   static char FuncName[]={"SUMA_PrependToROIdatum"};
   int i, N_nNew=-1, N_tNew=-1, *tPathNew=NULL, *nPathNew=NULL;
   SUMA_Boolean CommonTip = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!ROId1) SUMA_RETURN(YUP);
   if (!ROId1->N_n)  SUMA_RETURN(YUP);
   if (!ROId2) {
      fprintf (SUMA_STDERR, "Error %s: NULL ROId2.\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   /* make sure the last node of ROId1 and the first node of ROId2 match */
   if (ROId2->N_n) {
      if (ROId1->nPath[ROId1->N_n-1] != ROId2->nPath[0]) {
         fprintf (SUMA_STDERR, "Error %s: Last node of ROId1 is not the same as the first node of ROId2.\n", FuncName);
         SUMA_RETURN(NOPE);
      }
   }
   /* now merge the two */
   
   /* FIRST the nodes */
   /* figure out the new N_n */
   N_nNew = ROId1->N_n + ROId2->N_n -1;
   
   /* create a new nPath pointer */
   nPathNew = (int *)SUMA_calloc (N_nNew, sizeof (int));
   if (!nPathNew) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate. \n", FuncName);
      SUMA_RETURN(NOPE);
   }
   for (i=0; i<ROId1->N_n; ++i) nPathNew[i] = ROId1->nPath[i];
   for (i=1; i<ROId2->N_n; ++i) nPathNew[ROId1->N_n+i-1] = ROId2->nPath[i];
   SUMA_free(ROId2->nPath);
   ROId2->nPath = nPathNew;
   ROId2->N_n = N_nNew;
   
   /* SECOND THE triangles */
   CommonTip = NOPE;
   if (!ROId1->tPath || !ROId1->N_t) {
      /* nothing to do */
      ROId2->tPath = NULL;
      ROId2->N_t = 0;
      SUMA_RETURN(YUP);
   }else{
      /* do the strips have a common triangle at the end ? */
      if (ROId2->N_t) {
         if (ROId1->tPath[ROId1->N_t-1] == ROId2->tPath[0]) CommonTip = YUP;
      }
   }
   if (CommonTip) {
      /* figure out the new N_n */
      N_tNew = ROId1->N_t + ROId2->N_t -1;

      /* create a new tPath pointer */
      tPathNew = (int *)SUMA_calloc (N_tNew, sizeof (int));
      if (!tPathNew) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate. \n", FuncName);
         SUMA_RETURN(NOPE);
      }
      for (i=0; i<ROId1->N_t; ++i) tPathNew[i] = ROId1->tPath[i];
      for (i=1; i<ROId2->N_t; ++i) tPathNew[ROId1->N_t+i-1] = ROId2->tPath[i];
      SUMA_free(ROId2->tPath);
   }else {
      /* figure out the new N_n */
      N_tNew = ROId1->N_t + ROId2->N_t;

      /* create a new tPath pointer */
      tPathNew = (int *)SUMA_calloc (N_tNew, sizeof (int));
      if (!tPathNew) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate. \n", FuncName);
         SUMA_RETURN(NOPE);
      }
      for (i=0; i<ROId1->N_t; ++i) tPathNew[i] = ROId1->tPath[i];
      for (i=0; i<ROId2->N_t; ++i) tPathNew[ROId1->N_t+i] = ROId2->tPath[i];
      SUMA_free(ROId2->tPath);
   }
   ROId2->tPath = tPathNew;
   ROId2->N_t = N_tNew;
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Show contents of a drawn ROI datum
   SUMA_ShowDrawnROIDatum (ROId, Out, ShortVersion);
   
   \param ROId (SUMA_ROI_DATUM *) 
   \param Out (FILE *) (stderr if NULL)
   \param ShortVersion (SUMA_Boolean) if YUP, short version

*/
void SUMA_ShowDrawnROIDatum (SUMA_ROI_DATUM *ROId, FILE *out, SUMA_Boolean ShortVersion)
{
   static char FuncName[]={"SUMA_ShowDrawnROIDatum"};
   int i;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!out) out = SUMA_STDERR;
   
   if (!ROId) {
      fprintf(out, "%s: NULL ROId\n", FuncName);
      SUMA_RETURNe;
   }
   
   if (!ROId->N_n) {
      fprintf(out, "%s: Empty ROId. (N_n = 0)\n", FuncName);
      SUMA_RETURNe;
   }
   
   if (ROId->N_n && !ROId->nPath) {
      fprintf(out, "Error %s: nPath is NULL with N_n != 0.\n", FuncName);
      SUMA_RETURNe;
   }
   
   if (ROId->N_n == 1) {
      fprintf(out, "%s: ROId (type %d) has 1 node (%d) in nPath.\n", 
         FuncName, ROId->Type, ROId->nPath[0]);
   }else {
      fprintf(out, "%s: ROId (type %d) has %d nodes in nPath [%d..%d].\n", 
         FuncName, ROId->Type, ROId->N_n, ROId->nPath[0], ROId->nPath[ROId->N_n-1]);
      if (!ShortVersion) {
         for (i=0; i <ROId->N_n; ++i) fprintf (out, "%d: %d\t", i, ROId->nPath[i]);
         fprintf (out, "\n");
      }
   }
   
   if (ROId->N_t && !ROId->tPath) {
      fprintf(out, "Error %s: tPath is NULL with N_t != 0.\n", FuncName);
      SUMA_RETURNe;
   }
   
   if (!ROId->N_t) {
      fprintf(out, "%s: Empty ROId->tPath. (N_t = 0)\n", FuncName);
      SUMA_RETURNe;
   }else {
         if (ROId->N_t == 1) {
            fprintf(out, "%s: ROId (type %d) has 1 triangle (%d) in tPath.\n", 
               FuncName, ROId->Type, ROId->tPath[0]);
         }else {
            fprintf(out, "%s: ROId (type %d) has %d triangles in tPath [%d..%d].\n", 
               FuncName, ROId->Type, ROId->N_t, ROId->tPath[0], ROId->tPath[ROId->N_t-1]);
            if (!ShortVersion) {
               for (i=0; i <ROId->N_t; ++i) fprintf (out, "%d: %d\t", i, ROId->tPath[i]);
               fprintf (out, "\n");
            }
         }
   }
   
   SUMA_RETURNe;
}

/*!
   \brief Show contents of a drawn ROI
   SUMA_ShowDrawnROI (ROI, Out, ShortVersion);
   
   \param ROId (SUMA_DRAWN_ROI *) 
   \param Out (FILE *) (stderr if NULL)
   \param ShortVersion (SUMA_Boolean) if YUP, short version

*/
void SUMA_ShowDrawnROI (SUMA_DRAWN_ROI *D_ROI, FILE *out, SUMA_Boolean ShortVersion)
{
   static char FuncName[]={"SUMA_ShowDrawnROI"};
   int i;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!out) out = SUMA_STDERR;

   fprintf(out, "--------------------------------------------\n");
   
   if (!D_ROI) {
      fprintf(out, "%s: NULL D_ROI\n", FuncName);
      SUMA_RETURNe;
   }
   
   fprintf(out, "%s: ROI Label %s, Type %d, DrawStatus %d\n Idcode %s, Parent Idcode %s\n", 
         FuncName, D_ROI->Label, D_ROI->Type, D_ROI->DrawStatus, D_ROI->idcode_str, D_ROI->Parent_idcode_str );
   
   if (D_ROI->ActionStack) {
      fprintf (out, "%s: There are %d actions in the ActionStack.\n", FuncName, dlist_size(D_ROI->ActionStack));
   }else {
      fprintf (out, "%s: ActionStack is NULL.\n", FuncName);
   }
   
   if (!D_ROI->ROIstrokelist) {
      fprintf(out, "%s: NULL ROIstrokelist.\n", FuncName);
      SUMA_RETURNe;
   }
   

   if (!dlist_size(D_ROI->ROIstrokelist)) {
      fprintf(out, "%s: ROIstrokelist is empty.\n", FuncName);
   } else {
      DListElmt *NextElm=NULL;
      int cnt = 0;
      fprintf(out, "%s: ROIstrokelist has %d elements.\n", FuncName, dlist_size(D_ROI->ROIstrokelist));
      do {
         
         if (!NextElm) NextElm = dlist_head(D_ROI->ROIstrokelist);
         else NextElm = dlist_next(NextElm);
         ++cnt;
         fprintf(out, "%d\t+++++++++++\n", cnt);
         SUMA_ShowDrawnROIDatum ((SUMA_ROI_DATUM *)NextElm->data, out, ShortVersion);
      } while (NextElm != dlist_tail(D_ROI->ROIstrokelist));
   }
   
   fprintf(out, "--------------------------------------------\n");
   
   SUMA_RETURNe;
}

/*!
   \brief SUMA_FillToMask_Engine (FN, Visited, Mask, seed, N_Visited);
   the recursive function for SUMA_FillToMask.
   Do not use logging functions here.
*/

void SUMA_FillToMask_Engine (SUMA_NODE_FIRST_NEIGHB *FN, int *Visited, int *ROI_Mask, int nseed, int *N_Visited)
{  
   int i, nnext;
   
   Visited[nseed] = 1;
   ++*N_Visited;
   for (i=0; i<FN->N_Neighb[nseed]; ++i) {
      nnext = FN->FirstNeighb[nseed][i];
      if (!Visited[nnext] && !ROI_Mask[nnext]) SUMA_FillToMask_Engine(FN, Visited, ROI_Mask, nnext, N_Visited);
   }

   return;
}
/*!
\brief Returns the ROI formed by connected nodes that are bound by Mask
      ROIfill = SUMA_FillToMask (SO, ROI_Mask, FirstSurfNode);
      
\param SO (SUMA_SurfaceObject *)
\param ROI_Mask (int *) if (ROI_Mask[n]) then node n is a boundary
\param FirstSurfNode (int) node index from which the fill begins.
       
\return ROIfill (SUMA_ROI_DATUM *) of the type SUMA_ROI_NodeGroup
\sa SUMA_FillToMask_Engine
*/
SUMA_ROI_DATUM * SUMA_FillToMask(SUMA_SurfaceObject *SO, int *ROI_Mask, int nseed) 
{
   static char FuncName[]={"SUMA_FillToMask"};
   SUMA_ROI_DATUM *ROIfill = NULL;
   int *Visited = NULL;
   int N_Visited = 0, i, nnext;
   SUMA_Boolean LocalHead = NOPE;
   
   /* register at the first call only */
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!ROI_Mask) {
      SUMA_S_Err("NULL Mask.");
      SUMA_RETURN(NULL);
   }
   
   /* make sure your seed is not on the edge */
   if (ROI_Mask[nseed]) {
      SUMA_S_Err("seed is on the edge.");
      SUMA_RETURN(NULL);
   }

   if (!Visited) { /* allocate */
      Visited = (int *)SUMA_calloc (SO->N_Node, sizeof (int));
      if (!Visited) {
         SUMA_S_Err("Could not allocate for Visited.");
         SUMA_RETURN(NULL);
      }
   }
   
   N_Visited = 0;
   SUMA_FillToMask_Engine (SO->FN, Visited, ROI_Mask, nseed, &N_Visited);
   

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Found %d nodes to fill.\n", FuncName, N_Visited);
      
   ROIfill = SUMA_AllocROIDatum();
   ROIfill->Type = SUMA_ROI_NodeGroup;
   
   /* Now put the nodes in the path */
   ROIfill->N_n = N_Visited;
   ROIfill->nPath = (int *)SUMA_calloc (ROIfill->N_n, sizeof(int));
   if (!ROIfill->nPath) {
      SUMA_S_Err("Could not allocate for nPath.\n");
      if (Visited) SUMA_free(Visited);
      SUMA_RETURN(NULL);
   }
   
   N_Visited = 0;
   for (i=0; i<SO->N_Node; ++i) {
      if (Visited[i]) {
         ROIfill->nPath[N_Visited] = i;
         ++N_Visited;
      }
   }
   
   if (Visited) SUMA_free(Visited);
   SUMA_RETURN(ROIfill);
}

