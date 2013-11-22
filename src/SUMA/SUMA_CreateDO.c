/*! Functions to create Displayable Objects */
#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;
extern int SUMAg_N_DOv;

/* This macro is used to decide if displayable objects on a node 'n' are to be
   drawn. For now, this condition is applied to certain objects only. T
   The NIDO functions do not call on this macro yet.
   The problem is that NIDO functions draw one object at a time and you don't 
   want to form the node mask in SUMA_ProcessDODrawMask each time a DO is to be
   drawn. The byte mask should probably be formed at the level of main drawing 
   routine and freed afterwards. That also means that a subject's ID might need
   to be checked, in addition to the node numbers. But that should not be too 
   bad. */
#define DO_DRAW(mask, n, nc) (((nc == n || (nc < 0 && (!mask || mask[n]))))?1:0)
int SUMA_ProcessDODrawMask(SUMA_SurfaceViewer *sv,          
                                    SUMA_SurfaceObject *SO,
                                    byte **mask, int *ncross) 
{
   static char FuncName[]={"SUMA_ProcessDODrawMask"};
   int N_inmask=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv || !SO || !mask || !ncross) SUMA_RETURN(-1);
   if (*mask) {
      SUMA_S_Err("Must send me a null mask pointer");
      SUMA_RETURN(-1);
   }
   *ncross = -1;
   if (sv->DO_DrawMask != SDODM_All) { 
      /* Masking required, make sure surface qualifies */
      if (!(sv->Ch && iDO_isSO(sv->Ch->adoID))) {
         SUMA_LH("Cross hair not initialized yet, or not on surface");
         SUMA_RETURN (0);
      }
      if (SO!=(SUMA_SurfaceObject *)(SUMAg_DOv[sv->Ch->adoID].OP)) {
         /* Not the surface on which the cross hair lives, get out */
         SUMA_RETURN (0);
      }
      if (sv->Ch->datumID < 0 || sv->Ch->datumID >= SO->N_Node) {
         SUMA_S_Errv("Bad crosshair node %d on %s. This should not happen.\n",
                     sv->Ch->datumID, SO->Label);
         SUMA_RETURN (-1);
      }
   }
   switch (sv->DO_DrawMask) {
      case SDODM_n0CrossHair:
      case SDODM_n1CrossHair:
      case SDODM_n2CrossHair:
      case SDODM_n3CrossHair:
         if (sv->DO_DrawMask != SDODM_n0CrossHair) {
            /* get neighbors */
            if (!(*mask = SUMA_NodeNeighborMask(SO, sv->Ch->datumID, 
                                         SDODM_n0CrossHair-sv->DO_DrawMask, 
                                         &N_inmask))) {
               SUMA_S_Warnv("Failed to get neighborhood for node %d\n"
                            "Proceeding...\n", sv->Ch->datumID);
               *ncross = sv->Ch->datumID; /* show something at least */
               N_inmask = 1;
            } else {
               *(*mask+sv->Ch->datumID) = 1; ++N_inmask;
            }
         } else {
            *ncross = sv->Ch->datumID; /* just this node */
            N_inmask = 1;
         }
         break;
      case SDODM_All:
         N_inmask = SO->N_Node;
         break;
      case SDODM_Hide:
         N_inmask = 0;
         break;
      default:
         SUMA_S_Err("Bad draw mask mode");
         SUMA_RETURN (-1);
         break;  
   }       
   SUMA_LHv("Got ncross=%d. %d node objects to draw\n"
            , *ncross, N_inmask );
   SUMA_RETURN(N_inmask);
}

SUMA_NEW_SO_OPT *SUMA_NewNewSOOpt(void)
{
   static char FuncName[]={"SUMA_NewNewSOOpt"};
   SUMA_NEW_SO_OPT *nsoopt=NULL;
   SUMA_ENTRY;
   
   nsoopt = (SUMA_NEW_SO_OPT *) SUMA_calloc(1,sizeof(SUMA_NEW_SO_OPT));
   nsoopt->idcode_str = NULL;
   nsoopt->LocalDomainParent = SUMA_copy_string("SAME");
   nsoopt->LocalDomainParentID = NULL;
   nsoopt->FileFormat = SUMA_ASCII;
   nsoopt->FileType = SUMA_FT_NOT_SPECIFIED;
   nsoopt->DoMetrics = YUP;
   nsoopt->DoNormals = YUP;
   nsoopt->DoCenter = YUP;
   nsoopt->LargestBoxSize = -1.0;
   SUMA_RETURN(nsoopt);
}

SUMA_NEW_SO_OPT *SUMA_FreeNewSOOpt(SUMA_NEW_SO_OPT *nsopt)
{
   static char FuncName[]={"SUMA_FreeNewSOOpt"};
   SUMA_ENTRY;
   
   if (!nsopt) SUMA_RETURN(NULL);
   if (nsopt->idcode_str) SUMA_free(nsopt->idcode_str);
   if (nsopt->LocalDomainParentID) SUMA_free(nsopt->LocalDomainParentID);
   if (nsopt->LocalDomainParent) SUMA_free(nsopt->LocalDomainParent);
   SUMA_free(nsopt);
   
   SUMA_RETURN(NULL);
}

/*!
   Creates a surface object and its normals and edge list from a list of Nodes and triangles.
   NodeListp (float **) pointer to nodelist. 
                        This points to the vector of node coordinates.
                        The copy into SO is done by pointer and *NodeListp 
                        is set to NULL
                        to keep users on the straight and narrow.
   N_Node (int) number of nodes
   
   FaceSetList (int **) pointer to facesetlist. Assumes triangular mesh
   N_FaceSet (int) number of triangles
   nsooptu (SUMA_NEW_SO_OPT *) an options structure to dictate what to do with certain
                     fields of SO. At the moment, just pass NULL.
*/
SUMA_SurfaceObject *SUMA_NewSO ( float **NodeList, int N_Node, 
                                 int **FaceSetList, int N_FaceSet,
                                 SUMA_NEW_SO_OPT *nsooptu)
{
   static char FuncName[]={"SUMA_NewSO"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_NEW_SO_OPT *nsoopt=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nsooptu) {
      nsoopt = SUMA_NewNewSOOpt();
   } else {
      nsoopt = nsooptu;
   }
   
   SO = SUMA_Alloc_SurfObject_Struct(1); 
   
   SO->FileFormat = nsoopt->FileFormat;
   SO->FileType = nsoopt->FileType;

   SUMA_LH("NodeList");
   SO->NodeDim = 3;
   SO->NodeList = *NodeList; *NodeList = NULL;  /* keeps user from 
                                                   freeing afterwards ... */
   SO->N_Node = N_Node;

   if (nsoopt->DoCenter) {
      SUMA_LH("Center deal")
      SUMA_DIM_CENTER(SO);
   } else {
      SUMA_LH("Skipping Center deal")
   } 

   if (nsoopt->LargestBoxSize > 0.0) {
      SUMA_LH("BoxSize deal")
      SUMA_LARGEST_SIZE_SCALE(SO, nsoopt->LargestBoxSize);
   } else {
      SUMA_LH("Skipping BoxSize deal")
   }
    
   SUMA_LH("FaceSetList");
   SO->FaceSetDim = 3;
   SO->FaceSetList = *FaceSetList; *FaceSetList = NULL;  /* keeps user from 
                                                            freeing later ... */
   SO->N_FaceSet = N_FaceSet;
   
   if (nsoopt->DoMetrics) {
      SUMA_LH("Metrics");
      if (!SUMA_SurfaceMetrics_eng( SO, "EdgeList, MemberFace", 
                                    NULL, 0, SUMAg_CF->DsetList)) {
         SUMA_SL_Warn(  "Failed to compute metrics\n"
                        "Returing with whatever is salvageable");
      }
   } else {
      SUMA_LH("Skipping metrics");
   }      
   if (nsoopt->DoNormals) {
      SUMA_LH("Normals");
      SUMA_RECOMPUTE_NORMALS(SO);
   } else {
      SUMA_LH("Skipping normals");
   }
   SUMA_LH("trimmings");
   SO->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));  
   if (nsoopt->idcode_str) sprintf(SO->idcode_str, "%s", nsoopt->idcode_str);
   else UNIQ_idcode_fill (SO->idcode_str);
   if (nsoopt->LocalDomainParentID) 
      SO->LocalDomainParentID = SUMA_copy_string(nsoopt->LocalDomainParentID);
   else SO->LocalDomainParentID = SUMA_copy_string(SO->idcode_str);
   if (nsoopt->LocalDomainParent) 
      SO->LocalDomainParent = SUMA_copy_string(nsoopt->LocalDomainParent);
   else SO->LocalDomainParent = SUMA_copy_string("SAME");
   
   /* the stupid copies */
   if (sizeof(GLfloat) != sizeof(float)) { 
      SUMA_SL_Crit("GLfloat and float have differing sizes!\n"); 
      SUMA_RETURN(NOPE); }
   if (sizeof(GLint) != sizeof(int)) { 
      SUMA_SL_Crit("GLint and int have differing sizes!\n"); 
      SUMA_RETURN(NOPE); }

   SO->glar_NodeList = (GLfloat *)SO->NodeList;
   SO->glar_FaceSetList = (GLint *) SO->FaceSetList;
   SO->glar_NodeNormList = (GLfloat *) SO->NodeNormList; 
   SO->glar_FaceNormList = (GLfloat *) SO->FaceNormList; 
   
   if (LocalHead) SUMA_Print_Surface_Object(SO, NULL);
   
   if (nsooptu != nsoopt) {
      nsoopt=SUMA_FreeNewSOOpt(nsoopt); 
   }
   
   SUMA_RETURN(SO);
}

/*!
   \brief A function to create a surface that is a child of another.
           function can also be used to replace NodeList and/or 
           FaceSetList in the same SurfaceObject
   \param SO (SUMA_SurfaceObject *) 
   \param NodeList (float *): list of node coordinates, SO->NodeDim*N_Node elements long
   \param N_Node (int) : number of nodes in NodeList
   \param FaceSetList (int*): list of facesets, SO->FaceSetDim*N_FaceSet element long
   \param N_FaceSet (int): number of facesets
   \param replace: (SUMA_Boolean)   YUP = replace NodeList and/or FaceSetList in SO and return SO. 
                                    NOPE = return a new SurfaceObject, 
   \sa SUMA_NewSO
DO NOT FREE NodeList and/or FaceSetList upon returning
*/
SUMA_SurfaceObject *SUMA_CreateChildSO(SUMA_SurfaceObject * SO, 
                                       float *NodeList, int N_Node, 
                                       int *FaceSetList, int N_FaceSet,
                                       SUMA_Boolean replace)
{
   static char FuncName[]={"SUMA_CreateChildSO"};
   SUMA_SurfaceObject *SOn=NULL;
   SUMA_Boolean RedoNormals = NOPE, RedoFaces = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SO) SUMA_RETURN(NULL);
   if (!NodeList && !FaceSetList && replace ) { 
      SUMA_SL_Err("Nothing to do"); 
   }

   if (NodeList) { 
      if (N_Node != SO->N_Node) {
         SUMA_SL_Err("Not ready for partial node lists.\n");
         SUMA_RETURN(NULL);
      }  
   }

   if (replace) { SUMA_LH("Reusing old surface"); SOn = SO; }
   else { SUMA_LH("New Surface"); SOn =  SUMA_Alloc_SurfObject_Struct(1); } 

   if (NodeList) {
      if (!replace) {
         SUMA_LH("New Node List");
      } else {
         SUMA_LH("Freeing old node list and setting new one ");
         if (SOn->NodeList) SUMA_free(SOn->NodeList);
      }
      SOn->NodeDim = SO->NodeDim;
      SOn->NodeList = NodeList; SOn->N_Node = N_Node;
      SUMA_LH("Recalculating center");
      SUMA_DIM_CENTER(SOn);
      RedoNormals = YUP;
   } else {
      if (!replace) {
         if (SOn->NodeList) {
            SUMA_S_Err("Should not be here");
            SUMA_RETURN(NULL);
         }
         SUMA_LH("Copying old node list");
         SOn->NodeDim = SO->NodeDim;
         SOn->N_Node = SO->N_Node;
         SOn->NodeList = (float *)SUMA_malloc(SOn->N_Node*3*sizeof(float));
         if (!SOn->NodeList) { 
            SUMA_SL_Crit("Failed to allocate."); SUMA_RETURN(NULL); 
         }
         SUMA_COPY_VEC(SO->NodeList, SOn->NodeList, SOn->N_Node*3, float, float);
         RedoNormals = YUP;
      }
   }
 
   if (FaceSetList) {
      SUMA_LH("New FaceSet List");
      if (SOn->FaceSetList) {
         SUMA_S_Err("Should not be here");
         SUMA_RETURN(NULL);
      }
      SOn->FaceSetList = FaceSetList; 
      SOn->N_FaceSet = N_FaceSet; 
      SOn->FaceSetDim = SO->FaceSetDim;
      /* Need a new edge list */
      if (!SUMA_SurfaceMetrics(SOn, "EdgeList, MemberFace", NULL)) {
         SUMA_SL_Warn(  "Failed to compute metrics\n"
                        "Returing with whatever is salvageable");
      }
      RedoNormals = YUP;
   } else {
      if (!replace) {
         if (SOn->FaceSetList) {
                  SUMA_S_Err("Should not be here");
                  SUMA_RETURN(NULL);
         }         
         SUMA_LH("Copying old FaceSet list");
         SOn->N_FaceSet = SO->N_FaceSet;
         SOn->FaceSetDim = SO->FaceSetDim;
         SOn->FaceSetList = 
            (int *)SUMA_malloc(SOn->N_FaceSet*SOn->FaceSetDim*sizeof(int));
         if (!SOn->FaceSetList) { 
            SUMA_SL_Crit("Failed to allocate."); SUMA_RETURN(NULL); 
         }
         SUMA_COPY_VEC( SO->FaceSetList, SOn->FaceSetList, 
                        SOn->N_FaceSet*SOn->FaceSetDim, int, int);
         RedoNormals = YUP;
         /* Need to inherit edge list */
         if (0 &&!SUMA_SurfaceMetrics(SOn, "EdgeList, MemberFace", SO)) {
            SUMA_SL_Warn(  "Failed to compute metrics\n"
                           "Returing with whatever is salvageable");
         }
      }
   }

   if (RedoNormals) {
      SUMA_LH("Recalculating normals and convexitation");
      SUMA_RECOMPUTE_NORMALS(SOn);
      if (0 &&!SUMA_SurfaceMetrics(SOn, "Convexity", SO)) {
         SUMA_SL_Warn("Failed to compute metrics\n"
                      "Returing with whatever is salvageable");
      }
   }

   if (!replace) {
      SUMA_LH("New IDs");
      SOn->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));  
      UNIQ_idcode_fill (SOn->idcode_str);
      SOn->LocalDomainParentID = SUMA_copy_string(SO->LocalDomainParentID);
      SOn->Label = SUMA_append_string("cp.", SO->Label);
   }

   /* the stupid copies */
   
   SOn->glar_NodeList = (GLfloat *)SOn->NodeList;
   SOn->glar_NodeNormList = (GLfloat *)SOn->NodeNormList;
   SOn->glar_FaceSetList = (GLint *) SOn->FaceSetList;
   SOn->glar_FaceNormList = (GLfloat *) SOn->FaceNormList;
   
   SUMA_RETURN(SOn);
}
SUMA_SurfaceObject *SUMA_Cmap_To_SO (
   SUMA_COLOR_MAP *Cmap, float orig[3], float topright[3], int verb)
{
   static char FuncName[]={"SUMA_Cmap_To_SO"};
   SUMA_SurfaceObject *SO= NULL;
   int i, i3, i4, in, k;
   float dh, dw, *hp = NULL;
   SUMA_SURF_NORM SN;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Allocating surface.");
   SO = SUMA_Alloc_SurfObject_Struct(1);
   if (!Cmap) { SUMA_SL_Err("No Cmap"); SUMA_RETURN(NULL); }
   if (!Cmap->N_M[0]) { SUMA_SL_Err("No colours"); SUMA_RETURN(NULL); }
   if (!SO) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL); }
   
   #if 0
   if (Cmap->frac) { 
      /* DEBUG ONLY */
      verb = 2;
      SUMA_SL_Note("DEBUG ONLY, Leaky approach .. ");
      Cmap = SUMA_Linearize_Color_Map(Cmap, -1);
   }
   #endif
   
   /* scaling factors */
   dh = (topright[1] - orig[1]) / Cmap->N_M[0];
   hp = (float *)SUMA_calloc(Cmap->N_M[0]+1, sizeof(float));
   if (!hp) {
      SUMA_SL_Crit("malloc error");
      SUMA_RETURN(NULL);
   }
   hp[0] = 0.0;
   for (i=0; i<Cmap->N_M[0]; ++i) {
      if (Cmap->frac) {
         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: icol %d, frac=%f\n", 
               FuncName, i,  Cmap->frac[i]);
         if (Cmap->Sgn >= 0) {
            hp[i+1] = Cmap->frac[i]/Cmap->frac[Cmap->N_M[0]-1] * 
                        (topright[1] - orig[1]);
         } else {
            hp[i+1] = (1.0 +Cmap->frac[i]/Cmap->frac[Cmap->N_M[0]-1]) / 2.0 * 
                        (topright[1] - orig[1]);
         }
      } else hp[i+1] = hp[i]+dh;
   }
   
   dw = topright[0] - orig[0];
   if (dh <= 0 || dw <= 0) {
      SUMA_SL_Err("Whatchyoutalkinaboutwillis?");
      SUMA_RETURN(NULL);
   }
   
   SUMA_LH("Allocating surface elements.");
   /* allocate for the NodeList */
   SO->FileType = SUMA_CMAP_SO;
   SO->N_Node = 4 * (Cmap->N_M[0]);
   SO->N_FaceSet = 2 * Cmap->N_M[0];
   SO->NodeDim = 3;
   SO->EmbedDim = 2;
   SO->FaceSetDim = 3;
   SO->idcode_str = (char *)SUMA_calloc(SUMA_IDCODE_LENGTH, sizeof(char));
   SO->NodeList = (float *)SUMA_calloc(SO->N_Node * 3, sizeof(float));
   SO->FaceSetList = (int *)SUMA_calloc(SO->N_FaceSet * 3,  sizeof(int));
   SO->PermCol = (GLfloat *)SUMA_calloc(SO->N_Node * 4, sizeof(GLfloat));
   if (!SO->idcode_str || !SO->NodeList || !SO->FaceSetList || !SO->PermCol) { 
      SUMA_SL_Crit("Failed to allocate"); 
      SUMA_RETURN(NULL);
   }
   
   SUMA_LH("Filling up surface id.");
   /* fill up idcode*/
   UNIQ_idcode_fill(SO->idcode_str);
   
   SUMA_LH("Filling up surface nodelist.");
   /* fill up coordinates first */
   i=0;     /* color index */
   in = 0;  /* node index */
   i3 = in * 3;
   while (i < Cmap->N_M[0]) {
      /* 4 nodes per color */
      SO->NodeList[i3] =    orig[0]; 
      SO->NodeList[i3+1] = hp[i]   + orig[1]; 
      SO->NodeList[i3+2] = 0.0; 
               ++in; i3 = in * 3;
      SO->NodeList[i3] = dw+orig[0]; 
      SO->NodeList[i3+1] = hp[i]   + orig[1]; 
      SO->NodeList[i3+2] = 0.0; 
               ++in; i3 = in * 3;
      SO->NodeList[i3] = dw+orig[0]; 
      SO->NodeList[i3+1] = hp[i+1] + orig[1]; 
      SO->NodeList[i3+2] = 0.0; 
               ++in; i3 = in * 3;
      SO->NodeList[i3] =    orig[0]; 
      SO->NodeList[i3+1] = hp[i+1] + orig[1]; 
      SO->NodeList[i3+2] = 0.0; 
               ++in; i3 = in * 3;
      ++i;
   }
   
   SUMA_LH("Bounding Box");
   /* Calculate Min, Max, Mean */
   SUMA_MIN_MAX_SUM_VECMAT_COL ( SO->NodeList, SO->N_Node, SO->NodeDim, 
                                 SO->MinDims, SO->MaxDims, SO->Center);
     
   SO->Center[0] /= SO->N_Node;
   SO->Center[1] /= SO->N_Node;
   SO->Center[2] /= SO->N_Node;

   SUMA_MIN_VEC (SO->MinDims, 3, SO->aMinDims );
   SUMA_MAX_VEC (SO->MaxDims, 3, SO->aMaxDims);
   
   SUMA_LH("Filling up surface facesetlist.");
   /* fill up triangles */
   i = 0;   /* color index */
   i4 = 4*i;
   in = 0;  /* triangle index */
   i3 = in *3;
   while (i < Cmap->N_M[0]) {
      /* 2 triangles per color, 4 nodes per color*/
      SO->FaceSetList[i3] = i4; 
      SO->FaceSetList[i3+1] = i4+1; 
      SO->FaceSetList[i3+2] = i4+2; 
         ++in; i3 = in *3;
      SO->FaceSetList[i3] = i4; 
      SO->FaceSetList[i3+1] = i4+2; 
      SO->FaceSetList[i3+2] = i4+3; 
         ++in; i3 = in *3;; 
      ++i; i4 = 4*i;
   }
   
   SUMA_LH("Filling up surface colors.");
   /* fill up the color vector */
   i=0; /* color index */
   in = 0;  /* node index */
   i4 = in * 4;
   while (i < Cmap->N_M[0]) {
      for (k=0; k<4; ++k) {
         /* 4 nodes per color */
         SO->PermCol[i4]   = Cmap->M[i][0]; 
         SO->PermCol[i4+1] = Cmap->M[i][1]; 
         SO->PermCol[i4+2] = Cmap->M[i][2]; 
         if (Cmap->N_M[1] == 4) {
            SO->PermCol[i4+3] = Cmap->M[i][3]; 
         } else {
            SO->PermCol[i4+3] = 1.0;  
         }
         ++in; i4 = in * 4;
      }
      ++i;
   } 
   
   /* if verb, write out the results for checking */
   if (verb > 1) {
      char *fname;
      FILE *fout;
      
      SUMA_LH("writing out surface.");

      fname = SUMA_append_string(Cmap->Name, ".1D.NodeList");
      if (!fname) { SUMA_SL_Err("Failed to create name"); SUMA_RETURN(SO); }
      fout = fopen(fname,"w"); 
      if (fout) {
         for (i=0; i < SO->N_Node; ++i) 
            fprintf (fout,"%f %f %f\n", SO->NodeList[3*i], 
                        SO->NodeList[3*i+1], SO->NodeList[3*i+2]);
         fclose(fout); SUMA_free(fname); fname = NULL;
      } else { SUMA_SL_Err("Failed to write NodeList"); SUMA_RETURN(SO); }
      
      fname = SUMA_append_string(Cmap->Name, ".1D.FaceSetList");
      if (!fname) { SUMA_SL_Err("Failed to create name"); SUMA_RETURN(SO); }
      fout = fopen(fname,"w"); 
      if (fout) {
         for (i=0; i < SO->N_FaceSet; ++i) 
            fprintf (fout,"%d %d %d\n", SO->FaceSetList[3*i], 
                        SO->FaceSetList[3*i+1], SO->FaceSetList[3*i+2]);
         fclose(fout); SUMA_free(fname); fname = NULL;
      } else { SUMA_SL_Err("Failed to write FaceSetList"); SUMA_RETURN(SO); }
      
      fname = SUMA_append_string(Cmap->Name, ".1D.col");
      if (!fname) { SUMA_SL_Err("Failed to create name"); SUMA_RETURN(SO); }
      fout = fopen(fname,"w"); 
      if (fout) {
         for (i=0; i < SO->N_Node; ++i) 
            fprintf (fout,"%d %f %f %f %f\n", i, 
                  SO->PermCol[4*i], SO->PermCol[4*i+1], 
                  SO->PermCol[4*i+2], SO->PermCol[4*i+3]);
         fclose(fout); SUMA_free(fname); fname = NULL;
      } else { SUMA_SL_Err("Failed to write Col file"); SUMA_RETURN(SO); }
   }
   
   /* free hp */
   if (hp) SUMA_free(hp); hp = NULL;
   
   /* some more stuff */
   SN = SUMA_SurfNorm(SO->NodeList, SO->N_Node, SO->FaceSetList, SO->N_FaceSet );
   SO->NodeNormList = SN.NodeNormList;
   SO->FaceNormList = SN.FaceNormList;
   
   /* the shameful pointer copies */
   SO->glar_NodeList = (GLfloat *) SO->NodeList;
   SO->glar_FaceSetList = (GLint *) SO->FaceSetList;
   SO->glar_FaceNormList = (GLfloat *) SO->FaceNormList;
   SO->glar_NodeNormList = (GLfloat *) SO->NodeNormList;
   
   SUMA_RETURN(SO);
}

/*!
   Create a surface object for the colormap 
   This one creates a surface where successive colors share an edge.
   This makes the coloring annoying since you need to use flat shading 
   and make sure you colorize the proper nodes 
      (see p172, OpenGL programming guide and
   NIH-3 lab-book 157. The coloring as implemented here is not correct in that 
   it does not follow the i+2 rule (see OpenGL book ref) but I have decided to
   create a different surface which has 4 nodes per color and no sharing of 
   nodes/edges for successive colors.
   
   See SUMA_Cmap_To_SO
*/
SUMA_SurfaceObject *SUMA_Cmap_To_SO_old (SUMA_COLOR_MAP *Cmap, float orig[3],    
                                          float topright[3], int verb)
{
   static char FuncName[]={"SUMA_Cmap_To_SO_old"};
   SUMA_SurfaceObject *SO= NULL;
   int icol, i, i3, i4;
   float dh, dw;
   SUMA_SURF_NORM SN;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_SL_Err("Function is obsolete.\nUse SUMA_Cmap_To_SO");
   
   SUMA_LH("Allocating surface.");
   SO = SUMA_Alloc_SurfObject_Struct(1);
   if (!Cmap) { SUMA_SL_Err("No Cmap"); SUMA_RETURN(NULL); }
   if (!Cmap->N_M[0]) { SUMA_SL_Err("No colours"); SUMA_RETURN(NULL); }
   if (!SO) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL); }
   
   /* scaling factors */
   dh = (topright[1] - orig[1]) / Cmap->N_M[0];
   dw = topright[0] - orig[0];
   if (dh <= 0 || dw <= 0) {
      SUMA_SL_Err("Whatchyoutalkinaboutwillis?");
      SUMA_RETURN(NULL);
   }
   
   SUMA_LH("Allocating surface elements.");
   /* allocate for the NodeList */
   SO->FileType = SUMA_CMAP_SO;
   SO->N_Node = 2 * (Cmap->N_M[0] + 1);
   SO->N_FaceSet = 2 * Cmap->N_M[0];
   SO->NodeDim = 3;
   SO->EmbedDim = 2;
   SO->FaceSetDim = 3;
   SO->idcode_str = (char *)SUMA_calloc(SUMA_IDCODE_LENGTH, sizeof(char));
   SO->NodeList = (float *)SUMA_calloc(SO->N_Node * 3, sizeof(float));
   SO->FaceSetList = (int *)SUMA_calloc(SO->N_FaceSet * 3,  sizeof(int));
   SO->PermCol = (GLfloat *)SUMA_calloc(SO->N_Node * 4, sizeof(GLfloat));
   if (!SO->idcode_str || !SO->NodeList || !SO->FaceSetList || !SO->PermCol) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL);}
   
   SUMA_LH("Filling up surface id.");
   /* fill up idcode*/
   UNIQ_idcode_fill(SO->idcode_str);
   
   SUMA_LH("Filling up surface nodelist.");
   /* fill up coordinates first */
   /* first fill the oth and the 1st node. They don't follow the cute series */
   SO->NodeList[0] = orig[0];    
   SO->NodeList[1] = orig[1];    
   SO->NodeList[2] = 0.0;
   SO->NodeList[3] = dw+orig[0]; 
   SO->NodeList[4] = orig[1];    
   SO->NodeList[5] = 0.0;
   
   i=2;
   while (i < SO->N_Node) {
      /* even i */
      i3 = i * 3;
      SO->NodeList[i3] = dw+orig[0];
      SO->NodeList[i3+1] = i/2 * dh + orig[1]; 
      SO->NodeList[i3+2] = 0.0; ++i;
      /* odd i */ 
      i3 = i * 3;
      SO->NodeList[i3] = orig[0];   
      SO->NodeList[i3+1] = i/2 * dh + orig[1]; 
      SO->NodeList[i3+2] = 0.0; ++i;
   }
   
   SUMA_LH("Filling up surface facesetlist.");
   /* fill up triangles */
   /* fill up the oth and 1st triangles, they don't follow the cute series */
   SO->FaceSetList[0] = 0; SO->FaceSetList[1] = 1; SO->FaceSetList[2] = 2;
   SO->FaceSetList[3] = 2; SO->FaceSetList[4] = 3; SO->FaceSetList[5] = 0;
   
   icol = 1;
   while (icol < Cmap->N_M[0]) {
      i = 2 * icol;    /* 2 triangles per color */
      /* first triangle */
      i3 = i * 3;
      SO->FaceSetList[i3] = i+1; 
      SO->FaceSetList[i3+1] = i; 
      SO->FaceSetList[i3+2] = i+2;
      /* second triangle */
      i3 = (i+1) * 3;
      SO->FaceSetList[i3] = i+2; 
      SO->FaceSetList[i3+1] = i+3; 
      SO->FaceSetList[i3+2] = i+1; 
      ++icol;
   }
   
   SUMA_LH("Filling up surface colors.");
   /* fill up the color vector */
   /* Node 0 is special */
   SO->PermCol[0] = Cmap->M[0][0]; SO->PermCol[1] = Cmap->M[0][1]; SO->PermCol[2] = Cmap->M[0][2]; SO->PermCol[3] = 1.0;
   /* last node is special */
   i4 = 4 * (SO->N_Node -1);
   SO->PermCol[i4  ] = Cmap->M[Cmap->N_M[0]-1][0]; 
   SO->PermCol[i4+1] = Cmap->M[Cmap->N_M[0]-1][1]; 
   SO->PermCol[i4+2] = Cmap->M[Cmap->N_M[0]-1][2]; 
   SO->PermCol[i4+3] = 1.0;
   
   i=1;
   icol = 0;
   while (i < SO->N_Node -1) {
      /* Next two get the icolth color */
      i4 = 4*i;
      SO->PermCol[i4] = Cmap->M[icol][0];  SO->PermCol[i4+1] = Cmap->M[icol][1]; 
      SO->PermCol[i4+2] = Cmap->M[icol][2];SO->PermCol[i4+3] = 1.0;  ++i; 
      i4 = 4*i;
      SO->PermCol[i4] = Cmap->M[icol][0];  SO->PermCol[i4+1] = Cmap->M[icol][1]; 
      SO->PermCol[i4+2] = Cmap->M[icol][2];SO->PermCol[i4+3] = 1.0;  ++i; 
      ++icol;
   } 
   
   /* if verb, write out the results for checking */
   if (verb > 1) {
      char *fname;
      FILE *fout;
      
      SUMA_LH("writing out surface.");

      fname = SUMA_append_string(Cmap->Name, ".1D.NodeList");
      if (!fname) { SUMA_SL_Err("Failed to create name"); SUMA_RETURN(SO); }
      fout = fopen(fname,"w"); 
      if (fout) {
         for (i=0; i < SO->N_Node; ++i) fprintf (fout,"%f %f %f\n", SO->NodeList[3*i], SO->NodeList[3*i+1], SO->NodeList[3*i+2]);
         fclose(fout); SUMA_free(fname); fname = NULL;
      }else { SUMA_SL_Err("Failed to write NodeList"); SUMA_RETURN(SO); }
      
      fname = SUMA_append_string(Cmap->Name, ".1D.FaceSetList");
      if (!fname) { SUMA_SL_Err("Failed to create name"); SUMA_RETURN(SO); }
      fout = fopen(fname,"w"); 
      if (fout) {
         for (i=0; i < SO->N_FaceSet; ++i) fprintf (fout,"%d %d %d\n", SO->FaceSetList[3*i], SO->FaceSetList[3*i+1], SO->FaceSetList[3*i+2]);
         fclose(fout); SUMA_free(fname); fname = NULL;
      }else { SUMA_SL_Err("Failed to write FaceSetList"); SUMA_RETURN(SO); }
      
      fname = SUMA_append_string(Cmap->Name, ".1D.col");
      if (!fname) { SUMA_SL_Err("Failed to create name"); SUMA_RETURN(SO); }
      fout = fopen(fname,"w"); 
      if (fout) {
         for (i=0; i < SO->N_Node; ++i) fprintf (fout,"%d %f %f %f\n", i, SO->PermCol[4*i], SO->PermCol[4*i+1], SO->PermCol[4*i+2]);
         fclose(fout); SUMA_free(fname); fname = NULL;
      }else { SUMA_SL_Err("Failed to write Col file"); SUMA_RETURN(SO); }
   }
   
   /* some more stuff */
   SN = SUMA_SurfNorm(SO->NodeList,  SO->N_Node, SO->FaceSetList, SO->N_FaceSet );
   SO->NodeNormList = SN.NodeNormList;
   SO->FaceNormList = SN.FaceNormList;
   
   /* the shameful pointer copies */
   SO->glar_NodeList = (GLfloat *) SO->NodeList;
   SO->glar_FaceSetList = (GLint *) SO->FaceSetList;
   SO->glar_FaceNormList = (GLfloat *) SO->FaceNormList;
   SO->glar_NodeNormList = (GLfloat *) SO->NodeNormList;
   
   SUMA_RETURN(SO);
}

/*!
   A poor man's function to sniff if a DO file contains spheres or segments
   Not to be used as a general purpose DO file detector 
*/
SUMA_DO_Types SUMA_Guess_DO_Type(char *s)
{
   static char FuncName[]={"SUMA_Guess_DO_Type"};
   SUMA_DO_Types dotp = not_DO_type;
   FILE *fid=NULL;
   char sbuf[2000];
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!s) {
      SUMA_SL_Warn("Query with null file name");
      SUMA_RETURN(dotp);
   }
   
   if (strstr(s,"<nido_head")) SUMA_RETURN(NIDO_type);
   
   if (SUMA_isExtension(s,".niml.tract")) SUMA_RETURN(TRACT_type);
   
   fid = fopen(s,"r");
   
   if (!fid) {
      sprintf(sbuf,"Could not open file %s for reading.\n"
                   "cwd is set to: %s\n",s, SUMAg_CF->cwd);
      SUMA_SLP_Err(sbuf);
      SUMA_RETURN(dotp);
   }
   
   /* get first 100 chars */
   i = 0;
   sbuf[i] = '\0';
   while (i<100 && !feof(fid)) {
      sbuf[i] = fgetc(fid);
      if (feof(fid)) {
         break;
      }
      ++i;  
   }
   sbuf[i] = '\0';

   /* check for tags */
   if (strstr(sbuf,"#spheres")) {
      dotp = SP_type;
   } else if (strstr(sbuf,"#node-based_spheres")) {
      dotp = NBSP_type;
   } else if (strstr(sbuf,"#segments")) {
      dotp = LS_type;
   } else if (strstr(sbuf,"#oriented_segments")) {
      dotp = OLS_type;
   } else if (strstr(sbuf,"#node-based_segments")) {
      dotp = NBLS_type;
   } else if (strstr(sbuf,"#node-based_oriented_segments")) {
      dotp = NBOLS_type;
   } else if (strstr(sbuf,"#node-based_vectors")) {
      dotp = NBV_type; 
   } else if (strstr(sbuf,"#node-based_ball-vectors")) {
      dotp = ONBV_type;
   } else if (strstr(sbuf,"#planes")) {
      dotp = PL_type;
   } else if (strstr(sbuf,"#node_based_text")) {
      dotp = NBT_type;
   } else if (strstr(sbuf,"#dicom_based_text")) {
      dotp = DBT_type;
   } else if (strstr(sbuf,"#screen_based_text")) {
      dotp = SBT_type;
   } else if (strstr(sbuf,"#suma_dset")) { /* this case should not happen */
      dotp = SDSET_type;
   } else if (strstr(sbuf,"nido_head")) {
      dotp = NIDO_type;
   } else if (strstr(sbuf,"<") || strstr(sbuf,">")) {
      SUMA_S_Warnv("Bad format likely in niml file %s.\n"
                   "Could not find element '<nido_head'\n",
                   s);
   }
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: Searched header string:\n>>>%s<<<\ndotp = %d\n", 
               FuncName, sbuf, dotp);
   }
   
   fclose(fid); fid = NULL;
   
   SUMA_RETURN(dotp);
}

/* 
   If idcode_str is NULL, NIDO's idcode is a hash of the label
*/
SUMA_NIDO * SUMA_Alloc_NIDO ( char *idcode_str, char *Label, 
                              char *Parent_idcode_str)
{
   static char FuncName[]={"SUMA_Alloc_NIDO"};
   SUMA_NIDO * nido= NULL;
   float FontCol[4]={1.0, 0.3, 1.0, 1.0};

   SUMA_ENTRY;
   
   nido = (SUMA_NIDO *) SUMA_calloc(1,sizeof (SUMA_NIDO));
   if (!nido) {
      fprintf(stderr,"Error %s: Failed to allocate for nido\n", FuncName);
      SUMA_RETURN (nido);
   }
   
   if (Label) nido->Label = SUMA_copy_string(Label);
   else nido->Label = SUMA_copy_string("nuda");
   
   if (idcode_str) nido->idcode_str = SUMA_copy_string(idcode_str);
   else nido->idcode_str = UNIQ_hashcode(nido->Label);
   
   nido->ngr = NI_new_group_element();
   NI_rename_group(nido->ngr, "nido_head");
   
   nido->do_type = NIDO_type;
   if (Parent_idcode_str) {
      NI_set_attribute(nido->ngr, "Parent_idcode_str", Parent_idcode_str);
   } else {
      NI_set_attribute(nido->ngr, "Parent_idcode_str", "");
   }
   
   /* setup some default values for NIDOs*/
   NI_set_attribute(nido->ngr,"default_font", "helvetica_18");
   NI_SET_FLOATv(nido->ngr,"default_color", FontCol,4);
      
   SUMA_RETURN (nido);
}

SUMA_NIDO *SUMA_free_NIDO(SUMA_NIDO *NIDO) 
{
   static char FuncName[]={"SUMA_free_NIDO"};   
   
   SUMA_ENTRY;
   
   if (!NIDO) SUMA_RETURN(NULL);
   if (NIDO->ngr) NI_free_element(NIDO->ngr);
   if (NIDO->idcode_str) SUMA_free(NIDO->idcode_str);
   if (NIDO->Label) SUMA_free(NIDO->Label);
   SUMA_free(NIDO); NIDO = NULL;
   
   SUMA_RETURN(NIDO);
}


/*!
 allocate for a segment DO 
   SDO = SUMA_Alloc_SegmentDO ( N_n, Label)

   \param N_n (int) number of nodes to allocate for n0 and n1. 
      if N_n > 0  SDO->n0 and n1 (GLfloat *) vector to 3*N_n elements 
                  to contain the XYZ of nodes n0 and n1.
      else SDO->n0 and n1 = NULL
   \param Label (char *) label of segment DO. Pass NULL for no labels
   \param Parent_idcode_str (char *) Parent surface of SegmentDO.
                                     It should not be NULL 
                                     if the DO is node based 
   \param NodeBased (int) -1: (old usage) set to 0 if !Parent_idcode_str
                                                1 if Parent_idcode_str
                          0: segment ends specified with XYZ coords
                          1: One end is specified by node index, second is by XYZ
                          2: Both ends are specified by node index.
   \returns SDO (SUMA_SegmentDO *) 
     
*/
SUMA_SegmentDO * SUMA_Alloc_SegmentDO (int N_n, char *Label, int oriented, 
                                       char *Parent_idcode_str, int NodeBased, 
                                       SUMA_DO_Types type, 
                                       SUMA_DO_Types Parent_type, 
                                       char *DrawnDO_variant)
{
   static char FuncName[]={"SUMA_Alloc_SegmentDO"};
   SUMA_SegmentDO * SDO= NULL;
   char *hs = NULL;
   
   SUMA_ENTRY;
   
   SDO = (SUMA_SegmentDO *) SUMA_calloc(1,sizeof (SUMA_SegmentDO));
   if (!SDO) {
         fprintf(stderr,"Error %s: Failed to allocate for SDO\n", FuncName);
         SUMA_RETURN (SDO);
   }
   if (!DrawnDO_variant) DrawnDO_variant="";
   SDO->do_type = type;
   SDO->Parent_do_type = Parent_type;
   SDO->DrawnDO_variant = SUMA_copy_string(DrawnDO_variant);
   if (NodeBased == -1) {
      /* backward comaptibility */
      if (Parent_idcode_str) NodeBased = 1;
      else NodeBased = 0;
   }
   if (!Parent_idcode_str && NodeBased > 0) {
      SUMA_S_Warnv(
            "Bad combination! Parent_idcode_str is NULL and NodeBased = %d\n",
            NodeBased );
   }
   if (N_n > 0) {
      if (!Parent_idcode_str) { /*  NodeBased must be = 0 */
         SDO->NodeBased = 0;
         SDO->NodeID = NULL;
         SDO->NodeID1 = NULL;
         SDO->Parent_idcode_str = NULL;
         SDO->n0 = (GLfloat *) SUMA_calloc (3*N_n, sizeof(GLfloat));
         SDO->n1 = (GLfloat *) SUMA_calloc (3*N_n, sizeof(GLfloat));
         SDO->N_UnqNodes = -2; /* Cannot be set */
      } else {
         if (NodeBased == 1) {
            SDO->NodeBased = 1;
            SDO->n0 = NULL;
            SDO->n1 = (GLfloat *) SUMA_calloc (3*N_n, sizeof(GLfloat));
            SDO->Parent_idcode_str = SUMA_copy_string(Parent_idcode_str);
            SDO->NodeID = (int*) SUMA_calloc(N_n, sizeof(int));
            SDO->NodeID1 = NULL;
            SDO->N_UnqNodes = -1;
         } else if (NodeBased == 2) {
            SDO->NodeBased = 2;
            SDO->n0 = NULL;
            SDO->n1 = NULL;
            SDO->Parent_idcode_str = SUMA_copy_string(Parent_idcode_str);
            SDO->NodeID = (int*) SUMA_calloc(N_n, sizeof(int));
            SDO->NodeID1 = (int*) SUMA_calloc(N_n, sizeof(int));
            SDO->N_UnqNodes = -1;
         } 
      }
   
      if (  (!SDO->NodeBased && !(SDO->n0 && SDO->n1)) || 
            (SDO->NodeBased == 1 && !(SDO->NodeID && SDO->n1)) || 
            (SDO->NodeBased == 2 && !(SDO->NodeID && SDO->NodeID1)) ) {
         fprintf(stderr,
                  "Error %s: Failed to allocate for SDO->n1 or SDO->n0\n"
                  "Have NodeBased = %d, n0=%p, n1=%p, NodeID=%p, NodeID1=%p\n", 
            FuncName, SDO->NodeBased, SDO->n0, SDO->n1, 
                      SDO->NodeID, SDO->NodeID1);
         if (SDO->n0) SUMA_free (SDO->n0);
         if (SDO->n1) SUMA_free (SDO->n1);
         if (SDO->NodeID) SUMA_free(SDO->NodeID);
         if (SDO->NodeID1) SUMA_free(SDO->NodeID1);
         if (SDO->Parent_idcode_str) SUMA_free(SDO->Parent_idcode_str);
         if (SDO) SUMA_free (SDO);
         SUMA_RETURN (NULL);
      }
   } else {
      SDO->NodeID = NULL;
      SDO->NodeID1 = NULL;
      SDO->NodeBased = 0;
      SDO->Parent_idcode_str = NULL;
      SDO->n0 = NULL;
      SDO->n1 = NULL;
      SDO->N_n = 0;
      SDO->N_UnqNodes = -1;
   }
   
   /* create a string to hash an idcode */
   if (Label) hs = SUMA_copy_string(Label);
   else hs = SUMA_copy_string("NULL_");
   if (Parent_idcode_str) 
      hs = SUMA_append_replace_string(hs,Parent_idcode_str,"_",1);
   else hs = SUMA_append_replace_string(hs,"NULL","",1);
   SDO->idcode_str = UNIQ_hashcode(hs);
   SUMA_free(hs); hs = NULL;
   
   if (Label) {
      SDO->Label = (char *)SUMA_calloc (strlen(Label)+1, sizeof(char));
      SDO->Label = strcpy (SDO->Label, Label);
   } else {
      SDO->Label = NULL;
   }
   
   SDO->N_n = N_n;
   SDO->Stipple = SUMA_SOLID_LINE;
   /* setup some default values */
   SDO->LineWidth = 4.0;
   SDO->LineCol[0] = 1.0; SDO->LineCol[1] = 0.3; 
   SDO->LineCol[2] = 1.0; SDO->LineCol[3] = 1.0; 
   
   SDO->colv = NULL;
   SDO->thickv = NULL;
   SDO->stipv = NULL;
   SDO->Mstip = 1;
   SDO->stip = 0xAAAA;
   
   SDO->topobj = NULL;
   if (oriented) {
      SDO->botobj = gluNewQuadric();
   } else SDO->botobj = NULL;
   
   SUMA_RETURN (SDO);
}

/* Set the number of unique points in a segment DO.
   if N is provided and is >= 0, them SDO->N_UnqNodes is set to N
   and the function returns
   Otherwise, the function will figure out the number of unique points
   if possible, and if SDO->N_UnqNodes is not = -2. A value of 
   SDO->N_UnqNodes = -2 is meant to flag that no attempt should 
   be made to compute N_UnqNodes.
*/
int SUMA_Set_N_UnqNodes_SegmentDO(SUMA_SegmentDO * SDO, int N)
{
   static char FuncName[]={"SUMA_Set_N_UnqNodes_SegmentDO"};
   int *uu=NULL, *uus=NULL;
   SUMA_ENTRY;
   
   if (!SDO) SUMA_RETURN(-2); /* error */
   
   if (SDO->N_UnqNodes < -1) { /* flagged as not feasible, don't bother */
      SUMA_RETURN(SDO->N_UnqNodes);
   }                           
   if (!SDO->NodeID && !SDO->NodeID1) { /* nothing possible */
      SDO->N_UnqNodes = -2; SUMA_RETURN(SDO->N_UnqNodes);
   }
   if (N >= 0) { /* use it, no questions asked */
      SDO->N_UnqNodes = N; SUMA_RETURN(SDO->N_UnqNodes);
   }
   if (SDO->N_UnqNodes >= 0) { /* don't bother anew, return existing answer */
      SUMA_RETURN(SDO->N_UnqNodes);
   }
   
   /* Now we need to figure things out here */
   if (!SDO->NodeID && SDO->NodeID1) {
      uu = SUMA_UniqueInt(SDO->NodeID1, SDO->N_n, &(SDO->N_UnqNodes), 0);
      SUMA_ifree(uu);
      SUMA_RETURN(SDO->N_UnqNodes); 
   } else if (SDO->NodeID && !SDO->NodeID1) {
      uu = SUMA_UniqueInt(SDO->NodeID, SDO->N_n, &(SDO->N_UnqNodes), 0);
      SUMA_ifree(uu);
      SUMA_RETURN(SDO->N_UnqNodes); 
   } else { /* Both are set */
      if (!(uu = (int *)SUMA_malloc(SDO->N_n*2 * sizeof(int)))) {
         SUMA_S_Crit("Failed to allocate");
         SDO->N_UnqNodes = -2;
         SUMA_RETURN(SDO->N_UnqNodes);
      }
      memcpy(uu, SDO->NodeID, SDO->N_n*sizeof(int));
      memcpy(uu+SDO->N_n, SDO->NodeID1, SDO->N_n*sizeof(int));
      uus = SUMA_UniqueInt(uu, 2*SDO->N_n, &(SDO->N_UnqNodes), 0);
      SUMA_ifree(uus); SUMA_ifree(uu);
      SUMA_RETURN(SDO->N_UnqNodes);
   } 
   /* should not get here */
   SDO->N_UnqNodes = -2;
   SUMA_RETURN(SDO->N_UnqNodes);
}

void SUMA_free_SegmentDO (SUMA_SegmentDO * SDO)
{
   static char FuncName[]={"SUMA_free_SegmentDO"};

   SUMA_ENTRY;
   
   if (!SDO) SUMA_RETURNe;
   if (SDO->Parent_idcode_str) SUMA_free(SDO->Parent_idcode_str);
   if (SDO->NodeID) SUMA_free(SDO->NodeID); 
   if (SDO->NodeID1) SUMA_free(SDO->NodeID1); 
   if (SDO->n0) SUMA_free(SDO->n0);
   if (SDO->n1) SUMA_free(SDO->n1);
   if (SDO->idcode_str) SUMA_free(SDO->idcode_str);
   if (SDO->Label) SUMA_free(SDO->Label);
   if (SDO->thickv) SUMA_free(SDO->thickv);
   if (SDO->stipv) SUMA_free(SDO->stipv);
   if (SDO->colv) SUMA_free(SDO->colv);
   if (SDO->botobj) gluDeleteQuadric(SDO->botobj);
   if (SDO->topobj) gluDeleteQuadric(SDO->topobj);
   if (SDO) SUMA_free(SDO);
   
   SUMA_RETURNe;
}

/* Create a blank NIDO header 
   If idcode_str is NULL, the Label, determines the
   idcode_str. */
SUMA_NIDO *SUMA_BlankNIDO (char *idcode_str, char *Label, 
                           char *parent_so_id, char *coord_type,
                           char *font_name)
{
   static char FuncName[]={"SUMA_BlankNIDO"};
   SUMA_NIDO *nido = NULL;
   
   SUMA_ENTRY;
   
   nido = SUMA_Alloc_NIDO(idcode_str, Label, parent_so_id);
   
   if (parent_so_id) {
      NI_set_attribute(nido->ngr, "bond", "surface");
   }
   NI_set_attribute( nido->ngr, "coord_type", 
                     SUMA_CoordTypeName(SUMA_CoordType(coord_type)));
   NI_set_attribute(nido->ngr, "default_font", 
                    SUMA_glutBitmapFontName(SUMA_glutBitmapFont(font_name)));
   NI_set_attribute(nido->ngr, "default_color", 
                    "1.0 1.0 1.0 1.0"); 
   
   SUMA_RETURN(nido);                 
}

SUMA_NIDO *SUMA_ReadNIDO (char *fname, char *parent_so_id)
{
   static char FuncName[]={"SUMA_ReadNIDO"};
   SUMA_NIDO *nido = NULL;
   char *niname=NULL, *atr = NULL;
   float col[4];
   NI_element *nini=NULL;
   NI_stream ns=NULL;
   SUMA_Boolean LocalHead = NOPE;


   SUMA_ENTRY;
   
   if (!fname) SUMA_RETURN(NULL);
   
   if ((atr = strstr(fname,"<nido_head"))) {
      /* The fname is the nido */
      niname = SUMA_copy_string("str:");
      ns = NI_stream_open(niname, "r");
      NI_stream_setbuf( ns , atr ) ;
   } else if (SUMA_GuessFormatFromExtension(fname, NULL)==SUMA_NIML) {
      niname = SUMA_append_string("file:", fname);
      ns = NI_stream_open(niname, "r");
   } else {
      SUMA_S_Err("Only .niml.do format accepted");
      SUMA_RETURN(NULL);
   }
   {
      while ((nini = NI_read_element(ns, 1))) {
         if (SUMA_iswordin(nini->name,"nido_head")) {/* fill special fields */
            if (nido) {
               SUMA_S_Err("Not ready for a second header!");
               SUMA_RETURN(NULL);
            } else { /* time to create */
               /* is there an SO_label? */
               if ((atr = NI_get_attribute(nini,"SO_label"))) {
                  /* try to get parent */
               }
               atr = NI_get_attribute(nini,"bond");
               if (atr && atr[0] == 's' && !parent_so_id) {
                  SUMA_S_Err( "No surface available to which\n"
                              "the NIDO can bond");
                  SUMA_RETURN(NULL);
               }
               nido = SUMA_Alloc_NIDO(
                        NI_get_attribute(nini,"idcode_str"), 
                        fname, parent_so_id);
               NI_set_attribute(nido->ngr, "bond", atr);
            }
            if ((atr = NI_get_attribute(nini,"coord_type"))) {
               if (SUMA_CoordType(atr) != SUMA_COORD_TYPE_ERROR) {
                  NI_set_attribute(nido->ngr, "coord_type", atr);
               } else {
                  SUMA_S_Errv("coord_type attribute (%s) must be one of\n"
                              "local or screen, using default %s\n", 
                              atr, SUMA_CoordTypeName(SUMA_CoordType(NULL)));
                  NI_set_attribute( nido->ngr, 
                                    "coord_type", 
                                    SUMA_CoordTypeName(SUMA_CoordType(NULL)));
               }
            } else {
               SUMA_LH("Defaulting to coord_type = world");
               NI_set_attribute( nido->ngr, "coord_type",
                                 SUMA_CoordTypeName(SUMA_CoordType(NULL)));
            }
            if ((atr = NI_get_attribute(nini,"default_font"))) {
               if (SUMA_glutBitmapFont(atr)) {
                  NI_set_attribute(nido->ngr, "default_font", atr);
               } else {
                  SUMA_S_Errv("default_font %s not found.\n"
                              "Defaulting to %s \n", atr,
                           SUMA_glutBitmapFontName(SUMA_glutBitmapFont(NULL)));
                  NI_set_attribute(nido->ngr, "default_font", 
                           SUMA_glutBitmapFontName(SUMA_glutBitmapFont(NULL)));
               }
            } else {
               SUMA_LHv("Defaulting to font %s\n",
                        SUMA_glutBitmapFontName(SUMA_glutBitmapFont(NULL)));
               NI_set_attribute(nido->ngr, "default_font", 
                        SUMA_glutBitmapFontName(SUMA_glutBitmapFont(NULL)));
            }
            if ((atr = NI_get_attribute(nini,"default_color"))) {
               NI_GET_FLOATv(nini, "default_color", col, 4, LocalHead);
               if (NI_GOT) {
                  NI_set_attribute(nido->ngr, "default_color", atr);
               } else {
                  SUMA_S_Errv("default_color %s not found.\n"
                              "Defaulting to default_color = 1 1 1 1\n", atr);
                  NI_set_attribute(nido->ngr, "default_color", 
                                              "1.0 1.0 1.0 1.0");
               }
            } else {
               SUMA_LH("Defaulting to default_color = 1 1 1 1");
               NI_set_attribute(nido->ngr, "default_color", "1.0 1.0 1.0 1.0");
            }
         } else {
            if (!nido) {
               SUMA_S_Err("Found element before header!");
               SUMA_RETURN(NOPE);
            }
            NI_add_to_group(nido->ngr, nini);
            if (LocalHead) SUMA_ShowNel(nini);
         }
      } 
      NI_stream_close( ns ) ; ns = NULL;
      SUMA_free(niname); niname=NULL;
      if (LocalHead) SUMA_ShowNel(nido->ngr); 
   } 
   SUMA_RETURN(nido);
}

SUMA_SegmentDO * SUMA_ReadNBVecDO (char *s, int oriented, char *parent_SO_id)
{
   static char FuncName[]={"SUMA_ReadNBVecDO"};
   SUMA_SegmentDO *SDO = NULL;
   MRI_IMAGE * im = NULL;
   float *far=NULL;
   int itmp, itmp2, icol_thick = -1, icol_col=-1, icol_id = -1, icol_vec = -1;
   int nrow=-1, ncol=-1;
   SUMA_DO_Types dotp;
   char buf[30];
   
   SUMA_ENTRY;
   
   if (!s) {
      SUMA_SLP_Err("NULL s");
      SUMA_RETURN(NULL);
   }
   
   if (!parent_SO_id) {
      SUMA_SLP_Err("NULL parent_SO_id");
      SUMA_RETURN(NULL);
   }
   im = mri_read_1D (s);

   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NULL);
   }
   
   if (oriented) {
      sprintf(buf,"Oriented Node-Based Vectors");
      dotp = ONBV_type;
   } else {
      dotp = NBV_type;
      sprintf(buf,"Bottomless Node-Based Vector ");
   }
   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;

   if (!ncol) {
      SUMA_SLP_Err("Empty file");
      SUMA_RETURN(NULL);
   }
   
   icol_col = -1;
   icol_thick = -1;
   icol_id = -1;
   icol_vec = -1;
   switch (nrow) {
      case 3:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "vx vy vz\n", 
                              FuncName, buf, s);
         icol_vec = 0;
         break;
      case 4:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "n vx vy vz\n", 
                              FuncName, buf, s);
         icol_id = 0;
         icol_vec = 1;
         break;
      case 5:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "n vx vy vz th\n", 
                              FuncName, buf, s);
         icol_id = 0;
         icol_vec = 1;
         icol_thick = 4;
         break;
      case 8:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "n vx vy vz c0 c1 c2 c3\n", 
                              FuncName, buf, s);
         
         icol_id = 0;
         icol_vec = 1;
         icol_col = 4;
         break;
      case 9:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "n vx vy vz c0 c1 c2 c3 th\n", 
                              FuncName, buf, s);
         
         icol_id = 0;
         icol_vec = 1;
         icol_col = 4;
         icol_thick = 8;
         break;
      default:
         SUMA_SLP_Err("File must have\n"
                   "3,4,5,8 or 9 columns.");
         mri_free(im); im = NULL;   /* done with that baby */
         SUMA_RETURN(NULL);
   }

   /* allocate for segments DO */
   SDO = SUMA_Alloc_SegmentDO (ncol, s, oriented, parent_SO_id, 1, 
                               dotp, SO_type, NULL);
   if (!SDO) {
      fprintf(SUMA_STDERR,
                     "Error %s: Failed in SUMA_Allocate_SegmentDO.\n", FuncName);
      SUMA_RETURN(NULL);
   }

   /* fill up SDO */
   itmp = 0;
   while (itmp < SDO->N_n) {
      itmp2 = 3*itmp;
      SDO->n1[itmp2]   = far[itmp+(icol_vec  )*ncol];
      SDO->n1[itmp2+1] = far[itmp+(icol_vec+1)*ncol];
      SDO->n1[itmp2+2] = far[itmp+(icol_vec+2)*ncol];
      ++itmp;
   } 
   
   if (icol_id >= 0) {
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->NodeID[itmp]   = far[itmp+(icol_id  )*ncol];
         ++itmp;
      }
   } else {
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->NodeID[itmp]   = itmp;
         ++itmp;
      }
   }
   
   if (icol_col > 0) {
      SDO->colv = (GLfloat *)SUMA_malloc(4*sizeof(GLfloat)*SDO->N_n);
      if (!SDO->colv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual colors */
      itmp = 0;
      while (itmp < SDO->N_n) {
         itmp2 = 4*itmp;
         SDO->colv[itmp2]     = far[itmp+(icol_col  )*ncol];
         SDO->colv[itmp2+1]   = far[itmp+(icol_col+1)*ncol];
         SDO->colv[itmp2+2]   = far[itmp+(icol_col+2)*ncol];
         SDO->colv[itmp2+3]   = far[itmp+(icol_col+3)*ncol];
         ++itmp;
      } 
   }
   if (icol_thick > 0) {
      SDO->thickv = (GLfloat *)SUMA_malloc(sizeof(GLfloat)*SDO->N_n);
      if (!SDO->thickv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual thickness */
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->thickv[itmp]     = far[itmp+(icol_thick  )*ncol];
         ++itmp;
      } 
   }
   
   mri_free(im); im = NULL; far = NULL;

   SUMA_RETURN(SDO);
}

GLushort SUMA_int_to_stipplemask(int i) 
{
   switch (i) {
      case 0:
         return(0xFFFF); /* solid */
      case 1:
         return(0x0101); /* dotted */
      case 2:
         return(0xAAAA);
      case 3:
         return(0x0C0F);
      case 4:
         return(0x00FF);
      case 5:
         return(0x1C47); /* dash dot dash */
      default:
         return(0x0101);
   }
   return(0xFFFF);
}

/* eventually make this return a much finer
gradation of dottiness, no worries about
ugly randomness here */
GLushort SUMA_int_to_stipplemask_cont(int i) 
{
   switch (i) {
      case 0:
         return(0xFFFF); /* solid */
      case 1:
         return(0x0101); /* dotted */
      case 2:
         return(0xAAAA);
      case 3:
         return(0x0C0F);
      case 4:
         return(0x00FF);
      case 5:
         return(0x1C47); /* dash dot dash */
      default:
         return(0x0101);
   }
   return(0xFFFF);
}
SUMA_SegmentDO * SUMA_ReadSegDO (char *s, int oriented, char *parent_SO_id)
{
   static char FuncName[]={"SUMA_ReadSegDO"};
   SUMA_SegmentDO *SDO = NULL;
   MRI_IMAGE * im = NULL;
   float *far=NULL;
   int itmp, itmp2, icol_thick = -1, icol_col=-1, icol_stip=-1;
   int nrow=-1, ncol=-1, same = 0;
   char buf[30];
   SUMA_DO_Types dotp;
   
   SUMA_ENTRY;
   
   if (!s) {
      SUMA_SLP_Err("NULL s");
      SUMA_RETURN(NULL);
   }
   
   im = mri_read_1D (s);

   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NULL);
   }
   
   if (oriented) {
      dotp = OLS_type;
      sprintf(buf,"Oriented segment");
   } else {
      dotp = LS_type;
      sprintf(buf,"Segment");
   }
   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;

   if (!ncol) {
      SUMA_SLP_Err("Empty file");
      SUMA_RETURN(NULL);
   }
   
   icol_col = -1;
   icol_thick = -1;
   icol_stip = -1;
   switch (nrow) {
      case 6:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "x0 y0 z0 x1 y1 z1\n", 
                              FuncName, buf, s);
         break;
      case 7:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "x0 y0 z0 x1 y1 z1 th\n", 
                              FuncName, buf, s);
         icol_thick = 6;
         break;
      case 10:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "x0 y0 z0 x1 y1 z1 c0 c1 c2 c3\n", 
                              FuncName, buf, s);
         icol_col = 6;
         break;
      case 11:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "x0 y0 z0 x1 y1 z1 c0 c1 c2 c3 th\n", 
                              FuncName, buf, s);
         icol_col = 6;
         icol_thick = 10;
         break;
      case 12:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "x0 y0 z0 x1 y1 z1 c0 c1 c2 c3 th stp\n", 
                              FuncName, buf, s);
         icol_col = 6;
         icol_thick = 10;
         icol_stip = 11;
         break;
      default:
         SUMA_SLP_Err("File must have\n"
                   "6,7,10, 11, 12 columns.");
         mri_free(im); im = NULL;   /* done with that baby */
         SUMA_RETURN(NULL);
   }

   /* allocate for segments DO */
   SDO = SUMA_Alloc_SegmentDO (ncol, s, oriented, parent_SO_id, 0, 
                     dotp, parent_SO_id ? SO_type:type_not_set, NULL);
   if (!SDO) {
      fprintf(SUMA_STDERR,
              "Error %s: Failed in SUMA_Allocate_SegmentDO.\n", FuncName);
      SUMA_RETURN(NULL);
   }

   /* fill up SDO */
   itmp = 0;
   while (itmp < SDO->N_n) {
      itmp2 = 3*itmp;
      SDO->n0[itmp2]   = far[itmp       ];
      SDO->n0[itmp2+1] = far[itmp+  ncol];
      SDO->n0[itmp2+2] = far[itmp+2*ncol];
      SDO->n1[itmp2]   = far[itmp+3*ncol];
      SDO->n1[itmp2+1] = far[itmp+4*ncol];
      SDO->n1[itmp2+2] = far[itmp+5*ncol]; 
      ++itmp;
   } 
   
   if (icol_col > 0) {
      SDO->colv = (GLfloat *)SUMA_malloc(4*sizeof(GLfloat)*SDO->N_n);
      if (!SDO->colv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual colors */
      itmp = 0;
      while (itmp < SDO->N_n) {
         itmp2 = 4*itmp;
         SDO->colv[itmp2]     = far[itmp+(icol_col  )*ncol];
         SDO->colv[itmp2+1]   = far[itmp+(icol_col+1)*ncol];
         SDO->colv[itmp2+2]   = far[itmp+(icol_col+2)*ncol];
         SDO->colv[itmp2+3]   = far[itmp+(icol_col+3)*ncol];
         ++itmp;
      } 
   }
   if (icol_thick > 0) {
      SDO->thickv = (GLfloat *)SUMA_malloc(sizeof(GLfloat)*SDO->N_n);
      if (!SDO->thickv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual thickness */
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->thickv[itmp]     = far[itmp+(icol_thick  )*ncol];
         ++itmp;
      } 
   }
   if (icol_stip > 0) {
      SDO->stipv = (GLushort *)SUMA_malloc(sizeof(GLushort)*SDO->N_n);
      if (!SDO->stipv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual stippliness */
      itmp = 0; same =  1;
      while (itmp < SDO->N_n) {
         SDO->stipv[itmp]     = SUMA_int_to_stipplemask(
                                 (int)far[itmp+(icol_stip  )*ncol]);
         if (same && SDO->stipv[itmp] != SDO->stipv[0]) same = 0; 
         ++itmp;
      }
      if (same) {
         SDO->stip = (GLushort)SDO->stipv[0];
         SUMA_free(SDO->stipv); SDO->stipv=NULL;
      }
      if (SDO->stipv || 
          ( SDO->stip != 0 && SDO->stip != 0xFFFF)) {
               SDO->Stipple = SUMA_DASHED_LINE;
      } else {
         SDO->Stipple = SUMA_SOLID_LINE;
      }
   }
   mri_free(im); im = NULL; far = NULL;

   SUMA_RETURN(SDO);
}

SUMA_SegmentDO * SUMA_ReadNBSegDO (char *s, int oriented, char *parent_SO_id)
{
   static char FuncName[]={"SUMA_ReadNBSegDO"};
   SUMA_SegmentDO *SDO = NULL;
   MRI_IMAGE * im = NULL;
   float *far=NULL;
   int itmp, itmp2, icol_thick = -1, icol_col=-1, icol_stip=-1;
   int nrow=-1, ncol=-1;
   int NodeBased = 2, same=0;
   char buf[30];
   SUMA_DO_Types dotp;
   
   SUMA_ENTRY;
   
   if (!s) {
      SUMA_SLP_Err("NULL s");
      SUMA_RETURN(NULL);
   }
   
   im = mri_read_1D (s);

   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NULL);
   }
   
   if (oriented) {
      dotp = NBOLS_type;
      sprintf(buf,"Oriented Node-Based segment");
   } else {
      dotp = NBLS_type;
      sprintf(buf,"Node-Based Segment");
   }
   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;

   if (!ncol) {
      SUMA_SLP_Err("Empty file");
      SUMA_RETURN(NULL);
   }
   
   icol_col = -1;
   icol_thick = -1;
   icol_stip = -1;
   switch (nrow) {  
      case 2:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "n0 n1\n", 
                              FuncName, buf, s);
         break;
      case 3:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "n0 n1 th\n", 
                              FuncName, buf, s);
         icol_thick = 2;
         break;
      case 6:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "n0 n1 c0 c1 c2 c3\n", 
                              FuncName, buf, s);
         icol_col = 2;
         break;
      case 7:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "n0 n1 c0 c1 c2 c3 th\n", 
                              FuncName, buf, s);
         icol_col = 2;
         icol_thick = 6;
         break;
      case 8:
         fprintf(SUMA_STDERR,"%s: %s file %s's format:\n"
                              "n0 n1 c0 c1 c2 c3 th st\n", 
                              FuncName, buf, s);
         icol_col = 2;
         icol_thick = 6;
         icol_stip = 7;
         break;
      default:
         SUMA_SLP_Err("File must have\n"
                   "2, 3, 6, 7, or 8 columns.");
         mri_free(im); im = NULL;   /* done with that baby */
         SUMA_RETURN(NULL);
   }

   /* allocate for segments DO */
   SDO = SUMA_Alloc_SegmentDO (ncol, s, oriented, 
              parent_SO_id, NodeBased, dotp, 
              parent_SO_id?SO_type:type_not_set, NULL);
   if (!SDO) {
      fprintf(SUMA_STDERR,
               "Error %s: Failed in SUMA_Allocate_SegmentDO.\n", FuncName);
      SUMA_RETURN(NULL);
   }

   /* fill up SDO */
   itmp = 0;
   while (itmp < SDO->N_n) {
      SDO->NodeID[itmp]  = far[itmp       ];
      SDO->NodeID1[itmp] = far[itmp+  ncol];
      ++itmp;
   } 
   
   if (icol_col > 0) {
      SDO->colv = (GLfloat *)SUMA_malloc(4*sizeof(GLfloat)*SDO->N_n);
      if (!SDO->colv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual colors */
      itmp = 0;
      while (itmp < SDO->N_n) {
         itmp2 = 4*itmp;
         SDO->colv[itmp2]     = far[itmp+(icol_col  )*ncol];
         SDO->colv[itmp2+1]   = far[itmp+(icol_col+1)*ncol];
         SDO->colv[itmp2+2]   = far[itmp+(icol_col+2)*ncol];
         SDO->colv[itmp2+3]   = far[itmp+(icol_col+3)*ncol];
         ++itmp;
      } 
   }
   if (icol_thick > 0) {
      SDO->thickv = (GLfloat *)SUMA_malloc(sizeof(GLfloat)*SDO->N_n);
      if (!SDO->thickv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual thickness */
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->thickv[itmp]     = far[itmp+(icol_thick  )*ncol];
         ++itmp;
      } 
   }
   
   if (icol_stip > 0) {
      SDO->stipv = (GLushort *)SUMA_malloc(sizeof(GLushort)*SDO->N_n);
      if (!SDO->stipv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual stipness */
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->stipv[itmp]     = SUMA_int_to_stipplemask(
                                 (int)far[itmp+(icol_stip  )*ncol]);
         if (same && SDO->stipv[itmp] != SDO->stipv[0]) same = 0; 
         ++itmp;
      }
      if (same) {
         SDO->stip = (GLushort)SDO->stipv[0];
         SUMA_free(SDO->stipv); SDO->stipv=NULL;
      }
      if (SDO->stipv || 
          ( SDO->stip != 0 && SDO->stip != 0xFFFF)) {
               SDO->Stipple = SUMA_DASHED_LINE;
      } else {
         SDO->Stipple = SUMA_SOLID_LINE;
      }
   }
   mri_free(im); im = NULL; far = NULL;

   SUMA_RETURN(SDO);
}


NI_group *SUMA_SDO2niSDO(SUMA_SegmentDO *SDO) 
{
   static char FuncName[]={"SUMA_SDO2niSDO"};
   NI_group *ngr = NULL;
   NI_element *nel = NULL;
   
   SUMA_ENTRY;
   
   if (!SDO) { SUMA_RETURN(ngr); }
   
   ngr = NI_new_group_element();   
   NI_rename_group(ngr, "Segment_DO");
   
   NI_SET_STR(ngr, "idcode_str", SDO->idcode_str);
   NI_SET_STR(ngr, "Label", SDO->Label);
   NI_SET_INT(ngr, "NodeBased", SDO->NodeBased);
   NI_SET_STR(ngr, "Parent_idcode_str", SDO->Parent_idcode_str);
   NI_SET_INT(ngr, "N_n", SDO->N_n);
   NI_SET_FLOAT(ngr, "LineWidth", SDO->LineWidth);
   NI_SET_FLOATv(ngr, "LineCol", SDO->LineCol, 4);
   NI_SET_INT(ngr, "do_type", SDO->do_type);
   NI_SET_INT(ngr, "Parent_do_type", SDO->Parent_do_type);
   NI_SET_STR(ngr, "DrawnDO_variant", SDO->DrawnDO_variant);
   if (!SDO->DrawnDO_variant) SDO->DrawnDO_variant = SUMA_copy_string("");
   
   if (SDO->botobj) { NI_SET_INT(ngr, "oriented", 1); }
   else { NI_SET_INT(ngr, "oriented", 0); }
   
   if (SDO->NodeID) {
      nel = NI_new_data_element("NodeID", SDO->N_n);
      NI_add_column(nel, NI_INT, SDO->NodeID);
      NI_add_to_group( ngr, nel); 
   }
   if (SDO->NodeID1) {
      nel = NI_new_data_element("NodeID1", SDO->N_n);
      NI_add_column(nel, NI_INT, SDO->NodeID1);
      NI_add_to_group( ngr, nel); 
   }
   if (sizeof(GLfloat)!=sizeof(float)) { 
      SUMA_S_Err("I hate life"); SUMA_RETURN(NULL); }
   if (SDO->n0) {
      nel = NI_new_data_element("n0", 3*SDO->N_n);
      NI_add_column(nel, NI_FLOAT, SDO->n0);
      NI_add_to_group( ngr, nel); 
   }
   if (SDO->n1) {
      nel = NI_new_data_element("n1", 3*SDO->N_n);
      NI_add_column(nel, NI_FLOAT, SDO->n1);
      NI_add_to_group( ngr, nel); 
   }
   if (SDO->colv) {
      nel = NI_new_data_element("colv", 4*SDO->N_n);
      NI_add_column(nel, NI_FLOAT, SDO->colv);
      NI_add_to_group( ngr, nel); 
   }
   if (SDO->thickv) {
      nel = NI_new_data_element("thickv", SDO->N_n);
      NI_add_column(nel, NI_FLOAT, SDO->thickv);
      NI_add_to_group( ngr, nel); 
   }
   NI_SET_INT(ngr, "Stipple", SDO->Stipple);
   
   SUMA_RETURN(ngr);
}

SUMA_SegmentDO *SUMA_niSDO2SDO(NI_group *ngr) 
{
   static char FuncName[]={"SUMA_niSDO2SDO"};
   SUMA_SegmentDO *SDO=NULL;
   NI_element *nel = NULL;
   int N_n, oriented, ncp=0, NodeBased=-1;
   SUMA_DO_Types type, Parent_type;
   char att[500], *Parent_idcode_str=NULL, *Label=NULL, 
        *idcode_str=NULL, *DrawnDO_variant= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ngr) { SUMA_RETURN(SDO); }
   
   if (strcmp(ngr->name, "Segment_DO")) {
      SUMA_S_Err("NIML object not SDO");
      SUMA_RETURN(SDO);
   }
   NI_GET_STR_CP(ngr, "Parent_idcode_str", Parent_idcode_str);
   NI_GET_STR_CP(ngr, "Label", Label);
   NI_GET_INT(ngr, "oriented",oriented);
   NI_GET_INT(ngr, "N_n",N_n);
   NI_GET_INT(ngr, "do_type",type);
   NI_GET_INT(ngr, "NodeBased", NodeBased);   
   if (!NI_GOT) { NodeBased = -1; }
   NI_GET_INT(ngr, "Parent_do_type", Parent_type);
   if (!NI_GOT) { Parent_type = type_not_set; }
   NI_GET_STR_CP(ngr, "DrawnDO_variant", DrawnDO_variant);
   SDO = SUMA_Alloc_SegmentDO(N_n, Label, oriented, Parent_idcode_str, 
                              NodeBased, type, Parent_type, DrawnDO_variant);
   if (Label) SUMA_free(Label); Label = NULL; 
   if (Parent_idcode_str) SUMA_free(Parent_idcode_str); Parent_idcode_str = NULL;
   SUMA_ifree(DrawnDO_variant);
   
   NI_GET_STR_CP(ngr, "idcode_str", idcode_str);
   if (idcode_str) {
      SUMA_LHv("Have id %s in niml, replacing SDO's\n", idcode_str);
      SUMA_STRING_REPLACE(SDO->idcode_str, idcode_str);
      SUMA_free(idcode_str); idcode_str = NULL;
   }else {
      SUMA_LH("Have no id in niml");
   }
   
   NI_GET_FLOAT(ngr, "LineWidth", SDO->LineWidth);
   NI_GET_FLOATv(ngr, "LineCol", SDO->LineCol, 4,LocalHead);
   nel = SUMA_FindNgrNamedElement(ngr, "NodeID");
   if (!nel) {
      SDO->NodeID = NULL;
   } else {
      SDO->NodeID = 
            (int *)SUMA_Copy_Part_Column(nel->vec[0], 
                              NI_rowtype_find_code(nel->vec_typ[0]), 
                              nel->vec_len, NULL, 0, &ncp);
   }
   nel = SUMA_FindNgrNamedElement(ngr, "NodeID1");
   if (!nel) {
      SDO->NodeID1 = NULL;
   } else {
      SDO->NodeID1 = 
            (int *)SUMA_Copy_Part_Column(nel->vec[0], 
                              NI_rowtype_find_code(nel->vec_typ[0]), 
                              nel->vec_len, NULL, 0, &ncp);
   }
   nel = SUMA_FindNgrNamedElement(ngr, "n0");
   if (!nel) {
      SDO->n0 = NULL;
   } else {
      SDO->n0 = (float *)SUMA_Copy_Part_Column(nel->vec[0], NI_rowtype_find_code(nel->vec_typ[0]), nel->vec_len, NULL, 0, &ncp);
   }
   nel = SUMA_FindNgrNamedElement(ngr, "n1");
   if (!nel) {
      SDO->n1 = NULL;
   } else {
      SDO->n1 = (float *)SUMA_Copy_Part_Column(nel->vec[0], NI_rowtype_find_code(nel->vec_typ[0]), nel->vec_len, NULL, 0, &ncp);
   }
   nel = SUMA_FindNgrNamedElement(ngr, "colv");
   if (!nel) {
      SDO->colv = NULL;
   } else {
      SDO->colv = (float *)SUMA_Copy_Part_Column(nel->vec[0], NI_rowtype_find_code(nel->vec_typ[0]), nel->vec_len, NULL, 0, &ncp);
   }
   nel = SUMA_FindNgrNamedElement(ngr, "thickv");
   if (!nel) {
      SDO->thickv = NULL;
   } else {
      SDO->thickv = (float *)SUMA_Copy_Part_Column(nel->vec[0], NI_rowtype_find_code(nel->vec_typ[0]), nel->vec_len, NULL, 0, &ncp);
   }
   NI_GET_INT(ngr, "Stipple", SDO->Stipple);
   
   SUMA_RETURN(SDO);
}


SUMA_SegmentDO *SUMA_CreateSegmentDO(  
         int N_n, int oriented, int NodeBased, int Stipple,
         char *Label, char *idcode_str, 
         char *Parent_idcode_str, SUMA_DO_Types Parent_do_type,
         char *DrawnDO_variant, 
         float LineWidth, float *LineCol,
         int *NodeID, int *NodeID1, float *n0, float *n1,
         float *colv, float *thickv ) 
{
   static char FuncName[]={"SUMA_CreateSegmentDO"};
   SUMA_SegmentDO *SDO=NULL;
   int ncp=0, i;
   SUMA_DO_Types type;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (oriented) type = LS_type;
   else type = OLS_type;
   
   SDO = SUMA_Alloc_SegmentDO(N_n, Label, oriented, Parent_idcode_str, 
                              NodeBased, type, Parent_do_type, DrawnDO_variant);
   if (idcode_str) SUMA_STRING_REPLACE(SDO->idcode_str, idcode_str);   
   SDO->NodeBased = NodeBased;
   SDO->Stipple = Stipple;
   SDO->LineWidth =LineWidth;
   if (LineCol) { for (i=0; i<4; ++i) SDO->LineCol[i] = LineCol[i]; }
   else {   SDO->LineCol[0] = 0.4; SDO->LineCol[1] = 0.8; 
            SDO->LineCol[2] = 0.1; SDO->LineCol[3] = 1.0; }

   if (NodeID) {
      SDO->NodeID = (int *)SUMA_Copy_Part_Column(
                              (void*)NodeID, NI_rowtype_find_code(NI_INT), 
                              N_n, NULL, 0, &ncp);
   } else SDO->NodeID = NULL;   
   if (NodeID1) {
      SDO->NodeID1 = (int *)SUMA_Copy_Part_Column(
                              (void*)NodeID1, NI_rowtype_find_code(NI_INT), 
                              N_n, NULL, 0, &ncp);
   } else SDO->NodeID1 = NULL; 
   if (!n0) {
      SDO->n0 = NULL;
   } else {
      SDO->n0 = (float *)SUMA_Copy_Part_Column(
                           (void *)n0,  NI_rowtype_find_code(NI_FLOAT), 
                           3*N_n, NULL, 0, &ncp);
   }
   if (!n1) {
      SDO->n1 = NULL;
   } else {
      SDO->n1 = (float *)SUMA_Copy_Part_Column(
                           (void *)n1,  NI_rowtype_find_code(NI_FLOAT), 
                           3*N_n, NULL, 0, &ncp);
   }
   if (!colv) {
      SDO->colv = NULL;
   } else {
      SDO->colv = (float *)SUMA_Copy_Part_Column(
                           (void *)colv,  NI_rowtype_find_code(NI_FLOAT), 
                           4*N_n, NULL, 0, &ncp);
   }
   if (!thickv) {
      SDO->thickv = NULL;
   } else {
      SDO->thickv = (float *)SUMA_Copy_Part_Column(
                              (void *)thickv,  NI_rowtype_find_code(NI_FLOAT), 
                              N_n, NULL, 0, &ncp);
   }
   
   SUMA_RETURN(SDO);
}

GLenum SUMA_SphereStyleConvert (int v)
{
   switch (v) {
      case 0:
         return(GLU_POINT);
      case 1:
         return(GLU_LINE);
      case 2:
         return(GLU_FILL);
      default:
         return(GLU_FILL);
   }
}

SUMA_SphereDO * SUMA_ReadSphDO (char *s)
{
   static char FuncName[]={"SUMA_ReadSphDO"};
   SUMA_SphereDO *SDO = NULL;
   MRI_IMAGE * im = NULL;
   float *far=NULL;
   int itmp, itmp2, icol_rad=-1, icol_style = -1, icol_col = -1;
   int nrow=-1, ncol=-1;
   
   SUMA_ENTRY;
      
   if (!s) {
      SUMA_SLP_Err("NULL s");
      SUMA_RETURN(NULL);
   }
   
   im = mri_read_1D (s);

   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NULL);
   }

   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;

   if (!ncol) {
      SUMA_SLP_Err("Empty file");
      SUMA_RETURN(NULL);
   }
   
   icol_col = -1;
   icol_rad = -1;
   icol_style = -1;
   switch (nrow) {
      case 3:
         fprintf(SUMA_STDERR,"%s: Sphere file %s's format:\n"
                              "ox oy oz\n", FuncName, s);
         break;
      case 4:
         fprintf(SUMA_STDERR,"%s: Sphere file %s's format:\n"
                              "ox oy oz rd\n", FuncName, s);
         icol_rad = 3;
         break;
      case 5:
         fprintf(SUMA_STDERR,"%s: Sphere file %s's format:\n"
                              "ox oy oz rd st\n", FuncName, s);
         icol_rad = 3;
         icol_style = 4;
         break;
      case 7:
         fprintf(SUMA_STDERR,"%s: Sphere file %s's format:\n"
                              "ox oy oz c0 c1 c2 c3\n", FuncName, s);
         icol_col = 3;
         break;
      case 8:
         fprintf(SUMA_STDERR,"%s: Sphere file %s's format:\n"
                              "ox oy oz c0 c1 c2 c3 rd\n", FuncName, s);
         icol_col = 3;
         icol_rad = 7;
         break;
      case 9:
         fprintf(SUMA_STDERR,"%s: Sphere file %s's format:\n"
                              "ox oy oz c0 c1 c2 c3 rd st\n", FuncName, s);
         icol_col = 3;
         icol_rad = 7;
         icol_style = 8;
         break;
      default:
         SUMA_SLP_Err("File must have\n"
                   "3,4,5,7,8 or 9 columns.");
         mri_free(im); im = NULL;   /* done with that baby */
         SUMA_RETURN(NULL);
   }

   /* allocate for segments DO */
   SDO = SUMA_Alloc_SphereDO (ncol, s, NULL, SP_type);
   if (!SDO) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Allocate_SphereDO.\n", FuncName);
      SUMA_RETURN(NULL);
   }

   /* fill up SDO */
   itmp = 0;
   while (itmp < SDO->N_n) {
      itmp2 = 3*itmp;
      SDO->cxyz[itmp2]   = far[itmp       ];
      SDO->cxyz[itmp2+1] = far[itmp+  ncol];
      SDO->cxyz[itmp2+2] = far[itmp+2*ncol];
      ++itmp;
   } 
   
   if (icol_col > 0) {
      SDO->colv = (GLfloat *)SUMA_malloc(4*sizeof(GLfloat)*SDO->N_n);
      if (!SDO->colv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual colors */
      itmp = 0;
      while (itmp < SDO->N_n) {
         itmp2 = 4*itmp;
         SDO->colv[itmp2]     = far[itmp+(icol_col  )*ncol];
         SDO->colv[itmp2+1]   = far[itmp+(icol_col+1)*ncol];
         SDO->colv[itmp2+2]   = far[itmp+(icol_col+2)*ncol];
         SDO->colv[itmp2+3]   = far[itmp+(icol_col+3)*ncol];
         ++itmp;
      } 
   }
   if (icol_rad > 0) {
      SDO->radv = (GLfloat *)SUMA_malloc(sizeof(GLfloat)*SDO->N_n);
      if (!SDO->radv) {
         SUMA_SL_Crit("Failed in to allocate for radv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual radius */
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->radv[itmp]     = far[itmp+(icol_rad  )*ncol];
         ++itmp;
      } 
   }
   if (icol_style > 0) {
      SDO->stylev = (GLenum *)SUMA_malloc(sizeof(GLenum)*SDO->N_n);
      if (!SDO->stylev) {
         SUMA_SL_Crit("Failed in to allocate for radv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual style */
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->stylev[itmp] = SUMA_SphereStyleConvert(
                                 (int)far[itmp+(icol_style  )*ncol]);
         ++itmp;
      } 
   }
   mri_free(im); im = NULL; far = NULL;

   SUMA_RETURN(SDO);
}

int SUMA_TDO_N_tracts(SUMA_TractDO *tdo) 
{
   static char FuncName[]={"SUMA_TDO_N_tracts"};
   int ntr = -1, ii;
   TAYLOR_BUNDLE *tb=NULL;
   
   SUMA_ENTRY;
   
   if (!tdo || !tdo->net) SUMA_RETURN(ntr);   
   
   ntr = 0;
   for (ii=0; ii<tdo->net->N_tbv; ++ii) {
      if ((tb = tdo->net->tbv[ii])) ntr += tb->N_tracts;
   }
   
   SUMA_RETURN(ntr);
}

float *SUMA_TDO_Grid_Center(SUMA_TractDO *tdo, float *here) 
{
   static char FuncName[]={"SUMA_TDO_Grid_Center"};
   static int icall=0;
   static float fv[10][3];
   float A[4][4], I[3];
   THD_3dim_dataset *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   here[0] = here[1] = here[2] =  0.0;
   
   if (!tdo || !tdo->net || !tdo->net->grid) SUMA_RETURN(here);
   
   dset = tdo->net->grid;
   
   if (  !dset->daxes || 
         !ISVALID_MAT44(dset->daxes->ijk_to_dicom_real)) {
      SUMA_S_Err("no valid ijk_to_dicom_real") ;
      SUMA_RETURN(here);
   }
   MAT44_TO_AFF44(A, dset->daxes->ijk_to_dicom_real);
   I[0] = DSET_NX(dset)/2.0; 
   I[1] = DSET_NY(dset)/2.0;
   I[2] = DSET_NZ(dset)/2.0;
   AFF44_MULT_I(here, A, I);
   
   SUMA_RETURN(here);
}

float *SUMA_TDO_Points_Center(SUMA_TractDO *tdo, float *here) 
{
   static char FuncName[]={"SUMA_TDO_Points_Center"};
   static int icall=0;
   static float fv[10][3];
   int ii, jj, kk, npts = 0;
   TAYLOR_BUNDLE *tb=NULL;
   TAYLOR_TRACT *tt=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   here[0] = here[1] = here[2] =  0.0;
   
   if (!tdo || !tdo->net || !tdo->net->tbv) SUMA_RETURN(here);
   
   npts = 0;
   for (ii=0; ii<tdo->net->N_tbv; ++ii) {
      if ((tb = tdo->net->tbv[ii])) {
         for (jj=0; jj<tb->N_tracts; ++jj) {
            if ((tt = tb->tracts+jj) && tt->N_pts3>2) {
               kk=0;
               do {
                  here[0] += tt->pts[kk++];
                  here[1] += tt->pts[kk++];
                  here[2] += tt->pts[kk++];
               } while (kk<tt->N_pts3);
               npts += tt->N_pts3/3;
            }
         } 
      }
   }
   
   here[0] /= (float)npts;
   here[1] /= (float)npts;
   here[2] /= (float)npts;
   
   SUMA_RETURN(here);
}
float *SUMA_TDO_XYZ_Range(SUMA_TractDO *tdo, float *here) 
{
   static char FuncName[]={"SUMA_TDO_XYZ_Range"};
   static int icall=0;
   static float fv[10][6];
   int ii, jj, kk, ok;
   TAYLOR_BUNDLE *tb=NULL;
   TAYLOR_TRACT *tt=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   here[0] = here[2] = here[4] =  -20;
   here[1] = here[3] = here[5] =   20;
   if (!tdo || !tdo->net || !tdo->net->tbv) SUMA_RETURN(here);
   
   here[0] = here[2] = here[4] =   2000000000000;
   here[1] = here[3] = here[5] =  -2000000000000;
   ok = 0;
   for (ii=0; ii<tdo->net->N_tbv; ++ii) {
      if ((tb = tdo->net->tbv[ii])) {
         for (jj=0; jj<tb->N_tracts; ++jj) {
            if ((tt = tb->tracts+jj) && tt->N_pts3>2) {
               kk=0;
               do {
                  if (here[0] > tt->pts[kk]) here[0] = tt->pts[kk];
                  if (here[1] < tt->pts[kk]) here[1] = tt->pts[kk]; ++kk;
                  
                  if (here[2] > tt->pts[kk]) here[2] = tt->pts[kk];
                  if (here[3] < tt->pts[kk]) here[3] = tt->pts[kk]; ++kk;
               
                  if (here[4] > tt->pts[kk]) here[4] = tt->pts[kk];
                  if (here[5] < tt->pts[kk]) here[5] = tt->pts[kk]; ++kk;
               } while (kk<tt->N_pts3);
               if (!ok && kk>0) ok=1;
            }
         } 
      }
   }
   
   if (!ok) { /* weird, nothing at all, revert to defaults */
      here[0] = here[2] = here[4] =  -20;
      here[1] = here[3] = here[5] =   20;
   }
   
   SUMA_RETURN(here);
}

SUMA_TractDO *SUMA_ReadTractDO(char *s, char *parent_SO_id) 
{
   static char FuncName[]={"SUMA_ReadTractDO"};
   TAYLOR_NETWORK *net=NULL;
   SUMA_TractDO *TDO=NULL;
   
   SUMA_ENTRY;
      
   if (!s) {
      SUMA_SLP_Err("NULL s");
      SUMA_RETURN(NULL);
   }
   if (!(net = Read_Network(s))) {
      SUMA_S_Errv("Failed to read %s\n",s);
      SUMA_RETURN(NULL);
   }
   if (!(TDO = SUMA_Alloc_TractDO (s, parent_SO_id))) {
      SUMA_S_Err("Failed to init TDO.");
      SUMA_RETURN(NULL);
   }
   
   TDO->net = net;
   
   SUMA_RETURN(TDO);
}

SUMA_SphereDO * SUMA_ReadNBSphDO (char *s, char *parent_SO_id)
{
   static char FuncName[]={"SUMA_ReadNBSphDO"};
   SUMA_SphereDO *SDO = NULL;
   MRI_IMAGE * im = NULL;
   float *far=NULL;
   int itmp, itmp2, icol_rad=-1, icol_style = -1, icol_col = -1, icol_id = -1;
   int nrow=-1, ncol=-1;
   
   SUMA_ENTRY;
      
   if (!s) {
      SUMA_SLP_Err("NULL s");
      SUMA_RETURN(NULL);
   }
   if (!parent_SO_id) {
      SUMA_SLP_Err("NULL parent_SO_id");
      SUMA_RETURN(NULL);
   }
   im = mri_read_1D (s);

   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NULL);
   }

   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;

   if (!ncol) {
      SUMA_SLP_Err("Empty file");
      SUMA_RETURN(NULL);
   }
   
   icol_id = -1;
   icol_col = -1;
   icol_rad = -1;
   icol_style = -1;
   switch (nrow) {
      case 1:
         fprintf(SUMA_STDERR,"%s: Node-Based Sphere file %s's format:\n"
                              "n\n", FuncName, s);
         icol_id = 0;                     
         break;
      case 2:
         fprintf(SUMA_STDERR,"%s: Node-Based Sphere file %s's format:\n"
                              "n rd\n", FuncName, s);
         icol_id = 0; 
         icol_rad = 1;
         break;
      case 3:
         fprintf(SUMA_STDERR,"%s: Node-Based Sphere file %s's format:\n"
                              "n rd st\n", FuncName, s);
         icol_id = 0; 
         icol_rad = 1;
         icol_style = 2;
         break;
      case 5:
         fprintf(SUMA_STDERR,"%s: Node-Based Sphere file %s's format:\n"
                              "n c0 c1 c2 c3\n", FuncName, s);
         icol_id = 0; 
         icol_col = 1;
         break;
      case 6:
         fprintf(SUMA_STDERR,"%s: Node-Based Sphere file %s's format:\n"
                              "n c0 c1 c2 c3 rd\n", FuncName, s);
         icol_id = 0;
         icol_col = 1;
         icol_rad = 5;
         break;
      case 7:
         fprintf(SUMA_STDERR,"%s: Node-Based Sphere file %s's format:\n"
                              "n c0 c1 c2 c3 rd st\n", FuncName, s);
         icol_id = 0;
         icol_col = 1;
         icol_rad = 5;
         icol_style = 6;
         break;
      default:
         SUMA_SLP_Err("Node-Based Sphere File must have\n"
                   "1,2,3,5,6 or 7 columns.");
         mri_free(im); im = NULL;   /* done with that baby */
         SUMA_RETURN(NULL);
   }

   /* allocate for segments DO */
   SDO = SUMA_Alloc_SphereDO (ncol, s, parent_SO_id, NBSP_type);
   if (!SDO) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Allocate_SphereDO.\n", FuncName);
      SUMA_RETURN(NULL);
   }

   /* fill up SDO */
   itmp = 0;
   while (itmp < SDO->N_n) {
      SDO->NodeID[itmp] = far[itmp +(icol_id  )*ncol];
      ++itmp;
   } 
   
   if (icol_col > 0) {
      SDO->colv = (GLfloat *)SUMA_malloc(4*sizeof(GLfloat)*SDO->N_n);
      if (!SDO->colv) {
         SUMA_SL_Crit("Failed in to allocate for colv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual colors */
      itmp = 0;
      while (itmp < SDO->N_n) {
         itmp2 = 4*itmp;
         SDO->colv[itmp2]     = far[itmp+(icol_col  )*ncol];
         SDO->colv[itmp2+1]   = far[itmp+(icol_col+1)*ncol];
         SDO->colv[itmp2+2]   = far[itmp+(icol_col+2)*ncol];
         SDO->colv[itmp2+3]   = far[itmp+(icol_col+3)*ncol];
         ++itmp;
      } 
   }
   if (icol_rad > 0) {
      SDO->radv = (GLfloat *)SUMA_malloc(sizeof(GLfloat)*SDO->N_n);
      if (!SDO->radv) {
         SUMA_SL_Crit("Failed in to allocate for radv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual radius */
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->radv[itmp]     = far[itmp+(icol_rad  )*ncol];
         ++itmp;
      } 
   }
   if (icol_style > 0) {
      SDO->stylev = (GLenum *)SUMA_malloc(sizeof(GLenum)*SDO->N_n);
      if (!SDO->stylev) {
         SUMA_SL_Crit("Failed in to allocate for radv.");
         SUMA_RETURN(NULL);
      }
      /* fill up idividual style */
      itmp = 0;
      while (itmp < SDO->N_n) {
         SDO->stylev[itmp]     = SUMA_SphereStyleConvert((int)far[itmp+(icol_style  )*ncol]);
         ++itmp;
      } 
   }
   mri_free(im); im = NULL; far = NULL;

   SUMA_RETURN(SDO);
}

/* A function to take a single DO and propagate it so that it is
reprenseted on all nodes on the surface.
Basically, the function returns one NIDO with a element at each node.
There can only be one element, not a whole NIDO duplicated at each node 

An alternalte approach is in SUMA_Multiply_NodeNIDOObjects

An alternate approach, would return a vector of NIDO* with one NIDO pointer
for each node. This way one could put a whole lot of stuff in there.
At rendering time, instead of making one call to DrawNIDO, one calls 
DrawNIDO for each node that has a NIDO 
*/
SUMA_DO * SUMA_Multiply_NodeObjects ( SUMA_SurfaceObject *SO, 
                                      SUMA_DO *DO )
{
   static char FuncName[]={"SUMA_Multiply_NodeObjects"};
   SUMA_DO *DDO = NULL;
   SUMA_NIDO *nido=NULL, *niout=NULL;
   void *vel=NULL;
   char *atr=NULL;
   int i=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !DO) SUMA_RETURN(NULL);
   
   SUMA_LHv("Switch on DO type %s and coord type %d\n", 
            SUMA_ObjectTypeCode2ObjectTypeName(DO->ObjectType), 
            DO->CoordType);
   switch(DO->ObjectType) {
      case NIDO_type:
         nido=(SUMA_NIDO *)DO->OP;
         /* Easiest to duplicate the nido for each of the nodes */
         /* although its not efficient for fast drawing, it makes */
         /* is real easy to encode a boat load of information, and use*/
         /* drawNIDO without much headaches */
         if (nido->ngr->part_num != 1) {
            SUMA_S_Errv( "CommonNodeMarker has %d elements in it.\n"
                         "Not common I'd say.\n", nido->ngr->part_num);
            SUMA_RETURN(DDO);
         }
         if (nido->ngr->part_typ[0] !=  NI_ELEMENT_TYPE) {
            SUMA_S_Err("Not ready to duplicate anything but NI_ELEMENT_TYPE");
            SUMA_RETURN(DDO);
         }
         niout = SUMA_Alloc_NIDO(NULL, "from_CommonNodeObject", SO->idcode_str);
         if ((atr=NI_get_attribute(nido, "bond"))) 
            NI_set_attribute(niout->ngr, "bond", atr);
         else   NI_set_attribute(niout->ngr, "bond", "s");
         if ((atr=NI_get_attribute(nido, "coord_type"))) 
            NI_set_attribute(niout->ngr, "coord_type", atr);
         else NI_set_attribute(niout->ngr, "coord_type",  
                     SUMA_CoordTypeName(SUMA_CoordType(NULL)));
         if ((atr=NI_get_attribute(nido, "default_font"))) 
            NI_set_attribute(niout->ngr, "default_font", atr);
         else NI_set_attribute(niout->ngr, "default_font",  
                     SUMA_glutBitmapFontName(SUMA_glutBitmapFont(NULL)));
         if ((atr=NI_get_attribute(nido, "default_color"))) 
            NI_set_attribute(niout->ngr, "default_color", atr);
         else NI_set_attribute(niout->ngr, "default_color",  
                                                "1.0 1.0 1.0 1.0");
         
         /* Now for each node, create a new copy of that element */
         for (i=0; i<SO->N_Node; ++i) {
            if ((vel = NI_duplicate(nido->ngr->part[0], 1))) {
               /* assign object to the node */
               NI_SET_INT((NI_element *)vel, "node", i);
               NI_add_to_group(niout->ngr, vel);
            } else {
               SUMA_S_Err("Failed to create duplicate element");
               SUMA_RETURN(DDO);
            }
         }
         break;
      default:
         SUMA_S_Errv("Sorry Chip, goose %s (%d) ain't ready to fly.\n", 
                     SUMA_ObjectTypeCode2ObjectTypeName(DO->ObjectType),
                     DO->ObjectType);
         SUMA_RETURN(NULL);
   }           
   
   /* Now put niout in DDO */
   DDO = (SUMA_DO *)SUMA_calloc(1,sizeof(SUMA_DO));
   
   DDO->OP = (void *)niout; 
   DDO->ObjectType = NIDO_type;
   DDO->CoordType = SUMA_WORLD;
   niout = NULL;
   
   SUMA_RETURN(DDO);
}

/*
An alternalte approach to SUMA_Multiply_NodeObjects

An alternate approach, would return a vector of NIDO* with one NIDO pointer
for each node. This way one could put a whole lot of stuff in there.
At rendering time, instead of making one call to DrawNIDO, one calls 
DrawNIDO for each node that has a NIDO 
*/
SUMA_NIDO ** SUMA_Multiply_NodeNIDOObjects ( SUMA_SurfaceObject *SO, 
                                      SUMA_DO *DO )
{
   static char FuncName[]={"SUMA_Multiply_NodeNIDOObjects"};
   SUMA_NIDO **NIDOv = NULL;
   SUMA_NIDO *nido=NULL, *niout=NULL;
   void *vel=NULL;
   char *atr=NULL;
   int i=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !DO) SUMA_RETURN(NULL);
   
   SUMA_LHv("Switch on DO type %s and coord type %d\n", 
            SUMA_ObjectTypeCode2ObjectTypeName(DO->ObjectType), 
            DO->CoordType);
   switch(DO->ObjectType) {
      case NIDO_type:
         nido=(SUMA_NIDO *)DO->OP;
         
         NIDOv = (SUMA_NIDO **)SUMA_calloc(SO->N_Node, sizeof(SUMA_NIDO*));
         /* Easiest to duplicate the nido for each of the nodes */
         /* although its not efficient for fast drawing, it makes */
         /* is real easy to encode a boat load of information, and use*/
         /* drawNIDO without much headaches */
         
         /* Now for each node, create a new copy of that element */
         for (i=0; i<SO->N_Node; ++i) {
            if ((vel = NI_duplicate(nido->ngr, 1))) {
               /* assign object to the node */
               NI_SET_INT((NI_element *)vel, "default_node", i);
               niout = SUMA_Alloc_NIDO(NULL, 
                        "from_CommonNodeObject", SO->idcode_str);
               niout->ngr = vel;
               NIDOv[i] = niout; niout = NULL;
            } else {
               SUMA_S_Err("Failed to create duplicate element");
               SUMA_RETURN(NULL);
            }
         }
         break;
      default:
         SUMA_S_Errv("Sorry Chip, goose %s (%d) ain't ready to fly.\n", 
                     SUMA_ObjectTypeCode2ObjectTypeName(DO->ObjectType),
                     DO->ObjectType);
         SUMA_RETURN(NULL);
   }           
   
   SUMA_RETURN(NIDOv);
}


SUMA_PlaneDO * SUMA_ReadPlaneDO (char *s)
{
   static char FuncName[]={"SUMA_ReadPlaneDO"};
   SUMA_PlaneDO *SDO = NULL;
   MRI_IMAGE * im = NULL;
   float *far=NULL;
   int itmp, itmp2, icol_eq=-1, icol_cent = -1;
   int nrow=-1, ncol=-1;
   
   SUMA_ENTRY;
      
   if (!s) {
      SUMA_SLP_Err("NULL s");
      SUMA_RETURN(NULL);
   }
   
   im = mri_read_1D (s);

   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NULL);
   }

   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;

   if (!ncol) {
      SUMA_SLP_Err("Empty file");
      SUMA_RETURN(NULL);
   }
   
   icol_eq = -1;
   icol_cent = -1;
   switch (nrow) {
      case 7:
         fprintf(SUMA_STDERR,"%s: Plane file %s's format:\n"
                              "a b c d cx cy cz\n", FuncName, s);
         icol_eq = 0;
         icol_cent = 4;
         break;
      default:
         SUMA_SLP_Err(  "File must have\n"
                        "7 columns.");
         mri_free(im); im = NULL;   /* done with that baby */
         SUMA_RETURN(NULL);
   }

   /* allocate for  DO */
   SDO = SUMA_Alloc_PlaneDO (ncol, s, PL_type);
   if (!SDO) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Allocate_PlaneDO.\n", FuncName);
      SUMA_RETURN(NULL);
   }

   /* fill up SDO */
   /* fill up equations */
   itmp = 0;
   while (itmp < SDO->N_n) {
      itmp2 = 4*itmp;
      SDO->pleq[itmp2]     = far[itmp+(icol_eq  )*ncol];
      SDO->pleq[itmp2+1]   = far[itmp+(icol_eq+1)*ncol];
      SDO->pleq[itmp2+2]   = far[itmp+(icol_eq+2)*ncol];
      SDO->pleq[itmp2+3]   = far[itmp+(icol_eq+3)*ncol];
      ++itmp;
   } 
   /* fill up centers */
   itmp = 0;
   while (itmp < SDO->N_n) {
      itmp2 = 3*itmp;
      SDO->cxyz[itmp2]   = far[itmp+(icol_cent  )*ncol];
      SDO->cxyz[itmp2+1] = far[itmp+(icol_cent+1)*ncol];
      SDO->cxyz[itmp2+2] = far[itmp+(icol_cent+2)*ncol];
      ++itmp;
   } 
   
   mri_free(im); im = NULL; far = NULL;

   SUMA_RETURN(SDO);
}

/*! Allocate for a axis object */
SUMA_Axis* SUMA_Alloc_Axis (const char *Name, SUMA_DO_Types type)
{   
   static char FuncName[]={"SUMA_Alloc_Axis"};
   SUMA_Axis* Ax;

   SUMA_ENTRY;

   Ax = (SUMA_Axis *) SUMA_calloc(1,sizeof (SUMA_Axis));
   if (Ax == NULL) {
      fprintf(stderr,"SUMA_Alloc_Axis Error: Failed to allocate Ax\n");
      SUMA_RETURN (Ax);
   }
   Ax->do_type = type;
   
   /* setup some default values */
   Ax->atype = SUMA_STD_ZERO_CENTERED;
   Ax->XaxisColor[0] = 1.0;
   Ax->XaxisColor[1] = 0.0;
   Ax->XaxisColor[2] = 0.0;
   Ax->XaxisColor[3] = 1.0;
   
   Ax->YaxisColor[0] = 0.0;
   Ax->YaxisColor[1] = 1.0;
   Ax->YaxisColor[2] = 0.0;
   Ax->YaxisColor[3] = 1.0;
   
   Ax->ZaxisColor[0] = 0.0;
   Ax->ZaxisColor[1] = 0.0;
   Ax->ZaxisColor[2] = 1.0;
   Ax->ZaxisColor[3] = 1.0;
   
   Ax->LineWidth = 1.0;
   Ax->Stipple = SUMA_SOLID_LINE;
   Ax->XYZspan[0] = Ax->XYZspan[1] = Ax->XYZspan[2] = 800;
   
   Ax->Center[0] = Ax->Center[1] = Ax->Center[2] = 0.0;
   
   if (Name != NULL) {
      if (strlen(Name) > SUMA_MAX_LABEL_LENGTH-1) {
         fprintf(SUMA_STDERR, "Error %s: Name too long (> %d).\n",\
            FuncName, SUMA_MAX_LABEL_LENGTH);
         Ax->Label = NULL;
         Ax->idcode_str = NULL;
      } else {
         Ax->Label = (char *)SUMA_calloc (strlen(Name)+1, sizeof(char));
         Ax->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH+1, sizeof(char));
         if (Ax->Label == NULL) {
            fprintf(SUMA_STDERR,"Error %s: Failed to allocate for Ax->Name.\n", \
               FuncName);
         }
         sprintf(Ax->Label, "%s", Name);
         UNIQ_idcode_fill(Ax->idcode_str); 
      }
      
   }
   SUMA_RETURN (Ax);
}
void SUMA_Free_Axis (SUMA_Axis *Ax)
{
   static char FuncName[]={"SUMA_Free_Axis"};
   
   SUMA_ENTRY;

   if (Ax->Label != NULL) SUMA_free(Ax->Label);
   if (Ax->idcode_str != NULL) SUMA_free(Ax->idcode_str);
   if (Ax) SUMA_free(Ax);
   SUMA_RETURNe;
}

void SUMA_EyeAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *csv)
{
   static char FuncName[]={"SUMA_EyeAxisStandard"};
   SUMA_Boolean LocalHead = NOPE;
   char buf[200];
   
   SUMA_ENTRY;

   Ax->Stipple = SUMA_DASHED_LINE;
   Ax->XYZspan[0] = Ax->XYZspan[1] = Ax->XYZspan[2] = 1000.0;
   Ax->Center[0] = csv->GVS[csv->StdView].ViewCenter[0];
   Ax->Center[1] = csv->GVS[csv->StdView].ViewCenter[1];
   Ax->Center[2] = csv->GVS[csv->StdView].ViewCenter[2];
   SUMA_RETURNe;
}

void SUMA_MeshAxisStandard (SUMA_Axis* Ax, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_MeshAxisStandard"};
   SUMA_SurfaceViewer *sv=NULL;
   SUMA_SurfaceObject *cso=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!ado || !Ax) SUMA_RETURNe;
   
   switch (ado->do_type) {
      case SO_type: 
         cso=(SUMA_SurfaceObject *)ado;
         Ax->Stipple = SUMA_SOLID_LINE;
         Ax->XYZspan[0]= Ax->XYZspan[1]= Ax->XYZspan[2]= 100.0;
         Ax->BR[0][0] = cso->MinDims[0]; Ax->BR[0][1] = cso->MaxDims[0];
         Ax->BR[1][0] = cso->MinDims[1]; Ax->BR[1][1] = cso->MaxDims[1];
         Ax->BR[2][0] = cso->MinDims[2]; Ax->BR[2][1] = cso->MaxDims[2];
         Ax->Center[0] = cso->Center[0];
         Ax->Center[1] = cso->Center[1];
         Ax->Center[2] = cso->Center[2];
         sv = SUMA_BestViewerForDO(ado);
         if (!sv) sv = SUMAg_SVv;
         Ax->MTspace = 10; 
         Ax->mTspace = 2;
         Ax->MTsize = 4; Ax->mTsize = 2;
         SUMA_LHv("MTspace = %f, DimSclFac = %f\n", 
            Ax->MTspace, sv->GVS[sv->StdView].DimSclFac);
         {
            char *eee = getenv("SUMA_UseCrossTicks");
            if (eee) {
               SUMA_TO_LOWER(eee);
               if (strcmp (eee, "yes") == 0) Ax->DoCross = 1;
                  else Ax->DoCross = 0;
            } else {
               Ax->DoCross = 0;
            }
         }
         break;
      case GRAPH_LINK_type:
         SUMA_S_Notev("Nothing done for GRAPH_LINK_type, variant %s yet\n",
                         SUMA_ADO_variant(ado));
         break;
      default:
         SUMA_S_Errv("Not ready for type %s\n",
         SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         break;
   }

   SUMA_RETURNe;
}

void SUMA_WorldAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_WorldAxisStandard"};
   float MinDims[3], MaxDims[3], *xyzr;
   int i, j, Nsel, *Vis_IDs=NULL;
   SUMA_SurfaceObject *cso=NULL;
   SUMA_DSET *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!Ax) {
      SUMA_SL_Err("NULL Ax!");
      SUMA_RETURNe;
   }
   
   Ax->Stipple = SUMA_SOLID_LINE;
   Ax->XYZspan[0]= Ax->XYZspan[1]= Ax->XYZspan[2]= 100.0;
   Ax->MTspace = 10/sv->GVS[sv->StdView].DimSclFac; 
   Ax->mTspace = 2/sv->GVS[sv->StdView].DimSclFac;
   Ax->MTsize = 4/sv->GVS[sv->StdView].DimSclFac; 
   Ax->mTsize = 2/sv->GVS[sv->StdView].DimSclFac;
   {
      char *eee = getenv("SUMA_UseCrossTicks");
      if (eee) {
         SUMA_TO_LOWER(eee);
         if (strcmp (eee, "yes") == 0) Ax->DoCross = 1;
            else Ax->DoCross = 0;
      } else {
         Ax->DoCross = 0;
      }
   }
   
   Ax->Center[0] = sv->GVS[sv->StdView].RotaCenter[0];
   Ax->Center[1] = sv->GVS[sv->StdView].RotaCenter[1];
   Ax->Center[2] = sv->GVS[sv->StdView].RotaCenter[2];

   Vis_IDs = (int *)SUMA_malloc(sizeof(int)*SUMAg_N_DOv);
   Nsel = SUMA_Selectable_ADOs (sv, SUMAg_DOv, Vis_IDs);
   
   /* init */
   MinDims[0]=MinDims[1]=MinDims[2]=1; 
   MaxDims[0]=MaxDims[1]=MaxDims[2]=0; 
   if (Nsel > 0) {
      for (i=0; i<Nsel; ++i) {
         switch (SUMAg_DOv[Vis_IDs[i]].ObjectType) {
            case SO_type:
               cso = (SUMA_SurfaceObject *)SUMAg_DOv[Vis_IDs[i]].OP;
               if (MinDims[0] > MaxDims[0]) {/* initialize */
                  for (j=0; j<3; ++j) {
                     MinDims[j] = cso->MinDims[j];
                     MaxDims[j] = cso->MaxDims[j];
                  }
               } else {
                  for (j=0; j<3; ++j) {
                     if (cso->MinDims[j] < MinDims[j]) 
                        MinDims[j] = cso->MinDims[j];
                     if (cso->MaxDims[j] > MaxDims[j]) 
                        MaxDims[j] = cso->MaxDims[j];
                  }
               }
               break;
            case GRAPH_LINK_type:
               if(!(dset=SUMA_find_GLDO_Dset(
                              (SUMA_GraphLinkDO *)(SUMAg_DOv[Vis_IDs[i]].OP)))){
                  SUMA_S_Err("Gilda");
                  break;
               }
            
               if (!SUMA_IS_REAL_VARIANT(iDO_variant(Vis_IDs[i]))) break;
               xyzr = SUMA_GDSET_XYZ_Range(dset, iDO_variant(Vis_IDs[i]), NULL);
               if (MinDims[0] > MaxDims[0]) {/* initialize */
                  for (j=0; j<3; ++j) {
                     MinDims[j] = xyzr[2*j];
                     MaxDims[j] = xyzr[2*j+1];
                  }
               } else {
                  for (j=0; j<3; ++j) {
                     if (xyzr[2*j] < MinDims[j])  MinDims[j] = xyzr[2*j];
                     if (xyzr[2*j+1] > MaxDims[j]) MaxDims[j] = xyzr[2*j+1];
                  }
               }
               break;
            case TRACT_type: {
               SUMA_TractDO *tdo=(SUMA_TractDO *)SUMAg_DOv[Vis_IDs[i]].OP;
               xyzr = SUMA_TDO_XYZ_Range(tdo, NULL);
               if (MinDims[0] > MaxDims[0]) {/* initialize */
                  for (j=0; j<3; ++j) {
                     MinDims[j] = xyzr[2*j];
                     MaxDims[j] = xyzr[2*j+1];
                  }
               } else {
                  for (j=0; j<3; ++j) {
                     if (xyzr[2*j] < MinDims[j])  MinDims[j] = xyzr[2*j];
                     if (xyzr[2*j+1] > MaxDims[j]) MaxDims[j] = xyzr[2*j+1];
                  }
               }
               break; }
            default:
               SUMA_LHv("Ignoring dims of %s\n", iDO_label(Vis_IDs[i]));
               break;
         }
      }
      if (MinDims[0] > MaxDims[0]) {/* Bad trip */
         SUMA_S_Warn("Nothing good for initialization, using default");
         MinDims[0]=MinDims[1]=MinDims[2]=-20; 
         MaxDims[0]=MaxDims[1]=MaxDims[2]=20; 
      }
      Ax->BR[0][0] = MinDims[0]; Ax->BR[0][1] = MaxDims[0];
      Ax->BR[1][0] = MinDims[1]; Ax->BR[1][1] = MaxDims[1];
      Ax->BR[2][0] = MinDims[2]; Ax->BR[2][1] = MaxDims[2];
   } else {
      SUMA_LH("No Surfs/Selectable DOs, Going with defaults for now");
      Ax->BR[0][0] = -20; Ax->BR[0][1] = 20;
      Ax->BR[1][0] = -20; Ax->BR[1][1] = 20;
      Ax->BR[2][0] = -20; Ax->BR[2][1] = 20;
   }
   if (Vis_IDs) SUMA_free(Vis_IDs);
   
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_DrawSphereDO (SUMA_SphereDO *SDO, SUMA_SurfaceViewer *sv)
{
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0}, comcol[4], *cent=NULL;
   int i, N_n3, i3, ndraw=0, ncross=-1;
   GLfloat rad = 3;
   static char FuncName[]={"SUMA_DrawSphereDO"};
   float origwidth=0.0;
   SUMA_SurfaceObject *SO = NULL;
   byte *mask=NULL;
   byte AmbDiff = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SDO) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (sv && sv->DO_PickMode) {
      SUMA_S_Warn("Function not ready for picking mode, should be fixed");
      SUMA_RETURN(YUP);
   }
   
   if (SDO->NodeBased) { /* Locate the surface in question */
      SUMA_LH("Node-based spheres");
      if (!SDO->Parent_idcode_str) {
         SUMA_SL_Err("Object's parent idcode_str not specified.");
         SUMA_RETURN (NOPE);
      }
      SO = SUMA_findSOp_inDOv(SDO->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv); 
      if (!SO) {
         SUMA_SL_Err("Object's parent surface not found.");
         SUMA_RETURN (NOPE);
      }
      /* masking? */
      if ((ndraw = SUMA_ProcessDODrawMask(sv, SO, &mask, &ncross)) < 0) {
         SUMA_RETURN (NOPE);
      }  
      if (!ndraw) SUMA_RETURN(YUP);/* nothing to draw, nothing wrong */
      SUMA_LHv("ncross=%d\n", ncross);
   } else {
      SUMA_LH("Spheres ");
   }
   
   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   glLineWidth(SDO->LineWidth);
   
   comcol[0] = SDO->CommonCol[0]; /* *SUMAg_SVv[0].dim_amb; Naahhhh */
   comcol[1] = SDO->CommonCol[1]; /* *SUMAg_SVv[0].dim_amb;*/
   comcol[2] = SDO->CommonCol[2]; /* *SUMAg_SVv[0].dim_amb;*/
   comcol[3] = SDO->CommonCol[3]; /* *SUMAg_SVv[0].dim_amb;*/
   
   if (!SDO->colv) {
      if (AmbDiff) {
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, comcol);
      } else {
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
      }
      glMaterialfv(GL_FRONT, GL_EMISSION, comcol);
   }
   if (!SDO->radv) rad = SDO->CommonRad;
   if (!SDO->stylev) {
      gluQuadricDrawStyle (SDO->sphobj, SDO->CommonStyle); 
      if (SDO->CommonStyle == GLU_FILL) 
         gluQuadricNormals (SDO->sphobj , GLU_SMOOTH);
      else gluQuadricNormals (SDO->sphobj , GLU_NONE); 
   }
   
   for (i=0; i<SDO->N_n;++i) {
      i3 = 3*i;
      if (SDO->colv) {
         if (AmbDiff) {
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, &(SDO->colv[i*4]));
         } else {
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
         }
         glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[i*4]));
      }
      if (SDO->radv) rad = SDO->radv[i];
      if (SDO->stylev) {
         gluQuadricDrawStyle (SDO->sphobj, SDO->stylev[i]); 
         if (SDO->stylev[i] == GLU_FILL) 
            gluQuadricNormals (SDO->sphobj , GLU_SMOOTH);
         else gluQuadricNormals (SDO->sphobj , GLU_NONE); 
      }
      if (  SDO->NodeBased && SDO->NodeID[i] < SO->N_Node && 
            DO_DRAW(mask,SDO->NodeID[i],ncross)) {
         cent = &(SO->NodeList[3*SDO->NodeID[i]]);
      } else {
         cent = &(SDO->cxyz[i3]);
      }
      glTranslatef (cent[0], cent[1], cent[2]);
      gluSphere(SDO->sphobj, rad/* *SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06)  
                     User set values, not cool to play with dimensions! */, 
                SDO->CommonSlices, SDO->CommonStacks);
      glTranslatef (-cent[0], -cent[1], -cent[2]);
   }
   
   if (AmbDiff) {
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
   }
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
   glLineWidth(origwidth);

   if (mask) SUMA_free(mask); mask=NULL;
   SUMA_RETURN (YUP);
   
}

SUMA_SphereDO * SUMA_Alloc_SphereDO (  int N_n, char *Label, 
                                       char *Parent_idcode_str, 
                                       SUMA_DO_Types type)
{
   static char FuncName[]={"SUMA_Alloc_SphereDO"};
   SUMA_SphereDO* SDO;
   char *hs = NULL;
   
   SUMA_ENTRY;

   SDO = (SUMA_SphereDO*)SUMA_calloc(1,sizeof (SUMA_SphereDO));
   if (SDO == NULL) {
      fprintf(stderr,"SUMA_Alloc_SphereDO Error: Failed to allocate SDO\n");
      SUMA_RETURN (NULL);
   }
   SDO->do_type = type;
   
   if (N_n > 0) {
      if (!Parent_idcode_str) {
         SDO->NodeBased = 0;
         SDO->NodeID = NULL;
         SDO->Parent_idcode_str = NULL;
         SDO->cxyz = (GLfloat *) SUMA_calloc (3*N_n, sizeof(GLfloat));

         if (!SDO->cxyz) {
            fprintf( stderr,
                     "Error %s: Failed to allocate for SDO->cxyz\n", FuncName);
            if (SDO) SUMA_free (SDO);
            SUMA_RETURN (NULL);
         }
      } else {
         SDO->NodeBased = 1;
         SDO->Parent_idcode_str = SUMA_copy_string(Parent_idcode_str);
         SDO->NodeID = (int*) SUMA_calloc(N_n, sizeof(int));
         if (!SDO->NodeID) {
            fprintf(stderr,
                     "Error %s: Failed to allocate for SDO->NodeID\n", FuncName);
            if (SDO) SUMA_free (SDO);
            SUMA_RETURN (NULL);
         }
         SDO->cxyz = NULL;
      } 
      SDO->N_n = N_n;
   } else {
      SDO->cxyz = NULL;
      SDO->NodeBased = 0;
      SDO->Parent_idcode_str = NULL;
      SDO->NodeID = NULL;
      SDO->N_n = 0;
   }
   
   /* create a string to hash an idcode */
   if (Label) hs = SUMA_copy_string(Label);
   else hs = SUMA_copy_string("NULL_");
   if (Parent_idcode_str) 
      hs = SUMA_append_replace_string(hs,Parent_idcode_str,"_",1);
   else hs = SUMA_append_replace_string(hs,"NULL","",1);
   SDO->idcode_str = UNIQ_hashcode(hs);
   SUMA_free(hs); hs = NULL;

   
   if (Label) {
      SDO->Label = (char *)SUMA_calloc (strlen(Label)+1, sizeof(char));
      SDO->Label = strcpy (SDO->Label, Label);
   } else {
      SDO->Label = NULL;
   }
   
   /* create the ball object*/
   SDO->sphobj = gluNewQuadric();
   
   /* setup some default values */
   SDO->LineWidth = 4.0;
   SDO->CommonRad = 2.0;
   SDO->CommonCol[0] = 1.0; 
   SDO->CommonCol[1] = 1.0; 
   SDO->CommonCol[2] = 1.0; 
   SDO->CommonCol[3] = 1.0; 
   SDO->CommonSlices = 10;
   SDO->CommonStacks = 10;
   SDO->CommonStyle = GLU_FILL;
   SDO->radv=NULL;
   SDO->colv=NULL;
   SDO->stylev = NULL;
   
   SUMA_RETURN (SDO);
}

void SUMA_free_SphereDO (SUMA_SphereDO * SDO)
{
   static char FuncName[]={"SUMA_free_SphereDO"};

   SUMA_ENTRY;
   
   if (!SDO) SUMA_RETURNe;
   
   if (SDO->Parent_idcode_str) SUMA_free(SDO->Parent_idcode_str);
   if (SDO->NodeID) SUMA_free(SDO->NodeID); 
   if (SDO->cxyz) SUMA_free(SDO->cxyz);
   if (SDO->idcode_str) SUMA_free(SDO->idcode_str);
   if (SDO->Label) SUMA_free(SDO->Label);
   if (SDO->sphobj) gluDeleteQuadric(SDO->sphobj);
   if (SDO->radv) SUMA_free(SDO->radv);
   if (SDO->colv) SUMA_free(SDO->colv);
   if (SDO->stylev) SUMA_free(SDO->stylev);

   if (SDO) SUMA_free(SDO);
   
   SUMA_RETURNe;

}

SUMA_TractDO * SUMA_Alloc_TractDO (  char *Label, 
                                     char *Parent_idcode_str)
{
   static char FuncName[]={"SUMA_Alloc_TractDO"};
   SUMA_TractDO* TDO;
   char *hs = NULL;
   
   SUMA_ENTRY;

   TDO = (SUMA_TractDO*)SUMA_calloc(1,sizeof (SUMA_TractDO));
   if (TDO == NULL) {
      fprintf(stderr,"SUMA_Alloc_TractDO Error: Failed to allocate TDO\n");
      SUMA_RETURN (NULL);
   }
   TDO->do_type = TRACT_type;
   
   
   if (!Parent_idcode_str) {
      TDO->Parent_idcode_str = NULL;
   } else {
      TDO->Parent_idcode_str = SUMA_copy_string(Parent_idcode_str);
   } 
   
   /* create a string to hash an idcode */
   if (Label) hs = SUMA_copy_string(Label);
   else hs = SUMA_copy_string("NULL_");
   if (Parent_idcode_str) 
      hs = SUMA_append_replace_string(hs,Parent_idcode_str,"_",1);
   else hs = SUMA_append_replace_string(hs,"NULL","",1);
   TDO->idcode_str = UNIQ_hashcode(hs);
   SUMA_free(hs); hs = NULL;

   
   if (Label) {
      TDO->Label = (char *)SUMA_calloc (strlen(Label)+1, sizeof(char));
      TDO->Label = strcpy (TDO->Label, Label);
   } else {
      TDO->Label = NULL;
   }
   
   TDO->LineWidth = 1.0;
   TDO->LineCol[0] = 1.0;
   TDO->colv = NULL;
   TDO->thickv = NULL;
   
   TDO->Stipple = SUMA_SOLID_LINE;
   
   SUMA_RETURN (TDO);
}

void SUMA_free_TractDO (SUMA_TractDO * TDO)
{
   static char FuncName[]={"SUMA_free_TractDO"};

   SUMA_ENTRY;
   
   if (!TDO) SUMA_RETURNe;
   
   if (TDO->Parent_idcode_str) SUMA_free(TDO->Parent_idcode_str);
   if (TDO->Label) SUMA_free(TDO->Label);
   if (TDO->idcode_str) SUMA_free(TDO->idcode_str);
   if (TDO->net) Free_Network(TDO->net);
   
   if (TDO) SUMA_free(TDO);
   
   SUMA_RETURNe;

}

SUMA_GraphLinkDO * SUMA_Alloc_GraphLinkDO (  char *variant, 
                                             SUMA_DSET *ParentGraph)
{
   static char FuncName[]={"SUMA_Alloc_GraphLinkDO"};
   SUMA_GraphLinkDO* GLDO=NULL;
   char *hs = NULL, *pgi, *pgl;
   
   SUMA_ENTRY;

   if (!ParentGraph || !(pgi = SDSET_ID(ParentGraph))) {
      SUMA_S_Err("Get your parents");
      SUMA_RETURN (NULL);
   }
   if (!variant) { variant = "default"; }

   GLDO = (SUMA_GraphLinkDO*)SUMA_calloc(1,sizeof (SUMA_GraphLinkDO));
   if (GLDO == NULL) {
      SUMA_S_Err("Failed to allocate GLDO\n");
      SUMA_RETURN (NULL);
   }
   GLDO->do_type = GRAPH_LINK_type;
   GLDO->variant = SUMA_copy_string(variant);
   GLDO->Parent_idcode_str = SUMA_copy_string(pgi);
   
   /* create a string to hash an idcode */
   pgl = SDSET_LABEL(ParentGraph);
   hs = SUMA_append_replace_string(variant,
                  pgl?pgl:pgi,"_DOlink_",0);

   if (strcmp(variant,"TheShadow")) {
      GLDO->idcode_str = UNIQ_hashcode(hs);
   } else {
      GLDO->idcode_str = SUMA_copy_string(SDSET_ID(ParentGraph));
   }
   GLDO->Label = hs; hs = NULL;

   
   SUMA_RETURN (GLDO);
}

void SUMA_free_GraphLinkDO (SUMA_GraphLinkDO * GLDO)
{
   static char FuncName[]={"SUMA_free_GraphLinkDO"};

   SUMA_ENTRY;
   
   if (!GLDO) SUMA_RETURNe;
   
   if (GLDO->Parent_idcode_str) SUMA_free(GLDO->Parent_idcode_str);
   if (GLDO->Label) SUMA_free(GLDO->Label);
   if (GLDO->idcode_str) SUMA_free(GLDO->idcode_str);
   if (GLDO->variant) SUMA_free(GLDO->variant);
   
   if (GLDO) SUMA_free(GLDO);
   
   SUMA_RETURNe;
}


SUMA_Boolean SUMA_CreatePlaneQuads(SUMA_PlaneDO *SDO)
{
   static char FuncName[]={"SUMA_CreatePlaneQuads"};
   int i, j, N_n3, i3, n, k;
   GLfloat *boxdimv, xlim[2], ylim[2], zlim[2], 
            xtmp[4], ytmp[4], ztmp[4], eqn[3], *eq, a;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SDO->boxdimv) boxdimv = SDO->CommonBoxDims;
   else boxdimv = NULL;
   
   if (SDO->NodeList) SUMA_free(SDO->NodeList); SDO->NodeList=NULL;
   if (SDO->FaceSetList) SUMA_free(SDO->FaceSetList); SDO->FaceSetList=NULL;    
   if (SDO->nodecol) SUMA_free(SDO->nodecol); SDO->nodecol=NULL;    
   if (SDO->NodeNormList) SUMA_free(SDO->NodeNormList); SDO->NodeNormList=NULL;    
  
   SDO->N_Node = 4*SDO->N_n;
   SDO->NodeList = (GLfloat *)SUMA_calloc(SDO->N_Node*3, sizeof(GLfloat));
   SDO->NodeNormList = (GLfloat *)SUMA_calloc(SDO->N_Node*3, sizeof(GLfloat));
   SDO->N_FaceSet = SDO->N_n;
   SDO->FaceSetList = (GLint *)SUMA_calloc(4*SDO->N_FaceSet, sizeof(GLint));
   SDO->nodecol = (GLfloat *)SUMA_calloc(SDO->N_Node*4, sizeof(GLfloat));
   if (!SDO->NodeList || !SDO->NodeNormList || !SDO->FaceSetList || !SDO->nodecol) {
      SUMA_S_Crit("Failed to allocate");
      SUMA_RETURN(NOPE);
   }
   
   for (i=0; i<SDO->N_n;++i) {
      i3 = 3*i; n = 4*i; 
      if (SDO->boxdimv) boxdimv = &(SDO->boxdimv[i3]);
      /* calcuate limits of box limiting plane */
      xlim[0] = SDO->cxyz[0] - boxdimv[0]; 
      xlim[1] = SDO->cxyz[0] + boxdimv[0];
      ylim[0] = SDO->cxyz[1] - boxdimv[1]; 
      ylim[1] = SDO->cxyz[1] + boxdimv[1];
      zlim[0] = SDO->cxyz[2] - boxdimv[2]; 
      zlim[1] = SDO->cxyz[2] + boxdimv[2];
      
      SUMA_LHv("Box dims:\n[%3.2f %3.2f]\n[%3.2f %3.2f]\n[%3.2f %3.2f]\n", 
               xlim[0], xlim[1], ylim[0], ylim[1], zlim[0], zlim[1]);
               
	   /*using the limits, find the corrsponding 4 corners*/
      eq = &(SDO->pleq[4*i]);
      /* recalculate the plane's d parameter to go though center */
      eq[3] = -(eq[0]*SDO->cxyz[0]+eq[1]*SDO->cxyz[1]+eq[2]*SDO->cxyz[2]);
      if (eq[2] != 0.0f) {
         ztmp[0] = (-eq[3] -eq[0]*xlim[0] - eq[1]*ylim[0]) / eq[2];      
         ztmp[1] = (-eq[3] -eq[0]*xlim[1] - eq[1]*ylim[0]) / eq[2];      
         ztmp[2] = (-eq[3] -eq[0]*xlim[1] - eq[1]*ylim[1]) / eq[2];      
         ztmp[3] = (-eq[3] -eq[0]*xlim[0] - eq[1]*ylim[1]) / eq[2];      
         SDO->NodeList[3*(n  )  ] = xlim[0]; SDO->NodeList[3*(n  )+1] = ylim[0]; SDO->NodeList[3*(n  )+2] = ztmp[0]; /* Point 0 */
         SDO->NodeList[3*(n+1)  ] = xlim[1]; SDO->NodeList[3*(n+1)+1] = ylim[0]; SDO->NodeList[3*(n+1)+2] = ztmp[1]; /* Point 1 */
         SDO->NodeList[3*(n+2)  ] = xlim[1]; SDO->NodeList[3*(n+2)+1] = ylim[1]; SDO->NodeList[3*(n+2)+2] = ztmp[2]; /* Point 2 */
         SDO->NodeList[3*(n+3)  ] = xlim[0]; SDO->NodeList[3*(n+3)+1] = ylim[1]; SDO->NodeList[3*(n+3)+2] = ztmp[3]; /* Point 3 */
      } else if (eq[1] != 0.0f) {
         ytmp[0] = (-eq[3] -eq[0]*xlim[0] - eq[2]*zlim[0]) / eq[1];      
         ytmp[1] = (-eq[3] -eq[0]*xlim[1] - eq[2]*zlim[0]) / eq[1];      
         ytmp[2] = (-eq[3] -eq[0]*xlim[1] - eq[2]*zlim[1]) / eq[1];      
         ytmp[3] = (-eq[3] -eq[0]*xlim[0] - eq[2]*zlim[1]) / eq[1];
         SDO->NodeList[3*(n  )  ] = xlim[0]; SDO->NodeList[3*(n  )+1] = ytmp[0]; SDO->NodeList[3*(n  )+2] = zlim[0]; /* Point 0 */
         SDO->NodeList[3*(n+1)  ] = xlim[1]; SDO->NodeList[3*(n+1)+1] = ytmp[1]; SDO->NodeList[3*(n+1)+2] = zlim[0]; /* Point 1 */
         SDO->NodeList[3*(n+2)  ] = xlim[1]; SDO->NodeList[3*(n+2)+1] = ytmp[2]; SDO->NodeList[3*(n+2)+2] = zlim[1]; /* Point 2 */
         SDO->NodeList[3*(n+3)  ] = xlim[0]; SDO->NodeList[3*(n+3)+1] = ytmp[3]; SDO->NodeList[3*(n+3)+2] = zlim[1]; /* Point 3 */
      } else if (eq[0] != 0.0f) {
         xtmp[0] = (-eq[3] -eq[1]*ylim[0] - eq[2]*zlim[0]) / eq[0];      
         xtmp[1] = (-eq[3] -eq[1]*ylim[1] - eq[2]*zlim[0]) / eq[0];      
         xtmp[2] = (-eq[3] -eq[1]*ylim[1] - eq[2]*zlim[1]) / eq[0];      
         xtmp[3] = (-eq[3] -eq[1]*ylim[0] - eq[2]*zlim[1]) / eq[0];
         SDO->NodeList[3*(n  )  ] = xtmp[0]; SDO->NodeList[3*(n  )+1] = ylim[0]; SDO->NodeList[3*(n  )+2] = zlim[0]; /* Point 0 */
         SDO->NodeList[3*(n+1)  ] = xtmp[1]; SDO->NodeList[3*(n+1)+1] = ylim[1]; SDO->NodeList[3*(n+1)+2] = zlim[0]; /* Point 1 */
         SDO->NodeList[3*(n+2)  ] = xtmp[2]; SDO->NodeList[3*(n+2)+1] = ylim[1]; SDO->NodeList[3*(n+2)+2] = zlim[1]; /* Point 2 */
         SDO->NodeList[3*(n+3)  ] = xtmp[3]; SDO->NodeList[3*(n+3)+1] = ylim[0]; SDO->NodeList[3*(n+3)+2] = zlim[1]; /* Point 3 */
      } else {
         SUMA_S_Err("All zero plane?");
      }
      
      SUMA_LHv("4 points:\n[%3.2f %3.2f %3.2f]\n[%3.2f %3.2f %3.2f]\n[%3.2f %3.2f %3.2f]\n[%3.2f %3.2f %3.2f]\n", 
               SDO->NodeList[3*(n  )  ], SDO->NodeList[3*(n  )+1], SDO->NodeList[3*(n  )+2],
               SDO->NodeList[3*(n+1)  ], SDO->NodeList[3*(n+1)+1], SDO->NodeList[3*(n+1)+2],
               SDO->NodeList[3*(n+2)  ], SDO->NodeList[3*(n+2)+1], SDO->NodeList[3*(n+2)+2],
               SDO->NodeList[3*(n+3)  ], SDO->NodeList[3*(n+3)+1], SDO->NodeList[3*(n+3)+2]
               );
      
      /* Normals at each node */
      SUMA_NORM_VEC(eq, 3, a);
      if (a) {
         eqn[0] = eq[0] / a; eqn[1] = eq[1] / a; eqn[2] = eq[2] / a; 
      }else {
         eqn[0] = eqn[1] = eqn[2] = 0.0;
      }
      
      SDO->NodeNormList[3*(n  )  ] = eqn[0]; SDO->NodeNormList[3*(n  )+1] = eqn[1]; SDO->NodeNormList[3*(n  )+2] = eqn[2];
      SDO->NodeNormList[3*(n+1)  ] = eqn[0]; SDO->NodeNormList[3*(n+1)+1] = eqn[1]; SDO->NodeNormList[3*(n+1)+2] = eqn[2];
      SDO->NodeNormList[3*(n+2)  ] = eqn[0]; SDO->NodeNormList[3*(n+2)+1] = eqn[1]; SDO->NodeNormList[3*(n+2)+2] = eqn[2];
      SDO->NodeNormList[3*(n+3)  ] = eqn[0]; 
      SDO->NodeNormList[3*(n+3)+1] = eqn[1]; 
      SDO->NodeNormList[3*(n+3)+2] = eqn[2];
          
      /* Each quad representing a plane would be formed by nodes 
         n, n+1, n+2 and n+3 */
      SDO->FaceSetList[4*i  ] = i  ; 
      SDO->FaceSetList[4*i+1] = i+1; 
      SDO->FaceSetList[4*i+2] = i+2; 
      SDO->FaceSetList[4*i+3] = i+3; 
      
      /* form the stupid color vector */
      if (!SDO->colv) {
         for (k=0; k<4; ++k) {
            SDO->nodecol[4*(n+k)  ] = SDO->CommonCol[0]; 
            SDO->nodecol[4*(n+k)+1] = SDO->CommonCol[1];  
            SDO->nodecol[4*(n+k)+2] = SDO->CommonCol[2]; 
            SDO->nodecol[4*(n+k)+3] = SDO->CommonCol[3]; 
         }
      } else {
         for (k=0; k<4; ++k) {
            SDO->nodecol[4*(n+k)  ] = SDO->colv[4*i  ]; 
            SDO->nodecol[4*(n+k)+1] = SDO->colv[4*i+1];  
            SDO->nodecol[4*(n+k)+2] = SDO->colv[4*i+2]; 
            SDO->nodecol[4*(n+k)+3] = SDO->colv[4*i+3]; 
         }
      }
   }
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_DrawPlaneDO (SUMA_PlaneDO *SDO, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawPlaneDO"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0}, comcol[4];
   int i, N_n3, i3, rendmet;
   GLfloat boxdimv[3] = {3.0, 3.0, 3.0};
   float origwidth=0.0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SDO) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   if (SDO->PolyMode == SRM_Hide || sv->PolyMode == SRM_Hide) { 
      SUMA_RETURN(YUP); 
   }
   /* check on rendering mode */
   if (SDO->PolyMode != SRM_ViewerDefault) {
     /* not the default, do the deed */
     SUMA_SET_GL_RENDER_MODE(SDO->PolyMode); 
   }
   
   SUMA_CullOption(sv, "Hold");
   
   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   glLineWidth(SDO->LineWidth);
        
   if (!SDO->NodeList) {
      if (!SUMA_CreatePlaneQuads(SDO)) {
         SUMA_S_Err("Failed to create plane nodes");
         SUMA_RETURN(NOPE);
      }
   }
   
      
   /* This allows each node to follow the color specified when it was drawn 
      (in case you'll want to color corners differently someday) */ 
   glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE); 
   glEnable(GL_COLOR_MATERIAL);
         
   /*Now setup various pointers*/
   glEnableClientState (GL_COLOR_ARRAY);
   glEnableClientState (GL_VERTEX_ARRAY);
   glEnableClientState (GL_NORMAL_ARRAY);

   glColorPointer (4, GL_FLOAT, 0, SDO->nodecol);
   glVertexPointer (3, GL_FLOAT, 0, SDO->NodeList);
   glNormalPointer (GL_FLOAT, 0, SDO->NodeNormList);
   if (LocalHead) fprintf(stdout, "Ready to draw Elements %d\n", SDO->N_FaceSet); 
      
   rendmet = 0;
   switch (rendmet) {
      case 0:
            glDrawElements (GL_QUADS, (GLsizei)SDO->N_FaceSet*4, 
                            GL_UNSIGNED_INT, SDO->FaceSetList);
            break;
      case 1:
            glPointSize(4.0); /* keep outside of glBegin */
            /* it is inefficient to draw points using the 
               glar_FaceSetList because nodes are listed more 
               than once. You are better off creating an index vector 
               into glar_NodeList to place all the points, just once*/ 
            glDrawElements (GL_POINTS, (GLsizei)SDO->N_FaceSet*4, 
                            GL_UNSIGNED_INT, SDO->FaceSetList);
            break;
   } /* switch RENDER_METHOD */


   /*fprintf(stdout, "Disabling clients\n");*/
   glDisableClientState (GL_COLOR_ARRAY);   
   glDisableClientState (GL_VERTEX_ARRAY);
   glDisableClientState (GL_NORMAL_ARRAY);   
         
   glDisable(GL_COLOR_MATERIAL);
   
   glLineWidth(origwidth);

   /* reset viewer default rendering modes */
   if (SDO->PolyMode != SRM_ViewerDefault) {
     /* not the default, do the deed */
     SUMA_SET_GL_RENDER_MODE(SDO->PolyMode); 
   }  

   SUMA_CullOption(sv, "Restore");
  
   SUMA_RETURN (YUP);
   
}

SUMA_PlaneDO * SUMA_Alloc_PlaneDO (int N_n, char *Label, SUMA_DO_Types type)
{
   static char FuncName[]={"SUMA_Alloc_PlaneDO"};
   SUMA_PlaneDO* SDO;
   char *hs=NULL;
   
   SUMA_ENTRY;

   SDO = (SUMA_PlaneDO*)SUMA_calloc(1,sizeof (SUMA_PlaneDO));
   if (SDO == NULL) {
      fprintf(stderr,"SUMA_Alloc_PlaneDO Error: Failed to allocate SDO\n");
      SUMA_RETURN (NULL);
   }
   SDO->do_type = type;
   
   if (N_n > 0) {
      SDO->cxyz = (GLfloat *) SUMA_calloc (3*N_n, sizeof(GLfloat));
   
      if (!SDO->cxyz) {
         fprintf(stderr,"Error %s: Failed to allocate for SDO->cxyz\n", FuncName);
         if (SDO) SUMA_free (SDO);
         SUMA_RETURN (NULL);
      }
      SDO->N_n = N_n;
   } else {
      SDO->cxyz = NULL;
      SDO->N_n = 0;
   }
   
   /* create a string to hash an idcode */
   if (Label) hs = SUMA_copy_string(Label);
   else hs = SUMA_copy_string("NULL_");
   /*if (Parent_idcode_str) hs = SUMA_append_replace_string(hs,Parent_idcode_str,"_",1);
   else hs = SUMA_append_replace_string(hs,"NULL","",1);*/
   SDO->idcode_str = UNIQ_hashcode(hs);
   SUMA_free(hs); hs = NULL;
   
   if (Label) {
      SDO->Label = (char *)SUMA_calloc (strlen(Label)+1, sizeof(char));
      SDO->Label = strcpy (SDO->Label, Label);
   } else {
      SDO->Label = NULL;
   }
   
   /* create the ball object*/
   SDO->pleq = (GLfloat*)SUMA_calloc(SDO->N_n*4, sizeof(GLfloat));
   if (!SDO->cxyz) {
         fprintf(stderr,"Error %s: Failed to allocate for SDO->pleq\n", FuncName);
         SUMA_free_PlaneDO (SDO);
         SUMA_RETURN (NULL);
   }
   
   /* setup some default values */
   SDO->LineWidth = 4.0;
   SDO->CommonCol[0] = 1.0; SDO->CommonCol[1] = 1.0; SDO->CommonCol[2] = 1.0; SDO->CommonCol[3] = 1.0; 
   SDO->CommonBoxDims[0] = SDO->CommonBoxDims[1] = SDO->CommonBoxDims[2] = 10.0;
   SDO->boxdimv=NULL;
   SDO->colv=NULL;
   SDO->NodeList = NULL;
   SDO->NodeNormList = NULL;
   SDO->nodecol = NULL;
   SDO->FaceSetList = NULL;
   SDO->N_Node = 0;
   SDO->N_FaceSet = 0;
   SDO->PolyMode = SRM_ViewerDefault;
   SUMA_RETURN (SDO);
}

void SUMA_free_PlaneDO (SUMA_PlaneDO * SDO)
{
   static char FuncName[]={"SUMA_free_PlaneDO"};

   SUMA_ENTRY;
   
   if (!SDO) SUMA_RETURNe;
   
   if (SDO->cxyz) SUMA_free(SDO->cxyz);
   if (SDO->idcode_str) SUMA_free(SDO->idcode_str);
   if (SDO->Label) SUMA_free(SDO->Label);
   if (SDO->pleq) SUMA_free(SDO->pleq);
   if (SDO->boxdimv) SUMA_free(SDO->boxdimv);
   if (SDO->colv) SUMA_free(SDO->colv);
   if (SDO->NodeList) SUMA_free(SDO->NodeList);
   if (SDO->NodeNormList) SUMA_free(SDO->NodeNormList);
   if (SDO->nodecol) SUMA_free(SDO->nodecol);
   if (SDO->FaceSetList) SUMA_free(SDO->FaceSetList);
   if (SDO) SUMA_free(SDO);
   
   SUMA_RETURNe;

}

SUMA_Boolean SUMA_DrawTractDO (SUMA_TractDO *TDO, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawTractDO"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int i, i3a, i3b, n, N_pts, knet=0;
   float origwidth=0.0, Un, U[4]={0.0, 0.0, 0.0, 1.0}, *pa=NULL, *pb=NULL;
   TAYLOR_TRACT *tt=NULL;
   TAYLOR_BUNDLE *tb=NULL;
   byte *mask=NULL;
   byte color_by_mid = 0; /* this one should be interactively set ... */
   GLboolean gl_sm=FALSE;
   SUMA_Boolean ans = YUP;
   SUMA_Boolean LocalHead = NOPE; 
   
   SUMA_ENTRY;
   
   if (!TDO || !sv) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (sv->DO_PickMode) {
      SUMA_S_Warn("Function not ready for picking mode, should be fixed");
      SUMA_RETURN(YUP);
   }
   

   if (!TDO->net) SUMA_RETURN(YUP);
   
   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   gl_sm = glIsEnabled(GL_LINE_SMOOTH);
   if (!TDO->thickv) glLineWidth(0.1); //TDO->LineWidth);  
   
   switch (TDO->Stipple) {
      case SUMA_DASHED_LINE:
         glEnable(GL_LINE_STIPPLE);
         glLineStipple (1, 0x00FF); /* dashed, see OpenGL Prog guide, page 55 */
         break;
      case SUMA_SOLID_LINE:
         if (0) { 
            glEnable (GL_LINE_SMOOTH); /* makes lines fat, can't go too low 
                                          in thickness, fughetaboutit */
            if (0) glDepthMask(FALSE); /* Disabling depth masking makes lines 
                           coplanar with polygons
                           render without stitching, bleeding, or Z fighting.
                           Problem is, that it need to be turned on for proper
                           rendering of remaing objects, and that brings the 
                           artifact back. */
            glHint (GL_LINE_SMOOTH_HINT, GL_NICEST); 
         } else {
            glDisable(GL_LINE_SMOOTH);
         }
         break;
      default:
         fprintf(stderr,"Error %s: Unrecognized Stipple option\n", FuncName);
         ans = NOPE; goto GETOUT;
   }
   
   if (TDO->thickv) {/* slow slow slow */
      SUMA_S_Err("Not ready for thickness business");
      #if 0
      SUMA_LH("Drawing xyz to xyz with thickness ");
      if (!TDO->colv) glMaterialfv(GL_FRONT, GL_EMISSION, TDO->LineCol);
      glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
         /* turn off ambient and diffuse components */
      glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
      i = 0;
      N_n3 = 3*SDO->N_n;
      while (i < N_n3) {
         if (TDO->thickv) glLineWidth(TDO->thickv[i/3]);   
         glBegin(GL_LINES);
         if (TDO->colv) 
            glMaterialfv(GL_FRONT, GL_EMISSION, &(TDO->colv[4*(i/3)]));
         glVertex3f(SDO->n0[i], SDO->n0[i+1], SDO->n0[i+2]);
         glVertex3f(SDO->n1[i], SDO->n1[i+1], SDO->n1[i+2]); 
         i += 3;
         glEnd();
      }
      #endif
   } else {
      SUMA_LH("Drawing xyz to xyz ");
      glBegin(GL_LINES);
      if (!TDO->colv) 
         glMaterialfv(GL_FRONT, GL_EMISSION, TDO->LineCol); 
               /*turn on emissivity  */
      glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
               /* turn off ambient and diffuse components */
      glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
      
      for (knet=0; knet<TDO->net->N_tbv; ++knet) {
         tb = TDO->net->tbv[knet]; 
         for (n=0; tb && n<tb->N_tracts; ++n) {
            tt = tb->tracts+n;
            N_pts = TRACT_NPTS(tt);
            if (N_pts>2 && color_by_mid) {
               /* set color based on mid point */
               pa = tt->pts+(tt->N_pts3/2);
               pb = pa - 3;
               SUMA_SEG_DELTA_COL(pa,pb,U);
               glMaterialfv(GL_FRONT, GL_EMISSION, U);
            }
            i = 1;
            while (i < N_pts) {
               i3a = 3*(i-1); i3b = 3*i;
               pa = tt->pts+i3a; pb = tt->pts+i3b;
               if (TDO->colv) {
                  glMaterialfv(GL_FRONT, GL_EMISSION, &(TDO->colv[4*(i/3)]));
               } else if (!color_by_mid) { 
                  SUMA_SEG_DELTA_COL(pa,pb,U);
                  glMaterialfv(GL_FRONT, GL_EMISSION, U);
               }   
               glVertex3f(pa[0], pa[1], pa[2]);
               glVertex3f(pb[0], pb[1], pb[2]);
               ++i;
            }
         }
      }
      glEnd();
   }
         
   switch (TDO->Stipple) {
      case SUMA_DASHED_LINE:
         glDisable(GL_LINE_STIPPLE);
         break;
      case SUMA_SOLID_LINE:
         glDisable(GL_LINE_SMOOTH);
         break;
   }
   

   GETOUT:
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity */
   glLineWidth(origwidth);
   if (gl_sm) glEnable(GL_LINE_SMOOTH); else glDisable(GL_LINE_SMOOTH);
   if (mask) SUMA_free(mask); mask=NULL;
   
   
   SUMA_RETURN(ans);
}

/*!
   Create DOs for a graph dset 
*/
SUMA_Boolean SUMA_CreateGraphDOs(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_CreateGraphDOs"};
   SUMA_GraphLinkDO *GLDO=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !SUMA_isGraphDset(dset) || !dset->Aux) {
      SUMA_S_Errv("NULL or non Graph input: %p %d %p\n",
         dset, SUMA_isGraphDset(dset), dset->Aux);
      SUMA_RETURN(NOPE);   
   }
   
   /* Need a place holder for the dataset as displayable object
      because you just don't want to copy the dataset's pointer
      into DOv given the the dset pointers are actively managed
      elsewhere. TheShadow will never get displayed, and its 
      idcode_str is the same as the dataset's. 
      I just might live long enough to regret this. 
      TheShadow is a code word recognized by SUMA_Alloc_GraphLinkDO
      and other functions. Don't mess with it lightly*/
   if (!(GLDO = SUMA_Alloc_GraphLinkDO("TheShadow", dset))) {
      SUMA_S_Err("Failed to create TheShadow");
   } else { /* Add it to DOv */
      if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)GLDO, 
                       GRAPH_LINK_type, SUMA_WORLD)) {
         SUMA_S_Err("Failed to Add TheShadow");
         SUMA_free_GraphLinkDO(GLDO); GLDO=NULL;   
      }     
   }
   
   /* Now create a bunch of displayable DO links */
   
   if (!(GLDO = SUMA_Alloc_GraphLinkDO("G3D", dset))) {
      SUMA_S_Err("Failed to create G3D");
   } else { /* Add it to DOv */
      if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)GLDO, 
                       GRAPH_LINK_type, SUMA_WORLD)) {
         SUMA_S_Err("Failed to Add G3D");
         SUMA_free_GraphLinkDO(GLDO); GLDO=NULL;   
      }     
   }
   
   if (!(GLDO = SUMA_Alloc_GraphLinkDO("GMATRIX", dset))) {
      SUMA_S_Err("Failed to create GMATRIX");
   } else { /* Add it to DOv */
      if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)GLDO, 
                       GRAPH_LINK_type, SUMA_WORLD)) {
         SUMA_S_Err("Failed to Add GMATRIX");
         SUMA_free_GraphLinkDO(GLDO); GLDO=NULL;   
      }      
   }
   
   #if 0 /* Just a place holder, nothing is done for this type of display yet */
   if (!(GLDO = SUMA_Alloc_GraphLinkDO("GRELIEF", dset))) {
      SUMA_S_Err("Failed to create GRELIEF");
   } else { /* Add it to DOv */
      if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)GLDO, 
                       GRAPH_LINK_type, SUMA_WORLD)) {
         SUMA_S_Err("Failed to Add GMATRIX");
         SUMA_free_GraphLinkDO(GLDO); GLDO=NULL;   
      } 
   }
   #endif
            
   SUMA_RETURN(YUP);
}

SUMA_GraphLinkDO *SUMA_find_Dset_GLDO(SUMA_DSET *dset, char *variant, 
                                      int *ifound)
{
   static char FuncName[]={"SUMA_find_Dset_GLDO"};
   SUMA_GraphLinkDO *GLDO=NULL;
   int ii;
   
   SUMA_ENTRY;
   
   if (ifound) *ifound = -1;
   
   for (ii=0; ii<SUMAg_N_DOv; ++ii) {
      if (SUMAg_DOv[ii].ObjectType == GRAPH_LINK_type) {
         GLDO = (SUMA_GraphLinkDO *)SUMAg_DOv[ii].OP;
         if (!strcmp(GLDO->variant, variant) &&
             !strcmp(GLDO->Parent_idcode_str, SDSET_ID(dset))) {
            if (ifound) *ifound = ii;
            SUMA_RETURN(GLDO);   
         }
      }
   }
   SUMA_RETURN(NULL);
}  

SUMA_DSET *SUMA_find_GLDO_Dset(SUMA_GraphLinkDO *GLDO)
{
   static char FuncName[]={"SUMA_find_GLDO_Dset"};
   SUMA_DSET *dset=NULL;
   
   SUMA_ENTRY;
   
   if (!GLDO) SUMA_RETURN(dset);
   
   dset = SUMA_FindDset_s(GLDO->Parent_idcode_str, SUMAg_CF->DsetList);
   
   SUMA_RETURN(dset);
}

int SUMA_Picked_DO_ID(SUMA_COLID_OFFSET_DATUM *codf)
{
   static char FuncName[]={"SUMA_Picked_DO_ID"};
   int ido=-1;
   void *PP=NULL;
   
   SUMA_ENTRY;
   
   if (!codf) SUMA_RETURN(-1);
   
   switch (codf->ref_do_type) {
      case SDSET_type:
         #if 0
         if (!(PP = SUMA_FindDset_s(codf->ref_idcode_str, 
                                     SUMAg_CF->DsetList))) {
            SUMA_S_Err("Could not find reference dset");
         }
         SUMA_find_Dset_GLDO((SUMA_DSET *)PP, codf->variant,&ido);
         #else
         SUMA_S_Warn("I should not be picking from DO that cannot\n"
                     "be displayed without a variant");
         #endif
         SUMA_RETURN(ido);
      case GRAPH_LINK_type:
         if ((ido = SUMA_whichDO(codf->ref_idcode_str, 
                                 SUMAg_DOv, SUMAg_N_DOv)) < 0) {
            SUMA_S_Err("Could not find reference GRAPH_LINK");
         }
         SUMA_RETURN(ido);
         break;
      case SO_type:
         if ((ido = SUMA_findSO_inDOv (codf->ref_idcode_str, 
                                       SUMAg_DOv, SUMAg_N_DOv)) < 0) {
            SUMA_S_Err("Could not find reference SO");
         }
         SUMA_RETURN(ido);
         break;
      default:
         SUMA_S_Errv("Not equiped for type %d (%s) yet\n", codf->ref_do_type,
                     SUMA_ObjectTypeCode2ObjectTypeName(codf->ref_do_type));
         SUMA_RETURN(ido);
         break;
   }
   return(ido);
}

void *SUMA_Picked_reference_object(SUMA_COLID_OFFSET_DATUM *cod, 
                               SUMA_DO_Types *do_type)
{
   static char FuncName[]={"SUMA_Picked_reference_object"};
   void *PP=NULL;
   SUMA_DO_Types dd=type_not_set;
   
   SUMA_ENTRY;
   
   if (do_type) *do_type = type_not_set;
   
   if (!cod) SUMA_RETURN(PP);
   
   if (cod->ref_do_type == SDSET_type) {
      SUMA_S_Warn("Should not happen");
      if (!(PP = SUMA_FindDset_s(cod->ref_idcode_str, 
                                     SUMAg_CF->DsetList))) {
         SUMA_S_Err("Could not find reference dset");
      }
      if (do_type) *do_type = SDSET_type;
   } else if (cod->ref_do_type == GRAPH_LINK_type) {
      PP = (void *)SUMA_whichADOg(cod->ref_idcode_str);
      if (do_type) *do_type = GRAPH_LINK_type;
   } else if (cod->ref_do_type == SO_type) {
      if (!(PP = SUMA_findSOp_inDOv (cod->ref_idcode_str, 
                                              SUMAg_DOv, SUMAg_N_DOv))) {
         SUMA_S_Err("Could not find reference SO");
      }
      if (do_type) *do_type = SO_type;
   } else {
      SUMA_S_Warnv("Ref do_type %d (%s) is unexpected. "
                   "Trying to guess...\n",
            cod->ref_do_type, 
            SUMA_ObjectTypeCode2ObjectTypeName(cod->ref_do_type));
      if ((PP = SUMA_FindDset_s(cod->ref_idcode_str, 
                                        SUMAg_CF->DsetList))) {
         if (do_type) *do_type = SDSET_type;
      } else if ((PP = SUMA_findSOp_inDOv (cod->ref_idcode_str, 
                                                 SUMAg_DOv, SUMAg_N_DOv))){
         if (do_type) *do_type = SO_type;
      }                                       
   }
   
   /* ref_do_type recognized but object not found, see if you can guess, just 
      for debugging */
   if (!PP) {
      if (SUMA_find_any_object(cod->ref_idcode_str, &dd)) {
         SUMA_S_Errv("Found reference object but its type is %s, not %s\n",
                     SUMA_ObjectTypeCode2ObjectTypeName(dd), 
                     SUMA_ObjectTypeCode2ObjectTypeName(cod->ref_do_type));
      }
   }
   
   SUMA_RETURN(PP);
}

SUMA_Boolean SUMA_RegisterGraphDOs(SUMA_DSET *dset, SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_RegisterGraphDOs"};
   SUMA_GraphLinkDO *GLDO=NULL;
   int ifound=-1;
   
   SUMA_ENTRY;
   
   
   if (!dset || !SUMA_isGraphDset(dset) || !dset->Aux) {
      SUMA_S_Errv("NULL or non Graph input: %p %d %p\n",
         dset, SUMA_isGraphDset(dset), dset->Aux);
      SUMA_RETURN(NOPE);  
   }
   if ((GLDO =  SUMA_find_Dset_GLDO(dset, "TheShadow", &ifound))) {
      if (!SUMA_RegisterDO(ifound, sv)) {
         SUMA_S_Err("Failed to register TheShadow.\n");
      }
   }
   if ((GLDO =  SUMA_find_Dset_GLDO(dset, "G3D", &ifound))) {
      if (!SUMA_RegisterDO(ifound, sv)) {
         SUMA_S_Err("Failed to register G3D.\n");
      }
   }
   if ((GLDO =  SUMA_find_Dset_GLDO(dset, "GMATRIX", &ifound))) {
      if (!SUMA_RegisterDO(ifound, sv)) {
         SUMA_S_Err("Failed to register GMATRIX.\n");
      }
   }
   if ((GLDO =  SUMA_find_Dset_GLDO(dset, "GRELIEF", &ifound))) {
      if (!SUMA_RegisterDO(ifound, sv)) {
         SUMA_S_Err("Failed to register GRELEIF.\n");
      }
   }
   SUMA_RETURN(YUP);   
}

SUMA_Boolean SUMA_DrawGraphLinkDO (SUMA_GraphLinkDO *GLDO, 
                                   SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawGraphLinkDO"};
   SUMA_DSET *dset=NULL;
   SUMA_Boolean ans = NOPE;
   int ifound=0;
   SUMA_Boolean LocalHead = NOPE; 
   
   SUMA_ENTRY;
   
   if (!GLDO || !sv) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   dset = SUMA_find_GLDO_Dset(GLDO);
   
   if (!dset) { /* remove soft link and return politely */
    SUMA_LHv("Removing soft link %s for its object is no longer with us\n", 
             GLDO->Label);
    if (!SUMA_UnRegisterDO_idcode(GLDO->idcode_str, sv)) {
      SUMA_S_Err("Una furtiva lagrima");
    }
    SUMA_S_Warn("Not sure how to handle this yet. Deletion without making sure\n"
                "widgets are killed is asking for bad bad trouble.\n"
                "Deal with this when this comes up.\n");
    #if 0
    if (!SUMA_RemoveDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)GLDO,1)) {
      SUMA_S_Err("Alors la, bravo!");
      SUMA_RETURN(NOPE);
    }
    #endif
    SUMA_RETURN(YUP);
   }
   
   ans = SUMA_DrawGraphDO (GLDO, sv, NULL);    
   
   SUMA_RETURN(ans);
}

/*! What's the group of a DO? Return ANY if nothing is available*/
char *SUMA_iDO_group(int i)
{
   if (i >0 && i <SUMAg_N_DOv) {
      return(SUMA_DO_group(&(SUMAg_DOv[i])));
   }
   return(NULL);
}
char *SUMA_DO_group(SUMA_DO *DO)
{
   static char FuncName[]={"SUMA_DO_group"};
   static char gret[64]={"ANY"};
   SUMA_SurfaceObject *SO=NULL;
   
   SUMA_ENTRY;
   if (!DO) SUMA_RETURN(NULL);
   switch (DO->ObjectType) {
      case SO_type: 
         SO = (SUMA_SurfaceObject *)(DO->OP);
         SUMA_RETURN(SO->Group);
         break;
      default: /* any group for now */
         sprintf(gret,"ANY");
         SUMA_RETURN(gret);
         break;
   }
   SUMA_RETURN(gret);
}


/*! What's the state of a DO? 
    Return ANY_ANATOMICAL for a special case of those states
    that can be displayed in an AnatCorrect environment         */
char *SUMA_iDO_state(int i)
{
   if (i >0 && i <SUMAg_N_DOv) {
      return(SUMA_DO_state(&(SUMAg_DOv[i])));
   }
   return(NULL);
}

char *SUMA_DO_state(SUMA_DO *DO)
{
   static char FuncName[]={"SUMA_DO_state"};   
   SUMA_SurfaceObject *SO;
   SUMA_GraphLinkDO *GLDO;
   static char gret[256]={"ANY_ANATOMICAL"};
   
   SUMA_ENTRY;
   if (!DO) SUMA_RETURN(NULL);
   switch (DO->ObjectType) {
      case SO_type: 
         SO = (SUMA_SurfaceObject *)(DO->OP);
         SUMA_RETURN(SO->State);
         break;
      case GRAPH_LINK_type:
         GLDO = (SUMA_GraphLinkDO *)(DO->OP);         
         if (SUMA_isGLDO_AnatCorrect(GLDO)) {
            sprintf(gret,"ANY_ANATOMICAL");
            SUMA_RETURN(gret);
         } else {
            snprintf(gret, 255*sizeof(char),"%s_%s",
                        GLDO->variant, GLDO->Label);
            SUMA_RETURN(gret);
         }
         break;
      case TRACT_type:
         sprintf(gret,"ANY_ANATOMICAL");
         SUMA_RETURN(gret);
         break;
      default: /* any group for now */
         sprintf(gret,"ANY");
         SUMA_RETURN(gret);
         break;
   }
   sprintf(gret,"ANY");
   SUMA_RETURN(gret);
}

/*! Is a displayable object anatomically correct? 
   \sa SUMA_isDO_AnatCorrect
*/
int  SUMA_is_iDO_AnatCorrect(int dov_id)
{
   static char FuncName[]={"SUMA_is_iDO_AnatCorrect"};   
   SUMA_ENTRY;
   if (dov_id < 0 || dov_id>=SUMAg_N_DOv) {
      SUMA_S_Errv("Bad do_id %d, not in [%d %d[ returning 0\n",
                  dov_id, 0, SUMAg_N_DOv);
      SUMA_RETURN(0);
   }
   SUMA_RETURN(SUMA_isDO_AnatCorrect(&(SUMAg_DOv[dov_id])));
}

/*! Is a displayable object anatomically correct? 
   \sa SUMA_is_iDO_AnatCorrect
*/
int SUMA_isDO_AnatCorrect(SUMA_DO *DO)
{
   static char FuncName[]={"SUMA_isDO_AnatCorrect"};   
   SUMA_SurfaceObject *SO;
   SUMA_GraphLinkDO *GLDO;
   SUMA_ENTRY;
   
   if (!DO) SUMA_RETURN(0);
   switch (DO->ObjectType) {
      case SO_type: 
         SO = (SUMA_SurfaceObject *)(DO->OP);
         SUMA_RETURN(SO->AnatCorrect);
         break;
      case GRAPH_LINK_type:
         GLDO = (SUMA_GraphLinkDO*)(DO->OP);
         SUMA_RETURN(SUMA_isGLDO_AnatCorrect(GLDO));
         break;
      case TRACT_type:
         SUMA_RETURN(1);
         break;
      default: 
         SUMA_RETURN(0);
         break;
   }
   SUMA_RETURN(0);
}   


SUMA_Boolean SUMA_isGLDO_AnatCorrect(SUMA_GraphLinkDO *GLDO)
{
   if (!GLDO) return(NOPE);
   
   if (!GLDO->variant) return(NOPE);
        if (!strcmp(GLDO->variant,"default")) return(NOPE);
   else if (!strcmp(GLDO->variant,"GMATRIX")) return(NOPE);
   else if (!strcmp(GLDO->variant,"G3D")) {
      /* for now, just return YES, you'll stil need a 
         flag in the header to say if coordinate
         of nodes are anatomically correct or not */
      return(YUP);
   } else if (!strcmp(GLDO->variant,"GRELIEF")) return(NOPE);
   return(NOPE);
}

SUMA_Boolean SUMA_DrawGraphDO (SUMA_GraphLinkDO *gldo, SUMA_SurfaceViewer *sv, 
                               char *this_variant)
{
   static char FuncName[]={"SUMA_DrawGraphDO"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int i, i3a, i3b, n, N_pts, mode = 0;
   float origwidth=0.0, Un, U[4]={0.0, 0.0, 0.0, 1.0}, *pa=NULL, *pb=NULL;
   byte color_by_mid = 0; /* this one should be interactively set ... */
   GLboolean gl_sm=FALSE;
   SUMA_Boolean ans = YUP;
   SUMA_DSET *dset=NULL;
   char *variant=NULL;
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)gldo;
   SUMA_Boolean LocalHead = NOPE; 
   
   SUMA_ENTRY;
   
   if (!(dset=SUMA_find_GLDO_Dset(gldo)) || !sv || !SUMA_isGraphDset(dset)) {
      fprintf(stderr,"Error %s: NULL or bad pointers.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   if (!this_variant) {
      variant = SUMA_ADO_variant(ado);
   } else {
      variant = this_variant;
   }
   if (!strcmp(variant,"default") || variant[0]=='\0') {
      variant = "GMATRIX";
   }
   
   if (this_variant && strcmp(variant,SUMA_ADO_variant(ado))){/* desired variant
                                                 not what the doc sent us */
      gldo = SUMA_find_Dset_GLDO(dset, variant, NULL);
      if (!gldo) {
         SUMA_S_Errv("Could not find variant %s of dset %s\n",
                     variant, SDSET_LABEL(dset));
         SUMA_RETURN(NOPE);
      }
      ado = (SUMA_ALL_DO *)gldo;
   } 
   
   if (!strcmp(variant,"GMATRIX")) {   
      SUMA_DrawGraphDO_GMATRIX (gldo, sv);
   } else if (!strcmp(variant,"G3D")) {
      SUMA_DrawGraphDO_G3D (gldo, sv);
   } else if (!strcmp(variant,"GRELIEF")) {
      SUMA_DrawGraphDO_GRELIEF (gldo, sv);
   } else if (!strcmp(variant,"TheShadow")) {
      SUMA_LH("Never draw TheShadow!!!");
      SUMA_RETURN(YUP);
   } else {
      SUMA_S_Errv("Don't know about variant %s\n", variant);
      SUMA_RETURN(NOPE);
   }
      
   SUMA_RETURN(YUP);
}
SUMA_Boolean SUMA_BordFrac_to_GB(int BF, int *G, int *B) 
{
   static char FuncName[]={"SUMA_BordFrac_to_GB"};
   
   SUMA_ENTRY;
   
   if (!G || !B) SUMA_RETURN(NOPE);
   
   switch (BF) {
      default:
      case SW_SurfCont_DsetGmatBord0:
         *G=1; *B=0;
         break;
      case SW_SurfCont_DsetGmatBord5:
         *G=5; *B=1;
         break;
      case SW_SurfCont_DsetGmatBord10:
         *G=10; *B=1;
         break;
      case SW_SurfCont_DsetGmatBord20:
         *G=20; *B=1;
         break;
      case SW_SurfCont_DsetGmatBord40:
         *G=40; *B=1;
         break;
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_GDSET_clear_matrix_nido(SUMA_DSET *dset, int clear_SO)
{
   static char FuncName[]={"SUMA_GDSET_clear_matrix_nido"}; 
   SUMA_GRAPH_SAUX *GSaux = NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !SUMA_isGraphDset(dset) || !(GSaux=SDSET_GSAUX(dset))) {
      fprintf(stderr,"Error %s: NULL or bad pointers.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (GSaux->nido) SUMA_free_NIDO(GSaux->nido);
   GSaux->nido = NULL;
   
   if (clear_SO) {
      if (GSaux->FrameSO) SUMA_Free_Surface_Object(GSaux->FrameSO);
      GSaux->FrameSO = NULL;
   }
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_GDSET_refresh_matrix_nido(SUMA_DSET *dset, int also_SO)
{
   static char FuncName[]={"SUMA_GDSET_refresh_matrix_nido"}; 
   SUMA_GRAPH_SAUX *GSaux = NULL;
   
   SUMA_ENTRY;
   
   if (!SUMA_GDSET_clear_matrix_nido(dset, also_SO)) {
      SUMA_S_Err("Unclear!");
      SUMA_RETURN (NOPE);
   }
   
   /* recreate matrix_nido */
   if (!SUMA_GDSET_matrix_nido(dset)) {
      SUMA_S_Err("Failed to recreate matrix_nido");
      SUMA_RETURN(NOPE);
   }
   
   if (also_SO) {
      if (!(GSaux=SDSET_GSAUX(dset)) || GSaux->FrameSO) {
         SUMA_S_Err("This should not be, you just cleared it!");
      } else {
         GSaux->FrameSO = 
                     SUMA_Surface_Of_NIDO_Matrix(GSaux->nido);
      }
   }
   
   SUMA_RETURN(YUP);
}

SUMA_NIDO * SUMA_GDSET_matrix_nido(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GDSET_matrix_nido"};
   int G[3], B[3], N[3], M[3], g, b;
   SUMA_GRAPH_SAUX *GSaux = NULL;
   SUMA_OVERLAYS *curcol = NULL;
   char Label[64]={""};
   float MaxSize, fac=1.0;
   double Aff[4][4], V[12];
   NI_element *nini=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !SUMA_isGraphDset(dset) || !(GSaux=SDSET_GSAUX(dset))) {
      fprintf(stderr,"Error %s: NULL or bad pointers.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   if (GSaux->nido) SUMA_RETURN(GSaux->nido);
   
   if (!(curcol = SUMA_ADO_CurColPlane((SUMA_ALL_DO *)dset))) {
      SUMA_S_Err("Could not find current col plane!");
      SUMA_RETURN (NULL);
   }
   
   SUMA_LHv("Creating new NIDO for %s matrix\n", SDSET_LABEL(dset));
   if (!(GSaux->nido = SUMA_BlankNIDO (NULL, Label, NULL, "mobile", NULL))) {
         SUMA_S_Errv("Failed to allocate for GSaux->nido %s (%d)\n",
                     Label, SDSET_VECLEN(dset));
         SUMA_RETURN(NULL);
   }
   NI_set_attribute(GSaux->nido->ngr,"parent_dset_id", SDSET_ID(dset));

   /* fill in all the parameters per the controller settings */
   SUMA_BordFrac_to_GB(curcol->BordFrac , &g, &b);
   G[0] = g; G[1] = g; G[2] = 1; /* Pixels per matrix val */
                     NI_SET_INTv(GSaux->nido->ngr, "PixPerVal", G, 3); 
   B[0] = b; B[1] = b;  B[2] = 0; /* Border width */
                     NI_SET_INTv(GSaux->nido->ngr, "BorWid", B, 3);
   /* add a slice image nel */
   nini = NI_new_data_element("Slice", 2); /* symbolic tiny size */
   NI_SET_INT(nini,"k",0); /* 0th slice, always */
   NI_add_column (nini, NI_BYTE, NULL); 
   
   /* number of values in matrix */
   N[0] = SDSET_MATRIX_SZ0(dset); N[1] = SDSET_MATRIX_SZ1(dset); N[2]=1; 
   NI_SET_INTv(GSaux->nido->ngr, "Nijk", N, 3);
      
   /* number of pixels in resultant image */
   M[0] = G[0]*N[0] + B[0]*(N[0]+1);
   M[1] = G[1]*N[1] + B[1]*(N[1]+1);
   M[3] = 1;
   NI_SET_INTv(GSaux->nido->ngr, "PixCount", M, 3);

   SUMA_LH("Coords matrix");
   /* Note that X coord is for the 2nd matrix dimension,
      and Y coord is for the 1st (i) dimension */
   if (M[0]<M[1]) fac = 1.0/M[1];
   else fac = 1.0/M[0];
   MaxSize = 240.0; /* size in mm of displayed matrix */
   AFF44_LOAD(Aff,
              0.0      ,  MaxSize*fac,  0.0      ,  0.0      ,
             -MaxSize*fac,  0.0      ,  0.0      ,  0.0      ,
              0.0      ,  0.0      ,  MaxSize*fac,  0.0      );
   AFF44_TO_V12(V, Aff);
   NI_SET_DOUBLEv(GSaux->nido->ngr, "ijk_to_dicom_real", V, 12);
   /* and put in the inverse for good measure */
   SUMA_INV_V12MATRIX(V);
   NI_SET_DOUBLEv(GSaux->nido->ngr, "dicom_real_to_ijk", V, 12);

   NI_add_to_group(GSaux->nido->ngr, nini);
   
   SUMA_RETURN(GSaux->nido);
}

SUMA_Boolean SUMA_DrawGraphDO_GMATRIX (SUMA_GraphLinkDO *gldo, 
                                       SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawGraphDO_GMATRIX"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   static float txcol[3] = {1, 1, 1};
   int iseg, N_seg, usedel=0, M4, ii, jj, iim, jjm, si, pof = 0, rid,
       *ui, *uj, cc, ee, eem, *GNI=NULL, iname=0;
   int M[3], G[3], B[3], GB[3], N[3], is4, ii4, iipix, jjpix, iipixMax, jjpixMax;
   double Aff[4][4], I[3];
   DListElmt *el=NULL, *eln=NULL;
   NI_element *nini=NULL;
   GLfloat *colv=NULL, *texcoord=NULL;
   static GLuint texName;
   byte *bb=NULL;
   char Label[64]={""};
   SUMA_Boolean ans = YUP;
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)gldo;
   SUMA_DSET *dset=NULL;
   SUMA_GRAPH_SAUX *GSaux = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_COLORLIST_STRUCT *colstr=NULL;
   float XYZ[27];
   GLfloat sq_col[] = {1, 1, 1, 1.0};
   GLfloat per_col[] = {0.5, 0.5, 0.5, 1.0};
   GLboolean valid;
   GLfloat rpos[4];
   NI_element *nelxyz = NULL;
   char **names = NULL;
   SUMA_OVERLAYS *curcol = NULL;
   void *fontGL=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!gldo || !(dset=SUMA_find_GLDO_Dset(gldo)) || !sv 
         || !SUMA_isGraphDset(dset) || !(GSaux=SDSET_GSAUX(dset))) {
      fprintf(stderr,"Error %s: NULL or bad pointers.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
      
   if (sv->DO_PickMode) {
      SUMA_LH("In picking mode!");
   }
   

   #if USE_SER
   SUMA_RecordEnablingState(&(sv->SER)); /* Lazy, consider SUMA_GLStateTrack ,
                                         Also, why record it if it is already
                                         well preserved. */ 
   #endif
   
   /* Form empty NIDO*/
   snprintf(Label, 63*sizeof(char), "Image_%s_GMATRIX", SDSET_LABEL(dset));
   if (!GSaux->nido && ! (GSaux->nido = SUMA_GDSET_matrix_nido(dset))) {
      SUMA_S_Err("Failed to create nido");
      goto BUGOUT;
   } else {
      if (!(nini = SUMA_FindNgrNamedElement(GSaux->nido->ngr,"Slice"))) {
         SUMA_S_Err("Could not find Slice");
         goto BUGOUT;
      }
      NI_GET_INTv(GSaux->nido->ngr, "PixPerVal", G, 3, LocalHead);
      NI_GET_INTv(GSaux->nido->ngr, "BorWid", B, 3, LocalHead);
   }
   
   /* number of values in matrix */
   N[0] = SDSET_MATRIX_SZ0(dset); N[1] = SDSET_MATRIX_SZ1(dset); N[2] = 1;
   
   /* total num of pixels per value */
   GB[0] = G[0]+B[0];
   GB[1] = G[1]+B[1];
   GB[2] = G[2]+B[2];
   
   /* number of pixels in resultant image */
   M[0] = G[0]*N[0] + B[0]*(N[0]+1);
   M[1] = G[1]*N[1] + B[1]*(N[1]+1);
   M[2] = G[2]*N[2] + B[2]*(N[2]+1);
   NI_SET_INTv(GSaux->nido->ngr, "PixCount", M, 3);
   
   M4 = M[0]*M[1]*4;
   
   /* check on sufficient size in nini */
   if (nini->vec_len != M4) {
      NI_alter_veclen(nini, M4);
   }
   if (!(bb = (byte *)nini->vec[0])) {
      SUMA_S_Err("No bb to be had");
      goto BUGOUT;
   }
   
   SUMA_LHv("G=[%d %d], B=[%d %d], GB=[%d %d], M=[%d %d], M4=%d\n",
            G[0], G[1], B[0], B[1], GB[0], GB[1], M[0], M[1], M4);
   
   if (!(colstr = SUMA_GetColorListStruct (sv, SDSET_ID(dset)))) {
      SUMA_S_Errv("No col struct for %s ???\n", SDSET_LABEL(dset));
      goto BUGOUT;
   }
   
   /* Do we have a new remix ID? */
   NI_GET_INT(nini, "RemixID", rid);
   if (!NI_GOT || rid != colstr->RemixID) {
      SUMA_DrawDO_UL_Add(GSaux->DisplayUpdates, "nido_MapColors", 1);
      NI_SET_INT(nini, "RemixID", colstr->RemixID);
   }
   
   /* what kind of monstrosity is this ? */
   ui = uj = NULL;
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
      case MAT_TRI:
      case MAT_TRI_DIAG:
         SUMA_LH("Direct indexing between edge points and matrix row/columns");
         break;
      case MAT_SPARSE:
         if (!dset->inel) {
            SUMA_S_Err("Don't have inel, badly shaped dataset");
            goto BUGOUT;
         }
         if (  !(ui = SUMA_GetUniqueIndicesVec(dset,1)) || 
               !(uj = SUMA_GetUniqueIndicesVec(dset,2))    ) {
            SUMA_S_Err("Failed to get unique indices");
           goto BUGOUT;
         }
         break;
      default:
         SUMA_S_Err("Congrats");
         break;
   }
   
   if (dlist_size(GSaux->DisplayUpdates)) {/* GSaux->nido needs updating */
      el = dlist_head(GSaux->DisplayUpdates);
      do {
         usedel = 1; /* assume will consumate this element */
         if (!strcmp((char *)el->data,"nido_MapColors")) {
            /* blank out all values bb is ROW major, unlike the way 
               MAT_full is stored. i* vars are for 1st matrix dim, 
               j* vars are for the second*/
            memset(bb, 0, sizeof(byte)*M4);
            /* get the color vector */
            if (!(colv = colstr->glar_ColorList)) {
               SUMA_S_Errv("No colv for %s?\n", SDSET_LABEL(dset));
               goto BUGOUT;
            }
            N_seg = SDSET_VECLEN(dset);

            /* Fillup background grays */
            for (ii=0; ii<N[0]+1; ++ii) { /* 1st the rows */
               iipix = ii*GB[0]; iipixMax = iipix+B[0];
               while (iipix < iipixMax) {
                  jjpix = 0;
                  while (jjpix < M[1]) {
                     ii4 = (iipix*M[1]+jjpix)*4; 
                           /* Texture image is filled in row major, so
                              image in bb is transposed */
                     bb[ii4] = 128; ++ii4; 
                     bb[ii4] = 128; ++ii4; 
                     bb[ii4] = 128; ++ii4; 
                     bb[ii4] = 255; 
                     ++jjpix;
                  }
                  ++iipix;
               }
            }
            for (jj=0; jj<N[1]+1; ++jj) { /* now the cols */
               jjpix = jj*(GB[1]); jjpixMax = jjpix+B[1];
               while (jjpix < jjpixMax) {
                  iipix = 0;
                  while (iipix < M[0]) {
                     ii4 = (iipix*M[1]+jjpix)*4; 
                           /* Texture image is filled in row major, so
                              image in bb is transposed */
                     bb[ii4] = 128; ++ii4; 
                     bb[ii4] = 128; ++ii4; 
                     bb[ii4] = 128; ++ii4; 
                     bb[ii4] = 255; 
                     ++iipix;
                  }
                  ++jjpix;
               }
            }
            
            /* Fillup where you have segments */
            SUMA_LHv("Filling up image for %d cells\n", N_seg);
            for(iseg=0; iseg<N_seg; ++iseg) {
               if (!SUMA_GDSET_SegRowToPoints(dset, iseg, 
                                                &ii, 
                                                &jj,
                                                NULL)){
                  SUMA_S_Errv("Failed for edge %d\n", iseg);
               }
               si = SUMA_GDSET_EdgeRow_To_Index(dset, iseg);
               if (!ui) {
                  iim = ii; jjm = jj;
               } else {
                  iim = SUMA_ibinFind(ui, N[0], ii);
                  jjm = SUMA_ibinFind(uj, N[1], jj);
               }
               #if 0
               SUMA_LHv(
                  "Edge %d [%d %d] in %dx%d mat [%d %d], col [%d %d %d] (%d)\n",
                     si, ii, jj, N[0], N[1], iim, jjm, (byte)(255*colv[4*si]),
                           (byte)(255*colv[4*si+1]), (byte)(255*colv[4*si+2]),
                           GSaux->isColored[si]);
               #endif
               if (GSaux->isColored[si]) {
                  /* fill foreground */
                  iipix = iim*(GB[0])+B[0]; iipixMax = iipix+G[0];
                  while (iipix < iipixMax) {
                     jjpix = jjm*(GB[1])+B[1]; jjpixMax = jjpix+G[1];
                     while (jjpix < jjpixMax) {
                        ii4 = (iipix*M[1]+jjpix)*4; 
                           /* Texture image is filled in row major, so
                              image in bb is transposed */
                        is4 = 4*si;
                        bb[ii4] = (byte)(255*colv[is4++]); ++ii4; 
                        bb[ii4] = (byte)(255*colv[is4++]); ++ii4; 
                        bb[ii4] = (byte)(255*colv[is4++]); ++ii4; 
                        bb[ii4] = (byte)(255*colv[is4  ]); 
                        ++jjpix;
                     }
                     ++iipix;
                  }
               }
            }
            
            #if 0
            for (iipix=0; iipix<M[0]; ++iipix) {
               for (jjpix=0; jjpix<M[1]; ++jjpix) {
                     ii4 = (iipix*M[1]+jjpix)*4; 
                           /* Texture image is filled in row major, so
                              image in bb is transposed */
                  fprintf(stderr,"(%d %d %d %d)   ", 
                           bb[ii4], bb[ii4+1], bb[ii4+2], bb[ii4+3]);
               }
               fprintf(stderr,"\n");
            }
            #endif
         } else {
            usedel = 0;
            /* this may not need to be an error condition, you might just skip 
            this update if it is irrelevant ?*/
            SUMA_LHv("DisplayUpdates command %s not relevant here\n",
                        (char *)el->data);
         }
         
         if (el != dlist_tail(GSaux->DisplayUpdates)) eln = dlist_next(el);
         else eln = NULL;
         if (usedel) {
            /* delete used element */
            SUMA_DrawDO_UL_EmptyList(GSaux->DisplayUpdates, el);
            el = NULL;
         }
         el = eln;
      } while (el);
   }
   
   if (LocalHead) {
      /* Write the matrix to an image width is M[1], height is M[0] */
      if (!SUMA_PixelsToDisk(NULL, M[1], M[0], bb, 4, 1, "mat.ppm", 0, 1)) {
         SUMA_S_Err("Failed to save matrix to disk");
      } else {
         SUMA_LHv("Saved %dx%d matrix to %s\n", M[1], M[0], "mat.ppm");
      }
   }

   /* and draw the GSaux */   
   #if 0
   SUMA_ShowNel(GSaux->nido->ngr);
   #endif
   
   /* form the SO and draw it, if you must*/   
   if (!GSaux->FrameSO) GSaux->FrameSO = 
                     SUMA_Surface_Of_NIDO_Matrix(GSaux->nido);
   if (LocalHead) SUMA_SimpleDrawMesh(GSaux->FrameSO, NULL, NULL);

   /* See SUMA_DrawTextureNIDOnel() for more tests
      before drawing 2D textures */
   if ((pof = glIsEnabled(GL_POLYGON_OFFSET_FILL))) 
                     glDisable (GL_POLYGON_OFFSET_FILL);
   
   /* Now put in the texture */
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
   NI_GET_INT(nini,"texName",texName);
   if (!NI_GOT) {
      /* Need to generate texture */
      glGenTextures(1, &texName);
      /* Now store it */
      NI_SET_INT(nini,"texName",texName);
   }
   glBindTexture(GL_TEXTURE_2D, texName);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S, GL_REPEAT); 
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T, GL_REPEAT); 
   glTexParameteri(  GL_TEXTURE_2D,
                     GL_TEXTURE_MAG_FILTER, GL_NEAREST);
   glTexParameteri(  GL_TEXTURE_2D,
                     GL_TEXTURE_MIN_FILTER, GL_NEAREST);
         /* width is M[1], height is M[0] */
   glTexImage2D(  GL_TEXTURE_2D, 0, GL_RGBA, 
                  M[1], M[0], 0, GL_RGBA, 
                  GL_UNSIGNED_BYTE, bb);
   glEnable(GL_TEXTURE_2D);
   glTexEnvf(  GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, 
               GL_REPLACE);
         /* GL_DECAL, GL_REPLACE, GL_MODULATE, GL_BLEND */
   glBindTexture(GL_TEXTURE_2D, texName); 
   texcoord = GSaux->FrameSO->NodeList;
   SUMA_LHv("Texture as image, texName=%d, filename=%s\n"
             "coords:\n"
             "%.3f %.3f %.3f\n%.3f %.3f %.3f\n"
             "%.3f %.3f %.3f\n%.3f %.3f %.3f\n",
             texName, "CoorMat",
             texcoord[0], texcoord[1], texcoord[2],
             texcoord[3], texcoord[4], texcoord[5],
             texcoord[6], texcoord[7], texcoord[8],
             texcoord[9], texcoord[10], texcoord[11]);
   glBegin(GL_QUADS);
                        /* Transpose image in bb, because matrix is column major 
                           stored in a transposed image in row major order  
            Image corners: A-B    Surface points forming square: 0-1
                           | |                                   | |
                           D-C                                   3-2 
                                Mapping: A-->0, B-->3, C-->*/
   glTexCoord2f(0.0, 0.0); /* A */
                   /* 0 */ glVertex3f(texcoord[0], texcoord[1], texcoord[2]);
   glTexCoord2f(0.0, 1.0); /* B */
                   /* 3 */ glVertex3f(texcoord[9], texcoord[10], texcoord[11]);
   glTexCoord2f(1.0, 1.0); /* C */
                   /* 2 */ glVertex3f(texcoord[6], texcoord[7], texcoord[8]);
   glTexCoord2f(1.0, 0.0); /* D */
                   /* 1 */ glVertex3f(texcoord[3], texcoord[4], texcoord[5]);
   glEnd();
   glFlush();
   glDisable(GL_TEXTURE_2D);
   if (pof) glEnable (GL_POLYGON_OFFSET_FILL); /* April 2011  */

   /* Put a frame around the matrix */
   glLineWidth(2);
   glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, per_col );
            /* Add 1 to the Z coord to make the square float */
   texcoord = GSaux->FrameSO->NodeList;
   glBegin(GL_LINE_LOOP);
      glVertex3f(texcoord[0], texcoord[1], texcoord[2]  +0.5);
      glVertex3f(texcoord[3], texcoord[4], texcoord[5]  +0.5);
      glVertex3f(texcoord[6], texcoord[7], texcoord[8]  +0.5);
      glVertex3f(texcoord[9], texcoord[10], texcoord[11]+0.5);
   glEnd();
            
   /* Now for the selection */
   if (SUMA_SV_GetShowSelectedDatum(sv)) {
      if (GSaux->PR->datum_index != -1) {
         /* Highlight cell */
         if (!SUMA_GDSET_SegIndexToPoints(dset, GSaux->PR->datum_index, 
                                          &ii, &jj, NULL)) {
            SUMA_S_Err("What else?"); goto BUGOUT;
         }

         if ((cc=SUMA_GDSET_edgeij_to_GMATRIX_XYZ(dset, ii, jj, XYZ, 1)) < 0) {
            SUMA_S_Err("Failed to get square"); goto BUGOUT;
         } else if (cc == 0) {
            SUMA_LH("Nothing to mark?");
            goto GETOUT;
         }
         if (cc>=4) {
            SUMA_LHv("Cell Loop (cc=%d)\n", cc);
            glLineWidth(2);
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, sq_col );
                     /* Add 1 to the Z coord to make the square float */
            glBegin(GL_LINE_LOOP);
               glVertex3f(XYZ[3],  XYZ[4],  XYZ[5] +1.0);
               glVertex3f(XYZ[6],  XYZ[7],  XYZ[8] +1.0);
               glVertex3f(XYZ[9],  XYZ[10], XYZ[11]+1.0);
               glVertex3f(XYZ[12], XYZ[13], XYZ[14]+1.0);
            glEnd();
         }
      } else if (GSaux->PR->selectedEnode != -1 ) {
         if ((cc=SUMA_GDSET_edgeij_to_GMATRIX_XYZ(dset, 
                           GSaux->PR->selectedEnode, -1, XYZ, 1)) < 0) {
            SUMA_S_Err("Failed to get square"); goto BUGOUT;
         } else if (cc == 0) {
            SUMA_LH("Nothing to mark?");
            goto GETOUT;
         }
         if (cc>=4) {
            SUMA_LHv("Loop1 (cc=%d)\n", cc);
            glLineWidth(2);
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, sq_col );
                     /* Add 1 to the Z coord to make the square float */
            glBegin(GL_LINE_LOOP);
               glVertex3f(XYZ[3],  XYZ[4],  XYZ[5] +1.0);
               glVertex3f(XYZ[6],  XYZ[7],  XYZ[8] +1.0);
               glVertex3f(XYZ[9],  XYZ[10], XYZ[11]+1.0);
               glVertex3f(XYZ[12], XYZ[13], XYZ[14]+1.0);
            glEnd();
         }
         if (cc>=8) {
            SUMA_LHv("Loop2 (cc=%d)\n", cc);
            glLineWidth(2);
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, sq_col );
                     /* Add 1 to the Z coord to make the square float */
            glBegin(GL_LINE_LOOP);
               glVertex3f(XYZ[15],  XYZ[16],  XYZ[17] +1.0);
               glVertex3f(XYZ[18],  XYZ[19],  XYZ[20] +1.0);
               glVertex3f(XYZ[21],  XYZ[22],  XYZ[23]+1.0);
               glVertex3f(XYZ[24],  XYZ[25],  XYZ[26]+1.0);
            glEnd();
         }
         
      }
   }
   
   /* Show the text ? */
   if (!(curcol = SUMA_ADO_CurColPlane((SUMA_ALL_DO *)dset))) {
      SUMA_S_Err("Could not find current col plane!");
      SUMA_RETURN (NOPE);
   }
   if ((fontGL = SUMA_Font2GLFont(curcol->Font))) {
      int nl, tw, th, bh, bw, skpv, skph,
          lh = SUMA_glutBitmapFontHeight(fontGL), kkk=0, SGN=-1;
      float off = -lh, vrat, hrat;
      float Sz[3];

      bh = SUMA_glutBitmapFontHeight(fontGL); /* Height of font, in pixels */
      bw = glutBitmapWidth(fontGL, 'M'); /* M's a fat letter, width in pixels*/
      SUMA_GDSET_GMATRIX_CellPixSize(dset, sv, Sz);
      vrat = Sz[1]/(float)bh;
      hrat = Sz[0]/(float)bw;
      skpv = (int)(1.0/vrat);
      skph = (int)(1.0/hrat);
      SUMA_LHv("Height %.2f pixels/cell, Font height %d, Rat:%f, skip %d\n"
               "Width  %.2f pixels/cell, Font Width  %d, Rat:%f, skip %d\n"
               "  GB = [%d %d]\n",
               Sz[1], bh, vrat, skpv, 
               Sz[0], bw, hrat, skph,
               GB[0], GB[1]);  
               
      
      if (!(names = SUMA_GDSET_GetPointNamesColumn(dset, &ii, &nelxyz))) {
         SUMA_LH("No names!"); /* No need to weep */
         goto GETOUT;
      }
      if (!(GNI = SUMA_GDSET_GetPointIndexColumn(dset, &ii, NULL))) {
         if (ii == -2) {
            SUMA_S_Err("Bad news for indices!!");
            goto BUGOUT;
         }
      }
      if (!(SUMA_GDSET_GMATRIX_Aff(dset, Aff, 1))) {
         SUMA_S_Err("No Aff!!");
         goto BUGOUT;
      }
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor); 
      glMaterialfv(GL_FRONT, GL_EMISSION, txcol); /*turn on emissidity for text*/
      /* Do the column side */
      ee = 0;
      while (ee < N[0]) {
         if (!ui) {
            eem = ee; 
         } else {
            eem = ui[ee];
         }
         if (!GNI) iname = eem;
         else if ((iname = SUMA_ibinFind(GNI, nelxyz->vec_len, eem)) < 0){
            SUMA_S_Err("Index not found!");
            goto BUGOUT;
         }
         
         I[0] = (ee+0.5)*GB[0]-0.5; /* center of cell at row ee in pixels */
         I[1] = -0.15*GB[1]-0.5; /* offset to the left for where text will end */
         I[2] = 0.0; 
         AFF44_MULT_I(XYZ, Aff,I);
         glRasterPos3d(XYZ[0], XYZ[1], XYZ[2]);
         glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
         glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
         SUMA_TextBoxSize (names[iname], &tw, &th, &nl, fontGL);
         SUMA_LHv("For XYZ %f %f %f, name %s, width %d, height %d\n"
                  "Raster position (%g,%g, %g) is %s\n",
                  XYZ[0], XYZ[1], XYZ[2], names[iname], tw, th, 
                  rpos[0], rpos[1], rpos[2], 
                  valid ? "valid" : "INVALID");
      
         /* do some text action */
         if (valid) {
            glColor3fv(txcol); 
               /* offset for right align */
            glBitmap( 0, 0, 0, 0,  -(float)tw, -(float)th/4.0,  NULL );
            for (ii=0; names[iname][ii] != '\0'; ii++) {
               glutBitmapCharacter(fontGL, names[iname][ii]);
            }
         }
         /* A little notch? Just modify I[1] (matrix column index)*/
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, per_col );
         glBegin(GL_LINES);
         I[1] = -0.10*GB[1]-0.5; AFF44_MULT_I(XYZ, Aff,I);
         glVertex3f(XYZ[0],  XYZ[1],  XYZ[2] +1.5);
         I[1] = 0.05*GB[1]-0.5; AFF44_MULT_I(XYZ, Aff,I);
         glVertex3f(XYZ[0],  XYZ[1],  XYZ[2] +1.5);
         glEnd();
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);

         ee = ee+1+skpv;
      }
      ee = 0;
      while (ee < N[1]) {
         if (!uj) {
            eem = ee; 
         } else {
            eem = uj[ee];
         }
         if (!GNI) iname = eem; /* implicit */
         else if ((iname = SUMA_ibinFind(GNI, nelxyz->vec_len, eem)) < 0){
            SUMA_LH("Index not found!");
            goto BUGOUT;
         }
         I[0] = (N[0]+ 0.15)*GB[0]-0.5; I[1] = (ee+0.5)*GB[1]-0.5; I[2] = 0.0;
         AFF44_MULT_I(XYZ, Aff,I);
         glRasterPos3d(XYZ[0], XYZ[1], XYZ[2]);
         glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
         glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
         SUMA_TextBoxSize (names[iname], &tw, &th, &nl, fontGL);
         SUMA_LHv("For XYZ %f %f %f, name %s, text width %d\n"
                  "Raster position (%g,%g, %g) is %s\n",
                  XYZ[0], XYZ[1], XYZ[2], names[iname], tw,
                  rpos[0], rpos[1], rpos[2], 
                  valid ? "valid" : "INVALID");
         /* do some text action, go vert.*/
         off = 0;
         if (valid) {
            glColor3fv(txcol); 
            /* offset for varying horizontal center align 
               This should be done based on necessity or user
               input. Perhaps just offset those whose width
               exceeds a cell's width in screen pixels...
               Deal with this later.*/
            /* center on 1st character */   
               tw = glutBitmapWidth(fontGL,names[iname][0]);
               th = SUMA_glutBitmapFontHeight(fontGL);
               glBitmap( 0, 0, 0, 0,  
                         -(float)tw/2.0, -th,  NULL ); 
            for (ii=0; names[iname][ii] != '\0'; ii++) {
               tw = glutBitmapWidth(fontGL,names[iname][ii]);
               glutBitmapCharacter(fontGL, names[iname][ii]);
               /* Backup width, go down for vertical offset */
               glBitmap( 0, 0, 0, 0,  
                         -tw, -0.8*th,  NULL ); 
            }  
         }
         /* A little vertical notch? Just modify I[0] (matrix row index)*/
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, per_col );
         glBegin(GL_LINES);
         I[0] = (N[0]+0.15)*GB[0]-0.5; AFF44_MULT_I(XYZ, Aff,I);
         glVertex3f(XYZ[0],  XYZ[1],  XYZ[2] +1.5);
         I[0] = (N[0]-0.0)*GB[0]-0.5; AFF44_MULT_I(XYZ, Aff,I);
         glVertex3f(XYZ[0],  XYZ[1],  XYZ[2] +1.5);
         glEnd();
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
         
         ee = ee+1+skph;
      }

      glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);  
         /*turn off emissidity for text*/ 
   }

   
   goto GETOUT;
   
   BUGOUT:
   ans = NOPE;
   
   GETOUT:
   if (pof) glEnable (GL_POLYGON_OFFSET_FILL); /* April 2011  */
   
#if USE_SER
   /* Use in concert with SUMA_RecordEnablingState above */
   SUMA_RestoreEnablingState(&(sv->SER));  
#endif
      
   SUMA_RETURN(ans);
}

/*
   If you change how coordinates of framing surface relate
   to matrix pixel indices and matrix cells you should also
   update SUMA_WhatWasPicked_FrameSO(), SUMA_GDSET_edgeij_to_GMATRIX_XYZ(),
   and SUMA_DrawGraphDO_GMATRIX() at the very least
*/
SUMA_SurfaceObject *SUMA_Surface_Of_NIDO_Matrix(SUMA_NIDO *nido)
{
   static char FuncName[]={"SUMA_Surface_Of_NIDO_Matrix"};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_NEW_SO_OPT *nsoopt=NULL;
   NI_element *nini = NULL;
   float *NodeList=NULL, Eoff[2];
   double Aff[4][4], I[3], V[12], X[3];
   int M[2], Mf[2], GB[2], G[2], B[2], k, N_Node, N_FaceSet, 
       *FaceSetList=NULL, i, i3;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nido || !(nini = SUMA_FindNgrNamedElement(nido->ngr,"Slice"))) {
      SUMA_S_Err("Could not find Slice");
      SUMA_RETURN(SO);
   }
   
   NI_GET_INTv(nido->ngr, "PixCount", M, 2, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No pixel dims!");
      SUMA_RETURN(SO);
   }
   
   /* total num of pixels per value */
   NI_GET_INTv(nido->ngr, "PixPerVal", G, 2, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No PixPerVal!");
      SUMA_RETURN(SO);
   }
   NI_GET_INTv(nido->ngr, "BorWid", B, 2, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No BorWid!");
      SUMA_RETURN(SO);
   }
   GB[0] = G[0]+B[0];
   GB[1] = G[1]+B[1];
   
   NI_GET_DOUBLEv(nido->ngr, "ijk_to_dicom_real", V, 12, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No ijk_to_dicom_real!");
      SUMA_RETURN(SO);
   } else {
      V12_TO_AFF44(Aff, V);
   }
   
   
   NI_GET_INT(nini, "k", k);
   if (!NI_GOT) {
      SUMA_S_Warn("No k, setting to 0");
      k = 0;
   }
   
   /* Prepare the grid (see labbook NIH-6, pp 243*/
   N_Node = 12;
   N_FaceSet = 10;
   NodeList = (float *)SUMA_calloc(3*N_Node, sizeof(float));
   FaceSetList = (int *)SUMA_calloc(3*N_FaceSet, sizeof(int));
   I[2] = k;
   
   /* load the coordinates of the corners (not voxel/cell centers) */
   Mf[0] = M[0]-1; Mf[1] = M[1]-1; /* -1 because you want the max 
                                 to be the last index, not number of voxels*/
    /* Offset size for row/col selection, keep it generous in size, no
       harm in that.  */
   Eoff[0] = SUMA_MAX_PAIR(1, Mf[0]/2.0); 
   Eoff[1] = SUMA_MAX_PAIR(1, Mf[1]/2.0); 
   
   /* Oh just use the same dimension from the surround select region */
   Eoff[0] = SUMA_MAX_PAIR(Eoff[0], Eoff[1]);
   Eoff[1] = Eoff[0];
   
   i = 0; i3 = 3*i;  I[0] = -0.5; I[1] = -0.5;  
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   i = 1; i3 = 3*i;  I[0] = -0.5; I[1] = Mf[1]+0.5;  
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
     
   i = 2; i3 = 3*i;  I[0] = Mf[0]+0.5; I[1] = Mf[1]+0.5;  
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
     
   i = 3; i3 = 3*i;  I[0] = Mf[0]+0.5; I[1] = -0.5;  
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   i = 4; i3 = 3*i;  I[0] = -Eoff[0]-0.5; I[1] = -0.5;  
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   i = 5; i3 = 3*i;  I[0] = -Eoff[0]-0.5; I[1] = Mf[1]+0.5; 
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   i = 6; i3 = 3*i;  I[0] = -0.5; I[1] = Mf[1]+Eoff[1]+0.5;  
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   i = 7; i3 = 3*i;  I[0] = Mf[0]+0.5; I[1] = Mf[1]+Eoff[1]+0.5; 
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   i = 8; i3 = 3*i;  I[0] = Mf[0]+Eoff[0]+0.5; I[1] = Mf[1]+0.5; 
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   i = 9; i3 = 3*i;  I[0] = Mf[0]+Eoff[0]+0.5; I[1] = -0.5;  
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   i = 10; i3 = 3*i;  I[0] = Mf[0]+0.5; I[1] = -Eoff[1]-0.5;  
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   i = 11; i3 = 3*i;  I[0] = -0.5; I[1] = -Eoff[1]-0.5;  
                                                         AFF44_MULT_I(X, Aff,I);
   NodeList[i3++] = X[0]; NodeList[i3++] = X[1]; NodeList[i3++] = X[2];   
   
   /* form the triangles */
   i = 0; i3 = 3*i;
   FaceSetList[i3++] = 0; FaceSetList[i3++] = 1; FaceSetList[i3++] = 2; 
   
   i = 1; i3 = 3*i;
   FaceSetList[i3++] = 0; FaceSetList[i3++] = 2; FaceSetList[i3++] = 3; 
   
   i = 2; i3 = 3*i;
   FaceSetList[i3++] = 4; FaceSetList[i3++] = 5; FaceSetList[i3++] = 1; 
   
   i = 3; i3 = 3*i;
   FaceSetList[i3++] = 4; FaceSetList[i3++] = 1; FaceSetList[i3++] = 0; 
   
   i = 4; i3 = 3*i;
   FaceSetList[i3++] = 6; FaceSetList[i3++] = 7; FaceSetList[i3++] = 2; 
   
   i = 5; i3 = 3*i;
   FaceSetList[i3++] = 6; FaceSetList[i3++] = 2; FaceSetList[i3++] = 1; 
   
   i = 6; i3 = 3*i;
   FaceSetList[i3++] = 8; FaceSetList[i3++] = 9; FaceSetList[i3++] = 3; 
   
   i = 7; i3 = 3*i;
   FaceSetList[i3++] = 8; FaceSetList[i3++] = 3; FaceSetList[i3++] = 2; 
   
   i = 8; i3 = 3*i;
   FaceSetList[i3++] = 10; FaceSetList[i3++] = 11; FaceSetList[i3++] = 0; 
   
   i = 9; i3 = 3*i;
   FaceSetList[i3++] = 10; FaceSetList[i3++] = 0; FaceSetList[i3++] = 3; 
   
   nsoopt = SUMA_NewNewSOOpt();
   SO = SUMA_NewSO( &NodeList, N_Node, &FaceSetList, N_FaceSet, nsoopt );
   nsoopt = SUMA_FreeNewSOOpt(nsoopt);
   
   if (LocalHead) {
      char *ss = SUMA_SurfaceObject_Info(SO, NULL);
      fprintf(SUMA_STDERR,"La Surface\n%s\n", ss);
      SUMA_ifree(ss);
   }
   
   SUMA_RETURN(SO);
}

SUMA_Boolean SUMA_DrawGraphDO_GRELIEF (SUMA_GraphLinkDO *gldo, 
                                       SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawGraphDO_GRELIEF"};
   SUMA_ENTRY;

   SUMA_S_Err("Fill me up");

   SUMA_RETURN(YUP);
}

/* Fillup the Updates list to recreate everything */
SUMA_Boolean SUMA_DrawDO_UL_FullMonty(DList *dl) 
{
   static char FuncName[]={"SUMA_DrawDO_UL_FullMonty"};
   char *eldata=NULL;
   DListElmt *el = NULL;
   SUMA_ENTRY;
   if (!dl) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }   
   if (dlist_size(dl)) {
      SUMA_S_Warn("Update List not empty, emptying it now");
      SUMA_DrawDO_UL_EmptyList(dl, NULL);
   }
   dlist_ins_next(dl, dlist_tail(dl), SUMA_copy_string("SDO_NodeList"));
   dlist_ins_next(dl, dlist_tail(dl), SUMA_copy_string("SDO_MapColors"));
   dlist_ins_next(dl, dlist_tail(dl), SUMA_copy_string("SDO_SetStippling"));

   dlist_ins_next(dl, dlist_tail(dl), SUMA_copy_string("nido_MapColors"));   
   
   SUMA_RETURN(YUP);   
}


/* Add an update */
SUMA_Boolean SUMA_ADO_UL_Add(SUMA_ALL_DO *ado, char *com, int replace) 
{
   static char FuncName[]={"SUMA_ADO_UL_Add"};
   SUMA_GRAPH_SAUX *GSaux = NULL;
   
   SUMA_ENTRY;
   
   if (!(GSaux = SUMA_ADO_GSaux(ado))) SUMA_RETURN(NOPE);
   
   SUMA_RETURN(SUMA_DrawDO_UL_Add(GSaux->DisplayUpdates, com, replace));
}

SUMA_Boolean SUMA_DrawDO_UL_Add(DList *dl, char *com, int replace) 
{
   static char FuncName[]={"SUMA_DrawDO_UL_Add"};
   void *eldata=NULL;
   DListElmt *el = NULL;
   
   SUMA_ENTRY;
   
   if (!dl || !com || com[0]=='\0') {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }   

   if (replace) {
      if ((el = SUMA_DrawDO_UL_Find(dl, com))) {
         dlist_remove(dl, el, (void **)&eldata);
         SUMA_Free_Saux_DisplayUpdates_datum(eldata);
      }
   }  
   dlist_ins_next(dl, dlist_tail(dl), SUMA_copy_string(com));
   SUMA_RETURN(YUP);   
}


DListElmt *SUMA_DrawDO_UL_Find(DList *dl, char *com) 
{
   static char FuncName[]={"SUMA_DrawDO_UL_Find"};
   char *eldata=NULL;
   DListElmt *el = NULL, *eli=NULL;
   
   SUMA_ENTRY;
   
   if (!dl || !dlist_size(dl) || !com) {
      SUMA_RETURN(el);
   }   
   
   el = eli = NULL;
   do {
      if (!eli) eli = dlist_head(dl);
      else eli = dlist_next(eli);
      if (!strcmp(com, (char *)(eli->data))) el = eli;
   } while (!el && eli != dlist_tail(dl));
   
   SUMA_RETURN(el);
}

/* empty updates element del, or the whole list if del=NULL*/
SUMA_Boolean SUMA_DrawDO_UL_EmptyList(DList *dl, DListElmt *del) 
{
   static char FuncName[]={"SUMA_DrawDO_UL_EmptyList"};
   void *eldata=NULL;
   DListElmt *el = NULL;
   
   SUMA_ENTRY;
   if (!dl) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   if (!dlist_size(dl)) SUMA_RETURN(YUP);
   if (del) {
      dlist_remove(dl, del, &eldata);
      SUMA_Free_Saux_DisplayUpdates_datum(eldata);
   } else {
      while((el = dlist_head(dl))) {
         dlist_remove(dl, el, &eldata);
         SUMA_Free_Saux_DisplayUpdates_datum(eldata);
      }
   }
   SUMA_RETURN(YUP);     
}

#define USE_SER 0
SUMA_Boolean SUMA_DrawGraphDO_G3D (SUMA_GraphLinkDO *gldo, 
                                   SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawGraphDO_G3D"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int iseg, N_seg, usedel=0;
   DListElmt *el=NULL, *eln=NULL;
   float origwidth=0.0;
   GLboolean gl_sm=FALSE;
   char Label[64]={""};
   SUMA_Boolean ans = YUP;
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)gldo;
   SUMA_DSET *dset=NULL;
   SUMA_GRAPH_SAUX *GSaux = NULL;
   SUMA_Boolean LocalHead = NOPE; 
   
   SUMA_ENTRY;
   
   if (!gldo || !(dset=SUMA_find_GLDO_Dset(gldo)) || !sv 
         || !SUMA_isGraphDset(dset) || !(GSaux=SDSET_GSAUX(dset))) {
      fprintf(stderr,"Error %s: NULL or bad pointers.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
      
   if (sv->DO_PickMode) {
      SUMA_LH("In picking mode!");
   }
   

   #if USE_SER
   SUMA_RecordEnablingState(&(sv->SER)); /* Lazy, consider SUMA_GLStateTrack ,
                                         Also, why record it if it is already
                                         well preserved. */ 
   #endif
   
   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   /* Form segment DO*/
   snprintf(Label, 63*sizeof(char), "Edges_%s_G3D", SDSET_LABEL(dset));
   if (!GSaux->SDO) {
      if (!(GSaux->SDO = SUMA_Alloc_SegmentDO(SDSET_VECLEN(dset), Label, 1, 
                                 SUMA_ADO_idcode(ado), 2, 
                                 NBOLS_type, ado->do_type,
                                 SUMA_ADO_variant(ado)))) {
         SUMA_S_Errv("Failed to allocate for GSaux->SDO %s (%d)\n",
                     Label, SDSET_VECLEN(dset));
         goto BUGOUT;
      }
   }
   /* fill in all the parameters per the controller settings */
   GSaux->SDO->Stipple = SUMA_SOLID_LINE;
   GSaux->SDO->LineWidth = 3;
   GSaux->SDO->LineCol[0] = 0.4; GSaux->SDO->LineCol[1] = 0.8; 
           GSaux->SDO->LineCol[2] = 0.1; GSaux->SDO->LineCol[3] = 1.0;
   
   if (dlist_size(GSaux->DisplayUpdates)) {/* GSaux->SDO needs updating */
      el = dlist_head(GSaux->DisplayUpdates);
      do {
         usedel = 1; /* assume will consumate this element */
                if (!strcmp((char *)el->data,"SDO_NodeList")) {
               SUMA_GDSET_NodeList(dset, NULL, 1, NULL, SUMA_ADO_variant(ado)); 
         } else if (!strcmp((char *)el->data,"SDO_MapColors")) {
            SUMA_LH("Here is where you decide what needs coloring, \n"
                         "showing, etc. etc. For now we show all.\n");
            N_seg = SDSET_VECLEN(dset);
            if (!GSaux->SDO->NodeID) 
               GSaux->SDO->NodeID = (int *)SUMA_calloc(N_seg, sizeof(int));
            if (!GSaux->SDO->NodeID1) 
               GSaux->SDO->NodeID1 = (int *)SUMA_calloc(N_seg, sizeof(int));
            if (!GSaux->SDO->NodeID || !GSaux->SDO->NodeID1) {
               SUMA_S_Errv("Failed to allocate for %d segment indices\n",
                           N_seg);
               goto BUGOUT;
            }
            /* Fillup node segments */
            for(iseg=0; iseg<N_seg; ++iseg) {
               if (!SUMA_GDSET_SegRowToPoints(dset, iseg, 
                                                GSaux->SDO->NodeID+iseg, 
                                                GSaux->SDO->NodeID1+iseg,
                                                NULL)){
                  SUMA_S_Errv("Failed for edge %d\n", iseg);
               }
               #if 0
               SUMA_LHv("Edge row %d: %d %d\n", 
                        iseg, GSaux->SDO->NodeID[iseg], 
                              GSaux->SDO->NodeID1[iseg]);
               #endif
            }
            /* Colors? */
            GSaux->SDO->colv = SUMA_GetColorList(sv, SDSET_ID(dset));
            /* thickness? */
            GSaux->SDO->thickv = NULL;
            /* number of uniqe points */
            SUMA_Set_N_UnqNodes_SegmentDO(GSaux->SDO, GDSET_MAX_POINTS(dset));
         } else if (!strcmp((char *)el->data,"SDO_SetStippling")) {
            SUMA_LH("Not ready for stippling yet");
         }else {
            usedel = 0;
            /* this may not need to be an error condition, you might just skip 
            this update if it is irrelevant ?*/
            SUMA_LHv("DisplayUpdates command %s not relevant here\n",
                        (char *)el->data);
         }
         
         if (el != dlist_tail(GSaux->DisplayUpdates)) eln = dlist_next(el);
         else eln = NULL;
         if (usedel) {
            /* delete used element */
            SUMA_DrawDO_UL_EmptyList(GSaux->DisplayUpdates, el);
            el = NULL;
         }
         el = eln;
      } while (el);
   }
   /* and draw the GSaux */   
   if (!SUMA_DrawGSegmentDO(GSaux, sv)) {
      SUMA_S_Err("Failed to draw Segment DO!");
      goto BUGOUT;
   }
   
   goto GETOUT;
   
   BUGOUT:
   ans = NOPE;
   
   GETOUT:
   
#if USE_SER
   /* Use in concert with SUMA_RecordEnablingState above */
   SUMA_RestoreEnablingState(&(sv->SER));  
#else
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity */
   glLineWidth(origwidth);
#endif
      
   SUMA_RETURN(ans);
}


/*!
   Be careful not to access more than do_type in ado before
   you verify that the pointer is indeed for a DO */
SUMA_Boolean SUMA_Load_Dumb_DO(SUMA_ALL_DO *ado, SUMA_DUMB_DO *DDO)
{
   static char FuncName[]={"SUMA_Load_Dumb_DO"};
   
   SUMA_ENTRY;
   
   if (!ado || !DDO) SUMA_RETURN(NOPE);
    
   memset(DDO,0,sizeof(SUMA_DUMB_DO));
   DDO->err = 2; /* = not filled properly */
   
   switch (ado->do_type) {
      case SO_type: { 
         SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
         DDO->idcode_str = SO->idcode_str;
         DDO->NodeList = SO->NodeList;
         DDO->N_Node = SO->N_Node;  
         DDO->NodeIndex = NULL;
         if (SO->EL) DDO->AvgLe = SO->EL->AvgLe;
         
         DDO->err = 0;
         break; }
      case SDSET_type: {
         SUMA_S_Err("Bad idea, no nodelist possible without variant");
         break; }
      case GRAPH_LINK_type:{
         DDO->idcode_str = SUMA_ADO_idcode(ado);
         DDO->NodeList = SUMA_GDSET_NodeList(
                              SUMA_find_GLDO_Dset((SUMA_GraphLinkDO *)ado), 
                              &(DDO->N_Node), 0, &(DDO->NodeIndex),
                              SUMA_ADO_variant(ado));
         DDO->AvgLe = 4;
         DDO->err = 0;
         break; }
      default:
         SUMA_S_Errv("Not used to filling type %d\n", ado->do_type);
         SUMA_RETURN(!DDO->err);
         
   }
   
   SUMA_RETURN(!DDO->err);
}

void SUMA_Free_Saux_DisplayUpdates_datum(void *ddd)
{
   char *upd = (char *)ddd;
   
   SUMA_ifree(ddd);
   
   return;
}

SUMA_Boolean SUMA_AddDsetSaux(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_AddDsetSaux"};
   
   SUMA_ENTRY;
   
   if (!dset || !dset->Aux) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   
   if (SUMA_isGraphDset(dset)) {
      SUMA_GRAPH_SAUX *GSaux;
      if (dset->Aux->Saux) {
         GSaux = (SUMA_GRAPH_SAUX *)dset->Aux->Saux;
         /* empty old updates list */
         SUMA_DrawDO_UL_EmptyList(GSaux->DisplayUpdates, NULL);
         
         if (GSaux->SDO) {/* Free segment DO */
            /* remove this object from the colid list */
            SUMA_Remove_From_Pick_Colid_list(NULL,GSaux->SDO->idcode_str);
            SUMA_free_SegmentDO(GSaux->SDO); GSaux->SDO = NULL;
         }
         
         if (GSaux->nido) {/* Free segment DO */
            GSaux->nido = SUMA_free_NIDO(GSaux->nido);
         }
         if (GSaux->Overlay) {
            SUMA_S_Warn("Have overlay already, will remove it. Revisit later.");
            SUMA_FreeOverlayPointer(GSaux->Overlay);
            GSaux->Overlay = NULL;
         }
         
         if (GSaux->net) GSaux->net = NULL; /* pointer copy, do not free */
         if (GSaux->thd) SUMA_DestroyNgrHashDatum(GSaux->thd); GSaux->thd = NULL;
         
         if (GSaux->DOCont) {
            SUMA_S_Warn("Have controller already. Keep it.");
         }
      } else {
         dset->Aux->FreeSaux = SUMA_Free_GSaux;
         dset->Aux->Saux = (void *)SUMA_calloc(1,sizeof(SUMA_GRAPH_SAUX));
         GSaux = (SUMA_GRAPH_SAUX *)dset->Aux->Saux;
         
         GSaux->DisplayUpdates = (DList *)SUMA_malloc(sizeof(DList));
         dlist_init(GSaux->DisplayUpdates, SUMA_Free_Saux_DisplayUpdates_datum);
         
         GSaux->SDO = NULL;
         GSaux->nido = NULL;
         GSaux->Overlay = NULL;
         GSaux->DOCont = SUMA_CreateSurfContStruct(SDSET_ID(dset), SDSET_type);
         GSaux->PR = SUMA_New_Pick_Result(NULL);
         GSaux->thd = NULL;
         GSaux->net = NULL;
      }
      
      SUMA_DrawDO_UL_FullMonty(GSaux->DisplayUpdates);
   }    
   
   
   SUMA_RETURN(YUP);  
}

/* destroy the hash table of the tract paths */
SUMA_Boolean SUMA_DestroyNgrHashDatum(SUMA_NGR_INDEX_HASH_DATUM *thd)
{
   static char FuncName[]={"SUMA_DestroyNgrHashDatum"};
   SUMA_NGR_INDEX_HASH_DATUM *hd=NULL;
   
   SUMA_ENTRY;
   
   if (!thd) SUMA_RETURN(YUP);
   
   /* destroy all of the hash table */
   while (thd) {
      hd = thd;  /* will delete the head of the hash table list */
      HASH_DEL( thd, hd); /* remove the head from the list, after
                                 this macro, thd points to the next
                                 item in the list; the new head */ 
      SUMA_free(hd); hd = NULL; /* free hd, no longer needed */
   }
      
   SUMA_RETURN(YUP);
}

void SUMA_Free_GSaux(void *vSaux)
{
   static char FuncName[]={"SUMA_Free_GSaux"};
   
   SUMA_GRAPH_SAUX *Saux;
   
   if (!vSaux) return;
   Saux = (SUMA_GRAPH_SAUX *)vSaux;
   
   if (Saux->DisplayUpdates) {
      dlist_destroy(Saux->DisplayUpdates);
      SUMA_free(Saux->DisplayUpdates);
   }
   if (Saux->SDO) {
      SUMA_Remove_From_Pick_Colid_list(NULL,Saux->SDO->idcode_str);
      SUMA_free_SegmentDO(Saux->SDO); Saux->SDO = NULL;
   }
   
   if (Saux->FrameSO) SUMA_Free_Surface_Object(Saux->FrameSO); 
   Saux->FrameSO = NULL;
   Saux->nido = SUMA_free_NIDO(Saux->nido); 
   
   if (Saux->Overlay) {
      SUMA_FreeOverlayPointer(Saux->Overlay);
      Saux->Overlay = NULL;
   }
   
   SUMA_ifree(Saux->isColored);
   
   if (Saux->DOCont) SUMA_FreeSurfContStruct(Saux->DOCont);
   Saux->DOCont=NULL;
   
   if (Saux->PR) Saux->PR = SUMA_free_PickResult(Saux->PR);
   
   if (Saux->thd) SUMA_DestroyNgrHashDatum(Saux->thd); Saux->thd = NULL;
   if (Saux->net) Saux->net = NULL; /* never free this pointer copy */
   
   SUMA_ifree(Saux);
   return; 
}

/* From the indices of two nodes that form an edge,
find the XYZ coordinates of the center of the regions
where it would displayed in its frame surface 
\sa the function where picking is done on a GMATRIX
and function that create FrameSO 

Caution, XYZ may contain as many as 9 XYZ triplets (27 floats)

The function returns the number of triplets stored in XYZ
-1 to flag an error

For more coordinate translations see also:  
SUMA_WhatWasPicked_FrameSO(), SUMA_Surface_Of_NIDO_Matrix(), SUMA_GDSET_edgeij_to_GMATRIX_XYZ() and SUMA_DrawGraphDO_GMATRIX()
*/
int SUMA_GDSET_edgeij_to_GMATRIX_XYZ(SUMA_DSET *dset, 
                                        int ei, int ej, float *XYZ,
                                        int FourCorners) 
{
   static char FuncName[]={"SUMA_GDSET_edgeij_to_GMATRIX_XYZ"};
   SUMA_GRAPH_SAUX *GSaux=NULL;
   double V[12], Aff[4][4];
   int iim, jjm, cc = 0, eemi, eemj, ee, *ui, *uj, N[3], G[3], B[3], GB[3], M[3];
   float I[3], Ic[3], xxx[3];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !XYZ || (ei < 0 && ej < 0)) SUMA_RETURN(-1);
   
   GSaux = SDSET_GSAUX(dset);   
   if (!GSaux || !GSaux->FrameSO) { 
      SUMA_RETURN(-1);
   }
   
   /* number of values in matrix */
   N[0] = SDSET_MATRIX_SZ0(dset); N[1] = SDSET_MATRIX_SZ1(dset); 

   NI_GET_DOUBLEv(GSaux->nido->ngr, "ijk_to_dicom_real", V, 12, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No ijk_to_dicom_real");
      SUMA_RETURN(-1);
   }   
   V12_TO_AFF44(Aff, V);

   /* total num of pixels per value */
   NI_GET_INTv(GSaux->nido->ngr, "PixPerVal", G, 3, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No PixPerVal!");
      SUMA_RETURN(-1);
   }
   NI_GET_INTv(GSaux->nido->ngr, "BorWid", B, 3, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No BorWid!");
      SUMA_RETURN(-1);
   }
   GB[0] = G[0]+B[0];
   GB[1] = G[1]+B[1];
   
   NI_GET_INTv(GSaux->nido->ngr, "PixCount", M, 3, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No PixCount!");
      SUMA_RETURN(-1);
   }
   ui = uj = NULL;
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
      case MAT_TRI:
      case MAT_TRI_DIAG:
         SUMA_LH("Direct indexing between edge points and matrix row/col");
         break;
      case MAT_SPARSE:
         if (!dset->inel) {
            SUMA_S_Err("Don't have inel, badly shaped dataset");
            SUMA_RETURN(-1);
         }
         if (  !(ui = SUMA_GetUniqueIndicesVec(dset,1)) || 
               !(uj = SUMA_GetUniqueIndicesVec(dset,2))    ) {
            SUMA_S_Err("Failed to get unique indices");
            SUMA_RETURN(-1);
         }
         break;
      default:
         SUMA_S_Err("Rats");
         SUMA_RETURN(-1);
         break;
   }
   
   if (ei >= 0 && ej >= 0) {
      if (!ui) {
         iim = ei; jjm = ej;
      } else {
         iim = SUMA_ibinFind(ui, N[0], ei);
         jjm = SUMA_ibinFind(uj, N[1], ej);
      }
      /* CENTROID pixel coord in displayed matrix of cell(iim, jjm) */
      Ic[0] = (int)((iim+0.5)*GB[0]); 
      Ic[1] = (int)((jjm+0.5)*GB[1]); 
      Ic[2] = 0.0;
      SUMA_LHv("Matrix indices %d %d to Display indices of %d %d \n"
                   "    to Pixels %f %f (GB %d %d) to XYZ:\n", 
                   ei, ej, iim, jjm, Ic[0], Ic[1], GB[0], GB[1]);

      AFF44_MULT_I(XYZ, Aff,Ic); cc = 3;
      
      /* Now fill the coords of the 4 corners forming the square 
         around the cell */
      if (FourCorners) {
         I[0] = Ic[0]-GB[0]/2.0; I[1] = Ic[1]-GB[1]/2.0; I[2] = 0.0;
            AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* A */
         I[0] = Ic[0]-GB[0]/2.0; I[1] = Ic[1]+GB[1]/2.0; I[2] = 0.0;
            AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* B */
         I[0] = Ic[0]+GB[0]/2.0; I[1] = Ic[1]+GB[1]/2.0; I[2] = 0.0;
            AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* C */
         I[0] = Ic[0]+GB[0]/2.0; I[1] = Ic[1]-GB[1]/2.0; I[2] = 0.0;
            AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* D */
      }
   } else {
      if (ei >= 0) ee = ei;
      else ee = ej;
      
      if (!ui) {
         eemi = ee; eemj = ee;
      } else {
         eemi = SUMA_ibinFind(ui, N[0], ee);
         eemj = SUMA_ibinFind(uj, N[1], ee);
      }
      
      
      cc = 0;/* Not sure what to do for a center location, leave blank */
      XYZ[0] = XYZ[1] = XYZ[2] = 0.0; /* all zeros means nothing was found */
      if (eemi >= 0) { /* exists as row, set its Y coordinate only */
         I[0] = (eemi+0.5)*GB[0]; I[1] = 0; I[2] = 0.0;
         AFF44_MULT_I(xxx, Aff,I); cc = 3;
         XYZ[0] = XYZ[2] = 0.0; XYZ[1] = xxx[1];
      }
      if (eemj >= 0) { /* exists as column, set its X coordinate only*/
         I[0] = 0; I[1] = (eemj+0.5)*GB[1]; I[2] = 0.0;
         AFF44_MULT_I(xxx, Aff,I); cc = 3;
         XYZ[0] = xxx[0]; XYZ[1] = XYZ[2] = 0.0;
      }
      
      /* Now fill the coords of the 4 corners forming the rectangle 
         around the the whole row and the whole column whenever 
         applicable*/
      if (FourCorners) {
         cc=3;
         if (eemi >=0) { /* Point exists as row */
            SUMA_LHv("Marking row %d for edge point %d\n", eemi, ee);
                           /* The + B[k]/2.0 offset is to make the 
                           rectangle highlight sit over where the
                           user expects the boundary between cells 
                           to be, and to match how the cell selection
                           square is drawn. In reality only the top and left
                           boundaries belong to a cell */
            I[0] = eemi*GB[0]-0.5 + B[0]/2.0; I[1] = -0.5 + B[1]/2.0; 
                                                                  I[2] = 0.0;
               AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* A */
            I[0] = eemi*GB[0]-0.5 + B[0]/2.0; I[1] = M[1]-0.5 - B[1]/2.0;
                                                                  I[2] = 0.0;
               AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* B */
            I[0] = (eemi+1)*GB[0]-0.5+ B[0]/2.0; I[1] = M[1]-0.5 - B[1]/2.0; 
                                                                  I[2] = 0.0;
               AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* C */
            I[0] = (eemi+1)*GB[0]-0.5+ B[0]/2.0; I[1] = -0.5 + B[1]/2.0; 
                                                                  I[2] = 0.0;
               AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* D */
          }
          if (eemj >=0) { /* Point exists as column */   
            SUMA_LHv("Marking col %d for edge point %d\n", eemj, ee);
             I[0] = -0.5 + B[1]/2.0; I[1] = (eemj)*GB[1]-0.5 + B[1]/2.0; 
                                                                  I[2] = 0.0;
               AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* A */
             I[0] = -0.5 + B[1]/2.0; I[1] = (eemj+1)*GB[1]-0.5 + B[1]/2.0; 
                                                                  I[2] = 0.0;
               AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* B */
             I[0] = M[0]-0.5 - B[1]/2.0; I[1] = (eemj+1)*GB[1]-0.5 + B[1]/2.0; 
                                                                  I[2] = 0.0;
               AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* C */
             I[0] = M[0]-0.5 - B[1]/2.0; I[1] = (eemj)*GB[1]-0.5 + B[1]/2.0;  
                                                                  I[2] = 0.0;
               AFF44_MULT_I((XYZ+cc), Aff,I); cc += 3; /* D */  
          }
      }
          
   }
   
   SUMA_RETURN((cc/3)); /* Return the number of XYZ triplets filled */
}

/* 
   Get the size of a matrix cell in screen pixels
*/
int SUMA_GDSET_GMATRIX_CellPixSize(SUMA_DSET *dset, SUMA_SurfaceViewer *sv,
                                   float *Szc)
{
   static char FuncName[]={"SUMA_GDSET_GMATRIX_CellPixSize"};
   SUMA_GRAPH_SAUX *GSaux=NULL;
   float S[12], V[3],  Sz[3];
   int N[3];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !Sz || !sv) SUMA_RETURN(0);
   
   GSaux = SDSET_GSAUX(dset);   
   if (!GSaux || !GSaux->FrameSO) { 
      SUMA_RETURN(0);
   }
   
   /* number of values in matrix */
   N[0] = SDSET_MATRIX_SZ0(dset); N[1] = SDSET_MATRIX_SZ1(dset); 

   /* Get the width of the square in which the matrix is displayed */
   V[0] = SUMA_ABS(  GSaux->FrameSO->NodeList[1*3+0] - 
                     GSaux->FrameSO->NodeList[0*3+0] );
   /* Get the height of the square in which the matrix is displayed */
   V[1] = SUMA_ABS(  GSaux->FrameSO->NodeList[3*3+1] - 
                     GSaux->FrameSO->NodeList[0*3+1] );
   /* And something for the Z */
   V[2] = GSaux->FrameSO->NodeList[0*3+2];
   
   /* Now go to pixels */
   if (!SUMA_World2ScreenCoordsF(sv, 4, GSaux->FrameSO->NodeList, S, 
                                 NULL, YUP, YUP)) {
      SUMA_S_Err("Failed to project?");
   }
   Sz[0] = SUMA_ABS(S[1*3+0] - S[0*3+0]); 
   Sz[1] = SUMA_ABS(S[3*3+1] - S[0*3+1]); 
   Sz[2] = SUMA_ABS(S[0*3+2]);
   
   Szc[0] = Sz[0]/(float)N[1]; /* width, horizontal */
   Szc[1] = Sz[1]/(float)N[0]; /* height */
   
   SUMA_LHv("Width  of %f mm --> %f pixels, %f pixels/Cell, \n"
            "Height of %f mm --> %f pixels, %f pixels/Cell\n",
            V[0], Sz[0], Szc[0], V[1], Sz[1], Szc[1]);
   
   SUMA_RETURN(1); 
}

/* Return a pointer copy of a graph dset's node coordinates,
   *ind is a copy of the node index pointer */
float *SUMA_GDSET_NodeList(SUMA_DSET *dset, int *N_Node, int recompute, 
                           int **ind, char *this_variant) 
{
   static char FuncName[]={"SUMA_GDSET_NodeList"};
   NI_element *nel=NULL, *nelxyz=NULL;
   float *NodeList=NULL, *X=NULL, *Y=NULL, *Z=NULL;
   int ii, ii3, iicoord;
    char *cs = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (N_Node) *N_Node = -1;
   if (!this_variant || this_variant[0]=='\0') this_variant = "GMATRIX";
   if (!SUMA_IS_REAL_VARIANT(this_variant)) SUMA_RETURN(NULL);
      
   /* Check the drawing variant */
   if (!strcmp(this_variant,"G3D")) {
      if (!(nelxyz = SUMA_FindGDsetNodeListElement(dset))) {
         SUMA_S_Errv("Failed to find Dset %s's NodeListElement\n", 
                           SDSET_LABEL(dset));
         SUMA_RETURN(NULL);
      }
      if (!(cs = NI_get_attribute(nelxyz,"COLMS_LABS"))) {
         SUMA_S_Err("What can I say?");
         SUMA_RETURN(NULL);
      }
      if (ind) {
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, 
                                                "Gnode Index"))<0) {
            SUMA_LH("Failed to find I, assuming we have a full list"); 
         } else {
            *ind = (int *)nelxyz->vec[iicoord]; 
         }
      }
      if (!(nel = SUMA_FindNgrNamedElement(dset->ngr, "disp_NodeList"))) {
         SUMA_LHv("Need new disp_NodeList element, %ld floats long\n", 
                  GDSET_MAX_POINTS(dset));
         if (!(nel = NI_new_data_element("disp_NodeList",
                                          3*GDSET_MAX_POINTS(dset)))) {
            SUMA_S_Err("Failed to create disp_NodeList");
            SUMA_RETURN(NULL);
         }
         NI_add_to_group(dset->ngr, nel);
         NI_add_column( nel, NI_FLOAT, NULL);
         recompute = 1;
      }
      if (N_Node) *N_Node = GDSET_MAX_POINTS(dset);
      if (recompute) {
         SUMA_LHv("Recomputing XYZ of %ld nodes\n", GDSET_MAX_POINTS(dset));

         /* Fill the node list */
         NodeList = (float *)nel->vec[0]; 
         
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode X"))<0) {
            SUMA_S_Err("Failed to find X"); 
            SUMA_RETURN(NULL);
         }
         X = (float *)nelxyz->vec[iicoord]; 
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode Y"))<0){
            SUMA_S_Err("Failed to find Y"); 
            SUMA_RETURN(NULL);
         }
         Y = (float *)nelxyz->vec[iicoord]; 
         if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, "Gnode Z"))<0){
            SUMA_S_Err("Failed to find Z"); 
            SUMA_RETURN(NULL);
         }
         Z = (float *)nelxyz->vec[iicoord];
         for (ii=0, ii3=0; ii< GDSET_MAX_POINTS(dset); ++ii) {
            NodeList[ii3++] = X[ii]; NodeList[ii3++] = Y[ii];  
            NodeList[ii3++] = Z[ii];  
         }
      } else {
         SUMA_LH("Reusing XYZ from disp_NodeList");
         NodeList = (float *)nel->vec[0]; 
      }
      SUMA_RETURN(NodeList);                     
   } else if (!strcmp(this_variant,"GMATRIX")) {
      SUMA_LH("Nothing to return for GMATRIX, potentially one could return"
              "the NodeList from the G3D version ...");
      if (ind) *ind=NULL;
      SUMA_RETURN(NodeList); 
   } else if (!strcmp(this_variant,"GRELIEF")) {   
      SUMA_S_Err("Not ready yet for GMATRIX");
      if (ind) *ind=NULL;
      SUMA_RETURN(NodeList); 
   } else {
      SUMA_S_Errv("Bad draw variant >%s< for %s\n", 
                  this_variant, SDSET_LABEL(dset));
      SUMA_DUMP_TRACE("Bad draw variant");
      if (ind) *ind=NULL;
      SUMA_RETURN(NULL); 
   }
   SUMA_RETURN(NULL);
}

void SUMA_free_colid_offset_datum (void *vv) 
{
   SUMA_COLID_OFFSET_DATUM *cod = (SUMA_COLID_OFFSET_DATUM *)vv;
   if (cod) {
      SUMA_ifree(cod->Label);
      SUMA_ifree(cod->idcode_str);
      SUMA_ifree(cod->ref_idcode_str);
      SUMA_ifree(cod->variant);
      SUMA_ifree(cod->primitive);
      SUMA_free(cod);
   }
}

/*!
   Remove an entry from colid list. Usually you do this when a 
   particular DO with entries here gets chopped.
   
   At some point you will need to deal with the problem of segmentation
   in the colid map as you repeatedly delete intermediate chunks. 
   
   You could flush the whole thing with:
      dlist_destroy(csv->pick_colid_list); csv->pick_colid_list = NULL;
   and regenerate as needed a more solid  block, but for that you need to also 
   wipe out the DOs for wich the colorids are being destroyed. 
   Or simply, you need to mark those DOs as needing to be fully update/redrawn 
   with new colids. Not a big deal... 
*/
SUMA_Boolean SUMA_Remove_From_Pick_Colid_list(SUMA_SurfaceViewer *svu, 
                                              char *idcode_str)
{
   static char FuncName[]={"SUMA_Remove_From_Pick_Colid_list"};
   SUMA_COLID_OFFSET_DATUM *cod=NULL;
   DListElmt *el=NULL, *eld=NULL;
   SUMA_SurfaceViewer *sv;
   int isv, isv0, isv1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!svu) { isv0=0; isv1=SUMAg_N_SVv; }
   else { isv0 = SUMA_WhichSV(svu, SUMAg_SVv, SUMAg_N_SVv);  isv1 = isv0+1;}

   if (isv0 < 0) {
      SUMA_S_Err("Failed to identify sv");
      SUMA_RETURN(NOPE);
   }
   
   for (isv = isv0; isv < isv1; ++isv) {
      sv = &(SUMAg_SVv[isv]);
      if (!sv || !sv->pick_colid_list || !idcode_str 
              || !dlist_size(sv->pick_colid_list) ) continue;

      do {
         if (!el) el = dlist_head(sv->pick_colid_list);
         else el = dlist_next(el);
         eld = NULL;
         if ((cod = (SUMA_COLID_OFFSET_DATUM *)el->data)) {
            if (!strcmp(cod->idcode_str, idcode_str)) {
               eld = el; /* mark for removal */
               /* backup el */
               if (el != dlist_head(sv->pick_colid_list)) el = dlist_prev(el);
               else el = NULL; /* nothing left */
               SUMA_LHv("Removing colid for %s, %s\n", 
                        cod->Label, cod->primitive);
               dlist_remove(sv->pick_colid_list, eld, (void **)&cod); eld = NULL;
               SUMA_free_colid_offset_datum(cod); cod = NULL;
            }   
         }
      } while (dlist_size(sv->pick_colid_list) && 
               el && el != dlist_tail(sv->pick_colid_list));
   }
   
   SUMA_RETURN(YUP);
}

DListElmt * SUMA_Find_In_Pick_Colid_list(SUMA_SurfaceViewer *sv, 
                                         char *idcode_str, char *primitive)
{
   static char FuncName[]={"SUMA_Find_In_Pick_Colid_list"};
   DListElmt *el=NULL;
   SUMA_COLID_OFFSET_DATUM *cod;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv || !idcode_str || !primitive) { /* INSIST on all three
           otherwise you will have  partial matches, and returning
           just one can cause trouble in  calling functions */
      SUMA_S_Err("NULL input");          
      SUMA_RETURN(NULL);
   }                                           
   
   SUMA_LHv("Seeking %s and %s in pick_colid_list\n", idcode_str, primitive);
   if (!sv->pick_colid_list || !dlist_size(sv->pick_colid_list)) 
      SUMA_RETURN(NULL);
   
   do {
      if (!el) el = dlist_head(sv->pick_colid_list);
      else el = dlist_next(el);
      if ((cod = (SUMA_COLID_OFFSET_DATUM *)el->data)) {
         if (!strcmp(cod->idcode_str, idcode_str) &&
             !strcmp(cod->primitive, primitive)) {
            SUMA_RETURN(el);
         }
      }
   } while (el != dlist_tail(sv->pick_colid_list));
   
   SUMA_RETURN(NULL);
}


/* This function does not check for uniqueness of 
   object for which new colid is requested.
   Not sure if that is necessary or not at the moment.
*/
GLubyte *SUMA_New_colid(SUMA_SurfaceViewer *sv, 
                char *Label, char *idcode_str, char *primitive, char *variant,
                char *reference_idcode_str, SUMA_DO_Types ref_do_type,
                int N_n)
{
   static char FuncName[]={"SUMA_New_colid"};
   GLubyte *colid=NULL;
   long int r, g, b, a, n4, n4l;
   SUMA_COLID_OFFSET_DATUM *cod, *codlt=NULL;
   DListElmt *elt=NULL;
   DListElmt *elf=NULL;
   int nwarn=0; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv || !Label || !reference_idcode_str || N_n <=0 || !primitive) {
      SUMA_S_Errv("Don't be lazy %p %p %p %d %p\n", 
                  sv, Label, reference_idcode_str, N_n, primitive);
      SUMA_RETURN(colid);
   }
   if (!sv->pick_colid_list) {
      SUMA_S_Err("Pick_list is not initialized");
      SUMA_RETURN(colid);
   }  
   if (!sv->DO_PickMode) { /* nothing to do if not in pick mode */
      SUMA_RETURN(NULL);
   }
   
   if (!(colid = (GLubyte *)SUMA_calloc(4*N_n, 
                                       sizeof(GLubyte)))) {
      SUMA_S_Errv("Failed to allocate for 4x%d colid entries\n",N_n);
      SUMA_RETURN(NULL);                                    
   }
   
   /* Do we have this thing in already ? */
   cod = NULL;
   if ((elf = SUMA_Find_In_Pick_Colid_list(sv, idcode_str, primitive))) {
      cod = (SUMA_COLID_OFFSET_DATUM *)elf->data;
      
      if (cod->i1-cod->i0+1 == N_n && 
          !strcmp(cod->ref_idcode_str, reference_idcode_str) &&
          cod->ref_do_type == ref_do_type) { /* keep the faith */
          
      } else { /* Remove from list and recreate below */
         dlist_remove(sv->pick_colid_list, elf, (void **)&(cod));
         SUMA_free_colid_offset_datum(cod); cod = NULL;
      }
   }
    
   if (!cod) { /* need new one */
      cod = (SUMA_COLID_OFFSET_DATUM *)SUMA_malloc(
                           sizeof(SUMA_COLID_OFFSET_DATUM));
      cod->Label = SUMA_copy_string(Label);
      cod->idcode_str = SUMA_copy_string(idcode_str);
      cod->primitive = SUMA_copy_string(primitive);
      cod->variant = SUMA_copy_string(variant);
      cod->ref_idcode_str = SUMA_copy_string(reference_idcode_str);
      cod->ref_do_type = ref_do_type;
      if (dlist_size(sv->pick_colid_list) && 
         (elt = dlist_tail(sv->pick_colid_list))) {
         codlt = (SUMA_COLID_OFFSET_DATUM *)(elt->data);
         cod->i0 = codlt->i1+1;
      } else {
         cod->i0 = 100;   /* reserve 0 0 0 0 (== 0) for background, 
                            starting at 250 to help with debugging
                            start at 1 when comfortable with this */
      }
      cod->i1 = cod->i0+N_n-1;
      dlist_ins_next(sv->pick_colid_list, dlist_tail(sv->pick_colid_list), cod);
   }
   
   
   /* now fill colid, see Vox1D2Vox3D*/
   SUMA_COLID_N2RGBA(cod->i0,r,g,b,a);
   colid[0] = r; colid[1] = g; colid[2] = b; colid[3] = a;   
   n4=4; n4l=4*(cod->i1-cod->i0+1);
   while (n4<n4l) {
      /* fprintf(stderr,"%ld/%d: ",n4/4,N_n); */
      if (r<255) {
         ++r;
      } else {
         r = 0; 
         if (g < 255) {
            ++g;
         } else {
            g = 0;
            if (b<255) {
               ++b;
            } else {
               b=0;
               if (a < 255) {
                  ++a;
               } else {
                  if (!nwarn) {
                     SUMA_S_Err("Exceeding 32 bit color id. Clobbering");
                     ++nwarn;
                  }
               }
            } 
         }
      }
      colid[n4++]=r; 
      colid[n4++]=g; 
      colid[n4++]=b; 
      colid[n4++]=a; 
      /* fprintf(stderr,"  %ld %ld %ld %ld\n", r, g, b, a); */
   }
   SUMA_RETURN(colid);
}


/*!
   Return a vector of picking colors for a certain DO
   The reference idcode string is important because
   although all DOs have an idcode string, some of them
   are not persistent and may not present when it comes
   to a selection choice. The reference_idcode_str would
   then be for a permanent object associated with DO
*/
GLubyte *SUMA_DO_get_pick_colid(SUMA_ALL_DO *DO, char *idcode_str, 
                           char *DO_primitive, char *DO_variant,
                           char *ref_idcode_str,SUMA_DO_Types ref_do_type,
                                   SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DO_get_pick_colid"};
   GLubyte *colv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv || !sv->DO_PickMode) {
      SUMA_LH("Not in picking mode, leave in peace");
      SUMA_RETURN(colv);
   }
   if (!ref_idcode_str) {
      SUMA_S_Err("You must give me a reference_idcode_str");
      SUMA_RETURN(colv);
   }
   if (!DO_primitive) DO_primitive = "Not Set";
   
   switch (DO->do_type) {
      case SO_type: { 
         SUMA_S_Warn("I do not intend this picking type for surfaces. Go away");
         SUMA_RETURN(NULL);
         break; }
      case NBLS_type:
      case OLS_type:
      case NBOLS_type:
      case NBV_type:
      case ONBV_type:
      case LS_type: {
         SUMA_SegmentDO *SDO = (SUMA_SegmentDO *)DO;
         SUMA_LHv("Creating ids for %d segments in %s\n",
                  SDO->N_n, SDO->Label); 
         if (!strcmp(DO_primitive,"segments")) {
            if (!(colv = SUMA_New_colid(sv, SDO->Label, SDO->idcode_str,
                                 DO_primitive, DO_variant,
                                 ref_idcode_str, ref_do_type, SDO->N_n))) {
               SUMA_S_Errv("Failed to get colid for %s\n",
                           SDO->Label);
               SUMA_RETURN(NULL);
            }
            SUMA_RETURN(colv);
         } else  if (!strcmp(DO_primitive,"balls")) {
             if (SDO->N_UnqNodes < 0) {
               if (SDO->N_UnqNodes == -1) {
                  SUMA_S_Err("Looks like N_UnqNodes was not initialized.\n"
                        "I can do it here with SUMA_Set_N_UnqNodes_SegmentDO()\n"
                        "But for now I prefer to complain and return NULL");
                  SUMA_RETURN(NULL);
               } else {
                  SUMA_LHv("Have SDO->N_UnqNodes = %d, nothing to do here\n", 
                           SDO->N_UnqNodes);
                  SUMA_RETURN(NULL);
               }
             }
             if (!(colv = SUMA_New_colid(sv, SDO->Label, SDO->idcode_str,
                                 DO_primitive, DO_variant, ref_idcode_str, 
                                 ref_do_type, SDO->N_UnqNodes))) {
               SUMA_S_Errv("Failed to get colid for %s\n",
                           SDO->Label);
               SUMA_RETURN(NULL);
            }
            SUMA_RETURN(colv);
         } else  if (!strcmp(DO_primitive,"seg_balls")) { 
                              /* For generic segments, not those of graph DOs! */
             if (!(colv = SUMA_New_colid(sv, SDO->Label, SDO->idcode_str,
                                 DO_primitive, DO_variant, ref_idcode_str, 
                                 ref_do_type, SDO->N_n))) {
               SUMA_S_Errv("Failed to get colid for %s\n",
                           SDO->Label);
               SUMA_RETURN(NULL);
            }
            SUMA_RETURN(colv);
         } else {
            SUMA_S_Errv("Bad primitive %s for type %d, (%s)\n",
                           DO_primitive, DO->do_type,
                           SUMA_ObjectTypeCode2ObjectTypeName (DO->do_type));
            SUMA_RETURN(colv); 
         }
         break; }  
      case SDSET_type: {
         SUMA_S_Warn("I do not intend this picking type for dsets. Go away");
         SUMA_RETURN(NULL);
         break; }
      default:
         SUMA_S_Errv("Not supported for types %d (%s)\n", 
                     DO->do_type,
                     SUMA_ObjectTypeCode2ObjectTypeName (DO->do_type));
         SUMA_RETURN(NULL);
         
   }     

   SUMA_S_Warn("Take care of the cleanup after you get back!!!!");
   SUMA_RETURN(colv);
}

SUMA_Boolean SUMA_DrawSegmentDO (SUMA_SegmentDO *SDO, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawSegmentDO"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int i, N_n3, i3, n3, n, n1=0, n13=0, ncross=-1, ndraw=-1;
   long int n4;
   int gllst=0, gllsm=0;
   byte *msk=NULL;
   float origwidth=0.0, rad = 0.0, gain = 1.0;
   GLboolean ble=FALSE, dmsk=TRUE, gl_dt=TRUE;
   byte *mask=NULL;
   GLubyte *colid=NULL, *colidballs=NULL;
   SUMA_SurfaceObject *SO1 = NULL;
   SUMA_DSET *dset=NULL;
   SUMA_DUMB_DO DDO;
   DO_PICK_VARS;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SDO || !sv) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (SDO->NodeBased) { /* Locate the surface in question */
      SUMA_LH("Node-based vectors");
      if (!SDO->Parent_idcode_str) {
         SUMA_SL_Err("Object's parent idcode_str not specified.");
         SUMA_RETURN (NOPE);
      }
      DDO.err = 1; /* not set */
      if ((SO1 = SUMA_findSOp_inDOv(SDO->Parent_idcode_str, 
                                    SUMAg_DOv, SUMAg_N_DOv))) {
         SUMA_Load_Dumb_DO((SUMA_ALL_DO *)SO1, &DDO);
         /* masking? */
         if ((ndraw = SUMA_ProcessDODrawMask(sv, SO1, &mask, &ncross)) < 0) {
            SUMA_RETURN (NOPE);
         }  
         if (!ndraw) SUMA_RETURN(YUP);/* nothing to draw, nothing wrong */
         SUMA_LHv("ncross=%d\n", ncross);
      } else if ((dset = SUMA_find_GLDO_Dset(
                  (SUMA_GraphLinkDO *)SUMA_whichADOg(SDO->Parent_idcode_str)))) {
         SUMA_Load_Dumb_DO(SUMA_whichADOg(SDO->Parent_idcode_str), &DDO);
      }
      if (DDO.err) {
         if (DDO.err==1) {
            SUMA_SL_Err("Object's parent surface/graph set not found.");
         } else if (DDO.err==2) {
            SUMA_SL_Err("Could not fill DDO");
         } else {
            SUMA_SL_Err("Weird error.");
         } 
         SUMA_RETURN (NOPE);
      }
      if (!DDO.NodeList) {
         SUMA_S_Err("SDO is node based but could not get a NodeList");
         SUMA_RETURN (NOPE);
      }
      SUMA_LHv("Got DDO.err=%d, N_Node=%d, NodeList=[%f %f %f...], "
               "NodeIndex=[%d...]\n",
               DDO.err, DDO.N_Node,
                        DDO.N_Node ? DDO.NodeList[0]:0.0, 
                        DDO.N_Node ? DDO.NodeList[1]:0.0,  
                        DDO.N_Node ? DDO.NodeList[2]:0.0,
                        DDO.NodeIndex ? DDO.NodeIndex[0]:-1);
   } else {
      SUMA_LH("Segments ");
   }
   
   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   if (!SDO->thickv || SDO->NodeBased) glLineWidth(SDO->LineWidth);  

   if (!sv->DO_PickMode) { 
      switch (SDO->Stipple) {
         case SUMA_DASHED_LINE:
            if (!(gllst = glIsEnabled(GL_LINE_STIPPLE))) 
                                 glEnable(GL_LINE_STIPPLE);
            if (!SDO->stipv) {
               glLineStipple (SDO->Mstip, SDO->stip); 
            }
            break;
         case SUMA_SOLID_LINE:
            if (!(gllsm = glIsEnabled(GL_LINE_SMOOTH))) glEnable(GL_LINE_SMOOTH);
            if (0) glDepthMask(FALSE); /* Disabling depth masking makes lines 
                              coplanar with polygons
                              render without stitching, bleeding, or Z fighting.
                              Problem is, that it need to be turned on for proper
                              rendering of remaing objects, and that brings the 
                              artifact back. */
            glHint (GL_LINE_SMOOTH_HINT, GL_NICEST); 
            break;
         default:
            SUMA_S_Errv("Unrecognized Stipple option %d\n", SDO->Stipple);
            if (mask) SUMA_free(mask); SUMA_RETURN(NOPE);
      }
   } else {
      if (!(colid = SUMA_DO_get_pick_colid((SUMA_ALL_DO *)SDO, SDO->idcode_str,
                           "segments", SDO->DrawnDO_variant,
                           SDO->Parent_idcode_str, SDO->Parent_do_type,
                           sv) )) {
         SUMA_S_Err("Failed to create colid for picking.");
         SUMA_RETURN(NOPE);
      }
      /* in pick mode, we enable little */
      DO_PICK_DISABLES;
   }
   
   if (SDO->NodeBased == 0) {
      if (SDO->thickv || SDO->stipv) {/* slow slow slow */
         SUMA_LH("Drawing xyz to xyz with thickness ");
         if (!sv->DO_PickMode) {
            if (!SDO->colv) glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol);
               /* turn off ambient and diffuse components */
            glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
            glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
         }
         i = 0; n4=0;
         N_n3 = 3*SDO->N_n;
         while (i < N_n3) {
            if (SDO->thickv) glLineWidth(SDO->thickv[i/3]);   
            if (!sv->DO_PickMode && SDO->stipv) 
               glLineStipple (SDO->Mstip, SDO->stipv[i/3]); 
            glBegin(GL_LINES);
            if (colid) {
               n4 = 4*i; /* Always keep indexing tied to arrays of objects */
               glColor4ub(colid[n4], colid[n4+1], colid[n4+2], colid[n4+3]);
            } else if (SDO->colv) 
               glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(i/3)]));
            glVertex3f(SDO->n0[i], SDO->n0[i+1], SDO->n0[i+2]);
            glVertex3f(SDO->n1[i], SDO->n1[i+1], SDO->n1[i+2]); 
            i += 3;
            glEnd();
         }
      } else {
         SUMA_LH("Drawing xyz to xyz ");
         glBegin(GL_LINES);
         if (!sv->DO_PickMode) {
            if (!SDO->colv) 
               glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol); 
                     /*turn on emissivity  */
            glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
                     /* turn off ambient and diffuse components */
            glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
         }
         if (!SDO->NodeBased) {
            i = 0; n4=0;
            N_n3 = 3*SDO->N_n;
            while (i < N_n3) {
               if (colid) {
                  n4 = 4*i; /* Always keep indexing tied to arrays of objects */
                  glColor4ub(colid[n4], colid[n4+1], colid[n4+2], colid[n4+3]);
               } else if (SDO->colv) 
                  glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(i/3)]));
               glVertex3f(SDO->n0[i], SDO->n0[i+1], SDO->n0[i+2]);
               glVertex3f(SDO->n1[i], SDO->n1[i+1], SDO->n1[i+2]); 
               i += 3;
            }
         } 
         glEnd();
      }   
   } else if (  SDO->NodeBased == 1 ) {
      glBegin(GL_LINES);
      if (!sv->DO_PickMode) {
         if (!SDO->colv) 
            glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol); 
                  /*turn on emissivity  */
         glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
                  /* turn off ambient and diffuse components */
         glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);      
      }
      SUMA_LH("Drawing vectors ");
      i = 0; n4=0;
      gain = 1.0;
      while (i < SDO->N_n) {
         n = SDO->NodeID[i]; n3 = 3*n;
         if (LocalHead) 
            fprintf(SUMA_STDERR,"%s: %d/%d, %d\n", FuncName, i, SDO->N_n, n);
         if (n<DDO.N_Node && DO_DRAW(mask,n,ncross)) {
            i3 = 3*i;
            if (SDO->thickv) {
               gain = SDO->thickv[i]; 
            } else {
               /* gain = 1.0; no need, set above */  
            }
            if (colid) {
               n4 = 4*i; /* Always keep indexing tied to arrays of objects */
               glColor4ub(colid[n4], colid[n4+1], colid[n4+2], colid[n4+3]);
            } else if (SDO->colv) 
               glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(i)]));
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: SDO->n1 = [%f %f %f]\n", 
                     FuncName, SDO->n1[i3  ], SDO->n1[i3+1],SDO->n1[i3+2]); 
            /* SPARSE LIST NOTICE:
            This coordinate lookup assumes you have a full node
            list. DDO.NodeIndex is ignored. If you start using DDO.NodeIndex,
            change n<DDO.N_Node condition above since a node index can exceed
            the number of nodes in a sparse list */
            glVertex3f( DDO.NodeList[n3], DDO.NodeList[n3+1], 
                        DDO.NodeList[n3+2]);
            glVertex3f( DDO.NodeList[n3]  +(gain * SDO->n1[i3  ]), 
                        DDO.NodeList[n3+1]+(gain * SDO->n1[i3+1]), 
                        DDO.NodeList[n3+2]+(gain * SDO->n1[i3+2])); 
         }
         i += 1;
      }
      glEnd();
   } else if (  SDO->NodeBased == 2 ) {
      if (!SDO->thickv) {
         glBegin(GL_LINES);
         if (!sv->DO_PickMode) {
            if (!SDO->colv) 
               glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol); 
                     /*turn on emissivity  */
            glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
                     /* turn off ambient and diffuse components */
            glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);      
         }
         SUMA_LH("two-node vectors ");
         i = 0;
         gain = 1.0; n4=0;
         while (i < SDO->N_n) {
            n = SDO->NodeID[i]; n3 = 3*n; 
            n1 = SDO->NodeID1[i]; n13 = 3*n1;
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: %d/%d, %d,%d\n", 
                        FuncName, i, SDO->N_n, n, n1);
            if (n<DDO.N_Node && n1 < DDO.N_Node && DO_DRAW(mask,n,ncross)) {
               i3 = 3*i;
               
               if (colid){
                  n4 = 4*i; /* Always keep indexing tied to arrays of objects */
                  glColor4ub(colid[n4], colid[n4+1], colid[n4+2], colid[n4+3]);
                  if (LocalHead) 
               fprintf(SUMA_STDERR,"%s:         %d %d %d %d\n", 
                        FuncName, colid[n4], colid[n4+1],
                                  colid[n4+2], colid[n4+3]);
               } else if (SDO->colv) 
                  glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(i)]));
               /* SEE "SPARSE LIST NOTICE" above */
               glVertex3f( DDO.NodeList[n3], DDO.NodeList[n3+1], 
                           DDO.NodeList[n3+2]);
               glVertex3f( DDO.NodeList[n13  ], 
                           DDO.NodeList[n13+1], 
                           DDO.NodeList[n13+2]); 
            }
            i += 1;
         }
         glEnd();
      } else {/* slow slow slow */
         if (!sv->DO_PickMode) {
            if (!SDO->colv) glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol);
            glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
               /* turn off ambient and diffuse components */
            glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
         }
         SUMA_LH("two-node vectors, with thickness ");
         i = 0; n4=0;
         while (i < SDO->N_n) {
            n = SDO->NodeID[i]; n3 = 3*n;
            n1 = SDO->NodeID1[i]; n13 = 3*n1;
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: %d/%d, %d,%d\n", 
                        FuncName, i, SDO->N_n, n, n1);
            if (n<DDO.N_Node && n1 < DDO.N_Node && DO_DRAW(mask,n,ncross)) {
               i3 = 3*i;
               if (SDO->thickv) glLineWidth(SDO->thickv[i]); 
               glBegin(GL_LINES);
               if (colid){
                  n4 = 4*i; /* Always keep indexing tied to arrays of objects */
                  glColor4ub(colid[n4], colid[n4+1], colid[n4+2], colid[n4+3]);
               } else if (SDO->colv) 
                  glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(i)]));
               
               /* SEE "SPARSE LIST NOTICE" above */
               glVertex3f( DDO.NodeList[n3], DDO.NodeList[n3+1], 
                           DDO.NodeList[n3+2]);
               glVertex3f( DDO.NodeList[n13  ], 
                           DDO.NodeList[n13+1], 
                           DDO.NodeList[n13+2]); 
               glEnd();
            }
            i += 1;
         }
      }
   } else {
      SUMA_S_Err("Oh no. Bad logic");
      goto GETOUT;
   }
      
   if (!sv->DO_PickMode) {
      switch (SDO->Stipple) {
         case SUMA_DASHED_LINE:
            if (!gllst) glDisable(GL_LINE_STIPPLE);
            break;
         case SUMA_SOLID_LINE:
            if (!gllsm) glDisable(GL_LINE_SMOOTH);
            break;
      }
   }
   
   /* draw the bottom object */
   if (SDO->botobj) {
      SUMA_LH("Drawing bottom");
      glLineWidth(0.5);
      if (!SDO->colv) {
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, SDO->LineCol);
      }
      
      if (!SDO->NodeBased) {
         if (!SDO->thickv) 
            rad = SDO->LineWidth*0.5*sv->FOV[sv->iState]/FOV_INITIAL;
      } else {
         if (DDO.AvgLe != 0.0f) rad = DDO.AvgLe/4.0;
         else rad = 1.0/4.0;
      }
      gluQuadricDrawStyle (SDO->botobj, GLU_FILL); 
      gluQuadricNormals (SDO->botobj , GLU_SMOOTH);

      if (!SDO->NodeBased) {
         n4=0;
         for (i=0; i<SDO->N_n;++i) {
            i3 = 3*i;
            if (colid) {
               n4 = 4*i; /* Always keep indexing tied to arrays of objects */
               glColor4ub(colid[n4], colid[n4+1], colid[n4+2], colid[n4+3]);
            } else {
               if (SDO->colv) {
                  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                 &(SDO->colv[i*4]));
                  glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[i*4]));
               }
            }
            if (SDO->thickv) 
               rad = SDO->thickv[i]*0.5*sv->FOV[sv->iState]/FOV_INITIAL;
            /* fprintf(SUMA_STDERR,
                      "thickv[i] = %f, FOV %f, "
                      "FOV_INITIAL %f, radinit=%f, radcomp=%f\n", 
                        SDO->thickv[i], sv->FOV[sv->iState], FOV_INITIAL,
                        rad,  rad *SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06)); */
            glTranslatef (SDO->n0[i3], SDO->n0[i3+1], SDO->n0[i3+2]);
            gluSphere(SDO->botobj, SUMA_MAX_PAIR(rad, 0.005) 
                           /* *SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06) */ , 
                      10, 10);
            glTranslatef (-SDO->n0[i3], -SDO->n0[i3+1], -SDO->n0[i3+2]);
         }
      } else { /* Node based vector */
         /* create a mask for those spheres already drawn, 
            multiple vectors per node possible...*/
         msk = (byte *)SUMA_calloc(DDO.N_Node, sizeof(byte));
         if (!msk) { 
            SUMA_SL_Crit("Failed to allocate!\n");
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
            goto GETOUT;
         }
         if (sv->DO_PickMode) {
            if (!(colidballs = SUMA_DO_get_pick_colid(
                                 (SUMA_ALL_DO *)SDO, SDO->idcode_str,
                                 "seg_balls", SDO->DrawnDO_variant,
                                 SDO->Parent_idcode_str, SDO->Parent_do_type,
                                 sv) )) {
               SUMA_S_Err("Failed to create colid for ball picking.");
            }
         }
         n4=0;
         for (i=0; i<SDO->N_n;++i) {
            i3 = 3*i;
            n = SDO->NodeID[i]; n3 = 3*n;
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: %d/%d, %d\n", FuncName, i, SDO->N_n, n);
            if (n<DDO.N_Node && DO_DRAW(mask,n,ncross)) {
               if (!msk[n]) {
                  if (colidballs) {
                     n4 = 4*i; /* Keep indexing tied to arrays of objects */
                     glColor4ub( colidballs[n4  ], colidballs[n4+1], 
                                 colidballs[n4+2], colidballs[n4+3]);
                  } else   if (SDO->colv) {
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                    &(SDO->colv[i*4]));
                     glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[i*4]));
                  }
                  /* SEE "SPARSE LIST NOTICE" above */
                  glTranslatef ( DDO.NodeList[n3]  , 
                                 DDO.NodeList[n3+1]  , DDO.NodeList[n3+2]  );
                  gluSphere(SDO->botobj, SUMA_MAX_PAIR(rad, 0.005) 
                              /* *SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06) */ , 
                              10, 10);
                  glTranslatef (-DDO.NodeList[n3]  , 
                                -DDO.NodeList[n3+1]  , -DDO.NodeList[n3+2]  );
                  msk[n] = 1;
               } else {
                  SUMA_LH("Sphere already drawn");
               }
            }
         }
         if (msk) SUMA_free(msk); msk = NULL;
      }
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
   }

   GETOUT:
   if (sv->DO_PickMode) DO_PICK_RESTORE;

   SUMA_ifree(colid); SUMA_ifree(colidballs);
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity */
   glLineWidth(origwidth);
   if (mask) SUMA_free(mask); mask=NULL;
     
   SUMA_RETURN (YUP);
   
}

NI_element *SUMA_GDSET_Edge_Bundle(SUMA_DSET *gset, SUMA_GRAPH_SAUX *GSaux, 
                                   int edge_id, int alt_edge_id) 
{
   static char FuncName[]={"SUMA_GDSET_Edge_Bundle"};
   NI_element *nel=NULL;
   SUMA_NGR_INDEX_HASH_DATUM *hd=NULL;
   
   if (edge_id < 0 || !gset || !GSaux || !GSaux->thd) return(NULL);
   
   /* Get the ngr part number for the nel from the hash table */
   HASH_FIND_INT(GSaux->thd, &edge_id, hd); 
   if (!hd) {
      if (alt_edge_id < 0) return(NULL);
      else {
         HASH_FIND_INT(GSaux->thd, &alt_edge_id, hd);
         if (!hd) return(NULL);
      }
   }
   
   /* Have index, work it */
   if (GSaux->net) return((NI_element *)GSaux->net->part[hd->ngrindex]);
   else return((NI_element *)gset->ngr->part[hd->ngrindex]); /* old style */
   
   return(NULL);
}

#define SUMA_EDGEINDEX_TO_NELNUM(key,thd,hdbuf) { \
   HASH_FIND_INT(thd, &key, hdbuf); \
   if (hdbuf) key = hdbuf->ngrindex; else  key = -1; \
}

#define SUMA_GDSET_TRACK_NEL(dset, GSaux, itp) ( \
         GSaux->net ? (NI_element *)GSaux->net->part[itp]: \
                      (NI_element *)dset->ngr->part[itp] );

#define SUMA_DRAW_GRAPH_EDGE(DDO, cna3, cnb3, nelitp) {              \
   if (!nelitp) { /* good ole fashioned segment */                   \
      glVertex3f( DDO.NodeList[cna3], DDO.NodeList[cna3+1],          \
                  DDO.NodeList[cna3+2]);                             \
      glVertex3f( DDO.NodeList[cnb3  ],                              \
                  DDO.NodeList[cnb3+1],                              \
                  DDO.NodeList[cnb3+2]);                             \
   } else {     /* bundle mode */                                    \
      int nn, mm;                                                    \
      TAYLOR_TRACT *ttn=NULL;                                        \
      for (nn=0; nn<nelitp->vec_len; ++nn) {                         \
         ttn = (TAYLOR_TRACT *)(nelitp->vec[0])+nn;                  \
         mm=0;                                                       \
         while (mm < ttn->N_pts3-3) {                                \
      glVertex3f(ttn->pts[mm  ], ttn->pts[mm+1], ttn->pts[mm+2] );   \
      glVertex3f(ttn->pts[mm+3], ttn->pts[mm+4], ttn->pts[mm+5] );   \
            mm += 3;                                                 \
         }                                                           \
      }                                                              \
   }                                                                 \
}

/* A variant on DrawSegmentDO that is tailored to the drawing needs
of a graph dataset. Eventually, I might create a new DO type, but for
now we piggy back on the good old SegmentDO */ 
SUMA_Boolean SUMA_DrawGSegmentDO (SUMA_GRAPH_SAUX *GSaux, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawGSegmentDO"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   GLfloat  textcolor[4] = {1.0, 1.0, 1.0, 1.0}, 
            textshadcolor[4] = {0.0, 1.0, 1.0, 1.0};
   int i, iii, si, N_n3, i3, i4, cn3, cn, n, cn1=0, n1=0, 
       cn13=0, ncross=-1, ndraw=-1, tw=0, th=0, nl=0, istip=0;
   GLfloat rpos[4], col1[4], col2[4];
   long int n4;
   char **names=NULL;
   GLboolean valid;
   int gllst=0, gllsm=0, OnlyThroughNode = -1;
   byte *bbox=NULL;
   NI_element *nelitp = NULL;
   float origwidth=0.0, radconst = 0.0, rad = 0.0, 
         gain = 1.0, constcol[4], edgeconst=1.0, 
         vmin=1.0, vmax=1.0, Wfac=1.0, Sfac=1.0, cdim=1/3.0;
   GLboolean ble=FALSE, dmsk=TRUE, gl_dt=TRUE;
   byte *mask=NULL, *wmask=NULL, showword = 0;
   GLubyte *colid=NULL, *colidballs=NULL, *colballpick=NULL;
   SUMA_SurfaceObject *SO1 = NULL;
   SUMA_DSET *dset=NULL;
   SUMA_DUMB_DO DDO;
   SUMA_SegmentDO *SDO = NULL;
   DO_PICK_VARS;
   GLfloat selcol[4], Wrange[2];
   SUMA_OVERLAYS *curcol = NULL;
   void *fontGL=NULL;
   int ic0, ic1, s0, r0, s1, r1, TxtShadeMode=1, okind, ri, 
       *dsrt=NULL, *dsrt_ind=NULL, *GNI=NULL, wbox=0;
   int stipsel = 0; /* flag for stippling of selected edge */
   int depthsort = 1; /* Sort text and draw from farthest to closest */
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!GSaux || !sv || !(SDO=GSaux->SDO)) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   gl_dt = glIsEnabled(GL_DEPTH_TEST);
   
   if (stipsel) {
      SUMA_S_Warn("This option is no good at this moment when segment stippling"
                  "\n is turned on for all segments. Easy to fix though ...");
   }
   if (SDO->NodeBased == 2) { /* Locate the surface in question */
      SUMA_LH("Node-based vectors");
      if (!SDO->Parent_idcode_str) {
         SUMA_SL_Err("Object's parent idcode_str not specified.");
         SUMA_RETURN (NOPE);
      }
      
      if (!(dset = SUMA_find_GLDO_Dset(
                  (SUMA_GraphLinkDO *)SUMA_whichADOg(SDO->Parent_idcode_str)))) {
         SUMA_S_Err("Could not find dset for GLDO!");
         SUMA_RETURN (NOPE);
      }
      if (!(curcol = SUMA_ADO_CurColPlane((SUMA_ALL_DO *)dset))) {
         SUMA_S_Err("Could not find current col plane!");
         SUMA_RETURN (NOPE);
      }
      
      fontGL = SUMA_Font2GLFont(curcol->Font);
      SUMA_NodeCol2Col(curcol->NodeCol, constcol);
      
      TxtShadeMode = curcol->TxtShad;
      
      edgeconst = 1 * curcol->EdgeThickGain;

      glGetFloatv(GL_LINE_WIDTH, &origwidth);
      if (curcol->EdgeThick == SW_SurfCont_DsetEdgeThickConst) {
         SDO->LineWidth = 1.0*curcol->EdgeThickGain;
         glLineWidth(SDO->LineWidth);
      }
      
      if (curcol->EdgeThick == SW_SurfCont_DsetEdgeThickVal ||
          curcol->EdgeStip == SW_SurfCont_DsetEdgeStipVal){
         /* compute width / stippling scaling params */
         if (SUMA_ABS(curcol->OptScl->IntRange[0]) < 
             SUMA_ABS(curcol->OptScl->IntRange[1])) {
            vmin = SUMA_ABS(curcol->OptScl->IntRange[0]);
            vmax = SUMA_ABS(curcol->OptScl->IntRange[1]);   
         } else {
            vmin = SUMA_ABS(curcol->OptScl->IntRange[1]);
            vmax = SUMA_ABS(curcol->OptScl->IntRange[0]);   
         }
         if (vmin == vmax) { vmin = 0; vmax = 1.0; }
         /* Will need to fix this should you unalias segment drawing... */
         glGetFloatv(GL_ALIASED_LINE_WIDTH_RANGE, Wrange);
         if (vmax > vmin) {
            if ((Wfac = (Wrange[1]-Wrange[0])/(vmax)) <= 0.0) Wfac = 1.0;
         } else {
            if ((Wfac = (Wrange[1]-Wrange[0])/(vmin)) <= 0.0) Wfac = 1.0;
         }
         /* For stippling */
         if (vmax > vmin) {
            if ((Sfac = 15/(vmax)) <= 0.0) Sfac = 1.0;
         } else {
            if ((Sfac = 15/(vmin)) <= 0.0) Sfac = 1.0;
         }
      }
            
      DDO.err = 1; /* not set */
      SUMA_Load_Dumb_DO(SUMA_whichADOg(SDO->Parent_idcode_str), &DDO);
      if (DDO.err) {
         if (DDO.err==1) {
            SUMA_SL_Err("Object's parent graph set not found.");
         } else if (DDO.err==2) {
            SUMA_SL_Err("Could not fill DDO");
         } else {
            SUMA_SL_Err("Weird error.");
         } 
         SUMA_RETURN (NOPE);
      }
      if (!DDO.NodeList) {
         SUMA_S_Err("SDO is node based but could not get a NodeList");
         SUMA_RETURN (NOPE);
      }
      SUMA_LHv("Got DDO.err=%d, N_Node=%d, NodeList=[%f %f %f...], "
               "NodeIndex=[%d...]\n",
               DDO.err, DDO.N_Node,
                        DDO.N_Node ? DDO.NodeList[0]:0.0, 
                        DDO.N_Node ? DDO.NodeList[1]:0.0,  
                        DDO.N_Node ? DDO.NodeList[2]:0.0,
                        DDO.NodeIndex ? DDO.NodeIndex[0]:-1);
   } else {
      SUMA_S_Err("For graph dsets, Segment DO must be node based on both ends");
      SUMA_RETURN (NOPE);
   }
   
   if (!(names = SUMA_GDSET_GetPointNamesColumn(dset, &iii, NULL))) {
      SUMA_LH("No names!"); /* No need to weep */
   }
   if (!(GNI = SUMA_GDSET_GetPointIndexColumn(dset, &iii, NULL))) {
      if (iii == -2) {
         SUMA_S_Err("Bad news for indices!!");
         SUMA_RETURN(NOPE);
      }
   }

   for (i=0;i<3;++i) {
      textcolor[i] = 1.0 - sv->clear_color[i];
      textshadcolor[i] = sv->clear_color[i];
   }
   if (textcolor[0]+textcolor[1]+textcolor[2] > 1.5) {
      wbox = 0;
      cdim = 1/3.0;
   } else {
      wbox = 1;
      cdim = 3.0;
   }
   SUMA_LHv("Stippling %d (XXX=%d, Val=%d, 01=%d) wbox=%d\n",
                curcol->EdgeStip, SW_SurfCont_DsetEdgeStipXXX, 
                SW_SurfCont_DsetEdgeStipVal, SW_SurfCont_DsetEdgeStip1, wbox);
   if (curcol->EdgeStip == SW_SurfCont_DsetEdgeStipXXX ||
       curcol->EdgeStip < 0) {
      SDO->Stipple = SUMA_SOLID_LINE;
   } else {
      SDO->Stipple = SUMA_DASHED_LINE;
   }
   if (!sv->DO_PickMode) { 
      switch (SDO->Stipple) {
         case SUMA_DASHED_LINE:
            if (!(gllst = glIsEnabled(GL_LINE_STIPPLE))) 
                                 glEnable(GL_LINE_STIPPLE);
            if (curcol->EdgeStip != SW_SurfCont_DsetEdgeStipVal) {
               glLineStipple (1, SUMA_StippleLineMask_rand(
                          curcol->EdgeStip-SW_SurfCont_DsetEdgeStipVal, 2, 0));
            }
            break;
         case SUMA_SOLID_LINE:
            if (!(gllsm = glIsEnabled(GL_LINE_SMOOTH))) glEnable(GL_LINE_SMOOTH);
            if (0) glDepthMask(FALSE); /* Disabling depth masking makes lines 
                              coplanar with polygons
                              render without stitching, bleeding, or Z fighting.
                              Problem is, that it need to be turned on for proper
                              rendering of remaing objects, and that brings the 
                              artifact back. */
            glHint (GL_LINE_SMOOTH_HINT, GL_NICEST); 
            break;
         default:
            SUMA_S_Errv("Unrecognized Stipple option %d\n", SDO->Stipple);
            if (mask) SUMA_free(mask); SUMA_RETURN(NOPE);
      }
   } else {
      if (!(colid = SUMA_DO_get_pick_colid((SUMA_ALL_DO *)SDO, SDO->idcode_str,
                           "segments", SDO->DrawnDO_variant,
                           SDO->Parent_idcode_str, SDO->Parent_do_type,
                           sv) )) {
         SUMA_S_Err("Failed to create colid for picking.");
         SUMA_RETURN(NOPE);
      }
      /* in pick mode, we enable little */
      DO_PICK_DISABLES;
   }
   
   if (GSaux->PR->datum_index == -1 && GSaux->PR->selectedEnode != -1) {
      OnlyThroughNode = GSaux->PR->selectedEnode;
   } else OnlyThroughNode = -1;
   
   ic0 = -1; ic1=-1; r0 = -1; r1 = -1; s0 = -1; s1 = -1;
   if (!sv->DO_PickMode && (s0=GSaux->PR->datum_index) >=0 &&
       SUMA_SV_GetShowSelectedDatum(sv)) { /* Have selection to highlight, 
                                              Need to avoid it being masked
                                              by one of the edges if depth buffer
                                              is enabled. For this reason we 
                                              will mask all the edges between
                                              the points of the selected edge */
      r0 = SUMA_GDSET_EdgeIndex_To_Row(dset,s0);
      if (GSaux->isColored && 
         GSaux->isColored[s0]) {/* Don't show it if thresholded */
         ic0 = GSaux->isColored[s0];
         GSaux->isColored[s0] = 0;
         n = SDO->NodeID[r0];
         n1 = SDO->NodeID1[r0];
         SUMA_GDSET_PointsToSegRow(dset, n1, n, &r1);/* get the n1-->n edge */
         s1 = SUMA_GDSET_EdgeRow_To_Index(dset, r1);
         ic1 = GSaux->isColored[s1];
         GSaux->isColored[s1] = 0;
            /* Now do the highlight for edge s0, why wait till the end ?*/
         n = SDO->NodeID[r0]; 
         n1 = SDO->NodeID1[r0];
         SUMA_LHv("Highlight: edge row %d/%d edge index %d, [%d,%d] (%d)\n"
                  "Reverse edge: row %d, index %d\n", 
                  r0, SDO->N_n, s0, n, n1, DDO.N_Node,
                  r1, s1);
         
         /* get position of node n in NodeList */
         cn  = SUMA_NodeIndex_To_Index(DDO.NodeIndex, DDO.N_Node, n); 
            cn3 = 3*cn;
         cn1 = SUMA_NodeIndex_To_Index(DDO.NodeIndex, DDO.N_Node, n1); 
            cn13 = 3*cn1;
         if (cn<DDO.N_Node && cn1 < DDO.N_Node) {
            if (stipsel) {
               if(!(gllst = glIsEnabled(GL_LINE_STIPPLE)))
                                    glEnable(GL_LINE_STIPPLE);
               glLineStipple (1, SUMA_int_to_stipplemask(stipsel-1)); 
            }
            if (curcol->EdgeThick == SW_SurfCont_DsetEdgeThickVal) 
                  glLineWidth(((SUMA_ABS(curcol->V[r0]))*Wfac+Wrange[0])
                                                         *curcol->EdgeThickGain); 
            selcol[0] = 1-sv->clear_color[0];
            selcol[1] = 1-sv->clear_color[1];
            selcol[2] = 1-sv->clear_color[2];
            selcol[3] = 1-sv->clear_color[3]; 
            nelitp = SUMA_GDSET_Edge_Bundle(dset, GSaux, s0, s1);
            SUMA_LHv("Col:[%f %f %f %f]\n",
               selcol[0], selcol[1], selcol[2], selcol[3]);
            
            glBegin(GL_LINES);
            glMaterialfv(GL_FRONT, GL_EMISSION, selcol);
            
            SUMA_DRAW_GRAPH_EDGE(DDO, cn3, cn13, nelitp);
            
            glEnd();
            if (stipsel && !gllst) glDisable(GL_LINE_STIPPLE);
         }
      }
   }                                          
                                              
   if (  SDO->NodeBased == 2 ) {
      if (curcol->EdgeThick != SW_SurfCont_DsetEdgeThickVal &&
          curcol->EdgeStip != SW_SurfCont_DsetEdgeStipVal) {
         glBegin(GL_LINES);
         if (!sv->DO_PickMode) {
            if (!SDO->colv) 
               glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol); 
                     /*turn on emissivity  */
            glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
                     /* turn off ambient and diffuse components */
            glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);      
         }
         SUMA_LHv("two-node vectors\n"
                  "    DDO.NodeIndex %p, DDO.N_Node %d\n"
                  "    GSaux->isColored %p, OnlyThroughNode=%d\n",
                  DDO.NodeIndex, DDO.N_Node, GSaux->isColored,
                  OnlyThroughNode);
         i = 0;
         gain = 1.0; n4=0;
         while (i < SDO->N_n) {
            n = SDO->NodeID[i];  
            n1 = SDO->NodeID1[i];
            si = SUMA_GDSET_EdgeRow_To_Index(dset,i);
            
            /* Do we have a tract for this monster ? */
            nelitp = SUMA_GDSET_Edge_Bundle(dset, GSaux, si, -1);
            
            if (LocalHead) { 
               fprintf(SUMA_STDERR,
                    "%s: segment row %d/%d, index %d, nodes [%d,%d], MASK %d\n",
                        FuncName, i, SDO->N_n, 
                        si, n, n1, IN_MASK(GSaux->isColored,si));
               if (nelitp) {
                  
                  fprintf(SUMA_STDERR,
                     "   Have special tract of segment %d at '%s %s'\n",
                     si, nelitp ? nelitp->name:"nothing", 
                     nelitp ? SUMA_CHECK_NULL_STR(
                              NI_get_attribute(nelitp, "Edge_Index")):"NULL");
               }
               #if 0
               if (i==0) {
                  char *ss;
                  ss=SUMA_ShowMeSome(DDO.NodeIndex, SUMA_int, 
                                     DDO.N_Node, 5, NULL);
                  fprintf(SUMA_STDERR,"DDO.NodeIndex=%s\n", ss);
                  SUMA_ifree(ss);
               }
               #endif
            }
            if (OnlyThroughNode>=0 && 
                  (OnlyThroughNode != n &&  OnlyThroughNode != n1)) {
                     ++i; continue;
            }
            
            /* get position of node n in NodeList */
            cn  = SUMA_NodeIndex_To_Index(DDO.NodeIndex, DDO.N_Node, n); 
               cn3 = 3*cn;
            cn1 = SUMA_NodeIndex_To_Index(DDO.NodeIndex, DDO.N_Node, n1); 
               cn13 = 3*cn1;
            #if 0
               SUMA_LHv("Rows of nodes [%d %d] are [%d %d]\n",
                     n, n1, cn, cn1);
            #endif
            if (cn<DDO.N_Node && cn1 < DDO.N_Node && 
                IN_MASK(GSaux->isColored,si)) {
               i3 = 3*i;
               
               if (colid){
                  n4 = 4*i; /* Always keep indexing tied to arrays of objects */
                  glColor4ub(colid[n4], colid[n4+1], colid[n4+2], colid[n4+3]);
                  if (LocalHead) 
               fprintf(SUMA_STDERR,
                     "%s: colid for segment row %d         %d %d %d %d\n", 
                        FuncName, i, colid[n4], colid[n4+1],
                                  colid[n4+2], colid[n4+3]);
               } else if (SDO->colv) { /* Colv is index by datum index */
                  glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(si)]));
               }
               SUMA_LHv("Seg row %d between [%d,%d]: [%f %f %f --> %f %f %f]\n",
                 i, cn, cn1,
                 DDO.NodeList[cn3   ],DDO.NodeList[cn3 +1],DDO.NodeList[cn3 +2],
                 DDO.NodeList[cn13  ],DDO.NodeList[cn13+1],DDO.NodeList[cn13+2]);
               SUMA_DRAW_GRAPH_EDGE(DDO, cn3, cn13, nelitp);
            } else {
               if (LocalHead) 
                  fprintf(SUMA_STDERR, "masked segment %d?\n", i);
            }
            i += 1;
         }
         glEnd();
      } else {/* slow slow slow, variable stippling, edge thickness, or both*/
         if (!sv->DO_PickMode) {
            if (!SDO->colv) glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol);
            glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
               /* turn off ambient and diffuse components */
            glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
         }
         SUMA_LH("two-node vectors, with thickness ");
         i = 0;
         gain = 1.0; n4=0;
         while (i < SDO->N_n) {
            n = SDO->NodeID[i]; 
            n1 = SDO->NodeID1[i]; 
            si = SUMA_GDSET_EdgeRow_To_Index(dset,i);
            
            /* Tracts are not compatible with variable edge width */
            nelitp = NULL;
            
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: %d/%d, edge index %d [%d,%d]\n", 
                        FuncName, i, SDO->N_n, si, n, n1);
            if (OnlyThroughNode>=0 && 
                  (OnlyThroughNode != n &&  OnlyThroughNode != n1)) {
                     ++i; continue;
            }
            
            /* get position of node n in NodeList */
            cn  = SUMA_NodeIndex_To_Index(DDO.NodeIndex, DDO.N_Node, n); 
               cn3 = 3*cn;
            cn1 = SUMA_NodeIndex_To_Index(DDO.NodeIndex, DDO.N_Node, n1); 
               cn13 = 3*cn1;
            
            if (cn<DDO.N_Node && cn1 < DDO.N_Node && 
                IN_MASK(GSaux->isColored,si)) {
               i3 = 3*i;
               if (curcol->EdgeStip == SW_SurfCont_DsetEdgeStipVal) {
                  istip = 16-(int)((SUMA_ABS(curcol->V[i]))*Sfac);
                  
                  if (istip == 16) --istip; /* Not nice to avoid displaying
                                               min edge ... */
                  glLineStipple (1, SUMA_StippleLineMask_rand(istip, 1, 0)); 
               }
               if (curcol->EdgeThick == SW_SurfCont_DsetEdgeThickVal) {
                  glLineWidth(((SUMA_ABS(curcol->V[i]))*Wfac+Wrange[0])
                                                   *curcol->EdgeThickGain); 
               }
               glBegin(GL_LINES);
               if (colid){
                  n4 = 4*i; /* Always keep indexing tied to arrays of objects */
                  glColor4ub(colid[n4], colid[n4+1], colid[n4+2], colid[n4+3]);
               } else if (SDO->colv) { /* Colv is index by datum index */
                  glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(si)]));
               }
               SUMA_DRAW_GRAPH_EDGE(DDO, cn3, cn13, nelitp);
               
               glEnd();
            }
            i += 1;
         }
      }
   } else {
      SUMA_S_Err("Oh no. Bad logic");
      goto GETOUT;
   }
   
   if (GSaux->isColored) { /* undo the mask disturbance for selection highlight*/
      if (ic0 >= 0) GSaux->isColored[s0] = ic0;
      if (ic1 >= 0) GSaux->isColored[s1] = ic1;                                      }     

   /* Highlight selected Datum, ONLY if we did not go the mask route */
   if (!sv->DO_PickMode && (si=GSaux->PR->datum_index) >=0 &&
       SUMA_SV_GetShowSelectedDatum(sv) && !GSaux->isColored) {
      n = SDO->NodeID[i]; 
      n1 = SDO->NodeID1[i]; 
      SUMA_LHv("Highlight: edge %d/%d, edge index %d [%d,%d] (%d)\n", 
               i, SDO->N_n, si, n, n1, DDO.N_Node);
      /* get position of node n in NodeList */
      cn  = SUMA_NodeIndex_To_Index(DDO.NodeIndex, DDO.N_Node, n); 
         cn3 = 3*cn;
      cn1 = SUMA_NodeIndex_To_Index(DDO.NodeIndex, DDO.N_Node, n1); 
         cn13 = 3*cn1;
      if (cn<DDO.N_Node && cn1 < DDO.N_Node) {
         /* Kill depth test or you could get burried under two way edges 
            There is a problem though in that the edge wil ride over 
            other objects that otherwise would obscure it, so that might
            be ugly. To get around that, one would need to avid drawing
            any edge that is between the two nodes. Not a bad idea I guess,
            and then we can make do without the disabling of the depth test*/
         if (gl_dt) glDisable(GL_DEPTH_TEST);   
         if (stipsel) {
            if (!(gllst = glIsEnabled(GL_LINE_STIPPLE))) 
                                    glEnable(GL_LINE_STIPPLE);
            glLineStipple (1, SUMA_int_to_stipplemask(stipsel-1)); 
         }
         i3 = 3*i;
         if (curcol->EdgeThick == SW_SurfCont_DsetEdgeThickVal) 
                     glLineWidth(((SUMA_ABS(curcol->V[i]))*Wfac+Wrange[0])
                                                         *curcol->EdgeThickGain); 
         selcol[0] = 1-sv->clear_color[0];
         selcol[1] = 1-sv->clear_color[1];
         selcol[2] = 1-sv->clear_color[2];
         selcol[3] = 1-sv->clear_color[3]; 
         SUMA_LHv("Col:[%f %f %f %f]\n",
            selcol[0], selcol[1], selcol[2], selcol[3]);
         glBegin(GL_LINES);
         glMaterialfv(GL_FRONT, GL_EMISSION, selcol);
               
         glVertex3f( DDO.NodeList[cn3], DDO.NodeList[cn3+1], 
                     DDO.NodeList[cn3+2]);
         glVertex3f( DDO.NodeList[cn13  ], 
                     DDO.NodeList[cn13+1], 
                     DDO.NodeList[cn13+2]); 
         glEnd();
         if (gl_dt) glEnable(GL_DEPTH_TEST);
         if (stipsel && !gllst) glDisable(GL_LINE_STIPPLE);
      }
   } 
      
   if (!sv->DO_PickMode) {
      switch (SDO->Stipple) {
         case SUMA_DASHED_LINE:
            if (!gllst) glDisable(GL_LINE_STIPPLE);
            break;
         case SUMA_SOLID_LINE:
            if (!gllsm) glDisable(GL_LINE_SMOOTH);
            break;
      }
   }
   
   /* draw the bottom object */
   if (SDO->botobj) {
      float *xyz=(float *)SUMA_malloc(3*SDO->N_UnqNodes*sizeof(float));
      float *xyzr=NULL;
      int *GNIr=NULL;
      char **namesr=NULL;
      SUMA_LH("Drawing bottom");
      glLineWidth(0.5);
      if (!SDO->colv) {
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, SDO->LineCol);
      }
      
      if (DDO.AvgLe != 0.0f) radconst = DDO.AvgLe/4.0*curcol->NodeRadGain;
      else radconst = 1.0/4.0*curcol->NodeRadGain;
            
      gluQuadricDrawStyle (SDO->botobj, GLU_FILL); 
      gluQuadricNormals (SDO->botobj , GLU_SMOOTH);

      /* create a mask for those spheres already drawn, 
         multiple vectors per node possible...*/
      
      if (sv->DO_PickMode) {
         if (!(colidballs = SUMA_DO_get_pick_colid(
                              (SUMA_ALL_DO *)SDO, SDO->idcode_str,
                              "balls", SDO->DrawnDO_variant,
                              SDO->Parent_idcode_str, SDO->Parent_do_type,
                              sv) )) {
            SUMA_S_Err("Failed to create colid for ball picking.");
         }
      }
      
      /* Get the coords of the nodes to represent */
      for (i=0; i<SDO->N_UnqNodes;++i) {
         cn  = SUMA_NodeIndex_To_Index(DDO.NodeIndex, 
                                       DDO.N_Node, GNI ? GNI[i]:i); 
         cn3 = 3*cn;
         xyz[3*i  ] = DDO.NodeList[cn3  ];
         xyz[3*i+1] = DDO.NodeList[cn3+1];
         xyz[3*i+2] = DDO.NodeList[cn3+2];
      }
      
      if (fontGL && names && depthsort) {
         float *xyzsc= SUMA_malloc(3*SDO->N_UnqNodes*sizeof(float)), 
               *xyzscr=NULL;
         dsrt = SUMA_DepthSort(xyz, SDO->N_UnqNodes, names, 0, xyzsc);
         xyzr = SUMA_freorder_triplets(xyz, dsrt, SDO->N_UnqNodes); 
         xyzscr = SUMA_freorder_triplets(xyzsc, dsrt, SDO->N_UnqNodes); 
         GNIr = SUMA_reorder(GNI, dsrt, SDO->N_UnqNodes);
         namesr = SUMA_sreorder(names, dsrt, SDO->N_UnqNodes);
         #if 0
         fprintf(stderr,"Sorting from farthest to closest:\n");
         for (i=0; i<SDO->N_UnqNodes;++i) {
            fprintf(stderr,"dsrt[%d]=%d, namesr[%d] = %s @[%.2f %.2f %.2f],"
                           " names[%d]= %s @ [%.2f %.2f %.2f]\n",
                           i, dsrt[i], i, namesr[i], 
                           xyzr[3*i], xyzr[3*i+1], xyzr[3*i+2], i, names[i],
                           xyz[3*i], xyz[3*i+1], xyz[3*i+2]);
         }
         #endif
         wmask = SUMA_WordOverlapMask(sv->X->WIDTH, sv->X->HEIGHT,
                                      SDO->N_UnqNodes, 
                                      namesr, fontGL, xyzscr, -1);
         SUMA_ifree(xyzsc); SUMA_ifree(xyzscr); 
      } else {
         xyzr = xyz;
         GNIr = GNI;
         namesr = names;
         wmask = NULL;
      }
      n4=0; 
      for (i=0; i<SDO->N_UnqNodes;++i) {
         i3 = 3*i; i4 = 4*i;
         if (GNIr) {
            n = GNIr[i];
         } else {
            n = i;
         }
         
         if (wmask) showword = wmask[i];
         else showword = 255;
                   
         SUMA_LHv("%d/%d, %d, showword %d\n", 
                  i, SDO->N_UnqNodes, n, showword);
         okind=-2;
         if (curcol->NodeRad >= 0) {
            if (curcol->NodeRad == SW_SurfCont_DsetNodeRadVal) {
               if (okind == -2) {
                  if (!SUMA_GDSET_PointToDiagSegRowIndex(dset,n,&ri,&si)){
                     okind = -1;     
                  } else okind = 1;
               }
               if (okind>0) 
                  rad = SUMA_ABS(curcol->V[ri])*curcol->NodeRadGain;
            } else {
               rad = radconst;
            }
            if (OnlyThroughNode == n) {
               selcol[0] = 1-sv->clear_color[0];
               selcol[1] = 1-sv->clear_color[1];
               selcol[2] = 1-sv->clear_color[2];
               selcol[3] = 1-sv->clear_color[3]; 
               glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, selcol);
               glMaterialfv(GL_FRONT, GL_EMISSION, selcol);
            } else {
               if (colidballs) {
                  if (dsrt) {
                     i4 = 4*dsrt[i];
                  } else i4 = 4*i;
                  colballpick = colidballs+i4;
                  glColor4ub( colidballs[i4  ], colidballs[i4+1], 
                              colidballs[i4+2], colidballs[i4+3]);
               } else if (SDO->colv && 
                          curcol->NodeCol == SW_SurfCont_DsetNodeColVal) {
                  if (okind == -2) {
                     if (!SUMA_GDSET_PointToDiagSegRowIndex(
                                                      dset,n,&ri,&si)){
                        okind = -1;     
                     } else okind = 1;
                  }
                  if (okind>0) {
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                                      &(SDO->colv[si*4]));
                     glMaterialfv(GL_FRONT, GL_EMISSION, 
                                                      &(SDO->colv[si*4]));
                  } else {
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                                         constcol);
                     glMaterialfv(GL_FRONT, GL_EMISSION, constcol);
                  }
               } else {
                  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, constcol);
                  glMaterialfv(GL_FRONT, GL_EMISSION, constcol);
               }
            }
            glTranslatef ( xyzr[i3]  , xyzr[i3+1]  , xyzr[i3+2]  );
            gluSphere(SDO->botobj, SUMA_MAX_PAIR(rad, 0.005) 
                        /* *SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06) */ , 
                        10, 10);
            glTranslatef (-xyzr[i3]  ,  -xyzr[i3+1]  , -xyzr[i3+2]  );
         }
         if (fontGL && names) {
            if (gl_dt) glDisable(GL_DEPTH_TEST);
            if (colidballs) {
               SUMA_COPY_VEC(colballpick, col1, 4, GLbyte, GLfloat);
               SUMA_COPY_VEC(colballpick, col2, 4, GLbyte, GLfloat);
            } else {
               if (TxtShadeMode) {
                  SUMA_COPY_VEC(textshadcolor, col1, 4, GLfloat, GLfloat);
                  SUMA_COPY_VEC(textcolor, col2, 4, GLfloat, GLfloat);
               } else {
                  SUMA_COPY_VEC(textcolor, col1, 4, GLfloat, GLfloat);
               }
            }
            SUMA_LHv("namesr[%d] %s at %f %f %f, shad = %d, fontGL=%p"
                     "col1 [%.3f %.3f %.3f] col2 [%.3f %.3f %.3f] \n"
                     "size(GLbyte) %ld, size(byte) %ld\n",
                     i, names ? namesr[i]:"NULL"
                     , xyzr[i3], xyzr[i3+1], xyzr[i3+2],
                     TxtShadeMode, fontGL, col1[0], col1[1], col1[2],
                     col2[0], col2[1], col2[2], sizeof(GLbyte), sizeof(byte));
            if (colidballs) { /* No need to allow selection with different
                                 shadow modes, just put a box where the 
                                 text is to go and let the picking be based
                                 on that. If you want to see the text
                                 for debugging the pick buffer, block
                                 this if statement and let function
                                 proceed below */
               glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, col1);
               glMaterialfv(GL_FRONT, GL_EMISSION, col1);
               glRasterPos3d(xyzr[i3]  +rad , 
                       xyzr[i3+1]+rad , 
                       xyzr[i3+2] +rad );
               glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
               glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
               if (!valid) continue;
               SUMA_TextBoxSize (namesr[i], &tw, &th, &nl, fontGL);
               bbox = (byte*)SUMA_calloc(4*(tw+2)*(th+2), sizeof(byte));
               /* Turn box white? Someday you'll need to set the
                  box colors to be that of the background exactly... */
               iii=0;
               while (iii<4*(tw+2)*(th+2)) {
                  bbox[iii++] = colballpick[0];
                  bbox[iii++] = colballpick[1];
                  bbox[iii++] = colballpick[2];
                  bbox[iii++] = colballpick[3];
               }   
               glBitmap( 0, 0, 0, 0,  
                         0.0, -th/4.0,  NULL );
               glDrawPixels(tw, th, GL_RGBA, GL_UNSIGNED_BYTE, bbox);
               SUMA_ifree(bbox);
               continue; /* On to the next point */        
            }
            /* do some text action */
            switch(TxtShadeMode) {
               case SW_SurfCont_DsetTxtShad1:
               case SW_SurfCont_DsetTxtShad5:
                  if (showword > 128 || TxtShadeMode == 5) {
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, col1);
                     glMaterialfv(GL_FRONT, GL_EMISSION, col1);
                     glRasterPos3d(xyzr[i3]  +rad , 
                             xyzr[i3+1]+rad , 
                             xyzr[i3+2] +rad );
                     glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
                     glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
                     if (!valid) break;
                     /* Black font behind white font */
                     for (iii=0; namesr[i][iii] != '\0'; iii++) {
                        glutBitmapCharacter(fontGL, namesr[i][iii]);
                     }
                     /* Change the current raster color 
                     (can't set glMaterialfv(), alone, need 
                     call to glRasterPos3d which will also set the 
                     current raster color (GL_CURRENT_RASTER_COLOR)
                     Note that we draw the shadow first, to avoid
                     aliasing artifacts from the shadow*/
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                                         col2);
                     glMaterialfv(GL_FRONT, GL_EMISSION, col2);
                     glRasterPos3d(xyzr[i3]  +rad , 
                                   xyzr[i3+1]+rad , 
                                   xyzr[i3+2] +rad );
                     /* offset rel. to  shadow */
                     glBitmap( 0, 0, 0, 0,  
                               -1.0, -1.0,  NULL );
                     for (iii=0; namesr[i][iii] != '\0'; iii++) {
                        glutBitmapCharacter(fontGL, namesr[i][iii]);
                     }
                  }
                  break;
               case SW_SurfCont_DsetTxtShad2:
                  if (showword < 250) {
                     /* dim the colors */
                     col2[0] = col2[0]*cdim;
                     col2[1] = col2[1]*cdim;
                     col2[2] = col2[2]*cdim;
                  }
                  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, col1);
                  glMaterialfv(GL_FRONT, GL_EMISSION, col1);
                     /* Must come AFTER glMaterialfv */
                  glRasterPos3d( xyzr[i3]  +rad , 
                                 xyzr[i3+1]+rad , 
                                 xyzr[i3+2] +rad );
                  glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
                  glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
                  if (!valid) break;
                  /* Black font behind white font */
                  for (iii=0; names[i][iii] != '\0'; iii++) {
                     glutBitmapCharacter(fontGL, namesr[i][iii]);
                  }
                  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                                      col2);
                  glMaterialfv(GL_FRONT, GL_EMISSION, col2);
                  glRasterPos3d(xyzr[i3]  +rad , 
                                xyzr[i3+1]+rad , 
                                xyzr[i3+2] +rad );
                  SUMA_TextBoxSize (namesr[i], &tw, &th, &nl, fontGL);
                  /* offset rel. to  shadow */
                  glBitmap( 0, 0, 0, 0,  
                            -1.0, -1.0,  NULL );
                  for (iii=0; namesr[i][iii] != '\0'; iii++) {
                     glutBitmapCharacter(fontGL, namesr[i][iii]);
                  }
                  break;
               case SW_SurfCont_DsetTxtShad6:
               case SW_SurfCont_DsetTxtShad3:
                  /* Black box behind white font, hide if more than
                     half is masked */
                  if (showword > 128 || TxtShadeMode == 6) {
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, col1);
                     glMaterialfv(GL_FRONT, GL_EMISSION, col1);
                     /* Must come AFTER glMaterialfv */
                     glRasterPos3d( xyzr[i3]  +rad , 
                                    xyzr[i3+1]+rad , 
                                    xyzr[i3+2] +rad );
                     glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
                     glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
                     if (!valid) break;
                     SUMA_TextBoxSize (namesr[i], &tw, &th, &nl, fontGL);
                     SUMA_LHv("namesr[%d]=%s %d %d %d\n", 
                              i, namesr[i], tw, th, nl);
                     /* Had to add the +2 in calloc and memset to get rid
                     of a strange one pixel wide black line that showed 
                     up on the top right corner of the box. It is only
                     noticeable when wbox is set. Not sure what causes this*/ 
                     bbox = (byte*)SUMA_calloc(4*(tw+2)*(th+2), sizeof(byte));
                     /* Turn box white? Someday you'll need to set the
                        box colors to be that of the background exactly... */
                     if (wbox) memset(bbox, 255, 4*(tw+2)*(th+2)*sizeof(byte));
                     glBitmap( 0, 0, 0, 0,  
                               0.0, -th/4.0,  NULL );
                     glDrawPixels(tw, th, GL_RGBA, GL_UNSIGNED_BYTE, bbox);
                     SUMA_ifree(bbox);
                     /* Now draw the text */
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                                         col2);
                     glMaterialfv(GL_FRONT, GL_EMISSION, col2);
                     glRasterPos3d(xyzr[i3]  +rad , 
                                   xyzr[i3+1]+rad , 
                                   xyzr[i3+2] +rad );
                     for (iii=0; namesr[i][iii] != '\0'; iii++) {
                        glutBitmapCharacter(fontGL, namesr[i][iii]);
                     }
                  }
                  break;
               case SW_SurfCont_DsetTxtShad4:
                  /* Black box behind white font for unobstructed 
                     text. Obstructed text has no black background
                     and is dimmed by overlap. Tried variable dimming
                     but it gets confusing. Note that obstruction
                     is only computed for text with text, not text
                     with objects */
                  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, col1);
                  glMaterialfv(GL_FRONT, GL_EMISSION, col1);
                  /* Must come AFTER glMaterialfv */
                  glRasterPos3d( xyzr[i3]  +rad , 
                                 xyzr[i3+1]+rad , 
                                 xyzr[i3+2] +rad );
                  glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
                  glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
                  if (!valid) break;
                  SUMA_TextBoxSize (namesr[i], &tw, &th, &nl, fontGL);
                  SUMA_LHv("namesr[%d]=%s %d %d %d\n", 
                           i, namesr[i], tw, th, nl);
                  if (showword > 250) { /* Practically fully exposed
                                          Show word proudly, with background*/
                     bbox = (byte*)SUMA_calloc(4*(tw+2)*(th+2), sizeof(byte));
                     if (wbox) memset(bbox, 255, 4*(tw+2)*(th+2)*sizeof(byte));

                     glBitmap( 0, 0, 0, 0,  
                               0.0, -th/4.0,  NULL );
                     glDrawPixels(tw, th, GL_RGBA, GL_UNSIGNED_BYTE, bbox);
                     SUMA_ifree(bbox);
                  } else { /* dim the text to reduce clutter */
                     col2[0] = col2[0]*cdim;
                     col2[1] = col2[1]*cdim;
                     col2[2] = col2[2]*cdim;
                  }
                  /* Now draw the text */
                  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                                      col2);
                  glMaterialfv(GL_FRONT, GL_EMISSION, col2);
                  glRasterPos3d(xyzr[i3]  +rad , 
                                xyzr[i3+1]+rad , 
                                xyzr[i3+2] +rad );
                  for (iii=0; namesr[i][iii] != '\0'; iii++) {
                     glutBitmapCharacter(fontGL, namesr[i][iii]);
                  }
                  break;
               default:
                  SUMA_S_Warnv("Bad shadow val of %d, using simplest case\n", 
                               TxtShadeMode);
                  
                  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, textcolor);
                  glMaterialfv(GL_FRONT, GL_EMISSION, textcolor);
                  glRasterPos3d(xyzr[i3]  +rad , 
                          xyzr[i3+1]+rad , 
                          xyzr[i3+2] +rad );
                  glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
                  glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
                  if (!valid) break;
                  for (iii=0; namesr[i][iii] != '\0'; iii++) {
                     glutBitmapCharacter(fontGL, namesr[i][iii]);
                  }
                  break;
            }
            if (gl_dt) glEnable(GL_DEPTH_TEST);
         }               
      }
      if (xyzr != xyz) SUMA_ifree(xyzr); xyzr=NULL;
      if (namesr != names) SUMA_ifree(namesr); namesr=NULL;
      if (GNIr != GNI) SUMA_ifree(GNIr); GNIr = NULL;
      SUMA_ifree(xyz);
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
   }

   GETOUT:
   SUMA_ifree(dsrt); SUMA_ifree(wmask);
   if (sv->DO_PickMode) DO_PICK_RESTORE;
   
   SUMA_ifree(colid); SUMA_ifree(colidballs);
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity */
   glLineWidth(origwidth);
   if (mask) SUMA_free(mask); mask=NULL;
   if (gl_dt) glEnable(GL_DEPTH_TEST);
   SUMA_RETURN (YUP);
   
}

/* A function to find out which words would overlap when displayed in OpenGL
   Nwidth (int) width of viewport in pixels
   Nheight (int) height of viewport in pixels
   N_n (int) number of words
   names (char **) the words (spaces OK, just not multiple lines).
                   words in names should be sorted from the farthest
                   to the closest
   fontGL (void *) the font in use
   xyzr (float *) XYZ triplets specifying position of each word in 
                 SCREEN coordinates. 
   maxoverlap (float ) a number between 0 and 1: 
                  0 means a word will be let in if it does not overlap with
                    the boundaries of another
                  1 means all words will be allowed to show.
                  -1 means no thresholding in applied, get a continous 
                     overlap mask
   \ret mask (byte *) A byte mask with '1' where a word should be shown, 
                      0 otherwise. A NULL could signal an error, but also
                      that all words should be displayed.            
*/                        
byte *SUMA_WordOverlapMask(int Nwidth, int Nheight, int N_n, 
                           char **names, void *fontGL, float *xyzr,
                           float maxoverlap)
{
   static char FuncName[]={"SUMA_WordOverlapMask"};
   byte **overbuf=NULL, *mask=NULL;
   int i, ibuf, jbuf, empt, wh, *ww=NULL, nn, mm, offh, offw;
   float pempt;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   SUMA_LHv("Nwidth %d, Nheight %d maxoverlap %f\n"
            " Words from CLOSEST to farthest\n", 
            Nwidth, Nheight, maxoverlap);
   overbuf = (byte **)SUMA_allocate2D(Nwidth, Nheight, sizeof(byte));
   ww = (int *)SUMA_malloc(N_n*sizeof(int));
   mask = (byte *)SUMA_calloc(N_n, sizeof(byte));
   wh = SUMA_WordBoxSize(names, N_n, ww, fontGL);
   for (i = N_n-1; i>-1; --i) {
      ibuf = (int)xyzr[3*i]; jbuf = (int)xyzr[3*i+1];
      if (ibuf < 0) ibuf = 0;
      if (jbuf < 0) jbuf = 0;
      if (ibuf+ww[i]>Nwidth) offw = Nwidth-ibuf;
      else offw = ww[i];
      if (jbuf+wh>Nheight) offh = Nheight-jbuf;
      else offh = wh;
      empt = 0;
      for (nn=0; nn<offw; ++nn) {
         for (mm=0; mm<offh; ++mm) {
            if (!overbuf[nn+ibuf][mm+jbuf]) {
               ++empt;
            }
         }
      }
      pempt = (float)empt/(ww[i]*wh);
      if (maxoverlap >= 0.0) {
         if (pempt >= 1.0-maxoverlap) { /* deserves keeping, so mark it */
            mask[i] = (byte)(pempt*255.0);
            for (nn=0; nn<offw; ++nn) {
               for (mm=0; mm<offh; ++mm) {
                  if (overbuf[nn+ibuf][mm+jbuf] < 255) 
                        ++overbuf[nn+ibuf][mm+jbuf];
               }
            }
         } else {
            mask[i] = 0.0;
         }
      } else { /* continous marking */
         mask[i] = (byte)(pempt*255.0);
         for (nn=0; nn<offw; ++nn) {
            for (mm=0; mm<offh; ++mm) {
               if (overbuf[nn+ibuf][mm+jbuf] < 255) 
                        ++overbuf[nn+ibuf][mm+jbuf];
            }
         }
      }
      SUMA_LHv("%s pempt[%d] %f, mask[%d]=%d bloc=[%d %d], off [%d %d]\n", 
               names[i], i, pempt, i, mask[i], ibuf,jbuf, offw, offh);
         
   }
   
   if (LocalHead) {
      FILE *fid = fopen(FuncName,"w");
      for (mm=0; mm<Nheight; ++mm) {
         for (nn=0; nn<Nwidth; ++nn) {
            fprintf(fid,"%d ", overbuf[nn][mm]);
         }
         fprintf(fid,"\n");
      }
      fclose(fid); fid = NULL;
      SUMA_LHv("To view debugging image:\n"
               "  aiv %s\n", FuncName);
   }
      
   SUMA_ifree(ww); 
   SUMA_free2D((char **)overbuf, Nwidth); overbuf=NULL;

   SUMA_RETURN(mask);
}

SUMA_Boolean SUMA_LoadImageNIDOnel(NI_element *nel)
{
   static char FuncName[]={"SUMA_LoadImageNIDOnel"};
   MRI_IMAGE *im=NULL;
   int ir, ic, i1d, i1df, imx, i1d3, i1d4;
   byte *rgb = NULL, *imar=NULL;
   float alf = 0.0;
   char *fname=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (  !nel || 
         (  strcmp(nel->name,"I") && 
            strcmp(nel->name,"Tex" ) ) )
      SUMA_RETURN(NOPE);
   
   if (NI_IS_STR_ATTR_EQUAL(nel, "read_status", "read")) SUMA_RETURN(YUP);
    
   NI_set_attribute(nel,"read_status","fail");
   
   if (! (fname = SUMA_copy_string(NI_get_attribute(nel,"filename"))) )
      SUMA_RETURN(NOPE);
   if (!SUMA_search_file(&fname, NULL)) { /* can't find it */ 
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }
   if (!(im = mri_read_just_one(fname))) {
      SUMA_S_Errv("Failed to read image %s (READ from %s)\n", 
                  NI_get_attribute(nel,"filename"),
                  fname);
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }
   
   rgb = MRI_BYTE_PTR(im) ;
   NI_SET_INT(nel,"height",im->ny);
   NI_SET_INT(nel,"width", im->nx);

   if (im->kind != MRI_rgb && im->kind != MRI_byte) {
      SUMA_S_Errv("Image %s (read from %s) must be RGB or byte type.\n", 
                  NI_get_attribute(nel,"filename"),
                  fname);
      mri_free(im) ;
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE); 
   }

   if (LocalHead) 
      fprintf (SUMA_STDERR,
               "%s:\nNx (width) = %d, Ny (height) = %d\n", 
               FuncName, im->nx, im->ny);

   /* Now you can call  NI_alter_veclen, although all it does
   here is set vec_len to the new number ... */
   NI_alter_veclen(nel, (int)(im->nx * im->ny * 4));
   if (LocalHead) {
      SUMA_LH("veclen altered");
      SUMA_ShowNel(nel);
   }
   NI_add_column (nel, NI_BYTE, NULL); 
   if (!nel->vec[0]) {
      SUMA_SL_Crit("Failed to allocate.");
      mri_free(im) ;
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE); 
   }
   SUMA_LH("Filling");
   imar = (byte *)nel->vec[0];
   if (im->kind == MRI_rgb) {
      SUMA_LH("RGB");
      for (ir = 0; ir < im->ny; ++ir) {
         for (ic = 0; ic < im->nx; ++ic) {
            i1d = ic + ir * im->nx; /* equivalent 1d index into row 
                                       major image data */
            i1df = ic + (im->ny - ir - 1) * im->nx; /* column flipped index */
            i1d4 = 4 * i1d; i1d3 = 3*i1df; 
            imar[i1d4] = rgb[i1d3];    alf  = (float)imar[i1d4];   
                  ++i1d3; ++i1d4; 
            imar[i1d4] = rgb[i1d3];    alf += (float)imar[i1d4];   
                  ++i1d3; ++i1d4; 
            imar[i1d4] = rgb[i1d3];    alf += (float)imar[i1d4];                                      ++i1d4; 
            imar[i1d4] = 200; /* got no alpha to work with*/
         }
      }
   } else if (im->kind == MRI_byte) {  /* inefficient, but easy for now */
      SUMA_LH("BYTE");
      for (ir = 0; ir < im->ny; ++ir) {
         for (ic = 0; ic < im->nx; ++ic) {
            i1d = ic + ir * im->nx; /* equivalent 1d index into row 
                                       major image data */
            i1df = ic + (im->ny - ir - 1) * im->nx; /* column flipped index */
            i1d4 = 4 * i1d;  
            imar[i1d4] = (byte)rgb[i1df];       
                          ++i1d4; 
            imar[i1d4] = (byte)rgb[i1df];       
                          ++i1d4; 
            imar[i1d4] = (byte)rgb[i1df];                                                                   ++i1d4; 
            imar[i1d4] = (byte)rgb[i1df]; 
         }
      }
   } else {
      SUMA_S_Err("Image must be RGB or byte type.\nShould not be here!");
      mri_free(im) ;
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE); 
   }
   if (LocalHead) {
      FILE *fid=NULL;
      fid = fopen("junk2.img", "w");
      SUMA_disp_vecbytemat ( imar, im->nx*im->ny, 
                           4, 1, SUMA_ROW_MAJOR, fid, NOPE);
      fclose(fid);
   }
   
   SUMA_LH("Freedom");
   mri_free(im) ; im = NULL;
   SUMA_free(fname); fname = NULL;

   NI_set_attribute(nel,"read_status","read");
      
   SUMA_RETURN(YUP);
}

#define UseAlphaAndBlend 0   /* to hide calls that would be useful 
                              for alpha testing and/or blending */
SUMA_Boolean SUMA_DrawImageNIDOnel( NI_element *nel, 
                                    SUMA_SurfaceObject *default_SO,
                                    SUMA_DO_CoordUnits default_coord_units,
                                    float *default_color, 
                                    void *default_font, int default_node,
                                    SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_DrawImageNIDOnel"};
   float txloc[3] = {0.0, 0.0, 0.0};
   char  *string=NULL, *atr=NULL;
   GLfloat rpos[4];
   float Dx = 0.0;
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   GLdouble pfront[3] = {0.0, 0.0, 0.0}, pback[3]= {0.0, 0.0, 0.0};
   GLboolean valid;
   GLint viewport[4];
   int orthoreset = 0;
   int id=0, is = 0, sz[2]={0, 0};
   GLboolean TexOn2D=0, TexOnGenT=0, TexOnGenS=0;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_DO_CoordUnits coord_units = default_coord_units;
   
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!nel || strcmp(nel->name,"I")) SUMA_RETURN(NOPE);
   
   if (NI_IS_STR_ATTR_EQUAL(nel,"read_status","fail")) {
      /* can't be read */
      SUMA_RETURN(NOPE);
   }
   
   if (!NI_IS_STR_ATTR_EQUAL(nel,"read_status","read")) { /* read it */
      if (!SUMA_LoadImageNIDOnel(nel)) {
         SUMA_RETURN(NOPE);
      }
   } 
   
   
   /* has the box size been determined ?*/
   NI_GET_INTv(nel,"box_size", sz, 2, LocalHead);
   if (!NI_GOT) {
      NI_GET_INT(nel,"width", sz[0]);
      NI_GET_INT(nel,"height", sz[1]);
      NI_SET_INTv(nel,"box_size", sz, 2);
   }
   
   /* set up projection conditions and coordinates */
   if (!SUMA_PrepForNIDOnelPlacement(sv, nel, default_SO, default_node, 
                                     txloc, NULL, sz, 
                                     &orthoreset, coord_units, NULL, NULL)) {
      SUMA_RETURN(NOPE);
   }    

   /* how about that rendering mode ? */
   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   }
   
   /* texture's on? */
   if ((TexOn2D = glIsEnabled(GL_TEXTURE_2D))) { 
      /* turn off or affect image drawing */
      glDisable(GL_TEXTURE_2D);
   }
   if ((TexOnGenT = glIsEnabled(GL_TEXTURE_GEN_T))) glDisable(GL_TEXTURE_GEN_T);
   if ((TexOnGenS = glIsEnabled(GL_TEXTURE_GEN_S)))  glDisable(GL_TEXTURE_GEN_S);
                  
   /* have image, go for it */
   SUMA_LHv("Drawing the image for %s at raster pos: %f %f %f\n",
            CHECK_NULL_STR(NI_get_attribute(nel, "filename")), 
            txloc[0], txloc[1], txloc[2]);

   glRasterPos3f( txloc[0], txloc[1], txloc[2]);
   
   #if UseAlphaAndBlend
      initTest = glIsEnabled(GL_ALPHA_TEST);
      initBlend = glIsEnabled(GL_BLEND);
      
      /* here you set up your alpha testing, problem 
      at the moment, is that image reading routines 
      provide no alpha. But nel->vec[0] does contain
      alpha and thus can be used eventually for that.
      Note that parameters for ALPHA_TEST and BLEND 
      can easily be added to nel in the future*/
      
      glAlphaFunc(GL_GEQUAL, 0.25); /* this only needs to be called
               if it is changed elsewhere. If it is set differently
               per image, then we'll need to call it everytime. 
               I think it is easiest to call it repeatedly...
              to set GL_ALPHA_TEST's behaviour. Choose from:
              GL_NEVER, GL_LESS, GL_EQUAL, GL_LEQUAL, GL_GREATER, GL_NOTEQUAL, 
              GL_GEQUAL, and GL_ALWAYS */
      glEnable(GL_ALPHA_TEST);
      
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
   #endif
   

   glDrawPixels(sz[0], sz[1], GL_RGBA,
                GL_UNSIGNED_BYTE, nel->vec[0]);
   
   #if UseAlphaAndBlend
      if (initTest == GL_FALSE) glDisable(GL_ALPHA_TEST);
      else {
         /* need to spend time, putting restoring
         glBlendFunc and glAlphaFunc settings before
         this function altered them*/
      }
      if (initBlend == GL_FALSE) glDisable(GL_BLEND);
      else {
         /* need to spend time, putting restoring
         glBlendFunc and glAlphaFunc settings before
         this function altered them*/
      }
   #endif

   if (orthoreset) {/* return value */
      /* and just pop what you'd pushed in */
      glPopMatrix();
   }
   
   if (sv->PolyMode != SRM_Fill) {/* set fill mode back */
      SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
   }
   
   if (TexOn2D) {   /* put it back on if needed */
      glEnable(GL_TEXTURE_2D);
   }
   if (TexOnGenT) glEnable(GL_TEXTURE_GEN_T);
   if (TexOnGenS) glEnable(GL_TEXTURE_GEN_S);
 
   SUMA_RETURN(YUP);   
}

/* Returns parameter that controls what happens when texture 
  falls on top of vertex with color:
   GL_DECAL, GL_REPLACE, GL_MODULATE, GL_BLENDGL_REPLACE, GL_MODULATE */
int SUMA_NIDO_TexEnvMode(NI_element *nel, int def) 
{
   char *atr=NI_get_attribute(nel,"mix_mode");
   if (!atr) return(def);
   if (!strcmp(atr,"decal")) return(GL_DECAL);
   else if (!strcmp(atr,"blend")) return(GL_BLEND);
   else if (!strcmp(atr,"replace")) return(GL_REPLACE);
   else if (!strcmp(atr,"modulate")) return(GL_MODULATE);
   else return(def);
} 
int SUMA_NIDO_TexCoordGen(NI_element *nel) 
{
   char *atr=NI_get_attribute(nel,"coord_gen");
   if (!atr) return(GL_SPHERE_MAP);
   if (!strcmp(atr,"object")) return(GL_OBJECT_LINEAR);
   else if (!strcmp(atr,"eye")) return(GL_EYE_LINEAR);
   else if (!strcmp(atr,"sphere")) return(GL_SPHERE_MAP);
   else return(GL_SPHERE_MAP);
}
int SUMA_NIDO_QuadricStyle(NI_element *nel) 
{
   char *atr=NI_get_attribute(nel,"style");
   if (!atr) return(GLU_FILL);
   if (!strcmp(atr,"fill")) return(GLU_FILL);
   else if (!strcmp(atr,"line")) return(GLU_LINE);
   else if (!strcmp(atr,"silhouette")) return(GLU_SILHOUETTE);
   else if (!strcmp(atr,"point")) return(GLU_POINT);
   else return(GLU_FILL);
}

SUMA_Boolean SUMA_DrawTextureNIDOnel( NI_element *nel, 
                                    SUMA_SurfaceObject *default_SO,
                                    SUMA_DO_CoordUnits default_coord_units,
                                    float *default_color, 
                                    void *default_font, int default_node,
                                    SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_DrawTextureNIDOnel"};
   float txloc[3] = {0.0, 0.0, 0.0};
   float texcoord[12] = {0.0, 0.0, 0.0,
                         0.0, 1.0, 0.0,
                         1.0, 1.0, 0.0,
                         1.0, 0.0, 0.0};
   char  *string=NULL, *atr=NULL, *target = NULL;
   GLfloat rpos[4];
   float Dx = 0.0;
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   GLdouble pfront[3] = {0.0, 0.0, 0.0}, pback[3]= {0.0, 0.0, 0.0};
   GLboolean valid;
   GLint viewport[4];
   int orthoreset = 0;
   int id=0, ii = 0, sz[3]={0, 0, 0}, dt = 0, pof=0;
   int N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   SUMA_SurfaceObject *SOt=NULL;
   static GLuint texName;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_DO_CoordUnits coord_units = default_coord_units;
   
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!nel || strcmp(nel->name,"Tex")) SUMA_RETURN(NOPE);
      
   if (NI_IS_STR_ATTR_EQUAL(nel,"read_status","fail")) {
      /* can't be read */
      SUMA_RETURN(NOPE);
   }
   
   if (!NI_IS_STR_ATTR_EQUAL(nel,"read_status","read")) { /* read it */
      if (!SUMA_LoadImageNIDOnel(nel)) {
         SUMA_RETURN(NOPE);
      }
   } 
   
   
   /* has the box size been determined (only 2 dimensions needed)?*/
   NI_GET_INTv(nel,"box_size", sz, 2, LocalHead);
   if (!NI_GOT) {
      NI_GET_INT(nel,"width", sz[0]);
      NI_GET_INT(nel,"height", sz[1]);
      NI_SET_INTv(nel,"box_size", sz, 2);
   }
   
   /* set up projection conditions and coordinates */
   if (!SUMA_PrepForNIDOnelPlacement(sv, nel, default_SO, default_node, 
                                     txloc, texcoord, sz, 
                                     &orthoreset, coord_units, NULL, NULL)) {
      SUMA_RETURN(NOPE);
   }    

   /* how about that rendering mode ? */
   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   } else {
      /* If you don't turn offset off, FRAME bound texture (afniman.jpg)
         won't show ...  ZSS April 2011 */
      /* afniman.jpg (in @DO.examples) now does not show up unless
      view is in orthographic mode. Behavior might change whether or
      not one is in ortho mode. See comment below about turning off
      depth test.
                                          ZSS Jan 2012 */ 
      if ((pof = glIsEnabled(GL_POLYGON_OFFSET_FILL))) 
                        glDisable (GL_POLYGON_OFFSET_FILL);
   }
   
   
   /* does this have its own coordinates ? */
   target = NI_get_attribute(nel,"target");
   if (target && strcmp(target, "FRAME")==0) {
      SUMA_LH(  "Creating texture, see init pp 359 in \n"
                "OpenGL programming guide, 3rd ed.");
      #if 0 /* Not needed anymore, March 2013 */
                          /* Frame texture (afniman.jpg in @DO.examples         
                             had been obscuring meshes no matter where
                             it was placed in the Z direction. Not sure
                             why but since it is only to be used as
                             a background display, undoing GL_DEPTH_TEST
                             seemed to do the trick. Jan 2012 */
      if ((dt = glIsEnabled(GL_DEPTH_TEST))) glDisable(GL_DEPTH_TEST);
      #endif
      
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
      NI_GET_INT(nel,"texName",texName);
      if (!NI_GOT) {
         /* Need to generate texture */
         glGenTextures(1, &texName);
         /* Now store it */
         NI_SET_INT(nel,"texName",texName);
      }
      glBindTexture(GL_TEXTURE_2D, texName);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S, GL_REPEAT); 
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T, GL_REPEAT); 
      glTexParameteri(  GL_TEXTURE_2D,
                        GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(  GL_TEXTURE_2D,
                        GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexImage2D(  GL_TEXTURE_2D, 0, GL_RGBA, 
                     sz[0], sz[1], 0, GL_RGBA, 
                     GL_UNSIGNED_BYTE, nel->vec[0]);
      glEnable(GL_TEXTURE_2D);
      glTexEnvf(  GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, 
                  SUMA_NIDO_TexEnvMode(nel, GL_REPLACE));
         /* GL_DECAL, GL_REPLACE, GL_MODULATE, GL_BLEND */
      glBindTexture(GL_TEXTURE_2D, texName); 
      SUMA_LHv("Texture as image, texName=%d, filename=%s\n"
                   "coords:\n"
                   "%.3f %.3f %.3f\n%.3f %.3f %.3f\n"
                   "%.3f %.3f %.3f\n%.3f %.3f %.3f\n",
                   texName, CHECK_NULL_STR(NI_get_attribute(nel,"filename")),
                   texcoord[0], texcoord[1], texcoord[2],
                   texcoord[3], texcoord[4], texcoord[5],
                   texcoord[6], texcoord[7], texcoord[8],
                   texcoord[9], texcoord[10], texcoord[11]);
      /* Texture does not belong to surface to be rendered later,
      manually generate coordinates*/
      glBegin(GL_QUADS);
      glTexCoord2f(0.0, 0.0);glVertex3f(texcoord[0], texcoord[1], texcoord[2]);
      glTexCoord2f(0.0, 1.0);glVertex3f(texcoord[3], texcoord[4], texcoord[5]);
      glTexCoord2f(1.0, 1.0);glVertex3f(texcoord[6], texcoord[7], texcoord[8]);
      glTexCoord2f(1.0, 0.0);glVertex3f(texcoord[9], texcoord[10], texcoord[11]);
      glEnd();
      glFlush();
      glDisable(GL_TEXTURE_2D);
   } else {
      SUMA_LH("Nodebased textures must be set up before the mesh is\n"
               "drawn. This is done early in SUMA_DrawMesh() in function\n"
               "SUMA_SO_NIDO_Node_Texture()\n");
   }
   
   if (orthoreset) {/* return value */
      /* and just pop what you'd pushed in */
      glPopMatrix();
   }
   
   if (sv->PolyMode != SRM_Fill) {/* set fill mode back */
      SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
   } else {
      if (pof) glEnable (GL_POLYGON_OFFSET_FILL); /* April 2011  */
   }
   
   if (dt) glEnable(GL_DEPTH_TEST); /* Jan 2012 */

   SUMA_RETURN(YUP);   
}

SUMA_Boolean SUMA_PrepForNIDOnelPlacement (  SUMA_SurfaceViewer *sv,
                                             NI_element *nel, 
                                             SUMA_SurfaceObject *default_SO,
                                             int default_node,
                                             float *txloc, float *texcoord,
                                             int *sz, 
                                             int *orthoreset,
                                             SUMA_DO_CoordUnits coord_units,
                                             float *xyzoffset,
                                             int *jaggedwidths)
{
   static char FuncName[]={"SUMA_PrepForNIDOnelPlacement"};
   int id = 0, id3=0, k=0;
   char *atr=NULL, *atr_ha=NULL, *atr_va=NULL;
   SUMA_SurfaceObject *SO = NULL;
   float Ex=0.0, hln=0.0;
   GLdouble pfront[3] = {0.0, 0.0, 0.0}, pback[3]= {0.0, 0.0, 0.0};
   GLint viewport[4];
   static int iwarn=0;
   int N_texcoord = 4;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *orthoreset = 0;
   /* is this node based ?*/
   NI_GET_INT(nel, "node", id);
   if (!NI_GOT) id = default_node;
   if (id >= 0) {
      if (coord_units == SUMA_NORM_SCREEN_UNIT && !(iwarn)) {
         SUMA_S_Note("It makes litle sense to specify node\n"
                     "attribute when coord_type is 'fixed'.\n"
                     "This notice will not be issued again.\n");
         ++iwarn;
      }
      /* special surface? */
      if ((atr=NI_get_attribute(nel,"SO_label"))) {
         SO = SUMA_findSOp_inDOv(
                  SUMA_find_SOidcode_from_label(atr, 
                                 SUMAg_DOv, SUMAg_N_DOv),
                  SUMAg_DOv, SUMAg_N_DOv);
      } else { SO = default_SO; }
      if (!SO) {
         if (atr) { 
            SUMA_S_Errv("Could not find surface called %s\n",
                              atr);
         } else { 
            SUMA_S_Err("Got no daddy");
         }
         SUMA_RETURN(NOPE);
      } 
      txloc[0] = SO->NodeList[3*id];
      txloc[1] = SO->NodeList[3*id+1];
      txloc[2] = SO->NodeList[3*id+2];
      SUMA_LHv(  "Have surface %s for node %d.\n"
                     "[%.3f   %.3f  %.3f]\n", 
                     SO->Label, id,
                     txloc[0], txloc[1], txloc[2]);
   } else {
      if ((atr =  NI_get_attribute(nel,"p"))) {
         int natr = strlen(atr), i=0;
         if (natr > 1 && !SUMA_IS_DIGIT(atr[0])) {
            for (i=0; i<natr; ++i) {
               switch(SUMA_TO_LOWER_C((atr[i]))) {
                  case 't':
                     txloc[1]=1.0;
                     atr_va = "top";
                     break;
                  case 'b':
                     txloc[1]=0.0;
                     atr_va = "bot";
                     break;
                  case 'm':
                  case 'c':
                     if (i==0) {
                        txloc[1]=0.5;
                        atr_va = "center";
                     } else if (i==1) {
                        txloc[0]=0.5;
                        atr_ha = "center";
                     } else if (i==2) {
                        txloc[2]=0.5;
                     }
                     break;
                  case 'l':
                     txloc[0]=0.0;
                     atr_ha = "left";
                     break;
                  case 'r': 
                     if (i<2) {
                        txloc[0]=1.0;/* right */
                        atr_ha = "right";
                     } else  txloc[2]=1.0; /* rear */
                     break;
                  case 'f': /* front */
                     txloc[2]=0.0;
                     break;
               }
            }
         } else {
            NI_GET_FLOATv(nel,"coord",txloc,3, LocalHead);
            atr_va = NI_get_attribute(nel,"v_align");
            atr_ha = NI_get_attribute(nel,"h_align");
         }
      } else {
         NI_GET_FLOATv(nel,"coord",txloc,3, LocalHead);
         atr_va = NI_get_attribute(nel,"v_align");
         atr_ha = NI_get_attribute(nel,"h_align");
      }
      if (texcoord && NI_get_attribute(nel, "frame_coords")) {
         N_texcoord = 4;
         NI_GET_FLOATv( nel, "frame_coords", 
                        texcoord, 3*N_texcoord, LocalHead);
         if (!NI_GOT) N_texcoord = 0;
      }                       
            
      /* If this is screen-based coordinate, do the change,
      the function SUMA_NormScreenToWorld assumes that the OpenGL
      matrices are set appropriately already */
      /* justify */
      {
         glGetIntegerv(GL_VIEWPORT, viewport);
         SUMA_LHv("sz=[%d, %d, %d]\nviewport=[%d %d %d]\n", 
               sz[0], sz[1], sz[2], viewport[0], viewport[1],viewport[2]);
         
         if (xyzoffset) {
            xyzoffset[0]=txloc[0]; 
            xyzoffset[1]=txloc[1]; 
            xyzoffset[2]=txloc[2];
         }
         if ((atr = atr_ha)) {
            if (atr[0] == 'c' || atr[0] == 'C') { /* center */
               if (!jaggedwidths) {
                  txloc[0] = txloc[0] - ((float)sz[0]/2.0 / (float)viewport[2]);
               } else { /* align based for first line only, 
                           not widest part of box */
                  txloc[0] = txloc[0] - 
                           ((float)jaggedwidths[0]/2.0 / (float)viewport[2]);
               }
            } else if (atr[0] == 'r' || atr[0] == 'R') { /* right */
               txloc[0] = txloc[0] - ((float)sz[0] / (float)viewport[2]);
            }
         }
         if (nel->name[0] == 'T') { /* special treatment for text */
            if ((atr = atr_va)) {
               hln = sz[1]/sz[2]; /* height of one line, recalculated!*/
               Ex = (1-sz[2])*hln;  /* shift for text of more than one line */
               if (atr[0] == 'c' || atr[0] == 'C') { /* center */
                  txloc[1] = txloc[1] * 
                              (1.0 - ( ((float)hln/2.0 + Ex)  / 
                                       (float)viewport[3]));
               } else if (atr[0] == 't' || atr[0] == 'T') { /* top */
                  txloc[1] = txloc[1] *
                              (1.0 - ((float)hln / (float)viewport[3]));
               } else if ( sz[2] && /* needs work for multiline case*/
                     (atr[0] == 'b' || atr[0] == 'B')) { 
                  txloc[1] = txloc[1] - ( Ex / (float)viewport[3]);
               }
            }
         } else {
            if ((atr = atr_va)) {
               if (atr[0] == 'c' || atr[0] == 'C') { /* center */
                  txloc[1] = txloc[1] - ((float)sz[1]/2.0 / (float)viewport[3]);
               } else if (atr[0] == 't' || atr[0] == 'T') { /* top */
                  txloc[1] = txloc[1] - ((float)sz[1] / (float)viewport[3]);
               }
            }
         }
      }
      if (coord_units == SUMA_NORM_SCREEN_UNIT) {
         SUMA_LHv("initial coords: [%f %f %f]\n",
                  txloc[0], txloc[1], txloc[2]);
         for (id=0; id<3;++id) {
            if (txloc[id] >= 0.999) txloc[id] = 0.999;
               /*else it can get clipped*/
            else if (txloc[id] <= 0.001) txloc[id] = 0.001;
         }         
         if (!sv->ortho) {/*  for screen-based drawing, 
                              you do not want perspective. But no need to do 
                              this if in ortho mode already since for this
                              type of coordinate (SUMA_SCREEN) drawing
                              is done before any translation and rotation.
                              All one needs is to be sure projection is
                              orthographic.
                              It is not efficient to keep changing this 
                              for each text being displayed. But I do not expect
                              a lot of those to be displayed. Will see ...
                              I could make the calling function send a list of 
                              nels that ought to be displayed with the same
                              projection settings. */
            glPushMatrix();/* start fresh to avoid messing up current setting */
            *orthoreset = 1; /* keep track in order to pop matrix later */
            SUMA_SET_GL_PROJECTION(sv, 1);
         }
         
         SUMA_NormScreenToWorld(NULL, (double)txloc[0], (double)txloc[1],
                             pfront, pback, 0);
         
         txloc[0] = (float)( pfront[0] + 
                            (double)txloc[0]*( pback[0] - pfront[0] ) );
         txloc[1] = (float)( pfront[1] + 
                            (double)txloc[1]*( pback[1] - pfront[1] ) );
         txloc[2] = (float)( pfront[2] + 
                            (double)txloc[2]*( pback[2] - pfront[2] ) );
         
         SUMA_LHv("pfront coords: [%f %f %f]\n"
                  "pback  coords: [%f %f %f]\n"
                  "txloc  coords: [%f %f %f]\n",
                  pfront[0], pfront[1], pfront[2],
                  pback[0], pback[1], pback[2],
                  txloc[0], txloc[1], txloc[2]);
         
         /* do the same if you have a list of texture image coordinates */
         if (texcoord) {
            for (id=0; id<N_texcoord; ++id) {
               id3=3*id;
               for (k=0;k<3;++k) {
                  if       (texcoord[id3+k] > 0.999) 
                     texcoord[id3+k] = 0.999;
                  else if  (texcoord[id3+k] < 0.001) 
                     texcoord[id3+k] = 0.001;
               }
               SUMA_NormScreenToWorld( NULL, 
                                       (double)texcoord[id3  ], 
                                       (double)texcoord[id3+1],
                                       pfront, pback, 0);
               texcoord[id3  ]= (float)( pfront[0] + 
                            (double)texcoord[id3  ]*(pback[0] - pfront[0]) );
               texcoord[id3+1]= (float)( pfront[1] + 
                            (double)texcoord[id3+1]*(pback[1] - pfront[1]) );
               texcoord[id3+2]= (float)( pfront[2] + 
                            (double)texcoord[id3+2]*(pback[2] - pfront[2]) );
            }
         }
      }
   }
   SUMA_RETURN(YUP);
}


/*!
   See also SUMA_TextBoxSize 
*/
int *SUMA_NIDOtext_LineWidth(char *string, void *font, int *N_lines) 
{
   int is = 0, il=0, *iwidth=NULL;
   int Dx=0;
   
   if (N_lines) *N_lines = 0;
   if (!font || !string || !N_lines) return(NULL);
   
   for (is=0; string && string[is] != '\0'; is++) {
      if (string[is] == '\n') *N_lines=*N_lines+1;
   }
   if (is > 0) *N_lines=*N_lines+1;
   if (*N_lines) {
      iwidth = (int *)SUMA_calloc(*N_lines, sizeof(int));
      Dx = 0; il=0;
      for (is=0; string[is] != '\0'; is++) {
         if (string[is] == '\n') {
            iwidth[il] = Dx;
            /*fprintf(stderr,"ZSS: line[%d]=%d\n", il, iwidth[il]);*/
            Dx = 0; ++il;
         } else {
            Dx = Dx+glutBitmapWidth(font, string[is]);
         }
      }
      if (is > 0) {
         iwidth[il] = Dx;
         /*fprintf(stderr,"ZSS: line[%d]=%d\n", il, iwidth[il]);*/
      }
   }
   return(iwidth);
}

SUMA_Boolean SUMA_DrawTextNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *default_SO,
                                    SUMA_DO_CoordUnits default_coord_units,
                                    float *default_color, 
                                    void *default_font, int default_node,
                                    SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_DrawTextNIDOnel"};
   char  *string=NULL, *atr=NULL;
   GLfloat rpos[4];
   float Dx = 0.0;
   void *font=NULL;
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   float txloc[3] = {0.0, 0.0, 0.0}, xyzoffset[3]={0.0, 0.0, 0.0};
   GLfloat txcol[4];
   GLboolean valid;
   int orthoreset = 0, il=0, *lwidth=NULL, N_lines=0;
   int id=0, is = 0, sz[3]={0, 0,0}, newlineopen=0;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_DO_CoordUnits coord_units = default_coord_units;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LHv("Called %p\n", nel);
   
   if (!nel || strcmp(nel->name,"T")) SUMA_RETURN(NOPE); 

   SUMA_LHv(  "default_coord_units %d\n", default_coord_units);

   string = NI_get_attribute(nel,"text");  
   if (!string || string[0]=='\0') {
      /* nothing to write */
      SUMA_RETURN(YUP);
   }
   if ((atr = NI_get_attribute(nel, "font"))) {
      if (!(font = SUMA_glutBitmapFont(atr))) {
         SUMA_S_Errv("Bad font %s, using default for group\n", 
                     atr);
         font = default_font;
      }
   } else {
      font = default_font;
   }
   
   NI_GET_FLOATv(nel,"col", txcol, 4, LocalHead);
   
   if (!NI_GOT) {
      txcol[0] = default_color[0];
      txcol[1] = default_color[1];
      txcol[2] = default_color[2];
      txcol[3] = default_color[3];
   }
   
   /* get the width of each line. Note redundancy with TextBoxSize. */
   if (!(lwidth = SUMA_NIDOtext_LineWidth(string, font, &N_lines))) {
      SUMA_S_Warn("Could not get linewidths\n");
   }
   
   /* has the box size been determined , 3rd dim is number of lines */
   NI_GET_INTv(nel,"box_size", sz, 3, LocalHead);
   if (!NI_GOT) {
      SUMA_TextBoxSize(NI_get_attribute(nel,"text"), sz, sz+1, sz+2, font);
      NI_SET_INTv(nel,"box_size", sz, 3);
   }
   /* set up projection conditions and coordinates */
   if (!SUMA_PrepForNIDOnelPlacement(sv, nel, default_SO, default_node, 
                                     txloc, NULL, sz, 
                                     &orthoreset, coord_units, xyzoffset,
                                     lwidth)) {
      SUMA_RETURN(NOPE);
   }    

   glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
   glMaterialfv(GL_FRONT, GL_EMISSION, txcol); 
      /*turn on emissivity for text*/
   
   /* Recall: This next call will still subject txloc to the projection
      and the modelview matrices! */   
   glRasterPos3f( txloc[0], txloc[1], txloc[2]);
   glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
   glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
   SUMA_LHv("%s: Raster position (%.3f,%.3f, %3f) is %s\n",
            FuncName, rpos[0], rpos[1], rpos[2], 
            valid ? "valid" : "INVALID");

   /* do some text action */
   glColor3fv(txcol);
   SUMA_LHv(  "text:\n"
              ">>>%s<<<\n", string);

   /* The first line is not properly centered, that should be done
      in the PrepForNIDO placement function using lwidth */
   il=0;Dx = 0; 
   for (is=0; string && string[is] != '\0'; is++) {
      if (string[is] == '\n') {
         if (lwidth) { /* use precomputed distance 
			 Problems still occur when going across states, don't know why yet */
            Dx = lwidth[il];
            if (xyzoffset[0]==0.5 && il<N_lines-1) { /* center next line too */
               Dx = (float)(lwidth[il]+lwidth[il+1])/2.0;
            }
            /*fprintf(stderr,"lwidth[%d]=%d, Dx=%f", il, lwidth[il], Dx);*/
         }  
         glBitmap( 0, 0, 0, 0, 
               -(float)Dx, -(float) SUMA_glutBitmapFontHeight(font), 
                  NULL );
         Dx = 0; ++il;
         newlineopen=0;
      } else {
         newlineopen=1;
         glutBitmapCharacter(font, string[is]);
         if (!lwidth) Dx = Dx+glutBitmapWidth(font, string[is]);
      }
   }
   #if 0 /* Reset position after last line if it did not
            end with \n . Useless for now, but kept here just in case */
   if (newlineopen) {
      glBitmap( 0, 0, 0, 0, 
               -(float)Dx, -(float) SUMA_glutBitmapFontHeight(font), 
                  NULL );
   }
   #endif
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
      /*turn off emissidity for text*/   

   if (orthoreset) {/* return value */
      /* and just pop what you'd pushed in */
      glPopMatrix();
   }
   
   if (lwidth) SUMA_free(lwidth); lwidth=NULL;
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_DrawSphereNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *default_SO,
                                    SUMA_DO_CoordUnits default_coord_units,
                                    float *default_color, int default_node, 
                                    SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_DrawSphereNIDOnel"};
   char  *string=NULL, *atr=NULL;
   GLfloat rpos[4];
   float Dx = 0.0;
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   float txloc[3] = {0.0, 0.0, 0.0};
   GLfloat txcol[4];
   GLfloat rad = 0.0;
   int orthoreset = 0;
   int id=0, is = 0, sz[3]={0, 0,0}, slices, stacks;
   GLfloat origwidth=0.0, LineWidth=2.0;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_DO_CoordUnits coord_units = default_coord_units;
   static GLUquadricObj *sphobj=NULL;
   GLenum style=GLU_FILL;
   byte AmbDiff = 0; /* Do not turn on ambient and diffuse coloring */
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY; 

   if (!nel || strcmp(nel->name,"S")) SUMA_RETURN(NOPE); 

   SUMA_LHv(  "default_coord_units %d\n", default_coord_units);
   
   if (!sphobj) sphobj = gluNewQuadric(); /* This is created once 
                                             BUT NEVER freed. So technically
                                             it is a memory leak */
   
       
   NI_GET_FLOATv(nel,"col", txcol, 4, LocalHead);
   
   if (!NI_GOT) {
      txcol[0] = default_color[0];
      txcol[1] = default_color[1];
      txcol[2] = default_color[2];
      txcol[3] = default_color[3];
   } else {
      SUMA_LH("Have own color");
   }
   
   /* set up projection conditions and coordinates */
   if (!SUMA_PrepForNIDOnelPlacement(sv, nel, default_SO, default_node, 
                                     txloc, NULL, sz, 
                                     &orthoreset, coord_units, NULL, NULL)) {
      SUMA_RETURN(NOPE);
   }    

   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   NI_GET_FLOAT(nel,"line_width", LineWidth);
   if (!NI_GOT) LineWidth = 2;
   glLineWidth(LineWidth);
      
   if (AmbDiff) { /* If this is on, a sphere's color would not match well
                     because the mixing of ambient light. Keeping it off
                     makes the colors much more consistent.
                     Modified thanks to complaint by MSB */
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, txcol);
   }  else { /* lights out */
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor); 
   }
   glMaterialfv(GL_FRONT, GL_EMISSION, txcol);
   glColor3fv(txcol); 
   
   SO=default_SO;
   NI_GET_FLOAT(nel,"rad", rad);
   if (!NI_GOT) {
      NI_GET_FLOAT(nel,"rad.ef", rad);
      if (!NI_GOT) {
         rad = 10;
      } else {
         if (!SO) {
            SUMA_S_Err("Have no surface from which to get avg edge length");
            SUMA_RETURN(NOPE); 
         }
         if (!SO->EL) {
            if (!SUMA_SurfaceMetrics(SO, "EdgeList", NULL)){
               SUMA_S_Err("Failed to create EdgeList. Can't set radius");
               SUMA_RETURN(NOPE); 
            }
         }

         rad = rad*SO->EL->AvgLe;
      }
   }

   NI_GET_INT(nel,"slices", slices);
   if (!NI_GOT) slices = 10;
   NI_GET_INT(nel,"stacks", stacks);
   if (!NI_GOT) stacks = 10;

   style = SUMA_NIDO_QuadricStyle(nel);
   gluQuadricDrawStyle (sphobj, style);
   if (style == GLU_FILL) 
      gluQuadricNormals (sphobj , GLU_SMOOTH);
   else
      gluQuadricNormals (sphobj , GLU_NONE);
   
   glTranslatef (txloc[0], txloc[1], txloc[2]);
   gluSphere(sphobj, rad/* *SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06)  
                  User set values, not cool to play with dimensions! */, 
             slices, stacks);
   glTranslatef (-txloc[0], -txloc[1], -txloc[2]);
   
   glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor); 
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
   glLineWidth(origwidth);

   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
      /*turn off emissidity */   

   if (orthoreset) {/* return value */
      /* and just pop what you'd pushed in */
      glPopMatrix();
   }
   
   SUMA_RETURN(YUP);
}

SUMA_SurfaceObject *SUMA_Default_SO_4_NIDO(SUMA_NIDO *SDO, 
                                           SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Default_SO_4_NIDO"};
   char *atr=NULL, *SOid=NULL;
   SUMA_SurfaceObject *default_SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SDO) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   
   /* setup defaults based on group settings */
   default_SO = NULL;
   /* is there a surface in question or could we find a crutch? */
   default_SO = SUMA_findSOp_inDOv(
               NI_get_attribute(SDO->ngr, "Parent_idcode_str"), 
               SUMAg_DOv, SUMAg_N_DOv);
   SUMA_LHv("default_SO is now %p\n", default_SO);
   if (!default_SO) { /* keep trying */
      if ((atr = NI_get_attribute(SDO->ngr,"default_SO_label"))) {
         if (!strcmp(atr,"CURRENT")) { 
            SUMA_LH("Getting current SO");
            default_SO = SUMA_SV_Focus_SO(sv);
            /* last ditch */
            if (!default_SO) {
               SUMA_LH("No current SO, trying any");
               default_SO = SUMA_findanySOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, NULL);
            }
            if (!default_SO) {
               SUMA_S_Err("Could not find any surface to work with \n");
               SUMA_RETURN (NOPE);
            }
         } else {
            char *SOid = SUMA_find_SOidcode_from_label(
                              atr, SUMAg_DOv, SUMAg_N_DOv);
            if (SOid) {
               default_SO = SUMA_findSOp_inDOv (SOid, SUMAg_DOv, SUMAg_N_DOv);
            }
            if (!default_SO) {
               SUMA_S_Errv("Could not find surface labeled %s\n", atr);
               SUMA_RETURN (NOPE);
            }
         }
      }  else {
         SUMA_LH("No default_label, finding Any...");
         default_SO = SUMA_findanySOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, NULL);
         /* OK if you fail here ... */
      } 
   }
   
   SUMA_LHv("Default_SO %p\n", default_SO);
   
   SUMA_RETURN(default_SO);
}

SUMA_Boolean SUMA_DrawNIDO (SUMA_NIDO *SDO, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawNIDO"};
   int ip=0;
   int is, default_node=-1;
   byte *msk=NULL;
   SUMA_SurfaceObject *default_SO = NULL;
   NI_element *nel=NULL;
   NI_group *ngr = NULL;
   void * default_font=GLUT_BITMAP_9_BY_15;
   float txcol[4] = {0.2, 0.5, 1, 1.0};
   float default_color[4] = {0.2, 0.5, 1, 1.0};
   SUMA_DO_CoordType coord_type = SUMA_WORLD;
   SUMA_DO_CoordUnits default_coord_units = SUMA_WORLD_UNIT;
   char *atr=NULL;
   static int iwarn=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SDO) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (sv && sv->DO_PickMode) {
      if (SUMA_ADO_isLabel((SUMA_ALL_DO *)SDO,"AHorseWithNoName")) {
         /* viewer label, do not complain */
         SUMA_RETURN(YUP);
      } else {
         SUMA_S_Warnv(
            "Function not ready for picking mode on '%s', should be fixed.\n",
            SUMA_ADO_sLabel((SUMA_ALL_DO *)SDO));
         SUMA_RETURN(YUP);
      }
   }
   

   default_SO = SUMA_Default_SO_4_NIDO(SDO, sv);
   
   if (SUMA_isNIDO_SurfBased(SDO) && !default_SO) {
         SUMA_SL_Err("Object's parent surface not found.");
         SUMA_RETURN (NOPE);
   } 
   
   ngr = SDO->ngr;
   {
      SUMA_LH("Setting up other nido defaults:");
      
      /* set up group defaults */
         if ((atr = NI_get_attribute(ngr, "default_font"))) {
            if (!(default_font = SUMA_glutBitmapFont(atr))) {
               SUMA_S_Errv("Bad font %s, using default %s", 
                           atr,
                  SUMA_glutBitmapFontName(SUMA_glutBitmapFont(NULL)));
               default_font=SUMA_glutBitmapFont(NULL);
            }
         }
         NI_GET_INT(ngr,"default_node", default_node);
         if (!NI_GOT) default_node = -1;
         
         NI_GET_FLOATv(ngr,"default_color", txcol, 4,LocalHead);
         if (NI_GOT) {
            default_color[0] = txcol[0];
            default_color[1] = txcol[1]; 
            default_color[2] = txcol[2]; 
            default_color[3] = txcol[3]; 
            SUMA_LHv("default_color: [%f %f %f %f]\n",
                     default_color[0], default_color[1], 
                     default_color[2], default_color[3]);
         }
         if ((atr = NI_get_attribute(ngr, "coord_type"))) {
            if ((coord_type = SUMA_CoordType(atr))
                 == SUMA_COORD_TYPE_ERROR) {
               SUMA_S_Errv("Bad coord_type %s,"
                           "using default %s",
                           atr, 
                           SUMA_CoordTypeName(SUMA_CoordType(NULL)));
               coord_type = SUMA_CoordType(NULL);
            }
         }
         /* Now set default coordinate units */
         if (coord_type == SUMA_WORLD) {
            /* if users want object to move, then odds ar
            coords are in world units (rotating objects units */ 
            default_coord_units = SUMA_WORLD_UNIT;
         } else if (coord_type == SUMA_SCREEN) {
            /* If users want object to remain fixed relative to
            screen, then odds are coords are in normalized screen
            units */
            default_coord_units = SUMA_NORM_SCREEN_UNIT;
         }
         SUMA_LHv("Have coord_type %d and default_coord_units %d\n"
                  "default_color = %f %f %f %f\n"
                  "Group has %d parts in it.\n",
                coord_type, default_coord_units,
                default_color[0], default_color[1],
                default_color[2], default_color[3],
                SDO->ngr->part_num);  
   }

   /* loop through the elements */
   for( ip=0 ; ip < SDO->ngr->part_num ; ip++ ){ 
      nel = NULL;
      switch( SDO->ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)SDO->ngr->part[ip] ;
            if (0 && LocalHead)  {
               SUMA_ShowNel(nel);
            }
            break;
         default:
            SUMA_S_Errv(
               "Don't know what to make of this group element %d, ignoring.",
               SDO->ngr->part_typ[ip]);
            if (LocalHead || !iwarn) {SUMA_ShowNel(SDO->ngr); ++iwarn;}
            break;
      }
      if (nel) { /* something to display */
         SUMA_LHv("Attempting to draw %s\n", nel->name);
         if (! strcmp(nel->name,"T")) {
            if (!SUMA_DrawTextNIDOnel( nel, default_SO, 
                                       default_coord_units,
                                       default_color, 
                                       default_font, default_node,
                                       sv)) {
               SUMA_S_Warnv("Failed to draw %s\n", nel->name);
            }
         } else if (! strcmp(nel->name,"S")) {
            if (!SUMA_DrawSphereNIDOnel( nel, default_SO, 
                                       default_coord_units,
                                       default_color,  default_node,
                                       sv)) {
               SUMA_S_Warnv("Failed to draw %s\n", nel->name);
            }
         } else if (! strcmp(nel->name,"I")) {
            if (!SUMA_DrawImageNIDOnel(nel, default_SO, 
                                       default_coord_units,
                                       default_color, 
                                       default_font, default_node,
                                       sv)) {
               SUMA_S_Warnv("Failed to draw %s\n", nel->name);
            }
         } else if (! strcmp(nel->name,"Tex")) {
            if (!SUMA_DrawTextureNIDOnel(nel, default_SO, 
                                       default_coord_units,
                                       default_color, 
                                       default_font, default_node,
                                       sv)) {
               SUMA_S_Warnv("Failed to draw %s\n", nel->name);
            }
         } else if (! strcmp(nel->name,"3DTex")) {
            if (SUMAg_CF->Dev) {
               SUMA_LH("Going to draw 3DTexture");
               if (!SUMA_Draw3DTextureNIDOnel(nel, default_SO, 
                                          default_coord_units,
                                          default_color, 
                                          default_font, default_node,
                                          sv)) {
                  SUMA_S_Warnv("Failed to draw %s\n", nel->name);
               }
               SUMA_LH("Done drawing 3DTexture");
            } else {
               SUMA_S_Note("3DTex available in developer mode "
                           "(suma -dev) mode only.\n");
            }
         } else if (strcmp(nel->name,"nido_head")) {
            if (nel->name[0] != '#') {
               SUMA_S_Errv("Not ready for nel->name %s\n", nel->name);
            } else {
               if (LocalHead) 
                  SUMA_LHv("Skipping element %s\n", nel->name);  
            }
         }      
      }
      
   }
   
     
   SUMA_RETURN (YUP);
   
}


/*!
   A macro to be inserted into SUMA_SortedAxisSegmentList's switch statement
*/
#define SUMA_SORTED_AXIS_WORKS { \
            ASIp->Quad[0] = Q[ASIp->PointIndex[0]];   \
            ASIp->Quad[1] = Q[ASIp->PointIndex[1]];   \
            for (i=0;i<3;++i) d[i] = \
               ( P[ASIp->PointIndex[1]][i] - P[ASIp->PointIndex[0]][i] ); \
            SUMA_NORM_VEC (d, 3, ASIp->world_length); \
            for (i=0; i<3;++i) { /* projection direction along screen */ \
               ASIp->ScreenProj[i] = S[ASIp->PointIndex[1]*3+i] -   \
                                    S[ASIp->PointIndex[0]*3+i];    \
               if (i<2) { /* Don't want to use screen z depth */\
                  ASIp->ScreenProj_xy_length = ASIp->ScreenProj_xy_length + \
                                    ASIp->ScreenProj[i]*ASIp->ScreenProj[i]; \
               }  \
            }  \
            ASIp->ScreenProj_xy_length = sqrt(ASIp->ScreenProj_xy_length);  \
            for (i=0;i<3;++i) d[i] = \
               ( ( P[ASIp->PointIndex[0]][i] + P[ASIp->PointIndex[1]][i] ) \
                        / 2.0 - sv->Pcenter_close[i] ); \
            SUMA_NORM_VEC (d, 3, ASIp->MidSegDist);   \
            for (i=0;i<2;++i) d[i] = ( S[ASIp->PointIndex[0]*3+i] - LLC[i] );  \
            SUMA_NORM_VEC (d, 2, d1);  \
            for (i=0;i<2;++i) d[i] = ( S[ASIp->PointIndex[1]*3+i] - LLC[i] );  \
            SUMA_NORM_VEC (d, 2, d2);  \
            if (d1 < d2) { \
               ASIp->LLCclosestDist = d1; ASIp->LLCclosestPoint = 0; \
            }  \
            else { ASIp->LLCclosestDist = d2; ASIp->LLCclosestPoint = 1; } \
            for (i=0;i<3;++i) d[i] = \
                  ( C[ASIp->FaceIndex[0]][i] - sv->Pcenter_close[i] ); \
            SUMA_NORM_VEC (d, 3, d1);  \
            for (i=0;i<3;++i) d[i] = \
                  ( C[ASIp->FaceIndex[1]][i] - sv->Pcenter_close[i] ); \
            SUMA_NORM_VEC (d, 3, d2); \
            if (d1 < d2) ASIp->MidFaceDist = d1; else ASIp->MidFaceDist = d2;  \
            SUMA_COPY_VEC(P[ASIp->PointIndex[0]], ASIp->P1, 3, double, double); \
            SUMA_COPY_VEC(P[ASIp->PointIndex[1]], ASIp->P2, 3, double, double); \
            ASIp->TxOff[0] = (-ASIp->tick2_dir[0] - ASIp->tick1_dir[0]);   \
            ASIp->TxOff[1] = (-ASIp->tick2_dir[1] - ASIp->tick1_dir[1]);   \
            ASIp->TxOff[2] = (-ASIp->tick2_dir[2] - ASIp->tick1_dir[2]);   \
            SUMA_NORM_VEC (ASIp->TxOff, 3, d1); /* DON'T USE /= in next line! */\
            ASIp->TxOff[0] = ASIp->TxOff[0] / d1; \
            ASIp->TxOff[1] = ASIp->TxOff[1] / d1; \
            ASIp->TxOff[2] = ASIp->TxOff[2] / d1; \
}


/*!
   
   \param opt (SUMA_SORT_BOX_AXIS_OPTION) Specifies how sorting of segments is done in the list.
   
   Creates the various segments needed to form a box axis.
   
   - Your job is to free DList's elements and destroy and free DList after the function returns.
   
   \sa Labbook NIH-4 page 21 for annotation of Box Axis ....
*/
DList *SUMA_SortedAxisSegmentList (SUMA_SurfaceViewer *sv, 
                                   SUMA_Axis *Ax, SUMA_SORT_BOX_AXIS_OPTION opt)
{
   static char FuncName[]={"SUMA_SortedAxisSegmentList"};
   double P[8][3], C[6][3], d[3], d1, d2, S[24], 
          world_length;
   int Q[8];
   static double xAx[3] = {1, 0, 0}, yAx[3] = { 0, 1, 0 }, zAx[3] = {0, 0, 1}, 
                 LLC[3] = {0, 0, 0};
   static double mxAx[3] = {-1, 0, 0}, myAx[3] = { 0, -1, 0 }, 
                 mzAx[3] = {0, 0, -1};
   DList *list = NULL;
   DListElmt *Elm = NULL;
   SUMA_Boolean Found = NOPE;
   SUMA_AxisSegmentInfo **ASI = NULL, *ASIp=NULL, *ASIptmp=NULL;
   int i=0, j=0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   LLC[1] = (double)sv->WindHeight;
   if (Ax->atype != SUMA_SCALE_BOX) {
      SUMA_S_Err("Nothing to be done here.\nFor Scale Box type axis only.");
      SUMA_RETURN(NULL);
   }
   
   /* form box corner points */
   SUMA_BOX_CORNER_POINTS_FROM_AXIS(Ax, P);
   
   /* figure out equivalent screen coords */
   SUMA_World2ScreenCoords (sv, 8, (double *)P, S, Q, 0, YUP);
   
   /* form plane centers */
   for (i=0; i<3; ++i) { /* Plane a, b, f, e*/
      C[0][i] = ( P[0][i] + P[1][i] + P[5][i] + P[4][i] ) / 4.0; 
   } 
   for (i=0; i<3; ++i) {  /* Plane a, b, d, c*/
      C[1][i] = ( P[0][i] + P[1][i] + P[3][i] + P[2][i] ) / 4.0; 
   }
   for (i=0; i<3; ++i) {  /* Plane a, c, g, e*/
      C[2][i] = ( P[0][i] + P[2][i] + P[6][i] + P[4][i] ) / 4.0; 
   }
   for (i=0; i<3; ++i) {  /* Plane e, f, h, g*/
      C[3][i] = ( P[4][i] + P[5][i] + P[7][i] + P[6][i] ) / 4.0; 
   }
   for (i=0; i<3; ++i) {  /* Plane b, d, h, f*/
      C[4][i] = ( P[1][i] + P[3][i] + P[7][i] + P[5][i] ) / 4.0; 
   }
   for (i=0; i<3; ++i) {  /* Plane c, d, h, g*/
      C[5][i] = ( P[2][i] + P[3][i] + P[7][i] + P[6][i] ) / 4.0; 
   }
  
   /* for (i=0; i<3; ++i) sv->Ch->c[i] = sv->Plist_close[i];  */
   if (LocalHead) { 
      fprintf (SUMA_STDERR,"%s: sv->Pcenter_close = [%f %f %f]\n", 
                           FuncName, sv->Pcenter_close[0], sv->Pcenter_close[1],
                           sv->Pcenter_close[2]); }
   ASI = (SUMA_AxisSegmentInfo **) 
      SUMA_calloc(12, sizeof(SUMA_AxisSegmentInfo *));

   for (j=0; j<12; ++j) {
      ASI[j] = (SUMA_AxisSegmentInfo *)
                  SUMA_calloc(1,sizeof(SUMA_AxisSegmentInfo )); 
      ASIp = ASI[j];
      ASIp->SegIndex = j;
      switch (j) {
         case 0: /* seg, 1 */
            ASIp->AxisDim = 0; /* X axis */
            ASIp->PointIndex[0] = 0; /* a */
            ASIp->PointIndex[1] = 1; /* b */
            ASIp->FaceIndex[0] = 0; /* Plane a, b, f, e */
            ASIp->FaceIndex[1] = 1; /* Plane a, b, d, c */
            SUMA_COPY_VEC(zAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(yAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 1: /* Seg 2 */
            ASIp->AxisDim = 0;  /* X axis */
            ASIp->PointIndex[0] = 2; /* c */
            ASIp->PointIndex[1] = 3; /* d */
            ASIp->FaceIndex[0] = 1; /* Plane a, b, d, c */
            ASIp->FaceIndex[1] = 5; /* Plane c, d, h, g */
            SUMA_COPY_VEC(zAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(myAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 2: /* Seg 3 */
            ASIp->AxisDim = 0;  /* X axis */
            ASIp->PointIndex[0] = 4; /* e */
            ASIp->PointIndex[1] = 5; /* f */
            ASIp->FaceIndex[0] = 0; /* Plane a, b, f, e*/
            ASIp->FaceIndex[1] = 3; /* Plane e, f, h, g*/
            SUMA_COPY_VEC(mzAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(yAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 3: /* Seg 4 */ 
            ASIp->AxisDim = 0;  /* X axis */
            ASIp->PointIndex[0] = 6; /* g */
            ASIp->PointIndex[1] = 7; /* h */
            ASIp->FaceIndex[0] = 5; /* Plane c, d, h, g*/
            ASIp->FaceIndex[1] = 3; /* Plane e, f, h, g*/
            SUMA_COPY_VEC(mzAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(myAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 4: /* seg 5*/ 
            ASIp->AxisDim = 1; /* Y axis */
            ASIp->PointIndex[0] = 0; /* a */
            ASIp->PointIndex[1] = 2; /* c */
            ASIp->FaceIndex[0] = 1; /* Plane a, b, d, c*/
            ASIp->FaceIndex[1] = 2; /* Plane a, c, g, e*/
            SUMA_COPY_VEC(xAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(zAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 5: /* seg 6*/ 
            ASIp->AxisDim = 1; /* Y axis */ 
            ASIp->PointIndex[0] = 1; /* b */
            ASIp->PointIndex[1] = 3; /* d */
            ASIp->FaceIndex[0] = 1; /* Plane a, b, d, c*/
            ASIp->FaceIndex[1] = 4; /* Plane b, d, h, f*/
            SUMA_COPY_VEC(mxAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(zAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 6: /* seg 7*/ 
            ASIp->AxisDim = 1; /* Y axis */ 
            ASIp->PointIndex[0] = 4; /* e */
            ASIp->PointIndex[1] = 6; /* g */
            ASIp->FaceIndex[0] = 2; /* Plane a, c, g, e*/
            ASIp->FaceIndex[1] = 3; /* Plane e, f, h, g*/
            SUMA_COPY_VEC(xAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(mzAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 7: /* seg 8*/ 
            ASIp->AxisDim = 1; /* Y axis */ 
            ASIp->PointIndex[0] = 5; /* f */
            ASIp->PointIndex[1] = 7; /* h */
            ASIp->FaceIndex[0] = 3; /* Plane e, f, h, g*/
            ASIp->FaceIndex[1] = 4; /* Plane b, d, h, f*/
            SUMA_COPY_VEC(mxAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(mzAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 8: /* seg 9*/ 
            ASIp->AxisDim = 2; /* Z axis */ 
            ASIp->PointIndex[0] = 0; /* a */
            ASIp->PointIndex[1] = 4; /* e */
            ASIp->FaceIndex[0] = 0; /* Plane a, b, f, e */
            ASIp->FaceIndex[1] = 2; /* Plane a, c, g, e */
            SUMA_COPY_VEC(xAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(yAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 9: /* seg 10*/ 
            ASIp->AxisDim = 2; /* Z axis */ 
            ASIp->PointIndex[0] = 1; /* b */
            ASIp->PointIndex[1] = 5; /* f */
            ASIp->FaceIndex[0] = 0; /* Plane a, b, f, e*/
            ASIp->FaceIndex[1] = 4; /* Plane b, d, h, f*/
            SUMA_COPY_VEC(mxAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(yAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 10: /* seg 11*/ 
            ASIp->AxisDim = 2; /* Z axis */ 
            ASIp->PointIndex[0] = 2; /* c */
            ASIp->PointIndex[1] = 6; /* g */
            ASIp->FaceIndex[0] = 5; /* Plane c, d, h, g*/
            ASIp->FaceIndex[1] = 2; /* Plane a, c, g, e*/
            SUMA_COPY_VEC(xAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(myAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
         case 11: /* seg 12*/ 
            ASIp->AxisDim = 2; /* Z axis */ 
            ASIp->PointIndex[0] = 3; /* d */
            ASIp->PointIndex[1] = 7; /* h */
            ASIp->FaceIndex[0] = 5; /* Plane c, d, h, g*/
            ASIp->FaceIndex[1] = 4; /* Plane b, d, h, f*/
            SUMA_COPY_VEC(mxAx, ASIp->tick1_dir, 3, double, double);
            SUMA_COPY_VEC(myAx, ASIp->tick2_dir, 3, double, double);
            SUMA_SORTED_AXIS_WORKS;
            break;
      }
   }

   list = (DList *)SUMA_calloc(1,sizeof(DList));
   dlist_init(list, NULL);
   for (i=0; i<12; ++i) {
      ASIp = ASI[i];
      if (!list->size) {
         dlist_ins_next(list, dlist_tail(list), (void*)ASIp);
      } else {
         Elm = NULL;
         do {
            Found = NOPE;
            if (!Elm) {
               Elm = dlist_head(list);
            } else {
               Elm = dlist_next(Elm);
            }
            
            ASIptmp = (SUMA_AxisSegmentInfo *)Elm->data;
               switch (opt) {
                  case SUMA_BY_SEGMENT_DISTANCE:
                     if (ASIp->MidSegDist < ASIptmp->MidSegDist) {
                        dlist_ins_prev(list, Elm, (void *)ASIp);
                        Found = YUP;
                     }
                     break;
                  case SUMA_BY_PLANE_DISTANCE:
                     if (ASIp->MidFaceDist < ASIptmp->MidFaceDist) {
                        dlist_ins_prev(list, Elm, (void *)ASIp);
                        Found = YUP;
                     }
                     break;
                  case SUMA_SORT_BY_LLC_DISTANCE:
                     if (ASIp->LLCclosestDist < ASIptmp->LLCclosestDist) {
                        dlist_ins_prev(list, Elm, (void *)ASIp);
                        Found = YUP;
                     }
                     break;
                  case SUMA_SORT_BY_LL_QUAD:
                     if (ASIp->Quad[0] == SUMA_LOWER_LEFT_SCREEN || 
                         ASIp->Quad[1] == SUMA_LOWER_LEFT_SCREEN) {
                        SUMA_LH("Found a LLS");
                        dlist_ins_prev(list, dlist_head(list), (void *)ASIp);
                        Found = YUP;
                     }else {
                        dlist_ins_next(list, dlist_tail(list), (void *)ASIp);
                        Found = YUP;
                     }
                     break;
                  case SUMA_NO_SORT:
                     dlist_ins_next(list, Elm, (void *)ASIp); 
                     Found = YUP;
                     break;
                  default:
                     SUMA_S_Err("Whatchyoutalkingboutwillis?\n"
                                "Bad, bad bad bad.");
                     SUMA_RETURN(NULL);
               }
               if (!Found && Elm == dlist_tail(list)) {
                  dlist_ins_next(list, Elm, (void *)ASIp); 
                  Found = YUP;
               }
            } while (!Found); 
      }
      
   }
   if (LocalHead) {
      Elm = NULL;
      i = 0;
      fprintf (SUMA_STDERR,"%s: Sorting by %d order\n", FuncName, opt);
      do {
         if (!Elm) {
            Elm = dlist_head(list);
         } else {
            Elm = dlist_next(Elm);
         }
         ASIp = (SUMA_AxisSegmentInfo *)Elm->data;
         if (LocalHead) fprintf (SUMA_STDERR,"%s:\ni %d\ttype %d\tMidSegDist %f\tMidFaceDist %f\tQuads[%d, %d]\t world_length, screen_length_x, screen_length_y = [%.2f %.2f %.2f]\n", 
                     FuncName, i, ASIp->AxisDim, ASIp->MidSegDist, ASIp->MidFaceDist, ASIp->Quad[0], ASIp->Quad[1], ASIp->world_length, ASIp->ScreenProj[0], ASIp->ScreenProj[1]);
         ++i;
      } while(Elm != dlist_tail(list));
   }
   SUMA_free(ASI); ASI = NULL;
   SUMA_LH("Returning");
   SUMA_RETURN(list);
}

double SUMA_ScreenProjAngle(SUMA_AxisSegmentInfo *asi0, 
                            SUMA_AxisSegmentInfo *asi1) 
{
   int i=0;
   double u[2], dotdot=0.0;
   if (!asi0 || !asi1) return(dotdot);
   for (i=0; i<2; ++i) {
        dotdot += asi1->ScreenProj[i]/asi1->ScreenProj_xy_length*
                  asi0->ScreenProj[i]/asi0->ScreenProj_xy_length;                   } 
   return(fabs(dotdot));
}

/*!
   \sa Labbook NIH-4 page 21 for annotation of Box Axis ....
*/
SUMA_Boolean SUMA_DrawAxis (SUMA_Axis* Ax, SUMA_SurfaceViewer *sv)
{ 
   static char FuncName[]={"SUMA_DrawAxis"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   double P1[3], P2[3], cP[8][3], SC[12][3], d[12];
   int i, N_Ax, fst=-1, sec=-1, thr=-1;
   DList *slist=NULL;
   DListElmt *Elm=NULL;
   SUMA_AxisSegmentInfo *ASI = NULL;
   SUMA_AxisSegmentInfo *ASIv[3] = { NULL, NULL, NULL };
   SUMA_Boolean ShowTxt[3];
   float coslim = 0.9;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (sv && sv->DO_PickMode) {
      SUMA_LH("DrawAxis not for do picking mode");
      SUMA_RETURN(YUP); /* this condition is never fatal */
   }
   
   if (!Ax) {
      SUMA_SL_Err("Null Axis!");
      SUMA_RETURN(NOPE);
   }
   
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
   
   switch (Ax->atype) {
      case SUMA_STD_ZERO_CENTERED:
         SUMA_LHv("SUMA_STD_ZERO_CENTERED at %f %f %f\n",
                  Ax->Center[0], Ax->Center[1], Ax->Center[2]);
         glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
            /* turn off ambient and diffuse components */
         glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
         
         glMaterialfv(GL_FRONT, GL_EMISSION, Ax->XaxisColor); 
            /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(-Ax->XYZspan[0]+Ax->Center[0], Ax->Center[1], Ax->Center[2]);
         glVertex3f(Ax->XYZspan[0]+Ax->Center[0], Ax->Center[1], Ax->Center[2]); 
         glEnd();
         
         glMaterialfv(GL_FRONT, GL_EMISSION, Ax->YaxisColor); 
            /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ax->Center[0], -Ax->XYZspan[1]+Ax->Center[1], Ax->Center[2]);
         glVertex3f(Ax->Center[0], +Ax->XYZspan[1]+Ax->Center[1], Ax->Center[2]); 
         glEnd();
         
         glMaterialfv(GL_FRONT, GL_EMISSION, Ax->ZaxisColor); 
            /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ax->Center[0], Ax->Center[1], -Ax->XYZspan[2]+Ax->Center[2]);
         glVertex3f(Ax->Center[0], Ax->Center[1], Ax->XYZspan[2]+Ax->Center[2]); 
         glEnd();
         
         glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, NoColor); 
            /*turn off emissivity for axis*/

         break;
      case SUMA_SCALE_BOX:
         /* Sort segments by distance from screen center*/
         slist = SUMA_SortedAxisSegmentList (sv , Ax, SUMA_SORT_BY_LLC_DISTANCE); 
            /* - when using SUMA_BY_PLANE_DISTANCE, it makes sense to show  
               the first 4 segments, but you have no perception of the depth
               - when using SUMA_NO_SORT, show all 12 segments so you'll see 
                 the box
               - The world distance thingy does not quite work because because 
                 the plane you want to hide is the one opposite to the one 
                 closest to your face. That plane is not necessarily the farthest
                 from sv->Pcenter_close, perhaps what you need to do is 
                 define the lower left point of the box and allow for either a 
                 boxed axis or a 3d axis from the lower left corner.... 
               - So I am now using the screen coordinates 
                  SUMA_SORT_BY_LLC_DISTANCE and SUMA_SORT_BY_LL_QUAD,
                 and sorting by SUMA_SORT_BY_LLC_DISTANCE works best. 
                 You can just show the first 3 axis and you're cool, for most 
                 angles.
               - You also need a pixels/mm number to decide on where to put the 
                 text.*/ 

         if (sv->ShowWorldAxis == SUMA_THREE_WAX || 
             sv->ShowWorldAxis == SUMA_THREE_TEXT_WAX) N_Ax = 3;
         else N_Ax = slist->size;
 
         /* go through list first and decide which get text */
         ShowTxt[0]=NOPE; ShowTxt[1]=NOPE; ShowTxt[2]=NOPE;
         if (sv->ShowWorldAxis == SUMA_THREE_TEXT_WAX || 
             sv->ShowWorldAxis == SUMA_BOX_TEXT_WAX) { 
            Elm = dlist_head(slist); i = 0;
            do {
               if (!(ASIv[i]=(SUMA_AxisSegmentInfo *)Elm->data)) {
                  SUMA_S_Errv("Unexpected nullity at i=%d\n",i);
                  SUMA_RETURN(NOPE);
               }
               if (Elm != dlist_tail(slist)) {
                  Elm = dlist_next(Elm); 
               } else {
                  Elm = NULL;
               }
               ++i;
            } while (i < 3 && Elm);
            if (i < 3) {
               SUMA_S_Errv("Unexpected i = %d\n", i);
               SUMA_RETURN(NOPE);
            }
            if (ASIv[0]->ScreenProj_xy_length >= ASIv[1]->ScreenProj_xy_length &&
                ASIv[0]->ScreenProj_xy_length >= ASIv[2]->ScreenProj_xy_length) {
               fst = 0;
               if (ASIv[1]->ScreenProj_xy_length > 
                                          ASIv[2]->ScreenProj_xy_length){ 
                  sec = 1; thr = 2;
               } else {
                  sec = 2; thr = 1;
               }
            } 
            if (ASIv[1]->ScreenProj_xy_length >= ASIv[0]->ScreenProj_xy_length &&
                ASIv[1]->ScreenProj_xy_length >= ASIv[2]->ScreenProj_xy_length) {
               fst = 1; 
               if (ASIv[0]->ScreenProj_xy_length > 
                                          ASIv[2]->ScreenProj_xy_length){ 
                  sec = 0; thr = 2;
               } else {
                  sec = 2; thr = 0;
               }
            }
            if (ASIv[2]->ScreenProj_xy_length >= ASIv[0]->ScreenProj_xy_length &&
                ASIv[2]->ScreenProj_xy_length >= ASIv[1]->ScreenProj_xy_length) {
               fst = 2; 
               if (ASIv[0]->ScreenProj_xy_length > 
                                          ASIv[1]->ScreenProj_xy_length){ 
                  sec = 0; thr = 1;
               } else {
                  sec = 1; thr = 0;
               }
            }   
            /* always show first */
            ShowTxt[fst]=YUP;
            /* if second does not overlap with 1st show it too */
            if (SUMA_ScreenProjAngle(ASIv[fst], ASIv[sec]) < coslim) {
               ShowTxt[sec]=YUP;
            }
            /* if the third does not overlap with the 1st, 
               AND with the second if it is showing */
            SUMA_LHv("Dot 1 3 = %f, Dot 2 3 = %f\n",
               SUMA_ScreenProjAngle(ASIv[fst], ASIv[thr]),
               SUMA_ScreenProjAngle(ASIv[sec], ASIv[thr]));
               
            if (SUMA_ScreenProjAngle(ASIv[fst], ASIv[thr]) < coslim &&
                (!ShowTxt[sec] || 
                  SUMA_ScreenProjAngle(ASIv[sec], ASIv[thr]) < coslim) ) {
               ShowTxt[thr]=YUP;
            }
         }
         SUMA_LHv("fst, sec, thr: %d %d %d\n"
                      "Show flags[0,1,2]   : %d, %d, %d\n", 
                      fst, sec, thr,
                      ShowTxt[0], ShowTxt[1], ShowTxt[2]);
         
         Elm = dlist_head(slist); i = 0;
         do {
            ASI = (SUMA_AxisSegmentInfo *)Elm->data;
            if (ASI->AxisDim == 0) {
               SUMA_LHv("X axis, i = %d, SegIndex = %d, "
                        "world, Sx, Sy = %.2f,%.2f,%.2f, off = %f, %f %f\n", 
                         i, ASI->SegIndex, ASI->world_length, 
                         ASI->ScreenProj[0], ASI->ScreenProj[1], 
                         ASI->TxOff[0], ASI->TxOff[1],ASI->TxOff[2]); 
            } else if (ASI->AxisDim == 1) {
               SUMA_LHv("Y axis, i = %d, SegIndex = %d, "
                        "world, Sx, Sy = %.2f,%.2f,%.2f, off = %f, %f %f\n", 
                        i, ASI->SegIndex, ASI->world_length, 
                        ASI->ScreenProj[0], ASI->ScreenProj[1], 
                        ASI->TxOff[0], ASI->TxOff[1],ASI->TxOff[2]); 
            } else if (ASI->AxisDim == 2) {
               SUMA_LHv("Z axis, i = %d, SegIndex = %d, "
                        "world, Sx, Sy = %.2f,%.2f,%.2f, off = %f, %f %f\n", 
                        i, ASI->SegIndex, ASI->world_length, 
                        ASI->ScreenProj[0], ASI->ScreenProj[1], 
                        ASI->TxOff[0], ASI->TxOff[1],ASI->TxOff[2]); 
            } else { SUMA_S_Err("Major bobo."); SUMA_RETURN(NOPE); }
            
            if (i < 3 && ShowTxt[i]) { 
               SUMA_LHv("Axis %d, ScrLen=[%f %f], ScrVec=[%f %f %f]\n",
                     i, ASI->ScreenProj[0], ASI->ScreenProj[1], 
                     ASI->ScreenProj[0], ASI->ScreenProj[1], ASI->ScreenProj[2]);
                            
               SUMA_DrawLineAxis (ASI, Ax, YUP);
            } else {
               SUMA_DrawLineAxis (ASI, Ax, NOPE);
            }
            SUMA_free(ASI); ASI = NULL; 
            if (Elm != dlist_tail(slist)) {
               Elm = dlist_next(Elm); 
            } else {
               Elm = NULL;
            }
            ++i;
         } while (i < N_Ax && Elm);   
         
         /* destroy list */
         dlist_destroy(slist);SUMA_free(slist); slist = NULL;
         
         break;
      default:
         SUMA_S_Err("Should not be here.");
         SUMA_RETURN(NOPE);
         break;
   }
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
   \brief writes axis text 
*/
SUMA_Boolean SUMA_AxisText(SUMA_AxisSegmentInfo *ASIp, double *Ps)
{
   static char FuncName[]={"SUMA_AxisText"};
   GLboolean valid;
   GLfloat rpos[4];
   char txt[20]={"What the hell?"};
   int is;
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   static float txcol[3] = {1, 1, 1};
   static int width, height;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

      
   glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor); 
   glMaterialfv(GL_FRONT, GL_EMISSION, txcol); /*turn on emissidity for text*/
   glRasterPos3d(Ps[0], Ps[1], Ps[2]);
   glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
   glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
   if (LocalHead) fprintf(SUMA_STDERR, "%s: Raster position (%g,%g, %g) is %s\n",
      FuncName, rpos[0], rpos[1], rpos[2], valid ? "valid" : "INVALID");

   /* do some text action */
   if (valid) {
      glColor3fv(txcol); 
      sprintf(txt,"%.1f", Ps[ASIp->AxisDim]);
      /* sprintf(txt,"%s", MV_format_fval2(Ps[ASIp->AxisDim], 5)); */
      for (is=0; txt[is] != '\0'; is++) {
         glutBitmapCharacter(GLUT_BITMAP_9_BY_15, txt[is]);
      }  
   }
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);  
      /*turn off emissidity for text*/ 
      
   SUMA_RETURN(YUP);
}


/*! 
   \brief glutBitmapLength does not work, so here is a local version
   string length in pixels.
*/
int SUMA_glutBitmapLength(void *font, char *txt, char *txte)
{
   int l=0;
   if (!txt) return(0);
   if (!txte) txte=txt+strlen(txt);
   for (; *txt!='\0' && txt < txte; ++txt) {
      l = l + glutBitmapWidth(font, *txt);
   }
   return (l);
}

/*!
   \brief Font height in pixels 
*/
int SUMA_glutBitmapFontHeight(void *font) 
{
   if (!font) return(0);
   if (font == GLUT_BITMAP_9_BY_15)
         return(15);
   else if (font == GLUT_BITMAP_8_BY_13)
         return(13);
   else if (font == GLUT_BITMAP_TIMES_ROMAN_10)
         return(10);
   else if (font == GLUT_BITMAP_TIMES_ROMAN_24)
         return(24);
   else if (font == GLUT_BITMAP_HELVETICA_10)
         return(10);
   else if (font == GLUT_BITMAP_HELVETICA_12)
         return(12);
   else if (font == GLUT_BITMAP_HELVETICA_18)
         return(18);
   else
         return(0);
   
}

void * SUMA_glutBitmapFont(char *fontname)
{
   if (!fontname) return(GLUT_BITMAP_9_BY_15); /* default*/
   else if (strcmp(fontname,"f8") == 0 ||
       strcmp(fontname,"font8") == 0 ) return(GLUT_BITMAP_8_BY_13);
   else if (strcmp(fontname,"f9") == 0 ||
       strcmp(fontname,"font9") == 0 ) return(GLUT_BITMAP_9_BY_15);
   else if (strcmp(fontname,"tr10") == 0 ||
       strcmp(fontname,"times_roman10") == 0 )
         return(GLUT_BITMAP_TIMES_ROMAN_10);
   else if (strcmp(fontname,"tr24") == 0 ||
       strcmp(fontname,"times_roman24") == 0 )
         return(GLUT_BITMAP_TIMES_ROMAN_24);
   else if (strcmp(fontname,"he10") == 0 ||
       strcmp(fontname,"helvetica10") == 0 )
         return(GLUT_BITMAP_HELVETICA_10);
   else if (strcmp(fontname,"he12") == 0 ||
       strcmp(fontname,"helvetica12") == 0 )
         return(GLUT_BITMAP_HELVETICA_12);
   else if (strcmp(fontname,"he18") == 0 ||
       strcmp(fontname,"helvetica18") == 0 )
         return(GLUT_BITMAP_HELVETICA_18);
   else return(NULL);
}

char * SUMA_glutBitmapFontName(void * font)
{
      if (!font) {
         return("NULL font");
      } else if (font == GLUT_BITMAP_8_BY_13) {
         return("font8");
      } else if (font == GLUT_BITMAP_9_BY_15) {
         return("font9");
      } else if (font == GLUT_BITMAP_TIMES_ROMAN_10) {
         return("times_roman10");
      } else if (font == GLUT_BITMAP_TIMES_ROMAN_24) {
         return("times_roman24");
      } else if (font == GLUT_BITMAP_HELVETICA_10) {
         return("helvetica10");
      } else if (font == GLUT_BITMAP_HELVETICA_12) {
         return("helvetica12");
      } else if (font == GLUT_BITMAP_HELVETICA_18) {
         return("helvetica18");
      } else {
         return("bad font pointer");
      }
}

char *SUMA_CoordTypeName (SUMA_DO_CoordType tp)
{
   switch(tp) {
      case SUMA_SCREEN:
         return("fixed");
      case SUMA_WORLD:
         return("mobile");
      case SUMA_COORD_TYPE_ERROR:
         return("Bad coordinate type");
      default:
         return("What is this?");
   }
}
SUMA_DO_CoordType SUMA_CoordType (char *atr)
{
   if (!atr) return(SUMA_WORLD);
   if (  !strcmp(atr,"mobile") ||
         !strcmp(atr,"world") ) {
      return(SUMA_WORLD);
   }  else if (!strcmp(atr,"fixed") ||
               !strcmp(atr,"screen")) {
      return(SUMA_SCREEN);
   }else {
      return(SUMA_COORD_TYPE_ERROR);
   }
}


/*!
   \brief Figures out box size for some text
   if font is NULL, box sizes are in character units 
   You can use glutBitmapLength
               glutBitmapWidth
   (some good tips here: http://www.lighthouse3d.com/opengl/glut )   
   See also  SUMA_NIDOtext_LineWidth   
   
   Note that w will contain the size of the largest line in a multi-line object  
*/
SUMA_Boolean SUMA_TextBoxSize (char *txt, int *w, int *h, int *nl, void *font)
{
   static char FuncName[]={"SUMA_TextBoxSize"};
   char *op=NULL, *ops=NULL, *OPE=NULL;
   int nc=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *w = 0;
   *h = 0;
   *nl = 0;
   if (!txt || !strlen(txt)) SUMA_RETURN(YUP);
   

   
   op = txt;
   OPE = txt+strlen(txt);
   /* One line at time */
   ops = op;
   do {
      SUMA_SKIP_LINE(op,OPE);
      if (op > ops) {
         if (!font) {
            ++(*h);
            nc = op-ops;
            if (nc > *w) *w = nc;
         } else {
            *h = *h + SUMA_glutBitmapFontHeight(font);
            if (*op == '\0') {
               nc = SUMA_glutBitmapLength(font, ops, op);
            } else {
               nc = SUMA_glutBitmapLength(font, ops, op-1);
            }
            if (nc > *w) *w = nc;
         }
         ++(*nl);
      }
      ops = op;
   } while (op < OPE);
   
   if (!font) {
      SUMA_LHv("For >>>%s<<<\nNeed %d lines with a max of %d chars on one line\n"
               "number of lines: %d\n",
            txt, *h,*w, *nl);
   } else {
      SUMA_LHv("For >>>%s<<<\n"
               "Need %d pixels high with a max of %d pixels on one line\n"
               "number of lines: %d\n",
            txt, *h,*w, *nl);
   }
       
       
   SUMA_RETURN(YUP);
}

/*!
   Get widths for each single-line string in txt
   The function returns the height of one line, a constant for all single line
   texts. -1 is returned in error
*/
int SUMA_WordBoxSize (char **txt, int N_txt, int *w, void *font)
{
   static char FuncName[]={"SUMA_WordBoxSize"};
   char *s=NULL;
   int nc=0, ii;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!txt || N_txt < 1) SUMA_RETURN(-1);
   

   for (ii=0; ii<N_txt; ++ii) {
      if (!(s = txt[ii]) || *s=='\0') w[ii]=0;
      else {
         nc = 0;
         while (s[nc++]!='\0');
         if (!font) w[ii]=nc;
         else {
            nc=0; w[ii]=0;
            while (s[nc] != '\0') w[ii]+=glutBitmapWidth(font,s[nc++]);
         }
      }
   }
   

   if (LocalHead) {
      for (ii=0; ii<N_txt; ++ii) {
         fprintf(stderr,"%d width %d height for %s\n", 
                  w[ii], SUMA_glutBitmapFontHeight(font), txt[ii]);
      }
   }  
   SUMA_RETURN(SUMA_glutBitmapFontHeight(font));
}

/*! 
   \brief Draws a scale line.
   
   \param ASIp (SUMA_AxisSegmentInfo *) structure containing segment info
   \param Ax (SUMA_Axis *)
*/
SUMA_Boolean SUMA_DrawLineAxis ( SUMA_AxisSegmentInfo *ASIp, 
                                 SUMA_Axis *Ax, SUMA_Boolean AddText)
{
   static char FuncName[]={"SUMA_DrawLineAxis"};
   double u3[3],nu, nu3, txofffac, size[2], space[2];
   double Pt[3], Ps[3];
   int prec = 1000, NmT;
   int i, jj, nTick[2];
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
         
   glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
                           /* turn off ambient and diffuse components */
   glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
            
   if (ASIp->AxisDim == 0) {
      glMaterialfv(GL_FRONT, GL_EMISSION, Ax->XaxisColor); 
                                       /*turn on emissivity for X axis*/
      if (LocalHead) fprintf(SUMA_STDERR,"%s: X axis\n", FuncName); 
   } else if (ASIp->AxisDim == 1) {
      glMaterialfv(GL_FRONT, GL_EMISSION, Ax->YaxisColor); 
                                       /*turn on emissivity for Y axis*/
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Y axis\n", FuncName); 
   } else if (ASIp->AxisDim == 2) {
      glMaterialfv(GL_FRONT, GL_EMISSION, Ax->ZaxisColor); 
                                       /*turn on emissivity for Z axis*/
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Z axis\n", FuncName); 
   }
            
   glBegin(GL_LINES);
   /* draw the line */
   glVertex3d(ASIp->P1[0], ASIp->P1[1], ASIp->P1[2]);
   glVertex3d(ASIp->P2[0], ASIp->P2[1], ASIp->P2[2]);

   /* work the ticks */
   /* unit vector */
   SUMA_UNIT_VEC(ASIp->P1, ASIp->P2, u3, nu3);
   for (jj=0; jj<2; ++jj) {
      if (jj == 0) { 
         space[0] = Ax->mTspace;
         size[0] = Ax->mTsize;
      } else {
         space[1] = Ax->MTspace;
         size[1] = Ax->MTsize;
      }
      
      /* starting point */
      /* is ASIp->P1 an OK point ?*/
      SUMA_NORM_VEC(ASIp->P1, 3, nu);
      if (! ( (int)(prec * nu) % (int)(prec * space[jj]) ) ) {
         /* a good starting point */
         SUMA_COPY_VEC(ASIp->P1, Pt, 3, float, float);
         SUMA_LH("Using ASIp->P1 as starting tick point"); 
      } else {
          NmT = (int)(prec * nu) / (int)(prec * space[jj]); NmT /= prec;
          Pt[0] = NmT * space[jj] * u3[0] +ASIp->P1[0]; 
          Pt[1] = NmT * space[jj] * u3[1]+ASIp->P1[1];  
          Pt[2] = NmT * space[jj] * u3[2]+ASIp->P1[2];
      }
      SUMA_LHv("Starting ticks at [%f %f %f]\nnu3 = %f\n", 
               Pt[0], Pt[1], Pt[2], nu3);


      /* draw the ticks */
      i = 0;
      if (LocalHead) 
         fprintf(SUMA_STDERR,
            "%s:\nspace = %f\nsize = %f\n", FuncName, space[jj], size[jj]);
      if (Ax->DoCross) {
         size[jj] /= 2.0;
         while (i*space[jj] < nu3) {
            Ps[0] = i*space[jj]*u3[0] + Pt[0]; 
            Ps[1] = i*space[jj]*u3[1] + Pt[1]; 
            Ps[2] = i*space[jj]*u3[2] + Pt[2]; /* center */
            #if 0 
               if (LocalHead) 
                  fprintf( SUMA_STDERR,
                           "%s:\nPs = [%f %f %f]; \n", 
                           FuncName, Ps[0], Ps[1], Ps[2]);
            #endif
            glVertex3d( Ps[0]-ASIp->tick1_dir[0]*size[jj], 
                        Ps[1]-ASIp->tick1_dir[1]*size[jj], 
                        Ps[2]-ASIp->tick1_dir[2]*size[jj]);
            glVertex3d( Ps[0]+ASIp->tick1_dir[0]*size[jj], 
                        Ps[1]+ASIp->tick1_dir[1]*size[jj], 
                        Ps[2]+ASIp->tick1_dir[2]*size[jj]);
            glVertex3d( Ps[0]-ASIp->tick2_dir[0]*size[jj], 
                        Ps[1]-ASIp->tick2_dir[1]*size[jj], 
                        Ps[2]-ASIp->tick2_dir[2]*size[jj]);
            glVertex3d( Ps[0]+ASIp->tick2_dir[0]*size[jj], 
                        Ps[1]+ASIp->tick2_dir[1]*size[jj], 
                        Ps[2]+ASIp->tick2_dir[2]*size[jj]);
            ++i;
         }
      } else {
         while (i*space[jj] < nu3) {
            Ps[0] = i*space[jj]*u3[0] + Pt[0]; 
            Ps[1] = i*space[jj]*u3[1] + Pt[1]; 
            Ps[2] = i*space[jj]*u3[2] + Pt[2]; /* center */
            #if 0 
              if (LocalHead) 
                  fprintf( SUMA_STDERR,
                           "%s:\nPs = [%f %f %f]; \n", 
                           FuncName, Ps[0], Ps[1], Ps[2]);
            #endif
            glVertex3d( Ps[0], Ps[1], Ps[2]);
            glVertex3d( Ps[0]+ASIp->tick1_dir[0]*size[jj], 
                        Ps[1]+ASIp->tick1_dir[1]*size[jj],  
                        Ps[2]+ASIp->tick1_dir[2]*size[jj]);
            glVertex3d( Ps[0], Ps[1], Ps[2]);
            glVertex3d( Ps[0]+ASIp->tick2_dir[0]*size[jj], 
                        Ps[1]+ASIp->tick2_dir[1]*size[jj], 
                        Ps[2]+ASIp->tick2_dir[2]*size[jj]);
               #if 0 /* for a little debug */
               if (jj==1) {
                  glVertex3d(Ps[0], Ps[1], Ps[2]);
                  txofffac = 1.0 * size[1];
                  Ps[0] = i*space[1]*u3[0] + Pt[0] + txofffac * ASIp->TxOff[0]; 
                  Ps[1] = i*space[1]*u3[1] + Pt[1] + txofffac * ASIp->TxOff[1]; 
                  Ps[2] = i*space[1]*u3[2] + Pt[2] + txofffac * ASIp->TxOff[2];
                  glVertex3d(Ps[0], Ps[1], Ps[2]);
               }
               #endif
            ++i;
         }
      }
      nTick[jj] = i-1;
      
      
   }
   
   glEnd();

   glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, NoColor); 
      /*turn off emissivity for axis*/
   
         

   if (AddText) { /* do the text for major ticks only */
      float MinYstep, MinXstep, dSxT, dSyT, curXstep, curYstep;
      int OKnext;
      int DoZero = 0; /* skip zero label, they get all bunched up */
      dSxT = (float)fabs(ASIp->ScreenProj[0]) / (float)nTick[1];
      dSyT = (float)fabs(ASIp->ScreenProj[1]) / (float)nTick[1];
      MinYstep = 15 ; /* height of letters in pixels 
                        (using GLUT_BITMAP_9_BY_15) 
                        Might want to use SUMA_glutBitmapFontHeight
                        if you're using differing fonts*/
      MinXstep = 9 * 5; /* length of string in pixels 
                           (around 5 chars, including sign, using %.1f )*/
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s:\ndS = %f, %f\n", FuncName, dSxT, dSyT);
      i = 0;
      if (Ax->DoCross) { /* size has already been modified above ... */
         /* perhaps add a factor to the shift below, we'll see .. */
         txofffac = 2.0 * size[1];
      } else {
         txofffac = 1.0 * size[1];
      }
      
      OKnext = 1;
      curXstep =0; curYstep=0;
      while (i*space[1] < nu3) {
         if(OKnext) {
            Ps[0] = i*space[1]*u3[0] + Pt[0] + txofffac * ASIp->TxOff[0]; 
            Ps[1] = i*space[1]*u3[1] + Pt[1] + txofffac * ASIp->TxOff[1]; 
            Ps[2] = i*space[1]*u3[2] + Pt[2] + txofffac * ASIp->TxOff[2];
            if (DoZero || i > 0) {
               SUMA_AxisText(ASIp, Ps);
               SUMA_LHv("txofffac = %f, ASIp->TxOff=[%f %f %f]\n",
                  txofffac, ASIp->TxOff[0], ASIp->TxOff[1], ASIp->TxOff[2]);
            }
         }
         curXstep += dSxT; curYstep += dSyT; 
         if (curXstep > MinXstep || curYstep > MinYstep) { 
            OKnext = 1;
            curXstep =0; curYstep=0;
         } else {
            OKnext = 0;
         }
         ++i;
      }
   }

   SUMA_RETURN(YUP);
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
SUMA_DRAWN_ROI **SUMA_Find_ROIonSO (SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                                    int N_do, int *N_ROI)
{
   static char FuncName[]={"SUMA_Find_ROIonSO"};
   SUMA_DRAWN_ROI **ROIv=NULL;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   int i, roi_cnt=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *N_ROI = -1;
   
   /* allocate for maximum */
   ROIv = (SUMA_DRAWN_ROI **)SUMA_calloc(N_do, sizeof(SUMA_DRAWN_ROI *));
   if (!ROIv) {
      SUMA_SL_Crit("Failed to allocate for ROIv");
      SUMA_RETURN(NULL);
   }
   
   roi_cnt=0;
   for (i=0; i < N_do; ++i) {
      if (dov[i].ObjectType == ROIdO_type) {
         D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
         if (!strncmp(D_ROI->Parent_idcode_str, SO->idcode_str, 
                      strlen(SO->idcode_str))) {
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
   ROIv = (SUMA_DRAWN_ROI **)
               SUMA_realloc(ROIv, sizeof(SUMA_DRAWN_ROI *)*roi_cnt);
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
SUMA_DRAWN_ROI **SUMA_Find_ROIrelatedtoSO (SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                                           int N_do, int *N_ROI)
{
   static char FuncName[]={"SUMA_Find_ROIrelatedtoSO"};
   SUMA_DRAWN_ROI **ROIv=NULL;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   int i, roi_cnt=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *N_ROI = -1;
   
   /* allocate for maximum */
   ROIv = (SUMA_DRAWN_ROI **)SUMA_calloc(N_do,sizeof(SUMA_DRAWN_ROI *));
   if (!ROIv) {
      SUMA_SL_Crit("Failed to allocate for ROIv");
      SUMA_RETURN(NULL);
   }
   
   roi_cnt=0;
   for (i=0; i < N_do; ++i) {
      if (dov[i].ObjectType == ROIdO_type) {
         D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
         if (SUMA_isdROIrelated (D_ROI, (SUMA_ALL_DO *)SO)) {
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
   ROIv = (SUMA_DRAWN_ROI **)SUMA_realloc(ROIv, 
                                          sizeof(SUMA_DRAWN_ROI *)*roi_cnt);
   if (!ROIv) {
      SUMA_SL_Crit("Failed to reallocate for ROIv");
      SUMA_RETURN(NULL);
   }
   *N_ROI = roi_cnt;
   
   SUMA_RETURN(ROIv);
}

/*! Create Node-based spheres for a particular surface */
SUMA_Boolean SUMA_Draw_SO_NBSP (SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                                    int N_do, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Draw_SO_NBSP"};
   int i;
   SUMA_SphereDO *SDO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   for (i=0; i < N_do; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case NBSP_type:
            SDO = (SUMA_SphereDO *)dov[i].OP;
            if (SUMA_isNBDOrelated ((SUMA_NB_DO *)SDO, SO)) { /* draw it */
               if (LocalHead) 
                  fprintf(SUMA_STDERR, "%s: Drawing spheres \n", FuncName);
               if (!SUMA_DrawSphereDO (SDO, sv)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_DrawSphereDO.\n", 
                           FuncName);
               }
            }
         default:
            break;
      }
   }

   SUMA_RETURN(YUP);
}

/*! Create Node-based vectors for a particular surface */
SUMA_Boolean SUMA_Draw_SO_NBV (  SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                                 int N_do, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Draw_SO_NBV"};
   int i;
   SUMA_SegmentDO *SDO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   for (i=0; i < N_do; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case ONBV_type:
         case NBV_type:
         case NBOLS_type:
         case NBLS_type:
            SDO = (SUMA_SegmentDO *)dov[i].OP;
            if (SUMA_isNBDOrelated ((SUMA_NB_DO *)SDO, SO)) { /* draw it */
               if (LocalHead) 
                  fprintf(SUMA_STDERR, "%s: Drawing vec field \n", FuncName);
               if (!SUMA_DrawSegmentDO (SDO, sv)) {
                  fprintf(SUMA_STDERR, 
                     "Error %s: Failed in SUMA_DrawSegmentDO.\n", FuncName);
               }
            }
         default:
            break;
      }
   }

   SUMA_RETURN(YUP);
}

/*! Create NIML DOs that are attached to a surface */
SUMA_Boolean SUMA_Draw_SO_NIDO (  SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                                  int N_do, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Draw_SO_NIDO"};
   int i;
   SUMA_NIDO *SDO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   for (i=0; i < N_do; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case NIDO_type:
            SDO = (SUMA_NIDO *)dov[i].OP;
            SUMA_LHv("Testing NIDO\n"
                      "isSurfBased: %d\n"
                      "isNIDOrelated: %d\n", 
                      SUMA_isNIDO_SurfBased(SDO),
                      SUMA_isNIDOrelated (SDO, SO));
            if (  SUMA_isNIDO_SurfBased(SDO) && 
                  SUMA_isNIDOrelated (SDO, SO)) { /* draw it */
               if (!SUMA_DrawNIDO (SDO, sv)) {
                  fprintf(SUMA_STDERR, 
                     "Error %s: Failed in SUMA_DrawNIDO.\n", FuncName);
               }
            } else {
            }
         default:
            break;
      }
   }

   SUMA_RETURN(YUP);
}

/*!
   Get node-based texture for a certain surface 
*/
NI_element * SUMA_SO_NIDO_Node_Texture (  SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                                          int N_do, SUMA_SurfaceViewer *sv )
{
   static char FuncName[]={"SUMA_SO_NIDO_Node_Texture"};
   int i, ip, sz[2]={0, 0};
   float txloc[4]={0,0,0,0};
   char *target=NULL;
   NI_element *nel=NULL, *nelo=NULL;
   SUMA_NIDO *SDO = NULL;
   SUMA_SurfaceObject *default_SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   nelo=NULL;
   for (i=0; i < N_do; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case NIDO_type:
            SDO = (SUMA_NIDO *)dov[i].OP;
            SUMA_LHv("Testing NIDO\n"
                      "isSurfBased: %d\n"
                      "isNIDOrelated: %d\n", 
                      SUMA_isNIDO_SurfBased(SDO),
                      SUMA_isNIDOrelated (SDO, SO));
            if (  SUMA_isNIDO_SurfBased(SDO) && 
                  SUMA_isNIDOrelated (SDO, SO)) { 
               /* loop through the elements only grabbing
                  node-based texture */
               for( ip=0 ; ip < SDO->ngr->part_num ; ip++ ){ 
                  nel = NULL;
                  switch( SDO->ngr->part_typ[ip] ){
                     /*-- a sub-group ==> recursion! --*/
                     case NI_GROUP_TYPE:
                        break ;
                     case NI_ELEMENT_TYPE:
                        nel = (NI_element *)SDO->ngr->part[ip] ;
                        target = NI_get_attribute(nel,"target");
                        if ( ! strcmp(nel->name,"Tex") ) {
                           if (!target || strncmp(target, "ALL_SURF",8)==0 
                               || SUMA_iswordin(SO->Label,target)) {
                              nelo = nel;
                              goto SET_TEX; 
                           }
                        }
                        break;
                     default:
                        break;
                  }
               }
            }
         default:
            break;
      }
   }

   SET_TEX:
   

   SUMA_RETURN(nelo);
   
}

SUMA_Boolean SUMA_Draw_SO_Dset_Contours(SUMA_SurfaceObject *SO, 
                               SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Draw_SO_Dset_Contours"};
   SUMA_DSET *dd=NULL;
   SUMA_OVERLAYS *colplane=NULL;
   DListElmt *el=NULL;
   SUMA_DRAWN_ROI *D_ROI=NULL;
   int OverInd = -1, id2cont=0, id1cont=0, icont=0, ic, i2last=0;
   float off[3];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   el = dlist_head(SUMAg_CF->DsetList);
   while (el) {
      dd = (SUMA_DSET*)el->data;
      if (SUMA_isDsetRelated(dd,SO)) {
         SUMA_LHv("Have Dset %s related to SO\n", SDSET_LABEL(dd));
         if (!(colplane = SUMA_Fetch_OverlayPointerByDset (
                           (SUMA_ALL_DO *)SO, dd, &OverInd))) {
               SUMA_S_Errv(
                  "Failed to fetch existing %s dset's overlay pointer\n", 
                  SDSET_LABEL(dd));
               SUMA_RETURN(NOPE);
         }
         /* any contours? */
         if ( (colplane->ShowMode == SW_SurfCont_DsetViewCon || 
               colplane->ShowMode == SW_SurfCont_DsetViewCaC ) && 
              colplane->Contours && colplane->N_Contours) {
            /* draw them */
            for (ic=0; ic<colplane->N_Contours; ++ic) {
               D_ROI = (SUMA_DRAWN_ROI *)colplane->Contours[ic];
               SUMA_LHv("Dset Contouring %d\n", ic);
               
               if (D_ROI->CE && D_ROI->N_CE) {
                  /* Draw the contour */
                  if (!SO->patchNodeMask) {
                     glLineWidth(sv->ContThick); /* Changed from horrible '6' 
                                 now that glPolygonOffset is used to 
                                 allow for proper coplanar line and
                                 polygon rendering.  July 8th 2010 */
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                  D_ROI->FillColor);
                     SUMA_LH("Drawing contour ...");
                     
                     #if 1 /* Should be a little faster */
                     /* initialize first point down */
                     glBegin(GL_LINE_STRIP);
                     id1cont = 3 * D_ROI->CE[0].n1;
                     glVertex3f(SO->NodeList[id1cont], 
                                SO->NodeList[id1cont+1], 
                                SO->NodeList[id1cont+2]);
                     i2last = D_ROI->CE[0].n1;
                     for (icont = 0; icont < D_ROI->N_CE; ++icont) {
                        id2cont = 3 * D_ROI->CE[icont].n2;
                        if (i2last != D_ROI->CE[icont].n1) {
                           /* break in loop*/
                           glEnd(); /* end lines */
                           glBegin(GL_LINE_STRIP); /* begin again */
                           id1cont = 3 * D_ROI->CE[icont].n1;
                           glVertex3f(SO->NodeList[id1cont], 
                                      SO->NodeList[id1cont+1], 
                                      SO->NodeList[id1cont+2]);
                        }
                        /* put down next vertex */
                        glVertex3f(SO->NodeList[id2cont], 
                                   SO->NodeList[id2cont+1],    
                                   SO->NodeList[id2cont+2]);
                        i2last = D_ROI->CE[icont].n2;
                     }
                     glEnd();
                     #else /* old simpler way */
                     for (icont = 0; icont < D_ROI->N_CE; ++icont) {
                        id1cont = 3 * D_ROI->CE[icont].n1;
                        id2cont = 3 * D_ROI->CE[icont].n2;
                        glBegin(GL_LINES);
                        glVertex3f(SO->NodeList[id1cont], 
                                   SO->NodeList[id1cont+1], 
                                   SO->NodeList[id1cont+2]);
                        glVertex3f(SO->NodeList[id2cont], 
                                   SO->NodeList[id2cont+1],    
                                   SO->NodeList[id2cont+2]);
                        glEnd();

                     }
                     #endif
                  } else {
                     if (SO->EmbedDim == 2) {
                        glLineWidth(sv->ContThick);
                        glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                     D_ROI->FillColor);
                     } else {
                        glLineWidth(sv->ContThick);
                        glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                     D_ROI->FillColor);   
                     }
                     SUMA_LHv("Drawing contour on patch (%p)...", 
                              SO->NodeNormList);
                              /* set default offset to nothing*/
                     off[0]=0.0; off[1]=0.0; off[2]=0.0; 
                     if (SO->EmbedDim == 2) {
                        if (SO->NodeNormList && D_ROI->CE) {
                           /* just take a node in the ROI */
                           icont = 0;
                           while (  icont < D_ROI->N_CE && 
                                    ( (D_ROI->CE[icont].n1 >= SO->N_Node || 
                                       D_ROI->CE[icont].n2 >= SO->N_Node)  || 
                                  (!SO->patchNodeMask[D_ROI->CE[icont].n1] &&
                                   !SO->patchNodeMask[D_ROI->CE[icont].n2]) ) ) 
                              ++icont;
                           if (icont < D_ROI->N_CE && 
                                 D_ROI->CE[icont].n1 < SO->N_Node &&
                                 D_ROI->CE[icont].n2 < SO->N_Node )    {
                              id2cont = 3 * D_ROI->CE[icont].n2;
                              off[0] = 3*SO->NodeNormList[id2cont];
                              off[1] = 3*SO->NodeNormList[id2cont+1];
                              off[2] = 3*SO->NodeNormList[id2cont+2];
                           } else {
                              off[0] = off[1] = off[2] = 0.0;
                           }
                        }
                     }
                     #if 1 /* faster but more complicated */
                     icont = 0;
                     while (  icont < D_ROI->N_CE && 
                              ( (D_ROI->CE[icont].n1 >= SO->N_Node || 
                                 D_ROI->CE[icont].n2 >= SO->N_Node)  || 
                            (!SO->patchNodeMask[D_ROI->CE[icont].n1] &&
                             !SO->patchNodeMask[D_ROI->CE[icont].n2]) ) ) 
                        ++icont; /* skip if n1 or n2 exceed N_Node, 
                                    or neither of them is in the patch. 
                                    For patches, it is possible that 
                                    SOpatch->N_Node < SOLDP->Parent */
                     if (icont < D_ROI->N_CE && 
                           D_ROI->CE[icont].n1 < SO->N_Node &&
                           D_ROI->CE[icont].n2 < SO->N_Node ) {
                        glBegin(GL_LINE_STRIP);
                        id1cont = 3 * D_ROI->CE[icont].n1;
                        glVertex3f(SO->NodeList[id1cont]+off[0], 
                                   SO->NodeList[id1cont+1]+off[1], 
                                   SO->NodeList[id1cont+2]+off[2]);
                        i2last = D_ROI->CE[icont].n1;
                        while (icont < D_ROI->N_CE) {
                           if (D_ROI->CE[icont].n1 < SO->N_Node &&
                               D_ROI->CE[icont].n2 < SO->N_Node &&
                               SO->patchNodeMask[D_ROI->CE[icont].n1] &&
                               SO->patchNodeMask[D_ROI->CE[icont].n2] ) {
                              id2cont = 3 * D_ROI->CE[icont].n2;
                              if (i2last != D_ROI->CE[icont].n1) {
                                 /* break in loop*/
                                 glEnd(); /* end lines */
                                 glBegin(GL_LINE_STRIP); /* begin again */
                                 id1cont = 3 * D_ROI->CE[icont].n1;
                                 glVertex3f(SO->NodeList[id1cont]+off[0], 
                                            SO->NodeList[id1cont+1]+off[1], 
                                            SO->NodeList[id1cont+2]+off[2]);
                              }
                              /* put down next vertex */
                              glVertex3f(SO->NodeList[id2cont]+off[0], 
                                         SO->NodeList[id2cont+1]+off[1],    
                                         SO->NodeList[id2cont+2]+off[2]);
                              i2last = D_ROI->CE[icont].n2;
                           }
                           ++icont;
                        }
                        glEnd();
                     }
                     #else /* slower way */
                     for (icont = 0; icont < D_ROI->N_CE; ++icont) {
                        id1cont = 3 * D_ROI->CE[icont].n1;
                        id2cont = 3 * D_ROI->CE[icont].n2;
                        if (D_ROI->CE[icont].n1 < SO->N_Node &&
                            D_ROI->CE[icont].n2 < SO->N_Node &&
                            SO->patchNodeMask[D_ROI->CE[icont].n1] && 
                            SO->patchNodeMask[D_ROI->CE[icont].n2]) {

                           glBegin(GL_LINES);
                           glVertex3f(SO->NodeList[id2cont]+off[0], 
                                      SO->NodeList[id2cont+1]+off[1],    
                                      SO->NodeList[id2cont+2]+off[2]);
                           glVertex3f(SO->NodeList[id1cont]+off[0], 
                                      SO->NodeList[id1cont+1]+off[1], 
                                      SO->NodeList[id1cont+2]+off[2]); 
                           glEnd();
                        }
                     }
                     #endif
                  }
               }

            }
         }
      }
      el = dlist_next(el);
   }
     
   SUMA_RETURN(YUP);
}

/*! Create the ROIs for a particular surface */
SUMA_Boolean SUMA_Draw_SO_ROI (SUMA_SurfaceObject *SO, 
                               SUMA_DO* dov, int N_do, 
                               SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Draw_SO_ROI"};
   GLfloat ROI_SphCol[] = {1.0, 0.0, 0.0, 1.0};
   GLfloat ROI_SphCol_frst[] = {1.0, 0.0, 0.0, 1.0};
   GLfloat ROI_FaceGroup[] = {0.8, 0.3, 1.0, 1.0 };
   GLfloat ROI_NodeGroup[] = {0.8, 0.3, 0.5, 1.0 };
   GLfloat ROI_EdgeGroup[] = {0.8, 0.8, 0.1, 1.0 };
   GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   GLfloat NoCol[4] = {0.0, 0.0, 0.0, 0.0};
   GLfloat Red[4] = {1.0, 0.0, 0.0, 1.0};
   GLfloat Green[4] = {0.0, 1.0, 0.0, 1.0};
   GLfloat Blue[4] = {0.0, 0.0, 1.0, 1.0};
   GLfloat Yellow[4] = {1.0, 1.0, 0.0, 1.0};
   GLfloat Cyan[4] = {0.0, 1.0, 1.0, 1.0};
   GLfloat Pink[4] = {1.0, 0.0, 1.0, 1.0};
   int  i, id, ii, id1,id2, id3, EdgeIndex, FaceIndex, 
         Node1, Node2, Node3, N_ROId=0, idFirst=0,
         *nodemask=NULL;
   float dx, dy, dz = 0.0;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   SUMA_ROI_DATUM *ROId=NULL;
   SUMA_ROI *ROI = NULL;
   DListElmt *NextElm=NULL;
   float off[3];
   byte AmbDiff = 0;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   
   for (i=0; i < N_do; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case ROIdO_type:
            D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
            if (!D_ROI->ROIstrokelist) {
               fprintf (SUMA_STDERR, 
                        "Error %s: NULL ROIstrokeList.\n", FuncName);
               SUMA_RETURN (NOPE);
            }else if (!dlist_size(D_ROI->ROIstrokelist)) {
               if (LocalHead) 
                  fprintf (SUMA_STDERR, "%s: Empty ROIstrokelist.\n", FuncName);
               break;
            }
            if (SUMA_isdROIrelated (D_ROI, (SUMA_ALL_DO *)SO)) { /* draw it */
               if (LocalHead) 
                  fprintf(SUMA_STDERR, "%s: Drawing Drawn ROI %s (Status %d)\n", 
                          FuncName, D_ROI->Label, D_ROI->DrawStatus);
               if (D_ROI->DrawStatus == SUMA_ROI_InCreation) {
                  switch (D_ROI->Type) {
                     case SUMA_ROI_OpenPath:
                        SUMA_LH("Red first, Green next");
                        SUMA_COPY_VEC(Red, ROI_SphCol_frst, 4, GLfloat, GLfloat);
                        SUMA_COPY_VEC(Green, ROI_SphCol, 4, GLfloat, GLfloat);     
                        break;   
                     case SUMA_ROI_ClosedPath:
                        SUMA_LH("Yellow first, Cyan next");
                        SUMA_COPY_VEC( Yellow, ROI_SphCol_frst, 4, 
                                       GLfloat, GLfloat);
                        SUMA_COPY_VEC(Cyan, ROI_SphCol, 4, GLfloat, GLfloat);      
                        break;   
                     case SUMA_ROI_FilledArea:
                        SUMA_LH("Pink First, Yellow Next");
                        SUMA_COPY_VEC(Pink, ROI_SphCol_frst, 4, 
                                      GLfloat, GLfloat);
                        SUMA_COPY_VEC(Cyan, ROI_SphCol, 4, GLfloat, GLfloat);       
                        break;   
                     default:
                        SUMA_LH("Default");
                        ROI_SphCol_frst[0] = 1.0; 
                        ROI_SphCol_frst[1] = 0.3; 
                        ROI_SphCol_frst[2] = 1.0; 
                        ROI_SphCol_frst[3] = 1.0;     
                        ROI_SphCol[0] = 1.0; 
                        ROI_SphCol[1] = 1.0; 
                        ROI_SphCol[2] = 0.0; 
                        ROI_SphCol[3] = 1.0;     
                        break;

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
                              SUMA_LH("First sphere");
                              if (AmbDiff) { 
                                 glMaterialfv(GL_FRONT, 
                                              GL_AMBIENT_AND_DIFFUSE, 
                                              ROI_SphCol_frst);
                              }  else { /* AmbDiff lights out */
                                 glMaterialfv(GL_FRONT, 
                                              GL_AMBIENT_AND_DIFFUSE, NoColor);
                                 glMaterialfv(GL_FRONT, GL_EMISSION,
                                             ROI_SphCol_frst); 
                              }
                              idFirst = 3 * ROId->nPath[0];
                              glTranslatef ( SO->NodeList[idFirst], 
                                             SO->NodeList[idFirst+1], 
                                             SO->NodeList[idFirst+2]);
                              gluSphere( 
                                 SO->NodeMarker->sphobj, 
                                 SO->NodeMarker->sphrad*
                                    SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06), 
                                 SO->NodeMarker->slices, 
                                 SO->NodeMarker->stacks);
                              glTranslatef (-SO->NodeList[idFirst], 
                                            -SO->NodeList[idFirst+1], 
                                            -SO->NodeList[idFirst+2]);
                           } 

                           glLineWidth(6);
                           if (AmbDiff) { 
                              glMaterialfv(GL_FRONT, 
                                           GL_AMBIENT_AND_DIFFUSE, 
                                           ROI_SphCol);
                           }  else { /* AmbDiff lights out */
                              glMaterialfv(GL_FRONT, 
                                           GL_AMBIENT_AND_DIFFUSE, NoColor);
                              glMaterialfv(GL_FRONT, GL_EMISSION,
                                          ROI_SphCol); 
                           }
                           /* always start at 1 since the 0th node was 
                              drawn at the end of the previous ROId */
                           for (ii = 1; ii < ROId->N_n; ++ii) {
                              id = 3 * ROId->nPath[ii];
                              id2 = 3 * ROId->nPath[ii-1];

                              /* draw lines connecting spheres */
                              glBegin(GL_LINES);
                              glVertex3f(SO->NodeList[id2], SO->NodeList[id2+1], 
                                         SO->NodeList[id2+2]);
                              glVertex3f(SO->NodeList[id], SO->NodeList[id+1], 
                                         SO->NodeList[id+2]); 
                              glEnd();

                              glTranslatef (SO->NodeList[id], SO->NodeList[id+1],                                             SO->NodeList[id+2]);
                              gluSphere(
                                 SO->NodeMarker->sphobj, 
                                 SO->NodeMarker->sphrad*
                                    SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06),
                                 SO->NodeMarker->slices, SO->NodeMarker->stacks);
                              glTranslatef (-SO->NodeList[id], 
                                            -SO->NodeList[id+1], 
                                            -SO->NodeList[id+2]);
                           }


                           ++N_ROId;
                        }
                     } else { /* non segment type Drawn ROI */
                           #if 0 
                              /* it is too much to fill with spheres... */
                              glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, 
                                           ROI_NodeGroup);
                              for (ii=0; ii < ROId->N_n; ++ii) {
                                 id = 3 * ROId->nPath[ii];
                                 glTranslatef (SO->NodeList[id], 
                                               SO->NodeList[id+1], 
                                               SO->NodeList[id+2]);
                                 gluSphere(SO->NodeMarker->sphobj, 
                                           SO->NodeMarker->sphrad, 
                                           SO->NodeMarker->slices, 
                                           SO->NodeMarker->stacks);
                                 glTranslatef (-SO->NodeList[id], 
                                               -SO->NodeList[id+1], 
                                               -SO->NodeList[id+2]);
                              }
                           #endif
                     }
                  } while (NextElm != dlist_tail(D_ROI->ROIstrokelist));
               } else if (SUMAg_CF->ROI_contmode) {
                  /* finished, draw contour */
                  SUMA_LH("Finished DROI");
                  ROI_SphCol_frst[0] = 1.0; 
                  ROI_SphCol_frst[1] = 0.3; 
                  ROI_SphCol_frst[2] = 1.0; 
                  ROI_SphCol_frst[3] = 1.0;     
                  ROI_SphCol[0] = 1.0; 
                  ROI_SphCol[1] = 1.0; 
                  ROI_SphCol[2] = 0.0; 
                  ROI_SphCol[3] = 1.0;
                  
                  
                  if (D_ROI->CE) {
                     int id1cont, id2cont, icont;
                     /* Draw the contour */
                     
                     if (!SO->patchNodeMask) {
                        glLineWidth(2);
                        if (AmbDiff) { 
                           glMaterialfv(GL_FRONT, 
                                        GL_AMBIENT_AND_DIFFUSE, 
                                        D_ROI->FillColor);
                        }  else { /* AmbDiff lights out */
                           glMaterialfv(GL_FRONT, 
                                        GL_AMBIENT_AND_DIFFUSE, NoColor);
                           glMaterialfv(GL_FRONT, GL_EMISSION,
                                       D_ROI->FillColor); 
                        }
                        SUMA_LH("Drawing contour ...");
                        for (icont = 0; icont < D_ROI->N_CE; ++icont) {
                           id1cont = 3 * D_ROI->CE[icont].n1;
                           id2cont = 3 * D_ROI->CE[icont].n2;
                           glBegin(GL_LINES);
                           glVertex3f(SO->NodeList[id2cont], 
                                      SO->NodeList[id2cont+1],    
                                      SO->NodeList[id2cont+2]);
                           glVertex3f(SO->NodeList[id1cont], 
                                      SO->NodeList[id1cont+1], 
                                      SO->NodeList[id1cont+2]); 
                           glEnd();
                        }
                     } else {
                        if (SO->EmbedDim == 2) {
                           glLineWidth(1);
                        } else {
                           glLineWidth(2);
                        }
                        if (AmbDiff) { 
                           glMaterialfv(GL_FRONT, 
                                        GL_AMBIENT_AND_DIFFUSE, 
                                        D_ROI->FillColor);
                        }  else { /* AmbDiff lights out */
                           glMaterialfv(GL_FRONT, 
                                        GL_AMBIENT_AND_DIFFUSE, NoColor);
                           glMaterialfv(GL_FRONT, GL_EMISSION,
                                       D_ROI->FillColor); 
                        }

                        SUMA_LHv("Drawing contour on patch (%p)...", 
                                 SO->NodeNormList);
                                 /* set default offset to nothing*/
                        off[0]=0.0; off[1]=0.0; off[2]=0.0; 
                        if (SO->EmbedDim == 2) {
                           if (SO->NodeNormList && D_ROI->CE) {
                              /* just take a node in the ROI */
                              id2cont = 3 * D_ROI->CE[0].n2;
                              off[0] = 3*SO->NodeNormList[id2cont];
                              off[1] = 3*SO->NodeNormList[id2cont+1];
                              off[2] = 3*SO->NodeNormList[id2cont+2];
                           }
                        }
                        for (icont = 0; icont < D_ROI->N_CE; ++icont) {
                           id1cont = 3 * D_ROI->CE[icont].n1;
                           id2cont = 3 * D_ROI->CE[icont].n2;
                           if (SO->patchNodeMask[D_ROI->CE[icont].n1] && 
                               SO->patchNodeMask[D_ROI->CE[icont].n2]) {
                              
                              glBegin(GL_LINES);
                              glVertex3f(SO->NodeList[id2cont]+off[0], 
                                         SO->NodeList[id2cont+1]+off[1],    
                                         SO->NodeList[id2cont+2]+off[2]);
                              glVertex3f(SO->NodeList[id1cont]+off[0], 
                                         SO->NodeList[id1cont+1]+off[1], 
                                         SO->NodeList[id1cont+2]+off[2]); 
                              glEnd();
                           }
                        }
                     }
                  }
                  
               }

            }
            break;
            
         case ROIO_type:
            /* hopefully he distinction between drawn 
               and not drawn will no longer be needed .... */
            ROI = (SUMA_ROI *)dov[i].OP;
            if (SUMA_isROIrelated (ROI, SO)) { /* draw it */
               if (LocalHead) 
                  fprintf(SUMA_STDERR, 
                           "%s: Drawing ROI %s \n", FuncName, ROI->Label);
               switch (ROI->Type) { /* ROI types */
                  case SUMA_ROI_EdgeGroup:
                     if (AmbDiff) { 
                        glMaterialfv(GL_FRONT, 
                                     GL_AMBIENT_AND_DIFFUSE, 
                                     ROI_EdgeGroup);
                     }  else { /* AmbDiff lights out */
                        glMaterialfv(GL_FRONT, 
                                     GL_AMBIENT_AND_DIFFUSE, NoColor);
                        glMaterialfv(GL_FRONT, GL_EMISSION,
                                    ROI_EdgeGroup); 
                     }
                     for (ii=0; ii < ROI->N_ElInd; ++ii) {
                        EdgeIndex = ROI->ElInd[ii];
                        Node1 = SO->EL->EL[EdgeIndex][0];
                        Node2 = SO->EL->EL[EdgeIndex][1];
                        id = 3 * Node1;
                        id2 = 3 * Node2;
                        
                        glLineWidth(3);
                        
                        glBegin(GL_LINES);
                        glVertex3f( SO->NodeList[id2], SO->NodeList[id2+1], 
                                    SO->NodeList[id2+2]);
                        glVertex3f( SO->NodeList[id], SO->NodeList[id+1], 
                                    SO->NodeList[id+2]); 
                        glEnd();
                     }
                     break;
                  case SUMA_ROI_NodeGroup:
                     if (AmbDiff) { 
                        glMaterialfv(GL_FRONT, 
                                     GL_AMBIENT_AND_DIFFUSE, 
                                     ROI_NodeGroup);
                     }  else { /* AmbDiff lights out */
                        glMaterialfv(GL_FRONT, 
                                     GL_AMBIENT_AND_DIFFUSE, NoColor);
                        glMaterialfv(GL_FRONT, GL_EMISSION,
                                    ROI_NodeGroup); 
                     }
                     for (ii=0; ii < ROI->N_ElInd; ++ii) {
                        id = 3 * ROI->ElInd[ii];
                        glTranslatef (SO->NodeList[id], SO->NodeList[id+1], 
                                      SO->NodeList[id+2]);
                        gluSphere(SO->NodeMarker->sphobj, 
                                    SO->NodeMarker->sphrad*
                                    SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06), 
                           SO->NodeMarker->slices, SO->NodeMarker->stacks);
                        glTranslatef (-SO->NodeList[id], -SO->NodeList[id+1], 
                                      -SO->NodeList[id+2]);
                     }
                     break;
                  case SUMA_ROI_FaceGroup:   
                     if (AmbDiff) { 
                        glMaterialfv(GL_FRONT, 
                                     GL_AMBIENT_AND_DIFFUSE, 
                                     ROI_FaceGroup);
                     }  else { /* AmbDiff lights out */
                        glMaterialfv(GL_FRONT, 
                                     GL_AMBIENT_AND_DIFFUSE, NoColor);
                        glMaterialfv(GL_FRONT, GL_EMISSION,
                                    ROI_FaceGroup); 
                     }
                     
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
                              
                              dx = SUMA_SELECTED_FACESET_OFFSET_FACTOR * 
                                    SO->FaceNormList[id];
                              dy = SUMA_SELECTED_FACESET_OFFSET_FACTOR * 
                                    SO->FaceNormList[id+1];
                              dz = SUMA_SELECTED_FACESET_OFFSET_FACTOR * 
                                    SO->FaceNormList[id+2];


                              glBegin(GL_LINE_LOOP);
                                 glVertex3f( SO->NodeList[id1]+dx, 
                                             SO->NodeList[id1+1]+dy, 
                                             SO->NodeList[id1+2]+dz);
                                 glVertex3f( SO->NodeList[id2]+dx, 
                                             SO->NodeList[id2+1]+dy, 
                                             SO->NodeList[id2+2]+dz);
                                 glVertex3f( SO->NodeList[id3]+dx, 
                                             SO->NodeList[id3+1]+dy, 
                                             SO->NodeList[id3+2]+dz);
                              glEnd();


                              glBegin(GL_LINE_LOOP);
                                 glVertex3f( SO->NodeList[id1]-dx, 
                                             SO->NodeList[id1+1]-dy, 
                                             SO->NodeList[id1+2]-dz);
                                 glVertex3f( SO->NodeList[id2]-dx, 
                                             SO->NodeList[id2+1]-dy,    
                                             SO->NodeList[id2+2]-dz);
                                 glVertex3f( SO->NodeList[id3]-dx, 
                                             SO->NodeList[id3+1]-dy, 
                                             SO->NodeList[id3+2]-dz);
                              glEnd();
                           #endif
                           
                           glBegin(GL_LINE_LOOP);
                              glVertex3f( SO->NodeList[id1], 
                                          SO->NodeList[id1+1], 
                                          SO->NodeList[id1+2]);
                              glVertex3f( SO->NodeList[id2], 
                                          SO->NodeList[id2+1], 
                                          SO->NodeList[id2+2]);
                              glVertex3f( SO->NodeList[id3], 
                                          SO->NodeList[id3+1], 
                                          SO->NodeList[id3+2]);
                           glEnd();

                     }
                     break;
                  default:
                     fprintf( SUMA_STDERR, 
                              "Error %s: Not ready to drawn this type of ROI.\n",                               FuncName);
                     break;
               } /* ROI types */
            } /* draw it */
            break;
         default:
            /* not an ROI */
            break;
      }/* case Object Type */
      
   }
   
   /* lights out */
   if (AmbDiff) { 
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE,  NoColor);
   }  else { 
      glMaterialfv(GL_FRONT, 
                   GL_AMBIENT_AND_DIFFUSE, NoColor);
      glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
   }

   SUMA_RETURN (YUP);
}         

/*!
   \brief A wrapper for SUMA_Paint_SO_ROIplanes
   
   Notification of afni is done if requested 
*/
SUMA_Boolean SUMA_Paint_SO_ROIplanes_w (SUMA_SurfaceObject *SO, 
                                       SUMA_DO* dov, int N_do)
{
   static char FuncName[]={"SUMA_Paint_SO_ROIplanes_w"};
   NI_element **nelv=NULL;
   int N_nelv = 0, ii=0;
   SUMA_Boolean CreateNel, LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   CreateNel = SUMAg_CF->ROI2afni;
   if (!SUMA_Paint_SO_ROIplanes (SO, SUMAg_DOv, SUMAg_N_DOv,
                                 &CreateNel,
                                 &nelv, &N_nelv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes.");
      SUMA_RETURN(NOPE);
   }
   
   if (SUMAg_CF->ROI2afni != CreateNel) {
      /* it was turned off in the function */
      SUMAg_CF->ROI2afni = CreateNel;
      if (SUMAg_CF->X->DrawROI) {
         XmToggleButtonSetState (SUMAg_CF->X->DrawROI->AfniLink_tb, 
                                 SUMAg_CF->ROI2afni, NOPE);
      }
   }
   
   if (SUMAg_CF->ROI2afni) {
      SUMA_LH("Should send nels to AFNI...");
      if (N_nelv) {
         for (ii=0; ii < N_nelv; ++ii) {
            SUMA_LH("Send this nel to AFNI.");
            /* SUMA_ShowNel((void*)nelv[ii]);*/
            /* Here you should write elements to SUMA_TO_MATLAB_STREAM
            Also, you should have a variable saying how encoding should
            be done for each of the streams */
            if (NI_write_element( SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , 
                                  nelv[ii] , NI_BINARY_MODE ) < 0) {
               SUMA_SLP_Err("NI_write_element failed.");
            }
            SUMA_LH("Free this nel.");
            NI_free_element(nelv[ii]) ; nelv[ii] = NULL;
         }
         SUMA_LH("Now free nelv");
         SUMA_free(nelv);nelv = NULL;
      }
   }
   
   SUMA_RETURN(YUP);
   
}

/*!
   \brief Where real men draw their ROIs
   
   - First the function creates a list
   of the various ROI planes on SO
   - For each plane
      - Fill them up with ROIs nodes, do mixing if necessary
      - If CreateNel is YUP, a NI data set of the type
       SUMA_NODE_ROI is created for each color plane.
       These data sets are meant to be sent to AFNI in realtime.
       You will have to free nelvp
   
   
*/
SUMA_Boolean SUMA_Paint_SO_ROIplanes ( SUMA_SurfaceObject *SO, 
                                       SUMA_DO* dov, int N_do, 
                                       SUMA_Boolean *CreateNel,
                                       NI_element ***nelvp, int *N_nelvp)
{
   static char FuncName[]={"SUMA_Paint_SO_ROIplanes"};
   DList * ROIPlaneList = NULL;
   SUMA_ROI_PLANE *Plane = NULL;
   int *N_ColHist = NULL, *ivect = NULL, *Nodes=NULL, *ilab=NULL, *labvect=NULL;
   float *r=NULL, *g=NULL, *b=NULL, *rvect=NULL, *gvect=NULL, *bvect=NULL;
   float FillColor[4]={0.0, 0.0, 0.0, 1.0};
   int i= 0, ii= 0, N_NewNode = 0, istore= 0, OverInd=-1, 
      inode= 0, i_D_ROI= 0, LastOfPreSeg= 0, N_Nodes=0;
   SUMA_OVERLAY_PLANE_DATA sopd;
   DListElmt *NextPlaneElm = NULL, *NextROIElm = NULL, *NextElm=NULL;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   SUMA_ROI_DATUM *ROId=NULL;
   NI_element **nelv = NULL;
   char *mapname=NULL;
   char *eee = NULL;
   DList *list=NULL;
   static int iwarn=0;
   SUMA_EngineData *ED = NULL;
   SUMA_Boolean Unique = NOPE;
   static int iwarn2=0;
   SUMA_Boolean LocalHead = NOPE;
            
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   /* select the color map */
   {
      eee = getenv("SUMA_ROIColorMap");
      if (eee) {
         if (!strcmp(eee, "roi256")) {
            mapname = "ROI_i256";
            if (!iwarn) SUMA_S_Note( "roi256 colormap is now ROI_i256\n"
                           "To use old roi256, use ygbrp256");
            ++iwarn;   
         }else if (!strcmp(eee, "roi128")) {
            mapname = "ROI_i128";
            if (!iwarn) SUMA_S_Note( "roi128 colormap is now ROI_i128\n"
                           "To use old roi128, use ygbrp128"); 
            ++iwarn;   
         }else if (!strcmp(eee, "roi64")) {
            mapname = "ROI_i64";
            if (!iwarn) SUMA_S_Note( "roi64 colormap is now ROI_i64\n"
                           "To use old roi64, use ygbrp64"); 
            ++iwarn;   
         }else if (SUMA_StandardMapIndex(eee) >= 0) {
            mapname = eee;
         } else {
            mapname = "ROI_i64";
            if (LocalHead) 
               fprintf(SUMA_STDERR, "%s: Unrecognized colormap %s.\n"
                                    " Using %s instead.\n", 
                                    FuncName, eee, mapname);
         }
      } else {
         mapname = "ROI_i64";
         if (LocalHead) 
            fprintf(SUMA_STDERR,
               "%s: Undefined environment. Using default ROI colormap %s\n", 
               FuncName, mapname);
      }
   }
   if (LocalHead) {
      int N_tmp;
      char *nm_tmp = 
         SUMA_StandardMapName (SUMA_StandardMapIndex(mapname), &N_tmp);
         fprintf(SUMA_STDERR,
            "%s: mapcode = %d, named %s %d cols\n", 
            FuncName, SUMA_StandardMapIndex(mapname), nm_tmp, N_tmp);
   }
   /* intilialize list */
   ROIPlaneList = SUMA_Addto_ROIplane_List (NULL, NULL, 0);
   /* go through all ROIs and place each under its ROI plane */
   for (i=0; i < N_do; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case ROIdO_type:
            D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
            break;
         default:
            D_ROI = NULL;
            break;
      }
      if (D_ROI && SUMA_isdROIrelated (D_ROI, (SUMA_ALL_DO *)SO)) {
         /* found one, put it in list if useful */
         if (D_ROI->ROIstrokelist) {
               SUMA_LH("Adding plane");
               /* add it to plane list */
               /* Add the plane, even if it has no strokes in it. 
               Otherwise, the last undo will have no effect on the 
               paint job. Bug fix Wed Jun 11 11:04:08 EDT 2003*/
               ROIPlaneList = SUMA_Addto_ROIplane_List ( ROIPlaneList,
                                                         dov, i);
                                                         
         }
      
      }
   }   
   
   if (*CreateNel && !nelvp) {
      SUMA_SLP_Err("nelvp is null!\n"
                   "Turning ROIlink Off.");
      *CreateNel = NOPE;
   }
   /* Deal with SUMA_TO_MATLAB_STREAM_INDEX here too */
   if (*CreateNel && !SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX]) {
      SUMA_SLP_Err(  "You are not connected\n"
                     "to AFNI. Turning \n"
                     "ROIlink Off.");
      *CreateNel = NOPE;
   }
   
   if (*CreateNel) { 
      /* user wants ni elements to sent to AFNI */
      nelv = (NI_element **) 
         SUMA_calloc(dlist_size(ROIPlaneList), sizeof(NI_element *));
      if (!nelv) {
         SUMA_SLP_Err("Failed to allocate\nfor nelv.");
         SUMA_RETURN(NOPE);
      }
      *N_nelvp = dlist_size(ROIPlaneList);
   }
   
   /* For each ROI plane */
   NextPlaneElm = NULL;
   for (i=0; i < dlist_size(ROIPlaneList); ++i) {
      if (!NextPlaneElm) NextPlaneElm = dlist_head(ROIPlaneList);
      else NextPlaneElm = NextPlaneElm->next;

      Plane = (SUMA_ROI_PLANE *)NextPlaneElm->data;
      
      if (LocalHead) 
         fprintf (SUMA_STDERR,
                  "%s: Processing plane %s\n", 
                  FuncName, Plane->name);
      
      if (!dlist_size(Plane->ROI_index_lst)) continue;
      
      /* allocate for node color history and all node colors */
      N_ColHist = (int *) SUMA_calloc(SO->N_Node, sizeof (int));
      r = (float *)SUMA_malloc (SO->N_Node*sizeof(float));
      g = (float *)SUMA_malloc (SO->N_Node*sizeof(float));
      b = (float *)SUMA_malloc (SO->N_Node*sizeof(float));
      if (*CreateNel) ilab = (int *) SUMA_calloc(SO->N_Node, sizeof (int));
      if (!N_ColHist || !r || !g || !b || (*CreateNel && !ilab)) {
         SUMA_SLP_Crit( "Failed to allocate.\n"
                        "for N_ColHist, r, g or b.");
         SUMA_RETURN(NOPE);
      }
      
      N_NewNode = 0; /* keep track of the total number of colored nodes */
      /* now go through each ROI in that plane and merge the colors */
      NextROIElm = NULL;
      do {
         SUMA_LH("New NextROIElm");
         if (!NextROIElm) NextROIElm = dlist_head(Plane->ROI_index_lst);
         else NextROIElm = NextROIElm->next;
         i_D_ROI = (INT_CAST)(NextROIElm->data);
         if (LocalHead) fprintf (SUMA_STDERR, 
                                 "%s: Working with DO %d/%d.\n",
                                 FuncName,  i_D_ROI, N_do);  
         D_ROI = (SUMA_DRAWN_ROI *) dov[i_D_ROI].OP;
         
         /* Set the fillcolor */
         if (D_ROI->ColorByLabel) {
            SUMA_LHv("Color by label %d (iLabel=%d)\n", 
                     D_ROI->ColorByLabel, D_ROI->iLabel);

            if (!SUMAg_CF->ROI_CM) {
               if (!(SUMAg_CF->ROI_CM = SUMA_FindNamedColMap (mapname))) {
                  if (!iwarn2) {
                     SUMA_SLP_Err( "Failed to create\n"
                                 "color map. Reverting\n"
                                 "to FillColors\n This message is shown once.");
                     ++iwarn2;
                  }
                  D_ROI->ColorByLabel = NOPE;
               }
               if (LocalHead) {
                  fprintf (SUMA_STDERR,
                           "%s:\nHave colormap of code %d, %dx%d colors.\n",
                           FuncName, SUMA_StandardMapIndex(mapname),
                           SUMAg_CF->ROI_CM->N_M[0], SUMAg_CF->ROI_CM->N_M[1]);
               }
               
               /* if connected to AFNI, send color map */
               /* Need to add handling for SUMA_TO_MATLAB_STREAM_INDEX */
               if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] &&
                   SUMAg_CF->ROI2afni) {
                  int mapcode = -1;
                  list = SUMA_CreateList();
                  ED = SUMA_InitializeEngineListData (SE_SendColorMapToAfni);
                  mapcode = SUMA_StandardMapIndex(mapname);
                  if (!SUMA_RegisterEngineListCommand (  
                                                   list, ED, 
                                                   SEF_i, (void*)&mapcode, 
                                                   SES_SumaWidget, NULL, NOPE, 
                                                   SEI_Head, NULL )) {
                     fprintf(SUMA_STDERR,
                              "Error %s: Failed to register command\n", 
                              FuncName);
                     SUMA_RETURN(NOPE);
                  }
                  if (!SUMA_Engine (&list)) {
                     fprintf( stderr, 
                              "Error %s: SUMA_Engine call failed.\n", 
                              FuncName);
                     SUMA_RETURN(NOPE);
                  }
                  
               }
            }
         } else {
            SUMA_LHv("Not color by label %d (iLabel=%d)\n", 
                     D_ROI->ColorByLabel, D_ROI->iLabel);
         } 
         
         /* make sure Color ByLabel is possible */
         if (D_ROI->ColorByLabel) {
            if (D_ROI->iLabel < 0 || D_ROI->iLabel >= SUMAg_CF->ROI_CM->N_M[0]) {
               SUMA_SLP_Err(  "ROI iLabel < 0 or \n"
                              "higher than the number\n"
                              "of colors in the map.\n"
                              "Reverting to FillColors");
               D_ROI->ColorByLabel = NOPE;
            }
         }
         
         if (D_ROI->ColorByLabel) {
            if (D_ROI->iLabel < 0 || D_ROI->iLabel >= SUMAg_CF->ROI_CM->N_M[0]) {
               SUMA_SLP_Err(  "ROI iLabel < 0 or \n"
                              "higher than the number\n"
                              "of colors in the map.\n"
                              "Reverting to FillColors");
               D_ROI->ColorByLabel = NOPE;
            }
            /* coloring for ROIs is straight forward, index based */
            D_ROI->FillColor[0] = SUMAg_CF->ROI_CM->M[D_ROI->iLabel][0];
            D_ROI->FillColor[1] = SUMAg_CF->ROI_CM->M[D_ROI->iLabel][1];
            D_ROI->FillColor[2] = SUMAg_CF->ROI_CM->M[D_ROI->iLabel][2];
            if (SUMAg_CF->ROI_CM->N_M[1] == 4) 
               D_ROI->FillColor[3] = SUMAg_CF->ROI_CM->M[D_ROI->iLabel][3];
            else D_ROI->FillColor[3] = 1.0;
         } else {
            SUMA_COPY_VEC (D_ROI->FillColor, FillColor, 4,float, float);
         }
         
         /* now for each node in the DrawnROI, add its color */
         N_Nodes = 0;
         Unique = YUP; /* This is to eliminate redundant nodes 
                        that can legally occur when the path loops
                        over itself. As a result, nodes
                        that are visited multiple times appear
                        brighter than the surrounding. */
         Nodes = SUMA_NodesInROI (D_ROI, &N_Nodes, Unique);
         if (Nodes) {
            for (ii=0; ii < N_Nodes; ++ii) {
               inode = Nodes[ii];
               if (inode >= SO->N_Node) {
                  SUMA_S_Errv("Have node %d, SO->N_Node = %d\n"
                     "Make sure ROI loaded belongs to this surface.\n",
                              inode , SO->N_Node) ;
                              SUMA_RETURN(NOPE);
               }
               if (!N_ColHist[inode]) {
                  r[inode] = D_ROI->FillColor[0]*D_ROI->FillColor[3];
                  g[inode] = D_ROI->FillColor[1]*D_ROI->FillColor[3];
                  b[inode] = D_ROI->FillColor[2]*D_ROI->FillColor[3];
                  if (*CreateNel) ilab[inode] = D_ROI->iLabel;
                  ++N_NewNode;
               } else { /* already used up color, add new color */
                  SUMA_LH("Revisiting Color");
                  r[inode] = r[inode] + D_ROI->FillColor[0]*D_ROI->FillColor[3];
                  g[inode] = g[inode] + D_ROI->FillColor[1]*D_ROI->FillColor[3];
                  b[inode] = b[inode] + D_ROI->FillColor[2]*D_ROI->FillColor[3];
                  if (*CreateNel) {
                     /* IGNORE repeats for the same node for now */
                  }
               }
               ++N_ColHist[inode];
            }
            
            SUMA_free(Nodes);
         }
      } while (NextROIElm != dlist_tail(Plane->ROI_index_lst));
      
      SUMA_LH("Scaling and storing ");
      /* create a conveninent list of the colors that goes into */
      ivect = (int *)SUMA_malloc(N_NewNode * sizeof(int));
      rvect = (float *)SUMA_malloc(N_NewNode * sizeof(float));
      gvect = (float *)SUMA_malloc(N_NewNode * sizeof(float));
      bvect = (float *)SUMA_malloc(N_NewNode * sizeof(float));
      if (*CreateNel) labvect = (int *)SUMA_malloc(N_NewNode * sizeof(int));
      if (!ivect || !rvect || !gvect || !bvect || (*CreateNel && !labvect)) {
         SUMA_SLP_Crit( "Failed to allocate.\n"
                        "for *vect family");
         SUMA_RETURN(NOPE);
      }
      
      istore = 0;
      for (ii=0; ii < SO->N_Node; ++ii) {
         if (N_ColHist[ii]) {
            #if 0
               /* You do not want to average the colors after summing them
               because that will have the effect of dimming them.
               Say you had 0 0 1 and 0 1 0
               You'll end up with 0 0.5 0.5 which does not have the 
               same brightness as the original colors.
               You might want to make the brightness uniform but
               we leave that for posterity ...
               */ 
               if (N_ColHist[ii] > 1) {
                  /* scale the summed colors for that plane */
                  
                  r[ii] /= N_ColHist[ii];
                  g[ii] /= N_ColHist[ii];
                  b[ii] /= N_ColHist[ii];
               }
            #endif
            /* put the colors in the short vectors */
            ivect[istore] = ii;
            rvect[istore] = r[ii];
            gvect[istore] = g[ii];
            bvect[istore] = b[ii];
            if (*CreateNel) labvect[istore] = ilab[ii];
            ++istore;
         }
      }
      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: N_NewNode = %d, istore = %d.\n",
                                    FuncName, N_NewNode, istore);
      SUMA_LH("Freedom");
      /* free the big ones */
      if (N_ColHist) SUMA_free(N_ColHist); N_ColHist = NULL;
      if (r) SUMA_free(r); r = NULL;
      if (g) SUMA_free(g); g = NULL;
      if (b) SUMA_free(b); b = NULL;
      if (ilab) SUMA_free(ilab); ilab = NULL;
      
   /* put the colors in a color plane */
      memset(&sopd, 0, sizeof(SUMA_OVERLAY_PLANE_DATA)); 
      sopd.N = N_NewNode;
      sopd.Type = SOPT_ifff;
      sopd.Source = SES_Suma;
      sopd.GlobalOpacity = 0.3;
      sopd.isBackGrnd = NOPE;
      sopd.Show = YUP;
      sopd.DimFact = 0.5;
      sopd.i = (void *)ivect;
      sopd.r = (void *)rvect;
      sopd.g = (void *)gvect;
      sopd.b = (void *)bvect;
      sopd.a = NULL;

      SUMA_LH("Calling SUMA_iRGB_to_OverlayPointer");
      if (!SUMA_iRGB_to_OverlayPointer (  SO, Plane->name, 
                                          &sopd, &OverInd, 
                                          dov, N_do, SUMAg_CF->DsetList)) {
         SUMA_SLP_Err("Failed to fetch or create overlay pointer.");
         SUMA_RETURN(NOPE);
      }      
      SUMA_LH("Returned SUMA_iRGB_to_OverlayPointer");
      
      if (*CreateNel) {
         NI_element *nel = NULL;
         char *TargetVol=NULL;
         
         /* form the nel for this plane */
         SUMA_allow_nel_use(1);
         nel = SUMA_NewNel ( SUMA_NODE_ROI, /* one of SUMA_DSET_TYPE */
                       SO->LocalDomainParentID, /* idcode of Domain Parent */
                       NULL, /* idcode of geometry parent, not useful here*/
                       N_NewNode,
                       NULL, 
                       NULL); /* Number of elements */
         
         if (!nel) {
            SUMA_SLP_Err("Failed in SUMA_NewNel");
            SUMA_RETURN(NOPE);
         }
         
         if (N_NewNode) {
            /* Add the index column */
            SUMA_LH("Adding index column...");
            SUMA_allow_nel_use(1);
            if (!SUMA_AddNelCol (nel, "node index", 
                                 SUMA_NODE_INDEX, (void *)ivect, NULL, 1)) {
               SUMA_SL_Err("Failed in SUMA_AddNelCol");
               SUMA_RETURN(NOPE);
            }

            /* Add the label column */
            SUMA_LH("Adding label column...");
            SUMA_allow_nel_use(1);
            if (!SUMA_AddNelCol (nel, "integer label", 
                                 SUMA_NODE_ILABEL, (void *)labvect, NULL, 1)) {
               SUMA_SL_Err("Failed in SUMA_AddNelCol");
               SUMA_RETURN(NOPE);
            }
         }
         
         /* What is the target volume for this nel */
         TargetVol = SUMA_append_replace_string(SO->Group, Plane->name, "-", 0); 
         NI_set_attribute (nel, "target_volume", TargetVol);
         SUMA_free(TargetVol);
         
         /* which colormap was used */
         NI_set_attribute (nel, "color_map", SUMAg_CF->ROI_CM->Name);
         
         /* what is the volume parent ? This one will act as a gridparent 
            for the functional data set*/
         NI_set_attribute (nel, "volume_idcode", SO->VolPar->vol_idcode_str);
         
         nelv[i] = nel; nel = NULL;
         
         /* DO NOT FREE ivect, it is used in sopd */
         SUMA_free(labvect);labvect=NULL;
      }
      
   } 
   
   if (!dlist_size(ROIPlaneList)) {
      /* 
      SUMA_SLP_Err(  "Flow error\n"
                     "You are not expected\n"
                     "to land here." );
      Do not complain, you can get here if you
      are connected to SUMA. 
      Aint nothing wrong with this.
      */
      N_NewNode = 0;
      ivect = NULL; 
      rvect = NULL;
      gvect = NULL;
      bvect = NULL;
      if (*CreateNel && labvect) SUMA_free(labvect); labvect = NULL;  
   }
   
      
   SUMA_LH("Destroying list");
   /* destroy plane list */
   dlist_destroy (ROIPlaneList); SUMA_free(ROIPlaneList); ROIPlaneList = NULL; 

   /* Set the remix flag for that surface */
   if(!SUMA_SetRemixFlag (SO->idcode_str, SUMAg_SVv, SUMAg_N_SVv)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SetRemixFlag.\n", FuncName);
      SUMA_RETURN(NOPE);
   }   
   
   if (*CreateNel) {
      *nelvp = nelv;
   }
   
   /* should be cool, now return */   
   SUMA_RETURN(YUP);
}

/*!
   \brief int * SUMA_NodesInROI (SUMA_DRAWN_ROI *D_ROI, int *N_Nodes, SUMA_Boolean Unique) 
   Returns a vector containing the number of nodes making up an ROI 
   
   \param D_ROI (SUMA_DRAWN_ROI *)
   \param N_Nodes (int *) updated with the total number of nodes in Nodes
   \param Unique (SUMA_Boolean) Remove repeated node occurences
   \return Nodes (int *) N_Nodesx1 vector of nodes forming ROI
   
   - Nodes (and N_Nodes) might have duplicate node entries, unless you set Unique
*/
int * SUMA_NodesInROI (SUMA_DRAWN_ROI *D_ROI, int *N_Nodes, SUMA_Boolean Unique) 
{
   static char FuncName[]={"SUMA_NodesInROI"};
   int *Nodes = NULL, LastOfPreSeg, N_max = -1, ii;
   DListElmt *NextElm = NULL;
   SUMA_ROI_DATUM *ROId=NULL;
   
   SUMA_ENTRY;
   
   if (!dlist_size(D_ROI->ROIstrokelist)) {
      *N_Nodes = 0;
      SUMA_RETURN (NULL);
   }
   
   /* a quick count of number of nodes */
   SUMA_ROI_CRUDE_COUNT_NODES(D_ROI, N_max);
   
   if (!N_max) {
      *N_Nodes = 0;
      SUMA_RETURN (NULL);
   }
    
   Nodes = (int*)SUMA_calloc(N_max,sizeof(int));
   if (!Nodes) {
      SUMA_SLP_Crit("Failed to allocate for Nodes.");
      *N_Nodes = -1;
      SUMA_RETURN(NULL);
   }
     
   /* Fill 'er up */
   *N_Nodes = 0;
   LastOfPreSeg = -1; /* index of last node in previous segment */
   NextElm = NULL;
   do {
      if (!NextElm) NextElm = dlist_head(D_ROI->ROIstrokelist);
      else NextElm = dlist_next(NextElm);

      ROId = (SUMA_ROI_DATUM *)NextElm->data;
      
      for (ii=0; ii < ROId->N_n; ++ii) {
         if (ROId->nPath[ii] != LastOfPreSeg) {
            Nodes[*N_Nodes] = ROId->nPath[ii];
            ++ *N_Nodes;
         }
      }
      if (ROId->N_n) { /* store last node of segment */
         LastOfPreSeg = ROId->nPath[ROId->N_n - 1];
      } else {
         LastOfPreSeg = -1;
      }
   } while (NextElm != dlist_tail(D_ROI->ROIstrokelist));

   /* user wants sorting ? */
   if (Unique) {
      int *Nodes_Unq = NULL;
      int N_Nodes_Unq = -1;
      Nodes_Unq = SUMA_UniqueInt (Nodes, *N_Nodes, &N_Nodes_Unq, 0);
      if (Nodes) SUMA_free(Nodes);  Nodes = NULL; 
      *N_Nodes = N_Nodes_Unq;
      Nodes = Nodes_Unq;
   } 
      
   SUMA_RETURN(Nodes);
   
}

SUMA_Boolean SUMA_MinMaxNodesInROI (SUMA_DRAWN_ROI *D_ROI, 
                                    int MinMax[]) 
{
   static char FuncName[]={"SUMA_MinMaxNodesInROI"};
   int LastOfPreSeg, N_max = -1, ii;
   DListElmt *NextElm = NULL;
   SUMA_ROI_DATUM *ROId=NULL;
   
   SUMA_ENTRY;
   
   MinMax[0] = -1; MinMax[1] = -1;
   
   if (!D_ROI || !dlist_size(D_ROI->ROIstrokelist)) {
      SUMA_RETURN (NOPE);
   }
   
   /* a quick count of number of nodes */
   SUMA_ROI_CRUDE_COUNT_NODES(D_ROI, N_max);
   
   if (!N_max) {
      SUMA_RETURN (NOPE);
   }
    
   MinMax[0] = 10e8; 
   NextElm = NULL;
   do {
      if (!NextElm) NextElm = dlist_head(D_ROI->ROIstrokelist);
      else NextElm = dlist_next(NextElm);

      ROId = (SUMA_ROI_DATUM *)NextElm->data;
      
      for (ii=0; ii < ROId->N_n; ++ii) {
         if (ROId->nPath[ii] > MinMax[1]) MinMax[1] = ROId->nPath[ii];
         else if (ROId->nPath[ii] < MinMax[0]) MinMax[0] = ROId->nPath[ii];
      }
   } while (NextElm != dlist_tail(D_ROI->ROIstrokelist));

   SUMA_RETURN(YUP);
}

void SUMA_Free_ROI_PlaneData (void *da)
{
   static char FuncName[]={"SUMA_Free_ROI_PlaneData"};
   SUMA_ROI_PLANE *pl = NULL;
   
   SUMA_ENTRY;
   
   pl = (SUMA_ROI_PLANE *)da;
   
   if (!pl) SUMA_RETURNe;
   
   /* destroy the list containing ROIs belonging to plane */
   if (pl->ROI_index_lst) {
      dlist_destroy (pl->ROI_index_lst); 
      SUMA_free(pl->ROI_index_lst); pl->ROI_index_lst = NULL;
   }
   if (pl->name) SUMA_free(pl->name);
   
   /* now free the structure */
   SUMA_free(pl);
   
   SUMA_RETURNe;
}

/*!
   \brief Adds and ROI in the list of ROI planes
   
   ROIplaneList = SUMA_Addto_ROIplane_List (ROIplaneListIn,
                                             dov, idov);
   
   \param ROIplaneListIn (DList *) the list of 
      ROI planes
   \param dov (SUMA_DO *) vector of displayable objects
   \param idov (int) index into dov of DrawnROI object
   \return ROIplaneList DList *) the updated
      list of ROI planes
   
   - If the ROI is part of a new plane, 
   the plane is added to the list and the ROI
   is placed under that plane.
   - If the ROI is part of a plane in the list
   then the ROI is placed under that plane
   - The first time you call the function, 
   send in NULL for ROIplaneListIn to initialize
   the list.
   - The subsequent times you call the function
   use the returned ROIplaneList for ROIplaneListIn
   
*/
DList * SUMA_Addto_ROIplane_List (DList *ROIplaneListIn, SUMA_DO *dov, int idov)
{
   static char FuncName[]={"SUMA_Addto_ROIplane_List"};
   DList *ROIplaneList = NULL;
   DListElmt *NextElm = NULL;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   char *UsedName = NULL;
   SUMA_DO *doel = NULL;
   SUMA_ROI_PLANE *Plane=NULL;
   int i=0;
   SUMA_Boolean found = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ROIplaneListIn) { /* initialization land */
      ROIplaneList = (DList *)SUMA_calloc(1,sizeof(DList));
      dlist_init (ROIplaneList, SUMA_Free_ROI_PlaneData);
      SUMA_RETURN(ROIplaneList);
   } else {
      ROIplaneList = ROIplaneListIn;
   }
   
   doel = &(dov[idov]);
   
   if (doel->ObjectType != ROIdO_type) {
      SUMA_SLP_Crit("Only planning to deal\n"
                   "with ROIdO_type type");
      dlist_destroy(ROIplaneList); SUMA_free(ROIplaneList); ROIplaneList=NULL; 
      SUMA_RETURN(NULL);
   }
   
   D_ROI = (SUMA_DRAWN_ROI *)doel->OP;
   
   /* What is the name of this ROI's plane ?*/
   if (!D_ROI->ColPlaneName) {
      /* Bad, no color plane name, give it a fake one */
      UsedName = SUMA_copy_string("DefROIpl");
   }else {
      UsedName = SUMA_copy_string(D_ROI->ColPlaneName);
   }
   
   /* search for the plane name in the list */
   i = 0;
   found = NOPE;
   Plane = NULL;
   while (!found && i < ROIplaneList->size) {
      if (i == 0) NextElm = dlist_head(ROIplaneList);
      else NextElm = dlist_next(NextElm);
      Plane = (SUMA_ROI_PLANE *)NextElm->data;
      if (strcmp (UsedName,Plane->name) == 0) {
         SUMA_LH("PlaneFound");
         found = YUP;
         SUMA_free(UsedName); /* no longer needed */
      }
      ++i;
   }
   
   if (!found) { /* must create this plane */
      Plane = (SUMA_ROI_PLANE *)SUMA_calloc(1,sizeof(SUMA_ROI_PLANE));
      Plane->name = UsedName; /* preserved, don't go freeing UsedName later! */
      Plane->ROI_index_lst = (DList *) SUMA_calloc(1,sizeof(DList));
      dlist_init(Plane->ROI_index_lst, NULL);
      dlist_ins_next(ROIplaneList, dlist_tail(ROIplaneList), (void *)Plane);
   }
   
   /* now put the ROI in question in that list, 
      easiest is to store its index into dov */
   dlist_ins_next(Plane->ROI_index_lst, 
                  dlist_tail(Plane->ROI_index_lst), (VOID_CAST)idov);
   
   /* OK, done, now return */
   SUMA_RETURN(ROIplaneList);
}

/*! Create the cross hair */
SUMA_Boolean SUMA_DrawCrossHair (SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawCrossHair"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   static GLdouble radsph, fac;
   static GLfloat gapch, radch;
   GLboolean gl_dt;
   float origwidth = 0.0, off[3]={0.0, 0.0, 0.0}, *xyz=NULL;
   int scl = 0;
   SUMA_CrossHair* Ch = sv->Ch;
   SUMA_CLUST_DATUM *cd = NULL;
   SUMA_SurfaceObject *SO=NULL;
   
   SUMA_ENTRY;
   
   SO = SUMA_SV_Focus_SO(sv);
   
   if (sv->DO_PickMode) { /* nothing to draw in this mode */
      SUMA_RETURN(YUP);
   }
   
   scl = 0;
   if (SO) {
      if (SO->PolyMode == SRM_ViewerDefault && sv->PolyMode != SRM_Fill) {
         scl = 1;
      } else if (SO->PolyMode != SRM_ViewerDefault && SO->PolyMode != SRM_Fill) {
         scl = 1;
      }
      /* is this a flatty ? */
      if (Ch->datumID >= 0 && Ch->datumID < SO->N_Node 
          && SO->NodeNormList && SO->NodeDim == 3 &&
          SO->EmbedDim == 2) {
         off[0] = SO->NodeNormList[Ch->datumID*3];
         off[1] = SO->NodeNormList[Ch->datumID*3+1];   
         off[2] = SO->NodeNormList[Ch->datumID*3+2];   
      } 
   } else {
      if (sv->PolyMode != SRM_Fill) {
         scl = 1;
      }
   }    
   
   Ch->c[0] = Ch->c[0]+off[0];
   Ch->c[1] = Ch->c[1]+off[1];
   Ch->c[2] = Ch->c[2]+off[2];
   
   if (scl) {
      fac = SUMA_MAX_PAIR(sv->ZoomCompensate, 0.03);
      radsph = Ch->sphrad*fac*(SUMA_sv_auto_fov(sv)/FOV_INITIAL);
      gapch = Ch->g*fac*(SUMA_sv_auto_fov(sv)/FOV_INITIAL);
      radch = Ch->r*fac*(SUMA_sv_auto_fov(sv)/FOV_INITIAL);
   } else {
      fac = (SUMA_sv_auto_fov(sv)/FOV_INITIAL);
      radsph = Ch->sphrad*fac;
      gapch = Ch->g*fac;
      radch = Ch->r*fac;
   }
   if (!(gl_dt = glIsEnabled(GL_DEPTH_TEST)))  
      glEnable(GL_DEPTH_TEST);   /* To hide cross hair as it gets hidden
                                    by surfaces */
   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   glLineWidth(Ch->LineWidth);
   /*fprintf(SUMA_STDOUT, "Center: %f, %f, %f. Gap %f, Radius: %f\n",\
      Ch->c[0], Ch->c[2], Ch->c[2], gapch, radch);*/
                        /* turn off ambient and diffuse components */
      glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
      glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
      if (gapch) { /* gap */
                           /*turn on emissivity for axis*/
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->XaxisColor); 
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0] - radch, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] - gapch, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] + radch, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] + gapch, Ch->c[1], Ch->c[2]);
         glEnd();  
                           /*turn on emissivity for axis*/
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->YaxisColor); 
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1] - radch, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] - gapch, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] + radch, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] + gapch, Ch->c[2]);
         glEnd();  
                           /*turn on emissivity for axis*/
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->ZaxisColor); 
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - radch);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - gapch);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + radch);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + gapch);
         glEnd();  

      }/*gap */ else {/*no gap */
                           /*turn on emissivity for axis*/
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->XaxisColor); 
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0] - radch, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] + radch, Ch->c[1], Ch->c[2]);
         glEnd();  
                           /*turn on emissivity for axis*/
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->YaxisColor); 
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1] - radch, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] + radch, Ch->c[2]);
         glEnd();  
                           /*turn on emissivity for axis*/
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->ZaxisColor); 
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - radch);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + radch);
         glEnd();  
      }
                           /*turn off emissivity for axis*/
      glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 

   
   if (Ch->ShowSphere) {
      /*fprintf(SUMA_STDOUT, "SHOWING SPHERE\n");*/
                        /*turn on emissivity for sphere */
      glMaterialfv(GL_FRONT, GL_EMISSION, Ch->sphcol); 
      glTranslatef (Ch->c[0], Ch->c[1],Ch->c[2]);
      gluSphere(Ch->sphobj, radsph, Ch->slices, Ch->stacks);
      glTranslatef (-Ch->c[0], -Ch->c[1],-Ch->c[2]);
                        /*turn off emissivity for axis*/
      glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
   }
   
   if (SO && SO->SurfCont &&
       SUMA_NodeClustNumber(SO->SurfCont->curColPlane, Ch->datumID,
                            SO, &cd)) { 
            /* Mark node with maximum value in cluster where you just
               clicked, if possible. The Dale Stephens Option */
      
      if (cd->maxabsnode >=0) {
         xyz = SO->NodeList+SO->NodeDim*cd->maxabsnode;
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->sphcolCmax); 
         glTranslatef (xyz[0], xyz[1],xyz[2]);
         gluSphere(Ch->sphobjCmax, radsph, 4, 4);
         glTranslatef (-xyz[0], -xyz[1],-xyz[2]);
                           /*turn off emissivity for axis*/
         glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
      }
   }
   
   glLineWidth(origwidth);
   
   /* DEPTH_TEST is on for this function, turn it off
      if that was the case entering the function */ 
   if (!gl_dt) glDisable(GL_DEPTH_TEST);

   Ch->c[0] = Ch->c[0]-off[0];
   Ch->c[1] = Ch->c[1]-off[1];
   Ch->c[2] = Ch->c[2]-off[2];
   
   SUMA_RETURN (YUP);
}

/* Allocate for a CrossHair object */
SUMA_CrossHair* SUMA_Alloc_CrossHair (void)
{   
   static char FuncName[]={"SUMA_Alloc_CrossHair"};
   SUMA_CrossHair* Ch;
   
   SUMA_ENTRY;

   Ch = SUMA_calloc(1,sizeof (SUMA_CrossHair));
   if (Ch == NULL) {
      fprintf(stderr,"SUMA_Alloc_CrossHair Error: Failed to allocate Ch\n");
      SUMA_RETURN (NULL);
   }
   
   /* setup some default values */
   Ch->XaxisColor[0] = 1.0;
   Ch->XaxisColor[1] = 0.0;
   Ch->XaxisColor[2] = 0.0;
   Ch->XaxisColor[3] = 1.0;
   
   Ch->YaxisColor[0] = 0.0;
   Ch->YaxisColor[1] = 1.0;
   Ch->YaxisColor[2] = 0.0;
   Ch->YaxisColor[3] = 1.0;
   
   Ch->ZaxisColor[0] = 0.0;
   Ch->ZaxisColor[1] = 0.0;
   Ch->ZaxisColor[2] = 1.0;
   Ch->ZaxisColor[3] = 1.0;
   
   Ch->LineWidth = SUMA_CROSS_HAIR_LINE_WIDTH;
   Ch->Stipple = SUMA_SOLID_LINE;
   Ch->c[0] = Ch->c[1] = Ch->c[2] = 0.0;
   
   Ch->g = SUMA_CROSS_HAIR_GAP/SUMA_DimSclFac(NULL, NULL); 
   Ch->r = SUMA_CROSS_HAIR_RADIUS/SUMA_DimSclFac(NULL, NULL); 
   
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
   
   Ch->sphcol[0] = 1.0; Ch->sphcol[1] = 1.0; 
   Ch->sphcol[2] = 0.0; Ch->sphcol[3] = 0.0;
   Ch->sphrad = SUMA_CROSS_HAIR_SPHERE_RADIUS/SUMA_DimSclFac(NULL, NULL);
   Ch->slices = 10;
   Ch->stacks = 10;
   
   Ch->adoID = -1;
   Ch->datumID = -1;
   Ch->secID = -1;
   
   Ch->sphobjCmax = gluNewQuadric();
   Ch->sphcolCmax[0] = 0.0; Ch->sphcolCmax[1] = 0.0; 
   Ch->sphcolCmax[2] = 0.0; Ch->sphcolCmax[3] = 0.0;
   #ifdef SUMA_SOLID_LOCAL
      gluQuadricDrawStyle (Ch->sphobjCmax, GLU_FILL); 
      gluQuadricNormals (Ch->sphobjCmax , GLU_SMOOTH);
   #else
      gluQuadricDrawStyle (Ch->sphobjCmax, GLU_LINE);
      gluQuadricNormals (Ch->sphobjCmax , GLU_NONE);
   #endif
   SUMA_RETURN (Ch);
}

/*! Free a CrossHair object */
void SUMA_Free_CrossHair (SUMA_CrossHair *Ch)
{
   static char FuncName[]={"SUMA_Free_CrossHair"};
   
   SUMA_ENTRY;

   if (Ch->sphobj) gluDeleteQuadric(Ch->sphobj);
   if (Ch->sphobjCmax) gluDeleteQuadric(Ch->sphobjCmax);
   if (Ch) SUMA_free(Ch);
   SUMA_RETURNe;
}


/* Allocate for a SphereMarker object */
SUMA_SphereMarker* SUMA_Alloc_SphereMarker (void)
{   
   static char FuncName[]={"SUMA_Alloc_SphereMarker"};
   SUMA_SphereMarker* SM;
   
   SUMA_ENTRY;

   SM = (SUMA_SphereMarker*)SUMA_calloc(1,sizeof (SUMA_SphereMarker));
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
   SM->sphcol[0] = 0.50; SM->sphcol[1] = 0.5; SM->sphcol[2] = 1.0; 
                                              SM->sphcol[3] = 1.0;
   SM->sphrad = SUMA_SELECTED_NODE_SPHERE_RADIUS/SUMA_DimSclFac(NULL, NULL);
   SM->slices = 10;
   SM->stacks = 10;
   SM->c[0] = SM->c[1] = SM->c[2] = 0.0; 
   
   SUMA_RETURN (SM);
}

/*! Free a SphereMarker object */
void SUMA_Free_SphereMarker (SUMA_SphereMarker *SM)
{
   static char FuncName[]={"SUMA_Free_SphereMarker"};
   
   SUMA_ENTRY;

   if (SM->sphobj) gluDeleteQuadric(SM->sphobj);
   if (SM) SUMA_free(SM);
   SUMA_RETURNe;
}

/*! Create the highlighted faceset  marker */
SUMA_Boolean SUMA_DrawFaceSetMarker (SUMA_FaceSetMarker* FM, 
                                     SUMA_SurfaceViewer *sv)
{   
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0}, dx, dy, dz, fac;
   static char FuncName[]={"SUMA_DrawFaceSetMarker"};
   
   SUMA_ENTRY;

   fac = (SUMA_SELECTED_FACESET_OFFSET_FACTOR);
   dx = fac * FM->NormVect[0];
   dy = fac * FM->NormVect[1];
   dz = fac * FM->NormVect[2];
    
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
   
   SUMA_ENTRY;

   FM = SUMA_calloc(1,sizeof (SUMA_FaceSetMarker));
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

   SUMA_ENTRY;

   if (FM) SUMA_free(FM);
   SUMA_RETURNe;
}

/*!
   Modify the attributes of node objects, based on the data
*/
SUMA_Boolean SUMA_ApplyDataToNodeObjects(
            SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_ApplyDataToNodeObjects"};
   GLfloat *colp = NULL;
   SUMA_NIDO *nido=NULL;
   int ip, i, node, node4;
   float colv[4];
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!(colp = SUMA_GetColorList (sv, SurfObj->idcode_str))) SUMA_RETURN(NOPE);

   if (! (SurfObj->NodeObjects && 
          SurfObj->NodeObjects->ObjectType == NIDO_type) ) {
      SUMA_RETURN(NOPE);
   }
   
   if (!(nido = SurfObj->NodeObjects->OP)) SUMA_RETURN(NOPE);
   
   for( ip=0 ; ip < nido->ngr->part_num ; ip++ ){ 
      nel = NULL;
      switch( nido->ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            SUMA_SL_Err(
               "Don't know what to do with a group element, ignoring.");
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)nido->ngr->part[ip] ;
            if (0 && LocalHead)  {
               SUMA_ShowNel(nel);
            }
            NI_GET_INT(nel, "node", node);
            if (!NI_GOT) break;
            node4=4*node;
            for (i=0; i<4; ++i) colv[i]=colp[node4+i];
            
            /* assign the color at that node */
            NI_SET_FLOATv(nel, "col", colv, 4);
            
            /* there are many more things to do depending on the type
            of nel, but for now, leave as is */
            
            break;
         default:
            SUMA_SL_Err(
               "Don't know what to make of this group element, ignoring.");
            break;
      }
   }
   SUMA_RETURN(YUP);
}
/*!
   Modify the attributes of node objects, based on the data
*/
SUMA_Boolean SUMA_ApplyDataToNodeNIDOObjects(
            SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_ApplyDataToNodeNIDOObjects"};
   GLfloat *colp = NULL;
   SUMA_NIDO *nido=NULL;
   int ip, i, node, node4;
   float colv[4];
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!(colp = SUMA_GetColorList (sv, SurfObj->idcode_str))) SUMA_RETURN(NOPE);

   if (! (SurfObj->NodeNIDOObjects) ) {
      SUMA_RETURN(NOPE);
   }
   
   for( ip=0 ; ip < SurfObj->N_Node ; ip++ ){ 
      nido = SurfObj->NodeNIDOObjects[ip];
      if (nido) {
         node4=4*ip; for (i=0; i<4; ++i) colv[i]=colp[node4+i];
         NI_SET_FLOATv(nido->ngr, "default_color", colv, 4);
         if (LocalHead && ip==0) {
            SUMA_LHv("Node 0 markre has [%f %f %f %f]\n",
                        colv[0], colv[1], colv[2], colv[3]);
         }  
      }
   }
   SUMA_RETURN(YUP);
}

/*! Create a tesselated mesh */
void SUMA_DrawMesh(SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *sv)
{  
   static char FuncName[]={"SUMA_DrawMesh"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   GLfloat *colp = NULL;
   int i, ii, ND, id, ip, NP, PolyMode, sz[2]={0, 0};
   SUMA_DRAWN_ROI *DrawnROI = NULL;
   static GLuint texName; 
   GLfloat rotationMatrix[4][4];
   GLenum Face=GL_FRONT;
   NI_element *texnel=NULL;
   DList *st=NULL;
   SUMA_TRANS_MODES trmode;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Entered DrawMesh");
   
   if (  SurfObj->PolyMode == SRM_Hide || 
         sv->PolyMode == SRM_Hide ||
         SurfObj->TransMode == STM_16 ||
         sv->TransMode == STM_16) {
      SUMA_LH("Hiding surface"); 
      SUMA_RETURNe; 
   }

   if (sv->DO_PickMode) {
      SUMA_LH("No need to DrawMesh in DO picking mode");
      SUMA_RETURNe;
   }
   

   SUMA_LH("Might need to swap coords from the VisX transformed data");
   SUMA_VisX_Pointers4Display(SurfObj, 1);

   if (!SUMA_GLStateTrack( "new", &st, FuncName, NULL, NULL)) {
      SUMA_S_Err("Failed to create tracking list");
      SUMA_RETURNe; 
   } 
   /* check on rendering mode */
   if (SurfObj->PolyMode != SRM_ViewerDefault) {
     SUMA_LHv("Poly Mode %d\n", SurfObj->PolyMode);
     /* not the default, do the deed */
     #if 0 /* Need to start using MACRO below, but it is not working yet */
     SUMA_SET_GL_RENDER_MODE_TRACK(SurfObj->PolyMode, st); 
     #else
     SUMA_SET_GL_RENDER_MODE(SurfObj->PolyMode);
     #endif
   }

   /* check on rendering mode */
   if (SurfObj->TransMode == STM_ViewerDefault) {
      trmode = sv->TransMode;
   } else if (SurfObj->TransMode > STM_0) {
      trmode = SurfObj->TransMode;
   } else trmode = STM_0;
   
   if (trmode != STM_0) {
     /* not the default, do the deed */
     SUMA_LHv("Trans Mode %d\n", trmode);
     SUMA_SET_GL_TRANS_MODE(trmode, st); 
   }

   /* any texture for this surface? */
   texnel = SUMA_SO_NIDO_Node_Texture ( SurfObj, SUMAg_DOv, SUMAg_N_DOv, sv );
   if (texnel) {
      SUMA_LH(  "Creating texture, see init pp 415 in \n"
                "OpenGL programming guide, 3rd ed.");
      if (NI_IS_STR_ATTR_EQUAL(texnel,"read_status","fail")) {
         /* can't be read */
         SUMA_RETURNe;
      }

      if (!NI_IS_STR_ATTR_EQUAL(texnel,"read_status","read")) { /* read it */
         if (!SUMA_LoadImageNIDOnel(texnel)) {
            SUMA_RETURNe;
         }
         /* has the box size been determined (only 2 dimensions needed)?*/
         NI_GET_INTv(texnel,"box_size", sz, 2, LocalHead);
         if (!NI_GOT) {
            NI_GET_INT(texnel,"width", sz[0]);
            NI_GET_INT(texnel,"height", sz[1]);
            NI_SET_INTv(texnel,"box_size", sz, 2);
         }
         
      } 

      NI_GET_INTv(texnel,"box_size", sz, 2, LocalHead);

      /* For cool textures, see 
         http://www.filterforge.com/filters/category42-page1.html */
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
      NI_GET_INT(texnel,"texName",texName);
      if (!NI_GOT) {
         /* Need to generate texture */
         glGenTextures(1, &texName);
         /* Now store it */
         NI_SET_INT(texnel,"texName",texName);
      }
      glBindTexture(GL_TEXTURE_2D, texName);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S, GL_REPEAT); 
               /* GL_REPEAT, GL_CLAMP */
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T, GL_REPEAT); 
      glTexParameteri(  GL_TEXTURE_2D,
                        GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(  GL_TEXTURE_2D,
                        GL_TEXTURE_MIN_FILTER, GL_LINEAR);
         /* cotnrols interpolation of zoomed in/out texture,
         GL_LINEAR, GL_NEAREST, ... */
      glTexImage2D(  GL_TEXTURE_2D, 0, GL_RGBA, 
                     sz[0], sz[1], 0, GL_RGBA, 
                     GL_UNSIGNED_BYTE, texnel->vec[0]);
      glTexEnvf(  GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, 
                  SUMA_NIDO_TexEnvMode(texnel, GL_MODULATE));

      /* texture goes on surface which will be drawn later, 
         Set automatic texture coordinate generation */
      glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, 
                  SUMA_NIDO_TexCoordGen(texnel)); 
            /* GL_SPHERE_MAP, GL_EYE_LINEAR, GL_OBJECT_LINEAR */
      glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, 
                  SUMA_NIDO_TexCoordGen(texnel));
      glEnable(GL_TEXTURE_GEN_S);
      glEnable(GL_TEXTURE_GEN_T);
      glEnable(GL_TEXTURE_2D);
                     #if 0
                     SUMA_S_Note("I do not need all this");
                     glEnable(GL_CULL_FACE);
                     glEnable(GL_LIGHTING);
                     glEnable(GL_LIGHT0);
                     glEnable(GL_AUTO_NORMAL);
                     glEnable(GL_NORMALIZE);
                     glMaterialf(Face, GL_SHININESS, 64.0); 
                     #endif     
   }
         
   SUMA_LH("Draw Method");
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
            glVertex3fv(&SurfObj->NodeList[id]); 
               /* glVertex3f(0.1, 0.9, 0.0); */

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
         /* This allows each node to follow the color 
            specified when it was drawn */ 
         glColorMaterial(Face, GL_AMBIENT_AND_DIFFUSE); 
         glEnable(GL_COLOR_MATERIAL);
         
         /*Now setup various pointers*/
         glEnableClientState (GL_COLOR_ARRAY);
         glEnableClientState (GL_VERTEX_ARRAY);
         glEnableClientState (GL_NORMAL_ARRAY);
         colp = SUMA_GetColorList (sv, SurfObj->idcode_str);
         if (!colp) { /* no color list, try  PermCol */
            if (SurfObj->PermCol) {
               glColorPointer (4, GL_FLOAT, 0, SurfObj->PermCol);
            } else {
               SUMA_SL_Err("Null Color Pointer.");
            }
         } else { 
            glColorPointer (4, GL_FLOAT, 0, colp); 

         }
         glVertexPointer (3, GL_FLOAT, 0, SurfObj->glar_NodeList);
         glNormalPointer (GL_FLOAT, 0, SurfObj->glar_NodeNormList);
         if (LocalHead) 
            fprintf(stdout, "Ready to draw Elements %d\n", SurfObj->N_FaceSet); 
         switch (RENDER_METHOD) {
            case TRIANGLES:
               glDrawElements (  GL_TRIANGLES, (GLsizei)SurfObj->N_FaceSet*3, 
                                 GL_UNSIGNED_INT, SurfObj->glar_FaceSetList);
               break;
            case POINTS:
               glPointSize(4.0); /* keep outside of glBegin */
               /* it is inefficient to draw points using the 
                  glar_FaceSetList because nodes are listed more 
                  than once. You are better off creating an index 
                  vector into glar_NodeList to place all the points, just once*/ 
               glDrawElements (  GL_POINTS, (GLsizei)SurfObj->N_FaceSet*3, 
                                 GL_UNSIGNED_INT, SurfObj->glar_FaceSetList);
               break;
         } /* switch RENDER_METHOD */
         
        
         if (texnel) {
            /* kill baby kill */
            texnel = NULL; /* don't leave this function 
                                       with pointer copy */
            glDisable(GL_TEXTURE_2D);
            glDisable(GL_TEXTURE_GEN_T);
            glDisable(GL_TEXTURE_GEN_S);
         }
         
         /*fprintf(stdout, "Disabling clients\n");*/
         glDisableClientState (GL_COLOR_ARRAY);   
         glDisableClientState (GL_VERTEX_ARRAY);
         glDisableClientState (GL_NORMAL_ARRAY);   
         /*fprintf(stdout, "Out SUMA_DrawMesh, ARRAY mode\n");*/
         
         glDisable(GL_COLOR_MATERIAL);
         
         /* draw dset contours (only label dset for now) */
         SUMA_LH("Dset contours ");
         if (!SUMA_Draw_SO_Dset_Contours (SurfObj,  sv)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed in drawing Dset Contour objects.\n", 
                     FuncName);
         }
         /* draw surface ROIs */
         SUMA_LH("ROIs");
         if (!SUMA_Draw_SO_ROI (SurfObj, SUMAg_DOv, SUMAg_N_DOv, sv)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed in drawing ROI objects.\n", FuncName);
         }
         /* Draw Axis */
         SUMA_LH("Axis");
         if (SurfObj->MeshAxis && SurfObj->ShowMeshAxis)   {
            if (!SUMA_DrawAxis (SurfObj->MeshAxis, sv)) {
               fprintf( stderr,
                        "Error SUMA_DrawAxis: Unrecognized Stipple option\n");
            }
         }
         /* Draw node-based vectors */
         SUMA_LH("NBV");
         if (!SUMA_Draw_SO_NBV (SurfObj, SUMAg_DOv, SUMAg_N_DOv, sv)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed in drawing NBV objects.\n", FuncName);
         }
         
         /* Draw node-based spheres */
         SUMA_LH("NBSP");
         if (!SUMA_Draw_SO_NBSP (SurfObj, SUMAg_DOv, SUMAg_N_DOv, sv)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed in drawing NBSP objects.\n", FuncName);
         }
         
         /* Draw node-based NIDOs */
         SUMA_LH("NIDO");
         if (!SUMA_Draw_SO_NIDO (SurfObj, SUMAg_DOv, SUMAg_N_DOv, sv)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed in drawing NIDO objects.\n", FuncName);
         }
         
         /* Draw Selected Node Highlight */
         SUMA_LH("Highlight");
         if (SUMA_SV_GetShowSelectedDatum(sv) && SurfObj->SelectedNode >= 0) {
            if (LocalHead) fprintf(SUMA_STDOUT,"Drawing Node Selection \n");
            id = ND * SurfObj->SelectedNode;
            glMaterialfv(Face, GL_EMISSION, SurfObj->NodeMarker->sphcol); 
                  /*turn on emissidity for sphere */
            glMaterialfv(Face, GL_AMBIENT_AND_DIFFUSE, NoColor);
            glTranslatef ( SurfObj->NodeList[id], 
                           SurfObj->NodeList[id+1],SurfObj->NodeList[id+2]);
            gluSphere(  SurfObj->NodeMarker->sphobj,
                        SurfObj->NodeMarker->sphrad *          
                           (SUMA_sv_auto_fov(sv)/FOV_INITIAL) *  
                           SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06),
                        SurfObj->NodeMarker->slices, 
                        SurfObj->NodeMarker->stacks);
            glTranslatef ( -SurfObj->NodeList[id], 
                           -SurfObj->NodeList[id+1],
                           -SurfObj->NodeList[id+2]);
            glMaterialfv(Face, GL_EMISSION, NoColor); 
                     /*turn off emissidity for axis*/
         }
         
         /* Draw Selected FaceSet Highlight */
         if (SUMA_SV_GetShowSelectedFaceSet(sv) 
                                    && SurfObj->SelectedFaceSet >= 0) {
            if (LocalHead) fprintf(SUMA_STDOUT,"Drawing FaceSet Selection \n");            
            if (!SUMA_DrawFaceSetMarker (SurfObj->FaceSetMarker, sv)) {
               fprintf(SUMA_STDERR,
                  "Error SUMA_DrawMesh: Failed in SUMA_DrawFaceSetMarker\b");
            }
         }
         
         /* Draw node based markers */
         if (SurfObj->NodeObjects && 
             SurfObj->NodeObjects->ObjectType == NIDO_type) {
            if (!SUMA_ApplyDataToNodeObjects(SurfObj, sv)) {
               SUMA_S_Err("Failed to apply data to node objects");
            }
            if (!(SUMA_DrawNIDO ((SUMA_NIDO*)SurfObj->NodeObjects->OP, sv) ) ) {
               SUMA_S_Err("Failed to draw NodeObjects");
            }
            SUMA_LH("Drew Node objects");
         } else {
            SUMA_LH("No Node objects");
         }
         
         /* Draw node based markers2 */
         if (SurfObj->NodeNIDOObjects) {
            if (!SUMA_ApplyDataToNodeNIDOObjects(SurfObj, sv)) {
               SUMA_S_Err("Failed to apply data to node objects2");
            }
            for (i=0; i<SurfObj->N_Node; ++i) {
               if (!(SUMA_DrawNIDO (SurfObj->NodeNIDOObjects[i], sv) ) ) {
                  SUMA_S_Err("Failed to draw NodeObjects");
               }
            }
            SUMA_LH("Drew NodeNIDOObjects");
         } else {
            SUMA_LH("No NodeNIDOObjects");
         }
         break;

   } /* switch DRAW_METHOD */
   
   SUMA_LH("Bring the coords back where they ought to be");
   SUMA_VisX_Pointers4Display(SurfObj, 0);

   SUMA_LH("Undoing state changes");
   SUMA_GLStateTrack("r", &st, FuncName, NULL, NULL); 
   
   
   SUMA_LH("Done");
   SUMA_RETURNe;
} /* SUMA_DrawMesh */

/*! Create a simple tesselated mesh,
   You don't even need to pass sv for this function
   just simple surface drawing, node colors
   must be passed directly if desired */
void SUMA_SimpleDrawMesh(SUMA_SurfaceObject *SurfObj, 
                         GLfloat *colp, SUMA_SurfaceViewer *sv)
{  
   static char FuncName[]={"SUMA_SimpleDrawMesh"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   static GLfloat PermCol[] = {0.0, 0.0, 1.0, 1.0};
   int i, ii, ND, id, ip, NP, PolyMode, sz[2]={0, 0};
   SUMA_DRAWN_ROI *DrawnROI = NULL;
   GLfloat rotationMatrix[4][4];
   GLenum Face=GL_FRONT;
   DList *st=NULL;
   SUMA_TRANS_MODES trmode;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   SUMA_LH("Entered DrawMesh");
   
   if (  SurfObj->PolyMode == SRM_Hide || 
         (sv && sv->PolyMode == SRM_Hide) ||
         SurfObj->TransMode == STM_16 ||
         (sv && sv->TransMode == STM_16)) {
      SUMA_LH("Hiding surface"); 
      SUMA_RETURNe; 
   }

   if (sv && sv->DO_PickMode) {
      SUMA_LH("No need to DrawMesh in DO picking mode");
      SUMA_RETURNe;
   }
   

   #if 0 /* Not sure this will ever be needed for a simple mesh */
   SUMA_LH("Might need to swap coords from the VisX transformed data");
   SUMA_VisX_Pointers4Display(SurfObj, 1);
   #endif
   
   if (!SUMA_GLStateTrack( "new", &st, FuncName, NULL, NULL)) {
      SUMA_S_Err("Failed to create tracking list");
      SUMA_RETURNe; 
   } 
   /* check on rendering mode */
   if (SurfObj->PolyMode != SRM_ViewerDefault) {
     SUMA_LHv("Poly Mode %d\n", SurfObj->PolyMode);
     /* not the default, do the deed */
     #if 0 /* Need to start using MACRO below, but it is not working yet */
     SUMA_SET_GL_RENDER_MODE_TRACK(SurfObj->PolyMode, st); 
     #else
     SUMA_SET_GL_RENDER_MODE(SurfObj->PolyMode);
     #endif
   }

   /* check on rendering mode */
   if (SurfObj->TransMode == STM_ViewerDefault) {
      if (sv) trmode = sv->TransMode;
      else trmode = STM_0;
   } else if (SurfObj->TransMode > STM_0) {
      trmode = SurfObj->TransMode;
   } else trmode = STM_0;
   
   if (trmode != STM_0) {
     /* not the default, do the deed */
     SUMA_LHv("Trans Mode %d\n", trmode);
     SUMA_SET_GL_TRANS_MODE(trmode, st); 
   }


         
   SUMA_LH("Draw Method");
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
            glVertex3fv(&SurfObj->NodeList[id]); 
               /* glVertex3f(0.1, 0.9, 0.0); */

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
         /* This allows each node to follow the color 
            specified when it was drawn */ 
         glColorMaterial(Face, GL_AMBIENT_AND_DIFFUSE); 
         glEnable(GL_COLOR_MATERIAL);
         
         /*Now setup various pointers*/
         glEnableClientState (GL_COLOR_ARRAY);
         glEnableClientState (GL_VERTEX_ARRAY);
         glEnableClientState (GL_NORMAL_ARRAY);
         if (!colp) { /* no color list, try  PermCol */
            if (SurfObj->PermCol) {
               glColorPointer (4, GL_FLOAT, 0, SurfObj->PermCol);
            } else {
               glDisableClientState (GL_COLOR_ARRAY);   
               glColor4f(PermCol[0], PermCol[1], PermCol[2], PermCol[3]);
            }
         } else { 
            glColorPointer (4, GL_FLOAT, 0, colp); 
         }

         glVertexPointer (3, GL_FLOAT, 0, SurfObj->glar_NodeList);
         glNormalPointer (GL_FLOAT, 0, SurfObj->glar_NodeNormList);
         if (LocalHead) 
            fprintf(stdout, "Ready to draw Elements %d\n", SurfObj->N_FaceSet); 
         switch (RENDER_METHOD) {
            case TRIANGLES:
               glDrawElements (  GL_TRIANGLES, (GLsizei)SurfObj->N_FaceSet*3, 
                                 GL_UNSIGNED_INT, SurfObj->glar_FaceSetList);
               break;
            case POINTS:
               glPointSize(4.0); /* keep outside of glBegin */
               /* it is inefficient to draw points using the 
                  glar_FaceSetList because nodes are listed more 
                  than once. You are better off creating an index 
                  vector into glar_NodeList to place all the points, just once*/ 
               glDrawElements (  GL_POINTS, (GLsizei)SurfObj->N_FaceSet*3, 
                                 GL_UNSIGNED_INT, SurfObj->glar_FaceSetList);
               break;
         } /* switch RENDER_METHOD */
         
                 
         /*fprintf(stdout, "Disabling clients\n");*/
         glDisableClientState (GL_COLOR_ARRAY);   
         glDisableClientState (GL_VERTEX_ARRAY);
         glDisableClientState (GL_NORMAL_ARRAY);   
         /*fprintf(stdout, "Out SUMA_DrawMesh, ARRAY mode\n");*/
         
         glDisable(GL_COLOR_MATERIAL);
         
         break;

   } /* switch DRAW_METHOD */
   
   #if 0 /* keep in sync with stuff above */
   SUMA_LH("Bring the coords back where they ought to be");
   SUMA_VisX_Pointers4Display(SurfObj, 0);
   #endif
   
   SUMA_LH("Undoing state changes");
   SUMA_GLStateTrack("r", &st, FuncName, NULL, NULL); 
   
   
   SUMA_LH("Done");
   SUMA_RETURNe;
} /* SUMA_SimpleDrawMesh */


void SUMA_FreeVisXdatum (void *vxd)
{
   static char FuncName[]={"SUMA_FreeVisXdatum"};
   SUMA_VIS_XFORM_DATUM *xd = (SUMA_VIS_XFORM_DATUM*)vxd ;
  
   SUMA_ENTRY;
   
   if (xd) {
      if (xd->dxyz) SUMA_free(xd->dxyz); xd->dxyz = NULL;
      SUMA_free(xd); xd = NULL;
   }
   
   SUMA_RETURNe;
}

SUMA_VIS_XFORM_DATUM *SUMA_NewVisXdatum(char *label)
{
   static char FuncName[]={"SUMA_NewVisXdatum "};
   SUMA_VIS_XFORM_DATUM *xd = NULL;
   
   SUMA_ENTRY;
   
   xd = (SUMA_VIS_XFORM_DATUM *)SUMA_calloc(1, sizeof(SUMA_VIS_XFORM_DATUM)); 
   if (!label) label = "unlabeled";
   strncpy(xd->label, label, 63*sizeof(char)); xd->label[63]='\0';

   /* no longer needed */
      #if 0
      memset(&(xd->Xform), 0, 16*sizeof(double));
      vx.XformType = ID;
      #endif
      
   SUMA_RETURN(xd);
}

int SUMA_EmptyVisXform(SUMA_VIS_XFORM *vx) 
{
   static char FuncName[]={"SUMA_EmptyVisXform"};
   
   SUMA_ENTRY;
   
   if (vx->XformedCoords) {
      SUMA_free(vx->XformedCoords); 
      vx->XformedCoords = NULL;
   }
   vx->Applied = 0;
   if (vx->Xchain) {
      dlist_destroy(vx->Xchain);
      SUMA_free(vx->Xchain); vx->Xchain = NULL;
   }
   vx->Xchain = (DList *)SUMA_calloc(1, sizeof(DList));
   dlist_init(vx->Xchain, SUMA_FreeVisXdatum);
   
   SUMA_RETURN(1);
}

int SUMA_ApplyVisXform(SUMA_SurfaceObject *SO, char *which, 
                       SUMA_VISX_XFORM_DIRECTIONS direction, 
                       int recompute_norm) 
{
   static char FuncName[]={"SUMA_ApplyVisXform"};
   SUMA_VIS_XFORM *vx;
   float *xx=NULL;
   int n, nn, xform_orig=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !which) {
      SUMA_S_Errv("NULL input %p, %s\n", SO, CHECK_NULL_STR(which));
      SUMA_RETURN(0); 
   }
   SUMA_LHv("SO %s, which %s, direction %d, recompute_norm %d\n", 
            SO->Label, CHECK_NULL_STR(which), direction, recompute_norm);
   if (LocalHead) {
      SUMA_DUMP_TRACE(FuncName);
   }
   /* select transform to apply */
   if (!strcmp(which,"VisX0")) {
      xform_orig = 1;
      vx = &(SO->VisX0);
      SUMA_LH("Transforms in VisX0 always apply to orig coordinates");
   } else if (!strcmp(which,"VisX")) {
      xform_orig = 0;
      vx = &(SO->VisX);
      SUMA_LH("Transforms in VisX never apply to orig coordinates\n");
   } else {
      SUMA_S_Errv("Bad string form which %s\n",which);
      SUMA_RETURN(0);
   }
      
   /* check if application is needed */
   if (direction == FORWARD_XFORM) { /* forward xform */
      SUMA_LHv("Going Forward, Applied=%d\n", vx->Applied);
      if(vx->Applied) { /* already done */
         SUMA_LH("Transform already applied, nothing to do");
         SUMA_RETURN(1);
      } 
      
      if (!xform_orig) {
         if (!(vx->XformedCoords)) {
            if (!(vx->XformedCoords = 
                     (float *)SUMA_calloc(SO->N_Node*SO->NodeDim, 
                                          sizeof(float)))) {
               SUMA_S_Crit("Failed to allocate");
               SUMA_RETURN(0);  
            }
         }
         memcpy(vx->XformedCoords, SO->NodeList,
                SO->N_Node*SO->NodeDim*sizeof(float));
         xx = vx->XformedCoords;
      } else xx = SO->NodeList;
      
      SUMA_LHv("Applying VisX Chain xform, surf %s:\n"
                  "Nodelist[0] = %.3f %.3f, %.3f (%p)\n"
                  "xx      [0] = %.3f %.3f, %.3f (%p)\n",
               SO->Label,
               SO->NodeList[0], SO->NodeList[1], SO->NodeList[2], SO->NodeList,
               xx[0], xx[1], xx[2], xx);         
      if (!SUMA_Apply_VisX_Chain(xx, SO->N_Node, vx->Xchain, 0)) {
         SUMA_S_Err("Mille millions de mille sabords!");
         SUMA_RETURN(0); 
      }
      SUMA_LHv("Applied VisX Chain xform, surf %s:\n"
                  "Nodelist[0] = %.3f %.3f, %.3f (%p)\n"
                  "xx      [0] = %.3f %.3f, %.3f (%p)\n",
               SO->Label,
               SO->NodeList[0], SO->NodeList[1], SO->NodeList[2], SO->NodeList,
               xx[0], xx[1], xx[2], xx);         
      vx->Applied = 1;
      
      /* recompute dimensions if any change was done to the original nodelist */
      if (xform_orig) {
         SUMA_SetSODims(SO);
         SUMA_MeshAxisStandard (SO->MeshAxis, (SUMA_ALL_DO *)SO);
      }
      
      if (!xform_orig) { /* switch to xformed coords before normals 
                            are computed */
         SUMA_VisX_Pointers4Display(SO,1);
      }
         SUMA_LHv("Pre : SO->Center = [%f %f %f], Normal[0] = [%f %f %f]\n",
               SO->Center[0], SO->Center[1], SO->Center[2],
               SO->NodeNormList[0], SO->NodeNormList[1], SO->NodeNormList[2]);
         if (recompute_norm) { /* You'd want to do this all the time
                                  but it is too slow for interactive use */
            SUMA_RECOMPUTE_NORMALS(SO); 
         }
         SUMA_LHv("Post: SO->Center = [%f %f %f], Normal[0] = [%f %f %f]\n",
               SO->Center[0], SO->Center[1], SO->Center[2],
               SO->NodeNormList[0], SO->NodeNormList[1], SO->NodeNormList[2]);
      
      if (!xform_orig) {
         SUMA_VisX_Pointers4Display(SO,0);
      } 
      
      /* job is done */
      SUMA_LH("Appied xform");
      SUMA_RETURN(1);
   } else if (direction == UNDO_XFORM) { 
                        /* undo xform, not just apply the reverse  */
      SUMA_LHv("Undoing Applied=%d\n", vx->Applied);
      if (!vx->Applied) { /* already not applied */
         SUMA_RETURN(1);
      }
      if (!vx->XformedCoords) { /* the transform was applied to original coords,
                                  undo it */
         xx = SO->NodeList;
         if (!SUMA_Apply_VisX_Chain(xx, SO->N_Node, vx->Xchain, 1)) {
            SUMA_S_Err("Tonnerre de Brest!");
            SUMA_RETURN(0); 
         }
      } else { /*transfrom results were stored in vx->XformedCoords, just kill */
         SUMA_free(vx->XformedCoords); vx->XformedCoords = NULL;   
      }
      
      /* recompute dimensions if any change was done to the original nodelist */
      if (xform_orig) {
         SUMA_SetSODims(SO);
         SUMA_MeshAxisStandard (SO->MeshAxis, (SUMA_ALL_DO *)SO);
      }
      
      if (!xform_orig) { /* switch to xformed coords before normals 
                            are computed */
         SUMA_VisX_Pointers4Display(SO,1);
      }
      if (recompute_norm) { /* You'd want to do this all the time
                               but it is too slow for interactive use */
         SUMA_RECOMPUTE_NORMALS(SO); 
      }
      if (!xform_orig) {
         SUMA_VisX_Pointers4Display(SO,0);
      } 
      
      /* job is done */
      vx->Applied = 0;
      
      SUMA_RETURN(1);
   }
   SUMA_RETURN(1);  
}

/* Get a pointer to the coordinates that ought to be displayed 
   NEVER free this returned pointer! */
float *SUMA_VisX_CoordPointer(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_VisX_CoordPointer"};
   
   SUMA_ENTRY;
   
   if (!SO) SUMA_RETURN(NULL);
   if (SO->VisX.XformedCoords && !SO->VisX.Applied) {
      SUMA_S_Warn("Weird case 1, should not happen.\n"
                  "Returning orig list to be safe");
      SUMA_RETURN(SO->NodeList);
   } 
   if (SO->VisX0.XformedCoords) {
      SUMA_S_Warn("Weird case 2, should not happen.\n"
                  "VisX0 should not have coord copy.\n"
                  "Returning orig list to be safe");
      SUMA_RETURN(SO->NodeList);
   }
   if (SO->VisX.XformedCoords) SUMA_RETURN(SO->VisX.XformedCoords);
   else SUMA_RETURN(SO->NodeList);
   
   SUMA_RETURN(NULL);
}

/* Swap pointer copies from/to what needs to be displayed */
SUMA_Boolean SUMA_VisX_Pointers4Display(SUMA_SurfaceObject *SO, int fordisp)
{
   static char FuncName[]={"SUMA_VisX_Pointers4Display"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (fordisp) {
      if (SO->NodeList_swp) {
         SUMA_S_Err("You should never have this. Coordinate swapping\n"
                    "should always be undone before returning here\n");
         SUMA_RETURN(NOPE);  
      }
      SUMA_LHv("Applying swap (%d, %p)\n", 
               SO->VisX.Applied, SO->VisX.XformedCoords);
      if (SO->VisX.Applied && SO->VisX.XformedCoords) {
         SO->NodeList_swp = SO->NodeList;
         SO->NodeList = SO->VisX.XformedCoords;
      }
   } else { /* undo swap */
      SUMA_LH("Undoing swap");
      if (SO->NodeList_swp) {
         SO->NodeList = SO->NodeList_swp; SO->NodeList_swp = NULL;
      }
   }
   
   /* and the stupid copies */
   SO->glar_NodeList = SO->NodeList;
   SO->glar_NodeNormList = SO->NodeNormList;
   SO->glar_FaceNormList = SO->FaceNormList;
   
   SUMA_RETURN(YUP);
}

int SUMA_AllowPrying(SUMA_SurfaceViewer *sv, int *RegSO)
{
   static char FuncName[]={"SUMA_AllowPrying"};
   SUMA_SurfaceObject *SO1, *SO2;
   int N_RegSO, LoL, ir=-1, il=-1, ii=-1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SUMAg_CF->ROI_mode) SUMA_RETURN(0);
   
   N_RegSO = SUMA_RegisteredSOs (sv, SUMAg_DOv, RegSO);
   if (N_RegSO != 2) {
      SUMA_LH( "Not set up to pry other than 2 surfaces.\n"
               "Deal with this when the need arises.\n");
      /* MSB, as usual, makes the need arise! */
      for (ii=0, il=-1, ir=-1; ii<N_RegSO && (il <0 || ir < 0); ++ii) {
         SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[ii]].OP;
         if (SO1->Side == SUMA_LEFT && il < 0) il = ii;
         if (SO1->Side == SUMA_RIGHT && ir < 0) ir = ii;
      }
      if (il >= 0 && ir >= 0) {
         SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[il]].OP;
         SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[ir]].OP;
         ii = RegSO[0];
         RegSO[0] = RegSO[il]; RegSO[il] = ii;
         ii = RegSO[1];
         RegSO[1] = RegSO[ir]; RegSO[ir] = ii;
      } else {
         SUMA_RETURN(0);
      }
   } else {
      /* Do Surfaces form left right pair? */
      SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[0]].OP;
      SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[1]].OP;
      if (  (SO1->Side != SUMA_LEFT && SO1->Side != SUMA_RIGHT) ||
            (SO2->Side != SUMA_LEFT && SO2->Side != SUMA_RIGHT) ||
            (SO1->Side == SO2->Side) ) {
         SUMA_LH("Only work with left / right surface pairs")
         SUMA_RETURN(0);     
      }
   }
   
   LoL = SUMA_LeftShownOnLeft(sv, SO1, SO2, 1, 0);
   if (LoL == 0) { /* right surface on left of screen */
      SUMA_RETURN(-2);
   } else if (LoL == 1) { /* right surface on right of screen */
      SUMA_RETURN(2);
   } else if (LoL == -1) { /* Don't know, assume */
      SUMA_LH("Assuming left on left...");
      SUMA_RETURN(2);
   }
   /* Should not get here */
   SUMA_RETURN(2);
}

SUMA_Boolean SUMA_ApplyPrying(SUMA_SurfaceViewer *sv, float val, char *units, 
                              int recompute_norm)
{   
   static char FuncName[]={"SUMA_ApplyPrying"};
   int RegSO[SUMA_MAX_DISPLAYABLE_OBJECTS], N_RegSO, pr;
   SUMA_SurfaceObject *SO1=NULL, *SO2=NULL;

   SUMA_ENTRY;
   
   if (!sv) SUMA_RETURN(NOPE);
   if (units == NULL) units = "mouse";
   
   if ((pr = SUMA_AllowPrying(sv, RegSO))) {/* pry two surfaces */
         if (!sv->GVS[sv->StdView].LHlol) {
            sv->GVS[sv->StdView].LHlol = SUMA_SIGN(pr);
         }
      if (units[0] == 'm') { /* mouse movememt to degrees */
         sv->GVS[sv->StdView].LHpry = sv->GVS[sv->StdView].LHpry0+
                     sv->GVS[sv->StdView].LHlol*
               (90*2.5*val/(float)(sv->WindWidth+1.0)) ; 
      } else { /* assume degrees */
         sv->GVS[sv->StdView].LHpry = val;
      }
      /* just to be safe */
      if (sv->GVS[sv->StdView].LHpry > 90) 
         sv->GVS[sv->StdView].LHpry = 90;
      else if (sv->GVS[sv->StdView].LHpry < -90) 
         sv->GVS[sv->StdView].LHpry = -90;

      SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[0]].OP;
      SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[1]].OP;   
      /* reset applied flag, then reapply transform*/
      if (!SUMA_ApplyVisXform(SO1, "VisX", UNDO_XFORM, recompute_norm)) {
         SUMA_S_Err("Failed to apply SUMA_ApplyVisXform");
         SUMA_RETURN(0);
      } 
      if (!SUMA_ApplyVisXform(SO2, "VisX", UNDO_XFORM, recompute_norm)) {
         SUMA_S_Err("Failed to apply SUMA_ApplyVisXform");
         SUMA_RETURN(0);
      }
      if (!SUMA_ComputeVisX(SO1, SO2, sv, "VisX", recompute_norm)) {
         SUMA_S_Err("Failed to compute or apply prying xform");
      }
      
      SUMA_UpdateRotaCenter (sv, SUMAg_DOv, SUMAg_N_DOv);
      SUMA_UpdateViewPoint(sv, SUMAg_DOv, SUMAg_N_DOv, 0);
      SUMA_SetGLHome(sv);
   }
   SUMA_RETURN(YUP);
}
         
SUMA_Boolean SUMA_RecomputeNormsPrying(SUMA_SurfaceViewer *svu) 
{
   static char FuncName[]={"SUMA_RecomputeNormsPrying"};
   int RegSO[SUMA_MAX_DISPLAYABLE_OBJECTS], N_RegSO, n_sv, i, j;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_SurfaceViewer *sv=NULL;

   SUMA_ENTRY;
   
   if (!svu) n_sv = SUMAg_N_SVv;
   else n_sv = 1;
   for (i=0; i<n_sv; ++i) {
      if (!svu) {
         sv = &(SUMAg_SVv[i]);
      } else {
         sv = svu;
      }

      if (sv->GVS[sv->StdView].LHpry || 
          sv->GVS[sv->StdView].LHpry0) {/* just refresh normals */
         N_RegSO = SUMA_RegisteredSOs (sv, SUMAg_DOv, RegSO);
         for (j=0; j<N_RegSO; ++j) {
            SO = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[j]].OP;
            SUMA_VisX_Pointers4Display(SO,1);
            SUMA_RECOMPUTE_NORMALS(SO); /* a little slow, but not much
                                           to do if we're not keeping
                                           copy of orignal normals */
            SUMA_VisX_Pointers4Display(SO,0);
         }
      }
   }
   SUMA_RETURN(YUP);
}      

SUMA_Boolean SUMA_ResetPrying(SUMA_SurfaceViewer *svu)
{  
   static char FuncName[]={"SUMA_ResetPrying"};
   int RegSO[SUMA_MAX_DISPLAYABLE_OBJECTS], N_RegSO, n_sv, i;
   SUMA_SurfaceObject *SO1=NULL, *SO2=NULL;
   SUMA_SurfaceViewer *sv=NULL;

   SUMA_ENTRY;
   
   if (!svu) n_sv = SUMAg_N_SVv;
   else n_sv = 1;
   for (i=0; i<n_sv; ++i) {
      if (!svu) {
         sv = &(SUMAg_SVv[i]);
      } else {
         sv = svu;
      }

      if (sv->GVS[sv->StdView].LHpry || 
          sv->GVS[sv->StdView].LHpry0) {/* Cancel prying */
         N_RegSO = SUMA_RegisteredSOs (sv, SUMAg_DOv, RegSO);
         sv->GVS[sv->StdView].LHpry = 0.0;
         sv->GVS[sv->StdView].LHpry0 = 0.0;
         sv->GVS[sv->StdView].LHlol = 0;   
         SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[0]].OP;
         SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[RegSO[1]].OP;   
         /* reset applied flag, then reapply transform*/
         if (!SUMA_ApplyVisXform(SO1, "VisX", UNDO_XFORM, 1)) {
            SUMA_S_Err("Failed to apply SUMA_ApplyVisXform");
            SUMA_RETURN(0);
         } 
         if (!SUMA_ApplyVisXform(SO2, "VisX", UNDO_XFORM, 1)) {
            SUMA_S_Err("Failed to apply SUMA_ApplyVisXform");
            SUMA_RETURN(0);
         }
         if (!SUMA_ComputeVisX(SO1, SO2, sv, "VisX", 1)) {
            SUMA_S_Err("Failed to compute or apply prying xform");
         }
         
         SUMA_UpdateRotaCenter (sv, SUMAg_DOv, SUMAg_N_DOv);
         SUMA_UpdateViewPoint(sv, SUMAg_DOv, SUMAg_N_DOv, 0);
         SUMA_SetGLHome(sv);
         SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
      }
   }
   SUMA_RETURN(YUP);
}

/*! Determine when given one left and one right surface, whether or not
    the left hemisphere is shown on the left side of the viewer.
    
    If useParents then replace the surfaces by their local domain parents
    If applyViewingXform then apply the openGL viewing transformations 
                         before returning the answer.
                         
   0  --> Left on Right
   1  --> Left on Left
   -1 --> Can't tell / error
*/
int SUMA_LeftShownOnLeft(SUMA_SurfaceViewer *sv, 
                         SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2,
                         int useParents, int applyViewingXform) 
{
   static char FuncName[]={"SUMA_LeftShownOnLeft"};
   SUMA_SurfaceObject *SO=NULL;
   double x[6], s[6];
   int q[2];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv || !SO1 || !SO2) SUMA_RETURN(-1);
   
   if (useParents) {
      SO1 = SUMA_findSOp_inDOv( SO1->LocalDomainParentID, 
                                    SUMAg_DOv, SUMAg_N_DOv);
      SO2 = SUMA_findSOp_inDOv( SO2->LocalDomainParentID, 
                                    SUMAg_DOv, SUMAg_N_DOv);
      if (!SO1 || !SO2) SUMA_RETURN(-1);
   }
   
   if (  (SO1->Side != SUMA_LEFT && SO1->Side != SUMA_RIGHT) ||
         (SO2->Side != SUMA_LEFT && SO2->Side != SUMA_RIGHT) ||
         (SO1->Side == SO2->Side) ) {
      SUMA_LH("Only work with left / right surface pairs")
      SUMA_RETURN(-1);     
   }
   
   /* How about the arrangement of the surfaces? */
   if (SO2->Side == SUMA_LEFT) { SO = SO2; SO2 = SO1; SO1 = SO; SO=NULL; }
   x[0] = SO1->Center[0]; x[1] = SO1->Center[1]; x[2] = SO1->Center[2]; 
   x[3] = SO2->Center[0]; x[4] = SO2->Center[1]; x[5] = SO2->Center[2]; 
   SUMA_World2ScreenCoords (sv, 2, x, s, q, applyViewingXform, YUP);
   SUMA_LHv("Left  surface world  center: %f %f %f\n"
            "             screen center: %f %f %f\n"
            "Right surface world  center: %f %f %f\n"
            "             screen center: %f %f %f\n",
            x[0], x[1], x[2], s[0], s[1], s[2],
            x[3], x[4], x[5], s[3], s[4], s[5]);
   if (s[3] < s[0]) { /* right surface on left of screen */
      SUMA_RETURN(0);
   } else { /* left on left */
      SUMA_RETURN(1);
   }
   
   SUMA_RETURN(-1);
}

/*!
   Transform the parameters in the Axis structure by Xform
   Only  Center and BR are dealt with and the results stored
   in MAx if it is not NULL, otherwise MA's fields are modified
*/ 
SUMA_Boolean SUMA_XformAxis(SUMA_Axis *MA, double Xform[4][4], int inv, 
                            SUMA_Axis *MAx) 
{
   static char FuncName[]={"SUMA_XformAxis"};
   float pnts[32][3];
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!MA ) SUMA_RETURN(NOPE);

   /* Create the corners, center, and transform them all */
   SUMA_LHv("Center at input: %f %f %f\n"
            "BR X: %f %f\n"
            "   Y: %f %f\n"
            "   Z: %f %f\n", 
         MA->Center[0], MA->Center[1],MA->Center[2],
         MA->BR[0][0], MA->BR[0][1],
         MA->BR[1][0], MA->BR[1][1],
         MA->BR[2][0], MA->BR[2][1]);
   SUMA_BOX_CORNER_POINTS_FROM_AXIS(MA, pnts);
   SUMA_COPY_VEC(MA->Center, pnts[8], 3, float, float);
   
   /* transform the lot of them */
   SUMA_Apply_Coord_xform((float *)pnts,9,3, Xform, inv,NULL);
   
   /* put results in output */
   if (!MAx) MAx = MA;
   SUMA_COPY_VEC(pnts[8], MAx->Center, 3, float, float);
   
   for (i=0; i<3; ++i) {
      MAx->BR[i][0] = MAx->BR[i][1] = pnts[0][i];
   }
   for (i=1; i<8; ++i) {
            if (pnts[i][0] < MAx->BR[0][0]) MAx->BR[0][0]=pnts[i][0];
      else  if (pnts[i][0] > MAx->BR[0][1]) MAx->BR[0][1]=pnts[i][0];
      else  if (pnts[i][1] < MAx->BR[1][0]) MAx->BR[1][0]=pnts[i][1];
      else  if (pnts[i][1] > MAx->BR[1][1]) MAx->BR[1][1]=pnts[i][1];
      else  if (pnts[i][2] < MAx->BR[2][0]) MAx->BR[2][0]=pnts[i][2];
      else  if (pnts[i][2] > MAx->BR[2][1]) MAx->BR[2][1]=pnts[i][2];
   }
   
   SUMA_LHv("Center at output: %f %f %f\n"
            "BR X: %f %f\n"
            "   Y: %f %f\n"
            "   Z: %f %f\n", 
         MAx->Center[0], MAx->Center[1],MAx->Center[2],
         MAx->BR[0][0], MAx->BR[0][1],
         MAx->BR[1][0], MAx->BR[1][1],
         MAx->BR[2][0], MAx->BR[2][1]);

   SUMA_RETURN(YUP);
   
}

/* Compute the transform needed to properly position two 
   surfaces in the display */
int SUMA_ComputeVisX(SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2, 
                     SUMA_SurfaceViewer *sv, char *which, int recompute_norm) 
{
   static char FuncName[]={"SUMA_ComputeVisX"};
   SUMA_SurfaceObject *SOv[2];
   SUMA_VIS_XFORM_DATUM *x0=NULL, *x1=NULL;
   float dx=0.0, dx2;
   int splitx = 0;
   int i, state;
   double A[3], B[3], C[3], D[3], AC[3], AD[3], BC[3], BD[3];
   double dot1, dot2, doti;
   double Pt[3], Pb[3], C1[3], rotax[3], rotmag, rot=25;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if ( (SO1->Side != SUMA_LEFT && SO1->Side != SUMA_RIGHT) ||
        (SO2->Side != SUMA_LEFT && SO2->Side != SUMA_RIGHT) ||
        (SO1->Side == SO2->Side) ) {
      SUMA_LH("Only work with left and right surface pairs")
      SUMA_RETURN(1);     
   }
   
   if (SO1->NodeList_swp || SO2->NodeList_swp) {
      SUMA_S_Err("Cannot operate here while NodeList is swapped");
      SUMA_RETURN(0);
   }
   
   if (!sv->GVS[sv->StdView].LHlol) {
      if (SUMA_LeftShownOnLeft(sv, SO1, SO2, 1, 0)) {
         sv->GVS[sv->StdView].LHlol = 1;
      } else {
         sv->GVS[sv->StdView].LHlol = -1;
      }
      
   }
   if (SO1->Side == SUMA_LEFT) { SOv[0] = SO1; SOv[1] = SO2; }
   else { SOv[0] = SO2; SOv[1] = SO1;}
   
   splitx = 0;
      
   if (!strcmp(which,"VisX0")) {
      
   /* Making sure surfaces don't ride on each other 
      Assume we're only working on the RL, X axis   */
      if ((SO1->AnatCorrect || SO2->AnatCorrect)) {
         SUMA_LHv("Out of principle,  won't transform VisX0\n"
                  " for anatomically correct surfaces (%d %d)\n"
                  " Nothing to be done here\n",
                     SO1->AnatCorrect, SO2->AnatCorrect);
         SUMA_RETURN(1);
      }
      
      if (!(x0 = SUMA_Fetch_VisX_Datum ("Arrange_At_Load", SOv[0]->VisX0.Xchain, 
                                        ADD_AFTER, NULL))) {
         SUMA_S_Err("Should not have failed to fetch VisX0 with addition!");
         SUMA_RETURN(0);
      }
      if (!(x1 = SUMA_Fetch_VisX_Datum ("Arrange_At_Load", SOv[1]->VisX0.Xchain, 
                                        ADD_AFTER, NULL))) {
         SUMA_S_Err("Should not have failed to fetch VisX1 with addition!");
         SUMA_RETURN(0);
      }
      /* How much do the surfaces overlap along X? 
         SOv[0] is the left side surface , see NIH-6 pp 223*/
      dx = SOv[1]->MeshAxis->BR[0][0] - SOv[0]->MeshAxis->BR[0][1];
                                                /* xminRIGHT - xmaxLEFT */
      dx2 = SOv[1]->MeshAxis->BR[0][1] - SOv[0]->MeshAxis->BR[0][0];
                                                /* xmaxRIGHT - xminLEFT */
      /* Rightmost of Right side surface, RAI assumed for coords */
      A[0] = SOv[1]->MeshAxis->BR[0][0]; A[1]=0.0; A[2]=0.0; /* ignore Y & Z */
      /* Leftmost of Right side */
      B[0] = SOv[1]->MeshAxis->BR[0][1]; B[1]=0.0; B[2]=0.0; 
      
      /* same for the Left side surface */
      C[0] = SOv[0]->MeshAxis->BR[0][0]; C[1]=0.0; C[2]=0.0; 
      D[0] = SOv[0]->MeshAxis->BR[0][1]; D[1]=0.0; D[2]=0.0; 

      for (i=0; i<3; ++i) {
         AC[i] = C[i]-A[i];
         BC[i] = C[i]-B[i];
         AD[i] = D[i]-A[i];
         BD[i] = D[i]-B[i];
      }
      dot1 = SUMA_MT_DOT(AC, BC);
      dot2 = SUMA_MT_DOT(AD, BD);
      doti = BC[0];
      
      state = -1;
      if (dot1 >= 0.0 && dot2 >= 0.0) {
         if (doti >= 0) {
            state = 0;
         } else {
            state = 4;
         }
      } else if (dot1 >= 0.0 && dot2 < 0.0) {
         state = 3;
      } else if (dot1 < 0.0 && dot2 >= 0.0) {
         state = 1;
      } else if (dot1 < 0.0 && dot2 < 0.0) {
         state = 2;
      }
      if (state < 0) {
         SUMA_S_Warnv("Case not accounted for: dot1 = %f dot2 = %f doti = %f\n"
                      "No separation attempted\n",
               dot1, dot2, doti);
      }
      SUMA_LHv("Have Left surface %s: %.3f %.3f\n"
               "    right surface %s  %.3f %.3f\n"
               ", dx = %f, dx2=%f\n"
               "dot1 = %.3f dot2 = %.3f doti = %.3f, state = %d\n",
               SOv[0]->Label, 
                  SOv[0]->MeshAxis->BR[0][0], SOv[0]->MeshAxis->BR[0][1],
               SOv[1]->Label, 
                  SOv[1]->MeshAxis->BR[0][0], SOv[1]->MeshAxis->BR[0][1],
                  dx, dx2,
               dot1, dot2, doti, state);
               
      /* split the difference, if need be 
         If this fails at time, the separation amount
         should be rewritten in terms of the vectors defined above
         and for each of the states separately. For now we leave
         well enough alone.*/
      if (state == 1 || state == 2 || state == 3 || state == 4) { 
                                                      /* need separation */
         /* Augment the distance by 10% of the surface sizes 
            Don't bother for flat maps */
         if (SOv[0]->EmbedDim == 3 && SOv[1]->EmbedDim == 3) {
            SUMA_LH("Vanilla");
            dx = dx - 0.1/2.0*
                   ( (SOv[0]->MeshAxis->BR[0][1] - SOv[0]->MeshAxis->BR[0][0]) +
                     (SOv[1]->MeshAxis->BR[0][1] - SOv[1]->MeshAxis->BR[0][0]) );
            if (sv->GVS[sv->StdView].LHlol == -1) dx = -dx;
            /* move left surface to the left and right surface to the right */
            x0->Xform[0][3] = dx/2.0;
            x1->Xform[0][3] = -dx/2.0;
            x0->XformType = SHIFT; /* translation */
            x1->XformType = SHIFT; /* translation */
         } else if (SOv[0]->EmbedDim == 2 && SOv[1]->EmbedDim == 2) {
            /* you'll want to rotate one flat map about its Z axis
               You should really compute the needed x axis separation
               after the coords are rotated or risk overlap after rotation.
               Will see if that is needed in practice. Also, this may not
               be appropriate for flat maps not made by FreeSurfer, I can
               easily put a switch for that */
            SUMA_LH("Special flat");
            if (sv->GVS[sv->StdView].LHlol == -1) dx = -dx;
            rotax[0] = rotax[1] = 0.0; rotax[2] = 1.0;
            C[0] = SOv[1]->Center[0]; 
            C[1] = SOv[1]->Center[1]; 
            C[2] = SOv[1]->Center[2]; 
            if (!SUMA_BuildRotationMatrix(C, rotax, SUMA_PI, 
                                          x1->Xform)) {
               SUMA_S_Err("Failed to build rotation for surface 1");
               SUMA_RETURN(0);
            }
            x0->Xform[0][3] = dx/2.0;
            x1->Xform[0][3] = -dx/2.0;
            x0->XformType = SHIFT; /* translation */
            x1->XformType = AFFINE; /* affine */
         } else { /* would you ever get here? */
            SUMA_LH("Did not expect this");
            if (sv->GVS[sv->StdView].LHlol == -1) dx = -dx;
            /* move left surface to the left and right surface to the right */
            x0->Xform[0][3] = dx/2.0;
            x1->Xform[0][3] = -dx/2.0;
            x0->XformType = SHIFT; /* translation */
            x1->XformType = SHIFT; /* translation */
         }
         if (!SUMA_ApplyVisXform(SOv[0], "VisX0", 
                                 FORWARD_XFORM, recompute_norm)) {
               SUMA_S_Err("Failed to apply SUMA_ApplyVisXform");
               SUMA_RETURN(0);
         } 
         if (!SUMA_ApplyVisXform(SOv[1], "VisX0", 
                                 FORWARD_XFORM, recompute_norm)) {
            SUMA_S_Err("Failed to apply SUMA_ApplyVisXform");
            SUMA_RETURN(0);
         }
      }
   } else if (!strcmp(which,"VisX")){ 
      /* if Prying is to be added, it should be after CoordBias */
      x0 = SUMA_Fetch_VisX_Datum ("Prying", SOv[0]->VisX.Xchain, 
                                  ADD_AFTER,"CoordBias");
      x1 = SUMA_Fetch_VisX_Datum ("Prying", SOv[1]->VisX.Xchain, 
                                  ADD_AFTER, "CoordBias");

      /* open up the gap */
      rot = sv->GVS[sv->StdView].LHpry;
      if (!(SUMA_IS_GEOM_SYMM(SOv[0]->isSphere) && 
            SUMA_IS_GEOM_SYMM(SOv[1]->isSphere))   &&
          !(SOv[0]->EmbedDim == 2 && SOv[1]->EmbedDim == 2) ) {
         SUMA_LH("Regular deal");
         if (rot*sv->GVS[sv->StdView].LHlol >= 0) {/*rot. about posterior axes */
            /* Compute average medial Y axis (Box segment 12 from left hemi +
                                                  segment 11 from right hemi )
               (see SUMA_SortedAxisSegmentList() and labbook NIH-IV, pp 21)*/
            Pb[0] = (SOv[0]->MeshAxis->BR[0][1] +    /* pt B, left hemi (xmax)*/
                     SOv[1]->MeshAxis->BR[0][0])/2.0;/* pt A, right hemi (xmin)*/
            Pb[1] = (SOv[0]->MeshAxis->BR[1][0] +    /* pt B, left hemi (ymin)*/
                     SOv[1]->MeshAxis->BR[1][0])/2.0;/* pt A, right hemi (ymin)*/
            Pb[2] = (SOv[0]->MeshAxis->BR[2][0] +    /* pt B, left hemi (zmin)*/
                     SOv[1]->MeshAxis->BR[2][0])/2.0;/* pt A, right hemi (zmin)*/

            Pt[0] = (SOv[0]->MeshAxis->BR[0][1] +    /* pt F, left hemi (xmax)*/
                     SOv[1]->MeshAxis->BR[0][0])/2.0;/* pt E, right hemi (xmin)*/
            Pt[1] = (SOv[0]->MeshAxis->BR[1][0] +    /* pt F, left hemi (ymin)*/
                     SOv[1]->MeshAxis->BR[1][0])/2.0;/* pt E, right hemi (ymin)*/
            Pt[2] = (SOv[0]->MeshAxis->BR[2][1] +    /* pt F, left hemi (zmax)*/
                     SOv[1]->MeshAxis->BR[2][1])/2.0;/* pt E, right hemi (zmax)*/
         } else {
            /* Compute average medial Y axis (Box segment 10 from left hemi +
                                                  segment 9 from right hemi )
               (see  SUMA_SortedAxisSegmentList() and labbook NIH-IV, pp 21)*/
            Pb[0] = (SOv[0]->MeshAxis->BR[0][1] +    /* pt D, left hemi (xmax)*/
                     SOv[1]->MeshAxis->BR[0][0])/2.0;/* pt C, right hemi (xmin)*/
            Pb[1] = (SOv[0]->MeshAxis->BR[1][1] +    /* pt D, left hemi (ymax)*/
                     SOv[1]->MeshAxis->BR[1][1])/2.0;/* pt C, right hemi (ymax)*/
            Pb[2] = (SOv[0]->MeshAxis->BR[2][0] +    /* pt D, left hemi (zmin)*/
                     SOv[1]->MeshAxis->BR[2][0])/2.0;/* pt C, right hemi (zmin)*/

            Pt[0] = (SOv[0]->MeshAxis->BR[0][1] +    /* pt H, left hemi (xmax)*/
                     SOv[1]->MeshAxis->BR[0][0])/2.0;/* pt G, right hemi (xmin)*/
            Pt[1] = (SOv[0]->MeshAxis->BR[1][1] +    /* pt H, left hemi (ymax)*/
                     SOv[1]->MeshAxis->BR[1][1])/2.0;/* pt G, right hemi (ymax)*/
            Pt[2] = (SOv[0]->MeshAxis->BR[2][1] +    /* pt H, left hemi (zmax)*/
                     SOv[1]->MeshAxis->BR[2][1])/2.0;/* pt G, right hemi (zmax)*/
         } 
         SUMA_UNIT_VEC(Pb, Pt, rotax, rotmag);
         C[0] = C1[0] = (Pt[0] + Pb[0])/2.0;
         C[1] = C1[1] = (Pt[1] + Pb[1])/2.0;
         C[2] = C1[2] = (Pt[2] + Pb[2])/2.0;
      } else if ( SUMA_IS_GEOM_SYMM(SOv[0]->isSphere) && 
                  SUMA_IS_GEOM_SYMM(SOv[1]->isSphere)) { 
                  /* for spheres, best rotate about the screen's Y axis 
                  and spheres's own centers*/         
         C[0]  = SOv[0]->Center[0];  C[1]  = SOv[0]->Center[1]; 
                                     C[2]  = SOv[0]->Center[2]; 
         C1[0] = SOv[1]->Center[0];  C1[1] = SOv[1]->Center[1]; 
                                     C1[2] = SOv[1]->Center[2]; 
         SUMA_LH("Spheres!");
               /* transform screen Y axis to world axis */
         SUMA_NormScreenToWorld(sv, 0, 0, Pb, NULL, 1);   
         SUMA_NormScreenToWorld(sv, 0, 1, Pt, NULL, 1);   
         SUMA_UNIT_VEC(Pb, Pt, rotax, rotmag);
         rot = rot*2.0; /* go to +/- 180 */
      } else if ( SOv[0]->EmbedDim == 2 && SOv[1]->EmbedDim == 2) { 
                                                      /* for flatties */
         SUMA_LH("Flats!");
         C[0]  = SOv[0]->Center[0];  C[1]  = SOv[0]->Center[1]; 
                                     C[2]  = SOv[0]->Center[2]; 
         C1[0] = SOv[1]->Center[0];  C1[1] = SOv[1]->Center[1]; 
                                     C1[2] = SOv[1]->Center[2]; 
         rotax[0] = rotax[1] = 0.0; rotax[2] = 1.0;
         rot = rot*2.0; /* go to +/- 180 */
         
         splitx = 1;/* need to split them apart too */
      }
         SUMA_LHv("Rotation of %f degrees about [%f %f %f] on "
                  "[%f %f %f] and [%f %f %f], splitx = %d\n",
                  rot, rotax[0], rotax[1], rotax[2],
                  C[0], C[1], C[2], C1[0], C1[1], C1[2], splitx);
         if (!SUMA_BuildRotationMatrix(C, rotax, rot*SUMA_PI/180.0, 
                                       x0->Xform)) {
            SUMA_S_Err("Failed to build rotation for surface 0");
            SUMA_RETURN(0);
         } 
         if (!SUMA_BuildRotationMatrix(C1, rotax, -rot*SUMA_PI/180.0, 
                                       x1->Xform)) {
            SUMA_S_Err("Failed to build rotation for surface 0");
            SUMA_RETURN(0);
         }
         
         if (splitx) {
            SUMA_Axis mxfrm0, mxfrm1;
            float dxi = SOv[1]->MeshAxis->BR[0][0] - SOv[0]->MeshAxis->BR[0][1];;
            /* compute the new x axis overlap */
            
            SUMA_XformAxis(SOv[0]->MeshAxis, x0->Xform, 0, &mxfrm0);
            SUMA_XformAxis(SOv[1]->MeshAxis, x1->Xform, 0, &mxfrm1);
            dx  = mxfrm1.BR[0][0] - mxfrm0.BR[0][1];
            dxi = mxfrm0.BR[0][0] - mxfrm1.BR[0][1]; 
            
            if (SUMA_ABS(dx) > SUMA_ABS(dxi)) dx = dxi; /* inverted layout */                                    
            if (dx < 0) {
               if (sv->GVS[sv->StdView].LHlol == -1) dx = -dx;
               x0->Xform[0][3] += dx/2.0;
               x1->Xform[0][3] += -dx/2.0;
            }
         }
         x0->XformType = AFFINE; /* affine */
         x1->XformType = AFFINE; /* affine */
         if (!SUMA_ApplyVisXform(SOv[0], "VisX", 
                                 FORWARD_XFORM, recompute_norm)) {
               SUMA_S_Err("Failed to apply SUMA_ApplyVisXform");
               SUMA_RETURN(0);
         } 
         if (!SUMA_ApplyVisXform(SOv[1], "VisX", 
                                 FORWARD_XFORM, recompute_norm)) {
            SUMA_S_Err("Failed to apply SUMA_ApplyVisXform");
            SUMA_RETURN(0);
         }
   } else {
      SUMA_S_Errv("Bad Witch! %s\n", which);
      SUMA_RETURN(0);
   }            
   SUMA_RETURN(1);
}

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
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_SL_Warn("NULL SO");
      SUMA_RETURN(YUP);
   }
   if (LocalHead) {
      if (SO->Label) 
         fprintf (SUMA_STDERR, "%s: freeing SO %s\n", FuncName, SO->Label);
      else fprintf (SUMA_STDERR, "%s: freeing SO\n", FuncName);
   }
   /* Start with the big ones and down*/
   /* From SUMA 1.2 and on, some glar_ pointers are copies 
      of others and should not be freed */ 
   SO->glar_FaceSetList = NULL;
   SO->glar_NodeList = NULL;
   SO->glar_NodeNormList = NULL;
   SO->glar_FaceNormList = NULL;
   SO->NodeList_swp = NULL;
   
   if (LocalHead) fprintf (stdout, "SO->NodeList... ");
   if (SO->NodeList)   SUMA_free(SO->NodeList);
   /*fprintf (stdout, "SO->FaceSetList... ");*/ 
   if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
   /*fprintf (stdout, "SO->FaceSetList... ");*/ 
   if (SO->NodeNormList) SUMA_free(SO->NodeNormList);
   /*fprintf (stdout, "SO->NodeNormList... ");*/ 
   if (SO->FaceNormList) SUMA_free(SO->FaceNormList);
   /*fprintf (stdout, "SO->FaceNormList... ");*/ 
   if (SO->Name_NodeParent) SUMA_free(SO->Name_NodeParent);
   if (LocalHead) fprintf (stdout, "SO->Name.FileName... "); 
   if (SO->Name.FileName) SUMA_free(SO->Name.FileName);
   
   if (LocalHead) 
      fprintf (SUMA_STDERR, "%s: freeing SO->Name.Path\n", FuncName);
   if (SO->Name.Path) SUMA_free(SO->Name.Path);
   if (SO->SpecFile.Path) SUMA_free(SO->SpecFile.Path);
   if (SO->SpecFile.FileName) SUMA_free(SO->SpecFile.FileName);
   if (SO->MeshAxis) SUMA_Free_Axis (SO->MeshAxis);
   if (LocalHead) 
      fprintf (SUMA_STDERR, "%s: freeing SO->NodeMarker\n", FuncName);
   if (SO->NodeMarker) SUMA_Free_SphereMarker (SO->NodeMarker);
   if (LocalHead) 
      fprintf (SUMA_STDERR, "%s: freeing SO->FaceSetMarker\n", FuncName);
   if (SO->FaceSetMarker) SUMA_Free_FaceSetMarker(SO->FaceSetMarker);
   if (LocalHead) 
      fprintf (SUMA_STDERR, "%s: freeing SO->idcode_str\n", FuncName);
   if (SO->idcode_str) SUMA_free(SO->idcode_str); 
   if (SO->facesetlist_idcode_str) SUMA_free(SO->facesetlist_idcode_str);
   if (SO->nodelist_idcode_str) SUMA_free(SO->nodelist_idcode_str);
   if (SO->facenormals_idcode_str) SUMA_free(SO->facenormals_idcode_str);
   if (SO->nodenormals_idcode_str) SUMA_free(SO->nodenormals_idcode_str);
   if (SO->polyarea_idcode_str) SUMA_free(SO->polyarea_idcode_str);
   if (LocalHead) 
      fprintf (SUMA_STDERR, "%s: freeing SO->LocalDomainParentID\n", FuncName);
   if (SO->LocalDomainParentID) SUMA_free(SO->LocalDomainParentID);
   if (SO->LocalDomainParent) SUMA_free(SO->LocalDomainParent);
   if (SO->LocalCurvatureParentID) SUMA_free(SO->LocalCurvatureParentID);
   if (SO->LocalCurvatureParent) SUMA_free(SO->LocalCurvatureParent);
   if (SO->OriginatorID) SUMA_free(SO->OriginatorID);
   if (SO->DomainGrandParentID) SUMA_free(SO->DomainGrandParentID);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->Group\n", FuncName);
   if (SO->Group) SUMA_free(SO->Group);
   if (SO->State) SUMA_free(SO->State);
   if (SO->PolyArea) SUMA_free(SO->PolyArea);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->SC\n", FuncName);
   if (SO->SC) {
      SUMA_Free_SURFACE_CURVATURE(SO->SC);
   }
   if (SO->Group_idcode_str) SUMA_free(SO->Group_idcode_str);
   if (SO->OriginatorLabel) SUMA_free(SO->OriginatorLabel);
   if (SO->parent_vol_idcode_str) SUMA_free(SO->parent_vol_idcode_str);

   
   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: freeing %d overlays\n", FuncName, SO->N_Overlays);
   
   /* freeing overlays */
   if (SO->N_Overlays) {
      /* freeing color overlays */
      for (i=0; i < SO->N_Overlays; ++i) {
         SUMA_FreeOverlayPointer (SO->Overlays[i]);
         SO->Overlays[i] = NULL;
      }
      SO->N_Overlays = 0;
   }
   /*Now free the vector of pointers */
   SUMA_free(SO->Overlays);

   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing FN\n", FuncName);
   /* freeing FN,  make sure that there are no links to FN*/
   if (SO->FN) {
      if (!SUMA_Free_FirstNeighb (SO->FN)) {
         fprintf(SUMA_STDERR,
                  "Error SUMA_Free_Surface_Object : Failed to free SO->FN");
      }
      SO->FN = NULL;
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing Label\n", FuncName);
   /* freeing Label */
   if (SO->Label) SUMA_free(SO->Label);
   
   /* freeing EL,  make sure that there are no links to EL*/
   if (SO->EL) {
      SUMA_free_Edge_List (SO->EL);
   }
   SO->EL = NULL;
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing MF\n", FuncName);
   if (SO->MF){ 
      SUMA_Free_MemberFaceSets (SO->MF);
      SO->MF = NULL;
   }
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SurfCont\n", FuncName);
   if (SO->SurfCont) SUMA_FreeSurfContStruct(SO->SurfCont);
   
   if (SO->PermCol) SUMA_free(SO->PermCol);
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing VolPar\n", FuncName);
   if (SO->VolPar) SUMA_Free_VolPar(SO->VolPar); 
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing aSO\n", FuncName);
   if (SO->aSO) SO->aSO = SUMA_FreeAfniSurfaceObject(SO->aSO);
   
   
   if (LocalHead) 
      fprintf (SUMA_STDERR, "%s: freeing CommonNodeObject\n", FuncName);
   if (SO->CommonNodeObject) 
      SUMA_Free_Displayable_Object_Vect(SO->CommonNodeObject,1);
      SO->CommonNodeObject = NULL;
   if (SO->NodeObjects) 
      SUMA_Free_Displayable_Object_Vect (SO->NodeObjects, 1);
      SO->NodeObjects = NULL;

   if (SO->NodeNIDOObjects) { 
      for (i=0; i<SO->N_Node; ++i) {
         if (SO->NodeNIDOObjects[i]) SUMA_free_NIDO(SO->NodeNIDOObjects[i]);
      }
      SUMA_free(SO->NodeNIDOObjects);
   }
   
   if (SO->NodeAreas) SUMA_free(SO->NodeAreas); 
   
   SUMA_EmptyVisXform(&(SO->VisX0));
   SUMA_EmptyVisXform(&(SO->VisX));
   
   if (SO) SUMA_free(SO);
   
   if (LocalHead) fprintf (stdout, "Done\n");
   SUMA_RETURN (YUP);
}   

static char *SUMA_GeomTypeName(SUMA_GEOM_TYPE tp) {
   switch(tp) {
      case SUMA_GEOM_NOT_SET:
         return("Not Set");
      case SUMA_GEOM_SPHERE:
         return("Sphere");
      case SUMA_GEOM_ICOSAHEDRON:
         return("Icosahedron");
      case SUMA_GEOM_IRREGULAR:
         return("Irregular");
      case SUMA_N_GEOM:
         return("Number of geometries");
      default:
         return("Should not have this");
   }
   return("What the hell?");
}

char *SUMA_SideName(SUMA_SO_SIDE ss) {
   switch (ss) {
      case SUMA_SIDE_ERROR:
         return("side_error");
      case SUMA_NO_SIDE:
         return("no_side");
      case SUMA_LR:
         return("LR");
      case SUMA_LEFT:
         return("L");
      case SUMA_RIGHT:
         return("R");
      default:
         return("BadNewsCandidate");
   }
}

SUMA_SO_SIDE SUMA_SideType(char *ss) {
   if (!ss) return(SUMA_NO_SIDE);
   if (!strcmp(ss,"no_side")) return(SUMA_NO_SIDE);
   if (!strcmp(ss,"side_error")) return(SUMA_SIDE_ERROR);
   if (!strcmp(ss,"LR")) return(SUMA_LR);
   if (!strcmp(ss,"R")) return(SUMA_RIGHT);
   if (!strcmp(ss,"L")) return(SUMA_LEFT);
   return(SUMA_SIDE_ERROR);
}

char *SUMA_VisX_XformType2Name(SUMA_VISX_XFORM_TYPE tt)
{
   switch (tt) {
      case ID:
         return("Identity");
      case SHIFT:
         return("Translation");
      case AFFINE:
         return("Affine");
      case DISP:
         return("Displacement");
      default:
         return("Transform Type Unknown");
   }
   return("NichtGoot");
}

char *SUMA_VisX_Info(SUMA_VIS_XFORM VisX, int N_Node, char *mumble) 
{
   static char FuncName[]={"SUMA_VisX_Info"};
   int nn=0;
   char *s = NULL;
   DListElmt *el = NULL;
   SUMA_VIS_XFORM_DATUM *xx=NULL;
   SUMA_STRING *SS = NULL;

   SUMA_ENTRY;
   
   if (!mumble) mumble = "";

   SS = SUMA_StringAppend (NULL, NULL);
   SS = SUMA_StringAppend_va(SS, 
                        "%s%d xforms (%p), Applied %d, XformedCoords %p\n",
                        mumble,
                           VisX.Xchain?dlist_size(VisX.Xchain):0,
                           VisX.Xchain, 
                           VisX.Applied, VisX.XformedCoords);
   if (VisX.Xchain && dlist_size(VisX.Xchain)) {
      do {
         if (!el) el = dlist_head(VisX.Xchain);
         else el = dlist_next(el);
         xx = (SUMA_VIS_XFORM_DATUM *)el->data;
         if (xx->XformType==AFFINE || xx->XformType==SHIFT) {
            SS = SUMA_StringAppend_va(SS, 
                                    "   Xform #%d, %s, Type %s\n"
                                    "       Xform: %.3f %.3f %.3f %.3f \n"
                                    "              %.3f %.3f %.3f %.3f \n"
                                    "              %.3f %.3f %.3f %.3f \n"
                                    "              %.3f %.3f %.3f %.3f \n",
                        nn, xx->label, SUMA_VisX_XformType2Name(xx->XformType), 
                        xx->Xform[0][0], xx->Xform[0][1],
                           xx->Xform[0][2], xx->Xform[0][3],
                        xx->Xform[1][0], xx->Xform[1][1],
                           xx->Xform[1][2], xx->Xform[1][3],
                        xx->Xform[2][0], xx->Xform[2][1],
                           xx->Xform[2][2], xx->Xform[2][3],
                        xx->Xform[3][0], xx->Xform[3][1],
                           xx->Xform[3][2], xx->Xform[3][3]);
         } else if (xx->XformType==DISP) {
            SS = SUMA_StringAppend_va(SS, 
                                    "   Xform #%d, %s, Type %s\n"
                                    "       Disp: %.3f %.3f %.3f \n"
                                    "             ... \n"
                                    "             %.3f %.3f %.3f \n",
                        nn, xx->label, SUMA_VisX_XformType2Name(xx->XformType), 
                        N_Node>3 ? xx->dxyz[0]:0.0, 
                        N_Node>3 ? xx->dxyz[1]:0.0,
                        N_Node>3 ? xx->dxyz[2]:0.0,
                        N_Node>3 ? xx->dxyz[3*(N_Node-1)]:0.0, 
                        N_Node>3 ? xx->dxyz[3*(N_Node-1)+1]:0.0,
                        N_Node>3 ? xx->dxyz[3*(N_Node-1)+2]:0.0
                        );
         } else if (xx->XformType==ID) {
            SS = SUMA_StringAppend_va(SS, 
                                    "   Xform #%d, %s, Type %s\n"
                                    "   Identity\n",
                        nn, xx->label, SUMA_VisX_XformType2Name(xx->XformType));
         } else {
            SS = SUMA_StringAppend_va(SS, 
                                    "   Xform #%d, %s, Type %s\n"
                                    "   Bad news\n",
                        nn, xx->label, SUMA_VisX_XformType2Name(xx->XformType));
         }
         ++nn; 
      } while (el != dlist_tail(VisX.Xchain));
   }
   SUMA_SS2S(SS,s);
   SUMA_RETURN(s);
}

char *SUMA_ADO_Info(SUMA_ALL_DO *ado, DList *DsetList, int detail)
{
   static char FuncName[]={"SUMA_ADO_Info"};
   SUMA_STRING *SS = NULL;
   char *s = NULL;
   
   if (!ado) {
      SS = SUMA_StringAppend (NULL, NULL);
      SS = SUMA_StringAppend_va (SS, "NULL input to %s\n", FuncName);
      SS = SUMA_StringAppend (SS, NULL);
      s = SS->s; SUMA_free(SS);
      return(s);
   }
   switch(ado->do_type) {
      case SO_type:
         return(SUMA_SurfaceObject_Info((SUMA_SurfaceObject *)ado, DsetList));
      case SDSET_type:
         return(SUMA_DsetInfo((SUMA_DSET *)ado, detail));
      default:
         SS = SUMA_StringAppend (NULL, NULL);
         SS = SUMA_StringAppend_va (SS, "Not ready to give info for %s\n",
                  SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         SS = SUMA_StringAppend (SS, NULL);
         s = SS->s; SUMA_free(SS);
         return(s);
   }
   return(SUMA_copy_string("WhatTheWhat???"));
}

/*!
   \brief Creates a string containing information about the surface
   
   \param SO (SUMA_SurfaceObject *) pointer to surface object structure
   \param   DsetList (DList *) List of data sets (used to output convexity info)         
   \return s (char *) pointer to NULL terminated string containing surface info.
   It is your responsability to free it.
   \sa SUMA_Print_Surface_Object
   
*/
char *SUMA_SurfaceObject_Info (SUMA_SurfaceObject *SO, DList *DsetList)
{
   static char FuncName[]={"SUMA_SurfaceObject_Info"};
   int MaxShow = 5, i,j, ND = 0, NP = 0, N_max = 10000, eu=-1002;
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
      
   if (SO) {
      ND = SO->NodeDim;
      NP = SO->FaceSetDim;
      
      /* SO->Label */
      if (SO->Label == NULL)
         SS = SUMA_StringAppend (SS,"Label: NULL.\n");
      else   {
         SS = SUMA_StringAppend_va (SS, "Label: %s\n", SO->Label);
      }
      
      /* SO->AnatCorrect */
      if (SO->AnatCorrect) 
         SS = SUMA_StringAppend (SS,"Anatomically correct = YES\n");
      else SS = SUMA_StringAppend (SS,"Anatomically correct = NO\n");
      
      switch (SO->Side) {
         case SUMA_SIDE_ERROR:
            SS = SUMA_StringAppend (SS,"Error in side specification\n");
            break;
         case SUMA_NO_SIDE:
            SS = SUMA_StringAppend (SS,"No side specified.\n");
            break;
         case SUMA_LR:
            SS = SUMA_StringAppend (SS,"Left and Right hemispheres.\n");
            break;
         case SUMA_LEFT:
            SS = SUMA_StringAppend (SS,"Left hemisphere.\n");
            break;
         case SUMA_RIGHT:
            SS = SUMA_StringAppend (SS,"Right hemisphere.\n");
            break;
         default:
            SS = SUMA_StringAppend (SS,"Chimchunga.\n");
            break;
      }
      
      switch (SO->FileType) {
         case SUMA_SUREFIT:
            SS = SUMA_StringAppend_va (SS, "SureFit surface.\n");
            SS = SUMA_StringAppend_va (SS,"Coord FileName: %s \n", 
                                          SO->Name_coord.FileName);
            SS = SUMA_StringAppend_va (SS,"Coord Path: %s \n",  
                                          SO->Name_coord.Path);
            SS = SUMA_StringAppend_va (SS,"Topo FileName: %s \n",  
                                          SO->Name_topo.FileName);
            SS = SUMA_StringAppend_va (SS,"Topo Path: %s \n",  
                                          SO->Name_topo.Path);
            break;
         case SUMA_VEC:
            SS = SUMA_StringAppend_va (SS,"VEC surface.\n");
            SS = SUMA_StringAppend_va (SS,"NodeList FileName: %s \n",  
                                          SO->Name_coord.FileName);
            SS = SUMA_StringAppend_va (SS,"NodeList Path: %s \n",  
                                          SO->Name_coord.Path);
            SS = SUMA_StringAppend_va (SS,"FaceSetList FileName: %s \n",  
                                          SO->Name_topo.FileName);
            SS = SUMA_StringAppend_va (SS,"FaceSetList Path: %s \n",  
                                          SO->Name_topo.Path);
            break;
         case SUMA_FREE_SURFER:
         case SUMA_FREE_SURFER_PATCH:
            SS = SUMA_StringAppend_va (SS,"FreeSurfer surface.\n");
            SS = SUMA_StringAppend_va (SS,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend_va (SS,"Path: %s\n", SO->Name.Path);
            break;
         case SUMA_INVENTOR_GENERIC:
            SS = SUMA_StringAppend_va (SS,"Inventor generic surface.\n");
            SS = SUMA_StringAppend_va (SS,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend_va (SS,"Path: %s\n", SO->Name.Path);
            break;
         case SUMA_PLY: 
            SS = SUMA_StringAppend_va (SS,"PLY surface.\n");
            SS = SUMA_StringAppend_va (SS,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend_va (SS,"Path: %s\n", SO->Name.Path);
            break;
         case SUMA_MNI_OBJ: 
            SS = SUMA_StringAppend_va (SS,"MNI OBJ surface.\n");
            SS = SUMA_StringAppend_va (SS,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend_va (SS,"Path: %s\n", SO->Name.Path);
            break;
         case SUMA_OPENDX_MESH: 
            SS = SUMA_StringAppend_va (SS,"OpenDX surface.\n");
            SS = SUMA_StringAppend_va (SS,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend_va (SS,"Path: %s\n", SO->Name.Path);
            break;
         case SUMA_BRAIN_VOYAGER: 
            SS = SUMA_StringAppend_va (SS,"Brain Voyager surface.\n");
            SS = SUMA_StringAppend_va (SS,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend_va (SS,"Path: %s\n", SO->Name.Path);
            break;
         case SUMA_BYU: 
            SS = SUMA_StringAppend_va (SS,"BYU surface.\n");
            SS = SUMA_StringAppend_va (SS,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend_va (SS,"Path: %s\n", SO->Name.Path);
            break;
         case SUMA_GIFTI: 
            SS = SUMA_StringAppend_va (SS,"GIFTI surface.\n");
            SS = SUMA_StringAppend_va (SS,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend_va (SS,"Path: %s\n", SO->Name.Path);
            break;
         case SUMA_PREDEFINED: 
            SS = SUMA_StringAppend_va (SS,"Predefined surface.\n");
            SS = SUMA_StringAppend_va (SS,"FileName: %s\n", SO->Name.FileName);
            SS = SUMA_StringAppend_va (SS,"Path: %s\n", SO->Name.Path);
            break;
         case SUMA_FT_NOT_SPECIFIED:
            SS = SUMA_StringAppend_va (SS,"File Type not specified.\n");
            break;
         default:
            SS = SUMA_StringAppend_va (SS,"Unknown surface type.\n");
            break;
      }

      SS = SUMA_StringAppend_va (SS,"SpecFile:");
      if (SO->SpecFile.Path) SS = SUMA_StringAppend_va (SS,"%s",  
                                          SO->SpecFile.Path);
      if (SO->SpecFile.FileName) SS = SUMA_StringAppend_va (SS,"%s",  
                                          SO->SpecFile.FileName);
      SS = SUMA_StringAppend_va (SS,"\n");
      
      SS = SUMA_StringAppend_va (SS,"FileType: %d\t FileFormat: %d\n",  
                                          SO->FileType, SO->FileFormat);

      if (!SO->idcode_str) SS = SUMA_StringAppend_va (SS,"IDcode is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"IDcode: %s\n", SO->idcode_str);
      if (!SO->parent_vol_idcode_str) SS = SUMA_StringAppend_va  
                                          (SS,"parent_vol_IDcode is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"parent_vol_IDcode: %s\n",  
                                          SO->parent_vol_idcode_str);
      if (!SO->facesetlist_idcode_str) SS = SUMA_StringAppend_va  
                                          (SS,"faceset_IDcode is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"faceset_IDcode: %s\n",  
                                          SO->facesetlist_idcode_str);
      if (!SO->nodelist_idcode_str) SS = SUMA_StringAppend_va  
                                          (SS,"nodelist_IDcode is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"nodelist_IDcode: %s\n",  
                                          SO->nodelist_idcode_str);
      if (!SO->facenormals_idcode_str) SS = SUMA_StringAppend_va  
                                          (SS,"facenormals_IDcode is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"facenormals_IDcode: %s\n",  
                                          SO->facenormals_idcode_str);
      if (!SO->nodenormals_idcode_str) SS = SUMA_StringAppend_va  
                                          (SS,"nodenormals_IDcode is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"nodenormals_IDcode: %s\n",  
                                          SO->nodenormals_idcode_str);
      if (!SO->polyarea_idcode_str) SS = SUMA_StringAppend_va  
                                          (SS,"polyarea_IDcode is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"polyarea_IDcode: %s\n",  
                                          SO->polyarea_idcode_str);
      
      if (!SO->LocalDomainParent) SS = SUMA_StringAppend_va  
                                          (SS,"LocalDomainParent is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"LocalDomainParent: %s\n",  
                                          SO->LocalDomainParent);

      if (!SO->LocalDomainParentID) SS = SUMA_StringAppend_va  
                                          (SS,"LocalDomainParentID is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"LocalDomainParentID: %s\n",  
                                          SO->LocalDomainParentID);
     
      if (!SO->LocalCurvatureParent) SS = SUMA_StringAppend_va  
                                          (SS,"LocalCurvatureParent is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"LocalCurvatureParent: %s\n",  
                                          SO->LocalCurvatureParent);
       
      if (!SO->LocalCurvatureParentID) 
         SS = SUMA_StringAppend_va (SS,"LocalCurvatureParentID is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"LocalCurvatureParentID: %s\n",  
                                          SO->LocalCurvatureParentID);
       
      if (!SO->OriginatorID) 
         SS = SUMA_StringAppend_va (SS,"OriginatorID is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"OriginatorID: %s\n",  
                                          SO->OriginatorID);

      if (!SO->OriginatorLabel) 
         SS = SUMA_StringAppend_va (SS,"OriginatorLabel is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"OriginatorLabel: %s\n",  
                                          SO->OriginatorLabel);
       
      if (!SO->DomainGrandParentID) SS = SUMA_StringAppend_va  
                                          (SS,"DomainGrandParentID is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"DomainGrandParentID: %s\n",  
                                          SO->DomainGrandParentID);
             
      SS = SUMA_StringAppend_va (SS,
                                 "GroupLabel: %s\tGroupID: %s\t"
                                 "State: %s\t", 
                                 SO->Group, SO->Group_idcode_str,
                                 SO->State);

      if (SUMA_ismappable(SO)) {
         if (SUMA_isLocalDomainParent(SO)) {
            sprintf (stmp,"Surface is a Local Domain Parent.\n");
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
         sprintf (stmp,"ShowMeshAxis: %d\t MeshAxis Defined\n",  
                                          SO->ShowMeshAxis);
         SS = SUMA_StringAppend (SS,stmp);
      }   else {
         sprintf (stmp,"ShowMeshAxis: %d\t MeshAxis Undefined\n",  
                                          SO->ShowMeshAxis);
         SS = SUMA_StringAppend (SS,stmp);
      }  
      
      sprintf (stmp,"RenderMode: %d\n", SO->PolyMode);
      SS = SUMA_StringAppend (SS,stmp);

      sprintf (stmp,"TransMode: %d\n", SO->TransMode);
      SS = SUMA_StringAppend (SS,stmp);

      sprintf (stmp,"N_Node: %d\t NodeDim: %d, EmbedDim: %d\n", \
         SO->N_Node, SO->NodeDim, SO->EmbedDim);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"RotationWeight: %d, ViewCenterWeight %d\n",  
                     SO->RotationWeight, SO->ViewCenterWeight);
      SS = SUMA_StringAppend (SS,stmp);
      s = SUMA_VisX_Info(SO->VisX0, SO->N_Node, "VisX0: ");
      SS = SUMA_StringAppend_va(SS, s);
      SUMA_free(s); s = NULL;
      s = SUMA_VisX_Info(SO->VisX, SO->N_Node, "VisX: ");
      SS = SUMA_StringAppend_va(SS, s);
      SUMA_free(s); s = NULL;
      
      sprintf (stmp,"N_FaceSet: %d, FaceSetDim %d\n", SO->N_FaceSet, 
                     SO->FaceSetDim);
      SS = SUMA_StringAppend (SS,stmp);
      
      SUMA_EULER_SO(SO, eu);
      SS = SUMA_StringAppend_va (SS, "Euler Characteristic = %d\n\n", eu);
      
      sprintf (stmp,"Center of Mass: [%.3f\t%.3f\t%.3f]\n", 
                     SO->Center[0], SO->Center[1],SO->Center[2]);
      SS = SUMA_StringAppend (SS,stmp);
      if (  SO->isSphere == SUMA_GEOM_SPHERE || 
            SO->isSphere == SUMA_GEOM_ICOSAHEDRON ) {
         sprintf (stmp, "Surface is considered a %s.\n"
                        "Sphere Center: [%.3f\t%.3f\t%.3f]\n",
                        SUMA_GeomTypeName(SO->isSphere), 
                        SO->SphereCenter[0],
                        SO->SphereCenter[1],SO->SphereCenter[2]);
         SS = SUMA_StringAppend (SS,stmp);
         sprintf (stmp,"Sphere Radius: [%.3f]\n", SO->SphereRadius);
         SS = SUMA_StringAppend (SS,stmp);
      } else if (SO->isSphere > SUMA_GEOM_NOT_SET) {
         sprintf (stmp, "Surface geometry is considered irregular.\n"
                        "Sphere Center Set To: [%.3f\t%.3f\t%.3f]\n", 
                        SO->SphereCenter[0], SO->SphereCenter[1],
                        SO->SphereCenter[2]);
         SS = SUMA_StringAppend (SS,stmp);
         sprintf (stmp,"Sphere Radius Set To: [%.3f]\n", SO->SphereRadius);
         SS = SUMA_StringAppend (SS,stmp);
      }  else {
         sprintf (stmp, "Surface geometry has not been checked for type.\n"
                        "Sphere Center Set To: [%.3f\t%.3f\t%.3f]\n", 
                        SO->SphereCenter[0], SO->SphereCenter[1],
                        SO->SphereCenter[2]);
         SS = SUMA_StringAppend (SS,stmp);
         sprintf (stmp,"Sphere Radius Set To: [%.3f]\n", SO->SphereRadius);
         SS = SUMA_StringAppend (SS,stmp);
      }
      sprintf (stmp,"MaxDist From Center: %.3f at node %d\n", 
                     SO->MaxCentDist, SO->MaxCentDistNode);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"MinDist From Center: %.3f at node %d\n", 
                     SO->MinCentDist, SO->MinCentDistNode);
      SS = SUMA_StringAppend (SS,stmp);
      
      sprintf (stmp,"Maximum: [%.3f\t%.3f\t%.3f]\t (aMax %.3f)\n", 
                     SO->MaxDims[0], SO->MaxDims[1],SO->MaxDims[2], 
                     SO->aMaxDims);
      SS = SUMA_StringAppend (SS,stmp);

      sprintf (stmp,"Minimum: [%.3f\t%.3f\t%.3f]\t (aMin %.3f)\n\n",    
                     SO->MinDims[0], SO->MinDims[1],SO->MinDims[2], 
                     SO->aMinDims);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"SUMA_VolPar_Aligned: %d\n", SO->SUMA_VolPar_Aligned);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"APPLIED_A2Exp_XFORM: %s\n",
               SUMA_WarpTypeName(SO->APPLIED_A2Exp_XFORM));
      sprintf (stmp,"SelectedNode %d\n", SO->SelectedNode);
      SS = SUMA_StringAppend (SS,stmp);

      sprintf (stmp,"SelectedFaceSet %d\n\n", SO->SelectedFaceSet);
      SS = SUMA_StringAppend (SS,stmp);

      SS = SUMA_StringAppend (SS, SUMA_VolPar_Info(SO->VolPar));

      if (SO->NodeList == NULL) {
         sprintf (stmp,"NodeList is NULL\n\n");
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
         sprintf (stmp, 
                  "NodeList (showing %d out of %d elements):\n", 
                  MaxShow, SO->N_Node);
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

      SS = SUMA_StringAppend_va (SS, 
                     "Node Normal Direction (1=out, -1=in, 0=dunno) = %d\n",
                     SO->normdir);
      if (SO->NodeNormList == NULL) {
         sprintf (stmp,"NodeNormList is NULL\n\n");
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
         sprintf (stmp, "NodeNormList (showing %d out of %d elements):\n",
                        MaxShow, SO->N_Node);
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

      if (SO->NodeAreas == NULL) {
         sprintf (stmp,"NodeAreas is NULL\n\n");
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         if (MaxShow > SO->N_Node) MaxShow = SO->N_Node; 
         sprintf (stmp, "NodeAreas (showing %d out of %d elements):\n",
                        MaxShow, SO->N_Node);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow; ++i)   {
               sprintf (stmp, "\t%.3f\n", SO->NodeAreas[i]);
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
         sprintf (stmp, "FaceSetList: (showing %d out of %d elements):\n", 
                        MaxShow, SO->N_FaceSet);
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
         sprintf (stmp, "FaceNormList (showing %d out of %d elements):\n", 
                        MaxShow, SO->N_FaceSet);
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
         sprintf (stmp, "SO->MF (showing %d out of %d elements):\n", 
                        MaxShow, SO->N_Node);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\tNode %d: Member of %d FaceSets: ", 
                           i, SO->MF->N_Memb[i]);
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
         sprintf (stmp, 
                  "SO->FN, Max. Neighbs of %d "
                  "(showing %d out of %d elements):\n", 
                  SO->FN->N_Neighb_max, MaxShow, SO->N_Node);
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
         sprintf (stmp, "SO->EL, %d edges, %d unique edges.\n"
                        "Sloppy avg seg. length: %f\n"
                        "max_Hosts %d, min_Hosts %d "
                        "(showing %d out of %d elements):\n", 
               SO->EL->N_EL, SO->EL->N_Distinct_Edges, 
               SO->EL->AvgLe,
               SO->EL->max_N_Hosts, SO->EL->min_N_Hosts, MaxShow, SO->EL->N_EL);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\tEdge %d: %d %d\tFlip %d Tri %d N_tri %d\n",
                i, SO->EL->EL[i][0], SO->EL->EL[i][1], SO->EL->ELps[i][0],
                SO->EL->ELps[i][1],SO->EL->ELps[i][2]);
            SS = SUMA_StringAppend (SS,stmp);   
         }
         sprintf (stmp,"\n");
         SS = SUMA_StringAppend (SS,stmp);
         
         if (MaxShow > SO->N_FaceSet) MaxShow = SO->N_FaceSet; 
         sprintf (stmp, 
                  "Triangle Limbs, (showing %d out of %d elements):\n", 
                  MaxShow, SO->N_FaceSet);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\tTri_limb[%d][:] = %d %d %d\n", 
                           i, SO->EL->Tri_limb[i][0], 
                           SO->EL->Tri_limb[i][1],SO->EL->Tri_limb[i][2]);
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
         sprintf (stmp, 
                  "SO->PolyArea, showing %d out of %d elements:\n", 
                  MaxShow, SO->N_FaceSet);
         SS = SUMA_StringAppend (SS,stmp);
         for (i=0; i < MaxShow ; ++i)   {
            sprintf (stmp,"\tFaceSet %d: Area = %f\n", i, SO->PolyArea[i]);
            SS = SUMA_StringAppend (SS,stmp);
         }
      }
      sprintf (stmp,"\n");
      SS = SUMA_StringAppend (SS,stmp);
      
      if (DsetList) {
         float *Cx = NULL;
         Cx = (float *)SUMA_GetCx(SO->idcode_str, DsetList, 0);
         if (Cx == NULL) {
            sprintf (stmp,"Cx = NULL\n\n") ;
            SS = SUMA_StringAppend (SS,stmp);
         } else {
            if (MaxShow > SO->N_Node) MaxShow = SO->N_Node;
            sprintf (stmp, "Cx, showing %d out of %d elements:\n", 
                           MaxShow, SO->N_Node);
            SS = SUMA_StringAppend (SS,stmp);
            for (i=0; i < MaxShow ; ++i)   {
               sprintf (stmp,"\t Cx[%d] = %f\n", i, Cx[i]);
               SS = SUMA_StringAppend (SS,stmp);
            }
         }
      } else {
         SS = SUMA_StringAppend (SS, "NULL DsetList, No Cx can be found.\n");
      }
      
      if (SO->N_Overlays) {
         sprintf (stmp,"%d Overlay planes.\n", SO->N_Overlays);
         SS = SUMA_StringAppend (SS,stmp);
         s = SUMA_ColorOverlayPlane_Info(SO->Overlays, SO->N_Overlays, 0);
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
      
      if (!SO->PermCol) SUMA_StringAppend (SS,"PermCol = NULL\n");
      else SUMA_StringAppend (SS,"PermCol is NOT NULL\n");
      
      if (  (SO->PermCol && SO->N_Overlays) || 
            (SO->PermCol && SO->N_Overlays) ) {
         SUMA_StringAppend (SS,  "CONFLICT! "
                                 "Both PermCol and Overlays are specified!\n");
      }
      
      s = SUMA_AfniSurfaceObject_Info(SO->aSO, 0, NULL);
      
      SUMA_StringAppend (SS, s);
      SUMA_free(s); s = NULL;
      
   } else {
      sprintf (stmp, "NULL Surface Object Pointer.");
      SS = SUMA_StringAppend (SS, stmp);
   }   
   
   if (SO->CommonNodeObject) {
      s = SUMA_DOv_Info(SO->CommonNodeObject, 1, 1);
      SUMA_StringAppend (SS, s);
      SUMA_free(s); s = NULL;
   } else {
      SS = SUMA_StringAppend (SS, "CommonNodeObject is NULL\n");
   }
   
   if (SO->NodeObjects) {
      s = SUMA_DOv_Info(SO->NodeObjects, 1, 1);
      SUMA_StringAppend (SS, s);
      SUMA_free(s); s = NULL;
   } else {
      SS = SUMA_StringAppend (SS, "NodeObjects is NULL\n");
   }
   
   if (SO->NodeNIDOObjects) {
      SUMA_StringAppend (SS, "NodeNIDOObjects is NOT NULL\n");
   } else {
      SS = SUMA_StringAppend (SS, "NodeNIDOObjects is NULL\n");
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
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;
   
   if (SUMAg_CF)    
      s = SUMA_SurfaceObject_Info (SO, SUMAg_CF->DsetList);
   else 
      s = SUMA_SurfaceObject_Info (SO, NULL);
      
   if (s) {
      fprintf (Out, "%s", s);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_SurfaceObject_Info.\n", FuncName);
   }   
   
   SUMA_RETURNe;
}   

char *SUMA_SO_GeometricType(SUMA_SurfaceObject *SO) {
   static char FuncName[]={"SUMA_SO_GeometricType"};
   char *cc=NULL;
   
   SUMA_ENTRY;
   
   if (SO->aSO) {
      cc = SUMA_NI_AttrOfNamedElement(SO->aSO, 
                                      "Node_XYZ","GeometricType");
      SUMA_RETURN(cc);
   }
   
   if (SO->isSphere == SUMA_GEOM_SPHERE) {
      SUMA_RETURN("Spherical");
   }
   /* if need be, try guessing for different common surface type */  
   
   SUMA_RETURN("Unknown");
}

char *SUMA_SO_AnatomicalStructureSecondary(SUMA_SurfaceObject *SO) {
   static char FuncName[]={"SUMA_SO_AnatomicalStructureSecondary"};
   char *cc=NULL;
   
   SUMA_ENTRY;
   
   if (SO->aSO) {
      cc = SUMA_NI_AttrOfNamedElement(SO->aSO, 
                                      "Node_XYZ",
                                      "AnatomicalStructureSecondary");
     SUMA_RETURN(cc);
   }
   
   /* some guessing from FreeSurfer settings */
   if (  SUMA_iswordin_ci(SO->State,"pial") == 1 ||
         SUMA_iswordin_ci(SO->Label,"pial") == 1 ||
         SUMA_iswordin_ci(SO->Name.FileName,"pial") == 1 ) 
            SUMA_RETURN("Pial");
   if (SUMA_iswordin_ci(SO->State,"smoothwm") == 1||
         SUMA_iswordin_ci(SO->Label,"smoothwm") == 1 ||
         SUMA_iswordin_ci(SO->Name.FileName,"smoothwm") == 1)
            SUMA_RETURN("GrayWhite");
   if (SUMA_iswordin_ci(SO->State,"white") == 1||
         SUMA_iswordin_ci(SO->Label,"white") == 1 ||
         SUMA_iswordin_ci(SO->Name.FileName,"white") == 1)
               SUMA_RETURN("GrayWhite");
   
   
   SUMA_RETURN("Unknown");
}

char *SUMA_SO_AnatomicalStructurePrimary(SUMA_SurfaceObject *SO) {
   static char FuncName[]={"SUMA_SO_AnatomicalStructurePrimary"};
   char *cc=NULL;
   
   SUMA_ENTRY;
   
   if (SO->aSO) {
      cc = SUMA_NI_AttrOfNamedElement(SO->aSO, 
                                      "Node_XYZ",
                                      "AnatomicalStructurePrimary");
      SUMA_RETURN(cc);
   }
   
   /* weak guess, based on side */
   if (SO->Side <= SUMA_NO_SIDE) SO->Side = SUMA_GuessSide(SO);
   if (SO->Side == SUMA_LEFT) SUMA_RETURN("CortexLeft");
   if (SO->Side == SUMA_RIGHT) SUMA_RETURN("CortexRight");
   if (SO->Side == SUMA_LR) SUMA_RETURN("CortexRightAndLeft");
   
   
   SUMA_RETURN("Unknown");
}
char *SUMA_SO_TopologicalType(SUMA_SurfaceObject *SO) {
   static char FuncName[]={"SUMA_SO_TopologicalType"};
   char *cc=NULL;
   
   SUMA_ENTRY;
   
   if (SO->aSO) {
      cc = SUMA_NI_AttrOfNamedElement(SO->aSO, 
                                      "Mesh_IJK",
                                      "TopologicalType");
      SUMA_RETURN(cc);
   }
   
   /* guess, based on edges */
   if (SO->EL) {
      if (  SO->EL->min_N_Hosts == SO->EL->max_N_Hosts &&
            SO->EL->min_N_Hosts == 2 ) SUMA_RETURN("Closed");
      else if (SO->EL->min_N_Hosts == 1) SUMA_RETURN("Open"); /* could also be
                                                                 cut...*/   
      else if (SO->EL->max_N_Hosts > 2) SUMA_RETURN("Not_2_Manifold");
   }
   
   SUMA_RETURN("Unknown");
}
/*
   Keep function in sync with SUMA_ExtractAfniSO_FromSumaSO
*/
SUMA_Boolean SUMA_MergeAfniSO_In_SumaSO(NI_group **aSOp,
                                        SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_MergeAfniSO_In_SumaSO"};
   NI_group *aSO=NULL;
   int i,j,k;
   double *dv=NULL, xform[4][4];
   NI_element *nelxyz=NULL, *nelnormals=NULL, *nelxform=NULL, *nelijk=NULL;

   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;

   if (!SO) SUMA_RETURN(NOPE);
   if (aSOp) {
      if (SO->aSO) {
         SUMA_S_Err("Not ready to merge with pre-existing aSO");
         SUMA_RETURN(NOPE);
      }
      aSO = *aSOp; 
      if (!aSO) SUMA_RETURN(NOPE);

      if (SO->NodeList || SO->FaceSetList || SO->NodeNormList) {
         SUMA_S_Err("This assumes SO->NodeList and others like it to be NULL!");
         SUMA_RETURN(NOPE);
      }
      /* copy common fields one by one. 
         Follow AFNI_SurfaceObject structure closely.
         Keep new fields in aSO */
      SUMA_LH("Moving node coordinates");
      nelxyz = SUMA_FindNgrNamedElement(aSO, "Node_XYZ");
      SO->N_Node = SUMA_NI_get_int(nelxyz, "N_Node"); 
      SO->NodeDim = SUMA_NI_get_int(nelxyz, "NodeDim"); 
      SO->EmbedDim = SUMA_NI_get_int(nelxyz, "EmbedDim");
      if (nelxyz->vec_num) {
         if (!(SO->NodeList = (float *)SUMA_calloc(SO->NodeDim*SO->N_Node,
                                                  sizeof(float)))) {
            SUMA_S_Err("Failed to allocate");
            SUMA_RETURN(NOPE);
         }
         memcpy(  SO->NodeList, nelxyz->vec[0], 
                  SO->NodeDim*SO->N_Node*sizeof(float));
         NI_remove_column(nelxyz,0);
      } else {
         SO->NodeList = NULL;
      }
      
      
      SUMA_LH("Moving mesh ijk");
      nelijk = SUMA_FindNgrNamedElement(aSO, "Mesh_IJK");
      SO->N_FaceSet = SUMA_NI_get_int(nelijk, "N_FaceSet"); 
      SO->FaceSetDim = SUMA_NI_get_int(nelijk, "FaceSetDim"); 
      if (nelijk->vec_num) {
         if (!(SO->FaceSetList = (int *)
                                 SUMA_calloc(SO->FaceSetDim*SO->N_FaceSet,
                                             sizeof(int)))) {
            SUMA_S_Err("Failed to allocate");
            SUMA_RETURN(NOPE);
         }
         memcpy(  SO->FaceSetList, nelijk->vec[0], 
                  SO->FaceSetDim*SO->N_FaceSet*sizeof(int)); 
         NI_remove_column(nelijk,0);
      } else {
         SO->FaceSetList = NULL;
      }
      SUMA_LH("Moving normals");
      nelnormals = SUMA_FindNgrNamedElement(aSO, "Node_Normals");
      if (nelnormals->vec_num) {
         if (!(SO->NodeNormList = (float *)SUMA_calloc(SO->NodeDim*SO->N_Node,
                                                       sizeof(float)))) {
            SUMA_S_Err("Failed to allocate");
            SUMA_RETURN(NOPE);
         } 
         memcpy(  SO->NodeNormList, nelnormals->vec[0], 
                  SO->NodeDim*SO->N_Node*sizeof(float));
         NI_remove_column(nelnormals,0);
         SO->glar_NodeNormList = (GLfloat *) SO->NodeNormList; 
      } else {
         SO->NodeNormList = NULL;
         SO->glar_NodeNormList = NULL;
      }

      SO->aSO = aSO;
      *aSOp = NULL; /* allow no one to touch this anymore */

      /* Is there an xform to apply ? */
      SUMA_LH("Dealing with Xforms.\n"
              "Note that transform is not applied to normals.\n"
              "Not sure what the standard dictates for such a case.");
      if (!SUMA_GetSOCoordXform(SO, xform)) {
         SUMA_S_Err("Failed to get xform!");
         NI_set_attribute(nelxyz,"inxformspace","no");
      } else {
         if (!SUMA_Apply_Coord_xform(SO->NodeList, SO->N_Node, SO->NodeDim,
                                     xform, 0, NULL)) {
            SUMA_S_Err("Failed to apply xform!");
            NI_set_attribute(nelxyz,"inxformspace","no");
         }else{
         NI_set_attribute(nelxyz,"inxformspace","yes");
         }
      }
      
      /* Now fill up some additional fields */
      SUMA_LH("Filling extras");
      SO->Side = SUMA_GuessSide(SO);
      if (SO->isSphere == SUMA_GEOM_NOT_SET) SUMA_SetSphereParams(SO, -0.1);
      SO->AnatCorrect = SUMA_GuessAnatCorrect(SO);
      SO->State = 
         SUMA_append_replace_string(
            NI_get_attribute(nelxyz,"GeometricType"),
            NI_get_attribute(nelxyz,"AnatomicalStructureSecondary"),
            ".", 0);

   } else { /* add new one */
      SUMA_LH("Adding a new aSO");
      if (SO->aSO) {
         SUMA_S_Err("aSO already exists.");
         SUMA_RETURN(NOPE);
      }
      aSO = SUMA_NewAfniSurfaceObject();
      nelxyz = SUMA_FindNgrNamedElement(aSO, "Node_XYZ");
      NI_alter_veclen(nelxyz,SO->NodeDim*SO->N_Node);
      nelijk = SUMA_FindNgrNamedElement(aSO, "Mesh_IJK");
      NI_alter_veclen(nelijk,SO->FaceSetDim*SO->N_FaceSet);
      nelnormals = SUMA_FindNgrNamedElement(aSO, "Node_Normals");
      NI_alter_veclen(nelnormals,SO->NodeDim*SO->N_Node);
      nelxform = SUMA_FindNgrNamedElement(aSO, "Coord_System");
      /* fillup IDs, Creating function cannot call ID creating routines */
      SUMA_PUT_ID_ATTR(nelxyz,"idcode_str", NULL);
      SUMA_PUT_ID_ATTR(nelijk, "idcode_str", NULL);
      
      /* fillup date */
      {  char *date=tross_datetime();
         NI_set_attribute(nelxyz,"date", date); 
         NI_set_attribute(nelijk,"date", date);
         free(date);
      }
      
      /* populate xform with junk since this concept does not exist in SO */
      NI_set_attribute(nelxyz,"inxformspace","no");
      NI_set_attribute(nelxform, "dataspace", "NIFTI_XFORM_UNKNOWN");
      NI_set_attribute(nelxform, "xformspace", "NIFTI_XFORM_UNKNOWN");
      dv = (double *)nelxform->vec[0];
      k = 0;
      for (i=0; i<4;++i) 
         for (j=0; j<4;++j) { 
            if (i==j) dv[k]=1.0;
            else dv[k]=0.0;
            ++k;
         }
      
      /* Have aSO, now start filling it up */
      SUMA_LH("Filling new aSO");
      NI_SET_INT(nelxyz,"N_Node",SO->N_Node) ; 
      NI_SET_INT(nelxyz,"NodeDim", SO->NodeDim); 
      NI_SET_INT(nelxyz,"EmbedDim", SO->EmbedDim) ; 
      
      NI_SET_INT(nelijk,"N_FaceSet", SO->N_FaceSet) ; 
      NI_SET_INT(nelijk,"FaceSetDim", SO->FaceSetDim)  ; 
      
      NI_set_attribute( nelxyz,
                        "GeometricType",
                        SUMA_SO_GeometricType(SO));
      NI_set_attribute( nelxyz,
                        "AnatomicalStructureSecondary",
                        SUMA_SO_AnatomicalStructureSecondary(SO));
      NI_set_attribute( nelxyz,
                        "AnatomicalStructurePrimary",
                        SUMA_SO_AnatomicalStructurePrimary(SO));
      NI_set_attribute( nelijk,
                        "TopologicalType",
                        SUMA_SO_TopologicalType(SO));    
      SO->aSO = aSO; 
     
   }
   
   SUMA_RETURN(YUP);
}

/* wipe out any pre-existing xform in aSO */
SUMA_Boolean SUMA_Blank_AfniSO_Coord_System(NI_group *aSO)
{
   int i, j;
   double *dv;
   NI_element *nelxform=NULL;
   if (aSO) { 
      if (!(nelxform = SUMA_FindNgrNamedElement(aSO, "Coord_System"))){
         nelxform = NI_new_data_element("Coord_System", 16);
         NI_add_column(nelxform,NI_DOUBLE,NULL);
         NI_add_to_group(aSO, nelxform);
      }
      dv = (double *)nelxform->vec[0];
      if (dv) { /* make identity */
         for (i=0; i<4; ++i) 
            for (j=0; j<4; ++j) {
               if (i==j) dv[i*4+j]=1.0; 
               else dv[i*4+j]= 0.0;
            }
      }
      NI_set_attribute( nelxform,"dataspace","NIFTI_XFORM_UNKNOWN");
      NI_set_attribute( nelxform,"xformspace","NIFTI_XFORM_UNKNOWN");
   }
   return(YUP);
}

int SUMA_NumVE(SUMA_VolumeObject *VO) {
   int i=0;
   if (!VO) SUMA_RETURN(-1);
   if (VO->VE) {
      while (VO->VE[i]) ++i;
   }
   return(i);
}

SUMA_Boolean SUMA_AddDsetVolumeObject( SUMA_VolumeObject *VO, 
                                       THD_3dim_dataset **dsetp) {
   static char FuncName[]={"SUMA_AddDsetVolumeObject"};
   THD_3dim_dataset *dset=NULL;
   int n_VE=0;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (dsetp) dset = *dsetp;
   
   if (!dset) {
      SUMA_S_Err("Got nothing to work with!");
      SUMA_RETURN(NOPE);
   }
   n_VE = SUMA_NumVE(VO);
   
   if (dset) {
      SUMA_S_Warn("This is only setup to work with one sub-brick\n"
                  "Need to handle RGB types also at some point");
      SUMA_LHv("Adding dset %s in slot %d\n", DSET_HEADNAME(dset), n_VE);
      /* initialize */
      VO->VE[n_VE] = (SUMA_VolumeElement*)SUMA_calloc(1,
                                       sizeof(SUMA_VolumeElement));
      VO->VE[n_VE]->dset = dset; dset = NULL;
      SUMA_S_Note("Copy into texture");
      
      if (!(VO->VE[n_VE]->texvec = 
            SUMA_dset_to_tex3d(&(VO->VE[n_VE]->dset), (byte)n_VE))) {
         SUMA_S_Err("Failed in dset to text3d");
         VO = SUMA_FreeVolumeObject(VO);
         SUMA_RETURN(NOPE);
      }
      SUMA_LHv("Have slot %d,%s\n", n_VE, DSET_HEADNAME(VO->VE[n_VE]->dset));
      /* Set the box limits, assuming you are in RAI */
      SUMA_dset_extreme_corners(VO->VE[n_VE]->dset, 
                              VO->VE[n_VE]->vo0, VO->VE[n_VE]->voN);
   }
   
   if (dsetp) { /* make sure user can't manipulate initial volume */
      *dsetp=NULL;
   }
   
   SUMA_RETURN(YUP);
}
    
/*!
Create a Volume Object data structure 
*/
SUMA_VolumeObject *SUMA_CreateVolumeObject(char *Label)
{
   static char FuncName[]={"SUMA_CreateVolumeObject"};
   SUMA_VolumeObject *VO=NULL;
   int i, j, newval;
   int Texcomps=1, max3dtexdims=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   
   VO = (SUMA_VolumeObject *)SUMA_calloc(1,sizeof(SUMA_VolumeObject));
   if (VO == NULL) {
      SUMA_S_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   
   VO->do_type = VO_type;
   if (Label) {
      VO->Label = SUMA_copy_string(Label);
   } else {
      VO->Label = SUMA_copy_string("NoLabel");
   }
   VO->idcode_str = UNIQ_hashcode(VO->Label);
   VO->VoxelMarker = NULL;
   VO->Show = 1;
   
   VO->VE = (SUMA_VolumeElement **)
      SUMA_calloc(SUMA_MAX_N_VE, sizeof(SUMA_VolumeElement *));

   VO->CutPlane[0][0] = -1.0;  
   VO->CutPlane[0][1] = 0.0; 
   VO->CutPlane[0][2] = 0.0;     
   VO->CutPlane[0][3] = 50.0;

   VO->CutPlane[1][0] = 1.0;  
   VO->CutPlane[1][1] = 0.0; 
   VO->CutPlane[1][2] = 0.0;     
   VO->CutPlane[1][3] = 50.0;
    
   VO->CutPlane[2][0] = 0.0;  
   VO->CutPlane[2][1] = -1.0; 
   VO->CutPlane[2][2] = 0.0;     
   VO->CutPlane[2][3] = 50.0;
    
   VO->CutPlane[3][0] = 0.0;  
   VO->CutPlane[3][1] = 1.0; 
   VO->CutPlane[3][2] = 0.0;     
   VO->CutPlane[3][3] = 50.0;
    
   VO->CutPlane[4][0] = 0.0;  
   VO->CutPlane[4][1] = 0.0; 
   VO->CutPlane[4][2] = -1.0;     
   VO->CutPlane[4][3] = 50.0;

   VO->CutPlane[5][0] = 0.0;  
   VO->CutPlane[5][1] = 0.0; 
   VO->CutPlane[5][2] = 1.0;     
   VO->CutPlane[5][3] = 50.0;
    
   VO->UseCutPlane[0] = 1;
   VO->UseCutPlane[1] = 1;
   for (i=2; i<6; ++i) {
      VO->UseCutPlane[i] = 1;
   }
   VO->SelectedCutPlane = 0;
   
   VO->SelectedVoxel = -1;
   VO->ShowSelectedVoxel = 0;

   
   VO->SOcut = (SUMA_SurfaceObject **)SUMA_calloc(6, 
                                 sizeof(SUMA_SurfaceObject *));
   SUMA_RETURN(VO);
}/* SUMA_CreateVolumeObject */


SUMA_VolumeObject *SUMA_FreeVolumeObject(SUMA_VolumeObject *VO) {
   static char FuncName[]={"SUMA_FreeVolumeObject"};
   int i;
   SUMA_ENTRY;
   
   if (!VO) SUMA_RETURN(NULL);
   
   if (VO->VE) {
      i = 0;
      while (VO->VE[i]) {
         if (VO->VE[i]->dset) {
            DSET_delete(VO->VE[i]->dset) ;
            VO->VE[i]->dset = NULL;     
         }
         if (VO->VE[i]->texName) 
            SUMA_free(VO->VE[i]->texName); VO->VE[i]->texName = NULL;
         if (VO->VE[i]->texvec) 
            SUMA_free(VO->VE[i]->texvec); VO->VE[i]->texvec = NULL;
         SUMA_free(VO->VE[i]);
         ++i;
      }
      SUMA_free(VO->VE);
   }
   if (VO->VoxelMarker) {
      SUMA_S_Warn("Don't know how to free this yet! Leak Leak!");
   }
   if (VO->idcode_str) SUMA_free(VO->idcode_str); VO->idcode_str=NULL; 
   if (VO->Label) SUMA_free(VO->Label); VO->Label = NULL;
   if (VO->SOcut) {
      for (i=0; i<6; ++i) {
         if (VO->SOcut[i]) SUMA_Free_Surface_Object(VO->SOcut[i]);
      }
      SUMA_free(VO->SOcut);
   }
   
   SUMA_free(VO);
   
   SUMA_RETURN(NULL);
}
/*!
Create a Surface Object data structure 
*/

SUMA_SurfaceObject *SUMA_Alloc_SurfObject_Struct(int N)
{
   static char FuncName[]={"SUMA_Alloc_SurfObject_Struct"};
   SUMA_SurfaceObject *SO;
   int i, j;
   
   SUMA_ENTRY;

   SO = (SUMA_SurfaceObject *)SUMA_calloc(N, sizeof(SUMA_SurfaceObject));
   if (SO == NULL) {
      SUMA_alloc_problem(  "SUMA_Alloc_SurfObject_Struct:\n"
                           "could not allocate memory for SO");
   }
   
   for (i=0; i< N; ++i) {
      memset(&(SO[i]), 0, sizeof(SUMA_SurfaceObject));
      SO[i].do_type = SO_type;
      SO[i].FileType = SUMA_FT_NOT_SPECIFIED;
      SO[i].FileFormat = SUMA_FF_NOT_SPECIFIED;
      SO[i].NodeMarker = NULL;
      SO[i].SelectedNode = -1;
      SO[i].Name_NodeParent = NULL;
      SO[i].Label = NULL;
      SO[i].EmbedDim = 3;
               /* the zeros in Center, MaxDims, MinDims, */ 
               /* aMinDims and aMaxDims */
               /* are used to flag unitialized parameters */
               /* always keep zero for initialization */
               /* see SUMA_isSODimInitialized */      
      SO[i].Center[0] = SO[i].Center[1] = SO[i].Center[2] = 0.0;           
      SO[i].MaxDims[0] = SO[i].MaxDims[1] = SO[i].MaxDims[2] = 0.0;        
      SO[i].MinDims[0] = SO[i].MinDims[1] = SO[i].MinDims[2] = 0.0;        
      SO[i].aMinDims = 0.0;                                                
      SO[i].aMaxDims = 0.0;                                                
      SO[i].MaxCentDist = 0.0;
      SO[i].MaxCentDistNode = -1;
      SO[i].MinCentDist = 0.0;
      SO[i].MinCentDistNode = -1;
      
      SO[i].ViewCenterWeight = -1;
      SO[i].RotationWeight = -1;
      SO[i].patchNodeMask = NULL;
      SO[i].patchaMaxDims = 0.0;
      SO[i].patchaMinDims = 0.0;
      SO[i].patchMinDims[0] = SO[i].patchMinDims[1] = 
                                    SO[i].patchMinDims[2] = 0.0;
      SO[i].patchMaxDims[0] = SO[i].patchMaxDims[1] = 
                                    SO[i].patchMaxDims[2] = 0.0;
      SO[i].patchCenter[0] = SO[i].patchCenter[1] = 
                                    SO[i].patchCenter[2] = 0.0;
      SO[i].N_patchNode = 0;
      SO[i].MF = NULL;
      SO[i].FN = NULL;
      SO[i].EL = NULL;
      SO[i].PolyArea = NULL;
      SO[i].SC = NULL;
      SO[i].VolPar = NULL;
      SO[i].NodeDim = 0;
      SO[i].N_Node = 0;
      SO[i].NodeList = NULL; 
      SO[i].FaceSetDim = 0;
      SO[i].N_FaceSet = 0;
      SO[i].FaceSetList = NULL; 
      SO[i].FaceNormList = NULL; 
      SO[i].NodeNormList = NULL;
      SO[i].normdir = 0; 
      SO[i].glar_NodeList = NULL; 
      SO[i].glar_FaceSetList = NULL; 
      SO[i].glar_FaceNormList = NULL; 
      SO[i].glar_NodeNormList = NULL;
      SO[i].NodeList_swp = NULL; 
      /* create vector of pointers */
      SO[i].Overlays = 
         (SUMA_OVERLAYS **)
            SUMA_malloc(sizeof(SUMA_OVERLAYS *) * SUMA_MAX_OVERLAYS);
      /* fill pointers with NULL */
      for (j=0; j < SUMA_MAX_OVERLAYS; ++j) {
         SO[i].Overlays[j] = NULL;
      }
      SO[i].N_Overlays = 0;
      SO[i].SentToAfni = NOPE;
      
      SO[i].MeshAxis = NULL;
      SO[i].ShowMeshAxis = -1;
      SO[i].State = NULL;
      SO[i].Group = NULL;
      SO[i].FaceSetMarker = NULL;
      SO[i].SelectedFaceSet = -1;
      SO[i].idcode_str = NULL;
      SO[i].facesetlist_idcode_str = NULL;
      SO[i].nodelist_idcode_str = NULL;
      SO[i].facenormals_idcode_str = NULL;
      SO[i].nodenormals_idcode_str = NULL;
      SO[i].polyarea_idcode_str = NULL;
      SO[i].SpecFile.Path = NULL;
      SO[i].SpecFile.FileName = NULL;
      SO[i].Name.Path = NULL;
      SO[i].Name.FileName = NULL;
      SO[i].Name_coord.Path = NULL;
      SO[i].Name_coord.FileName = NULL;
      SO[i].Name_topo.Path = NULL;
      SO[i].Name_topo.FileName = NULL;
      SO[i].SUMA_VolPar_Aligned = NOPE;
      SO[i].APPLIED_A2Exp_XFORM = NO_WARP;
      SO[i].SurfCont = NULL; /* This is now handled in SUMA_LoadSpec_eng 
                              (used to be SUMA_CreateSurfContStruct();) */
      SO[i].PolyMode = SRM_ViewerDefault;
      SO[i].Show = YUP;
      SO[i].Side = SUMA_NO_SIDE;
      SO[i].isSphere = SUMA_GEOM_NOT_SET;
      SO[i].SphereRadius = -1.0;
      SO[i].SphereCenter[0] = -1.0; 
      SO[i].SphereCenter[1] = -1.0; 
      SO[i].SphereCenter[2] = -1.0; 
      SO[i].AnatCorrect = NOPE;
      SO[i].DomainGrandParentID = NULL;
      SO[i].OriginatorID = NULL;
      SO[i].LocalDomainParent = NULL;
      SO[i].LocalCurvatureParent = NULL;
      SO[i].LocalDomainParentID = NULL;
      SO[i].LocalCurvatureParentID = NULL;
      SO[i].PermCol = NULL;
      SO[i].Group_idcode_str = NULL;
      SO[i].OriginatorLabel = NULL;
      SO[i].parent_vol_idcode_str = NULL;
      
      SO[i].aSO = NULL;
      
      SO[i].CommonNodeObject = NULL;
      SO[i].NodeObjects = NULL;
      SO[i].NodeNIDOObjects = NULL;
      SO[i].NodeAreas = NULL;
      
      SUMA_EmptyVisXform(&(SO[i].VisX0));
      SUMA_EmptyVisXform(&(SO[i].VisX));
     }
   SUMA_RETURN(SO);
}/* SUMA_Alloc_SurfObject_Struct */

SUMA_Boolean SUMA_nixSODim(SUMA_SurfaceObject *SO) 
{
   if (!SO) return(NOPE);
   
   SO->MaxDims[0] = SO->MaxDims[1] = SO->MaxDims[2] = 0.0;
   SO->MinDims[0] = SO->MinDims[1] = SO->MinDims[2] = 0.0;
   SO->aMinDims = SO->aMaxDims == 0.0;
   return(YUP);
}

SUMA_Boolean SUMA_isSODimInitialized(SUMA_SurfaceObject *SO) 
{
   if (!SO) return(NOPE);
   
   if (  SO->MaxDims[0] == 0.0 && SO->MaxDims[1] == 0.0 && 
                                          SO->MaxDims[2] == 0.0 &&
         SO->MinDims[0] == 0.0 && SO->MinDims[1] == 0.0 && 
                                          SO->MinDims[2] == 0.0 &&
         SO->aMinDims == 0.0 && SO->aMaxDims == 0.0) { 
         
         return(NOPE); 
   }
   return(YUP);
}

SUMA_Boolean SUMA_SetSODims(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_SetSODims"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) SUMA_RETURN(NOPE);
      SUMA_MIN_MAX_SUM_VECMAT_COL (
            SO->NodeList, SO->N_Node, 
            SO->NodeDim, SO->MinDims, 
            SO->MaxDims, SO->Center);

      SO->Center[0] /= SO->N_Node;
      SO->Center[1] /= SO->N_Node;
      SO->Center[2] /= SO->N_Node;

      SUMA_MIN_VEC (SO->MinDims, 3, SO->aMinDims );
      SUMA_MAX_VEC (SO->MaxDims, 3, SO->aMaxDims);
      
      SUMA_SO_MAX_MIN_DIST(SO, SO->MaxCentDist, SO->MaxCentDistNode,
                               SO->MinCentDist, SO->MinCentDistNode);
       
   SUMA_LHv("Min:[%f %f %f]\n"
            "Max:[%f %f %f]\n"
            "aMax: %f, aMin %f\n"
            "Max Dist To Cent: %f at %d\n"
            "Min Dist To Cent: %f at %d\n",
            SO->MinDims[0], SO->MinDims[1],SO->MinDims[2],
            SO->MaxDims[0], SO->MaxDims[1],SO->MaxDims[2],
            SO->aMaxDims, SO->aMinDims,
            SO->MaxCentDist, SO->MaxCentDistNode,
            SO->MinCentDist, SO->MinCentDistNode);
   
   SUMA_RETURN(YUP);
}

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
   
   SUMA_ENTRY;
      
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
   
   SUMA_ENTRY;
   
   
   if (D_ROI->Parent_idcode_str) SUMA_free(D_ROI->Parent_idcode_str);
   if (D_ROI->idcode_str) SUMA_free(D_ROI->idcode_str);
   if (D_ROI->Label) SUMA_free(D_ROI->Label);
   if (D_ROI->ColPlaneName) SUMA_free(D_ROI->ColPlaneName); 
   if (D_ROI->ROIstrokelist) SUMA_EmptyDestroyList(D_ROI->ROIstrokelist);
   if (D_ROI->ActionStack) SUMA_EmptyDestroyActionStack(D_ROI->ActionStack);
   if (D_ROI->CE) SUMA_free(D_ROI->CE);
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
   
   SUMA_ENTRY;
   
   ROI = (SUMA_ROI *) SUMA_calloc(1,sizeof(SUMA_ROI));
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
   
   \sa SUMA_NIMLDrawnROI_to_DrawnROI where a SUMA_DRAWN_ROI is also created
*/
SUMA_DRAWN_ROI *SUMA_AllocateDrawnROI (
   char *Parent_idcode_str, 
   SUMA_ROI_DRAWING_STATUS DrawStatus, 
   SUMA_ROI_DRAWING_TYPE Type, 
   char *label, int ilabel) 
{
   SUMA_DRAWN_ROI *D_ROI = NULL;
   static int ROI_index = 1;
   char stmp[32], sd='\0';
   SUMA_SurfaceObject *SO=NULL;
   static char FuncName[]={"SUMA_AllocateDrawnROI"};
   
   SUMA_ENTRY;
   
   D_ROI = (SUMA_DRAWN_ROI *) SUMA_calloc(1,sizeof(SUMA_DRAWN_ROI));
   D_ROI->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));
   D_ROI->Parent_idcode_str = 
      (char *)SUMA_calloc (strlen(Parent_idcode_str)+1, sizeof (char));
   D_ROI->Parent_side = SUMA_NO_SIDE;
   /* get some decent name for colplane */
   SO = SUMA_findSOp_inDOv(Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   if (SO && SO->Label) {
      if (SO->Side == SUMA_LEFT) {
         sd = 'L';
      } else if (SO->Side == SUMA_RIGHT) {
         sd = 'R';
      } else if (SO->Side == SUMA_LR) {
         sd = 'B';
      } else if (SO->Side == SUMA_NO_SIDE) {
         sd = '-';
      } else if (SO->Side == SUMA_SIDE_ERROR) {
         sd = 'E';
      } 
      snprintf(stmp,12*sizeof(char),".%c.%s",sd,SO->State);
      D_ROI->ColPlaneName = SUMA_append_string("ROI",stmp);
      D_ROI->Parent_side = SO->Side;
   } else {
      D_ROI->ColPlaneName = SUMA_copy_string("DefROIpl");
   }
   D_ROI->FillColor[0] = 1.0; 
   D_ROI->FillColor[1] = 0.0; 
   D_ROI->FillColor[2] = 0.0;
   D_ROI->FillColor[3] = 1.0;
   D_ROI->EdgeColor[0] = 0.0; 
   D_ROI->EdgeColor[1] = 0.0; 
   D_ROI->EdgeColor[2] = 1.0;
   D_ROI->EdgeColor[3] = 1.0;
   D_ROI->EdgeThickness = 2;
   D_ROI->ROIstrokelist = (DList *)SUMA_calloc(1,sizeof(DList));
   dlist_init(D_ROI->ROIstrokelist, SUMA_FreeROIDatum);
   D_ROI->CE = NULL;
   D_ROI->N_CE = -1;
   
   if (label) 
      D_ROI->Label = (char *)SUMA_calloc (strlen(label)+1, sizeof(char));
   else D_ROI->Label = (char *)SUMA_calloc (20, sizeof(char));
   
   if (  !D_ROI || 
         !D_ROI->idcode_str || !D_ROI->Parent_idcode_str || !D_ROI->Label) {
      fprintf (SUMA_STDERR, "Error %s: Failed allocating.\n", FuncName);
      SUMA_RETURN (NULL);
   }
      
   UNIQ_idcode_fill(D_ROI->idcode_str);   
   
   D_ROI->Parent_idcode_str = 
      strcpy (D_ROI->Parent_idcode_str, Parent_idcode_str);
   if (label) D_ROI->Label = strcpy (D_ROI->Label, label);
   else sprintf (D_ROI->Label, "auto label %d", ROI_index);
   
   D_ROI->DrawStatus = DrawStatus;
   D_ROI->Type = Type;
   
   D_ROI->ActionStack = SUMA_CreateActionStack ();
   D_ROI->StackPos = NULL;
   
   D_ROI->iLabel = ilabel;
   D_ROI->ColorByLabel = YUP;
   
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
   
   SUMA_ENTRY;
   
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
   
   SUMA_ENTRY;
   
   ROId = (SUMA_ROI_DATUM *) SUMA_calloc(1,sizeof(SUMA_ROI_DATUM));
   
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
   
   SUMA_ENTRY;
   
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
   
   SUMA_ENTRY;
   
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
SUMA_Boolean SUMA_PrependToROIdatum (
   SUMA_ROI_DATUM *ROId1, 
   SUMA_ROI_DATUM *ROId2)
{
   static char FuncName[]={"SUMA_PrependToROIdatum"};
   int i=0, N_nNew=-1, N_tNew=-1, *tPathNew=NULL, *nPathNew=NULL;
   SUMA_Boolean CommonTip = NOPE;
   
   SUMA_ENTRY;
   
   if (!ROId1) SUMA_RETURN(YUP);
   if (!ROId1->N_n)  SUMA_RETURN(YUP);
   if (!ROId2) {
      fprintf (SUMA_STDERR, "Error %s: NULL ROId2.\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   /* make sure the last node of ROId1 and the first node of ROId2 match */
   if (ROId2->N_n) {
      if (ROId1->nPath[ROId1->N_n-1] != ROId2->nPath[0]) {
         fprintf (SUMA_STDERR, 
                  "Error %s: Last node of ROId1 is not the same \n"
                  "as the first node of ROId2.\n", FuncName);
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
   
   SUMA_ENTRY;
   
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

#define SUMA_FS_DIJKSTRA_DISTANCE_FACTOR 1.20711 /* taken from pp 198, col 1 Fischl et al Neuroimage 9, 195-207 1999, Cortical Surface-Based Analysis */
void SUMA_ReportDrawnROIDatumLength(SUMA_SurfaceObject *SO, SUMA_ROI_DATUM *ROId,                                     FILE *out, 
                                    SUMA_WIDGET_INDEX_DRAWROI_WHATDIST option)
{
   static char FuncName[]={"SUMA_ReportDrawnROIDatumLength"};
   int N0, N1, i, N_n, *nPath, N_left;
   SUMA_Boolean *isNodeInMesh = NULL;
   float *p1, *p2;
   float ds = 0, d = 0, ds_c, dd, dd_c, deuc;
   char *s = NULL;
   SUMA_STRING *SS = NULL;   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ROId) SUMA_RETURNe;
   if (ROId->N_n < 2) SUMA_RETURNe;
   if (option != SW_DrawROI_WhatDistAll && option != SW_DrawROI_WhatDistTrace) {
      SUMA_SL_Err("Why do you get this here ?");
      SUMA_RETURNe;
   }
   SS = SUMA_StringAppend (NULL, NULL);

   /* calculate path distance */
   ds = 0.0;
   for (i=0; i<ROId->N_n-1; ++i) {
      p1 = &(SO->NodeList[3*ROId->nPath[i]]);
      p2 = &(SO->NodeList[3*ROId->nPath[i+1]]);
      SUMA_SEG_NORM(p1, p2, d); 
      ds = ds + d;
   }
   ds_c = ds / SUMA_FS_DIJKSTRA_DISTANCE_FACTOR;
   SUMA_SEG_LENGTH((SO->NodeList+SO->NodeDim*ROId->nPath[0]),
                   (SO->NodeList+SO->NodeDim*ROId->nPath[ROId->N_n - 1]), 
                   deuc); 
   dd = -1.0;  dd_c = -1.0;
   if (option == SW_DrawROI_WhatDistAll) { /* do shortest distance */
      isNodeInMesh = (SUMA_Boolean*) SUMA_malloc(  SO->N_Node * 
                                                   sizeof(SUMA_Boolean)); 
      N_left = SO->N_Node; for (i=0;i<N_left;++i) isNodeInMesh[i] = YUP;
      if (!isNodeInMesh) {
         SUMA_SL_Err("Failed to allocate!\n"
                     "Will not compute shortest distance.");
      }else {
         nPath = SUMA_Dijkstra ( SO, ROId->nPath[0], 
                                 ROId->nPath[ROId->N_n - 1], 
                                 isNodeInMesh, &N_left, 1, &dd, &N_n);
         if (nPath) { 
            SUMA_free(nPath); 
            nPath = NULL; 
            dd_c = dd / SUMA_FS_DIJKSTRA_DISTANCE_FACTOR;
         } else { 
            dd = -2.0;  dd_c = -2.0;
         }
         SUMA_free(isNodeInMesh); isNodeInMesh = NULL;
      }
      SS = SUMA_StringAppend_va(SS,
         "#Distances on %s\n"
         "#n0\tn1\tN_n\td\td_c\tds\tds_c\td3\n"
         "%d\t%d\t%d\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\n", 
         SO->Label, ROId->nPath[0], ROId->nPath[ROId->N_n - 1], 
         ROId->N_n, ds, ds_c, dd, dd_c, deuc);
   } else if (option == SW_DrawROI_WhatDistTrace) {
      SS = SUMA_StringAppend_va(SS,
         "#Distances on %s\n"
         "#n0\tn1\tN_n\td\td_c\td3\n"
         "%d\t%d\t%d\t%.2f\t%.2f\t%.2f\n", 
         SO->Label, ROId->nPath[0], ROId->nPath[ROId->N_n - 1], 
         ROId->N_n, ds, ds_c, deuc);
   }
   
   SUMA_SS2S(SS,s);
   if (out) fprintf(out, "%s", s);
   
   SUMA_L_Text(s);
   
   SUMA_free (s); s= NULL;
   SUMA_RETURNe;
} 
/*!
   \brief Show contents of a drawn ROI
   SUMA_ShowDrawnROI (ROI, Out, ShortVersion);
   
   \param ROId (SUMA_DRAWN_ROI *) 
   \param Out (FILE *) (stderr if NULL)
   \param ShortVersion (SUMA_Boolean) if YUP, short version

*/
void SUMA_ShowDrawnROI (SUMA_DRAWN_ROI *D_ROI, FILE *out, 
                        SUMA_Boolean ShortVersion)
{
   static char FuncName[]={"SUMA_ShowDrawnROI"};
   int i;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDERR;

   fprintf(out, "--------------------------------------------\n");
   
   if (!D_ROI) {
      fprintf(out, "%s: NULL D_ROI\n", FuncName);
      SUMA_RETURNe;
   }
   
   fprintf(out, "%s: ROI Label %s, Type %d, DrawStatus %d\n"
                " Idcode %s, Parent Idcode %s, Side %s\n", 
         FuncName, D_ROI->Label, D_ROI->Type, D_ROI->DrawStatus, 
         D_ROI->idcode_str, D_ROI->Parent_idcode_str, 
         SUMA_SideName(D_ROI->Parent_side) );
   
   if (D_ROI->ActionStack) {
      fprintf (out, "%s: There are %d actions in the ActionStack.\n", 
                     FuncName, dlist_size(D_ROI->ActionStack));
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
      fprintf(out, "%s: ROIstrokelist has %d elements.\n", 
                     FuncName, dlist_size(D_ROI->ROIstrokelist));
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
   \brief SUMA_FillToMask_Engine (FN, Visited, Mask, seed, N_Visited, N_Node);
   the engine function for SUMA_FillToMask.
   replaces the recursive version now called SUMA_FillToMask_Engine_old
*/
void SUMA_FillToMask_Engine (SUMA_NODE_FIRST_NEIGHB *FN, int *Visited, int *ROI_Mask, 
                              int nseed, int *N_Visited, int N_Node)
{  
   static char FuncName[]={"SUMA_FillToMask_Engine"};
   int i, nnext;
   int *candidate = NULL;
   int N_candidate = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   candidate = (int *)SUMA_calloc(N_Node, sizeof(int));
   if (!candidate) {
      SUMA_SL_Crit("Failed to Allocate");
      SUMA_RETURNe;
   }
   
   do {
      if (!Visited[nseed]) { Visited[nseed] = 1; ++*N_Visited; } /* add the seed, if not added yet */
      
      for (i=0; i<FN->N_Neighb[nseed]; ++i) {
         nnext = FN->FirstNeighb[nseed][i];
         /* fprintf (SUMA_STDERR,"nnext=%d\n", nnext); fflush(SUMA_STDERR); */
         if (!Visited[nnext] && !ROI_Mask[nnext]) {
            /*fprintf (SUMA_STDERR,"N_candidate=%d\n", N_candidate);*/
            candidate[N_candidate] = nnext; ++N_candidate; 
            Visited[nnext] = 1; ++*N_Visited;   /* add this candidate so you don't revisit it as a new candidate later */
         }
      }      
      if (N_candidate) { /* Need this safeguard, in case there are no candidates. 
                        Happened when fill area has one empty node in it only! 
                        Fixed in Madison 06, ZSS*/
         nseed = candidate[N_candidate-1]; --N_candidate;
      }
   } while (N_candidate);
   
   if (candidate) SUMA_free(candidate); candidate = NULL; 
   SUMA_RETURNe;
}

/*!
   \brief SUMA_FillToMask_Engine (FN, Visited, Mask, seed, N_Visited);
   the recursive function for SUMA_FillToMask.
   Do not use logging functions here.
   
   CAN cause Illegal instruction interrupts, bad for blood glucose levels.
   Need non recursive version (crashes at cnt = 70676)
*/

void SUMA_FillToMask_Engine_old (SUMA_NODE_FIRST_NEIGHB *FN, int *Visited, int *ROI_Mask, int nseed, int *N_Visited)
{  
   int i, nnext;
   /* static cnt = 0;
   ++cnt;
   fprintf (SUMA_STDERR,"    cnt = %d\n", cnt); fflush(SUMA_STDERR); */
   Visited[nseed] = 1;
   ++*N_Visited;
   for (i=0; i<FN->N_Neighb[nseed]; ++i) {
      nnext = FN->FirstNeighb[nseed][i];
      /* fprintf (SUMA_STDERR,"nnext=%d\n", nnext); fflush(SUMA_STDERR); */
      if (!Visited[nnext] && !ROI_Mask[nnext]) {
         /*fprintf (SUMA_STDERR,"In!\n"); fflush(SUMA_STDERR); */
         SUMA_FillToMask_Engine_old(FN, Visited, ROI_Mask, nnext, N_Visited);
      }
   }
   /* --cnt; */
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
   SUMA_ENTRY;
   
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

   SUMA_FillToMask_Engine (SO->FN, Visited, ROI_Mask, nseed, &N_Visited, SO->N_Node);
   
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


/*! 
   \brief function to turn a set of nodes into a DrawnROI
   
   \param Node (int *) pointer to set of nodes forming ROI
               Redundant nodes are removed. Nodes are copied
               to new location so you can free this pointer
               if you wish.
   \param N_Node (int) number of nodes in ROI
   \param Value (int) to go in ROI->iLabel
   \param Parent_idcode_str (char *) to get copied into 
         ROI->Parent_idcode_str
   \param Label (char *) to get copied into ROI->Label
   \param ColPlaneName (char *) to get copied into ROI->ColPlaneName 
   \param FillColor (float[4])
   \param EdgeColor (float[4])
   \param EdgeThickness (int)
   \param ForDisplay (SUMA_Boolean) YUP: Prepare ROI for display
                                    (creates the contour for the ROI,
                                    requires that Parent surface object be loaded)
   \return ROI (SUMA_DRAWN_ROI *)
   
   - No DO/Undos possible in this format
*/

SUMA_DRAWN_ROI * SUMA_1DROI_to_DrawnROI ( 
   int *Node, int N_Node, int Value, char *Parent_idcode_str, 
   char *Label, char *ColPlaneName, 
   float *FillColor, float *EdgeColor, int EdgeThickness, 
   SUMA_DO *dov, int N_dov, SUMA_Boolean ForDisplay)
{
   static char FuncName[]={"SUMA_1DROI_to_DrawnROI"};
   SUMA_ROI_DATUM *ROI_Datum = NULL;
   SUMA_DRAWN_ROI *ROI = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!Node) SUMA_RETURN(NULL);
   
   /* allocate and initialize */
   ROI = SUMA_AllocateDrawnROI (Parent_idcode_str, SUMA_ROI_Finished,
                               SUMA_ROI_Collection,  Label,  Value);
   
   /* add the colors */
   SUMA_COPY_VEC(EdgeColor, ROI->EdgeColor, 4, float, float);
   SUMA_COPY_VEC(FillColor, ROI->FillColor, 4, float, float);
   ROI->EdgeThickness = EdgeThickness;
   
   /* fill in the only ROI datum */
   ROI_Datum = SUMA_AllocROIDatum ();
   ROI_Datum->action = SUMA_BSA_Undefined;
   if(LocalHead) 
      fprintf (SUMA_STDERR,
               "%s: About to add %d nodes of value %d...\n", 
               FuncName, N_Node, Value);
   
   ROI_Datum->nPath = SUMA_UniqueInt(Node, N_Node, &ROI_Datum->N_n, NOPE);
   if (!ROI_Datum->nPath) {
      SUMA_SLP_Crit("Failed to allocate");
      SUMA_RETURN(NOPE);
   }
   ROI_Datum->Type = SUMA_ROI_NodeGroup;
   
   SUMA_LH("Appending stroke");
   /* just append that baby */
   dlist_ins_next(ROI->ROIstrokelist, dlist_tail(ROI->ROIstrokelist), 
                  (void *)ROI_Datum);
   
   if (ForDisplay) {
      /* You must find the contour by yourself. This is normally
      done when the status is set to SUMA_ROI_Finished via the 
      action stack functions */
      { 
         int *cNodes, N_cNodes, i=0;
         SUMA_Boolean Unique = NOPE;
         SUMA_SurfaceObject *SO=NULL;
         SUMA_LH("Getting Contour ");
         N_cNodes = 0;
         Unique = NOPE; 
            /* Set to YUP if you have node indices listed more than once. 
                           1D ROIs are uniquized in the reading functions*/
         cNodes = SUMA_NodesInROI (ROI, &N_cNodes, Unique);
         if (cNodes) {
            if (!(SO = SUMA_findSOp_inDOv(ROI->Parent_idcode_str, dov, N_dov))) {
               SUMA_SLP_Err("No surface found");
               SUMA_RETURN(NOPE);
            }
            for (i=0; i<N_cNodes; ++i) {
               if (cNodes[i] < 0 || cNodes[i] >= SO->N_Node) {
                  SUMA_SLP_Err("Nodes in ROI negative, or >= SO->N_Node");
                  SUMA_RETURN(NOPE);
               }
            }
            ROI->CE = SUMA_GetContour (
                        SO, 
                        cNodes, N_cNodes, &(ROI->N_CE), 
                        0, NULL, NULL, 1);
            if (!ROI->CE) { SUMA_LH("Null DrawnROI->CE"); }
            else { SUMA_LH("Good DrawnROI->CE"); }
            SUMA_free(cNodes);
         }
      }
   }
   SUMA_RETURN(ROI);
}

/* ------------------------------------------------------------
   A set of functions to keep track of rendering state changes.
   A function to keep track of OpenGL's rendering state changes. 
   The idea is to be able to leave things as they were at a 
   particular point, no matter what functions do in the meanwhile.
   ------------------------------------------------------------ */
/*!

   SUMA_GLStateTrack(char *action, DList **stu, char *progenitor,
                     char *state, void *val);
   
   action (char *): "n" for "new" return a new empty list of states.
                          Use "new" when you want to set a 
                          starting point for recording the state changes
                    "s" for "set" set a particular state:
                          if state has not been set yet, 
                              create an entry for it
                              record its current value
                          set state value per user's request
                    "f" for "force" reset the initial value of a state
                           if state has not been set yet, 
                              set state value per user's request
                           else modifiy initial state value to user's request
                    "r" for "revert" undo all the state settings in the  list
                             delete list and return NULL
                    "k" for "kill" delete list without reverting to initial
                                   states      
   stu (DList **): Linked list keeping track of states that get modified
                  *stu should be NULL when calling with action "new"
                  *stu is set to NULL after actions "r" or "k"
   progenitor (char *): Who is issuing these actions? Just for debugging.
   state (char *): Name of state to be acted on. Examples include:
                  "GL_LIGHTING", "GL_POLYGON_STIPPLE", etc.
   val (void *): The value for the state. In most cases this is just
                 0 or 1, for disable or enable, respectively. 
                 
   SEE ALSO: SUMA_RecordEnablingState() and SUMA_RestoreEnablingState()
*/
int SUMA_GLStateTrack(char *action, DList **stu, char *progenitor,
                      char *state, void *val)
{
   static char FuncName[]={"SUMA_GLStateTrack"};
   DList *st=NULL;
   DListElmt *el=NULL;
   SUMA_GL_STEL *stel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!action || !stu) {
      SUMA_S_Err("Bad action, or bad stu");
      SUMA_RETURN(0);
   }
   
   st = *stu;
   
   SUMA_LHv("Action %s, st=%p, progenitor %s, state %s, val %p\n", 
            action, st, CHECK_NULL_STR(progenitor), CHECK_NULL_STR(state), val);
   /* A new tracking list */
   if (action[0] == 'n') {  
      if (st != NULL) {
         SUMA_S_Err("When calling with new, need to have *stu = NULL");
         SUMA_RETURN(0);
      }
      st = (DList *)SUMA_calloc(1, sizeof(DList));
      *stu = st;
      dlist_init(st, SUMA_FreeStateTrackEl);
      SUMA_RETURN(1);
   }
   
   /* A dump of the tracking list */
   if (action[0] == 'd') {/* short for !strcmp(action, "dump") */
      if (st == NULL) {
         fprintf(SUMA_STDERR, "NULL tracking list\n");
         SUMA_RETURN(0);
      }
      fprintf(SUMA_STDERR, "%d element in tracking list\n", dlist_size(st));
      do {
         if (!el) el = dlist_head(st);
         else el = el->next;      
         stel = (SUMA_GL_STEL *)el->data;
         fprintf(SUMA_STDERR, "%s now %s, initial %s, by %s\n", 
                  stel->state_s, stel->now_s, stel->init_s, stel->whodunit);
      } while (el);
      SUMA_RETURN(1);
   }
   
   /* Set, or force set a state */
   if (action[0] == 's' || action[0] == 'f') { /* set or force */
      if (st == NULL) { /* no tracking, just do it */
         SUMA_LH("No tracking");
         stel = SUMA_NewStateTrackEl(state, progenitor);
         if (!SUMA_SetTrackElVal(stel, val, "apply")) {
            SUMA_S_Err("Failed setting state val");
            SUMA_free(stel); stel=NULL;
            SUMA_RETURN(0);
         }
         SUMA_free(stel); stel=NULL;
         SUMA_RETURN(1);
      }
      if (!(stel = SUMA_FindStateTrackEl(state, st))) { /* a new one */
         SUMA_LH("New stel");
         stel = SUMA_NewStateTrackEl(state, progenitor);
         if (!SUMA_SetTrackElVal(stel, val, "save")) {
            SUMA_S_Err("Failed setting state val");
            SUMA_free(stel); stel=NULL;
            SUMA_RETURN(0);
         }
         /* add to list */
         dlist_ins_next(st, dlist_tail(st), (void *)stel);
      } else { /* an existing one */
         SUMA_LH("Reuse stel");
         if (!SUMA_SetTrackElVal(stel, val, 
                                 (action[0] == 'f') ? "save":"apply")) {
            SUMA_S_Err("Failed updating state val");
            SUMA_free(stel); stel=NULL;
            SUMA_RETURN(0);
         }
      }
      SUMA_RETURN(1);
   }
   
   /* revert all to initial state */
   if (action[0] == 'r' || action[0] == 'k') { /* revert or kill */
      if (st == NULL) { 
         fprintf(SUMA_STDERR, "NULL tracking list, cannot revert\n");
         SUMA_RETURN(0);
      }
      if (dlist_size(st)) {
         do {
            if (!el) el = dlist_head(st);
            else el = el->next;      
            if (el) {
               stel = (SUMA_GL_STEL *)el->data;
               if (action[0] == 'r') { /* revert */
                  SUMA_LHv("reverting %s\n", stel->state_s);
                  SUMA_SetTrackElVal(stel, NULL, "revert");
                  SUMA_LHv("  reverted%s\n", stel->state_s);
               }
            }
         } while (el && (el != dlist_tail(st)));
      }
      /* destroy list */
      SUMA_LH("Shiva time");
      dlist_destroy(st); SUMA_free(st); st = NULL;
      *stu = NULL;
      SUMA_RETURN(1);
   }
   
   SUMA_S_Err("Had nothing to do");
   SUMA_RETURN(0);
}

/* Only put states that are controlled by glEnable/glDisable here */
int SUMA_GLstateToEnum(char *state) {
   if (!state) return(-1);
   if (!strcmp(state,"GL_LIGHTING")) return(GL_LIGHTING);
   if (!strcmp(state,"GL_POLYGON_STIPPLE")) return(GL_POLYGON_STIPPLE);
   if (!strcmp(state,"GL_DEPTH_TEST")) return(GL_DEPTH_TEST);
   if (!strcmp(state,"GL_POLYGON_OFFSET_FILL")) return(GL_POLYGON_OFFSET_FILL);
   return(-2);
}

/* Only put states that are controlled by glEnable/glDisable here */
char *SUMA_EnumToGLstate(int glpar) {
   if (glpar==-1) return(NULL);
   if (glpar==GL_LIGHTING) return("GL_LIGHTING");
   if (glpar==GL_POLYGON_STIPPLE) return("GL_POLYGON_STIPPLE");
   if (glpar==GL_DEPTH_TEST) return("GL_DEPTH_TEST");
   if (glpar==GL_POLYGON_OFFSET_FILL) return("GL_POLYGON_OFFSET_FILL");
   return("unknown");
}

/*!
   Set gl state and keep tracking.
   if act[0] == "r" : Revert to initial setting. val is not needed.
      act[0] == "s" : Save initial setting and apply val.
      act[0] == "i" : Note initial setting, and apply nothing.
      act[0] == "a"  : Don't touch initial setting, just apply setting
                        in val
*/
int SUMA_SetTrackElVal(SUMA_GL_STEL *stel, void *val, char *act) {
   static char FuncName[]={"SUMA_SetTrackElVal"};
   GLenum glpar;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!stel || !act) {
      SUMA_S_Err("Nothing to do");
      SUMA_RETURN(0);
   }
   
   if (act[0] != 'r' && act[0] != 's' && act[0] != 'a' && act[0] != 'i') {
      SUMA_S_Errv("Bad act==%s\n", act)
      SUMA_RETURN(0);
   }
   
   SUMA_LHv("---->Have %s, init_s %s, now_s %s, act %s, val %p\n",
            stel->state_s, stel->init_s, stel->now_s, act, val);
            
   if ((glpar = (GLenum)SUMA_GLstateToEnum(stel->state_s)) >= 0) { 
      if (act[0] != 'r') {
         if (act[0] == 's' || act[0] == 'i') {
            stel->init_i = (int)glIsEnabled(glpar);
            sprintf(stel->init_s,"%d", stel->init_i);
            if (act[0] == 'i') {
               stel->now_i = stel->init_i;
               sprintf(stel->now_s,"%d", stel->now_i);
            }
         } 
         if (act[0] == 'a' || act[0] == 's') {
            {  /* Note, than when passing a '0' for disable, val == NULL,
                  so you can't object to a NULL val, or use it as a special
                  flag */
               stel->now_i = (int)(val?1:0);
               sprintf(stel->now_s,"%d", stel->now_i);
            }
         } 
         /* apply */
         if (act[0] != 'i') {
            if (stel->now_i) glEnable(glpar);
            else glDisable(glpar);
         }
      } else { /* revert to initial setting */
         if (stel->now_i != stel->init_i) {
            if (stel->init_i) glEnable(glpar);
            else glDisable(glpar);
            stel->now_i = stel->init_i;
         }
      }
   } else if (!strcmp(stel->state_s,"glPolygonOffset")) {
      if (act[0] != 'r') {
         if (act[0] == 's' || act[0] == 'i') {
            glGetFloatv(GL_POLYGON_OFFSET_FACTOR, stel->init_f4);
            glGetFloatv(GL_POLYGON_OFFSET_UNITS, stel->init_f4+1);
            sprintf(stel->init_s,"%f, %f", stel->init_f4[0], stel->init_f4[1]);
            if (act[0] == 'i') {
               stel->now_f4[0] = stel->init_f4[0]; 
               stel->now_f4[1] = stel->init_f4[1];
               sprintf(stel->now_s,"%f, %f", 
                           stel->now_f4[0], stel->now_f4[1]); 
            }
         } 
         if (act[0] == 'a' || act[0] == 's') {
            if (!val) {
               SUMA_S_Warn("Nothing to do here");
               SUMA_RETURN(0);
            } else {
               float *fv=(float*)val;
               stel->now_f4[0] = fv[0];
               stel->now_f4[1] = fv[1];
               sprintf(stel->now_s,"%f, %f", 
                           stel->now_f4[0], stel->now_f4[1]); 
            }
         }
         /* apply */
         if (act[0] != 'i') {
            glPolygonOffset(stel->now_f4[0], stel->now_f4[1]);
         }
      } else { /* revert to initial setting */
         if (stel->now_f4[0] != stel->init_f4[0] ||
             stel->now_f4[1] != stel->init_f4[1]) {
            glPolygonOffset(stel->init_f4[0], stel->init_f4[1]);
            stel->now_f4[0] = stel->init_f4[0];
            stel->now_f4[1] = stel->init_f4[1];
         }
      }
   } else if (!strcmp(stel->state_s,"glPolygonMode")) {
      if (act[0] != 'r') {
         if (act[0] == 's' || act[0] == 'i') {
               /* This is a little problematic. While I can set
               polygon mode separately for FRONT and BACK or triangles,
               I can only query state common to front and and back ... */
            stel->init_f4[0] = (float)GL_FRONT_AND_BACK; /* no choice */ 
            glGetFloatv(GL_POLYGON_MODE, stel->init_f4+1);
            sprintf(stel->init_s,"%d, %d", 
                  (int)stel->init_f4[0], (int)stel->init_f4[1]);
            if (act[0] == 'i') {
               stel->now_f4[0] = stel->init_f4[0]; 
               stel->now_f4[1] = stel->init_f4[1];
               sprintf(stel->now_s,"%d, %d", 
                           (int)stel->now_f4[0], (int)stel->now_f4[1]); 
            }
         }
         if (act[0] == 'a' || act[0] == 's') {
            if (!val) {
               SUMA_S_Warn("Nothing to do here");
               SUMA_RETURN(0);
            } else {
               float *fv=(float*)val;
               stel->now_f4[0] = fv[0];
               stel->now_f4[1] = fv[1];
               sprintf(stel->now_s,"%d, %d", 
                           (int)stel->now_f4[0], (int)stel->now_f4[1]);  
            }
         } 
         /* apply */
         if (act[0] != 'i') {
            glPolygonOffset((GLenum)stel->now_f4[0], (GLenum)stel->now_f4[1]);
         }
      } else { /* revert to initial setting */
         if (stel->now_f4[0] != stel->init_f4[0] ||
             stel->now_f4[1] != stel->init_f4[1]) {
            glPolygonOffset((GLenum)stel->init_f4[0], (GLenum)stel->init_f4[1]);
            stel->now_f4[0] = stel->init_f4[0];
            stel->now_f4[1] = stel->init_f4[1];
         }
      }
   } else {
      SUMA_S_Errv("Not ready to set anything for %s\n", stel->init_s);
      SUMA_RETURN(0);
   }
   
   SUMA_LHv("---->On output %s, init_s %s, now_s %s, act %s, val %p\n",
            stel->state_s, stel->init_s, stel->now_s, act, val);
   SUMA_RETURN(1);
}

SUMA_GL_STEL *SUMA_NewStateTrackEl(char *state, char *progenitor) {
   static char FuncName[]={"SUMA_NewStateTrackEl"};
   SUMA_GL_STEL *stel=NULL;
   
   SUMA_ENTRY;
   
   if (!state) {
      SUMA_S_Err("Nothing to do");
      SUMA_RETURN(stel);
   }
   
   stel = (SUMA_GL_STEL *)SUMA_calloc(1, sizeof(SUMA_GL_STEL));
   strcpy(stel->state_s,state);
   
   if (!progenitor) progenitor = "unknown";
   strcpy(stel->whodunit,progenitor);
   
   
   SUMA_RETURN(stel);
}

void SUMA_FreeStateTrackEl(void *stel) {
   if (stel) {
      SUMA_free(stel);
   }
   return;
}
SUMA_GL_STEL *SUMA_FindStateTrackEl(char *state, DList *st) {
   static char FuncName[]={"SUMA_FindStateTrackEl"};
   DListElmt *el=NULL;
   SUMA_GL_STEL *stel=NULL;
   
   SUMA_ENTRY;
   if (!state || !st) SUMA_RETURN(NULL);
   if (!dlist_size(st)) SUMA_RETURN(NULL);
   do {
      if (!el) el = dlist_head(st);
      else el = el->next;      
      if (el) {
         stel = (SUMA_GL_STEL *)el->data;
         if (!strcmp(stel->state_s, state)) SUMA_RETURN(stel);
      }
   } while (el && (el != dlist_tail(st)));
   
   SUMA_RETURN(NULL);
}

/* ------------------------------------------------------------ */

/* Stippling masks for transparency */
static int stippleMask_shft[17] = { 0, 0, 0, 0, 0, 
                                    0, 0, 0, 0, 0, 
                                    0, 0, 0, 0, 0, 
                                    0, 0 };
static GLubyte stippleMask[17][128] =
{
  /* NOTE: 0% opaqueness is faster to set and probably faster to render with:
	glDisable(GL_POLYGON_STIPPLE);
	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE); */
  {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

  {0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

  {0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55},

  {0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55},

  {0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55},

  {0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55},

  {0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55},

  {0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55},

  {0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77},

  {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77},

  /* NOTE: 100% opaqueness is faster to set and probably faster to render with:
        glDisable(GL_POLYGON_STIPPLE); */
  {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff},
};

const GLubyte stippleMask_init[17][128] =
{
  /* NOTE: 0% opaqueness is faster to set and probably faster to render with:
	glDisable(GL_POLYGON_STIPPLE);
	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE); */
  {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

  {0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

  {0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0x88, 0x88, 0x88, 0x88, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x00, 0x00, 0x00, 0x00},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x44, 0x44, 0x44, 0x44, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x11, 0x11, 0x11, 0x11},

  {0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55},

  {0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0xaa, 0x55, 0x55, 0x55, 0x55},

  {0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xee, 0xee, 0xee, 0xee, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55},

  {0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xbb, 0xbb, 0xbb, 0xbb, 0x55, 0x55, 0x55, 0x55},

  {0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55},

  {0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x55, 0x55, 0x55, 0x55},

  {0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77},

  {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x77, 0x77, 0x77, 0x77},

  /* NOTE: 100% opaqueness is faster to set and probably faster to render with:
        glDisable(GL_POLYGON_STIPPLE); */
  {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff},
};

void SUMA_StippleMaskResest(void) {
   int n;
   for (n=0; n<17; ++n) {
      if (stippleMask_shft[n]) {
         memcpy(stippleMask[n], stippleMask_init[n], 128*sizeof(GLubyte));
         stippleMask_shft[n] = 0;
      }
   }
   return;
}

GLubyte * SUMA_StippleMaskShift(GLubyte *mm) {
   static GLubyte bt[1024], b0;
   int n, nbits, n8, k;
   
   /* Turn stipple mask to pixel mask of 0s and 1s */
   for (n=0; n<128; ++n) {
      n8 = 8*n;
      bt[n8+0] =  ( (mm[n] & ( 1 << 0 )) >> 0 ) ; 
      bt[n8+1] =  ( (mm[n] & ( 1 << 1 )) >> 1 ) ; 
      bt[n8+2] =  ( (mm[n] & ( 1 << 2 )) >> 2 ) ; 
      bt[n8+3] =  ( (mm[n] & ( 1 << 3 )) >> 3 ) ; 
      bt[n8+4] =  ( (mm[n] & ( 1 << 4 )) >> 4 ) ; 
      bt[n8+5] =  ( (mm[n] & ( 1 << 5 )) >> 5 ) ; 
      bt[n8+6] =  ( (mm[n] & ( 1 << 6 )) >> 6 ) ; 
      bt[n8+7] =  ( (mm[n] & ( 1 << 7 )) >> 7 ) ; 
   }
   /* offset each row of the 32x32 pixel mask */
   for (n=0; n<32; ++n) {
      b0 = bt[n*32];
      for (k=0; k<31; ++k) {
         n8 = k+n*32;
         bt[n8] = bt[n8+1];
      }
      bt[31+n*32] = b0;
   }

   /* Now recreate stipple mask  */
   for (n=0; n<128; ++n) {
      n8 = 8*n;
      mm[n] =  ( bt[n8  ] << 0 ) | ( bt[n8+1] << 1 ) | ( bt[n8+2] << 2 )  |
               ( bt[n8+3] << 3 ) | ( bt[n8+4] << 4 ) | ( bt[n8+5] << 5 )  |
               ( bt[n8+6] << 6 ) | ( bt[n8+7] << 7 ) ;
   }
      
   return(mm);
}

const GLubyte *SUMA_StippleMask(int transp) {
   if (transp < STM_0 || transp > STM_16) {
      fprintf(stderr,"Error SUMA_StippleMask: Bad transp %d\n", transp); 
      transp = STM_0;
   }
   transp = transp - STM_0;
   return((const GLubyte *)stippleMask[16-transp]);
}

const GLubyte *SUMA_StippleMask_shift(int transp) {
   const GLubyte *mm=NULL;
   if (transp < STM_0 || transp > STM_16) {
      fprintf(stderr,"Error SUMA_StippleMask: Bad transp %d\n", transp); 
      transp = STM_0;
   }
   transp = transp - STM_0;
   
   mm = (const GLubyte *)SUMA_StippleMaskShift(stippleMask[16-transp]);
   ++stippleMask_shft[16-transp];
   return(mm);
}

const GLubyte *SUMA_StippleMask_rand(int transp) {
   GLubyte bt[1024];
   static GLubyte sm[128];
   static int seed = 0;
   int n, nbits, *ir=NULL, n8;
    
   if (transp < STM_0 || transp > STM_16) {
      fprintf(stderr,"Error SUMA_StippleMask: Bad transp %d\n", transp); 
      transp = STM_0;
   }
   
   transp = transp - STM_0;
   nbits = (int)((float)transp/16.0*1024);
   ir = z_rand_order(0, 1023, seed++);
   if (transp <= 8) {
      memset(bt, 1, sizeof(GLubyte)*1024);
      for (n=0; n<nbits; ++n) bt[ir[n]]=0;
   } else {
      memset(bt, 0, sizeof(GLubyte)*1024);
      for (n=0; n<1024-nbits; ++n) bt[ir[n]]=1;
   }
   SUMA_free(ir); ir=NULL;
   /* turn to byte stipple mask */
   for (n=0; n<128; ++n) {
      n8 = 8*n;
      sm[n] =  ( bt[n8  ] << 0 ) | ( bt[n8+1] << 1 ) | ( bt[n8+2] << 2 )  |
               ( bt[n8+3] << 3 ) | ( bt[n8+4] << 4 ) | ( bt[n8+5] << 5 )  |
               ( bt[n8+6] << 6 ) | ( bt[n8+7] << 7 ) ;
   }
   return((const GLubyte *)sm);
}

/* Chunky stippling pattern 
Valid stip values are from 0 (no holes) to 16 (nothing shown) inclusive */
GLushort SUMA_StippleLineMask_rand(int stip, int chunk_width, int rseed) {
   GLubyte bt[16];
   static GLushort sm;
   static int seed = 0;
   int n, *ir=NULL, maxchunks, nchunks, maxlevel=16, 
       j0, j1, transp;
   
   if (chunk_width < 1 || chunk_width > 16) {
      chunk_width = 2;
   } 
   
   if (!(maxchunks = SUMA_ROUND(16/chunk_width))) {
      maxchunks = 1;
   }
   
   if (stip < 0  || 
       stip > 16) {
      fprintf(stderr,"Error SUMA_StippleMask_rand: Bad stip %d\n", stip); 
      stip = 8;
   }
   
   if (stip == 16) return(0); /* nothing will be shown */
   else if (stip == 0) { /* no stippling */
      for (n=0; n<16; ++n)  sm = sm | 1 << n ;
      return(sm);
   }
   
   /* I'd rather think transparency */
   transp = 16-stip;
   if (!(nchunks = (int)((float)transp/maxlevel*maxchunks))) {
      nchunks = 1;
   }
   
   if (rseed) {   
      ir = z_rand_order(0, maxchunks-1, seed++);
   } else {
      ir = z_rand_order(0, maxchunks-1, 1111);
   }
   memset(bt, 0, sizeof(GLubyte)*16);
   for (n=0; n<nchunks; ++n) {
      j0 = ir[n]*chunk_width; j1 = SUMA_MIN_PAIR(j0+chunk_width, 16);
      while(j0 < j1) {
         bt[j0]=1; ++j0;
      }
   }
      #if 0
   fprintf(stderr,"%d chunks (%d wide) @chunk indices[", nchunks, chunk_width);
   for (n=0; n<nchunks; ++n) { fprintf(stderr,"%d ", ir[n]); }
   fprintf(stderr,"]\n");
   for (n=0; n<16; ++n) {
      fprintf(stderr,"%d",bt[n]);
   }
   fprintf(stderr,"\n");
      #endif
   SUMA_free(ir); ir=NULL;
   /* turn to byte stipple mask */
   sm = 0;
   for (n=0; n<16; ++n) {
      sm = sm | bt[n] << n ;
   }
   return(sm);
}

