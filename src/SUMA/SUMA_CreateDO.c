/*! Functions to create Displayable Objects */
#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;
extern int SUMAg_N_DOv;

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
   NodeListp (float **) pointer to nodelist. This points to the vector of node coordinates.
                        The copy into SO is done by pointer and *NodeListp is set to NULL
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
SUMA_SurfaceObject *SUMA_Cmap_To_SO (SUMA_COLOR_MAP *Cmap, float orig[3], float topright[3], int verb)
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
   if (!Cmap->N_Col) { SUMA_SL_Err("No colours"); SUMA_RETURN(NULL); }
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
   dh = (topright[1] - orig[1]) / Cmap->N_Col;
   hp = (float *)SUMA_calloc(Cmap->N_Col+1, sizeof(float));
   if (!hp) {
      SUMA_SL_Crit("malloc error");
      SUMA_RETURN(NULL);
   }
   hp[0] = 0.0;
   for (i=0; i<Cmap->N_Col; ++i) {
      if (Cmap->frac) {
         if (LocalHead) fprintf (SUMA_STDERR, "%s: icol %d, frac=%f\n", FuncName, i,  Cmap->frac[i]);
         if (Cmap->Sgn >= 0) {
            hp[i+1] = Cmap->frac[i]/Cmap->frac[Cmap->N_Col-1] * (topright[1] - orig[1]);
         } else {
            hp[i+1] = (1.0 +Cmap->frac[i]/Cmap->frac[Cmap->N_Col-1]) / 2.0 * (topright[1] - orig[1]);
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
   SO->N_Node = 4 * (Cmap->N_Col);
   SO->N_FaceSet = 2 * Cmap->N_Col;
   SO->NodeDim = 3;
   SO->EmbedDim = 2;
   SO->FaceSetDim = 3;
   SO->idcode_str = (char *)SUMA_malloc(SUMA_IDCODE_LENGTH * sizeof(char));
   SO->NodeList = (float *)SUMA_malloc(SO->N_Node * 3*sizeof(float));
   SO->FaceSetList = (int *)SUMA_malloc(SO->N_FaceSet * 3 * sizeof(int));
   SO->PermCol = (GLfloat *)SUMA_malloc(SO->N_Node * 4*sizeof(GLfloat));
   if (!SO->idcode_str || !SO->NodeList || !SO->FaceSetList || !SO->PermCol) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL);}
   
   SUMA_LH("Filling up surface id.");
   /* fill up idcode*/
   UNIQ_idcode_fill(SO->idcode_str);
   
   SUMA_LH("Filling up surface nodelist.");
   /* fill up coordinates first */
   i=0;     /* color index */
   in = 0;  /* node index */
   i3 = in * 3;
   while (i < Cmap->N_Col) {
      /* 4 nodes per color */
      SO->NodeList[i3] =    orig[0]; SO->NodeList[i3+1] = hp[i]   + orig[1]; SO->NodeList[i3+2] = 0.0; ++in; i3 = in * 3;
      SO->NodeList[i3] = dw+orig[0]; SO->NodeList[i3+1] = hp[i]   + orig[1]; SO->NodeList[i3+2] = 0.0; ++in; i3 = in * 3;
      SO->NodeList[i3] = dw+orig[0]; SO->NodeList[i3+1] = hp[i+1] + orig[1]; SO->NodeList[i3+2] = 0.0; ++in; i3 = in * 3;
      SO->NodeList[i3] =    orig[0]; SO->NodeList[i3+1] = hp[i+1] + orig[1]; SO->NodeList[i3+2] = 0.0; ++in; i3 = in * 3;
      ++i;
   }
   
   SUMA_LH("Bounding Box");
   /* Calculate Min, Max, Mean */
   SUMA_MIN_MAX_SUM_VECMAT_COL (SO->NodeList, SO->N_Node, SO->NodeDim, SO->MinDims, SO->MaxDims, SO->Center);
     
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
   while (i < Cmap->N_Col) {
      /* 2 triangles per color, 4 nodes per color*/
      SO->FaceSetList[i3] = i4; SO->FaceSetList[i3+1] = i4+1; SO->FaceSetList[i3+2] = i4+2; ++in; i3 = in *3;
      SO->FaceSetList[i3] = i4; SO->FaceSetList[i3+1] = i4+2; SO->FaceSetList[i3+2] = i4+3; ++in; i3 = in *3;; 
      ++i; i4 = 4*i;
   }
   
   SUMA_LH("Filling up surface colors.");
   /* fill up the color vector */
   i=0; /* color index */
   in = 0;  /* node index */
   i4 = in * 4;
   while (i < Cmap->N_Col) {
      for (k=0; k<4; ++k) {
         /* 4 nodes per color */
         SO->PermCol[i4]   = Cmap->M[i][0]; SO->PermCol[i4+1] = Cmap->M[i][1]; 
         SO->PermCol[i4+2] = Cmap->M[i][2]; SO->PermCol[i4+3] = 1.0;  ++in; i4 = in * 4;
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
   
   /* free hp */
   if (hp) SUMA_free(hp); hp = NULL;
   
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
   Create a surface object for the colormap 
   This one creates a surface where successive colors share an edge.
   This makes the coloring annoying since you need to use flat shading 
   and make sure you colorize the proper nodes (see p172, OpenGL programming guide and
   NIH-3 lab-book 157. The coloring as implemented here is not correct in that 
   it does not follow the i+2 rule (see OpenGL book ref) but I have decided to
   create a different surface which has 4 nodes per color and no sharing of 
   nodes/edges for successive colors.
   
   See SUMA_Cmap_To_SO
*/
SUMA_SurfaceObject *SUMA_Cmap_To_SO_old (SUMA_COLOR_MAP *Cmap, float orig[3], float topright[3], int verb)
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
   if (!Cmap->N_Col) { SUMA_SL_Err("No colours"); SUMA_RETURN(NULL); }
   if (!SO) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL); }
   
   /* scaling factors */
   dh = (topright[1] - orig[1]) / Cmap->N_Col;
   dw = topright[0] - orig[0];
   if (dh <= 0 || dw <= 0) {
      SUMA_SL_Err("Whatchyoutalkinaboutwillis?");
      SUMA_RETURN(NULL);
   }
   
   SUMA_LH("Allocating surface elements.");
   /* allocate for the NodeList */
   SO->FileType = SUMA_CMAP_SO;
   SO->N_Node = 2 * (Cmap->N_Col + 1);
   SO->N_FaceSet = 2 * Cmap->N_Col;
   SO->NodeDim = 3;
   SO->EmbedDim = 2;
   SO->FaceSetDim = 3;
   SO->idcode_str = (char *)SUMA_malloc(SUMA_IDCODE_LENGTH * sizeof(char));
   SO->NodeList = (float *)SUMA_malloc(SO->N_Node * 3*sizeof(float));
   SO->FaceSetList = (int *)SUMA_malloc(SO->N_FaceSet * 3 * sizeof(int));
   SO->PermCol = (GLfloat *)SUMA_malloc(SO->N_Node * 4*sizeof(GLfloat));
   if (!SO->idcode_str || !SO->NodeList || !SO->FaceSetList || !SO->PermCol) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL);}
   
   SUMA_LH("Filling up surface id.");
   /* fill up idcode*/
   UNIQ_idcode_fill(SO->idcode_str);
   
   SUMA_LH("Filling up surface nodelist.");
   /* fill up coordinates first */
   /* first fill the oth and the 1st node. They don't follow the cute series */
   SO->NodeList[0] = orig[0];    SO->NodeList[1] = orig[1];    SO->NodeList[2] = 0.0;
   SO->NodeList[3] = dw+orig[0]; SO->NodeList[4] = orig[1];    SO->NodeList[5] = 0.0;
   
   i=2;
   while (i < SO->N_Node) {
      /* even i */
      i3 = i * 3;
      SO->NodeList[i3] = dw+orig[0];SO->NodeList[i3+1] = i/2 * dh + orig[1]; SO->NodeList[i3+2] = 0.0; ++i;
      /* odd i */ 
      i3 = i * 3;
      SO->NodeList[i3] = orig[0];   SO->NodeList[i3+1] = i/2 * dh + orig[1]; SO->NodeList[i3+2] = 0.0; ++i;
   }
   
   SUMA_LH("Filling up surface facesetlist.");
   /* fill up triangles */
   /* fill up the oth and 1st triangles, they don't follow the cute series */
   SO->FaceSetList[0] = 0; SO->FaceSetList[1] = 1; SO->FaceSetList[2] = 2;
   SO->FaceSetList[3] = 2; SO->FaceSetList[4] = 3; SO->FaceSetList[5] = 0;
   
   icol = 1;
   while (icol < Cmap->N_Col) {
      i = 2 * icol;    /* 2 triangles per color */
      /* first triangle */
      i3 = i * 3;
      SO->FaceSetList[i3] = i+1; SO->FaceSetList[i3+1] = i; SO->FaceSetList[i3+2] = i+2;
      /* second triangle */
      i3 = (i+1) * 3;
      SO->FaceSetList[i3] = i+2; SO->FaceSetList[i3+1] = i+3; SO->FaceSetList[i3+2] = i+1; 
      ++icol;
   }
   
   SUMA_LH("Filling up surface colors.");
   /* fill up the color vector */
   /* Node 0 is special */
   SO->PermCol[0] = Cmap->M[0][0]; SO->PermCol[1] = Cmap->M[0][1]; SO->PermCol[2] = Cmap->M[0][2]; SO->PermCol[3] = 1.0;
   /* last node is special */
   i4 = 4 * (SO->N_Node -1);
   SO->PermCol[i4  ] = Cmap->M[Cmap->N_Col-1][0]; 
   SO->PermCol[i4+1] = Cmap->M[Cmap->N_Col-1][1]; 
   SO->PermCol[i4+2] = Cmap->M[Cmap->N_Col-1][2]; 
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
   SUMA_DO_Types dotp = no_type;
   FILE *fid=NULL;
   char sbuf[2000];
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!s) {
      SUMA_SL_Warn("Query with null file name");
      SUMA_RETURN(dotp);
   }
   
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
   } else if (strstr(sbuf,"nido_head")) {
      dotp = NIDO_type;
   } 
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: Searched header string:\n>>>%s<<<\ndotp = %d\n", 
               FuncName, sbuf, dotp);
   }
   
   fclose(fid); fid = NULL;
   
   SUMA_RETURN(dotp);
}

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
   int i;
   
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
      if N_n > 0  SDO->n0 and n1 (GLfloat *) vector to 3*N_n elements to contain the XYZ of nodes n0 and n1.
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
                                       SUMA_DO_Types type)
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
   SDO->do_type = type;
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
      } else {
         if (NodeBased == 1) {
            SDO->NodeBased = 1;
            SDO->n0 = NULL;
            SDO->n1 = (GLfloat *) SUMA_calloc (3*N_n, sizeof(GLfloat));
            SDO->Parent_idcode_str = SUMA_copy_string(Parent_idcode_str);
            SDO->NodeID = (int*) SUMA_calloc(N_n, sizeof(int));
            SDO->NodeID1 = NULL;
         } else if (NodeBased == 2) {
            SDO->NodeBased = 2;
            SDO->n0 = NULL;
            SDO->n1 = NULL;
            SDO->Parent_idcode_str = SUMA_copy_string(Parent_idcode_str);
            SDO->NodeID = (int*) SUMA_calloc(N_n, sizeof(int));
            SDO->NodeID1 = (int*) SUMA_calloc(N_n, sizeof(int));
         } 
      }
   
      if (  (!SDO->NodeBased && !(SDO->n0 && SDO->n1)) || 
            (SDO->NodeBased == 1 && !(SDO->NodeID && SDO->n1)) || 
            (SDO->NodeBased == 2 && !(SDO->NodeID && SDO->NodeID1)) ) {
         fprintf(stderr,
                  "Error %s: Failed to allocate for SDO-n1 or SDO->n0\n", 
                  FuncName);
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
   
   SDO->topobj = NULL;
   if (oriented) {
      SDO->botobj = gluNewQuadric();
   } else SDO->botobj = NULL;
   
   SUMA_RETURN (SDO);
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
   if (SDO->colv) SUMA_free(SDO->colv);
   if (SDO->botobj) gluDeleteQuadric(SDO->botobj);
   if (SDO->topobj) gluDeleteQuadric(SDO->topobj);
   if (SDO) SUMA_free(SDO);
   
   SUMA_RETURNe;
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
   if (SUMA_GuessFormatFromExtension(fname, NULL)==SUMA_NIML) {
      niname = SUMA_append_string("file:", fname);
      ns = NI_stream_open(niname, "r");
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
               nido = SUMA_Alloc_NIDO(NULL, fname, parent_so_id);
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
   } else  {
      SUMA_S_Err("Only .niml.do format accepted");
      SUMA_RETURN(NULL);
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
   SDO = SUMA_Alloc_SegmentDO (ncol, s, oriented, parent_SO_id, 1, dotp);
   if (!SDO) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Allocate_SegmentDO.\n", FuncName);
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

SUMA_SegmentDO * SUMA_ReadSegDO (char *s, int oriented, char *parent_SO_id)
{
   static char FuncName[]={"SUMA_ReadSegDO"};
   SUMA_SegmentDO *SDO = NULL;
   MRI_IMAGE * im = NULL;
   float *far=NULL;
   int itmp, itmp2, icol_thick = -1, icol_col=-1;
   int nrow=-1, ncol=-1;
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
      default:
         SUMA_SLP_Err("File must have\n"
                   "6,7,10 or 11 columns.");
         mri_free(im); im = NULL;   /* done with that baby */
         SUMA_RETURN(NULL);
   }

   /* allocate for segments DO */
   SDO = SUMA_Alloc_SegmentDO (ncol, s, oriented, NULL, 0, dotp);
   if (!SDO) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Allocate_SegmentDO.\n", FuncName);
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
   
   mri_free(im); im = NULL; far = NULL;

   SUMA_RETURN(SDO);
}

SUMA_SegmentDO * SUMA_ReadNBSegDO (char *s, int oriented, char *parent_SO_id)
{
   static char FuncName[]={"SUMA_ReadNBSegDO"};
   SUMA_SegmentDO *SDO = NULL;
   MRI_IMAGE * im = NULL;
   float *far=NULL;
   int itmp, itmp2, icol_thick = -1, icol_col=-1;
   int nrow=-1, ncol=-1;
   int NodeBased = 2;
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
      default:
         SUMA_SLP_Err("File must have\n"
                   "2, 3, 6, or 7 columns.");
         mri_free(im); im = NULL;   /* done with that baby */
         SUMA_RETURN(NULL);
   }

   /* allocate for segments DO */
   SDO = SUMA_Alloc_SegmentDO (ncol, s, oriented, 
                               parent_SO_id, NodeBased, dotp);
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
   SUMA_DO_Types type;
   char att[500], *Parent_idcode_str=NULL, *Label=NULL, *idcode_str=NULL;
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
   
   SDO = SUMA_Alloc_SegmentDO(N_n, Label, oriented, Parent_idcode_str, NodeBased, type);
   if (Label) SUMA_free(Label); Label = NULL; 
   if (Parent_idcode_str) SUMA_free(Parent_idcode_str); Parent_idcode_str = NULL;
   
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
         char *Label, char *idcode_str, char *Parent_idcode_str,
         float LineWidth, float *LineCol,
         int *NodeID, int *NodeID1, float *n0, float *n1,
         float *colv, float *thickv 
                                       ) 
{
   static char FuncName[]={"SUMA_CreateSegmentDO"};
   SUMA_SegmentDO *SDO=NULL;
   int ncp=0, i;
   SUMA_DO_Types type;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (oriented) type = LS_type;
   else type = OLS_type;
   
   SDO = SUMA_Alloc_SegmentDO(N_n, Label, oriented, Parent_idcode_str, NodeBased, type);
   if (idcode_str) SUMA_STRING_REPLACE(SDO->idcode_str, idcode_str);   
   SDO->NodeBased = NodeBased;
   SDO->Stipple = Stipple;
   SDO->LineWidth =LineWidth;
   if (LineCol) { for (i=0; i<4; ++i) SDO->LineCol[i] = LineCol[i]; }
   else { SDO->LineCol[0] = 0.4; SDO->LineCol[1] = 0.8; SDO->LineCol[2] = 0.1; SDO->LineCol[3] = 1.0; }

   if (NodeID) {
      SDO->NodeID = (int *)SUMA_Copy_Part_Column((void*)NodeID, NI_rowtype_find_code(NI_INT), N_n, NULL, 0, &ncp);
   } else SDO->NodeID = NULL;   
   if (NodeID1) {
      SDO->NodeID1 = (int *)SUMA_Copy_Part_Column((void*)NodeID1, NI_rowtype_find_code(NI_INT), N_n, NULL, 0, &ncp);
   } else SDO->NodeID1 = NULL; 
   if (!n0) {
      SDO->n0 = NULL;
   } else {
      SDO->n0 = (float *)SUMA_Copy_Part_Column((void *)n0,  NI_rowtype_find_code(NI_FLOAT), 3*N_n, NULL, 0, &ncp);
   }
   if (!n1) {
      SDO->n1 = NULL;
   } else {
      SDO->n1 = (float *)SUMA_Copy_Part_Column((void *)n1,  NI_rowtype_find_code(NI_FLOAT), 3*N_n, NULL, 0, &ncp);
   }
   if (!colv) {
      SDO->colv = NULL;
   } else {
      SDO->colv = (float *)SUMA_Copy_Part_Column((void *)colv,  NI_rowtype_find_code(NI_FLOAT), 4*N_n, NULL, 0, &ncp);
   }
   if (!thickv) {
      SDO->thickv = NULL;
   } else {
      SDO->thickv = (float *)SUMA_Copy_Part_Column((void *)thickv,  NI_rowtype_find_code(NI_FLOAT), N_n, NULL, 0, &ncp);
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
         SDO->stylev[itmp]     = SUMA_SphereStyleConvert((int)far[itmp+(icol_style  )*ncol]);
         ++itmp;
      } 
   }
   mri_free(im); im = NULL; far = NULL;

   SUMA_RETURN(SDO);
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

void SUMA_MeshAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceObject *cso)
{
   static char FuncName[]={"SUMA_MeshAxisStandard"};
   
   SUMA_ENTRY;

   Ax->Stipple = SUMA_SOLID_LINE;
   Ax->XYZspan[0]= Ax->XYZspan[1]= Ax->XYZspan[2]= 100.0;
   Ax->BR[0][0] = cso->MinDims[0]; Ax->BR[0][1] = cso->MaxDims[0];
   Ax->BR[1][0] = cso->MinDims[1]; Ax->BR[1][1] = cso->MaxDims[1];
   Ax->BR[2][0] = cso->MinDims[2]; Ax->BR[2][1] = cso->MaxDims[2];
   Ax->Center[0] = cso->Center[0];
   Ax->Center[1] = cso->Center[1];
   Ax->Center[2] = cso->Center[2];
   Ax->MTspace = 10; Ax->mTspace = 2;
   Ax->MTsize = 4; Ax->mTsize = 2;
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
   SUMA_RETURNe;
}

void SUMA_WorldAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_WorldAxisStandard"};
   float MinDims[3], MaxDims[3];
   int i, j, Nvis, *Vis_IDs=NULL;
   SUMA_SurfaceObject *cso=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!Ax) {
      SUMA_SL_Err("NULL Ax!");
      SUMA_RETURNe;
   }
   
   Ax->Stipple = SUMA_SOLID_LINE;
   Ax->XYZspan[0]= Ax->XYZspan[1]= Ax->XYZspan[2]= 100.0;
   Ax->MTspace = 10; Ax->mTspace = 2;
   Ax->MTsize = 4; Ax->mTsize = 2;
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
   Nvis = SUMA_VisibleSOs (sv, SUMAg_DOv, Vis_IDs);
   
   if (Nvis > 0) {
      for (i=0; i<Nvis; ++i) {
         cso = (SUMA_SurfaceObject *)SUMAg_DOv[Vis_IDs[i]].OP;
         if (!i) {
            for (j=0; j<3; ++j) {
               MinDims[j] = cso->MinDims[j];
               MaxDims[j] = cso->MaxDims[j];
            }
         } else {
            for (j=0; j<3; ++j) {
               if (cso->MinDims[j] < MinDims[j]) MinDims[j] = cso->MinDims[j];
               if (cso->MaxDims[j] > MaxDims[j]) MaxDims[j] = cso->MaxDims[j];
            }
         }
      }
      Ax->BR[0][0] = MinDims[0]; Ax->BR[0][1] = MaxDims[0];
      Ax->BR[1][0] = MinDims[1]; Ax->BR[1][1] = MaxDims[1];
      Ax->BR[2][0] = MinDims[2]; Ax->BR[2][1] = MaxDims[2];
   }
   if (Vis_IDs) SUMA_free(Vis_IDs);
   
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_DrawSphereDO (SUMA_SphereDO *SDO, SUMA_SurfaceViewer *sv)
{
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0}, comcol[4], *cent=NULL;
   int i, N_n3, i3;
   GLfloat rad = 3;
   static char FuncName[]={"SUMA_DrawSphereDO"};
   float origwidth=0.0;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SDO) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
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
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, comcol);
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
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, &(SDO->colv[i*4]));
         glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[i*4]));
      }
      if (SDO->radv) rad = SDO->radv[i];
      if (SDO->stylev) {
         gluQuadricDrawStyle (SDO->sphobj, SDO->stylev[i]); 
         if (SDO->stylev[i] == GLU_FILL) 
            gluQuadricNormals (SDO->sphobj , GLU_SMOOTH);
         else gluQuadricNormals (SDO->sphobj , GLU_NONE); 
      }
      if (SDO->NodeBased) {
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
   glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
   glLineWidth(origwidth);

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
   SDO->CommonCol[0] = 1.0; SDO->CommonCol[1] = 1.0; SDO->CommonCol[2] = 1.0; SDO->CommonCol[3] = 1.0; 
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
      SDO->NodeNormList[3*(n+3)  ] = eqn[0]; SDO->NodeNormList[3*(n+3)+1] = eqn[1]; SDO->NodeNormList[3*(n+3)+2] = eqn[2];
          
      /* Each quad representing a plane would be formed by nodes n, n+1, n+2 and n+3 */
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
   if (SDO->PolyMode == SRM_Hide || sv->PolyMode == SRM_Hide) { SUMA_RETURN(YUP); }
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
   
      
   /* This allows each node to follow the color specified when it was drawn (in case you'll want to color corners differently someday) */ 
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
            glDrawElements (GL_QUADS, (GLsizei)SDO->N_FaceSet*4, GL_UNSIGNED_INT, SDO->FaceSetList);
            break;
      case 1:
            glPointSize(4.0); /* keep outside of glBegin */
            /* it is inefficient to draw points using the glar_FaceSetList because nodes are listed more 
            than once. You are better off creating an index vector into glar_NodeList to place all the points, just once*/ 
            glDrawElements (GL_POINTS, (GLsizei)SDO->N_FaceSet*4, GL_UNSIGNED_INT, SDO->FaceSetList);
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


SUMA_Boolean SUMA_DrawSegmentDO (SUMA_SegmentDO *SDO, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawSegmentDO"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int i, N_n3, i3, n3, n, n1=0, n13=0;
   byte *msk=NULL;
   float origwidth=0.0, rad = 0.0, gain = 1.0;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SDO) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (SDO->NodeBased) { /* Locate the surface in question */
      SUMA_LH("Node-based vectors");
      if (!SDO->Parent_idcode_str) {
         SUMA_SL_Err("Object's parent idcode_str not specified.");
         SUMA_RETURN (NOPE);
      }
      SO = SUMA_findSOp_inDOv(SDO->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv); 
      if (!SO) {
         SUMA_SL_Err("Object's parent surface not found.");
         SUMA_RETURN (NOPE);
      }       
   } else {
      SUMA_LH("Segments ");
   }
   
   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   if (!SDO->thickv || SDO->NodeBased) glLineWidth(SDO->LineWidth);  
   
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
   
   if (SDO->NodeBased == 0) {
      if (SDO->thickv) {/* slow slow slow */
         SUMA_LH("Drawing xyz to xyz with thickness ");
         if (!SDO->colv) glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol);
         glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
            /* turn off ambient and diffuse components */
         glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
         i = 0;
         N_n3 = 3*SDO->N_n;
         while (i < N_n3) {
            if (SDO->thickv) glLineWidth(SDO->thickv[i/3]);   
            glBegin(GL_LINES);
            if (SDO->colv) 
               glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(i/3)]));
            glVertex3f(SDO->n0[i], SDO->n0[i+1], SDO->n0[i+2]);
            glVertex3f(SDO->n1[i], SDO->n1[i+1], SDO->n1[i+2]); 
            i += 3;
            glEnd();
         }
      } else {
         SUMA_LH("Drawing xyz to xyz ");
         glBegin(GL_LINES);
         if (!SDO->colv) 
            glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol); 
                  /*turn on emissivity  */
         glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
                  /* turn off ambient and diffuse components */
         glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);

         if (!SDO->NodeBased) {
            i = 0;
            N_n3 = 3*SDO->N_n;
            while (i < N_n3) {
               if (SDO->colv) 
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
      if (!SDO->colv) 
         glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol); 
               /*turn on emissivity  */
      glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
               /* turn off ambient and diffuse components */
      glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);      
      SUMA_LH("Drawing vectors ");
      i = 0;
      gain = 1.0;
      while (i < SDO->N_n) {
         n = SDO->NodeID[i]; n3 = 3*n;
         if (LocalHead) 
            fprintf(SUMA_STDERR,"%s: %d/%d, %d\n", FuncName, i, SDO->N_n, n);
         if (n<SO->N_Node) {
            i3 = 3*i;
            if (SDO->thickv) {
               gain = SDO->thickv[i]; 
            } else {
               /* gain = 1.0; no need, set above */  
            }
            if (SDO->colv) 
               glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(i)]));
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: SDO->n1 = [%f %f %f]\n", 
                     FuncName, SDO->n1[i3  ], SDO->n1[i3+1],SDO->n1[i3+2]); 
            glVertex3f( SO->NodeList[n3], SO->NodeList[n3+1], 
                        SO->NodeList[n3+2]);
            glVertex3f( SO->NodeList[n3]  +(gain * SDO->n1[i3  ]), 
                        SO->NodeList[n3+1]+(gain * SDO->n1[i3+1]), 
                        SO->NodeList[n3+2]+(gain * SDO->n1[i3+2])); 
         }
         i += 1;
      }
      glEnd();
   } else if (  SDO->NodeBased == 2 ) {
      if (!SDO->thickv) {
         glBegin(GL_LINES);
         if (!SDO->colv) 
            glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol); 
                  /*turn on emissivity  */
         glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
                  /* turn off ambient and diffuse components */
         glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);      

         SUMA_LH("two-node vectors ");
         i = 0;
         gain = 1.0;
         while (i < SDO->N_n) {
            n = SDO->NodeID[i]; n3 = 3*n;
            n1 = SDO->NodeID1[i]; n13 = 3*n1;
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: %d/%d, %d,%d\n", 
                        FuncName, i, SDO->N_n, n, n1);
            if (n<SO->N_Node && n1 < SO->N_Node) {
               i3 = 3*i;
               
               if (SDO->colv) 
                  glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(i)]));
               
               glVertex3f( SO->NodeList[n3], SO->NodeList[n3+1], 
                           SO->NodeList[n3+2]);
               glVertex3f( SO->NodeList[n13  ], 
                           SO->NodeList[n13+1], 
                           SO->NodeList[n13+2]); 
            }
            i += 1;
         }
         glEnd();
      } else {/* slow slow slow */
         if (!SDO->colv) glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol);
         glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); 
            /* turn off ambient and diffuse components */
         glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);

         SUMA_LH("two-node vectors, with thickness ");
         i = 0;
         while (i < SDO->N_n) {
            n = SDO->NodeID[i]; n3 = 3*n;
            n1 = SDO->NodeID1[i]; n13 = 3*n1;
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: %d/%d, %d,%d\n", 
                        FuncName, i, SDO->N_n, n, n1);
            if (n<SO->N_Node && n1 < SO->N_Node) {
               i3 = 3*i;
               if (SDO->thickv) glLineWidth(SDO->thickv[i]); 
               glBegin(GL_LINES);
               if (SDO->colv) 
                  glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[4*(i)]));
               
               glVertex3f( SO->NodeList[n3], SO->NodeList[n3+1], 
                           SO->NodeList[n3+2]);
               glVertex3f( SO->NodeList[n13  ], 
                           SO->NodeList[n13+1], 
                           SO->NodeList[n13+2]); 
               glEnd();
            }
            i += 1;
         }
      }
   } else {
      SUMA_S_Err("Oh no. Bad logic");
      goto GETOUT;
   }
   
   switch (SDO->Stipple) {
      case SUMA_DASHED_LINE:
         glDisable(GL_LINE_STIPPLE);
         break;
      case SUMA_SOLID_LINE:
         break;
   }
   
   /* draw the bottom object */
   SUMA_LH("Drawing bottom");
   if (SDO->botobj) {
      glLineWidth(0.5);
      if (!SDO->colv) {
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, SDO->LineCol);
      }
      
      if (!SDO->NodeBased) {
         if (!SDO->thickv) rad = SDO->LineWidth*0.5*sv->FOV[sv->iState]/FOV_INITIAL;
      } else {
         if (SO->EL) rad = SO->EL->AvgLe/4.0;
         else rad = 1.0/4.0;
      }
      gluQuadricDrawStyle (SDO->botobj, GLU_FILL); 
      gluQuadricNormals (SDO->botobj , GLU_SMOOTH);

      if (!SDO->NodeBased) {
         for (i=0; i<SDO->N_n;++i) {
            i3 = 3*i;
            if (SDO->colv) {
               glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, &(SDO->colv[i*4]));
               glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[i*4]));
            }
            if (SDO->thickv) rad = SDO->thickv[i]*0.5*sv->FOV[sv->iState]/FOV_INITIAL;
            /* fprintf(SUMA_STDERR,"thickv[i] = %f, FOV %f, FOV_INITIAL %f, radinit=%f, radcomp=%f\n", 
                           SDO->thickv[i], sv->FOV[sv->iState], FOV_INITIAL,
                           rad,  rad *SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06)); */
            glTranslatef (SDO->n0[i3], SDO->n0[i3+1], SDO->n0[i3+2]);
            gluSphere(SDO->botobj, SUMA_MAX_PAIR(rad, 0.005) /* *SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06) */ , 
                      10, 10);
            glTranslatef (-SDO->n0[i3], -SDO->n0[i3+1], -SDO->n0[i3+2]);
         }
      } else { /* Node based vector */
         /* create a mask for those spheres already drawn, multiple vectors per node possible...*/
         msk = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte));
         if (!msk) { 
            SUMA_SL_Crit("Failed to allocate!\n");
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
            goto GETOUT;
         }
         for (i=0; i<SDO->N_n;++i) {
            i3 = 3*i;
            n = SDO->NodeID[i]; n3 = 3*n;
            if (LocalHead) fprintf(SUMA_STDERR,"%s: %d/%d, %d\n", FuncName, i, SDO->N_n, n);
            if (n<SO->N_Node) {
               if (!msk[n]) {
                  if (SDO->colv) {
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, &(SDO->colv[i*4]));
                     glMaterialfv(GL_FRONT, GL_EMISSION, &(SDO->colv[i*4]));
                  }

                  glTranslatef (SO->NodeList[n3]  , SO->NodeList[n3+1]  , SO->NodeList[n3+2]  );
                  gluSphere(SDO->botobj, SUMA_MAX_PAIR(rad, 0.005) /* *SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06) */ , 
                            10, 10);
                  glTranslatef (-SO->NodeList[n3]  , -SO->NodeList[n3+1]  , -SO->NodeList[n3+2]  );
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
   
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity */
   glLineWidth(origwidth);
     
   SUMA_RETURN (YUP);
   
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
                                    void *default_font,
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
   if (!SUMA_PrepForNIDOnelPlacement(sv, nel, default_SO, 
                                     txloc, NULL, sz, 
                                     &orthoreset, coord_units)) {
      SUMA_RETURN(NOPE);
   }    

   /* how about that rendering mode ? */
   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   }
   
   /* texture's on? */
   if (TexOn2D = glIsEnabled(GL_TEXTURE_2D)) { 
      /* turn off or affect image drawing */
      glDisable(GL_TEXTURE_2D);
   }
   if (TexOnGenT = glIsEnabled(GL_TEXTURE_GEN_T)) glDisable(GL_TEXTURE_GEN_T);
   if (TexOnGenS = glIsEnabled(GL_TEXTURE_GEN_S))  glDisable(GL_TEXTURE_GEN_S);
                  
   /* have image, go for it */
   SUMA_LH(  "Drawing the image.");

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
                                    void *default_font,
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
   int id=0, ii = 0, sz[3]={0, 0, 0};
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
   if (!SUMA_PrepForNIDOnelPlacement(sv, nel, default_SO, 
                                     txloc, texcoord, sz, 
                                     &orthoreset, coord_units)) {
      SUMA_RETURN(NOPE);
   }    

   /* how about that rendering mode ? */
   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   }
   
   /* does this have its own coordinates ? */
   target = NI_get_attribute(nel,"target");
   if (target && strcmp(target, "FRAME")==0) {
      SUMA_LH(  "Creating texture, see init pp 359 in \n"
                "OpenGL programming guide, 3rd ed.");
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
      SUMA_LHv("Texture as image\n"
                   "coords:\n"
                   "%.3f %.3f %.3f\n%.3f %.3f %.3f\n"
                   "%.3f %.3f %.3f\n%.3f %.3f %.3f\n",
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
      /* THIS block must be executed Immediately before a surface is drawn,
         otherwise, the texture will apply to other objects that are drawn
         next. To do this, set a texnel flag for those surfaces involved and 
         have DrawMesh set the textures there.  */
      N_SOlist = SUMA_VisibleSOs(sv, SUMAg_DOv, SOlist);
      if (!target || strncmp(target, "ALL_SURF",8)==0) {
         for (ii=0; ii < N_SOlist; ++ii) {
            SOt = (SUMA_SurfaceObject *)SUMAg_DOv[SOlist[ii]].OP;
            SOt->texnel = nel; /* better not keep texnel long after it is drawn!
                                 nel could be blown away after drawing! */
         }
      } else {
         for (ii=0; ii < N_SOlist; ++ii) {
            SOt = (SUMA_SurfaceObject *)SUMAg_DOv[SOlist[ii]].OP;
            if (SUMA_iswordin(SOt->Label,target)) 
               SOt->texnel = nel; /* SOt->texnel copy of nel should not be kept 
                                    after drawing! */
         }
      }   
   }
   
   if (orthoreset) {/* return value */
      /* and just pop what you'd pushed in */
      glPopMatrix();
   }
   
   if (sv->PolyMode != SRM_Fill) {/* set fill mode back */
      SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
   }
   
   SUMA_RETURN(YUP);   
}

SUMA_Boolean SUMA_PrepForNIDOnelPlacement (  SUMA_SurfaceViewer *sv,
                                             NI_element *nel, 
                                             SUMA_SurfaceObject *default_SO,
                                             float *txloc, float *texcoord,
                                             int *sz, 
                                             int *orthoreset,
                                             SUMA_DO_CoordUnits coord_units)
{
   static char FuncName[]={"SUMA_PrepForNIDOnelPlacement"};
   int id = 0, id3=0, k=0;
   char *atr=NULL;
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
   if (NI_GOT) {
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
      NI_GET_FLOATv(nel,"coord",txloc,3, LocalHead);
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
         SUMA_LHv("sz=[%d, %d, %d]\n", sz[0], sz[1], sz[2]);
         glGetIntegerv(GL_VIEWPORT, viewport);
         
         if (atr = NI_get_attribute(nel,"h_align")) {
            if (atr[0] == 'c' || atr[0] == 'C') { /* center */
               txloc[0] = txloc[0] - ((float)sz[0]/2.0 / (float)viewport[2]);
            } else if (atr[0] == 'r' || atr[0] == 'R') { /* right */
               txloc[0] = txloc[0] - ((float)sz[0] / (float)viewport[2]);
            }
         }
         if (nel->name[0] == 'T') { /* special treatment for text */
            if (atr = NI_get_attribute(nel,"v_align")) {
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
            if (atr = NI_get_attribute(nel,"v_align")) {
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
            if (txloc[id] >= 0.999999) txloc[id] = 0.999999;
               /*else it can get clipped*/
            else if (txloc[id] <= 0.000001) txloc[id] = 0.000001;
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
                             pfront, pback);
         
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
                  if       (texcoord[id3+k] > 0.999999) 
                     texcoord[id3+k] = 0.999999;
                  else if  (texcoord[id3+k] < 0.000001) 
                     texcoord[id3+k] = 0.000001;
               }
               SUMA_NormScreenToWorld( NULL, 
                                       (double)texcoord[id3  ], 
                                       (double)texcoord[id3+1],
                                       pfront, pback);
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


SUMA_Boolean SUMA_DrawTextNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *default_SO,
                                    SUMA_DO_CoordUnits default_coord_units,
                                    float *default_color, 
                                    void *default_font,
                                    SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_DrawTextNIDOnel"};
   char  *string=NULL, *atr=NULL;
   GLfloat rpos[4];
   float Dx = 0.0;
   void *font=NULL;
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   float txloc[3] = {0.0, 0.0, 0.0};
   GLfloat txcol[4];
   GLboolean valid;
   int orthoreset = 0;
   int id=0, is = 0, sz[3]={0, 0,0};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_DO_CoordUnits coord_units = default_coord_units;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;

   if (!nel || strcmp(nel->name,"T")) SUMA_RETURN(NOPE); 

   SUMA_LHv(  "default_coord_units %d\n", default_coord_units);
    
   if (atr = NI_get_attribute(nel, "font")) {
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
   
   /* has the box size been determined , 3rd dim is number of lines */
   NI_GET_INTv(nel,"box_size", sz, 3, LocalHead);
   if (!NI_GOT) {
      SUMA_TextBoxSize(NI_get_attribute(nel,"text"), sz, sz+1, sz+2, font);
      NI_SET_INTv(nel,"box_size", sz, 3);
   }
   /* set up projection conditions and coordinates */
   if (!SUMA_PrepForNIDOnelPlacement(sv, nel, default_SO, 
                                     txloc, NULL, sz, 
                                     &orthoreset, coord_units)) {
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
   string = NI_get_attribute(nel,"text"); 
   SUMA_LHv(  "text:\n"
              ">>>%s<<<\n", string);
   for (is=0; string && string[is] != '\0'; is++) {
      if (string[is] == '\n') {
         glBitmap( 0, 0, 0, 0, 
                  -(float) Dx, -(float) SUMA_glutBitmapFontHeight(font), 
                  NULL );
         Dx = 0; 
      } else {
         glutBitmapCharacter(font, string[is]);
         Dx = Dx+(float) glutBitmapWidth(font, string[is]);
      }
   }

   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
      /*turn off emissidity for text*/   

   if (orthoreset) {/* return value */
      /* and just pop what you'd pushed in */
      glPopMatrix();
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_DrawSphereNIDOnel(  NI_element *nel, 
                                    SUMA_SurfaceObject *default_SO,
                                    SUMA_DO_CoordUnits default_coord_units,
                                    float *default_color, 
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
   }
   
   /* set up projection conditions and coordinates */
   if (!SUMA_PrepForNIDOnelPlacement(sv, nel, default_SO, 
                                     txloc, NULL, sz, 
                                     &orthoreset, coord_units)) {
      SUMA_RETURN(NOPE);
   }    

   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   NI_GET_FLOAT(nel,"line_width", LineWidth);
   if (!NI_GOT) LineWidth = 2;
   glLineWidth(LineWidth);
      
   glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, txcol);
   glMaterialfv(GL_FRONT, GL_EMISSION, txcol);
   glColor3fv(txcol); 
   
   NI_GET_FLOAT(nel,"rad", rad);
   if (!NI_GOT) rad = 10;

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

SUMA_Boolean SUMA_DrawNIDO (SUMA_NIDO *SDO, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawNIDO"};
   int ip=0;
   int is;
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
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SDO) {
      fprintf(stderr,"Error %s: NULL pointer.\n", FuncName);
      SUMA_RETURN (NOPE);
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
            default_SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
            /* last ditch */
            if (!default_SO) {
               SUMA_LH("No current SO, trying any");
               default_SO = SUMA_findanySOp_inDOv(SUMAg_DOv, SUMAg_N_DOv);
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
         default_SO = SUMA_findanySOp_inDOv(SUMAg_DOv, SUMAg_N_DOv);
         /* OK if you fail here ... */
      } 
   }
   SUMA_LHv("Default_SO %p\n", default_SO);
   if (SUMA_isNIDO_SurfBased(SDO) && !default_SO) {
         SUMA_SL_Err("Object's parent surface not found.");
         SUMA_RETURN (NOPE);
   } 
   
   ngr = SDO->ngr;
   {
      SUMA_LH("Setting up other nido defaults:");
      
      /* set up group defaults */
         if (atr = NI_get_attribute(ngr, "default_font")) {
            if (!(default_font = SUMA_glutBitmapFont(atr))) {
               SUMA_S_Errv("Bad font %s, using default %s", 
                           atr,
                  SUMA_glutBitmapFontName(SUMA_glutBitmapFont(NULL)));
               default_font=SUMA_glutBitmapFont(NULL);
            }
         }
         
         NI_GET_FLOATv(ngr,"default_color", txcol, 4,LocalHead);
         if (NI_GOT) {
            default_color[0] = txcol[0];
            default_color[1] = txcol[1]; 
            default_color[2] = txcol[2]; 
            default_color[3] = txcol[3]; 
         }
         if (atr = NI_get_attribute(ngr, "coord_type")) {
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
            SUMA_SL_Err(
               "Don't know what to make of this group element, ignoring.");
            break;
      }
      if (nel) { /* something to display */
         SUMA_LHv("Attempting to draw %s\n", nel->name);
         if (! strcmp(nel->name,"T")) {
            if (!SUMA_DrawTextNIDOnel( nel, default_SO, 
                                       default_coord_units,
                                       default_color, 
                                       default_font, sv)) {
               SUMA_S_Warnv("Failed to draw %s\n", nel->name);
            }
         } else if (! strcmp(nel->name,"S")) {
            if (!SUMA_DrawSphereNIDOnel( nel, default_SO, 
                                       default_coord_units,
                                       default_color, 
                                       sv)) {
               SUMA_S_Warnv("Failed to draw %s\n", nel->name);
            }
         } else if (! strcmp(nel->name,"I")) {
            if (!SUMA_DrawImageNIDOnel(nel, default_SO, 
                                       default_coord_units,
                                       default_color, 
                                       default_font, sv)) {
               SUMA_S_Warnv("Failed to draw %s\n", nel->name);
            }
         } else if (! strcmp(nel->name,"Tex")) {
            if (!SUMA_DrawTextureNIDOnel(nel, default_SO, 
                                       default_coord_units,
                                       default_color, 
                                       default_font, sv)) {
               SUMA_S_Warnv("Failed to draw %s\n", nel->name);
            }
         } else if (! strcmp(nel->name,"3DTex")) {
            if (SUMAg_CF->Dev) {
               SUMA_LH("Going to draw 3DTexture");
               if (!SUMA_Draw3DTextureNIDOnel(nel, default_SO, 
                                          default_coord_units,
                                          default_color, 
                                          default_font, sv)) {
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
               if (LocalHead) SUMA_LHv("Skipping element %s\n", nel->name);  
            }
         }      
      }
      
   }
   
   GETOUT:
     
   SUMA_RETURN (YUP);
   
}


/*!
   A macro to be inserted into SUMA_SortedAxisSegmentList's switch statement
*/
#define SUMA_SORTED_AXIS_WORKS { \
            ASIp->Quad[0] = Q[ASIp->PointIndex[0]];   \
            ASIp->Quad[1] = Q[ASIp->PointIndex[1]];   \
            for (i=0;i<3;++i) d[i] = ( P[ASIp->PointIndex[1]][i] - P[ASIp->PointIndex[0]][i] ); \
            SUMA_NORM_VEC (d, 3, ASIp->world_length); \
            ASIp->screen_length_x = S[ASIp->PointIndex[1]*3] - S[ASIp->PointIndex[0]*3];   \
            ASIp->screen_length_y = S[ASIp->PointIndex[1]*3+1] - S[ASIp->PointIndex[0]*3+1];   \
            for (i=0;i<3;++i) d[i] = ( ( P[ASIp->PointIndex[0]][i] + P[ASIp->PointIndex[1]][i] ) / 2.0 - sv->Pcenter_close[i] ); \
            SUMA_NORM_VEC (d, 3, ASIp->MidSegDist);   \
            for (i=0;i<2;++i) d[i] = ( S[ASIp->PointIndex[0]*3+i] - LLC[i] );  \
            SUMA_NORM_VEC (d, 2, d1);  \
            for (i=0;i<2;++i) d[i] = ( S[ASIp->PointIndex[1]*3+i] - LLC[i] );  \
            SUMA_NORM_VEC (d, 2, d2);  \
            if (d1 < d2) { ASIp->LLCclosestDist = d1; ASIp->LLCclosestPoint = 0; }  \
            else { ASIp->LLCclosestDist = d2; ASIp->LLCclosestPoint = 1; } \
            for (i=0;i<3;++i) d[i] = ( C[ASIp->FaceIndex[0]][i] - sv->Pcenter_close[i] ); \
            SUMA_NORM_VEC (d, 3, d1);  \
            for (i=0;i<3;++i) d[i] = ( C[ASIp->FaceIndex[1]][i] - sv->Pcenter_close[i] ); \
            SUMA_NORM_VEC (d, 3, d2); if (d1 < d2) ASIp->MidFaceDist = d1; else ASIp->MidFaceDist = d2;  \
            SUMA_COPY_VEC(P[ASIp->PointIndex[0]], ASIp->P1, 3, double, double);  \
            SUMA_COPY_VEC(P[ASIp->PointIndex[1]], ASIp->P2, 3, double, double);  \
            ASIp->TxOff[0] = (-ASIp->tick2_dir[0] - ASIp->tick1_dir[0]);   \
            ASIp->TxOff[1] = (-ASIp->tick2_dir[1] - ASIp->tick1_dir[1]);   \
            ASIp->TxOff[2] = (-ASIp->tick2_dir[2] - ASIp->tick1_dir[2]);   \
            SUMA_NORM_VEC (ASIp->TxOff, 3, d1); /* DO NOT USE /= in the next line !!! */\
            ASIp->TxOff[0] = ASIp->TxOff[0] / d1; ASIp->TxOff[1] = ASIp->TxOff[1] / d1; ASIp->TxOff[2] = ASIp->TxOff[2] / d1; \
}
/*!
   
   \param opt (SUMA_SORT_BOX_AXIS_OPTION) Specifies how sorting of segments is done in the list.
   
   Creates the various segments needed to form a box axis.
   
   - Your job is to free DList's elements and destroy and free DList after the function returns.
   
   \sa Labbook NIH-4 page 21 for annotation of Box Axis ....
*/
DList *SUMA_SortedAxisSegmentList (SUMA_SurfaceViewer *sv, SUMA_Axis *Ax, SUMA_SORT_BOX_AXIS_OPTION opt)
{
   static char FuncName[]={"SUMA_SortedAxisSegmentList"};
   double P[8][3], C[6][3], d[3], d1, d2, S[24], world_length, screen_length_x, screen_length_y;
   int Q[8];
   static double xAx[3] = {1, 0, 0}, yAx[3] = { 0, 1, 0 }, zAx[3] = {0, 0, 1}, LLC[3] = {0, 0, 0};
   static double mxAx[3] = {-1, 0, 0}, myAx[3] = { 0, -1, 0 }, mzAx[3] = {0, 0, -1};
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
   P[0][0] = Ax->BR[0][0]; P[0][1] = Ax->BR[1][0]; P[0][2] = Ax->BR[2][0]; /*xmin, ymin, zmin */ 
   P[1][0] = Ax->BR[0][1]; P[1][1] = Ax->BR[1][0]; P[1][2] = Ax->BR[2][0]; /*xmax, ymin, zmin */ 
   P[2][0] = Ax->BR[0][0]; P[2][1] = Ax->BR[1][1]; P[2][2] = Ax->BR[2][0]; /*xmin, ymax, zmin */ 
   P[3][0] = Ax->BR[0][1]; P[3][1] = Ax->BR[1][1]; P[3][2] = Ax->BR[2][0]; /*xmax, ymax, zmin */
   P[4][0] = Ax->BR[0][0]; P[4][1] = Ax->BR[1][0]; P[4][2] = Ax->BR[2][1]; /*xmin, ymin, zmax */ 
   P[5][0] = Ax->BR[0][1]; P[5][1] = Ax->BR[1][0]; P[5][2] = Ax->BR[2][1]; /*xmax, ymin, zmax */ 
   P[6][0] = Ax->BR[0][0]; P[6][1] = Ax->BR[1][1]; P[6][2] = Ax->BR[2][1]; /*xmin, ymax, zmax */ 
   P[7][0] = Ax->BR[0][1]; P[7][1] = Ax->BR[1][1]; P[7][2] = Ax->BR[2][1]; /*xmax, ymax, zmax */
   
   /* figure out equivalent screen coords */
   SUMA_World2ScreenCoords (sv, 8, (double *)P, S, Q, 0);
   
   /* form plane centers */
   for (i=0; i<3; ++i) { C[0][i] = ( P[0][i] + P[1][i] + P[5][i] + P[4][i] ) / 4.0; } /* Plane a, b, f, e*/
   for (i=0; i<3; ++i) { C[1][i] = ( P[0][i] + P[1][i] + P[3][i] + P[2][i] ) / 4.0; } /* Plane a, b, d, c*/
   for (i=0; i<3; ++i) { C[2][i] = ( P[0][i] + P[2][i] + P[6][i] + P[4][i] ) / 4.0; } /* Plane a, c, g, e*/
   for (i=0; i<3; ++i) { C[3][i] = ( P[4][i] + P[5][i] + P[7][i] + P[6][i] ) / 4.0; } /* Plane e, f, h, g*/
   for (i=0; i<3; ++i) { C[4][i] = ( P[1][i] + P[3][i] + P[7][i] + P[5][i] ) / 4.0; } /* Plane b, d, h, f*/
   for (i=0; i<3; ++i) { C[5][i] = ( P[2][i] + P[3][i] + P[7][i] + P[6][i] ) / 4.0; } /* Plane c, d, h, g*/
  
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
                     if (ASIp->Quad[0] == SUMA_LOWER_LEFT_SCREEN || ASIp->Quad[1] == SUMA_LOWER_LEFT_SCREEN) {
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
                     SUMA_S_Err("Whatchyoutalkingboutwillis?\nBad, bad bad bad.");
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
                     FuncName, i, ASIp->AxisDim, ASIp->MidSegDist, ASIp->MidFaceDist, ASIp->Quad[0], ASIp->Quad[1], ASIp->world_length, ASIp->screen_length_x, ASIp->screen_length_y);
         ++i;
      } while(Elm != dlist_tail(list));
   }
   SUMA_free(ASI); ASI = NULL;
   SUMA_LH("Returning");
   SUMA_RETURN(list);
}


/*!
   \sa Labbook NIH-4 page 21 for annotation of Box Axis ....
*/
SUMA_Boolean SUMA_DrawAxis (SUMA_Axis* Ax, SUMA_SurfaceViewer *sv)
{ 
   static char FuncName[]={"SUMA_DrawAxis"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   double P1[3], P2[3], cP[8][3], SC[12][3], d[12];
   int i, N_Ax;
   DList *slist=NULL;
   DListElmt *Elm=NULL;
   SUMA_AxisSegmentInfo *ASI = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
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
         glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); /* turn off ambient and diffuse components */
         glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
         
         glMaterialfv(GL_FRONT, GL_EMISSION, Ax->XaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(-Ax->XYZspan[0]+Ax->Center[0], Ax->Center[1], Ax->Center[2]);
         glVertex3f(Ax->XYZspan[0]+Ax->Center[0], Ax->Center[1], Ax->Center[2]); 
         glEnd();
         
         glMaterialfv(GL_FRONT, GL_EMISSION, Ax->YaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ax->Center[0], -Ax->XYZspan[1]+Ax->Center[1], Ax->Center[2]);
         glVertex3f(Ax->Center[0], +Ax->XYZspan[1]+Ax->Center[1], Ax->Center[2]); 
         glEnd();
         
         glMaterialfv(GL_FRONT, GL_EMISSION, Ax->ZaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ax->Center[0], Ax->Center[1], -Ax->XYZspan[2]+Ax->Center[2]);
         glVertex3f(Ax->Center[0], Ax->Center[1], Ax->XYZspan[2]+Ax->Center[2]); 
         glEnd();
         
         glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, NoColor); /*turn off emissivity for axis*/

         break;
      case SUMA_SCALE_BOX:
         /* Sort segments by distance from screen center*/
         slist = SUMA_SortedAxisSegmentList (sv , Ax, SUMA_SORT_BY_LLC_DISTANCE); 
                     /* - when using SUMA_BY_PLANE_DISTANCE, it makes sense to show  
                        the first 4 segments, but you have no perception of the depth
                        - when using SUMA_NO_SORT, show all 12 segments so you'll see the box
                        - The world distance thingy does not quite work because because the plane you want to
                        hide is the one opposite to the one closest to your face. That plane is not
                        necessarily the farthest from sv->Pcenter_close, perhaps what you need to do is 
                        define the lower left point of the box and allow for either a boxed axis
                        or a 3d axis from the lower left corner.... 
                        - So I am now using the screen coordinates SUMA_SORT_BY_LLC_DISTANCE and SUMA_SORT_BY_LL_QUAD,
                        and sorting by SUMA_SORT_BY_LLC_DISTANCE works best. You can just show the first 3 axis and you're
                        cool, for most angles.
                        - You also need a pixels/mm number
                        to decide on where to put the text.*/ 
         
         if (sv->ShowWorldAxis == SUMA_THREE_WAX || sv->ShowWorldAxis == SUMA_THREE_TEXT_WAX) N_Ax = 3;
         else N_Ax = slist->size;
 
         Elm = dlist_head(slist); i = 0;
         do {
            ASI = (SUMA_AxisSegmentInfo *)Elm->data;
            if (ASI->AxisDim == 0) {
               if (LocalHead) fprintf(SUMA_STDERR,"%s: X axis, i = %d, SegIndex = %d, world, Sx, Sy = %.2f,%.2f,%.2f, off = %f, %f %f\n", FuncName, i, ASI->SegIndex, ASI->world_length, ASI->screen_length_x, ASI->screen_length_y, ASI->TxOff[0], ASI->TxOff[1],ASI->TxOff[2]); 
            } else if (ASI->AxisDim == 1) {
               if (LocalHead) fprintf(SUMA_STDERR,"%s: Y axis, i = %d, SegIndex = %d, world, Sx, Sy = %.2f,%.2f,%.2f, off = %f, %f %f\n", FuncName, i, ASI->SegIndex, ASI->world_length, ASI->screen_length_x, ASI->screen_length_y, ASI->TxOff[0], ASI->TxOff[1],ASI->TxOff[2]); 
            } else if (ASI->AxisDim == 2) {
               if (LocalHead) fprintf(SUMA_STDERR,"%s: Z axis, i = %d, SegIndex = %d, world, Sx, Sy = %.2f,%.2f,%.2f, off = %f, %f %f\n", FuncName, i, ASI->SegIndex, ASI->world_length, ASI->screen_length_x, ASI->screen_length_y, ASI->TxOff[0], ASI->TxOff[1],ASI->TxOff[2]); 
            } else { SUMA_S_Err("Major bobo."); SUMA_RETURN(NOPE); }
            
            if (i < 3 && (sv->ShowWorldAxis == SUMA_THREE_TEXT_WAX || sv->ShowWorldAxis == SUMA_BOX_TEXT_WAX)) 
               SUMA_DrawLineAxis (ASI, Ax, YUP);
            else SUMA_DrawLineAxis (ASI, Ax, NOPE);
            
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
               "Need %d pixels with a max of %d pixels on one line\n"
               "number of lines: %d\n",
            txt, *h,*w, *nl);
   }
       
       
   SUMA_RETURN(YUP);
}

/*! 
   \brief Draws a scale line.
   
   \param ASIp (SUMA_AxisSegmentInfo *) structure containing segment info
   \param Ax (SUMA_Axis *)
*/
SUMA_Boolean SUMA_DrawLineAxis ( SUMA_AxisSegmentInfo *ASIp, SUMA_Axis *Ax, SUMA_Boolean AddText)
{
   static char FuncName[]={"SUMA_DrawLineAxis"};
   double u3[3],nu, nu3, txofffac, size[2], space[2];
   double Pt[3], Ps[3];
   int prec = 1000, NmT;
   int i, jj, nTick[2];
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
         
   glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); /* turn off ambient and diffuse components */
   glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
            
   if (ASIp->AxisDim == 0) {
      glMaterialfv(GL_FRONT, GL_EMISSION, Ax->XaxisColor); /*turn on emissivity for X axis*/
      if (LocalHead) fprintf(SUMA_STDERR,"%s: X axis\n", FuncName); 
   } else if (ASIp->AxisDim == 1) {
      glMaterialfv(GL_FRONT, GL_EMISSION, Ax->YaxisColor); /*turn on emissivity for Y axis*/
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Y axis\n", FuncName); 
   } else if (ASIp->AxisDim == 2) {
      glMaterialfv(GL_FRONT, GL_EMISSION, Ax->ZaxisColor); /*turn on emissivity for Z axis*/
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
          Pt[0] = NmT * space[jj] * u3[0] +ASIp->P1[0]; Pt[1] = NmT * space[jj] * u3[1]+ASIp->P1[1];  Pt[2] = NmT * space[jj] * u3[2]+ASIp->P1[2];
      }
      if (LocalHead) fprintf(SUMA_STDERR,"%s:\nStarting ticks at [%f %f %f]\nnu3 = %f\n", FuncName, Pt[0], Pt[1], Pt[2], nu3);


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
      dSxT = (float)fabs(ASIp->screen_length_x) / (float)nTick[1];
      dSyT = (float)fabs(ASIp->screen_length_y) / (float)nTick[1];
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
                  SUMA_AxisText(ASIp, Ps);
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
SUMA_DRAWN_ROI **SUMA_Find_ROIonSO (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_do, int *N_ROI)
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

/*! Create Node-based spheres for a particular surface */
SUMA_Boolean SUMA_Draw_SO_NBSP (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_do, SUMA_SurfaceViewer *sv)
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


/*! Create the ROIs for a particular surface */
SUMA_Boolean SUMA_Draw_SO_ROI (SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_do, SUMA_SurfaceViewer *sv)
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
   int i, id, ii, id1,id2, id3, EdgeIndex, FaceIndex, Node1, Node2, Node3, N_ROId=0, idFirst=0;
   float dx, dy, dz = 0.0;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   SUMA_ROI_DATUM *ROId=NULL;
   SUMA_ROI *ROI = NULL;
   DListElmt *NextElm=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
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
                        SUMA_LH("Red first, Green next");
                        SUMA_COPY_VEC(Red, ROI_SphCol_frst, 4, GLfloat, GLfloat);
                        SUMA_COPY_VEC(Green, ROI_SphCol, 4, GLfloat, GLfloat);     
                        break;   
                     case SUMA_ROI_ClosedPath:
                        SUMA_LH("Yellow first, Cyan next");
                        SUMA_COPY_VEC(Yellow, ROI_SphCol_frst, 4, GLfloat, GLfloat);
                        SUMA_COPY_VEC(Cyan, ROI_SphCol, 4, GLfloat, GLfloat);      
                        break;   
                     case SUMA_ROI_FilledArea:
                        SUMA_LH("Pink First, Yellow Next");
                        SUMA_COPY_VEC(Pink, ROI_SphCol_frst, 4, GLfloat, GLfloat);
                        SUMA_COPY_VEC(Cyan, ROI_SphCol, 4, GLfloat, GLfloat);       
                        break;   
                     default:
                        SUMA_LH("Default");
                        ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 0.3; ROI_SphCol_frst[2] = 1.0; ROI_SphCol_frst[3] = 1.0;     
                        ROI_SphCol[0] = 1.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 0.0; ROI_SphCol[3] = 1.0;     
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
                              glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, ROI_SphCol_frst);
                              idFirst = 3 * ROId->nPath[0];
                              glTranslatef (SO->NodeList[idFirst], SO->NodeList[idFirst+1], SO->NodeList[idFirst+2]);
                              gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad*SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06), 
                                       SO->NodeMarker->slices, SO->NodeMarker->stacks);
                              glTranslatef (-SO->NodeList[idFirst], -SO->NodeList[idFirst+1], -SO->NodeList[idFirst+2]);
                           } 

                           glLineWidth(6);
                           glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, ROI_SphCol);
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
                              gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad*SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06),
                                        SO->NodeMarker->slices, SO->NodeMarker->stacks);
                              glTranslatef (-SO->NodeList[id], -SO->NodeList[id+1], -SO->NodeList[id+2]);
                           }


                           ++N_ROId;
                        }
                     } else { /* non segment type Drawn ROI */
                           #if 0 
                              /* it is too much to fill with spheres... */
                              glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, ROI_NodeGroup);
                              for (ii=0; ii < ROId->N_n; ++ii) {
                                 id = 3 * ROId->nPath[ii];
                                 glTranslatef (SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]);
                                 gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
                                 glTranslatef (-SO->NodeList[id], -SO->NodeList[id+1], -SO->NodeList[id+2]);
                              }
                           #endif
                     }
                  } while (NextElm != dlist_tail(D_ROI->ROIstrokelist));
               } else {
                  /* finished, draw contour */
                  SUMA_LH("Finished DROI");
                  ROI_SphCol_frst[0] = 1.0; ROI_SphCol_frst[1] = 0.3; ROI_SphCol_frst[2] = 1.0; ROI_SphCol_frst[3] = 1.0;     
                  ROI_SphCol[0] = 1.0; ROI_SphCol[1] = 1.0; ROI_SphCol[2] = 0.0; ROI_SphCol[3] = 1.0;
                  
                  
                  if (D_ROI->CE) {
                     int id1cont, id2cont, icont;
                     /* Draw the contour */
                     glLineWidth(6);
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, D_ROI->FillColor);
                     
                     for (icont = 0; icont < D_ROI->N_CE; ++icont) {
                        SUMA_LH("Drawing contour ...");
                        id1cont = 3 * D_ROI->CE[icont].n1;
                        id2cont = 3 * D_ROI->CE[icont].n2;
                        glBegin(GL_LINES);
                        glVertex3f(SO->NodeList[id2cont], SO->NodeList[id2cont+1], SO->NodeList[id2cont+2]);
                        glVertex3f(SO->NodeList[id1cont], SO->NodeList[id1cont+1], SO->NodeList[id1cont+2]); 
                        glEnd();
                     }
                  }
                  
                  
                  
               }

            }
            break;
            
         case ROIO_type:
            /* hopefully he distinction between drawn and not drawn will no longer be needed .... */
            ROI = (SUMA_ROI *)dov[i].OP;
            if (SUMA_isROIrelated (ROI, SO)) { /* draw it */
               if (LocalHead) fprintf(SUMA_STDERR, "%s: Drawing ROI %s \n", FuncName, ROI->Label);
               switch (ROI->Type) { /* ROI types */
                  case SUMA_ROI_EdgeGroup:
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, ROI_EdgeGroup);
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
                     break;
                  case SUMA_ROI_NodeGroup:
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, ROI_NodeGroup);
                     for (ii=0; ii < ROI->N_ElInd; ++ii) {
                        id = 3 * ROI->ElInd[ii];
                        glTranslatef (SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]);
                        gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad*SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06), 
                           SO->NodeMarker->slices, SO->NodeMarker->stacks);
                        glTranslatef (-SO->NodeList[id], -SO->NodeList[id+1], -SO->NodeList[id+2]);
                     }
                     break;
                  case SUMA_ROI_FaceGroup:   
                     glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, ROI_FaceGroup);
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
         XmToggleButtonSetState (SUMAg_CF->X->DrawROI->AfniLink_tb, SUMAg_CF->ROI2afni, NOPE);
      }
   }
   
   if (SUMAg_CF->ROI2afni) {
      SUMA_LH("Should send nels to AFNI...");
      if (N_nelv) {
         for (ii=0; ii < N_nelv; ++ii) {
            SUMA_LH("Send this nel to AFNI.");
            /* SUMA_ShowNel((void*)nelv[ii]);*/
            if (NI_write_element( SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nelv[ii] , NI_BINARY_MODE ) < 0) {
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
   float FillColor[3]={0.0, 0.0, 0.0};
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
   SUMA_Boolean LocalHead = NOPE;
            
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   /* select the color map */
   {
      eee = getenv("SUMA_ROIColorMap");
      if (eee) {
         if (!strcmp(eee, "roi256")) {
            mapname = "ROI_256";
            if (!iwarn) SUMA_S_Note( "roi256 colormap is now ROI_256\n"
                           "To use old roi256, use ygbrp256");
            ++iwarn;   
         }else if (!strcmp(eee, "roi128")) {
            mapname = "ROI_128";
            if (!iwarn) SUMA_S_Note( "roi128 colormap is now ROI_128\n"
                           "To use old roi128, use ygbrp128"); 
            ++iwarn;   
         }else if (!strcmp(eee, "roi64")) {
            mapname = "ROI_64";
            if (!iwarn) SUMA_S_Note( "roi64 colormap is now ROI_64\n"
                           "To use old roi64, use ygbrp64"); 
            ++iwarn;   
         }else if (SUMA_StandardMapCode(eee) >= 0) {
            mapname = eee;
         } else {
            mapname = "ROI_64";
            if (LocalHead) 
               fprintf(SUMA_STDERR, "%s: Unrecognized colormap %s.\n"
                                    " Using %s instead.\n", 
                                    FuncName, eee, mapname);
         }
      } else {
         mapname = "ROI_64";
         if (LocalHead) 
            fprintf(SUMA_STDERR,
               "%s: Undefined environment. Using default ROI colormap %s\n", 
               FuncName, mapname);
      }
   }
   if (LocalHead) {
      int N_tmp;
      char *nm_tmp = 
         SUMA_StandardMapName (SUMA_StandardMapCode(mapname), &N_tmp);
         fprintf(SUMA_STDERR,
            "%s: mapcode = %d, named %s %d cols\n", 
            FuncName, SUMA_StandardMapCode(mapname), nm_tmp, N_tmp);
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
      if (D_ROI && SUMA_isdROIrelated (D_ROI, SO)) {
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
         i_D_ROI = (int)(NextROIElm->data);
         if (LocalHead) fprintf (SUMA_STDERR, 
                                 "%s: Working with DO %d/%d.\n",
                                 FuncName,  i_D_ROI, N_do);  
         D_ROI = (SUMA_DRAWN_ROI *) dov[i_D_ROI].OP;
         
         /* Set the fillcolor */
         if (D_ROI->ColorByLabel) {
            if (!SUMAg_CF->ROI_CM) {
               if (!(SUMAg_CF->ROI_CM = SUMA_FindNamedColMap (mapname))) {
                  SUMA_SLP_Err( "Failed to create\n"
                                 "color map. Reverting\n"
                                 "to FillColors");
                  D_ROI->ColorByLabel = NOPE;
               }
               if (LocalHead) {
                  fprintf (SUMA_STDERR,
                           "%s:\nHave colormap of code %d, %d colors.\n",
                           FuncName, SUMA_StandardMapCode(mapname),
                           SUMAg_CF->ROI_CM->N_Col);
               }
               /* if connected to AFNI, send color map */
               if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] &&
                   SUMAg_CF->ROI2afni) {
                  int mapcode = -1;
                  list = SUMA_CreateList();
                  ED = SUMA_InitializeEngineListData (SE_SendColorMapToAfni);
                  mapcode = SUMA_StandardMapCode(mapname);
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
         } 
         
         /* make sure Color ByLabel is possible */
         if (D_ROI->ColorByLabel) {
            if (D_ROI->iLabel < 0 || D_ROI->iLabel >= SUMAg_CF->ROI_CM->N_Col) {
               SUMA_SLP_Err(  "ROI iLabel < 0 or \n"
                              "higher than the number\n"
                              "of colors in the map.\n"
                              "Reverting to FillColors");
               D_ROI->ColorByLabel = NOPE;
            }
         }
         
         if (D_ROI->ColorByLabel) {
            if (D_ROI->iLabel < 0 || D_ROI->iLabel >= SUMAg_CF->ROI_CM->N_Col) {
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
         } else {
            SUMA_COPY_VEC (D_ROI->FillColor, FillColor, 3,float, float);
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
               if (!N_ColHist[inode]) {
                  r[inode] = D_ROI->FillColor[0];
                  g[inode] = D_ROI->FillColor[1];
                  b[inode] = D_ROI->FillColor[2];
                  if (*CreateNel) ilab[inode] = D_ROI->iLabel;
                  ++N_NewNode;
               } else { /* already used up color, add new color */
                  SUMA_LH("Revisiting Color");
                  r[inode] = r[inode] + D_ROI->FillColor[0];
                  g[inode] = g[inode] + D_ROI->FillColor[1];
                  b[inode] = b[inode] + D_ROI->FillColor[2];
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
                  dlist_tail(Plane->ROI_index_lst), (void *)idov);
   
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
   float origwidth = 0.0;
   SUMA_CrossHair* Ch = sv->Ch;
   
   SUMA_ENTRY;
   
   fac = SUMA_MAX_PAIR(sv->ZoomCompensate, 0.03);
   radsph = Ch->sphrad*fac*sqrt(SUMA_sv_fov_original(sv)/FOV_INITIAL);
   gapch = Ch->g*fac;
   radch = Ch->r*fac;
   if (gl_dt = glIsEnabled(GL_DEPTH_TEST)) 
      glDisable(GL_DEPTH_TEST); /*cross hair always on top */
   glGetFloatv(GL_LINE_WIDTH, &origwidth);
   glLineWidth(Ch->LineWidth);
   /*fprintf(SUMA_STDOUT, "Center: %f, %f, %f. Gap %f, Radius: %f\n",\
      Ch->c[0], Ch->c[2], Ch->c[2], gapch, radch);*/
      glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); /* turn off ambient and diffuse components */
      glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
      if (gapch) { /* gap */
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->XaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0] - radch, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] - gapch, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] + radch, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] + gapch, Ch->c[1], Ch->c[2]);
         glEnd();  

         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->YaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1] - radch, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] - gapch, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] + radch, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] + gapch, Ch->c[2]);
         glEnd();  

         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->ZaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - radch);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - gapch);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + radch);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + gapch);
         glEnd();  

      }/*gap */ else {/*no gap */
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->XaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0] - radch, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] + radch, Ch->c[1], Ch->c[2]);
         glEnd();  
         
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->YaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1] - radch, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] + radch, Ch->c[2]);
         glEnd();  

         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->ZaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - radch);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + radch);
         glEnd();  
      }
      glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity for axis*/

   
   if (Ch->ShowSphere) {
      /*fprintf(SUMA_STDOUT, "SHOWING SPHERE\n");*/
      glMaterialfv(GL_FRONT, GL_EMISSION, Ch->sphcol); /*turn on emissivity for sphere */
      glTranslatef (Ch->c[0], Ch->c[1],Ch->c[2]);
      gluSphere(Ch->sphobj, radsph, Ch->slices, Ch->stacks);
      glTranslatef (-Ch->c[0], -Ch->c[1],-Ch->c[2]);
      glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity for axis*/
   }
   
   glLineWidth(origwidth);
   if (gl_dt) glEnable(GL_DEPTH_TEST);
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
   
   SUMA_ENTRY;

   if (Ch->sphobj) gluDeleteQuadric(Ch->sphobj);
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
   
   SUMA_ENTRY;

   if (SM->sphobj) gluDeleteQuadric(SM->sphobj);
   if (SM) SUMA_free(SM);
   SUMA_RETURNe;
}

/*! Create the highlighted faceset  marker */
SUMA_Boolean SUMA_DrawFaceSetMarker (SUMA_FaceSetMarker* FM, SUMA_SurfaceViewer *sv)
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
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
      
   SUMA_LH("Poly Mode");
   
   if (  SurfObj->PolyMode == SRM_Hide || 
         sv->PolyMode == SRM_Hide) { 
      SUMA_LH("Hiding surface"); 
      SUMA_RETURNe; 
   }
   
   /* check on rendering mode */
   if (SurfObj->PolyMode != SRM_ViewerDefault) {
     /* not the default, do the deed */
     SUMA_SET_GL_RENDER_MODE(SurfObj->PolyMode); 
   }

   if (SurfObj->texnel) {
      SUMA_LH(  "Creating texture, see init pp 415 in \n"
                "OpenGL programming guide, 3rd ed.");
      NI_GET_INTv(SurfObj->texnel,"box_size", sz, 2, LocalHead);

      /* For cool textures, see 
         http://www.filterforge.com/filters/category42-page1.html */
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
      NI_GET_INT(SurfObj->texnel,"texName",texName);
      if (!NI_GOT) {
         /* Need to generate texture */
         glGenTextures(1, &texName);
         /* Now store it */
         NI_SET_INT(SurfObj->texnel,"texName",texName);
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
                     GL_UNSIGNED_BYTE, SurfObj->texnel->vec[0]);
      glTexEnvf(  GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, 
                  SUMA_NIDO_TexEnvMode(SurfObj->texnel, GL_MODULATE));

      /* texture goes on surface which will be drawn later, 
         Set automatic texture coordinate generation */
      glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, 
                  SUMA_NIDO_TexCoordGen(SurfObj->texnel)); 
            /* GL_SPHERE_MAP, GL_EYE_LINEAR, GL_OBJECT_LINEAR */
      glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, 
                  SUMA_NIDO_TexCoordGen(SurfObj->texnel));
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
                     glMaterialf(GL_FRONT, GL_SHININESS, 64.0); 
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
         glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE); 
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
                  /* ZSS: Used to be: 
                  glColorPointer (  4, GL_FLOAT, 0, 
                                    SUMA_GetColorList (sv, 
                                       SurfObj->idcode_str));
                 A redundant call, to say the least! */
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
         
        
         if (SurfObj->texnel) {
            /* kill baby kill */
            SurfObj->texnel = NULL; /* don't leave this function 
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
         if (SurfObj->ShowSelectedNode && SurfObj->SelectedNode >= 0) {
            if (LocalHead) fprintf(SUMA_STDOUT,"Drawing Node Selection \n");
            id = ND * SurfObj->SelectedNode;
            glMaterialfv(GL_FRONT, GL_EMISSION, SurfObj->NodeMarker->sphcol); 
                  /*turn on emissidity for sphere */
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
            glTranslatef ( SurfObj->NodeList[id], 
                           SurfObj->NodeList[id+1],SurfObj->NodeList[id+2]);
            gluSphere(  SurfObj->NodeMarker->sphobj,
                        SurfObj->NodeMarker->sphrad *          
                           (SUMA_sv_fov_original(sv)/FOV_INITIAL) *  
                           SUMA_MAX_PAIR(sv->ZoomCompensate, 0.06),
                        SurfObj->NodeMarker->slices, 
                        SurfObj->NodeMarker->stacks);
            glTranslatef ( -SurfObj->NodeList[id], 
                           -SurfObj->NodeList[id+1],
                           -SurfObj->NodeList[id+2]);
            glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); 
                     /*turn off emissidity for axis*/
         }
         
         /* Draw Selected FaceSet Highlight */
         if (SurfObj->ShowSelectedFaceSet && SurfObj->SelectedFaceSet >= 0) {
            if (LocalHead) fprintf(SUMA_STDOUT,"Drawing FaceSet Selection \n");            
            if (!SUMA_DrawFaceSetMarker (SurfObj->FaceSetMarker, sv)) {
               fprintf(SUMA_STDERR,
                  "Error SUMA_DrawMesh: Failed in SUMA_DrawFaceSetMarker\b");
            }
         } 
         break;

   } /* switch DRAW_METHOD */
   
   SUMA_LH("RenderMode");
   /* reset viewer default rendering modes */
   if (SurfObj->PolyMode != SRM_ViewerDefault) {
     /* not the default, do the deed */
     SUMA_SET_GL_RENDER_MODE(sv->PolyMode); 
   }   
   
   SUMA_LH("Done");
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
   
   /* freeing Label */
   if (SO->Label) SUMA_free(SO->Label);
   
   /* freeing EL,  make sure that there are no links to EL*/
   if (SO->EL) {
      SUMA_free_Edge_List (SO->EL);
   }
   SO->EL = NULL;
   
   if (SO->MF){ 
      SUMA_Free_MemberFaceSets (SO->MF);
      SO->MF = NULL;
   }
   if (SO->SurfCont) SUMA_FreeSurfContStruct(SO->SurfCont);
   
   if (SO->PermCol) SUMA_free(SO->PermCol);
   
   if (SO->VolPar) SUMA_Free_VolPar(SO->VolPar); 
   
   if (SO->aSO) SO->aSO = SUMA_FreeAfniSurfaceObject(SO->aSO);
   
   if (SO->texnel) SO->texnel = NULL;
   
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
      if (SO->AnatCorrect) SS = SUMA_StringAppend (SS,"Anatomically correct = YES\n");
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
      
      sprintf (stmp,"N_Node: %d\t NodeDim: %d, EmbedDim: %d\n", \
         SO->N_Node, SO->NodeDim, SO->EmbedDim);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"RotationWeight: %d, ViewCenterWeight %d\n",  
                     SO->RotationWeight, SO->ViewCenterWeight);
      SS = SUMA_StringAppend (SS,stmp);
      sprintf (stmp,"N_FaceSet: %d, FaceSetDim %d\n", SO->N_FaceSet, 
                     SO->FaceSetDim);
      SS = SUMA_StringAppend (SS,stmp);
      
      SUMA_EULER_SO(SO, eu);
      SS = SUMA_StringAppend_va (SS, "Euler No. = %d\n\n", eu);
      
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
      
      if (SO->PolyArea == NULL) {
         sprintf (stmp,"SO->texnel = NULL\n\n") ;
         SS = SUMA_StringAppend (SS,stmp);
      } else {
         sprintf (stmp,"SO->texnel = an element named %s\n\n", 
                  SO->texnel->name) ;
         SS = SUMA_StringAppend (SS,stmp);
      }
      
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
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_SurfaceObject_Info.\n", FuncName);
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
      } else {
         SO->NodeNormList = NULL;
      }

      SO->aSO = aSO;
      *aSOp = NULL; /* allow no one to touch this anymore */

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

      /* Is there an xform to apply ? */
      SUMA_LH("Dealing with Xforms");
      if (!SUMA_GetSOCoordXform(SO, xform)) {
         SUMA_S_Err("Failed to get xform!");
         NI_SET_INT(nelxyz,"inxformspace",0);
      } else {
         if (!SUMA_Apply_Coord_xform(SO->NodeList, SO->N_Node, SO->NodeDim,
                                     xform, 0, NULL)) {
            SUMA_S_Err("Failed to apply xform!");
            NI_SET_INT(nelxyz,"inxformspace",0);
         }else{
            NI_SET_INT(nelxyz,"inxformspace",1);
         }
      }
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
      NI_SET_INT(nelxyz,"inxformspace",  0);
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
      SO[i].ShowSelectedNode = NOPE;
      SO[i].Name_NodeParent = NULL;
      SO[i].Label = NULL;
      SO[i].EmbedDim = 3;
      SO[i].Center[0] = SO[i].Center[1] = SO[i].Center[2] = 0.0;           /* the zeros in Center, MaxDims, MinDims, */ 
      SO[i].MaxDims[0] = SO[i].MaxDims[1] = SO[i].MaxDims[2] = 0.0;        /* aMinDims and aMaxDims */
      SO[i].MinDims[0] = SO[i].MinDims[1] = SO[i].MinDims[2] = 0.0;        /* are used to flag unitialized parameters */
      SO[i].aMinDims = 0.0;                                                /* always keep zero for initialization */      
      SO[i].aMaxDims = 0.0;                                                /* see SUMA_isSODimInitialized */
      SO[i].ViewCenterWeight = -1;
      SO[i].RotationWeight = -1;
      SO[i].patchaMaxDims = 0.0;
      SO[i].patchaMinDims = 0.0;
      SO[i].patchMinDims[0] = SO[i].patchMinDims[1] = SO[i].patchMinDims[2] = 0.0;
      SO[i].patchMaxDims[0] = SO[i].patchMaxDims[1] = SO[i].patchMaxDims[2] = 0.0;
      SO[i].patchCenter[0] = SO[i].patchCenter[1] = SO[i].patchCenter[2] = 0.0;
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
      SO[i].ShowSelectedFaceSet = -1;
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
      SO[i].SurfCont = NULL; /* This is now handled in SUMA_LoadSpec_eng (used to be SUMA_CreateSurfContStruct();) */
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
      
      SO[i].texnel = NULL;
     }
   SUMA_RETURN(SO);
}/* SUMA_Alloc_SurfObject_Struct */

SUMA_Boolean SUMA_isSODimInitialized(SUMA_SurfaceObject *SO) 
{
   if (!SO) return(NOPE);
   
   if (  SO->MaxDims[0] == 0.0 && SO->MaxDims[1] == 0.0 && SO->MaxDims[2] == 0.0 &&
         SO->MinDims[0] == 0.0 && SO->MinDims[1] == 0.0 && SO->MinDims[2] == 0.0 &&
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
   SUMA_LHv("Min:[%f %f %f]\n"
            "Max:[%f %f %f]\n"
            "aMax: %f, aMin %f\n",
            SO->MinDims[1], SO->MinDims[2],SO->MinDims[3],
            SO->MaxDims[1], SO->MaxDims[2],SO->MaxDims[3],
            SO->aMaxDims, SO->aMinDims );
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
   } else {
      D_ROI->ColPlaneName = SUMA_copy_string("DefROIpl");
   }
   D_ROI->FillColor[0] = 1.0; 
   D_ROI->FillColor[1] = 0.0; 
   D_ROI->FillColor[2] = 0.0;
   D_ROI->EdgeColor[0] = 0.0; 
   D_ROI->EdgeColor[1] = 0.0; 
   D_ROI->EdgeColor[2] = 1.0;
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
void SUMA_ReportDrawnROIDatumLength(SUMA_SurfaceObject *SO, SUMA_ROI_DATUM *ROId, FILE *out, SUMA_WIDGET_INDEX_DRAWROI_WHATDIST option)
{
   static char FuncName[]={"SUMA_ReportDrawnROIDatumLength"};
   int N0, N1, i, N_n, *nPath, N_left;
   SUMA_Boolean *isNodeInMesh = NULL;
   float *p1, *p2;
   float ds = 0, d = 0, ds_c, dd, dd_c;
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
         "#n0\tn1\tN_n\td\td_c\tds\tds_c\n"
         "%d\t%d\t%d\t%.2f\t%.2f\t%.2f\t%.2f\n", 
         SO->Label, ROId->nPath[0], ROId->nPath[ROId->N_n - 1], 
         ROId->N_n, ds, ds_c, dd, dd_c);
   } else if (option == SW_DrawROI_WhatDistTrace) {
      SS = SUMA_StringAppend_va(SS,
         "#Distances on %s\n"
         "#n0\tn1\tN_n\td\td_c\n"
         "%d\t%d\t%d\t%.2f\t%.2f\n", 
         SO->Label, ROId->nPath[0], ROId->nPath[ROId->N_n - 1], 
         ROId->N_n, ds, ds_c);
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
void SUMA_ShowDrawnROI (SUMA_DRAWN_ROI *D_ROI, FILE *out, SUMA_Boolean ShortVersion)
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
   \param FillColor (float[3])
   \param EdgeColor (float[3])
   \param EdgeThickness (int)
   \param ForDisplay (SUMA_Boolean) YUP: Prepare ROI for display
                                    (creates the contour for the ROI,
                                    requires that Parent surface object be loaded)
   \return ROI (SUMA_DRAWN_ROI *)
   
   - No DO/Undos possible in this format
*/

SUMA_DRAWN_ROI * SUMA_1DROI_to_DrawnROI ( int *Node, int N_Node, int Value, char *Parent_idcode_str, 
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
   SUMA_COPY_VEC(EdgeColor, ROI->EdgeColor, 3, float, float);
   SUMA_COPY_VEC(FillColor, ROI->FillColor, 3, float, float);
   ROI->EdgeThickness = EdgeThickness;
   
   /* fill in the only ROI datum */
   ROI_Datum = SUMA_AllocROIDatum ();
   ROI_Datum->action = SUMA_BSA_Undefined;
   if(LocalHead) fprintf (SUMA_STDERR,"%s: About to add %d nodes of value %d...\n", FuncName, N_Node, Value);
   
   ROI_Datum->nPath = SUMA_UniqueInt(Node, N_Node, &ROI_Datum->N_n, NOPE);
   if (!ROI_Datum->nPath) {
      SUMA_SLP_Crit("Failed to allocate");
      SUMA_RETURN(NOPE);
   }
   ROI_Datum->Type = SUMA_ROI_NodeGroup;
   
   SUMA_LH("Appending stroke");
   /* just append that baby */
   dlist_ins_next(ROI->ROIstrokelist, dlist_tail(ROI->ROIstrokelist), (void *)ROI_Datum);
   
   if (ForDisplay) {
      /* You must find the contour by yourself. This is normally
      done when the status is set to SUMA_ROI_Finished via the 
      action stack functions */
      { 
         int *cNodes, N_cNodes;
         SUMA_Boolean Unique = NOPE;

         SUMA_LH("Getting Contour ");
         N_cNodes = 0;
         Unique = NOPE; /* Set to YUP if you have node indices listed more than once. 
                           1D ROIs are uniquized in the reading functions*/
         cNodes = SUMA_NodesInROI (ROI, &N_cNodes, Unique);
         if (cNodes) {
            ROI->CE = SUMA_GetContour (
                           SUMA_findSOp_inDOv(ROI->Parent_idcode_str, dov, N_dov), 
                           cNodes, N_cNodes, &(ROI->N_CE), 0, NULL);
            if (!ROI->CE) { SUMA_LH("Null DrawnROI->CE"); }
            else { SUMA_LH("Good DrawnROI->CE"); }
            SUMA_free(cNodes);
         }
      }
   }
   SUMA_RETURN(ROI);
}
