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
   
   nsoopt = (SUMA_NEW_SO_OPT *) SUMA_malloc(sizeof(SUMA_NEW_SO_OPT));
   nsoopt->idcode_str = NULL;
   nsoopt->LocalDomainParentID = NULL;
   nsoopt->FileFormat = SUMA_ASCII;
   nsoopt->FileType = SUMA_FT_NOT_SPECIFIED;
   
   SUMA_RETURN(nsoopt);
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
SUMA_SurfaceObject *SUMA_NewSO(float **NodeList, int N_Node, int **FaceSetList, int N_FaceSet, SUMA_NEW_SO_OPT *nsooptu)
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
   SO->NodeList = *NodeList; *NodeList = NULL;  /* keeps user from freeing afterwards ... */
   SO->N_Node = N_Node;
   
   SUMA_LH("Center deal")
   SUMA_DIM_CENTER(SO);
   
   SUMA_LH("FaceSetList");
   SO->FaceSetDim = 3;
   SO->FaceSetList = *FaceSetList; *FaceSetList = NULL;  /* keeps user from freeing afterwards ... */
   SO->N_FaceSet = N_FaceSet;
   
   SUMA_LH("Metrics");
   if (!SUMA_SurfaceMetrics(SO, "EdgeList, MemberFace", NULL)) {
      SUMA_SL_Warn("Failed to compute metrics\nReturing with whatever is salvageable");
   }
      
   SUMA_LH("Normals");
   SUMA_RECOMPUTE_NORMALS(SO);
   
   SUMA_LH("trimmings");
   SO->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));  
   if (nsoopt->idcode_str) sprintf(SO->idcode_str, "%s", nsoopt->idcode_str);
   else UNIQ_idcode_fill (SO->idcode_str);
   if (nsoopt->LocalDomainParentID) SO->LocalDomainParentID = SUMA_copy_string(nsoopt->LocalDomainParentID);
   SO->LocalDomainParentID = SUMA_copy_string(SO->idcode_str);
   
   /* the stupid copies */
   SO->glar_NodeList = (GLfloat *)SO->NodeList;
   SO->glar_FaceSetList = (GLint *) SO->FaceSetList;
   SO->glar_NodeNormList = (GLfloat *) SO->NodeNormList; 
   SO->glar_FaceNormList = (GLfloat *) SO->FaceNormList; 
   
   if (nsooptu != nsoopt) {
      SUMA_free(nsoopt); nsoopt=NULL; 
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
   if (!NodeList && !FaceSetList && replace ) { SUMA_SL_Err("Nothing to do"); }

   if (NodeList) { 
      if (N_Node != SO->N_Node) {
         SUMA_SL_Err("Not ready for partial node lists.\n");
         SUMA_RETURN(NULL);
      }  
   }

   if (replace) { SUMA_LH("Reusing old surface"); SOn = SO; }
   else { SUMA_LH("New Surface"); SOn =  SUMA_Alloc_SurfObject_Struct(1); } 

   if (NodeList) {
      SUMA_LH("New Node List");
      SOn->NodeDim = SO->NodeDim;
      SOn->NodeList = NodeList; SOn->N_Node = N_Node;
      SUMA_LH("Recalculating center");
      SUMA_DIM_CENTER(SOn);
      RedoNormals = YUP;
   } else {
      if (!replace) {
         SUMA_LH("Copying old node list");
         SOn->NodeDim = SO->NodeDim;
         SOn->N_Node = SO->N_Node;
         SOn->NodeList = (float *)SUMA_malloc(SOn->N_Node*3*sizeof(float));
         if (!SOn->NodeList) { SUMA_SL_Crit("Failed to allocate."); SUMA_RETURN(NULL); }
         SUMA_COPY_VEC(SO->NodeList, SOn->NodeList, SOn->N_Node*3, float, float);
         RedoNormals = YUP;
      }
   }
 
   if (FaceSetList) {
      SUMA_LH("New FaceSet List");
      SOn->FaceSetList = FaceSetList; SOn->N_FaceSet = N_FaceSet; SOn->FaceSetDim = SO->FaceSetDim;
      /* Need a new edge list */
      if (!SUMA_SurfaceMetrics(SOn, "EdgeList, MemberFace", NULL)) {
         SUMA_SL_Warn("Failed to compute metrics\nReturing with whatever is salvageable");
      }
      RedoNormals = YUP;
   } else {
      if (!replace) {
         SUMA_LH("Copying old FaceSet list");
         SOn->N_FaceSet = SO->N_FaceSet;
         SOn->FaceSetDim = SO->FaceSetDim;
         SOn->FaceSetList = (int *)SUMA_malloc(SOn->N_FaceSet*SOn->FaceSetDim*sizeof(int));
         if (!SOn->FaceSetList) { SUMA_SL_Crit("Failed to allocate."); SUMA_RETURN(NULL); }
         SUMA_COPY_VEC(SO->FaceSetList, SOn->FaceSetList, SOn->N_FaceSet*SOn->FaceSetDim, int, int);
         RedoNormals = YUP;
         /* Need to inherit edge list */
         if (0 &&!SUMA_SurfaceMetrics(SOn, "EdgeList, MemberFace", SO)) {
            SUMA_SL_Warn("Failed to compute metrics\nReturing with whatever is salvageable");
         }
      }
   }

   if (RedoNormals) {
      SUMA_LH("Recalculating normals and convexitation");
      SUMA_RECOMPUTE_NORMALS(SOn);
      if (0 &&!SUMA_SurfaceMetrics(SOn, "Convexity", SO)) {
         SUMA_SL_Warn("Failed to compute metrics\nReturing with whatever is salvageable");
      }
   }

   if (!replace) {
      SUMA_LH("New IDs");
      SOn->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));  
      UNIQ_idcode_fill (SOn->idcode_str);
      SOn->LocalDomainParentID = SUMA_copy_string(SO->LocalDomainParentID);
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
            hp[i+1] = Cmap->frac[i] * (topright[1] - orig[1]);
         } else {
            hp[i+1] = (1.0 +Cmap->frac[i]) / 2.0 * (topright[1] - orig[1]);
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
   static char FuncName[]={"SUMA_Alloc_SegmentDO"};
   SUMA_SegmentDO * SDO= NULL;

   SUMA_ENTRY;
   
   SDO = (SUMA_SegmentDO *) SUMA_malloc (sizeof (SUMA_SegmentDO));
   if (!SDO) {
         fprintf(stderr,"Error %s: Failed to allocate for SDO\n", FuncName);
         SUMA_RETURN (SDO);
   }
   if (N_n > 0) {
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
      SDO->Label = (char *)SUMA_calloc (strlen(Label)+1, sizeof(char));
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

   SUMA_ENTRY;
   
   if (!SDO) SUMA_RETURNe;
   
   if (SDO->n0) SUMA_free(SDO->n0);
   if (SDO->n1) SUMA_free(SDO->n1);
   if (SDO->idcode_str) SUMA_free(SDO->idcode_str);
   if (SDO->Label) SUMA_free(SDO->Label);
   if (SDO) SUMA_free(SDO);
   
   SUMA_RETURNe;
}

SUMA_SegmentDO * SUMA_ReadSegDO (char *s)
{
   static char FuncName[]={"SUMA_ReadSegDO"};
   SUMA_SegmentDO *SDO = NULL;
   MRI_IMAGE * im = NULL;
   float *far=NULL;
   int itmp, itmp2;
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
   if (nrow !=  6 ) {
      SUMA_SLP_Err("File must have\n"
                   "6 columns.");
      mri_free(im); im = NULL;   /* done with that baby */
      SUMA_RETURN(NULL);
   }/* find out if file exists and how many values it contains */

   /* allocate for segments DO */
   SDO = SUMA_Alloc_SegmentDO (ncol, s);
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

   mri_free(im); im = NULL; far = NULL;

   SUMA_RETURN(SDO);
}


/*! Allocate for a axis object */
SUMA_Axis* SUMA_Alloc_Axis (const char *Name)
{   
   static char FuncName[]={"SUMA_Alloc_Axis"};
   SUMA_Axis* Ax;

   SUMA_ENTRY;

   Ax = (SUMA_Axis *) SUMA_malloc (sizeof (SUMA_Axis));
   if (Ax == NULL) {
      fprintf(stderr,"SUMA_Alloc_Axis Error: Failed to allocate Ax\n");
      SUMA_RETURN (Ax);
   }
   
   /* setup some default values */
   Ax->type = SUMA_STD_ZERO_CENTERED;
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
         Ax->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH+1, sizeof(char));
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
   
   SUMA_ENTRY;

   if (Ax->Name != NULL) SUMA_free(Ax->Name);
   if (Ax->idcode_str != NULL) SUMA_free(Ax->idcode_str);
   if (Ax) SUMA_free(Ax);
   SUMA_RETURNe;
}

void SUMA_EyeAxisStandard (SUMA_Axis* Ax, SUMA_SurfaceViewer *csv)
{
   static char FuncName[]={"SUMA_EyeAxisStandard"};
   SUMA_Boolean LocalHead = NOPE;
   
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
   static char FuncName[]={"SUMA_EyeAxisStandard"};
   
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

SUMA_Boolean SUMA_DrawSegmentDO (SUMA_SegmentDO *SDO)
{
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   int i, N_n3;
   static char FuncName[]={"SUMA_DrawSegmentDO"};
   
   SUMA_ENTRY;
   
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
   glMaterialfv(GL_FRONT, GL_EMISSION, SDO->LineCol); /*turn on emissivity for */
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
   
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity */

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
   if (Ax->type != SUMA_SCALE_BOX) {
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
   if (LocalHead) { fprintf (SUMA_STDERR,"%s: sv->Pcenter_close = [%f %f %f]\n", FuncName, sv->Pcenter_close[0], sv->Pcenter_close[1], sv->Pcenter_close[2]); }
   ASI = (SUMA_AxisSegmentInfo **) SUMA_malloc( 12 * sizeof(SUMA_AxisSegmentInfo *));

   for (j=0; j<12; ++j) {
      ASI[j] = (SUMA_AxisSegmentInfo *) SUMA_malloc(sizeof(SUMA_AxisSegmentInfo )); ASIp = ASI[j];
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

   list = (DList *)SUMA_malloc(sizeof(DList));
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
   
   switch (Ax->type) {
      case SUMA_STD_ZERO_CENTERED:
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
         dlist_destroy(slist);
         SUMA_free(slist); slist = NULL;
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
   glRasterPos3f(Ps[0], Ps[1], Ps[2]);
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
   glMaterialfv(GL_FRONT, GL_EMISSION, NoColor);  /*turn off emissidity for text*/ 
      
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
   glVertex3f(ASIp->P1[0], ASIp->P1[1], ASIp->P1[2]);
   glVertex3f(ASIp->P2[0], ASIp->P2[1], ASIp->P2[2]);

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
      if (LocalHead) fprintf(SUMA_STDERR,"%s:\nspace = %f\nsize = %f\n", FuncName, space[jj], size[jj]);
      if (Ax->DoCross) {
         size[jj] /= 2.0;
         while (i*space[jj] < nu3) {
            Ps[0] = i*space[jj]*u3[0] + Pt[0]; Ps[1] = i*space[jj]*u3[1] + Pt[1]; Ps[2] = i*space[jj]*u3[2] + Pt[2]; /* center */
            #if 0 
               if (LocalHead) fprintf(SUMA_STDERR,"%s:\nPs = [%f %f %f]; \n", FuncName, Ps[0], Ps[1], Ps[2]);
            #endif
            glVertex3f(Ps[0]-ASIp->tick1_dir[0]*size[jj], Ps[1]-ASIp->tick1_dir[1]*size[jj], Ps[2]-ASIp->tick1_dir[2]*size[jj]);
            glVertex3f(Ps[0]+ASIp->tick1_dir[0]*size[jj], Ps[1]+ASIp->tick1_dir[1]*size[jj], Ps[2]+ASIp->tick1_dir[2]*size[jj]);
            glVertex3f(Ps[0]-ASIp->tick2_dir[0]*size[jj], Ps[1]-ASIp->tick2_dir[1]*size[jj], Ps[2]-ASIp->tick2_dir[2]*size[jj]);
            glVertex3f(Ps[0]+ASIp->tick2_dir[0]*size[jj], Ps[1]+ASIp->tick2_dir[1]*size[jj], Ps[2]+ASIp->tick2_dir[2]*size[jj]);
            ++i;
         }
      } else {
         while (i*space[jj] < nu3) {
            Ps[0] = i*space[jj]*u3[0] + Pt[0]; Ps[1] = i*space[jj]*u3[1] + Pt[1]; Ps[2] = i*space[jj]*u3[2] + Pt[2]; /* center */
            #if 0 
              if (LocalHead) fprintf(SUMA_STDERR,"%s:\nPs = [%f %f %f]; \n", FuncName, Ps[0], Ps[1], Ps[2]);
            #endif
            glVertex3f(Ps[0], Ps[1], Ps[2]);
            glVertex3f(Ps[0]+ASIp->tick1_dir[0]*size[jj], Ps[1]+ASIp->tick1_dir[1]*size[jj], Ps[2]+ASIp->tick1_dir[2]*size[jj]);
            glVertex3f(Ps[0], Ps[1], Ps[2]);
            glVertex3f(Ps[0]+ASIp->tick2_dir[0]*size[jj], Ps[1]+ASIp->tick2_dir[1]*size[jj], Ps[2]+ASIp->tick2_dir[2]*size[jj]);
               #if 0 /* for a little debug */
               if (jj==1) {
                  glVertex3f(Ps[0], Ps[1], Ps[2]);
                  txofffac = 1.0 * size[1];
                  Ps[0] = i*space[1]*u3[0] + Pt[0] + txofffac * ASIp->TxOff[0]; 
                  Ps[1] = i*space[1]*u3[1] + Pt[1] + txofffac * ASIp->TxOff[1]; 
                  Ps[2] = i*space[1]*u3[2] + Pt[2] + txofffac * ASIp->TxOff[2];
                  glVertex3f(Ps[0], Ps[1], Ps[2]);
               }
               #endif
            ++i;
         }
      }
      nTick[jj] = i-1;
      
      
   }
   
   glEnd();

   glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, NoColor); /*turn off emissivity for axis*/
   
         

   if (AddText) { /* do the text for major ticks only */
      float MinYstep, MinXstep, dSxT, dSyT, curXstep, curYstep;
      int OKnext;
      dSxT = (float)fabs(ASIp->screen_length_x) / (float)nTick[1];
      dSyT = (float)fabs(ASIp->screen_length_y) / (float)nTick[1];
      MinYstep = 15 ; /* height of letters in pixels (using GLUT_BITMAP_9_BY_15) */
      MinXstep = 9 * 5; /* length of string in pixels (around 5 chars, including sign, using %.1f )*/
      if (LocalHead) fprintf (SUMA_STDERR,"%s:\ndS = %f, %f\n", FuncName, dSxT, dSyT);
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
   
   SUMA_ENTRY;
   
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
                              gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
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
                              gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
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
                        gluSphere(SO->NodeMarker->sphobj, SO->NodeMarker->sphrad, SO->NodeMarker->slices, SO->NodeMarker->stacks);
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
            /* SUMA_ShowNel(nelv[ii]);*/
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
   float FillColor[3];
   int i, ii, N_NewNode = 0, istore, OverInd=-1, inode, i_D_ROI, LastOfPreSeg, N_Nodes=0;
   SUMA_OVERLAY_PLANE_DATA sopd;
   DListElmt *NextPlaneElm = NULL, *NextROIElm = NULL, *NextElm=NULL;
   SUMA_DRAWN_ROI *D_ROI = NULL;
   SUMA_ROI_DATUM *ROId=NULL;
   NI_element **nelv = NULL;
   SUMA_STANDARD_CMAP mapcode;
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_Boolean Unique = NOPE;
   SUMA_Boolean LocalHead = NOPE;
            
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   /* select the color map */
   {
      char *eee = getenv("SUMA_ROIColorMap");
      if (eee) {
         if (strcmp (eee, "bgyr64") == 0) {
            mapcode = SUMA_CMAP_BGYR64;
         } else if (strcmp (eee, "ygbrp64") == 0) {
            mapcode = SUMA_CMAP_ROI64;
         } else if (strcmp (eee, "roi64") == 0) {
            mapcode = SUMA_CMAP_ROI64;
         } else if (strcmp (eee, "ygbrp128") == 0) {
            mapcode = SUMA_CMAP_ROI128;
         } else if (strcmp (eee, "ygbrp256") == 0) {
            mapcode = SUMA_CMAP_ROI256;
         } else if (strcmp (eee, "roi128") == 0) {
            mapcode = SUMA_CMAP_ROI128;
         } else if (strcmp (eee, "roi256") == 0) {
            mapcode = SUMA_CMAP_ROI256;
         } else {
            mapcode = SUMA_CMAP_ROI128;
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Unrecognized option. Using default\n", FuncName);
         }
      } else {
         mapcode = SUMA_CMAP_ROI128;
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Undefined environment. Using default\n", FuncName);
      }
   }
   if (LocalHead) {
      int N_tmp;
      char *nm_tmp = SUMA_StandardMapName (mapcode, &N_tmp);
      fprintf(SUMA_STDERR,"%s: mapcode = %d, named %s %d cols\n", FuncName, mapcode, nm_tmp, N_tmp);
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
      nelv = (NI_element **) SUMA_calloc(dlist_size(ROIPlaneList), sizeof(NI_element *));
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
      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Processing plane %s\n", FuncName, Plane->name);
      
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
               if (!(SUMAg_CF->ROI_CM = SUMA_GetStandardMap (mapcode))) {
                  SUMA_SLP_Err( "Failed to create\n"
                                 "color map. Reverting\n"
                                 "to FillColors");
                  D_ROI->ColorByLabel = NOPE;
               }
               if (LocalHead) {
                  fprintf (SUMA_STDERR,"%s:\nHave colormap of code %d, %d colors.\n", FuncName, mapcode, SUMAg_CF->ROI_CM->N_Col);
               }
               /* if connected to AFNI, send color map */
               if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && SUMAg_CF->ROI2afni) {
                  list = SUMA_CreateList();
                  ED = SUMA_InitializeEngineListData (SE_SendColorMapToAfni);
                  if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                         SEF_i, (void*)&mapcode, 
                                                         SES_SumaWidget, NULL, NOPE, 
                                                         SEI_Head, NULL )) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
                     SUMA_RETURN(NOPE);
                  }
                  if (!SUMA_Engine (&list)) {
                     fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
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
      SUMA_free(N_ColHist); 
      SUMA_free(r); 
      SUMA_free(g); 
      SUMA_free(b);
      if (*CreateNel) SUMA_free(ilab);
      
   /* put the colors in a color plane */
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
      if (!SUMA_iRGB_to_OverlayPointer (SO, Plane->name, &sopd, &OverInd, dov, N_do, SUMAg_CF->DsetList)) {
         SUMA_SLP_Err("Failed to fetch or create overlay pointer.");
         SUMA_RETURN(NOPE);
      }      
      SUMA_LH("Returned SUMA_iRGB_to_OverlayPointer");
      
      if (*CreateNel) {
         NI_element *nel = NULL;
         char *TargetVol=NULL;
         
         /* form the nel for this plane */
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
            if (!SUMA_AddNelCol (nel, "node index", SUMA_NODE_INDEX, (void *)ivect, NULL, 1)) {
               SUMA_SL_Err("Failed in SUMA_AddNelCol");
               SUMA_RETURN(NOPE);
            }

            /* Add the label column */
            SUMA_LH("Adding label column...");
            if (!SUMA_AddNelCol (nel, "integer label", SUMA_NODE_ILABEL, (void *)labvect, NULL, 1)) {
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
         NI_set_attribute (nel, "volume_idcode", SO->VolPar->idcode_str);
         
         nelv[i] = nel; nel = NULL;
         
         /* DO NOT FREE ivect, it is used in sopd */
         SUMA_free(labvect);
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
      if (*CreateNel) labvect = NULL;  
   }
   
      
   SUMA_LH("Destroying list");
   /* destroy plane list */
   dlist_destroy (ROIPlaneList);

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
    
   Nodes = (int*)SUMA_malloc(N_max*sizeof(int));
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

void SUMA_Free_ROI_PlaneData (void *da)
{
   static char FuncName[]={"SUMA_Free_ROI_PlaneData"};
   SUMA_ROI_PLANE *pl = NULL;
   
   SUMA_ENTRY;
   
   pl = (SUMA_ROI_PLANE *)da;
   
   if (!pl) SUMA_RETURNe;
   
   /* destroy the list containing ROIs belonging to plane */
   if (pl->ROI_index_lst) dlist_destroy (pl->ROI_index_lst);
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
   SUMA_ROI_PLANE *Plane;
   int i;
   SUMA_Boolean found = NOPE, LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ROIplaneListIn) { /* initialization land */
      ROIplaneList = (DList *)SUMA_malloc(sizeof(DList));
      dlist_init (ROIplaneList, SUMA_Free_ROI_PlaneData);
      SUMA_RETURN(ROIplaneList);
   } else {
      ROIplaneList = ROIplaneListIn;
   }
   
   doel = &(dov[idov]);
   
   if (doel->ObjectType != ROIdO_type) {
      SUMA_SLP_Crit("Only planning to deal\n"
                   "with ROIdO_type type");
      dlist_destroy(ROIplaneList);
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
      Plane = (SUMA_ROI_PLANE *)SUMA_malloc(sizeof(SUMA_ROI_PLANE));
      Plane->name = UsedName; /* preserved, don't go freeing UsedName later! */
      Plane->ROI_index_lst = (DList *) SUMA_malloc(sizeof(DList));
      dlist_init(Plane->ROI_index_lst, NULL);
      dlist_ins_next(ROIplaneList, dlist_tail(ROIplaneList), (void *)Plane);
   }
   
   /* now put the ROI in question in that list, easiest is to store its index into dov */
   dlist_ins_next(Plane->ROI_index_lst, dlist_tail(Plane->ROI_index_lst), (void *)idov);
   
   /* OK, done, now return */
   SUMA_RETURN(ROIplaneList);
}

/*! Create the cross hair */
SUMA_Boolean SUMA_DrawCrossHair (SUMA_CrossHair* Ch)
{
   static char FuncName[]={"SUMA_DrawCrossHair"};
   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   
   SUMA_ENTRY;

   glLineWidth(Ch->LineWidth);
   /*fprintf(SUMA_STDOUT, "Center: %f, %f, %f. Gap %f, Radius: %f\n",\
      Ch->c[0], Ch->c[2], Ch->c[2], Ch->g, Ch->r);*/
      glMaterialfv(GL_FRONT, GL_AMBIENT, NoColor); /* turn off ambient and diffuse components */
      glMaterialfv(GL_FRONT, GL_DIFFUSE, NoColor);
      if (Ch->g) { /* gap */
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->XaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0] - Ch->r, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] - Ch->g, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] + Ch->r, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] + Ch->g, Ch->c[1], Ch->c[2]);
         glEnd();  

         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->YaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1] - Ch->r, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] - Ch->g, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] + Ch->r, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] + Ch->g, Ch->c[2]);
         glEnd();  

         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->ZaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - Ch->r);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - Ch->g);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + Ch->r);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + Ch->g);
         glEnd();  

      }/*gap */ else {/*no gap */
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->XaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0] - Ch->r, Ch->c[1], Ch->c[2]);
         glVertex3f(Ch->c[0] + Ch->r, Ch->c[1], Ch->c[2]);
         glEnd();  
         
         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->YaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1] - Ch->r, Ch->c[2]);
         glVertex3f(Ch->c[0], Ch->c[1] + Ch->r, Ch->c[2]);
         glEnd();  

         glMaterialfv(GL_FRONT, GL_EMISSION, Ch->ZaxisColor); /*turn on emissivity for axis*/
         glBegin(GL_LINES);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] - Ch->r);
         glVertex3f(Ch->c[0], Ch->c[1], Ch->c[2] + Ch->r);
         glEnd();  
      }
      glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissivity for axis*/

   
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
   
   SUMA_ENTRY;

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
   
   SUMA_ENTRY;

   if (Ch->sphobj) gluDeleteQuadric(Ch->sphobj);
   if (Ch) SUMA_free(Ch);
   SUMA_RETURNe;
}


/* Allocate for a SphereMarker object */
SUMA_SphereMarker* SUMA_Alloc_SphereMarker (void)
{   
   static char FuncName[]={"SUMA_SphereMarker"};
   SUMA_SphereMarker* SM;
   
   SUMA_ENTRY;

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
   
   SUMA_ENTRY;

   if (SM->sphobj) gluDeleteQuadric(SM->sphobj);
   if (SM) SUMA_free(SM);
   SUMA_RETURNe;
}

/*! Create the highlighted faceset  marker */
SUMA_Boolean SUMA_DrawFaceSetMarker (SUMA_FaceSetMarker* FM)
{   static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0}, dx, dy, dz;
   static char FuncName[]={"SUMA_DrawFaceSetMarker"};
   
   SUMA_ENTRY;

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
   
   SUMA_ENTRY;

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

   SUMA_ENTRY;

   if (FM) SUMA_free(FM);
   SUMA_RETURNe;
}

#define TestImage 0 
#define TestTexture 0 /* needs TestImage to be active */
/*! Create a tesselated mesh */
void SUMA_DrawMesh(SUMA_SurfaceObject *SurfObj, SUMA_SurfaceViewer *sv)
{  static GLfloat NoColor[] = {0.0, 0.0, 0.0, 0.0};
   GLfloat *colp = NULL;
   int i, ii, ND, id, ip, NP, PolyMode;
   static char FuncName[]={"SUMA_DrawMesh"};
   SUMA_DRAWN_ROI *DrawnROI = NULL;
      static unsigned char *image=NULL;
      static GLuint texName;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   #if TestImage
   if (image) {
      SUMA_SL_Note("Binding texture");
      glBindTexture(GL_TEXTURE_2D, texName);
   }
   #endif
      
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
         /* This allows each node to follow the color specified when it was drawn */ 
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
            glColorPointer (4, GL_FLOAT, 0, SUMA_GetColorList (sv, SurfObj->idcode_str));
         }
         glVertexPointer (3, GL_FLOAT, 0, SurfObj->glar_NodeList);
         glNormalPointer (GL_FLOAT, 0, SurfObj->glar_NodeNormList);
         /* fprintf(stdout, "Ready to draw Elements %d\n", SurfObj->N_FaceSet); */
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

         #if TestImage
         if (1){
            GLboolean valid;
            GLfloat rpos[4];
            char  string[]= {"Yo Baby sssup? 1 2 3, 4.2 mm"};
            int is;
            float txcol[3] = {0.2, 0.5, 1};
            static int width, height;

            SUMA_SL_Note(  "Doing the splat and the text thing\n"
                           "from Kilgard's renderSplat in splatlogo.c\n"
                           "to understand scaling operations.\n");
            
            if (SurfObj->ShowSelectedNode && SurfObj->SelectedNode >= 0) {
               id = ND * SurfObj->SelectedNode;
            } else { id = 0; }
            
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
            glMaterialfv(GL_FRONT, GL_EMISSION, txcol); /*turn on emissidity for text*/
            glRasterPos3f(SurfObj->NodeList[id], SurfObj->NodeList[id+1],SurfObj->NodeList[id+2]);
            glGetFloatv(GL_CURRENT_RASTER_POSITION, rpos);
            glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &valid);
            printf("%s: Raster position (%g,%g, %g) is %s\n",
               FuncName, rpos[0], rpos[1], rpos[2], valid ? "valid" : "INVALID");

            /* do some text action */
            SUMA_SL_Note(  "Some colored text\n"
                           "Might affect la drawing\n"
                           "color elsewhere");
            glColor3fv(txcol); 
            for (is=0; string[is] != '\0'; is++) {
               glutBitmapCharacter(GLUT_BITMAP_9_BY_15, string[is]);
            }  
            glMaterialfv(GL_FRONT, GL_EMISSION, NoColor); /*turn off emissidity for text*/

            if (!image) {
               FILE *fid;
               SUMA_SL_Note(  "Reading the image.");
               image = SUMA_read_ppm("IMG_0526.ppm", &width, &height, 1);
               if (!image) {
                  SUMA_SL_Err("Failed to read image.");
               }else{
                  #if TestTexture
                  SUMA_SL_Note("Creating texture, see init pp 415 in OpenGL programming guide, 3red");
                  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
                  glGenTextures(1, &texName);
                  glBindTexture(GL_TEXTURE_2D, texName);
                  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S, GL_CLAMP); /* GL_REPEAT, GL_CLAMP */
                  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER, GL_LINEAR);
                  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER, GL_LINEAR);
                  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, image);
                  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE); /* GL_REPLACE, GL_MODULATE */
                  glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP); /* GL_SPHERE_MAP, GL_EYE_LINEAR, GL_OBJECT_LINEAR */
                  glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
                  glEnable(GL_TEXTURE_GEN_S);
                  glEnable(GL_TEXTURE_GEN_T);
                  glEnable(GL_TEXTURE_2D);
                  glEnable(GL_CULL_FACE);
                  glEnable(GL_LIGHTING);
                  glEnable(GL_LIGHT0);
                  glEnable(GL_AUTO_NORMAL);
                  glEnable(GL_NORMALIZE);
                  glMaterialf(GL_FRONT, GL_SHININESS, 64.0);
                  #endif
               }
               /*
               fid = fopen("junk.img", "w");
               SUMA_disp_vecucmat (image, width*height, 4, 1, SUMA_ROW_MAJOR, fid, NOPE);
               fclose(fid);
               */
               
            }
            if (image) {
               SUMA_SL_Note(  "Drawing the image.");
               /* NOTE. The raster position has been pushed aside by the string.
               If you want it back, you need a new call to glRasterPos3f */
               glRasterPos3f(SurfObj->NodeList[id], SurfObj->NodeList[id+1],SurfObj->NodeList[id+2]);
               glAlphaFunc(GL_GEQUAL, 0.25);/* Should do this only once, not each time you render ...*/
               glEnable(GL_ALPHA_TEST);
               glDrawPixels(width, height, GL_RGBA,
                  GL_UNSIGNED_BYTE, image);
               glDisable(GL_ALPHA_TEST);
            }
         }
         #endif

         /*fprintf(stdout, "Disabling clients\n");*/
         glDisableClientState (GL_COLOR_ARRAY);   
         glDisableClientState (GL_VERTEX_ARRAY);
         glDisableClientState (GL_NORMAL_ARRAY);   
         /*fprintf(stdout, "Out SUMA_DrawMesh, ARRAY mode\n");*/
         
         glDisable(GL_COLOR_MATERIAL);
         
         /* draw surface ROIs */
         if (!SUMA_Draw_SO_ROI (SurfObj, SUMAg_DOv, SUMAg_N_DOv)) {
            fprintf (SUMA_STDERR, "Error %s: Failed in drawing ROI objects.\n", FuncName);
         }
         /* Draw Axis */
         if (SurfObj->MeshAxis && SurfObj->ShowMeshAxis)   {
            if (!SUMA_DrawAxis (SurfObj->MeshAxis, sv)) {
               fprintf(stderr,"Error SUMA_DrawAxis: Unrecognized Stipple option\n");
            }
         }
         
         /* Draw Selected Node Highlight */
         if (SurfObj->ShowSelectedNode && SurfObj->SelectedNode >= 0) {
            /*fprintf(SUMA_STDOUT,"Drawing Node Selection \n");*/
            id = ND * SurfObj->SelectedNode;
            glMaterialfv(GL_FRONT, GL_EMISSION, SurfObj->NodeMarker->sphcol); /*turn on emissidity for sphere */
            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, NoColor);
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
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_SL_Warn("NULL SO");
      SUMA_RETURN(YUP);
   }
   if (LocalHead) {
      if (SO->Label) fprintf (SUMA_STDERR, "%s: freeing SO %s\n", FuncName, SO->Label);
      else fprintf (SUMA_STDERR, "%s: freeing SO\n", FuncName);
   }
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
   if (SO->SpecFile.Path) SUMA_free(SO->SpecFile.Path);
   if (SO->SpecFile.FileName) SUMA_free(SO->SpecFile.FileName);
   if (SO->MeshAxis) SUMA_Free_Axis (SO->MeshAxis);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->NodeMarker\n", FuncName);
   if (SO->NodeMarker) SUMA_Free_SphereMarker (SO->NodeMarker);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->FaceSetMarker\n", FuncName);
   if (SO->FaceSetMarker) SUMA_Free_FaceSetMarker(SO->FaceSetMarker);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->idcode_str\n", FuncName);
   if (SO->idcode_str) free(SO->idcode_str); /* DO NOT use SUMA_free because this pointer is created by UNIQ_hashcode which uses afni's calloc 
                                                If you do so, you'll get a nasty warning from SUMA_free*/
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing SO->LocalDomainParentID\n", FuncName);
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
   

   #if 0 /* no more Cx inside SO */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing Cx\n", FuncName);
   /* freeing Cx,  make sure that there are no links to Cx*/
   if (SO->Cx || SO->Cx_Inode) { /* there should be no case where only one of two is null but if such a case existed, you'll get notified below. */
      if (SUMA_ReleaseLink(SO->Cx_Inode)) { 
         /* some links are left, do not free memory */
      } else {
         if (SO->Cx) SUMA_free(SO->Cx);
         /* now free SO->Cx_Inode */
         if (SO->Cx_Inode) SUMA_free(SO->Cx_Inode);
      }
      SO->Cx = NULL;
      SO->Cx_Inode = NULL;
   } 
   #endif 
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing %d overlays\n", FuncName, SO->N_Overlays);
   
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
               fprintf(SUMA_STDERR,"Error SUMA_Free_Surface_Object : Failed to free SO->FN");
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
   
   if (SO) SUMA_free(SO);
   
   if (LocalHead) fprintf (stdout, "Done\n");
   SUMA_RETURN (YUP);
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
            SS = SUMA_StringAppend_va (SS,"Coord FileName: %s \n", SO->Name_coord.FileName);
            SS = SUMA_StringAppend_va (SS,"Coord Path: %s \n", SO->Name_coord.Path);
            SS = SUMA_StringAppend_va (SS,"Topo FileName: %s \n", SO->Name_topo.FileName);
            SS = SUMA_StringAppend_va (SS,"Topo Path: %s \n", SO->Name_topo.Path);
            break;
         case SUMA_VEC:
            SS = SUMA_StringAppend_va (SS,"VEC surface.\n");
            SS = SUMA_StringAppend_va (SS,"NodeList FileName: %s \n", SO->Name_coord.FileName);
            SS = SUMA_StringAppend_va (SS,"NodeList Path: %s \n", SO->Name_coord.Path);
            SS = SUMA_StringAppend_va (SS,"FaceSetList FileName: %s \n", SO->Name_topo.FileName);
            SS = SUMA_StringAppend_va (SS,"FaceSetList Path: %s \n", SO->Name_topo.Path);
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
         case SUMA_FT_NOT_SPECIFIED:
            SS = SUMA_StringAppend_va (SS,"File Type not specified.\n");
            break;
         default:
            SS = SUMA_StringAppend_va (SS,"Unknown surface type.\n");
            break;
      }

      SS = SUMA_StringAppend_va (SS,"SpecFile:");
      if (SO->SpecFile.Path) SS = SUMA_StringAppend_va (SS,"%s", SO->SpecFile.Path);
      if (SO->SpecFile.FileName) SS = SUMA_StringAppend_va (SS,"%s", SO->SpecFile.FileName);
      SS = SUMA_StringAppend_va (SS,"\n");
      
      SS = SUMA_StringAppend_va (SS,"FileType: %d\t FileFormat: %d\n", SO->FileType, SO->FileFormat);

      SS = SUMA_StringAppend_va (SS,"IDcode: %s\n", SO->idcode_str);
      
      if (!SO->LocalDomainParent) SS = SUMA_StringAppend_va (SS,"LocalDomainParent is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"LocalDomainParent: %s\n", SO->LocalDomainParent);

      if (!SO->LocalDomainParentID) SS = SUMA_StringAppend_va (SS,"LocalDomainParentID is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"LocalDomainParentID: %s\n", SO->LocalDomainParentID);
     
      if (!SO->LocalCurvatureParent) SS = SUMA_StringAppend_va (SS,"LocalCurvatureParent is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"LocalCurvatureParent: %s\n", SO->LocalCurvatureParent);
       
      if (!SO->LocalCurvatureParentID) SS = SUMA_StringAppend_va (SS,"LocalCurvatureParentID is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"LocalCurvatureParentID: %s\n", SO->LocalCurvatureParentID);
       
      if (!SO->OriginatorID) SS = SUMA_StringAppend_va (SS,"OriginatorID is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"OriginatorID: %s\n", SO->OriginatorID);
       
      if (!SO->DomainGrandParentID) SS = SUMA_StringAppend_va (SS,"DomainGrandParentID is NULL\n");
      else SS = SUMA_StringAppend_va (SS,"DomainGrandParentID: %s\n", SO->DomainGrandParentID);
             
      SS = SUMA_StringAppend_va (SS,"Group: %s\tState: %s\n", SO->Group, SO->State);

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
      sprintf (stmp,"N_FaceSet: %d, FaceSetDim %d\n", SO->N_FaceSet, SO->FaceSetDim);
      SS = SUMA_StringAppend (SS,stmp);
      
      SUMA_EULER_SO(SO, eu);
      SS = SUMA_StringAppend_va (SS, "Euler No. = %d\n\n", eu);
      
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
      sprintf (stmp,"TAGALIGN_APPLIED: %d\n", SO->TAGALIGN_APPLIED);
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
         sprintf (stmp, "SO->EL, %d edges, %d unique edges.\n"
                        "max_Hosts %d, min_Hosts %d (showing %d out of %d elements):\n", \
               SO->EL->N_EL, SO->EL->N_Distinct_Edges, SO->EL->max_N_Hosts, SO->EL->min_N_Hosts, MaxShow, SO->EL->N_EL);
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
      
      if (DsetList) {
         float *Cx = NULL;
         Cx = (float *)SUMA_GetCx(SO->idcode_str, DsetList, 0);
         if (Cx == NULL) {
            sprintf (stmp,"Cx = NULL\n\n") ;
            SS = SUMA_StringAppend (SS,stmp);
         } else {
            if (MaxShow > SO->N_Node) MaxShow = SO->N_Node;
            sprintf (stmp, "Cx, showing %d out of %d elements:\n", MaxShow, SO->N_Node);
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
      
      if ( (SO->PermCol && SO->N_Overlays) || (SO->PermCol && SO->N_Overlays) ) {
         SUMA_StringAppend (SS,"CONFLICT! Both PermCol and Overlays are specified!\n");
      }
      
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

/*!
Create a Surface Object data structure 
*/

SUMA_SurfaceObject *SUMA_Alloc_SurfObject_Struct(int N)
{
   static char FuncName[]={"SUMA_Alloc_SurfObject_Struct"};
   SUMA_SurfaceObject *SO;
   int i, j;
   
   SUMA_ENTRY;

   SO = (SUMA_SurfaceObject *)SUMA_malloc(sizeof(SUMA_SurfaceObject)*N);
   if (SO == NULL) {
      SUMA_alloc_problem("SUMA_Alloc_SurfObject_Struct: could not allocate memory for SO");
   }
   
   for (i=0; i< N; ++i) {
      SO[i].FileType = SUMA_FT_NOT_SPECIFIED;
      SO[i].FileFormat = SUMA_FF_NOT_SPECIFIED;
      SO[i].NodeMarker = NULL;
      SO[i].Name_NodeParent = NULL;
      SO[i].Label = NULL;
      SO[i].EmbedDim = 3;
      SO[i].MF = NULL;
      SO[i].FN = NULL;
      SO[i].EL = NULL;
      SO[i].PolyArea = NULL;
      SO[i].SC = NULL;
      SO[i].VolPar = NULL;
      SO[i].NodeList = NULL; 
      SO[i].FaceSetList = NULL; 
      SO[i].FaceNormList = NULL; 
      SO[i].NodeNormList = NULL; 
      SO[i].glar_NodeList = NULL; 
      SO[i].glar_FaceSetList = NULL; 
      SO[i].glar_FaceNormList = NULL; 
      SO[i].glar_NodeNormList = NULL; 
      /* create vector of pointers */
      SO[i].Overlays = (SUMA_OVERLAYS **) SUMA_malloc(sizeof(SUMA_OVERLAYS *) * SUMA_MAX_OVERLAYS);
      /* fill pointers with NULL */
      for (j=0; j < SUMA_MAX_OVERLAYS; ++j) {
         SO[i].Overlays[j] = NULL;
      }
      SO[i].N_Overlays = 0;
      SO[i].SentToAfni = NOPE;
      
      SO[i].MeshAxis = NULL;
      SO[i].State = NULL;
      SO[i].Group = NULL;
      SO[i].FaceSetMarker = NULL;
      SO[i].idcode_str = NULL;
      SO[i].SpecFile.Path = NULL;
      SO[i].SpecFile.FileName = NULL;
      SO[i].Name.Path = NULL;
      SO[i].Name.FileName = NULL;
      SO[i].Name_coord.Path = NULL;
      SO[i].Name_coord.FileName = NULL;
      SO[i].Name_topo.Path = NULL;
      SO[i].Name_topo.FileName = NULL;
      SO[i].SUMA_VolPar_Aligned = NOPE;
      SO[i].VOLREG_APPLIED = NOPE;
      SO[i].TAGALIGN_APPLIED = NOPE;
      SO[i].SurfCont = NULL; /* This is now handled in SUMA_LoadSpec_eng (used to be SUMA_CreateSurfContStruct();) */
      SO[i].PolyMode = SRM_ViewerDefault;
      SO[i].Show = YUP;
      SO[i].Side = SUMA_NO_SIDE;
      SO[i].AnatCorrect = NOPE;
      SO[i].DomainGrandParentID = NULL;
      SO[i].OriginatorID = NULL;
      SO[i].LocalDomainParent = NULL;
      SO[i].LocalCurvatureParent = NULL;
      SO[i].LocalDomainParentID = NULL;
      SO[i].LocalCurvatureParentID = NULL;
      SO[i].PermCol = NULL;
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
   
   \sa SUMA_NIMLDrawnROI_to_DrawnROI where a SUMA_DRAWN_ROI is also created
*/
SUMA_DRAWN_ROI *SUMA_AllocateDrawnROI (char *Parent_idcode_str, SUMA_ROI_DRAWING_STATUS DrawStatus, 
                                       SUMA_ROI_DRAWING_TYPE Type, char *label, int ilabel) 
{
   SUMA_DRAWN_ROI *D_ROI = NULL;
   static int ROI_index = 1;
   static char FuncName[]={"SUMA_AllocateDrawnROI"};
   
   SUMA_ENTRY;
   
   D_ROI = (SUMA_DRAWN_ROI *) SUMA_malloc (sizeof(SUMA_DRAWN_ROI));
   D_ROI->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));
   D_ROI->Parent_idcode_str = (char *)SUMA_calloc (strlen(Parent_idcode_str)+1, sizeof (char));
   D_ROI->ColPlaneName = SUMA_copy_string("DefROIpl");
   D_ROI->FillColor[0] = 1.0; D_ROI->FillColor[1] = 0.0; D_ROI->FillColor[2] = 0.0;
   D_ROI->EdgeColor[0] = 0.0; D_ROI->EdgeColor[1] = 0.0; D_ROI->EdgeColor[2] = 1.0;
   D_ROI->EdgeThickness = 2;
   D_ROI->ROIstrokelist = (DList *)SUMA_malloc (sizeof(DList));
   dlist_init(D_ROI->ROIstrokelist, SUMA_FreeROIDatum);
   D_ROI->CE = NULL;
   D_ROI->N_CE = -1;
   
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
SUMA_Boolean SUMA_PrependToROIdatum (SUMA_ROI_DATUM *ROId1, SUMA_ROI_DATUM *ROId2)
{
   static char FuncName[]={"SUMA_PrependToROIdatum"};
   int i, N_nNew=-1, N_tNew=-1, *tPathNew=NULL, *nPathNew=NULL;
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
      isNodeInMesh = (SUMA_Boolean*) SUMA_malloc(SO->N_Node * sizeof(SUMA_Boolean)); N_left = SO->N_Node; for (i=0;i<N_left;++i) isNodeInMesh[i] = YUP;
      if (!isNodeInMesh) {
         SUMA_SL_Err("Failed to allocate!\nWill not compute shortest distance.");
      }else {
         nPath = SUMA_Dijkstra (SO, ROId->nPath[0], ROId->nPath[ROId->N_n - 1], isNodeInMesh, &N_left, 1, &dd, &N_n);
         if (nPath) { 
            SUMA_free(nPath); nPath = NULL; dd_c = dd / SUMA_FS_DIJKSTRA_DISTANCE_FACTOR;
         } else { 
            dd = -2.0;  dd_c = -2.0;
         }
         SUMA_free(isNodeInMesh); isNodeInMesh = NULL;
      }
      SS = SUMA_StringAppend_va(SS,
         "#Distances on %s\n"
         "#n0\tn1\tN_n\td\td_c\tds\tds_c\n"
         "%d\t%d\t%d\t%.2f\t%.2f\t%.2f\t%.2f\n", 
         SO->Label, ROId->nPath[0], ROId->nPath[ROId->N_n - 1], ROId->N_n, ds, ds_c, dd, dd_c);
   } else if (option == SW_DrawROI_WhatDistTrace) {
      SS = SUMA_StringAppend_va(SS,
         "#Distances on %s\n"
         "#n0\tn1\tN_n\td\td_c\n"
         "%d\t%d\t%d\t%.2f\t%.2f\n", 
         SO->Label, ROId->nPath[0], ROId->nPath[ROId->N_n - 1], ROId->N_n, ds, ds_c);
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
