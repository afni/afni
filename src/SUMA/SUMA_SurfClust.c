#include "SUMA_suma.h"

#undef STAND_ALONE

#if defined SUMA_SurfClust_STANDALONE
#define STAND_ALONE 
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */
#else
extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  
#endif

static int BuildMethod;
typedef enum { SUMA_NO_BUILD_METHOD, SUMA_OFFSETS2, SUMA_OFFSETS_LL, SUMA_OFFSETS2_NO_REC } SUMA_CLUST_BUILD_METHODS;

void SUMA_FreeClustDatum (void * data)
{
   static char FuncName[]={"SUMA_FreeClustDatum"};
   SUMA_CLUST_DATUM *Clust = NULL;
   SUMA_ENTRY;
   
   if (!data) SUMA_RETURNe;
   Clust = (SUMA_CLUST_DATUM *)data;
   if (Clust->NodeList) SUMA_free(Clust->NodeList); 
   if (Clust->ValueList) SUMA_free(Clust->ValueList);
   SUMA_free(Clust);
   
   SUMA_RETURNe;
}

/*!
   \brief Calculate area of each node as one third of the sum of
   the areas of incident triangles. 
   If you change this function make sure changes are also done
   on RickR's compute_node_areas since the two functions use
   the same principle.
*/
float *SUMA_CalculateNodeAreas(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_CalculateNodeAreas"};
   float *NodeAreas=NULL;
   int *flist = NULL, i, c;
   
   SUMA_ENTRY;
   
   if (!SO) { SUMA_RETURN(NodeAreas); }
   if (!SO->PolyArea) {
      if (!SUMA_SurfaceMetrics (SO, "PolyArea", NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         SUMA_RETURN(NodeAreas);
      }
   }
   
   NodeAreas = (float *)SUMA_malloc(SO->N_Node*sizeof(float));
   if (!NodeAreas) { SUMA_SL_Crit ("Failed to allocate for NodeAreas"); SUMA_RETURN(NodeAreas); }
   
   for (i=0; i<SO->N_Node; ++i) {
      flist = SO->MF->NodeMemberOfFaceSet[i];
      NodeAreas[i] = 0.0;
      for (c = 0; c < SO->MF->N_Memb[i]; c++) {
         NodeAreas[i] += SO->PolyArea[flist[c]];
      }
      NodeAreas[i] /= 3.0;
   }
   
   SUMA_RETURN(NodeAreas);
}


/*!
   \brief builds a cluster starting from some node 
   
   \param dothisnode (int) start building from this node
   \param AddToThisClust (SUMA_CLUST_DATUM *)pointer to cluster to add to. Send NULL
                    when function is first called. This is a non NULL when the 
                    function recurses.
   \param ToBeAssigned (float *) if ToBeAssigned[i] then node i (index into SO's nodelist)
                                 is considered in the clustering and the value at that
                                 node is ToBeAssigned[i]. Vector is SO->N_Node elements
                                 long. Gets modified during recursion.
   \param N_ToBeAssigned (int *) pointer to number of value values in ToBeAssigned. 
                                 Changes with recursive calls. 
   \param NodeArea(float *) Vector containing area of each node of surface
   \param SO (SUMA_SurfaceObject *) the usual deal
   \param Opt (SUMA_SURFCLUST_OPTIONS *) options for cluster building.     
   
   \sa recursive calls SUCK a lot of memory and are a nightmare to track with 
   function I/O. They are also slow and not necessary! 
   
   \sa SUMA_Build_Cluster_From_Node_NoRec       
*/
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node(int dothisnode, SUMA_CLUST_DATUM *AddToThisClust, 
                                                float *ToBeAssigned, int *N_TobeAssigned, float *NodeArea,
                                                SUMA_SurfaceObject *SO, SUMA_SURFCLUST_OPTIONS *Opt)
{
   static char FuncName[]={"SUMA_Build_Cluster_From_Node"};
   SUMA_CLUST_DATUM *Clust = NULL;
   static int ncall;
   int il, jl, neighb;
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   DList *offlist = NULL;
   DListElmt *elm = NULL;
   SUMA_OFFSET_LL_DATUM *dat=NULL;
   int NewClust = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   ++ncall;
   if (dothisnode < 0) {
      SUMA_SL_Err("Unexpected negative index.");
      SUMA_RETURN(NULL);
   }
   if (!AddToThisClust) {
      Clust = (SUMA_CLUST_DATUM *)SUMA_malloc(sizeof(SUMA_CLUST_DATUM));   
      Clust->N_Node = 0; Clust->totalarea = 0.0; /* Clust->rank = -1; */  
      Clust->totalvalue = 0.0; Clust->totalabsvalue = 0.0;  
      Clust->minvalue = ToBeAssigned[dothisnode]; Clust->minnode = dothisnode;
      Clust->maxvalue = ToBeAssigned[dothisnode]; Clust->maxnode = dothisnode; 
      Clust->varvalue = 0.0;  Clust->centralnode = 0; Clust->weightedcentralnode = 0; 
      Clust->NodeList = (int *)SUMA_malloc((*N_TobeAssigned) * sizeof(int)); 
      Clust->ValueList = (float *)SUMA_malloc((*N_TobeAssigned) * sizeof(float));  
      if (!Clust->NodeList || !Clust->ValueList) { 
         SUMA_SL_Crit("Failed to allocate for NodeList or ValueList");  
         SUMA_free(Clust); Clust = NULL;
         SUMA_RETURN(NULL);  
      }
      NewClust = 1;
      if (LocalHead) fprintf (SUMA_STDERR,"%s: New Cluster     %p, with node %d\n", FuncName, Clust, dothisnode);
   } else { 
      NewClust = 0;
      Clust = AddToThisClust; 
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Reusing Cluster %p, with node %d\n", FuncName, Clust, dothisnode);
   }
   
   /* Add node to cluster */
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Adding node %d to cluster %p of %d nodes\n", FuncName, dothisnode, Clust, Clust->N_Node);
   Clust->NodeList[Clust->N_Node] = dothisnode; 
   Clust->totalarea += NodeArea[dothisnode]; 
   Clust->totalvalue += ToBeAssigned[dothisnode];
   Clust->totalabsvalue += (float)fabs((float)ToBeAssigned[dothisnode]);
   if (ToBeAssigned[dothisnode] < Clust->minvalue) { Clust->minvalue = ToBeAssigned[dothisnode]; Clust->minnode = dothisnode; }
   if (ToBeAssigned[dothisnode] > Clust->maxvalue) { Clust->maxvalue = ToBeAssigned[dothisnode]; Clust->maxnode = dothisnode; }
   Clust->ValueList[Clust->N_Node] = ToBeAssigned[dothisnode];
   ++Clust->N_Node;

   /* mark it as assigned, an reduce the number of nodes left to assign*/
   ToBeAssigned[dothisnode] = 0; --(*N_TobeAssigned);
   
   if (BuildMethod == SUMA_OFFSETS2) {
      /* Tres bad memory utilization due to recursive calls */
      if (*N_TobeAssigned) {
         /* look in its vicinity - bad memory usage due to recursive calls*/
         OffS = SUMA_Initialize_getoffsets (SO->N_Node);
         SUMA_getoffsets2 (dothisnode, SO, Opt->DistLim, OffS, NULL, 0);
         #if 0
         if (NewClust) {
            FILE *fid=NULL;
            char *s=NULL, tmp[50];
            fid = fopen("offsets2.1D", "w"); 
            if (!fid) {
               SUMA_SL_Err("Could not open file for writing.\nCheck file permissions, disk space.\n");
            } else {
               s = SUMA_ShowOffset_Info(OffS, 0);
               if (s) { fprintf(fid,"%s\n", s);  SUMA_free(s); s = NULL;}
               fclose(fid);
            }      
         }
         #endif
         /* search to see if any are to be assigned */
         for (il=1; il<OffS->N_layers; ++il) { /* starting at layer 1, layer 0 is the node itself */
            for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) {
               neighb = OffS->layers[il].NodesInLayer[jl];
               if (ToBeAssigned[neighb] && OffS->OffVect[neighb] <= Opt->DistLim) {
                     /* take that node into the cluster */
                     SUMA_Build_Cluster_From_Node( neighb, Clust, ToBeAssigned, N_TobeAssigned, NodeArea, SO, Opt); 
               }
            }
         }
         /* free this OffS structure (Note you can't recycle the same structure because you are using many
         OffS at one because of recursive calls */
         if (OffS) SUMA_Free_getoffsets(OffS); OffS = NULL;
      }
   } else if (BuildMethod == SUMA_OFFSETS_LL) { 
      if (*N_TobeAssigned) {
         /* look in its vicinity */
         if (!(offlist = SUMA_getoffsets_ll (dothisnode, SO, Opt->DistLim, NULL, 0))) {
            SUMA_SL_Err("Failed to get offsets.\nNo cleanup done.");
            SUMA_RETURN(NULL);
         }
         #if 0
         if (NewClust) {
            FILE *fid=NULL;
            char *s=NULL, tmp[50];
            fid = fopen("offsets_ll.1D", "w"); 
            if (!fid) {
               SUMA_SL_Err("Could not open file for writing.\nCheck file permissions, disk space.\n");
            } else {
               s = SUMA_ShowOffset_ll_Info(offlist, 0);
               if (s) { fprintf(fid,"%s\n", s);  SUMA_free(s); s = NULL;}
               fclose(fid);
            }      
         }
         #endif

         /* search to see if any are to be assigned, start at layer 1*/
         elm = dlist_head(offlist);
         dat = (SUMA_OFFSET_LL_DATUM *)elm->data;
         if (dat->layer != 0) {
            SUMA_SL_Err("Unexpected non zero layer for first element.");
            SUMA_RETURN(NULL);
         }
         do {
            elm = elm->next;
            dat = (SUMA_OFFSET_LL_DATUM *)elm->data;
            neighb = dat->ni;
            if (ToBeAssigned[neighb] && dat->off <= Opt->DistLim) {
               /* take that node into the cluster */
               SUMA_Build_Cluster_From_Node( neighb, Clust, ToBeAssigned, N_TobeAssigned, NodeArea, SO, Opt); 
            }      
         } while (elm != dlist_tail(offlist));
         dlist_destroy(offlist);  
      }      
   }
   
   SUMA_RETURN(Clust);
}

/*! 
   A macro for SUMA_Build_Cluster_From_Node_NoRec
*/
#define SUMA_ADD_NODE_TO_CLUST(dothisnode, Clust, NodeArea, ToBeAssigned) {   \
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Adding node %d to cluster %p of %d nodes\n", FuncName, dothisnode, Clust, Clust->N_Node);   \
   Clust->NodeList[Clust->N_Node] = dothisnode; \
   Clust->totalarea += NodeArea[dothisnode]; \
   Clust->totalvalue += ToBeAssigned[dothisnode];  \
   Clust->totalabsvalue += (float)fabs((float)ToBeAssigned[dothisnode]);   \
   if (ToBeAssigned[dothisnode] < Clust->minvalue) { Clust->minvalue = ToBeAssigned[dothisnode]; Clust->minnode = dothisnode; }  \
   if (ToBeAssigned[dothisnode] > Clust->maxvalue) { Clust->maxvalue = ToBeAssigned[dothisnode]; Clust->maxnode = dothisnode; }  \
   Clust->ValueList[Clust->N_Node] = ToBeAssigned[dothisnode]; \
   ++Clust->N_Node;  \
}
/*!
   \brief builds a cluster starting from some node 
   
   \param dothisnode (int) start building from this node
   \param ToBeAssigned (float *) if ToBeAssigned[i] then node i (index into SO's nodelist)
                                 is considered in the clustering and the value at that
                                 node is ToBeAssigned[i]. Vector is SO->N_Node elements
                                 long. Gets modified in function.
   \param N_ToBeAssigned (int *) pointer to number of value values in ToBeAssigned. 
                                 Gets modified in function. 
   \param NodeArea(float *) Vector containing area of each node of surface
   \param SO (SUMA_SurfaceObject *) the usual deal
   \param Opt (SUMA_SURFCLUST_OPTIONS *) options for cluster building.     
      
   \sa Based on recursive SUMA_Build_Cluster_From_Node       
*/
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node_NoRec    (  int dothisnode, 
                                                            float *ToBeAssigned, int *N_TobeAssigned, float *NodeArea,
                                                            SUMA_SurfaceObject *SO, SUMA_SURFCLUST_OPTIONS *Opt   )
{
   static char FuncName[]={"SUMA_Build_Cluster_From_Node_NoRec"};
   SUMA_CLUST_DATUM *Clust = NULL;
   static int ncall;
   int il, jl, neighb, itmp;
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   DList *offlist = NULL, *candlist=NULL;
   DListElmt *elm = NULL, *dothiselm=NULL;
   SUMA_OFFSET_LL_DATUM *dat=NULL;
   int NewClust = 0;
   byte *visited=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   ++ncall;
   if (dothisnode < 0) {
      SUMA_SL_Err("Unexpected negative index.");
      SUMA_RETURN(NULL);
   }

      OffS = SUMA_Initialize_getoffsets (SO->N_Node);
      Clust = (SUMA_CLUST_DATUM *)SUMA_malloc(sizeof(SUMA_CLUST_DATUM));   
      Clust->N_Node = 0; Clust->totalarea = 0.0; /* Clust->rank = -1; */  
      Clust->totalvalue = 0.0; Clust->totalabsvalue = 0.0;  
      Clust->minvalue = ToBeAssigned[dothisnode]; Clust->minnode = dothisnode;
      Clust->maxvalue = ToBeAssigned[dothisnode]; Clust->maxnode = dothisnode; 
      Clust->varvalue = 0.0;  Clust->centralnode = 0; Clust->weightedcentralnode = 0;
      Clust->NodeList = (int *)SUMA_malloc((*N_TobeAssigned) * sizeof(int)); 
      Clust->ValueList = (float *)SUMA_malloc((*N_TobeAssigned) * sizeof(float));  
      if (!Clust->NodeList || !Clust->ValueList || !OffS) { 
         SUMA_SL_Crit("Failed to allocate for NodeList or ValueList");  
         SUMA_free(Clust); Clust = NULL;
         SUMA_RETURN(NULL);  
      }
   candlist = (DList*)SUMA_malloc(sizeof(DList));
   visited = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte));
   if (!visited || !candlist) {
      SUMA_SL_Crit("Failed to allocate for visited or candlist");  
      SUMA_free(Clust); Clust = NULL;
      SUMA_RETURN(NULL);  
   }
   dlist_init(candlist, NULL);
   /* Add node to cluster */
   SUMA_ADD_NODE_TO_CLUST(dothisnode, Clust, NodeArea, ToBeAssigned);
   /* mark it as assigned, an reduce the number of nodes left to assign*/
   ToBeAssigned[dothisnode] = 0; --(*N_TobeAssigned);
   visited[dothisnode] = YUP;
   dlist_ins_next(candlist, dlist_tail(candlist), (void *)dothisnode);
      /* Tres bad memory utilization due to recursive calls */
      while (*N_TobeAssigned && dlist_size(candlist)) {
         /* look in its vicinity */
         dothiselm = dlist_head(candlist); dothisnode = (int) dothiselm->data;
         SUMA_getoffsets2 (dothisnode, SO, Opt->DistLim, OffS, NULL, 0);
         /* remove node from candidate list */
         dlist_remove(candlist, dothiselm, (void*)&itmp);
         /* search to see if any are to be assigned */
         for (il=1; il<OffS->N_layers; ++il) { /* starting at layer 1, layer 0 is the node itself */
            for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) {
               neighb = OffS->layers[il].NodesInLayer[jl];
               if (ToBeAssigned[neighb] && OffS->OffVect[neighb] <= Opt->DistLim) {
                     /* take that node into the cluster */
                     SUMA_ADD_NODE_TO_CLUST(neighb, Clust, NodeArea, ToBeAssigned);
                     /* mark it as assigned, an reduce the number of nodes left to assign*/
                     ToBeAssigned[neighb] = 0; --(*N_TobeAssigned);
                     /* mark it as a candidate if it has not been visited as a candidate before */
                     if (!visited[neighb]) {
                        dlist_ins_next(candlist, dlist_tail(candlist), (void *)neighb);
                        visited[neighb] = YUP;   
                     }
                     
               }
            }
         }
         /* recycle */
         SUMA_Recycle_getoffsets (OffS);
      }
   /* free this OffS structure  */
   if (OffS) SUMA_Free_getoffsets(OffS); OffS = NULL;
   
   SUMA_RETURN(Clust);
}

/*!
   \brief Finds clusters of data on the surface.
   
   \param SO (SUMA_SurfaceObject *) pointer to surface in question
   \param ni (int *) vector of N_Ni node indices that are to be considered
                     for clustering
   \param nv (float *) vector of N_Ni node values (corresponding to ni) 
                      Only non 0 values are considered for clustering
   \param N_ni (int) number of values in ni and nv
   \param dothisnode (int) index of node (into SO's nodelist, not ni) to start/proceed from
   \param Opt (SUMA_SURFCLUST_OPTIONS *) structure containing clustering options
   \param AddToThisClust (SUMA_CLUST_DATUM *) add to this cluster
   \param NodeArea (float *) SO->N_Node vector of node areas. 
   \return ClustList (DList *) list of clusters, in no particular order. Processing of the list is to
                              be done later.
   
   \sa SUMA_Build_Cluster_From_Node
*/     
DList *SUMA_FindClusters ( SUMA_SurfaceObject *SO, int *ni, float *nv, int N_ni, 
                           int dothisnode, SUMA_SURFCLUST_OPTIONS *Opt, 
                           float *NodeArea)
{
   static char FuncName[]={"SUMA_FindClusters"};
   DList *list=NULL;
   DListElmt *elm=NULL;
   float *ToBeAssigned=NULL;
   float mean;
   int N_n, nc, i, kk;
   SUMA_CLUST_DATUM *Clust = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !nv || !ni) {
      SUMA_S_Err("Bad parameters");
   }
   if (dothisnode == -1) { /* initialize */
      SUMA_LH("Initializing");
      nc = 0;
      /* initialize the list */
      list = (DList *)SUMA_malloc(sizeof(DList));
      dlist_init(list, SUMA_FreeClustDatum); 
      ToBeAssigned = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
      N_n = N_ni;
      for (i=0; i<N_n; ++i) {
         ToBeAssigned[ni[i]] = nv[i];
      }
   }
   
   while (N_n > 0) {
      dothisnode = -1;
      for (i=0;i<SO->N_Node; ++i) { 
         if (ToBeAssigned[i]) {  
            dothisnode = i; continue;  
         }  
      }
      if (dothisnode < 0) {
         SUMA_S_Err("Not expected here. dothisnode < 0"); 
         SUMA_RETURN(list);
      } 

      if (BuildMethod == SUMA_OFFSETS2_NO_REC) {
         Clust = SUMA_Build_Cluster_From_Node_NoRec(dothisnode, ToBeAssigned, &N_n, NodeArea, SO, Opt);
      } else if (BuildMethod == SUMA_OFFSETS2 || BuildMethod == SUMA_OFFSETS_LL) {
         Clust = SUMA_Build_Cluster_From_Node(dothisnode, NULL, ToBeAssigned, &N_n, NodeArea, SO, Opt);
      } else {
         SUMA_SL_Err("No Such Method!");
         SUMA_RETURN(list);
      }
      if (!Clust) {
         SUMA_SL_Err("Failed in SUMA_Build_Cluster_From_Node*");
         SUMA_RETURN(list);
      }   
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Cluster %p is finished, %d nodes\n", FuncName, Clust, Clust->N_Node); 
      mean = Clust->totalvalue/((float)Clust->N_Node);
      for (kk=0; kk < Clust->N_Node; ++kk) {
         Clust->varvalue += (Clust->ValueList[kk] - mean) * (Clust->ValueList[kk] - mean);   
      }
      if (Clust->N_Node > 1) Clust->varvalue /= (Clust->N_Node - 1);
      else Clust->varvalue = 0.0;
      /* reallocate to save space */
      Clust->NodeList = (int *)SUMA_realloc(Clust->NodeList, sizeof(int)*Clust->N_Node);
      Clust->ValueList = (float *)SUMA_realloc(Clust->ValueList, sizeof(float)*Clust->N_Node);
      if (!Clust->NodeList || !Clust->ValueList) { 
         SUMA_SL_Crit("Failed to reallocate for NodeList or ValueList");  
         SUMA_RETURN(NULL);   
      }
      /* find the central node */
      if (!SUMA_ClusterCenterofMass  (SO, Clust, 1)) {
         SUMA_SL_Err("Failed to find central node");  
         SUMA_RETURN(list);   
      }

      dlist_ins_next(list, dlist_tail(list), (void *)Clust); 
      ++nc;
   }   
  
   if (N_n == 0) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: No more nodes to consider, cleaning up.\n", FuncName);
      if (ToBeAssigned) SUMA_free(ToBeAssigned); ToBeAssigned = NULL;   \
   }
   
   SUMA_RETURN(list);
} 

/*! Show the ViewState structure */
SUMA_Boolean SUMA_Show_SurfClust_list(DList *list, FILE *Out, int detail, char *params) 
{
   static char FuncName[]={"SUMA_Show_SurfClust_list"};
   char *s = NULL;
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;

   s = SUMA_Show_SurfClust_list_Info(list,  detail, params);
   if (!s) {
      SUMA_SL_Err("Failed in SUMA_Show_SurfClust_list_Info");
      SUMA_RETURN(NOPE);
   }  else {
      fprintf(Out, "%s", s);
      SUMA_free(s); s = NULL;
   }
   
   SUMA_RETURN(YUP);
}

/*! Show the SurfClust list */
char *SUMA_Show_SurfClust_list_Info(DList *list, int detail, char *params) 
{
   static char FuncName[]={"SUMA_Show_SurfClust_list_Info"};
   int i, ic, max;
   SUMA_STRING *SS = NULL;
   DListElmt *elmt=NULL;
   SUMA_CLUST_DATUM *cd=NULL;
   char *s=NULL, *pad_str, str[20];   
   int lc[]= { 6, 6, 9, 9, 9, 6, 6, 9, 6, 9, 6, 9, 9 };
   char Col[][12] = { {"# Rank"}, {"num Nd"}, {"Area"}, {"Mean"}, {"|Mean|"},{"Cent"}, {"W Cent"},{"Min V"}, {"Min Nd"}, {"Max V"}, {"Max Nd"} , {"Var"}, {"SEM"} };
   SUMA_ENTRY;

   SS = SUMA_StringAppend (NULL, NULL);
   
   if (!list) {
      SS = SUMA_StringAppend (SS,"NULL list.\n");
      SUMA_SS2S(SS,s); 
      SUMA_RETURN(s);  
   }

   if (!list->size) {
      SS = SUMA_StringAppend (SS,"Empty list.\n");
      SUMA_SS2S(SS,s); 
      SUMA_RETURN(s);  
   }else{
      SS = SUMA_StringAppend_va (SS,"#Col. 0  = Rank \n"
                                    "#Col. 1  = Number of nodes\n"
                                    "#Col. 2  = Total area (units^2)\n"
                                    "#Col. 3  = Mean value\n"
                                    "#Col. 4  = Mean absolute value\n"
                                    "#Col. 5  = Central node\n"
                                    "#Col. 6  = Weighted central node\n"
                                    "#Col. 7  = Minimum value\n"
                                    "#Col. 8  = Minimum node\n"
                                    "#Col. 9  = Maximum value\n"
                                    "#Col. 10 = Maximum node\n"
                                    "#Col. 11 = Variance\n"
                                    "#Col. 12 = Standard error of the mean\n"
                                    "#Command history:\n"
                                    "#%s\n", params);
      
      for (ic=0; ic<13; ++ic) {
         if (ic == 0) sprintf(str,"%s", Col[ic]); 
         else sprintf(str,"%s", Col[ic]); 
         pad_str = SUMA_pad_string(str, ' ', lc[ic], 0);
         SS = SUMA_StringAppend_va (SS,"%s   ", pad_str);
         SUMA_free(pad_str);
      }
      SS = SUMA_StringAppend_va (SS,"\n");
      if (detail == 1) {
         SS = SUMA_StringAppend_va (SS,"#Other columns: list of 5 first nodes in ROI.\n");   
      }
      if (detail == 2) {
         SS = SUMA_StringAppend_va (SS,"#Other columns: list all  nodes in ROI.\n");   
      }
      if (detail > 0) {
         SS = SUMA_StringAppend_va (SS,"#A total of %d cluster%s were found.\n", list->size, SUMA_COUNTER_PLURAL(list->size));
      }
   }
   
   elmt = NULL; 
   ic = 1; 
   do {
      if (!elmt) elmt = dlist_head(list); else elmt = elmt->next;
      if (!elmt) SS = SUMA_StringAppend_va (SS,"#%d%s cluster element is NULL!\n", ic, SUMA_COUNTER_SUFFIX(ic));
      else {
         cd = (SUMA_CLUST_DATUM *)elmt->data;
         if (detail > 0) SS = SUMA_StringAppend_va (SS,"#%d%s cluster\n", ic, SUMA_COUNTER_SUFFIX(ic));
         SS = SUMA_StringAppend_va (SS,"%6d   %6d   %9.2f"
                                       "   %9.3f   %9.3f"
                                       "   %6d   %6d"
                                       "   %9.3f   %6d"
                                       "   %9.3f   %6d"
                                       "   %9.3f   %9.3f"
                                       , ic, cd->N_Node, cd->totalarea
                                       , cd->totalvalue/((float)cd->N_Node), cd->totalabsvalue/((float)cd->N_Node)
                                       , cd->centralnode, cd->weightedcentralnode 
                                       , cd->minvalue, cd->minnode
                                       , cd->maxvalue, cd->maxnode
                                       , cd->varvalue, sqrt(cd->varvalue/cd->N_Node));
         if (detail > 0) {
            if (detail == 1) {
               if (cd->N_Node < 5) max = cd->N_Node; else max = 5;
            } else max = cd->N_Node;
            for (i=0;i<max; ++i) SS = SUMA_StringAppend_va (SS,"%d\t", cd->NodeList[i]);
         }
         SS = SUMA_StringAppend(SS,"\n"); 
      }
      ++ic; 
   } while (elmt != dlist_tail(list));
   
   SUMA_SS2S(SS,s);
   
   SUMA_RETURN (s);
}


/*! Turn the clusters to a cluster dataset mask*/
SUMA_DSET *SUMA_SurfClust_list_2_Dset(SUMA_SurfaceObject *SO, DList *list, SUMA_Boolean FullList, char *leName) 
{
   static char FuncName[]={"SUMA_SurfClust_list_2_Dset"};
   int i, ic, max, j, rank;
   DListElmt *elmt=NULL;
   SUMA_DSET *dset = NULL;
   int *NodeIndex=NULL, N_Node, *Val = NULL;
   SUMA_CLUST_DATUM *cd=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!list) {
      SUMA_SL_Err("NULL list");
      SUMA_RETURN(dset);  
   }
   if (FullList) N_Node = SO->N_Node;
   else {
         elmt = NULL; N_Node = 0;
         do {
            if (!elmt) elmt = dlist_head(list);
            else elmt = elmt->next;
            cd = (SUMA_CLUST_DATUM *)elmt->data; 
            N_Node += cd->N_Node;
         } while (elmt != dlist_tail(list));
   }
   NodeIndex = (int *)SUMA_malloc(N_Node*sizeof(int));
   Val = (int *)SUMA_malloc(N_Node * sizeof(int));
   if (!NodeIndex || !Val){
      SUMA_SL_Crit("Failed to allocate NodeIndex and or Val");
      SUMA_RETURN(dset);
   }
   if (FullList) {
      for (i=0; i<N_Node; ++i) {
         NodeIndex[i] = i; Val[i] = 0;
      }
      elmt = NULL; rank = 1;
      do {
         if (!elmt) elmt = dlist_head(list);
         else elmt = elmt->next;
         cd = (SUMA_CLUST_DATUM *)elmt->data; 
         for (j=0; j<cd->N_Node; ++j) {
            Val[cd->NodeList[j]] = rank;
         }
         ++rank;
      } while (elmt != dlist_tail(list));
   } else {
      elmt = NULL; rank = 1; i = 0;
      do {
         if (!elmt) elmt = dlist_head(list);
         else elmt = elmt->next;
         cd = (SUMA_CLUST_DATUM *)elmt->data; 
         for (j=0; j<cd->N_Node; ++j) {
            Val[i] = rank;
            NodeIndex[i] = cd->NodeList[j];
            ++i;
         }
         ++rank;
      } while (elmt != dlist_tail(list)); 
   }
   
   SUMA_LH("Creating dset pointer");
   dset = SUMA_CreateDsetPointer(
                                 leName,         /* usually the filename */
                                 SUMA_NODE_ROI,                /* mix and match */
                                 NULL,    /* no idcode, let the function create one from the filename*/
                                 NULL,       /* no domain str specified */
                                 N_Node    /* Number of nodes allocated for */
                                 ); /* DO NOT free dset, it is store in DsetList */
                           
	/* form the dataset */
   SUMA_LH("Adding NodeDef column ...");
   if (!SUMA_AddNelCol (   dset->nel, /* the famed nel */ 
                           "le Node Def", 
                           SUMA_NODE_INDEX,
                           (void *)NodeIndex, 
                           NULL,  
                           1 
                           )) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN(NULL);                    
   }
  
   if (!SUMA_AddNelCol (dset->nel, "Cluster Rank", SUMA_NODE_INT, (void *)Val, NULL ,1)) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN (NULL);
   }
   
   if (NodeIndex) SUMA_free(NodeIndex); NodeIndex = NULL;
   if (Val) SUMA_free(Val); Val = NULL;
   
   SUMA_RETURN (dset);
}

/*!
   \brief Finds a node that best approximates the property of the center of mass on the surface.
   Note that the real center of mass of a curved surface is rarely on the surface, so this is 
   an attempt at localizing a node that is central to an ROI and that can reflect the weight, 
   or activity, distribution in that ROI.
   
   \param SO (SUMA_SurfaceObject *)
   \param cd (SUMA_CLUST_DATUM *) basically the output of SUMA_SUMA_Build_Cluster_From_Node or 
                                    an element of the list returned by SUMA_FindClusters
   \param UseSurfDist (int) 0: use distances along the surface (approximated by distances on the graph)
                            1: use Euclidian distances. 
   \return ans (int) : NOPE failed, YUP succeeded.
*/
int SUMA_ClusterCenterofMass  (SUMA_SurfaceObject *SO, SUMA_CLUST_DATUM *cd, int UseSurfDist)
{
   static char FuncName[]={"SUMA_ClusterCenterofMass"};
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   SUMA_OFFSET_STRUCT OS;
   int *CoverThisNode = NULL, i, c, ni, nc, centralnode, weightedcentralnode, 
      nanch, k, WeightByValue = 1;
   float Uia[3], dia, Uca[3], dca,  s[3], s_w[3], *DistVec=NULL, 
         *weightedcentrality=NULL, minweightedcentrality = 0.0, *centrality=NULL, 
         mincentrality = 0.0, mtotal_w, mi_w, fac, mtotal, mi;   
   static int ncall;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   centralnode = -1;
   weightedcentralnode = -1;
   if (!SO || !cd) { SUMA_RETURN(NOPE); }
   if (!cd->N_Node || !cd->NodeList) { SUMA_RETURN(NOPE); }
   OffS = SUMA_Initialize_getoffsets (SO->N_Node);
   if (!OffS) {
      SUMA_SL_Err("Failed to allocate for OffS");
      SUMA_RETURN(NOPE);
   }

   CoverThisNode = (int *)SUMA_calloc(SO->N_Node, sizeof(int));
   if (!CoverThisNode) {
      SUMA_SL_Crit("Failed to allocate for CoverThisNode");
      SUMA_RETURN(NOPE);
   }
   for (i=0; i<cd->N_Node; ++i) { CoverThisNode[cd->NodeList[i]] = 1; }
   nanch = cd->NodeList[0]; /* anchor node */
   SUMA_getoffsets2 (cd->NodeList[0], SO, 0, OffS, CoverThisNode, cd->N_Node);
   /* help me with a nicer structure (for sanity)*/
   if (!SUMA_GetOffset2Offset (OffS, &OS)) {
      SUMA_SL_Err("Failed in SUMA_GetOffset2Offset");
      SUMA_RETURN(NOPE);
   }
   if (OffS) SUMA_Free_getoffsets(OffS); OffS = NULL;
   /* put the distance in an easy to search array */
   DistVec = (float *) SUMA_calloc(SO->N_Node, sizeof(float));
   centrality = (float *)SUMA_malloc(OS.N_Neighb * sizeof(float));
   weightedcentrality = (float *)SUMA_malloc(OS.N_Neighb * sizeof(float));
   if (!DistVec || !centrality || !weightedcentrality) {
      SUMA_SL_Crit("Failed to allocate.");
      SUMA_RETURN(NOPE);
   }
   for (c=0; c < OS.N_Neighb; ++c) { DistVec[OS.Neighb_ind[c]] = OS.Neighb_dist[c]; }
   
   /* Now calculate the center of massity of each node 
   This is no center of mass in the proper definition of the term*/
   
   #if 1
   if (cd->N_Node == 1) {
      centralnode = cd->NodeList[0];
      weightedcentralnode = cd->NodeList[0];
   } else {
      for (c=0; c < OS.N_Neighb; ++c) {
         nc = OS.Neighb_ind[c]; /* Node index into SO of center node */
         s[0] = s[1] = s[2] = 0.0; s_w[0] = s_w[1] = s_w[2] = 0.0; 
         centrality[c] = 0.0; weightedcentrality[c] = 0.0; mtotal_w = 0.0; /* recalculated each time, not a big deal ... */
         for (i=0; i<cd->N_Node; ++i) {
            mi_w = cd->ValueList[i];
            mtotal_w += mi_w; 
            ni = cd->NodeList[i]; /* Node index into SO of other node in cluster */
            SUMA_UNIT_VEC((&(SO->NodeList[3*ni])), (&(SO->NodeList[3*nanch])), Uia, dia);
            if (UseSurfDist) { for (k=0; k<3;++k) { fac = Uia[k] * DistVec[ni]; s_w[k] += mi_w * fac; s[k] += fac;} }
            else { for (k=0; k<3;++k) { fac = Uia[k] * dia; s_w[k] += mi_w * fac; s[k] += fac; } }
         }
         SUMA_UNIT_VEC((&(SO->NodeList[3*nc])), (&(SO->NodeList[3*nanch])), Uca, dca);
         if (UseSurfDist) { for (k=0; k<3;++k) { fac = Uca[k] * DistVec[nc]; s_w[k] -= mtotal_w * fac; s[k] -= cd->N_Node * fac;  } }
         else { for (k=0; k<3;++k) { fac =  Uca[k] * dca; s_w[k] -= mtotal_w * fac; s[k] -= cd->N_Node * fac;} }
         
         SUMA_NORM_VEC(s, 3, centrality[c]); SUMA_NORM_VEC(s_w, 3, weightedcentrality[c]); 
         if (LocalHead) fprintf(SUMA_STDERR,"%s: call %d, Centrality of node %d / %d = %f , weighted %f\n", FuncName, ncall, nc, nanch, centrality[c], weightedcentrality[c]); 
         if (c == 0) { 
            mincentrality = centrality[c]; centralnode = nc; 
            minweightedcentrality = weightedcentrality[c]; weightedcentralnode = nc; 
         } else {
            if (centrality[c] < mincentrality) { mincentrality = centrality[c]; centralnode = nc; }
            if (weightedcentrality[c] < minweightedcentrality) { minweightedcentrality = weightedcentrality[c]; weightedcentralnode = nc; }
         }
         
      }
   }
   cd->centralnode = centralnode;
   cd->weightedcentralnode = weightedcentralnode;
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Central node chosen %d, weighted %d\n", FuncName, cd->centralnode, cd->weightedcentralnode);
   #else
   if (cd->N_Node == 1) {
      centralnode = cd->NodeList[0];
   } else {
      for (c=0; c < OS.N_Neighb; ++c) {
         nc = OS.Neighb_ind[c]; /* Node index into SO of center node */
         s[0] = s[1] = s[2] = 0.0;
         centrality[c] = 0.0; mtotal = 0.0; /* recalculated each time, not a big deal ... */
         for (i=0; i<cd->N_Node; ++i) {
            if (WeightByValue) mi = cd->ValueList[i];
            else mi = 1.0;
            mtotal += mi;
            ni = cd->NodeList[i]; /* Node index into SO of other node in cluster */
            SUMA_UNIT_VEC((&(SO->NodeList[3*ni])), (&(SO->NodeList[3*nanch])), Uia, dia);
            if (UseSurfDist) { for (k=0; k<3;++k) s[k] += mi * Uia[k] * DistVec[ni]; }
            else { for (k=0; k<3;++k) s[k] += mi * Uia[k] * dia; }
         }
         SUMA_UNIT_VEC((&(SO->NodeList[3*nc])), (&(SO->NodeList[3*nanch])), Uca, dca);
         if (UseSurfDist) { for (k=0; k<3;++k) s[k] -= mtotal * Uca[k] * DistVec[nc]; }
         else { for (k=0; k<3;++k) s[k] -= mtotal * Uca[k] * dca; }
         
         SUMA_NORM_VEC(s, 3, centrality[c]);
         if (LocalHead) fprintf(SUMA_STDERR,"%s: call %d, Centrality of node %d / %d = %f\n", FuncName, ncall, nc, nanch, centrality[c]); 
         if (c == 0) { mincentrality = centrality[c]; centralnode = nc; }
         else if (centrality[c] < mincentrality) { mincentrality = centrality[c]; centralnode = nc; }
      }
   }
   cd->centralnode = centralnode;
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Central node chosen %d\n", FuncName, cd->centralnode);
   cd->weightedcentralnode = -1;
   #endif   
   
   if (CoverThisNode) SUMA_free(CoverThisNode); CoverThisNode = NULL;
   if (OS.Neighb_dist) SUMA_free(OS.Neighb_dist); OS.Neighb_dist = NULL;
   if (OS.Neighb_ind) SUMA_free(OS.Neighb_ind); OS.Neighb_ind = NULL;
   if (DistVec) SUMA_free(DistVec); DistVec = NULL;
   if (centrality) SUMA_free(centrality); centrality = NULL;
   if (weightedcentrality) SUMA_free(weightedcentrality); weightedcentrality = NULL;

   ++ncall;
   SUMA_RETURN(YUP);  
}  
 
SUMA_Boolean SUMA_Sort_ClustersList (DList *list, SUMA_SURF_CLUST_SORT_MODES SortMode)
{
   static char FuncName[]={"SUMA_Sort_ClustersList"};
   DListElmt *elmt=NULL, *max_elmt=NULL, *elmt_comp=NULL;
   SUMA_CLUST_DATUM *cd=NULL, *cd_comp=NULL, *cd_max=NULL;
   int r = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!list) { SUMA_RETURN(NOPE); }
   switch (SortMode) {
      case SUMA_SORT_CLUST_NOT_SET:
         SUMA_S_Err("Why are you calling me?");
         SUMA_RETURN(NOPE);
         break;
      case SUMA_SORT_CLUST_NO_SORT:
         SUMA_RETURN(YUP);
         break;
      case SUMA_SORT_CLUST_BY_NUMBER_NODES:
      case SUMA_SORT_CLUST_BY_AREA:
         elmt = NULL;
         do {
            if (!elmt) elmt = dlist_head(list);
            else elmt = elmt->next;
            cd = (SUMA_CLUST_DATUM *)elmt->data; 
            /* compare to all ahead of this element */
            if (elmt != dlist_tail(list)) {
               max_elmt = elmt; cd_max = (SUMA_CLUST_DATUM *)max_elmt->data; elmt_comp = NULL;
               do {
                  if (!elmt_comp) elmt_comp = elmt->next; 
                  else elmt_comp = elmt_comp->next; 
                  cd_comp = (SUMA_CLUST_DATUM *)elmt_comp->data; 
                  if (SortMode == SUMA_SORT_CLUST_BY_NUMBER_NODES) {
                     if (cd_comp->N_Node > cd_max->N_Node) { max_elmt = elmt_comp; cd_max = (SUMA_CLUST_DATUM *)max_elmt->data; }  
                  }else if (SortMode == SUMA_SORT_CLUST_BY_AREA) {
                     if (cd_comp->totalarea > cd_max->totalarea) { max_elmt = elmt_comp; cd_max = (SUMA_CLUST_DATUM *)max_elmt->data; }  
                  }
               } while (elmt_comp != dlist_tail(list));
               if (max_elmt != elmt) { /* max is the real deal */
                  dlist_remove(list, max_elmt, (void *)(&cd_max));
                  dlist_ins_prev(list, elmt, (void *)cd_max);
                  elmt = elmt->prev; /* continue from that one */
               }
            }
         } while (elmt != dlist_tail(list));
         SUMA_RETURN(YUP);
         break;
      default:
         break; 
   }
   
   
   
   SUMA_RETURN(YUP);
}
#ifdef SUMA_SurfClust_STANDALONE
void usage_SUMA_SurfClust ()
   {
      static char FuncName[]={"usage_SUMA_SurfClust"};
      char * s = NULL;
      s = SUMA_help_basics();
      printf ( "\nUsage: A program to check the quality of surfaces.\n"
               "  SurfClust <-spec SpecFile> [<-sv sv_name>]\n"
               "            <-surf_A insurf> [<-surf_B insurf>] \n"
               "            <-input inData.1D dcol_index> \n"
               "            <-rmm rad>\n"
               "            <-amm2>\n"
               "            [-prefix OUTPREF]  \n"
               "\n"
               "  Mandatory parameters:\n"
               "\n"
               "  Optional Parameters:\n"
               "     -prefix OUTPREF: Prefix for output.\n"
               "                      Default is the prefix of \n"
               "                      the input dataset.\n"
               "     -out_roidset: Output an ROI dataset.\n"
               "                   The ROI dataset's prefix has\n"
               "                   _ClstMsk affixed to the OUTPREF.\n"
               "     -out_fulllist: Force an output for all\n"
               "                    nodes of insurf.\n"
               "     -sort_none: No sorting of ROI clusters.\n"
               "     -sort_n_nodes: Sorting based on number of nodes\n"
               "                    in cluster.\n"
               "     -sort_area: Sorting based on area of clusters \n"
               "                 (default).\n"
               "\n"
               "%s"
               "\n", s);
               /* Can use -O2_NR, -O2 and -Oll to try different implementations 
               of the cluster building function. -O2 and -Oll are recursive and
               work well for small clusters, a catastrophy for large clusters.
               -O2_NR is fast and reliable. */
       SUMA_free(s); s = NULL;        
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
       exit (0);
   }

/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_SURFCLUST_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_SURFCLUST_OPTIONS *SUMA_SurfClust_ParseInput (char *argv[], int argc)
{
   static char FuncName[]={"SUMA_SurfClust_ParseInput"}; 
   SUMA_SURFCLUST_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outname;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_SURFCLUST_OPTIONS *)SUMA_malloc(sizeof(SUMA_SURFCLUST_OPTIONS));
   
   kar = 1;
   Opt->spec_file = NULL;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->N_surf = -1;
   Opt->DistLim = -1.0;
   Opt->AreaLim = -1.0;
   Opt->in_name = NULL;
   Opt->nodecol = -1;
   Opt->labelcol = -1;
   Opt->OutROI = NOPE;
   Opt->FullROIList = NOPE;
   Opt->WriteFile = NOPE;
   Opt->SortMode = SUMA_SORT_CLUST_NOT_SET;
   for (i=0; i<SURFCLUST_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
   BuildMethod = SUMA_OFFSETS2_NO_REC;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_SurfClust();
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-O2") == 0)) {
         BuildMethod = SUMA_OFFSETS2;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-O2_NR") == 0)) {
         BuildMethod = SUMA_OFFSETS2_NO_REC;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-Oll") == 0)) {
         BuildMethod = SUMA_OFFSETS_LL;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec \n");
				exit (1);
			}
			Opt->spec_file = argv[kar];
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-sv") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -sv \n");
				exit (1);
			}
			Opt->sv_name = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix \n");
				exit (1);
			}
			Opt->out_prefix = SUMA_copy_string(argv[kar]);
			Opt->WriteFile = YUP;
         brk = YUP;
		}
            
      if (!brk && (strncmp(argv[kar], "-surf_", 6) == 0)) {
			if (kar + 1>= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -surf_X SURF_NAME \n");
				exit (1);
			}
			ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFCLUST_MAX_SURF) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option is out of range.\n");
				exit (1);
         }
         kar ++;
         Opt->surf_names[ind] = argv[kar];
         Opt->N_surf = ind+1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 arguments after -input \n");
				exit (1);
			}
			Opt->in_name = argv[kar]; kar ++;
         /* no need for that one Opt->nodecol = atoi(argv[kar]); kar ++; */
         Opt->labelcol = atoi(argv[kar]); 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-rmm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -rmm \n");
				exit (1);
			}
			Opt->DistLim = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-amm2") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -amm2 \n");
				exit (1);
			}
			Opt->AreaLim = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-out_roidset") == 0)) {
         Opt->OutROI = YUP;
			brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-out_fulllist") == 0)) {
         Opt->FullROIList = YUP;
			brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-sort_none") == 0)) {
         Opt->SortMode = SUMA_SORT_CLUST_NO_SORT;
			brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-sort_n_nodes") == 0)) {
         Opt->SortMode = SUMA_SORT_CLUST_BY_NUMBER_NODES;
			brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-sort_area") == 0)) {
         Opt->SortMode = SUMA_SORT_CLUST_BY_AREA;
			brk = YUP;
      }
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   /* sanitorium */
   if (Opt->DistLim < 0) {
      fprintf (SUMA_STDERR, "must use options -rmm  \n");
      exit(1);
   }
   if (!Opt->out_prefix) {
      Opt->out_prefix = SUMA_RemoveDsetExtension(Opt->in_name, SUMA_NO_DSET_FORMAT);
   }
   
   if (Opt->SortMode == SUMA_SORT_CLUST_NOT_SET) { Opt->SortMode = SUMA_SORT_CLUST_BY_AREA; }

   if (BuildMethod == SUMA_OFFSETS2) { SUMA_S_Note("Using Offsets2"); }
   else if (BuildMethod == SUMA_OFFSETS_LL) { SUMA_S_Note("Using Offsets_ll"); } 
   else if (BuildMethod == SUMA_OFFSETS2_NO_REC) { if (LocalHead) SUMA_S_Note("Using no recursion"); }
   else {
      SUMA_SL_Err("Bad BuildMethod");
      exit(1);
   } 
   
   SUMA_RETURN(Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfClust"}; 
	int kar, SO_read, *ni=NULL, N_ni;
   float *data_old = NULL, *far = NULL, *nv=NULL;
   void *SO_name = NULL;
   SUMA_SurfaceObject *SO = NULL, *SOnew = NULL;
   MRI_IMAGE *im = NULL;
   SUMA_DSET_FORMAT iform;
   SUMA_SURFCLUST_OPTIONS *Opt;  
	SUMA_SurfSpecFile Spec; 
   DList *list = NULL;
   SUMA_DSET *dset = NULL;
   float *NodeArea = NULL;
   FILE *clustout=NULL;
   char *ClustOutName = NULL, *params=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;
   
   
   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   if (argc < 6)
       {
          usage_SUMA_SurfClust();
          exit (1);
       }
   
   Opt = SUMA_SurfClust_ParseInput (argv, argc);

   if (Opt->WriteFile) {
      ClustOutName = SUMA_append_string(Opt->out_prefix, "_Clst.1D");   
      if (SUMA_filexists(ClustOutName)) {
         fprintf (SUMA_STDERR,"Error %s:\nOutput file %s exists, will not overwrite.\n", FuncName, ClustOutName);
         exit(1);
      }
   }

   /* read  surfaces */
   if (!SUMA_Read_SpecFile (Opt->spec_file, &Spec)) {
		fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
		exit(1);
	}
   SO_read = SUMA_spec_select_surfs(&Spec, Opt->surf_names, SURFCLUST_MAX_SURF, 0);
   if ( SO_read != Opt->N_surf )
   {
	   if (SO_read >=0 )
         fprintf(SUMA_STDERR,"Error %s:\nFound %d surfaces, expected %d.\n", FuncName,  SO_read, Opt->N_surf);
      exit(1);
   }
   /* now read into SUMAg_DOv */
   if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, Opt->sv_name, 0, SUMAg_CF->DsetList) ) {
	   fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
      exit(1);
   }
   SO = SUMA_find_named_SOp_inDOv(Opt->surf_names[0], SUMAg_DOv, SUMAg_N_DOv);
   NodeArea = SUMA_CalculateNodeAreas(SO);
   if (!NodeArea) {
      SUMA_S_Err("Failed to calculate Node Areas.\n");
      exit(1);
   }   
   /* load the data */   
   iform = SUMA_NO_DSET_FORMAT;
   dset = SUMA_LoadDset (Opt->in_name, &iform, 0); 
   if (!dset) { SUMA_S_Err(  "Failed to load dataset.\n"
                                 "Make sure file exists\n"
                                 "and is of the specified\n"
                                 "format."); 
               exit(1); }
   if (!SUMA_OKassign(dset, SO)) {
      SUMA_SL_Err("Failed to assign data set to surface.");
      exit(1);
   }
   if (LocalHead) SUMA_ShowDset(dset, 0, NULL);
   /* get the node index column */
   ni = SUMA_GetNodeDef(dset);
   N_ni = SDEST_VECLEN(dset);
   if (!ni) {
      SUMA_S_Err("Failed to find node index column");
      exit(1);
   }
   nv = SUMA_Col2Float(dset->nel, Opt->labelcol, 0);
   if (!nv) {
      SUMA_S_Err("Failed to find node value column");
      exit(1);
   }
   
   /* make the call */
   list = SUMA_FindClusters (SO, ni, nv, N_ni, -1, Opt, NodeArea);
   
   /* sort the list */
   if (!SUMA_Sort_ClustersList (list, Opt->SortMode)) {
      SUMA_S_Err("Failed to sort cluster list");
      exit(1);
   }
       
   /* Show the results */
   params = SUMA_HistString(FuncName, argc, argv, NULL);
   if (Opt->WriteFile) {
      clustout = fopen(ClustOutName, "w");
      if (!clustout) {
         fprintf (SUMA_STDERR,"Error %s:\nFailed to open %s for writing.\nCheck permissions.\n",  FuncName, ClustOutName);
         exit(1);
      }
      SUMA_Show_SurfClust_list(list, clustout, 0, params);
      fclose(clustout);clustout = NULL;  
   }  else SUMA_Show_SurfClust_list(list, NULL, 0, params);
   
   
   if (Opt->OutROI) {
      SUMA_DSET *dset_roi = NULL;
      char *ROIprefix = NULL;
      char *NameOut = NULL;
      ROIprefix = SUMA_append_string(Opt->out_prefix, "_ClstMsk");
      /* Call this function, write out the resultant dset to disk then cleanup */
      if (Opt->FullROIList) {
         dset_roi = SUMA_SurfClust_list_2_Dset(SO, list, YUP, ROIprefix);
      } else {
         dset_roi = SUMA_SurfClust_list_2_Dset(SO, list, NOPE, ROIprefix);
      }
      if (!dset_roi) {
         SUMA_S_Err("NULL dset_roi");
         exit(1);
      }
      NameOut = SUMA_WriteDset (ROIprefix, dset_roi, SUMA_ASCII_NIML, 0, 0);
      if (!NameOut) { SUMA_SL_Err("Failed to write dataset."); exit(1); } 
      SUMA_FreeDset((void *)dset_roi); dset_roi = NULL; 
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
      if (ROIprefix) SUMA_free(ROIprefix); ROIprefix = NULL; 
   }
   
   if (ClustOutName) SUMA_free(ClustOutName); ClustOutName = NULL;
   if (list) dlist_destroy(list); SUMA_free(list); list = NULL;
   if (nv) SUMA_free(nv); nv = NULL;
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt) SUMA_free(Opt);
   if (dset) SUMA_FreeDset((void *)dset); dset = NULL;
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   exit(0);
}
#endif
