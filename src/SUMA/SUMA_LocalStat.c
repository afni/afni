#include "SUMA_suma.h"


extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  

static int BuildMethod = SUMA_OFFSETS2_NO_REC;

void SUMA_SurfClust_Set_Method(int m)
{
   BuildMethod = m;
   return;
}
int SUMA_SurfClust_Get_Method(void) 
{ return(BuildMethod); }

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
   mask: if not null, then calculate areas for nodes in mask only
   If you change this function make sure changes are also done
   on RickR's compute_node_areas since the two functions use
   the same principle.
*/
float *SUMA_CalculateNodeAreas(SUMA_SurfaceObject *SO, byte *mask)
{
   static char FuncName[]={"SUMA_CalculateNodeAreas"};
   float *NodeAreas=NULL;
   int *flist = NULL, i, c;
   
   SUMA_ENTRY;
   
   if (!SO) { SUMA_RETURN(NodeAreas); }
   if (!SO->PolyArea || !SO->MF) {
      if (!SUMA_SurfaceMetrics_eng(SO, "PolyArea|MemberFace", NULL, 0, SUMAg_CF->DsetList)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         SUMA_RETURN(NodeAreas);
      }
   }
   
   NodeAreas = (float *)SUMA_malloc(SO->N_Node*sizeof(float));
   if (!NodeAreas) { SUMA_SL_Crit ("Failed to allocate for NodeAreas"); SUMA_RETURN(NodeAreas); }
   
   for (i=0; i<SO->N_Node; ++i) {
      NodeAreas[i] = 0.0;
      if (!mask || mask[i]) {
         flist = SO->MF->NodeMemberOfFaceSet[i];
         for (c = 0; c < SO->MF->N_Memb[i]; c++) {
            NodeAreas[i] += SO->PolyArea[flist[c]];
         }
         NodeAreas[i] /= 3.0;
      }
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
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node(
               int dothisnode, SUMA_CLUST_DATUM *AddToThisClust, 
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
      Clust->varvalue = 0.0;  Clust->centralnode = 0; 
      Clust->weightedcentralnode = 0; 
      Clust->NodeList = (int *)SUMA_malloc((*N_TobeAssigned) * sizeof(int)); 
      Clust->ValueList = (float *)SUMA_malloc((*N_TobeAssigned) * sizeof(float));  
      if (!Clust->NodeList || !Clust->ValueList) { 
         SUMA_SL_Crit("Failed to allocate for NodeList or ValueList");  
         SUMA_free(Clust); Clust = NULL;
         SUMA_RETURN(NULL);  
      }
      NewClust = 1;
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: New Cluster     %p, with node %d\n", 
                              FuncName, Clust, dothisnode);
   } else { 
      NewClust = 0;
      Clust = AddToThisClust; 
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Reusing Cluster %p, with node %d\n", 
                              FuncName, Clust, dothisnode);
   }
   
   /* Add node to cluster */
   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s: Adding node %d to cluster %p of %d nodes\n", 
                          FuncName, dothisnode, Clust, Clust->N_Node);
   Clust->NodeList[Clust->N_Node] = dothisnode; 
   Clust->totalarea += NodeArea[dothisnode]; 
   Clust->totalvalue += ToBeAssigned[dothisnode];
   Clust->totalabsvalue += (float)fabs((float)ToBeAssigned[dothisnode]);
   if (ToBeAssigned[dothisnode] < Clust->minvalue) { 
      Clust->minvalue = ToBeAssigned[dothisnode]; 
      Clust->minnode = dothisnode; 
   }
   if (ToBeAssigned[dothisnode] > Clust->maxvalue) { 
      Clust->maxvalue = ToBeAssigned[dothisnode]; 
      Clust->maxnode = dothisnode; 
   }
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
               SUMA_SL_Err("Could not open file for writing.\n"
                           "Check file permissions, disk space.\n");
            } else {
               s = SUMA_ShowOffset_Info(OffS, 0);
               if (s) { fprintf(fid,"%s\n", s);  SUMA_free(s); s = NULL;}
               fclose(fid);
            }      
         }
         #endif
         /* search to see if any are to be assigned */
         if (Opt->DistLim >= 0.0) {
            for (il=1; il<OffS->N_layers; ++il) { 
                                          /* starting at layer 1, 
                                             layer 0 is the node itself */
               for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) {
                  neighb = OffS->layers[il].NodesInLayer[jl];
                  if (  ToBeAssigned[neighb] && 
                        OffS->OffVect[neighb] <= Opt->DistLim) {
                        /* take that node into the cluster */
                        SUMA_Build_Cluster_From_Node( 
                                    neighb, Clust, ToBeAssigned, 
                                    N_TobeAssigned, NodeArea, SO, Opt); 
                  }
               }
            }
         } else { /* accept nodes connect by -((int)Opt->DistLim) edges or less*/
            for (il=1; il <= -((int)Opt->DistLim); ++il) {
               for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) {
                  neighb = OffS->layers[il].NodesInLayer[jl];
                  if (  ToBeAssigned[neighb] ) {
                        /* take that node into the cluster */
                        SUMA_Build_Cluster_From_Node( 
                                    neighb, Clust, ToBeAssigned, 
                                    N_TobeAssigned, NodeArea, SO, Opt); 
                  }
               }
            }
         }
         /* free this OffS structure (Note you can't recycle the same 
            structure because you are using many OffS at one because 
            of recursive calls */
         if (OffS) SUMA_Free_getoffsets(OffS); OffS = NULL;
      }
   } else if (BuildMethod == SUMA_OFFSETS_LL) { 
      if (*N_TobeAssigned) {
         /* look in its vicinity */
         if (!(offlist = SUMA_getoffsets_ll (dothisnode, SO, Opt->DistLim, 
                                             NULL, 0))) {
            SUMA_SL_Err("Failed to get offsets.\nNo cleanup done.");
            SUMA_RETURN(NULL);
         }
         #if 0
         if (NewClust) {
            FILE *fid=NULL;
            char *s=NULL, tmp[50];
            fid = fopen("offsets_ll.1D", "w"); 
            if (!fid) {
               SUMA_SL_Err("Could not open file for writing.\n"
                           "Check file permissions, disk space.\n");
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
            if (ToBeAssigned[neighb]) {
               if (dat->off <= Opt->DistLim) {
                  /* take that node into the cluster */
                  SUMA_Build_Cluster_From_Node( neighb, Clust, ToBeAssigned, 
                                                N_TobeAssigned, NodeArea, SO, 
                                                Opt); 
               } else if (dat->layer <= -(int)Opt->DistLim) {
                  /* take that node into the cluster */
                  SUMA_Build_Cluster_From_Node( neighb, Clust, ToBeAssigned, 
                                                N_TobeAssigned, NodeArea, SO, 
                                                Opt);
               }
            }      
         } while (elm != dlist_tail(offlist));
         dlist_destroy(offlist); SUMA_free(offlist); offlist = NULL;   
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
   static int N_Orig = -1;
   int il, jl, neighb;
   void *dtmp=NULL;
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   DList *offlist = NULL, *candlist=NULL;
   DListElmt *elm = NULL, *dothiselm=NULL;
   SUMA_OFFSET_LL_DATUM *dat=NULL;
   int NewClust = 0;
   byte *visited=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   ++ncall;
   /* a trick to know when to initialize */
   if (Opt->update < 0) { N_Orig = *N_TobeAssigned; Opt->update = -Opt->update; }
   
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
      Clust->varvalue = 0.0;  Clust->centralnode = 0; 
      Clust->weightedcentralnode = 0;
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
   dlist_ins_next(candlist, dlist_tail(candlist), (VOID_CAST)dothisnode);
      while (*N_TobeAssigned && dlist_size(candlist)) {
         /* look in its vicinity */
         dothiselm = dlist_head(candlist); 
         dothisnode = (INT_CAST) dothiselm->data;
         SUMA_getoffsets2 (dothisnode, SO, Opt->DistLim, OffS, NULL, 0);
         /* remove node from candidate list */
         dlist_remove(candlist, dothiselm, (void*)&dtmp);
         /* search to see if any are to be assigned */
         if (Opt->DistLim >= 0.0) {
            for (il=1; il<OffS->N_layers; ++il) { 
               /*  starting at layer 1, layer 0 is the node itself */
               for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) {
                  neighb = OffS->layers[il].NodesInLayer[jl];
                  if (  ToBeAssigned[neighb] && 
                        OffS->OffVect[neighb] <= Opt->DistLim) {
                     /* take that node into the cluster */
                     SUMA_ADD_NODE_TO_CLUST( neighb, Clust, 
                                             NodeArea, ToBeAssigned);
                     /* mark it as assigned, an reduce the number 
                        of nodes left to assign*/
                     ToBeAssigned[neighb] = 0; --(*N_TobeAssigned);
                     if (Opt->update) {
                        if (N_Orig - *N_TobeAssigned >= Opt->update) {
                           if (LocalHead) 
                              fprintf( SUMA_STDERR,
                                       "%s: tick (%d nodes processed)\n", 
                                       FuncName, N_Orig - *N_TobeAssigned); 
                           else fprintf(SUMA_STDERR,".");

                           N_Orig = *N_TobeAssigned;
                        }
                     }
                     /* mark it as a candidate if it has not been visited as 
                        a candidate before */
                     if (!visited[neighb]) {
                        dlist_ins_next(candlist, dlist_tail(candlist), 
                                       (VOID_CAST)neighb);
                        visited[neighb] = YUP;   
                     }
                  }
               }
            }
         } else {/* accept nodes connected by up to -((int)Opt->DistLim) edges */
            for (il=1; il <= -((int)Opt->DistLim); ++il) {
               for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) {
                  neighb = OffS->layers[il].NodesInLayer[jl];
                  if (  ToBeAssigned[neighb] ) {
                        /* take that node into the cluster */
                     SUMA_ADD_NODE_TO_CLUST( neighb, Clust, 
                                             NodeArea, ToBeAssigned);
                     /* mark it as assigned, an reduce the number 
                        of nodes left to assign*/
                     ToBeAssigned[neighb] = 0; --(*N_TobeAssigned);
                     if (Opt->update) {
                        if (N_Orig - *N_TobeAssigned >= Opt->update) {
                           if (LocalHead) 
                              fprintf( SUMA_STDERR,
                                       "%s: tick (%d nodes processed)\n", 
                                       FuncName, N_Orig - *N_TobeAssigned); 
                           else fprintf(SUMA_STDERR,".");

                           N_Orig = *N_TobeAssigned;
                        }
                     }
                     /* mark it as a candidate if it has not been visited as 
                        a candidate before */
                     if (!visited[neighb]) {
                        dlist_ins_next(candlist, dlist_tail(candlist), 
                                       (VOID_CAST)neighb);
                        visited[neighb] = YUP;   
                     }
                  }
               }
            }
         }
         /* recycle */
         SUMA_Recycle_getoffsets (OffS);
      }
   /* free this OffS structure  */
   if (OffS) SUMA_Free_getoffsets(OffS); OffS = NULL;
   if (Opt->update) fprintf(SUMA_STDERR,"\n");
   /* destroy the list */
   if (candlist) { dlist_destroy(candlist); SUMA_free(candlist); candlist  = NULL; }
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
DList *SUMA_FindClusters ( SUMA_SurfaceObject *SO, int *ni, 
                           float *nv, int N_ni, 
                           int dothisnode, SUMA_SURFCLUST_OPTIONS *Opt, 
                           float *NodeArea)
{
   static char FuncName[]={"SUMA_FindClusters"};
   DList *list=NULL;
   DListElmt *elm=NULL;
   float *ToBeAssigned=NULL;
   float mean;
   int N_n, nc, i, kk, PureNothing=0;
   SUMA_CLUST_DATUM *Clust = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !nv || !ni) {
      SUMA_S_Err("Bad parameters");
      SUMA_RETURN(list);
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
         if (!nv[i]) {
            ++PureNothing;
         }
         ToBeAssigned[ni[i]] = nv[i];
      }
      if (Opt->update) {
         fprintf( SUMA_STDERR,
                  "%s: Have %d nodes to work with. %d nodes have 0 value.\n",
                  FuncName, N_n, PureNothing);
      }
   }
   
   while (N_n - PureNothing > 0) {
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
         Clust = SUMA_Build_Cluster_From_Node_NoRec(  dothisnode, 
                                                      ToBeAssigned, 
                                                      &N_n, NodeArea, 
                                                      SO, Opt);
      } else if ( BuildMethod == SUMA_OFFSETS2 || 
                  BuildMethod == SUMA_OFFSETS_LL) {
         Clust = SUMA_Build_Cluster_From_Node(  dothisnode, NULL, 
                                                ToBeAssigned, 
                                                &N_n, NodeArea, 
                                                SO, Opt);
      } else {
         SUMA_S_Errv("No Such Method (%d)!\n", BuildMethod);
         SUMA_DUMP_TRACE(FuncName);
         SUMA_RETURN(list);
      }
      if (!Clust) {
         SUMA_SL_Err("Failed in SUMA_Build_Cluster_From_Node*");
         SUMA_RETURN(list);
      }   
      if (LocalHead) fprintf( SUMA_STDERR,
                              "%s: Cluster %p is finished, %d nodes\n",
                              FuncName, Clust, Clust->N_Node); 
      
      if ( (Opt->AreaLim > 0 && Clust->totalarea < Opt->AreaLim)  || 
           (Opt->NodeLim > 0 && Clust->N_Node < Opt->NodeLim) ) {
         SUMA_LH("Cluster less than area (or node number) limit");
         SUMA_FreeClustDatum((void *)Clust); Clust = NULL;
      } else {
         mean = Clust->totalvalue/((float)Clust->N_Node);
         for (kk=0; kk < Clust->N_Node; ++kk) {
            Clust->varvalue += 
               (Clust->ValueList[kk] - mean) * (Clust->ValueList[kk] - mean);   
         }
         if (Clust->N_Node > 1) Clust->varvalue /= (Clust->N_Node - 1);
         else Clust->varvalue = 0.0;
         /* reallocate to save space */
         Clust->NodeList = 
            (int *)SUMA_realloc(Clust->NodeList, sizeof(int)*Clust->N_Node);
         Clust->ValueList = 
            (float *)SUMA_realloc(  Clust->ValueList,
                                    sizeof(float)*Clust->N_Node);
         if (!Clust->NodeList || !Clust->ValueList) { 
            SUMA_SL_Crit("Failed to reallocate for NodeList or ValueList");  
            SUMA_RETURN(NULL);   
         }
         /* find the central node */
         if (Opt->DoCentrality) {
            if (Opt->update) {
                  SUMA_SL_Note(  "Looking for central nodes...\n"
                                 "(use -no_cent to skip this slow step)");
            } 
            if (!SUMA_ClusterCenterofMass  (SO, Clust, 1)) {
               SUMA_SL_Err("Failed to find central node");  
               SUMA_RETURN(list);   
            }
         }

         dlist_ins_next(list, dlist_tail(list), (void *)Clust); 
         ++nc;
      }
   }   
  
   if (N_n == 0) {
      if (LocalHead) fprintf( SUMA_STDERR,
                              "%s: No more nodes to consider, cleaning up.\n",
                              FuncName);
      if (ToBeAssigned) SUMA_free(ToBeAssigned); ToBeAssigned = NULL;  
   }
   
   SUMA_RETURN(list);
} 

/*! Show the ViewState structure */
SUMA_Boolean SUMA_Show_SurfClust_list(
                  DList *list, FILE *Out, int detail, char *params) 
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
   char Col[][12] = { 
      {"# Rank"}, {"num Nd"}, {"Area"}, {"Mean"}, 
      {"|Mean|"},{"Cent"}, {"W Cent"},{"Min V"}, 
      {"Min Nd"}, {"Max V"}, {"Max Nd"} , {"Var"}, {"SEM"} };
   SUMA_ENTRY;

   SS = SUMA_StringAppend (NULL, NULL);
   
   if (!list) {
      SS = SUMA_StringAppend (SS,"NULL cluster list.\n");
      SUMA_SS2S(SS,s); 
      SUMA_RETURN(s);  
   }

   if (!list->size) {
      SS = SUMA_StringAppend (SS,"Empty cluster list.\n");
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
         SS = SUMA_StringAppend_va (
                  SS,"#Other columns: list of 5 first nodes in ROI.\n");   
      }
      if (detail == 2) {
         SS = SUMA_StringAppend_va (
                  SS,"#Other columns: list all  nodes in ROI.\n");   
      }
      if (detail > 0) {
         SS = SUMA_StringAppend_va (
                  SS,"#A total of %d cluster%s were found.\n", 
                  list->size, SUMA_COUNTER_PLURAL(list->size));
      }
   }
   
   elmt = NULL; 
   ic = 1; 
   do {
      if (!elmt) elmt = dlist_head(list); else elmt = elmt->next;
      if (!elmt) SS = SUMA_StringAppend_va (
                           SS,"#%d%s cluster element is NULL!\n", 
                           ic, SUMA_COUNTER_SUFFIX(ic));
      else {
         cd = (SUMA_CLUST_DATUM *)elmt->data;
         if (detail > 0) SS = SUMA_StringAppend_va (
                                 SS,"#%d%s cluster\n", 
                                 ic, SUMA_COUNTER_SUFFIX(ic));
         SS = SUMA_StringAppend_va (SS, 
                  "%6d   %6d   %9.2f"
                  "   %9.3f   %9.3f"
                  "   %6d   %6d"
                  "   %9.3f   %6d"
                  "   %9.3f   %6d"
                  "   %9.3f   %9.3f"
                  , ic, cd->N_Node, cd->totalarea
                  , cd->totalvalue/((float)cd->N_Node)
                  , cd->totalabsvalue/((float)cd->N_Node)
                  , cd->centralnode, cd->weightedcentralnode 
                  , cd->minvalue, cd->minnode
                  , cd->maxvalue, cd->maxnode
                  , cd->varvalue, sqrt(cd->varvalue/cd->N_Node));
         if (detail > 0) {
            if (detail == 1) {
               if (cd->N_Node < 5) max = cd->N_Node; else max = 5;
            } else max = cd->N_Node;
            for (i=0;i<max; ++i) 
               SS = SUMA_StringAppend_va (SS,"%d\t", cd->NodeList[i]);
         }
         SS = SUMA_StringAppend(SS,"\n"); 
      }
      ++ic; 
   } while (elmt != dlist_tail(list));
   
   SUMA_SS2S(SS,s);
   
   SUMA_RETURN (s);
}

/*! Masks a data set by a clustering list*/
SUMA_DSET *SUMA_MaskDsetByClustList(
   SUMA_DSET *idset, SUMA_SurfaceObject *SO, 
   DList *list, SUMA_Boolean FullList, char *leName) 
{
   static char FuncName[]={"SUMA_MaskDsetByClustList"};
   int i, j;
   DListElmt *elmt=NULL;
   SUMA_DSET *dset = NULL;
   int *ni=NULL, N_ni, cnt;
   SUMA_CLUST_DATUM *cd=NULL;
   byte *ismask=NULL, *rowmask=NULL, *colmask = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!list || list->size == 0) {
      SUMA_SL_Err("NULL or empty list");
      SUMA_RETURN(dset);  
   }
   /* which nodes in mask ? */
   ismask = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte)); 
               /* you need to allocate for SO->N_Node, 
                 to be safe, otherwise you will need
                 to search for the highest node index.. */
   elmt = NULL; cnt = 0;
   do {
      if (!elmt) elmt = dlist_head(list);
      else elmt = elmt->next;
      cd = (SUMA_CLUST_DATUM *)elmt->data; 
      for (j=0; j<cd->N_Node; ++j) {
            if(LocalHead) fprintf (SUMA_STDERR,"nic=%d\t", cd->NodeList[j]);
            ismask[cd->NodeList[j]] = 1;
            ++cnt;
      }
   } while (elmt != dlist_tail(list));
   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s:\n%d nodes in cluster list.\n", FuncName, cnt);
   
   /* now form a rowmask vector to parallel rows in idset */
   rowmask = (byte *)SUMA_calloc(SDSET_VECLEN(idset), sizeof(byte));
   colmask = (byte *)SUMA_calloc(SDSET_VECNUM(idset), sizeof(byte));
   
   /* get the node index column in idset */
   ni = SUMA_GetNodeDef(idset);
   N_ni = SDSET_VECLEN(idset);
   if (!ni) {
      SUMA_SL_Err("Failed to find node index column");
      SUMA_RETURN(NULL);
   }
   /* now, fill rowmask */
   for (i=0; i<SDSET_VECLEN(idset); ++i) { 
      if (ismask[ni[i]]) { 
         rowmask[i]=1; 
         if(LocalHead) fprintf (SUMA_STDERR,"%d,%d\t", ni[i], i); 
      } 
   }
   /* fill colmask*/
   for (i=0; i<SDSET_VECNUM(idset); ++i) { 
      if (SUMA_isDsetColumn_inferred(idset, i)) {
         colmask[i]=0;
         if (LocalHead) 
            fprintf( SUMA_STDERR,
                     "%s: Column %d will not be written "
                     "because it is inferred.\n",
                     FuncName, i);
      } else colmask[i]=1;
   }
   
   dset = SUMA_MaskedCopyofDset(idset, rowmask, colmask, !FullList, 1);
   if (!dset) {
      SUMA_SL_Err("Failed to create masked copy of input");
      SUMA_RETURN(NULL);
   }
   
   if (rowmask) SUMA_free(rowmask); rowmask = NULL;
   if (colmask) SUMA_free(colmask); colmask = NULL;
   if (ismask) SUMA_free(ismask); ismask = NULL;
   
   SUMA_RETURN (dset);
}
/*! Turn the clusters to a cluster dataset mask*/
SUMA_DSET *SUMA_SurfClust_list_2_DsetMask(
                  SUMA_SurfaceObject *SO, DList *list, 
                  SUMA_Boolean FullList, char *leName) 
{
   static char FuncName[]={"SUMA_SurfClust_list_2_DsetMask"};
   int i, ic, max, j, rank;
   DListElmt *elmt=NULL;
   SUMA_DSET *dset = NULL;
   int *NodeIndex=NULL, N_Node, *Val = NULL;
   SUMA_CLUST_DATUM *cd=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!list || list->size == 0) {
      SUMA_SL_Err("NULL or empty list");
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
               NULL,    /* no idcode, let function create one from filename*/
               NULL,       /* no domain str specified */
               N_Node    /* Number of nodes allocated for */
               ); /* DO NOT free dset, it is store in DsetList */
                           
	/* form the dataset */
   SUMA_LH("Adding NodeDef column ...");
   if (!SUMA_AddDsetNelCol (   
                           dset,  
                           "le Node Def", 
                           SUMA_NODE_INDEX,
                           (void *)NodeIndex, 
                           NULL,  
                           1 
                           )) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN(NULL);                    
   }
  
   if (!SUMA_AddDsetNelCol (  dset, "Cluster Rank", 
                              SUMA_NODE_INT, (void *)Val, NULL ,1)) {
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
                                  Function will fill centralnode and weightedcentralnode
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
   if (cd->N_Node == SO->N_Node) {
      /* not much to do, return 0 */
      SUMA_SL_Note("Cluster spans entire surface.\nNo central node.\n");
      cd->centralnode = 0;
      cd->weightedcentralnode = 0;
      SUMA_RETURN(YUP);
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

#define SUMA_WORLD_STATS_NODE_DBG { \
   if (n == ndbg ) {  \
      if (OffS_out) {  \
          FILE *mf ;    \
          char *mfname = SUMA_append_replace_num("SUMA_SurfLocalstat", "_node%ddbg_Col_", n, SUMA_int, 0);   \
          mfname = SUMA_append_replace_string(mfname, lblcp, "", 1); \
          mfname = SUMA_append_replace_string(mfname, ".1D.dset", "", 1); \
          SUMA_NICEATE_FILENAME(mfname,'\0');   \
          mf=fopen(mfname,"w");   \
          if (!mf) { SUMA_S_Errv("Failed to open %s for writing.\n", mfname); }   \
          else {   \
             fprintf(mf, "#Node %d in mask, total of %d neighbs of which %d went into output of (%f).\n",    \
                   n, OffS_out[n].N_Neighb, nval-1, fout[n]);   \
             if (nmask) {\
               fprintf(mf, "#nmask in use, nmask[n]=%d, strict_mask = %d\n", nmask[n], strict_mask); \
               fprintf(mf, "#Col. 0: Node index of neighbor (1st row is debug node itself)\n"); \
               fprintf(mf, "#Col. 1: Graph distance of neighbor from debug node\n");   \
               fprintf(mf, "#Col. 2: Neighbor value\n"); \
               fprintf(mf, "#Col. 3: nmask value of neighbor (see strict_mask flag also)\n"); \
               fprintf(mf, "%6d\t%+2.3f\t%+2.3f\t%2d\n", n, 0.0, fin_orig[n], nmask[n]);  \
             } else { \
               fprintf(mf, "#No masking\n"); \
               fprintf(mf, "#Col. 0: Node index of neighbor (1st row is debug node itself)\n"); \
               fprintf(mf, "#Col. 1: Graph distance of neighbor from debug node\n");   \
               fprintf(mf, "#Col. 2: Neighbor value\n"); \
               fprintf(mf, "%6d\t%+2.3f\t%+2.3f\n", n, 0.0, fin_orig[n]);  \
             } \
             if (!nmask) {  \
                for (j=0; j<OffS_out[n].N_Neighb; ++j) {  \
                   nj = OffS_out[n].Neighb_ind[j];  \
                   if (OffS_out[n].Neighb_dist[j] <= rhood) { fprintf(mf, "%6d\t%+2.3f\t%+2.3f\n", nj, OffS_out[n].Neighb_dist[j], fin_orig[nj]);  }\
                }/* for j*/ \
             } else {   \
                for (j=0; j<OffS_out[n].N_Neighb; ++j) {  \
                   nj = OffS_out[n].Neighb_ind[j];  \
                   if (nmask[nj] || !strict_mask) { \
                     if (OffS_out[n].Neighb_dist[j] <= rhood) { fprintf(mf, "%6d\t%+2.3f\t%+2.3f\t%2d\n", nj, OffS_out[n].Neighb_dist[j], fin_orig[nj], nmask[nj]);  }\
                   }\
                }/* for j*/ \
             } \
             SUMA_S_Notev("Node %d in mask, total of %d neighbs of which %d went into output of (%f).\nSee also %s\n", \
                           n, OffS_out[n].N_Neighb, nval-1, fout[n], mfname);\
             SUMA_free(mfname); mfname=NULL;\
             fclose(mf); mf=NULL; \
          }  \
      } else { \
         SUMA_S_Err("Ihr Idioten!\nThis debug macro is for the offset method!"); \
         {  \
            FILE *mf ;    \
            char *mfname = SUMA_append_replace_num("SUMA_SurfLocalstat", "_node%ddbg_Col_", n, SUMA_int, 0);   \
            mfname = SUMA_append_replace_string(mfname, lblcp, "", 1); \
            mfname = SUMA_append_replace_string(mfname, ".1D.dset", "", 1); \
            SUMA_NICEATE_FILENAME(mfname,'\0');   \
             mf=fopen(mfname,"w");   \
             if (!mf) { SUMA_S_Errv("Failed to open %s for writing.\n", mfname); }   \
             else {   \
                fprintf(mf, "#Node %d in mask, total of %d neighbs in approx mask.\n",    \
                      n, nval);\
                for (j=0; j<SO->N_Node; ++j) { if (fwhm_mask[j]) fprintf(mf, "%d\n", j); }   \
            }  \
            SUMA_free(mfname); mfname=NULL;\
             fclose(mf); mf=NULL; \
         }  \
      }\
   } \
}

static double FWHM_MinArea = -1.0; 
double SUMA_GetFWHM_MinArea(void) 
{ 
   return(FWHM_MinArea); 
}
void SUMA_SetFWHM_MinArea(double MinArea) 
{ 
   FWHM_MinArea = MinArea; 
   return; 
}

#define FAST_APPROX 1
SUMA_DSET *SUMA_CalculateLocalStats(SUMA_SurfaceObject *SO, SUMA_DSET *din, 
                                    byte *nmask, byte strict_mask,
                                    float rhood, SUMA_OFFSET_STRUCT *UseThisOffset,
                                    int ncode, int *code, 
                                    SUMA_DSET *UseThisDout, int ndbg,
                                    SUMA_SurfaceObject *SOf)
{
   static char FuncName[]={"SUMA_CalculateLocalStats"};
   SUMA_DSET *dout = NULL;
   int *icols = NULL, N_icols = -1, *ind = NULL, n_incopy=-1;
   int ic = -1, k = -1, n = -1, nj=-1, jj=-1, N_nmask=-1, nval=-1, j=-1;
   void *ncoli=NULL;
   int masked_only = 1;
   char *lblcp=NULL;
   float *fin_orig=NULL, *fout = NULL, fp = -1.0;
   byte *fwhm_mask=NULL;
   SUMA_OFFSET_STRUCT *OffS_out=NULL;
   byte *bfull=NULL;
   float *NodeAreaVec=NULL, ZoneArea, MinZoneArea;
   int ipl;
   float Eq[4];
   float *SegDist = NULL;
   int *mask_record = NULL;
   DList *striplist_vec[3];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   if (ncode <=0 || !din || rhood <= 0.0f || !code || !SO) {
      SUMA_S_Errv("Bad input: SO=%p, din=%p, nmask=%p, rhood=%f, ncode=%d,code=%p, UseThisDout=%p\n", 
            SO, din, nmask, rhood, ncode, code, UseThisDout);
      SUMA_RETURN(NULL);
   }
  
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(din, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NOPE);   
   }
   
   
   if (UseThisDout) dout = UseThisDout;
   else {
      if (!(ind = SDSET_NODE_INDEX_COL(din))) {
         SUMA_S_Note("Trying to populate the node index element");
         if (!SUMA_PopulateDsetNodeIndexNel(din, 0)) {
            SUMA_S_Err("Failed to populate NodeIndex Nel");
            SUMA_RETURN(NULL);
         }
      }
      /* Create a dset, at least as big as din*/
      if ((ind = SDSET_NODE_INDEX_COL(din))) {
         if (!masked_only) {
            /* preserve all rows */
            ncoli = SUMA_Copy_Part_Column(ind, NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)), SDSET_VECLEN(din), NULL, masked_only, &n_incopy);
         } else {
            ncoli = SUMA_Copy_Part_Column(ind, NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)), SDSET_VECLEN(din), nmask, masked_only, &n_incopy);  
         }
         if (!ncoli) {
            SUMA_SL_Err("No index data got copied.");
            SUMA_RETURN(NULL);
         }
         dout = SUMA_CreateDsetPointer("LocalStat", SUMA_NODE_BUCKET, NULL,  NI_get_attribute(din->ngr,"domain_parent_idcode"), n_incopy);
         if (!SUMA_AddDsetNelCol (dout, NI_get_attribute(din->inel,"COLMS_LABS"), SUMA_NODE_INDEX, ncoli, NULL ,1)) {
            SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
            SUMA_FreeDset((void*)dout); dout = NULL;
            SUMA_RETURN(NULL);
         }
         if (lblcp) SUMA_free(lblcp); lblcp = NULL;
         if (ncoli) SUMA_free(ncoli); ncoli = NULL; 
      } else {
         SUMA_S_Err("Do not have node indices in input dset! and could not create one.");
         SUMA_RETURN(NULL);
      }
   }
   
   /* some checks? Some day? */
   if (dout == UseThisDout) {
      if (SDSET_VECLEN(dout) != SDSET_VECLEN(din)) {
         SUMA_S_Errv("Mismatch in recycled dset (%d rows) and input dset (%d rows)\n", SDSET_VECLEN(dout),  SDSET_VECLEN(din));
         SUMA_FreeDset((void*)dout); dout = NULL;
         SUMA_RETURN(NULL);
      }
      if (SDSET_VECNUM(dout) != SDSET_VECNUM(din)) {
         SUMA_S_Errv("Mismatch in recycled dset (%d cols) and input dset (%d cols)\n", SDSET_VECNUM(dout),  SDSET_VECNUM(din));
         SUMA_FreeDset((void*)dout); dout = NULL;
         SUMA_RETURN(NULL);
      }
   }
   
   /* calculate the areas per node, if need be (a little overhead)*/
   MinZoneArea = SUMA_GetFWHM_MinArea() ;
   if (MinZoneArea > 0.0) {
      NodeAreaVec = SUMA_CalculateNodeAreas(SO, NULL);
   }
   
   /* Now, for each code, do the dance */
   for (ic=0; ic <ncode; ++ic) {
      for (k=0; k < N_icols; ++k) {
         /* get a float copy of the data column */
         fin_orig = SUMA_DsetCol2Float (din, icols[k], 1);
         if (!fin_orig) {
            SUMA_SL_Crit("Failed to get copy of column. Woe to thee!");
            SUMA_RETURN(NOPE);
         }
         fout = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
         if (!fout) {
            SUMA_SL_Crit("Failed to allocate fout!");
            SUMA_RETURN(NOPE);
         }
         /* make sure column is not sparse, one value per node */
         if (k==0) {
            SUMA_LH( "Special case k = 0, going to SUMA_MakeSparseColumnFullSorted");
            bfull = NULL;
            if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(din), 0.0, &bfull, din, SO->N_Node)) {
               SUMA_S_Err("Failed to get full column vector");
               SUMA_RETURN(NOPE);
            }
            if (bfull) {
               SUMA_LH( "Something was filled in SUMA_MakeSparseColumnFullSorted\n" );
               /* something was filled in good old SUMA_MakeSparseColumnFullSorted */
               if (nmask) {   /* combine bfull with nmask */
                  SUMA_LH( "Merging masks\n" );
                  for (jj=0; jj < SO->N_Node; ++jj) { if (nmask[jj] && !bfull[jj]) nmask[jj] = 0; }   
               } else { nmask = bfull; }
            } 
            if (nmask) {
               N_nmask = 0;
               for (n=0; n<SO->N_Node; ++n) { if (nmask[n]) ++ N_nmask; }
               SUMA_LHv("FWHMing with node mask (%d nodes in mask)\n", N_nmask);
               if (!N_nmask) {
                  SUMA_S_Warn("Empty mask, nothing to do");
               }
            }
            /* now calculate the neighbor offset structure */
            if (!UseThisOffset && code[ic] != NSTAT_FWHMx) {   /* no need for Offset with FWHMx, zones must be big, too big for this method */
               SUMA_LH("Calculating OffS_out ...");
               OffS_out = SUMA_FormNeighbOffset (SO, rhood, NULL, nmask, -1.0);
            } else {
               OffS_out = UseThisOffset;
            }
         } else {
            SUMA_LH( "going to SUMA_MakeSparseColumnFullSorted");
            if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(din), 0.0, NULL, din, SO->N_Node)) {
               SUMA_S_Err("Failed to get full column vector");
               SUMA_RETURN(NOPE);
            }
            /* no need for reworking nmask and bfull for each column...*/
         }
         /* Now I have the data column, nice and solid , do the stats */
         switch (code[ic]) {
            case NSTAT_MEAN:
               lblcp = SUMA_DsetColLabelCopy(din, icols[k], 1); lblcp = SUMA_append_replace_string("mean_", lblcp, "", 2);
               if (!SUMA_AddDsetNelCol (dout, lblcp, SUMA_NODE_FLOAT, (void *)fout, NULL ,1)) {
                  SUMA_S_Crit("Failed to add dset column");
                  SUMA_RETURN(NULL);
               }
               if (!nmask) {
                  SUMA_LH("No mask");
                  for (n=0; n < SO->N_Node; ++n) {
                     fp = fin_orig[n];
                     nval = 1;
                     for (j=0; j<OffS_out[n].N_Neighb; ++j) {
                        nj = OffS_out[n].Neighb_ind[j];
                        if (OffS_out[n].Neighb_dist[j] <= rhood) { fp += fin_orig[nj]; ++nval; }
                     }/* for j*/
                        fout[n] = fp/(float)(nval);
                        SUMA_WORLD_STATS_NODE_DBG;
                  } /* for n */
               } else {
                  SUMA_LH("Have mask");
                  for (n=0; n < SO->N_Node; ++n) {
                     if (nmask[n]) {
                        fp = fin_orig[n];
                        nval = 1;
                        for (j=0; j<OffS_out[n].N_Neighb; ++j) {
                           nj = OffS_out[n].Neighb_ind[j];
                           if (nmask[nj] || !strict_mask) {
                              if (OffS_out[n].Neighb_dist[j] <= rhood) { fp += fin_orig[nj]; ++nval; }
                           } 
                        }/* for j*/
                        fout[n] = fp/(float)nval;
                        SUMA_WORLD_STATS_NODE_DBG;
                     } else {
                        fout[n] = fin_orig[n];
                     }
                  } /* for n */
               } 
               SUMA_free(lblcp); lblcp = NULL;
               break;
            
            case -666: /* used to be NSTAT_FWHMx: */
               lblcp = SUMA_DsetColLabelCopy(din, icols[k], 1); lblcp = SUMA_append_replace_string("fwhm_", lblcp, "", 2);
               if (!SUMA_AddDsetNelCol (dout, lblcp, SUMA_NODE_FLOAT, (void *)fout, NULL ,1)) {
                  SUMA_S_Crit("Failed to add dset column");
                  SUMA_RETURN(NULL);
               }
               /* form a mask for fwhm function */
               if (!(fwhm_mask = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte)))) {
                  SUMA_S_Crit("Failed to allocate fwhm_mask");
                  SUMA_RETURN(NULL);
               }
               /* create the slice strips, don't want to do this repeatedly */
               if (SUMA_Get_UseSliceFWHM()) {
                  for (ipl=0; ipl<3;++ipl) {
                     /* get the intersection strips, start alond the various directions */
                     Eq[0] = Eq[1]=Eq[2]=Eq[3] = 0.0;
                     Eq[ipl] = 1.0; Eq[3] = -SO->Center[ipl];  /* 0==Saggittal, 1==Coronal, 2==Axial */
                     SUMA_LHv("Kill me!\nEq:[%f %f %f %f], step: %f\n", Eq[0], Eq[1], Eq[2], Eq[3], SO->EL->AvgLe);
                     if (!(striplist_vec[ipl] = SUMA_SliceAlongPlane(SO, Eq, SO->EL->AvgLe))) {
                        SUMA_S_Err("Failed to slice along plane");
                        SUMA_RETURN(NULL);
                     }
                  }               
               }      
               if (!nmask) {
                  SUMA_LH("No mask");
                  for (n=0; n < SO->N_Node; ++n) {
                     /* build thy fwhm mask (must have a clean mask here ) */
                     fwhm_mask[n] = 1; nval = 1; 
                     if (NodeAreaVec) ZoneArea = NodeAreaVec[n]; else ZoneArea = -1.0;
                     for (j=0; j<OffS_out[n].N_Neighb; ++j) {
                        nj = OffS_out[n].Neighb_ind[j];
                        if (OffS_out[n].Neighb_dist[j] <= rhood) { 
                           if (NodeAreaVec) ZoneArea += NodeAreaVec[nj];
                           fwhm_mask[nj] = 1; ++nval; 
                        }
                     }/* for j*/
                        if (ZoneArea > 0.0  && ZoneArea < MinZoneArea) {
                           if (n==ndbg) {
                              SUMA_S_Notev("At node %d,\ndata for FWHM estimate spans %g mm2, need %g mm2 for minimum. Bad for node.\n",
                                     n,  ZoneArea, MinZoneArea);
                           }
                           fout[n] = -1.0;
                        } else {
                           if (n==ndbg) {
                              SUMA_S_Notev("At node %d,\ndata for FWHM estimate spans %g mm2, need %g mm2 for minimum. Looks good.\n", 
                                    n, ZoneArea, MinZoneArea);
                           }
                           if (n==ndbg) SUMA_SetDbgFWHM(1);
                           if (SUMA_Get_UseSliceFWHM()) {
                              fout[n] = SUMA_estimate_slice_FWHM_1dif( SO, fin_orig, fwhm_mask, 1, NULL, striplist_vec);
                           } else {   
                              fout[n] = SUMA_estimate_FWHM_1dif( SO, fin_orig, fwhm_mask, 1);
                           }
                           if (n==ndbg) SUMA_SetDbgFWHM(0);
                        }  
                     SUMA_WORLD_STATS_NODE_DBG;
                     /* reset mask */
                     fwhm_mask[n] = 0; 
                     for (j=0; j<OffS_out[n].N_Neighb; ++j) {
                        nj = OffS_out[n].Neighb_ind[j]; fwhm_mask[nj] = 0; 
                     }
                  } /* for n */
               } else {
                  SUMA_LH("Have mask");
                  if (!strict_mask) {
                     SUMA_S_Warn("For fwhm, masking must be STRICT!\nProceeding with foolishness.");
                  }
                  for (n=0; n < SO->N_Node; ++n) {
                     if (nmask[n]) {
                        /* build thy fwhm mask (must have a clean mask here ) */
                        fwhm_mask[n] = 1; nval = 1;
                        if (NodeAreaVec) ZoneArea = NodeAreaVec[n]; else ZoneArea = -1.0;
                        for (j=0; j<OffS_out[n].N_Neighb; ++j) {
                           nj = OffS_out[n].Neighb_ind[j];
                           if (nmask[nj] || !strict_mask) {
                              if (OffS_out[n].Neighb_dist[j] <= rhood) { 
                                 if (NodeAreaVec) ZoneArea += NodeAreaVec[nj];
                                 fwhm_mask[nj] = 1; ++nval;
                              }
                           } 
                        }/* for j*/
                            if (ZoneArea > 0.0 && ZoneArea < MinZoneArea) {
                              if (n==ndbg) {
                                 SUMA_S_Notev("At node %d,\ndata for FWHM estimate spans %g mm2, need %g mm2 for minimum accepted\n", 
                                                n,  ZoneArea, MinZoneArea);
                              }
                              fout[n] = -1.0;
                           } else {
                              if (n==ndbg) {
                                 SUMA_S_Notev("At node %d,\ndata for FWHM estimate spans %g mm2, need %g mm2 for minimum. Looks good.\n", 
                                                n, ZoneArea, MinZoneArea);
                              }
                              if (n==ndbg) SUMA_SetDbgFWHM(1);
                              if (SUMA_Get_UseSliceFWHM()) {
                                 fout[n] = SUMA_estimate_slice_FWHM_1dif( SO, fin_orig, fwhm_mask, 1, NULL, striplist_vec);
                              } else {
                                 fout[n] = SUMA_estimate_FWHM_1dif( SO, fin_orig, fwhm_mask, 1);
                              }
                              if (n==ndbg) SUMA_SetDbgFWHM(0);
                           }  
                        SUMA_WORLD_STATS_NODE_DBG;
                        /* reset mask */
                        fwhm_mask[n] = 0; 
                        for (j=0; j<OffS_out[n].N_Neighb; ++j) {
                           nj = OffS_out[n].Neighb_ind[j]; fwhm_mask[nj] = 0; 
                        }
                     } else {
                        fout[n] = 0.0; nval = 0;/* Non, rien de rien */
                     }
                  } /* for n */
               }
               if (SUMA_Get_UseSliceFWHM()) {
                  for (ipl=0; ipl<3; ++ipl) { SUMA_FREE_DLIST(striplist_vec[ipl]); striplist_vec[ipl] = NULL; }
               }
               SUMA_free(fwhm_mask); fwhm_mask = NULL;
               SUMA_free(lblcp); lblcp = NULL;
               break;
               
            case NSTAT_FWHMx:
               lblcp = SUMA_DsetColLabelCopy(din, icols[k], 1); lblcp = SUMA_append_replace_string("fwhm_", lblcp, "", 2);
               if (!SUMA_AddDsetNelCol (dout, lblcp, SUMA_NODE_FLOAT, (void *)fout, NULL ,1)) {
                  SUMA_S_Crit("Failed to add dset column");
                  SUMA_RETURN(NULL);
               }
               /* form a mask for fwhm function */
               if (!(fwhm_mask = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte)))) {
                  SUMA_S_Crit("Failed to allocate fwhm_mask");
                  SUMA_RETURN(NULL);
               }
               
               #if FAST_APPROX
               if (!SOf && SO->isSphere) SOf = SO;
               SegDist = SUMA_SegmentDistortion(SO, SOf); /* this function should return a vector of 1s if SO == SOf */
               mask_record = (int *)SUMA_calloc(SO->N_Node,sizeof(int));
               if (!SegDist || !mask_record) { SUMA_S_Crit("Failed to allocate"); SUMA_RETURN(NULL);}
               nval = -1;     /* Must initialize nval this way for SUMA_APPROX_NEIGHBORS */
               #endif
               if (!nmask) {
                  SUMA_LH("No mask");
                  for (n=0; n < SO->N_Node; ++n) {
                     #if FAST_APPROX
                     SUMA_APPROX_NEIGHBORS(SO, SOf, n, rhood, SegDist, mask_record, fwhm_mask, nval);
                     #else
                     /* build thy fwhm mask (must have a clean mask here ) */
                     nval = SUMA_ApproxNeighbors(SO, SOf, n, rhood, fwhm_mask);
                     #endif
                     
                     if (n==ndbg) SUMA_SetDbgFWHM(1);
                     if (nval > 6) fout[n] = SUMA_estimate_FWHM_1dif( SO, fin_orig, fwhm_mask, 1);
                     else fout[n] = -1.0;
                     if (n==ndbg) SUMA_SetDbgFWHM(0);
                     SUMA_WORLD_STATS_NODE_DBG;
                     
                  } /* for n */
               } else {
                  SUMA_LH("Have mask");
                  if (!strict_mask) {
                     SUMA_S_Warn("For fwhm, masking must be STRICT!\nProceeding with foolishness.");
                  }
                  for (n=0; n < SO->N_Node; ++n) {
                     if (nmask[n]) {
                        #if FAST_APPROX
                        SUMA_APPROX_NEIGHBORS(SO, SOf, n, rhood, SegDist, mask_record, fwhm_mask, nval);
                        #else
                        /* build thy fwhm mask (must have a clean mask here ) */
                        nval = SUMA_ApproxNeighbors(SO, SOf, n, rhood, fwhm_mask);
                        #endif
                        if (nval > 6) {
                           /* Now qualify the fwhm_mask to include only nodes in nmask, if strict_mask */
                           if (strict_mask) {
                              for (j=0; j<SO->N_Node; ++j) {
                                 fwhm_mask[j] *= nmask[j];
                              }
                           }
                           if (n==ndbg) SUMA_SetDbgFWHM(1);
                           fout[n] = SUMA_estimate_FWHM_1dif( SO, fin_orig, fwhm_mask, 1);
                           if (n==ndbg) SUMA_SetDbgFWHM(0);
                        } else {
                           fout[n] = -1.0;
                        }
                        SUMA_WORLD_STATS_NODE_DBG;
                     } else {
                        fout[n] = 0.0; nval = 0;/* Non, rien de rien */
                     }
                  } /* for n */
               }
               #if FAST_APPROX
               if (SegDist) SUMA_free(SegDist); SegDist = NULL;
               if (mask_record) SUMA_free(mask_record); mask_record = NULL;
               #else
               SUMA_ApproxNeighbors(NULL, NULL, -1, 0, NULL);  /* cleanup ApproxNeighbors */
               #endif
               SUMA_free(fwhm_mask); fwhm_mask = NULL;
               SUMA_free(lblcp); lblcp = NULL;
               break;
            default:
               SUMA_S_Errv("Should not be here, not ready to deal with this stat (%d)\n", code[ic]);
               SUMA_RETURN(NULL);
               break;
         }
         
         /* add this column to the output dset */
         if (!SUMA_Float2DsetCol (dout, icols[k], fout, 1, nmask)) {
            SUMA_S_Err("Failed to update dset's values");
            SUMA_RETURN(NOPE);      
         }
         
         if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL;
         if (fout) SUMA_free(fout); fout = NULL;
      } /* for k */
      /* Pre Dec 06 stupidity: if (bfull == nmask) { if (nmask) SUMA_free(nmask); nmask = NULL; bfull = NULL; } */
      if (bfull) SUMA_free(bfull); bfull = NULL;
         
   }/* for ic */
   if (NodeAreaVec) SUMA_free(NodeAreaVec); NodeAreaVec = NULL; 
   if (!UseThisOffset) OffS_out = SUMA_free_NeighbOffset (SO, OffS_out);
   
   SUMA_RETURN(dout);
} 
