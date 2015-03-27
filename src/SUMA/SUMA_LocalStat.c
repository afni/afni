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
      if (!SUMA_SurfaceMetrics_eng(SO, "PolyArea|MemberFace", NULL, 0, 
                                   SUMAg_CF->DsetList)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", 
                               FuncName);
         SUMA_RETURN(NodeAreas);
      }
   }
   
   NodeAreas = (float *)SUMA_malloc(SO->N_Node*sizeof(float));
   if (!NodeAreas) { 
      SUMA_SL_Crit ("Failed to allocate for NodeAreas"); 
      SUMA_RETURN(NodeAreas); 
   }
   
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
   \param AddToThisClust (SUMA_CLUST_DATUM *)pointer to cluster to add to. 
                    Send NULL when function is first called. 
                    This is a non NULL when the function recurses.
   \param ToBeAssigned (float *) if ToBeAssigned[i] then node i 
                   (index into SO's nodelist) is considered in the clustering 
                   and the value at that node is ToBeAssigned[i]. 
                   Vector is SO->N_Node elements long. 
                   Gets modified during recursion.
   \param N_ToBeAssigned (int *) pointer to number of value values 
                                 in ToBeAssigned. 
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
   int il, jl, neighb, nnn, kkk;
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
      Clust->minabsvalue = SUMA_ABS(Clust->minvalue); 
         Clust->minabsnode = Clust->minnode;
      Clust->maxabsvalue = SUMA_ABS(Clust->maxvalue); 
         Clust->maxabsnode = Clust->maxnode;
      Clust->varvalue = 0.0;  Clust->centralnode = 0; 
      Clust->weightedcentralnode = 0; 
      Clust->NodeList = (int *)SUMA_malloc((*N_TobeAssigned) * sizeof(int)); 
      Clust->ValueList = (float *)SUMA_malloc((*N_TobeAssigned) * sizeof(float));
      memset(Clust->com, 0, 3*sizeof(float));
      memset(Clust->cen, 0, 3*sizeof(float));
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
   if (SUMA_ABS(ToBeAssigned[dothisnode]) < Clust->minabsvalue) { 
      Clust->minabsvalue = SUMA_ABS(ToBeAssigned[dothisnode]); 
      Clust->minabsnode = dothisnode; 
   }
   if (SUMA_ABS(ToBeAssigned[dothisnode]) > Clust->maxabsvalue) { 
      Clust->maxabsvalue = SUMA_ABS(ToBeAssigned[dothisnode]); 
      Clust->maxabsnode = dothisnode; 
   }
   Clust->ValueList[Clust->N_Node] = ToBeAssigned[dothisnode];
   nnn = SO->NodeDim*dothisnode;
   for (kkk = 0; kkk<SO->NodeDim; ++kkk) {
      Clust->com[kkk] += ToBeAssigned[dothisnode] * SO->NodeList[nnn+kkk];
      Clust->cen[kkk] += SO->NodeList[nnn+kkk];
   }
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
#define SUMA_ADD_NODE_TO_CLUST(dothisnode, Clust, NodeArea, ToBeAssigned, SO){ \
   int nnn, kkk; \
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Adding node %d to cluster %p of %d nodes\nClustMax is %f, incoming value is %f\n", FuncName, dothisnode, Clust, Clust->N_Node, Clust->maxvalue, ToBeAssigned[dothisnode]);   \
   Clust->NodeList[Clust->N_Node] = dothisnode; \
   Clust->totalarea += NodeArea[dothisnode]; \
   Clust->totalvalue += ToBeAssigned[dothisnode];  \
   Clust->totalabsvalue += (float)fabs((float)ToBeAssigned[dothisnode]);   \
   if (ToBeAssigned[dothisnode] < Clust->minvalue) { Clust->minvalue = ToBeAssigned[dothisnode]; Clust->minnode = dothisnode; }  \
   if (ToBeAssigned[dothisnode] > Clust->maxvalue) { Clust->maxvalue = ToBeAssigned[dothisnode]; Clust->maxnode = dothisnode; }  \
   if (SUMA_ABS(ToBeAssigned[dothisnode]) < Clust->minabsvalue) { Clust->minabsvalue = SUMA_ABS(ToBeAssigned[dothisnode]); Clust->minabsnode = dothisnode; }\
   if (SUMA_ABS(ToBeAssigned[dothisnode]) > Clust->maxabsvalue) { Clust->maxabsvalue = SUMA_ABS(ToBeAssigned[dothisnode]); Clust->maxabsnode = dothisnode; }\
   Clust->ValueList[Clust->N_Node] = ToBeAssigned[dothisnode]; \
   nnn = SO->NodeDim*dothisnode; \
   for (kkk = 0; kkk<SO->NodeDim; ++kkk) {\
      Clust->com[kkk] += ToBeAssigned[dothisnode] * SO->NodeList[nnn+kkk]; \
      Clust->cen[kkk] += SO->NodeList[nnn+kkk]; \
   }  \
   ++Clust->N_Node;  \
}

/*!
   \brief builds a cluster starting from some node 
   
   \param dothisnode (int) start building from this node
   \param ToBeAssigned (float *) if ToBeAssigned[i] then node i (index into 
                                 SO's nodelist) is considered in the clustering 
                                 and the value at that node is ToBeAssigned[i].
                                 Vector is SO->N_Node elements long. Gets 
                                 modified in function.
   \param N_ToBeAssigned (int *) pointer to number of value values in 
                                 ToBeAssigned. Gets modified in function. 
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
   int NewClust = 0, NeedNewLine = 0;
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
      Clust->minabsvalue = SUMA_ABS(Clust->minvalue); 
         Clust->minabsnode = Clust->minnode;
      Clust->maxabsvalue = SUMA_ABS(Clust->maxvalue); 
         Clust->maxabsnode = Clust->maxnode;
      Clust->varvalue = 0.0;  Clust->centralnode = 0; 
      Clust->weightedcentralnode = 0;
      Clust->NodeList = (int *)SUMA_malloc((*N_TobeAssigned) * sizeof(int)); 
      Clust->ValueList = (float *)SUMA_malloc((*N_TobeAssigned) * sizeof(float));
      memset(Clust->com, 0, 3*sizeof(float));
      memset(Clust->cen, 0, 3*sizeof(float));
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
   SUMA_ADD_NODE_TO_CLUST(dothisnode, Clust, NodeArea, ToBeAssigned, SO);
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
                                             NodeArea, ToBeAssigned, SO);
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
                           NeedNewLine = 1;

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
                                             NodeArea, ToBeAssigned, SO);
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
                           NeedNewLine = 1;
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
   if (NeedNewLine) fprintf(SUMA_STDERR,"\n");
   /* destroy the list */
   if (candlist) { 
      dlist_destroy(candlist); SUMA_free(candlist); candlist  = NULL; 
   }
   SUMA_RETURN(Clust);
}

char *SUMA_ClustCommandLineFromOpt(char *pname, SUMA_SurfaceObject *SO, 
                            SUMA_SURFCLUST_OPTIONS *Opt, 
                            char *filler) 
{
   static char FuncName[]={"SUMA_ClustCommandLineFromOpt"};
   SUMA_STRING *SS=NULL;
   char *s = NULL;
   
   SUMA_ENTRY;
   
   if (!Opt) SUMA_RETURN(s);
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (!pname) pname = "SurfClust";
   
   if (SO) {
      if (!(s = SUMA_SurfaceFileName(SO,1))) {
         s = SUMA_copy_string("Qui_Etes_Vous.gii");
      }
   } else {
      s = SUMA_copy_string("SOunknown");
   }
   SS = SUMA_StringAppend_va(SS,"%s -i %s -input %s %d "
                                "-rmm %f "
            , pname, s, Opt->in_name, 
              Opt->labelcol, Opt->DistLim );
   if (s) SUMA_free(s); s= NULL;
   if (Opt->tind>=0) {
      if (Opt->DoThreshold == SUMA_NO_THRESH) {
      } else if (Opt->DoThreshold == SUMA_LESS_THAN) {
         SS = SUMA_StringAppend_va(SS,"-thresh_col %d -thresh %f ",
                              Opt->tind, Opt->ThreshR[0]);
      } else if (Opt->DoThreshold == SUMA_ABS_LESS_THAN) {
         SS = SUMA_StringAppend_va(SS,"-thresh_col %d -athresh %f ",
                              Opt->tind, Opt->ThreshR[0]);
      } else if (Opt->DoThreshold == SUMA_THRESH_OUTSIDE_RANGE) {
         SS = SUMA_StringAppend_va(SS,"-thresh_col %d -ex_range %f %f",
                              Opt->tind, Opt->ThreshR[0], Opt->ThreshR[1]);
      } else if (Opt->DoThreshold == SUMA_THRESH_INSIDE_RANGE) {
         SS = SUMA_StringAppend_va(SS,"-thresh_col %d -in_range %f %f",
                              Opt->tind, Opt->ThreshR[0], Opt->ThreshR[1]);
      } else {
         SS = SUMA_StringAppend(SS,"NO_COMPARABLE_THRESHOLD ");
      }
   }
   if (Opt->AreaLim < 0) {
      SS = SUMA_StringAppend_va(SS,"-n %d ", (int)-Opt->AreaLim);
   } else if (Opt->AreaLim > 0){
      SS = SUMA_StringAppend_va(SS,"-amm2 %f ", Opt->AreaLim);
   }
   switch (Opt->SortMode) {
      case SUMA_SORT_CLUST_NO_SORT:
         SS = SUMA_StringAppend(SS,"-sort_none ");
         break;
      case SUMA_SORT_CLUST_BY_NUMBER_NODES:
         SS = SUMA_StringAppend(SS,"-sort_n_nodes ");
         break;
      case SUMA_SORT_CLUST_BY_AREA:
         SS = SUMA_StringAppend(SS,"-sort_area ");
         break;
      default:
         SS = SUMA_StringAppend(SS,"-sort_none ");
         break;
   }
   if (Opt->DoCentrality) {
      SS = SUMA_StringAppend(SS,"-cent ");
   } else {
      SS = SUMA_StringAppend(SS,"-no_cent ");
   }
   if (filler) SS = SUMA_StringAppend(SS,filler);
   
   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
}

float *SUMA_AvgGradient(SUMA_SurfaceObject *SO, float **FirstNeighbDist,
                             float *nv, byte *mask,
                             byte mask_zeros, SUMA_GRAD_SCALE_OPTS normopt)
{
   static char FuncName[]={"SUMA_AvgGradient"};   
   int i=0, k=0, ki=0, ng=0, nmsk=0;
   float *gr=NULL, avg=0.0;
   double gavg=0.0; 
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->FN || !nv || !FirstNeighbDist) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(gr);
   }
   
   if (mask_zeros) {
      if (!mask) {
         mask = (byte *)SUMA_malloc(SO->N_Node*sizeof(byte));
         memset(mask,1,SO->N_Node*sizeof(byte));
      }
      for (i=0; i<SO->N_Node; ++i) if (!nv[i]) mask[i]=0;
   }
   
   if (!(gr = (float *)SUMA_calloc(SO->N_Node, sizeof(float)))) {
      SUMA_S_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   gavg = 0.0; nmsk = 0;
   for (i=0; i<SO->N_Node; ++i) {
      if (SO->FN->NodeId[i] != i) {
         SUMA_S_Errv("Unexpected node index mismatch (%d,%d)."
                    "Please report case to author.",
                    SO->FN->NodeId[i], i);
         SUMA_free(gr); gr=NULL; SUMA_RETURN(NULL);
      }
      gr[i]=0.0; 
      if (IN_MASK(mask, i)) {
         ng = 0; ++nmsk;
         avg = nv[i]; gavg += nv[i];
         for (k=0; k<SO->FN->N_Neighb[i]; ++k) {
            ki = SO->FN->FirstNeighb[i][k];
            if (IN_MASK(mask,ki)) {
               gr[i] += (nv[i]-nv[ki])/FirstNeighbDist[i][k]; ++ng;
               avg += nv[ki];
            }
         }
         if (ng) {
            gr[i] /= (float)ng; /* average gradient */
            avg = avg / (ng+1.0); /* local intensity average */
            switch(normopt) {
               case SUMA_GMEAN_GRAD_SCALE:
               case SUMA_NO_GRAD_SCALE:
                  break;
               case SUMA_MEAN_GRAD_SCALE:
                  gr[i] = 100.0*gr[i]/avg;
                  break;
               default:
                  ERROR_message("Bad normalization option");
                  break;
            }
         }
      }
   }
   if (normopt == SUMA_GMEAN_GRAD_SCALE && nmsk > 0 && gavg != 0.0) {
      gavg = gavg/(float)nmsk/100.0;
      for (i=0; i<SO->N_Node; ++i) {
         if (IN_MASK(mask,ki)) {
            gr[i] = gr[i] / gavg;
         }
      } 
   }
   
   SUMA_RETURN(gr);
}

SUMA_DSET * SUMA_DsetAvgGradient(
   SUMA_SurfaceObject *SO, float **FirstNeighbDist, SUMA_DSET *din, 
   byte *maskp, byte mask_by_zeros, SUMA_GRAD_SCALE_OPTS normopt)
{
   static char FuncName[]={"SUMA_DsetAvgGradient"};
   SUMA_DSET *dout = NULL;
   int *icols = NULL, N_icols = -1, *ind = NULL, n_incopy=-1, masked_only=0;
   int k = -1, N_mask=-1;
   void *ncoli=NULL;
   char *lblcp=NULL, *s=NULL;
   float *fin_orig=NULL, *fout = NULL, fp = -1.0, **NeighbDist=NULL;
   byte *mask=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

      
   if (!SO || !din) {
      SUMA_S_Err("Bad input");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      if (!SUMA_SurfaceMetrics_eng(SO, "EdgeList|PolyArea|MemberFace",NULL, 
                                   0, SUMAg_CF->DsetList)) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
      }
   }
   
   if (maskp) mask=maskp;
   
   if (!FirstNeighbDist) {
      NeighbDist = SUMA_CalcNeighbDist (SO);
   } else {
      NeighbDist = FirstNeighbDist;
   }
   if (!NeighbDist) {
      SUMA_S_Err("NULL dists");
      SUMA_RETURN(NULL);
   }
   
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(din, &N_icols);
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NOPE);   
   }
   SUMA_LHv("Have %d numeric columns of input.\n", N_icols);
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
         ncoli = 
            SUMA_Copy_Part_Column(ind, 
               NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)), 
               SDSET_VECLEN(din), NULL, masked_only, &n_incopy);
      } else {
         ncoli = 
            SUMA_Copy_Part_Column(ind, 
               NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)), 
               SDSET_VECLEN(din), mask, masked_only, &n_incopy);  
      }
      if (!ncoli) {
         SUMA_SL_Err("No index data got copied.");
         SUMA_RETURN(NULL);
      }
      dout = SUMA_CreateDsetPointer("AvgGradient", SUMA_NODE_BUCKET, NULL,  
                                    SDSET_IDMDOM(din), n_incopy);
      if (!SUMA_AddDsetNelCol (dout, NI_get_attribute(din->inel,"COLMS_LABS"),                                    SUMA_NODE_INDEX, ncoli, NULL ,1)) {
         SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
         SUMA_FreeDset((void*)dout); dout = NULL;
         SUMA_RETURN(NULL);
      }
      if (ncoli) SUMA_free(ncoli); ncoli = NULL; 
   } else {
      SUMA_S_Err( "Do not have node indices in input dset!\n"
                  " and could not create one.");
      SUMA_RETURN(NULL);
   }
   
   /* Now, for each code, do the dance */
   for (k=0; k < N_icols; ++k) {
      /* get a float copy of the data column */
      if (!(fin_orig = SUMA_DsetCol2FloatFullSortedColumn(din, icols[k], &mask, 
                                                0.0, SO->N_Node,
                                                &N_mask, k==0?YUP:NOPE))){
         SUMA_S_Err("Failed to extract");
         SUMA_FreeDset(dout); dout=NULL;
         if (!maskp) SUMA_free(mask); mask=NULL;
         SUMA_RETURN(dout);
      }
      
      if (LocalHead) {
         s = SUMA_ShowMeSome(fin_orig, SUMA_float, SO->N_Node, 10, NULL);
         SUMA_LHv("fin_orig:\n%s\n", s); SUMA_free(s);
      }
      
      
      /* Now I have the data column, nice and solid , do the stats */
      fout = SUMA_AvgGradient(SO, NeighbDist, fin_orig, mask, 
                              mask_by_zeros, normopt);
      if (!fout) {
         SUMA_SL_Crit("Failed to compute gradient fout!");
         SUMA_RETURN(NULL);
      }
      
      lblcp = SUMA_DsetColLabelCopy(din, icols[k], 1); 
      lblcp = SUMA_append_replace_string("D[", lblcp, "", 2);
      lblcp = SUMA_append_replace_string(lblcp, "]", "", 1);
      if (!SUMA_AddDsetNelCol (dout, lblcp, SUMA_NODE_FLOAT, 
                               NULL, NULL ,1)) {
         SUMA_S_Crit("Failed to add dset column");
         SUMA_RETURN(NULL);
      }
      SUMA_free(lblcp); lblcp=NULL;
      SUMA_LHv("Sticking column %d in dset (fout[0]=%f)\n", k, fout[0]);
      if (!SUMA_Vec2DsetCol (dout, k, (void *)fout, SUMA_float, 
                             masked_only, mask)) {
         SUMA_S_Err("Failed to store output");
         SUMA_free(fin_orig); fin_orig = NULL; SUMA_free(fout); fout = NULL; 
         if (maskp) SUMA_free(mask); mask=NULL;
         SUMA_FreeDset(dout); dout=NULL;
         SUMA_RETURN(dout);
     }
         
         if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL;
         if (fout) SUMA_free(fout); fout = NULL;
   } /* for k */
   
   if (!maskp) SUMA_free(mask); mask = NULL;
   if (NeighbDist != FirstNeighbDist) {
      SUMA_free2D((char **)NeighbDist, SO->FN->N_Node);       
   }
   SUMA_RETURN(dout);
} 


/* Find the extreme points of a dataset */
SUMA_DSET *SUMA_DsetExtrema(
   SUMA_SurfaceObject *SO, float **FirstNeighbDist, 
   SUMA_DSET *din, SUMA_DSET *dgrad, float r, float fthresh, float gthresh, 
   byte *maskp, byte mask_by_zeros, SUMA_EXTREMA_DIRECTIONS dir,
   char *tout)
{
   static char FuncName[]={"SUMA_DsetExtrema"};
      SUMA_DSET *dout = NULL;
   int *icols = NULL, N_icols = -1, *ind = NULL, n_incopy=-1, masked_only=0;
   int k = -1, N_mask=-1, *isrt=NULL, dbg_node=-1;
   int *mout=NULL, n_peak=0, ni=0, cand=-1, il=0, jl=0, neighb=0; 
   void *ncoli=NULL;
   char *lblcp=NULL, *s=NULL, Cside=' ', *Sdir=NULL;
   float *fin=NULL,  *gin=NULL, **NeighbDist=NULL, *finsrt=NULL;
   byte *mask=NULL, *ex_mask=NULL;
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   FILE *ftable=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO || !din) {
      SUMA_S_Err("Bad input");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      if (!SUMA_SurfaceMetrics_eng(SO, "EdgeList|PolyArea|MemberFace",NULL,
                                   0, SUMAg_CF->DsetList)) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
      }
   }
   
   if (SO->Side == SUMA_LEFT) Cside = 'L';
   else if (SO->Side == SUMA_RIGHT) Cside = 'R';
   
   switch (dir) {
      case SUMA_MAXIMUS:
         Sdir = "max";
         break;
      case SUMA_MINIMUS:
         Sdir = "min";
         break;
      case SUMA_EXTREMUS:
         Sdir = "ext";
         break;
      default:
         SUMA_S_Errv("Bad news for dir %d\n", dir);
         SUMA_RETURN(NULL);
   }

   if (tout) {
      if (!(ftable = fopen(tout,"w"))) {
         SUMA_S_Errv("Could not open %s for writing\n", tout);
         SUMA_RETURN(NULL);
      }
      fprintf(ftable,
                     "#Output of SurfExtrema. Columns are comma separated.\n"
                     "#Col.1: Node index \n"
                     "#Col.2: Hemisphere side if applicable\n"
                     "#Col.3: Rank of %s\n"
                     "#Col.4: Value at node\n"
                     "#Col.5: Gradient at node\n"
                     "#Col.6: X Y Z coordinates of node. \n"
                  "#        Use proper -sv option to relate to volume coords.\n"
                     "#Col.7: Sub-brick index of input\n"
                     "#Col.8: Sub-brick label of input\n"
                     "#To select a set of columns, you can use cut. "
                     "# For example, say you want cols 1,2, and 4:\n"
                     "#    \\cut -f '1,2,4' -d ',' %s\n"
                     "# If you just want nodeindex with hemisphere\n"
                     "# label stuck to it (from 3dGroupInCorr's batch mode):\n"
                     "#    \\cut -f '1,2' -d ',' %s | \\sed 's/ *, *//g'\n",
                      Sdir, tout, tout);
   }
   if (maskp) mask=maskp;
   
   if (!FirstNeighbDist) {
      NeighbDist = SUMA_CalcNeighbDist (SO);
   } else {
      NeighbDist = FirstNeighbDist;
   }
   if (!NeighbDist) {
      SUMA_S_Err("NULL dists");
      SUMA_RETURN(NULL);
   }
   
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(din, &N_icols);
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NOPE);   
   }
   SUMA_LHv("Have %d numeric columns of input.\n", N_icols);
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
         ncoli = 
            SUMA_Copy_Part_Column(ind, 
               NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)), 
               SDSET_VECLEN(din), NULL, masked_only, &n_incopy);
      } else {
         ncoli = 
            SUMA_Copy_Part_Column(ind, 
               NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)), 
               SDSET_VECLEN(din), mask, masked_only, &n_incopy);  
      }
      if (!ncoli) {
         SUMA_SL_Err("No index data got copied.");
         SUMA_RETURN(NULL);
      }
      dout = SUMA_CreateDsetPointer("Extrema", SUMA_NODE_BUCKET, NULL,  
                                    SDSET_IDMDOM(din), n_incopy);
      if (!SUMA_AddDsetNelCol (dout, NI_get_attribute(din->inel,"COLMS_LABS"),                                    SUMA_NODE_INDEX, ncoli, NULL ,1)) {
         SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
         SUMA_FreeDset((void*)dout); dout = NULL;
         SUMA_RETURN(NULL);
      }
      if (ncoli) SUMA_free(ncoli); ncoli = NULL; 
   } else {
      SUMA_S_Err( "Do not have node indices in input dset!\n"
                  " and could not create one.");
      SUMA_RETURN(NULL);
   }
   
   OffS = SUMA_Initialize_getoffsets (SO->N_Node);
   
   /* Now, for each column, do the dance */
   for (k=0; k < N_icols; ++k) {
      lblcp = SUMA_DsetColLabelCopy(din, icols[k], 1); 

      /* get a float copy of the data column */
      if (!(fin = SUMA_DsetCol2FloatFullSortedColumn(din, icols[k], &mask, 
                                                0.0, SO->N_Node,
                                                &N_mask, k==0?YUP:NOPE))){
         SUMA_S_Err("Failed to extract");
         SUMA_FreeDset(dout); dout=NULL;
         if (!maskp) SUMA_free(mask); mask=NULL;
         SUMA_RETURN(dout);
      }
      
      if (LocalHead) {
         s = SUMA_ShowMeSome(fin, SUMA_float, SO->N_Node, 10, NULL);
         SUMA_LHv("fin:\n%s\n", s); SUMA_free(s);
      }
      if (dgrad) {
         if (!(gin = SUMA_DsetCol2FloatFullSortedColumn(dgrad, icols[k], &mask, 
                                                0.0, SO->N_Node,
                                                &N_mask, k==0?YUP:NOPE))){
            SUMA_S_Err("Failed to extract");
            SUMA_FreeDset(dout); dout=NULL;
            if (!maskp) SUMA_free(mask); mask=NULL;
            SUMA_RETURN(dout);
         }
      } else {
         gin = SUMA_AvgGradient(SO, NeighbDist, fin, mask, 
                                 mask_by_zeros, SUMA_MEAN_GRAD_SCALE);
         if (!gin) {
            SUMA_SL_Crit("Failed to compute gradient gin!");
            SUMA_RETURN(NULL);
         }         
      }
      
      /* Now do some work */
      /* sort the values in fin (lowest first)*/
      finsrt = (float *)SUMA_malloc(SO->N_Node*sizeof(float));
      memcpy(finsrt, fin, SO->N_Node*sizeof(float));
      
      if (dir == SUMA_EXTREMUS) { /* Take absolute value */
         for (ni=0; ni<SO->N_Node; ++ni) finsrt[ni] = SUMA_ABS(finsrt[ni]);
      }
      
      isrt = SUMA_z_qsort(finsrt, SO->N_Node);
      if (LocalHead) {
         s = SUMA_ShowMeSome(isrt, SUMA_int, SO->N_Node, 10, NULL);
         SUMA_LHv("isrt:\n%s\n", s); SUMA_free(s);
      }
      /* find top candidate */
      ex_mask = (byte *)SUMA_malloc(SO->N_Node*sizeof(byte));
      if (!mask) {
         memset(ex_mask,1,sizeof(byte)*SO->N_Node);
      } else {
         memcpy(ex_mask, mask, sizeof(byte)*SO->N_Node);
      }
      mout = (int*)SUMA_calloc(SO->N_Node, sizeof(int));
      n_peak = 0;
      ni = SO->N_Node;
      cand = -1;
      while(ni > 0) { 
         --ni; 
         #if 0
         if (isrt[ni] == dbg_node) {
            fprintf(SUMA_STDERR,"ex_mask[%d]=%d, gin=%f (%f), fin=%f (%f)\n", 
                        isrt[ni], ex_mask[isrt[ni]],
                        gin[isrt[ni]], gthresh, finsrt[ni], fthresh);
         }
         #endif
         if (  ex_mask[isrt[ni]] && (
               (  dir == SUMA_MAXIMUS && /* Maxima */
                     gin[isrt[ni]] >= gthresh &&
                     finsrt[ni] >= fthresh  )  || 
               (  dir == SUMA_MINIMUS && /* minima */
                     gin[isrt[ni]] <= gthresh &&
                     finsrt[ni] <= fthresh  )  ||
               (  dir == SUMA_EXTREMUS && /* absolute */
                     SUMA_ABS(gin[isrt[ni]]) >= gthresh &&
                     SUMA_ABS(fin[isrt[ni]]) >= fthresh  ) 
                                    )
            ) { /* have good one */
            ++n_peak;
            cand = isrt[ni]; 
            mout[cand] = n_peak; /* store peak number */  
            ex_mask[cand] = 0; /* mark it as no longer eligible */
            SUMA_LHv("Node %d %c is peak number %d: Val %f, Grad %f,"
                     " Coords:  %f %f %f "
                     " input %d:%s\n",
                     cand, Cside, n_peak, fin[isrt[ni]], 
                     gin[isrt[ni]],
                     SO->NodeList[SO->NodeDim*cand],
                     SO->NodeList[SO->NodeDim*cand+1],
                     SO->NodeList[SO->NodeDim*cand+2],
                     icols[k], lblcp);
            if (ftable) {
               fprintf(ftable,"%d , %c , %d , %f , %f , %f %f %f , %d , %s\n",
                        cand, Cside, n_peak, fin[isrt[ni]], gin[isrt[ni]],
                        SO->NodeList[SO->NodeDim*cand],
                        SO->NodeList[SO->NodeDim*cand+1],
                        SO->NodeList[SO->NodeDim*cand+2],
                        icols[k], lblcp);
            }
            /* mark all nodes within r as out of contest */
            SUMA_getoffsets2 (cand, SO, r, OffS, NULL, 0);
            if (r > 0.0) {
               for (il=1; il<OffS->N_layers; ++il) { 
                  /*  starting at layer 1, layer 0 is the node itself */
                  for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) {
                     neighb = OffS->layers[il].NodesInLayer[jl];
                     if (OffS->OffVect[neighb] <= r) {
                        #if 0
                        if (neighb == dbg_node) {
                           fprintf(SUMA_STDERR,
                           "node %d is at dist %f (%f) from candidate node %d\n",
                                     neighb, OffS->OffVect[neighb], r, cand);
                        }
                        #endif
                        ex_mask[neighb] = 0; /* take it out */
                     }
                  }
               }
            } else {
               SUMA_S_Err("Not ready to deal with negative r");
               SUMA_RETURN(NULL);
            }
         }
         SUMA_Recycle_getoffsets (OffS); 
      }  
      
      lblcp = SUMA_append_replace_string(Sdir, lblcp, "[", 2);
      lblcp = SUMA_append_replace_string(lblcp, "]", "", 1);
      if (!SUMA_AddDsetNelCol (dout, lblcp, SUMA_NODE_INT, 
                               NULL, NULL ,1)) {
         SUMA_S_Crit("Failed to add dset column");
         SUMA_RETURN(NULL);
      }
      SUMA_free(lblcp); lblcp=NULL;
      SUMA_LHv("Sticking column %d in dset (mout[0]=%d)\n", k, mout[0]);
      if (!SUMA_Vec2DsetCol (dout, k, (void *)mout, SUMA_int, 
                             masked_only, mask)) {
         SUMA_S_Err("Failed to store output");
         SUMA_free(fin); fin = NULL;
         SUMA_free(finsrt); finsrt = NULL; 
         SUMA_free(mout); mout = NULL; 
         if (maskp) SUMA_free(mask); mask=NULL;
         SUMA_FreeDset(dout); dout=NULL;
         SUMA_RETURN(dout);
      }
         
      if (mout) SUMA_free(mout); mout=NULL;
      if (ex_mask) SUMA_free(ex_mask); ex_mask=NULL;
      if (fin) SUMA_free(fin); fin = NULL;
      if (finsrt) SUMA_free(finsrt); finsrt = NULL; 
      if (isrt) SUMA_free(isrt); isrt = NULL;
   } /* for k */
   
   if (ftable) fclose(ftable); ftable = NULL;
   if (!maskp) SUMA_free(mask); mask = NULL;
   if (NeighbDist != FirstNeighbDist) {
      SUMA_free2D((char **)NeighbDist, SO->FN->N_Node);       
   }
   if (OffS) SUMA_Free_getoffsets(OffS); OffS = NULL;

   SUMA_RETURN(dout);
   
   
   
}


/*!
   \brief Finds locally extreme nodes on the surface 
   
   \param dothisnode (int) start building from this node
   \param ToBeAssigned (float *) 
         if ToBeAssigned[i] then node i (index into SO's nodelist)
         is considered in the clustering and the value at that
         node is ToBeAssigned[i]. Vector is SO->N_Node elements
         long. Gets modified in function.
   \param N_ToBeAssigned (int *) pointer to number of value values 
         in ToBeAssigned. Gets modified in function. 
   \param NodeArea(float *) Vector containing area of each node of surface
   \param SO (SUMA_SurfaceObject *) the usual deal
   \param Opt (SUMA_SURFCLUST_OPTIONS *) options for cluster building.     
      
*/
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node_Extrema (  
   int dothisnode, 
   float *ToBeAssigned, int *N_TobeAssigned, float *NodeArea,
   SUMA_SurfaceObject *SO, SUMA_SURFCLUST_OPTIONS *Opt   )
{
   static char FuncName[]={"SUMA_Build_Cluster_From_Node_Extrema"};
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
   memset(Clust->com, 0, 3*sizeof(float));
   memset(Clust->cen, 0, 3*sizeof(float));
   candlist = (DList*)SUMA_malloc(sizeof(DList));
   visited = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte));
   if (!visited || !candlist) {
      SUMA_SL_Crit("Failed to allocate for visited or candlist");  
      SUMA_free(Clust); Clust = NULL;
      SUMA_RETURN(NULL);  
   }
   dlist_init(candlist, NULL);
   /* Add node to cluster */
   SUMA_ADD_NODE_TO_CLUST(dothisnode, Clust, NodeArea, ToBeAssigned, SO);
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
                                             NodeArea, ToBeAssigned, SO);
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
                                             NodeArea, ToBeAssigned, SO);
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
   if (candlist) { 
      dlist_destroy(candlist); 
      SUMA_free(candlist); candlist  = NULL; 
   }
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
   \param dothisnode (int) index of node (into SO's nodelist, not ni) 
                           to start/proceed from
   \param Opt (SUMA_SURFCLUST_OPTIONS *) structure containing clustering options
   \param AddToThisClust (SUMA_CLUST_DATUM *) add to this cluster
   \param NodeArea (float *) SO->N_Node vector of node areas. 
   \return ClustList (DList *) list of clusters, in no particular order. 
                           Processing of the list is to be done later.
   
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
   
   if (!NodeArea) {
      if (!SO->NodeAreas) SO->NodeAreas = SUMA_CalculateNodeAreas(SO, NULL);
      NodeArea = SO->NodeAreas;
   }
   
   if (!NodeArea) {
      SUMA_S_Err("No node areas and failed to get them from SO\n");
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
         SUMA_LH("Building clusts, no rec");
         Clust = SUMA_Build_Cluster_From_Node_NoRec(  dothisnode, 
                                                      ToBeAssigned, 
                                                      &N_n, NodeArea, 
                                                      SO, Opt);
      } else if ( BuildMethod == SUMA_OFFSETS2 || 
                  BuildMethod == SUMA_OFFSETS_LL ) {
         Clust = SUMA_Build_Cluster_From_Node(  dothisnode, NULL, 
                                                ToBeAssigned, 
                                                &N_n, NodeArea, 
                                                SO, Opt);
      } else {
         SUMA_S_Errv("No Such Method (%d)!\n", BuildMethod);
         SUMA_DUMP_TRACE("%s", FuncName);
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
           (Opt->NodeLim > 0 && Clust->N_Node < Opt->NodeLim) ||
           (Opt->NodeLim < 0 && Opt->AreaLim < 0 && 
            Clust->N_Node < -Opt->AreaLim) ) { /* The last option
                                       is to allow users to use a negative
                                       area in the interface to have the effect
                                       of limiting by number of nodes */
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
                  DList *list, FILE *Out, int detail, char *params, char *opts) 
{
   static char FuncName[]={"SUMA_Show_SurfClust_list"};
   char *s = NULL;
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;

   s = SUMA_Show_SurfClust_list_Info(list,  detail, params, opts);
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
char *SUMA_Show_SurfClust_list_Info(DList *list, int detail, char *params, 
                                    char *opts) 
{
   static char FuncName[]={"SUMA_Show_SurfClust_list_Info"};
   int i, ic, max;
   SUMA_STRING *SS = NULL;
   DListElmt *elmt=NULL;
   SUMA_CLUST_DATUM *cd=NULL;
   char *s=NULL, *pad_str, str[20];   
   int lc[]= { 6, 6, 9, 9, 9, 6, 6, 9, 6, 9, 6, 9, 9, 9, 8, 9, 8, 
               9, 9, 9, 9, 9, 9 };
   char Col[][12] = { 
      {"# Rank"}, {"num Nd"}, {"Area"}, {"Mean"}, 
      {"|Mean|"},{"Cent"}, {"W Cent"},{"Min V"}, 
      {"Min Nd"}, {"Max V"}, {"Max Nd"} , {"Var"}, {"SEM"},
      {"Min |V|"}, {"|Min| Nd"}, {"Max |V|"}, {"|Max| Nd"},
      {"COM x"},  {"COM y"}, {"COM z"}, {"Cent x"},  {"Cent y"}, {"Cent z"} };  
   
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
      if (!opts || !strstr(opts,"No1DColHead")) {
         SS = SUMA_StringAppend (SS,"#Col. 0  = Rank \n"
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
                                    "#Col. 13 = Minimum |value|\n"
                                    "#Col. 14 = |Minimum| node\n"
                                    "#Col. 15  = Maximum |value|\n"
                                    "#Col. 16 = |Maximum| node\n"
                                    "#Col. 17 = Center of Mass x\n"
                                    "#Col. 18 = Center of Mass y\n"
                                    "#Col. 19 = Center of Mass z\n"
                                    "#Col. 20 = Centroid x\n"
                                    "#Col. 21 = Centroid y\n"
                                    "#Col. 22 = Centroid z\n"
                                    );
      } 
      SS = SUMA_StringAppend_va (SS,"#Command history:\n"
                                    "#%s\n", params);
      for (ic=0; ic<23; ++ic) {
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
                  "   %9.3f   %8d"
                  "   %9.3f   %8d"
                  "   %9.3f   %9.3f   %9.3f"
                  "   %9.3f   %9.3f   %9.3f"
                  , ic, cd->N_Node, cd->totalarea
                  , cd->totalvalue/((float)cd->N_Node)
                  , cd->totalabsvalue/((float)cd->N_Node)
                  , cd->centralnode, cd->weightedcentralnode 
                  , cd->minvalue, cd->minnode
                  , cd->maxvalue, cd->maxnode
                  , cd->varvalue, sqrt(cd->varvalue/cd->N_Node)
                  , cd->minabsvalue, cd->minabsnode
                  , cd->maxabsvalue, cd->maxabsnode
                  , cd->com[0]/cd->totalvalue
                  , cd->com[1]/cd->totalvalue
                  , cd->com[2]/cd->totalvalue
                  , cd->cen[0]/((float)cd->N_Node)
                  , cd->cen[1]/((float)cd->N_Node)
                  , cd->cen[2]/((float)cd->N_Node)
                  );
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

/*! Turn SurfClust list nimly*/
NI_element *SUMA_SurfClust_list_2_nel(DList *list, int detail, char *params, 
                                     char *opts) 
{
   static char FuncName[]={"SUMA_SurfClust_list_2_nel"};
   int i, ic;
   DListElmt *elmt=NULL;
   SUMA_CLUST_DATUM *cd=NULL;
   NI_element *nel=NULL;
   char *s=NULL;  
   float *fv=NULL;
   int *iv=NULL; 
   int tlc[]= { NI_INT, NI_INT, NI_FLOAT, NI_FLOAT, 
                NI_FLOAT, NI_INT, NI_INT, NI_FLOAT, 
                NI_INT, NI_FLOAT, NI_INT, NI_FLOAT, NI_FLOAT, 
                NI_FLOAT, NI_INT, NI_FLOAT, NI_INT };
   char Col[][12] = { 
      {"Rank"}, {"num Nd"}, {"Area"}, {"Mean"}, 
      {"|Mean|"},{"Cent"}, {"W Cent"},{"Min V"}, 
      {"Min Nd"}, {"Max V"}, {"Max Nd"} , {"Var"}, {"SEM"},
      {"Min |V|"}, {"|Min| Nd"}, {"Max |V|"}, {"|Max| Nd"} };
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   
   if (!list) {
      SUMA_RETURN(nel);  
   }
   
   nel = NI_new_data_element("SurfClust",list->size);
   s = SUMA_copy_string(Col[0]);
   for (i=1;i<17;++i) {
      s = SUMA_append_replace_string(s,Col[i],";",1);
   }
   NI_set_attribute(nel,"ColumnLabels",s);
   NI_set_attribute(nel,"CommandLine",params);

   
   for (i=0;i<17;++i) {
      NI_add_column(nel, tlc[i], NULL);
      elmt = NULL; 
      ic = 0; 
      do {
         if (!elmt) elmt = dlist_head(list); else elmt = elmt->next;
         if (!elmt)  {
            SUMA_S_Warnv(" cluster %d element is NULL!\n", ic);
         } else {
            switch(tlc[i]) {
               case NI_FLOAT:
                  fv = (float*)nel->vec[i];
                  break;
               case NI_INT:
                  iv = (int*)nel->vec[i];
                  break;
               default:
                  SUMA_S_Errv("Not ready for type %d, col %d\n", tlc[i], i);
                  break;
            }
            cd = (SUMA_CLUST_DATUM *)elmt->data;
            switch(i) {
               case 0:
                  iv[ic] = ic+1; break;
               case 1:
                  iv[ic] = cd->N_Node; break;
               case 2:
                  fv[ic] = cd->totalarea; break;
               case 3:
                  fv[ic] = cd->totalvalue/((float)cd->N_Node); break;
               case 4:  
                  fv[ic] = cd->totalabsvalue/((float)cd->N_Node); break;
               case 5:  
                  iv[ic] = cd->centralnode; break;
               case 6:  
                  iv[ic] = cd->weightedcentralnode; break;
               case 7:  
                  fv[ic] = cd->minvalue; break;
               case 8:  
                  iv[ic] = cd->minnode; break;
               case 9:  
                  fv[ic] = cd->maxvalue; break;
               case 10:  
                  iv[ic] = cd->maxnode; break;
               case 11:
                  fv[ic] = cd->varvalue; break;
               case 12: 
                  fv[ic] = sqrt(cd->varvalue/cd->N_Node); break;
               case 13:
                  fv[ic] = cd->minabsvalue; break;
               case 14:
                  iv[ic] = cd->minabsnode; break;
               case 15:
                  fv[ic] = cd->maxabsvalue; break;
               case 16:
                  iv[ic] = cd->maxabsnode; break;
               default:
                  SUMA_S_Errv("Not ready for column %d\n", i);
                  break;
            }
         }
         ++ic; 
      } while (elmt != dlist_tail(list));
   }
   
   if (LocalHead) SUMA_ShowNel(nel);
   SUMA_RETURN (nel);
}

byte *SUMA_ClustList2Mask(DList *list, int NodeMax)
{
   static char FuncName[]={"SUMA_ClustList2Mask"};
   int j, cnt, crank;
   DListElmt *elmt=NULL;
   SUMA_CLUST_DATUM *cd=NULL;
   byte *ismask=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!list || list->size == 0) {
      SUMA_SL_Err("NULL or empty list");
      SUMA_RETURN(ismask);  
   }
   /* which nodes in mask ? */
   ismask = (byte *)SUMA_calloc(NodeMax, sizeof(byte)); 
               /* you need to allocate for NodeMax (SO->N_Node), 
                 to be safe, otherwise you will need
                 to search for the highest node index.. */
   elmt = NULL; cnt = 0; crank = 0;
   do {
      ++crank;
      if (!elmt) elmt = dlist_head(list);
      else elmt = elmt->next;
      cd = (SUMA_CLUST_DATUM *)elmt->data; 
      for (j=0; j<cd->N_Node; ++j) {
            if(LocalHead) fprintf (SUMA_STDERR,"nic=%d\t", cd->NodeList[j]);
            ismask[cd->NodeList[j]] = (byte)crank;
            ++cnt;
      }
   } while (elmt != dlist_tail(list));
   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s:\n%d nodes in cluster list.\n", FuncName, cnt);
   
   SUMA_RETURN(ismask);
   
}

/*! Masks a data set by a clustering list*/
SUMA_DSET *SUMA_MaskDsetByClustList(
   SUMA_DSET *idset, SUMA_SurfaceObject *SO, 
   DList *list, SUMA_Boolean FullList, char *leName) 
{
   static char FuncName[]={"SUMA_MaskDsetByClustList"};
   int i, j;
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
   if (!(ismask = SUMA_ClustList2Mask(list, SO->N_Node))) {
      SUMA_S_Err("No nodes in list or bad list");
      SUMA_RETURN(dset);  
   }
   
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
   \brief Finds a node that best approximates the property of the center of 
          mass on the surface.
   The real center of mass of a curved surface is rarely on the surface, 
   so this is an attempt at localizing a node that is central to an ROI 
   and that can reflect the weight, or activity, distribution in that ROI.
   
   \param SO (SUMA_SurfaceObject *)
   \param cd (SUMA_CLUST_DATUM *) basically the output of 
                                  SUMA_SUMA_Build_Cluster_From_Node or 
                                  an element of the list returned by 
                                  SUMA_FindClusters
               Function will fill centralnode and weightedcentralnode
   \param UseSurfDist (int) 0: use distances along the surface 
                               (approximated by distances on the graph)
                            1: use Euclidian distances. 
   \return ans (int) : NOPE failed, YUP succeeded.
                       
*/
int SUMA_ClusterCenterofMass  (SUMA_SurfaceObject *SO, SUMA_CLUST_DATUM *cd, 
                               int UseSurfDist)
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
   for (c=0; c < OS.N_Neighb; ++c) { 
      DistVec[OS.Neighb_ind[c]] = OS.Neighb_dist[c]; 
   }
   
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
         centrality[c] = 0.0; weightedcentrality[c] = 0.0; 
         mtotal_w = 0.0; /* recalculated each time, not a big deal ... */
         for (i=0; i<cd->N_Node; ++i) {
            mi_w = cd->ValueList[i];
            mtotal_w += mi_w; 
            ni = cd->NodeList[i];/*Node index into SO of other node in cluster*/
            SUMA_UNIT_VEC((&(SO->NodeList[3*ni])), (&(SO->NodeList[3*nanch])), 
                          Uia, dia);
            if (UseSurfDist) { 
               for (k=0; k<3;++k) { 
                  fac = Uia[k] * DistVec[ni]; 
                  s_w[k] += mi_w * fac; 
                  s[k] += fac;
            } } else { 
               for (k=0; k<3;++k) { 
                  fac = Uia[k] * dia; 
                  s_w[k] += mi_w * fac; 
                  s[k] += fac; 
            } }
         }
         SUMA_UNIT_VEC((&(SO->NodeList[3*nc])), (&(SO->NodeList[3*nanch])), 
                       Uca, dca);
         if (UseSurfDist) { 
            for (k=0; k<3;++k) { 
               fac = Uca[k] * DistVec[nc]; 
               s_w[k] -= mtotal_w * fac; 
               s[k] -= cd->N_Node * fac;  
         } } else { 
            for (k=0; k<3;++k) { 
               fac =  Uca[k] * dca; 
               s_w[k] -= mtotal_w * fac; 
               s[k] -= cd->N_Node * fac;
         } }
         
         SUMA_NORM_VEC(s, 3, centrality[c]); 
         SUMA_NORM_VEC(s_w, 3, weightedcentrality[c]); 
         SUMA_LH("call %d, Centrality of node %d / %d = %f , weighted %f\n", 
                  ncall, nc, nanch, centrality[c], weightedcentrality[c]); 
         if (c == 0) { 
            mincentrality = centrality[c]; 
            centralnode = nc; 
            minweightedcentrality = weightedcentrality[c]; 
            weightedcentralnode = nc; 
         } else {
            if (centrality[c] < mincentrality) { 
               mincentrality = centrality[c]; 
               centralnode = nc; 
            }
            if (weightedcentrality[c] < minweightedcentrality) { 
               minweightedcentrality = weightedcentrality[c]; 
               weightedcentralnode = nc; 
            }
         }
         
      }
   }
   cd->centralnode = centralnode;
   cd->weightedcentralnode = weightedcentralnode;
   SUMA_LH("Central node chosen %d, weighted %d\n", 
           cd->centralnode, cd->weightedcentralnode);
   #else
   if (cd->N_Node == 1) {
      centralnode = cd->NodeList[0];
   } else {
      for (c=0; c < OS.N_Neighb; ++c) {
         nc = OS.Neighb_ind[c]; /* Node index into SO of center node */
         s[0] = s[1] = s[2] = 0.0;
         centrality[c] = 0.0; 
         mtotal = 0.0;/*recalculated each time, not a big deal ... */
         for (i=0; i<cd->N_Node; ++i) {
            if (WeightByValue) mi = cd->ValueList[i];
            else mi = 1.0;
            mtotal += mi;
            ni = cd->NodeList[i];/*Node index into SO of other node in cluster*/
            SUMA_UNIT_VEC((&(SO->NodeList[3*ni])), (&(SO->NodeList[3*nanch])), 
                          Uia, dia);
            if (UseSurfDist) { 
               for (k=0; k<3;++k) s[k] += mi * Uia[k] * DistVec[ni]; 
            } else { for (k=0; k<3;++k) s[k] += mi * Uia[k] * dia; }
         }
         SUMA_UNIT_VEC((&(SO->NodeList[3*nc])), (&(SO->NodeList[3*nanch])), 
                       Uca, dca);
         if (UseSurfDist) { 
            for (k=0; k<3;++k) s[k] -= mtotal * Uca[k] * DistVec[nc]; 
         } else { for (k=0; k<3;++k) s[k] -= mtotal * Uca[k] * dca; }
         
         SUMA_NORM_VEC(s, 3, centrality[c]);
         SUMA_LH("call %d, Centrality of node %d / %d = %f\n", 
                 ncall, nc, nanch, centrality[c]); 
         if (c == 0) { mincentrality = centrality[c]; centralnode = nc; }
         else if (centrality[c] < mincentrality) { 
            mincentrality = centrality[c]; centralnode = nc; 
         }
      }
   }
   cd->centralnode = centralnode;
   SUMA_LH("Central node chosen %d\n", cd->centralnode);
   cd->weightedcentralnode = -1;
   #endif   
   
   if (CoverThisNode) SUMA_free(CoverThisNode); CoverThisNode = NULL;
   if (OS.Neighb_dist) SUMA_free(OS.Neighb_dist); OS.Neighb_dist = NULL;
   if (OS.Neighb_ind) SUMA_free(OS.Neighb_ind); OS.Neighb_ind = NULL;
   if (DistVec) SUMA_free(DistVec); DistVec = NULL;
   if (centrality) SUMA_free(centrality); centrality = NULL;
   if (weightedcentrality) SUMA_free(weightedcentrality); 
            weightedcentrality = NULL;

   ++ncall;
   SUMA_RETURN(YUP);  
}  
 
SUMA_Boolean SUMA_Sort_ClustersList (DList *list, 
                                     SUMA_SURF_CLUST_SORT_MODES SortMode)
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
               max_elmt = elmt; 
               cd_max = (SUMA_CLUST_DATUM *)max_elmt->data; elmt_comp = NULL;
               do {
                  if (!elmt_comp) elmt_comp = elmt->next; 
                  else elmt_comp = elmt_comp->next; 
                  cd_comp = (SUMA_CLUST_DATUM *)elmt_comp->data; 
                  if (SortMode == SUMA_SORT_CLUST_BY_NUMBER_NODES) {
                     if (cd_comp->N_Node > cd_max->N_Node) { 
                        max_elmt = elmt_comp; 
                        cd_max = (SUMA_CLUST_DATUM *)max_elmt->data; 
                     }  
                  }else if (SortMode == SUMA_SORT_CLUST_BY_AREA) {
                     if (cd_comp->totalarea > cd_max->totalarea) { 
                        max_elmt = elmt_comp; 
                        cd_max = (SUMA_CLUST_DATUM *)max_elmt->data; 
                     }  
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

int SUMA_NodeClustNumber(SUMA_OVERLAYS *Sover, int node, 
                         SUMA_SurfaceObject *SO, 
                         SUMA_CLUST_DATUM **cdp)
{
   static char FuncName[]={"SUMA_NodeClustNumber"};
   int icl=0;
   
   SUMA_ENTRY;
   
   if (cdp) *cdp = NULL;
   if (!Sover || node < 0) SUMA_RETURN(0);
   if (!SO) SO = SUMA_SO_of_ColPlane(Sover);
   if (!SO  || !Sover->ClustOfNode 
            || !Sover->ClustList) SUMA_RETURN(0);
   if (node >= SO->N_Node) SUMA_RETURN(0);
   if (cdp && Sover->ClustOfNode[node]) {
      *cdp = (SUMA_CLUST_DATUM *)dlist_ith_elmt_data(
                                 Sover->ClustList, Sover->ClustOfNode[node]-1);
      if (*cdp == NULL) {
         SUMA_S_Errv("Null datum for node %d's cluster %d.\n"
                     "This should not happen\n",
                     node, Sover->ClustOfNode[node]);
         SUMA_RETURN(0);
      }
   }
   SUMA_RETURN(Sover->ClustOfNode[node]);
}   


SUMA_SURFCLUST_OPTIONS *SUMA_create_SurfClust_Opt(char *init_for)
{
   static char FuncName[]={"SUMA_create_SurfClust_Opt"};
   SUMA_SURFCLUST_OPTIONS *Opt=NULL;
   int i;
   
   SUMA_ENTRY;
   
   Opt = (SUMA_SURFCLUST_OPTIONS *)
               SUMA_calloc(1, sizeof(SUMA_SURFCLUST_OPTIONS));
   
   /* DO NOT CHANGE THESE DEFAULTS, see init_for below */
   Opt->out_prefix = NULL;
   Opt->oform = SUMA_ASCII_NIML;
   Opt->DistLim = -1.5;
   Opt->AreaLim = -1.0;
   Opt->NodeLim = -1;
   Opt->in_name = NULL;
   Opt->nodecol = -1;
   Opt->labelcol = -1;
   Opt->OutROI = NOPE;
   Opt->OutClustDset = NOPE;
   Opt->FullROIList = NOPE;
   Opt->WriteFile = NOPE;
   Opt->DoThreshold = SUMA_NO_THRESH;
   Opt->ThreshR[0] = Opt->ThreshR[1] = 0.0;
   Opt->tind = 0;
   Opt->prepend_node_index = NOPE;
   Opt->update = 0;
   Opt->SortMode = SUMA_SORT_CLUST_NOT_SET;
   Opt->DoCentrality = 1;
   
   if (!init_for || !strcmp(init_for, "SurfClust")) {
      /* Do nothing stuff above is good to go */
   } else if (!strcmp(init_for, "InteractiveClust")) {
      Opt->update = -(30000);
      Opt->AreaLim = -1.0;
      Opt->NodeLim = -1;
      Opt->DoCentrality = 0; 
      Opt->DistLim = -1;
      Opt->SortMode = SUMA_SORT_CLUST_BY_AREA;
   } else {
      SUMA_S_Errv("Don't know how to init for %s\n"
                  "Returning with defaults for SurfClust program", init_for);
   }
   
   SUMA_RETURN(Opt);
}  

SUMA_SURFCLUST_OPTIONS *SUMA_free_SurfClust_Opt(SUMA_SURFCLUST_OPTIONS *Opt)
{
   static char FuncName[]={"SUMA_free_SurfClust_Opt"};
   SUMA_ENTRY;
   if (Opt) {
      if (Opt->out_prefix) SUMA_free(Opt->out_prefix); 
      SUMA_free(Opt);
   }
   SUMA_RETURN(NULL);
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
SUMA_DSET *SUMA_CalculateLocalStats(
   SUMA_SurfaceObject *SO, SUMA_DSET *din, 
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
      SUMA_S_Errv("Bad input: SO=%p, din=%p, nmask=%p, "
                  "rhood=%f, ncode=%d,code=%p, UseThisDout=%p\n", 
            SO, din, nmask, rhood, ncode, code, UseThisDout);
      SUMA_RETURN(NULL);
   }
  
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(din, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NULL);   
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
            ncoli = 
               SUMA_Copy_Part_Column(ind, 
                  NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)), 
                  SDSET_VECLEN(din), NULL, masked_only, &n_incopy);
         } else {
            ncoli = 
               SUMA_Copy_Part_Column(ind, 
                  NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)), 
                  SDSET_VECLEN(din), nmask, masked_only, &n_incopy);  
         }
         if (!ncoli) {
            SUMA_SL_Err("No index data got copied.");
            SUMA_RETURN(NULL);
         }
         dout = SUMA_CreateDsetPointer("LocalStat", SUMA_NODE_BUCKET, NULL,  
                                       SDSET_IDMDOM(din), n_incopy);
         if (!SUMA_AddDsetNelCol (dout, NI_get_attribute(din->inel,"COLMS_LABS"),                                    SUMA_NODE_INDEX, ncoli, NULL ,1)) {
            SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
            SUMA_FreeDset((void*)dout); dout = NULL;
            SUMA_RETURN(NULL);
         }
         if (lblcp) SUMA_free(lblcp); lblcp = NULL;
         if (ncoli) SUMA_free(ncoli); ncoli = NULL; 
      } else {
         SUMA_S_Err( "Do not have node indices in input dset!\n"
                     " and could not create one.");
         SUMA_RETURN(NULL);
      }
   }
   
   /* some checks? Some day? */
   if (dout == UseThisDout) {
      if (SDSET_VECLEN(dout) != SDSET_VECLEN(din)) {
         SUMA_S_Errv("Mismatch in recycled dset (%d rows)"
                     " and input dset (%d rows)\n", 
                     SDSET_VECLEN(dout),  SDSET_VECLEN(din));
         SUMA_FreeDset((void*)dout); dout = NULL;
         SUMA_RETURN(NULL);
      }
      if (SDSET_VECNUM(dout) != SDSET_VECNUM(din)) {
         SUMA_S_Errv("Mismatch in recycled dset (%d cols) "
                     "and input dset (%d cols)\n", 
                     SDSET_VECNUM(dout),  SDSET_VECNUM(din));
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
            SUMA_RETURN(NULL);
         }
         fout = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
         if (!fout) {
            SUMA_SL_Crit("Failed to allocate fout!");
            SUMA_RETURN(NULL);
         }
         /* make sure column is not sparse, one value per node */
         if (k==0) {
            SUMA_LH( "Special case k = 0, going to"
                     " SUMA_MakeSparseColumnFullSorted");
            bfull = NULL;
            if (!SUMA_MakeSparseColumnFullSorted(
                     &fin_orig, SDSET_VECFILLED(din), 0.0, 
                     &bfull, din, SO->N_Node)) {
               SUMA_S_Err("Failed to get full column vector");
               SUMA_RETURN(NULL);
            }
            if (bfull) {
               SUMA_LH( 
                  "Something was filled in SUMA_MakeSparseColumnFullSorted\n" );
               if (nmask) {   /* combine bfull with nmask */
                  SUMA_LH( "Merging masks\n" );
                  for (jj=0; jj < SO->N_Node; ++jj) { 
                     if (nmask[jj] && !bfull[jj]) nmask[jj] = 0; 
                  }   
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
            if (!UseThisOffset && code[ic] != NSTAT_FWHMx) { /* no need for 
               Offset with FWHMx, zones must be big, too big for this method */
               SUMA_LH("Calculating OffS_out ...");
               OffS_out = SUMA_FormNeighbOffset (SO, rhood, NULL, nmask, -1.0);
            } else {
               OffS_out = UseThisOffset;
            }
         } else {
            SUMA_LH( "going to SUMA_MakeSparseColumnFullSorted");
            if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(din),                                                   0.0, NULL, din, SO->N_Node)) {
               SUMA_S_Err("Failed to get full column vector");
               SUMA_RETURN(NULL);
            }
            /* no need for reworking nmask and bfull for each column...*/
         }
         /* Now I have the data column, nice and solid , do the stats */
         switch (code[ic]) {
            case NSTAT_MEAN:
               lblcp = SUMA_DsetColLabelCopy(din, icols[k], 1); 
               lblcp = SUMA_append_replace_string("mean_", lblcp, "", 2);
               if (!SUMA_AddDsetNelCol (dout, lblcp, SUMA_NODE_FLOAT, 
                                        (void *)fout, NULL ,1)) {
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
                        if (OffS_out[n].Neighb_dist[j] <= rhood) { 
                           fp += fin_orig[nj]; ++nval; }
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
                              if (OffS_out[n].Neighb_dist[j] <= rhood) { 
                                 fp += fin_orig[nj]; ++nval; }
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
               lblcp = SUMA_DsetColLabelCopy(din, icols[k], 1); 
               lblcp = SUMA_append_replace_string("fwhm_", lblcp, "", 2);
               if (!SUMA_AddDsetNelCol (dout, lblcp, SUMA_NODE_FLOAT, 
                                          (void *)fout, NULL ,1)) {
                  SUMA_S_Crit("Failed to add dset column");
                  SUMA_RETURN(NULL);
               }
               /* form a mask for fwhm function */
               if (!(fwhm_mask = 
                        (byte *)SUMA_calloc(SO->N_Node, sizeof(byte)))) {
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
                  for (ipl=0; ipl<3; ++ipl) { 
                     SUMA_FREE_DLIST(striplist_vec[ipl]); 
                     striplist_vec[ipl] = NULL; }
               }
               SUMA_free(fwhm_mask); fwhm_mask = NULL;
               SUMA_free(lblcp); lblcp = NULL;
               break;
               
            case NSTAT_FWHMx:
               lblcp = SUMA_DsetColLabelCopy(din, icols[k], 1); 
               lblcp = SUMA_append_replace_string("fwhm_", lblcp, "", 2);
               if (!SUMA_AddDsetNelCol (dout, lblcp, SUMA_NODE_FLOAT, 
                                          (void *)fout, NULL ,1)) {
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
               SegDist = SUMA_SegmentDistortion(SO, SOf); 
                  /* this function should return a vector of 1s if SO == SOf */
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
            SUMA_RETURN(NULL);      
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
