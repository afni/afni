#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  

/*!
   Change SUMA_MorphInfo for MapIcosahedron into
   SUMA_M2M_STRUCT, populating just enough fields
   that you can use the function SUMA_M2M_interpolate */
SUMA_M2M_STRUCT *SUMA_MorphInfo2M2M(SUMA_MorphInfo *MI)
{
   static char FuncName[]={"SUMA_MorphInfo2M2M"};
   SUMA_M2M_STRUCT *M2M=NULL;
   int i=0;
   
   SUMA_ENTRY;
   
   M2M = (SUMA_M2M_STRUCT*)SUMA_calloc(1, sizeof(SUMA_M2M_STRUCT));
   M2M->M1_N_Nodes = MI->N_Node_std;
   M2M->M2_N_Nodes = MI->N_Node_orig;
   M2M->M1Nn = MI->N_Node_std; /* number of nodes on std mesh */
                           /* node indices on std mesh */
   if (!(M2M->M1n = (int *)SUMA_calloc(MI->N_Node_std, sizeof(int)))) {
      SUMA_S_Crit("Failed to allocate. Leaky return."); SUMA_RETURN(NULL);
   }  
   for (i=0;i<MI->N_Node_std; ++i) M2M->M1n[i]=i;
   
   M2M->M2t_M1n = NULL;    /* not needed by SUMA_M2M_interpolate */
   M2M->M2pb_M1n = NULL;   /* not needed by SUMA_M2M_interpolate */
   M2M->M2p_M1n = NULL;   /* not needed by SUMA_M2M_interpolate */
   M2M->PD = NULL;   /* not needed by SUMA_M2M_interpolate */
   
   if (!(M2M->M2Nne_M1n = (int *)SUMA_calloc(MI->N_Node_std, sizeof(int)))) {
      SUMA_S_Crit("Failed to allocate. Leaky return."); SUMA_RETURN(NULL);
   }
   if (!(M2M->M2ne_M1n = (int **)SUMA_calloc(MI->N_Node_std, sizeof(int*)))) {
      SUMA_S_Crit("Failed to allocate. Leaky return."); SUMA_RETURN(NULL);
   } 
   for (i=0;i<MI->N_Node_std; ++i) 
      M2M->M2ne_M1n[i] = (int *) SUMA_malloc(3*sizeof(int));
   if (!(M2M->M2we_M1n = (double **)
                     SUMA_calloc(MI->N_Node_std, sizeof(double*)))) {
      SUMA_S_Crit("Failed to allocate. Leaky return."); SUMA_RETURN(NULL);
   }
   for (i=0;i<MI->N_Node_std; ++i) 
      M2M->M2we_M1n[i] = (double *) SUMA_malloc(3*sizeof(double));
   
   for (i=0;i<MI->N_Node_std; ++i) {
      M2M->M2Nne_M1n[i] = 3;
      M2M->M2we_M1n[i][0] = (double)MI->Weight[3*i];
      M2M->M2ne_M1n[i][0] = MI->ClsNodes[3*i];
      
      M2M->M2we_M1n[i][1] = (double)MI->Weight[3*i+1];
      M2M->M2ne_M1n[i][1] = MI->ClsNodes[3*i+1];
      if (M2M->M2we_M1n[i][1] > M2M->M2we_M1n[i][0]) { /* second is closer */
         /* swap */
         M2M->M2we_M1n[i][1] = M2M->M2we_M1n[i][0];
         M2M->M2we_M1n[i][0] = (double)MI->Weight[3*i+1];
         M2M->M2ne_M1n[i][1] = M2M->M2ne_M1n[i][0];
         M2M->M2ne_M1n[i][0] = MI->ClsNodes[3*i+1];
      }   
      
      M2M->M2we_M1n[i][2] = (double)MI->Weight[3*i+2];
      M2M->M2ne_M1n[i][2] = MI->ClsNodes[3*i+2];
      if (M2M->M2we_M1n[i][2] > M2M->M2we_M1n[i][0]) { /* third is closer */
         /* swap */
         M2M->M2we_M1n[i][2] = M2M->M2we_M1n[i][0];
         M2M->M2we_M1n[i][0] = (double)MI->Weight[3*i+2];
         M2M->M2ne_M1n[i][2] = M2M->M2ne_M1n[i][0];
         M2M->M2ne_M1n[i][0] = MI->ClsNodes[3*i+2];
      }      
   }   

   SUMA_RETURN(M2M);
}  

SUMA_M2M_STRUCT *SUMA_NewM2M(char *SO1_id, int N_SO1_nodes, 
                             char *SO2_id, int N_SO2_nodes)
{
   static char FuncName[]={"SUMA_NewM2M"};
   SUMA_M2M_STRUCT *M2M=NULL;
   SUMA_ENTRY;
   
   /* if (!SO1_id || !SO2_id) SUMA_RETURN(M2M); */
   
   M2M = (SUMA_M2M_STRUCT*)SUMA_calloc(1, sizeof(SUMA_M2M_STRUCT));
   
   M2M->M1Nn = N_SO1_nodes;
   M2M->M1_N_Nodes = N_SO1_nodes;
   M2M->M2_N_Nodes = N_SO2_nodes;
   M2M->M1n = (int*)SUMA_calloc(M2M->M1Nn, sizeof(int));
   M2M->M2t_M1n = (int*)SUMA_calloc(M2M->M1Nn, sizeof(int));
   M2M->M2Nne_M1n = (int*)SUMA_calloc(M2M->M1Nn, sizeof(int));
   M2M->M2ne_M1n = (int**)SUMA_calloc(M2M->M1Nn, sizeof(int*));
   M2M->M2pb_M1n = (float *)SUMA_calloc(2*M2M->M1Nn, sizeof(float));
   M2M->M2p_M1n = (float *)SUMA_calloc(3*M2M->M1Nn, sizeof(float));
   M2M->PD = (double *)SUMA_calloc(M2M->M1Nn, sizeof(double));
   M2M->M2we_M1n = (double**)SUMA_calloc(M2M->M1Nn, sizeof(double*));
   if (!M2M->M1n || !M2M->M2t_M1n || 
       !M2M->M2Nne_M1n || !M2M->M2ne_M1n || !M2M->M2we_M1n) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   M2M->M1_IDcode = M2M->M2_IDcode = NULL;
   
   if (SO1_id) M2M->M1_IDcode = SUMA_copy_string(SO1_id);
   if (SO2_id) M2M->M2_IDcode = SUMA_copy_string(SO2_id);
   
   
   SUMA_RETURN(M2M);
}  

SUMA_M2M_STRUCT *SUMA_FreeM2M(SUMA_M2M_STRUCT *M2M)
{
   static char FuncName[]={"SUMA_FreeM2M"};
   int i;
   
   SUMA_ENTRY;
   
   if (!M2M) SUMA_RETURN(NULL);
   if (M2M->M2we_M1n) {
      for (i=0; i<M2M->M1Nn; ++i) { 
         if (M2M->M2we_M1n[i]) {
            SUMA_free(M2M->M2we_M1n[i]);
            M2M->M2we_M1n[i] = NULL;
         }
      }
      SUMA_free(M2M->M2we_M1n);
      M2M->M2we_M1n = NULL;
   }
   if (M2M->M2ne_M1n) {
      for (i=0; i<M2M->M1Nn; ++i) { 
         if (M2M->M2ne_M1n[i]) {
            SUMA_free(M2M->M2ne_M1n[i]);
            M2M->M2ne_M1n[i] = NULL;
         }
      }
      SUMA_free(M2M->M2ne_M1n);
      M2M->M2ne_M1n = NULL;
   }
   if (M2M->M1n) SUMA_free(M2M->M1n); M2M->M1n = NULL;
   if (M2M->M2t_M1n) SUMA_free(M2M->M2t_M1n); M2M->M2t_M1n= NULL;
   if (M2M->M2Nne_M1n) SUMA_free(M2M->M2Nne_M1n); M2M->M2Nne_M1n = NULL;
   if (M2M->M2pb_M1n) SUMA_free(M2M->M2pb_M1n); M2M->M2pb_M1n = NULL;
   if (M2M->M2p_M1n) SUMA_free(M2M->M2p_M1n); M2M->M2p_M1n = NULL;
   if (M2M->PD) SUMA_free(M2M->PD); M2M->PD = NULL;
   if (M2M->M1_IDcode) SUMA_free(M2M->M1_IDcode); M2M->M1_IDcode = NULL;
   if (M2M->M2_IDcode) SUMA_free(M2M->M2_IDcode); M2M->M2_IDcode = NULL;

   SUMA_free(M2M);
   SUMA_RETURN(NULL);     
} 
  
char *SUMA_M2M_node_Info (SUMA_M2M_STRUCT *M2M, int node)
{
   static char FuncName[]={"SUMA_M2M_node_Info"};
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   int i, found, j;
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend(NULL, NULL);
      
   if (!M2M) { SS = SUMA_StringAppend(SS,"NULL M2M"); goto CLEAN_RETURN; }
   
   if (M2M->M1_IDcode) { 
      SS = SUMA_StringAppend_va(SS, "M1_IDcode %s\n", M2M->M1_IDcode); }
   else { SS = SUMA_StringAppend_va(SS, "M1_IDcode is NULL\n"); }
   if (M2M->M2_IDcode) { 
      SS = SUMA_StringAppend_va(SS, "M2_IDcode %s\n", M2M->M2_IDcode); }
   else { SS = SUMA_StringAppend_va(SS, "M2_IDcode is NULL\n"); }
  
   i = 0; found = 0;
   while (i < M2M->M1Nn && !found) {
      if (M2M->M1n[i] == node) {
         found = 1;
      } else ++i;
   }
   
   if (!found) { 
      SS = SUMA_StringAppend_va (SS, "Node %d not found in M2M->M1n", node); 
      goto CLEAN_RETURN; }
   
   SS = SUMA_StringAppend_va (SS, "Mapping results for node %d (n1) of mesh 1 (M1 %d nodes):\n", M2M->M1n[i], M2M->M1_N_Nodes);
   SS = SUMA_StringAppend_va (SS, "Index of triangle (t2) in mesh 2 (M2 %d nodes) hosting n1: %d\n", M2M->M2_N_Nodes, M2M->M2t_M1n[i]);
   SS = SUMA_StringAppend_va (SS, "Projection coordinates in t2 (%f,%f,%f)\n", M2M->M2p_M1n[3*i], M2M->M2p_M1n[3*i+1], M2M->M2p_M1n[3*i+2]);
   SS = SUMA_StringAppend_va (SS, "Projection barycentric coordinates in t2 (%g,%g)\n", M2M->M2pb_M1n[2*i], M2M->M2pb_M1n[2*i+1]);
   SS = SUMA_StringAppend_va (SS, "Projection distance of n1 onto t2 is: %g\n", M2M->PD[i]);
   SS = SUMA_StringAppend_va (SS, "Number of nodes (n2) in M2 considered neighbors to n1: %d\n", M2M->M2Nne_M1n[i]);
   SS = SUMA_StringAppend_va (SS, "n2   \tw2weight\n");
   for (j=0; j< M2M->M2Nne_M1n[i]; ++j) {
      SS = SUMA_StringAppend_va (SS, "%s\t%g\n", MV_format_fval2(M2M->M2ne_M1n[i][j], 5), M2M->M2we_M1n[i][j]);
   }
   
   
   CLEAN_RETURN:
   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
} 
/*!
   \brief a function to find the mapping on nodes from SO1's mesh M1 to 
          SO2's mesh M2
   The mapping method is Nearest Neighbor, each node nj considered from M1, 
         will get for neighbors the nodes forming the triangle in M2 that 
         nj projects to.
   
   \param SO1 (SUMA_SurfaceObject *) surface 1
   \param SO2 (SUMA_SurfaceObject *) surface 2
   \param NL_1 (int *) list of node indices to consider from surface 1 
               IF NULL, then all nodes of surface 1 are considered
   \param N_NL_1 (int) number of values in NL_1 (= SO1->N_Node if NL_1 is NULL)
   \param PD_1 (float *) vector of SO1->N_Node triplets specifying the direction
                         of the projection of nodes in NL_1. 
             PD_1[3*NL_1[j]], PD_1[3*NL_1[j]+1], PD_1[3*NL_1[j]+2] are the 
                        projection directions of node NL_1[j].
                  If NULL then the projection direction for node NL_1[j] is:
             SO1->NodeNormList[3*NL_1[j]], SO1->NodeNormList[3*NL_1[j]+1], 
               SO1->NodeNormList[3*NL_1[j]+2] 
   \param dlim (float) maximum distance to search in each direction, 
                     along the projection vector. 
                     If 0, then a default of 100.0 is used.
   \param ClosestPossible (int ) 0: Do nothing
                                1: If you find no intersection along the
                                   projection, take the closest node 
                                   from S2 as your result.
                                2: Same as 1, AND use closet node from 
                                   S2 even if you find a projection
                                   target as long as the closest node
                                   from S2 is closer than the projection.
   \return M2M (SUMA_M2M_STRUCT *) Mesh to Mesh mapping structure, 
                     see SUMA_SurfaceToSurface.h for details
*/ 
SUMA_M2M_STRUCT *SUMA_GetM2M_NN( SUMA_SurfaceObject *SO1, 
                                 SUMA_SurfaceObject *SO2,
                                 int *oNL_1, int N_NL_1, 
                                 float *PD_1, float dlim, 
                                 int NodeDbg, int ClosestPossible)
{
   static char FuncName[]={"SUMA_GetM2M_NN"};
   SUMA_M2M_STRUCT *M2M = NULL;
   int *NL_1 = NULL;
   int j, id, id2, nnt, k, nj, t3, j3, n2closest=-1;
   float *triNode0, *triNode1, *triNode2, *hit;
   float delta_t;
   double *wv, wgt[3], weight_tot, dhit, 
         n2closestd=12345679.0, n2closestvec[3]={0.0, 0.0, 0.0},
         dtmp=0.0; 
   float P0[3], P1[3], P2[3], N0[3], *PP=NULL;
   float Points[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} } ;
   SUMA_MT_INTERSECT_TRIANGLE *MTI = NULL;
   struct timeval tt; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_etime (&tt, 0);
   
   if (!SO1 || !SO2) { SUMA_SL_Err("NULL input"); goto CLEAN_EXIT; }
   if (!oNL_1) N_NL_1 = SO1->N_Node;
   if (N_NL_1 < 1) { SUMA_SL_Err("No nodes to consider"); goto CLEAN_EXIT; }
   if (dlim <= 0) dlim = 100.0; 
   /* start filling M2M */
   M2M = SUMA_NewM2M(SO1->idcode_str, N_NL_1, SO2->idcode_str, SO2->N_Node);
   if (!M2M) { SUMA_SL_Crit("Failed to create M2M"); goto CLEAN_EXIT; }
   
   /* fill up M2M->M1n */
   if (!oNL_1) { for (j=0; j<N_NL_1; ++j) M2M->M1n[j]= j; }
   else { for (j=0; j<N_NL_1; ++j) M2M->M1n[j]= oNL_1[j]; }
   
   if (!PD_1) PD_1 = SO1->NodeNormList;
   else {
      /* be safe and normalize */
      for (j = 0; j < M2M->M1Nn; ++j) {
         nj    = M2M->M1n[j];
         id    = SO1->NodeDim * nj;
         PP = PD_1+id;
         SUMA_NORM(dtmp,PP);
         if(dtmp != 0.0) { PP[0] /= dtmp; 
                           PP[1] /= dtmp;
                           PP[2] /= dtmp; }
      }
   }
   
   for (j = 0; j < M2M->M1Nn; ++j) {
      j3    = 3 * j;
      nj    = M2M->M1n[j];
      id    = SO1->NodeDim * nj;
      PP    = SO1->NodeList+id;
      P0[0] = SO1->NodeList[id];
      P0[1] = SO1->NodeList[id+1];
      P0[2] = SO1->NodeList[id+2];

      N0[0] = PD_1[id  ];
      N0[1] = PD_1[id+1];
      N0[2] = PD_1[id+2];
      
      SUMA_POINT_AT_DISTANCE(N0, P0, dlim, Points);
      
      P1[0] = Points[0][0];
      P1[1] = Points[0][1];
      P1[2] = Points[0][2];
      P2[0] = Points[1][0];
      P2[1] = Points[1][1];
      P2[2] = Points[1][2];
      
      n2closest = -1;
      if (ClosestPossible) {
         /* Find the closest node in S02 to node in SO1 */
         SUMA_CLOSEST_NODE_VEC(SO2, PP, n2closest, n2closestd, n2closestvec); 
      } 
      if (ClosestPossible != 3) { 
         /* now determine the distance along normal */
         MTI = SUMA_MT_intersect_triangle(P0, P1, SO2->NodeList, SO2->N_Node, 
                                       SO2->FaceSetList, SO2->N_FaceSet, MTI, 0);
         if (LocalHead) 
            fprintf(SUMA_STDERR,"%s: number of hits for node %d : %d\n", 
                                FuncName, nj, MTI->N_hits);
      }  
      if (ClosestPossible ==3 || MTI->N_hits ==0) {
         if (n2closest < 0) {/* no other option */
            if (LocalHead) 
               fprintf(SUMA_STDERR, 
                    "%s: Could not find hit for node %d in either direction.\n", 
                       FuncName, nj);
            M2M->M2Nne_M1n[j] = 0;
            M2M->M2t_M1n[j] = -1;
            M2M->PD[j] = 0.0;
            M2M->M2pb_M1n[2*j  ] = -1.0;
            M2M->M2pb_M1n[2*j+1] = -1.0;
            j3 = 3*j; hit = &(M2M->M2p_M1n[j3]);
            hit[0] = -1.0;
            hit[1] = -1.0;
            hit[2] = -1.0; 
         } else {
            /* adopt closest node as hit */
            if (LocalHead) 
               fprintf(SUMA_STDERR, 
                    "%s: Could not find hit for node %d in either direction.\n"
                    "    Or user chose closest node always\n"
                    "Adopting closest node %d in S2\n", 
                       FuncName, nj, n2closest);
            M2M->M2Nne_M1n[j] = 1;
            M2M->M2ne_M1n[j] = (int *)SUMA_malloc(M2M->M2Nne_M1n[j]*sizeof(int));
            *(M2M->M2ne_M1n[j]  ) = n2closest;
            M2M->M2we_M1n[j] = 
               (double *)SUMA_malloc(M2M->M2Nne_M1n[j]*sizeof(double));
            *(M2M->M2we_M1n[j]  ) = 1.0;   
            M2M->M2t_M1n[j] = -1;
            M2M->PD[j] = (float)sqrtf(n2closestd);
            /* decide on the sign of the distance */
               SUMA_NORM(dtmp,n2closestvec);
               if(dtmp != 0.0) { n2closestvec[0] /= dtmp; 
                                 n2closestvec[1] /= dtmp;
                                 n2closestvec[2] /= dtmp; }
               if (PD_1[id+0]*n2closestvec[0]+
                   PD_1[id+1]*n2closestvec[1]+
                   PD_1[id+2]*n2closestvec[2] < 0.0) {
                     /* had to go opposite the projection direction */ 
                  M2M->PD[j] = -M2M->PD[j];   
               }
            M2M->M2pb_M1n[2*j  ] = -1.0;
            M2M->M2pb_M1n[2*j+1] = -1.0;
            j3 = 3*j; hit = &(M2M->M2p_M1n[j3]);
            hit[0] = SO2->NodeList[3*n2closest  ];
            hit[1] = SO2->NodeList[3*n2closest+1];
            hit[2] = SO2->NodeList[3*n2closest+2]; 
         }     
      } else {
         if (LocalHead) {
             for (k = 0; k < MTI->N_el; k++) {
               if (MTI->isHit[k] == YUP) 
                  fprintf( SUMA_STDERR, 
                           "%s: hit %d: %f (%f, %f)\n",
                           FuncName, k, MTI->t[k], MTI->u[k], MTI->v[k]);
            }
         }
         
         if (ClosestPossible > 1 && n2closest >= 0) { 
            /* compare distance with brutish closest node 
                                  and pick closest
            dhit = (MTI->P[0]-PP[0])*(MTI->P[0]-PP[0])+
                   (MTI->P[1]-PP[1])*(MTI->P[1]-PP[1])+
                   (MTI->P[2]-PP[2])*(MTI->P[2]-PP[2]); (or use MTI directly)*/
            dhit = MTI->t[MTI->ifacemin] * MTI->t[MTI->ifacemin];
            if (dhit <= n2closestd) n2closest=-1; /* cancel that option */ 
         } else {
            n2closest=-1; /* cancel that option */
         }
         
         if (!ClosestPossible || n2closest < 0) { /* take projection result */ 
             if (LocalHead) 
               fprintf(SUMA_STDERR, 
            "%s: keeping hit node %d for node %d at distance %f\n", 
                       FuncName, MTI->inodemin, nj, MTI->t[MTI->ifacemin]);
           
            /* store the hit location */
            j3 = 3*j; hit = &(M2M->M2p_M1n[j3]);
            hit[0] = MTI->P[0];
            hit[1] = MTI->P[1];
            hit[2] = MTI->P[2];

            M2M->M2t_M1n[j] = MTI->ifacemin;
            M2M->PD[j] = MTI->t[MTI->ifacemin];

            /* create the neighborhood list */
            M2M->M2Nne_M1n[j] = 3; /* always three neighbors for NN method, 
                                       first node always the closest! */
            M2M->M2ne_M1n[j] = (int *)SUMA_malloc(M2M->M2Nne_M1n[j]*sizeof(int));

            *(M2M->M2ne_M1n[j]  ) = MTI->inodemin;   /*! the closest node */
            nnt = (MTI->inodeminlocal+1)%3; 
            *(M2M->M2ne_M1n[j]+1) = SO2->FaceSetList[3*MTI->ifacemin+nnt]; 
                                                  /*index of second node in t2 */
            nnt = (MTI->inodeminlocal+2)%3; 
            *(M2M->M2ne_M1n[j]+2) = SO2->FaceSetList[3*MTI->ifacemin+nnt]; 
                                                 /*index of third node in t2 */



            if (M2M->M1n[j] == NodeDbg) {
               fprintf(SUMA_STDERR, 
                        "%s: Hit coords for node %d of M1: \n"
                        "%f %f %f\n"
                        "%f %f %f\n", 
                        FuncName, M2M->M1n[j], hit[0], hit[1], hit[2], 
                        M2M->M2p_M1n[j3], 
                        M2M->M2p_M1n[j3+1], 
                        M2M->M2p_M1n[j3+2]);
            }


            /* store the barycentric (u,v) location of intersection */
            M2M->M2pb_M1n[2*j  ] = MTI->u[MTI->ifacemin];
            M2M->M2pb_M1n[2*j+1] = MTI->v[MTI->ifacemin];

            /**determine weights which are the barycetric coordinates of the 
               intersection node.
               The intersected triangle is formed by the 1st three nodes 
               stored in  M2ne_M1n
               RESPECT THE ORDER in M2ne_M1n */
               /* get pointers to x,y,z of each node of intersected triangle*/
               t3 = 3*MTI->ifacemin;
               triNode0 = &(SO2->NodeList[ 3*M2M->M2ne_M1n[j][0] ]);
               triNode1 = &(SO2->NodeList[ 3*M2M->M2ne_M1n[j][1] ]);
               triNode2 = &(SO2->NodeList[ 3*M2M->M2ne_M1n[j][2] ]);

            SUMA_TRI_AREA( ((MTI->P)), triNode1, triNode2, wgt[0] ); 
            SUMA_TRI_AREA( ((MTI->P)), triNode0, triNode2, wgt[1] ); 
            SUMA_TRI_AREA( ((MTI->P)), triNode0, triNode1, wgt[2] ); 

            weight_tot =  wgt[0] + wgt[1] + wgt[2];

            /* Now for the weights of each neighboring node */
            M2M->M2we_M1n[j] = 
               (double *)SUMA_malloc(M2M->M2Nne_M1n[j]*sizeof(double));
            wv = M2M->M2we_M1n[j];
            if (weight_tot) {
               wv[0] = wgt[0] / weight_tot;
               wv[1] = wgt[1] / weight_tot;
               wv[2] = wgt[2] / weight_tot;
            }else { /* some triangles have zero area in FreeSurfer surfaces */
               wv[0] = wv[1] =  wv[2] = 1.0/3.0;
            }
         } else { /* just take the closest node there was */
            M2M->M2Nne_M1n[j] = 1;
            M2M->M2ne_M1n[j] = (int *)SUMA_malloc(M2M->M2Nne_M1n[j]*sizeof(int));
            *(M2M->M2ne_M1n[j]  ) = n2closest;
            M2M->M2we_M1n[j] = 
               (double *)SUMA_malloc(M2M->M2Nne_M1n[j]*sizeof(double));
            *(M2M->M2we_M1n[j]  ) = 1.0;   
            M2M->M2t_M1n[j] = -1;
            M2M->PD[j] = (float)sqrtf(n2closestd);
            /* decide on the sign of the distance */
               SUMA_NORM(dtmp,n2closestvec);
               if(dtmp != 0.0) { n2closestvec[0] /= dtmp; 
                                 n2closestvec[1] /= dtmp;
                                 n2closestvec[2] /= dtmp; }
               if (PD_1[id+0]*n2closestvec[0]+
                   PD_1[id+1]*n2closestvec[1]+
                   PD_1[id+2]*n2closestvec[2] < 0.0) {
                     /* had to go opposite the projection direction */ 
                  M2M->PD[j] = -M2M->PD[j];   
               }
            M2M->M2pb_M1n[2*j  ] = -1.0;
            M2M->M2pb_M1n[2*j+1] = -1.0;
            j3 = 3*j; hit = &(M2M->M2p_M1n[j3]);
            hit[0] = SO2->NodeList[3*n2closest  ];
            hit[1] = SO2->NodeList[3*n2closest+1];
            hit[2] = SO2->NodeList[3*n2closest+2]; 
            if (LocalHead) 
               fprintf(SUMA_STDERR, 
            "%s: Replace hit node %d for node %d with closest node %d in S2 \n"
                  "           at %f distance versus %f\n", 
                       FuncName, MTI->inodemin, nj, n2closest, 
                       M2M->PD[j],  MTI->t[MTI->ifacemin]);
         }
      }

      
      if (!(j%500) && j) {
         delta_t = SUMA_etime(&tt, 1);
         fprintf (SUMA_STDERR, 
                  " [%d]/[%d] %.2f/100%% completed. "
                  "  Dt = %.2f min done of %.2f min total\r" ,  
                  j, N_NL_1, (float)j / N_NL_1 * 100, 
                  delta_t/60, delta_t/j * N_NL_1/60);
         if (LocalHead) {
            char *s = NULL;
            s = SUMA_M2M_node_Info(M2M, M2M->M1n[j]);
            fprintf(SUMA_STDERR,"\n***\n%s\n***\n", s); 
            SUMA_free(s); s = NULL;
         }
      }

   }

   if (LocalHead) {
      delta_t = SUMA_etime(&tt, 1);
      fprintf (SUMA_STDERR, 
               " [%d]/[%d] %.2f/100%% completed. "
               "  Dt = %.2f min done of %.2f min total\r" ,  
               j, N_NL_1, (float)j / N_NL_1 * 100, 
               delta_t/60, delta_t/j * N_NL_1/60);
      fprintf (SUMA_STDERR, "\n");
   }

   CLEAN_EXIT:
   if (MTI) MTI = SUMA_Free_MT_intersect_triangle(MTI); 

   
   SUMA_RETURN(M2M);
}

SUMA_M2M_STRUCT * SUMA_niml_to_M2M(NI_group *ngr)
{
   static char FuncName[]={"SUMA_niml_to_M2M"};
   SUMA_M2M_STRUCT *M2M = NULL;
   int i=0, j=0, k=0, Nmax=0;
   int *itmp=NULL;
   double *dtmp = NULL;
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ngr) SUMA_RETURN(M2M);
   if (strcmp(ngr->name,"M2M")) {
      SUMA_S_Errv("NI_group name is %s, looking for M2M\n", ngr->name);
      SUMA_RETURN(M2M);
   }
   
   NI_GET_INT(ngr, "M1_N_Nodes", i);
   NI_GET_INT(ngr, "M2_N_Nodes", j);
   
   if (!(M2M = SUMA_NewM2M(NI_get_attribute(ngr,"M1_IDcode"), i, 
                           NI_get_attribute(ngr,"M2_IDcode"), j))) {
      SUMA_S_Err("Failed to initialize");
      SUMA_RETURN(M2M);
   }

   NI_GET_INT(ngr, "M1Nn", M2M->M1Nn);
   
   /* retrieve 1D arrays */
   if (!(nel =  SUMA_FindNgrNamedElement(ngr, "M1n"))) {
      SUMA_S_Err("Missing M1n");
      SUMA_RETURN(SUMA_FreeM2M(M2M));
   }
   memcpy(M2M->M1n, nel->vec[0], M2M->M1Nn*sizeof(int)); 
   
   if (!(nel =  SUMA_FindNgrNamedElement(ngr, "M2t_M1n"))) {
      SUMA_S_Err("Missing M2t_M1n");
      SUMA_RETURN(SUMA_FreeM2M(M2M));
   }
   memcpy(M2M->M2t_M1n, nel->vec[0], M2M->M1Nn*sizeof(int)); 
   
   if (!(nel =  SUMA_FindNgrNamedElement(ngr, "M2pb_M1n"))) {
      SUMA_S_Err("Missing M2pb_M1n");
      SUMA_RETURN(SUMA_FreeM2M(M2M));
   }
   memcpy(M2M->M2pb_M1n, nel->vec[0], 2*M2M->M1Nn*sizeof(float));
   
   if (!(nel =  SUMA_FindNgrNamedElement(ngr, "M2p_M1n"))) {
      SUMA_S_Err("Missing M2p_M1n");
      SUMA_RETURN(SUMA_FreeM2M(M2M));
   }
   memcpy(M2M->M2p_M1n, nel->vec[0], 3*M2M->M1Nn*sizeof(float));
   
   if (!(nel =  SUMA_FindNgrNamedElement(ngr, "PD"))) {
      SUMA_S_Err("Missing PD");
      SUMA_RETURN(SUMA_FreeM2M(M2M));
   }
   memcpy(M2M->PD, nel->vec[0], M2M->M1Nn*sizeof(double));
   
   if (!(nel =  SUMA_FindNgrNamedElement(ngr, "M2Nne_M1n"))) {
      SUMA_S_Err("Missing M2Nne_M1n");
      SUMA_RETURN(SUMA_FreeM2M(M2M));
   }
   memcpy(M2M->M2Nne_M1n, nel->vec[0], M2M->M1Nn*sizeof(int));
   
   /* now the ragged monsters */
   Nmax = 0;
   for (i=0; i<M2M->M1Nn; ++i) Nmax += M2M->M2Nne_M1n[i];

   if (!(nel =  SUMA_FindNgrNamedElement(ngr, "M2ne_M1n"))) {
      SUMA_S_Err("Missing M2ne_M1n");
      SUMA_RETURN(SUMA_FreeM2M(M2M));
   }
   itmp = (int *)(nel->vec[0]);
   k = 0;
   for (i=0; i<M2M->M1Nn; ++i) {
      M2M->M2ne_M1n[i] = (int *)SUMA_calloc(M2M->M2Nne_M1n[i], sizeof(int));
      for (j=0; j<M2M->M2Nne_M1n[i]; ++j) {
         M2M->M2ne_M1n[i][j] = itmp[k]; ++k;
      }
   }
   itmp = NULL;
   
   if (!(nel =  SUMA_FindNgrNamedElement(ngr, "M2we_M1n"))) {
      SUMA_S_Err("Missing M2we_M1n");
      SUMA_RETURN(SUMA_FreeM2M(M2M));
   }
   dtmp = (double *)(nel->vec[0]);
   k = 0;
   for (i=0; i<M2M->M1Nn; ++i) {
      M2M->M2we_M1n[i] = (double *)
                           SUMA_calloc(M2M->M2Nne_M1n[i], sizeof(double));
      for (j=0; j<M2M->M2Nne_M1n[i]; ++j) {
         M2M->M2we_M1n[i][j] = dtmp[k]; ++k;
      }
   }
   dtmp=NULL;
   
   SUMA_RETURN(M2M);
}

NI_group *SUMA_M2M_to_niml (SUMA_M2M_STRUCT *M2M)
{
   static char FuncName[]={"SUMA_M2M_to_niml"};
   NI_group *ngr = NULL;
   int i=0, j=0, k=0, Nmax=0;
   int *itmp=NULL;
   double *dtmp = NULL;
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!M2M) SUMA_RETURN(ngr);
   
   /* form group */
   ngr = NI_new_group_element();
   NI_rename_group(ngr,"M2M");
   
   /* add easy stuff */
   NI_SET_STR(ngr, "M1_IDcode", M2M->M1_IDcode);
   NI_SET_INT(ngr, "M1_N_Nodes", M2M->M1_N_Nodes);
   
   NI_SET_STR(ngr, "M2_IDcode", M2M->M2_IDcode);
   NI_SET_INT(ngr, "M2_N_Nodes", M2M->M2_N_Nodes);
   
   NI_SET_INT(ngr, "M1Nn", M2M->M1Nn);
   
   /* Now the 1D arrays */
   nel = NI_new_data_element("M1n", M2M->M1Nn); 
   NI_add_column_stride ( nel, NI_INT, M2M->M1n, 1);
   NI_add_to_group(ngr, nel);
   
   nel = NI_new_data_element("M2t_M1n", M2M->M1Nn); 
   NI_add_column_stride ( nel, NI_INT, M2M->M2t_M1n, 1);
   NI_add_to_group(ngr, nel);
   
   nel = NI_new_data_element("M2pb_M1n", 2*M2M->M1Nn); 
   NI_add_column_stride ( nel, NI_FLOAT, M2M->M2pb_M1n, 1);
   NI_add_to_group(ngr, nel);
   
   nel = NI_new_data_element("M2p_M1n", 3*M2M->M1Nn);
   NI_add_column_stride ( nel, NI_FLOAT, M2M->M2p_M1n, 1);
   NI_add_to_group(ngr, nel); 
   
   nel = NI_new_data_element("PD", M2M->M1Nn); 
   NI_add_column_stride ( nel, NI_DOUBLE, M2M->PD, 1);
   NI_add_to_group(ngr, nel); 
   
   nel = NI_new_data_element("M2Nne_M1n", M2M->M1Nn);
   NI_add_column_stride ( nel, NI_INT, M2M->M2Nne_M1n, 1);
   NI_add_to_group(ngr, nel); 
   
   /* now the ragged monsters */
   Nmax = 0;
   for (i=0; i<M2M->M1Nn; ++i) Nmax += M2M->M2Nne_M1n[i];

   if (!(itmp = (int *)calloc(Nmax, sizeof(int)))) {
      SUMA_S_Crit("Failed to allocate");
      NI_free_element(ngr); SUMA_RETURN(NULL); 
   }
   k = 0;
   for (i=0; i<M2M->M1Nn; ++i) {
      for (j=0; j<M2M->M2Nne_M1n[i]; ++j) {
         itmp[k] = M2M->M2ne_M1n[i][j]; ++k;
      }
   }
   nel = NI_new_data_element("M2ne_M1n", Nmax);
   NI_add_column_stride ( nel, NI_INT, itmp, 1); SUMA_free(itmp); itmp = NULL;
   NI_add_to_group(ngr, nel); 
   
   if (!(dtmp = (double *)calloc(Nmax, sizeof(double)))) {
      SUMA_S_Crit("Failed to allocate");
      NI_free_element(ngr); SUMA_RETURN(NULL); 
   }
   k = 0;
   for (i=0; i<M2M->M1Nn; ++i) {
      for (j=0; j<M2M->M2Nne_M1n[i]; ++j) {
         dtmp[k] = M2M->M2we_M1n[i][j]; ++k;
      }
   }
   nel = NI_new_data_element("M2we_M1n", Nmax);
   NI_add_column_stride ( nel, NI_DOUBLE, dtmp, 1); SUMA_free(dtmp); dtmp = NULL;
   NI_add_to_group(ngr, nel);
   
   SUMA_RETURN(ngr);
}

SUMA_M2M_STRUCT *SUMA_Load_M2M (char *fname) 
{
   static char FuncName[]={"SUMA_Load_M2M"};
   NI_stream ns = NULL;
   void *nini=NULL;
   char *niname = NULL, *fname2=NULL;
   SUMA_M2M_STRUCT *M2M = NULL;
   
   SUMA_ENTRY;
   
   if (!fname) SUMA_RETURN(M2M);
   
   fname2 = SUMA_Extension(fname, ".niml.M2M", 0);
   niname = SUMA_append_string("file:", fname2);
   SUMA_free(fname2); fname2=NULL;
   ns = NI_stream_open(niname, "r");
   if (!ns) {
      SUMA_S_Crit("Failed to open NI stream for reading.\n");
      if (niname) SUMA_free(niname); niname = NULL;
      SUMA_RETURN(M2M);
   }
   SUMA_free(niname); niname = NULL;
   
   nini = NI_read_element(ns, 1) ; 
   NI_stream_close( ns ) ; ns = NULL;
   if (NI_element_type(nini) != NI_GROUP_TYPE) {
      SUMA_S_Err("NIML not group type");
      NI_free_element(nini); SUMA_RETURN(M2M);
   }
   M2M = SUMA_niml_to_M2M((NI_group*)nini);
   NI_free_element(nini); nini = NULL;
   
   SUMA_RETURN(M2M);
}  

SUMA_Boolean SUMA_Save_M2M(char *fname, SUMA_M2M_STRUCT *M2M) 
{
   static char FuncName[]={"SUMA_Save_M2M"};
   NI_stream ns = NULL;
   char *niname = NULL, *fname2=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   if (!fname || !M2M) SUMA_RETURN(NOPE);

   fname2 = SUMA_Extension(fname, ".niml.M2M", 0);
   niname = SUMA_append_string("file:", fname2);
   SUMA_free(fname2); fname2=NULL;
   
   ns = NI_stream_open(niname, "w");
   if (!ns) {
      SUMA_S_Crit("Failed to open NI stream for writing.\n");
      if (niname) SUMA_free(niname); niname = NULL;
      SUMA_RETURN(NOPE);
   }
   SUMA_free(niname); niname = NULL;
   
   if (!(ngr=SUMA_M2M_to_niml(M2M))) {
      SUMA_S_Err("Failed to create ngr");
      NI_stream_close( ns ) ; ns = NULL;
      SUMA_RETURN(NOPE);
   }
   
   NI_write_element(ns, ngr, NI_BINARY_MODE);
   NI_stream_close( ns ) ; ns = NULL;
   NI_free_element(ngr); ngr = NULL;
    
   SUMA_RETURN(YUP);
}
/*!
   dseto = SUMA_morphDsetToStd (dset, M2M, imode);
   Function to map dsets from one mesh to another per MI
   \param dset (SUMA_DSET *) dset to morph
   \param M2M (SUMA_M2M_STRUCT *)structure containing morph information
   \param useclosest (int) 1: Nearest neighbor interpolation
                           0: Barycentric
   \ret dseto
*/
SUMA_DSET *SUMA_morphDsetToStd (SUMA_DSET *dset, SUMA_M2M_STRUCT *M2M, 
                                int useclosest)
{
   static char FuncName[]={"SUMA_morphDsetToStd"};
   SUMA_DSET *ndset=NULL;
   byte *bfull=NULL;
   int N_inmask=-1, i;
   SUMA_VARTYPE vtp = SUMA_notypeset;
   char *new_name=NULL;
   float *fin=NULL, *fout=NULL;
   char *s=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !M2M) {
      SUMA_S_Err("NULL or bad input");
      SUMA_RETURN(ndset);
   } 
   
   if (!SUMA_is_AllConsistentNumeric_dset(dset, &vtp)) {
      SUMA_S_Errv("Columns in %s not all numeric and of the same type\n",
                  SDSET_LABEL(dset));
      SUMA_RETURN(ndset);
   }
   
   /* form new dset */
   new_name = SUMA_append_string( "copy.",SDSET_FILENAME(dset));
   ndset =  SUMA_CreateDsetPointer( 
               new_name, 
               SDSET_TYPE(dset), 
               NULL, 
               SDSET_IDMDOM(dset),
               M2M->M1_N_Nodes );
   SUMA_free(new_name); new_name=NULL;
   
   if (!SUMA_AddDsetNelCol ( ndset, "Node Index", 
                             SUMA_NODE_INDEX, (void *)M2M->M1n, NULL, 1)) {
      SUMA_S_Err("Failed to add node index column");
      SUMA_FreeDset(ndset); ndset=NULL;
      SUMA_RETURN(ndset);                                 
   }
   
   SUMA_COPY_DSETWIDE_ATTRIBUTES(dset, ndset);
   
   /* do it the cautious way, one column at a time */
   for (i=0; i<SDSET_VECNUM(dset); ++i) {
      if (!(fin = SUMA_DsetCol2FloatFullSortedColumn(dset, i, &bfull, 
                                                0.0, M2M->M2_N_Nodes,
                                                &N_inmask, i==0?YUP:NOPE))){
         SUMA_S_Err("Failed to extract");
         SUMA_FreeDset(ndset); ndset=NULL;
         if (bfull) SUMA_free(bfull); bfull=NULL;
         SUMA_RETURN(ndset);
      }
      
      if (LocalHead) {
         s = SUMA_ShowMeSome(fin, SUMA_float, M2M->M2_N_Nodes, 10, NULL);
         SUMA_LHv("fin:\n%s\n", s); SUMA_free(s);
      }
      if (!(fout = SUMA_M2M_interpolate(M2M, fin,
                                  1, M2M->M2_N_Nodes,
                                  SUMA_COLUMN_MAJOR, useclosest))) {
         SUMA_S_Err("Failed to map");
         SUMA_free(fin); fin = NULL;
         if (bfull) SUMA_free(bfull); bfull=NULL;
         SUMA_FreeDset(ndset); ndset=NULL;
         SUMA_RETURN(ndset);
      }
      if (LocalHead) {
         s = SUMA_ShowMeSome(fout, SUMA_float, M2M->M1_N_Nodes, 10, NULL);
         SUMA_LHv("fout:\n%s\n", s); SUMA_free(s);
      }
      /* make place for fout */
      new_name = SUMA_DsetColLabelCopy(dset,i, 0);
      SUMA_LHv("Allocating for column %s\n", new_name);
      if (!SUMA_InsertDsetNelCol (ndset, new_name, SDSET_COLTYPE(dset,i), 
                                 NULL, NULL ,1, i)) {
         SUMA_S_Err("Failed to insert col");
         SUMA_free(fin); fin = NULL; SUMA_free(fout); fout = NULL;
         if (bfull) SUMA_free(bfull); bfull=NULL;
         SUMA_FreeDset(ndset); ndset=NULL;
         SUMA_RETURN(ndset);
      }  
      /* stick fout in output */
      SUMA_LHv("Sticking column %d in dset (fout[0]=%f, %d values expected)\n", 
               i, fout[0], SDSET_VECLEN(ndset));
               /* Do not use bfull in call below. bfull is for original array */
      if (!SUMA_Vec2DsetCol (ndset, i, (void *)fout, SUMA_float, 0, NULL)) { 
         SUMA_S_Err("Failed to store output");
         SUMA_free(fin); fin = NULL; SUMA_free(fout); fout = NULL; 
         if (bfull) SUMA_free(bfull); bfull=NULL;
         SUMA_FreeDset(ndset); ndset=NULL;
         SUMA_RETURN(ndset);
     }
     SUMA_free(fin); fin = NULL; SUMA_free(fout); fout = NULL; 
   }
   
   if (bfull) SUMA_free(bfull); bfull=NULL;
   
   SUMA_COPY_DSET_ALL_COL_ATTRIBUTES(dset, ndset);

   if (LocalHead) {
      SUMA_ShowDset(ndset, 0, NULL);
   }
   
   SUMA_RETURN(ndset);
}

/*!
   \brief A function to interpolate data from one mesh onto another
   \param M2M (SUMA_M2M_STRUCT *) 
   \param far_data (float *) Data from mesh 2. The vector in far_data can 
                             represent an nvec * ncol matrix of values stored
                             in row major or column major order. Think of each
                             column as a separate sub-brick.
   \param ncol (int) number of columns in far_data
   \param nrow (int) number of rows in far_data (this number must be equal to the number of nodes on mesh 2!)
   \param d_order (SUMA_INDEXING_ORDER) SUMA_ROW_MAJOR, i.e. xyz xyz xyz xyz
                                       SUMA_COLUMN_MAJOR, i.e. xxxx yyyy zzzz
   \param useCloset (int) 1 means use only data form the closest node
                    0 means use data from all neighbors in M2M
   \return dt (float *) interpolation of far_data from mesh 2 (M2) onto nodes of M1
                        dt is ncol*M2M->M1Nn in the same order as d_order
*/
float *SUMA_M2M_interpolate(SUMA_M2M_STRUCT *M2M, float *far_data, 
                            int ncol, int nrow, SUMA_INDEXING_ORDER d_order, 
                            int useClosest )
{
   static char FuncName[]={"SUMA_M2M_interpolate"};
   int j, k, i, nk, nkid, njid, N_k, nj;
   float *dt=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!M2M || !far_data) {
      SUMA_SL_Err("NULL input");
      SUMA_RETURN(dt);
   }
   /* allocation */
   dt = (float *)SUMA_calloc(M2M->M1Nn*ncol, sizeof(float));
   if (!dt) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(dt); }

   /* here we go */
   if (d_order == SUMA_ROW_MAJOR) {
      if (!useClosest) {
         SUMA_LH("Using all neighbors, ROW MAJOR interpolation");
         for (j=0; j<M2M->M1Nn; ++j) {
            nj = M2M->M1n[j]; /* node on M1 */
            njid = j*ncol; /* ROW MAJOR BABY */
            N_k = M2M->M2Nne_M1n[j]; 
            for (i=0; i<ncol; ++i) { /* for each column */
               dt[njid+i] = 0.0;
               for (k=0; k<N_k; ++k) { /* for each neighbor */
                  nk = M2M->M2ne_M1n[j][k]; 
                  nkid = nk * ncol; /* ROW MAJOR BABY */
                  dt[njid+i] += far_data[nkid+i] * M2M->M2we_M1n[j][k]; 
               }
            }
         }   
      } else {
         SUMA_LH("Using immediate neighbor, ROW MAJOR interpolation");
         k = 0; /* just the closest neighbor  */
         for (j=0; j<M2M->M1Nn; ++j) {
            nj = M2M->M1n[j]; /* node on M1 */
            njid = j*ncol; /* ROW MAJOR BABY */
            for (i=0; i<ncol; ++i) { /* for each column */
               dt[njid+i] = 0.0;      
               if (M2M->M2Nne_M1n[j]) { /* Some nodes have no neighbors! */
                  /* k = 0, set above, just the closest neighbor  */
                  nk = M2M->M2ne_M1n[j][k]; 
                  nkid = nk * ncol; /* ROW MAJOR BABY */
                  dt[njid+i] += far_data[nkid+i];
               }
            }
         }
      }
   } else if (d_order == SUMA_COLUMN_MAJOR) {
      if (!useClosest) {
         SUMA_LH("Using all neighbors, COLUMN MAJOR interpolation");
         for (i=0; i<ncol; ++i) { /* for each column */
            for (j=0; j<M2M->M1Nn; ++j) { /* for each node on M1 */
               nj = M2M->M1n[j]; 
               njid = j+i*M2M->M1Nn; 
                  /* index of nj's ith column entry into dt, COLUMN MAJOR BABY */
               dt[njid] = 0;
               N_k = M2M->M2Nne_M1n[j]; 
               for (k=0; k<N_k; ++k) { /* for each neighbor */
                  nk = M2M->M2ne_M1n[j][k];
                  nkid = nk + i*nrow; 
                     /* index of nj's kth neighbor's data into 
                        far_data, COLUMN MAJOR BABY */ 
                  dt[njid] += far_data[nkid]* M2M->M2we_M1n[j][k];
               }
            }
         }
      } else {
         SUMA_LH("Using immediate neighbor, COLUMN MAJOR interpolation");
         k = 0;   /* just the closest neighbor  */
         for (i=0; i<ncol; ++i) { /* for each column */
            for (j=0; j<M2M->M1Nn; ++j) { /* for each node on M1 */
               nj = M2M->M1n[j]; 
               njid = j+i*M2M->M1Nn; 
                  /* index of nj's ith column entry into dt, COLUMN MAJOR BABY */
               dt[njid] = 0;
               N_k = M2M->M2Nne_M1n[j]; 
               if (M2M->M2Nne_M1n[j]) { /* Some nodes have no neighbors! */
                  /* k = 0, set above, just the closest neighbor  */
                  nk = M2M->M2ne_M1n[j][k];
                  nkid = nk + i*nrow; 
                     /* index of nj's kth neighbor's data into 
                        far_data, COLUMN MAJOR BABY */ 
                  dt[njid] += far_data[nkid];
               }
            }
         }
      }
   } else {
      SUMA_SL_Err("Bad order option");
      SUMA_free(dt); dt = NULL; SUMA_RETURN(dt); 
   }

   SUMA_RETURN(dt); 
}
