/* ---------------------------------------------------------------
   This file is to contain functions to form 
    surface object structures that are independent 
    of GIFTI and SUMA 
    No functions defined in suma_datasets.c sould be made 
    here as this object will go in libmri.a.
    
    This is only for the most basic of functions.
    Add funky stuff to suma_datasets.c       ZSS      Feb 28 08
---------------------------------------------------------------*/

#include "suma_suma.h"

NI_group *SUMA_NewAfniSurfaceObject(void)
{
   static char FuncName[]={"SUMA_NewAfniSurfaceObject"};
   NI_group *aSO=NULL;
   NI_group *ngr=NULL;
   SUMA_ENTRY;
   
   aSO = NI_new_group_element();
   NI_rename_group(aSO, "SurfaceObject");
   
   ngr = SUMA_NewAfniSurfaceObjectTriangle();
   NI_add_to_group(aSO, ngr);
   ngr = SUMA_NewAfniSurfaceObjectPointset();
   NI_add_to_group(aSO, ngr);
   ngr = SUMA_NewAfniSurfaceObjectNormals();
   NI_add_to_group(aSO, ngr);
   SUMA_RETURN(aSO);
}

NI_group *SUMA_NewAfniSurfaceObjectTriangle(void)
{
   static char FuncName[]={"SUMA_NewAfniSurfaceObjectTriangle"};
   NI_element *nel=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "Gifti_Triangle");
   nel = NI_new_data_element("Mesh_IJK", 1);
   NI_add_to_group(ngr, nel);
   
   SUMA_RETURN(ngr);
}

NI_group *SUMA_NewAfniSurfaceObjectPointset(void)
{
   static char FuncName[]={"SUMA_NewAfniSurfaceObjectPointset"};
   NI_element *nel=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "Gifti_Pointset");
   nel = NI_new_data_element("Node_XYZ", 4251);
   NI_add_to_group(ngr, nel);
   nel = NI_new_data_element("Coord_System", 16);
   NI_add_column(nel,NI_DOUBLE,NULL);
   NI_add_to_group(ngr, nel);
   
   SUMA_RETURN(ngr);
}
NI_group *SUMA_NewAfniSurfaceObjectNormals(void)
{ 
   static char FuncName[]={"SUMA_NewAfniSurfaceObjectNormals"};
   NI_element *nel=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "Gifti_Normals");
   nel = NI_new_data_element("Node_Normals", 1);
   NI_add_to_group(ngr, nel);
   
   SUMA_RETURN(ngr);
}

#define IF_FREE(ggg) { if (ggg) SUMA_free(ggg); ggg = NULL; }


NI_group *SUMA_FreeAfniSurfaceObject(NI_group *aSO)
{
   static char FuncName[]={"SUMA_FreeAfniSurfaceObject"};
   
   SUMA_ENTRY;
   
   if (aSO) NI_free_element(aSO);
   
   SUMA_RETURN(NULL);
}

void SUMA_FindNgrNamedElementRec(NI_group *ngr, 
                                 char *elname, 
                                 NI_element **nelp)
{
   static char FuncName[]={"SUMA_FindNgrNamedElementRec"};
   NI_element *nel = NULL;
   char *rs=NULL;
   int ip;
   int LocalHead = 0;
   
   SUMA_ENTRY;
    
   if (!ngr || !elname) { 
      SUMA_S_Err("NULL input "); 
      SUMA_RETURNe; 
   }
  /* now read the elements in this group */
   for( ip=0 ; ip < ngr->part_num ; ip++ ){ 
      switch( ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            if (LocalHead > 1)  {
                     fprintf( SUMA_STDERR,
                              "%s:  Looking for %s   in group %s \n",
                              FuncName, elname, ngr->name);
            }
            SUMA_FindNgrNamedElementRec(  (NI_group *)ngr->part[ip], 
                                          elname, 
                                          nelp);
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngr->part[ip] ;
            if (LocalHead > 1)  {
               fprintf( SUMA_STDERR,
                        "%s:>%d<  Looking for %s   name=%s \n"
                        "vec_len=%d vec_filled=%d, vec_num=%d\n", 
                        FuncName, ip, elname, 
                        nel->name, nel->vec_len, nel->vec_filled, 
                        nel->vec_num );
            }
            if (!strcmp(elname, nel->name)) { 
               *nelp=nel; 
               if (LocalHead) {
                  fprintf( SUMA_STDERR,
                        "%s: Found %s in group %s\n",
                        FuncName, nel->name, ngr->name);
               }
               SUMA_RETURNe; 
            }   
            break;
         default:
            SUMA_SL_Err("Don't know what to make of this group element\n"
                        "ignoring.");
            break;
      }
   }


   SUMA_RETURNe;
}
NI_element *SUMA_FindNgrNamedElement(NI_group *ngr, char *elname)
{
   static char FuncName[]={"SUMA_FindNgrNamedElement"};
   NI_element *nel = NULL;
   char *rs=NULL;
   int ip;
   int LocalHead = 0;
   
   SUMA_ENTRY;
    
   if (!ngr || !elname) { 
      SUMA_S_Err("NULL input "); 
      SUMA_RETURN(nel); 
   }
   
   SUMA_FindNgrNamedElementRec(ngr, elname, &nel);
   if (LocalHead) {
      if (nel) {
         fprintf( SUMA_STDERR,
                  "%s: Found nel %s\n",
                  FuncName, elname);
      } else {
         fprintf( SUMA_STDERR,
                  "%s: nel %s not found\n",
                  FuncName, elname);
      }         
   }
   SUMA_RETURN(nel);
}

char *SUMA_NI_AttrOfNamedElement(NI_group *ngr, char *elname, char *attrname)
{
   static char FuncName[]={"SUMA_NI_AttrOfNamedElement"};
   NI_element *nel = NULL;
   
   SUMA_ENTRY;
   
   if (!ngr || !elname || !attrname) { 
      SUMA_S_Err("NULL input");
      fprintf(SUMA_STDERR,"%s: %p %p %p\n", FuncName, ngr, elname, attrname); 
      SUMA_RETURN(NULL); 
   }
   nel = SUMA_FindNgrNamedElement(ngr, elname);
   if (!nel) SUMA_RETURN(NULL);
   SUMA_RETURN(NI_get_attribute(nel,attrname));
}

int SUMA_NI_intAttrOfNamedElement(NI_group *ngr, char *elname, char *attrname)
{
   static char FuncName[]={"SUMA_NI_intAttrOfNamedElement"};
   NI_element *nel = NULL;
   
   SUMA_ENTRY;
   
   if (!ngr || !elname || !attrname) { 
      SUMA_S_Err("NULL input "); 
      SUMA_RETURN(0); 
   }
   nel = SUMA_FindNgrNamedElement(ngr, elname);
   if (!nel) SUMA_RETURN(0);
   SUMA_RETURN(SUMA_NI_get_int(nel,attrname));
}

double SUMA_NI_doubleAttrOfNamedElement(NI_group *ngr, char *elname, 
                                     char *attrname)
{
   static char FuncName[]={"SUMA_NI_doubleAttrOfNamedElement"};
   NI_element *nel = NULL;
   
   SUMA_ENTRY;
   
   if (!ngr || !elname || !attrname) { 
      SUMA_S_Err("NULL input "); 
      SUMA_RETURN(0); 
   }
   nel = SUMA_FindNgrNamedElement(ngr, elname);
   if (!nel) SUMA_RETURN(0);
   SUMA_RETURN(SUMA_NI_get_double(nel,attrname));
}

int SUMA_NI_get_int(NI_element *nel, char *attrname)
{
   static char FuncName[]={"SUMA_NI_get_int"};
   int n=0;
   char *s=NULL;
   
   SUMA_ENTRY;
   if (nel && attrname && (s=NI_get_attribute(nel,attrname))) {
      n = (int)strtol(s,NULL,10);
   }
   SUMA_RETURN(n);
}

double SUMA_NI_get_double(NI_element *nel, char *attrname)
{
   static char FuncName[]={"SUMA_NI_get_double"};
   double n=0;
   char *s=NULL;
   
   SUMA_ENTRY;
   if (nel && attrname && (s=NI_get_attribute(nel,attrname))) {
      n = strtod(s,NULL);
   }
   SUMA_RETURN(n);
}

void SUMA_NI_set_int(NI_element *nel, char *attrname, int n)
{
   static char FuncName[]={"SUMA_NI_set_int"};
   char sb[32]={""};
   
   SUMA_ENTRY;
   if (nel && attrname) {
      sprintf(sb,"%d",n);
      NI_set_attribute(nel, attrname, sb);
   }
   SUMA_RETURNe;
}

void SUMA_NI_set_double(NI_element *nel, char *attrname, double n)
{
   static char FuncName[]={"SUMA_NI_set_double"};
   char sb[32]={""};
   
   SUMA_ENTRY;
   if (nel && attrname) {
      sprintf(sb,"%f",n);
      NI_set_attribute(nel, attrname, sb);
   }
   SUMA_RETURNe;
}

/*! 
   \brief A function to calulate shortest distance on a graph,
   and return the path corresponding to the short distance without
   relying on the SurfaceObject structure.
   
   It uses exactly the same approach as SUMA_Dijkstra but it has been
   modified to eventually survive in the AFNI world without SUMA's defines.
   
   Perhaps at some point I should make SUMA_Dijkstra call this one 
   directly, something a la SUMA_Dijkstra_usegen . This way I would
   not have two versions of the same algorithm in the code.
   
   \param N_Node: Total number of nodes
   \param NodeList: 3*N_node vector of floats. Each triplet of values
                     contains the xyz coordinates of a node.
                     This can be NULL, as long as FirstNeighbDist is not.
   \param N_Neihbv: N_node vector of ints. N_Neighbv[n] contains the 
                     number of nodes that are connected by an edge 
                     (i.e. first order neighbors) to n
   \param FirstNeighb: N_Node x MAX_N_Neighb matrix of ints.
                     FirstNeighb[n] is a vector of at least N_Neighb[n] node 
                     indices. Those are the indices of the first order neighbors
                     of n
   \param FirstNeighbDist: N_Node x MAX_N_Neighb matrix of floats.
                     FirstNeighbDist[n] is a vector of at least N_Neighb[n] 
                     distances from node n. Say node n1 is the second neighbor of
                     n. Then n1 = FirstNeighb[n][1] and the distance between 
                     n and n1 is d1 = FirstNeighbDist[n][1]. 
                     If FirstNeighbDist is NULL, the d1 is calculated based on
                     the Euclidian distance between the XYZ triplets starting at
                     NodeList[3*n] and NodeList[3*n1].
                     Naturally, you'll need to pass one of NodeList 
                     or FirstNeighbDist. If both are not NULL, then 
                     FirstNeighbDist takes precedence.
   \param Nx: index of starting node
   \param Ny: index of ending node
   \param isNodeInMeshp: N_Node vector which can be used to restrict which nodes
                         can be considered in the path. You can pass NULL to have
                         all nodes be considered
   \param N_isNodeInMesh: Pointer to the total number of nodes that 
               make up the mesh (subset of SO)
               This parameter is passed as a pointer because as nodes in the mesh
               are visited, that number is reduced and represents when the
               function returns, the number of nodes that were
               never visited in the search. 
               Set to NULL, if isNodeInMesh is NULL
   \param Method_Number (int) selector for which algorithm to use. Choose from:
                     0 - Straight forward implementation, slow
                     1 - Variation to eliminate long searches for minimum of L,
                         much much much faster than 0, 5 times more memory.
   \param Path_length (float *)  The distance between Nx and Ny. 
                                 This value is negative if no path between 
                                 Nx and Ny was found.
   \param N_Path (int *) Number of nodes forming the Path vector

   \return Path (float) A vector of N_Path node indices forming the shortest 
                        path, from Nx (Path[0]) to Ny (Path[*N_Path - 1]). 
                        NULL is returned in case of error.
 
*/
int * SUMA_Dijkstra_generic (int N_Node, 
                     float *NodeList, int NodeDim, int dist_metric,
                     int *N_Neighbv, int **FirstNeighb, float **FirstNeighbDist,
                     int Nx, int Ny, 
                     byte *isNodeInMeshp, 
                     int *N_isNodeInMesh, int Method_Number, 
                     float *Lfinal, int *N_Path,
                     int verb)
{
   static char FuncName[] = {"SUMA_Dijkstra_generic"};
   float *L = NULL, Lmin = -1.0, DT_DIJKSTRA;
   double de=0.0, le=0.0;
   int i, iw, iv, v, w, N_Neighb, *Path = NULL, N_loc=-1, kk=0;
   SUMA_Boolean *isNodeInMesh=NULL;
   struct  timeval  start_time;
   SUMA_DIJKSTRA_PATH_CHAIN *DC = NULL, *DCi=NULL, *DCp=NULL;
   SUMA_Boolean Found = NOPE;
   /* variables for method 2 */
   int N_Lmins, *vLmins=NULL, *vLocInLmins=NULL, 
      iLmins, ReplacingNode, ReplacedNodeLocation;
   float *Lmins=NULL; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *Lfinal = -1.0;
   *N_Path = 0;
   
   if (!isNodeInMeshp) {
      if (!(isNodeInMesh = (SUMA_Boolean *)SUMA_malloc(  sizeof(SUMA_Boolean) * 
                                                         N_Node) )) {
                                                   
         SUMA_S_Err("Failed to allocate"); 
         goto CLEANUP;
      }
      memset((void*)isNodeInMesh, 1,  sizeof(SUMA_Boolean) * N_Node);
      N_isNodeInMesh = &N_loc;
      *N_isNodeInMesh = N_Node;                                          
   } else {
      isNodeInMesh = isNodeInMeshp;
   }
   
   /* make sure Both Nx and Ny exist in isNodeInMesh */
   if (  Nx < 0 || Nx >= N_Node ||
         Ny < 0 || Ny >= N_Node ) {
      fprintf (SUMA_STDERR,
               "\aError %s: Node %d (Nx) or %d (Ny) is outside range [0..%d[ \n"
               , FuncName, Nx, Ny, N_Node);
      goto CLEANUP;     
   }
   if (!isNodeInMesh[Nx]) {
      fprintf (SUMA_STDERR,
               "\aError %s: Node %d (Nx) is not in mesh.\n", FuncName, Nx);
      goto CLEANUP;
   }  
   if (!isNodeInMesh[Ny]) {
      fprintf (SUMA_STDERR,
               "\aError %s: Node %d (Ny) is not in mesh.\n", FuncName, Ny);
      goto CLEANUP;
   }

   if (!N_Neighbv || !FirstNeighb) {
      fprintf (SUMA_STDERR, 
               "Error %s: missing N_Neighb or FirstNeighb.\n", FuncName);
      goto CLEANUP;
   }
   
   if (!NodeList && !FirstNeighbDist) {
      fprintf (SUMA_STDERR, 
               "Error %s: need at least NodeList or FirstNeighbDist.\n", 
               FuncName);
      goto CLEANUP;
   }  
   if (LocalHead) {
      /* Start timer for next function */
      SUMA_etime(&start_time,0);      
   }
   
   /* allocate for chain */
   DC = (SUMA_DIJKSTRA_PATH_CHAIN *)SUMA_malloc(
                     sizeof(SUMA_DIJKSTRA_PATH_CHAIN) * N_Node);
   if (!DC) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate. \n", FuncName);
      goto CLEANUP;
   }
   
   switch (Method_Number) {
   
      case 0:  /* Method 0, Brute force */
         /* allocate for vertices labels */
         L = (float *) SUMA_calloc (N_Node, sizeof (float));
         if (!L) {
            fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
            goto CLEANUP;
         }

         /* label all vertices with very large numbers, 
         initialize path previous pointers to null */
         for (i=0; i < N_Node; ++i) {
            L[i] = LARGE_NUM;   
            DC[i].Previous = NULL;
         }
         /* label starting vertex with 0 */
         L[Nx] = 0.0;
         Lmin = 0.0;
         v = Nx;
         *Lfinal = -1.0;
         /* initialize path at Nx */
         DC[Nx].Previous = NULL;
         DC[Nx].node = Nx;
         DC[Nx].le = 0.0;
         DC[Nx].order = 0;
         *N_Path = 0;
         /* Brute force method */
         do {
            /* find v in Mesh / L(v) is minimal */
            /* this sucks up a lot of time because it is searching 
            the entire set of N_Node instead of the one that was 
            intersected only.
            This can be sped up, considerably */
            SUMA_MIN_LOC_VEC(L, N_Node, Lmin, v);   
                  /* locates and finds the minimum of L, nodes not in mesh will 
                  keep their large values and will not be picked*/
            if (!isNodeInMesh[v]) {
               if (verb) {
                  fprintf (SUMA_STDERR, 
                           "\aERROR %s: Dijkstra derailed. v = %d, Lmin = %f.\n"
                           " Try another point.", FuncName, v, Lmin);
               }
               goto CLEANUP; 
            }
            if (v == Ny) {
               if (LocalHead) fprintf (SUMA_STDERR, "%s: Done.\n", FuncName);
               *Lfinal = L[v];
               Found = YUP;
            } else {
               N_Neighb = N_Neighbv[v];
               for (i=0; i < N_Neighb; ++i) {
                  w = FirstNeighb[v][i];
                  if (isNodeInMesh[w]) {
                     iw = NodeDim*w;
                     iv = NodeDim*v;
                     if (!FirstNeighbDist) {/* calculate edge length */
                        if (dist_metric == 0) {
                           le = 0.0; de = 0.0;
                           for (kk=0; kk<NodeDim; ++kk) {
                              de = (NodeList[iw+kk] - NodeList[iv+kk]);
                              le += de*de;
                           }
                           le = sqrt ( le );
                        } else {
                           fprintf (SUMA_STDERR, 
                                 "ERROR %s: Only Euclidian distance supported\n"
                                 , FuncName);
                           goto CLEANUP; 
                        }
                     } else {
                        le = FirstNeighbDist[v][i];
                     }
                     if (L[w] > L[v] + le ) {
                        L[w] = L[v] + le;  
                        /* update the path */
                        DCp = &(DC[v]); /* previous path */
                        DC[w].Previous = (void *) DCp;
                        DC[w].le = le;
                        DC[w].node = w;
                        DC[w].order = DCp->order + 1;
                     } 
                  }
               }

               /* remove node v from isNodeInMesh and reset their distance 
                  value to a very large one, 
                  this way you do not have to reinitialize this variable. */
               isNodeInMesh[v] = NOPE;
               *N_isNodeInMesh -= 1;
               L[v] = LARGE_NUM; 
               Found = NOPE;
            }
         } while (*N_isNodeInMesh > 0 && !Found);

         if (!Found) {
            fprintf (SUMA_STDERR, 
                     "Error %s: No more nodes in mesh, "
                     "failed to reach target.\n", FuncName);
            goto CLEANUP;
         }else {
            if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: Path between Nodes %d and %d is %f.\n", 
                        FuncName, Nx, Ny, *Lfinal);
         }


         if (LocalHead) {
            /* stop timer */
            DT_DIJKSTRA = SUMA_etime(&start_time,1);
            fprintf (SUMA_STDERR, 
                     "%s: Method 1- Elapsed time in function %f seconds.\n", 
                     FuncName, DT_DIJKSTRA);
         }

         if (L) SUMA_free(L); L = NULL;
         break;

      case 1:  /********* Method 1- faster minimum searching *******************/
         if (LocalHead) {
            /* Start timer for next function */
            SUMA_etime(&start_time,0);      
         }

         /* allocate for vertices labels and minimums vectors*/
         L = (float *) SUMA_calloc (N_Node, sizeof (float));        
               /* L[i] = distance to a node i*/
         Lmins = (float *) SUMA_calloc (N_Node, sizeof (float));    
               /* Lmins = vector of minimum calculated distances to node */
         vLmins = (int *) SUMA_calloc (N_Node, sizeof (int));       
               /* vLmins[i] = index (into L) of the node having a 
                  distance Lmins[i] */
         vLocInLmins = (int *) SUMA_calloc (N_Node, sizeof (int));  
               /* vLocInLmin[j] = index (into Lmins) of a node having 
                  index j (into L) */

         if (!L || !Lmins || !vLmins || !vLocInLmins) {
            fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
            goto CLEANUP;
         }

         /* label all vertices with very large numbers and initialize 
            vLocInLmins to -1*/
         for (i=0; i < N_Node; ++i) {
            L[i] = LARGE_NUM;
            Lmins[i] = LARGE_NUM;   
            vLocInLmins[i] = -1;            
            DC[i].Previous = NULL;
         }

         /* label starting vertex with 0 */
         L[Nx] = 0.0;
         *Lfinal = -1.0;

         /* initialize values of vectors used to keep track of minimum 
            values of L and their corresponding nodes */
         Lmins[0] = 0.0;
         vLmins[0] = Nx;
         vLocInLmins[Nx] = 0;
         N_Lmins = 1;

         /* initialize path at Nx */
         DC[Nx].Previous = NULL;
         DC[Nx].node = Nx;
         DC[Nx].le = 0.0;
         DC[Nx].order = 0;
         *N_Path = 0;
         
         /* method with efficient tracking of minimum */
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: about to MIN_LOC ....N_isNodeInMesh = %d\n", 
                     FuncName, *N_isNodeInMesh);
         do {
            /* find v in Mesh / L(v) is minimal */
            SUMA_MIN_LOC_VEC(Lmins, N_Lmins, Lmin, iLmins);   
               /* locates the minimum value in Lmins vector */
            v = vLmins[iLmins];   /* get the node for this Lmin value */
            if (!isNodeInMesh[v]) {
               if (verb) {
                  fprintf (SUMA_STDERR,"\aERROR %s: \n"
                                    "Dijkstra derailed. v = %d, Lmin = %f\n."
                                    "Try another point.", FuncName, v, Lmin);
               }
               goto CLEANUP;
            }
            #ifdef LOCALDEBUG
               fprintf (SUMA_STDERR, "%s: Node v = %d.\n", FuncName, v);
            #endif
            if (v == Ny) {
               if (LocalHead) fprintf (SUMA_STDERR, "%s: Done.\n", FuncName);
               *Lfinal = L[v];
               Found = YUP;
            } else {
               N_Neighb = N_Neighbv[v];
               for (i=0; i < N_Neighb; ++i) {
                  w = FirstNeighb[v][i];
                  if (isNodeInMesh[w]) {
                     iw = NodeDim*w;
                     iv = NodeDim*v;
                     if (!FirstNeighbDist) {/* calculate edge length */
                        if (dist_metric == 0) {
                           le = 0.0; de = 0.0;
                           for (kk=0; kk<NodeDim; ++kk) {
                              de = (NodeList[iw+kk] - NodeList[iv+kk]);
                              le += de*de;
                           }
                           le = sqrt ( le );
                        } else {
                           fprintf (SUMA_STDERR, 
                                 "ERROR %s: Only Euclidian distance supported\n"
                                 , FuncName);
                           goto CLEANUP; 
                        }
                     } else {
                        le = FirstNeighbDist[v][i];
                     }
                     if (L[w] > L[v] + le ) {
                        #ifdef LOCALDEBUG
                           fprintf (SUMA_STDERR, 
                                    "%s: L[%d]=%f > L[%d] = %f + le = %f.\n", 
                                    FuncName, w, L[w], v, L[v], le);
                        #endif
                        L[w] = L[v] + le; 
                        /* update the path */
                        DCp = &(DC[v]); /* previous path */
                        DC[w].Previous = (void *) DCp;
                        DC[w].le = le;
                        DC[w].node = w;
                        DC[w].order = DCp->order + 1;
                        
                        if (vLocInLmins[w] < 0) { 
                           #ifdef LOCALDEBUG
                              fprintf (SUMA_STDERR, 
                                 "%s: adding entry for w = %d - First Hit. \n", 
                                 FuncName, w);
                           #endif
                           Lmins[N_Lmins] = L[w]; 
                                 /* add this value to Lmins vector */
                           vLmins[N_Lmins] = w; 
                                 /* store the node for this Lmins value */
                           vLocInLmins[w] = N_Lmins; 
                                 /* store where that node is represented 
                                    in Lmins */
                           ++N_Lmins;  /* increment N_Lmins */  
                        } else {
                           #ifdef LOCALDEBUG
                              fprintf (SUMA_STDERR, 
                                 "%s: modifying entry for w = %d  Second Hit.\n",                                  FuncName, w); 
                           #endif
                           Lmins[vLocInLmins[w]] = L[w]; 
                                 /* update value for Lmins */
                        }                        
                     }else {
                        #ifdef LOCALDEBUG
                           fprintf (SUMA_STDERR, 
                                    "%s: L[%d]=%f < L[%d] = %f + le = %f.\n", 
                                    FuncName, w, L[w], v, L[v], le); 
                        #endif
                     } 
                  }
               }

               /* remove node v from isNodeInMesh and reset their distance 
                  value to a very large one, 
                  this way you do not have to reinitialize this variable. */
               isNodeInMesh[v] = NOPE;
               *N_isNodeInMesh -= 1;
               L[v] = LARGE_NUM; 
               Found = NOPE;

               /* also remove the values (by swapping it with last element) 
                  for this node from Lmins */
               #ifdef LOCALDEBUG
                  {
                     int kkk;
                     fprintf (SUMA_STDERR,"Lmins\tvLmins\tvLocInLmins\n");
                     for (kkk=0; kkk < N_Lmins; ++kkk) 
                        fprintf (SUMA_STDERR,"%f\t%d\t%d\n", 
                                             Lmins[kkk], vLmins[kkk], 
                                             vLocInLmins[vLmins[kkk]] );
               
                  }
               #endif
               
               if (vLocInLmins[v] >= 0) { /* remove its entry if there is one */
                  #ifdef LOCALDEBUG
                     fprintf (SUMA_STDERR, 
                              "%s: removing node v = %d. N_Lmins = %d\n", 
                              FuncName,  v, N_Lmins);
                  #endif
                  --N_Lmins;
                  ReplacingNode = vLmins[N_Lmins];
                  ReplacedNodeLocation = vLocInLmins[v];
                  Lmins[vLocInLmins[v]] = Lmins[N_Lmins];
                  vLmins[vLocInLmins[v]] = vLmins[N_Lmins];
                  vLocInLmins[ReplacingNode] = ReplacedNodeLocation;
                  vLocInLmins[v] = -1;
                  Lmins[N_Lmins] = LARGE_NUM; 
               }
            }
         } while (*N_isNodeInMesh > 0 && !Found);

         if (!Found) {
            fprintf (SUMA_STDERR, 
                     "Error %s: No more nodes in mesh, "
                     "failed to reach target %d. NLmins = %d\n", 
                     FuncName, Ny, N_Lmins);
            goto CLEANUP;
         }else {
            if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: Path between Nodes %d and %d is %f.\n", 
                        FuncName, Nx, Ny, *Lfinal);
         }


         if (LocalHead) {
            /* stop timer */
            DT_DIJKSTRA = SUMA_etime(&start_time,1);
            fprintf (SUMA_STDERR, 
                     "%s: Method 2- Elapsed time in function %f seconds.\n", 
                     FuncName, DT_DIJKSTRA);
         }

         if (L) SUMA_free(L); L = NULL;
         if (Lmins) SUMA_free(Lmins); Lmins = NULL;
         if (vLmins) SUMA_free(vLmins); vLmins = NULL;
         if (vLocInLmins) SUMA_free(vLocInLmins); vLocInLmins = NULL;
         break;   /********** Method 1- faster minimum searching **************/
      default: 
         fprintf (SUMA_STDERR, 
                  "Error %s: No such method (%d).\n", 
                  FuncName, Method_Number);
         goto CLEANUP;
         break;
   }
   
   /* now reconstruct the path */
   *N_Path = DC[Ny].order+1;
   Path = (int *) SUMA_calloc (*N_Path, sizeof(int));
   if (!Path) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      goto CLEANUP;
   }
   
   DCi = &(DC[Ny]);
   iv = *N_Path - 1;
   Path[iv] = Ny;
   if (iv > 0) {
      do {
         --iv;
         DCp = (SUMA_DIJKSTRA_PATH_CHAIN *) DCi->Previous;
         Path[iv] = DCp->node;
         DCi = DCp;
      } while (DCi->Previous);
   }
   
   if (iv != 0) {
      fprintf (SUMA_STDERR, 
               "Error %s: iv = %d. This should not be.\n", 
               FuncName, iv);
   }  
   
   CLEANUP:
      if (L) SUMA_free(L);
      if (Lmins) SUMA_free(Lmins);
      if (vLmins)  SUMA_free(vLmins);
      if (vLocInLmins)   SUMA_free(vLocInLmins);
      if (DC) SUMA_free(DC);
      if (!isNodeInMeshp) SUMA_free(isNodeInMesh); 
   
   SUMA_RETURN (Path);
}
