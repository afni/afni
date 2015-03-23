#ifndef SUMA_SURFCLUST_INCLUDED
#define SUMA_SURFCLUST_INCLUDED

/* structures to be used by surface clusters functions */

typedef struct {
   int N_Node; /*!< number of nodes in this cluster */
   int *NodeList; /*!< list of node (indices) in this cluster. Allocated space
                        is always for more than N_Node elements. But 
                        values beyond NodeList[N_Node-1] are of no meaning. */
   float *ValueList; /*!< list of corresponding node values. */
   float totalarea;
   float totalvalue;
   float totalabsvalue;
   float minvalue;
   float maxvalue;
   float minabsvalue;
   float maxabsvalue;
   float varvalue;
   int minnode;
   int maxnode;
   int minabsnode;
   int maxabsnode;
   int centralnode;
   int weightedcentralnode;
   float com[3]; /*center of mass * N_Node*/
   float cen[3]; /*centroid * N_Node */
   /*int rank;*//* completely useless ... rank is inferred from order in list! */
} SUMA_CLUST_DATUM;

                           
#endif
