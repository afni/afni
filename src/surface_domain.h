#ifndef _SURFACE_DOMAIN_HEADER_
#define _SURFACE_DOMAIN_HEADER_

#define AFNI_SURFACE_DOMAIN_TYPE 53001

/*---------------------------------------------------------------*/
/* Struct to hold a surface domain: node, triangles, etc. */

typedef struct {
   int  type ;             /*!< Integer code indicating type of struct.
                                Should always be AFNI_SURFACE_DOMAIN_TYPE. */

   char idcode[32];        /*!< String containing the idcode of the surface. */
   char idcode_volpar[32]; /*!< String containing the idcode of the dataset
                                to which this surface is attachable. */

   char label[32];         /*!< String containing a label for the surface.
                                Used for window titles and saved image names.
                                May be NULL. */

   int N_Node;             /*!< Number of nodes in the surface object */

   int *NodeId;            /*!< N_Node x 1 vector containing the nodes'
                                indices or IDs.
                              * If not NULL, then the [ X Y Z ] coordinates for
                                a node with ID n are:
                         [ NodeList[3*k]  NodeList[3*k+1]  NodeList[3*k+2] ],
                                where k is such that NodeId[k] = n.
                              * If NULL, then we assume that NodeId[k] = k.
                                This means that the [ X Y Z ] coordinates for
                                node n are:
                         [ NodeList[3*n]  NodeList[3*n+1]  NodeList[3*n+2] ]. */

   int NodeId_sorted ;     /*! Flag indicating if the NodeID array has been
                               sorted into increasing order.  If it is sorted,
                               then NodeList will have been sorted similarly. */

   float *NodeList;        /*!< (3*N_Node) x 1 vector containing the
                                XYZ node coordinates. */

   int N_FaceSet;          /*!< Number of triangles defining the surface  */

   int *FaceSetList;       /*!< (3*N_FaceSetList) x 1 vector specifying the
                                triangles constituting the surface mesh.
                                The triplet
                      [ FaceSetList[3*k] FaceSetList[3*k+1] FaceSetList[3*k+2] ]
                                indicates the node indices forming the kth
                                triangle in the mesh. */

   float *NodeNormList ; /*!< (3*N_Node) x 1 vector containing
                              unit normal vectors for each node in
                              NodeList.  May be NULL. */

   float *FaceNormList ; /*!< (3*N_FaceSet) x 1 vector containing
                              unit normal vectors for each triangle
                              in FaceSetList.  May be NULL. */

} AFNI_Surface_Domain ;  /* aka SUDOM */

/*-----------------------------------------------------------------*/
/*! Macro to test if a pointer to an AFNI_Surface_Domain is valid. */

#define ISVALID_SUDOM(p)                                   \
  ( (p) != NULL && (p)->type == AFNI_SURFACE_DOMAIN_TYPE )

/*----------------------------------------------*/
/*! Macro to return 'k' such that NodeId[k] = n */

#define SUDOM_NODE_ID(sd,n)                                       \
  ( ((sd)->NodeId == NULL) ? (n) : SUDOM_find_node_id((sd),(n)) )

/*------------------------------------------------------------------*/
/* Prototypes for utility functions on AFNI_Surface_Domain structs. */

extern int SUDOM_find_node_id( AFNI_Surface_Domain *sd , int n ) ;

extern void SUDOM_sort_node_id( AFNI_Surface_Domain *sd ) ;

#endif /* _SURFACE_DOMAIN_HEADER_ */
