#ifndef SUMA_AFNI_SURFACE_INCLUDE
#define SUMA_AFNI_SURFACE_INCLUDE

#define NI_SETA_INT(ngr, name, val)  {\
   char m_stmp[100]; sprintf(m_stmp,"%d", val);   \
   NI_set_attribute(ngr, name, m_stmp);  \
}
#define NI_GETA_INT(ngr, name, val)  {\
   char *m_s = NI_get_attribute(ngr, name);  \
   if (m_s) { val = atoi(m_s); } else { val = 0; }\
}
#define NI_SETA_FLOAT(ngr, name, val)  {\
   char m_stmp[100]; sprintf(m_stmp,"%f", val);   \
   NI_set_attribute(ngr, name, m_stmp);  \
}
#define NI_GETA_FLOAT(ngr, name, val)  {\
   char *m_s = NI_get_attribute(ngr, name);  \
   if (m_s) { val = atof(m_s); } else { val = 0.0; }\
}


NI_group *SUMA_NewAfniSurfaceObject(void);
NI_group *SUMA_NewAfniSurfaceObjectTriangle(void);
NI_group *SUMA_NewAfniSurfaceObjectPointset(void);
NI_group *SUMA_NewAfniSurfaceObjectNormals(void);
NI_group *SUMA_FreeAfniSurfaceObject(NI_group *aSO);
NI_element *SUMA_FindNgrNamedElement(NI_group *ngr, char *elname);
int SUMA_NI_get_int(NI_element *nel, char *attrname);
double SUMA_NI_get_double(NI_element *nel, char *attrname);
void SUMA_NI_set_int(NI_element *nel, char *attrname, int n);
void SUMA_NI_set_double(NI_element *nel, char *attrname, double n);
char *SUMA_NI_AttrOfNamedElement(NI_group *ngr, char *elname, char *attrname);
int SUMA_NI_intAttrOfNamedElement(NI_group *ngr, char *elname, char *attrname);
double SUMA_NI_doubleAttrOfNamedElement(NI_group *ngr, char *elname, 
                                       char *attrname);

/*! Structure to contain the path between one node and the next. The path is defined in terms of the previous one, plus an edge from
the previous to the current */
#define LARGE_NUM 9e300
typedef struct {
   int node; /*!< Index of current node*/ 
   float le;   /*!< Length of edge between current node 
                     and previous one. 0 for starting node. */ 
   int order; /*!< Path order to node. A path order of i means i segments 
                   are needed to reach node from the starting node. 
                   0 for starting node*/
   void *Previous; /*!< pointer to path leading up to the previous node. 
                        NULL for starting node. This pointer is to be typecast 
                        to SUMA_DIJKSTRA_PATH_CHAIN **/
} SUMA_DIJKSTRA_PATH_CHAIN;
int * SUMA_Dijkstra_generic (int N_Node, 
                     float *NodeList, int NodeDim, int dist_metric,
                     int *N_Neighbv, int **FirstNeighb, float **FirstNeighbDist,
                     int Nx, int Ny, 
                     byte *isNodeInMeshp, 
                     int *N_isNodeInMesh, int Method_Number, 
                     float *Lfinal, int *N_Path,
                     int verb);


#endif
