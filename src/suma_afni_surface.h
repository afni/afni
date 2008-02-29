#ifndef SUMA_AFNI_SURFACE_INCLUDE
#define SUMA_AFNI_SURFACE_INCLUDE

typedef struct {
   int N_Node; /*!< Number of nodes in the SO */
   int NodeDim; /*!< Dimension of Node coordinates 3 for 3D only 3 
                     is used for now, with flat surfaces having z = 0*/
   int EmbedDim; /*!<   Embedding dimension of the surface, 
                        2 for flat surfaces 3 for ones with non zero 
                        curvature. */ 
   float *NodeList; /*!< N_Node x 3 vector containing the XYZ node coordinates. 
                        If NodeDim is 2 then the third column is all zeros
                        Prior to SUMA  1.2 this used to be a 2D matrix 
                        (a vector of vectors) */
   /* meta data (all GIFTI) */
   char *AnatomicalStructurePrimary;
   char *AnatomicalStructureSecondary;
   char *GeometricType;
   char *UniqueID;
   char *date;
   
   /* coordsys (all GIFTI) */
   char *dataspace;
   char *xformspace;
   double xform[4][4];
   byte  inxformspace;  /* (internal) 1 = coordinates are in xform space */

} AFNI_SurfaceObject_POINTSET;

typedef struct {
   int N_FaceSet; /*!< Number of polygons defining the surface  */
   int FaceSetDim; /*!< Number of sides on the polygon */
   int *FaceSetList; /*!<  N_FaceSetList x FaceSetDim vector describing 
                           the polygon set that makes up the SO.
                           Each row contains the indices (into NodeList) of 
                           the nodes that make up a polygon 
                           Prior to SUMA  1.2 this used to be a 2D matrix 
                           (a vector of vectors) */
   /* meta data (all GIFTI) */
   char *TopologicalType;
   char *UniqueID;
   char *date;
} AFNI_SurfaceObject_TRIANGLE;

typedef struct {  /* BEFORE YOU ADD ANYTHING HERE, 
                     See comment at closing brace */
  AFNI_SurfaceObject_POINTSET *ps;
  AFNI_SurfaceObject_TRIANGLE *tr; 
} AFNI_SurfaceObject; /* Keep content and order of fields identical to
                        those in the beginning of SUMA_SurfaceObject 
                        in SUMA_define.h */
AFNI_SurfaceObject *SUMA_NewAfniSurfaceObject(void);
AFNI_SurfaceObject_TRIANGLE *SUMA_NewAfniSurfaceObjectTriangle(void);
AFNI_SurfaceObject_POINTSET *SUMA_NewAfniSurfaceObjectPointset(void);
AFNI_SurfaceObject *SUMA_FreeAfniSurfaceObject(AFNI_SurfaceObject *aSO);
AFNI_SurfaceObject_POINTSET *SUMA_FreeAfniSurfaceObjectPointset(
                                    AFNI_SurfaceObject_POINTSET *ps);
AFNI_SurfaceObject_TRIANGLE *SUMA_FreeAfniSurfaceObjectTriangle(
                                    AFNI_SurfaceObject_TRIANGLE *tr);
#endif
