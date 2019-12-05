/* a file that gets inserted into GTS's surface.c
No bells and whistles, need to compile in GTS library 
ANY MODIFICATIONS HERE REQUIRE YOU REMAKE libgts.a*/

/* Functions fail if used directly in SUMA's code (at least on Mac OSX), 
inserting them via:
#include "../../SUMA_gts_insert.c" 
gts's surface.c works just fine
*/
#include <stdlib.h>
#include "gts.h"
static int debug;

void gts_set_debug_suma(int val) { debug = val; return;}
int  gts_get_debug_suma(void) { return(debug); }

/* functions gts_surface_suma, vertex_load, and face_load 
   are based on 
            gts_surface_write_vtk, write_vertex_vtk, write_face_vtk,
   respectively */

static void vertex_load (GtsPoint * p, gpointer * data)
{
  float *NodeList = (float *)data[0];
  int *n = (int *)data[1];
  NodeList[*n*3] = (float) p->x;
  NodeList[*n*3+1] = (float) p->y;
  NodeList[*n*3+2] = (float) p->z;
  if (debug) {
     fprintf (stderr, "Node %d: %g %g %g\n", 
      *n, (double)NodeList[*n*3], 
          (double)NodeList[*n*3+1] , 
          (double)NodeList[*n*3+2]); 
  }
  GTS_OBJECT (p)->reserved = GUINT_TO_POINTER ((*((guint *) data[1]))++);
  *n++; /* does not work, but trick above does */
}
static void face_load (GtsTriangle * t, gpointer * data)
{
  int *FaceSetList = (int *)data[0];
  int *n = (int *)data[1];
  GtsVertex * v1, * v2, * v3;
  gts_triangle_vertices (t, &v1, &v2, &v3);
  FaceSetList[*n*3] = (int)GPOINTER_TO_UINT (GTS_OBJECT (v1)->reserved);
  FaceSetList[*n*3+1] =(int) GPOINTER_TO_UINT (GTS_OBJECT (v2)->reserved);
  FaceSetList[*n*3+2] = (int)GPOINTER_TO_UINT (GTS_OBJECT (v3)->reserved);
  if (debug) {
    fprintf (stderr, "Triangle %d: %d %d %d\n", 
               *n, FaceSetList[*n*3], 
                   FaceSetList[*n*3+1], 
                   FaceSetList[*n*3+2]); 
  }
  GTS_OBJECT (t)->reserved = GUINT_TO_POINTER ((*((guint *) data[1]))++);
  *n++; /* does not work, but trick above does */
}


void gts_surface_suma (GtsSurface * s, 
                        float **NodeListp, int *N_Nodep, int *NodeDimp, 
                        int **FaceSetListp, int *N_FaceSetp, int *FaceSetDimp)
{
  guint n = 0;
  gpointer data[2];
  GtsSurfaceStats stats;
  float *NodeList = NULL;
  int *FaceSetList = NULL;
  
  g_return_if_fail (s != NULL);

  

  gts_surface_stats (s, &stats);
  
  /* get the stats  */
  if (debug) {
   fprintf (stderr,
	   "gts_surface_suma: Number of vertices %u\n",
	   stats.edges_per_vertex.n);
   fprintf (stderr,
	   "gts_surface_suma: Number of triangles %u\n",
	   stats.n_faces);
  }
  NodeList = (float *)calloc( stats.edges_per_vertex.n * 3, sizeof(float));
  FaceSetList = (int *)calloc(stats.n_faces * 3, sizeof(int)); 
  
  if (!NodeList || !FaceSetList) { 
   fprintf(stderr,"Critical Error gts_surface_suma: Could not allocate.\n");
   g_return_if_fail (0);
  }
  
  /* get the nodes */
  n = 0;
  data[0] = (gpointer)NodeList;
  data[1] = (gpointer)&n;
  gts_surface_foreach_vertex (s, (GtsFunc) vertex_load, data);
  
  /* get the facesets */
  n = 0;
  data[0] = (gpointer)FaceSetList;
  data[1] = (gpointer)&n; 
  gts_surface_foreach_face (s, (GtsFunc) face_load , data);
  
  /* don't know what these two are for, 
  assuming it has to do with the ->reserved business in vertex_load and face_load above */
  gts_surface_foreach_vertex (s, (GtsFunc) gts_object_reset_reserved, NULL);  
  gts_surface_foreach_face (s, (GtsFunc) gts_object_reset_reserved, NULL);  
  
  /* set results */
  *N_FaceSetp = (int)stats.n_faces;
  *N_Nodep = (int)stats.edges_per_vertex.n;
  *NodeListp = NodeList;
  *FaceSetListp = FaceSetList;
  *NodeDimp = 3;
  *FaceSetDimp = 3;
  return;
}
