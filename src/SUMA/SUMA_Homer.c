#include "SUMA_suma.h"

#include "SUMA_Homer.h"

#if defined SUMA_Homer_STAND_ALONE
#define STAND_ALONE 
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs stored in SVv */
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


#ifdef SUMA_Homer_STAND_ALONE

void usage_SUMA_Homer()
{
   printf ("\n\33[1mUsage: \33[0m SUMA_Homer\n");
   exit (1);
}   

float * SUMA_HomerVertex(Point3 *Vert, int sz_vect, int *N)
{
   static char FuncName[]={"SUMA_HomerVertex"};
   float *NodeList=NULL;
   int i, k;
   
   *N = sz_vect/sizeof(Point3);
   fprintf(stderr,"%d (%d/%d) elements in Vert.\n", 
      *N, sz_vect, sizeof(Point3));
   
   NodeList = (float *)malloc(*N*3*sizeof(float));
   k = 0;
   for (i=0; i<*N; ++i) {
      NodeList[k] = (float)Vert[i].x; ++k;
      NodeList[k] = (float)Vert[i].y; ++k;
      NodeList[k] = (float)Vert[i].z; ++k;
   }
   
   return(NodeList);
}

int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_Homer"}; 
   float *NodeList = NULL;
   int N_NodeList = 0;
   
   NodeList = SUMA_HomerVertex(X1_X5_Sphere_vertex, sizeof(X1_X5_Sphere_vertex), &N_NodeList);
   
   SUMA_disp_vect(NodeList, 3*N_NodeList);
   return(0);
}
#endif
