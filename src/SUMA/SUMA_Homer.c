#include "SUMA_suma.h"

#include "SUMA_Homer.h"
#include "SUMA_head_01.h"

#if defined SUMA_Homer_STAND_ALONE
#define STAND_ALONE 
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
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

/*!
   \brief Change the Vert structure to a SUMA NodeList vector
   \param Vert (Point3 *)
   \param sz_vect (int) total size of Vert
   \param N (int *) to contain the number of nodes in Vert
   \SUMA_RETURN NodeList (float *) 3Nx1 vector of XYZ coordinates.
*/
float * SUMA_HomerVertex(Point3 *Vert, int sz_vect, int *N)
{
   static char FuncName[]={"SUMA_HomerVertex"};
   float *NodeList=NULL;
   int i, k;
   SUMA_Boolean LocalHead = NOPE;
     
   SUMA_ENTRY;
   
   *N = sz_vect/sizeof(Point3);
   if (LocalHead) fprintf(SUMA_STDERR,"%d (%d/%d) elements in Vert.\n", 
      *N, sz_vect, (int)sizeof(Point3));
   
   NodeList = (float *)SUMA_malloc(*N*3*sizeof(float));
   k = 0;
   for (i=0; i<*N; ++i) {
      NodeList[k] = 50.0*(float)Vert[i].x; ++k;
      NodeList[k] = 50.0*(float)Vert[i].y; ++k;
      NodeList[k] = 50.0*(float)Vert[i].z; ++k;
   }
   
   SUMA_RETURN(NodeList);
}
/*!
   \brief Change the face vector to a SUMA FaceSetList vector
   Polygons are automatically triangulated
   
   \param face (long *) vector of ace indices. Faces are separated
                       by -1 entries
   \param sz_vect (int) total size of Vert
   \param N (int *) to contain the number of faces is FaceSetList
   \SUMA_RETURN FaceSetList (int *) 3Nx1 vector of triangles making up mesh.
*/

int * SUMA_HomerFace(long *face, int sz_vect, int *N)
{
   static char FuncName[]={"SUMA_HomerFace"};
   int i, k, N_alloc, iface, iface0, iFS3;
   int *FaceSetList=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *N = sz_vect/sizeof(long);
   if (LocalHead) fprintf(SUMA_STDERR,"%d (%d/%d) elements in Vert.\n", 
      *N, sz_vect, (int)sizeof(long));
   
   /* Can't guess ahead of time, make sure you check down the line */
   N_alloc = *N*3;
   FaceSetList = (int *)SUMA_malloc(N_alloc*sizeof(int));   
   if (!FaceSetList) {
      fprintf (SUMA_STDERR,"Error %s: Failed to reallocate.\n", FuncName);
      SUMA_RETURN(NULL);
   } 
   iFS3 =0; /* index of triangulated facet */
   iface = 0;
   iface0 = 0;
   while (iface < *N) {
      iface0 = iface ; /* 1s node in polygon */
      if (iface0 < 0) {
         fprintf(SUMA_STDERR, "Error %s: Unexpected end flag", FuncName);
         SUMA_free(FaceSetList); 
         SUMA_RETURN(NULL);
      }
      if (LocalHead) fprintf(SUMA_STDERR,
            "%s: iface0 = %d, face[%d] = %d: ", 
            FuncName, iface0, iface0, (int)face[iface0]) ;
      do {
         if (iFS3+3 > N_alloc) {
            N_alloc = 2 * N_alloc;
            FaceSetList = (int *)realloc((void *)FaceSetList, N_alloc * sizeof(int));
            if (!FaceSetList) {
               fprintf (SUMA_STDERR,"Error %s: Failed to reallocate.\n", FuncName);
               SUMA_RETURN(NULL);
            } 
         }
         FaceSetList[iFS3] = face[iface0]; /* first node in polygon is first node of triangles forming polygon */
         if (FaceSetList[iFS3] < 0) {
            fprintf (SUMA_STDERR,"Negative index loaded (loc 0)\n");
         }
         if (LocalHead) fprintf(SUMA_STDERR,
            "t(%d, ", (int)face[iface0]);
         if (iface == iface0) ++iface;
         if (LocalHead) fprintf(SUMA_STDERR,
            "%d, ", (int)face[iface]);
         ++iFS3;
         FaceSetList[iFS3] = face[iface]; /* node 2 */
         if (FaceSetList[iFS3] < 0) {
            fprintf (SUMA_STDERR,"Negative index loaded (loc 1)\n");
         }
         if (LocalHead) fprintf(SUMA_STDERR,
            "%d) ", (int)face[iface+1]);
         ++iFS3; 
         FaceSetList[iFS3] = face[iface+1]; /* node 3 */
         if (FaceSetList[iFS3] < 0) {
            fprintf (SUMA_STDERR,"Negative index loaded (loc 2)\n");
         }
         ++iFS3; ++iface; 
      } while (face[iface+1] >= 0);
      if (LocalHead) fprintf(SUMA_STDERR," iFS3/N_alloc = %d/%d\n", iFS3, N_alloc);
      ++iface; /* skip -1 */
      ++iface; /* goto next */
   }
   
   *N = iFS3 / 3;
   
   /* reallocate */

      if (LocalHead) {
         int tmpmin=-100, n3, itmp;
         n3 = 3 * *N;
         fprintf (SUMA_STDERR,"%s: N_FaceSet %d\n", FuncName, *N);
         SUMA_MIN_VEC (FaceSetList, n3, tmpmin);
         fprintf (SUMA_STDERR,"Minimum index is %d\n", tmpmin);
         if (tmpmin < 0) {
            fprintf (SUMA_STDERR,"Error %s: Bad ass pre-alloc negative number\n", FuncName);
            for (itmp=0; itmp<n3; ++itmp) {
               fprintf (SUMA_STDERR, "%d: %d\n", itmp, FaceSetList[itmp]);
               if (FaceSetList[itmp] < 0) {
                  fprintf (SUMA_STDERR,"%s: Min of %d, at %d\n", FuncName, FaceSetList[itmp], itmp);
               }
            } 
         }
      }

   FaceSetList = (int *)SUMA_realloc((void *)FaceSetList, iFS3 * sizeof(int));
      if (LocalHead) {
         int tmpmin=-100, n3, itmp;
         n3 = 3 * *N;
         fprintf (SUMA_STDERR,"%s: N_FaceSet %d\n", FuncName, *N);
         SUMA_MIN_VEC (FaceSetList, n3, tmpmin);
         fprintf (SUMA_STDERR,"Minimum index is %d\n", tmpmin);
         if (tmpmin < 0) {
            fprintf (SUMA_STDERR,"Error %s: Bad post realloc ass negative number\n", FuncName);
            for (itmp=0; itmp<n3; ++itmp) {
               fprintf (SUMA_STDERR, "%d: %d\n", itmp, FaceSetList[itmp]);
               if (FaceSetList[itmp] < 0) {
                  fprintf (SUMA_STDERR,"%s: Min of %d, at %d\n", FuncName, FaceSetList[itmp], itmp);
               }
            } 
         }
      }
   
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Returning (iFS3 = %d, N = %d...)\n", FuncName, iFS3, *N);
   
   SUMA_RETURN(FaceSetList); 
}

SUMA_SurfaceObject *SUMA_HJS_Surface(int ipart)
{
   static char FuncName[]={"SUMA_HJS_Surface"};
   SUMA_SurfaceObject *SO=NULL;
   int *FaceSetList=NULL, N_Node, N_FaceSet;
   float *NodeList=NULL;
   SUMA_NEW_SO_OPT *nsoopt = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   switch (ipart) {
      case 0:      
         NodeList = SUMA_HomerVertex(X1_X5_Sphere_vertex, sizeof(X1_X5_Sphere_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_Sphere_face, sizeof(X1_X5_Sphere_face), &N_FaceSet);
         break;
      case 1:
         NodeList = SUMA_HomerVertex(X1_X5_X12_lleg_vertex, sizeof(X1_X5_X12_lleg_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X12_lleg_face, sizeof(X1_X5_X12_lleg_face), &N_FaceSet);
         break;
      case 2:
         NodeList = SUMA_HomerVertex(X1_X5_X12_Rleg_vertex, sizeof(X1_X5_X12_Rleg_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X12_Rleg_face, sizeof(X1_X5_X12_Rleg_face), &N_FaceSet);
         break;
      case 3:
         NodeList = SUMA_HomerVertex(X1_X5_X12_Sphere_vertex, sizeof(X1_X5_X12_Sphere_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X12_Sphere_face, sizeof(X1_X5_X12_Sphere_face), &N_FaceSet);
         break;
      case 4:
         NodeList = SUMA_HomerVertex(X1_X5_X12_X31_Sphere_vertex, sizeof(X1_X5_X12_X31_Sphere_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X12_X31_Sphere_face, sizeof(X1_X5_X12_X31_Sphere_face), &N_FaceSet);
         break;
      case 5:
         NodeList = SUMA_HomerVertex(X1_X5_X44_X45_vertex, sizeof(X1_X5_X44_X45_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_X45_face, sizeof(X1_X5_X44_X45_face), &N_FaceSet);
         break;
      case 6:
         NodeList = SUMA_HomerVertex(X1_X5_X44_Torus_vertex, sizeof(X1_X5_X44_Torus_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_Torus_face, sizeof(X1_X5_X44_Torus_face), &N_FaceSet);
         break;
      case 7:
         NodeList = SUMA_HomerVertex(X1_X5_X44_X57_Sphere_vertex, sizeof(X1_X5_X44_X57_Sphere_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_X57_Sphere_face, sizeof(X1_X5_X44_X57_Sphere_face), &N_FaceSet);
         break;
      case 8:
         NodeList = SUMA_HomerVertex(X1_X5_X44_X88_Sphere_vertex, sizeof(X1_X5_X44_X88_Sphere_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_X88_Sphere_face, sizeof(X1_X5_X44_X88_Sphere_face), &N_FaceSet);
         break;
      case 9:
         NodeList = SUMA_HomerVertex(X1_X5_X44_X88_X95_Sphere_vertex, sizeof(X1_X5_X44_X88_X95_Sphere_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_X88_X95_Sphere_face, sizeof(X1_X5_X44_X88_X95_Sphere_face), &N_FaceSet);
         break;
      case 10:
         NodeList = SUMA_HomerVertex(X1_X5_X120_Sphere_Sphere_vertex, sizeof(X1_X5_X120_Sphere_Sphere_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_Sphere_Sphere_face, sizeof(X1_X5_X120_Sphere_Sphere_face), &N_FaceSet);
         break;
      case 11:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_Sphere_vertex, sizeof(X1_X5_X120_X127_Sphere_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_Sphere_face, sizeof(X1_X5_X120_X127_Sphere_face), &N_FaceSet);
         break;
      case 12:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X134_vertex, sizeof(X1_X5_X120_X127_X134_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X134_face, sizeof(X1_X5_X120_X127_X134_face), &N_FaceSet);
         break;
      case 13:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_Torus_vertex, sizeof(X1_X5_X120_X127_Torus_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_Torus_face, sizeof(X1_X5_X120_X127_Torus_face), &N_FaceSet);
         break;
      case 14:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X146_vertex, sizeof(X1_X5_X120_X127_X146_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X146_face, sizeof(X1_X5_X120_X127_X146_face), &N_FaceSet);
         break;
      case 15:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X152_vertex, sizeof(X1_X5_X120_X127_X152_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X152_face, sizeof(X1_X5_X120_X127_X152_face), &N_FaceSet);
         break;
      case 16:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X158_vertex, sizeof(X1_X5_X120_X127_X158_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X158_face, sizeof(X1_X5_X120_X127_X158_face), &N_FaceSet);
         break;
      case 17:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X164_Sphere_vertex, sizeof(X1_X5_X120_X127_X164_Sphere_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X164_Sphere_face, sizeof(X1_X5_X120_X127_X164_Sphere_face), &N_FaceSet);
         break;
      case 18:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X177_Torus_vertex, sizeof(X1_X5_X120_X127_X177_Torus_vertex), &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X177_Torus_face, sizeof(X1_X5_X120_X127_X177_Torus_face), &N_FaceSet);
         break;
      default:
         SUMA_SL_Err("No more parts");
         SUMA_RETURN(NULL);
         break;
   }
                     
   /* SUMA_disp_vect(NodeList, 3*N_Node); */
   /* SUMA_disp_dvect(FaceSetList, 3*N_FaceSet);  */
   if (LocalHead) {
      int tmpmin=-100, n3, itmp;
      n3 = 3 * N_FaceSet;
      fprintf (SUMA_STDERR,"%s: N_Node %d, N_FaceSet %d\n", FuncName, N_Node, N_FaceSet);
      SUMA_MIN_VEC (FaceSetList, n3, tmpmin);
      fprintf (SUMA_STDERR,"Minimum index is %d\n", tmpmin);
      if (tmpmin < 0) {
         fprintf (SUMA_STDERR,"Error %s: Bad in return ass negative number\n", FuncName);
         for (itmp=0; itmp<n3; ++itmp) {
            fprintf (SUMA_STDERR, "%d: %d\n", itmp, FaceSetList[itmp]);
            if (FaceSetList[itmp] < 0) {
               fprintf (SUMA_STDERR,"%s: Min of %d, at %d\n", FuncName, FaceSetList[itmp], itmp);
            }
         } 
      }
   }
   
   /* create a surface */
   nsoopt = SUMA_NewNewSOOpt();
   SO = SUMA_NewSO(&NodeList, N_Node, &FaceSetList, N_FaceSet, nsoopt);
   SO->normdir = -1;
   
   nsoopt=SUMA_FreeNewSOOpt(nsoopt); 
   
   SUMA_RETURN(SO);
}

SUMA_SurfaceObject *SUMA_head_01_surface(void)
{
   static char FuncName[]={"SUMA_head_01_surface"};
   int *FaceSetList=NULL;
   float *NodeList=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_NEW_SO_OPT *nsoopt = NULL;
   
   SUMA_ENTRY;
   
   /* create a surface */
   nsoopt = SUMA_NewNewSOOpt();
   NodeList = (float *)SUMA_malloc(d1_head_01_1D_coord*d2_head_01_1D_coord*sizeof(float));
   memcpy(NodeList, head_01_1D_coord, d1_head_01_1D_coord*d2_head_01_1D_coord*sizeof(float));
   FaceSetList = (int *)SUMA_malloc(d1_head_01_1D_topo*d2_head_01_1D_topo*sizeof(int));
   memcpy(FaceSetList, head_01_1D_topo, d1_head_01_1D_topo*d2_head_01_1D_topo*sizeof(int));
   
   SO = SUMA_NewSO(&NodeList, d1_head_01_1D_coord, &FaceSetList, d1_head_01_1D_topo, nsoopt);
   SO->normdir = 1;
   
   nsoopt=SUMA_FreeNewSOOpt(nsoopt); 
   
   SUMA_RETURN(SO);
}

#ifdef SUMA_Homer_STAND_ALONE

void usage_SUMA_Homer()
{
   printf ("\nUsage:  SUMA_Homer\n");
   exit (1);
}   


int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_Homer"}; 
   float *NodeList = NULL;
   int N_Node = 0, N_FaceSet = 0,
      N_parts = 0, ipart=0;
   int *FaceSetList = NULL;
   char sbuf[100], fName[100];
   SUMA_SURF_NORM SN;
   SUMA_OVERLAYS *NewColPlane=NULL;
   SUMA_SurfaceObject **SOv=NULL;
   FILE *SpecOut = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
	SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   

   
   N_parts = 19;
   SOv = (SUMA_SurfaceObject **) SUMA_malloc(N_parts * sizeof(SUMA_SurfaceObject *));
   
   SpecOut = fopen("HJS.spec", "w");
   if (!SpecOut) {
      fprintf(SUMA_STDERR,"Error %s: Failed in opening spec file.\n", FuncName);
		exit(1);
   }
   
   fprintf (SpecOut,"\tGroup = HJS\n");
   fprintf (SpecOut,"\tStateDef = Duffed\n"); 

   for (ipart = 0; ipart < N_parts; ++ipart) {
      switch (ipart) {
         case 0:      
            NodeList = SUMA_HomerVertex(X1_X5_Sphere_vertex, sizeof(X1_X5_Sphere_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_Sphere_face, sizeof(X1_X5_Sphere_face), &N_FaceSet);
            break;
         case 1:
            NodeList = SUMA_HomerVertex(X1_X5_X12_lleg_vertex, sizeof(X1_X5_X12_lleg_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X12_lleg_face, sizeof(X1_X5_X12_lleg_face), &N_FaceSet);
            break;
         case 2:
            NodeList = SUMA_HomerVertex(X1_X5_X12_Rleg_vertex, sizeof(X1_X5_X12_Rleg_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X12_Rleg_face, sizeof(X1_X5_X12_Rleg_face), &N_FaceSet);
            break;
         case 3:
            NodeList = SUMA_HomerVertex(X1_X5_X12_Sphere_vertex, sizeof(X1_X5_X12_Sphere_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X12_Sphere_face, sizeof(X1_X5_X12_Sphere_face), &N_FaceSet);
            break;
         case 4:
            NodeList = SUMA_HomerVertex(X1_X5_X12_X31_Sphere_vertex, sizeof(X1_X5_X12_X31_Sphere_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X12_X31_Sphere_face, sizeof(X1_X5_X12_X31_Sphere_face), &N_FaceSet);
            break;
         case 5:
            NodeList = SUMA_HomerVertex(X1_X5_X44_X45_vertex, sizeof(X1_X5_X44_X45_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X44_X45_face, sizeof(X1_X5_X44_X45_face), &N_FaceSet);
            break;
         case 6:
            NodeList = SUMA_HomerVertex(X1_X5_X44_Torus_vertex, sizeof(X1_X5_X44_Torus_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X44_Torus_face, sizeof(X1_X5_X44_Torus_face), &N_FaceSet);
            break;
         case 7:
            NodeList = SUMA_HomerVertex(X1_X5_X44_X57_Sphere_vertex, sizeof(X1_X5_X44_X57_Sphere_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X44_X57_Sphere_face, sizeof(X1_X5_X44_X57_Sphere_face), &N_FaceSet);
            break;
         case 8:
            NodeList = SUMA_HomerVertex(X1_X5_X44_X88_Sphere_vertex, sizeof(X1_X5_X44_X88_Sphere_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X44_X88_Sphere_face, sizeof(X1_X5_X44_X88_Sphere_face), &N_FaceSet);
            break;
         case 9:
            NodeList = SUMA_HomerVertex(X1_X5_X44_X88_X95_Sphere_vertex, sizeof(X1_X5_X44_X88_X95_Sphere_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X44_X88_X95_Sphere_face, sizeof(X1_X5_X44_X88_X95_Sphere_face), &N_FaceSet);
            break;
         case 10:
            NodeList = SUMA_HomerVertex(X1_X5_X120_Sphere_Sphere_vertex, sizeof(X1_X5_X120_Sphere_Sphere_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X120_Sphere_Sphere_face, sizeof(X1_X5_X120_Sphere_Sphere_face), &N_FaceSet);
            break;
         case 11:
            NodeList = SUMA_HomerVertex(X1_X5_X120_X127_Sphere_vertex, sizeof(X1_X5_X120_X127_Sphere_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_Sphere_face, sizeof(X1_X5_X120_X127_Sphere_face), &N_FaceSet);
            break;
         case 12:
            NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X134_vertex, sizeof(X1_X5_X120_X127_X134_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X134_face, sizeof(X1_X5_X120_X127_X134_face), &N_FaceSet);
            break;
         case 13:
            NodeList = SUMA_HomerVertex(X1_X5_X120_X127_Torus_vertex, sizeof(X1_X5_X120_X127_Torus_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_Torus_face, sizeof(X1_X5_X120_X127_Torus_face), &N_FaceSet);
            break;
         case 14:
            NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X146_vertex, sizeof(X1_X5_X120_X127_X146_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X146_face, sizeof(X1_X5_X120_X127_X146_face), &N_FaceSet);
            break;
         case 15:
            NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X152_vertex, sizeof(X1_X5_X120_X127_X152_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X152_face, sizeof(X1_X5_X120_X127_X152_face), &N_FaceSet);
            break;
         case 16:
            NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X158_vertex, sizeof(X1_X5_X120_X127_X158_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X158_face, sizeof(X1_X5_X120_X127_X158_face), &N_FaceSet);
            break;
         case 17:
            NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X164_Sphere_vertex, sizeof(X1_X5_X120_X127_X164_Sphere_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X164_Sphere_face, sizeof(X1_X5_X120_X127_X164_Sphere_face), &N_FaceSet);
            break;
         case 18:
            NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X177_Torus_vertex, sizeof(X1_X5_X120_X127_X177_Torus_vertex), &N_Node);
            FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X177_Torus_face, sizeof(X1_X5_X120_X127_X177_Torus_face), &N_FaceSet);
            break;
         default:
            SUMA_SL_Err("No more parts");
            SUMA_RETURN(-1);
            break;
      }
                     
      /* SUMA_disp_vect(NodeList, 3*N_Node); */
      /* SUMA_disp_dvect(FaceSetList, 3*N_FaceSet);  */
      if (LocalHead) {
         int tmpmin=-100, n3, itmp;
         n3 = 3 * N_FaceSet;
         fprintf (SUMA_STDERR,"%s: N_Node %d, N_FaceSet %d\n", FuncName, N_Node, N_FaceSet);
         SUMA_MIN_VEC (FaceSetList, n3, tmpmin);
         fprintf (SUMA_STDERR,"Minimum index is %d\n", tmpmin);
         if (tmpmin < 0) {
            fprintf (SUMA_STDERR,"Error %s: Bad in return ass negative number\n", FuncName);
            for (itmp=0; itmp<n3; ++itmp) {
               fprintf (SUMA_STDERR, "%d: %d\n", itmp, FaceSetList[itmp]);
               if (FaceSetList[itmp] < 0) {
                  fprintf (SUMA_STDERR,"%s: Min of %d, at %d\n", FuncName, FaceSetList[itmp], itmp);
               }
            } 
         }
      }
      
      /* Now create an SO for that thing */
      SOv[ipart] = SUMA_Alloc_SurfObject_Struct(1);
      /* calculate the curvatures */
      SOv[ipart]->NodeList = NodeList;
      SOv[ipart]->N_Node = N_Node;
      SOv[ipart]->FaceSetList = FaceSetList;
      SOv[ipart]->N_FaceSet = N_FaceSet;
      sprintf (fName, "Springfield/HomerJaySimpson_%d", ipart);
      SOv[ipart]->Group = SUMA_copy_string("HJS");
      SOv[ipart]->State = SUMA_copy_string("Duffed");
      SOv[ipart]->Name = SUMA_StripPath(fName);
      SOv[ipart]->FileType = SUMA_PLY;
      SOv[ipart]->FileFormat = SUMA_FF_NOT_SPECIFIED;
      SOv[ipart]->idcode_str = UNIQ_hashcode(fName); 
      SOv[ipart]->SUMA_VolPar_Aligned = NOPE;
      SOv[ipart]->VolPar = NULL;
      SOv[ipart]->NodeDim = 3;
      SOv[ipart]->FaceSetDim = 3;
      
      SUMA_MIN_MAX_SUM_VECMAT_COL (
         SOv[ipart]->NodeList, SOv[ipart]->N_Node, SOv[ipart]->NodeDim, 
         SOv[ipart]->MinDims, SOv[ipart]->MaxDims, SOv[ipart]->Center);

      SOv[ipart]->Center[0] /= SOv[ipart]->N_Node;
      SOv[ipart]->Center[1] /= SOv[ipart]->N_Node;
      SOv[ipart]->Center[2] /= SOv[ipart]->N_Node;

      SUMA_MIN_VEC (SOv[ipart]->MinDims, 3, SOv[ipart]->aMinDims );
      SUMA_MAX_VEC (SOv[ipart]->MaxDims, 3, SOv[ipart]->aMaxDims);
      
      /* Calculate SurfaceNormals */
      SN = SUMA_SurfNorm(SOv[ipart]->NodeList,  SOv[ipart]->N_Node, 
                  SOv[ipart]->FaceSetList, SOv[ipart]->N_FaceSet );
      SOv[ipart]->NodeNormList = SN.NodeNormList;
      SOv[ipart]->FaceNormList = SN.FaceNormList;

      /*create the structures for GL rendering */
      /*The data is being duplicated at the moment and perhaps I should just stick with the 1D stuf */
      SOv[ipart]->glar_NodeList = (GLfloat *) SOv[ipart]->NodeList; /* just copy the pointer, not the data */
      SOv[ipart]->glar_FaceSetList = (GLint *) SOv[ipart]->FaceSetList; /* just copy the pointer, not the data */
      SOv[ipart]->glar_FaceNormList = (GLfloat *) SOv[ipart]->FaceNormList; /* just copy the pointer, not the data */
      SOv[ipart]->glar_NodeNormList = (GLfloat *) SOv[ipart]->NodeNormList; /* just copy the pointer, not the data */

      /* a surface object does contribute to the rotation center of the viewer displaying it */
      SOv[ipart]->RotationWeight = SOv[ipart]->N_Node;
      SOv[ipart]->ViewCenterWeight = SOv[ipart]->N_Node;

      /* No selections yet, but make the preps */
      SOv[ipart]->ShowSelectedNode = YUP;
      SOv[ipart]->ShowSelectedFaceSet = YUP;
      SOv[ipart]->SelectedFaceSet = -1;
      SOv[ipart]->SelectedNode = -1;
      
      /* create the ball object*/
      SOv[ipart]->NodeMarker = SUMA_Alloc_SphereMarker ();
      if (SOv[ipart]->NodeMarker == NULL) {
         fprintf(SUMA_STDERR,"Error%s: Could not allocate for SOv[ipart]->NodeMarker\n", FuncName);
         SUMA_Free_Surface_Object (SOv[ipart]);
         SUMA_RETURN (1);
      }
      /* create the FaceSetMarker object */
      SOv[ipart]->FaceSetMarker = SUMA_Alloc_FaceSetMarker();
      if (SOv[ipart]->FaceSetMarker == NULL) {
         fprintf(SUMA_STDERR,"Error%s: Could not allocate for SOv[ipart]->FaceSetMarker\n", FuncName);
         SUMA_Free_Surface_Object (SOv[ipart]);
         SUMA_RETURN (1);
      }
      
      /* make it its own mapping reference */
      SOv[ipart]->LocalDomainParentID = SUMA_copy_string (SOv[ipart]->idcode_str);
      
      if (SUMA_existSO (SOv[ipart]->idcode_str, SUMAg_DOv, SUMAg_N_DOv)) {
         fprintf(SUMA_STDERR,"Error %s: Surface %d is specifed more than once, multiple copies ignored.\n",
             FuncName, ipart);
         SUMA_Free_Surface_Object (SOv[ipart]);
         SUMA_RETURN (1);
      }
      
      SUMA_LH("Doing Metrics...");
      if (!SUMA_SurfaceMetrics (SOv[ipart], "Convexity, EdgeList, MemberFace", NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         SUMA_Free_Surface_Object (SOv[ipart]);
         SUMA_RETURN (1);
      }

            {
               SUMA_DSET *dset=NULL;/* create the color plane for Convexity*/
             
             /* create an overlay plane */
               if (!(dset = (SUMA_DSET *)SUMA_GetCx(SOv[ipart]->idcode_str, SUMAg_CF->DsetList, 1))) {
                  SUMA_SL_Err("Failed to find dset!");
                  SUMA_RETURN (NOPE);
               }
               NewColPlane = SUMA_CreateOverlayPointer ("Convexity", dset, SOv[ipart]->idcode_str, NULL);
               if (!NewColPlane) {
                  fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateOverlayPointer.\n", FuncName);
                  SUMA_RETURN (NOPE);
               } 
               
               /* Add this plane to SOv[ipart]->Overlays */
               if (!SUMA_AddNewPlane (SOv[ipart], NewColPlane, NULL, -1, 0)) {
                  SUMA_SL_Crit("Failed in SUMA_AddNewPlane");
                  SUMA_FreeOverlayPointer(NewColPlane);
                  SUMA_RETURN (NOPE);
               }
               
               if (!SUMAg_CF->scm) {   
                  SUMAg_CF->scm = SUMA_Build_Color_maps();
                  if (!SUMAg_CF->scm) {
                     SUMA_SL_Err("Failed to build color maps.\n");
                     SUMA_RETURN (NOPE);
                  }
               }
               if (!SUMA_SetConvexityPlaneDefaults(SOv[ipart], SUMAg_CF->DsetList)) {
                  SUMA_SL_Err("Failed to set plane defaults."); SUMA_RETURN(NOPE);
               }

               /* colorize the plane */
               SUMA_ColorizePlane(NewColPlane);
            }

      /* all the previous stuff is nice and dandy but it takes a lot more to
         get this thing working */
      /* Write out the surfaces in PLY format and create a dummy spec file */
      if (!SUMA_Save_Surface_Object (  fName, SOv[ipart], 
                                       SUMA_PLY, SUMA_FF_NOT_SPECIFIED, NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
         exit (1);
      }

      fprintf (SpecOut,"NewSurface\n");
      fprintf (SpecOut, "\tSurfaceFormat = ASCII\n");
      fprintf (SpecOut, "\tSurfaceType = Ply\n");
      fprintf (SpecOut, "\tSurfaceName = %s\n", fName);
      fprintf (SpecOut, "\tMappingRef = SAME\n");
      fprintf (SpecOut, "\tSurfaceState = %s\n", SOv[ipart]->State);
      fprintf (SpecOut, "\tEmbedDimension = 3\n\n");

      
   }   
   if (SpecOut) fclose (SpecOut);
   SUMA_RETURN(0);
}
#endif
