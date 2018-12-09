#include "SUMA_suma.h"

#include "SUMA_Homer.h"
#include "SUMA_head_01Define.c"
#include "SUMA_HomerDefine.c"

int is_END_vert (Point3 Vert) {
   if ( SUMA_ABS(Vert.x-11111.11111f)<0.01 &&
        SUMA_ABS(Vert.y-22222.22222f)<0.01 &&
        SUMA_ABS(Vert.z-33333.33333f)<0.01) return(1);
   return(0);
}
/*!
   \brief Change the Vert structure to a SUMA NodeList vector
   \param Vert (Point3 *)
   \param N (int *) to contain the number of nodes in Vert
   \SUMA_RETURN NodeList (float *) 3Nx1 vector of XYZ coordinates.
*/
float * SUMA_HomerVertex(Point3 *Vert, int *N)
{
   static char FuncName[]={"SUMA_HomerVertex"};
   float *NodeList=NULL;
   int i, k;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   *N = 0;
   while (!is_END_vert(Vert[*N])) ++*N;

   if (LocalHead) fprintf(SUMA_STDERR,"%d  elements in Vert.\n",
      *N);

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
   \param N (int *) to contain the number of faces is FaceSetList
   \SUMA_RETURN FaceSetList (int *) 3Nx1 vector of triangles making up mesh.
*/

int * SUMA_HomerFace(long *face, int *N)
{
   static char FuncName[]={"SUMA_HomerFace"};
   int i, k, N_alloc, iface, iface0, iFS3;
   int *FaceSetList=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   *N = 0;
   while (face[*N] > -2) ++*N;

   if (LocalHead) fprintf(SUMA_STDERR,"%d elements in Vert.\n",
      *N);

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


   if (LocalHead)
      fprintf(SUMA_STDERR,"%s: Returning (iFS3 = %d, N = %d...)\n",
                  FuncName, iFS3, *N);

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
         NodeList = SUMA_HomerVertex(X1_X5_Sphere_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_Sphere_face, &N_FaceSet);
         break;
      case 1:
         NodeList = SUMA_HomerVertex(X1_X5_X12_lleg_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X12_lleg_face, &N_FaceSet);
         break;
      case 2:
         NodeList = SUMA_HomerVertex(X1_X5_X12_Rleg_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X12_Rleg_face, &N_FaceSet);
         break;
      case 3:
         NodeList = SUMA_HomerVertex(X1_X5_X12_Sphere_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X12_Sphere_face, &N_FaceSet);
         break;
      case 4:
         NodeList = SUMA_HomerVertex(X1_X5_X12_X31_Sphere_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X12_X31_Sphere_face, &N_FaceSet);
         break;
      case 5:
         NodeList = SUMA_HomerVertex(X1_X5_X44_X45_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_X45_face, &N_FaceSet);
         break;
      case 6:
         NodeList = SUMA_HomerVertex(X1_X5_X44_Torus_vertex,  &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_Torus_face,  &N_FaceSet);
         break;
      case 7:
         NodeList = SUMA_HomerVertex(X1_X5_X44_X57_Sphere_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_X57_Sphere_face, &N_FaceSet);
         break;
      case 8:
         NodeList = SUMA_HomerVertex(X1_X5_X44_X88_Sphere_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_X88_Sphere_face, &N_FaceSet);
         break;
      case 9:
         NodeList = SUMA_HomerVertex(X1_X5_X44_X88_X95_Sphere_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X44_X88_X95_Sphere_face, &N_FaceSet);
         break;
      case 10:
         NodeList = SUMA_HomerVertex(X1_X5_X120_Sphere_Sphere_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_Sphere_Sphere_face, &N_FaceSet);
         break;
      case 11:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_Sphere_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_Sphere_face, &N_FaceSet);
         break;
      case 12:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X134_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X134_face, &N_FaceSet);
         break;
      case 13:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_Torus_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_Torus_face, &N_FaceSet);
         break;
      case 14:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X146_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X146_face, &N_FaceSet);
         break;
      case 15:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X152_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X152_face, &N_FaceSet);
         break;
      case 16:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X158_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X158_face, &N_FaceSet);
         break;
      case 17:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X164_Sphere_vertex,
                                       &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X164_Sphere_face,
                                       &N_FaceSet);
         break;
      case 18:
         NodeList = SUMA_HomerVertex(X1_X5_X120_X127_X177_Torus_vertex, &N_Node);
         FaceSetList = SUMA_HomerFace(X1_X5_X120_X127_X177_Torus_face,
                                       &N_FaceSet);
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
      fprintf (SUMA_STDERR,"%s: part %d, N_Node %d, N_FaceSet %d\n",
                                 FuncName, ipart, N_Node, N_FaceSet);
      SUMA_MIN_VEC (FaceSetList, n3, tmpmin);
      fprintf (SUMA_STDERR,"Minimum index is %d\n", tmpmin);
      if (tmpmin < 0) {
         fprintf (SUMA_STDERR,
               "Error %s: Bad in return ass negative number\n", FuncName);
         for (itmp=0; itmp<n3; ++itmp) {
            fprintf (SUMA_STDERR, "%d: %d\n", itmp, FaceSetList[itmp]);
            if (FaceSetList[itmp] < 0) {
               fprintf (SUMA_STDERR,
                  "%s: Min of %d, at %d\n", FuncName, FaceSetList[itmp], itmp);
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

