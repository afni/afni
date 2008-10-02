#include "SUMA_suma.h"

#undef STAND_ALONE

#if defined SUMA_SurfSmooth_STAND_ALONE
#define STAND_ALONE
#elif defined SUMA_SurfQual_STANDALONE
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
   \brief Calculate the sine and cosines of angles in a triangle 
   return -2 where calculation fails
*/
SUMA_Boolean SUMA_TriTrig( float *p1, float *p2, float *p3, 
                           double *s, double *c, double *a)
{
   static char FuncName[]={"SUMA_TriTrig"};
   double U13[3], U12[3], U23[3], U21[3], X[3];
   double Xn, Un13, Un12, Un23, Up1, Up2, Up3;
   int k;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!p1 || !p2 || !p3 || !s || !c) SUMA_RETURN(NOPE);
   
#if 0
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "n1=[%f, %f, %f];\n"
                           "n2=[%f, %f, %f];\n"
                           "n3=[%f, %f, %f];\n",
                           FuncName, 
                           p1[0], p1[1], p1[2],
                           p2[0], p2[1], p2[2],
                           p3[0], p3[1], p3[2]);
   }
#endif
   /* vectors and their norms */
   Un12 = Un13 = Un23 = 0.0f;
   for (k=0;k<3;++k) {
      U12[k] = p2[k] - p1[k]; Un12 += (U12[k]*U12[k]);
      U21[k] = p1[k] - p2[k];
      U13[k] = p3[k] - p1[k]; Un13 += (U13[k]*U13[k]);
      U23[k] = p3[k] - p2[k]; Un23 += (U23[k]*U23[k]);
   }
   
#if 0
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "U12=[%f, %f, %f]; Un12^2=%f;\n"
                           "U13=[%f, %f, %f]; Un13^2=%f;\n"
                           "U23=[%f, %f, %f]; Un23^2=%f;\n",
                           FuncName, 
                           U12[0], U12[1], U12[2], Un12,
                           U13[0], U13[1], U13[2], Un13,
                           U23[0], U23[1], U23[2], Un23);
   }
#endif   
   Up1 = Un12*Un13;
   Up2 = Un12*Un23;
   Up3 = Un13*Un23;
   
   if (Up1 > 0.0f) {
      /* sine of angle at n1 */
      SUMA_MT_CROSS(X, U12, U13);
      Xn = X[0]*X[0] +  X[1]*X[1] + X[2]*X[2];
      s[0] = sqrtf(Xn/Up1);
      /* now cosine */
      c[0] = SUMA_MT_DOT(U12,U13)/(sqrtf(Up1));
   } else {
      s[0] = -2.0;
      c[0] = -2.0;
   }
   if (Up2 > 0.0f) {
      /* sine of angle at n2 */
      SUMA_MT_CROSS(X, U23, U21);
      Xn = X[0]*X[0] +  X[1]*X[1] + X[2]*X[2];
      s[1] = sqrtf(Xn/Up2);
      /* now cosine */
      c[1] = SUMA_MT_DOT(U23,U21)/(sqrtf(Up2));
   } else {
      s[1] = -2.0;
      c[1] = -2.0;
   }
   if (Up3 > 0.0f) {
      /* sine of angle at n3 */
      SUMA_MT_CROSS(X, U13, U23);
      Xn = X[0]*X[0] +  X[1]*X[1] + X[2]*X[2];
      s[2] = sqrtf(Xn/Up3);
      /* now cosine */
      c[2] = SUMA_MT_DOT(U13,U23)/(sqrtf(Up3));
   } else {
      s[2] = -2.0;
      c[2] = -2.0;
   }

   /* now angles */
   if (a) {
      for (k=0; k<3; ++k) {
         if (s[k] >= 0.0f) { /* always the case, unless you have a reference
                                direction to compare with cross product. 
                                Unsigned angles only then.*/
            a[k] = acos(c[k]);   /* You could do 180-a1-a2 but that goes awry
                                    is calculations fail on a1 or a2... */
         } else { a[k] = -2.0; }
      }
   }
#if 0
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "s =[%f, %f, %f]; \n"
                           "c =[%f, %f, %f]; \n"
                           ,FuncName, 
                           s[0], s[1], s[2], 
                           c[0], c[1], c[2]);
   }
#endif
   SUMA_RETURN(YUP);
}
/*!

   \brief A function to calculate the geodesic distance of nodes connected to node n
          See labbook NIH-3 pp 138 and on for notes on algorithm 
   \param n (int) index of center node
   \param SO (SUMA_SurfaceObject *) structure containing surface object
   \param off (float *) a vector such that off[i] = the geodesic distance of node i
                        to node n. The vector should be initialized to -1.0
   \param lim (float) maximum geodesic distance to travel
   
   - This function is too slow. See SUMA_getoffsets2
   \sa SUMA_getoffsets2
*/
#define DBG 1
#define DoCheck 1

#if 0 /* now set in default arguments */
int SUMA_GEOMCOMP_NI_MODE = NI_BINARY_MODE;
#endif

void SUMA_Set_SurfSmooth_NodeDebug(int n)
{
   SUMA_SSidbg = n;
}


/*!
   \brief function to subdivide triangles to meet a maxarea criterion
   Divisions are done by adding a node at the centroid of the triangle 
   to be subdivided. Bad idea, for very large triangles, such as produced
   by convex hull, you could end up with nodes that have hundreds of neighbors...
*/
int SUMA_Subdivide_Mesh(   float **NodeListp, int *N_Nodep, int **FaceSetListp, 
                           int *N_FaceSetp, float maxarea)
{
   static char FuncName[]={"SUMA_Subdivide_Mesh"};
   int in, it, N_NodeAlloc, N_FaceSetAlloc, N_Node, 
         N_FaceSet, it3, in0, in1, in2, inc3, inc, itn, itn3;
   float c[3];
   float *NodeList = NULL, a, *n1, *n2, *n0;
   int *FaceSetList = NULL;
   SUMA_SurfaceObject SObuf, *SO=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   SO = &SObuf;
   
   N_NodeAlloc = N_Node = *N_Nodep;
   N_FaceSetAlloc = N_FaceSet = *N_FaceSetp;
   NodeList = *NodeListp;
   FaceSetList = *FaceSetListp;
   SO->NodeList = NodeList; SO->FaceSetList = FaceSetList;
   if (!NodeList || !FaceSetList) { 
      SUMA_SL_Err("NULL input"); SUMA_RETURN(NOPE); }
   
   it = 0; /* triangle index */
   while (it < N_FaceSet) {
      it3 = 3*it; 
      in0 = FaceSetList[it3]; in1 = FaceSetList[it3+1]; in2 = FaceSetList[it3+2]; /* node indices */
      n0 = &(NodeList[3*in0]); n1 = &(NodeList[3*in1]); n2 = &(NodeList[3*in2]);   /* node coordinates */
      SUMA_TRI_AREA(n0, n1, n2, a); /* area of triangle */
      if (a > maxarea) {
         if (N_NodeAlloc <= N_Node) { /* need to realloc ?*/
            N_NodeAlloc += 20000;
            NodeList = (float *)SUMA_realloc(NodeList, N_NodeAlloc * 3 * sizeof(float));
            /* you always add 2 triangles per new node here */
            N_FaceSetAlloc += 40000;
            FaceSetList = (int *)SUMA_realloc(FaceSetList, N_FaceSetAlloc * 3 * sizeof(int));
            if (!NodeList || !FaceSetList) { SUMA_SL_Crit("Failed to realloc"); SUMA_RETURN(NOPE); }
            SO->NodeList = NodeList; SO->FaceSetList = FaceSetList;
         }
         SUMA_FACE_CENTROID(SO, it, c); /* c is the centroid of triangle it */
         inc = N_Node; inc3 = inc*3;  ++N_Node; /* index of new centroid node */
         NodeList[inc3] = c[0]; NodeList[inc3+1] = c[1]; NodeList[inc3+2] = c[2];   /* add new centroid to bottom of list */
         FaceSetList[it3+2] = inc; /* old triangle is now 1st new triangle in0 in1 inc */
         itn = N_FaceSet; itn3 = 3 * itn; ++N_FaceSet; /* index of new second triangle */ 
         FaceSetList[itn3] = inc; FaceSetList[itn3+1] = in1; FaceSetList[itn3+2] = in2;
         itn = N_FaceSet; itn3 = 3 * itn; ++N_FaceSet; /* index of new third triangle */ 
         FaceSetList[itn3] = inc; FaceSetList[itn3+1] = in2; FaceSetList[itn3+2] = in0; 
      } else {
         ++it;
      }
   }
   
   /* reallocate */
   FaceSetList = (int *)SUMA_realloc(FaceSetList, N_FaceSet * 3 * sizeof(int));
   NodeList = (float *)SUMA_realloc(NodeList, N_Node * 3 * sizeof(float));
   
   *NodeListp = NodeList;
   *FaceSetListp = FaceSetList;
   *N_FaceSetp = N_FaceSet;
   *N_Nodep = N_Node;
   
   SUMA_RETURN(YUP);
}


/*!
   \brief Function to allocate and initialize a SUMA_VTI *  structure
   \param N_TriIndex (int): Number of triangles whose intersections will be sought
   \param **TriIndex (int **): Pointer to vector containing indices of triangles 
                               whose intersections will be sought. This vector will
                               essentially be stored in vti if you supply it and its
                               pointer is set to NULL so that you can't free it afterwards.
                               If this parameter is NULL, then an empty vti->TriIndex is 
                               created.
   \return vti (SUMA_VTI *): Initialized structure containing allocated
                              TriIndex, N_IntersectedVoxels, IntersectedVoxels vectors.
   - Free with SUMA_FreeVTI
*/
SUMA_VTI *SUMA_CreateVTI(int N_TriIndex, int *TriIndex)
{
   static char FuncName[]={"SUMA_CreateVTI"};
   SUMA_VTI *vti = NULL;
   
   SUMA_ENTRY;
   if (!N_TriIndex) {
      SUMA_SL_Err("Nothing to do !");
      SUMA_RETURN(vti);
   }
   
   vti = (SUMA_VTI *)SUMA_malloc(sizeof(SUMA_VTI));
   vti->N_TriIndex = N_TriIndex;
   if (TriIndex ) {
      vti->TriIndex = TriIndex;
   }else {
      /* create empty copy */
      vti->TriIndex = (int *)SUMA_calloc(N_TriIndex, sizeof(int));
      if (!vti->TriIndex) {
         SUMA_SL_Crit("Failed to allocate for vti->TriIndex");
         SUMA_RETURN(NULL);
      }
   }
   vti->N_IntersectedVoxels = (int *)SUMA_calloc(N_TriIndex, sizeof(int));
   vti->IntersectedVoxels = (int **)SUMA_calloc(N_TriIndex, sizeof(int*));
   if (!vti->N_IntersectedVoxels || !vti->IntersectedVoxels) {
         SUMA_SL_Crit("Failed to allocate for vti's innerds");
         SUMA_RETURN(NULL);
   }

   SUMA_RETURN(vti);
}

SUMA_VTI * SUMA_FreeVTI(SUMA_VTI *vti)
{
   static char FuncName[]={"SUMA_FreeVTI"};
   int i;
   
   SUMA_ENTRY;
   
   if (!vti) SUMA_RETURN(NULL);
   if (vti->TriIndex) SUMA_free(vti->TriIndex);
   if (vti->IntersectedVoxels) {
      for (i=0; i<vti->N_TriIndex; ++i) {
         if (vti->IntersectedVoxels[i]) free(vti->IntersectedVoxels[i]);
      }
      SUMA_free(vti->IntersectedVoxels);
   }
   if (vti->N_IntersectedVoxels) SUMA_free(vti->N_IntersectedVoxels);    
   SUMA_free(vti);
     
   SUMA_RETURN(NULL);
}     
/*!
   \brief Function to return a set of voxels that are intersected by
   a triangle. 
   
   \param SO (SUMA_SurfaceObject *)
   \param VolPar (SUMA_VOLPAR *)
   \param NodeIJKlist (float *) the equivalent of SO->NodeList only in i,j,k indices into VolPar's grid.
                           In the future, might want to allow for this param to be NULL and have it
                           be generated internally from SO->NodeList and VolPar.
   \param vti (SUMA_VTI *) properly initialized Voxel Triangle Intersection structure
   \return vti (SUMA_VTI *) filled up VTI structure.
   - Closely based on section in function SUMA_SurfGridIntersect
   If you find bugs here, fix them there too. 
*/
SUMA_VTI *SUMA_GetVoxelsIntersectingTriangle(   
   SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, float *NodeIJKlist,
                                                SUMA_VTI *vti )
{
   static char FuncName[]={"SUMA_GetVoxelsIntersectingTriangle"};
   int ti, nx, ny, nz, nxy, nxyz, N_inbox, n1, n2, n3, nt, nt3, nijk, nf;
   int N_alloc, N_realloc, en, *voxelsijk=NULL, N_voxels1d = 0, *voxels1d = NULL;
   int *TriIndex=NULL, N_TriIndex;
   float dxyz[3];
   float tol_dist = 0; /* A way to fatten up the shell a little bit. Set to 0 if no fat is needed */
   float *p1, *p2, *p3, min_v[3], max_v[3], p[3], dist;
   FILE *fp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   if (SO->FaceSetDim != 3 || SO->NodeDim != 3) {
      SUMA_SL_Err("SO->FaceSetDim != 3 || SO->NodeDim != 3"); 
      SUMA_RETURN(NULL);
   }
   if (!vti) {
      SUMA_SL_Err("vti must be non NULL");
      SUMA_RETURN(NULL);
   }
   if (vti->N_TriIndex <= 0) {
      SUMA_SL_Err("vti must be initialized");
      SUMA_RETURN(NULL);
   }
   
   TriIndex = vti->TriIndex;
   N_TriIndex = vti->N_TriIndex;
   
   nx = VolPar->nx; ny = VolPar->ny; nz = VolPar->nz; nxy = nx * ny; nxyz = nx * ny * nz;
   
   if (LocalHead) {
      SUMA_LH("Debug mode, writing file: SUMA_GetVoxelsIntersectingTriangle.1D");
      fp = fopen("SUMA_GetVoxelsIntersectingTriangle.1D","w");
      if (fp) fprintf(fp, "# Voxels from %s that intersect the triangles \n", VolPar->filecode);
   }   
   /* cycle through all triangles and find voxels that intersect them */
   N_alloc = 2000; /* expected maximum number of voxels in triangle's bounding box */
   N_realloc = 0;
   voxelsijk = (int *)SUMA_malloc(sizeof(int)*N_alloc*3);
   if (!voxelsijk) { SUMA_SL_Crit("Failed to Allocate!"); SUMA_RETURN(NULL);  }   
   dxyz[0] = VolPar->dx; dxyz[1] = VolPar->dy; dxyz[2] = VolPar->dz;
   for (ti=0; ti<N_TriIndex; ++ti) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Now processing %dth triangle\n", FuncName, ti);
      nf = TriIndex[ti];
      if (LocalHead) fprintf(SUMA_STDERR,"\t\tindexed %d, \n", nf);
      n1 = SO->FaceSetList[SO->FaceSetDim*nf]; n2 = SO->FaceSetList[SO->FaceSetDim*nf+1]; n3 = SO->FaceSetList[SO->FaceSetDim*nf+2];
      if (LocalHead) fprintf(SUMA_STDERR,"\t\tmade up of  nodes %d, %d. %d . \n", n1, n2, n3);
      /* find the bounding box of the triangle */
      p1 = &(NodeIJKlist[3*n1]); p2 = &(NodeIJKlist[3*n2]); p3 = &(NodeIJKlist[3*n3]); 
      SUMA_TRIANGLE_BOUNDING_BOX(p1, p2, p3, min_v, max_v);
      
      /* quick check of preallocate size of voxelsijk */
      en =((int)(max_v[0] - min_v[0] + 5) * (int)(max_v[1] - min_v[1] + 5) * (int)(max_v[2] - min_v[2] + 5)); 
      if ( en > N_alloc) {
         ++N_realloc; if (N_realloc > 5) { SUMA_SL_Warn("Reallocating, increase limit to improve speed.\nEither triangles too large or grid too small"); }
         N_alloc = 2*en;
         voxelsijk = (int *)SUMA_realloc(voxelsijk, 3*N_alloc*sizeof(int));
         if (!voxelsijk) { SUMA_SL_Crit("Failed to Allocate!"); SUMA_RETURN(NULL); }
      } 
      /* find the list of voxels inhabiting this box */
      N_inbox = 0;
      if (!SUMA_VoxelsInBox(voxelsijk, &N_inbox, min_v, max_v)) {
         SUMA_SL_Err("Unexpected error!"); SUMA_RETURN(NULL); 
      }
      if (!N_inbox) { SUMA_SL_Err("Unexpected error, no voxels in box!"); SUMA_RETURN(NULL);  }
      if (N_inbox >= N_alloc) { SUMA_SL_Err("Allocation trouble!"); SUMA_RETURN(NULL);  }
      if (LocalHead) fprintf(SUMA_STDERR,"\t\t%d nodes in box\n", N_inbox);
      
      /* allocate for 1D indices of voxels intersecting the triangle */
      if (voxels1d) {
         SUMA_SL_Err("NULL pointer expected here");
         SUMA_RETURN(NULL);
      }
      if (LocalHead) fprintf(SUMA_STDERR,"\t\tShit man, %d nodes in box\n", N_inbox);
      voxels1d = (int *)malloc(N_inbox * sizeof(int)); /* Too many SUMA_mallocs keep it simple here, this function is called many many times */
      if (LocalHead) fprintf(SUMA_STDERR,"\t\tWTF man, %d nodes in box\n", N_inbox);
      N_voxels1d=0;
      if (!voxels1d) {
         SUMA_SL_Crit("Failed to allocate voxels1d");
         SUMA_RETURN(NULL);
      }
      /* mark these voxels as inside the business */
      if (LocalHead) fprintf(SUMA_STDERR,"%s:\t\tabout to process %d voxels\n", FuncName, N_inbox);
      for (nt=0; nt < N_inbox; ++nt) {
         nt3 = 3*nt;
         if (voxelsijk[nt3] < nx &&  voxelsijk[nt3+1] < ny &&  voxelsijk[nt3+2] < nz) {
            nijk = SUMA_3D_2_1D_index(voxelsijk[nt3], voxelsijk[nt3+1], voxelsijk[nt3+2], nx , nxy);  
            { 
               /* what side of the plane is this voxel on ? */
               p[0] = (float)voxelsijk[nt3]; p[1] = (float)voxelsijk[nt3+1]; p[2] = (float)voxelsijk[nt3+2]; 
               SUMA_DIST_FROM_PLANE(p1, p2, p3, p, dist);
               /* Does voxel contain any of the nodes ? */
               if (tol_dist && SUMA_ABS(dist) < tol_dist) dist = tol_dist; /* Fatten the representation a little bit 
                                                             There are holes in the mask created using
                                                             the condition below alone. I am not sure
                                                             why that is the case but whatever gap there
                                                             is in one plane, results from a thick line in
                                                             the other. Don't know if that is a bug yet
                                                             or an effect of discretization. At any rate
                                                             it should not affect what I plan to do with
                                                             this. Could the bug be in 
                                                             SUMA_isVoxelIntersect_Triangle?
                                                             Thu Dec 22 17:03:48 EST 2005, Update:
                                                             SUMA_isVoxelIntersect_Triangle had a precision
                                                             bug. It has been fixed but I have not reexamined
                                                             this block yet*/
               if (!(SUMA_IS_STRICT_NEG(VolPar->Hand * dist))) { /* voxel is outside (along normal) */
                  /* does this triangle actually intersect this voxel ?*/
                  if (SUMA_isVoxelIntersect_Triangle (p, dxyz, p1, p2, p3)) {
                     /* looks good, store it */
                     if (LocalHead) fprintf(SUMA_STDERR,"nt %d, N_voxels1d %d\n", nt, N_voxels1d);
                     voxels1d[N_voxels1d] = nijk; ++N_voxels1d;
                     if (fp) fprintf(fp, "%d %d %d\n", voxelsijk[nt3], voxelsijk[nt3+1], voxelsijk[nt3+2]);
                  } 
               }
               
            }
         }
      }
      /* store the results */
      vti->IntersectedVoxels[ti] = voxels1d; 
      vti->N_IntersectedVoxels[ti] = N_voxels1d;
      voxels1d = NULL; N_voxels1d = 0; 
   }
   
   if (LocalHead) {
      if (fp) fclose(fp); fp = NULL;
   } 
   SUMA_RETURN(vti);
}
/*!
   \brief Function to detect surface self intersection
   returns -1 in case of error,
            0 in case of no intersection
            1 in case of intersection
*/ 
int SUMA_isSelfIntersect(SUMA_SurfaceObject *SO, int StopAt, byte *report)
{
   static char FuncName[]={"SUMA_isSelfIntersect"};
   float *NodePos = NULL, *p1=NULL, *p2=NULL, *p3 = NULL, p[3], *ep1=NULL, *ep2=NULL;
   int hit = 0, k, t1, t2, it, it3, n1, n2, n3;
   SUMA_MT_INTERSECT_TRIANGLE *MTI = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO->EL) {
      SUMA_SL_Err("NULL SO->EL");
      SUMA_RETURN(-1);
   }
   
   if (StopAt < 1) StopAt = 1;
   
   hit = 0; k = 0;
   while (k < SO->EL->N_EL) {
         t1 = SO->EL->ELps[k][1]; t2 = SO->EL->ELps[SUMA_MIN_PAIR(k+1, SO->EL->N_EL-1)][1];
         ep1 = &(SO->NodeList[3*SO->EL->EL[k][0]]); ep2 = &(SO->NodeList[3*SO->EL->EL[k][1]]);
         /* find out if segment intersects */
         MTI = SUMA_MT_intersect_triangle(ep1, ep2, SO->NodeList, SO->N_Node, SO->FaceSetList, SO->N_FaceSet, MTI); 
         for (it=0; it<SO->N_FaceSet; ++it) {
            if (MTI->isHit[it] && it != t1 && it != t2 && MTI->u[it] > SUMA_EPSILON && MTI->v[it] > SUMA_EPSILON) {
               /* ray hit triangle, is intersection inside segment ? */
               /* SUMA_LH("Checking hit..."); */
               it3 = SO->FaceSetDim*it;
               n1 = SO->FaceSetList[it3]; n2 = SO->FaceSetList[it3+1]; n3 = SO->FaceSetList[it3+2];
               p1 = &(SO->NodeList[SO->NodeDim*n1]); p2 = &(SO->NodeList[SO->NodeDim*n2]); p3 = &(SO->NodeList[SO->NodeDim*n3]);   
               SUMA_FROM_BARYCENTRIC(MTI->u[it], MTI->v[it], p1, p2, p3, p);
               
               if (p[0] > ep1[0] && p[0] < ep2[0]) {
                  /* if  (LocalHead && it == 1638) {
                  fprintf(SUMA_STDERR,"%s: Segment [%d %d] u = %f v = %f\nep1: %.6f   %.6f   %.6f\n p : %.6f   %.6f  %.6f\nep2: %.6f   %.6f  %.6f\n", 
                                 FuncName, SO->EL->EL[k][0], SO->EL->EL[k][1], MTI->u[it], MTI->v[it],ep1[0], ep1[1], ep1[2], p[0], p[1], p[2], ep2[0], ep2[1], ep2[2]); 
                  } */ 
                  if (p[1] > ep1[1] && p[1] < ep2[1]) {
                     if (p[2] > ep1[2] && p[2] < ep2[2]) {
                        /* point in segment, self intersection detected. */
                        if (report || LocalHead) fprintf(SUMA_STDERR,"%s: Triangle %d (%d, %d, %d) was hit by segment formed by nodes [%d, %d]\n", 
                           FuncName, it, n1, n2, n3, SO->EL->EL[k][0], SO->EL->EL[k][1]);
                           ++ hit;
                           if (report) { report[SO->EL->EL[k][0]] = report[SO->EL->EL[k][1]] = 1; }
                        break;
                     }
                  }
               }
            }
         }
         if (hit >= StopAt) break;
         /* skip duplicate edges */
         if (SO->EL->ELps[k][2] > 0) {
               k += SO->EL->ELps[k][2];
         } else ++k;
   }
   
   if (MTI) MTI = SUMA_Free_MT_intersect_triangle(MTI); 
   
   if (report || LocalHead) {
      if (!hit) {
         SUMA_LH("Surface does not self intersect.");
      } else {
         SUMA_LH("Surface self intersects.");
      }
   }
   SUMA_RETURN(hit);
}

/*!
   \brief find the neighbors to a voxel.
   \param ijk (int) a voxel's 1D index 
   \param ni, nj, nk (int) number of voxels in each of the three directions
   \param ntype (SUMA_VOX_NEIGHB_TYPES) neighborhood type
                  SUMA_VOX_NEIGHB_FACE a maximum total of 6 neighbors
                  SUMA_VOX_NEIGHB_EDGE a maximum total of 6 + 12 neighbors
                  SUMA_VOX_NEIGHB_CORNER a maximum total of 6 + 12 + 8 neighbors
   \param nl (int *) vector to contain the 1D indices of neighboring voxels. Voxels 
                     outside the volume boundaries are not considered. You should make sure nl
                     can hold a total of 26 values.
   \param N_n (int) number of neighbors.
*/               
int SUMA_VoxelNeighbors (int ijk, int ni, int nj, int nk, SUMA_VOX_NEIGHB_TYPES ntype, int *nl)
{
   static char FuncName[]={"SUMA_VoxelNeighbors"};
   int i, j, k;
   int nij, N_n;
   
   SUMA_ENTRY;
   
   N_n = 0; nij = ni * nj;
   
   /* change ijk to 3D */
   SUMA_1D_2_3D_index(ijk, i, j, k, ni, nij);
   /* 
   if (ijk == 5030) {
      fprintf(SUMA_STDERR,"%s:[%d] %d %d %d\n", FuncName, ijk, i, j, k);      
   }
   */
   if (i >= ni || i < 0) { SUMA_SL_Err("Voxel out of bounds along i direction"); SUMA_RETURN(N_n); }
   if (j >= nj || j < 0) { SUMA_SL_Err("Voxel out of bounds along j direction"); SUMA_RETURN(N_n); }
   if (k >= nk || k < 0) { SUMA_SL_Err("Voxel out of bounds along k direction"); SUMA_RETURN(N_n); }
  
   /* start with the face neighbors */
   if (i-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i-1, j, k, ni, nij); ++N_n; }
   if (j-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i, j-1, k, ni, nij); ++N_n; } 
   if (k-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i, j, k-1, ni, nij); ++N_n; } 
   if (i+1 < ni) { nl[N_n] = SUMA_3D_2_1D_index(i+1, j, k, ni, nij); ++N_n; } 
   if (j+1 < nj) { nl[N_n] = SUMA_3D_2_1D_index(i, j+1, k, ni, nij); ++N_n; } 
   if (k+1 < nk) { nl[N_n] = SUMA_3D_2_1D_index(i, j, k+1, ni, nij); ++N_n; } 
   /*
   if (ijk == 5030) {
      fprintf(SUMA_STDERR,"%s:[%d] %d %d %d %d %d %d\n", FuncName, ijk, 
                              nl[0],nl[1],nl[2],nl[3],nl[4],nl[5] );      
   }
   */   
   if ( ntype < SUMA_VOX_NEIGHB_EDGE) { SUMA_RETURN(N_n); }
   
   /* add edge neighbors */
   if (i-1 >= 0 && j-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i-1, j-1, k, ni, nij); ++N_n; }
   if (i-1 >= 0 && j+1 < nj) { nl[N_n] = SUMA_3D_2_1D_index(i-1, j+1, k, ni, nij); ++N_n; }
   if (i-1 >= 0 && k-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i-1, j, k-1, ni, nij); ++N_n; }
   if (i-1 >= 0 && k+1 < nk) { nl[N_n] = SUMA_3D_2_1D_index(i-1, j, k+1, ni, nij); ++N_n; }
   if (j-1 >= 0 && k-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i, j-1, k-1, ni, nij); ++N_n; } 
   if (j-1 >= 0 && k+1 < nk) { nl[N_n] = SUMA_3D_2_1D_index(i, j-1, k+1, ni, nij); ++N_n; } 
   if (i+1 < ni && j-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i+1, j-1, k, ni, nij); ++N_n; }
   if (i+1 < ni && j+1 < nj) { nl[N_n] = SUMA_3D_2_1D_index(i+1, j+1, k, ni, nij); ++N_n; }
   if (i+1 < ni && k-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i+1, j, k-1, ni, nij); ++N_n; }
   if (i+1 < ni && k+1 < nk) { nl[N_n] = SUMA_3D_2_1D_index(i+1, j, k+1, ni, nij); ++N_n; }
   if (j+1 < nj && k-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i, j+1, k-1, ni, nij); ++N_n; } 
   if (j+1 < nj && k+1 < nk) { nl[N_n] = SUMA_3D_2_1D_index(i, j+1, k+1, ni, nij); ++N_n; } 
   
   if ( ntype < SUMA_VOX_NEIGHB_CORNER) { SUMA_RETURN(N_n); }
   
   /* add corner neighbors */
   if (i-1 >= 0 && j-1 >= 0 && k-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i-1, j-1, k-1, ni, nij); ++N_n; }
   if (i-1 >= 0 && j-1 >= 0 && k+1 < nk) { nl[N_n] = SUMA_3D_2_1D_index(i-1, j-1, k+1, ni, nij); ++N_n; }
   if (i-1 >= 0 && j+1 < nj && k-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i-1, j+1, k-1, ni, nij); ++N_n; }
   if (i-1 >= 0 && j+1 < nj && k+1 < nk) { nl[N_n] = SUMA_3D_2_1D_index(i-1, j+1, k+1, ni, nij); ++N_n; }
   if (i+1 < ni && j-1 >= 0 && k-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i+1, j-1, k-1, ni, nij); ++N_n; }
   if (i+1 < ni && j-1 >= 0 && k+1 < nk) { nl[N_n] = SUMA_3D_2_1D_index(i+1, j-1, k+1, ni, nij); ++N_n; }
   if (i+1 < ni && j+1 < nj && k-1 >= 0) { nl[N_n] = SUMA_3D_2_1D_index(i+1, j+1, k-1, ni, nij); ++N_n; }
   if (i+1 < ni && j+1 < nj && k+1 < nk) { nl[N_n] = SUMA_3D_2_1D_index(i+1, j+1, k+1, ni, nij); ++N_n; }

   
   SUMA_RETURN(N_n);
}

/*!
   \brief Function to fill the volume enclose in a mask
   \param ijkmask (byte *) mask (nvox x 1), typically the result of the intersection of a closed surface with a volume
   \param ijkseed (int) 1D index of seed voxel. Must be inside the mask and not a part of it.
   \param ni (int) number of voxels in the i direction
   \param nj (int) number of voxels in the j direction
   \param nk (int) number of voxels in the k direction
   \param N_in (int *) to contain the number of voxels inside the mask
   \parm usethisisin (byte *)store results in this mask vector rather than allocate for a new one.
   \param fillhole (int) fills small holes, intended to correct for volume masks created from surfaces with minor intersections
   \return isin (byte *) a nvox x 1 vector containing:
      0: for voxels outside mask
      1: for voxels inside mask
       
*/
byte *SUMA_FillToVoxelMask(byte *ijkmask, int ijkseed, int ni, int nj, int nk, int *N_in, byte *usethisisin) 
{
   static char FuncName[]={"SUMA_FillToVoxelMask"};
   byte *isin = NULL, *visited=NULL;
   DList*candlist=NULL;
   DListElmt *dothiselm=NULL;
   int dothisvoxel;
   void * dtmp=NULL;
   int nl[50], N_n, in ,neighb, nijk, i, j, k, nij;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *N_in = 0;
   
   if (!ijkmask) {
      SUMA_SL_Err("Nothing to do");
      SUMA_RETURN(NULL);
   }
   if (ijkmask[ijkseed]) {
      SUMA_SL_Err("Seed is on mask. Bad business.");
      SUMA_RETURN(NULL);
   }
   
   nij = ni * nj;
   nijk = ni * nj * nk;
   
   if (LocalHead) {
      SUMA_1D_2_3D_index (ijkseed, i, j, k, ni, nij);
      fprintf(SUMA_STDERR,"%s:\nSeed is %d %d %d\n", FuncName, i, j, k); 
   }
   candlist = (DList*)SUMA_malloc(sizeof(DList));
   visited = (byte *)SUMA_calloc(nijk, sizeof(byte));
   if (!visited || !candlist) {
      SUMA_SL_Crit("Failed to allocate for visited or candlist");  
      SUMA_RETURN(NULL);  
   }
   
   if (usethisisin) {
      isin = usethisisin;
      SUMA_LH("Reusing isin");
   } else {
      isin = (byte *)SUMA_calloc(nijk, sizeof(byte));
      if (!isin) {
         SUMA_SL_Crit("Failed to allocate");
         SUMA_RETURN(NULL);
      }
      SUMA_LH("Fresh isin");
   }
   
   dothisvoxel = ijkseed;
   dlist_init(candlist, NULL);
   
   isin[dothisvoxel] = 1; ++(*N_in); /* Add voxel to cluster */
   visited[dothisvoxel] = 1;  
   dlist_ins_next(candlist, dlist_tail(candlist), (const void *)dothisvoxel); /* Add voxel as next candidate*/
   
   while (dlist_size(candlist)) {
      /* find neighbors in its vicinity */
      dothiselm = dlist_head(candlist); dothisvoxel = (int) dothiselm->data;
      N_n = SUMA_VoxelNeighbors (dothisvoxel, ni, nj, nk, SUMA_VOX_NEIGHB_FACE, nl);
      /*
         if (dothisvoxel == 5030 && LocalHead) {
            for (in=0; in<N_n; ++in) {  fprintf(SUMA_STDERR,"%s: pre removal %d\n", FuncName, nl[in]); }    
         }
      */
      /* remove node from candidate list */
      dlist_remove(candlist, dothiselm, (void *)(&dtmp)); /* Make sure dtmp has enough space to hold a pointer! An int does not hold a pointer on 64 bit MCs */
      /*
         if (dothisvoxel == 5030 && LocalHead) {
            for (in=0; in<N_n; ++in) {  fprintf(SUMA_STDERR,"%s: post removal %d\n", FuncName, nl[in]); }    
         }
      */
      /* search to see if any are to be assigned */
      /* if (dothisvoxel == 5030 && LocalHead) fprintf(SUMA_STDERR,"%s: dothisvoxel = %d\n", FuncName, dothisvoxel);*/
      for (in=0; in<N_n; ++in) { 
         neighb = nl[in];
         /* if (dothisvoxel == 5030 && LocalHead) fprintf(SUMA_STDERR,"   Working neighb %d, ijkmask[neighb] = %d\n", neighb, ijkmask[neighb]);*/
         if (!ijkmask[neighb]) {
            /* if (dothisvoxel == 5030 && LocalHead) fprintf(SUMA_STDERR,"   neighb %d marked isin\n", neighb); */
            isin[neighb] = 1; ++(*N_in); /* Add voxel to cluster */
            /* mark it as a candidate if it has not been visited as a candidate before */
            if (!visited[neighb]) {
               /* if (dothisvoxel == 5030 && LocalHead) fprintf(SUMA_STDERR,"   neighb %d added to candidate list\n", neighb); */
               dlist_ins_next(candlist, dlist_tail(candlist), (const void *)neighb);
               visited[neighb] = 1;   
            }
         } else {
         /*   if (dothisvoxel == 5030 && LocalHead) fprintf(SUMA_STDERR,"   neighb %d already in mask\n", neighb); */
         }
      }
   }
   
   if (visited) SUMA_free(visited); visited = NULL;
   if (candlist) { dlist_destroy(candlist); SUMA_free(candlist); candlist  = NULL; }

   
   SUMA_RETURN(isin);
}

/*!
   \brief find voxels whose centers are inside the box with corners c1 and c2
   c1, c2 are in voxel index coordinates. c1 is the minimum coordinates point.
   c2 is the maximum coordinates point.
*/
SUMA_Boolean SUMA_VoxelsInBox(int *voxelsijk, int *N_in, float *c1, float *c2)
{
   static char FuncName[]={"SUMA_VoxelsInBox"};
   int n3, i, j, k;
   int N_Allocated = 0;
   
   SUMA_ENTRY;
   
   if (!voxelsijk) { 
      SUMA_SL_Err("NULL voxelsijk");
      SUMA_RETURN(NOPE); 
   }
   
   if (*N_in != 0) { N_Allocated = *N_in; }
   *N_in = 0;
   
   #if 0
   for (k = SUMA_ROUND(c1[2]); k <= SUMA_ROUND(c2[2]); ++k) {
      for (j = SUMA_ROUND(c1[1]); j <= SUMA_ROUND(c2[1]); ++j) {
         for (i = SUMA_ROUND(c1[0]); i <= SUMA_ROUND(c2[0]); ++i) {
            n3 = 3*(*N_in);
            voxelsijk[n3] = i; voxelsijk[n3+1] = j; voxelsijk[n3+2] = k; 
            ++(*N_in); 
         }
      }
   }
   #else
   for (k = (int)(c1[2]); k <= SUMA_CEIL(c2[2]); ++k) {
      for (j = (int)(c1[1]); j <= SUMA_CEIL(c2[1]); ++j) {
         for (i = (int)(c1[0]); i <= SUMA_CEIL(c2[0]); ++i) {
            if (N_Allocated) {
               if (*N_in >= N_Allocated) {
                  fprintf(SUMA_STDERR,"Error %s: More voxels inbox than allocated (%d)\n", FuncName, N_Allocated);
                  SUMA_RETURN(NOPE);
               }
            }
            n3 = 3*(*N_in);
            voxelsijk[n3] = i; voxelsijk[n3+1] = j; voxelsijk[n3+2] = k; 
            ++(*N_in); 
         }
      }
   }
   #endif     
   SUMA_RETURN(YUP); 
}


/*!
   \brief Applies an affine transform the coordinates in NodeList
   
   \param NodeList (float *) a vector of node XYZ triplets. (N_Node x 3 long)
   \param N_Node (int) number of nodes in NodeList
   \param M (float **) the affine transform matrix. 
                     Minimum size is 3 rows x 4 columns. 
                     The top left 3x3 is mat
                     The right most column is the shift vector vec (3 elements)
   \param center (float *) If center is not null then
                     XYZnew = mat * (vec - center) + vec + center
                     else  XYZnew = mat * (vec ) + vec 
   \return ans (SUMA_Boolean ) 1 OK, 0 not OK
   
   - COORDINATES IN NodeList are REPLACED with transformed ones.
                        
*/                     
SUMA_Boolean SUMA_ApplyAffine (float *NodeList, int N_Node, float M[][4], float *center)
{
   static char FuncName[] = {"SUMA_ApplyAffine"};
   float **XYZo, **Mr, **XYZn, D[3];
   int i, i3, idbg = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_S_Note("Use SUMA_Apply_Coord_xform instead");
   
   if (!NodeList || N_Node <=0) { 
      SUMA_SL_Err("Bad Entries.\n");
      SUMA_RETURN(NOPE);
   }
   
   Mr = (float **)SUMA_allocate2D(3, 3, sizeof(float));
   XYZn = (float **)SUMA_allocate2D(3, 1, sizeof(float));
   XYZo = (float **)SUMA_allocate2D(3, 1, sizeof(float));
   
   SUMA_LH("Forming Mr");
   Mr[0][0] = M[0][0]; Mr[0][1] = M[0][1]; Mr[0][2] = M[0][2]; 
   Mr[1][0] = M[1][0]; Mr[1][1] = M[1][1]; Mr[1][2] = M[1][2]; 
   Mr[2][0] = M[2][0]; Mr[2][1] = M[2][1]; Mr[2][2] = M[2][2];
   D[0] = M[0][3]; D[1] = M[1][3]; D[2] = M[2][3];
   
   SUMA_LH("Transforming");
   if (LocalHead ) {
      i3 = 3*idbg;
      fprintf (SUMA_STDERR,"In: %f %f %f\n", NodeList[i3], NodeList[i3+1], NodeList[i3+2]);
   }
   for (i=0; i< N_Node; ++i) {
      i3 = 3 * i;
      if (!center) {
         XYZo[0][0] = NodeList[i3]; XYZo[1][0] = NodeList[i3+1]; XYZo[2][0] = NodeList[i3+2];
      } else {
         XYZo[0][0] = NodeList[i3] - center[0]; XYZo[1][0] = NodeList[i3+1] - center[1]; XYZo[2][0] = NodeList[i3+2] - center[2];
      }   

      SUMA_MULT_MAT(Mr, XYZo, XYZn, 3, 3, 1, float,float,float);
      
      if (!center) { 
         NodeList[i3] = XYZn[0][0]+D[0]; NodeList[i3+1] = XYZn[1][0]+D[1]; NodeList[i3+2] = XYZn[2][0]+D[2]; 
      } else {
         NodeList[i3] = XYZn[0][0]+D[0] + center[0]; NodeList[i3+1] = XYZn[1][0]+D[1]+ center[1]; NodeList[i3+2] = XYZn[2][0]+D[2]+ center[2]; 
      }
      
   }
   if (LocalHead ) {
      i3 = 3*idbg;
      fprintf (SUMA_STDERR,"Out: %f %f %f\n", NodeList[i3], NodeList[i3+1], NodeList[i3+2]);
   }
   SUMA_LH("Done");
   
   SUMA_free2D((char**)Mr, 3);
   SUMA_free2D((char**)XYZn, 3);
   SUMA_free2D((char**)XYZo, 3);
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_getoffsets (int n, SUMA_SurfaceObject *SO, float *Off, float lim) 
{
   static char FuncName[]={"SUMA_getoffsets"};
   int i, ni, iseg;
   float Off_tmp;
   SUMA_Boolean Visit = NOPE;
   static SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   #if DoCheck
   if (!SO->FN || !SO->EL) {
      SUMA_SL_Err("SO->FN &/| SO->EL are NULL.\n");
      SUMA_RETURN(NOPE);
   }
   #endif
   
   #if DBG
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Working node %d, %d neighbs. lim = %f\n", 
                                    FuncName, n, SO->FN->N_Neighb[n], lim);
   #endif
   
   for (i=0; i < SO->FN->N_Neighb[n]; ++i) {
      ni = SO->FN->FirstNeighb[n][i]; /* for notational sanity */
      iseg = SUMA_FindEdge (SO->EL, n, SO->FN->FirstNeighb[n][i]);
      #if DoCheck
      if (iseg < 0) {
         SUMA_SL_Err("Failed to find segment");
         SUMA_RETURN(NOPE);
      }
      #endif
      
      Off_tmp = Off[n] + SO->EL->Le[iseg];   /* that is the distance from n (original n) to ni along
                                                that particular path */
                                             
      Visit = NOPE;
      if (Off[ni] < 0 || Off_tmp < Off[ni]) { /* Distance improvement, visit/revist that node */
         if (Off_tmp < lim) { /* only record if less than lim */
            Visit = YUP;
            Off[ni] = Off_tmp;
         } 
      } 
      
      #if DBG
      if (LocalHead) fprintf(SUMA_STDERR,"%s: %d --> %d. Visit %d, Current %f, Old %f\n", 
         FuncName, n, ni, Visit, Off_tmp, Off[ni]);
      #endif
      
      #if 0
         { int jnk; fprintf(SUMA_STDOUT,"Pausing ..."); jnk = getchar(); fprintf(SUMA_STDOUT,"\n"); }
      #endif

      if (Visit) { /* a new node has been reached with an offset less than limit, go down that road */
         if (!SUMA_getoffsets (ni, SO, Off, lim))  {
            SUMA_SL_Err("Failed in SUMA_getoffsets");
            SUMA_RETURN (NOPE);
         }
      }
   }

   SUMA_RETURN(YUP);
}

/*!
   \brief Allocate and initialize SUMA_GET_OFFSET_STRUCT* struct 
   OffS = SUMA_Initialize_getoffsets (N_Node);
   
   \param N_Node(int) number of nodes forming mesh 
   \return OffS (SUMA_GET_OFFSET_STRUCT *) allocate structure
           with initialized fields for zeroth order layer
   
   \sa SUMA_AddNodeToLayer
   \sa SUMA_Free_getoffsets
   \sa SUMA_Initialize_getoffsets
*/           
   
SUMA_GET_OFFSET_STRUCT *SUMA_Initialize_getoffsets (int N_Node)
{
   static char FuncName[]={"SUMA_Initialize_getoffsets"};
   int i;
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   
   SUMA_ENTRY;
   
   if (N_Node <= 0) {
      SUMA_SL_Err("Bad values for N_Node");
      SUMA_RETURN (OffS);
   }
   
   OffS = (SUMA_GET_OFFSET_STRUCT *)SUMA_malloc(sizeof(SUMA_GET_OFFSET_STRUCT));
   if (!OffS) {
      SUMA_SL_Err("Failed to allocate for OffS");
      SUMA_RETURN (OffS);
   }
   
   OffS->OffVect = (float *) SUMA_malloc(N_Node * sizeof(float));
   OffS->LayerVect = (int *) SUMA_malloc(N_Node * sizeof(int));
   OffS->N_Nodes = N_Node;
   
   if (!OffS->LayerVect || !OffS->OffVect) {
      SUMA_SL_Err("Failed to allocate for OffS->LayerVect &/| OffS->OffVect");
      SUMA_free(OffS);
      SUMA_RETURN (OffS);
   }
   
   /* initialize vectors */
   for (i=0; i< N_Node; ++i) {
      OffS->OffVect[i] = 0.0;
      OffS->LayerVect[i] = -1;
   }
   
   /* add a zeroth layer for node n */
   OffS->N_layers = 1;
   OffS->layers = (SUMA_NODE_NEIGHB_LAYER *) SUMA_malloc(OffS->N_layers * sizeof(SUMA_NODE_NEIGHB_LAYER));
   OffS->layers[0].N_AllocNodesInLayer = 1;
   OffS->layers[0].NodesInLayer = (int *) SUMA_malloc(OffS->layers[0].N_AllocNodesInLayer * sizeof(int));
   OffS->layers[0].N_NodesInLayer = 0;   
   
   SUMA_RETURN (OffS);
   
}

/*!
   \brief Add node n to neighboring layer LayInd in OffS
   ans = SUMA_AddNodeToLayer (n, LayInd, OffS);
   
   \param n (int)
   \param LayInd (int)
   \param OffS (SUMA_GET_OFFSET_STRUCT *)
   \return YUP/NOPE (good/bad)
   
   - allocation is automatically taken care of
   
   \sa SUMA_Free_getoffsets
   \sa SUMA_Initialize_getoffsets
*/
SUMA_Boolean SUMA_AddNodeToLayer (int n, int LayInd, SUMA_GET_OFFSET_STRUCT *OffS)
{
   static char FuncName[]={"SUMA_AddNodeToLayer"};
   static SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* is this a new layer */
   if (LayInd > OffS->N_layers) { /* error */
      SUMA_SL_Err("LayInd > OffS->N_layers. This should not be!");
      SUMA_RETURN(NOPE);
   } else if (LayInd == OffS->N_layers) { /* need a new one */
      SUMA_LH("Adding layer");
      OffS->N_layers += 1;
      OffS->layers = (SUMA_NODE_NEIGHB_LAYER *) SUMA_realloc(OffS->layers, OffS->N_layers*sizeof(SUMA_NODE_NEIGHB_LAYER));
      OffS->layers[LayInd].N_AllocNodesInLayer = 200;
      OffS->layers[LayInd].NodesInLayer = (int *) SUMA_malloc(OffS->layers[LayInd].N_AllocNodesInLayer * sizeof(int));
      OffS->layers[LayInd].N_NodesInLayer = 0;
   }
   
   OffS->layers[LayInd].N_NodesInLayer += 1;
   /* do we need to reallocate for NodesInLayer ? */
   if (OffS->layers[LayInd].N_NodesInLayer ==  OffS->layers[LayInd].N_AllocNodesInLayer) { /* need more space */
      SUMA_LH("reallocating neighbors");
      OffS->layers[LayInd].N_AllocNodesInLayer += 200;
      OffS->layers[LayInd].NodesInLayer = (int *) SUMA_realloc (OffS->layers[LayInd].NodesInLayer, OffS->layers[LayInd].N_AllocNodesInLayer * sizeof(int));
   }
   
   OffS->layers[LayInd].NodesInLayer[OffS->layers[LayInd].N_NodesInLayer - 1] = n;
   
   SUMA_RETURN(YUP); 
}

/*!
   \brief free memory associated with SUMA_GET_OFFSET_STRUCT * struct
   
   \param OffS (SUMA_GET_OFFSET_STRUCT *) Offset strcture
   \return NULL
   
   \sa SUMA_Recycle_getoffsets
   \sa SUMA_Initialize_getoffsets
*/
SUMA_GET_OFFSET_STRUCT * SUMA_Free_getoffsets (SUMA_GET_OFFSET_STRUCT *OffS) 
{
   static char FuncName[]={"SUMA_Free_getoffsets"};
   int i = 0;
   static SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!OffS) SUMA_RETURN(NULL);
   
   if (OffS->layers) {
      for (i=0; i< OffS->N_layers; ++i) if (OffS->layers[i].NodesInLayer) SUMA_free(OffS->layers[i].NodesInLayer);
      SUMA_free(OffS->layers);
   }
   
   if (OffS->OffVect) SUMA_free(OffS->OffVect);
   if (OffS->LayerVect) SUMA_free(OffS->LayerVect);
   SUMA_free(OffS); OffS = NULL;
   
   SUMA_RETURN(NULL);
}

/*!
   \brief reset the SUMA_GET_OFFSET_STRUCT after it has been used by a node
   \param OffS (SUMA_GET_OFFSET_STRUCT *) Offset structure that has node neighbor
                                          info and detail to be cleared
   \return (YUP/NOPE) success/failure 
   
   - No memory is freed here
   - The used node layer indices are reset to -1
   - The number of nodes in each layer are reset to 0
   
   \sa SUMA_Free_getoffsets to free this structure once and for all
   \sa SUMA_Initialize_getoffsets
*/
SUMA_Boolean SUMA_Recycle_getoffsets (SUMA_GET_OFFSET_STRUCT *OffS)
{
   static char FuncName[]={"SUMA_Recycle_getoffsets"};
   int i, j;
   static SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   for (i=0; i < OffS->N_layers; ++i) {
      /* reset the layer index of used nodes in LayerVect */
      for (j=0; j < OffS->layers[i].N_NodesInLayer; ++j) {
         OffS->LayerVect[OffS->layers[i].NodesInLayer[j]] = -1;
      }
      /* reset number of nodes in each layer */
      OffS->layers[i].N_NodesInLayer = 0;
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief calculates the length of the segments defined
   by a node and its first-order neighbors. The resulting
   matrix very closely resembles SO->FN->FirstNeighb
   DistFirstNeighb = SUMA_CalcNeighbDist (SO);
   
   \param SO (SUMA_SurfaceObject *) with FN field required
   \return DistFirstNeighb (float **) DistFirstNeighb[i][j] contains the 
                                      length of the segment formed by nodes
                                      SO->FN->NodeId[i] and SO->FN->FirstNeighb[i][j]
                                      
   This function was created to try and speed up SUMA_getoffsets2 but it proved
   useless.
   Sample code showing two ways of getting segment length:
   #if 1
         // calculate segment distances(a necessary horror) 
         // this made no difference in speed  
         DistFirstNeighb = SUMA_CalcNeighbDist (SO);
         if (!DistFirstNeighb) { 
            SUMA_SL_Crit("Failed to allocate for DistFirstNeighb\n");
            exit(1);
         }
         { int n1, n2, iseg;
            n1 = 5; n2 = SO->FN->FirstNeighb[n1][2];
            iseg = SUMA_FindEdge(SO->EL, n1, n2);
            fprintf(SUMA_STDERR, "%s: Distance between nodes %d and %d:\n"
                                 "from DistFirstNeighb = %f\n"
                                 "from SO->EL->Le = %f\n", FuncName, n1, n2,
                                 DistFirstNeighb[n1][2], SO->EL->Le[iseg]);
            exit(1);
         } 
   #endif
*/   
   
float ** SUMA_CalcNeighbDist (SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_CalcNeighbDist"};
   float **DistFirstNeighb=NULL, *a, *b;
   int i, j;
   static SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) { SUMA_RETURN(NULL); }
   if (!SO->FN) { SUMA_RETURN(NULL); }
   
   DistFirstNeighb = (float **)SUMA_allocate2D(SO->FN->N_Node, SO->FN->N_Neighb_max, sizeof(float));
   if (!DistFirstNeighb) {
      SUMA_SL_Crit("Failed to allocate for DistFirstNeighb");
      SUMA_RETURN(NULL);
   }
   for (i=0; i < SO->FN->N_Node; ++i) {
      a = &(SO->NodeList[3*SO->FN->NodeId[i]]);
      for (j=0; j < SO->FN->N_Neighb[i]; ++j) {
         b = &(SO->NodeList[3*SO->FN->FirstNeighb[i][j]]);
         SUMA_SEG_LENGTH(a, b, DistFirstNeighb[i][j]);
         if (SO->FN->NodeId[i] == 5 && SO->FN->FirstNeighb[i][j] == 133092) {
            fprintf (SUMA_STDERR, "%f %f %f\n%f %f %f\n%f\n", 
               SO->NodeList[3*SO->FN->NodeId[i]], SO->NodeList[3*SO->FN->NodeId[i]+1], SO->NodeList[3*SO->FN->NodeId[i]+2],
               SO->NodeList[3*SO->FN->FirstNeighb[i][j]], SO->NodeList[3*SO->FN->FirstNeighb[i][j]+1], 
               SO->NodeList[3*SO->FN->FirstNeighb[i][j]+2], DistFirstNeighb[i][j]);
         }
      }
   }
   
   SUMA_RETURN (DistFirstNeighb);
}

/*!
   \brief A function to calculate the geodesic distance of nodes connected to node n
           SUMA_getoffsets was the first incarnation but it was too slow.
    ans = SUMA_getoffsets2 (n, SO, lim, OffS, CoverThisNode, N_CoverThisNode) 
   
   \param n (int) index of center node
   \param SO (SUMA_SurfaceObject *) structure containing surface object
   \param lim (float) maximum geodesic distance to travel 
                     (ignored when CoverThisNode is used)
   \param OffS (SUMA_GET_OFFSET_STRUCT *) initialized structure to contain
          the nodes that neighbor n within lim mm 
          or until all the nodes in CoverThisNode are used up
   \param CoverThisNode (int *) SO->N_Node mask vector such that
                                 if CoverThisNode[i] then node i
                                 has to be reached (supersedes lim)
                                 NULL if you don't want to use it.
   \param N_CoverThisNode (int) number of nodes to cover (where CoverThisNode = 1).
   \return ans (SUMA_Boolean) YUP = GOOD, NOPE = BAD
   
   \sa SUMA_AddNodeToLayer
   \sa SUMA_Free_getoffsets
   \sa SUMA_Initialize_getoffsets
   \sa SUMA_getoffsets_ll



The following code was used to test different methods for calculating the segment length,
none (except for Seg = constant) proved to be faster, probably because of memory access time.
One of the options required the use of DistFirstNeighb which is calculated by function
SUMA_CalcNeighbDist. It mirrors SO->EL->FirstNeighb  

static int SEG_METHOD;
switch (SEG_METHOD) {
   case CALC_SEG: 
      // this is the slow part, too many redundant computations. 
      //cuts computation time by a factor > 3 if Seg was set to a constant
      //However, attempts at accessing pre-calculated segment lengths
      //proved to be slower. 
      SUMA_SEG_LENGTH (a, b, Seg); 
      break;
   case FIND_EDGE_MACRO:
      // this one's even slower, calculations have been made once but
      //function calls are costly (7.53 min)
      iseg = -1;
      if (n_k < n_jne) {SUMA_FIND_EDGE (SO->EL, n_k, n_jne, iseg);}
      else {SUMA_FIND_EDGE (SO->EL, n_jne, n_k, iseg);}
      if (iseg < 0) { 
         SUMA_SL_Err("Segment not found.\nSetting Seg = 10000.0");
         Seg = 10000.0; 
      } else Seg = SO->EL->Le[iseg];
      break;
   case FIND_EDGE:
      //this one's even slower, calculations have been made once but
      //function calls are costly
      iseg = SUMA_FindEdge (SO->EL, n_k, n_jne); 
      Seg = SO->EL->Le[iseg];
      break;

   case DIST_FIRST_NEIGHB:
      // consumes memory but might be faster than previous 2 (5.22 min)
      Seg = DistFirstNeighb[n_jne][k];
      break;
   case CONST:
      // 1.7 min 
      Seg = 1.0;
      break;
   default:
      SUMA_SL_Err("Bad option");
      break;
}                    
*/
SUMA_Boolean SUMA_getoffsets2 (int n, SUMA_SurfaceObject *SO, float lim, SUMA_GET_OFFSET_STRUCT *OffS, int *CoverThisNode, int N_CoverThisNode) 
{
   static char FuncName[]={"SUMA_getoffsets2"};
   int LayInd, il, n_il, n_jne, k, n_prec = -1, n_k, jne, iseg=0;
   float Off_tmp, Seg, *a, *b, minSeg, SegPres; /*! *** SegPres added Jul 08 04, ZSS bug before ... */
   SUMA_Boolean Visit = NOPE;
   SUMA_Boolean AllDone = NOPE;
   static SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!OffS) {
      SUMA_SL_Err("NULL OffS");
      SUMA_RETURN(NOPE);
   }
   
   /* setup 0th layer */
   OffS->OffVect[n] = 0.0;   /* n is at a distance 0.0 from itself */
   OffS->LayerVect[n] = 0;   /* n is on the zeroth layer */
   OffS->layers[0].N_NodesInLayer = 1;
   OffS->layers[0].NodesInLayer[0] = n;
   if (CoverThisNode) { 
      if (CoverThisNode[n]) {
         CoverThisNode[n] = 0; --N_CoverThisNode;
      }
   }
   LayInd = 1;  /* index of next layer to build */
   AllDone = NOPE;
   while (!AllDone) {
      
      AllDone = YUP; /* assume that this would be the last layer */
      for (il=0; il < OffS->layers[LayInd - 1].N_NodesInLayer; ++il) { /* go over all nodes in previous layer */
         n_il =  OffS->layers[LayInd - 1].NodesInLayer[il]; /* node from previous layer */
         for (jne=0; jne < SO->FN->N_Neighb[n_il]; ++jne) { /* go over all the neighbours of node n_il */
            n_jne = SO->FN->FirstNeighb[n_il][jne];        /* node that is an immediate neighbor to n_il */
            if (OffS->LayerVect[n_jne] < 0) { /* node is not assigned to a layer yet */
               OffS->LayerVect[n_jne] =  LayInd;    /* assign new layer index to node */
               OffS->OffVect[n_jne] = 0.0;          /* reset its distance from node n */
               SUMA_AddNodeToLayer (n_jne, LayInd, OffS);   /* add the node to the nodes in the layer */
               minSeg = 100000.0;
               n_prec = -1; 
               Seg = 0.0;
               SegPres = 0.0;
               for (k=0; k < SO->FN->N_Neighb[n_jne]; ++k) { /* calculate shortest distance of node to any precursor */  
                  n_k = SO->FN->FirstNeighb[n_jne][k];
                  if (OffS->LayerVect[n_k] == LayInd - 1) { /* this neighbor is a part of the previous layer, good */
                     if (n_prec < 0) n_prec = SO->FN->FirstNeighb[n_jne][0];
                     a = &(SO->NodeList[3*n_k]); b = &(SO->NodeList[3*n_jne]);
                     /* this is the slow part, too many redundant computations. 
                        Computation time is cut by a factor > 2 if Seg was set to a constant
                        However, attempts at accessing pre-calculated segment lengths
                        proved to be slower. See Comments in function help*/
                     SUMA_SEG_LENGTH_SQ (a, b, Seg);                    
                     if (OffS->OffVect[n_prec] + Seg < minSeg) {
                        minSeg = Seg + OffS->OffVect[n_prec];
                        SegPres = Seg;
                        n_prec = n_k;
                     }
                  }
               }/* for k */
               
               if (n_prec < 0) { /* bad news */
                  SUMA_SL_Crit("No precursor found for node.");
                  OffS = SUMA_Free_getoffsets (OffS);
                  SUMA_RETURN(NOPE);
               } else {
                  OffS->OffVect[n_jne] = OffS->OffVect[n_prec] + sqrt(SegPres); SegPres = 0.0;
                  if (!CoverThisNode) {
                     if (OffS->OffVect[n_jne] < lim) { /* must go at least one more layer */
                        AllDone = NOPE;
                     }
                  } else {
                     if (CoverThisNode[n_jne]) {
                        CoverThisNode[n_jne] = 0; --N_CoverThisNode;
                     }
                     if (N_CoverThisNode > 0) {
                        AllDone = NOPE;
                     }
                  }
               }
            } /* node not already in layer */
            
         } /* for jne */
      
      } /* for il */    
      ++LayInd;
   } /* while AllDone */
   
   SUMA_RETURN(YUP);
}

void SUMA_Free_Offset_ll_Datum(void *data)
{
   static char FuncName[]={"SUMA_Free_Offset_ll_Datum"};
   SUMA_OFFSET_LL_DATUM *dt;
   
   SUMA_ENTRY;
   
   if (data) {
      dt = (SUMA_OFFSET_LL_DATUM *)data; 
      SUMA_free(dt);
   }
   
   SUMA_RETURNe;
}   

SUMA_OFFSET_LL_DATUM *SUMA_New_Offset_ll_Datum(int n, int layer)
{
   static char FuncName[]={"SUMA_New_Offset_ll_Datum"};
   SUMA_OFFSET_LL_DATUM * datum = NULL;
   
   SUMA_ENTRY;
   
   datum = (SUMA_OFFSET_LL_DATUM *)SUMA_malloc(sizeof(SUMA_OFFSET_LL_DATUM));
   datum->ni = n;
   datum->layer = layer;
   datum->off = -1.0;
   
   SUMA_RETURN(datum);
}
#define SUMA_BEGINNING_OF_LAYER(list, LayInd, Elm) {   \
   SUMA_OFFSET_LL_DATUM * m_dat = NULL; \
   DListElmt *m_Elm = NULL;   \
   do {  \
     if (m_Elm) m_Elm = m_Elm->next;   \
     else m_Elm =  dlist_head(list); \
     m_dat = (SUMA_OFFSET_LL_DATUM *)m_Elm->data; \
   } while(m_dat->layer != LayInd && m_Elm != dlist_tail(list));   \
   if (m_dat->layer != LayInd) Elm = NULL;  \
   else Elm = m_Elm; \
} 
#define SUMA_FIND_ELMENT_FOR_NODE(list, n_jne, Elm){  \
   SUMA_OFFSET_LL_DATUM * m_dat = NULL; \
   DListElmt *m_Elm = NULL;   \
   do {  \
     if (m_Elm) m_Elm = m_Elm->next;   \
     else m_Elm =  dlist_head(list); \
     m_dat = (SUMA_OFFSET_LL_DATUM *)m_Elm->data; \
   } while(m_dat->ni != n_jne && m_Elm != dlist_tail(list));   \
   if (m_dat->ni != n_jne) Elm = NULL;  \
   else Elm = m_Elm; \
}
DList * SUMA_getoffsets_ll (int n, SUMA_SurfaceObject *SO, float lim, int *CoverThisNode, int N_CoverThisNode) 
{
   static char FuncName[]={"SUMA_getoffsets_ll"};
   int LayInd, il, n_il, n_jne, k, n_prec = -1, n_k, jne, iseg=0;
   float Off_tmp, Seg, *a, *b, minSeg, SegPres; /*! *** SegPres added Jul 08 04, ZSS bug before ... */
   SUMA_Boolean Visit = NOPE;
   SUMA_Boolean AllDone = NOPE;
   SUMA_OFFSET_LL_DATUM * n_dat = NULL, *dat = NULL, *dat_nk = NULL, *dat_prec = NULL, *dat_ne=NULL;
   DList *list = NULL;
   DListElmt *elm = NULL, *elm_prec = NULL, *elm_ne=NULL, *elm_nk=NULL;
   static SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   /* create the list */
   SUMA_LH("Initializing list ...");
   list = (DList *)SUMA_malloc(sizeof(DList));
   dlist_init(list, SUMA_Free_Offset_ll_Datum);
   
   /* setup 0th layer */
   SUMA_LH("New OffsetDatum");
   n_dat = SUMA_New_Offset_ll_Datum(n, 0);
   n_dat->off = 0.0;   /* n is at a distance 0.0 from itself */
   dlist_ins_next(list, dlist_tail(list), (void*)n_dat);
   
   if (CoverThisNode) { 
      if (CoverThisNode[n]) {
         CoverThisNode[n] = 0; --N_CoverThisNode;
      }
   }
   LayInd = 1;  /* index of next layer to build */
   AllDone = NOPE;
   while (!AllDone) {
      AllDone = YUP; /* assume that this would be the last layer */
      elm = NULL;
         do {
            if (!elm) { SUMA_BEGINNING_OF_LAYER(list, (LayInd-1), elm); }
            else elm = elm->next;
            if (!elm) {
               SUMA_SL_Err("Could not find beginning of layer!");
               SUMA_RETURN(NULL);
            }
            dat = (SUMA_OFFSET_LL_DATUM *)elm->data;
            if (dat->layer == LayInd -1) {
               n_il = dat->ni;
               for (jne=0; jne < SO->FN->N_Neighb[n_il]; ++jne) { /* go over all the neighbours of node n_il */
                  n_jne = SO->FN->FirstNeighb[n_il][jne];        /* node that is an immediate neighbor to n_il */
                  SUMA_FIND_ELMENT_FOR_NODE(list, n_jne, elm_ne);
                  if (!elm_ne) { /* node not in any layer */
                     dat_ne = SUMA_New_Offset_ll_Datum(n_jne, LayInd); /* create an element for it */
                     dat_ne->off = 0.0;
                     dlist_ins_next(list, dlist_tail(list), (void*)dat_ne);
                     minSeg = 100000.0;
                     n_prec = -1; 
                     Seg = 0.0;
                     SegPres = 0.0;
                     for (k=0; k < SO->FN->N_Neighb[n_jne]; ++k) { /* calculate shortest distance of node to any precursor */  
                        n_k = SO->FN->FirstNeighb[n_jne][k];
                        SUMA_FIND_ELMENT_FOR_NODE(list, n_k, elm_nk); 
                        if (n_prec < 0 && elm_nk) { 
                           n_prec = n_k; elm_prec = elm_nk; 
                           dat_prec = (SUMA_OFFSET_LL_DATUM *)elm_prec->data;
                        }
                        if (elm_nk) {
                           dat_nk = (SUMA_OFFSET_LL_DATUM *)elm_nk->data;
                           if (dat_nk->layer == LayInd - 1) { /* this neighbor is a part of the previous layer, good */
                              a = &(SO->NodeList[3*n_k]); b = &(SO->NodeList[3*n_jne]);
                              /* this is the slow part, too many redundant computations. 
                                 Computation time is cut by a factor > 2 if Seg was set to a constant
                                 However, attempts at accessing pre-calculated segment lengths
                                 proved to be slower. See Comments in function help*/
                              SUMA_SEG_LENGTH_SQ (a, b, Seg);                    
                              if (dat_prec->off + Seg < minSeg) {
                                 minSeg = Seg + dat_prec->off;
                                 SegPres = Seg;
                                 n_prec = n_k;
                                 elm_prec = elm_nk;
                                 dat_prec = dat_nk;
                              }
                           }
                        } /* if elm_nk */
                     }/* for k */
                     if (n_prec < 0) { /* bad news */
                        SUMA_SL_Crit("No precursor found for node.");
                        SUMA_RETURN(NULL);
                     } else {
                        dat_ne->off = dat_prec->off + sqrt(SegPres); SegPres = 0.0;
                        if (!CoverThisNode) {
                           if (dat_ne->off < lim) { /* must go at least one more layer */
                              AllDone = NOPE;
                           }
                        } else {
                           if (CoverThisNode[n_jne]) {
                              CoverThisNode[n_jne] = 0; --N_CoverThisNode;
                           }
                           if (N_CoverThisNode > 0) {
                              AllDone = NOPE;
                           }
                        }
                     }
                  } /* if elm_ne */
               } /* for jne */
            } /* dat->layer == LayInd */
         }  while (dat->layer == (LayInd-1) && elm != dlist_tail(list));
      
      ++LayInd;
   } /* while AllDone */
   
   SUMA_RETURN(list);
}

typedef struct {
   SUMA_SurfaceObject *SO;
   SUMA_SurfaceObject *SOref;
   SUMA_COMM_STRUCT *cs;
   double Vref;
   double Rref;
   double V;
   double R;
   float *tmpList;
} SUMA_VolDiffDataStruct; /*!< a special struct for the functions to equate the volume of two surfaces */
typedef struct {
   SUMA_SurfaceObject *SO;
   SUMA_SurfaceObject *SOref;
   SUMA_COMM_STRUCT *cs;
   double Aref;
   double Rref;
   double A;
   double R;
   float *tmpList;
} SUMA_AreaDiffDataStruct; /*!< a special struct for the functions to equate the area of two surfaces */

/*!
   \brief Changes the coordinates of SO's nodes so that the new average radius of the surface
   is equal to r
   
   This function is an integral part of the function for equating the areas of 2 surfaces.
   Nodes are stretched by a fraction equal to:
      (Rref - r) / Rref * Un where Un is the distance of the node from the center of the surface
      Rref is the reference radius, r is the desired radius
   \param SO (SUMA_SurfaceObject *) Surface object, obviously
   \param r (double) (see above)
   \param Rref (double) (see above)
   \pram tmpList (float *) a pre-allocated vector to contain the new coordinates of the surface 
   \return A (double) the area of the new surface (post streching)
   \sa SUMA_AreaDiff
*/
double SUMA_NewAreaAtRadius(SUMA_SurfaceObject *SO, double r, 
                           double Rref, float *tmpList)
{
   static char FuncName[]={"SUMA_NewAreaAtRadius"};
   double Dr, A=0.0,  Un, U[3], Dn, P2[2][3], c[3];
   float *fp;
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* calculate Dr and normalize by the radius of SOref */
   Dr = ( r - Rref ) / Rref;
   /* Now loop over all the nodes in SO and add the deal */
   for (i=0; i<SO->N_Node; ++i) {
      /* change node coordinate of each node by Dr, along radial direction  */
      fp = &(SO->NodeList[3*i]); 
      SUMA_UNIT_VEC(SO->Center, fp, U, Un);
      Dn = Dr*Un + Un;
      if (Un) {
         SUMA_COPY_VEC(SO->Center, c, 3, float, double);
         SUMA_POINT_AT_DISTANCE_NORM(U, c, Dn, P2);
         tmpList[3*i  ] = (float)P2[0][0]; 
         tmpList[3*i+1] = (float)P2[0][1]; 
         tmpList[3*i+2] = (float)P2[0][2];
      } else {
         SUMA_SL_Err("Identical points!\n"
                     "No coordinates modified");
         SUMA_RETURN(0);
      }
   }


   /* calculate the new Area */
   fp = SO->NodeList;/* save NodeList */
   SO->NodeList = tmpList; /* use new coordinates */
   A = fabs((double)SUMA_Mesh_Area(SO, NULL, -1));
   SO->NodeList = fp; fp = NULL;   /* make NodeList point to the original data */

   SUMA_LHv("Old: Rref=%.4f \n"
            "New:    r=%.4f, Area=%.4f\n", Rref, r, A);
   SUMA_RETURN(A);
} 
/*!
   \brief Change the coordinates so that the new surface has a radius r
   \param SO: La surface
   \param r: Le radius
   \param Center: If not NULL then a 3x1 vector containing the Center of SO
                  Use this when SO->Center is not OK for some reason. Else, SO->Center
                  is used.
*/
SUMA_Boolean SUMA_NewSurfaceRadius(SUMA_SurfaceObject *SO, double r, float *Center)
{
   static char FuncName[]={"SUMA_NewSurfaceRadius"};
   double Un, U[3], Dn, P2[2][3], c[3];
   float *fp;
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SO || !SO->NodeList) { SUMA_S_Err("Imbecile!"); SUMA_RETURN(NOPE); }
   if (!Center) Center = SO->Center;
   
   /* Now loop over all the nodes in SO and add the deal */
   for (i=0; i<SO->N_Node; ++i) {
      /* change node coordinate of each node by Dr, along radial direction  */
      fp = &(SO->NodeList[3*i]); SUMA_UNIT_VEC(Center, fp, U, Un);
      if (Un) {
         SUMA_COPY_VEC(Center, c, 3, float, double);
         SUMA_POINT_AT_DISTANCE_NORM(U, c, r, P2);
         SO->NodeList[3*i] = (float)P2[0][0]; SO->NodeList[3*i+1] = (float)P2[0][1]; SO->NodeList[3*i+2] = (float)P2[0][2];
      } else {
         SUMA_SL_Err("Identical points!\n"
                     "No coordinates modified");
      }
   }

   SUMA_RETURN(YUP);
}

/*!
   \brief Changes the coordinates of SO's nodes so that the new average radius of the surface
   is equal to r
   
   This function is an integral part of the function for equating the volumes of 2 surfaces.
   Nodes are stretched by a fraction equal to:
      (Rref - r) / Rref * Un where Un is the distance of the node from the center of the surface
      Rref is the reference radius, r is the desired radius
   \param SO (SUMA_SurfaceObject *) Surface object, obviously
   \param r (double) (see above)
   \param Rref (double) (see above)
   \pram tmpList (float *) a pre-allocated vector to contain the new coordinates of the surface 
   \return V (double) the volume of the new surface (post streching)
   \sa SUMA_VolDiff
*/
double SUMA_NewVolumeAtRadius(SUMA_SurfaceObject *SO, double r, double Rref, float *tmpList)
{
   static char FuncName[]={"SUMA_NewVolumeAtRadius"};
   double Dr, V=0.0,  Un, U[3], Dn, P2[2][3], c[3];
   float *fp;
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* calculate Dr and normalize by the radius of SOref */
   Dr = ( r - Rref ) / Rref;

   /* Now loop over all the nodes in SO and add the deal */
   for (i=0; i<SO->N_Node; ++i) {
      /* change node coordinate of each node by Dr, along radial direction  */
      fp = &(SO->NodeList[3*i]); SUMA_UNIT_VEC(SO->Center, fp, U, Un);
      Dn = Dr*Un + Un;
      if (Un) {
         SUMA_COPY_VEC(SO->Center, c, 3, float, double);
         SUMA_POINT_AT_DISTANCE_NORM(U, c, Dn, P2);
         tmpList[3*i] = (float)P2[0][0]; tmpList[3*i+1] = (float)P2[0][1]; tmpList[3*i+2] = (float)P2[0][2];
      } else {
         SUMA_SL_Err("Identical points!\n"
                     "No coordinates modified");
         SUMA_RETURN(0);
      }
   }


   /* calculate the new volume */
   fp = SO->NodeList;/* save NodeList */
   SO->NodeList = tmpList; /* use new coordinates */
   V = fabs((double)SUMA_Mesh_Volume(SO, NULL, -1));
   SO->NodeList = fp; fp = NULL;   /* make NodeList point to the original data */

   SUMA_RETURN(V);
} 

double SUMA_AreaDiff(double r, void *fvdata)
{
   static char FuncName[]={"SUMA_AreaDiff"};
   double da, *fp, Dr, A;
   static int ncall=0;
   int i;
   static double Rref = 0.0, Aref = 0.0;
   SUMA_SurfaceObject *SO, *SOref;
   SUMA_COMM_STRUCT *cs=NULL;
   SUMA_AreaDiffDataStruct *fdata = (SUMA_AreaDiffDataStruct*)fvdata ;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!fdata) {
      SUMA_LH("Reset");
      Rref = 0.0; Aref = 0.0;
      ncall = 0;
      SUMA_RETURN(0.0);
   }
   
   SO = fdata->SO;
   SOref = fdata->SOref;
   cs = fdata->cs;
   
   if (!ncall) {
      SUMA_LH("Initializing, calculating Aref and Rref");
      Aref = fdata->Aref;
      Rref = fdata->Rref;
      if (LocalHead) { 
         fprintf(SUMA_STDERR,"%s: Reference area = %f, radius = %f \n",
                        FuncName, Aref, Rref); }
      if (cs->Send) { /* send the first monster 
                     ( it's SOref "in SUMA" that's being modified on the fly) */
         if (!SUMA_SendToSuma (  SOref, cs, (void *)SO->NodeList, 
                                 SUMA_NODE_XYZ, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
   }
   
   A = SUMA_NewAreaAtRadius(SO, r, Rref, fdata->tmpList);
   da = A - Aref; /* the area difference */
   if (LocalHead) {
      fprintf(SUMA_STDERR,
               "%s: Call %d, A = %f, Aref = %f, da = %f\n", 
               FuncName,  ncall, A, Aref, da);
      fprintf(SUMA_STDERR, "SOref->idcode_str=%s\n", SOref->idcode_str);
   }
      
   /* need an update ? */
   if (cs->Send) { /* send the update 
                     (it's SOref "in SUMA" that's being modified on the fly) */
      if (!SUMA_SendToSuma (SOref, cs, (void *)fdata->tmpList, 
                            SUMA_NODE_XYZ, 1)) {
      SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
   }

   ++ncall;
   
   SUMA_RETURN(da);
}

double SUMA_VolDiff(double r, void *fvdata)
{
   static char FuncName[]={"SUMA_VolDiff"};
   double dv, *fp, Dr, V;
   static int ncall=0;
   int i;
   static double Rref = 0.0, Vref = 0.0;
   SUMA_SurfaceObject *SO, *SOref;
   SUMA_COMM_STRUCT *cs=NULL;
   SUMA_VolDiffDataStruct *fdata = (SUMA_VolDiffDataStruct*)fvdata ;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!fdata) {
      SUMA_LH("Reset");
      Rref = 0.0; Vref = 0.0;
      ncall = 0;
      SUMA_RETURN(0.0);
   }
   
   SO = fdata->SO;
   SOref = fdata->SOref;
   cs = fdata->cs;
   
   if (!ncall) {
      SUMA_LH("Initializing, calculating Vref and Rref");
      Vref = fdata->Vref;
      Rref = fdata->Rref;
      if (LocalHead) { fprintf(SUMA_STDERR,"%s: Reference volume = %f, radius = %f \n", FuncName, Vref, Rref); }
      if (cs->Send) { /* send the first monster (it's SOref "in SUMA" that's being modified on the fly) */
         if (!SUMA_SendToSuma (SOref, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
   }
   
   V = SUMA_NewVolumeAtRadius(SO, r, Rref, fdata->tmpList);
   dv = V-Vref; /* the volume difference */
      
   /* need an update ? */
   if (cs->Send) { /* send the update (it's SOref "in SUMA" that's being modified on the fly) */
      if (!SUMA_SendToSuma (SOref, cs, (void *)fdata->tmpList, SUMA_NODE_XYZ, 1)) {
      SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
   }

   ++ncall;
   
   SUMA_RETURN(dv);
}
 
/*! \brief Binary Zero Search, a function to find the zero of a function 
   \param a (double) 1st point, f(a) < 0
   \param b (double) 2nd point, f(b) > 0 (actually all you need is that f(a)*f(b) is < 0
   \param *f (double )(double x, void *data) function to find the zero point (at the right x)
   \param fdata(void *)a pointer to the data that accompanies x as input to f
   \param Nitermax (int) the maximum number of iterations
   \param tol(double) the tolerance for convergence. Stop when ( |f(x)| < tol ) 
*/
double SUMA_BinaryZeroSearch( double a, double b, 
                              double(*f)(double x, void *data), 
                              void *fdata, int Nitermax, double tol) {
   static char FuncName[]={"SUMA_BinaryZeroSearch"};
   int Niter;
   double x, fx;
   SUMA_Boolean done;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (Nitermax < 0) Nitermax = 1000;

   x = 0.0;
   Niter = 0;
   done = NOPE;
   while(!done && Niter < Nitermax) {
      x = (a+b)/2.0;
      fx = (*f)(x, fdata);
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: %d\ta=%.4f\tb=%.4f\tx=%.4f\tfx=%.4f\n", 
                             FuncName, Niter, a, b, x, fx);
      if (fx < 0) a = x;
      else b = x;
      if (fabs(fx) < tol) done = YUP;
      ++Niter;
   }
   
   /* Now do a cleanup call */
   fx = (*f)(x, NULL);
   
   if (!done) {
      SUMA_SL_Warn(  "Reached iteration limit\n"
                     "without converging.\n");
   }
   
   SUMA_RETURN(x);
}


/*
#define FROM_THIS_NODE 0
#define TO_THIS_NODE 10
*/
/*!
   \brief a function to find two values a and b such that
   DA(a) is < 0 and DA(b) is > 0
   These two starting points are used for the optimization function
   SUMA_BinaryZeroSearch
*/
SUMA_Boolean SUMA_GetAreaDiffRange(
      SUMA_AreaDiffDataStruct *fdata, double *ap, double *bp)
{
   static char FuncName[]={"SUMA_GetAreaDiffRange"};
   double a = 0.0, b = 0.0, nat=0, nbt=0, An, Bn;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* decide on segment range */
   fdata->Aref = fabs((double)SUMA_Mesh_Area(fdata->SOref, NULL, -1));
   SUMA_SO_RADIUS(fdata->SOref, fdata->Rref);
   fdata->A = fabs((double)SUMA_Mesh_Area(fdata->SO, NULL, -1));
   SUMA_SO_RADIUS(fdata->SO, fdata->R);

   if (fdata->Aref > fdata->A) { /* current settings low end, 
                                    acceptable for a */
      a = fdata->R; 
      An = fdata->A;
      /* now find b such that area at b is larger than Aref */
      b = fdata->Rref;
      do {
         b *= 1.1;
         Bn = SUMA_NewAreaAtRadius(fdata->SO, b, fdata->Rref, fdata->tmpList);
         ++nbt;
      } while ( fdata->Aref > Bn && nbt < 200); /* stop when area at B is 
                                                   larger than Aref */
   } else { /* current settings high end, acceptable for b */
      b = fdata->R;
      Bn = fdata->A;
      /* now find a such that area at a is less than Aref */
      a = fdata->Rref;
      do {
         a *= 0.9;
         An = SUMA_NewAreaAtRadius(fdata->SO, a, fdata->Rref, fdata->tmpList);
         ++nat;
      } while ( fdata->Aref < An && nat < 200); /* stop when area at A is
                                                   smaller than Aref */
   
   }
   *ap = a; *bp = b;

   if (nat >= 200 || nbt >= 200) {
      SUMA_SL_Err("Failed to find segment.");
      SUMA_RETURN(NOPE);
   }

   if (LocalHead) {
      fprintf (SUMA_STDERR,
         "%s:\nChosen range is [%f %f] with Areas [%f %f]\n"
         "             , reference Area is %f\n", 
            FuncName, a, b, An, Bn, fdata->Aref);
   }
   
   SUMA_RETURN(YUP); 
}
/*!
   \brief a function to find two values a and b such that
   DV(a) is < 0 and DV(b) is > 0
   These two starting points are used for the optimization function
   SUMA_BinaryZeroSearch
*/
SUMA_Boolean SUMA_GetVolDiffRange(SUMA_VolDiffDataStruct *fdata, double *ap, double *bp)
{
   static char FuncName[]={"SUMA_GetVolDiffRange"};
   double a = 0.0, b = 0.0, nat=0, nbt=0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* decide on segment range */
   fdata->Vref = fabs((double)SUMA_Mesh_Volume(fdata->SOref, NULL, -1));
   SUMA_SO_RADIUS(fdata->SOref, fdata->Rref);
   fdata->V = fabs((double)SUMA_Mesh_Volume(fdata->SO, NULL, -1));
   SUMA_SO_RADIUS(fdata->SO, fdata->R);

   /* a very simple range setting. might very well fail at times */
   if (fdata->Vref > fdata->V) { /* current settings low end, acceptable for a */ 
      a = fdata->R; 
      b = fdata->Rref;/* now find b such that volume at b is larger than Vref */
      do {
         SUMA_LH("Looking for b");
         b *= 1.1; ++nbt; 
      } while ( fdata->Vref > SUMA_NewVolumeAtRadius(fdata->SO, b, fdata->Rref, fdata->tmpList) && nbt < 200);/* stop when volume  at B is larger than Vref */
   }else{ /* current settings high end, acceptable for b */ 
      b = fdata->R; 
      a = fdata->Rref;/* now find a such that volume at a is less than Vref */
      do {
         SUMA_LH("Looking for a");
         a *= 0.9; ++nat;
      } while ( fdata->Vref < SUMA_NewVolumeAtRadius(fdata->SO, a, fdata->Rref, fdata->tmpList) && nat < 200); /* stop when volume  at A is smaller than Vref */
   }

   *ap = a; *bp = b;

   if (nat >= 200 || nbt >= 200) {
      SUMA_SL_Err("Failed to find segment.");
      SUMA_RETURN(NOPE);
   }

   SUMA_RETURN(YUP); 
}

/*!
   \brief inflates or deflates a surface to make the area of one surface (SO) equal to the area of another (SOref)
   \param SO: The surface to modify. SO's NodeList pointer is reallocated in the function!
   \param SOref: The reference surface
   \param tol (float): The acceptable difference between the two areas
   \param cs (SUMA_COMM_STRUCT *): The suma communication structure
   
   - This function does not update the normals and other coordinate related properties for SO.
   \sa SUMA_RECOMPUTE_NORMALS 
*/
SUMA_Boolean SUMA_EquateSurfaceAreas(
                  SUMA_SurfaceObject *SO, 
                  SUMA_SurfaceObject *SOref, 
                  float tol, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_EquateSurfaceAreas"};
   int iter, i, iter_max, ndiv;
   double a, b, d;
   SUMA_AreaDiffDataStruct fdata;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO || !SOref) { SUMA_SL_Err("NULL surfaces"); SUMA_RETURN(NOPE); }
   if (  SO->N_Node != SOref->N_Node 
      || SO->N_FaceSet != SOref->N_FaceSet) { 
         SUMA_SL_Err("Surfaces not isotopic"); SUMA_RETURN(NOPE); 
   }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           " SO    Center: %f, %f, %f\n"
                           " SOref Center: %f, %f, %f\n"
                           , FuncName, 
                           SO->Center[0], SO->Center[1], SO->Center[2],
                           SOref->Center[0], SOref->Center[1],
                           SOref->Center[2]);  
   }
     
   /* fill up fdata */
   fdata.SO = SO; fdata.SOref = SOref; fdata.cs = cs;
   fdata.tmpList = (float *)SUMA_malloc(
                              SOref->NodeDim * SOref->N_Node * sizeof(float));
   if (!fdata.tmpList) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(0);
   }

   if (!SUMA_GetAreaDiffRange(&fdata, &a, &b)) {
      SUMA_SL_Err("Failed to get range");
      SUMA_RETURN(NOPE);
   }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\na = %f\tb=%f\n", FuncName, a, b);
   }      
   SUMA_BinaryZeroSearch(a, b, SUMA_AreaDiff, &fdata, 500, tol);  
   
   /* now make the new node list be SO's thingy*/
   SUMA_free(SO->NodeList); SO->NodeList = fdata.tmpList; fdata.tmpList = NULL;
       
   SUMA_RETURN(YUP);
}

/*!
   \brief inflates or deflates a surface to make the volume of one surface (SO) equal to the volume of another (SOref)
   \param SO: The surface to modify
   \param SOref: The reference surface
   \param tol (float): The acceptable difference between the two volumes
   \param cs (SUMA_COMM_STRUCT *): The suma communication structure
   
   - This function does not update the normals and other coordinate related properties for SO.
   \sa SUMA_RECOMPUTE_NORMALS 
*/
SUMA_Boolean SUMA_EquateSurfaceVolumes(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float tol, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_EquateSurfaceVolumes"};
   int iter, i, iter_max, ndiv;
   double a, b, d;
   SUMA_VolDiffDataStruct fdata;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO || !SOref) { SUMA_SL_Err("NULL surfaces"); SUMA_RETURN(NOPE); }
   if (SO->N_Node != SOref->N_Node || SO->N_FaceSet != SOref->N_FaceSet) { SUMA_SL_Err("Surfaces not isotopic"); SUMA_RETURN(NOPE); }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           " SO    Center: %f, %f, %f\n"
                           " SOref Center: %f, %f, %f\n"
                           , FuncName, 
                           SO->Center[0], SO->Center[1], SO->Center[2],
                           SOref->Center[0], SOref->Center[1], SOref->Center[2]);  
   }
     
   /* fill up fdata */
   fdata.SO = SO; fdata.SOref = SOref; fdata.cs = cs;
   fdata.tmpList = (float *)SUMA_malloc(SOref->NodeDim * SOref->N_Node * sizeof(float));
   if (!fdata.tmpList) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(0);
   }

   if (!SUMA_GetVolDiffRange(&fdata, &a, &b)) {
      SUMA_SL_Err("Failed to get range");
      SUMA_RETURN(NOPE);
   }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\na = %f\tb=%f\n", FuncName, a, b);
   }      
   SUMA_BinaryZeroSearch(a, b, SUMA_VolDiff, &fdata, 500, tol);  
   
   /* now make the new node list be SO's thingy*/
   SUMA_free(SO->NodeList); SO->NodeList = fdata.tmpList; fdata.tmpList = NULL;
       
   SUMA_RETURN(YUP);
}

/*!
   \brief stretch each node along the center--node direction such that the new distance is = radius
   \param SO The surface to be modified.
          Adjust node coordinates of SO so that
          Node i on SO is repositioned such 
          that |c i| = radius
          c is the center of SO, calculated as the average coordinate.
   \param SOref reference SurfaceObject, used to communicate with SUMA 
   \param radius , you know what.
   \param cs the famed communication structure
   
   \sa SUMA_ProjectToSphere
*/
/*
#define FROM_THIS_NODE 0
#define TO_THIS_NODE 10
*/
SUMA_Boolean SUMA_ProjectSurfaceToSphere(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref ,float radius, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_ProjectSurfaceToSphere"};
   int i=0, j=0, cnt = 0, istrt, istp;
   struct timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all, ave_dist= 0.0, dj = 0.0, ave_dist_ref= 0.0, *a=NULL;
   float P2[2][3], U[3], Un;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO || (cs && !SOref)) { SUMA_SL_Err("NULL surface"); SUMA_RETURN(NOPE); }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           " SO    Center: %f, %f, %f\n"
                           " radius = %f\n", FuncName, 
                           SO->Center[0], SO->Center[1], SO->Center[2],
                           radius);  
   }
      
   #ifdef FROM_THIS_NODE
   istrt = FROM_THIS_NODE;
   istp = TO_THIS_NODE+1;
   #else
   istrt = 0;
   istp = SO->N_Node;
   #endif
   ave_dist_ref =  radius;
   for (i =istrt ; i<istp; ++i) {
      if (i == 0) {
         SUMA_etime(&start_time,0);
      }
      /* move node i to the reference average location
      Do not travel along normals, you should travel along
      radial direction Center-->node*/
      a = &(SO->NodeList[3*i]); SUMA_UNIT_VEC(SO->Center, a, U, Un);
      if (Un) {
         SUMA_POINT_AT_DISTANCE_NORM(U, SO->Center, ave_dist_ref, P2);
         SO->NodeList[3*i] = P2[0][0]; SO->NodeList[3*i+1] = P2[0][1]; SO->NodeList[3*i+2] = P2[0][2];
      } else {
            SUMA_SL_Err("Identical points!\n"
                        "No coordinates modified");
      }
      
      if (LocalHead) {
         if (! (i%999)) {
            a = &(SO->NodeList[3*i]);
            SUMA_SEG_LENGTH(a, SO->Center, dj);
            fprintf(SUMA_STDERR, "%s:\n"
                           "node i=%d, avg_dist_ref = %f\ncnt = %d\n"
                           "Check on P2: New dist =%f ?=? %f\n", 
                           FuncName, i, ave_dist_ref, cnt, dj, ave_dist_ref);
            etime_GetOffset = SUMA_etime(&start_time,1);
            fprintf(SUMA_STDERR, "%s: Search to %f mm took %f seconds for %d nodes.\n"
                  "Projected completion time: %f minutes\n",
                  FuncName, radius, etime_GetOffset, i+1,
                  etime_GetOffset * SO->N_Node / 60.0 / (i+1));
         }
      }
      if (! (i%99) && cs) {
         if (cs->Send) { /* send the first monster (it's SOref  "in SUMA" that's being modified on the fly*/
            if (!SUMA_SendToSuma (SOref, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }
      }
      
      #ifdef FROM_THIS_NODE
      {
         FILE *fid=NULL;
         char *outname=NULL, tmp[20];
         int ii;
         if (cs->Send) { /* send the first monster (it's SOref that's being modified on the fly*/
            if (!SUMA_SendToSuma (SOref, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }
         sprintf(tmp,"offset_n%d", FROM_THIS_NODE);
         outname = SUMA_Extension("", ".1D", YUP);
         outname = SUMA_append_replace_string(outname, "offset.1D", "", 1);
         fid = fopen(outname, "w"); free(outname); outname = NULL;
         if (!fid) {
            SUMA_SL_Err("Could not open file for writing.\nCheck file permissions, disk space.\n");
         } else {
            fprintf (fid,"#Column 1 = Node index\n"
                         "#column 2 = Neighborhood layer\n"
                         "#Column 3 = Distance from node %d\n", 99);
            for (ii=0; ii<SO->N_Node; ++ii) {
               if (OffS->LayerVect[ii] >= 0) {
                  fprintf(fid,"%d\t%d\t%f\n", ii, OffS->LayerVect[ii], OffS->OffVect[ii]);
               }
            }
            fclose(fid);
         }
         { int jnk; fprintf(SUMA_STDOUT,"Pausing, next node is %d...", i+1); jnk = getchar(); fprintf(SUMA_STDOUT,"\n"); }
      }
      #endif
             
      
   }   
   SO->isSphere = SUMA_GEOM_SPHERE;
   SO->SphereRadius = radius;
   SUMA_COPY_VEC(SO->Center, SO->SphereCenter, 3, float, float);

   
   SUMA_RETURN(YUP);
}

/*!
   \brief make the size of 2 surfaces match see help -match_size option in SurfSmooth
   \param SO The surface to be modified.
          Adjust node coordinates of SO so that
          it matches the original size.
          Node i on SO is repositioned such 
          that |c i| = 1/N sum(|cr j|) where
          c and cr are the centers of SO and SOref, respectively.
          N is the number of nodes that are within max_off along
          the surface (geodesic) from node i.
          j is one of the nodes neighboring i.
   \param SOref The surface to be matched
   \param max_off geodesic neighborhood to search around i
   \param cs the famed communication structure
*/
/*
#define FROM_THIS_NODE 0
#define TO_THIS_NODE 10
*/
SUMA_Boolean SUMA_EquateSurfaceSize(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float max_off, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_EquateSurfaceSize"};
   int i=0, j=0, cnt = 0, istrt, istp;
   struct timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all, ave_dist= 0.0, dj = 0.0, ave_dist_ref= 0.0, *a=NULL;
   float P2[2][3], U[3], Un;
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO || !SOref) { SUMA_SL_Err("NULL surfaces"); SUMA_RETURN(NOPE); }
   if (SO->N_Node != SOref->N_Node || SO->N_FaceSet != SOref->N_FaceSet) { SUMA_SL_Err("Surfaces not isotopic"); SUMA_RETURN(NOPE); }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           " SO    Center: %f, %f, %f\n"
                           " SOref Center: %f, %f, %f\n"
                           " max_off = %f\n", FuncName, 
                           SO->Center[0], SO->Center[1], SO->Center[2],
                           SOref->Center[0], SOref->Center[1], SOref->Center[2],
                           max_off);  
   }
      
   OffS = SUMA_Initialize_getoffsets (SOref->N_Node);
   #ifdef FROM_THIS_NODE
   istrt = FROM_THIS_NODE;
   istp = TO_THIS_NODE+1;
   #else
   istrt = 0;
   istp = SOref->N_Node;
   #endif
   for (i =istrt ; i<istp; ++i) {
      if (i == 0) {
         SUMA_etime(&start_time,0);
      }
      SUMA_getoffsets2 (i, SOref, max_off, OffS, NULL, 0);
      /* find average distance between nodes within offset and center of surface */
      a = &(SOref->NodeList[3*i]); SUMA_SEG_LENGTH(a, SOref->Center, ave_dist_ref); 
      cnt = 1;
      #ifdef FROM_THIS_NODE
            fprintf(SUMA_STDERR, "%s: Considering the following %d neighbors to:\n"
                                 "i=%d; [%f, %f, %f]; d[%d] = %f\n", FuncName, OffS->N_Nodes,
                                    i, SOref->NodeList[3*i], SOref->NodeList[3*i+1], SOref->NodeList[3*i+2], 
                                    cnt, ave_dist_ref);
      #endif
      for (j=0; j<OffS->N_Nodes; ++j)
      {
         
         if (i!=j && OffS->LayerVect[j] >= 0 && OffS->OffVect[j] <= max_off)
         {
            
            a = &(SOref->NodeList[3*j]); SUMA_SEG_LENGTH(a, SOref->Center, dj);
            ave_dist_ref += dj;
            ++cnt;
            #ifdef FROM_THIS_NODE
               fprintf(SUMA_STDERR, ""
                                 "j=%d; [%f, %f, %f]; d[%d] = %f\n",
                                    j, SOref->NodeList[3*j], SOref->NodeList[3*j+1], SOref->NodeList[3*j+2], 
                                    cnt, dj);
            #endif
         }
      }
      ave_dist_ref /=  (float)cnt;
      /* move node i to the reference average location
      Do not travel along normals, you should travel along
      radial direction Center-->node*/
      a = &(SO->NodeList[3*i]); SUMA_UNIT_VEC(SO->Center, a, U, Un);
      if (Un) {
         SUMA_POINT_AT_DISTANCE_NORM(U, SO->Center, ave_dist_ref, P2);
         SO->NodeList[3*i] = P2[0][0]; SO->NodeList[3*i+1] = P2[0][1]; SO->NodeList[3*i+2] = P2[0][2];
      } else {
            SUMA_SL_Err("Identical points!\n"
                        "No coordinates modified");
      }
      
      if (LocalHead) {
         if (! (i%999)) {
            a = &(SO->NodeList[3*i]);
            SUMA_SEG_LENGTH(a, SOref->Center, dj);
            fprintf(SUMA_STDERR, "%s:\n"
                           "node i=%d, avg_dist_ref = %f\ncnt = %d\n"
                           "Check on P2: New dist =%f ?=? %f\n", 
                           FuncName, i, ave_dist_ref, cnt, dj, ave_dist_ref);
            etime_GetOffset = SUMA_etime(&start_time,1);
            fprintf(SUMA_STDERR, "%s: Search to %f mm took %f seconds for %d nodes.\n"
                  "Projected completion time: %f minutes\n",
                  FuncName, max_off, etime_GetOffset, i+1,
                  etime_GetOffset * SO->N_Node / 60.0 / (i+1));
         }
      }
      if (! (i%99) && cs) {
         if (cs->Send) { /* send the first monster (it's SOref  "in SUMA" that's being modified on the fly*/
            if (!SUMA_SendToSuma (SOref, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }
      }
      
      #ifdef FROM_THIS_NODE
      {
         FILE *fid=NULL;
         char *outname=NULL, tmp[20];
         int ii;
         if (cs->Send) { /* send the first monster (it's SOref that's being modified on the fly*/
            if (!SUMA_SendToSuma (SOref, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }
         sprintf(tmp,"offset_n%d", FROM_THIS_NODE);
         outname = SUMA_Extension("", ".1D", YUP);
         outname = SUMA_append_replace_string(outname, "offset.1D", "", 1);
         fid = fopen(outname, "w"); free(outname); outname = NULL;
         if (!fid) {
            SUMA_SL_Err("Could not open file for writing.\nCheck file permissions, disk space.\n");
         } else {
            fprintf (fid,"#Column 1 = Node index\n"
                         "#column 2 = Neighborhood layer\n"
                         "#Column 3 = Distance from node %d\n", 99);
            for (ii=0; ii<SO->N_Node; ++ii) {
               if (OffS->LayerVect[ii] >= 0) {
                  fprintf(fid,"%d\t%d\t%f\n", ii, OffS->LayerVect[ii], OffS->OffVect[ii]);
               }
            }
            fclose(fid);
         }
         { int jnk; fprintf(SUMA_STDOUT,"Pausing, next node is %d...", i+1); jnk = getchar(); fprintf(SUMA_STDOUT,"\n"); }
      }
      #endif
             
      /* recycle offsets structure */
      SUMA_Recycle_getoffsets (OffS);
      
   }   
   
   /* offsets are to be freed */
   SUMA_Free_getoffsets(OffS); OffS = NULL;
   
   SUMA_RETURN(YUP);
}

/*!
   \brief calculate the interpolation weights required to smooth data on the surface
   using M.K. Chung et al. Neuroimage 03
   
   \param SO (SUMA_SurfaceObject *) Surface object with valid SO->NodeList, SO->FaceSetList and SO->FN
   \return wgt (float **) 2D matrix of the same size as SO->FirstNeighb that contains the
                           weights to be applied to a node's neighbors in interpolation
                           Free the result with SUMA_free2D ((char **)wgt, SO->N_Node);
                           The weights are computed using the Finite Element method.
   \sa SUMA_Chung_Smooth
*/
float ** SUMA_Chung_Smooth_Weights (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_Chung_Smooth_Weights"};
   float **wgt=NULL, *coord_nbr=NULL, *cotan=NULL, *tfp=NULL;
   float dv[3], p[3], q[3];
   float area, area_p, area_q, dot_p, dot_q;
   int i, j, k, n, j3p1, j3m1, n3, j3=0, nj, nj3;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_SL_Err("Null SO");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      SUMA_SL_Err("Null SO->FN");
      SUMA_RETURN(NULL);
   }
   
   /* in the demo mesh that Moo Chung gave me, 
   Node 17 has the following neighbors in ccw order:
   231 230 250 261 239 236 - 231 230 250 261 239 236 
   Set SUMA_SSidbg = 17 and turn on LocalHead to get a confirmation of this
   by this function*/
   /* implement the non-parametric weight estimation method */
   wgt = (float **)SUMA_allocate2D(SO->N_Node, SO->FN->N_Neighb_max, sizeof(float));  /* vector of node weights */
   coord_nbr = (float *)SUMA_malloc((SO->FN->N_Neighb_max + 2) * sizeof(float) * 3); /* vector of neighboring node coordinates */ 
   cotan = (float *)SUMA_malloc(SO->FN->N_Neighb_max * sizeof(float)); 
   if (!wgt || !coord_nbr || !cotan) {
      SUMA_SL_Crit("Failed to allocate for wgt &/|coord_nbr &/|cotan");
      SUMA_RETURN(NULL);
   }
   
   for (n=0; n < SO->N_Node; ++n) {
      n3 = 3 * n;
      /* translate the coordinates of the neighboring nodes to make n be the origin */
      for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
         j3 = 3 * (j+1);
         nj = SO->FN->FirstNeighb[n][j]; nj3 = 3 * nj;
         coord_nbr[j3] = SO->NodeList[nj3] - SO->NodeList[n3];
         coord_nbr[j3+1] = SO->NodeList[nj3+1] - SO->NodeList[n3+1];
         coord_nbr[j3+2] = SO->NodeList[nj3+2] - SO->NodeList[n3+2];
      }  /* for j */
      /* padd with last neighbor at the very beginning and 1st neighbor at the end 
         in matlab: coord_nbr = [coord_nbr(:,last_neighb) coord_nbr coord_nbr(:,first_neighb)];
      */   
      for (k=0; k < 3; ++k) coord_nbr[k] = coord_nbr[j3+k];  
      j3 = 3 * ( SO->FN->N_Neighb[n] + 1);
      for (k=0; k < 3; ++k) coord_nbr[j3+k] = coord_nbr[3+k];
      if (LocalHead && n == SUMA_SSidbg) { SUMA_disp_vect (coord_nbr, 3 * (SO->FN->N_Neighb[n] + 2)) ;  }
      
      
      area = 0.0;
      for (j=1; j<=SO->FN->N_Neighb[n]; ++j) { 
         j3 = 3 * j; j3p1 = 3 * (j+1); j3m1 = 3 * (j-1);
         for (k=0; k < 3; ++k) dv[k] = coord_nbr[j3p1+k] - coord_nbr[j3+k]; 
         tfp = &(coord_nbr[j3p1]);
         dot_p = SUMA_MT_DOT (tfp, dv);
         SUMA_MT_CROSS(p, tfp, dv);
         for (k=0; k < 3; ++k) dv[k] = coord_nbr[j3m1+k] - coord_nbr[j3+k]; 
         tfp = &(coord_nbr[j3m1]);
         dot_q = SUMA_MT_DOT (tfp, dv);
         SUMA_MT_CROSS(q, tfp, dv);
         
         SUMA_NORM(area_p, p); 
         SUMA_NORM(area_q, q); 
         
         cotan[j-1] = dot_p/area_p + dot_q/area_q;
         area += area_p/2.0;
         if (LocalHead && n == SUMA_SSidbg) {
            fprintf (SUMA_STDERR,"[%d->%d] area_p, area_q = %f, %f\n",
                                  n, SO->FN->FirstNeighb[n][j-1],
                                  area_p / 2.0,  area_q / 2.0);
         }
      }
      
      for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
         wgt[n][j] = cotan[j]/area;
      }
      if (LocalHead && n == SUMA_SSidbg) {
         fprintf (SUMA_STDERR,"%s: Weight Results for neighbors of %d (matlab node %d):\n",
                              FuncName, n, n+1);
         SUMA_disp_vect (wgt[n], SO->FN->N_Neighb[n]);
      }
   }  /* for n */

   /* free local variables */
   if (coord_nbr) SUMA_free(coord_nbr); coord_nbr = NULL;
   if (cotan) SUMA_free(cotan); cotan = NULL;

   SUMA_RETURN(wgt);
}

/*!
   \brief calculate the interpolation weights required to smooth data on the surface
   using M.K. Chung 's new method (see SUMA_Chung_Smooth_05 for refs)
   
   \param SO (SUMA_SurfaceObject *) Surface object with valid SO->NodeList, SO->FaceSetList and SO->FN
   \return wgt (float **) 2D matrix of the same size as SO->FirstNeighb that contains the
                           weights to be applied to a node's neighbors in interpolation
                           Free the result with SUMA_free2D ((char **)wgt, SO->N_Node);
                           The weights are computed using the Finite Element method.
   \sa SUMA_Chung_Smooth_05
   \sa Moo's hk_smooth.m
*/

float ** SUMA_Chung_Smooth_Weights_05_Pre_07 (SUMA_SurfaceObject *SO, float sigma)
{
   static char FuncName[]={"SUMA_Chung_Smooth_Weights_05_Pre_07"};
   float **wgt=NULL, *dist=NULL, *kern=NULL, *tfp=NULL;
   float dx,dy,dz, skern;
   int j, n, n3, nj, nj3, skern_warn = 0;
   int *n_troub=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_SL_Err("Null SO");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      SUMA_SL_Err("Null SO->FN");
      SUMA_RETURN(NULL);
   }
   if (sigma < 0.00001) sigma = 1.0;
   
   /* in the demo mesh that Moo Chung gave me, 
   Node 17 has the following neighbors in ccw order:
   231 230 250 261 239 236 - 231 230 250 261 239 236 
   Set SUMA_SSidbg = 17 and turn on LocalHead to get a confirmation of this
   by this function*/
   /* implement the non-parametric weight estimation method */
   wgt  = (float **)SUMA_allocate2D(SO->N_Node, (SO->FN->N_Neighb_max+1), sizeof(float));  /* vector of node weights */
   dist = (float *)SUMA_malloc((SO->FN->N_Neighb_max + 1 )* sizeof(float)); 
   kern = (float *)SUMA_malloc((SO->FN->N_Neighb_max + 1 )* sizeof(float)); 
   if (!wgt || !dist || !kern) {
      SUMA_SL_Crit("Failed to allocate for wgt &/|dist &/|kern");
      SUMA_RETURN(NULL);
   }
   
   for (n=0; n < SO->N_Node; ++n) {
      n3 = 3 * n;
      /* Distances from neighboring nodes to n  */
      for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
         nj = SO->FN->FirstNeighb[n][j]; nj3 = 3 * nj;
         dx = SO->NodeList[nj3  ] - SO->NodeList[n3  ];
         dy = SO->NodeList[nj3+1] - SO->NodeList[n3+1];
         dz = SO->NodeList[nj3+2] - SO->NodeList[n3+2];
         dist[j+1] = (dx*dx+dy*dy+dz*dz);
      }  /* for j */
      /* padd with 0 at the very beginning, distance from n to itself */   
      dist[0] =  0.0;
      if (LocalHead && n == SUMA_SSidbg) { 
         SUMA_S_Note("Showing neighbors vector followed by distance vector"); 
         SUMA_disp_dvect (SO->FN->FirstNeighb[n], (SO->FN->N_Neighb[n]));
         SUMA_disp_vect (dist, (SO->FN->N_Neighb[n] + 1)) ;  
      }
      
      /* calculate numerator of the kernel and the sum for all distances*/
      skern = 0.0;
      for (j=0; j<=SO->FN->N_Neighb[n]; ++j) {
         kern[j] = SUMA_CHUNG_KERNEL_NUMER(dist[j],sigma);
         if (LocalHead && n == SUMA_SSidbg) {
            fprintf (SUMA_STDERR,"%s: Neighb %d of %d: dist %f sigma %f: %g\n", 
                           FuncName, SO->FN->FirstNeighb[n][j], n, 
                           dist[j], sigma, SUMA_CHUNG_KERNEL_NUMER(dist[j],sigma));
         }
         skern += kern[j];
      }
      
      if (skern < 1.0f+1e-8) {
         if (!skern_warn) {
            n_troub = (int *)SUMA_malloc(sizeof(int)*SO->N_Node);
            SUMA_S_Warnv(  "   Weights sum < 1.0f+1e-8 at node %d\n"
                        "   Mesh may be too coarse for kernel\n"
                        "   bandwidth of %f in float precision.\n"
                        "   Consider decreasing your number of iterations. \n"
                        "   Future similar warnings are muted, but \n"
                        "   a count is issued at the end.\n", n, sigma);
         }
         if (n_troub) n_troub[skern_warn] = n;
         ++skern_warn;
      }
       
      /* now calculate the weights */
      for (j=0; j<=SO->FN->N_Neighb[n]; ++j) { 
         wgt[n][j] = kern[j]/skern;
      }
      
      if (LocalHead && n == SUMA_SSidbg) {
         fprintf (SUMA_STDERR,"%s: Weight Results for neighbors of %d (matlab node %d):\nskern=%f",
                              FuncName, n, n+1, skern);
         SUMA_disp_vect (wgt[n], SO->FN->N_Neighb[n]+1);
      }
   }  /* for n */

   /* free local variables */
   if (kern) SUMA_free(kern); kern = NULL;
   if (dist) SUMA_free(dist); dist = NULL;

   if (skern_warn) {
      SUMA_S_Warnv("    %d precision warnings out of %d nodes forming surface (%.5f %%).\n",
                       skern_warn, SO->N_Node, (float)skern_warn/(float)SO->N_Node*100.0);
      if (n_troub) {
         char *s = SUMA_ShowMeSome((void*)n_troub, SUMA_int, 
                                    skern_warn, SUMA_MIN_PAIR(20, skern_warn), 
                                    "Nodes with possible precision problems:\n   ");
         fprintf(SUMA_STDERR,"%s\n", s);
         SUMA_free(s); s= NULL;
         SUMA_free(n_troub); n_troub = NULL;
      }
   }
   SUMA_RETURN(wgt);
}

float ** SUMA_Chung_Smooth_Weights_05_single (SUMA_SurfaceObject *SO, float sigma)
{
   static char FuncName[]={"SUMA_Chung_Smooth_Weights_05_single"};
   float **wgt=NULL, *dist=NULL, *kern=NULL, *tfp=NULL;
   float dx,dy,dz, skern;
   int j, n, n3, nj, nj3, skern_warn = 0;
   int *n_troub=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_SL_Err("Null SO");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      SUMA_SL_Err("Null SO->FN");
      SUMA_RETURN(NULL);
   }
   if (sigma < 0.00001) sigma = 1.0;
   
   /* in the demo mesh that Moo Chung gave me, 
   Node 17 has the following neighbors in ccw order:
   231 230 250 261 239 236 - 231 230 250 261 239 236 
   Set SUMA_SSidbg = 17 and turn on LocalHead to get a confirmation of this
   by this function*/
   /* implement the non-parametric weight estimation method */
   wgt  = (float **)SUMA_allocate2D(SO->N_Node, (SO->FN->N_Neighb_max+1), sizeof(float));  /* vector of node weights */
   dist = (float *)SUMA_malloc((SO->FN->N_Neighb_max + 1 )* sizeof(float)); 
   kern = (float *)SUMA_malloc((SO->FN->N_Neighb_max + 1 )* sizeof(float)); 
   if (!wgt || !dist || !kern) {
      SUMA_SL_Crit("Failed to allocate for wgt &/|dist &/|kern");
      SUMA_RETURN(NULL);
   }
   
   for (n=0; n < SO->N_Node; ++n) {
      n3 = 3 * n;
      /* Distances from neighboring nodes to n  */
      for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
         nj = SO->FN->FirstNeighb[n][j]; nj3 = 3 * nj;
         dx = SO->NodeList[nj3  ] - SO->NodeList[n3  ];
         dy = SO->NodeList[nj3+1] - SO->NodeList[n3+1];
         dz = SO->NodeList[nj3+2] - SO->NodeList[n3+2];
         dist[j+1] = (dx*dx+dy*dy+dz*dz);
      }  /* for j */
      /* padd with 0 at the very beginning, distance from n to itself */   
      dist[0] =  0.0;
      if (LocalHead && n == SUMA_SSidbg) { 
         SUMA_S_Note("Showing neighbors vector followed by distance vector"); 
         SUMA_disp_dvect (SO->FN->FirstNeighb[n], (SO->FN->N_Neighb[n]));
         SUMA_disp_vect (dist, (SO->FN->N_Neighb[n] + 1)) ;  
      }
      
      /* calculate numerator of the kernel and the sum for all distances*/
      skern = 0.0;
      for (j=0; j<=SO->FN->N_Neighb[n]; ++j) {
         kern[j] = SUMA_CHUNG_KERNEL_NUMER(dist[j],sigma);
         if (LocalHead && n == SUMA_SSidbg) {
            fprintf (SUMA_STDERR,"%s: Neighb %d of %d: dist %f sigma %f: %g\n", 
                           FuncName, SO->FN->FirstNeighb[n][j], n, 
                           dist[j], sigma, SUMA_CHUNG_KERNEL_NUMER(dist[j],sigma));
         }
         skern += kern[j];
      }
      
      if (skern < 1.0f+1e-8) {
         if (!skern_warn) {
            n_troub = (int *)SUMA_malloc(sizeof(int)*SO->N_Node);
            SUMA_S_Warnv(  "   Weights sum < 1.0f+1e-8 at node %d\n"
                        "   Mesh may be too coarse for kernel\n"
                        "   bandwidth of %f in float precision.\n"
                        "   Consider decreasing your number of iterations. \n"
                        "   Future similar warnings are muted, but \n"
                        "   a count is issued at the end.\n", n, sigma);
         }
         if (n_troub) n_troub[skern_warn] = n;
         ++skern_warn;
      }
       
      /* now calculate the weights */
      for (j=0; j<=SO->FN->N_Neighb[n]; ++j) { 
         wgt[n][j] = kern[j]/skern;
      }
      
      if (LocalHead && n == SUMA_SSidbg) {
         fprintf (SUMA_STDERR,"%s: Weight Results for neighbors of %d (matlab node %d):\nskern=%f",
                              FuncName, n, n+1, skern);
         SUMA_disp_vect (wgt[n], SO->FN->N_Neighb[n]+1);
      }
   }  /* for n */

   /* free local variables */
   if (kern) SUMA_free(kern); kern = NULL;
   if (dist) SUMA_free(dist); dist = NULL;

   if (skern_warn) {
      SUMA_S_Warnv("    %d precision warnings out of %d nodes forming surface (%.5f %%).\n",
                       skern_warn, SO->N_Node, (float)skern_warn/(float)SO->N_Node*100.0);
      if (n_troub) {
         char *s = SUMA_ShowMeSome((void*)n_troub, SUMA_int, 
                                    skern_warn, SUMA_MIN_PAIR(20, skern_warn), 
                                    "Nodes with possible precision problems:\n   ");
         fprintf(SUMA_STDERR,"%s\n", s);
         SUMA_free(s); s= NULL;
         SUMA_free(n_troub); n_troub = NULL;
      }
   }
   SUMA_RETURN(wgt);
}


double ** SUMA_Chung_Smooth_Weights_07 (SUMA_SurfaceObject *SO, double sigma)
{
   static char FuncName[]={"SUMA_Chung_Smooth_Weights_07"};
   double **wgt=NULL, *dist=NULL, *kern=NULL, *tfp=NULL;
   double dx,dy,dz, skern;
   int j, n, n3, nj, nj3, skern_warn = 0;
   int *n_troub=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_SL_Err("Null SO");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      SUMA_SL_Err("Null SO->FN");
      SUMA_RETURN(NULL);
   }
   if (sigma < 0.00001) sigma = 1.0;
   
   /* in the demo mesh that Moo Chung gave me, 
   Node 17 has the following neighbors in ccw order:
   231 230 250 261 239 236 - 231 230 250 261 239 236 
   Set SUMA_SSidbg = 17 and turn on LocalHead to get a confirmation of this
   by this function*/
   /* implement the non-parametric weight estimation method */
   wgt  = (double **)SUMA_malloc(SO->N_Node * sizeof(double*));  /* vector of node weights */
   dist = (double *)SUMA_malloc((SO->FN->N_Neighb_max + 1 )* sizeof(double)); 
   kern = (double *)SUMA_malloc((SO->FN->N_Neighb_max + 1 )* sizeof(double)); 
   if (!wgt || !dist || !kern) {
      SUMA_SL_Crit("Failed to allocate for wgt &/|dist &/|kern");
      SUMA_RETURN(NULL);
   }
   
   for (n=0; n < SO->N_Node; ++n) {
      n3 = 3 * n;
      /* Distances from neighboring nodes to n  */
      for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
         nj = SO->FN->FirstNeighb[n][j]; nj3 = 3 * nj;
         dx = SO->NodeList[nj3  ] - SO->NodeList[n3  ];
         dy = SO->NodeList[nj3+1] - SO->NodeList[n3+1];
         dz = SO->NodeList[nj3+2] - SO->NodeList[n3+2];
         dist[j+1] = (dx*dx+dy*dy+dz*dz);
      }  /* for j */
      /* padd with 0 at the very beginning, distance from n to itself */   
      dist[0] =  0.0;
      if (LocalHead && n == SUMA_SSidbg) { 
         SUMA_S_Note("Showing neighbors vector followed by distance^2 vector"); 
         SUMA_disp_dvect (SO->FN->FirstNeighb[n], (SO->FN->N_Neighb[n]));
         SUMA_disp_doubvect (dist, (SO->FN->N_Neighb[n] + 1)) ;  
      }
      
      /* calculate numerator of the kernel and the sum for all distances*/
      skern = 0.0;
      for (j=0; j<=SO->FN->N_Neighb[n]; ++j) {
         kern[j] = SUMA_CHUNG_KERNEL_NUMER(dist[j],sigma);
         if (LocalHead && n == SUMA_SSidbg) {
            fprintf (SUMA_STDERR,"%s: Neighb %d of %d: dist^2 %f sigma %f: %g\n", 
                           FuncName, SO->FN->FirstNeighb[n][j], n, 
                           dist[j], sigma, SUMA_CHUNG_KERNEL_NUMER(dist[j],sigma));
         }
         skern += kern[j];
      }
      
      if (skern < 1.0f+1e-8) {
         if (!skern_warn) {
            n_troub = (int *)SUMA_malloc(sizeof(int)*SO->N_Node);
            SUMA_S_Warnv(  "   Weights sum < 1.0f+1e-8 at node %d\n"
                        "   Mesh may be too coarse for kernel\n"
                        "   bandwidth of %f in float precision.\n"
                        "   Consider decreasing your number of iterations. \n"
                        "   Future similar warnings are muted, but \n"
                        "   a count is issued at the end.\n", n, sigma);
         }
         if (n_troub) n_troub[skern_warn] = n;
         ++skern_warn;
      }
       
      /* now calculate the weights */
      wgt[n] = (double *)SUMA_malloc(sizeof(double)*(SO->FN->N_Neighb[n]+1));
      for (j=0; j<=SO->FN->N_Neighb[n]; ++j) { 
         wgt[n][j] = kern[j]/skern;
      }
      
      if (LocalHead && n == SUMA_SSidbg) {
         fprintf (SUMA_STDERR,"%s: Weight Results for neighbors of %d (matlab node %d):\nskern=%f",
                              FuncName, n, n+1, skern);
         SUMA_disp_doubvect (wgt[n], SO->FN->N_Neighb[n]+1);
      }
   }  /* for n */

   /* free local variables */
   if (kern) SUMA_free(kern); kern = NULL;
   if (dist) SUMA_free(dist); dist = NULL;

   if (skern_warn) {
      SUMA_S_Warnv("    %d precision warnings out of %d nodes forming surface (%.5f %%).\n",
                       skern_warn, SO->N_Node, (float)skern_warn/(float)SO->N_Node*100.0);
      if (n_troub) {
         char *s = SUMA_ShowMeSome((void*)n_troub, SUMA_int, 
                                    skern_warn, SUMA_MIN_PAIR(20, skern_warn), 
                                    "Nodes with possible precision problems:\n   ");
         fprintf(SUMA_STDERR,"%s\n", s);
         SUMA_free(s); s= NULL;
         SUMA_free(n_troub); n_troub = NULL;
      }
   }
   SUMA_RETURN(wgt);
}

static byte SUMA_Taubin_Weights=SUMA_SMOOTH_NOT_SET;
byte SUMA_Get_Taubin_Weights(void)
{
   return(SUMA_Taubin_Weights);
}

void SUMA_Set_Taubin_Weights(SUMA_TAUBIN_SMOOTH_OPTIONS tb) 
{
   SUMA_Taubin_Weights = tb;
}

/*!
   \brief calculate the Fujiwara interpolation weights. Compensates for irregular edge lengths, trying
   to better preserve geometry in instances where mesh is irregular 
   (Taubin G, Eurographics 2000)
   
   \param SO (SUMA_SurfaceObject *) Surface object with valid SO->NodeList, SO->FaceSetList and SO->FN
   \param NewNodeList (float *) a node list to use instead of the one in SO. Useful when you're calling 
                                the function repeatedly. Set to NULL if you want to use SO->NodeList
   \param UseThisWeight (float ***) Pointer to a weight matrix that is to be used instead of reallocating a new
                                    one. If UseThisWeight == NULL, a new wgt is returned at each time.
                                    If *UseThisWeight == NULL, a new wgt is allocated and stored in *UseThisWeight
                                    so the next time you call the function with UseThisWeight, a new wgt needs
                                    not be created.
   \return wgt (float **) 2D matrix of the same size as SO->FirstNeighb that contains the
                           weights to be applied to a node's neighbors in interpolation
                           Free the result with SUMA_free2D ((char **)wgt, SO->N_Node);
                           
   \sa SUMA_Taubin_Smooth
*/
float ** SUMA_Taubin_Fujiwara_Smooth_Weights (SUMA_SurfaceObject *SO, float *NewNodeList, float ***UseThisWeight)
{
   static char FuncName[]={"SUMA_Taubin_Fujiwara_Smooth_Weights"};
   float **wgt=NULL, *coord_nbr=NULL, *cotan=NULL, *tfp=NULL, *nl = NULL;
   float dv[3], p[3], q[3];
   double cij=0.0;
   float area, area_p, area_q, dot_p, dot_q;
   int i, j, k, n, j3p1, j3m1, n3, j3=0, nj, nj3;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_SL_Err("Null SO");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      SUMA_SL_Err("Null SO->FN");
      SUMA_RETURN(NULL);
   }
   if (NewNodeList) {
      nl = NewNodeList;
   } else {
      nl = SO->NodeList;
   }
   
   SUMA_SL_Note("FUJIWARA!!!!");
   /* implement the Fujiwara weight estimation method */
   wgt = NULL;
   if (UseThisWeight) {
      wgt = *UseThisWeight;
   } 
   if (!wgt) {
      wgt = (float **)SUMA_allocate2D(SO->N_Node, SO->FN->N_Neighb_max, sizeof(float));  /* vector of node weights */
      if (UseThisWeight) { /* store it for use later on */
         *UseThisWeight = wgt;
      }
   }
   if (!wgt) {
      SUMA_SL_Crit("Failed to allocate for wgt &/|coord_nbr &/|cotan");
      SUMA_RETURN(NULL);
   }
   
   for (n=0; n < SO->N_Node; ++n) {
      n3 = 3 * n;
      area = 0.0;
      for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
         j3 = 3 * (j);
         nj = SO->FN->FirstNeighb[n][j]; nj3 = 3 * nj;
         SUMA_SEG_LENGTH( (&(nl[nj3])), (&(nl[n3])), cij);
         if (cij > 0.00001) {
            cij = 1/cij;
         } else {
            cij = 0.0;
         }
         area += (float)cij; 
         wgt[n][j] = (float)cij;
      }  /* for j */
      if (area) { 
         for (j=0; j<SO->FN->N_Neighb[n]; ++j) { wgt[n][j] /= area; }
      }
      
      
      if (LocalHead && n == SUMA_SSidbg) {
         fprintf (SUMA_STDERR,"%s: Weight Results for neighbors of %d (matlab node %d):\n",
                              FuncName, n, n+1);
         SUMA_disp_vect (wgt[n], SO->FN->N_Neighb[n]);
      }
   }  /* for n */

   SUMA_RETURN(wgt);
}

/*!
   \brief calculate the Desbrun interpolation weights, results in zero tangential component to smoothing
   (Taubin G, Eurographics 2000)
   
   \param SO (SUMA_SurfaceObject *) Surface object with valid SO->NodeList, SO->FaceSetList and SO->FN
   \param NewNodeList (float *) a node list to use instead of the one in SO. Useful when you're calling 
                                the function repeatedly. Set to NULL if you want to use SO->NodeList
   \param UseThisWeight (float ***) Pointer to a weight matrix that is to be used instead of reallocating a new
                                    one. If UseThisWeight == NULL, a new wgt is returned at each time.
                                    If *UseThisWeight == NULL, a new wgt is allocated and stored in *UseThisWeight
                                    so the next time you call the function with UseThisWeight, a new wgt needs
                                    not be created.
   \return wgt (float **) 2D matrix of the same size as SO->FirstNeighb that contains the
                           weights to be applied to a node's neighbors in interpolation
                           Free the result with SUMA_free2D ((char **)wgt, SO->N_Node);
                           
   \sa SUMA_Taubin_Smooth
*/
float ** SUMA_Taubin_Desbrun_Smooth_Weights (SUMA_SurfaceObject *SO, float *NewNodeList, float ***UseThisWeight)
{
   static char FuncName[]={"SUMA_Taubin_Desbrun_Smooth_Weights"};
   float **wgt=NULL, *coord_nbr=NULL, *cotan=NULL, *tfp=NULL, *nl = NULL;
   float dv[3], p[3], q[3];
   double cij=0.0, cot1, cot2, Uc[3], U1[3], U2[3], Ucn, U1n, U2n;
   float area, area_p, area_q, dot_p, dot_q;
   int N_inci_alloc=100;
   int i, j, k, n, j3p1, j3m1, n3, j3=0, nj, nj3, nk_1, nk_2, N_inci, inci[N_inci_alloc];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_SL_Err("Null SO");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      SUMA_SL_Err("Null SO->FN");
      SUMA_RETURN(NULL);
   }
   if (NewNodeList) {
      nl = NewNodeList;
   } else {
      nl = SO->NodeList;
   }
   
   SUMA_SL_Note("DESBRUN!!!!");
   /* implement the Desbrun weight estimation method */
   wgt = NULL;
   if (UseThisWeight) {
      wgt = *UseThisWeight;
   } 
   if (!wgt) {
      wgt = (float **)SUMA_allocate2D(SO->N_Node, SO->FN->N_Neighb_max, sizeof(float));  /* vector of node weights */
      if (UseThisWeight) { /* store it for use later on */
         *UseThisWeight = wgt;
      }
   }
   if (!wgt) {
      SUMA_SL_Crit("Failed to allocate for wgt");
      SUMA_RETURN(NULL);
   }
   
   for (n=0; n < SO->N_Node; ++n) {
      n3 = 3 * n;
      area = 0.0;
      for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
         j3 = 3 * (j);
         nj = SO->FN->FirstNeighb[n][j]; nj3 = 3 * nj;
         /* find the incident triangle to n nj */
         N_inci = 0;
         if (!SUMA_Get_Incident(n, nj, SO->EL, inci, &N_inci, 0, 0)) {
            SUMA_SL_Err("Failed to find incident triangles!\n");
            SUMA_RETURN(NULL);
         }else if (N_inci > 2) {
            SUMA_SL_Warn("Non 2-manifold surface!");
            if (N_inci > N_inci_alloc) {
               SUMA_SL_Crit("Bad allocation for inci.");
               SUMA_RETURN(NULL);
            }
         }
         
         /* calculate cij = cot1 + cot2 */
         
         cot1= cot2 = cij = 0.0;   
         /* find the third node from each of the triangles */
         SUMA_THIRD_TRIANGLE_NODE(nj,n,inci[0],SO->FaceSetList,nk_1);
         /* find the cotangent of the opposing angles at nk_1 and nk_2 */
         /* form vectors */
         SUMA_UNIT_VEC((&(nl[3*nk_1])), (&(nl[3*n ])), U1, U1n);
         SUMA_UNIT_VEC((&(nl[3*nk_1])), (&(nl[3*nj])), U2, U2n);
         SUMA_MT_CROSS(Uc, U1, U2); Ucn = sqrt(Uc[0]*Uc[0]+Uc[1]*Uc[1]+Uc[2]*Uc[2]);
         /* cotangent = dot / cross product*/
         if (Ucn > 0.00000000001) cot1 = SUMA_MT_DOT(U1,U2)/Ucn;
         if (N_inci > 1) {
            SUMA_THIRD_TRIANGLE_NODE(nj,n,inci[1],SO->FaceSetList,nk_2);
            /* form vectors */
            SUMA_UNIT_VEC((&(nl[3*nk_2])), (&(nl[3*n ])), U1, U1n);
            SUMA_UNIT_VEC((&(nl[3*nk_2])), (&(nl[3*nj])), U2, U2n);
            SUMA_MT_CROSS(Uc, U1, U2); Ucn = sqrt(Uc[0]*Uc[0]+Uc[1]*Uc[1]+Uc[2]*Uc[2]);
            /* cotangent = dot / cross product*/
            if (Ucn > 0.00000000001) cot2 = SUMA_MT_DOT(U1,U2)/Ucn;
         }
         if (LocalHead && n == SUMA_SSidbg) {
            int kk;
            fprintf (SUMA_STDERR,"%s: Edge [%d %d]:   third nodes:\n", 
                              FuncName, n, nj);
            for (kk=0; kk<N_inci; ++kk) {
               fprintf (SUMA_STDERR,"   %d", inci[kk]);
            }
            fprintf (SUMA_STDERR,"\n"); 
            fprintf (SUMA_STDERR,"   cot: %f   %f\n", cot1, cot2);              
         }
         cij = SUMA_MAX_PAIR((cot1 + cot2), 0.05); /* give a minimum of 5% weight to each node */
         
         area += (float)cij; 
         wgt[n][j] = (float)cij;
      }  /* for j */
      if (area) { 
         for (j=0; j<SO->FN->N_Neighb[n]; ++j) { wgt[n][j] /= area; }
      }
      
      
      if (LocalHead && n == SUMA_SSidbg) {
         fprintf (SUMA_STDERR,"%s: Weight Results for neighbors of %d (matlab node %d):\n",
                              FuncName, n, n+1);
         SUMA_disp_vect (wgt[n], SO->FN->N_Neighb[n]);
      }
   }  /* for n */

   SUMA_RETURN(wgt);
}
/*!
   \brief Show the transfer function (f(k)) for the Taubin 
   smoothing algorithm for a combination of scaling factors
   and number of iterations 
   
   \param l (float)  postive scaling factor
   \param m (float)  negative scaling factor
                    (for avoiding the shrinkage of surfaces)
            The band-pass frequency (kbp) is 1/m + 1/l
            f(kbp) = 1
   \param N_iter (int) number of iterations
   \param Out (FILE *) pointer to output file. If NULL,
                  output is to stdout
   \return ans (SUMA_Boolean) YUP, no problems
                              NOPE, yes problems
   
   The output is of the form:
   k f(k)
   
   where k is the normalized frequency and 
   f(k) is the value of the transfer function at k
   
   \sa figure 4 in Geometric Signal Processing on 
   Polygonal Meshes (Taubin G, Eurographics 2000)
   \sa SUMA_Taubin_Smooth
   \sa SUMA_Taubin_Smooth_Coef
*/   
SUMA_Boolean  SUMA_Taubin_Smooth_TransferFunc (float l, float m, int N, FILE *Out)
{
   static char FuncName[]={"SUMA_Taubin_Smooth_TransferFunc"};
   FILE *Outp = NULL;
   int i, imax = 100;
   float fk, k;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (N % 2) {
      SUMA_SL_Err("N_iter must be even");
      SUMA_RETURN(NOPE);
   }
   
   if (!Out) Outp = stdout;
   else Outp = Out;
   
   k = 0.0;
   for (i=0; i< imax; ++i) {
      fk = pow( ( ( 1-m*k ) * ( 1-l*k ) ) , N / 2 );
      fprintf (Outp,"%f %f\n", k, fk);
      k += (float)i/(float)imax;
   }
   
   
   SUMA_RETURN(YUP);
}
/*!
   \brief Calculates Mu(m) and Lambda(l) smoothing coefficients
   based on Taubin's smoothing algorithm in Geometric Signal 
   Processing on Polygonal Meshes (Eurographics 2000)
   
   \param k (float) the pass-band frequency (typically 0.1)
   \param *l (float) what will be the postive scaling factor
   \param *m (float) what will be the negative scaling factor
                     (for avoiding the shrinkage of surfaces)
   \return ans (SUMA_Boolean) YUP, good solution found
                              NOPE, solution could not be found        
            
   k, l and m are related by the following equations:
   
   k = 1/l + 1/m                 (eq 8)
   0 = 1 - 3(l+m) + 5lm          (eq 9)
   
   the solutions must verify the following:
   l > 0
   m < -l < 0
   
   \sa SUMA_Taubin_Smooth_TransferFunc 
   \sa SUMA_Taubin_Smooth
    
*/ 
SUMA_Boolean SUMA_Taubin_Smooth_Coef (float k, float *l, float *m)
{
   static char FuncName[]={"SUMA_Taubin_Smooth_Coef"};
   int i;
   float ls[2], delta;
   SUMA_Boolean Done = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (k < 0) { SUMA_SL_Err("k < 0"); SUMA_RETURN(NOPE); }
   
   /* l1 and l2 are solutions of the quadratic equation:
      (5 - 3 k) l^2 + k l - 1 = 0 */
   delta = ( k * k - 12.0 * k + 20 );
   if (delta < 0) { SUMA_SL_Err("Delta is < 0 for specified k"); SUMA_RETURN(NOPE); }
   
   ls[0] = ( -k + sqrt(delta) ) / ( 10 - 6 * k );
   ls[1] = ( -k - sqrt(delta) ) / ( 10 - 6 * k );
   if (ls[0] < 0 && ls[1] < 0) { SUMA_SL_Err("No positive solution for l"); SUMA_RETURN(NOPE); }
   
   if (ls[1] > ls[0]) { /* swap them */
      *l = ls[0]; ls[0] = ls[1]; ls[1] = *l;
   }
   
   Done = NOPE;
   i = 0;
   while (!Done && i < 2) {
      /* calculate mu */
      *l = ls[i]; 
      *m = *l / ( k * *l - 1.0 );
      if (*m < 0) Done = YUP;
      ++i;
   }
   
   if (!Done) { SUMA_SL_Err("No good solutions found."); SUMA_RETURN(NOPE); }
   
   if ( ! ( ( *m < -1.0 * *l ) && ( -1.0 * *l < 0 ) ) ) {
      SUMA_SL_Err("Solution did not meet m < -l < 0"); SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief performs smoothing based on Taubin's smoothing 
   algorithm in Geometric Signal Processing on Polygonal 
   Meshes (Eurographics 2000)
   
   fout =  SUMA_Taubin_Smooth (SUMA_SurfaceObject *SO, float **wgt, 
                            float lambda, float mu, float *fin, 
                            int N_iter, int vpn,  d_order,
                            float *fout_user, SUMA_COMM_STRUCT *cs);
                            
   \param SO (SUMA_SurfaceObject *SO) The surface, with NodeList, FaceSetList
                                       and FN fields present
   \param wgt (float **) interpolation weights for each node. 
                         The dimentions of wgt are equal to those of 
                         SO->FN->FirstNeighb
                         These weights may need to be re-evaluated for
                         each iteration
                         For equal weights (1/SO->FN->N_FirstNeighb[n]), 
                         just pass NULL         
   \param lambda (float) postive scaling factor
   \param mu (float) negative scaling factor 
   \param fin (float *) vector containing node data. The length of this vector
                        is vpn x SO->N_Node , where vpn is the number of values
                        per node. 
   \param N_iter (int)  number of iterations (same weights are used in each iteration)
   \param vpn (int) number of values per node in fin
   \param d_order (SUMA_INDEXING_ORDER) Indicates how multiple values per node are stored in fin
                        SUMA_ROW_MAJOR: The data in fin is stored in *** Row Major *** order.
                        The ith value (start at 0) for node n is at index fin[vpn*n+i]
                        SUMA_COLUMN_MAJOR: The data in fin is stored in *** Column Major *** order.
                        The ith (start at 0) value for node n is at index fin[n+SO->N_Node*i]; 
                        etc...
   \param fout_user (float *) a pointer to the vector where the smoothed version of fin will reside
                        You can pass NULL and the function will do the allocation for you and return 
                        the pointer to the smoothed data in fout.
                        If you already have space allocated for the result, then pass the pointer
                        in fout_user and save on allocation time and space. In that case, fout
                        is equal to fout_user.
                        Either way, you are responsible for freeing memory pointed to by fout.
                        DO NOT PASS fout_user = fin 
   \param cs (SUMA_COMM_STRUCT *) See SUMA_Chung_Smooth
   \param nmask (byte *) NULL == filter all nodes 
                         else filter node n if nmask[n]
   \return fout (float *) A pointer to the smoothed data (vpn * SO->N_Node values). 
                        You will have to free the memory allocated for fout yourself.
                        
    
   
*/
float * SUMA_Taubin_Smooth (SUMA_SurfaceObject *SO, float **wgt, 
                            float lambda, float mu, float *fin_orig, 
                            int N_iter, int vpn, SUMA_INDEXING_ORDER d_order,
                            float *fout_final_user, SUMA_COMM_STRUCT *cs, 
                            byte *nmask, byte strict_mask)
{
   static char FuncName[]={"SUMA_Taubin_Smooth"};
   float *fout_final=NULL, *fbuf=NULL, *fin=NULL, *fout=NULL, *fin_next=NULL, *ftmp=NULL;
   float fp, dfp, fpj;
   int i, n , k, j, niter, vnk, n_offset, DoThis, nj=-1, nnei=-1; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (N_iter % 2) {
      SUMA_SL_Err("N_iter must be an even number\n");
      SUMA_RETURN(NULL);
   }
   
   if (!SO || !fin_orig) {
      SUMA_SL_Err("NULL SO or fin_orig\n");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN\n");
      SUMA_RETURN(NULL);
   }
   
   if (vpn < 1) {
      SUMA_SL_Err("vpn < 1\n");
      SUMA_RETURN(NULL);
   }  
   
   if (fout_final_user == fin_orig) {
      SUMA_SL_Err("fout_final_user == fin_orig");
      SUMA_RETURN(NULL);
   }
   
   if (!fout_final_user) { /* allocate for output */
      fout_final = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
      if (!fout_final) {
         SUMA_SL_Crit("Failed to allocate for fout_final\n");
         SUMA_RETURN(NULL);
      }
   }else {
      fout_final = fout_final_user; /* pre-allocated */
   }
   
   if (d_order == SUMA_COLUMN_MAJOR) {
         SUMA_SL_Warn("SUMA_COLUMN_MAJOR has not been thoroughly tested.");
   }
   
   if (cs->Send) {
      if(vpn != 3) {
         SUMA_SL_Warn("It does not look like you are smoothing coordinates!\nCommunication halted.");
         cs->Send = NOPE;
      }
      if (d_order == SUMA_COLUMN_MAJOR) {
         SUMA_SL_Warn("Talking with SUMA_COLUMN_MAJOR has not been tested.");
      }
   }
   
   /* allocate for buffer */
   fbuf = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
   if (!fbuf) {
      SUMA_SL_Crit("Failed to allocate for fbuf\n");
      SUMA_RETURN(NULL);
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: Mu = %f, Lambda = %f\nShould have M(%f)< -L(%f) < 0\nN_iter=%d\n", 
         FuncName, mu, lambda, mu, -lambda, N_iter);
   }
   
   if (cs->Send) { /* send the first monster */
      if (!SUMA_SendToSuma (SO, cs, (void *)fin_orig, SUMA_NODE_XYZ, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
   }
   
   fin_next = fin_orig;
   switch (d_order) {
      case SUMA_COLUMN_MAJOR:
         for (niter=0; niter < N_iter; ++niter) {
            if ( niter % 2 ) { /* odd */
               fin = fin_next; /* input from previous output buffer */
               fout = fout_final; /* results go into final vector */
               fin_next = fout_final; /* in the next iteration, the input is from fout_final */
            } else { /* even */
               /* input data is in fin_new */
               fin = fin_next;
               fout = fbuf; /* results go into buffer */
               fin_next = fbuf; /* in the next iteration, the input is from the buffer */
               if (wgt && niter) {
                  /* recalculate the weights */
                  if (SUMA_Taubin_Weights == SUMA_FUJIWARA) {
                     SUMA_Taubin_Fujiwara_Smooth_Weights(SO, fin, &wgt);
                  } else if (SUMA_Taubin_Weights == SUMA_DESBRUN) {
                     SUMA_Taubin_Desbrun_Smooth_Weights(SO, fin, &wgt);
                  } else {
                     SUMA_SL_Err("Weights not set");
                  }
               }
            }
            for (k=0; k < vpn; ++k) {
               n_offset = k * SO->N_Node;  /* offset of kth node value in fin */
               for (n=0; n < SO->N_Node; ++n) {
                  DoThis = 1;
                  if (nmask && !nmask[n]) DoThis = 0;
                  vnk = n+n_offset; 
                  if (DoThis) {
                     fp = fin[vnk]; /* kth value at node n */
                     dfp = 0.0;
                     if (nmask) {
                        nnei = 0;
                        for (j=0; j < SO->FN->N_Neighb[n]; ++j) { /* calculating the laplacian */
                              nj = SO->FN->FirstNeighb[n][j];
                              if (nmask[nj] || !strict_mask){ /* consider only neighbors that are in mask if strict_mask is 1*/
                                 fpj = fin[nj+n_offset]; /* value at jth neighbor of n */
                                 if (wgt) dfp += wgt[n][j] * (fpj - fp); 
                                 else { dfp += (fpj - fp); ++nnei; }/* will apply equal weight later */
                              }
                        }/* for j*/
                     } else {
                        nnei = SO->FN->N_Neighb[n];
                        for (j=0; j < SO->FN->N_Neighb[n]; ++j) { /* calculating the laplacian */
                              fpj = fin[SO->FN->FirstNeighb[n][j]+n_offset]; /* value at jth neighbor of n */
                              if (wgt) dfp += wgt[n][j] * (fpj - fp); 
                              else dfp += (fpj - fp); /* will apply equal weight later */
                        }/* for j*/
                     }
                     if (niter%2) { /* odd */
                        if (wgt) fout[vnk] = fin[vnk] + mu * dfp;
                        else fout[vnk] = fin[vnk] + mu * dfp / (float)nnei;   /* apply equal weight factor here */
                     }else{ /* even */
                       if (wgt) fout[vnk] = fin[vnk] + lambda * dfp;
                       else fout[vnk] = fin[vnk] + lambda * dfp / (float)nnei;  /* apply equal weight factor here */
                     }
                  } else {
                     fout[vnk] = fin[vnk];
                  }      
               }/* for n */   
            }/* for k */
            if (cs->Send) {
               /* SUMA_SendToSuma does not deal with such COLUMN_MAJOR order.
               Must flip things here, boooooring */
               if (!niter) { /* allocate for buffer */
                  ftmp = (float *) SUMA_malloc(3*SO->N_Node*sizeof(float));
                  if (!ftmp) { SUMA_SL_Err("Failed to allocate. Communication Off.\n"); cs->Send = NOPE; }
               }
               if (ftmp) {
                  for (i=0; i<SO->N_Node; ++i) { ftmp[3*i] = fout[i]; ftmp[3*i+1] = fout[i+SO->N_Node];  ftmp[3*i+2] = fout[i+2*SO->N_Node];}
                  if (!SUMA_SendToSuma (SO, cs, (void *)ftmp, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
               if (niter == N_iter -1) { /* free the buffer */
                  if (ftmp) { SUMA_free(ftmp); ftmp = NULL; }
               }
            }
         }/* for niter */
         break;
      case SUMA_ROW_MAJOR:
         for (niter=0; niter < N_iter; ++niter) {
            if ( niter % 2 ) { /* odd */
               fin = fin_next; /* input from previous output buffer */
               fout = fout_final; /* results go into final vector */
               fin_next = fout_final; /* in the next iteration, the input is from fout_final */
            } else { /* even */
               /* input data is in fin_new */
               fin = fin_next;
               fout = fbuf; /* results go into buffer */
               fin_next = fbuf; /* in the next iteration, the input is from the buffer */
               if (wgt && niter ) {
                  /* recalculate the weights */
                  if (SUMA_Taubin_Weights == SUMA_FUJIWARA) {
                     SUMA_Taubin_Fujiwara_Smooth_Weights(SO, fin, &wgt);
                  } else if (SUMA_Taubin_Weights == SUMA_DESBRUN) {
                     SUMA_Taubin_Desbrun_Smooth_Weights(SO, fin, &wgt);
                  } else {
                     SUMA_SL_Err("Weights not set");
                  }
               }
            }
            for (n=0; n < SO->N_Node; ++n) {
               DoThis = 1;
               if (nmask && !nmask[n]) DoThis = 0;
               vnk = n * vpn; /* index of 1st value at node n */
               for (k=0; k < vpn; ++k) {
                  if (DoThis) {
                     fp = fin[vnk]; /* kth value at node n */
                     dfp = 0.0;
                     if (nmask) {
                        nnei = 0;
                        for (j=0; j < SO->FN->N_Neighb[n]; ++j) { /* calculating the laplacian */
                           nj = SO->FN->FirstNeighb[n][j];
                           if (nmask[nj] || !strict_mask) { /* consider only neighbors that are in mask if strict_mask is 1*/
                            
                              fpj = fin[nj*vpn+k]; /* value at jth neighbor of n */
                              if (wgt) dfp += wgt[n][j] * (fpj - fp); 
                              else { dfp += (fpj - fp); ++nnei;} /* will apply equal weight later */
                           }
                        }/* for j*/
                     } else {
                        nnei = SO->FN->N_Neighb[n];
                        for (j=0; j < SO->FN->N_Neighb[n]; ++j) { /* calculating the laplacian */
                           fpj = fin[SO->FN->FirstNeighb[n][j]*vpn+k]; /* value at jth neighbor of n */
                           if (wgt) dfp += wgt[n][j] * (fpj - fp); 
                           else dfp += (fpj - fp); /* will apply equal weight later */
                        }/* for j*/
                     }
                     if (niter%2) { /* odd */
                        if (wgt) fout[vnk] = fin[vnk] + mu * dfp;
                        else fout[vnk] = fin[vnk] + mu * dfp / (float)nnei;   /* apply equal weight factor here */
                     }else{ /* even */
                       if (wgt) fout[vnk] = fin[vnk] + lambda * dfp;
                       else fout[vnk] = fin[vnk] + lambda * dfp / (float)nnei;  /* apply equal weight factor here */
                     }
                  } else {
                     fout[vnk] = fin[vnk];
                  } 
                  ++vnk; /* index of next value at node n */
               } /* for k */
            }/* for n */
            if (cs->Send) {
               if (!SUMA_SendToSuma (SO, cs, (void *)fout, SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
               }
            }
         }/* for niter */
         break;
      default:
         SUMA_SL_Err("Bad Major, very bad.\n");
         SUMA_RETURN(NULL);
         break;
   }
   
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;
      
   SUMA_RETURN(fout);
}

static int OffsetDebugNode = -1; /* debugging variable for Offset computing functions */
void SUMA_Set_OffsetDebugNode (int d)
{
   OffsetDebugNode = d;
   return;
}


/*!
   \brief a function to turn the often cumbersome SUMA_GET_OFFSET_STRUCT
   to a more friendly SUMA_OFFSET_STRUCT
*/

SUMA_Boolean SUMA_GetOffset2Offset (SUMA_GET_OFFSET_STRUCT *GOS, SUMA_OFFSET_STRUCT *OS) 
{
   static char FuncName[]={"SUMA_GetOffset2Offset"};
   int il, jl, noffs;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!GOS || !OS) {
      SUMA_SL_Err("NULL input"); SUMA_RETURN(NOPE);
   }
   
   OS->N_Neighb = 0; 
   for (il=1; il<GOS->N_layers; ++il) {
      OS->N_Neighb += GOS->layers[il].N_NodesInLayer;
   }
   OS->Neighb_ind = (int *)SUMA_malloc(OS->N_Neighb * sizeof(int));
   OS->Neighb_dist = (float *)SUMA_malloc(OS->N_Neighb * sizeof(float));
   if (!OS->Neighb_ind || !OS->Neighb_dist) {
      SUMA_SL_Crit("Failed to allocate.");
      SUMA_RETURN(NOPE);
   }
   
   noffs = 0;
   for (il=1; il<GOS->N_layers; ++il) {
      for (jl=0; jl<GOS->layers[il].N_NodesInLayer; ++jl) {
         OS->Neighb_ind[noffs] = GOS->layers[il].NodesInLayer[jl]; 
         OS->Neighb_dist[noffs] = GOS->OffVect[OS->Neighb_ind[noffs]];
         ++noffs;
      }
   }
   
   SUMA_RETURN(YUP);
}

char * SUMA_ShowOffset_Info (SUMA_GET_OFFSET_STRUCT *OffS, int detail)
{
   static char FuncName[]={"SUMA_ShowOffset_Info"};
   SUMA_STRING *SS = NULL;
   int ii, *ltmp=NULL, *imap = NULL;
   char *s=NULL;   

   SUMA_ENTRY;

   SS = SUMA_StringAppend (NULL, NULL);

   if (!OffS) {
      SS = SUMA_StringAppend (SS,"#NULL offset structure.\n");
   } else {
      SS = SUMA_StringAppend_va (SS,"#Node Offsets (graph distance) from node %d\n", OffS->layers[0].NodesInLayer[0]);
      SS = SUMA_StringAppend_va (SS,"#Column 0 = Node index\n"
                                    "#column 1 = Neighborhood layer\n"
                                    "#Column 2 = Distance from node %d\n", OffS->layers[0].NodesInLayer[0]);
      ltmp = (int *)SUMA_malloc(OffS->N_Nodes*sizeof(int)); /* make a copy to avoid disturbinh OffS's contents*/
      if (!ltmp) {
         SUMA_SL_Crit("Failed to allocate for ltmp");
         SUMA_RETURN(NULL);
      }
      for (ii=0; ii<OffS->N_Nodes; ++ii) ltmp[ii] = OffS->LayerVect[ii]; 
      imap = SUMA_z_dqsort(ltmp,OffS->N_Nodes); 
      for (ii=0; ii<OffS->N_Nodes; ++ii) {
         if (OffS->LayerVect[imap[ii]] >= 0) {
            SS = SUMA_StringAppend_va (SS,"%6d\t%6d\t%f\n", imap[ii], OffS->LayerVect[imap[ii]], OffS->OffVect[imap[ii]]);
         }
      }
   }
   if (ltmp) SUMA_free(ltmp); ltmp = NULL;
   if (imap) SUMA_free(imap); imap = NULL;
   SUMA_SS2S(SS,s);

   SUMA_RETURN(s);
}

char * SUMA_ShowOffset_ll_Info (DList *list, int detail)
{
   static char FuncName[]={"SUMA_ShowOffset_ll_Info"};
   SUMA_STRING *SS = NULL;
   DListElmt *elm = NULL;
   SUMA_OFFSET_LL_DATUM *dat=NULL;
   int ii;
   char *s=NULL;   

   SUMA_ENTRY;

   SS = SUMA_StringAppend (NULL, NULL);

   if (!list) {
      SS = SUMA_StringAppend (SS,"#NULL offset list.\n");
   } else {
      do {
         if (!elm) elm = dlist_head(list); 
         else elm = elm->next;
         dat = (SUMA_OFFSET_LL_DATUM *)elm->data;
         if (elm == dlist_head(list)) {
            SS = SUMA_StringAppend_va (SS,"#Node Offsets (graph distance) from node %d\n", dat->ni);
            SS = SUMA_StringAppend_va (SS,"#Column 0 = Node index\n"
                                       "#column 1 = Neighborhood layer\n"
                                       "#Column 2 = Distance from node %d\n", dat->ni);
         }
         SS = SUMA_StringAppend_va (SS,"%6d\t%6d\t%f\n", dat->ni, dat->layer, dat->off);
      } while (elm != dlist_tail(list));
   }
   SUMA_SS2S(SS,s);

   SUMA_RETURN(s);
}

/*!
   For each node past layer 0, determine the propagation location prop_loc of that node. The 
   propagation direction is given by the vector formed by vector: node-->prop_loc
   Here's how the function works:
   For each node njl in layer il, 
      find nodes surrounding it and store njl and its neighbors in n_sur
      calculate the 1st principal component of the matrix formed by the coordinates of these nodes
      Form the plane that has the 1st principal component as a normal and that passes by njl
      Intersect the plane with those triangles that are not in the previous layer (i.e. intersect with triangles going forward)
      One triangle should intersect the plane and the intersection point (other than njl of course) is
      called the propagation location of node njl. 
*/
#define LOC_DBG 1
int SUMA_OffsetLayerPropagationLocation(SUMA_SurfaceObject *SO, SUMA_GET_OFFSET_STRUCT *OffS, float *PropLoc)
{
   static char FuncName[]={"SUMA_OffsetLayerPropagationLocation"};
   int n_sur[50], iseg, njl, njjll, jjll, in_sur, hit, itgood, it, il0, nil0, n2, n3, i;
   int N_Incident, N_IncidentMax=100, Incident[N_IncidentMax];
   int noffs = 0, il, jl, ip;
   float data_mat[50*3], *p1, *p2;
   double trace, pc_vec[9], pc_eig[3], Eq[4], pinter[3], pc0[3];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (SO->NodeDim != 3) {
      SUMA_S_Err("Need 3 dims for surf coords.");
      SUMA_RETURN(0);
   }
   if (!SO->EL) {
      SUMA_S_Err("Need SO->EL.");
      SUMA_RETURN(0);
   }
   i = OffS->layers[0].NodesInLayer[0];
   if (i==OffsetDebugNode) {
      LocalHead = YUP;
      SUMA_LHv("Debug mode for central node %d (must have LOC_DBG defined as 1): \n", i);
   }
   noffs = 0;
   for (il=1; il<OffS->N_layers; ++il) { /* for each layer */  
      for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) { /* for each node in layer */
         PropLoc[3*noffs  ] = PropLoc[3*noffs+1] = PropLoc[3*noffs+2] = 0.0 ;
         njl = OffS->layers[il].NodesInLayer[jl];
         in_sur = 0;
         n_sur[in_sur] = njl; ++in_sur; 
         /* SUMA_LHv("get nodes in layer %d, connected to node %d\n", il, njl); */
         for (jjll=0; jjll<OffS->layers[il].N_NodesInLayer; ++jjll) {
            njjll = OffS->layers[il].NodesInLayer[jjll];
            if (njjll != njl) {
               iseg = -1; 
               /* SUMA_LHv("Looking for edge: %d %d\n", njjll, njl); */
               if (njjll < njl) {SUMA_FIND_EDGE (SO->EL, njjll, njl, iseg);}
               else {SUMA_FIND_EDGE (SO->EL, njl, njjll, iseg);}
               /* SUMA_LHv("iseg is %d\n", iseg); */
               if (iseg < 0) { 
                  /* SUMA_LH("Segment not found."); */
               } else {
                  /* segment found, accept node in neighborhood */
                  if (in_sur > 49) {
                     SUMA_S_Crit("Hard limit exceeded! Fix it.");
                     exit(1);
                  }
                  n_sur[in_sur] = njjll; ++in_sur;
               }  
            }
         }
         if (in_sur < 2) {
            /* Node juts out with no nodes connected to it to meet the 
            layer's distance criterion. This node is going nowhere*/
            fprintf(SUMA_STDERR, "Warning %s: \n"
                                 "in_sur is %d\n"
                                 "Central node: %d\n"
                                 "Layer %d, node %d.\n"
                                 "%d nodes in this layer are:\n",
                                 FuncName, in_sur, i, il, n_sur[0], OffS->layers[il].N_NodesInLayer); 
            for (ip=0; ip<OffS->layers[il].N_NodesInLayer; ++ip) fprintf(SUMA_STDERR, "%d   ", OffS->layers[il].NodesInLayer[ip]);
            fprintf(SUMA_STDERR, "\nPropLoc[%d] is set to 0s.\n", noffs);
            goto NEW_OFFS;
         } else if (in_sur == 2) {
            /* Node has only one neighbor in layer. 
            This happens when other neighboring nodes were part of the
            previous layer. In this case, the direction of this node is towards its only
            neighbor */
            PropLoc[3*noffs+0] = SO->NodeList[3*n_sur[1]  ];
            PropLoc[3*noffs+1] = SO->NodeList[3*n_sur[1]+1];
            PropLoc[3*noffs+2] = SO->NodeList[3*n_sur[1]+2];
            SUMA_LHv("Storing lazy hit (noffs=%d)\n"
                     "PropLoc[%d]=%f %f %f\n", noffs, noffs, 
                     PropLoc[3*noffs+0], PropLoc[3*noffs+1], PropLoc[3*noffs+2]); 
            goto NEW_OFFS;
         } else if (in_sur > 6) {
            fprintf(SUMA_STDERR, "Warning %s: \n"
                                 "in_sur is %d\n"
                                 "Although it is common for it \n"
                                 "to be slightly larger than 3.\n"
                                 "the value observed seems excessive.\n"
                                 "check mesh at nodes below\n"
                                 "Central node: %d\n"
                                 "Layer %d, node %d.\n",
                                 FuncName, in_sur, i, il, n_sur[0]); 
         } 
         /* Now we have in n_sur, the indces of nodes that would enter the PCA */
         for (ip=0; ip<in_sur; ++ip) { /* load the coords */
               data_mat[ip  ] = SO->NodeList[3*n_sur[ip]  ];
               data_mat[ip+3] = SO->NodeList[3*n_sur[ip]+1];
               data_mat[ip+6] = SO->NodeList[3*n_sur[ip]+2];
         }

         #if LOC_DBG
         SUMA_LHv("Calculating pca of nodes %d %d %d:\n"
                  "[%f %f %f\n%f %f %f\n%f %f %f]\n",
                  n_sur[0], n_sur[1], n_sur[2],
                  SO->NodeList[3*n_sur[0]],SO->NodeList[3*n_sur[0]+1],SO->NodeList[3*n_sur[0]+2], 
                  SO->NodeList[3*n_sur[1]],SO->NodeList[3*n_sur[1]+1],SO->NodeList[3*n_sur[1]+2], 
                  SO->NodeList[3*n_sur[2]],SO->NodeList[3*n_sur[2]+1],SO->NodeList[3*n_sur[2]+2]); 
         #endif
                  
         if ((trace = pca_fast3 (data_mat, in_sur, 1, pc_vec, pc_eig)) < 0) {
            fprintf(SUMA_STDERR, "Warning %s:\n"
                                 "Failed calculating PC at\n"
                                 "Central node: %d\n"
                                 "Layer %d, nodes for pca: %d %d %d\n",
                                 FuncName, i, il, n_sur[0], n_sur[1], n_sur[2]);
            goto NEW_OFFS;
         } 
         /* Have PC, will travel */
         /* Find equation of plane passing by node n_sur[0] and having the PC for a normal */
         pc0[0] = pc_vec[0]; pc0[1] = pc_vec[3]; pc0[2] = pc_vec[6];
         p1 = &(SO->NodeList[3*n_sur[0]]);
         SUMA_LHv("   Forming plane with normal [%f %f %f], passing by point [%f %f %f]\n", 
            pc0[0], pc0[1], pc0[2], p1[0], p1[1], p1[2]); 
         SUMA_PLANE_NORMAL_POINT(pc0, p1, Eq);
         /* Then find the triangle that is      incident to node n_sur[0] 
                                          AND   not part of the previous layer 
                                          AND   intersects the plane Eq*/
         N_Incident = N_IncidentMax; /* pass limit to SUMA_Get_NodeIncident */
         if (!SUMA_Get_NodeIncident(n_sur[0], SO, Incident, &N_Incident)) {
            SUMA_S_Err("Failed to get incident triangles.");
            goto NEW_OFFS;
         }
         if (!N_Incident) {
            SUMA_S_Err("No incident triangles");
            goto NEW_OFFS;
         }
         /* of those triangles, which ones have a node in the previous layer? */
         SUMA_LHv("   Searching for acceptable triangles out of a total of %d\n", N_Incident); 
         itgood = 0;
         for (it=0; it<N_Incident; ++it) {
            hit = 0;
            for (il0=0; il0<OffS->layers[il-1].N_NodesInLayer;++il0) {
               nil0 = OffS->layers[il-1].NodesInLayer[il0];
               if (  SO->FaceSetList[3*Incident[it]  ] == nil0 || 
                     SO->FaceSetList[3*Incident[it]+1] == nil0 || 
                     SO->FaceSetList[3*Incident[it]+2] == nil0) {
                  hit = 1;
                  break;
               }
            }
            if (!hit) {
               Incident[itgood] = Incident[it];
               ++itgood;
            }
         } 
         N_Incident = itgood;
         if (LocalHead) {
            fprintf (SUMA_STDERR,"%s:\n"
                                 "    Which of remaining %d triangles intersect the plane [%.2f %.2f %.2f %.2f]\n"
                                 "    Triangles:\n",
                                 FuncName, N_Incident,Eq[0], Eq[1], Eq[2], Eq[3]); 
            for (it=0; it<N_Incident; ++it) {
               fprintf (SUMA_STDERR,"   %d", Incident[it]);
            }
            fprintf (SUMA_STDERR,"\n");
         }
         /* Now, which of these good triangles intersect the plane ?*/
         hit = 0;
         for (it=0; it<N_Incident; ++it) {
            SUMA_LHv("      Checking Triangle %d\n", Incident[it]); 
            SUMA_TWO_OTHER_TRIANGLE_NODES(n_sur[0],Incident[it],SO->FaceSetList,n2,n3);
            if (n2 < 0 || n3 < 0) {
               SUMA_S_Err("Failed to find other nodes in triangle");
               goto NEW_OFFS;
            } 
            p1 = &(SO->NodeList[3*n2]);
            p2 = &(SO->NodeList[3*n3]);
            SUMA_LHv("Calling SUMA_SEGMENT_PLANE_INTERSECT, nodes of triangle are %d %d %d\n", n_sur[0], n2, n3); 
            SUMA_SEGMENT_PLANE_INTERSECT(p1, p2, Eq, hit, pinter);
            if (hit) {  /* OK, have hit, and the propagation direction */
               PropLoc[3*noffs+0] = pinter[0];
               PropLoc[3*noffs+1] = pinter[1];
               PropLoc[3*noffs+2] = pinter[2];
               SUMA_LHv("Storing hit (noffs=%d)\n"
                        "PropLoc[%d]=%f %f %f\n", noffs, noffs, 
                        PropLoc[3*noffs+0], PropLoc[3*noffs+1], PropLoc[3*noffs+2]);
               break;
            } 
         }
         if (!hit) {
            SUMA_LHv("             Failed to get hit (noffs=%d)\n", noffs);
         }
         
         NEW_OFFS:
         SUMA_LHv("Done with noffs=%d\n", noffs); 
         ++noffs;
      }  /* for each node in layer */
   }  /* for each layer */  
   SUMA_RETURN(1);
}

/*!
   \brief creates a vector of node neighbors structures such that:
   OffS = SUMA_FormNeighbOffset ( SUMA_SurfaceObject *SO, float OffsetLim, const char *Opts, byte *nmask, float FWHM);
   
   \param OffS (SUMA_OFFSET_STRUCT *) SO->Node x 1 vector of structures 
         OffS[i] is a structure containing node neighbors of node i
         OffS[i].N_Neighb (int) number of neighbors of node i
         OffS[i].Neighb_ind (int *) OffS[i].N_Neighb x 1 vector containing 
                                    nodes neighboring node i
         OffS[i].Neighb_dist (float *) OffS[i].N_Neighb x 1 vector containing 
                                    node distances from node i. 
                                    These are the shortest distances ON THE GRAPH.
                                    The distances might be larger than OffsetLim
                                    because the child function SUMA_getoffsets2
                                    does so.
         OffS[i].Prop_Direc (float *) OffS[i].N_Neighb x 3 vector containing 
                                    contour propagation directions at each node,
                                    in layers 1 and above. This is only created
                                    if Opts has "DoProp" in it.           
                           
   \param OffsetLim (float) maximal inclusive neighbor distance. In reality, some 
                           nodes may be farther apart. But all nodes closer than 
                           OffsetLim to each node will be included
   \param Opts (const char *) if contains the string "DoProp" then the function
                              also calculates the propagation directions.
                              Default is NULL, no extras.
   \param nmask (byte *) a mask vector, if non null, then only nodes n, where nmask[n] == 1
                           are processed 
   \param FWHM (float) if > 0, then the distances x are changed in Neighb_dist are 
                              changed to G(x) = 1/(sqrt(2pi)Sig)*exp(-(x*x)/(2*Sig*Sig)) 
                              where Sig is calculated from FWHM by Sig = FWHM /   2.354820;
   - NOTE: This function will chew up a lot of memory, real quick.
            An approximate equation for the size needed for OffS:
               (mean_N_Neighb_per_Node * 8 + 12) * SO->N_Node Bytes
               With OffsetLim = 10 and a FreeSurfer surface of 150'000 nodes,
               the average number of neighbors per node is ~800. 
               Which means 962MB for the returned structure.
         When memory usage is this high, you should consider using SUMA_getoffsets2
         repeatedly for each node, as is done in this function.
         
*/

SUMA_OFFSET_STRUCT *SUMA_FormNeighbOffset ( SUMA_SurfaceObject *SO, float OffsetLim, const char *opts, byte *nmask, float FWHM)
{
   static char FuncName[]={"SUMA_FormNeighbOffset"};
   int i, ii, il, jl, ip, noffs,  DoProp = 0, iproc, N_mask;
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   struct  timeval start_time;
   double scl=0.0, ds2=0.0, sig, d;
   float etime_GetOffset, mean_N_Neighb, dist, dist_norm;   
   SUMA_OFFSET_STRUCT *OffS_out=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN(NULL); }
   if (!SO->FN || !SO->EL) {
      /* do it here */
      if (!SO->EL && !(SO->EL = SUMA_Make_Edge_List_eng (SO->FaceSetList, SO->N_FaceSet, SO->N_Node, SO->NodeList, 0, SO->idcode_str))) {
         SUMA_S_Err("Failed to create Edge_List");
         SUMA_RETURN(NULL);
      }
      if (!SO->FN && !(SO->FN = SUMA_Build_FirstNeighb( SO->EL, SO->N_Node, SO->idcode_str)) ) {
         SUMA_S_Err("Failed to create FirstNeighb");
         SUMA_RETURN(NULL);
      }
   }
   
   if (SUMA_iswordin(opts,"DoProp") == 1) {
      SUMA_LH("Propagation Directions Will Be Computed");
      DoProp = 1;
   }

   /* calculate sigma */
   if (FWHM > 0.0) {
      sig = FWHM /   2.354820;
      ds2 = 2*sig*sig;
      scl = iSQ_2PI/sig;
   } else sig = -1.0;
   /* calculate the offset limit, if allowed */
   if (OffsetLim < 0.0) {
      if (sig > 0.0) {
         OffsetLim = 3.5*sig;
      } else {
         SUMA_S_Errv("Have OffsetLim =%f and no FWHM (%f) from which to estimate it.\n", OffsetLim, FWHM);
         SUMA_RETURN(NULL);
      }
   }
   SUMA_LHv("OffsetLim set to %f\nSigma set to %f\n", OffsetLim, sig);
   
   N_mask=0;
   if (nmask) {
      for (i=0; i<SO->N_Node; ++i) if (nmask[i]) ++N_mask;
   } else N_mask = SO->N_Node;
   
   OffS_out = (SUMA_OFFSET_STRUCT *)SUMA_malloc(SO->N_Node * sizeof(SUMA_OFFSET_STRUCT));
   
   SUMA_etime(&start_time,0);
   
   
   OffS = SUMA_Initialize_getoffsets (SO->N_Node);
   mean_N_Neighb = 0;
   dist_norm = 1.1 * OffsetLim;
   iproc = 0;
   for (i=0; i < SO->N_Node; ++i) {
      OffS_out[i].N_Neighb = 0;
      OffS_out[i].Neighb_ind = NULL;
      OffS_out[i].Neighb_dist = NULL; 
      OffS_out[i].Neighb_PropLoc = NULL;
      if (!nmask || nmask[i]) {
         SUMA_getoffsets2 (i, SO, OffsetLim, OffS, NULL, 0);
         /* Now store all the relevant info in OffS in OffS_out[i] */
         for (il=1; il<OffS->N_layers; ++il) {
            OffS_out[i].N_Neighb += OffS->layers[il].N_NodesInLayer;
         }
         OffS_out[i].Neighb_ind = (int *)SUMA_malloc(OffS_out[i].N_Neighb * sizeof(int));
         OffS_out[i].Neighb_dist = (float *)SUMA_malloc(OffS_out[i].N_Neighb * sizeof(float));
         if (DoProp) {
            OffS_out[i].Neighb_PropLoc = (float*)SUMA_malloc(OffS_out[i].N_Neighb * sizeof(float)*3);
         }  
         mean_N_Neighb += OffS_out[i].N_Neighb;
         noffs = 0;
         for (il=1; il<OffS->N_layers; ++il) {
            for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) {
               OffS_out[i].Neighb_ind[noffs] = OffS->layers[il].NodesInLayer[jl]; 
               #if 1
               /* don't play fancy with the weights here */
               d = OffS->OffVect[OffS_out[i].Neighb_ind[noffs]];
               if (sig > 0.0) {
                  OffS_out[i].Neighb_dist[noffs] = scl * exp(-(d*d)/(ds2));
               }  else {
                  OffS_out[i].Neighb_dist[noffs] = d;
               }
               #else
               dist = OffS->OffVect[OffS_out[i].Neighb_ind[noffs]];
               if (dist > OffsetLim) OffS_out[i].Neighb_dist[noffs] = 0;
               else OffS_out[i].Neighb_dist[noffs] = (dist ); 
               #endif
               ++noffs;
            }
         }

         if (DoProp) {
            SUMA_LH("Going to SUMA_OffsetLayerPropagationLocation\n");
            if (!SUMA_OffsetLayerPropagationLocation(SO, OffS, OffS_out[i].Neighb_PropLoc)) {
               SUMA_S_Err("Failed to calculation propagation location.");
            }  
            SUMA_LH("Done with SUMA_OffsetLayerPropagationLocation\n");
         }

         /* Show me the offsets for one node*/
         if (0) {
            if (i == OffsetDebugNode) {
               FILE *fid=NULL;
               char *outname=NULL;
               outname = SUMA_Extension("SomethingOffset", ".1D", YUP);
               outname = SUMA_append_replace_string(outname, "offset.1D", "", 1);
               fid = fopen(outname, "w"); free(outname); outname = NULL;
               if (!fid) {
                  SUMA_SL_Err("Could not open file for writing.\nCheck file permissions, disk space.\n");
               } else {
                  fprintf (fid,"#Column 1 = Node index\n"
                               "#column 2 = Neighborhood layer\n"
                               "#Column 3 = Distance from node %d\n", OffsetDebugNode);
                  for (ii=0; ii<SO->N_Node; ++ii) {
                     if (OffS->LayerVect[ii] >= 0) {
                        fprintf(fid,"%d\t%d\t%f\n", ii, OffS->LayerVect[ii], OffS->OffVect[ii]);
                     }
                  }
                  fclose(fid);
               }
            }
         }
         ++iproc;
         if (iproc == 100) {
            etime_GetOffset = SUMA_etime(&start_time,1);
            fprintf(SUMA_STDERR, "%s: Search to %f mm took %f seconds for %d nodes.\n"
                                 "Projected completion time: %f minutes\n"
                                 "Projected memory need for structure %f MB\n", 
                                 FuncName, OffsetLim, etime_GetOffset, iproc, 
                                 etime_GetOffset * N_mask / 60.0 / (float)(iproc),
                                 (mean_N_Neighb / (iproc) * 8 + 12)* N_mask/1000000.0);
         }
         SUMA_Recycle_getoffsets (OffS);
      } /* if node in mask */
   }
   SUMA_Free_getoffsets(OffS); OffS = NULL;
   
   etime_GetOffset = SUMA_etime(&start_time,1);
   fprintf(SUMA_STDERR, "%s: Search to %f mm took %f seconds for %d nodes.\n"
                        "Mean number of neighbors per node: %f\n",
                        FuncName, OffsetLim, etime_GetOffset, SO->N_Node, mean_N_Neighb / SO->N_Node);
   
   SUMA_RETURN(OffS_out);           
} 

/*!
   \brief frees what is created by SUMA_FormNeighbOffset
   
   \sa SUMA_FormNeighbOffset
*/
SUMA_OFFSET_STRUCT * SUMA_free_NeighbOffset (SUMA_SurfaceObject *SO, SUMA_OFFSET_STRUCT *OffS_out)
{
   static char FuncName[]={"SUMA_free_NeighbOffset"};
   int i;
   SUMA_ENTRY;

   if (!SO) {
      SUMA_S_Err("NULL SO!");
      SUMA_RETURN(NULL);
   }
   if (!OffS_out) SUMA_RETURN(NULL);
   for (i=0; i < SO->N_Node; ++i) {
      OffS_out[i].N_Neighb = 0;
      if (OffS_out[i].Neighb_dist) SUMA_free(OffS_out[i].Neighb_dist); OffS_out[i].Neighb_dist = NULL;
      if (OffS_out[i].Neighb_ind) SUMA_free(OffS_out[i].Neighb_ind); OffS_out[i].Neighb_ind = NULL;
      if (OffS_out[i].Neighb_PropLoc) SUMA_free(OffS_out[i].Neighb_PropLoc); OffS_out[i].Neighb_PropLoc = NULL;
   }
   SUMA_free(OffS_out); 
   SUMA_RETURN(NULL);
}

/*!
   \brief A filtering function that is based on brute force estimates of node neighbor distance
   matrix. 
   It is not finished because it makes no use of the neighbor distances to properly weigh the interpolation.
   It ends up being too slow because of the high memory load for computing OffS_out
*/
float *SUMA_Offset_GeomSmooth( SUMA_SurfaceObject *SO, int N_iter, float OffsetLim, float *fin_orig, 
                              int vpn, SUMA_INDEXING_ORDER d_order, float *fout_final_user,
                              SUMA_COMM_STRUCT *cs)
{
   
   static char FuncName[]= {"SUMA_Offset_GeomSmooth"};
   float *fout_final=NULL, *fbuf=NULL, *fin_next=NULL, *fin=NULL, *fout=NULL;
   int niter=0, i, il,jl, j, ii,  noffs;
   struct  timeval start_time;
   float etime_GetOffset, weight_tot;   
   SUMA_OFFSET_STRUCT *OffS_out=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN(NULL); }
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN");
      SUMA_RETURN(NULL);
   }   
   if (N_iter % 2) {
      SUMA_SL_Err("N_iter must be an even number\n");
      SUMA_RETURN(NULL);
   }
   if (vpn < 1) {
      SUMA_SL_Err("vpn < 1\n");
      SUMA_RETURN(NULL);
   }  
   
   if (fout_final_user == fin_orig) {
      SUMA_SL_Err("fout_final_user == fin_orig");
      SUMA_RETURN(NULL);
   }
   
   if (!fout_final_user) { /* allocate for output */
      fout_final = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
      if (!fout_final) {
         SUMA_SL_Crit("Failed to allocate for fout_final\n");
         SUMA_RETURN(NULL);
      }
   }else {
      fout_final = fout_final_user; /* pre-allocated */
   }
   
   /* allocate for buffer */
   fbuf = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
   if (!fbuf) {
      SUMA_SL_Crit("Failed to allocate for fbuf\n");
      SUMA_RETURN(NULL);
   }
   
   
   if (cs->Send) { /* send the first monster */
      if (!SUMA_SendToSuma (SO, cs, (void *)fin_orig, SUMA_NODE_XYZ, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
   }
   SUMA_LH("Calculating OffS_out ...");
   OffS_out = SUMA_FormNeighbOffset (SO, OffsetLim, NULL, NULL, -1.0);
   fin_next = fin_orig;
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (niter=0; niter < N_iter; ++niter) {
            if ( niter % 2 ) { /* odd */
               fin = fin_next; /* input from previous output buffer */
               fout = fout_final; /* results go into final vector */
               fin_next = fout_final; /* in the next iteration, the input is from fout_final */
            } else { /* even */
               /* input data is in fin_new */
               fin = fin_next;
               fout = fbuf; /* results go into buffer */
               fin_next = fbuf; /* in the next iteration, the input is from the buffer */
            }
            
            SUMA_etime(&start_time,0);
            
            for (i=0; i < SO->N_Node; ++i) {
               for (j=0; j < vpn; ++j) {
                  /* do the averaging using OffS_out NO ATTENTION IS GIVEN TO PROPER WEIGHING YET!*/
                  fout[i*vpn+j] = fin[i*vpn+j];
                  for (il=0; il<OffS_out[i].N_Neighb; ++il) {
                     fout[i*vpn+j] += fin[OffS_out[i].Neighb_ind[il]*vpn+j];
                  }
                  fout[i*vpn+j] /= (OffS_out[i].N_Neighb+1);  
               }
            }
            
            etime_GetOffset = SUMA_etime(&start_time,1);
            fprintf(SUMA_STDERR, "%s: Smoothing at dist %f took %f seconds for %d nodes.\n",
                           FuncName, OffsetLim, etime_GetOffset, SO->N_Node);
            
            if (cs->Send) {
               if (!SUMA_SendToSuma (SO, cs, (void *)fout, SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
               }
            }
         }
         break;
      case SUMA_COLUMN_MAJOR:
         SUMA_SL_Err("Column Major not implemented");
         SUMA_RETURN(NULL);
         break;
      default:
         SUMA_SL_Err("Bad Major, very bad.\n");
         SUMA_RETURN(NULL);
         break;
   }
   
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;

   /* Have to free OffS_out */
   OffS_out = SUMA_free_NeighbOffset (SO, OffS_out);
      
   SUMA_RETURN(fout);    
}

float *SUMA_NN_GeomSmooth( SUMA_SurfaceObject *SO, int N_iter, float *fin_orig, 
                           int vpn, SUMA_INDEXING_ORDER d_order, float *fout_final_user,
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask)
{
   static char FuncName[]= {"SUMA_NN_GeomSmooth"};
   float *fout_final=NULL, *fbuf=NULL, *fin_next=NULL, *fin=NULL, *fout=NULL;
   int niter=0;

   SUMA_ENTRY;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN(NULL); }
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN");
      SUMA_RETURN(NULL);
   }   
   if (N_iter % 2) {
      SUMA_SL_Err("N_iter must be an even number\n");
      SUMA_RETURN(NULL);
   }
   if (vpn < 1) {
      SUMA_SL_Err("vpn < 1\n");
      SUMA_RETURN(NULL);
   }  
   
   if (fout_final_user == fin_orig) {
      SUMA_SL_Err("fout_final_user == fin_orig");
      SUMA_RETURN(NULL);
   }
   
   if (!fout_final_user) { /* allocate for output */
      fout_final = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
      if (!fout_final) {
         SUMA_SL_Crit("Failed to allocate for fout_final\n");
         SUMA_RETURN(NULL);
      }
   }else {
      fout_final = fout_final_user; /* pre-allocated */
   }
   
   /* allocate for buffer */
   fbuf = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
   if (!fbuf) {
      SUMA_SL_Crit("Failed to allocate for fbuf\n");
      SUMA_RETURN(NULL);
   }
   
   
   if (cs->Send) { /* send the first monster */
      if (!SUMA_SendToSuma (SO, cs, (void *)fin_orig, SUMA_NODE_XYZ, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
   }
   
   fin_next = fin_orig;
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (niter=0; niter < N_iter; ++niter) {
            if ( niter % 2 ) { /* odd */
               fin = fin_next; /* input from previous output buffer */
               fout = fout_final; /* results go into final vector */
               fin_next = fout_final; /* in the next iteration, the input is from fout_final */
            } else { /* even */
               /* input data is in fin_new */
               fin = fin_next;
               fout = fbuf; /* results go into buffer */
               fin_next = fbuf; /* in the next iteration, the input is from the buffer */
            }
            fout = SUMA_SmoothAttr_Neighb ( fin, vpn*SO->N_Node, 
                                                 fout, SO->FN, vpn, nmask, strict_mask);
            if (cs->Send) {
               if (!SUMA_SendToSuma (SO, cs, (void *)fout, SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
               }
            }
         }
         break;
      case SUMA_COLUMN_MAJOR:
         SUMA_SL_Err("Column Major not implemented");
         SUMA_RETURN(NULL);
         break;
      default:
         SUMA_SL_Err("Bad Major, very bad.\n");
         SUMA_RETURN(NULL);
         break;
   }
   
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;

   SUMA_RETURN(fout);    
}
        

/*!
   \brief Filter data defined on the surface using M.K. Chung et al.'s method (Neuroimage 03)
   dm_smooth = SUMA_Chung_Smooth (SO, wgt, N_iter, FWHM, fin, vpn, d_order, fout_user, SUMA_COMM_STRUCT *cs);
   
   \param SO (SUMA_SurfaceObject *) Surface object with valid 
                                    SO->NodeList, SO->FaceSetList and SO->FN 
   \param wgt (float **) interpolation weights for each node. 
                           These weights are obtained from SUMA_Chung_Smooth_Weights 
   \param N_iter (int) number of smoothing iterations (must be even, > 1) 
   \param FWHM (float) Full Width at Half Maximum of equivalent Gaussian filter
   \param fin (float *) vector containing node data. The length of this vector
                        is vpn x SO->N_Node , where vpn is the number of values
                        per node. 
   \param vpn (int) the numberof values per node in fin 
   \param d_order (SUMA_INDEXING_ORDER) Indicates how multiple values per node are stored in fin
                        SUMA_ROW_MAJOR: The data in fin is stored in *** Row Major *** order.
                        The ith value (start at 0) for node n is at index fin[vpn*n+i]
                        SUMA_COLUMN_MAJOR: The data in fin is stored in *** Column Major *** order.
                        The ith (start at 0) value for node n is at index fin[n+SO->N_Node*i]; 
                        etc...
   \param fout_user (float *) a pointer to the vector where the smoothed version of fin will reside
                        You can pass NULL and the function will do the allocation for you and return 
                        the pointer to the smoothed data in dm_smooth.
                        If you already have space allocated for the result, then pass the pointer
                        in fout_user and save on allocation time and space. In that case, dm_smooth
                        is equal to fout_user.
                        Either way, you are responsible for freeing memory pointed to by dm_smooth
                        DO NOT PASS fout_user = fin 
   \param cs (SUMA_COMM_STRUCT *) A pointer to the structure containing info for taking to SUMA
                           
   \return dm_smooth (float *) A pointer to the smoothed data (vpn * SO->N_Node values). 
                        You will have to free the memory allocated for dm_smooth yourself.
                        
   \sa SUMA_Chung_Smooth_Weights
                        
*/


float * SUMA_Chung_Smooth (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, float *fin_orig, 
                           int vpn, SUMA_INDEXING_ORDER d_order, float *fout_final_user,
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask)
{
   static char FuncName[]={"SUMA_Chung_Smooth"};
   float *fout_final = NULL, *fbuf=NULL, *fin=NULL, *fout=NULL, *fin_next = NULL;
   float delta_time, fp, dfp, fpj, minfn=0.0, maxfn=0.0;
   int n , k, j, niter, vnk, os, jj, nj=-1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (N_iter % 2) {
      SUMA_SL_Err("N_iter must be an even number\n");
      SUMA_RETURN(NULL);
   }
   
   if (!SO || !wgt || !fin_orig) {
      SUMA_SL_Err("NULL SO or wgt or fin_orig\n");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN\n");
      SUMA_RETURN(NULL);
   }
   
   if (vpn < 1) {
      SUMA_SL_Err("vpn < 1\n");
      SUMA_RETURN(NULL);
   }  
   
   if (fout_final_user == fin_orig) {
      SUMA_SL_Err("fout_final_user == fin_orig");
      SUMA_RETURN(NULL);
   }
   
   if (!fout_final_user) { /* allocate for output */
      fout_final = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
      if (!fout_final) {
         SUMA_SL_Crit("Failed to allocate for fout_final\n");
         SUMA_RETURN(NULL);
      }
   }else {
      fout_final = fout_final_user; /* pre-allocated */
   }
   
   /* allocate for buffer */
   fbuf = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
   if (!fbuf) {
      SUMA_SL_Crit("Failed to allocate for fbuf\n");
      SUMA_RETURN(NULL);
   }
   
   
   if (cs->Send) { /* send the first monster */
      if (!SUMA_SendToSuma (SO, cs, (void *)fin_orig, SUMA_NODE_RGBAb, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
   }
   
   fin_next = fin_orig;
   delta_time= (FWHM * FWHM)/(16*N_iter*log(2));
   switch (d_order) {
      case SUMA_COLUMN_MAJOR:
         for (niter=0; niter < N_iter; ++niter) {
            if ( niter % 2 ) { /* odd */
               fin = fin_next; /* input from previous output buffer */
               fout = fout_final; /* results go into final vector */
               fin_next = fout_final; /* in the next iteration, the input is from fout_final */
            } else { /* even */
               /* input data is in fin_new */
               fin = fin_next;
               fout = fbuf; /* results go into buffer */
               fin_next = fbuf; /* in the next iteration, the input is from the buffer */
            }
            for (k=0; k < vpn; ++k) {
               os = SO->N_Node*k;   /* node value indexing offset */
               for (n=0; n < SO->N_Node; ++n) {
                  vnk = n+os; /* index of kth value at node n */
                  fp = fin[vnk]; /* kth value at node n */
                  dfp = 0.0;
                  if (!nmask) {
                     if (SO->FN->N_Neighb[n]) minfn = maxfn = fin[SO->FN->FirstNeighb[n][0]+os];
                     for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                        fpj = fin[SO->FN->FirstNeighb[n][j]+os]; /* value at jth neighbor of n */
                        if (fpj < minfn) minfn = fpj;
                        if (fpj > maxfn) maxfn = fpj;
                        dfp += wgt[n][j] * (fpj - fp); 
                     }/* for j*/
                     fout[vnk] = fin[vnk] + delta_time * dfp;
                     if (fout[vnk] < minfn) fout[vnk] = minfn;
                     if (fout[vnk] > maxfn) fout[vnk] = maxfn;
                  } else { /* masking potential */
                     if (nmask[n]) {
                        if (SO->FN->N_Neighb[n]) {
                           jj = 0;
                           if (strict_mask) { /* consider only neighbors that are in mask */
                              do {
                                 minfn = maxfn = fin[SO->FN->FirstNeighb[n][jj]+os]; ++jj;
                              } while (!nmask[SO->FN->FirstNeighb[n][jj]] && jj < SO->FN->N_Neighb[n]);
                           } else { /* consider all neighbors */
                              minfn = maxfn = fin[SO->FN->FirstNeighb[n][jj]+os];
                           }
                        }
                        for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                           nj = SO->FN->FirstNeighb[n][j];
                           if (nmask[nj] || !strict_mask) { /* consider only neighbors that are in mask if strict_mask is 1*/
                              fpj = fin[nj+os]; /* value at jth neighbor of n */
                              if (fpj < minfn) minfn = fpj;
                              if (fpj > maxfn) maxfn = fpj;
                              dfp += wgt[n][j] * (fpj - fp);
                           } 
                        }/* for j*/
                        fout[vnk] = fin[vnk] + delta_time * dfp;
                        if (fout[vnk] < minfn) fout[vnk] = minfn;
                        if (fout[vnk] > maxfn) fout[vnk] = maxfn;
                     } else {
                        fout[vnk] = fin[vnk];
                     }
                  }
               }/* for n */ 
                 
               if (cs->Send) {
                  if (!SUMA_SendToSuma (SO, cs, (void *)fout, SUMA_NODE_RGBAb, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
            } /* for k */
         }/* for niter */
         break;
      case SUMA_ROW_MAJOR:
         SUMA_SL_Err("Row Major not implemented");
         SUMA_RETURN(NULL);
         break;
      default:
         SUMA_SL_Err("Bad Major, very bad.\n");
         SUMA_RETURN(NULL);
         break;
   }
   
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;
               
   SUMA_RETURN(fout);
}


/*!
   \brief A version of SUMA_Chung_Smooth that works on SUMA_DSET rather than
   a float array.
   NOTE: dset is changed on output (blurred values) but idcode
   remains unchanged
*/
SUMA_Boolean SUMA_Chung_Smooth_dset (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, SUMA_DSET *dset, 
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask)
{
   static char FuncName[]={"SUMA_Chung_Smooth_dset"};
   float *fout_final = NULL, *fbuf=NULL, *fin=NULL, *fout=NULL, *fin_next = NULL, *fin_orig = NULL;
   float delta_time, fp, dfp, fpj, minfn=0.0, maxfn=0.0;
   int n , k, j, niter, jj, *icols=NULL, N_icols, N_nmask, nj=-1;
   byte *bfull=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (N_iter % 2) {
      SUMA_SL_Err("N_iter must be an even number\n");
      SUMA_RETURN(NOPE);
   }
   
   if (!SO || !wgt || !dset) {
      SUMA_SL_Err("NULL SO or wgt or dset\n");
      SUMA_RETURN(NOPE);
   }
   
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN\n");
      SUMA_RETURN(NOPE);
   }
    
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(dset, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NOPE);   
   }
   
   /* allocate for buffer and output */
   fbuf = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   fout_final = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   if (!fbuf || !fout_final) {
      SUMA_SL_Crit("Failed to allocate for fbuf and fout_final\n");
      SUMA_RETURN(NOPE);
   }
   
   if (cs->Send && N_icols > 1) {
      SUMA_S_Warn("Only 1st data column will be sent to SUMA in talk_mode.");
   }
   
   delta_time= (FWHM * FWHM)/(16*N_iter*log(2));
   
   /* Begin filtering operation for each column */
   for (k=0; k < N_icols; ++k) {
      /* get a float copy of the data column */
      fin_orig = SUMA_DsetCol2Float (dset, icols[k], 1);
      if (!fin_orig) {
         SUMA_SL_Crit("Failed to get copy of column. Woe to thee!");
         SUMA_RETURN(NOPE);
      }
      /* make sure column is not sparse, one value per node */
      if (k==0) {
         SUMA_LH( "Special case k = 0, going to SUMA_MakeSparseColumnFullSorted");
         bfull = NULL;
         if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(dset), 0.0, &bfull, dset, SO->N_Node)) {
            SUMA_S_Err("Failed to get full column vector");
            SUMA_RETURN(NOPE);
         }
         if (bfull) {
            SUMA_LH( "Something was filled in SUMA_MakeSparseColumnFullSorted\n" );
            /* something was filled in good old SUMA_MakeSparseColumnFullSorted */
            if (nmask) {   /* combine bfull with nmask */
               SUMA_LH( "Merging masks\n" );
               for (jj=0; jj < SO->N_Node; ++jj) { if (nmask[jj] && !bfull[jj]) nmask[jj] = 0; }   
            } else { nmask = bfull; }
         } 
         if (nmask) {
            N_nmask = 0;
            for (n=0; n<SO->N_Node; ++n) { if (nmask[n]) ++ N_nmask; }
            SUMA_LHv("Blurring with node mask (%d nodes in mask)\n", N_nmask);
            if (!N_nmask) {
               SUMA_S_Warn("Empty mask, nothing to do");
               goto CLEANUP;
            }
         }
      } else {
         SUMA_LH( "going to SUMA_MakeSparseColumnFullSorted");
         if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(dset), 0.0, NULL, dset, SO->N_Node)) {
            SUMA_S_Err("Failed to get full column vector");
            SUMA_RETURN(NOPE);
         }
         /* no need for reworking nmask and bfull for each column...*/
      }
           
      if (cs->Send && k == 0) { /* send the first monster */
         if (!SUMA_SendToSuma (SO, cs, (void *)fin_orig, SUMA_NODE_RGBAb, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
      /* filter this column for each of the iterations */
      fin_next = fin_orig;
      for (niter=0; niter < N_iter; ++niter) {
         SUMA_LHv("niter %d\n", niter);
         if ( niter % 2 ) { /* odd */
            fin = fin_next; /* input from previous output buffer */
            fout = fout_final; /* results go into final vector */
            fin_next = fout_final; /* in the next iteration, the input is from fout_final */
         } else { /* even */
            /* input data is in fin_new */
            fin = fin_next;
            fout = fbuf; /* results go into buffer */
            fin_next = fbuf; /* in the next iteration, the input is from the buffer */
         }
         /* filter iteration for each node in data column k*/
         if (!nmask) {
            for (n=0; n < SO->N_Node; ++n) {
               /*SUMA_LHv("node %d\n", n);*/
               fp = fin[n]; /* kth value at node n */
               dfp = 0.0;
               if (SO->FN->N_Neighb[n]) minfn = maxfn = fin[SO->FN->FirstNeighb[n][0]];
               for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                  fpj = fin[SO->FN->FirstNeighb[n][j]]; /* value at jth neighbor of n */
                  if (fpj < minfn) minfn = fpj;
                  if (fpj > maxfn) maxfn = fpj;
                  dfp += wgt[n][j] * (fpj - fp); 
               }/* for j*/
               fout[n] = fin[n] + delta_time * dfp;
               if (fout[n] < minfn) fout[n] = minfn;
               if (fout[n] > maxfn) fout[n] = maxfn;
            }/* for n */ 
         } else {  /* masking potential */
            for (n=0; n < SO->N_Node; ++n) {
               /*SUMA_LHv("node %d\n", n);*/
               fp = fin[n]; /* kth value at node n */
               dfp = 0.0;
               if (nmask[n]) {
                  if (SO->FN->N_Neighb[n]) {
                     jj = 0;
                     if (strict_mask) { /* consider only neighbors that are in mask */
                        do {
                           minfn = maxfn = fin[SO->FN->FirstNeighb[n][jj]]; ++jj;
                        } while (!nmask[SO->FN->FirstNeighb[n][jj]] && jj < SO->FN->N_Neighb[n]);
                     } else { /* consider all neighbors */
                        minfn = maxfn = fin[SO->FN->FirstNeighb[n][jj]];
                     }
                  }
                  for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                     nj = SO->FN->FirstNeighb[n][j];
                     if (nmask[nj] || !strict_mask) { /* consider only neighbors that are in mask if strict_mask is 1*/
                        fpj = fin[nj]; /* value at jth neighbor of n */
                        if (fpj < minfn) minfn = fpj;
                        if (fpj > maxfn) maxfn = fpj;
                        dfp += wgt[n][j] * (fpj - fp);
                     } 
                  }/* for j*/
                  fout[n] = fin[n] + delta_time * dfp;
                  if (fout[n] < minfn) fout[n] = minfn;
                  if (fout[n] > maxfn) fout[n] = maxfn;
               } else {
                  fout[n] = fin[n];
               }
            }/* for n */ 
         } /* masking potential */
         
         if (cs->Send && k == 0) {
            if (!SUMA_SendToSuma (SO, cs, (void *)fout, SUMA_NODE_RGBAb, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }

      } /* for niter */
      
      if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL;
      
      /* Now we need to shove the filtered data back into the dset */
      if (!SUMA_Float2DsetCol (dset, icols[k], fout_final, 1, nmask)) {
         SUMA_S_Err("Failed to update dset's values");
         SUMA_RETURN(NOPE);      
      }
      
   } /* for each col */
   
   CLEANUP:
   if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL; /* just in case, this one's still alive from a GOTO */
   /* Pre Dec 06 stupidity: if (bfull == nmask) { if (nmask) SUMA_free(nmask); nmask = NULL; bfull = NULL; } */
   if (bfull) SUMA_free(bfull); bfull = NULL;
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;
   if (fout_final) SUMA_free(fout_final); fout_final = NULL;
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Filter data defined on the surface using M.K. Chung et al.'s 2005 method 
    [1] Chung, M.K., Robbins,S., Dalton, K.M., Davidson, R.J., Evans, A.C. (2004) 
        Cortical thickness analysis in autism via heat kernel smoothing. NeuroImage, submitted. 
        http://www.stat.wisc.edu/~mchung/papers/ni_heatkernel.pdf

    [2] Chung, M.K. (2004) Heat kernel smoothing and its application to cortical manifolds. 
        Technical Report 1090. Department of Statististics, Universisty of Wisconsin-Madison. 
        http://www.stat.wisc.edu/~mchung/papers/heatkernel_tech.pdf
   
   dm_smooth = SUMA_Chung_Smooth_05 (SO, wgt, N_iter, FWHM, fin, vpn, d_order, fout_user, SUMA_COMM_STRUCT *cs);
   
   \param SO (SUMA_SurfaceObject *) Surface object with valid 
                                    SO->NodeList, SO->FaceSetList and SO->FN 
   \param wgt (float **) interpolation weights for each node. 
                           These weights are obtained from SUMA_Chung_Smooth_Weights 
   \param N_iter (int) number of smoothing iterations (must be even, > 1) 
   \param FWHM (float) Full Width at Half Maximum of equivalent Gaussian filter
   \param fin (float *) vector containing node data. The length of this vector
                        is vpn x SO->N_Node , where vpn is the number of values
                        per node. 
   \param vpn (int) the numberof values per node in fin 
   \param d_order (SUMA_INDEXING_ORDER) Indicates how multiple values per node are stored in fin
                        SUMA_ROW_MAJOR: The data in fin is stored in *** Row Major *** order.
                        The ith value (start at 0) for node n is at index fin[vpn*n+i]
                        SUMA_COLUMN_MAJOR: The data in fin is stored in *** Column Major *** order.
                        The ith (start at 0) value for node n is at index fin[n+SO->N_Node*i]; 
                        etc...
   \param fout_user (float *) a pointer to the vector where the smoothed version of fin will reside
                        You can pass NULL and the function will do the allocation for you and return 
                        the pointer to the smoothed data in dm_smooth.
                        If you already have space allocated for the result, then pass the pointer
                        in fout_user and save on allocation time and space. In that case, dm_smooth
                        is equal to fout_user.
                        Either way, you are responsible for freeing memory pointed to by dm_smooth
                        DO NOT PASS fout_user = fin 
   \param cs (SUMA_COMM_STRUCT *) A pointer to the structure containing info for taking to SUMA
                           
   \return dm_smooth (float *) A pointer to the smoothed data (vpn * SO->N_Node values). 
                        You will have to free the memory allocated for dm_smooth yourself.
                        
   \sa SUMA_Chung_Smooth_Weights_05
                        
*/


float * SUMA_Chung_Smooth_05 (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, float *fin_orig, 
                           int vpn, SUMA_INDEXING_ORDER d_order, float *fout_final_user,
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask)
{
   static char FuncName[]={"SUMA_Chung_Smooth_05"};
   float *fout_final = NULL, *fbuf=NULL, *fin=NULL, *fout=NULL, *fin_next = NULL;
   float fp, dfp, fpj, dfps = 0.0;
   int n , k, j, niter, vnk, os, jj, nj;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (N_iter % 2) {
      SUMA_SL_Err("N_iter must be an even number\n");
      SUMA_RETURN(NULL);
   }
   
   if (!SO || !wgt || !fin_orig) {
      SUMA_SL_Err("NULL SO or wgt or fin_orig\n");
      SUMA_RETURN(NULL);
   }
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN\n");
      SUMA_RETURN(NULL);
   }
   
   if (vpn < 1) {
      SUMA_SL_Err("vpn < 1\n");
      SUMA_RETURN(NULL);
   }  
   
   if (fout_final_user == fin_orig) {
      SUMA_SL_Err("fout_final_user == fin_orig");
      SUMA_RETURN(NULL);
   }
   
   if (!fout_final_user) { /* allocate for output */
      fout_final = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
      if (!fout_final) {
         SUMA_SL_Crit("Failed to allocate for fout_final\n");
         SUMA_RETURN(NULL);
      }
   }else {
      fout_final = fout_final_user; /* pre-allocated */
   }
   
   /* allocate for buffer */
   fbuf = (float *)SUMA_calloc(SO->N_Node * vpn, sizeof(float));
   if (!fbuf) {
      SUMA_SL_Crit("Failed to allocate for fbuf\n");
      SUMA_RETURN(NULL);
   }
   
   
   if (cs->Send) { /* send the first monster */
      if (!SUMA_SendToSuma (SO, cs, (void *)fin_orig, SUMA_NODE_RGBAb, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
   }
   
   fin_next = fin_orig;
   switch (d_order) {
      case SUMA_COLUMN_MAJOR:
         for (niter=0; niter < N_iter; ++niter) {
            if ( niter % 2 ) { /* odd */
               fin = fin_next; /* input from previous output buffer */
               fout = fout_final; /* results go into final vector */
               fin_next = fout_final; /* in the next iteration, the input is from fout_final */
            } else { /* even */
               /* input data is in fin_new */
               fin = fin_next;
               fout = fbuf; /* results go into buffer */
               fin_next = fbuf; /* in the next iteration, the input is from the buffer */
            }
            for (k=0; k < vpn; ++k) {
               os = SO->N_Node*k;   /* node value indexing offset */
               for (n=0; n < SO->N_Node; ++n) {
                  vnk = n+os; /* index of kth value at node n */
                  fp = fin[vnk]; /* kth value at node n */
                  dfp = 0.0;
                  if (!nmask) {
                     for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                        fpj = fin[SO->FN->FirstNeighb[n][j]+os]; /* value at jth neighbor of n */
                        dfp += wgt[n][j+1] * (fpj); 
                     }/* for j*/
                     fout[vnk] = fin[vnk] * wgt[n][0] +  dfp;
                  } else { /* masking potential */
                     if (nmask[n]) {
                        dfps = wgt[n][0];
                        for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                           {
                              nj = SO->FN->FirstNeighb[n][j];
                              if (nmask[nj] || !strict_mask) { /* consider only neighbors that are in mask if strict_mask is 1*/
                                 fpj = fin[nj+os]; /* value at jth neighbor of n */
                                 dfp += wgt[n][j+1] * fpj;
                                 dfps += wgt[n][j+1];
                              }
                           } 
                        }/* for j*/
                        fout[vnk] = (fin[vnk] * wgt[n][0] +  dfp)/dfps;
                     } else {
                        fout[vnk] = fin[vnk];
                     }
                  }
               }/* for n */ 
                 
               if (cs->Send) {
                  if (!SUMA_SendToSuma (SO, cs, (void *)fout, SUMA_NODE_RGBAb, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
            } /* for k */
         }/* for niter */
         break;
      case SUMA_ROW_MAJOR:
         SUMA_SL_Err("Row Major not implemented");
         SUMA_RETURN(NULL);
         break;
      default:
         SUMA_SL_Err("Bad Major, very bad.\n");
         SUMA_RETURN(NULL);
         break;
   }
   
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;
               
   SUMA_RETURN(fout);
}

/*!
   A version of SUMA_Chung_Smooth_05 that works with SUMA_DSETs 
   NOTE: dset is changed on output (blurred values) but idcode
   remains unchanged
*/
SUMA_Boolean SUMA_Chung_Smooth_05_single_dset (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, SUMA_DSET *dset, 
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask)
{
   static char FuncName[]={"SUMA_Chung_Smooth_05_signle_dset"};
   float *fout_final = NULL, *fbuf=NULL, *fin=NULL, *fout=NULL, *fin_next = NULL, *fin_orig = NULL;
   float fp, dfp, fpj, minfn=0.0, maxfn=0.0, dfps = 0.0;
   int n , k, j, niter, jj, nj, *icols=NULL, N_icols, N_nmask, kth_buf;
   byte *bfull=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /*
   if (N_iter % 2) {
      SUMA_SL_Err("N_iter must be an even number\n");
      SUMA_RETURN(NOPE);
   }
   */
   
   if (!SO || !wgt || !dset) {
      SUMA_SL_Err("NULL SO or wgt or dset\n");
      SUMA_RETURN(NOPE);
   }
   
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN\n");
      SUMA_RETURN(NOPE);
   }
    
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(dset, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NOPE);   
   }
   SUMA_LHv("Have %d columns to process.\n", N_icols);
   
   /* make a copy of nmask if need be */
   if (nmask) {
      bfull = (byte *)SUMA_malloc(sizeof(byte)*SO->N_Node);
      memcpy((void *)bfull, (void *)nmask, sizeof(byte)*SO->N_Node);
   }
   
   /* allocate for buffer and output */
   fbuf = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   fout_final = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   if (!fbuf || !fout_final) {
      SUMA_SL_Crit("Failed to allocate for fbuf and fout_final\n");
      SUMA_RETURN(NOPE);
   }
   SUMA_LH("Done with buffer allocation");
   
   if (cs && cs->Send && N_icols > 1) {
      SUMA_S_Warn("Only 1st data column will be sent to SUMA in talk_mode.");
   }
   
   
   /* Begin filtering operation for each column */
   for (k=0; k < N_icols; ++k) {
      SUMA_LHv("Filtering column %d\n",icols[k]); 
      if (k==0) {
         fin_orig = SUMA_DsetCol2FloatFullSortedColumn (dset, icols[k], &bfull, 0.0, SO->N_Node, &N_nmask, YUP);
      } else {
         fin_orig = SUMA_DsetCol2FloatFullSortedColumn (dset, icols[k], &bfull, 0.0, SO->N_Node, &N_nmask, NOPE);
      }
           
      if (cs && cs->Send && k == 0) { /* send the first monster */
         if (!SUMA_SendToSuma (SO, cs, (void *)fin_orig, SUMA_NODE_RGBAb, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }

      /* filter this column for each of the iterations */
      fin_next = fin_orig;
      for (niter=0; niter < N_iter; ++niter) {
         SUMA_LHv("niter %d\n", niter);
         if ( niter % 2 ) { /* odd */
            fin = fin_next; /* input from previous output buffer */
            fout = fout_final; /* results go into final vector */
            fin_next = fout_final; /* in the next iteration, the input is from fout_final */
         } else { /* even */
            /* input data is in fin_new */
            fin = fin_next;
            fout = fbuf; /* results go into buffer */
            fin_next = fbuf; /* in the next iteration, the input is from the buffer */
         }
         /* filter iteration for each node in data column k*/
         if (!bfull) {
            for (n=0; n < SO->N_Node; ++n) {
               /*SUMA_LHv("node %d\n", n);*/
               fp = fin[n]; /* kth value at node n */
               dfp = 0.0;
               for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                  fpj = fin[SO->FN->FirstNeighb[n][j]]; /* value at jth neighbor of n */
                  dfp += wgt[n][j+1] * (fpj); 
               }/* for j*/
               fout[n] = fin[n] * wgt[n][0] +  dfp;
            }/* for n */ 
         } else {  /* masking potential */
            for (n=0; n < SO->N_Node; ++n) {
               /*SUMA_LHv("node %d\n", n);*/
               fp = fin[n]; /* kth value at node n */
               dfp = 0.0;
               if (bfull[n]) {
                  dfps = wgt[n][0]; 
                  for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                     {
                        nj = SO->FN->FirstNeighb[n][j];
                        if (bfull[nj] || !strict_mask) { /* consider only neighbors that are in mask if strict_mask is 1*/
                           fpj = fin[nj]; /* value at jth neighbor of n */
                           dfp += wgt[n][j+1] * fpj;
                           dfps += wgt[n][j+1];
                        }
                     } 
                  }/* for j*/
                  fout[n] = (fin[n] * wgt[n][0] +  dfp)/dfps;
               } else {
                  fout[n] = fin[n];
               }
            }/* for n */ 
         } /* masking potential */
         
         if (cs && cs->Send && k == 0) {
            kth_buf = cs->kth;
            if (niter == N_iter -1) {
               cs->kth = 1;   /* send the last iteration no matter what */
            }
            if (!SUMA_SendToSuma (SO, cs, (void *)fout, SUMA_NODE_RGBAb, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
            cs->kth = kth_buf;
         }

      } /* for niter */
      
      if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL;
      
      
      if (N_iter % 2 ) {/* if Niter is odd, then copy contents into fout final from fbuf */
         SUMA_LHv("Copying buffer content, N_iter = %d\n", N_iter);
         memcpy((void*)fout_final, (void *)fbuf, SO->N_Node*sizeof(float));
      }
      
      /* Now we need to shove the filtered data back into the dset */
      if (!SUMA_Float2DsetCol (dset, icols[k], fout_final, 1, bfull)) {
         SUMA_S_Err("Failed to update dset's values");
         SUMA_RETURN(NOPE);      
      }
      
   } /* for each col */
   
   CLEANUP:
   if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL; /* just in case, this one's still alive from a GOTO */
   if (bfull) SUMA_free(bfull); bfull = NULL;
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;
   if (fout_final) SUMA_free(fout_final); fout_final = NULL;
   
   SUMA_RETURN(YUP);
}

/*!
   A version of SUMA_Chung_Smooth_05 that works with SUMA_DSETs 
   NOTE: dset is changed on output (blurred values) but idcode
   remains unchanged
*/
SUMA_Boolean SUMA_Chung_Smooth_05_Pre_07_dset (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, SUMA_DSET *dset, 
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask)
{
   static char FuncName[]={"SUMA_Chung_Smooth_05_Pre_07_dset"};
   float *fout_final = NULL, *fbuf=NULL, *fin=NULL, *fout=NULL, *fin_next = NULL, *fin_orig = NULL;
   float delta_time, fp, dfp, fpj, minfn=0.0, maxfn=0.0, dfps = 0.0;
   int n , k, j, niter, jj, nj, *icols=NULL, N_icols, N_nmask, kth_buf;
   byte *bfull=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /*
   if (N_iter % 2) {
      SUMA_SL_Err("N_iter must be an even number\n");
      SUMA_RETURN(NOPE);
   }
   */
   
   if (!SO || !wgt || !dset) {
      SUMA_SL_Err("NULL SO or wgt or dset\n");
      SUMA_RETURN(NOPE);
   }
   
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN\n");
      SUMA_RETURN(NOPE);
   }
    
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(dset, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NOPE);   
   }
   
   /* allocate for buffer and output */
   fbuf = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   fout_final = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   if (!fbuf || !fout_final) {
      SUMA_SL_Crit("Failed to allocate for fbuf and fout_final\n");
      SUMA_RETURN(NOPE);
   }
   
   if (cs->Send && N_icols > 1) {
      SUMA_S_Warn("Only 1st data column will be sent to SUMA in talk_mode.");
   }
   
   delta_time= (FWHM * FWHM)/(16*N_iter*log(2));
   
   /* Begin filtering operation for each column */
   for (k=0; k < N_icols; ++k) {
      /* get a float copy of the data column */
      fin_orig = SUMA_DsetCol2Float (dset, icols[k], 1);
      if (!fin_orig) {
         SUMA_SL_Crit("Failed to get copy of column. Woe to thee!");
         SUMA_RETURN(NOPE);
      }
      /* make sure column is not sparse, one value per node */
      if (k==0) {
         SUMA_LH( "Special case k = 0, going to SUMA_MakeSparseColumnFullSorted");
         bfull = NULL;
         if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(dset), 0.0, &bfull, dset, SO->N_Node)) {
            SUMA_S_Err("Failed to get full column vector");
            SUMA_RETURN(NOPE);
         }
         if (bfull) {
            SUMA_LH( "Something was filled in SUMA_MakeSparseColumnFullSorted\n" );
            /* something was filled in good old SUMA_MakeSparseColumnFullSorted */
            if (nmask) {   /* combine bfull with nmask */
               SUMA_LH( "Merging masks\n" );
               for (jj=0; jj < SO->N_Node; ++jj) { if (nmask[jj] && !bfull[jj]) nmask[jj] = 0; }   
            } else { nmask = bfull; }
         } 
         if (nmask) {
            N_nmask = 0;
            for (n=0; n<SO->N_Node; ++n) { if (nmask[n]) ++ N_nmask; }
            SUMA_LHv("Blurring with node mask (%d nodes in mask)\n", N_nmask);
            if (!N_nmask) {
               SUMA_S_Warn("Empty mask, nothing to do");
               goto CLEANUP;
            }
         }
      } else {
         SUMA_LH( "going to SUMA_MakeSparseColumnFullSorted");
         if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(dset), 0.0, NULL, dset, SO->N_Node)) {
            SUMA_S_Err("Failed to get full column vector");
            SUMA_RETURN(NOPE);
         }
         /* no need for reworking nmask and bfull for each column...*/
      }
           
      if (cs->Send && k == 0) { /* send the first monster */
         if (!SUMA_SendToSuma (SO, cs, (void *)fin_orig, SUMA_NODE_RGBAb, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }

      /* filter this column for each of the iterations */
      fin_next = fin_orig;
      for (niter=0; niter < N_iter; ++niter) {
         SUMA_LHv("niter %d\n", niter);
         if ( niter % 2 ) { /* odd */
            fin = fin_next; /* input from previous output buffer */
            fout = fout_final; /* results go into final vector */
            fin_next = fout_final; /* in the next iteration, the input is from fout_final */
         } else { /* even */
            /* input data is in fin_new */
            fin = fin_next;
            fout = fbuf; /* results go into buffer */
            fin_next = fbuf; /* in the next iteration, the input is from the buffer */
         }
         /* filter iteration for each node in data column k*/
         if (!nmask) {
            for (n=0; n < SO->N_Node; ++n) {
               /*SUMA_LHv("node %d\n", n);*/
               fp = fin[n]; /* kth value at node n */
               dfp = 0.0;
               for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                  fpj = fin[SO->FN->FirstNeighb[n][j]]; /* value at jth neighbor of n */
                  dfp += wgt[n][j+1] * (fpj); 
               }/* for j*/
               fout[n] = fin[n] * wgt[n][0] +  dfp;
            }/* for n */ 
         } else {  /* masking potential */
            for (n=0; n < SO->N_Node; ++n) {
               /*SUMA_LHv("node %d\n", n);*/
               fp = fin[n]; /* kth value at node n */
               dfp = 0.0;
               if (nmask[n]) {
                  dfps = wgt[n][0];
                  for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                     {
                        nj = SO->FN->FirstNeighb[n][j];
                        if (nmask[nj] || !strict_mask) { /* consider only neighbors that are in mask if strict_mask is 1*/
                           fpj = fin[nj]; /* value at jth neighbor of n */
                           dfp += wgt[n][j+1] * fpj;
                           dfps += wgt[n][j+1];
                        }
                     } 
                  }/* for j*/
                  fout[n] = (fin[n] * wgt[n][0] +  dfp)/dfps;
               } else {
                  fout[n] = fin[n];
               }
            }/* for n */ 
         } /* masking potential */
         
         if (cs->Send && k == 0) {
            kth_buf = cs->kth;
            if (niter == N_iter -1) {
               cs->kth = 1;   /* send the last iteration no matter what */
            }
            if (!SUMA_SendToSuma (SO, cs, (void *)fout, SUMA_NODE_RGBAb, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
            cs->kth = kth_buf;
         }

      } /* for niter */
      
      if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL;
      
      
      if (N_iter % 2 ) {/* if Niter is odd, then copy contents into fout final from fbuf */
         SUMA_LHv("Copying buffer content, N_iter = %d\n", N_iter);
         memcpy((void*)fout_final, (void *)fbuf, SO->N_Node*sizeof(float));
      }
      
      /* Now we need to shove the filtered data back into the dset */
      if (!SUMA_Float2DsetCol (dset, icols[k], fout_final, SO->N_Node, nmask)) {
         SUMA_S_Err("Failed to update dset's values");
         SUMA_RETURN(NOPE);      
      }
      
   } /* for each col */
   
   CLEANUP:
   if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL; /* just in case, this one's still alive from a GOTO */
   /* Pre Dec 06 stupidity: if (bfull == nmask) { if (nmask) SUMA_free(nmask); nmask = NULL; bfull = NULL; } */
   if (bfull) SUMA_free(bfull); bfull = NULL;
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;
   if (fout_final) SUMA_free(fout_final); fout_final = NULL;
   
   SUMA_RETURN(YUP);
}

/*!
   NOTE: In this function, IF N_iter is set to -1, EVERY column is 
   filtered separately until it reaches the FWHM. This means columns
   can be filtered with differing numbers of iterations, something
   which is not always desirable!
*/
SUMA_Boolean SUMA_Chung_Smooth_07_dset (SUMA_SurfaceObject *SO, double **wgt, 
                           int *N_iter, double *FWHMp, SUMA_DSET *dset, 
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask)
{
   static char FuncName[]={"SUMA_Chung_Smooth_07_dset"};
   double   *fout_final = NULL, *fbuf=NULL, *fin=NULL, 
            *fout=NULL, *fin_next = NULL, *fin_orig = NULL, FWHM;
   double fp, dfp, fpj, minfn=0.0, maxfn=0.0, dfps=0.0;
   float *fin_float=NULL, *fsend=NULL, *fwhmg=NULL;
   int n , k, j, niter=-1, jj, nj, 
      *icols=NULL, N_icols, N_nmask, kth_buf, niter_alloc;
   byte *bfull=NULL, stop = 0;
   char stmp[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
  
   FWHM = *FWHMp;
   
   if (*N_iter > 0 && FWHM > 0) {
      SUMA_S_Err("Can't specify both of FWHM and N_iter");
      SUMA_RETURN(NOPE);
   }
 
   if (!SO || !wgt || !dset) {
      SUMA_SL_Err("NULL SO or wgt or dset\n");
      SUMA_RETURN(NOPE);
   }
   
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN\n");
      SUMA_RETURN(NOPE);
   }
    
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(dset, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NOPE);   
   }
   SUMA_LHv("Have %d columns to process.\n", N_icols);
   if (*N_iter < 0 && N_icols > 1) {
      SUMA_S_Note(
         "Each column will be blurred until it reaches the desired FWHM.\n"
         "Therefore it is possible (make that likely) that different columns\n"
         "are blurred by a differing number of iterations.\n");
   }  
   
   /* make a copy of nmask if need be */
   if (nmask) {
      bfull = (byte *)SUMA_malloc(sizeof(byte)*SO->N_Node);
      memcpy((void *)bfull, (void *)nmask, sizeof(byte)*SO->N_Node);
   }
   
   /* allocate for buffer and output */
   niter_alloc = 500;
   fwhmg = (float*)SUMA_calloc(niter_alloc, sizeof(float));
   fbuf = (double *)SUMA_calloc(SO->N_Node, sizeof(double));
   fin_orig = (double *)SUMA_calloc(SO->N_Node, sizeof(double));
   fout_final = (double *)SUMA_calloc(SO->N_Node, sizeof(double));
   if (!fbuf || !fout_final || !fin_orig || !fwhmg) {
      SUMA_SL_Crit("Failed to allocate for fbuf and fout_final\n");
      SUMA_RETURN(NOPE);
   }
   SUMA_LH("Done with buffer allocation");
   
   if (cs && cs->Send && N_icols > 1) {
      SUMA_S_Warn("Only 1st data column will be sent to SUMA in talk_mode.");
   }
   
   
   /* Begin filtering operation for each column */
   for (k=0; k < N_icols; ++k) {
      SUMA_LHv(
         "Filtering column %d, N_iter=%d, FWHM=%f\n",icols[k], *N_iter, FWHM); 
      if (k==0) {
         fin_float = SUMA_DsetCol2FloatFullSortedColumn (
            dset, icols[k], &bfull, 0.0, SO->N_Node, &N_nmask, YUP);
      } else {
         fin_float = SUMA_DsetCol2FloatFullSortedColumn (
            dset, icols[k], &bfull, 0.0, SO->N_Node, &N_nmask, NOPE);
      }
      /* copy the float to double array */
      for (n=0; n<SO->N_Node; ++n) fin_orig[n] = (double)fin_float[n];
           
      if (cs && cs->Send && k == 0) { /* send the first monster */
         /* Must do this stupid copy */
         if (!SUMA_SendToSuma (SO, cs, (void *)fin_float, SUMA_NODE_RGBAb, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }

      /* filter this column for each of the iterations */
      fin_next = fin_orig;
      niter = 0; stop = 0;
      if (*N_iter < 0) {
         fwhmg[niter] = SUMA_estimate_FWHM_1dif( SO, fin_float, bfull, 1);
         if (fwhmg[niter] > FWHM) stop = 1; 
      } else {
         if (niter >= *N_iter) stop = 1;
      }
      
      while (!stop) {
         SUMA_LHv("niter %d\n", niter);
         if ( niter % 2 ) { /* odd */
            fin = fin_next; /* input from previous output buffer */
            fout = fout_final; /* results go into final vector */
            fin_next = fout_final; /*  in the next iteration, 
                                       the input is from fout_final */
         } else { /* even */
            /* input data is in fin_new */
            fin = fin_next;
            fout = fbuf; /* results go into buffer */
            fin_next = fbuf; /*  in the next iteration, 
                                 the input is from the buffer */
         }
         /* filter iteration for each node in data column k*/
         if (!bfull) {
            for (n=0; n < SO->N_Node; ++n) {
               fp = fin[n]; /* kth value at node n */
               dfp = 0.0;
               for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                  /* value at jth neighbor of n */
                  fpj = fin[SO->FN->FirstNeighb[n][j]]; 
                  dfp += wgt[n][j+1] * (fpj); 
               }/* for j*/
               fout[n] = fin[n] * wgt[n][0] +  dfp;
               #if 0
                  if (LocalHead && n == SUMA_SSidbg) 
                     SUMA_LHv("node %d, fin %g, fout %g\n", n, fin[n], fout[n]);
               #endif
            }/* for n */ 
         } else {  /* masking potential */
            for (n=0; n < SO->N_Node; ++n) {
               fp = fin[n]; /* kth value at node n */
               dfp = 0.0;
               if (bfull[n]) {
                  dfps = wgt[n][0];
                  for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                     {
                        nj = SO->FN->FirstNeighb[n][j];
                        if (bfull[nj] || !strict_mask) { 
                           /* consider only neighbors that are 
                              in mask if strict_mask is 1*/
                           fpj = fin[nj]; /* value at jth neighbor of n */
                           dfp += wgt[n][j+1] * fpj;
                           dfps += wgt[n][j+1];
                        }
                     } 
                  }/* for j*/
                  fout[n] = (fin[n] * wgt[n][0] +  dfp)/dfps;
               } else {
                  fout[n] = fin[n];
               }
               #if 0
                  if (LocalHead && n == SUMA_SSidbg) 
                     SUMA_LHv("node %d, fin %g, fout %g\n", n, fin[n], fout[n]);
               #endif
            }/* for n */ 
         } /* masking potential */
         if (cs && cs->Send && k == 0) { /*  IF YOU CHANGE THIS CONDITION, 
                                             CHANGE IT IN next block! */
            /* Must do this stupid copy */
            if (!fsend) { fsend = (float*)SUMA_malloc(sizeof(float)*SO->N_Node); }
            for (n=0; n < SO->N_Node; ++n) { fsend[n] = (float)fout[n]; }
            kth_buf = cs->kth;
            if (niter == *N_iter -1) {
               cs->kth = 1;   /* send the last iteration no matter what */
            }
            if (!SUMA_SendToSuma (SO, cs, (void *)fsend, SUMA_NODE_RGBAb, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
            cs->kth = kth_buf;
         }
         if (*N_iter < 0) {
            /* base on fwhm */
            if (!fsend) { fsend = (float*)SUMA_malloc(sizeof(float)*SO->N_Node); }
            if (!(cs && cs->Send && k == 0)) {/* Else fsend is setup above */
               for (n=0; n < SO->N_Node; ++n) { fsend[n] = (float)fout[n]; }
            }
            if (niter >= niter_alloc-1) {
               fwhmg = SUMA_realloc(fwhmg,sizeof(float)*(niter_alloc+500)); 
               niter_alloc = niter_alloc+500;
            }
            ++niter; 
            fwhmg[niter] = SUMA_estimate_FWHM_1dif( SO, fsend, bfull, 1);
            if (fwhmg[niter] > FWHM) stop = 1; 
            fprintf(SUMA_STDERR,"   iteration %d, fwhmg = %f\n", 
                     niter, fwhmg[niter]);
         } else {
            ++niter; 
            if (niter >= *N_iter) stop = 1;
         }
      } /* for niter */
      
      if ( (niter % 2) ) {/*  if niter is odd, then copy 
                              contents into fout final from fbuf */
         SUMA_LHv("Copying buffer content, N_iter = %d\n", *N_iter);
         memcpy((void*)fout_final, (void *)fbuf, SO->N_Node*sizeof(double));
      }

      /* Now we need to shove the filtered data back into the dset */
      for (n=0; n<SO->N_Node; ++n) fin_float[n] = (float)fout_final[n];
      if (LocalHead &&  SUMA_SSidbg >= 0 && SUMA_SSidbg <= SO->N_Node) {
         SUMA_LHv("fin_float[%d] = %f,  fout_final[%d] = %f\n",
               SUMA_SSidbg, fin_float[SUMA_SSidbg], 
               SUMA_SSidbg, fout_final[SUMA_SSidbg]);
      }  
      if (!SUMA_Float2DsetCol (dset, icols[k], fin_float, 1, bfull)) {
         SUMA_S_Err("Failed to update dset's values");
         SUMA_RETURN(NOPE);      
      }
      
      if (fin_float) SUMA_free(fin_float); fin_float = NULL;
      
      if (niter == 0) {
         if (*N_iter < 0) {
            SUMA_S_Notev(  "Data column %d had a FWHM of %f," 
                           "which is greater than the requested FHWM of %f\n"
                           "No filtering done there.\n", 
                           icols[k], fwhmg[niter], (float)FWHM);
         } 
      }
      #if 0
         if (*N_iter < 0) {
            FILE *foutiter=NULL;
            char fname[500];
            SUMA_S_Note("Kill me");
            sprintf(fname, "FWHM_vs_iteration_Col_%d_Le+%.3f", 
                     icols[k], SO->EL->AvgLe);
            foutiter = fopen(fname, "w");
            fprintf(foutiter,"#iteration     fwhm  Col %d AvgLe = %f\n",
                     icols[k], SO->EL->AvgLe);
            for (n=0; n<=*N_iter; ++n) {
               fprintf(foutiter, "%d   %f \n", n, fwhmg[n]);
            }
            fclose (foutiter); foutiter = NULL;
         }
         /* recover the value FROM the dset of that node in question */
         if (LocalHead && SUMA_SSidbg >= 0 && SUMA_SSidbg <= SO->N_Node) {
            int nnn = -1;
            nnn = SUMA_GetNodeRow_FromNodeIndex_s(
                        dset, SUMA_SSidbg, 
                        SO->N_Node);
            SUMA_LHv("Value at node %d (row %d), column %d: %f\n",
               SUMA_SSidbg, nnn, icols[k], 
               SUMA_GetDsetValInCol2( dset,icols[k], nnn ) );
         }         
      #endif
   } /* for each col */
   
   /* record values for posterity */
   *N_iter = niter;
   *FWHMp = FWHM;
   
   CLEANUP:
   if (fwhmg) SUMA_free(fwhmg); fwhmg=NULL;
   if (fsend) SUMA_free(fsend); fsend=NULL;
   if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL; 
   if (fin_float) SUMA_free(fin_float); fin_float = NULL; /* just in case, 
                                                            this one's still
                                                            alive from a GOTO */
   if (bfull) SUMA_free(bfull); bfull = NULL;
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;
   if (fout_final) SUMA_free(fout_final); fout_final = NULL;
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_WriteSmoothingRecord (  SUMA_SurfaceObject *SO, 
                                          float *fwhmg, int Niter, 
                                          double *sigma, int cnst_sig,
                                          char *prefix)
{
   static char FuncName[]={"SUMA_WriteSmoothingRecord"};
   FILE *foutiter=NULL;
   int n;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->EL || !fwhmg) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   SUMA_LHv("Initial FWHM of %f\n"
            "FWHM %f reached after %d iterations.\n", fwhmg[0], fwhmg[Niter], Niter);
   {
      char fname[500];
      snprintf(fname, 450*sizeof(char), "%s.1D.smrec",  prefix ? prefix:"anonyme" );
      SUMA_S_Notev("Writing FWHM progression history to %s ...\n", fname);
      foutiter = fopen(fname, "w");
      fprintf(foutiter, "#History of FWHM versus iteration number.\n"
                        "#Surface %s had average segment length of %f \n"
                        "#Initial FWHM of %f\n"
                        "#Col.0 : iteration number\n"
                        "#Col.1 : estimated fwhm\n"
                        "#Col.2 : kernel bandwidth (sigma)\n"
                        , SO->Label, SO->EL->AvgLe, fwhmg[0]);
      for (n=0; n<=Niter; ++n) {
         fprintf(foutiter, "%d   %f   %f\n", n, fwhmg[n], cnst_sig ? *sigma:sigma[n]);
      }
      fclose (foutiter); foutiter = NULL;
   }
     
   SUMA_RETURN(YUP);
}

/*!
   This function repeatedly blurs a dataset until the FWHM of the dataset
   just surpasses FWHM.
   SO: Surface object over which data are defined
   wgt: The weights for the immediate neighbors
   N_iter: Pointer to an integer showing the number of iterations that ended
           up being used
   FWHM: The desired FWHM
   dset: The dataset to be filtred
   nmask: The nodes to which the analysis is restricted
   strict_mask: Use only values from nodes inside mask (should be 1)
   FWHM_mixmode: Choose from "arit" or "geom" for the arithmetic and geometric
                 mean, respectively. When one has multi-sub-bricks for
                 input the FWHM of all sub-bricks is either the arithmetic 
                 mean or the geometric mean.
   Note that this implementation is rather slow. But life is hard, because
   all columns are to be processed at each iteration. Rather than iterating 
   on each column, successively.
    
*/
SUMA_Boolean SUMA_Chung_Smooth_07_toFWHM_dset (
                           SUMA_SurfaceObject *SO, double **wgt, 
                           int *N_iter, double *FWHMp, SUMA_DSET *dset, 
                           byte *nmask, byte strict_mask, 
                           char *FWHM_mixmode, float **fwhmgp)
{
   static char FuncName[]={"SUMA_Chung_Smooth_07_toFWHM_dset"};
   double fp, dfp, fpj, minfn=0.0, maxfn=0.0, FWHM, dfps=0.0;
   float *fin_float=NULL, *fwhmg=NULL, *fwhmv=NULL, 
         *fout_final = NULL, *fin=NULL, *fout=NULL;
   int   n , k, j, niter, jj, nj, *icols=NULL, 
         N_icols, N_nmask, kth_buf, niter_alloc, N;
   int   NITERMAX = 3000;
   char  LogFWHM_hist[]={""}; /* for now don't save history.  
                                 to turn it back on, use something like: 
                                 {"FWHM_vs_iteration"}; */
   byte *bfull=NULL, stop = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   FWHM = *FWHMp;
   
   if (*N_iter > 0 && FWHM > 0) {
      SUMA_S_Err("Too much information. One or the other please");
      SUMA_RETURN(NOPE);
   }
   if (!SO || !wgt || !dset) {
      SUMA_SL_Err("NULL SO or wgt or dset\n");
      SUMA_RETURN(NOPE);
   }
   
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN\n");
      SUMA_RETURN(NOPE);
   }
    
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(dset, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NOPE);   
   }
   SUMA_LHv("Have %d columns to process.\n", N_icols);
   
   /* make a copy of nmask if need be */
   if (nmask) {
      bfull = (byte *)SUMA_malloc(sizeof(byte)*SO->N_Node);
      memcpy((void *)bfull, (void *)nmask, sizeof(byte)*SO->N_Node);
   }
   
   /* allocate for buffer and output */
   niter_alloc = 500;
   fwhmg = (float*)SUMA_calloc(niter_alloc, sizeof(float));
   fout_final = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   if (!fout_final  || !fwhmg) {
      SUMA_SL_Crit("Failed to allocate for fbuf and fout_final\n");
      SUMA_RETURN(NOPE);
   }
   SUMA_LH("Done with buffer allocation");
   
   
   /* estimate the FWHM of the dataset */
   if (!strict_mask) {
      SUMA_S_Warn("Strict mask is always applied for FWHM estimation.");
   }

   /* iterations have not begun */
   stop = 0;
   niter = 0;
   while (!stop) {
      if (*N_iter < 0) { /* function knows best */
         if (niter >= niter_alloc) {   
            fwhmg = SUMA_realloc(fwhmg,sizeof(float)*(niter_alloc+500)); 
            niter_alloc = niter_alloc+500; 
         }
         fwhmg[niter] = 0.0; N = 0;
         if (!(fwhmv = SUMA_estimate_dset_FWHM_1dif(  SO, dset, 
                                                icols, N_icols, nmask, 
                                                1, NULL))) {
            SUMA_S_Err("Rien ne va plus");
            SUMA_RETURN(NOPE);
         }
         SUMA_FWHM_MEAN(fwhmv, N_icols, fwhmg[niter], FWHM_mixmode, N);
         if (N <= 0) {
            SUMA_S_Err("Failed to get mean fwhm");
            SUMA_RETURN(NOPE);
         }
         if (fwhmg[niter] > FWHM) stop = 1;
         else if (niter > NITERMAX) {
            SUMA_S_Warnv( 
                  "More than %d iterations failed to reach target of %f.\n"
                  "Stopping at fwhm of %f.\n"
                  "Consider increasing kernel bandwidth\n", 
                  niter-1,FWHM, fwhmg[niter]);
            stop =1;
         }
      } else { /* caller is the boss */
         if (niter >= *N_iter) stop = 1;
      } 
      if (!stop) {
         if (*N_iter < 0) { 
            SUMA_LHv("Beginning iteration %d, fwhm = %f; target %f\n", 
                      niter, fwhmg[niter], FWHM); }
         else { SUMA_LHv("Iteration %d/%d\n", niter+1,*N_iter);}
         /* Dull report */
         if (!LocalHead && !(niter % 10)) 
            fprintf( SUMA_STDERR,
                     "Iteration %d, fwhm = %f; target %f\n", 
                      niter, fwhmg[niter], FWHM);
         /* Begin filtering operation for each column */
         for (k=0; k < N_icols; ++k) {
            /*SUMA_LHv("Filtering column %d\n",icols[k]); */
            if (k==0) {
               fin_float = 
                  SUMA_DsetCol2FloatFullSortedColumn (
                     dset, icols[k], &bfull, 0.0, SO->N_Node, &N_nmask, YUP);
            } else {
               fin_float = 
                  SUMA_DsetCol2FloatFullSortedColumn (
                     dset, icols[k], &bfull, 0.0, SO->N_Node, &N_nmask, NOPE);
            }


            /* filter this column for each of the iterations */
            fin = fin_float;
            fout = fout_final;
            /* filter iteration for each node in data column k*/
            if (!bfull) {
               for (n=0; n < SO->N_Node; ++n) {
                  fp = fin[n]; /* kth value at node n */
                  dfp = 0.0;
                  for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                     fpj = (double)fin[SO->FN->FirstNeighb[n][j]]; 
                                          /* value at jth neighbor of n */
                     dfp += wgt[n][j+1] * (fpj); 
                  }/* for j*/
                  fout[n] = (float)((double)fin[n] * wgt[n][0] +  dfp);
                  #if 0
                     if (LocalHead && n == SUMA_SSidbg) 
                        SUMA_LHv("node %d, fin %g, fout %g\n", 
                                 n, fin[n], fout[n]);
                  #endif
               }/* for n */ 
            } else {  /* masking potential */
               for (n=0; n < SO->N_Node; ++n) {
                  /*SUMA_LHv("node %d\n", n);*/
                  fp = fin[n]; /* kth value at node n */
                  dfp = 0.0;
                  if (bfull[n]) {
                     dfps = wgt[n][0];
                     for (j=0; j < SO->FN->N_Neighb[n]; ++j) {
                        {
                           nj = SO->FN->FirstNeighb[n][j];
                           if (bfull[nj] || !strict_mask) { 
                                 /* consider only neighbors that are in mask 
                                    if strict_mask is 1*/
                              fpj = (double)fin[nj]; 
                                          /* value at jth neighb. of n */
                              dfp += wgt[n][j+1] * fpj;
                              dfps += wgt[n][j+1];
                           }
                        } 
                     }/* for j*/
                     fout[n] = (float)(((double)fin[n] * wgt[n][0] +  dfp)/dfps);
                  } else {
                     fout[n] = fin[n];
                  }
               }/* for n */ 
            } /* masking potential */


            /* Now we need to shove the filtered data back into the dset */
            if (!SUMA_Float2DsetCol (dset, icols[k], fout, 1, bfull)) {
               SUMA_S_Err("Failed to update dset's values");
               SUMA_RETURN(NOPE);      
            }
            /* Need to free fin_float now */
            SUMA_free(fin_float); fin_float = NULL;
         }/* repeat for each column */

         if (fwhmv) SUMA_free(fwhmv); fwhmv = NULL;
         ++niter; /* go back for another pass */
      }   
   }
   
   if (*N_iter < 0) {
      SUMA_LHv("Initial FWHM of %f\n"
               "FWHM %f reached after %d iterations.\n", fwhmg[0], fwhmg[niter], niter);
      /*
      for (k=1; k<=niter; ++k) {
         fprintf(SUMA_STDERR,"     FWHM[%d]= %f\n", k-1, fwhmg[k]); 
      }
      */
      if (LogFWHM_hist[0] != '\0') {
         {
            FILE *foutiter=NULL;
            char fname[500];
            sprintf(fname, "%s_Le+%.3f",  LogFWHM_hist, SO->EL->AvgLe);
            SUMA_S_Notev("Writing FWHM progression history to %s ...\n", fname);
            foutiter = fopen(fname, "w");
            fprintf(foutiter,"#iteration     fwhm  @ AvgLe = %f\n#Initial FWHM of %f\n", SO->EL->AvgLe, fwhmg[0]);
            for (n=0; n<=niter; ++n) {
               fprintf(foutiter, "%d   %f \n", n, fwhmg[n]);
            }
            fclose (foutiter); foutiter = NULL;
         }
     }
   }
   
   /* set the number of iterations (does nothing if user passed *N_iter > 0)*/
   *N_iter = niter;
   *FWHMp = FWHM;
   
   CLEANUP:
   if (fwhmv) SUMA_free(fwhmv); fwhmv = NULL;
   if (!fwhmgp) {
      if (fwhmg) SUMA_free(fwhmg); fwhmg=NULL;
   } else { /* save it for the trip back home */
      *fwhmgp = fwhmg; fwhmg = NULL;
   }
   if (fin_float) SUMA_free(fin_float); fin_float = NULL; /* just in case, this one's still alive from a GOTO */
   if (bfull) SUMA_free(bfull); bfull = NULL;
   if (fout_final) SUMA_free(fout_final); fout_final = NULL;
   
   SUMA_RETURN(YUP);
}

int SUMA_Chung_Smooth_05_N_iter (double fwhm, double AvgLe, double *sigmap)
{
   static char FuncName[]={"SUMA_Chung_Smooth_05_N_iter"};
   double sequiv;
   int N_iter = -1;

   SUMA_ENTRY;
   SUMA_S_Err("Bad news in tennis shoes. Don't use me no more.");
   SUMA_RETURN(-1);
   
   /* make a suggestion */
   *sigmap = sqrt(-(AvgLe*AvgLe)/(2*log(0.1))); /* making the average SUMA_CHUNG_KERNEL_NUMER be 10 percent */
   /* have sigma and fwhm, what is N? */
   sequiv  = fwhm * 0.42466090;
   N_iter = SUMA_POW2(sequiv/(*sigmap));
   if (N_iter % 2) ++N_iter;
   if (N_iter < 4) N_iter = 4; /* need a few iterations */
   /* now reset sigma based on number of iterations */
   *sigmap = sequiv / sqrt(N_iter);

   SUMA_RETURN(N_iter);
}


/* see logistic.m and ilogistic.m, which are complimentary to FWHM_Beta2.m*/
double SUMA_logistic (double *beta, double x)
{
   return(   beta[0] + 
                  beta[1]*( ( 1+beta[2]*exp(-(x/beta[3])) ) / ( 1+beta[4]*exp(-(x/beta[3]))) ) );
}

double SUMA_ilogistic (double *beta, double a)
{
   static char FuncName[]={"SUMA_ilogistic"};
   double ll = (beta[0]+beta[1]-a) / (a*beta[4]-beta[1]*beta[2]-beta[0]*beta[4]);
   if (ll <= 0.0f) { fprintf(SUMA_STDERR,"Error SUMA_ilogistic:\n log of <= 0 value.\nReturning 0.0\n");return(0.0); }
   return(   -beta[3] * log( ll )  );
} 

/* A function to guess the appropriate sigma (kernel std) from 
the desired (suggested) number of iterations (default 100) and suitable for
a particular mesh
beta is a vector of parameters for a logistic function that fits the function
relating kernel width. beta is determined by the script FWHM_Beta2.m, see also FWHM_GetSig.m
Make sure that changes there are reflected here and vice versa.
*/

static double betadefault[]={ 0.2826  ,  1.2059 , -10.9964  ,  0.1691 ,  23.2139   };
         
double SUMA_SigForFWHM(float AvgLe, double dfwhm, int *niterest, double *beta)
{
   static char FuncName[]={"SUMA_SigForFWHM"};
   double wt, avg_wt;
   double Delta, Sig2, Sigma;
   int niter = -1;
   double SigmaLim[]={0.5, 11}; /*  Lower than 0.5 causes non linear slopes of FWHM versus Niter 
                                    (see bunch with low Sigma/AvgLe in Figures 1 and especially 4)
                                    I do not have values higher than 11. */
   double DeltaLim[]={0.2, 1.5}; /* Low enough to allow Sigmas as low as 0.5, 
                                    high is take care of below */
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;

   if (niterest) niter = *niterest;
   else niter = -1;
   if (dfwhm <= 0) { SUMA_S_Err("dfwhm is <=0 !"); SUMA_RETURN(-1.0); }
   if (niter <= 0) niter = 100;
   if (!beta) beta = betadefault;
   Sigma = -1;   
   
   if (dfwhm/AvgLe < 2) {
      SUMA_S_Errv("FWHM desired (%.3f) is too close to average intersegment length (%.3f).\n"
                  "The function fit is poor for this extreme.\n", dfwhm, AvgLe);
      SUMA_RETURN(Sigma);
   }
   
   /* upper limit for Delta, not that critical */
   DeltaLim[1] = SUMA_logistic(beta, SigmaLim[1]);
   
   /* What is the Delta needed? */
   Delta = AvgLe * dfwhm / sqrt(niter); 

   /* within lims? */
   if (Delta > DeltaLim[1]) {
      Delta = DeltaLim[1];
      niter = SUMA_POW2((AvgLe * dfwhm / Delta));  /* VERY UNRELIABLE */
      SUMA_S_Notev("Large Delta, niter (wild) guess %d\n", niter);
      Sigma = dfwhm / AvgLe / 10.0;    /* sigma would be 1/10 of fwhm  (recall this Sigma is already normalized by avgle) */
   } else if ( Delta < DeltaLim[0]) {
      Delta = DeltaLim[0];
      niter = SUMA_POW2((AvgLe * dfwhm / Delta));
      SUMA_S_Notev("Low Delta, niter guess %d\n", niter);
      Sigma = SUMA_ilogistic(beta, Delta);
   } else {
      Sigma = SUMA_ilogistic(beta, Delta);
   }
   
   /* some more safeguards */
   if (Sigma < SigmaLim[0] && dfwhm > 3.0*AvgLe) { /*  That fwhm condition is mostly to keep the number of iterations largish when fwhm is small */
      Sigma = SigmaLim[0];
      Delta = SUMA_logistic(beta, Sigma);
      niter = SUMA_POW2((AvgLe * dfwhm / Delta));
      SUMA_S_Notev("Low Sigma/AvgLe, increased it to %.3f\n"
                   "Expected niter now: %d\n", Sigma, niter);
   }
   
   if (niterest) *niterest = niter;
      
   if (LocalHead) {
      fprintf (SUMA_STDERR,"For FWHM of = %f\n"
                        " kernel bandwidth per iteration= %f\n"
                        " kernel FWHM per iteration     = %f\n"
                        " N_iter (guess) = %d\n", dfwhm, Sigma*AvgLe, Sigma*AvgLe/0.42466090, niter);
   }
   avg_wt = SUMA_CHUNG_KERNEL_NUMER((AvgLe*AvgLe),Sigma*AvgLe);
   fprintf(SUMA_STDERR, "Kernel Bandwidth / Average Edge Distance = %f/%f = %f\n"
                        "   Corresponding Kernel Numerator = %g\n", 
                           Sigma, AvgLe, Sigma*AvgLe, avg_wt);

   SUMA_RETURN(Sigma);
}


/*!
   Function to apply successive blurring operations so that no two neighboring nodes have the same
   value. The blurring is done on all columns of the data but the testing is done on column indexed
   icol
   If MaskZeros is set to 1 then nodes having a value of 0 are not considered
   
   I am not convinced, this solves much. Go back and see effect on 1D sample in matlab...
*/
SUMA_Boolean SUMA_FixNN_Oversampling ( SUMA_SurfaceObject *SO, SUMA_DSET *dset, byte *nmask, 
                                       int icol, SUMA_Boolean MaskZeros) 
{
   static char FuncName[]={"SUMA_FixNN_Oversampling"};
   char stmp[100];
   float *c;
   double dfwhm, **wgt=NULL;
   double sigma, fwhmttt;
   byte *bfull = NULL, *blur_zone=NULL, *blur_zone_2=NULL;
   int N_nmask, N_zeros, ipass=0, N_iter, n, j, nj, nn;
   SUMA_Boolean SameZone=YUP; /* if YUP, then keep blurring the first zone detected.
                                 if NOPE, then zone changes with each pass (results in many interpolation artifacts at edges 
                                         of shrinking zone...*/
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   if (!SO->EL || !SO->FN) {
      if (!SUMA_SurfaceMetrics(SO, "EdgeList", NULL)) {
         SUMA_S_Err("This elevator has not been inspected in 3 months!\nThis makes me SICK!\n");
         SUMA_RETURN(NOPE);
      }
   }
   
   
   if (nmask) {
      SUMA_LH("Copying mask");
      bfull = (byte*)SUMA_malloc(sizeof(byte)*SO->N_Node);
      memcpy((void*)bfull, (void *)nmask, sizeof(byte)*SO->N_Node);
   } 
   
   SUMA_LH("Blur parameters");
   dfwhm = (double)SO->EL->AvgLe;
   N_iter = 20;
   sigma = SUMA_SigForFWHM( SO->EL->AvgLe, dfwhm, NULL, NULL) * SO->EL->AvgLe;
   
   ipass = 0;
   do {
      if (!blur_zone) { blur_zone = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte)); }
      SUMA_LHv("Going to get 'c', pass %d\n", ipass);
      /* get the column of interest, nice and plump */
      c = SUMA_DsetCol2FloatFullSortedColumn (dset, icol, &bfull, 0.0, SO->N_Node, &N_nmask, YUP);
      /* Now for each node in the mask, see if you have similar values*/
      SUMA_LHv("Looking for zeros bfull: %p     c: %p.\n", bfull, c);
      N_zeros = 0;
      for (n=0; n<SO->N_Node; ++n) {
         if (!bfull || bfull[n]) {
            if (!MaskZeros || c[n] != 0.0f) {
               for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
                  nj = SO->FN->FirstNeighb[n][j];
                  if (c[n] == c[nj]) {
                     blur_zone[n] = 1;
                     blur_zone[nj] = 1;
                     ++N_zeros;
                  }
               }
            }
         }
      }
      
      /* fatten blur_zone */
      if (!blur_zone_2) { blur_zone_2 = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte)); }
      for (n=0; n<SO->N_Node; ++n) {
         if (blur_zone[n]) {
            for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
               nj = SO->FN->FirstNeighb[n][j];
               if (!bfull || bfull[nj]) {
                  if (!blur_zone[nj] && (!MaskZeros || c[nj] != 0.0f)) blur_zone_2[nj] = 1;
               }
            }
         }   
      }
      
      for (n=0; n<SO->N_Node; ++n) { if (blur_zone_2[n]) blur_zone[n] = 1; }
      if (!SameZone) { SUMA_free(blur_zone_2); blur_zone_2 = NULL; }

      if (N_zeros) {
         SUMA_LHv("Have %d redundancies at pass %d (%d nodes in mask).\n"
                  " We must reduce redundancies.\n"
                  " Have addtional blur fwhm of %f, sigma %g, Niter %d\n", 
                  N_zeros, ipass, N_nmask, dfwhm, sigma, N_iter);
         if (!wgt) {
            wgt = SUMA_Chung_Smooth_Weights_07(SO, sigma);
            if (!wgt) {
               SUMA_SL_Err("Failed to compute weights.\n");
               SUMA_RETURN(NOPE);
            }
         }
         nn = 1; fwhmttt = -1.0;
         if (!SUMA_Chung_Smooth_07_dset ( SO, wgt, 
                                       &nn, &fwhmttt, 
                                       dset, NULL, blur_zone, 1)) {
            SUMA_S_Err("Failed in  SUMA_Chung_Smooth_07_dset");
            SUMA_RETURN(NOPE);                            
         }
      }
      if (LocalHead) {
         sprintf(stmp,"junk_blurzone_pass%d.1D.dset",ipass);
         SUMA_WRITE_ARRAY_1D(blur_zone, SO->N_Node, 1, stmp);
      }
      SUMA_free(c); c = NULL;
      if (!SameZone) { SUMA_free(blur_zone); blur_zone = NULL; }
      ++ipass;
   } while (N_zeros > 0.01 *  N_nmask);
   
   if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;
   if (bfull) bfull = NULL;
   SUMA_RETURN(YUP);
}
 
static int UseSliceFWHM = 0;
void SUMA_Set_UseSliceFWHM(int v) { UseSliceFWHM = v; }
int SUMA_Get_UseSliceFWHM(void) { return(UseSliceFWHM); }

/*!
   A wrapper to make repeated calls to SUMA_estimate_FWHM_1dif for a dataset
*/
float *SUMA_estimate_dset_FWHM_1dif(SUMA_SurfaceObject *SO, SUMA_DSET *dset, 
                                    int *icols, int N_icols, byte *nmask, 
                                    int nodup, char *options)
{
   static char FuncName[]={"SUMA_estimate_dset_FWHM_1dif"};
   int k, jj, N_nmask=0, n= 0;
   float *fwhmv=NULL, *fin_orig=NULL;
   byte *bfull=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   if (!dset || !SO) {
      SUMA_S_Errv("NULL input fim=%p, SO=%p\n", dset, SO);
      SUMA_RETURN(NULL);
   }
   if (!SO->FN || !SO->EL) {
      /* do it here */
      if (!SO->EL && !(SO->EL = SUMA_Make_Edge_List_eng (SO->FaceSetList, SO->N_FaceSet, SO->N_Node, SO->NodeList, 0, SO->idcode_str))) {
         SUMA_S_Err("Failed to create Edge_List");
         SUMA_RETURN(NULL);
      }
      if (!SO->FN && !(SO->FN = SUMA_Build_FirstNeighb( SO->EL, SO->N_Node, SO->idcode_str)) ) {
         SUMA_S_Err("Failed to create FirstNeighb");
         SUMA_RETURN(NULL);
      }
   }


   if (!(fwhmv = (float *)SUMA_calloc(N_icols, sizeof(float)))) {
      SUMA_S_Err("Failed to callocate");
      SUMA_RETURN(fwhmv);
   }
   
   if (nmask) {
      bfull = (byte*)SUMA_malloc(sizeof(byte)*SO->N_Node);
      memcpy((void*)bfull, (void*)nmask, sizeof(byte)*SO->N_Node);
   }
   
   /* Begin operation for each column */
   for (k=0; k < N_icols; ++k) {
      /* get a float copy of the data column */
      fin_orig = SUMA_DsetCol2Float (dset, icols[k], 1);
      if (!fin_orig) {
         SUMA_SL_Crit("Failed to get copy of column. Woe to thee!");
         SUMA_free(fwhmv); fwhmv = NULL;
         goto CLEANUP;
      }
      /* make sure column is not sparse, one value per node */
      if (k==0) {
         SUMA_LH( "Special case k = 0, going to SUMA_MakeSparseColumnFullSorted");
         bfull = NULL;
         if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(dset), 0.0, &bfull, dset, SO->N_Node)) {
            SUMA_S_Err("Failed to get full column vector");
            SUMA_free(fwhmv); fwhmv = NULL;
            goto CLEANUP;
         }
         if (bfull) {
            SUMA_LH( "Something was filled in SUMA_MakeSparseColumnFullSorted\n" );
            /* something was filled in good old SUMA_MakeSparseColumnFullSorted */
            if (nmask) {   /* combine bfull with nmask */
               SUMA_LH( "Merging masks\n" );
               for (jj=0; jj < SO->N_Node; ++jj) { if (nmask[jj] && !bfull[jj]) nmask[jj] = 0; }   
            } else { nmask = bfull; }
         } 
         if (nmask) {
            N_nmask = 0;
            for (n=0; n<SO->N_Node; ++n) { if (nmask[n]) ++ N_nmask; }
            SUMA_LHv("Blurring with node mask (%d nodes in mask)\n", N_nmask);
            if (!N_nmask) {
               SUMA_S_Warn("Empty mask, nothing to do");
               SUMA_free(fwhmv); fwhmv = NULL; goto CLEANUP;
            }
         }
      } else {
         SUMA_LH( "going to SUMA_MakeSparseColumnFullSorted");
         if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(dset), 0.0, NULL, dset, SO->N_Node)) {
            SUMA_S_Err("Failed to get full column vector");
            SUMA_free(fwhmv); fwhmv = NULL;
            goto CLEANUP;
         }
         /* no need for reworking nmask and bfull for each column...*/
         
      }
      
      if (SUMA_Get_UseSliceFWHM()) {
         fwhmv[k] = SUMA_estimate_slice_FWHM_1dif( SO, fin_orig, nmask, nodup, NULL, NULL);
      } else {
         fwhmv[k] = SUMA_estimate_FWHM_1dif( SO, fin_orig, nmask, nodup);
      }
      if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL; /* just in case, this one's still alive from a GOTO */
   } /* for k */

   CLEANUP:
   
   if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL; /* just in case, this one's still alive from a GOTO */
   if (bfull) SUMA_free(bfull); bfull=NULL; 
   SUMA_RETURN(fwhmv);
}
#define OK_FWHM_DBG
static int DbgFWHM_1_dif=0;
void SUMA_SetDbgFWHM(int i) { DbgFWHM_1_dif=i; return; }
int SUMA_GetDbgFWHM(void) { return(DbgFWHM_1_dif); }
/*!
   Estimate the FWHM on a surface. 
   FWHM based on implementation in mri_estimate_FWHM_1dif (Forman et. al 1995)
   SO (SUMA_SurfaceObject *) La surface
   fim (float *) SO->N_Node x 1 vector of data values
   mask (byte *) SO->N_Node x 1 mask vector (NULL for no masking)
   nodup (int ) 0- allow a segment to be counted twice (uncool, but slightly faster)
               1- Do not allow a segment to be counted twice (respectful approach)
*/
float SUMA_estimate_FWHM_1dif( SUMA_SurfaceObject *SO, float *fim , byte *nmask, int nodup )
{
   static char FuncName[]={"SUMA_estimate_FWHM_1dif"};
   
   double ds;                  /* average segment size */
   double fsum, fsq, var , arg ;
   double dfds, dfdssum, dfdssq, varss;
   int count, counts, oke, iseg, k, in, ink;
   float ss=-1.0f , par[2], prob=0.0, stat;
   byte *visited = NULL;
   FILE *fdbg=NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   #ifdef OK_FWHM_DBG
      if (SUMA_GetDbgFWHM()) {
         SUMA_S_Warn("Function in debug mode. File of same name created!\n");
         fdbg = fopen(FuncName,"w");
         fprintf(fdbg,"#--------------------\n#n1   n2  SegLen   dfds\n");
      }
   #endif
   if (!fim || !SO) {
      SUMA_S_Errv("NULL input fim=%p, SO=%p\n", fim, SO);
      SUMA_RETURN(ss);
   }
   
   if (!SO->FN || !SO->EL) {
      if (!SUMA_SurfaceMetrics_eng(SO, "EdgeList", NULL, 0, SUMAg_CF->DsetList)){
         SUMA_SL_Err("Failed to create needed accessories");
         SUMA_RETURN(ss);
      }
   } 
   if (!SO->MF || !SO->PolyArea) {
      if (!SUMA_SurfaceMetrics_eng(SO, "MemberFace|PolyArea", NULL, 0, SUMAg_CF->DsetList)){
         SUMA_SL_Err("Failed to create needed accessories");
         SUMA_RETURN(ss);
      }
   }

   if (!SO->FN || !SO->EL || !SO->MF || !SO->PolyArea) {
      SUMA_S_Errv("J'ai besoin des voisins(%p) et des cotes (%p), cherie\nEt en plus, MF(%p) and PolyArea(%p)", 
            SO->FN, SO->EL, SO->MF, SO->PolyArea);
      SUMA_RETURN(ss);
   }
   /*----- estimate the variance of the data -----*/

   fsum = 0.0; fsq = 0.0; count = 0;
   for (in = 0;  in < SO->N_Node;  in++){
      if( !nmask || nmask[in] ) { count++; arg = fim[in]; fsum += arg; fsq  += arg*arg; }
   }
   if( count < 9 || fsq <= 0.0 ){     /* no data? */
      SUMA_RETURN(ss) ;
   }
   
   var = (fsq - (fsum * fsum)/count) / (count-1.0);
   if( var <= 0.0 ){                  /* crappy data? */
      SUMA_RETURN(ss);
   }

   if (nodup) {
      if (!(visited = (byte *)SUMA_calloc(SO->EL->N_EL, sizeof(byte)))) {
         SUMA_S_Err("Failed to bytocate for visited.");
         SUMA_RETURN(ss);
      }
   }
      
   /*----- estimate the partial derivatives -----*/

   dfdssum = 0.0;   
   dfdssq  = 0.0;   
   counts  = 0;
   ds = 0.0;     
   for (in = 0;  in < SO->N_Node;  in++){
      if( !nmask || nmask[in] ){
         arg = fim[in] ;
         for(k=0; k < SO->FN->N_Neighb[in]; ++k) { 
            ink = SO->FN->FirstNeighb[in][k];
            if (!nmask || nmask[ink]) { /* neighbour also in mask */
               /* locate the segment, and get distance */
               iseg = -1;
               if (in < ink) { SUMA_FIND_EDGE(SO->EL, in, ink, iseg); }
               else { SUMA_FIND_EDGE(SO->EL, ink, in, iseg); }
               if (iseg < 0) {
                  SUMA_S_Errv("Could not find segment between nodes %d and %d\nThis should not happen.\n", in, ink);
                  SUMA_RETURN(ss);
               }  
               if (nodup) { /* make sure edge is fresh */
                  if (visited[iseg]) oke = 0;
                  else { oke = 1; visited[iseg] = 1; }
               } else oke = 1;
               if (oke) {
                  ds += SO->EL->Le[iseg];
                  dfds = (fim[ink] - arg) ;
                  dfdssum += dfds; dfdssq += dfds*dfds; 
                  #ifdef OK_FWHM_DBG
                     if (SUMA_GetDbgFWHM()) {
                        fprintf(fdbg,"%5d %5d   %.3f  %.3f\n", in, ink, SO->EL->Le[iseg], dfds);      
                     }
                  #endif
                  counts++;
               }
            }
         }
      }
   }   

   
   if (visited) SUMA_free(visited); visited = NULL;
   
   /*----- estimate the variance of the partial derivatives -----*/

   varss = (counts < 36) ? 0.0
                       : (dfdssq - (dfdssum * dfdssum)/counts) / (counts-1.0);
   ds /= (double)counts;  /* the average segment length */

   /*----- now estimate the FWHMs -----*/

   /*---- 2.35482 = sqrt(8*log(2)) = sigma-to-FWHM conversion factor ----*/

   /* with random noise, varss should be about 2*var if the ratio of varss/var
   is not significantly different from 1 then set FWHM to 0.0 rather than the
   error flag */
   par[0] = (float)SO->N_Node;
   par[1] = (float)SO->N_Node; /* assuming independence of course */
   prob = THD_stat_to_pval(   SUMA_MAX_PAIR(varss/(2.0*var),(2.0*var)/varss), 
                              NI_STAT_FTEST , 
                              par ) ;
   if (prob > 0.01) {
      /* so what would the smallest acceptable FWHM be? */
      stat = THD_pval_to_stat (0.01, NI_STAT_FTEST, par);
      arg = 1.0 - 0.5*(2.0/stat);
      ss = 2.35482*sqrt( -1.0 / (4.0*log(arg)) );
      SUMA_S_Notev(  "   Distribution of data is possibly random noise (p=%f)\n"
                     "   Expect fwhm to be no different from 0 \n"
                     "   FWHM values up to %.2f(segments) or %.2f(mm)\n"
                     "   are likely meaningless (at p=0.01) on this mesh.\n\n",
                      prob, ss, ss*ds);
   }
   arg = 1.0 - 0.5*(varss/var);
   if (arg <= 0.0 || arg >= 1.0) {
      if (arg <=0 && prob > 0.01) ss = 0.0f; else ss = -1.0f;
   } else {
      ss = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*ds;
   }
   
   #ifdef OK_FWHM_DBG
      if (SUMA_GetDbgFWHM()) {
         fprintf(fdbg,"#counts=%d\n#var=%f\n#varss=%f\n#ds=%.3f\n#arg=%.3f\n#ss=%f\n", counts, var, varss, ds, arg, ss);
         fclose(fdbg);
      }
   #endif                  
   SUMA_RETURN(ss) ;
}
/*!
   Estimate the FWHM on a surface using the slice strips. 
   FWHM based on implementation in mri_estimate_FWHM_1dif (Forman et. al 1995)
   SO (SUMA_SurfaceObject *) La surface
   fim (float *) SO->N_Node x 1 vector of data values
   mask (byte *) SO->N_Node x 1 mask vector (NULL for no masking)
   nodup (int ) 0- allow a segment to be counted twice (uncool, but slightly faster)
               1- Do not allow a segment to be counted twice (respectful approach)
*/
float SUMA_estimate_slice_FWHM_1dif( SUMA_SurfaceObject *SO, float *fim , byte *nmask, int nodup, float *ssvr, DList **striplist_vec)
{
   static char FuncName[]={"SUMA_estimate_slice_FWHM_1dif"};
   
   double ds;                  /* average segment size */
   double fsum, fsq, var , arg , arg_n;
   double dfds, dfdssum, dfdssq, varss;
   int count, counts, oke, iseg, k, in0, in1, ink, in0_n, in1_n, ipl;
   float ssc=-1.0f , ssv[3], par[2], prob=0.0, stat;
   DList *striplist=NULL;
   float Eq[4], *p4=NULL, *p4_n=NULL, U[3], Un;
   void *vp=NULL;
   byte *visited = NULL;
   FILE *fdbg=NULL;
   SUMA_STRIP *strip=NULL;
   DListElmt *loope, *loopp, *loope_n, *loopp_n, *listelm;
   SUMA_Boolean clsd = NOPE;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   #ifdef OK_FWHM_DBG
      if (SUMA_GetDbgFWHM()) {
         SUMA_S_Warn("Function in debug mode. File of same name created!\n");
         fdbg = fopen(FuncName,"w");
         fprintf(fdbg,"#--------------------\n#n1   n2  SegLen   dfds\n");
      }
   #endif
   if (!fim || !SO) {
      SUMA_S_Errv("NULL input fim=%p, SO=%p\n", fim, SO);
      SUMA_RETURN(ssc);
   }
   
   if (!SO->FN || !SO->EL) {
      if (!SUMA_SurfaceMetrics_eng(SO, "EdgeList", NULL, 0, SUMAg_CF->DsetList)){
         SUMA_SL_Err("Failed to create needed accessories");
         SUMA_RETURN(ssc);
      }
   } 
   if (!SO->MF || !SO->PolyArea) {
      if (!SUMA_SurfaceMetrics_eng(SO, "MemberFace|PolyArea", NULL, 0, SUMAg_CF->DsetList)){
         SUMA_SL_Err("Failed to create needed accessories");
         SUMA_RETURN(ssc);
      }
   }

   if (!SO->FN || !SO->EL || !SO->MF || !SO->PolyArea) {
      SUMA_S_Errv("J'ai besoin des voisins(%p) et des cotes (%p), cherie\nEt en plus, MF(%p) and PolyArea(%p)", 
            SO->FN, SO->EL, SO->MF, SO->PolyArea);
      SUMA_RETURN(ssc);
   }
   /*----- estimate the variance of the data -----*/

   fsum = 0.0; fsq = 0.0; count = 0;
   for (in0 = 0;  in0 < SO->N_Node;  in0++){
      if( !nmask || nmask[in0] ) { count++; arg = fim[in0]; fsum += arg; fsq  += arg*arg; }
   }
   if( count < 9 || fsq <= 0.0 ){     /* no data? */
      SUMA_RETURN(ssc) ;
   }
   
   var = (fsq - (fsum * fsum)/count) / (count-1.0);
   if( var <= 0.0 ){                  /* crappy data? */
      SUMA_RETURN(ssc);
   }
   
   ssv[0] = ssv[1] = ssv[2] = -1.0;
   for (ipl=0; ipl<3;++ipl) {
      if (!striplist_vec) { /* need to create your own */
         /* get the intersection strips, start alond the various directions */
         Eq[0] = Eq[1]=Eq[2]=Eq[3] = 0.0;
         Eq[ipl] = 1.0; Eq[3] = -SO->Center[ipl];  /* 0==Saggittal, 1==Coronal, 2==Axial */
         SUMA_LHv("Kill me!\nEq:[%f %f %f %f], step: %f\n", Eq[0], Eq[1], Eq[2], Eq[3], SO->EL->AvgLe);
         if (!(striplist = SUMA_SliceAlongPlane(SO, Eq, SO->EL->AvgLe))) {
            SUMA_S_Err("Failed to slice along plane");
            SUMA_RETURN(ssc);
         }
         /*SUMA_display_edge_striplist(striplist, &(SUMAg_SVv[0]), SO, "ShowConnectedPoints"); */
      } else {
         striplist = striplist_vec[ipl];
      }

      /*----- estimate the partial derivatives -----*/
      SUMA_LHv("Have a striplist of %d elements\n", dlist_size(striplist));
      dfdssum = 0.0;   
      dfdssq  = 0.0;   
      counts  = 0;
      ds = 0.0;     
      strip = NULL;
      listelm = NULL;
      clsd = NOPE;
      do {
         if (!listelm) listelm = dlist_head(striplist);
         else listelm = dlist_next(listelm);
         strip = (SUMA_STRIP*)listelm->data;
         loope = NULL;
         loopp = NULL;
         SUMA_LHv("Have a strip of %d points/edges\n", dlist_size(strip->Edges));
         if (clsd = SUMA_isEdgeStripClosed(strip->Edges, SO)) { /* close list */
            dlist_ins_next(strip->Edges, dlist_tail(strip->Edges), (dlist_head(strip->Edges))->data);
            dlist_ins_next(strip->Points, dlist_tail(strip->Points), (dlist_head(strip->Points))->data);
         }
         do {
            if (!loope) {
               loope = dlist_head(strip->Edges);
               loopp = dlist_head(strip->Points);
            }
            loope_n = dlist_next(loope);  /* the next point */
            loopp_n = dlist_next(loopp); 
            /* which nodes from the edge? */
            in0 = SO->EL->EL[(int)loope->data][0];
            in1 = SO->EL->EL[(int)loope->data][1];
            in0_n = SO->EL->EL[(int)loope_n->data][0];
            in1_n = SO->EL->EL[(int)loope_n->data][1];
            if( !nmask || (nmask[in0]&& nmask[in1] && nmask[in0_n]&& nmask[in1_n])){
               p4 = (float*)loopp->data;
               arg = p4[3]*fim[in0] + (1.0-p4[3])*fim[in1] ;   /* interpolated value at intersection point on edge */
               p4_n = (float*)loopp_n->data;
               arg_n = p4_n[3]*fim[in0_n] + (1.0-p4_n[3])*fim[in1_n] ; 
               SUMA_UNIT_VEC(p4, p4_n, U, Un);
               ds += Un;
               dfds = arg_n - arg;
               dfdssum += dfds; dfdssq += dfds*dfds; 
               #ifdef OK_FWHM_DBG
                  if (SUMA_GetDbgFWHM()) {
                     fprintf(fdbg,"%5d %5d %5d %5d   %.3f  %.3f\n", in0, in1, in0_n, in1_n, Un, dfds);      
                  }
               #endif
               counts++;

            }
            loope = loope_n; loopp = loopp_n; 
         } while (loope != dlist_tail(strip->Edges));
         if (clsd) { /* now remove last addition */
            dlist_remove(strip->Edges, dlist_tail(strip->Edges), &vp);
            dlist_remove(strip->Points, dlist_tail(strip->Points), &vp);
         }
      } while (listelm != dlist_tail(striplist));   


      /*----- estimate the variance of the partial derivatives -----*/
      SUMA_LH("Mmmm, La Variance");
      varss = (counts < 36) ? 0.0
                          : (dfdssq - (dfdssum * dfdssum)/counts) / (counts-1.0);
      ds /= (double)counts;  /* the average segment length */

      /*----- now estimate the FWHMs -----*/

      /*---- 2.35482 = sqrt(8*log(2)) = sigma-to-FWHM conversion factor ----*/

      /* with random noise, varss should be about 2*var if the ratio of varss/var
      is not significantly different from 1 then set FWHM to 0.0 rather than the
      error flag */
      SUMA_LH("Mmmm, Le Bruit");
      par[0] = (float)SO->N_Node;
      par[1] = (float)SO->N_Node; /* assuming independence of course */
      prob = THD_stat_to_pval( SUMA_MAX_PAIR(varss/(2.0*var), (2.0*var)/varss)  , NI_STAT_FTEST , par ) ;
      if (prob > 0.01) {
         /* so what would the smallest acceptable FWHM be? */
         stat = THD_pval_to_stat (0.01, NI_STAT_FTEST, par);
         arg = 1.0 - 0.5*(2.0/stat);
         ssv[ipl] = 2.35482*sqrt( -1.0 / (4.0*log(arg)) );
         SUMA_S_Notev(  "   Distribution of data is possibly random noise (p=%f)\n"
                        "   Expect fwhm to be no different from 0 \n"
                        "   FWHM values up to %.2f(segments) or %.2f(mm)\n"
                        "   are likely meaningless (at p=0.01) on this mesh.\n\n", prob, ssv[ipl], ssv[ipl]*ds);
      }
      arg = 1.0 - 0.5*(varss/var);
      if (arg <= 0.0 || arg >= 1.0) {
         if (arg <=0 && prob > 0.01) ssv[ipl] = 0.0f; else ssv[ipl] = -1.0f;
      } else {
         ssv[ipl] = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*ds;
      }
      SUMA_LHv("The FWHM along plane %d: %f\n", ipl, ssv[ipl]);

      #ifdef OK_FWHM_DBG
         if (SUMA_GetDbgFWHM()) {
            fprintf(fdbg,"#counts=%d\n#var=%f\n#varss=%f\n#ds=%.3f\n#arg=%.3f\n#ss=%f\n", counts, var, varss, ds, arg, ssv[ipl]);
            fclose(fdbg);
         }
      #endif                  
      if (striplist_vec) {
         /* not yours, do not free */
         striplist = NULL;
      } else {
         SUMA_LH("Mmmm, La Liberte");
         SUMA_FREE_DLIST(striplist); striplist = NULL;
      }
   }
   /* combine, mean for now */
   if (ssv[0] >=0.0 && ssv[2]>=0.0 && ssv[3] >= 0.0) {
      ssc = (ssv[0]+ssv[1]+ssv[2])/3.0;
      if (ssvr) { ssvr[0] = ssv[0]; ssvr[1] = ssv[1]; ssvr[2] = ssv[2]; }
   } else {
      ssc = -1.0;
   }
   SUMA_RETURN(ssc) ;
}

SUMA_Boolean SUMA_Offset_Smooth_dset( 
   SUMA_SurfaceObject *SO, 
   float FWHM, float OffsetLim, 
   int N_iter,
   SUMA_DSET *dset, 
   SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask) 
{
   static char FuncName[]={"SUMA_Offset_Smooth_dset"};
   float *fout_final = NULL, *fbuf=NULL, *fin=NULL, *fout=NULL, 
         *fin_next = NULL, *fin_orig = NULL;
   float fp, dfp, fpj, wt, wts, sig, fwhm_orig, fwhm_out;
   double dj, ds2, scl;
   int n , k, j, niter, vnk, os, jj, nj=-1, *icols=NULL, N_icols, N_nmask;
   byte *bfull=NULL;
   SUMA_OFFSET_STRUCT *OffS_out = NULL;
   
   SUMA_Boolean LocalHead = YUP;

   SUMA_ENTRY;
   
   SUMA_S_Warn("Niter is not treated properly");
   SUMA_S_Warn("No useful weighting in place");
   
   SUMA_S_Warn("Useless and obsolete. DO NOT USE");
   SUMA_RETURN(NOPE);
   
   if (!SO || !dset) {
      SUMA_S_Errv("NULL SO (%p) or dset(%p)\n", SO, dset);
      SUMA_RETURN(NOPE);
   }
   
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN\n");
      SUMA_RETURN(NOPE);
   }
    
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(dset, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      SUMA_RETURN(NOPE);   
   }
   
   /* allocate for buffer and output */
   fbuf = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   fout_final = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   if (!fbuf || !fout_final) {
      SUMA_SL_Crit("Failed to allocate for fbuf and fout_final\n");
      SUMA_RETURN(NOPE);
   }
   
   if (cs->Send && N_icols > 1) {
      SUMA_S_Warn("Only 1st data column will be sent to SUMA in talk_mode.");
   }
   
   if (N_iter < 1) {
      SUMA_S_Errv("Niter = %d!\n", N_iter);
   }
   /* reduce FWHM by Niter */
   FWHM = FWHM/sqrt(N_iter);
   
   /* calculate sigma */
   if (FWHM > 0.0) {
      sig = FWHM /   2.354820;
      ds2 = 2*sig*sig;
      scl = iSQ_2PI/sig;
   } else {
      SUMA_S_Errv("Bad FWHM %f", FWHM);
      SUMA_RETURN(NOPE);
   }

   /* calculate the offset limit, if allowed */
   if (OffsetLim < 0.0) {
      if (sig > 0.0) {
         OffsetLim = 3.5*sig;
      } else {
         SUMA_S_Errv("Have OffsetLim =%f and no FWHM (%f) from which to estimate it.\n", OffsetLim, FWHM);
         SUMA_RETURN(NOPE);
      }
   }

   /* Check for plausible values */
   if (OffsetLim < 3*SO->EL->AvgLe) {
      int Niter_sug = SUMA_MAX_PAIR(1, (int)((float)N_iter*SUMA_POW2(OffsetLim/3.0/(float)SO->EL->AvgLe)));
      fprintf(SUMA_STDERR,"Error %s:%d\n"
                  "********************************************\n"
                  "Inapropriate values for Niter of %d and/or\n"
                  "FWHM of %.1f. Per iteration, fwhm is %.4f\n"
                  "and OffsetLim is set to %f.\n"
                  "But the internodal distance of %.3f is\n"
                  "too large for proper estimation.\n", 
                  FuncName, __LINE__,
                  N_iter, FWHM*sqrt(N_iter), FWHM, OffsetLim, SO->EL->AvgLe);
      if (Niter_sug < N_iter) {
         fprintf(SUMA_STDERR,"Try replacing Niter by %d. If you still get\n"
                  "this warning then your FWHM is too small for your\n"
                  "mesh\n"
                  "********************************************\n",
                   Niter_sug);
      } else {
         fprintf(SUMA_STDERR,"Your FWHM is too small for this \n"
                  "mesh\n"
                  "********************************************\n");
      }
      SUMA_RETURN(NOPE); 
   }
   SUMA_LHv("OffsetLim set to %f\nSigma set to %f per iteration with %d iterations (FWHM per iteration=%f)\n", OffsetLim, sig, N_iter, FWHM);
     
   /* Begin filtering operation for each column */
   for (k=0; k < N_icols; ++k) {
      /* get a float copy of the data column */
      fin_orig = SUMA_DsetCol2Float (dset, icols[k], 1);
      if (!fin_orig) {
         SUMA_SL_Crit("Failed to get copy of column. Woe to thee!");
         SUMA_RETURN(NOPE);
      }
      /* make sure column is not sparse, one value per node */
      if (k==0) {
         SUMA_LH( "Special case k = 0, going to SUMA_MakeSparseColumnFullSorted");
         bfull = NULL;
         if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(dset), 0.0, &bfull, dset, SO->N_Node)) {
            SUMA_S_Err("Failed to get full column vector");
            SUMA_RETURN(NOPE);
         }
         if (bfull) {
            SUMA_LH( "Something was filled in SUMA_MakeSparseColumnFullSorted\n" );
            /* something was filled in good old SUMA_MakeSparseColumnFullSorted */
            if (nmask) {   /* combine bfull with nmask */
               SUMA_LH( "Merging masks\n" );
               for (jj=0; jj < SO->N_Node; ++jj) { if (nmask[jj] && !bfull[jj]) nmask[jj] = 0; }   
            } else { nmask = bfull; }
         } 
         if (nmask) {
            N_nmask = 0;
            for (n=0; n<SO->N_Node; ++n) { if (nmask[n]) ++ N_nmask; }
            SUMA_LHv("Blurring with node mask (%d nodes in mask)\n", N_nmask);
            if (!N_nmask) {
               SUMA_S_Warn("Empty mask, nothing to do");
               goto CLEANUP;
            }
         }
         /* now calculate the neighbor offset structure */
         SUMA_LHv("Calculating OffS_out FWHM=%f, OffsetLim = %f\n", FWHM, OffsetLim);
         OffS_out = SUMA_FormNeighbOffset (SO, OffsetLim, NULL, nmask, FWHM);
      } else {
         SUMA_LH( "going to SUMA_MakeSparseColumnFullSorted");
         if (!SUMA_MakeSparseColumnFullSorted(&fin_orig, SDSET_VECFILLED(dset), 0.0, NULL, dset, SO->N_Node)) {
            SUMA_S_Err("Failed to get full column vector");
            SUMA_RETURN(NOPE);
         }
         /* no need for reworking nmask and bfull for each column...*/
         
      }
           
      if (cs->Send && k == 0) { /* send the first monster */
         if (!SUMA_SendToSuma (SO, cs, (void *)fin_orig, SUMA_NODE_RGBAb, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
      
      if (SUMA_Get_UseSliceFWHM()) {
         fwhm_orig = SUMA_estimate_slice_FWHM_1dif( SO, fin_orig, nmask, 1, NULL, NULL);
      } else {
         fwhm_orig = SUMA_estimate_FWHM_1dif(SO, fin_orig, nmask, 1);
      }
      SUMA_LHv("FWHM_orig for col. %d is : %f\n", k, fwhm_orig);
      
      /* filter this column for each of the iterations */
      fin_next = fin_orig;
      for (niter=0; niter < N_iter; ++niter) {
         SUMA_LHv("niter %d\n", niter);
         if ( niter % 2 ) { /* odd */
            fin = fin_next; /* input from previous output buffer */
            fout = fout_final; /* results go into final vector */
            fin_next = fout_final; /* in the next iteration, the input is from fout_final */
         } else { /* even */
            /* input data is in fin_new */
            fin = fin_next;
            fout = fbuf; /* results go into buffer */
            fin_next = fbuf; /* in the next iteration, the input is from the buffer */
         }
         
         if (!nmask) {
            for (n=0; n < SO->N_Node; ++n) {
               fp = fin[n]; /* kth value at node n */
               wt = iSQ_2PI/sig; 
               dfp = wt*fp; wts = wt;
               /* if (n == 1358) { fprintf(SUMA_STDOUT,"fin[%d]=%f; Have %d neighbs, wt = %f\n", n, fin[n], OffS_out[n].N_Neighb, wt); } */
               for (j=0; j<OffS_out[n].N_Neighb; ++j) {
                  nj = OffS_out[n].Neighb_ind[j];
                  fpj = fin[nj]; /* value at jth neighbor of n */
                  wt = OffS_out[n].Neighb_dist[j];   /*  */ 
                  dfp += wt * (fpj);  wts += wt;
                  /* if (n == 1358) { fprintf(SUMA_STDOUT,"   fin[%d]=%f; wt=%f, dfp=%f; wts=%f\n", nj, fin[nj],  wt, dfp, wts); }  */
               }/* for j*/
               fout[n] = dfp/(wts);
            }/* for n */
         }else{/* masking potential */
            for (n=0; n < SO->N_Node; ++n) {
               fp = fin[n]; /* kth value at node n */
               wt = iSQ_2PI/sig; 
               dfp = wt*fp; wts = wt;
               if (nmask[n]) {
                  for (j=0; j<OffS_out[n].N_Neighb; ++j) {
                     nj = OffS_out[n].Neighb_ind[j];
                     if (nmask[nj] || !strict_mask) { /* consider only neighbors that are in mask if strict_mask is 1*/ 
                        fpj = fin[nj]; /* value at jth neighbor of n */
                        wt = OffS_out[n].Neighb_dist[j];   
                        dfp += wt * (fpj);  wts += wt;
                     }
                  }/* for j*/
               }
               fout[n] = dfp/(wts);
            }/* for n */
         }/* masking potential */
            
        if (cs->Send && k == 0) {
            if (!SUMA_SendToSuma (SO, cs, (void *)fout, SUMA_NODE_RGBAb, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }

      } /* for niter */
      
      if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL;
      
      /* Now we need to shove the filtered data back into the dset */
      if (N_iter % 2) { /* odd */
         fout = fbuf;
      } else fout = fout_final;
      
      if (!SUMA_Float2DsetCol (dset, icols[k], fout, 1, nmask)) {
         SUMA_S_Err("Failed to update dset's values");
         SUMA_RETURN(NOPE);      
      }

      if (SUMA_Get_UseSliceFWHM()) {
         fwhm_out = SUMA_estimate_slice_FWHM_1dif( SO, fout, nmask, 1, NULL, NULL);
      } else {
         fwhm_out = SUMA_estimate_FWHM_1dif(SO, fout, nmask, 1);
      }
      SUMA_LHv("FWHM_out for col. %d is : %f\n", k, fwhm_out);

      
   } /* for each col */
   
   CLEANUP:
   OffS_out = SUMA_free_NeighbOffset (SO, OffS_out);
   if (fin_orig) SUMA_free(fin_orig); fin_orig = NULL; /* just in case, this one's still alive from a GOTO */
   /* Pre Dec 06 stupidity: if (bfull == nmask) { if (nmask) SUMA_free(nmask); nmask = NULL; bfull = NULL; } */
   if (bfull) SUMA_free(bfull); bfull = NULL;
   if (fbuf) SUMA_free(fbuf); fbuf = NULL;
   if (fout_final) SUMA_free(fout_final); fout_final = NULL;
   
   SUMA_RETURN(YUP);
}


/*!
   \brief determine overall orientation of triangle normals and change triangle orientation if required
   \param NodeList (float *) xyz vector of node coords
   \param N_Node (int) number of nodes
   \param FaceSetList (int *) [n1 n2 n3] vector of triangles
   \param N_FaceSet (int) number of triangles
   \param orient (int) 0: Do not change orientation
                        1: make sure most normals point outwards from center. Flip all triangles if necessary (unless Force is used)
                        -1: make sure most normals point towards center. Flip all triangles if necessary (unless Force is used)
   \param Force (int) 1: Force the flipping of only those triangles whose normals point in the wrong direction (opposite to orient).
                           With this option, you will destroy the winding consistency of a surface!  
   \return ans (int):   0: error
                        1: most normals were pointing outwards
                       -1:  most normals were pointing inwards
*/
int SUMA_OrientTriangles (float *NodeList, int N_Node, int *FaceSetList, int N_FaceSet, int orient, int Force)
{
   static char FuncName[]={"SUMA_OrientTriangles"};
   int i, j, ip, negdot, posdot, sgn, NP, ND, n1, n2, n3, flip;
   float d1[3], d2[3], c[3], tc[3], U[3], dot, *norm, mag;
   FILE *fout = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   if (!NodeList || !FaceSetList || !N_Node || !N_FaceSet) {
      SUMA_SL_Err("Null or no input");
      SUMA_RETURN(0);
   }
   norm = (float *)SUMA_calloc(3*N_FaceSet, sizeof(float));
   if (!norm) {
      SUMA_SL_Crit("Failed to allocate for norm"); SUMA_RETURN(0);
   }
   if (Force) {
      SUMA_SL_Warn("Using Force option! You might destroy triangulation consistency of surface!");
   }
   NP = ND = 3;
   /* calculate the center coordinate */
   c[0] = c[1] = c[2] = 0.0;
   for (i=0; i < N_Node; ++i) {
      ip = ND * i; c[0] += NodeList[ip]; c[1] += NodeList[ip+1]; c[2] += NodeList[ip+2];    
   }
   c[0] /= N_Node; c[1] /= N_Node; c[2] /= N_Node;
   
   /* calculate normals for each triangle, taken from SUMA_SurfNorm*/
   if (0 && LocalHead) {
      SUMA_SL_Note("Writing SUMA_OrientTriangles.1D");
      fout = fopen("SUMA_OrientTriangles.1D", "w");
   }
   negdot = 0; posdot = 0;
   for (i=0; i < N_FaceSet; i++) {
      ip = NP * i;
      n1 = FaceSetList[ip]; n2 = FaceSetList[ip+1]; n3 = FaceSetList[ip+2];   /* node indices making up triangle */
      tc[0] = (NodeList[3*n1]   + NodeList[3*n2]   + NodeList[3*n3]  )/3; /* centroid of triangle */
      tc[1] = (NodeList[3*n1+1] + NodeList[3*n2+1] + NodeList[3*n3+1])/3; 
      tc[2] = (NodeList[3*n1+2] + NodeList[3*n2+2] + NodeList[3*n3+2])/3; 
      /* calc normal */
      for (j=0; j < 3; j++) {
         d1[j] = NodeList[(ND*n1)+j] - NodeList[(ND*n2)+j];
         d2[j] = NodeList[(ND*n2)+j] - NodeList[(ND*n3)+j];
      }
      norm[ip] = d1[1]*d2[2] - d1[2]*d2[1];
      norm[ip+1] = d1[2]*d2[0] - d1[0]*d2[2];
      norm[ip+2] = d1[0]*d2[1] - d1[1]*d2[0];
      
      /* dot the normal with vector from center to node */
      U[0] = tc[0] - c[0]; U[1] = tc[1] - c[1]; U[2] = tc[2] - c[2];
      SUMA_DOTP_VEC(U, &(norm[ip]), dot, 3, float, float);
      if (dot < 0) {
         ++negdot;
         if (0 && LocalHead) { fprintf (SUMA_STDERR,"%s: Triangle %d has a negative dot product %f\nc  =[%.3f %.3f %.3f]\ntc =[%.3f %.3f %.3f]\nnorm=[%.3f %.3f %.3f]\n",
                      FuncName, i, dot, c[0], c[1], c[2], tc[0], tc[1], tc[2], norm[ip+0], norm[ip+1], norm[ip+2]); }
         
      } else {
         if (fout) { 
               SUMA_NORM_VEC(norm,3,mag); if (!mag) mag = 1; mag /= 5; 
               if (fout) fprintf (fout,"%.3f %.3f %.3f %.3f %.3f %.3f\n", tc[0], tc[1], tc[2], tc[0]+norm[ip+0]/mag, tc[1]+norm[ip+1]/mag, tc[2]+norm[ip+2]/mag);
         }
         ++posdot;
      }      
      
      if (Force) {
         if ( (dot < 0 && orient > 0) || (dot > 0 && orient < 0)) {
            n1 = FaceSetList[ip]; FaceSetList[ip] = FaceSetList[ip+2]; FaceSetList[ip+2] = n1;  
         }
      }
   }
   if (fout) fclose(fout); fout = NULL;
   flip = 0; sgn = 0;
   if (posdot >= negdot) {
      SUMA_LH("Normals appear to point away from center");
      sgn = 1;
      if (orient < 0) flip = 1;
   } else {
      SUMA_LH("Normals appear to point towards center");
      sgn = -1;
      if (orient > 0) flip = 1;
   }
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\n Found %d positive dot products and %d negative ones.\n", FuncName, posdot, negdot);
   }
   
   if (flip && !Force) {
      SUMA_LH("Flipping");
      for (i=0; i < N_FaceSet; i++) {
         ip = NP * i;
         n1 = FaceSetList[ip]; FaceSetList[ip] = FaceSetList[ip+2]; FaceSetList[ip+2] = n1;
      }
   }
   
   if (norm) SUMA_free(norm); norm = NULL;
   
   SUMA_RETURN(sgn);
}

/* 
   \brief a function to turn a surface patch (not all vertices are in use) into a surface where all nodes are used.
   \param NodeList (float *) N_Nodelist * 3 vector containing xyz triplets for vertex coordinates
   \param N_NodeList (int) you know what
   \param PatchFaces (int *) N_PatchFaces * PatchDim vector containing node indices forming triangulation
   \param N_PatchFaces (int) obvious
   \param PatchDim (int) 3 for triangular, 4 for rectangular patches etc. ..
   \return SO (SUMA_SurfaceObject *) surface object structure with NodeList, N_NodeList, FaceSetList and N_FaceSetList
                                     filled. Note node indexing in SO is not related to the indexing in patch.
   - Nothing but NodeList and FaceSetList is created here. KEEP IT THAT WAY
*/

SUMA_SurfaceObject *SUMA_Patch2Surf(float *NodeList, int N_NodeList, int *PatchFaces, int N_PatchFaces, int PatchDim)
{
   static char FuncName[]={"SUMA_Patch2Surf"};
   SUMA_SurfaceObject *SO=NULL;
   int i = 0, cnt = 0;
   int *imask = NULL;
   int N_Node = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!NodeList || !PatchFaces) {
      SUMA_SL_Err("Null input");
      SUMA_RETURN(SO);
   }
   
   imask = (int*)SUMA_calloc(N_NodeList , sizeof(int));
   if (!imask) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(SO);
   }  
   /* count the number of nodes and initialize imask*/
   SO = SUMA_Alloc_SurfObject_Struct(1);
   if (!SO) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(SO);
   }
   SO->N_FaceSet = N_PatchFaces;
   SO->N_Node = 0;
   for (i=0; i<3*N_PatchFaces; ++i) {
      if (!imask[PatchFaces[i]]) {
         imask[PatchFaces[i]] = -1;
         ++SO->N_Node;
      }
   }
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: %d nodes in patch\n", FuncName, SO->N_Node);
   }
   SO->NodeList = (float *)SUMA_malloc(sizeof(float)*3*SO->N_Node);
   SO->FaceSetList = (int *)SUMA_malloc(sizeof(int)*PatchDim*N_PatchFaces);
   if (!SO->NodeList || !SO->FaceSetList) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(SO);
   }
   SO->NodeDim = 3;
   SO->FaceSetDim = PatchDim;
   
   cnt = 0;
   for (i=0; i<3*N_PatchFaces; ++i) {
      if (imask[PatchFaces[i]] < 0) {
         imask[PatchFaces[i]] = cnt;
         SO->NodeList[3*cnt  ] = NodeList[3*PatchFaces[i]  ];
         SO->NodeList[3*cnt+1] = NodeList[3*PatchFaces[i]+1];
         SO->NodeList[3*cnt+2] = NodeList[3*PatchFaces[i]+2];
         ++cnt;
      }
      SO->FaceSetList[i] = imask[PatchFaces[i]];
   }
   
   SUMA_RETURN(SO);
}
/*!
   \brief a function to return a mask indicating if a node is 
   part of a patch or not
   isNodeInPatch = SUMA_MaskOfNodesInPatch( SUMA_SurfaceObject *SO, int * N_NodesUsedInPatch);

   \param SO (SUMA_SurfaceObject *) the surface object
   \param N_NodesUsedInPatch (int *) will contain the number of nodes used in the mesh of the patch (that is SO->FaceSetList)
                                     if *N_NodesUsedInPatch == SO->N_Node then all nodes in the nodelist are used
                                     in the mesh
   \return isNodeInPatch (SUMA_Boolean *) a vector SO->N_Node long such that if isNodeInPatch[n] = YUP then node n is used
                                    in the mesh 
*/
SUMA_Boolean *SUMA_MaskOfNodesInPatch(
                  SUMA_SurfaceObject *SO, int *N_NodesUsedInPatch)
{
   static char FuncName[]={"SUMA_MaskOfNodesInPatch"};
   int k;
   SUMA_Boolean *NodesInPatchMesh = NULL;

   SUMA_ENTRY;

   *N_NodesUsedInPatch = 0;

   if (!SO) {
      SUMA_SL_Err("NULL SO");
      SUMA_RETURN(NULL);
   }
   if (!SO->FaceSetList || !SO->N_FaceSet) {
      SUMA_SL_Err("NULL or empty SO->FaceSetList");
      SUMA_RETURN(NULL);
   }

   NodesInPatchMesh = (SUMA_Boolean *)
                        SUMA_calloc(SO->N_Node, sizeof(SUMA_Boolean)); 
   if (!NodesInPatchMesh) {
      SUMA_SL_Crit("Failed to allocate for NodesInPatchMesh");
      SUMA_RETURN(NULL);
   }
   for (k=0; k<SO->FaceSetDim*SO->N_FaceSet; ++k) {
      if (!NodesInPatchMesh[SO->FaceSetList[k]]) { 
         ++*N_NodesUsedInPatch;
         NodesInPatchMesh[SO->FaceSetList[k]] = 1;         
      }
   }

   SUMA_RETURN(NodesInPatchMesh);  
}

/*!
   Given a set of node indices, return a patch of the original surface that contains them
   
   Patch = SUMA_getPatch (NodesSelected, N_Nodes, Full_FaceSetList, N_Full_FaceSetList, Memb, MinHits)

   \param NodesSelected (int *) N_Nodes x 1 Vector containing indices of selected nodes. 
            These are indices into NodeList making up the surface formed by Full_FaceSetList.
   \param N_Nodes (int) number of elements in NodesSelected
   \param Full_FaceSetList (int *) N_Full_FaceSetList  x 3 vector containing the triangles forming the surface 
   \param N_Full_FaceSetList (int) number of triangular facesets forming the surface
   \param Memb (SUMA_MEMBER_FACE_SETS *) structure containing the node membership information (result of SUMA_MemberFaceSets function)
   \param MinHits (int) minimum number of nodes to be in a patch before the patch is selected. 
         Minimum is 1, Maximum logical is 3, assuming you do not have repeated node indices
         in NodesSelected.
   \ret Patch (SUMA_PATCH *) Structure containing the patch's FaceSetList, FaceSetIndex (into original surface) and number of elements.
         returns NULL in case of trouble.   Free Patch with SUMA_freePatch(Patch);

   \sa SUMA_MemberFaceSets, SUMA_isinbox, SUMA_PATCH
*/

SUMA_PATCH * SUMA_getPatch (  int *NodesSelected, int N_Nodes, 
                              int *Full_FaceSetList, int N_Full_FaceSetList, 
                              SUMA_MEMBER_FACE_SETS *Memb, int MinHits)
{
   static char FuncName[]={"SUMA_getPatch"};
   int * BeenSelected;
   int i, j, node, ip, ip2, NP;
   SUMA_PATCH *Patch;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   NP = 3;
   BeenSelected = (int *)SUMA_calloc (N_Full_FaceSetList, sizeof(int));
   Patch = (SUMA_PATCH *)SUMA_malloc(sizeof(SUMA_PATCH));
   
   if (!BeenSelected || !Patch) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for BeenSelected or patch.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   /* find out the total number of facesets these nodes are members of */
   Patch->N_FaceSet = 0; /* total number of facesets containing these nodes */
   for (i=0; i < N_Nodes; ++i) {
      node = NodesSelected[i];
      for (j=0; j < Memb->N_Memb[node]; ++j) {
         if (!BeenSelected[Memb->NodeMemberOfFaceSet[node][j]]) {
            /* this faceset has not been selected, select it */
            ++ Patch->N_FaceSet;
         }
         ++ BeenSelected[Memb->NodeMemberOfFaceSet[node][j]];
      }   
   }
   
   /* now load these facesets into a new matrix */
   
   Patch->FaceSetList = (int *) SUMA_calloc (Patch->N_FaceSet * 3, sizeof(int));
   Patch->FaceSetIndex = (int *) SUMA_calloc (Patch->N_FaceSet, sizeof(int));
   Patch->nHits = (int *) SUMA_calloc (Patch->N_FaceSet, sizeof(int));
   
   if (!Patch->FaceSetList || !Patch->FaceSetIndex || !Patch->nHits) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for Patch->FaceSetList || Patch_FaceSetIndex.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   j=0;
   for (i=0; i < N_Full_FaceSetList; ++i) {
      if (BeenSelected[i] >= MinHits) {
         Patch->nHits[j] = BeenSelected[i];
         Patch->FaceSetIndex[j] = i;
         ip = NP * j;
         ip2 = NP * i;
         Patch->FaceSetList[ip] = Full_FaceSetList[ip2];
         Patch->FaceSetList[ip+1] = Full_FaceSetList[ip2+1];
         Patch->FaceSetList[ip+2] = Full_FaceSetList[ip2+2];
         ++j;
      }
   }
   
   /* reset the numer of facesets because it might have changed given the MinHits condition,
   It won't change if MinHits = 1.
   It's OK not to change the allocated space as long as you are using 1D arrays*/
   Patch->N_FaceSet = j;
   
   if (BeenSelected) SUMA_free(BeenSelected);
   
   SUMA_RETURN(Patch);   
}

/*!
   ans = SUMA_freePatch (SUMA_PATCH *Patch) ;
   frees Patch pointer 
   \param Patch (SUMA_PATCH *) Surface patch pointer
   \ret ans (SUMA_Boolean)
   \sa SUMA_getPatch
*/

SUMA_Boolean SUMA_freePatch (SUMA_PATCH *Patch) 
{
   static char FuncName[]={"SUMA_freePatch"};
   
   SUMA_ENTRY;
   
   
   if (Patch->FaceSetIndex) SUMA_free(Patch->FaceSetIndex);
   if (Patch->FaceSetList) SUMA_free(Patch->FaceSetList);
   if (Patch->nHits) SUMA_free(Patch->nHits);
   if (Patch) SUMA_free(Patch);
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_ShowPatch (SUMA_PATCH *Patch, FILE *Out) 
{
   static char FuncName[]={"SUMA_ShowPatch"};
   int ip, i;
   
   SUMA_ENTRY;
   
   if (!Out) Out = stderr;
   
   fprintf (Out, "Patch Contains %d triangles:\n", Patch->N_FaceSet);
   fprintf (Out, "FaceIndex (nHits): FaceSetList[0..2]\n");
   for (i=0; i < Patch->N_FaceSet; ++i) {
      ip = 3 * i;   
      fprintf (Out, "%d(%d):   %d %d %d\n",
            Patch->FaceSetIndex[i], Patch->nHits[i], Patch->FaceSetList[ip],
            Patch->FaceSetList[ip+1], Patch->FaceSetList[ip+2]);
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Returns the contour of a patch 
   if you have the patch already created, then pass it in the last argument
   mode (int) 0: nice contour, not necessarily outermost boundary
              1: outermost edge, might look a tad jagged
*/
SUMA_CONTOUR_EDGES * SUMA_GetContour (
         SUMA_SurfaceObject *SO, int *Nodes, int N_Node, 
         int *N_ContEdges, int ContourMode, SUMA_PATCH *UseThisPatch)
{
   static char FuncName[]={"SUMA_GetContour"};
   SUMA_EDGE_LIST * SEL=NULL;
   SUMA_PATCH *Patch = NULL;
   int i, Tri, Tri1, Tri2, sHits;
   SUMA_CONTOUR_EDGES *CE = NULL;
   SUMA_Boolean *isNode=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   *N_ContEdges = -1;
   
   /* get the Node member structure if needed*/
   if (!SO->MF) {
      SUMA_SLP_Err("Member FaceSet not created.\n");
      SUMA_RETURN(CE);
   }  

   /* create a flag vector of which node are in Nodes */
   isNode = (SUMA_Boolean *) SUMA_calloc(SO->N_Node, sizeof(SUMA_Boolean));
   if (!isNode) {
      SUMA_SLP_Crit("Failed to allocate for isNode");
      SUMA_RETURN(CE);
   }
   
   for (i=0; i < N_Node; ++i) isNode[Nodes[i]] = YUP;
  
   if (UseThisPatch) {
      SUMA_LH("Using passed patch");
      Patch = UseThisPatch;
   } else { 
      SUMA_LH("Creating patch");
      switch (ContourMode) {
         case 0:
            Patch = SUMA_getPatch (Nodes, N_Node, SO->FaceSetList, SO->N_FaceSet, SO->MF, 2);
            break;
         case 1:
            Patch = SUMA_getPatch (Nodes, N_Node, SO->FaceSetList, SO->N_FaceSet, SO->MF, 1);
            break;
         default:
            SUMA_SL_Err("Bad contour mode"); SUMA_RETURN(NULL);
            break;
      }
   }
   if (LocalHead) SUMA_ShowPatch (Patch,NULL);
   
   if (Patch->N_FaceSet) {
      SEL = SUMA_Make_Edge_List_eng (  Patch->FaceSetList, 
                                       Patch->N_FaceSet, SO->N_Node, 
                                       SO->NodeList, 0, NULL);
   
      if (0 && LocalHead) SUMA_Show_Edge_List (SEL, NULL);
      /* allocate for maximum */
      CE = (SUMA_CONTOUR_EDGES *) 
            SUMA_calloc(SEL->N_EL, sizeof(SUMA_CONTOUR_EDGES));
      if (!CE) {
         SUMA_SLP_Crit("Failed to allocate for CE");
         SUMA_RETURN(CE);
      }
   
      switch (ContourMode) {
         case 0: /* a pretty contour, edges used here may 
                     not be the outermost of the patch */
            /* edges that are part of unfilled triangles are good */
            i = 0;
            *N_ContEdges = 0;
            while (i < SEL->N_EL) {
               if (SEL->ELps[i][2] == 2) {
                  Tri1 = SEL->ELps[i][1];
                  Tri2 = SEL->ELps[i+1][1];
                  sHits = Patch->nHits[Tri1] + Patch->nHits[Tri2];
                  if (sHits == 5 || sHits == 4) { 
                     /* one tri with 3 hits and one with 2 hits 
                        or 2 Tris with 2 hits each */
                     /* Pick edges that are part of only one 
                        triangle with three hits */
                     /* or two triangles with two hits */
                     /* There's one more condition, both nodes 
                        have to be a part of the original list */
                     if (isNode[SEL->EL[i][0]] && isNode[SEL->EL[i][1]]) {
                        CE[*N_ContEdges].n1 = SEL->EL[i][0];
                        CE[*N_ContEdges].n2 = SEL->EL[i][1];
                        ++ *N_ContEdges;

                        if (LocalHead) {
                           fprintf (SUMA_STDERR,
                                    "%s: Found edge made up of nodes [%d %d]\n",
                              FuncName, SEL->EL[i][0], SEL->EL[i][1]);
                        }
                     }
                  }
               }

               if (SEL->ELps[i][2] > 0) {
                  i += SEL->ELps[i][2];
               } else {
                  i ++;
               }
            }
            break;
         case 1: /* outermost contour, not pretty, 
                   but good for getting the outermost edge */
            i = 0;
            *N_ContEdges = 0;
            while (i < SEL->N_EL) {
               if (SEL->ELps[i][2] == 1) {
                  CE[*N_ContEdges].n1 = SEL->EL[i][0];
                  CE[*N_ContEdges].n2 = SEL->EL[i][1];
                  ++ *N_ContEdges;
                  if (LocalHead) {
                           fprintf (SUMA_STDERR,
                                    "%s: Found edge made up of nodes [%d %d]\n",
                                    FuncName, SEL->EL[i][0], SEL->EL[i][1]);
                  }
               }
               if (SEL->ELps[i][2] > 0) {
                  i += SEL->ELps[i][2];
               } else {
                  i ++;
               }
            }
            break;
         default:
            SUMA_SL_Err("Bad ContourMode");
            SUMA_RETURN(NULL);
            break;            
      }
      
      /* Now reallocate */
      if (! *N_ContEdges) {
         SUMA_free(CE); CE = NULL;
         SUMA_RETURN(CE);
      }else {
         CE = (SUMA_CONTOUR_EDGES *) 
                  SUMA_realloc (CE, *N_ContEdges * sizeof(SUMA_CONTOUR_EDGES));
         if (!CE) {
            SUMA_SLP_Crit("Failed to reallocate for CE");
            SUMA_RETURN(CE);
         }
      }
         
      SUMA_free_Edge_List (SEL); SEL = NULL;
   }
   
   if (!UseThisPatch) {
      SUMA_freePatch (Patch); 
   }
   Patch = NULL;
   
   SUMA_free(isNode); isNode = NULL;
   
   SUMA_RETURN(CE);
}

/*!
   \brief Stitch together two isotopic patches to calculate the volume between them
   
   \param SO1 : The first surface of the pattie
   \param SO2 : The second surface of the pattie
   \param Nodes (int *): N_Node x 1 vector of indices containing nodes that form the patch
   \param N_Node (int): Number of nodes in SO. 
   \param UseThisSo : If you send a pointer to an empty (but allocated) surface structure,
                      The pattie's surface is returned in UseThisSo. Otherwise, the temporary
                      surface is tossed in the trash can.
   \pram minPatchHits (int): Since you're forming a patch from nodes, you'll need to select the
                             minimum number of nodes to be in a patch (triangle) before the patch 
                             is selected. Minimum is 1, Maximum logical is 3. 
                             If you choose 1, you will have nodes in the patch that are not included
                             in Nodes vector. But that is the only way to get something back for just
                             one node. 
                             If you choose 3, you will have the same number of nodes in the patch as you
                             do in the vector Nodes. However, you'll get no patches formed if your Nodes
                             vector contains, one or two nodes for example ...
   
   Testing so far in /home/ziad/SUMA_test/afni:
   for a bunch of nodes:
      SurfMeasures -func node_vol -spec ../SurfData/SUMA/DemoSubj_lh.spec -surf_A lh.smoothwm.asc -surf_B lh.pial.asc -nodes_1D lhpatch.1D.roi'[0]' -out_1D SM_out.1D
      SurfPatch -spec ../SurfData/SUMA/DemoSubj_lh.spec -surf_A lh.smoothwm -surf_B lh.pial.asc -hits 3 -input lhpatch.1D.roi 0 1
      answer is 326, 13% different from sum in SM_out.1D's second column (373)... 
      
   for a single node (remember change -hits option to 1):
      SurfMeasures -func node_vol -spec ../SurfData/SUMA/DemoSubj_lh.spec -surf_A lh.smoothwm.asc -surf_B lh.pial.asc -nodes_1D lhpatch_1node.1D'[0]' -out_1D SM_out_1node.1D
      SurfPatch -spec ../SurfData/SUMA/DemoSubj_lh.spec -surf_A lh.smoothwm -surf_B lh.pial.asc -hits 1 -input lhpatch_1node.1D 0 1
      (divide answer of 2.219910 by 3 (0.73997), difference at 3rd signifcant digit from SM_out_1node.1D of 0.731866)
   
   for a box:
      SurfMeasures -func node_vol -spec RectPly.spec -surf_A RectSurf.ply -surf_B RectSurf2.ply -nodes_1D RectAllPatch.1D'[1]' -out_1D SM_out_Rect.1D
      SurfPatch -spec RectPly.spec -surf_A RectSurf.ply -surf_B RectSurf2.ply -hits 3 -input RectAllPatch.1D 0 1 -vol_only
        
*/
double SUMA_Pattie_Volume (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2, int *Nodes, int N_Node, SUMA_SurfaceObject *UseThisSO, int minPatchHits)
{
   static char FuncName[]={"SUMA_Pattie_Volume"};
   double Vol = 0.0;
   int N_ContEdges=0, i,  i3, n, NodesPerPatch, *NewIndex = NULL, inew3, cnt, n1, n2, trouble;
   SUMA_PATCH *P1 = NULL;
   FILE *fid=NULL;
   SUMA_CONTOUR_EDGES *CE = NULL;
   SUMA_SurfaceObject *SOc = NULL;
   SUMA_SURF_NORM SN;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO1 || !SO2 || !Nodes || !N_Node) {
      SUMA_SL_Err("Bad input.");
      SUMA_RETURN(Vol);
   }
   if (SO1->N_Node != SO2->N_Node || SO1->N_FaceSet != SO2->N_FaceSet) {
      SUMA_SL_Err("Surfaces Not Isotopic");
      SUMA_RETURN(Vol);
   }
   
   /* form the patch */
   SUMA_LH("Forming patch...");
   P1 = SUMA_getPatch (Nodes, N_Node, SO1->FaceSetList, SO1->N_FaceSet, SO1->MF, minPatchHits);
   if (!P1) {
      SUMA_SL_Err("Failed to create patches.\n");
      SUMA_RETURN(Vol);
   }
   if (!P1->N_FaceSet) {
      SUMA_SL_Err("No patch could be formed");
      SUMA_RETURN(Vol);
   }
   /* form the contour */
   SUMA_LH("Forming contour...");
   CE = SUMA_GetContour (SO1, Nodes, N_Node, &N_ContEdges, 1, P1);
   if (!N_ContEdges) {
      SUMA_SL_Err("No contour edges found.\n"
                  "It looks like patches form\n"
                  "closed surfaces.\n");
      SUMA_RETURN(Vol);
   }
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\n Found %d contour segments.\n", FuncName, N_ContEdges);
   }
   
   /* create a mapping from old numbering scheme to new one */
   SUMA_LH("Creating Mapping Index...");
   NewIndex = (int *)SUMA_malloc(SO1->N_Node * sizeof(int));
   if (!NewIndex) {
      SUMA_SL_Crit("Failed to allocate for NewIndex");
      SUMA_RETURN(Vol);
   }
   SUMA_INIT_VEC(NewIndex, SO1->N_Node, -1, int);
   NodesPerPatch = 0;
   for (i=0; i < P1->N_FaceSet; ++i) {
      i3 = 3*i;
      n = P1->FaceSetList[i3];   if (NewIndex[n] < 0) { NewIndex[n] = NodesPerPatch; ++NodesPerPatch; }   
      n = P1->FaceSetList[i3+1]; if (NewIndex[n] < 0) { NewIndex[n] = NodesPerPatch; ++NodesPerPatch; }   
      n = P1->FaceSetList[i3+2]; if (NewIndex[n] < 0) { NewIndex[n] = NodesPerPatch; ++NodesPerPatch; }
   }
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\n"
                  "Number of nodes in patch (%d), in N_Node (%d)\n"
                  , FuncName, NodesPerPatch, N_Node);
   }
   if (NodesPerPatch != N_Node) {
      fprintf(SUMA_STDERR, "Note:\n"
                           "Have %d nodes in patch, %d nodes in input.\n", NodesPerPatch, N_Node);
   }
   
   /* Building composite surface */
   SUMA_LH("Building composite surface...");
   if (UseThisSO) { 
      SOc = UseThisSO;
      if (SOc->NodeList || SOc->FaceSetList) {
         SUMA_SL_Err("You want me to use a filled SurfaceObject structure!\n"
                     "How rude!");
         SUMA_RETURN(Vol);
      }
   } else {
      SOc = SUMA_Alloc_SurfObject_Struct(1);
   }
   SOc->N_Node = NodesPerPatch*2;
   SOc->N_FaceSet = P1->N_FaceSet*2+2*N_ContEdges;
   SOc->NodeDim = 3;
   SOc->FaceSetDim = 3;
   SOc->NodeList = (float *)SUMA_malloc(SOc->NodeDim*SOc->N_Node*sizeof(float));
   SOc->FaceSetList = (int *)SUMA_malloc(SOc->FaceSetDim*SOc->N_FaceSet*sizeof(int));
   /* first create the NodeList from S01 && SO2*/
   for (i=0; i<SO1->N_Node; ++i) {
      if (NewIndex[i] >=0) { /* this node is used */
         i3 = 3*i;
         inew3 = 3 * NewIndex[i];
         SOc->NodeList[inew3  ] = SO1->NodeList[i3  ];
         SOc->NodeList[inew3+1] = SO1->NodeList[i3+2];
         SOc->NodeList[inew3+2] = SO1->NodeList[i3+1];
         inew3 = 3 * (NewIndex[i]+NodesPerPatch);
         SOc->NodeList[inew3  ] = SO2->NodeList[i3  ];
         SOc->NodeList[inew3+1] = SO2->NodeList[i3+2];
         SOc->NodeList[inew3+2] = SO2->NodeList[i3+1];
      }
   }
   /* Now add the pre-existing patches */
   cnt = 0;
   for (i=0; i<P1->N_FaceSet; ++i) {
      i3 = 3*i;
      n = P1->FaceSetList[i3  ]; SOc->FaceSetList[cnt] = NewIndex[n]; ++cnt;
      n = P1->FaceSetList[i3+1]; SOc->FaceSetList[cnt] = NewIndex[n]; ++cnt;
      n = P1->FaceSetList[i3+2]; SOc->FaceSetList[cnt] = NewIndex[n]; ++cnt;                     
   }
   for (i=0; i<P1->N_FaceSet; ++i) { /* Now for SO2's */
      i3 = 3*i;
      n = P1->FaceSetList[i3  ]; SOc->FaceSetList[cnt] = NewIndex[n]+NodesPerPatch; ++cnt;
      n = P1->FaceSetList[i3+1]; SOc->FaceSetList[cnt] = NewIndex[n]+NodesPerPatch; ++cnt;
      n = P1->FaceSetList[i3+2]; SOc->FaceSetList[cnt] = NewIndex[n]+NodesPerPatch; ++cnt;                     
   }
   
   /* Now you need to add the stitches, for each segment you'll need 2 triangles*/
   for (i=0; i<N_ContEdges; ++i) {
      n1 = NewIndex[CE[i].n1]; n2 = NewIndex[CE[i].n2];
      SOc->FaceSetList[cnt] = n1; ++cnt;
      SOc->FaceSetList[cnt] = n2; ++cnt;
      SOc->FaceSetList[cnt] = n2+NodesPerPatch; ++cnt;
      SOc->FaceSetList[cnt] = n1; ++cnt;
      SOc->FaceSetList[cnt] = n2+NodesPerPatch; ++cnt;
      SOc->FaceSetList[cnt] = n1+NodesPerPatch; ++cnt;
   }
   
   /* calculate EdgeList */
   if (!SUMA_SurfaceMetrics_eng(SOc, "EdgeList", NULL, 0, SUMAg_CF->DsetList)){
      SUMA_SL_Err("Failed to create EdgeList");
      SUMA_RETURN(Vol);
   }
   
   /* make sure that's a closed surface */
   if (SOc->EL->max_N_Hosts != 2 || SOc->EL->min_N_Hosts != 2) {
      SUMA_SL_Err("Created surface is not a closed one.\n"
                  "Or patches have tessellation problems.");
      SUMA_RETURN(Vol);
   }
   
   /* fix the winding */
   if (!SUMA_MakeConsistent(SOc->FaceSetList, SOc->N_FaceSet, SOc->EL, 0, &trouble)) {
      SUMA_SL_Err("Failed to make surface consistent");
      SUMA_RETURN(Vol);
   }
   
   /* Now calculate FaceSetNormals and triangle areas*/
   SN = SUMA_SurfNorm(SOc->NodeList,  SOc->N_Node, SOc->FaceSetList, SOc->N_FaceSet );
   SOc->NodeNormList = SN.NodeNormList;
   SOc->FaceNormList = SN.FaceNormList;

   if (!SUMA_SurfaceMetrics_eng(SOc, "PolyArea", NULL, 0, SUMAg_CF->DsetList)){
      SUMA_SL_Err("Failed to create EdgeList");
      SUMA_RETURN(Vol);
   }
      
   /* debug */
   if (LocalHead) {
      fid = fopen("Junk_NodeList.1D", "w");
      SUMA_disp_vecmat (SOc->NodeList, SOc->N_Node, SOc->NodeDim, 1, SUMA_ROW_MAJOR, fid, NOPE);
      fclose(fid);
      fid = fopen("Junk_FaceSetList.1D", "w");
      SUMA_disp_vecdmat(SOc->FaceSetList, SOc->N_FaceSet, SOc->FaceSetDim, 1, SUMA_ROW_MAJOR, fid , NOPE);
      fclose(fid);
   }
   
   /* calculate the volume */
   SUMA_LH("Calculating volume");
   Vol = SUMA_Mesh_Volume(SOc, NULL, -1);
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s:\n"
                           "Volume = %f\n", FuncName, Vol);
   }
   
   /* cleanup */
   SUMA_LH("Cleanup");
   if (P1) SUMA_freePatch(P1); P1 = NULL;
   if (NewIndex) SUMA_free(NewIndex); NewIndex = NULL;
   if (SOc != UseThisSO) SUMA_Free_Surface_Object(SOc); SOc = NULL;
   if (CE) SUMA_free(CE); CE=NULL;
   
   SUMA_RETURN(Vol);
}

/*!
   \brief Calculate the volume of a mesh per the
   method in Hughes, S.W. et al. Phys. Med. Biol. 1996
   
   Tested with Icosahedron surface (see direct vol computation in CreateIcosahedron)
   
   Tested with tetrahedron below, which has a volume of 0.166667
   Also, the same volume was obtained with a rotated version of the same tetrahedron
TetraFaceSetList.1D
0 1 2
0 3 1
0 2 3
2 1 3

TetraNodeList.1D
0 0 0
1 0 0
0 1 0
1 1 1
which should have a volume of (1/3)A*h of 1/3 * 0.5 * 1
   
   Volume of smoothwm surface did not change if surface was rotated and shifted, 
   a very good thing.

If you try to calculate the volume of a boxe's surface with the box's axes 
in alignment with the X, Y and Z directions, you will get nan for an answer
because the sum of the weights is ~= 0 . If you rotate the surface, the problem
will be solved. 

   - It is extremely important that the surface mesh be consistently defined. 
   - Detecting consistency must be done ahead of time (need program to do that) 
   because doing it here will slow the computations a lot and consistency fix 
   requires recalculating the normals and all that depends on them (quite a bit).
    
*/

double SUMA_Mesh_Volume(SUMA_SurfaceObject *SO, int *FSI, int N_FaceSet) 
{
   static char FuncName[]={"SUMA_Mesh_Volume"};
   double Vol = 0.0, c[3], anx, any, anz, sx, sy, sz, kx, ky, kz, kt;
   float *pa = NULL;
   int i, fc;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN(Vol);  }
   if (!SO->FaceNormList) { SUMA_SL_Err("NULL SO->FaceNormList"); SUMA_RETURN(Vol);  }
   if (!SO->PolyArea) { 
      if (!SUMA_SurfaceMetrics_eng (SO, "PolyArea", NULL, 0, SUMAg_CF->DsetList)) {
         SUMA_SL_Err("Failed to compute SO->PolyArea"); SUMA_RETURN(Vol);  
      }
   }
   pa = SO->PolyArea;
   
   if (FSI || N_FaceSet != -1) {
      SUMA_SL_Err("FSI and N_FaceSet are two stupid options never to be used.\nUse NULL and -1, respectively.");
      SUMA_RETURN(Vol);
   }
   
   if (!FSI) { 
      N_FaceSet = SO->N_FaceSet;    
   }

   
   /* calculate vector of areas * normals */
   kx = ky = kz = sx = sy = sz = 0.0;
   for (i=0; i<N_FaceSet; ++i) {
      if (FSI) fc = FSI[i];
      else fc = i;
      SUMA_FACE_CENTROID(SO, fc, c);
      #if 0 
         if (LocalHead) fprintf(SUMA_STDERR,"Area: %f , normal (%f, %f, %f)\n", 
               pa[fc], SO->FaceNormList[3*fc], SO->FaceNormList[3*fc+1], SO->FaceNormList[3*fc+2]);
      #endif
      anx = pa[fc] * SO->FaceNormList[3*fc];   kx += anx;  sx += c[0] * anx;
      any = pa[fc] * SO->FaceNormList[3*fc+1]; ky += any;  sy += c[1] * any;
      anz = pa[fc] * SO->FaceNormList[3*fc+2]; kz += anz;  sz += c[2] * anz;
   }
   kt = (kx+ky+kz); /* need to normalize k so that sum is 1, kx, ky and kz are supposed to 
                  "weight the volume according to its orientation relative to the axes."
                  For a sphere, you can use 1/3 for kx, ky and kz...   */
   if (fabs(kt) < 1e-15) { 
      SUMA_SL_Warn("Weight constants sum to ~= 0.\n"
                   "Volume measurements may be off.\n"
                   "If your surface's axes are along\n"
                   "the X, Y and Z directions, as you \n"
                   "could have with a box's surface, rotating\n"
                   "the surface will solve the problem.");
      fprintf(SUMA_STDERR, "%s:\n"
                           "kx + ky + kz = kt\n"
                           "%f + %f + %f = %f\n"
                           "sx, sy, sz = %f, %f, %f\n",
                           FuncName, kx, ky, kz, kx+ky+kz, sx, sy, sz);   }
   kx /= kt;
   ky /= kt;
   kz /= kt;
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "%f + %f + %f = %f\n"
                           "sx, sy, sz = %f, %f, %f\n",
                           FuncName, kx, ky, kz, kx+ky+kz, sx, sy, sz);
   }
   Vol = kx * sx + ky *sy + kz * sz;
   
   SUMA_RETURN(Vol);
}

/*!
   \brief computes the total area of a surface.
   NOTE: This function will replace whatever values you have in SO->PolyArea.
   If SO->PolyArea is NULL, it will remain that way.
*/
double SUMA_Mesh_Area(SUMA_SurfaceObject *SO, int *FaceSets, int N_FaceSet) 
{
   static char FuncName[]={"SUMA_Mesh_Area"};
   double A = 0.0, a = 0.0;
   int i, i3;
   float *n0, *n1, *n2;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN(A);  }
   if (!SO->FaceSetList) { SUMA_SL_Err("NULL SO->FaceSetList"); SUMA_RETURN(A);  }
      
   if (!FaceSets ) { 
      if (N_FaceSet != -1) {
         SUMA_SL_Err("With NULL FaceSets, use -1 for N_FaceSet");
         SUMA_RETURN(A);
      }
      N_FaceSet = SO->N_FaceSet; 
      FaceSets = SO->FaceSetList;    
   }else {
      if (N_FaceSet < 0) {
         SUMA_SL_Err("N_FaceSet < 0");
         SUMA_RETURN(A);
      }
   }

   A = 0.0;
   if (SO->PolyArea) {
      for (i=0;  i<N_FaceSet; ++i) {
         i3 = 3*i;
         n0 = &(SO->NodeList[3*FaceSets[i3]]);
         n1 = &(SO->NodeList[3*FaceSets[i3+1]]);
         n2 = &(SO->NodeList[3*FaceSets[i3+2]]);
         SUMA_TRI_AREA( n0, n1, n2, a);
         SO->PolyArea[i] = (float)a;
         A += a;
      }
   } else {
      for (i=0;  i<N_FaceSet; ++i) {
         i3 = 3*i;
         n0 = &(SO->NodeList[3*FaceSets[i3]]);
         n1 = &(SO->NodeList[3*FaceSets[i3+1]]);
         n2 = &(SO->NodeList[3*FaceSets[i3+2]]);
         SUMA_TRI_AREA( n0, n1, n2, a);
         A += a;
      }
   }
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\n   A = %f\n", FuncName, A);
   }
   SUMA_RETURN(A);
}
/*!
 
From File : Plane_Equation.c
Author : Ziad Saad
Date : Thu Nov 19 14:55:54 CST 1998
 
\brief   finds the plane passing through 3 points
 
 
Usage : 
      Eq = SUMA_Plane_Equation (P1, P2, P3, thisEq)
 
 
   \param P1 (float *) 1x3 vector Coordinates of Point1
   \param P2 (float *) 1x3 vector Coordinates of Point2
   \param P3 (float *) 1x3 vector Coordinates of Point3
   \param thisEq (float *) use this pointer to store Eq rather than create a new one (set to NULL if you want a new one)
   \return Eq (float *) 1x4 vector containing the equation of the plane containing the three points.
         The equation of the plane is : 
         Eq[0] X + Eq[1] Y + Eq[2] Z + Eq[3] = 0
   
         If the three points are colinear, Eq = [0 0 0 0]
 
   \sa SUMA_PLANE_NORMAL_POINT
*/
float * SUMA_Plane_Equation (float * P1, float *P2, float *P3, float *usethisEq)
{/*SUMA_Plane_Equation*/
   float *Eq;
   static char FuncName[] = {"SUMA_Plane_Equation"}; 
    
   SUMA_ENTRY;
   if (usethisEq) Eq = usethisEq;
   else Eq = (float *) SUMA_calloc(4,sizeof(float));
   if (!Eq)
      {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
         SUMA_RETURN (NULL);
      }
   
   Eq[0] = P1[1] * (P2[2]-P3[2]) 
         + P2[1] * (P3[2]-P1[2]) 
         + P3[1] * (P1[2]-P2[2]);
         
   Eq[1] = P1[2] * (P2[0]-P3[0]) 
         + P2[2] * (P3[0]-P1[0]) 
         + P3[2] * (P1[0]-P2[0]);
         
   Eq[2] = P1[0] * (P2[1]-P3[1]) 
         + P2[0] * (P3[1]-P1[1]) 
         + P3[0] * (P1[1]-P2[1]);
         
   Eq[3] =  - P1[0] * (P2[1] * P3[2] - P3[1] * P2[2]) 
            - P2[0] * (P3[1] * P1[2] - P1[1] * P3[2]) 
            - P3[0] * (P1[1] * P2[2] - P2[1] * P1[2]);

   SUMA_RETURN (Eq);
}/*SUMA_Plane_Equation*/


/*! 
 
\brief Determines the intersection of a plane and a surface 

 
   
   SPI = SUMA_Surf_Plane_Intersect (SO, PlaneEq)

\param SO (SUMA_SurfaceObject *) Pointer to surface object structure.
\param  PlaneEq (float *) : 4x1 vector containing the 4 coefficients of the equation 
                     of the plane to intersect the surface
                     PlaneEq[0] X + PlaneEq[1] Y + PlaneEq[2] Z + PlaneEq[3] = 0
\return SPI (SUMA_SURF_PLANE_INTERSECT *) Pointer to intersection structure. See help on fields in SUMA_define.h
                                             NULL in case of error.
     
 

\sa SUMA_SEGMENT_PLANE_INTERSECT
\sa an older matlab version in Surf_Plane_Intersect_2.m  
\sa SUMA_PlaneEq
\sa SUMA_Allocate_SPI
\sa SUMA_free_SPI
    
   
*/
 
SUMA_SURF_PLANE_INTERSECT *SUMA_Surf_Plane_Intersect (SUMA_SurfaceObject *SO, float *PlaneEq)
{/*SUMA_Surf_Plane_Intersect*/
   static char FuncName[]={"SUMA_Surf_Plane_Intersect"};
   int  i, k , k3, i3, n1, n2;
   float DT_ABVBEL, DT_POSNEG, u;
   float *NodePos;
   SUMA_SURF_PLANE_INTERSECT *SPI;
   struct  timeval start_time, start_time2;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_Boolean  Hit;
   
   SUMA_ENTRY;
   
   /* Start timer for next function */
   SUMA_etime(&start_time2,0);
      
   if (LocalHead)   fprintf(SUMA_STDERR, "%s : Determining intersecting segments ...\n", FuncName);
   
   /* allocate for the return structure.
   NOTE: If (in a different form of this function) you do not allocate for SPI each time you call the function, make sure you reset all 
   elements of the following vector fields: 
   IsTriHit[] = NOPE;
   TriBranch[] = 0
   */
   SPI = SUMA_Allocate_SPI (SO);
   if (!SPI) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Allocate_SPI\n", FuncName);
      SUMA_RETURN (SPI);
   }
   
   /* allocate for temporary stuff */
   NodePos = (float *) SUMA_calloc (SO->N_Node , sizeof(float));

   if (!NodePos )
      {
         fprintf (SUMA_STDERR, "Error %s: Could not allocate in SUMA_Surf_Plane_Intersect\n", FuncName);
         SUMA_free_SPI (SPI); SPI = NULL;
         SUMA_RETURN (SPI);
      }

      
   /* Start timer for next function */
   SUMA_etime(&start_time,0);
                           
   /* Find out which nodes are above and which are below the plane */
   for (k=0;k<SO->N_Node; ++k)
      {
         k3 = 3*k;
         NodePos[k] = PlaneEq[0] * SO->NodeList[k3] + PlaneEq[1] * SO->NodeList[k3+1] 
                     +PlaneEq[2] * SO->NodeList[k3+2] + PlaneEq[3] ;
      }
      
   /* stop timer */
   DT_ABVBEL = SUMA_etime(&start_time,1);
                              
   
   /*
      NodePos is < 0 for nodes below the plane and > 0 for points above the plane
      Go through each connection and determine if it intersects the plane
      If a segment intersects the surface, it means that the sign
      of p  would be <= 0 (each point is on a different side of the plane)
   */
   
   /* Start timer for next function */
   SUMA_etime(&start_time,0);
   
   /*
      Determine the segments intersecting the surface,
      The triangles that contain these segments.
      The nodes that form the intersected segments 
   */
   SPI->N_IntersEdges = 0;
   SPI->N_IntersTri = 0;
   SPI->N_NodesInMesh = 0;
   k=0; 
   Hit = NOPE;
   while (k < SO->EL->N_EL)
      {
         /* find out if segment intersects */
         /* if (SUMA_IS_NEG(NodePos[SO->EL->EL[k][0]] * NodePos[SO->EL->EL[k][1]])) { *//* can speed this check by explicitly checking for sign difference: 
                                                                                    if (SUMA_SIGN(a) != SUMA_SIGN(b)) ... */   
         if (SUMA_SIGN(NodePos[SO->EL->EL[k][0]]) != SUMA_SIGN(NodePos[SO->EL->EL[k][1]]) ) {
            Hit = YUP;
            /* find the intersection point in that segment */
            u = -NodePos[SO->EL->EL[k][0]] / (NodePos[SO->EL->EL[k][1]] - NodePos[SO->EL->EL[k][0]]);
            i3 = 3 * k;
            n1 = 3 * SO->EL->EL[k][0];
            n2 = 3 * SO->EL->EL[k][1];
            
            SPI->IntersNodes[i3] = SO->NodeList[n1] + u * ( SO->NodeList[n2] - SO->NodeList[n1] ); ++i3; ++n2; ++n1;
            SPI->IntersNodes[i3] = SO->NodeList[n1] + u * ( SO->NodeList[n2] - SO->NodeList[n1] ); ++i3; ++n2; ++n1;
            SPI->IntersNodes[i3] = SO->NodeList[n1] + u * ( SO->NodeList[n2] - SO->NodeList[n1] ); ++i3; ++n2; ++n1;
            
            
            /* fprintf (SUMA_STDERR,"%s: Edge %d (nodes %d %d), IntersNodes[%d]= [%f, %f, %f]\n", 
               FuncName, k, SO->EL->EL[k][0], SO->EL->EL[k][1], 3*k, SPI->IntersNodes[3*k], 
               SPI->IntersNodes[3*k+1], SPI->IntersNodes[3*k+2]);
            */
               
            /* Store the intersected segment */
            SPI->IntersEdges[SPI->N_IntersEdges] = k;
            ++SPI->N_IntersEdges;
            
            /* mark this segment in the boolean vector to speed up some other functions */
            SPI->isEdgeInters[k] = YUP;
                  
            /* Store the index of the triangle hosting this edge*/
            if (!SPI->isTriHit[SO->EL->ELps[k][1]]) {
               SPI->IntersTri[SPI->N_IntersTri] = SO->EL->ELps[k][1];
               ++(SPI->N_IntersTri);
               SPI->isTriHit[SO->EL->ELps[k][1]] = YUP;
            }
            
            /* mark the nodes forming the intersection edges */
            if (!SPI->isNodeInMesh[SO->EL->EL[k][0]]) {
               SPI->isNodeInMesh[SO->EL->EL[k][0]] = YUP;
               ++(SPI->N_NodesInMesh);
            }
            if (!SPI->isNodeInMesh[SO->EL->EL[k][1]]) {
               SPI->isNodeInMesh[SO->EL->EL[k][1]] = YUP;
               ++(SPI->N_NodesInMesh);
            } 
         } else {
            Hit = NOPE;
         }
            
            /*  skip ahead of duplicate edge listings */
            if (SO->EL->ELps[k][2] > 0) {
               if (Hit) { /* you must mark these triangles */
                  i3 = 3 * k;
                  for (i=1; i < SO->EL->ELps[k][2]; ++i) {
                     SPI->isEdgeInters[k+i] = YUP;
                     n1 = 3 * (k+i);
                     SPI->IntersNodes[n1] = SPI->IntersNodes[i3]; ++i3; ++n1;
                     SPI->IntersNodes[n1] = SPI->IntersNodes[i3]; ++i3; ++n1;
                     SPI->IntersNodes[n1] = SPI->IntersNodes[i3]; ++i3; ++n1;
                     /*
                     fprintf (SUMA_STDERR,"%s: Edge %d, IntersNodes[%d]= [%f, %f, %f]\n", 
                        FuncName, k+i, n1, SPI->IntersNodes[3*(k+i)], SPI->IntersNodes[3*(k+i)+1], SPI->IntersNodes[3*(k+i)+2]);
                     */
                     if (!SPI->isTriHit[SO->EL->ELps[k+i][1]]) {
                        SPI->IntersTri[SPI->N_IntersTri] = SO->EL->ELps[k+i][1];
                        ++(SPI->N_IntersTri);
                        SPI->isTriHit[SO->EL->ELps[k+i][1]] = YUP;   
                     }
                  }
               }
               k += SO->EL->ELps[k][2];
            } else ++k;
      }
            
      
   /* stop timer */
   DT_POSNEG = SUMA_etime(&start_time,1);
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Found %d intersect segments, %d intersected triangles, %d nodes in mesh (exec time %f + %f = %f secs).\n", 
      FuncName, SPI->N_IntersEdges, SPI->N_IntersTri, SPI->N_NodesInMesh, DT_ABVBEL, DT_POSNEG, DT_ABVBEL + DT_POSNEG);
   
/* free locally allocated memory */
if (NodePos) SUMA_free (NodePos);
   

SUMA_RETURN (SPI);
}/*SUMA_Surf_Plane_Intersect*/

/*!
   A function to intersect a surface along a set of parallel planes.
   The set of planes is defined by the equation of one of them and by 
   step, the spacing between them. 
   If step is < 0, step is set to be the average segment length 
*/
DList *SUMA_SliceAlongPlane(SUMA_SurfaceObject *SO, float *Eq, float step)
{
   static char FuncName[]={"SUMA_SliceAlongPlane"};
   DList *striplist=NULL, *stripnext=NULL;
   int i=-1, imin, imax; 
   float d, dmin, dmax, nrm;
   SUMA_SURF_PLANE_INTERSECT *SPIl = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SO || !SO->EL || !Eq) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(striplist);
   }
   if (step < 0) step = SO->EL->AvgLe;
   
   /* calculate elevations from plane */
   dmin = 1000000.0; dmax = -100000000.0; imin = imax = -1;
   for (i=0; i<SO->N_Node; ++i) { 
      d = Eq[0]*SO->NodeList[3*i] + Eq[1]*SO->NodeList[3*i+1] + Eq[2]*SO->NodeList[3*i+2] + Eq[3];
      if (d > dmax) { dmax = d; imax = i; }
      else if (d < dmin) { dmin = d; imin = i; }
   }
   nrm = sqrt(Eq[0]*Eq[0]+Eq[1]*Eq[1]+Eq[2]*Eq[2]);
   SUMA_LHv("Farthest nodes are %d at %f and %d at %f\n",imax,  dmax/nrm, imin, dmin/nrm);
   d = Eq[3];
   Eq[3] = Eq[3]-dmax;
   do {
      SUMA_LHv("Eq now [%f %f %f %f]\n", Eq[0], Eq[1], Eq[2], Eq[3]);
      SPIl = SUMA_Surf_Plane_Intersect (SO, Eq);
      if (!striplist) {
         striplist = SUMA_SPI_to_EdgeStrips(SO, SPIl);
      } else {
         stripnext = SUMA_SPI_to_EdgeStrips(SO, SPIl);
         if (stripnext) {
            SUMA_MergeLists_Beg2_End1(stripnext,striplist);
            SUMA_FREE_DLIST(stripnext);/* get rid of stripnext */
         }
      }
      if (SPIl) SUMA_free_SPI (SPIl);
      Eq[3] = Eq[3]+step*nrm;
   } while (Eq[3]<= d - dmin);

   SUMA_RETURN(striplist);
}

/*!
   \brief a wrapper function for SUMA_Surf_Plane_Intersect that returns the intersection 
   in the form of an ROI datum
   
   \param SO (SUMA_SurfaceObject *)
   \param Nfrom (int) index of node index on SO from which the path should begin
   \param Nto (int) index of node index on SO where the path will end
   \param P (float *) XYZ of third point that will form the cutting plane with Nfrom and Nto's coordinates
            This point is usually, the Near Plane clipping point of Nto's picking line.
   \return ROId (SUMA_ROI_DATUM *) pointer to ROI datum structure which contains the NodePath from Nfrom to Nto
   along with other goodies.      
*/
SUMA_ROI_DATUM *SUMA_Surf_Plane_Intersect_ROI (SUMA_SurfaceObject *SO, int Nfrom, int Nto, float *P)
{
   static char FuncName[]={"SUMA_Surf_Plane_Intersect_ROI"};
   SUMA_ROI_DATUM *ROId=NULL;
   SUMA_Boolean LocalHead = NOPE;
   int N_left;
   SUMA_SURF_PLANE_INTERSECT *SPI = NULL;
   SUMA_ROI *ROIe = NULL, *ROIt = NULL, *ROIn = NULL, *ROIts = NULL;
   float *Eq = NULL;
   /* The 3 flags below are for debugging. */
   SUMA_Boolean DrawIntersEdges=NOPE; /* Draw edges intersected by plane   */
   SUMA_Boolean DrawIntersTri = NOPE; /* Draw triangles intersected by plane */
   SUMA_Boolean DrawIntersNodeStrip = NOPE; /* Draw intersection node strip which is the shortest path between beginning and ending nodes */ 
   SUMA_Boolean DrawIntersTriStrip=NOPE; /* Draw intersection triangle strip which is the shortest path between beginning and ending nodes */
   
   SUMA_ENTRY;
   
   /* computing plane's equation */
   Eq = SUMA_Plane_Equation ( &(SO->NodeList[3*Nfrom]), 
                              P,
                              &(SO->NodeList[3*Nto]) , NULL);
   
   if (!Eq) {
      fprintf(SUMA_STDOUT,"Error %s: Failed in SUMA_Plane_Equation.\n", FuncName);
      SUMA_RETURN(ROId);
   } 
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Computing Intersection with Surface.\n", FuncName);
   SPI = SUMA_Surf_Plane_Intersect (SO, Eq);
   if (!SPI) {
      fprintf(SUMA_STDOUT,"Error %s: Failed in SUMA_Surf_Plane_Intersect.\n", FuncName);
      SUMA_RETURN(ROId);
   }
   
   #if 0 /* some sample code to play with surface slicing interactively. 
            This breaks the ROI drawing cycle. Use only for debugging. */
   {
      DList *striplist=NULL;
      SUMA_S_Warn("Kill me!");
      striplist = SUMA_SliceAlongPlane(SO, Eq, SO->EL->AvgLe);
      SUMA_display_edge_striplist(striplist, &(SUMAg_SVv[0]), SO, "ShowEdges, ShowConnectedPoints, ShowPoints");
      SUMA_FREE_DLIST(striplist);
      SUMA_RETURN(ROId);
   }
   #endif
   
   if (Eq) SUMA_free(Eq); Eq = NULL; /* not needed further */
   
   if (DrawIntersEdges) {
      /* Show all intersected edges */
      ROIe =  SUMA_AllocateROI ( SO->idcode_str,      SUMA_ROI_EdgeGroup, 
                                 "SurfPlane Intersection - Edges", 
                                 SPI->N_IntersEdges,  SPI->IntersEdges);
      if (!ROIe) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateROI.\n", FuncName);
      } else {
         if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIe, ROIO_type, SUMA_WORLD)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
         }
      }
   }
   
   if (DrawIntersTri) {
      /* Show all intersected triangles */
      ROIt =  SUMA_AllocateROI (SO->idcode_str, SUMA_ROI_FaceGroup, "SurfPlane Intersection - Triangles", SPI->N_IntersTri, SPI->IntersTri);
      if (!ROIt) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateROI.\n", FuncName);
      } else {
         if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIt, ROIO_type, SUMA_WORLD)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
         }
      }
   }

   /* create ROId */
   ROId = SUMA_AllocROIDatum ();
   ROId->Type = SUMA_ROI_NodeSegment;

   /* calculate shortest path */
   N_left = SPI->N_NodesInMesh;
   ROId->nPath = SUMA_Dijkstra (SO, Nfrom, Nto, SPI->isNodeInMesh, &N_left, 1, &(ROId->nDistance), &(ROId->N_n));
   if (ROId->nDistance < 0 || !ROId->nPath) {
      fprintf(SUMA_STDERR,"\aError %s: Failed in fast SUMA_Dijkstra.\n*** Two points are not connected by intersection. Repeat last selection.\n", FuncName);

      /* clean up */
      if (SPI) SUMA_free_SPI (SPI); 
      SPI = NULL;
      if (ROId) SUMA_FreeROIDatum (ROId);
      SUMA_RETURN(NULL);   
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Shortest inter nodal distance along edges between nodes %d <--> %d (%d nodes) is %f.\n", 
      FuncName, Nfrom, Nto, ROId->N_n, ROId->nDistance);
   

   #if 0
   
      /* FOR FUTURE USAGE:
         When drawing on the surface, it is possible to end up with node paths for which the 
         triangle strip tracing routines fail. That's paretly because it is possible for these
         node paths to visit one node more than once, eg: ... 34 23 34 .... 
         That is OK for drawing purposes but not for say, making measurements on the surface.
      */
      
      /* calculate shortest path along the intersection of the plane with the surface */
      /* get the triangle path corresponding to shortest distance between Nx and Ny */

      /* Old Method: Does not result is a strip of triangle that is continuous or connected
      by an intersected edge. Function is left here for historical reasons. 
         tPath = SUMA_NodePath_to_TriPath_Inters (SO, SPI, Path, N_Path, &N_Tri); */

      /* you should not need to go much larger than NodeDist except when you are going for 
      1 or 2 triangles away where discrete jumps in d might exceed the limit. 
      Ideally, you want this measure to be 1.5 NodeDist or say, 15 mm, whichever is less.... */

      /* THIS SHOULD BE OPTIONAL */
      ROId->tPath = SUMA_IntersectionStrip (SO, SPI, ROId->nPath, ROId->N_n, &(ROId->tDistance), 2.5 *ROId->nDistance, &(ROId->N_t));                      
      if (!ROId->tPath) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_IntersectionStrip. Proceeding\n", FuncName);
         /* do not stop here, it is OK if you can't find the triangle strip */
         /* if (ROId) SUMA_FreeROIDatum (ROId);
         SUMA_RETURN(NULL);   */
      } else {
         /* ROId->tPath has a potentially enourmous chunk of memory allocated for it. Trim the fat. */
         {
            int *tPath_tmp=NULL, i_tmp=0;
            tPath_tmp = (int *)SUMA_calloc (ROId->N_t, sizeof(int));
            if (!tPath_tmp) {
               SUMA_RegisterMessage (SUMAg_CF->MessageList, "Failed to allocate for tpath_tmp", FuncName, SMT_Critical, SMA_LogAndPopup);
               SUMA_RETURN(NULL);
            }
            for (i_tmp=0; i_tmp<ROId->N_t; ++i_tmp) tPath_tmp[i_tmp] = ROId->tPath[i_tmp];
            SUMA_free(ROId->tPath);
            ROId->tPath = tPath_tmp;
         } 

         fprintf (SUMA_STDERR, "%s: Shortest inter nodal distance along surface between nodes %d <--> %d is %f.\nTiangle 1 is %d\n", 
            FuncName, Nfrom, Nto, ROId->tDistance, ROId->tPath[0]);

         if (DrawIntersTriStrip) {
            /* Show intersected triangles, along shortest path */
            ROIts =  SUMA_AllocateROI (SO->idcode_str, SUMA_ROI_FaceGroup, "SurfPlane Intersection - Triangles- Shortest", ROId->N_t, ROId->tPath);
            if (!ROIts) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateROI.\n", FuncName);
               if (ROIn) SUMA_freeROI(ROIn);
               if (ROIts) SUMA_freeROI(ROIts);
               if (ROId) SUMA_FreeROIDatum (ROId);
               SUMA_RETURN(NULL);   
            }
            if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIts, ROIO_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
               if (ROIn) SUMA_freeROI(ROIn);
               if (ROIts) SUMA_freeROI(ROIts);
               if (ROId) SUMA_FreeROIDatum (ROId);
               SUMA_RETURN(NULL);
            }
         }

         if (ROId->nPath && DrawIntersNodeStrip) {
            #if 0
               /* Show me the Path */
               for (ii=0; ii < ROId->N_n; ++ii) fprintf(SUMA_STDERR," %d\t", ROId->nPath[ii]);
            #endif

            /* Show Path */
            ROIn =  SUMA_AllocateROI (SO->idcode_str, SUMA_ROI_NodeGroup, "SurfPlane Intersection - Nodes", ROId->N_n, ROId->nPath);
            if (!ROIn) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateROI.\n", FuncName);
               if (ROIn) SUMA_freeROI(ROIn);
               if (ROIts) SUMA_freeROI(ROIts);
               if (ROId) SUMA_FreeROIDatum (ROId);
               SUMA_RETURN(NULL);
            }
            if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIn, ROIO_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
               if (ROIn) SUMA_freeROI(ROIn);
               if (ROIts) SUMA_freeROI(ROIts);
               if (ROId) SUMA_FreeROIDatum (ROId);
               SUMA_RETURN(NULL);
            }

         }
      }                        
   #endif
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Freeing Eq...\n", FuncName);
   if (Eq) SUMA_free(Eq);

   if (LocalHead) fprintf(SUMA_STDERR,"%s: Freeing SPI...\n", FuncName);
   if (SPI) SUMA_free_SPI (SPI);
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s:Done Freeing...\n", FuncName);      
   
   SUMA_RETURN(ROId);
}

/*!

\brief 
SBv =  SUMA_AssignTriBranch (SO, SPI, Nx, BranchCount, DoCopy)
\param SO (SUMA_SurfaceObject *) Pointer to Surface Object structure 
\param SPI (SUMA_SURF_PLANE_INTERSECT *) Pointer to Surface Plane Intersection structure 
\param Nx (int) Node index to start the first branch at. This parameter is optional. pass -1 if you do not want it set.
\param BranchCount (int *) Pointer to the total number of branches found.
\param DoCopy (SUMA_Boolean) flag indicating whether to preserve (YUP) the values in SPI->IntersEdges and SPI->N_IntersEdges or not (NOPE).
                             If you choose YUP, a copy of SPI->IntersEdges is made and manipulated. 
\return Bv (SUMA_TRI_BRANCH*) Pointer to a vector of *BranchCount branches formed by the intersections. 
      A branch is formed by a series of connected triangles. A Branch can be a loop but that is not determined in this function.
      NULL if trouble is encountered. 
      On some surfaces with cuts you may have a branch split in two which requires welding. 
      No such thing is done here but a warning is printed out.
                  
NOTE: The vector SPI->IntersEdges is modified by this function. 
\sa SUMA_free_STB for freeing Bv
*/

#define SUMA_MAX_BRANCHES 300

/* assign a branch to each triangle intersected */
SUMA_TRI_BRANCH* SUMA_AssignTriBranch (SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI, 
                                       int Nx, int *BranchCount, SUMA_Boolean DoCopy)
{
   static char FuncName[]={"SUMA_AssignTriBranch"};
   int *IntersEdgesCopy = NULL, N_IntersEdgesCopy, i_Branch, E1, kedge, i, 
         N_iBranch[SUMA_MAX_BRANCHES], NBlist[SUMA_MAX_BRANCHES], iBranch = 0, 
         N_Branch, Bcnt, ilist, j, ivisit, *VisitationOrder, TriCheck;
   SUMA_Boolean Local_IntersEdgesCopy = NOPE;
   int *TriBranch = NULL; /*!< Vector of SO->EL->N_EL / 3 elements but with only N_IntersTri meaningful values. If TriBranch[j] = b
                     then triangle j in SO->FaceSet is a member of Branch b. Branch numbering starts at 1 and may not be consecutive. 
                     A branch is a collection of connected triangles and may form a closed loop */
   SUMA_TRI_BRANCH *Bv = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;


   i_Branch = 0;
   
   /* make a copy of SPI->N_IntersEdges since this vector will be modified and might be needed later, 
      might make that optional later and just copy pointers if IntersEdgesCopy is not needed elsewhere.
      IntersEdgesCopy flag is used to decide on freeing IntersEdgesCopy or not.*/

   VisitationOrder = (int *)SUMA_calloc (SO->N_FaceSet, sizeof (int)); /* keeps track of the order in which triangles are visited. This is used for creating branches with the proper sequence */
   TriBranch = (int *)SUMA_calloc (SO->EL->N_EL / 3,  sizeof(int));
   
   if (!VisitationOrder || !TriBranch) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      if (TriBranch) SUMA_free(TriBranch);
      if (VisitationOrder) SUMA_free(VisitationOrder);
      SUMA_RETURN (NULL);   
   }
   
   N_IntersEdgesCopy = SPI->N_IntersEdges;
   if (DoCopy) {
      IntersEdgesCopy = (int *) SUMA_calloc (N_IntersEdgesCopy, sizeof (int));
      Local_IntersEdgesCopy = YUP;
      for (i=0; i < N_IntersEdgesCopy; ++i) {
         IntersEdgesCopy[i] = SPI->IntersEdges[i];
      }
   }else {
      Local_IntersEdgesCopy = NOPE;
      IntersEdgesCopy = SPI->IntersEdges;
   }

   if (!IntersEdgesCopy) {
     fprintf (SUMA_STDERR, "Error %s: Failed to allocate for or receive IntersEdgesCopy.\n", FuncName);
     if (TriBranch) SUMA_free(TriBranch);
     if (VisitationOrder) SUMA_free(VisitationOrder);
     SUMA_RETURN (NULL);
   }

   ivisit = 0;
   while (N_IntersEdgesCopy) {
   
      if (!i_Branch && Nx >= 0) {
         /* start from edge containing Nx, this is only done at the starting point (i_Branch = 0) */
         E1 = -1;
         i=0;
         while (i < N_IntersEdgesCopy && E1 < 0) {
            if ( (SO->EL->EL[IntersEdgesCopy[i]][0] == Nx) || (SO->EL->EL[IntersEdgesCopy[i]][1] == Nx) ) {
               E1 = IntersEdgesCopy[i];
               kedge = i;
            }  
            ++i;
         }
      }else {
         /* no starting orders, start from any decent edge */
         /* find an edge with one hosting triangle */
         E1 = SUMA_Find_Edge_Nhost (SO->EL, IntersEdgesCopy, N_IntersEdgesCopy, &kedge, 1);
      }      

      if (E1 < 0) { /* no such edge found, take first edge in InInter */
            kedge = 0;
            E1 = IntersEdgesCopy[kedge];
            if (LocalHead) fprintf (SUMA_STDERR, "%s: No 1 host edge edge found.\n", FuncName);
      }else {
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Found edge.\n", FuncName);
      }
      
      /* remove this edge from the list */
      --(N_IntersEdgesCopy);
      if (LocalHead) fprintf (SUMA_STDERR, "%s: kedge = %d, N_IntersEdgesCopy = %d.\n", FuncName, kedge, N_IntersEdgesCopy);
      IntersEdgesCopy[kedge] = IntersEdgesCopy[N_IntersEdgesCopy];

      /* start a new i_Branch - All i_Branch indices must be > 0*/
      ++i_Branch;   
      if (i_Branch > SUMA_MAX_BRANCHES-1) {
         fprintf (SUMA_STDERR, "Error %s: No more than %d branches allowed.\n", FuncName, SUMA_MAX_BRANCHES);
         SUMA_RETURN (NULL); 
      } 
      
      /* mark the triangle containing E1 */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Marking triangle %d with branch %d.\n", FuncName, SO->EL->ELps[E1][1], i_Branch);
      TriBranch[SO->EL->ELps[E1][1]] = i_Branch;
      VisitationOrder[ivisit] = SO->EL->ELps[E1][1]; ++ivisit;
      
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Called recursive SUMA_Mark_Tri.\n", FuncName);
      if (!SUMA_Mark_Tri (SO->EL, E1, i_Branch, TriBranch, IntersEdgesCopy, &(N_IntersEdgesCopy), VisitationOrder, &ivisit)) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Mark_Tri.\n", FuncName);
      }
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Returned from recursive SUMA_Mark_Tri.\n", FuncName);

      /* repeat till all edges are used up */
   }

   if (Local_IntersEdgesCopy) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing IntersEdgesCopy.\n", FuncName);
      SUMA_free(IntersEdgesCopy); 
   }else {
      /* also change N_IntersEdges */
      SPI->N_IntersEdges = N_IntersEdgesCopy;
   }

   /* SUMA_disp_dvect (TriBranch, SO->N_FaceSet);  */

   N_Branch = i_Branch;

   /* determine the number of branch elements to allocate for - IDIOT PROOF, doing it in the recursive function SUMA_Mark_Tri was annoying*/
   for (i=0; i <= N_Branch; ++i) N_iBranch[i] = 0; /* remember, Branch numbering starts at 1 */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Searching all %d intersected triangles.\n", FuncName, SPI->N_IntersTri);
   Bcnt = 0;
   for (i=0; i < SO->N_FaceSet; ++i) {
      if (TriBranch[i]) {
         /* fprintf (SUMA_STDERR, "%d:%d\t", TriBranch[i], N_iBranch[TriBranch[i]]); */
         ++Bcnt;
         N_iBranch[TriBranch[i]] = N_iBranch[TriBranch[i]] + 1;
      }
   }
   
   #if 0
      fprintf (SUMA_STDERR, "Values in N_iBranch, idiot proof:\n");
      SUMA_disp_dvect (N_iBranch, N_Branch+1);
      fprintf (SUMA_STDERR, "\n");
   #endif
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Found %d triangles belonging to a branch out of %d intersected triangles.\n", FuncName, Bcnt, SPI->N_IntersTri);
     
   /* Now you want to create a vector of N_Branches to represent the intersection */
   Bv = (SUMA_TRI_BRANCH *) SUMA_malloc (sizeof(SUMA_TRI_BRANCH)*(N_Branch+1)); /* you should only need N_Branch, but that 1 won't hurt ...*/
   if (!Bv) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate for Bv.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* initialize allocated Bv elements */
   for (i=0; i<= N_Branch; ++i) { /* You have allocated for N_Branch+1*/
      Bv[i].list = NULL;
      Bv[i].N_list = 0;
   } 
   
   Bcnt = 0;
   for (i=0; i<= N_Branch; ++i) { /* Branch numbering starts at 1 */
      if (N_iBranch[i]) {
         /* something in that branch, allocate and initialize*/
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Allocating for %d elements, Old Branch %d, New Branch %d.\n", FuncName, N_iBranch[i], i, Bcnt); 
         Bv[Bcnt].list = (int *) SUMA_calloc (N_iBranch[i]+1, sizeof(int));
         Bv[Bcnt].N_list = N_iBranch[i];
         Bv[Bcnt].iBranch = Bcnt;
         NBlist[i] = Bcnt; /* store new indexing for Branches */
         ++Bcnt;
      }
      
   }
   
   /* store the total number of used branches */
   *BranchCount = Bcnt;
   
   /* now fill up the branches*/
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Filling up branches...\n", FuncName);
   for (i=0; i <= N_Branch; ++i) N_iBranch[i] = 0; /* now use this vector as a counter for the filling at each new branch index */
   for (i=0; i < SPI->N_IntersTri; ++i) { /* only go over visited triangles */
      TriCheck = TriBranch[VisitationOrder[i]]; /* returns the branch number of triangle VisitationOrder[i] */
      if (TriCheck) {
         Bcnt = NBlist[TriCheck]; /* get the new branch number from the original (old) branch number */
         #if 0
         fprintf (SUMA_STDERR,"%s: Tricheck = %d\n", FuncName, TriCheck); */
         if (Bcnt >= *BranchCount) {
            fprintf (SUMA_STDERR, "\aError %s: BranchCount = %d <= Bcnt = %d.\n", FuncName, *BranchCount, Bcnt);
         }
         if (N_iBranch[Bcnt] >= Bv[Bcnt].N_list) {
            fprintf (SUMA_STDERR, "\aError %s: Bcnt = %d. N_iBranch[Bcnt] = %d >= Bv[Bcnt].N_list = %d\n", FuncName, Bcnt, N_iBranch[Bcnt], Bv[Bcnt].N_list);
         }
         #endif
         Bv[Bcnt].list[N_iBranch[Bcnt]] = VisitationOrder[i]; /* store the index of the visited triangle in that branch */
         N_iBranch[Bcnt] += 1; /* store the number of elements in that branch */
      }
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: freeing ...\n", FuncName);
   if (VisitationOrder) SUMA_free(VisitationOrder);
   if (TriBranch) SUMA_free(TriBranch);
   SUMA_RETURN (Bv);    
}

/*! 
   Function to show the contents of a SUMA_TRI_BRANCH structure
*/
SUMA_Boolean SUMA_show_STB (SUMA_TRI_BRANCH *B, FILE *Out)
{
   static char FuncName[]={"SUMA_show_STB"};
   int i;
   
   SUMA_ENTRY;
  
   if (!Out) Out = SUMA_STDERR;
   
   if (!B) {
      fprintf (Out, "%s: Empy structure.\n", FuncName);
   }
   
   fprintf (Out, "%s:\tBranch #%d. %d elements in list\nlist:\t", FuncName, B->iBranch, B->N_list);
   for (i=0; i < B->N_list; ++i) {
      fprintf (Out, "%d\t", B->list[i]);
   }
   fprintf (Out, "\n");
   
   SUMA_RETURN (YUP);
}

/*!
   Function to free a vector of SUMA_TRI_BRANCH structures.
*/

void SUMA_free_STB (SUMA_TRI_BRANCH *Bv, int N_Bv) 
{

   static char FuncName[]={"SUMA_free_STB"};
   int i;
   
   SUMA_ENTRY;
   
   for (i=0; i < N_Bv; ++i) {
      if (Bv[i].list) SUMA_free(Bv[i].list);
   }
   if (Bv) SUMA_free(Bv);
   
   SUMA_RETURNe;
    
}

/*!
   \brief Allocates a structure for computing the intersection of a surface with a plane
   The allocation is done conservatively, expecting the worse case scenario. 
   
   \param SO (SUMA_SurfaceObject *) Surface Object structure used to get number of nodes, edges, etc ...
   \return SPI (SUMA_SURF_PLANE_INTERSECT *) Pointer to surface plane intersection structure.
         see structure definition for more info.

*/
SUMA_SURF_PLANE_INTERSECT * SUMA_Allocate_SPI (SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_Allocate_SPI"};
   int i;
   SUMA_SURF_PLANE_INTERSECT *SPI = NULL;
   
   SUMA_ENTRY;
   
   SPI = (SUMA_SURF_PLANE_INTERSECT *) SUMA_malloc(sizeof(SUMA_SURF_PLANE_INTERSECT));
   if (!SPI) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate for SPI\n", FuncName);
      SUMA_RETURN (SPI);
   }
   
   SPI->IntersEdges = (int *) SUMA_calloc (SO->EL->N_EL, sizeof(int)); /* allocate for the max imaginable*/
   SPI->IntersNodes = (float *) SUMA_calloc (3 * SO->EL->N_EL, sizeof(float));
   SPI->isEdgeInters = (SUMA_Boolean *) SUMA_calloc (SO->EL->N_EL, sizeof(SUMA_Boolean));
   SPI->IntersTri = (int *) SUMA_calloc (SO->N_FaceSet, sizeof(int));
   SPI->isNodeInMesh = (SUMA_Boolean *) SUMA_calloc (SO->N_Node, sizeof(SUMA_Boolean));
   SPI->isTriHit = (SUMA_Boolean *) SUMA_calloc (SO->N_FaceSet, sizeof(SUMA_Boolean));

   if (!SPI->IntersEdges || !SPI->IntersTri || !SPI->IntersNodes || !SPI->isTriHit || !SPI->isEdgeInters)
      {
         fprintf (SUMA_STDERR, "Error %s: Could not allocate \n", FuncName);
         SUMA_RETURN (SPI);
      }
   
   SPI->N_IntersEdges = 0;
   SPI->N_IntersTri = 0;
   SPI->N_NodesInMesh = 0;  
   SUMA_RETURN (SPI);
}

/*!
free the SPI structure    
*/
void SUMA_free_SPI (SUMA_SURF_PLANE_INTERSECT *SPI)
{
   static char FuncName[]={"SUMA_free_SPI"};
   
   SUMA_ENTRY;
   
   if (!SPI) SUMA_RETURNe;
   if (SPI->IntersTri) SUMA_free(SPI->IntersTri);
   if (SPI->IntersNodes) SUMA_free(SPI->IntersNodes);
   if (SPI->IntersEdges) SUMA_free(SPI->IntersEdges);
   if (SPI->isNodeInMesh) SUMA_free(SPI->isNodeInMesh); 
   if (SPI->isTriHit) SUMA_free (SPI->isTriHit);
   if (SPI->isEdgeInters) SUMA_free (SPI->isEdgeInters);
     
   if (SPI) SUMA_free(SPI);
   
   SUMA_RETURNe;
}

/*! 
Show the SPI structure 
*/
SUMA_Boolean SUMA_Show_SPI (SUMA_SURF_PLANE_INTERSECT *SPI, FILE * Out, SUMA_SurfaceObject *SO, char *opref, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Show_SPI"};
   int i, j;
   char *fname=NULL;
   FILE *fout=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   if (!Out) Out = SUMA_STDERR;
   
   if (!SPI) {
      fprintf (Out,"Error %s: NULL POINTER.\n", FuncName);
   }
   
   fprintf (Out,"Intersection Edges: %d\n[", SPI->N_IntersEdges);
   for (i=0; i < SPI->N_IntersEdges; ++i) {
      fprintf (Out, "%d, %d\n", SO->EL->EL[SPI->IntersEdges[i]][0], SO->EL->EL[SPI->IntersEdges[i]][1]);
   }
   fprintf (Out," ]\n");
   if (opref) {
      fname = SUMA_append_string(opref, "_edges.1D");
      fout = fopen(fname,"w");
      fprintf (fout, "#segments\n#segments intersected by plane");
      for (i=0; i < SPI->N_IntersEdges; ++i) {
         fprintf (fout, "%f %f %f    %f %f %f\n", 
            SO->NodeList[3*SO->EL->EL[SPI->IntersEdges[i]][0]],
            SO->NodeList[3*SO->EL->EL[SPI->IntersEdges[i]][0]+1],
            SO->NodeList[3*SO->EL->EL[SPI->IntersEdges[i]][0]+2], 
            SO->NodeList[3*SO->EL->EL[SPI->IntersEdges[i]][1]],
            SO->NodeList[3*SO->EL->EL[SPI->IntersEdges[i]][1]+1],
            SO->NodeList[3*SO->EL->EL[SPI->IntersEdges[i]][1]+2]);
      }
      fclose(fout); fout = NULL; SUMA_free(fname); fname=NULL;   
   }
   if (sv) {
      SUMA_SegmentDO *SDO = NULL;
      if ((SDO = SUMA_Alloc_SegmentDO (SPI->N_IntersEdges, "Show_SPI_segs", 0, NULL, 0, LS_type))) {
         SDO->do_type = LS_type;
         for (i=0; i < SPI->N_IntersEdges; ++i) {
            for (j=0; j<3;++j) {
               SDO->n0[3*i+j] = SO->NodeList[3*SO->EL->EL[SPI->IntersEdges[i]][0]+j];
               SDO->n1[3*i+j] = SO->NodeList[3*SO->EL->EL[SPI->IntersEdges[i]][1]+j];
            }
         }   
         /* addDO */
         if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)SDO, LS_type, SUMA_WORLD)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
            SUMA_RETURNe;
         }

         /* register DO with viewer */
         if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
            SUMA_RETURNe;
         }

         /* redisplay curent only*/
         sv->ResetGLStateVariables = YUP;
         SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
      }
   }
   
   fprintf (Out,"Intersection Nodes: %d\n[", SPI->N_IntersEdges);
   for (i=0; i < SO->EL->N_EL; ++i) {
      if (SPI->isEdgeInters[i]) fprintf (Out, "%f, %f, %f, ", SPI->IntersNodes[3*i], SPI->IntersNodes[3*i+1], SPI->IntersNodes[3*i+2]);
   }
   fprintf (Out," ]\n");
   if (opref) {
      fname = SUMA_append_string(opref, "_interspoints.1D");
      fout = fopen(fname,"w");
      fprintf (fout, "#spheres\n#locations of intersections\n");
      for (i=0; i < SO->EL->N_EL; ++i) {
         if (SPI->isEdgeInters[i]) 
            fprintf (fout, "%f %f %f\n ", SPI->IntersNodes[3*i], SPI->IntersNodes[3*i+1], SPI->IntersNodes[3*i+2]);
      }
      fclose(fout); fout = NULL; SUMA_free(fname); fname=NULL;   
   }
   if (sv) {
      SUMA_SphereDO *SDO = NULL;
      if ((SDO = SUMA_Alloc_SphereDO (SPI->N_IntersEdges, "Show_SPI_interspoints", NULL, SP_type))) {
         SDO->do_type = SP_type;
         SDO->CommonRad = SO->EL->AvgLe/6.0;
         for (i=0; i < SPI->N_IntersEdges; ++i) {
            {
               SDO->cxyz[3*i  ] = SPI->IntersNodes[3*SPI->IntersEdges[i]  ];
               SDO->cxyz[3*i+1] = SPI->IntersNodes[3*SPI->IntersEdges[i]+1];
               SDO->cxyz[3*i+2] = SPI->IntersNodes[3*SPI->IntersEdges[i]+2]; 
            }
         }
         /* addDO */
         if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)SDO, SP_type, SUMA_WORLD)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
            SUMA_RETURNe;
         }

         /* register DO with viewer */
         if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
            SUMA_RETURNe;
         }

         /* redisplay curent only*/
         sv->ResetGLStateVariables = YUP;
         SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
      }
      
   }
   fprintf (Out,"Intersected Triangles: %d\n[", SPI->N_IntersTri);
   for (i=0; i < SPI->N_IntersTri; ++i) {
      fprintf (Out, "t%d\t", SPI->IntersTri[i]);
   }
   fprintf (Out," ]\n");
   SUMA_RETURN(YUP);
}

#define NO_LOG
SUMA_Boolean SUMA_Mark_Tri (SUMA_EDGE_LIST  *EL, int E1, int iBranch, int *TriBranch, int *IsInter, int *N_IsInter, int *VisitationOrder, int *ivisit)
{
   static char FuncName[]={"SUMA_Mark_Tri"};
   int Tri = -1, Found, k, kedge = 0, E2, Ntri = 0;
   static int In = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   /* this is a recursive function, you don't want to log every time it is called */
   /* SUMA_EN TRY;  *//* syntax (space) error on purpose, to avoid upsetting AnalyzeTrace on this file  */
   
   ++In;
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Entered #%d.\n", FuncName, In);
   
   /* find the next triangle hosting E1, if possible, otherwise it is the end of the branch. */
   if (EL->ELps[E1][2] != 2) { /* reached a dead end , end of branch */
      /* mark triangle, remove E1 from list and return */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: reached end of branch.\n", FuncName);
      kedge = 0;
      Found = NOPE;
      while (!Found && kedge < *N_IsInter) {
         if (IsInter[kedge] == E1) {
            Found = YUP;
            *N_IsInter = *N_IsInter - 1;
            IsInter[kedge] = IsInter[*N_IsInter];
         } else ++kedge;
      }
      return (YUP);
   }else {
      Tri = EL->ELps[E1][1];
      if (TriBranch[Tri]) { /* try second triangle */
         Tri = EL->ELps[E1+1][1];
      }
      if (LocalHead) fprintf (SUMA_STDERR, "%s: moving on to triangle %d.\n", FuncName, Tri);
   }
   
   if (!TriBranch[Tri]) { 
      /* unvisited, mark with iBranch */
      TriBranch[Tri] = iBranch;
      VisitationOrder[*ivisit] = Tri;
      ++(*ivisit);
      /* find other edges in this triangle that have been intersected */
      Found = NOPE; 
      k = 0;
      while (!Found && k < 3) {
         E2 = EL->Tri_limb[Tri][k]; /* this may not be the first occurence of this edge since the list contains duplicates */
         if (LocalHead) {
            fprintf (SUMA_STDERR, "%s: Trying edge E2 %d (%d %d), tiangle %d, edge %d.\n", 
                     FuncName, E2, EL->EL[E2][0], EL->EL[E2][1], Tri, k);
         }
         while (EL->ELps[E2][2] < 0) { /* find the first occurence of this edge in the list */
            E2--;
         }
         if (LocalHead) fprintf (SUMA_STDERR, "%s: E2 changed to %d. E1 is %d\n", FuncName, E2, E1);
         if (E2 != E1) {
            /* was E2 intersected ? */
            kedge = 0;
            while (!Found && kedge < *N_IsInter) {
               if (IsInter[kedge] == E2) {
                  Found = YUP;
                  if (LocalHead) fprintf (SUMA_STDERR, "%s: E2 is intersected.\n", FuncName);
               }
               else ++kedge;
            }
         }
         ++k;
      }
      
      if (!Found) {
         fprintf (SUMA_STDERR, "Error %s: No second edge found.\n", FuncName);
         return (NOPE);
      } else {
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Removing E2 from List and calling SUMA_Mark_Tri.\n", FuncName);
         /* remove this new edge from the list */
         *N_IsInter = *N_IsInter - 1;
         IsInter[kedge] = IsInter[*N_IsInter];
         
         /* continue visitation */
         if (!SUMA_Mark_Tri (EL, E2, iBranch, TriBranch, IsInter, N_IsInter, VisitationOrder, ivisit)) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Mark_Tri.\n", FuncName);
            return (NOPE);
         } 
         return (YUP);
      }
   } else {
      if (TriBranch[Tri] != iBranch) {
         fprintf (SUMA_STDERR, "\a%s: Branches colliding, Must weld %d to %d.\n", FuncName, iBranch, TriBranch[Tri]);
         
         /* DO NOT MODIFY THE VALUE OF BRANCH or you will mistakingly link future branches*/ 
      }
      /* visited, end of branch return */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: End of branch. Returning.\n", FuncName);
      return (YUP);
   }
   
   fprintf (SUMA_STDERR, "Error %s: Should not be here.\n", FuncName);
   return (NOPE);
}

/*!
   E = SUMA_Find_Edge_N_Host (EL, IsInter, N_IsInter, Nhost);
   \brief Finds an edge that has Nhost hosting triangles. Only the edges indexed in IsInter are examined 
   \param EL (SUMA_EDGE_LIST *) Complete Edge list structure for surface
   \param IsInter (int *) vector containing indices into EL->EL matrix which contains the EdgeList
   \param N_IsInter (int) number of elements in IsInter
   \param kedge (int *) pointer to index into IsInter where E was found
   \param Nhost number of hosting triangles (should be 2 for a closed surface, 1 for edge edges and more than 2 for errors in tessellation
   \return E (int) index into EL->EL of the first edge (of those listed in IsInter) encountered that has N hosting triangles.
      -1 is returned if no edges are found
   
   This function is meant to be used with SUMA_Surf_Plane_Intersect
*/
int SUMA_Find_Edge_Nhost (SUMA_EDGE_LIST  *EL, int *IsInter, int N_IsInter, int *i, int Nhost)
{
   static char FuncName[]={"SUMA_Find_Edge_Nhost"};
   
   SUMA_ENTRY;

   for (*i=0; *i < N_IsInter; ++(*i)) {
      if (EL->ELps[IsInter[*i]][2] == Nhost) SUMA_RETURN (IsInter[*i]);
   }
   
   SUMA_RETURN (-1);

}

/*! 
\brief Path = SUMA_Dijkstra (SO, Nx, Ny, isNodeInMesh, N_isNodeInMesh, Method_Number, Path_length, N_Path);
      Finds the shortest distance between nodes Nx and Ny on SO with a restriction on the number of nodes
      available for travel. In other terms, the search space is limited to a subset of the nodes forming SO. 
      The subset of nodes is stored in isNodeInMesh. This subset is typically specified in SUMA_Surf_Plane_Intersect.
      
      
 Path = SUMA_Dijkstra ( SO, Nx,  Ny, 
                        isNodeInMesh, N_isNodeInMesh, Method_Number, 
                        Path_length, N_Path)
\param SO (SUMA_SurfaceObject *) The surface Object structure. 
            NodeList, EL and FN are needed. 
\param Nx (int) The node index (referring to SO's nodes) where the search begins.
\param Ny (int) The node index (referring to SO's nodes) where the search ends.
\param isNodeInMesh (SUMA_Boolean *) 
         Pointer to SO->N_Node long vector such that 
         if (isNodeInMesh[i]) then node i is part of the 
         mesh that is used in the search path. This mesh is a subset 
         of SO->FaceSetList and is typically obtained when one 
         runs SUMA_Surf_Plane_Intersect. Running SUMA_Dijkstra on 
         a complete surface is only for very patient people.
   NOTE:  This vector is modified as a node is visited. Make sure you 
         do not use it after this function has been called.
         Set to NULL if you want all nodes used.
\param N_isNodeInMesh (int *) Pointer to the total number of nodes that 
               make up the mesh (subset of SO)
               This parameter is passed as a pointer because as nodes in the mesh
               are visited, that number is reduced and represents when the
               function returns, the number of nodes that were
               never visited in the search. 
               Set to NULL, if isNodeInMesh is NULL
\param Method_Number (int) selector for which algorithm to use. Choose from:
                     0 - Straight forward implementation, slow
                     1 - Variation to eliminate long searches for minimum of L, much much much faster than 0, 5 times more memory.
\param Path_length (float *) The distance between Nx and Ny. This value is negative if no path between Nx and Ny was found.
\param N_Path (int *) Number of nodes forming the Path vector

\return Path (float) A vector of N_Path node indices forming the shortest path, from Nx (Path[0]) to Ny (Path[*N_Path - 1]). 
                  NULL is returned in case of error.

\sa Graph Theory by Ronald Gould and labbook NIH-2 page 154 for path construction
*/
#define LARGE_NUM 9e300
/* #define LOCALDEBUG */ /* lots of debugging info. */
int * SUMA_Dijkstra (SUMA_SurfaceObject *SO, int Nx, 
                     int Ny, SUMA_Boolean *isNodeInMeshp, 
                     int *N_isNodeInMesh, int Method_Number, 
                     float *Lfinal, int *N_Path)
{
   static char FuncName[] = {"SUMA_Dijkstra"};
   SUMA_Boolean LocalHead = NOPE;
   float *L = NULL, Lmin = -1.0, le = 0.0, DT_DIJKSTRA;
   int i, iw, iv, v, w, N_Neighb, *Path = NULL, N_loc=-1;
   SUMA_Boolean *isNodeInMesh=NULL;
   struct  timeval  start_time;
   SUMA_DIJKSTRA_PATH_CHAIN *DC = NULL, *DCi=NULL, *DCp=NULL;
   SUMA_Boolean Found = NOPE;
   /* variables for method 2 */
   int N_Lmins, *vLmins=NULL, *vLocInLmins=NULL, 
      iLmins, ReplacingNode, ReplacedNodeLocation;
   float *Lmins=NULL; 
   
   
   SUMA_ENTRY;
   
   *Lfinal = -1.0;
   *N_Path = 0;
   
   if (!isNodeInMeshp) {
      if (!(isNodeInMesh = (SUMA_Boolean *)SUMA_malloc(  sizeof(SUMA_Boolean) * 
                                                         SO->N_Node) )) {
                                                   
         SUMA_S_Err("Failed to allocate"); 
         goto CLEANUP;
      }
      memset((void*)isNodeInMesh, 1,  sizeof(SUMA_Boolean) * SO->N_Node);
      N_isNodeInMesh = &N_loc;
      *N_isNodeInMesh = SO->N_Node;                                          
   } else {
      isNodeInMesh = isNodeInMeshp;
   }
   
   /* make sure Both Nx and Ny exist in isNodeInMesh */
   if (!isNodeInMesh[Nx]) {
      fprintf (SUMA_STDERR,"\aError %s: Node %d (Nx) is not in mesh.\n", FuncName, Nx);
      goto CLEANUP;
   }  
   if (!isNodeInMesh[Ny]) {
      fprintf (SUMA_STDERR,"\aError %s: Node %d (Ny) is not in mesh.\n", FuncName, Ny);
      goto CLEANUP;
   }

   if (!SO->FN) {
      fprintf (SUMA_STDERR, "Error %s: SO does not have FN structure.\n", FuncName);
      goto CLEANUP;
   }

   if (LocalHead) {
      /* Start timer for next function */
      SUMA_etime(&start_time,0);      
   }
   
   /* allocate for chain */
   DC = (SUMA_DIJKSTRA_PATH_CHAIN *) SUMA_malloc (sizeof(SUMA_DIJKSTRA_PATH_CHAIN) * SO->N_Node);
   if (!DC) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate. \n", FuncName);
      goto CLEANUP;
   }
   
   switch (Method_Number) {
   
      case 0:  /* Method 0, Brute force */
         /* allocate for vertices labels */
         L = (float *) SUMA_calloc (SO->N_Node, sizeof (float));
         if (!L) {
            fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
            goto CLEANUP;
         }

         /* label all vertices with very large numbers, initialize path previous pointers to null */
         for (i=0; i < SO->N_Node; ++i) {
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
            /* this sucks up a lot of time because it is searching the entire set of SO->N_Node instead of the one that was intersected only.
            This can be sped up, considerably */
            SUMA_MIN_LOC_VEC(L, SO->N_Node, Lmin, v);   /* locates and finds the minimum of L, nodes not in mesh will keep their large values and will not be picked*/
            if (!isNodeInMesh[v]) {
               fprintf (SUMA_STDERR, "\aERROR %s: Dijkstra derailed. v = %d, Lmin = %f\n. Try another point.", FuncName, v, Lmin);
               goto CLEANUP; 
            }
            if (v == Ny) {
               if (LocalHead) fprintf (SUMA_STDERR, "%s: Done.\n", FuncName);
               *Lfinal = L[v];
               Found = YUP;
            } else {
               N_Neighb = SO->FN->N_Neighb[v];
               for (i=0; i < N_Neighb; ++i) {
                  w = SO->FN->FirstNeighb[v][i];
                  if (isNodeInMesh[w]) {
                     iw = 3*w;
                     iv = 3*v;
                     le = sqrt ( (SO->NodeList[iw] - SO->NodeList[iv]) * (SO->NodeList[iw] - SO->NodeList[iv]) +
                                 (SO->NodeList[iw+1] - SO->NodeList[iv+1]) * (SO->NodeList[iw+1] - SO->NodeList[iv+1]) +
                                 (SO->NodeList[iw+2] - SO->NodeList[iv+2]) * (SO->NodeList[iw+2] - SO->NodeList[iv+2]) );
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

               /* remove node v from isNodeInMesh and reset their distance value to a very large one, 
                  this way you do not have to reinitialize this variable. */
               isNodeInMesh[v] = NOPE;
               *N_isNodeInMesh -= 1;
               L[v] = LARGE_NUM; 
               Found = NOPE;
            }
         } while (*N_isNodeInMesh > 0 && !Found);

         if (!Found) {
            fprintf (SUMA_STDERR, "Error %s: No more nodes in mesh, failed to reach target.\n", FuncName);
            goto CLEANUP;
         }else {
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Path between Nodes %d and %d is %f.\n", FuncName, Nx, Ny, *Lfinal);
         }


         if (LocalHead) {
            /* stop timer */
            DT_DIJKSTRA = SUMA_etime(&start_time,1);
            fprintf (SUMA_STDERR, "%s: Method 1- Elapsed time in function %f seconds.\n", FuncName, DT_DIJKSTRA);
         }

         if (L) SUMA_free(L); L = NULL;
         break;

      case 1:  /********* Method 1- faster minimum searching *******************/
         if (LocalHead) {
            /* Start timer for next function */
            SUMA_etime(&start_time,0);      
         }

         /* allocate for vertices labels and minimums vectors*/
         L = (float *) SUMA_calloc (SO->N_Node, sizeof (float));        /* L[i] = distance to a node i*/
         Lmins = (float *) SUMA_calloc (SO->N_Node, sizeof (float));    /* Lmins = vector containing minimum calculated distances to node */
         vLmins = (int *) SUMA_calloc (SO->N_Node, sizeof (int));       /* vLmins[i] = index (into L) of the node having a distance Lmins[i] */
         vLocInLmins = (int *) SUMA_calloc (SO->N_Node, sizeof (int));  /* vLocInLmin[j] = index (into Lmins) of a node having index j (into L) */

         if (!L || !Lmins || !vLmins || !vLocInLmins) {
            fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
            goto CLEANUP;
         }

         /* label all vertices with very large numbers and initialize vLocInLmins to -1*/
         for (i=0; i < SO->N_Node; ++i) {
            L[i] = LARGE_NUM;
            Lmins[i] = LARGE_NUM;   
            vLocInLmins[i] = -1;            
            DC[i].Previous = NULL;
         }

         /* label starting vertex with 0 */
         L[Nx] = 0.0;
         *Lfinal = -1.0;

         /* initialize values of vectors used to keep track of minimum values of L and their corresponding nodes */
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
         if (LocalHead) fprintf (SUMA_STDERR, "%s: about to MIN_LOC ....N_isNodeInMesh = %d\n", FuncName, *N_isNodeInMesh);
         do {
            /* find v in Mesh / L(v) is minimal */
            SUMA_MIN_LOC_VEC(Lmins, N_Lmins, Lmin, iLmins);   /* locates the minimum value in Lmins vector */
            v = vLmins[iLmins];   /* get the node for this Lmin value */
            if (!isNodeInMesh[v]) {
               fprintf (SUMA_STDERR,"\aERROR %s: \n"
                                    "Dijkstra derailed. v = %d, Lmin = %f\n."
                                    "Try another point.", FuncName, v, Lmin);
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
               N_Neighb = SO->FN->N_Neighb[v];
               for (i=0; i < N_Neighb; ++i) {
                  w = SO->FN->FirstNeighb[v][i];
                  if (isNodeInMesh[w]) {
                     iw = 3*w;
                     iv = 3*v;
                     le = sqrt ( (SO->NodeList[iw] - SO->NodeList[iv]) * (SO->NodeList[iw] - SO->NodeList[iv]) +
                                 (SO->NodeList[iw+1] - SO->NodeList[iv+1]) * (SO->NodeList[iw+1] - SO->NodeList[iv+1]) +
                                 (SO->NodeList[iw+2] - SO->NodeList[iv+2]) * (SO->NodeList[iw+2] - SO->NodeList[iv+2]) );
                     if (L[w] > L[v] + le ) {
                        #ifdef LOCALDEBUG
                           fprintf (SUMA_STDERR, "%s: L[%d]=%f > L[%d] = %f + le = %f.\n", FuncName, w, L[w], v, L[v], le);
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
                              fprintf (SUMA_STDERR, "%s: adding entry for w = %d - First Hit. \n", FuncName, w);
                           #endif
                           Lmins[N_Lmins] = L[w]; /* add this value to Lmins vector */
                           vLmins[N_Lmins] = w; /* store the node for this Lmins value */
                           vLocInLmins[w] = N_Lmins; /* store where that node is represented in Lmins */
                           ++N_Lmins;  /* increment N_Lmins */  
                        } else {
                           #ifdef LOCALDEBUG
                              fprintf (SUMA_STDERR, "%s: modifying entry for w = %d  Second Hit.\n", FuncName, w); */
                           #endif
                           Lmins[vLocInLmins[w]] = L[w]; /* update value for Lmins */
                        }                        
                     }else {
                        #ifdef LOCALDEBUG
                           fprintf (SUMA_STDERR, "%s: L[%d]=%f < L[%d] = %f + le = %f.\n", FuncName, w, L[w], v, L[v], le); */
                        #endif
                     } 
                  }
               }

               /* remove node v from isNodeInMesh and reset their distance value to a very large one, 
                  this way you do not have to reinitialize this variable. */
               isNodeInMesh[v] = NOPE;
               *N_isNodeInMesh -= 1;
               L[v] = LARGE_NUM; 
               Found = NOPE;

               /* also remove the values (by swapping it with last element) for this node from Lmins */
               #ifdef LOCALDEBUG
                  {
                     int kkk;
                     fprintf (SUMA_STDERR,"Lmins\tvLmins\tvLocInLmins\n");
                     for (kkk=0; kkk < N_Lmins; ++kkk) fprintf (SUMA_STDERR,"%f\t%d\t%d\n", Lmins[kkk], vLmins[kkk], vLocInLmins[vLmins[kkk]] );
               
                  }
               #endif
               
               if (vLocInLmins[v] >= 0) { /* remove its entry if there is one */
                  #ifdef LOCALDEBUG
                     fprintf (SUMA_STDERR, "%s: removing node v = %d. N_Lmins = %d\n", FuncName,  v, N_Lmins);
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
            fprintf (SUMA_STDERR, "Error %s: No more nodes in mesh, failed to reach target %d. NLmins = %d\n", FuncName, Ny, N_Lmins);
            goto CLEANUP;
         }else {
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Path between Nodes %d and %d is %f.\n", FuncName, Nx, Ny, *Lfinal);
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

/*!
\brief Converts a path formed by a series of connected nodes to a series of edges
   ePath = SUMA_NodePath_to_EdgePath (EL, Path, N_Path, N_Edge);
   \param EL (SUMA_EDGE_LIST *) Pointer to edge list structure
   \param Path (int *) vector of node indices forming a path.
                       Sequential nodes in Path must be connected on the surface mesh.
   \param N_Path (int) number of nodes in the path 
   \param N_Edge (int *) pointer to integer that will contain the number of edges in the path
                        usually equal to N_Path if path is a loop (closed) or N_Path - 1. 
                        0 if function fails.
   \param ePath (int *) pointer to vector containing indices of edges forming the path. 
                        The indices are into EL->EL and represent the first occurence of the
                        edge between Path[i] and Path[i+1].
                        NULL if trouble is encountered.
                        
   \sa SUMA_NodePath_to_EdgePath_Inters
   \sa Path S in labbook NIH-2 page 153
*/
int *SUMA_NodePath_to_EdgePath (SUMA_EDGE_LIST *EL, int *Path, int N_Path, int *N_Edge)
{
   static char FuncName[]={"SUMA_NodePath_to_EdgePath"};
   int *ePath = NULL, i, i0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
 
 
  *N_Edge = 0;
   ePath = (int *) SUMA_calloc(N_Path, sizeof(int));
   if (!ePath) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      SUMA_RETURN (NULL);
   }

   for (i=1; i<N_Path; ++i) {
      i0 = Path[i-1];
      /* find the location of the edge between i0 and i1 */
      ePath[i-1] = SUMA_FindEdge (EL, i0, Path[i]); 
      if (ePath[i-1] < 0) { /* error */
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_FindEdge.\n", FuncName);
         SUMA_free(ePath);
         *N_Edge = 0;
         SUMA_RETURN (NULL);   
      }else {
         ++(*N_Edge);
      }
   }

   SUMA_RETURN (ePath);   
}   

/*!
\brief determines whether to edges are identical or not. Recall
that an edge can be represented multiple times in SO->EL, once for
each triangle that uses it. Two edges are the same if and only if
EL->EL[E1][0] == EL->EL[E2][0] && EL->EL[E1][1] == EL->EL[E2][1]

ans = SUMA_isSameEdge ( EL, E1, E2); 

\param EL (SUMA_EDGE_LIST *) Edge List structure.
\param E1 (int) edge index
\param E2 (int) edge index
\return ans (SUMA_Boolean) YUP/NOPE
*/

SUMA_Boolean SUMA_isSameEdge (SUMA_EDGE_LIST *EL, int E1, int E2) 
{
   static char FuncName[]={"SUMA_isSameEdge"};
   
   SUMA_ENTRY;

   if (EL->EL[E1][0] == EL->EL[E2][0] && EL->EL[E1][1] == EL->EL[E2][1]) {
      SUMA_RETURN (YUP);
   } else {
      SUMA_RETURN (NOPE);
   }
   
}

/*!
\brief This function determines the strip of triangles necessary to go from one node to another
along intersected edges. 
tPath = SUMA_IntersectionStrip (SO, SPI, 
            int *nPath, int N_nPath, float *dinters, float dmax, int *N_tPath)
\param SO (SUMA_SurfaceObject *) pointer to Surface Object structure
\param SPI (SUMA_SURF_PLANE_INTERSECT *) pointer surface/plane intersection structure
\param nPath (int *) series of nodes forming the Dijkstra path between nodes Nx (nPath[0] and NynPath[N_nPath-1])
\param N_nPath (int) number of nodes in nPath. Note: Only nPath[0], nPath[1] and nPath[N_nPath-1] are used by this function
\param dinters (float *) pointer sum of distances between intersection points on intersected edges for all triangles in tPath.
               This distance is a better approximation for the distance along the cortical surface than the distance obtained
               along the shortest path.
\param dmax (float) distance beyond which to quit searching. Usually this distance is slightly larger than the distance
                  along the path returned by SUMA_Dijkstra but dinters should always be less than the distance along the shortest path.
\param N_tPath (int *) pointer to the number of triangle indices in tPath
\return tPath (int*) pointer to vector containing the indices of triangles travelled from 
         nPath[0] to nPath[N_nPath] (or vice versa if nPath[0] = SO->N_Node-1).

NOTE: Although the number of elements in tPath may be small, the number of elements allocated for is SO->N_FaceSet
Make sure you free tPath when you are done with it.

NOTE: This function can be used to create a node path formed by intersection points along edges but that is not implemented yet.

\sa SUMA_Surf_Plane_Intersect
\sa SUMA_Dijkstra
\sa SUMA_FromIntEdgeToIntEdge

*/
int * SUMA_IntersectionStrip (SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI, 
            int *nPath, int N_nPath, float *dinters, float dmax, int *N_tPath)
{
   static char FuncName[]={"SUMA_IntersectionStrip"};
   int *tPath1 = NULL, *tPath2 = NULL, Incident[50], N_Incident, Nx = -1, 
      Ny = -1, Tri = -1, Tri1 = -1, istart, n2 = -1, n3 = -1, E1, E2, cnt, N_tPath1, N_tPath2;
   float d1, d2;
   SUMA_Boolean *Visited = NULL, Found, LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* find the edge containing the 1st 2 nodes of the Dijkstra path */
   /* find the triangle that contains the edge formed by the 1st 2 nodes of the Dijkstra path and is intersected by the plane*/
   Tri1 = -1;
   if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: Looking for a triangle containing nodes [%d %d].\n", FuncName, nPath[0], nPath[1]);
   }
   
   Found = SUMA_Get_Incident(nPath[0], nPath[1], SO->EL, Incident, &N_Incident, 1, 0);
   if (!Found) {
      /* no such triangle, get a triangle that contains nPath[0] and is intersected */
      fprintf (SUMA_STDERR, "%s: No triangle contains nodes [%d %d].\n", FuncName, nPath[0], nPath[1]);
      if (nPath[0] == SO->N_Node - 1) {
         fprintf (SUMA_STDERR, "Warning %s: 1st node is last node of surface, traversing path backwards.\n", FuncName);
         Nx = nPath[N_nPath - 1];
         Ny = nPath[0];
      }else {
         Nx = nPath[0];
         Ny = nPath[N_nPath - 1];
      }
      istart = SO->EL->ELloc[Nx];
      /* find an edge containing the first node and belonging to an intersected triangle */
      Found = NOPE;
      while (SO->EL->EL[istart][0] == Nx && !Found) {
         Tri = SO->EL->ELps[istart][1];
         if (SPI->isTriHit[Tri]) {
            Found = YUP;
            Tri1 = Tri;
         }
         ++istart;
      } 
   }else {
      Nx = nPath[0];
      Ny = nPath[N_nPath - 1];
      
      /* find which of these triangles was intersected */
      if (LocalHead) {
         fprintf (SUMA_STDERR, "%s: Found %d triangles containing nodes [%d %d].\n", FuncName, N_Incident, nPath[0], nPath[1]);
         for (cnt = 0; cnt < N_Incident; ++cnt) fprintf (SUMA_STDERR, "%d isHit %d\n", Incident[cnt], SPI->isTriHit[Incident[cnt]]);
         fprintf (SUMA_STDERR, "\n"); 
      }
      Found = NOPE;
      cnt = 0;
      while (cnt < N_Incident && !Found) {
         if (SPI->isTriHit[Incident[cnt]]) {
            Found = YUP;
            Tri1 = Incident[cnt];
         }
         ++cnt;
      }
   }
   
   if (!Found) {
      fprintf (SUMA_STDERR, "Error %s: Starting Edge could not be found.\n", FuncName);
      SUMA_RETURN (NULL);
   }else if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: Starting with triangle %d.\n", FuncName, Tri1);
   }

   /* found starting triangle edge, begin with side 1 */
   if (SO->FaceSetList[3*Tri1] == Nx) {
      n2 = SO->FaceSetList[3*Tri1+1];
      n3 = SO->FaceSetList[3*Tri1+2];
   } else if (SO->FaceSetList[3*Tri1+1] == Nx) {
      n2 = SO->FaceSetList[3*Tri1];
      n3 = SO->FaceSetList[3*Tri1+2];
   } else if (SO->FaceSetList[3*Tri1+2] == Nx) {
      n2 = SO->FaceSetList[3*Tri1];
      n3 = SO->FaceSetList[3*Tri1+1];
   } else {
      fprintf (SUMA_STDERR, "Error %s: Triangle %d does not contain Nx %d.\n", FuncName, Tri1, Nx);
      SUMA_RETURN (NULL);
   }  
   
   
   
   E1 = SUMA_FindEdgeInTri (SO->EL, Nx, n2, Tri1);
   if (!SPI->isEdgeInters[E1]) {
      E1 = SUMA_FindEdgeInTri (SO->EL, Nx, n3, Tri1);
   }
   /* now choose E2 such that E2 is also intersected */
   if (!SUMA_isSameEdge (SO->EL, SO->EL->Tri_limb[Tri1][0], E1) && SPI->isEdgeInters[SO->EL->Tri_limb[Tri1][0]]) {
      E2 = SO->EL->Tri_limb[Tri1][0];
   }else if (!SUMA_isSameEdge (SO->EL, SO->EL->Tri_limb[Tri1][1], E1) && SPI->isEdgeInters[SO->EL->Tri_limb[Tri1][1]]) {
      E2 = SO->EL->Tri_limb[Tri1][1];
   }else if (!SUMA_isSameEdge (SO->EL, SO->EL->Tri_limb[Tri1][2], E1) && SPI->isEdgeInters[SO->EL->Tri_limb[Tri1][2]]) {
      E2 = SO->EL->Tri_limb[Tri1][2];
   }else {
      fprintf (SUMA_STDERR,"Error %s: No E2 found.\n", FuncName);
      SUMA_RETURN (NULL);
   }

   Visited = (SUMA_Boolean *) SUMA_calloc (SO->N_FaceSet, sizeof(SUMA_Boolean));
   tPath1 = (int *) SUMA_calloc (SO->N_FaceSet, sizeof(int));
   if (!Visited || !tPath1) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      if (Visited) SUMA_free(Visited);
      if (tPath2) SUMA_free(tPath1); 
      SUMA_RETURN (NULL);
   }
   
   N_tPath1 = 0;
   if (!SUMA_FromIntEdgeToIntEdge (Tri1, E1, E2, SO->EL, SPI, Ny, Visited, &d1, dmax, tPath1, &N_tPath1)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_FromIntEdgeToIntEdge.\n", FuncName);
      if (Visited) SUMA_free(Visited);
      if (tPath2) SUMA_free(tPath1);      
      SUMA_RETURN (NULL);
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: Found a distance of %f.\n\n\n", FuncName, d1);
   }
   
   /* Now try going in the other direction, E2->E1 */
   cnt = E2;
   E2 = E1;
   E1 = cnt;
   
   /* reset the values of Visited */
   for (cnt=0; cnt < SO->N_FaceSet; ++cnt) if (Visited[cnt]) Visited[cnt] = NOPE;
   
   tPath2 = (int *) SUMA_calloc (SO->N_FaceSet, sizeof(int));
   if (!Visited || !tPath2) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      if (Visited) SUMA_free(Visited);
      if (tPath1) SUMA_free(tPath1);
      if (tPath2) SUMA_free(tPath2);
      SUMA_RETURN (NULL);
   }

   N_tPath2 = 0;
   if (!SUMA_FromIntEdgeToIntEdge (Tri1, E1, E2, SO->EL, SPI, Ny, Visited, &d2, dmax, tPath2, &N_tPath2)) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_FromIntEdgeToIntEdge.\n", FuncName);
      if (Visited) SUMA_free(Visited);
      if (tPath1) SUMA_free(tPath1);
      if (tPath2) SUMA_free(tPath2);
      SUMA_RETURN (NULL);
   }
   
   if (Visited) SUMA_free(Visited);
   
   if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: Found a distance of %f.\n", FuncName, d2);
   }
   
   if (d2 < d1) {
      *N_tPath = N_tPath2;
      *dinters = d2;
      if (tPath1) SUMA_free(tPath1);
      SUMA_RETURN (tPath2);
   } else {
      *dinters = d1;
      *N_tPath = N_tPath1;
      if (tPath2) SUMA_free(tPath2);
      SUMA_RETURN (tPath1);
   }
   
}

/*!
\brief This function moves from one intersected edge to the next until a certain node is encountered or a 
a certain distance is exceeded. By intersected edge, I mean an edge of the surface's mesh that was 
intersected by a plane.

ans = SUMA_FromIntEdgeToIntEdge (int Tri, int E1, int E2, SUMA_EDGE_LIST *EL, SPI, int Ny,
         Visited, float *d, float dmax, int *tPath, int *N_tPath);

\param Tri (int) index of triangle to start with (index into SO->FaceSetList)
\param E1 (int) index of edge in Tri to start from
\param E2 (int) index of edge in Tri to move in the direction of (Both E1 and E2  must be intersected edges)
\param EL (SUMA_EDGE_LIST *) pointer to the edge list structure, typically SO->EL
\param SPI (SUMA_SURF_PLANE_INTERSECT *) pointer to structure containing intersection of plane with surface
\param Ny (int) node index to stop at (index into SO->NodeList)
\param Visited (SUMA_Boolean *) pointer to vector (SO->N_FaceSet elements) that keeps track of triangles visited.
      This vector should be all NOPE when you first call this function. 
\param d (float *) pointer to total distance from first intersected edge E1 to the last edge that contains E2
\param dmax (float) maximum distance to go for before reversing and going in the other direction. Typically 
         this measure should be a bit larger than the distance of a Dijkstra path although you should never get a 
         distance that is larger than the Dijkstra path.
\param tPath (int *) vector of indices of triangles visited from first edge to last edge (make sure you allocate a bundle for tPath)
\param N_tPath (int *) number of elements in tPath (make sure you initialize this one to zero before you call this function )
\return ans (SUMA_Boolean) YUP/NOPE, for success/failure. 

         NOTE: This function is recursive.

\sa SUMA_Surf_Plane_Intersect
\sa SUMA_IntersectionStrip

*/
SUMA_Boolean SUMA_FromIntEdgeToIntEdge (int Tri, int E1, int E2, SUMA_EDGE_LIST *EL, SUMA_SURF_PLANE_INTERSECT *SPI, int Ny,
         SUMA_Boolean *Visited, float *d, float dmax, int *tPath, int *N_tPath)
{  static char FuncName[]={"SUMA_FromIntEdgeToIntEdge"};
   int Tri2 = 0, cnt, Incident[5], N_Incident;
   float dx, dy, dz;
   SUMA_Boolean Found, LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (Tri < 0 || E1 < 0 || E2 < 0) {
      fprintf (SUMA_STDERR, "Error %s: Tri (%d) or E1 (%d) or E2 (%d) is negative!\n", FuncName, Tri, E1, E2);
      SUMA_RETURN (NOPE);
   }
   
   
   dx = (SPI->IntersNodes[3*E2] - SPI->IntersNodes[3*E1]);
   dy = (SPI->IntersNodes[3*E2+1] - SPI->IntersNodes[3*E1+1]);
   dz = (SPI->IntersNodes[3*E2+2] - SPI->IntersNodes[3*E1+2]);
   if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: Entered - Tri %d, E1 %d [%d %d], E2 %d [%d %d]\n\tdx = %f dy = %f dz = %f\n", 
         FuncName, Tri, E1, EL->EL[E1][0], EL->EL[E1][1], E2, EL->EL[E2][0], EL->EL[E2][1], dx, dy, dz);
   }
   *d += sqrt( dx * dx + dy * dy + dz * dz);
   
   if (*d > dmax) {
      /* path already longer than Dijkstra path, no need to search further in this direction, get out with this d value */
      fprintf (SUMA_STDERR, "%s: Path longer than dmax. Returning.\n", FuncName);
      SUMA_RETURN (YUP);
   }
   
   if (EL->EL[E2][0] == Ny || EL->EL[E2][1] == Ny) {
      fprintf (SUMA_STDERR, "%s: Found Ny, d = %f\n", FuncName, *d);
      if (!Visited[Tri]) {
         /* add triangle to path */
         tPath[*N_tPath] = Tri;
         ++*N_tPath;
      }
      SUMA_RETURN (YUP);
   } else if (Visited[Tri]) {
      fprintf (SUMA_STDERR, "Error %s: Triangle %d already visited.\n",FuncName, Tri); 
      SUMA_RETURN (NOPE);
   }
     
   /* mark triangle as visited */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Marking triangle %d and adding %dth element to tPath.\n", FuncName, Tri, *N_tPath);
   Visited[Tri] = YUP;
   
   /* add triangle to path */
   tPath[*N_tPath] = Tri;
   ++*N_tPath;
   
   /* now get the second intersected triangle, incident to E2 */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Searching for triangles incident to E2 %d.\n", FuncName, E2);
   if (!SUMA_Get_Incident(EL->EL[E2][0], EL->EL[E2][1], EL, Incident, &N_Incident, 1, 0)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to get Incident triangles.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* find Tri2 such that Tri2 != Tri and Tri2 is an intersected triangle */
   cnt = 0;
   Found = NOPE;
   while (cnt < N_Incident && !Found) {
      if (SPI->isTriHit[Incident[cnt]] && Incident[cnt] != Tri && !Visited[Incident[cnt]]) {
         Found = YUP;
         Tri2 = Incident[cnt];
      }
      ++cnt;
   }
   
   if (!Found) {
      fprintf (SUMA_STDERR,"Error %s: Could not find next triangle.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   Tri = Tri2;
   E1 = E2;
   
   /* now find the new E2 */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Finding new E2.\n", FuncName);
   
   if (!SUMA_isSameEdge (EL, EL->Tri_limb[Tri][0], E1) && SPI->isEdgeInters[EL->Tri_limb[Tri][0]]) {
      E2 = EL->Tri_limb[Tri][0];
   }else if (!SUMA_isSameEdge (EL, EL->Tri_limb[Tri][1], E1) && SPI->isEdgeInters[EL->Tri_limb[Tri][1]]) {
      E2 = EL->Tri_limb[Tri][1];
   }else if (!SUMA_isSameEdge (EL, EL->Tri_limb[Tri][2], E1) && SPI->isEdgeInters[EL->Tri_limb[Tri][2]]) {
      E2 = EL->Tri_limb[Tri][2];
   }else {
      fprintf (SUMA_STDERR,"Error %s: No E2 found.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* call the same function again */
   if (!SUMA_FromIntEdgeToIntEdge (Tri, E1, E2, EL, SPI, Ny, Visited, d, dmax, tPath, N_tPath)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_FromIntEdgeToIntEdge.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   SUMA_RETURN (YUP);
}


SUMA_STRIP *SUMA_alloc_strip (char *parent_ID)
{
   static char FuncName[]={"SUMA_alloc_strip"};
   SUMA_STRIP *strp=NULL;
   
   SUMA_ENTRY;

   strp = (SUMA_STRIP *)SUMA_malloc(sizeof(SUMA_STRIP));
   strp->Edges = (DList *)SUMA_malloc(sizeof(DList));
   dlist_init(strp->Edges, NULL);
   strp->Nodes = (DList *)SUMA_malloc(sizeof(DList));
   dlist_init(strp->Nodes, NULL);
   strp->Triangles = (DList *)SUMA_malloc(sizeof(DList));
   dlist_init(strp->Triangles, NULL);
   /* not terribly efficient for storing points!*/
   strp->Points = (DList *)SUMA_malloc(sizeof(DList));
   dlist_init(strp->Points, SUMA_free);
   if (parent_ID) strp->parent_ID = SUMA_copy_string(parent_ID);
      
   SUMA_RETURN(strp);
}

void SUMA_free_strip (void *vstrp)
{
   static char FuncName[]={"SUMA_free_strip"};
   SUMA_STRIP *strp = (SUMA_STRIP *) vstrp;
   SUMA_ENTRY;
   
   if (strp) {
      if (strp->Edges) dlist_destroy(strp->Edges); SUMA_free(strp->Edges); strp->Edges = NULL;
      if (strp->Points) dlist_destroy(strp->Points); SUMA_free(strp->Points); strp->Points = NULL;
      if (strp->Nodes) dlist_destroy(strp->Nodes); SUMA_free(strp->Nodes); strp->Nodes = NULL;
      if (strp->Triangles) dlist_destroy(strp->Triangles); SUMA_free(strp->Triangles); strp->Triangles = NULL;
      if (strp->parent_ID) SUMA_free(strp->parent_ID); strp->parent_ID = NULL;
      SUMA_free(strp); strp = NULL;
   }
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_MergeStrips(DList *striplist, SUMA_SurfaceObject *SO, char *MergeBy)
{
   static char FuncName[]={"SUMA_MergeStrips"};
   DListElmt *list_elm=NULL, *listnext_elm=NULL, *elm=NULL;
   SUMA_STRIP *strip=NULL, *stripnext=NULL;
   int repeat = 0, stripnext_first_edge = -1;
   int stripnext_last_edge = -1, strip_first_edge = -1;
   int strip_last_edge=-1;
   SUMA_Boolean ans = YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!striplist || dlist_size(striplist) < 2) SUMA_RETURN(YUP); /* nothing to do here */
   
   SUMA_LHv("Have list of %d strips.\nMerging by %s\n", dlist_size(striplist), MergeBy);
   
   if (!strcmp(MergeBy,"edges")) { /* Merge by edges */
      do {
         if (!list_elm) { list_elm = listnext_elm = dlist_head(striplist); }
         else { list_elm = listnext_elm = dlist_next(list_elm); }
         strip = (SUMA_STRIP *)list_elm->data;
         strip_first_edge = (int)(dlist_head(strip->Edges))->data;
         strip_last_edge = (int)(dlist_tail(strip->Edges))->data;
         repeat = 0;
         do {
            listnext_elm = dlist_next(listnext_elm);
            stripnext = (SUMA_STRIP *)listnext_elm->data;
            stripnext_first_edge = (int)(dlist_head(stripnext->Edges))->data;
            stripnext_last_edge = (int)(dlist_tail(stripnext->Edges))->data;

            SUMA_LHv( "Strip from edges %d to %d\n"
                     "Stripnext  edges %d to %d\n", strip_first_edge, strip_last_edge,
                                                    stripnext_first_edge, stripnext_last_edge);
            /* any reason to merge?*/
            if (strip_last_edge == stripnext_first_edge || SUMA_whichTri_e (SO->EL, strip_last_edge, stripnext_first_edge, 1, !LocalHead)>=0) {
               SUMA_S_Note("Merging beginning of next to end of 1");
               repeat = 1;
               SUMA_MergeLists_Beg2_End1(stripnext->Edges, strip->Edges);
               SUMA_MergeLists_Beg2_End1(stripnext->Nodes, strip->Nodes);
               SUMA_MergeLists_Beg2_End1(stripnext->Points, strip->Points);
               SUMA_MergeLists_Beg2_End1(stripnext->Triangles, strip->Triangles);
             } else if (strip_last_edge == stripnext_last_edge || SUMA_whichTri_e (SO->EL, strip_last_edge, stripnext_last_edge, 1, !LocalHead)>=0) {
               SUMA_S_Note("Merging end of next to end of 1");
               repeat = 1;
               SUMA_MergeLists_End2_End1(stripnext->Edges, strip->Edges);
               SUMA_MergeLists_End2_End1(stripnext->Nodes, strip->Nodes);
               SUMA_MergeLists_End2_End1(stripnext->Points, strip->Points);
               SUMA_MergeLists_End2_End1(stripnext->Triangles, strip->Triangles);
            } else if (strip_first_edge == stripnext_last_edge || SUMA_whichTri_e (SO->EL, strip_first_edge, stripnext_last_edge, 1, !LocalHead)>=0) {
               SUMA_S_Note("Merging end of next to beginning of 1");
               repeat = 1;
               SUMA_MergeLists_End2_Beg1(stripnext->Edges, strip->Edges);
               SUMA_MergeLists_End2_Beg1(stripnext->Nodes, strip->Nodes);
               SUMA_MergeLists_End2_Beg1(stripnext->Points, strip->Points);
               SUMA_MergeLists_End2_Beg1(stripnext->Triangles, strip->Triangles);
            } else if (strip_first_edge == stripnext_first_edge || SUMA_whichTri_e (SO->EL, strip_first_edge, stripnext_first_edge, 1, !LocalHead)>=0) {
               SUMA_S_Note("Merging beginning of next to beginning of 1");
               repeat = 1;
               SUMA_MergeLists_Beg2_Beg1(stripnext->Edges, strip->Edges);
               SUMA_MergeLists_Beg2_Beg1(stripnext->Nodes, strip->Nodes);
               SUMA_MergeLists_Beg2_Beg1(stripnext->Points, strip->Points);
               SUMA_MergeLists_Beg2_Beg1(stripnext->Triangles, strip->Triangles);
            }
            if (repeat) { /* merger done, remove next strip */
               /* remove listnext_elm */
               dlist_remove(striplist, listnext_elm, (void*)&stripnext); SUMA_free_strip(stripnext);
            }
         } while (!repeat && listnext_elm != dlist_tail(striplist));

         SUMA_LHv("Now list of %d strips (repeat=%d).\n", dlist_size(striplist), repeat);

      } while (!repeat && dlist_next(list_elm) != dlist_tail(striplist));
   } else { /* Merge by other list*/
      SUMA_S_Err("Not ready for this, no sir.\n");
      SUMA_RETURN(NOPE);   
   }   
   
   if (repeat) ans = ans * SUMA_MergeStrips(striplist, SO, MergeBy);
   SUMA_RETURN(ans);
}

SUMA_Boolean SUMA_isEdgeStripClosed(DList *edgestrip, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isEdgeStripClosed"};
   int e0, e1;
   SUMA_ENTRY;
   
   if (!edgestrip || !SO || !SO->EL) {
      SUMA_S_Errv("Null input edgestrip %p or SO %p or SO->EL %p\n", edgestrip, SO, SO->EL);
      SUMA_RETURN(NOPE);
   }
   
   if (dlist_size(edgestrip) < 2) SUMA_RETURN(NOPE);
   
   e0 = (int)((dlist_head(edgestrip))->data);
   e1 = (int)((dlist_tail(edgestrip))->data);
   if (e0 >= SO->EL->N_EL || e1 >= SO->EL->N_EL) {
      SUMA_S_Errv("Edge %d or %d is >= than SO->EL->N_EL (%d)\n", e0, e1, SO->EL->N_EL);
      SUMA_RETURN(NOPE);
   }
   if (  SO->EL->EL[e0][0] == SO->EL->EL[e1][0]   ||
         SO->EL->EL[e0][1] == SO->EL->EL[e1][0]   ||
         SO->EL->EL[e0][0] == SO->EL->EL[e1][1]   ||
         SO->EL->EL[e0][1] == SO->EL->EL[e1][1]   ) SUMA_RETURN(YUP);
   
   
   SUMA_RETURN(NOPE);
}

/*!
   Given a SPI structure, return the various connected paths that are formed by the intersections
*/
DList * SUMA_SPI_to_EdgeStrips(SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI)
{
   static char FuncName[]={"SUMA_SPI_to_EdgeStrips"};
   int *Epath=NULL, N_Epath=-1;
   int Estart, Tstart, E0, Ec0, Ec1, Ec2, n0, n1, n2, i, j;
   int *Visited=NULL, Incident[5], N_Incident; 
   int VisOrder = 0, T0;
   float *p4=NULL, *p0=NULL, *p1=NULL, Un, Un2, U[3], U2[3];
   DList *striplist=NULL;
   SUMA_STRIP *one_strp=NULL;
   SUMA_Boolean *TVisited = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
    
   if (!SPI || !SO->EL) { SUMA_S_Errv("NULL spi (%p) || el (%p)\n", SPI, SO->EL); SUMA_RETURN(striplist);  }
   
   /* something to keep track of what has been visited */
   Visited = (int *) SUMA_calloc(SO->EL->N_EL, sizeof(int));
   Epath = (int *)SUMA_calloc(SO->EL->N_EL, sizeof(int));
   TVisited = (SUMA_Boolean *) SUMA_calloc(SO->N_FaceSet, sizeof(SUMA_Boolean));
   
   if (LocalHead){
      FILE *fout=fopen(FuncName,"w");
      SUMA_LH("In debug mode, writing AAA* files to disk...");
      SUMA_Show_SPI(SPI, fout, SO, "AAA", &(SUMAg_SVv[0]));
      fclose(fout);
   }
   
   /* Find an intersected edge for starters */
   do {  
      i = 0;
      Estart = -1; N_Epath = 0;
      do {
         if (!Visited[SPI->IntersEdges[i]]) Estart = SPI->IntersEdges[i];
         ++i; 
      } while (i<SPI->N_IntersEdges && Estart < 0);
      /* Put Estart in the Epath */
      E0 = Estart;
         while (E0 >= 0) { 
            SUMA_LHv("Now with E0=%d [nodes %d %d]\n   N_Epath = %d\n", E0, SO->EL->EL[E0][0], SO->EL->EL[E0][1], N_Epath);
            Epath[N_Epath] = E0; ++N_Epath;
            /* mark E0 as visited */
            Visited[E0] = VisOrder; ++VisOrder;
            /* Find a triangle incident to E0 and that is also intersected */
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Searching for triangles incident to E0 %d.\n", FuncName, E0);
            if (!SUMA_Get_Incident(SO->EL->EL[E0][0], SO->EL->EL[E0][1], SO->EL, Incident, &N_Incident, 0, 1)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to get Incident triangles.\n", FuncName);
               SUMA_FREE_DLIST(striplist);
               goto CLEANUP_RETURN;
            }
            if (N_Incident > 2) {
               SUMA_S_Err("Surface not 2 manifold. Will not proceed.\n");
               SUMA_FREE_DLIST(striplist);
               goto CLEANUP_RETURN;
            }
            T0 = -1;
            if (N_Incident) {
               if (LocalHead) {
                  if (N_Incident > 1) { SUMA_LHv("Have incident triangles %d and %d\n", Incident[0], Incident[1]);}
                  else { SUMA_LHv("Have one incident triangle %d\n", Incident[0]); }
               } 
               if (SPI->isTriHit[Incident[0]] && !TVisited[Incident[0]]) T0 = Incident[0];
               else if (N_Incident > 1 && SPI->isTriHit[Incident[1]] && !TVisited[Incident[1]]) T0 = Incident[1];
               if (T0 >= 0) {
                  TVisited[T0] = YUP;
                  if (E0 == Estart) {  
                     Tstart = T0; 
                     SUMA_LHv("Tstart = %d\n", Tstart);
                  } else {
                     SUMA_LHv("Marked triangle %d\n", T0);
                  }
               } else {
                  SUMA_LHv("End of journey at edge %d\n", E0);
               }
            }

            E0 = -1;
            if (T0>=0) { /* have gun, will travel, find next edge */
               /* find the other interesected edge of this triangle*/
               n0 = SO->FaceSetList[3*T0]; n1 = SO->FaceSetList[3*T0+1]; n2 = SO->FaceSetList[3*T0+2];   
               SUMA_LHv("Working triangle %d, nodes: %d %d %d\n", T0, n0, n1, n2);
               /* find the two intersected edges */
               Ec0 = SUMA_FindEdge (SO->EL, n0, n1); 
               Ec1 = SUMA_FindEdge (SO->EL, n0, n2); 
               Ec2 = SUMA_FindEdge (SO->EL, n1, n2); 
               if (!Visited[Ec0] && SPI->isEdgeInters[Ec0]) { E0 = Ec0; /* have a new candidate */}
               else if (!Visited[Ec1] && SPI->isEdgeInters[Ec1]) { E0 = Ec1; /* have a new candidate */} 
               else if (!Visited[Ec2] && SPI->isEdgeInters[Ec2]) { E0 = Ec2; /* have a new candidate */} 
               else {  /* no where to go */ }
            }
         }  

      if (N_Epath > 0) {
         if (!striplist) {
            striplist = (DList*)SUMA_malloc(sizeof(DList));
            dlist_init(striplist, SUMA_free_strip);
         }
         one_strp = SUMA_alloc_strip(SO->idcode_str);
         /* now add edge sequence to this list */ 
         for (i=0; i<N_Epath; ++i) {
            dlist_ins_next(one_strp->Edges, dlist_tail(one_strp->Edges), (void *)Epath[i]);
            /* here you can add the Points (xyz of intersections), if you like */
            p4 = (float *)SUMA_malloc(sizeof(float)*4); 
            p4[0] = SPI->IntersNodes[3*Epath[i]];  p4[1] = SPI->IntersNodes[3*Epath[i]+1]; p4[2] = SPI->IntersNodes[3*Epath[i]+2];    
            /* Store the position of the point as a fraction of the edge length from the first node forming edge */
            n0 = SO->EL->EL[Epath[i]][0]; n1 = SO->EL->EL[Epath[i]][1];
            p0 = &(SO->NodeList[3*n0]); p1 = &(SO->NodeList[3*n1]);
            SUMA_UNIT_VEC(p0, p1, U, Un);
            SUMA_UNIT_VEC(p0, p4, U2, Un2);
            p4[3] = Un2/Un;   /* Hide it here. Forgive me Lord for I have sinned */
            dlist_ins_next(one_strp->Points, dlist_tail(one_strp->Points), (void *)p4);
         }
         /* add the stip to the striplist */
         dlist_ins_next(striplist, dlist_tail(striplist), (void *)one_strp);
         one_strp = NULL; /* do not touch it anymore */
      } 
   } while (Estart >=0);
   
   /* here is where you combine all the strips */
   if (!SUMA_MergeStrips(striplist, SO, "edges")) {
      SUMA_S_Err("An error occurred while merging strips!\n");
      SUMA_FREE_DLIST(striplist);
      goto CLEANUP_RETURN;
   }
   
   if (LocalHead) {  /* new, using list of strips */
      SUMA_display_edge_striplist(striplist, &(SUMAg_SVv[0]), SO, "ShowEdges, ShowConnectedPoints, ShowPoints");
   }
   
   CLEANUP_RETURN:
   if (Epath) SUMA_free(Epath); Epath = NULL;
   if (Visited) SUMA_free(Visited); Visited = NULL;
   if (TVisited) SUMA_free(TVisited); TVisited = NULL;
   
   SUMA_RETURN(striplist);
}    

/*!
\brief Converts a series of connected nodes into a series of connected triangles that were intersected by 
the plane. 
There is no guarantee that two nodes that belong to triangles intersected by the plane and part of the shortest path
(as returned by SUMA_Dijkstra) for an edge that belongs to a triangle intersected by the plane. See labbook NIH-2 page 
158 for sketches illustrating this point. So the strip of triangles that you will get back may have holes in it since 
it only allows intersected triangles to be members of the path. 

   tPath = SUMA_NodePath_to_TriPath_Inters (SO, SPI, int *nPath, int N_nPath, int *N_tPath)
   
   \param SO (SUMA_SurfaceObject *) structure containing surface object
   \param SPI (SUMA_SURF_PLANE_INTERSECT *) surface plane intersection structure
   \param nPath (int *) vector containing the shortest path defined by its nodes (output of SUMA_Dijkstra)
   \param N_nPath (int) number of elements in nPath
   \param N_tPath (int *)pointer to number of elements returned in tPath
   \return tPath (int *) vector of *N_tPath indices of triangles that form a strip along the shortest path.
   
   \sa SUMA_Dijkstra
   \sa SUMA_NodePath_to_EdgePath
   \sa SUMA_Surf_Plane_Intersect
   
   \sa labbook NIH-2 pages 158, and 159 for MissingTriangles
*/

int *SUMA_NodePath_to_TriPath_Inters ( SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI, int *nPath, int N_nPath, int *N_tPath)
{
   static char FuncName[]={"SUMA_NodePath_to_TriPath_Inters"};
   int *tPath = NULL, e, i, N_nc, nc[3], N_HostTri, E, j, 
      HostTri, PrevTri, k, N1[2], N2[2], cnt, MissTri = 0, candidate;
   SUMA_Boolean Found, LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   tPath = (int *) SUMA_calloc(2*N_nPath, sizeof(int));
   if (!tPath) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   *N_tPath = 0;
   for (i=0; i < N_nPath - 1; ++i) {
      /* find the edge corresponding to two consecutive nodes in the path */
      E = SUMA_FindEdge (SO->EL, nPath[i], nPath[i+1]);
      if (E < 0) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_FindEdge.\n", FuncName);
         SUMA_free(tPath);
         SUMA_RETURN(NULL);
      }
      /* find the triangles containing E and intersected by the plane */
      N_HostTri = SO->EL->ELps[E][2]; /* number of hosting triangles */
      if (N_HostTri > 2) {
         fprintf (SUMA_STDERR, "Warning %s: Surface is not a surface, Edge %d has more than %d hosting triangles.\n", FuncName, E, N_HostTri);
      }
      candidate = 0;
      /* search for a hosting triangle that was intersected */
      for (j=0; j < N_HostTri; ++j) {
         HostTri = SO->EL->ELps[E+j][1];
         if (SPI->isTriHit[HostTri]) { /* a candidate for adding to the path */
            ++candidate;
            if (*N_tPath > 2*N_nPath) {
               fprintf (SUMA_STDERR, "Error %s: N_tPath = %d > %d allocated.\n", FuncName, *N_tPath, 2*N_nPath);
            }  
            #if 1
            /* This block is an attempt to get All triangles intersected by plane AND having a node as part of the shortest path.
            It does not work well, probably because some of the functions called need fixing... */
            if (*N_tPath == 0) { /* if that is the first triangle in the path, add it without much fuss */
               tPath[*N_tPath] = HostTri; /* hosting triangle index */
               ++ (*N_tPath);
            } else { /* make sure there is continuation along edges */
               PrevTri = tPath[*N_tPath - 1];
               N_nc = SUMA_isTriLinked (&(SO->FaceSetList[3*PrevTri]), &(SO->FaceSetList[3*HostTri]), nc);
               if (!N_nc) {
                  fprintf (SUMA_STDERR, "Warning %s: Triangles %d and %d are not linked.\nAdding triangle %d anyway.\n", 
                     FuncName, PrevTri, HostTri, HostTri);
                  /* add triangle, anyway */
                  tPath[*N_tPath] = HostTri; 
                  ++ (*N_tPath);
               }else if (N_nc == 1) {
                  /* must fill triangle gap get the triangle with the common node and common edges*/
                  /* first, find remaining nodes in PrevTri  */
                  e = 0;
                  for (k=0; k <3; ++k) {
                     if (SO->FaceSetList[3*PrevTri+k] != nc[0]) {
                        N1[e] = SO->FaceSetList[3*PrevTri+k]; ++e;
                     }
                  }
                  /* then find remaining nodes in HostTri  */
                  e = 0;
                  for (k=0; k <3; ++k) {
                     if (SO->FaceSetList[3*HostTri+k] != nc[0]) {
                        N2[e] = SO->FaceSetList[3*HostTri+k]; ++e;
                     }
                  }
                  /* find a triangle that has either one of the following node combinations, in addition to nc[0]:
                  N1[0], N2[0] or N1[0], N2[1] or N1[1], N2[0] or N1[1], N2[1] */
                  Found = NOPE;
                  cnt = 0;
                  while (!Found && cnt < 4) {
                     switch (cnt) {
                        case 0:
                           MissTri = SUMA_whichTri (SO->EL, nc[0], N1[0], N2[0], 1);
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: looking for triangle with nodes %d and %d... Tri = %d\n", 
                                 FuncName, N1[0], N2[0], MissTri);
                           break;
                        case 1:
                           MissTri = SUMA_whichTri (SO->EL, nc[0], N1[0], N2[1], 1);
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: looking for triangle with nodes %d and %d... Tri = %d\n", 
                                 FuncName, N1[0], N2[1], MissTri);
                           break;
                        case 2:
                           MissTri = SUMA_whichTri (SO->EL, nc[0], N1[1], N2[0], 1);
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: looking for triangle with nodes %d and %d... Tri = %d\n", 
                                 FuncName, N1[1], N2[0], MissTri);
                           break;
                        case 3:
                           MissTri = SUMA_whichTri (SO->EL, nc[0], N1[1], N2[1], 1);
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: looking for triangle with nodes %d and %d... Tri = %d\n", 
                                 FuncName, N1[1], N2[1], MissTri);
                           break;
                     }
                     if (MissTri >= 0) {
                        Found = YUP;
                     }
                     ++cnt;
                  }
                  if (!Found) {
                     fprintf (SUMA_STDERR, "Warning %s: Failed to find missing triangle.\n", FuncName);
                     tPath[*N_tPath] = HostTri; 
                     ++ (*N_tPath);
                  }else {
                     /* add the missing triangle first, then the HostTri */
                     tPath[*N_tPath] = MissTri; 
                     ++ (*N_tPath);
                     tPath[*N_tPath] = HostTri; 
                     ++ (*N_tPath);
                  }
               }else if (N_nc == 2) {
                  /* Triangles share an edge so no problem, insert the new triangle in the path */
                  tPath[*N_tPath] = HostTri; 
                  ++ (*N_tPath);
               }else {
                  fprintf (SUMA_STDERR, "Error %s: Triangles %d and %d are identical.\n", FuncName, PrevTri, HostTri);
                  SUMA_free(tPath);
                  SUMA_RETURN(NULL);
               }
            }
            #else 
               tPath[*N_tPath] = HostTri; 
               ++ (*N_tPath);
            #endif   
         }
      }
      if (!candidate) {
         fprintf (SUMA_STDERR, "\aWarning %s: Nodes %d and %d of edge %d had no intersected hosting triangle.\n", FuncName, nPath[i], nPath[i+1], E);
         
      }
   }
   
   SUMA_RETURN (tPath);   
}
/*!
\brief Converts a series of connected nodes into a series of connected triangles that belong to a branch.
The function fails at times, picking the long instead of the short path but it is left here in case I 
need it in the future.
 
   tPath = SUMA_NodePath_to_TriPath_Inters_OLD (SO, Bv, Path, N_Path, N_Tri);
   \param SO (SUMA_SurfaceObject *) Pointer to surface object
   \param Bv (SUMA_TRI_BRANCH*) Pointer to tiangle branch containing nodes in path.
   \param Path (int *) vector of node indices forming a path.
                       Sequential nodes in Path must be connected on the surface mesh.
   \param N_Path (int) number of nodes in the path 
   \param N_Tri (int *) pointer to integer that will contain the number of triangles in the path
                        0 if function fails.
   \return tPath (int *) pointer to vector containing indices of triangles forming the path. 
                        The indices are into SO->FaceSetList.
                        NULL if trouble is encountered.
                        
   \sa SUMA_NodePath_to_EdgePath
   \sa Path I in NIH-2, labbook page 153
*/
int *SUMA_NodePath_to_TriPath_Inters_OLD (SUMA_SurfaceObject *SO, SUMA_TRI_BRANCH *Bv, int *Path, int N_Path, int *N_Tri)
{
   static char FuncName[]={"SUMA_NodePath_to_TriPath_Inters_OLD"};
   int *tPath = NULL, ilist, i0, Tri, eTri, EdgeBuf, Tri0, Tri1, Direction, i1, loc2f, iDirSet;
   SUMA_Boolean LocalHead = NOPE, Found = NOPE;
   
   SUMA_ENTRY;
 
 
  *N_Tri = 0;
   tPath = (int *) SUMA_calloc(Bv->N_list+1, sizeof(int));
   if (!tPath) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* the first triangle should contain the first node in the path */
      i0 = Path[0];
      Tri0 = Bv->list[0];
      if (SO->FaceSetList[3*Tri0] != i0 && SO->FaceSetList[3*Tri0+1] != i0 && SO->FaceSetList[3*Tri0+2] != i0) {
         fprintf (SUMA_STDERR, "Error %s: Did not find node %d in first triangle in branch.\n", FuncName, i0);
         SUMA_free(tPath);
         *N_Tri = 0;
         SUMA_RETURN (NULL);  
      }
   
   
   /* initiliaze first node results and look for the second node */
   tPath[0] = Tri0;
   *N_Tri = 1;
   Found = NOPE;
   ilist = 0;

   if (LocalHead)   fprintf(SUMA_STDERR, "%s: Going forward looking for third node\n", FuncName);
   if (N_Path > 2) {
      iDirSet = 2; /* search for third node in list, that helps determine the direction more reliably */
   }else {
      iDirSet = 1; /* settle for the second node */
   }
   
   ilist = 1;
   while (!Found && ilist < Bv->N_list) {
      tPath[*N_Tri] = Bv->list[ilist];
      if (SO->FaceSetList[3*Bv->list[ilist]] == Path[iDirSet] || 
         SO->FaceSetList[3*Bv->list[ilist]+1] == Path[iDirSet] ||
         SO->FaceSetList[3*Bv->list[ilist]+2] == Path[iDirSet]) {
            Found = YUP;
      }
      ++(*N_Tri);
      ++ilist;      
   }
    
   if (!Found) {
      fprintf (SUMA_STDERR, "Error %s: Did not find next node %d in branch.\n", FuncName, Path[iDirSet]);
      SUMA_free(tPath);
      *N_Tri = 0;
      SUMA_RETURN (NULL); 
   }
  
   loc2f = *N_Tri; /* number of steps to find second node in the forward direction */

   /* do the same in the backwards direction */
   tPath[0] = Tri0;
   *N_Tri = 1;
   Found = NOPE;
   ilist = 0;
   
   if (LocalHead) fprintf(SUMA_STDERR, "%s: Going backwards looking for third node\n", FuncName);
   ilist = Bv->N_list - 1;
   while (!Found && ilist >=  0) {
      tPath[*N_Tri] = Bv->list[ilist];
      if (LocalHead) fprintf(SUMA_STDERR, "%s: trying triangle %d for node %d.\n", FuncName, Bv->list[ilist], Path[N_Path-1]);
      if (SO->FaceSetList[3*Bv->list[ilist]] == Path[iDirSet] || 
         SO->FaceSetList[3*Bv->list[ilist]+1] == Path[iDirSet] ||
         SO->FaceSetList[3*Bv->list[ilist]+2] == Path[iDirSet]) {
            Found = YUP;
      }
      ++(*N_Tri);
      --ilist;      
   }
   
   if (*N_Tri < loc2f) { 
      /* go backwards, shorter. This is based on triangle count, 
         it would be more accurate based on distance of intersected edge */
      Direction = -1;
   } else Direction = 1;
   
   /* now do the whole thing */
   
   tPath[0] = Tri0;
   *N_Tri = 1;
   Found = NOPE;
   ilist = 0;
   if (Direction == 1) { /* move forward until you reach the last node */
     if (LocalHead)   fprintf(SUMA_STDERR, "%s: Going forward, final pass \n", FuncName);
     ilist = 1;
      while (!Found && ilist < Bv->N_list) {
         tPath[*N_Tri] = Bv->list[ilist];
         if (SO->FaceSetList[3*Bv->list[ilist]] == Path[N_Path-1] || 
            SO->FaceSetList[3*Bv->list[ilist]+1] == Path[N_Path-1] ||
            SO->FaceSetList[3*Bv->list[ilist]+2] == Path[N_Path-1]) {
               Found = YUP;
         }
         ++(*N_Tri);
         ++ilist;      
      }
   } else { /* move backwards */
      if (LocalHead) fprintf(SUMA_STDERR, "%s: Going backwards, final pass \n", FuncName);
      ilist = Bv->N_list - 1;
      while (!Found && ilist >=  0) {
         tPath[*N_Tri] = Bv->list[ilist];
         if (LocalHead) fprintf(SUMA_STDERR, "%s: trying triangle %d for node %d.\n", FuncName, Bv->list[ilist], Path[N_Path-1]);
         if (SO->FaceSetList[3*Bv->list[ilist]] == Path[N_Path-1] || 
            SO->FaceSetList[3*Bv->list[ilist]+1] == Path[N_Path-1] ||
            SO->FaceSetList[3*Bv->list[ilist]+2] == Path[N_Path-1]) {
               Found = YUP;
         }
         ++(*N_Tri);
         --ilist;      
      }
      
   }   

   if (!Found) {
      fprintf (SUMA_STDERR, "Error %s: Path not completed.\n", FuncName);
      SUMA_free(tPath);
      *N_Tri = 0;
      SUMA_RETURN (NULL);
   }else {
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: Path is %d triangles long:\n", FuncName, *N_Tri); 
         for (ilist=0; ilist< *N_Tri; ++ilist) {
            fprintf (SUMA_STDERR,"t%d\t", tPath[ilist]);
         }
         fprintf (SUMA_STDERR,"\n");
      }
   }
   SUMA_RETURN (tPath);
}   


SUMA_Boolean SUMA_CenterOfSphere(double *p1, double *p2, double *p3, double *p4, double *c)
{
   static char FuncName[]={"SUMA_CenterOfSphere"};
   double pp1[3], pp2[3], pp3[3], pp4[3];
   THD_dmat33  mat;
   double n1, n2, n3, d3;
   double x2Mx1, x3Mx1, x4Mx1;
   double y2My1, y3My1, y4My1;
   double z2Mz1, z3Mz1, z4Mz1;
   double spp1, spp2, spp3, spp4;
   int i = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* calculate doubles */
   for (i=0; i<3; ++i) {
      pp1[i] = p1[i]*p1[i];
      pp2[i] = p2[i]*p2[i];
      pp3[i] = p3[i]*p3[i];
      pp4[i] = p4[i]*p4[i];
   }
   spp1 = pp1[0] + pp1[1] + pp1[2];
   spp2 = pp2[0] + pp2[1] + pp2[2];
   spp3 = pp3[0] + pp3[1] + pp3[2];
   spp4 = pp4[0] + pp4[1] + pp4[2];
   
   /* calculate differences */
   x2Mx1 = p2[0] - p1[0];
   x3Mx1 = p3[0] - p1[0];
   x4Mx1 = p4[0] - p1[0];
   y2My1 = p2[1] - p1[1];
   y3My1 = p3[1] - p1[1];
   y4My1 = p4[1] - p1[1];
   z2Mz1 = p2[2] - p1[2];
   z3Mz1 = p3[2] - p1[2];
   z4Mz1 = p4[2] - p1[2];
   
   
   /* calculate N1 */
   mat.mat[0][0] = spp2 - (spp1);
   mat.mat[1][0] = spp3 - (spp1);
   mat.mat[2][0] = spp4 - (spp1);
   mat.mat[0][1] = y2My1;
   mat.mat[1][1] = y3My1;
   mat.mat[2][1] = y4My1;
   mat.mat[0][2] = z2Mz1;
   mat.mat[1][2] = z3Mz1;
   mat.mat[2][2] = z4Mz1;
   n1 = MAT_DET(mat);
   SUMA_LHv("n1=%f\n", n1);
   
   /* calculate N2 */
   mat.mat[0][0] = x2Mx1;
   mat.mat[1][0] = x3Mx1;
   mat.mat[2][0] = x4Mx1;
   mat.mat[0][1] = spp2 - (spp1);
   mat.mat[1][1] = spp3 - (spp1);
   mat.mat[2][1] = spp4 - (spp1);
   mat.mat[0][2] = z2Mz1;
   mat.mat[1][2] = z3Mz1;
   mat.mat[2][2] = z4Mz1;
   n2 = MAT_DET(mat);
   SUMA_LHv("n2=%f\n", n2);
   
   /* calculate N3 */
   mat.mat[0][0] = x2Mx1;
   mat.mat[1][0] = x3Mx1;
   mat.mat[2][0] = x4Mx1;
   mat.mat[0][1] = y2My1;
   mat.mat[1][1] = y3My1;
   mat.mat[2][1] = y4My1;
   mat.mat[0][2] = spp2 - (spp1);
   mat.mat[1][2] = spp3 - (spp1);
   mat.mat[2][2] = spp4 - (spp1);
   n3 = MAT_DET(mat);
   SUMA_LHv("n3=%f\n", n3);
   
   /* calculate D3 */
   mat.mat[0][0] = x2Mx1;
   mat.mat[1][0] = x3Mx1;
   mat.mat[2][0] = x4Mx1;
   mat.mat[0][1] = y2My1;
   mat.mat[1][1] = y3My1;
   mat.mat[2][1] = y4My1;
   mat.mat[0][2] = z2Mz1;
   mat.mat[1][2] = z3Mz1;
   mat.mat[2][2] = z4Mz1;
   d3 = MAT_DET(mat);
   SUMA_LHv("d3=%f\n", d3);
   
   if (d3) {
      /* Center */
      c[0] = n1/(2.0*d3);
      c[1] = n2/(2.0*d3);
      c[2] = n3/(2.0*d3);
      SUMA_LHv("c=[%f, %f, %f]\n", c[0], c[1], c[2]);

      SUMA_RETURN(YUP);
   } else {
      c[0] = 1.0; c[1] = -2.0; c[2] = 3.0;
      SUMA_LH("0 denominator, solution impossibile\n");
      SUMA_RETURN(NOPE);
   }
}

extern int *z_rand_order(int bot, int top, long int seed) ;

SUMA_Boolean SUMA_GetCenterOfSphereSurface(SUMA_SurfaceObject *SO, int Nquads, double *cs, double *cm)
{
   static char FuncName[]={"SUMA_GetCenterOfSphereSurface"};
   double  p1[3], p2[3], p3[3], p4[3], c[3];
   double *cx=NULL, *cy=NULL, *cz=NULL;
   int ii, nn[4], jj, Ns, nmax;
   int *ir = NULL, cnt;
   SUMA_Boolean LocalHead=NOPE;

   SUMA_ENTRY;

   c[0] = -11111.0; c[1] = -22222.0; c[2] = -33333.0;
   cs[0] = cs[1] = cs[2] = 0.0;
   if (!(ir = z_rand_order(0, SO->N_Node-1, 111111311))) {
      SUMA_S_Err("Failed to get randomized list");
      SUMA_RETURN(NOPE);
   } else {
      /* minimum number of distinct quads */
      nmax = (SO->N_Node-1)/4;
      if (Nquads < 1) Ns = SUMA_MIN_PAIR(100, nmax);
      else Ns = SUMA_MIN_PAIR(Nquads, nmax);
      cx = (double*)SUMA_malloc(sizeof(double)*Ns);
      cy = (double*)SUMA_malloc(sizeof(double)*Ns);
      cz = (double*)SUMA_malloc(sizeof(double)*Ns);
      cs[0] = cs[1] = cs[2] = 0.0;
      cnt = 0;
      for (jj=0; jj<Ns; ++jj) {
         nn[0] = ir[4*jj+0]; 
         nn[1] = ir[4*jj+1]; 
         nn[2] = ir[4*jj+2]; 
         nn[3] = ir[4*jj+3];
         for (ii=0; ii<3; ++ii) {
            p1[ii] = (double)SO->NodeList[3*nn[0]+ii];
            p2[ii] = (double)SO->NodeList[3*nn[1]+ii];
            p3[ii] = (double)SO->NodeList[3*nn[2]+ii];
            p4[ii] = (double)SO->NodeList[3*nn[3]+ii];
         }
         /* Find a nice center */
         if (SUMA_CenterOfSphere( p1, p2, p3, p4, c )) {
            SUMA_LHv(  "Center estimate %d:\n"
                           "  [%f   %f   %f]\n", jj, c[0], c[1], c[2]);
            for (ii=0; ii<3;++ii) {
               cs[ii] += c[ii];
            }
            cx[cnt] = c[0];
            cy[cnt] = c[1];
            cz[cnt] = c[2];
            ++cnt;
         }
      }
      Ns = cnt;
      for (ii=0; ii<3;++ii) {
         cs[ii] /= (double)Ns;
      }
      /* sort coords */
      qsort(cx, Ns, sizeof(double), (int(*) (const void *, const void *)) SUMA_compare_double);
      qsort(cy, Ns, sizeof(double), (int(*) (const void *, const void *)) SUMA_compare_double);
      qsort(cz, Ns, sizeof(double), (int(*) (const void *, const void *)) SUMA_compare_double);
      cm[0] = cx[Ns/2];
      cm[1] = cy[Ns/2];
      cm[2] = cz[Ns/2];
      SUMA_LHv(  "Average of %d center estimates:\n"
                 "  [%f   %f   %f]\n"
                 "Median of %d center estimates:\n"
                 "  [%f   %f   %f]\n"
                     , Ns, cs[0], cs[1], cs[2],
                       Ns, cm[0], cm[1], cm[2]); 
      SUMA_free(cx); SUMA_free(cy); SUMA_free(cz); cx = cy = cz = NULL;
      if (ir) SUMA_free(ir); ir = NULL;
   }
   SUMA_RETURN(YUP);
}

/*  Average segment length in SO2 / S01 */
float *SUMA_SegmentDistortion (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2)
{
   static char FuncName[]={"SUMA_SegmentDistortion"};
   float *SegDist=NULL, *p1_1, *p1_2, *p2_1, *p2_2, d_1, d_2;
   int i, k;
   
   SUMA_ENTRY;
   
   if (!SO1 || !SO2) { SUMA_S_Err("NULL input"); SUMA_RETURN(SegDist); }
   if (SO1->N_Node != SO2->N_Node) { SUMA_S_Err("input mismatch"); SUMA_RETURN(SegDist); }
   
   if (!SO1->FN) { SUMA_SurfaceMetrics(SO1, "EdgeList", NULL); } 
   if (!SO2->FN) { SUMA_SurfaceMetrics(SO2, "EdgeList", NULL); } 
   if (!SO1->FN || !SO2->FN) { SUMA_S_Err("Failed to calculate FN"); SUMA_RETURN(SegDist); }
   
   SegDist = (float *)SUMA_calloc(SO1->N_Node, sizeof(float));
   
   if (SO1 == SO2) {
      for (i=0; i<SO1->N_Node; ++i) SegDist[i] = 1.0;
      SUMA_RETURN(SegDist); 
   }
   
   for (i=0; i<SO1->N_Node; ++i) {
      p1_1 = &(SO1->NodeList[3*i]);
      p1_2 = &(SO2->NodeList[3*i]);
      SegDist[i] = 0.0;
      for (k=0; k<SO1->FN->N_Neighb[i]; ++k) {
         p2_1 = &(SO1->NodeList[3*k]);      
         p2_2 = &(SO2->NodeList[3*k]);
         SUMA_SEG_LENGTH_SQ (p1_1, p2_1, d_1);
         SUMA_SEG_LENGTH_SQ (p1_2, p2_2, d_2);
         if (d_1) SegDist[i] += sqrt(d_2 / d_1);      
      }
      if (SO1->FN->N_Neighb[i]) SegDist[i] /= SO1->FN->N_Neighb[i];   
   }
   
   SUMA_RETURN(SegDist);
}
 
/*!
   \brief, a function for approximate but rapid delineation of the set of nodes  
   within a distance form a node on the surface.
   The approximation works via the spherical version of the surface and assumes
   that the distortions have a low spatial frequency. 
   The function must be called with an 'cleanup mode' (cent = -1 ) at the end.
   \sa SUMA_APPROXNEIGHBORS
*/
   
int SUMA_ApproxNeighbors ( SUMA_SurfaceObject *SO,
                           SUMA_SurfaceObject *SOf,  /* the spherical (or someday flat) version of SO */ 
                           int cent,      /* the central node*/
                           float dnei,     /* the search distance, along the surface from node cent */
                           byte *nmask     /* to contain the nodes within rad from Cent */)
{
   static char FuncName[]={"SUMA_ApproxNeighbors"};
   int N_nmask=-1;
   static float *SegDist = NULL;
   static int *mask_record=NULL;
   static int N_nmask_last = -2;
   int i;
   float dnei_sp, alph, rsearch;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (cent == -1) { 
      SUMA_LH("Cleanup mode");
      N_nmask_last = -2;
      if (SegDist) SUMA_free(SegDist); SegDist = NULL;
      if (mask_record) SUMA_free(mask_record); mask_record = NULL;
      SUMA_RETURN(0);
   }
    
   if (!SO) { SUMA_S_Err("NULL SO"); SUMA_RETURN(N_nmask); }
   
   /* Set the flat surface */
   if (!SOf) {
      if (SO->isSphere) SOf = SO;
   }
   if (!SOf) { SUMA_S_Err("Have no flat surface to work with"); SUMA_RETURN(N_nmask); }
   
   /* is this the first time this is called */
   if (N_nmask_last == -2) {
      SUMA_LH("Initializing");
      if (SegDist || mask_record) { SUMA_S_Err("This is not appreciated."); SUMA_RETURN(N_nmask); }
      SegDist = SUMA_SegmentDistortion(SO, SOf); /* this function should return a vector of 1s if SO == SOf */
      mask_record = (int *)SUMA_calloc(SO->N_Node,sizeof(int));
      N_nmask_last = -1;
   }
   
   if (!SegDist || !mask_record) { SUMA_S_Errv("Should not happen here (%p %p) (have you initialized?)\n", SegDist, mask_record); SUMA_RETURN(N_nmask); }
   if (!nmask) { SUMA_S_Err("NULL nmask"); SUMA_RETURN(N_nmask); }
   
   /* Now cleanup the previous record */
   for (i=0; i<N_nmask_last; ++i) { nmask[mask_record[i]] = 0; }

   /* Calculate the Equivalent neighborhood distance on the spherical version of the surface */
   dnei_sp = dnei * SegDist[cent];  
   alph = dnei_sp / SOf->SphereRadius;
   rsearch = sin(alph/2.0)*2.0*SOf->SphereRadius;
   /* Nodes that fall within a sphere of radius rsearch and centered on cent are within dnei_sp on
   the sphere. We approximate that they would be within dnei mm from cent on the original surface */
   /* N_nmask = SUMA_nodesinsphere2( SOf->NodeList, SOf->N_Node,
                                  &(SOf->NodeList[3*cent]), rsearch, 
                                  mask_record, NULL ); */
   SUMA_NODESINSPHERE2( SOf->NodeList, SOf->N_Node, &(SOf->NodeList[3*cent]), rsearch, mask_record, N_nmask);
   
   SUMA_LHv("Have %d nodes in mask\n", N_nmask);
   for (i=0; i<N_nmask; ++i) nmask[mask_record[i]] = 1;
   if (LocalHead && cent == 0) {
      int ccnt=0;
      char oname[100];
      FILE *fid=NULL;
      sprintf(oname, "neighb_node%d.1D.dset", cent);
      fid=fopen(oname,"w");
      fprintf(fid, "#neighbors of node %d per function ApproxNeighbors\n", cent);
      for (i=0; i<N_nmask; ++i) fprintf(fid, "%d\n", mask_record[i]); 
      fclose(fid); fid = NULL;
      for (i=0; i<SO->N_Node; ++i) { if (nmask[i]) ++ccnt; }
      SUMA_LHv("Verified final mask has %d nodes (should be = %d)\n", ccnt, N_nmask); 
   }
   N_nmask_last = N_nmask;                                
   
   SUMA_RETURN(N_nmask);
}

/*!
   create a dataset of random values
   N_Node (int) number of nodes on surface
   nc     (int) number of columns (sub-bricks)
   seed   (unsigned int) if 0 then seed = 123456 
   scale  (float) if !0.0 then use it to scale output.
                  unscaled output is 0 to 1
   norm   (byte) if 1 then draw samples from a Normal distribution
*/  
SUMA_DSET *SUMA_RandomDset(int N_Node, int nc, unsigned int seed, float scale, byte norm) 
{
   static char FuncName[]={"SUMA_RandomDset"};
   SUMA_DSET *dset = NULL;
   float *fr=NULL;
   int i;

   SUMA_ENTRY;
   
   if (seed == 0) seed = 123456; /* don't change that one */

   if (!(fr = (float *)SUMA_malloc(sizeof(float)*N_Node*nc))) {SUMA_S_Crit("Failed to mallocate"); SUMA_RETURN(NULL); }
   srand(seed);
   if (norm) {
      for (i=0; i<N_Node*nc; ++i) { 
         fr[i] = (float)(SUMA_GRAN(0.,1.)); /* use a gaussian baby */
      }   
   } else {
      for (i=0; i<N_Node*nc; ++i) { 
         fr[i] = (float)(((double)rand()/(double)RAND_MAX)); 
      }   
   }

   if (scale) for (i=0; i<N_Node*nc; ++i) fr[i] *= scale;

   if (!(dset = SUMA_far2dset_ns("Blurozovsky", NULL, NULL, &fr, N_Node, nc, 0))) {
      SUMA_S_Err("Failed to create random dataset");
      SUMA_RETURN(NULL);;
   }
   if (fr) SUMA_free(fr); fr = NULL;

   SUMA_RETURN(dset);
}

   
#if 0
   /************************** BEGIN Branch Functions **************************/ 
   /* these are functions that were ported to support the first version of SUMA_Surf_Plane_Intersect was was to be identical to
   Surf_Plane_Intersect. They are left here in case I need them in the future. */
   
                  /***

                  File : SUMA_FindBranch.c
                  Author : Ziad Saad
                  Date : Thu Nov 12 16:33:34 CST 1998

                  Purpose : 
                      This is a C version of the matlab function SUMA_FindBranch2, check out the help 
                     over there.

                      This version is supposed to be faster than the working previous one called SUMA_FindBranch.c_V1
                     If you want to use SUMA_FindBranch.c_V1, you need to rename it to SUMA_FindBranch.c and make the appropriate 
                     changes in prototype.h

                     the working version of SUMA_FindBranch.c_V1 is in Backup010499 directory and should be used with 
                     Surf_Plane_Intersect.c_V1

                  Usage : 


                  Input paramters : 
                            InterMat (int **) pointer to a 2D int array of dimention [IMsz, 4]
                           IMsz (int) number or rows in InterMat
                           InterNodes (float **) pointer to a 2D float array that contains the
                              intersection nodes XYZ coordinates, size of the array is [INsz,3]
                           verbose (int) verbose flag (0/1)
                           WBsz (int *) the number of elements in WeldedBranch

                  Returns : 
                            WeldedBranch (SUMA_BRANCH *) is a pointer to structures branch that will contain 
                              the various branches. You need to pass a pointer only, allocation for this 
                              pointer should be done from the calling function.
                              see : /home/ziad/Programs/C/Z/Zlib/mystructs.h for details on
                              the fields of the structures branch

                           NOTE : The function uses static allocation for WeldedBranch
                           do not try to free WeldedBranch



                  ***/
                  SUMA_BRANCH * SUMA_FindBranch (int ** InterMat, int N_InterMat, float ** InterNodes, int ** NodeLoc_in_InterMat, int verbose,  int * WBsz)
                  {/*SUMA_FindBranch*/
                     int DBG , VeryFirstSeed, Seed, sz_Branch, kk;
                     int n_comp = 0, ntmpint, nunqrow , NodeIndex , curnode , BranchIndex;
                     int ntmpint2D_V2, N_vunq , brEnd1 , brEnd2 , i, k;
                     int tmpint2D_V2[1][2], *v, *vunq;
                     int *tmpint, *unqrow, iii, GotSeed;
                     static char FuncName[]={"SUMA_FindBranch"};
                     float Dprecision;
                     static SUMA_BRANCH * branch;
                     struct  timeval  start_time, tt_sub, start_time2;
                     float DT_WELDSUMA_BRANCH, DT_BUILDSUMA_BRANCH, DT_WELDSUMA_BRANCHONLY ,DT_FINDININTVECT, DT_VUNQ;
                     FILE *TimeOut;
                     SUMA_Boolean LocalHead = NOPE; 

                     SUMA_ENTRY;

                     if (LocalHead) SUMA_disp_dmat (NodeLoc_in_InterMat, 20, 4, 1); 

                     /* open a file to output timing info and run some stats on them */
                     TimeOut = fopen("FB.TimeOut","a");   

                     DBG = 1;
                     Dprecision = 0.001;

                     VeryFirstSeed = 0;

                     /* Now you need to find the different branches */

                     Seed = VeryFirstSeed;   /* That should not matter */

                     /* Allocate for branch */
                     if (!branch)
                        {
                           branch = (SUMA_BRANCH *) SUMA_calloc(SUMA_BRANCHMAX, sizeof(SUMA_BRANCH));
                           if (!branch )
                              {
                                 fprintf (SUMA_STDERR, "Error %s: Could not allocate for branch", FuncName);
                                 SUMA_RETURN (NULL);
                              }
                        }

                     if (LocalHead) fprintf(SUMA_STDERR, "%s : Determining branches\n", FuncName);

                     /* Start timer for next function */
                     SUMA_etime(&start_time,0);

                     /* initialize the first branch */
                     BranchIndex = 0;
                     NodeIndex = 0;
                     branch[BranchIndex].start = Seed;
                     branch[BranchIndex].list[NodeIndex] = branch[BranchIndex].start;
                     curnode = branch[BranchIndex].start;
                     n_comp = N_InterMat;
                     ntmpint2D_V2 = 0;
                     while (n_comp)
                        {
                           /* see if you can find the node */
                           /*printf ("curnode = %d, n_comp = %d\n", curnode, n_comp);                   */
                           if (NodeLoc_in_InterMat[curnode][2] > -1)
                              {
                                 tmpint2D_V2[0][0] = NodeLoc_in_InterMat[curnode][2];
                                 tmpint2D_V2[0][1] = NodeLoc_in_InterMat[curnode][3];
                                 NodeLoc_in_InterMat[curnode][2] = -1;
                                 ntmpint2D_V2 = 1;
                              }
                           else
                           if (NodeLoc_in_InterMat[curnode][0] > -1)
                              {
                                 tmpint2D_V2[0][0] = NodeLoc_in_InterMat[curnode][0];
                                 tmpint2D_V2[0][1] = NodeLoc_in_InterMat[curnode][1];
                                 NodeLoc_in_InterMat[curnode][0] = -1;
                                 ntmpint2D_V2 = 1;
                              }
                           else
                              ntmpint2D_V2 = 0;

                           if (!ntmpint2D_V2)  /* Nothing found */
                              {
                                 /* store the last point as a stopping point */
                                 branch[BranchIndex].last = branch[BranchIndex].list[NodeIndex];
                                 branch[BranchIndex].listsz = NodeIndex + 1;

                                 /* start a new branch */
                                 /*pick any seed one that does not have a -1 entry in NodeLoc_in_InterMat*/
                                 iii = 0;
                                 GotSeed = 0;
                                 while (!GotSeed)
                                 {
                                    if (NodeLoc_in_InterMat[iii][2] > -1)
                                       {

                                          Seed = InterMat[NodeLoc_in_InterMat[iii][2]][NodeLoc_in_InterMat[iii][3]];
                                          GotSeed = 1;
                                       }
                                    else
                                    if (NodeLoc_in_InterMat[iii][0] > -1)
                                       {
                                          Seed = InterMat[NodeLoc_in_InterMat[iii][0]][NodeLoc_in_InterMat[iii][1]];
                                          GotSeed = 1;
                                       }
                                    else
                                       {
                                          ++iii;
                                          GotSeed = 0;
                                       }
                                 }
                                 ++BranchIndex;
                                 NodeIndex=0;
                                 branch[BranchIndex].start = Seed;
                                 branch[BranchIndex].list[NodeIndex] = branch[BranchIndex].start;
                                 curnode = branch[BranchIndex].start;
                              }
                           else /* That's a normal point, add it */
                              {
                                 ++NodeIndex;
                                 if (tmpint2D_V2[0][1]) /* take the first element */
                                    branch[BranchIndex].list[NodeIndex] = 
                                       InterMat[tmpint2D_V2[0][0]][0];
                                    else /* take second element */
                                    branch[BranchIndex].list[NodeIndex] = 
                                       InterMat[tmpint2D_V2[0][0]][1];

                                 /* make the new node current */
                                 curnode = branch[BranchIndex].list[NodeIndex];

                                 --n_comp;
                              }

                        }

                     /* now store the very last point as a stopping point */

                     branch[BranchIndex].last = branch[BranchIndex].list[NodeIndex];
                     branch[BranchIndex].listsz = NodeIndex + 1;

                     sz_Branch = BranchIndex + 1;

                     /* stop timer */
                     DT_BUILDSUMA_BRANCH = SUMA_etime(&start_time,1);

                     if (LocalHead) fprintf(SUMA_STDERR, "%s : Welding branches\n", FuncName);

                     /* now, if possible, link the branches together */
                     /* Start timer for next function */
                     SUMA_etime(&start_time,0);

                     /* initialize some variables */
                     v = (int *)SUMA_calloc(2*sz_Branch,sizeof(int));
                     if (!v)
                        {
                           fprintf (SUMA_STDERR, "Error %s: Could not allocate", FuncName);
                           SUMA_RETURN (NULL);
                        }
                     for (i=0;i<sz_Branch;++i)
                        {
                           v[i] = branch[i].start;
                           v[i+sz_Branch] = branch[i].last;
                        }


                     vunq = SUMA_UniqueInt (v, 2*sz_Branch, &N_vunq, 0);

                     for (i=0;i<N_vunq;++i)
                        {
                           /* find out how many time each end of a branch is used */

                           tmpint = SUMA_Find_inIntVect (v, 2*sz_Branch, vunq[i], &ntmpint);

                           if (ntmpint == 2)
                              {
                                 /*good, two branches can be joined together */
                                 if (tmpint[0] >= sz_Branch)
                                    {   
                                       tmpint[0] = tmpint[0] - sz_Branch;
                                       brEnd1 = 1;
                                    }
                                 else
                                    brEnd1 = 0;
                                 if (tmpint[1] >= sz_Branch)
                                    {   
                                       tmpint[1] = tmpint[1] - sz_Branch;
                                       brEnd2 = 1;
                                    }
                                 else
                                    brEnd2 = 0;

                                 if (tmpint[1] != tmpint[0])
                                    {   /*   Path is not circular, join together */

                                       SUMA_WeldBranches (branch, &sz_Branch, tmpint[0] ,tmpint[1] , brEnd1, brEnd2);

                                       for (k=0;k<sz_Branch;++k)
                                          {
                                             v[k] = branch[k].start;
                                             v[k+sz_Branch] = branch[k].last;
                                          }
                                    }
                              }
                           SUMA_free(tmpint);
                        }

                     /* Now go through and determine which branches are closed loops */
                     for (i=0;i<sz_Branch; ++i)
                        {
                           if (branch[i].start == branch[i].last)
                              branch[i].closed = 1;
                           else
                              branch[i].closed = 0;

                        }


                     *WBsz = sz_Branch; /* store the number of branches to SUMA_RETURN it */

                     /* stop timer */
                     DT_WELDSUMA_BRANCH = SUMA_etime(&start_time,1);

                     if (LocalHead) fprintf(SUMA_STDERR, "%s : Freeing allocation\n", FuncName);

                     SUMA_free(vunq);
                     SUMA_free(v);

                     /* Printout timing info on screen */
                     if (LocalHead) {
                        printf ("\n\t\t%s, time fractions :\n",FuncName);
                        printf ("\t\t\tDT_WELDSUMA_BRANCH time: %f sec\n", DT_WELDSUMA_BRANCH);
                        printf ("\t\t\t DT_BUILDSUMA_BRANCH percent time : %f sec\n",  DT_BUILDSUMA_BRANCH);
                     }
                     fprintf(TimeOut, "%f\t%f\n", 
                        DT_BUILDSUMA_BRANCH, DT_WELDSUMA_BRANCH ); 


                     fclose(TimeOut);
                     SUMA_RETURN (branch);

                  }/*SUMA_FindBranch*/

                  /***

                  File : SUMA_WeldBranches.c
                  Author : Ziad Saad
                  Date : Sat Nov 14 19:30:19 CST 1998

                  Purpose : 
                     mimics the function SUMA_WeldBranches.m, check out the help over there.

                      Except that the SUMA_RETURNeed welded branches are not in the same order as
                     those SUMA_RETURNeed by the matlab function

                  Usage : 
                   void SUMA_WeldBranches ( BRANCH *branch, int *sz_Branch, int brIndx1, int brIndx2 , int brEnd1, int brEnd2 );


                  Input paramters : 
                      branch   (BRANCH *)   a vector of structures BRANCH
                     sz_Branch   (int *)   pointer to the scalar containing the number of elements of branch
                     brIndx1   (int)   index (into branch) of the first branch to weld
                     brIndx2    (int)   index (into branch) of the second branch to weld
                     brEnd1   (int)   if 0 then weld at start of branch 1
                                    if 1 then weld at end of branch 1
                     brEnd2   (int) same as brEnd1 but for branch 2

                  Returns : 
                     nothing, but what it does is weld branch1 to branch2 and puts the welded branch in the position of
                     min(branch1, branch2). The returned branch is always one branch shorter than the branch sent into the
                     function.


                  Support : 



                  Side effects : 



                  ***/
                  void SUMA_WeldBranches ( SUMA_BRANCH *branch, int *sz_Branch, int brIndx1, int brIndx2 , int brEnd1, int brEnd2 )
                  {/*SUMA_WeldBranches*/
                     SUMA_BRANCH tmp;
                     int nlst1, nlst2, k, tmpreplace, tmpmove;
                     static char FuncName[]={"SUMA_WeldBranches"};
                     SUMA_Boolean LocalHead = NOPE;

                     SUMA_ENTRY;

                     nlst1 = branch[brIndx1].listsz;
                     nlst2 = branch[brIndx2].listsz;
                     tmp.listsz = nlst1 + nlst2 - 1;

                     if (!brEnd1  && !brEnd2)
                        {
                           tmp.last = branch[brIndx2].last;
                           tmp.start = branch[brIndx1].last;
                           for (k= nlst1; k>0; --k)
                              tmp.list[nlst1-k] =  branch[brIndx1].list[k-1];   
                           for (k=1;k<nlst2;++k) /*skip the common element */
                              tmp.list[nlst1+k-1] = branch[brIndx2].list[k];
                        }
                     else if (brEnd1  && brEnd2)
                        {
                           tmp.last = branch[brIndx2].start;
                           tmp.start = branch[brIndx1].start;
                           for (k= 0; k <nlst1; ++k)
                              tmp.list[k] = branch[brIndx1].list[k];
                           for (k=nlst2; k >1 ; --k)
                              tmp.list[nlst1+nlst2-k] = branch[brIndx2].list[k-2];
                        }   
                     else if (!brEnd1 && brEnd2)
                        {
                           tmp.last = branch[brIndx2].start;
                           tmp.start = branch[brIndx1].last;
                           for (k=nlst1; k > 0; --k)
                              tmp.list[nlst1 - k] = branch[brIndx1].list[k-1];
                           for (k=nlst2; k > 1; --k)
                              tmp.list[nlst1+nlst2-k] = branch[brIndx2].list[k-2];
                        }
                     else if (brEnd1 && !brEnd2)
                        {
                           tmp.last = branch[brIndx2].last;
                           tmp.start = branch[brIndx1].start;
                           for (k=0;k<nlst1;++k)
                              tmp.list[k] = branch[brIndx1].list[k];
                           for (k=0;k<nlst2-1;++k)
                              tmp.list[nlst1+k] = branch[brIndx2].list[k+1];
                        }

                     /* decide where to put the welded branch and whether to move the last branch (or the one before it) up */
                     if (brIndx1 > brIndx2)
                        {
                           tmpreplace = brIndx2;
                           tmpmove = brIndx1;
                        }
                     else
                        {
                           tmpreplace = brIndx1;
                           tmpmove = brIndx2;
                        }
                     /* replace branch[tmpreplace]   with tmp */
                     branch[tmpreplace].start = tmp.start;
                     branch[tmpreplace].last = tmp.last;
                     branch[tmpreplace].listsz = tmp.listsz;
                     for(k=0;k<branch[tmpreplace].listsz;++k)
                        branch[tmpreplace].list[k] = tmp.list[k];

                     /*copy branch[sz_Branch-1] (the last branch) into position brIndx2 */
                     /*by now, tmpmove is definetly larger than tmpreplace*/
                     /* if tmpmove is not the last branch, then move the last branch up one*/
                     /* otherwise, no need to move anything */

                     if (tmpmove < *sz_Branch-1)
                        {
                           branch[tmpmove].start = branch[*sz_Branch-1].start;
                           branch[tmpmove].last = branch[*sz_Branch-1].last;
                           branch[tmpmove].listsz = branch[*sz_Branch-1].listsz;
                           for(k=0;k<branch[tmpmove].listsz;++k)
                              branch[tmpmove].list[k] = branch[*sz_Branch-1].list[k];
                        }

                     /* change the size of the branch vector */
                     --*sz_Branch;

                     SUMA_RETURNe;

                  }/*SUMA_WeldBranches*/

   /************************** END Branch Functions **************************/                   


#endif 
