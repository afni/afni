#include "SUMA_suma.h"

#undef STAND_ALONE

#if defined SUMA_SurfSmooth_STAND_ALONE
#define STAND_ALONE
#elif defined SUMA_getPatch_STANDALONE
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

/*!
   \brief function to subdivide triangles to meet a maxarea criterion
   Divisions are done by adding a node at the centroid of the triangle 
   to be subdivided. Bad idea, for very large triangles, such as produced
   by convex hull, you could end up with nodes that have hundreds of neighbors...
*/
int SUMA_Subdivide_Mesh(float **NodeListp, int *N_Nodep, int **FaceSetListp, int *N_FaceSetp, float maxarea)
{
   static char FuncName[]={"SUMA_Subdivide_Mesh"};
   int in, it, N_NodeAlloc, N_FaceSetAlloc, N_Node, N_FaceSet, it3, in0, in1, in2, inc3, inc, itn, itn3;
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
   if (!NodeList || !FaceSetList) { SUMA_SL_Err("NULL input"); SUMA_RETURN(NOPE); }
   
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
SUMA_VTI *SUMA_GetVoxelsIntersectingTriangle(   SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, float *NodeIJKlist,
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
   SUMA_Boolean LocalHead = YUP;
   
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
                                                             SUMA_isVoxelIntersect_Triangle?*/
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
   int dothisvoxel, itmp;
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
   
   if (usethisisin) isin = usethisisin;
   else {
      isin = (byte *)SUMA_calloc(nijk, sizeof(byte));
      if (!isin) {
         SUMA_SL_Crit("Failed to allocate");
         SUMA_RETURN(NULL);
      }
   }
   
   dothisvoxel = ijkseed;
   dlist_init(candlist, NULL);
   
   
   isin[dothisvoxel] = 1; ++(*N_in); /* Add voxel to cluster */
   visited[dothisvoxel] = 1;  
   dlist_ins_next(candlist, dlist_tail(candlist), (void *)dothisvoxel); /* Add voxel as next candidate*/
   
   while (dlist_size(candlist)) {
      /* find neighbors in its vicinity */
      dothiselm = dlist_head(candlist); dothisvoxel = (int) dothiselm->data;
      N_n = SUMA_VoxelNeighbors (dothisvoxel, ni, nj, nk, SUMA_VOX_NEIGHB_FACE, nl);
      /* remove node from candidate list */
      dlist_remove(candlist, dothiselm, (void*)&itmp);
      /* search to see if any are to be assigned */
      for (in=0; in<N_n; ++in) { 
         neighb = nl[in];
         if (!ijkmask[neighb]) {
            isin[neighb] = 1; ++(*N_in); /* Add voxel to cluster */
            /* mark it as a candidate if it has not been visited as a candidate before */
            if (!visited[neighb]) {
               dlist_ins_next(candlist, dlist_tail(candlist), (void *)neighb);
               visited[neighb] = 1;   
            }
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
double SUMA_NewAreaAtRadius(SUMA_SurfaceObject *SO, double r, double Rref, float *tmpList)
{
   static char FuncName[]={"SUMA_NewAreaAtRadius"};
   double Dr, A=0.0,  Un, U[3], Dn, P2[2][3], c[3];
   float *fp;
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* calculate Dr and normalize by the radius of SOref */
   Dr = ( Rref - r ) / Rref;

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


   /* calculate the new Area */
   fp = SO->NodeList;/* save NodeList */
   SO->NodeList = tmpList; /* use new coordinates */
   A = fabs((double)SUMA_Mesh_Area(SO, NULL, -1));
   SO->NodeList = fp; fp = NULL;   /* make NodeList point to the original data */

   SUMA_RETURN(A);
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
   Dr = ( Rref - r ) / Rref;

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
      if (LocalHead) { fprintf(SUMA_STDERR,"%s: Reference volume = %f, radius = %f \n", FuncName, Aref, Rref); }
      if (cs->Send) { /* send the first monster (it's SOref "in SUMA" that's being modified on the fly) */
         if (!SUMA_SendToSuma (SOref, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
   }
   
   A = SUMA_NewAreaAtRadius(SO, r, Rref, fdata->tmpList);
   da = Aref - A; /* the area difference */
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Call %d, A = %f, Aref = %f, da = %f\n", FuncName,ncall, A, Aref, da);
   }
      
   /* need an update ? */
   if (cs->Send) { /* send the update (it's SOref "in SUMA" that's being modified on the fly) */
      if (!SUMA_SendToSuma (SOref, cs, (void *)fdata->tmpList, SUMA_NODE_XYZ, 1)) {
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
   dv = Vref - V; /* the volume difference */
      
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
double SUMA_BinaryZeroSearch(double a, double b, double(*f)(double x, void *data), void *fdata, int Nitermax, double tol) {
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
      if (LocalHead) fprintf(SUMA_STDERR,"%s: %d\ta=%.4f\tb=%.4f\tx=%.4f\tfx=%.4f\n", 
                                          FuncName, Niter, a, b, x, fx);
      if (fx < 0) a = x;
      else b = x;
      if (fabs(fx) < tol) done = YUP;
      ++Niter;
   }
   
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
SUMA_Boolean SUMA_GetAreaDiffRange(SUMA_AreaDiffDataStruct *fdata, double *ap, double *bp)
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

   /* a very simple range setting. might very well fail at times */
   if (fdata->Aref - fdata->A < 0) { 
      a = fdata->R; /* choose 'a' such that Aref - A is < 0, Choose b later*/
      An = fdata->A;
      b = fdata->Rref;
      do {
         SUMA_LH("Looking for b");
         b *= 1.3;  /* choose A that Aref - A is > 0 */
         Bn = SUMA_NewAreaAtRadius(fdata->SO, b, fdata->Rref, fdata->tmpList);
         ++nbt;
      } while ( fdata->Aref - Bn < 0 && nbt < 200);
   }else{
      b = fdata->R; /* choose 'b' such that Aref - A is > 0, Choose a later*/
      Bn = fdata->A;
      a = fdata->Rref;
      do {
         SUMA_LH("Looking for a");
         a *= 0.7;
         An = SUMA_NewAreaAtRadius(fdata->SO, a, fdata->Rref, fdata->tmpList);
         ++nat;
      } while ( fdata->Aref -  An> 0 && nat < 200);
   }

   *ap = a; *bp = b;

   if (nat >= 200 || nbt >= 200) {
      SUMA_SL_Err("Failed to find segment.");
      SUMA_RETURN(NOPE);
   }

   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s:\nChosen range is [%f %f] with Areas [%f %f], reference Area is %f\n", 
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
   if (fdata->Vref - fdata->V < 0) { 
      a = fdata->R; /* choose 'a' such that Vref - V is < 0, Choose b later*/
      b = fdata->Rref;
      do {
         SUMA_LH("Looking for b");
         b *= 1.3; ++nbt; /* choose b that Vref - V is > 0 */
      } while ( fdata->Vref - SUMA_NewVolumeAtRadius(fdata->SO, b, fdata->Rref, fdata->tmpList) < 0 && nbt < 200);
   }else{
      b = fdata->R; /* choose 'b' such that Vref - V is > 0, Choose a later*/
      a = fdata->Rref;
      do {
         SUMA_LH("Looking for a");
         a *= 0.7; ++nat;
      } while ( fdata->Vref - SUMA_NewVolumeAtRadius(fdata->SO, a, fdata->Rref, fdata->tmpList) > 0 && nat < 200);
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
SUMA_Boolean SUMA_EquateSurfaceAreas(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float tol, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_EquateSurfaceAreas"};
   int iter, i, iter_max, ndiv;
   double a, b, d;
   SUMA_AreaDiffDataStruct fdata;
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
          c is the centers of SO .
   \param SOref reference SurfaceObject, used to communicate with SUMA 
   \param radius , you know what.
   \param cs the famed communication structure
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

   if (!SO || !SOref) { SUMA_SL_Err("NULL surface"); SUMA_RETURN(NOPE); }
   
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
   int i, j, k, n, j3p1, j3m1, n3, j3=0, nj, nj3, i_dbg;
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
   
   i_dbg = -1; /* index of debugging node, set to -1 for no debugging */
   /* in the demo mesh that Moo Chung gave me, 
   Node 17 has the following neighbors in ccw order:
   231 230 250 261 239 236 - 231 230 250 261 239 236 
   Set i_dbg = 17 and turn on LocalHead to get a confirmation of this
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
      if (LocalHead && n == i_dbg) { SUMA_disp_vect (coord_nbr, 3 * (SO->FN->N_Neighb[n] + 2)) ;  }
      
      
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
         if (LocalHead && n == i_dbg) {
            fprintf (SUMA_STDERR,"[%d->%d] area_p, area_q = %f, %f\n",
                                  n, SO->FN->FirstNeighb[n][j-1],
                                  area_p / 2.0,  area_q / 2.0);
         }
      }
      
      for (j=0; j<SO->FN->N_Neighb[n]; ++j) {
         wgt[n][j] = cotan[j]/area;
      }
      if (LocalHead && n == i_dbg) {
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
                            byte *nmask)
{
   static char FuncName[]={"SUMA_Taubin_Smooth"};
   float *fout_final=NULL, *fbuf=NULL, *fin=NULL, *fout=NULL, *fin_next=NULL, *ftmp=NULL;
   float fp, dfp, fpj;
   int i, n , k, j, niter, vnk, n_offset, DoThis; 
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
                     for (j=0; j < SO->FN->N_Neighb[n]; ++j) { /* calculating the laplacian */
                        fpj = fin[SO->FN->FirstNeighb[n][j]+n_offset]; /* value at jth neighbor of n */
                        if (wgt) dfp += wgt[n][j] * (fpj - fp); 
                        else dfp += (fpj - fp); /* will apply equal weight later */
                     }/* for j*/
                     if (niter%2) { /* odd */
                        if (wgt) fout[vnk] = fin[vnk] + mu * dfp;
                        else fout[vnk] = fin[vnk] + mu * dfp / (float)SO->FN->N_Neighb[n];   /* apply equal weight factor here */
                     }else{ /* even */
                       if (wgt) fout[vnk] = fin[vnk] + lambda * dfp;
                       else fout[vnk] = fin[vnk] + lambda * dfp / (float)SO->FN->N_Neighb[n];  /* apply equal weight factor here */
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
            }
            for (n=0; n < SO->N_Node; ++n) {
               DoThis = 1;
               if (nmask && !nmask[n]) DoThis = 0;
               vnk = n * vpn; /* index of 1st value at node n */
               for (k=0; k < vpn; ++k) {
                  if (DoThis) {
                     fp = fin[vnk]; /* kth value at node n */
                     dfp = 0.0;
                     for (j=0; j < SO->FN->N_Neighb[n]; ++j) { /* calculating the laplacian */
                        fpj = fin[SO->FN->FirstNeighb[n][j]*vpn+k]; /* value at jth neighbor of n */
                        if (wgt) dfp += wgt[n][j] * (fpj - fp); 
                        else dfp += (fpj - fp); /* will apply equal weight later */
                     }/* for j*/
                     if (niter%2) { /* odd */
                        if (wgt) fout[vnk] = fin[vnk] + mu * dfp;
                        else fout[vnk] = fin[vnk] + mu * dfp / (float)SO->FN->N_Neighb[n];   /* apply equal weight factor here */
                     }else{ /* even */
                       if (wgt) fout[vnk] = fin[vnk] + lambda * dfp;
                       else fout[vnk] = fin[vnk] + lambda * dfp / (float)SO->FN->N_Neighb[n];  /* apply equal weight factor here */
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
   \brief creates a vector of node neighbors structures such that:
   OffS = SUMA_FormNeighbOffset ( SUMA_SurfaceObject *SO, float OffsetLim);
   
   \param OffS (SUMA_OFFSET_STRUCT *) SO->Node x 1 vector of structures 
         OffS[i] is a structure containing node neighbors of node i
         OffS[i].N_Neighb (int) number of neighbors of node i
         OffS[i].Neighb_ind (int *) OffS[i].N_Neighb x 1 vector containing 
                                    nodes neighboring node i
         OffS[i].Neighb_dist (float *) OffS[i].N_Neighb x 1 vector containing 
                                    node distances from node i. 
                                    These are the shortes distances ON THE GRAPH.
                                    The distances might be larger than OffsetLim
                                    because the child function SUMA_getoffsets2
                                    does so.
   \param OffsetLim (float) maximal inclusive neighbor distance. In reality, some 
                           nodes may be farther apart. But all nodes closer than 
                           OffsetLim to each node will be included
   - NOTE: This function will chew up a lot of memory, real quick.
            An approximate equation for the size needed for OffS:
               (mean_N_Neighb_per_Node * 8 + 12) * SO->N_Node Bytes
               With OffsetLim = 10 and a FreeSurfer surface of 150'000 nodes,
               the average number of neighbors per node is ~800. 
               Which means 962MB for the returned structure.
         When memory usage is this high, you should consider using SUMA_getoffsets2
         repeatedly for each node, as is done in this function.
         
*/
 
SUMA_OFFSET_STRUCT *SUMA_FormNeighbOffset ( SUMA_SurfaceObject *SO, float OffsetLim)
{
   static char FuncName[]={"SUMA_FormNeighbOffset"};
   int i, ii, il, jl, noffs, ShowNode = 4;
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   struct  timeval start_time;
   float etime_GetOffset, mean_N_Neighb, dist, dist_norm;   
   SUMA_OFFSET_STRUCT *OffS_out=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!SO) { SUMA_SL_Err("NULL SO"); SUMA_RETURN(NULL); }
   if (!SO->FN) {
      SUMA_SL_Err("NULL SO->FN");
      SUMA_RETURN(NULL);
   }
   OffS_out = (SUMA_OFFSET_STRUCT *)SUMA_malloc(SO->N_Node * sizeof(SUMA_OFFSET_STRUCT));
   
   SUMA_etime(&start_time,0);
   
   OffS = SUMA_Initialize_getoffsets (SO->N_Node);
   mean_N_Neighb = 0;
   dist_norm = 1.1 * OffsetLim;
   for (i=0; i < SO->N_Node; ++i) {
      SUMA_getoffsets2 (i, SO, OffsetLim, OffS, NULL, 0);
      /* Now store all the relevant info in OffS in OffS_out[i] */
      OffS_out[i].N_Neighb = 0; 
      for (il=1; il<OffS->N_layers; ++il) {
         OffS_out[i].N_Neighb += OffS->layers[il].N_NodesInLayer;
      }
      OffS_out[i].Neighb_ind = (int *)SUMA_malloc(OffS_out[i].N_Neighb * sizeof(int));
      OffS_out[i].Neighb_dist = (float *)SUMA_malloc(OffS_out[i].N_Neighb * sizeof(float));
      mean_N_Neighb += OffS_out[i].N_Neighb;
      noffs = 0;
      for (il=1; il<OffS->N_layers; ++il) {
         for (jl=0; jl<OffS->layers[il].N_NodesInLayer; ++jl) {
            OffS_out[i].Neighb_ind[noffs] = OffS->layers[il].NodesInLayer[jl]; 
            #if 1
            /* don't play fancy with the weights here */
            OffS_out[i].Neighb_dist[noffs] = OffS->OffVect[OffS_out[i].Neighb_ind[noffs]];
            #else
            dist = OffS->OffVect[OffS_out[i].Neighb_ind[noffs]];
            if (dist > OffsetLim) OffS_out[i].Neighb_dist[noffs] = 0;
            else OffS_out[i].Neighb_dist[noffs] = (dist ); 
            #endif
            ++noffs;
         }
      }
      
      /* Show me the offsets for one node*/
      if (0) {
         if (i == ShowNode) {
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
                            "#Column 3 = Distance from node %d\n", ShowNode);
               for (ii=0; ii<SO->N_Node; ++ii) {
                  if (OffS->LayerVect[ii] >= 0) {
                     fprintf(fid,"%d\t%d\t%f\n", ii, OffS->LayerVect[ii], OffS->OffVect[ii]);
                  }
               }
               fclose(fid);
            }
         }
      }
               
      if (i == 99) {
         etime_GetOffset = SUMA_etime(&start_time,1);
         fprintf(SUMA_STDERR, "%s: Search to %f mm took %f seconds for %d nodes.\n"
                              "Projected completion time: %f minutes\n"
                              "Projected memory need for structure %f MB", 
                              FuncName, OffsetLim, etime_GetOffset, i+1, 
                              etime_GetOffset * SO->N_Node / 60.0 / (i+1),
                              (mean_N_Neighb / (i+1) * 8 + 12)* SO->N_Node/1000000.0);
      }
      SUMA_Recycle_getoffsets (OffS);
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
   OffS_out = SUMA_FormNeighbOffset (SO, OffsetLim);
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
                           SUMA_COMM_STRUCT *cs)
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
                                                 fout, SO->FN, vpn);
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
                           SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_Chung_Smooth"};
   float *fout_final = NULL, *fbuf=NULL, *fin=NULL, *fout=NULL, *fin_next = NULL;
   float delta_time, fp, dfp, fpj, minfn=0.0, maxfn=0.0;
   int n , k, j, niter, vnk, os;
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

#ifdef SUMA_SurfSmooth_STAND_ALONE
void usage_SUMA_SurfSmooth ()
   {
      static char FuncName[]={"usage_SUMA_SurfSmooth"};
      char * s = NULL, *st = NULL;
      s = SUMA_help_basics();
      st = SUMA_help_talk();
      printf ("\nUsage:  SurfSmooth <-spec SpecFile> <-surf_A insurf> <-met method> \n"
              "\n"
              "   Some methods require additional options detailed below.\n"
              "   I recommend using the -talk_suma option to watch the \n"
              "   progression of the smoothing in real-time in suma.\n"
              "\n"
              "   Method specific options:\n"
              "      LB_FEM: <-input inData.1D> <-fwhm f>\n"
              "              This method is used to filter data\n"
              "              on the surface.\n"
              "      LM: [-kpb k] [-lm l m] [-surf_out surfname]\n"
              "          This method is used to filter the surface's\n"
              "          geometry (node coordinates).\n"
              "      NN_geom: smooth by averaging coordinates of \n"
              "               nearest neighbors.\n"
              "               This method causes shrinkage of surface\n"
              "               and is meant for test purposes only.\n"
              "\n"
              "   Common options:\n"
              "      [-Niter N] [-output out.1D] [-h/-help] \n"
              "      [-add_index] [-ni_text|-ni_binary] [-talk_suma]\n\n"
              "\n"
              "   Detailed usage:\n"
              "      -spec SpecFile: Name of specfile containing surface of interest.\n"
              "                      If the surface does not have a spec file, use the \n"
              "                      program quickspec to create one.\n"
              "      -surf_A insurf: Name of surface of interest. \n"
              "                      NOTE: i_TYPE inSurf option is now obsolete.\n"
              "      -met method: name of smoothing method to use. Choose from:\n"
              "                 LB_FEM: The method by Chung et al. 03.\n"
              "                         This method is used for filtering \n"
              "                         data on the surface not for smoothing the\n"
              "                         surface's geometry per se. See References below.\n"
              "                 LM: The smoothing method proposed by G. Taubin 2000\n"
              "                     This method is used for smoothing\n"
              "                     a surface's geometry. See References below.\n"
              "                 NN_geom: A simple nearest neighbor coordinate smoothing.\n"
              "                          This interpolation method causes surface shrinkage\n"
              "                          that might need to be corrected with the -match_*\n"
              "                          options below. \n" 
              "\n"
              "   Options for LB_FEM:\n"
              "      -input inData.1D: file containing data (in 1D format)\n"
              "                        Each column in inData.1D is processed separately.\n"
              "                        The number of rows must equal the number of\n"
              "                        nodes in the surface. You can select certain\n"
              "                        columns using the [] notation adopted by AFNI's\n"
              "                        programs.\n"
              "      -fwhm f: Full Width at Half Maximum in surface coordinate units (usuallly mm)\n"
              "               of an equivalent Gaussian filter had the surface been flat.\n"
              "               With curved surfaces, the equation used to estimate FWHM is \n"
              "               an approximation. \n"
              "               Blurring on the surface depends on the geodesic instead \n"
              "               of the Euclidean disntaces. See Ref #1 for more details \n"
              "               on this parameter.\n"
              "\n"
              "   Options for LM:\n"
              "      -kpb k: Band pass frequency (default is 0.1).\n"
              "              values should be in the range 0 < k < 10\n"
              "              -lm and -kpb options are mutually exclusive.\n"
              "      -lm l m: Lambda and Mu parameters. Sample values are:\n"
              "               0.6307 and -.6732\n"
              "      NOTE: -lm and -kpb options are mutually exclusive.\n"
              "      -surf_out surfname: Writes the surface with smoothed coordinates\n"
              "                          to disk. For SureFit and 1D formats, only the\n"
              "                          coord file is written out.\n"
              "      NOTE: -surf_out and -output are mutually exclusive.\n"  
              "\n"
              "   Options for NN_geom:\n"
              "      -match_size r: Adjust node coordinates of smoothed surface to \n"
              "                   approximates the original's size.\n"
              "                   Node i on the filtered surface is repositioned such \n"
              "                   that |c i| = 1/N sum(|cr j|) where\n"
              "                   c and cr are the centers of the smoothed and original\n"
              "                   surfaces, respectively.\n"
              "                   N is the number of nodes that are within r [surface \n"
              "                   coordinate units] along the surface (geodesic) from node i.\n"
              "                   j is one of the nodes neighboring i.\n"
              "      -match_vol tol: Adjust node coordinates of smoothed surface to \n"
              "                   approximates the original's volume.\n"
              "                   Nodes on the filtered surface are repositioned such\n"
              "                   that the volume of the filtered surface equals, \n"
              "                   within tolerance tol, that of the original surface. \n"
              "                   See option -vol in SurfaceMetrics for information about\n"
              "                   and calculation of the volume of a closed surface.\n"
              "      -match_area tol: Adjust node coordinates of smoothed surface to \n"
              "                   approximates the original's surface.\n"
              "                   Nodes on the filtered surface are repositioned such\n"
              "                   that the surface of the filtered surface equals, \n"
              "                   within tolerance tol, that of the original surface. \n"
              "      -match_sphere rad: Project nodes of smoothed surface to a sphere\n"
              "                   of radius rad. Projection is carried out along the \n"
              "                   direction formed by the surface's center and the node.\n"
              "\n"
              "   Common options:\n"
              "      -Niter N: Number of smoothing iterations (default is 100)\n"
              "                For practical reasons, this number must be a multiple of 2\n"
              "          NOTE: For LB_FEM method, the number of iterations controls the\n"
              "                iteration steps (dt in Ref #1).\n"
              "                dt = fwhm*fwhm / (16*Niter*log(2));\n"
              "                dt must satisfy conditions that depend on the internodal\n"
              "                distance and the spatial derivatives of the signals being \n"
              "                filtered on the surface.\n"
              "                As a rule of thumb, if increasing Niter does not alter\n"
              "                the results then your choice is fine (smoothing has converged).\n"
              "                For an example of the artifact caused by small Niter see:\n"
              "          http://afni.nimh.nih.gov/sscc/staff/ziad/SUMA/SuSmArt/DSart.html\n"
              "      -output out.1D: Name of output file. \n"
              "                      The default is inData_sm.1D with LB_FEM method\n"
              "                      and NodeList_sm.1D with LM method.\n" 
              "      -add_index : Output the node index in the first column.\n"
              "                   This is not done by default.\n"
              "\n"
              "%s"
              "\n"
              "%s"
              "\n"
              "   Sample commands lines for data smoothing:\n"
              "      SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met LB_FEM   \\\n"
              "                  -input in.1D -Niter 100 -fwhm 8 -add_index         \\\n"
              "                  -output in_sm8.1D \n"
              "         This command filters (on the surface) the data in in.1D\n"
              "         and puts the output in in_sm8.1D with the first column \n"
              "         containing the node index and the second containing the \n"
              "         filtered version of in.1D.\n"
              "\n"
              "         The surface used in this example had no spec file, so \n"
              "         a quick.spec was created using:\n"
              "         quickspec -tn 1D NodeList.1D FaceSetList.1D \n"
              "\n"
              "         You can colorize the input and output data using ScaleToMap:\n"
              "         ScaleToMap  -input in.1D 0 1 -cmap BGYR19       \\\n"
              "                     -clp MIN MAX > in.1D.col            \\\n"
              "         ScaleToMap  -input in_sm8.1D 0 1 -cmap BGYR19   \\\n"
              "                     -clp MIN MAX > in_sm8.1D.col        \\\n"
              "\n"
              "         For help on using ScaleToMap see ScaleToMap -help\n"
              "         Note that the MIN MAX represent the minimum and maximum\n"
              "         values in in.1D. You should keep them constant in both \n"
              "         commands in order to be able to compare the resultant colorfiles.\n"
              "         You can import the .col files with the 'c' command in SUMA.\n"
              "\n"
              "         You can send the data to SUMA with each iteration.\n"
              "         To do so, start SUMA with these options:\n"
              "         suma -spec quick.spec -niml &\n"
              "         and add these options to SurfSmooth's command line above:\n"
              "         -talk_suma -refresh_rate 5\n" 
              "\n"
              "   Sample commands lines for surface smoothing:\n"
              "      SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met LM    \\\n"
              "                  -output NodeList_sm100.1D -Niter 100 -kpb 0.1   \n"
              "         This command smoothes the surface's geometry. The smoothed\n"
              "         node coordinates are written out to NodeList_sm100.1D. \n"
              "\n"
              "   Sample command for considerable surface smoothing and inflation\n"
              "   back to original volume:\n"
              "       SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met NN_geom \\\n"
              "                   -output NodeList_inflated_mvol.1D -Niter 1500 \\\n"
              "                   -match_vol 0.01\n" 
              "   Sample command for considerable surface smoothing and inflation\n"
              "   back to original area:\n"
              "       SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met NN_geom \\\n"
              "                   -output NodeList_inflated_marea.1D -Niter 1500 \\\n"
              "                   -match_area 0.01\n" 
              "\n"
              "   References: \n"
              "      (1) M.K. Chung et al.   Deformation-based surface morphometry\n"
              "                              applied to gray matter deformation. \n"
              "                              Neuroimage 18 (2003) 198-213\n"
              "          M.K. Chung   Statistical morphometry in computational\n"
              "                       neuroanatomy. Ph.D. thesis, McGill Univ.,\n"
              "                       Montreal, Canada\n"
              "      (2) G. Taubin.       Mesh Signal Processing. \n"
              "                           Eurographics 2000.\n"
              "\n"
              "   See Also:   \n"
              "       ScaleToMap to colorize the output, however it is better\n"
              "       to load surface datasets directly into SUMA and colorize\n"
              "       them interactively."
              "\n"
              "\n", st, s); SUMA_free(s); s = NULL; SUMA_free(st); st = NULL;
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
       exit (0);
   }


#define SURFSMOOTH_MAX_SURF 1  /*!< Maximum number of input surfaces */

typedef enum { SUMA_NO_METH, SUMA_LB_FEM, SUMA_LM, SUMA_BRUTE_FORCE, SUMA_NN_GEOM} SUMA_SMOOTHING_METHODS;

typedef struct {
   float OffsetLim;
   float lim;
   float fwhm;
   float kpb;
   float l;
   float m;
   int ShowNode;
   int Method;
   int dbg;
   int N_iter;
   int AddIndex;
   int insurf_method; /* method used to specify input surfaces. 
                        0 then none input 
                        1 the old way
                        2 the new (-spec way) */
   SUMA_SO_File_Type iType;
   char *vp_name;
   char *sv_name;
   char *if_name;
   char *if_name2;
   char *in_name;
   char *out_name;   /* this one's dynamically allocated so you'll have to free it yourself */
   char *ShowOffset_DBG;
   char *surf_out;
   char *surf_names[SURFSMOOTH_MAX_SURF];
   char *spec_file;
   int MatchMethod;
} SUMA_SURFSMOOTH_OPTIONS;

/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_SURFSMOOTH_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_SURFSMOOTH_OPTIONS *SUMA_SurfSmooth_ParseInput (char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfSmooth_ParseInput"}; 
   SUMA_SURFSMOOTH_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outname;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_SURFSMOOTH_OPTIONS *)SUMA_malloc(sizeof(SUMA_SURFSMOOTH_OPTIONS));
   
   kar = 1;
   Opt->OffsetLim = 10.0;
   Opt->MatchMethod = 0;
   Opt->lim = 1000000.0;
   Opt->fwhm = -1;
   Opt->ShowNode = -1;
   Opt->Method = SUMA_NO_METH;
   Opt->dbg = 0;
   Opt->if_name = NULL;
   Opt->if_name2 = NULL;
   Opt->in_name = NULL;
   Opt->out_name = NULL;
   Opt->vp_name = NULL; 
   Opt->sv_name = NULL;
   Opt->surf_out = NULL;
   Opt->ShowOffset_DBG = NULL;
   Opt->iType = SUMA_FT_NOT_SPECIFIED;
   Opt->N_iter = 100;
   Opt->kpb = -1.0;
   Opt->l = -1.0;
   Opt->m = -1.0;
   Opt->AddIndex = 0;
   Opt->insurf_method = 0;
   Opt->spec_file = NULL;
   for (i=0; i<SURFSMOOTH_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_SurfSmooth();
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      #if 0 /* now  in the SUMA_GENERIC_ARGV_PARSE struct */
      if (!brk && strcmp(argv[kar], "-ni_text") == 0)
		{
         SUMA_GEOMCOMP_NI_MODE = NI_TEXT_MODE;
         brk = YUP;
      }
      
      if (!brk && strcmp(argv[kar], "-ni_binary") == 0)
		{
         SUMA_GEOMCOMP_NI_MODE = NI_BINARY_MODE;
         brk = YUP;
      }
      
		if (!brk && strcmp(argv[kar], "-sh") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -sh \n");
				exit (1);
			}
			if (strcmp(argv[kar],"localhost") != 0) {
            Opt->suma_host_name = SUMA_copy_string(argv[kar]);
         }else {
           fprintf (SUMA_STDERR, "localhost is the default for -sh\nNo need to specify it.\n");
         }

			brk = YUP;
		}	
      if (!brk && strcmp(argv[kar], "-refresh_rate") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -refresh_rate \n");
				exit (1);
			}
			Opt->rps = atof(argv[kar]);
         if (Opt->rps <= 0) {
            fprintf (SUMA_STDERR, "Bad value (%f) for refresh_rate\n", Opt->rps);
				exit (1);
         }

			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-talk_suma") == 0)) {
			Opt->talk_suma = 1; 
			brk = YUP;
		}
      
      #endif
      if (!brk && strcmp(argv[kar], "-dist") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -dist \n");
				exit (1);
			}
			Opt->OffsetLim = atof(argv[kar]);
         if (Opt->OffsetLim <= 0) {
            fprintf (SUMA_STDERR, "Bad value (%f) for refresh_rate\n", Opt->OffsetLim);
				exit (1);
         }

			brk = YUP;
		}
     
      if (!brk && (strcmp(argv[kar], "-dbg_n") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 arguments after -dbg_n \n");
				exit (1);
			}
			Opt->ShowNode = atoi(argv[kar]); kar ++;
         Opt->ShowOffset_DBG = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-Niter") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 arguments after -Niter \n");
				exit (1);
			}
			Opt->N_iter = atoi(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-kpb") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 arguments after -kpb \n");
				exit (1);
			}
         if (Opt->l != -1.0  || Opt->m != -1.0) {
            fprintf (SUMA_STDERR, "options -lm and -kpb are mutually exclusive\n");
				exit (1);
         }
			Opt->kpb = atof(argv[kar]); 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-surf_out") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 arguments after -surf_out\n");
				exit (1);
			}
         if (outname) {
            fprintf (SUMA_STDERR, "-output and -surf_out are mutually exclusive.\n");
            exit(1);
         }
			Opt->surf_out = argv[kar]; 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-lm") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 arguments after -lm \n");
				exit (1);
			}
         if (Opt->kpb != -1.0) {
            fprintf (SUMA_STDERR, "options -lm and -kpb are mutually exclusive\n");
				exit (1);
         }
			Opt->l = atof(argv[kar]); kar ++;
         Opt->m = atof(argv[kar]);  
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input \n");
				exit (1);
			}
			Opt->in_name = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-output") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -output\n");
				exit (1);
			}
			if (Opt->surf_out) {
            fprintf (SUMA_STDERR, "options -surf_out and -output are mutually exclusive\n");
				exit (1);
         }
         outname = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-add_index") == 0)) {
			Opt->AddIndex = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_fs") == 0)) {
         SUMA_SL_Err("Option -i_fs is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_fs \n");
				exit (1);
			}
			Opt->if_name = argv[kar];
         Opt->iType = SUMA_FREE_SURFER;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_sf") == 0)) {
         SUMA_SL_Err("Option -i_sf is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_sf\n");
				exit (1);
			}
			Opt->if_name = argv[kar]; kar ++;
         Opt->if_name2 = argv[kar];
         Opt->iType = SUMA_SUREFIT;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_vec") == 0)) {
         SUMA_SL_Err("Option -i_vec is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_vec\n");
				exit (1);
			}
			Opt->if_name = argv[kar]; kar ++;
         Opt->if_name2 = argv[kar];
         Opt->iType = SUMA_VEC;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_dx") == 0)) {
         SUMA_SL_Err("Option -i_ply is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_dx\n ");
				exit (1);
			}
			Opt->if_name = argv[kar];
         Opt->iType = SUMA_OPENDX_MESH;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_ply") == 0)) {
         SUMA_SL_Err("Option -i_ply is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_ply\n ");
				exit (1);
			}
			Opt->if_name = argv[kar];
         Opt->iType = SUMA_PLY;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec \n");
				exit (1);
			}
			Opt->spec_file = argv[kar];
         if (!Opt->insurf_method) Opt->insurf_method = 2;
         else {
            fprintf (SUMA_STDERR, "already specified spec file.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strncmp(argv[kar], "-surf_", 6) == 0)) {
			if (kar + 1>= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -surf_X SURF_NAME \n");
				exit (1);
			}
			ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFSMOOTH_MAX_SURF) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option is out of range,\n"
                                  "   only surf_A allowed.\n");
				exit (1);
         }
         kar ++;
         Opt->surf_names[ind] = argv[kar];
			if (Opt->insurf_method && Opt->insurf_method != 2) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option must be used with -spec option.\n");
            exit(1);
         }
         brk = YUP;
		}
      
      
      if (!brk && (strcmp(argv[kar], "-lim") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -lim \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-match_size") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -match_size \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
         Opt->MatchMethod = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-match_vol") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -match_vol \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
         Opt->MatchMethod = 2;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-match_area") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -match_area \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
         Opt->MatchMethod = 3;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-match_sphere") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -match_sphere \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
         Opt->MatchMethod = 4;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-fwhm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -fwhm \n");
				exit (1);
			}
			Opt->fwhm = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-met") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -met \n");
				exit (1);
			}
			if (strcmp(argv[kar], "LB_FEM") == 0)  Opt->Method = SUMA_LB_FEM;
         else if (strcmp(argv[kar], "LM") == 0)  Opt->Method = SUMA_LM;
         else if (strcmp(argv[kar], "BF") == 0)  Opt->Method = SUMA_BRUTE_FORCE;
         else if (strcmp(argv[kar], "NN_geom") == 0)  Opt->Method = SUMA_NN_GEOM;
         else {
            fprintf (SUMA_STDERR, "Method %s not supported.\n", argv[kar]);
				exit (1);
         }
			brk = YUP;
		}
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   if (Opt->N_iter < 1) {
      fprintf (SUMA_STDERR,"Error %s:\nWith -Niter N option, N must be > 1\n", FuncName);
      exit (1);
   }
   
   if ( (Opt->N_iter % 2) &&
        (Opt->Method == SUMA_LB_FEM || Opt->Method == SUMA_LM) ) {
      fprintf (SUMA_STDERR, "Number of iterations must be a multiple of 2.\n%d is not a multiple of 2.\n", Opt->N_iter);
      exit(1);
   }
   
   if (Opt->ShowNode < 0 && Opt->ShowOffset_DBG) {
      fprintf (SUMA_STDERR,"Error %s:\nBad debug node index (%d) in option -dbg_n\n", FuncName, Opt->ShowNode);
      exit (1);
   }
   
   if (Opt->insurf_method == 1) {
      SUMA_SL_Err("Obsolete method for surface specification.\nShould not have gotten here.");
      exit(1);
   }
      
   /* can't test for file existence here because of square brackets */
   if (0 && Opt->in_name && !SUMA_filexists(Opt->in_name)) {
      fprintf (SUMA_STDERR,"Error %s:\n%s not found.\n", FuncName, Opt->if_name);
      exit(1);
   }
   
   if (Opt->Method == SUMA_NO_METH) {
      fprintf (SUMA_STDERR,"Error %s:\nNo method was specified.\n", FuncName);
      exit(1);  
   }
   
   if (ps->cs->talk_suma && Opt->insurf_method != 2) {
      fprintf (SUMA_STDERR,   "must specify surface using -spec option\n"
                              "if you whish to talk to suma.\n");
      exit(1); 
   }
   
   if (0 && ps->cs->talk_suma && Opt->Method != SUMA_LB_FEM) {
      fprintf (SUMA_STDERR,   "talk option only valid with -LB_FEM\n");
      exit(1); 
   }
   
   if (Opt->insurf_method == 2) {
      if (!Opt->surf_names[0] || !Opt->spec_file) {
         fprintf (SUMA_STDERR,   "failed to specify either -spec or -surf_X options.\n");
         exit(1);  
      }
   }
    
   if (outname) {
      if (SUMA_filexists(outname)) {
         fprintf (SUMA_STDERR,"Error %s:\noutput file %s exists.\n", FuncName, outname);
         exit(1);
      }
      Opt->out_name = SUMA_copy_string(outname);
   } else {
      switch (Opt->Method) {
         case SUMA_LB_FEM:
            /* form autoname  */
            Opt->out_name = SUMA_Extension(Opt->in_name, ".1D", YUP); /*remove .1D */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name,"_sm", "", 1); /* add _sm to prefix */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name,".1D", "", 1); /* add .1D */
            break;
         case SUMA_LM:
            /* form autoname  */
            Opt->out_name = SUMA_copy_string("NodeList_sm.1D");
            break;
         case SUMA_BRUTE_FORCE:
            /* form autoname  */
            Opt->out_name = SUMA_copy_string("NodeList_Offsetsm.1D");
            break;
         case SUMA_NN_GEOM:
            /* form autoname  */
            Opt->out_name = SUMA_copy_string("NodeList_NNsm.1D");
            break;
         default:
            fprintf (SUMA_STDERR,"Error %s:\nNot ready for this option here.\n", FuncName);
            exit(1);
            break;
      }
      if (SUMA_filexists(Opt->out_name)) {
         fprintf (SUMA_STDERR,"Error %s:\noutput file %s exists.\n", FuncName, Opt->out_name);
         exit(1);
      }
   }

   /* method specific checks */
   switch (Opt->Method) {
      case SUMA_LB_FEM:
         if (!Opt->in_name) {
            fprintf (SUMA_STDERR,"Error %s:\ninput data not specified.\n", FuncName);
            exit(1);
         }
         if (Opt->fwhm ==  -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\n-fwhm option must be used with -met LB_FEM.\n", FuncName); 
            exit(1);
         }else if (Opt->fwhm <= 0.0) {
            fprintf (SUMA_STDERR,"Error %s:\nFWHM must be > 0\n", FuncName);
            exit(1);
         }
         if (Opt->kpb >= 0) {
            fprintf (SUMA_STDERR,"Error %s:\n-kpb option is not valid with -met LB_FEM.\n", FuncName); 
            exit(1);
         }         
         
         break;
      case SUMA_BRUTE_FORCE:
         
         break;
      case SUMA_LM:
         
         if ( (Opt->l != -1.0 || Opt->m != -1.0) && Opt->kpb != -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\nYou cannot mix options -kpb and -lm \n", FuncName);
            exit(1);
         }
         if (Opt->kpb != -1.0 && (Opt->kpb < 0.000001 || Opt->kpb > 10)) {
            fprintf (SUMA_STDERR,"Error %s:\nWith -kpb k option, you should satisfy 0 < k < 10\n", FuncName);
            exit(1);
         }
         if (Opt->l == -1.0 && Opt->m == -1.0 && Opt->kpb == -1.0) {
            Opt->kpb = 0.1;
         }

         if (Opt->l == -1.0 || Opt->m == -1.0) { /* convert kpb into l and m */
            if (!SUMA_Taubin_Smooth_Coef (Opt->kpb, &(Opt->l), &(Opt->m))) {
               SUMA_SL_Err("Failed to find smoothing coefficients");
               exit(1);            
            }
         } 

         if (Opt->in_name) {
            fprintf (SUMA_STDERR,"Error %s:\nOption -input not valid with -met LM.\n", FuncName);
            exit(1);
         }
         
         if (Opt->fwhm !=  -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\nOption -fwhm not valid with -met LM.\n", FuncName);
            exit(1);
         }
         
         break;
      case SUMA_NN_GEOM:
         if (Opt->in_name) {
            fprintf (SUMA_STDERR,"Error %s:\nOption -input not valid with -met NN_geom.\n", FuncName);
            exit(1);
         }
         
         if (0 && Opt->lim > 1000) {
            fprintf (SUMA_STDERR,"Error %s:\n-lim option not specified.\n", FuncName);
            exit(1);
         }
         
         if (Opt->fwhm !=  -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\nOption -fwhm not valid with -met NN_geom.\n", FuncName);
            exit(1);
         }
         break;
         
      default:
         fprintf (SUMA_STDERR,"Error %s:\nNot ready for this option here.\n", FuncName);
         exit(1);
         break;
   }
   
   SUMA_RETURN (Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfSmooth"}; 
	int kar, icol, nvec, ncol=0, i, ii;
   float *data_old = NULL, *far = NULL;
   float **DistFirstNeighb;
   void *SO_name = NULL;
   SUMA_SurfaceObject *SO = NULL, *SOnew = NULL;
   MRI_IMAGE *im = NULL;
   SUMA_SFname *SF_name = NULL;
   struct  timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all;   
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   SUMA_SURFSMOOTH_OPTIONS *Opt;  
   FILE *fileout=NULL; 
   float **wgt=NULL, *dsmooth=NULL;
   SUMA_INDEXING_ORDER d_order=SUMA_NO_ORDER;
   SUMA_COMM_STRUCT *cs = NULL;
	SUMA_SurfSpecFile Spec; 
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;
   
   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-i;-sv;-talk;");
   
   if (argc < 6)
       {
          usage_SUMA_SurfSmooth();
          exit (1);
       }
   
   Opt = SUMA_SurfSmooth_ParseInput (argv, argc, ps);
   cs = ps->cs;
   if (!cs) exit(1);
   
   /* now for the real work */
   if (Opt->insurf_method == 1) { /* method 1 */
      SUMA_SL_Err("Input in this method is no longer supported.\n");
      exit(1);
   } else { /* method 2 */
      int SO_read = -1;
      
      if (!SUMA_Read_SpecFile (Opt->spec_file, &Spec)) {
			fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
			exit(1);
		}
      SO_read = SUMA_spec_select_surfs(&Spec, Opt->surf_names, SURFSMOOTH_MAX_SURF, 0);
      if ( SO_read != 1 )
      {
	      fprintf(SUMA_STDERR,"Error %s: Found %d surfaces, expected only 1.\n", FuncName,  SO_read);
         exit(1);
      }
      /* now read into SUMAg_DOv */
      if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, Opt->sv_name, 0, SUMAg_CF->DsetList) ) {
	      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
         exit(1);
      }
      /* now identify surface needed */
      SO = SUMA_find_named_SOp_inDOv(Opt->surf_names[0], SUMAg_DOv, SUMAg_N_DOv);
   }
   
   if (!SO) {
      fprintf (SUMA_STDERR,"Error %s: Failed to read input surface.\n", FuncName);
      exit (1);
   }

   if (Opt->ShowNode >= 0 && Opt->ShowNode >= SO->N_Node) {
      fprintf (SUMA_STDERR,"Error %s: Requesting debugging info for a node index (%d) \n"
                           "that does not exist in a surface of %d nodes.\nRemember, indexing starts at 0.\n", 
                           FuncName, Opt->ShowNode, SO->N_Node);
      exit (1);
   }
   
      
   /* form EL and FN */
   if (!SO->EL || !SO->FN) {
      /* normally you'd call SUMA_SurfaceMetrics_eng (SO, "EdgeList", NULL, 0) 
      but that should be done in SUMA_SurfaceMetrics_eng. */
      SUMA_SLP_Err("Unexpexted NULL SO->EL or SO->FN");
      exit(1); 
   }

   
   /* see if SUMA talk is turned on */
   if (ps->cs->talk_suma) {
      cs->istream = SUMA_GEOMCOMP_LINE;
      
      if (Opt->Method == SUMA_LB_FEM) { 
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
      }else if (Opt->Method == SUMA_LM) { 
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
      }else if (Opt->Method == SUMA_NN_GEOM) { 
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
      }else if (Opt->Method == SUMA_BRUTE_FORCE) { 
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
      }else {
         SUMA_SL_Err("Can't talk to suma with the chosen method.\n");
         ps->cs->talk_suma = NOPE;
      }
   }
  
   ncol = 3; /* default for geometry smoothing. That is changed below for data smoothing. */
   switch (Opt->Method) {
      case SUMA_LB_FEM: 
         /* Moo Chung's method for interpolation weights */
         {
            /* now load the input data */
            im = mri_read_1D (Opt->in_name);

            if (!im) {
               SUMA_SL_Err("Failed to read 1D file");
               exit(1);
            }

            far = MRI_FLOAT_PTR(im);
            nvec = im->nx;
            ncol = im->ny;
            d_order = SUMA_COLUMN_MAJOR;
            
            if (!nvec) {
               SUMA_SL_Err("Empty file");
               exit(1);
            }
            if (nvec != SO->N_Node) {
               fprintf(SUMA_STDERR, "Error %s:\n"
                                    "Expecting 1D file to have %d rows\n"
                                    "                    found %d rows instead.\n",
                                     FuncName, SO->N_Node, nvec);
               exit(1); 
            }
            if (LocalHead) SUMA_etime(&start_time,0);
            wgt = SUMA_Chung_Smooth_Weights(SO);
            if (!wgt) {
               SUMA_SL_Err("Failed to compute weights.\n");
               exit(1);
            }
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: weight computation took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            
            dsmooth = SUMA_Chung_Smooth (SO, wgt, Opt->N_iter, Opt->fwhm, far, ncol, SUMA_COLUMN_MAJOR, NULL, cs);
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            #if 0
            /* writing is now done below ... */
            fileout = fopen(Opt->out_name, "w");
            if (Opt->AddIndex) SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, d_order, fileout, YUP);
            else SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, d_order, fileout, NOPE);
            fclose(fileout); fileout = NULL;

            if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
            #endif
            if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;
         }
         break;
         
      case SUMA_NN_GEOM:
         /* brute forcem nearset neighbor interpolation */
         {
            if (LocalHead) SUMA_etime(&start_time,0);
            
            d_order =  SUMA_ROW_MAJOR; 
            
            dsmooth = SUMA_NN_GeomSmooth( SO, Opt->N_iter, SO->NodeList,
                                          3, d_order, NULL, cs);
            if (0 && LocalHead) {
               SUMA_LH("See dsmooth.1D");
               SUMA_disp_vecmat (dsmooth, SO->N_Node, 3, 1,  d_order, NULL, YUP);
            }
            if (!dsmooth) {
               SUMA_SL_Err("Failed in SUMA_NN_Geom_Smooth");
               exit(1);
            }
            
            /* writing of results is done below */
         }
         break;  
      case SUMA_BRUTE_FORCE:
         /* a method that will likely be dropped NOT FINISHED  */
         {
            if (LocalHead) SUMA_etime(&start_time,0);
            
            d_order =  SUMA_ROW_MAJOR; 
            
            SUMA_etime(&start_time_all,0);
            dsmooth = SUMA_Offset_GeomSmooth( SO, Opt->N_iter, Opt->OffsetLim, SO->NodeList,
                                              3, d_order, NULL, cs);
            if (0 && LocalHead) {
               SUMA_LH("See dsmooth.1D");
               SUMA_disp_vecmat (dsmooth, SO->N_Node, 3, 1,  d_order, NULL, YUP);
            }
            if (!dsmooth) {
               SUMA_SL_Err("Failed in SUMA_Offset_Geom_Smooth");
               exit(1);
            }
            
            /* writing of results is done below */
            
            etime_GetOffset_all = SUMA_etime(&start_time_all,1);
            fprintf(SUMA_STDERR, "%s: Done.\nSearch to %f mm took %f minutes for %d nodes.\n" , 
                                 FuncName, Opt->lim, etime_GetOffset_all / 60.0 , SO->N_Node);

         }
         break;
      case SUMA_LM:
         /* Taubin's */
         {
            
            if (LocalHead) SUMA_etime(&start_time,0);
            
            d_order =  SUMA_ROW_MAJOR; 
            dsmooth = SUMA_Taubin_Smooth (SO, NULL, 
                            Opt->l, Opt->m, SO->NodeList, 
                            Opt->N_iter, 3, d_order,
                            NULL, cs, NULL); 

            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                                    "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            /* writing of results is done below */
         }
         break;
      default:
         SUMA_SL_Err("Bad method, should not be here.");
         exit(1);
         break;
   }
   
   
   if (Opt->Method == SUMA_BRUTE_FORCE || Opt->Method == SUMA_NN_GEOM)
   {
      if (Opt->MatchMethod) {   
         SUMA_LH("Fixing shrinkage...");
         /* Create a surface that is a descendant of SO, use the new coordinates */
         SOnew = SUMA_CreateChildSO( SO,
                                     dsmooth, SO->N_Node,
                                     NULL, -1,
                                     0); /* SOnew->NodeList is now == dsmooth */

         /* fix the shrinking ..*/

         switch (Opt->MatchMethod) {
            case 1:
               if (!SUMA_EquateSurfaceSize(SOnew, SO, Opt->lim, cs)) {
                  SUMA_SL_Warn("Failed to fix surface size.\nTrying to finish ...");
               }

               /* send the unshrunk bunk */
               if (cs->Send) {
                  SUMA_LH("Sending last fix to SUMA ...");
                  if (!SUMA_SendToSuma (SO, cs, (void *)SOnew->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }

               /* to make matters parallel with the other methods, keep dsmooth and free SOnew */
               dsmooth = SOnew->NodeList; /* CHECK IF THAT's the case here... coordinates have a new pointer after Equating surface size */
               SOnew->NodeList = NULL; /* new coordinates will stay alive in dsmooth */
               SUMA_Free_Surface_Object(SOnew); SOnew=NULL;

               break;
            case 2:
               if (!SUMA_EquateSurfaceVolumes(SOnew, SO, Opt->lim, cs)) {
                  SUMA_SL_Warn("Failed to fix surface size.\nTrying to finish ...");
               }

               /* send the unshrunk bunk */
               if (cs->Send) {
                  SUMA_LH("Sending last fix to SUMA ...");
                  if (!SUMA_SendToSuma (SO, cs, (void *)SOnew->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
               /* to make matters parallel with the other methods, keep dsmooth and free SOnew */
               dsmooth = SOnew->NodeList; /* coordinates have a new pointer after Equating surface volumes */
               SOnew->NodeList = NULL; /* new coordinates will stay alive in dsmooth */
               SUMA_Free_Surface_Object(SOnew); SOnew=NULL;
               break;
            case 3:
               if (!SUMA_EquateSurfaceAreas(SOnew, SO, Opt->lim, cs)) {
                  SUMA_SL_Warn("Failed to fix surface size.\nTrying to finish ...");
               }

               /* send the unshrunk bunk */
               if (cs->Send) {
                  SUMA_LH("Sending last fix to SUMA ...");
                  if (!SUMA_SendToSuma (SO, cs, (void *)SOnew->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
               /* to make matters parallel with the other methods, keep dsmooth and free SOnew */
               dsmooth = SOnew->NodeList; /* coordinates have a new pointer after Equating surface volumes */
               SOnew->NodeList = NULL; /* new coordinates will stay alive in dsmooth */
               SUMA_Free_Surface_Object(SOnew); SOnew=NULL;
               break;
            case 4:
               if (!SUMA_ProjectSurfaceToSphere(SOnew, SO, Opt->lim, cs)) {
                  SUMA_SL_Warn("Failed to fix surface size.\nTrying to finish ...");
               }

               /* send the unshrunk bunk */
               if (cs->Send) {
                  SUMA_LH("Sending last fix to SUMA ...");
                  if (!SUMA_SendToSuma (SO, cs, (void *)SOnew->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }

               /* to make matters parallel with the other methods, keep dsmooth and free SOnew */
               dsmooth = SOnew->NodeList; /* CHECK IF THAT's the case here... coordinates have a new pointer after Equating surface size */
               SOnew->NodeList = NULL; /* new coordinates will stay alive in dsmooth */
               SUMA_Free_Surface_Object(SOnew); SOnew=NULL;
               break;
            case 0:
               break;
            default:
               SUMA_SL_Err("Huh ?");
               break;   
         }
      }
      if (LocalHead) {
         etime_GetOffset = SUMA_etime(&start_time,1);
         fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                              "Projected time per 100000 nodes is: %f minutes\n", 
                                 FuncName, etime_GetOffset, SO->N_Node, 
                                 etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
      }

   }
   
   /* write out the filtered geometry. Should not be executed for data smoothing */
   if (Opt->surf_out) {
      if (!dsmooth) {
         SUMA_SL_Err("NULL dsmooth for geometry smoothing. Either failed to smooth or logical error.");
         exit(1);
      }
       
      SUMA_free(SO->NodeList); SO->NodeList = dsmooth; dsmooth = NULL; /* replace NodeList */
      switch (SO->FileType) {
         case SUMA_SUREFIT:
            SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
            sprintf(SF_name->name_coord,"%s", Opt->surf_out);
            SF_name->name_topo[0] = '\0'; 
            SO_name = (void *)SF_name;
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_SUREFIT, SUMA_ASCII, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;
         case SUMA_VEC:
            SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
            sprintf(SF_name->name_coord,"%s", Opt->surf_out);
            SF_name->name_topo[0] = '\0';
            SO_name = (void *)SF_name;
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_VEC, SUMA_ASCII, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;
         case SUMA_FREE_SURFER:
            SO_name = (void *)Opt->surf_out; 
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_FREE_SURFER, SUMA_ASCII, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;
         case SUMA_FREE_SURFER_PATCH:
            fprintf (SUMA_STDERR,"Error %s: No support for writing Free Surfer patches.\n", FuncName);
            exit (1);  
            break;
         case SUMA_PLY:
            SO_name = (void *)Opt->surf_out; 
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_PLY, SUMA_FF_NOT_SPECIFIED, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;
         case SUMA_OPENDX_MESH:
            SO_name = (void *)Opt->surf_out; 
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_OPENDX_MESH, SUMA_ASCII, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;  
         default:
            fprintf (SUMA_STDERR,"Error %s: Bad format.\n", FuncName);
            exit(1);
      }
   } else {
      if (!dsmooth) {
         SUMA_SL_Err("NULL dsmooth for data smoothing. Either failed to smooth or logical error.");
         exit(1);
      }      
      fileout = fopen(Opt->out_name, "w");
      if (Opt->AddIndex) SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, d_order, fileout, YUP);
      else SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, d_order, fileout, NOPE);
      fclose(fileout); fileout = NULL;
   }



   /* you don't want to exit rapidly because the SUMA might not be done processing the last elements*/
   if (cs->Send && !cs->GoneBad) {
      /* cleanup and close connections */
      if (Opt->Method == SUMA_LB_FEM) {
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NODE_RGBAb, 2)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
         }
      }else if (Opt->Method == SUMA_LM) {
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NODE_XYZ, 2)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
         }
      }else if (Opt->Method == SUMA_NN_GEOM) {
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NODE_XYZ, 2)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
         }
      }
   }   
      
   
   SUMA_LH("clean up");
   if (SO->NodeList != dsmooth) {
      SUMA_LH("Freeing dsmooth...:");
      if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
      SUMA_LH("Done:");
   }
   mri_free(im); im = NULL;   /* done with that baby */
   if (cs) cs = NULL; /* ps->cs if freed below */
   if (SF_name) SUMA_free(SF_name);
   if (Opt->insurf_method == 1) { if (SO) SUMA_Free_Surface_Object(SO); }
   if (data_old) SUMA_free(data_old);  
   if (Opt->out_name) SUMA_free(Opt->out_name); Opt->out_name = NULL;
   if (Opt) SUMA_free(Opt);
 	if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }

   exit(0);
}
#endif

#ifdef SUMA_getPatch_STANDALONE
#define SURFPATCH_MAX_SURF 10  /*!< Maximum number of input surfaces */

void usage_SUMA_getPatch ()
   {
      static char FuncName[]={"usage_SUMA_getPatch"};
      char * s = NULL;
      s = SUMA_help_basics();
      printf ( "\nUsage:\n"
               "  SurfPatch <-spec SpecFile> <-surf_A insurf> <-surf_B insurf> ...\n"
               "            <-input nodefile inode ilabel> <-prefix outpref>  \n"
               "            [-hits min_hits] [-masklabel msk] [-vol]\n"
               "\n"
               "Usage 1:\n"
               "  The program creates a patch of surface formed by nodes \n"
               "  in nodefile.\n"
               "  Mandatory parameters:\n"
               "     -spec SpecFile: Spec file containing input surfaces.\n"
               "     -surf_X: Name of input surface X where X is a character\n"
               "              from A to Z. If surfaces are specified using two\n"
               "              files, use the name of the node coordinate file.\n"
               "     -input nodefile inode ilabel: \n"
               "            nodefile is the file containing nodes defining the patch.\n"
               "            inode is the index of the column containing the nodes\n"
               "            ilabel is the index of the column containing labels of\n"
               "                   the nodes in column inode. If you want to use\n"
               "                   all the nodes in column indode, then set this \n"
               "                   parameter to -1 (default). \n"
               "                   If ilabel is not equal to 0 then the corresponding \n"
               "                   node is used in creating the patch.\n"
               "                   See -masklabel option for one more variant.\n"
               "     -prefix outpref: Prefix of output patch. If more than one surface\n"
               "                      are entered, then the prefix will have _X added\n"
               "                      to it, where X is a character from A to Z.\n"
               "                      Output format depends on the input surface's.\n"
               "                      With that setting, checking on pre-existing files\n"
               "                      is only done before writing the new patch, which is\n"
               "                      annoying. You can set the output type ahead of time\n"
               "                      using -out_type option. This way checking for pre-existing\n"
               "                      output files can be done at the outset.\n"
               "\n" 
               "  Optional parameters:\n"
               "     -out_type TYPE: Type of all output patches, regardless of input surface type.\n"
               "                     Choose from: FreeSurfer, SureFit, 1D and Ply.\n"
               "     -hits min_hits: Minimum number of nodes specified for a triangle\n"
               "                     to be made a part of the patch (1 <= min_hits <= 3)\n"
               "                     default is 2.\n"
               "     -masklabel msk: If specified, then only nodes that are labeled with\n"
               "                     with msk are considered for the patch.\n"
               "                     This option is useful if you have an ROI dataset file\n"
               "                     and whish to create a patch from one out of many ROIs\n"
               "                     in that file. This option must be used with ilabel \n"
               "                     specified (not = -1)\n"
               "\n"
               "Usage 2:\n"
               "  The program can also be used to calculate the volume between the same patch\n"
               "  on two isotopic surfaces. See -vol option below.\n"
               "      -vol: Calculate the volume formed by the patch on surf_A and\n"
               "            and surf_B. For this option, you must specify two and\n"
               "            only two surfaces with surf_A and surf_B options.\n"
               "      -vol_only: Only calculate the volume, don't write out patches.\n"
               "\n"
               "%s"
               "\n",s); SUMA_free(s); s = NULL;
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
       exit (0);
   }

typedef struct {
   SUMA_SO_File_Type iType;
   SUMA_SO_File_Type oType;
   char *out_prefix;
   char *sv_name;
   char *surf_names[SURFPATCH_MAX_SURF];
   int N_surf;
   char *spec_file;
   char *in_name;
   int minhits;
   int thislabel;
   int labelcol;
   int nodecol;
   int DoVol;
   int VolOnly;
} SUMA_GETPATCH_OPTIONS;

/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_GETPATCH_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_prefix); 
               SUMA_free(Opt);
*/
SUMA_GETPATCH_OPTIONS *SUMA_GetPatch_ParseInput (char *argv[], int argc)
{
   static char FuncName[]={"SUMA_GetPatch_ParseInput"}; 
   SUMA_GETPATCH_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outprefix;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_GETPATCH_OPTIONS *)SUMA_malloc(sizeof(SUMA_GETPATCH_OPTIONS));

   kar = 1;
   Opt->iType = SUMA_FT_NOT_SPECIFIED;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->spec_file = NULL;
   Opt->in_name = NULL;
   Opt->minhits = 2;
   Opt->labelcol = -1;
   Opt->nodecol = -1;
   Opt->thislabel = -1;
   Opt->N_surf = -1;
   Opt->DoVol = 0;
   Opt->VolOnly = 0;
   Opt->oType = SUMA_FT_NOT_SPECIFIED;
   for (i=0; i<SURFPATCH_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
	brk = NOPE;
   
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_getPatch();
          exit (0);
		}
		
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec \n");
				exit (1);
			}
			Opt->spec_file = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-hits") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -hits \n");
				exit (1);
			}
			Opt->minhits = atoi(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-masklabel") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -masklabel \n");
				exit (1);
			}
			Opt->thislabel = atoi(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-vol") == 0)) {
			Opt->DoVol = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-vol_only") == 0)) {
			Opt->DoVol = 1;
         Opt->VolOnly = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix \n");
				exit (1);
			}
			Opt->out_prefix = SUMA_copy_string(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-out_type") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -out_type \n");
				exit (1);
			}
			Opt->oType = SUMA_guess_surftype_argv(argv[kar]);
         brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar+2 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 3 arguments after -input \n");
				exit (1);
			}
			Opt->in_name = argv[kar]; kar ++;
         Opt->nodecol = atoi(argv[kar]); kar ++;
         Opt->labelcol = atoi(argv[kar]); 
			brk = YUP;
		}
      
      if (!brk && (strncmp(argv[kar], "-surf_", 6) == 0)) {
			if (kar + 1>= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -surf_X SURF_NAME \n");
				exit (1);
			}
			ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFPATCH_MAX_SURF) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option is out of range.\n");
				exit (1);
         }
         kar ++;
         Opt->surf_names[ind] = argv[kar];
         Opt->N_surf = ind+1;
         brk = YUP;
		}
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
      
   }
   
   /* sanity checks */
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("SurfPatch");
   
   if (Opt->thislabel >= 0 && Opt->labelcol < 0) {
      SUMA_SL_Err("Cannot use -masklabel without specifying ilabel in -input option");
      exit(1);
   } 
   if (Opt->minhits < 1 || Opt->minhits > 3) {
      SUMA_SL_Err("minhits must be > 0 and < 3");
      exit(1);
   }
   if (Opt->N_surf < 1) {
      SUMA_SL_Err("No surface specified.");
      exit(1);
   }
   if (!Opt->in_name) {
      SUMA_SL_Err("No input specified.");
      exit(1);
   }
   if (Opt->DoVol && Opt->N_surf != 2) {
      SUMA_SL_Err("Must specify 2 and only 2 surfaces with -vol options");
      exit(1);
   }
   SUMA_RETURN (Opt);
     
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfPatch"};
   SUMA_GETPATCH_OPTIONS *Opt; 
   char *ppref=NULL, ext[5]; 
   float *far=NULL;
   MRI_IMAGE *im = NULL;
   int SO_read = -1;
   int *NodePatch=NULL, N_NodePatch=-1, *FaceSetList=NULL , N_FaceSet = -1;          
   int i, inodeoff=-1, ilabeloff=-1, nvec, ncol, cnt;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_PATCH *ptch = NULL; 
   SUMA_SurfSpecFile Spec;
   SUMA_INDEXING_ORDER d_order;
   void *SO_name = NULL;
   SUMA_Boolean exists = NOPE;
   SUMA_SO_File_Type typetmp;
   SUMA_Boolean LocalHead = NOPE;
	
   SUMA_mainENTRY;
   
   SUMA_STANDALONE_INIT;
   
   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   if (argc < 4)
       {
          usage_SUMA_getPatch();
          exit (1);
       }
   
   Opt = SUMA_GetPatch_ParseInput (argv, argc);
   
   if (Opt->oType != SUMA_FT_NOT_SPECIFIED && !Opt->VolOnly) { 
      for (i=0; i < Opt->N_surf; ++i) {
         if (Opt->N_surf > 1) {
            sprintf(ext, "_%c", 65+i);
            ppref = SUMA_append_string(Opt->out_prefix, ext);
         } else {
            ppref = SUMA_copy_string(Opt->out_prefix);
         }
         
         SO_name = SUMA_Prefix2SurfaceName(ppref, NULL, NULL, Opt->oType, &exists);
         if (exists) {
            fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, ppref);
            exit(1);
         }
         if (ppref) SUMA_free(ppref); ppref = NULL; 
         if (SO_name) SUMA_free(SO_name); SO_name = NULL;
      } 
   }
   
   /* read all surfaces */
   if (!SUMA_Read_SpecFile (Opt->spec_file, &Spec)) {
		fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
		exit(1);
	}
   SO_read = SUMA_spec_select_surfs(&Spec, Opt->surf_names, SURFPATCH_MAX_SURF, 0);
   if ( SO_read != Opt->N_surf )
   {
	   if (SO_read >=0 )
         fprintf(SUMA_STDERR,"Error %s:\nFound %d surfaces, expected %d.\n", FuncName,  SO_read, Opt->N_surf);
      exit(1);
   }
   /* now read into SUMAg_DOv */
   if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, Opt->sv_name, 0, SUMAg_CF->DsetList) ) {
	   fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
      exit(1);
   }
   
   /* read in the file containing the node information */
   im = mri_read_1D (Opt->in_name);

   if (!im) {
      SUMA_SL_Err("Failed to read 1D file");
      exit(1);
   }

   far = MRI_FLOAT_PTR(im);
   nvec = im->nx;
   ncol = im->ny;
   if (Opt->nodecol >= ncol || Opt->labelcol >= ncol) {
      fprintf(SUMA_STDERR, "\n"
                           "Error %s: Input file has a total of %d columns.\n"
                           "One or both user-specified node (%d) and \n"
                           "label (%d) columns are too high. Maximum usable\n"
                           "column index is %d.\n"
                           , FuncName, ncol, Opt->nodecol, Opt->labelcol, ncol -1 );
      exit(1);
   }
   
   d_order = SUMA_COLUMN_MAJOR;

   if (!nvec) {
      SUMA_SL_Err("Empty file");
      exit(1);
   }
   /* form the node vector */
   NodePatch = (int *)SUMA_malloc(sizeof(int)*nvec);
   if (!NodePatch) {
      SUMA_SL_Crit("Failed to allocate.");
      exit(1);
   }
   inodeoff = Opt->nodecol*nvec;
   if (Opt->labelcol < 0) { /* all listed nodes */ 
      for (i=0; i<nvec; ++i) {
         NodePatch[i] = far[i+inodeoff];
      }
      N_NodePatch = nvec;
   } else {
      ilabeloff =  Opt->labelcol*nvec;
      if (Opt->thislabel < 0) { /* all nodes with non zero labels */
         cnt = 0;
         for (i=0; i<nvec; ++i) {
            if (far[i+ilabeloff]) {
               NodePatch[cnt] = far[i+inodeoff];
               ++cnt;
            }
         }
         N_NodePatch = cnt;     
      } else { /* select labels */
         cnt = 0;
         for (i=0; i<nvec; ++i) {
            if (far[i+ilabeloff] == Opt->thislabel) {
               NodePatch[cnt] = far[i+inodeoff];
               ++cnt;
            }
         }
         N_NodePatch = cnt;    
      }
      NodePatch = (int *) SUMA_realloc(NodePatch , sizeof(int)*N_NodePatch);
   }
   
   /* done with im, free it */
   mri_free(im); im = NULL;   
   
   if (Opt->DoVol) {
      SUMA_SurfaceObject *SO1 = SUMA_find_named_SOp_inDOv(Opt->surf_names[0], SUMAg_DOv, SUMAg_N_DOv);
      SUMA_SurfaceObject *SO2 = SUMA_find_named_SOp_inDOv(Opt->surf_names[1], SUMAg_DOv, SUMAg_N_DOv);
      double Vol = 0.0;
      
      if (!SO1 || !SO2) {
         SUMA_SL_Err("Failed to load surfaces.");
         exit(1);
      }
      /* a chunk used to test SUMA_Pattie_Volume */
      Vol = SUMA_Pattie_Volume(SO1, SO2, NodePatch, N_NodePatch, NULL, Opt->minhits);
      fprintf (SUMA_STDERR,"Volume = %f\n", fabs(Vol));
   }
   
   
   if (!Opt->VolOnly) {
      FaceSetList = NULL;
      N_FaceSet = -1;
      for (i=0; i < Opt->N_surf; ++i) {/* loop to read in surfaces */
         /* now identify surface needed */
         SO = SUMA_find_named_SOp_inDOv(Opt->surf_names[i], SUMAg_DOv, SUMAg_N_DOv);
         if (!SO) {
            fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Failed to find surface %s\n"
                                 "in spec file. Use full name.\n",
                                 FuncName, Opt->surf_names[i]);
            exit(1);
         }
         /* extract the patch */
         if (!SO->MF) {
            SUMA_SL_Warn ("NULL MF");
         }
         ptch = SUMA_getPatch (NodePatch, N_NodePatch, SO->FaceSetList, SO->N_FaceSet, SO->MF, Opt->minhits);
         if (!ptch) {
            SUMA_SL_Err("Failed to form patch.");
            exit(1);
         }
         if (LocalHead) SUMA_ShowPatch(ptch, NULL);

         /* Now create a surface with that patch */
         if (Opt->N_surf > 1) {
            sprintf(ext, "_%c", 65+i);
            ppref = SUMA_append_string(Opt->out_prefix, ext);
         } else {
            ppref = SUMA_copy_string(Opt->out_prefix);
         }
         /* save the original type */
         typetmp = SO->FileType;
         if (Opt->oType != SUMA_FT_NOT_SPECIFIED) SO->FileType = Opt->oType;
         SO_name = SUMA_Prefix2SurfaceName(ppref, NULL, NULL, SO->FileType, &exists);
         if (ppref) SUMA_free(ppref); ppref = NULL;
         /* save the original pointers to the facesets and their number */
         FaceSetList = SO->FaceSetList;
         N_FaceSet = SO->N_FaceSet;
         /* replace with Patch */
         SO->FaceSetList = ptch->FaceSetList;
         SO->N_FaceSet = ptch->N_FaceSet; 
         if (SO->N_FaceSet <= 0) {
            SUMA_S_Warn("The patch is empty.\n Non existing surface not written to disk.\n");
         } else {
            if (!SUMA_Save_Surface_Object (SO_name, SO, SO->FileType, SUMA_ASCII, NULL)) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
                  exit (1);
            }
         }
         /* bring SO back to shape */
         SO->FileType = typetmp;
         SO->FaceSetList = FaceSetList; FaceSetList = NULL;
         SO->N_FaceSet = N_FaceSet; N_FaceSet = -1;
         if (SO_name) SUMA_free(SO_name); SO_name = NULL;
         if (ptch) SUMA_freePatch(ptch); ptch = NULL;
      }
   } 
   
   SUMA_LH("clean up");
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt) SUMA_free(Opt);   
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {SUMA_SL_Err("SUMAg_CF Cleanup Failed!");}
   
   SUMA_RETURN(0);
} 
#endif
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
   c[0] = c[1] = c[2];
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
SUMA_Boolean *SUMA_MaskOfNodesInPatch(SUMA_SurfaceObject *SO, int *N_NodesUsedInPatch)
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

   NodesInPatchMesh = (SUMA_Boolean *)SUMA_calloc(SO->N_Node, sizeof(SUMA_Boolean)); 
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
   int * BeenSelected;
   int i, j, node, ip, ip2, NP;
   SUMA_PATCH *Patch;
   static char FuncName[]={"SUMA_getPatch"};
   
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
   static char FuncName[]={"SUMA_freePatch"};
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
SUMA_CONTOUR_EDGES * SUMA_GetContour (SUMA_SurfaceObject *SO, int *Nodes, int N_Node, int *N_ContEdges, int ContourMode, SUMA_PATCH *UseThisPatch)
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
      SEL = SUMA_Make_Edge_List_eng (Patch->FaceSetList, Patch->N_FaceSet, SO->N_Node, SO->NodeList, 0, NULL);
   
      if (0 && LocalHead) SUMA_Show_Edge_List (SEL, NULL);
      /* allocate for maximum */
      CE = (SUMA_CONTOUR_EDGES *) SUMA_malloc(SEL->N_EL * sizeof(SUMA_CONTOUR_EDGES));
      if (!CE) {
         SUMA_SLP_Crit("Failed to allocate for CE");
         SUMA_RETURN(CE);
      }
   
      switch (ContourMode) {
         case 0: /* a pretty contour, edges used here may not be the outermost of the patch */
            /* edges that are part of unfilled triangles are good */
            i = 0;
            *N_ContEdges = 0;
            while (i < SEL->N_EL) {
               if (SEL->ELps[i][2] == 2) {
                  Tri1 = SEL->ELps[i][1];
                  Tri2 = SEL->ELps[i+1][1];
                  sHits = Patch->nHits[Tri1] + Patch->nHits[Tri2];
                  if (sHits == 5 || sHits == 4) { /* one tri with 3 hits and one with 2 hits or 2 Tris with 2 hits each */
                     /* Pick edges that are part of only one triangle with three hits */
                                                 /* or two triangles with two hits */
                     /* There's one more condition, both nodes have to be a part of the original list */
                     if (isNode[SEL->EL[i][0]] && isNode[SEL->EL[i][1]]) {
                        CE[*N_ContEdges].n1 = SEL->EL[i][0];
                        CE[*N_ContEdges].n2 = SEL->EL[i][1];
                        ++ *N_ContEdges;

                        if (LocalHead) {
                           fprintf (SUMA_STDERR,"%s: Found edge made up of nodes [%d %d]\n",
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
         case 1: /* outermost contour, not pretty, but good for getting the outermost edge */
            i = 0;
            *N_ContEdges = 0;
            while (i < SEL->N_EL) {
               if (SEL->ELps[i][2] == 1) {
                  CE[*N_ContEdges].n1 = SEL->EL[i][0];
                  CE[*N_ContEdges].n2 = SEL->EL[i][1];
                  ++ *N_ContEdges;
                  if (LocalHead) {
                           fprintf (SUMA_STDERR,"%s: Found edge made up of nodes [%d %d]\n",
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
         CE = (SUMA_CONTOUR_EDGES *) SUMA_realloc (CE, *N_ContEdges * sizeof(SUMA_CONTOUR_EDGES));
         if (!CE) {
            SUMA_SLP_Crit("Failed to reallocate for CE");
            SUMA_RETURN(CE);
         }
      }
         
      SUMA_free_Edge_List (SEL); 
   }
   
   if (!UseThisPatch) {
      SUMA_freePatch (Patch); 
   }
   Patch = NULL;
   
   SUMA_free(isNode);
   
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
            
            /* 
            fprintf (SUMA_STDERR,"%s: Edge %d, IntersNodes[%d]= [%f, %f, %f]\n", 
               FuncName, k, 3*k, SPI->IntersNodes[3*k], SPI->IntersNodes[3*k+1], SPI->IntersNodes[3*k+2]);
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
   
   if (DrawIntersEdges) {
      /* Show all intersected edges */
      ROIe =  SUMA_AllocateROI (SO->idcode_str, SUMA_ROI_EdgeGroup, "SurfPlane Intersection - Edges", SPI->N_IntersEdges, SPI->IntersEdges);
      if (!ROIe) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateROI.\n", FuncName);
      } else {
         if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIe, ROIO_type, SUMA_LOCAL)) {
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
         if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIt, ROIO_type, SUMA_LOCAL)) {
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
            if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIts, ROIO_type, SUMA_LOCAL)) {
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
            if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIn, ROIO_type, SUMA_LOCAL)) {
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
SUMA_Boolean SUMA_Show_SPI (SUMA_SURF_PLANE_INTERSECT *SPI, FILE * Out, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_Show_SPI"};
   int i;
   
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
   
   fprintf (Out,"Intersection Nodes: %d\n[", SPI->N_IntersEdges);
   for (i=0; i < SO->EL->N_EL; ++i) {
      if (SPI->isEdgeInters[i]) fprintf (Out, "%f, %f, %f, ", SPI->IntersNodes[3*i], SPI->IntersNodes[3*i+1], SPI->IntersNodes[3*i+2]);
   }
   fprintf (Out," ]\n");
   
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
   /* SUMA_ENTRY;    */
   
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
      
      
 Path = SUMA_Dijkstra (SO, Nx,  Ny, isNodeInMesh, N_isNodeInMesh, Method_Number, Path_length, N_Path)
\param SO (SUMA_SurfaceObject *) The surface Object structure. NodeList, EL and FN are needed. 
\param Nx (int) The node index (referring to SO's nodes) where the search begins.
\param Ny (int) The node index (referring to SO's nodes) where the search ends.
\param isNodeInMesh (SUMA_Boolean *) Pointer to SO->N_Node long vector such that 
                                       if (isNodeInMesh[i]) then node i is part of the 
                                       mesh that is used in the search path. This mesh is a subset 
                                       of SO->FaceSetList and is typically obtained when one 
                                       runs SUMA_Surf_Plane_Intersect. Running SUMA_Dijkstra on 
                                       a complete surface is only for very patient people.
                                NOTE:  This vector is modified as a node is visited. Make sure you 
                                       do not use it after this function has been called.
\param N_isNodeInMesh (int *) Pointer to the total number of nodes that make up the mesh (subset of SO)
               This parameter is passed as a pointer because as nodes in the mesh are visited, that
               number is reduced and represents when the function returns, the number of nodes that were
               never visited in the search. 
\param Method_Number (int) selector for which algorithm to use. Choose from:
                     0 - Straight forward implementation, slow
                     1 - Variation to eliminate long searches for minimum of L, much much much faster than 0, 5 time more memory.
\param Path_length (float *) The distance between Nx and Ny. This value is negative if no path between Nx and Ny was found.
\param N_Path (int *) Number of nodes forming the Path vector

\return Path (float) A vector of N_Path node indices forming the shortest path, from Nx (Path[0]) to Ny (Path[*N_Path - 1]). 
                  NULL is returned in case of error.

\sa Graph Theory by Ronald Gould and labbook NIH-2 page 154 for path construction
*/
#define LARGE_NUM 9e300
/* #define LOCALDEBUG */ /* lots of debugging info. */
int * SUMA_Dijkstra (SUMA_SurfaceObject *SO, int Nx, int Ny, SUMA_Boolean *isNodeInMesh, int *N_isNodeInMesh, int Method_Number, float *Lfinal, int *N_Path)
{
   static char FuncName[] = {"SUMA_Dijkstra"};
   SUMA_Boolean LocalHead = NOPE;
   float *L = NULL, Lmin = -1.0, le = 0.0, DT_DIJKSTRA;
   int i, iw, iv, v, w, N_Neighb, *Path = NULL;
   struct  timeval  start_time;
   SUMA_DIJKSTRA_PATH_CHAIN *DC = NULL, *DCi, *DCp;
   SUMA_Boolean Found = NOPE;
   /* variables for method 2 */
   int N_Lmins, *vLmins, *vLocInLmins, iLmins, ReplacingNode, ReplacedNodeLocation;
   float *Lmins; 
   
   
   SUMA_ENTRY;
   
   *Lfinal = -1.0;
   *N_Path = 0;
   
   /* make sure Both Nx and Ny exist in isNodeInMesh */
   if (!isNodeInMesh[Nx]) {
      fprintf (SUMA_STDERR,"\aError %s: Node %d (Nx) is not in mesh.\n", FuncName, Nx);
      SUMA_RETURN (NULL);
   }  
   if (!isNodeInMesh[Ny]) {
      fprintf (SUMA_STDERR,"\aError %s: Node %d (Ny) is not in mesh.\n", FuncName, Ny);
      SUMA_RETURN (NULL);
   }

   if (!SO->FN) {
      fprintf (SUMA_STDERR, "Error %s: SO does not have FN structure.\n", FuncName);
      SUMA_RETURN (NULL);
   }

   if (LocalHead) {
      /* Start timer for next function */
      SUMA_etime(&start_time,0);      
   }
   
   /* allocate for chain */
   DC = (SUMA_DIJKSTRA_PATH_CHAIN *) SUMA_malloc (sizeof(SUMA_DIJKSTRA_PATH_CHAIN) * SO->N_Node);
   if (!DC) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate. \n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   switch (Method_Number) {
   
      case 0:  /* Method 0, Brute force */
         /* allocate for vertices labels */
         L = (float *) SUMA_calloc (SO->N_Node, sizeof (float));
         if (!L) {
            fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
            SUMA_free(DC);
            SUMA_RETURN (NULL);
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
               SUMA_free (L);
               SUMA_free(DC);
               SUMA_RETURN (NULL); 
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
            SUMA_free (L);
            SUMA_free(DC);
            SUMA_RETURN (NULL);
         }else {
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Path between Nodes %d and %d is %f.\n", FuncName, Nx, Ny, *Lfinal);
         }


         if (LocalHead) {
            /* stop timer */
            DT_DIJKSTRA = SUMA_etime(&start_time,1);
            fprintf (SUMA_STDERR, "%s: Method 1- Elapsed time in function %f seconds.\n", FuncName, DT_DIJKSTRA);
         }

         SUMA_free(L);
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
            SUMA_RETURN (NULL);
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
               fprintf (SUMA_STDERR, "\aERROR %s: Dijkstra derailed. v = %d, Lmin = %f\n. Try another point.", FuncName, v, Lmin);
               SUMA_free (L);
               SUMA_free (Lmins);
               SUMA_free(vLmins);
               SUMA_free(vLocInLmins);
               SUMA_free(DC);
               SUMA_RETURN (NULL);
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
            SUMA_free (L);
            SUMA_free (Lmins);
            SUMA_free(vLmins);
            SUMA_free(vLocInLmins);
            SUMA_free(DC);
            SUMA_RETURN (NULL);
         }else {
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Path between Nodes %d and %d is %f.\n", FuncName, Nx, Ny, *Lfinal);
         }


         if (LocalHead) {
            /* stop timer */
            DT_DIJKSTRA = SUMA_etime(&start_time,1);
            fprintf (SUMA_STDERR, "%s: Method 2- Elapsed time in function %f seconds.\n", FuncName, DT_DIJKSTRA);
         }

         SUMA_free(L);
         SUMA_free(Lmins);
         SUMA_free(vLmins);
         SUMA_free(vLocInLmins);
         break;   /********** Method 1- faster minimum searching **************/
      default: 
         fprintf (SUMA_STDERR, "Error %s: No such method (%d).\n", FuncName, Method_Number);
         if (DC) SUMA_free(DC);
         SUMA_RETURN (NULL);
         break;
   }
   
   /* now reconstruct the path */
   *N_Path = DC[Ny].order+1;
   Path = (int *) SUMA_calloc (*N_Path, sizeof(int));
   if (!Path) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      if (DC) SUMA_free(DC);
      SUMA_RETURN (NULL);
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
      fprintf (SUMA_STDERR, "Error %s: iv = %d. This should not be.\n", FuncName, iv);
   }  
   
   SUMA_free(DC);
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
   
   Found = SUMA_Get_Incident(nPath[0], nPath[1], SO->EL, Incident, &N_Incident, 1);
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
\param N_tPath (int *) number of elements in tPath
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
   if (!SUMA_Get_Incident(EL->EL[E2][0], EL->EL[E2][1], EL, Incident, &N_Incident, 1)) {
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



#ifdef SUMA_SurfQual_STANDALONE
#define SURFQUAL_MAX_SURF 10  /*!< Maximum number of input surfaces */

void usage_SUMA_SurfQual ()
   {
      static char FuncName[]={"usage_SUMA_SurfQual"};
      char * s = NULL;
      s = SUMA_help_basics();
      printf ( "\nUsage: A program to check the quality of surfaces.\n"
               "  SurfQual <-spec SpecFile> <-surf_A insurf> <-surf_B insurf> ...\n"
               "             <-sphere> [-self_intersect] [-prefix OUTPREF]  \n"
               "\n"
               "  Mandatory parameters:\n"
               "     -spec SpecFile: Spec file containing input surfaces.\n"
               "     -surf_X: Name of input surface X where X is a character\n"
               "              from A to Z. If surfaces are specified using two\n"
               "              files, use the name of the node coordinate file.\n"
               "  Mesh winding consistency and 2-manifold checks are performed\n"
               "  on all surfaces.\n"
               "  Optional parameters:\n"
               "     -self_intersect: Check if surface is self intersecting.\n"
               "                      This option is rather slow, so be patient.\n"
               "                      In the presence of intersections, the output file\n"
               "                      OUTPREF_IntersNodes.1D.dset will contain the indices\n"
               "                      of nodes forming segments that intersect the surface.\n"
               "  Most other checks are specific to spherical surfaces (see option below).\n"
               "     -sphere: Indicates that surfaces read are spherical.\n"
               "              With this option you get the following output.\n"
               "              - Absolute deviation between the distance (d) of each\n"
               "                node from the surface's center and the estimated\n"
               "                radius(r). The distances, abs (d - r), are \n"
               "                and written to the file OUTPREF_Dist.1D.dset .\n"
               "                The first column represents node index and the \n"
               "                second is the absolute distance. A colorized \n"
               "                version of the distances is written to the file \n"
               "                OUTPREF_Dist.1D.col (node index followed \n"
               "                by r g b values). A list of the 10 largest absolute\n"
               "                distances is also output to the screen.\n"
               "              - Also computed is the cosine of the angle between \n"
               "                the normal at a node and the direction vector formed\n"
               "                formed by the center and that node. Since both vectors\n"
               "                are normalized, the cosine of the angle is the dot product.\n"
               "                On a sphere, the abs(dot product) should be 1 or pretty \n"
               "                close. Nodes where abs(dot product) < 0.9 are flagged as\n"
               "                bad and written out to the file OUTPREF_BadNodes.1D.dset .\n"
               "                The file OUTPREF_dotprod.1D.dset contains the dot product \n"
               "                values for all the nodes. The files with colorized results\n"
               "                are OUTPREF_BadNodes.1D.col and OUTPREF_dotprod.1D.col .\n"
               "                A list of the bad nodes is also output to the screen for\n"
               "                convenience. You can use the 'j' option in SUMA to have\n"
               "                the cross-hair go to a particular node. Use 'Alt+l' to\n"
               "                have the surface rotate and place the cross-hair at the\n"
               "                center of your screen.\n"
               "              NOTE: For detecting topological problems with spherical\n"
               "                surfaces, I find the dot product method to work best.\n"              
               "  Optional parameters:\n"
               "     -prefix OUTPREF: Prefix of output files. If more than one surface\n"
               "                      are entered, then the prefix will have _X added\n"
               "                      to it, where X is a character from A to Z.\n"
               "                      THIS PROGRAM WILL OVERWRITE EXISTING FILES.\n"
               "                      Default prefix is the surface's label.\n"
               "\n"
               "  Comments:\n"
               "     - The colorized (.col) files can be loaded into SUMA (with the 'c' \n"
               "     option. By focusing on the bright spots, you can find trouble spots\n"
               "     which would otherwise be very difficult to locate.\n"
               "     - You should also pay attention to the messages output when the \n"
               "     surfaces are being loaded, particularly to edges (segments that \n"
               "     join 2 nodes) are shared by more than 2 triangles. For a proper\n"
               "     closed surface, every segment should be shared by 2 triangles. \n"
               "     For cut surfaces, segments belonging to 1 triangle only form\n"
               "     the edge of that surface.\n"
               "     - There are no utilities within SUMA to correct these defects.\n"
               "     It is best to fix these problems with the surface creation\n"
               "     software you are using.\n"
               "     - Some warnings may be redundant. That should not hurt you.\n"
               "%s"
               "\n", s);
       SUMA_free(s); s = NULL;        
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
       exit (0);
   }

typedef struct {
   SUMA_SO_File_Type iType;
   char *out_prefix;
   char *sv_name;
   char *surf_names[SURFQUAL_MAX_SURF];
   int N_surf;
   char *spec_file;
   char *surftype;
   int self_intersect;
} SUMA_SURFQUAL_OPTIONS;

/*!
   \brief parse the arguments for SurfQual program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_SURFQUAL_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_prefix); 
               SUMA_free(Opt);
*/
SUMA_SURFQUAL_OPTIONS *SUMA_SurfQual_ParseInput (char *argv[], int argc)
{
   static char FuncName[]={"SUMA_SurfQual_ParseInput"}; 
   SUMA_SURFQUAL_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outprefix;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_SURFQUAL_OPTIONS *)SUMA_malloc(sizeof(SUMA_SURFQUAL_OPTIONS));

   kar = 1;
   Opt->iType = SUMA_FT_NOT_SPECIFIED;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->spec_file = NULL;
   Opt->N_surf = -1;
   Opt->surftype = NULL;
   Opt->self_intersect = 0;
   for (i=0; i<SURFQUAL_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
	brk = NOPE;
   
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_SurfQual();
          exit (0);
		}
		
      /* skip the options parsed in SUMA_ParseInput_basics */
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-sphere") == 0)) {
			if (Opt->surftype) {
            SUMA_S_Err("Surface type already specified.\nOnly one type allowed.");
            exit(1);
         }
         Opt->surftype = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-self_intersect") == 0)) {
         Opt->self_intersect = 1;
			brk = YUP;
		}
      
      
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec \n");
				exit (1);
			}
			Opt->spec_file = argv[kar];
			brk = YUP;
		}
            
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix \n");
				exit (1);
			}
			Opt->out_prefix = SUMA_copy_string(argv[kar]);
			brk = YUP;
		}
            
      if (!brk && (strncmp(argv[kar], "-surf_", 6) == 0)) {
			if (kar + 1>= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -surf_X SURF_NAME \n");
				exit (1);
			}
			ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFQUAL_MAX_SURF) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option is out of range.\n");
				exit (1);
         }
         kar ++;
         Opt->surf_names[ind] = argv[kar];
         Opt->N_surf = ind+1;
         brk = YUP;
		}
      
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
      
   }
   
   if (Opt->N_surf < 1) {
      SUMA_SL_Err("No surface specified.");
      exit(1);
   }
      
   SUMA_RETURN (Opt);
     
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfQual"};
   char *OutName = NULL, ext[5], *prefix = NULL, *shist=NULL;
   SUMA_SURFQUAL_OPTIONS *Opt; 
   int SO_read = -1;
   int i, cnt, trouble;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfSpecFile Spec;
   void *SO_name = NULL;
   SUMA_Boolean DoConv = NOPE, DoSphQ = NOPE, DoSelfInt = NOPE;   
   SUMA_Boolean LocalHead = NOPE;
	
   SUMA_mainENTRY;
   
   SUMA_STANDALONE_INIT;
   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   if (argc < 4)
       {
          usage_SUMA_SurfQual();
          exit (1);
       }
   
   Opt = SUMA_SurfQual_ParseInput (argv, argc);
   
   /* read all surfaces */
   if (!SUMA_Read_SpecFile (Opt->spec_file, &Spec)) {
		fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
		exit(1);
	}
   SO_read = SUMA_spec_select_surfs(&Spec, Opt->surf_names, SURFQUAL_MAX_SURF, 0);
   if ( SO_read != Opt->N_surf )
   {
	   if (SO_read >=0 )
         fprintf(SUMA_STDERR,"Error %s:\nFound %d surfaces, expected %d.\n", FuncName,  SO_read, Opt->N_surf);
      exit(1);
   }
   /* now read into SUMAg_DOv */
   if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, Opt->sv_name, 0, SUMAg_CF->DsetList) ) {
	   fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
      exit(1);
   }
  
   if (Opt->self_intersect) DoSelfInt = YUP;
   
   DoConv = NOPE;
   DoSphQ = NOPE;   
   if (Opt->surftype) {
      if (!strcmp(Opt->surftype, "-sphere")) { 
         DoSphQ = YUP;
      }else {
         /* Don't complain anymore, maybe winding checking is all users need */
      }
   }
   
   for (i=0; i < Opt->N_surf; ++i) {/* loop to read in surfaces */
      /* now identify surface needed */
      SO = SUMA_find_named_SOp_inDOv(Opt->surf_names[i], SUMAg_DOv, SUMAg_N_DOv);
      if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface %s\n"
                              "in spec file. Use full name.\n",
                              FuncName, Opt->surf_names[i]);
         exit(1);
      }
      
      if (!SO->EL) SUMA_SurfaceMetrics(SO, "EdgeList", NULL);
      if (!SO->MF) SUMA_SurfaceMetrics(SO, "MemberFace", NULL);
      if (!SO->Label) SUMA_SurfaceFileName(SO, NOPE);
      
      
      /* do the quality thing based on the Opt->surftype */
      if (!Opt->out_prefix) prefix = SUMA_copy_string(SO->Label);
      else prefix = SUMA_copy_string (Opt->out_prefix);
      
      /* check the winding */
      if (!SUMA_MakeConsistent (SO->FaceSetList, SO->N_FaceSet, SO->EL, 0, &trouble)) {
         SUMA_S_Warn("Failed to make sure surface's mesh is consistently wound.\n"
                     "You should fix the mesh.\n");
      }
      if (DoConv) {
         float *Cx = NULL;
         if (Opt->N_surf > 1) {
            sprintf(ext,"_%c", 65+i);
            OutName = SUMA_append_replace_string (prefix, "_Conv_detail.1D.dset", ext, 0);
         } else { 
            OutName = SUMA_append_string (prefix, "_Conv_detail.1D.dset");
         }
         Cx = SUMA_Convexity_Engine ( SO->NodeList, SO->N_Node, 
                                      SO->NodeNormList, SO->FN, OutName);
         if (Cx) SUMA_free(Cx); Cx = NULL;
         if (OutName) SUMA_free(OutName); OutName = NULL;
      } 
      if (DoSphQ) {
         if (Opt->N_surf > 1) {
            sprintf(ext,"_%c", 65+i);
            OutName = SUMA_append_string (prefix, ext);
         } else { 
            OutName = SUMA_copy_string (prefix);
         }
         shist = SUMA_HistString (NULL, argc, argv, NULL);
         SUMA_SphereQuality (SO, OutName, shist);   
         if (shist) SUMA_free(shist); shist = NULL;
         if (OutName) SUMA_free(OutName); OutName = NULL;
      }
      
   }
   
  
   if (trouble) { /* put winding problem here to make it visible */
      fprintf (SUMA_STDERR,"\n");
      SUMA_S_Warn("Mesh is not consistent, use ConvertSurface's -make_consistent \n"
                  "option to fix the problem before proceeding further.\n"
                  "Other results reported by this and other programs\n"
                  "may be incorrect if mesh is not consistently wound.\n" ); 
   } else {
      fprintf (SUMA_STDERR,"\n");
      fprintf (SUMA_STDERR,"Surface is consistently wound\n");
   }
   { 
      int eu;
      SUMA_EULER_SO(SO, eu);
      fprintf (SUMA_STDERR,"\n");
      fprintf(SUMA_STDERR,"Surface Euler number is: %d\n", eu);
   }
   if ((SO->EL->min_N_Hosts == 1 || SO->EL->max_N_Hosts == 1)) {
         fprintf (SUMA_STDERR,"\n");
         fprintf(SUMA_STDERR,"Warning %s:\n Min/Max number of edge hosting triangles: [%d/%d] \n", FuncName, SO->EL->min_N_Hosts, SO->EL->max_N_Hosts);
         fprintf(SUMA_STDERR," You have edges that form a border in the surface.\n");
   }
   if (SO->EL->min_N_Hosts == 2 && SO->EL->max_N_Hosts == 2) {
      fprintf (SUMA_STDERR,"\n");
      fprintf(SUMA_STDERR,"Surface is closed and is a 2-manifold.");
   }
   if (SO->EL->min_N_Hosts > 2 || SO->EL->max_N_Hosts > 2) {
      fprintf (SUMA_STDERR,"\n");
      fprintf(SUMA_STDERR, "Warning %s:\n"
                           "Min/Max number of edge hosting triangles: [%d/%d] \n", FuncName, SO->EL->min_N_Hosts, SO->EL->max_N_Hosts);
      fprintf(SUMA_STDERR, "Warning %s:\n"
                           " You have edges that belong to more than two triangles.\n"
                           " Bad for analysis assuming surface is a 2-manifold.\n", FuncName);
      if (1) {
         int iii=0;
         fprintf(SUMA_STDERR, " These edges are formed by the following nodes:\n");
         for (iii = 0; iii < SO->EL->N_EL; ++iii) { 
            if (SO->EL->ELps[iii][2] > 2) fprintf (SUMA_STDERR," %d: Edge [%d %d] shared by %d triangles.\n", 
                                             iii+1, SO->EL->EL[iii][0], SO->EL->EL[iii][1] , SO->EL->ELps[iii][2] );
         }
      }
   }
   
   if (DoSelfInt) {
      int nsi, iii;
      FILE *fout=NULL;
      byte *report = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte));
      if (!report) {
         SUMA_SL_Crit("Failed to allocate for report");
         report = NULL;
      }  
      fprintf( SUMA_STDERR, "\n\nChecking for intersections...:\n");
      nsi = SUMA_isSelfIntersect(SO, 500, report);
      if (nsi) {
         fprintf(SUMA_STDERR, " Surface is self intersecting.\n%d segments were found to intersect the surface.\n", nsi);
         if (nsi >= 500) {
            fprintf(SUMA_STDERR, " It is possible that you have additional segments intersecting the surface.\n");
         }
         if (report) {
            if (Opt->N_surf > 1) {
               sprintf(ext,"_%c", 65+i);
               OutName = SUMA_append_replace_string (prefix, "_IntersNodes.1D.dset", ext, 0);
            } else { 
               OutName = SUMA_append_string (prefix, "_IntersNodes.1D.dset");
            }
            fout = fopen(OutName, "w");
            if (fout) {
               fprintf(fout,  "#List of nodes that are part of segments which intersect the surface\n"
                              "#%s\n"
                              "#A total of %d segments (search limit is 500) were found to intersect the surface.\n"
                              "#Col.1 : Node index\n"
                              "#Col.2 : Dummny flag, always 1\n", SUMA_CHECK_NULL_STR(SO->Label), nsi );
               for (iii=0; iii<SO->N_Node; ++iii) if (report[iii]) fprintf(fout, "%d\t1\n", iii);
               fclose(fout); fout = NULL;
            } else {
               SUMA_SL_Err("Failed to open file for output.");
            }
            if (OutName) SUMA_free(OutName);
         }         
      }else {
         fprintf(SUMA_STDERR, " Surface is not self intersecting.\n");
      }   
      if (report) SUMA_free(report); report = NULL;
   }
   
   fprintf (SUMA_STDERR,"\n");
   
   SUMA_LH("clean up");
   if (prefix) SUMA_free(prefix); prefix = NULL;
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt) SUMA_free(Opt);   
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   SUMA_RETURN(0);
} 
#endif



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
