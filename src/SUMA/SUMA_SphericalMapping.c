#include "SUMA_suma.h"
#include "SUMA_Macros.h"

#undef STAND_ALONE

#if defined SUMA_CreateIcosahedron_STAND_ALONE 
#define STAND_ALONE 
#elif defined SUMA_MapIcosahedron_STAND_ALONE
#define STAND_ALONE 
#elif defined SUMA_Map_SurfacetoSurface_STAND_ALONE
#define STAND_ALONE 
#elif defined SUMA_AverageMaps_STAND_ALONE
#define STAND_ALONE 
#endif


#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs stored in SVv */
SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */
#else
extern SUMA_CommonFields *SUMAg_CF;
#endif

float ep = 1e-4; /* this represents the smallest coordinate difference to be expected between neighboring nodes. Do not make it too small or else you will get round off errors. It is reassigned in SUMA_MakeIcosahedron, becoming dependent upon the recursion depth.  (Assigned here in case SUMA_binTesselate used without SUMA_CreateIcosahedron) Talk to Brenna Argall for details. */

/*!
   \brief A function to test if a spherical surface is indeed spherical
   
   SUMA_SphereQuality (SUMA_SurfaceObject *SO, char *Froot)
   
   This function function reports on a few parameters indicative of
   the quality of a spherical surface:
   it calculates the absolute deviation between
   the distance of each node from SO->Center (d) and the estimated radius(r)
      abs (d - r) 
   The distances are sorted and written to the file: <Froot>_SortedDdist.1D . The
   first column represents node index. A colorized version is written to 
   <Froot>_SortedDdist.1D.col (node index followed by r g b values)
   
   The function also computes the cosine of the angle between the normal at
   a node and the direction vector formed by the center and that node. 
   Since both vectors are normalized, the cosine of the angle is the dot product. 
   On a sphere, the abs(dot product) should be 1 or pretty close. abs(dot product) < 0.9 are 
   flagged as bad and written out to the file <Froot>_BadNodes.1D . 
   The file <Froot>_dotprod.1D contains the dot product values for all the 
   nodes. The file with colorized results are <Froot>_BadNodes.1D.col and 
   <Froot>_dotprod.1D.col
   
      
*/
SUMA_Boolean SUMA_SphereQuality(SUMA_SurfaceObject *SO, char *Froot)
{
   static char FuncName[]={"SUMA_SphereQuality"};
   float *dist = NULL, mdist, *dot=NULL, nr, r[3], *bad_dot = NULL;
   int i, i3, *isortdist = NULL;
   int *bad_ind = NULL, ibad =-1;
   FILE *fid;
   char *fname;
   SUMA_COLOR_MAP *CM;
   SUMA_SCALE_TO_MAP_OPT * OptScl;
   SUMA_COLOR_SCALED_VECT * SV;
   SUMA_Boolean LocalHead = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SO) {
      SUMA_SL_Err("NULL SO");
      SUMA_RETURN(NOPE);
   }
   
   /* get the options for creating the scaled color mapping */
   OptScl = SUMA_ScaleToMapOptInit();
   if (!OptScl) {
      fprintf (SUMA_STDERR,"Error %s: Could not get scaling option structure.\n", FuncName);
      exit (1); 
   }
   
   /* get the color map */
   CM = SUMA_GetStandardMap (SUMA_CMAP_MATLAB_DEF_BYR64);
   if (CM == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Could not get standard colormap.\n", FuncName);
      exit (1); 
   }
   
   /* compare the distance of each node to the distance to estimated radius */
   dist = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   mdist = 0.0;
   for (i=0; i<SO->N_Node; ++i) {
      i3 = 3*i;
      dist[i] =   sqrt ( pow((double)(SO->NodeList[i3]   - SO->Center[0]), 2.0) +
                         pow((double)(SO->NodeList[i3+1] - SO->Center[1]), 2.0) +
                         pow((double)(SO->NodeList[i3+2] - SO->Center[2]), 2.0) );
      mdist += dist[i];
   }
   mdist /= (float)SO->N_Node;
   
   /* calculate the difference from mdist */
   for (i=0; i<SO->N_Node; ++i) dist[i] = fabs(dist[i] - mdist);
   
   /* Now sort that */ 
   isortdist = SUMA_z_qsort ( dist , SO->N_Node  );
   
   /* report */
   fprintf (SUMA_STDERR,"%s: Reporting on Spheriosity of %s\n", FuncName, SO->Label);
   fprintf (SUMA_STDERR,"Mean distance from center (estimated radius): %f\n", mdist);
   fprintf (SUMA_STDERR,"Largest 10 absolute departures from estimated radius:\n");
   for (i=SO->N_Node-1; i > SO->N_Node - 10; --i) {
      fprintf (SUMA_STDERR,"dist @ %d: %f\n", isortdist[i], dist[i]);
   }
   
   
   if (LocalHead) {
      /* Colorize results */
      SV = SUMA_Create_ColorScaledVect(SO->N_Node);
      if (!SV) {
         fprintf (SUMA_STDERR,"Error %s: Could not allocate for SV.\n", FuncName);
         exit(1);
      }

      if (!SUMA_ScaleToMap (dist, SO->N_Node, dist[0], dist[SO->N_Node-1], CM, OptScl, SV)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
         exit(1);
      }
   
      /* write the data */
      fname = SUMA_append_string(Froot, "_SortedDdist.1D");
      fprintf (SUMA_STDERR,"%s:\nWriting %s...\n", FuncName, fname);
      fid = fopen(fname, "w");
      for (i=0; i<SO->N_Node; ++i) fprintf(fid,"%d\t%f\n", isortdist[i], dist[i]);
      fclose(fid);
      free(fname);
      
      /* write the colorized data */
      fname = SUMA_append_string(Froot, "_SortedDdist.1D.col");
      fprintf (SUMA_STDERR,"%s:\nWriting %s...\n", FuncName, fname);
      fid = fopen(fname, "w");
      for (i=0; i<SO->N_Node; ++i) fprintf(fid,"%d\t%f\t%f\t%f\n", isortdist[i], SV->cM[i][0], SV->cM[i][1], SV->cM[i][2]);
      fclose(fid);
      free(fname);
      if (SV) SUMA_Free_ColorScaledVect (SV);
   }
      
   /* New idea:
   If we had a perfect sphere then the normal of each node
   will be colinear with the direction of the vector between the
   sphere's center and the node.
   The mode the deviation, the worse the sphere */
   dot     = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   bad_ind = (int *)  SUMA_calloc(SO->N_Node, sizeof(int)  );
   bad_dot = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
   ibad = 0;
   for (i=0; i<SO->N_Node; ++i) {
      i3 = 3*i;
      r[0] = SO->NodeList[i3]   - SO->Center[0];
      r[1] = SO->NodeList[i3+1] - SO->Center[1];
      r[2] = SO->NodeList[i3+2] - SO->Center[2];
      nr = sqrt ( r[0] * r[0] + r[1] * r[1] + r[2] * r[2] );
      r[0] /= nr; r[1] /= nr; r[2] /= nr; 
      
      dot[i] = r[0]*SO->NodeNormList[i3]   + 
               r[1]*SO->NodeNormList[i3+1] +
               r[2]*SO->NodeNormList[i3+2] ;
      
      if (fabs(dot[i]) < 0.9) {
         bad_ind[ibad] = i;
         bad_dot[ibad] = dot[i];
         ++ibad;
      }
   }
   
   bad_ind = (int *)  SUMA_realloc(bad_ind, ibad * sizeof(int));
   bad_dot = (float *)SUMA_realloc(bad_dot, ibad * sizeof(float));
   
   if (LocalHead) {
      fname = SUMA_append_string(Froot, "_dotprod.1D");
      fprintf (SUMA_STDERR,"%s:\nWriting %s...\n", FuncName, fname);
      fid = fopen(fname, "w");
      for (i=0; i<SO->N_Node; ++i) fprintf(fid,"%d\t%f\n", i, dot[i]);
      fclose(fid);
      free(fname);
      
      /* write the colorized data */
      SV = SUMA_Create_ColorScaledVect(SO->N_Node);
      if (!SV) {
         fprintf (SUMA_STDERR,"Error %s: Could not allocate for SV.\n", FuncName);
         exit(1);
      }

      if (!SUMA_ScaleToMap (dot, SO->N_Node, -1.0, 1.0, CM, OptScl, SV)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
         exit(1);
      }
      fname = SUMA_append_string(Froot, "_dotprod.1D.col");
      fprintf (SUMA_STDERR,"%s:\nWriting %s...\n", FuncName, fname);
      fid = fopen(fname, "w");
      for (i=0; i<SO->N_Node; ++i) fprintf(fid,"%d\t%f\t%f\t%f\n", i, SV->cM[i][0], SV->cM[i][1], SV->cM[i][2]);
      fclose(fid);
      free(fname);
      if (SV) SUMA_Free_ColorScaledVect (SV);
      
      fname = SUMA_append_string(Froot, "_BadNodes.1D");
      fprintf (SUMA_STDERR,"%s:\nWriting %s...\n", FuncName, fname);
      fid = fopen(fname, "w");
      for (i=0; i<ibad; ++i) fprintf(fid,"%d\t%f\n", bad_ind[i], bad_dot[i]);
      fclose(fid);
      free(fname);
      
      /* write the colorized data */
      SV = SUMA_Create_ColorScaledVect(ibad);
      if (!SV) {
         fprintf (SUMA_STDERR,"Error %s: Could not allocate for SV.\n", FuncName);
         exit(1);
      }

      if (!SUMA_ScaleToMap (bad_dot, ibad, -1.0, 1.0, CM, OptScl, SV)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
         exit(1);
      }
      fname = SUMA_append_string(Froot, "_BadNodes.1D.col");
      fprintf (SUMA_STDERR,"%s:\nWriting %s...\n", FuncName, fname);
      fid = fopen(fname, "w");
      for (i=0; i<ibad; ++i) fprintf(fid,"%d\t%f\t%f\t%f\n", bad_ind[i], SV->cM[i][0], SV->cM[i][1], SV->cM[i][2]);
      fclose(fid);
      free(fname);
      if (SV) SUMA_Free_ColorScaledVect (SV);
      
   }
   
   /* report */
   fprintf (SUMA_STDERR,"Nodes with normals at angle with radial direction: abs(dot product < 0.9)\n");
   for (i=0; i < ibad; ++i) {
      fprintf (SUMA_STDERR,"cos(ang) @ %d: %f\n", bad_ind[i], bad_dot[i]);
   }   
   
   if (dot) SUMA_free(dot);
   if (bad_dot) SUMA_free(bad_dot);
   if (bad_ind) SUMA_free(bad_ind);
   if (isortdist) SUMA_free(isortdist);
   if (dist) SUMA_free(dist);
   if (CM) SUMA_Free_ColorMap (CM);
   if (OptScl) SUMA_free(OptScl);
   
   
   SUMA_RETURN(YUP);
}

/*!
  SUMA_binTesselate(nodeList, triList, nCtr, tCtr, recDepth, depth, n1, n2, n3);

  This function divides 1 triangle into 4 recursively to depth recDepth.
  \param nodeList (float *) 3 x N_Node list of nodes (updated as new nodes created during tesselation)
  \param triList (int *) 3 x N_Triangle list of nodes assoicated with each triangle (updated as new triangles created during tesselation)
  \param nCtr (int *) index of most recently added node to nodeList
  \param tCtr (int *) index of most recently added triangle to triList
  \param recDepth (int) recursion depth
  \param depth (int) current depth
  \param n1, n2, n3 (int) indices in nodeList corresponding to three nodes of triangle being tesselated
  \return void (but nodeList and triList updated)

  Written by Brenna Argall
 
*/

void SUMA_binTesselate(float *nodeList, int *triList, int *nCtr, int *tCtr, int recDepth, int depth, int n1, int n2, int n3)
{
   double x1=0,y1=0,z1=0, x2=0,y2=0,z2=0, x3=0,y3=0,z3=0;
   double x12=0, y12=0, z12=0;
   double x23=0, y23=0, z23=0;
   double x31=0, y31=0, z31=0;
   int currIndex, index1, index2, index3;
   int i=0, j=0, m=0, k=0;
   static char FuncName[]={"SUMA_binTesselate"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   currIndex = (nCtr[0]-2)/3;

   x1=(double)nodeList[3*n1]; y1=(double)nodeList[3*n1+1]; z1=(double)nodeList[3*n1+2];
   x2=(double)nodeList[3*n2]; y2=(double)nodeList[3*n2+1]; z2=(double)nodeList[3*n2+2];
   x3=(double)nodeList[3*n3]; y3=(double)nodeList[3*n3+1]; z3=(double)nodeList[3*n3+2];
  
   x12=(x1+x2)/2.0; y12=(y1+y2)/2.0; z12=(z1+z2)/2.0;
   x23=(x2+x3)/2.0; y23=(y2+y3)/2.0; z23=(z2+z3)/2.0;
   x31=(x3+x1)/2.0; y31=(y3+y1)/2.0; z31=(z3+z1)/2.0;

   /**prevents creation of duplicate nodes*/
   index1 = -1; index2 = -1; index3 = -1;
   i=0; j=0;
   for (i=0; i<=currIndex; ++i) {
      j = 3*i;
      if ( fabs(nodeList[j]-x12)<ep && fabs(nodeList[j+1]-y12)<ep && fabs(nodeList[j+2]-z12)<ep ) {
         index1 = i;
      }
      if ( fabs(nodeList[j]-x23)<ep && fabs(nodeList[j+1]-y23)<ep && fabs(nodeList[j+2]-z23)<ep ) {
         index2 = i;
      }
      if ( fabs(nodeList[j]-x31)<ep && fabs(nodeList[j+1]-y31)<ep && fabs(nodeList[j+2]-z31)<ep ) {
         index3 = i;
      }
   }
  
   if (index1==-1) {
      ++currIndex;
      index1 = currIndex;
      SUMA_addNode( nodeList, nCtr, (float)x12, (float)y12, (float)z12);
   }
   if (index2==-1) {
      ++currIndex;
      index2 = currIndex;
      SUMA_addNode( nodeList, nCtr, (float)x23, (float)y23, (float)z23);
   }
   if (index3==-1) {
      ++currIndex;
      index3 = currIndex;
      SUMA_addNode( nodeList, nCtr, (float)x31, (float)y31, (float)z31);
   }
  
   /**if recursion depth met, add 4 triangles to list referenced by tPtr*/
   if (depth>=recDepth) {
      SUMA_addTri( triList, tCtr, n1, index1, index3);
      SUMA_addTri( triList, tCtr, index1, n2, index2);
      SUMA_addTri( triList, tCtr, index3, index2, n3);
      SUMA_addTri( triList, tCtr, index3, index2, index1);
   }

   /**recursion depth not met: call tesselate on each of 4 new triangles*/
   else {
      ++depth;
      SUMA_binTesselate( nodeList, triList, nCtr, tCtr, recDepth, depth, n1, index1, index3 );
      SUMA_binTesselate( nodeList, triList, nCtr, tCtr, recDepth, depth, index1, n2, index2 );
      SUMA_binTesselate( nodeList, triList, nCtr, tCtr, recDepth, depth, index3, index2, n3 );
      SUMA_binTesselate( nodeList, triList, nCtr, tCtr, recDepth, depth, index3, index2, index1 );
   }

   SUMA_RETURNe;
}

/*!
  SUMA_tesselate(nodeList, triList, nCtr, tCtr, N_Div, n0, n1, n2);

  This function tesselates triangle by dividing edges into N_Div segments.
  \param nodeList (float *) 3 x N_Node list of nodes (updated as new nodes created during tesselation)
  \param triList (int *) 3 x N_Triangle list of nodes assoicated with each triangle (updated as new triangles created during tesselation)
  \param nCtr (int *) index of most recently added node to nodeList
  \param tCtr (int *) index of most recently added triangle to triList
  \param N_Div (int) number of edge divides
  \param n1,n2,n3 (int) indices in nodeList corresponding to three nodes of triangle being tesselated
  \return void (but nodeList and triList updated)

  Written by Brenna Argall
 
*/
void SUMA_tesselate( float *nodeList, int *triList, int *nCtr, int *tCtr, int N_Div, int n0, int n1, int n2) {

   int i=0, j=0;
   int *edge01=NULL, *edge12=NULL, *edge20=NULL, *currFloor=NULL;
   static char FuncName[]={"SUMA_tesselate"};
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   edge01 = SUMA_divEdge( nodeList, nCtr, n0, n1, N_Div);
   edge12 = SUMA_divEdge( nodeList, nCtr, n2, n1, N_Div);
   edge20 = SUMA_divEdge( nodeList, nCtr, n0, n2, N_Div);
   if (!edge01 || !edge12 || !edge20) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_divEdge.\n", FuncName);
      SUMA_RETURNe;
   }
  
   currFloor = edge20;

   for (i=1; i<N_Div; ++i) {
      SUMA_triangulateRow( nodeList, triList, nCtr, tCtr, N_Div-i, currFloor, edge01[i], edge12[i]);
   }
  
   SUMA_addTri( triList, tCtr, currFloor[1], n1, currFloor[0]);

   if (edge01) SUMA_free(edge01);
   if (edge12) SUMA_free(edge12);
   if (edge20) SUMA_free(edge20);

   SUMA_RETURNe;
}

/*!
  edge = SUMA_divEdge( nodeList, nCtr, node1, node2, N_Div);
  
  Divides an edge defined by node1-node2 into N_Div segments.
  \param nodeList (float *) 3 x N_Node list of nodes
  \param nCtr (int *) current number of elements in nodeList
  \param node1, node2 (int) nodes defining edge being divided
  \param N_Div (int) number of segments edge divided into
  \return edge (int *) N_Div+1 list of nodes on edge (after segmentation)

  Written by Brenna Argall
*/
int * SUMA_divEdge( float *nodeList, int *nCtr, int node1, int node2, int N_Div) {

   float *newNodes = NULL;
   float n1[3], n2[3];
   int *edge = NULL;
   int i=0, j=0, k=0, m=0;
   int currIndex = (nCtr[0]-2)/3;
   static char FuncName[]={"SUMA_divEdge"};
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
 
  
   edge = (int *) SUMA_calloc(N_Div+1, sizeof(int));
   newNodes = (float *)SUMA_calloc (3*(N_Div-1), sizeof(float));
  
   if (!edge || !newNodes) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      SUMA_RETURN (edge);
   }
  
   for(i=0; i<N_Div+1; ++i) {
      edge[i] = -1;
   }
  
   edge[0] = node1;  edge[N_Div] = node2;

   n1[0] = nodeList[3*node1];  n1[1] = nodeList[3*node1+1];  n1[2] = nodeList[3*node1+2];
   n2[0] = nodeList[3*node2];  n2[1] = nodeList[3*node2+1];  n2[2] = nodeList[3*node2+2];

   /*create new nodes*/
   for(i=0; i<N_Div-1; ++i) {
      j = 3*i;
      newNodes[j] =   ((i+1.0)/(float)N_Div)*(n2[0]-n1[0]) + n1[0];
      newNodes[j+1] = ((i+1.0)/(float)N_Div)*(n2[1]-n1[1]) + n1[1];
      newNodes[j+2] = ((i+1.0)/(float)N_Div)*(n2[2]-n1[2]) + n1[2];
   }

   /*check for existing nodes*/
   for (i=0; i<=currIndex; ++i) {
      j = 3*i;
      for (m=0; m<N_Div-1; ++m) {
         k = 3*m;
         if ( fabs(nodeList[j]-newNodes[k])<ep && fabs(nodeList[j+1]-newNodes[k+1])<ep && 
              fabs(nodeList[j+2]-newNodes[k+2])<ep ) {
            edge[m+1] = i;
         }
      }
   }

   for (i=1; i<N_Div; ++i) {
      if (edge[i]==-1) {
         SUMA_addNode( nodeList, nCtr, newNodes[3*(i-1)], newNodes[3*(i-1)+1], newNodes[3*(i-1)+2]);
         edge[i] = (nCtr[0]-2)/3;
      }
   }

   if (newNodes) SUMA_free(newNodes);
   
   SUMA_RETURN  (edge);
}

/*!
  SUMA_triangulateRow (nodeList, triList, nCtr, tCtr, N_Div, currFloor, node1, node2);

  Creates triangulation between line segments currFloor and node1-node2.  It is expected that node1-node2 has one fewer node than currFloor.
  \param nodeList (float *) 3 x N_Node list of nodes
  \param triList (int *) 3 x N_Tri list of node indicies corresponding to triangles
  \param nCtr (int *) current number of elements in nodeList
  \param tCtr (int *) current number of elements in triList
  \param N_Div (int) number of divisions to be created from line segment node1-node2
  \param currFloor (int *) vector containing nodes of line segment "below" segment node1-node2 (length N_Div+1)
  \param node1, node2 (int) nodeList indices of nodes defining segment "above" currFloor
  \return void (but triList and nodeList updated)

  Written by Brenna Argall
*/
/*see LNB p28 for diagram*/
void SUMA_triangulateRow( float *nodeList, int *triList, int *nCtr, int *tCtr, int N_Div, int *currFloor, int node1, int node2) {
  
   int i=0, j=0;
   float n1[3], n2[3], newNode[3];
   int  *newArray = NULL;
   static char FuncName[]={"SUMA_triangulateRow"};
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   newArray = (int *)SUMA_calloc(N_Div+1, sizeof(int));
   if (!newArray) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      SUMA_RETURNe;
   }
   
   n1[0] = nodeList[3*node1];  n1[1] = nodeList[3*node1+1];  n1[2] = nodeList[3*node1+2];
   n2[0] = nodeList[3*node2];  n2[1] = nodeList[3*node2+1];  n2[2] = nodeList[3*node2+2];
   newArray[0] = node1;  newArray[N_Div] = node2;

   SUMA_addTri( triList, tCtr, currFloor[1], currFloor[0], newArray[0]);

   for (i=1; i<N_Div; ++i) {
      newNode[0] = ((float)i/(float)N_Div)*(n2[0]-n1[0]) + n1[0];
      newNode[1] = ((float)i/(float)N_Div)*(n2[1]-n1[1]) + n1[1];
      newNode[2] = ((float)i/(float)N_Div)*(n2[2]-n1[2]) + n1[2];
  
      SUMA_addNode( nodeList, nCtr, newNode[0], newNode[1], newNode[2]);
      newArray[i] = (nCtr[0]-2)/3;
      SUMA_addTri( triList, tCtr, newArray[i-1], currFloor[i], newArray[i]);
      SUMA_addTri( triList, tCtr, currFloor[i+1], newArray[i], currFloor[i]);
   }
   SUMA_addTri( triList, tCtr, newArray[N_Div-1], currFloor[N_Div], newArray[N_Div]);
   SUMA_addTri( triList, tCtr, newArray[N_Div], currFloor[N_Div+1], currFloor[N_Div]);

   for (i=0; i<N_Div+1; ++i) {
      currFloor[i] = newArray[i];
   }

   if (newArray) SUMA_free(newArray);

   SUMA_RETURNe;
}


/*!
  SUMA_addNode(nodeList, ctr, x, y, z);

  Function to add the x, y, z corrdinates of a node to nodeList.
  \param nodeList (float *) 3 x N_node array of x,y,z coordinates of nodes
  \param ctr (int *) current position in nodeList
  \param x, y, z (float) x, y, z values of added node

*/
void SUMA_addNode(float *nodeList, int *ctr, float x, float y, float z) {
  
   static char FuncName[]={"SUMA_addNode"};
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
  
   ++*ctr;
   nodeList[*ctr] = x;  
   ++*ctr;
   nodeList[*ctr] = y;  
   ++*ctr;
   nodeList[*ctr] = z;

   SUMA_RETURNe;
}

/*!
  SUMA_addTri(triList, ctr, n1, n2, n3);

  Function to add the three nodes of a triangle to triList.
  \param triList (int *) 3 x N_tri array of node indices creating triangles
  \param ctr (int *) current position in triList
  \param n1, n2, n3 (int *) nodeList indices of nodes creating added triangle
*/
void SUMA_addTri(int *triList, int *ctr, int n1, int n2, int n3) {

   static char FuncName[]={"SUMA_addTri"};
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ++*ctr;
   triList[*ctr] = n1;
   ++*ctr;
   triList[*ctr] = n2;
   ++*ctr;
   triList[*ctr] = n3;

   SUMA_RETURNe;
}

/*!
  SO = SUMA_CreateIcosahedron (r, depth, ctr, bin);
 
  This function creates an icosahedron of size r and to tesselation extent depth.
  \param r (float) size of icosahedron (distance from center to node).
  \param depth (int) number of edge subdivisions (bin='n') or depth of recursive tesselation (bin='y')
  \param ctr (float[]) coordinates of center of icosahedron
  \param bin (char[]) indicates whether tesselation binary/recursive ('y') or brute ('n')
  \ret SO (SUMA_SurfaceObject *) icosahedron is a surface object structure.
  returns NULL if function fails.
  SO returned with NodeList, N_Node, FaceSetList, N_FaceSet, and NodeNormList
     
  Written by Brenna Argall  
*/
SUMA_SurfaceObject * SUMA_CreateIcosahedron (float r, int depth, float ctr[3], char bin[]) 
{
   static char FuncName[]={"SUMA_CreateIcosahedron"};
   SUMA_SurfaceObject *SO = NULL;
   int i, numNodes=0, numTri=0;
   float a,b, lgth;
   int nodePtCt, triPtCt, *icosaTri=NULL;
   float *icosaNode=NULL;
   SUMA_SURF_NORM SN;
   SUMA_NODE_FIRST_NEIGHB *firstNeighb=NULL;
   SUMA_Boolean DoWind = YUP;
   int n=0, m=0, in=0;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SO = SUMA_Alloc_SurfObject_Struct(1);
   if (SO == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Surface Object.", FuncName);
      SUMA_RETURN (NULL);
   }  

   
   if (strcmp(bin, "y") == 0) { numTri = 20*pow(2,2*depth); }  //exact
   else {
      if (depth !=0) {  numTri = 20*pow(depth, 2); }
      else numTri = 20;
   }
   if (depth != 0) {  numNodes = 3*numTri; }  //conservative
   else numNodes = 12;
     

   if (LocalHead) fprintf(SUMA_STDERR,"%s: Allocated for %d Nodes, %d numTri\n", FuncName, numNodes, numTri);
   
   /**icosahedron creation and tesselation*/
   
   a = r*(1+sqrt(5)) / (sqrt(10+2*sqrt(5)));
   b = 2*r / (sqrt(10+2*sqrt(5)));
   lgth = sqrt( pow(0-b,2) + pow(b-a,2) + pow(-a-0,2) );  //determine length of edge by dist node0->node1
 
   /*assign ep to be 1/2 the lenth of the maximum final distance between two nodes
     (see LNB p3 / p29)*/
   if (strcmp(bin, "y") == 0) {
      ep = lgth / pow(2, depth+1);
   }
   else ep = lgth / (2*depth);

   /**create icosahedron node list*/
   nodePtCt = -1;
   icosaNode = (float *) SUMA_calloc(3*numNodes, sizeof(float));
   icosaTri = (int *) SUMA_calloc(3*numTri, sizeof(int));

   if (!icosaNode || !icosaTri) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for icosaNode and/or icosaTri.\n",FuncName);
      SUMA_Free_Surface_Object (SO);
      SUMA_RETURN (NULL); 
   }

   SUMA_addNode( icosaNode, &nodePtCt, 0+ctr[0], b+ctr[1], -a+ctr[2] );  
   SUMA_addNode( icosaNode, &nodePtCt, 0+ctr[0], b+ctr[1], a+ctr[2] );
   SUMA_addNode( icosaNode, &nodePtCt, 0+ctr[0], -b+ctr[1], a+ctr[2] );  
   SUMA_addNode( icosaNode, &nodePtCt, 0+ctr[0], -b+ctr[1], -a+ctr[2] );
   SUMA_addNode( icosaNode, &nodePtCt, -b+ctr[0], a+ctr[1], 0+ctr[2] );  
   SUMA_addNode( icosaNode, &nodePtCt, -b+ctr[0], -a+ctr[1], 0+ctr[2] );
   SUMA_addNode( icosaNode, &nodePtCt, b+ctr[0], a+ctr[1], 0+ctr[2] );   
   SUMA_addNode( icosaNode, &nodePtCt, b+ctr[0], -a+ctr[1], 0+ctr[2] );
   SUMA_addNode( icosaNode, &nodePtCt, a+ctr[0], 0+ctr[1], b+ctr[2] );   
   SUMA_addNode( icosaNode, &nodePtCt, -a+ctr[0], 0+ctr[1], -b+ctr[2] );
   SUMA_addNode( icosaNode, &nodePtCt, -a+ctr[0], 0+ctr[1], b+ctr[2] );  
   SUMA_addNode( icosaNode, &nodePtCt, a+ctr[0], 0+ctr[1], -b+ctr[2] );

   /**tesselate icosahedron*/

   triPtCt = -1;

   /**if recursion depth is 0, just make icosahedron (no tesselation)*/
   if (depth==0) {

      SUMA_addTri( icosaTri, &triPtCt, 0, 4, 6 );   SUMA_addTri( icosaTri, &triPtCt, 1, 6, 4 );
      SUMA_addTri( icosaTri, &triPtCt, 0, 9, 4 );   SUMA_addTri( icosaTri, &triPtCt, 1, 8, 6 );
      SUMA_addTri( icosaTri, &triPtCt, 0, 3, 9 );   SUMA_addTri( icosaTri, &triPtCt, 1, 2, 8 );
      SUMA_addTri( icosaTri, &triPtCt, 0, 11, 3 );  SUMA_addTri( icosaTri, &triPtCt, 1, 10, 2 );
      SUMA_addTri( icosaTri, &triPtCt, 0, 6, 11 );  SUMA_addTri( icosaTri, &triPtCt, 1, 4, 10 );
      SUMA_addTri( icosaTri, &triPtCt, 2, 7, 8 );   SUMA_addTri( icosaTri, &triPtCt, 3, 11, 7 );
      SUMA_addTri( icosaTri, &triPtCt, 2, 5, 7 );   SUMA_addTri( icosaTri, &triPtCt, 3, 7, 5 );
      SUMA_addTri( icosaTri, &triPtCt, 2, 10, 5 );  SUMA_addTri( icosaTri, &triPtCt, 3, 5, 9 );
      SUMA_addTri( icosaTri, &triPtCt, 4, 9, 10 );  SUMA_addTri( icosaTri, &triPtCt, 6, 8, 11 );
      SUMA_addTri( icosaTri, &triPtCt, 5, 10, 9 );  SUMA_addTri( icosaTri, &triPtCt, 7, 11, 8 );
   }

   else {
      if (strcmp(bin, "y") == 0) {
         /*binary tesselation*/
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 0, 4, 6);
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 0, 9, 4 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 0, 3, 9 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 0, 11, 3 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 0, 6, 11 );
       
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 1, 6, 4 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 1, 8, 6 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 1, 2, 8 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 1, 10, 2 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 1, 4, 10 );
       
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 2, 7, 8 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 2, 5, 7 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 2, 10, 5 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 4, 9, 10 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 5, 10, 9 );
       
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 3, 11, 7 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 3, 7, 5 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 3, 5, 9 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 6, 8, 11 );
         SUMA_binTesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 7, 11, 8 );
      }

      else {
         /*brute tesselation*/
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 0, 4, 6);
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 0, 9, 4 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 0, 3, 9 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 0, 11, 3 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 0, 6, 11 );
       
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 6, 4 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 8, 6 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 2, 8 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 10, 2 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 1, 4, 10 );
       
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 2, 7, 8 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 2, 5, 7 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 2, 10, 5 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 4, 9, 10 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 5, 10, 9 );
       
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 3, 11, 7 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 3, 7, 5 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 3, 5, 9 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 6, 8, 11 );
         SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, depth, 7, 11, 8 );
      }
   }

   numNodes = (nodePtCt+1)/3;
   numTri = (triPtCt+1)/3;

   if (LocalHead) fprintf(SUMA_STDERR,"%s: There are %d nodes, %d triangles in the icosahedron.\n", FuncName, numNodes, numTri);

   /* store in SO and get out */
   SO->NodeList = icosaNode;
   SO->FaceSetList = icosaTri;
   SO->N_Node = numNodes;
   SO->N_FaceSet = numTri;
   SO->NodeDim = 3;
   SO->FaceSetDim = 3;
   SO->idcode_str = (char *)SUMA_calloc (SUMA_IDCODE_LENGTH, sizeof(char));   
   UNIQ_idcode_fill (SO->idcode_str);
   
   /* check the winding ? */
   if (DoWind) {
      fprintf(SUMA_STDOUT, "%s: Making Edge list ....\n", FuncName); 
      SO->EL = SUMA_Make_Edge_List (SO->FaceSetList, SO->N_FaceSet, SO->N_Node, SO->NodeList);
      if (SO->EL == NULL) {
         fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_Make_Edge_List. Neighbor list will not be created\n", FuncName);
         SUMA_Free_Surface_Object (SO);
         SUMA_RETURN (NULL);
      } else {
         fprintf(SUMA_STDOUT, "%s: Creating inode for %s....\n", FuncName, SO->idcode_str); 
         /* you also need to create EL_Inode with EL */
         SO->EL_Inode = SUMA_CreateInode ((void *)SO->EL, SO->idcode_str);
         if (!SO->EL_Inode) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInode\n", FuncName);
         }
         fprintf(SUMA_STDOUT, "%s: Creating inode DONE....\n", FuncName); 
      }
      
      if (!SUMA_MakeConsistent (SO->FaceSetList, SO->N_FaceSet, SO->EL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_MakeConsistent.\n", FuncName);
         SUMA_Free_Surface_Object (SO);
         SUMA_RETURN (NULL);
      }
      else {
         fprintf(SUMA_STDERR,"%s: Eeeexcellent. All triangles consistent.\n", FuncName);
      }
      /* determine the MemberFaceSets */
      fprintf(SUMA_STDOUT, "%s: Determining MemberFaceSets  ...\n", FuncName);
      SO->MF = SUMA_MemberFaceSets(SO->N_Node, SO->FaceSetList, SO->N_FaceSet, SO->FaceSetDim);
      if (SO->MF->NodeMemberOfFaceSet == NULL) {
         fprintf(SUMA_STDERR,"Error %s: Error in SUMA_MemberFaceSets\n", FuncName);
         SO->MF = NULL;
         SUMA_Free_Surface_Object (SO);
         SUMA_RETURN (NULL);
      }
   }
   
   /* create surface normals */
   SN = SUMA_SurfNorm( SO->NodeList, SO->N_Node, SO->FaceSetList, SO->N_FaceSet);
   SO->NodeNormList = SN.NodeNormList;
   SO->FaceNormList = SN.FaceNormList;

   /*create first neighbor list*/
//   firstNeighb = SUMA_Build_FirstNeighb( SO->EL, SO->N_Node);
   if (!DoWind) SO->EL = SUMA_Make_Edge_List (SO->FaceSetList, SO->N_FaceSet, SO->N_Node, SO->NodeList);
   SO->FN = SUMA_Build_FirstNeighb( SO->EL, SO->N_Node);
   if(SO->FN==NULL) fprintf(SUMA_STDERR, "Error %s: Failed in creating neighb list.\n", FuncName);

   SUMA_RETURN (SO);
}

/*!
  SUMA_Boolean = SUMA_inNodeNeighb( surf, nodeList, node, PO, P1);

  Determines whether or not point P1 is inside of triangles of which node[0] or [1] or [2] is a node.
  \param surf (SUMA_SurfaceObject) surface being intersected by P1
  \param nodeList (float *) 3 x N_Node vector of nodes in surface (pass as NULL if equals surf->NodeList)
  \param node (int *) vector to contain 3 nodes of intersected triangle,
  originally contains three nodes to work with. if you 
  want only 1 or 2 nodes examined, use node[1] = -1 or 
  node[2] = -1, respectively
  \param PO (float *) point to form ray with P1 st ray slope = node normal of P1
  \param P1 (float *) intersecting point in question; if not on surface, returned with point where ray intersects surface
  \ret found (SUMA_Boolean) true if P1 in triangle with node[0] as a node
  
  Written by Ziad Saad / Brenna Argall
*/

SUMA_Boolean SUMA_inNodeNeighb( SUMA_SurfaceObject *surf, float *nodeList, int *node, float *P0, float *P1) {

   int i=0, j=0, k=0, examinedNum=0;
   SUMA_Boolean found=NOPE;
   float hitOnSurf[3];
   int  incidentTri[100], N_incident = 0, itry;
   int examinedTri[100], ifound, i_node0 = -1, i_node1 = -1, i_node2 = -1;
   SUMA_Boolean LocalHead = NOPE;
   static char FuncName[]={"SUMA_inNodeNeighb"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (nodeList==NULL) {
      fprintf (SUMA_STDERR, "Warning %s: Assigning surf->NodeList to nodeList.\n", FuncName); 
      nodeList = surf->NodeList;
   }

   if (LocalHead) fprintf(SUMA_STDERR, "%s: P0-P1 [%f, %f, %f] - [%f, %f, %f]\n", 
                          FuncName, P0[0], P0[1], P0[2], P1[0], P1[1], P1[2]);

   found = NOPE;
   itry = 0;
   examinedNum = 0;
   while (itry < 3 && node[itry] >= 0 && !found) {
      if (LocalHead) fprintf(SUMA_STDERR, "%s: Trying neighbors of node %d.\n", FuncName, node[itry]);
      i = 0;
      while ((i < surf->FN->N_Neighb[node[itry]] ) && !found) { 

         if (!SUMA_Get_Incident( node[itry], surf->FN->FirstNeighb[node[itry]][i], surf->EL, incidentTri, &N_incident)) {
            fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Get_Incident.\n", FuncName);
            SUMA_RETURN (NOPE);
         }

         /**check triangles incident to current edge*/
         j = 0;
         while ((j < N_incident) && !found) {

            /**triangle in list?*/
            SUMA_IS_IN_VEC(examinedTri, examinedNum, incidentTri[j], ifound);
            
            /**if not found , add index to list and test for intersection*/
            if (ifound < 0) {
               examinedTri[examinedNum] = incidentTri[j];
               ++examinedNum;

               i_node0 = surf->FaceSetList[ 3*incidentTri[j] ];
               i_node1 = surf->FaceSetList[ 3*incidentTri[j]+1 ];
               i_node2 = surf->FaceSetList[ 3*incidentTri[j]+2 ];

               if (SUMA_MT_isIntersect_Triangle (P0, P1, &(nodeList[3*i_node0]), &(nodeList[3*i_node1]), 
                                                 &(nodeList[3*i_node2]), hitOnSurf, NULL, NULL)) {
                  found = YUP;
                  node[0] = i_node0;
                  node[1] = i_node1;
                  node[2] = i_node2;
                  if (LocalHead) {
                     fprintf(SUMA_STDERR, "%s: Triangle %d [%d, %d, %d] is intersected at (%f, %f, %f)\n", 
                             FuncName, incidentTri[j], node[0], node[1], node[2], hitOnSurf[0], hitOnSurf[1], hitOnSurf[2]);
                     fprintf(SUMA_STDERR, "%s: Coordinates of nodes forming triangle are:\n", FuncName);
                     fprintf(SUMA_STDERR, "%f, %f, %f\n", nodeList[3*i_node0], nodeList[3*i_node0+1], nodeList[3*i_node0+2]);
                     fprintf(SUMA_STDERR, "%f, %f, %f\n", nodeList[3*i_node1], nodeList[3*i_node1+1], nodeList[3*i_node1+2]);
                     fprintf(SUMA_STDERR, "%f, %f, %f\n", nodeList[3*i_node2], nodeList[3*i_node2+1], nodeList[3*i_node2+2]);
                  }  
#if 0 /* turn on to compare intersection results to those obtained with SUMA_MT_intersect_triangle */
                  {
                     /* try the other (slower) method for intersection and compare results*/
                     SUMA_MT_INTERSECT_TRIANGLE *MTI;
                     MTI = SUMA_MT_intersect_triangle(P1, P0, nodeList, surf->N_Node, surf->FaceSetList, surf->N_FaceSet, NULL);
                     if (MTI) {
                        if (LocalHead)fprintf(SUMA_STDERR, "%s: Meth2-Triangle %d [%d, %d, %d] is intersected at (%f, %f, %f)\n", 
                                              FuncName, MTI->ifacemin, surf->FaceSetList[3*MTI->ifacemin], surf->FaceSetList[3*MTI->ifacemin+1],
                                              surf->FaceSetList[3*MTI->ifacemin+2], MTI->P[0], MTI->P[1], MTI->P[2]);  

                        if (MTI->N_hits) {
                           /* compare results */
                           if (MTI->ifacemin != incidentTri[j]) {
                              fprintf (SUMA_STDERR,"Error %s: Warning, mismatch in results of triangle intersection. This should not be\n", FuncName);
                              exit(1);
                           }
                        }

                        MTI = SUMA_Free_MT_intersect_triangle(MTI);
                     } 

                  }
#endif  

                  P1[0] = hitOnSurf[0];  P1[1] = hitOnSurf[1];  P1[2] = hitOnSurf[2];
               }else {
                  if (LocalHead)fprintf(SUMA_STDERR, "%s: Triangle %d [%d, %d, %d] is not intersected.\n",
                                        FuncName, incidentTri[j], i_node0, i_node1, i_node2);
               } 
            }
            ++j;
         }
         ++i;
      }
      ++itry;   
   }
  
   SUMA_RETURN (found);
}


/*!
  weight = SUMA_detWeight ( node0, node1, node2, hitPt );

  This function determines the weight of each of three nodes on a given point based upon distance. 
  \param node0 (double[3]) contains x,y,z coordinates for first node
  \param node1 (double[3]) contains x,y,z coordinates for second node
  \param node2 (double[3]) contains x,y,z coordinates for third node
  \param ptHit (double[3]) contains x,y,z coordinates for point feeling weight
  \return weight (double[3]) contains weights for each node0, node1, node2

  Written by Brenna Argall
*/
float * SUMA_detWeight (float node0[3], float node1[3], float node2[3], float ptHit[3]) {

   int i=0;
   float triNode0[3], triNode1[3], triNode2[3];
   float p00[3], p01[3], p02[3];
   float p10[3], p11[3], p12[3];
   float p20[3], p21[3], p22[3];
   float tri0[3], tri1[3], tri2[3], triOrig[3];
   float s0=0, s1=0, s2=0, sOrig=0, A0=0, A1=0, A2=0, Aorig=0;
   float wsum=0, *weight=NULL;
   static char FuncName[]={"SUMA_detWeight"};
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
  
   /*weights determined by linear interpolation based on areas of triangles resulting
     from lines parallel to edges of hit triangle and intersecting ptHit (see p6-12 LNB)*/
  
   p00[0] = node0[0];  p00[1] = node0[1];  p00[2] = node0[2];
   p11[0] = node1[0];  p11[1] = node1[1];  p11[2] = node1[2];
   p22[0] = node2[0];  p22[1] = node2[1];  p22[2] = node2[2];

   /**end points of parallel lines*/
 
   /** (nodes of subtriangle / associated with original node) */
   /** (p00,p01,p02 / triNode0), (p10,p11,p12 / triNode1), (p20,p21,p22 / triNode2)*/
   for (i=0; i<3; ++i) {
      /*assign p01*/
      if (p00[i]==p22[i]) { p01[i] = intersection_map( p11[i], p22[i], p00[i], p11[i], ptHit[i] ); }
      else { p01[i] = intersection_map( p11[i], p22[i], p11[i], p00[i], ptHit[i] ); }
      /*assign p02*/
      if (p11[i]==p00[i]) { p02[i] = intersection_map( p11[i], p22[i], p22[i], p00[i], ptHit[i] ); }
      else { p02[i] = intersection_map( p11[i], p22[i], p00[i], p22[i], ptHit[i] ); }
      /*assign p10*/
      if (p22[i]==p11[i]) { p10[i] = intersection_map( p22[i], p00[i], p00[i], p11[i], ptHit[i] ); }
      else { p10[i] = intersection_map( p22[i], p00[i], p11[i], p00[i], ptHit[i] ); }
      /*assign p12*/
      if (p11[i]==p00[i]) { p12[i] = intersection_map( p22[i], p00[i], p11[i], p22[i], ptHit[i] ); }
      else { p12[i] = intersection_map( p22[i], p00[i], p22[i], p11[i], ptHit[i] ); }
      /*assign p20*/
      if (p22[i]==p11[i]) { p20[i] = intersection_map( p00[i], p11[i], p22[i], p00[i], ptHit[i] ); }
      else { p20[i] = intersection_map( p00[i], p11[i], p00[i], p22[i], ptHit[i] ); }
      /*assign p21*/
      if (p00[i]==p22[i]) { p21[i] = intersection_map( p00[i], p11[i], p11[i], p22[i], ptHit[i] ); }
      else { p21[i] = intersection_map( p00[i], p11[i], p22[i], p11[i], ptHit[i] ); }
   }

   /**length of subtriangle edges*/

   tri0[0] = sqrt( pow(p01[0]-p00[0],2) + pow(p01[1]-p00[1],2) + pow(p01[2]-p00[2],2) );
   tri0[1] = sqrt( pow(p02[0]-p01[0],2) + pow(p02[1]-p01[1],2) + pow(p02[2]-p01[2],2) );
   tri0[2] = sqrt( pow(p00[0]-p02[0],2) + pow(p00[1]-p02[1],2) + pow(p00[2]-p02[2],2) );
  
   tri1[0] = sqrt( pow(p11[0]-p10[0],2) + pow(p11[1]-p10[1],2) + pow(p11[2]-p10[2],2) );
   tri1[1] = sqrt( pow(p12[0]-p11[0],2) + pow(p12[1]-p11[1],2) + pow(p12[2]-p11[2],2) );
   tri1[2] = sqrt( pow(p10[0]-p12[0],2) + pow(p10[1]-p12[1],2) + pow(p10[2]-p12[2],2) );
  
   tri2[0] = sqrt( pow(p21[0]-p20[0],2) + pow(p21[1]-p20[1],2) + pow(p21[2]-p20[2],2) );
   tri2[1] = sqrt( pow(p22[0]-p21[0],2) + pow(p22[1]-p21[1],2) + pow(p22[2]-p21[2],2) );
   tri2[2] = sqrt( pow(p20[0]-p22[0],2) + pow(p20[1]-p22[1],2) + pow(p20[2]-p22[2],2) );
  
   /**area of subtriangles*/
  
   s0 = .5*(tri0[0] + tri0[1] + tri0[2]);
   s1 = .5*(tri1[0] + tri1[1] + tri1[2]);
   s2 = .5*(tri2[0] + tri2[1] + tri2[2]);
  
   A0 = sqrt( s0*(s0-tri0[0])*(s0-tri0[1])*(s0-tri0[2]) );
   A1 = sqrt( s1*(s1-tri1[0])*(s1-tri1[1])*(s1-tri1[2]) );
   A2 = sqrt( s2*(s2-tri2[0])*(s2-tri2[1])*(s2-tri2[2]) );

   /*length of edges and area of original triangle*/

   triOrig[0] = sqrt( pow(p11[0]-p00[0],2) + pow(p11[1]-p00[1],2) + pow(p11[2]-p00[2],2) );
   triOrig[1] = sqrt( pow(p22[0]-p11[0],2) + pow(p22[1]-p11[1],2) + pow(p22[2]-p11[2],2) );
   triOrig[2] = sqrt( pow(p00[0]-p22[0],2) + pow(p00[1]-p22[1],2) + pow(p00[2]-p22[2],2) );

   sOrig = .5*(triOrig[0] + triOrig[1] + triOrig[2]);
   Aorig = sqrt( sOrig*(sOrig-triOrig[0])*(sOrig-triOrig[1])*(sOrig-triOrig[2]) );
  
   /**weights*/
   weight = (float *)SUMA_calloc( 3, sizeof(float) );
   weight[0] = (Aorig-A0)/Aorig;  weight[1] = (Aorig-A1)/Aorig;  weight[2] = (Aorig-A2)/Aorig;
   wsum = weight[0] + weight[1] + weight[2];
   weight[0] = weight[0]/wsum;  weight[1] = weight[1]/wsum;  weight[2] = weight[2]/wsum;
  
   //  fprintf(SUMA_STDERR, "weight: (%f, %f, %f)\n", weight[0], weight[1], weight[2]);
  
   SUMA_RETURN (weight);

} 

/*!
  SUMA_binSearch( nodeList, target, seg);

  This function performs a binary search.  The indices of the elements in nodeList surrounding target will be stored in (overwrite) seg; thus seg[0]=seg[1]=i implies that an exact match was found at index i.
  \param nodeList (float *) vector of sorted values
  \param target (float) value seeking
  \param seg (int *) contains begin and end point of segment being searched

  Written by Brenna Argall
*/
void SUMA_binSearch( float *nodeList, float target, int *seg) {
  
   int mid=0;
   int beg = seg[0], end = seg[1];
   static char FuncName[]={"SUMA_binSearch"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (beg!=end) {
      mid =(end-beg)/2 + beg;
      /**no exact match, but elements above and below found*/
      if (beg+1==end) {
         seg[0] = beg;
         seg[1] = end;
      }
      else if (target==nodeList[mid]) {
         seg[0] = mid;
         seg[1] = mid;
      }
      /**keep searching*/
      else if ( target  < nodeList[mid]) {
         seg[0] = beg;  seg[1] = mid;
         SUMA_binSearch( nodeList, target, seg);
      }
      else if ( target > nodeList[mid]) {
         seg[0] = mid;  seg[1] = end;
         SUMA_binSearch( nodeList, target, seg);
      }
   }
   /**exact match; beg==end or target==nodeList[ indexList[mid] ]*/
   else {
      seg[0] = mid;
      seg[1] = mid;
   }
  
   SUMA_RETURNe;
}
 
/**gives value for intersection of two lines, as defined in SUMA_MapSurface (see p10 LNB)*/
float intersection_map(float a, float b, float c, float d, float val) {
  
   float sol = (val*(c-d) - d*(a-b)) / (c+b-a-d);

   return sol;
}


/*!
  MI = MapSurface (surf1, surf2);

  This function creates a mapping of one surface onto another (surfaces assumed to be spherical).
  \param surf1 (SUMA_SurfaceObject *) first surface of surface object structure
  \param surf2 (SUMA_SurfaceObject *) second surface of surface object structure
  \return MI (SUMA_MorphInfo *) contains information necessary to perform forwards and backwards morphing;
  returns NULL if function fails.
  MI returned with N_Node, N_FaceSet, Weight, ClsNodes and FaceSetList.

  Written by Brenna Argall
*/

SUMA_MorphInfo * SUMA_MapSurface (SUMA_SurfaceObject *surf1, SUMA_SurfaceObject *surf2)
{

   static char FuncName[]={"SUMA_MapSurface"};

   /**surf1 variables*/
   int numNodes_1=0, numFace_1=0;
   float *nodeList_1=NULL, *ctrNodeList_1=NULL;
   int *faceList_1=NULL;

   /**surf2 variables*/
   int numNodes_2=0, numFace_2=0;
   float *nodeList_2=NULL, *ctrNodeList_2=NULL;
   int *faceList_2=NULL;

   int i=0, j=0, k=0, m=0;
   float *weight=NULL;
   int *clsNodes=NULL;
   SUMA_MorphInfo *MI;
   float ctr1[3], ctr2[3], zero[3];
   float a=0, b=0, c=0, r2=0;
   float  *justX_2=NULL;
   int *iSrtd_2=NULL;
   float currNode[3], ptHit[3], currDist=0, avgDist=0.0, pi=3.14159265359;
   int seg[2];

   float d0=100, d1=100, d2=100, tempD=100;
   SUMA_Boolean found=NOPE;
   int i_node0=0, i_node1=0, i_node2=0, nodes[3];
   float *triNode0, *triNode1, *triNode2, weight_tot;
   SUMA_SO_map *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   MI = SUMA_Create_MorphInfo();
   if (MI == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for MorphInfo.\n", FuncName);
      SUMA_RETURN (NULL);
   }  

   /**assign surf1 variables*/
   nodeList_1 = surf1->NodeList;
   faceList_1 = surf1->FaceSetList;
   numNodes_1 = surf1->N_Node;
   numFace_1 = surf1->N_FaceSet;

   /**assign surf2 variables*/
   nodeList_2 = surf2->NodeList;
   faceList_2 = surf2->FaceSetList;
   numNodes_2 = surf2->N_Node;
   numFace_2 = surf2->N_FaceSet;

   clsNodes = (int *)SUMA_calloc( 3*numNodes_1, sizeof(int) );
   weight = (float *)SUMA_calloc( 3*numNodes_1, sizeof(float) );
   if (!clsNodes || !weight) {
      if (clsNodes) SUMA_free(clsNodes);
      if (weight) SUMA_free(weight);
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for clsNodes || weight.\n", FuncName);
      SUMA_RETURN (NULL);
   }

   /**center surf1 to surf2 , that will make it easier to debug in SUMA*/
   ctr1[0]=0; ctr1[1]=0; ctr1[2]=0;
   ctr2[0]=0; ctr2[1]=0; ctr2[2]=0;

   for (i=0; i<numNodes_1; ++i) {
      j = 3*i;
      ctr1[0] = ctr1[0] + nodeList_1[j];
      ctr1[1] = ctr1[1] + nodeList_1[j+1];
      ctr1[2] = ctr1[2] + nodeList_1[j+2];
   }
   ctr1[0] = ctr1[0]/numNodes_1;
   ctr1[1] = ctr1[1]/numNodes_1;
   ctr1[2] = ctr1[2]/numNodes_1;

   for (i=0; i<numNodes_2; ++i) {
      j = 3*i;
      ctr2[0] = ctr2[0] + nodeList_2[j];
      ctr2[1] = ctr2[1] + nodeList_2[j+1];
      ctr2[2] = ctr2[2] + nodeList_2[j+2];
   }
   ctr2[0] = ctr2[0]/numNodes_2;
   ctr2[1] = ctr2[1]/numNodes_2;
   ctr2[2] = ctr2[2]/numNodes_2;

   /* set the zero center to be that of surf 2 */
   zero[0] = ctr2[0];
   zero[1] = ctr2[1];
   zero[2] = ctr2[2];
   
   ctrNodeList_1 = (float *) SUMA_calloc( 3*numNodes_1, sizeof(float) );
   ctrNodeList_2 = (float *) SUMA_calloc( 3*numNodes_2, sizeof(float) );
   if (!ctrNodeList_1 || !ctrNodeList_2) {
      if (ctrNodeList_1) SUMA_free(ctrNodeList_1);
      if (ctrNodeList_2) SUMA_free(ctrNodeList_2);
      if (clsNodes) SUMA_free(clsNodes);
      if (weight) SUMA_free(weight);
      if (iSrtd_2) SUMA_free(iSrtd_2);
      if (justX_2) SUMA_free(justX_2);
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for ctrNodeList_1 || ctrNodeList_2.\n", FuncName);
      SUMA_RETURN (NULL);
   }


   /* one of these two loops will be useless if we stick to having zero be the center of the one  of the two surfaces.... */
   for (i=0; i<numNodes_1; ++i) {
      j = 3*i;
      ctrNodeList_1[j]   = nodeList_1[j]   - ctr1[0] + zero[0];
      ctrNodeList_1[j+1] = nodeList_1[j+1] - ctr1[1] + zero[1];
      ctrNodeList_1[j+2] = nodeList_1[j+2] - ctr1[2] + zero[2];
   }
   for (i=0; i<numNodes_2; ++i) {
      j = 3*i;
      ctrNodeList_2[j]   = nodeList_2[j]   - ctr2[0] + zero[0];
      ctrNodeList_2[j+1] = nodeList_2[j+1] - ctr2[1] + zero[1];
      ctrNodeList_2[j+2] = nodeList_2[j+2] - ctr2[2] + zero[2];
   }
  
   r2 = sqrt( pow( ctrNodeList_2[0], 2) + pow( ctrNodeList_2[1], 2) + pow( ctrNodeList_2[2], 2) );
   avgDist = (4*pi*pow(r2,2))/numNodes_2;    //average distance between nodes on surf2 surface+
  
   /**sort x of NodeList_2*/
   justX_2 = (float *) SUMA_calloc( numNodes_2, sizeof(float) );
   if (!justX_2 ) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for justX_2.\n", FuncName);
      if (ctrNodeList_1) SUMA_free(ctrNodeList_1);
      if (ctrNodeList_2) SUMA_free(ctrNodeList_2);
      if (clsNodes) SUMA_free(clsNodes);
      if (weight) SUMA_free(weight);
      SUMA_RETURN (NULL);
   }
  
   for (i=0; i<numNodes_2; ++i) {
      j = 3*i;
      justX_2[i] = ctrNodeList_2[j];
   }
  
   iSrtd_2 = SUMA_z_qsort( justX_2, numNodes_2 );
   if (!iSrtd_2) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_z_qsort.\n", FuncName);
      if (ctrNodeList_1) SUMA_free(ctrNodeList_1);
      if (ctrNodeList_2) SUMA_free(ctrNodeList_2);
      if (clsNodes) SUMA_free(clsNodes);
      if (weight) SUMA_free(weight);
      if (justX_2) SUMA_free(justX_2);

      SUMA_RETURN (NULL);
   }
  
   /** mapping surf1 to surf2 */
   fprintf(SUMA_STDERR,"Computing intersections...\n");
   
   for (i=0; i<numNodes_1; ++i) {

      j=3*i; 
      currNode[0]=ctrNodeList_1[j];
      currNode[1]=ctrNodeList_1[j+1];
      currNode[2]=ctrNodeList_1[j+2];
    
      /**compute inflation of node onto sphere by adjusting surf1 node so that its distance from (0,0,0)
         exactly equals the radius of the spherical surf2 (r2)*/
      currDist = sqrt( pow(currNode[0], 2) + pow(currNode[1], 2) + pow(currNode[2], 2) );
      ptHit[0] = (r2/currDist)*currNode[0];
      ptHit[1] = (r2/currDist)*currNode[1];
      ptHit[2] = (r2/currDist)*currNode[2];

      /**find 3 nodes in ctrNodeList_2 closest to ptHit*/
      d0=100;  d1=100;  d2=100; tempD=100; 
      i_node0=-1; i_node1=-1, i_node2=-1;  
      seg[0] = 0; 
      seg[1] = numNodes_2-1;
    
      SUMA_binSearch( justX_2, ptHit[0], seg );
      k = seg[0];
      while ( ptHit[0] - ctrNodeList_2[3*iSrtd_2[k]] < 10*avgDist && k>0) { --k; }
      seg[0] = k+1;
      k = seg[1];
      while (ctrNodeList_2[3*iSrtd_2[k]] - ptHit[0] < 10*avgDist && k<numNodes_2-1) { ++k; }
      seg[1] = k-1;
    
      /*make certain there are at least 3 nodes in the segment*/
      if (seg[1]-seg[0]<3) {
         if (seg[1]>numNodes_2-5) {
            seg[0] = seg[0]-6;
         }
         else if (seg[0]<4) {
            seg[1] = seg[1]+6;
         }
         else {
            seg[0] = seg[0]-3;
            seg[1] = seg[1]+3;
         }
      }
    
      if (LocalHead) fprintf(SUMA_STDERR,"----------------------------------------\n");
      for (k=seg[0]; k<=seg[1]; ++k) {
         m = 3*iSrtd_2[k];
         if (ptHit[1]-ctrNodeList_2[m+1] < 10*avgDist) {
            if (ptHit[1]-ctrNodeList_2[m+1] > -10*avgDist) {
               if (ptHit[2]-ctrNodeList_2[m+2] < 10*avgDist) {
                  if (ptHit[2]-ctrNodeList_2[m+2] > -10*avgDist) {
      
                     tempD = sqrt( pow(ptHit[0]-ctrNodeList_2[m],2) + pow(ptHit[1]-ctrNodeList_2[m+1],2) + 
                                   pow(ptHit[2]-ctrNodeList_2[m+2],2) );
         
                     if (tempD < d2) {
                        if (tempD < d1) {
                           if (tempD < d0) {
                              d2 = d1;    i_node2 = i_node1;  
                              d1 = d0;    i_node1 = i_node0; 
                              d0 = tempD;  i_node0 = iSrtd_2[k]; 
                           } else {
                              d2 = d1;    i_node2 = i_node1;
                              d1 = tempD;  i_node1 = iSrtd_2[k];
                           }
                        } else {
                           d2 = tempD;  i_node2 = iSrtd_2[k];
                        }
                     }
                  }
               }
            }
         }
      }

      if (LocalHead) {
         fprintf(SUMA_STDERR, "%s: PtHit: [%f, %f, %f].\n", FuncName, ptHit[0], ptHit[1], ptHit[2]);
         fprintf(SUMA_STDERR, "%s: Node %d [%f, %f, %f], distances %f.\n", 
                 FuncName, i_node0, ctrNodeList_2[3*i_node0], ctrNodeList_2[3*i_node0+1], ctrNodeList_2[3*i_node0+2], d0);
         fprintf(SUMA_STDERR, "%s: Node %d [%f, %f, %f], distances %f.\n", 
                 FuncName, i_node1, ctrNodeList_2[3*i_node1], ctrNodeList_2[3*i_node1+1], ctrNodeList_2[3*i_node1+2], d1);
         fprintf(SUMA_STDERR, "%s: Node %d [%f, %f, %f], distances %f.\n", 
                 FuncName, i_node2, ctrNodeList_2[3*i_node2], ctrNodeList_2[3*i_node2+1], ctrNodeList_2[3*i_node2+2], d2);
      }  
      /**find nodes of intersected triangle*/

      if (surf2->FN == NULL) {
         fprintf(SUMA_STDERR, "%s: Surf2->FN is NULL.  Exiting.\n", FuncName);
         exit(1);
      }

      if (LocalHead) {
         fprintf(SUMA_STDERR, "%s: orig ptHit (%f, %f, %f)\n", FuncName, ptHit[0], ptHit[1], ptHit[2]);
         fprintf(SUMA_STDERR, "%s: Trying 1- node %d\n", FuncName, i_node0);
      }

      /* search neighborhoods of closest 3 nodes */
      nodes[0] = i_node0;  nodes[1] = i_node1;  nodes[2] = i_node2;
      found = SUMA_inNodeNeighb( surf2, ctrNodeList_2, nodes, zero, ptHit);

      if (!found) {
         /* try brute force */
         if (LocalHead) fprintf(SUMA_STDERR, "%s: Trying Brute force.\n", FuncName);
         {
            SUMA_MT_INTERSECT_TRIANGLE *MTI;
         
            MTI = SUMA_MT_intersect_triangle(ptHit, zero, ctrNodeList_2, surf2->N_Node, surf2->FaceSetList, surf2->N_FaceSet, NULL);
            if (MTI) {
               if (MTI->N_hits) {
                  if (LocalHead)fprintf(SUMA_STDERR, "%s: Brute force-Triangle %d [%d, %d, %d] is intersected at (%f, %f, %f)\n", 
                                        FuncName, MTI->ifacemin, surf2->FaceSetList[3*MTI->ifacemin], surf2->FaceSetList[3*MTI->ifacemin+1],
                                        surf2->FaceSetList[3*MTI->ifacemin+2], MTI->P[0], MTI->P[1], MTI->P[2]);  
                  found = YUP;
                  ptHit[0] = MTI->P[0];
                  ptHit[1] = MTI->P[1];
                  ptHit[2] = MTI->P[2];
               }

               MTI = SUMA_Free_MT_intersect_triangle(MTI);
            } 

         }
      }
   
      if (!found) {
         nodes[0] = i_node0;  nodes[1] = i_node1;  nodes[2] = i_node2;
         fprintf(SUMA_STDERR, "Error %s: !!!!!!!!!! intersected triangle not found.\n", FuncName);
         exit (1);
      } 
    
      if (LocalHead) fprintf (SUMA_STDERR, "%s: (%d : %d : %d)\n  ptHit(%f, %f, %f)\n", FuncName, nodes[0], nodes[1], nodes[2], ptHit[0], ptHit[1], ptHit[2]);

      /**node indices of triangle intersected by ptHit*/
      clsNodes[j] = nodes[0];  clsNodes[j+1] = nodes[1];  clsNodes[j+2] = nodes[2];
    
      /** pointers to x,y,z of each node of intersected triangle*/
      triNode0 = &(ctrNodeList_2[ 3*nodes[0] ]);
      triNode1 = &(ctrNodeList_2[ 3*nodes[1] ]);
      triNode2 = &(ctrNodeList_2[ 3*nodes[2] ]);
    
      /**determine weights which are the barycetric corrdinates of the intersection node*/
      SUMA_TRI_AREA( ptHit, triNode1, triNode2, weight[j]); 
      SUMA_TRI_AREA( ptHit, triNode0, triNode2, weight[j+1]); 
      SUMA_TRI_AREA( ptHit, triNode0, triNode1, weight[j+2]); /* if the index of the intersected triangle is very cheap to obtain, 
                                                                 you could set weight[j+2] = SO->PolyArea[Face] - weight[j+1] - weight[j+0] 
                                                                 Of course, you must first compute PolyArea with SUMA_SurfaceMetrics*/

      weight_tot = weight[j] + weight[j+1] + weight[j+2];
      if (weight_tot) {
         weight[j] /= weight_tot;
         weight[j+1] /= weight_tot;
         weight[j+2] /= weight_tot;
      }else { /* some triangles have zero area in FreeSurfer surfaces */
         weight[j] = weight[j+1] = weight[j+2] = 1.0/3.0;
      }

   }

   MI->N_Node = numNodes_1;
   MI->N_FaceSet = numFace_1;
   MI->Weight = weight;
   MI->ClsNodes = clsNodes;
   MI->FaceSetList = (int *) SUMA_calloc( 3*numFace_1, sizeof(int));
   if (!MI->FaceSetList) {
      fprintf(SUMA_STDERR, "Error %s: Failed to allocate for MI->FaceSetList.\n", FuncName);
      if (ctrNodeList_1) SUMA_free(ctrNodeList_1);
      if (ctrNodeList_2) SUMA_free(ctrNodeList_2);
      if (clsNodes) SUMA_free(clsNodes);
      if (weight) SUMA_free(weight);
      if (iSrtd_2) SUMA_free(iSrtd_2);
      if (justX_2) SUMA_free(justX_2);
      SUMA_RETURN (NULL);
   }
   for (i=0; i<numFace_1; ++i) {
      j = 3*i;
      MI->FaceSetList[j] = faceList_1[j];
      MI->FaceSetList[j+1] = faceList_1[j+1];
      MI->FaceSetList[j+2] = faceList_1[j+2];
   }

   if (ctrNodeList_1) SUMA_free(ctrNodeList_1);
   if (ctrNodeList_2) SUMA_free(ctrNodeList_2);
   if (iSrtd_2) SUMA_free(iSrtd_2);
   if (justX_2) SUMA_free(justX_2);
  
   SUMA_RETURN (MI);
} 
 


/*!
  function used to create a SUMA_SO_map structure
*/
SUMA_SO_map *SUMA_Create_SO_map (void) 
{
   static char FuncName[]={"SUMA_Create_SO_map"};
   SUMA_SO_map *SOM = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SOM = (SUMA_SO_map *) SUMA_malloc (sizeof(SUMA_SO_map));
   if (!SOM) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for SOM.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   SOM->N_Node = 0;
   SOM->NewNodeList = NULL;
   SOM->NodeVal = NULL;
   SOM->NodeDisp = NULL;
   SOM->NodeCol = NULL;
   
   SUMA_RETURN (SOM);
}

/*!
  function to free SO_map
*/
SUMA_Boolean SUMA_Free_SO_map (SUMA_SO_map *SOM) 
{
   static char FuncName[]={"SUMA_Free_SO_map"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SOM) {
      SUMA_RETURN (YUP);
   }

   if (SOM->NewNodeList) SUMA_free (SOM->NewNodeList);
   if (SOM->NodeVal) SUMA_free (SOM->NodeVal);
   if (SOM->NodeDisp) SUMA_free (SOM->NodeDisp);
   if (SOM->NodeCol) SUMA_free(SOM->NodeCol);
   
   SUMA_free (SOM);
   
   SUMA_RETURN (YUP);
}

/*!
  function to Show SO_map
*/
SUMA_Boolean SUMA_Show_SO_map (SUMA_SO_map *SOM, FILE *out) 
{
   static char FuncName[]={"SUMA_Show_SO_map"};
   int i=0, imax;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!out) out = SUMA_STDERR;
   
   fprintf (out, "\n%s: Showing contents of SUMA_SO_map structure:\n", FuncName); 
   if (!SOM) {
      fprintf (out, "\tpointer is NULL.\n");
      SUMA_RETURN (YUP);
   }
   
   if (SOM->N_Node > 5) imax = 5; 
   else imax = SOM->N_Node;
   
   fprintf (SUMA_STDERR, "NodeList, (1st %d elements):\n", imax);
   for (i=0; i<imax; ++i) {
      fprintf (SUMA_STDERR, "\t%f, %f, %f\n", SOM->NewNodeList[3*i], SOM->NewNodeList[3*i+1], SOM->NewNodeList[3*i+2]);
   }

   SUMA_RETURN (YUP);
}


/*!
  function used to create a SUMA_MorphInfo structure
*/
SUMA_MorphInfo *SUMA_Create_MorphInfo (void) 
{
   static char FuncName[]={"SUMA_Create_MorphInfo"};
   SUMA_MorphInfo *MI = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   MI = (SUMA_MorphInfo *) SUMA_malloc (sizeof(SUMA_MorphInfo));
   if (!MI) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for MI.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   MI->IDcode = NULL;
   MI->N_Node = 0;
   MI->N_FaceSet = 0;
   MI->Weight = NULL;
   MI->ClsNodes = NULL;
   MI->FaceSetList = NULL;
   
   SUMA_RETURN (MI);
}

/*!
  function to free MorphInfo
*/
SUMA_Boolean SUMA_Free_MorphInfo (SUMA_MorphInfo *MI) 
{
   static char FuncName[]={"SUMA_Free_MorphInfo"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!MI) {
      SUMA_RETURN (YUP);
   }

   if (MI->IDcode) SUMA_free (MI->IDcode);
   if (MI->Weight) SUMA_free (MI->Weight);
   if (MI->ClsNodes) SUMA_free (MI->ClsNodes);
   if (MI->FaceSetList) SUMA_free (MI->FaceSetList);
   
   SUMA_free (MI);
   
   SUMA_RETURN (YUP);
}

/*!
  newNodeList = SUMA_morphToStd( nodeList, MI);

  Function to morph surface to standard grid.
  \param nodeList (float *) 3 x N_Node vector containing nodes of surface being morphed
  \param MI (SUMA_MorphInfo *) structure containing morph information
  \ret newNodeList (float *) morphed nodeList

  Written by Brenna Argall
*/
float* SUMA_morphToStd (float *nodeList, SUMA_MorphInfo *MI) {

   float *newNodeList = NULL;
   int i=0, j=0;
   static char FuncName[] = {"SUMA_morphToStd"};
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
  
   newNodeList = (float *) SUMA_calloc( 3*MI->N_Node, sizeof(float));
   if (!newNodeList) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate. \n", FuncName);
      SUMA_RETURN (NULL);
   }
  
   for (i=0; i<(MI->N_Node); ++i){
      j = 3*i;
      newNodeList[j] = (MI->Weight[j])*nodeList[3*(MI->ClsNodes[j])] +         //node0 x
         (MI->Weight[j+1])*nodeList[3*(MI->ClsNodes[j+1])] +     //node1 x
         (MI->Weight[j+2])*nodeList[3*(MI->ClsNodes[j+2])];      //node2 x
      newNodeList[j+1] = (MI->Weight[j])*nodeList[3*(MI->ClsNodes[j])+1] +     //node0 y
         (MI->Weight[j+1])*nodeList[3*(MI->ClsNodes[j+1])+1] + //node1 y
         (MI->Weight[j+2])*nodeList[3*(MI->ClsNodes[j+2])+1];  //node2 y
      newNodeList[j+2] = (MI->Weight[j])*nodeList[3*(MI->ClsNodes[j])+2] +     //node0 z
         (MI->Weight[j+1])*nodeList[3*(MI->ClsNodes[j+1])+2] + //node1 z
         (MI->Weight[j+2])*nodeList[3*(MI->ClsNodes[j+2])+2];  //node2 z   
   }
  
   SUMA_RETURN( newNodeList);
}

/*!
  array = SUMA_readColor( numNodes, colFileNm);

  Function to read a colorfile into an array.
  \param numNodes (int) size of created array
  \param colFileNm (char *) name of color file to be read
  \ret colArray (float *) array of colorfile values

  Written by Brenna Argall
*/
float* SUMA_readColor (int numNodes, char* colFileNm) {

   float *colArray=NULL;
   FILE *colFile=NULL;
   char *line=NULL, *temp=NULL;
   int i=0, j=0, k=0, index=0;
   static char FuncName[]={"SUMA_readColor"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   colArray = (float *) SUMA_calloc( 3*numNodes, sizeof(float) );
   line = (char *) SUMA_calloc( 10000, sizeof(char));
   temp = (char *) SUMA_calloc( 10000, sizeof(char));

   if( (colFile = fopen(colFileNm, "r"))==NULL) {
      fprintf (SUMA_STDERR, "Failed in opening %s for reading.\n", colFileNm);
      exit(1);
   }
   else {
      fgets( line, 1000, colFile);
      while( !feof(colFile) ) {

         j = 3*index;
         i = 0;
         while ( isdigit(line[i]) ) ++i;
     
         ++i;  k=0;
         while ( !isspace(line[i])) {
            temp[k] = line[i];
            ++i;  ++k;
         }
         colArray[j] = atof(temp);
         SUMA_free(temp);
         temp = SUMA_calloc(10000, sizeof(char));
      
         ++i;  k=0;
         while ( !isspace(line[i])) {
            temp[k] = line[i];
            ++i;  ++k;
         }
         colArray[j+1] = atof(temp);
         SUMA_free(temp);
         temp = SUMA_calloc( 10000, sizeof(char));
      
         ++i;  k=0;
         while ( !isspace(line[i])) {
            temp[k] = line[i];
            ++i;  ++k;
         }
         colArray[j+2] = atof(temp);
         SUMA_free(temp);
         temp = SUMA_calloc( 10000, sizeof(char));
      
         fgets( line, 10000, colFile ); 
         ++index;
      }
   }
   SUMA_free(line);
   SUMA_free(temp);

   SUMA_RETURN( colArray);
}

/*!
  SUMA_writeColorFile(array, size, fileNm);

  Function to write out colorfile.
  \param array (float*) list of colors to be written
  \param size (int) size of array
  \param fileNm (char) name of file to be written to

  Written by Brenna Argall
*/
void SUMA_writeColorFile (float *array, int numNode, char fileNm[]) {   

   FILE *outFile=NULL;
   int i=0, j=0;
   static char FuncName[] = {"SUMA_writeColorFile"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if((outFile = fopen(fileNm, "w"))==NULL) {
      fprintf(SUMA_STDERR, "Could not open file %s.\n", fileNm);
      exit(1);
   }
   else {
      for (i=0; i < numNode; ++i) {
         j = i*3;
         fprintf (outFile, "%d\t%f\t%f\t%f\n", i, array[j], array[j+1], array[j+2]);
      }
      fclose (outFile);
   }
   SUMA_RETURNe;
}

/*!
  SUMA_writeFSfile(nodeList, faceList, numNode, numFace, firstLine, fileNm);

  Function to write out file in freesurfer format. 
  \param nodeList (float *) list of nodes
  \param faceList (int *) list of faces
  \param numNode (int) number of nodes
  \param numFace (int) number of faces
  \param firstLine (char) comment string for first line of file
  \param fileNm (char) name of file to be written to
  \ret void

  Written by Brenna Argall
*/
void SUMA_writeFSfile (float *nodeList, int *faceList, int numNode, int numFace, char firstLine[], char fileNm[]) {

   FILE *outFile=NULL;
   int i=0, j=0;
   static char FuncName[]={"SUMA_writeFSfile"};
  
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName); 
  
   outFile = fopen(fileNm, "w");
   if (!outFile) {
      fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, fileNm);
      exit(1);
   }
   else {
      fprintf (outFile,"%s\n", firstLine);
      fprintf (outFile, "%d %d\n", numNode, numFace);


    
      j=0;
      for (i=0; i<numNode; ++i) {
         j=3*i;
         fprintf (outFile, "%f  %f  %f  0\n", nodeList[j], nodeList[j+1], nodeList[j+2]);
      }
    
      j=0;
      for (i=0; i<numFace; ++i) {
         j = 3*i;
         fprintf (outFile, "%d %d %d 0\n", faceList[j], faceList[j+1], faceList[j+2]);
      }
    
      fclose(outFile);
   }
  
   SUMA_RETURNe;
}

/*!
  SUMA_writeSpecFile( numSurf, program, group, specFileNm);

  Function to write suma spec file.
  \param surfaces (SUMA_specSurfInfo *) necessary surface information for spec file
  \param numSurf (int) number of surfaces in spec file
  \param program (char[]) name of program calling function
  \param group (char[]) name of group
  \param fileNm (char[]) name of created spec file
  \return void

  Written by Brenna Argall
*/
void SUMA_writeSpecFile (SUMA_SpecSurfInfo *surfaces, int numSurf, char program[], char group[], char specFileNm[]) {

   FILE *outFile=NULL;
   int i=0, k=0, tag=0, ifSmwm=0;
   static char FuncName[]={"SUMA_writeSpecFile"};
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   outFile = fopen(specFileNm, "w");
   if (!outFile) {
      fprintf (SUMA_STDERR, "Failed in opening %s for writing.\n", specFileNm); 
      exit (1);
   }
   else {
      fprintf (outFile, "# %s spec file for %s\n\n", program, group);
      fprintf (outFile, "#define the group\n\tGroup = %s\n\n", group);
      fprintf (outFile, "#define various States\n");
      for (i=0; i<numSurf; ++i) {
         for (k=0; k<i; ++k) {
            if ( strcmp( surfaces[k].state, surfaces[i].state ) == 0) tag = -1;
         }
         if (tag==0) {
            fprintf( outFile, "\tStateDef = %s\n", surfaces[i].state);
         }
      }

      for (i=0; i<numSurf; ++i) {
         fprintf (outFile, "\nNewSurface\n\tSurfaceFormat = %s\n\tSurfaceType = %s\n", surfaces[i].format, surfaces[i].type);
         fprintf (outFile, "\tFreeSurferSurface = %s\n\tMappingRef = %s\n", surfaces[i].fileToRead, surfaces[i].mapRef );
         fprintf (outFile, "\tSurfaceState = %s\n\tEmbedDimension = %s\n", surfaces[i].state, surfaces[i].dim);
      }

      fclose(outFile);
   }
   SUMA_RETURNe;
}

#ifdef SUMA_CreateIcosahedron_STAND_ALONE

void SUMA_CreateIcosahedron_usage ()
   
{/*Usage*/
   printf ( "\n\33[1mUsage: \33[0mCreateIcosahedron [-rad r] [-rd recDepth] [-ld linDepth] \n"
            "                         [-ctr ctr] [-prefix fout] [-help]\n");
   printf ( "\n\t-rad r: size of icosahedron. (optional, default 100)\n");
   printf ( "\n\t-rd recDepth: recursive (binary) tesselation depth for icosahedron \n"
            "\t    (optional, default:3) \n"
            "\t    (recommended to approximate number of nodes in brain: 6\n"
            "\t    let rd2 = 2 * recDepth\n"
            "\t    Nvert = 2 + 10 * 2^rd2\n"
            "\t    Ntri  = 20 * 2^rd2\n"
            "\t    Nedge = 30 * 2^rd2\n");
   printf ( "\n\t-ld linDepth: number of edge divides for linear icosahedron tesselation\n"
            "\t    (optional, default uses binary tesselation).\n"
            "\t    Nvert = 2 + 10 * linDepth^2\n"
            "\t    Ntri  = 20 * linDepth^2\n"
            "\t    Nedge = 30 * linDepth^2\n");
   printf ( "\n\t-ctr ctr: coordinates of center of icosahedron. \n"
            "\t    (optional, default 0,0,0)\n");
   printf ( "\n\t-prefix fout: prefix for output files. \n"
            "\t    (optional, default CreateIco)\n");
   printf ( "\n\t-help: help message\n");
   printf ( "\n\t    Brenna D. Argall LBC/NIMH/NIH bargall@codon.nih.gov \n"
            "\t    Ziad S. Saad     SSC/NIMH/NIH ziad@nih.gov\n"
            "\t\t    Fri Sept 20 2002, Last Modified Thu Oct  2 2003\n\n");
   exit (0);
}/*Usage*/
/*!
  stand alone program to create an icosahedron and write it to file in Freesurfer format. 

*/
int main (int argc, char *argv[])
{/* main SUMA_CreateIcosahedron */
 
   static char FuncName[]={"SUMA_CreateIcosahedron-main"};
   int kar, depth, i, j;
   float r, ctr[3];
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean brk, LocalHead = NOPE;
   char fout[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
   char bin[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
   char surfFileNm[1000], outSpecFileNm[1000];
   SUMA_SpecSurfInfo *surfaces;

   /* allocate space for CommonFields structure */
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   if (LocalHead) fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);
   
   
   /* read in the options */
   r = 100;
   depth = 3;
   ctr[0] = 0; ctr[1] = 0; ctr[2] = 0;
   sprintf (fout, "%s", "CreateIco");
   sprintf (bin, "%s", "y");
   kar = 1;
   brk = NOPE;
   while (kar < argc) { /* loop accross command line options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         SUMA_CreateIcosahedron_usage ();
         exit (1);
      }
            
      if (!brk && (strcmp(argv[kar], "-rad") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -r ");
               exit (1);
            }
            r = atof(argv[kar]);
            brk = YUP;
         }      
      if (!brk && (strcmp(argv[kar], "-rd") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -rd ");
               exit (1);
            }
            depth = atoi(argv[kar]);
            sprintf (bin, "y");
            brk = YUP;

         }      
      if (!brk && (strcmp(argv[kar], "-ld") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -ld ");
               exit (1);
            }
            depth = atoi(argv[kar]);
            sprintf (bin, "n");
            brk = YUP;
         }      
      if (!brk && strcmp(argv[kar], "-ctr") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -ctr ");
               exit (1);
            }
            ctr[0] = atof(argv[kar]); kar ++;
            ctr[1] = atof(argv[kar]); kar ++;
            ctr[2] = atof(argv[kar]);

            brk = YUP;
         }   

      if (!brk && strcmp(argv[kar], "-prefix") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -so ");
               exit (1);
            }
            sprintf (fout, "%s", argv[kar]);

            brk = YUP;
         }   

      if (!brk) {
         fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command ine options */

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Recursion depth %d, Size %f.\n", FuncName, depth, r);

   /**assign output file names */
   sprintf (surfFileNm, "%s_surf.asc", fout);
   sprintf (outSpecFileNm, "%s.spec", fout);   

   if ( SUMA_filexists(surfFileNm) || SUMA_filexists(outSpecFileNm)) {
      fprintf (SUMA_STDERR,"Error %s: At least one of output files %s, %s exists.\nWill not overwrite.\n", \
               FuncName, surfFileNm, outSpecFileNm);
      exit(1);
   }


   /**create icosahedron*/
   SO = SUMA_CreateIcosahedron (r, depth, ctr, bin);
   if (!SO) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateIcosahedron.\n", FuncName);
      exit (1);
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, surfFileNm);


   /**write tesselated icosahedron to file*/
   SUMA_writeFSfile (SO->NodeList, SO->FaceSetList, SO->N_Node, SO->N_FaceSet, 
                     "#tesselated icosahedron for SUMA_CreateIcosahedron (SUMA_SphericalMapping.c)", surfFileNm);

   /**write spec file*/
   surfaces = (SUMA_SpecSurfInfo *) SUMA_calloc(1, sizeof(SUMA_SpecSurfInfo));

   strcpy (surfaces[0].format, "ASCII");  strcpy (surfaces[0].type, "FreeSurfer");   
   sprintf (surfaces[0].fileToRead, "%s", surfFileNm); strcpy( surfaces[0].mapRef, "SAME");  
   strcpy (surfaces[0].state, "icosahedron"); strcpy (surfaces[0].dim, "3");
  
   SUMA_writeSpecFile ( surfaces, 1, FuncName, fout, outSpecFileNm );
   fprintf (SUMA_STDERR, "\n* To view in SUMA, load spec file %s *\n\n", outSpecFileNm);

   /* free the surface object */
   SUMA_Free_Surface_Object (SO);
   SUMA_free(surfaces);

   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

   exit(0);
  
}/* main SUMA_CreateIcosahedron*/
#endif



#ifdef SUMA_Map_SurfacetoSurface_STAND_ALONE

void SUMA_Map_StoS_usage ()
   
{/*Usage*/
   printf ("\n\33[1mUsage: \33[0m SUMA_Map_SurfacetoSurface <-s1 spec1> <-s2 spec2> [-c col] [-prefix fout]\n");
   printf ("\n\tspec1: spec file containing first surface. (Only the first surface of the file is read)\n");
   printf ("\n\tspec2: spec file containing second surface. (Required to be spherical)\n");
   printf ("\n\tcol: a colorfile for the second surface, to be mapped (optional). \n");
   printf ("\n\tfout: prefix for output files. (optional, default StoS)\n");
   printf ("\n\t    Brenna D. Argall LBC/NIMH/NIH bargall@codon.nih.gov \n\t\t\t Fri Sept 20 14:23:42 EST 2002\n\n");
   exit (0);
}/*Usage*/
/*!
  stand alone program to map one surface to another (surf1->surf2) and write mapping to file in FreeSurfer format. 

*/
int main (int argc, char *argv[])
{/* main SUMA_Map_SurfacetoSurface */
   static char FuncName[]={"SUMA_Map_SurfacetoSurface-main"};
   SUMA_Boolean brk, LocalHead = NOPE, SurfIn = NOPE, color=NOPE;
   char fout[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
   char *surf2colFileNm=NULL;
   char surfFileNm[1000], colFileNm[1000], outSpecFileNm[1000];
   SUMA_SpecSurfInfo *surfaces=NULL;
 
   int kar, i, j;
   SUMA_SurfSpecFile spec1, spec2;  
   SUMA_SurfaceObject *surf1=NULL, *surf2=NULL;
   char *spec1File=NULL, *spec2File=NULL;
   char *surf1file=NULL, *surf2file=NULL;
   SUMA_SFname *surf1file_SF=NULL, *surf2file_SF=NULL;  //for surefit surfaces
   SUMA_MorphInfo *MI=NULL;
   float *colSurf2=NULL, *mapColSurf2=NULL, *mapSurf=NULL;
   char *tag1=NULL, *tag2=NULL;

   /* allocate space for CommonFields structure */
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   if (LocalHead) fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);
   
   /* read in the options */
   kar = 1;
   sprintf( fout, "%s", "StoS");
   brk = NOPE;
   if (argc < 4) {
      SUMA_Map_StoS_usage ();
      exit (1);
   }
   while (kar < argc) { /* loop accross command line options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         SUMA_Map_StoS_usage ();
         exit (1);
      }
            
      if (!brk && (strcmp(argv[kar], "-s1") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -ico ");
               exit (1);
            }
            spec1File = argv[kar];
            brk = YUP;
         }      
      
      if (!brk && strcmp(argv[kar], "-s2") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -sp ");
               exit (1);
            }
            spec2File = argv[kar];
            brk = YUP;
         }   

      if (!brk && strcmp(argv[kar], "-c") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -c ");
               exit (1);
            }
            surf2colFileNm = argv[kar];
            color = YUP;
            brk = YUP;
         }   

      if (!brk && strcmp(argv[kar], "-prefix") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -prefix ");
               exit (1);
            }
            sprintf (fout, "%s", argv[kar]);

            brk = YUP;
         }   

      if (!brk) {
         fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command line options */


   if (LocalHead) fprintf (SUMA_STDERR, "%s: %s contains first surface, %s contains second.\n", FuncName, spec1File, spec2File);
   
   if (spec1File == NULL || spec2File == NULL) {
      fprintf (SUMA_STDERR,"Error %s: No spec file specified for surf1 &/or surf2.\n", FuncName);
      exit(1);
   }
   if ( !SUMA_Read_SpecFile (spec1File, &spec1)) {
      fprintf(SUMA_STDERR,"Error %s: Error in %s SUMA_Read_SpecFile\n", FuncName, surf1file);
      exit(1);
   }
   if ( !SUMA_Read_SpecFile (spec2File, &spec2)) {
      fprintf(SUMA_STDERR,"Error %s: Error in %s SUMA_Read_SpecFile\n", FuncName, surf2file);
      exit(1);
   }

   /* assign output file names and information*/
   sprintf (surfFileNm, "%s_mappedSurf.asc", fout);
   sprintf (colFileNm, "%s_col.col", fout);
   sprintf (outSpecFileNm, "%s.spec", fout);
    
   if ( SUMA_filexists(surfFileNm) || SUMA_filexists(colFileNm) || SUMA_filexists(outSpecFileNm) ) {
      fprintf (SUMA_STDERR,"Error %s: At least one of output files %s, %s, %s exists.\nWill not overwrite.\n", \
               FuncName, surfFileNm, colFileNm, outSpecFileNm);
      exit(1);
   }
  

   /**load surfaces */

   if (SUMA_iswordin( spec1.SurfaceType[0], "FreeSurfer") == 1) {
      surf1file = spec1.FreeSurferSurface[0];
      surf1 = SUMA_Load_Surface_Object(surf1file, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
      tag1 = "FS";
   }
   else {
      if (SUMA_iswordin( spec1.SurfaceType[0], "SureFit") == 1) {
         surf1file_SF = (SUMA_SFname*) SUMA_malloc( sizeof(SUMA_SFname));
         strcpy( surf1file_SF->name_coord, spec1.CoordFile[0] );
         strcpy( surf1file_SF->name_topo, spec1.TopoFile[0] );
         strcpy( surf1file_SF->name_param, spec1.SureFitVolParam[0] );
         surf1 = SUMA_Load_Surface_Object( surf1file_SF, SUMA_SUREFIT, SUMA_ASCII, NULL);
         tag1 = "SF";
      }
   }

   if (SUMA_iswordin( spec2.State[0], "sphere") ==1) {
      if (SUMA_iswordin( spec2.SurfaceType[0], "FreeSurfer") == 1) {
         surf2file = spec2.FreeSurferSurface[0];
         surf2 = SUMA_Load_Surface_Object(surf2file, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
         tag2 = "FS";
      }
      else {
         if (SUMA_iswordin( spec2.SurfaceType[0], "SureFit") == 1) {
            surf1file_SF = (SUMA_SFname*) SUMA_malloc( sizeof(SUMA_SFname));
            strcpy( surf2file_SF->name_coord, spec2.CoordFile[0] );
            strcpy( surf2file_SF->name_topo, spec2.TopoFile[0] );
            strcpy( surf2file_SF->name_param, spec2.SureFitVolParam[0] );
            surf2 = SUMA_Load_Surface_Object( surf2file_SF, SUMA_SUREFIT, SUMA_ASCII, NULL);
            tag2 = "SF";
         }
      }
   }
  
   if (surf1==NULL || surf2==NULL) {
      fprintf(SUMA_STDERR, "%s: At least one specfile did not contain a suitable surface.\nCannot continue.\n", FuncName);
      exit(1);
   }

   /**map surf1 to surf2 */
   MI = SUMA_MapSurface( surf1, surf2);
   if (!MI) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Map_SurfacetoSurface.\n", FuncName);
      exit (1);
   }
   mapSurf = SUMA_morphToStd( surf2->NodeList, MI);

   /**read and map color file for surf2, if given*/
   if (color) {
      colSurf2 = SUMA_readColor( surf2->N_Node, surf2colFileNm );
      mapColSurf2 = SUMA_morphToStd( colSurf2, MI);
   }

   /**write surfaces to file*/
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, surfFileNm);
   SUMA_writeFSfile (mapSurf, surf1->FaceSetList, surf1->N_Node, surf1->N_FaceSet, 
                     "#surf1 mapped to surf2 for SUMA_Map_SurfacetoSurface (SUMA_SphericalMapping.c)", surfFileNm);

   if (color) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, colFileNm);
      SUMA_writeColorFile (mapColSurf2, surf1->N_Node, colFileNm);
   }
   
   /**write spec file*/
   surfaces = SUMA_calloc(3, sizeof(SUMA_SpecSurfInfo));

   if ( strcmp(tag1, "FS") == 0 ) {
      strcpy (surfaces[0].type, "FreeSurfer");
      sprintf (surfaces[0].fileToRead, "%s", surf1file);
   }
   else {
      strcpy (surfaces[0].type, "SureFit");
      sprintf (surfaces[0].fileToRead, "%s", surf1file_SF->name_coord);
   }
   strcpy (surfaces[0].format, "ASCII");  strcpy(surfaces[0].mapRef, "SAME");
   strcpy (surfaces[0].state, "origSurf1"); strcpy (surfaces[0].dim, "3");

   strcpy (surfaces[1].format, "ASCII");               strcpy (surfaces[1].type, "FreeSurfer");   
   sprintf (surfaces[1].fileToRead, "%s", surfFileNm); strcpy( surfaces[1].mapRef, "SAME");  
   strcpy (surfaces[1].state, "mappedSurf");         strcpy (surfaces[1].dim, "3");

   if ( strcmp(tag2, "FS") == 0 ) {
      strcpy (surfaces[2].type, "FreeSurfer");
      sprintf (surfaces[2].fileToRead, "%s", surf2file);
   }
   else {
      strcpy (surfaces[2].type, "SureFit");
      sprintf (surfaces[2].fileToRead, "%s", surf2file_SF->name_coord);
   }
   strcpy (surfaces[2].format, "ASCII");  strcpy(surfaces[2].mapRef, "SAME");
   strcpy (surfaces[2].state, "origSurf2"); strcpy (surfaces[2].dim, "3");
  
   SUMA_writeSpecFile ( surfaces, 3, FuncName, fout, outSpecFileNm );
   fprintf (SUMA_STDERR, "\n* To view in SUMA, load spec file %s *\n\n", outSpecFileNm);



   /* free the variables */
   SUMA_Free_MorphInfo (MI);
   SUMA_Free_Surface_Object (surf2);
   SUMA_Free_Surface_Object (surf1);
   if (color) {
      SUMA_free(colSurf2);
   }
   SUMA_free(mapSurf);
   SUMA_free(surfaces);

   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

   exit(0);
  
}/* main SUMA_Map_SurfacetoSurface*/
#endif




#ifdef SUMA_MapIcosahedron_STAND_ALONE

void SUMA_MapIcosahedron_usage ()
   
{/*Usage*/
   printf ( "\n\33[1mUsage: \33[0mMapIcosahedron <-spec specFile> \n"
            "                      [-rd recDepth] [-ld linDepth] \n"
            "                      [-it numIt] [-prefix fout] \n"
            "                      [-verb] [-help]\n");
   printf ( "\nCreates new versions of the original-mesh surfaces using the mesh\n"
            "of an icosahedron. FOR MORE HELP, WRITE ziad@nih.gov\n");
   printf ( "\n\t-spec specFile: spec file containing original-mesh surfaces\n"
            "\t     including the spherical and warped spherical surfaces.\n");
   printf ( "\n\t-rd recDepth: recursive (binary) tesselation depth for icosahedron.\n"
            "\t     (optional, default:3) See CreateIcosahedron for more info.\n");
   printf ( "\n\t-ld linDepth: number of edge divides for linear icosahedron tesselation \n"
            "\t     (optional, default uses binary tesselation).\n"
            "\t     See CreateIcosahedron -help for more info.\n");
   printf ( "\n\t*Note: Enter -1 for recDepth or linDepth to let program \n"
            "\t       choose a depth that best approximates the number of nodes in\n"
            "\t       original-mesh surfaces.\n");
   printf ( "\n\t-it numIt: number of smoothing interations \n"
            "\t     (optional, default none).\n");
   printf ( "\n\t-prefix fout: prefix for output files.\n"
            "\t     (optional, default MapIco)\n");
   printf ( "\n\t[-sph_check]: Run tests for checking the spherical surface (sphere.asc)\n"
            "\t             The program exits after the checks.\n"
            "\t             This option is for debugging FreeSurfer surfaces only.\n");
   printf ( "\n\t[-sphreg_check]: Run tests for checking the spherical surface (sphere.reg.asc)\n"
            "\t             The program exits after the checks.\n"
            "\t             This option is for debugging FreeSurfer surfaces only.\n");
   printf ( "\n\t-sph_check and -sphreg_check are mutually exclusive.\n");
   printf ( "\n\t-verb: When specified, includes original-mesh surfaces \n"
            "\t    and icosahedron in output spec file.\n"
            "\t    (optional, default does not include original-mesh surfaces)\n");
   printf ( "\n\t    Brenna D. Argall LBC/NIMH/NIH bargall@codon.nih.gov \n"
            "\t    Ziad S. Saad     SSC/NIMH/NIH ziad@nih.gov\n"
            "\t\t    Fri Sept 20 2002, Last Modified Thu Oct  2 2003\n\n");
   exit (0);
}/*Usage*/
/*!
  stand alone program to map one surface to another and write mapping to file in FreeSurfer format. 

*/
int main (int argc, char *argv[])
{/* main SUMA_MapIcosahedron */

   static char FuncName[]={"SUMA_MapIcosahedron-main"};
   SUMA_Boolean brk, LocalHead = NOPE, SurfIn = NOPE, color=NOPE, smooth=NOPE, verb=NOPE;
   char fout[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
   char *smwmColFileNm=NULL;
   char icoFileNm[10000], mapSphrFileNm[10000], mapInflFileNm[10000], mapSmWmFileNm[10000]; 
   char mapWhiteFileNm[10000], mapPialFileNm[10000], mapSphrNoRegFileNm[10000];
   char colFileNm[10000], outSpecFileNm[10000], ctrSphrFileNm[10000];
   SUMA_SpecSurfInfo *surfaces=NULL;
   SUMA_MorphInfo *MI=NULL;
   char bin[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
   int numTriBin=0, numTriLin=0, numIt=0;

   int kar, i, j, k, it, depth, i_currSurf;
   float r, ctrX, ctrY, ctrZ, ctr[3];
   SUMA_SurfSpecFile brainSpec;  
   SUMA_SurfaceObject *sphrSurf=NULL, *inflSurf=NULL, *smwmSurf=NULL, *icoSurf=NULL;
   SUMA_SurfaceObject *sphrNoRegSurf=NULL, *whiteSurf=NULL, *pialSurf=NULL;
   char *brainSpecFile=NULL, *sphrFile=NULL, *inflFile=NULL, *smwmFile=NULL, *OutName = NULL;
   char *whiteFile=NULL, *pialFile=NULL, *sphrNoRegFile=NULL, *sphrColFileNm=NULL;
   SUMA_SFname *sphrFile_SF=NULL, *inflFile_SF=NULL, *smwmFile_SF=NULL;       //for surefit surfaces
   SUMA_SFname *whiteFile_SF=NULL, *sphrNoRegFile_SF=NULL, *pialFile_SF=NULL;
   float *mapInflNodeList=NULL, *mapSmWmNodeList=NULL, *mapWhiteNodeList=NULL, *mapSphrList=NULL;
   float *mapSphrNoRegNodeList=NULL, *mapPialNodeList=NULL, *colArray=NULL, *mapCol=NULL;
   float *ctrSphrList=NULL;
   float *smweight=NULL, lambda=0, mu=0, deltaX=0, deltaY=0, deltaZ=0, delta=0;
   struct  timeval start_time;
   float etime_MapSurface;
   SUMA_Boolean CheckSphereReg,CheckSphere;
    
   /* allocate space for CommonFields structure */
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   if (LocalHead) fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);

   /* clueless user ? */
   if (argc < 2) {
      SUMA_MapIcosahedron_usage ();
      exit (1); 
   }
   
   /* read in the options */
   depth = 3;
   sprintf( fout, "%s", "MapIco");
   sprintf( bin, "%s", "y");
   smooth = NOPE;  numIt=0;
   verb = NOPE;
   kar = 1;
   brk = NOPE;
   CheckSphere = NOPE;
   CheckSphereReg = NOPE;

   while (kar < argc) { /* loop accross command line options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         SUMA_MapIcosahedron_usage ();
         exit (1);
      }
            
      if (!brk && (strcmp(argv[kar], "-spec") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -spec \n");
               exit (1);
            }
            brainSpecFile = argv[kar];
            brk = YUP;
         }      
      if (!brk && strcmp(argv[kar], "-c") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -c \n");
               exit (1);
            }
            sphrColFileNm = argv[kar];
            color = YUP;
            brk = YUP;
         }   
      if (!brk && (strcmp(argv[kar], "-rd") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -rd \n");
               exit (1);
            }
            depth = atoi(argv[kar]);
            sprintf (bin, "y");
            brk = YUP;

         }      
      if (!brk && (strcmp(argv[kar], "-ld") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -ld \n");
               exit (1);
            }
            depth = atoi(argv[kar]);
            sprintf (bin, "n");
            brk = YUP;
         }      
      if (!brk && (strcmp(argv[kar], "-it") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -it \n");
               exit (1);
            }
            smooth = YUP;
            numIt = atoi(argv[kar]);
            brk = YUP;
         }      
      if (!brk && (strcmp(argv[kar], "-verb") == 0 ))
         {
            verb = YUP;
            brk = YUP;

         }
      if (!brk && (strcmp(argv[kar], "-sphreg_check") == 0 ))
         {
            if (CheckSphere) {
               fprintf (SUMA_STDERR, "-sphreg_check & -sph_check are mutually exclusive.\n");
               exit (1);
            }
            CheckSphereReg = YUP;
            brk = YUP;

         }      
      if (!brk && (strcmp(argv[kar], "-sph_check") == 0 ))
         {
            if (CheckSphereReg) {
               fprintf (SUMA_STDERR, "-sphreg_check & -sph_check are mutually exclusive.\n");
               exit (1);
            }
            CheckSphere = YUP;
            brk = YUP;

         } 
      if (!brk && strcmp(argv[kar], "-prefix") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -prefix ");
               exit (1);
            }
            sprintf (fout, "%s", argv[kar]);
            brk = YUP;
         }   

      if (!brk) {
         fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } 
      else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command line options */

   /* check for some sanity */
   if (bin[0] == 'y' && depth > 10) {
      fprintf (SUMA_STDERR, "%s: You cannot use a recursive depth > 10.\n", FuncName);
      exit(1);
   }
   if (LocalHead) fprintf (SUMA_STDERR, "%s: %s contains surfaces, tesselation depth is %d.\n", FuncName, brainSpecFile, depth);
   if (brainSpecFile == NULL) {
      fprintf (SUMA_STDERR,"Error %s: No spec file specified.\n", FuncName);
      exit(1);
   }
   if ( !SUMA_Read_SpecFile (brainSpecFile, &brainSpec)) {
      fprintf(SUMA_STDERR,"Error %s: Error in %s SUMA_Read_SpecFile\n", FuncName, brainSpecFile);
      exit(1);
   }

   /* assign output file names */
   sprintf (icoFileNm, "%s_icoSurf.asc", fout);
   sprintf (mapSphrFileNm, "%s_mappedSphr.asc", fout);
   sprintf (ctrSphrFileNm, "%s_origSphr.asc", fout);
   sprintf (mapSphrNoRegFileNm, "%s_mappedSphrNoReg.asc", fout);
   sprintf (mapPialFileNm, "%s_mappedPial.asc", fout);
   sprintf (mapInflFileNm, "%s_mappedInfl.asc", fout);
   sprintf (mapSmWmFileNm, "%s_mappedSmWm.asc", fout);
   sprintf (mapWhiteFileNm, "%s_mappedWhite.asc", fout);
   sprintf (colFileNm, "%s_col.col", fout);
   sprintf (outSpecFileNm, "%s.spec", fout);
     
   if ( SUMA_filexists(icoFileNm) || SUMA_filexists(mapSphrFileNm) || SUMA_filexists(mapInflFileNm) ||
        SUMA_filexists(colFileNm) || SUMA_filexists(mapSmWmFileNm) || SUMA_filexists(outSpecFileNm) ||
        SUMA_filexists(mapWhiteFileNm) || SUMA_filexists(mapSphrNoRegFileNm) || SUMA_filexists(mapPialFileNm)) {
      fprintf (SUMA_STDERR,"Error %s: At least one of output files (prefix %s) exists.\nWill not overwrite.\n", 
               FuncName, fout);
      exit(1);
   }
  
   /** load surfaces */
   if (CheckSphere) {
      fprintf(SUMA_STDERR,"%s:\n:Checking sphere surface only.\n", FuncName);
   }else if (CheckSphereReg) {
      fprintf(SUMA_STDERR,"%s:\n:Checking sphere.reg surface only.\n", FuncName);
   }
   
   for (i=0; i<brainSpec.N_States; ++i) {
    
      /**reg sphere*/
      if (SUMA_iswordin( brainSpec.State[i], "sphere.reg") ==1 && !CheckSphere) {
         if (SUMA_iswordin( brainSpec.SurfaceType[i], "FreeSurfer") == 1) {
            sphrFile = brainSpec.FreeSurferSurface[i];
            sphrSurf = SUMA_Load_Surface_Object( sphrFile, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
         }
         else {
            if (SUMA_iswordin( brainSpec.SurfaceType[i], "SureFit") == 1) {
               sphrFile_SF = (SUMA_SFname*) SUMA_malloc( sizeof(SUMA_SFname));
               strcpy( sphrFile_SF->name_coord, brainSpec.CoordFile[i] );
               strcpy( sphrFile_SF->name_topo, brainSpec.TopoFile[i] );
               strcpy( sphrFile_SF->name_topo, brainSpec.SureFitVolParam[i] );
               sphrSurf = SUMA_Load_Surface_Object( sphrFile_SF, SUMA_SUREFIT, SUMA_ASCII, NULL);
            }
         }
         /* Check on spheriosity of surface */
         if (CheckSphereReg) {
            SUMA_SurfaceMetrics(sphrSurf, "EdgeList, MemberFace", NULL);
            sphrSurf->Label = SUMA_SurfaceFileName(sphrSurf, NOPE);
            OutName = SUMA_append_string (sphrSurf->Label, "_Conv_detail.1D");
            sphrSurf->Cx = SUMA_Convexity_Engine ( sphrSurf->NodeList, sphrSurf->N_Node, 
                                                   sphrSurf->NodeNormList, sphrSurf->FN, OutName);
            if (sphrSurf) SUMA_SphereQuality (sphrSurf, "SphereRegSurf");
            fprintf(SUMA_STDERR, "%s:\nExiting after SUMA_SphereQuality (sphrSurf)\n", FuncName);
            if (sphrSurf) SUMA_Free_Surface_Object (sphrSurf);
            if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
            if (OutName) free(OutName);
            exit(0);
         } else {
            SUMA_SurfaceMetrics(sphrSurf, "EdgeList, MemberFace", NULL);
         }   

      }
      /**sphere*/
      if ( SUMA_iswordin( brainSpec.State[i], "sphere") == 1 &&
           SUMA_iswordin( brainSpec.State[i], "sphere.reg") == 0 && !CheckSphereReg) {
         if (SUMA_iswordin( brainSpec.SurfaceType[i], "FreeSurfer") == 1) {
            sphrNoRegFile = brainSpec.FreeSurferSurface[i];
            sphrNoRegSurf = SUMA_Load_Surface_Object(sphrNoRegFile, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
         }
         else {
            if (SUMA_iswordin( brainSpec.SurfaceType[i], "SureFit") == 1) {
               sphrNoRegFile_SF = (SUMA_SFname *) SUMA_malloc( sizeof(SUMA_SFname));
               strcpy( smwmFile_SF->name_coord, brainSpec.CoordFile[i] );
               strcpy( smwmFile_SF->name_topo, brainSpec.TopoFile[i] );
               strcpy( smwmFile_SF->name_topo, brainSpec.SureFitVolParam[i] );
               sphrNoRegSurf = SUMA_Load_Surface_Object( sphrNoRegFile_SF, SUMA_SUREFIT, SUMA_ASCII, NULL);
            }
         }
         /* Check on spheriosity of surface */
         if (CheckSphere) {
            sphrNoRegSurf->Label = SUMA_SurfaceFileName(sphrNoRegSurf, NOPE);
            SUMA_SurfaceMetrics(sphrNoRegSurf, "EdgeList, MemberFace", NULL);
            OutName = SUMA_append_string (sphrNoRegSurf->Label, "_Conv_detail.1D");
            sphrNoRegSurf->Cx = SUMA_Convexity_Engine (sphrNoRegSurf->NodeList, sphrNoRegSurf->N_Node, 
                                                       sphrNoRegSurf->NodeNormList, sphrNoRegSurf->FN, OutName);
            if (sphrNoRegSurf) SUMA_SphereQuality (sphrNoRegSurf, "SphereSurf");
            fprintf(SUMA_STDERR, "%s:\nExiting after SUMA_SphereQuality (sphrNoRegSurf)\n", FuncName);
            if (sphrNoRegSurf) SUMA_Free_Surface_Object (sphrNoRegSurf);
            if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
            if (OutName) free(OutName);
            exit(0);
         }
      }
      /**inflated*/
      if ((SUMA_iswordin( brainSpec.State[i], "inflated") ==1) && !CheckSphere && !CheckSphereReg) {
         if (SUMA_iswordin( brainSpec.SurfaceType[i], "FreeSurfer") == 1) {
            inflFile = brainSpec.FreeSurferSurface[i];
            inflSurf = SUMA_Load_Surface_Object( inflFile, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
         }
         else {
            if (SUMA_iswordin( brainSpec.SurfaceType[i], "SureFit") == 1) {
               inflFile_SF = (SUMA_SFname*)SUMA_malloc( sizeof(SUMA_SFname));
               strcpy( inflFile_SF->name_coord, brainSpec.CoordFile[i] );
               strcpy( inflFile_SF->name_topo, brainSpec.TopoFile[i] );
               strcpy( inflFile_SF->name_topo, brainSpec.SureFitVolParam[i] );
               inflSurf = SUMA_Load_Surface_Object( inflFile_SF, SUMA_SUREFIT, SUMA_ASCII, NULL);
            }
         }
      }
      
      /**pial*/
      if ((SUMA_iswordin( brainSpec.State[i], "pial") ==1) && !CheckSphere && !CheckSphereReg){
         if (SUMA_iswordin( brainSpec.SurfaceType[i], "FreeSurfer") == 1) {
            pialFile = brainSpec.FreeSurferSurface[i];
            pialSurf = SUMA_Load_Surface_Object(pialFile, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
         }
         else {
            if (SUMA_iswordin( brainSpec.SurfaceType[i], "SureFit") == 1) {
               pialFile_SF = (SUMA_SFname*) SUMA_malloc( sizeof(SUMA_SFname));
               strcpy( smwmFile_SF->name_coord, brainSpec.CoordFile[i] );
               strcpy( smwmFile_SF->name_topo, brainSpec.TopoFile[i] );
               strcpy( smwmFile_SF->name_topo, brainSpec.SureFitVolParam[i] );
               pialSurf = SUMA_Load_Surface_Object( pialFile_SF, SUMA_SUREFIT, SUMA_ASCII, NULL);
            }
         }
      }
      /**smoothwm*/
      if ((SUMA_iswordin( brainSpec.State[i], "smoothwm") ==1) && !CheckSphere && !CheckSphereReg){
         if (SUMA_iswordin( brainSpec.SurfaceType[i], "FreeSurfer") == 1) {
            smwmFile = brainSpec.FreeSurferSurface[i];
            smwmSurf = SUMA_Load_Surface_Object(smwmFile, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
         }
         else {
            if (SUMA_iswordin( brainSpec.SurfaceType[i], "SureFit") == 1) {
               smwmFile_SF = SUMA_malloc( sizeof(SUMA_SFname));
               strcpy( smwmFile_SF->name_coord, brainSpec.CoordFile[i] );
               strcpy( smwmFile_SF->name_topo, brainSpec.TopoFile[i] );
               strcpy( smwmFile_SF->name_topo, brainSpec.SureFitVolParam[i] );
               smwmSurf = SUMA_Load_Surface_Object( smwmFile_SF, SUMA_SUREFIT, SUMA_ASCII, NULL);
            }
         }
      }
      /**white*/
      if ((SUMA_iswordin( brainSpec.State[i], "white") ==1) && !CheckSphere && !CheckSphereReg) {
         if (SUMA_iswordin( brainSpec.SurfaceType[i], "FreeSurfer") == 1) {
            whiteFile = brainSpec.FreeSurferSurface[i];
            whiteSurf = SUMA_Load_Surface_Object(whiteFile, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
         }
         else {
            if (SUMA_iswordin( brainSpec.SurfaceType[i], "SureFit") == 1) {
               whiteFile_SF = (SUMA_SFname*) SUMA_malloc( sizeof(SUMA_SFname));
               strcpy( smwmFile_SF->name_coord, brainSpec.CoordFile[i] );
               strcpy( smwmFile_SF->name_topo, brainSpec.TopoFile[i] );
               strcpy( smwmFile_SF->name_topo, brainSpec.SureFitVolParam[i] );
               whiteSurf = SUMA_Load_Surface_Object( whiteFile_SF, SUMA_SUREFIT, SUMA_ASCII, NULL);
            }
         }
      }
   }

   if ( sphrSurf==NULL ) {
      fprintf(SUMA_STDERR, "Error %s: Sphere.reg brain state missing from Spec file.\nWill not contintue.\n", FuncName);
      exit(1);
   }
   if ( sphrNoRegSurf!=NULL && !(sphrSurf->N_Node == sphrNoRegSurf->N_Node) ) {
      fprintf(SUMA_STDERR, "Error %s: Surfaces differ in node number. Exiting.\n", FuncName);
      exit(1);
   }
   if ( inflSurf!=NULL && !(sphrSurf->N_Node == inflSurf->N_Node) ) {
      fprintf(SUMA_STDERR, "Error %s: Surfaces differ in node number. Exiting.\n", FuncName);
      exit(1);
   }
   if ( pialSurf!=NULL && !(sphrSurf->N_Node == pialSurf->N_Node) ) {
      fprintf(SUMA_STDERR, "Error %s: Surfaces differ in node number. Exiting.\n", FuncName);
      exit(1);
   }
   if ( smwmSurf!=NULL && !(sphrSurf->N_Node == smwmSurf->N_Node) ) {
      fprintf(SUMA_STDERR, "Error %s: Surfaces differ in node number. Exiting.\n", FuncName);
      exit(1);
   }
   if ( whiteSurf!=NULL && !(sphrSurf->N_Node == whiteSurf->N_Node) ) {
      fprintf(SUMA_STDERR, "Error %s: Surfaces differ in node number. Exiting.\n", FuncName);
      exit(1);
   }
   
   /**prepare for writing spec file*/
   if ( verb ) {
      surfaces = (SUMA_SpecSurfInfo *)SUMA_calloc(2*brainSpec.N_States+1, sizeof(SUMA_SpecSurfInfo));

      sprintf (surfaces[0].fileToRead, "%s", icoFileNm);  strcpy (surfaces[0].state, "icosahedron");
      strcpy( surfaces[0].mapRef, "SAME");  strcpy (surfaces[0].format, "ASCII");   
      strcpy (surfaces[0].type, "FreeSurfer");   strcpy (surfaces[0].dim, "3");
      
      sprintf (surfaces[1].fileToRead, "%s", mapSphrFileNm);  strcpy (surfaces[1].state, "mappedSphere.reg");
      sprintf (surfaces[2].fileToRead, "%s", ctrSphrFileNm);  strcpy (surfaces[2].state, "sphere.reg");
      i_currSurf = 2;
   }
   else {
      surfaces = (SUMA_SpecSurfInfo *)SUMA_calloc(brainSpec.N_States, sizeof(SUMA_SpecSurfInfo));
      sprintf (surfaces[0].fileToRead, "%s", mapSphrFileNm);  strcpy (surfaces[0].state, "mappedSphere.reg");
      i_currSurf = 0;
   }      

   /**determine depth such that numTri best approximates (but overestimates) sphrSurf->N_FaceSet? */ 
   if ( depth<0 ) {

      /*closest for recursive*/
      i = 0;  numTriBin = 20;
      while ( numTriBin < sphrSurf->N_FaceSet ) {
         ++i;
         numTriBin = 20*( pow(2,2*i) );
      }

      /*closest for linear*/
      j = 1;  numTriLin = 20;
      while ( numTriLin < sphrSurf->N_FaceSet ) {
         ++j;
         numTriLin = 20*( pow(j,2) );
      }

      if ( fabs(numTriLin-sphrSurf->N_FaceSet) < fabs(numTriBin-sphrSurf->N_FaceSet) ) {
         depth = j;
         sprintf (bin, "n");
      }
      else {
         depth = i;
         sprintf (bin, "y");
      }
   }

   /**determine radius for icosahedron*/ 
   ctrX=0; ctrY=0; ctrZ=0; j=0;
   for (i=0; i<sphrSurf->N_Node; ++i) {
      j = 3*i;
      ctrX = ctrX + sphrSurf->NodeList[j];
      ctrY = ctrY + sphrSurf->NodeList[j+1];
      ctrZ = ctrZ + sphrSurf->NodeList[j+2];
   }
   ctrX = ctrX/(sphrSurf->N_Node);
   ctrY = ctrY/(sphrSurf->N_Node);
   ctrZ = ctrZ/(sphrSurf->N_Node);

   ctr[0] = 0; ctr[1] = 0; ctr[2] = 0;
   r = sqrt( pow( (sphrSurf->NodeList[0]-ctrX), 2) + pow( (sphrSurf->NodeList[1]-ctrY), 2) 
             + pow( (sphrSurf->NodeList[2]-ctrZ), 2) );
  
   /**center sphere to (0,0,0) for writing to file*/
   if ( verb ) {
      ctrSphrList = (float *) SUMA_calloc( 3*(sphrSurf->N_Node), sizeof(float));
      for (i=0; i<sphrSurf->N_Node; ++i) {
         j = 3*i;
         ctrSphrList[j] = sphrSurf->NodeList[j] - ctrX;
         ctrSphrList[j+1] = sphrSurf->NodeList[j+1] - ctrY;
         ctrSphrList[j+2] = sphrSurf->NodeList[j+2] - ctrZ;
      }
   }

   /**create icosahedron*/
   icoSurf = SUMA_CreateIcosahedron (r, depth, ctr, bin);
   if (!icoSurf) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_MapIcosahedron.\n", FuncName);
      exit (1);
   }

   /**determine smoothing weights if indicated*/
   if ( smooth ) {
      /*equal weights => each edge has cost of one, so weight is 1/numEdges*/
      if (icoSurf->FN==NULL) {
         fprintf(SUMA_STDERR, "Warning %s: Node first neighbor list not created; cannot smooth.\n", FuncName);
         smooth = NOPE;
      }
      else {
         smweight = SUMA_calloc( icoSurf->N_Node, sizeof(float));
         for (i=0; i<icoSurf->N_Node; ++i) {
            smweight[i] = 1/(float)(icoSurf->FN->N_Neighb[i]);
         }
         lambda = (float)(.1+sqrt(18.81))/9.4;
         mu = (float)(.1+sqrt(18.81))/(.1*sqrt(18.81)-9.39);
      }
   }


   /**determine morph parameters by mapping icosahedron to spherical brain */
   /* start timer */
   SUMA_etime(&start_time,0);
   MI = SUMA_MapSurface( icoSurf, sphrSurf ) ;
   if (!MI) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_MapIcosahedron.\n", FuncName);
      exit (1);
   }
   etime_MapSurface = SUMA_etime(&start_time,1);
   
   /**morph sphere.reg backwards*/
   mapSphrList = SUMA_morphToStd( sphrSurf->NodeList, MI);

  
   /**morph sphere.reg backwards and write to file*/
   mapSphrList = SUMA_morphToStd( sphrSurf->NodeList, MI);

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, mapSphrFileNm);
   SUMA_writeFSfile (mapSphrList, MI->FaceSetList, MI->N_Node, MI->N_FaceSet, 
                     "#icosahedron mapped to spherical brain for SUMA_MapIcosahedron (SUMA_SphericalMapping.c)", mapSphrFileNm);
 
   /**morph backwards (for surfaces besides sphere.reg) and write to file
      (using weighting from SUMA_MapSurfaces)*/
   
   if (sphrNoRegSurf!=NULL){

      mapSphrNoRegNodeList = SUMA_morphToStd(sphrNoRegSurf->NodeList, MI);
      ++i_currSurf;
      strcpy (surfaces[i_currSurf].state, "mappedSphrNoReg");
      sprintf (surfaces[i_currSurf].fileToRead, "%s", mapSphrNoRegFileNm);
      if ( verb ) {
         ++i_currSurf;
         strcpy (surfaces[i_currSurf].state, "sphere");
         sprintf (surfaces[i_currSurf].fileToRead, "%s", sphrNoRegFile);
      }
      fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, mapSphrNoRegFileNm);
      SUMA_writeFSfile (mapSphrNoRegNodeList, MI->FaceSetList, MI->N_Node, MI->N_FaceSet, 
                        "#standard sphere brain for SUMA_MapIcosahedron (SUMA_SphericalMapping.c)", mapSphrNoRegFileNm);
   }
   if (inflSurf!=NULL) {

      mapInflNodeList = SUMA_morphToStd(inflSurf->NodeList, MI);
      ++i_currSurf;
      strcpy (surfaces[i_currSurf].state, "mappedInflated");
      sprintf (surfaces[i_currSurf].fileToRead, "%s", mapInflFileNm);
      if ( verb ) {
         ++i_currSurf;
         strcpy (surfaces[i_currSurf].state, "inflated");
         sprintf (surfaces[i_currSurf].fileToRead, "%s", inflFile);
      }
      fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, mapInflFileNm);
      SUMA_writeFSfile (mapInflNodeList, MI->FaceSetList, MI->N_Node, MI->N_FaceSet, 
                        "#standard to inflated brain for SUMA_MapIcosahedron (SUMA_SphericalMapping.c)", mapInflFileNm);
   }
   if (pialSurf!=NULL){

      mapPialNodeList = SUMA_morphToStd(pialSurf->NodeList, MI);

      /*smooth pial surface, if indicated*/
      if (smooth) {
         for (it=0; it<2*numIt; ++it) {
            for (i=0; i<icoSurf->N_Node; ++i) {
               j = 3*i;
               deltaX=0;  deltaY=0;  deltaZ=0;
               for ( k=0; k<icoSurf->FN->N_Neighb[i]; ++k) {
                  /*laplacian operator*/
                  deltaX = deltaX + smweight[i]*
                     (mapPialNodeList[ 3*(icoSurf->FN->FirstNeighb[i][k]) ] - mapPialNodeList[j]);
                  deltaY = deltaY + smweight[i]*
                     (mapPialNodeList[ 3*(icoSurf->FN->FirstNeighb[i][k]) +1 ] - mapPialNodeList[j+1]);
                  deltaZ = deltaZ + smweight[i]*
                     (mapPialNodeList[ 3*(icoSurf->FN->FirstNeighb[i][k]) +2 ] - mapPialNodeList[j+2]);
               }
               /*taubin smoothing*/
               if ( it%2 == 0 ) {
                  mapPialNodeList[j] = mapPialNodeList[j] + lambda*deltaX;
                  mapPialNodeList[j+1] = mapPialNodeList[j+1] + lambda*deltaY;
                  mapPialNodeList[j+2] = mapPialNodeList[j+2] + lambda*deltaZ;
               }
               else {
                  mapPialNodeList[j] = mapPialNodeList[j] + mu*deltaX;
                  mapPialNodeList[j+1] = mapPialNodeList[j+1] + mu*deltaY;
                  mapPialNodeList[j+2] = mapPialNodeList[j+2] + mu*deltaZ;
               }
            }
         }
      }

      ++i_currSurf;
      strcpy (surfaces[i_currSurf].state, "mappedPial");
      sprintf (surfaces[i_currSurf].fileToRead, "%s", mapPialFileNm);
      if ( verb ) {
         ++i_currSurf;
         strcpy (surfaces[i_currSurf].state, "pial");
         sprintf (surfaces[i_currSurf].fileToRead, "%s", pialFile);
      }
      fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, mapPialFileNm);
      SUMA_writeFSfile (mapPialNodeList, MI->FaceSetList, MI->N_Node, MI->N_FaceSet, 
                        "#standard pial brain for SUMA_MapIcosahedron (SUMA_SphericalMapping.c)", mapPialFileNm);
   }

   if (smwmSurf!=NULL) {

      mapSmWmNodeList = SUMA_morphToStd(smwmSurf->NodeList, MI);
     
      /*smooth smwm surface, if indicated*/
      if (smooth) {
         for (it=0; it<2*numIt; ++it) {
            for (i=0; i<icoSurf->N_Node; ++i) {
               j = 3*i;
               deltaX=0;  deltaY=0;  deltaZ=0;
               for ( k=0; k<icoSurf->FN->N_Neighb[i]; ++k) {
                  /*laplacian operator*/
                  deltaX = deltaX + smweight[i]*
                     (mapSmWmNodeList[ 3*(icoSurf->FN->FirstNeighb[i][k]) ] - mapSmWmNodeList[j]);
                  deltaY = deltaY + smweight[i]*
                     (mapSmWmNodeList[ 3*(icoSurf->FN->FirstNeighb[i][k]) +1 ] - mapSmWmNodeList[j+1]);
                  deltaZ = deltaZ + smweight[i]*
                     (mapSmWmNodeList[ 3*(icoSurf->FN->FirstNeighb[i][k]) +2 ] - mapSmWmNodeList[j+2]);
               }
               /*taubin smoothing*/
               if ( it%2 == 0 ) {
                  mapSmWmNodeList[j] = mapSmWmNodeList[j] + lambda*deltaX;
                  mapSmWmNodeList[j+1] = mapSmWmNodeList[j+1] + lambda*deltaY;
                  mapSmWmNodeList[j+2] = mapSmWmNodeList[j+2] + lambda*deltaZ;
               }
               else {
                  mapSmWmNodeList[j] = mapSmWmNodeList[j] + mu*deltaX;
                  mapSmWmNodeList[j+1] = mapSmWmNodeList[j+1] + mu*deltaY;
                  mapSmWmNodeList[j+2] = mapSmWmNodeList[j+2] + mu*deltaZ;
               }
            }
         }
      }
      
      ++i_currSurf;
      strcpy (surfaces[i_currSurf].state, "mappedSmWm");
      sprintf (surfaces[i_currSurf].fileToRead, "%s", mapSmWmFileNm);
      if ( verb ) {
         ++i_currSurf;
         strcpy (surfaces[i_currSurf].state, "smoothwm");
         sprintf (surfaces[i_currSurf].fileToRead, "%s", smwmFile);
      }
      fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, mapSmWmFileNm);
      SUMA_writeFSfile (mapSmWmNodeList, MI->FaceSetList, MI->N_Node, MI->N_FaceSet, 
                        "#standard smoothwm brain for SUMA_MapIcosahedron (SUMA_SphericalMapping.c)", mapSmWmFileNm);
   }
   if (whiteSurf!=NULL){

      mapWhiteNodeList = SUMA_morphToStd(whiteSurf->NodeList, MI);
     
      /*smooth white surface, if indicated*/
      if (smooth) {
         for (it=0; it<2*numIt; ++it) {
            for (i=0; i<icoSurf->N_Node; ++i) {
               j = 3*i;
               deltaX=0;  deltaY=0;  deltaZ=0;
               for ( k=0; k<icoSurf->FN->N_Neighb[i]; ++k) {
                  /*laplacian operator*/
                  deltaX = deltaX + smweight[i]*
                     (mapWhiteNodeList[ 3*(icoSurf->FN->FirstNeighb[i][k]) ] - mapWhiteNodeList[j]);
                  deltaY = deltaY + smweight[i]*
                     (mapWhiteNodeList[ 3*(icoSurf->FN->FirstNeighb[i][k]) +1 ] - mapWhiteNodeList[j+1]);
                  deltaZ = deltaZ + smweight[i]*
                     (mapWhiteNodeList[ 3*(icoSurf->FN->FirstNeighb[i][k]) +2 ] - mapWhiteNodeList[j+2]);
               }
               /*taubin smoothing*/
               if ( it%2 == 0 ) {
                  mapWhiteNodeList[j] = mapWhiteNodeList[j] + lambda*deltaX;
                  mapWhiteNodeList[j+1] = mapWhiteNodeList[j+1] + lambda*deltaY;
                  mapWhiteNodeList[j+2] = mapWhiteNodeList[j+2] + lambda*deltaZ;
               }
               else {
                  mapWhiteNodeList[j] = mapWhiteNodeList[j] + mu*deltaX;
                  mapWhiteNodeList[j+1] = mapWhiteNodeList[j+1] + mu*deltaY;
                  mapWhiteNodeList[j+2] = mapWhiteNodeList[j+2] + mu*deltaZ;
               }
            }
         }
      }

      ++i_currSurf;
      strcpy (surfaces[i_currSurf].state, "mappedWhite");
      sprintf (surfaces[i_currSurf].fileToRead, "%s", mapWhiteFileNm);
      if ( verb ) {
         ++i_currSurf;
         strcpy (surfaces[i_currSurf].state, "white");
         sprintf (surfaces[i_currSurf].fileToRead, "%s", whiteFile);
      }
      fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, mapWhiteFileNm);
      SUMA_writeFSfile (mapWhiteNodeList, MI->FaceSetList, MI->N_Node, MI->N_FaceSet, 
                        "#standard white brain for SUMA_MapIcosahedron (SUMA_SphericalMapping.c)", mapWhiteFileNm);
   }


   /**morph colorfile, if given, and write to file*/
   if(color) {
      colArray = SUMA_readColor( sphrSurf->N_Node, sphrColFileNm);
      mapCol = SUMA_morphToStd( colArray, MI);

      fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n ", FuncName, colFileNm);
      SUMA_writeColorFile (mapCol, MI->N_Node, colFileNm);
   }

   /**write icosahedron, sphere.reg to file*/
   if ( verb ) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, icoFileNm);
      SUMA_writeFSfile (icoSurf->NodeList, icoSurf->FaceSetList, icoSurf->N_Node, icoSurf->N_FaceSet, 
                        "#icosahedron for SUMA_MapIcosahedron (SUMA_SphericalMapping.c)", icoFileNm);
      
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, ctrSphrFileNm);
      SUMA_writeFSfile (ctrSphrList, sphrSurf->FaceSetList, sphrSurf->N_Node, sphrSurf->N_FaceSet, 
                     "#centered original sphere for  SUMA_MapIcosahedron (SUMA_SphericalMapping.c)", ctrSphrFileNm);
   }

   /**write spec file*/
   if ( verb ) {

      /* verbose spec file*/
      for( i=1; i<=i_currSurf/2; ++i ) {
         j = 2*i-1;
         /*set mapping reference*/
         if (smwmSurf!=NULL) {
            if ( SUMA_iswordin(surfaces[j].state, "mappedSmWm") == 1 ) {
            strcpy (surfaces[j].mapRef, "SAME");
            strcpy (surfaces[j+1].mapRef, "SAME");
            }
            else {
               strcpy (surfaces[j].mapRef, mapSmWmFileNm);
               strcpy (surfaces[j+1].mapRef, smwmFile);
            }
         }
         else {
            strcpy( surfaces[j].mapRef, "SAME");
            strcpy( surfaces[j+1].mapRef, "SAME");
         }
         /*set all else*/
         strcpy (surfaces[j].format, "ASCII");   strcpy (surfaces[j].type, "FreeSurfer");   
         strcpy (surfaces[j].dim, "3");
         strcpy (surfaces[j+1].format, "ASCII");   strcpy (surfaces[j+1].type, "FreeSurfer");   
         strcpy (surfaces[j+1].dim, "3");
      }
   }
   else {

      /* minimized spec file*/
      for( i=0; i<=i_currSurf; ++i ) {
         /*set mapping reference*/
         if (smwmSurf!=NULL) {
            if ( SUMA_iswordin(surfaces[i].state, "mappedSmWm") == 1 ) {
               strcpy (surfaces[i].mapRef, "SAME");
            }
            else {
               strcpy (surfaces[i].mapRef, mapSmWmFileNm);
            }
         }
         else {
            strcpy( surfaces[i].mapRef, "SAME");
         }
      /*set all else*/
         strcpy (surfaces[i].format, "ASCII");   strcpy (surfaces[i].type, "FreeSurfer");   
         strcpy (surfaces[i].dim, "3");
      }
   }
   
   SUMA_writeSpecFile ( surfaces, i_currSurf+1, FuncName, fout, outSpecFileNm );

   fprintf (SUMA_STDERR, "\nSUMA_MapSurface took %f seconds to execute.\n", etime_MapSurface); 
   fprintf (SUMA_STDERR, "\n* To view in SUMA, load spec file %s *\n\n", outSpecFileNm);


   /* free variables */
   SUMA_Free_MorphInfo (MI);
   /* fprintf(SUMA_STDERR, "poo1\n"); */
   SUMA_free(surfaces);
   /* fprintf(SUMA_STDERR, "poo2\n"); */
   SUMA_free(mapSphrList);
   /* fprintf(SUMA_STDERR, "poo3\n"); */
   if (verb)   SUMA_free(ctrSphrList);
   /* fprintf(SUMA_STDERR, "poo4\n"); */
   SUMA_Free_Surface_Object (icoSurf);
   /* fprintf(SUMA_STDERR, "poo5\n"); */
   SUMA_Free_Surface_Object (sphrSurf);
   /* fprintf(SUMA_STDERR, "poo6\n"); */
   if (smooth) SUMA_free(smweight);
   /* fprintf(SUMA_STDERR, "poo7\n"); */

   if (color) {
      SUMA_free(mapCol);
   }
   /* fprintf(SUMA_STDERR, "poo8\n"); */
   if (inflSurf!=NULL) {
      SUMA_free(mapInflNodeList);
      SUMA_Free_Surface_Object (inflSurf);
   }
   /* fprintf(SUMA_STDERR, "poo9\n"); */
   if (pialSurf!=NULL) {
      SUMA_free(mapPialNodeList);
      SUMA_Free_Surface_Object (pialSurf);
   }
   /* fprintf(SUMA_STDERR, "poo10\n"); */
   if (smwmSurf!=NULL) {
      SUMA_free(mapSmWmNodeList);
      SUMA_Free_Surface_Object (smwmSurf);
   }
   /* fprintf(SUMA_STDERR, "poo11\n"); */
   if (whiteSurf!=NULL) {
      SUMA_free(mapWhiteNodeList);
      SUMA_Free_Surface_Object (whiteSurf);
   }
   /* fprintf(SUMA_STDERR, "poo12\n"); */
   if (sphrNoRegSurf!=NULL) {
      SUMA_free(mapSphrNoRegNodeList);
      SUMA_Free_Surface_Object (sphrNoRegSurf);
   }
   /* fprintf(SUMA_STDERR, "poo13\n"); */
SUMA_Free_Surface_Object (whiteSurf);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

   exit(0);
 
}/* main SUMA_MapIcosahedron*/
#endif

/*!
  array = SUMA_readANOVA1D( numNodes, fileNm, avgArray);

  Function to read a 1D file into an array.
  \param N_Nodes (int) size of created array; if passed as -1, size set to 500000
  \param fileNm (char *) name of 1D file to be read
  \param sig (SUMA_Boolean) YUP -> read significance column; NOPE -> read average column of 1D file
  \ret valArray (float **) 2 x array of 1D file values; [0] is average, [1] is significance

  Written by Brenna Argall
*/
float* SUMA_readANOVA1D (int *N_Node, char* fileNm, SUMA_Boolean sig) {

   float *valArray=NULL, *tmpValArray=NULL;
   FILE *file=NULL;
   char *line=NULL, *temp=NULL;
   int i=0, k=0, index=0, lgth=0;
   static char FuncName[]={"SUMA_readMapDump"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /**if no node amount passed, set to 500,000*/
   if ( *N_Node==-1 )  lgth = 500000;

   tmpValArray = (float *)SUMA_calloc( lgth, sizeof(float) );
   line = (char *) SUMA_calloc( 10000, sizeof(char));
   temp = (char *) SUMA_calloc( 10000, sizeof(char));

   if( (file = fopen(fileNm, "r"))==NULL) {
      fprintf (SUMA_STDERR, "Failed in opening %s for reading.\n", fileNm);
      exit(1);
   }
   else {
      fgets( line, 1000, file);

      /**skip through comments*/
      while( line[0]=='#' ) {
         fgets( line, 10000, file);
      }

      while( !feof(file) ) {

         i = 0;  k = 0;
         while ( !isspace(line[i]) ) ++i;
         while ( isspace(line[i]) ) ++i;
         if (!sig) {
            /*if indicated average, read first column of 1D file into array*/
            while ( !isspace(line[i]) ) { 
               temp[k] = line[i];
               ++i;  ++k;
            }
            tmpValArray[index] = atof(temp);
         }
         else {
            /*if indicated significance, read second column of 1D file into array*/
            while ( !isspace(line[i]) ) ++i;
            while ( isspace(line[i]) ) ++i;
            while ( !isspace(line[i]) ) {  
               temp[k] = line[i];
               ++i;  ++k;
            }
            tmpValArray[index] = atof(temp);
         }
         ++index;
         
         SUMA_free(temp);
         temp = SUMA_calloc(10000, sizeof(char));
         fgets( line, 10000, file ); 
       }
   }
   
   if ( *N_Node!=-1 ) {
      if ( *N_Node!=index ) {
         /*if *N_Node previously known, make sure matches length of file*/
         fprintf(SUMA_STDERR, "%s: File contains more nodes than were indicated.\n", FuncName);
         *N_Node = -1; 
      }
      /**N_Node previously known and correct, pass back array as is*/
      else  {
         valArray = tmpValArray;
      }
   }
   else {
      /*if *N_Node not previously known, create array of exact length and pass back length in *N_Node*/
      valArray = (float*)SUMA_calloc( index, sizeof(float));
      for (i=0; i<index; ++i) {
         valArray[i] = tmpValArray[i];
      }
      *N_Node = index;
   }

   SUMA_free(line);
   SUMA_free(temp);
   SUMA_free(tmpValArray);

   SUMA_RETURN( valArray );
}

/*!
  array = SUMA_readMapDump( numNodes, dumpFileNm);

  Function to read a dumpfile into an array.
  \param N_Nodes (int) size of created array; if passed as -1, size set to 500000
  \param dumpFileNm (char *) name of dump file to be read
  \ret valArray (float *) array of dumpfile v0 values

  Written by Brenna Argall
*/
float* SUMA_readMapDump (int *N_Node, char* dumpFileNm) {

   float *tmpValArray=NULL, *valArray=NULL;
   FILE *dumpFile=NULL;
   char *line=NULL, *temp=NULL;
   int i=0, k=0, index=0, lgth=0;
   static char FuncName[]={"SUMA_readMapDump"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /**if no node amount passed, set to 500,000*/
   if ( *N_Node==-1 )  lgth = 500000;
   
   tmpValArray = (float *) SUMA_calloc( lgth, sizeof(float) );
   line = (char *) SUMA_calloc( 10000, sizeof(char));
   temp = (char *) SUMA_calloc( 10000, sizeof(char));

   if( (dumpFile = fopen(dumpFileNm, "r"))==NULL) {
      fprintf (SUMA_STDERR, "Failed in opening %s for reading.\n", dumpFileNm);
      exit(1);
   }
   else {
      fgets( line, 1000, dumpFile);

      /**skip through comments*/
      while( line[0]=='#' ) {
         fgets( line, 10000, dumpFile);
      }

      while( !feof(dumpFile) ) {

         i = 0;  k = 0;
         while ( isspace(line[i]) ) ++i;
         while ( isdigit(line[i]) ) ++i;  //node
         while ( isspace(line[i]) ) ++i;
         while ( isdigit(line[i]) ) ++i;  //1dindex
         while ( isspace(line[i]) ) ++i;
         while ( isdigit(line[i]) ) ++i;  //i
         while ( isspace(line[i]) ) ++i;
         while ( isdigit(line[i]) ) ++i;  //j
         while ( isspace(line[i]) ) ++i;
         while ( isdigit(line[i]) ) ++i;  //k
         while ( isspace(line[i]) ) ++i;
     
         k = 0;
         while ( !isspace(line[i]) ) {    //v0
            temp[k] = line[i];
            ++i;  ++k;
         }
         tmpValArray[index] = atof(temp);
         ++index;

         SUMA_free(temp);
         temp = SUMA_calloc(10000, sizeof(char));
         fgets( line, 10000, dumpFile ); 
       }
   }
   
   if ( *N_Node!=-1 ) {
      if ( *N_Node!=index ) {
         /*if *N_Node previously known, make sure matches length of file*/
         fprintf(SUMA_STDERR, "%s: File contains more nodes than were indicated.\n", FuncName);
         *N_Node = -1; 
      }
      /**N_Node previously known and correct, pass back array as is*/
      else valArray = tmpValArray;
   }
   else {
      /*if *N_Node not previously known, create array of exact length and pass back length in *N_Node*/
      valArray = SUMA_calloc( index, sizeof(float));
      for (i=0; i<index; ++i) {
         valArray[i] = tmpValArray[i];
      }
      *N_Node = index;
   }

   SUMA_free(line);
   SUMA_free(temp);
   SUMA_free(tmpValArray);

   SUMA_RETURN( valArray );
}

#ifdef SUMA_AverageMaps_STAND_ALONE

void SUMA_AverageMaps_usage ()
   
{/*Usage*/
   printf ("\n\33[1mUsage: \33[0m SUMA_AverageMaps <-num numMap> <-maps mapFile> [-sgm sgmapFile] [-cut cutoff] [-trans] [-vr valRange] [-cr colRange] [-div numDiv] [-anova whichVal] [-prefix fout]\n");
   printf ("\n\tnumMap: number of mapFiles.\n");
   printf ("\n\tmapFile: file names containing maps.\n");
   printf ("\n\tsgmapFile: file name of colorfile containing sulci/gyri maps.\n\t  (optional; without effect if no cuttoff and trans off)\n");
   printf ("\n\tcutoff: value range between which activations are not displayed.\n\t  (optional; without effect if no sgmapFile)\n");
   printf ("\n\t-trans: indicates whether activation transparent.\n\t  (optional; takes no arguments; default 'off')\n\t  ('on' without effective with no sgmapFile)\n");
   printf ("\n\tvalRange: range of activation values displayed (optional).\n\t  (values outside of range take on colors of endpoints)\n");
   printf ("\t  Note: input low and then high (for example, -vr 2 10).\n");
   printf ("\n\tcolRange: specifies roygb color range (optional).\n");
   printf ("\t  Note: input 'r' 'y' 'g' or 'b'; input low and then high.\n");
   printf ("\t  Example: -cr r y has range of red->yellow, with yellow being high.\n");
   printf ("\n\tnumDiv: number of discrete divisions of color range (optional).\n\t  Note: color range will not be continuous.\n");
   printf ("\n\twhichVal: indicates whether average or significance wanted from 1D file from 3dANOVA \n\t  (optional, though necessary to distinguish from 1D dump files).\n\t  Takes 's' (significance) or 'a' (average) only as arguments.\n");
   printf ("\n\tfout: prefix for output files.\n\t  (optional, default AvgMap)\n");
   printf ("\n\t    Brenna D. Argall LBC/NIMH/NIH bargall@codon.nih.gov \n\t\t   Mon February 24 146:23:42 EST 2003\n\n");
   exit (0);
}/*Usage*/
/*!
  stand alone program to average activation maps and output in ascii format. 

*/
int main (int argc, char *argv[])
{/* main SUMA_AverageMaps */

   static char FuncName[]={"SUMA_AverageMaps-main"};
   int numMap, numDiv, colRng[2];
   float cutoff[2], valRng[2];
   SUMA_FileName *mapFiles=NULL, sgmapFile;
   char fout[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
   char colFileNm[1000];
   char *input;
   SUMA_Boolean brk, LocalHead = NOPE, sgmap, cut, trans, cr, vr, div, anova, sig, avg;

   int kar, i, j, k, numVal, tmpNumVal, numArrayRng;
   int numCol, *colUsed=NULL;
   float *tempValArray=NULL, *valArray=NULL, *origValArray=NULL, *sgArray=NULL, *colArray=NULL;
   float high, low, arrayRng[2], partition, pooMin, pooMax, temp;
   float divColRng[2][3], sizeValDiv, sizeColDiv[3];
   float color[4][3], *colIncr=NULL, *colSeg=NULL;
   float incR, incG, incB, avgR, avgG, avgB;
   SUMA_COLOR_MAP *colMap=NULL;
   SUMA_SCALE_TO_MAP_OPT *optInit=NULL;
   SUMA_COLOR_SCALED_VECT *colsv=NULL;
   float minVal=0, maxVal=0;

   /* allocate space for CommonFields structure */
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   if (LocalHead) fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);


   /* clueless user ? */
   if (argc < 2) {
      SUMA_AverageMaps_usage ();
      exit (1); 
   }
   
   /* read in the options */
   sprintf( fout, "%s", "AvgMap");
   numMap = -1;
   sgmap = NOPE;
   cut = NOPE;
   trans = NOPE;
   cr = NOPE; vr = NOPE;
   div = NOPE;  numDiv=0;
   colRng[0]=-1; colRng[1]=-1;
   anova = NOPE;  sig = NOPE;  avg = NOPE;
   kar = 1;
   brk = NOPE;
   while (kar < argc) { /* loop accross command line options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         SUMA_AverageMaps_usage ();
         exit (1);
      }
            
      if (!brk && (strcmp(argv[kar], "-num") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -num ");
               exit (1);
            }
            numMap = atoi(argv[kar]);
            mapFiles = (SUMA_FileName *)SUMA_calloc(numMap, sizeof(SUMA_FileName));
            brk = YUP;
         }      
      if (!brk && (strcmp(argv[kar], "-maps") == 0 ))
         {
            if ( numMap == -1 ) {
               fprintf(SUMA_STDERR, "Error %s: -num argument must be given before -maps arguements.\n", FuncName);
               exit(1);
            }
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -maps ");
               exit (1);
            }

            for (i=0; i<numMap; ++i) {
               mapFiles[i].FileName = argv[kar];
               kar ++;
            }
            kar --;
            brk = YUP;
         }      
      if (!brk && strcmp(argv[kar], "-sgm") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -sgm ");
               exit (1);
            }
            sgmap = YUP;
            sgmapFile.FileName = argv[kar];
            brk = YUP;
         }   
      if (!brk && strcmp(argv[kar], "-cut") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -cut ");
               exit (1);
            }
            cut = YUP;
            cutoff[0] = atof(argv[kar]);
            kar++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need two arguments after -cut ");
               exit (1);
            }
            cutoff[1] = atof(argv[kar]);
            if (cutoff[1]<cutoff[0]) {
               /*make sure low is [0]*/
               temp = cutoff[0];
               cutoff[0] = cutoff[1];
               cutoff[1] = temp;
            }
            brk = YUP;
            
         }   
      if (!brk && strcmp(argv[kar], "-trans") == 0)
         {
            trans = YUP;
            brk = YUP;
         }   
      if (!brk && strcmp(argv[kar], "-vr") == 0)
         {
            vr = YUP;
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -vr ");
               exit (1);
            }
            valRng[0] = atof(argv[kar]);
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need two arguments after -vr ");
               exit (1);
            }
            valRng[1] = atof(argv[kar]);
            if (valRng[1]<valRng[0]) {
               /*make sure low is in [0]*/
               temp = valRng[0];
               valRng[0] = valRng[1];
               valRng[1] = temp;
            }
            brk = YUP;
         }   
      if (!brk && strcmp(argv[kar], "-cr") == 0)
         {
            cr = YUP;
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -cr ");
               exit (1);
            }
            input = argv[kar];
            if (input[0]=='r') colRng[0]=3;
            if (input[0]=='y') colRng[0]=2;
            if (input[0]=='g') colRng[0]=1;
            if (input[0]=='b') colRng[0]=0;

            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need tow arguments after -cr ");
               exit (1);
            }
            input = argv[kar];
            if (input[0]=='r') colRng[1]=3;
            if (input[0]=='y') colRng[1]=2;
            if (input[0]=='g') colRng[1]=1;
            if (input[0]=='b') colRng[1]=0;
            brk = YUP;
         }
      if (!brk && strcmp(argv[kar], "-div") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -div ");
               exit (1);
            }
            div = YUP;
            numDiv = atoi(argv[kar]);
            brk = YUP;
         }   
      if (!brk && strcmp(argv[kar], "-anova") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -anova ");
               exit (1);
            }
            input = argv[kar];
            if (input[0]=='a') avg = YUP;
            else if (input[0]=='s') sig = YUP;
            else {
               fprintf (SUMA_STDERR, "Incorrect argument for -anova (takes only 'a' or 's').\n");
               exit (1);
            }
            anova = YUP;
            brk = YUP;
         }   
      if (!brk && strcmp(argv[kar], "-prefix") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -prefix ");
               exit (1);
            }
            sprintf (fout, "%s", argv[kar]);
            brk = YUP;
         }   
   

      if (!brk) {
         fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } 
      else {   
         brk = NOPE;
         kar ++;
      }
   }

   /**print input / check for dumb input*/
   sprintf( colFileNm, "%s.col", fout);
   if ( SUMA_filexists(colFileNm) ) {
      fprintf (SUMA_STDERR,"Error %s: Output files %s exists.\nWill not overwrite.\n", FuncName, colFileNm);
      exit(1);
   }
   fprintf(SUMA_STDERR, "\n");
   for (i=0; i<numMap; ++i) {
      j=0;
      fprintf(SUMA_STDERR, "File %d: ", i);
      while ( mapFiles[i].FileName[j]!=NULL) {
         fprintf(SUMA_STDERR, "%c", mapFiles[i].FileName[j]);
         j++;
      }
      fprintf(SUMA_STDERR, "\n");
   }
   if (sgmap) {
      j=0;
      fprintf(SUMA_STDERR, "Sulci/gyri map: ");
      while ( sgmapFile.FileName[j]!=NULL ) {
         fprintf(SUMA_STDERR, "%c", sgmapFile.FileName[j]);
         j++;
      }
      fprintf(SUMA_STDERR, "\n");
   }
   if (cut) fprintf(SUMA_STDERR, "Cutoff: %f->%f\n", cutoff[0], cutoff[1]);
   if (cr) {
      if ( colRng[1]==-1 || colRng[0]==-1 ) {
         fprintf(SUMA_STDERR, "\nError %s: Color range input invalid.  Exiting.\n", FuncName);
         exit (1);
      }
   }
   if (vr) {
      fprintf(SUMA_STDERR, "ValRange: %f->%f \n", valRng[0], valRng[1]);
      if (valRng[0]>valRng[1]) {
         fprintf(SUMA_STDERR, "\nError %s: High and low values overlap. Exiting.\n", FuncName);
         exit(1);
      }
   }
   if (vr && cut) if ( (valRng[1] < cutoff[0]) || (valRng[0]>cutoff[1]))  fprintf(SUMA_STDERR, "\nWarning %s: ValRng (%f->%f) does not intersect cutoff range(%f->%f). Output will be one color.\n", FuncName, valRng[0], valRng[1], cutoff[0], cutoff[1]); 
   if (div) {
      if (!vr || !cr) {
         fprintf(SUMA_STDERR, "\nError %s: Use of -div requires -cr and -vr input. Exiting.\n", FuncName);
         exit(1);
      }
      fprintf(SUMA_STDERR, "%d divisions of color range (%d segments)\n", numDiv, numDiv+1);
   }


   /**if 1D file from ANOVA, average already computed, so just read*/
   if (anova) {
      numVal = -1;
      high = -1000.0; low = 1000.0;
      valArray = SUMA_readANOVA1D( &numVal, mapFiles[0].FileName, sig);
      for (i=0; i<numVal; ++i) {
         if ( high<valArray[i] ) high = valArray[i];
         if ( low>valArray[i] ) low = valArray[i];
      }
   }

   /**otherwise read files and average*/
   else {
      /**read first file into valArray*/
      numVal = -1;  tmpNumVal = -1;
      tempValArray = SUMA_readMapDump( &numVal, mapFiles[0].FileName );
      valArray = SUMA_calloc( numVal, sizeof(float)); 
      high = -1000.0; low = 1000.0;
     
      for (i=0; i<numVal; ++i) {
         valArray[i] = tempValArray[i];
         if (high<tempValArray[i]) high = tempValArray[i];
         if (low>tempValArray[i]) low = tempValArray[i];
      }
      fprintf(SUMA_STDERR, "\nfile0: low %f, high %f\n", low, high);
      SUMA_free(tempValArray);
      
      /**read remaining files into valArray*/
      for (i=1; i<numMap; ++i) {
         
         tmpNumVal = -1;
         tempValArray = SUMA_readMapDump( &tmpNumVal, mapFiles[i].FileName );
         high = 0.0;  
         low = 100.0;
         if ( tmpNumVal != numVal ) {
            fprintf(SUMA_STDERR, "Error %s: Input files do not contain the same number of nodes. Exiting.\n", FuncName);
            exit(1);
         }
         else {
            for (k=0; k<numVal; ++k) {
               valArray[k] = valArray[k] + tempValArray[k];
               if (high<tempValArray[k]) high = tempValArray[k];
               if (low>tempValArray[k]) low = tempValArray[k];
            }
         }
         fprintf(SUMA_STDERR, "file%d: low %f, high %f\n", i, low, high);
         SUMA_free(tempValArray);
      }
   }

   /**process valArray*/
   minVal = (float)valArray[0]/(float)numMap;  
   maxVal = -minVal;
   origValArray = SUMA_calloc( numVal, sizeof(float));

   for (i=0; i<numVal; ++i) {
      valArray[i] = (float)valArray[i] / (float)numMap;
      origValArray[i] = valArray[i];
      if ( minVal>valArray[i] ) minVal = valArray[i];
      else if ( maxVal<valArray[i] ) maxVal = valArray[i];

      if (vr) {
         if (valArray[i] > valRng[1]) valArray[i] = valRng[1];
         if (valArray[i] < valRng[0])  valArray[i] = valRng[0];
      }
   }
   fprintf(SUMA_STDERR, "Value Range: low %f, high %f (midpoint %f)\n", minVal, maxVal, (maxVal+minVal)/2.0);


   /*assign high and low of value range to user input or actual data*/
   if (vr) {
      arrayRng[0] = valRng[0];
      arrayRng[1] = valRng[1];
   }
   else {
      arrayRng[0] = minVal;
      arrayRng[1] = maxVal;
      valRng[0] = minVal;
      valRng[1] = maxVal;
   }
   numArrayRng = numVal;

   /**scale valArray to color*/
   colArray = SUMA_calloc( 3*numVal, sizeof(float));
   numCol = 0; 
   partition = 0;  temp = 0;
   sizeValDiv = 0;

   if (cr) {
      numCol = abs( colRng[1]-colRng[0] ) + 1;
      
      if (!div) {
         /*extend range of values to accomodate reduced color range*/
         partition = (valRng[1]-valRng[0])/(float)numCol;
         if ( colRng[0]<colRng[1] ) {
            /*color order b->r (red is high)*/
            arrayRng[0] = valRng[0] - colRng[0]*(partition);
            arrayRng[1] = valRng[1] + (3-colRng[1])*(partition);
            numArrayRng = numVal + (numVal/numCol)*(colRng[0] + (3-colRng[1]));
            fprintf(SUMA_STDERR, "arrayRng: %f->%f, numArray=%d\n", arrayRng[0], arrayRng[1], numArrayRng);
         }
         else {
            /*color order r->b (blue is high) => flip ordering of valArray*/
            for (i=0; i<numVal; ++i)  { 
               valArray[i] = -1*valArray[i];
            }
            temp = arrayRng[0];
            arrayRng[0] = -1*arrayRng[1];
            arrayRng[1] = -1*temp;
            arrayRng[0] = arrayRng[0] - colRng[1]*(partition);
            arrayRng[1] = arrayRng[1] + (3-colRng[0])*(partition);
            numArrayRng = numVal + (numVal/numCol)*(colRng[1] + (3-colRng[0]));
         }
      }
      else {
         /*if number of divisions specified, create own discretized color gradient*/
         
         /*rbg values for each color*/
         color[3][0]=1;  color[3][1]=0;    color[3][2]=0;  //red
         color[2][0]=1;  color[2][1]=.75;  color[2][2]=0;  //yellow
         color[1][0]=0;  color[1][1]=1;    color[1][2]=0;  //green
         color[0][0]=0;  color[0][1]=0;    color[0][2]=1;  //blue

         /*if only one color in range, set segment to it*/
         if (numCol==1) {
            colUsed = SUMA_calloc(1, sizeof(int));
            colUsed[0] = colRng[0];
            colSeg = SUMA_calloc( 9, sizeof(float));
            for (i=0; i<3; ++i) {
               j = 3*i;
               colSeg[j] = color[colUsed[0]][0];
               colSeg[j+1] = color[colUsed[0]][1];
               colSeg[j+2] = color[colUsed[0]][2];
            }
         }
         
         else {

            /*determine all colors within range*/
            colUsed = SUMA_calloc(numCol, sizeof(int));
            colUsed[0] = colRng[0];
            colUsed[numCol-1] = colRng[1];

            for (i=1; i<numCol-1; ++i) {
               if ( colUsed[0]<colUsed[numCol-1] ) colUsed[i] = colUsed[i-1] + 1;
               else colUsed[i] = colUsed[i-1] - 1;
            }
            
            /*create 1D array of rgb (numDiv-1) increments within color, to determine larger incremental
              changes for segments
              # of incremental segments = # of final segments - 2 (without the end segments) = numDiv-1*/
            colIncr = SUMA_calloc( 3*(numCol-1)*(numDiv), sizeof(float));
            for (i=0; i<numCol-1; ++i) {
               /*divide each color by the number of incremental segments*/
               incR = (color[colUsed[i+1]][0] - color[colUsed[i]][0]) / (float)(numDiv);
               incG = (color[colUsed[i+1]][1] - color[colUsed[i]][1]) / (float)(numDiv);
               incB = (color[colUsed[i+1]][2] - color[colUsed[i]][2]) / (float)(numDiv);
               
               /*set numDiv-1 (number of incremental segments) slots in colIncr to amount of color divisions*/
               for (j=0; j<numDiv; ++j) {
                  k = 3*(j + i*(numDiv));
                  colIncr[k] = incR;
                  colIncr[k+1] = incG;
                  colIncr[k+2] = incB;
               }
            }
            
            /*average groups (of size numDiv+1) of increments from colIncr to find color for each final segment*/
            /*colSeg array is 1D with 3 x number of segments entered, in order RGB*/
            colSeg = SUMA_calloc( 3*(numDiv+1), sizeof(float));
            
            /*first segment*/
            colSeg[0] = color[colUsed[0]][0];
            colSeg[1] = color[colUsed[0]][1];
            colSeg[2] = color[colUsed[0]][2];

            /*remaining (incremental and last) segments*/
            for (i=1; i<numDiv+1; ++i) {
               avgR = 0.0;  avgG = 0.0;  avgB = 0.0; 
               for (j=0; j<numCol-1; ++j) {
                  /*sum numCol-1 increments to get incremental value for incremental segment i*/
                  k = 3*(j + (i-1)*(numCol-1));
                  avgR = avgR + colIncr[k];
                  avgG = avgG + colIncr[k+1];
                  avgB = avgB + colIncr[k+2];
               }

               /*add incremental value to color value of prior segment*/
               colSeg[3*i] = colSeg[3*(i-1)] + avgR;
               colSeg[3*i+1] = colSeg[3*(i-1)+1] + avgG;
               colSeg[3*i+2] = colSeg[3*(i-1)+2] + avgB;
            }
            
            /*determine size of discrete increments*/
            sizeValDiv = (valRng[1]-valRng[0]) / (float)(numDiv-1);
         }
      }
   }
   else {
      if (!div) {
         /*if gradient but no color range, set colRng from blue->red*/
         colRng[0] = 0;
         colRng[1] = 3;
      }
   }
   
   /**put colors into 1D array*/
   if (!div) {
      /*continuous colors*/
      optInit = SUMA_ScaleToMapOptInit();
      colMap = SUMA_GetStandardMap( SUMA_CMAP_BGYR19 );
      colsv = SUMA_Create_ColorScaledVect( numArrayRng );
      SUMA_ScaleToMap( valArray, numVal, arrayRng[0], arrayRng[1], colMap, optInit, colsv);
      
      for (i=0; i<numVal; ++i) {
         j=3*i;
         colArray[j] = colsv->cM[i][0];
         colArray[j+1] = colsv->cM[i][1];
         colArray[j+2] = colsv->cM[i][2];
      }
   }
   else {
      /*discrete colors*/
      for (i=0; i<numVal; ++i) {
         j=3*i;
         if ( valArray[i]<=valRng[0] ) {
            /*value out of range => set to minimum color*/
            colArray[j] = colSeg[0];
            colArray[j+1] = colSeg[1];
            colArray[j+2] = colSeg[2];
         }
         else if ( valArray[i]>=valRng[1] ) {
            /*value out of range => set to maximum color*/
            colArray[j] = colSeg[3*numDiv];
            colArray[j+1] = colSeg[3*numDiv + 1];
            colArray[j+2] = colSeg[3*numDiv + 2];
         }
         else {
            /*value falls within range of discrete color block*/
            for (k=1; k<numDiv; ++k) {
               if ( valArray[i] < (valRng[0]+k*sizeValDiv) ) {
                  colArray[j] =  colSeg[3*k];
                  colArray[j+1] = colSeg[3*k+1];
                  colArray[j+2] = colSeg[3*k+2];
                  break;
               }
            }
         }
      }
   }

   /**read and apply sulci/gyri map if indicated*/
   if (sgmap) {
      sgArray = SUMA_readColor( numVal, sgmapFile.FileName);
    
      for (i=0; i<numVal; ++i) {
         j=3*i;
         if (trans) {
            /*overlay with sgmap*/
            colArray[j] = colArray[j] + sgArray[j];
            colArray[j+1] = colArray[j+1] + sgArray[j+1];
            colArray[j+2] = colArray[j+2] + sgArray[j+2];
         }
         if (cut) {
            /*eliminate minimum values if indicated*/
            if ( (origValArray[i]>cutoff[0]) && (origValArray[i]<cutoff[1]) ) {
               colArray[j] = sgArray[j];
               colArray[j+1] = sgArray[j+1];
               colArray[j+2] = sgArray[j+2];
            }
         }
      }
   }

   /**print color ranges to screen*/
   fprintf(SUMA_STDERR, "\nCOLOR RANGES:\n\t[%f : %f]\t", minVal, valRng[0] );
   if (colRng[0]==3) fprintf(SUMA_STDERR, "Red\n");
   else if (colRng[0]==2) fprintf(SUMA_STDERR, "Yellow\n");
   else if (colRng[0]==1) fprintf(SUMA_STDERR, "Green\n");
   else if (colRng[0]==0) fprintf(SUMA_STDERR, "Blue\n");
   if (div) {
      fprintf(SUMA_STDERR, "\t(%f : %f)\n", valRng[0], valRng[0]+sizeValDiv );
      for (k=2; k<numDiv; ++k) {
         fprintf(SUMA_STDERR, "\t[%f : %f)\n", valRng[0]+(k-1)*sizeValDiv, valRng[0]+k*sizeValDiv );
      }
   }
   else fprintf(SUMA_STDERR, "\t(%f : %f)\t(gradient)\n", valRng[0], valRng[1]);
   fprintf(SUMA_STDERR, "\t[%f : %f]\t", valRng[1], maxVal );
   if (colRng[1]==3) fprintf(SUMA_STDERR, "Red\n");
   else if (colRng[1]==2) fprintf(SUMA_STDERR, "Yellow\n");
   else if (colRng[1]==1) fprintf(SUMA_STDERR, "Green\n");
   else if (colRng[1]==0) fprintf(SUMA_STDERR, "Blue\n");
   

   /**write to colorfile*/
   fprintf (SUMA_STDERR, "\n%s: Now writing %s to disk ...\n", FuncName, colFileNm);
   SUMA_writeColorFile (colArray, numVal, colFileNm);

   /**free variable*/
   SUMA_free(mapFiles);
   SUMA_free(valArray);
   SUMA_free(origValArray);
   SUMA_free(colArray);
   if (div) {
      SUMA_free(colUsed);
      SUMA_free(colIncr);
      SUMA_free(colSeg);
   }
   if (!div) SUMA_free(colsv);
   if (sgmap) SUMA_free(sgArray);
   /* fprintf(SUMA_STDERR, "freepoo4\n");*/

   exit(0);
  
}/* main SUMA_AverageMaps*/
#endif
