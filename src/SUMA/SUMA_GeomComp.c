#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_DO *SUMAg_DOv;
extern int SUMAg_N_DOv;

/*!
   Given a set of node indices, return a patch of the original surface that contains them
   
   Patch = SUMA_getPatch (NodesSelected, N_Nodes, Full_FaceSetList, N_Full_FaceSetList, Memb)

   \param NodesSelected (int *) N_Nodes x 1 Vector containing indices of selected nodes. 
            These are indices into NodeList making up the surface formed by Full_FaceSetList.
   \param N_Nodes (int) number of elements in NodesSelected
   \param Full_FaceSetList (int *) N_Full_FaceSetList  x 3 vector containing the triangles forming the surface 
   \param N_Full_FaceSetList (int) number of triangular facesets forming the surface
   \param Memb (SUMA_MEMBER_FACE_SETS *) structure containing the node membership information (result of SUMA_MemberFaceSets function)

   \ret Patch (SUMA_PATCH *) Structure containing the patch's FaceSetList, FaceSetIndex (into original surface) and number of elements.
         returns NULL in case of trouble.   Free Patch with SUMA_freePatch(Patch);

   \sa SUMA_MemberFaceSets, SUMA_isinbox, SUMA_PATCH
*/

SUMA_PATCH * SUMA_getPatch (int *NodesSelected, int N_Nodes, int *Full_FaceSetList, int N_Full_FaceSetList, SUMA_MEMBER_FACE_SETS *Memb)
{
   int * BeenSelected;
   int i, j, node, ip, ip2, NP;
   SUMA_PATCH *Patch;
    static char FuncName[]={"SUMA_getPatch"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   NP = 3;
   BeenSelected = (int *)SUMA_calloc (N_Full_FaceSetList, sizeof(int));
   Patch = (SUMA_PATCH *)SUMA_malloc(sizeof(Patch));
   
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
            BeenSelected[Memb->NodeMemberOfFaceSet[node][j]] = 1;
            ++ Patch->N_FaceSet;
         }
      }   
   }
   
   /* now load these facesets into a new matrix */
   
   Patch->FaceSetList = (int *) SUMA_calloc (Patch->N_FaceSet * 3, sizeof(int));
   Patch->FaceSetIndex = (int *) SUMA_calloc (Patch->N_FaceSet, sizeof(int));
   
   if (!Patch->FaceSetList || !Patch->FaceSetIndex) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for Patch->FaceSetList || Patch_FaceSetIndex.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   j=0;
   for (i=0; i < N_Full_FaceSetList; ++i) {
      if (BeenSelected[i]) {
         Patch->FaceSetIndex[j] = i;
         ip = NP * j;
         ip2 = NP * i;
         Patch->FaceSetList[ip] = Full_FaceSetList[ip2];
         Patch->FaceSetList[ip+1] = Full_FaceSetList[ip2+1];
         Patch->FaceSetList[ip+2] = Full_FaceSetList[ip2+2];
         ++j;
      }
   }
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   
   if (Patch->FaceSetIndex) SUMA_free(Patch->FaceSetIndex);
   if (Patch->FaceSetList) SUMA_free(Patch->FaceSetList);
   if (Patch) SUMA_free(Patch);
   SUMA_RETURN(YUP);
   
}

/*!
 
From File : Plane_Equation.c
Author : Ziad Saad
Date : Thu Nov 19 14:55:54 CST 1998
 
\brief   finds the plane passing through 3 points
 
 
Usage : 
      Eq = SUMA_Plane_Equation (P1, P2, P3)
 
 
   \param P1 (float *) 1x3 vector Coordinates of Point1
   \param P2 (float *) 1x3 vector Coordinates of Point2
   \param P3 (float *) 1x3 vector Coordinates of Point3
   \return Eq (float *) 1x4 vector containing the equation of the plane containing the three points.
         The equation of the plane is : 
         Eq[0] X + Eq[1] Y + Eq[2] Z + Eq[3] = 0
   
         If the three points are colinear, Eq = [0 0 0 0]
 
*/
float * SUMA_Plane_Equation (float * P1, float *P2, float *P3)
{/*SUMA_Plane_Equation*/
   float *Eq;
   static char FuncName[] = {"SUMA_Plane_Equation"}; 
    
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   Eq = (float *) SUMA_calloc(4,sizeof(float));
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
         if (SUMA_IS_NEG(NodePos[SO->EL->EL[k][0]] * NodePos[SO->EL->EL[k][1]])) {
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* computing plane's equation */
   Eq = SUMA_Plane_Equation ( &(SO->NodeList[3*Nfrom]), 
                              P,
                              &(SO->NodeList[3*Nto]) );
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);


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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
  
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   /* if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);    */
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   SUMA_Boolean LocalHead = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
 
 
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* find the edge containing the 1st 2 nodes of the Dijkstra path */
   /* find the triangle that contains the edge formed by the 1st 2 nodes of the Dijkstra path and is intersected by the plane*/
   Tri1 = -1;
   if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: Looking for a triangle containing nodes [%d %d].\n", FuncName, nPath[0], nPath[1]);
   }
   
   Found = SUMA_Get_Incident(nPath[0], nPath[1], SO->EL, Incident, &N_Incident);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   if (!SUMA_Get_Incident(EL->EL[E2][0], EL->EL[E2][1], EL, Incident, &N_Incident)) {
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
                           MissTri = SUMA_whichTri (SO->EL, nc[0], N1[0], N2[0]);
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: looking for triangle with nodes %d and %d... Tri = %d\n", 
                                 FuncName, N1[0], N2[0], MissTri);
                           break;
                        case 1:
                           MissTri = SUMA_whichTri (SO->EL, nc[0], N1[0], N2[1]);
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: looking for triangle with nodes %d and %d... Tri = %d\n", 
                                 FuncName, N1[0], N2[1], MissTri);
                           break;
                        case 2:
                           MissTri = SUMA_whichTri (SO->EL, nc[0], N1[1], N2[0]);
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: looking for triangle with nodes %d and %d... Tri = %d\n", 
                                 FuncName, N1[1], N2[0], MissTri);
                           break;
                        case 3:
                           MissTri = SUMA_whichTri (SO->EL, nc[0], N1[1], N2[1]);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
 
 
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

                     if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
                     SUMA_Boolean LocalHead = YUP;

                     if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
