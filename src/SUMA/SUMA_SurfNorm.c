/*#define TEST*/
/*#define DEBUG_3*/
#ifdef DEBUG_1
   #define DEBUG_2
   #define DEBUG_3
#endif
   
/* Header FILES */
   
#include "SUMA_suma.h"

#ifdef SUMA_SurfNorm_STAND_ALONE
   /* these global variables must be declared even if they will not be used by this main */
   SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
   SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
   int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
   SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
   int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
   SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */
#else
   extern SUMA_CommonFields *SUMAg_CF; 
#endif


/* CODE */
   
   
   
/*!**
File : SUMA_SurfNorm.c
\author Ziad Saad
Date : Thu Jan 3 14:46:55 EST 2002
   
Purpose : 
   Calculate the polygon and node normals making up a surface
   It is assumed that FaceSetList does NOT contain an index >= N_NodeList
   
Usage : 
   RetStrct =   SUMA_SurfNorm (NodeList, N_NodeList, FaceSetList,  N_FaceSetList );
   
   
Input paramters : 
\param  NodeList (float *): N_NodeList x 3 vector containing the XYZ coordinates of each node
\param  N_NodeList (int): number of nodes in NodeList
\param  FaceSetList (int *): [N_FaceSetList x 3] vector (matrix prior to SUMA 1.2) of node indices forming each triangular facet (FaceSets). 
                              The indices must be relative to NodeList
\param  N_FaceSetList (int): 1st dimension of FaceSetList

   
Returns : 
\return   RetStrct, a structure of the type SUMA_SURF_NORM with the following fields
   N_Node (int) Number of nodes, 1st dim of NodeNormList
   N_Face (int) Number of facesets, 1st dim of FaceNormList
   FaceNormList (float *) N_Face x 3 vector (was matrix prior to SUMA 1.2) containing normalized normal vectors for each triangular faceset 
   NodeNormList (float *) N_Node x 3 vector (was matrix prior to SUMA 1.2) containing normalized normal vectors for each node
   
Support : 
\sa SUMA_MemberFaceSets  
\sa   
   
Side effects : 
   to free memory from RetStrct
   if (RetStrct.FaceNormList) SUMA_free(RetStrct.FaceNormList);
   if (RetStrct.NodeNormList) SUMA_free(RetStrct.NodeNormList);

The node normals are obtained by averaging the normals of the surrounding facesets (ie facesets containing the node).
This is an approximation and in special cases like a cube with a particular tesselation, the normals could be biased 
in direction if the node is a member of more triangles on one side of the object than the other sides. 
See Lab-book NIH-2, page 142 for an illustration or this miserable ascii rendition. 
               
            -------------------
           *             ##    *          Here, Node I will have its normal biased towards the direction of N2 and N3
          *          ##       * |         because node I is part of two triangles from one side and one triangle in    
         *       ##    N1    *  |         each of the other 2 sides.
        *    ##             *   |
       *##              I  * N3 |
       ------------------ *     | 
       |##               |######|    
       |  ##     N4      |     *
       |     ##          | N2 *
       |        ##       |   *
       |           ##    |  *
       |              ## | *
       ------------------     
***/
SUMA_SURF_NORM SUMA_SurfNorm (float *NodeList, int N_NodeList, int *FaceSetList, int N_FaceSetList )
{/*SUMA_SurfNorm*/
   static char stmp[200], FuncName[]={"SUMA_SurfNorm"}; 
   float d1[3], d2[3], d, nrm;
   SUMA_SURF_NORM RetStrct;
   int *Index, *N_Memb, i, j, maxind, NotMember, id, id2, ND, ip, NP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ND = 3;
   NP = 3;
   RetStrct.N_Node = N_NodeList; /* redundant, for clean up outside function */
   RetStrct.N_Face = N_FaceSetList;

   /* allocate space */
   if (LocalHead) fprintf(SUMA_STDERR,"%s: %d %d\n", FuncName, N_NodeList, N_FaceSetList);
   RetStrct.FaceNormList = (float *)SUMA_calloc (N_FaceSetList * NP, sizeof(float));
   RetStrct.NodeNormList = (float *)SUMA_calloc (N_NodeList * ND, sizeof(float));
   Index = (int *)SUMA_calloc (N_NodeList, sizeof(int));
   N_Memb = (int *)SUMA_calloc (N_NodeList, sizeof(int));
   if (!RetStrct.FaceNormList || !RetStrct.NodeNormList || !Index || !N_Memb)
      {
         SUMA_alloc_problem (FuncName);
         SUMA_RETURN (RetStrct);
      }

   /* calculate and normalize triangle normals */
   maxind = N_NodeList -1;
   for (i=0; i < N_FaceSetList; i++) {
      ip = NP * i;
      for (j=0; j < 3; j++) {
         d1[j] = NodeList[(ND*FaceSetList[ip])+j] - NodeList[(ND*FaceSetList[ip+1])+j];
         d2[j] = NodeList[(ND*FaceSetList[ip+1])+j] - NodeList[(ND*FaceSetList[ip+2])+j];
      }
      RetStrct.FaceNormList[ip  ] = d1[1]*d2[2] - d1[2]*d2[1];
      RetStrct.FaceNormList[ip+1] = d1[2]*d2[0] - d1[0]*d2[2];
      RetStrct.FaceNormList[ip+2] = d1[0]*d2[1] - d1[1]*d2[0];
      d = sqrt(RetStrct.FaceNormList[ip]*RetStrct.FaceNormList[ip]+RetStrct.FaceNormList[ip+1]*RetStrct.FaceNormList[ip+1]+RetStrct.FaceNormList[ip+2]*RetStrct.FaceNormList[ip+2]);
      if (d == 0.0) {
         /* I used to return here with a nasty message, but it seems that FreeSurfer surfaces contain such conditions 
         So, now I just set the normal to 1.0 in that case */
         /*SUMA_error_message (FuncName,"Zero length vector, returning",1);
         if (RetStrct.FaceNormList) SUMA_free(RetStrct.FaceNormList);
         if (RetStrct.NodeNormList) SUMA_free(RetStrct.NodeNormList);
         if (Index) SUMA_free(Index);
         if (N_Memb) SUMA_free(N_Memb);
         SUMA_RETURN (RetStrct);*/
         RetStrct.FaceNormList[ip] = 1.0;
         RetStrct.FaceNormList[ip+1] = 1.0;
         RetStrct.FaceNormList[ip+2] = 1.0;
         
      } else {
         RetStrct.FaceNormList[ip] /= d;
         RetStrct.FaceNormList[ip+1] /= d;
         RetStrct.FaceNormList[ip+2] /= d;
      }
      
      #if 0
         /* a test for the function SUMA_TriNorm */
         {
            float test_norm[3];
            SUMA_TriNorm (&(NodeList[(ND*FaceSetList[ip])]), &(NodeList[(ND*FaceSetList[ip+1])]), &(NodeList[(ND*FaceSetList[ip+2])]), test_norm);
            if (test_norm[0] != RetStrct.FaceNormList[ip] || test_norm[1] != RetStrct.FaceNormList[ip+1] || test_norm[2] != RetStrct.FaceNormList[ip+2]) {
               fprintf (SUMA_STDERR, "Error %s: Test of SUMA_TriNorm failed, difference in norms. Exiting.\n", FuncName);
               exit(1);
            } 
            fprintf (SUMA_STDERR,".");
         }
         
      #endif
      
         /*each node making up the FaceSet will get its normal vector updated*/
         if (FaceSetList[ip] > maxind || FaceSetList[ip+1] > maxind || FaceSetList[ip+2] > maxind) {
            SUMA_error_message (FuncName,"FaceSetList contains indices >= N_NodeList",1);
            if (RetStrct.FaceNormList) SUMA_free(RetStrct.FaceNormList);
            if (RetStrct.NodeNormList) SUMA_free(RetStrct.NodeNormList);
            if (Index) SUMA_free(Index);
            if (N_Memb) SUMA_free(N_Memb);
            SUMA_RETURN (RetStrct);
         }

         
         id2 = ND * FaceSetList[ip];
         RetStrct.NodeNormList[id2] += RetStrct.FaceNormList[ip];
         RetStrct.NodeNormList[id2+1] += RetStrct.FaceNormList[ip+1];
         RetStrct.NodeNormList[id2+2] += RetStrct.FaceNormList[ip+2];
         ++N_Memb[FaceSetList[ip]];
         id2 = ND * FaceSetList[ip+1];
         RetStrct.NodeNormList[id2] += RetStrct.FaceNormList[ip];
         RetStrct.NodeNormList[id2+1] += RetStrct.FaceNormList[ip+1];
         RetStrct.NodeNormList[id2+2] += RetStrct.FaceNormList[ip+2];
         ++N_Memb[FaceSetList[ip+1]];
         id2 = ND * FaceSetList[ip+2];
         RetStrct.NodeNormList[id2] += RetStrct.FaceNormList[ip];
         RetStrct.NodeNormList[id2+1] += RetStrct.FaceNormList[ip+1];
         RetStrct.NodeNormList[id2+2] += RetStrct.FaceNormList[ip+2];
         ++N_Memb[FaceSetList[ip+2]];
   }
      SUMA_LH("Normalizing");
      /*Now normalize NodeNormList*/
      NotMember = 0;
      for (i=0; i<N_NodeList; ++i)
         {
            id = ND * i;
            if (N_Memb[i])
            {
               /* fprintf(SUMA_STDERR,"%s: Node %d, Normal (pre scale) %f %f %f\n", FuncName, i, RetStrct.NodeNormList[id], RetStrct.NodeNormList[id+1], RetStrct.NodeNormList[id+2]); */
               RetStrct.NodeNormList[id] /= N_Memb[i];
               RetStrct.NodeNormList[id+1] /= N_Memb[i];
               RetStrct.NodeNormList[id+2] /= N_Memb[i];
               /* normalize */
               nrm = sqrt(RetStrct.NodeNormList[id]*RetStrct.NodeNormList[id] + RetStrct.NodeNormList[id+1]*RetStrct.NodeNormList[id+1] + RetStrct.NodeNormList[id+2]*RetStrct.NodeNormList[id+2]); 
               if (nrm) { /* at times nrm is 0. This happened once on a flat surface that was not consistently wound. Nodes that were 
                              members of two triangles of opposed normals ended up with a normal (and nrm) of 0 */ 
                  RetStrct.NodeNormList[id] /= nrm;
                  RetStrct.NodeNormList[id+1] /= nrm;
                  RetStrct.NodeNormList[id+2] /= nrm;
               } 
               
               /* fprintf(SUMA_STDERR,"%s: Node %d, N_Memb[i] = %d, nrm = %f\n", FuncName, i, N_Memb[i], nrm); */
            }
            else
            {
               ++NotMember;
               /*
               For some patches (Free Surfer's) such conditions are frequent, spitting out that message becomes old too quick */
               /*
               fprintf(stdout,"\n%s Warning: Node %d is not a member of any FaceSets, returning unit vector as normal.\n", FuncName, i); 
               */
               RetStrct.NodeNormList[id] = RetStrct.NodeNormList[id+1] = RetStrct.NodeNormList[id+2] = 1.0;
            }
         }
      if (NotMember) {
         sprintf (stmp, "(IGNORE for surfaces with cuts\n"
                        "%d nodes (%f%% of total) are\n"
                        "not members of any FaceSets.\n"
                        "Their normals are set to the\n"
                        "unit vector.\n", 
                        NotMember, (float)NotMember/(float)N_NodeList*100.0);
         SUMA_SL_Note(stmp);
      }
      
   if (N_Memb) SUMA_free(N_Memb);
   if (Index) SUMA_free(Index);
   SUMA_RETURN (RetStrct);
}/*SUMA_SurfNorm*/

/*!
   Try to guess the direction of the surface normals
   0 = dunno
   1 = out
   -1 = inwards 
   
*/
int SUMA_SurfNormDir (SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_SurfNormDir"};
   int in, cntneg, cntpos;
   float *a, *b, dot, d, U[3];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO->N_Node) {
      SUMA_S_Err("No Nodes!");
      SUMA_RETURN(0);
   }
   if (!SO->NodeNormList) {
      SUMA_RECOMPUTE_NORMALS(SO);
   }
   
   cntneg = 0;
   cntpos = 0;
   
   for (in=0; in<SO->N_Node; ++in) {   
      a = &(SO->NodeList[3*in]);
      SUMA_UNIT_VEC(SO->Center, a , U, d); /* original distance from center */
      b = &(SO->NodeNormList[3*in]);
      SUMA_DOTP_VEC(U, b, dot, 3, float, float); /* dot product with normal at node*/
      if (dot < 0) {
         ++cntneg;    
      } else {
         ++cntpos;
      }
   }
         
   if (cntneg < cntpos) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: %.2f%% sure that normals point outwards.\n", FuncName, (cntpos-cntneg)/(float)SO->N_Node*100.0);
      SUMA_RETURN(1);
   } else if (cntneg > cntpos) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: %.2f%% sure that normals point inwards.\n", FuncName, (cntneg-cntpos)/(float)SO->N_Node*100.0);
      SUMA_RETURN(-1);
   } else {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Not sure where node normals point!\n", FuncName);
      SUMA_RETURN(0);
   }
   
   SUMA_RETURN(0);
}
   
   
   
/*!**
File : SUMA_MemberFaceSets.c
\author Ziad Saad
Date : Thu Jan 3 12:01:49 EST 2002
   
Purpose : 
   Determines for each Node Index, from 0 to Nind-1 the indices of the FaceSets to which the node belongs.
   
Usage : 
   RetStruct = SUMA_MemberFaceSets (int Nind, int * FaceSetList, int nFace, int FaceSetDim);
   
Input paramters : 
\param  Nind (int) : Total number of nodes in Index. ( No value (node index) in FaceSetList can be > Nind-1 )
\param  FaceSetList (int *) : The FaceSetList vector (used to be matrix, prior to SUMA 1.2), [nFace x FaceSetDim] 
\param  nFace (int) : number of FaceSets in FaceSetList 
\param FaceSetDim (int): column (2nd) dimension of FaceSetList (usually 3 or 4)

Returns : 
\return  RetStruct : a SUMA_MEMBER_FACE_SETS  type structure with the following fields
NodeMemberOfFaceSet (int **) a matrix containing the indices of the FaceSets containing the nodes.
                         Each row i in NodeMemberOfFaceSet lists all FaceSets containing the  
                         node i. The list is delimited by a -1 entry
                         for all but the node that has a max of N_Memb_max members.
                         the size of NodeMemberOfFaceSet is [Nind x N_Memb_max];
   N_Memb (int *) the number of 1st order neighbors for each node in Index
   N_Memb_max (int) the second dimension of NodeMemberOfFaceSet, also represents
                          the maximum number of FaceSets that
                          any node belongs to.      
   Nnode (int) : it's equal to Nind but it is kept here so that the structure can contain enough info to 
             free memory that is allocated to it. 
   
                          
if (RetStruct.NodeMemberOfFaceSet == NULL) then an error occurred in the function 
   
Support : 
\sa  MemberFaceSets.c and .m for methods that work when the search is required for a few nodes only
   
Example: 
FaceSetList is:
1 4 6
6 4 2
6 1 3

and Nind is:
7

Then RetStruct.NodeMemberOfFaceSet is:
-1   0   0 ( Node 0 is not used in FaceSetList )
0   2   -1   (Node 1 is used in Facesets 0 & 2 )
1   -1   0   (Node 2 is used in Faceset 1)   
2   -1   0   (Node 3 is used in Faceset 2)   
0   1   -1   (Node 4 is used in Facesets 0 & 1)
-1   0   0   (Node 5 in not used in FaceSetList)   
0   1   2   (Node 6 is used in all Facesets)   

and RetStruct.N_Memb is:
0   2   1   1   2   0   3   

Side effects : 
   To free RetStruct, use:
   if (RetStruct.NodeMemberOfFaceSet) SUMA_free2D((char **)RetStruct.NodeMemberOfFaceSet, Nind);
   if (RetStruct.N_Memb) SUMA_free(RetStruct.N_Memb);
   if (RetStruct) SUMA_free(RetStrct);
   
   
***/
SUMA_MEMBER_FACE_SETS *SUMA_MemberFaceSets (int Nind, int * FaceSetList, 
                                          int nFr , int FaceDim, char *ownerid)
{/*SUMA_MemberFaceSets*/
   static char FuncName[]={"SUMA_MemberFaceSets"}; 
   SUMA_MEMBER_FACE_SETS *RetStrct;
   int **tmpMember;
   int i, inode, iface, ip , NP;
   
   SUMA_ENTRY;

   NP = FaceDim;
   RetStrct = (SUMA_MEMBER_FACE_SETS *)
                  SUMA_malloc(sizeof(SUMA_MEMBER_FACE_SETS));
   RetStrct->idcode_str = NULL;
   SUMA_NEW_ID(RetStrct->idcode_str, NULL);
   RetStrct->N_links = 0;
   if (ownerid) sprintf(RetStrct->owner_id, "%s", ownerid);
   else RetStrct->owner_id[0] = '\0';
   RetStrct->LinkedPtrType = SUMA_LINKED_MEMB_FACE_TYPE;
   
   RetStrct->N_Memb_max = RetStrct->Nnode = 0;
   RetStrct->N_Memb = NULL;
   RetStrct->NodeMemberOfFaceSet = NULL;
   
   /* Allocate return variables */
   tmpMember = (int **) SUMA_allocate2D (Nind, SUMA_MAX_MEMBER_FACE_SETS ,sizeof(int));
   RetStrct->N_Memb = (int *) SUMA_calloc (Nind, sizeof(int));
   
   if (!tmpMember || !RetStrct->N_Memb)
      {
         fprintf (SUMA_STDERR,"Error %s: Failed to allocate for tmpMember or RetStrct->N_Memb\n", FuncName);
         SUMA_RETURN (RetStrct);
      }
   
   /* loop through all facesets and tag nodes that make up FaceSets*/
   for (iface=0; iface<nFr; ++iface) {/*iface*/
      i = 0;
      ip = NP * iface;
      do {
         inode = FaceSetList[ip + i];
         if (inode > Nind) {
            fprintf (SUMA_STDERR,"Error %s: FaceSetList contains node indices >= Nind\n", FuncName);
            SUMA_RETURN (RetStrct);
         }
         tmpMember[inode][RetStrct->N_Memb[inode]] = iface; 
         ++RetStrct->N_Memb[inode];
         if (RetStrct->N_Memb[inode] >= SUMA_MAX_MEMBER_FACE_SETS) {
            fprintf (SUMA_STDERR,"Error %s: Node %d is member of (%d FaceSets) more than SUMA_MAX_MEMBER_FACE_SETS (%d)\n",\
                FuncName, inode, RetStrct->N_Memb[inode], SUMA_MAX_MEMBER_FACE_SETS);
            SUMA_RETURN (RetStrct);
         }
         if (RetStrct->N_Memb[inode] > RetStrct->N_Memb_max) RetStrct->N_Memb_max = RetStrct->N_Memb[inode];
         ++i;
      } while (i < FaceDim);
   }
   
   /*allocate just enough for returning variables */
   RetStrct->NodeMemberOfFaceSet = (int **) SUMA_allocate2D (Nind, RetStrct->N_Memb_max ,sizeof(int));
   if (!RetStrct->NodeMemberOfFaceSet)
      {
         fprintf(SUMA_STDERR,"Error %s: Failed to allocate for RetStrct->NodeMemberOfFaceSet\n", FuncName);
         SUMA_RETURN (RetStrct);
      }

   /* loop through all nodes, cp results into RetStrct->NodeMemberOfFaceSet and seal with -1 */
   for (inode = 0; inode < Nind; ++inode) {
      i = 0;
      while (i < RetStrct->N_Memb[inode]) {
         RetStrct->NodeMemberOfFaceSet[inode][i] = tmpMember[inode][i];
         ++i;
      }
      /*seal with -1 */
      if (RetStrct->N_Memb[inode] < RetStrct->N_Memb_max) RetStrct->NodeMemberOfFaceSet[inode][i] = -1;
   }

   /* Clean up time */
   if (tmpMember) SUMA_free2D((char **)tmpMember, Nind);
   
   RetStrct->Nnode = Nind;
   SUMA_RETURN (RetStrct);

}/*SUMA_MemberFaceSets*/

/*! Free a SUMA_MEMBER_FACE_SETS structure */
 
SUMA_Boolean SUMA_Free_MemberFaceSets (SUMA_MEMBER_FACE_SETS *MF)
{
   static char FuncName[]={"SUMA_Free_MemberFaceSets"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!MF) { SUMA_RETURN (YUP); }
   if (MF->N_links) {
      SUMA_LH("Just a link release");
      MF = (SUMA_MEMBER_FACE_SETS *)SUMA_UnlinkFromPointer((void *)MF);
      SUMA_RETURN (YUP);
   }
   
   SUMA_LH("No more links, here we go");
   if (MF->idcode_str) SUMA_free(MF->idcode_str);
   if (MF->NodeMemberOfFaceSet) SUMA_free2D((char **)MF->NodeMemberOfFaceSet, MF->Nnode);
   if (MF->N_Memb) SUMA_free(MF->N_Memb);
   if (MF) SUMA_free(MF);
   SUMA_RETURN (YUP);
}   
#ifdef TEST_SUMA_MemberFaceSets
void usage ()
   
  {/*Usage*/
          printf ("\nUsage:  SUMA_MemberFaceSets <FaceSetList> <indexfile> \n");
          printf ("\t <FaceSetList> : file containing the facesetlist \n");
          printf ("\t <index file> : file containing the indices of the nodes \n");
          printf ("\t              You're looking up wich node belongs to which FaceSets\n\n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \tThu Jan 3 12:01:49 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
   int n, nind, **X;
   SUMA_MEMBER_FACE_SETS *RetStrct;
   
   X = (int **) SUMA_allocate2D(3,3, sizeof(int));
   X[0][0] = 1; X[0][1]= 4; X[0][2]= 6;
   X[1][0] = 6; X[1][1]= 4; X[1][2]= 2;
   X[2][0] = 6; X[2][1]= 1; X[2][2]= 3;
   
    
   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_MemberFaceSets-Main-");
   
   
   n = 3;
   nind = 7;
   
   RetStrct = SUMA_MemberFaceSets  (nind, X, n , n);
   
   printf ("\nMember FaceSets :\n");
   SUMA_disp_dmat (RetStrct->NodeMemberOfFaceSet, nind, RetStrct->N_Memb_max, 1);

   printf ("\n# of Member FaceSets :\n");
   SUMA_disp_dvect (RetStrct->N_Memb , RetStrct->Nnode);

   if (RetStrct->N_Memb)
      {
         if (!SUMA_Free_MemberFaceSets (RetStrct)) {
            fprintf(SUMA_STDERR,"Error %s : Failed to free RetStrct", FuncName);
            exit(1);
         }
      }
   SUMA_free2D((char **)X, 3);
   SUMA_RETURN (0);   
}/* Main */
#endif

#ifdef SUMA_SurfNorm_STAND_ALONE
void usage ()
   
  {/*Usage*/
          printf ("\nUsage:  SUMA_SurfNorm <NodeList> <FaceSetList> \n");
          printf ("\t ..... \n\n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \tThu Jan 3 14:46:55 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_SurfNorm-Main-"}; 
   SUMA_SURF_NORM RetStrct;
   int nface, nnode;
   int *FaceSetList;
   float *NodeList;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_mainENTRY;
   
   /* allocate space for CommonFields structure */
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   if (LocalHead) fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);
   
   
   
   if (argc < 3)
       {
          usage ();
          exit (1);
       }
   
   nnode = SUMA_float_file_size(argv[1]);
   nnode /= 3;
   nface = SUMA_float_file_size(argv[2]);
   nface /= 3;

   FaceSetList = (int *) SUMA_calloc (nface * 3, sizeof(int));
   NodeList = (float *) SUMA_calloc (nnode * 3, sizeof(float)); 
   
   if (!FaceSetList || !NodeList)
      {
         SUMA_alloc_problem(FuncName);
         exit (0);
      }
   
   SUMA_Read_dfile (FaceSetList, argv[2], nface * 3);
   printf ("\nFaceSets (%d):\n", nface);
   SUMA_disp_vecdmat (FaceSetList, nface, 3, 1);
   
   SUMA_Read_file (NodeList, argv[1], 3*nnode);
   
   printf ("\nNodes : (%d)\n", nnode);
   SUMA_disp_vecmat (NodeList, nnode, 3, 1, SUMA_ROW_MAJOR, NULL, 0);
   
   RetStrct = SUMA_SurfNorm(NodeList,  nnode, FaceSetList, nface );
   printf ("\nNode Norms:\n");
   SUMA_disp_vecmat (RetStrct.NodeNormList, RetStrct.N_Node, 3, 1, SUMA_ROW_MAJOR, NULL, 0);
   printf ("\nFaceSet Norms:\n");
   SUMA_disp_vecmat (RetStrct.FaceNormList, RetStrct.N_Face, 3, 1, SUMA_ROW_MAJOR, NULL, 0);
   
   
   if (RetStrct.FaceNormList) SUMA_free(RetStrct.FaceNormList);
   if (RetStrct.NodeNormList) SUMA_free(RetStrct.NodeNormList);
   if (FaceSetList) SUMA_free(FaceSetList);
   if (NodeList) SUMA_free(NodeList);
   SUMA_RETURN (0);
}/* Main */
#endif
