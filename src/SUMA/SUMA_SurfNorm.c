/*#define TEST*/
/*#define DEBUG_3*/
#ifdef DEBUG_1
	#define DEBUG_2
	#define DEBUG_3
#endif
   
/* Header FILES */
   
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <GL/GLwMDrawA.h>  /* Motif OpenGL drawing area. */
   
#include "SUMA_suma.h"

/* CODE */
   
   
   
/*!**
File : SUMA_SurfNorm.c
\author Ziad Saad
Date : Thu Jan 3 14:46:55 EST 2002
   
Purpose : 
   Calculate the polygon and node normals making up a surface
   It is assumed that FaceSetList does NOT contain an index >= N_NodeList
   
Usage : 
	RetStrct =	SUMA_SurfNorm (NodeList, N_NodeList, FaceSetList,  N_FaceSetList );
   
   
Input paramters : 
\param  NodeList (float **): [N_NodeList x 3] matrix containing the XYZ coordinates of each node
\param  N_NodeList (int): 1st dimension of NodeList
\param  FaceSetList (int **): [N_FaceSetList x 3] matrix of node indices forming each triangular facet (FaceSets). 
                              The indices must be relative to NodeList
\param  N_FaceSetList (int): 1st dimension of FaceSetList

   
Returns : 
\return   RetStrct, a structure of the type SUMA_SURF_NORM with the following fields
	N_Node (int) Number of nodes, 1st dim of Node_NormList
	N_Face (int) Number of facesets, 1st dim of Face_NormList
	Face_NormList (float **) N_Face x 3 matrix containing normalized normal vectors for each triangular faceset 
	Node_NormList (float **) N_Node x 3 matrix containing normalized normal vectors for each node
   
Support : 
\sa SUMA_MemberFaceSets  
\sa   
   
Side effects : 
	to free memory from RetStrct
	if (RetStrct.Face_NormList) SUMA_free2D((char **)RetStrct.Face_NormList, RetStrct.N_Face);
	if (RetStrct.Node_NormList) SUMA_free2D((char **)RetStrct.Node_NormList, RetStrct.N_Node);
   
***/
SUMA_SURF_NORM SUMA_SurfNorm (float **NodeList, int N_NodeList, int **FaceSetList, int N_FaceSetList )
{/*SUMA_SurfNorm*/
   char FuncName[100]; 
	float d1[3], d2[3], d, nrm;
	SUMA_SURF_NORM RetStrct;
	int *Index, *N_Memb, i, j, maxind, NotMember;
	
   /* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_SurfNorm");

	RetStrct.N_Node = N_NodeList; /* redundant, but practical for clean up outside the function */
	RetStrct.N_Face = N_FaceSetList;

	/* allocate space */
 	RetStrct.Face_NormList = (float **)SUMA_allocate2D (N_FaceSetList, 3, sizeof(float));
	RetStrct.Node_NormList = (float **)SUMA_allocate2D (N_NodeList, 3, sizeof(float));
	Index = (int *)calloc (N_NodeList, sizeof(int));
	N_Memb = (int *)calloc (N_NodeList, sizeof(int));
	if (!RetStrct.Face_NormList || !RetStrct.Node_NormList || !Index || !N_Memb)
		{
			SUMA_alloc_problem (FuncName);
			return (RetStrct);
		}
	
	/* calculate and normalize triangle normals */
	maxind = N_NodeList -1;
	for (i=0; i < N_FaceSetList; i++) {
		for (j=0; j < 3; j++) {
			d1[j] = NodeList[FaceSetList[i][0]][j] - NodeList[FaceSetList[i][1]][j];
			d2[j] = NodeList[FaceSetList[i][1]][j] - NodeList[FaceSetList[i][2]][j];
		}
		RetStrct.Face_NormList[i][0] = d1[1]*d2[2] - d1[2]*d2[1];
		RetStrct.Face_NormList[i][1] = d1[2]*d2[0] - d1[0]*d2[2];
		RetStrct.Face_NormList[i][2] = d1[0]*d2[1] - d1[1]*d2[0];
		d = sqrt(RetStrct.Face_NormList[i][0]*RetStrct.Face_NormList[i][0]+RetStrct.Face_NormList[i][1]*RetStrct.Face_NormList[i][1]+RetStrct.Face_NormList[i][2]*RetStrct.Face_NormList[i][2]);
		if (d == 0.0) {
			/* I used to return here with a nasty message, but it seems that FreeSurfer surfaces contain such conditions 
			So, now I just set the normal to 1.0 in that case */
			/*SUMA_error_message (FuncName,"Zero length vector, returning",1);
			if (RetStrct.Face_NormList) SUMA_free2D((char **)RetStrct.Face_NormList, N_FaceSetList);
			if (RetStrct.Node_NormList) SUMA_free2D((char **)RetStrct.Node_NormList, N_NodeList);
			if (Index) free(Index);
			if (N_Memb) free(N_Memb);
			return (RetStrct);*/
		RetStrct.Face_NormList[i][0] = 1.0;
		RetStrct.Face_NormList[i][1] = 1.0;
		RetStrct.Face_NormList[i][2] = 1.0;
			
		} else {
			RetStrct.Face_NormList[i][0] /= d;
			RetStrct.Face_NormList[i][1] /= d;
			RetStrct.Face_NormList[i][2] /= d;
		}
			/*each node making up the FaceSet will get its normal vector updated*/
			if (FaceSetList[i][0] > maxind || FaceSetList[i][1] > maxind || FaceSetList[i][2] > maxind) {
				SUMA_error_message (FuncName,"FaceSetList contains indices >= N_NodeList",1);
				if (RetStrct.Face_NormList) SUMA_free2D((char **)RetStrct.Face_NormList, N_FaceSetList);
				if (RetStrct.Node_NormList) SUMA_free2D((char **)RetStrct.Node_NormList, N_NodeList);
				if (Index) free(Index);
				if (N_Memb) free(N_Memb);
				return (RetStrct);
			}

			RetStrct.Node_NormList[FaceSetList[i][0]][0] += RetStrct.Face_NormList[i][0];
			RetStrct.Node_NormList[FaceSetList[i][0]][1] += RetStrct.Face_NormList[i][1];
			RetStrct.Node_NormList[FaceSetList[i][0]][2] += RetStrct.Face_NormList[i][2];
			++N_Memb[FaceSetList[i][0]];
			RetStrct.Node_NormList[FaceSetList[i][1]][0] += RetStrct.Face_NormList[i][0];
			RetStrct.Node_NormList[FaceSetList[i][1]][1] += RetStrct.Face_NormList[i][1];
			RetStrct.Node_NormList[FaceSetList[i][1]][2] += RetStrct.Face_NormList[i][2];
			++N_Memb[FaceSetList[i][1]];
			RetStrct.Node_NormList[FaceSetList[i][2]][0] += RetStrct.Face_NormList[i][0];
			RetStrct.Node_NormList[FaceSetList[i][2]][1] += RetStrct.Face_NormList[i][1];
			RetStrct.Node_NormList[FaceSetList[i][2]][2] += RetStrct.Face_NormList[i][2];
			++N_Memb[FaceSetList[i][2]];
	}
		/*Now normalize Node_NormList*/
		NotMember = 0;
		for (i=0; i<N_NodeList; ++i)
			{
				if (N_Memb[i])
				{
					RetStrct.Node_NormList[i][0] /= N_Memb[i];
					RetStrct.Node_NormList[i][1] /= N_Memb[i];
					RetStrct.Node_NormList[i][2] /= N_Memb[i];
					/* normalize */
					nrm = sqrt(RetStrct.Node_NormList[i][0]*RetStrct.Node_NormList[i][0] + RetStrct.Node_NormList[i][1]*RetStrct.Node_NormList[i][1] + RetStrct.Node_NormList[i][2]*RetStrct.Node_NormList[i][2]); 
					RetStrct.Node_NormList[i][0] /= nrm;
					RetStrct.Node_NormList[i][1] /= nrm;
					RetStrct.Node_NormList[i][2] /= nrm;
					
				}
				else
				{
					++NotMember;
					/*
					For some patches (Free Surfer's) such conditions are frequent, spitting out that message becomes old too quick */
					/*
					fprintf(stdout,"\n%s Warning: Node %d is not a member of any FaceSets, returning unit vector as normal.\n", FuncName, i); 
					*/
					RetStrct.Node_NormList[i][0] = RetStrct.Node_NormList[i][1] = RetStrct.Node_NormList[i][2] = 1.0;
				}
			}
		if (NotMember) {
			fprintf(SUMA_STDERR,"\nWARNING %s: %d nodes (%f%% of total) are not members of any FaceSets. Their Normals are set to the unit vector.\n\n\a"\
					,FuncName, NotMember, (float)NotMember/(float)N_NodeList*100.0);
		}
	if (N_Memb) free (N_Memb);
	if (Index) free (Index);
	return (RetStrct);
}/*SUMA_SurfNorm*/

   
   
   
/*!**
File : SUMA_MemberFaceSets.c
\author Ziad Saad
Date : Thu Jan 3 12:01:49 EST 2002
   
Purpose : 
   Determines for each Node Index, from 0 to Nind-1 the indices of the FaceSets to which the node belongs.
   
Usage : 
	RetStruct = SUMA_MemberFaceSets (int Nind, int ** FaceSetList, int nFace, int FaceSetDim);
	
Input paramters : 
\param  Nind (int) : Total number of nodes in Index. ( No value (node index) in FaceSetList can be > Nind-1 )
\param  FaceSetList (int **) : The FaceSetList matrix, [nFace x FaceSetDim] 
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
-1	0	0 ( Node 0 is not used in FaceSetList )
0	2	-1	(Node 1 is used in Facesets 0 & 2 )
1	-1	0	(Node 2 is used in Faceset 1)	
2	-1	0	(Node 3 is used in Faceset 2)	
0	1	-1	(Node 4 is used in Facesets 0 & 1)
-1	0	0	(Node 5 in not used in FaceSetList)	
0	1	2	(Node 6 is used in all Facesets)	

and RetStruct.N_Memb is:
0	2	1	1	2	0	3	

Side effects : 
   To free RetStruct, use:
	if (RetStruct.NodeMemberOfFaceSet) free2D((char **)RetStruct.NodeMemberOfFaceSet, Nind);
	if (RetStruct.N_Memb) free (RetStruct.N_Memb);
   
   
***/
SUMA_MEMBER_FACE_SETS *SUMA_MemberFaceSets (int Nind, int ** FaceSetList, int nFr , int FaceDim)
{/*SUMA_MemberFaceSets*/
   char FuncName[100]; 
   SUMA_MEMBER_FACE_SETS *RetStrct;
	int **tmpMember;
	int i, inode, iface;
   
	/* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_MemberFaceSets");
	
	RetStrct = (SUMA_MEMBER_FACE_SETS *)malloc(sizeof(SUMA_MEMBER_FACE_SETS));
	
	RetStrct->N_Memb_max = RetStrct->Nnode = 0;
	RetStrct->N_Memb = NULL;
	RetStrct->NodeMemberOfFaceSet = NULL;
	
	/* Allocate return variables */
	tmpMember = (int **) SUMA_allocate2D (Nind, SUMA_MAX_MEMBER_FACE_SETS ,sizeof(int));
	RetStrct->N_Memb = (int *) calloc (Nind, sizeof(int));
	
	if (!tmpMember || !RetStrct->N_Memb)
		{
			fprintf (SUMA_STDERR,"Error %s: Failed to allocate for tmpMember or RetStrct->N_Memb\n", FuncName);
			return (RetStrct);
		}
	
	/* loop through all facesets and tag nodes that make up FaceSets*/
	for (iface=0; iface<nFr; ++iface) {/*iface*/
		i = 0;
		do {
			inode = FaceSetList[iface][i];
			if (inode > Nind) {
				fprintf (SUMA_STDERR,"Error %s: FaceSetList contains node indices >= Nind\n", FuncName);
				return (RetStrct);
			}
			tmpMember[inode][RetStrct->N_Memb[inode]] = iface; 
			++RetStrct->N_Memb[inode];
			if (RetStrct->N_Memb[inode] >= SUMA_MAX_MEMBER_FACE_SETS) {
				fprintf (SUMA_STDERR,"Error %s: Node %d is member of (%d FaceSets) more than SUMA_MAX_MEMBER_FACE_SETS (%d)\n",\
					 FuncName, inode, RetStrct->N_Memb[inode], SUMA_MAX_MEMBER_FACE_SETS);
				return (RetStrct);
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
			return (RetStrct);
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
	return (RetStrct);

}/*SUMA_MemberFaceSets*/

/*! Free a SUMA_MEMBER_FACE_SETS structure */
 
SUMA_Boolean SUMA_Free_MemberFaceSets (SUMA_MEMBER_FACE_SETS *MF)
{
	if (MF->NodeMemberOfFaceSet) SUMA_free2D((char **)MF->NodeMemberOfFaceSet, MF->Nnode);
	if (MF->N_Memb) free (MF->N_Memb);
	if (MF) free (MF);
	return (YUP);
}   
#ifdef TEST_SUMA_MemberFaceSets
void usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_MemberFaceSets <FaceSetList> <indexfile> \n");
          printf ("\t <FaceSetList> : file containing the facesetlist \n");
			 printf ("\t <index file> : file containing the indices of the nodes \n");
			 printf ("\t              You're looking up wich node belongs to which FaceSets\n\n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \tThu Jan 3 12:01:49 EST 2002 \n");
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
	return (0);	
}/* Main */
#endif

#ifdef TEST_SUMA_SurfNorm
void usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_SurfNorm <NodeList> <FaceSetList> \n");
          printf ("\t ..... \n\n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \tThu Jan 3 14:46:55 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
   SUMA_SURF_NORM RetStrct;
	int nface, nnode;
	int **FaceSetList;
	float **NodeList;
	
   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_SurfNorm-Main-");
   
   
   if (argc < 3)
       {
          usage ();
          exit (1);
       }
   
	nnode = SUMA_float_file_size(argv[1]);
	nnode /= 3;
	nface = SUMA_float_file_size(argv[2]);
	nface /= 3;

	FaceSetList = (int **) SUMA_allocate2D (nface, 3, sizeof(int));
	NodeList = (float **) SUMA_allocate2D (nnode, 3, sizeof(float)); 
	
	if (!FaceSetList || !NodeList)
		{
			SUMA_alloc_problem(FuncName);
			exit (0);
		}
	
	SUMA_Read_2Ddfile (FaceSetList, argv[2], nface, 3);
	printf ("\nFaceSets (%d):\n", nface);
	SUMA_disp_dmat (FaceSetList, nface, 3, 1);
	
	SUMA_Read_2Dfile (NodeList, argv[1], nnode, 3);
	
	printf ("\nNodes : (%d)\n", nnode);
	SUMA_disp_mat (NodeList, nnode, 3, 1);
	
	RetStrct = SUMA_SurfNorm(NodeList,  nnode, FaceSetList, nface );
	printf ("\nNode Norms:\n");
	SUMA_disp_mat (RetStrct.Node_NormList, RetStrct.N_Node, 3, 1);
	printf ("\nFaceSet Norms:\n");
	SUMA_disp_mat (RetStrct.Face_NormList, RetStrct.N_Face, 3, 1);
	
	
	if (RetStrct.Face_NormList) SUMA_free2D((char **)RetStrct.Face_NormList, RetStrct.N_Face);
	if (RetStrct.Node_NormList) SUMA_free2D((char **)RetStrct.Node_NormList, RetStrct.N_Node);
	if (FaceSetList) SUMA_free2D((char **)FaceSetList, nface);
	if (NodeList) SUMA_free2D((char **)NodeList, nnode);
	return (0);
}/* Main */
#endif
