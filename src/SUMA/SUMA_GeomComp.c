#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 

/*!
	Given a set of node indices, return a patch of the original surface that contains them
	
	Patch = SUMA_getPatch (NodesSelected, N_Nodes, Full_FaceSetList, N_Full_FaceSetList, Memb)

	\param NodesSelected (int *) N_Nodes x 1 Vector containing indices of selected nodes. 
				These are indices into NodeList making up the surface formed by Full_FaceSetList.
	\param N_Nodes (int) number of elements in NodesSelected
	\param Full_FaceSetList (int **) N_Full_FaceSetList  x 3 matrix containing the triangles forming the surface 
	\param N_Full_FaceSetList (int) number of triangular facesets forming the surface
	\param Memb (SUMA_MEMBER_FACE_SETS *) structure containing the node membership information (result of SUMA_MemberFaceSets function)

	\ret Patch (SUMA_PATCH *) Structure containing the patch's FaceSetList, FaceSetIndex (into original surface) and number of elements.
			returns NULL in case of trouble.	Free Patch with SUMA_freePatch(Patch);

	\sa SUMA_MemberFaceSets, SUMA_isinbox, SUMA_PATCH
*/

SUMA_PATCH * SUMA_getPatch (int *NodesSelected, int N_Nodes, int **Full_FaceSetList, int N_Full_FaceSetList, SUMA_MEMBER_FACE_SETS *Memb)
{
	int * BeenSelected;
	int i, j, node;
	SUMA_PATCH *Patch;
 	static char FuncName[]={"SUMA_getPatch"};
	
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
	
	
	BeenSelected = (int *)calloc (N_Full_FaceSetList, sizeof(int));
	Patch = (SUMA_PATCH *)malloc(sizeof(Patch));
	
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
	
	Patch->FaceSetList = (int **) SUMA_allocate2D (Patch->N_FaceSet, 3, sizeof(int));
	Patch->FaceSetIndex = (int *) calloc (Patch->N_FaceSet, sizeof(int));
	
	if (!Patch->FaceSetList || !Patch->FaceSetIndex) {
		fprintf (SUMA_STDERR,"Error %s: Could not allocate for Patch->FaceSetList || Patch_FaceSetIndex.\n", FuncName);
		SUMA_RETURN(NULL);
	}
	j=0;
	for (i=0; i < N_Full_FaceSetList; ++i) {
		if (BeenSelected[i]) {
			Patch->FaceSetIndex[j] = i;
			Patch->FaceSetList[j][0] = Full_FaceSetList[i][0];
			Patch->FaceSetList[j][1] = Full_FaceSetList[i][1];
			Patch->FaceSetList[j][2] = Full_FaceSetList[i][2];
			++j;
		}
	}
	
	if (BeenSelected) free(BeenSelected);
	
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
	
	
	if (Patch->FaceSetIndex) free(Patch->FaceSetIndex);
	if (Patch->FaceSetList) SUMA_free2D((char **)Patch->FaceSetList, Patch->N_FaceSet);
	if (Patch) free(Patch);
	SUMA_RETURN(YUP);
	
}
