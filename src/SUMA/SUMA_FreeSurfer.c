
#include "SUMA_suma.h"
 
#ifdef SUMA_FreeSurfer_STAND_ALONE
	SUMA_CommonFields *SUMAg_CF;
#else
	extern SUMA_CommonFields *SUMAg_CF; 
#endif
 
/*! Functions to read and manipulate FreeSurfer surfaces*/

/* CODE */
   
   
   
/*!**
File : SUMA_SureFit.c
\author Ziad Saad
Date : Fri Feb 8 16:29:06 EST 2002
   
Purpose : 
   Read SureFit data
   
*/

/*!**  
Function: SUMA_FreeSurfer_Read 
Usage : 
Ret = SUMA_FreeSurfer_Read (surfname, FreeSurfer)
	
	For a full surface definition, it is assumed that the first line can be a comment.
	The second line contains the number of nodes followed by the number of FaceSets
	The NodeList follows with X Y Z 0
	The FaceSetList follows with i1 i2 i3 0
   
	
Input paramters : 
\param surfname (char *) name of surface (or patch) file output by:
        mris_convert <surface_name> <surface_name.asc>
   or if it is a patch by
        mris_convert -p <patch_name> <patch_name.asc>
       
\param FreeSurfer (SUMA_FreeSurfer_struct *) pointer to the FreeSurfer structure
   
Returns : 
\return  (SUMA_Boolean) YUP/NOPE for success/failure
   
Support : 
\sa   LoadFreeSurf.m
\sa   
   
Side effects : 
   
   
   
***/
SUMA_Boolean SUMA_FreeSurfer_Read (char * f_name, SUMA_FreeSurfer_struct *FS)
{/*SUMA_FreeSurfer_Read*/
   char stmp[50]; 
   FILE *fs_file;
	int ex, cnt, jnki, amax[3], maxamax, maxamax2, id, ND, id2, NP, ip, *NodeId;
	float jnkf, *NodeList;
	char c;
	static char FuncName[]={"SUMA_FreeSurfer_Read"};
	SUMA_Boolean LocalHead = NOPE;
	   
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"Error %s: File %s does not exist or cannot be read.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}else {
		fprintf(SUMA_STDERR,"%s: File %s exists and will be read.\n", FuncName, f_name);
	}
	
	
	/* start reading */
	fs_file = fopen (f_name,"r");
	if (fs_file == NULL)
		{
			SUMA_error_message (FuncName,"Could not open input file ",0);
			SUMA_RETURN (NOPE);
		}

	sprintf(FS->name, "%s", f_name);
	
	/* read first character and check if it is a comment */
	ex = fscanf (fs_file,"%c",&c);
	if (c == '#') {
		if (LocalHead) fprintf (SUMA_STDOUT, "%s: Found comment\n", FuncName); 
		
      /*skip till next line */
		sprintf(FS->comment,"#"); 
		cnt = 0;
		while (ex != EOF && c != '\n') {
			ex = fscanf (fs_file,"%c",&c);
			if (cnt < SUMA_MAX_STRING_LENGTH-2) {
				sprintf(FS->comment, "%s%c", FS->comment, c);
				++cnt;
			} else {
				fprintf(SUMA_STDERR,"Error %s: Too long a comment in FS file, increase SUMA_FS_MAX_COMMENT_LENGTH\n", FuncName);
				SUMA_RETURN (NOPE);
			}
		}
	}
	
	/* find out if surface is patch */
	sprintf(stmp,"patch");
	if (SUMA_iswordin (FS->comment, stmp) == 1) {
		FS->isPatch = YUP;
	}
	else {
		FS->isPatch = NOPE;
	}
		
	/* read in the number of nodes and the number of facesets */
	ex = fscanf(fs_file, "%d %d", &(FS->N_Node), &(FS->N_FaceSet));
	
	if (LocalHead) fprintf (SUMA_STDOUT, "%s: Allocating for NodeList (%dx3) and FaceSetList(%dx3)\n", FuncName, FS->N_Node, FS->N_FaceSet);
   
   /* allocate space for NodeList and FaceSetList */
	FS->NodeList = (float *)SUMA_calloc(FS->N_Node * 3, sizeof(float));
	FS->FaceSetList = (int *)SUMA_calloc(FS->N_FaceSet * 3, sizeof(int));
	FS->NodeId = (int *)SUMA_calloc(FS->N_Node, sizeof(int));
	if (FS->NodeList == NULL || FS->FaceSetList == NULL || FS->NodeId == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Could not allocate for FS->NodeList &/| FS->FaceSetList &/| FS->NodeId\n", FuncName);
		SUMA_RETURN (NOPE);
	} 
	if (FS->isPatch) {
		FS->FaceSetIndexInParent = (int *)SUMA_calloc(FS->N_FaceSet, sizeof(int));
		if (FS->FaceSetIndexInParent == NULL) {
			fprintf(SUMA_STDERR,"Error %s: Could not allocate for FS->FaceSetIndexInParent\n", FuncName);
			SUMA_RETURN (NOPE);
		}
	} else {
		FS->FaceSetIndexInParent = NULL;
	}
	
	if (!FS->isPatch) {
	   if (LocalHead) fprintf (SUMA_STDOUT, "%s: Reading full surface...\n", FuncName);
		/* read in the nodes */
		cnt = 0;
		while (ex != EOF && cnt < FS->N_Node) {
			FS->NodeId[cnt] = cnt;
			id = 3 * cnt;
			ex = fscanf(fs_file, "%f %f %f %f", &(FS->NodeList[id]), &(FS->NodeList[id+1]),&(FS->NodeList[id+2]), &jnkf);
			++cnt;
		}
		if (cnt != FS->N_Node) {
			fprintf(SUMA_STDERR,"Error %s: Expected %d nodes, %d read.\n", FuncName, FS->N_Node, cnt);
			SUMA_RETURN (NOPE);
		}

		/* read in the facesets */
		cnt = 0;
		while (ex != EOF && cnt < FS->N_FaceSet) {
			ip = 3 * cnt;
			ex = fscanf(fs_file, "%d %d %d %d", &(FS->FaceSetList[ip]), &(FS->FaceSetList[ip+1]),&(FS->FaceSetList[ip+2]), &jnki);
			++cnt;
		}
		if (cnt != FS->N_FaceSet) {
			fprintf(SUMA_STDERR,"Error %s: Expected %d FaceSets, %d read.\n", FuncName, FS->N_FaceSet, cnt);
			SUMA_RETURN (NOPE);
		}
	} /* read a full surface */
	else { /* that's a patch */
	   if (LocalHead) fprintf (SUMA_STDOUT, "%s: Reading patch ...\n", FuncName);
		/* Node IDs are a reference to those in the parent surface */
		cnt = 0;
      while (ex != EOF && cnt < FS->N_Node) {
			ex = fscanf(fs_file, "%d", &(FS->NodeId[cnt]));
			id = 3 * cnt;
         /* fprintf (SUMA_STDERR, "FS->NodeId[cnt] = %d: cnt = %d, id=%d, id1 = %d, id2 = %d\n", FS->NodeId[cnt], cnt, id, id+1, id+2); */ 
			ex = fscanf(fs_file, "%f %f %f", &(FS->NodeList[id]),&(FS->NodeList[id+1]),&(FS->NodeList[id+2]));
			++cnt;
		}
      if (cnt != FS->N_Node) {
			fprintf(SUMA_STDERR,"Error %s: Expected %d nodes, %d read.\n", FuncName, FS->N_Node, cnt);
			SUMA_RETURN (NOPE);
		}
		
      if (LocalHead) fprintf (SUMA_STDOUT, "%s: Reading FaceSets...\n", FuncName);
		/* read in the facesets */
		cnt = 0;
		while (ex != EOF && cnt < FS->N_FaceSet) {
			ex = fscanf(fs_file, "%d", &(FS->FaceSetIndexInParent[cnt]));
			ip = 3 * cnt;
			ex = fscanf(fs_file, "%d %d %d",  &(FS->FaceSetList[ip]), &(FS->FaceSetList[ip+1]),&(FS->FaceSetList[ip+2]));
			++cnt;
		}
		if (cnt != FS->N_FaceSet) {
			fprintf(SUMA_STDERR,"Error %s: Expected %d FaceSets, %d read.\n", FuncName, FS->N_FaceSet, cnt);
			SUMA_RETURN (NOPE);
		}
		/* The FaceSet List which will be read next, uses indices into the NodeList of the parent surface
		This means that it expects a NodeList of the size of the NodeList in the parent surface. 
		One could read the maximum number of nodes in the parent surface and create a NodeList of that size.
		However, that would require keeping track of the link between the patch file and the parent file.
		Instead, I will search through the FaceSetList for the highest index and allocate a new nodelist to match it*/
      
		SUMA_MAX_VEC(FS->FaceSetList, FS->N_FaceSet * 3, maxamax); ++maxamax;
      /* make sure that the node list does not refer to nodes of an index higher than that in NodeId */
      SUMA_MAX_VEC(FS->NodeId, FS->N_Node, maxamax2); ++maxamax2;
      if (maxamax2 > maxamax) {
         fprintf(SUMA_STDERR,"Error %s: Found NodeId in the NodeList larger than Ids found in FaceSetList.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
      if (LocalHead) fprintf (SUMA_STDOUT, "%s: Copying NodeList, allocating for new nodelist %dx3 elements...\n", \
         FuncName, maxamax);
         
		NodeList = (float *)SUMA_calloc(maxamax * 3, sizeof(float));
		NodeId = (int *)SUMA_calloc (maxamax, sizeof(int));
      
      if (NodeList == NULL || NodeId == NULL)
		{
			fprintf(SUMA_STDERR,"Error %s: Could not allocate for NodeList or NodeId\n", FuncName);
			SUMA_RETURN (NOPE);
		} 
		/*Now copy pertinent nodes into NodeList */
		
      for (cnt=0; cnt< FS->N_Node; ++cnt) {
			id = 3*cnt; 
			id2 = 3*FS->NodeId[cnt];
         /* fprintf (SUMA_STDERR, "%s: id = %d id2 = %d\n", FuncName, id, id2); */
			NodeList[id2] = FS->NodeList[id];
			NodeList[id2+1] = FS->NodeList[id+1];
			NodeList[id2+2] = FS->NodeList[id+2];
		}
		
      /* this is redundant here, but should be done to match what comes out of a full surface */
      for (cnt=0; cnt< maxamax; ++cnt) {
         NodeId[cnt] = cnt;
      }
      
      /* Now free FS->NodeList & FS->NodeId */
		free(FS->NodeList);
		free(FS->NodeId);
      
		/*make FS->NodeList be NodeList */
		FS->NodeList = NodeList;
      FS->NodeId = NodeId;
		FS->N_Node = maxamax;
	} /* read a patch */
	
	fclose (fs_file);
	SUMA_RETURN (YUP);
	
}/* SUMA_FreeSurfer_Read*/

/*! 
	free memory allocated for FreeSurfer structure  
*/
SUMA_Boolean SUMA_Free_FreeSurfer (SUMA_FreeSurfer_struct *FS)
{
	static char FuncName[]={"SUMA_Free_FreeSurfer"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	if (FS->FaceSetList != NULL) free(FS->FaceSetList);
	if (FS->NodeList != NULL) free(FS->NodeList);
	if (FS->NodeId != NULL) free(FS->NodeId);
	if (FS->FaceSetIndexInParent != NULL) free(FS->FaceSetIndexInParent);
	if (FS != NULL) free (FS);
	SUMA_RETURN (YUP);
}

/*! 
	Show elements of FreeSurfer structure 
*/
void SUMA_Show_FreeSurfer (SUMA_FreeSurfer_struct *FS, FILE *Out)
{	
	static char FuncName[]={"SUMA_Show_FreeSurfer"};
	int ND = 3, id, ip;
	
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	if (Out == NULL) Out = SUMA_STDOUT;
	fprintf (Out, "Comment: %s\n", FS->comment);
	fprintf (Out, "N_Node %d\n", FS->N_Node);
	fprintf (Out, "First 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		FS->NodeId[0], FS->NodeList[0], FS->NodeList[1], FS->NodeList[2],
		FS->NodeId[1], FS->NodeList[3], FS->NodeList[4], FS->NodeList[5]);
	fprintf (Out, "Last 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		FS->NodeId[FS->N_Node-2], FS->NodeList[3*(FS->N_Node-2)], FS->NodeList[3*(FS->N_Node-2)+1], FS->NodeList[3*(FS->N_Node-2)+2],
		FS->NodeId[FS->N_Node-1], FS->NodeList[3*(FS->N_Node-1)], FS->NodeList[3*(FS->N_Node-1)+1], FS->NodeList[3*(FS->N_Node-1)+2]);
	fprintf (Out, "N_FaceSet %d\n", FS->N_FaceSet);
	if (!FS->isPatch) {
		fprintf (Out, "First 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
			FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2],
			FS->FaceSetList[3], FS->FaceSetList[4], FS->FaceSetList[5]);
		fprintf (Out, "Last 2 polygons:\n%d %d %d\n%d %d %d\n", \
			FS->FaceSetList[3 * (FS->N_FaceSet-2)], FS->FaceSetList[3 * (FS->N_FaceSet-2) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-2) + 2],
			FS->FaceSetList[3 * (FS->N_FaceSet-1)], FS->FaceSetList[3 * (FS->N_FaceSet-1) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-1) + 2]);
	} else {
		fprintf (Out, "First 2 polygons:\n\t[parent ID:%d] %d %d %d\n\t[parent ID:%d] %d %d %d\n", \
			FS->FaceSetIndexInParent[0], FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2],
			FS->FaceSetIndexInParent[1], FS->FaceSetList[3], FS->FaceSetList[4], FS->FaceSetList[5]);
		fprintf (Out, "Last 2 polygons:\n\t[parent ID:%d]%d %d %d\n\t[parent ID:%d]%d %d %d\n", \
			FS->FaceSetIndexInParent[FS->N_FaceSet-2], FS->FaceSetList[3 * (FS->N_FaceSet-2)], \
			FS->FaceSetList[3 * (FS->N_FaceSet-2) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-2) + 2], \
			FS->FaceSetIndexInParent[FS->N_FaceSet-1], FS->FaceSetList[3 * (FS->N_FaceSet-1)], \
			FS->FaceSetList[3 * (FS->N_FaceSet-1) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-1) + 2]);
	}
	SUMA_RETURNe;

}

#ifdef SUMA_FreeSurfer_STAND_ALONE

SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs stored in SVv */
SUMA_DO *SUMAg_DOv;	/*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF;

void usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_FreeSurfer f_name \n");
          printf ("\t ..... \n\n");
          printf ("\t To Compile:\ngcc -DSUMA_FreeSurfer_STAND_ALONE -Wall -o $1 $1.c -I./ -I//usr/X11R6/include SUMA_lib.a\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \tFri Feb 8 16:29:06 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
   char FS_name[200];
	SUMA_FreeSurfer_struct *FS;
	
   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_FreeSurfer-Main-");
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
	
	/* Allocate for FS */
	FS = (SUMA_FreeSurfer_struct *) SUMA_malloc(sizeof(SUMA_FreeSurfer_struct));	
	if (FS == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for FS\n", FuncName);
		exit(1);
	}
   
   if (argc < 2)
       {
          usage ();
          exit (1);
       }
   
	sprintf(FS_name, "%s", argv[1]);
	if (!SUMA_FreeSurfer_Read (FS_name, FS)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_FreeSurfer_Read\n", FuncName);
		exit(1);
	}
	
	
	SUMA_Show_FreeSurfer (FS, NULL);
	fprintf(stdout, "freeing ..\n");
	if (!SUMA_Free_FreeSurfer (FS)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Free_FreeSurfer.\n", FuncName);
		exit(1);
	}
	
	if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

	return (0);
}/* Main */
#endif
