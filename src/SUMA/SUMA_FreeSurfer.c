#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <math.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <GL/GLwMDrawA.h>  /* Motif OpenGL drawing area. */

#include "SUMA_suma.h"
 
 
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
	int ex, cnt, jnki, amax[3], maxamax;
	float jnkf, **NodeList;
	char c;
	static char FuncName[]={"SUMA_FreeSurfer_Read"};
	   
	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"Error %s: File %s does not exist or cannot be read.\n", FuncName, f_name);
		return (NOPE);
	}else {
		fprintf(SUMA_STDERR,"%s: File %s exists and will be read.\n", FuncName, f_name);
	}
	
	
	/* start reading */
	fs_file = fopen (f_name,"r");
	if (fs_file == NULL)
		{
			SUMA_error_message (FuncName,"Could not open input file ",0);
			return (NOPE);
		}

	sprintf(FS->name, "%s", f_name);
	
	/* read first character and check if it is a comment */
	ex = fscanf (fs_file,"%c",&c);
	if (c == '#') {
		/*fprintf (SUMA_STDOUT, "Found comment\n");*/
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
				return (NOPE);
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
	
	/* allocate space for NodeList and FaceSetList */
	FS->NodeList = (float **)SUMA_allocate2D(FS->N_Node, 3, sizeof(float));
	FS->FaceSetList = (int **)SUMA_allocate2D(FS->N_FaceSet, 3, sizeof(int));
	FS->NodeId = (int *)calloc(FS->N_Node, sizeof(int));
	if (FS->NodeList == NULL || FS->FaceSetList == NULL || FS->NodeId == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Could not allocate for FS->NodeList &/| FS->FaceSetList &/| FS->NodeId\n", FuncName);
		return (NOPE);
	} 
	if (FS->isPatch) {
		FS->FaceSetIndexInParent = (int *)calloc(FS->N_FaceSet, sizeof(int));
		if (FS->FaceSetIndexInParent == NULL) {
			fprintf(SUMA_STDERR,"Error %s: Could not allocate for FS->FaceSetIndexInParent\n", FuncName);
			return (NOPE);
		}
	} else {
		FS->FaceSetIndexInParent = NULL;
	}
	
	if (!FS->isPatch) {
		/* read in the nodes */
		cnt = 0;
		while (ex != EOF && cnt < FS->N_Node) {
			FS->NodeId[cnt] = cnt;
			ex = fscanf(fs_file, "%f %f %f %f", &(FS->NodeList[cnt][0]), &(FS->NodeList[cnt][1]),&(FS->NodeList[cnt][2]), &jnkf);
			++cnt;
		}
		if (cnt != FS->N_Node) {
			fprintf(SUMA_STDERR,"Error %s: Expected %d nodes, %d read.\n", FuncName, FS->N_Node, cnt);
			return (NOPE);
		}

		/* read in the facesets */
		cnt = 0;
		while (ex != EOF && cnt < FS->N_FaceSet) {
			ex = fscanf(fs_file, "%d %d %d %d", &(FS->FaceSetList[cnt][0]), &(FS->FaceSetList[cnt][1]),&(FS->FaceSetList[cnt][2]), &jnki);
			++cnt;
		}
		if (cnt != FS->N_FaceSet) {
			fprintf(SUMA_STDERR,"Error %s: Expected %d FaceSets, %d read.\n", FuncName, FS->N_FaceSet, cnt);
			return (NOPE);
		}
	} /* read a full surface */
	else { /* that's a patch */
		/* Node IDs are a reference to those in the parent surface */
		cnt = 0;
		while (ex != EOF && cnt < FS->N_Node) {
			ex = fscanf(fs_file, "%d", &(FS->NodeId[cnt]));
			ex = fscanf(fs_file, "%f %f %f", &(FS->NodeList[cnt][0]), &(FS->NodeList[cnt][1]),&(FS->NodeList[cnt][2]));
			++cnt;
		}
		if (cnt != FS->N_Node) {
			fprintf(SUMA_STDERR,"Error %s: Expected %d nodes, %d read.\n", FuncName, FS->N_Node, cnt);
			return (NOPE);
		}
		
		/* read in the facesets */
		cnt = 0;
		while (ex != EOF && cnt < FS->N_FaceSet) {
			ex = fscanf(fs_file, "%d", &(FS->FaceSetIndexInParent[cnt]));
			ex = fscanf(fs_file, "%d %d %d",  &(FS->FaceSetList[cnt][0]), &(FS->FaceSetList[cnt][1]),&(FS->FaceSetList[cnt][2]));
			++cnt;
		}
		if (cnt != FS->N_FaceSet) {
			fprintf(SUMA_STDERR,"Error %s: Expected %d FaceSets, %d read.\n", FuncName, FS->N_FaceSet, cnt);
			return (NOPE);
		}
		/* The FaceSet List which will be read next, uses indices into the NodeList of the parent surface
		This means that it expects a NodeList of the size of the NodeList in the parent surface. 
		One could read the maximum number of nodes in the parent surface and create a NodeList of that size.
		However, that would require keeping track of the link between the patch file and the parent file.
		Instead, I will search through the FaceSetList for the highest index and allocate a new nodelist to match it*/

		SUMA_MAX_MAT_COL(FS->FaceSetList, FS->N_FaceSet, 3, amax);
		SUMA_MAX_VEC(amax,3,maxamax); ++maxamax;
		NodeList = (float **)SUMA_allocate2D(maxamax, 3, sizeof(float));
		if (NodeList == NULL)
		{
			fprintf(SUMA_STDERR,"Error %s: Could not allocate for NodeList\n", FuncName);
			return (NOPE);
		} 
		/*Now copy pertinent nodes into NodeList */
		for (cnt=0; cnt< FS->N_Node; ++cnt) {
			NodeList[FS->NodeId[cnt]][0] = FS->NodeList[cnt][0];
			NodeList[FS->NodeId[cnt]][1] = FS->NodeList[cnt][1];
			NodeList[FS->NodeId[cnt]][2] = FS->NodeList[cnt][2];
		}
		/* Now free FS->NodeList */
		SUMA_free2D((char **)FS->NodeList, FS->N_Node);
		/*make FS->NodeList be NodeList */
		FS->NodeList = NodeList;
		FS->N_Node = maxamax;
	} /* read a patch */
	
	fclose (fs_file);
	return(YUP);
	
}/* SUMA_FreeSurfer_Read*/

/*! 
	free memory allocated for FreeSurfer structure  
*/
SUMA_Boolean SUMA_Free_FreeSurfer (SUMA_FreeSurfer_struct *FS)
{
	if (FS->FaceSetList != NULL) SUMA_free2D((char **)FS->FaceSetList, FS->N_FaceSet);
	if (FS->NodeList != NULL) SUMA_free2D((char **)FS->NodeList, FS->N_Node);
	if (FS->NodeId != NULL) free(FS->NodeId);
	if (FS->FaceSetIndexInParent != NULL) free(FS->FaceSetIndexInParent);
	if (FS != NULL) free (FS);
	return (YUP);
}

/*! 
	Show elements of FreeSurfer structure 
*/
void SUMA_Show_FreeSurfer (SUMA_FreeSurfer_struct *FS, FILE *Out)
{	
	if (Out == NULL) Out = SUMA_STDOUT;
	fprintf (Out, "Comment: %s\n", FS->comment);
	fprintf (Out, "N_Node %d\n", FS->N_Node);
	fprintf (Out, "First 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		FS->NodeId[0], FS->NodeList[0][0], FS->NodeList[0][1], FS->NodeList[0][2],
		FS->NodeId[1], FS->NodeList[1][0], FS->NodeList[1][1], FS->NodeList[1][2]);
	fprintf (Out, "Last 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		FS->NodeId[FS->N_Node-2], FS->NodeList[FS->N_Node-2][0], FS->NodeList[FS->N_Node-2][1], FS->NodeList[FS->N_Node-2][2],
		FS->NodeId[FS->N_Node-1], FS->NodeList[FS->N_Node-1][0], FS->NodeList[FS->N_Node-1][1], FS->NodeList[FS->N_Node-1][2]);
	fprintf (Out, "N_FaceSet %d\n", FS->N_FaceSet);
	if (!FS->isPatch) {
		fprintf (Out, "First 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
			FS->FaceSetList[0][0], FS->FaceSetList[0][1], FS->FaceSetList[0][2],
			FS->FaceSetList[1][0], FS->FaceSetList[1][1], FS->FaceSetList[1][2]);
		fprintf (Out, "Last 2 polygons:\n%d %d %d\n%d %d %d\n", \
			FS->FaceSetList[FS->N_FaceSet-2][0], FS->FaceSetList[FS->N_FaceSet-2][1], FS->FaceSetList[FS->N_FaceSet-2][2],
			FS->FaceSetList[FS->N_FaceSet-1][0], FS->FaceSetList[FS->N_FaceSet-1][1], FS->FaceSetList[FS->N_FaceSet-1][2]);
	} else {
		fprintf (Out, "First 2 polygons:\n\t[parent ID:%d] %d %d %d\n\t[parent ID:%d] %d %d %d\n", \
			FS->FaceSetIndexInParent[0], FS->FaceSetList[0][0], FS->FaceSetList[0][1], FS->FaceSetList[0][2],
			FS->FaceSetIndexInParent[1], FS->FaceSetList[1][0], FS->FaceSetList[1][1], FS->FaceSetList[1][2]);
		fprintf (Out, "Last 2 polygons:\n\t[parent ID:%d]%d %d %d\n\t[parent ID:%d]%d %d %d\n", \
			FS->FaceSetIndexInParent[FS->N_FaceSet-2], FS->FaceSetList[FS->N_FaceSet-2][0], FS->FaceSetList[FS->N_FaceSet-2][1], FS->FaceSetList[FS->N_FaceSet-2][2],
			FS->FaceSetIndexInParent[FS->N_FaceSet-1], FS->FaceSetList[FS->N_FaceSet-1][0], FS->FaceSetList[FS->N_FaceSet-1][1], FS->FaceSetList[FS->N_FaceSet-1][2]);
	}
	return;

}

#ifdef STAND_ALONE
void usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_FreeSurfer f_name \n");
          printf ("\t ..... \n\n");
          printf ("\t To Compile:\ngcc -DSTAND_ALONE -Wall -o $1 $1.c -I./ -I//usr/X11R6/include SUMA_lib.a\n");
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
   
	/* Allocate for FS */
	FS = (SUMA_FreeSurfer_struct *) malloc(sizeof(SUMA_FreeSurfer_struct));	
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
	
	return (0);
}/* Main */
#endif
