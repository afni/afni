
#include "SUMA_suma.h"
#include "PLY/ply.h"

#undef STAND_ALONE

#if defined SUMA_SureFit_STAND_ALONE
#define STAND_ALONE 
#elif defined SUMA_FreeSurfer_STAND_ALONE
#define STAND_ALONE
#elif defined SUMA_Ply_Read_STAND_ALONE
#define STAND_ALONE
#elif defined SUMA_ConvertSurface_STAND_ALONE
#define STAND_ALONE
#elif defined SUMA_ROI2dataset_STAND_ALONE
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
   
/* CODE */
   
   
   
/*!**  
Function: SUMA_SureFit_Read_Coord 
Usage : 
Ret = SUMA_SureFit_Read_Coord (coordname, SureFit)
   
   
Input paramters : 
\param coordname (char *) name of .coord file
\param SureFit (SUMA_SureFit_struct *) pointer to the SureFit structure
   
Returns : 
\return  (SUMA_Boolean) YUP/NOPE for success/failure
   
Support : 
\sa   
\sa   
   
Side effects : 
   
   
   
***/
SUMA_Boolean SUMA_SureFit_Read_Coord (char * f_name, SUMA_SureFit_struct *SF)
{/*SUMA_SureFit_Read_Coord*/
   static char FuncName[]={"SUMA_SureFit_Read_Coord"}; 
   FILE *sf_file;
	int ex, EndHead, FoundHead, evl, cnt, skp, ND, id;
	char stmp[100], head_strt[100], head_end[100], s[1000], delimstr[] = {' ', '\0'}, *st;
	
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
	
	ND = 3;
	
	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"File %s does not exist or cannot be read.\n", f_name);
		SUMA_RETURN (NOPE);
	}
	
	sprintf(SF->name_coord, "%s", f_name);
	
	/* start reading */
	sf_file = fopen (f_name,"r");
	if (sf_file == NULL)
		{
			SUMA_error_message (FuncName,"Could not open input file ",0);
			SUMA_RETURN (NOPE);
		}

	/* read until you reach the begin header BeginHeader */
	ex = 1;
	FoundHead = 0;
	sprintf(head_strt,"BeginHeader");
	while (ex != EOF && !FoundHead)
	{
		ex = fscanf (sf_file,"%s",s);
		if (strlen (s) >= strlen(head_strt)) 
			{
				evl = SUMA_iswordin (s,head_strt);
				if (evl == 1) FoundHead = 1;
			}
	}
	
	if (!FoundHead) {
		fprintf(SUMA_STDERR,"Error %s: BeginHeader not found in %s.\nPerhaps you are using old versions of Caret/SureFit files.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}
	EndHead = 0;
	sprintf(head_end,"EndHeader");
	sprintf(delimstr," ");
	
	while (ex != EOF && !EndHead)	{
		ex = fscanf (sf_file,"%s",s);
		/*fprintf(stdout,"%s\n", s);*/
		if (strlen (s) >= strlen(head_end)) 
			{
				evl = SUMA_iswordin (s,head_end);
				if (evl == 1) EndHead = 1;
			}
		/* look for some tokens */
		skp = 0;
		if (!EndHead) {
			st = strtok(s, delimstr);
			sprintf(stmp,"encoding");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found encoding\n");*/
				ex = fscanf (sf_file,"%s",(SF->encoding_coord));
				skp = 1;
			}
			
			sprintf(stmp,"configuration_id");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found configuration_id\n");*/
				ex = fscanf (sf_file,"%s",(SF->configuration_id));
				skp = 1;
			}
			
			sprintf(stmp,"coordframe_id");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found configuration_id\n");*/
				ex = fscanf (sf_file,"%s",(SF->coordframe_id));
				skp = 1;
			}

		}
	}
	/* Now read the Number of Nodes */
	fscanf(sf_file, "%d", &SF->N_Node);
	/*fprintf (stdout,"Expecting %d nodes.\n", SF->N_Node);*/
	
	/* allocate space */
	SF->NodeList = (float *)SUMA_calloc(SF->N_Node * ND, sizeof(float));
	SF->NodeId = (int *)SUMA_calloc (SF->N_Node, sizeof(int));
	
	if (SF->NodeList == NULL || SF->NodeId == NULL) {
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space for NodeList &/| NodeId.\n", FuncName);
		SUMA_RETURN (NOPE);
	}
	
	/* Now read the nodes until the end of the file */
		cnt = 0;
		while (ex != EOF && cnt < SF->N_Node)	{
			id = cnt * ND;
			ex = fscanf (sf_file,"%d %f %f %f",&(SF->NodeId[cnt]), \
					&(SF->NodeList[id]), &(SF->NodeList[id+1]), &(SF->NodeList[id+2]));
			++cnt;
		}
	if (cnt != SF->N_Node) {
		fprintf(SUMA_STDERR, "Error %s: Expecting %d Nodes, read %d.\n", FuncName, SF->N_Node, cnt);
		SUMA_RETURN (NOPE);
	}
	fclose (sf_file);
	SUMA_RETURN (YUP); 
}/*SUMA_SureFit_Read_Coord*/

SUMA_Boolean SUMA_SureFit_Read_Topo (char * f_name, SUMA_SureFit_struct *SF)
{/*SUMA_SureFit_Read_Topo*/
	static char FuncName[]={"SUMA_SureFit_Read_Topo"}; 
   FILE *sf_file;
	int ex, EndHead, FoundHead, evl, cnt, skp, jnk, i, ip, NP;
	char stmp[100], head_strt[100], head_end[100], s[1000], delimstr[] = {' ', '\0'}, *st;
	
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"File %s does not exist or cannot be read.\n", f_name);
		SUMA_RETURN (NOPE);
	}
	
	sprintf(SF->name_topo, "%s", f_name);
	
	/* start reading */
	sf_file = fopen (f_name,"r");
	if (sf_file == NULL)
		{
			SUMA_error_message (FuncName,"Could not open input file ",0);
			SUMA_RETURN (NOPE);
		}

	/* read until you reach the begin header BeginHeader */
	ex = 1;
	FoundHead = 0;
	sprintf(head_strt,"BeginHeader");
	while (ex != EOF && !FoundHead)
	{
		ex = fscanf (sf_file,"%s",s);
		if (strlen (s) >= strlen(head_strt)) 
			{
				evl = SUMA_iswordin (s,head_strt);
				if (evl == 1) FoundHead = 1;
			}
	}
	if (!FoundHead) {
		fprintf(SUMA_STDERR,"Error %s: BeginHeader not found in %s.\nPerhaps you are using old versions of Caret/SureFit files.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}
	EndHead = 0;
	sprintf(head_end,"EndHeader");
	sprintf(delimstr," ");
	
	while (ex != EOF && !EndHead)	{
		ex = fscanf (sf_file,"%s",s);
		/*fprintf(stdout,"%s\n", s);*/
		if (strlen (s) >= strlen(head_end)) 
			{
				evl = SUMA_iswordin (s,head_end);
				if (evl == 1) EndHead = 1;
			}
		/* look for some tokens */
		skp = 0;
		if (!EndHead) {
			st = strtok(s, delimstr);
			sprintf(stmp,"encoding");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found encoding\n");*/
				ex = fscanf (sf_file,"%s",(SF->encoding_topo));
				skp = 1;
			}
			
			sprintf(stmp,"perimeter_id");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found perimeter_id\n");*/
				ex = fscanf (sf_file,"%s",(SF->perimeter_id));
				skp = 1;
			}
			
			sprintf(stmp,"date");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found date\n");*/
				ex = fscanf (sf_file,"%s\n",(SF->date));
				skp = 1;
			}

		}
	}
	/* Now read the Number of Nodes Specs */
	fscanf(sf_file, "%d", &SF->N_Node_Specs);
	/*fprintf (stdout,"Expecting %d Node_Specs.\n", SF->N_Node_Specs);*/
	
	SF->FN.N_Node = SF->N_Node_Specs;
	SF->FN.N_Neighb_max = 0;

	/* allocate for Node Specs Matrix and First_Neighb structure*/
	SF->Specs_mat = (int **) SUMA_allocate2D(SF->N_Node_Specs, 6, sizeof(int));
	/*assume maximum number of neighbors is SUMA_MAX_NUMBER_NODE_NEIGHB */
	SF->FN.FirstNeighb = (int **) SUMA_allocate2D(SF->FN.N_Node, SUMA_MAX_NUMBER_NODE_NEIGHB, sizeof (int));
	SF->FN.N_Neighb = (int *) SUMA_calloc (SF->FN.N_Node, sizeof(int));
	SF->FN.NodeId = (int *) SUMA_calloc (SF->FN.N_Node, sizeof(int));
	
	if (SF->Specs_mat == NULL || SF->FN.FirstNeighb == NULL || SF->FN.N_Neighb == NULL || SF->FN.NodeId == NULL ){
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space for SF->Specs_mat &/| SF->FN.FirstNeighb &/| SF->FN.N_Neighb &/| SF->FN.NodeId.\n", FuncName);
		SUMA_RETURN (NOPE);
	} 
	
	/* Now read the node specs */
	/*fprintf (stdout,"About to read specs\n");*/
	cnt = 0;
	while (ex != EOF && cnt < SF->N_Node_Specs)	{
		ex = fscanf (sf_file,"%d %d %d %d %d %d",&(SF->Specs_mat[cnt][0]), &(SF->Specs_mat[cnt][1]), \
				&(SF->Specs_mat[cnt][2]), &(SF->Specs_mat[cnt][3]), &(SF->Specs_mat[cnt][4]), &(SF->Specs_mat[cnt][5]));
		SF->FN.NodeId[cnt] = SF->Specs_mat[cnt][0];
		SF->FN.N_Neighb[cnt] = SF->Specs_mat[cnt][1];
		if (SF->FN.N_Neighb[cnt] > SUMA_MAX_NUMBER_NODE_NEIGHB-1) {
			fprintf (SUMA_STDERR,"Error %s: Node %d has more neighbors (%d) than the maximum allowed (%d)\n", \
				FuncName, SF->FN.NodeId[cnt], SF->FN.N_Neighb[cnt], SUMA_MAX_NUMBER_NODE_NEIGHB-1);
			SUMA_RETURN (NOPE);
		}
		if (SF->FN.N_Neighb[cnt] > SF->FN.N_Neighb_max) SF->FN.N_Neighb_max = SF->FN.N_Neighb[cnt];
		
		/* Now Read in the Neighbors info */
		for (i=0; i < SF->FN.N_Neighb[cnt]; ++ i) {
			ex = fscanf (sf_file,"%d %d", &jnk, &(SF->FN.FirstNeighb[cnt][i]));
		}
		/* seal with -1 */
		SF->FN.FirstNeighb[cnt][SF->FN.N_Neighb[cnt]] = -1;
		
		++cnt;
	}
	if (cnt != SF->N_Node_Specs) {
		fprintf(SUMA_STDERR, "Error %s: Expecting %d NodeSpecs, read %d.\n", FuncName, SF->N_Node_Specs, cnt);
		SUMA_RETURN (NOPE);
	}
	/*fprintf (stdout, "Done with Node Specs.\n");*/
	ex = fscanf (sf_file,"%d", &(SF->N_FaceSet));
	/*fprintf (stdout, "Expecting to read %d facesets.\n", SF->N_FaceSet);*/
	
	NP = 3;
	SF->FaceSetList = (int *) SUMA_calloc(SF->N_FaceSet * 3, sizeof(int));
	if (SF->FaceSetList == NULL){
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space for SF->FaceSetList.\n", FuncName);
		SUMA_RETURN (NOPE);
	} 
	
	/*fprintf (stdout,"About to read FaceSets\n");*/
	cnt = 0;
	while (ex != EOF && cnt < SF->N_FaceSet)	{
		ip = NP * cnt;
		ex = fscanf (sf_file,"%d %d %d ",&(SF->FaceSetList[ip]), &(SF->FaceSetList[ip+1]), \
				&(SF->FaceSetList[ip+2]));
		++cnt;
	}
	if (cnt != SF->N_FaceSet) {
		fprintf(SUMA_STDERR, "Error %s: Expecting %d FaceSets, read %d.\n", FuncName, SF->N_FaceSet, cnt);
		SUMA_RETURN (NOPE);
	}
	fclose (sf_file);
	
SUMA_RETURN (YUP);
}/*SUMA_SureFit_Read_Topo*/

/*!
Show data structure containing SureFit surface object
*/
void SUMA_Show_SureFit (SUMA_SureFit_struct *SF, FILE *Out)
{	int cnt, id, ND, NP;
	static char FuncName[]={"SUMA_Show_SureFit"};
	
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	ND = 3;
	NP = 3;
	if (Out == NULL) Out = SUMA_STDOUT;
	fprintf (Out, "\n%s: Coord Info\n", SF->name_coord);
	fprintf (Out, "N_Node %d\n", SF->N_Node);
	fprintf (Out, "encoding_coord: %s\nconfiguration id: %s, coordframe_id: %s\n", SF->encoding_coord,SF->configuration_id, SF->coordframe_id);
	fprintf (Out, "First 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		SF->NodeId[0], SF->NodeList[0], SF->NodeList[1], SF->NodeList[2],
		SF->NodeId[1], SF->NodeList[3], SF->NodeList[4], SF->NodeList[5]);
	if (SF->N_Node > 2) {
      fprintf (Out, "Last 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		   SF->NodeId[SF->N_Node-2], SF->NodeList[ND*(SF->N_Node-2)], SF->NodeList[ND*(SF->N_Node-2)+1], SF->NodeList[ND*(SF->N_Node-2)+2],
		   SF->NodeId[SF->N_Node-1], SF->NodeList[ND*(SF->N_Node-1)], SF->NodeList[ND*(SF->N_Node-1)+1], SF->NodeList[ND*(SF->N_Node-1)+2]);
	}
   fprintf (Out, "\n%s: Topo Info\n", SF->name_topo);
	fprintf (Out, "N_Node_Specs %d\n", SF->N_Node_Specs);
	fprintf (Out, "ecnoding_topo: %s, date %s\n",  SF->encoding_topo, SF->date);
	fprintf (Out, "N_FaceSet %d\n", SF->N_FaceSet);
	if (SF->N_FaceSet > 2) {
	   fprintf (Out, "First 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
		   SF->FaceSetList[0], SF->FaceSetList[1], SF->FaceSetList[2],
		   SF->FaceSetList[3], SF->FaceSetList[4], SF->FaceSetList[5]);
      fprintf (Out, "Last 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
		   SF->FaceSetList[NP*(SF->N_FaceSet-2)], SF->FaceSetList[NP*(SF->N_FaceSet-2) + 1], SF->FaceSetList[NP*(SF->N_FaceSet-2) + 2],
		   SF->FaceSetList[NP*(SF->N_FaceSet-1)], SF->FaceSetList[NP*(SF->N_FaceSet-1) + 1], SF->FaceSetList[NP*(SF->N_FaceSet-1) + 2]);
	} else {
      fprintf (Out, "First polygon:\n\t%d %d %d\n", \
		   SF->FaceSetList[0], SF->FaceSetList[1], SF->FaceSetList[2] );
   }
   fprintf (Out, "\nNode Specs (%d):\n", SF->N_Node_Specs);
	fprintf (Out, "First Entry: \t%d %d %d %d %d %d\n", \
	SF->Specs_mat[0][0], SF->Specs_mat[0][1],SF->Specs_mat[0][2], SF->Specs_mat[0][3],SF->Specs_mat[0][4], SF->Specs_mat[0][5]);
	cnt = 0;
	while (cnt < SF->FN.N_Neighb[0]) {
		fprintf (Out, "\t%d %d\n", cnt, SF->FN.FirstNeighb[0][cnt]); 
		++cnt;
	}
	fprintf (Out, "Last Entry: \t%d %d %d %d %d %d\n", \
		SF->Specs_mat[SF->N_Node_Specs-1][0], SF->Specs_mat[SF->N_Node_Specs-1][1],SF->Specs_mat[SF->N_Node_Specs-1][2],\
		SF->Specs_mat[SF->N_Node_Specs-1][3],SF->Specs_mat[SF->N_Node_Specs-1][4], SF->Specs_mat[SF->N_Node_Specs-1][5]);
	cnt = 0;
	while (cnt < SF->FN.N_Neighb[SF->N_Node_Specs-1]) {
		fprintf (Out, "\t%d %d\n", cnt, SF->FN.FirstNeighb[SF->N_Node_Specs-1][cnt]); 
		++cnt;
	}

	SUMA_RETURNe;
}

/*!
   \brief writes a surface in SureFit format
   ans = SUMA_SureFit_Write (Fname,SO);
   
   \param  Fname (SUMA_SFname *) uses the SureFit filename structure to store
                                 the names (and paths) of the NodeList (name_coord)
                                 and the FaceSetList (name_topo) files.
                                 If strlen(name_coord) == 0 then the coord file is not
                                 written out. 
                                 If strlen(name_topo) == 0 then topo file is not written out
   \param SO (SUMA_SurfaceObject *) pointer to SO structure.
   \return YUP/NOPE
   
   \sa SUMA_SureFit_Read_Topo() 
   \sa SUMA_SureFit_Read_Coord() 
   
   The function will not overwrite pre-existing files.
   

NOTE: Header info is incomplete. 
for .coord:
BeginHeader
configuration_id NA
coordframe_id NA
encoding ASCII
EndHeader

for .topo:
BeginHeader
date NA
encoding ASCII
perimeter_id NA
EndHeader

also, the last 4 integers in the node neighbor list lines are set to 0. I have never used them and do not know what they are for.

The naming convention of SureFit surfaces is not enforced.

*/  
SUMA_Boolean SUMA_SureFit_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO)
{
   
   static char FuncName[]={"SUMA_SureFit_Write"};
   int i, j;
   FILE *outFile = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (strlen(Fname->name_coord)) {
      if (SUMA_filexists(Fname->name_coord)) {
         fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_coord);
         SUMA_RETURN (NOPE);
      }
   }
   
   if (strlen(Fname->name_topo)) {
      if (SUMA_filexists(Fname->name_topo)) {
         fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_topo);
         SUMA_RETURN (NOPE);
      }
   }
   
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: Must have NodeDim and FaceSetDim = 3.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (strlen(Fname->name_coord)) {
      outFile = fopen(Fname->name_coord, "w");
      if (!outFile) {
         fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, Fname->name_coord);
         SUMA_RETURN (NOPE);
      }

      /* write header */
      fprintf (outFile,"BeginHeader\nconfiguration_id NA\ncoordframe_id NA\nencoding ASCII\nEndHeader\n");
      fprintf (outFile,"%d\n", SO->N_Node);

      j=0;
      for (i=0; i<SO->N_Node; ++i) {
         j=SO->NodeDim * i;
         fprintf (outFile, "%d %f %f %f\n", i, SO->NodeList[j], SO->NodeList[j+1], SO->NodeList[j+2]);
      }

      fclose (outFile);
   }
   
   if (strlen(Fname->name_topo)) {
      outFile = fopen(Fname->name_topo, "w");
      if (!outFile) {
         fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, Fname->name_topo);
         SUMA_RETURN (NOPE);
      }

      /* make sure you have the first neighbor list ! */
      if (!SO->FN) {
         fprintf (SUMA_STDERR, "%s: Must compute Node Neighborhood list.\n", FuncName);
         if (!SUMA_SurfaceMetrics(SO, "EdgeList", NULL)){
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         #if 0 /* better to use SUMA_SurfaceMetrics */
         if (!SO->EL) {
            fprintf (SUMA_STDERR, "%s: Computing Edge List...\n", FuncName);
            SO->EL = SUMA_Make_Edge_List (SO->FaceSetList, SO->N_FaceSet, SO->N_Node, SO->NodeList);
         }
         if (!SO->EL) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Make_Edge_List.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         fprintf (SUMA_STDERR, "%s: Computing FirstNeighb list.\n", FuncName);
         SO->FN = SUMA_Build_FirstNeighb (SO->EL, SO->N_Node);
         if (!SO->FN) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Build_FirstNeighb.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         #endif

      }
      /* write header */
      fprintf (outFile,"BeginHeader\ndate NA\nencoding ASCII\nperimeter_id NA\nEndHeader\n");
      fprintf (outFile,"%d\n", SO->N_Node);
	   j = 0;
	   while (j < SO->FN->N_Node)	{
         /* dunno what last 4 ints of upcoming line are */
		   fprintf (outFile,"%d %d 0 0 0 0\n", SO->FN->NodeId[j], SO->FN->N_Neighb[j]);

		   /* Now write the Neighbors info */
		   for (i=0; i < SO->FN->N_Neighb[j]; ++ i) {
			   fprintf (outFile,"%d %d\n", i, SO->FN->FirstNeighb[j][i]);
		   }
		   ++j;
	   }

	   fprintf (outFile,"%d\n", SO->N_FaceSet);

      j=0;
      for (i=0; i<SO->N_FaceSet; ++i) {
         j = SO->FaceSetDim * i;
         fprintf (outFile, "%d %d %d\n", SO->FaceSetList[j], SO->FaceSetList[j+1], SO->FaceSetList[j+2]);
      }

      fclose (outFile);
   }
   SUMA_RETURN (YUP);

}

/*!
free data structure containing SureFit surface object
*/
SUMA_Boolean SUMA_Free_SureFit (SUMA_SureFit_struct *SF)  
{
	static char FuncName[]={"SUMA_Free_SureFit"};
	
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	if (SF->NodeList != NULL) SUMA_free(SF->NodeList);
	if (SF->NodeId != NULL) SUMA_free(SF->NodeId);
	if (SF->Specs_mat != NULL) SUMA_free2D ((char **)SF->Specs_mat, SF->N_Node_Specs);
	if (SF->FN.FirstNeighb != NULL) SUMA_free2D((char **)SF->FN.FirstNeighb, SF->FN.N_Node);
	if (SF->FN.N_Neighb != NULL) SUMA_free(SF->FN.N_Neighb);
	if (SF->FN.NodeId != NULL) SUMA_free(SF->FN.NodeId);
	if (SF->FaceSetList != NULL) SUMA_free(SF->FaceSetList);
	if (SF!= NULL) SUMA_free(SF);
	
	SUMA_RETURN (YUP);
}

/*!
Read in some parameters from a .param file
*/
SUMA_Boolean SUMA_Read_SureFit_Param (char *f_name, SUMA_SureFit_struct *SF)
{
	static char FuncName[]={"SUMA_Read_SureFit_Param"};
	int ex, evl; 
   FILE *sf_file;
	SUMA_Boolean Done;
	char delimstr[] = {' ', '\0'}, stmp[100], s[1000], *st;

	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"File %s does not exist or cannot be read.\n", f_name);
		SUMA_RETURN (NOPE);
	}

	sprintf(SF->name_param, "%s", f_name);

	/* start reading */
	sf_file = fopen (f_name,"r");
	if (sf_file == NULL)
		{
			fprintf (SUMA_STDERR,"Error %s: Could not open input file ",FuncName);
			SUMA_RETURN (NOPE);
		}

	/* read until you reach something you like */
	ex = 1;
	Done = NOPE;			
	sprintf(delimstr,"=");
	while (ex != EOF && !Done)
	{
		ex = fscanf (sf_file,"%s",s);
		
		sprintf(stmp,"ACx_WholeVolume");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACx_WholeVolume */
			/*fprintf(SUMA_STDOUT, "Found ACx_WholeVolume:");*/
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			SF->AC_WholeVolume[0] = atof(st);
			/*fprintf(SUMA_STDOUT, " %f\n", SF->AC_WholeVolume[0]);*/
			continue;
		}
		sprintf(stmp,"ACy_WholeVolume");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACy_WholeVolume */
			/*fprintf(SUMA_STDOUT, "Found ACy_WholeVolume:");*/
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			SF->AC_WholeVolume[1] = atof(st);
			/*fprintf(SUMA_STDOUT, " %f\n", SF->AC_WholeVolume[1]);*/
			continue;
		}
		sprintf(stmp,"ACz_WholeVolume");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACz_WholeVolume */
			/*fprintf(SUMA_STDOUT, "Found ACz_WholeVolume:");*/
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			SF->AC_WholeVolume[2] = atof(st);
			/*fprintf(SUMA_STDOUT, " %f\n", SF->AC_WholeVolume[2]);*/
			continue;
		}
		 
		sprintf(stmp,"ACx");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACx */
			/*fprintf(SUMA_STDOUT, "Found ACx:");*/
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			SF->AC[0] = atof(st);
			/*fprintf(SUMA_STDOUT, " %f\n", SF->AC[0]);*/
			continue;
		}
		sprintf(stmp,"ACy");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACy */
			/*fprintf(SUMA_STDOUT, "Found ACy:");*/
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			SF->AC[1] = atof(st);
			/*fprintf(SUMA_STDOUT, " %f\n", SF->AC[1]);*/
			continue;
		}
		sprintf(stmp,"ACz");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACz */
			/*fprintf(SUMA_STDOUT, "Found ACz:");*/
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			SF->AC[2] = atof(st);
			/*fprintf(SUMA_STDOUT, " %f\n", SF->AC[2]);*/
			continue;
		}
		
	}
	
	fclose(sf_file);
	
	/* Sanity Checks */
	if (SF->AC[0] == 0.0 && SF->AC[1] == 0.0 && SF->AC[2] == 0.0) {
		fprintf (SUMA_STDERR,"Error %s: All values for AC are 0.0. Check your params file.\n", FuncName);
		SUMA_RETURN (NOPE);
	}
	
	if (SF->AC_WholeVolume[0] == 0.0 && SF->AC_WholeVolume[1] == 0.0 && SF->AC_WholeVolume[2] == 0.0) {
		fprintf (SUMA_STDERR,"Error %s: All values for AC_WholeVolume are 0.0. Check your params file.\n", FuncName);
		SUMA_RETURN (NOPE);
	}
	
	if (SF->AC[0] == SF->AC_WholeVolume[0] && SF->AC[1] == SF->AC_WholeVolume[1] && SF->AC[2] == SF->AC_WholeVolume[2])
	{
		SUMA_SL_Warn("Idetincal values for AC and AC_WholeVolume.\nCheck your params file if not using Talairach-ed surfaces.\n");
      /* looks like that's OK for TLRC surfaces ...*/
      /*
      fprintf (SUMA_STDERR,"Error %s: Idetincal values for AC and AC_WholeVolume. Check your params file.\n", FuncName);
		SUMA_RETURN (NOPE);
      */
	}
	if (SF->AC[0] < 0 || SF->AC[1] < 0 || SF->AC[2] < 0 || SF->AC_WholeVolume[0] < 0 || SF->AC_WholeVolume[1] < 0 || SF->AC_WholeVolume[2] < 0) 
	{
		fprintf (SUMA_STDERR,"Error %s: Negative values in AC or AC_WholeVolume. Check you params file.\n", FuncName);
		SUMA_RETURN (NOPE);
	}
	
	SUMA_RETURN (YUP);
}
#ifdef SUMA_SureFit_STAND_ALONE



void usage_SUMA_SureFit_Main ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_SureFit CoordRoot TopoRoot \n");
          printf ("\t ..... \n\n");
          printf ("\t To Compile:\ngcc -DSUMA_SureFit_STAND_ALONE -Wall -o $1 $1.c -I./ -I//usr/X11R6/include SUMA_lib.a\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \tFri Feb 8 16:29:06 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_SureFit_Main"}; 
   char SF_name[200];
	SUMA_SureFit_struct *SF;
	
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
	
	/* Allocate for SF */
	SF = (SUMA_SureFit_struct *) SUMA_malloc(sizeof(SUMA_SureFit_struct));	
	if (SF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for SF\n", FuncName);
		exit(1);
	}
   
   if (argc < 3)
       {
          usage_SUMA_SureFit_Main ();
          exit (1);
       }
   
	sprintf(SF_name, "%s.coord", argv[1]);
	if (!SUMA_SureFit_Read_Coord (SF_name, SF)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SureFit_Read_Coord.\n", FuncName);
		exit(1);
	}
	
	sprintf(SF_name, "%s.topo", argv[2]);
	if (!SUMA_SureFit_Read_Topo (SF_name, SF)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SureFit_Read_Topo.\n", FuncName);
		exit(1);
	}
	
	SUMA_Show_SureFit (SF, NULL);
	fprintf(stdout, "freeing ..\n");
	if (!SUMA_Free_SureFit (SF)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Free_SureFit.\n", FuncName);
		exit(1);
	}
	
	if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

	SUMA_RETURN (0);
}/* Main */
#endif


/*! Functions to read and manipulate FreeSurfer surfaces*/

/* CODE */
   
/* just call engine with debug set                20 Oct 2003 [rickr] */
SUMA_Boolean SUMA_FreeSurfer_Read (char * f_name, SUMA_FreeSurfer_struct *FS)
{/* SUMA_FreeSurfer_Read */
   static char FuncName[]={"SUMA_FreeSurfer_Read"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SUMA_RETURN(SUMA_FreeSurfer_Read_eng(f_name, FS, 1));
}/* SUMA_FreeSurfer_Read */

   
/* - changed function name to XXX_eng             20 Oct 2003 [rickr]
 * - added debug parameter
 * - print non-error info only if debug
*/
/*!**  
Usage : 
Ret = SUMA_FreeSurfer_Read_eng (surfname, FreeSurfer, debug)
	
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

\param debug (int) flag specifying whether to output non-error info
   
Returns : 
\return  (SUMA_Boolean) YUP/NOPE for success/failure
   
Support : 
\sa   LoadFreeSurf.m
\sa   
   
Side effects : 
   
   
   
***/
SUMA_Boolean SUMA_FreeSurfer_Read_eng (char * f_name, SUMA_FreeSurfer_struct *FS, int debug)
{/*SUMA_FreeSurfer_Read_eng*/
   char stmp[50]; 
   FILE *fs_file;
	int ex, cnt, jnki, amax[3], maxamax, maxamax2, id, ND, id2, NP, ip, *NodeId;
	float jnkf, *NodeList;
	char c;
	static char FuncName[]={"SUMA_FreeSurfer_Read_eng"};
	SUMA_Boolean LocalHead = NOPE;
	   
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"Error %s: File %s does not exist or cannot be read.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}else if ( debug ) {
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
	
   if (FS->N_Node <= 0 || FS->N_FaceSet <= 0) {
      SUMA_SL_Crit("Trouble parsing FreeSurfer file.\nNull or negative number of nodes &/| facesets.\n");
      SUMA_RETURN(NOPE);
   }
   
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
		SUMA_free(FS->NodeList);
		SUMA_free(FS->NodeId);
      
		/*make FS->NodeList be NodeList */
		FS->NodeList = NodeList;
      FS->NodeId = NodeId;
		FS->N_Node = maxamax;
	} /* read a patch */
	
	fclose (fs_file);
	SUMA_RETURN (YUP);
	
}/* SUMA_FreeSurfer_Read_eng*/

/*! 
	free memory allocated for FreeSurfer structure  
*/
SUMA_Boolean SUMA_Free_FreeSurfer (SUMA_FreeSurfer_struct *FS)
{
	static char FuncName[]={"SUMA_Free_FreeSurfer"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	if (FS->FaceSetList != NULL) SUMA_free(FS->FaceSetList);
	if (FS->NodeList != NULL) SUMA_free(FS->NodeList);
	if (FS->NodeId != NULL) SUMA_free(FS->NodeId);
	if (FS->FaceSetIndexInParent != NULL) SUMA_free(FS->FaceSetIndexInParent);
	if (FS != NULL) SUMA_free(FS);
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
	if (FS->N_Node > 2) {
      fprintf (Out, "Last 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		   FS->NodeId[FS->N_Node-2], FS->NodeList[3*(FS->N_Node-2)], FS->NodeList[3*(FS->N_Node-2)+1], FS->NodeList[3*(FS->N_Node-2)+2],
		   FS->NodeId[FS->N_Node-1], FS->NodeList[3*(FS->N_Node-1)], FS->NodeList[3*(FS->N_Node-1)+1], FS->NodeList[3*(FS->N_Node-1)+2]);
   }
	fprintf (Out, "N_FaceSet %d\n", FS->N_FaceSet);
	if (!FS->isPatch) {
		if (FS->N_FaceSet > 2) {
        fprintf (Out, "First 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
			FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2],
			FS->FaceSetList[3], FS->FaceSetList[4], FS->FaceSetList[5]);
        fprintf (Out, "Last 2 polygons:\n%d %d %d\n%d %d %d\n", \
	   		FS->FaceSetList[3 * (FS->N_FaceSet-2)], FS->FaceSetList[3 * (FS->N_FaceSet-2) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-2) + 2],
		   	FS->FaceSetList[3 * (FS->N_FaceSet-1)], FS->FaceSetList[3 * (FS->N_FaceSet-1) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-1) + 2]);
      }else {
         fprintf (Out, "First polygon:\n\t%d %d %d\n", \
			FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2]);
      }
	} else {
		if (FS->N_FaceSet > 2) {
         fprintf (Out, "First 2 polygons:\n\t[parent ID:%d] %d %d %d\n\t[parent ID:%d] %d %d %d\n", \
		   	FS->FaceSetIndexInParent[0], FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2],
		   	FS->FaceSetIndexInParent[1], FS->FaceSetList[3], FS->FaceSetList[4], FS->FaceSetList[5]);
		   fprintf (Out, "Last 2 polygons:\n\t[parent ID:%d]%d %d %d\n\t[parent ID:%d]%d %d %d\n", \
		   	FS->FaceSetIndexInParent[FS->N_FaceSet-2], FS->FaceSetList[3 * (FS->N_FaceSet-2)], \
		   	FS->FaceSetList[3 * (FS->N_FaceSet-2) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-2) + 2], \
		   	FS->FaceSetIndexInParent[FS->N_FaceSet-1], FS->FaceSetList[3 * (FS->N_FaceSet-1)], \
		   	FS->FaceSetList[3 * (FS->N_FaceSet-1) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-1) + 2]);
      } else {
         fprintf (Out, "First polygon:\n\t[parent ID:%d] %d %d %d\n", \
		   	FS->FaceSetIndexInParent[0], FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2]);
      }
	}
	SUMA_RETURNe;

}

#ifdef SUMA_FreeSurfer_STAND_ALONE


void usage_SUMA_FreeSurfer_Main ()
   
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
          usage_SUMA_FreeSurfer_Main ();
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

/*** Code to read ply format data 
Ply functions are based on code by Greg Turk. 
---------------------------------------------------------------

Copyright (c) 1994 The Board of Trustees of The Leland Stanford
Junior University.  All rights reserved.   
  
Permission to use, copy, modify and distribute this software and its   
documentation for any purpose is hereby granted without fee, provided   
that the above copyright notice and this permission notice appear in   
all copies of this software and that you do not sell the software.   
  
THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,   
EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY   
WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.   


*/

/* user's vertex and face definitions for a polygonal object */

typedef struct Vertex {
  float x,y,z;             /* the usual 3-space position of a vertex */
} Vertex;

typedef struct Face {
  unsigned char intensity; /* this user attaches intensity to faces */
  unsigned char nverts;    /* number of vertex indices in list */
  int *verts;              /* vertex index list */
} Face;

/* information needed to describe the user's data to the PLY routines */

PlyProperty vert_props[] = { /* list of property information for a vertex */
  {"x", PLY_FLOAT, PLY_FLOAT, offsetof(Vertex,x), 0, 0, 0, 0},
  {"y", PLY_FLOAT, PLY_FLOAT, offsetof(Vertex,y), 0, 0, 0, 0},
  {"z", PLY_FLOAT, PLY_FLOAT, offsetof(Vertex,z), 0, 0, 0, 0},
};

PlyProperty face_props[] = { /* list of property information for a vertex */
  {"intensity", PLY_UCHAR, PLY_UCHAR, offsetof(Face,intensity), 0, 0, 0, 0},
  {"vertex_indices", PLY_INT, PLY_INT, offsetof(Face,verts),
   1, PLY_UCHAR, PLY_UCHAR, offsetof(Face,nverts)},
};

/*!
   \brief Reads a Ply formatted file into a SUMA_SurfaceObject structure
   ans = SUMA_Ply_Read (f_name, SO);
   
   \param f_name (char *) name (and path) of .ply file to read. Extension .ply is optional
   \param SO (SUMA_SurfaceObject *) pointer to a structure to return surface in f_name in
   \return ans (SUMA_Boolean) YUP/NOPE
   
   The following fields are set in SO:
   SO->NodeDim
   SO->FaceSetDim
   SO->NodeList
   SO->FaceSetList
   SO->N_Node;
   SO->N_FaceSet;
   SO->Name;
   SO->FileType;
   SO->FileFormat
   
   \sa SUMA_Ply_Write()
   
   This function is a wrap around code by Greg Turk. 
   
*/
SUMA_Boolean SUMA_Ply_Read (char * f_name, SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_Ply_Read"};
   int i,j,k, j3, ji;
   PlyFile *ply = NULL;
   int nelems;
   char **elist = NULL;
   int file_type;
   float version;
   int nprops;
   int num_elems;
   PlyProperty **plist = NULL;
   Vertex **vlist = NULL;
   Face **flist = NULL;
   char *elem_name;
   int num_comments;
   char **comments = NULL;
   int num_obj_info;
   char **obj_info = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* open a PLY file for reading */
   ply = ply_open_for_reading(f_name, &nelems, &elist, &file_type, &version);
   if (!ply) {
      fprintf (SUMA_STDERR, "Error %s: Failed to find/read %s.\n", FuncName, f_name);
      SUMA_RETURN (NOPE);
   }
   
   /* print what we found out about the file */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: version %f\n", FuncName, version);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: type %d\n", FuncName, file_type);

   /* go through each kind of element that we learned is in the file */
   /* and read them */

   for (i = 0; i < nelems; i++) {

    /* get the description of the first element */
    elem_name = elist[i];
    plist = ply_get_element_description (ply, elem_name, &num_elems, &nprops);

    /* print the name of the element, for debugging */
    if (LocalHead) fprintf (SUMA_STDERR, "%s: element %s %d\n", FuncName, elem_name, num_elems);

    /* if we're on vertex elements, read them in */
    if (equal_strings ("vertex", elem_name)) {

      /* create a vertex list to hold all the vertices */
      #ifdef USE_PLY_VERTEX
      vlist = (Vertex **) SUMA_malloc (sizeof (Vertex *) * num_elems);
      #endif
      
      SO->NodeList = (float *) SUMA_calloc (3*num_elems, sizeof(float));
      if (!SO->NodeList) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate for SO->NodeList.\n", FuncName);
         SUMA_RETURN(NOPE);
      }
      
      /* set up for getting vertex elements */

      ply_get_property (ply, elem_name, &vert_props[0]);
      ply_get_property (ply, elem_name, &vert_props[1]);
      ply_get_property (ply, elem_name, &vert_props[2]);
      
      SO->NodeDim = 3;
      SO->N_Node = num_elems;
      /* grab all the vertex elements */
      for (j = 0; j < num_elems; j++) {

        /* grab and element from the file */
        #ifdef USE_PLY_VERTEX
        //vlist[j] = (Vertex *) SUMA_malloc (sizeof (Vertex));
        //ply_get_element (ply, (void *) vlist[j]);
        /* print out vertex x,y,z for debugging */
        if (LocalHead) fprintf (SUMA_STDERR, "%s vertex: %g %g %g\n", FuncName, vlist[j]->x, vlist[j]->y, vlist[j]->z);
        /* copy to NodeList */
        j3 = SO->NodeDim*j;
        SO->NodeList[j3] = vlist[j]->x;
        SO->NodeList[j3+1] = vlist[j]->y;
        SO->NodeList[j3+2] = vlist[j]->z;
        
        #else
        j3 = SO->NodeDim*j;
        ply_get_element (ply, (void *) &(SO->NodeList[j3]));
        /* print out vertex x,y,z for debugging */
        if (LocalHead) fprintf (SUMA_STDERR, "%s vertex: %g %g %g\n", FuncName, 
         SO->NodeList[j3], SO->NodeList[j3+1], SO->NodeList[j3+2]);
        #endif
         
      }
    }

    /* if we're on face elements, read them in */
    if (equal_strings ("face", elem_name)) {

      /* create a list to hold all the face elements */
      flist = (Face **) SUMA_malloc (sizeof (Face *) * num_elems);

      /* set up for getting face elements */

      ply_get_property (ply, elem_name, &face_props[0]);
      ply_get_property (ply, elem_name, &face_props[1]);

      /* grab all the face elements */
      for (j = 0; j < num_elems; j++) {

        /* grab and element from the file */
        flist[j] = (Face *) SUMA_malloc (sizeof (Face));
        ply_get_element (ply, (void *) flist[j]);

        /* print out face info, for debugging */
        if (LocalHead) {
         fprintf (SUMA_STDERR,"%s face: %d, list = ", FuncName, flist[j]->intensity);
         for (k = 0; k < flist[j]->nverts; k++)
            fprintf (SUMA_STDERR,"%d ", flist[j]->verts[k]);
         fprintf (SUMA_STDERR,"\n");
        }
        
      }
      /* copy face elements to SO structure */
      SO->FaceSetDim = flist[0]->nverts;
      SO->N_FaceSet = num_elems;
      SO->FaceSetList = (int *) SUMA_calloc (SO->FaceSetDim * num_elems, sizeof(int));
      if (!SO->FaceSetList) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate for SO->NodeList.\n", FuncName);
         if (SO->NodeList) SUMA_free(SO->NodeList); 
         SUMA_RETURN(NOPE);
      }
      
      for (j = 0; j < num_elems; j++) {
         if (flist[j]->nverts != SO->FaceSetDim) {
            fprintf (SUMA_STDERR, "Error %s: All FaceSets must have the same dimension for SUMA.\n", FuncName);
            if (SO->NodeList) SUMA_free(SO->NodeList); 
            if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
            SO->NodeList = NULL;
            SO->FaceSetList = NULL;
            SUMA_RETURN(NOPE);
         }
         ji = SO->FaceSetDim * j;
         for (k = 0; k < flist[j]->nverts; k++)
            SO->FaceSetList[ji+k] = flist[j]->verts[k];
      }
    }

   /* fill up a few more fields */
   SO->FileType = SUMA_PLY;
   if (file_type == PLY_ASCII) SO->FileFormat = SUMA_ASCII;
      else if (file_type == PLY_BINARY_BE) SO->FileFormat = SUMA_BINARY_BE;
         else if (file_type == PLY_BINARY_LE) SO->FileFormat = SUMA_BINARY_LE;
            else {
               fprintf (SUMA_STDERR, "Error %s: PLY_TYPE %d not recognized.\n", FuncName, file_type);
            }
            
   SO->Name = SUMA_StripPath(f_name);
   
   /* print out the properties we got, for debugging */
      if (LocalHead) {
         for (j = 0; j < nprops; j++)
            fprintf (SUMA_STDERR, "%s property %s\n", FuncName, plist[j]->name);
      }
   }

   /* grab and print out the comments in the file */
   comments = ply_get_comments (ply, &num_comments);
   if (LocalHead) {   
      for (i = 0; i < num_comments; i++)
         fprintf (SUMA_STDERR, "%s comment = '%s'\n", FuncName, comments[i]);
   }
   
   /* grab and print out the object information */
   obj_info = ply_get_obj_info (ply, &num_obj_info);
   if (LocalHead) {   
      for (i = 0; i < num_obj_info; i++)
         fprintf (SUMA_STDERR, "%s obj_info = '%s'\n", FuncName, obj_info[i]);
   }
   
   /* free the allocations necessary for vertex and facesetlists */
   for (j = 0; j < SO->N_FaceSet; j++) {
      SUMA_free(flist[j]);
   }
   SUMA_free(flist); flist = NULL;
   
   #ifdef USE_PLY_VERTEX
   for (j = 0; j < SO->N_Node; j++) {
      SUMA_free(vlist[j]);
   }
   SUMA_free(vlist); vlist = NULL;
   #endif
   /* close the PLY file, ply structure is freed within*/
   ply_close (ply);
   
   /* free plist */
   for (j = 0; j < nprops; j++) if (plist[j]) SUMA_free (plist[j]);
   if (plist) SUMA_free(plist);
   
   /* free comments */
   for (i = 0; i < num_comments; i++) if (comments[i]) SUMA_free (comments[i]);
   if (comments) SUMA_free (comments);
   
   /* free elist */
   for (i = 0; i < nelems; i++) if (elist[i]) SUMA_free (elist[i]);
   if (elist) SUMA_free (elist);
   
   /* free obj_info */
   for (i = 0; i < num_obj_info; i++) if (obj_info[i]) SUMA_free (obj_info[i]);
   if (obj_info) SUMA_free (obj_info);
    
   SUMA_RETURN(YUP);
}

/*!
   \brief Writes an SO into a .ply file
   ans = SUMA_Ply_Write (f_name, SO);
   \param f_name (char *) name of .ply file. if .ply is not attached it will be added.
   \param SO (SUMA_SurfaceObject *) Surface object to write out. 
      if SO->FileFormat = SUMA_BINARY_BE or SUMA_BINARY_LE the surface is written in binary ply format.
      SUMA_BINARY is set to SUMA_BINARY_BE
   \return ans (SUMA_Boolean) success flag.
   
   In its current incarnation, the function does not overwrite a pre-existing file.
      
*/ 
SUMA_Boolean SUMA_Ply_Write (char * f_name, SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_Ply_Write"};
   int i,j;
   PlyFile *ply = NULL;
   int nelems;
   int file_type;
   float version;
   int nverts ;
   int nfaces ;
   char *f_name2, *elem_names[] = { "vertex", "face" };/* list of the kinds of elements in the user's object */
   int n_elem_names = 2;
   Vertex **verts = NULL;
   Face *faces = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (SUMA_filexists (f_name)) {
      fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n", FuncName, f_name);
      SUMA_RETURN (NOPE);
   }else {
      f_name2 = (char*) SUMA_malloc ((strlen(f_name)+10) * sizeof(char));
      sprintf(f_name2,"%s.ply", f_name);
      if (SUMA_filexists (f_name2)) {
         fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n", FuncName, f_name2);
         SUMA_free(f_name2);
         SUMA_RETURN (NOPE);
      }
      SUMA_free(f_name2);
   }
   
   nverts = SO->N_Node;
   nfaces = SO->N_FaceSet;
   
   /* must have XYZ */
   if (SO->NodeDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: SO->NodeDim != 3.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   /* create the object in ply format */
   verts = (Vertex **) SUMA_malloc (nverts*sizeof(Vertex *));
   faces = (Face *) SUMA_malloc (nfaces*sizeof(Face));
   if (!verts || !faces) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      if (verts) SUMA_free(verts);
      if (faces) SUMA_free(faces);
      SUMA_RETURN (NOPE);
   }

   for (i = 0; i < nfaces; i++) {
      faces[i].intensity = '\001';
      faces[i].nverts = SO->FaceSetDim;
      faces[i].verts = &(SO->FaceSetList[SO->FaceSetDim*i]);
   }
   
   /* open either a binary or ascii PLY file for writing */
   /* (the file will be called "test.ply" because the routines */
   /*  enforce the .ply filename extension) */

   switch (SO->FileType) {
      case SUMA_BINARY_BE:
         ply = ply_open_for_writing(f_name, n_elem_names, elem_names, PLY_BINARY_BE, &version);
         break;
      
      case SUMA_BINARY_LE:
         ply = ply_open_for_writing(f_name, n_elem_names, elem_names, PLY_BINARY_LE, &version);
         break;
      
      case SUMA_ASCII:
         ply = ply_open_for_writing(f_name, n_elem_names, elem_names, PLY_ASCII, &version);
         break;
      
      case SUMA_BINARY:
         ply = ply_open_for_writing(f_name, n_elem_names, elem_names, PLY_BINARY_BE, &version);
         break;
            
      default:
         fprintf (SUMA_STDERR, "Error %s: Unrecognized file type.\n", FuncName);
         SUMA_RETURN (NOPE);
         break;  
   }

   if (!ply) {
      fprintf (SUMA_STDERR,"Error %s: Failed to create %s.\n", FuncName, f_name);
      if (verts) SUMA_free(verts);
      if (faces) SUMA_free(faces);
      SUMA_RETURN (NOPE);
   }
   /* describe what properties go into the vertex and face elements */

   ply_element_count (ply, "vertex", nverts);
   ply_describe_property (ply, "vertex", &vert_props[0]);
   ply_describe_property (ply, "vertex", &vert_props[1]);
   ply_describe_property (ply, "vertex", &vert_props[2]);

   ply_element_count (ply, "face", nfaces);
   ply_describe_property (ply, "face", &face_props[0]);
   ply_describe_property (ply, "face", &face_props[1]);

   /* write a comment and an object information field */
   ply_put_comment (ply, "author: Greg Turk");
   ply_put_obj_info (ply, "random information");

   /* we have described exactly what we will put in the file, so */
   /* we are now done with the header info */
   ply_header_complete (ply);

   /* set up and write the vertex elements */
   ply_put_element_setup (ply, "vertex");
   for (i = 0; i < nverts; i++)
    ply_put_element (ply, (void *) &(SO->NodeList[SO->NodeDim*i]));

   /* set up and write the face elements */
   ply_put_element_setup (ply, "face");
   for (i = 0; i < nfaces; i++)
    ply_put_element (ply, (void *) &faces[i]);

   /* close the PLY file */
   ply_close (ply);

   /* free */
   if (verts) SUMA_free(verts);
   if (faces) SUMA_free(faces);
   
   SUMA_RETURN (YUP);
}

/*! 
   \brief Function to write a surface object to a FreeSurfer .asc file format
   ans = SUMA_Boolean SUMA_FS_Write (fileNm, SO, firstLine);
   \param  fileNm (char *) name (and path) of file.
   \param  SO (SUMA_SurfaceObject *) Surface Object
   \param firstLine (char *) string to place as comment (begins with #) in fileNm
   \return YUP/NOPE
   
   
   The function will not overwrite pre-existing.
   Written by Brenna Bargall
*/
SUMA_Boolean SUMA_FS_Write (char *fileNm, SUMA_SurfaceObject *SO, char *firstLine) 
{
   static char FuncName[]={"SUMA_FS_Write"};
   int i, j;
   FILE *outFile = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (SUMA_filexists(fileNm)) {
      fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, fileNm);
      SUMA_RETURN (NOPE);
   }
   
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: Must have NodeDim and FaceSetDim = 3.\n",FuncName);
      SUMA_RETURN (NOPE);
   }

   outFile = fopen(fileNm, "w");
   if (!outFile) {
      fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, fileNm);
      SUMA_RETURN (NOPE);
   } 
   
   fprintf (outFile,"#%s\n", firstLine);
   fprintf (outFile, "%d %d\n", SO->N_Node, SO->N_FaceSet);

   j=0;
   for (i=0; i<SO->N_Node; ++i) {
      j=SO->NodeDim * i;
      fprintf (outFile, "%f  %f  %f  0\n", SO->NodeList[j], SO->NodeList[j+1], SO->NodeList[j+2]);
   }

   j=0;
   for (i=0; i<SO->N_FaceSet; ++i) {
      j = SO->FaceSetDim * i;
      fprintf (outFile, "%d %d %d 0\n", SO->FaceSetList[j], SO->FaceSetList[j+1], SO->FaceSetList[j+2]);
   }
    
   
   fclose(outFile);

   SUMA_RETURN (YUP);
   
}

/*!
   \brief writes the NodeList and FaceSetList of SO to 2 ascii files
   ans = SUMA_Boolean SUMA_VEC_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO);
   \param  Fname (SUMA_SFname *) uses the SureFit filename structure to store
                                 the names (and paths) of the NodeList (name_coord)
                                 and the FaceSetList (name_topo) files.
                                 if (strlen(name_coord) == 0) no coord is written
                                 if (strlen(name_topo) == 0) no topo is written
   \param SO (SUMA_SurfaceObject *) pointer to SO structure.
   \return YUP/NOPE
   
   \sa SUMA_VEC_Read
   The function will not overwrite pre-existing files.
   
*/
SUMA_Boolean SUMA_VEC_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO)
{
   
   static char FuncName[]={"SUMA_VEC_Write"};
   int i, j;
   FILE *outFile = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (strlen(Fname->name_coord)) {
      if (SUMA_filexists(Fname->name_coord)) {
         fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_coord);
         SUMA_RETURN (NOPE);
      }
   }
   if (strlen(Fname->name_topo)) {
      if (SUMA_filexists(Fname->name_topo)) {
         fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_topo);
         SUMA_RETURN (NOPE);
      }
   }
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: Must have NodeDim and FaceSetDim = 3.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (strlen(Fname->name_coord)) {
      outFile = fopen(Fname->name_coord, "w");
      if (!outFile) {
         fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, Fname->name_coord);
         SUMA_RETURN (NOPE);
      }

      j=0;
      for (i=0; i<SO->N_Node; ++i) {
         j=SO->NodeDim * i;
         fprintf (outFile, "%f  %f  %f \n", SO->NodeList[j], SO->NodeList[j+1], SO->NodeList[j+2]);
      }

      fclose (outFile);
   }
   
   if (strlen(Fname->name_topo)) {   
      outFile = fopen(Fname->name_topo, "w");
      if (!outFile) {
         fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, Fname->name_topo);
         SUMA_RETURN (NOPE);
      }
      j=0;
      for (i=0; i<SO->N_FaceSet; ++i) {
         j = SO->FaceSetDim * i;
         fprintf (outFile, "%d %d %d\n", SO->FaceSetList[j], SO->FaceSetList[j+1], SO->FaceSetList[j+2]);
      }

      fclose (outFile);
   }
   
   SUMA_RETURN (YUP);

}

/*!
   \brief A function to write a Surface Object into a Surefit ascii format 
   \param F_prefix (char *) Prefix of surace filenames. Output will be of the form:
         Prefix.N_NODE.topo
         Prefix.N_NODE.coord where N_Node is the number of nodes making up the surface.
   \param SO (SUMA_SurfaceObject *) surface object
   \return YUP/NOPE
   
*/


#ifdef SUMA_Ply_Read_STAND_ALONE
void usage_SUMA_Ply_Read_Main ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_Ply_Read -s f_name \n");
          printf ("\t reads in a .ply file and writes it out to copy_f_name.ply\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \t Wed Jan  8 13:44:29 EST 2003 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_Ply_Read_Main"}; 
	int kar;
   char *f_name=NULL, out_f_name[200];
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean brk;
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   
   if (argc < 3)
       {
          usage_SUMA_Ply_Read_Main ();
          exit (1);
       }
   
   kar = 1;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_Ply_Read_Main();
          exit (1);
		}
		
		if (!brk && (strcmp(argv[kar], "-s") == 0)) {
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -s ");
				exit (1);
			}
			f_name = argv[kar];
			/*fprintf(SUMA_STDOUT, "Found: %s\n", f_name);*/

			brk = YUP;
		}
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   if (!f_name) {
      fprintf (SUMA_STDERR,"Error %s: Missing filename.\n", FuncName);
      exit(1);
   }
   
   SO = SUMA_Alloc_SurfObject_Struct(1);   
   if (!SUMA_Ply_Read (f_name, SO)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Ply_Read.\n", FuncName);
      exit (1);
   } 
   
   SO->Label = SUMA_SurfaceFileName (SO, NOPE);
   sprintf (out_f_name , "copy_%s", SO->Label);   
   fprintf (SUMA_STDERR,"%s: Success apparent. Now writing SO to %s\n", FuncName, out_f_name);
   if (!SUMA_Ply_Write (out_f_name, SO)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Ply_Read.\n", FuncName);
      exit (1);
   } 
      
   SUMA_Free_Surface_Object (SO);
   
   return (0);
 } /* Main */  
#endif

#ifdef SUMA_ConvertSurface_STAND_ALONE
void usage_SUMA_ConvertSurface ()
   
  {/*Usage*/
          static char FuncName[]={"usage_SUMA_ConvertSurface"};
          char * s = NULL;
          
          printf ("\n\33[1mUsage: \33[0m ConvertSurface <-i_TYPE inSurf> <-o_TYPE outSurf> \n"
                  "    [<-sv SurfaceVolume [VolParam for sf surfaces]>] [-tlrc] [-MNI_rai/-MNI_lpi]\n"
                  "    reads in a surface and writes it out in another format.\n"
                  "    Note: This is a not a general utility conversion program. \n"
                  "    Only fields pertinent to SUMA are preserved.\n"
                  "    -i_TYPE inSurf specifies the input surface, TYPE is one of the following:\n"
                  "       fs: FreeSurfer surface. \n"
                  "           Only .asc surfaces are read.\n"
                  "       sf: SureFit surface. \n"
                  "           You must specify the .coord followed by the .topo file.\n"
                  "       vec: Simple ascii matrix format. \n"
                  "            You must specify the NodeList file followed by the FaceSetList file.\n"
                  "            NodeList contains 3 floats per line, representing X Y Z vertex coordinates.\n"
                  "            FaceSetList contains 3 ints per line, representing v1 v2 v3 triangle vertices.\n"
                  "       ply: PLY format, ascii or binary.\n"
                  "            Only vertex and triangulation info is preserved.\n"
                  "    -o_TYPE outSurf specifies the output surface, TYPE is one of the following:\n"
                  "       fs: FreeSurfer ascii surface. \n"
                  "       sf: SureFit surface. (NOT IMPLEMENTED YET)\n"
                  "           You must specify the .coord followed by the .topo file.\n"
                  "       vec: Simple ascii matrix format. \n"
                  "            see help for vec under -i_TYPE options for format specifications.\n"
                  "       ply: PLY format, ascii or binary.\n"
                  "    -sv SurfaceVolume [VolParam for sf surfaces]\n"
                  "       This option must not come before the -i_TYPE option.\n"
                  "       If you supply a surface volume, the coordinates of the input surface.\n"
                  "        are modified to SUMA's convention and aligned with SurfaceVolume.\n"
                  "        You must also specify a VolParam file for SureFit surfaces.\n"
                  "    -tlrc: Apply Talairach transform (which must be in talairach version of \n"
                  "        SurfaceVolume) to the surface vertex coordinates. \n"
                  "        This option must be used with the -sv option.\n"
                  "    -MNI_rai/-MNI_lpi: Apply Andreas Meyer Lindenberg's transform to turn \n"
                  "        AFNI tlrc coordinates (RAI) into MNI coord space \n"
                  "        in RAI (with -MNI_rai) or LPI (with -MNI_lpi)).\n"
                  "        NOTE: -MNI_lpi option has not been tested yet (I have no data\n"
                  "        to test it on. Verify alignment with AFNI and please report\n"
                  "        any bugs.\n" 
                  "        This option can be used without the -tlrc option.\n"
                  "        But that assumes that surface nodes are already in\n"
                  "        AFNI RAI tlrc coordinates .\n"    
                  "   NOTE: The vertex coordinates coordinates of the input surfaces are only\n"
                  "         transformed if -sv option is used. If you do transform surfaces, \n"
                  "         take care not to load them into SUMA with another -sv option.\n"); 
          s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL; 
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \t Wed Jan  8 13:44:29 EST 2003 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ConvertSurface"}; 
	int kar;
   char  *if_name = NULL, *of_name = NULL, *if_name2 = NULL, 
         *of_name2 = NULL, *sv_name = NULL, *vp_name = NULL, 
         *OF_name = NULL, *OF_name2 = NULL, *tlrc_name = NULL;
   SUMA_SO_File_Type iType = SUMA_FT_NOT_SPECIFIED, oType = SUMA_FT_NOT_SPECIFIED;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_PARSED_NAME *of_name_strip = NULL, *of_name2_strip = NULL;
   SUMA_SFname *SF_name = NULL;
   void *SO_name = NULL;
   THD_warp *warp=NULL ;
   THD_3dim_dataset *aset=NULL;
   SUMA_Boolean brk, Do_tlrc, Do_mni_RAI, Do_mni_LPI ;
   SUMA_Boolean LocalHead = NOPE;
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   
   if (argc < 4)
       {
          usage_SUMA_ConvertSurface ();
          exit (1);
       }
   
   kar = 1;
	brk = NOPE;
   Do_tlrc = NOPE;
   Do_mni_RAI = NOPE;
   Do_mni_LPI = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_ConvertSurface();
          exit (0);
		}
		
		if (!brk && (strcmp(argv[kar], "-i_fs") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_fs ");
				exit (1);
			}
			if_name = argv[kar];
         iType = SUMA_FREE_SURFER;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_sf") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_sf");
				exit (1);
			}
			if_name = argv[kar]; kar ++;
         if_name2 = argv[kar];
         iType = SUMA_SUREFIT;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_vec") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_vec");
				exit (1);
			}
			if_name = argv[kar]; kar ++;
         if_name2 = argv[kar];
         iType = SUMA_VEC;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_ply") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_ply ");
				exit (1);
			}
			if_name = argv[kar];
         iType = SUMA_PLY;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-sv") == 0)) {
         if (iType == SUMA_FT_NOT_SPECIFIED) {
            fprintf (SUMA_STDERR, " -sv option must be preceeded by -i_TYPE option.");
            exit(1);
         }
         kar ++;
			if (iType == SUMA_SUREFIT) {
            if (kar+1 >= argc)  {
		  		   fprintf (SUMA_STDERR, "need 2 argument after -sv (SurfaceVolume and VolumeParent)");
				   exit (1);
			   }
            sv_name = argv[kar]; kar ++;
            vp_name = argv[kar];
         } else {
            if (kar >= argc)  {
		  		   fprintf (SUMA_STDERR, "need argument after -sv ");
				   exit (1);
			   }
			   sv_name = argv[kar];
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-o_fs") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -o_fs ");
				exit (1);
			}
			of_name = argv[kar];
         oType = SUMA_FREE_SURFER;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-o_sf") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -o_sf");
				exit (1);
			}
			of_name = argv[kar]; kar ++;
         of_name2 = argv[kar];
         oType = SUMA_SUREFIT;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-o_vec") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -o_vec");
				exit (1);
			}
			of_name = argv[kar]; kar ++;
         of_name2 = argv[kar];
         oType = SUMA_VEC;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-o_ply") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -o_ply ");
				exit (1);
			}
			of_name = argv[kar];
         oType = SUMA_PLY;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-tlrc") == 0)) {
         Do_tlrc = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-MNI_rai") == 0)) {
         Do_mni_RAI = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-MNI_lpi") == 0)) {
         Do_mni_LPI = YUP;
         brk = YUP;
      }
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   /* sanity checks */
   if (Do_mni_LPI && Do_mni_RAI) {
      fprintf (SUMA_STDERR,"Error %s:\nCombining -MNI_lpi and -MNI_rai options.\nNot good.", FuncName);
      exit(1);
   }
   
   if (!if_name) {
      fprintf (SUMA_STDERR,"Error %s: input surface not specified.\n", FuncName);
      exit(1);
   }
   if (!of_name) {
      fprintf (SUMA_STDERR,"Error %s: output surface not specified.\n", FuncName);
      exit(1);
   }
   if (iType == SUMA_FT_NOT_SPECIFIED) {
      fprintf (SUMA_STDERR,"Error %s: input type not recognized.\n", FuncName);
      exit(1);
   }
   if (oType == SUMA_FT_NOT_SPECIFIED) {
      fprintf (SUMA_STDERR,"Error %s: output type not recognized.\n", FuncName);
      exit(1);
   }
   if (iType == SUMA_SUREFIT) {
      if (!if_name2) {
         fprintf (SUMA_STDERR,"Error %s: input SureFit surface incorrectly specified.\n", FuncName);
         exit(1);
      }
      if (sv_name && !vp_name) {
         fprintf (SUMA_STDERR,"Error %s: VolParent must specified with -sv potion for SureFit surfaces. \n", FuncName);
         exit(1);
      }
   }
   if (iType == SUMA_VEC) {
      if (!if_name2) {
         fprintf (SUMA_STDERR,"Error %s: input vec surface incorrectly specified.\n", FuncName);
         exit(1);
      }
   }

   if (( Do_mni_RAI || Do_mni_LPI) && !Do_tlrc) {
      SUMA_SL_Warn ("I hope you know what you're doing.\nThe MNI transform should only be applied to a\nSurface in the AFNI tlrc coordinate space.\n");
   }
   
   if (oType == SUMA_SUREFIT) {
      if (!of_name2) {
       fprintf (SUMA_STDERR,"Error %s: output SureFit surface incorrectly specified. \n", FuncName);
       exit(1);
      }
   }
   
   if (oType == SUMA_VEC) {
      if (!of_name2) {
       fprintf (SUMA_STDERR,"Error %s: output vec surface incorrectly specified. \n", FuncName);
       exit(1);
      }
   }

   /* test for existence of input files */
   if (!SUMA_filexists(if_name)) {
      fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, if_name);
      exit(1);
   }
   
   if (if_name2) {
      if (!SUMA_filexists(if_name2)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, if_name2);
         exit(1);
      }
   }

   if (sv_name) {
      if (!SUMA_filexists(sv_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, sv_name);
         exit(1);
      }
   }
   
   if (Do_tlrc && !sv_name) {
      fprintf (SUMA_STDERR,"Error %s: -tlrc must be used with -sv option.\n", FuncName);
      exit(1);
   }
   
   if (vp_name) {
      if (!SUMA_filexists(vp_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, vp_name);
         exit(1);
      }
   }

   /* check for existence of output files */
   if (of_name) {
      of_name_strip = SUMA_ParseFname (of_name);
      OF_name = (char *) SUMA_malloc (sizeof(char)*(strlen(of_name)+20));
   }
   if (of_name2) {
      of_name2_strip = SUMA_ParseFname (of_name2);
      OF_name2 = (char *) SUMA_malloc (sizeof(char)*(strlen(of_name2)+20));
   }

   if (oType == SUMA_FREE_SURFER) {
      if (strcmp (of_name_strip->Ext,".asc")==0) {
         sprintf (OF_name,"%s%s.asc", of_name_strip->Path, of_name_strip->FileName_NoExt);
      }else {
         sprintf (OF_name,"%s%s.asc", of_name_strip->Path, of_name_strip->FileName);
      }
   }else if (oType == SUMA_PLY) {
      if (strcmp (of_name_strip->Ext,".ply")==0) {
         sprintf (OF_name,"%s%s.ply", of_name_strip->Path, of_name_strip->FileName_NoExt);
      }else {
         sprintf (OF_name,"%s%s.ply", of_name_strip->Path, of_name_strip->FileName);
      }
   }else if (oType == SUMA_SUREFIT) {
      if (strcmp (of_name_strip->Ext,".coord")==0) {
         sprintf (OF_name,"%s%s.coord", of_name_strip->Path, of_name_strip->FileName_NoExt);
      }else {
         sprintf (OF_name,"%s%s.coord", of_name_strip->Path, of_name_strip->FileName);
      }
      if (strcmp (of_name2_strip->Ext,".topo")==0) {
         sprintf (OF_name2,"%s%s.topo", of_name2_strip->Path, of_name2_strip->FileName_NoExt);
      }else {
         sprintf (OF_name2,"%s%s.topo", of_name2_strip->Path, of_name2_strip->FileName);
      }
   }else {
      sprintf (OF_name, "%s",of_name);
      if (of_name2) sprintf(OF_name2, "%s",of_name2);
   }
    
   if (SUMA_filexists(OF_name)) {
      fprintf (SUMA_STDERR,"Error %s: %s exists already.\n", FuncName, OF_name);
      exit(1);
   }
   
   if (of_name2) {
      if (SUMA_filexists(OF_name2)) {
         fprintf (SUMA_STDERR,"Error %s: %s exists already.\n", FuncName, OF_name2);
         exit(1);
      }
   }
   
   /* now for the real work */
   /* prepare the name of the surface object to read*/
   switch (iType) {
      case SUMA_SUREFIT:
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         sprintf(SF_name->name_coord,"%s", if_name);
         sprintf(SF_name->name_topo,"%s", if_name2); 
         if (!vp_name) { /* initialize to empty string */
            SF_name->name_param[0] = '\0'; 
         }
         else {
            sprintf(SF_name->name_param,"%s", vp_name);
         }
         SO_name = (void *)SF_name;
         fprintf (SUMA_STDOUT,"Reading %s and %s...\n", SF_name->name_coord, SF_name->name_topo);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_SUREFIT, SUMA_ASCII, sv_name);
         break;
      case SUMA_VEC:
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         sprintf(SF_name->name_coord,"%s", if_name);
         sprintf(SF_name->name_topo,"%s", if_name2); 
         SO_name = (void *)SF_name;
         fprintf (SUMA_STDOUT,"Reading %s and %s...\n", SF_name->name_coord, SF_name->name_topo);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_VEC, SUMA_ASCII, sv_name);
         break;
      case SUMA_FREE_SURFER:
         SO_name = (void *)if_name; 
         fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_FREE_SURFER, SUMA_ASCII, sv_name);
         break;  
      case SUMA_PLY:
         SO_name = (void *)if_name; 
         fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_PLY, SUMA_FF_NOT_SPECIFIED, sv_name);
         break;  
      default:
         fprintf (SUMA_STDERR,"Error %s: Bad format.\n", FuncName);
         exit(1);
   }
   
   if (!SO) {
      fprintf (SUMA_STDERR,"Error %s: Failed to read input surface.\n", FuncName);
      exit (1);
   }
   
   if (Do_tlrc) {
      fprintf (SUMA_STDOUT,"Performing talairach transform...\n");

      /* form the tlrc version of the surface volume */
      tlrc_name = (char *) SUMA_calloc (strlen(SO->VolPar->dirname)+strlen(SO->VolPar->prefix)+60, sizeof(char));
      sprintf (tlrc_name, "%s%s+tlrc.HEAD", SO->VolPar->dirname, SO->VolPar->prefix);
      if (!SUMA_filexists(tlrc_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, tlrc_name);
         exit(1);
      }
      
      /* read the tlrc header */
      aset = THD_open_dataset(tlrc_name) ;
      if( !ISVALID_DSET(aset) ){
         fprintf (SUMA_STDERR,"Error %s: %s is not a valid data set.\n", FuncName, tlrc_name) ;
         exit(1);
      }
      if( aset->warp == NULL ){
         fprintf (SUMA_STDERR,"Error %s: tlrc_name does not contain a talairach transform.\n", FuncName);
         exit(1);
      }
      
      warp = aset->warp ;
      
      /* now warp the coordinates, one node at a time */
      if (!SUMA_AFNI_forward_warp_xyz(warp, SO->NodeList, SO->N_Node)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AFNI_forward_warp_xyz.\n", FuncName);
         exit(1);
      }

      
   }
   
   if (Do_mni_RAI) {
      /* apply the mni warp */
      if (!SUMA_AFNItlrc_toMNI(SO->NodeList, SO->N_Node, "RAI")) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AFNItlrc_toMNI.\n", FuncName);
         exit(1);
      }
   }
   
   if (Do_mni_LPI) {
      /* apply the mni warp */
      if (!SUMA_AFNItlrc_toMNI(SO->NodeList, SO->N_Node, "LPI")) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AFNItlrc_toMNI.\n", FuncName);
         exit(1);
      }
   }
   
  
   if (LocalHead) SUMA_Print_Surface_Object (SO, stderr);
   
   fprintf (SUMA_STDOUT,"Writing surface...\n");
   

   /* write the surface object */
   switch (oType) {
      case SUMA_SUREFIT:
         if (SF_name) SUMA_free(SF_name);
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         sprintf(SF_name->name_coord,"%s", of_name);
         sprintf(SF_name->name_topo,"%s", of_name2); 
         if (!vp_name) { /* initialize to empty string */
            SF_name->name_param[0] = '\0'; 
         }
         else {
            sprintf(SF_name->name_param,"%s", vp_name);
         }
         SO_name = (void *)SF_name;
         if (!SUMA_Save_Surface_Object (SO_name, SO,  SUMA_SUREFIT, SUMA_ASCII)) {
            fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
            exit (1);
         }
         break;
      case SUMA_VEC:
         if (SF_name) SUMA_free(SF_name);
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         sprintf(SF_name->name_coord,"%s", of_name);
         sprintf(SF_name->name_topo,"%s", of_name2); 
         SO_name = (void *)SF_name;
         if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_VEC, SUMA_ASCII)) {
            fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
            exit (1);
         }
         break;
      case SUMA_FREE_SURFER:
         SO_name = (void *)of_name; 
         if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_FREE_SURFER, SUMA_ASCII)) {
            fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
            exit (1);
         }
         break;  
      case SUMA_PLY:
         SO_name = (void *)of_name; 
         if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_PLY, SUMA_FF_NOT_SPECIFIED)) {
            fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
            exit (1);
         }
         break;  
      default:
         fprintf (SUMA_STDERR,"Error %s: Bad format.\n", FuncName);
         exit(1);
   }
   
   
   
   if (of_name_strip) of_name_strip = SUMA_Free_Parsed_Name (of_name_strip);
   if (of_name2_strip) of_name2_strip = SUMA_Free_Parsed_Name (of_name2_strip);
   if (OF_name) SUMA_free(OF_name);
   if (OF_name2) SUMA_free(OF_name2);
   if (SF_name) SUMA_free(SF_name);
   if (SO) SUMA_Free_Surface_Object(SO);
   return (0);
}
#endif

/*!
   \brief Handles opening an ROI file
   \param filename (char *)
   \param data(void *) 
   
   - results are placed in SUMAg_DOv
   
*/
void SUMA_OpenDrawnROI (char *filename, void *data)
{
   static char FuncName[]={"SUMA_OpenDrawnROI"};
   DList *list=NULL;
   SUMA_DRAWN_ROI **ROIv=NULL;
   int i, N_ROI;
   SUMA_SurfaceObject *SO=NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SUMA_LH("Called");   

   /* check for type ... */
   
   if (SUMA_isExtension(filename, ".niml.roi")) {
      /* load niml ROI */
      if (!( ROIv = SUMA_OpenDrawnROI_NIML (filename, &N_ROI, YUP))) {
         SUMA_SLP_Err("Failed to read NIML ROI.");
         SUMA_RETURNe;
      }
   }else if (SUMA_isExtension(filename, ".1D.roi")) {
      /* load 1D ROI */
      /* You need to select a parent surface */
      SUMA_SLP_Warn("Assuming parent surface.");
      SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_SVv[0].Focus_SO_ID].OP);
      if (!( ROIv = SUMA_OpenDrawnROI_1D (filename, SO->idcode_str, &N_ROI, YUP))) {
         SUMA_SLP_Err("Failed to read NIML ROI.");
         SUMA_RETURNe;
      }
   }else {
      SUMA_SLP_Err(  "Failed to recognize\n"
                     "ROI type from filename.");
      SUMA_RETURNe;
   } 
   
   /* put those ROIs in SUMAg_DOv */
   for (i=0; i < N_ROI; ++i) {
      /* add ROI to DO list */
      if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIv[i], ROIdO_type, SUMA_LOCAL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
      }
   }
   /* free ROIv */
   if (ROIv) SUMA_free(ROIv); ROIv = NULL;

   /* if there are no currentROIs selected, set currentROI to the
      first in the list */
   if (!SUMAg_CF->X->DrawROI->curDrawnROI) {
      i = 0;
      do {
         if (SUMAg_DOv[i].ObjectType == ROIdO_type) SUMAg_CF->X->DrawROI->curDrawnROI =
                                             (SUMA_DRAWN_ROI *)SUMAg_DOv[i].OP;
         ++i;
      } while (i < SUMAg_N_DOv && !SUMAg_CF->X->DrawROI->curDrawnROI);
   }
   
   if (SUMAg_CF->X->DrawROI->curDrawnROI) {
      SUMA_InitializeDrawROIWindow(SUMAg_CF->X->DrawROI->curDrawnROI);   
   }
   
   /* Now update the Paint job on the ROI plane */
   if (!SUMA_Paint_SO_ROIplanes_w (
            SUMA_findSOp_inDOv(SUMAg_CF->X->DrawROI->curDrawnROI->Parent_idcode_str, 
            SUMAg_DOv, SUMAg_N_DOv), SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURNe;
   }
   
   /* put a nice redisplay here */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_Suma, NULL); 
   if (!SUMA_Engine(&list)) {
      SUMA_SLP_Err("Failed to redisplay.");
      SUMA_RETURNe;
   }
   
   SUMA_RETURNe; 
}

/*!
   Since parent information does not exist in ROI 1D files, 
   you need to specify the parent surface.
   
   ans = SUMA_OpenDrawnROI_1D (filename, Parent_idcode_str);
   
   \param filename (char *) name of 1D file (see below for formats)
   \param Parent_idcode_str (char *) idcode of parent surface
   \param N_ROI (int *) will contain the number of ROIs read in
   \param ForDisplay (SUMA_Boolean) YUP: prepares ROI for display
   \return ans (SUMA_Boolean) YUP for good NOPE for not good.
   
   - The ROI is added to SUMAg_DOv
   
   - The 1D file can have multiple formats. Some are particularly
   inefficient and should not be used. But we aim to please so 
   anything goes. You can have a varying number of columns which 
   are i, l, r, g, b. These columns stand for index, label, red,
   green and blue, respectively.
      * format 1: i 
      Only a bunch of node (int) indices are supplied. Label of all nodes
      defaults to 0 and a default color is given
      * format 2: i l
      Each node carries a label (int) with it. This way you can
      have multiple ROIs specified in one 1D file. The same
      color is assigned to each of the ROIs. 
      * format 3: i r g b
      A bunch of nodes with r g b (float) triplet specifying the nodes'
      colors. Obviously, since all nodes belong to the same ROI, 
      the specification of an r g b for each node is redundant since
      all nodes in the same ROI have the same color. Use the 
      niml format if that sounds crazy to you. If you want a different
      color for each node then you should not load the data as an ROI
      but as a node color file. 
      * format 4: i l r g b
      A bunch of nodes with labels and r g b .
      Like format 2 but with the possibility of specifying the colors of
      each ROI. 
       
*/ 
SUMA_DRAWN_ROI ** SUMA_OpenDrawnROI_1D (char *filename, char *Parent_idcode_str,
                                        int *N_ROI, SUMA_Boolean ForDisplay)
{
   static char FuncName[]={"SUMA_OpenDrawnROI_1D"};
   MRI_IMAGE *im = NULL;
   int ncol, nrow, *iLabel=NULL, *iNode = NULL, *isort=NULL,
      i, N_Labels = 0, *iStart=NULL, *iStop=NULL, cnt = 0;
   float *far=NULL, *r=NULL, *g=NULL, *b=NULL, *RGB=NULL;
   SUMA_DRAWN_ROI **ROIv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SUMA_LH("Called");
   
   *N_ROI = 0;
   
   im = mri_read_1D (filename);
   
   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NULL);
   }
   
   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;
   
   if (!ncol) {
      SUMA_SL_Err("Empty file");
      SUMA_RETURN(NULL);
   }
   if (nrow != 1 && nrow != 2 && nrow != 4 && nrow != 5) {
      SUMA_SL_Err("File must have\n"
                  " 1,2,4 or 5 columns.");
      mri_free(im); im = NULL;   /* done with that baby */
      SUMA_RETURN(NULL);
   }
   
   if (LocalHead) {
      SUMA_disp_vect(far, ncol*nrow);
   }
      
   switch (nrow) {
      case 1:
         /* Node index only*/
         SUMA_LH ("1D format: i");
         iNode = (int *)SUMA_malloc(ncol*sizeof(int));
         iLabel = (int *)SUMA_malloc(1*sizeof(int));
         if (!iNode ||!iLabel) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         for (i=0; i < ncol; ++i) iNode[i] = (int)far[i];
         mri_free(im); im = NULL;   /* done with that baby */
         
         iLabel[0] = 0;
         N_Labels = 1;
         iStart = (int *)SUMA_malloc(1*sizeof(int));
         iStop = (int *)SUMA_malloc(1*sizeof(int));
         RGB = (float *)SUMA_malloc(3*1*sizeof(float));
         iStart[0] = 0;
         iStop[0] = ncol-1;
         RGB[0] = 1.0; RGB[1] = 1.0; RGB[2] = 0;
         break;
      case 2:
         /* Node index & Node Label */
         SUMA_LH("1D format: i l");
         /* copy the node indices and labels for cleanliness */
         iLabel = (int *)SUMA_malloc(ncol*sizeof(int));
         iNode = (int *)SUMA_malloc(ncol*sizeof(int));
         if (!iNode || !iLabel) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         for (i=0; i < ncol; ++i) iLabel[i] = (int)far[i+ncol];
         /* sort the Labels and the iNode accordingly */
         isort = SUMA_z_dqsort( iLabel, ncol);
         for (i=0; i < ncol; ++i) iNode[i] = (int)far[isort[i]];

         mri_free(im); im = NULL;   /* done with that baby */

         /* Count the number of distinct labels */
         N_Labels = 1;
         for (i=1; i < ncol; ++i) if (iLabel[i] != iLabel[i-1]) ++N_Labels;
         /* store where each label begins and ends */
         iStart = (int *)SUMA_malloc(N_Labels*sizeof(int));
         iStop = (int *)SUMA_malloc(N_Labels*sizeof(int));
         RGB = (float *)SUMA_malloc(3*N_Labels*sizeof(float));
         if (!iStart || !iStop) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         cnt = 0;
         iStart[cnt] = 0;
         iStop[cnt] = ncol -1;
         RGB[3*cnt] = 1.0; RGB[3*cnt+1] = 1.0; RGB[3*cnt+2] = 0;
         for (i=1; i < ncol; ++i) {
            if (iLabel[i] != iLabel[i-1]) {
               iStop[cnt] = i-1;
               ++cnt; 
               iStart[cnt] = i;
               iStop[cnt] = ncol -1;
               RGB[3*cnt] = 1.0; RGB[3*cnt+1] = 1.0; RGB[3*cnt+2] = 0;
            }
         }
         break;
      case 4:
         /* Node index, R G B */
         SUMA_LH("1D format: i R G B");
         iNode = (int *)SUMA_malloc(ncol*sizeof(int));
         iLabel = (int *)SUMA_malloc(1*sizeof(int));
         r = (float *)SUMA_malloc(ncol*sizeof(float));
         g = (float *)SUMA_malloc(ncol*sizeof(float));
         b = (float *)SUMA_malloc(ncol*sizeof(float));
         if (!iNode || !iLabel || !r || !g || !b) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         
         for (i=0; i < ncol; ++i) {
            iNode[i] = (int)far[i];
            r[i] = (float)far[i+ncol];
            g[i] = (float)far[i+2*ncol];
            b[i] = (float)far[i+3*ncol];
         }
         
         iLabel[0] = 0;
         N_Labels = 1;
         iStart = (int *)SUMA_malloc(1*sizeof(int));
         iStop = (int *)SUMA_malloc(1*sizeof(int));
         RGB = (float *)SUMA_malloc(3*1*sizeof(float));
         mri_free(im); im = NULL;   /* done with that baby */
        
         iStart[0] = 0;
         iStop[0] = ncol-1;
         RGB[0] = r[0]; RGB[1] = g[0]; RGB[2] = b[0];
         break;
      case 5:
         /* Node index, Node Label, R G B */
         SUMA_LH("1D format: i l R G B");
         /* copy the node indices and labels for cleanliness */
         iLabel = (int *)SUMA_malloc(ncol*sizeof(int));
         iNode = (int *)SUMA_malloc(ncol*sizeof(int));
         r = (float *)SUMA_malloc(ncol*sizeof(float));
         g = (float *)SUMA_malloc(ncol*sizeof(float));
         b = (float *)SUMA_malloc(ncol*sizeof(float));
         if (!iNode || !iLabel || !r || !g || !b) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         for (i=0; i < ncol; ++i) iLabel[i] = (int)far[i+ncol];
         /* sort the Labels and the iNode accordingly */
         isort = SUMA_z_dqsort( iLabel, ncol);
         for (i=0; i < ncol; ++i) {
            iNode[i] = (int)far[isort[i]];
            r[i] = (float)far[isort[i]+2*ncol];
            g[i] = (float)far[isort[i]+3*ncol];
            b[i] = (float)far[isort[i]+4*ncol];
         }     
         mri_free(im); im = NULL;   /* done with that baby */

         /* Count the number of distinct labels */
         N_Labels = 1;
         for (i=1; i < ncol; ++i) if (iLabel[i] != iLabel[i-1]) ++N_Labels;
         /* store where each label begins and ends */
         iStart = (int *)SUMA_malloc(N_Labels*sizeof(int));
         iStop = (int *)SUMA_malloc(N_Labels*sizeof(int));
         RGB = (float *)SUMA_malloc(3*N_Labels*sizeof(float));
         if (!iStart || !iStop || !RGB) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         cnt = 0;
         iStart[cnt] = 0;
         iStop[cnt] = ncol -1;
         RGB[3*cnt] = r[0]; RGB[3*cnt+1] = g[0]; RGB[3*cnt+2] = b[0];
         for (i=1; i < ncol; ++i) {
            if (iLabel[i] != iLabel[i-1]) {
               iStop[cnt] = i-1;
               ++cnt; 
               iStart[cnt] = i;
               iStop[cnt] = ncol -1;
               RGB[3*cnt] = r[i]; RGB[3*cnt+1] = g[i]; RGB[3*cnt+2] = b[i];
            }
         }
         break;
      default:
         SUMA_SLP_Err("Unrecognized 1D format");
         mri_free(im); im = NULL;   /* done with that baby */
         break;
   }
   
   
   ROIv = (SUMA_DRAWN_ROI **)SUMA_malloc(N_Labels*sizeof(SUMA_DRAWN_ROI*));
   
   for (i=0; i < N_Labels; ++i) {
      int Value, N_Node, *Node=NULL;
      float fillcolor[3], edgecolor[3];
      int edgethickness;
      char stmp[20], *Label=NULL;
      SUMA_PARSED_NAME *NewName=NULL; 
      
      edgethickness = 3;
      fillcolor[0] = RGB[3*i]; fillcolor[1] = RGB[3*i+1]; fillcolor[2] = RGB[3*i+2]; 
      edgecolor[0] = 0; edgecolor[1] = 0; edgecolor[2] = 1; 
      Value = iLabel[iStart[i]]; /* the index label of this ROI */
      N_Node = iStop[i] - iStart[i] + 1; /* Number of Nodes in this ROI */
      Node = &(iNode[iStart[i]]); /* pointer to location of first index in this ROI */
      /* prepare a label for these ROIs */
      NewName = SUMA_ParseFname (filename);
      if (!NewName) {
         Label = SUMA_copy_string("BadLabel");
      }else {
         sprintf(stmp,"(%d)", Value);
         Label = SUMA_append_string(stmp,NewName->FileName_NoExt);
      }
      SUMA_LH("Transforming to Drawn ROIs...");
      ROIv[i] = SUMA_1DROI_to_DrawnROI( Node, N_Node , 
                                    Value, Parent_idcode_str,
                                    Label, NULL,
                                    fillcolor, edgecolor, edgethickness, 
                                    SUMAg_DOv, SUMAg_N_DOv,
                                    ForDisplay);
      
      if (Label) SUMA_free(Label); Label = NULL;
      if (NewName) SUMA_Free_Parsed_Name(NewName); NewName = NULL;
      if (LocalHead) fprintf (SUMA_STDERR, "%s: ROI->Parent_idcode_str %s\n", FuncName, ROIv[i]->Parent_idcode_str);

   }
 
   SUMA_LH("Freeing...");
   
   if (iLabel) SUMA_free(iLabel); iLabel = NULL;
   if (isort) SUMA_free(isort); isort = NULL;
   if (iNode) SUMA_free(iNode); iNode = NULL;
   if (iStart) SUMA_free(iStart); iStart = NULL;
   if (iStop) SUMA_free(iStop); iStop = NULL;
   if (r) SUMA_free(r); r = NULL;
   if (g) SUMA_free(g); g = NULL;
   if (b) SUMA_free(b); b = NULL;
   if (RGB) SUMA_free(RGB); RGB = NULL;
   
   *N_ROI = N_Labels;
   SUMA_RETURN(ROIv);
     
}

/*!
   \brief Loads a niml ROI 
   
   \param ForDisplay (SUMA_Boolean) YUP: Performs checks to see if ROI with similar idcode already
                                          exists and if parent surface is loaded.
                                    NOPE: Does not check for above conditions.
*/
SUMA_DRAWN_ROI ** SUMA_OpenDrawnROI_NIML (char *filename, int *N_ROI, SUMA_Boolean ForDisplay)
{ /* begin embedded function */
   static char FuncName[]={"SUMA_OpenDrawnROI_NIML"};
   char stmp[SUMA_MAX_NAME_LENGTH+100], *nel_idcode;
   NI_element *nel = NULL;
   NI_element **nelv=NULL;
   NI_stream ns ;
   int n_read=0, idat, answer, inel, iDO, N_nel;
   SUMA_NIML_ROI_DATUM *niml_ROI_datum_buff=NULL;
   SUMA_NIML_DRAWN_ROI * nimlROI=NULL;
   SUMA_DRAWN_ROI **ROIv=NULL;
   SUMA_Boolean found = YUP, AddNel = YUP, AlwaysReplace = NOPE, NeverReplace = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   *N_ROI = 0;
   
   if (SUMAg_CF->nimlROI_Datum_type < 0) {
      SUMA_SL_Err("Bad niml type code");
      SUMA_RETURN(NULL);
   }
   if (LocalHead) fprintf(SUMA_STDERR, "%s: roi_type code = %d\n", FuncName, SUMAg_CF->nimlROI_Datum_type) ;

   sprintf(stmp,"file:%s", filename);
   ns = NI_stream_open( stmp , "r" ) ;
   if( ns == NULL ){
      SUMA_SL_Err("Can't open ROI file"); 
      SUMA_RETURN(NULL);
   }
   
   nelv = (NI_element **) SUMA_calloc(SUMA_MAX_DISPLAYABLE_OBJECTS, sizeof(NI_element *));
   if (!nelv) {
      SUMA_SLP_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   NeverReplace = NOPE;
   AlwaysReplace = NOPE;
   inel = 0;
   do {
      nel = NI_read_element(ns,1) ;
      
      if (nel) {
         found = YUP;
         
         if (LocalHead && 0) SUMA_nel_stdout (nel);
         
         if (strcmp(nel->name,SUMA_Dset_Type_Name(SUMA_NODE_ROI))) {
            SUMA_SLP_Err ("ni element not of the \n Node ROI variety.\nElement discarded.");
            NI_free_element(nel) ; nel = NULL;
            SUMA_RETURN(NULL);
         }
         /* somewhat redundant test */
         if (nel->vec_typ[0] != SUMAg_CF->nimlROI_Datum_type) {
            SUMA_SLP_Err ("Datum type mismatch.");
            NI_free_element(nel) ; nel = NULL;
            SUMA_RETURN(NULL);
         }

         if (ForDisplay) {
            /* find out if a displayable object exists with the same idcode_str */
            nel_idcode = NI_get_attribute( nel , "idcode_str");
            if (SUMA_existDO(nel_idcode, SUMAg_DOv, SUMAg_N_DOv)) {
               if (AlwaysReplace) {
                  AddNel = YUP; 
               }
               if (NeverReplace) {
                  AddNel = NOPE;
               }
               if (!AlwaysReplace && !NeverReplace) {   /* ASk */
                  sprintf(stmp, "Found duplicate ROIs.\n"\
                                             "Replace ROI %s (%s) by\n" \
                                             "version in file ?", 
                  NI_get_attribute( nel , "Label"), nel_idcode); 

                  answer = SUMA_AskUser_ROI_replace (SUMAg_SVv[0].X->TOPLEVEL, 
                                    stmp, 
                                    0);
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: Got %d, You ?\n", FuncName, answer);
                  switch (answer) {
                     case SUMA_YES:
                        SUMA_LH("YES");
                        AddNel = YUP;
                        break;

                     case SUMA_NO:
                        SUMA_LH("NO");
                        /* don't add this one */
                        AddNel = NOPE;
                        break;

                     case SUMA_YES_ALL:
                        SUMA_LH("YES ALL");
                        /* cancel Check_Prior */
                        AddNel = YUP;
                        AlwaysReplace = YUP;
                        break;

                     case SUMA_NO_ALL:
                        SUMA_LH("NO ALL");
                        /* don't add this one and set flag to ignore the doubles */
                        AddNel = NOPE;
                        NeverReplace = YUP;
                        break;

                     default:
                        SUMA_SLP_Crit("Don't know what to do with this button.");
                        SUMA_RETURN(NULL);
                        break;
                  }
               } 
            } else {
               AddNel = YUP;
            } 
         
            /* make sure element's parent exists */
            if (AddNel) {
               SUMA_LH("Checking for Parent surface...");
               if ((iDO = SUMA_whichDO(NI_get_attribute( nel , "Parent_idcode_str"), SUMAg_DOv, SUMAg_N_DOv)) < 0) {
                  SUMA_SLP_Err(  "ROI's parent surface\n"
                                 "is not loaded. ROI is\n"
                                 "discarded." );
                  AddNel = NOPE;
               }
            }
         } else {
            AddNel = YUP; /* ignore checks */
         }         
         
         if (AddNel) {
            SUMA_LH("Adding Nel");
            nelv[inel] = nel;
            ++inel; 
         }else {
            SUMA_LH("Skipping Nel");
         }
         
         ++n_read;
      }else {
         found = NOPE;
      } 
      
   } while (found);
   
   NI_stream_close(ns) ;
   N_nel = inel;
   
   if( !n_read){
      SUMA_SL_Err("Found no elements in file!"); 
      SUMA_free(nelv);
      SUMA_RETURN(NULL);
   }
   
   /* Now turn those nel into ROIS */
   ROIv = (SUMA_DRAWN_ROI **) SUMA_malloc(N_nel*sizeof(SUMA_DRAWN_ROI*));
   for (inel=0; inel < N_nel; ++inel) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Processing nel %d/%d...\n", FuncName, inel, N_nel);
      nel = nelv[inel];
      nel_idcode = NI_get_attribute( nel , "idcode_str");

      /* store nel in nimlROI struct */

      /* allocate for nimlROI */
      nimlROI = (SUMA_NIML_DRAWN_ROI *)SUMA_malloc(sizeof(SUMA_NIML_DRAWN_ROI));
      nimlROI->Type = (int)strtod(NI_get_attribute( nel , "Type"), NULL);
      nimlROI->idcode_str = NI_get_attribute( nel , "idcode_str");
      nimlROI->Parent_idcode_str = NI_get_attribute( nel , "Parent_idcode_str");
      nimlROI->Label = NI_get_attribute( nel , "Label");
      nimlROI->iLabel = (int)strtod(NI_get_attribute( nel , "iLabel"), NULL);
      nimlROI->N_ROI_datum = nel->vec_len;
      nimlROI->ColPlaneName = NI_get_attribute( nel , "ColPlaneName");
      if (SUMA_StringToNum (NI_get_attribute( nel , "FillColor"), 
                           nimlROI->FillColor, 3) < 0) {
         SUMA_SLP_Err("Failed in reading FillColor.");
         SUMA_free(nelv);
         SUMA_RETURN(NULL);
      }
      if (SUMA_StringToNum (NI_get_attribute( nel , "EdgeColor"), 
                           nimlROI->EdgeColor, 3) < 0) {
         SUMA_SLP_Err("Failed in reading EdgeColor.");
         SUMA_free(nelv);
         SUMA_RETURN(NULL);
      }
      nimlROI->EdgeThickness = (int)strtod(NI_get_attribute( nel , "EdgeThickness"), NULL);              
      
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: vec_type[0] = %d (%d)\n", 
            FuncName, nel->vec_typ[0], SUMAg_CF->nimlROI_Datum_type) ;
         fprintf (SUMA_STDERR,"%s: vec_len =%d\tvec_num = %d\nidcode_str %s, Parent_idcode_str %s\n",
            FuncName, nel->vec_len, nel->vec_num,
            nimlROI->idcode_str, nimlROI->Parent_idcode_str);
      }

      nimlROI->ROI_datum = (SUMA_NIML_ROI_DATUM *)SUMA_malloc(nimlROI->N_ROI_datum*sizeof(SUMA_NIML_ROI_DATUM));

      /* DO NOT use niml_ROI_datum_buff = (SUMA_NIML_ROI_DATUM *)nel->vec[idat];
      inside the loop.
      For the SUMA_NIML_DRAWN_ROI you have one column of (SUMA_NIML_ROI_DATUM *)
      ni_type = "SUMA_NIML_ROI_DATUM". If you had for type:
      "SUMA_NIML_ROI_DATUM, int" then you'd have two columns with the second
      column being a vector of ints. The only caveat is that the second column
      must be of equal length to the first. */
      niml_ROI_datum_buff = (SUMA_NIML_ROI_DATUM *)nel->vec[0]; 
      /* now fill the ROI_datum structures */
      SUMA_LH("Filling ROI datum structures...");
      for (idat=0; idat< nimlROI->N_ROI_datum ; ++idat) {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: i=%d\n", FuncName, idat);
         nimlROI->ROI_datum[idat].action = niml_ROI_datum_buff[idat].action;
         nimlROI->ROI_datum[idat].Type = niml_ROI_datum_buff[idat].Type;
         nimlROI->ROI_datum[idat].N_n = niml_ROI_datum_buff[idat].N_n;
         if (nimlROI->ROI_datum[idat].N_n > 0) {
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Copying nPath, %d values\n", FuncName, nimlROI->ROI_datum[idat].N_n);
            nimlROI->ROI_datum[idat].nPath = (int *)SUMA_malloc(sizeof(int)*nimlROI->ROI_datum[idat].N_n);
            memcpy(nimlROI->ROI_datum[idat].nPath, niml_ROI_datum_buff[idat].nPath, sizeof(int)*nimlROI->ROI_datum[idat].N_n);
         } else {
            SUMA_LH("Null nPath");
            nimlROI->ROI_datum[idat].nPath = NULL;
         } 
         if (LocalHead) { 
            fprintf (SUMA_STDERR,"%s: Segment %d\tType %d\tN_n %d\taction %d\n", 
               FuncName, idat, nimlROI->ROI_datum[idat].Type,  
               nimlROI->ROI_datum[idat].N_n,nimlROI->ROI_datum[idat].action);
         }
      }

      /* Does ROI already exist with the same idcode_str ?*/
      
      SUMA_LH("Checking for duplicates...");
      if ((iDO = SUMA_whichDO(nel_idcode, SUMAg_DOv, SUMAg_N_DOv)) >= 0) {
         SUMA_LH("Duplicate found ... Deleteing old one...");
         /* ROI already exists delete it */
         if (!SUMA_DeleteROI ((SUMA_DRAWN_ROI *)SUMAg_DOv[iDO].OP)) {
            SUMA_SLP_Err("Failed to delete ROI");
            SUMA_RETURN(NULL); 
         }
      }
      
      /* transfom nimlROI to a series of drawing actions */
      SUMA_LH("Transforming ROI to a series of actions...");
      ROIv[inel] = SUMA_NIMLDrawnROI_to_DrawnROI (nimlROI, ForDisplay);
      if (LocalHead) fprintf (SUMA_STDERR, "%s: ROI->Parent_idcode_str %s\n", FuncName, ROIv[inel]->Parent_idcode_str);

      /* manually free nimlROI fields that received copies of allocated space as opposed to pointer copies */
      SUMA_free(nimlROI->idcode_str);
      SUMA_free(nimlROI->Parent_idcode_str); 
      SUMA_free(nimlROI->Label);
      SUMA_free(nimlROI->ColPlaneName);


      /* free nimlROI */
      nimlROI = SUMA_Free_NIMLDrawROI(nimlROI);

      /* free nel and get it ready for the next load */
      NI_free_element(nel) ; nel = NULL;
            
   }
   
   /* free nelv */
   SUMA_free(nelv);
   
   *N_ROI = N_nel;
   SUMA_RETURN(ROIv);
} 

/*!
   \brief turns a bunch of ROIs into a NI dataset
   
   \param ROIv (SUMA_DRAWN_ROI**) vector of ROI structures
   \param N_ROIv (int) number of ROI structures
   \param Parent_idcode_str (char *) idcode of parent surface
   \return nel (NI_element *) structure to data set
                              NULL if failed
*/
NI_element *SUMA_ROIv2dataset (SUMA_DRAWN_ROI** ROIv, int N_ROIv, char *Parent_idcode_str) 
{
   static char FuncName[]={"SUMA_ROIv2dataset"};
   int ii, i, nn, cnt, N_NodesTotal = 0, 
      *ip=NULL, *NodesTotal=NULL, *LabelsTotal=NULL;
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* Now you have the ROIs, concatenate all NodesInROI vectors into 1*/
   /* count the total number of nodes */
   N_NodesTotal = 0;
   for (ii=0; ii < N_ROIv; ++ii) {
      SUMA_ROI_CRUDE_COUNT_NODES(ROIv[ii], cnt);
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: ROI #%d: %d nodes\n", FuncName, ii, cnt);
      }
      N_NodesTotal += cnt;
   }
   if (LocalHead) fprintf (SUMA_STDERR,"%s: %d nodes total.\n", FuncName, N_NodesTotal);

   NodesTotal = (int *)SUMA_calloc(N_NodesTotal, sizeof(int));
   LabelsTotal = (int *)SUMA_calloc(N_NodesTotal, sizeof(int));

   if (!NodesTotal || !LabelsTotal) {
      SUMA_S_Err("Failed to allocate.");
      SUMA_RETURN(nel);
   }

   cnt = 0;
   N_NodesTotal = 0;
   for (ii=0; ii <  N_ROIv; ++ii) {
      SUMA_LH("Appending ROI");
      /* You do not need the Unique operation in SUMA_NodesInROI,
      but it is nice to have so that you can report the nodes that 
      were part of more than one ROI. If you do not Set the Unique 
      flag in SUMA_NodesInROI you will likely get node duplication
      but these nodes are in the same ROI and therefore are not
      duplicates to be removed....*/
      ip = SUMA_NodesInROI (ROIv[ii], &nn, YUP);
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: Nodes in ROI #%d\n", FuncName, ii);
         SUMA_disp_dvect (ip, nn);
      }
      for (i=0; i < nn; ++i) {
         NodesTotal[cnt] = ip[i];
         LabelsTotal[cnt] = ROIv[ii]->iLabel;
         ++cnt;
      }
      N_NodesTotal += nn;
      SUMA_freeDrawnROI (ROIv[ii]); ROIv[ii] = NULL; /* free the Drawn ROI */
      SUMA_free(ip);ip=NULL;
   }

   if (LocalHead) {
      SUMA_disp_dvect (NodesTotal, N_NodesTotal);
   }

   /* Now you want to make sure that no two nodes are listed twice */
   /* sort NodesTotal and rearrange LabelsTotal accordingly */
   {  int *isort = NULL, *LabelsTotal_r = NULL,
         *NodesTotal_u = NULL, N_NodesTotal_u, *iu = NULL;
      char report[100];

      isort = SUMA_z_dqsort(NodesTotal, N_NodesTotal);
      LabelsTotal_r = SUMA_reorder (LabelsTotal, isort, N_NodesTotal);
      SUMA_free(LabelsTotal);
      LabelsTotal = LabelsTotal_r; LabelsTotal_r = NULL;
      SUMA_free(isort); isort = NULL;

      /* now get the unique set of nodes */
      NodesTotal_u = SUMA_UniqueInt_ind (NodesTotal, N_NodesTotal, &N_NodesTotal_u, &iu);
      /* reorder LabelsTotal to contain data from the nodes left in NodesTotal_u */
      LabelsTotal_r = SUMA_reorder (LabelsTotal, iu, N_NodesTotal_u);
      SUMA_free(NodesTotal); NodesTotal = NULL;
      SUMA_free(LabelsTotal); LabelsTotal = NULL;
      SUMA_free(iu); iu = NULL;
      NodesTotal = NodesTotal_u; NodesTotal_u = NULL;
      LabelsTotal = LabelsTotal_r; LabelsTotal_r = NULL;

      if (N_NodesTotal - N_NodesTotal_u) {
         sprintf(report, "%d/%d nodes had duplicate entries.\n"
                         "(ie same node part of more than 1 ROI)\n"
                         "Duplicate entries were eliminated.", 
                         N_NodesTotal - N_NodesTotal_u , N_NodesTotal);

         N_NodesTotal = N_NodesTotal_u; N_NodesTotal_u = 0;
         SUMA_SLP_Warn(report);
      }
   }

   /* construct a NIML data set for the output */
   SUMA_LH("Creating nel ");
   nel = SUMA_NewNel ( SUMA_NODE_ROI, /* one of SUMA_DSET_TYPE */
                       Parent_idcode_str, /* idcode of Domain Parent */
                       NULL, /* idcode of geometry parent, not useful here*/
                       N_NodesTotal); /* Number of elements */

   if (!nel) {
      SUMA_SL_Err("Failed in SUMA_NewNel");
      SUMA_RETURN(nel);
   }

   /* Add the index column */
   SUMA_LH("Adding index column...");
   if (!SUMA_AddNelCol (nel, SUMA_NODE_INDEX, (void *)NodesTotal, NULL, 1)) {
      SUMA_SL_Err("Failed in SUMA_AddNelCol");
      SUMA_RETURN(nel);
   }

   /* Add the label column */
   SUMA_LH("Adding label column...");
   if (!SUMA_AddNelCol (nel, SUMA_NODE_ILABEL, (void *)LabelsTotal, NULL, 1)) {
      SUMA_SL_Err("Failed in SUMA_AddNelCol");
      SUMA_RETURN(nel);
   }

   if (NodesTotal) SUMA_free(NodesTotal); NodesTotal = NULL;
   if (LabelsTotal) SUMA_free(LabelsTotal); LabelsTotal = NULL;
   
   SUMA_RETURN(nel);
}
   
   
#ifdef SUMA_ROI2dataset_STAND_ALONE
void usage_ROI2dataset_Main ()
   
  {/*Usage*/
      static char FuncName[]={"usage_ROI2dataset_Main"};
      char * s = NULL;
      fprintf(SUMA_STDOUT, 
            "\n"
            "Usage: \n"
            "   ROI2dataset <-prefix dsetname> [...] <-input ROI1 ROI2 ...>\n"
            "               [<-of ni_bi|ni_as|1D>] \n"
            "               [<-dom_par_id idcode>] \n"
          /* "   [<-dom_par domain> NOT IMPLEMENTED YET] \n" */
            "    This program transforms a series of ROI files\n"
            "    to a node dataset. This data set will contain\n"
            "    the node indices in the first column and their\n"
            "    ROI values in the second column.\n"
            "    Duplicate node entries (nodes that are part of\n"
            "    multiple ROIs) will get ignored. You will be\n"
            "    notified when this occurs. \n"
            "\n"
            "Mandatory parameters:\n"
            "    -prefix dsetname: Prefix of output dataset.\n"
            "                      Program will not overwrite existing\n"
            "                      datasets.\n"
            "    -input ROI1 ROI2....: ROI files to turn into a \n"
            "                          data set. This parameter MUST\n"
            "                          be the last one on command line.\n"
            "\n"
            "Optional parameters:\n"
            "(all optional parameters must be specified before the\n"
            " -input parameters.)\n"
            "    -h | -help: This help message\n"
            "    -of FORMAT: Output format of dataset. FORMAT is one of:\n"
            "                ni_bi: NIML binary\n"
            "                ni_as: NIML ascii (default)\n"
            "                1D   : 1D AFNI format.\n"
            "    -dom_par_id id: Idcode of domain parent.\n"
            "                    When specified, only ROIs have the same\n"
            "                    domain parent are included in the output.\n"
            "                    If id is not specified then the first\n"
            "                    domain parent encountered in the ROI list\n"
            "                    is adopted as dom_par_id.\n"
            "                    1D roi files do not have domain parent \n"
            "                    information. They will be added to the \n"
            "                    output data under the chosen dom_par_id.\n"
            "    -prefix dsetname: Prefix of output data set.\n"
            "\n");
         s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
         fprintf(SUMA_STDOUT, 
            "       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \n");
     exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ROI2dataset"}; 
   char  *prefix_name, **input_name_v=NULL, *out_name=NULL, 
         *Parent_idcode_str = NULL, *dummy_idcode_str = NULL, *stmp=NULL;
   int kar, brk, N_input_name, cnt = 0, N_ROIv, N_tROI, ii, i, nn;
   NI_element *nel=NULL;
   NI_stream ns;
   SUMA_DSET_FORMAT Out_Format = SUMA_ASCII_NIML;
   SUMA_DRAWN_ROI ** ROIv = NULL, **tROIv = NULL;
	SUMA_Boolean AddThis = NOPE, LocalHead = NOPE;
	
   /* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
	
   if (argc < 4) {
      usage_ROI2dataset_Main ();
   }
   
   /* parse the command line */
   kar = 1;
	brk = NOPE;
   prefix_name = NULL;
   input_name_v = NULL;
   N_input_name = 0;
   Out_Format = SUMA_ASCII_NIML;
   Parent_idcode_str = NULL;
   while (kar < argc) { /* loop accross command ine options */
		/* SUMA_LH("Parsing command line..."); */
      
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_ROI2dataset_Main();
          exit (1);
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix ");
				exit (1);
			}
			prefix_name = argv[kar];
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-of") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -of ");
				exit (1);
			}
			if (!strcmp(argv[kar], "ni_as")) Out_Format = SUMA_ASCII_NIML;
         else if (!strcmp(argv[kar], "ni_bi")) Out_Format = SUMA_BINARY_NIML;
         else if (!strcmp(argv[kar], "1D")) Out_Format = SUMA_1D;
         else {
            fprintf (SUMA_STDERR, "%s not a valid option with -of.\n", argv[kar]);
				exit (1);
         }   
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-dom_par_id") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -dom_par_id");
				exit (1);
			}
			Parent_idcode_str = SUMA_copy_string(argv[kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need at least one argument after -input ");
				exit (1);
			}
         input_name_v = (char **)SUMA_malloc((argc-kar+1)*sizeof(char *));
         
         cnt = 0;
         while (kar < argc) {
            input_name_v[cnt] = argv[kar];
            ++cnt; ++kar;
         }
         N_input_name = cnt;
         brk = YUP;
      }
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}   
   }   
   
   /* form the output name and check for existence */
   switch (Out_Format) {
      case SUMA_ASCII_NIML:
      case SUMA_BINARY_NIML:
         out_name = SUMA_Extension(prefix_name, ".niml.dset", NOPE); 
         break;
      case SUMA_1D:
         out_name = SUMA_Extension(prefix_name, ".1D.dset", NOPE); 
         break;
      default:
         SUMA_S_Err("Output format not supported");
         exit(1);
         break;
   }
   
   SUMA_LH (out_name);
    
   /* check for existence of out_name */
   if (SUMA_filexists(out_name)) {
      fprintf(SUMA_STDERR,"Error %s:\n Output file %s exists.\n", 
                           FuncName, out_name);
      exit(1); 
   }
   
   /* check for input files */
   if (N_input_name <= 0) {
      fprintf(SUMA_STDERR,"Error %s:\n No ROI files specified.\n",
                           FuncName);
      exit(1); 
   }
    
   /* read in the data sets */
   /* create a dummy idcode_str for potential 1D data sets */
   N_ROIv = 0;
   Parent_idcode_str = NULL;
   dummy_idcode_str = UNIQ_hashcode("DummyNameNothingLikeIt");
   for (i=0; i < N_input_name; ++i) {
      if (SUMA_isExtension(input_name_v[i], ".niml.roi")) {
         /* load niml ROI */
         if (!( tROIv = SUMA_OpenDrawnROI_NIML (input_name_v[i], &N_tROI, NOPE))) {
            SUMA_S_Err("Failed to read NIML ROI.");
            exit(1);
         }
      }else if (SUMA_isExtension(input_name_v[i], ".1D.roi")) {
         /* load 1D ROI */
         if (!( tROIv = SUMA_OpenDrawnROI_1D (input_name_v[i], dummy_idcode_str, &N_tROI, NOPE))) {
            SUMA_S_Err("Failed to read NIML ROI.");
            exit(1);
         }
      }else {
         SUMA_S_Err(  "Failed to recognize\n"
                      "ROI type from filename.");
         exit(1);
      } 
      
      /* copy temporary ROIv into the main ROIv */
      ROIv = (SUMA_DRAWN_ROI **)SUMA_realloc(ROIv, (N_ROIv + N_tROI) * sizeof(SUMA_DRAWN_ROI*));
      if (!ROIv) {
         SUMA_S_Err("Failed to allocate.");
         exit(1);
      }

      /* Now go throught the ROIs and load them if possible into ROIv */
      for (ii=0; ii < N_tROI; ++ii) {
         if (!Parent_idcode_str) {
            /* try to find out what the Parent_idcode_str is */
            if (strcmp(tROIv[ii]->Parent_idcode_str, dummy_idcode_str)) {
               fprintf (SUMA_STDERR,"%s: Adopting Parent_idcode_str (%s) in ROI %s\n",
                                  FuncName, tROIv[ii]->Parent_idcode_str, tROIv[ii]->Label);
               /* good, use it as the Parent_idcode_str for all upcoming ROIs */
               Parent_idcode_str = SUMA_copy_string(tROIv[ii]->Parent_idcode_str);
            }
         } 
         
         AddThis = NOPE;
         if (!strcmp(tROIv[ii]->Parent_idcode_str, dummy_idcode_str)) {
            AddThis = YUP;
         } else {
            if (strcmp(tROIv[ii]->Parent_idcode_str, Parent_idcode_str)) {
               fprintf (SUMA_STDERR,"Warning %s:\n Ignoring ROI labeled %s\n"
                                    "because of Parent_idcode_str mismatch.\n", 
                                    FuncName, tROIv[ii]->Label); 
               AddThis = NOPE;
               /* free structure of tROIv[ii] */
               SUMA_freeDrawnROI (tROIv[ii]); tROIv[ii] = NULL;
            }
            else AddThis = YUP;
            
         }
         if (AddThis) {
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Adding %dth ROI to ROIv...\n",
                            FuncName, N_ROIv);
            ROIv[N_ROIv] = tROIv[ii];
            
            ++N_ROIv;
         }
          
      }
      /* now free tROIv vector */
      if (tROIv) SUMA_free(tROIv); tROIv = NULL;  
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: Kept a total of %d ROIs with parent %s\n",
                        FuncName, N_ROIv, Parent_idcode_str);
        
   }
   
   if (!(nel = SUMA_ROIv2dataset (ROIv, N_ROIv, Parent_idcode_str))) {
      SUMA_SL_Err("Failed in SUMA_ROIv2dataset");
      exit(1);
   }
   
   /* Add the history line */
   if (!SUMA_AddNelHist (nel, FuncName, argc, argv)) {
      SUMA_SL_Err("Failed in SUMA_AddNelHist");
      exit(1);
   }


   
   /* open stream */
   stmp = SUMA_append_string ("file:", out_name);
   ns = NI_stream_open( stmp , "w" ) ;
   if( ns == NULL ){
      fprintf (stderr,"Error  %s:\nCan't open %s!"
                  , FuncName, stmp); 
      exit(1);
   }
   
   /* write nel */
   switch (Out_Format) {
      case SUMA_ASCII_NIML:
         nn = NI_write_element(  ns , nel , NI_TEXT_MODE ); 
         break;
      case SUMA_BINARY_NIML:
         nn = NI_write_element(  ns , nel , NI_BINARY_MODE ); 
         break;
      case SUMA_1D:
         nn = NI_write_element(  ns , nel , NI_TEXT_MODE | NI_HEADERSHARP_FLAG);  
         break;
      default:
         SUMA_S_Err("Output format not supported");
         exit(1);
         break;
   }
 
   if (nn < 0) {
      SUMA_S_Err ("Failed in NI_write_element");
      exit(1);
   }
   
   /* close the stream */
   NI_stream_close( ns ) ; 
   
   /* free nel */
   NI_free_element(nel) ; nel = NULL;
   
   /* free others */
   if (stmp) SUMA_free(stmp);
   if (ROIv) SUMA_free (ROIv);
   if (out_name) SUMA_free(out_name);
   if (Parent_idcode_str) SUMA_free(Parent_idcode_str);
   if (dummy_idcode_str) free(dummy_idcode_str); /* this one's allocated 
                                                   by Bob's functions */
   return(0);
}/* Main */
#endif   

/*!
   \brief handles savinf SO to ascii filename
   
   \param filename (char *)
   \param data(void *) pointer to SUMA_SAVESO_STRUCT containing sv and SO be saved
   
   - This function frees the SUMA_SAVESO_STRUCT before returning
*/
void SUMA_SaveSOascii (char *filename, void *data)
{
   static char FuncName[]={"SUMA_SaveSOascii"};
   char *newname = NULL, *newprefix = NULL, *tmp1= NULL, *tmp2= NULL;
   FILE *Fout = NULL;
   static int answer;
   int ND=-1, NP=-1, ii=-1, id=-1,ip=-1; 
   GLfloat *glar_ColorList = NULL;
   SUMA_SAVESO_STRUCT *SaveSO_data = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SUMA_LH("Called");   
      
   if (!data) {
      SUMA_SLP_Err("NULL data");
      SUMA_RETURNe;
   }
   
   SaveSO_data = (SUMA_SAVESO_STRUCT *)data;
   if (!SaveSO_data->SO || !SaveSO_data->sv) {
      SUMA_SLP_Err("Null SO or Null sv");
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }
   
   /* remove any of the extensions to be used */
   tmp1 = SUMA_Extension(filename, ".1D.xyz", YUP);
   tmp2 = SUMA_Extension(tmp1, ".1D.tri", YUP);
   newprefix = SUMA_Extension(tmp2, ".1D.col", YUP);
   if (tmp1) SUMA_free(tmp1); tmp1 = NULL;
   if (tmp2) SUMA_free(tmp2); tmp2 = NULL;
   
   /* add a .xyz extension */
   if (newname) SUMA_free(newname); newname = NULL;
   newname = SUMA_Extension(newprefix, ".1D.xyz", NOPE); 
   if (!newname) {
      SUMA_SL_Err("Invalid filename");
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }
   SUMA_LH(newname);
   /* check for filename existence */
   if (SUMA_filexists (newname)) {
      answer = SUMA_AskUser_ROI_replace (SUMAg_SVv[0].X->TOPLEVEL, 
                                    "Prefix exists, overwrite?", 
                                    SUMA_NO);
      if (answer == SUMA_NO ||answer == SUMA_NO_ALL) {
         if (newname) SUMA_free(newname); newname = NULL;
         if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
         SUMA_RETURNe; 
      }
   } 

   /* add a .tri extension */
   if (answer != SUMA_YES_ALL && answer != SUMA_YES) {
      if (newname) SUMA_free(newname);newname = NULL;
      newname = SUMA_Extension(newprefix, ".1D.tri", NOPE); 
      if (!newname) {
         SUMA_SL_Err("Invalid filename");
         if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
         SUMA_RETURNe;
      }
      SUMA_LH(newname);
      /* check for filename existence */
      if (SUMA_filexists (newname)) {
         answer = SUMA_AskUser_ROI_replace (SUMAg_SVv[0].X->TOPLEVEL, 
                                       "Prefix exists, overwrite?", 
                                       SUMA_NO);
         if (answer == SUMA_NO ||answer == SUMA_NO_ALL) {
            if (newname) SUMA_free(newname);newname = NULL;
            if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
            SUMA_RETURNe; 
         }
      } 
   }
   
   /* add a .col extension */
   if (answer != SUMA_YES_ALL  && answer != SUMA_YES) {
      if (newname) SUMA_free(newname); newname = NULL;
      newname = SUMA_Extension(newprefix, ".1D.col", NOPE); 
      if (!newname) {
         SUMA_SL_Err("Invalid filename");
         if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
         SUMA_RETURNe;
      }
      SUMA_LH(newname);
      /* check for filename existence */
      if (SUMA_filexists (newname)) {
         answer = SUMA_AskUser_ROI_replace (SUMAg_SVv[0].X->TOPLEVEL, 
                                       "Prefix exists, overwrite?", 
                                       SUMA_NO);
         if (answer == SUMA_NO ||answer == SUMA_NO_ALL) {
            if (newname) SUMA_free(newname);newname = NULL;
            if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
            SUMA_RETURNe; 
         }
      } 
   }
   
   /* OK, names are acceptable, proceed */
   ND = SaveSO_data->SO->NodeDim;
   NP = SaveSO_data->SO->FaceSetDim;

   if (newname) SUMA_free(newname);newname = NULL;
   newname = SUMA_Extension(newprefix, ".1D.xyz", NOPE);  
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Preparing to write .1D.xyz %s.\n", FuncName, newname); 
   Fout = fopen(newname, "w");
   if (Fout == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Could not open file %s for writing.\n", FuncName, newname);
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }

   fprintf(Fout, "#FileContents = Node coordinates\n#RowFormat = X Y Z\n#N_Nodes = %d\n#Source = SUMA, surface %s (idcode: %s)\n",
          SaveSO_data->SO->N_Node, SaveSO_data->SO->Label, SaveSO_data->SO->idcode_str);
   for (ii=0; ii < SaveSO_data->SO->N_Node; ++ii) {
      id = ND * ii;
      fprintf(Fout, "%f\t%f\t%f\n", \
         SaveSO_data->SO->NodeList[id], SaveSO_data->SO->NodeList[id+1],SaveSO_data->SO->NodeList[id+2]);
   }
   fclose (Fout);

   if (newname) SUMA_free(newname);newname = NULL;
   newname = SUMA_Extension(newprefix, ".1D.tri", NOPE);  
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Preparing to write .1D.tri %s.\n", FuncName, newname); 
   Fout = fopen(newname, "w");
   if (Fout == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Could not open file %s for writing.\n", FuncName, newname);
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }
   
   fprintf(Fout, "#FileContents = Triangles\n#RowFormat = n1 n2 n3\n#N_Tri = %d\n#Source = SUMA, surface %s (idcode: %s)\n",
          SaveSO_data->SO->N_FaceSet, SaveSO_data->SO->Label, SaveSO_data->SO->idcode_str);
   for (ii=0; ii < SaveSO_data->SO->N_FaceSet; ++ii) {
      ip = NP * ii;
      fprintf(Fout, "%d\t%d\t%d\n", \
         SaveSO_data->SO->FaceSetList[ip], SaveSO_data->SO->FaceSetList[ip+1],SaveSO_data->SO->FaceSetList[ip+2]);
   }
   fclose (Fout);

   if (newname) SUMA_free(newname);newname = NULL;
   newname = SUMA_Extension(newprefix, ".1D.col", NOPE);  
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Preparing to write .1D.col %s.\n", FuncName, newname); 
   Fout = fopen(newname, "w");
   if (Fout == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Could not open file %s for writing.\n", FuncName, newname);
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }
    glar_ColorList = SUMA_GetColorList (SaveSO_data->sv, SaveSO_data->SO->idcode_str);
    if (!glar_ColorList) {
      fprintf(SUMA_STDERR, "Error %s: NULL glar_ColorList. BAD.\n", FuncName);
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
    }
   fprintf(Fout, "#FileContents = Node Colors\n#RowFormat = n R G B\n#N_Nodes = %d\n#Source = SUMA, surface %s (idcode: %s)\n",
          SaveSO_data->SO->N_Node, SaveSO_data->SO->Label, SaveSO_data->SO->idcode_str);
   for (ii=0; ii < SaveSO_data->SO->N_Node; ++ii) {
      ip = 4 * ii;
      fprintf(Fout, "%d\t%f\t%f\t%f\n", \
         ii, glar_ColorList[ip], glar_ColorList[ip+1], glar_ColorList[ip+2]);
   }
   fclose (Fout);

   if (LocalHead) fprintf(SUMA_STDERR, "%s: Wrote files to disk.\n", 
      FuncName);

   if (newname) SUMA_free(newname);newname = NULL;
   if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
   if (newprefix) SUMA_free(newprefix); 
   SUMA_RETURNe;
}

/*!
   \brief handles saving ROI to filename.
   
   \param filename (char *)
   \param data(void *) pointer to DrawnROI stucture to be saved.
          If you pass null then SUMAg_CF->X->DrawROI->curDrawnROI is used.
*/
void SUMA_SaveDrawnROI (char *filename, void *data)
{
   static char FuncName[]={"SUMA_SaveDrawnROI"};
   SUMA_DRAWN_ROI *DrawnROI=NULL;
   SUMA_SurfaceObject *SO= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SUMA_LH("Called");   
   if (!data) {
      DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
   } else {
      DrawnROI = (SUMA_DRAWN_ROI *)data;
   }
   
   /* is there a DrawnROI to work with ? */
   if (!DrawnROI) {
      SUMA_SLP_Err("No ROI selected.");
      SUMA_RETURNe;
   }
   
   /* Find the parent SO of that ROI */
   SO = SUMA_findSOp_inDOv(DrawnROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   if (!SO) {
      SUMA_SLP_Err("No Parent surface found.");
      SUMA_RETURNe;
   }
   
   /* switch the type of saving */
   switch (SUMAg_CF->X->DrawROI->SaveMode) {
      case SW_DrawROI_SaveMode1D:
         if (!SUMA_SaveDrawnROI_1D (filename, SO, DrawnROI, SUMAg_CF->X->DrawROI->SaveWhat)) {
            SUMA_SLP_Err("Failed to save ROI to disk");
            SUMA_RETURNe;
         }   
         break;
      case SW_DrawROI_SaveModeNIML:
         if (!SUMA_SaveDrawnROINIML (filename, SO, DrawnROI, SUMAg_CF->X->DrawROI->SaveWhat, SUMA_ASCII)) {
            SUMA_SLP_Err("Failed to save ROI to disk");
            SUMA_RETURNe;
         }
         break;
      case SW_DrawROI_SaveMode:
      case SW_N_DrawROI_SaveMode:
      default:
         SUMA_SL_Err("WhatYouTalkinAbout?");
         SUMA_RETURNe;
         break;
   }
   
   SUMA_RETURNe;
} 

SUMA_Boolean SUMA_SaveDrawnROI_1D (char *filename, SUMA_SurfaceObject *SO, SUMA_DRAWN_ROI *DrawnROI, int SaveWhat) 
{
   static char FuncName[]={"SUMA_SaveDrawnROI_1D"};
   char stmp[SUMA_MAX_NAME_LENGTH+20];
   SUMA_DRAWN_ROI **ROIv = NULL;
   int N_ROI=0;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   SUMA_LH("Called");   

   if (SaveWhat == SW_DrawROI_SaveWhatThis) {
      if (!SUMA_Write_DrawnROI_1D (&DrawnROI, 1, filename)) {
         sprintf(stmp,"Failed to write %s", filename);
         SUMA_SLP_Err(stmp);
         SUMA_RETURN(NOPE);
      }
   }else if (SaveWhat == SW_DrawROI_SaveWhatRelated){
      /* get the pointers to the ROIs that are related to SO*/
      if (!(ROIv = SUMA_Find_ROIrelatedtoSO (SO, SUMAg_DOv, SUMAg_N_DOv, &N_ROI))) {
         SUMA_SLP_Err("Failed to write ROIs related to SO.");
         SUMA_RETURN(NOPE);
      }
      if (!SUMA_Write_DrawnROI_1D (ROIv, N_ROI, filename)) {
         sprintf(stmp,"Failed to write %s", filename);
         SUMA_SLP_Err(stmp);
         SUMA_RETURN(NOPE);
      }
        
      if (ROIv) SUMA_free(ROIv);    
   } else {
      SUMA_SLP_Err("SaveWhat option not nderstood");
      SUMA_RETURN(NOPE);
   }
   
   

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_SaveDrawnROINIML (char *filename, SUMA_SurfaceObject *SO, SUMA_DRAWN_ROI *DrawnROI, int SaveWhat, int Format) 
{
   static char FuncName[]={"SaveDrawnROINIML"};
   char stmp[SUMA_MAX_NAME_LENGTH+20];
   SUMA_DRAWN_ROI **ROIv = NULL;
   int N_ROI=0;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   SUMA_LH("Called");   

   if (SaveWhat == SW_DrawROI_SaveWhatThis) {
      if (!SUMA_Write_DrawnROI_NIML (&DrawnROI, 1, filename, Format)) {
         sprintf(stmp,"Failed to write %s", filename);
         SUMA_SLP_Err(stmp);
         SUMA_RETURN(NOPE);
      }
   }else if (SaveWhat == SW_DrawROI_SaveWhatRelated){
      /* get the pointers to the ROIs that are related to SO*/
      if (!(ROIv = SUMA_Find_ROIrelatedtoSO (SO, SUMAg_DOv, SUMAg_N_DOv, &N_ROI))) {
         SUMA_SLP_Err("Failed to write ROIs related to SO.");
         SUMA_RETURN(NOPE);
      }
      if (!SUMA_Write_DrawnROI_NIML (ROIv, N_ROI, filename, Format)) {
         sprintf(stmp,"Failed to write %s", filename);
         SUMA_SLP_Err(stmp);
         SUMA_RETURN(NOPE);
      }
        
      if (ROIv) SUMA_free(ROIv);    
   } else {
      SUMA_SLP_Err("SaveWhat option not nderstood");
      SUMA_RETURN(NOPE);
   }
  
   SUMA_RETURN(YUP);
}

/*!
   \brief writes a vector of SUMA_DRAWN_ROI * to disk in NIML format
*/

SUMA_Boolean SUMA_Write_DrawnROI_NIML (SUMA_DRAWN_ROI **ROIv, int N_ROI, char *filename, int Format) 
{
   static char FuncName[]={"SUMA_Write_DrawnROI_NIML"};
   char stmp[SUMA_MAX_NAME_LENGTH+20];
   char *newname=NULL;
   int i;
   NI_element *nel ;
   NI_stream ns ;
   SUMA_NIML_DRAWN_ROI *niml_ROI = NULL;
   SUMA_DRAWN_ROI *ROI = NULL;
   SUMA_Boolean WriteBin = NOPE, LocalHead = NOPE;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (Format == SUMA_ASCII) WriteBin = NOPE;
   else if (Format == SUMA_BINARY) WriteBin = YUP;
   else {
      SUMA_SL_Err("Wrong format");
      SUMA_RETURN(NOPE);
   }
   
   if (SUMAg_CF->nimlROI_Datum_type < 0) {
      SUMA_SL_Err("Bad niml type code");
      SUMA_RETURN(NOPE);
   }
   if (LocalHead) fprintf(SUMA_STDERR, "%s: roi_type code = %d\n", FuncName, SUMAg_CF->nimlROI_Datum_type) ;

   /* add a .niml.roi extension */
   if (strlen(filename) >= SUMA_MAX_NAME_LENGTH-20) {
      SUMA_SLP_Err("Give me a break, what kind of a filename is this ?");
      SUMA_RETURN(NOPE);
   }

   sprintf(stmp,"file:%s", filename);
   newname = SUMA_Extension(stmp, ".niml.roi", NOPE); 
   SUMA_LH(newname);
   ns = NI_stream_open( newname , "w" ) ;

   /* write the various ROIs */
   for (i=0; i < N_ROI; ++i) {
      ROI = ROIv[i];
      if (!ROI) {
         SUMA_SL_Err("NULL ROI!");
         NI_stream_close( ns ) ; 
         SUMA_RETURN(NOPE);
      }
      /* Transform the ROI to niml friendly structure */
      if (!(niml_ROI = SUMA_DrawnROI_to_NIMLDrawnROI (ROI))) {
         SUMA_SL_Err("NULL niml_ROI!");
         NI_stream_close( ns ) ; 
         SUMA_RETURN(NOPE);
      }
 
      /* Now create a ni element */
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Creating new element of %d segments\n", FuncName, niml_ROI->N_ROI_datum);
      nel = NI_new_data_element(SUMA_Dset_Type_Name(SUMA_NODE_ROI),  niml_ROI->N_ROI_datum);

      SUMA_LH("Adding column...");
      NI_add_column( nel , SUMAg_CF->nimlROI_Datum_type, niml_ROI->ROI_datum );

      SUMA_LH("Setting attributes...");
      NI_set_attribute (nel, "idcode_str", niml_ROI->idcode_str);
      NI_set_attribute (nel, "Parent_idcode_str", niml_ROI->Parent_idcode_str);
      NI_set_attribute (nel, "Label", niml_ROI->Label);
      sprintf(stmp,"%d", niml_ROI->iLabel);
      NI_set_attribute (nel, "iLabel", stmp);
      sprintf(stmp,"%d", niml_ROI->Type);
      NI_set_attribute (nel, "Type", stmp);
      NI_set_attribute (nel, "ColPlaneName", niml_ROI->ColPlaneName);
      sprintf(stmp,"%f %f %f", niml_ROI->FillColor[0], niml_ROI->FillColor[1],
                              niml_ROI->FillColor[2]);
      NI_set_attribute (nel, "FillColor",stmp);
      sprintf(stmp,"%f %f %f", niml_ROI->EdgeColor[0], niml_ROI->EdgeColor[1],
                              niml_ROI->EdgeColor[2]);
      NI_set_attribute (nel, "EdgeColor",stmp);
      sprintf(stmp,"%d", niml_ROI->EdgeThickness);
      NI_set_attribute (nel, "EdgeThickness", stmp);                   
      
      if (LocalHead) SUMA_nel_stdout (nel);

      if (!WriteBin) {
         SUMA_LH ("Writing element, Text mode.");
         if (NI_write_element( ns , nel , NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) < 0) {
           SUMA_SL_Err("Badness, failed to write nel");
           NI_stream_close( ns ) ; 
           SUMA_RETURN(NOPE);
         }
      } else {
         SUMA_LH ("Writing element, Binary mode.");
         if (NI_write_element( ns , nel , NI_BINARY_MODE) < 0) {
           SUMA_SL_Err("Badness, failed to write nel");
           NI_stream_close( ns ) ; 
           SUMA_RETURN(NOPE);
         }
      } 

      /* free nel */
      NI_free_element(nel) ; nel = NULL;

      /* free the niml_ROI structure */
      niml_ROI = SUMA_Free_NIMLDrawROI (niml_ROI); niml_ROI = NULL;
      
   }
   
   NI_stream_close( ns ) ; 
   
   if (newname) SUMA_free(newname);
   
   SUMA_RETURN(YUP);
}


/*!
   \brief A function to take a SUMA_DRAWN_ROI struct and return an equivalent
   SUMA_1D_DRAWN_ROI struct. 
   
   - Do not free SUMA_1D_DRAWN_ROI manually, many of its fields are 
   pointer copies of values in SUMA_DRAWN_ROI.
   
   \sa SUMA_Free_1DDrawROI
*/
SUMA_1D_DRAWN_ROI * SUMA_DrawnROI_to_1DDrawROI (SUMA_DRAWN_ROI *ROI)
{
   static char FuncName[]={"SUMA_DrawnROI_to_1DDrawROI"};
   SUMA_1D_DRAWN_ROI *ROI_1D=NULL;
   SUMA_ROI_DATUM *ROI_Datum=NULL;
   DListElmt *Elm = NULL;
   int i = -1, cnt = 0, *isort=NULL, *iLabel=NULL, *iNode=NULL;
   SUMA_Boolean LocalHead = NOPE;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!ROI) {
      SUMA_SL_Err("Null ROI");
      SUMA_RETURN(NULL);
   }
   
   /* count the total number of nodes in ROI */
   
   /* allocate for nimlROI */
   ROI_1D = (SUMA_1D_DRAWN_ROI *)SUMA_malloc(sizeof(SUMA_1D_DRAWN_ROI));
   Elm = NULL;
   ROI_1D->N = 0;
   do {
      if (!Elm) Elm = dlist_head(ROI->ROIstrokelist); 
      else Elm = Elm->next;
      ROI_Datum = (SUMA_ROI_DATUM *)Elm->data;
      ROI_1D->N += ROI_Datum->N_n;
   } while (Elm != dlist_tail(ROI->ROIstrokelist));
      
   ROI_1D->Type = (int)ROI->Type;
   ROI_1D->idcode_str = ROI->idcode_str;
   ROI_1D->Parent_idcode_str = ROI->Parent_idcode_str;
   ROI_1D->Label = ROI->Label;
   ROI_1D->iNode = NULL;
   ROI_1D->iLabel = NULL;
   iNode = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   iLabel = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   if (!iNode || !iLabel) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   /* now fill the node indices and the node values */
   Elm = NULL;
   cnt = 0;
   do {
      if (!Elm) Elm = dlist_head(ROI->ROIstrokelist);
      else Elm = Elm->next;
      ROI_Datum = (SUMA_ROI_DATUM *)Elm->data;
      for (i=0; i < ROI_Datum->N_n; ++i) {
         iNode[cnt] = ROI_Datum->nPath[i];
         iLabel[cnt] = ROI->iLabel;
         ++cnt;
      }
   } while (Elm != dlist_tail(ROI->ROIstrokelist));
   
   /* some node entries are redundant, clear those up */
   /* first sort iNode */
   isort = SUMA_z_dqsort( iNode, ROI_1D->N);

   /* Now sort the labels accordingly */
   ROI_1D->iLabel = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   ROI_1D->iNode = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   if (!ROI_1D->iNode || !ROI_1D->iLabel) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   for (i=0; i < ROI_1D->N; ++i) {
      ROI_1D->iLabel[i] = iLabel[isort[i]];
   }
   if (iLabel) SUMA_free(iLabel); iLabel = NULL; /* done with unsorted version of iLabel */
   
   /* Now remove redundant entries */
   cnt = 0;
   ROI_1D->iNode[cnt] = iNode[0]; 
   ROI_1D->iLabel[cnt] = ROI_1D->iLabel[0];
   ++cnt;
   for (i=1;i<ROI_1D->N;++i)
    {
      if ((iNode[i] != iNode[i- 1]))
         {
            ROI_1D->iNode[cnt] = iNode[i];
            ROI_1D->iLabel[cnt] = ROI_1D->iLabel[i]; 
            ++cnt;   
         }
   }

   
   /* you would reallocate here, because cnt is always <= ROI_1D->N,
   but it is not worth the effort because cnt is only slightly
   less than ROI_1D->N */
   
   /* just update ROI_1D->N */
   ROI_1D->N = cnt;
   
   if (isort) SUMA_free(isort); isort = NULL;
   if (iNode) SUMA_free(iNode); iNode = NULL;
   
   SUMA_RETURN(ROI_1D);
}

/*!
   \brief frees a ROI_1D structure. These structures are created by
    the likes of SUMA_DrawnROI_to_1DDrawROI
    
    \sa SUMA_DrawnROI_to_1DDrawROI
*/
SUMA_1D_DRAWN_ROI * SUMA_Free_1DDrawROI (SUMA_1D_DRAWN_ROI *ROI_1D)
{
   static char FuncName[]={"SUMA_Free_1DDrawROI"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!ROI_1D) SUMA_RETURN(NULL);
   
   if (ROI_1D->iLabel) SUMA_free(ROI_1D->iLabel); 
   if (ROI_1D->iNode) SUMA_free(ROI_1D->iNode);
   
   SUMA_free(ROI_1D);
   
   SUMA_RETURN(NULL);
}


/*!
   \brief writes a vector of SUMA_DRAWN_ROI * to disk in afni's 1D ascii format
*/

SUMA_Boolean SUMA_Write_DrawnROI_1D (SUMA_DRAWN_ROI **ROIv, int N_ROI, char *filename) 
{
   static char FuncName[]={"SUMA_Write_DrawnROI_1D"};
   char *newname=NULL;
   int i,j;
   SUMA_1D_DRAWN_ROI *ROI_1D = NULL;
   SUMA_DRAWN_ROI *ROI = NULL;
   FILE *fout=NULL;
   SUMA_Boolean LocalHead = NOPE;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* add a .1D.roi extension */
   newname = SUMA_Extension(filename, ".1D.roi", NOPE); 
   if (!newname) {
      SUMA_SL_Err("Invalid filename");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_LH(newname);

   fout = fopen(newname,"w");
   if (!fout) {
      SUMA_SL_Err("Failed to open file for writing.");
      SUMA_RETURN(NOPE);
   }

   /* write the various ROIs */
   for (i=0; i < N_ROI; ++i) {
      ROI = ROIv[i];
      if (!ROI) {
         SUMA_SL_Err("NULL ROI!");
         fclose(fout);
         SUMA_RETURN(NOPE);
      }
      /* Transform the ROI to niml friendly structure */
      if (!(ROI_1D = SUMA_DrawnROI_to_1DDrawROI (ROI))) {
         SUMA_SL_Err("NULL niml_ROI!");
         fclose(fout);
         SUMA_RETURN(NOPE);
      }

      /* write it out in a NIML friendly format */
      /* begin with attributes */
      fprintf (fout,"# %s\n", SUMA_Dset_Type_Name(SUMA_NODE_ROI));
      fprintf (fout,"#  ni_type = \"SUMA_1D_ROI_DATUMorint,int?\"\n");
      fprintf (fout,"#  ni_dimen = \"%d\"\n",  ROI_1D->N);
      fprintf (fout,"#  ni_datasize = \"???\"\n");
      fprintf (fout,"#  idcode_str = \"%s\"\n", ROI_1D->idcode_str);
      fprintf (fout,"#  Parent_idcode_str = \"%s\"\n", ROI_1D->Parent_idcode_str);
      fprintf (fout,"#  Label = \"%s\"\n", ROI_1D->Label);
      fprintf (fout,"# >\n");
      for (j=0; j < ROI_1D->N; ++j) 
         fprintf (fout," %d %d\n", ROI_1D->iNode[j], ROI_1D->iLabel[j]);
      fprintf (fout,"# </%s>\n", SUMA_Dset_Type_Name(SUMA_NODE_ROI));
      fprintf (fout,"\n");
       
      /* free the ROI_1D structure */
      ROI_1D = SUMA_Free_1DDrawROI (ROI_1D); ROI_1D = NULL;
   }
   
   fclose(fout) ; 
   if (newname) SUMA_free(newname);
   
   SUMA_RETURN(YUP);
}
