
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

   if (SUMA_filexists(Fname->name_coord)) {
      fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_coord);
      SUMA_RETURN (NOPE);
   }
   if (SUMA_filexists(Fname->name_topo)) {
      fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_topo);
      SUMA_RETURN (NOPE);
   }
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: Must have NodeDim and FaceSetDim = 3.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
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
		fprintf (SUMA_STDERR,"Error %s: Idetincal values for AC and AC_WholeVolume. Check you params file.\n", FuncName);
		SUMA_RETURN (NOPE);
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
   
   
   
/*!**  
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
		SUMA_free(FS->NodeList);
		SUMA_free(FS->NodeId);
      
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
    for (j = 0; j < nprops; j++)
      fprintf (SUMA_STDERR, "%s property %s\n", FuncName, plist[j]->name);
   }

   /* grab and print out the comments in the file */
   comments = ply_get_comments (ply, &num_comments);
   for (i = 0; i < num_comments; i++)
    fprintf (SUMA_STDERR, "%s comment = '%s'\n", FuncName, comments[i]);

   /* grab and print out the object information */
   obj_info = ply_get_obj_info (ply, &num_obj_info);
   for (i = 0; i < num_obj_info; i++)
    fprintf (SUMA_STDERR, "%s obj_info = '%s'\n", FuncName, obj_info[i]);

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

   if (SUMA_filexists(Fname->name_coord)) {
      fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_coord);
      SUMA_RETURN (NOPE);
   }
   if (SUMA_filexists(Fname->name_topo)) {
      fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_topo);
      SUMA_RETURN (NOPE);
   }
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: Must have NodeDim and FaceSetDim = 3.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
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
          printf ("\n\33[1mUsage: \33[0m SUMA_ConvertSurface <-i_TYPE inSurf> <-o_TYPE outSurf> [<-sv SurfaceVolume [VolParam for sf surfaces]>] [-tlrc]\n");
          printf ("\t reads in a surface and writes it out in another format.\n");
          printf ("\t Note: This is a not a general utility conversion program. \n");
          printf ("\t Only fields pertinent to SUMA are preserved.\n");
          printf ("\t -i_TYPE inSurf specifies the input surface, TYPE is one of the following:\n");
          printf ("\t    fs: FreeSurfer surface. \n");
          printf ("\t        Only .asc surfaces are read.\n");
          printf ("\t    sf: SureFit surface. \n");
          printf ("\t        You must specify the .coord followed by the .topo file.\n");
          printf ("\t    vec: Simple ascii matrix format. \n");
          printf ("\t         You must specify the NodeList file followed by the FaceSetList file.\n");
          printf ("\t         NodeList contains 3 floats per line, representing X Y Z vertex coordinates.\n");
          printf ("\t         FaceSetList contains 3 ints per line, representing v1 v2 v3 triangle vertices.\n");
          printf ("\t    ply: PLY format, ascii or binary.\n");
          printf ("\t         Only vertex and triangulation info is preserved.\n");
          printf ("\t -o_TYPE outSurf specifies the output surface, TYPE is one of the following:\n");
          printf ("\t    fs: FreeSurfer ascii surface. \n");
          printf ("\t    sf: SureFit surface. (NOT IMPLEMENTED YET)\n");
          printf ("\t        You must specify the .coord followed by the .topo file.\n");
          printf ("\t    vec: Simple ascii matrix format. \n");
          printf ("\t         see help for vec under -i_TYPE options for format specifications.\n");
          printf ("\t    ply: PLY format, ascii or binary.\n");
          printf ("\t -sv SurfaceVolume [VolParam for sf surfaces]\n");
          printf ("\t    This option must not come before the -i_TYPE option.\n");
          printf ("\t    If you supply a surface volume, the coordinates of the input surface.\n");
          printf ("\t     are modified to SUMA's convention and aligned with SurfaceVolume.\n");
          printf ("\t     You must also specify a VolParam file for SureFit surfaces.\n");
          printf ("\t -tlrc: Apply taliairach transform (which must be in talairach version of SurfaceVolume)\n");
          printf ("\t     to the surface vertex coordinates. This option must be used with the -sv option.\n");
          printf ("\tNOTE: The vertex coordinates coordinates of the input surfaces are only\n");
          printf ("\t      transformed if -sv option is used. If you do transform surfaces, \n");
          printf ("\t      take care not to load them into SUMA with another -sv option.\n");  
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \t Wed Jan  8 13:44:29 EST 2003 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_ConvertSurface"}; 
	int kar;
   char *if_name = NULL, *of_name = NULL, *if_name2 = NULL, *of_name2 = NULL, *sv_name = NULL, *vp_name = NULL, *OF_name = NULL, *OF_name2 = NULL, *tlrc_name = NULL;
   SUMA_SO_File_Type iType = SUMA_FT_NOT_SPECIFIED, oType = SUMA_FT_NOT_SPECIFIED;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_PARSED_NAME *of_name_strip = NULL, *of_name2_strip = NULL;
   SUMA_SFname *SF_name = NULL;
   void *SO_name = NULL;
   THD_warp *warp=NULL ;
   THD_3dim_dataset *aset=NULL;
   SUMA_Boolean brk, Do_tlrc;
   
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
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_ConvertSurface();
          exit (1);
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
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   /* sanity checks */
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
   
   SUMA_Print_Surface_Object (SO, stderr);
   
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
   \param data(void *) */
void SUMA_OpenDrawnROI (char *filename, void *data)
{
   static char FuncName[]={"SUMA_OpenDrawnROI"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SUMA_LH("Called");   

   /* check for type ... */
   /* NOT DONE YET */
   
   /* load niml ROI */
   if (!SUMA_OpenDrawnROI_NIML (filename)) {
      SUMA_SLP_Err("Failed to read NIML ROI.");
      SUMA_RETURNe;
   }
   
   SUMA_RETURNe; 
}

SUMA_Boolean SUMA_OpenDrawnROI_NIML (char *filename)
{ /* begin embedded function */
   static char FuncName[]={"SUMA_OpenDrawnROI_NIML"};
   char stmp[SUMA_MAX_NAME_LENGTH+100];
   NI_element *nel = NULL;
   NI_stream ns ;
   int n_read=0, i;
   SUMA_NIML_ROI_DATUM *niml_ROI_datum_buff=NULL;
   SUMA_NIML_DRAWN_ROI * nimlROI=NULL;
   SUMA_DRAWN_ROI *ROI=NULL;
   SUMA_Boolean found = YUP, LocalHead = YUP;
   
   if (SUMAg_CF->nimlROI_Datum_type < 0) {
      SUMA_SL_Err("Bad niml type code");
      SUMA_RETURN(NOPE);
   }
   if (LocalHead) fprintf(SUMA_STDERR, "%s: roi_type code = %d\n", FuncName, SUMAg_CF->nimlROI_Datum_type) ;

   sprintf(stmp,"file:%s", filename);
   ns = NI_stream_open( stmp , "r" ) ;
   if( ns == NULL ){
      SUMA_SL_Err("Can't open ROI file"); 
      SUMA_RETURN(NOPE);
   }
   
   do {
      if (nel) {
         SUMA_SL_Err ("nel is not null!");
         SUMA_RETURN(NOPE);
      }
      nel = NI_read_element(ns,1) ;
      
      if (nel) {
         found = YUP;
         
         if (LocalHead && 0) SUMA_nel_stdout (nel);
         
         if (strcmp(nel->name,"A_drawn_ROI")) {
            SUMA_SLP_Err ("ni element not of the \n'A_drawn_ROI' variety.\nElement discarded.");
            SUMA_RETURN(NOPE);
         }
         /* somewhat redundant test */
         if (nel->vec_typ[0] != SUMAg_CF->nimlROI_Datum_type) {
            SUMA_SLP_Err ("Datum type mismatch.");
            SUMA_RETURN(NOPE);
         }

         /* find out if a displayable object exists with the same idcode_str */
         if (SUMA_existDO(NI_get_attribute( nel , "idcode_str"), SUMAg_DOv, SUMAg_N_DOv)) {
            sprintf(stmp, "Found duplicate ROIs.\n"\
                                       "Replace ROI %s (%s) by\n" \
                                       "version in file ?", 
                  NI_get_attribute( nel , "Label"), NI_get_attribute( nel , "idcode_str")); 
            switch (AskUser (SUMAg_SVv[0].X->TOPLEVEL, 
                              stmp, 
                              "Yes", "No", 1)) {
               case 1:
                  SUMA_LH("1");
                  break;
                  
               case 2:
                  SUMA_LH("2");
                  break;
               
               default:
                  SUMA_LH("default");
                  break;
            }
         }
         /* store nel in nimlROI struct */
         
         /* allocate for nimlROI */
         nimlROI = (SUMA_NIML_DRAWN_ROI *)SUMA_malloc(sizeof(SUMA_NIML_DRAWN_ROI));
         nimlROI->Type = (int)strtod(NI_get_attribute( nel , "Type"), NULL);
         nimlROI->idcode_str = NI_get_attribute( nel , "idcode_str");
         nimlROI->Parent_idcode_str = NI_get_attribute( nel , "Parent_idcode_str");
         nimlROI->Label = NI_get_attribute( nel , "Label");
         nimlROI->iLabel = (int)strtod(NI_get_attribute( nel , "iLabel"), NULL);
         nimlROI->N_ROI_datum = nel->vec_len;
         niml_ROI_datum_buff = nel->vec[0];
         if (LocalHead) {
            fprintf (SUMA_STDERR,"%s: vec_type[0] = %d (%d)\n", 
               FuncName, nel->vec_typ[0], SUMAg_CF->nimlROI_Datum_type) ;
            fprintf (SUMA_STDERR,"%s: vec_len =%d\tvec_num = %d\nidcode_str %s, Parent_idcode_str %s\n",
               FuncName, nel->vec_len, nel->vec_num,
               nimlROI->idcode_str, nimlROI->Parent_idcode_str);
         }
                
         nimlROI->ROI_datum = (SUMA_NIML_ROI_DATUM *)SUMA_malloc(nimlROI->N_ROI_datum*sizeof(SUMA_NIML_ROI_DATUM));
         
         /* now fill the ROI_datum structures */
          
         for (i=0; i< nimlROI->N_ROI_datum ; ++i) {
            nimlROI->ROI_datum[i].action = niml_ROI_datum_buff[i].action;
            nimlROI->ROI_datum[i].Type = niml_ROI_datum_buff[i].Type;
            nimlROI->ROI_datum[i].N_n = niml_ROI_datum_buff[i].N_n;
            if (nimlROI->ROI_datum[i].N_n > 0) {
               nimlROI->ROI_datum[i].nPath = (int *)SUMA_malloc(sizeof(int)*nimlROI->ROI_datum[i].N_n);
               memcpy(nimlROI->ROI_datum[i].nPath, niml_ROI_datum_buff[i].nPath, sizeof(int)*nimlROI->ROI_datum[i].N_n);
            } else {
               nimlROI->ROI_datum[i].nPath = NULL;
            } 
            if (LocalHead) { 
               fprintf (SUMA_STDERR,"%s: Segment %d\tType %d\tN_n %d\taction %d\n", 
                  FuncName, i, nimlROI->ROI_datum[i].Type,  
                  nimlROI->ROI_datum[i].N_n,nimlROI->ROI_datum[i].action);
            }
         }

         /* transfom nimlROI to a series of ROI actions */
         ROI = SUMA_NIMLDrawnROI_to_DrawnROI (nimlROI);
         if (LocalHead) fprintf (SUMA_STDERR, "%s: ROI->Parent_idcode_str %s\n", FuncName, ROI->Parent_idcode_str);
         
         /* add ROI to DO list */
         if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROI, ROIdO_type, SUMA_LOCAL)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
         }

         /* free nel and get it ready for the next load */
         NI_free_element(nel) ; nel = NULL;
         
         /* free nimlROI */
         nimlROI = SUMA_Free_NIMLDrawROI(nimlROI);
         
         ++n_read;
      }else {
         found = NOPE;
      } 
      
   } while (found);
   
   NI_stream_close(ns) ;

   if( !n_read){
     SUMA_SL_Err("Found no elements in file!"); 
     SUMA_RETURN(NOPE);
   }
   
   
   SUMA_RETURN(YUP);
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
   SUMA_Boolean LocalHead = YUP;
   
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
   SUMA_Boolean LocalHead = YUP;
   
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
   SUMA_Boolean LocalHead = YUP;
   
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
   int i;
   NI_element *nel ;
   NI_stream ns ;
   SUMA_NIML_DRAWN_ROI *niml_ROI = NULL;
   SUMA_DRAWN_ROI *ROI = NULL;
   SUMA_Boolean WriteBin = NOPE, LocalHead = YUP;

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

   sprintf(stmp,"file:%s", filename);
   ns = NI_stream_open( stmp , "w" ) ;

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
      nel = NI_new_data_element("A_drawn_ROI",  niml_ROI->N_ROI_datum);

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

      if (LocalHead) SUMA_nel_stdout (nel);

      /* Now write the element */
      if (strlen(filename) >= SUMA_MAX_NAME_LENGTH-20) {
         SUMA_SLP_Err("Give me a break, what kind of a filename is this ?");
         NI_stream_close( ns ) ; 
         SUMA_RETURN(NOPE);
      }
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
   int i = -1, cnt = 0;
   SUMA_Boolean LocalHead = YUP;

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
   ROI_1D->iNode = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   ROI_1D->iLabel = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   if (!ROI_1D->iNode || !ROI_1D->iLabel) {
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
         ROI_1D->iNode[cnt] = ROI_Datum->nPath[i];
         ROI_1D->iLabel[cnt] = ROI->iLabel;
         ++cnt;
      }
   } while (Elm != dlist_tail(ROI->ROIstrokelist));
   
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
   SUMA_Boolean LocalHead = YUP;
   
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
   char stmp[SUMA_MAX_NAME_LENGTH+20];
   int i,j;
   SUMA_1D_DRAWN_ROI *ROI_1D = NULL;
   SUMA_DRAWN_ROI *ROI = NULL;
   FILE *fout=NULL;
   SUMA_Boolean LocalHead = YUP;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   fout = fopen(filename,"w");
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
      fprintf (fout,"# <A_drawn_ROI\n");
      fprintf (fout,"#  ni_type = \"SUMA_1D_ROI_DATUMorint,int?\"\n");
      fprintf (fout,"#  ni_dimen = \"%d\"\n",  ROI_1D->N);
      fprintf (fout,"#  ni_datasize = \"???\"\n");
      fprintf (fout,"#  idcode_str = \"%s\"\n", ROI_1D->idcode_str);
      fprintf (fout,"#  Parent_idcode_str = \"%s\"\n", ROI_1D->Parent_idcode_str);
      fprintf (fout,"#  Label = \"%s\"\n", ROI_1D->Label);
      fprintf (fout,"# >\n");
      for (j=0; j < ROI_1D->N; ++j) 
         fprintf (fout," %d %d\n", ROI_1D->iNode[j], ROI_1D->iLabel[j]);
      fprintf (fout,"# </A_drawn_ROI>\n");
      fprintf (fout,"\n");
       
      /* free the ROI_1D structure */
      ROI_1D = SUMA_Free_1DDrawROI (ROI_1D); ROI_1D = NULL;
   }
   
   fclose(fout) ; 
   
   SUMA_RETURN(YUP);
}
