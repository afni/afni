
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
#endif
   
/* CODE */
   
   
   
/*!**
File : SUMA_SureFit.c
\author Ziad Saad
Date : Fri Feb 8 16:29:06 EST 2002
   
Purpose : 
   Read SureFit data
   
*/
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
	fprintf (Out, "Last 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		SF->NodeId[SF->N_Node-2], SF->NodeList[ND*(SF->N_Node-2)], SF->NodeList[ND*(SF->N_Node-2)+1], SF->NodeList[ND*(SF->N_Node-2)+2],
		SF->NodeId[SF->N_Node-1], SF->NodeList[ND*(SF->N_Node-1)], SF->NodeList[ND*(SF->N_Node-1)+1], SF->NodeList[ND*(SF->N_Node-1)+2]);
	fprintf (Out, "\n%s: Topo Info\n", SF->name_topo);
	fprintf (Out, "N_Node_Specs %d\n", SF->N_Node_Specs);
	fprintf (Out, "ecnoding_topo: %s, date %s\n",  SF->encoding_topo, SF->date);
	fprintf (Out, "N_FaceSet %d\n", SF->N_FaceSet);
	fprintf (Out, "First 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
		SF->FaceSetList[0], SF->FaceSetList[1], SF->FaceSetList[2],
		SF->FaceSetList[3], SF->FaceSetList[4], SF->FaceSetList[5]);
	fprintf (Out, "Last 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
		SF->FaceSetList[NP*(SF->N_FaceSet-2)], SF->FaceSetList[NP*(SF->N_FaceSet-2) + 1], SF->FaceSetList[NP*(SF->N_FaceSet-2) + 2],
		SF->FaceSetList[NP*(SF->N_FaceSet-1)], SF->FaceSetList[NP*(SF->N_FaceSet-1) + 1], SF->FaceSetList[NP*(SF->N_FaceSet-1) + 2]);
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
   
   In its current incarnation, the function does overwrite a pre-existing file.
      
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
   char *elem_names[] = { "vertex", "face" };/* list of the kinds of elements in the user's object */
   int n_elem_names = 2;
   Vertex **verts = NULL;
   Face *faces = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   
   The function will overwrite pre-existing files!
   Written by Brenna Bargall
*/
SUMA_Boolean SUMA_FS_Write (char *fileNm, SUMA_SurfaceObject *SO, char *firstLine) 
{
   static char FuncName[]={"SUMA_FS_Write"};
   int i, j;
   FILE *outFile = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   The function will overwrite pre-existing files!
   
*/
SUMA_Boolean SUMA_VEC_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO)
{
   
   static char FuncName[]={"SUMA_VEC_Write"};
   int i, j;
   FILE *outFile = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
          printf ("\n\33[1mUsage: \33[0m SUMA_ConvertSurface <-i_TYPE inSurf> <-o_TYPE outSurf> [<-sv SurfaceVolume [VolParam for sf surfaces]>]\n");
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
   char *if_name = NULL, *of_name = NULL, *if_name2 = NULL, *of_name2 = NULL, *sv_name = NULL, *vp_name = NULL, *OF_name = NULL, *OF_name2 = NULL;
   SUMA_SO_File_Type iType = SUMA_FT_NOT_SPECIFIED, oType = SUMA_FT_NOT_SPECIFIED;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_PARSED_NAME *of_name_strip = NULL, *of_name2_strip = NULL;
   SUMA_SFname *SF_name = NULL;
   void *SO_name = NULL;
   SUMA_Boolean brk;
   
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
      if (!of_name2) {
       fprintf (SUMA_STDERR,"Error %s: output SureFit surface incorrectly specified. \n", FuncName);
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
   
   fprintf (SUMA_STDOUT,"Reading surface...\n");
   
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
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_SUREFIT, SUMA_ASCII, vp_name);
         break;
      case SUMA_VEC:
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         sprintf(SF_name->name_coord,"%s", if_name);
         sprintf(SF_name->name_topo,"%s", if_name2); 
         SO_name = (void *)SF_name;
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_VEC, SUMA_ASCII, NULL);
         break;
      case SUMA_FREE_SURFER:
         SO_name = (void *)if_name; 
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
         break;  
      case SUMA_PLY:
         SO_name = (void *)if_name; 
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_PLY, SUMA_FF_NOT_SPECIFIED, NULL);
         break;  
      default:
         fprintf (SUMA_STDERR,"Error %s: Bad format.\n", FuncName);
         exit(1);
   }
   
   if (!SO) {
      fprintf (SUMA_STDERR,"Error %s: Failed to read input surface.\n", FuncName);
      exit (1);
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
