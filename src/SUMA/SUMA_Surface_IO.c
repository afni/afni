
#include "SUMA_suma.h"

#undef STAND_ALONE

#if defined SUMA_SureFit_STAND_ALONE
#define STAND_ALONE 
#elif defined SUMA_FreeSurfer_STAND_ALONE
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
