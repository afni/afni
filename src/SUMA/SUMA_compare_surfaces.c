/*Program to compute the minimum distance metric between two surfaces */

/* Header FILES */
   
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs stored in SVv */
SUMA_DO *SUMAg_DOv;	/*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */

/* function declarations */
float maximum(int N, float *inarray);
float minimum(int N, float *inarray);
void cmp_surf_usage ();

/*
File : SUMA_compare_surfaces.c
authors : Shruti Japee and Ziad Saad
Date : Thu August 1 10:24:01 EST 2002
Purpose : Two determine the min distance between nodes on one surface to faces on another.
*/

int main (int argc,char *argv[])
{
  static char FuncName[]={"SUMA_compare_surfaces"}; 
  SUMA_SurfSpecFile Spec;
  char *specfilename = NULL;
  int kar, id;
  SUMA_Boolean brk;
  SUMA_Boolean SurfIn = NOPE;

  SUMA_SurfaceObject *Surf1 = NULL, *Surf2=NULL;
  char *Surf1_FileName=NULL;
  char *Surf2_FileName=NULL;
  char *Vol1Parent_FileName=NULL;
  char *Vol2Parent_FileName=NULL;
  
  /* for SureFit surface */
  SUMA_SFname *Surf1_SFName=NULL, *Surf2_SFName=NULL;
   
  /* other variables */
  int i;
  int num_nodes1;
  int num_nodes2;  
  float P0[3];
  float delta_t; 
  float P1[3];
  float N0[3];
  float maxdistance, mindistance;
  float *distance;
  float **Points;
  SUMA_COLOR_MAP *MyColMap;
  SUMA_SCALE_TO_MAP_OPT *MyOpt;
  SUMA_COLOR_SCALED_VECT * MySV;
  SUMA_MT_INTERSECT_TRIANGLE *triangle;
  SUMA_SURF_NORM SN1;
  SUMA_SURF_NORM SN2;
  SUMA_SurfaceObject *SO1, *SO2;
  struct timeval tt; 
  FILE *colorfile;
  FILE *distancefile;
  char colorfilename[1000];
  char distancefilename[1000];
  char *tag1 = NULL;
  char *tag2 = NULL;
  char *state1 = NULL;
  char *state2 = NULL;
  char *hemi = NULL;
  float B_dim[3];
  SUMA_ISINBOX isin;
  SUMA_PATCH *Patch=NULL;
  SUMA_Boolean TryFull = NOPE, FullOnly;
  SUMA_MEMBER_FACE_SETS *Memb = NULL;
  int *FaceSet_tmp;
  int N_FaceSet_tmp;
  char *fout = NULL;

  if (argc < 9) {
    cmp_surf_usage();
    exit (1);
  }
  
  /* allocate space for CommonFields structure */
  SUMAg_CF = SUMA_Create_CommonFields ();
  if (SUMAg_CF == NULL) {
    fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
    exit(1);
  }
  
  /* read in the surfaces */
  kar = 1;
  brk = NOPE;
  SurfIn = NOPE;
  FullOnly = YUP;
  while (kar < argc) {
    /* loop accross command line options */
    if ((strcmp(argv[kar], "-h") == 0) || (strcmp(argv[kar], "-help") == 0)) {
      cmp_surf_usage ();
      exit (1);
    }
    if (!brk && (strcmp(argv[kar], "-sv1")) == 0) {
      kar ++;
      if (kar >= argc) {
	fprintf (SUMA_STDERR, "need argument after -sv1");
	exit (1);
      }
      Vol1Parent_FileName = argv[kar];
      brk = YUP;
    }
    if (!brk && (strcmp(argv[kar], "-sv2")) == 0) {
      kar ++;
      if (kar >= argc) {
	fprintf (SUMA_STDERR, "need argument after -sv2");
	exit (1);
      }
      Vol2Parent_FileName = argv[kar];
      brk = YUP;
    }
    if (!brk && (strcmp(argv[kar], "-prefix")) == 0) {
      kar ++;
      if (kar >= argc) {
	fprintf (SUMA_STDERR, "need argument after -prefix");
	exit (1);
      }
      fout = argv[kar];
      brk = YUP;
    }
    if (!brk && (strcmp(argv[kar], "-hemi")) == 0) {
      kar ++;
      if (kar >= argc) {
	fprintf (SUMA_STDERR, "need argument after -hemi");
	exit (1);
	 }
      hemi = argv[kar];
      brk = YUP;
    }
    if (!brk && (strcmp(argv[kar], "-spec")) == 0) {
      kar ++;
      if (kar >= argc) {
	fprintf (SUMA_STDERR, "need argument after -spec ");
	exit (1);
      }
      specfilename = argv[kar];
      brk = YUP;
    }
    
    if (!brk && (strcmp(argv[kar], "-box")) == 0) {
      kar ++;
      if (kar+2 >= argc) {
	fprintf (SUMA_STDERR, "need 3 arguments after -box");
	exit (1);
      }
      B_dim[0] = atof(argv[kar]); kar ++;
		B_dim[1] = atof(argv[kar]); kar ++;
		B_dim[2] = atof(argv[kar]);
		
		FullOnly = NOPE;
      brk = YUP;
    }

    if (!brk) {
      fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
      exit (1);
    } 
    else {	
      brk = NOPE;
      kar ++;
    }
  }/* loop across command line options */
  
	
  if (specfilename == NULL) {
    fprintf (SUMA_STDERR,"Error %s: No spec filename specified.\n", FuncName);
    exit(1);
  }
  if (!SUMA_Read_SpecFile (specfilename, &Spec)) {
    fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
    exit(1);
  }	



  /**** loading the first surface *****/
  if (SUMA_iswordin(Spec.SurfaceType[0], "FreeSurfer") == 1) {
    Surf1_FileName = Spec.FreeSurferSurface[0];
    Surf1 = SUMA_Load_Surface_Object(Surf1_FileName, SUMA_FREE_SURFER, SUMA_ASCII, Vol1Parent_FileName);
    tag1 =  "FS";
  }
  else 
    if (SUMA_iswordin(Spec.SurfaceType[0], "SureFit") == 1) {
      Surf1_SFName = SUMA_malloc(sizeof(SUMA_SFname));
      strcpy(Surf1_SFName->name_coord,Spec.SureFitCoord[0]);
      strcpy(Surf1_SFName->name_topo, Spec.SureFitTopo[0]);
      strcpy(Surf1_SFName->name_param, Spec.SureFitVolParam[0]);
      Surf1 = SUMA_Load_Surface_Object(Surf1_SFName, SUMA_SUREFIT, SUMA_ASCII,Vol1Parent_FileName);
      tag1 =  "SF";
    }
  state1 = Spec.State[0];
  
  /*** loading the second surface ****/
  fprintf(SUMA_STDERR,"loading the next surface \n");
  if (SUMA_iswordin(Spec.SurfaceType[1], "FreeSurfer") == 1) {
    Surf2_FileName = Spec.FreeSurferSurface[1];
    Surf2 = SUMA_Load_Surface_Object(Surf2_FileName, SUMA_FREE_SURFER, SUMA_ASCII, Vol2Parent_FileName);
    tag2 =  "FS";
  }
  else 
    if (SUMA_iswordin(Spec.SurfaceType[1], "SureFit") == 1) {
      Surf2_SFName = SUMA_malloc(sizeof(SUMA_SFname));
      strcpy(Surf2_SFName->name_coord, Spec.SureFitCoord[1]);
      strcpy(Surf2_SFName->name_topo, Spec.SureFitTopo[1]);
      strcpy(Surf2_SFName->name_param, Spec.SureFitVolParam[1]);
      Surf2 = SUMA_Load_Surface_Object(Surf2_SFName, SUMA_SUREFIT, SUMA_ASCII,Vol2Parent_FileName);
      tag2 =  "SF";
    }
  state2 = Spec.State[1];

  /* check on the output filename */
  if (fout == NULL) { /* form default name */
    sprintf(distancefilename, "%s_%s_%s_%s_%s.dist",hemi,tag1,state1,tag2,state2);
    sprintf(colorfilename, "%s_%s_%s_%s_%s.col",hemi,tag1,state1,tag2,state2);
  }else {
    sprintf (colorfilename, "%s.col", fout);
    sprintf (distancefilename, "%s.dist", fout);
  }
  
  if (SUMA_filexists(colorfilename) || SUMA_filexists(distancefilename)) {
    fprintf (SUMA_STDERR,"Error %s: One or both of output files %s, %s exists.\nWill not overwrite.\n", \
	     FuncName, distancefilename, colorfilename);
    exit(1);
  }
	 
  /***********************************************************************************************/

  SO1 = Surf1;
  SO2 = Surf2;
  //SUMA_Print_Surface_Object ( SO1, NULL);
  //SUMA_Print_Surface_Object ( SO2, NULL);

  num_nodes1 = SO1->N_Node;
  num_nodes2 = SO2->N_Node;
  
  fprintf(SUMA_STDERR, "Number of nodes in surface 1: %d \n", num_nodes1);
  fprintf(SUMA_STDERR, "Number of nodes in surface 2: %d \n", num_nodes2);
  SN1 = SUMA_SurfNorm(SO1->NodeList,  SO1->N_Node, SO1->FaceSetList, SO1->N_FaceSet);
  SN2 = SUMA_SurfNorm(SO2->NodeList,  SO2->N_Node, SO2->FaceSetList, SO2->N_FaceSet);
  
	if (!FullOnly) {
		/* get the Node member structure */
		fprintf(SUMA_STDOUT, "%s: Computing MemberFaceSets... \n", FuncName);
		Memb = SUMA_MemberFaceSets (SO2->N_Node, SO2->FaceSetList, SO2->N_FaceSet, 3);
   	if (Memb == NULL) {
			fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_MemberFaceSets. \n", FuncName);
			exit(1);
		}
	}

  /* Take each node in the SO1-> Nodelist and its corresponding SN1->NodeNormList.  This is the normalized normal vector to the node on the first surface.
     So each node is P0.  P1 is computed as some point along the normal vector to that node.  Lets say P0 is P1 + 20 mm along the normal. 
     Now feed P0 and P1 into the intersect triangle routine and feed the node and face list of surface 2 */
  
  distance = SUMA_malloc(num_nodes1*sizeof(float));

  /* for each node on the first surface do the following */
  SUMA_etime (&tt, 0);
  for (i = 0; i < num_nodes1; i++) {
    id = SO1->NodeDim * i;
	 P0[0] = SO1->NodeList[id];
    P0[1] = SO1->NodeList[id+1];
    P0[2] = SO1->NodeList[id+2];
    N0[0] = SN1.NodeNormList[id];
    N0[1] = SN1.NodeNormList[id+1];
    N0[2] = SN1.NodeNormList[id+2];
    Points = SUMA_Point_At_Distance(N0, P0, 1000);
    P1[0] = Points[0][0];
    P1[1] = Points[0][1];
    P1[2] = Points[0][2];
    
	if (!FullOnly) { /* trying to speed up intersection computations by restricting it to nodes within a box */
		TryFull = NOPE;
		/* search for nodes on surface 2 within xxmm of P0 */
		isin = SUMA_isinbox (SO2->NodeList, SO2->N_Node, P0, B_dim, 0);
		if (isin.nIsIn) {
			/* find the patch of surface 2 that is formed by those intersection nodes */
			Patch = SUMA_getPatch (isin.IsIn, isin.nIsIn, SO2->FaceSetList, SO2->N_FaceSet, Memb);
			if (Patch == NULL) {
				fprintf(SUMA_STDERR, "Error %s: Null returned from SUMA_getPatch.\n", FuncName);
				exit (1);
			}

			/* Perform the intersection based on that patch using Shruti's version */
			FaceSet_tmp = Patch->FaceSetList;
			N_FaceSet_tmp = Patch->N_FaceSet;
		} else {
			fprintf (SUMA_STDOUT, "%s: No nodes in box about node %d. Trying for full surface intersection.\n", FuncName, i);
			TryFull = YUP; /* flag to send it to full intersection */
		}
	} 
	
	if (FullOnly || TryFull) {
		Patch = NULL;
		FaceSet_tmp = SO2->FaceSetList;
		N_FaceSet_tmp = SO2->N_FaceSet;
	}

    /*now try with the segment from Points[0] to Points[1] returned above. */
    triangle = SUMA_MT_intersect_triangle(Points[0],Points[1], SO2->NodeList, SO2->N_Node, SO2->FaceSetList, SO2->N_FaceSet);
    //      SUMA_Show_MT_intersect_triangle(triangle, NULL);
    if (triangle->N_hits ==0) {
      distance[i] = -1;
      // fprintf(SUMA_STDERR, "Could not find hit for node %d in either direction.\n", i);
    }
    else {
      distance[i] = sqrtf(pow(triangle->P[0]-P0[0],2)+pow(triangle->P[1]-P0[1],2)+pow(triangle->P[2]-P0[2],2));
    }
	 
    SUMA_Free_MT_intersect_triangle(triangle); 
    if (Patch) SUMA_freePatch(Patch);
	 
    /* outputting as a color file */
    if (!(i%100)) {
      delta_t = SUMA_etime(&tt, 1);
      fprintf (SUMA_STDERR, " [%d]/[%d] %.2f/100%% completed. Dt = %.2f min done of %.2f min total\r" ,  i, num_nodes1, (float)i / num_nodes1 * 100, delta_t/60, delta_t/i * num_nodes1/60);
    }      
  }

  /* write out the distance file */
  if((distancefile = fopen(distancefilename, "w"))==NULL) {
    fprintf(SUMA_STDERR, "Could not open file distance.txt.\n");
    exit(1);
  }
  else {  
    for (i=0; i < num_nodes1; ++i) {
      fprintf (distancefile,"%d\t%f\n", i, distance[i]);
    }
    fclose (distancefile);
  }
  
  /* output this distance as a color file */
  MyColMap = SUMA_GetStandardMap(SUMA_CMAP_MATLAB_DEF_BGYR64);
  MyOpt = SUMA_ScaleToMapOptInit();
  MySV = SUMA_Create_ColorScaledVect(num_nodes1);
  mindistance = minimum(num_nodes1, distance);
  maxdistance = maximum(num_nodes1, distance);
  SUMA_ScaleToMap(distance,num_nodes1,mindistance, maxdistance, MyColMap,MyOpt,MySV);

  
  /* write out the distance color file */
  if((colorfile = fopen(colorfilename, "w"))==NULL) {
    fprintf(SUMA_STDERR, "Could not open file distance.col.\n");
    exit(1);
  }
  else {
    for (i=0; i < num_nodes1; ++i) {
      fprintf (colorfile,"%d\t%f\t%f\t%f\n", i, MySV->cM[i][0], MySV->cM[i][1], MySV->cM[i][2]);
    }
    fclose (colorfile);
  }
  
  if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
  
  return 1;
}

/*************************** FUNCTION DEFINITIONS **********************************/

void cmp_surf_usage ()
{
  printf ("\n\33[1mUsage: \33[0m SUMA_compare_surfaces \n\t-spec <Spec file>\n\t-hemi <L or R>\n\t-sv1 <volparentaligned1.BRIK>\n\t-sv2 <volparentaligned2.BRIK> \n\t[-box <wX wY wZ>] \n\t[-prefix <fileprefix>]\n\n");
  printf ("\n\t-spec <Spec file>: File containing surface specification. This file is typically \n");
  printf ("\t                   generated by @SUMA_Make_Spec_FS (for FreeSurfer surfaces) or \n");
  printf ("\t                   @SUMA_Make_Spec_SF (for SureFit surfaces). The Spec file should \n");
  printf ("\t                   be located in the directory containing the surfaces.\n");
  printf ("\n\t-hemi <left or right>: specify the hemisphere being processed \n");
  printf ("\n\t-sv1 <volume parent BRIK>:volume parent BRIK for first surface \n");
  printf ("\n\t-sv2 <volume parent BRIK>:volume parent BRIK for second surface \n");
  printf ("\n\t[-prefix <fileprefix>]: Prefix for distance and node color output files.\n");
  printf ("\t                 This option is optional. Existing file will not be overwritten.\n");
  printf ("\n\t-box <wX wY wZ>: restrict intersection computations for nodes \n");
  printf ("\t        contained in a box of w* dimensions. This might speed things\n");
  printf ("\t        up sometimes if the box dimension needs not be large.\n");
  printf ("\t        This option is pretty much useless.\n"); 
  /*
    printf ("\n\t[-dev]: This option will give access to options that are not well polished for consumption.\n");
    printf ("\n\t        \n");
  */
  printf ("\n\n\tFor more help: http://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm\n");
  printf ("\n\n\tIf you can't get help here, please get help somewhere.\n");
  SUMA_Version(NULL);
  
  printf ("\n\t Shruti Japee LBC/NIMH/NIH shruti@codon.nih.gov Ziad Saad SSSC/NIMH/NIH ziad@nih.gov \n\n");
  exit (0);
}



float minimum (int N, float *inarray)
{
  float min = inarray[0];
  int i;
  for (i = 1; i < N; i++)
    {
      if (inarray[i] < min)
	min = inarray[i];
    }
  return min;
}
/* determine the max in a vector of floats */
float maximum (int N, float *inarray)
{
  float max = inarray[0];
  int i;
  for (i = 1; i < N; i++)
    {
      if (inarray[i] > max)
	max = inarray[i];
    }
  return max;
}
