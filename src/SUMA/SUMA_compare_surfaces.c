/*Program to compute the minimum distance metric between two surfaces */

/* Header FILES */
   
#include "SUMA_suma.h"

/* function declarations */
float maximum(int N, float *inarray);
float minimum(int N, float *inarray);
void cmp_surf_usage ();

/*
File : SUMA_compare_surfaces_direction.c
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
  int trouble;
  int i,j,k;
  int num_nodes1;
  int num_nodes2;
  int onenode = -1, istart = -1, istop = -1, FailedDistance = -1; 
  float P0[3];
  float delta_t; 
  float P1[3];
  float P2[3];
  float N0[3];
  float maxdistance, mindistance;
  float *distance, distanceneg, distancepos;
  float Points[2][3]={{0.0, 0.0, 0.0},{0.0, 0.0, 0.0}};
  SUMA_COLOR_MAP *MyColMap;
  SUMA_SCALE_TO_MAP_OPT *MyOpt;
  SUMA_COLOR_SCALED_VECT * MySV;
  SUMA_MT_INTERSECT_TRIANGLE *triangle = NULL;
  SUMA_SurfaceObject *SO1, *SO2;
  struct timeval tt; 
  FILE *colorfile;
  FILE *distancefile;
  FILE *segfile1, *segfile2, *segfile3,*trianglesfile;
  char colorfilename[SUMA_MAX_FILENAME_LENGTH];
  char distancefilename[SUMA_MAX_FILENAME_LENGTH];
  char segmentfilename1[SUMA_MAX_FILENAME_LENGTH],segmentfilename2[SUMA_MAX_FILENAME_LENGTH],segmentfilename3[SUMA_MAX_FILENAME_LENGTH],trianglesfilename[SUMA_MAX_FILENAME_LENGTH];
  char *tag1 = NULL;
  char *tag2 = NULL;
  char *state1 = NULL;
  char *state2 = NULL;
  char *hemi = NULL;
  float B_dim[3];
  char *fout = NULL, *fname=NULL;
  SUMA_Boolean KeepMTI = YUP, Partial = NOPE, SkipConsistent = NOPE;

  SUMA_STANDALONE_INIT;
  SUMA_mainENTRY;
  
  if (argc < 7) {
    cmp_surf_usage();
    exit (1);
  }
  
  /* read in the surfaces */
  kar = 1;
  brk = NOPE;
  SurfIn = NOPE;
   Partial = NOPE;
   SkipConsistent = NOPE;
  while (kar < argc) {
    /* loop accross command line options */
    if ((strcmp(argv[kar], "-h") == 0) || (strcmp(argv[kar], "-help") == 0)) {
      cmp_surf_usage ();
      exit (1);
    }
   
   if (!brk && (strcmp(argv[kar], "-nocons") == 0)) {
      SkipConsistent = YUP;
      brk = YUP;
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
    
    if (!brk && (strcmp(argv[kar], "-onenode")) == 0) {
      if (Partial) {
         fprintf (SUMA_STDERR, "-onenode is incompatible with -noderange");
	      exit (1);
      }
      kar ++;
      if (kar >= argc) {
	fprintf (SUMA_STDERR, "need argument after -onenode");
	exit (1);
	   }
      istart = atoi(argv[kar]);
      istop = istart;
      Partial = YUP;
      brk = YUP;
    }
    
    if (!brk && (strcmp(argv[kar], "-noderange")) == 0) {
      if (Partial) {
         fprintf (SUMA_STDERR, "-noderange is incompatible with -onenode");
	      exit (1);
      }
      kar ++;
      if (kar+1 >= argc) {
	fprintf (SUMA_STDERR, "need 2 arguments after -noderange");
	exit (1);
	   }
      istart = atoi(argv[kar]); kar ++;
      istop = atoi(argv[kar]);
      Partial = YUP;
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
  

  /* allocate for the surface objects */
  Surf1 = (SUMA_SurfaceObject *) SUMA_malloc(sizeof(SUMA_SurfaceObject));	
  SO1 = (SUMA_SurfaceObject *) SUMA_malloc(sizeof(SUMA_SurfaceObject));	

	
  if (specfilename == NULL) {
    fprintf (SUMA_STDERR,"Error %s: No spec filename specified.\n", FuncName);
    exit(1);
  }
  if (!SUMA_AllocSpecFields(&Spec)) {
   SUMA_S_Err("Failed to allocate Spec Fields");
   exit(1);
  }
  if (!SUMA_Read_SpecFile (specfilename, &Spec)) {
    fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
    exit(1);
  }	

  /**** loading the first surface *****/
  if (SUMA_iswordin(Spec.SurfaceType[0], "FreeSurfer") == 1) {
    Surf1_FileName = Spec.SurfaceFile[0];
    Surf1 = SUMA_Load_Surface_Object(Surf1_FileName, SUMA_FREE_SURFER, SUMA_ASCII, Vol1Parent_FileName);
    tag1 =  "FS";
  }
  else 
    if (SUMA_iswordin(Spec.SurfaceType[0], "SureFit") == 1) {
      Surf1_SFName = SUMA_malloc(sizeof(SUMA_SFname));
      strcpy(Surf1_SFName->name_coord,Spec.CoordFile[0]);
      strcpy(Surf1_SFName->name_topo, Spec.TopoFile[0]);
      strcpy(Surf1_SFName->name_param, Spec.SureFitVolParam[0]);
      Surf1 = SUMA_Load_Surface_Object(Surf1_SFName, SUMA_SUREFIT, SUMA_ASCII,Vol1Parent_FileName);
      tag1 =  "SF";
    }
  state1 = Spec.State[0];
  
  /*** loading the second surface ****/
  fprintf(SUMA_STDERR,"loading the next surface \n");
  if (SUMA_iswordin(Spec.SurfaceType[1], "FreeSurfer") == 1) {
    Surf2_FileName = Spec.SurfaceFile[1];
    Surf2 = SUMA_Load_Surface_Object(Surf2_FileName, SUMA_FREE_SURFER, SUMA_ASCII, Vol2Parent_FileName);
    tag2 =  "FS";
  }
  else 
    if (SUMA_iswordin(Spec.SurfaceType[1], "SureFit") == 1) {
      Surf2_SFName = SUMA_malloc(sizeof(SUMA_SFname));
      strcpy(Surf2_SFName->name_coord, Spec.CoordFile[1]);
      strcpy(Surf2_SFName->name_topo, Spec.TopoFile[1]);
      strcpy(Surf2_SFName->name_param, Spec.SureFitVolParam[1]);
      Surf2 = SUMA_Load_Surface_Object(Surf2_SFName, SUMA_SUREFIT, SUMA_ASCII,Vol2Parent_FileName);
      tag2 =  "SF";
    }
  state2 = Spec.State[1];

  /* check on the output filename */
  if (fout == NULL) { /* form default name */
    fname = (char *)SUMA_malloc (
            ( strlen(hemi) + strlen(tag1) + strlen(state1) + strlen(tag2) + strlen(state2) + 20 ) *
            sizeof(char)); 
    sprintf(fname, "%s_%s_%s_%s_%s",hemi,tag1,state1,tag2,state2);

  } else {
     fname = SUMA_copy_string(fout); 
  }
  
    sprintf (colorfilename, "%s.col", fname);
    sprintf (distancefilename, "%s.dist", fname);
   
  /* section that will create the files that contain 6 values corresponding to two points to draw segments in SUMA */

  sprintf(segmentfilename1, "%s.allsegs.txt",fname);
  sprintf(segmentfilename2, "%s.longsegs.txt",fname);
  sprintf(segmentfilename3, "%s.badsegs.txt",fname); 
  sprintf(trianglesfilename, "%s.triangles.txt",fname); 

 if((segfile1 = fopen(segmentfilename1, "w"))==NULL) {
    fprintf(SUMA_STDERR, "Could not open segment file 1.\n");
    exit(1);
  }
 if((segfile2 = fopen(segmentfilename2, "w"))==NULL) {
    fprintf(SUMA_STDERR, "Could not open segment file 2.\n");
    exit(1);
  }
 if((segfile3 = fopen(segmentfilename3, "w"))==NULL) {
    fprintf(SUMA_STDERR, "Could not open segment file 3.\n");
    exit(1);
  }

 if((trianglesfile = fopen(trianglesfilename, "w"))==NULL) {
    fprintf(SUMA_STDERR, "Could not open triangles file.\n");
    exit(1);
  }


  if (SUMA_filexists(colorfilename) || SUMA_filexists(distancefilename)) {
    fprintf (SUMA_STDERR,"Error %s: One or both of output files %s, %s exists.\nWill not overwrite.\n", \
	     FuncName, distancefilename, colorfilename);
    exit(1);
  }
	 
  /***********************************************************************************************/

  SO1 = Surf1;
  SO2 = Surf2;
  

  if (SkipConsistent) {
   fprintf (SUMA_STDERR,"Skipping consistency check.\n");
  } else {
     if (!SO1->EL) SO1->EL = SUMA_Make_Edge_List (SO1->FaceSetList, SO1->N_FaceSet, SO1->N_Node,SO1->NodeList, SO1->idcode_str); 
     if (SUMA_MakeConsistent (SO1->FaceSetList, SO1->N_FaceSet, SO1->EL, 1, &trouble) == YUP)
       fprintf(SUMA_STDERR,"faces are consistent\n");
     else
       fprintf(SUMA_STDERR,"faces are not consistent\n");
  }
 
  if (!SO2->EL) SO2->EL = SUMA_Make_Edge_List (SO2->FaceSetList, SO2->N_FaceSet, SO2->N_Node, SO2->NodeList, SO2->idcode_str); 
  if (SUMA_MakeConsistent (SO2->FaceSetList, SO2->N_FaceSet, SO2->EL, 1, &trouble) == YUP)
    fprintf(SUMA_STDERR,"faces are consistent\n");
  else
    fprintf(SUMA_STDERR,"faces are not consistent\n");
   

   num_nodes1 = SO1->N_Node;
   num_nodes2 = SO2->N_Node;
   
   fprintf(SUMA_STDERR, "Number of nodes in surface 1: %d \n", num_nodes1);
   fprintf(SUMA_STDERR, "Number of nodes in surface 2: %d \n", num_nodes2);
   fprintf(SUMA_STDERR, "Number of faces in surface 1: %d \n", SO1->N_FaceSet);
   fprintf(SUMA_STDERR, "Number of faces in surface 2: %d \n", SO2->N_FaceSet);
   if (!SO1->NodeNormList) SUMA_RECOMPUTE_NORMALS(SO1);
   if (!SO2->NodeNormList) SUMA_RECOMPUTE_NORMALS(SO2);
   
   
   /* add some noise to surface 2 */
   /*  for (i = 0; i < SO2->N_Node; i ++)
     {
       id = SO2->NodeDim*i;
       SO2->NodeList[id] += 0.0002;
       SO2->NodeList[id+1] += 0.0003;
       SO2->NodeList[id+2] -= 0.0004;
     }
     
   */
    
  /* Take each node in the SO1-> Nodelist and its corresponding SO1->NodeNormList.  This is the normalized normal vector to the node on the first surface.
     So each node is P0.  P1 is computed as some point along the normal vector to that node.  Lets say P0 is P1 + 20 mm along the normal. 
     Now feed P0 and P1 into the intersect triangle routine and feed the node and face list of surface 2 */
  
  distance = SUMA_malloc(num_nodes1*sizeof(float));
  /* *****YOU SHOULD ALLOCATE FOR triangle that is done in the function****** triangle = SUMA_malloc(sizeof(SUMA_MT_INTERSECT_TRIANGLE)); */
  /* for each node on the first surface do the following */
  SUMA_etime (&tt, 0);
  
   if (!Partial){
      istart = 0;
      istop = SO1->N_Node-1;
   } else {
      if (istart > istop) {
         fprintf (SUMA_STDERR,"Error %s: starting node %d > stopping node %d\n", 
           FuncName, istart, istop);
         exit(1);
      }
      if (istart < 0 || istop > SO1->N_Node-1) {
         fprintf (SUMA_STDERR,"Error %s: starting node %d is either < 0 or stopping node > %d (N_Node -1)\n", 
           FuncName, onenode, SO1->N_Node-1);
         exit(1);
      }
   }
  
  FailedDistance = 0; 
  for (i = istart; i <= istop; i++) {
    id = SO1->NodeDim * i;
    P0[0] = SO1->NodeList[id];
    P0[1] = SO1->NodeList[id+1];
    P0[2] = SO1->NodeList[id+2];

    N0[0] = SO1->NodeNormList[id];
    N0[1] = SO1->NodeNormList[id+1];
    N0[2] = SO1->NodeNormList[id+2];

   SUMA_POINT_AT_DISTANCE(N0, P0, 100, Points);
   P1[0] = Points[0][0];
   P1[1] = Points[0][1];
   P1[2] = Points[0][2];
   P2[0] = Points[1][0];
   P2[1] = Points[1][1];
   P2[2] = Points[1][2];

   /* now determine the distance along normal */
   triangle = SUMA_MT_intersect_triangle(P0,P1, SO2->NodeList, SO2->N_Node, SO2->FaceSetList, SO2->N_FaceSet, triangle, 0);
   /* fprintf(SUMA_STDERR,"number of hits for node %d : %d\n", i,triangle->N_hits); */ 
   if (triangle->N_hits ==0) {
   fprintf(SUMA_STDERR, "Could not find hit for node %d in either direction.\n", i);
   fprintf(segfile3,"%f %f %f %f %f %f\n",P0[0],P0[1],P0[2],P1[0],P1[1],P1[2]);
   distance[i] = 0.0;
   }
   else {
   fprintf(trianglesfile,"distance for surf 1 node %d:\n",i);
   for (k = 0; k < triangle->N_el; k++) {
   if (triangle->isHit[k] == YUP)
   fprintf(trianglesfile, "hit %d: %f (%f, %f)\n",k,triangle->t[k], triangle->u[k], triangle->v[k]);
   }
   /* distance[i] = sqrtf(pow(triangle->P[0]-P0[0],2)+pow(triangle->P[1]-P0[1],2)+pow(triangle->P[2]-P0[2],2)); */
   distance[i] = triangle->t[triangle->ifacemin];
   fprintf(segfile2,"%f %f %f %f %f %f\n",P0[0],P0[1],P0[2],P1[0],P1[1],P1[2]);
   }

   if (!KeepMTI) triangle = SUMA_Free_MT_intersect_triangle(triangle); 
    

   fprintf(segfile1,"%f %f %f %f %f %f\n",P0[0],P0[1],P0[2],P1[0],P1[1],P1[2]);

    if (!(i%100)) {
      delta_t = SUMA_etime(&tt, 1);
      fprintf (SUMA_STDERR, 
               " [%d]/[%d] %.2f/100%% completed. "
               "Dt = %.2f min done of %.2f min total\r" ,  
               i, num_nodes1, (float)i / num_nodes1 * 100, delta_t/60, 
               delta_t/i * num_nodes1/60);
    }
   
   if (Partial) {
      /* output distance to screen */
      fprintf(SUMA_STDERR, "\nDistance at node %d is %f\n", i, distance[i]);
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
     MyColMap = SUMA_FindNamedColMap("byr64");
     MyOpt = SUMA_ScaleToMapOptInit();
     MySV = SUMA_Create_ColorScaledVect(num_nodes1, 0);
     mindistance = minimum(num_nodes1, distance);
     maxdistance = maximum(num_nodes1, distance);
     SUMA_ScaleToMap(distance,num_nodes1,mindistance, maxdistance, 
                     MyColMap,MyOpt,MySV);


     /* write out the distance color file */
     if((colorfile = fopen(colorfilename, "w"))==NULL) {
       fprintf(SUMA_STDERR, "Could not open file distance.col.\n");
       exit(1);
     }
     else {
       for (i=0; i < num_nodes1; ++i) {
         fprintf (colorfile,"%d\t%f\t%f\t%f\n", 
                  i, MySV->cV[3*i  ], MySV->cV[3*i+1], MySV->cV[3*i+2]);
       }
       fclose (colorfile);
     }

  if (fname) SUMA_free(fname);
  if (!SUMA_FreeSpecFields(&Spec)) {
   SUMA_S_Err("Failed to free spec fields");
  }
  if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);  
  SUMA_RETURN (1);
}

/*************************** FUNCTION DEFINITIONS **********************************/

void cmp_surf_usage ()
{
  static char FuncName[]={"cmp_surf_usage"};
  char * s = NULL;
  s = SUMA_help_basics();
  printf ("\n"
          "   Usage:    CompareSurfaces \n"
          "             -spec <Spec file>\n"
          "             -hemi <L or R>\n"
          "             -sv1 <volparentaligned1.BRIK>\n"
          "             -sv2 <volparentaligned2.BRIK> \n"
          "             [-prefix <fileprefix>]\n"
          "\n"
          "   NOTE: This program is now superseded by SurfToSurf\n"
          "\n"
          "   This program calculates the distance, at each node in Surface 1 (S1) to Surface 2 (S2)\n"
          "   The distances are computed along the local surface normal at each node in S1.\n"
          "   S1 and S2 are the first and second surfaces encountered in the spec file, respectively.\n"
          "\n"
          "   -spec <Spec file>: File containing surface specification. This file is typically \n"
          "                      generated by @SUMA_Make_Spec_FS (for FreeSurfer surfaces) or \n"
          "                      @SUMA_Make_Spec_SF (for SureFit surfaces).\n"
          "   -hemi <left or right>: specify the hemisphere being processed \n"
          "   -sv1 <volume parent BRIK>:volume parent BRIK for first surface \n"
          "   -sv2 <volume parent BRIK>:volume parent BRIK for second surface \n"
          "\n"
          "Optional parameters:\n"
          "   [-prefix <fileprefix>]: Prefix for distance and node color output files.\n"
          "                           Existing file will not be overwritten.\n"
          "   [-onenode <index>]: output results for node index only. \n"
          "                       This option is for debugging.\n"
          "   [-noderange <istart> <istop>]: output results from node istart to node istop only. \n"
          "                                  This option is for debugging.\n"
          "   NOTE: -noderange and -onenode are mutually exclusive\n"
          "   [-nocons]: Skip mesh orientation consistency check.\n"
          "              This speeds up the start time so it is useful\n"
          "              for debugging runs.\n"
          "\n"
          "%s"
          "\n   For more help: https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/SUMA/main_toc.html\n"
          "\n"
          "\n   If you can't get help here, please get help somewhere.\n", s);
  SUMA_free(s); s = NULL;
  s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
  printf ("\n    Shruti Japee LBC/NIMH/NIH shruti@codon.nih.gov Ziad S. Saad SSSC/NIMH/NIH saadz@mail.nih.gov \n\n");
  /*
    printf ("\n   [-dev]: This option will give access to options that are not well polished for consumption.\n");
    printf ("\n           \n");
  */

  // [PT: June 4, 2018] former link above: https://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm
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
