#include "SUMA_suma.h"

#ifdef SUMA_CreateIcosahedron_STAND_ALONE
   /* these global variables must be declared even if they will not be used by this main */
   SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
   SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
   int SUMAg_N_SVv = 0; /*!< Number of SVs stored in SVv */
   SUMA_DO *SUMAg_DOv;	/*!< Global pointer to Displayable Object structure vector*/
   int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
   SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */
#else
   extern SUMA_CommonFields *SUMAg_CF; 
#endif
 

/**divides 1 triangle into 4 recursively to recDepth*/
void SUMA_tesselate(float *nodeList, int *triList, int *nCtr, int *tCtr, int recDepth, int depth, int n1, int n2, int n3)
{
  float x1,y1,z1, x2,y2,z2, x3,y3,z3;
  float x12, y12, z12;
  float x23, y23, z23;
  float x31, y31, z31;
  int currIndex, index1, index2, index3;
  int i, j;
  static char FuncName[]={"SUMA_tesselate"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

  //fprintf(SUMA_STDERR,"nCtr value : %d\n",nCtr[0]);
  //fprintf(SUMA_STDERR,"depth value : %d\n",depth);

  currIndex = (nCtr[0]-2)/3;

  x1=nodeList[3*n1]; y1=nodeList[3*n1+1]; z1=nodeList[3*n1+2];
  x2=nodeList[3*n2]; y2=nodeList[3*n2+1]; z2=nodeList[3*n2+2];
  x3=nodeList[3*n3]; y3=nodeList[3*n3+1]; z3=nodeList[3*n3+2];
  
  x12=(x1+x2)/2; y12=(y1+y2)/2; z12=(z1+z2)/2;
  x23=(x2+x3)/2; y23=(y2+y3)/2; z23=(z2+z3)/2;
  x31=(x3+x1)/2; y31=(y3+y1)/2; z31=(z3+z1)/2;

  //  fprintf(SUMA_STDERR, "\ntri (%d %d %d) : currIndex=%d : ", n1, n2 ,n3, currIndex);

 /**prevents creation of duplicate nodes*/
  index1 = -1; index2 = -1; index3 = -1;
  i=0; j=0;
  for (i=0; i<=currIndex; ++i) {
    j = 3*i;
    if (nodeList[j]==x12 && nodeList[j+1]==y12 && nodeList[j+2]==z12) {
      index1 = i;
      //      fprintf(SUMA_STDERR, " n12 exists ");
    }
    if (nodeList[j]==x23 && nodeList[j+1]==y23 && nodeList[j+2]==z23) {
      index2 = i;
      // fprintf(SUMA_STDERR, " n23 exists ");
    }
    if (nodeList[j]==x31 && nodeList[j+1]==y31 && nodeList[j+2]==z31) {
      index3 = i;
      //fprintf(SUMA_STDERR, " n31 exists ");
    }
  }
  if (index1==-1) {
    ++currIndex;
    index1 = currIndex;
    //fprintf(SUMA_STDERR, " n12 new ");
    SUMA_addNode( nodeList, nCtr, x12, y12, z12);
  }
  if (index2==-1) {
    ++currIndex;
    index2 = currIndex;
    //fprintf(SUMA_STDERR, " n23 new ");
    SUMA_addNode( nodeList, nCtr, x23, y23, z23);
  }
  if (index3==-1) {
    ++currIndex;
    index3 = currIndex;
    //fprintf(SUMA_STDERR, " n31 new ");
    SUMA_addNode( nodeList, nCtr, x31, y31, z31);
  }

  /**if recursion depth met, add 4 triangles to list referenced by tPtr*/
  if (depth>=recDepth) {
    SUMA_addTri( triList, tCtr, n1, index1, index3);
    SUMA_addTri( triList, tCtr, index1, n2, index2);
    SUMA_addTri( triList, tCtr, index3, index2, n3);
    SUMA_addTri( triList, tCtr, index1, index2, index3);
  }
  
  /**call tesselate on each of 4 new triangles*/
  else {
    ++depth;
    SUMA_tesselate( nodeList, triList, nCtr, tCtr, recDepth, depth, n1, index1, index3 );
    SUMA_tesselate( nodeList, triList, nCtr, tCtr, recDepth, depth, index1, n2, index2 );
    SUMA_tesselate( nodeList, triList, nCtr, tCtr, recDepth, depth, index3, index2, n3 );
    SUMA_tesselate( nodeList, triList, nCtr, tCtr, recDepth, depth, index1, index2, index3 );
  }
  
   SUMA_RETURNe;
}

/**adds x,y,z coors of node to nodeList*/
void SUMA_addNode(float *nodeList, int *ctr, float x, float y, float z) {
  
  ++*ctr;
  nodeList[*ctr] = x;  
  ++*ctr;
  nodeList[*ctr] = y;  
  ++*ctr;
  nodeList[*ctr] = z;
  
  return;
}


/**adds x,y,z coors of node to nodeList */
void SUMA_addTri(int *triList, int *ctr, int n1, int n2, int n3) {

  ++*ctr;
  triList[*ctr] = n1;
  ++*ctr;
  triList[*ctr] = n2;
  ++*ctr;
  triList[*ctr] = n3;
  
  return;
}

/*!
 SO = SUMA_CreateIcosahedron (r, recDepth);
 
 This function creates an icosahedron of size r and a tesselation recursion depth recDepth.
 \param r (float) size of icosahedron.
 \param recDepth (int) number of subdivisions of intial tesselation
 \ret SO (SUMA_SurfaceObject *) icosahedron is a surface object structure.
   returns NULL if function fails.
   WHAT GETS FILLED IN SO ....
     
  Written by Brenna Argall  
*/
SUMA_SurfaceObject * SUMA_CreateIcosahedron (float r, int recDepth) 
{
   static char FuncName[]={"SUMA_CreateIcosahedron"};
   SUMA_SurfaceObject *SO = NULL;
   int i, numNodes, numTri;
   float a,b;
   int nodePtCt, triPtCt, *icosaTri;
   float *icosaNode;
   SUMA_SURF_NORM SN;

   SUMA_Boolean LocalHead = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SO = SUMA_Alloc_SurfObject_Struct(1);
   if (SO == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Surface Object.", FuncName);
      SUMA_RETURN (NULL);
   }  
   
   if (recDepth != 0) {
    numNodes = 3;
    for(i=1; i<=recDepth; ++i) {
      numNodes = numNodes + 3*pow(2,2*(i-1));
    }
    numNodes = 20*numNodes;  /* This is a conservative esitmate */
   }
   else numNodes = 12;
   
   numTri = 20*pow(2,2*recDepth);

   if (LocalHead) fprintf(SUMA_STDERR,"%s: Allocated for %d Nodes, %d numTri\n", FuncName, numNodes, numTri);
   
   /**icosahedron creation and tesselation*/
   
   a = r*(1+sqrt(5)) / (sqrt(10+2*sqrt(5)));
   b = 2*r / (sqrt(10+2*sqrt(5)));
   if (LocalHead) fprintf(SUMA_STDERR,"%s: just before node creation\n", FuncName);

   /**create icosahedron node list*/
   nodePtCt = -1;
   icosaNode = SUMA_calloc(3*numNodes, sizeof(float));
   icosaTri = calloc(3*numTri, sizeof(int));

   if (!icosaNode || !icosaTri) {
   fprintf (SUMA_STDERR,"Error %s: Could not allocate for icosaNode and/or icosaTri.\n",FuncName);
   SUMA_Free_Surface_Object (SO);
   SUMA_RETURN (NULL); 
   }

   SUMA_addNode( icosaNode, &nodePtCt, 0, b, -a );  SUMA_addNode( icosaNode, &nodePtCt, 0, b, a );
   SUMA_addNode( icosaNode, &nodePtCt, 0, -b, a );  SUMA_addNode( icosaNode, &nodePtCt, 0, -b, -a );
   SUMA_addNode( icosaNode, &nodePtCt, -b, a, 0 );  SUMA_addNode( icosaNode, &nodePtCt, -b, -a, 0 );
   SUMA_addNode( icosaNode, &nodePtCt, b, a, 0 );   SUMA_addNode( icosaNode, &nodePtCt, b, -a, 0 );
   SUMA_addNode( icosaNode, &nodePtCt, a, 0, b );   SUMA_addNode( icosaNode, &nodePtCt, -a, 0, -b );
   SUMA_addNode( icosaNode, &nodePtCt, -a, 0, b );  SUMA_addNode( icosaNode, &nodePtCt, a, 0, -b );

   if (LocalHead) fprintf(SUMA_STDERR,"%s: just after node creation\n", FuncName);

   /**tesselate icosahedron*/
   triPtCt = -1;

   /**if recursion depth is 0, just make icosahedron (no tesselation)*/
   if (LocalHead) fprintf(SUMA_STDERR,"%s: recDepth = %d\n", FuncName, recDepth);

   if (recDepth==0) {
    if (LocalHead) fprintf(SUMA_STDERR,"%s: recDepth=0.\n", FuncName);

    SUMA_addTri( icosaTri, &triPtCt, 0, 6, 4 );   SUMA_addTri( icosaTri, &triPtCt, 1, 4, 6 );
    SUMA_addTri( icosaTri, &triPtCt, 0, 4, 9 );   SUMA_addTri( icosaTri, &triPtCt, 1, 6, 8 );
    SUMA_addTri( icosaTri, &triPtCt, 0, 9, 3 );   SUMA_addTri( icosaTri, &triPtCt, 1, 8, 2 );
    SUMA_addTri( icosaTri, &triPtCt, 0, 3, 11 );  SUMA_addTri( icosaTri, &triPtCt, 1, 2, 10 );
    SUMA_addTri( icosaTri, &triPtCt, 0, 11, 6 );  SUMA_addTri( icosaTri, &triPtCt, 1, 10, 4 );
    SUMA_addTri( icosaTri, &triPtCt, 2, 8, 7 );   SUMA_addTri( icosaTri, &triPtCt, 3, 7, 11 );
    SUMA_addTri( icosaTri, &triPtCt, 2, 7, 5 );   SUMA_addTri( icosaTri, &triPtCt, 3, 5, 7 );
    SUMA_addTri( icosaTri, &triPtCt, 2, 5, 10 );  SUMA_addTri( icosaTri, &triPtCt, 3, 9, 5 );
    SUMA_addTri( icosaTri, &triPtCt, 4, 10, 9 );  SUMA_addTri( icosaTri, &triPtCt, 6, 11, 8 );
    SUMA_addTri( icosaTri, &triPtCt, 5, 9, 10 );  SUMA_addTri( icosaTri, &triPtCt, 7, 8, 11 );
   }

   else {
    if (LocalHead) fprintf(SUMA_STDERR,"%s: just before node tesselation.\n", FuncName);

    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 0, 6, 4);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 0, 4, 9);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 0, 9, 3);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 0, 3, 11);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 0, 11, 6);

    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 1, 4, 6);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 1, 6, 8);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 1, 8, 2);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 1, 2, 10);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 1, 10, 4);

    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 2, 8, 7);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 2, 7, 5);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 2, 5, 10);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 4, 10, 9);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 5, 9, 10);

    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 3, 7, 11);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 3, 5, 7);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 3, 9, 5);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 6, 11, 8);
    SUMA_tesselate(icosaNode, icosaTri, &nodePtCt, &triPtCt, recDepth, 1, 7, 8, 11);
   }

   numNodes = (nodePtCt)/3 + 1;

   if (LocalHead) fprintf(SUMA_STDERR,"%s: %d nodes, %d triangles in icosahedron.\n", FuncName, numNodes, numTri);

   /* store in SO and get out */
   SO->NodeList = icosaNode;
   SO->FaceSetList = icosaTri;
   SO->N_Node = numNodes;
   SO->N_FaceSet = numTri;
   SO->NodeDim = 3;
   SO->FaceSetDim = 3;
   
   SN = SUMA_SurfNorm( SO->NodeList, SO->N_Node, SO->FaceSetList, SO->N_FaceSet);
   SO->NodeNormList = SN.NodeNormList;
   SO->FaceNormList = SN.FaceNormList;
   
   SUMA_RETURN (SO);
}

/*!
put documentation here ...


SUMA_SO_map * SUMA_MapBrenna (SUMA_Surface_Object *icosa, SUMA_Surface_Object *sph)
{
   numIcoNodes = icosa->N_Node;
   icosaNode = icosa->NodeList;
   
} 

*/

/*!
function used to create a SUMA_SO_map structure
*/
SUMA_SO_map *SUMA_Create_SO_map (void) 
{
   static char FuncName[]={"SUMA_Create_SO_map"};
   SUMA_SO_map *SOM = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SOM = SUMA_malloc (sizeof(SUMA_SO_map));
   if (!SOM) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for SOM.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   SOM->N_Node = 0;
   SOM->NewNodeList = NULL;
   SOM->NodeVal = NULL;
   SOM->NodeDisp = NULL;
   SOM->NodeCol = NULL;
   
   SUMA_RETURN (SOM);
}

/*!
   function to free SO_map
*/
SUMA_Boolean SUMA_Free_SO_map (SUMA_SO_map *SOM) 
{
   static char FuncName[]={"SUMA_Free_SO_map"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SOM) {
      SUMA_RETURN (YUP);
   }
   
   if (SOM->NewNodeList) free (SOM->NewNodeList);
   if (SOM->NodeVal) free (SOM->NodeVal);
   if (SOM->NodeDisp) free (SOM->NodeDisp);
   if (SOM->NodeCol) free(SOM->NodeCol);
   
   free (SOM);
   
   SUMA_RETURN (YUP);
}

/*!
   function to Show SO_map
*/
SUMA_Boolean SUMA_Show_SO_map (SUMA_SO_map *SOM, FILE *out) 
{
   static char FuncName[]={"SUMA_Show_SO_map"};
   int i=0, imax;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!out) out = SUMA_STDERR;
   
   fprintf (out, "\n%s: Showing contents of SUMA_SO_map structure:\n", FuncName); 
   if (!SOM) {
      fprintf (out, "\tpointer is NULL.\n");
      SUMA_RETURN (YUP);
   }
   
   if (SOM->N_Node > 5) imax = 5; 
   else imax = SOM->N_Node;
   
   fprintf (SUMA_STDERR, "NodeList, (1st %d elements):\n", imax);
   for (i=0; i<imax; ++i) {
      fprintf (SUMA_STDERR, "\t%f, %f, %f\n", SOM->NewNodeList[3*i], SOM->NewNodeList[3*i+1], SOM->NewNodeList[3*i+2]);
   }
   
   SUMA_RETURN (YUP);
}
#ifdef SUMA_CreateIcosahedron_STAND_ALONE

void SUMA_CreateIcosahedron_usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_CreateIcosahedron [-r r] [-rd recDepth]\n");
          printf ("\n\tr: size of icosahedron. (optional, default 100)\n");
          printf ("\n\trecDepth: ccccccccccccccc. (optional, default 3)\n");
          
          printf ("\t\t\t Brenna D. Argall LBC/NIMH/NIH xxxxxxxxxxxxx.gov \t Wed Mar 20 14:23:42 EST 2002\n");
          exit (0);
  }/*Usage*/
/*!
   stand alone program to create an icosahedron and write it to file in Freesurfer format. 

*/
int main (int argc, char *argv[])
{/* main SUMA_CreateIcosahedron */
   static char FuncName[]={"SUMA_CreateIcosahedron-main"};
   int kar, recDepth, i, j;
   float r;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean brk, LocalHead = YUP;
   char sout[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
   FILE *icosaFile;
   
	/* allocate space for CommonFields structure */
	if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   if (LocalHead) fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);
   
   
	/* read in the options */
	r = 100;
   recDepth = 3;
   sprintf (sout, "icosaTess.asc");
   kar = 1;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			SUMA_CreateIcosahedron_usage ();
         exit (1);
		}
				
		if (!brk && (strcmp(argv[kar], "-r") == 0 ))
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -r ");
				exit (1);
			}
			r = atof(argv[kar]);
			brk = YUP;
		}		
		
		if (!brk && strcmp(argv[kar], "-rd") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -rd ");
				exit (1);
			}
			recDepth = atoi(argv[kar]);

			brk = YUP;
		}	

		if (!brk && strcmp(argv[kar], "-so") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -so ");
				exit (1);
			}
			sprintf (sout, "%s", argv[kar]);

			brk = YUP;
		}	

		if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
		
	}/* loop accross command ine options */

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Recursion depth %d, Size %f.\n", FuncName, recDepth, r);

   SO = SUMA_CreateIcosahedron (r, recDepth);
   if (!SO) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateIcosahedron.\n", FuncName);
      exit (1);
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, sout);
   
   /**write tesselated icosahedron to file*/
  icosaFile = fopen(sout, "w");
   if (!icosaFile) {
      fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n", FuncName, sout);
      exit(1);
   }
   
  fprintf (icosaFile,"#tesselated icosahedron for SUMA_recursion.c\n");
  fprintf (icosaFile, "%d %d\n", SO->N_Node, SO->N_FaceSet);
  j=0;
  for (i=0; i<SO->N_Node; ++i) {
    j=3*i;
    fprintf (icosaFile, "%f  %f  %f  0\n", SO->NodeList[j], SO->NodeList[j+1], SO->NodeList[j+2]);
  }
  
  for (i=0; i<SO->N_FaceSet; ++i) {
    j = 3*i;
    fprintf (icosaFile, "%d %d %d 0\n", SO->FaceSetList[j], SO->FaceSetList[j+1], SO->FaceSetList[j+2]);
  }
  fclose(icosaFile);

  if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
  
  exit(0);
  
}/* main SUMA_CreateIcosahedron*/
#endif
