#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_SurfToSurf (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfToSurf"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
"\n"
"Usage: SurfToSurf <-i_TYPE S1> [<-sv SV1>]\n"
"                  <-i_TYPE S2> [<-sv SV1>]\n"
"                  [<-prefix PREFIX>]\n"
"                  [<-output_params PARAM_LIST>]\n"
"                  [<-node_indices NODE_INDICES>]\n"
"                  [<-proj_dir PROJ_DIR>]\n"
"                  [<-data DATA>]\n"
"                  [<-node_debug NODE>]\n"
"                  [<-debug DBG_LEVEL>]\n"
"                  [-make_consistent]\n"
" \n"
"  This program is used to interpolate data from one surface (S2)\n"
" to another (S1), assuming the surfaces are quite similar in\n"
" shape but having different meshes (non-isotopic).\n"
" This is done by projecting each node (nj) of S1 along the normal\n"
" at nj and finding the closest triangle t of S2 that is intersected\n"
" by this projection. Projection is actually bidirectional.\n"
" If such a triangle t is found, the nodes (of S2) forming it are \n"
" considered to be the neighbors of nj.\n" 
" Values (arbitrary data, or coordinates) at these neighboring nodes\n"
" are then transfered to nj using barycentric interpolation or \n"
" nearest-node interpolation.\n"
" Nodes whose projections fail to intersect triangles in S2 are given\n"
" nonsensical values of -1 and 0.0 in the output.\n" 
"\n"
" Mandatory input:\n"
"  Two surfaces are required at input. See -i_TYPE options\n"
"  below for more information. \n"
"\n"
" Optional input:\n"
"  -prefix PREFIX: Specify the prefix of the output file.\n"
"                  The output file is in 1D format at the moment.\n"
"                  Default is SurfToSurf\n"
"  -output_params PARAM_LIST: Specify the list of mapping\n"
"                             parameters to include in output\n"
"     PARAM_LIST can have any or all of the following:\n"
"        NearestTriangleNodes: Use Barycentric interpolation (default)\n"
"                              and output indices of 3 nodes from S2\n"
"                              that neighbor nj of S1\n"
"        NearestNode: Use only the closest node from S2 (of the three \n"
"                     closest neighbors) to nj of S1 for interpolation\n"
"                     and output the index of that closest node.\n"
"        NearestTriangle: Output index of triangle t from S2 that\n"
"                         is the closest to nj along its projection\n"
"                         direction. \n"  
"        DistanceToSurf: Output distance (signed) from nj, along \n"
"                        projection direction to S2.\n"
"                        This is the parameter output by the precursor\n"
"                        program CompareSurfaces\n"
"        ProjectionOnSurf: Output coordinates of projection of nj onto \n"
"                          triangle t of S2.\n"
"        Data: Output the data from S2, interpolated onto S1\n"
"              If no data is specified via the -data option, then\n"
"              the XYZ coordinates of SO2's nodes are considered\n"
"              the data.\n"
"  -data DATA: 1D file containing data to be interpolated.\n"
"              Each row i contains data for node i of S2.\n"
"              You must have one row for each node making up S2.\n"
"              In other terms, if S2 has N nodes, you need N rows\n"
"              in DATA. \n"
"              Each column of DATA is processed separately (think\n"
"              sub-bricks, and spatial interpolation).\n"
"              You can use [] selectors to choose a subset \n"
"              of columns.\n"
"              If -data option is not specified and Data is in PARAM_LIST\n"
"              then the XYZ coordinates of SO2's nodes are the data.\n"
"  -node_indices NODE_INDICES: 1D file containing the indices of S1\n"
"                              to consider. The default is all of the\n"
"                              nodes in S1. Only one column of values is\n"
"                              allowed here, use [] selectors to choose\n"
"                              the column of node indices if NODE_INDICES\n"
"                              has multiple columns in it.\n"
"  -proj_dir PROJ_DIR: 1D file containing projection directions to use\n"
"                      instead of the node normals of S1.\n"
"                      Each row should contain one direction for each\n"
"                      of the nodes forming S1.\n"
"  -make_consistent: Force a consistency check and correct triangle \n"
"                    orientation of S1 if needed. Triangles are also\n"
"                    oriented such that the majority of normals point\n"
"                    away from center of surface.\n"
"                    The program might not succeed in repairing some\n"
"                    meshes with inconsistent orientation.\n" 
"\n"
"%s"
"%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n"
             "       Shruti Japee LBC/NIMH/NIH  shruti@codon.nih.gov \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_SurfToSurf_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_BrainWrap_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk, accepting_out;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   kar = 1;
   brk = NOPE;
   Opt->in_1D = NULL;
   Opt->NodeDbg = -1;
   Opt->debug = 0;
   Opt->NearestNode = 0;
   Opt->NearestTriangle = 0;
   Opt->DistanceToMesh = 0;
   Opt->ProjectionOnMesh = 0;
   Opt->Data = 0;
   Opt->in_name = NULL;
   Opt->out_prefix = NULL;
   Opt->fix_winding = 0;
   accepting_out = NOPE;
   while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      
      if (!brk && accepting_out) { /* make sure you have not begun with new options */
         if (*(argv[kar]) == '-') accepting_out = NOPE;
      }
		
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SurfToSurf(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -debug \n");
            exit (1);
         }
         
         Opt->debug = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-node_debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -node_debug \n");
            exit (1);
         }
         
         Opt->NodeDbg = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-node_indices") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a parameter after -node_indices \n");
            exit (1);
         }
         
         Opt->in_nodeindices = argv[++kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-output_params") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need at least one parameter after output_params \n");
            exit (1);
         }
         
         accepting_out = YUP;
         brk = YUP;
      }
      
      if (!brk && accepting_out && (strcmp(argv[kar], "NearestNode") == 0)) {
         if (Opt->NearestNode < 1) Opt->NearestNode = 1;
         brk = YUP;
      }
      
      if (!brk && accepting_out && (strcmp(argv[kar], "NearestTriangleNodes") == 0)) {
         if (Opt->NearestNode < 3) Opt->NearestNode = 3;
         brk = YUP;
      }
      
      if (!brk && accepting_out && (strcmp(argv[kar], "NearestTriangle") == 0)) {
         if (Opt->NearestTriangle < 1) Opt->NearestTriangle = 1;
         brk = YUP;
      }
      
      if (!brk && accepting_out && (strcmp(argv[kar], "DistanceToSurf") == 0)) {
         Opt->DistanceToMesh = 1;
         brk = YUP;
      }
      
      if (!brk && accepting_out && (strcmp(argv[kar], "ProjectionOnSurf") == 0)) {
         Opt->ProjectionOnMesh = 1;
         brk = YUP;
      }
      
      if (!brk && accepting_out && (strcmp(argv[kar], "Data") == 0)) {
         Opt->Data = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-data") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a name after -data \n");
            exit (1);
         }
         ++kar;
         if (strcmp(argv[kar],"_XYZ_") == 0) { 
            /* default Opt->in_name = NULL*/
            if (Opt->in_name) {
               SUMA_SL_Err("Unexpected non null value");
               exit (1);
            }
         } else {
            Opt->in_name = SUMA_copy_string(argv[kar]);
         }
         Opt->Data = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a name after -prefix \n");
            exit (1);
         }
         
         Opt->out_prefix = SUMA_Extension(argv[++kar],".1D", YUP);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-proj_dir") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a name after -proj_dir \n");
            exit (1);
         }
         
         Opt->in_1D = argv[++kar];
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-make_consistent") == 0))
      {
         Opt->fix_winding = 1;
         brk = YUP;
      }
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   /* set default for NearestNode if nothing has been set */
   if (Opt->NearestNode < 1) Opt->NearestNode = 3;
   
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("SurfToSurf"); 
   
   SUMA_RETURN(Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfToSurf"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfaceObject *SO1=NULL, *SO2 = NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   SUMA_M2M_STRUCT *M2M = NULL;
   int N_Spec=0, *nodeind = NULL, N_nodeind, icol, i, j;
   MRI_IMAGE *im = NULL, *im_data=NULL;
	int nvec, ncol=0, nvec_data, ncol_data, Nchar;
   float *far = NULL, *far_data=NULL, *dt = NULL, *projdir=NULL;
   char *outname = NULL, *s=NULL, sbuf[100];
   void *SO_name = NULL;   
   FILE *outptr=NULL;
   SUMA_Boolean exists = NOPE;
   SUMA_INDEXING_ORDER d_order;
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-s;-sv;-o;");
   
   if (argc < 2) {
      usage_SurfToSurf(ps);
      exit (1);
   }
   
   Opt = SUMA_SurfToSurf_ParseInput (argv, argc, ps);

   /* if output surface requested, check on pre-existing file */
   if (ps->o_N_surfnames) {
      SO_name = SUMA_Prefix2SurfaceName(ps->o_surfnames[0], NULL, NULL, ps->o_FT[0], &exists);
      if (exists) {
         fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, ps->o_surfnames[0]);
         exit(1);
      }
   } 
   
   if (Opt->debug > 2) LocalHead = YUP;
   
   outname = SUMA_append_string(Opt->out_prefix,".1D");
   if (SUMA_filexists(outname)) {
      fprintf(SUMA_STDERR,"Output file %s exists.\n", outname);
      exit(1);
   }
   
   /* Load the surfaces from command line*/
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec != 1) {
      SUMA_S_Err( "Multiple spec at input.\n"
                  "Do not mix surface input types together\n");
      exit(1);
   }

      if (Spec->N_Surfs != 2) {
      SUMA_S_Err("2 surfaces expected.");
      exit(1);
   }
   
   SO1 = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], 0);
   if (!SO1) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
   }
   if (!SUMA_SurfaceMetrics(SO1, "EdgeList|MemberFace", NULL)) { SUMA_SL_Err("Failed to create edge list for SO1"); exit(1);  }
   if (Opt->fix_winding) {
            int orient, trouble;
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Making sure S1 is consistently orientated\n", FuncName);
            if (!SUMA_MakeConsistent (SO1->FaceSetList, SO1->N_FaceSet, SO1->EL, Opt->debug, &trouble)) {
               SUMA_SL_Err("Failed in SUMA_MakeConsistent");
            }
            if (trouble && LocalHead) {
               fprintf(SUMA_STDERR,"%s: trouble value of %d from SUMA_MakeConsistent.\n"
                                    "Inconsistencies were found and corrected unless \n"
                                    "stderr output messages from SUMA_MakeConsistent\n"
                                    "indicate otherwise.\n", FuncName, trouble);
            }
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Checking orientation.\n", FuncName);
            orient = SUMA_OrientTriangles (SO1->NodeList, SO1->N_Node, SO1->FaceSetList, SO1->N_FaceSet, 1, 0);
            if (orient < 0) { 
               /* flipping was done, dump the edge list since it is not automatically updated (should do that in function, just like in SUMA_MakeConsistent,  shame on you) */ 
               if (SO1->EL) SUMA_free_Edge_List(SO1->EL); SO1->EL = NULL; 
               if (!SUMA_SurfaceMetrics(SO1, "EdgeList", NULL)) { SUMA_SL_Err("Failed to create edge list for SO1"); exit(1);  }
               /* free normals, new ones needed (Normals should be flipped inside  of SUMA_OrientTriangles! (just like in SUMA_MakeConsistent) ) */
               if (SO1->NodeNormList) SUMA_free(SO1->NodeNormList); SO1->NodeNormList = NULL;
               if (SO1->FaceNormList) SUMA_free(SO1->FaceNormList); SO1->FaceNormList = NULL;
            }
            if (!orient) { fprintf(SUMA_STDERR,"Error %s:\nFailed in SUMA_OrientTriangles\n", FuncName); }
            if (LocalHead) {
               if (orient < 0) { SUMA_SL_Note("S1 was reoriented"); }
               else { SUMA_SL_Note("S1 was properly oriented"); }
            }
   }
  
   
   if (!SO1->NodeNormList || !SO1->FaceNormList) { SUMA_LH("Node Normals"); SUMA_RECOMPUTE_NORMALS(SO1); }
   if (Opt->NodeDbg >= SO1->N_Node) {
      SUMA_SL_Warn("node_debug index is larger than number of nodes in surface, ignoring -node_debug.");
      Opt->NodeDbg = -1;
   }
      
   SO2 = SUMA_Load_Spec_Surf(Spec, 1, ps->sv[1], 0);
   if (!SO2) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
   }  
   if (!SUMA_SurfaceMetrics(SO2, "EdgeList|MemberFace", NULL)) { SUMA_SL_Err("Failed to create edge list for SO2"); exit(1);  }
   if (!SO2->NodeNormList || !SO2->FaceNormList) { SUMA_LH("Node Normals"); SUMA_RECOMPUTE_NORMALS(SO2); }
   
   if (LocalHead) { 
      SUMA_LH("Surf1");
      SUMA_Print_Surface_Object(SO1, NULL);
      SUMA_LH("Surf2");
      SUMA_Print_Surface_Object(SO2, NULL);
   }
   
   /* a select list of nodes? */
   nodeind = NULL; N_nodeind = 0;
   if (Opt->in_nodeindices) {
      im = mri_read_1D(Opt->in_nodeindices);
      if (!im) { SUMA_SL_Err("Failed to read 1D file of node indices"); exit(1);}
      far = MRI_FLOAT_PTR(im);
      N_nodeind = nvec = im->nx;
      ncol = im->ny;
      if (ncol != 1) { SUMA_SL_Err("More than one column in node index input file."); exit(1);}
      nodeind = (int *)SUMA_calloc(nvec, sizeof(int));
      if (!nodeind) { SUMA_SL_Crit("Failed to allocate"); exit(1); }
      for (i=0;i<nvec;++i) { 
         nodeind[i] = (int)far[i]; 
         if (nodeind[i] < 0 || nodeind[i] >= SO1->N_Node) {
            fprintf(SUMA_STDERR, "Error %s: A node index of %d was found in input file %s, entry %d.\n"
                                 "Acceptable indices are positive and less than %d\n", 
                                    FuncName, nodeind[i], Opt->in_nodeindices, i, SO1->N_Node);
            exit(1);
         }
      } 
      mri_free(im); im = NULL;   /* done with that baby */
   }
   
   /* a preset directions vector ?*/
   projdir = NULL; 
   if (Opt->in_1D) {
      im = mri_read_1D(Opt->in_1D);
      if (!im) { SUMA_SL_Err("Failed to read 1D file of projection directions"); exit(1);}
      far = MRI_FLOAT_PTR(im);
      if (im->ny != 3) { SUMA_SL_Err("Need three columns in projection directions file."); exit(1); }
      if (im->nx != SO1->N_Node) {
         fprintf(SUMA_STDERR, "Error %s: You must have a direction for each node in SO1.\n"
                              "%d directions found but SO1 has %d nodes.\n", FuncName, im->nx, SO1->N_Node);
         exit(1);
      }

      /* change to row major major and make it match nodeind */
      projdir = (float *)SUMA_calloc(SO1->N_Node*3, sizeof(float));
      if (!projdir) { SUMA_SL_Crit("Failed to allocate"); exit(1); }
      for (i=0; i<SO1->N_Node; ++i) {
         projdir[3*i  ] = far[i              ];
         projdir[3*i+1] = far[i+  SO1->N_Node];
         projdir[3*i+2] = far[i+2*SO1->N_Node];
      }
      mri_free(im); im = NULL;   /* done with that baby */

   }
   
   if (SO_name) {
      /* user is interpolating surface coords, check on other input insanity */
      if (nodeind) {
         fprintf(SUMA_STDERR, "Error %s: You cannot combine option -o_TYPE with -node_indices", FuncName);
         exit(1);
      }
      if (Opt->in_name) {
         fprintf(SUMA_STDERR, "Error %s: You cannot combine option -o_TYPE with -data", FuncName);
         exit(1);
      }
   } 
   /* a file containing data? */
   if (Opt->in_name) {
      im_data = mri_read_1D(Opt->in_name);
      if (!im_data) { SUMA_SL_Err("Failed to read 1D file of data"); exit(1);}
      far_data = MRI_FLOAT_PTR(im_data);
      nvec_data = im_data->nx;
      ncol_data = im_data->ny;
      if (nvec_data != SO2->N_Node) {
         SUMA_SL_Err("Your data file must have one row for each node in surface 2.\n"); exit(1);
      }
      d_order = SUMA_COLUMN_MAJOR;
   } else { 
      im_data = NULL;
      far_data = SO2->NodeList;
      nvec_data = SO2->N_Node;
      ncol_data = 3;
      d_order = SUMA_ROW_MAJOR;
   }
   
     
   SUMA_LH("Going for the mapping of SO1 --> SO2");
   M2M = SUMA_GetM2M_NN( SO1, SO2, nodeind, N_nodeind, projdir, 0, Opt->NodeDbg);
   
   /* Now show the mapping results for a debug node ? */
   if (Opt->NodeDbg >= 0) {
      char *s = NULL;
      s = SUMA_M2M_node_Info(M2M, Opt->NodeDbg);
      fprintf(SUMA_STDERR,"%s: Debug for node %d ([%f, %f, %f])of SO1:\n%s\n\n", 
                           FuncName, Opt->NodeDbg, 
                           SO1->NodeList[3*Opt->NodeDbg], SO1->NodeList[3*Opt->NodeDbg+1], SO1->NodeList[3*Opt->NodeDbg+2],
                           s); 
      SUMA_free(s); s = NULL;
   }
   
   /* Now please do the interpolation */
   if (Opt->Data) {
      if (Opt->NearestNode > 1) dt = SUMA_M2M_interpolate(M2M, far_data, ncol_data, nvec_data, d_order, 0 );
      else if (Opt->NearestNode == 1) dt = SUMA_M2M_interpolate(M2M, far_data, ncol_data, nvec_data, d_order, 1 );
      if (!dt) {
         SUMA_SL_Err("Failed to interpolate");
         exit(1);
      }
   }
   
   SUMA_LH("Forming the output");
   outptr = fopen(outname,"w");
   if (!outptr) {
      SUMA_SL_Err("Failed to open file for output.\n");
      exit(1);
   }
   
   /* first create the header of the output */
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend_va(SS, 
      "#Mapping from nodes on surf 1 (S1) to nodes on surf 2 (S2)\n"
      "#  Surf 1 is labeled %s, idcode:%s\n"
      "#  Surf 2 is labeled %s, idcode:%s\n",
      SO1->Label, SO1->idcode_str, SO2->Label, SO2->idcode_str);
   icol = 0;
   SS = SUMA_StringAppend_va(SS, "#Col. %d:\n"
                                 "#     S1n (or nj): Index of node on S1\n"
                                 , icol); 
   ++icol;
   if (Opt->NearestNode > 1) {
      SS = SUMA_StringAppend_va(SS, 
         "#Col. %d..%d:\n"
         "#     S2ne_S1n: Indices of %d nodes on S2 \n"
         "#     that are closest neighbors of nj.\n"
         "#     The first index is that of the node on S2 that is closest \n"
         "#     to nj. If -1 then these values should be ignored because\n"
         "#     in such cases, nj's projection failed.\n" 
         , icol, icol+Opt->NearestNode-1, Opt->NearestNode); 
      icol += Opt->NearestNode;
      SS = SUMA_StringAppend_va(SS, 
         "#Col. %d..%d:\n"
         "#     S2we_S1n: Weights assigned to nodes on surf 2 (S2) \n"
         "#     that are closest neighbors of nj.\n"
         , icol, icol+Opt->NearestNode-1, Opt->NearestNode); 
      icol += Opt->NearestNode;
   } else if (Opt->NearestNode == 1) {
      SS = SUMA_StringAppend_va(SS, 
         "#Col. %d:\n"
         "#     S2ne_S1n: Index of the node on S2 (label:%s idcode:%s)\n"
         "#     that is the closest neighbor of nj.\n"
         "#     If -1 then this value should be ignored because\n"
         "#     nj's projection failed.\n" 
         , icol, SO2->Label, SO2->idcode_str); 
      ++icol;
   }
   if (Opt->NearestTriangle) { 
      SS = SUMA_StringAppend_va(SS, 
         "#Col. %d:\n"
         "#     S2t_S1n: Index of the S2 triangle that hosts node nj on S1.\n"
         "#     In other words, nj's closest projection onto S2 falls on \n"
         "#     triangle S2t_S1n\n"
         "#     If -1 then this value should be ignored because \n"
         "#     nj's projection failed.\n" 
         , icol); 
      ++icol; 
   }
   if (Opt->ProjectionOnMesh) { 
      SS = SUMA_StringAppend_va(SS, 
         "#Col. %d..%d:\n"
         "#     S2p_S1n: Coordinates of projection of nj onto S2\n"
         , icol, icol+2); 
      icol += 3; 
   }
   if (Opt->DistanceToMesh) {
      SS = SUMA_StringAppend_va(SS, 
         "#Col. %d:\n"
         "#     Closest distance from nj to S2\n"
         , icol); 
         ++icol;
   }
   if (Opt->Data) {
      if (!Opt->in_name) {
         SS = SUMA_StringAppend_va(SS, 
      "#Col. %d..%d:\n"
      "#     Interpolation using XYZ coordinates of S2 nodes that neighbor nj\n"
      "#     (same as coordinates of node's projection onto triangle in S2, \n"
      "#     if using barycentric interpolation)\n"
         , icol, icol+2); 
      icol += 3; 
} else {
SS = SUMA_StringAppend_va(SS, 
         "#Col. %d..%d:\n"
         "#     Interpolation of data at nodes on S2 that neighbor nj\n"
         "#     Data obtained from %s\n"
         , icol, icol+ncol_data-1, Opt->in_name);  icol += ncol_data;
      }
   } 
   s = SUMA_HistString("SurfToSurf", argc, argv, NULL);
   SS = SUMA_StringAppend_va(SS, 
                                "#History:\n"
                                "#%s\n", s); SUMA_free(s); s = NULL;
   SUMA_SS2S(SS,s);
   fprintf(outptr,"%s\n",s); SUMA_free(s); s = NULL;
   
   /* put headers atop columns */
   Nchar = 6; /* if you change this number you'll need to fix the formats below */
   for (i=0; i<icol; ++i) { sprintf(sbuf,"#%s", MV_format_fval2(i, Nchar -1)); fprintf(outptr,"%6s   ", sbuf); }
   fprintf(outptr,"\n");
   
   /* Now put in the values, make sure you parallel columns above! */
   for (i=0; i<M2M->M1Nn; ++i) {
      fprintf(outptr,"%6s   ", MV_format_fval2(M2M->M1n[i], Nchar));
      if (Opt->NearestNode > 0) {
         for (j=0; j<Opt->NearestNode; ++j) { 
            if (j < M2M->M2Nne_M1n[i]) fprintf(outptr,"%6s   ", MV_format_fval2(M2M->M2ne_M1n[i][j], Nchar)); 
            else fprintf(outptr,"%6s   ", "-1"); 
         } /* Neighboring nodes */
      } 
      if (Opt->NearestNode > 1) { /* add the weights */
         for (j=0; j<Opt->NearestNode; ++j) { 
            if (j < M2M->M2Nne_M1n[i]) fprintf(outptr,"%6s   ", MV_format_fval2(M2M->M2we_M1n[i][j], Nchar)); 
            else fprintf(outptr,"%6s   ", "0.0"); 
         } 
      }
      if (Opt->NearestTriangle) {
         fprintf(outptr,"%6s   ", MV_format_fval2(M2M->M2t_M1n[i], Nchar)); 
      }
      if (Opt->ProjectionOnMesh) {
         fprintf(outptr,"%6s   ", MV_format_fval2(M2M->M2p_M1n[3*i], Nchar));
         fprintf(outptr,"%6s   ", MV_format_fval2(M2M->M2p_M1n[3*i+1], Nchar));
         fprintf(outptr,"%6s   ", MV_format_fval2(M2M->M2p_M1n[3*i+2], Nchar)); 
      }
      if (Opt->DistanceToMesh) { 
         fprintf(outptr,"%6s   ", MV_format_fval2(M2M->PD[i], Nchar)); 
      }
      if (Opt->Data) {
         if (!Opt->in_name) {
            fprintf(outptr,"%6s   ", MV_format_fval2(dt[3*i], Nchar));
            fprintf(outptr,"%6s   ", MV_format_fval2(dt[3*i+1], Nchar));
            fprintf(outptr,"%6s   ", MV_format_fval2(dt[3*i+2], Nchar));
         } else { /* Column major business */
            for (j=0; j<ncol_data; ++j) { fprintf(outptr,"%6s   ", MV_format_fval2(dt[i+j*M2M->M1Nn], Nchar)); }
         }
      }
      fprintf(outptr,"\n");
   }
   
   /* do they want an output surface ? */
   if (SO_name) {
      float *tmpfv = NULL;
      SUMA_LH("Writing surface");
      tmpfv = SO1->NodeList;
      SO1->NodeList = dt;
      if (!SUMA_Save_Surface_Object (SO_name, SO1, ps->o_FT[0], ps->o_FF[0], NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
         exit (1);
      }
      SO1->NodeList = tmpfv; tmpfv = NULL;
   }
   
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }

   if (projdir) SUMA_free(projdir); projdir = NULL;
   if (SO_name) SUMA_free(SO_name); SO_name = NULL;   
   if (outptr) fclose(outptr); outptr = NULL;
   if (dt) SUMA_free(dt); dt = NULL;
   if (s) SUMA_free(s); s = NULL;
   if (im_data) mri_free(im_data); im_data = NULL;   /* done with the data */
   if (nodeind) SUMA_free(nodeind); nodeind = NULL;
   if (M2M) M2M = SUMA_FreeM2M(M2M);
   if (SO1) SUMA_Free_Surface_Object(SO1); SO1 = NULL;
   if (SO2) SUMA_Free_Surface_Object(SO2); SO2 = NULL;
   if (Spec) SUMA_free(Spec); Spec = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
