
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_ROIgrow (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_ROIgrow"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: ROIgrow <-i_TYPE SURF> <-roi_nodes ROI.1D> <-lim LIM>\n"
               "               [-prefix PREFIX]\n"
               "       A program to expand an ROI on the surface.\n"
               "       The roi is grown from each node by a user-determined\n"
               "       distance (geodesic, measured along the mesh).\n"
               "\n"
               "  Mandatory Parameters:\n"
               "     -i_TYPE SURF: Specify input surface.\n"
               "             You can also use -t* and -spec and -surf\n"
               "             methods to input surfaces. See below\n"
               "             for more details.\n"
               "     -roi_nodes ROI.1D: Name of 1D file containing\n"
               "                     node indices. Use the [] column\n"
               "                     specifier if you have more than\n"
               "                     one column in the ROI file.\n"
               "     -lim LIM: Distance to cover from each node.\n"
               "               The units of LIM are those of the surface's\n"
               "               node coordinates. Distances are calculated\n"
               "               along the surface's mesh."
               "\n"
               "  Optional Parameters:\n"
               "     -prefix PREFIX: Prefix of 1D output dataset.\n"
               "                     Default is ROIgrow\n"
               " \n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_ROIgrow_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_BrainWrap_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->out_prefix = NULL;
   Opt->d1 = -1;
   Opt->in_nodeindices = NULL;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_ROIgrow(ps);
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
      
      if (!brk && (strcmp(argv[kar], "-roi_nodes") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a parameter after -roi_nodes \n");
            exit (1);
         }
         
         Opt->in_nodeindices = argv[++kar];
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
      
      if (!brk && (strcmp(argv[kar], "-lim") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a parameter after -lim \n");
            exit (1);
         }
         
         Opt->d1 = atof(argv[++kar]);
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
   
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("ROIgrow"); 
   if (Opt->d1 < 0) {
      fprintf (SUMA_STDERR,"-lim option not specified.");
      exit (1);
   }
   if (!Opt->in_nodeindices) {
      fprintf (SUMA_STDERR,"-roi_nodes option not specified.");
      exit (1);
   }
   SUMA_RETURN(Opt);
}

byte * SUMA_ROIgrow( SUMA_SurfaceObject *SO, 
                     int *nodeind, int N_nodeind,
                     float lim)
{
   static char FuncName[]={"SUMA_ROIgrow"};
   byte *nmask = NULL;
   int i, n, nj;
   SUMA_GET_OFFSET_STRUCT *OffS = NULL; 
   struct timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !nodeind || lim <= 0) {
      SUMA_S_Err("Input error");
      SUMA_RETURN(nmask);
   }
      
   SUMA_etime(&start_time_all,0);

   OffS = SUMA_Initialize_getoffsets (SO->N_Node);
   /* for each node, find the nodes within a particular distance */
   nmask = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte));
   for (i=0; i<N_nodeind; ++i) {
      n = nodeind[i];
      nmask[n] = 1;
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Calculating offsets from node %d\n",FuncName, n);
      if (i == 0) {
         SUMA_etime(&start_time,0);
      }
      SUMA_getoffsets2 (n, SO, lim, OffS, NULL, 0);
      if (LocalHead && i == 99) {
         etime_GetOffset = SUMA_etime(&start_time,1);
         fprintf(SUMA_STDERR, "%s: Search to %f mm took %f seconds for %d nodes.\n"
                  "Projected completion time: %f minutes\n",
                  FuncName, lim, etime_GetOffset, i+1,
                  etime_GetOffset * N_nodeind / 60.0 / (i+1));
      }
      
      /* go over all nodes in offset */
      for (nj=0; nj < OffS->N_Nodes; nj++) { /* nj is a node index */
         if (OffS->LayerVect[nj] > 0) { /* nj is in the neighborhood of n */
            if (!nmask[nj]) { /* there's potential */
               if (OffS->OffVect[nj] <=lim) { /* within the limit */
                  nmask[nj] = 1;
               }   
            }
         }
      }
      
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Recycling OffS\n", FuncName);
      SUMA_Recycle_getoffsets (OffS);
      if (LocalHead)
         fprintf(SUMA_STDERR,"%s: Done.\n", FuncName);
   
   }
   etime_GetOffset_all = SUMA_etime(&start_time_all,1);
   if (LocalHead) fprintf(SUMA_STDERR, "%s: Done.\nSearch to %f mm took %f minutes for %d nodes.\n" ,
                            FuncName, lim, etime_GetOffset_all / 60.0 , SO->N_Node);
   SUMA_Free_getoffsets(OffS);
   
   SUMA_RETURN(nmask);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"ROIgrow"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   byte *nmask=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec=0, *nodeind = NULL, N_nodeind, i;
   MRI_IMAGE *im = NULL;
	int nvec, ncol=0;
   float *far=NULL;
   char *outname = NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;");
   
   if (argc < 2) {
      usage_ROIgrow(ps);
      exit (1);
   }
   
   Opt = SUMA_ROIgrow_ParseInput (argv, argc, ps);

   
   outname = SUMA_append_string(Opt->out_prefix,".1D");
   if (SUMA_filexists(outname)) {
      fprintf(SUMA_STDERR,"Output file %s exists.\n", outname);
      exit(1);
   }
   
   if (Opt->debug > 2) LocalHead = YUP;
   /* Load the surfaces from command line*/
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec != 1) {
      SUMA_S_Err( "Multiple spec at input.\n");
      exit(1);
   }

   if (Spec->N_Surfs != 1) {
      SUMA_S_Err("1 surface expected.");
      exit(1);
   }
   
   SO = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], 0);
   if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
   }
   if (!SUMA_SurfaceMetrics(SO, "EdgeList|MemberFace", NULL)) { SUMA_SL_Err("Failed to create edge list for SO"); exit(1);  }
   if (LocalHead) { 
      SUMA_LH("Surf");
      SUMA_Print_Surface_Object(SO, NULL);
   }

   /* read the ROI nodes */
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
         if (nodeind[i] < 0 || nodeind[i] >= SO->N_Node) {
            fprintf(SUMA_STDERR, "Error %s: A node index of %d was found in input file %s, entry %d.\n"
                                 "Acceptable indices are positive and less than %d\n", 
                                    FuncName, nodeind[i], Opt->in_nodeindices, i, SO->N_Node);
            exit(1);
         }
      } 
      mri_free(im); im = NULL;   /* done with that baby */
   }

   if (!(nmask = SUMA_ROIgrow(SO, nodeind, N_nodeind, Opt->d1))) {
      SUMA_S_Err("Failed in SUMA_ROIgrow");
      exit(1);
   }  
   
   {
      FILE *fout = NULL;
      
      fout = fopen(outname, "w");
      fprintf(fout,  "#Col. 0 Node index\n");
      
      for (i=0; i<SO->N_Node; ++i) {
         if (nmask[i]) fprintf(fout,"%d\n", i);
      }
      fclose(fout); fout = NULL;
   }
   
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (outname) SUMA_free(outname); outname = NULL;
   if (nmask) SUMA_free(nmask); nmask = NULL;
   if (nodeind) SUMA_free(nodeind); nodeind = NULL;
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
