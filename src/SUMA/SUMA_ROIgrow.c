
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
               "     -roi_labels ROI_LABELS: Data column containing\n"
               "                             integer labels of ROIs.\n"
               "                             Each integer label gets\n"
               "                             grown separately.\n"
               "                             If ROI_LABELS is in niml\n"
               "                             format, then you need not\n"
               "                             use -roi_nodes because node \n"
               "                             indices are stored with the \n"
               "                             labels.\n"
               "        Notice: With this option, an output is created for\n"
               "                each label. The output contains two columns:\n"
               "                One with node indices and one with the label.\n"
               "                When this option is not used, you get one\n"
               "                column out containing node indices only.\n"
               "     -full_list: Output a row for each node on the surface.\n"
               "                 Nodes not in the grown ROI, receive a 0 for\n"
               "                 a label. This option is ONLY for use with\n"
               "                 -roi_labels. This way you can combine \n"
               "                 multiple grown ROIs with, say, 3dcalc.\n"
               "                 For such operations, you are better off \n"
               "                 using powers of 2 for integer labels.\n"
               "     -roi_nodes ROI_INDICES: Data column containing\n"
               "                     node indices of ROI. \n"
               "                     Use the [] column\n"
               "                     specifier if you have more than\n"
               "                     one column in the data file.\n"
               "                     To get node indices from a niml dset\n"
               "                     use the '[i]' selector.\n"
               "     -grow_from_edge: Grow ROIs from their edges rather than\n"
               "                      the brute force default. This might \n"
               "                      make the program faster on large ROIs  \n"
               "                      and large surfaces.\n"
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
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_ROIgrow_ParseInput(
               char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
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
   Opt->in_name = NULL;
   Opt->PushToEdge = 0;
   Opt->b1 = 0;
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
      
      if (!brk && (strcmp(argv[kar], "-roi_labels") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a parameter after -roi_labels \n");
            exit (1);
         }
         
         Opt->in_name = argv[++kar];
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
      
      if (!brk && (strcmp(argv[kar], "-grow_from_edge") == 0))
      {
         Opt->PushToEdge = 1;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-full_list") == 0))
      {
         Opt->b1 = 1;
         brk = YUP;
      }
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,
                  "Error %s:\nOption %s not understood.\n"
                  "Try -help for usage\n", FuncName, argv[kar]);
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
   if (!Opt->in_nodeindices && !Opt->in_name) {
      fprintf (SUMA_STDERR,
               "neither -roi_nodes nor -roi_labels option are specified.\n");
      exit (1);
   }
   if (Opt->b1 && !Opt->in_name) {
      SUMA_S_Err("Cannot use -full_list without -roi_labels");
      exit(1);
   }
   SUMA_RETURN(Opt);
}

byte * SUMA_ROIgrow( SUMA_SurfaceObject *SO, 
                     int *nodeind, int N_nodeind,
                     float lim, int useedge)
{
   static char FuncName[]={"SUMA_ROIgrow"};
   byte *nmask = NULL;
   int N_CE, i, n, nj, *nodeindfast=NULL, *nodeindfastunq=NULL;
   SUMA_CONTOUR_EDGES *CE = NULL;
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
   if (useedge) { /* faster, perhaps, for larger ROIs*/
      SUMA_LH ("In edge growing mode!");
      if ((CE = SUMA_GetContour( SO, nodeind, N_nodeind, &N_CE, 
                                 0, NULL, NULL, 1))) {
         if (N_CE > 0) {
            /* replace nodeind by nodes forming edges only*/
            nodeindfast = (int *)SUMA_calloc(2*N_CE, sizeof(int));
            /* filler up */
            i=0;
            while (i<2*N_CE) {
               nodeindfast[i] = CE[i/2].n1; ++i;
               nodeindfast[i] = CE[i/2].n2; ++i;
            }
            /* unique is what we want */
            nodeindfastunq = SUMA_UniqueInt(nodeindfast, 2*N_CE, &nj, 0);
            if (LocalHead) {
               char *SS=SUMA_ShowMeSome(  nodeind, SUMA_int, 
                                          N_nodeind, N_nodeind, NULL);
               SUMA_LHv("Nodes given:\n%s\n",SS); SUMA_free(SS); 
               SS=SUMA_ShowMeSome(nodeindfastunq, SUMA_int, nj, nj, NULL);
               SUMA_LHv("Nodes seletcted:\n%s\n",SS); SUMA_free(SS); 
            }
            SUMA_free(nodeindfast); 
            SUMA_free(CE); CE = NULL;
            SUMA_LHv("ROI is now to be grown from \n"
                         "%d rather than %d nodes.\n", nj, N_nodeind);
            /* fill up nmask with initial ROI first */
            for (i=0; i<N_nodeind; ++i) { nmask[nodeind[i]] = 1; }
            /* now pretend all you have is edges nodes */ 
            nodeind = nodeindfastunq;
            N_nodeind = nj;
         }
      }
   }
   for (i=0; i<N_nodeind; ++i) {
      n = nodeind[i];
      nmask[n] = 1;
      if (LocalHead) 
         fprintf(SUMA_STDERR,
                  "%s: Calculating offsets from node %d\n",FuncName, n);
      if (i == 0) {
         SUMA_etime(&start_time,0);
      }
      SUMA_getoffsets2 (n, SO, lim, OffS, NULL, 0);
      if (LocalHead && i == 99) {
         etime_GetOffset = SUMA_etime(&start_time,1);
         fprintf(
            SUMA_STDERR, "%s: Search to %f mm took %f seconds for %d nodes.\n"
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
   if (LocalHead) 
      fprintf(SUMA_STDERR, 
         "%s: Done.\nSearch to %f mm took %f minutes for %d nodes.\n" ,
         FuncName, lim, etime_GetOffset_all / 60.0 , SO->N_Node);
   SUMA_Free_getoffsets(OffS);
   
   if (nodeindfastunq) SUMA_free(nodeindfastunq); nodeindfastunq = NULL;
   SUMA_RETURN(nmask);
}

#define SUMA_WRITE_GROWN_MASK(ilb){ \
   FILE *fout = NULL;   \
   int m_i; \
   char *m_oname = NULL, m_stmp[256]; \
   if (ilb < 0) sprintf(m_stmp, ".1D"); \
   else sprintf(m_stmp, ".%d.1D", ilb);  \
   m_oname = SUMA_append_string(Opt->out_prefix, m_stmp);     \
   if (!THD_ok_overwrite() && SUMA_filexists(m_oname)) {   \
      fprintf(SUMA_STDERR,"Output file %s exists.\n", outname);   \
      exit(1); \
   }  \
   fout = fopen(m_oname, "w");   \
   if (ilb < 0) { \
      fprintf(fout,  "#Col. 0 Node index\n");   \
      for (m_i=0; m_i<SO->N_Node; ++m_i) {   \
         if (nmask[m_i]) fprintf(fout,"%d\n", m_i); \
      }  \
   } else { \
      fprintf(fout,  "#Col. 0 Node index\n");   \
      fprintf(fout,  "#Col. 1 ROI val.\n");   \
      for (m_i=0; m_i<SO->N_Node; ++m_i) {   \
         if (nmask[m_i]) fprintf(fout,"%d   %d\n", m_i, ilb); \
         else if (Opt->b1) fprintf(fout,"%d   %d\n", m_i, 0);  \
      }  \
   }  \
   fclose(fout); fout = NULL; \
   SUMA_free(m_oname);  \
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"ROIgrow"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   byte *nmask=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec=0, *nodeind = NULL, N_nodeind, i, cnt, j;
   int *nodeind_tmp=NULL, N_nodeind_tmp, free_nodeind = 1;
	int  *nodelabels = NULL, N_nodelabels, *lbls=NULL, N_lbls=0;
   char *outname = NULL;
   SUMA_DSET *inds_dset=NULL, *lbls_dset=NULL;
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
   if (!THD_ok_overwrite() && SUMA_filexists(outname)) {
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
   if (!SUMA_SurfaceMetrics_eng(SO, "EdgeList|MemberFace", 
                                 NULL, 0, SUMAg_CF->DsetList)) {
      SUMA_SL_Err("Failed to create edge list for SO"); 
      exit(1);  
   }
   if (LocalHead) { 
      SUMA_LH("Surf");
      SUMA_Print_Surface_Object(SO, NULL);
   }

   /* read the ROI nodes */
   nodeind = NULL; N_nodeind = 0;
   if (Opt->in_nodeindices) {
      SUMA_DSET_FORMAT form=SUMA_NO_DSET_FORMAT;
      if (!(inds_dset = SUMA_LoadDset_s(Opt->in_nodeindices, &form, 0))) {
         SUMA_S_Errv("Could not read input '%s'\n", Opt->in_nodeindices);
         exit(1);
      }
      if (SDSET_VECNUM(inds_dset) != 1) {
         SUMA_S_Errv(
            "Node indices dset (%s) has more than one column.\n",
            Opt->in_nodeindices);
         exit(1);   
      }
      if (!(nodeind = SUMA_DsetCol2Int(inds_dset, 0, 1))) {
         SUMA_S_Err("Failed to extract nodeindices");
         exit(1);
      }
      N_nodeind = SDSET_VECFILLED(inds_dset); 
      
      for (i=0;i<N_nodeind;++i) { 
         if (nodeind[i] < 0 || nodeind[i] >= SO->N_Node) {
            fprintf(
               SUMA_STDERR, 
               "Error %s: A node index of %d was found in dset %s row %d.\n"
               "Acceptable indices are positive and less than %d\n", 
               FuncName, nodeind[i], Opt->in_nodeindices, i, SO->N_Node);
            exit(1);
         }
      } 
   }

   nodelabels = NULL; N_nodelabels = 0;
   if (Opt->in_name) {
      SUMA_DSET_FORMAT form=SUMA_NO_DSET_FORMAT;
      lbls_dset = SUMA_LoadDset_s(Opt->in_name, &form, 0);
      if (SDSET_VECNUM(lbls_dset) != 1) {
         SUMA_S_Errv(
            "Node labels dset (%s) has more than one column.\n",
            Opt->in_name);
         exit(1);   
      }
      if (!(nodelabels = SUMA_DsetCol2Int(lbls_dset, 0, 1))) {
         SUMA_S_Err("Failed to extract nodelabels");
         exit(1);
      }
      N_nodelabels = SDSET_VECFILLED(lbls_dset);
      if (!nodeind) {/* node indices are from the node index of the dset */
         if (!(nodeind = SUMA_GetNodeDef(lbls_dset))) {
            SUMA_S_Err("No node indices to go with node labels!");
            exit(1);
         }
         N_nodeind = SDSET_VECFILLED(lbls_dset); free_nodeind = 0;
      }
   }
   
   if (nodelabels) {   
      if (N_nodelabels != N_nodeind) {
         SUMA_S_Err("number of labels differs from number of node indices");
         exit(1);
      }
      
      /* grow each label separately */
      if (!(lbls =  SUMA_UniqueInt (  nodelabels, N_nodelabels, &N_lbls, 0))) {
         SUMA_S_Crit("Failed to uniquate");
         exit(1);
      }
      SUMA_LHv("Have %d unique labels...\n", N_lbls);
      /* Now do the deeds */
      for (i=0; i<N_lbls; ++i) {
         if (lbls[i]) {
            /* make a copy of nodeind */
            if (!(nodeind_tmp = 
                  (int *)SUMA_calloc(N_nodelabels, sizeof(int)))) {
               SUMA_S_Crit("Failed to allocate");
               exit(1);
            }
            cnt = 0;
            for (j=0; j<N_nodelabels; ++j) {
               if (nodelabels[j]==lbls[i]) {
                  nodeind_tmp[cnt] = nodeind[j];
                  ++cnt;
               }
            }
            N_nodeind_tmp = cnt;
            SUMA_LHv("Processing label %d, %d nodes...\n",
                     lbls[i], N_nodeind_tmp);
            if (!(nmask = SUMA_ROIgrow(SO, nodeind_tmp, 
                                       N_nodeind_tmp, Opt->d1,
                                       Opt->PushToEdge))) {
               SUMA_S_Err("Failed in SUMA_ROIgrow");
               exit(1);
            }
            SUMA_WRITE_GROWN_MASK(lbls[i]);
            SUMA_free(nmask); nmask=NULL; 
            SUMA_free(nodeind_tmp); nodeind_tmp = NULL;   
         }
      }
      SUMA_free(lbls); lbls = NULL;
   } else {
      if (!(nmask = SUMA_ROIgrow(SO, nodeind,
                                 N_nodeind, Opt->d1, 
                                 Opt->PushToEdge))) {
         SUMA_S_Err("Failed in SUMA_ROIgrow");
         exit(1);
      }
      SUMA_WRITE_GROWN_MASK(-1);  
   }

   
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { 
            SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (outname) SUMA_free(outname); outname = NULL;
   if (nmask) SUMA_free(nmask); nmask = NULL;
   if (free_nodeind && nodeind) SUMA_free(nodeind); nodeind = NULL;
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (lbls_dset) SUMA_FreeDset(lbls_dset); lbls_dset = NULL;
   if (inds_dset) SUMA_FreeDset(inds_dset); inds_dset = NULL;
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
