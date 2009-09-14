#include "SUMA_suma.h"

#undef STAND_ALONE

#if defined SUMA_SurfClust_STANDALONE
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
#endif

static int BuildMethod;

#ifdef SUMA_SurfClust_STANDALONE
void usage_SUMA_SurfClust ()
   {
      static char FuncName[]={"usage_SUMA_SurfClust"};
      char * s = NULL;
      s = SUMA_help_basics();
      printf ( 
"\nUsage: A program to perform clustering analysis surfaces.\n"
"  SurfClust <-spec SpecFile> \n"/* [<-sv sv_name>] */
"            <-surf_A insurf> \n"
"            <-input inData.1D dcol_index> \n"
"            <-rmm rad>\n"
"            [-amm2 minarea]\n"
"            [-prefix OUTPREF]  \n"
"            [-out_clusterdset] [-out_roidset] \n"
"            [-out_fulllist]\n"
"            [-sort_none | -sort_n_nodes | -sort_area]\n"
"\n"
"  The program can outputs a table of the clusters on the surface,\n"
"  a mask dataset formed by the different clusters and a clustered\n"
"  version of the input dataset.\n"
"\n"
"  Mandatory parameters:\n"
"     -spec SpecFile: The surface spec file.\n"
"     -surf_A insurf: The input surface name.\n"
"     -input inData.1D dcol_index: The input 1D dataset\n"
"                                  and the index of the\n"
"                                  datacolumn to use\n"
"                                  (index 0 for 1st column).\n"
"                                  Values of 0 indicate \n"
"                                  inactive nodes.\n"
"     -rmm rad: Maximum distance between an activated node\n"
"               and the cluster to which it belongs.\n"
"               Distance is measured on the surface's graph (mesh).\n"
"               If you want the distance to be in number of edges,\n"
"               set rad to -N for an N edge max distance.\n"
"               For example -rmm -2 means that nodes connected\n"
"               by 1 or two edges are in a cluster.\n"
"\n"
"  Optional Parameters:\n"
"     -thresh_col tcolind: Index of thresholding column.\n"
"                          Default is column 0.\n "
"     -thresh tval: Apply thresholding prior to clustering.\n"
"                   A node n is considered if thresh_col[n] > tval.\n"
"     -athresh tval: Apply absolute thresholding prior to clustering.\n"
"                    A node n is considered if | thresh_col[n] | > tval.\n" 
"     -amm2 minarea: Do not output resutls for clusters having\n"
"                    an area less than minarea.\n"
"     -prefix OUTPREF: Prefix for output.\n"
"                      Default is the prefix of \n"
"                      the input dataset.\n"
"                      If this option is used, the\n"
"                      cluster table is written to a file called\n"
"                      OUTPREF_ClstTable_rXX_aXX.1D. Otherwise the\n"
"                      table is written to stdout. \n"
"                      You can specify the output format by adding\n"
"                      extensions to OUTPREF. For example, \n"
"                      OUTPREF.1D.dset will force the output to be \n"
"                      in the .1D format. \n"
"                      See ConvertDset for many more format options.\n"
"     -out_clusterdset: Output a clustered version of inData.1D \n"
"                       preserving only the values of nodes that \n"
"                       belong to clusters that passed the rmm and amm2\n" 
"                       conditions above.\n"
"                       The clustered dset's prefix has\n"
"                       _Clustered_rXX_aXX affixed to the OUTPREF\n"
"     -out_roidset: Output an ROI dataset with the value\n"
"                   at each node being the rank of its\n"
"                   cluster. The ROI dataset's prefix has\n"
"                   _ClstMsk_rXX_aXX affixed to the OUTPREF\n"
"                   where XX represent the values for the\n"
"                   the -rmm and -amm2 options respectively.\n"
"                   The program will not overwrite pre-existing\n"
"                   dsets.\n"
"     -prepend_node_index: Force the output dataset to have node\n"
"                    indices in column 0 of output. Use this option\n"
"                    if you are parsing .1D format datasets.\n"
"     -out_fulllist: Output a value for all nodes of insurf.\n"
"                    This option must be used in conjuction with\n"
"                    -out_roidset and/or out_clusterdset.\n"
"                    With this option, the output files might\n"
"                    be mostly 0, if you have small clusters.\n"
"                    However, you should use it if you are to \n"
"                    maintain the same row-to-node correspondence\n"
"                    across multiple datasets.\n"  
"     -sort_none: No sorting of ROI clusters.\n"
"     -sort_n_nodes: Sorting based on number of nodes\n"
"                    in cluster.\n"
"     -sort_area: Sorting based on area of clusters \n"
"                 (default).\n"
"     -update perc: Pacify me when perc of the data have been\n"
"                   processed. perc is between 1%% and 50%%.\n"
"                   Default is no update.\n"
"     -no_cent: Do not find the central nodes.\n"
"               Finding the central node is a \n"
"               relatively slow operation. Use\n"
"               this option to skip it.\n"
/*"     -sv SurfaceVolume \n"
"        If you supply a surface volume, the coordinates of the input surface.\n"
"        are modified to SUMA's convention and aligned with SurfaceVolume.\n"
"     -acpc: Apply acpc transform (which must be in acpc version of \n"
"        SurfaceVolume) to the surface vertex coordinates. \n"
"        This option must be used with the -sv option.\n"
"     -tlrc: Apply Talairach transform (which must be a talairach version of \n"
"        SurfaceVolume) to the surface vertex coordinates. \n"
"        This option must be used with the -sv option.\n"
"     -MNI_rai/-MNI_lpi: Apply Andreas Meyer Lindenberg's transform to turn \n"
"        AFNI tlrc coordinates (RAI) into MNI coord space \n"
"        in RAI (with -MNI_rai) or LPI (with -MNI_lpi)).\n"
"        NOTE: -MNI_lpi option has not been tested yet (I have no data\n"
"        to test it on. Verify alignment with AFNI and please report\n"
"        any bugs.\n" 
"        This option can be used without the -tlrc option.\n"
"        But that assumes that surface nodes are already in\n"
"        AFNI RAI tlrc coordinates .\n"    
"\n" */
"\n"
"  The cluster table output:\n"
"  A table where ach row shows results from one cluster.\n"
"  Each row contains 13 columns:   \n"
"     Col. 0  Rank of cluster (sorting order).\n"
"     Col. 1  Number of nodes in cluster.\n"
"     Col. 2  Total area of cluster. Units are the \n"
"             the surface coordinates' units^2.\n"
"     Col. 3  Mean data value in cluster.\n"
"     Col. 4  Mean of absolute data value in cluster.\n"
"     Col. 5  Central node of cluster (see below).\n"
"     Col. 6  Weighted central node (see below).\n"
"     Col. 7  Minimum value in cluster.\n"
"     Col. 8  Node where minimum value occurred.\n"
"     Col. 9  Maximum value in cluster.\n"
"     Col. 10 Node where maximum value occurred.\n"
"     Col. 11 Variance of values in cluster.\n"
"     Col. 12 Standard error of the mean ( sqrt(variance/number of nodes) ).\n"
"   The CenterNode n is such that: \n"
"   ( sum (Uia * dia * wi) ) - ( Uca * dca * sum (wi) ) is minimal\n" 
"     where i is a node in the cluster\n"
"           a is an anchor node on the surface\n"
"           sum is carried over all nodes i in a cluster\n"
"           w. is the weight of a node \n"
"              = 1.0 for central node \n"
"              = value at node for the weighted central node\n"
"           U.. is the unit vector between two nodes\n"
"           d.. is the distance between two nodes on the graph\n"
"              (an approximation of the geodesic distance)\n"
"   If -no_cent is used, CenterNode columns are set to 0.\n"
"\n"
"%s"
"\n"
      , s);
/* Can use -O2_NR, -O2 and -Oll to try different implementations 
of the cluster building function. -O2 and -Oll are recursive and
work well for small clusters, a catastrophy for large clusters.
-O2_NR is fast and reliable. */
       SUMA_free(s); s = NULL;        
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
       exit (0);
   }

/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_SURFCLUST_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_SURFCLUST_OPTIONS *SUMA_SurfClust_ParseInput (char *argv[], int argc)
{
   static char FuncName[]={"SUMA_SurfClust_ParseInput"}; 
   SUMA_SURFCLUST_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outname;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_SURFCLUST_OPTIONS *)SUMA_malloc(sizeof(SUMA_SURFCLUST_OPTIONS));
   
   kar = 1;
   Opt->spec_file = NULL;
   Opt->out_prefix = NULL;
   Opt->oform = SUMA_ASCII_NIML;
   Opt->sv_name = NULL;
   Opt->N_surf = -1;
   Opt->DistLim = -1.5;
   Opt->AreaLim = -1.0;
   Opt->in_name = NULL;
   Opt->nodecol = -1;
   Opt->labelcol = -1;
   Opt->OutROI = NOPE;
   Opt->OutClustDset = NOPE;
   Opt->FullROIList = NOPE;
   Opt->WriteFile = NOPE;
   Opt->DoThreshold = 0;
   Opt->Thresh = 0.0;
   Opt->tind = 0;
   Opt->prepend_node_index = NOPE;
   Opt->update = 0;
   Opt->SortMode = SUMA_SORT_CLUST_NOT_SET;
   Opt->DoCentrality = 1;
   for (i=0; i<SURFCLUST_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
   BuildMethod = SUMA_OFFSETS2_NO_REC;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_SurfClust();
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-no_cent") == 0)) {
         Opt->DoCentrality = 0;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-O2") == 0)) {
         BuildMethod = SUMA_OFFSETS2;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-O2_NR") == 0)) {
         BuildMethod = SUMA_OFFSETS2_NO_REC;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-Oll") == 0)) {
         BuildMethod = SUMA_OFFSETS_LL;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec \n");
				exit (1);
			}
			Opt->spec_file = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-sv") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -sv \n");
				exit (1);
			}
			Opt->sv_name = argv[kar];
			brk = YUP;
		}
      
       
      if (!brk && (strcmp(argv[kar], "-update") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -update \n");
				exit (1);
			}
			Opt->update = atof(argv[kar]);
         if (Opt->update < 1 || Opt->update > 100) {
            fprintf (SUMA_STDERR, "-update needs a parameter between 1 and 50 (I have %.1f)\n", Opt->update);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix \n");
				exit (1);
			}
         Opt->oform = SUMA_GuessFormatFromExtension(argv[kar], NULL);		
         if (Opt->oform == SUMA_NO_DSET_FORMAT) Opt->oform = SUMA_ASCII_NIML;
         Opt->out_prefix = SUMA_RemoveDsetExtension_s(argv[kar], Opt->oform);	
         Opt->WriteFile = YUP;
         brk = YUP;
		}
            
      if (!brk && (strncmp(argv[kar], "-surf_", 6) == 0)) {
			if (kar + 1>= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -surf_X SURF_NAME \n");
				exit (1);
			}
			ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFCLUST_MAX_SURF) {
            fprintf (SUMA_STDERR, 
                     "-surf_X SURF_NAME option is out of range.\n"
                     "Only %d surfaces are allowed. \n"
                     "Must start with surf_A for first surface.\n", 
                     SURFCLUST_MAX_SURF);
				exit (1);
         }
         kar ++;
         Opt->surf_names[ind] = argv[kar];
         Opt->N_surf = ind+1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 arguments after -input \n");
				exit (1);
			}
			Opt->in_name = argv[kar]; kar ++;
         /* no need for that one Opt->nodecol = atoi(argv[kar]); kar ++; */
         Opt->labelcol = atoi(argv[kar]); 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-rmm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -rmm \n");
				exit (1);
			}
			Opt->DistLim = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-thresh") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -thresh \n");
				exit (1);
			}
			Opt->DoThreshold = 1;
         Opt->Thresh = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-athresh") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -athresh \n");
				exit (1);
			}
			Opt->DoThreshold = 2;
         Opt->Thresh = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-thresh_col") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -thresh_col \n");
				exit (1);
			}
         Opt->tind = atoi(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-amm2") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -amm2 \n");
				exit (1);
			}
			Opt->AreaLim = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-out_roidset") == 0)) {
         Opt->OutROI = YUP;
			brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-prepend_node_index") == 0)) {
         Opt->prepend_node_index = YUP;
			brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-out_clusterdset") == 0)) {
         Opt->OutClustDset = YUP;
			brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-out_fulllist") == 0)) {
         Opt->FullROIList = YUP;
			brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-sort_none") == 0)) {
         Opt->SortMode = SUMA_SORT_CLUST_NO_SORT;
			brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-sort_n_nodes") == 0)) {
         Opt->SortMode = SUMA_SORT_CLUST_BY_NUMBER_NODES;
			brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-sort_area") == 0)) {
         Opt->SortMode = SUMA_SORT_CLUST_BY_AREA;
			brk = YUP;
      }
      
      if (!brk) {
			fprintf (SUMA_STDERR,
                  "Error %s:\n"
                  "Option %s not understood.\n"
                  "Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   /* sanitorium */
   if (Opt->DistLim == -1.5f) {
      fprintf (SUMA_STDERR, "must use option -rmm  \n");
      exit(1);
   }
   if (!Opt->out_prefix) {
      Opt->out_prefix = 
         SUMA_RemoveDsetExtension_s(Opt->in_name, SUMA_NO_DSET_FORMAT);
   }
   
   if (Opt->SortMode == SUMA_SORT_CLUST_NOT_SET) { 
      Opt->SortMode = SUMA_SORT_CLUST_BY_AREA; }

   if (BuildMethod == SUMA_OFFSETS2) { SUMA_S_Note("Using Offsets2"); }
   else if (BuildMethod == SUMA_OFFSETS_LL) { SUMA_S_Note("Using Offsets_ll"); } 
   else if (BuildMethod == SUMA_OFFSETS2_NO_REC) { 
      if (LocalHead) SUMA_S_Note("Using no recursion"); }
   else {
      SUMA_SL_Err("Bad BuildMethod");
      exit(1);
   } 
   
   SUMA_SurfClust_Set_Method(BuildMethod);
   
   if (Opt->FullROIList && !(Opt->OutROI || Opt->OutClustDset)) {
      SUMA_SL_Err("-out_fulllist must be used in conjunction "
                  "with -out_ROIdset or -out_clusterdset");
      exit(1);
   }   
   SUMA_RETURN(Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfClust"}; 
	int kar, SO_read, *ni=NULL, N_ni, cnt, i, *nip=NULL;
   float *data_old = NULL, *far = NULL, *nv=NULL, *nt = NULL;
   void *SO_name = NULL;
   SUMA_SurfaceObject *SO = NULL, *SOnew = NULL;
   MRI_IMAGE *im = NULL;
   SUMA_DSET_FORMAT iform;
   SUMA_SURFCLUST_OPTIONS *Opt;  
	SUMA_SurfSpecFile Spec; 
   DList *list = NULL;
   SUMA_DSET *dset = NULL;
   float *NodeArea = NULL;
   FILE *clustout=NULL;
   char *ClustOutName = NULL, *params=NULL, stmp[200];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   
   
   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   if (argc < 6)
       {
          usage_SUMA_SurfClust();
          exit (1);
       }
   
   Opt = SUMA_SurfClust_ParseInput (argv, argc);
   
   if (Opt->WriteFile) {
      if (Opt->DistLim >= 0.0) {
         if (Opt->AreaLim < 0) sprintf(stmp,"_ClstTable_r%.1f.1D", Opt->DistLim);
         else sprintf(  stmp,"_ClstTable_r%.1f_a%.1f.1D", 
                        Opt->DistLim, Opt->AreaLim);
      } else {
         if (Opt->AreaLim < 0) 
            sprintf(stmp,"_ClstTable_e%d.1D", -(int)Opt->DistLim);
         else sprintf(  stmp,"_ClstTable_e%d_a%.1f.1D", 
                        -(int)Opt->DistLim, Opt->AreaLim);
      }
      ClustOutName = SUMA_append_string(Opt->out_prefix, stmp);   
      if (SUMA_filexists(ClustOutName) && !THD_ok_overwrite()) {
         fprintf (SUMA_STDERR,
                  "Error %s:\n"
                  "Output file %s exists, will not overwrite.\n", 
                  FuncName, ClustOutName);
         exit(1);
      }
   }

   /* read  surfaces */
   if (!SUMA_AllocSpecFields(&Spec)) {
      SUMA_S_Err("Failed to allocate Spec Fields");
      exit(1);
   }
   if (!SUMA_Read_SpecFile (Opt->spec_file, &Spec)) {
		fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
		exit(1);
	}
   SO_read = SUMA_spec_select_surfs(
                  &Spec, Opt->surf_names, 
                  SURFCLUST_MAX_SURF, 0);
   if ( SO_read != Opt->N_surf )
   {
	   if (SO_read >=0 )
         fprintf( SUMA_STDERR,
                  "Error %s:\nFound %d surfaces, expected %d.\n", 
                  FuncName,  SO_read, Opt->N_surf);
      exit(1);
   }
   /* now read into SUMAg_DOv */
   if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, 
                           Opt->sv_name, 0, SUMAg_CF->DsetList) ) {
	   fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
      exit(1);
   }
   
   if (!(SO = SUMA_find_named_SOp_inDOv(  Opt->surf_names[0], 
                                          SUMAg_DOv, SUMAg_N_DOv))) {
      SUMA_S_Errv("Failed to find surface %s\n", Opt->surf_names[0]);
      exit(1);                   
   }
   NodeArea = SUMA_CalculateNodeAreas(SO, NULL);
   if (!NodeArea) {
      SUMA_S_Err("Failed to calculate Node Areas.\n");
      exit(1);
   }   
   /* load the data */   
   iform = SUMA_NO_DSET_FORMAT;
   dset = SUMA_LoadDset_s (Opt->in_name, &iform, 0); 
   if (LocalHead) SUMA_ShowDset(dset, 0, NULL);
   if (!dset) { SUMA_S_Err(  "Failed to load dataset.\n"
                              "Make sure file exists\n"
                              "and is of the specified\n"
                              "format."); 
               exit(1); }
   if (!SUMA_OKassign(dset, SO)) {
      SUMA_SL_Err("Failed to assign data set to surface.");
      exit(1);
   }
   /* get the node index column */
   nip = SUMA_GetNodeDef(dset);
   N_ni = SDSET_VECLEN(dset);
   if (!nip) {
      SUMA_S_Err("Failed to find node index column");
      exit(1);
   }
   /* copy nip's contents because you will be modifying in the thresholding below */
   ni = (int *)SUMA_malloc(N_ni*sizeof(int));
   memcpy (ni, nip, N_ni*sizeof(int));
   nv = SUMA_DsetCol2Float(dset, Opt->labelcol, 0);
   if (!nv) {
      SUMA_S_Err("Failed to find node value column");
      exit(1);
   }
   
   /* any thresholding ? */
   if (Opt->DoThreshold) {
      nt = SUMA_DsetCol2Float(dset, Opt->tind, 0);
      if (!nt) {
         SUMA_S_Err("Failed to find threshold column");
         exit(1);
      }
      cnt = 0;
      if (Opt->DoThreshold == 1) {
         if (Opt->update) 
            fprintf( SUMA_STDERR,
                     "%s: Thresholding at %f...\n", FuncName, Opt->Thresh);
         for (i=0;i<N_ni; ++i) {
            if (nt[i] >= Opt->Thresh) {
               ni[cnt] = ni[i];
               nv[cnt] = nv[i];
               ++cnt;
            }
         }
      } else {
         SUMA_LH("ABS Thresholding...");
         for (i=0;i<N_ni; ++i) {
            if (fabs(nt[i]) >= Opt->Thresh) {
               ni[cnt] = ni[i];
               nv[cnt] = nv[i];
               ++cnt;
            }
         }
      }
      N_ni = cnt;
   }
   if (Opt->update) {
      Opt->update = -(N_ni * Opt->update / 100); /* make it negative 
                                                   before you begin a 
                                                   clustering operation */
      if (LocalHead) {
         fprintf( SUMA_STDERR,
                  "Update parameter, once every %d nodes\n"
                  "%d nodes to work with.\n", 
                  -(int)Opt->update, N_ni);
      }    
   }
   
   /* make the call */
   list = SUMA_FindClusters (SO, ni, nv, N_ni, -1, Opt, NodeArea);
   if (!list) {
      SUMA_S_Err("Failed in SUMA_FindClusters"); 
      exit(1);      
   }
   
   if (list->size) {
      /* sort the list */
      if (!SUMA_Sort_ClustersList (list, Opt->SortMode)) {
         SUMA_S_Err("Failed to sort cluster list");
         exit(1);
      }
   }       
   /* Show the results */
   params = SUMA_HistString(FuncName, argc, argv, NULL);
   if (Opt->WriteFile) {
      clustout = fopen(ClustOutName, "w");
      if (!clustout) {
         fprintf (SUMA_STDERR,
                  "Error %s:\n"
                  "Failed to open %s for writing.\n"
                  "Check permissions.\n",  
                  FuncName, ClustOutName);
         exit(1);
      }
      SUMA_Show_SurfClust_list(list, clustout, 0, params);
      fclose(clustout);clustout = NULL;  
   }  else SUMA_Show_SurfClust_list(list, NULL, 0, params);
   
   if (!list->size) {
      /* nothing left to do, quit */
      exit(0);
   }
   
   if (Opt->OutROI) {
      SUMA_DSET *dset_roi = NULL;
      char *ROIprefix = NULL;
      char *NameOut = NULL;
      if (Opt->DistLim >= 0.0) {
         if (Opt->AreaLim < 0) sprintf(stmp,"_ClstMsk_r%.1f", Opt->DistLim);
         else sprintf(stmp,"_ClstMsk_r%.1f_a%.1f", Opt->DistLim, Opt->AreaLim);
      } else {
         if (Opt->AreaLim < 0) sprintf(stmp,"_ClstMsk_e%d", -(int)Opt->DistLim);
         else sprintf(stmp,"_ClstMsk_e%d_a%.1f", 
                           -(int)Opt->DistLim, Opt->AreaLim);
      }
      ROIprefix = SUMA_append_string(Opt->out_prefix, stmp);
      /* Call this function, write out the resultant dset to disk 
         then cleanup */
      dset_roi = 
         SUMA_SurfClust_list_2_DsetMask(SO, list, Opt->FullROIList, ROIprefix);
      
      if (!dset_roi) {
         SUMA_S_Err("NULL dset_roi");
         exit(1);
      }
      if (Opt->prepend_node_index) {/* prepend node index? */         
         if (!SUMA_InsertDsetNelCol (
               dset_roi, "Node Index Copy", SUMA_NODE_INT, 
               (void *)(dset_roi->inel->vec[0]), NULL ,1, 0)) {
            SUMA_S_Err("Failed to insert column");
         }
         if (LocalHead) SUMA_ShowDset(dset_roi,0, NULL); 
      }
      
      NameOut = SUMA_WriteDset_s (  ROIprefix, dset_roi, Opt->oform, 
                                    THD_ok_overwrite(), 0);
      if (!NameOut) { SUMA_SL_Err("Failed to write dataset."); exit(1); } 
      SUMA_FreeDset((void *)dset_roi); dset_roi = NULL; 
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
      if (ROIprefix) SUMA_free(ROIprefix); ROIprefix = NULL; 
   }
   
   if (Opt->OutClustDset) {
      SUMA_DSET *dset_clust = NULL;
      char *Clustprefix = NULL;
      char *NameOut = NULL;
      if (Opt->DistLim >= 0.0) {
         if (Opt->AreaLim < 0) sprintf(stmp,"_Clustered_r%.1f", Opt->DistLim);
         else sprintf(stmp,"_Clustered_r%.1f_a%.1f", Opt->DistLim, Opt->AreaLim);
      } else {
         if (Opt->AreaLim < 0) 
            sprintf(stmp,"_Clustered_e%d", -(int)Opt->DistLim);
         else sprintf(stmp,"_Clustered_e%d_a%.1f", 
                           -(int)Opt->DistLim, Opt->AreaLim);
      }
      Clustprefix = SUMA_append_string(Opt->out_prefix, stmp);
      /* Call this function, write out the resultant dset to disk 
         then cleanup */
      
      dset_clust = 
         SUMA_MaskDsetByClustList(  dset, SO, list, 
                                    Opt->FullROIList, Clustprefix);
      if (!dset_clust) {
         SUMA_S_Err("NULL dset_clust");
         exit(1);
      }
      NameOut = SUMA_WriteDset_s (  Clustprefix, dset_clust, Opt->oform, 
                                    THD_ok_overwrite(), 0);
      if (!NameOut) { SUMA_SL_Err("Failed to write dataset."); exit(1); } 
      SUMA_FreeDset((void *)dset_clust); dset_clust = NULL; 
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
      if (Clustprefix) SUMA_free(Clustprefix); Clustprefix = NULL; 
   }
   
   if (!SUMA_FreeSpecFields(&Spec)) {
      SUMA_S_Err("Failed to free Spec fields");
   }
   if (ClustOutName) SUMA_free(ClustOutName); ClustOutName = NULL;
   if (list) dlist_destroy(list); SUMA_free(list); list = NULL;
   if (ni) SUMA_free(ni); ni = NULL;
   if (nv) SUMA_free(nv); nv = NULL;
   if (nt) SUMA_free(nt); nt = NULL;
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt) SUMA_free(Opt);
   if (dset) SUMA_FreeDset((void *)dset); dset = NULL;
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   exit(0);
}
#endif
