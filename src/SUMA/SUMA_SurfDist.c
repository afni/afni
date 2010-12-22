#include "SUMA_suma.h"

void usage_SurfDist (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfDist"};
      char * s = NULL, *sio=NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
"\n"
"Usage: SurfDist  [OPTIONS] <SURFACE> <NODEPAIRS>\n"
"       Output shortest distance between NODEPAIRS along\n"
"       the nesh of SURFACE, or the Euclidian distance.\n"
"\n"
"Mandatory options:\n"
"  <SURFACE> : Surface on which distances are computed.\n"
"              (For option's syntax, see \n"
"              'Specifying input surfaces' section below).\n"
"  <NODEPAIRS> : Specifying node pairs can be done in two ways\n"
"\n"
"     <FROM_TO_NODES>: A dataset of two columns where each row\n"
"               specifies a node pair.\n"
"               (For option's syntax, see \n"
"              'SUMA dataset input options' section below).\n"                   "   or\n"
"     <-from_node START>: Specify one starting node.\n"
"     <TO_NODES>: Specify one column of 'To' node indices.\n"
"                 Node pairs are between START and each node\n"
"                 in TO_NODES.\n"
"               (For option's syntax, see \n"
"              'SUMA dataset input options' section below).\n"                   "\n"
"Optional stuff:\n"
"  -node_path_do PATH_DO: Output the shortest path between\n"
"                         each node pair as a SUMA Displayable\n"
"                         object.\n"
"  -Euclidian: Calculate Euclidian distance, rather than graph distance.\n" 
"  -Graph: Calculate distance along the mesh (default).\n" 
"\n"
"  example 1:\n"
"     echo make a toy surface\n"
"     CreateIcosahedron\n"
"     echo Create some nodepairs\n"
"     echo 2 344 > nodelist.1D\n"
"     echo 416 489 >> nodelist.1D\n"
"     echo 415 412 >> nodelist.1D\n"
"     echo 123 32414 >> nodelist.1D\n"
"     echo Get distances and write out results in a 1D file\n"
"     SurfDist -i CreateIco_surf.asc \\\n"
"              -input nodelist.1D \\\n"
"              -node_path_do node_path   > example.1D\n"
"     echo 'The internode distances are in this file:'\n"
"     cat example.1D\n"
"     echo 'And you can visualize the paths this way:'\n"
"     suma -niml &\n"
"     DriveSuma -com show_surf -label ico \\\n"
"                       -i_fs CreateIco_surf.asc \\\n"
"               -com viewer_cont -load_do node_path.1D.do\n"
"\n"
"  example 2: (for tcsh)\n"
"     echo Say one has a filled ROI called: Area.niml.roi on \n"
"     echo a surface called lh.smoothwm.asc.\n"
"     set apref = Area\n"
"     set surf = lh.smoothwm.asc\n"
"     echo Create a dataset from this ROI with:\n"
"     ROI2dataset -prefix ${apref} -input ${apref}.niml.roi\n"
"     echo Get the nodes column forming the area\n"
"     ConvertDset -i ${apref}.niml.dset'[i]' -o_1D_stdout \\\n" 
"                              > ${apref}Nodes.1D \n"
"     echo Calculate distance from node 85329 to each of " 
"${apref}Nodes.1D\n" 
"     SurfDist  -from_node 85329 -input ${apref}Nodes.1D \\\n" 
"               -i ${surf}   > ${apref}Dists.1D\n"
"     echo Combine node indices and distances from node  85329\n"
"     1dcat ${apref}Nodes.1D ${apref}Dists.1D'[2]' \\\n"
"                                  > welt.1D.dset \n"
"     echo Now load welt.1D.dset and overlay on surface\n"
"     echo Distances are in the second column\n"
"     echo 'And you can visualize the distances this way:'\n"
"     suma -niml &\n"
"     sleep 4\n"
"     DriveSuma -com show_surf -label oke \\\n"
"                       -i_fs ${surf} \\\n"
"               -com  pause hit enter when surface is ready \\\n"
"               -com surf_cont -load_dset welt.1D.dset \\\n"
"                              -I_sb 1 -T_sb 1 -T_val 0.0 \n"
"\n"
"  example 3:\n"
"     echo make a toy surface\n"
"     CreateIcosahedron\n"
"     echo Create some nodepairs\n"
"     echo 2 344 > nodelist.1D\n"
"     echo 416 489 >> nodelist.1D\n"
"     echo 415 412 >> nodelist.1D\n"
"     echo 123 32414 >> nodelist.1D\n"
"     echo Get Euclidian distances and write out results to file\n"
"     SurfDist -i CreateIco_surf.asc \\\n"
"              -input nodelist.1D \\\n"
"              -Euclidian   > example3.1D\n"   
"%s"
"%s"
"\n", 
               ps->hverb ? sio:"Use -help for more detail.\n", 
               ps->hverb ? s:"");
      if (s) SUMA_free(s); s = NULL; 
      if (sio) SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_SurfDist_ParseInput(
   char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfDist_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct(); 
   Opt->ps = ps;  /* just hold it there for convenience */
   Opt->bases_prefix =NULL;
   Opt->iopt = -1;
   Opt->b1 = 0;
   Opt->b2 = 0;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 ps->hverb = 1;
          usage_SurfDist(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-for_Daniel") == 0))
      {
         Opt->b1 = 1;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-Euclidian") == 0))
      {
         Opt->b2 = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-graph") == 0))
      {
         Opt->b2 = 0;
         brk = YUP;
      }
      
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
      
      if (!brk && (strcmp(argv[kar], "-from_node") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need an integer after -from_node \n");
            exit (1);
         }
         
         Opt->iopt = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-node_path_do") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a name after -node_path_do \n");
            exit (1);
         }
         
         Opt->bases_prefix = argv[++kar];
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			SUMA_S_Errv("Option %s not understood.\n"
                     "Try -help for usage\n", 
                     argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
      
   }
   
   SUMA_RETURN(Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfDist"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_DSET_FORMAT iform = SUMA_NO_DSET_FORMAT;
   SUMA_DSET *din=NULL, *dout=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int ii, N_Spec, N_inmask = -1, jj, kk;
   int *nPath = NULL;
   int Nfrom=-1, Nto=-1, N_n=-1;
   float nDistance; 
   char *npout = NULL, *nout = NULL;
   SUMA_Boolean *dm=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   FILE *fpout = NULL, *fout = SUMA_STDOUT;
   float cout[4];
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-s;-sv;-spec;-dset;-mask;-cmap"); 

   
   if (argc < 2) {
      usage_SurfDist(ps);
      exit (1);
   }
   
   Opt = SUMA_SurfDist_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   if (Opt->b1) {
      /* The secret option for Daniel,
      See NIH-5 labbook pp 46 for graph */
      int N_Neighb_Max = 5; /* max number of nieghbors a node can have*/
      int N_Node = 7;
      int N_Neighb[7];
      int **FirstNeighb=NULL;
      float **FirstNeighbDist=NULL; 
      int N_np=3;
      int np[3][2];
      
      /* fill number of neighbors for nodes 0 to 6 */
      ii=0;
      N_Neighb[ii++] = 4;      
      N_Neighb[ii++] = 2;
      N_Neighb[ii++] = 3;
      N_Neighb[ii++] = 2;
      N_Neighb[ii++] = 2;
      N_Neighb[ii++] = 0;
      N_Neighb[ii++] = 1;
      
      /* fill neighborhood (edges) and distances */
      FirstNeighb = (int **)calloc(N_Node, sizeof(int*));
      FirstNeighbDist = (float **)calloc(N_Node, sizeof(float*));
      for (ii=0; ii<7;++ii) {
         FirstNeighb[ii] = (int *)calloc(N_Neighb_Max, sizeof(int)); 
         FirstNeighbDist[ii] = (float *)calloc(N_Neighb_Max, sizeof(float)); 
      }
      FirstNeighb[0][0]=2;    FirstNeighbDist[0][0]=1.0; /*1st neighb of node 0*/
      FirstNeighb[0][1]=1;    FirstNeighbDist[0][1]=1.0; /*2nd neighb of node 0*/
      FirstNeighb[0][2]=4;    FirstNeighbDist[0][2]=5.0; /*3rd neighb of node 0*/
      FirstNeighb[0][3]=6;    FirstNeighbDist[0][3]=2.0; /* ... */

      FirstNeighb[1][0]=0;    FirstNeighbDist[1][0]=1.0; /*1st neighb of node 1*/
      FirstNeighb[1][1]=2;    FirstNeighbDist[1][1]=2.0; /*2nd neighb of node 1*/
      
      FirstNeighb[2][0]=1;    FirstNeighbDist[2][0]=2.0; 
      FirstNeighb[2][1]=3;    FirstNeighbDist[2][1]=1.0;
      FirstNeighb[2][2]=0;    FirstNeighbDist[2][2]=1.0;
   
      FirstNeighb[3][0]=4;    FirstNeighbDist[3][0]=2.0;
      FirstNeighb[3][1]=2;    FirstNeighbDist[3][1]=1.0;
   
      FirstNeighb[4][0]=3;    FirstNeighbDist[4][0]=2.0;
      FirstNeighb[4][1]=0;    FirstNeighbDist[4][1]=5.0;
      
      FirstNeighb[5][0]=-1;   /* not necessary, but to emphasize */
      
      FirstNeighb[6][0]=0;    FirstNeighbDist[6][0]=2.0;
      
      if (Opt->bases_prefix) { /* pprepare to write out path */
         npout = SUMA_AfniPrefix(Opt->bases_prefix, NULL, NULL, NULL);
         npout = SUMA_append_replace_string(npout, ".1D.do", "", 1);
         if (!(fpout = fopen(npout,"w"))) {
            SUMA_S_Errv("Failed to open %s for writing.\n", npout);
            exit(1);
         }
         fprintf(fpout, "#node-based_segments\n");
      }
      
      /* Now get the shortest distance between some nodes pairs*/
      ii=0;
      np[ii][0] = 0; np[ii][1] = 4; ++ii;  /* from node 0 to node 4 */
      np[ii][0] = 6; np[ii][1] = 5; ++ii;  /* from node 6 to node 5 */
      np[ii][0] = 1; np[ii][1] = 2; ++ii;  /* from node 1 to node 2 */
      /* work  the node pairs */
      fprintf(fout, "#Internodal distance along graph \n");
      fprintf(fout, "#%-6s %-6s %-6s\n",
                    "From" , "to", "Dist." );
      for (ii=0; ii < N_np; ++ii) {
         if (!SUMA_a_good_col(Opt->ps->cmap, ii, cout)) {/*just to get a color*/
            SUMA_S_Errv("Failed to get color from map %s\n", Opt->ps->cmap);
            exit(1);
         }         
         if ( !(nPath = SUMA_Dijkstra_generic ( 
                           7, 
                           NULL, -1, 0,
                           N_Neighb, FirstNeighb, FirstNeighbDist,
                           np[ii][0], np[ii][1], 
                           NULL, NULL, 
                           1, 
                           &nDistance, &N_n, 0)) ) {
            nDistance = -1.0;
         } else {
            if (fpout) {
               for(kk=1; kk<N_n; ++kk) 
                  fprintf(fpout, 
                           "%d %d %.2f %.2f %.2f 1.0\n", 
                           nPath[kk-1], nPath[kk],  
                           cout[0], cout[1], cout[2]); 
               fprintf(fpout, "\n");
            }
            SUMA_free(nPath); nPath = NULL;
         }
      
         fprintf(fout, " %-6d %-6d %-4.2f\n", 
                       np[ii][0], np[ii][1], nDistance);
      }
      
      goto CLEANUP;
   }
   
   if (Opt->ps->N_dsetname != 1) {
      SUMA_S_Errv("Need one and only one dset please.\n"
                  "Have %d on command line.\n", 
                  Opt->ps->N_dsetname);
      exit(1);
   }
   if (!(din = SUMA_LoadDset_s (Opt->ps->dsetname[0], &iform, 0))) {
      SUMA_S_Errv("Failed to load dset named %s\n", Opt->ps->dsetname[0]);
      exit(1);
   }
   if (Opt->bases_prefix) {
      npout = SUMA_AfniPrefix(Opt->bases_prefix, NULL, NULL, NULL);
      npout = SUMA_append_replace_string(npout, ".1D.do", "", 1);
      if (!(fpout = fopen(npout,"w"))) {
         SUMA_S_Errv("Failed to open %s for writing.\n", npout);
         exit(1);
      }
      fprintf(fpout, "#node-based_segments\n");
   }
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }

   SUMA_LH("Loading surface...");
   SO = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], Opt->debug);
   if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      
   }   
   /* need some metrics */
   if (!SUMA_SurfaceMetrics_eng( SO, "EdgeList", NULL, 
                                 SUMA_MAX_PAIR(0, Opt->debug -1), 
                                 SUMAg_CF->DsetList)){
      SUMA_S_Err("Failed to metricate");
      exit(1);
   }
   if (!(Opt->nmask = SUMA_load_all_command_masks(
                        Opt->ps->bmaskname, Opt->ps->nmaskname, Opt->ps->cmask,
                        SO->N_Node, &N_inmask)) 
         && N_inmask < 0) {
         SUMA_S_Err("Failed loading mask");
         exit(1);
   }
   
   Nfrom = -1;
   if (SDSET_VECNUM(din) == 1) {
      if (Opt->iopt == -1 ) {
         SUMA_S_Err( "Must use -from_node START \n"
                     "if input dataset has one column\n");
         exit(1);
      } else if (Opt->iopt < 0 || Opt->iopt >= SO->N_Node) {
         SUMA_S_Errv("Bad node index. Must have 0<= START < %d\n"
                     "Have START = %d \n", Opt->iopt, SO->N_Node);
         exit(1);
      }
      Nfrom = Opt->iopt;
   }
   /* dset has to have one or two columns */
   if (SDSET_VECNUM(din)!= 2 && SDSET_VECNUM(din) != 1) {
      SUMA_S_Errv(
         "Expected one or two columns in input data from %s, have %d!\n",
         Opt->ps->dsetname[0], SDSET_VECNUM(din)); 
      exit(1);
   }
   /* work  the node pairs */
   if (!Opt->b2) {
      fprintf(fout, "#Internodal distance along graph of surface %s\n"
                        "#A distance of -1 indicates an error of sorts.\n", 
                        SO->Label);
      fprintf(fout, "#%-6s %-6s %-6s\n",
                           "From" , "to", "gDist." );
   } else {
      fprintf(fout, "#Internodal euclidian distance for surface %s\n"
                        "#A distance of -1 indicates an error of sorts.\n", 
                        SO->Label);
      fprintf(fout, "#%-6s %-6s %-6s\n",
                           "From" , "to", "eDist." );
   }  
   if (Opt->nmask) {
      if (!(dm = (SUMA_Boolean *)SUMA_malloc(sizeof(SUMA_Boolean) * 
                                             SO->N_Node))) {
         SUMA_S_Err("Failed to allocate");
         exit(1);
      }
   }
   for (ii=0; ii<SDSET_VECLEN(din); ++ii) {
      if (!SUMA_a_good_col(Opt->ps->cmap, ii, cout)) {
         SUMA_S_Errv("Failed to get color from map %s\n", Opt->ps->cmap);
         exit(1);
      }

      if (SDSET_VECNUM(din) == 2) {
         Nfrom = (int)SUMA_GetDsetValInCol2(din, 0, ii);
         Nto = (int)SUMA_GetDsetValInCol2(din, 1, ii);
      } else if (SDSET_VECNUM(din) == 1) {
         /* Nfrom setup above */
         Nto = (int)SUMA_GetDsetValInCol2(din, 0, ii);
      } else {
         SUMA_S_Err("What gives? Should not be here!\n");
         exit(1);
      }
      if (Opt->nmask) for(jj=0; jj<SO->N_Node; ++jj) dm[jj] = Opt->nmask[jj];
      
      /* You can replace SUMA_Dijkstra with SUMA_Dijkstra_usegen 
      and compare the results. They should be identical, of course */
      if (!Opt->b2) {
         if (  Nfrom < 0 || Nfrom >= SO->N_Node ||
               Nto   < 0 || Nto   >= SO->N_Node ||
               !(nPath = SUMA_Dijkstra ( SO, Nfrom, Nto, 
                                 dm, NULL, 1, 
                                 &nDistance, &N_n)) ) {
            nDistance = -1.0;
         } else {
            if (fpout) {
               for(kk=1; kk<N_n; ++kk) 
                  fprintf(fpout, 
                           "%d %d %.2f %.2f %.2f 1.0\n", 
                           nPath[kk-1], nPath[kk], 
                           cout[0], cout[1], cout[2]); 
               fprintf(fpout, "\n");
            }
            SUMA_free(nPath); nPath = NULL;
         }
      } else {
         if (Nfrom < 0 || Nfrom >= SO->N_Node ||
               Nto   < 0 || Nto   >= SO->N_Node) {
            nDistance = -1.0;
         } else {
            float *v1 = SO->NodeList+SO->NodeDim*Nfrom;
            float *v2 = SO->NodeList+SO->NodeDim*Nto;
            nDistance=0;
            for (kk=0; kk<SO->NodeDim; ++kk) {
               nDistance += SUMA_POW2(v1[kk]-v2[kk]);
            }
            nDistance = sqrt(nDistance); 
         }
      }
      fprintf(fout, " %-6d %-6d %-4.2f\n", 
                           Nfrom, Nto, nDistance
                           );
            
   }
   
   CLEANUP:
   if (npout) SUMA_free(npout); npout = NULL;
   if (nout) SUMA_free(nout); nout = NULL;
   if (dm) SUMA_free(dm); dm = NULL;
   if (fpout && fpout!= SUMA_STDERR && fpout != SUMA_STDOUT) 
      fclose(fpout); fpout = NULL;
   if (fout!= SUMA_STDERR && fout != SUMA_STDOUT) fclose(fout); fout = NULL;
   if (!SUMA_FreeSpecFields(Spec)) {
      SUMA_S_Err("Failed to free Spec fields");
   } SUMA_free(Spec); Spec = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
