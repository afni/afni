/*USE This sample to start writing standalone programs.
Change SurfToSurf to the program name of your choosing.
*/
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
      printf ( "\n"
               "Usage: SurfToSurf <-i_TYPE S1> [<-sv SV1>]\n"
               "                  <-i_TYPE S2> [<-sv SV1>]\n"
               "                  [<-prefix PREFIX>]\n"
               "                  [<-output_params .....>]\n" 
               " \n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
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
   Opt->NodeDbg = -1;
   Opt->debug = 0;
   Opt->NearestNode = 0;
   Opt->NearestTriangle = 0;
   Opt->DistanceToMesh = 0;
   Opt->ProjectionOnMesh = 0;
   Opt->Data = 0;
   Opt->in_name = NULL;
   Opt->out_prefix = NULL;
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
      
      if (!brk && accepting_out && (strcmp(argv[kar], "DistanceToMesh") == 0)) {
         Opt->DistanceToMesh = 1;
         brk = YUP;
      }
      
      if (!brk && accepting_out && (strcmp(argv[kar], "ProjectionOnMesh") == 0)) {
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
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
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
   float *far = NULL, *far_data=NULL, *dt = NULL;
   char *outname = NULL, *s=NULL, sbuf[100];
   void *SO_name = NULL;   
   FILE *outptr=NULL;
   SUMA_Boolean exists = NOPE;
   SUMA_INDEXING_ORDER d_order;
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;

	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;

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
         }
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
   M2M = SUMA_GetM2M_NN( SO1, SO2, nodeind, N_nodeind, NULL, 0, Opt->NodeDbg);
   
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
   if (Opt->NearestNode > 1) dt = SUMA_M2M_interpolate(M2M, far_data, ncol_data,  d_order, 0 );
   else if (Opt->NearestNode == 1) dt = SUMA_M2M_interpolate(M2M, far_data, ncol_data,  d_order, 1 );
   
   SUMA_LH("Forming the output");
   outptr = fopen(outname,"w");
   if (!outptr) {
      SUMA_SL_Err("Failed to open file for output.\n");
      exit(1);
   }
   
   /* first create the header of the output */
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend_va(SS, "#Mapping from nodes on mesh 1 (M1) to nodes on mesh 2 (M2)\n"
                                 "#  Mesh 1 is labeled %s, idcode:%s\n"
                                 "#  Mesh 2 is labeled %s, idcode:%s\n",
                                 SO1->Label, SO1->idcode_str, SO2->Label, SO2->idcode_str);
   icol = 0;
   SS = SUMA_StringAppend_va(SS, "#Col. %d:\n"
                                 "#     M1n (or nj): Index of node on M1\n"
                                 , icol); ++icol;
   if (Opt->NearestNode > 1) {
      SS = SUMA_StringAppend_va(SS, 
                                 "#Col. %d..%d:\n"
                                 "#     M2ne_M1n: Indices of %d nodes on M2 \n"
                                 "#     that are closest neighbors of nj.\n"
                                 "#     The first index is that of the node on M2 that is closest to nj\n" 
                                 , icol, icol+Opt->NearestNode-1, Opt->NearestNode); icol += Opt->NearestNode;
      SS = SUMA_StringAppend_va(SS, 
                                 "#Col. %d..%d:\n"
                                 "#     M2we_M1n: Weights assigned to nodes on mesh 2 (M2) \n"
                                 "#     that are closest neighbors of nj.\n"
                                 , icol, icol+Opt->NearestNode-1, Opt->NearestNode); icol += Opt->NearestNode;
   } else if (Opt->NearestNode == 1) {
      SS = SUMA_StringAppend_va(SS, 
                                 "#Col. %d:\n"
                                 "#     M2ne_M1n: Index of the node on M2 (label:%s idcode:%s)\n"
                                 "#     that is the closest neighbor of nj.\n"
                                 , icol, SO2->Label, SO2->idcode_str); ++icol;
   }
   if (Opt->NearestTriangle) { 
      SS = SUMA_StringAppend_va(SS, 
                                 "#Col. %d:\n"
                                 "#     M2t_M1n: Index of the triangle on M2 that hosts node nj on M1.\n"
                                 "#     In other words, nj's closest projection onto M2 falls on \n"
                                 "#     triangle M2t_M1n\n"
                                 , icol); ++icol; 
   }
   if (Opt->ProjectionOnMesh) { 
      SS = SUMA_StringAppend_va(SS, 
                                 "#Col. %d..%d:\n"
                                 "#     M2p_M1n: Coordinates of projection of nj onto M2\n"
                                 , icol, icol+2); icol += 3; 
   }
   if (Opt->DistanceToMesh) {
      SS = SUMA_StringAppend_va(SS, 
                                 "#Col. %d:\n"
                                 "#     Closest distance from nj to M2\n"
                                 , icol); ++icol;
   }
   if (!Opt->in_name) {
      SS = SUMA_StringAppend_va(SS, 
                                 "#Col. %d..%d:\n"
                                 "#     Interpolation using XYZ coordinates of nodes on M2 that neighbor nj\n"
                                 , icol, icol+2); icol += 3; 
   } else {
      SS = SUMA_StringAppend_va(SS, 
                                 "#Col. %d..%d:\n"
                                 "#     Interpolation of data at nodes on M2 that neighbor nj\n"
                                 "#     Data obtained from %s\n"
                                 , icol, icol+ncol_data-1, Opt->in_name);  icol += ncol_data;
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
         for (j=0; j<Opt->NearestNode; ++j) { fprintf(outptr,"%6s   ", MV_format_fval2(M2M->M2ne_M1n[i][j], Nchar)); } /* Neighboring nodes */
      } 
      if (Opt->NearestNode > 1) { /* add the weights */
         for (j=0; j<Opt->NearestNode; ++j) { 
            fprintf(outptr,"%6s   ", MV_format_fval2(M2M->M2we_M1n[i][j], Nchar)); 
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
      if (!Opt->in_name) {
         fprintf(outptr,"%6s   ", MV_format_fval2(dt[3*i], Nchar));
         fprintf(outptr,"%6s   ", MV_format_fval2(dt[3*i+1], Nchar));
         fprintf(outptr,"%6s   ", MV_format_fval2(dt[3*i+2], Nchar));
      } else {
         for (j=0; j<ncol_data; ++j) { fprintf(outptr,"%6s   ", MV_format_fval2(dt[i+j*ncol_data], Nchar)); }
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
