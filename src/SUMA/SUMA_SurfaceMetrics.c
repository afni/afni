#include "SUMA_suma.h"


/*#define  DO_SCALE_RANGE   *//*!< scale node coordinates to 0 <--> 100. DO NOT USE IT, OBSOLETE*/
#ifndef DO_SCALE_RANGE
   #define DO_SCALE 319.7   /*!< scale node coordinates by specified factor. Useful for tesscon coordinate system in iv files*/
#endif

SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */

void usage_SUMA_SurfaceMetrics ()
   {
      static char FuncName[]={"usage_SUMA_SurfaceMetrics"};
      char * s = NULL;
      s = SUMA_help_basics();
      printf ( "\n"
               "Usage: SurfaceMetrics <-Metric1> [[-Metric2] ...] \n"
               "                  <-spec SpecFile> <-surf_A insurf> \n"
               "                  [<-sv SurfaceVolume [VolParam for sf surfaces]>]\n"
               "                  [-tlrc] [<-prefix prefix>]\n"
               "\n"
               "Outputs information about a surface's mesh\n"
               "\n"
               "   -Metric1: Replace -Metric1 with the following:\n"
               "      -vol: calculates the volume of a surface.\n"
               "            Volume unit is the cube of your surface's\n"
               "            coordinates unit, obviously.\n"
               "            Volume's sign depends on the orientation\n"
               "            of the surface's mesh.\n" 
               "            Make sure your surface is a closed one\n"
               "            and that winding is consistent.\n"
               "            Use SurfQual to check the surface.\n"
               "            If your surface's mesh has problems,\n"
               "            the result is incorrect. \n"
               "            Volume is calculated using Gauss's theorem,\n"
               "            see [Hughes, S.W. et al. 'Application of a new \n"
               "            discreet form of Gauss's theorem for measuring \n"
               "            volume' in Phys. Med. Biol. 1996].\n"
               "      -conv: output surface convexity at each node.\n"
               "         Output file is prefix.conv. Results in two columns:\n"
               "         Col.0: Node Index\n"
               "         Col.1: Convexity\n"
               "         This is the measure used to shade sulci and gyri in SUMA.\n"
               "         C[i] = Sum(dj/dij) over all neighbors j of i\n"
               "         dj is the distance of neighboring node j to the tangent plane at i\n"
               "         dij is the length of the segment ij\n"
               "      -closest_node XYZ_LIST.1D: Find the closest node on the surface\n"
               "                              to each XYZ triplet in XYZ_LIST.1D\n"
               "                              Note that it is assumed that the XYZ\n"
               "                              coordinates are in RAI (DICOM) per AFNI's\n"
               "                              coordinate convention. For correspondence\n"
               "                              with coordinates observed in SUMA and AFNI\n"
               "                              be sure to use the proper -sv parameter for\n"
               "                              the surface and XYZ coordinates in question.\n"
               "         Output file is prefix.closest.1D. Results in 8 columns:\n"
               "         Col.0: Index of closest node.\n"
               "         Col.1: Distance of closest node to XYZ reference point.\n"
               "         Col.2..4: XYZ of reference point (same as XYZ_LIST.1D, copied \n"
               "                   here for clarity).\n"
               "         Col.5..7: XYZ of closest node (after proper surface coordinate\n"
               "                   transformation, including SurfaceVolume transform.\n"
               "      -area: output area of each triangle. \n"
               "         Output file is prefix.area. Results in two columns:\n"
               "         Col.0: Triangle Index\n"
               "         Col.1: Triangle Area\n"
               "      -curv: output curvature at each node.\n"
               "         Output file is prefix.curv. Results in nine columns:\n"
               "         Col.0: Node Index\n"
               "         Col.1-3: vector of 1st principal direction of surface\n"
               "         Col.4-6: vector of 2nd principal direction of surface\n"
               "         Col.7: Curvature along T1\n"
               "         Col.8: Curvature along T2\n"
               "         Curvature algorithm by G. Taubin from: \n"
               "         'Estimating the tensor of curvature of surface \n"
               "         from a polyhedral approximation.'\n"
               "      -edges: outputs info on each edge. \n"
               "         Output file is prefix.edges. Results in five columns:\n"
               "         Col.0: Edge Index (into a SUMA structure).\n"
               "         Col.1: Index of the first node forming the edge\n"
               "         Col.2: Index of the second node forming the edge\n"
               "         Col.3: Number of triangles containing edge\n"
               "         Col.4: Length of edge.\n"
               "      -node_normals: Outputs segments along node normals.\n"
               "                     Segments begin at node and have a default\n"
               "                     magnitude of 1. See option 'Alt+Ctrl+s' in \n"
               "                     SUMA for visualization.\n"
               "      -face_normals: Outputs segments along triangle normals.\n"
               "                     Segments begin at centroid of triangles and \n"
               "                     have a default magnitude of 1. See option \n"
               "                     'Alt+Ctrl+s' in SUMA for visualization.\n"
               "      -normals_scale SCALE: Scale the normals by SCALE (1.0 default)\n"
               "                     For use with options -node_normals and -face_normals\n"
               "      -coords: Output coords of each node after any transformation \n"
               "         that is normally carried out by SUMA on such a surface.\n"
               "         Col. 0: Node Index\n"
               "         Col. 1: X\n"
               "         Col. 2: Y\n"
               "         Col. 3: Z\n"     
               "      -sph_coords: Output spherical coords of each node.\n"
               "      -sph_coords_center x y z: Shift each node by  x y z\n"
               "                                before calculating spherical\n"
               "                                coordinates. Default is the\n"
               "                                center of the surface.\n"
               "          Both sph_coords options output the following:\n"
               "          Col. 0: Node Index\n"
               "          Col. 1: R (radius)\n"
               "          Col. 2: T (azimuth)\n"
               "          Col. 3: P (elevation)\n"
               "      -boundary_nodes: Output nodes that form a boundary of a surface\n"
               "                   i.e. they form edges that belong to one and only\n"
               "                   one triangle.\n"
               "      -internal_nodes: Output nodes that are not a boundary.\n"
               "                   i.e. they form edges that belong to more than\n"
               "                   one triangle.\n"
               "\n"
               "      You can use any or all of these metrics simultaneously.\n"
               "\n"
               "   -spec SpecFile: Name of specfile containing surface of interest.\n"
               "                   If the surface does not have a spec file, use the \n"
               "                   program quickspec to create one.\n"
               "   -surf_A insurf: Name of surface of interest. \n"
               "                   NOTE: i_TYPE inSurf option is now obsolete.\n"
               "\n"
               "   -sv SurfaceVolume [VolParam for sf surfaces]: Specify a surface volume\n"
               "                   for surface alignment. See ConvertSurface -help for more info.\n"
               "\n"
               "   -tlrc: Apply Talairach transform to surface.\n"
               "                   See ConvertSurface -help for more info.\n"
               "\n"
               "   -prefix prefix: Use prefix for output files. (default is prefix of inSurf)\n"
               "%s"
               "\n", s);
      SUMA_free(s); s = NULL;
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf ( "       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \n"
               "       Mon May 19 15:41:12 EDT 2003\n"
               "\n");   
   }
#define SURFACEMETRICS_MAX_SURF 10

int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"Main_SUMA_SurfaceMetrics"};
   char  *OutName=NULL, *OutPrefix = NULL, *if_name = NULL, 
         *if_name2 = NULL, *sv_name = NULL, *vp_name = NULL,
         *tlrc_name = NULL;
   float *Cx = NULL, sph_center[3], NormScale;
   SUMA_STRING *MetricList = NULL;
   int i, n1, n2, n1_3, n2_3, kar, nt, SO_read;
   double edgeL2;
   FILE *fout=NULL;
   SUMA_SO_File_Type iType = SUMA_FT_NOT_SPECIFIED;
   SUMA_SurfaceObject *SO = NULL;   
   SUMA_SFname *SF_name = NULL;
   void *SO_name = NULL;   
   SUMA_SurfSpecFile Spec;
   THD_warp *warp=NULL ;
   THD_3dim_dataset *aset=NULL;
   char *surf_names[SURFACEMETRICS_MAX_SURF];
   char *spec_file, *histnote;
   char *closest_to_xyz = NULL;
   int insurf_method = 0, N_surf = 0, ind = 0;
   SUMA_Boolean   brk, Do_tlrc, Do_conv, Do_curv, 
                  Do_area, Do_edges, Do_vol, Do_sph, NewCent, Do_cord, Do_TriNorm, 
                  Do_NodeNorm, Do_en, Do_in, LocalHead = NOPE;  
   
   SUMA_mainENTRY;
   
	SUMA_STANDALONE_INIT;
   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);

   if (argc < 4)
       {
          SUMA_S_Err("Too few parameters");
          usage_SUMA_SurfaceMetrics ();
          exit (1);
       }
   
   MetricList = SUMA_StringAppend (NULL, NULL);
   kar = 1;
	brk = NOPE;
   Do_cord = NOPE;
   Do_sph = NOPE;
   Do_vol = NOPE;
   Do_tlrc = NOPE;
   Do_conv = NOPE;
   Do_area = NOPE;
   Do_curv = NOPE;
   Do_edges = NOPE;
   Do_TriNorm = NOPE;
   Do_NodeNorm = NOPE;
   Do_en = NOPE;
   Do_in = NOPE;
   closest_to_xyz = NULL;
   NormScale = 5.0;
   NewCent = NOPE;
   OutPrefix = NULL;
   for (i=0; i<SURFACEMETRICS_MAX_SURF; ++i) { surf_names[i] = NULL; }   
   spec_file = NULL;

	while (kar < argc) { /* loop accross command ine options */
		/* fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName); */
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_SurfaceMetrics();
          exit (1);
		}
		
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
		if (!brk && (strcmp(argv[kar], "-i_fs") == 0)) {
         SUMA_SL_Err("Option -i_fs is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_fs ");
				exit (1);
			}
			if_name = argv[kar];
         iType = SUMA_FREE_SURFER;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_sf") == 0)) {
         SUMA_SL_Err("Option -i_sf is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_sf");
				exit (1);
			}
			if_name = argv[kar]; kar ++;
         if_name2 = argv[kar];
         iType = SUMA_SUREFIT;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_vec") == 0)) {
         SUMA_SL_Err("Option -i_vec is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_vec");
				exit (1);
			}
			if_name = argv[kar]; kar ++;
         if_name2 = argv[kar];
         iType = SUMA_VEC;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_ply") == 0)) {
         SUMA_SL_Err("Option -i_ply is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_ply ");
				exit (1);
			}
			if_name = argv[kar];
         iType = SUMA_PLY;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec \n");
				exit (1);
			}
			spec_file = argv[kar];
         if (!insurf_method) insurf_method = 2;
         else {
            fprintf (SUMA_STDERR, "already specified spec file.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strncmp(argv[kar], "-surf_", 6) == 0)) {
			if (kar + 1>= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -surf_X SURF_NAME \n");
				exit (1);
			}
			ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFACEMETRICS_MAX_SURF) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option is out of range,\n"
                                  "   only surf_A allowed.\n");
				exit (1);
         }
         kar ++;
         surf_names[ind] = argv[kar];
         N_surf = ind+1;
			if (insurf_method != 2) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option must be used with -spec option.\n");
            exit(1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-sph_coords_center") == 0)) {
         kar ++;
			if (kar+2 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 3 arguments after -sph_coords_center \n");
				exit (1);
			}
			sph_center[0] = atof(argv[kar]); kar ++;
         sph_center[1] = atof(argv[kar]); kar ++;
         sph_center[2] = atof(argv[kar]);
         NewCent = YUP;
         Do_sph = YUP;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-sph_coords") == 0)) {
         Do_sph = YUP;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-coords") == 0)) {
         Do_cord = YUP;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix ");
				exit (1);
			}
			OutPrefix = SUMA_copy_string(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-sv") == 0)) {
         if (0 && iType == SUMA_FT_NOT_SPECIFIED) {/* iType input no longer allowed */
            fprintf (SUMA_STDERR, " -sv option must be preceeded by -i_TYPE option.");
            exit(1);
         }
         kar ++;
			if (iType == SUMA_SUREFIT) {
            if (kar+1 >= argc)  {
		  		   fprintf (SUMA_STDERR, "need 2 argument after -sv (SurfaceVolume and VolumeParent)");
				   exit (1);
			   }
            sv_name = argv[kar]; kar ++;
            vp_name = argv[kar];
         } else {
            if (kar >= argc)  {
		  		   fprintf (SUMA_STDERR, "need argument after -sv ");
				   exit (1);
			   }
			   sv_name = argv[kar];
         }
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-tlrc") == 0)) {
         Do_tlrc = YUP;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-node_normals") == 0)) {
         Do_NodeNorm = YUP;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-face_normals") == 0)) {
         Do_TriNorm = YUP;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-normals_scale") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -normals_scale ");
				exit (1);
			}
         NormScale = atof(argv[kar]);
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-closest_node") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -closest_node ");
				exit (1);
			}
         closest_to_xyz = argv[kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-conv") == 0)) {
         Do_conv = YUP;
         MetricList = SUMA_StringAppend (MetricList, "Convexity "); 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-area") == 0)) {
         Do_area = YUP;
         MetricList = SUMA_StringAppend (MetricList, "PolyArea "); 
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-curv") == 0)) {
         Do_curv = YUP;
         MetricList = SUMA_StringAppend (MetricList, "Curvature "); 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-edges") == 0)) {
         Do_edges = YUP;
         MetricList = SUMA_StringAppend (MetricList, "EdgeList "); 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-vol") == 0)) {
         Do_vol = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-boundary_nodes") == 0)) {
         Do_en = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-internal_nodes") == 0)) {
         Do_in = YUP;
         brk = YUP;
      }
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   if (N_surf < 1) {
      SUMA_SL_Err("No surface specified.");
      exit(1);
   }

   /* clean MetricList */
   MetricList = SUMA_StringAppend (MetricList, NULL); 
   
   /* sanity checks */
   if (!strlen(MetricList->s) && !Do_vol && !Do_sph && !Do_cord && !Do_TriNorm && !Do_NodeNorm && !Do_en && !Do_in && !closest_to_xyz) {
      SUMA_S_Err("No Metrics specified.\nNothing to do.\n");
      exit(1);
   }
   
   if (0 && sv_name) { /* stupid check for volumes... */
      if (!SUMA_filexists(sv_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, sv_name);
         exit(1);
      }
   }
   
   if (Do_tlrc && !sv_name) {
      fprintf (SUMA_STDERR,"Error %s: -tlrc must be used with -sv option.\n", FuncName);
      exit(1);
   }
   
   if (vp_name) {
      if (!SUMA_filexists(vp_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, vp_name);
         exit(1);
      }
   }

   
   /* read all surfaces */
   if (!SUMA_AllocSpecFields(&Spec)) { SUMA_S_Err("Error initing"); exit(1); }
   if (!SUMA_Read_SpecFile (spec_file, &Spec)) {
		fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
		exit(1);
	}
   SO_read = SUMA_spec_select_surfs(&Spec, surf_names, SURFACEMETRICS_MAX_SURF, 0);
   if ( SO_read != N_surf )
   {
	   if (SO_read >=0 )
         fprintf(SUMA_STDERR,"Error %s:\nFound %d surfaces, expected %d.\n", FuncName,  SO_read, N_surf);
      exit(1);
   }
   
   /* now read into SUMAg_DOv */
   if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, sv_name, 0, SUMAg_CF->DsetList) ) {
	   fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
      exit(1);
   }   SO = SUMA_find_named_SOp_inDOv(surf_names[0], SUMAg_DOv, SUMAg_N_DOv);
   if (!SO) {
      fprintf (SUMA_STDERR,"Error %s: Failed to read input surface.\n", FuncName);
      exit (1);
   }
   
   if (Do_tlrc) {
      fprintf (SUMA_STDOUT,"Performing talairach transform...\n");

      /* form the tlrc version of the surface volume */
      tlrc_name = (char *) SUMA_calloc (strlen(SO->VolPar->dirname)+strlen(SO->VolPar->prefix)+60, sizeof(char));
      sprintf (tlrc_name, "%s%s+tlrc.HEAD", SO->VolPar->dirname, SO->VolPar->prefix);
      if (!SUMA_filexists(tlrc_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, tlrc_name);
         exit(1);
      }
      
      /* read the tlrc header */
      aset = THD_open_dataset(tlrc_name) ;
      if( !ISVALID_DSET(aset) ){
         fprintf (SUMA_STDERR,"Error %s: %s is not a valid data set.\n", FuncName, tlrc_name) ;
         exit(1);
      }
      if( aset->warp == NULL ){
         fprintf (SUMA_STDERR,"Error %s: tlrc_name does not contain a talairach transform.\n", FuncName);
         exit(1);
      }
      
      warp = aset->warp ;
      
      /* now warp the coordinates, one node at a time */
      if (!SUMA_AFNI_forward_warp_xyz(warp, SO->NodeList, SO->N_Node)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AFNI_forward_warp_xyz.\n", FuncName);
         exit(1);
      }

      
      
   }
   
   /* check on normals */
   if ((Do_TriNorm || Do_NodeNorm) && (!SO->NodeNormList || !SO->FaceNormList)) {
      SUMA_RECOMPUTE_NORMALS(SO);
   }
   
   /* create the surface label*/
   SO->Label = SUMA_SurfaceFileName (SO, NOPE);
   if (!SO->Label) {
      SUMA_S_Err("Failed to create Label");
      exit(1);
   }

   if (LocalHead) SUMA_Print_Surface_Object (SO, stderr);
   
   /* Now do the deed */
   SUMA_LH (MetricList->s);
   if (strlen(MetricList->s)) {
      if (!SUMA_SurfaceMetrics (SO, MetricList->s, NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         exit(1);
      }
   }

   SUMA_LH ("Done with Metrics");
   
   /* output time */
   if (!OutPrefix) {
      OutPrefix = SUMA_copy_string(SO->Label);
   }
   
   OutName = (char*) SUMA_malloc((strlen(OutPrefix) + 30) * sizeof(char));
   histnote = SUMA_HistString (NULL, argc, argv, NULL);
   
   if (Do_sph) {
      float *sph=NULL;
      sprintf(OutName, "%s.sphcoord.1D.dset", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Edge output file exists.\nWill not overwrite.");
         exit(1);
      }
      if (NewCent) ;
      else if (SO->Center) { SUMA_COPY_VEC(SO->Center, sph_center, 3, float, float); }
      else {
         SUMA_SL_Err("SO has no center");
         exit(1);
      }
      sph = SUMA_Cart2Sph(SO->NodeList, SO->N_Node, sph_center);

      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Spherical coords, \n");
      fprintf (fout,"#  cartesian coords shifted by [%f %f %f] prior to xform\n", sph_center[0], sph_center[1], sph_center[2]);
      fprintf (fout,"#nI = Node Index\n");
      fprintf (fout,"#r  = Rho (radius)\n");
      fprintf (fout,"#t  = theta(azimuth)\n");
      fprintf (fout,"#p  = phi(elevation)\n");
      fprintf (fout,"#nI\tr\tt\tp\n\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i < SO->N_Node; ++i) {
            fprintf (fout,"%d\t%f\t%f\t%f\n",
                  i, sph[3*i], sph[3*i+1],sph[3*i+2]);
      }
      
      fclose(fout); fout = NULL;
      
      if (sph) SUMA_free(sph); sph = NULL;
   }  
   
   if (Do_NodeNorm) {
      float norm[3];
      sprintf(OutName, "%s.NodeNormSeg.1D", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Node normals output file exists.\nWill not overwrite.");
         exit(1);
      }
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Node normals.\n");
      fprintf (fout,"#  Segments from node along the direction of the normal (of magnitude %f)\n", NormScale);
      fprintf (fout,"#  1st three columns are node's coordinates\n");
      fprintf (fout,"#  2nd three columns are the coordinate of a second point along the normal\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i<SO->N_Node; ++i) {
         norm[0] = SO->NodeNormList[3*i]; norm[1] = SO->NodeNormList[3*i+1]; norm[2] = SO->NodeNormList[3*i+2];
         /* normal is expected to be normalized...*/
         norm[0] *= NormScale; norm[1] *= NormScale; norm[2] *= NormScale;
         fprintf (fout,"%f %f %f \t%f %f %f\n", 
            SO->NodeList[3*i], SO->NodeList[3*i+1], SO->NodeList[3*i+2], 
            SO->NodeList[3*i]+norm[0], SO->NodeList[3*i+1]+norm[1], SO->NodeList[3*i+2]+norm[2]);            
      }
      
      fclose(fout); fout = NULL;
      
   }
   
   if (Do_TriNorm) {
      float tc[3], norm[3];
      int n1, n2, n3;
      if (SO->FaceSetDim != 3) {
         SUMA_S_Err("Triangular meshes only please.");
         exit(1);
      }
      sprintf(OutName, "%s.TriNormSeg.1D", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Triangle normals output file exists.\nWill not overwrite.");
         exit(1);
      }
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Triangle normals.\n");
      fprintf (fout,"#  Segments from centroid of triangle along the direction of the normal (of magnitude %f)\n", NormScale);
      fprintf (fout,"#  1st three columns are centroid's coordinates\n");
      fprintf (fout,"#  2nd three columns are the coordinate of a second point along the normal\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i<SO->N_FaceSet; ++i) {
         n1 = SO->FaceSetList[3*i]; n2 = SO->FaceSetList[3*i+1]; n3 = SO->FaceSetList[3*i+2];
         /* coordinate of centroid */
         tc[0] = (SO->NodeList[3*n1]   + SO->NodeList[3*n2]   + SO->NodeList[3*n3]  )/3; /* centroid of triangle */
         tc[1] = (SO->NodeList[3*n1+1] + SO->NodeList[3*n2+1] + SO->NodeList[3*n3+1])/3; 
         tc[2] = (SO->NodeList[3*n1+2] + SO->NodeList[3*n2+2] + SO->NodeList[3*n3+2])/3;
         norm[0] = SO->FaceNormList[3*i]; norm[1] = SO->FaceNormList[3*i+1]; norm[2] = SO->FaceNormList[3*i+2];
         /* normal is expected to be normalized...*/
         norm[0] *= NormScale; norm[1] *= NormScale; norm[2] *= NormScale;
         fprintf (fout,"%f %f %f \t%f %f %f\n", tc[0], tc[1], tc[2], tc[0]+norm[0], tc[1]+norm[1], tc[2]+norm[2]);            
      }
      
      fclose(fout); fout = NULL;
   }
   
   if (Do_cord) {
      sprintf(OutName, "%s.coord.1D.dset", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Edge output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Cartesian coords, \n");
      fprintf (fout,"#  Center is: [%f %f %f] \n", SO->Center[0], SO->Center[1], SO->Center[2]);
      fprintf (fout,"#nI = Node Index\n");
      fprintf (fout,"#x  = X \n");
      fprintf (fout,"#y  = Y\n");
      fprintf (fout,"#z  = Z\n");
      fprintf (fout,"#nI\tx\ty\tz\n\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i < SO->N_Node; ++i) {
            fprintf (fout,"%d\t%f\t%f\t%f\t\n",
                  i, SO->NodeList[3*i], SO->NodeList[3*i+1],SO->NodeList[3*i+2]);
      }
      
      fclose(fout); fout = NULL;
   }  
   
   if (Do_edges) {
      
      SUMA_S_Note("Writing edges...");
      
      if (!SO->EL) {
         SUMA_S_Err("Edge list not computed.");
         exit(1);
      }
      
      sprintf(OutName, "%s.edges", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Edge output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Edge List\n");
      fprintf (fout,"#eI = Edge Index\n");
      fprintf (fout,"#n1 = Node 1\n");
      fprintf (fout,"#n2 = Node 2\n");
      fprintf (fout,"#nt = Number of triangles containing edge\n"); 
      fprintf (fout,"#eL = Edge Length\n");
      fprintf (fout,"#eI\tn1\tn2\tnt\teL\n\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i < SO->EL->N_EL; ++i) {
         if (SO->EL->ELps[i][2] >= 0) {
            n1 = SO->EL->EL[i][0];
            n2 = SO->EL->EL[i][1];
            nt = SO->EL->ELps[i][2];
            n1_3 = 3 * n1;
            n2_3 = 3 * n2;
            edgeL2 = ( (SO->NodeList[n2_3] - SO->NodeList[n1_3]) * (SO->NodeList[n2_3] - SO->NodeList[n1_3]) ) +
                     ( (SO->NodeList[n2_3+1] - SO->NodeList[n1_3+1]) * (SO->NodeList[n2_3+1] - SO->NodeList[n1_3+1]) ) +
                     ( (SO->NodeList[n2_3+2] - SO->NodeList[n1_3+2]) * (SO->NodeList[n2_3+2] - SO->NodeList[n1_3+2]) ); 
                     
            fprintf (fout,"%d\t%d\t%d\t%d\t%f\n",
                  i, n1, n2, nt, sqrt(edgeL2));
                  
         }   
      }
      fclose(fout); fout = NULL;
      
   }
   
   if (Do_area) {
      SUMA_S_Note("Writing areas...");
      
      if (!SO->PolyArea) {
         SUMA_S_Err("Areas not computed");
         exit(1);
      }  
      
      sprintf(OutName, "%s.area", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Area output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#FaceSet Area\n");
      fprintf (fout,"#fI = FaceSet Index\n");
      fprintf (fout,"#fA = FaceSet Area\n");
      fprintf (fout,"#fI\t#fA\n\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      for (i=0; i < SO->N_FaceSet; ++i) {
         fprintf (fout,"%d\t%f\n", i, SO->PolyArea[i]);
      }  
      
      fclose(fout); fout = NULL;
   }
   
   if (Do_curv) {
      SUMA_S_Note("Writing curvatures ...");
      
      if (!SO->SC) {
         SUMA_S_Err("Curvatures not computed");
         exit(1);
      }
      
      sprintf(OutName, "%s.curv.1D.dset", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Curvature output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }  
      
      fprintf (fout,"#Curvature\n");
      fprintf (fout,"#nI = Node Index\n");
      fprintf (fout,"#T1 = 1 x 3 vector of 1st principal direction of surface\n");
      fprintf (fout,"#T2 = 1 x 3 vector of 2nd principal direction of surface\n");
      fprintf (fout,"#Kp1 = curvature along T1\n");
      fprintf (fout,"#Kp2 = curvature along T2\n");
      fprintf (fout,"#nI\tT1[0]\tT1[1]\tT1[2]\tT2[0]\tT2[1]\tT2[2]\tKp1\tKp2\n\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i < SO->N_Node; ++i) {
         fprintf (fout,"%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n",
            i, SO->SC->T1[i][0], SO->SC->T1[i][1], SO->SC->T1[i][2], 
            SO->SC->T2[i][0], SO->SC->T2[i][1], SO->SC->T2[i][2],
            SO->SC->Kp1[i], SO->SC->Kp2[i] );
      }
      
      fclose(fout); fout = NULL;
   }
   
   if (Do_conv) {
      SUMA_S_Note("Writing convexities ...");
      Cx = (float *)SUMA_GetCx(SO->idcode_str, SUMAg_CF->DsetList, 0);
      if (!Cx) {
         SUMA_S_Err("Convexities not computed");
         exit(1);
      }
      
      sprintf(OutName, "%s.conv.1D.dset", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Convexities output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }  
      
      fprintf (fout,"#Convexity\n");
      fprintf (fout,"#nI = Node Index\n");
      fprintf (fout,"#C = Convexity\n");
      fprintf (fout,"#nI\tC\n\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i < SO->N_Node; ++i) {
         fprintf (fout,"%d\t%f\n", i, Cx[i]);
      }
      
      fclose(fout); fout = NULL;
   }   
   
   if (Do_vol) {
      float vol;
      fprintf (SUMA_STDOUT,"Calculating surface volume...\n");
      vol = SUMA_Mesh_Volume(SO, NULL, -1);
      fprintf (SUMA_STDERR,   "Volume of closed surface is %f (units3).\n"
                              "Signed volume is  %f (units3).\n", fabs(vol), vol); 
   }
   
   if (Do_en || Do_in) {
      byte *enmask = NULL;
      if (Do_en) fprintf (SUMA_STDOUT,"finding boundary nodes...\n");
      else fprintf (SUMA_STDOUT,"finding internal nodes...\n");
      enmask = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte));  
      if (!enmask) {
         SUMA_S_Crit("Failed to allocate.");
         exit(1);
      }
      if (!SO->EL) {
         SUMA_S_Err("Unexpected error, no edge list.");
         exit(1);
      }
      /* first find the boundary nodes */
      i=0;
      while(i<SO->EL->N_EL) {
         /* find edges that form boundaries */
         if (SO->EL->ELps[i][2] == 1) {
            enmask[SO->EL->EL[i][0]] = 1;
            enmask[SO->EL->EL[i][1]] = 1; 
         }
         ++i;
      }
      
      if (Do_en) {
         sprintf(OutName, "%s.boundarynodes.1D.dset", OutPrefix);
         if (SUMA_filexists(OutName)) {
            SUMA_S_Err("Boundarynodes output file exists.\nWill not overwrite.");
            exit(1);
         }
      } else {
         /* actually need internal ones */
         i=0;
         while(i<SO->EL->N_EL) {
            /* find edges that are internal, NOT part of boundary already */
            if (SO->EL->ELps[i][2] > 1) {
               if (!enmask[SO->EL->EL[i][0]]) enmask[SO->EL->EL[i][0]] = 2;
               if (!enmask[SO->EL->EL[i][1]]) enmask[SO->EL->EL[i][1]] = 2; 
            }
            ++i;
         }
         sprintf(OutName, "%s.internalnodes.1D.dset", OutPrefix);
         if (SUMA_filexists(OutName)) {
            SUMA_S_Err("Internalnodes output file exists.\nWill not overwrite.");
            exit(1);
         }
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }  
      
      if (Do_en) {
         fprintf (fout,"#Boundary Nodes\n");
      } else {
         fprintf (fout,"#Internal Nodes\n");
      }
      fprintf (fout,"#nI = Node Index\n");
      fprintf (fout,"#nI\n\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      if (Do_en) {
         for (i=0; i < SO->N_Node; ++i) {
            if (enmask[i] == 1) fprintf (fout,"%d\n", i);
         }
      } else {
         for (i=0; i < SO->N_Node; ++i) {
            if (enmask[i] == 2) fprintf (fout,"%d\n", i);
         }
      }
      SUMA_free(enmask); enmask = NULL;
      
      fclose(fout); fout = NULL;
   }
   
   if (closest_to_xyz) { /* read xyz list, report closest node (SLOW implementation...)*/
      MRI_IMAGE *im = NULL;
      float *far=NULL, *p=NULL;
      int nx2, N_XYZ = 0,i, i3, *closest=NULL, n;
      double xyz[3], d, *dXYZ=NULL;

      /* load the 1D file */
      im = mri_read_1D (closest_to_xyz);
      if (!im) {
         fprintf(SUMA_STDERR,"Error %s:\n Failed to read/find %s.\n", FuncName, closest_to_xyz);
         exit(1);
      }   

      far = MRI_FLOAT_PTR(im);
      if (im->nx == 0) {
         fprintf(SUMA_STDERR,"Error %s:\n Empty file %s.\n", FuncName, closest_to_xyz);
         exit(1);
      }
      if (im->ny != 3) {
         fprintf(SUMA_STDERR,"Error %s:\n Found %d columns in %s. Expecting 3\n", FuncName, im->ny, closest_to_xyz);
         exit(1);
      }

      /* set the results vector */
      dXYZ = (double *)SUMA_malloc(im->nx*sizeof(double));
      closest = (int *)SUMA_malloc(im->nx*sizeof(int));
      if (!dXYZ || !closest) {
         SUMA_S_Crit("Failed to allocate.");
         exit(1);
      }

      nx2 = 2*im->nx;
      for (i=0;i<im->nx; ++i) {  /* for each of the coordinates in question */
         xyz[0] = (double)far[i];
         xyz[1] = (double)far[i+im->nx];
         xyz[2] = (double)far[i+nx2];
         dXYZ[i] = 1023734552736672366372.0;
         closest[i] = -1;
         for (n=0; n<SO->N_Node; ++n) {
            p = &(SO->NodeList[SO->NodeDim*n]);
            SUMA_SEG_NORM(p, xyz, d);
            if (d < dXYZ[i]) {
               dXYZ[i] = d; closest[i] = n;
            }
         }
      }

      /* write out the results */
      sprintf(OutName, "%s.closest.1D.dset", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Closest nodes output file exists.\nWill not overwrite.");
         exit(1);
      }

      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }  

      fprintf (fout,"#closest nodes reference points\n");
      fprintf (fout,"#n = index of closest node\n");
      fprintf (fout,"#d = distance of closest node\n");
      fprintf (fout,"#X = X of reference point\n");
      fprintf (fout,"#Y = Y of reference point\n");
      fprintf (fout,"#Z = Z of reference point\n");
      fprintf (fout,"#Xn = X of node\n");
      fprintf (fout,"#Yn = Y of node\n");
      fprintf (fout,"#Zn = Z of node\n");
     
      fprintf (fout,"#n\td\tX\tY\tZ\tXn\tYn\tZn\n\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);

      for (i=0; i < im->nx; ++i) {
         fprintf (fout,"%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n", 
            closest[i], dXYZ[i], far[i], far[i+im->nx], far[i+nx2], 
               SO->NodeList[SO->NodeDim*closest[i]], 
               SO->NodeList[SO->NodeDim*closest[i]+1], 
               SO->NodeList[SO->NodeDim*closest[i]+2]);
      }

      fclose(fout); fout = NULL;

      /* clean up im */
      if (im) mri_free(im); im = NULL; 

      /* clean up other */
      if (closest) SUMA_free(closest); closest = NULL;
      if (dXYZ) SUMA_free(dXYZ); dXYZ = NULL;
   }
   
   SUMA_LH("Clean up");
   /* clean up */
   if (!SUMA_FreeSpecFields(&Spec)) { SUMA_S_Err("Error freeing"); exit(1); }

   if (MetricList) SUMA_free(MetricList);
   if (OutPrefix) SUMA_free(OutPrefix);
   if (OutName) SUMA_free(OutName);   
   if (SO) SUMA_Free_Surface_Object(SO);
   
   /* dset and its contents are freed in SUMA_Free_CommonFields */
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

   if (histnote) SUMA_free(histnote);
   
   SUMA_RETURN(0);
} /* Main */
