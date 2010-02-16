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

void usage_SUMA_SurfaceMetrics (SUMA_GENERIC_ARGV_PARSE *ps)
   {
      static char FuncName[]={"usage_SUMA_SurfaceMetrics"};
      char * s = NULL, *sio=NULL;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
         "\n"
"Usage: SurfaceMetrics <-Metric1> [[-Metric2] ...] \n"
"                  <-SURF_1> \n"
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
"      -tri_sines/-tri_cosines: (co)sine of angles at nodes forming\n"
"                                   triangles.\n"
"         Output file is prefix.(co)sine. Results in 4 columns:\n"
"         Col.0: Triangle Index\n"
"         Col.1: (co)sine of angle at node 0\n"
"         Col.2: (co)sine of angle at node 1\n"
"         Col.3: (co)sine of angle at node 2\n"
"      -tri_CoSines: Both cosines and sines.\n"
"      -tri_angles: Unsigned angles in radians of triangles.\n"
"         Col.0: Triangle Index\n"
"         Col.1: angle at node 0\n"
"         Col.2: angle at node 1\n"
"         Col.3: angle at node 2\n"
"      -node_angles: Unsigned angles in radians at nodes of surface.\n"
"         Col.0: Node Index\n"
"         Col.1: minimum angle at node \n"
"         Col.2: maximum angle at node \n"
"         Col.3: average angle at node \n"
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
"      -boundary_triangles: Output triangles that form a boundary of a surface\n"
"                   i.e. they contain edges that belong to one and only\n"
"                   one triangle.\n"
"      -internal_nodes: Output nodes that are not a boundary.\n"
"                   i.e. they form edges that belong to more than\n"
"                   one triangle.\n"
"\n"
"      You can use any or all of these metrics simultaneously.\n"
"\n"
"     (-SURF_1):  An option for specifying the surface.\n"
"                 (For option's syntax, see 'Specifying input surfaces'\n"
"                 section below).\n"
"\n"
"   -sv SurfaceVolume [VolParam for sf surfaces]: Specify a surface volume\n"
"                   for surface alignment. See ConvertSurface -help for \n"
"                   more info.\n"
"\n"
"   -tlrc: Apply Talairach transform to surface.\n"
"                   See ConvertSurface -help for more info.\n"
"\n"
"   -prefix prefix: Use prefix for output files. \n"
"                   (default is prefix of inSurf)\n"
"\n"
"   -quiet: Quiet\n"
"\n"
"    Options for applying arbitrary affine transform:\n"
"    [xyz_new] = [Mr] * [xyz_old - cen] + D + cen\n"
"    -xmat_1D mat: Apply transformation specified in 1D file mat.1D.\n"
"                  to the surface's coordinates.\n"
"                  [mat] = [Mr][D] is of the form:\n"
"                  r11 r12 r13 D1\n"
"                  r21 r22 r23 D2\n"
"                  r31 r32 r33 D3\n"
"                  or\n"
"                  r11 r12 r13 D1 r21 r22 r23 D2 r31 r32 r33 D3\n"
"    -ixmat_1D mat: Same as xmat_1D except that mat is replaced by inv(mat)\n"
"\n"
"        NOTE: For both -xmat_1D and -ixmat_1D, you can replace mat with \n"
"              one of the special strings:\n"
"              'RandShift', 'RandRigid', or 'RandAffine' which would create\n"
"              a transform on the fly. \n"
"    -seed SEED: Use SEED to seed the random number generator for random\n"
"                matrix generation\n"
"\n"
"    -xcenter x y z: Use vector cen = [x y z]' for rotation center.\n"
"                    Default is cen = [0 0 0]'\n"
"    -polar_decomp: Apply polar decomposition to mat and preserve\n"
"                   orthogonal component and shift only. \n"
"                   For more information, see cat_matvec's -P option.\n"
"                   This option can only be used in conjunction with\n"
"                   -xmat_1D\n"
"\n"
"%s"
"\n"
"%s"
"\n", sio, s);
      SUMA_free(s); s = NULL;
      SUMA_free(sio); sio = NULL;
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf ( "       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \n"
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
   int i, n1, n2, n1_3, n2_3, kar, nt, SO_read, N_Spec=0;
   double edgeL2, xcen[3];
   double xform[4][4];
   char *xmat_name=NULL;
   FILE *fout=NULL;
   SUMA_SO_File_Type iType = SUMA_FT_NOT_SPECIFIED;
   SUMA_SurfaceObject *SO = NULL;   
   SUMA_SFname *SF_name = NULL;
   void *SO_name = NULL;   
   SUMA_SurfSpecFile *pSpec=NULL;
   THD_warp *warp=NULL ;
   THD_3dim_dataset *aset=NULL;
   char *surf_names[SURFACEMETRICS_MAX_SURF];
   char *spec_file, *histnote;
   char *closest_to_xyz = NULL;
   int insurf_method = 0, N_surf = 0, ind = 0, quiet=0, randseed;
   SUMA_Boolean   brk, Do_tlrc, Do_conv, Do_curv, 
                  Do_area, Do_edges, Do_vol, Do_sph, NewCent, 
                  Do_cord, Do_TriNorm, Do_TriSine, Do_TriCosine,
                  Do_TriCoSine, Do_TriAngles, Do_PolDec,
                  Do_NodeAngles, Do_cen, Do_xmat, Do_inv,
                  Do_NodeNorm, Do_en, Do_in, Do_et; 
   SUMA_Boolean   LocalHead = NOPE;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   
	SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-s;-sv;");
   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);

   if (argc < 4)
       {
          SUMA_S_Err("Too few parameters");
          usage_SUMA_SurfaceMetrics (ps);
          exit (1);
       }
   
   MetricList = SUMA_StringAppend (NULL, NULL);
   kar = 1;
	brk = NOPE;
   xmat_name = NULL;
   xcen[0] = 0.0; xcen[1] = 0.0; xcen[2] = 0.0;
   Do_cen = NOPE;
   Do_xmat = NOPE;
   Do_inv = 0;
   Do_PolDec = 0;
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
   Do_TriSine = NOPE;
   Do_TriCosine = NOPE;
   Do_TriCoSine = NOPE;
   Do_TriAngles = NOPE;
   Do_NodeAngles = NOPE;
   Do_en = NOPE;
   Do_et = NOPE;
   Do_in = NOPE;
   quiet = 0;
   randseed = 12345;
   closest_to_xyz = NULL;
   NormScale = 5.0;
   NewCent = NOPE;
   OutPrefix = NULL;
   for (i=0; i<SURFACEMETRICS_MAX_SURF; ++i) { surf_names[i] = NULL; }   
   spec_file = NULL;

	while (kar < argc) { /* loop accross command ine options */
		/* fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName); */
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_SurfaceMetrics(ps);
          exit (1);
		}
		
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-sph_coords_center") == 0)) {
         kar ++;
			if (kar+2 >= argc)  {
		  		fprintf (SUMA_STDERR, 
                     "need 3 arguments after -sph_coords_center \n");
				exit (1);
			}
			sph_center[0] = atof(argv[kar]); kar ++;
         sph_center[1] = atof(argv[kar]); kar ++;
         sph_center[2] = atof(argv[kar]);
         NewCent = YUP;
         Do_sph = YUP;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-seed") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 integer after -seed\n");
				exit (1);
			}
			randseed = atoi(argv[kar]); 
			brk = YUP;
		}
      if (!brk && ( (strcmp(argv[kar], "-xmat_1d") == 0) ||
                    (strcmp(argv[kar], "-xmat_1D") == 0) ) ) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -xmat_1D\n");
				exit (1);
			}
			xmat_name = argv[kar]; 
         Do_xmat = YUP;
         Do_inv = 0;
			brk = YUP;
		}
      if (!brk && ( (strcmp(argv[kar], "-ixmat_1d") == 0) ||
                    (strcmp(argv[kar], "-ixmat_1D") == 0) ) ) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -ixmat_1D\n");
				exit (1);
			}
			xmat_name = argv[kar]; 
         Do_xmat = YUP;
         Do_inv = 1;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-polar_decomp") == 0)) {
         Do_PolDec = YUP;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-xcenter") == 0)) {
         kar ++;
			if (kar+2>= argc)  {
		  		fprintf (SUMA_STDERR, "need 3 arguments after -xcenter\n");
				exit (1);
			}
			xcen[0] = atof(argv[kar]); ++kar;
			xcen[1] = atof(argv[kar]); ++kar;
			xcen[2] = atof(argv[kar]); 
         Do_cen = YUP;
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-quiet") == 0)) {
         quiet = YUP;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-tri_sines") == 0)) {
         Do_TriSine = YUP;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-tri_angles") == 0)) {
         Do_TriAngles = YUP;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-node_angles") == 0)) {
         Do_NodeAngles = YUP;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-tri_cosines") == 0)) {
         Do_TriCosine = YUP;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-tri_CoSines") == 0)) {
         Do_TriCoSine = YUP;
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
         if (0 && iType == SUMA_FT_NOT_SPECIFIED) {
                              /* iType input no longer allowed */
            fprintf (SUMA_STDERR, 
                     " -sv option must be preceeded by -i_TYPE option.");
            exit(1);
         }
         kar ++;
			if (iType == SUMA_SUREFIT) {
            if (kar+1 >= argc)  {
		  		   fprintf (SUMA_STDERR, 
                  "need 2 argument after -sv (SurfaceVolume and VolumeParent)");
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
      
      if (!brk && (strcmp(argv[kar], "-boundary_triangles") == 0)) {
         Do_et = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-internal_nodes") == 0)) {
         Do_in = YUP;
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,
                  "Error %s: Option %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   /* clean MetricList */
   MetricList = SUMA_StringAppend (MetricList, NULL); 
   
   /* sanity checks */
   if (!strlen(MetricList->s) && 
      !Do_vol && !Do_sph && !Do_cord && 
      !Do_TriNorm && !Do_NodeNorm && 
      !Do_en && !Do_et && !Do_in && !closest_to_xyz &&
      !Do_TriSine && !Do_TriCosine && !Do_TriCoSine && 
      !Do_TriAngles && !Do_NodeAngles) {
      SUMA_S_Err("No Metrics specified.\nNothing to do.\n");
      exit(1);
   }
   
   if (Do_tlrc && !ps->sv[0]) {
      fprintf (SUMA_STDERR,
               "Error %s: -tlrc must be used with -sv option.\n", FuncName);
      exit(1);
   }

   if ((Do_xmat || Do_cen) && (Do_tlrc)) {
      fprintf (SUMA_STDERR,
               "Error %s: You can't do -tlrc or with -xmat_1D and -xcenter.\n", 
               FuncName);
      exit(1);
   }
   
   if ((!Do_xmat && Do_cen)) {
      fprintf (SUMA_STDERR,
               "Error %s: You can't use -xcenter without -xmat_1D.\n", FuncName);
      exit(1);
   }
   
   pSpec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }

   if (N_Spec > 1 ) {
      SUMA_S_Err( "Mike, you cannot mix -spec with -i or -t options "
                  "for specifying surfaces.");
      exit(1);
   }
   
   if (Do_xmat) {
      MRI_IMAGE *im = NULL;
      double *far=NULL;
      int nrow, ncol;
      if (!strcmp(xmat_name,"RandRigid")) {
         SUMA_FillRandXform(xform, randseed, 2);
      } else if (!strcmp(xmat_name,"RandAffine")) {
         SUMA_FillRandXform(xform, randseed, 3);
      } else if (!strcmp(xmat_name,"RandShift")) {
         SUMA_FillRandXform(xform, randseed, 1);
      } else {
         im = mri_read_double_1D (xmat_name);

         if (!im) {
            SUMA_SLP_Err("Failed to read 1D file");
            exit(1);
         }
         far = MRI_DOUBLE_PTR(im);
         nrow = im->nx;
         ncol = im->ny;
         if (nrow == 1) {
            if (ncol != 12) { 
               SUMA_SL_Err("Mat file must have\n"
                           "one row of 12 columns.");
               mri_free(im); im = NULL;   /* done with that baby */
               exit(1);
            }
            i = 0;
            while (i < 12) {
               xform[i/4][0] = far[i]; ++i;
               xform[i/4][1] = far[i]; ++i;
               xform[i/4][2] = far[i]; ++i;
               xform[i/4][3] = far[i]; ++i;
            }
            xform[3][0] = 0.0;  
            xform[3][1] = 0.0;  
            xform[3][2] = 0.0;  
            xform[3][3] = 1.0;
         } else {
            if (ncol < 4 ) {
               SUMA_SL_Err("Mat file must have\n"
                           "at least 4 columns.");
               mri_free(im); im = NULL;   /* done with that baby */
               exit(1);
            }
            if (nrow < 3 ) {
               SUMA_SL_Err("Mat file must have\n"
                           "at least 3 rows.");
               mri_free(im); im = NULL;   /* done with that baby */
               exit(1);
            }
            if (ncol > 4) {
               SUMA_SL_Warn(  "Ignoring entries beyond 4th \n"
                              "column in transform file.");
            }
            if (nrow > 3) {
               SUMA_SL_Warn(  "Ignoring entries beyond 3rd\n"
                              "row in transform file.\n");
            }
            for (i=0; i < 3; ++i) {
               xform[i][0] = far[i];
               xform[i][1] = far[i+nrow];
               xform[i][2] = far[i+2*nrow];
               xform[i][3] = far[i+3*nrow];
            }
            xform[3][0] = 0.0;  
            xform[3][1] = 0.0;  
            xform[3][2] = 0.0;  
            xform[3][3] = 1.0;
         }  
      }
      if (1|| LocalHead) {
         fprintf(SUMA_STDERR,"\n++ SurfaceMetrics xform:\n");
         for (i=0; i < 4; ++i) {
            fprintf(SUMA_STDERR," %+.5f\t%+.5f\t%+.5f\t%+.5f\n",
                   xform[i][0], xform[i][1], 
                   xform[i][2], xform[i][3]);  
         }
         fprintf(SUMA_STDERR,"\n");
      }
      
      mri_free(im); im = NULL;
      
      if (Do_inv) {
         mat44 A, A0;
   
         LOAD_MAT44( A0, \
                  xform[0][0], xform[0][1], xform[0][2], xform[0][3],    \
                  xform[1][0], xform[1][1], xform[1][2], xform[1][3],    \
                  xform[2][0], xform[2][1], xform[2][2], xform[2][3]   );
         A = nifti_mat44_inverse(A0);
         UNLOAD_MAT44(A,   \
                  xform[0][0], xform[0][1], xform[0][2], xform[0][3],    \
                  xform[1][0], xform[1][1], xform[1][2], xform[1][3],    \
                  xform[2][0], xform[2][1], xform[2][2], xform[2][3]   );
      }            

      
      if (Do_PolDec) {
         #ifdef USE_DECOMPOSE_SHOEMAKE
            /* a little something to do a polar decomposition on M into M = Q*S*/
            {
               float det, m[4][4], q[4][4], s[4][4];
               char *stmp = SUMA_append_string("QS_",xmat_name);
               FILE *fout = fopen(stmp,"w"); SUMA_free(stmp); stmp = NULL;
               SUMA_S_Note("FixMe! #include above and if(1) here ...");
               det = polar_decomp(M, q,s);
               fprintf(fout,"#[M][D]: (D is the shift)\n");
               for (i=0;i<3; ++i)
                  fprintf(fout,  "#%.5f   %.5f  %.5f  %.5f\n", 
                                 M[i][0], M[i][1], M[i][2], M[i][3]); 
               fprintf(fout,"#Q:\n");
               for (i=0;i<3; ++i)
                  fprintf(fout,  "#%.5f   %.5f  %.5f  %.5f\n", 
                                 q[i][0], q[i][1], q[i][2], q[i][3]); 
               fprintf(fout,"#S:\n");
               for (i=0;i<3; ++i)
                  fprintf(fout,  "#%.5f   %.5f  %.5f  %.5f\n", 
                                 s[i][0], s[i][1], s[i][2], s[i][3]);
               fprintf(fout,"#det: %f\n", det);
               fprintf(fout,  "#[Q][D]: A close xform to [M][D], "
                              "without scaling.\n#M = Q*S\n");
               for (i=0;i<3; ++i)
                  fprintf(fout,  "%.5f   %.5f  %.5f  %.5f\n", 
                                 q[i][0], q[i][1], q[i][2], M[i][3]);
               fclose(fout); SUMA_free(stmp); stmp = NULL;
            }
            /* replace user's xform with orthogonal one: */
            if (!quiet) fprintf(SUMA_STDERR,"Replacing matrix:\n");
            for (i=0;i<3 && !quiet; ++i)
                  fprintf( SUMA_STDERR,
                           " %.5f   %.5f  %.5f  %.5f\n", 
                           M[i][0], M[i][1], M[i][2], M[i][3]); 
            if (!quiet) fprintf(SUMA_STDERR,"     with matrix:\n");
            for (i=0;i<3 && !quiet; ++i)
                  fprintf(SUMA_STDOUT, 
                           " %.5f   %.5f  %.5f  %.5f\n", 
                           q[i][0], q[i][1], q[i][2], M[i][3]);
            for (i=0;i<3; ++i) { 
               M[i][0] = q[i][0]; M[i][1] = q[i][1]; M[i][2] = q[i][2]; 
            }
            
         #else
            {/* use the NIFTI polar decomposition function 
               (same results as above)*/
               mat33 Q, A;
               for (i=0;i<3;++i) { 
                  A.m[i][0] = xform[i][0]; 
                  A.m[i][1] = xform[i][1]; 
                  A.m[i][2] = xform[i][2]; 
               }
               Q = nifti_mat33_polar( A );
               /* replace user's xform with orthogonal one: */
               if (!quiet) fprintf(SUMA_STDERR,"Replacing matrix:\n");
               for (i=0;i<3 && !quiet; ++i)
                     fprintf( SUMA_STDERR,
                              " %.5f   %.5f  %.5f  %.5f\n", 
                              xform[i][0], xform[i][1], 
                              xform[i][2], xform[i][3]); 
               if (!quiet) fprintf(SUMA_STDERR,"     with matrix:\n");
               for (i=0;i<3 && !quiet; ++i)
                     fprintf( SUMA_STDERR,
                              " %.5f   %.5f  %.5f  %.5f\n", 
                              Q.m[i][0], Q.m[i][1], Q.m[i][2], xform[i][3]);
               for (i=0;i<3; ++i) { 
                  xform[i][0] = Q.m[i][0]; 
                  xform[i][1] = Q.m[i][1]; 
                  xform[i][2] = Q.m[i][2]; 
               }
                
            }
         #endif 
      }
   }
   
   SUMA_LH("Loading surface...");
   SO = SUMA_Load_Spec_Surf(pSpec, 0, ps->sv[0], 1);

   if (Do_xmat) {
      if (!quiet) fprintf (SUMA_STDERR,"Performing affine transform...\n");
      if (!quiet) {
         for (i=0; i<3 ; ++i) {
            fprintf (SUMA_STDERR,
                     "M[%d][:] = %f %f %f %f\n", 
                     i, xform[i][0], xform[i][1], xform[i][2], xform[i][3]);
         }
         fprintf (SUMA_STDERR,"Cen[:] %f %f %f\n", xcen[0], xcen[1], xcen[2]);
      }
      if (Do_cen) {
         if (!SUMA_Apply_Coord_xform(  SO->NodeList, SO->N_Node, SO->NodeDim,
                                       xform, 0, xcen)) { 
            SUMA_SL_Err("Failed to xform coordinates"); exit(1); 
         }
      } else {
         if (!SUMA_Apply_Coord_xform(  SO->NodeList, SO->N_Node, SO->NodeDim,
                                       xform, 0, NULL)) { 
            SUMA_SL_Err("Failed to xform coordinates"); exit(1); 
         }
      }
      /* recalculate normals */
      SUMA_RECOMPUTE_NORMALS(SO);
   }
   
   if (Do_tlrc) {
      if (!quiet) fprintf (SUMA_STDERR,"Performing talairach transform...\n");

      /* form the tlrc version of the surface volume */
      tlrc_name = (char *) SUMA_calloc (strlen(SO->VolPar->dirname)+
                                        strlen(SO->VolPar->prefix)+60, 
                                        sizeof(char));
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
      double *sph=NULL;
      sprintf(OutName, "%s.sphcoord.1D.dset", OutPrefix);
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
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
         SUMA_S_Err( "Failed to open file for writing.\n"
                     "Check your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Spherical coords, \n");
      fprintf (fout,
               "#  cartesian coords shifted by [%f %f %f] prior to xform\n",
                sph_center[0], sph_center[1], sph_center[2]);
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
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
         SUMA_S_Err("Node normals output file exists.\nWill not overwrite.");
         exit(1);
      }
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err( "Failed to open file for writing.\n"
                     "Check your permissions.\n");
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
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
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
         n1 = SO->FaceSetList[3*i]; 
         n2 = SO->FaceSetList[3*i+1]; 
         n3 = SO->FaceSetList[3*i+2];
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
   
   if (Do_TriAngles) {
      int n1, n2, n3;
      float *p1, *p2, *p3;
      double s[3], c[3], a[3];
      if (SO->FaceSetDim != 3) {
         SUMA_S_Err("Triangular meshes only please.");
         exit(1);
      }
      sprintf(OutName, "%s.TriAngles.1D", OutPrefix);
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
         SUMA_S_Err( "Triangle normals output file exists.\n"
                     "Will not overwrite.");
         exit(1);
      }
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err( "Failed to open file for writing.\n"
                     "Check your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Triangle Angles in radians(unsigned).\n");
      fprintf (fout,"#Col. 0: Triangle index.\n");
      fprintf (fout,"#Col. 1: angle at node 1\n");
      fprintf (fout,"#Col. 2: angle at node 2\n");
      fprintf (fout,"#Col. 3: angle at node 3\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i<SO->N_FaceSet; ++i) {
         n1 = SO->FaceSetList[3*i]; 
         n2 = SO->FaceSetList[3*i+1]; 
         n3 = SO->FaceSetList[3*i+2];
         p1 = &(SO->NodeList[3*n1]);
         p2 = &(SO->NodeList[3*n2]);
         p3 = &(SO->NodeList[3*n3]);
         if (!SUMA_TriTrig(p1, p2, p3, s, c, a)) {
            SUMA_S_Err("Failed in SUMA_TriTrig");
            exit(1);
         }

         fprintf (fout,"%d \t%f %f %f \n", i, a[0], a[1], a[2]);
      }
   }
   if (Do_NodeAngles) {
      int n1, n2, n3;
      float *p1, *p2, *p3;
      double s[3], c[3], a[3];
      double *mia=NULL, *maa=NULL, *mea=NULL;
      int *n_a=NULL;
      if (SO->FaceSetDim != 3) {
         SUMA_S_Err("Triangular meshes only please.");
         exit(1);
      }
      sprintf(OutName, "%s.NodeAngles.1D", OutPrefix);
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
         SUMA_S_Err( "Node angles output file exists.\n"
                     "Will not overwrite.");
         exit(1);
      }
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err( "Failed to open file for writing.\n"
                     "Check your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Angles at nodes in radians(unsigned).\n");
      fprintf (fout,"#Col. 0: Node index.\n");
      fprintf (fout,"#Col. 1: minimum angle at node \n");
      fprintf (fout,"#Col. 2: maximum angle at node \n");
      fprintf (fout,"#Col. 3: average angle at node \n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      mea = (double *)SUMA_calloc(SO->N_Node, sizeof(double));
      mia = (double *)SUMA_calloc(SO->N_Node, sizeof(double));
      maa = (double *)SUMA_calloc(SO->N_Node, sizeof(double));
      n_a = (int *)SUMA_calloc(SO->N_Node, sizeof(int));
      if (!mea || !mia || !maa || !n_a) {
         SUMA_S_Crit("Failed to allocate");
         exit(1);
      }
      for (i=0; i<SO->N_FaceSet; ++i) {
         n1 = SO->FaceSetList[3*i]; 
         n2 = SO->FaceSetList[3*i+1]; 
         n3 = SO->FaceSetList[3*i+2];
         p1 = &(SO->NodeList[3*n1]);
         p2 = &(SO->NodeList[3*n2]);
         p3 = &(SO->NodeList[3*n3]);
         if (!SUMA_TriTrig(p1, p2, p3, s, c, a)) {
            SUMA_S_Err("Failed in SUMA_TriTrig");
            exit(1);
         }

         if (!n_a[n1]) {
            mea[n1] = a[0]; ++n_a[n1];
            mia[n1] = a[0];
            maa[n1] = a[0];
         } else {
            mea[n1] += a[0]; ++n_a[n1];
            if (mia[n1] > a[0]) mia[n1] = a[0];
            if (maa[n1] < a[0]) maa[n1] = a[0];
         }
         if (!n_a[n2]) {
            mea[n2] = a[1]; ++n_a[n2];
            mia[n2] = a[1];
            maa[n2] = a[1];
         } else {
            mea[n2] += a[1]; ++n_a[n2];
            if (mia[n2] > a[1]) mia[n2] = a[1];
            if (maa[n2] < a[1]) maa[n2] = a[1];
         }
         if (!n_a[n3]) {
            mea[n3] = a[2]; ++n_a[n3];
            mia[n3] = a[2];
            maa[n3] = a[2];
         } else {
            mea[n3] += a[2]; ++n_a[n3];
            if (mia[n3] > a[2]) mia[n3] = a[2];
            if (maa[n3] < a[2]) maa[n3] = a[2];
         }
      }
      for (i=0; i<SO->N_Node; ++i) {
         if (n_a[i]) {
            fprintf (fout,"%d \t%f %f %f \n",
                  i, mia[i], maa[i], mea[i]/(double)n_a[i]);
         } else {
            fprintf (fout,"%d \t%f %f %f \n",
                  i, -1.0, -1.0, -1.0);
         }
      }
      SUMA_free(mea); SUMA_free(mia); SUMA_free(n_a); SUMA_free(maa);
   }
   if (Do_TriSine) {
      int n1, n2, n3;
      float *p1, *p2, *p3;
      double s[3], c[3], *a=NULL;
      if (SO->FaceSetDim != 3) {
         SUMA_S_Err("Triangular meshes only please.");
         exit(1);
      }
      sprintf(OutName, "%s.TriSine.1D", OutPrefix);
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
         SUMA_S_Err( "Triangle sines output file exists.\n"
                     "Will not overwrite.");
         exit(1);
      }
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err( "Failed to open file for writing.\n"
                     "Check your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Triangle Sines.\n");
      fprintf (fout,"#Col. 0: Triangle index.\n");
      fprintf (fout,"#Col. 1: sin(angle) at node 1\n");
      fprintf (fout,"#Col. 2: sin(angle) at node 2\n");
      fprintf (fout,"#Col. 3: sin(angle) at node 3\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i<SO->N_FaceSet; ++i) {
         n1 = SO->FaceSetList[3*i]; 
         n2 = SO->FaceSetList[3*i+1]; 
         n3 = SO->FaceSetList[3*i+2];
         p1 = &(SO->NodeList[3*n1]);
         p2 = &(SO->NodeList[3*n2]);
         p3 = &(SO->NodeList[3*n3]);
         if (!SUMA_TriTrig(p1, p2, p3, s, c, a)) {
            SUMA_S_Err("Failed in SUMA_TriTrig");
            exit(1);
         }

         fprintf (fout,"%d \t%f %f %f \n", i, s[0], s[1], s[2]);
      }
   }
   if (Do_TriCosine) {
      int n1, n2, n3;
      float *p1, *p2, *p3;
      double s[3], c[3], *a=NULL;
      if (SO->FaceSetDim != 3) {
         SUMA_S_Err("Triangular meshes only please.");
         exit(1);
      }
      sprintf(OutName, "%s.TriCosine.1D", OutPrefix);
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
         SUMA_S_Err( "Triangle cosines output file exists.\n"
                     "Will not overwrite.");
         exit(1);
      }
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err( "Failed to open file for writing.\n"
                     "Check your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Triangle Cosines.\n");
      fprintf (fout,"#Col. 0: Triangle index.\n");
      fprintf (fout,"#Col. 1: cos(angle) at node 1\n");
      fprintf (fout,"#Col. 2: cos(angle) at node 2\n");
      fprintf (fout,"#Col. 3: cos(angle) at node 3\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i<SO->N_FaceSet; ++i) {
         n1 = SO->FaceSetList[3*i]; 
         n2 = SO->FaceSetList[3*i+1]; 
         n3 = SO->FaceSetList[3*i+2];
         p1 = &(SO->NodeList[3*n1]);
         p2 = &(SO->NodeList[3*n2]);
         p3 = &(SO->NodeList[3*n3]);
         if (!SUMA_TriTrig(p1, p2, p3, s, c, a)) {
            SUMA_S_Err("Failed in SUMA_TriTrig");
            exit(1);
         }

         fprintf (fout,"%d \t%f %f %f \n", i, c[0], c[1], c[2]);
      }
   }
   if (Do_TriCoSine) {
      int n1, n2, n3;
      float *p1, *p2, *p3;
      double s[3], c[3], *a=NULL;
      if (SO->FaceSetDim != 3) {
         SUMA_S_Err("Triangular meshes only please.");
         exit(1);
      }
      sprintf(OutName, "%s.TriCoSine.1D", OutPrefix);
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
         SUMA_S_Err( "Triangle cosines and sines output file exists.\n"
                     "Will not overwrite.");
         exit(1);
      }
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err( "Failed to open file for writing.\n"
                     "Check your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Triangle Cosines and Sines.\n");
      fprintf (fout,"#Col. 0: Triangle index.\n");
      fprintf (fout,"#Col. 1: cos(angle) at node 1\n");
      fprintf (fout,"#Col. 2: cos(angle) at node 2\n");
      fprintf (fout,"#Col. 3: cos(angle) at node 3\n");
      fprintf (fout,"#Col. 4: sin(angle) at node 1\n");
      fprintf (fout,"#Col. 5: sin(angle) at node 2\n");
      fprintf (fout,"#Col. 6: sin(angle) at node 3\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      
      for (i=0; i<SO->N_FaceSet; ++i) {
         n1 = SO->FaceSetList[3*i]; 
         n2 = SO->FaceSetList[3*i+1]; 
         n3 = SO->FaceSetList[3*i+2];
         p1 = &(SO->NodeList[3*n1]);
         p2 = &(SO->NodeList[3*n2]);
         p3 = &(SO->NodeList[3*n3]);
         if (!SUMA_TriTrig(p1, p2, p3, s, c, a)) {
            SUMA_S_Err("Failed in SUMA_TriTrig");
            exit(1);
         }

         fprintf (fout,"%d \t%f %f %f \t%f %f %f\n",
                      i, c[0], c[1], c[2], s[0], s[1], s[2]);
      }
   }
   
   if (Do_cord) {
      sprintf(OutName, "%s.coord.1D.dset", OutPrefix);
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
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
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
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
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
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
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
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
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
         SUMA_S_Err("Convexities output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err( "Failed to open file for writing.\n"
                     "Check your permissions.\n");
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
      int prob;
      if (!quiet) fprintf (SUMA_STDERR,"Calculating surface volume...\n");
      vol = SUMA_Mesh_Volume(SO, NULL, -1, 1, &prob);
      if (prob) {
         SUMA_S_Warn("Precision problem calculating surface\n"
                     "Repeat with a rotated version of surface");
      }
      if (!quiet) {
         fprintf (SUMA_STDOUT,   
               "Volume of closed surface is %f (units3).\n"
               "Signed volume is  %f (units3).\n", fabs(vol), vol); 
      } else {
         fprintf (SUMA_STDOUT,   
               "%f , %f \n", fabs(vol), vol); 
      }
   }
   
   if (Do_en || Do_in) {
      byte *enmask = NULL;
      if (!quiet) {
         if (Do_en) fprintf (SUMA_STDERR,"finding boundary nodes...\n");
         else fprintf (SUMA_STDERR,"finding internal nodes...\n");
      }
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
         if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
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
         if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
            SUMA_S_Err("Internalnodes output file exists.\nWill not overwrite.");
            exit(1);
         }
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\n"
                    "Check your permissions.\n");
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
   
   if (Do_et) {
      int N_b, *boundt=(int*)SUMA_calloc(SO->N_FaceSet, sizeof(int));
      
      N_b = SUMA_BoundaryTriangles(SO, boundt, 0);
      SUMA_S_Note("Writing edge triangles...");
      
      sprintf(OutName, "%s.boundary_triangles", OutPrefix);
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
         SUMA_S_Err("Area output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err( "Failed to open file for writing.\n"
                     "Check your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Boundary Triangles Area\n");
      fprintf (fout,"#BT = Index of boundary triangle\n");
      fprintf (fout,"#BT\n\n");
      if (histnote) fprintf (fout,"#History:%s\n", histnote);
      for (i=0; i < N_b; ++i) {
         fprintf (fout,"%d\n",  boundt[i]);
      }  
      if (boundt) SUMA_free(boundt); boundt=NULL;
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
      if (!THD_ok_overwrite() && SUMA_filexists(OutName)) {
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
   if (MetricList) SUMA_free(MetricList);
   if (OutPrefix) SUMA_free(OutPrefix);
   if (OutName) SUMA_free(OutName);   
   if (SO) SUMA_Free_Surface_Object(SO);
   if (!SUMA_FreeSpecFields(pSpec)) {
      SUMA_S_Err("Failed to free Spec fields");
   } SUMA_free(pSpec); pSpec = NULL; 
 	if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   
   /* dset and its contents are freed in SUMA_Free_CommonFields */
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

   if (histnote) SUMA_free(histnote);
    
   SUMA_RETURN(0);
} /* Main */
