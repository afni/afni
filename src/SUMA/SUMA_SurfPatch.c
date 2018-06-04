#include "SUMA_suma.h"

#define SURFPATCH_MAX_SURF 10  /*!< Maximum number of input surfaces */

void usage_SUMA_getPatch (SUMA_GENERIC_ARGV_PARSE *ps, int detail)
   {
      static char FuncName[]={"usage_SUMA_getPatch"};
      char * s = NULL, *sio=NULL;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
"\nUsage:\n"
"  SurfPatch <-spec SpecFile> <-surf_A insurf> <-surf_B insurf> ...\n"
"            <-input nodefile inode ilabel> <-prefix outpref>  \n"
"            [-hits min_hits] [-masklabel msk] [-vol] [-patch2surf]\n"
"            [-vol_only] [-coord_gain] [-check_bowtie] [-fix_bowtie] \n"
"            [-ok_bowtie] [-adjust_contour] [-do-not-adjust_contour] \n"
"            [-stiched_surface SURF]   \n"
"\n%s", detail ? "":"use -h or -help for more help detail.\n");
   if (detail) {
      printf ( 
"Usage 1:\n"
"  The program creates a patch of surface formed by nodes \n"
"  in nodefile.\n"
"  The program can also be used to calculate the volume between the same patch\n"
"  on two isotopic surfaces. See -vol option below.\n"
"\n"
"     -spec SpecFile: Spec file containing input surfaces.\n"
"     -surf_X: Name of input surface X where X is a character\n"
"              from A to Z. If surfaces are specified using two\n"
"              files, use the name of the node coordinate file.\n"
"     -input nodefile inode ilabel: \n"
"            nodefile is the file containing nodes defining the patch.\n"
"            inode is the index of the column containing the nodes\n"
"            ilabel is the index of the column containing labels of\n"
"                   the nodes in column inode. If you want to use\n"
"                   all the nodes in column indode, then set this \n"
"                   parameter to -1 (default). \n"
"                   If ilabel is not equal to 0 then the corresponding \n"
"                   node is used in creating the patch.\n"
"                   See -masklabel option for one more variant.\n"
"     -prefix outpref: Prefix of output patch. If more than one surface\n"
"                      are entered, then the prefix will have _X added\n"
"                      to it, where X is a character from A to Z.\n"
"                      Output format depends on the input surface's.\n"
"                      With that setting, checking on pre-existing files\n"
"                      is only done before writing the new patch, which is\n"
"                      annoying. You can set the output type ahead of time\n"
"                      using -out_type option. This way checking for \n"
"                      pre-existing output files can be done at the outset.\n"
"     -vol: Calculate the volume formed by the patch on surf_A and\n"
"            and surf_B. For this option, you must specify two and\n"
"            only two surfaces with surf_A and surf_B options.\n"
"     -vol_only: Only calculate the volume, don't write out patches.\n"
"                See also -fix_bowtie option below.\n"
"\n"
"  ** If you are more interested in the volume attributed to one node, or a \n"
"     set of nodes, between two isotopic surfaces, you are much better off \n"
"     using SurfMeasures' -node_volg option. SurfMeasures has an efficient \n"
"     implementation of the Gauss Theorem based volume estimation.\n"
"\n"
"     -out_type TYPE: Type of all output patches, regardless of input \n"
"                     surface type.\n"
"                     Choose from: FreeSurfer, SureFit, 1D and Ply.\n"
"     -hits min_hits: Minimum number of nodes specified for a triangle\n"
"                     to be made a part of the patch (1 <= min_hits <= 3)\n"
"                     default is 2.\n"
"     -masklabel msk: If specified, then only nodes that are labeled with\n"
"                     with msk are considered for the patch.\n"
"                     This option is useful if you have an ROI dataset file\n"
"                     and whish to create a patch from one out of many ROIs\n"
"                     in that file. This option must be used with ilabel \n"
"                     specified (not = -1)\n"
"     -patch2surf: Turn surface patch into a surface where only nodes used in\n"
"                  forming the mesh are preserved.\n"
"     -node_depth NODE_DEPTH: Compute depth of each node after projection onto\n"
"                  the 1st principal direction of the nodes making up the\n"
"                  surface. The results are written in a file with prefix\n"
"                  NODE_DEPTH.pcdepth.1D.dset. You must use -patch2surf \n"
"                  in order to use this option. \n"
"                  Option is similar to the one in program ConvertSurface.\n" 
"     -check_bowtie: Check if the patch has a section hanging by one node to\n"
"                    the rest of the mesh. Think of a patch made of two \n"
"                    triangles that are connected at one node only. \n"
"                    Think Bowtie. Bowties should not occur if original \n"
"                    surface is 2 manifold and min_hits == 1\n"
"                    -check_bowtie is the default when -vol or -vol_only \n"
"                    are used. Volume computation will fail in the presence\n"
"                    of bowties.\n"
"     -fix_bowtie: Modify patch to eliminate bowties. This only works if \n"
"                  min_hits is > 1. The repair is done by relaxing min_hits\n"
"                  at the node(s) where the bowtie happens.\n"
"     -ok_bowtie: Do not check for, or fix bowties. \n"
"                 Default when -vol* are not used.\n" 
"     -adjust_contour: Once the patch is created, shrink its contours at nodes\n"
"                      that were not in nodefile (non-selected).\n"
"                      Each non-selected node is moved to the center of mass\n"
"                      of itself and neighboring selected nodes.\n"
"                      This adjustment might make sense when min_hits < 3\n"
"                      \n"
"     -do-not-adjust_contour:  Do not adjust contrours.\n" 
"                              This is the default.\n"
"     -stiched_surface STICHED: Write out the stiched surface used to\n"
"                               calculate the volume. \n"
"                               If -adjust_contour is used, this option also\n"
"                               writes out a file that shows which \n"
"                               nodes on the original surface were adjusted.\n"
"                               The first column in the node number. The 2nd\n"
"                               contains the number of selected nodes that \n"
"                               neighbored non-selected nodes in the patch.\n" 
"     -coord_gain GAIN: Multiply node coordinates by a GAIN.\n"
"                       That's useful if you have a tiny patch that needs\n"
"                       enlargement for easier viewing in SUMA.\n"
"                       Although you can zoon over very large ranges in SUMA\n"
"                       tiny tiny patches are hard to work with because\n"
"                       SUMA's parameters are optimized to work with objects\n"
"                       on the order of a brain, not on the order of 1 mm.\n"
"                       Gain is applied just before writing out patches.\n"
"     -flip_orientation: Change orientation of triangles before writing\n"
"                        surfaces.\n"
"     -verb VERB: Set verbosity level, 1 is the default.\n"
"\n"
"   Example 1: Given an ROI, a white matter and a gray matter surface\n"
"              calculate the volume of cortex enclosed by the roi on\n"
"              both surfaces.\n"
"              Assume you have the spec file and surfaces already. You can\n"
"              get the same files from the SUMA directory in the AFNI \n"
"              workshop SUMA's archive which you can get with: \n"
"                 afni_open -aw suma_demo.tgz\n"
"\n"
"              Draw an ROI on the surface and save it as: lh.manualroi.1D.roi\n"
"\n"
"         To calculate the volume and create a enclosing surface:\n"
"             SurfPatch   -spec DemoSubj_lh.spec \\\n"
"                         -sv DemoSubj_SurfVol+orig  \\\n"
"                         -surf_A lh.smoothwm  \\\n"
"                         -surf_B lh.pial   \\\n"
"                         -prefix lh.patch \\\n"
"                         -input lh.manualroi.1D.roi 0 -1  \\\n"
"                         -out_type fs   \\\n"
"                         -vol  \\\n"
"                         -adjust_contour \\\n"
"                         -stiched_surface lh.stiched   \\\n"
"                         -flip_orientation \n"
"\n"
"   Example 2: If you want to voxelize the region between the two surfaces\n"
"              you can run the following on the output.\n"
"                 3dSurfMask -i lh.stiched.ply \\\n"
"                            -prefix lh.closed -fill_method SLOW \\\n"
"                            -grid_parent DemoSubj_SurfVol+orig.HEAD \n"
"              3dSurfMask will output a dataset called lh.closed.d+orig which\n"
"              contains the signed closest distance from each voxel to the \n"
"              surface. Negative distances are outside the surface.\n"
"\n"
"              To examine the results:\n"
"                 suma -npb 71 -i lh.stiched.ply -sv DemoSubj_SurfVol+orig. &\n"
"                 afni -npb 71 -niml -yesplugouts & \n"
"                 DriveSuma -npb 71 -com viewer_cont -key 't' \n"
"                 plugout_drive  -npb 71  \\\n"
"                                -com 'SET_OVERLAY lh.closed.d' \\\n"
"                                -com 'SET_FUNC_RANGE A.3' \\\n"
"                                -com 'SET_PBAR_NUMBER A.10' \\\n"
"                                -com 'SET_DICOM_XYZ A. 10 70 22 '\\\n"
"                                -quit\n"
"\n"
"%s"
"\n"
"%s"
"\n", (detail > 1) ? sio : "Use -help for I/O and miscellaneous options." , 
      (detail > 1) ? s : "");
   }
    if (sio) SUMA_free(sio); s = NULL;        
    if (s) SUMA_free(s); s = NULL;        
    if (detail) {
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
    }
   return;
}

typedef struct {
   SUMA_SO_File_Type iType;
   SUMA_SO_File_Type oType;
   char *out_prefix;
   char *out_volprefix;
   char *in_name;
   int minhits;
   int thislabel;
   int labelcol;
   int nodecol;
   int DoVol;
   int VolOnly;
   int FixBowTie;
   int adjust_contour;
   float coordgain;
   SUMA_Boolean Do_p2s;
   int verb;
   int flip;
   char *nodedepth;
} SUMA_GETPATCH_OPTIONS;

/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_GETPATCH_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_prefix); 
               SUMA_free(Opt);
*/
SUMA_GETPATCH_OPTIONS *SUMA_GetPatch_ParseInput (char *argv[], int argc,
                                                 SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_GetPatch_ParseInput"}; 
   SUMA_GETPATCH_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outprefix;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_GETPATCH_OPTIONS *)SUMA_malloc(sizeof(SUMA_GETPATCH_OPTIONS));

   kar = 1;
   Opt->iType = SUMA_FT_NOT_SPECIFIED;
   Opt->out_prefix = NULL;
   Opt->out_volprefix = NULL;
   Opt->in_name = NULL;
   Opt->minhits = 2;
   Opt->labelcol = -1;
   Opt->nodecol = -1;
   Opt->thislabel = -1;
   Opt->DoVol = 0;
   Opt->VolOnly = 0;
   Opt->coordgain = 0.0;
   Opt->Do_p2s = NOPE;
   Opt->nodedepth = NULL;
   Opt->FixBowTie = -1;
   Opt->adjust_contour = -1;
   Opt->oType = SUMA_FT_NOT_SPECIFIED;
   Opt->verb = 1;
   Opt->flip = 0;
	brk = NOPE;
   
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_getPatch(ps, strlen(argv[kar]) > 3 ? 2:1);
          exit (0);
		}
		
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-hits") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -hits \n");
				exit (1);
			}
			Opt->minhits = atoi(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-coord_gain") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -coord_gain \n");
				exit (1);
			}
			Opt->coordgain = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-masklabel") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -masklabel \n");
				exit (1);
			}
			Opt->thislabel = atoi(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-node_depth") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -node_depth \n");
				exit (1);
			}
			Opt->nodedepth = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-patch2surf") == 0)) {
         Opt->Do_p2s = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-vol") == 0)) {
			Opt->DoVol = 1;
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-flip_orientation") == 0)) {
			Opt->flip = 1;
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-vol_only") == 0)) {
			Opt->DoVol = 1;
         Opt->VolOnly = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-adjust_contour") == 0)) {
			Opt->adjust_contour = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-do-not-adjust_contour") == 0)) {
			Opt->adjust_contour = 0;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-check_bowtie") == 0)) {
			Opt->FixBowTie = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-fix_bowtie") == 0)) {
			Opt->FixBowTie = 2;
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-ok_bowtie") == 0)) {
			Opt->FixBowTie = 0;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix \n");
				exit (1);
			}
			Opt->out_prefix = SUMA_copy_string(argv[kar]);
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-verb") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -verb \n");
				exit (1);
			}
			Opt->verb = atoi(argv[kar]);
         if (Opt->verb < 0 || Opt->verb > 10) {
            SUMA_S_Errv("Something fishy with -verb value of %s\n"
                        "Need integer from 0 to 2\n", argv[kar]);
            exit(1);
         }
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-stiched_surface") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -stiched_surface \n");
				exit (1);
			}
			Opt->out_volprefix = SUMA_copy_string(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-out_type") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -out_type \n");
				exit (1);
			}
			if ((Opt->oType = 
               SUMA_GuessSurfFormatFromExtension(argv[kar], NULL)) <= 
                  SUMA_FT_NOT_SPECIFIED) {
            /* For backward compatibility */
            Opt->oType = SUMA_guess_surftype_argv(argv[kar]);        
         }
         if (Opt->oType <= SUMA_FT_NOT_SPECIFIED) Opt->oType=SUMA_GIFTI;
         brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar+2 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 3 arguments after -input \n");
				exit (1);
			}
			Opt->in_name = argv[kar]; kar ++;
         Opt->nodecol = atoi(argv[kar]); kar ++;
         Opt->labelcol = atoi(argv[kar]); 
			brk = YUP;
		}
      
#if 0
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
      
      if (!brk && (strncmp(argv[kar], "-surf_", 6) == 0)) {
			if (kar + 1>= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -surf_X SURF_NAME \n");
				exit (1);
			}
			ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFPATCH_MAX_SURF) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option is out of range.\n");
				exit (1);
         }
         kar ++;
         Opt->surf_names[ind] = argv[kar];
         Opt->N_surf = ind+1;
         brk = YUP;
		}
#endif      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,
                  "Error %s:\nOption %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
         suggest_best_prog_option(argv[0], argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
      
   }
   
   /* sanity checks */
   if (Opt->FixBowTie < 0) {
      if (Opt->DoVol) Opt->FixBowTie = 1; /* important to check in this case */
      else Opt->FixBowTie = 0;
   }
   
   if (Opt->adjust_contour < 0) {
      if (Opt->DoVol) Opt->adjust_contour = 0;
      else Opt->adjust_contour = 0;
   }
   
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("SurfPatch");
   
   if (Opt->thislabel >= 0 && Opt->labelcol < 0) {
      SUMA_SL_Err("Cannot use -masklabel without specifying "
                  "ilabel in -input option");
      exit(1);
   } 
   if (Opt->minhits < 1 || Opt->minhits > 3) {
      SUMA_SL_Err("minhits must be > 0 and < 3");
      exit(1);
   }
   if (!Opt->in_name) {
      SUMA_SL_Err("No input specified.");
      exit(1);
   }
   SUMA_RETURN (Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfPatch"};
   SUMA_GETPATCH_OPTIONS *Opt; 
   char *ppref=NULL, ext[5]; 
   float *far=NULL;
   MRI_IMAGE *im = NULL;
   int SO_read = -1;
   int   *NodePatch=NULL, N_NodePatch=-1, *FaceSetList=NULL , 
         N_FaceSet = -1, N_Node = -1, N_Spec=0;          
   int i, inodeoff=-1, ilabeloff=-1, nvec, ncol, cnt;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_PATCH *ptch = NULL; 
   SUMA_SurfSpecFile *Spec;
   SUMA_INDEXING_ORDER d_order;
   void *SO_name = NULL;
   SUMA_Boolean exists = NOPE;
   SUMA_SO_File_Type typetmp;
   SUMA_SurfaceObject *SOnew = NULL;
   float *NodeList = NULL;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean LocalHead = NOPE;
	
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-s;-sv;");
   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   Opt = SUMA_GetPatch_ParseInput (argv, argc, ps);
   if (argc < 2)
    {
       SUMA_S_Err("Too few options");
       usage_SUMA_getPatch(ps, 0);
       exit (1);
    }


   /* read all surfaces */
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }

   if (N_Spec > 1 ) {
      SUMA_S_Err( "Mike, you cannot mix -spec with -i or -t options "
                  "for specifying surfaces.");
      exit(1);
   }
   
   if (Spec->N_Surfs < 1) {
      SUMA_S_Err("No surfaces");
      exit(1);
   }
     
   if (Opt->DoVol && Spec->N_Surfs != 2) {
      SUMA_S_Errv("Must specify 2 and only 2 surfaces with -vol options\n"
                  "Have %d from the command line\n",Spec->N_Surfs);
      exit(1);
   }
   
   if (!Opt->Do_p2s && Opt->nodedepth) {
      SUMA_S_Err("You should not use -node_depth without -patch2surf");
      exit(1);
   }
   
   if (Opt->oType != SUMA_FT_NOT_SPECIFIED && !Opt->VolOnly) { 
      for (i=0; i < Spec->N_Surfs; ++i) {
         if (Spec->N_Surfs > 1) {
            sprintf(ext, "_%c", 65+i);
            ppref = SUMA_append_string(Opt->out_prefix, ext);
         } else {
            ppref = SUMA_copy_string(Opt->out_prefix);
         }
         
         SO_name = SUMA_Prefix2SurfaceName(ppref, NULL, NULL, 
                                           Opt->oType, &exists);
         if (exists && !THD_ok_overwrite()) {
            fprintf(SUMA_STDERR, "Error %s:\nOutput file(s) %s* on disk.\n"
                                 "Will not overwrite.\n", FuncName, ppref);
            exit(1);
         }
         if (ppref) SUMA_free(ppref); ppref = NULL; 
         if (SO_name) SUMA_free(SO_name); SO_name = NULL;
      } 
   }
   
   /* read in the file containing the node information */
   im = mri_read_1D (Opt->in_name);

   if (!im) {
      SUMA_S_Errv("Failed to read 1D file '%s'\n", Opt->in_name);
      exit(1);
   }

   far = MRI_FLOAT_PTR(im);
   nvec = im->nx;
   ncol = im->ny;
   if (Opt->nodecol >= ncol || Opt->labelcol >= ncol) {
      fprintf(SUMA_STDERR, "\n"
                           "Error %s: Input file has a total of %d columns.\n"
                           "One or both user-specified node (%d) and \n"
                           "label (%d) columns are too high. Maximum usable\n"
                           "column index is %d.\n"
                           , FuncName, ncol, Opt->nodecol, 
                           Opt->labelcol, ncol -1 );
      exit(1);
   }
   
   d_order = SUMA_COLUMN_MAJOR;

   if (!nvec) {
      SUMA_SL_Err("Empty file");
      exit(1);
   }
   /* form the node vector */
   NodePatch = (int *)SUMA_malloc(sizeof(int)*nvec);
   if (!NodePatch) {
      SUMA_SL_Crit("Failed to allocate.");
      exit(1);
   }
   inodeoff = Opt->nodecol*nvec;
   if (Opt->labelcol < 0) { /* all listed nodes */ 
      for (i=0; i<nvec; ++i) {
         NodePatch[i] = far[i+inodeoff];
      }
      N_NodePatch = nvec;
   } else {
      ilabeloff =  Opt->labelcol*nvec;
      if (Opt->thislabel < 0) { /* all nodes with non zero labels */
         cnt = 0;
         for (i=0; i<nvec; ++i) {
            if (far[i+ilabeloff]) {
               NodePatch[cnt] = far[i+inodeoff];
               ++cnt;
            }
         }
         N_NodePatch = cnt;     
      } else { /* select labels */
         cnt = 0;
         for (i=0; i<nvec; ++i) {
            if (far[i+ilabeloff] == Opt->thislabel) {
               NodePatch[cnt] = far[i+inodeoff];
               ++cnt;
            }
         }
         N_NodePatch = cnt;    
      }
      NodePatch = (int *) SUMA_realloc(NodePatch , sizeof(int)*N_NodePatch);
   }
   
   /* done with im, free it */
   mri_free(im); im = NULL;   
   
   if (Opt->DoVol) {
      SUMA_SurfaceObject *SO1 = 
         SUMA_Load_Spec_Surf_with_Metrics(Spec, 0, ps->sv[0], 0);
      SUMA_SurfaceObject *SO2 = 
         SUMA_Load_Spec_Surf_with_Metrics(Spec, 1, ps->sv[0], 0);
      double Vol = 0.0;
      SUMA_SurfaceObject *SOp = SUMA_Alloc_SurfObject_Struct(1);
      byte *adj_N=NULL;
      
      if (Opt->adjust_contour) 
         adj_N = SUMA_calloc(SO1->N_Node, sizeof(byte));
      
      if (!SO1 || !SO2) {
         SUMA_SL_Err("Failed to load surfaces.");
         exit(1);
      }
      /* a chunk used to test SUMA_Pattie_Volume */
      Vol = SUMA_Pattie_Volume(SO1, SO2, NodePatch, N_NodePatch, 
                               SOp, Opt->minhits, 
                               Opt->FixBowTie, Opt->adjust_contour, 
                               adj_N, Opt->verb);
      fprintf (SUMA_STDOUT,"Volume = %f\n", fabs(Vol));
      if (Opt->out_volprefix) {
         if (Opt->oType != SUMA_FT_NOT_SPECIFIED) SOp->FileType = Opt->oType;
         if (Opt->flip) {
            if (Opt->verb > 1) 
               SUMA_S_Note("Flipping stitched surf's triangles\n");
            SUMA_FlipSOTriangles (SOp);
         }

         if (!(SUMA_Save_Surface_Object_Wrap ( Opt->out_volprefix, NULL,
                                               SOp, SUMA_PLY, SUMA_ASCII, 
                                               NULL))) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to write surface object.\n", FuncName);
         }
         if (Opt->adjust_contour && adj_N) {
            Opt->out_volprefix = 
                     SUMA_append_replace_string(Opt->out_volprefix, 
                                                   ".adjneighb","",1);
            ppref = SUMA_Extension(Opt->out_volprefix, ".1D.dset", NOPE);
            SUMA_WRITE_IND_ARRAY_1D(adj_N, NULL, SO1->N_Node, 1, ppref);
            SUMA_free(ppref); ppref=NULL;
         }
      }
      if (SOp) SUMA_Free_Surface_Object(SOp); SOp = NULL;
   }
   
   
   if (!Opt->VolOnly) {
      FaceSetList = NULL;
      N_FaceSet = -1;
      for (i=0; i < Spec->N_Surfs; ++i) {/* loop to read in surfaces */
         /* now identify surface needed */
         if (!(SO = SUMA_Load_Spec_Surf_with_Metrics(Spec, i, ps->sv[0], 0))) {
            SUMA_S_Err("Failed to load surface .\n");
            exit(1);
         }
         if (SO->aSO) {
            /* otherwise, when you reset the number of FaceSets for example,
               and you still write in GIFTI, the old contents of aSO will
               prevail */
            SO->aSO = SUMA_FreeAfniSurfaceObject(SO->aSO); 
         }
         /* extract the patch */
         ptch = SUMA_getPatch (NodePatch, N_NodePatch, SO->N_Node,
                               SO->FaceSetList,  SO->N_FaceSet, 
                               SO->MF, Opt->minhits, 
                               Opt->FixBowTie, (!i && !Opt->DoVol)); 
                                    /* verbose only for first patch, and 
                                    if no volume computation was required  
                                    This is to keep the warnings to a minimum*/
         if (!ptch) {
            SUMA_SL_Err("Failed to form patch.");
            exit(1);
         }
         if (LocalHead) SUMA_ShowPatch(ptch, NULL);
      
         /* Now create a surface with that patch */
         if (Spec->N_Surfs > 1) {
            sprintf(ext, "_%c", 65+i);
            ppref = SUMA_append_string(Opt->out_prefix, ext);
         } else {
            ppref = SUMA_copy_string(Opt->out_prefix);
         }
         /* save the original type */
         typetmp = SO->FileType;
         if (Opt->oType != SUMA_FT_NOT_SPECIFIED) SO->FileType = Opt->oType;
         SO_name = SUMA_Prefix2SurfaceName(ppref, NULL, NULL, 
                                           SO->FileType, &exists);
         if (ppref) SUMA_free(ppref); ppref = NULL;
         /* save the original pointers to the facesets and their number */
         FaceSetList = SO->FaceSetList;
         N_FaceSet = SO->N_FaceSet;
         NodeList = SO->NodeList;
         N_Node = SO->N_Node;
         
         /* replace with Patch */
         SO->FaceSetList = ptch->FaceSetList;
         SO->N_FaceSet = ptch->N_FaceSet; 
         
         if (Opt->Do_p2s) {
            if (LocalHead) 
               fprintf (SUMA_STDERR,
                        "%s: Changing patch to surface...\n", FuncName);
            SOnew = SUMA_Patch2Surf(SO->NodeList, SO->N_Node, 
                                    SO->FaceSetList, SO->N_FaceSet, 3);
            if (!SOnew) {
               SUMA_S_Err("Failed to change patch to surface.");
               exit(1);
            }
            SO->FaceSetList = SOnew->FaceSetList;
            SO->N_FaceSet = SOnew->N_FaceSet;
            SO->N_Node = SOnew->N_Node;
            SO->NodeList = SOnew->NodeList;
         }
          
         if (SO->N_FaceSet <= 0) {
            SUMA_S_Warn("The patch is empty.\n"
                        " Non existing surface not written to disk.\n");
         } else {
            /* Is the gain wanted? */
            if (Opt->coordgain) {
               SUMA_SL_Note("Applying coord gain to surface nodes!");
               for (cnt=0; cnt < SO->NodeDim*SO->N_Node; ++cnt) 
                  SO->NodeList[cnt] *= Opt->coordgain;
            }
            if (Opt->flip) {
               if (Opt->verb > 1) SUMA_S_Note("Flipping triangles\n");
               SUMA_FlipTriangles (SO->FaceSetList, SO->N_FaceSet);
               SUMA_RECOMPUTE_NORMALS(SO);
            }

            if (!SUMA_Save_Surface_Object (SO_name, SO, SO->FileType, 
                                           SUMA_ASCII, NULL)) {
                  fprintf (SUMA_STDERR,
                           "Error %s: Failed to write surface object.\n", 
                           FuncName);
                  exit (1);
            }
         }
         
         if (Opt->nodedepth) {
            float *dpth=NULL, mx=0.0;
            SUMA_LH("Writing node depths into %s...\n", Opt->nodedepth);
            SUMA_PC_XYZ_PROJ *pcp=NULL;
            if (SUMA_NodeDepth(SO->NodeList, SO->N_Node, E1_DIR_PRJ, &dpth, 
                               0.0, NULL, &mx, &pcp) < 0) {
               SUMA_S_Err("Failed to compute node depth");
               exit(1);
            } else {
               if (!SUMA_WriteNodeDepth(Opt->nodedepth,pcp,dpth, mx)) {
                  SUMA_S_Err("Failed to write node depth");
                  exit(1);
               } 
            }
            SUMA_ifree(dpth);
            pcp = SUMA_Free_PC_XYZ_Proj(pcp);
         }

         /* bring SO back to shape */
         SO->FileType = typetmp;
         SO->FaceSetList = FaceSetList; FaceSetList = NULL;
         SO->N_FaceSet = N_FaceSet; N_FaceSet = -1;
         SO->NodeList = NodeList; NodeList = NULL;
         SO->N_Node = N_Node; N_Node = -1;
         
         
         if (SO_name) SUMA_free(SO_name); SO_name = NULL;
         if (ptch) SUMA_freePatch(ptch); ptch = NULL;
         if (SOnew) SUMA_Free_Surface_Object(SOnew); SOnew = NULL; 
               /* get rid of old surface object */
            

      }
   } 
   
   SUMA_LH("clean up");
   if (!SUMA_FreeSpecFields(Spec)) { SUMA_S_Err("Failed to free spec fields"); }
   SUMA_free(Spec); Spec = NULL;
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt->out_volprefix) SUMA_free(Opt->out_volprefix); 
                                                Opt->out_volprefix = NULL;
   if (Opt) SUMA_free(Opt);   
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {
      SUMA_SL_Err("SUMAg_CF Cleanup Failed!");
   }
   
   SUMA_RETURN(0);
} 
