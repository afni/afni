#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

#define SURFPATCH_MAX_SURF 10  /*!< Maximum number of input surfaces */

void usage_SUMA_getPatch ()
   {
      static char FuncName[]={"usage_SUMA_getPatch"};
      char * s = NULL;
      s = SUMA_help_basics();
      printf ( "\nUsage:\n"
               "  SurfPatch <-spec SpecFile> <-surf_A insurf> <-surf_B insurf> ...\n"
               "            <-input nodefile inode ilabel> <-prefix outpref>  \n"
               "            [-hits min_hits] [-masklabel msk] [-vol]\n"
               "\n"
               "Usage 1:\n"
               "  The program creates a patch of surface formed by nodes \n"
               "  in nodefile.\n"
               "  Mandatory parameters:\n"
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
               "                      using -out_type option. This way checking for pre-existing\n"
               "                      output files can be done at the outset.\n"
               "\n" 
               "  Optional parameters:\n"
               "     -coord_gain GAIN: Multiply node coordinates by a GAIN.\n"
               "                       That's useful if you have a tiny patch that needs\n"
               "                       enlargement for easier viewing in SUMA.\n"
               "                       Although you can zoon over very large ranges in SUMA\n"
               "                       tiny tiny patches are hard to work with because\n"
               "                       SUMA's parameters are optimized to work with objects\n"
               "                       on the order of a brain, not on the order of 1 mm.\n"
               "                       WARNING: Do not use this option if you are measuring\n"
               "                       the volume of a patch!\n"
               "     -out_type TYPE: Type of all output patches, regardless of input surface type.\n"
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
               "\n"
               "Usage 2:\n"
               "  The program can also be used to calculate the volume between the same patch\n"
               "  on two isotopic surfaces. See -vol option below.\n"
               "      -vol: Calculate the volume formed by the patch on surf_A and\n"
               "            and surf_B. For this option, you must specify two and\n"
               "            only two surfaces with surf_A and surf_B options.\n"
               "      -vol_only: Only calculate the volume, don't write out patches.\n"
               "\n"
               "%s"
               "\n",s); SUMA_free(s); s = NULL;
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
       exit (0);
   }

typedef struct {
   SUMA_SO_File_Type iType;
   SUMA_SO_File_Type oType;
   char *out_prefix;
   char *sv_name;
   char *surf_names[SURFPATCH_MAX_SURF];
   int N_surf;
   char *spec_file;
   char *in_name;
   int minhits;
   int thislabel;
   int labelcol;
   int nodecol;
   int DoVol;
   int VolOnly;
   float coordgain;
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
SUMA_GETPATCH_OPTIONS *SUMA_GetPatch_ParseInput (char *argv[], int argc)
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
   Opt->sv_name = NULL;
   Opt->spec_file = NULL;
   Opt->in_name = NULL;
   Opt->minhits = 2;
   Opt->labelcol = -1;
   Opt->nodecol = -1;
   Opt->thislabel = -1;
   Opt->N_surf = -1;
   Opt->DoVol = 0;
   Opt->VolOnly = 0;
   Opt->coordgain = 0.0;
   Opt->oType = SUMA_FT_NOT_SPECIFIED;
   for (i=0; i<SURFPATCH_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
	brk = NOPE;
   
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_getPatch();
          exit (0);
		}
		
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec \n");
				exit (1);
			}
			Opt->spec_file = argv[kar];
			brk = YUP;
		}
      
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
      
      if (!brk && (strcmp(argv[kar], "-vol") == 0)) {
			Opt->DoVol = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-vol_only") == 0)) {
			Opt->DoVol = 1;
         Opt->VolOnly = 1;
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
      
      if (!brk && (strcmp(argv[kar], "-out_type") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -out_type \n");
				exit (1);
			}
			Opt->oType = SUMA_guess_surftype_argv(argv[kar]);
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
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
      
   }
   
   /* sanity checks */
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("SurfPatch");
   
   if (Opt->thislabel >= 0 && Opt->labelcol < 0) {
      SUMA_SL_Err("Cannot use -masklabel without specifying ilabel in -input option");
      exit(1);
   } 
   if (Opt->minhits < 1 || Opt->minhits > 3) {
      SUMA_SL_Err("minhits must be > 0 and < 3");
      exit(1);
   }
   if (Opt->N_surf < 1) {
      SUMA_SL_Err("No surface specified.");
      exit(1);
   }
   if (!Opt->in_name) {
      SUMA_SL_Err("No input specified.");
      exit(1);
   }
   if (Opt->DoVol && Opt->N_surf != 2) {
      SUMA_SL_Err("Must specify 2 and only 2 surfaces with -vol options");
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
   int *NodePatch=NULL, N_NodePatch=-1, *FaceSetList=NULL , N_FaceSet = -1;          
   int i, inodeoff=-1, ilabeloff=-1, nvec, ncol, cnt;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_PATCH *ptch = NULL; 
   SUMA_SurfSpecFile Spec;
   SUMA_INDEXING_ORDER d_order;
   void *SO_name = NULL;
   SUMA_Boolean exists = NOPE;
   SUMA_SO_File_Type typetmp;
   SUMA_Boolean LocalHead = NOPE;
	
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   if (argc < 4)
       {
          usage_SUMA_getPatch();
          exit (1);
       }
   
   Opt = SUMA_GetPatch_ParseInput (argv, argc);
   
   if (Opt->oType != SUMA_FT_NOT_SPECIFIED && !Opt->VolOnly) { 
      for (i=0; i < Opt->N_surf; ++i) {
         if (Opt->N_surf > 1) {
            sprintf(ext, "_%c", 65+i);
            ppref = SUMA_append_string(Opt->out_prefix, ext);
         } else {
            ppref = SUMA_copy_string(Opt->out_prefix);
         }
         
         SO_name = SUMA_Prefix2SurfaceName(ppref, NULL, NULL, Opt->oType, &exists);
         if (exists) {
            fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, ppref);
            exit(1);
         }
         if (ppref) SUMA_free(ppref); ppref = NULL; 
         if (SO_name) SUMA_free(SO_name); SO_name = NULL;
      } 
   }
   
   /* read all surfaces */
   if (!SUMA_AllocSpecFields(&Spec)) { SUMA_S_Err("Failed to initialize spec fields."); exit(1); }
   if (!SUMA_Read_SpecFile (Opt->spec_file, &Spec)) {
		fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
		exit(1);
	}
   SO_read = SUMA_spec_select_surfs(&Spec, Opt->surf_names, SURFPATCH_MAX_SURF, 0);
   if ( SO_read != Opt->N_surf )
   {
	   if (SO_read >=0 )
         fprintf(SUMA_STDERR,"Error %s:\nFound %d surfaces, expected %d.\n", FuncName,  SO_read, Opt->N_surf);
      exit(1);
   }
   /* now read into SUMAg_DOv */
   if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, Opt->sv_name, 0, SUMAg_CF->DsetList) ) {
	   fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
      exit(1);
   }
   
   /* read in the file containing the node information */
   im = mri_read_1D (Opt->in_name);

   if (!im) {
      SUMA_SL_Err("Failed to read 1D file");
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
                           , FuncName, ncol, Opt->nodecol, Opt->labelcol, ncol -1 );
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
      SUMA_SurfaceObject *SO1 = SUMA_find_named_SOp_inDOv(Opt->surf_names[0], SUMAg_DOv, SUMAg_N_DOv);
      SUMA_SurfaceObject *SO2 = SUMA_find_named_SOp_inDOv(Opt->surf_names[1], SUMAg_DOv, SUMAg_N_DOv);
      double Vol = 0.0;
      
      if (!SO1 || !SO2) {
         SUMA_SL_Err("Failed to load surfaces.");
         exit(1);
      }
      /* a chunk used to test SUMA_Pattie_Volume */
      Vol = SUMA_Pattie_Volume(SO1, SO2, NodePatch, N_NodePatch, NULL, Opt->minhits);
      fprintf (SUMA_STDERR,"Volume = %f\n", fabs(Vol));
   }
   
   
   if (!Opt->VolOnly) {
      FaceSetList = NULL;
      N_FaceSet = -1;
      for (i=0; i < Opt->N_surf; ++i) {/* loop to read in surfaces */
         /* now identify surface needed */
         SO = SUMA_find_named_SOp_inDOv(Opt->surf_names[i], SUMAg_DOv, SUMAg_N_DOv);
         if (!SO) {
            fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Failed to find surface %s\n"
                                 "in spec file. Use full name.\n",
                                 FuncName, Opt->surf_names[i]);
            exit(1);
         }
         /* extract the patch */
         if (!SO->MF) {
            SUMA_SL_Warn ("NULL MF");
         }
         ptch = SUMA_getPatch (NodePatch, N_NodePatch, SO->FaceSetList, SO->N_FaceSet, SO->MF, Opt->minhits);
         if (!ptch) {
            SUMA_SL_Err("Failed to form patch.");
            exit(1);
         }
         if (LocalHead) SUMA_ShowPatch(ptch, NULL);
      
         /* Now create a surface with that patch */
         if (Opt->N_surf > 1) {
            sprintf(ext, "_%c", 65+i);
            ppref = SUMA_append_string(Opt->out_prefix, ext);
         } else {
            ppref = SUMA_copy_string(Opt->out_prefix);
         }
         /* save the original type */
         typetmp = SO->FileType;
         if (Opt->oType != SUMA_FT_NOT_SPECIFIED) SO->FileType = Opt->oType;
         SO_name = SUMA_Prefix2SurfaceName(ppref, NULL, NULL, SO->FileType, &exists);
         if (ppref) SUMA_free(ppref); ppref = NULL;
         /* save the original pointers to the facesets and their number */
         FaceSetList = SO->FaceSetList;
         N_FaceSet = SO->N_FaceSet;
         
         /* replace with Patch */
         SO->FaceSetList = ptch->FaceSetList;
         SO->N_FaceSet = ptch->N_FaceSet; 
         
         if (SO->N_FaceSet <= 0) {
            SUMA_S_Warn("The patch is empty.\n Non existing surface not written to disk.\n");
         } else {
            /* Is the gain wanted? */
            if (Opt->coordgain) {
               SUMA_SL_Note("Applying coord gain to surface nodes!");
               for (cnt=0; cnt < SO->NodeDim*SO->N_Node; ++cnt) SO->NodeList[cnt] *= Opt->coordgain;
            }
            if (!SUMA_Save_Surface_Object (SO_name, SO, SO->FileType, SUMA_ASCII, NULL)) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
                  exit (1);
            }
         }
         /* bring SO back to shape */
         SO->FileType = typetmp;
         SO->FaceSetList = FaceSetList; FaceSetList = NULL;
         SO->N_FaceSet = N_FaceSet; N_FaceSet = -1;
         if (SO_name) SUMA_free(SO_name); SO_name = NULL;
         if (ptch) SUMA_freePatch(ptch); ptch = NULL;
      }
   } 
   
   SUMA_LH("clean up");
   if (!SUMA_FreeSpecFields(&Spec)) { SUMA_S_Err("Failed to free spec fields"); }
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt) SUMA_free(Opt);   
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {SUMA_SL_Err("SUMAg_CF Cleanup Failed!");}
   
   SUMA_RETURN(0);
} 
