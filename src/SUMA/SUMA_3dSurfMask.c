#include "SUMA_suma.h"
#include "thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"

static char labels[7][64]={
                           "Out",
                           "Out, In box",
                           "Out, Touching",
                           "Contains Node",
                           "In, Touching",
                           "In, In Box",
                           "In" };
static char labels_slow[3][64]={
                           "Out",
                           "Out, In box",
                           "In" };
static int keys[7]={0, 1, 2, 3, 4, 5, 6};
static int N_labels = 7;
static int N_labels_slow = 3;

void usage_3dSurfMask (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_3dSurfMask"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
   "\n"
   "Usage: 3dSurfMask <-i_TYPE SURFACE> <-prefix PREFIX> \n"
   "                [<-fill_method METH>] \n"
   "                <-grid_parent GRID_VOL> [-sv SURF_VOL] [-mask_only]\n"
   " \n"
   "  Creates 2 volumetric datasets that mark voxel based on their\n"
   "  location relative to the surface.\n"
   "  Voxels in the first volume (named PREFIX.m) label voxel positions \n"
   "  relative to the surface. With -fill_method set to FAST, you get a \n"
   "  a CRUDE mask with voxel values set to the following:\n"
   "     0: Voxel outside surface\n"
   "     1: Voxel just outside the surface. This means the voxel\n"
   "        center is outside the surface but inside the \n"
   "        bounding box of a triangle in the mesh. \n"
   "     2: Voxel intersects the surface (a triangle), \n"
   "        but center lies outside.\n"
   "     3: Voxel contains a surface node.\n"
   "     4: Voxel intersects the surface (a triangle), \n"
   "        center lies inside surface. \n"
   "     5: Voxel just inside the surface. This means the voxel\n"
   "        center is inside the surface and inside the \n"
   "        bounding box of a triangle in the mesh. \n"
   "     6: Voxel inside the surface. \n"
   "  Masks obtained with -fill_method FAST could have holes in them.\n"
   "  To decide on whether a voxel lies inside or outside the surface\n"
   "  you should use the signed distances in PREFIX.d below, or use\n"
   "  -fill_method slow.\n"
   "\n"
   "  With -fill_method set to SLOW you get a better mask with voxels set\n"
   "  to the following:\n"
   "     0: Voxel outside surface\n"
   "     1: Voxel outside the surface but in its bounding box\n"
   "     2: Voxel inside the surface \n"
   "\n"
   "  Voxels values in the second volume (named PREFIX.d) reflect the \n"
   "  shortest distance of voxels in PREFIX.m to the surface.\n"
   "  The distances are signed to reflect whether a voxel is inside \n"
   "  or outsider the surface. Voxels inside the surface have positive\n"
   "  distances, voxels outside have a negative distance.\n"
   "  If the signs appear reversed, use option -flip_orientation.\n"
   "\n"
   "  Mandatory Parameters:\n"
   "     -i_TYPE SURFACE: Specify input surface.\n"
   "             You can also use -t* and -spec and -surf\n"
   "             methods to input surfaces. See below\n"
   "             for more details.\n"
   "     -prefix PREFIX: Prefix of output dataset.\n"
   "     -grid_parent GRID_VOL: Specifies the grid for the\n"
   "                  output volume."
   "\n"
   "  Other parameters:\n"
   "     -mask_only: Produce an output dataset where voxels\n"
   "                 are 1 inside the surface and 0 outside,\n"
   "                 instead of the more nuanced output above.\n"                    "     -flip_orientation: Flip triangle winding of surface mesh.\n"
   "                        Use this option when the sign of the distances\n"
   "                        in PREFIX.m comes out wrong. Voxels inside\n"
   "                        the surface have a positive distance.\n"
   "                        This can happen when the winding of the triangles\n"
   "                        is reversed.\n"
   "     -fill_method METH: METH can take two values; SLOW, and FAST[default].\n"
   "                        FAST can produce holes under certain conditions.\n"
   "     -no_dist: Do not compute the distances, just the mask from the first \n"
   "               step.\n"
   "\n"
   " Example: (tcsh syntax)\n"
   "  1- Find distance of voxels around and inside of toy surface:\n"
   "\n"
   "  echo 'Create toy data' \n"
   "    @auto_tlrc -base TT_N27+tlrc -base_copy ToyVolume \n"
   "\n"
   "    CreateIcosahedron -rad 50 -ld 1\n"
   "    sed 's/Anatomical = N/Anatomical = Y/' CreateIco.spec > __ttt \n"
   "    mv __ttt CreateIco.spec\n"
   "\n"
   "  echo 'Do computations'\n"
   "    3dSurfMask -i_fs CreateIco.asc -sv ToyVolume+tlrc \\\n"
   "                -prefix ToyMasks -flip_orientation   \\\n"
   "                -grid_parent ToyVolume+tlrc \n"
   "\n"
   "  echo 'Cut and paste commands below to show you the results'\n"
   "    suma -npb 70 -niml -spec CreateIco.spec -sv ToyVolume+tlrc &\n"
   "    afni -npb 70 -niml -yesplugouts & \n"
   "    DriveSuma -npb 70 -com viewer_cont -key 't'  \n"
   "    plugout_drive -npb 70 -com 'SET_OVERLAY A ToyMasks.d' \\\n"
   "                          -com 'SET_THRESHOLD A.0'  \\\n"
   "                          -com 'SET_PBAR_NUMBER A.10'  \\\n"
   "                          -quit \n"
   "\n"
   " See also examples in SurfPatch -help\n"
   "\n"              
   "%s"
   "%s"
            "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_3dSurfMask_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_3dSurfMask_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->obj_type = 1; /* nuanced integer values in output */
   Opt->b1 = 0;
   Opt->b2 = 0;
   Opt->MaskMode = 0;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_3dSurfMask(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -prefix \n");
            exit (1);
         }
         Opt->out_vol_prefix = 
            SUMA_AfniPrefix(argv[++kar], Opt->out_vol_view, 
                            NULL, &(Opt->out_vol_exists));
         
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-fill_method") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -fill_method \n");
            exit (1);
         }
         ++kar;
         if (!strcmp(argv[kar],"SLOW") ||
             !strcmp(argv[kar],"Slow") ||
             !strcmp(argv[kar],"slow")) {
            Opt->b2 = 1;
         } else if (!strcmp(argv[kar],"FAST") ||
             !strcmp(argv[kar],"Fast") ||
             !strcmp(argv[kar],"fast")) {
            Opt->b2 = 0;
         } else {
            SUMA_S_Errv("Value %s not ok for -fill_method\n", argv[kar]); 
            exit(1);
         }
         
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-grid_parent") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a dataset after -grid_parent \n");
            exit (1);
         }
         Opt->out_grid_prefix = SUMA_AfniPrefix(argv[++kar], Opt->out_grid_view, 
                                                NULL, &(Opt->out_grid_exists));
         if (!SUMA_AfniExistsView(Opt->out_grid_exists, Opt->out_grid_view)) {
            fprintf(SUMA_STDERR, "Error 3dSurfMask:\n"
                                 "Grid parent %s%s does not exist.\n", 
                                 Opt->out_grid_prefix, Opt->out_grid_view);
            exit(1);
         }
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-mask_only") == 0))
      {
         Opt->obj_type = 2;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-no_dist") == 0))
      {
         Opt->MaskMode = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-flip_orientation") == 0))
      {
         Opt->b1 = 1;
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
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,
                  "Error 3dSurfMask: Option %s not understood\n", argv[kar]);
         suggest_best_prog_option(argv[0], argv[kar]);
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
   static char FuncName[]={"3dSurfMask"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec=0;
   short *isin = NULL;
   int N_in = 0, i;
   SUMA_FORM_AFNI_DSET_STRUCT *OptDs = NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_VOLPAR *vp = NULL;
   THD_3dim_dataset *dset = NULL, *dsetd=NULL;
   char *vpname=NULL;
   SUMA_FileName NewName;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-s;-sv;");
   
   if (argc < 2) {
      usage_3dSurfMask(ps);
      exit (1);
   }
   
   Opt = SUMA_3dSurfMask_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   /* check on inputs */
   if (ps->s_N_surfnames + ps->i_N_surfnames + ps->t_N_surfnames != 1) {
      SUMA_S_Err("Multiple surface specifications used. "
                 "Only one surface allowed.");
      exit(1);
   }

   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }
   if (N_Spec != 1) {
      SUMA_S_Err("Multiple spec at input.");
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
   if (Opt->b1) SUMA_FlipSOTriangles(SO);
   
   if (Opt->out_grid_prefix) {
      vpname = SUMA_append_string(Opt->out_grid_prefix, Opt->out_grid_view);
      vp = SUMA_VolPar_Attr(vpname);
   } else { 
      vp = SO->VolPar;
      if (!vp) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Need a grid parent.\n",
                                 FuncName );
            exit(1);
      }
      vpname = SUMA_copy_string(ps->sv[0]);
      if (!SUMA_AfniView(ps->sv[0], Opt->out_grid_view)) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Failed to get view!!!\n",
                                 FuncName );
            exit(1);
         
      }
   }
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Using %s for grid\n", FuncName, vpname);
   }
   /* some checks ...*/
   if (!Opt->out_vol_prefix) { 
      Opt->out_vol_prefix = 
         SUMA_AfniPrefix("3dSurfMask", NULL, NULL, &(Opt->out_vol_exists)); 
      strncpy(Opt->out_vol_view, Opt->out_grid_view, SUMA_VIEW_LENGTH);
   }
   
   if (SUMA_AfniExistsView(Opt->out_vol_exists, Opt->out_vol_view)) {
      fprintf(SUMA_STDERR, "Error 3dSurfMask:\nOutput volume %s%s exists.\n", 
               Opt->out_vol_prefix, Opt->out_vol_view);
      exit(1);
   }
   
   if (Opt->out_grid_prefix) {
      if (!SUMA_AfniExistsView(Opt->out_grid_exists, Opt->out_grid_view)) {
         fprintf(SUMA_STDERR, 
                  "Error 3dSurfMask:\nGrid parent %s%s does not exist.\n", 
                  Opt->out_grid_prefix, Opt->out_grid_view);
         exit(1);
      }
   }

   
   if (!Opt->b2) {
      isin = SUMA_FindVoxelsInSurface (SO, vp, &N_in, 1, NULL);
   } else {
      isin = SUMA_FindVoxelsInSurface_SLOW (SO, vp, &N_in, 0);
   }
   if (!isin) {
      SUMA_S_Err("No voxels in surface");
      exit(1);
   }
   OptDs = SUMA_New_FormAfniDset_Opt();
   NewName = SUMA_StripPath(Opt->out_vol_prefix);
   OptDs->prefix = SUMA_append_string(NewName.FileName,".m"); 
   OptDs->prefix_path = SUMA_copy_string(NewName.Path); 
   
   
   /* master dset */
   OptDs->master = SUMA_copy_string(vpname);
   OptDs->datum = MRI_byte;
   OptDs->full_list = 1;
   { float * isin_float = NULL;
      isin_float = (float *)SUMA_malloc(sizeof(float)*vp->nx*vp->ny*vp->nz);
      if (!isin_float) {
         SUMA_SL_Crit("Failed to allocate");
         exit(1);
      }
      
      if (Opt->obj_type == 2) {
         for (i=0; i<vp->nx*vp->ny*vp->nz; ++i) { 
            if (isin[i] > 1) isin_float[i] = 1.0; 
            else isin_float[i] = 0.0; 
         }                               
      } else {
         for (i=0; i<vp->nx*vp->ny*vp->nz; ++i) isin_float[i] = (float)isin[i];
      }
      dset = SUMA_FormAfnidset (NULL, isin_float, vp->nx*vp->ny*vp->nz, OptDs);
      if (!dset) {
         SUMA_SL_Err("Failed to create output dataset!");
      } else {
         char **lblv = NULL;
         if (!Opt->b2) {
            lblv = (char **)SUMA_calloc(N_labels, sizeof(char*));
            for (i=0; i<N_labels; ++i) lblv[i] = SUMA_copy_string(labels[i]);
            if (!SUMA_SetDsetLabeltable(dset, lblv, N_labels, keys)) { 
               SUMA_S_Err("Failed to add labels");
            }
            for (i=0; i<N_labels; ++i) SUMA_free(lblv[i]);
         } else {
            lblv = (char **)SUMA_calloc(N_labels_slow, sizeof(char*));
            for (i=0; i<N_labels_slow; ++i) 
               lblv[i] = SUMA_copy_string(labels_slow[i]);
            if (!SUMA_SetDsetLabeltable(dset, lblv, N_labels_slow, keys)) { 
               SUMA_S_Err("Failed to add labels");
            }
            for (i=0; i<N_labels_slow; ++i) SUMA_free(lblv[i]);
         }
         SUMA_free(lblv); lblv=NULL;   
         tross_Make_History( FuncName , argc,argv , dset ) ;
         DSET_write(dset) ;
      }
      SUMA_free(isin_float); isin_float = NULL;
   }

   if (!Opt->MaskMode) {
      SUMA_S_Note("Voxelizing...");
      dsetd = SUMA_VoxelToSurfDistances(SO, dset, NULL, isin, Opt->b2 ? 2:0);
      tross_Make_History( FuncName , argc,argv , dsetd ) ;
      OptDs->prefix = SUMA_append_string(NewName.FileName, ".d");
      EDIT_dset_items( dsetd ,
                       ADN_prefix    , OptDs->prefix ,
                       ADN_directory_name , OptDs->prefix_path,
                       ADN_none);
      DSET_write(dsetd);
   }                 
   if (vpname) SUMA_free(vpname); vpname = NULL;
   if (dset) { DSET_delete(dset); dset = NULL; }
   if (dsetd) { DSET_delete(dsetd); dsetd = NULL; }
   if (OptDs) { OptDs->mset = NULL; OptDs = SUMA_Free_FormAfniDset_Opt(OptDs);  }
   if (vp != SO->VolPar) SUMA_Free_VolPar(vp); vp = NULL;
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
