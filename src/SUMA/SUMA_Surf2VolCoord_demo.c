#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

static char help_msg[]= {
"Once you load a surface and its surface volume,,\n"
"its node coordinates are transformed based on the\n"
"surface format type and the transforms stored in\n"
"the surface volume. At this stage, the node coordinates\n"
"are in what we call RAImm DICOM where x coordinate is\n"
"from right (negative) to left (positive) and y coordinate\n"
"from anterior to posterior and z from inferior to superior\n"
"This RAI coordinate corresponds to the mm coordinates\n"
"displayed by AFNI in the top left corner of the controller\n"
"when you have RAI=DICOM order set (right click on coordinate\n"
"text are to see option. When you open the surface with the\n"
"same sv in SUMA and view the sv volume in AFNI, the coordinate\n"
"of a node on an anatomically correct surface should be close\n"
"to the coordinate displayed in AFNI.\n"
"In the output, RAImm is the coordinate just described for a \n"
"particular node.\n"
"The next coordinate in the output is called 3dfind, which stands\n"
"for three dimensional float index. 3dfind is a transformation \n"
"of the RAImm coordinates to a coordinate in the units of the\n"
"voxel grid. The voxel with the closest center to a location\n"
"at RAImm would then be at round(3dfind). In other terms, \n"
"RAImm is the coordinate closest to voxel  \n"
" V(round(3dfind[0]), round(3dfind[1]), round(3dfind[2])\n"
"To see index coordinates, rather than mm coordinates in \n"
"AFNI, set: Define Datamode --> Misc --> Voxel Coords?\n"
"Note that the index coordinates would be different for the\n"
"underlay and overlay because they are usually at different\n"
"resolution and/or orientation. To see the overlay coordinates\n"
"make sure you have 'See Overlay' turned on.\n"
"The last value in the output is the value from the chosen\n"
"sub-brick\n"
};

void usage_Surf2VolCoord_demo (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_Surf2VolCoord_demo"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: Surf2VolCoord_demo <-i_TYPE SURFACE> \n"
               "                      <-grid_parent GRID_VOL> \n"
               "                      [-grid_subbrick GSB]\n"
               "                      [-sv SURF_VOL] \n"
               "                      [-one_node NODE]\n"
               " \n"
               "  Illustrates how surface coordinates relate to voxel grid."
               "  The program outputs surface and equivalent volume coordinates\n"
               "  for all nodes in the surface after it is aligned via its sv.\n"
               "  The code is intended as a source code demo.\n"
               "\n"
               "  Mandatory Parameters:\n"
               "     -i_TYPE SURFACE: Specify input surface.\n"
               "             You can also use -t* and -spec and -surf\n"
               "             methods to input surfaces. See below\n"
               "             for more details.\n"
               "     -prefix PREFIX: Prefix of output dataset.\n"
               "     -grid_parent GRID_VOL: Specifies the grid for the\n"
               "                  output volume.\n"
               "  Optional Parameters:\n"
               "     -grid_subbrick GSB: Sub-brick from which data are taken.\n"
               "     -one_node NODE: Output results for node NODE only.\n"
               "\n"
               "The output is lots of text so you're better off\n"
               "redirecting to a file.\n"
               "%s"                 
               "\n"                    
               "%s"
               "%s"
               "\n", help_msg, sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_Surf2VolCoord_demo_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_Surf2VolCoord_demo_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->obj_type = 0; /* sub-brick index */
   Opt->NodeDbg = -1;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_Surf2VolCoord_demo(ps);
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
         Opt->out_vol_prefix = SUMA_AfniPrefix(argv[++kar], Opt->out_vol_view, NULL, &(Opt->out_vol_exists));
         
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-grid_parent") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a dataset after -grid_parent \n");
            exit (1);
         }
         Opt->out_grid_prefix = SUMA_AfniPrefix(argv[++kar], Opt->out_grid_view, NULL, &(Opt->out_grid_exists));
         if (!SUMA_AfniExistsView(Opt->out_grid_exists, Opt->out_grid_view)) {
            fprintf(SUMA_STDERR, "Error Surf2VolCoord_demo:\nGrid parent %s%s does not exist.\n", Opt->out_grid_prefix, Opt->out_grid_view);
            exit(1);
         }
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-grid_subbrick") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -grid_subbrick \n");
            exit (1);
         }
         Opt->obj_type = atoi(argv[++kar]);
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
      if (!brk && (strcmp(argv[kar], "-one_node") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -one_node \n");
            exit (1);
         }
         
         Opt->NodeDbg = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,
                  "Error Surf2VolCoord_demo:\n"
                  "Option %s not understood. Try -help for usage\n", argv[kar]);
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
   static char FuncName[]={"Surf2VolCoord_demo"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec=0;
   short *isin = NULL;
   int N_in = 0, i;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_VOLPAR *vp = NULL;
   THD_3dim_dataset *dset = NULL;
   char *vpname=NULL;
   SUMA_Boolean LocalHead = NOPE;
   double *dvec = NULL;
   float *tmpXYZ=NULL;
   int di, dj, dk, dijk, nx, ny, nxy, i0, i1;
   float fi, fj, fk;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-sv;");
   
   if (argc < 2) {
      usage_Surf2VolCoord_demo(ps);
      exit (1);
   }
   
   Opt = SUMA_Surf2VolCoord_demo_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   /* some checks ...*/
   if (!Opt->out_vol_prefix) { 
      Opt->out_vol_prefix = SUMA_AfniPrefix("Surf2VolCoord_demo", NULL, NULL, &(Opt->out_vol_exists)); 
      sprintf(Opt->out_vol_view, Opt->out_grid_view); 
   }
   
   if (SUMA_AfniExistsView(Opt->out_vol_exists, Opt->out_vol_view)) {
      fprintf(SUMA_STDERR, "Error Surf2VolCoord_demo:\nOutput volume %s%s exists.\n", Opt->out_vol_prefix, Opt->out_vol_view);
      exit(1);
   }
   
   if (Opt->out_grid_prefix) {
      if (!SUMA_AfniExistsView(Opt->out_grid_exists, Opt->out_grid_view)) {
         fprintf(SUMA_STDERR, "Error Surf2VolCoord_demo:\nGrid parent %s%s does not exist.\n", Opt->out_grid_prefix, Opt->out_grid_view);
         exit(1);
      }
   }

   /* check on inputs */
   if (ps->s_N_surfnames + ps->i_N_surfnames + ps->t_N_surfnames != 1) {
      SUMA_S_Err("Multiple surface specifications used. Only one surface allowed.");
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
   if (Opt->NodeDbg >= 0 && Opt->NodeDbg >= SO->N_Node) {
      fprintf (SUMA_STDERR,"Error %s:\n"
                              "Node index %d is >= SO->N_Node (%d)\n"
                              "\n",
                              FuncName, Opt->NodeDbg, SO->N_Node );
         exit(1);
   }
   
   /* By now SO is the surface object whose coordinates have transformed
   so that it is in register with the surface volume specifed on command line.
   */
   
   /* Now let us read the volume from which you would be accessing values.
   That volume should be in alignment with -sv but not at the same resolution.
   I assume that this volume contains one sub-brick for simplicity. If no such 
   volume is specified then we'll use the -sv volume for that */
   
   if (Opt->out_grid_prefix) {
      vpname = SUMA_append_string(Opt->out_grid_prefix, Opt->out_grid_view);
      vp = SUMA_VolPar_Attr(vpname);
      dset = THD_open_dataset( vpname );
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
      dset = THD_open_dataset( vpname );
   }
   /* load .BRIK into memory */   
   DSET_load(dset);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Using %s for grid\n", FuncName, vpname);
   }
   if (Opt->obj_type >= DSET_NVALS(dset)) {
      fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Grid dset has a total of %d sub-bricks\n"
                                 " user specified sb#%d!!!\n",
                                 FuncName, DSET_NVALS(dset), Opt->obj_type );
            exit(1);
   }
   /* transform the surface's coordinates from RAImm to 3dfind 
      RAI coordinates are the mm coordinates you see displayed in AFNI's top left corner 
      3dfind are the equivalent coordinates expressed in index coordinates on the grid volume 
      Say you have a node at 13.4, 23, -54 mm (RAI)
      Its 3dfind coordinate might be: 3.2 , 56.1,   124.8 which would be closest to 
      voxel with ijk indices 3, 56, 125 in the volume defined by grid parent (dset or vp).
      To get that voxel's value, you just access element 3,56, 125. 
      Note that in this example, I am getting voxel values for all the coords in vector tmpXYZ 
      and those happen to be all the nodes of the surface. But you could put whatever coordinates
      or subset of coordinates you want in there. Just be sure to change SO->N_Node to reflect
      the number of triplets in tmpXYZ   
   */
      
   /* copy surface coordinates to preserve them, we're going to ijk land */
   tmpXYZ = (float *)SUMA_malloc(SO->N_Node * 3 * sizeof(float));
   if (!tmpXYZ) {
      SUMA_SL_Crit("Faile to allocate");
      exit(1);
   }
   memcpy ((void*)tmpXYZ, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
   if (!SUMA_vec_dicomm_to_3dfind (tmpXYZ, SO->N_Node, vp)) {
      SUMA_SL_Err("Failed to effectuate coordinate transform.");
      SUMA_free(tmpXYZ); tmpXYZ = NULL;
      exit(1);
   }

   /* Now, let us loop through all the coordinates of interest and see what
   voxels they fall in and what values are at those voxels. */
   
   /* first let us load the data from one sub-brick into a double vector
      (recall that data can be stored in variety of precisions on disk). */
   dvec = (double *)SUMA_malloc(sizeof(double) * DSET_NVOX(dset));
   if (!dvec) {
      SUMA_S_Errv("Failed to allocate for %d dvec.\nOh misery.\n", DSET_NVOX(dset));
      exit(1);
   }
   EDIT_coerce_scale_type( DSET_NVOX(dset) , 
                           DSET_BRICK_FACTOR(dset,Opt->obj_type) ,
                           DSET_BRICK_TYPE(dset,Opt->obj_type), 
                           DSET_ARRAY(dset, Opt->obj_type) ,      /* input  */
                           MRI_double               , dvec  ) ;   /* output */
   
   nx = DSET_NX(dset); ny = DSET_NY(dset); nxy = nx * ny; 
   if (Opt->NodeDbg >= 0) { i0 = Opt->NodeDbg; i1 = Opt->NodeDbg+1; }
   else { i0 = 0; i1 = SO->N_Node;};
   for (i=i0; i<i1; ++i) {
      fi = tmpXYZ[3*i  ]; di = SUMA_ROUND(fi);
      fj = tmpXYZ[3*i+1]; dj = SUMA_ROUND(fj);
      fk = tmpXYZ[3*i+2]; dk = SUMA_ROUND(fk);
      dijk = SUMA_3D_2_1D_index(di, dj, dk, nx, nxy);
      fprintf(SUMA_STDOUT, 
               "Node Index %d: RAImm %.3f %.3f %.3f : 3dfind %.1f %.1f %.1f : 3dind %d %d %d : Val %f\n",
                i,
                SO->NodeList[3*i], SO->NodeList[3*i+1], SO->NodeList[3*i+2], 
                fi, fj, fk, di, dj, dk,
                dvec[dijk]);
      
   }
   SUMA_free(dvec); dvec = NULL; 
   SUMA_free(tmpXYZ); tmpXYZ = NULL;
   /* no need for data in input volume anymore */
   PURGE_DSET(dset);
   

   if (vpname) SUMA_free(vpname); vpname = NULL;
   if (dset) { DSET_delete(dset); dset = NULL; }
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
