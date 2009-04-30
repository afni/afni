#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_3dSurfMask (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_3dSurfMask"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
   "\n"
         "Usage: 3dSurfMask <-i_TYPE SURFACE> <-prefix PREFIX>\n"
         "                <-grid_parent GRID_VOL> [-sv SURF_VOL] [-mask_only]\n"
         " \n"
         "  Creates a volumetric dataset that marks the inside\n"
         "    of the surface."
         "  Voxels in the output dataset are set to the following\n"
         "  values:\n"
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
         "                 instead of the more nuanced output above.\n"                 
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
            fprintf(SUMA_STDERR, "Error 3dSurfMask:\nGrid parent %s%s does not exist.\n", Opt->out_grid_prefix, Opt->out_grid_view);
            exit(1);
         }
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-mask_only") == 0))
      {
         Opt->obj_type = 2;
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
			fprintf (SUMA_STDERR,"Error 3dSurfMask:\nOption %s not understood. Try -help for usage\n", argv[kar]);
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
   THD_3dim_dataset *dset = NULL;
   char *vpname=NULL;
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
      Opt->out_vol_prefix = SUMA_AfniPrefix("3dSurfMask", NULL, NULL, &(Opt->out_vol_exists)); 
      sprintf(Opt->out_vol_view, Opt->out_grid_view); 
   }
   
   if (SUMA_AfniExistsView(Opt->out_vol_exists, Opt->out_vol_view)) {
      fprintf(SUMA_STDERR, "Error 3dSurfMask:\nOutput volume %s%s exists.\n", Opt->out_vol_prefix, Opt->out_vol_view);
      exit(1);
   }
   
   if (Opt->out_grid_prefix) {
      if (!SUMA_AfniExistsView(Opt->out_grid_exists, Opt->out_grid_view)) {
         fprintf(SUMA_STDERR, "Error 3dSurfMask:\nGrid parent %s%s does not exist.\n", Opt->out_grid_prefix, Opt->out_grid_view);
         exit(1);
      }
   }

   
   isin = SUMA_FindVoxelsInSurface (SO, vp, &N_in, 0, NULL);
   if (!isin) {
      SUMA_S_Err("No voxels in surface");
      exit(1);
   }
   OptDs = SUMA_New_FormAfniDset_Opt();
   if (Opt->out_vol_prefix) {
      SUMA_FileName NewName = SUMA_StripPath(Opt->out_vol_prefix);
      OptDs->prefix = NewName.FileName; NewName.FileName = NULL;
      OptDs->prefix_path = NewName.Path; NewName.Path = NULL;
   }  else {
      OptDs->prefix = SUMA_copy_string("3dSurfMask");
      OptDs->prefix_path = SUMA_copy_string("./");
   }
   
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
      
      if (Opt->obj_type == 2) for (i=0; i<vp->nx*vp->ny*vp->nz; ++i) { if (isin[i] > 1) isin_float[i] = 1.0; else isin_float[i] = 0.0; }                               
      else {
         for (i=0; i<vp->nx*vp->ny*vp->nz; ++i) isin_float[i] = (float)isin[i];
      }
      dset = SUMA_FormAfnidset (NULL, isin_float, vp->nx*vp->ny*vp->nz, OptDs);
      if (!dset) {
         SUMA_SL_Err("Failed to create output dataset!");
      } else {
         tross_Make_History( FuncName , argc,argv , dset ) ;
         DSET_write(dset) ;
      }
      SUMA_free(isin_float); isin_float = NULL;
   }

   if (vpname) SUMA_free(vpname); vpname = NULL;
   if (dset) { DSET_delete(dset); dset = NULL; }
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
