#include "SUMA_suma.h"

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

void usage_Surf2VolCoord_demo (SUMA_GENERIC_ARGV_PARSE *ps, int detail)
{
      static char FuncName[]={"usage_Surf2VolCoord_demo"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
"Usage: Surf2VolCoord <-i_TYPE SURFACE> \n"
"                      <-grid_parent GRID_VOL> \n"
"                      [-grid_subbrick GSB]\n"
"                      [-sv SURF_VOL] \n"
"                      [-one_node NODE]\n"
"                      [-closest_nodes XYZ.1D]\n"
" \n"
"  Relates node indices to coordinates:\n"
"  ------------------------------------\n"
"  Given x y z coordinates, return the nodes closest to them.\n"
"  For example:\n"
"      Surf2VolCoord    -i SUMA/std60.lh.pial.asc \\\n"
"                       -i SUMA/std60.rh.pial.asc \\\n"
"                       -sv anat+tlrc. -qual LR   \\\n"
"                       -closest_nodes XYZ.1D\n"
"      If you are not sure you have the proper -sv, verify with SUMA:\n"
"        suma -i SUMA/std60.lh.pial.asc  \\\n"
"             -i SUMA/std60.rh.pial.asc  \\\n"
"             -sv anat+tlrc. -niml &\n"
"        afni -niml &\n"
"      Then press 't' in SUMA to send surfaces to AFNI.\n"
"\n"
   );
   if (detail) {
      printf(
"  Mandatory Parameters:\n"
"     -closest_nodes XYZ.1D: A coordinate file specifying coordinates\n"
"                           for which the closest nodes will be found.\n"
/*"                           If you want to specify a coordinate on the\n"
"                           command line you could do:\n"
"                        -closest_nodes \"1D:17.2 12.4 -5.1\\'\"\n"*/
"                  Note: The coordinates in XYZ.1D are in RAI by default.\n"
"                        You can use -LPI if you need to.\n"
"     -closest_node 'X Y Z': An easier way to specify a single node's coords.\n"
"  Optional Parameters:\n"
"     -qual STRING: A string of characters that are used to indentify\n"
"                   the surface in which the closest node was found.\n"
"                   This is useful when you have two surfaces specified\n"
"                   like the left and right hemispheres for example.\n"
"                   In that case you can set STRING to LR if the first\n"
"                   surface is the left hemisphere and the second is the\n"
"                   right hemisphere. If you had node 12342 on the left hemi\n"
"                   and 7745 on the right, the output would look like this:\n"
"                        1342L \n"
"                        7745R \n"
"                   If qual is not set, no qualifying characters are added if \n"
"                   up only have one surface on the command line. \n"
"                   The sequence ABC... is used otherwise.\n"
"     -LPI: The coordinate axis direction for values in XYZ.1D are in LPI.\n"
"           As a result, the program will negate the sign of the X, and Y\n"
"           coordinates in XYZ.1D \n"
"     -RAI: The coordinate axis direction for values in XYZ.1D are in RAI\n"
"           which is the default. No transformation is applied to values in\n"
"           XYZ.1D\n"
"     -verb LEVEL: Verbosity level, default is 0\n"
"     -prefix PREFIX: Output results to file PREFIX (will overwrite).\n"
"                     Default is stdout\n"
      );
   }
      printf(
"\n"
"  In Demo mode:\n"
"  -------------\n"
"  Illustrates how surface coordinates relate to voxel grid.\n"
"  The program outputs surface and equivalent volume coordinates\n"
"  for all nodes in the surface after it is aligned via its sv.\n"
"  The code is intended as a source code demo.\n"
"\n");
   if (detail) {
      printf(
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
"The output is lots of text so you're better off redirecting to a file.\n"
      );
   }
   if (detail > 1) {
       printf(
"%s"
"\n"
"%s"
"%s"
               "\n", help_msg, sio,  s);
      SUMA_free(s); s = NULL;
      SUMA_free(st); st = NULL;
      SUMA_free(sio); sio = NULL;
   }
   if (detail) {
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
   }
   exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_Surf2VolCoord_demo_ParseInput(
                     char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_Surf2VolCoord_demo_ParseInput"};
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar, i;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->obj_type = 0; /* sub-brick index */
   Opt->NodeDbg = -1;
   Opt->debug=0;
   Opt->b1=0;
   Opt->n_fvec=0;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_Surf2VolCoord_demo(ps, strlen(argv[kar]) > 3 ? 2:1);
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
            SUMA_AfniPrefix(argv[++kar], Opt->out_vol_view, NULL,
                            &(Opt->out_vol_exists));

         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-closest_nodes") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a 1D file after -closest_nodes \n");
            exit (1);
         }
         if (!(Opt->fvec = SUMA_Load1D_eng(argv[++kar],
                                 &(Opt->fvec_dim), &(Opt->n_fvec), 1, 1))) {
            SUMA_S_Errv("Failed to read %s\n", argv[kar]);
            exit(1);
         }
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-closest_node") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR,
                     "need a triplet in quotes after -closest_node \n");
            exit (1);
         }
         Opt->fvec = (float *)SUMA_calloc(3, sizeof(float));
         i = SUMA_StringToNum(argv[++kar], (void *)(Opt->fvec), 3, 1);
         if (i != 3) {
            SUMA_S_Errv("Could not get 3 values from %s\n", argv[kar]);
            exit (1);
         }
         Opt->n_fvec = 1; Opt->fvec_dim = 3;
         brk = YUP;
      }


      if (!brk && (strcmp(argv[kar], "-qual") == 0)) {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -qual \n");
            exit (1);
         }
         Opt->s = SUMA_copy_string(argv[++kar]);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-RAI") == 0)) {
         Opt->b1 = 0;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-LPI") == 0)) {
         Opt->b1 = 1;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-verb") == 0)) {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need an integer after -verb \n");
            exit (1);
         }
         Opt->debug = atoi(argv[++kar]);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-grid_parent") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a dataset after -grid_parent \n");
            exit (1);
         }
         Opt->out_grid_prefix =
            SUMA_AfniPrefix(argv[++kar], Opt->out_grid_view, NULL,
                            &(Opt->out_grid_exists));
         if (!SUMA_AfniExistsView(Opt->out_grid_exists, Opt->out_grid_view)) {
            fprintf(SUMA_STDERR,
                    "Error Surf2VolCoord_demo:\n"
                    "Grid parent %s%s does not exist.\n",
                    Opt->out_grid_prefix, Opt->out_grid_view);
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
   static char FuncName[]={"Surf2VolCoord_demo"};
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec=0;
   short *isin = NULL;
   int N_in = 0, i, *minnode=NULL, *bestflag=NULL, isrf=0;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_VOLPAR *vp = NULL;
   THD_3dim_dataset *dset = NULL;
   char *vpname=NULL;
   SUMA_Boolean LocalHead = NOPE;
   double *dvec = NULL;
   float *tmpXYZ=NULL, *mindist=NULL;
   int di, dj, dk, dijk, nx, ny, nxy, i0, i1, mode;
   float fi, fj, fk;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-sv;");

   Opt = SUMA_Surf2VolCoord_demo_ParseInput (argv, argc, ps);
   if (argc < 2) {
      SUMA_S_Err("Too few options");
      usage_Surf2VolCoord_demo(ps, 0);
      exit (1);
   }


   if (Opt->debug > 2) LocalHead = YUP;
   if (Opt->n_fvec) mode = 1;
   else mode = 0;

   /* some checks ...*/
   if (!Opt->out_vol_prefix) {
      if (mode==0) {
         Opt->out_vol_prefix =
            SUMA_AfniPrefix("Surf2VolCoord_demo",
                            NULL, NULL, &(Opt->out_vol_exists));
         strncpy(Opt->out_vol_view, Opt->out_grid_view, SUMA_VIEW_LENGTH);
      }
   } else {
      if (SUMA_AfniExistsView(Opt->out_vol_exists, Opt->out_vol_view)) {
         fprintf(SUMA_STDERR,
                  "Error Surf2VolCoord_demo:\nOutput volume %s%s exists.\n",
                  Opt->out_vol_prefix, Opt->out_vol_view);
         exit(1);
      }
   }

   if (Opt->out_grid_prefix) {
      if (!SUMA_AfniExistsView(Opt->out_grid_exists, Opt->out_grid_view)) {
         fprintf(SUMA_STDERR, "Error Surf2VolCoord_demo:\n"
                              "Grid parent %s%s does not exist.\n",
                              Opt->out_grid_prefix, Opt->out_grid_view);
         exit(1);
      }
   }

   /* check on inputs */
   if ((ps->s_N_surfnames && ps->i_N_surfnames) ||
       (ps->s_N_surfnames &&  ps->t_N_surfnames) ||
       (ps->i_N_surfnames &&  ps->t_N_surfnames)) {
      SUMA_S_Err("Multiple surface input methods used."
                 "Only one method allowed.");
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

   if (mode == 0 && Spec->N_Surfs!=1) {
      SUMA_S_Err("Only one surface allowed in demo mode");
      exit(1);
   }
   if (Opt->s && strlen(Opt->s) < Spec->N_Surfs) {
      SUMA_S_Errv("Have %d surfaces but qualifier string %s is %d characters.\n",
                  Spec->N_Surfs, Opt->s, (int)strlen(Opt->s));
      exit(1);
   }

   if (mode == 1 && Opt->b1) {/* go from LPI to RAI */
      for (i=0; i<Opt->n_fvec; ++i) {
         Opt->fvec[Opt->fvec_dim*i] = -Opt->fvec[Opt->fvec_dim*i];
         Opt->fvec[Opt->fvec_dim*i+1] = -Opt->fvec[Opt->fvec_dim*i+1];
      }
   }

   for (isrf=0; isrf < Spec->N_Surfs; ++isrf) {
      SO = SUMA_Load_Spec_Surf(Spec, isrf, ps->sv[0], 0);
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


      if (mode == 1) {
         double dd=0.0;
         int n;
         if (Opt->fvec_dim != SO->NodeDim) {
            SUMA_S_Errv("Have %d dims for xyz, and %d for surf.\n",
                  Opt->fvec_dim, SO->NodeDim);
            exit (1);
         }
         if (!mindist) mindist =
               (float *)SUMA_calloc(Opt->n_fvec,sizeof(float));
         if (!minnode) minnode =
               (int *)SUMA_calloc(Opt->n_fvec,sizeof(int));
         if (!bestflag) bestflag =
               (int *)SUMA_calloc(Opt->n_fvec,sizeof(int));
         for (i=0; i<Opt->n_fvec; ++i) {
            if (isrf==0) mindist[i] = 156779800000.0;
            for (n=0; n<SO->N_Node; ++n) {
               SUMA_SEG_NORM( (SO->NodeList+SO->NodeDim*n),
                              (Opt->fvec+Opt->fvec_dim*i), dd);
               if (dd < mindist[i]) {
                  mindist[i] = dd; minnode[i] = n; bestflag[i]=isrf;
               }
            }
         }
      } else {
         /***********          The demo mode *****************/
         /* By now SO is the surface object whose coordinates have transformed
         so that it is in register with the surface volume specifed on command
         line.
         */

         /* Now let us read the volume from which you would be accessing values.
         That volume should be in alignment with -sv but not at the same
         resolution.
         I assume that this volume contains one sub-brick for simplicity. If no
         such volume is specified then we'll use the -sv volume for that */

         if (Opt->out_grid_prefix) {
            vpname = SUMA_append_string(Opt->out_grid_prefix,
                                        Opt->out_grid_view);
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
            RAI coordinates are the mm coordinates you see displayed in AFNI's
               top left corner
            3dfind are the equivalent coordinates expressed in index coordinates
               on the grid volume
            Say you have a node at 13.4, 23, -54 mm (RAI), its 3dfind coordinate
               might be: 3.2 , 56.1,   124.8 which would be closest to voxel with
               ijk indices 3, 56, 125 in the volume defined by grid parent
               (dset or vp).
            To get that voxel's value, you just access element 3,56, 125.
            Note that in this example, I am getting voxel values for all  coords
            in vector tmpXYZ and those happen to be all the nodes of the surface.
            But you could put whatever coordinates or subset of coordinates you
            want in there. Just be sure to change SO->N_Node to reflect
            the number of triplets in tmpXYZ
         */

         /* copy surface coordinates to preserve them, we're going to ijk land */
         tmpXYZ = (float *)SUMA_malloc(SO->N_Node * 3 * sizeof(float));
         if (!tmpXYZ) {
            SUMA_SL_Crit("Faile to allocate");
            exit(1);
         }
         memcpy ((void*)tmpXYZ, (void *)SO->NodeList,
                  SO->N_Node * 3 * sizeof(float));
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
            SUMA_S_Errv("Failed to allocate for %d dvec.\nOh misery.\n",
                        DSET_NVOX(dset));
            exit(1);
         }
         EDIT_coerce_scale_type( DSET_NVOX(dset) ,
                                 DSET_BRICK_FACTOR(dset,Opt->obj_type) ,
                                 DSET_BRICK_TYPE(dset,Opt->obj_type),
                                 DSET_ARRAY(dset, Opt->obj_type) ,   /* input  */
                                 MRI_double               , dvec  ) ;/* output */

         nx = DSET_NX(dset); ny = DSET_NY(dset); nxy = nx * ny;
         if (Opt->NodeDbg >= 0) { i0 = Opt->NodeDbg; i1 = Opt->NodeDbg+1; }
         else { i0 = 0; i1 = SO->N_Node;};
         for (i=i0; i<i1; ++i) {
            fi = tmpXYZ[3*i  ]; di = SUMA_ROUND(fi);
            fj = tmpXYZ[3*i+1]; dj = SUMA_ROUND(fj);
            fk = tmpXYZ[3*i+2]; dk = SUMA_ROUND(fk);
            dijk = SUMA_3D_2_1D_index(di, dj, dk, nx, nxy);
            fprintf(SUMA_STDOUT,
                     "Node Index %d: RAImm %.3f %.3f %.3f : "
                     "3dfind %.1f %.1f %.1f : 3dind %d %d %d : Val %f\n",
                      i,
                   SO->NodeList[3*i], SO->NodeList[3*i+1], SO->NodeList[3*i+2],
                      fi, fj, fk, di, dj, dk,
                      dvec[dijk]);

         }
         SUMA_free(dvec); dvec = NULL;
         SUMA_free(tmpXYZ); tmpXYZ = NULL;
         /* no need for data in input volume anymore */
         PURGE_DSET(dset);
      }

      if (vpname) SUMA_free(vpname); vpname = NULL;
      if (dset) { DSET_delete(dset); dset = NULL; }
      if (vp != SO->VolPar) SUMA_Free_VolPar(vp); vp = NULL;
      if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   }

   if (Opt->n_fvec) {
      FILE *fout=NULL;
      if (Opt->out_vol_prefix) {
         if (!(fout = fopen(Opt->out_vol_prefix,"w"))) {
            SUMA_S_Errv("Failed to open %s for output",
                        Opt->out_vol_prefix);
            exit(1);
         }
      } else {
         fout = SUMA_STDOUT;
      }
      if (Opt->debug) fprintf(fout,"#Closest_Node Distance_to_Coord\n");
      for (i=0; i<Opt->n_fvec; ++i) {
         fprintf(fout,"%d%c %f\n",
                  minnode[i],
                  Opt->s ? Opt->s[bestflag[i]]:'A'+bestflag[i],
                  mindist[i]);
      }
      if (fout != SUMA_STDOUT) fclose(fout); fout=NULL;
   }


   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (N_Spec) {
      int k=0;
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) {
            SUMA_S_Err("Failed to free spec fields"); }
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (mindist) SUMA_free(mindist); mindist = NULL;
   if (minnode) SUMA_free(minnode); minnode = NULL;
   if (bestflag) SUMA_free(bestflag); bestflag = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF))
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);

}
