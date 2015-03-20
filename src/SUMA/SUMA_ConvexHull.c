
#include "SUMA_suma.h"


/*!
   A function to call SUMA_qhull_wrap or SUMA_qdelaunay_wrap
*/
SUMA_SurfaceObject *SUMA_ConvexHullSurface(
               SUMA_GENERIC_PROG_OPTIONS_STRUCT * Opt)
{
   static char FuncName[]={"SUMA_ConvexHullSurface"};
   SUMA_SurfaceObject *SO=NULL;
   float *xyz=NULL, *xyzp=NULL, *txyz=NULL;
   int npt, *ijk=NULL, nf=0, cnt, i, j, k, nxx, nyy, nzz,N_txyz=-1;
   FILE *fid=NULL;
   THD_fvec3 fv, iv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   npt = 0;
   N_txyz=-1;
   if (Opt->UseThisBrain) {
      MRI_IMAGE *im = NULL;
      float *far=NULL;
      int nx2, i3;

      /* load the 1D file */
      im = mri_read_1D (Opt->UseThisBrain);
      if (!im) {
         SUMA_S_Err("Failed to read file %s", Opt->UseThisBrain);
         SUMA_RETURN(NULL);
      }   

      far = MRI_FLOAT_PTR(im);
      if (im->nx == 0) {
         SUMA_S_Errv("Empty file %s.\n", Opt->UseThisBrain);
         SUMA_RETURN(NULL);
      }
      if (im->ny != 3) {
         SUMA_S_Errv("Found %d columns in %s. Expecting 3\n", 
                     im->ny, Opt->UseThisBrain);
         SUMA_RETURN(NULL);
      }

      /* copy the columns */
      N_txyz = im->nx;
      txyz = (float *)SUMA_malloc(im->nx*im->ny*sizeof(float));
      if (!txyz) {
         SUMA_S_Crit("Failed to allocate.");
         SUMA_RETURN(NULL);
      }
      nx2 = 2*im->nx;
      for (i=0; i<N_txyz; ++i) {
         i3 = 3*i;
         txyz[i3  ] = far[i];
         txyz[i3+1] = far[i+im->nx];
         txyz[i3+2] = far[i+nx2];
      }

      /* done, clean up and out you go */
      if (im) mri_free(im); im = NULL;       
   }
   
   if (Opt->in_vol) {
      cnt = 0; npt = 0;
      nxx = (DSET_NX(Opt->in_vol)); 
      nyy = (DSET_NY(Opt->in_vol));
      nzz = (DSET_NZ(Opt->in_vol));

      if (Opt->debug) fprintf(SUMA_STDERR,"%s:\nRunning qhull...\n", FuncName);
      xyz = (float *)SUMA_malloc(3*nxx*nyy*nzz*sizeof(float));
      if (!xyz) {
         SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NULL);
      }
      for(  k = 0 ; k < nzz ; k++ ) {
         for(  j = 0 ; j < nyy ; j++ ) {
            for(  i = 0 ; i < nxx ; i++ ) {
               if (Opt->mcfv[cnt] == 1) {
                  fv.xyz[0] = DSET_XORG(Opt->in_vol) + i * DSET_DX(Opt->in_vol);
                  fv.xyz[1] = DSET_YORG(Opt->in_vol) + j * DSET_DY(Opt->in_vol);
                  fv.xyz[2] = DSET_ZORG(Opt->in_vol) + k * DSET_DZ(Opt->in_vol);
                  /* change mm to RAI coords */
		            iv = SUMA_THD_3dmm_to_dicomm( Opt->in_vol->daxes->xxorient, 
                                                Opt->in_vol->daxes->yyorient, 
                                                Opt->in_vol->daxes->zzorient, 
                                                fv );
                  xyz[3*npt] = iv.xyz[0]; 
                  xyz[3*npt+1] = iv.xyz[1]; xyz[3*npt+2] = iv.xyz[2]; 
                  npt++;
               }
               ++cnt;
            }
         }
      }
   } else if (Opt->XYZ) {
      xyz = (float *)SUMA_malloc(3*Opt->N_XYZ*sizeof(float));
      if (!xyz) {
         SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NULL);
      }
      for(  k = 0 ; k < 3*Opt->N_XYZ ; k++ ) {  
         xyz[k] = Opt->XYZ[k]; npt = Opt->N_XYZ; 
      }   
   } else {
      SUMA_S_Err("No input");
      goto CLEANUP; 
   }
   
   if (Opt->corder) {
      SUMA_PC_XYZ_PROJ *pcp=NULL;
      if (Opt->geom==1) {
         SUMA_S_Warn("PCA projection makes no sense for usual convex hull");
      }
      if (!(pcp = SUMA_Project_Coords_PCA (xyz, npt, npt/2, NULL,
                                            E3_PLN_PRJ, ROT_2_Z,0))) {
         SUMA_S_Err("Failed to project");
         goto CLEANUP;   
      }
      xyzp = pcp->xyzp; pcp->xyzp = NULL;
      pcp = SUMA_Free_PC_XYZ_Proj(pcp);
   } else {
      xyzp = xyz;
   }

   if (N_txyz >= 0 && N_txyz != npt) {
      SUMA_S_Errv("Mismatch between number of coordinates for convex hull\n"
                  "and number of coordinates to adopt in the end.\n"
                  " %d, versus %d in -these_coords\n",
                  npt, N_txyz);
      goto CLEANUP;            
   }  
   
   if (Opt->geom == 1) { /* convex hull */
      if (! (nf = SUMA_qhull_wrap(npt, xyzp, &ijk, 1, Opt->s)) ) {
         fprintf(SUMA_STDERR,"%s:\nFailed in SUMA_qhull_wrap\n", FuncName);
         goto CLEANUP; 
      }
      
      /* Other than unif==0 make no sense here, but leave it to the user */
      switch (Opt->unif) {
         case 0:  /* coordinates as passed to qhull, 
                     could be projected ones*/
            SO = SUMA_Patch2Surf(xyzp, npt, ijk, nf, 3);
            break;
         case 1:  /* Original corrdinates passed to qhull
                     (pre-projections, if any) */
            SO = SUMA_Patch2Surf(xyz, npt, ijk, nf, 3);
            break;
         case 2: /* special coordinates passed by user, 
                    never passed in any form to qhull */
            SUMA_S_Warn("Makes no sense to mess with coords for convex hull...");
            SO = SUMA_Patch2Surf(txyz, npt, ijk, nf, 3);
            break;
         default:
            SUMA_S_Err("pit of despair");
            goto CLEANUP; 
      }
            
      if (Opt->debug) fprintf(SUMA_STDERR,"%s:\n%d triangles.\n", FuncName, nf);
   } else if (Opt->geom == 2) { /* triangulation */
      if (! (nf = SUMA_qdelaunay_wrap(npt, xyzp, &ijk, 1, Opt->s)) ) {
         fprintf(SUMA_STDERR,"%s:\nFailed in SUMA_qdelaunay_wrap\n", FuncName);
         goto CLEANUP;    
      }
      switch (Opt->unif) {
         case 0:  /* coordinates as passed to qdelaunay, 
                     could be projected ones*/
            if (xyz == xyzp) xyz=NULL; /* xyzp will be set to 
                                         null in next call, 
                                         so xyz is treated 
                                         the same here */
            SO = SUMA_NewSO(&xyzp, npt, &ijk, nf, NULL);
            SUMA_LHv("xyzp %p, ijk %p\n", txyz, ijk);
           break;
         case 1:  /* Original corrdinates passed to qdelaunay
                     (pre-projections, if any) */
            SO = SUMA_NewSO(&xyz, npt, &ijk, nf, NULL);
            SUMA_LHv("xyz %p, ijk %p\n", txyz, ijk);
            break;
         case 2:  /* special coordinates passed by user, 
                     never passed in any form to qdelaunay */
            SO = SUMA_NewSO(&txyz, npt, &ijk, nf, NULL);
            SUMA_LHv("txyz %p, ijk %p\n", txyz, ijk);
            break;
         default:
            SUMA_S_Err("pit of despair, again");
            goto CLEANUP; 
      }
   } else {
      SUMA_S_Errv("Opt->geom = %d not valid\n", Opt->geom);
      goto CLEANUP;      
   }  
   
   CLEANUP:
   if (ijk) SUMA_free(ijk); ijk=NULL;
   if(txyz) SUMA_free(txyz); txyz=NULL;
   if (xyzp != xyz && xyzp != NULL) SUMA_free(xyzp); xyzp = NULL;
   if (xyz) SUMA_free(xyz); xyz = NULL;

   SUMA_RETURN(SO);
}

void usage_SUMA_ConvexHull (SUMA_GENERIC_ARGV_PARSE *ps)
   {
      static char FuncName[]={"usage_SUMA_ConvexHull"};
      char * s = NULL, *sio=NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
"\n"
"Usage: A program to find the convex hull, or perform a delaunay triangulation\n"
"       of a set of points.\n"
"  This program is a wrapper for the qhull, and qdelaunay programs.\n"
"  see copyright notice by running suma -sources.\n"
"\n"
"  ConvexHull  \n"
"     usage 1: < -input VOL >\n"
"              < -isoval V | -isorange V0 V1 | -isocmask MASK_COM >\n"
"              [<-xform XFORM>]\n"
"     usage 2: < i_TYPE input surface >\n"
"              [<-sv SURF_VOL>]\n"
"     usage 3: < -input_1D XYZ >\n"
"              [<-q_opt OPT>]\n"     
"     common optional:\n"
"              [< -o_TYPE PREFIX>]\n"
"              [< -debug DBG >]\n"  
"\n"
"  Mandatory parameters, choose one of three usage modes:\n"
"  Usage 1:\n"
"     You must use one of the following two options:\n"
"     -input VOL: Input AFNI (or AFNI readable) volume.\n"
"     You must use one of the following iso* options:\n"
"     -isoval V: Create isosurface where volume = V\n"
"     -isorange V0 V1: Create isosurface where V0 <= volume < V1\n"
"     -isocmask MASK_COM: Create isosurface where MASK_COM != 0\n"
"        For example: -isocmask '-a VOL+orig -expr (1-bool(a-V))' \n"
"        is equivalent to using -isoval V. \n"
"     NOTE: -isorange and -isocmask are only allowed with -xform mask\n"
"            See -xform below for details.\n"
"\n"
"  Usage 2:\n"
"     -i_TYPE SURF:  Use the nodes of a surface model\n"
"                    for input. See help for i_TYPE usage\n"
"                    below.\n"
"\n"
"  Usage 3:\n"
"     -input_1D XYZ: Construct the triangulation of the points\n"
"                    contained in 1D file XYZ. If the file has\n"
"                    more than 3 columns, use AFNI's [] selectors\n"
"                    to specify the XYZ columns.\n"
"     -q_opt OPT: Meshing option OPT can be one of.\n"
"                    convex_hull: For convex hull of points (default)\n"
"                    triangulate_xy: Delaunay triangulation using x y coords\n"
"\n"
" These three options are only useful with -q_opt triangulate_xy\n"
"     -proj_xy: Project points onto plane whose normal is the third principal \n"
"               component. Then rotate projection so that plane in parallel to\n"
"               Z = constant.\n"
"     -orig_coord: Use original coordinates when writing surface, not\n"
"                  transformed ones.\n"
"     -these_coords COORDS.1D: Use coordinates in COORDS.1D when \n"
"                              writing surface.\n" 
"\n" 
"  Optional Parameters:\n"
"     Usage 1 only:\n"
"     -xform XFORM:  Transform to apply to volume values\n"
"                    before searching for sign change\n"
"                    boundary. XFORM can be one of:\n"
"            mask: values that meet the iso* conditions\n"
"                  are set to 1. All other values are set\n"
"                  to -1. This is the default XFORM.\n"
"            shift: subtract V from the dataset and then \n"
"                   search for 0 isosurface. This has the\n"
"                   effect of constructing the V isosurface\n"
"                   if your dataset has a continuum of values.\n"
"                   This option can only be used with -isoval V.\n"
"            none: apply no transforms. This assumes that\n"
"                  your volume has a continuum of values \n"
"                  from negative to positive and that you\n"
"                  are seeking to 0 isosurface.\n"
"                  This option can only be used with -isoval 0.\n"
"     Usage 2 only:\n"
"     -sv SURF_VOL: Specify a surface volume which contains\n"
"                   a transform to apply to the surface node\n"
"                   coordinates prior to constructing the \n"
"                   convex hull.\n"
"     All Usage:\n"
"     -o_TYPE PREFIX: prefix of output surface.\n"
"        where TYPE specifies the format of the surface\n"
"        and PREFIX is, well, the prefix.\n"
"        TYPE is one of: fs, 1d (or vec), sf, ply.\n"
"        Default is: -o_ply \n"
"\n"
"%s\n"
"\n"
/*"     -debug DBG: debug levels of 0 (default), 1, 2, 3.\n"
"        This is no Rick Reynolds debug, which is oft nicer\n"
"        than the results, but it will do.\n"
"\n" */
"%s"
               "\n", sio, s);
       SUMA_free(s); s = NULL;  SUMA_free(sio); sio = NULL;          
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
       exit (0);
   }
/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_GENERIC_PROG_OPTIONS_STRUCT *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_ConvexHull_ParseInput (char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_ConvexHull_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar, i, ind;
   char *outname;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_GENERIC_PROG_OPTIONS_STRUCT *)SUMA_malloc(sizeof(SUMA_GENERIC_PROG_OPTIONS_STRUCT));
   
   kar = 1;
   Opt->spec_file = NULL;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->N_surf = -1;
   Opt->in_name = NULL;
   Opt->cmask = NULL;
   Opt->MaskMode = SUMA_ISO_UNDEFINED;
   for (i=0; i<SUMA_GENERIC_PROG_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
   Opt->in_vol = NULL;
   Opt->nvox = -1;
   Opt->ninmask = -1;
   Opt->mcfv = NULL;
   Opt->debug = 0;
   Opt->v0 = 0.0;
   Opt->v1 = -1.0;
   Opt->fvec = NULL;
   Opt->SurfFileType = SUMA_PLY;
   Opt->SurfFileFormat = SUMA_ASCII;
   Opt->xform = SUMA_ISO_XFORM_MASK;
   Opt->obj_type = -1;
   Opt->obj_type_res = -1;
   Opt->XYZ = NULL;
   Opt->in_1D = NULL;
   Opt->N_XYZ = 0;
   Opt->s = SUMA_copy_string("convex_hull");
   Opt->geom = 1;
   Opt->corder = 0;
   Opt->unif = 0;
   Opt->UseThisBrain = NULL;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_ConvexHull(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-xform") == 0)) {
         kar ++;
         if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -xform \n");
				exit (1);
			}
         if (!strcmp(argv[kar], "mask")) {
            Opt->xform = SUMA_ISO_XFORM_MASK;
         } else if (!strcmp(argv[kar], "none")) {
            Opt->xform = SUMA_ISO_XFORM_NONE;
         } else if (!strcmp(argv[kar], "shift")) {
            Opt->xform = SUMA_ISO_XFORM_SHIFT;
         }else {
            fprintf (SUMA_STDERR, 
                     "%s is a bad parameter for -xform option. \n", argv[kar]);
				exit (1);
         }
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-input_1D") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input_1D \n");
				exit (1);
			}
			Opt->in_1D = argv[kar];
         brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-these_coords") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -these_coords \n");
				exit (1);
			}
			Opt->UseThisBrain = argv[kar];
         Opt->unif = 2;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-q_opt") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need option after -q_opt \n");
				exit (1);
			}
         if (!strcmp(argv[kar],"triangulate_xy")) {
            Opt->geom = 2;
         } else if (!strcmp(argv[kar],"convex_hull")) {
            Opt->geom = 1;
         } else {
            SUMA_S_Errv("Bad value of %s for -q_opt", argv[kar]);
            exit(1);
         }  
			Opt->s = SUMA_copy_string(argv[kar]);
         brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-proj_xy") == 0)) {
         Opt->corder = 1;
         brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-orig_coord") == 0)) {
         Opt->unif = 1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -debug \n");
				exit (1);
			}
			Opt->debug = atoi(argv[kar]);
         if (Opt->debug > 2) { LocalHead = YUP; }
         brk = YUP;
		}      
      
      if (!brk && (strcmp(argv[kar], "-isocmask") == 0)) {
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -isocmask \n");
				exit (1);
			}
			Opt->cmask = argv[kar];
         Opt->MaskMode = SUMA_ISO_CMASK;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-isoval") == 0)) {
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -isoval \n");
				exit (1);
			}
			Opt->v0 = atof(argv[kar]);
         Opt->MaskMode = SUMA_ISO_VAL;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-isorange") == 0)) {
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 arguments after -isorange \n");
				exit (1);
			}
			Opt->v0 = atof(argv[kar]);kar ++;
         Opt->v1 = atof(argv[kar]);
         Opt->MaskMode = SUMA_ISO_RANGE;
         if (Opt->v1 < Opt->v0) {
            fprintf (SUMA_STDERR, 
                     "range values wrong. Must have %f <= %f \n", 
                     Opt->v0, Opt->v1);
				exit (1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input \n");
				exit (1);
			}
			Opt->in_name = SUMA_copy_string(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,
                  "Error %s:\n"
                  "Option %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   /* transfer some options to Opt from ps. 
      Clunky because this is retrofitting */
   if (ps->o_N_surfnames) {
      Opt->out_prefix = SUMA_copy_string(ps->o_surfnames[0]);
      Opt->SurfFileType = ps->o_FT[0];
      Opt->SurfFileFormat = ps->o_FF[0];
   }
   
   if (ps->i_N_surfnames) {
      if (Opt->in_name || Opt->in_1D) {
         SUMA_S_Err( "Options -i_TYPE, -input and -input_1D"
                     " are mutually exclusive.");
         exit(1);
      }
   }
   
   if (Opt->in_name) {
      if (ps->i_N_surfnames !=0 || Opt->in_1D) {
         fprintf (SUMA_STDERR,"Error %s:\nOptions -i_TYPE, -input and -input_1D are mutually exclusive.\n", FuncName);
         exit(1);
      }
   }
   
   if (Opt->in_1D) {
      if (ps->i_N_surfnames !=0 || Opt->in_name) {
         fprintf (SUMA_STDERR,"Error %s:\nOptions -i_TYPE, -input and -input_1D are mutually exclusive.\n", FuncName);
         exit(1);
      }
   }
   
   if (Opt->in_name && Opt->obj_type >= 0) {
      fprintf (SUMA_STDERR,"Error %s:\nOptions -input and -shape are mutually exclusive.\n", FuncName);
      exit(1);
   }
   if ((!Opt->in_name && Opt->obj_type < 0) && (!Opt->in_1D && !ps->i_N_surfnames)) {
      fprintf (SUMA_STDERR,"Error %s:\nEither -input or -input_1D or -i_TYPE options must be used.\n", FuncName);
      exit(1);
   }
   
   if (Opt->in_1D || ps->i_N_surfnames) {
      if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
         fprintf (SUMA_STDERR,"Error %s:\nCannot use -iso* options with either -input_1D or -i_TYPE options.\n", FuncName);
         exit(1);
      }
      if (Opt->xform != SUMA_ISO_XFORM_MASK) {
         fprintf (SUMA_STDERR,"Error %s:\nCannot use -xform option with either -input_1D or -i_TYPE options.\n", FuncName);
         exit(1);
      }
   }
   
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("convexhull_out");
   
   if (Opt->xform == SUMA_ISO_XFORM_NONE) {
      if (Opt->v0 != 0) {
         fprintf (SUMA_STDERR,"Error %s: Bad %f isovalue\nWith -xform none you can only extract the 0 isosurface.\n(i.e. -isoval 0)\n", FuncName, Opt->v0);
         exit(1);
      }
      if (Opt->MaskMode != SUMA_ISO_VAL) {
         fprintf (SUMA_STDERR,"Error %s: \nWith -xform none you can only use -isoval 0\n", FuncName);
         exit(1);
      }
   }
   
   if (Opt->xform == SUMA_ISO_XFORM_SHIFT) {
      if (Opt->MaskMode != SUMA_ISO_VAL) {
         fprintf (SUMA_STDERR,"Error %s: \nWith -xform shift you can only use -isoval val\n", FuncName);
         exit(1);
      }
   }
   
   SUMA_RETURN(Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"ConvexHull"}; 
	int i, i3, nspec = 0;
   void *SO_name=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   char  stmp[200];
   SUMA_Boolean exists = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   
   
   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-i;-sv;");
   
   if (argc < 2) {
      usage_SUMA_ConvexHull(ps);
      exit (1);
   }
   
   Opt = SUMA_ConvexHull_ParseInput (argv, argc, ps);
      
   SO_name = SUMA_Prefix2SurfaceName(Opt->out_prefix, NULL, NULL, 
                                     Opt->SurfFileType, &exists);
   if (exists && !THD_ok_overwrite()) {
      SUMA_S_Err("Output file(s) %s* on disk.\nWill not overwrite.\n", 
                 Opt->out_prefix);
      exit(1);
   }
   
   if (Opt->obj_type < 0) {
      if (Opt->in_name) {
         if (Opt->debug) {
            SUMA_S_Note("Creating mask...");
         }
         if (!SUMA_Get_isosurface_datasets (Opt)) {
            SUMA_SL_Err("Failed to get data.");
            exit(1);
         }

         if (Opt->debug > 1) {
            if (Opt->debug == 2) {
               FILE *fout=fopen("inmaskvec.1D","w");
               SUMA_S_Note("Writing masked values...\n");
               if (!fout) {
                  SUMA_SL_Err("Failed to write maskvec");
                  exit(1);
               }
               fprintf(fout,"#Col. 0 Voxel Index\n"
                            "#Col. 1 Is a mask (all values here should be 1)\n");
               for (i=0; i<Opt->nvox; ++i) {
                  if (Opt->mcfv[i]) {
                     fprintf(fout,"%d %.2f\n", i, Opt->mcfv[i]);
                  }
               }
               fclose(fout); fout = NULL;
            } else {
               FILE *fout=fopen("maskvec.1D","w");
               SUMA_S_Note("Writing all mask values...\n");
               if (!fout) {
                  SUMA_S_Err("Failed to write maskvec");
                  exit(1);
               }
               fprintf(fout,  "#Col. 0 Voxel Index\n"
                              "#Col. 1 Is in mask ?\n" );
               for (i=0; i<Opt->nvox; ++i) {
                  fprintf(fout,"%d %.2f\n", i, Opt->mcfv[i]);
               }
               fclose(fout); fout = NULL;
            }
         }
      } else if (Opt->in_1D) {
            MRI_IMAGE *im = NULL;
            float *far=NULL;
            int nx2;
            
            /* load the 1D file */
            im = mri_read_1D (Opt->in_1D);
            if (!im) {
               SUMA_S_Err("Failed to read file");
               exit(1);
            }   

            far = MRI_FLOAT_PTR(im);
            if (im->nx == 0) {
               fprintf(SUMA_STDERR,"Error %s:\n Empty file %s.\n", FuncName, Opt->in_1D);
               exit(1);
            }
            if (im->ny != 3) {
               fprintf(SUMA_STDERR,"Error %s:\n Found %d columns in %s. Expecting 3\n", FuncName, im->ny, Opt->in_1D);
               exit(1);
            }
            
            /* copy the columns */
            Opt->N_XYZ = im->nx;
            Opt->XYZ = (float *)SUMA_malloc(im->nx*im->ny*sizeof(float));
            if (!Opt->XYZ) {
               SUMA_S_Crit("Failed to allocate.");
               exit(1);
            }
            nx2 = 2*im->nx;
            for (i=0;i<Opt->N_XYZ; ++i) {
               i3 = 3*i;
               Opt->XYZ[i3  ] = far[i];
               Opt->XYZ[i3+1] = far[i+im->nx];
               Opt->XYZ[i3+2] = far[i+nx2];
            }
            
            /* done, clean up and out you go */
            if (im) mri_free(im); im = NULL; 
      } else if (ps->i_N_surfnames) {
         SUMA_SurfSpecFile *Spec=NULL;
         SUMA_SurfaceObject *SO=NULL;
         
         if (ps->i_N_surfnames > 1) {
            SUMA_S_Err("Only 1 input surface allowed!");
            exit(1);
         }
         Spec = SUMA_IO_args_2_spec(ps, &nspec);
         if (!Spec) {
            SUMA_S_Err("Failed to create spec!");
            exit(1);
         }
         if (nspec != 1) {
            SUMA_S_Warn("Expected one spec and nothing else");
         }
         /* load the surface object */
         SO = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], 0);
         if (!SO) {
            SUMA_S_Err("Failed to read surface.");
            exit(1);
         }
         /* transfer coords */
         if(SO->NodeDim != 3) {
            SUMA_S_Err("bad node coords.");
            exit(1);
         }
         
         Opt->N_XYZ = SO->N_Node;
         Opt->XYZ = (float *)SUMA_malloc(SO->N_Node * SO->NodeDim * sizeof(float));
         if (!Opt->XYZ) {
            SUMA_S_Crit("Failed to allocate.");
            exit(1);
         }
         for (i=0;i<SO->NodeDim*SO->N_Node; ++i) Opt->XYZ[i] = SO->NodeList[i];
         
         if (nspec) {
            int k=0; 
            for (k=0; k<nspec; ++k) {
               if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
            }
            SUMA_free(Spec); Spec = NULL; nspec = 0;
         }

         if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
      } else {
         SUMA_S_Err("No input!");
         exit(1);
      }
   } else {
      SUMA_S_Err("Bad input!");
      exit(1);
   }
   
               
   /* Now call Marching Cube functions */
   if (!(SO = SUMA_ConvexHullSurface(Opt))) {
      SUMA_S_Err("Failed to create surface.\n");
      exit(1);
   }

   /* write the surface to disk */
   if (!SUMA_Save_Surface_Object (SO_name, SO, 
                        Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
      fprintf (SUMA_STDERR,
                  "Error %s: Failed to write surface object.\n", FuncName);
      exit (1);
   }
   
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt->fvec) SUMA_free(Opt->fvec); Opt->fvec = NULL;
   if (Opt->mcfv) {SUMA_free(Opt->mcfv); Opt->mcfv = NULL;} 
   if (Opt->in_vol) { DSET_delete( Opt->in_vol); Opt->in_vol = NULL;} 
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt->XYZ) SUMA_free(Opt->XYZ); Opt->XYZ = NULL;
   if (Opt) SUMA_free(Opt);
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   if (SO_name) SUMA_free(SO_name); SO_name = NULL;
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
}   
