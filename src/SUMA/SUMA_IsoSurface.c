/*! File to contain routines for creating isosurfaces.

NOTE: MarchingCube code was translated from Thomas Lewiner's C++
implementation of the paper:
Efficient Implementation of Marching Cubes´ Cases with Topological Guarantees
by Thomas Lewiner, Hélio Lopes, Antônio Wilson Vieira and Geovan Tavares 
in Journal of Graphics Tools. 
http://www-sop.inria.fr/prisme/personnel/Thomas.Lewiner/JGT.pdf
*/

#include "SUMA_suma.h"
#include "MarchingCubes/MarchingCubes.h"

#undef STAND_ALONE

#if defined SUMA_IsoSurface_STANDALONE
#define STAND_ALONE 
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */
#else
extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  
#endif

/*! a macro version of MarchingCubes' set_data */
#define SUMA_SET_MC_DATA(mcb, val, i, j, k) {  \
   mcb->data[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y] = val; \
}

/*!
   A function version of the program mc by Thomas Lewiner
   see main.c in ./MarchingCubes
*/
SUMA_SurfaceObject *SUMA_MarchingCubesSurface(SUMA_ISOSURFACE_OPTIONS * Opt)
{
   static char FuncName[]={"SUMA_MarchingCubesSurface"};
   SUMA_SurfaceObject *SO=NULL;
   int nxx, nyy, nzz, cnt, i, j, k, *FaceSetList=NULL;
   float *NodeList=NULL;
   SUMA_NEW_SO_OPT *nsoopt = NULL;
   THD_fvec3 fv, iv;
   MCB *mcp ;
   
   SUMA_ENTRY;
   
   if (Opt->obj_type < 0) {
      nxx = DSET_NX(Opt->in_vol);
      nyy = DSET_NY(Opt->in_vol);
      nzz = DSET_NZ(Opt->in_vol);

      if (Opt->debug) {
         fprintf(SUMA_STDERR,"%s:\nNxx=%d\tNyy=%d\tNzz=%d\n", FuncName, nxx, nyy, nzz);
      }

      mcp = MarchingCubes(-1, -1, -1);
      set_resolution( mcp, nxx, nyy, nzz ) ;
      init_all(mcp) ;
      if (Opt->debug) fprintf(SUMA_STDERR,"%s:\nSetting data...\n", FuncName);
      cnt = 0;
      for(  k = 0 ; k < mcp->size_z ; k++ ) {
         for(  j = 0 ; j < mcp->size_y ; j++ ) {
            for(  i = 0 ; i < mcp->size_x ; i++ ) {
               SUMA_SET_MC_DATA ( mcp, Opt->mcdatav[cnt], i, j, k); 
               ++cnt;
            }
         }
      }

   } else {
      /* built in */
      nxx = nyy = nzz = Opt->obj_type_res;
      mcp = MarchingCubes(-1, -1, -1);
      set_resolution( mcp, nxx, nyy, nzz) ;
      init_all(mcp) ;
      compute_data( *mcp , Opt->obj_type) ;
   }

   if (Opt->debug) fprintf(SUMA_STDERR,"%s:\nrunning MarchingCubes...\n", FuncName);
   run(mcp) ;
   clean_temps(mcp) ;

   if (Opt->debug > 1) {
      fprintf(SUMA_STDERR,"%s:\nwriting out NodeList and FaceSetList...\n", FuncName);
      write1Dmcb(mcp);
   }

   if (Opt->debug) {
      fprintf(SUMA_STDERR,"%s:\nNow creating SO...\n", FuncName);
   }

   NodeList = (float *)SUMA_malloc(sizeof(float)*3*mcp->nverts);
   FaceSetList = (int *)SUMA_malloc(sizeof(int)*3*mcp->ntrigs);
   if (!NodeList || !FaceSetList)  {
      SUMA_SL_Crit("Failed to allocate!");
      SUMA_RETURN(SO);
   }
   
   if (Opt->obj_type < 0) {
      if (Opt->debug) {
         fprintf(SUMA_STDERR,"%s:\nCopying vertices, changing to DICOM \nOrig:(%f %f %f) \nD:(%f %f %f)...\n", 
            FuncName, DSET_XORG(Opt->in_vol), DSET_YORG(Opt->in_vol), DSET_ZORG(Opt->in_vol),
                        DSET_DX(Opt->in_vol), DSET_DY(Opt->in_vol), DSET_DZ(Opt->in_vol));
      }
      for ( i = 0; i < mcp->nverts; i++ ) {
         j = 3*i; /* change from index coordinates to mm DICOM, next three lines are equivalent of SUMA_THD_3dfind_to_3dmm*/
         fv.xyz[0] = DSET_XORG(Opt->in_vol) + mcp->vertices[i].x * DSET_DX(Opt->in_vol);
         fv.xyz[1] = DSET_YORG(Opt->in_vol) + mcp->vertices[i].y * DSET_DY(Opt->in_vol);
         fv.xyz[2] = DSET_ZORG(Opt->in_vol) + mcp->vertices[i].z * DSET_DZ(Opt->in_vol);
         /* change mm to RAI coords */
		   iv = SUMA_THD_3dmm_to_dicomm( Opt->in_vol->daxes->xxorient, Opt->in_vol->daxes->yyorient, Opt->in_vol->daxes->zzorient, fv );
         NodeList[j  ] = iv.xyz[0];
         NodeList[j+1] = iv.xyz[1];
         NodeList[j+2] = iv.xyz[2];
      }
      for ( i = 0; i < mcp->ntrigs; i++ ) {
         j = 3*i;
         FaceSetList[j  ] = mcp->triangles[i].v3;
         FaceSetList[j+1] = mcp->triangles[i].v2;
         FaceSetList[j+2] = mcp->triangles[i].v1;
      }
   } else {
      /* built in */
      for ( i = 0; i < mcp->nverts; i++ ) {
         j = 3*i;
         NodeList[j  ] = mcp->vertices[i].x;
         NodeList[j+1] = mcp->vertices[i].y;
         NodeList[j+2] = mcp->vertices[i].z;
      }   
      for ( i = 0; i < mcp->ntrigs; i++ ) {
         j = 3*i;
         FaceSetList[j  ] = mcp->triangles[i].v3;
         FaceSetList[j+1] = mcp->triangles[i].v2;
         FaceSetList[j+2] = mcp->triangles[i].v1;
      }
   }
   

   nsoopt = SUMA_NewNewSOOpt();
   SO = SUMA_NewSO(&NodeList, mcp->nverts, &FaceSetList, mcp->ntrigs, nsoopt);

   if (Opt->debug) {
      fprintf(SUMA_STDERR,"%s:\nCleaning mcp...\n", FuncName);
   }
   clean_all(mcp) ;
   free(mcp);
   
   SUMA_RETURN(SO);
}
/*!
   Shamelessly stolen from Rick who stole it from Bob. To hide this theft, I use longer names for functions and variables.
*/
SUMA_Boolean SUMA_Get_isosurface_datasets (SUMA_ISOSURFACE_OPTIONS * Opt)
{
   static char FuncName[]={"SUMA_Get_isosurface_datasets"};
   int i;
   
   SUMA_ENTRY;
   
   Opt->in_vol = THD_open_dataset( Opt->in_name );
   if (!ISVALID_DSET(Opt->in_vol)) {
      if (!Opt->in_name) {
         SUMA_SL_Err("NULL input volume.");
         SUMA_RETURN(NOPE);
      } else {
         SUMA_SL_Err("invalid volume.");
         SUMA_RETURN(NOPE);
      }
   } else if ( DSET_BRICK_TYPE(Opt->in_vol, 0) == MRI_complex) {
      SUMA_SL_Err("Can't do complex data.");
      SUMA_RETURN(NOPE);
   }
   
   Opt->nvox = DSET_NVOX( Opt->in_vol );
   if (DSET_NVALS( Opt->in_vol) != 1) {
      SUMA_SL_Err("Input volume can only have one sub-brick in it.\nUse [.] selectors to choose sub-brick needed.");
      SUMA_RETURN(NOPE);
   }
   
   
   Opt->mcdatav = (double *)SUMA_malloc(sizeof(double)*Opt->nvox);
   if (!Opt->mcdatav) {
      SUMA_SL_Crit("Failed to allocate for maskv");
      SUMA_RETURN(NOPE);
   }
   if (Opt->xform == SUMA_ISO_XFORM_MASK) {
      switch (Opt->MaskMode) {
         case SUMA_ISO_CMASK:
            if (Opt->cmask) {
               /* here's the second order grand theft */
               int    clen = strlen( Opt->cmask );
	            char * cmd;
               byte *bmask;

	            /* save original cmask command, as EDT_calcmask() is destructive */
	            cmd = (char *)malloc((clen + 1) * sizeof(char));
	            strcpy( cmd,  Opt->cmask);

	            bmask = EDT_calcmask( cmd, &Opt->ninmask );

	            free( cmd );			   /* free EDT_calcmask() string */

	            if ( bmask == NULL ) {
	               SUMA_SL_Err("Failed to compute mask from -cmask option");
	               SUMA_free(Opt->mcdatav); Opt->mcdatav=NULL;
                  SUMA_RETURN(NOPE);
	            } 
	            if ( Opt->ninmask != Opt->nvox ) {
	               SUMA_SL_Err("Input and cmask datasets do not have "
		                        "the same dimensions\n" );
	               SUMA_free(Opt->mcdatav); Opt->mcdatav=NULL;
	               SUMA_RETURN(NOPE);
	            }
	            Opt->ninmask = THD_countmask( Opt->ninmask, bmask );
               for (i=0; i<Opt->nvox; ++i) if (bmask[i]) Opt->mcdatav[i] = (float)bmask[i]; else Opt->mcdatav[i] = -1;
               free(bmask);bmask=NULL;
            } else {
               SUMA_SL_Err("NULL cmask"); SUMA_RETURN(NOPE);
            }
            break;
         case SUMA_ISO_VAL:
         case SUMA_ISO_RANGE:
            /* load the dset */
            DSET_load(Opt->in_vol);
            Opt->dvec = (double *)SUMA_malloc(sizeof(double) * Opt->nvox);
            if (!Opt->dvec) {
               SUMA_SL_Crit("Faile to allocate for dvec.\nOh misery.");
               SUMA_RETURN(NOPE);
            }
            EDIT_coerce_scale_type( Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
                                    DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) ,      /* input  */
                                    MRI_double               , Opt->dvec  ) ;   /* output */
            /* no need for data in input volume anymore */
            PURGE_DSET(Opt->in_vol);

            Opt->ninmask = 0;
            if (Opt->MaskMode == SUMA_ISO_VAL) {
               for (i=0; i<Opt->nvox; ++i) {
                  if (Opt->dvec[i] == Opt->v0) { 
                     Opt->mcdatav[i] = 1; ++ Opt->ninmask; 
                  }  else Opt->mcdatav[i] = -1;
               }
            } else if (Opt->MaskMode == SUMA_ISO_RANGE) {
               for (i=0; i<Opt->nvox; ++i) {
                  if (Opt->dvec[i] >= Opt->v0 && Opt->dvec[i] < Opt->v1) { 
                     Opt->mcdatav[i] = 1; ++ Opt->ninmask; 
                  }  else Opt->mcdatav[i] = -1;
               }
            } else {
               SUMA_SL_Err("Bad Miracle.");
               SUMA_RETURN(NOPE);
            }
            SUMA_free(Opt->dvec); Opt->dvec = NULL; /* this vector is not even created in SUMA_ISO_CMASK mode ...*/
            break;
         default:
            SUMA_SL_Err("Unexpected value of MaskMode");
            SUMA_RETURN(NOPE);
            break;   
      }
   } else if (Opt->xform == SUMA_ISO_XFORM_SHIFT) {
      /* load the dset */
      DSET_load(Opt->in_vol);
      Opt->dvec = (double *)SUMA_malloc(sizeof(double) * Opt->nvox);
      if (!Opt->dvec) {
         SUMA_SL_Crit("Failed to allocate for dvec.\nOh misery.");
         SUMA_RETURN(NOPE);
      }
      EDIT_coerce_scale_type( Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
                              DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) ,      /* input  */
                              MRI_double               , Opt->dvec  ) ;   /* output */
      /* no need for data in input volume anymore */
      PURGE_DSET(Opt->in_vol);
      Opt->ninmask = Opt->nvox;
      for (i=0; i<Opt->nvox; ++i) { 
         Opt->mcdatav[i] = Opt->dvec[i] - Opt->v0;
      }
      SUMA_free(Opt->dvec); Opt->dvec = NULL; /* this vector is not even created in SUMA_ISO_CMASK mode ...*/
   } else if (Opt->xform == SUMA_ISO_XFORM_NONE) {
      /* load the dset */
      DSET_load(Opt->in_vol);
      Opt->dvec = (double *)SUMA_malloc(sizeof(double) * Opt->nvox);
      if (!Opt->dvec) {
         SUMA_SL_Crit("Faile to allocate for dvec.\nOh misery.");
         SUMA_RETURN(NOPE);
      }
      EDIT_coerce_scale_type( Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
                              DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) ,      /* input  */
                              MRI_double               , Opt->dvec  ) ;   /* output */
      /* no need for data in input volume anymore */
      PURGE_DSET(Opt->in_vol);
      Opt->ninmask = Opt->nvox;
      for (i=0; i<Opt->nvox; ++i) { 
         Opt->mcdatav[i] = Opt->dvec[i];
      }
      SUMA_free(Opt->dvec); Opt->dvec = NULL; /* this vector is not even created in SUMA_ISO_CMASK mode ...*/   
   } else {
      SUMA_SL_Err("Bad Opt->xform.");
      SUMA_RETURN(NOPE);
   }
   
   if ( Opt->ninmask  <= 0 ) {
	   SUMA_SL_Err("No isovolume found!\nNothing to do." );
	   SUMA_RETURN(NOPE);
	}
   
   if (Opt->debug > 0) {
      fprintf( SUMA_STDERR, "%s:\nInput dset %s has nvox = %d, nvals = %d",
		                        FuncName, Opt->in_name, Opt->nvox, DSET_NVALS(Opt->in_vol) );
	   fprintf( SUMA_STDERR, " (%d voxels in mask)\n", Opt->ninmask );
   }
   
   SUMA_RETURN(YUP);
}

#ifdef SUMA_IsoSurface_STANDALONE

static char SUMA_ISO_Obj_Types[][10] = { {"Cushin"}, {"Sphere"}, {"Plane"}, {"Cassini"}, {"Blooby"}, {"Chair"}, {"Cyclide"}, {"2 Torus"}, {"mc case"}, {"Drip"} };

void usage_SUMA_IsoSurface ()
   {
      static char FuncName[]={"usage_SUMA_IsoSurface"};
      char * s = NULL;
      int i;
      s = SUMA_help_basics();
      printf ( "\n"
               "Usage: A program to perform isosurface extraction from a volume.\n"
               "  Based on code by Thomas Lewiner (see below).\n"
               "\n"
               "  IsoSurface  < -input VOL | -shape S GR >\n"
               "              < -isoval V | -isorange V0 V1 | -isocmask MASK_COM >\n"
               "              [< -o_TYPE PREFIX>]\n"
               "              [< -debug DBG >]\n"  
               "\n"
               "  Mandatory parameters:\n"
               "     You must use one of the following two options:\n"
               "     -input VOL: Input volume.\n"
               "     -shape S GR: Built in shape.\n"
               "                  where S is the shape number, \n"
               "                  between 0 and 9 (see below). \n"
               "                  and GR is the grid size (like 64).\n"
               "                  If you use -debug 1 with this option\n"
               "                  a .1D volume called mc_shape*.1D is\n"
               "                  written to disk. Watch the debug output\n"
               "                  for a command suggesting how to turn\n"
               "                  this 1D file into a BRIK volume for viewing\n"
               "                  in AFNI.\n"
               "     You must use one of the following iso* options:\n"
               "     -isoval V: Create isosurface where volume = V\n"
               "     -isorange V0 V1: Create isosurface where V0 <= volume < V1\n"
               "     -isocmask MASK_COM: Create isosurface where MASK_COM != 0\n"
               "        For example: -isocmask '-a VOL+orig -expr (1-bool(a-V))' \n"
               "        is equivalent to using -isoval V. \n"
               "     NOTE: -isorange and -isocmask are only allowed with -xform mask\n"
               "            See -xform below for details.\n"
               "\n"
               "  Optional Parameters:\n"
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
               "     -o_TYPE PREFIX: prefix of output surface.\n"
               "        where TYPE specifies the format of the surface\n"
               "        and PREFIX is, well, the prefix.\n"
               "        TYPE is one of: fs, 1d (or vec), sf, ply.\n"
               "        Default is: -o_ply \n"
               "\n"
               "     -debug DBG: debug levels of 0 (default), 1, 2, 3.\n"
               "        This is no Rick Reynolds debug, which is oft nicer\n"
               "        than the results, but it will do.\n"
               "\n"
               "  Built In Shapes:\n"); 
   for (i=0; i<10;++i) {
      printf(  "     %d: %s\n", i, SUMA_ISO_Obj_Types[i]);   
   }
   printf (    "\n" 
               "  NOTE:\n"
               "  The code for the heart of this program is a translation of:\n"
               "  Thomas Lewiner's C++ implementation of the algorithm in:\n"
               "  Efficient Implementation of Marching Cubes´ Cases with Topological Guarantees\n"
               "  by Thomas Lewiner, Hélio Lopes, Antônio Wilson Vieira and Geovan Tavares \n"
               "  in Journal of Graphics Tools. \n"
               "  http://www-sop.inria.fr/prisme/personnel/Thomas.Lewiner/JGT.pdf\n"
               "\n"
               "%s"
               "\n", s);
       SUMA_free(s); s = NULL;        
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
       exit (0);
   }
/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_ISOSURFACE_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_ISOSURFACE_OPTIONS *SUMA_IsoSurface_ParseInput (char *argv[], int argc)
{
   static char FuncName[]={"SUMA_IsoSurface_ParseInput"}; 
   SUMA_ISOSURFACE_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outname;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_ISOSURFACE_OPTIONS *)SUMA_malloc(sizeof(SUMA_ISOSURFACE_OPTIONS));
   
   kar = 1;
   Opt->spec_file = NULL;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->N_surf = -1;
   Opt->in_name = NULL;
   Opt->cmask = NULL;
   Opt->MaskMode = SUMA_ISO_UNDEFINED;
   for (i=0; i<ISOSURFACE_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
   Opt->in_vol = NULL;
   Opt->nvox = -1;
   Opt->ninmask = -1;
   Opt->mcdatav = NULL;
   Opt->debug = 0;
   Opt->v0 = 0.0;
   Opt->v1 = -1.0;
   Opt->dvec = NULL;
   Opt->SurfFileType = SUMA_PLY;
   Opt->SurfFileFormat = SUMA_ASCII;
   Opt->xform = SUMA_ISO_XFORM_MASK;
   Opt->obj_type = -1;
   Opt->obj_type_res = -1;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_IsoSurface();
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
            fprintf (SUMA_STDERR, "%s is a bad parameter for -xform option. \n", argv[kar]);
				exit (1);
         }
         brk = YUP;
      }
      
      #if 0 /* no support yet */
      if (!brk && (strcmp(argv[kar], "-o_bv") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -o_bv \n");
				exit (1);
			}
         Opt->out_prefix = SUMA_copy_string(argv[kar]);
         Opt->SurfFileType = SUMA_BRAIN_VOYAGER;
         Opt->SurfFileFormat = SUMA_BINARY;
			brk = YUP;
		}
      #endif
      if (!brk && (strcmp(argv[kar], "-o_fs") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -o_fs \n");
				exit (1);
			}
         Opt->out_prefix = SUMA_copy_string(argv[kar]);
         Opt->SurfFileType = SUMA_FREE_SURFER;
         Opt->SurfFileFormat = SUMA_ASCII;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-o_sf") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -o_sf \n");
				exit (1);
			}
         Opt->out_prefix = SUMA_copy_string(argv[kar]);
         Opt->SurfFileType = SUMA_SUREFIT;
         Opt->SurfFileFormat = SUMA_ASCII;
			brk = YUP;
		}
      
      if (!brk && ( (strcmp(argv[kar], "-o_vec") == 0) || (strcmp(argv[kar], "-o_1d") == 0) ) ) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -o_1d or -o_vec \n");
				exit (1);
			}
         Opt->out_prefix = SUMA_copy_string(argv[kar]);
         Opt->SurfFileType = SUMA_VEC;
         Opt->SurfFileFormat = SUMA_ASCII;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-o_ply") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -o_ply \n");
				exit (1);
			}
         Opt->out_prefix = SUMA_copy_string(argv[kar]);
         Opt->SurfFileType = SUMA_PLY;
         Opt->SurfFileFormat = SUMA_FF_NOT_SPECIFIED;
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
            fprintf (SUMA_STDERR, "range values wrong. Must have %f <= %f \n", Opt->v0, Opt->v1);
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
      
      if (!brk && (strcmp(argv[kar], "-shape") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 arguments after -shape \n");
				exit (1);
			}
			Opt->obj_type = atoi(argv[kar]); kar ++;
         Opt->obj_type_res = atoi(argv[kar]);
         if (Opt->obj_type < 0 || Opt->obj_type > 9) {
            fprintf (SUMA_STDERR, "Error %s:\nShape number (S) must be between 0 and 9. I have %d\n", FuncName, Opt->obj_type);
            exit (1);
         }
         if (Opt->obj_type_res < 0) {
            fprintf (SUMA_STDERR, "Error %s:\nShape grid resolution (GR) must be > 0 . I have %d\n", FuncName, Opt->obj_type_res);
            exit (1);
         }
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
   
   if (Opt->in_name && Opt->obj_type >= 0) {
      fprintf (SUMA_STDERR,"Error %s:\nOptions -input and -shape are mutually exclusive.\n", FuncName);
      exit(1);
   }
   if (!Opt->in_name && Opt->obj_type < 0) {
      fprintf (SUMA_STDERR,"Error %s:\nEither -input or -shape options must be used.\n", FuncName);
      exit(1);
   }
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("isosurface_out");
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
   static char FuncName[]={"IsoSurface"}; 
	int i;
   void *SO_name=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_ISOSURFACE_OPTIONS *Opt;  
   char  stmp[200];
   SUMA_Boolean exists = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;
   
   
   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   if (argc < 2) {
      usage_SUMA_IsoSurface();
      exit (1);
   }
   
   Opt = SUMA_IsoSurface_ParseInput (argv, argc);
   
   set_suma_debug(Opt->debug);
   
   SO_name = SUMA_Prefix2SurfaceName(Opt->out_prefix, NULL, NULL, Opt->SurfFileType, &exists);
   if (exists) {
      fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, Opt->out_prefix);
      exit(1);
   }
   
   if (Opt->obj_type < 0) {
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
            fprintf(fout,  "#Col. 0 Voxel Index\n"
                           "#Col. 1 Is a mask (all values here should be 1)\n" );
            for (i=0; i<Opt->nvox; ++i) {
               if (Opt->mcdatav[i]) {
                  fprintf(fout,"%d %.2f\n", i, Opt->mcdatav[i]);
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
               fprintf(fout,"%d %.2f\n", i, Opt->mcdatav[i]);
            }
            fclose(fout); fout = NULL;
         }
      }
   } else {
      if (Opt->debug) {
         SUMA_S_Note("Using built in types...");
      }
   }
   /* Now call Marching Cube functions */
   if (!(SO = SUMA_MarchingCubesSurface(Opt))) {
      SUMA_S_Err("Failed to create surface.\n");
      exit(1);
   }

   /* write the surface to disk */
   if (!SUMA_Save_Surface_Object (SO_name, SO, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
      exit (1);
   }
   
   if (Opt->dvec) SUMA_free(Opt->dvec); Opt->dvec = NULL;
   if (Opt->mcdatav) {SUMA_free(Opt->mcdatav); Opt->mcdatav = NULL;} 
   if (Opt->in_vol) { DSET_delete( Opt->in_vol); Opt->in_vol = NULL;} 
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt) SUMA_free(Opt);
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   if (SO_name) SUMA_free(SO_name); SO_name = NULL;
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
}
#endif      
