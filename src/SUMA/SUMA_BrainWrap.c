#include "SUMA_suma.h"
#include "../thd_brainormalize.h"

#undef STAND_ALONE

#if defined SUMA_BrainWrap_STANDALONE
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

/* 
   volume returned is negative if failed to read volume. 
   Volume units are in mm3
*/         
float SUMA_LoadPrepInVol (SUMA_ISOSURFACE_OPTIONS *Opt)
{
   static char FuncName[]={"SUMA_LoadPrepInVol"};
   int i;
   float vol=-1;
   
   SUMA_ENTRY;
   
   Opt->in_vol = THD_open_dataset( Opt->in_name );
   if (!ISVALID_DSET(Opt->in_vol)) {
      if (!Opt->in_name) {
         SUMA_SL_Err("NULL input volume.");
         SUMA_RETURN(vol);
      } else {
         SUMA_SL_Err("invalid volume.");
         SUMA_RETURN(vol);
      }
   } else if ( DSET_BRICK_TYPE(Opt->in_vol, 0) == MRI_complex) {
      SUMA_SL_Err("Can't do complex data.");
      SUMA_RETURN(vol);
   }
   
   Opt->nvox = DSET_NVOX( Opt->in_vol );
   if (DSET_NVALS( Opt->in_vol) != 1) {
      SUMA_SL_Err("Input volume can only have one sub-brick in it.\nUse [.] selectors to choose sub-brick needed.");
      SUMA_RETURN(vol);
   }
   
   Opt->VolCM[0] = XCM; Opt->VolCM[1] = YCM; Opt->VolCM[2] = ZCM;
   
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
   
   /* estimate the volume */
   vol = 0.0;
   for (i=0; i<Opt->nvox; ++i) { if (Opt->dvec[i]) ++vol; }
   
   vol *= fabs(DSET_DX(Opt->in_vol) * DSET_DY(Opt->in_vol) * DSET_DZ(Opt->in_vol) );
   
   
   SUMA_RETURN(vol);
}

/*!
   finds the mean coordinate of the neighboring nodes (not including node i)
*/
#define SUMA_MEAN_NEIGHB_COORD(SO, i, mnc) { \
   int m_j, m_in; \
   mnc[0] = 0; mnc[1] = 0; mnc[2] = 0; \
   for (m_j = 0; m_j < SO->FN->N_Neighb[i]; ++m_j) {  \
      m_in = 3*SO->FN->FirstNeighb[i][m_j]; \
      mnc[0] += SO->NodeList[m_in]; \
      mnc[1] += SO->NodeList[m_in+1]; \
      mnc[2] += SO->NodeList[m_in+2]; \
   }  \
   mnc[0] = mnc[0] / (SO->FN->N_Neighb[i]); mnc[1] = mnc[1] / (SO->FN->N_Neighb[i]);  mnc[2] = mnc[2] / (SO->FN->N_Neighb[i]); \
}
/*!
   find the mean segment length 
*/
#define SUMA_MEAN_SEGMENT_LENGTH(SO, l) { \
   int m_cnt=0, m_j;   \
   float *m_p1, *m_p2, m_d=0.0, m_v[3];  \
   for (m_j=0; m_j < SO->EL->N_EL; ++m_j) {  \
      if (SO->EL->ELps[m_j][2] >= 0) { \
         m_p1 = &(SO->NodeList[SO->EL->EL[m_j][0]]); m_p2 = &(SO->NodeList[SO->EL->EL[m_j][1]]); \
         SUMA_UNIT_VEC(m_p1, m_p2, m_v, m_d);   \
         l += m_d;   \
         ++m_cnt; \
      }  \
   }  \
   l /= m_cnt; \
}

/*!
   Based on BET's recipe 
*/
int SUMA_Find_IminImax (SUMA_SurfaceObject *SO, SUMA_ISOSURFACE_OPTIONS *Opt, int ni, float t2, float tm, float *Iminp, float *Imaxp )
{
   static char FuncName[]={"SUMA_Find_IminImax"};
   float Imax, Imin;
   
   SUMA_ENTRY;
   
   
   SUMA_RETURN(YUP);
}

/*!
   Based on BET's recipe 
*/
int SUMA_StretchToFitLeCerveau (SUMA_SurfaceObject *SO, SUMA_ISOSURFACE_OPTIONS *Opt)
{
   static char FuncName[]={"SUMA_StretchToFitLeCerveau"};
   int it=0, N_it=200, in=0, ii;
   float mnc[3], s[3], sn[3], st[3], dp, 
         nsn, rmin, rmax, E, F, su1, su2, 
         r, u1[3], u2[3], u3[3], u[3],
         tm=0.0, t2 = 0.0, t98 = 0.0, Imin, Imax, l,
         *n=NULL;
   
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   /* estimate the t2, t98 and tm parameters, should do them regionally by Bob's octant method */
   ii = -1;
   do {
      ++ii;
      if (Opt->dvec[ii]) { tm = t2 = t98 = Opt->dvec[ii]; }
   } while (Opt->dvec[ii] && ii < Opt->nvox -1);
   
   
   rmin = 3.33; rmax = 10.0;  E = (1/rmin + 1/rmax)/2; F = 6/(1/rmin - 1/rmax); 
   su1 = 0.5; 
   for (it=0; it < N_it; ++it) {
      SUMA_MEAN_SEGMENT_LENGTH(SO, l);
      for (in=0; in<SO->N_Node; ++in) {
         SUMA_MEAN_NEIGHB_COORD(SO, in, mnc);
         n = &(SO->NodeList[3*in]);
         s[0] = mnc[0] - n[0]; s[1] = mnc[1] - n[1]; s[2] = mnc[2] - n[2];
         SUMA_DOTP_VEC(s, &(SO->NodeNormList[3*in]), dp, 3, float, float);  /* dp is the dot product of s with the normal at in */ 
         SUMA_SCALE_VEC(&(SO->NodeNormList[3*in]), sn, dp, 3, float, float); /* sn is the component of s along normal */
         SUMA_NORM_VEC(sn, 3, nsn); /* norm of sn */
         SUMA_SUB_VEC(s, sn, st, 3, float, float, float);  /* st is the tangential component of s along normal */
         SUMA_SCALE_VEC(st, u1, su1, 3, float, float); /* u1 = st * su1 */
         
         r = ( l * l ) / (2.0 * nsn);
         su2 = ( 1 + tanh(F*(1/r - E)))/2.0;
         SUMA_SCALE_VEC(sn, u2, su2, 3, float, float); /* u2 = sn * su2 */
         
         SUMA_Find_IminImax(SO, Opt, in, t2, tm, &Imin, &Imax);         
      } /* loop over number of nodes */
   } /* loop over number of iterations */
   
   SUMA_RETURN(YUP);
}

#ifdef SUMA_BrainWrap_STANDALONE
void usage_SUMA_BrainWrap (SUMA_GENERIC_ARGV_PARSE *ps)
   {
      static char FuncName[]={"usage_SUMA_BrainWrap"};
      char * s = NULL, *sio=NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: A program to find the convex hull of a set of points.\n"
               "  This program is a wrapper for the Qhull program.\n"
               "  see copyright notice by running suma -sources.\n"
               "\n"
               "  BrainWrap  \n"
               "     usage 1: < -input VOL >\n"
               "              < -isoval V | -isorange V0 V1 | -isocmask MASK_COM >\n"
               "              [<-xform XFORM>]\n"
               "     usage 2: < i_TYPE input surface >\n"
               "              [<-sv SURF_VOL>]\n"
               "     usage 3: < -input_1D XYZ >\n"      
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
               "     -input_1D XYZ: Construct the convex hull of the points\n"
               "                    contained in 1D file XYZ. If the file has\n"
               "                    more than 3 columns, use AFNI's [] selectors\n"
               "                    to specify the XYZ columns.\n"
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
SUMA_ISOSURFACE_OPTIONS *SUMA_BrainWrap_ParseInput (char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_BrainWrap_ParseInput"}; 
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
   Opt->XYZ = NULL;
   Opt->in_1D = NULL;
   Opt->N_XYZ = 0;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_BrainWrap(ps);
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
      
      if (!brk && (strcmp(argv[kar], "-input_1D") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input_1D \n");
				exit (1);
			}
			Opt->in_1D = argv[kar];
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
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   /* transfer some options to Opt from ps. Clunky because this is retrofitting */
   if (ps->o_N_surfnames) {
      Opt->out_prefix = SUMA_copy_string(ps->o_surfnames[0]);
      Opt->SurfFileType = ps->o_FT[0];
      Opt->SurfFileFormat = ps->o_FF[0];
   }
   
   if (ps->i_N_surfnames) {
      if (Opt->in_name || Opt->in_1D) {
         fprintf (SUMA_STDERR,"Error %s:\nOptions -i_TYPE, -input and -input_1D are mutually exclusive.\n", FuncName);
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
   
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("brainwrap_out");
   
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
   static char FuncName[]={"BrainWrap"}; 
	int i, i3;
   void *SO_name=NULL;
   float vol;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_ISOSURFACE_OPTIONS *Opt;  
   char  stmp[200];
   SUMA_Boolean exists = NOPE;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean LocalHead = YUP;

	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;
   
   
   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-i;-sv;");
   
   if (argc < 2) {
      usage_SUMA_BrainWrap(ps);
      exit (1);
   }
   
   Opt = SUMA_BrainWrap_ParseInput (argv, argc, ps);
   SO_name = SUMA_Prefix2SurfaceName(Opt->out_prefix, NULL, NULL, Opt->SurfFileType, &exists);
   if (exists) {
      fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, Opt->out_prefix);
      exit(1);
   }
   
   /* Load the AFNI volume 
   This volume must have already been cleaned up by SpatNorm */
   vol = SUMA_LoadPrepInVol (Opt);
   if ( vol <= 0 ) {
      SUMA_S_Err("Failed to load/prep volume");
      exit(1);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Got me a volume of %f mm3\n", FuncName, vol);
   }
   
   
   /* Now create that little sphere that is about to expand */
   SO = SUMA_CreateIcosahedron (pow(vol*3.14159/4.0, 1/3.0)/3.0, 50, Opt->VolCM, "n", 1);
   if (!SO) {
      SUMA_S_Err("Failed to create Icosahedron");
      exit(1);
   }
   if (LocalHead) SUMA_Print_Surface_Object (SO, NULL);      
   
   /* This is it baby, start walking */
   SUMA_StretchToFitLeCerveau (SO, Opt);
   
   /* write the surface to disk */
   if (!SUMA_Save_Surface_Object (SO_name, SO, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
      exit (1);
   }
   
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt->dvec) SUMA_free(Opt->dvec); Opt->dvec = NULL;
   if (Opt->mcdatav) {SUMA_free(Opt->mcdatav); Opt->mcdatav = NULL;} 
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
#endif
