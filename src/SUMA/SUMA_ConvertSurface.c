#include "SUMA_suma.h"
#ifdef USE_DECOMPOSE_SHOEMAKE
#include "Decompose.c"           /* For testing, put it in library if you'll be using it */
#endif

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_SUMA_ConvertSurface (SUMA_GENERIC_ARGV_PARSE *ps)
   
  {/*Usage*/
          static char FuncName[]={"usage_SUMA_ConvertSurface"};
          char * s = NULL, *sio=NULL;
          
          s = SUMA_help_basics();
          sio  = SUMA_help_IO_Args(ps);
         
          printf (
"\n"
"Usage:  ConvertSurface <-i_TYPE inSurf> <-o_TYPE outSurf> \n"
"                       [<-sv SurfaceVolume [VolParam for sf surfaces]>] \n"
"                       [-tlrc] [-MNI_rai/-MNI_lpi][-xmat_1D XMAT]\n"
"    reads in a surface and writes it out in another format.\n"
"    Note: This is a not a general utility conversion program. \n"
"    Only fields pertinent to SUMA are preserved.\n"
"%s"
"\n"
"  Alternate GIFTI output qualifiers:\n"
"     You can alternately set gifti data arrays encoding with:\n"
"        -xml_ascii: For ASCII  (human readable)\n"
"        -xml_b64:   For Base64 (more compact)\n"
"        -xml_b64gz: For Base64 GZIPPED (most compact, needs gzip libraries)\n"
"     If AFNI_NIML_TEXT_DATA environment variable is set to YES, the\n"
"     the default is -xml_ascii, otherwise it is -xml_b64\n" 
"\n"
/*                  "    -i_TYPE inSurf specifies the input surface, TYPE is one of the following:\n"
"       fs: FreeSurfer surface. \n"
"           If surface name has .asc it is assumed to be\n"
"           in ASCII format. Otherwise it is assumed to be\n"
"           in BINARY_BE (Big Endian) format.\n"
"           Patches in Binary format cannot be read at the moment.\n"
"       sf: SureFit surface. \n"
"           You must specify the .coord followed by the .topo file.\n"
"       vec (or 1d): Simple ascii matrix format. \n"
"            You must specify the NodeList file followed by the FaceSetList file.\n"
"            NodeList contains 3 floats per line, representing X Y Z vertex coordinates.\n"
"            FaceSetList contains 3 ints per line, representing v1 v2 v3 triangle vertices.\n"
"       ply: PLY format, ascii or binary.\n"
"            Only vertex and triangulation info is preserved.\n"
"       bv: BrainVoyager format. \n"
"           Only vertex and triangulation info is preserved.\n"
"    -ipar_TYPE ParentSurf specifies the parent surface. Only used\n"
"            when -o_fsp is used, see below.\n"
"    -o_TYPE outSurf specifies the output surface, TYPE is one of the following:\n"
"       fs: FreeSurfer ascii surface. \n"
"       fsp: FeeSurfer ascii patch surface. \n" 
"            In addition to outSurf, you need to specify\n"
"            the name of the parent surface for the patch.\n"
"            using the -ipar_TYPE option\n"
"       sf: SureFit surface. \n"
"           You must specify the .coord followed by the .topo file.\n"
"       vec (or 1D): Simple ascii matrix format. \n"
"            see help for vec under -i_TYPE options for format specifications.\n"
"       ply: PLY format, ascii or binary.\n"
"    -sv SurfaceVolume [VolParam for sf surfaces]\n"
"       This option must not come before the -i_TYPE option.\n"
"       If you supply a surface volume, the coordinates of the input surface.\n"
"        are modified to SUMA's convention and aligned with SurfaceVolume.\n"
"        You must also specify a VolParam file for SureFit surfaces.\n" */
"    -orient_out STR: Output coordinates in STR coordinate system. \n"
"                      STR is a three character string following AFNI's \n"
"                      naming convention. The program assumes that the   \n"
"                      native orientation of the surface is RAI, unless you \n"
"                      use the -MNI_lpi option. The coordinate transformation\n"
"                      is carried out last, just before writing the surface \n"
"                      to disk.\n"
"    -native: Write the output surface in the coordinate system native to its\n"
"             format.\n"
"             Option makes sense for BrainVoyager, Caret/SureFit and \n"
"             FreeSurfer surfaces.\n"
"             But the implementation for Caret/Surefit is not finished yet \n"
"             (ask if needed).\n" 
"    -make_consistent: Check the consistency of the surface's mesh (triangle\n"
"                      winding). This option will write out a new surface \n"
"                      even if the mesh was consistent.\n"
"                      See SurfQual -help for mesh checks.\n"
"    -radial_to_sphere rad: Push each node along the center-->node direction\n"
"                           until |center-->node| = rad.\n"
"    -acpc: Apply acpc transform (which must be in acpc version of \n"
"        SurfaceVolume) to the surface vertex coordinates. \n"
"        This option must be used with the -sv option.\n"
"    -tlrc: Apply Talairach transform (which must be a talairach version of \n"
"        SurfaceVolume) to the surface vertex coordinates. \n"
"        This option must be used with the -sv option.\n"
"    -MNI_rai/-MNI_lpi: Apply Andreas Meyer Lindenberg's transform to turn \n"
"        AFNI tlrc coordinates (RAI) into MNI coord space \n"
"        in RAI (with -MNI_rai) or LPI (with -MNI_lpi)).\n"
"        NOTE: -MNI_lpi option has not been tested yet (I have no data\n"
"        to test it on. Verify alignment with AFNI and please report\n"
"        any bugs.\n" 
"        This option can be used without the -tlrc option.\n"
"        But that assumes that surface nodes are already in\n"
"        AFNI RAI tlrc coordinates .\n"    
"   NOTE: The vertex coordinates coordinates of the input surfaces are only\n"
"         transformed if -sv option is used. If you do transform surfaces, \n"
"         take care not to load them into SUMA with another -sv option.\n"
"\n"
"    -patch2surf: Change a patch, defined here as a surface with a mesh that\n"
"                 uses only a subset of the full nodelist, to a surface\n"
"                 where all the nodes in nodelist are used in the mesh.\n"
"                 Note that node indices will no longer correspond between\n"
"                 the input patch and the output surface.\n"
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
"    -xcenter x y z: Use vector cen = [x y z]' for rotation center.\n"
"                    Default is cen = [0 0 0]'\n"
"    -polar_decomp: Apply polar decomposition to mat and preserve\n"
"                   orthogonal component and shift only. \n"
"                   For more information, see cat_matvec's -P option.\n"
"                   This option can only be used in conjunction with\n"
"                   -xmat_1D\n"
"\n"
"%s\n"
                  , sio, s); SUMA_free(sio); sio = NULL; SUMA_free(s); s = NULL; 
          s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL; 
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \t Wed Jan  8 13:44:29 EST 2003 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ConvertSurface"}; 
	int kar, volexists, i, Doinv;
   float DoR2S;
   double xcen[3];
   double xform[4][4];
   char  *if_name = NULL, *of_name = NULL, *if_name2 = NULL, 
         *of_name2 = NULL, *sv_name = NULL, *vp_name = NULL,
         *OF_name = NULL, *OF_name2 = NULL, *tlrc_name = NULL,
         *acpc_name=NULL, *xmat_name = NULL, *ifpar_name = NULL, 
         *ifpar_name2 = NULL;
   SUMA_SO_File_Type iType = SUMA_FT_NOT_SPECIFIED, 
                     iparType = SUMA_FT_NOT_SPECIFIED,
                     oType = SUMA_FT_NOT_SPECIFIED;
   SUMA_SO_File_Format iForm = SUMA_FF_NOT_SPECIFIED, 
                        iparForm = SUMA_FF_NOT_SPECIFIED, 
                        oFormat = SUMA_FF_NOT_SPECIFIED;
   SUMA_SurfaceObject *SO = NULL, *SOpar = NULL, *SOsurf = NULL;
   SUMA_PARSED_NAME *of_name_strip = NULL, *of_name2_strip = NULL;
   SUMA_SFname *SF_name = NULL;
   void *SO_name = NULL;
   char orsurf[6], orcode[6];
   THD_warp *warp=NULL ;
   THD_3dim_dataset *aset=NULL;
   SUMA_Boolean brk, Do_tlrc, Do_mni_RAI, Do_mni_LPI, Do_acpc, Docen;
   SUMA_Boolean Doxmat, Do_wind, Do_p2s, onemore, Do_native, Do_PolDec;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean exists;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
	
   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-i;-sv;-ipar;");
   
   if (argc < 4)
       {
          usage_SUMA_ConvertSurface (ps);
          exit (1);
       }
   
   
   kar = 1;
   xmat_name = NULL;
   xcen[0] = 0.0; xcen[1] = 0.0; xcen[2] = 0.0;
	brk = NOPE;
   orcode[0] = '\0'; 
   sprintf(orsurf,"RAI");
   Docen = NOPE;
   Doxmat = NOPE;
   Do_tlrc = NOPE;
   Do_mni_RAI = NOPE;
   Do_mni_LPI = NOPE;
   Do_acpc = NOPE;
   Do_wind = NOPE;
   Do_p2s = NOPE;
   Do_native = NOPE;
   DoR2S = 0.0;
   Do_PolDec = NOPE;
   Doinv = 0;
   onemore = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_ConvertSurface(ps);
          exit (0);
		}
		
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      SUMA_TO_LOWER(argv[kar]);
		      
      if (!brk && (strcmp(argv[kar], "-xmat_1d") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -xmat_1D\n");
				exit (1);
			}
			xmat_name = argv[kar]; 
         Doxmat = YUP;
         Doinv = 0;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-ixmat_1d") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -ixmat_1D\n");
				exit (1);
			}
			xmat_name = argv[kar]; 
         Doxmat = YUP;
         Doinv = 1;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-polar_decomp") == 0)) {
         Do_PolDec = YUP;
			brk = YUP;
		}
      
      
      if (!brk && (strcmp(argv[kar], "-make_consistent") == 0)) {
         Do_wind = YUP;
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
         Docen = YUP;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-native") == 0)) {
         Do_native = YUP;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-orient_out") == 0)) {
         kar ++;
			if (kar>= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -orient_out\n");
				exit (1);
			}
			snprintf(orcode, 4*sizeof(char), "%s", argv[kar]);
         if (!SUMA_ok_orstring(orcode)) {
            fprintf (SUMA_STDERR, "%s is a bad orientation string\n", orcode);
				exit (1);
         } 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-radial_to_sphere") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -radial_to_sphere\n");
				exit (1);
			}
         DoR2S = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-patch2surf") == 0)) {
         Do_p2s = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-xml_ascii") == 0)) {
         oFormat = SUMA_XML_ASCII_SURF;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-xml_b64") == 0)) {
         oFormat = SUMA_XML_B64_SURF;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-xml_b64gz") == 0)) {
         oFormat = SUMA_XML_B64GZ_SURF;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-tlrc") == 0)) {
         Do_tlrc = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-acpc") == 0)) {
         Do_acpc = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-mni_rai") == 0)) {
         Do_mni_RAI = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-mni_lpi") == 0)) {
         Do_mni_LPI = YUP;
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
   /* transfer info from ps structure (backward compat) */

   if (ps->o_N_surfnames) {
      of_name = ps->o_surfnames[0];
      of_name2 = ps->o_surftopo[0];
      oType = ps->o_FT[0];
      if (oFormat == SUMA_FF_NOT_SPECIFIED) {
         oFormat = ps->o_FF[0];
      }
   }
   if (ps->i_N_surfnames) {
      if_name = ps->i_surfnames[0];
      if_name2 = ps->i_surftopo[0];
      iType = ps->i_FT[0];
      iForm = ps->i_FF[0];
   }
   if (ps->ipar_N_surfnames) {
      ifpar_name = ps->ipar_surfnames[0];
      ifpar_name2 = ps->ipar_surftopo[0];
      iparType = ps->ipar_FT[0];
      iparForm = ps->ipar_FF[0];
   }
   
   if (ps->N_sv) sv_name = ps->sv[0];
   if (ps->N_vp) vp_name = ps->vp[0];
         
   /* sanity checks */
   if (Do_native && orcode[0] != '\0') {
      SUMA_S_Err("Options -native and -orient_out are mutually exclusive");
      exit(1);   
   }
   
   if (Do_mni_LPI && Do_mni_RAI) {
      fprintf (SUMA_STDERR,"Error %s:\nCombining -MNI_lpi and -MNI_rai options.\nNot good.", FuncName);
      exit(1);
   }
   
   if (!if_name) {
      fprintf (SUMA_STDERR,"Error %s: input surface not specified.\n", FuncName);
      exit(1);
   }
   if (!of_name) {
      fprintf (SUMA_STDERR,"Error %s: output surface not specified.\n", FuncName);
      exit(1);
   }
   if (iType == SUMA_FT_NOT_SPECIFIED) {
      fprintf (SUMA_STDERR,"Error %s: input type not recognized.\n", FuncName);
      exit(1);
   }
   if (oType == SUMA_FT_NOT_SPECIFIED) {
      fprintf (SUMA_STDERR,"Error %s: output type not recognized.\n", FuncName);
      exit(1);
   }
   if (  oType != SUMA_GIFTI && 
         oFormat >= SUMA_XML_SURF && 
         oFormat <= SUMA_XML_B64GZ_SURF){
      fprintf (SUMA_STDERR,
               "Error %s: XML output options only valid with -o_gii\n", 
               FuncName);
      exit(1);
   }
   if (iType == SUMA_SUREFIT) {
      if (!if_name2) {
         fprintf (SUMA_STDERR,
                  "Error %s: input SureFit surface incorrectly specified.\n", 
                  FuncName);
         exit(1);
      }
      if (sv_name && !vp_name) {
         fprintf (SUMA_STDERR,
                  "Error %s: VolParent must specified with -sv potion for SureFit surfaces. \n", 
                  FuncName);
         exit(1);
      }
   }
   if (iType == SUMA_VEC) {
      if (!if_name2) {
         fprintf (SUMA_STDERR,"Error %s: input vec surface incorrectly specified.\n", FuncName);
         exit(1);
      }
   }

   if (( Do_mni_RAI || Do_mni_LPI) && !Do_tlrc) {
      SUMA_SL_Warn ("I hope you know what you're doing.\nThe MNI transform should only be applied to a\nSurface in the AFNI tlrc coordinate space.\n");
   }
   
   if (Do_acpc && Do_tlrc) {
      fprintf (SUMA_STDERR,"Error %s: You can't do -tlrc and -acpc simultaneously.\n", FuncName);
      exit(1);
   }
   
   if ((Doxmat || Docen) && (Do_acpc || Do_tlrc)) {
      fprintf (SUMA_STDERR,"Error %s: You can't do -tlrc or -acpc with -xmat_1D and -xcenter.\n", FuncName);
      exit(1);
   }
   
   if ((!Doxmat && Docen)) {
      fprintf (SUMA_STDERR,"Error %s: You can't use -xcenter without -xmat_1D.\n", FuncName);
      exit(1);
   }
   if (oType == SUMA_SUREFIT) {
      if (!of_name2) {
       fprintf (SUMA_STDERR,"Error %s: output SureFit surface incorrectly specified. \n", FuncName);
       exit(1);
      }
   }
   
   if (oType == SUMA_VEC) {
      if (!of_name2) {
       fprintf (SUMA_STDERR,"Error %s: output vec surface incorrectly specified. \n", FuncName);
       exit(1);
      }
   }
   
   
   /* test for existence of input files */
   if (!SUMA_filexists(if_name)) {
      fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, if_name);
      exit(1);
   }
   
   if (if_name2) {
      if (!SUMA_filexists(if_name2)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, if_name2);
         exit(1);
      }
   }

   if (ifpar_name2) {
      if (!SUMA_filexists(ifpar_name2)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, ifpar_name2);
         exit(1);
      }
   }
   
   if (ifpar_name) {
      if (!SUMA_filexists(ifpar_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, ifpar_name);
         exit(1);
      }
   }
   
   if (xmat_name) {
      if (!SUMA_filexists(xmat_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, xmat_name);
         exit(1);
      }
   } else {
      if (Do_PolDec) {
         SUMA_S_Err("-polar_decomp is useless without -xmat_1D");
         exit(1);
      }
   }

   if (sv_name) {
      char *head = NULL, view[10];
      head = SUMA_AfniPrefix(sv_name, view, NULL, &volexists);
      if (!SUMA_AfniExistsView(volexists, view)) {
         fprintf (SUMA_STDERR,
                  "Error %s: volume %s not found.\n", FuncName, head);
         exit(1);
      }
      if (head) SUMA_free(head); head = NULL;
   }
   
  
   if ((Do_tlrc || Do_acpc) && (!sv_name)) {
      fprintf (SUMA_STDERR,
               "Error %s: -tlrc must be used with -sv option.\n", FuncName);
      exit(1);
   }
   
   if (vp_name) {
      if (!SUMA_filexists(vp_name)) {
         fprintf (SUMA_STDERR,
                  "Error %s: %s not found.\n", FuncName, vp_name);
         exit(1);
      }
   }

   /* check for existence of output files */
   if (of_name2) {
      SUMA_SFname *SFname;

      SO_name = SUMA_2Prefix2SurfaceName (of_name, of_name2, NULL, 
                                          vp_name, oType, &exists);
      SFname = (SUMA_SFname *)SO_name;
      OF_name2 = SUMA_copy_string(SFname->name_topo);
      OF_name = SUMA_copy_string(SFname->name_coord);
   } else {
      SO_name = SUMA_Prefix2SurfaceName (of_name, NULL, vp_name, oType, &exists);
      OF_name = SUMA_copy_string((char *) SO_name);
   }
   
   if (exists && !THD_ok_overwrite()) {
      if (OF_name2) 
         fprintf (SUMA_STDERR,
                  "Error %s: output file(s) %s and/or %s exist already.\n", 
                  FuncName, OF_name, OF_name2);
      else fprintf ( SUMA_STDERR,
                     "Error %s: output file %s exists already.\n", 
                     FuncName, OF_name);
      exit(1);
   }
      
   /* now for the real work */
   if (Doxmat) {
      MRI_IMAGE *im = NULL;
      double *far=NULL;
      int nrow, ncol;
      
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
      
      if (LocalHead) {
         fprintf(SUMA_STDERR,"\n++ ConvertSurface xform:\n");
         for (i=0; i < 4; ++i) {
            fprintf(SUMA_STDERR," %+.5f\t%+.5f\t%+.5f\t%+.5f\n",
                   xform[i][0], xform[i][1], 
                   xform[i][2], xform[i][3]);  
         }
         fprintf(SUMA_STDERR,"\n");
      }
      
      mri_free(im); im = NULL;
      
      if (Doinv) {
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
            fprintf(SUMA_STDOUT,"Replacing matrix:\n");
            for (i=0;i<3; ++i)
                  fprintf( SUMA_STDOUT,
                           " %.5f   %.5f  %.5f  %.5f\n", 
                           M[i][0], M[i][1], M[i][2], M[i][3]); 
            fprintf(SUMA_STDOUT,"     with matrix:\n");
            for (i=0;i<3; ++i)
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
               fprintf(SUMA_STDOUT,"Replacing matrix:\n");
               for (i=0;i<3; ++i)
                     fprintf( SUMA_STDOUT,
                              " %.5f   %.5f  %.5f  %.5f\n", 
                              xform[i][0], xform[i][1], 
                              xform[i][2], xform[i][3]); 
               fprintf(SUMA_STDOUT,"     with matrix:\n");
               for (i=0;i<3; ++i)
                     fprintf( SUMA_STDOUT,
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
   /* prepare the name of the surface object to read*/
   SO = SUMA_Load_Surface_Object_Wrapper ( if_name, if_name2, vp_name, iType, iForm, sv_name, 1);
   if (!SO) {
      fprintf (SUMA_STDERR,"Error %s: Failed to read input surface.\n", FuncName);
      exit (1);
   }

   if (DoR2S > 0.0000001) {
      if (!SUMA_ProjectSurfaceToSphere(SO, NULL , DoR2S , NULL)) {
         SUMA_S_Err("Failed to project to surface");
         exit(1);
      }
   }

   if (ifpar_name) {
      SOpar = SUMA_Load_Surface_Object_Wrapper ( ifpar_name, ifpar_name2, vp_name, iparType, iparForm, sv_name, 1);
      if (!SOpar) {
         fprintf (SUMA_STDERR,"Error %s: Failed to read input parent surface.\n", FuncName);
         exit (1);
      }
      /* need edge list */
      if (!SUMA_SurfaceMetrics_eng (SOpar,"EdgeList", NULL, 0, SUMAg_CF->DsetList)) {
         SUMA_SL_Err("Failed to create edgelist for parent");
         exit(1);
      }
   }
   
   
   /* if Do_wind */
   if (Do_wind) {
      fprintf (SUMA_STDOUT,"Checking and repairing mesh's winding consistency...\n");
      /* check the winding, but that won't fix the normals, 
      you'll have to recalculate those things, if need be ... */
      if (!SUMA_SurfaceMetrics_eng (SO, "CheckWind", NULL, 0, SUMAg_CF->DsetList)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         exit(1);
      }   
   }
   
   if (Do_tlrc) {
      fprintf (SUMA_STDOUT,"Performing talairach transform...\n");

      /* form the tlrc version of the surface volume */
      tlrc_name = (char *) SUMA_calloc (strlen(SO->VolPar->dirname)+strlen(SO->VolPar->prefix)+60, sizeof(char));
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
   
   if (Do_acpc) {
      fprintf (SUMA_STDOUT,"Performing acpc transform...\n");

      /* form the acpc version of the surface volume */
      acpc_name = (char *) SUMA_calloc (strlen(SO->VolPar->dirname)+strlen(SO->VolPar->prefix)+60, sizeof(char));
      sprintf (acpc_name, "%s%s+acpc.HEAD", SO->VolPar->dirname, SO->VolPar->prefix);
      if (!SUMA_filexists(acpc_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, acpc_name);
         exit(1);
      }
      
      /* read the acpc header */
      aset = THD_open_dataset(acpc_name) ;
      if( !ISVALID_DSET(aset) ){
         fprintf (SUMA_STDERR,"Error %s: %s is not a valid data set.\n", FuncName, acpc_name) ;
         exit(1);
      }
      if( aset->warp == NULL ){
         fprintf (SUMA_STDERR,"Error %s: acpc_name does not contain an acpc transform.\n", FuncName);
         exit(1);
      }
      
      warp = aset->warp ;
      
      /* now warp the coordinates, one node at a time */
      if (!SUMA_AFNI_forward_warp_xyz(warp, SO->NodeList, SO->N_Node)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AFNI_forward_warp_xyz.\n", FuncName);
         exit(1);
      }

      
   }
   
   if (Do_mni_RAI) {
      fprintf (SUMA_STDOUT,"Performing MNI_RAI transform...\n");
      /* apply the mni warp */
      if (!SUMA_AFNItlrc_toMNI(SO->NodeList, SO->N_Node, "RAI")) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AFNItlrc_toMNI.\n", FuncName);
         exit(1);
      }
      sprintf(orsurf,"RAI");
   }
   
   if (Do_mni_LPI) {
      fprintf (SUMA_STDOUT,"Performing MNI_LPI transform...\n");
      /* apply the mni warp */
      if (!SUMA_AFNItlrc_toMNI(SO->NodeList, SO->N_Node, "LPI")) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AFNItlrc_toMNI.\n", FuncName);
         exit(1);
      }
      sprintf(orsurf,"LPI");
   }
   
   if (Doxmat) {
      fprintf (SUMA_STDOUT,"Performing affine transform...\n");
      if (LocalHead) {
         for (i=0; i<3 ; ++i) {
            fprintf (SUMA_STDERR,"M[%d][:] = %f %f %f %f\n", i, xform[i][0], xform[i][1], xform[i][2], xform[i][3]);
         }
         fprintf (SUMA_STDERR,"Cen[:] %f %f %f\n", xcen[0], xcen[1], xcen[2]);
      }
      if (Docen) {
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
   }
   
   if (orcode[0] != '\0') {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Changing coordinates from %s to %s\n", FuncName, orsurf, orcode);
      if (!SUMA_CoordChange(orsurf, orcode, SO->NodeList, SO->N_Node)) {
         SUMA_S_Err("Failed to change coords.");
         exit(1);
      }
   }
   
   if (Do_p2s) {
      SUMA_SurfaceObject *SOold = SO;
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Changing patch to surface...\n", FuncName);
      SO = SUMA_Patch2Surf(SOold->NodeList, SOold->N_Node, SO->FaceSetList, SO->N_FaceSet, 3);
      if (!SO) {
         SUMA_S_Err("Failed to change patch to surface.");
         exit(1);
      }
      
      /* get rid of old surface object */
      SUMA_Free_Surface_Object(SOold);
   }
   
   if (Do_native) {
      if (!SUMA_Delign_to_VolPar (SO, NULL)) {
         SUMA_S_Err("Failed to transform coordinates to native space");
         exit(1);  
      }
   }
   
   if (LocalHead) SUMA_Print_Surface_Object (SO, stderr);
   
   fprintf (SUMA_STDOUT,"Writing surface...\n");
   
   
   /* write the surface object */
   if (!(SUMA_Save_Surface_Object ( SO_name,
                                    SO, oType, oFormat, SOpar))) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed to write surface object.\n", 
                  FuncName);
         exit (1);
   } 
   
   
   
   if (of_name_strip) of_name_strip = SUMA_Free_Parsed_Name (of_name_strip);
   if (of_name2_strip) of_name2_strip = SUMA_Free_Parsed_Name (of_name2_strip);
   if (OF_name) SUMA_free(OF_name);
   if (OF_name2) SUMA_free(OF_name2);
   if (SF_name) SUMA_free(SF_name);
   if (SO_name) SUMA_free(SO_name);
   if (SO) SUMA_Free_Surface_Object(SO);
   if (SOpar) SUMA_Free_Surface_Object(SOpar);
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   return (0);
}
