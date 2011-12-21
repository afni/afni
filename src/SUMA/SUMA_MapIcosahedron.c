#include "SUMA_suma.h"
#include "SUMA_Macros.h"

void SUMA_MapIcosahedron_usage ()
{/*Usage*/
   static char FuncName[]={"SUMA_MapIcosahedron_usage"};
   char * s = NULL;
   printf ( 
"\n"
"Usage: MapIcosahedron <-spec specFile> \n"
"                      [-rd recDepth] [-ld linDepth] \n"
"                      [-morph morphSurf] \n"
"                      [-it numIt] [-prefix fout] \n"
"                      [-NN_dset_map DSET]\n"
"                      [-dset_map DSET] [-fix_cut_surfaces]\n"
"                      [-verb] [-help] [...]\n"
"\n"
"Creates new versions of the original-mesh surfaces using the mesh\n"
"of an icosahedron. \n"
"\n"
"   -spec specFile: spec file containing original-mesh surfaces\n"
"        including the spherical and warped spherical surfaces.\n"
"\n"
"        You cannot use a specfile that has surfaces from both hemispheres.\n"
"        Such spec files are usually called *_both.spec\n"
"        You will need to run MapIcosahedron separately on each hemisphere\n"
"        then use the program inspec to merge the two new spec files.\n"
"        Say MapIcosahedron produced std.LH.spec and std.RH.spec, \n"
"        you can use the following to merge them:\n"
"              inspec   -LRmerge std.LH_SPEC.spec std.RH_SPEC.spec \\\n"
"                       -prefix std.BOTH_SPEC.spec\n"
"\n"
"   -rd recDepth: recursive (binary) tesselation depth for icosahedron.\n"
"        (optional, default:3) See CreateIcosahedron for more info.\n"
"\n"
"   -ld linDepth: number of edge divides for linear icosahedron tesselation \n"
"        (optional, default uses binary tesselation).\n"
"        See CreateIcosahedron -help for more info.\n"
"\n"
"   *Note: Enter -1 for recDepth or linDepth to let program \n"
"          choose a depth that best approximates the number of nodes in\n"
"          original-mesh surfaces.\n"
"\n"
"   -morph morphSurf: \n"
"\n"
"   -NN_dset_map DSET: Map DSET onto the new mesh. \n"
"                      Use Nearest Neighbor interpolation\n"
"   -dset_map DSET:  Same as NN_dset_map but with barycentric interpolation\n"
"\n"
"   *Note: You can use repeated instances of either -NN_dset_map or -dset_map\n"
"          to process multiple datasets at once.\n"
"\n"    
"        Old Usage:\n"
"        ----------\n"
"        State name of spherical surface to which icosahedron \n"
"        is inflated. Typical example for FreeSurfer surfaces would be \n"
"        'sphere.reg', and that's the default used by the program. \n"
"\n"
"        New Usage:\n"
"        ----------\n"
"        State name or filename of spherical surface to which icosahedron \n"
"        is inflated. Typical example for FreeSurfer surfaces would be \n"
"        'sphere.reg', and that's the default used by the program. \n"
"        Searching is first done assuming a State name and if that does\n"
"        not return exactly one match, a search based on the filename\n"
"        is carried out.\n"
"\n"
"   The following four options affect the geometric center and radius\n"
"   settings of morphSurf. In previous versions, the geometric center\n"
"   was set to the center of mass. A better estimate of the geometric\n"
"   center is now obtained and this might make standard-mesh surfaces\n"
"   less sensitive to distortions in the spherical surfaces.\n"
"   With this change, the coordinates of the nodes will be silghtly\n"
"   different from in previous versions. If you insist on the old \n"
"   method, use the option -use_com below.\n"
"   ----------------------------------------------------------------\n"
"   -sphere_at_origin: Geometric center of morphSurf sphere is at \n"
"                      0.0 0.0 0.0. This is usually the case but\n"
"                      if you do not know, let the program guess.\n"
"\n"
"   -sphere_center cx cy cz: Geometric center of morphSurf sphere. \n"
"                            If not specified, it will be estimated.\n"
"      Note: It is best to specify cx cy cz or use -sphere_at_origin\n"
"            when the center is known.\n"
"\n"
"   -use_com: (ONLY for backward compatibility)\n"
"             Use this option to make the center of mass of morpSurf.\n"
"             be the geometric center estimate. This is not optimal,\n"
"             use this option only for backward compatibility.\n"
"             The new results, i.e. without -use_com, should always be\n"
"             better.\n"
"\n"
"   -sphere_radius R: Radius of morphSurf sphere. If not specified,\n"
"                     this would be the average radius of morpSurf.\n"
"                     \n"
"   ----------------------------------------------------------------\n"
"\n"
"   -it numIt: number of smoothing interations \n"
"        (optional, default none).\n"
"\n"
"   -prefix FOUT: prefix for output files.\n"
"        (optional, default 'std.')\n"
"\n"
"   -morph_sphere_check: Do some quality checks on morphSurf and exit.\n"
"                        This option now replaces -sph_check and -sphreg_check\n"
"                        See output of SurfQual -help for more info on this\n"
"                        option's output.\n"
"\n"
"**********************************************\n"
"-sph_check and -sphreg_check are now OBSOLETE. \n"
"\n"
"   [-sph_check]:(OBSOLETE, use -morph_sphere_check instead) \n"
"                Run tests for checking the spherical surface (sphere.asc)\n"
"                The program exits after the checks.\n"
"                This option is for debugging FreeSurfer surfaces only.\n"
"\n"
"   [-sphreg_check]: (OBSOLETE, use -morph_sphere_check instead)\n"
"                Run tests for checking the spherical surface (sphere.reg.asc)\n"
"                The program exits after the checks.\n"
"                This option is for debugging FreeSurfer surfaces only.\n"
"\n"
"   -sph_check and -sphreg_check are mutually exclusive.\n"
"\n"
"**********************************************\n"
"\n"
"   -all_surfs_spec: When specified, includes original-mesh surfaces \n"
"       and icosahedron in output spec file.\n"
"       (optional, default does not include original-mesh surfaces)\n"
"   -verb: verbose.\n"
"   -fix_cut_surfaces: Check and fix standard-mesh surfaces with cuts for \n"
"                      cross-cut connections.\n"
"   -check_cut_surfaces: (default) Check standard-mesh surfaces with cuts for \n"
"                      cross-cut connections.\n"
"   -forget_cut_surface: Do not check standard-mesh surfaces with cuts for \n"
"                      cross-cut connections.\n"
"   -write_nodemap: (default) Write a file showing the mapping of each \n"
"                   node in the icosahedron to the closest\n"
"                   three nodes in the original mesh.\n"
"                   The file is named by the prefix of the output\n"
"                   spec file and suffixed by MI.1D\n"
"  NOTE: This option is useful for understanding what contributed\n"
"        to a node's position in the standard meshes (STD_M).\n"
"        Say a triangle on the  STD_M version of the white matter\n"
"        surface (STD_WM) looks fishy, such as being large and \n"
"        obtuse compared to other triangles in STD_M. Right\n"
"        click on that triangle and get one of its nodes (Ns)\n"
"        search for Ns in column 0 of the MI.1D file. The three\n"
"        integers (N0, N1, N2) on the same row as Ns will point \n"
"        to the three nodes on the original meshes (sphere.reg) \n"
"        to which Ns (from the icosahedron) was mapped. Go to N1\n"
"        (or N0 or N2) on the original sphere.reg and examine the\n"
"        mesh there, which is best seen in mesh view mode ('p' button).\n"
"        It will most likely be the case that the sphere.reg mesh\n"
"        there would be highly distorted (quite compressed).\n"    
"   -no_nodemap: Opposite of write_nodemap\n"
"\n"
"NOTE 1: The algorithm used by this program is applicable\n"
"      to any surfaces warped to a spherical coordinate\n"
"      system. However for the moment, the interface for\n"
"      this algorithm only deals with FreeSurfer surfaces.\n"
"      This is only due to user demand and available test\n"
"      data. If you want to apply this algorithm using surfaces\n"
"      created by other programs such as SureFit and Caret, \n"
"      Send saadz@mail.nih.gov a note and some test data.\n"
"\n"
"NOTE 2: At times, the standard-mesh surfaces are visibly\n"
"      distorted in some locations from the original surfaces.\n"
"      So far, this has only occurred when original spherical \n"
"      surfaces had topological errors in them. \n"
"      See SurfQual -help and SUMA's online documentation \n"
"      for more detail.\n"
"\n" );

   s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;

   printf ( "\n"
"          Brenna D. Argall LBC/NIMH/NIH  \n"
"(contact) Ziad S. Saad     SSC/NIMH/NIH saadz@mail.nih.gov\n"
"\n"
"\n");
   exit (0);
}/*Usage*/
#define SCRUBIT { \
   if (SUMAg_DOv) \
      SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv); \
   if (!SUMA_Free_CommonFields(SUMAg_CF))       \
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);  \
}
/*!
  stand alone program to map one surface to another and write mapping to file. 

*/
int main (int argc, char *argv[])
{/* main SUMA_MapIcosahedron */

   static char FuncName[]={"MapIcosahedron"};
   SUMA_Boolean brk, smooth=NOPE, verb=NOPE, all_surfs_spec=NOPE;
   char fout[SUMA_MAX_FILENAME_LENGTH];
   char icoFileNm[SUMA_MAX_FILENAME_LENGTH], outSpecFileNm[SUMA_MAX_FILENAME_LENGTH];
   char bin[SUMA_MAX_FILENAME_LENGTH], *histnote=NULL;
   int numTriBin=0, numTriLin=0, numIt=0;

   int kar, i, j, k, p, kspec=0, depth, found = 0, ifound=-1, ii=0;
   char *brainSpecFile=NULL, *OutName = NULL, *morph_surf = NULL;
   SUMA_SurfSpecFile brainSpec;
   SUMA_SurfSpecFile *stdSpec = NULL;
  
   int new_state=0, N_in_name=0;
   char *in_name[100], *uname=NULL, *oname=NULL;
   int in_mode[100];
   float r, ctrX, ctrY, ctrZ, ctr[3];
   SUMA_SurfaceObject   *icoSurf=NULL;
   SUMA_MorphInfo *MI=NULL;
   float *smNodeList=NULL, lambda, mu, bpf, *Cx = NULL;
   SUMA_INDEXING_ORDER d_order;
   SUMA_COMM_STRUCT *cs = NULL;
   struct  timeval start_time;
   float etime_MapSurface, UserRadius=-1.0, Uctr[3];
   int UserCenter=-1;
   double cent[3], centmed[3];
   char snote[1000], sbuf[1000];
   SUMA_Boolean UseCOM, CheckSphere, WriteMI;
   SUMA_SurfaceObject *SO=NULL, *SO_morph=NULL, *SOw=NULL;
   void *writeFile=NULL, *vbufp=NULL;
   int oform=SUMA_ASCII_NIML, iform=SUMA_NO_DSET_FORMAT;
   int correct_cuts = 0;
   float minimum_sine=0.03;
   SUMA_DSET *dset=NULL, *dseto=NULL; 
   SUMA_M2M_STRUCT *M2M=NULL;
   SUMA_Boolean LocalHead = NOPE;

   FILE *tmpFile=NULL;
    
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   /* allocate space for CommonFields structure */
   if (LocalHead) 
      fprintf (SUMA_STDERR,
               "%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct(SUMA_MAX_DISPLAYABLE_OBJECTS);
   if (LocalHead) 
      fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);
   
   cs = SUMA_Create_CommSrtuct();
   if (!cs) exit(1);

   /* clueless user ? */
   if (argc < 2) {
      SUMA_MapIcosahedron_usage ();
      exit (1); 
   }
   
   /* read in the options */
   correct_cuts=-1; /* -1 just check , 0 do not check, 1 check and fix*/
   minimum_sine = 0.03; /* minimum acceptable sine of angle,
                           a limit of 0.03 corresponds to an angle less
                           than 1.7 degrees */
   UserCenter = -1;
   Uctr[0] = 0.0; Uctr[1] = 0.0; Uctr[2] = 0.0;
   UserRadius = -1.0;
   depth = 3;
   morph_surf = NULL;
   sprintf( fout, "%s", "std.");
   sprintf( bin, "%s", "y");
   smooth = NOPE;  numIt=0;
   verb = NOPE;
   all_surfs_spec = NOPE;
   kar = 1;
   brk = NOPE;
   CheckSphere = NOPE;
   UseCOM = NOPE;
   WriteMI = YUP;
   N_in_name = 0;
   oform = SUMA_ASCII_NIML;
   while (kar < argc) { /* loop accross command line options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         SUMA_MapIcosahedron_usage ();
         exit (1);
      }
      
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);

		if (!brk && (strcmp(argv[kar], "-iodbg") == 0)) {
			fprintf( SUMA_STDOUT,
                  "Warning %s: SUMA running in in/out debug mode.\n", FuncName);
			SUMA_INOUT_NOTIFY_ON;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-memdbg") == 0)) {
         fprintf( SUMA_STDOUT,
                  "Warning %s: SUMA running in memory trace mode.\n", FuncName);
         SUMAg_CF->MemTrace = YUP;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-spec") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -spec \n");
               exit (1);
            }
            brainSpecFile = argv[kar];
            if (strcmp(SUMA_FnameGet(brainSpecFile,"pa",SUMAg_CF->cwd), "./")) {
               SUMA_S_Err(
                 "No path allowed with spec file's name.\n"
                 "MapIcosahedron should be run from directory \n"
                 "containing spec file.");
               exit(1);
            }
            brk = YUP;
         }
      if (!brk && (strcmp(argv[kar], "-sphere_radius") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -sphere_radius \n");
               exit (1);
            }
            UserRadius = atof(argv[kar]);
            brk = YUP;
         }
      if (!brk && (strcmp(argv[kar], "-sphere_at_origin") == 0 ))
         {
            UserCenter  = 1;
            Uctr[0] = 0.0; Uctr[1] = 0.0; Uctr[2] = 0.0;
            brk = YUP;
         }
      
      if (!brk && (strcmp(argv[kar], "-use_com") == 0)) {
         fprintf(SUMA_STDOUT, "\n"
                              "Warning %s:\n"
                              " Using sphere's center of mass as a\n"
                              " geometric center. This is only for\n"
                              " compulsive backward comaptibility.\n"
                              " It is better NOT to use this option.\n", 
                              FuncName);
         UseCOM = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-NN_dset_map") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need 1argument after -NN_dset_map \n");
               exit (1);
            }
            in_name[N_in_name] = argv[kar];
            in_mode[N_in_name] = 1; /* nearest neighbor */
            ++N_in_name;
            brk = YUP;
         }

      if (!brk && (strcmp(argv[kar], "-dset_map") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need 1argument after -dset_map \n");
               exit (1);
            }
            in_name[N_in_name] = argv[kar];
            in_mode[N_in_name] = 0; /* barycentric */
            ++N_in_name;
            brk = YUP;
         }
         
      if (!brk && (strcmp(argv[kar], "-sphere_center") == 0 ))
         {
            kar ++;
            if (kar+2 >= argc)  {
               fprintf (SUMA_STDERR, "need 3 arguments after -sphere_center \n");
               exit (1);
            }
            UserCenter  = 1;
            Uctr[0] = atof(argv[kar]); kar ++;
            Uctr[1] = atof(argv[kar]); kar ++;
            Uctr[2] = atof(argv[kar]); 
            brk = YUP;
         }      
      if (!brk && (strcmp(argv[kar], "-rd") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -rd \n");
               exit (1);
            }
            depth = atoi(argv[kar]);
            sprintf (bin, "y");
            brk = YUP;
            
         }      
      if (!brk && (strcmp(argv[kar], "-ld") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -ld \n");
               exit (1);
            }
            depth = atoi(argv[kar]);
            sprintf (bin, "n");
            brk = YUP;
         }      
      if (!brk && strcmp(argv[kar], "-write_nodemap") == 0)
         {
            WriteMI = YUP;
            brk = YUP;
         }
      if (!brk && strcmp(argv[kar], "-no_nodemap") == 0)
         {
            WriteMI = NOPE;
            brk = YUP;
         }
      if (!brk && strcmp(argv[kar], "-morph") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -morph ");
               exit (1);
            }
            morph_surf = argv[kar];
            
            brk = YUP;
         }   
      if (!brk && (strcmp(argv[kar], "-it") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -it \n");
               exit (1);
            }
            smooth = YUP;
            numIt = atoi(argv[kar]);
            brk = YUP;
         }      
      if (!brk && (strcmp(argv[kar], "-verb") == 0 ))
         {
            SUMA_S_Note("-verb option no longer produces\n"
                        "a spec file that includes original\n"
                        "meshes. See -all_surfs_spec for that.\n");
            verb = YUP;
            brk = YUP;
            
         }
      if (!brk && (strcmp(argv[kar], "-all_surfs_spec") == 0 ))
         {
            all_surfs_spec = YUP;
            brk = YUP;
            
         }
      
      if (!brk && (strcmp(argv[kar], "-morph_sphere_check") == 0 ))
         {
            CheckSphere = YUP;
            brk = YUP;
         }
               
      if (!brk && (strcmp(argv[kar], "-sphreg_check") == 0 ))
         {
            SUMA_S_Err( "This option is now obsolete, "
                        "please use -morph_sphere_check");
            exit(1);
         }      
      if (!brk && (strcmp(argv[kar], "-sph_check") == 0 ))
         {
            SUMA_S_Err( "This option is now obsolete, "
                        "please use -morph_sphere_check");
            brk = YUP;
            
         } 
      if (!brk && (strcmp(argv[kar], "-fix_cut_surfaces") == 0 ))
         {
            correct_cuts = 1;
            brk = YUP;
            
         }
      if (!brk && (strcmp(argv[kar], "-check_cut_surfaces") == 0 ))
         {
            correct_cuts = -1;
            brk = YUP;
            
         }
      if (!brk && (strcmp(argv[kar], "-forget_cut_surfaces") == 0 ))
         {
            correct_cuts = 0;
            brk = YUP;
            
         }
        
      if (!brk && strcmp(argv[kar], "-prefix") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -prefix ");
               exit (1);
            }
            sprintf (fout, "%s", argv[kar]);
            brk = YUP;
         }   
      
      if (!brk) {
         fprintf (SUMA_STDERR,
                  "Error %s: Option %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
         suggest_best_prog_option(argv[0], argv[kar]);
         exit (1);
      } 
      else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command line options */

   /* check on output file name */
   if (!THD_ok_overwrite()) {
      sprintf (outSpecFileNm, "%s%s", 
               fout, SUMA_FnameGet(brainSpecFile,"f", SUMAg_CF->cwd));
      if (SUMA_filexists(outSpecFileNm)) {
         SUMA_S_Errv("File %s exists, change prefix or use -overwrite.\n",
                     outSpecFileNm);
         exit(1);
      } else {
         sprintf (outSpecFileNm, "%s%s.spec", 
                  fout, SUMA_FnameGet(brainSpecFile,"f", SUMAg_CF->cwd));
         if (SUMA_filexists(outSpecFileNm)) {
            SUMA_S_Errv("File %s exists, change prefix or use -overwrite.\n",
                        outSpecFileNm);
            exit(1);
         }
      }
   }
   /* reset spec prefix */
   sprintf (outSpecFileNm, "%s%s", 
            fout, SUMA_FnameGet(brainSpecFile,"f", SUMAg_CF->cwd));
   
   if (LocalHead && !UseCOM && UserCenter == -1) {/* Enough already. Dec 2011 */
      SUMA_S_Note("\n"
                  "---------------------------------------------------\n"
                  "The program now uses the estimated geometric center\n"
                  "of the sphere rather than the center of mass.  This\n"
                  "is  more  robusto.   But if you INSIST on the older\n"
                  "approach for backward compatibility, you can revert\n"
                  "to the old method with -use_com option.\n"
                  "---------------------------------------------------\n"
                  "\n");
   }
   /* check for some sanity */
   if (bin[0] == 'y' && depth > 10) {
      fprintf (SUMA_STDERR, 
               "%s: You cannot use a recursive depth > 10.\n", FuncName);
      exit(1);
   }
   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: %s contains surfaces, tesselation depth is %d.\n", 
               FuncName, brainSpecFile, depth);
   if (brainSpecFile == NULL) {
      fprintf (SUMA_STDERR,"Error %s: No spec file specified.\n", FuncName);
      exit(1);
   }
 
   /* read spec file*/
   if (!SUMA_AllocSpecFields(&brainSpec)) {
      SUMA_S_Err("Failed to allocate spec fields");
      exit(1);
   }
   if ( !SUMA_Read_SpecFile (brainSpecFile, &brainSpec)) {
      fprintf( SUMA_STDERR,
               "Error %s: Error in %s SUMA_Read_SpecFile\n", 
               FuncName, brainSpecFile);
      exit(1);
   }
   /* scan trough spec file and make sure there is one and only one
      LocalDomainParent. Otherwise issue a warning */
   found = 0;
   for (i=0; i<brainSpec.N_Surfs; ++i) {
      if (strstr(brainSpec.MappingRef[i],"SAME") ||
          strstr(brainSpec.LocalDomainParent[i],"SAME")) ++found;
   }
   if (found != 1) {
      SUMA_S_Warnv(
         "***********************************************************\n"
         "You should normally have only 1 LocalDomainParent\n"
         "(or MappingRef) in the spec file passed to MapIcosahedron.\n"
         "%d were found.\n"
         "All surfaces must have the same (or a subset of the) mesh\n"
         "or your morphSurf\n"
         "You should not a be using a spec file that combines surfaces\n"
         "from both hemispheres. Run MapIcosahedron twice separately,\n"
         "then merge the resultant spec files with\n"
         "   inspec  -LRmerge LH_SPEC.spec RH_SPEC.spec \\\n"
         "           -prefix BOTH_SPEC.spec\n"
         "\n" 
         "Program will proceed, assuming you know what you're doing. \n"
         "***********************************************************\n",
         found
      );
   }
   
   /* load spec file (which loads surfaces)*/
   if ( !SUMA_LoadSpec_eng( &brainSpec, SUMAg_DOv, &SUMAg_N_DOv, 
                            NULL, 0 , SUMAg_CF->DsetList) ) {
      fprintf(SUMA_STDERR, "Error %s: Error in SUMA_LoadSpec\n", FuncName);
      exit(1);
   }

   histnote = SUMA_HistString (NULL, argc, argv, NULL);

   
   if (CheckSphere) {
      fprintf(SUMA_STDERR,"%s:\n:Checking morph surface only.\n", FuncName);
   }
   
   /* Allocate for output spec */
   stdSpec = (SUMA_SurfSpecFile *)SUMA_malloc(sizeof(SUMA_SurfSpecFile));
   if (!SUMA_AllocSpecFields(stdSpec)) {
      SUMA_S_Err("Failed to initialize stdSpec\n" );
      exit(1);
   }
   stdSpec->N_Surfs = 0;
   stdSpec->N_States = 0;
   stdSpec->N_Groups = 1;
   strcpy(stdSpec->SpecFilePath, SUMA_FnameGet(fout,"pa", SUMAg_CF->cwd));
   strcpy(stdSpec->SpecFileName, SUMA_FnameGet(fout,"f", SUMAg_CF->cwd));  
   
   for (i=0; i<brainSpec.N_Surfs; ++i) {
      if (!brainSpec.IDcode[i]) {
         SUMA_S_Errv( "NULL idcode in brainSpec.IDcode[%d], \n"
                     "this should not be after loading the surfaces.\n",
                     i);
         exit(1);
      }
      /* find the surface of this ID */
      if (!(SO = SUMA_findSOp_inDOv( brainSpec.IDcode[i], 
                                    SUMAg_DOv, SUMAg_N_DOv))) {
         SUMA_S_Errv( "Failed to find surface with id %s in DOv!",
                      brainSpec.IDcode[i]  );
         exit(1);
      }
      
      /* Now add that surface's standard version into stdSpec */
      ++stdSpec->N_Surfs;
      SUMA_copy_spec_entries( &brainSpec, stdSpec,
                              i,         stdSpec->N_Surfs -1, 0);
      
      /* Change the obvious few entries*/
      
      snprintf(stdSpec->State[stdSpec->N_Surfs -1], 
               (SUMA_MAX_LABEL_LENGTH-1)*sizeof(char),
               "std.%s", brainSpec.State[i]);
      /* add state to StateList */
      new_state = 1;
      if (brainSpec.N_States) {
         sprintf(sbuf,"%s|",stdSpec->State[stdSpec->N_Surfs -1]); 
         if (SUMA_iswordin(stdSpec->StateList, sbuf)) new_state = 0;
      }
      if (new_state) {
         sprintf( stdSpec->StateList, "%s|", 
                  stdSpec->State[stdSpec->N_Surfs -1]);
         ++stdSpec->N_States;
      }
      
      if (brainSpec.TopoFile[i] && brainSpec.TopoFile[i][0])    
         snprintf(stdSpec->TopoFile[stdSpec->N_Surfs -1],
                  (SUMA_MAX_FP_NAME_LENGTH-1)*sizeof(char),
                  "%s%s%s", 
                  SUMA_FnameGet(brainSpec.TopoFile[i], "pa", SUMAg_CF->cwd),
                  fout,
                  SUMA_FnameGet(brainSpec.TopoFile[i], "f", SUMAg_CF->cwd));
      if (brainSpec.CoordFile[i] && brainSpec.CoordFile[i][0])    
         snprintf(stdSpec->CoordFile[stdSpec->N_Surfs -1],
                  (SUMA_MAX_FP_NAME_LENGTH-1)*sizeof(char),
                  "%s%s%s", 
                  SUMA_FnameGet(brainSpec.CoordFile[i], "pa", SUMAg_CF->cwd),
                  fout,
                  SUMA_FnameGet(brainSpec.CoordFile[i], "f", SUMAg_CF->cwd));
      if (brainSpec.SurfaceFile[i] && brainSpec.SurfaceFile[i][0])    
         snprintf(stdSpec->SurfaceFile[stdSpec->N_Surfs -1],
                  (SUMA_MAX_FP_NAME_LENGTH-1)*sizeof(char),
                  "%s%s%s", 
                  SUMA_FnameGet(brainSpec.SurfaceFile[i], "pa", SUMAg_CF->cwd),
                  fout,
                  SUMA_FnameGet(brainSpec.SurfaceFile[i], "f", SUMAg_CF->cwd));
      if (  brainSpec.LocalDomainParent[i] && 
            brainSpec.LocalDomainParent[i][0] &&
            !strstr(brainSpec.LocalDomainParent[i], "SAME") ) {
         snprintf(stdSpec->LocalDomainParent[stdSpec->N_Surfs -1],
                  (SUMA_MAX_FP_NAME_LENGTH-1)*sizeof(char),
                  "%s%s%s", 
                  SUMA_FnameGet( brainSpec.LocalDomainParent[i], "pa", 
                                 SUMAg_CF->cwd),
                  fout,
                  SUMA_FnameGet( brainSpec.LocalDomainParent[i], "f", 
                                 SUMAg_CF->cwd) );
      }
      #if 0
         /* You should not preserve domain grandparent! */
         if (  brainSpec.DomainGrandParentID[i] && 
               brainSpec.DomainGrandParentID[i][0] &&
               !strstr(brainSpec.DomainGrandParentID[i], "SAME") ) {
            snprintf(stdSpec->DomainGrandParentID[stdSpec->N_Surfs -1],
                     (SUMA_MAX_FP_NAME_LENGTH-1)*sizeof(char),
                     "%s%s%s", 
                     SUMA_FnameGet( brainSpec.DomainGrandParentID[i], "pa",
                                    SUMAg_CF->cwd),
                     fout,
                     SUMA_FnameGet( brainSpec.DomainGrandParentID[i], "f", 
                                    SUMAg_CF->cwd) );
         }
      #elif 0 
         /* Make the grand parent be the icosahedron.
            Note however that left and right hemisphere node indices
            do not reflect similar structures. So you can't use node indices 
            across hemispheres to find corresponding locations.
            So to be safe for now, I am turning this off, because 
            one often assumes that same domain parent means same node index
            implies same anatomical area...
             */
         if (bin[0] == 'y') {
            snprintf(stdSpec->DomainGrandParentID[stdSpec->N_Surfs -1],
                  (SUMA_MAX_FP_NAME_LENGTH-1)*sizeof(char),
                  "ico.rd.%d",depth);
         } else {
            snprintf(stdSpec->DomainGrandParentID[stdSpec->N_Surfs -1],
                  (SUMA_MAX_FP_NAME_LENGTH-1)*sizeof(char),
                  "ico.ld.%d",depth);
         }  
      #endif
      if (  brainSpec.LocalCurvatureParent[i] && 
            brainSpec.LocalCurvatureParent[i][0] &&
            !strstr(brainSpec.LocalCurvatureParent[i], "SAME") ) {
         snprintf(stdSpec->LocalCurvatureParent[stdSpec->N_Surfs -1],
                  (SUMA_MAX_FP_NAME_LENGTH-1)*sizeof(char),
                  "%s%s%s", 
                  SUMA_FnameGet( brainSpec.LocalCurvatureParent[i], "pa", 
                                 SUMAg_CF->cwd),
                  fout,
                  SUMA_FnameGet( brainSpec.LocalCurvatureParent[i], "f", 
                                 SUMAg_CF->cwd) );
      }
      /* deal with label dsets */
      if (  brainSpec.LabelDset[i] &&
            brainSpec.LabelDset[i][0] ) {
         snprintf(stdSpec->LabelDset[stdSpec->N_Surfs -1],
                  (SUMA_MAX_FP_NAME_LENGTH-1)*sizeof(char),
                  "%s%s%s", 
                  SUMA_FnameGet( brainSpec.LabelDset[i], "pa", 
                                 SUMAg_CF->cwd),
                  fout,
                  SUMA_FnameGet( brainSpec.LabelDset[i], "f", 
                                 SUMAg_CF->cwd) );
         /* add to list of in_name to produce if not there */
         ifound = -1;
         ii=0;
         while (ii<N_in_name && ifound < 0) {
            if (!strcmp(SUMA_FnameGet(in_name[ii], "F", SUMAg_CF->cwd), 
                        SUMA_FnameGet( brainSpec.LabelDset[i], "F", 
                                                         SUMAg_CF->cwd)) ) {
               ifound = ii;
            }
            ++ii;
         }
         if (ifound < 0) {
            SUMA_S_Notev("Adding -NN_dset_map %s\n\n", brainSpec.LabelDset[i]);
            in_name[N_in_name] = brainSpec.LabelDset[i];
            in_mode[N_in_name] = 1;
            ++N_in_name;
         } else {
            SUMA_S_Notev("dset %s already on list\n\n", brainSpec.LabelDset[i]);
         }
      }
      /* NOTICE: leave the IDcode untouched, it is a convenient way
      to refer to the precursor surface below. */
      
   }
   
   if (all_surfs_spec ) {
      /* append a copy of all surfs in original spec */
      for (i=0; i<brainSpec.N_Surfs; ++i) {
         ++stdSpec->N_Surfs;
         SUMA_copy_spec_entries( &brainSpec, stdSpec,
                                 i,         stdSpec->N_Surfs -1, 0);
      }
      /* and add all of the states */
      if (  strlen(stdSpec->StateList) + strlen(brainSpec.StateList) 
            > SUMA_MAX_N_SURFACE_SPEC*99) {
         SUMA_S_Errv( "Huge string problem:\n"
                      ">>%s<<+>>%s<<\n",
                      stdSpec->StateList, brainSpec.StateList);
         exit(1);   
      }
      strcat(stdSpec->StateList, brainSpec.StateList);
      stdSpec->N_States += brainSpec.N_States;
      
      /* icosahedron is added below */
   }
   
   /* Some tests the quality of the new spec */
   if (!SUMA_CheckOnSpecFile (stdSpec)) {
      SUMA_S_Err("Problems with spec struct");
      exit(1);
   }
   
   /* find the morph surface */
   SO_morph = NULL;
   if (morph_surf == NULL) {
      i = 0; found = 0;
      while (i < brainSpec.N_Surfs) {
         if (!strcmp("sphere.reg", brainSpec.State[i])) {
            ++found; 
            if (!(SO_morph = SUMA_findSOp_inDOv (brainSpec.IDcode[i], 
                                                 SUMAg_DOv, SUMAg_N_DOv))) {
               SUMA_S_Err("Could not locate suface!");
               exit(1);
            }
         }
         ++i;   
      }
      if (found != 1){
         SUMA_S_Errv("%d surface%s had a state called %s\n"
                     "Only 1 must match.\n",
                     found, SUMA_COUNTER_PLURAL(found), "sphere.reg");
         exit(1);
      }
   } else {
      /* first assume morph_surf refers to a state
         then search based on name */
      i = 0; found = 0;
      while (i < brainSpec.N_Surfs) {
         if (!strcmp(morph_surf, brainSpec.State[i])) {
            ++found; 
            SUMA_LHv("Matched:\n"
                     "%s\n"
                     "%s\n", morph_surf, brainSpec.State[i]);
            if (!(SO_morph = SUMA_findSOp_inDOv (brainSpec.IDcode[i], 
                                                 SUMAg_DOv, SUMAg_N_DOv))) {
               SUMA_S_Err("Could not locate suface!");
               exit(1);
            }
         }
         ++i;   
      }
      if (found != 1) {
         SUMA_LHv("Lafounda, found = %d\n", found);
         if (found > 1){
            SUMA_S_Notev("Found %d surfaces using state name matching.\n"
                         "Trying filename matching.\n", found);
         }
         i = SUMA_unique_name_ind(&brainSpec, morph_surf);
         if (i == -1) {
            SUMA_S_Errv("0 matches for filename-based search for %s\n",
                        morph_surf);
            exit(1);
         } else if (i == -2) {
            SUMA_S_Errv("More than 1 matches for filename-based search for %s\n",
                        morph_surf);
            exit(1);
         } else if (i == -3) {
            SUMA_S_Errv("'Run for you life' matches for filename-based "
                        "search for %s\n",
                         morph_surf);
            exit(1);
         } else {
            found = 1;
            if (!(SO_morph = SUMA_findSOp_inDOv (brainSpec.IDcode[i], 
                                                 SUMAg_DOv, SUMAg_N_DOv))) {
               SUMA_S_Err("Could not locate suface!");
               exit(1);
            }
         }
      }
      SUMA_LHv("found %d, i %d\n", found, i);
   }
   if (!SO_morph) {
      SUMA_S_Err("NULL SO_morph");
      exit(1);
   }
   
   /* calculate extras for SO_morph */
   if (SO_morph->EL==NULL) 
      SUMA_SurfaceMetrics_eng(SO_morph, "EdgeList", NULL, 0, SUMAg_CF->DsetList);
   if (SO_morph->MF==NULL) 
      SUMA_SurfaceMetrics_eng(SO_morph, "MemberFace", NULL, 
                              0, SUMAg_CF->DsetList);    
   if (!SO_morph->Label) {
      SO_morph->Label =  SUMA_SurfaceFileName(SO_morph, NOPE);
   }
   if (!(SUMA_SetSphereParams(SO_morph, 0.1))) {
      SUMA_S_Err("Failed to set sphere parameters");
      exit(1);
   }
   if (SO_morph->isSphere != SUMA_GEOM_SPHERE) {
      SUMA_S_Warn( "Morph surface is not recognized as a sphere!\n"
                   "Examine output closely.\n"
                   "Use program SurfQual to get detailed\n"
                   "information on the sphere's quality.\n");
   }
   if (SO_morph->EL->min_N_Hosts < 2) {
      SUMA_S_Warn( "Morph surface is not a closed sphere!\n"
                   "Examine output closely.\n"
                   "Use program SurfQual to get detailed\n"
                   "information on the sphere's quality.\n");
   }
   if (SO_morph->EL->max_N_Hosts > 2) {
      SUMA_S_Warn( "Morph surface mesh is not a 2-manifold!\n"
                   "Examine output closely.\n"
                   "Use program SurfQual to get detailed\n"
                   "information on the sphere's quality.\n");
   }
   
   /* is this the check only ? */
   if ( CheckSphere ) {
      OutName = SUMA_append_string (SO_morph->Label, 
                                    "_Conv_detail.1D.dset");
      Cx = SUMA_Convexity_Engine ( SO_morph->NodeList, 
                                   SO_morph->N_Node, 
                                   SO_morph->NodeNormList, 
                                   SO_morph->FN, OutName);
      if (Cx) SUMA_free(Cx); Cx = NULL;
      SUMA_SphereQuality (SO_morph, SO_morph->Label, NULL);
      fprintf( SUMA_STDERR, 
               "%s:\nExiting after SUMA_SphereQuality\n", FuncName);

      if (OutName) SUMA_free(OutName); OutName = NULL;   
      SCRUBIT;
      exit (0);
   }
   
   /*make certain same number of nodes in all (full, not patch) surfaces*/
   for (i=0; i<brainSpec.N_Surfs; ++i) {
      if (!(SO = SUMA_findSOp_inDOv( brainSpec.IDcode[i], 
                                    SUMAg_DOv, SUMAg_N_DOv))) {
         SUMA_S_Errv( "Failed to find surface with id %s in DOv!",
                      brainSpec.IDcode[i]  );
         exit(1);
      } 
      if (  SO_morph->N_Node < SO->N_Node ) {
               /* This check used to be a strict != for N_Node,
                  but it was skipped for known freesurfer patches 
                  (those patches were identified by their state names) 
                  Now the program only requires that the number of nodes
                  in the morph surface be >= than that of the surface to be 
                  similarly morphed             ZSS Dec 31 2008 */
         fprintf( SUMA_STDERR, 
                  "Error %s:\n"
                  "  Morph Surface %s has less nodes (%d) \n"
                  "   than surface %s                (%d)\n"         
                  "Exiting.\n"
                  , FuncName, SO_morph->Label,
                  SO_morph->N_Node, 
                  SO->Label,
                  SO->N_Node);
         SCRUBIT;
         exit(1);
      }
   }

   
   /**determine depth such that numTri best 
      approximates (but overestimates) SO_morph->N_FaceSet? */ 
   if ( depth<0 ) {
     
      /*closest for recursive*/
      i = 0;  numTriBin = 20;
      while ( numTriBin < SO_morph->N_FaceSet ) {
         ++i;
         numTriBin = 20*( pow(2,2*i) );
      }
      
      /*closest for linear*/
      j = 1;  numTriLin = 20;
      while ( numTriLin < SO_morph->N_FaceSet ) {
         ++j;
         numTriLin = 20*( pow(j,2) );
      }
      
      if (  fabs(numTriLin-SO_morph->N_FaceSet) < 
            fabs(numTriBin-SO_morph->N_FaceSet) ) {
         depth = j;
         sprintf (bin, "n");
      }
      else {
         depth = i;
         sprintf (bin, "y");
      }
   }
   
   /**determine radius for icosahedron*/  
   ctrX=0; ctrY=0; ctrZ=0; j=0;
   for (i=0; i<SO_morph->N_Node; ++i) {
      j = 3*i;
      ctrX = ctrX + SO_morph->NodeList[j];
      ctrY = ctrY + SO_morph->NodeList[j+1];
      ctrZ = ctrZ + SO_morph->NodeList[j+2];
   }
   ctrX = ctrX/(SO_morph->N_Node);
   ctrY = ctrY/(SO_morph->N_Node);
   ctrZ = ctrZ/(SO_morph->N_Node);
   
   if (UserCenter > -1) {
      SUMA_S_Notev(  "User specified center of surface %s = \n"
                     "  [%.4f   %.4f   %.4f]\n"
                     "Center of mass of surface is = \n"
                     "  [%.4f   %.4f   %.4f]\n" , 
                        SO_morph->Label, 
                        Uctr[0], Uctr[1], Uctr[2],
                        ctrX, ctrY, ctrZ);
      ctrX = Uctr[0];
      ctrY = Uctr[1];
      ctrZ = Uctr[2];
   }else{
      if (!SUMA_GetCenterOfSphereSurface( SO_morph, 500, 
                                          cent, centmed)) {
         SUMA_S_Err("Failed to estimate center of spherical surface.");
         exit(1);
      }else{
         if (UseCOM) {
            if (verb) {
               SUMA_S_Notev(  
                  "Using (not recommended) center of mass coordinate of \n"
                  "  [%f   %f   %f]\n"
                  "instead of estimated geometric center of:\n"
                  "  [%f   %f   %f]\n"
                  "for surface %s in absence of specified \n"
                  "geometric center.\n"
                              , ctrX, ctrY, ctrZ,
                                centmed[0], centmed[1], centmed[2],
                                SO_morph->Label );
            }
            sprintf(snote, 
               "Notice: Forced to use COM of [%f   %f   %f], "
               "instead of geom. center of [%f   %f   %f] for %s.\n"
                              , ctrX, ctrY, ctrZ,
                              centmed[0], centmed[1], centmed[2],
                              SO_morph->Label );
         } else {
            if (verb) {
               SUMA_S_Notev(  
                  "Using (recommended) estimated geometric center of:\n"
                  "  [%f   %f   %f]\n"
                  "rather than center of mass coordinate of \n"
                  "  [%f   %f   %f]\n"
                  "for surface %s in absence of specified \n"
                  "geometric center.\n", 
                              centmed[0], centmed[1], centmed[2], 
                              ctrX, ctrY, ctrZ,
                              SO_morph->Label );
            }
            sprintf(snote, 
               "Notice: Used geom. center of [%f   %f   %f] for %s. "
               "COM was [%f   %f   %f].\n",
                              centmed[0], centmed[1], centmed[2], 
                              SO_morph->Label, ctrX, ctrY, ctrZ);
            ctrX = centmed[0];
            ctrY = centmed[1];
            ctrZ = centmed[2];
         }
      }

   }
     
   ctr[0] = 0; ctr[1] = 0; ctr[2] = 0;
   r = sqrt(   pow( (SO_morph->NodeList[0]-ctrX), 2) + 
               pow( (SO_morph->NodeList[1]-ctrY), 2) 
             + pow( (SO_morph->NodeList[2]-ctrZ), 2) );
   if (UserRadius > -1) {
      if (verb) {   
         SUMA_S_Notev(  "Surface %s\n"
                     "  User specified radius = %.4f\n"
                     "  Average raidus is     = %.4f\n" , 
                        SO_morph->Label, 
                        UserRadius,
                        r);
      } 
      sprintf( sbuf,
               " User specified radius = %.4f, Average raidus is     = %.4f."
               , UserRadius, r);
      strcat (snote,sbuf);
      r = UserRadius;
   } else {
      if (verb) {
         SUMA_S_Notev(  "Surface %s\n"
                     "  Using average radius of %.4f\n"
                     , SO_morph->Label, r  );
      } 
      sprintf(sbuf," Using average radius of %.4f", r);
      strcat (snote,sbuf);
   }
   
   if (verb) 
      SUMA_S_Notev(  "Creating Icodahedron of radius %f and center [%f %f %f]\n"
                  , r, ctr[0], ctr[1], ctr[2]);
   /**create icosahedron*/
   icoSurf = SUMA_CreateIcosahedron (r, depth, ctr, bin, 0);
   if (!icoSurf) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_MapIcosahedron.\n", FuncName);
      SCRUBIT;
      exit (1);
   }
   
   /**write icosahedron to file, if indicated*/
   if ( all_surfs_spec ) {
      sprintf (icoFileNm, "%s_icoSurf", fout);
      if (verb) 
         fprintf (SUMA_STDERR, 
                  "\n"
                  "%s: Now writing surface %s to disk ...\n"
                  , FuncName, icoFileNm);
      if (!(vbufp = SUMA_Save_Surface_Object_Wrap(icoFileNm, icoFileNm, icoSurf, 
                               SO_morph->FileType, SO_morph->FileFormat,
                               NULL))) {
         SUMA_S_Err("Failed to write icosahedron");
         exit(1);
      }
      /* Now add icosahedron */
      strcat(stdSpec->StateList, "icosahedron|");
      stdSpec->N_States += 1;
      
      ++stdSpec->N_Surfs;
      /*add to spec*/
      strcpy  (stdSpec->State[stdSpec->N_Surfs-1], "icosahedron");
      strcpy  (stdSpec->SurfaceType[stdSpec->N_Surfs-1],    
               SUMA_SurfaceTypeString(SO_morph->FileType));
      strcpy  (stdSpec->SurfaceFormat[stdSpec->N_Surfs-1],  
               SUMA_SurfaceFormatString(SO_morph->FileFormat));
      strcpy  (stdSpec->LocalDomainParent[stdSpec->N_Surfs-1], "./SAME");
      strcpy  (stdSpec->AnatCorrect[stdSpec->N_Surfs-1], "N");
      if (  SO_morph->FileType == SUMA_SUREFIT || 
            SO_morph->FileType == SUMA_VEC ) {
         strcpy  (stdSpec->TopoFile[stdSpec->N_Surfs-1], 
                  ((SUMA_SFname *)vbufp)->name_topo);
         strcpy  (stdSpec->CoordFile[stdSpec->N_Surfs-1], 
                  ((SUMA_SFname *)vbufp)->name_coord);
      } else {
         strcpy  (stdSpec->SurfaceFile[stdSpec->N_Surfs-1], (char *)vbufp);
      }
      stdSpec->EmbedDim[stdSpec->N_Surfs-1] = 3;
      SUMA_free(vbufp); vbufp = NULL; 
   }
   
   
   /** determine morph parameters by mapping icosahedron to spherical brain */
   
   /* start timer */
   SUMA_etime(&start_time,0);
   
   
   if (UseCOM) {/* if old method, reset isSphere flags */
      icoSurf->isSphere = SUMA_GEOM_NOT_SET;
      SO_morph->isSphere = SUMA_GEOM_NOT_SET;
   }
   
   MI = SUMA_MapSurface( icoSurf, SO_morph, verb ) ;
   if (!MI) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_MapIcosahedron.\n", FuncName);
      if (icoSurf) SUMA_Free_Surface_Object(icoSurf);
      SCRUBIT;
      exit (1);
   }
   
   if (WriteMI) {
      FILE *fp=NULL;
      char *fname =  SUMA_copy_string(outSpecFileNm);
      
      fname = SUMA_append_replace_string( SUMA_CropExtension(fname,".spec"),
                        ".MI.1D","",1);
      /* always overwrite, unless you allow a prefix not based on
         outSpecFileNm */
      if (0 && SUMA_filexists(fname) && !THD_ok_overwrite()) {
         SUMA_S_Errv("File %s exists, will not overwrite.", fname);
         exit(1);
      }
      if (!(fp = fopen(fname,"w")) ) {
         SUMA_S_Errv("Failed to open %s for writing.\n", fname);
         exit (1);
      }
      fprintf(fp, 
"# Col. 0: Std-mesh icosahedron's node index.\n"
"# Col. 1..3: 1st..3rd closest nodes from original mesh (%s)\n"
"# Col. 4..6: 4th..6th interpolation weight for each of the 3 closest nodes.\n"
"# History:%s\n"
"# %s\n"
                  , SO_morph->Label, histnote, snote);
      for (i=0; i<MI->N_Node_std; ++i) {
         fprintf(fp, "%6d   %6d %6d %6d   %f %f %f\n", 
                     i, MI->ClsNodes[3*i], MI->ClsNodes[3*i+1], 
                     MI->ClsNodes[3*i+2], 
                     MI->Weight[3*i], MI->Weight[3*i+1], 
                     MI->Weight[3*i+2]  ); 
      }
      SUMA_free(fname); fname=NULL; 
      fclose(fp); fp = NULL;   
   }
   
   etime_MapSurface = SUMA_etime(&start_time,1);

   /* Map the datasets from orig to stdandard space */
   if (N_in_name) {
      if (!(M2M = SUMA_MorphInfo2M2M(MI))) { /* go to more generic struct */
         SUMA_S_Err("Failed to create M2M");
         exit(1);
      }
      for (i=0; i<N_in_name; ++i) {
         if (verb) SUMA_S_Notev("Processing dset %s\n", in_name[i]);
         iform = SUMA_NO_DSET_FORMAT;
         if (!(dset = SUMA_LoadDset_s (in_name[i], &iform, 0))) {
            SUMA_S_Errv("Failed to load %s\n", in_name[i]);
            exit(1);
         }
         if (!(dseto = SUMA_morphDsetToStd (dset, M2M, in_mode[i]))) {
            SUMA_S_Errv("Failed to map %s\n", in_name[i]);
            exit(1);
         }
         uname = SUMA_append_replace_string(
               SUMA_FnameGet(in_name[i],"pa", SUMAg_CF->cwd), 
                             SUMA_FnameGet(in_name[i],"f",SUMAg_CF->cwd), 
                             fout, 0);
         oname = SUMA_WriteDset_s (uname, dseto, SUMA_ASCII_NIML, 1, 1); 
         if (verb) SUMA_S_Notev("Wrote %s\n", oname);
         if (oname) SUMA_free(oname); oname=NULL;
         if (uname) SUMA_free(uname); oname=NULL;
         if (dseto) SUMA_FreeDset(dseto); dseto = NULL;
         if (dset) SUMA_FreeDset(dset); dset = NULL;
      }
      M2M = SUMA_FreeM2M(M2M);
   }
   /**   morph surfaces backwards and write to file
         (using weighting from SUMA_MapSurfaces)      */
   
   for (i=0; i<brainSpec.N_Surfs; ++i) {
      if (!(SO = SUMA_findSOp_inDOv( brainSpec.IDcode[i], 
                                    SUMAg_DOv, SUMAg_N_DOv))) {
         SUMA_S_Errv( "Failed to find surface with id %s in DOv!",
                      brainSpec.IDcode[i]  );
         exit(1);
      } 
      if ( SO->EL==NULL) 
            SUMA_SurfaceMetrics(SO, "EdgeList", NULL);
      if ( SO->EL && SO->N_Node) 
         SO->FN = 
            SUMA_Build_FirstNeighb( SO->EL, 
                                    SO->N_Node, 
                                    SO->idcode_str, verb );
      if ( SO->FN==NULL || SO->EL==NULL ) {
         fprintf( SUMA_STDERR, 
                  "Error %s: Failed in acquired Surface Metrics.\n", 
                  FuncName);
         exit (1);
      }
      
      /* find relevant entry in output spec file */
      for (k=0; k<stdSpec->N_Surfs; ++k) {
         if (!strcmp(SO->idcode_str, stdSpec->IDcode[k]) &&
              ( strcmp(stdSpec->SurfaceFile[k], brainSpec.SurfaceFile[i]) ||
                strcmp(stdSpec->TopoFile[k], brainSpec.TopoFile[i]) || 
                strcmp(stdSpec->CoordFile[k], brainSpec.CoordFile[i]) ) ) {
            kspec = k;
            break;
         }
      }
      if (verb) {
         SUMA_S_Notev("Matched stdSpec entry %d (%s %s %s) \n"
                      "to original surface %s\n",
                      kspec, stdSpec->SurfaceFile[k], 
                      stdSpec->TopoFile[k], stdSpec->CoordFile[k],
                      SO->Label); 
      }
      SOw = SUMA_morphToStd( SO, MI, YUP);
      if ( !SOw ) {
         fprintf( SUMA_STDERR, 
                  "Error %s: Failed in morphing surface object.\n", FuncName);
         if (icoSurf) SUMA_Free_Surface_Object(icoSurf);
         if (SOw) SUMA_free (SOw);
         SCRUBIT;
         exit (1);
      }            
      SOw->FileType = SO->FileType;
      
      if (SO->FaceSetDim == 3 && correct_cuts) {
         int n_fixes = 0, eu=0;
         SUMA_EULER_SO(SOw, eu);
         if (eu != 2) {/* check for bad triangles */
            do {
               int N_bounds;
               int i, n1, n2, n3, *boundt=NULL, ilast=0;
               float *p1, *p2, *p3;
               double s[3], c[3], *a=NULL;

               if (!(boundt = (int *)SUMA_calloc(SOw->N_FaceSet, sizeof(int)))) {
                  SUMA_S_Err("Failed to allocate");
                  if (icoSurf) SUMA_Free_Surface_Object(icoSurf);
                  if (SOw) SUMA_free (SOw);
                  SCRUBIT;
                  exit (1);
               }
               /* get flag of boundary triangles */
               N_bounds = SUMA_BoundaryTriangles (SOw, boundt, 1);
               n_fixes = 0;
               i=0;
               while ( i<SOw->N_FaceSet) {
                  if ( boundt[i] > 0) {
                     n1 = SOw->FaceSetList[3*i]; 
                     n2 = SOw->FaceSetList[3*i+1]; 
                     n3 = SOw->FaceSetList[3*i+2];
                     p1 = &(SOw->NodeList[3*n1]);
                     p2 = &(SOw->NodeList[3*n2]);
                     p3 = &(SOw->NodeList[3*n3]);
                     if (!SUMA_TriTrig(p1, p2, p3, s, c, a)) {
                        SUMA_S_Err("Failed in SUMA_TriTrig");
                        exit(1);
                     }
                     if (SUMA_ABS(s[0]) < minimum_sine ||
                         SUMA_ABS(s[1]) < minimum_sine ||
                         SUMA_ABS(s[2]) < minimum_sine ) { 
                        if (correct_cuts > 0) {
                           /* mark triangle for removal  */
                           SOw->FaceSetList[3*i] = -1;
                           SOw->FaceSetList[3*i+1] = -2;
                           SOw->FaceSetList[3*i+2] = -3; 
                        }
                        ++n_fixes;
                     } 
                  } 
                  ++i;
               }
               if (n_fixes) {
                  if (correct_cuts > 0) {
                     i=0;
                     while ( i<SOw->N_FaceSet) {
                        if (SOw->FaceSetList[3*i  ] == -1 &&
                            SOw->FaceSetList[3*i+1] == -2  &&
                            SOw->FaceSetList[3*i+2] == -3 ) {
                           ilast = SOw->N_FaceSet - 1;
                           SOw->FaceSetList[3*i  ] = SOw->FaceSetList[3*ilast  ];
                           SOw->FaceSetList[3*i+1] = SOw->FaceSetList[3*ilast+1];
                           SOw->FaceSetList[3*i+2] = SOw->FaceSetList[3*ilast+2];
                           --SOw->N_FaceSet;
                        } else {
                           ++i;
                        }
                     }
                     /* Need to recompute EL, and FN, and try one more time */
                     SUMA_free_Edge_List(SOw->EL); SOw->EL=NULL;
                     SUMA_Free_FirstNeighb(SOw->FN); SOw->FN=NULL;
                     SUMA_SurfaceMetrics_eng( SOw, "EdgeList", NULL, 
                                              0, SUMAg_CF->DsetList );
                     SUMA_S_Notev(
                        "%d very obtuse boundary triangles were clipped\n"
                        " from the standard mesh version of %s.\n"
                        "    Repeating ...\n", 
                        n_fixes, SO->Label);
                  } else {
                     /* just warn and leave */
                     SUMA_S_Warnv("Standard mesh version of cut surface %s \n"
                                  "may have edges linking nodes on either side\n"
                              "of the cut. About %d triangles may be affected.\n"
                                  "Consider using option -fix_cut_surfaces to\n"
                                  "remove such links.\n\n", SO->Label, n_fixes);
                     n_fixes = 0;
                  }
               } else {
                  if (verb) {
                     SUMA_S_Note("No baldly obtuse triangles found\n");
                  }
               }
               if (boundt) SUMA_free(boundt); boundt=NULL;
            } while (n_fixes);
         } 
      } 
      
      /*smooth surface, if indicated*/
      /*(only for smwm, pial or white surfaces)*/
      if ( smooth && SO->AnatCorrect ) { 
         if (verb) {
            SUMA_S_Notev("Smoothing standard mesh version of %s\n",
                          SO->Label);
         }
         /* ZSS replaced ( id==0 || id==1 || id==5 ) with AnatCorrect  */
         bpf = 0.1;
         if ( !SUMA_Taubin_Smooth_Coef (bpf, &lambda, &mu) )
            fprintf( SUMA_STDERR, 
                     "Error %s: "
                     "Failed in acquiring Taubin Coefficients.  \n"
                     "Surface will not be smoothed.\n"
                     "\n", FuncName);
         else {
            d_order =  SUMA_ROW_MAJOR; 
            SOw->FN = icoSurf->FN;  /*all smwm pial and white surfaces 
                                          have same connectivity as icoSurf*/

            smNodeList = SUMA_Taubin_Smooth (SOw, NULL, lambda, mu, 
                                             SOw->NodeList, 
                                             2*numIt, 3, d_order, NULL, cs, 
                                             NULL, 1);
            SOw->FN = NULL;  /* or else it gets freed below! */
            if ( !smNodeList ) 
               fprintf( SUMA_STDERR, 
                        "Error %s: Failed in Taubin Smoothing.  \n"
                        "   Surface will not be smoothed.\n"
                        "\n"
                        , FuncName);
            else {
               SUMA_free( SOw->NodeList );
               SOw->NodeList = smNodeList;
            }
         }
      }
      if (!UseCOM) {
         /* project to sphere only if spheres */
         SUMA_SetSphereParams(SO, 0.2);
         if (SO->isSphere == SUMA_GEOM_SPHERE) {
            if ( verb ) 
               SUMA_S_Note("Projecting standard mesh surface to sphere");
            SUMA_ProjectToSphere(SOw, SO->SphereCenter, 
                                 SO->SphereRadius);
         }
      }
       
      if (verb) 
         fprintf (SUMA_STDERR, 
                  "%s: Now writing surface %s to disk ...\n", 
                  FuncName, SO->Label);
      writeFile = NULL;
      if (SO->FileType == SUMA_SUREFIT || SO->FileType == SUMA_VEC) {
         writeFile = 
            SUMA_Save_Surface_Object_Wrap (  stdSpec->CoordFile[kspec],
                                             stdSpec->TopoFile[kspec],
                                             SOw, 
                                             SO->FileType, SO->FileFormat,
                                             NULL);
      } else {
         writeFile = 
            SUMA_Save_Surface_Object_Wrap (  stdSpec->SurfaceFile[kspec],
                                             stdSpec->SurfaceFile[kspec],
                                             SOw, 
                                             SO->FileType, SO->FileFormat,
                                             NULL);
      }
      if(!writeFile) {
         SUMA_S_Err("Failed to write surface.");
         exit (0);
      } else {
         SUMA_free(writeFile); writeFile = NULL;
      }
      
      if ( SOw ) SUMA_Free_Surface_Object( SOw );
      SOw = NULL;

   }
   
   
   /*write spec file*/
   
   if (!SUMA_Write_SpecFile(stdSpec, outSpecFileNm, FuncName, histnote)) {
      SUMA_S_Err("Failed to write spec file!");
      exit(1);
   }  
   
   
   if (verb) 
      fprintf (SUMA_STDERR, 
               "\n"
               "SUMA_MapSurface took %f seconds to execute.\n"
               , etime_MapSurface); 
   fprintf (SUMA_STDERR, 
            "\n"
            "**               **\n"
            "    To view in SUMA, run:\n"
            "  suma -spec %s \n"
            "**               **\n"
            "\n"
            , outSpecFileNm);
   
   
   /* free variables */
   if (!SUMA_FreeSpecFields(&brainSpec)) {
      SUMA_S_Err("Faile to free spec fields");
   }
   if (!SUMA_FreeSpecFields(stdSpec)) {
      SUMA_S_Err("Faile to free spec fields");
   } 
   SUMA_free(stdSpec); 
   
   if (MI) SUMA_Free_MorphInfo (MI);
   if (histnote) SUMA_free(histnote);

   /*free surfaces*/
   if (icoSurf) SUMA_Free_Surface_Object (icoSurf); icoSurf = NULL;
   
   SCRUBIT;   


   SUMA_RETURN(0);
   
}/* main SUMA_MapIcosahedron*/

