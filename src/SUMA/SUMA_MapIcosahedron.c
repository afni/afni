#include "SUMA_suma.h"
#include "SUMA_Macros.h"

SUMA_SurfaceViewer *SUMAg_cSV; 
SUMA_SurfaceViewer *SUMAg_SVv; 
int SUMAg_N_SVv = 0; 
SUMA_DO *SUMAg_DOv;  
int SUMAg_N_DOv = 0; 
SUMA_CommonFields *SUMAg_CF; 


void SUMA_MapIcosahedron_usage ()
   
{/*Usage*/
   static char FuncName[]={"SUMA_MapIcosahedron_usage"};
   char * s = NULL;
   printf ( "\n"
            "Usage: MapIcosahedron <-spec specFile> \n"
            "                      [-rd recDepth] [-ld linDepth] \n"
            "                      [-morph morphSurf] \n"
            "                      [-it numIt] [-prefix fout] \n"
            "                      [-verb] [-help] [...]\n"
            "\n"
            "Creates new versions of the original-mesh surfaces using the mesh\n"
            "of an icosahedron. \n"
            "\n"
            "   -spec specFile: spec file containing original-mesh surfaces\n"
            "        including the spherical and warped spherical surfaces.\n"
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
            "   -morph morphSurf: surface state to which icosahedron is inflated \n"
            "        accectable inputs are 'sphere.reg' and 'sphere'\n"
            "        (optional, default uses sphere.reg over sphere).\n"
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
            "        (optional, default MapIco)\n"
            "\n"
            "   NOTE: See program SurfQual -help for more info on the following 2 options.\n"
            "   [-sph_check]: Run tests for checking the spherical surface (sphere.asc)\n"
            "                The program exits after the checks.\n"
            "                This option is for debugging FreeSurfer surfaces only.\n"
            "\n"
            "   [-sphreg_check]: Run tests for checking the spherical surface (sphere.reg.asc)\n"
            "                The program exits after the checks.\n"
            "                This option is for debugging FreeSurfer surfaces only.\n"
            "\n"
            "   -sph_check and -sphreg_check are mutually exclusive.\n"
            "\n"
            "   -all_surfs_spec: When specified, includes original-mesh surfaces \n"
            "       and icosahedron in output spec file.\n"
            "       (optional, default does not include original-mesh surfaces)\n"
            "   -verb: verbose.\n"
            "   -write_nodemap: (default) Write a file showing the mapping of each \n"
            "                   node in the icosahedron to the closest\n"
            "                   three nodes in the original mesh.\n"
            "                   The file is named by the prefix FOUT\n"
            "                   suffixed by _MI.1D\n"
            "  NOTE: This option is useful for understanding what contributed\n"
            "        to a node's position in the standard meshes (STD_M).\n"
            "        Say a triangle on the  STD_M version of the white matter\n"
            "        surface (STD_WM) looks fishy, such as being large and \n"
            "        obtuse compared to other triangles in STD_M. Right\n"
            "        click on that triangle and get one of its nodes (Ns)\n"
            "        search for Ns in column 0 of the _MI.1D file. The three\n"
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
            "          Brenna D. Argall LBC/NIMH/NIH brenna.argall@nih.gov \n"
            "(contact) Ziad S. Saad     SSC/NIMH/NIH saadz@mail.nih.gov\n"
            "          Fri Sept 20 2002\n"
            "\n");
   exit (0);
}/*Usage*/
/*!
  stand alone program to map one surface to another and write mapping to file. 

*/
int main (int argc, char *argv[])
{/* main SUMA_MapIcosahedron */

   static char FuncName[]={"MapIcosahedron"};
   SUMA_Boolean brk, smooth=NOPE, verb=NOPE, all_surfs_spec=NOPE;
   char fout[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
   char icoFileNm[10000], outSpecFileNm[10000];
   char bin[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH], *histnote=NULL;
   int numTriBin=0, numTriLin=0, numIt=0;

   int kar, i, j, k, p, it, id = -1, depth;
   char *brainSpecFile=NULL, *OutName = NULL, *morph_surf = NULL;
   SUMA_SurfSpecFile brainSpec;  

   int i_surf, i_morph, mx_N_surf, N_inSpec, N_skip;
   float r, ctrX, ctrY, ctrZ, ctr[3];
   int *spec_order=NULL, *spec_mapRef=NULL;
   SUMA_SpecSurfInfo *spec_info=NULL;
   SUMA_SurfaceObject **surfaces_orig=NULL, *icoSurf=NULL, *currSurf=NULL, *currMapRef=NULL;
   SUMA_MorphInfo *MI=NULL;
   float *smNodeList=NULL, lambda, mu, bpf, *Cx = NULL;
   SUMA_INDEXING_ORDER d_order;
   SUMA_COMM_STRUCT *cs = NULL;
   struct  timeval start_time;
   float etime_MapSurface, UserRadius=-1.0, Uctr[3];
   int UserCenter=-1;
   double cent[3], centmed[3];
   char snote[1000];
   SUMA_Boolean UseCOM, CheckSphereReg, CheckSphere, skip, writeFile, WriteMI;
   SUMA_Boolean LocalHead = NOPE;

   FILE *tmpFile=NULL;
    
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   /* allocate space for CommonFields structure */
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   #if 0
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   #endif
   
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
   UserCenter = -1;
   Uctr[0] = 0.0; Uctr[1] = 0.0; Uctr[2] = 0.0;
   UserRadius = -1.0;
   depth = 3;
   morph_surf = NULL;
   sprintf( fout, "%s", "MapIco");
   sprintf( bin, "%s", "y");
   smooth = NOPE;  numIt=0;
   verb = NOPE;
   all_surfs_spec = NOPE;
   kar = 1;
   brk = NOPE;
   CheckSphere = NOPE;
   CheckSphereReg = NOPE;
   UseCOM = NOPE;
   WriteMI = YUP;
   while (kar < argc) { /* loop accross command line options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         SUMA_MapIcosahedron_usage ();
         exit (1);
      }
      
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);

		if (!brk && (strcmp(argv[kar], "-iodbg") == 0)) {
			fprintf(SUMA_STDOUT,"Warning %s: SUMA running in in/out debug mode.\n", FuncName);
			SUMA_INOUT_NOTIFY_ON;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-memdbg") == 0)) {
         fprintf(SUMA_STDOUT,"Warning %s: SUMA running in memory trace mode.\n", FuncName);
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
                              " It is better NOT to use this option.\n", FuncName);
         UseCOM = YUP;
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
            if (strcmp (morph_surf, "sphere.reg") != 0 && strcmp (morph_surf, "sphere") != 0 ) {
               fprintf (SUMA_STDERR, " Only 'sphere' or 'sphere.reg' are allowed with -morph option\n"
                                     " User specified %s\n", morph_surf);
               exit (1);
            }
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
      
      if (!brk && (strcmp(argv[kar], "-sphreg_check") == 0 ))
         {
            if (CheckSphere) {
               fprintf (SUMA_STDERR, "-sphreg_check & -sph_check are mutually exclusive.\n");
               exit (1);
            }
            CheckSphereReg = YUP;
            brk = YUP;
            
         }      
      if (!brk && (strcmp(argv[kar], "-sph_check") == 0 ))
         {
            if (CheckSphereReg) {
               fprintf (SUMA_STDERR, "-sphreg_check & -sph_check are mutually exclusive.\n");
               exit (1);
            }
            CheckSphere = YUP;
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
         fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } 
      else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command line options */

   if (!UseCOM && UserCenter == -1) {
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
      fprintf (SUMA_STDERR, "%s: You cannot use a recursive depth > 10.\n", FuncName);
      exit(1);
   }
   if (LocalHead) fprintf (SUMA_STDERR, "%s: %s contains surfaces, tesselation depth is %d.\n", FuncName, brainSpecFile, depth);
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
      fprintf(SUMA_STDERR,"Error %s: Error in %s SUMA_Read_SpecFile\n", FuncName, brainSpecFile);
      exit(1);
   }
   /* load spec file (which loads surfaces)*/
   if ( !SUMA_LoadSpec_eng( &brainSpec, SUMAg_DOv, &SUMAg_N_DOv, NULL, 0 , SUMAg_CF->DsetList) ) {
      fprintf(SUMA_STDERR, "Error %s: Error in SUMA_LoadSpec\n", FuncName);
      exit(1);
   }

   histnote = SUMA_HistString (NULL, argc, argv, NULL);

   /** load surfaces */
   
   if (CheckSphere) {
      fprintf(SUMA_STDERR,"%s:\n:Checking sphere surface only.\n", FuncName);
   }else if (CheckSphereReg) {
      fprintf(SUMA_STDERR,"%s:\n:Checking sphere.reg surface only.\n", FuncName);
   }
   
   /*contains information regarding spec order and mapping reference
     [0]=>smwm, [1]=>pial, [2]=>infl, [3]=>sphr, [4]=>sphr.reg, [5]=>white [6]=>occip.patch*/
   mx_N_surf = 10;
   spec_order = SUMA_calloc( mx_N_surf, sizeof(int));
   spec_mapRef = SUMA_calloc( mx_N_surf, sizeof(int));
   for (i=0; i<mx_N_surf; ++i) {
      spec_order[i] = -1;
      spec_mapRef[i] = -1;
   }
   
   /*establish spec_info structure*/
   if ( all_surfs_spec ) N_inSpec = 2*brainSpec.N_Surfs+1;
   else        N_inSpec = brainSpec.N_Surfs;

   spec_info = (SUMA_SpecSurfInfo *)SUMA_calloc( N_inSpec, sizeof(SUMA_SpecSurfInfo));
   surfaces_orig = (SUMA_SurfaceObject **) SUMA_calloc( mx_N_surf, sizeof(SUMA_SurfaceObject));
   N_skip = 0;
   id = -1; /* initialize id to calm compiler */
   for (i=0; i<brainSpec.N_Surfs; ++i) {
      
      skip = NOPE;
      
      if (all_surfs_spec) i_surf = 2*(i-N_skip);
      else i_surf = i-N_skip;
      
      if (SUMA_isSO(SUMAg_DOv[i])) 
         currSurf = (SUMA_SurfaceObject *)(SUMAg_DOv[i].OP);
      
      /*find surface id and set some spec info*/
      /*reg sphere*/
      if (SUMA_iswordin( currSurf->State, "sphere.reg") ==1 ) 
         id = 4;
      /*sphere*/
      else if ( SUMA_iswordin( currSurf->State, "sphere") == 1 &&
                SUMA_iswordin( currSurf->State, "sphere.reg") == 0 ) 
         id = 3;
      /*inflated*/
      else if ((SUMA_iswordin( currSurf->State, "inflated") ==1) ) 
         id = 2;
      /*pial*/
      else if ((SUMA_iswordin( currSurf->State, "pial") ==1) )
         id = 1;
      /*smoothwm*/
      else if ((SUMA_iswordin( currSurf->State, "smoothwm") ==1) )
         id = 0;
      /*white*/
      else if ((SUMA_iswordin( currSurf->State, "white") ==1) ) 
         id = 5;
      /*3d patch*/
      else if ((SUMA_iswordin( currSurf->State, "occip.patch.3d") ==1) ) 
         id = 6;
      /*flat patch*/
      else if ((SUMA_iswordin( currSurf->State, "occip.patch.flat") ==1) ) 
         id = 7;
      /*3d patch*/
      else if ((SUMA_iswordin( currSurf->State, "full.patch.3d") ==1) ) 
         id = 8;
      /*flat patch*/
      else if ((SUMA_iswordin( currSurf->State, "full.patch.flat") ==1) ) 
         id = 9;
      else {
         
            fprintf(SUMA_STDERR, "\nWarning %s: Surface State %s not recognized. Skipping...\n", 
               FuncName, currSurf->State);
            if ( all_surfs_spec ) N_inSpec = N_inSpec-2;
            else        N_inSpec = N_inSpec-1;
            N_skip = N_skip+1;
            skip = YUP;
      }
      
      if ( ( CheckSphere || CheckSphereReg) && (id != 3 && id !=4) ) skip = YUP;
      
      if ( !skip ) {

         if (id < 0) {
            SUMA_SL_Err("This cannot be.\n"
                        "id < 0 !!!\n");
            exit(1);
         }
         
         spec_order[id] = i-N_skip;
         sprintf(spec_info[i_surf].state, "std.%s", currSurf->State );

         /*place surface into structure*/
         surfaces_orig[id] = (SUMA_SurfaceObject *)(SUMAg_DOv[i].OP);

         
         /* Check on spheriosity of surface, if sphere */
         if ( (id==4 && CheckSphereReg) || (id==3 && CheckSphere) ){
            if (surfaces_orig[id]->EL==NULL) 
               SUMA_SurfaceMetrics(surfaces_orig[id], "EdgeList", NULL);
            if (surfaces_orig[id]->MF==NULL) 
               SUMA_SurfaceMetrics(surfaces_orig[id], "MemberFace", NULL);    
            surfaces_orig[id]->Label = SUMA_SurfaceFileName(surfaces_orig[id], NOPE);
            OutName = SUMA_append_string (surfaces_orig[id]->Label, "_Conv_detail.1D.dset");
            Cx = SUMA_Convexity_Engine ( surfaces_orig[id]->NodeList, surfaces_orig[id]->N_Node, 
                                         surfaces_orig[id]->NodeNormList, surfaces_orig[id]->FN, OutName);
            if (Cx) SUMA_free(Cx); Cx = NULL;
            if (surfaces_orig[id]) {
               if (id == 4) SUMA_SphereQuality (surfaces_orig[id], "SphereRegSurf", NULL);
               else if (id == 3) SUMA_SphereQuality (surfaces_orig[id], "SphereSurf", NULL);
               else {
                  SUMA_SL_Err("Logic flow error.");
                  exit(1);
               }
            }
            fprintf(SUMA_STDERR, "%s:\nExiting after SUMA_SphereQuality\n", FuncName);
            if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
            if (surfaces_orig) SUMA_free (surfaces_orig);
            if (spec_order) SUMA_free(spec_order);
            if (spec_mapRef) SUMA_free(spec_mapRef);
            if (spec_info) SUMA_free(spec_info);
            if (OutName) SUMA_free(OutName); OutName = NULL;
            if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
            exit(0);
         }
         
         
         /*set out file names (and check for unsupported surface types)*/
         if ( surfaces_orig[id]->FileType!=SUMA_FREE_SURFER && 
              surfaces_orig[id]->FileType!=SUMA_PLY && surfaces_orig[id]->FileType!=SUMA_VEC ) { 
            fprintf(SUMA_STDERR, "\n***\n   The Surface Type is not currently handled by this program\n     due to lack of data.\n   If you would like this option to be added, please contact\n     saadz@mail.nih.gov or brenna.argall@nih.gov.\n***\n\n");
            if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
            if (surfaces_orig) SUMA_free (surfaces_orig);
            if (spec_order) SUMA_free(spec_order);
            if (spec_mapRef) SUMA_free(spec_mapRef);
            if (spec_info) SUMA_free(spec_info);
            if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
            exit (0);
         }
         else {
            if ( surfaces_orig[id]->FileType==SUMA_PLY )
               sprintf(spec_info[i_surf].fileToRead, "%s_%s.ply", fout, spec_info[i_surf].state);
            else
               sprintf(spec_info[i_surf].fileToRead, "%s_%s.asc", fout, spec_info[i_surf].state);
            if (all_surfs_spec) strcpy(spec_info[i_surf+1].fileToRead, surfaces_orig[id]->Name.FileName);
         }
         
         if ( SUMA_filexists(spec_info[i_surf].fileToRead) ) {
            fprintf (SUMA_STDERR,"Error %s: %s exists. Will not overwrite.\n", FuncName, spec_info[i_surf].fileToRead);
            if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
            if (surfaces_orig) SUMA_free (surfaces_orig);
            if (spec_order) SUMA_free(spec_order);
            if (spec_mapRef) SUMA_free(spec_mapRef);
            if (spec_info) SUMA_free(spec_info);
            if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
            exit(1);
         }
         
         
         /**set spec info*/
         
         /*set mapping reference*/
         currMapRef = (SUMA_SurfaceObject *)SUMAg_DOv[ SUMA_whichDO(surfaces_orig[id]->LocalDomainParentID, SUMAg_DOv,SUMAg_N_DOv) ].OP;
         if (SUMA_iswordin( currMapRef->State, "sphere.reg") ==1 ) 
            spec_mapRef[id] = 4;
         else if ( SUMA_iswordin( currMapRef->State, "sphere") == 1 &&
                   SUMA_iswordin( currMapRef->State, "sphere.reg") == 0 ) 
            spec_mapRef[id] = 3;
         else if ( SUMA_iswordin( currMapRef->State, "inflated") ==1 )
            spec_mapRef[id] = 2;
         else if ( SUMA_iswordin( currMapRef->State, "pial") ==1 )
            spec_mapRef[id] = 1;
         else if ( SUMA_iswordin( currMapRef->State, "smoothwm") ==1 )
            spec_mapRef[id] = 0;
         else if ( SUMA_iswordin( currMapRef->State, "white") ==1 )
            spec_mapRef[id] = 5;
         else if ( SUMA_iswordin( currMapRef->State, "occip.patch.3d") ==1 )
            spec_mapRef[id] = 6;
         else if ( SUMA_iswordin( currMapRef->State, "occip.patch.flat") ==1 )
            spec_mapRef[id] = 7;
         else if ( SUMA_iswordin( currMapRef->State, "full.patch.3d") ==1 )
            spec_mapRef[id] = 8;
         else if ( SUMA_iswordin( currMapRef->State, "full.patch.flat") ==1 )
            spec_mapRef[id] = 9;
         else {
            /*mapping ref is not one of the regular surface states*/
            fprintf(SUMA_STDERR, "\nWarning %s: Mapping Reference %s has no recognized surface state in its name.\n\tSetting to default smoothwm.\n\n", FuncName, currMapRef->State);
            spec_mapRef[id] = 0;
         }
         
         /*set all else*/
         if ( !all_surfs_spec ) k=1;
         else k=2;
         for ( j=0; j<k; ++j) {
            if ( surfaces_orig[id]->FileType==SUMA_PLY ) 
               sprintf( spec_info[i_surf+j].type, "Ply");
            else if (surfaces_orig[id]->FileType==SUMA_FREE_SURFER) 
               sprintf( spec_info[i_surf+j].type, "FreeSurfer");
            else if (surfaces_orig[id]->FileType==SUMA_VEC) 
               sprintf( spec_info[i_surf+j].type, "Vec");
            if ( surfaces_orig[id]->FileFormat==SUMA_ASCII ) 
               sprintf( spec_info[i_surf+j].format, "ASCII");
            else if (surfaces_orig[id]->FileType==SUMA_BINARY ||
                     surfaces_orig[id]->FileType==SUMA_BINARY_BE ||
                     surfaces_orig[id]->FileType==SUMA_BINARY_LE) 
               sprintf( spec_info[i_surf+j].format, "BINARY");
            strcpy (spec_info[i_surf+j].dim, "3");
            if (j>0) strcpy(spec_info[i_surf+j].state, currSurf->State);  /*states for mapped surfaces already set above*/
         }
      }
   }

   /**finish setting mapRef in spec_info (now that all fileToRead names are set)*/
   for (id=0; id<mx_N_surf; ++id) {
      if (spec_order[id]>=0) {
         /*surface state id exists*/
         
         if ( all_surfs_spec ) i_surf = 2*spec_order[id];
         else i_surf = spec_order[id];
         
         if ( spec_order[spec_mapRef[id]] < 0 ) {
            /*mapping reference not a surface in the spec file*/
            fprintf(SUMA_STDERR, "Warning %s: Mapping Reference for surface %d is not included in the spec file.\n\tSetting to 'SAME'.\n", FuncName, spec_order[id]); 
            strcpy( spec_info[i_surf].mapRef, "SAME" );
            if (all_surfs_spec) strcpy( spec_info[i_surf+1].mapRef, "SAME");
         } 
         else if ( spec_mapRef[id] == id ) {
            /*mapping reference is SAME*/
            strcpy( spec_info[i_surf].mapRef, "SAME" );
            if (all_surfs_spec) strcpy( spec_info[i_surf+1].mapRef, "SAME");
         }
         else {
            /*mapping reference is a surface in the spec file, distinct from current surface*/
            if (all_surfs_spec) {
               strcpy( spec_info[i_surf].mapRef, spec_info[2*spec_order[spec_mapRef[id]]].fileToRead );
               strcpy( spec_info[i_surf+1].mapRef, spec_info[2*spec_order[spec_mapRef[id]]+1].fileToRead );
            }
            else strcpy( spec_info[i_surf].mapRef, spec_info[spec_order[spec_mapRef[id]]].fileToRead );
         }
      }
   }
   
   /*determine which sphere to be morphed*/
   i_morph = -1;
   if ( morph_surf!=NULL ) {
      /*sphere specified by user input*/
      if (strcmp (morph_surf, "sphere.reg") == 0) { /* (SUMA_iswordin( morph_surf, "sphere.reg") ==1 ) */
         fprintf(SUMA_STDERR, "Note %s: Using sphere.reg, per user request.\n", FuncName);
         i_morph = 4;
      } else if ( strcmp( morph_surf, "sphere") == 0 ) { /*( SUMA_iswordin( morph_surf, "sphere") == 1 &&SUMA_iswordin( morph_surf, "sphere.reg") == 0 ) */
         fprintf(SUMA_STDERR, "Note %s: Using sphere, per user request.\n", FuncName);
         i_morph = 3;
      } else {
         fprintf(SUMA_STDERR, "\nError %s: Indicated morphSurf (%s) is not sphere or sphere.reg.\n", FuncName, morph_surf);
         morph_surf = NULL;
         exit (1);
      }
      if ( i_morph!=-1 ) {
         if ( spec_order[i_morph]==-1 ) {
            /*user specified sphere does not exist*/
            fprintf(SUMA_STDERR, "\nError %s: Indicated morphSurf (%s) does not exist.\n", FuncName, morph_surf);
            morph_surf = NULL;
            exit (1);
         }
      }
   }
   if ( morph_surf==NULL) {
      /*no morphing sphere specified by user input*/
      if ( spec_order[4]!=-1 ) {
         /*sphere.reg exists*/
         i_morph = 4;
      }
      else if ( spec_order[3]!=-1 )  {
         /*sphere exists (but no sphere.reg)*/
         i_morph = 3;
      }
      else {
         /*no spherical input -> exit*/
         fprintf(SUMA_STDERR, "\nError %s: Neither sphere.reg nor sphere brain states present in Spec file.\nWill not contintue.\n", FuncName);
         if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
         if (surfaces_orig) SUMA_free (surfaces_orig);
         if (spec_order) SUMA_free(spec_order);
         if (spec_mapRef) SUMA_free(spec_mapRef);
         if (spec_info) SUMA_free(spec_info);
         if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
         exit(1);
      }
   }
   
   /*calculate surface metric for sphere to be morphed*/
   if (surfaces_orig[i_morph]->EL==NULL) 
      SUMA_SurfaceMetrics(surfaces_orig[i_morph], "EdgeList", NULL);
   if (surfaces_orig[i_morph]->MF==NULL) 
      SUMA_SurfaceMetrics(surfaces_orig[i_morph], "MemberFace", NULL);
   
   /*make certain same number of nodes in all (full, not patch) surfaces*/
   for (i=0; i<mx_N_surf; ++i) {
      if ( spec_order[i]!=-1 && i!=6 && i!=7 && i!=8 && i!=9 &&!(surfaces_orig[i_morph]->N_Node == surfaces_orig[i]->N_Node) ) {
         fprintf(SUMA_STDERR, "Error %s: Surfaces (ref [%d], %d!=%d) differ in node number. Exiting.\n", FuncName, i, surfaces_orig[i_morph]->N_Node, surfaces_orig[i]->N_Node);
         if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
         if (surfaces_orig) SUMA_free (surfaces_orig);
         if (spec_order) SUMA_free(spec_order);
         if (spec_mapRef) SUMA_free(spec_mapRef);
         if (spec_info) SUMA_free(spec_info);
         if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
         exit(1);
      }
   }  
   
   
   /**determine depth such that numTri best approximates (but overestimates) surfaces_orig[i_morph]->N_FaceSet? */ 
   if ( depth<0 ) {
      
      /*closest for recursive*/
      i = 0;  numTriBin = 20;
      while ( numTriBin < surfaces_orig[i_morph]->N_FaceSet ) {
         ++i;
         numTriBin = 20*( pow(2,2*i) );
      }
      
      /*closest for linear*/
      j = 1;  numTriLin = 20;
      while ( numTriLin < surfaces_orig[i_morph]->N_FaceSet ) {
         ++j;
         numTriLin = 20*( pow(j,2) );
      }
      
      if ( fabs(numTriLin-surfaces_orig[i_morph]->N_FaceSet) < fabs(numTriBin-surfaces_orig[i_morph]->N_FaceSet) ) {
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
   for (i=0; i<surfaces_orig[i_morph]->N_Node; ++i) {
      j = 3*i;
      ctrX = ctrX + surfaces_orig[i_morph]->NodeList[j];
      ctrY = ctrY + surfaces_orig[i_morph]->NodeList[j+1];
      ctrZ = ctrZ + surfaces_orig[i_morph]->NodeList[j+2];
   }
   ctrX = ctrX/(surfaces_orig[i_morph]->N_Node);
   ctrY = ctrY/(surfaces_orig[i_morph]->N_Node);
   ctrZ = ctrZ/(surfaces_orig[i_morph]->N_Node);
   
   if (UserCenter > -1) {
      SUMA_S_Notev(  "User specified center of surface %s = \n"
                     "  [%.4f   %.4f   %.4f]\n"
                     "Center of mass of surface is = \n"
                     "  [%.4f   %.4f   %.4f]\n" , 
                        surfaces_orig[i_morph]->Label, 
                        Uctr[0], Uctr[1], Uctr[2],
                        ctrX, ctrY, ctrZ);
      ctrX = Uctr[0];
      ctrY = Uctr[1];
      ctrZ = Uctr[2];
   }else{
      if (!SUMA_GetCenterOfSphereSurface(surfaces_orig[i_morph], 500, cent, centmed)) {
         SUMA_S_Err("Failed to estimate center of spherical surface.");
         exit(1);
      }else{
         if (UseCOM) {
            if (verb) {
               SUMA_S_Notev(  "Using (not recommended) center of mass coordinate of \n"
                           "  [%f   %f   %f]\n"
                           "instead of estimated geometric center of:\n"
                           "  [%f   %f   %f]\n"
                           "for surface %s in absence of specified \n"
                           "geometric center.\n",
                              ctrX, ctrY, ctrZ,
                              centmed[0], centmed[1], centmed[2],
                              surfaces_orig[i_morph]->Label );
            }
            sprintf(snote, "Notice: Forced to use COM of [%f   %f   %f], instead of geom. center of [%f   %f   %f] for %s.\n"
                              , ctrX, ctrY, ctrZ,
                              centmed[0], centmed[1], centmed[2],
                              surfaces_orig[i_morph]->Label );
         } else {
            if (verb) {
               SUMA_S_Notev(  "Using (recommended) estimated geometric center of:\n"
                           "  [%f   %f   %f]\n"
                           "rather than center of mass coordinate of \n"
                           "  [%f   %f   %f]\n"
                           "for surface %s in absence of specified \n"
                           "geometric center.\n", 
                              centmed[0], centmed[1], centmed[2], 
                              ctrX, ctrY, ctrZ,
                              surfaces_orig[i_morph]->Label );
            }
            sprintf(snote, "Notice: Used geom. center of [%f   %f   %f] for %s. COM was [%f   %f   %f].\n",
                              centmed[0], centmed[1], centmed[2], surfaces_orig[i_morph]->Label, ctrX, ctrY, ctrZ);
            ctrX = centmed[0];
            ctrY = centmed[1];
            ctrZ = centmed[2];
         }
      }

   }
     
   ctr[0] = 0; ctr[1] = 0; ctr[2] = 0;
   r = sqrt( pow( (surfaces_orig[i_morph]->NodeList[0]-ctrX), 2) + pow( (surfaces_orig[i_morph]->NodeList[1]-ctrY), 2) 
             + pow( (surfaces_orig[i_morph]->NodeList[2]-ctrZ), 2) );
   if (UserRadius > -1) {
      if (verb) {   
         SUMA_S_Notev(  "Surface %s\n"
                     "  User specified radius = %.4f\n"
                     "  Average raidus is     = %.4f\n" , 
                        surfaces_orig[i_morph]->Label, 
                        UserRadius,
                        r);
      } 
      sprintf(snote,"%s User specified radius = %.4f, Average raidus is     = %.4f.", snote, UserRadius, r);
      r = UserRadius;
   } else {
      if (verb) {
         SUMA_S_Notev(  "Surface %s\n"
                     "  Using average radius of %.4f\n", surfaces_orig[i_morph]->Label, r  );
      } 
      sprintf(snote,"%s Using average radius of %.4f", snote, r);
   }
   
   if (verb) SUMA_S_Notev(  "Creating Icodahedron of radius %f and center [%f %f %f]\n", r, ctr[0], ctr[1], ctr[2]);
   /**create icosahedron*/
   icoSurf = SUMA_CreateIcosahedron (r, depth, ctr, bin, 0);
   if (!icoSurf) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_MapIcosahedron.\n", FuncName);
      if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
      if (surfaces_orig) SUMA_free (surfaces_orig);
      if (spec_order) SUMA_free(spec_order);
      if (spec_mapRef) SUMA_free(spec_mapRef);
      if (spec_info) SUMA_free(spec_info);
      if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
      exit (1);
   }
   
   /**write icosahedron to file, if indicated*/
   if ( all_surfs_spec ) {
      sprintf (icoFileNm, "%s_icoSurf.asc", fout);
      if (verb) fprintf (SUMA_STDERR, "\n%s: Now writing surface %s to disk ...\n", FuncName, icoFileNm);
      SUMA_writeFSfile (icoSurf, "!ascii version in FreeSurfer format (MapIcosahedron)", icoFileNm);
      /*add to spec*/
      strcpy  (spec_info[ N_inSpec-1 ].type, "FreeSurfer");
      strcpy  (spec_info[ N_inSpec-1 ].format, "ASCII");
      strcpy  (spec_info[ N_inSpec-1 ].mapRef, "SAME");
      strcpy  (spec_info[ N_inSpec-1 ].dim, "3");
      strcpy  (spec_info[ N_inSpec-1 ].state, "icosahedron");
      strcpy (spec_info[ N_inSpec-1 ].fileToRead, icoFileNm);
   }
   
   
   /**determine morph parameters by mapping icosahedron to spherical brain */
   
   /* start timer */
   SUMA_etime(&start_time,0);
   
   
   if (UseCOM) {/* if old method, reset isSphere flags */
      icoSurf->isSphere = SUMA_GEOM_NOT_SET;
      surfaces_orig[i_morph]->isSphere = SUMA_GEOM_NOT_SET;
   }
   
   MI = SUMA_MapSurface( icoSurf, surfaces_orig[i_morph], verb) ;
   if (!MI) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_MapIcosahedron.\n", FuncName);
      if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
      if (surfaces_orig) SUMA_free (surfaces_orig);
      if (icoSurf) SUMA_Free_Surface_Object(icoSurf);
      if (spec_order) SUMA_free(spec_order);
      if (spec_mapRef) SUMA_free(spec_mapRef);
      if (spec_info) SUMA_free(spec_info);
      if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
      exit (1);
   }
   
   if (WriteMI) {
      FILE *fp=NULL;
      char *fname = SUMA_append_string(fout,"_MI.1D");
      if (!(fp = fopen(fname,"w")) ) {
         SUMA_S_Err("Failed to open %s for writing.");
         exit (1);
      }
      fprintf(fp, "# Col. 0: Std-mesh icosahedron's node index.\n"
                  "# Col. 1..3: 1st..3rd closest nodes from original mesh (%s)\n"
                  "# History:%s\n"
                  "# %s\n"
                  , surfaces_orig[i_morph]->Label, histnote, snote);
      for (i=0; i<MI->N_Node; ++i) {
         fprintf(fp, "%6d   %6d %6d %6d\n", 
            i, MI->ClsNodes[3*i], MI->ClsNodes[3*i+1], MI->ClsNodes[3*i+2]); 
      }
      SUMA_free(fname); fname=NULL; 
      fclose(fp); fp = NULL;   
   }
   
   etime_MapSurface = SUMA_etime(&start_time,1);

   
   /**morph surfaces backwards and write to file
      (using weighting from SUMA_MapSurfaces)*/
   
   for (id=0; id<mx_N_surf; ++id) {

      if ( spec_order[id] != -1 ) {
         /*can only morph surfaces given in spec file*/
         
         /*get morphed surface object*/
         if ( surfaces_orig[id]->EL==NULL) 
            SUMA_SurfaceMetrics(surfaces_orig[id], "EdgeList", NULL);
         if ( surfaces_orig[id]->EL && surfaces_orig[id]->N_Node) 
            surfaces_orig[id]->FN = SUMA_Build_FirstNeighb( surfaces_orig[id]->EL, surfaces_orig[id]->N_Node, surfaces_orig[id]->idcode_str);
         if ( surfaces_orig[id]->FN==NULL || surfaces_orig[id]->EL==NULL ) {
            fprintf(SUMA_STDERR, "Error %s: Failed in acquired Surface Metrics.\n", FuncName);
            if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
            if (surfaces_orig) SUMA_free (surfaces_orig);
            if (icoSurf) SUMA_Free_Surface_Object(icoSurf);
            if (currSurf) SUMA_free (currSurf);
            if (spec_order) SUMA_free(spec_order);
            if (spec_mapRef) SUMA_free(spec_mapRef);
            if (spec_info) SUMA_free(spec_info);
            if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
            exit (1);
         }            

         currSurf = SUMA_morphToStd( surfaces_orig[id], MI, YUP);
         if ( !currSurf ) {
            fprintf(SUMA_STDERR, "Error %s: Failed in morphing surface object.\n", FuncName);
            if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
            if (surfaces_orig) SUMA_free (surfaces_orig);
            if (icoSurf) SUMA_Free_Surface_Object(icoSurf);
            if (currSurf) SUMA_free (currSurf);
            if (spec_order) SUMA_free(spec_order);
            if (spec_mapRef) SUMA_free(spec_mapRef);
            if (spec_info) SUMA_free(spec_info);
            if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
            exit (1);
         }            
         currSurf->FileType = surfaces_orig[id]->FileType;
         
         /*smooth surface, if indicated*/
         /*(only for smwm, pial or white surfaces)*/
         if ( smooth && ( id==0 || id==1 || id==5 ) ) {

            bpf = 0.1;
            if ( !SUMA_Taubin_Smooth_Coef (bpf, &lambda, &mu) )
               fprintf(SUMA_STDERR, "Error %s: Failed in acquiring Taubin Coefficients.  Surface will not be smoothed.\n\n", FuncName);
            
            else {
               d_order =  SUMA_ROW_MAJOR; 
               currSurf->FN = icoSurf->FN;  /*all smwm pial and white surfaces have same connectivity as icoSurf*/

               smNodeList = SUMA_Taubin_Smooth (currSurf, NULL, lambda, mu, currSurf->NodeList, 
                                                2*numIt, 3, d_order, NULL, cs, NULL, 1);
               if ( !smNodeList ) 
                  fprintf(SUMA_STDERR, "Error %s: Failed in Taubin Smoothing.  Surface will not be smoothed.\n\n", FuncName);
               else {
                  SUMA_free( currSurf->NodeList);
                  currSurf->NodeList = smNodeList;
               }
            }
         }
         
         if (!UseCOM) {
            /* project to sphere only if spheres */
            SUMA_SetSphereParams(surfaces_orig[id], 0.2);
            if (surfaces_orig[id]->isSphere == SUMA_GEOM_SPHERE) {
               if ( verb ) SUMA_S_Note("Projecting standard mesh surface to sphere");
               SUMA_ProjectToSphere(currSurf, surfaces_orig[id]->SphereCenter, surfaces_orig[id]->SphereRadius);
            }
         } 
         
         /*write to file*/
         if ( all_surfs_spec ) i_surf = 2*spec_order[id];
         else i_surf = spec_order[id];
         
         if (verb) fprintf (SUMA_STDERR, "%s: Now writing surface %s to disk ...\n", FuncName, spec_info[i_surf].fileToRead);
         writeFile = NOPE;
         if ( SUMA_iswordin(spec_info[i_surf].type, "FreeSurfer") ==1) 
            writeFile = SUMA_Save_Surface_Object (spec_info[i_surf].fileToRead, currSurf, SUMA_FREE_SURFER, SUMA_ASCII, NULL);
         else if ( SUMA_iswordin(spec_info[i_surf].type, "Ply") ==1) 
            writeFile = SUMA_Save_Surface_Object (spec_info[i_surf].fileToRead, currSurf, SUMA_PLY, SUMA_FF_NOT_SPECIFIED, NULL);
         else if ( SUMA_iswordin(spec_info[i_surf].type, "Vec") ==1) 
            writeFile = SUMA_Save_Surface_Object (spec_info[i_surf].fileToRead, currSurf, SUMA_VEC, SUMA_ASCII, NULL);
         else {
            fprintf(SUMA_STDERR, "\n** Surface format (%s) is not currently handled by this program due to lack of data.\n"
                                 "If you would like this option to be added, please contact\n"
                                 "   saadz@mail.nih.gov or brenna.argall@nih.gov.\n\n", spec_info[i_surf].type); 
            exit (0);
         }
         
         if ( !writeFile ) {
            fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
            if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
            if (surfaces_orig) SUMA_free (surfaces_orig);
            if (icoSurf) SUMA_Free_Surface_Object(icoSurf);
            if (currSurf) SUMA_free (currSurf);
            if (spec_order) SUMA_free(spec_order);
            if (spec_mapRef) SUMA_free(spec_mapRef);
            if (spec_info) SUMA_free(spec_info);
            if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
            exit (1);
         }

         if ( currSurf->FN ) currSurf->FN=NULL;
         if ( currSurf ) SUMA_Free_Surface_Object( currSurf );
         currSurf = NULL;
      }
   }
   
   
   /*write spec file*/
   sprintf (outSpecFileNm, "%s_std.spec", fout);
   SUMA_writeSpecFile ( spec_info, N_inSpec, FuncName, fout, outSpecFileNm, histnote );
   
   
   if (verb) fprintf (SUMA_STDERR, "\nSUMA_MapSurface took %f seconds to execute.\n", etime_MapSurface); 
   fprintf (SUMA_STDERR, "\n**\t\t\t\t\t**\n\t  To view in SUMA, run:\n\tsuma -spec %s \n**\t\t\t\t\t**\n\n", outSpecFileNm);
   
   
   /* free variables */
   if (!SUMA_FreeSpecFields(&brainSpec)) {
      SUMA_S_Err("Faile to free spec fields");
   }
   if (spec_order) SUMA_free(spec_order);
   if (spec_mapRef) SUMA_free(spec_mapRef);
   if (spec_info) SUMA_free(spec_info);
   if (MI) SUMA_Free_MorphInfo (MI);
   if (histnote) SUMA_free(histnote);

   /*free surfaces*/
   if (icoSurf) SUMA_Free_Surface_Object (icoSurf);
   if (currSurf) { SUMA_Free_Surface_Object(currSurf);} 
   if (SUMAg_DOv) SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv);
   if (surfaces_orig) SUMA_free (surfaces_orig);
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   SUMA_RETURN(0);
   
}/* main SUMA_MapIcosahedron*/

