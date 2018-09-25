#include "SUMA_suma.h"

void usage_SUMA_quickspec(SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"usage_SUMA_quickspec"};
   char * s = NULL, *sio=NULL;
   sio  = SUMA_help_IO_Args(ps);

   printf (
"\nUsage:  quickspec \n"
"        <-tn TYPE NAME> ...\n"
"        <-tsn TYPE STATE NAME> ...\n"
"        [<-spec specfile>] [-h/-help]\n"
"  Use this spec file for quick and dirty way of \n"
"  loading a surface into SUMA or the command line programs.\n"
"\n"
"Options:\n"
"%s\n"
"   -tsnad TYPE STATE NAME ANATFLAG LDP: \n"
"                 specify surface type, state, name, anatomical correctness, \n"
"                 and its Local Domain Parent.\n"
"        ANATFLAG: 'Y' if surface is anatomically correct (default).\n"
"                  'N' if it is not anatomically correct.\n"
"        LDP: Name of Local Domain Parent surface.\n"
"             Use SAME (default) if surface is its own LDP.\n"
"   -tsnadm TYPE STATE NAME ANATFLAG LDP MARKER: \n"
"                 specify surface type, state, name, anatomical correctness, \n"
"                 Local Domain Parent, and node marker file.\n"
"        MARKER: A niml.do Displayable Object (DO) to put at every\n"
"                node of the surface. See @DO.examples for information\n"
"                about displayable objects\n"
"   -tsnadl TYPE STATE NAME ANATFLAG LDP LABELDSET: \n"
"                 specify surface type, state, name, anatomical correctness, \n"
"                 Local Domain Parent, and a label dataset file.\n"
"        LABELDSET: A surface dataset containing node labels.\n"
"   -spec specfile: Name of spec file output.\n"
"                   Default is quick.spec\n"
"                   The program will only overwrite \n"
"                   quick.spec (the default) spec file.\n"
"   -h or -help: This message here.\n"
"\n"
"  You can use any combinaton of -tn and -tsn options.\n"
"  Fields in the spec file that are (or cannot) be specified\n"
"  by this program are set to default values.\n"
"\n   This program was written to ward off righteous whiners and is\n"
"  not meant to replace the venerable @SUMA_Make_Spec_XX scripts.\n"
"\n"
     , sio); SUMA_free(sio); sio = NULL;
     s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
     printf("      Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \n\t\t Tue Dec 30\n"
            "\n");
    return;
}

int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"quickspec"};
   int detail, kar, i, j, N_surf, N_name, idefstate;
   FILE *fid = NULL;
   char *spec_name, stmp[500], *Unique_st;
   SUMA_SO_File_Type TypeC[SUMA_MAX_N_SURFACE_SPEC];
   static char
         *State[SUMA_MAX_N_SURFACE_SPEC],
         *Name_coord[SUMA_MAX_N_SURFACE_SPEC],
         *Name_topo[SUMA_MAX_N_SURFACE_SPEC],
         Anat[SUMA_MAX_N_SURFACE_SPEC],
         *LDP[SUMA_MAX_N_SURFACE_SPEC],
         *MARK[SUMA_MAX_N_SURFACE_SPEC],
         *LABEL[SUMA_MAX_N_SURFACE_SPEC];
   SUMA_GENERIC_ARGV_PARSE *ps;
   SUMA_Boolean brk;

   SUMA_mainENTRY;
   
   /* allocate space for CommonFields structure */
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf( SUMA_STDERR,
               "Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   
   ps = SUMA_Parse_IO_Args(argc, argv, "-t;");

   if (argc < 3)
       {
          usage_SUMA_quickspec (ps);
          exit (0);     /* status 0 on -help    18 Sep 2018 [rickr] */
       }

   kar = 1;
   brk = NOPE;
   detail = 1;
   N_surf = 0;
   N_name = 0;
   spec_name = NULL;
   while (kar < argc) { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
          usage_SUMA_quickspec(ps);
          exit (0);     /* status 0 on -help    18 Sep 2018 [rickr] */
      }
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -spec \n");
            exit (1);
         }
         spec_name = argv[kar];
         if (!THD_ok_overwrite() && SUMA_filexists(spec_name)) {
            fprintf (SUMA_STDERR, 
               "File %s exists, choose another one.\n", spec_name);
            exit(1);
         }
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-tn") == 0)) {
         if (N_surf >= SUMA_MAX_N_SURFACE_SPEC) {
            SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
            exit(1);
         }
         /* get the type */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "Type argument must follow -tn \n");
            exit (1);
         }
         TypeC[N_surf] = SUMA_SurfaceTypeCode(argv[kar]);
         if (TypeC[N_surf] == SUMA_FT_ERROR ||
             TypeC[N_surf] == SUMA_FT_NOT_SPECIFIED) {
            fprintf (SUMA_STDERR, "%s is a bad file type.\n", argv[kar]);
            exit(1);
         }
         /* get the name */
         if (TypeC[N_surf] == SUMA_SUREFIT || TypeC[N_surf] == SUMA_VEC)
            N_name = 2;
         else N_name = 1;
         if (kar+N_name >= argc)  {
            fprintf (SUMA_STDERR, "need %d elements for NAME \n", N_name);
            exit (1);
         }
         kar ++; Name_coord[N_surf] = argv[kar];
         if (N_name == 2) {
            kar ++; Name_topo[N_surf] = argv[kar];
         } else {
            Name_topo[N_surf] = NULL;
         }
         State[N_surf] = NULL;
         Anat[N_surf] = 'Y';
         LDP[N_surf] = NULL;
         ++N_surf; 
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-tsn") == 0)) {
         if (N_surf >= SUMA_MAX_N_SURFACE_SPEC) {
            SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
            exit(1);
         }
         /* get the type */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "TYPE argument must follow -tsn \n");
            exit (1);
         }
         TypeC[N_surf] = SUMA_SurfaceTypeCode(argv[kar]);
         if (  TypeC[N_surf] == SUMA_FT_ERROR ||
               TypeC[N_surf] == SUMA_FT_NOT_SPECIFIED) {
            fprintf (SUMA_STDERR, "%s is a bad file TYPE.\n", argv[kar]);
            exit(1);
         }
         /* get the state */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                     "STATE argument must follow TYPE with -tsn \n");
            exit (1);
         }
         State[N_surf] = argv[kar];

         /* get the name */
         if (  TypeC[N_surf] == SUMA_SUREFIT ||
               TypeC[N_surf] == SUMA_VEC) N_name = 2;
         else N_name = 1;
         if (kar+N_name >= argc)  {
            fprintf (SUMA_STDERR, "need %d elements for NAME \n", N_name);
            exit (1);
         }
         kar ++; Name_coord[N_surf] = argv[kar];
         if (N_name == 2) {
            kar ++; Name_topo[N_surf] = argv[kar];
         } else {
            Name_topo[N_surf] = NULL;
         }

         Anat[N_surf] = 'Y';
         LDP[N_surf] = NULL;
         ++N_surf; 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-tsnad") == 0)) {
         if (N_surf >= SUMA_MAX_N_SURFACE_SPEC) {
            SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
            exit(1);
         }
         /* get the type */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "TYPE argument must follow -tsnad \n");
            exit (1);
         }
         TypeC[N_surf] = SUMA_SurfaceTypeCode(argv[kar]);
         if (  TypeC[N_surf] == SUMA_FT_ERROR ||
               TypeC[N_surf] == SUMA_FT_NOT_SPECIFIED) {
            fprintf (SUMA_STDERR, "%s is a bad file TYPE.\n", argv[kar]);
            exit(1);
         }
         /* get the state */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                     "STATE argument must follow TYPE with -tsnad \n");
            exit (1);
         }
         State[N_surf] = argv[kar];

         /* get the name */
         if (  TypeC[N_surf] == SUMA_SUREFIT ||
               TypeC[N_surf] == SUMA_VEC) N_name = 2;
         else N_name = 1;
         if (kar+N_name >= argc)  {
            fprintf (SUMA_STDERR, "need %d elements for NAME \n", N_name);
            exit (1);
         }
         kar ++; Name_coord[N_surf] = argv[kar];
         if (N_name == 2) {
            kar ++; Name_topo[N_surf] = argv[kar];
         } else {
            Name_topo[N_surf] = NULL;
         }


         /* get the anatomical flag */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                     "Anatomical flag must follow NAME with -tsnad \n");
            exit (1);
         }
         Anat[N_surf] = SUMA_TO_UPPER_C(argv[kar][0]);
         if (Anat[N_surf] != 'Y' && Anat[N_surf] != 'N') {
            SUMA_S_Err("Anatomical flag must be either 'y' or 'n'");
            exit (1);
         }
         /* get the LDP */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                 "LocalDomainParent must follow Anatomical flag with -tsnad \n");
            exit (1);
         }
         LDP[N_surf] = argv[kar];
         
         ++N_surf; 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-tsnadm") == 0)) {
         if (N_surf >= SUMA_MAX_N_SURFACE_SPEC) {
            SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
            exit(1);
         }
         /* get the type */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "TYPE argument must follow -tsnad \n");
            exit (1);
         }
         TypeC[N_surf] = SUMA_SurfaceTypeCode(argv[kar]);
         if (  TypeC[N_surf] == SUMA_FT_ERROR ||
               TypeC[N_surf] == SUMA_FT_NOT_SPECIFIED) {
            fprintf (SUMA_STDERR, "%s is a bad file TYPE.\n", argv[kar]);
            exit(1);
         }
         /* get the state */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                     "STATE argument must follow TYPE with -tsnad \n");
            exit (1);
         }
         State[N_surf] = argv[kar];

         /* get the name */
         if (  TypeC[N_surf] == SUMA_SUREFIT ||
               TypeC[N_surf] == SUMA_VEC) N_name = 2;
         else N_name = 1;
         if (kar+N_name >= argc)  {
            fprintf (SUMA_STDERR, "need %d elements for NAME \n", N_name);
            exit (1);
         }
         kar ++; Name_coord[N_surf] = argv[kar];
         if (N_name == 2) {
            kar ++; Name_topo[N_surf] = argv[kar];
         } else {
            Name_topo[N_surf] = NULL;
         }


         /* get the anatomical flag */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                     "Anatomical flag must follow NAME with -tsnad \n");
            exit (1);
         }
         Anat[N_surf] = SUMA_TO_UPPER_C(argv[kar][0]);
         if (Anat[N_surf] != 'Y' && Anat[N_surf] != 'N') {
            SUMA_S_Err("Anatomical flag must be either 'y' or 'n'");
            exit (1);
         }
         /* get the LDP */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                 "LocalDomainParent must follow Anatomical flag with -tsnad \n");
            exit (1);
         }
         LDP[N_surf] = argv[kar];

         /* get the nodeMarker */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                 "LocalDomainParent must follow Anatomical flag with -tsnad \n");
            exit (1);
         }
         MARK[N_surf] = argv[kar];
         ++N_surf; 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-tsnadl") == 0)) {
         if (N_surf >= SUMA_MAX_N_SURFACE_SPEC) {
            SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
            exit(1);
         }
         /* get the type */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "TYPE argument must follow -tsnad \n");
            exit (1);
         }
         TypeC[N_surf] = SUMA_SurfaceTypeCode(argv[kar]);
         if (  TypeC[N_surf] == SUMA_FT_ERROR ||
               TypeC[N_surf] == SUMA_FT_NOT_SPECIFIED) {
            fprintf (SUMA_STDERR, "%s is a bad file TYPE.\n", argv[kar]);
            exit(1);
         }
         /* get the state */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                     "STATE argument must follow TYPE with -tsnad \n");
            exit (1);
         }
         State[N_surf] = argv[kar];

         /* get the name */
         if (  TypeC[N_surf] == SUMA_SUREFIT ||
               TypeC[N_surf] == SUMA_VEC) N_name = 2;
         else N_name = 1;
         if (kar+N_name >= argc)  {
            fprintf (SUMA_STDERR, "need %d elements for NAME \n", N_name);
            exit (1);
         }
         kar ++; Name_coord[N_surf] = argv[kar];
         if (N_name == 2) {
            kar ++; Name_topo[N_surf] = argv[kar];
         } else {
            Name_topo[N_surf] = NULL;
         }


         /* get the anatomical flag */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                     "Anatomical flag must follow NAME with -tsnad \n");
            exit (1);
         }
         Anat[N_surf] = SUMA_TO_UPPER_C(argv[kar][0]);
         if (Anat[N_surf] != 'Y' && Anat[N_surf] != 'N') {
            SUMA_S_Err("Anatomical flag must be either 'y' or 'n'");
            exit (1);
         }
         /* get the LDP */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                 "LocalDomainParent must follow Anatomical flag with -tsnad \n");
            exit (1);
         }
         LDP[N_surf] = argv[kar];

         /* get the nodeMarker */
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, 
                 "LocalDomainParent must follow Anatomical flag with -tsnad \n");
            exit (1);
         }
         LABEL[N_surf] = argv[kar];
         ++N_surf; 
         brk = YUP;
      }
      
      if (!brk) {
         fprintf (SUMA_STDERR,
                  "Error %s: Option %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
         exit (1);
      } else { 
         brk = NOPE;
         kar ++;
      }
   }

   /* write out the comments */
   if (!spec_name) {
      fid = fopen("quick.spec", "w");
   } else {
      fid = fopen(spec_name,"w");
   }
   if (!fid){
      SUMA_SL_Err("Failed to open file for output");
      exit(1);
   }
   fprintf(fid,"# define the group\n");
   fprintf(fid,"\tGroup = QuickSpec\n");


   /* now create a list of unique states */
   idefstate = 0;
   if (!State[0]) {
      Unique_st = SUMA_copy_string ("\tStateDef = S_1\n");
      idefstate = 1;
   } else {
      sprintf(stmp, "\tStateDef = %s\n", State[0]);
      Unique_st = SUMA_copy_string (stmp);
   }
   for (i=1; i < N_surf; ++i) {
      if (!State[i]) {
         ++idefstate;
         sprintf(stmp,"\tStateDef = S_%d\n", idefstate);
         Unique_st = SUMA_append_replace_string (Unique_st, stmp, "", 1);
      } else {
         if (SUMA_iswordin(Unique_st, State[i]) != 1) {
            sprintf(stmp, "\tStateDef = %s\n", State[i]);
            Unique_st = SUMA_append_replace_string(Unique_st, stmp, "", 1);
         }
      }
   }
   fprintf (fid, "# define the various States\n");
   fprintf (fid, "%s\n", Unique_st);

   /* check on LDP correctness */
   for (i=0; i < N_surf; ++i) {
      if (LDP[i]) {
         if (!strcmp(LDP[i],"same") || !strcmp(LDP[i],"Same"))
            SUMA_TO_UPPER(LDP[i]);
         if (strcmp(LDP[i],"SAME")) {
            j= 0;
            while (j<N_surf && strcmp(LDP[i], Name_coord[j])) ++j;
            if (j == N_surf) {
               SUMA_S_Errv("Could not find a surface named %s\n"
                           "to be the local domain parent of %s\n",
                           LDP[i], Name_coord[i]);
               exit(1);
            }
            if (!strcmp(LDP[i], Name_coord[i])) {/* reset to SAME*/
               LDP[i] = NULL; /* this results is SAME below */
            }
         }
      }
   }
   /* now loop accross surfaces and write out the results */
   idefstate = 0;
   for (i=0; i < N_surf; ++i) {
      fprintf(fid, "\nNewSurface\n");
      fprintf(fid, "\tSurfaceType = %s\n", SUMA_SurfaceTypeString(TypeC[i]));
      if (!State[i]) {
         ++idefstate;
         fprintf(fid, "\tSurfaceState = S_%d\n", idefstate);
      } else fprintf(fid, "\tSurfaceState = %s\n", State[i]);
      if (Name_topo[i]) {
         fprintf(fid, "\tCoordFile = %s\n", Name_coord[i]);
         fprintf(fid, "\tTopoFile = %s\n", Name_topo[i]);
      } else {
         fprintf(fid, "\tSurfaceName = %s\n", Name_coord[i]);
      }
      /* add LocalDomainParent */
      if (LDP[i]) fprintf(fid, "\tLocalDomainParent = %s\n", LDP[i]);
      else fprintf(fid, "\tLocalDomainParent = SAME\n");
      /* add Anatomical */
      if (Anat[i]) fprintf(fid, "\tAnatomical = %c\n", Anat[i]);
      else fprintf(fid, "\tAnatomical = Y\n");
      /* add nodeMarker */
      if (MARK[i]) fprintf(fid, "\tNodeMarker = %s\n", MARK[i]);
      if (LABEL[i]) fprintf(fid, "\tLabelDset = %s\n", LABEL[i]);

      /* binary ? */
      switch (TypeC[i]) {
         case SUMA_FREE_SURFER:
            if (!SUMA_isExtension(Name_coord[i], ".asc")) {
               fprintf(fid, "\tSurfaceFormat = BINARY\n");
            }
            break;
         default:
            break;
      }
   }

   fclose(fid); fid = NULL;

   if (Unique_st) SUMA_free(Unique_st); Unique_st = NULL;

   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {
      fprintf(SUMA_STDERR,"Error %s: SUMAg_CF Cleanup Failed!\n", FuncName);
      exit(1);
   }

   SUMA_RETURN(0);

}/* main quickspec */
