#include "SUMA_suma.h"
#include "SUMA_Macros.h"

void SUMA_CreateIcosahedron_usage ()
   
{/*Usage*/
   static char FuncName[]={"SUMA_CreateIcosahedron_usage"};
   char * s = NULL;
   
   printf ( 
"\n"
"Usage: CreateIcosahedron [-rad r] [-rd recDepth] [-ld linDepth] \n"
"                         [-ctr ctr] [-prefix fout] [-help]\n"
"\n"
"   -rad r: size of icosahedron. (optional, default 100)\n"
"        The edge length l = 4 r / sqrt(10+2sqrt(5)) \n"
"        The area a = 5 sqrt(3) l^2 \n"
"        The volume v = 5/12 (3+sqrt(5)) l^3 \n"
"\n"
"   -rd recDepth: recursive (binary) tessellation depth for icosahedron \n"
"       (optional, default:3) \n"
"       (recommended to approximate number of nodes in brain: 6\n"
"       let rd2 = 2 * recDepth\n"
"       Nvert = 2 + 10 * 2^rd2\n"
"       Ntri  = 20 * 2^rd2\n"
"       Nedge = 30 * 2^rd2\n"
"\n"
"   -ld linDepth: number of edge divides for linear icosahedron tessellation\n"
"       (optional, default uses binary tessellation).\n"
"       Nvert = 2 + 10 * linDepth^2\n"
"       Ntri  = 20 * linDepth^2\n"
"       Nedge = 30 * linDepth^2\n"
"\n"
"   -min_nodes MIN_NODES: Automatically select the -ld value which produces an\n"
"                         icosahedron of at least MIN_NODES nodes.\n"
"\n"
"   -nums: output the number of nodes (vertices), triangles, edges, \n"
"          total volume and total area then quit\n"
"\n"
"   -nums_quiet: same as -nums but less verbose. For the machine in you.\n"
"\n"
"   -ctr ctr: coordinates of center of icosahedron. \n"
"       (optional, default 0,0,0)\n"
"\n"
"   -tosphere: project nodes to sphere.\n"
"\n"
"   -prefix fout: prefix for output files. \n"
"       (optional, default CreateIco)\n"
"                 The surface is written out in FreeSurfer's .asc\n"
"                 format by default. To change that, include a\n"
"                 valid extension to the prefix such as: fout.gii \n"       
"\n"
"   -help: help message\n"
"\n");
    s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
    printf ("\n"
            "       Brenna D. Argall LBC/NIMH/NIH bargall@codon.nih.gov \n"
            "       Ziad S. Saad     SSC/NIMH/NIH saadz@mail.nih.gov\n");
   exit (0);
}/*Usage*/
/*!
  stand alone program to create an icosahedron and write it to file in Freesurfer format. 

*/
int main (int argc, char *argv[])
{/* main SUMA_CreateIcosahedron */
 
   static char FuncName[]={"SUMA_CreateIcosahedron-main"};
   int kar, depth, i, j;
   float r, ctr[3], a, b, lgth, A = 0.0, V = 0.0;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean brk;
   int NumOnly, ToSphere;
   SUMA_Boolean LocalHead = NOPE;
   char *histnote=NULL;
   char fout[SUMA_MAX_FILENAME_LENGTH];
   char bin[SUMA_MAX_FILENAME_LENGTH];
   char  outSpecFileNm[SUMA_MAX_FILENAME_LENGTH], *fouts=NULL;
   SUMA_SurfSpecFile *stdSpec = NULL;
   void *vbufp=NULL;
   SUMA_SO_File_Format FileFormat = SUMA_ASCII;
   SUMA_SO_File_Type FileType = SUMA_FREE_SURFER;

   SUMA_mainENTRY;
   
   /* allocate space for CommonFields structure */
   if (LocalHead) 
      fprintf (SUMA_STDERR,
               "%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf( SUMA_STDERR,
               "Error %s: Failed in SUMA_Create_CommonFields\n", 
               FuncName);
      exit(1);
   }
   if (LocalHead) 
      fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);
   
   
   /* read in the options */
   r = 100;
   depth = 3;
   ctr[0] = 0; ctr[1] = 0; ctr[2] = 0;
   sprintf (fout, "%s", "CreateIco");
   sprintf (bin, "%s", "y");
   NumOnly = 0;
   ToSphere = 0;
   kar = 1;
   brk = NOPE;
   while (kar < argc) { /* loop across command line options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         SUMA_CreateIcosahedron_usage ();
         exit (1);
      }
            
      if (!brk && (strcmp(argv[kar], "-rad") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -r ");
               exit (1);
            }
            r = atof(argv[kar]);
            brk = YUP;
         }      
      
      if (!brk && (strcmp(argv[kar], "-tosphere") == 0 ))
         {
            ToSphere = 1;
            brk = YUP;
         } 
             
      if (!brk && (strcmp(argv[kar], "-rd") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -rd ");
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
               fprintf (SUMA_STDERR, "need argument after -ld ");
               exit (1);
            }
            depth = atoi(argv[kar]);
            sprintf (bin, "n");
            brk = YUP;
         }      
      if (!brk && (strcmp(argv[kar], "-min_nodes") == 0 ))
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -ld ");
               exit (1);
            }
            {
               int min_nodes = atoi(argv[kar]);
               depth = 1;
               while ((2 + (10 * depth * depth)) < min_nodes 
                        && depth<500) ++depth;
               if (depth >= 500) {
                  fprintf (SUMA_STDERR, 
                        "-min_nodes of %d is too big", min_nodes);
                  exit (1);
               }
               fprintf (SUMA_STDERR, 
                  "Minimum -ld to result in at least %d nodes was: %d\n",
                     min_nodes, depth);
            }
            sprintf (bin, "n");
            brk = YUP;
         }
      if (!brk && (strcmp(argv[kar], "-nums") == 0 ))
         {
            NumOnly = 1;
            brk = YUP;
         }      
      if (!brk && (strcmp(argv[kar], "-nums_quiet") == 0 ))
         {
            NumOnly = 2;
            brk = YUP;
         }   
      if (!brk && strcmp(argv[kar], "-ctr") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -ctr ");
               exit (1);
            }
            ctr[0] = atof(argv[kar]); kar ++;
            ctr[1] = atof(argv[kar]); kar ++;
            ctr[2] = atof(argv[kar]);

            brk = YUP;
         }   

      if (!brk && strcmp(argv[kar], "-prefix") == 0)
         {
            kar ++;
            if (kar >= argc)  {
               fprintf (SUMA_STDERR, "need argument after -so ");
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
      } else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop across command line options */
   histnote = SUMA_HistString (NULL, argc, argv, NULL);
   
   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: depth %d, Size %f.\n", FuncName, depth, r);

   if (NumOnly) {
      /* output counts and quit */
      int Ntri, Nedge, Nvert;
         Nvert = SUMA_IcoNums(depth, !strcmp(bin, "y"), 'v');
         Ntri = SUMA_IcoNums(depth, !strcmp(bin, "y"), 't');
         Nedge = SUMA_IcoNums(depth, !strcmp(bin, "y"), 'e');
      
      SUMA_ICOSAHEDRON_DIMENSIONS(r, a, b, lgth);
      A = 1/4.0 * lgth * lgth * sqrt(3.0);   
         /* surface area, equation from mathworld.wolfram.com */
      V = 5.0 / 12.0 * ( 3 + sqrt(5.0) ) * lgth * lgth * lgth; 
         /* volume, equation from mathworld.wolfram.com*/
      if (NumOnly == 1) 
         fprintf (SUMA_STDOUT,
                  "#Nvert\t\tNtri\t\tNedge\t\tArea\t\t\tVolume\n" 
                  "%d\t\t%d\t\t%d\t\t%f\t\t%f\n", 
                  Nvert, Ntri, Nedge, A, V);
      else fprintf (SUMA_STDOUT," %d\t\t%d\t\t%d\t\t%f\t\t%f\n", 
                                 Nvert, Ntri, Nedge, A, V);
      
      exit(0);
   }
   /**assign output file names */
   FileType = SUMA_GuessSurfFormatFromExtension(fout, "toy.asc"); 
   fouts = SUMA_RemoveSurfNameExtension(fout, FileType);
   SUMA_S_Notev("%s, %s\n", fout, fouts);
   sprintf( outSpecFileNm, 
            "%s%s.spec",
            SUMA_FnameGet(fouts,"pa", SUMAg_CF->cwd), 
            SUMA_FnameGet(fouts,"f", SUMAg_CF->cwd));

   if (SUMA_filexists(outSpecFileNm)) {
      fprintf (SUMA_STDERR,
               "Error %s: \n"
               "Spec filename %s and maybe surface file for prefix %s exists.\n"
               "Will not overwrite.\n", 
               FuncName, outSpecFileNm, fout);
      exit(1);
   }


   /**create icosahedron*/
   SO = SUMA_CreateIcosahedron (r, depth, ctr, bin, ToSphere);
   if (!SO) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_CreateIcosahedron.\n", FuncName);
      exit (1);
   }
   SO->FileFormat = FileFormat;
   SO->FileType = FileType;

   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: Now writing surface %s to disk ...\n", FuncName, fout);


   if (!(vbufp = SUMA_Save_Surface_Object_Wrap(fouts, fouts, SO, 
                               SO->FileType, SO->FileFormat,
                               NULL))) {
         SUMA_S_Err("Failed to write icosahedron");
         exit(1);
   }
   SUMA_free(fouts); fouts = NULL;
   
   /**write spec file*/
   stdSpec = (SUMA_SurfSpecFile *)SUMA_malloc(sizeof(SUMA_SurfSpecFile));
   if (!SUMA_AllocSpecFields(stdSpec)) {
      SUMA_S_Err("Failed to initialize stdSpec\n" );
      exit(1);
   }
   stdSpec->N_Surfs = 0;
   stdSpec->N_States = 1;
   sprintf( stdSpec->Group[0], "Icosahedron");
   sprintf( stdSpec->StateList,"icos.%dvert.%dtri|", 
            SO->N_Node/3, SO->N_FaceSet/3);
   stdSpec->N_Groups = 1;
   strcpy(stdSpec->SpecFilePath, SUMA_FnameGet(fouts,"pa", SUMAg_CF->cwd));
   strcpy(stdSpec->SpecFileName, SUMA_FnameGet(fouts,"f", SUMAg_CF->cwd));  
   
   ++stdSpec->N_Surfs;
   /*add to spec*/
   sprintf  (stdSpec->State[stdSpec->N_Surfs-1], 
             "icos.%dvert.%dtri", 
             SO->N_Node/3, SO->N_FaceSet/3);
   strcpy  (stdSpec->SurfaceType[stdSpec->N_Surfs-1],    
            SUMA_SurfaceTypeString(SO->FileType));
   strcpy  (stdSpec->SurfaceFormat[stdSpec->N_Surfs-1],  
            SUMA_SurfaceFormatString(SO->FileFormat));
   strcpy  (stdSpec->LocalDomainParent[stdSpec->N_Surfs-1], "./SAME");
   strcpy  (stdSpec->AnatCorrect[stdSpec->N_Surfs-1], "N");
   if (  SO->FileType == SUMA_SUREFIT || 
         SO->FileType == SUMA_VEC ) {
      strcpy  (stdSpec->TopoFile[stdSpec->N_Surfs-1], 
               ((SUMA_SFname *)vbufp)->name_topo);
      strcpy  (stdSpec->CoordFile[stdSpec->N_Surfs-1], 
               ((SUMA_SFname *)vbufp)->name_coord);
   } else {
      strcpy  (stdSpec->SurfaceFile[stdSpec->N_Surfs-1], (char *)vbufp);
   }
   stdSpec->EmbedDim[stdSpec->N_Surfs-1] = 3;
   SUMA_free(vbufp); vbufp = NULL; 
   
    if (!SUMA_Write_SpecFile(stdSpec, outSpecFileNm, FuncName, histnote)) {
      SUMA_S_Err("Failed to write spec file!");
      exit(1);
   }
   
   fprintf (SUMA_STDERR, 
            "\n* To view in SUMA, run:\n suma -spec %s \n\n", outSpecFileNm);

   /* free the surface object */
   if (LocalHead) 
      fprintf(SUMA_STDERR, "\n... before free surf in createIco\n\n");
   SUMA_Free_Surface_Object (SO);


   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   if (histnote) SUMA_free(histnote);
   if (fouts) SUMA_free(fouts);
   
   SUMA_RETURN(0);
  
}/* main SUMA_CreateIcosahedron*/


