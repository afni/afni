#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "SUMA_suma.h"

#define SURFPATCH_MAX_SURF 1  /*!< Maximum number of input surfaces */

#ifdef SUMA_SampBias_STAND_ALONE
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
SUMA_Boolean LocalHead = NOPE;


void usage_SUMA_SampBias ()
{
   static char FuncName[]={"usage_SUMA_SampBias"};
   char * s = NULL;
   s = SUMA_help_basics();
   printf ( "\nUsage:\n"
            "  SampBias -spec SPECFILE -surf SURFNAME -plimit limit -dlimit limit -out FILE\n"
            "\n"
            "  Mandatory parameters:\n"
            "     -spec SpecFile: Spec file containing input surfaces.\n"
            "     -surf SURFNAME: Name of input surface \n"
            "     -plimit limit: maximum length of path along surface in mm.\n"
            "                    default is 50 mm\n"
            "     -dlimit limit: maximum length of euclidean distance in mm.\n"
            "                    default is 1000 mm\n"
            "     -out FILE: output dataset\n"
            "\n"
            "\n"
            "%s"
            "\n",s); SUMA_free(s); s = NULL;
   s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
   printf(" blame Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
   exit (0);
}
    
typedef struct {
   SUMA_SO_File_Type iType;
   char *sv_name;
   char *surf_names[SURFPATCH_MAX_SURF];
   int N_surf;
   char *spec_file;
   float plimit;
   float dlimit;
   char *outfile;
   char *histnote;
} SUMA_KUBATEST_OPTIONS;


/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_GETPATCH_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->outfile);
               SUMA_free(Opt->histnote); 
               SUMA_free(Opt);
*/
SUMA_KUBATEST_OPTIONS *SUMA_SampBias_ParseInput (char *argv[], int argc, SUMA_KUBATEST_OPTIONS* Opt)
{
   static char FuncName[]={"SUMA_SampBias_ParseInput"}; 
   int kar, i, ind;
   char *outprefix;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;


   kar = 1;
   Opt->iType = SUMA_FT_NOT_SPECIFIED;
   Opt->sv_name = NULL;
   Opt->spec_file = NULL;
   Opt->N_surf = -1;
   for (i=0; i<SURFPATCH_MAX_SURF; ++i) 
      Opt->surf_names[i] = NULL;
   brk = NOPE;
   Opt->plimit = 50;
   Opt->dlimit = 1000;
   Opt->outfile = NULL;

   while (kar < argc) 
   { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0)
      {
         usage_SUMA_SampBias();
         exit (0);
      }

      SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      if (!brk && (strcmp(argv[kar], "-spec") == 0)) 
      {
         kar ++;
         if (kar >= argc)  
         {
            fprintf (SUMA_STDERR, "need argument after -spec \n");
            exit (1);
         }
         Opt->spec_file = argv[kar];
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-surf") == 0))
      {
         if (kar + 1>= argc)  
         {
            fprintf (SUMA_STDERR, "need argument after -surf SURF_NAME \n");
            exit (1);
         }
         /*ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFPATCH_MAX_SURF) 
         {
            fprintf (SUMA_STDERR, "you can use only one surface\n");
            exit (1);
         }
         */
         kar ++;
         Opt->surf_names[0] = argv[kar];
         Opt->N_surf = 1;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-plimit") == 0)) 
      {
         kar++;
         if (kar >= argc)  
         {
            fprintf (SUMA_STDERR, "need argument after -plimit \n");
            exit (1);
         }
         Opt->plimit = atof(argv[kar]);
         brk = YUP;
      }
     
      if (!brk && (strcmp(argv[kar], "-dlimit") == 0)) 
      {
         kar++;
         if (kar >= argc)  
         {
            fprintf (SUMA_STDERR, "need argument after -dlimit \n");
            exit (1);
         }
         Opt->dlimit = atof(argv[kar]);
         brk = YUP;
      }
     
      if (!brk && (strcmp(argv[kar], "-out") == 0)) 
      {
         kar++;
         if (kar >= argc)  
         {
            fprintf (SUMA_STDERR, "need argument after -out \n");
            exit (1);
         }
         Opt->outfile = SUMA_copy_string(argv[kar]);
         brk = YUP;
      }
     
     if (!brk) 
      {
         fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } else 
      {
         brk = NOPE;
         kar ++;
      }
   }

   /* sanity checks */
   if (Opt->N_surf != 1) 
   {
      SUMA_SL_Err("No surface specified.");
      exit(1);
   }
   if (Opt->spec_file == NULL) 
   {
      SUMA_SL_Err("No spec file specified.");
      exit(1);
   }
   if (Opt->outfile == NULL) 
   {
      SUMA_SL_Err("No outfile specified.");
      exit(1);
   }
   else if (SUMA_filexists(Opt->outfile))
   {
      SUMA_SL_Err("that outfile already exists");
      exit(1);
   }
   
   Opt->histnote = SUMA_HistString (NULL, argc, argv, NULL);
   
   SUMA_RETURN (Opt);
}

void calcWithOffsets(SUMA_SurfaceObject *SO, SUMA_KUBATEST_OPTIONS* Opt)
{
   static char FuncName[]={"calcWithOffsets"};
   /* initialize OffS */
   SUMA_GET_OFFSET_STRUCT *OffS = SUMA_Initialize_getoffsets (SO->N_Node);
   struct timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all;
   int i = 0;
   FILE* outFile = NULL;
   SUMA_Boolean write = YUP;
   
   SUMA_ENTRY;

   outFile = fopen(Opt->outfile, "w");
   fprintf(outFile,  "#Col. 0 Node index\n"
                     "#Col. 1 Node for which the ratio in 4 is the largest. (Companion of Node in Col.0)\n"
                     "#Col. 2 distance in 3D\n"
                     "#Col. 3 shortest surface path\n"
                     "#Col. 4 Ratio of path/distance\n");
   if (Opt->histnote) {
      fprintf(outFile,  "#History:%s\n", Opt->histnote);
   }
      
   SUMA_etime(&start_time_all,0);
   for (i=0; i < SO->N_Node; ++i)
/*   i=45552; */
   {
      float pathD = 0;/*shortest distance along surface to node ii*/
      float geomD = 0;/*geometric distance to node ii*/
      float ratio = 1;
      int j = 0;
      int ii = 0;

      /* show me the offset from node 0 */
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Calculating offsets from node %d\n",FuncName, i);
      if (i == 0) {
         SUMA_etime(&start_time,0);
      }
      SUMA_getoffsets2 (i, SO, Opt->plimit, OffS, NULL, 0);
      if (i == 99) {
         etime_GetOffset = SUMA_etime(&start_time,1);
         fprintf(SUMA_STDERR, "%s: Search to %f mm took %f seconds for %d nodes.\n"
                  "Projected completion time: %f minutes\n",
                  FuncName, Opt->plimit, etime_GetOffset, i+1,
                  etime_GetOffset * SO->N_Node / 60.0 / (i+1));
      }
      /*find smallest ratio*/
      for (j=0; j < OffS->N_Nodes; j++)
      {
         if( i!=j && OffS->LayerVect[j] >= 0)
         {
            float x1 = SO->NodeList[i*3+0];
            float x2 = SO->NodeList[j*3+0];
            float y1 = SO->NodeList[i*3+1];
            float y2 = SO->NodeList[j*3+1];
            float z1 = SO->NodeList[i*3+2];
            float z2 = SO->NodeList[j*3+2];
            float dx = x1 - x2;
            float dy = y1 - y2;
            float dz = z1 - z2;
            float d1 = OffS->OffVect[j];
            float d2 = sqrt(dx*dx + dy*dy + dz*dz);
            float r = d1 / d2;
            /*printf("i=%i j=%i offvect=%f dist=%f r=%f layervect=%i\n", i,j,d1,d2,r, OffS->LayerVect[j]); */
            if ( d2 < Opt->dlimit && d1 < Opt->plimit && r > ratio )
            {
               ratio = r;
               ii = j;
               pathD = d1;
               geomD = d2;
            }
         }
      }
      if (write)
         fprintf(outFile, "%i\t%i\t%f\t%f\t%f\n", i, ii, geomD, pathD, ratio);

      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Recycling OffS\n", FuncName);
      SUMA_Recycle_getoffsets (OffS);
      if (LocalHead)
         fprintf(SUMA_STDERR,"%s: Done.\n", FuncName);
   }
   if (write)
      fclose(outFile);

   etime_GetOffset_all = SUMA_etime(&start_time_all,1);
   fprintf(SUMA_STDERR, "%s: Done.\nSearch to %f mm took %f minutes for %d nodes.\n" ,
                            FuncName, Opt->plimit, etime_GetOffset_all / 60.0 , SO->N_Node);
   SUMA_Free_getoffsets(OffS);
   SUMA_RETURNe;
}

#ifdef SUMA_SampBias_STAND_ALONE
int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"iotest"};
   int SO_read = -1;
   int i;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfSpecFile Spec;
   void *SO_name = NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_KUBATEST_OPTIONS Opt;
   
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   
   /* Allocate space for DO structure */
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   if (argc < 4)
   {
      usage_SUMA_SampBias();
      exit (1);
   }
   
   SUMA_SampBias_ParseInput (argv, argc, &Opt);
   
   /* read all surfaces */
   if (!SUMA_AllocSpecFields(&Spec)) {
      SUMA_S_Err("Failed to AllocateSpecFields");
      exit (1);
   }
   if (!SUMA_Read_SpecFile (Opt.spec_file, &Spec))
   {
      fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
      exit(1);
   }
   SO_read = SUMA_spec_select_surfs(&Spec, Opt.surf_names, SURFPATCH_MAX_SURF, 0);
   if ( SO_read != Opt.N_surf )
   {
       if (SO_read >=0 )
          fprintf(SUMA_STDERR,"Error %s:\nFound %d surfaces, expected %d.\n", FuncName,  SO_read, Opt.N_surf);
       exit(1);
   }
   /* now read into SUMAg_DOv */
   if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, Opt.sv_name, 0, SUMAg_CF->DsetList) )
   {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
      exit(1);
   }
   
   /* now identify surface needed */
   SO = SUMA_find_named_SOp_inDOv(Opt.surf_names[0], SUMAg_DOv, SUMAg_N_DOv);
   if (!SO)
   {
      fprintf (SUMA_STDERR,"Error %s:\n"
                           "Failed to find surface %s\n"
                           "in spec file. Use full name.\n",
                           FuncName, Opt.surf_names[0]);
      exit(1);
   }
   calcWithOffsets(SO, &Opt);
      
   SUMA_LH("clean up");
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }

   if (Opt.outfile) SUMA_free(Opt.outfile);
   if (Opt.histnote) SUMA_free(Opt.histnote);
   if (!SUMA_FreeSpecFields(&Spec)) {
      SUMA_S_Err("Failed to free SpecFields");
   }
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {SUMA_SL_Err("SUMAg_CF Cleanup Failed!");}
   
   
   SUMA_RETURN(0);
}
#endif
