#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "SUMA_suma.h"
#include "SUMA_GeomComp.h"
#include "SUMA_gts.h"

#define SURFPATCH_MAX_SURF 1  /*!< Maximum number of input surfaces */

/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

SUMA_Boolean LocalHead = NOPE;


void usage_SUMA_getPatch ()
{
   static char FuncName[]={"usage_SUMA_getPatch"};
   char * s = NULL;
   s = SUMA_help_basics();
   printf ( "\nUsage:\n"
            "  SampBias -spec SPECFILE -surf SURFNAME\n"
            "\n"
            "  Mandatory parameters:\n"
            "     -spec SpecFile: Spec file containing input surfaces.\n"
            "     -surf SURFNAME: Name of input surface \n"
            "     -plimit limit: maximum length of path along surface in mm.\n"
            "                    default is 50 mm\n"
            "     -dlimit limit: maximum length of euclidean distance in mm.\n"
            "                    default is 1000 mm\n"
            "\n"
            "\n"
            "%s"
            "\n",s); SUMA_free(s); s = NULL;
   s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
   printf(" blame Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
   exit (0);
}
    
typedef struct {
   SUMA_SO_File_Type iType;
   char *sv_name;
   char *surf_names[SURFPATCH_MAX_SURF];
   int N_surf;
   char *spec_file;
   float box_dim[3];
   float plimit;
   float dlimit;
   char *outfile;
} SUMA_KUBATEST_OPTIONS;

SUMA_KUBATEST_OPTIONS *Opt=NULL;

/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_GETPATCH_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_prefix); 
               SUMA_free(Opt);
*/
SUMA_KUBATEST_OPTIONS *SUMA_GetPatch_ParseInput (char *argv[], int argc)
{
   static char FuncName[]={"SUMA_GetPatch_ParseInput"}; 
   int kar, i, ind;
   char *outprefix;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   Opt = (SUMA_KUBATEST_OPTIONS *)SUMA_malloc(sizeof(SUMA_KUBATEST_OPTIONS));

   kar = 1;
   Opt->iType = SUMA_FT_NOT_SPECIFIED;
   Opt->sv_name = NULL;
   Opt->spec_file = NULL;
   Opt->N_surf = -1;
   for (i=0; i<SURFPATCH_MAX_SURF; ++i) 
      Opt->surf_names[i] = NULL;
   brk = NOPE;
   for (i=0; i<3; i++)
      Opt->box_dim[i] = 0;
   Opt->plimit = 50;
   Opt->dlimit = 1000;
   Opt->outfile = NULL;


   while (kar < argc) 
   { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0)
      {
         usage_SUMA_getPatch();
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
      if (!brk && (strcmp(argv[kar], "-size") == 0)) 
      {
         if (kar+3 >= argc)  
         {
            fprintf (SUMA_STDERR, "need 3 coordinates after -size \n");
            exit (1);
         }
         Opt->box_dim[0] = atof(argv[kar+1]);
         Opt->box_dim[1] = atof(argv[kar+2]);
         Opt->box_dim[2] = atof(argv[kar+3]);
         kar += 3;
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
         Opt->outfile = argv[kar];
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
   SUMA_RETURN (Opt);
}

double distance(SUMA_SurfaceObject *SO, int n, int m)
{
   int i;
   double result = 0;
   for (i = 0; i<3; i++)
      result += pow(SO->NodeList[n*3+i] - SO->NodeList[m*3+i], 2);
   return sqrt(result);
}

void calcWithDijkstra (SUMA_SurfaceObject *SO, float* box_dim)
{
   static char FuncName[]={"calcWithDijkstra"};
   const int istart = 0;
   const int istop = SO->N_Node;
   int n =0, j =0, N_nPath =0, N_isNodeInMesh=0;
   FILE *outFile = fopen("Test1.1D.dset", "w");
   SUMA_Boolean isNodeInMesh[SO->N_Node]; //subset (whole set) of nodes that dijkstra will use
   struct timeval t;
   
   for (j=0; j<SO->N_Node; j++)
      isNodeInMesh[j] = NOPE;
   //float Result[SO->N_Node][5];
   //int iResult = 0;
   if (!outFile)
   {
      fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, "Test.1D.dset");
      exit(1);
   }
   SUMA_etime(&t, 0);
   for (n=istart; n < istop; n++)
   { //loops through nodes
      double smallestR = 1;
      int smallestm = 0;
      float Dj = 0; // dijkstra distance
      double smallestDj = 0;
      double smallestD = 0; //smallest straight line distance
      int *nPath = NULL;
      SUMA_ISINBOX ans = SUMA_isinbox(SO->NodeList, SO->N_Node, &(SO->NodeList[n*3]), box_dim, 1);
      //if (!(n%10)) printf("at node %i\n", n);
      N_isNodeInMesh = ans.nIsIn;
      printf("n = %i, n in box = %i, time = %f, projected time = %f hours\n",n, ans.nIsIn, SUMA_etime(&t,1), SUMA_etime(&t, 1)/(float)n*SO->N_Node/3600);

      for (j=0; j < ans.nIsIn; ++j)
      { //loops through nodes inthe box around node n
         int m = ans.IsIn[j]; /* a neighbor of n in box */
         if ( m != n )
         { //ignore path to itself
            //printf("looking at path from %i to %i\n",n, m);
            int t;
            float straightD =0, ratio=1;
            for (t=0; t<SO->N_Node; t++)
               isNodeInMesh[t] = NOPE;
            for (t=0; t < ans.nIsIn; t++)
               isNodeInMesh[ans.IsIn[t]] = YUP; /* a neighbor of in */
            N_isNodeInMesh = ans.nIsIn;
            //printf("isNodeInMesh[%i] = %i\n",m, isNodeInMesh[m]);
            nPath = SUMA_Dijkstra  (SO, n, m, isNodeInMesh, &N_isNodeInMesh, 1, &Dj, &N_nPath);
            if (nPath)
            {
               straightD = distance(SO, n, m);
               ratio = straightD / Dj;
               /*printf( "to node %i\n"
                  "straight distance: %f\n"
                  "dijkstra distance: %f\n"
                  "ratio: %f\n",
                  m, straightD, D, ratio);
               */
            }
            //else printf("error no path found\n");
            nPath = NULL;
            //printf("ratio %f smallestR %f\n", ratio, smallestR);
            if ( ratio < smallestR)
            {
               smallestR = ratio;
               smallestm = m;
               smallestDj = Dj;
               smallestD = straightD;
               //printf("smallest ratio is now: %f\n", smallestR);
            }
         }
      }
      //Result[i][0] = (double) n; // node n
      //Result[i][1] = (double) smallestm;// the node m for which R was the smallest (neighbor)
      //Result[i][2] = smallestR; //smallest ratio for smallest m
      //i++;
      //fprintf(stdout, "node %i to node %i, ratio: %f\n", n, smallestm, smallestR);
      fprintf(outFile, "%i\t%i\t%f\t%f\t%f\n", n, smallestm, smallestD, smallestDj, smallestR);
   }
   fclose(outFile);
}

void calcWithOffsets(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"calcWithOffsets"};
   /* initialize OffS */
   SUMA_GET_OFFSET_STRUCT *OffS = SUMA_Initialize_getoffsets (SO->N_Node);
   struct timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all;
   int i = 0;
   FILE* outFile = NULL;
   SUMA_Boolean write = YUP;
   if (write)
   {
      outFile = fopen(Opt->outfile, "r");
      if(outFile == NULL)
         outFile = fopen(Opt->outfile, "w");
      else
      {
         fclose(outFile);
         fprintf(SUMA_STDERR, "%s already exists\n", Opt->outfile);
         exit(1);
      }  
   }
   SUMA_ENTRY;

   SUMA_etime(&start_time_all,0);
   for (i=0; i < SO->N_Node; ++i)
   {
      float pathD = 0;//shortest distance along surface to node ii
      float geomD = 0;//geometric distance to node ii
      float ratio = 1;
      int j = 0;
      int ii = 0;

      /* show me the offset from node 0 */
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Calculating offsets from node %d\n",FuncName, i);
      if (i == 0) {
         SUMA_etime(&start_time,0);
      }
      SUMA_getoffsets2 (i, SO, Opt->plimit, OffS);
      if (i == 99) {
         etime_GetOffset = SUMA_etime(&start_time,1);
         fprintf(SUMA_STDERR, "%s: Search to %f mm took %f seconds for %d nodes.\n"
                  "Projected completion time: %f minutes\n",
                  FuncName, Opt->plimit, etime_GetOffset, i+1,
                  etime_GetOffset * SO->N_Node / 60.0 / (i+1));
      }
      //find smallest ratio
      for (j=0; j < OffS->N_Nodes; j++)
      {
         if( i!=j && OffS->LayerVect[j] >= 0)
         {
            float d1 = OffS->OffVect[j];
            float d2 = distance (SO, i, j);
            float r = d2 / d1;
            //printf("i=%i j=%i offvect=%f dist=%f r=%f layervect=%i\n", i,j,d1,d2,r, OffS->LayerVect[j]); 
            if ( d2 < Opt->dlimit && r < ratio )
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

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"iotest"};
   //SUMA_KUBATEST_OPTIONS *Opt; 
   int SO_read = -1;
   int i;//, inodeoff=-1, ilabeloff=-1, nvec, ncol, cnt;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfSpecFile Spec;
   void *SO_name = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_mainENTRY;
   
   SUMA_STANDALONE_INIT;
   
   /* Allocate space for DO structure */
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   if (argc < 4)
   {
      usage_SUMA_getPatch();
      exit (1);
   }
   
   SUMA_GetPatch_ParseInput (argv, argc);
   
   /* read all surfaces */
   if (!SUMA_Read_SpecFile (Opt->spec_file, &Spec))
   {
      fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
      exit(1);
   }
   SO_read = SUMA_spec_select_surfs(&Spec, Opt->surf_names, SURFPATCH_MAX_SURF, 0);
   if ( SO_read != Opt->N_surf )
   {
       if (SO_read >=0 )
          fprintf(SUMA_STDERR,"Error %s:\nFound %d surfaces, expected %d.\n", FuncName,  SO_read, Opt->N_surf);
       exit(1);
   }
   /* now read into SUMAg_DOv */
   if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, Opt->sv_name, 0, SUMAg_CF->DsetList) )
   {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
      exit(1);
   }
   
   /* now identify surface needed */
   SO = SUMA_find_named_SOp_inDOv(Opt->surf_names[0], SUMAg_DOv, SUMAg_N_DOv);
   if (!SO)
   {
      fprintf (SUMA_STDERR,"Error %s:\n"
                           "Failed to find surface %s\n"
                           "in spec file. Use full name.\n",
                           FuncName, Opt->surf_names[0]);
      exit(1);
   }
   //SUMA_SurfaceMetrics(SO, "Convexity, EdgeList, MemberFace", NULL);

   //calcWithDijkstra(SO, Opt->box_dim);
   calcWithOffsets(SO);
      
   SUMA_LH("clean up");
   if (Opt) SUMA_free(Opt);
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }

   if (!SUMA_Free_CommonFields(SUMAg_CF)) {SUMA_SL_Err("SUMAg_CF Cleanup Failed!");}
   
   SUMA_RETURN(0);
}
