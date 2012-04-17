#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "SUMA_suma.h"

#define SURFPATCH_MAX_SURF 1  /*!< Maximum number of input surfaces */



void usage_SUMA_SampBias (SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"usage_SUMA_SampBias"};
   char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
   s = SUMA_help_basics();
   sio  = SUMA_help_IO_Args(ps);
   printf ( 
"\nUsage:\n"
"  SampBias -spec SPECFILE -surf SURFNAME -plimit limit -dlimit limit -out FILE\n"
"\n"
"  Mandatory parameters:\n"
"     -spec SpecFile: Spec file containing input surfaces.\n"
"     -surf SURFNAME: Name of input surface \n"
"     -plimit limit: maximum length of path along surface in mm.\n"
"                    default is 50 mm\n"
"     -dlimit limit: maximum length of euclidean distance in mm.\n"
"                    default is 1000 mm\n"
"     -out FILE: output results in .1D format.\n"
"     -prefix PREFIX: output results into a proper surface-based\n"
"                     dataset. A more modern version of -out.\n"
"\n"
"           NOTE: FILE and PREFIX (below) have differing numbers\n"
"                 of columns.\n"
"\n"
"     -segdo SEGDO: Output a displayable object file that contains\n"
"                   segments between paired nodes.\n"
"             See 'Ctrl+Alt+s' in SUMA's interactive help\n"
" Example:\n"
"     SampBias -i std12.lh.smoothwm.asc         \\\n"
"              -segdo std12.sampbias.lh         \\\n"
"              -prefix std12.sampbias.lh        \n"
"\n"
"\n"
"%s"
"%s"
"\n",
         ps->hverb > 1 ? sio:"Use -help for more detail.\n", 
         ps->hverb > 1 ? s:""); 
      if (s) SUMA_free(s); s = NULL; 
      if (st) SUMA_free(st); st = NULL; 
      if (sio) SUMA_free(sio); sio = NULL;       

   #if 0
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
   #endif
   printf(" blame Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
   return ;
}
    
typedef struct {
   int debug;
   float plimit;
   float dlimit;
   char *outfile;
   char *histnote;
   char *prefix;
   char *segdo;
   SUMA_GENERIC_ARGV_PARSE *ps;
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
SUMA_KUBATEST_OPTIONS *SUMA_SampBias_ParseInput(
      char *argv[], int argc, SUMA_KUBATEST_OPTIONS* Opt,
      SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SampBias_ParseInput"}; 
   int kar, i, ind;
   char *outprefix;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;


   kar = 1;
   brk = NOPE;
   Opt->debug = 0;
   Opt->plimit = 50;
   Opt->dlimit = 1000;
   Opt->outfile = NULL;
   Opt->prefix = NULL;
   Opt->segdo = NULL;
   Opt->ps=ps;
   while (kar < argc) 
   { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0)
      {
         ps->hverb = strlen(argv[kar])>3?2:1;
         usage_SUMA_SampBias(ps);
         exit (0);
      }

      SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      
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
          
      if (!brk && (strcmp(argv[kar], "-segdo") == 0)) 
      {
         kar++;
         if (kar >= argc)  
         {
            fprintf (SUMA_STDERR, "need argument after -segdo \n");
            exit (1);
         }
         Opt->segdo = SUMA_Extension(argv[kar], ".1D.do", NOPE);
         brk = YUP;
      }
     
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) 
      {
         kar++;
         if (kar >= argc)  
         {
            fprintf (SUMA_STDERR, "need argument after -prefix \n");
            exit (1);
         }
         Opt->prefix = SUMA_copy_string(argv[kar]);
         brk = YUP;
      }
      
      
     if (!brk && !ps->arg_checked[kar]) 
      {
         SUMA_S_Errv("Option %s not understood. Try -help for usage\n", 
                     argv[kar]);
         suggest_best_prog_option(argv[0], argv[kar]);
         exit (1);
      } else 
      {
         brk = NOPE;
         kar ++;
      }
   }

   /* sanity checks */
   if (Opt->outfile == NULL && Opt->prefix == NULL) 
   {
      SUMA_SL_Err("No outfile, or prefix  specified.");
      exit(1);
   } 
   if (Opt->outfile) {
      if (!THD_ok_overwrite() && SUMA_filexists(Opt->outfile)) {
         SUMA_S_Errv("Outfile %s already exists\n", Opt->outfile);
         exit(1);
      }
   }
   if (Opt->prefix) {
      SUMA_DSET_NAME_CHECK(Opt->prefix); 
   }

   Opt->histnote = SUMA_HistString (NULL, argc, argv, NULL);
   
   SUMA_RETURN (Opt);
}

SUMA_DSET *calcWithOffsets(SUMA_SurfaceObject *SO, SUMA_KUBATEST_OPTIONS* Opt)
{
   static char FuncName[]={"calcWithOffsets"};
   /* initialize OffS */
   SUMA_GET_OFFSET_STRUCT *OffS = SUMA_Initialize_getoffsets (SO->N_Node);
   struct timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all;
   float *gD=NULL, *pD=NULL, *rD=NULL;
   int *Pr=NULL, *Nd=NULL, *Lj=NULL;
   float pathD = 0;/*shortest distance along surface to node ii*/
   float geomD = 0;/*geometric distance to node ii*/
   float ratio = 1;
   int i = 0,  j = 0, ii = 0, lj =0;
   float x1, x2 , y1, y2, z1, z2, dx, dy, dz, d1, d2, r;
   FILE* outFile = NULL, *segDO=NULL;
   SUMA_DSET *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (Opt->outfile) {
      if (!(outFile = fopen(Opt->outfile, "w"))) {
         SUMA_S_Errv("Failed to open %s for writing.\n",
                  Opt->outfile);
         SUMA_RETURN(NULL);
      }
      fprintf(outFile,  
         "#Col. 0 Node index\n"
         "#Col. 1 Node for which the ratio in 4 is the largest. "
         " (Companion of Node in Col.0)\n"
         "#Col. 2 distance in 3D\n"
         "#Col. 3 shortest surface path\n"
         "#Col. 4 Ratio of path/distance\n"
         "#Col. 5 Neighborhood layer of node in Col.1, "
         " i.e. Neighborhood layer of the companion of node in Col.0");
      if (Opt->histnote) {
         fprintf(outFile,  "#History:%s\n", Opt->histnote);
      }
   }
   
   if (Opt->segdo) {
      if (!(segDO = fopen(Opt->segdo, "w"))) {
         SUMA_S_Errv("Failed to open %s for writing.\n",
                  Opt->segdo);
         SUMA_RETURN(NULL);
      }
      fprintf(segDO,
         "#node-based_segments\n");
      if (Opt->histnote) {
         fprintf(segDO,  "#History:%s\n", Opt->histnote);
      }
   } 
    
   Nd=(int *)SUMA_calloc(SO->N_Node, sizeof(int));   
   Pr=(int *)SUMA_calloc(SO->N_Node, sizeof(int));   
   Lj=(int *)SUMA_calloc(SO->N_Node, sizeof(int));   
   gD=(float *)SUMA_calloc(SO->N_Node, sizeof(float));   
   pD=(float *)SUMA_calloc(SO->N_Node, sizeof(float));   
   rD=(float *)SUMA_calloc(SO->N_Node, sizeof(float)); 
   if (!Pr || !gD || !pD || !rD) {
      SUMA_S_Err("Failed to allocate");
      SUMA_RETURN(NOPE);
   }  
   
   SUMA_etime(&start_time_all,0);
   for (i=0; i < SO->N_Node; ++i)
   {
      pathD = 0;/*shortest distance along surface to node ii*/
      geomD = 0;/*geometric distance to node ii*/
      ratio = 1;
      j = 0;
      ii = 0;
      lj = -1;
      /* show me the offset from node 0 */
      SUMA_LHv("Calculating offsets from node %d\n", i);
      if (i == 0) {
         SUMA_etime(&start_time,0);
      }
      SUMA_getoffsets2 (i, SO, Opt->plimit, OffS, NULL, 0);
      if (i == 99) {
         etime_GetOffset = SUMA_etime(&start_time,1);
         SUMA_LHv("Search to %f mm took %f seconds for %d nodes.\n"
                  "Projected completion time: %f minutes\n",
                  Opt->plimit, etime_GetOffset, i+1,
                  etime_GetOffset * SO->N_Node / 60.0 / (i+1));
      }
      /*find smallest ratio*/
      for (j=0; j < OffS->N_Nodes; j++)
      {
         if( i!=j && OffS->LayerVect[j] >= 0)
         {
            x1 = SO->NodeList[i*3+0];
            x2 = SO->NodeList[j*3+0];
            y1 = SO->NodeList[i*3+1];
            y2 = SO->NodeList[j*3+1];
            z1 = SO->NodeList[i*3+2];
            z2 = SO->NodeList[j*3+2];
            dx = x1 - x2;
            dy = y1 - y2;
            dz = z1 - z2;
            d1 = OffS->OffVect[j];
            d2 = sqrt(dx*dx + dy*dy + dz*dz);
            r = d1 / d2;
            if ( d2 < Opt->dlimit && d1 < Opt->plimit && r > ratio )
            {
               if (r > 1000) {
                  SUMA_S_Notev("Extreme Ratio:\n"
                        "node=%d (%f %f %f), paired with node %d (%f %f %f)\n"
                        " geo_dist=%f Euc_dist=%f r=%f layer=%d\n", 
                     i, x1, y1, z1,
                     j, x2, y2, z2,
                     d1, d2, r, OffS->LayerVect[j]);
               }
               ratio = r;
               ii = j;
               lj = OffS->LayerVect[j];
               pathD = d1;
               geomD = d2;
            }
         }
      }
      Nd[i] = i;
      Pr[i] = ii;
      Lj[i] = lj;
      gD[i] = geomD;
      pD[i] = pathD;
      rD[i] = ratio;
      if (outFile)
         fprintf(outFile, "%i\t%i\t%f\t%f\t%f\t%d\n", 
                     i, ii, geomD, pathD, ratio, lj);
      if (segDO) {
         float r,g,b,a;
         SUMA_RAND_COL(i?-1:0,r,g,b,a);
         fprintf(segDO, "%i\t%i\t%f %f %f %f\n", i, ii, r,g,b,1.0);
      }
      
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Recycling OffS\n", FuncName);
      SUMA_Recycle_getoffsets (OffS);
      if (LocalHead)
         fprintf(SUMA_STDERR,"%s: Done.\n", FuncName);
   }
   
   if (outFile) fclose(outFile);
   if (segDO) fclose(segDO);
   
   dset = SUMA_CreateDsetPointer(Opt->prefix, SUMA_NODE_BUCKET,
                                 NULL, SO->idcode_str,
                                 SO->N_Node);
   if (!SUMA_AddDsetNelCol(dset, "node index", SUMA_NODE_INDEX, Nd, NULL, 1)) {
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   } 
   if (!SUMA_AddDsetNelCol(dset, "PairingNode", SUMA_NODE_INT, Pr, NULL, 1)) {
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   } 
   if (!SUMA_AddDsetNelCol(dset, "LayerOfPairNode", SUMA_NODE_INT, Lj, NULL, 1)){
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   } 
   if (!SUMA_AddDsetNelCol(dset, "Eucl.Dist", SUMA_NODE_FLOAT, gD, NULL, 1)) {
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   } 
   if (!SUMA_AddDsetNelCol(dset, "Geo.Dist",SUMA_NODE_FLOAT,  pD, NULL, 1)) {
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   } 
   if (!SUMA_AddDsetNelCol(dset, "G/E.Dist",SUMA_NODE_FLOAT,  rD, NULL, 1)) {
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   } 
   
   etime_GetOffset_all = SUMA_etime(&start_time_all,1);
   SUMA_S_Notev("Done.\nSearch to %f mm took %f minutes for %d nodes.\n" ,
                 Opt->plimit, etime_GetOffset_all / 60.0 , SO->N_Node);
   SUMA_Free_getoffsets(OffS);
   SUMA_free(Nd); Nd = NULL;
   SUMA_free(Pr); Pr = NULL;
   SUMA_free(Lj); Lj = NULL;
   SUMA_free(gD); gD = NULL;
   SUMA_free(pD); pD = NULL;
   SUMA_free(rD); rD = NULL;
   SUMA_RETURN(dset);
}

#if 1
int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SampBias"};
   int N_Spec = -1;
   int i;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfSpecFile *Spec;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   void *SO_name = NULL;
   SUMA_DSET *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_KUBATEST_OPTIONS Opt;
   
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   
   /* Allocate space for DO structure */
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-spec;-sv;-s;-o;");
   
   SUMA_SampBias_ParseInput (argv, argc, &Opt, ps);
   if (argc < 4)
   {
      SUMA_S_Err("Too few arguments");
      exit (0);
   }
   
   
   /* read surface */
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }

   SUMA_LH("Loading surface...");
   SO = SUMA_Load_Spec_Surf_with_Metrics(Spec, 0, ps->sv[0], Opt.debug);
   if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      
   }
   
   
   if (!(dset = calcWithOffsets(SO, &Opt))) {
      SUMA_S_Err("Failed to calc ratios");
      exit(1);
   }
      
   if (!SUMA_AddNgrHist(dset->ngr, "SampBias", argc, argv)) {
      SUMA_S_Err("Failed to add history");
   }

   if (Opt.prefix) {
      char *cc = SUMA_WriteDset_s(Opt.prefix, dset, 
                                  SUMA_NO_DSET_FORMAT, 1,1);
      if (cc) SUMA_free(cc);
   }   
      
   SUMA_LH("clean up");
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }

   if (Opt.outfile) SUMA_free(Opt.outfile);
   if (Opt.histnote) SUMA_free(Opt.histnote);
   if (Opt.prefix) SUMA_free(Opt.prefix);
   if (Opt.segdo) SUMA_free(Opt.segdo);
   
   if (!SUMA_FreeSpecFields(Spec)) {
      SUMA_S_Err("Failed to free SpecFields");
   } SUMA_free(Spec); Spec = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {
      SUMA_SL_Err("SUMAg_CF Cleanup Failed!");}
   
   
   SUMA_RETURN(0);
}
#endif
