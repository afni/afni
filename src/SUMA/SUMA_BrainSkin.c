#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "SUMA_suma.h"
#include "SUMA_gts.h"

#define SURFPATCH_MAX_SURF 1  /*!< Maximum number of input surfaces */



void usage_SUMA_BrainSkin (SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"usage_SUMA_BrainSkin"};
   char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
   s = SUMA_help_basics();
   sio  = SUMA_help_IO_Args(ps);
   printf ( 
"\n"
" A program to create an unfolded surface that wraps the brain (skin) \n"
" and Gyrification Indices.\n"
"\n"
"Usage 1:\n"
"  BrainSkin <-SURF> <-skingrid VOL> <-prefix PREFIX> \n"
"            [<-plimit PLIM>] [<-dlimit DLIM>] [<-segdo SEGDO>]\n"
"\n"
"\n"
"  Mandatory parameters:\n"
"     (-SURF):  An option for specifying the surface to smooth or\n"
"                 the domain over which DSET is defined.\n"
"                 (For option's syntax, see 'Specifying input surfaces'\n"
"                 section below).\n"
"     -skingrid VOL: A high-res volume to provide a grid for voxelization\n"
"                    steps. Typically this is the same volume used with \n"
"                    for the -sv option.\n"
"     -prefix PREFIX: Prefix to use for variety of output files.\n"
"                     Default is 'brainskin' and overwrite is turned on.\n"
"\n"
"  Parameters used when finding node pairs spanning sulci:\n" 
"     -plimit PLIM: maximum length of path along surface in mm.\n"
"                    Node pairing not considered if nodes are more than\n"
"                    PLIM along the surface.\n"
"                    Default is 50.\n"
"     -dlimit DLIM: maximum length of euclidean distance in mm.\n"
"                   Node pairing not considered if nodes have a Euclidian\n"
"                   distance of more than DLIM.\n"
"                   Default is 1000 mm. (no limit)\n"
"     -segdo SEGDO: Output a displayable object file that contains\n"
"                   segments between paired nodes.\n"
"             See 'Ctrl+Alt+s' in SUMA's interactive help\n"
"\n"
" Parameters for voxelization step:\n"
"     -voxelize VOXMETH: Voxelization method VOXMETH. Choose from:\n"
"                        slow: Sure footed but slow.\n"
"                        fast: Faster and works OK.\n"
"                        mask: Fastest and works OK too (default)\n"
"     -infill INFILLMETH: Infill method INFILLMETH. Choose from:\n"
"                        slow: proper infill, but not needed.\n"
"                        fast: brutish infill, all we need. (default)\n"
"\n"
"  Esoteric Options:\n"
"     -out FILE: Output intermediary results from skin forming step\n"
"\n"
"Output:\n"
"  Surfaces:\n"
"     PREFIX.stitch.gii: A bunch of triangles for closing the surface.\n"
"     PREFIX.skin.gii: Initial skin surface\n"
"     PREFIX.skin_simp.gii: Reduced mesh version of initial skin surface.\n"
"     PREFIX.skin.isotopic.gii: Original surface inflated inside skin surface\n" 
"  Datasets:\n"
"     PREFIX.ptchvox+orig: Surface patching voxels.\n"
"     PREFIX.surfvox+orig: Voxels inside original surface\n"
"     PREFIX.skinvox+orig: Mix of ptchvox and surfvox.\n"
"     PREFIX.infilled+orig: skin vox dataset filled in.\n"
"     PREFIX.niml.dset: Results of computations for finding node pairs\n"
"                       that span sulci.\n"
"     PREFIX.areas.niml.dset: Results of computations for inflating \n"
"                             initial surface inside skin surface.\n"
"  Displayable Objects\n"
"     PREFIX.1D.do: Segments between node pairs spanning sulci.\n" 
"Example:\n"
"   BrainSkin -spec std60.DemoSubj_lh.spec \\\n"
"         -surf_A std60.lh.pial.asc \\\n"
"         -sv DemoSubj_SurfVol+orig \\\n"
"         -skingrid DemoSubj_SurfVol+orig. \\\n"
"         -prefix stitched.std60.lh.f \\\n"
"         -segdo stitched.std60.lh.1D.do \\\n"
"         -overwrite \n"
"\n"
"  Usage 2: Use a smooth surface to model outer contours of a mask volume\n"
"\n"
"  BrainSkin <-vol_skin MASK> <-vol_hull MASK> [-prefix  PREFIX]\n" 
"\n"
"     -vol_skin MASK: Deform an Icosahedron to match the outer \n"
"                     boundary of a mask volume.\n"
"     -no_zero_attraction: With vol_skin, the surface will try to shrink\n"
"                          agressively, even if there is no promise of\n"
"                          non-zero values below. Use this option if\n"
"                          you do not want zero values to attract the surface\n"
"                          inwards. This option is only useful with -vol_skin \n"
"                          and it must follow it.\n"
"     -vol_hull MASK: Deform an Icosahedron to match the convex \n"
"                     hull of a mask volume.\n"
"     -vol_skin and -vol_hull are mutually exclusive\n"
"     -node_dbg N: Output debugging information for node N for -vol_skin\n"
"                  and -vol_hull options.\n"
"\n"
"  The program exits after creating the surface.\n"
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
   char *skingrid;
   char *in_name;
   int smoothskin;
   int shrink_mode;
   int voxmeth;
   int infill;
   int node_dbg;
   SUMA_DSET_FORMAT sform;
   SUMA_GENERIC_ARGV_PARSE *ps;
} SUMA_BRAIN_SKIN_OPTIONS;


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
SUMA_BRAIN_SKIN_OPTIONS *SUMA_BrainSkin_ParseInput(
      char *argv[], int argc, SUMA_BRAIN_SKIN_OPTIONS* Opt,
      SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_BrainSkin_ParseInput"}; 
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
   Opt->skingrid = NULL;
   Opt->sform = SUMA_NO_DSET_FORMAT;
   Opt->voxmeth = 0;
   Opt->in_name = NULL;
   Opt->shrink_mode = 2; /* agressive */
   Opt->smoothskin = 1;
   Opt->infill = 2;
   Opt->node_dbg=-1;
   Opt->ps=ps;
   while (kar < argc) 
   { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0)
      {
         ps->hverb = strlen(argv[kar])>3?2:1;
         usage_SUMA_BrainSkin(ps);
         exit (0);
      }

      SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      if (!brk && (strcmp(argv[kar], "-verb") == 0)) {
         ++Opt->debug;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-plimit") == 0)) {
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
     
      if (!brk && (strcmp(argv[kar], "-voxelize") == 0)) 
      {
         kar++;
         if (kar >= argc)  
         {
            fprintf (SUMA_STDERR, "need argument after -voxelize \n");
            exit (1);
         }
         if (!strcasecmp(argv[kar],"fast")) Opt->voxmeth = 1;
         else if (!strcasecmp(argv[kar],"slow")) Opt->voxmeth = 2;
         else if (!strcasecmp(argv[kar],"mask")) Opt->voxmeth = 0;
         else {
            SUMA_S_Errv("Bad value %s for -voxelize\n", argv[kar]);
         }
         brk = YUP;
      }
     
      if (!brk && (strcmp(argv[kar], "-infill") == 0)) 
      {
         kar++;
         if (kar >= argc)  
         {
            fprintf (SUMA_STDERR, "need argument after -infill \n");
            exit (1);
         }
         if (!strcasecmp(argv[kar],"fast")) Opt->infill = 2;
         else if (!strcasecmp(argv[kar],"slow")) Opt->infill = 1;
         else {
            SUMA_S_Errv("Bad value %s for -infill\n", argv[kar]);
         }
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
         Opt->segdo = SUMA_copy_string(argv[kar]);
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
         SUMA_DSET_NAME_CHECK(argv[kar]); 
         Opt->prefix = SUMA_RemoveDsetExtension_eng (argv[kar], &Opt->sform);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-skingrid") == 0)) 
      {
         kar++;
         if (kar >= argc)  
         {
            fprintf (SUMA_STDERR, "need dset after -skingrid \n");
            exit (1);
         }
         Opt->skingrid = SUMA_copy_string(argv[kar]);
         brk = YUP;
      }
     
      if (!brk && (strcmp(argv[kar], "-vol_skin") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -vol_skin \n");
				exit (1);
			}
			Opt->shrink_mode = 2;
         Opt->in_name = SUMA_copy_string(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-no_zero_attraction") == 0)) {
			if (!Opt->shrink_mode)  {
		  		fprintf (SUMA_STDERR, "Use -vol_skin before -no_zero_attraction \n");
				exit (1);
			}
			Opt->shrink_mode = 1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-node_dbg") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need integer after -node_dbg \n");
				exit (1);
			}
			Opt->node_dbg = atoi(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-vol_hull") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -vol_hull \n");
				exit (1);
			}
         Opt->shrink_mode = 0;
			Opt->in_name = SUMA_copy_string(argv[kar]);
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

   if (!Opt->prefix) {
      Opt->prefix = SUMA_copy_string("brainskin");
      AFNI_setenv("AFNI_DECONFLICT=OVERWRITE") ; 
      THD_set_quiet_overwrite(1);
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

   SUMA_setBrainWrap_NodeDbg(Opt->node_dbg);

   Opt->histnote = SUMA_HistString (NULL, argc, argv, NULL);
   
   SUMA_RETURN (Opt);
}

SUMA_DSET *calcWithOffsets(SUMA_SurfaceObject *SO, SUMA_BRAIN_SKIN_OPTIONS* Opt)
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

SUMA_DSET *SUMA_calcWithOffsets_Skin( SUMA_SurfaceObject *SO, 
                                 SUMA_BRAIN_SKIN_OPTIONS* Opt)
{
   static char FuncName[]={"SUMA_calcWithOffsets_Skin"};
   /* initialize OffS */
   SUMA_GET_OFFSET_STRUCT *OffS = SUMA_Initialize_getoffsets (SO->N_Node);
   struct timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all;
   float *gD=NULL, *pD=NULL, *rD=NULL, *Cx=NULL, *nrmA=NULL, *nrmB=NULL;
   int *Pr=NULL, *Nd=NULL, *Lj=NULL, *nT=NULL;
   float pathD = 0;/*shortest distance along surface to node ii*/
   float geomD = 0;/*geometric distance to node ii*/
   float ratio = 1;
   float Cxlim=0.5;
   int i = 0,  j = 0, ii = 0, lj =0,N_hits, nhits=0;
   float x1, x2 , y1, y2, z1, z2, dx, dy, dz, d1, d2, r;
   double dotA, dotB;
   FILE* outFile = NULL, *segDO=NULL;
   byte *nmask=NULL;
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
   
   if (!(Cx = (float *)SUMA_GetCx(SO->idcode_str, SUMAg_CF->DsetList, 0))) {
      if (!SUMA_SurfaceMetrics_eng (SO, "Convexity", NULL, 
                                    0, SUMAg_CF->DsetList)) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      Cx = (float *)SUMA_GetCx(SO->idcode_str, SUMAg_CF->DsetList, 0);
   }
   if (!Cx) {
      SUMA_S_Err("Need convexity, can't get at it.\n");
      SUMA_RETURN (NULL);
   } 

    
   Nd=(int *)SUMA_calloc(SO->N_Node, sizeof(int));   
   Pr=(int *)SUMA_calloc(SO->N_Node, sizeof(int)); 
   Lj=(int *)SUMA_calloc(SO->N_Node, sizeof(int));   
   nT=(int *)SUMA_calloc(SO->N_Node, sizeof(int));   
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
      x1 = SO->NodeList[i*3+0];
      y1 = SO->NodeList[i*3+1];
      z1 = SO->NodeList[i*3+2];
      nrmA = SO->NodeNormList+i*3;
      
      SUMA_LHv("Calculating offsets from node %d\n", i);
      if (i == 0) {
         SUMA_etime(&start_time,0);
      }
      if (i == 99 || i == 500) {
         etime_GetOffset = SUMA_etime(&start_time,1);
         SUMA_S_Notev("Search to %f mm took %f seconds for %d nodes.\n"
                  "Projected completion time: %f minutes\n",
                  Opt->plimit, etime_GetOffset, i+1,
                  etime_GetOffset * SO->N_Node / 60.0 / (i+1));
      }
      /* initialize output */
      Nd[i] = i;
      pathD = 0; pD[i] = pathD;/*shortest distance along surface to node ii*/
      geomD = 0; gD[i] = geomD;/*geometric distance to node ii*/
      ratio = 1; rD[i] = ratio;
      ii = -1; Pr[i] = ii;
      lj = -1; Lj[i] = lj; 
      nhits = -1; nT[i] = nhits;   
      
      if (Cx[i]<Cxlim) continue;
      
      SUMA_getoffsets2 (i, SO, Opt->plimit, OffS, NULL, 0);
      /* restrict candidate nodes to those in offset 
         Might want to change restriction to those in box centered
         on i and of size related to max search lim */ 
      nmask = SUMA_GetOffset2bytemask (OffS, nmask);

      /*find smallest ratio*/
      for (j=0; j < OffS->N_Nodes; j++)
      {
         if( i!=j && 
             OffS->LayerVect[j] >= 1 && /* no need for nearest neighbs */
             Cx[j]>=Cxlim /* only positive convexity need apply */)
         {
            x2 = SO->NodeList[j*3+0];
            y2 = SO->NodeList[j*3+1];
            z2 = SO->NodeList[j*3+2];
            dx = x2 - x1;
            dy = y2 - y1;
            dz = z2 - z1;
            d1 = OffS->OffVect[j];
            d2 = sqrt(dx*dx + dy*dy + dz*dz);
            r = d1 / d2;
            /* How many triangles to you intersect sir? */
            if (  d2 < Opt->dlimit && /* meets Euclidian distance limits*/
                  d1 < Opt->plimit && /* meets geodesic distance limits*/
                  r > ratio  /* better than the last */)
            {
               /* You don't want a junction inside the surface, 
                  even if the segment won't intersect the surface as 
                  will be tested ahead. This means normal at A should be
                  in direction of AB and normal at B should be in direction '
                  of BA*/
               dx = dx/d2; dy=dy/d2; dz=dz/d2;
               dotA =  nrmA[0]*dx +nrmA[1]*dy +nrmA[2]*dz;
               nrmB = SO->NodeNormList+j*3;
               dotB = -nrmB[0]*dx -nrmB[1]*dy -nrmB[2]*dz;
               if (0 && i==1018) {
                  SUMA_S_Notev("Nodes %d %d have dotA %f, dotB %f\n", 
                                 i, j, dotA, dotB);
               }
               if (dotA < -0.20 || dotB < -0.20) {  /* don't go < 0, normal 
                                                   estimates can be choppy 
                            You are not guaranteed to avoid them
                            all, particularly on low-res surfaces 
                            with pointy surfaces. Instead you should integrate
                            along the shortes path and be sure that you go
                            through negative convexity zones. 
                            Maybe later...*/
                  /* get out, we're going inside surface */
                  continue;
               }
               /* one more condition, segment should not intersect surface 
                  That is a costly one, don't put up higher */
               N_hits = 1; /* Setting limit on number of triangles pierced
                              between two nodes. This avoids waisting time 
                              computing numerous intersections in next
                              call.  */
               SUMA_MT_count_intersect_triangle(
                              (void *)(long)i, (void *)(long)j, 
                              SO->NodeList, SO->N_Node, 
                              SO->FaceSetList, SO->N_FaceSet, 
                              &N_hits, NULL, 1, nmask,
                              1, NULL, NULL, NULL);
               if (i==1018 && j==932) {
                  SUMA_S_Notev("Nodes %d %d caused %d hits\n", i, j, N_hits);
               }
               if (N_hits) {
                  /* segment between i and j intersects surface, don't take it*/
                  continue;
               }
               
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
               nhits = N_hits;
            }
         }
      }
      
      Nd[i] = i;
      Pr[i] = ii;
      Lj[i] = lj; 
      gD[i] = geomD;
      pD[i] = pathD;
      rD[i] = ratio;
      nT[i] = nhits;
      if (ii>0) {
         if (outFile)
            fprintf(outFile, "%i\t%i\t%f\t%f\t%f\t%d\t%d\n", 
                        i, ii, geomD, pathD, ratio, lj, nhits);
         if (segDO) {
            float r,g,b,a;
            SUMA_RAND_COL(i?-1:0,r,g,b,a);
            fprintf(segDO, "%i\t%i\t%f %f %f %f\n", i, ii, r,g,b,1.0);
         }
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
   if (!SUMA_AddDsetNelCol(dset, "LayerOfPairNode", SUMA_NODE_INT, Lj, NULL, 1)){
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   } 
   if (!SUMA_AddDsetNelCol(dset, "TrianglesHit", SUMA_NODE_INT, nT, NULL, 1)){
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
   SUMA_free(nT); nT = NULL;
   SUMA_free(gD); gD = NULL;
   SUMA_free(pD); pD = NULL;
   SUMA_free(rD); rD = NULL;
   SUMA_RETURN(dset);
}

SUMA_DSET *SUMA_whatdoicallthis( SUMA_SurfaceObject *SO, 
                                 SUMA_BRAIN_SKIN_OPTIONS* Opt)
{
   static char FuncName[]={"SUMA_whatdoicallthis"};
   /* initialize OffS */
   struct timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all;
   float *gD=NULL, *pD=NULL, *dp=NULL, *Cx=NULL;
   int *Pr=NULL, *Nd=NULL, *Pr2=NULL;
   int i = 0,  j = 0;
   double  dptmp, dx, dy, dz, dxyz2;
   float *x1, *x2, *n1, *n2, box[3]={20.0, 20.0, 20.0};
   float dotlim = -0.25, convlim = 0.5; 
   SUMA_ISINBOX SIIB;
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
"#Col. 0 Node index of node n \n"
"#Col. 1 Node m which is closest to n and with an opposite pointing normal\n"
"#Col. 2 distance in 3D between n & m\n"
"#Col. 3 shortest surface path between n & m\n"
"#Col. 4 dot product of normals\n");
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
   
   if (!(Cx = (float *)SUMA_GetCx(SO->idcode_str, SUMAg_CF->DsetList, 0))) {
      if (!SUMA_SurfaceMetrics_eng (SO, "Convexity", NULL, 
                                    0, SUMAg_CF->DsetList)) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      Cx = (float *)SUMA_GetCx(SO->idcode_str, SUMAg_CF->DsetList, 0);
   }
   if (!Cx) {
      SUMA_S_Err("Need convexity, can't get at it.\n");
      SUMA_RETURN (NULL);
   } 
   Nd=(int *)SUMA_calloc(SO->N_Node, sizeof(int));   
   Pr=(int *)SUMA_calloc(SO->N_Node, sizeof(int));   
   Pr2=(int *)SUMA_calloc(SO->N_Node, sizeof(int));   
   gD=(float *)SUMA_calloc(SO->N_Node, sizeof(float));   
   pD=(float *)SUMA_calloc(SO->N_Node, sizeof(float));   
   dp=(float *)SUMA_calloc(SO->N_Node, sizeof(float)); 
   if (!Pr || !gD || !pD || !dp) {
      SUMA_S_Err("Failed to allocate");
      SUMA_RETURN(NOPE);
   }  
   SUMA_etime(&start_time_all,0);
   for (i=0; i < SO->N_Node; ++i)
   {
      x1 = SO->NodeList+3*i;
      n1 = SO->NodeNormList+3*i;
      
      if (Cx[i] < convlim) continue; /* No need to process convex regions */
      /* find closest nodes within sphere */
      SIIB = SUMA_isinbox (SO->NodeList, SO->N_Node, 
                           x1 , box, 1 );
      Nd[i] = i;
      Pr[i] = -1;
      Pr2[i] = -1;
      dp[i] = 1.0; 
      /* find the closest node with negative normal direction */
      for (j=0; j < SIIB.nIsIn; ++j) {
         x2 = SO->NodeList+3*SIIB.IsIn[j];
         n2 = SO->NodeNormList+3*SIIB.IsIn[j];
         dptmp = n1[0]*n2[0]+n1[1]*n2[1]+n1[2]*n2[2];
         #if 1 /* pair based on distance, with direction constraint*/
         if (dptmp < dotlim && Cx[j]>=convlim) { /* potential candidate */
            if (SIIB.d[j] < gD[i] || Pr[i]<0) {
               gD[i] = SIIB.d[j];  
               /*Pr2[i] = Pr[i];  store last best */
               Pr[i] = SIIB.IsIn[j];
               dp[i] = dptmp;
               pD[i] = 0.0; /* eventually get shortest distance,
                               Dijkstra, most likely */
            }
         }
         #else /* Pair with best direction constraint */
         if (dptmp < dp[i] && dptmp < dotlim) {
               gD[i] = SIIB.d[j];  
               /*Pr2[i] = Pr[i];  store last best */
               Pr[i] = SIIB.IsIn[j];
               dp[i] = dptmp;
               pD[i] = 0.0; /* eventually get shortest distance,
                               Dijkstra, most likely, but only after
                               you sort out the best pairing */
         }
         #endif
      }
      SUMA_Free_IsInBox(&SIIB);
      if (Pr[i] < 0) { /* nothing worthy */
         Pr[i]=0; dp[i] = 0.0; gD[i] = 0.0; pD[i] = 0.0;
      } else {
         if (segDO) {
            float r,g,b,a;
            SUMA_RAND_COL(i?-1:0,r,g,b,a);
            fprintf(segDO, "%i\t%i\t%f %f %f %f\n", Nd[i], Pr[i], r,g,b,1.0);
            if (Pr2[i]>=0){
               SUMA_RAND_COL(i?-1:0,r,g,b,a);
               fprintf(segDO, "%i\t%i\t%f %f %f %f\n", Nd[i], Pr2[i], r,g,b,1.0);
            }
         }         
      }
      if (outFile)
         fprintf(outFile, "%i\t%i\t%f\t%f\t%f\t%d\n", 
                     Nd[i], Pr[i], gD[i], pD[i], dp[i], Pr2[i]);
      
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
   if (!SUMA_AddDsetNelCol(dset, "Eucl.Dist", SUMA_NODE_FLOAT, gD, NULL, 1)) {
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   } 
   if (!SUMA_AddDsetNelCol(dset, "Geo.Dist",SUMA_NODE_FLOAT,  pD, NULL, 1)) {
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   } 
   if (!SUMA_AddDsetNelCol(dset, "DotProd_Norms",SUMA_NODE_FLOAT, dp, NULL, 1)) {
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   }
   if (!SUMA_AddDsetNelCol(dset, "SecondPairing", SUMA_NODE_INT, Pr2, NULL, 1)) {
      SUMA_S_Err("Failed to add new data");
      SUMA_FreeDset(dset); SUMA_RETURN(NULL);
   }  
   
   etime_GetOffset_all = SUMA_etime(&start_time_all,1);
   SUMA_S_Notev("Done.\nSearch to %f mm took %f minutes for %d nodes.\n" ,
                 Opt->plimit, etime_GetOffset_all / 60.0 , SO->N_Node);

   SUMA_free(Nd); Nd = NULL;
   SUMA_free(Pr); Pr = NULL;
   SUMA_free(Pr2); Pr2 = NULL;
   SUMA_free(dp); dp = NULL;
   SUMA_free(gD); gD = NULL;
   SUMA_free(pD); pD = NULL;
   
   SUMA_RETURN(dset);
}

/*! 
   i1 must be a sorted monotonic series of node numbers.
   i2 node index paired with i1
   
   pairmet: 1: Pair segments base on minimal angle between segments
            2: Pair segments to minimize angle between triangle and 
               radial direction
*/
SUMA_SurfaceObject *SUMA_triangulate_segments(SUMA_SurfaceObject *SO, 
                           int *i1, int *i2, int n_i, 
                           int pairmet) 
{
   static char FuncName[]={"SUMA_triangulate_segments"};
   int *tr=NULL, *cand=NULL, N_tri=0, *pairing=NULL;
   int itr=0, itr_alloc=0, n3=-1, n4=-1, i, k, nnn;
   float fff=0.0, *dotp=NULL, *dirs=NULL, angmin=0.0, 
         *p1=NULL, *p2=NULL, *p3=NULL, pavg[3], 
         dp1p2[3], trinorm[3], *ang=NULL, *nl=NULL;
   SUMA_SurfaceObject *SOo=NULL;
   SUMA_NEW_SO_OPT *nsoopt=NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->FN || !i1 || !i2) {
      SUMA_S_Errv("Null or incomplete input %p %p %p %p\n",
                  SO, SO->FN, i1, i2);
      SUMA_RETURN(NULL);
   }
   dirs = (float *)SUMA_calloc(n_i*3,sizeof(float));
   pairing = (int *)SUMA_calloc(SO->N_Node, sizeof(int)); 
            memset(pairing, -1, sizeof(int)*SO->N_Node);
   cand = (int *)SUMA_calloc(SO->FN->N_Neighb_max, sizeof(int));
   ang = (float *)SUMA_calloc(SO->FN->N_Neighb_max, sizeof(float));
   dotp = (float *)SUMA_calloc(SO->FN->N_Neighb_max, sizeof(float));
   for (i=0; i<n_i; ++i) {
      if (i2[i] >= 0) {
         p1 = &(SO->NodeList[3*i1[i]]);
         p2 = &(SO->NodeList[3*i2[i]]);
         SUMA_UNIT_VEC(p1, p2, dp1p2,fff);
         dirs[3*i  ] = dp1p2[0]; dirs[3*i+1] = dp1p2[1]; dirs[3*i+2] = dp1p2[2]; 
      }
   }
   
   for (i=0; i<n_i; ++i) {
      if (SO->FN->NodeId[i1[i]] != i1[i]) {
         SUMA_S_Err("Not expecting incomplete list here..");
         SUMA_RETURN(NULL);
      }
      if (i2[i] < 0) continue; 
      if (i1[i] == i2[i]) {
         /* initialized to nothingness ...*/
         SUMA_S_Errv("Identical node pairs: %d %d\n", i1[i], i2[i]);
         continue; 
      }
      
      n3 = -1; n4 = -1;
      angmin = 2.0;
      switch (pairmet) { 
         case 1: /* Pair segments that have smallest angle between them */
            for (k=0; k<SO->FN->N_Neighb[i1[i]]; ++k) {
               cand[k] = SO->FN->FirstNeighb[i1[i]][k];
               if ((nnn = SUMA_FindFirst_inIntVect(i1,i1+n_i,cand[k]))>=0) {
                  /* what is the angle between these two segments? */
                  ang[k] =(dirs[3*nnn  ]*dirs[3*i  ] +
                           dirs[3*nnn+1]*dirs[3*i+1] +
                           dirs[3*nnn+2]*dirs[3*i+2] ); 
                  if ( n3 < 0 || pairing[cand[k]] == i1[i] || ang[k]<angmin) {
                     n3 = cand[k]; 
                     if (i2[nnn] != i2[i]) n4 = i2[nnn];
                     else n4 = -1;
                     angmin = ang[k];
                  }
               } 
            }
            break;
         case 2: /* Pair a segment with a node of another segment such that
                    the angle is minimal with the radial direction 
                    THIS IS NOT GOOD for non-convex surfaces */
            for (k=0; k<SO->FN->N_Neighb[i1[i]]; ++k) {
               cand[k] = SO->FN->FirstNeighb[i1[i]][k];
               if ((nnn = SUMA_FindFirst_inIntVect(i1,i1+n_i,cand[k]))>=0) {
                  /* Normal of triangle i1[i], i2[i], cand[k] */
                  p1 = &(SO->NodeList[3*i1[i]]);
                  p2 = &(SO->NodeList[3*i2[i]]);
                  p3 = &(SO->NodeList[3*cand[k]]);
                  SUMA_TRI_NORM_NORM(p1,p2,p3,trinorm);
                  /* Direction from triangle center to center */
                  pavg[0] = (p1[0]+p2[0]+p3[0])/3.0;
                  pavg[1] = (p1[1]+p2[1]+p3[1])/3.0;
                  pavg[2] = (p1[2]+p2[2]+p3[2])/3.0;
                  SUMA_UNIT_VEC(pavg, SO->Center, dp1p2,fff);
                  /* what is the angle between triangle and radial direction? */
                  ang[k] =(dp1p2[0]*trinorm[0] +
                           dp1p2[1]*trinorm[1] +
                           dp1p2[2]*trinorm[2] ); 
                  if ( n3 < 0 || pairing[cand[k]] == i1[i] || ang[k]<angmin) {
                     n3 = cand[k]; 
                     if (i2[nnn] != i2[i]) n4 = i2[nnn];
                     else n4 = -1;
                     angmin = ang[k];
                  }
               }
            }
            break;
         default:
            SUMA_S_Err("Bad pairmet");
            SUMA_RETURN(NULL);
      }
      if (n3 < 0) {
         SUMA_LH("No triangle possible");
         continue;          
      } else {
         if (itr+1 >= itr_alloc) {
            itr_alloc += 2*(n_i+1);
            tr = (int *)SUMA_realloc(tr, itr_alloc*3*sizeof(int));
         }
         SUMA_LHv("Nodepair %d %d, with %d %d , trs (%d, %d)\n", 
                     i1[i], i2[i], n3, n4, itr, itr+1);
         if (i1[i] != i2[i] && i1[i] != n3 && i2[i] != n3) {
            pairing[n3] = i1[i];
            tr[3*itr  ] = i1[i]; tr[3*itr+1] = i2[i];  tr[3*itr+2] = n3; 
            ++itr;
         } else {
            fprintf(stderr,"Should not have happened!\n");
         }
         if (n4 >= 0) {
            if (n3 != i2[i] && n4 != n3 && i2[i] != n4) {
               tr[3*itr  ] = n3;    tr[3*itr+1] = n4;     tr[3*itr+2] = i2[i];
               ++itr;
            } else {
               fprintf(stderr,"Should not have happened either!\n");
            }
         }
      }
   }
   
   
   nl = (float *)SUMA_calloc(SO->N_Node*3, sizeof(float));
   tr = (int *)SUMA_realloc(tr, itr*3*sizeof(int));
   memcpy(nl, SO->NodeList,SO->N_Node*3*sizeof(float));
   nsoopt = SUMA_NewNewSOOpt();
   nsoopt->DoMetrics = NOPE;
   
   SOo = SUMA_NewSO(&nl, SO->N_Node, &tr, itr, nsoopt);
   nsoopt=SUMA_FreeNewSOOpt(nsoopt);
   
   /* flip orientations so that all triangles point outwards */
   if (!SUMA_OrientSOTriangles(SOo, 1, 1, SO->Center)) {
      SUMA_S_Err("Failed to force orientation, proceeding anyway.");
   }
   
   if (pairing) SUMA_free(pairing); pairing=NULL;
   if (cand) SUMA_free(cand); cand=NULL;
   if (dotp) SUMA_free(dotp); dotp=NULL;
   if (dirs) SUMA_free(dirs); dirs=NULL;
   if (ang) SUMA_free(ang); ang=NULL;
   
   SUMA_RETURN(SOo);
}

SUMA_Boolean SUMA_ProjectSO_Nodes(SUMA_SurfaceObject *SO, 
                     SUMA_SurfaceObject *SOe, 
                     int max_hits, float **projp, float **distp) 
{
   static char FuncName[]={"SUMA_ProjectSO_Nodes"};
   float *p1, d[3], dn, *dist=NULL, *proj=NULL;
   int ii, ii3, iface_min_dist=-1, N_hits = 0;
   
   SUMA_ENTRY;
   
   if (!SO || !SOe || !projp || !distp) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   if (*projp || *distp) {
      SUMA_S_Err("not NULL values for *projp || *distp");
      SUMA_RETURN(NOPE);
   }
   dist = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
      *distp = dist;
   proj = (float *)SUMA_calloc(3*SO->N_Node, sizeof(float));
      *projp = proj;
   for (ii=0; ii<SO->N_Node; ++ii) {
      ii3 = 3*ii;
      p1 = SO->NodeList+ii3;
      /* projection dir */
      SUMA_UNIT_VEC(SO->Center, p1, d, dn);
      N_hits = max_hits;
      if (!SUMA_MT_count_intersect_triangle(p1, d, 
                           SOe->NodeList, SOe->N_Node, 
                           SOe->FaceSetList, SOe->N_FaceSet, 
                           &N_hits, NULL,
                           0, NULL,
                           2,
                           dist+ii, &iface_min_dist, proj+ii3)) {
         SUMA_S_Err("Bummer");
         SUMA_RETURN(NOPE);                     
      }
      if (ii==501) {
         SUMA_S_Notev("Node %d on %s, X [%f %f %f], Proj. Dir [%f %f %f]\n"
                      "Hits tri %d on %s, Proj. Loc. [%f %f %f], dist %f\n"
                      , ii, SUMA_CHECK_NULL_STR(SO->Label)
                              , p1[0], p1[1], p1[2], d[0], d[1], d[2],
                      iface_min_dist, SUMA_CHECK_NULL_STR(SOe->Label), 
                           proj[ii3], proj[ii3+1], proj[ii3+2], dist[ii]);   
      }
   }
   
   SUMA_RETURN(YUP);   
}

#if 1
int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"BrainSkin"};
   int N_Spec = -1;
   int i;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfaceObject *SOfs = NULL;
   SUMA_SurfSpecFile *Spec;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   void *SO_name = NULL;
   SUMA_DSET *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_BRAIN_SKIN_OPTIONS Opt;
   
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   
   /* Allocate space for DO structure */
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-spec;-sv;-s;-o;-talk;");
   
   SUMA_BrainSkin_ParseInput (argv, argc, &Opt, ps);
   
   /* see if SUMA talk is turned on */
   if (ps->cs->talk_suma) {
      ps->cs->istream = SUMA_BRAINWRAP_LINE;
      ps->cs->kth = 1; /* make sure all surfaces get sent */
      if (!SUMA_SendToSuma (NULL, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         ps->cs->Send = NOPE;
         ps->cs->afni_Send = NOPE;
         ps->cs->talk_suma = NOPE;
      }
   }
   
   if (Opt.in_name) {
      SUMA_SurfaceObject *SO=NULL;
      THD_3dim_dataset *dset=NULL;
      char *ppo=NULL;
      if (!(dset = THD_open_dataset( Opt.in_name ))) {
         SUMA_S_Err("Failed to read %s", Opt.in_name );
         exit(1);
      }
      DSET_load(dset);
      if (!(SO = SUMA_Mask_Skin(dset, 0, Opt.smoothskin, 
                                Opt.shrink_mode, ps->cs))){
         SUMA_S_Err("Failed to create mask");
         exit(1);
      }
      ppo = SUMA_ModifyName (Opt.prefix, "append", ".volskin", NULL);
      SUMA_Save_Surface_Object_Wrap(ppo, NULL, SO, 
                                    SUMA_GIFTI, SUMA_ASCII, NULL);
      SUMA_free(ppo); ppo=NULL;
      SUMA_Free_Surface_Object(SO); SO = NULL; 
      DSET_delete(dset); dset=NULL;
      SUMA_ifree(Opt.in_name);
      exit(0);
	}

   
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
   
   
   
   if (!(dset = SUMA_calcWithOffsets_Skin( SO, &Opt))) {
      SUMA_S_Err("Failed to calc sulcal stitches");
      exit(1);
   }
   #if 0 
         /* another method - normals based, weak, not enough  */
         if (!(dset = SUMA_whatdoicallthis( SO, &Opt))) {
            SUMA_S_Err("Failed to calc sulcal stitches");
            exit(1);
         }
         /* --------------------------------------------------------- */
   
         /* the first method, what BrainSkin is all about 
            This should be turned back on because BrainSkin is
            being usurped */
         if (!(dset = calcWithOffsets(SO, &Opt))) {
            SUMA_S_Err("Failed to calc ratios");
            exit(1);
         }
         /* --------------------------------------------------------- */
   #endif
   
   if (!SUMA_AddNgrHist(dset->ngr, "BrainSkin", argc, argv)) {
      SUMA_S_Err("Failed to add history");
   }

   if (Opt.prefix) {
      char *cc = SUMA_WriteDset_s(Opt.prefix, dset, 
                                  Opt.sform, 1,1);
      if (cc) SUMA_free(cc);
   }   
   
   if (Opt.skingrid) {
      SUMA_SurfaceObject *SOo=NULL, *SOf=NULL, *SOinf=NULL, *SOinfA=NULL;
      float tot_init, tot_infl, unit_init, unit_infl;
      int ii;
      byte *bb=NULL;
      float *fv1=NULL, *fv2=NULL, *areas=NULL, *areasA=NULL, 
            *dist=NULL, *proj=NULL, *distw=NULL;
      char *ppo=NULL, *cc=NULL;
      THD_3dim_dataset *gdset=NULL, *odset=NULL, *odseti=NULL, *odsetvs=NULL;
      SUMA_DSET *adset=NULL;
      
      if (!Opt.prefix) Opt.prefix = SUMA_copy_string("luxskin");
      
      SOo = SUMA_triangulate_segments(SO, SDSET_NODE_INDEX_COL(dset),
                             (int *)SDSET_COL(dset,0), SDSET_VECLEN(dset),1);
      ppo = SUMA_ModifyName (Opt.prefix, "append", ".stitch", NULL);
      SUMA_Save_Surface_Object_Wrap(ppo, NULL, SOo, 
                                    SUMA_GIFTI, SUMA_ASCII, NULL);
      SUMA_free(ppo); ppo=NULL;
      
      SUMA_S_Note("Now getting intersection of stitching with grid");
      gdset = THD_open_dataset(Opt.skingrid);
      odset = SUMA_SurfaceIntersectionVolume(SOo, gdset);
      ppo = SUMA_ModifyName (Opt.prefix, "append",".ptchvox", NULL);
      EDIT_dset_items( odset , ADN_prefix,  ppo,  ADN_none ) ;
      DSET_write(odset); SUMA_free(ppo); ppo=NULL;
      
      /* Now voxelize the original surface */
      switch (Opt.voxmeth) {
         case 2:
            SUMA_S_Note("Now voxelizing initial surface, slow method");
            odseti = SUMA_VoxelizeSurface(SO, gdset, 2, NULL);
            break;
         case 1:
            SUMA_S_Note("Now voxelizing initial surface, fast method");
            odseti = SUMA_VoxelizeSurface(SO, gdset, 1, NULL);
            break;
         case 0:
            SUMA_S_Note("Now voxelizing initial surface, mask method");
            odseti = SUMA_MaskizeSurface(SO, gdset, 1);
            break;
         default:
            SUMA_S_Errv("voxmeth %d not allowed\n", Opt.voxmeth);
            exit(1);
      }
      ppo = SUMA_ModifyName (Opt.prefix, "append",".surfvox", NULL);
      EDIT_dset_items( odseti , ADN_prefix,  ppo,  ADN_none ) ;
      DSET_write(odseti); SUMA_free(ppo); ppo=NULL; 
      
      /* Now combine the two */
      fv1 = THD_extract_to_float(0, odset);
      fv2 = THD_extract_to_float(0, odseti);
      switch (Opt.voxmeth) {
         case 2:
            for (ii=0; ii<DSET_NVOX(odset); ++ii) {
               if (fv2[ii]<0.0) {
                  if (fv1[ii]) fv2[ii] = 1.0;
                  else fv2[ii] = 0.0;
               }
            }   
            break;
         case 1:
            for (ii=0; ii<DSET_NVOX(odset); ++ii) {
               if (fv2[ii]< 0.0) {
                  if (fv1[ii]) fv2[ii] = 1.0;
                  else fv2[ii] = 0.0;
               }
            }  
            break;
         case 0:
            for (ii=0; ii<DSET_NVOX(odset); ++ii) {
               if (fv2[ii]< 4.0) {
                  if (fv1[ii]) fv2[ii] = 4.0;
                  else fv2[ii] = 0.0;
               }
            }  
            break;
         default:
            SUMA_S_Errv("voxmeth %d not allowed\n", Opt.voxmeth);
            exit(1);
      }
      
      EDIT_substscale_brick(odseti, 0, MRI_float, 
                            fv2, MRI_short, -1.0);
      ppo = SUMA_ModifyName (Opt.prefix, "append",".skinvox",NULL);
      EDIT_dset_items(  odseti , ADN_prefix,  ppo,  ADN_none ) ;
      EDIT_BRICK_LABEL (odseti, 0, "patched");      
      DSET_write(odseti); SUMA_free(ppo); ppo=NULL;
      free(fv1); fv1 = NULL; free(fv2); fv2 = NULL;
      
      /* Now infill */
      SUMA_S_Note("Now infilling");
      DSET_delete(odset); odset = NULL;
      if (!SUMA_VolumeInFill(odseti, &odset, Opt.infill, -1, -1, 
                                    Opt.infill == 2?1:-1,0,0,0.0, NULL)) {
         SUMA_S_Err("Failed to infill");
         exit(1);
      }
      ppo = SUMA_ModifyName (Opt.prefix, "append",".infilled",NULL);
      EDIT_dset_items(  odset , ADN_prefix,  ppo,  ADN_none ) ;
      EDIT_BRICK_LABEL (odset, 0, "infilled");      
      DSET_write(odset); SUMA_free(ppo); ppo=NULL;
      
      /* And now you construct a new surface */
      SUMA_S_Note("Isosurface extraction and smoothing");
      SOf = SUMA_THD_IsoSurface(odset, 1.0, 0.0, 1, Opt.debug);
      /* Now some smoothin, a la Taubin */
      if (!SUMA_Taubin_Smooth_SO( SOf, SUMA_EQUAL, 0.1, NULL, 0, 100)) {
         SUMA_S_Err("Failed to smooth surface");
      }                  
      ppo = SUMA_ModifyName (Opt.prefix, "append", ".skin", NULL);
      SUMA_Save_Surface_Object_Wrap(ppo, NULL, SOf, 
                                    SUMA_GIFTI, SUMA_ASCII, NULL);
      SUMA_free(ppo); ppo=NULL;
      
      /* simplify that surface */
      SUMA_S_Notev("Mesh simplification to %d nodes\n",
                   17000);
      SOfs = SUMA_Mesh_Resample_nodes (SOf, 17000); 
      /* flip orientations so that all triangles point outwards */
      if (!SUMA_OrientSOTriangles(SOfs, 1, 0, SOfs->Center)) {
         SUMA_S_Err("Failed to force orientation 2, proceeding anyway.");
      }
      ppo = SUMA_ModifyName (Opt.prefix, "append", ".skin_simp", NULL);
      SUMA_Save_Surface_Object_Wrap(ppo, NULL, SOfs, 
                                    SUMA_GIFTI, SUMA_ASCII, NULL);
      SUMA_free(ppo); ppo=NULL;
      
      /* project pial surface nodes radially onto skin surface, to get a 
         rough idea of how far each node goes and where it is to endup */
      SUMA_S_Note("Projection time");
      proj = NULL; dist = NULL;
      if (!SUMA_ProjectSO_Nodes(SO, SOfs, 2, &proj, &dist)) {
         SUMA_S_Err("Failed projecting nodes.\n");
         exit(1);
      }
      
      /* voxelize simplified skin surface */
      SUMA_S_Note("Voxelizing skin");
      bb = THD_makemask( odset , 0 , 1.0,-1.0 ) ;
      odsetvs = SUMA_VoxelizeSurface(SOfs, gdset, 1, bb);
      free(bb); bb = NULL;
      ppo = SUMA_ModifyName (Opt.prefix, "append",".skinvoxelized",NULL);
      EDIT_dset_items(  odsetvs , ADN_prefix,  ppo,  ADN_none ) ;
      EDIT_BRICK_LABEL (odsetvs, 0, "voxelizedskin");      
      DSET_write(odsetvs); SUMA_free(ppo); ppo=NULL;
      
      /* compute node areas of initial surface */
      areas = SUMA_CalculateNodeAreas(SO, NULL);
      ppo = SUMA_ModifyName (Opt.prefix, "append", ".areas", NULL);
      adset = SUMA_CreateDsetPointer(ppo, SUMA_NODE_BUCKET,
                                     NULL, SO->idcode_str,
                                     SO->N_Node);
      if (!SUMA_AddNgrHist(adset->ngr, "BrainSkin", argc, argv)) {
         SUMA_S_Err("Failed to add history");
      }
      SUMA_free(ppo); ppo=NULL;
      if (!SUMA_AddDsetIndexCol(adset, NULL, NULL, NULL)) {
         SUMA_S_Err("Failed to add new data");
         SUMA_FreeDset(adset); exit(1);
      } 
      if (!SUMA_AddDsetNelCol(adset, "folded_area", SUMA_NODE_FLOAT, areas, 
                              NULL, 1)) {
         SUMA_S_Err("Failed to add new data");
         SUMA_FreeDset(adset); exit(1);
      }
      SUMA_free(areas); areas=NULL; 
      
      /* Now smooth original surface inside enclosure */
      SUMA_S_Note("Inflations");
      SOinf = SUMA_CreateChildSO(SO, NULL, -1, NULL, -1, 0);
      SUMA_SurfaceMetrics(SOinf, "EdgeList|MemberFace", NULL);
      #if 0
         /* 100, 10 works for std12 surfs
            800, 80 works for std60 surfs */
      /* turn distances to weights */
      distw = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
      for (ii=0; ii<SO->N_Node; ++ii) {
         distw[ii] = exp(-sqrt(dist[ii]));
      }
      if (!SUMA_NN_GeomSmooth2_SO(SOinf, NULL, 0, 1000, 100, SOfs,
                                  proj, distw )) {
         SUMA_S_Err("Failed to geom smooth 2 surface");
      }                  
      
      /* A little more smoothing now but with no anchors 
      Reduces risk for some folding. Perhaps should have
      more progressive hardening of weights and maybe some
      smoothing of projection directions inside smoothing 
      function */
      if (!SUMA_NN_GeomSmooth2_SO(SOinf, NULL, 0, 100, 10, SOfs,
                                  NULL, NULL )) {
         SUMA_S_Err("Failed to geom smooth 2 surface step 2");
      }
      #else 
      distw = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
      for (ii=0; ii<SO->N_Node; ++ii) {
         distw[ii] = exp(-dist[ii]);
      }
      SUMA_S_Warn("Looks like 2000, 1001 was better than 1000, 10001\n"
                   "Could it be that 1000, 501 is also OK...");
      if (!SUMA_NN_GeomSmooth3_SO(SOinf, NULL, 0, 2000, 1001, SOfs,
                                  distw, odsetvs, NULL)) {
         SUMA_S_Err("Failed to geom smooth 3 surface");
      } 
      #endif
      /* match the area of the enclosing surface */
      SUMA_S_Note("Matching areas");
      if (!SUMA_EquateSurfaceAreas(SOinf, SOfs, 0.001, NULL)) {
                  SUMA_SL_Warn("Failed to fix surface size.\n"
                               "Trying to finish ...");
      }
      SUMA_S_Notev("Pial Area: %f\n"
                   "Inflated Pial-Isotopic Area: %f\n"
                   "Enclosing, Skin Area: %f\n",
                   SUMA_Mesh_Area(SO, NULL, -1),
                   SUMA_Mesh_Area(SOinf, NULL, -1),
                   SUMA_Mesh_Area(SOfs, NULL, -1));
      ppo = SUMA_ModifyName (Opt.prefix, "append", ".skin.isotopic", NULL);
      SUMA_Save_Surface_Object_Wrap(ppo, NULL, SOinf, 
                                    SUMA_GIFTI, SUMA_ASCII, NULL);
      SUMA_free(ppo); ppo=NULL;
      SUMA_free(distw); distw=NULL;
      
      /* Compute the inflated areas */
      areas = SUMA_CalculateNodeAreas(SOinf, NULL);
      if (!SUMA_AddDsetNelCol(adset, "inflated_area", SUMA_NODE_FLOAT, areas, 
                              NULL, 1)) {
         SUMA_S_Err("Failed to add new data");
         SUMA_FreeDset(adset); exit(1);
      }
      
      /* store distance to skin */
      if (!SUMA_AddDsetNelCol(adset, "SkinDist", SUMA_NODE_FLOAT, dist, 
                              NULL, 1)) {
         SUMA_S_Err("Failed to add new data");
         SUMA_FreeDset(adset); exit(1);
      }
      
      #if 0
         /* just a sanity check, useless otherwise */
      SUMA_S_Note("Inflations to match initial area");
      SOinfA = SUMA_CreateChildSO(SOinf, NULL, -1, NULL, -1, 0);
      SUMA_SurfaceMetrics(SOinfA, "EdgeList|MemberFace", NULL);
      SUMA_EquateSurfaceAreas( SOinfA, SO, 0.1, NULL);
      SUMA_RECOMPUTE_POLYGON_AREAS(SOinfA);
      ppo = SUMA_ModifyName (Opt.prefix, "append", 
                              ".skin.isotopic.Amatched", NULL);
      SUMA_Save_Surface_Object_Wrap(ppo, NULL, SOinfA, 
                                    SUMA_GIFTI, SUMA_ASCII, NULL);
      SUMA_free(ppo); ppo=NULL;
      /* Compute the areas after surface is inflated to match initial area*/
      areasA = SUMA_CalculateNodeAreas(SOinfA, NULL);
      if (!SUMA_AddDsetNelCol(adset, "inflated_orig_area", 
                              SUMA_NODE_FLOAT, areasA, 
                              NULL, 1)) {
         SUMA_S_Err("Failed to add new data");
         SUMA_FreeDset(adset); exit(1);
      }
      SUMA_free(areasA); areasA=NULL;
      #endif
       
      /* and the GI */
      fv1 = (float *)SDSET_VEC(adset,0); /* initial area */
      fv2 = (float *)SDSET_VEC(adset,1); /* after inflation*/
      tot_init = 0.0;
      tot_infl = 0.0;
      unit_init = 0.0;
      unit_infl = 0.0;
      for (ii=0; ii<SO->N_Node; ++ii) {
         tot_init += fv1[ii];
         tot_infl += fv2[ii];
         if (fv2[ii] != 0.0) areas[ii] = fv1[ii]/fv2[ii];
         else areas[ii]=0.0;
      }
      unit_init = tot_init/(float)SO->N_Node;
      unit_infl = tot_infl/(float)SO->N_Node;
      if (!SUMA_AddDsetNelCol(adset, "GI", SUMA_NODE_FLOAT, areas, 
                              NULL, 1)) {
         SUMA_S_Err("Failed to add new data");
         SUMA_FreeDset(adset); exit(1);
      }
      
      /* assume uniform initial area assignment */ 
      for (ii=0; ii<SO->N_Node; ++ii) {
         areas[ii] = unit_init / fv2[ii]; 
      }
      if (!SUMA_AddDsetNelCol(adset, "GIu", SUMA_NODE_FLOAT, areas, 
                              NULL, 1)) {
         SUMA_S_Err("Failed to add new data");
         SUMA_FreeDset(adset); exit(1);
      }
      
      /* compute distance between initial coords and final ones */
      fv1 = SO->NodeList;
      fv2 = SOinf->NodeList;
      for (ii=0; ii<SO->N_Node; ++ii) {
         SUMA_SEG_LENGTH( fv1, fv2, areas[ii]);
         fv1 = fv1+3; fv2 = fv2+3; 
      }      
      if (!SUMA_AddDsetNelCol(adset, "CoordDist", SUMA_NODE_FLOAT, areas, 
                              NULL, 1)) {
         SUMA_S_Err("Failed to add new data");
         SUMA_FreeDset(adset); exit(1);
      }
      SUMA_free(areas); areas=NULL; 
      
      cc = SUMA_WriteDset_s((SDSET_FILENAME(adset)), adset, 
                            Opt.sform, 1,1);
      if (cc) SUMA_free(cc);
      
      if (dist) SUMA_free(dist); dist = NULL;
      if (proj) SUMA_free(proj); proj = NULL;
      DSET_delete(gdset); gdset=NULL;
      DSET_delete(odset); odset = NULL;
      DSET_delete(odseti); odseti = NULL;
      DSET_delete(odsetvs); odsetvs = NULL;
      SUMA_Free_Surface_Object(SOo); SOo = NULL; 
      SUMA_Free_Surface_Object(SOinf); SOinf = NULL; 
      if (SOinfA) SUMA_Free_Surface_Object(SOinfA); SOinfA = NULL; 
      SUMA_Free_Surface_Object(SOf); SOf = NULL; 
      SUMA_Free_Surface_Object(SOfs); SOfs = NULL; 
      
      SUMA_S_Notev("Total GI=%f\n",tot_init/tot_infl); 
   } 
   
   SUMA_LH("clean up");
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }

   if (Opt.outfile) SUMA_free(Opt.outfile);
   if (Opt.histnote) SUMA_free(Opt.histnote);
   if (Opt.prefix) SUMA_free(Opt.prefix);
   if (Opt.segdo) SUMA_free(Opt.segdo);
   if (Opt.skingrid) SUMA_free(Opt.skingrid);
   SUMA_ifree(Opt.in_name);
   if (!SUMA_FreeSpecFields(Spec)) {
      SUMA_S_Err("Failed to free SpecFields");
   } SUMA_free(Spec); Spec = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {
      SUMA_SL_Err("SUMAg_CF Cleanup Failed!");}
   
   
   SUMA_RETURN(0);
}
#endif
