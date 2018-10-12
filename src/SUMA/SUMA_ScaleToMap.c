#include "SUMA_suma.h"


   void SUMA_ScaleToMap_usage ()
   {
      static char FuncName[]={"SUMA_ScaleToMap_usage"};
      char * s = NULL;

      SUMA_ENTRY;

      s = SUMA_help_basics();
      fprintf (SUMA_STDOUT,
"\nUsage:  ScaleToMap <-input IntFile icol vcol>  \n"
"    [-cmap MapType] [-cmapfile Mapfile] [-cmapdb Palfile] [-frf] \n"
"    [-clp/-perc_clp clp0 clp1] [-apr/-anr range]\n"
"    [-interp/-nointerp/-direct] [-msk msk0 msk1] [-nomsk_col]\n"
"    [-msk_col R G B] [-br BrightFact]\n"
"    [-h/-help] [-verb] [-showmap] [-showdb]\n"
"\n"
"    -input IntFile icol vcol: input data.\n"
"       Infile: 1D formatted ascii file containing node values\n"
"       icol: index of node index column \n"
"       (-1 if the node index is implicit)\n"
"       vcol: index of node value column.\n"
"       Example: -input ValOnly.1D -1 0 \n"
"       for a 1D file containing node values\n"
"       in the first column and no node indices.\n"
"       Example: -input NodeVal.1D 1 3\n"
"       for a 1D file containing node indices in\n"
"       the SECOND column and node values in the \n"
"       FOURTH column (index counting begins at 0)\n"
"    -v and -iv options are now obsolete.\n"
"       Use -input option instead.\n"
"    -cmap MapName: (optional, default RGYBR20) \n"
"       choose one of the standard colormaps available with SUMA:\n"
"       RGYBR20, BGYR19, BW20, GRAY20, MATLAB_DEF_BYR64, \n"
"       ROI64, ROI128\n"
"       You can also use AFNI's default paned color maps:\n"
"       The maps are labeled according to the number of \n"
"       panes and their sign. Example: afni_p10\n"
"       uses the positive 10-pane afni colormap.\n"
"       afni_n10 is the negative counterpart.\n"
"       These maps are meant to be used with\n"
"       the options -apr and -anr listed below.\n"
"       You can also load non-default AFNI colormaps\n"
"       from .pal files (AFNI's colormap format); see option\n"
"       -cmapdb below.\n"
"    -cmapdb Palfile: read color maps from AFNI .pal file\n"
"       In addition to the default paned AFNI colormaps, you\n"
"       can load colormaps from a .pal file.\n"
"       To access maps in the Palfile you must use the -cmap option\n"
"       with the label formed by the name of the palette, its sign\n"
"       and the number of panes. For example, to following palette:\n"
"       ***PALETTES deco [13]\n"
"       should be accessed with -cmap deco_n13\n"
"       ***PALETTES deco [13+]\n"
"       should be accessed with -cmap deco_p13\n"
"    -cmapfile Mapfile: read color map from Mapfile.\n"
"       Mapfile:1D formatted ascii file containing colormap.\n"
"               each row defines a color in one of two ways:\n"
"               R  G  B        or\n"
"               R  G  B  f     \n"
"       where R, G, B specify the red, green and blue values, \n"
"       between 0 and 1 and f specifies the fraction of the range\n"
"       reached at this color. THINK values of right of AFNI colorbar.\n"
"       The use of fractions (it is optional) would allow you to create\n"
"       non-linear color maps where colors cover differing fractions of \n"
"       the data range.\n"
"       Sample colormap with positive range only (a la AFNI):\n"
"               0  0  1  1.0\n"
"               0  1  0  0.8\n"
"               1  0  0  0.6\n"
"               1  1  0  0.4\n"
"               0  1  1  0.2\n"
"       Note the order in which the colors and fractions are specified.\n"
"       The bottom color of the +ve colormap should be at the bottom of the\n"
"       file and have the lowest +ve fraction. The fractions here define a\n"
"       a linear map so they are not necessary but they illustrate the format\n"
"       of the colormaps.\n"
"       Comparable colormap with negative range included:\n"
"               0  0  1   1.0\n"
"               0  1  0   0.6\n"
"               1  0  0   0.2\n"
"               1  1  0  -0.2\n"
"               0  1  1  -0.6\n"
"       The bottom color of the -ve colormap should have the \n"
"       lowest -ve fraction. \n"
"       You can use -1 -1 -1 for a color to indicate a no color\n"
"       (like the 'none' color in AFNI). Values mapped to this\n"
"       'no color' will be masked as with the -msk option.\n"
"       If your 1D color file has more than three or 4 columns,\n"
"       you can use the [] convention adopted by AFNI programs\n"
"       to select the columns you need.\n"
"    -frf: (optional) first row in file is the first color.\n"
"       As explained in the -cmapfile option above, the first \n"
"       or bottom (indexed 0 )color of the colormap should be \n"
"       at the bottom of the file. If the opposite is true, use\n"
"       the -frf option to signal that.\n"
"       This option is only useful with -cmapfile.\n"
"    -clp/-perc_clp clp0 clp1: (optional, default no clipping)\n"
"       clips values in IntVect. if -clp is used then values in vcol\n"
"       < clp0 are clipped to clp0 and > clp1 are clipped to clp1\n"
"       if -perc_clp is used them vcol is clipped to the values \n"
"       corresponding to clp0 and clp1 percentile.\n"
"       The -clp/-prec_clp options are mutually exclusive with -apr/-anr.\n"
"    -apr range: (optional) clips the values in IntVect to [0 range].\n"
"       This option allows range of colormap to be set as in AFNI, \n"
"       with Positive colorbar (Pos selected).\n"
"       This option is mutually exclusive with -clp/-perc_clp).\n"
"       set range = 0 for autoranging.\n"
"       If you use -apr and your colormap contains fractions, you\n"
"       must use a positive range colormap.\n"
"    -anr range: (optional) clips the values in IntVect to [-range range].\n"
"       This option allows range of colormap to be set as in AFNI, \n"
"       with Negative colorbar (Pos NOT selected).\n"
"       This option is mutually exclusive with -clp/-perc_clp).\n"
"       set range = 0 for autoranging.\n"
"       If you use -anr and your colormap contains fractions, you\n"
"       must use a negative range colormap.\n"
"    -interp: (default) use color interpolation between colors in colormap\n"
"       If a value is assigned between two colors on the colorbar,\n"
"       it receives a color that is an interpolation between those two colors.\n"
"       This is the default behaviour in SUMA and AFNI when using the continuous\n"
"       colorscale. Mutually exclusive with -nointerp and -direct options.\n"
"    -nointerp: (optional) turns off color interpolation within the colormap\n"
"       Color assigniment is done a la AFNI when the paned colormaps are used.\n"
"       Mutually exclusive with -interp and -direct options.\n"
"    -direct: (optional) values (typecast to integers) are mapped directly\n"
"       to index of color in color maps. Example: value 4 is assigned\n"
"       to the 5th (index 4) color in the color map (same for values\n"
"       4.2 and 4.7). This mapping scheme is useful for ROI indexed type\n"
"       data. Negative data values are set to 0 and values >= N_col \n"
"       (the number of colors in the colormap) are set to N_col -1\n"
"    -msk_zero: (optional) values that are 0 will get masked no matter\n"
"       what colormaps or mapping schemes you are using. \n"
"       AFNI masks all zero values by default.\n"
"    -msk msk0 msk1: (optinal, default is no masking) \n"
"       Values in vcol (BEFORE clipping is performed) \n"
"       between [msk0 msk1] are masked by the masking color.\n"
"    -msk_col R G B: (optional, default is 0.3 0.3 0.3) \n"
"       Sets the color of masked voxels.\n"
"    -nomsk_col: do not output nodes that got masked.\n"
"       It does not make sense to use this option with\n"
"       -msk_col.\n"
"    -br BrightFact: (optional, default is 1) \n"
"       Applies a brightness factor to the colors \n"
"       of the colormap and the mask color.\n"
"    -h or -help: displays this help message.\n"
"\n"
"   The following options are for debugging and sanity checks.\n"
"    -verb: (optional) verbose mode.\n"
"    -showmap: (optional) print the colormap to the screen and quit.\n"
"       This option is for debugging and sanity checks.\n"
"       You can use MakeColorMap in Usage3 to write out a colormap\n"
"       in its RGB form.\n"
"    -showdb: (optional) print the colors and colormaps of AFNI\n"
"       along with any loaded from the file Palfile.\n"
"%s"
                              "\n", s);
      SUMA_free(s); s = NULL;
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      fprintf (SUMA_STDOUT,   "    Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \n"
                              "      July 31/02 \n"
                              "\n");
      SUMA_RETURNe;
   }

int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ScaleToMap"};
   char  *IntName = NULL, *Prfx, h[9],
         *CmapFileName = NULL, *dbfile = NULL, *MapName=NULL;
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   int N_V, N_Int, kar, k, ii, i, icol=-1, vcol=-1, Sgn, interpmode, k3;
   int Vminloc, Vmaxloc, *iV = NULL;
   float Vmin, Vmax, brfact;
   float *V = NULL, *Vsort = NULL;
   float IntRange[2], MaskColor[3], MaskRange[2]={0.0, 0.0}, arange;
   SUMA_Boolean ApplyClip, ApplyMask, setMaskCol, ApplyPercClip, Vopt;
   SUMA_Boolean iVopt, inopt, NoMaskCol, MapSpecified, alaAFNI, MaskZero;
   SUMA_Boolean brk, frf, ShowMap, ShowMapdb;
   SUMA_COLOR_MAP *CM;
   SUMA_SCALE_TO_MAP_OPT * OptScl;
   int MapType, freecm = 1;
   SUMA_COLOR_SCALED_VECT * SV;
   SUMA_AFNI_COLORS *SAC=NULL;
   SUMA_Boolean FromAFNI = NOPE;
   int imap, isPmap, isNmap;
   SUMA_Boolean LocalHead = NOPE;


   SUMA_STANDALONE_INIT;
   SUMAg_CF->isGraphical = YUP;
   SUMA_mainENTRY;

   /* this is placed down here to */
   if (argc < 2) {
      SUMA_ScaleToMap_usage();
      exit (0);
   }
   kar = 1;
   brfact = 1; /* the brightness factor */
   MaskColor[0] = MaskColor[1] = MaskColor[2] = 0.3;
   ApplyClip = NOPE;
   ApplyPercClip = NOPE;
   ApplyMask = NOPE;
   NoMaskCol = NOPE;
   MaskZero = NOPE;
   setMaskCol = NOPE;
   Vopt = NOPE;
   iVopt = NOPE;
   inopt = NOPE;
   MapType = SUMA_CMAP_RGYBR20;
   brk = NOPE;
   MapSpecified = NOPE;
   CmapFileName = NULL;
   interpmode = SUMA_UNDEFINED_MODE;
   ShowMap = NOPE;
   alaAFNI = NOPE;   /* applying the alaAFNI mapping */
   frf = NOPE;
   arange  = -1.0; /* afni range specified */
   Sgn = 0;
   ShowMapdb = NOPE;
   while (kar < argc) { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         SUMA_ScaleToMap_usage();
         exit (0);
      }

      SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      if (!brk && strcmp(argv[kar], "-verb") == 0) {
         LocalHead = NOPE;
         brk = YUP;
      }

      if (!brk && strcmp(argv[kar], "-ionot") == 0) {
         SUMA_SL_Err("-ionot is obsolete. \n"
                     "Use -trace option.");
         exit (1);
         SUMA_INOUT_NOTIFY_ON;
         brk = YUP;
      }

      if (!brk && strcmp(argv[kar], "-msk_zero") == 0) {
         MaskZero = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
         if (kar+2 >= argc)  {
            fprintf (SUMA_STDERR, "need 3 arguments after -input \n");
            exit (1);
         }
         IntName = argv[kar]; kar ++;
         icol = atoi(argv[kar]); kar ++;
         vcol = atoi(argv[kar]);
         inopt = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-apr") == 0)) {
         if (arange >= 0) {
            fprintf (SUMA_STDERR, "range has already been specified.\n");
            exit (1);
         }
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -apr \n");
            exit (1);
         }
         arange = atof(argv[kar]);
         if (arange < 0) {
            fprintf (SUMA_STDERR, "range must be positive.\n");
            exit (1);
         }
         Sgn = 1;
         alaAFNI = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-anr") == 0)) {
         if (arange >= 0) {
            fprintf (SUMA_STDERR, "range has already been specified.\n");
            exit (1);
         }
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -anr \n");
            exit (1);
         }
         arange = atof(argv[kar]);
         if (arange < 0) {
            fprintf (SUMA_STDERR, "range must be positive.\n");
            exit (1);
         }

         Sgn = -1;
         alaAFNI = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-v") == 0)) {
         fprintf (SUMA_STDERR, "\n -v option is now obsolete.\nUse -input option instead.\n");
         exit (1);
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -v \n");
            exit (1);
         }
         IntName = argv[kar];
         Vopt = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-iv") == 0)) {
         fprintf (SUMA_STDERR, "\n -iv option is now obsolete.\nUse -input option instead.\n");
         exit (1);
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -iv \n");
            exit (1);
         }
         IntName = argv[kar];
         iVopt = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-br") == 0)) {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -br \n");
            exit (1);
         }
         brfact = atof(argv[kar]);

         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-frf") == 0)) {
         frf = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-showmap") == 0)) {
         ShowMap = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-showdb") == 0)) {
         ShowMapdb = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-nointerp") == 0)) {
         if (interpmode != SUMA_UNDEFINED_MODE) {
            fprintf (SUMA_STDERR, "Color interpolation mode already set.\n");
         }
         interpmode = SUMA_NO_INTERP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-direct") == 0)) {
         if (interpmode != SUMA_UNDEFINED_MODE) {
            fprintf (SUMA_STDERR, "Color interpolation mode already set.\n");
         }
         interpmode = SUMA_DIRECT;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-interp") == 0)) {
         if (interpmode != SUMA_UNDEFINED_MODE) {
            fprintf (SUMA_STDERR, "Color interpolation mode already set.\n(-nointerp, -direct and -interp are mutually exclusive.\n");
         }
         interpmode = SUMA_INTERP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-clp") == 0)) {
         kar ++;
         if (kar+1 >= argc)  {
              fprintf (SUMA_STDERR, "need 2 arguments after -clp \n");
            exit (1);
         }
         ApplyClip = YUP;
         IntRange[0] = atof(argv[kar]); kar ++;
         IntRange[1] = atof(argv[kar]);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-perc_clp") == 0)) {
         kar ++;
         if (kar+1 >= argc)  {
              fprintf (SUMA_STDERR, "need 2 arguments after -perc_clp ");
            exit (1);
         }
         ApplyPercClip = YUP;
         IntRange[0] = atof(argv[kar]); kar ++;
         IntRange[1] = atof(argv[kar]);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-msk") == 0)) {
         kar ++;
         if (kar+1 >= argc)  {
              fprintf (SUMA_STDERR, "need 2 arguments after -msk ");
            exit (1);
         }
         ApplyMask = YUP;
         MaskRange[0] = atof(argv[kar]); kar ++;
         MaskRange[1] = atof(argv[kar]);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-nomsk_col") == 0)) {
         NoMaskCol = YUP;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-msk_col") == 0)) {
         kar ++;
         if (kar+2 >= argc)  {
              fprintf (SUMA_STDERR, "need 3 arguments after -msk_col ");
            exit (1);
         }
         setMaskCol = YUP;
         MaskColor[0] = atof(argv[kar]); kar ++;
         MaskColor[1] = atof(argv[kar]); kar ++;
         MaskColor[2] = atof(argv[kar]);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-cmapfile") ==0)) {
         if (MapSpecified) {
            fprintf (SUMA_STDERR, "Color map already specified.\n-cmap and -cmapfile are mutually exclusive\n");
            exit (1);
         }
         MapSpecified = YUP;
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need 1 arguments after -cmapfile ");
            exit (1);
         }

         CmapFileName = argv[kar];
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-cmapdb") ==0)) {
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need 1 arguments after -cmapdb ");
            exit (1);
         }

         dbfile = argv[kar];
         brk = YUP;
      }


      if (!brk && (strcmp(argv[kar], "-cmap") ==0)) {
         if (MapSpecified) {
            fprintf (SUMA_STDERR, "Color map already specified.\n-cmap and -cmapfile are mutually exclusive\n");
            exit (1);
         }
         MapSpecified = YUP;
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need 1 arguments after -cmap ");
            exit (1);
         }
         MapName = argv[kar];

         brk = YUP;
      }

      if (!brk) {
         fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } else {
         brk = NOPE;
         kar ++;
      }

   }/* loop accross command ine options */

   /* Get your colors straightened out */
   if (!SUMAg_CF->scm) {
      SUMAg_CF->scm = SUMA_Build_Color_maps();
      if (!SUMAg_CF->scm) {
         SUMA_SL_Err("Failed to build color maps.\n");
         exit(1);
      }
   }

   SAC = SUMAg_CF->scm;
   /* are there database files to read */
   if (dbfile) {
      SUMA_LH("Now trying to read db file");
      if (SUMA_AFNI_Extract_Colors ( dbfile, SAC ) < 0) {
         SUMA_S_Errv("Failed to read %s colormap file.\n", dbfile);
         exit(1);
      }
   }

   FromAFNI = NOPE; /* assume colormap is not coming from SAC
                       (the colormap database structure) */
   if (CmapFileName) {
      /* load the color map */
      CM = SUMA_Read_Color_Map_1D (CmapFileName);
      if (CM == NULL) {
         SUMA_S_Err("Could not load colormap.\n");
         exit (1);
      }
      if (frf) {
         SUMA_LH("Flipping colormap");
         SUMA_Flip_Color_Map (CM);
      }

      if (!CM->Sgn) CM->Sgn = Sgn;
   }else{
      /* dunno what kind of map yet. Try default first */
      if (MapName) {
         CM = SUMA_FindNamedColMap (MapName);
         freecm = 0;
         if (CM) {
            /* good, sign it and out you go */
            CM->Sgn = Sgn;
         } else {
            SUMA_S_Err("Could not get standard colormap.\n");
            exit (1);
         }
      } else {
         SUMA_LH("An AFNI color map ");
         /* a color from AFNI's maps */
         FromAFNI = YUP;
         imap = SUMA_Find_ColorMap ( MapName, SAC->CMv, SAC->N_maps, -2);
         if (imap < 0) {
            SUMA_S_Errv("Could not find colormap %s.\n", MapName);
            exit (1);
         }
         CM = SAC->CMv[imap];
      }
   }


   /* show the colromap on STDERR */
   if (ShowMap) {
      fprintf (SUMA_STDERR, "%s: Colormap used:\n", FuncName);
      SUMA_Show_ColorMapVec (&CM, 1, NULL, 2);
      {
         SUMA_SurfaceObject *SO = NULL;
         float orig[3]     = { SUMA_CMAP_ORIGIN  };
         float topright[3] = { SUMA_CMAP_TOPLEFT };

         SO = SUMA_Cmap_To_SO (CM, orig, topright, 2);
         if (SO) SUMA_Free_Surface_Object(SO);
      }
      exit(0);
   }

   /* show all the colors and colormaps in SAC on STDERR */
   if (ShowMapdb) {
      fprintf (SUMA_STDERR, "%s: AFNI colormaps found in db:\n", FuncName);
      SUMA_Show_ColorVec (SAC->Cv, SAC->N_cols, NULL);
      SUMA_Show_ColorMapVec (SAC->CMv, SAC->N_maps, NULL, 2);
      exit(0);
   }


   if (!IntName) {
      fprintf (SUMA_STDERR,"Error %s: No input file specified.\n", FuncName);
      exit(1);
   }

   /* default interpolation mode */
   if (interpmode == SUMA_UNDEFINED_MODE) interpmode = SUMA_INTERP;

   /* check input */
   if (!SUMA_filexists (IntName)) {
      fprintf (SUMA_STDERR,"Error %s: File %s could not be found.\n", FuncName, IntName);
      exit(1);
   }

   if (frf && !CmapFileName) {
      fprintf (SUMA_STDERR,"Error %s: -frf option is only valid with -cmapfile.\n", FuncName);
      exit(1);
   }

   if (ApplyPercClip && ApplyClip) {
      fprintf (SUMA_STDERR,"Error %s: Simultaneous use of -clp and -perc_clp. You should be punished.\n", FuncName);
      exit(1);
   }

   if ((ApplyPercClip || ApplyClip) && arange >= 0.0) {
      fprintf (SUMA_STDERR,"Error %s: Simultaneous use of -clp/-perc_clp and -apr/anr.\n Read the help.\n", FuncName);
      exit(1);
   }

   if (iVopt || Vopt) {
      fprintf (SUMA_STDERR,"Error %s: -v and -iv are obsolete.\n Use -input option instead.\n", FuncName);
      exit(1);
   }

   if (!inopt) {
      fprintf (SUMA_STDERR,"Error %s: -input option must be specified.\n", FuncName);
      exit(1);
   }

   im = mri_read_1D (IntName);

   if (!im) {
      SUMA_S_Err("Failed to read file");
      exit (1);
   }

   if (vcol < 0) {
      fprintf (SUMA_STDERR,"Error %s: vcol must be > 0\n", FuncName);
      exit(1);
   }

   far = MRI_FLOAT_PTR(im);
   if (icol < 0 && icol != -1) {
      fprintf (SUMA_STDERR,"Error %s: icol(%d) can only have -1 for a negative value\n", FuncName, icol);
      exit(1);
   }

   if (icol >= im->ny || vcol >= im->ny) {
      fprintf (SUMA_STDERR,"Error %s: icol(%d) and vcol(%d) must be < %d\nwhich is the number of columns in %s\n",
          FuncName, icol, vcol, im->ny, IntName);
      exit(1);
   }


   if (brfact <=0 || brfact > 1) {
      fprintf (SUMA_STDERR,"Error %s: BrightFact must be > 0 and <= 1.\n", FuncName);
      exit (1);
   }

   if (MaskColor[0] < 0 || MaskColor[0] > 1 || MaskColor[1] < 0 || MaskColor[1] > 1 || MaskColor[2] < 0 || MaskColor[2] > 1) {
      fprintf (SUMA_STDERR,"Error %s: MaskColor values must be >=0 <=1.\n", FuncName);
      exit(1);
   }


   N_V = im->nx;
   V = (float *) SUMA_calloc (N_V, sizeof(float));
   iV = (int *) SUMA_calloc (N_V, sizeof(int));
   if (!V || !iV) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for V or iV.\n", FuncName);
      exit(1);
   }

   if (icol < 0) {
     for (ii=0; ii < N_V; ++ii) {
         iV[ii] = ii;
         V[ii] = far[vcol*N_V+ii];
     }
   } else {
      for (ii=0; ii < N_V; ++ii) {
         iV[ii] = (int)far[icol*N_V+ii];
         V[ii] = far[vcol*N_V+ii];
      }
   }

   mri_free(im); im = NULL;

   /* read values per node */
   /* SUMA_disp_vect (V, 3);  */

   /* find the min/max of V */
   SUMA_MIN_MAX_VEC(V, N_V, Vmin, Vmax, Vminloc, Vmaxloc)
   /* fprintf (SUMA_STDERR,"%s: Vmin=%f, Vmax = %f\n", FuncName, Vmin, Vmax);*/

   if (arange == 0.0) {
      if (fabs((double)Vmin) > fabs((double)Vmax)) arange = (float)fabs((double)Vmin);
      else arange = (float)fabs((double)Vmax);
   }
   /* figure out the range if PercRange is used */
   if (ApplyPercClip) {

      fprintf (SUMA_STDERR,"%s: Percentile range [%f..%f] is equivalent to ", FuncName, IntRange[0], IntRange[1]);
      Vsort = SUMA_PercRange (V, NULL, N_V, IntRange, IntRange, NULL);
      fprintf (SUMA_STDERR,"[%f..%f]\n", IntRange[0], IntRange[1]);
      ApplyClip = YUP;

      if (Vsort) SUMA_free(Vsort);
      else {
         fprintf (SUMA_STDERR,"Error %s: Error in SUMA_PercRange.\n", FuncName);
         exit(1);
      }
   }


   /* get the options for creating the scaled color mapping */
   OptScl = SUMA_ScaleToMapOptInit();
   if (!OptScl) {
      fprintf (SUMA_STDERR,
               "Error %s: Could not get scaling option structure.\n", FuncName);
      exit (1);
   }

   /* work the options a bit */
   if (ApplyMask) {
      OptScl->ApplyMask = ApplyMask;
      OptScl->MaskRange[0] = MaskRange[0];
      OptScl->MaskRange[1] = MaskRange[1];
      OptScl->MaskColor[0] = MaskColor[0];
      OptScl->MaskColor[1] = MaskColor[1];
      OptScl->MaskColor[2] = MaskColor[2];
   }

   if (ApplyClip) {
      OptScl->ApplyClip = YUP;
      OptScl->IntRange[0] = IntRange[0]; OptScl->IntRange[1] = IntRange[1];
   }

   OptScl->interpmode = interpmode;

   OptScl->BrightFact = brfact;

   if (MaskZero) OptScl->MaskZero = YUP;

   /* map the values in V to the colormap */
      /* allocate space for the result */
      SV = SUMA_Create_ColorScaledVect(N_V, 0);
      if (!SV) {
         fprintf (SUMA_STDERR,
                  "Error %s: Could not allocate for SV.\n", FuncName);
         exit(1);
      }

      /* finally ! */
      if (alaAFNI) {
         if (LocalHead) {
            fprintf (SUMA_STDERR,
                     "%s: Calling SUMA_ScaleToMap_alaAFNI\n", FuncName);
            fprintf (SUMA_STDERR,"%s: arange = %f\n",  FuncName, arange);
         }
         if (CM->frac) {
            if (CM->frac[0] > 0 && CM->Sgn == -1) {
               SUMA_S_Err ("Color map fractions positive with -anr option");
               exit(1);
            }
            if (CM->frac[0] < 0 && CM->Sgn == 1) {
               SUMA_S_Err ("Color map fractions negative with -apr option");
               exit(1);
            }
         }

         if (Sgn) {
            if (Sgn != CM->Sgn) {
               SUMA_S_Warn ("Mixing positive maps (all fractions > 0) "
                            "with -anr option\n"
                            "or vice versa. That is allowed but know what"
                            " you're doing.\n");
            }
         }
         if (!SUMA_ScaleToMap_alaAFNI (V, N_V, arange, CM, OptScl, SV)) {
            fprintf (SUMA_STDERR,
               "Error %s: Failed in SUMA_ScaleToMap_alaAFNI.\n", FuncName);
            exit(1);
         }
      } else {
         if (LocalHead)
            fprintf (SUMA_STDERR,"%s: Calling SUMA_ScaleToMap\n", FuncName);
         if (!SUMA_ScaleToMap (V, N_V, Vmin, Vmax, CM, OptScl, SV)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
            exit(1);
         }
      }

   /* Now write the colored vector back to disk */
   if (NoMaskCol) {
      for (k=0; k < N_V; ++k) {
         k3 = 3*k;
         if (!SV->isMasked[k])
            fprintf (SUMA_STDOUT, "%d %f %f %f\n",
                     iV[k], SV->cV[k3  ], SV->cV[k3+1], SV->cV[k3+2]);
      }
   } else {
      for (k=0; k < N_V; ++k) {
         k3 = 3*k;
         fprintf (SUMA_STDOUT, "%d %f %f %f\n",
                  iV[k], SV->cV[k3  ], SV->cV[k3+1], SV->cV[k3+2]);
      }
   }

   /* freeing time */
   if (V) SUMA_free(V);
   if (iV) SUMA_free(iV);
   if (!FromAFNI && freecm) if (CM) SUMA_Free_ColorMap (CM); /* only free CM if
                                       it was a pointer copy from a map in SAC */
   if (OptScl) SUMA_free(OptScl);
   if (SV) SUMA_Free_ColorScaledVect (SV);
   #if 0
      if (SAC) SAC = SUMA_DestroyAfniColors(SAC); /* destroy SAC */
   #else
      SAC = NULL; /* freeing is done in SUMAg_CF */
   #endif
   SUMA_Free_CommonFields(SUMAg_CF);

   SUMA_RETURN (0);
}
