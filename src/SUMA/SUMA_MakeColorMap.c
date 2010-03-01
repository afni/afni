#include "SUMA_suma.h"


SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void SUMA_MakeColorMap_usage ()
   {
      static char FuncName[]={"SUMA_MakeColorMap_usage"};
      char * s = NULL;
      
      SUMA_ENTRY;
    
      s = SUMA_help_basics();
      fprintf (SUMA_STDOUT, 
"\n"
 "Usage1: \n"
 "MakeColorMap <-fn Fiducials_Ncol> [-pos] [-ah prefix] [-h/-help]\n"
 "    Creates a colormap of N colors that contains the fiducial colors.\n"
 "    -fn Fiducials_Ncol: Fiducial colors and their indices in the color\n"
 "                        map are listed in file Fiducials_Ncol.\n"
 "       Each row contains 4 tab delimited values:\n"
 "       R G B i\n"
 "       R G B values are between 0 and 1 and represent the \n"
 "       i-th color in the colormap. i should be between 0 and\n"
 "       N-1, N being the total number of colors in the colormap.\n"
 "\n"
 "Usage2: \n"
 "MakeColorMap <-f Fiducials> <-nc N> [-sl] [-ah prefix] [-h/-help]\n"
 "    Creates a colormap of N colors that contains the fiducial colors.\n"
 "    -f Fiducials:  Fiducial colors are listed in an ascii file Fiducials. \n"
 "       Each row contains 3 tab delimited R G B values between 0 and 1.\n"
 "    -nc N: Total number of colors in the color map.\n"
 "    -sl: (optional, default is NO) if used, the last color in the Fiducial \n"
 "       list is omitted. This is useful in creating cyclical color maps.\n"
 "\n"
 "Usage3: \n"
 "MakeColorMap <-std MapName>\n"
 "    Returns one of SUMA's standard colormaps. Choose from:\n"
 "    rgybr20, ngray20, gray20, bw20, bgyr19, \n"
 "    matlab_default_byr64, roi128, roi256, roi64\n"
 " or if the colormap is in a .pal file:  \n"
 "MakeColorMap -cmapdb Palfile -cmap MapName\n"
 "\n"
 "Usage4:\n"
 "MakeColorMap <-fscolut lbl0 lbl1> \n"
 "             [<-fscolutfile FS_COL_LUT>]\n"
 "   Create AFNI/SUMA colormaps of FreeSurfer colors\n"
 "   indexed between lbl0 and lbl1. \n"
 "   -fscolut lbl0 lbl1: Get colors indexed between\n"
 "                        lbl0 and lbl1, non existing\n"
 "                        integer labels are given a \n"
 "                        gray color. Use -fscolut -1 -1 to\n"
 "                        get all the colors and labels.\n"
 "   -fscolutfile FS_COL_LUT: Use color LUT file FS_COL_LUT\n"
 "                            Default is to use \n"
 "                            $FREESURFER_HOME/FreeSurferColorLUT.txt\n"
 "   -show_fscolut: Show all of the LUT\n"
 "\n"
 "Common options to all usages:\n"
 "    -ah prefix: (optional, Afni Hex format.\n"
 "                 default is RGB values in decimal form)\n"
 "       use this option if you want a color map formatted to fit \n"
 "       in AFNI's .afnirc file. The colormap is written out as \n"
 "      prefix_01 = #xxxxxxx \n      prefix_02 = #xxxxxxx\n       etc...\n" 
 "    -ahc prefix: optional, Afni Hex format, ready to go into.\n"
 "                 pbardefs.h \n"
 "    -h or -help: displays this help message.\n"
 "    -flipud: Flip the map upside down. If the colormap is being \n"
 "             created for interactive loading into SUMA with the 'New'\n"
 "             button from the 'Surface Controller' you will need\n"
 "             to flip it upside down. \n"
 "\n"
 "Example Usage 1: Creating a colormap of 20 colors that goes from \n"
 "Red to Green to Blue to Yellow to Red.\n"
 "\n"
 "   The file FidCol_Nind contains the following:\n"
 "   1 0 0 0\n   0 1 0 5\n   0 0 1 10\n   1 1 0 15\n   1 0 0 19\n"
 "\n"
 "   The following command will generate the RGB colormap in decimal form:\n"
 "   MakeColorMap -fn FidCol_Nind \n"
 "\n"
 "   The following command will generate the colormap and write it as \n"
 "   an AFNI color palette file:\n"
 "   MakeColorMap -fn FidCol_Nind -ah TestPalette > TestPalette.pal\n"
 "\n"
 "Example Usage 2: Creating a cyclical version of the colormap in usage 1:\n"
 "\n"
 "   The file FidCol contains the following:\n"
 "   1 0 0\n   0 1 0\n   0 0 1\n   1 1 0\n   1 0 0\n"
 "\n"
 "   The following command will generate the RGB colormap in decimal form:\n"
 "   MakeColorMap -f FidCol -sl -nc 20 \n"
 "\n"
 "Example Usage 3: \n"
 "   MakeColorMap -std ngray20 \n"
 "\n"
 "Example Usage 4: \n"
 "   MakeColorMap -fscolut 0 255\n"
 "\n"
 "To read in a new colormap into AFNI, either paste the contents of \n"
 "TestPalette.pal in your .afnirc file or read the .pal file using \n"
 "AFNI as follows:\n"
 "1- run afni\n2- Define Function --> right click on Inten (over colorbar) \n"
 "   --> Read in palette (choose TestPalette.pal)\n"
 "3- set the #colors chooser (below colorbar) to 20 (the number of colors in \n"
 "   TestPalette.pal).\n"
                            "%s",s);
      SUMA_free(s); s = NULL;
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      fprintf (SUMA_STDOUT, "    Ziad S. Saad & Rick R. Reynolds SSCC/NIMH/NIH saadz@mail.nih.gov    Tue Apr 23 14:14:48 EDT 2002\n\n");
      
      SUMA_RETURNe;
   }
 
int main (int argc,char *argv[])
{/* Main */
   static char  FuncName[]={"MakeColorMap"};
   char  *fscolutname = NULL, *FidName = NULL, 
         *Prfx = NULL, h[9], *StdType=NULL, *dbfile=NULL, *MapName=NULL; 
   int Ncols = 0, N_Fid = 0, kar, i, ifact, *Nind = NULL, imap = -1, MapSpecified = 0;
   int fsbl0, fsbl1, showfscolut;
   float **Fid=NULL, **M=NULL;
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   int AfniHex=0, freesm;
   SUMA_Boolean   brk, SkipLast, PosMap, 
                  Usage1, Usage2, Usage3, Usage4, flipud, fscolut,
                  LocalHead = NOPE;
   SUMA_COLOR_MAP *SM=NULL;
      
   SUMA_STANDALONE_INIT;

   SUMA_mainENTRY;
   

   
   if (argc < 2) {
      SUMA_MakeColorMap_usage();
      exit (1);
   }
   
   kar = 1;
   freesm = 1;
   fscolutname = NULL;
   fsbl0 = -1;
   fsbl1 = -1;
   brk = NOPE;
   SkipLast = NOPE;
   AfniHex = 0;
   PosMap = NOPE;
   Usage1 = NOPE;
   Usage2 = NOPE;
   Usage3 = NOPE;
   Usage4 = NOPE;
   flipud = NOPE;
   fscolut = NOPE;
   showfscolut = 0;
   MapSpecified = NOPE;
   while (kar < argc) { /* loop accross command ine options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
          SUMA_MakeColorMap_usage();
         exit (1);
      }
      
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
     
      if (!brk && (strcmp(argv[kar], "-v") == 0))
      {
         LocalHead = NOPE;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-flipud") == 0))
      {
         flipud = YUP;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-f") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -f ");
            exit (1);
         }
         FidName = argv[kar];
         Usage1 = YUP;
         brk = YUP;
      }      
      
      if (!brk && (strcmp(argv[kar], "-fscolutfile") == 0))
      {
         Usage4=YUP;
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need 1 argument after -fscolutfile ");
            exit (1);
         }
         fscolutname = argv[kar];
         if (fsbl0 < 0) {
            fsbl0 = 0;
            fsbl1 = 255;
         }  
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-fscolut") == 0))
      {
         fscolut = YUP;
         Usage4=YUP;
         kar ++;
         if (kar+1 >= argc)  {
              fprintf (SUMA_STDERR, "need 2 arguments after -fscolut ");
            exit (1);
         }
         fsbl0 = atoi(argv[kar]); ++kar;
         fsbl1 = atoi(argv[kar]);
         if (fsbl0 > fsbl1 || fsbl0 < -1 || fsbl1 > 10000) {
            SUMA_S_Errv("-fscolut values of %d and %d either\n"
                        "do not make sense or exceed range 0 to 10000\n",
                        fsbl0, fsbl1);
            exit(1);
         } 
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-show_fscolut") == 0))
      {
         showfscolut = 1;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-fn") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -fn ");
            exit (1);
         }
         FidName = argv[kar];
         Usage2 = YUP;
         brk = YUP;
      }      
      
      if (!brk && (strcmp(argv[kar], "-nc") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -nc ");
            exit (1);
         }
         Ncols = atoi(argv[kar]);
         Usage1 = YUP;
         brk = YUP;
      }      
   
      if (!brk && (strcmp(argv[kar], "-ah") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -ah ");
            exit (1);
         }
         Prfx = argv[kar];
         AfniHex = 1; 
         brk = YUP;
      }      
      
      if (!brk && (strcmp(argv[kar], "-ahc") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -ahc ");
            exit (1);
         }
         Prfx = argv[kar];
         AfniHex = 2; 
         brk = YUP;
      } 
      if (!brk && (strcmp(argv[kar], "-std") == 0))
      {
         kar ++;
         if (MapSpecified) {
            fprintf (SUMA_STDERR, "Color map already specified.\n-cmap and -std are mutually exclusive\n");
            exit (1);
         }
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -std ");
            exit (1);
         }
         MapSpecified = YUP;
         StdType = argv[kar];
         Usage3 = YUP; 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-cmapdb") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -cmapdb ");
            exit (1);
         }
         SUMAg_CF->isGraphical = YUP; /* WILL NEED X DISPLAY TO RESOLVE COLOR NAMES */
         dbfile = argv[kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-cmap") ==0)) {
         if (MapSpecified) {
            fprintf (SUMA_STDERR, "Color map already specified.\n-cmap and -std are mutually exclusive\n");
            exit (1);
         }
         MapSpecified = YUP;
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need 1 arguments after -cmap ");
            exit (1);
         }
         Usage3 = YUP; 
         MapName = argv[kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-sl") == 0))
      {
         SkipLast = YUP;         
         brk = YUP;
      }      
      
      if (!brk && (strcmp(argv[kar], "-pos") == 0))
      {
         /* obsolete */
         PosMap = YUP;
         
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
   
   /* check input */
   if (  (Usage1 && (Usage2 || Usage3 || Usage4)) || 
         (Usage2 && (Usage1 || Usage3 || Usage4)) || 
         (Usage3 && (Usage1 || Usage2 || Usage4)) || 
         (Usage4 && (Usage1 || Usage2 || Usage3)) ) {
      fprintf (SUMA_STDERR,"Error %s: Mixing options from multiple usage modes.\n", FuncName);
      exit(1);
   }
   
   if (!Usage1 && !Usage2 && !Usage3 && !Usage4) {
      fprintf (SUMA_STDERR,"Error %s: One of these options must be used:\n"
                           "-f, -fn,  -std, or -fscolut.\n", FuncName);
      exit(1);
   }
   
   /* are there database files to read */
   if (dbfile) {
      SUMA_LH("Now trying to read db file");
      if (!SUMAg_CF->scm) {   
         SUMAg_CF->scm = SUMA_Build_Color_maps();
         if (!SUMAg_CF->scm) {
            SUMA_SL_Err("Failed to build color maps.\n");
            exit(1);
         }
      }
      if (SUMA_AFNI_Extract_Colors ( dbfile, SUMAg_CF->scm ) < 0) {
         fprintf (SUMA_STDERR,"Error %s: Failed to read %s colormap file.\n", FuncName, dbfile);
         exit(1);
      }
   }
   
   if (Usage1 || Usage2) {
      if (!SUMA_filexists (FidName)) {
         fprintf (SUMA_STDERR,"Error %s: File %s could not be found.\n", FuncName, FidName);
         exit(1);
      }
      
      /* read the fiducials file */
      im = mri_read_1D (FidName);
      if (!im) {
         SUMA_S_Err("Failed to read file");
         exit(1);
      }

      far = MRI_FLOAT_PTR(im);
      N_Fid = im->nx * im->ny;
   }

   if (PosMap) {
      fprintf (SUMA_STDERR,"\nWarning %s: -pos option is obsolete.\n", FuncName);
   }
   
   
   /* allocate for fiducials */
   if (Usage1) {
      if (N_Fid % 3) {
         fprintf (SUMA_STDERR,
                  "Error %s: Not all rows in %s appear to have RGB triplets.\n", 
                  FuncName, FidName);
         exit (1);
      }

      Fid = (float **) SUMA_allocate2D (N_Fid / 3, 3, sizeof(float));
      if (Fid == NULL) {
         fprintf (SUMA_STDERR,
                  "Error %s: Could not allocate for Fid.\n", FuncName);
         exit(1);
      }

      for (i=0; i < im->nx; ++i) {
         Fid[i][0] = far[i];
         Fid[i][1] = far[i+im->nx];
         Fid[i][2] = far[i+2*im->nx];
      }
      
      mri_free(im); im = NULL; 
      /* now create the color map */
      SM = SUMA_MakeColorMap (Fid, N_Fid/3, 0, Ncols, SkipLast, FuncName);
      if (SM == NULL) {
         fprintf (SUMA_STDERR,
                  "Error %s: Error in SUMA_MakeColorMap.\n", FuncName);
         exit(1);
      }
   } 
   if (Usage2) { /* second usage */
      if (N_Fid % 4) {
         fprintf (SUMA_STDERR,
                  "Error %s: Not all rows in %s appear to have "
                  "RGB N quadruplets.\n", FuncName, FidName);
         exit (1);
      }

      Fid = (float **) SUMA_allocate2D (N_Fid / 4, 3, sizeof(float));
      Nind = (int *) SUMA_calloc (N_Fid/4, sizeof(int));
      if (Fid == NULL || !Nind) {
         fprintf (SUMA_STDERR,
                  "Error %s: Could not allocate for Fid or Nind.\n", FuncName);
         exit(1);
      }
      
      for (i=0; i < im->nx; ++i) {
         Fid[i][0] = far[i];
         Fid[i][1] = far[i+im->nx];
         Fid[i][2] = far[i+2*im->nx];
         Nind[i] = (int)far[i+3*im->nx];
      }
      
      mri_free(im); im = NULL; 
      
      /* now create the color map */
      SM = SUMA_MakeColorMap_v2 (Fid, N_Fid/4, 0, Nind, SkipLast, FuncName); 
      if (SM == NULL) {
         fprintf (SUMA_STDERR,
                  "Error %s: Error in SUMA_MakeColorMap.\n", FuncName);
         exit(1);
      }
      Ncols = SM->N_M[0];
   }
   
   if (Usage3) { /* third usage */
      if (!MapName) {
         SM = SUMA_FindNamedColMap (StdType);
         freesm = 0;
         if (SM == NULL) {
            fprintf (SUMA_STDERR,
                     "Error %s: Error in SUMA_MakeColorMap.\n", FuncName);
            exit(1);
         }
         Ncols = SM->N_M[0];
      } else {
         imap = SUMA_Find_ColorMap ( MapName, SUMAg_CF->scm->CMv, 
                                     SUMAg_CF->scm->N_maps, -2);
         if (imap < 0) {
            fprintf (SUMA_STDERR,
                     "Error %s: Could not find colormap %s.\n", 
                     FuncName, MapName);
            exit (1); 
         }
         SM = SUMAg_CF->scm->CMv[imap]; 
         Ncols = SM->N_M[0];
      }
   }
   
   if (Usage4) { /* 4th usage */
      if (!(SM = SUMA_FScolutToColorMap(fscolutname, fsbl0, 
                                         fsbl1, showfscolut))) {
         SUMA_S_Err("Failed to get FreeSurfer colormap.");
         exit(1);
      }
      Ncols = SM->N_M[0];
   }
   
   if (flipud) {
      SUMA_Flip_Color_Map (SM);
   }
   
   M = SM->M;

   if (AfniHex && Ncols > 20) {
      if (!Usage4) {
         SUMA_S_Note("Writing colormap in colorscale format.\n");
      }  
   }
   
   
   
   if (!AfniHex) {
         SUMA_disp_mat (M, Ncols, 3, 1);
         /*SUMA_Show_ColorMapVec (&SM, 1, NULL, 2);*/
   } else {
         if (Usage4 || Ncols > 20) {
            if (AfniHex == 1) {
               fprintf (stdout, "%s \n", Prfx);
               for (i=0; i < Ncols; ++i) {
                  
                  /* Now create the hex form */
                  r_sprintf_long_to_hex (h, 
                        (unsigned long)rint((M[i][0]*255)), 1, 0);
                  fprintf (stdout, "#%s", h); 

                  r_sprintf_long_to_hex (h, 
                        (unsigned long)rint((M[i][1]*255)), 1, 0);
                  fprintf (stdout, "%s", h);

                  r_sprintf_long_to_hex (h, 
                        (unsigned long)rint((M[i][2]*255)), 1, 0);
                  fprintf (stdout, "%s \n", h);
               }
                fprintf (stdout, "\n") ;
            } else if (AfniHex == 2){  /* to go in the C code 
                              (see pbardef.h and pbar.c)*/
               char *p2 = SUMA_copy_string(Prfx); 
               SUMA_TO_UPPER(p2);
               fprintf (stdout, "static char %s[] = {\n   \"%s \"\n   \"", 
                  p2, Prfx); SUMA_free(p2); p2 = NULL;
               for (i=0; i < Ncols; ++i) {
                  if (i) {
                     if (!(i % 4)) { fprintf (stdout, " \"\n   \""); }
                     else { fprintf (stdout, " "); }
                  }
                  /* Now create the hex form */
                  r_sprintf_long_to_hex (h, 
                        (unsigned long)rint((M[i][0]*255)), 1, 0);
                  fprintf (stdout, "#%s", h); 

                  r_sprintf_long_to_hex (h, 
                        (unsigned long)rint((M[i][1]*255)), 1, 0);
                  fprintf (stdout, "%s", h);

                  r_sprintf_long_to_hex (h, 
                        (unsigned long)rint((M[i][2]*255)), 1, 0);
                  fprintf (stdout, "%s", h);
               }
                fprintf (stdout, " \"\n};\n") ;
            } else {
               SUMA_S_Err("AfniHex should be 0, 1, or 2\n");
               exit(1);
            }
         } else {
            fprintf (stdout, "\n***COLORS\n");

            for (i=0; i < Ncols; ++i) {
               /* Now create the hex form */
               r_sprintf_long_to_hex (h, 
                     (unsigned long)rint((M[i][0]*255)), 1, 0);
               if (i<10) fprintf (stdout, "%s_0%d = #%s", Prfx, i, h);
                  else fprintf (stdout, "%s_%d = #%s", Prfx, i, h); 

               r_sprintf_long_to_hex (h, 
                     (unsigned long)rint((M[i][1]*255)), 1, 0);
               fprintf (stdout, "%s", h);

               r_sprintf_long_to_hex (h, 
                     (unsigned long)rint((M[i][2]*255)), 1, 0);
               fprintf (stdout, "%s\n", h);
            }

            /* color map */

            fprintf (stdout, "\n***PALETTES %s [%d]\n//1 to -1 range\n", 
                     Prfx, Ncols);
            ifact = 2;
            for (i=0; i < Ncols; ++i) {
               fprintf (stdout, "%f -> ", 1.0 - (float)(ifact*i)/Ncols);
               if (i<10) fprintf (stdout, "%s_0%d\n", Prfx, i);
                  else fprintf (stdout, "%s_%d\n", Prfx, i); 
            }
            fprintf (stdout, 
                     "\n***PALETTES %s [%d+]\n//1 to 0 range\n", Prfx, Ncols);
            ifact = 1;
            for (i=0; i < Ncols; ++i) {
               fprintf (stdout, "%f -> ", 1.0 - (float)(ifact*i)/Ncols);
               if (i<10) fprintf (stdout, "%s_0%d\n", Prfx, i);
                  else fprintf (stdout, "%s_%d\n", Prfx, i); 
            }
         }
   }
   
   /* free allocated space */
   if (Usage1)  {
      if (Fid) SUMA_free2D((char **)Fid, N_Fid / 3);
   } else {
      if (Fid) SUMA_free2D((char **)Fid, N_Fid / 4);
      if (Nind) SUMA_free(Nind);
   }
   
   if (SM && !MapName && freesm) SUMA_Free_ColorMap(SM);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) { 
      SUMA_SL_Err("Failed to free commonfields."); 
   }
   
   SUMA_RETURN (0);
}   
