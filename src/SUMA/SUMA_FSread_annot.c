#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */
 

void usage_SUMA_FSread_annot_Main ()
   
  {/*Usage*/
      static char FuncName[]={"usage_SUMA_FSread_annot_Main"};
      char * s = NULL;
          printf (
"\n"
"Usage:  \n"
"  FSread_annot   <-input ANNOTFILE>  \n"
"                 [-FScmap CMAPFILE]   \n"
"                 [-col_1D annot.1D.col]  \n"
"                 [-roi_1D annot.1D.roi] \n"
"                 [-cmap_1D annot.1D.cmap]\n"
"                 [show_FScmap]\n"
"                 [-help]  \n"
"  Reads a FreeSurfer annotaion file and outputs\n"
"  an equivalent ROI file and/or a colormap file \n"
"  for use with SUMA.\n"
"\n"
"  Required options:\n"
"     -input ANNOTFILE: Binary formatted FreeSurfer\n"
"                       annotation file.\n"
"     AND one of the optional options.\n"
"  Optional options:\n"
"     -FScmap CMAPFILE: Get the colormap from the Freesurfer \n"
"                       colormap file CMAPFILE.\n"
"                       Colormaps inside the ANNOTFILE would be\n"
"                       ignored. See also MakeColorMap's fscolut* options.\n"
"     -col_1D annot.1D.col: Write a 4-column 1D color file. \n"
"                           The first column is the node\n"
"                           index followed by r g b values.\n"
"                           This color file can be imported \n"
"                           using the 'c' option in SUMA.\n"
"                           If no colormap was found in the\n"
"                           ANNOTFILE then the file has 2 columns\n"
"                           with the second being the annotation\n"
"                           value.\n"
"     -roi_1D annot.1D.roi: Write a 5-column 1D roi file.\n"
"                           The first column is the node\n"
"                           index, followed by its index in the\n"
"                           colormap, followed by r g b values.\n"
"                           This roi file can be imported \n"
"                           using the 'Load' button in SUMA's\n"
"                           'Draw ROI' controller.\n"
"                           If no colormap was found in the\n"
"                           ANNOTFILE then the file has 2 columns\n"
"                           with the second being the annotation\n"
"                           value. \n"
"     -cmap_1D annot.1D.cmap: Write a 4-column 1D color map file.\n"
"                             The first column is the color index,\n"
"                             followed by r g b and flag values.\n"
"                             The name of each color is inserted\n"
"                             as a comment because 1D files do not\n"
"                             support text data.\n"
"     -show_FScmap: Show the info of the colormap in the ANNOT file.\n"
"\n"        
                  "\n");
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
       exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"FSread_annot"};
   int kar, Showct;
   char *fname = NULL, *fcmap = NULL, *froi = NULL, *fcol = NULL, *ctfile=NULL;
   SUMA_Boolean SkipCoords = NOPE, brk;
   SUMA_Boolean LocalHead = NOPE;	
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf( SUMA_STDERR,
               "Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   
   /* parse command line */
   kar = 1;
   fname = NULL;
   froi = NULL;
   fcmap = NULL;
   fcol = NULL;
	brk = NOPE;
   ctfile = NULL;
   Showct = 0;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_FSread_annot_Main();
          exit (0);
		}
      
      if (!brk && (strcmp(argv[kar], "-show_FScmap") == 0)) {
         Showct = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input\n");
				exit (1);
			}
         fname = argv[kar];
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-FScmap") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -FScmap\n");
				exit (1);
			}
         ctfile = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-roi_1D") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -ROI_1D\n");
				exit (1);
			}
         froi = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-cmap_1D") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -cmap_1D\n");
				exit (1);
			}
         fcmap = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-col_1D") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -col_1D\n");
				exit (1);
			}
         fcol = argv[kar];
			brk = YUP;
		}
      
      if (!brk) {
			fprintf (SUMA_STDERR,
                  "Error %s:\n"
                  "Option %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   if (!fcmap && !froi && !fcol && !Showct) {
      SUMA_SL_Err("Nothing to do.\n"
                  "Use either -cmap_1D or \n"
                  " -roi_1D or -col_1D or \n"
                  " -show_FScmap options.");
      exit(1);
   }
   if (!fname) {
      SUMA_SL_Err("No input file specified.");
      exit(1);
   }
   
   SUMA_readFSannot (fname, froi, fcmap, fcol, Showct, ctfile);
   
   exit(0);
}
