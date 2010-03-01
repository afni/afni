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
"                 [-FScmaprange iMin iMax]\n"
"                 [-FSversion VER]\n"
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
"                       With FSversion set to 2009, if FScmap is not set, \n"
"                       the program will attempt to locate \n"
"                       FreeSurferColorLUT.txt based on the environment\n"
"                       variable $FREESURFER_HOME\n"
"                       You can use FS_DEFAULT to force the program to load\n"
"                       FreeSurfer's $FREESURFER_HOME/FreeSurferColorLUT.txt\n"
"     -FScmaprange iMin iMax: CMAPFILE contains multiple types of labels\n"
"                       The annotation values in ANNOTFILE can map to multiple\n"
"                       labels if you do not restrict the range with \n"
"                       iMin and iMax. That is because annotation values\n"
"                       encode color in RGB which is used to lookup a name\n"
"                       and integer label from CMAPFILE. The same color is \n"
"                       used for multiple labels.\n"
"                       When an external CMAPFILE is needed (a2009 versions), \n"
"                       the programs uses a default of [13100 13199] for lh,\n"
"                       [14100 14199] for rh surfaces.\n"
"                       If CMAPFILE is set to FS_DEFAULT in a2005 versions,\n"
"                       the programs uses a default of [3100 3199] for lh,\n"
"                       [4100 4199] for rh surfaces.\n"
"     -FSversion VER: VER is the annotation file vintage. Choose from 2009, \n"
"                     or 2005. The program will attempt to guess from the name\n"
"                     ANNOTFILE and would default to 2005.\n"
"     -hemi HEMI: Specify hemisphere. HEMI is one of lh or rh.\n"
"                 Program guesses by default\n"
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
"     -dset annot.niml.dset: Write the annotation and colormap as a \n"
"                            niml formatted Label Dset. This type of dset\n"
"                            gets special treatement in SUMA.\n"
"     -cmap_1D annot.1D.cmap: Write a 4-column 1D color map file.\n"
"                             The first column is the color index,\n"
"                             followed by r g b and flag values.\n"
"                             The name of each color is inserted\n"
"                             as a comment because 1D files do not\n"
"                             support text data.\n"
"     -show_FScmap: Show the info of the colormap in the ANNOT file.\n"
"     -dset DSET: Write out a niml formatted label dataset which is handled\n"
"                   in a special way in SUMA. If AFNI_NIML_TEXT_DATA is set\n"
"                   to YES, then output is ASCII NIML.\n"
"\n"        
                  "\n");
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
       exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"FSread_annot"};
   int kar, Showct, testmode;
   char *fname = NULL, *fcmap = NULL, *fdset = NULL, 
         *froi = NULL, *fcol = NULL, *ctfile=NULL, sbuf[1024]={""};
   SUMA_Boolean SkipCoords = NOPE, brk;
   SUMA_DSET *dset=NULL;
   int lbl1,lbl2, ver, hemi, FSdefault;
   SUMA_Boolean LocalHead = NOPE;	
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   
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
   testmode = 0;
   lbl1 = -1;
   lbl2 = -1;
   ver = -1;
   FSdefault = 0;
   hemi=0;
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
      
      if (!brk && (strcmp(argv[kar], "-testmode") == 0)) {
         testmode = 1;
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
         if (!strcmp(ctfile,"FS_DEFAULT")) {
            char *eee = getenv("FREESURFER_HOME");
            if (!eee) {
               SUMA_S_Err("Environment variable FREESURFER_HOME not set.\n"
                          "Cannot locate FreeSurferColorLUT.txt\n");
               exit (1);
            } else {
               sprintf(sbuf, "%s/FreeSurferColorLUT.txt", eee);
               ctfile = sbuf;
               FSdefault = 1;
               SUMA_S_Notev("Using %s\n", ctfile);
            }              
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-FSversion") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -FSversion\n");
				exit (1);
			}
         if (strstr(argv[kar],"2009")) ver = 2009;
         else if (strstr(argv[kar],"2005")) ver = 2005;
         else {
            fprintf (SUMA_STDERR, 
                  "Bad value for -FSversion of %s (looking for 2005 or 2009)\n",
                  argv[kar]);
				exit (1);
         }
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-hemi") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -hemi\n");
				exit (1);
			}
         if (strstr(argv[kar],"lh")) hemi = -1;
         else if (strstr(argv[kar],"rh")) hemi = 1;
         else {
            fprintf (SUMA_STDERR, 
                  "Bad value for -hemi of %s (looking for lh or rh)\n",
                  argv[kar]);
				exit (1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-FScmaprange") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -FScmaprange\n");
				exit (1);
			}
         lbl1 = atoi(argv[kar]); ++kar;
         lbl2 = atoi(argv[kar]); 
         
         if (lbl1 > lbl2 || lbl1 < -1) {
            fprintf (SUMA_STDERR, 
                  "Bad value for -FScmaprange of [%d %d]\n",
                  lbl1, lbl2);
				exit (1);
         }
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
      
      if (!brk && ( (strcmp(argv[kar], "-prefix") == 0) ||
                    (strcmp(argv[kar], "-dset") == 0) ) ) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -dset\n");
				exit (1);
			}
         fdset = argv[kar];
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
   
   if (!fname) {
      SUMA_SL_Err("No input file specified.");
      exit(1);
   }
   
   if (ver == -1) {
      /* guess at version */
      if (strstr(fname,"2009")) {
         ver = 2009;
         SUMA_S_Notev("Guessed FS annot version of %d\n", ver);
      } else if (strstr(fname,"2005")) {
         ver = 2005;
         SUMA_S_Notev("Guessed FS annot version of %d\n", ver);
      } else {
         SUMA_S_Notev("Assuming FS annot version of %d\n", ver);
      }
   }
   
   if (hemi == 0) {
      if (strstr(fname,"lh.")) {
         hemi = -1;
         SUMA_S_Note("Guessed left hemisphere");
      } else if (strstr(fname,"rh.")) {
         SUMA_S_Note("Guessed right hemisphere");
         hemi = 1;
      } else {
         if (ver == 2009) {
            hemi = -1;
            SUMA_S_Note("Assuming left hemisphere.\n");
         } else {
            /* leave it not set */
         }
      }
   }
   
   if (ver == 2009 && !ctfile) {
      char *eee = getenv("FREESURFER_HOME");
      if (!eee) {
         SUMA_S_Warn("Environment variable FREESURFER_HOME not set.\n"
                    "Cannot locate FreeSurferColorLUT.txt\n");
      } else {
         sprintf(sbuf, "%s/FreeSurferColorLUT.txt", eee);
         ctfile = sbuf;
         SUMA_S_Notev("Using %s\n", ctfile);
      }  
   }
   
   if (lbl1 < 0 && lbl2 < 0) {
      /* need some setup */
      if (ver == 2009) {
         if (hemi == -1) {
            lbl1 = 13100;
            lbl2 = 13199;
            SUMA_S_Notev("Setting -FScmaprange to [%d %d]\n",
                        lbl1, lbl2);
         } else if (hemi == 1) {
            lbl1 = 14100;
            lbl2 = 14199;
            SUMA_S_Notev("Setting -FScmaprange to [%d %d]\n",
                        lbl1, lbl2);
         } else {
            SUMA_S_Warn("-FScmaprange is not set.\n"
                         "You may need to set it, check results.\n");
         }
      } else if (ver == 2005 && FSdefault) {
         if (hemi == -1) {
            lbl1 = 3100;
            lbl2 = 3199;
            SUMA_S_Notev("Setting -FScmaprange to [%d %d]\n",
                        lbl1, lbl2);
         } else if (hemi == 1) {
            lbl1 = 4100;
            lbl2 = 4199;
            SUMA_S_Notev("Setting -FScmaprange to [%d %d]\n",
                        lbl1, lbl2);
         } else {
            SUMA_S_Warn("-FScmaprange is not set.\n"
                         "You may need to set it, check results.\n");
         }
      }
   }
   
   if (!fcmap && !froi && !fcol && !Showct && !fdset) {
      SUMA_SL_Err("Nothing to do.\n"
                  "Use either -cmap_1D or \n"
                  " -roi_1D or -col_1D or \n"
                  " -show_FScmap options.");
      exit(1);
   }

   if (fdset) {
      int exists = 0;
      char *ooo=NULL;
      exists = SUMA_WriteDset_NameCheck_s (fdset, NULL, 
                                           SUMA_ASCII_NIML, 0, &ooo);
      if (exists != 0 && !THD_ok_overwrite()) {
         SUMA_S_Errv("Output dataset %s exists.\n", ooo);
         SUMA_free(ooo); ooo=NULL;
         exit(1);
      }
   }

   if (froi) {
      if (SUMA_filexists(froi) && !THD_ok_overwrite()) { 
         fprintf( SUMA_STDERR,
                  "Error %s: File %s exists, will not overwrite.\n", 
                  FuncName, froi);
	      SUMA_RETURN (NOPE);
      }
   }
     
   if (fcmap) {
      if (SUMA_filexists(fcmap) && !THD_ok_overwrite()) { 
         fprintf( SUMA_STDERR,
                  "Error %s: File %s exists, will not overwrite.\n", 
                  FuncName, fcmap);
	      SUMA_RETURN (NOPE);
      }
   }
     
   if (fcol) {
      if (SUMA_filexists(fcol) && !THD_ok_overwrite()) { 
         fprintf( SUMA_STDERR,
                  "Error %s: File %s exists, will not overwrite.\n", 
                  FuncName, fcol);
	      SUMA_RETURN (NOPE);
      }
   }
     
   if (!SUMA_readFSannot (fname, froi, fcmap, fcol, Showct, ctfile, 
                          lbl1, lbl2, &dset)) {
      SUMA_S_Err("Failed reading annotation file (or output file exists)");
      exit(1);
   }
   
   if (!dset && fdset) {
      SUMA_S_Err("Have no dset to write");
      exit(1);
   }
   
   if (fdset) {
      if (AFNI_yesenv("AFNI_NIML_TEXT_DATA")) {
         SUMA_WriteDset_eng(fdset, dset, SUMA_ASCII_NIML, 1, 1);
      } else {
         SUMA_WriteDset_eng(fdset, dset, SUMA_BINARY_NIML, 1, 1);      
      }
   }
   
   if (testmode) {
      int key, indx, ism, suc;
      SUMA_COLOR_MAP *SM2=NULL, *SM=NULL;
      char *s=NULL, stmp[256];
      SUMA_PARSED_NAME *sname=NULL;
      NI_group *ngr=NULL;
      
      SUMA_S_Note("Testing Chunk Begins");
      
      /* check */
      if (!SUMA_is_Label_dset(dset, &ngr)) {
         SUMA_S_Err("Dset is no label dset");
         exit(1);
      }
      /* write it */
      
      /* play with the colormap */
      if (ngr) {
         if (!(SM = SUMA_NICmapToCmap(ngr))){
            SUMA_S_Err("Failed to create SUMA colormap");
            exit(1);
         }
         ngr = NULL; /* that's a copy of what was in dset, do not free it */
         if (!SUMA_CreateCmapHash(SM)) {
            SUMA_S_Err("Failed to create hash");
            exit(1);
         }
         /* Now pretend you are retrieving the index in cmap of some key */
         for (ism=0; ism < SM->N_M[0]; ++ism) {
            /* the key is coming from SM, because I store all keys there
              But key normally comes from a certain node's value */
            key = SM->idvec[ism];
            indx = SUMA_ColMapKeyIndex(key, SM);
            if (indx < 0) {
               SUMA_S_Errv("Hashkey %d not found\n", key);
            } else {
               fprintf(SUMA_STDERR,
                        "hashed id %d --> index %d\n"
                        "known  id %d --> index %d\n",
                        key, indx,
                        key, ism);
            }
         }

         /* Now try it with an unknown key */
         key = -13;
         indx = SUMA_ColMapKeyIndex(key, SM);
         if (indx < 0) {
            fprintf(SUMA_STDERR,
                     "id %d is not in the hash table, as expected\n", key);
         } else {
            SUMA_S_Errv("Should not have found %d\n", key);
         }      

         SUMA_S_Note("Now Show it to me");
         s = SUMA_ColorMapVec_Info (&SM, 1, 2);
         if (s) {
            fprintf(SUMA_STDERR,"%s", s); SUMA_free(s); s = NULL;
         }

         SUMA_S_Notev("Now turn it to niml (%s)\n", SM->Name);
         ngr = SUMA_CmapToNICmap(SM);
         sname = SUMA_ParseFname(SM->Name, NULL);
         snprintf(stmp, 128*sizeof(char), 
                  "file:%s.niml.cmap", sname->FileName_NoExt); 
         NEL_WRITE_TX(ngr, stmp, suc);
         if (!suc) {
            SUMA_S_Errv("Failed to write %s\n", stmp);
         }
         SUMA_Free_Parsed_Name(sname); sname = NULL;

         SUMA_S_Note("Now turn niml colormap to SUMA's colormap");
         SM2 = SUMA_NICmapToCmap(ngr);
         SUMA_S_Note("Now Show it to me2");
         s = SUMA_ColorMapVec_Info (&SM2, 1, 2);
         if (s) {
            fprintf(SUMA_STDERR,"%s", s); SUMA_free(s); s = NULL;
         }

         NI_free(ngr); ngr=NULL;
         SUMA_Free_ColorMap(SM); SM = NULL;
         SUMA_Free_ColorMap(SM2); SM2 = NULL;
      }
      
      SUMA_S_Note("Testing Chunk End");
   }

   if (dset) SUMA_FreeDset(dset); dset = NULL;
   
   exit(0);
}
