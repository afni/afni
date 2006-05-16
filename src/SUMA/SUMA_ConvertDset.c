/* This program is meant be run with SUMA's libraries */
#define STAND_ALONE
#define SUMA_Test_DSET_IO_STANDALONE

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "mrilib.h"
#include "niml.h"
#include "../niml/niml_private.h"
#include "xutil.h"


#include "SUMA_suma.h"

#ifdef STAND_ALONE
   #if defined SUMA_COMPILED
      /* need to define these global variables because function calls are made to functions in files that declare these variables as extern */
      SUMA_CommonFields *SUMAg_CF;
      SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
      SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                          SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
      int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
      SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
      int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
   #endif
#endif



#ifdef SUMA_ConvertDset_STANDALONE
void usage_ConverDset()
{
   static char FuncName[]={"usage_ConverDset"};
   char *s = NULL;
   s = SUMA_help_basics();
   printf ( "Usage: \n"
            "  ConvertDset -o_TYPE -input DSET [-i_TYPE] [-prefix OUT_PREF]\n"
            "  Converts a surface dataset from one format to another.\n"
            "  Mandatory parameters:\n"
            "     -o_TYPE: TYPE of output datasets\n"
            "              where TYPE is one of:\n"
            "           niml_asc (or niml): for ASCII niml format.\n"
            "           niml_bi:            for BINARY niml format.\n"
            "           1D:                 for AFNI's 1D ascii format.\n"
            "     -input DSET: Input dataset to be converted.\n"
            "  Optional parameters:\n"
            "     -i_TYPE: TYPE of input datasets\n"
            "              where TYPE is one of:\n"
            "           niml: for niml data sets.\n"
            "           1D:   for AFNI's 1D ascii format.\n"
            "           1Dp:  like 1D but with no comments\n"
            "                 or other 1D formatting gimmicks.\n"
            "           dx: OpenDX format, expects to work on 1st\n"
            "               object only.\n"
            "           If no format is specified, the program will \n"
            "           guess however that might slow \n"
            "           operations down considerably.\n"
            "     -prefix OUT_PREF: Output prefix for data set.\n"
            "                       Default is something based\n"
            "                       on the input prefix.\n"
            "  Notes:\n"
            "     -This program will not overwrite pre-existing files.\n"  
            "     -The new data set is given a new idcode.\n"
            "%s"
            "\n", s);
   SUMA_free(s); s = NULL;
   #ifdef SUMA_COMPILED
   s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
   #endif
   fprintf (SUMA_STDOUT, "    Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov    Thu Apr  8 16:15:02 EDT 2004\n\n");
   exit(0); 
}
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ConvertDset"};
   int kar, brk, i_input, i;
   SUMA_DSET_FORMAT iform, oform;
   SUMA_DSET *dset = NULL;
   char *NameOut, *prfx = NULL, *prefix = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_mainENTRY;
   
   SUMA_STANDALONE_INIT;

   if (argc < 3) {
      usage_ConverDset  ();
      exit (1);
   }

   iform = SUMA_NO_DSET_FORMAT;
   oform = SUMA_NO_DSET_FORMAT;
   i_input = -1;
   prfx = NULL;
   kar = 1;
   brk = NOPE;
   while (kar < argc) { /* loop accross command ine options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         usage_ConverDset  ();
         exit (1);
      }
      
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      SUMA_TO_LOWER(argv[kar]);
      if (!brk && (strcmp(argv[kar], "-i_1d") == 0))
      {
         if (iform != SUMA_NO_DSET_FORMAT) {
            SUMA_SL_Err("input type already specified.");
            exit(1);
         }
         iform = SUMA_1D;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-i_niml") == 0))
      {
         if (iform != SUMA_NO_DSET_FORMAT) {
            SUMA_SL_Err("input type already specified.");
            exit(1);
         }
         iform = SUMA_NIML;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-i_dx") == 0))
      {
         if (iform != SUMA_NO_DSET_FORMAT) {
            SUMA_SL_Err("input type already specified.");
            exit(1);
         }
         iform = SUMA_ASCII_OPEN_DX_DSET;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-o_1d") == 0))
      {
         if (oform != SUMA_NO_DSET_FORMAT) {
            SUMA_SL_Err("output type already specified.");
            exit(1);
         }
         oform = SUMA_1D;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-o_1dp") == 0))
      {
         if (oform != SUMA_NO_DSET_FORMAT) {
            SUMA_SL_Err("output type already specified.");
            exit(1);
         }
         oform = SUMA_1D_PURE;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-o_niml") == 0))
      {
         if (oform != SUMA_NO_DSET_FORMAT) {
            SUMA_SL_Err("output type already specified.");
            exit(1);
         }
         
         oform = SUMA_ASCII_NIML;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-o_niml_asc") == 0))
      {
         if (oform != SUMA_NO_DSET_FORMAT) {
            SUMA_SL_Err("output type already specified.");
            exit(1);
         }
         
         oform = SUMA_ASCII_NIML;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-o_niml_bi") == 0))
      {
         if (oform != SUMA_NO_DSET_FORMAT) {
            SUMA_SL_Err("output type already specified.");
            exit(1);
         }
         
         oform = SUMA_BINARY_NIML;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-input") == 0))
      {
         if (kar+1 >= argc) {
            SUMA_SL_Err("Need argument after -input");
            exit(1);
         }
         if (i_input >= 0) {
            SUMA_SL_Err("-input already specified.");
            exit(1);
         }
         i_input = kar+1;
         ++kar;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0))
      {
         if (kar+1 >= argc) {
            SUMA_SL_Err("Need argument after -prefix");
            exit(1);
         }
         ++kar;
         prfx = argv[kar];
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
    
   if (oform == SUMA_NO_DSET_FORMAT) {
      SUMA_SL_Err("Output format MUST be specified");
      exit(1);
   }

   for (i=i_input; i<i_input + 1; ++i) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Reading %s...\n", FuncName, argv[i]); 
      dset = SUMA_LoadDset (argv[i], &iform, 0); 
      if (!dset) { SUMA_SL_Err(  "Failed to load dataset.\n"
                                 "Make sure file exists\n"
                                 "and is of the specified\n"
                                 "format."); exit(1); }
      if (!prfx) {
         /* don't use iform because some 1Ds are NIML compatible and they get
         read-in as such unless you specifically order otherwise. */
         prefix = SUMA_RemoveDsetExtension(argv[i], SUMA_NO_DSET_FORMAT);
      } else { 
         prefix = SUMA_copy_string(prfx); 
      }
      
      /* set a new ID for the dset */
      SUMA_NewDsetID (dset); 
      
      NameOut = SUMA_WriteDset (prefix, dset, oform, 0, 0);
      if (!NameOut) { SUMA_SL_Err("Failed to write dataset."); exit(1); } 
      if (prefix) SUMA_free(prefix); prefix = NULL;    
      if (dset) SUMA_FreeDset((void *)dset); dset = NULL;
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
   }
   
	SUMA_RETURN(0);
}    
#endif
