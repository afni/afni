/* This program is meant be run with SUMA's libraries */
#define STAND_ALONE

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
      /* need to define these global variables because function calls 
      are made to functions in files that declare these variables as extern */
      SUMA_CommonFields *SUMAg_CF;
      SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface
                                          Viewer structure*/
      SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector
                 containing the various Surface Vewer Structures SUMAg_SVv 
                 contains SUMA_MAX_SURF_VIEWERS structures */
      int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
      SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object 
                                 structure vector*/
      int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
   #endif
#endif



void usage_ConverDset()
{
   static char FuncName[]={"usage_ConverDset"};
   char *s = NULL, *sd=NULL, *sm=NULL;
   s = SUMA_help_basics();
   sd = SUMA_help_dset();
   sm = SUMA_help_mask();
   printf ( 
"Usage: \n"
"  ConvertDset -o_TYPE -input DSET [-i_TYPE] [-prefix OUT_PREF]\n"
"  Converts a surface dataset from one format to another.\n"
"  Mandatory parameters:\n"
"     -o_TYPE: TYPE of output datasets\n"
"              where TYPE is one of:\n"
"           niml_asc (or niml): for ASCII niml format.\n"
"           niml_bi:            for BINARY niml format.\n"
"           1D:                 for AFNI's 1D ascii format.\n"
"           1Dp:                like 1D but with no comments\n"
"                               or other 1D formatting gimmicks.\n"
"           1Dpt:               like 1Dp but transpose the output.\n"
"           gii:                GIFTI format default .\n"
"           gii_asc:            GIFTI format with ascii DataArrays.\n"
"           gii_b64:            GIFTI format with Base 64 encoded DataArrays.\n"
"           gii_b64gz:          GIFTI format with B64 enconding and gzipping.\n"
"         For stderr and stdout output use one of:\n"
"           1D_stderr, 1D_stdout, niml_stderr, or niml_stdout, \n"
"           1Dp_stdout, 1Dp_stderr, 1Dpt_stdout, 1Dpt_stderr\n"
"         Actually, this parameter is not that mandatory, the program\n"
"         can look at extensions on the prefix to guess the output\n"
"         format. If the prefix has no extension and o_TYPE is not\n"
"         specified, then the output format is the same as that of the\n"
"         input.\n"
"     -input DSET: Input dataset to be converted.\n"
"                  See more on input datasets below.\n"
"  Optional parameters:\n"
"     -add_node_index: Add a node index element if one does not exist\n"
"                      in the input dset. With this option, the indexing\n"
"                      is assumed to be implicit (0,1,2,3.... for rows 0,1\n"
"                      2,3,...). If that is not the case, use -node_index_1D\n"
"                      option below. \n"
"     -node_index_1D INDEX.1D: Specify file containing node indices\n"
"                              Use this to provide node indices with \n"
"                              a .1D dset. In many cases for .1D data\n"
"                              this option is DSET.1D'[0]'\n"
"     -node_select_1D MASK.1D: Specify the nodes you want to keep in the\n"
"                              output.\n" 
"     -prepend_node_index_1D: Add a node index column to the data, rather\n"
"                             than keep it as part of the metadata.\n"
"     -pad_to_node max_index: Output a full dset from node 0 \n"
"                            to node max_index (a total of \n"
"                            max_index + 1 nodes). Nodes that\n"
"                            get no value from input DSET are\n"
"                            assigned a value of 0\n"
"                            Notice that padding get done at the\n"
"                            very end.\n"
"\n"
"     -i_TYPE: TYPE of input datasets\n"
"              where TYPE is one of:\n"
"           niml: for niml data sets.\n"
"           1D:   for AFNI's 1D ascii format.\n"
"           dx: OpenDX format, expects to work on 1st\n"
"               object only.\n"
"           If no format is specified, the program will \n"
"           guess using the extension first and the file\n"
"           content next. However the latter operation might \n"
"           slow operations down considerably.\n"
"     -prefix OUT_PREF: Output prefix for data set.\n"
"                       Default is something based\n"
"                       on the input prefix.\n"
"  Notes:\n"
"     -This program will not overwrite pre-existing files.\n"  
"     -The new data set is given a new idcode.\n"
"\n"
"%s"
"\n"
"%s"
"\n"
"%s"
"\n"
"Examples:\n"
"1-   Plot a node's time series from a niml dataset:\n"
"     ConvertDset -input DemoSubj_EccCntavir.niml.dset'#5779#' \\\n"
"                 -o_1D_stdout | 1dplot -nopush -stdin \n"
"\n", sd, sm, s);
   SUMA_free(s); s = NULL; SUMA_free(sd); sd = NULL; SUMA_free(sm); sm = NULL;  
   #ifdef SUMA_COMPILED
   s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
   #endif
   fprintf (SUMA_STDOUT, "    Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov    Thu Apr  8 16:15:02 EDT 2004\n\n");
   exit(0); 
}
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ConvertDset"};
   int   kar, brk, i_input, i, j, *Ti=NULL, 
         *indexmap = NULL, add_node_index, prepend_node_index ;
   byte *Tb=NULL, *auto_nmask=NULL;
   float *fv = NULL;
   SUMA_DSET_FORMAT iform, oform;
   SUMA_DSET *dset = NULL, *dseti=NULL, *dset_m = NULL;
   char *NameOut, *prfx = NULL, *prefix = NULL;
   char *ooo=NULL, *node_index_1d = NULL, *node_mask = NULL;
   int overwrite = 0, exists = 0, N_inmask=-1, pad_to_node = -1;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   ps = SUMA_Parse_IO_Args(argc, argv, "-mask;");

   if (argc < 3) {
      usage_ConverDset  ();
      exit (1);
   }

   pad_to_node = -1;
   add_node_index = 0;
   prepend_node_index=0;
   iform = SUMA_NO_DSET_FORMAT;
   oform = SUMA_NO_DSET_FORMAT;
   overwrite = 0;
   i_input = -1;
   prfx = NULL;
   node_index_1d = NULL;
   node_mask = NULL;
   exists = 0;
   kar = 1;
   brk = NOPE;
   while (kar < argc) { /* loop accross command ine options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         usage_ConverDset  ();
         exit (1);
      }
      
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      SUMA_TO_LOWER(argv[kar]);
      
      if (SUMA_isOutputFormatFromArg(argv[kar], &oform)) {
         brk = YUP;
      } 

      
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
      
      
      if (  !brk && 
            (  strcmp(argv[kar], "-input") == 0 ||
               strcmp(argv[kar], "-i") == 0 || 
               strcmp(argv[kar], "-i_") == 0)   )
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
      
      if (!brk && (strcmp(argv[kar], "-node_index_1d") == 0))
      {
         if (kar+1 >= argc) {
            SUMA_SL_Err("Need argument after -node_index_1D");
            exit(1);
         }
         ++kar;
         node_index_1d = argv[kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-node_select_1d") == 0))
      {
         if (kar+1 >= argc) {
            SUMA_SL_Err("Need argument after -node_select_1D");
            exit(1);
         }
         ++kar;
         node_mask = argv[kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-add_node_index") == 0))
      {
         
         add_node_index = 1;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-prepend_node_index") == 0))
      {
         
         prepend_node_index = 1;
         brk = YUP;
      }
      if (  !brk && 
            (  strcmp(argv[kar], "-prefix") == 0 ||
               strcmp(argv[kar], "-o") == 0 ||  
               strcmp(argv[kar], "-o_") == 0 ) )
      {
         if (kar+1 >= argc) {
            SUMA_SL_Err("Need argument after -prefix");
            exit(1);
         }
         ++kar;
         prfx = argv[kar];
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
         fprintf (SUMA_STDERR,
            "Error %s: Option %s not understood. Try -help for usage\n",
               FuncName, argv[kar]);
         exit (1);
      } else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command ine options */
   
   if (!prfx) {
      prfx = "you_look_marvellous.niml.dset";
      overwrite = 1;
   } else {
      overwrite = THD_ok_overwrite();
   }
   pad_to_node = MRILIB_DomainMaxNodeIndex;

   if (oform == SUMA_NO_DSET_FORMAT) {
      if (prfx) {
         /* try to guess */
         oform = SUMA_GuessFormatFromExtension(prfx, argv[i_input]);
         SUMA_LHv("Guessing output format to be: %s\n", 
                  SUMA_Dset_Format_Name(oform));
         if (oform == SUMA_NO_DSET_FORMAT) {
            SUMA_SL_Err("Output format MUST be specified, or should be\n"
                        "obvious from prefix or the first input file.\n");
            exit(1);
         }
      } else {
         SUMA_S_Err("You need to specify either -o_TYPE or -prefix\n");
         exit(1);
      }
   }
   
   exists = SUMA_WriteDset_NameCheck_s (prfx, NULL, oform, 0, &ooo);
   if (exists != 0 && !overwrite) {
      SUMA_S_Errv("Output dataset %s exists.\n", ooo);
      SUMA_free(ooo); ooo=NULL;
      exit(1);
   }

   for (i=i_input; i<i_input + 1; ++i) {
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s:\n Reading %s...\n", FuncName, argv[i]); 
      dset = SUMA_LoadDset_s (argv[i], &iform, 0); 
      if (!dset) { SUMA_SL_Err(  "Failed to load dataset.\n"
                                 "Make sure file exists\n"
                                 "and is of the specified\n"
                                 "format."); exit(1); }
      if (LocalHead) {
         fprintf(SUMA_STDERR,"%s:\n Read dset of format %s\n", 
            FuncName, SUMA_Dset_Format_Name(iform));
         SUMA_ShowDset(dset, 0, NULL);
      }
      
      SUMA_LH("Checking on inel...");
      /* make sure inel is initialized*/
      if (!dset->inel || !SDSET_NODEINDLEN(dset)) { 
         SUMA_SL_Err("Bad dset->inel\nOld niml dset?"); 
         SUMA_ShowDset(dset,0, NULL); 
         SUMA_DUMP_TRACE("Bad dset->inel, dumping trace for debug:");
         SUMA_FreeDset(dset); dset = NULL; 
         SUMA_RETURN(1); 
      }

      SUMA_LH("On to node index stuff...");
      if (node_index_1d) { /* add a node index column */
         iform = SUMA_1D;
         if (!(dseti = SUMA_LoadDset_s (node_index_1d, &iform, 0))) {
            SUMA_S_Err("Failed to load node index dset");
            exit(1);
         } 
         if (SDSET_VECNUM(dseti) != 1) {
            SUMA_S_Err("Bad node index source, only one column allowed");
            exit(1);
         }
         if (SDSET_VECFILLED(dseti) != SDSET_VECFILLED(dset)) {
            SUMA_S_Err(
               "mismatch in number of values in index source and dataset");
            exit(1);
         } 
         Ti = (int *) SUMA_calloc(SDSET_VECFILLED(dseti), sizeof(int));
         fv = (float *)dseti->dnel->vec[0];
         for (j=0; j<SDSET_VECFILLED(dseti); ++j) {
            Ti[j] = (int)fv[j];
         }
         if (!SUMA_AddDsetNelCol (  dset, "Node Index", 
                                    SUMA_NODE_INDEX, (void *)Ti, NULL, 1)) {
            SUMA_SL_Err("Failed to add column");
            if (Ti) SUMA_free(Ti); Ti = NULL;
            exit(1);
         }
         SUMA_free(Ti); Ti = NULL; 
         SUMA_FreeDset(dseti); dseti = NULL;
      }
      
      if (add_node_index) {
         if (!SUMA_PopulateDsetNodeIndexNel(dset, 1)) {
            SUMA_S_Err("Failed to add node index column");
            exit(1);
         }
      }
       
      SUMA_LHv("On to auto_nmask ...%p %p %p\n", 
               ps->bmaskname,ps->nmaskname,ps->cmask);
      if (!(auto_nmask = 
               SUMA_load_all_command_masks(  ps->bmaskname, 
                                             ps->nmaskname, 
                                             ps->cmask, 
                                             SDSET_VECFILLED(dset), 
                                             &N_inmask)) 
            && N_inmask < 0) {
            SUMA_S_Err("Failed loading mask");
            exit(1);
      }
      if (auto_nmask) { /* mask input here */
         SUMA_LH("Masking here ...");
         if (!(dset_m = SUMA_MaskedCopyofDset(dset, auto_nmask, NULL, 1, 0))){
            SUMA_S_Err("Failed to mask dset by mask options\n");
            exit(1);
         }
         SUMA_FreeDset(dset); dset = NULL;
         dset = dset_m;  dset_m = NULL;       
      }
      
      SUMA_LH("On to node_mask ...");
      if (node_mask) { /* mask dataset */
         iform = SUMA_1D;
         if (!(dseti = SUMA_LoadDset_s (node_mask, &iform, 0))) {
            SUMA_S_Err("Failed to load node_selection dset");
            exit(1);
         } 
         if (SDSET_VECNUM(dseti) != 1) {
            SUMA_S_Err("Bad node index source, only one column allowed");
            exit(1);
         }
         
         Ti = (int *) SUMA_calloc(SDSET_VECFILLED(dseti), sizeof(int));
         fv = (float *)dseti->dnel->vec[0];
         for (j=0; j<SDSET_VECFILLED(dseti); ++j) Ti[j] = (int)fv[j];
         if (!(dset_m = SUMA_MaskedByNodeIndexCopyofDset(
                  dset, Ti, SDSET_VECFILLED(dseti),  NULL, 1, 0))) {
            SUMA_S_Err("Failed to mask dset by node indices\n");
            SUMA_S_Note(   "If your input dataset did not have a node index \n"
                           "explicitly defined, use -add_node_index or\n"
                           "-node_index_1D options to specify node indices.\n" )
            exit(1);
         }
         
         SUMA_free(Ti); Ti = NULL; 
         SUMA_free(indexmap); indexmap = NULL;
         SUMA_FreeDset(dseti); dseti = NULL;         
         SUMA_FreeDset(dset); dset = NULL;
         dset = dset_m;  dset_m = NULL;       
      }

      
      if (pad_to_node >= 0) {
         dset_m = SUMA_PaddedCopyofDset ( dset, pad_to_node );
         SUMA_FreeDset(dset); dset = NULL;
         dset = dset_m; dset_m = NULL;       
      }
      
      SUMA_LH("On to prefix ...");
      
      if (!prfx) {
         /* don't use iform because some 1Ds are NIML compatible and they get
         read-in as such unless you specifically order otherwise. */
         prefix = SUMA_RemoveDsetExtension_s(argv[i], SUMA_NO_DSET_FORMAT);
      } else { 
         prefix = SUMA_copy_string(prfx); 
      }
      
      /* set a new ID for the dset */
      SUMA_NEWDSET_ID_LABEL_HIST(dset, prefix) ;
      
      
      if (prepend_node_index) {/* prepend node index? */         
         if (!SUMA_InsertDsetNelCol (  dset, "Node Index Copy", 
                                       SUMA_NODE_INT, 
                                       (void *)(dset->inel->vec[0]), 
                                       NULL ,1, 0)) {
            SUMA_S_Err("Failed to insert column");
         }
         if (LocalHead) SUMA_ShowDset(dset,0, NULL); 
      }
      SUMA_LHv("About to write dset to %s\n", prefix);
      NameOut = SUMA_WriteDset_s (prefix, dset, oform, overwrite, 0);
      
      
      if (!NameOut && !SUMA_IS_DSET_STDXXX_FORMAT(oform)) { 
         SUMA_SL_Err("Failed to write dataset."); exit(1); 
      } 
      if (prefix) SUMA_free(prefix); prefix = NULL;    
      if (dset) SUMA_FreeDset((void *)dset); dset = NULL;
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
   }
   
	SUMA_RETURN(0);
}    
