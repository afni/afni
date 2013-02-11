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

void usage_ConverDset(SUMA_GENERIC_ARGV_PARSE *ps, int detail)
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
"\n%s", detail ? "":"use -h or -help for more help detail.\n");
   if (detail) {
      printf ( 
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
"             ** NOTE: It is highly recommended you use one of -node_index_1D\n"
"                       or -add_node_index when going from 1D format to NIML \n"
"                       GIFTI formats.\n"
"     -node_index_1D INDEX.1D: Specify file containing node indices\n"
"                              Use this to provide node indices with \n"
"                              a .1D dset. In many cases for .1D data\n"
"                              this option is DSET.1D'[0]'\n"
"     -node_select_1D MASK.1D: Specify the nodes you want to keep in the\n"
"                              output. \n"
"                              The order of the rows in the output dataset \n"
"                              reflect the order of the nodes in MASK.1D.\n"
"                              Note that the presence of duplicate nodes in\n"
"                              MASK.1D is not allowed. Also, node indices \n"
"                              that do not have data in the input dataset will\n"
"                              be ignored.\n" 
"     -prepend_node_index_1D: Add a node index column to the data, rather\n"
"                             than keep it as part of the metadata.\n"
"     -pad_to_node MAX_INDEX: Output a full dset from node 0 \n"
"                            to node MAX_INDEX (a total of \n"
"                            MAX_INDEX + 1 nodes). Nodes that\n"
"                            get no value from input DSET are\n"
"                            assigned a value of 0\n"
"                            If MAX_INDEX is set to 0 it means you want\n"
"                            to pad the maximum node in the input dataset.\n"
"             ** Notice that padding gets done at the very end.\n"
"             ** Instead of directly setting MAX_INDEX to an integer you \n"
"                can set MAX_INDEX to something like:\n"
"             ld120 (or rd17) which sets MAX_INDEX to be the maximum \n"
"                node index on an Icosahedron with -ld 120. See \n"
"                CreateIcosahedron for details.\n"
"             d:DSET.niml.dset which sets MAX_INDEX to the maximum node found\n"
"                      in dataset DSET.niml.dset.\n"       
"\n"
"     -labelize CMAP: Turn the dataset into a labeled set per the colormap in\n"
"                     CMAP. A CMAP can easily be generated with MakeColorMap's\n"
"                     options -usercolorlutfile and -suma_cmap.\n"
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
"     -split N: Split a multi-column dataset into about N output datasets\n"
"               with all having the same number of columns, except perhaps\n"
"               for the last one. Confused? try:\n"
"               ConvertDset -i v2s.lh.TS.niml.dset -split 3 \\\n"
"                           -prefix Split3\n"
"               3dinfo -n4 -label Split3.000* v2s.lh.TS.niml.dset\\\n"
"     -no_history: Do not include a history element in the output\n"
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
"\n"
"2-   Change a dataset to a labeled dataset using the colormap generated \n"
"     in Example 5 of MakeColorMap's help\n"
"        ConvertDset -i you_look_marvellous.niml.dset \\\n"
"                    -o you_look_labeled.niml.dset -labelize toylut.niml.cmap\n"
"     The advantage of having a labeled dataset is that you can see the label \n"
"     of a node when you click on it in SUMA, and you can extract\n"
"     regions based on their labels. For example, with the dataset created\n"
"     above you can run the following command to extract a mask of the  \n"
"     nodes labeled 'Small_Face' with something like:\n"
"        3dcalc -a you_look_labeled.niml.dset'<Small_Face>' \\\n"
"               -expr 'a' -prefix Small_Face.only\n"
"     This assumes of course that your colormap toylut.niml.cmap does have \n"
"     an entry labeled 'Small_Face'\n"
"\n"
"\n"
"\n",(detail > 1) ? sd : "Use -help for more detail.",
     (detail > 1) ? sm : "",
     (detail > 1) ? s : "");
      }
   SUMA_free(s); s = NULL; SUMA_free(sd); sd = NULL; SUMA_free(sm); sm = NULL;  
   if (detail) {
      #ifdef SUMA_COMPILED
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      #endif
      fprintf (SUMA_STDOUT, 
         "    Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov\n\n");
   }
   return; 
}
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ConvertDset"};
   int   kar, brk, i_input, i, j, *Ti=NULL, 
         *indexmap = NULL, add_node_index, prepend_node_index,
         no_hist ;
   byte *Tb=NULL, *auto_nmask=NULL;
   float *fv = NULL;
   SUMA_DSET_FORMAT iform, oform;
   SUMA_DSET *dset = NULL, *dseti=NULL, *dset_m = NULL;
   char *NameOut, *prfx = NULL, *prefix = NULL, *cmapfile;
   char *ooo=NULL, *node_index_1d = NULL, *node_mask = NULL;
   int overwrite = 0, exists = 0, N_inmask=-1, pad_to_node = -1;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   int orderednodelist = 1, split=0;
   SUMA_COLOR_MAP *SM=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   ps = SUMA_Parse_IO_Args(argc, argv, "-mask;");

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
   split = 0;
   no_hist = 0;
   cmapfile=NULL;
   kar = 1;
   brk = NOPE;
   while (kar < argc) { /* loop accross command ine options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         usage_ConverDset  (ps, strlen(argv[kar]) > 3 ? 2:1);
         exit (0);
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

      if (!brk && (strcmp(argv[kar], "-no_history") == 0))
      {
         no_hist = 1;
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

      if (!brk && (strcmp(argv[kar], "-labelize") == 0))
      {
         if (kar+1 >= argc) {
            SUMA_SL_Err("Need colrmap after -labelize");
            exit(1);
         }
         ++kar;
         cmapfile = argv[kar];
         if (!(SM = SUMA_LoadCmapFile_eng(cmapfile))) {
            SUMA_S_Errv("Failed to load cmap file %s\n", cmapfile);
            exit(1);
         }
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
      if (!brk && (strcmp(argv[kar], "-prepend_node_index_1d") == 0))
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
      
      if (!brk && (strcmp(argv[kar], "-split") == 0))
      {
         if (kar+1 >= argc) {
            SUMA_SL_Err("Need positive integer after -split");
            exit(1);
         }
         ++kar;
         split = atoi(argv[kar]);
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
         fprintf (SUMA_STDERR,
            "Error %s: Option %s not understood. Try -help for usage\n",
               FuncName, argv[kar]);
         suggest_best_prog_option(argv[0], argv[kar]);
         exit (1);
      } else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command ine options */
   if (argc < 3) {
      usage_ConverDset  (ps, 0);
      exit (1);
   }


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
             
      if (pad_to_node == 0) {
         DSET_MAX_NODE_INDEX(dset, pad_to_node);
         if (pad_to_node < 0) {
            SUMA_S_Err( "Failed to get max node index in input dset.\n" 
                  "Cannot set pad_to_node automatically\n");   
            exit(1);
         }
      }
      if (pad_to_node > 0) {
         SUMA_S_Notev("Padding output dset until node %d\n", pad_to_node);
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
         if (orderednodelist) {
            int *inlu=NULL, N_inlu=0;
            /* make sure indexlist is unique */
            inlu = SUMA_UniqueInt(Ti, SDSET_VECFILLED(dseti), 
                                  &N_inlu, 0);
            SUMA_free(inlu); inlu = NULL;
            if (N_inlu != SDSET_VECFILLED(dseti)) {
               SUMA_S_Err( "Indexlist contains duplicate entries.\n"
                           "This is not supported.");
               exit(1);
            }
   

            if (!(dset_m = SUMA_MaskedByOrderedNodeIndexCopyofDset(
                     dset, Ti, SDSET_VECFILLED(dseti),  NULL, 1, 0))) {
               SUMA_S_Err("Failed to mask dset by node indices\n");
               SUMA_S_Note("If your input dataset did not have a node index \n"
                           "explicitly defined, use -add_node_index or\n"
                           "-node_index_1D options to specify node indices.\n" )
               exit(1);
            }
         } else {
            if (!(dset_m = SUMA_MaskedByNodeIndexCopyofDset(
                     dset, Ti, SDSET_VECFILLED(dseti),  NULL, 1, 0))) {
               SUMA_S_Err("Failed to mask dset by node indices\n");
               SUMA_S_Note("If your input dataset did not have a node index \n"
                           "explicitly defined, use -add_node_index or\n"
                           "-node_index_1D options to specify node indices.\n" )
               exit(1);
            }
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
      
      
      if (no_hist) {
         SUMA_RemoveDsetHist(dset);
      }
      
      if (prepend_node_index) {/* prepend node index? */         
         if (!SUMA_InsertDsetNelCol (  dset, "Node Index Copy", 
                                       SUMA_NODE_INT, 
                                       (void *)(dset->inel->vec[0]), 
                                       NULL ,1, 0)) {
            SUMA_S_Err("Failed to insert column");
         }
         if (LocalHead) SUMA_ShowDset(dset,0, NULL); 
      }
      
      if (cmapfile && SM) { /* labelize */
         SUMA_DSET *idset;
         if (!SUMA_is_AllConsistentCastType_dset(dset, SUMA_int)) { 
            idset = SUMA_CoercedCopyofDset(dset, SUMA_int, NULL);
         } else {
            idset = dset;
         }
         if (!(SUMA_dset_to_Label_dset_cmap(idset, SM))) {
            SUMA_S_Err("Failed to make change");
            exit(1);
         }
      
         if (idset != dset) {
            SUMA_FreeDset(dset); dset = idset; idset=NULL;
         }
      }
      
      SUMA_LHv("About to write dset to %s\n", prefix);
      if (!split) {
         NameOut = SUMA_WriteDset_s (prefix, dset, oform, overwrite, 0);
         if (!NameOut && !SUMA_IS_DSET_STDXXX_FORMAT(oform)) { 
            SUMA_SL_Err("Failed to write dataset."); exit(1); 
         } else {
            if (NameOut) SUMA_free(NameOut); NameOut = NULL;      
         }
      } else {
         int ksp, ikp, ikps, nsplits=(int)ceil((float)SDSET_VECNUM(dset)/split);
         SUMA_DSET *ds=NULL;
         char cbuf[12]={""}, *prefs = NULL;
         byte *colmask=NULL;
         colmask=(byte*)SUMA_malloc(sizeof(byte)*SDSET_VECNUM(dset));
         for (ksp=0; ksp<split; ++ksp) {
            sprintf(cbuf,"%04d",ksp);
            memset(colmask, 0, sizeof(byte)*SDSET_VECNUM(dset));
            ikp = ksp*nsplits; 
            ikps = SUMA_MIN_PAIR(SDSET_VECNUM(dset), (ksp+1)*nsplits);
            if (ikp == ikps) continue; /* all one */
            while (ikp < ikps) colmask[ikp++]=1;
            prefs = SUMA_RemoveDsetExtension_s(prefix,SUMA_NO_DSET_FORMAT);
            prefs = SUMA_append_replace_string(prefs,cbuf,".", 1);
            if (!(ds = SUMA_MaskedCopyofDset(dset, NULL, colmask, 1, 1))) {
               SUMA_S_Err("Failed to get masked copy");
               exit(1); 
            } 
            NameOut = SUMA_WriteDset_s (prefs, ds, oform, overwrite, 0);
            if (!NameOut && !SUMA_IS_DSET_STDXXX_FORMAT(oform)) { 
               SUMA_SL_Err("Failed to write dataset."); exit(1); 
            } else {
               if (NameOut) SUMA_free(NameOut); NameOut = NULL;
            }
            if (prefs) SUMA_free(prefs);
            SUMA_FreeDset(ds); ds=NULL; 
         }
         SUMA_free(colmask); colmask=NULL;
      }
      
      if (SM) SUMA_Free_ColorMap(SM); SM = NULL;
      if (prefix) SUMA_free(prefix); prefix = NULL;    
      if (dset) SUMA_FreeDset((void *)dset); dset = NULL;
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
   }
   
	SUMA_RETURN(0);
}    
