#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "mrilib.h"
#include "niml/niml.h"
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
"     -dset_labels 'SB_LABEL_0 SB_LABEL_1 ...'\n"
"                  Label the columns (sub-bricks) of the output dataset\n"
"                  You must have as many labels as you have sub-bricks in\n"
"                  the output dataset."
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
"                              reflects the order of the nodes in MASK.1D.\n"
"                              Note that the presence of duplicate nodes in\n"
"                              MASK.1D is not allowed, so if MASK.1D came\n"
"                              from ROI2dataset's -nodelist, recreate it with\n"
"                              option -nodelist.nodups instead. \n"
"                              Also, node indices that do not have data in the\n"
"                              input dataset will be ignored.\n"
"                              When in doubt, use the 1D output format along \n"
"                              with -prepend_node_index_1D and spot check your\n"
"                              results.\n" 
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
"     -graphize: Turn the dataset into a SUMA graph dataset.\n"
"                See input format constraints under -onegraph and -multigraph\n"
"     -graph_nodelist_1D NODEINDLIST.1D NODELIST.1D: Two files specifying the \n"
"                                  indices and the coordinates of the graph's\n"
"                                  nodes. In sum you need I X Y Z (RAI mm).\n"
"                                  but the I comes from NODEINDLIST.1D and the\n"
"                                  X Y Z coordinates from NODELIST.1D\n"
"                                  If you have everything in one file, use\n"
"                                  the same filename twice with proper column\n"
"                                  selectors.\n"
"     -graph_full_nodelist_1D NODELIST.1D: Same as above, but without the need\n"
"                                  for NODEINDLIST.1D. In that case, indices\n"
"                                  will implicitly go from 0 to N-1, with N\n"
"                                  being the number of nodes.\n"
"     -graph_named_nodelist_txt NODENAMES.txt NODELIST.1D: Two files used to \n"
"                                  specify graph node indices, string labels, \n"
"                                  and their coordinates.\n"
"                                  In sum you need I LABEL X Y Z (RAI mm).\n"
"                                  The I and LABEL come from NODENAMES.txt and\n"
"                                  the X Y Z coordinates from NODELIST.1D\n"
"                          Also, you can assign to each graph node a group ID\n"
"                                  and nodes with the same group ID can be \n"
"                                  displayed with the same color in SUMA.\n"
"                                  To do so, add a third column to \n"
"                                  NODENAMES.txt so that you have: I LABEL GID\n"
"                                  with GID being the integer group ID.\n"
"                                  Color selection for the different group IDs\n"
"                                  is done automatically with ConvertDset, but\n"
"                                  you can set your own by appending three \n"
"                                  more columns to NODENAMES.txt to have:\n"
"                                     I LABEL GID R G B\n"
"                                  with R, G, and B values between 0 and 1.0\n"
"     -graph_XYZ_LPI: Coords in NodeList.1D are in LPI instead of RAI \n"
"     -graph_edgelist_1D EDGELIST.1D: i j indices of graph nodes defining edge\n"
"                                   with each row matching the input dset row.\n"
"                                   This option only works with -multigraph\n"
"                                   This option also marks the graph as being\n"
"                                   a sparse matrix, even if a square matrix \n"
"                                   is provided.\n"
"     -onegraph: Expect input dataset to be one square matrix defining the\n"
"                graph (default).\n"
"     -multigraph: Expect each column in input dataset to define an entire\n"
"                  graph. Each column in this case should be a column-stacked\n"
"                  square matrix.\n" 
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
   float *fv = NULL, *cols=NULL;
   SUMA_DSET_FORMAT iform, oform;
   SUMA_DSET *dset = NULL, *dseti=NULL, *dset_m = NULL;
   char *NameOut, *prfx = NULL, *prefix = NULL, *cmapfile, 
         *graph_nodelist_1D=NULL, *graph_nodeindlist_1D=NULL,
         *graph_edgelist_1D=NULL, *graph_nodeindlist_txt=NULL, **names=NULL;
   char *ooo=NULL, *node_index_1d = NULL, *node_mask = NULL;
   int overwrite = 0, exists = 0, N_inmask=-1, pad_to_node = -1, *ivec=NULL;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   int orderednodelist = 1, split=0, toGDSET=0, OneMat, *clan=NULL;
   float fv5[5];
   int nv, mxgrp, RAI;
   char *stmp=NULL, colnm[32];
   SUMA_COLOR_MAP *SM=NULL;
   NI_str_array *dlabs=NULL;
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
   toGDSET=0;
   OneMat=1;
   RAI = 1;
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

      if (!brk && (strcmp(argv[kar], "-graph_xyz_lpi") == 0))
      {
         RAI = 0;
         brk = YUP;
      }
      
      
      if (!brk && (strcmp(argv[kar], "-graphize") == 0))
      {
         toGDSET = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-onegraph") == 0))
      {
         OneMat = 1;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-multigraph") == 0))
      {
         OneMat = 0;
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
      
      if (!brk && (strcmp(argv[kar], "-graph_nodelist_1d") == 0))
      {
         if (kar+2 >= argc) {
            SUMA_SL_Err("Need 2 arguments after -graph_nodelist_1D");
            exit(1);
         }
         ++kar;
         graph_nodeindlist_1D = argv[kar];
         ++kar;
         graph_nodelist_1D = argv[kar];
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-graph_full_nodelist_1d") == 0))
      {
         if (kar+1 >= argc) {
            SUMA_SL_Err("Need 1 arguments after -graph_full_nodelist_1D");
            exit(1);
         }
         graph_nodeindlist_1D = NULL;
         ++kar;
         graph_nodelist_1D = argv[kar];
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-graph_named_nodelist_txt") == 0))
      {
         if (kar+2 >= argc) {
            SUMA_SL_Err("Need 2 arguments after -graph_named_nodelist_txt");
            exit(1);
         }
         ++kar;
         graph_nodeindlist_txt = argv[kar];
         ++kar;
         graph_nodelist_1D = argv[kar];
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-graph_edgelist_1d") == 0))
      {
         if (kar+1 >= argc) {
            SUMA_SL_Err("Need argument after -graph_edgelist_1D");
            exit(1);
         }
         ++kar;
         graph_edgelist_1D = argv[kar];
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
            SUMA_SL_Err("Need argument after -prefix/-o* options");
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
      
      if (!brk && (strcmp(argv[kar], "-dset_labels") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -dset_labels \n");
				exit (1);
			}
			dlabs = NI_strict_decode_string_list(argv[kar] ,";, ");
         brk = 1;
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
      
      if (dlabs) {
         if (dlabs->num != SDSET_VECNUM(dset)) {
            SUMA_S_Err("You have %d labels but %d sub-bricks in dset %s",
                        dlabs->num, SDSET_VECNUM(dset), SDSET_LABEL(dset));
         } else {
            int ii;
            for (ii=0; ii<SDSET_VECNUM(dset); ++ii) {
               SUMA_UpdateDsetColLabel(dset, ii, dlabs->str[ii]);
            }
         }
         NI_delete_str_array(dlabs); dlabs = NULL;
      }
      
      if (toGDSET) {
         SUMA_LH("Going to graph format");
         if (graph_edgelist_1D) {
            int *ie=NULL;
            SUMA_DSET *dsetc=NULL;
            iform = SUMA_1D;
            if (!(dseti = SUMA_LoadDset_s (graph_edgelist_1D, &iform, 0))) {
               SUMA_S_Err("Failed to load nodelist ");
               exit(1);
            }
            if (SDSET_VECNUM(dseti) != 3 && SDSET_VECNUM(dseti) != 2 ) {
               SUMA_S_Err("Bad edgelist source, only 2or 3 columns allowed");
               exit(1);
            }
            if (!(dsetc = SUMA_CoercedCopyofDset(dseti, SUMA_int, NULL))) {
               SUMA_S_Err("Failed to copy to ints?");
               exit(1);
            }  
            SUMA_FreeDset(dseti); dseti = dsetc; dsetc = NULL;
            switch (SDSET_VECNUM(dseti)){
               case 2:
                  if (!(SUMA_Dset_to_GDSET(&dset, NULL, OneMat, 
                                           NULL,
                                           (int *)SDSET_VEC(dseti,0),
                                           (int *)SDSET_VEC(dseti,1)))) {
                     SUMA_S_Err("Failed to graphize");
                  }
                  break;
               case 3:
                  if (!(SUMA_Dset_to_GDSET(&dset, NULL, OneMat, 
                                           (int *)SDSET_VEC(dseti,0),
                                           (int *)SDSET_VEC(dseti,1),
                                           (int *)SDSET_VEC(dseti,2)))) {
                     SUMA_S_Err("Failed to graphize");
                  }
                  break;
               default:
                  SUMA_S_Err("More infidel than an infidel!");
                  break;
            }
            SUMA_FreeDset(dseti); dseti = NULL;
         } else {
            if (!(SUMA_Dset_to_GDSET(&dset, NULL, OneMat, NULL, NULL, NULL))) {
               SUMA_S_Err("Failed to graphize");
            }
         }
         
         if (graph_nodelist_1D) {
            SUMA_DSET *dsetind=NULL;
            SUMA_LH("Set Aux shape");
            if (!SUMA_GDSET_Set_Aux_matrix_shape(dset)) {
               SUMA_S_Err("Need my matrix params");
               exit(1);
            } 
            
            SUMA_LH("Now the nodelist");
            iform = SUMA_1D;
            if (!(dseti = SUMA_LoadDset_s (graph_nodelist_1D, &iform, 0))) {
               SUMA_S_Err("Failed to load nodelist %s", graph_nodelist_1D);
               exit(1);
            }
            if (SDSET_VECNUM(dseti) != 3) {
               SUMA_S_Err("Bad nodelist source\n"
                          "Only 3 column allowed, have %d of them in %s", 
                          SDSET_VECNUM(dseti), graph_nodelist_1D);
               exit(1);
            }
            if (graph_nodeindlist_1D) {
               if (!(dsetind = 
                        SUMA_LoadDset_s (graph_nodeindlist_1D, &iform, 0))) {
                  SUMA_S_Err("Failed to load node index list %s",
                             graph_nodeindlist_1D);
                  exit(1);
               }
               if (SDSET_VECNUM(dsetind) != 1) {
                  SUMA_S_Err("Bad nodelist index source\n"
                             "Only 1 column allowed, have %d of them in %s",
                             SDSET_VECNUM(dsetind), graph_nodeindlist_1D);
                  exit(1);
               }
               if (SDSET_VECFILLED(dseti) != SDSET_VECFILLED(dsetind)) {
                  SUMA_S_Errv( 
                     "mismatch in number of values between %s and %s\n",
                     graph_nodelist_1D, graph_nodeindlist_1D);
                  exit(1);
               }
               /* coerce index input to int */
               {
                  SUMA_DSET *dsetc=NULL;
                  if (!(dsetc = SUMA_CoercedCopyofDset(dsetind, 
                                                       SUMA_int, NULL))) {
                     SUMA_S_Err("Failed coerce");
                     exit(1);
                  }
                  SUMA_FreeDset(dsetind); dsetind = dsetc; dsetc = NULL;
               }
               ivec = SDSET_VEC(dsetind,0);
               dsetind->dnel->vec[0] = NULL; 
               SUMA_FreeDset(dsetind); dsetind = NULL;
            } else if (graph_nodeindlist_txt) {
               char *fl=NULL, *fle=NULL, *fl2=NULL;
               int ok=0, cnt=0, mxcol=0, nalloc=0, nchar, ans;
               float dum;
               
               /* Load file that has node indices and labels */
               if (!(fl = SUMA_file_suck(graph_nodeindlist_txt, &nchar))) {
                  SUMA_S_Errv("Faile to read %s\n", graph_nodeindlist_txt);
                  exit(1);
               }
               fle = fl+nchar;
               ok = 1;
               cnt = 0; mxcol = 0, nalloc=0;
               while (ok && fl < fle) {
                  SUMA_SKIP_BLANK(fl, fle);
                  do {
                     /* skip comment, if any */
                     SUMA_IS_COMMENT_LINE(fl, fle, '#', ans);
                     if (ans) {  
                        SUMA_LH("Skipping comment..."); 
                        SUMA_SKIP_LINE(fl, fle);
                     }
                  } while (ans); 
                  SUMA_SKIP_BLANK(fl, fle); if (fl == fle) break; 
                  SUMA_LHv("Now at >>%s<<\n", fl);
                  /* read first number */
                  SUMA_ADVANCE_PAST_NUM(fl, dum, ok); 
                  if (!ok && fl!=fle) { 
                     SUMA_S_Errv("Failed to read i, stuck at >>%s<<\n", 
                                 fl); exit(1);
                  }
                  if (cnt >= nalloc) {
                     nalloc = nalloc+256;
                     ivec = (int *)SUMA_realloc(ivec, nalloc*sizeof(int));
                     names = (char **)SUMA_realloc(names, nalloc*sizeof(char *));
                     clan = (int *)SUMA_realloc(clan, nalloc*sizeof(int));
                     cols = (float *)SUMA_realloc(cols, 3*nalloc*sizeof(int));
                  }
                  SUMA_LHv("index[%d] %f\n", cnt, dum);
                  ivec[cnt] = (int)dum;
                  /* Now get the label */
                  SUMA_GET_BETWEEN_BLANKS(fl, fle, fl2);
                  if (fl2 > fl) {
                     names[cnt]=NULL;
                     SUMA_COPY_TO_STRING(fl, fl2, names[cnt]);
                     SUMA_LHv("  Name[%d] %s, fl2[0] is >%c<\n",
                                 cnt, names[cnt], fl2[0]);
                     fl = fl2;
                  } else {
                     SUMA_S_Errv("Failed to get label associated with index %d\n"
                                 ,ivec[cnt]);
                     exit(1); 
                  }
                  /* And lastly, do we have numbers left? */
                  SUMA_SKIP_PURE_BLANK(fl, fle);
                  SUMA_GET_TO_EOL(fl, fle, fl2);
                  if (fl2 > fl) {
                     SUMA_COPY_TO_STRING(fl, fl2, stmp);
                     SUMA_LH("Parsing %s", stmp);
                     /* colors anyone? */
                     nv = SUMA_StringToNum(stmp, (void *)fv5, 4, 1);
                     switch (nv) {
                        case 0:
                           clan[cnt] = -2;
                        case 1:
                           clan[cnt] = (int)fv5[0];
                           cols[3*cnt  ] = -1.0;
                           cols[3*cnt+1] = -1.0;
                           cols[3*cnt+2] = -1.0;
                           break;
                        case 4:
                           clan[cnt] = (int)fv5[0];
                           cols[3*cnt  ] = fv5[1];
                           cols[3*cnt+1] = fv5[2];
                           cols[3*cnt+2] = fv5[3];
                           break;
                        default:
                           SUMA_S_Err(
                              "Expected 1 or 4  values after name %s, got %d"
                              "Replacing with special group",
                              names[cnt], nv);
                           clan[cnt] = -1;
                           cols[3*cnt  ] = -1.0;
                           cols[3*cnt+1] = -1.0;
                           cols[3*cnt+2] = -1.0;
                           break;
                     }
                     fl = fl2;
                  } else {
                     clan[cnt] = -2;
                  }
                  SUMA_ifree(stmp);
                  ++cnt;
               }
               if (cnt != SDSET_VECFILLED(dseti)) {
                  SUMA_S_Errv("Have %d entries in %s but %d enties in %s\n",
                           cnt, graph_nodeindlist_txt, 
                           SDSET_VECFILLED(dseti), graph_nodelist_1D);
                  exit(1);
               }
               /* check on colors and grouping */
               if (clan[0] == -2) {/* No grouping, no colors */
                  SUMA_ifree(clan); SUMA_ifree(cols);
               } else { 
                  mxgrp = -1;
                  for (cnt=0; cnt <SDSET_VECFILLED(dseti); ++cnt) {
                     if (clan[cnt] > mxgrp) mxgrp = clan[cnt];
                  }
                  for (cnt=0; cnt <SDSET_VECFILLED(dseti); ++cnt) {
                     if (clan[cnt] < 0) clan[cnt] = mxgrp + 1;
                  }
                  sprintf(colnm, "%d", SUMA_MIN_PAIR(mxgrp+2,255));
                  for (cnt=0; cnt <SDSET_VECFILLED(dseti); ++cnt) {
                     if (cols[3*cnt] < 0) {
                        SUMA_a_good_col(colnm, clan[cnt], fv5);
                        cols[3*cnt  ] = fv5[0];
                        cols[3*cnt+1] = fv5[1];
                        cols[3*cnt+2] = fv5[1];
                     }
                  }
               }
            }  else {
               ivec = NULL; /* SUMA_AddGDsetNodeListElement will generate one */
            }

            SUMA_LH( "Have %d node indices %d .. %d in %s\n"
                     "Graph %s has %ld segment nodes, %ld nodes defined.\n", 
                    SDSET_VECFILLED(dseti), ivec[0], 
                    ivec[SDSET_VECFILLED(dseti)-1], SDSET_LABEL(dseti),
                    SDSET_LABEL(dset), GDSET_N_SEG_POINTS(dset),
                    GDSET_N_ALL_POINTS(dset));
            if (!RAI) {
               int cnt;
               float *fvx = (float *)SDSET_VEC(dseti,0);
               float *fvy = (float *)SDSET_VEC(dseti,1);
               SUMA_LH("Flipping to LPI");
               for (cnt=0; cnt <SDSET_VECFILLED(dseti); ++cnt) {
                  fvx[cnt] = -fvx[cnt];
                  fvy[cnt] = -fvy[cnt];
               }
            }
            if (!(SUMA_AddGDsetNodeListElement(dset, ivec,
                                                     SDSET_VEC(dseti,0),
                                                     SDSET_VEC(dseti,1),
                                                     SDSET_VEC(dseti,2),
                                                     names,
                                                     clan,
                                                     cols,
                                                     SDSET_VECFILLED(dseti)))) {
               SUMA_S_Err("Failed to add node list");
               exit(1);                                       
            }
            SUMA_FreeDset(dseti); dseti = NULL;
            if (ivec) free(ivec); ivec=NULL;
            if (names) SUMA_free(names); names = NULL;
            SUMA_ifree(cols); SUMA_ifree(clan);
         }
         if (LocalHead) SUMA_ShowDset(dset,0, NULL);  
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
