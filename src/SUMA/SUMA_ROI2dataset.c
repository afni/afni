#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_ROI2dataset_Main ()
   
  {/*Usage*/
      static char FuncName[]={"usage_ROI2dataset_Main"};
      char * s = NULL;
      fprintf(SUMA_STDOUT, 
"\n"
"Usage: \n"
"   ROI2dataset <-prefix dsetname> [...] <-input ROI1 ROI2 ...>\n"
"               [<-of ni_bi|ni_as|1D>] \n"
"               [<-dom_par_id idcode>] \n"
/* "   [<-dom_par domain> NOT IMPLEMENTED YET] \n" */
"    This program transforms a series of ROI files\n"
"    to a node dataset. This data set will contain\n"
"    the node indices in the first column and their\n"
"    ROI values in the second column.\n"
"    Duplicate node entries (nodes that are part of\n"
"    multiple ROIs) will get ignored. You will be\n"
"    notified when this occurs. \n"
"\n"
"Mandatory parameters:\n"
"    -prefix     dsetname: Prefix of output dataset.\n"
"                          Program will not overwrite existing\n"
"                          datasets.\n"
"                          See also -label_dset alternate below.\n"
"\n"      
" and/or\n"
"    -nodelist        NL: Prefix for a set of .1D files\n"
"    -nodelist.nodups NL: that contain a list of node indices\n"
"                         in the order in which they appear in\n"
"                         an ROI. This way you can make use of the\n"
"                         directionality of an ROI line instead of just \n"
"                         treating it as a set of nodes. \n"
"                         For each integer label 'i' in the ROI files provided\n"
"                         with the -input option, you will get a file called\n"
"                         NL.i.1D listing the nodes in the order they were \n"
"                         encountered in an ROI file and across ROI files.\n"
"                         If you want duplicate node entries removed, then\n"
"                         use -nodelist.nodups instead.\n"
"                   Note: You can use the output of -nodelist as input to \n"
"                         ConvertDset's -node_select_1D option. \n"
"                         This is not the case for -nodelist.nodups because\n"
"                         ConvertDset's -node_select_1D does allow for \n"
"                         duplicate node entries. \n"
"    -input ROI1 ROI2....: ROI files to turn into a \n"
"                          data set. This parameter MUST\n"
"                          be the last one on command line.\n"
"\n"
"Optional parameters:\n"
"    All optional parameters must be specified before the -input parameters.\n"
"\n"
"    -label_dset dsetname: Write a label dataset, instead of a simple dataset.\n"
"                          Labeled datasets are treated differently in SUMA.\n"   "                          This option also sets the output format to NIML.\n"
"    -h | -help: This help message\n"
"    -of FORMAT: Output format of dataset. FORMAT is one of:\n"
"                ni_bi: NIML binary\n"
"                ni_as: NIML ascii (default)\n"
"                1D   : 1D AFNI format.\n"
"    -dom_par_id id: Idcode of domain parent.\n"
"                    When specified, only ROIs have the same\n"
"                    domain parent are included in the output.\n"
"                    If id is not specified then the first\n"
"                    domain parent encountered in the ROI list\n"
"                    is adopted as dom_par_id.\n"
"                    1D roi files do not have domain parent \n"
"                    information. They will be added to the \n"
"                    output data under the chosen dom_par_id.\n"
"    -pad_to_node max_index: Output a full dset from node 0 \n"
"                            to node max_index (a total of \n"
"                            max_index + 1 nodes). Nodes that\n"
"                            are not part of any ROI will get\n"
"                            a default label of 0 unless you\n"
"                            specify your own padding label.\n"
"    -pad_label padding_label: Use padding_label (an integer) to\n"
"                            label nodes that do not belong\n"
"                            to any ROI. Default is 0.\n" 
"\n");
         s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
         fprintf(SUMA_STDOUT, 
            "       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \n");
     exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ROI2dataset"}; 
   char  *prefix_name, **input_name_v=NULL, *out_name=NULL, 
         *Parent_idcode_str = NULL, *dummy_idcode_str = NULL, 
         *stmp=NULL, *sss=NULL, *nodelist=NULL, *outlist_name=NULL,
         cbuf[50];
   int   kar, brk, N_input_name, cnt = 0, N_ROIv, 
         N_tROI, ii, i, nn, pad_to, pad_val;
   SUMA_DSET *dset=NULL;
   NI_stream ns;
   SUMA_DSET_FORMAT Out_Format = SUMA_ASCII_NIML;
   SUMA_DRAWN_ROI ** ROIv = NULL, **tROIv = NULL;
	SUMA_Boolean AddThis = NOPE;
   SUMA_ROI_EXTRACT *dd=NULL;
   DList *ddl=NULL;
   DListElmt *el=NULL;
   int nodups=0, olabel = 0;
   SUMA_COLOR_MAP *cmap=NULL;
   SUMA_Boolean LocalHead = NOPE;
	
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
	
   if (argc < 4) {
      usage_ROI2dataset_Main ();
   }
   
   /* parse the command line */
   kar = 1;
	brk = NOPE;
   prefix_name = NULL;
   input_name_v = NULL;
   N_input_name = 0;
   Out_Format = SUMA_NO_DSET_FORMAT; 
   Parent_idcode_str = NULL;
   pad_to = -1;
   pad_val = 0;
   nodelist=NULL;
   olabel = 0;
   nodups = 0;
   while (kar < argc) { /* loop accross command ine options */
		/* SUMA_LH("Parsing command line..."); */
      
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_ROI2dataset_Main();
          exit (1);
		}
      
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix ");
				exit (1);
			}
			prefix_name = argv[kar];
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-label_dset") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -label_dset ");
				exit (1);
			}
			prefix_name = argv[kar];
         if (AFNI_yesenv("AFNI_NIML_TEXT_DATA")) Out_Format = SUMA_ASCII_NIML;
         else Out_Format = SUMA_BINARY_NIML;
         olabel = 1;
         brk = YUP;
		}
      
      if (  !brk && 
            (strncmp(argv[kar], "-nodelist", 9) == 0) ) {
         if (strcmp(argv[kar], "-nodelist.nodups") == 0) nodups = 1;
         else nodups = 0;
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -nodelist* options ");
				exit (1);
			}
			nodelist = argv[kar];
         
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-of") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -of ");
				exit (1);
			}
         Out_Format = SUMA_NO_DSET_FORMAT;
                     /* Can't use isOutputFormatFromArg because -of is not part
                     of the passed argument */
         if (!SUMA_isFormatFromArg(argv[kar], &Out_Format)) {
            fprintf (SUMA_STDERR, 
                     "%s not a valid option with -of.\n", argv[kar]);
				exit (1);
         }
         
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-dom_par_id") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -dom_par_id");
				exit (1);
			}
			Parent_idcode_str = SUMA_copy_string(argv[kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need at least one argument after -input ");
				exit (1);
			}
         input_name_v = (char **)SUMA_malloc((argc-kar+1)*sizeof(char *));
         
         cnt = 0;
         while (kar < argc) {
            input_name_v[cnt] = argv[kar];
            ++cnt; ++kar;
         }
         N_input_name = cnt;
         brk = YUP;
      }
      
      #if 0   /* now handled in main_ENTRY() */
      if (!brk && (strcmp(argv[kar], "-pad_to_node") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -pad_to_node");
				exit (1);
			}
			pad_to = atoi(argv[kar]);
         brk = YUP;
      }
      #endif
      if (!brk && ( (strcmp(argv[kar], "-pad_label") == 0) || 
                     (strcmp(argv[kar], "-pad_val") == 0) )) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -pad_label");
				exit (1);
			}
			pad_val = atoi(argv[kar]);
         brk = YUP;
      }
      
      if (!brk) {
			fprintf (SUMA_STDERR,
                  "Error %s: Option %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}   
   }   
   
   if (MRILIB_DomainMaxNodeIndex >= 0) pad_to = MRILIB_DomainMaxNodeIndex;

   if (!prefix_name && !nodelist) {
      fprintf (SUMA_STDERR,
               "Error %s: No output prefix or nodelist was specified.\n"
               , FuncName);
      exit(1);
   }
   
   if (prefix_name) {
      if (Out_Format == SUMA_NO_DSET_FORMAT) {
         Out_Format = SUMA_GuessFormatFromExtension(prefix_name,
                                                    "love.niml.dset");
      }
      /* form the output name and check for existence */
      #if 1
      out_name = SUMA_Extension( prefix_name, 
                                 (char *)SUMA_ExtensionOfDsetFormat(Out_Format),
                                 NOPE);
      #else
      switch (Out_Format) {
         case SUMA_ASCII_NIML:
         case SUMA_BINARY_NIML:
            out_name = SUMA_Extension(prefix_name, ".niml.dset", NOPE); 
            break;
         case SUMA_1D:
            out_name = SUMA_Extension(prefix_name, ".1D.dset", NOPE); 
            break;
         default:
            SUMA_S_Err("Output format not supported");
            exit(1);
            break;
      }
      #endif
      SUMA_LH (out_name);

      /* check for existence of out_name */
      if (SUMA_filexists(out_name) && !THD_ok_overwrite()) {
         fprintf(SUMA_STDERR,"Error %s:\n Output file %s exists.\n", 
                              FuncName, out_name);
         exit(1); 
      }
   } else out_name = NULL;
   
   
   /* check for input files */
   if (N_input_name <= 0) {
      fprintf(SUMA_STDERR,"Error %s:\n No ROI files specified.\n",
                           FuncName);
      exit(1); 
   }
    
   /* read in the data sets */
   /* create a dummy idcode_str for potential 1D data sets */
   N_ROIv = 0;
   Parent_idcode_str = NULL;
   dummy_idcode_str = UNIQ_hashcode("DummyNameNothingLikeIt");
   for (i=0; i < N_input_name; ++i) {
      if (SUMA_isExtension(input_name_v[i], ".niml.roi")) {
         /* load niml ROI */
         if (!( tROIv = SUMA_OpenDrawnROI_NIML (input_name_v[i], 
                                                      &N_tROI, NOPE))) {
            SUMA_S_Err("Failed to read NIML ROI.");
            exit(1);
         }
      }else if (SUMA_isExtension(input_name_v[i], ".1D.roi")) {
         /* load 1D ROI */
         if (!( tROIv = SUMA_OpenDrawnROI_1D (input_name_v[i], 
                                          dummy_idcode_str, &N_tROI, NOPE))) {
            SUMA_S_Err("Failed to read NIML ROI.");
            exit(1);
         }
      }else {
         SUMA_S_Errv(  "Failed to recognize\n"
                      "ROI type from filename '%s'\n", input_name_v[i]);
         exit(1);
      } 
      
      SUMA_LH("Copying temporary ROIv into the main ROIv ");
      /* copy temporary ROIv into the main ROIv */
      ROIv = (SUMA_DRAWN_ROI **)
               SUMA_realloc(ROIv, (N_ROIv + N_tROI) * sizeof(SUMA_DRAWN_ROI*));
      if (!ROIv) {
         SUMA_S_Err("Failed to allocate.");
         exit(1);
      }

      /* Now go throught the ROIs and load them if possible into ROIv */
      SUMA_LHv("Cycling over %d rois\n", N_tROI);
      for (ii=0; ii < N_tROI; ++ii) {
         if (!Parent_idcode_str) {
            /* try to find out what the Parent_idcode_str is */
            if (tROIv[ii]->Parent_idcode_str && dummy_idcode_str &&  
               strcmp(tROIv[ii]->Parent_idcode_str, dummy_idcode_str)) {
               fprintf (SUMA_STDERR,
                        "%s: Adopting Parent_idcode_str (%s) in ROI %s\n",
                        FuncName, tROIv[ii]->Parent_idcode_str, 
                        tROIv[ii]->Label);
               /* good, use it as the Parent_idcode_str for all upcoming ROIs */
               Parent_idcode_str = 
                  SUMA_copy_string(tROIv[ii]->Parent_idcode_str);
            }
         } 
         
         AddThis = NOPE;
         if (tROIv[ii]->Parent_idcode_str && dummy_idcode_str &&  
            !strcmp(tROIv[ii]->Parent_idcode_str, dummy_idcode_str)) {
            AddThis = YUP;
         } else {
            if (tROIv[ii]->Parent_idcode_str && dummy_idcode_str &&  
               strcmp(tROIv[ii]->Parent_idcode_str, Parent_idcode_str)) {
               fprintf (SUMA_STDERR,"Warning %s:\n Ignoring ROI labeled %s\n"
                                    "because of Parent_idcode_str mismatch.\n", 
                                    FuncName, tROIv[ii]->Label); 
               AddThis = NOPE;
               /* free structure of tROIv[ii] */
               SUMA_freeDrawnROI (tROIv[ii]); tROIv[ii] = NULL;
            }
            else AddThis = YUP;
            
         }
         if (AddThis) {
            if (LocalHead) fprintf (SUMA_STDERR,
                                    "%s: Adding %dth ROI to ROIv...\n",
                                    FuncName, N_ROIv);
            ROIv[N_ROIv] = tROIv[ii];
            
            ++N_ROIv;
         }
          
      }
      /* now free tROIv vector */
      if (tROIv) SUMA_free(tROIv); tROIv = NULL;  
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: Kept a total of %d ROIs with parent %s\n",
                        FuncName, N_ROIv, Parent_idcode_str);
        
   }
   
   if (nodelist) { /* output node list wanted */
      ddl = SUMA_ROIv2NodeLists (ROIv, N_ROIv, nodups);
      if (!ddl) {
         SUMA_S_Err("Failed in SUMA_ROIv2NodeLists");
      }
      el = dlist_head(ddl);
      while (el) {
         dd = (SUMA_ROI_EXTRACT *)el->data;
         sprintf(cbuf, ".%0d.1D", dd->label);
         outlist_name = SUMA_append_string(nodelist, cbuf);
         /* check for existence of outlist_name */
         if (SUMA_filexists(outlist_name) && !THD_ok_overwrite()) {
            fprintf(SUMA_STDERR,"Error %s:\n Output file %s exists.\n", 
                                 FuncName, outlist_name);
            exit(1); 
         }
         
         SUMA_WRITE_INT_ARRAY_1D(dd->vals, dd->N_vals, 1, outlist_name);
         SUMA_free(outlist_name); outlist_name = NULL;
         el = dlist_next(el);
      } 

   }
   
   if (out_name) { /* output dset required */
      if (!(dset = SUMA_ROIv2Grpdataset ( ROIv, N_ROIv, 
                                          Parent_idcode_str, 
                                          pad_to, pad_val, 
                                          &cmap))) {
         SUMA_SL_Err("Failed in SUMA_ROIv2Grpdataset");
         exit(1);
      }

      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: Adding history\n",
                           FuncName);

      }

      /* Add the history line */
      if (!SUMA_AddNgrHist (dset->ngr, FuncName, argc, argv)) {
         SUMA_SL_Err("Failed in SUMA_AddNgrHist");
         exit(1);
      }


      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: preparing to write results\n",
                           FuncName);

      }

      if (olabel) {
         if (!(SUMA_dset_to_Label_dset(dset, cmap))) {
            SUMA_S_Err("Failed to make change");
            exit(1);
         }
      }
            
      /* write nel */
      switch (Out_Format) {
         case SUMA_ASCII_NIML:
         case SUMA_BINARY_NIML:
            /* open stream */
            stmp = SUMA_append_string ("file:", out_name);
            ns = NI_stream_open( stmp , "w" ) ;
            if( ns == NULL ){
               fprintf (stderr,"Error  %s:\nCan't open %s!"
                           , FuncName, stmp); 
               exit(1);
            }
            if (Out_Format == SUMA_ASCII_NIML) {
               nn = NI_write_element(  ns , dset->ngr , NI_TEXT_MODE ); 
            } else {
               nn = NI_write_element(  ns , dset->ngr , NI_BINARY_MODE ); 
            }
            if (nn < 0) {
               SUMA_S_Err ("Failed in NI_write_element");
               exit(1);
            }

            /* close the stream */
            NI_stream_close( ns ) ; 
            break;
         case SUMA_1D:
            stmp = SUMA_append_string ("file:", out_name);
            ns = NI_stream_open( stmp , "w" ) ;
            if( ns == NULL ){
               fprintf (stderr,"Error  %s:\nCan't open %s!"
                           , FuncName, stmp); 
               exit(1);
            }
            if (LocalHead) SUMA_ShowNel(dset->dnel);
            NI_insert_column(dset->dnel, dset->inel->vec_typ[0], 
                              dset->inel->vec[0], 0); 
            if (LocalHead) SUMA_ShowNel(dset->dnel);
            nn = NI_write_element(  ns , dset->dnel , 
                                    NI_TEXT_MODE | NI_HEADERSHARP_FLAG);  
            NI_remove_column(dset->dnel, 0); 
            if (nn < 0) {
               SUMA_S_Err ("Failed in NI_write_element");
               exit(1);
            }

            /* close the stream */
            NI_stream_close( ns ) ; 
            break;
         default:
            nn = 1;
            sss = SUMA_WriteDset_s( out_name, dset, 
                                    Out_Format, THD_ok_overwrite(),0);
            if (sss) SUMA_free(sss); sss=NULL;
            break;
      }


      /* free nel */
      SUMA_FreeDset(dset); dset = NULL;
   }
      
   /* free others */
   if (stmp) SUMA_free(stmp);
   if (ROIv) SUMA_free (ROIv);
   if (outlist_name) SUMA_free(outlist_name);
   if (ddl) SUMA_free(ddl);
   if (out_name) SUMA_free(out_name);
   if (Parent_idcode_str) SUMA_free(Parent_idcode_str);
   if (dummy_idcode_str) free(dummy_idcode_str); /* this one's allocated 
                                                   by Bob's functions */
   return(0);
}/* Main */
