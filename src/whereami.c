/*** Whereami.c modified 1/11/05 -- main function by Mike Angstadt of U Chicago ***/

#define MAIN

#include "mrilib.h"
#include "afni.h"
#include <stdio.h>
#include <stdlib.h>
#include "thd_ttatlas_query.h"

static int           have_dseTT = -1   ;
static THD_3dim_dataset * dseTT = NULL ;
static THD_3dim_dataset * dseTT_big = NULL ; /* 01 Aug 2001 */
static int           have_dseCA_EZ_MPM = -1   ;
static THD_3dim_dataset * dseCA_EZ_MPM = NULL ;
static int           have_dseCA_EZ_PMaps = -1   ;
static THD_3dim_dataset * dseCA_EZ_PMaps = NULL ;
static int           have_dseCA_EZ_ML = -1   ;
static THD_3dim_dataset * dseCA_EZ_ML = NULL ;

static MCW_cluster * wamiclust=NULL ;
static MCW_cluster * wamiclust_CA_EZ=NULL ;



/**Original code by Mike Angstadt *******************************************
  Main function added by Mike Angstadt on 1/12/05
  usage: whereami x y z [output format]
  where x,y,z are float coordinates in tlrc space
  and output format is a 0 or 1
   0 (default) just outputs the string as it would appear from the interactive
   AFNI Where am I? command
   1 outputs the string as a tab-delimited list of the form:
   Focus point: Some area <tab> Within 6 mm: Some area <tab> etc
**************************************************************************
  New version by Ziad S. Saad Feb 06
*/

#define zischar(ch) ( ( ((ch) >= 'A' && (ch) <= 'Z' ) || ((ch) >= 'a' && (ch) <= 'z' ) ) ? 1 : 0 )
#define isnakedarg(s) ( ( (s)[0] == '-' && strlen(s) > 1 && zischar((s)[1]) ) ? 0 : 1 )

void whereami_usage(void) 
{
   int i = 0;
   
   ENTRY("whereami_usage");
      printf(  "Usage: whereami [x y z [output_format]] [-lpi/-spm] [-atlas ATLAS] \n"
               "   ++ Reports brain areas located at x y z mm in TLRC space according \n"
               "   to atlases present with your AFNI installation.\n"
               "   ++ Show the contents of available atlases\n"
               "   ++ Extract ROIs for certain atlas regions using symbolic notation\n"
               "Options (all options are optional):\n"
               "   <x y z [output_format]>: Specifies the x y z coordinates of the \n"
               "                            location probed. Coordinate are in mm and \n"
               "                            assumed to be in RAI or DICOM format, unless\n"
               "                            otherwise specified (see -lpi/-spm below)\n"
               "                            In the AFNI viewer, coordinate format is\n"
               "                            specified above the coordinates in the top-left\n"
               "                            of the AFNI controller. Right click in that spot\n"
               "                            to change between RAI/DICOM and LPI/SPM.\n"
               "                     NOTE I:In the output, the coordinates are reported\n"
               "                            in LPI, in keeping with the convention used\n"
               "                            in most publications.\n"
               "                    NOTE II:To go between LPI and RAI, simply flip the \n"
               "                            sign of the X and Y coordinates.\n"
               "\n"
               "                            Output_format is an optional flag where:\n"
               "                            0 is for standard AFNI 'Where am I?' format.\n"
               "                            1 is for Tab separated list, meant to be \n"
               "                            friendly for use in spreadsheets. \n"
               "                            The default output flag is 0. You can use\n"
               "                            options -tab/-classic instead of the 0/1 flag.\n"
               " -lpi/-spm: Input coordinates are in LPI or SPM format. Default input\n"
               "            format is RAI/DICOM.\n"
               " -classic: Classic output format (output_format = 0).\n"
               " -tab: Tab delimited output (output_format = 1). Useful for spreadsheeting.\n"
               " -atlas ATLAS: Use atlas ATLAS for the query.\n"
               "               You can use this option repeatedly to specify\n"
               "               more than one atlas. Default is all available atlases.\n"
               "               ATLAS is one of:\n"
               "   %-12s: Created by tracing Talairach and Tournoux brain illustrations.\n"
               "   Generously contributed by Jack Lancaster and Peter Fox of RIC UTHSCSA)\n"
               "\n"
               "   %-12s: Anatomy Toolbox's atlases with some created from cytoarchitectonic \n"
               "   %-12s: studies of 10 human post-mortem brains (CA_N27_MPM, CA_N27_PM). \n"
               "   %-12s: Generously contributed by Simon Eickhoff,\n"
               "   %-12s: Katrin Amunts and Karl Zilles of IME, Julich, \n"
               "   Germany. Please take into account the references and abide by the \n"
               "   warning below (provided with the Anatomy toolbox) when using these atlases:\n", 
               Atlas_Code_to_Atlas_Name(AFNI_TLRC_ATLAS),
               Atlas_Code_to_Atlas_Name(CA_EZ_N27_MPM_ATLAS),
               Atlas_Code_to_Atlas_Name(CA_EZ_N27_ML_ATLAS),
               Atlas_Code_to_Atlas_Name(CA_EZ_N27_PMAPS_ATLAS),
               Atlas_Code_to_Atlas_Name(CA_EZ_N27_LR_ATLAS));                       
      i = 0;
      printf(  "Anatomy Toolbox Reference and Warning:\n"
               "--------------------------------------\n" );
      do { 
         printf(  "   %s\n" , CA_EZ_REF_STR[i]);
         ++i;  
      } while (CA_EZ_REF_STR[i][0] != '\0');
              /* "      [1] Auditory cortex (TE 1.0, TE 1.1, TE 1.2) : Morosan et al., Neuroimage, 2001\n"
               "      [2] Broca's area (BA 44, BA 45) : Amunts et al., J Comp Neurol, 1999\n"
               "      [3] Motor cortex (BA 4a, BA 4p, BA 6) : Geyer et al., Nature, 1996 ; S. Geyer, Springer press 2003\n"
               "      [4] Somatosensory cortex (BA 3a, BA 3b, BA 1 BA 2) : Geyer et al., Neuroimage, 1999 + 2000 ; Grefkes et al., Neuroimage, 2001\n"
               "      [5] Parietal operculum / SII (OP 1, OP 2, OP 3, OP 4) : Eickhoff et al., Cerebral Cortex, 2005a,b\n"
               "      [6] Amygdala (CM/LB/SF), Hippocampus (FD/CA /SUB/EC/HATA) : Amunts et al., Anat Embryol, 2005\n"
               "      [7] Visual cortex (BA 17, BA 18) : Amunts et al., Neuroimage, 2000\n"
               "      Warning:\n"
               "        All other areas may only be used after consultation (contact S.Eickhoff@fz-juelich.de)\n" */
       printf( "   \nSee Eickhoff et al. Neuroimage 25 (2005) for more info on Probability Maps (CA_N27_PM)\n"
               "    and Maximum Probability Maps (CA_N27_MPM)\n");
       printf( "--------------------------------------\n\n" 
               " -atlas_sort: Sort results by atlas (default)\n"
               " -zone_sort | -radius_sort: Sort by radius of search\n"
               " -old : Run whereami in the olde (Pre Feb. 06) way.\n"
               " -show_atlas_code: Show the integer code<--> area label of the atlases\n"
               "                   in use. The output is not too pretty because\n"
               "                   the option is for debugging use.\n"
               " -show_atlas_region REGION_CODE: You can now use symbolic notation to\n"
               "                                 select atlas regions. REGION_CODE has three\n"
               "                                 colon-separated elements forming it:\n"
               "                           Atlas_Name:Side:Area.\n"
               "                     Atlas_Name: one of the atlas names listed above.\n"
               "                                 If you do not have a particular atlas in your AFNI\n"
               "                                 installation, you'll need to download it (see below).\n"
               "                     Side      : Either left, right or nothing(::) for bilateral.\n"
               "                     Area      : A string identifying an area. The string cannot contain\n"
               "                                 blanks. Replace blanks by '_' for example Cerebellar Vermis\n"
               "                                 is Cerebellar_Vermis. You can also use the abbreviated \n" 
               "                                 version cereb_ver and the program will try to guess at \n"
               "                                 what you want and offer suggestions if it can't find the\n"
               "                                 area or if there is ambiguity. Abbreviations are formed\n"
               "                                 by truncating the components (chunks) of an area's name (label).\n"
               "                                 For example:\n"
               "                              1- TT_Daemon::ant_cing specifies the bilateral\n"
               "                                 anterior cingulate in the TT_Daemon atlas.\n"
               "                              2- CA_N27_ML:left:hippo specifies the left\n"
               "                                 hippocampus in the CA_N27_ML atlas.\n"
               "                              3- CA_N27_MPM:right:124 specifies the right\n"
               "                                 ROI with integer code 124 in the CA_N27_MPM atlas\n"
               "                              4- CA_N27_ML::cereb_ver seeks the Cerebellar\n"
               "                                 Vermis in the CA_N27_ML atlas. However there\n"
               "                                 many distinct areas with this name so the program\n"
               "                                 will return with 'potential matches' or suggestions.\n"
               "                                 Use the suggestions to refine your query. For example:\n"
               "                                 CA_N27_ML::cereb_vermis_8\n"
               " -mask_atlas_region REGION_CODE: Same as -show_atlas_region, plus\n"
               "                                 write out a mask dataset of the region.\n"
               " -prefix PREFIX: Prefix for the output mask dataset\n"
               " -dbg DEBUG: Debug flag\n"
               /*" -bmask BIN_ROI_MASK\n"*/
               "\n"
               "Note on the reported coordinates of the Focus Point:\n"
               "Coordinates of the Focus Point are reported in 3 coordinate spaces.\n"
               "The 3 spaces are Talairach (TLRC), MNI, MNI Anatomical (MNI Anat.). All three\n"
               "coordinates are reported in the LPI coordinate order.\n"
               "The TLRC coordinates follow the convention specified by the Talairach and Tournoux Atlas.\n"
               "The MNI coordinates are derived from the TLRC ones using an approximation equation.\n"
               "The MNI Anat. coordinates are a shifted version of the MNI coordinates (see Eickhoff et al. 05).\n"
               " However since the MNI coordinates reported here are derived from TLRC by an approximate \n"
               " function it is best to derive the MNI Anat. coordinates in a different manner.\n"
               " This is possible because the MNI Anat. coordinates are defined relative to the single-subject N27 \n"
               " dataset. MNI Anat. coordinates are thus derived via the 12 piece-wise linear transformations \n"
               " used to put the MNI N27 brain in TLRC space.\n" 
               "\n"
               "Installing Atlases:\n"
               "   Atlases are stored as AFNI datasets, plus perhaps an extra file or two.\n"
               "   These files should be placed in a location that AFNI can find. \n"
               "   Let us refer to this directory as ATLAS_DIR, usually it is the same as\n"
               "   the directory in which AFNI's binaries (such as the program afni) reside.\n"
               "   At a minimum, you need the TTatlas+tlrc dataset present to activate the AFNI\n"
               "   'whereami' feature. To install it, if you do not have it already, download \n"
               "   TTatlas+tlrc* from this link: http://afni.nimh.nih.gov/pub/dist/tgz/\n"
               "   and move TTatlas+tlrc* to ATLAS_DIR.\n"
               "   The Anatomy Toolbox atlases are in archives called CA_EZ_v*.tgz with *\n"
               "   indicating a particular version number. Download the archive from:\n"
               "   http://afni.nimh.nih.gov/pub/dist/tgz/, unpack it and move all the \n"
               "   files in the upacked directory into ATLAS_DIR.\n"
               "\n"
               "Questions Comments:\n"
               "   Ziad S. Saad   (ziad@nih.gov)\n"
               "   SSCC/NIMH/NIH/DHHS/USA\n" 
               "\n");
   EXRETURN;
}
int main(int argc, char **argv)
{
   float x, y, z;
   char *string, *fstring, atlas_name[256], *sfp=NULL, *shar = NULL;
   int output = 0;
   int first = 1, num = 0;
   int a, nakedland = 0, k = 0, Show_Atlas_Code=0;
   int iarg, dicom = 1, i, nakedarg, arglen;
   AFNI_ATLAS *aa = NULL;
   AFNI_ATLAS_REGION *aar= NULL;
   int *imatch=NULL, nmatch=0;
   byte isatlasused[NUMBER_OF_ATLASES];
   AFNI_ATLAS_CODES ac, atlaslist[NUMBER_OF_ATLASES]={ UNKNOWN_ATLAS, UNKNOWN_ATLAS, UNKNOWN_ATLAS, UNKNOWN_ATLAS  };
   byte OldMethod = 0;
   int N_atlaslist = 0, nbest = 0;
   byte atlas_sort = 1, LocalHead = 0, write_mask=0;
   ATLAS_SEARCH *as=NULL;
   char *mskpref= NULL, *bmsk = NULL;
   

   mskpref = NULL; 
   bmsk = NULL;   
   write_mask = 0;
   dicom = 1;
   output = 0;
   OldMethod = 0;
   for (k=0; k < NUMBER_OF_ATLASES; ++k)  isatlasused[k] = 0;
   iarg = 1 ; nakedarg = 0; Show_Atlas_Code = 0; shar = NULL;
   sprintf(atlas_name, "TT_Daemon");
   while( iarg < argc ){
      arglen = strlen(argv[iarg]);
      if(!isnakedarg(argv[iarg])) {
         /* fprintf(stderr, "Not naked!\n"); */
         /* Parse land */
         nakedland = 0;
         #ifdef USE_TRACING
               if( strncmp(argv[iarg],"-trace",5) == 0 ){
                  DBG_trace = 1 ;
                  iarg++ ; continue ;
               }
               if( strncmp(argv[iarg],"-TRACE",5) == 0 ){  
                  DBG_trace = 2 ;
                  iarg++ ; continue ;
               }
         #endif
         for (i=1;i<arglen; ++i) argv[iarg][i] = tolower(argv[iarg][i]);

         if (strcmp(argv[iarg],"-spm") == 0 || strcmp(argv[iarg],"-lpi") == 0) { 
            dicom = 0; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-help") == 0 ) { 
            whereami_usage();
            return(1); 
            continue; 
         }
         if (strcmp(argv[iarg],"-old") == 0 ) { 
            OldMethod = 1; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-dicom") == 0 || strcmp(argv[iarg],"-rai") == 0) { 
            dicom = 1; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-zone_sort") == 0 || strcmp(argv[iarg],"-radius_sort") == 0) { 
            atlas_sort = 0; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-atlas_sort") == 0 ) { 
            atlas_sort = 1; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-classic") == 0 ) { 
            output = 0; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-tab") == 0 ) { 
            output = 1; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-atlas") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Need parameter after -atlas\n"); return(1);
            }
            if (N_atlaslist >= NUMBER_OF_ATLASES) { 
               fprintf(stderr,"** Too many atlases specified.\n");
               return(1);
            }
            if ( strcmp(argv[iarg],Atlas_Code_to_Atlas_Name(AFNI_TLRC_ATLAS)) == 0 ) {
               if (!isatlasused[AFNI_TLRC_ATLAS]) {
                  isatlasused[AFNI_TLRC_ATLAS] = 1;
                  atlaslist[N_atlaslist] = AFNI_TLRC_ATLAS;
                  ++N_atlaslist;
               }
            } else if (strcmp(argv[iarg],Atlas_Code_to_Atlas_Name(CA_EZ_N27_MPM_ATLAS) ) == 0) {
               if (!isatlasused[CA_EZ_N27_MPM_ATLAS]) {
                  isatlasused[CA_EZ_N27_MPM_ATLAS] = 1;
                  atlaslist[N_atlaslist]= CA_EZ_N27_MPM_ATLAS; 
                  ++N_atlaslist;
               }
            } else if (strcmp(argv[iarg],Atlas_Code_to_Atlas_Name(CA_EZ_N27_ML_ATLAS)) == 0) {
               if (!isatlasused[CA_EZ_N27_ML_ATLAS]) {
                  isatlasused[CA_EZ_N27_ML_ATLAS] = 1;   
                  atlaslist[N_atlaslist]= CA_EZ_N27_ML_ATLAS; 
                  ++N_atlaslist;
               }
            } else if (strcmp(argv[iarg],Atlas_Code_to_Atlas_Name(CA_EZ_N27_PMAPS_ATLAS)) == 0) {
               if (!isatlasused[CA_EZ_N27_PMAPS_ATLAS]) {
                  isatlasused[CA_EZ_N27_PMAPS_ATLAS] = 1;   
                  atlaslist[N_atlaslist]= CA_EZ_N27_PMAPS_ATLAS; 
                  ++N_atlaslist;
               }
            } else if (strcmp(argv[iarg],Atlas_Code_to_Atlas_Name(CA_EZ_N27_LR_ATLAS)) == 0) {
               if (!isatlasused[CA_EZ_N27_LR_ATLAS]) {
                  isatlasused[CA_EZ_N27_LR_ATLAS] = 1;   
                  atlaslist[N_atlaslist]= CA_EZ_N27_LR_ATLAS; 
                  ++N_atlaslist;
               }
            } else if (strcmp(argv[iarg],"CA_EZ") == 0) {
               if (!isatlasused[CA_EZ_N27_MPM_ATLAS]) { 
                  atlaslist[N_atlaslist]= CA_EZ_N27_MPM_ATLAS; 
                  ++N_atlaslist;
               }
               if (!isatlasused[CA_EZ_N27_ML_ATLAS]) {
                  atlaslist[N_atlaslist]= CA_EZ_N27_ML_ATLAS; 
                  ++N_atlaslist;
               }
               if (!isatlasused[CA_EZ_N27_PMAPS_ATLAS]) {
                  atlaslist[N_atlaslist]= CA_EZ_N27_PMAPS_ATLAS; 
                  ++N_atlaslist;
               }
               isatlasused[CA_EZ_N27_MPM_ATLAS] = 1;
               isatlasused[CA_EZ_N27_ML_ATLAS] = 1;   
               isatlasused[CA_EZ_N27_PMAPS_ATLAS] = 1;
            } else {
               fprintf(stderr,"** Atlas name %s is not recognized\n", argv[iarg]);
               return(1);
            }
            sprintf(atlas_name,"%s",argv[iarg]);
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-show_atlas_code") == 0) {
            Show_Atlas_Code = 1;
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-show_atlas_region") == 0 || strcmp(argv[iarg],"-mask_atlas_region") == 0) {
            if (strncmp(argv[iarg],"-mask", 4) == 0) write_mask = 1;
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Need parameter after -show_atlas_region/-mask_atlas_region\n"); return(1);
            }            
            shar = argv[iarg];
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-dbg") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Need parameter after -dbg\n"); return(1);
            }            
            LocalHead = MIN_PAIR(atoi(argv[iarg]), 4);
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-prefix") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Need parameter after -prefix\n"); return(1);
            }            
            mskpref = argv[iarg];
            ++iarg;
            continue; 
         }
         
         if (strcmp(argv[iarg],"-bmask") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Need parameter after -bmask\n"); return(1);
            }            
            bmsk = argv[iarg];
            ++iarg;
            continue; 
         }
         { /* bad news in tennis shoes */
            fprintf(stderr,"** bad option %s\n", argv[iarg]);
            return 1;
         }
      
      } else {
         /* xyz format */
         if (nakedarg && !nakedland) {
            fprintf(stderr,"** Keep x, y, z and output options together\n"
                           "argument %s not appropriate here.\n", argv[iarg]);
            return 1;
         }
         if (nakedarg == 0) { x = atof(argv[iarg]); nakedland = 1; }
         else if (nakedarg == 1) y = atof(argv[iarg]);
         else if (nakedarg == 2) z = atof(argv[iarg]);
         else if (nakedarg == 3) output = atoi(argv[iarg]);
         ++nakedarg;
         ++iarg;
         continue;
      }
      
      fprintf(stderr,"** Shouldn't be here Jim! (%s)\n", argv[iarg]);
      return 1;
   }
   
   if (N_atlaslist == 0) {
      /* use all */
      atlaslist[N_atlaslist] = AFNI_TLRC_ATLAS; ++N_atlaslist; isatlasused[AFNI_TLRC_ATLAS] = 1;
      atlaslist[N_atlaslist] = CA_EZ_N27_MPM_ATLAS; ++N_atlaslist; isatlasused[CA_EZ_N27_MPM_ATLAS] = 1;
      atlaslist[N_atlaslist] = CA_EZ_N27_ML_ATLAS; ++N_atlaslist; isatlasused[CA_EZ_N27_ML_ATLAS] = 1;
      atlaslist[N_atlaslist] = CA_EZ_N27_PMAPS_ATLAS; ++N_atlaslist; isatlasused[CA_EZ_N27_PMAPS_ATLAS] = 1;
   }
   
   if (nakedarg < 3 && !Show_Atlas_Code && !shar) {
      whereami_usage();
      return 1;
   }
   
   if (LocalHead) Set_Show_Atlas_Mode(LocalHead);

   if (Show_Atlas_Code) {
      for (k=0; k < N_atlaslist; ++k) {
         aa = Build_Atlas(atlaslist[k]);  
         Show_Atlas(aa); 
         aa = Free_Atlas(aa);
      }
   }
   
   if (shar) {
         Set_ROI_String_Decode_Verbosity(1); /* help the user */
         /* Do the match business */
         ac = UNKNOWN_ATLAS;
         if (!(aar = ROI_String_Decode(shar, &ac))) {
            ERROR_message("ROI string decoding failed.");
         } else {
            if (LocalHead) { 
               fprintf(stderr,"User seeks the following region in atlas %s:\n", Atlas_Code_to_Atlas_Name(ac));
               Show_Atlas_Region(aar);  
            }
            /* is this an OK atlas */
            if (ac <= UNKNOWN_ATLAS || ac >= NUMBER_OF_ATLASES) {
               ERROR_message("Atlas not found");
               exit(1);
            }
            if (aar->N_chnks < 1 && aar->id <= 0) {
               ERROR_message("bad or empty label");
               exit(1);
            }
            if (!(aa = Build_Atlas(ac))) {
               ERROR_message("Failed to build atlas");
               exit(1);
            }
           
            if (LocalHead > 1) Show_Atlas(aa); 
            as = Find_Atlas_Regions(aa,aar, NULL);
            /* analyze the matches, remember no left/right decisions made yet, and even if labels are present, 
               right/left sides may not have different ids in atlas...  */
            string = Report_Found_Regions(aa, aar, as, &nbest);
            if (string) {
               fprintf(stderr,"%s\n", string);   
            } else {
               ERROR_message("NULL string returned");
               exit(1);
            }
            /* Now we know what matches, give me a mask */
            if (nbest && write_mask) {
               int codes[3], n_codes;
               THD_3dim_dataset *maskset=NULL;
               
               if (nbest > 2) {
                  ERROR_message("Should not get this");
                  exit(1);
               }
               n_codes = 1;
               codes[0] = aa->reg[as->iloc[0]]->id;
               if (nbest == 2) {
                  if (aa->reg[as->iloc[0]]->id != aa->reg[as->iloc[1]]->id) {
                     n_codes = 2;
                     codes[1] = aa->reg[as->iloc[1]]->id;
                  }
               }
               if (!(maskset = Atlas_Region_Mask(ac, aar, codes, n_codes))) {
                  ERROR_message("Failed to create mask");
                  exit(1);
               } else {
                  tross_Make_History( "whereami" , argc, argv , maskset ) ;
                  if (mskpref) {
                        EDIT_dset_items(  maskset,
                          ADN_prefix    , mskpref,
                           ADN_none ) ;
                  }
                  if( THD_is_file(DSET_HEADNAME(maskset)) ) {
                     ERROR_message("Output dataset %s already exists -- can't overwrite", DSET_HEADNAME(maskset)) ;
                     exit(1);
                  }
                  if (LocalHead) {
                     fprintf(stderr,"Writing ROI mask to %s...\n", DSET_HEADNAME(maskset));
                  }
                  DSET_write(maskset) ;
                  DSET_delete(maskset); maskset = NULL;
               }
            }
            aar = Free_Atlas_Region(aar);
            if (as) as = Free_Atlas_Search(as); 
            if (string) free(string); string = NULL;
         } 
         aa = Free_Atlas(aa);
   }
   

   if (nakedarg < 3) { /* nothing left to do */
      return(0);
   }

   if (!dicom) {
      /* go from lpi to rai */
      x = -x;
      y = -y; 
   }

  
   if (OldMethod) {
     string = TT_whereami_old(x,y,z);
     if (string == NULL ) {                              /* 30 Apr 2005 [rickr] */
       fprintf(stderr,"** whereami lookup failure: is TTatlas+tlrc/TTatlas.nii.gz available?\n");
       fprintf(stderr,"   (the TTatlas+tlrc or TTatlas.nii.gz dataset must be in your PATH)\n");
       return 1;
     }

     #if 0 /* ZSS does not work Fri Jan 20 15:54:41 EST 2006 */
     if (output == 1) {
       fstring = malloc(sizeof(string));
       strncpy(fstring, "Focus point", 11);
       num = 11;
       for(a = 0; string[a] != '\0'; a++) {
         /* remove header info up to Focus point:
             remove newlines as you go; once you hit a *, stop */
         if ((string[a] != ':') && (first == 1)) {
           continue;
         }
         first = 0;
         if ((string[a] == ' ') && (string[a-1] == ' ')) {
           continue;
         }
         if ((string[a] == '*')) {
           fstring[num] = '\0';
           printf("%s\n", fstring);
           break;
         }
         if ((string[a] != '\n')) {
           if (string[a] == 'W') {
             fstring[num++] = '\t';
           }
           fstring[num++] = string[a];
         } else {
           fstring[num++] = ' ';
         }
       }
         free(fstring);
     } else {
       printf("%s\n", string);
     }
     #else
      if (output == 1) { 
         /* ZSS: my best interpretation of the original intent of output == 1 */
         fstring = strdup(string);
         /* ignore everything up till Focus point */
         sfp = strstr(string,"Focus point");
         if (!sfp) {
            fprintf(stderr,"** Failed to find 'Focus point' string.\n"
                           "This is a beuge please inform the authors.\n");
            return(1);
         }
         /* copy all the rest, replacing each new line followed by a non blank with a tab. */
         k = 0;
         while (*sfp != '\0' && sfp < string+strlen(string)) {
            if (*sfp == '\n') { /* new line encountered */
               /* put a tab in copy string */
               fstring[k] = '\t'; ++k;
               /* skip until next character */
               while (!zischar(*sfp) &&  sfp < string+strlen(string)) ++ sfp;
            }else{
               /* add the character */
               fstring[k] = *sfp; ++k; ++ sfp;
            }

         }
         fstring[k] = '\0';
         fprintf(stdout,"%s\n", fstring);
         free(fstring); fstring = NULL;
      } else {
         printf("%s\n", string);
      }
     #endif
   } /* end TT_Daemon */

   
   if (!OldMethod) {
      /* Set TT_whereami's atlas list */
      for (k=0; k < N_atlaslist; ++k) {
         TT_whereami_add_atlas(atlaslist[k]);
      }
      
      /* the new whereami */
      if (atlas_sort) {
         if (output == 1) TT_whereami_set_outmode (TAB1_WAMI_ATLAS_SORT);
         else TT_whereami_set_outmode (CLASSIC_WAMI_ATLAS_SORT);
      } else {
         if (output == 1) TT_whereami_set_outmode (TAB1_WAMI_ZONE_SORT);
         else TT_whereami_set_outmode (CLASSIC_WAMI_ZONE_SORT);
      }
      string = TT_whereami(x,y,z);
      if (string) fprintf(stdout,"%s\n", string);
      else fprintf(stdout,"whereami NULL string out.\n");
      if (string) free(string); string = NULL;            
   }
    
   if (bmsk) {
      byte *bmask_vol = NULL;
      THD_3dim_dataset *mset=NULL;
      ATLAS_DSET_HOLDER adh;
      /* load the mask dset */
	   if (!(mset = THD_open_dataset (bmsk))) {
         fprintf(stderr,"** Failed to open mask set %s.\n", bmsk);
         return(1);
      } 

	   if (!(bmask_vol = THD_makemask( mset , 0 , 1.0,0.0 ))) {  /* get all non-zero values */
         fprintf(stderr,"** No byte for you.\n");
         return(1);
      }
      
      /* for each atlas */
      for (k=0; k < N_atlaslist; ++k) {
         adh = Atlas_With_Trimming (atlaslist[k], 0);
         if (!adh.dset) {
            fprintf(stderr,"** Warning: Atlas %s could not be loaded.\n", Atlas_Code_to_Atlas_Name(atlaslist[k]));
            continue;
         }  
         /* resample mask per atlas, use linear interpolation, cut-off at 0.5 */

            /* Get range of integral values in atlas, create counting array Max_Range elements long*/

            /* for each sub-brick sb */
                /* if mask[i] ++count[atlas_sb[i]]; */
      }
         
   }
   
return 0;
}
