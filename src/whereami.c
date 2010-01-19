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

char *PrettyRef(char *ref) {
   int i=0;
   char *pj=NULL, *pea=NULL;
   static char strbuf[500];
   
   pj = strstr(ref, "-> ");
   if (!pj || pj == ref) return(ref);
   
   pea = pj; /* now go back until no more - are found */
   while (pea[0] == '-') --pea; 
   
   pj = pj + 3; /* start of journal reference */
   
   /* copy name of area */
   i = 0;
   while (ref<=pea) {
      strbuf[i] = *ref; ++ref;++i;
   }
   strbuf[i] = '\0';
   
   /* now add the reference */
   snprintf(strbuf, 490*sizeof(char), "%s\n            -> %s", strbuf, pj);
   
   return(strbuf);
}

void whereami_usage(void) 
{
   int i = 0;
   
   ENTRY("whereami_usage");
      printf(  
"Usage: whereami [x y z [output_format]] [-lpi/-spm] [-atlas ATLAS] \n"
"   ++ Reports brain areas located at x y z mm in TLRC space according \n"
"   to atlases present with your AFNI installation.\n"
"   ++ Show the contents of available atlases\n"
"   ++ Extract ROIs for certain atlas regions using symbolic notation\n"
"   ++ Report on the overlap of ROIs with Atlas-defined regions.\n"
"\n"
"Options (all options are optional):\n"
"-----------------------------------\n"
"    x y z [output_format] : Specifies the x y z coordinates of the \n"
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
" -coord_file XYZ.1D: Input coordinates are stored in file XYZ.1D\n"
"                     Use the '[ ]' column selectors to specify the\n"
"                     X,Y, and Z columns in XYZ.1D.\n"
"                     Say you ran the following 3dclust command:\n"
"           3dclust -1Dformat -1clip 0.3  5 3000 func+orig'[1]' > out.1D\n"
"                     You can run whereami on each cluster's center\n"
"                     of mass with:\n"
"           whereami -coord_file out.1D'[1,2,3]' -tab\n"
"               NOTE: You cannot use -coord_file AND specify x,y,z on\n"
"                     command line.\n" 
" -lpi/-spm: Input coordinates' orientation is in LPI or SPM format. \n"
" -rai/-dicom: Input coordinates' orientation is in RAI or DICOM format.\n"
" NOTE: The default format for input coordinates' orientation is set by \n"
"       AFNI_ORIENT environment variable. If it is not set, then the default \n"
"       is RAI/DICOM\n"
" -space SPC: Space of input coordinates.\n"
"       SPC can be MNI, MNI_ANAT or TLRC which is the default.\n"
"       If SPC is either MNI or MNI_ANAT space, the x,y,z coordinates are\n"
"       transformed to TLRC space prior to whereami query.\n"
" -classic: Classic output format (output_format = 0).\n"
" -tab: Tab delimited output (output_format = 1). \n"
"       Useful for spreadsheeting.\n"
" -atlas ATLAS: Use atlas ATLAS for the query.\n"
"               You can use this option repeatedly to specify\n"
"               more than one atlas. Default is all available atlases.\n"
"               ATLAS is one of:\n"
"   %-12s: Created by tracing Talairach and Tournoux brain illustrations.\n"
"   Generously contributed by Jack Lancaster and Peter Fox of RIC UTHSCSA)\n"
"\n"
"   %-12s: Anatomy Toolbox's atlases, some created from cytoarchitectonic \n"
"   %-12s: studies of 10 human post-mortem brains (CA_N27_MPM, CA_N27_PM). \n"
"   %-12s: Generously contributed by Simon Eickhoff,\n"
"   %-12s: Katrin Amunts and Karl Zilles of IME, Julich, \n"
"   Germany. Please take into account the references and abide by the \n"
"   warning below (provided with the Anatomy toolbox) when using these \n"
"   atlases:\n", 
               Atlas_Code_to_Atlas_Name(AFNI_TLRC_ATLAS),
               Atlas_Code_to_Atlas_Name(CA_EZ_N27_MPM_ATLAS),
               Atlas_Code_to_Atlas_Name(CA_EZ_N27_ML_ATLAS),
               Atlas_Code_to_Atlas_Name(CA_EZ_N27_PMAPS_ATLAS),
               Atlas_Code_to_Atlas_Name(CA_EZ_N27_LR_ATLAS));                       
      i = 0;
      printf(  "   Anatomy Toolbox Reference and Warning:\n"
               "   --------------------------------------\n" );
      do { 
         printf(  "      %s\n" , PrettyRef(CA_EZ_REF_STR[i]));
         ++i;  
      } while (CA_EZ_REF_STR[i][0] != '\0');
              /* "      [1] Auditory cortex (TE 1.0, TE 1.1, TE 1.2) : Morosan et al., NeuroImage, 2001\n"
               "      [2] Broca's area (BA 44, BA 45) : Amunts et al., J Comp Neurol, 1999\n"
               "      [3] Motor cortex (BA 4a, BA 4p, BA 6) : Geyer et al., Nature, 1996 ; S. Geyer, Springer press 2003\n"
               "      [4] Somatosensory cortex (BA 3a, BA 3b, BA 1 BA 2) : Geyer et al., Neuroimage, 1999 + 2000 ; Grefkes et al., Neuroimage, 2001\n"
               "      [5] Parietal operculum / SII (OP 1, OP 2, OP 3, OP 4) : Eickhoff et al., Cerebral Cortex, 2005a,b\n"
               "      [6] Amygdala (CM/LB/SF), Hippocampus (FD/CA /SUB/EC/HATA) : Amunts et al., Anat Embryol, 2005\n"
               "      [7] Visual cortex (BA 17, BA 18) : Amunts et al., Neuroimage, 2000\n"
               "      Warning:\n"
               "        All other areas may only be used after consultation (contact S.Eickhoff@fz-juelich.de)\n" */
       printf( 
"   \n"
"   See Eickhoff et al. Neuroimage 25 (2005) for more info on:\n"
"       Probability Maps (CA_N27_PM)\n"
"       and Maximum Probability Maps (CA_N27_MPM)\n");
       printf( 
"   ----------------------------------------------------------\n"
"\n" 
" -atlas_sort: Sort results by atlas (default)\n"
" -zone_sort | -radius_sort: Sort by radius of search\n"
" -old : Run whereami in the olde (Pre Feb. 06) way.\n"
" -show_atlas_code: Shows integer code to area label map of the atlases\n"
"                   in use. The output is not too pretty because\n"
"                   the option is for debugging use.\n"
" -show_atlas_region REGION_CODE: You can now use symbolic notation to\n"
"                                 select atlas regions. REGION_CODE has \n"
"                                 three colon-separated elements forming it:\n"
"            Atlas_Name:Side:Area.\n"
"      Atlas_Name: one of the atlas names listed above.\n"
"                  If you do not have a particular atlas in your AFNI\n"
"                  installation, you'll need to download it (see below).\n"
"      Side      : Either left, right or nothing(::) for bilateral.\n"
"      Area      : A string identifying an area. The string cannot contain\n"
"                  blanks. Replace blanks by '_' for example Cerebellar Vermis\n"
"                  is Cerebellar_Vermis. You can also use the abbreviated \n" 
"                  version cereb_ver and the program will try to guess at \n"
"                  what you want and offer suggestions if it can't find the\n"
"                  area or if there is ambiguity. Abbreviations are formed\n"
"                  by truncating the components (chunks) of an area's name \n"
"                  (label). For example:\n"
"               1- TT_Daemon::ant_cing specifies the bilateral\n"
"                  anterior cingulate in the TT_Daemon atlas.\n"
"               2- CA_N27_ML:left:hippo specifies the left\n"
"                  hippocampus in the CA_N27_ML atlas.\n"
"               3- CA_N27_MPM:right:124 specifies the right\n"
"                  ROI with integer code 124 in the CA_N27_MPM atlas\n"
"               4- CA_N27_ML::cereb_ver seeks the Cerebellar\n"
"                  Vermis in the CA_N27_ML atlas. However there\n"
"                  many distinct areas with this name so the program\n"
"                  will return with 'potential matches' or suggestions.\n"
"                  Use the suggestions to refine your query. For example:\n"
"                  CA_N27_ML::cereb_vermis_8\n"
" -mask_atlas_region REGION_CODE: Same as -show_atlas_region, plus\n"
"                                 write out a mask dataset of the region.\n"
" -prefix PREFIX: Prefix for the output mask dataset\n"
" -max_areas MAX_N: Set a limit on the number of distinct areas to report.\n"
"             This option will override the value set by the environment\n"
"             variable AFNI_WHEREAMI_MAX_FIND, which is now set to %d\n"
"             The variable  AFNI_WHEREAMI_MAX_FIND should be set in your\n"
"             .afnirc file.\n"
" -max_search_radius MAX_RAD: Set a limit on the maximum searching radius when\n"
"                     reporting results. This option will override the \n"
"                     value set by the environment variable \n"
"                     AFNI_WHEREAMI_MAX_SEARCH_RAD,\n"
"                     which is now set to %f .\n" 
" NOTE: You can turn off some of the whining by setting the environment \n"
"       variable  AFNI_WHEREAMI_NO_WARN\n"            
" -debug DEBUG: Debug flag\n"
" -CA_N27_version: Output the version of the Anatomy Toolbox atlases and quit.\n"
"                  If you get warnings that AFNI's version differs from that \n"
"                  of the atlas' datasets then you will need to download the \n"
"                  latest atlas datasets from AFNI's website. You cannot use \n"
"                  older atlases because the atlas' integer-code to area-label\n"
"                  map changes from one version to the next.\n"
"                  To get the version of the atlas' datasets, run 3dNotes \n"
"                  on the atlases and look for 'Version' in one of the notes\n"
"                  printed out.\n" 
"\n"
"Options for determining the percent overlap of ROIs with Atlas-defined areas:\n"
"---------------------------------------------------------------------------\n"
" -bmask MASK: Report on the overlap of all non-zero voxels in MASK dataset\n"
"              with various atlas regions. NOTE: The mask itself is not binary,\n"
"              the masking operation results in a binary mask.\n"
" -omask ORDERED_MASK:Report on the overlap of each ROI formed by an integral \n"
"                     value in ORDERED_MASK. For example, if ORDERED_MASK has \n"
"                     ROIs with values 1, 2, and 3, then you'll get three \n"
"                     reports, one for each ROI value. Note that -omask and\n"
"                     -bmask are mutually exclusive.\n"
" -cmask MASK_COMMAND: command for masking values in BINARY_MASK, \n"
"                      or ORDERED_MASK on the fly.\n"
"        e.g. whereami -bmask JoeROIs+tlrc \\\n"
"                      -cmask '-a JoeROIs+tlrc -expr equals(a,2)'\n"
"              Would set to 0, all voxels in JoeROIs that are not\n"
"              equal to 2.\n"
"        Note that this mask should form a single sub-brick,\n"
"        and must be at the same resolution as BINARY_MASK or ORDERED_MASK.\n"
"        This option follows the style of 3dmaskdump (since the\n"
"        code for it was, uh, borrowed from there (thanks Bob!, thanks Rick!)).\n"
"        See '3dmaskdump -help' for more information.\n"
"\n"
"Note on the reported coordinates of the Focus Point:\n"
"----------------------------------------------------\n"
"  Coordinates of the Focus Point are reported in 3 coordinate spaces.\n"
"The 3 spaces are Talairach (TLRC), MNI, MNI Anatomical (MNI Anat.). \n"
"All three coordinates are reported in the LPI coordinate order.\n"
"  The TLRC coordinates follow the convention specified by the Talairach and \n"
"     Tournoux Atlas.\n"
"  The MNI coordinates are derived from the TLRC ones using an approximation \n"
"     equation.\n"
"  The MNI Anat. coordinates are a shifted version of the MNI coordinates \n"
"     (see Eickhoff et al. 05).\n"
"\n"
"  However because the MNI coordinates reported here are derived from TLRC \n"
"by an approximate function it is best to derive the MNI Anat. coordinates \n"
"in a different manner. This option is possible because the MNI Anat. \n"
"coordinates are defined relative to the single-subject N27 dataset. \n"
"MNI Anat. coordinates are thus derived via the 12 piece-wise \n"
"linear transformations used to put the MNI N27 brain in TLRC space.\n" 
"\n"
"Installing Atlases:\n"
"-------------------\n"
"   Atlases are stored as AFNI datasets, plus perhaps an extra file or two.\n"
"   These files should be placed in a location that AFNI can find. \n"
"   Let us refer to this directory as ATLAS_DIR, usually it is the same as\n"
"   the directory in which AFNI's binaries (such as the program afni) reside.\n"
"   At a minimum, you need the TTatlas+tlrc dataset present to activate the \n"
"   AFNI 'whereami' feature. To install it, if you do not have it already, \n"
"   download TTatlas+tlrc* from this link: \n"
"   http://afni.nimh.nih.gov/pub/dist/tgz/\n"
"   and move TTatlas+tlrc* to ATLAS_DIR.\n"
"   The Anatomy Toolbox atlases are in archives called CA_EZ_v*.tgz with *\n"
"   indicating a particular version number. Download the archive from:\n"
"   http://afni.nimh.nih.gov/pub/dist/tgz/, unpack it and move all the \n"
"   files in the unpacked directory into ATLAS_DIR.\n"
"\n"
"How To See Atlas Data In AFNI as datasets:\n"
"------------------------------------------\n"
"   If you want to view the atlases in the same session\n"
"   that you are working with, choose one of options below.\n"
"   For the sake of illustrations, I will assume that atlases\n"
"   reside in directory: /user/abin/\n"
" 1-Load the session where atlases reside on afni's command\n"
"   line: afni ./ /user/abin\n" 
" 2-Set AFNI's environment variable AFNI_GLOBAL_SESSION\n"
"   to the directory where the atlases reside.\n"
"   You can add the following to you .afnirc file:\n"
"   AFNI_GLOBAL_SESSION = /user/abin\n"
"   Or, for a less permanent solution, you can set this environment\n"
"   variable in the shell you are working in with (for csh and tcsh):\n"
"   setenv AFNI_GLOBAL_SESSION /user/abin \n"
"   ***********\n"
"   BE CAREFUL: Do not use the AFNI_GLOBAL_SESSION approach\n"
"   *********** if the data in your session is not already \n"
"   written in +tlrc space. To be safe, you must have\n"
"   both +tlrc.HEAD and +tlrc.BRIK for all datasets\n"
"   in that session (directory). Otherwise, if the anat parents are\n"
"   not properly set, you can end up applying the +tlrc transform\n"
"   from one of the atlases instead of the proper anatomical \n"
"   parent for that session.\n"
"\n"
"   Note: You can safely ignore the:\n"
"              ** Can't find anat parent ....  \n"
"         messages for the Atlas datasets.\n"
"\n"
"Convenient Colormaps For Atlas Datasets:\n"
"----------------------------------------\n"
"   New colormaps (colorscales) have been added\n"
"   to AFNI to facilitate integral valued datasets\n"
"   like ROIs and atlases. Here's what to do:\n"
"     o set the color map number chooser to '**' \n"
"     o right-click on the color map and select 'Choose Colorscale'\n"
"     o pick one of: CytoArch_ROI_256, CytoArch_ROI_256_gap, ROI_32. etc.\n"
"     o set autorange off and set the range to the number of colors \n"
"       in the chosen map (256, 32, etc.). \n"
"       Color map CytoArch_ROI_256_gap was created for the proper viewing\n"
"       of the Maximum Probability Maps of the Anatomy Toolbox.\n"
"\n"
"How To See Atlas regions overlaid in the AFNI GUI:\n"
"--------------------------------------------------\n"
"   To see specific atlas regions overlaid on underlay and other overlay data,\n"
"     1. In Overlay control panel, check \"See TT Atlas Regions\" \n"
"     2. Switch view to Talairach in View Panel\n"
"     3. Right-click on image and select \"-Atlas colors\". In the Atlas colors\n"
"        menu, select the colors you would like and then choose Done.\n"
"     The images need to be redrawn to see the atlas regions, for instance,\n"
"        by changing slices. Additional help is available in the Atlas colors\n"
"        menu.\n"
"   For the renderer plug-in, the underlay and overlay datasets should both\n"
"     have Talairach view datasets actually written out to disk\n"
"   The whereami and \"Talairach to\" functions are also available by right-\n"
"     clicking in an image window.\n\n"
"Examples:\n"
"_________\n"
"   To find a cluster center close to the top of the brain at -12,-26, 76 (LPI),\n"
"   whereami, assuming the coordinates are in Talairach space, would report:\n"
"   > whereami -12 -26 76 -lpi\n"
"   > Focus point (LPI)= \n"
"   -12 mm [L], -26 mm [P], 76 mm [S] {T-T Atlas}\n\n"
"   Atlas CA_N27_MPM: Cytoarch. Max. Prob. Maps (N27)\n"
"   Within 4 mm: Area 6\n"
"   Within 7 mm: Area 4a\n"
"\n"
"   Atlas CA_N27_ML: Macro Labels (N27)\n"
"   Within 1 mm: Left Paracentral Lobule\n"
"   Within 6 mm: Left Precentral Gyrus\n"
"   -AND- Left Postcentral Gyrus\n"
"\n"
"   To create a mask dataset of both the left and right amygdala, you can do the\n"
"   following (although masks and datasets can be specified in the same way for\n"
"   other afni commands, so a mask, very often, is not needed as a separate\n"
"   dataset):\n"
"   > whereami -prefix amymask -mask_atlas_region 'TT_Daemon::amygdala'\n\n"
"Questions Comments:\n"
"-------------------\n"
"   Ziad S. Saad   (saadz@mail.nih.gov)\n"
"   SSCC/NIMH/NIH/DHHS/USA\n"
"\n"
"Thanks to Kristina Simonyan for feedback and testing.\n"
"\n" 
"\n",Init_Whereami_Max_Find(), Init_Whereami_Max_Rad());

   PRINT_COMPILE_DATE ;
   EXRETURN;
}

/*----------------------------------------------------------------------------*/
extern int * SUMA_Dijkstra_generic (int N_Node, 
                     float *NodeList, int NodeDim, int dist_metric,
                     int *N_Neighbv, int **FirstNeighb, float **FirstNeighbDist,
                     int Nx, int Ny, 
                     byte *isNodeInMeshp, 
                     int *N_isNodeInMesh, int Method_Number, 
                     float *Lfinal, int *N_Path,
                     int verb);

int main(int argc, char **argv)
{
   float x, y, z, xi, yi, zi, tx, ty, tz;
   char *string, *fstring, atlas_name[256], *sfp=NULL, *shar = NULL;
   int output = 0;
   int first = 1, num = 0;
   int a, nakedland = 0, k = 0, Show_Atlas_Code=0;
   int iarg, dicom = 1, i, nakedarg, arglen, ixyz=0, nxyz=0;
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
   byte *cmask=NULL ; int ncmask=0 ;
   int dobin = 0, N_areas, mni;
   char *coord_file=NULL;
   float *coord_list = NULL, rad;
   THD_fvec3 tv, m;
   byte b1;
   
   b1 = 0;
   mni = -1;
   dobin = 0;
   ncmask=0 ;
   cmask=NULL ;
   mskpref = NULL; 
   bmsk = NULL;   
   write_mask = 0;
   dicom = -1; /* not set */
   output = 0;
   rad = -1.0;
   N_areas = -1;
   OldMethod = 0;
   coord_file = NULL;
   for (k=0; k < NUMBER_OF_ATLASES; ++k)  isatlasused[k] = 0;
   iarg = 1 ; nakedarg = 0; Show_Atlas_Code = 0; shar = NULL;
   sprintf(atlas_name, "TT_Daemon");
   xi = 0.0; yi=0.0, zi=0.0;
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
         if (strcmp(argv[iarg],"-ca_n27_version") == 0) { 
            fprintf(stdout,"Anatomy Toolbox Version in AFNI is:\n%s\n", CA_EZ_VERSION_STR);  
            return(0);
         }
         
         if (  strcmp(argv[iarg],"-rai") == 0 || 
               strcmp(argv[iarg],"-dicom") == 0) { 
            dicom = 1; 
            ++iarg;
            continue; 
         }
         
         if (  strcmp(argv[iarg],"-for_daniel") == 0 ) { 
            b1 = 1; 
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
         if (strcmp(argv[iarg],"-space") == 0) { 
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -space\n"); return(1);
            }
            if (strcmp(argv[iarg],"MNI") == 0 || strcmp(argv[iarg],"mni") == 0) {
               mni = 1; 
            } else if (strcasecmp(argv[iarg],"MNI_ANAT") == 0){
               mni = 2;
            } else if (strcmp(argv[iarg],"TLRC") == 0 || strcmp(argv[iarg],"tlrc") == 0) {
               mni = 0; 
            } else {
               fprintf(stderr,"** Error: %s is invalid. Must use either MNI or TLRC\n", argv[iarg]);
               return(1);
            }
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
               fprintf(stderr,"** Error: Need parameter after -atlas\n"); return(1);
            }
            if (N_atlaslist >= NUMBER_OF_ATLASES) { 
               fprintf(stderr,"** Error: Too many atlases specified.\n");
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
               fprintf(stderr,"** Error: Atlas name %s is not recognized\n", argv[iarg]);
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
               fprintf(stderr,"** Error: Need parameter after -show_atlas_region/-mask_atlas_region\n"); return(1);
            }            
            shar = argv[iarg];
            ++iarg;
            continue; 
         }
         
         if (strcmp(argv[iarg],"-coord_file") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -coord_file\n"); return(1);
            }
            coord_file = argv[iarg];
            ++iarg;
            continue;             
         }
         
         if (strcmp(argv[iarg],"-max_areas") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -max_areas\n"); return(1);
            }
            N_areas = atoi(argv[iarg]);
            if (N_areas < 1 || N_areas > 50) {
               fprintf(stderr,"** Error: -max_areas parameter must be between 1 and 50.\n"); return(1);
            }
            Set_Whereami_Max_Find(N_areas);
            ++iarg;
            continue;             
         }
         
         if (strcmp(argv[iarg],"-max_search_radius") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -max_search_radius\n"); return(1);
            }
            rad = atof(argv[iarg]);
            if (rad < 1.0 || rad > 9.5) {
               fprintf(stderr,"** Error: -max_search_radius parameter must be between 1.0 and 9.5.\n"); return(1);
            }
            Set_Whereami_Max_Rad(rad);
            ++iarg;
            continue;             
         } 
         if (strcmp(argv[iarg],"-dbg") == 0 || strcmp(argv[iarg],"-debug") == 0 ) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -debug\n"); return(1);
            }            
            LocalHead = MIN_PAIR(atoi(argv[iarg]), 4);
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-prefix") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -prefix\n"); return(1);
            }            
            mskpref = argv[iarg];
            ++iarg;
            continue; 
         }
         
         if (strcmp(argv[iarg],"-bmask") == 0 || strcmp(argv[iarg],"-omask") == 0 ) {
            if (strcmp(argv[iarg],"-bmask") == 0) dobin = 1;
            else dobin = 0;
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -bmask\n"); return(1);
            }
            if (bmsk) {
               fprintf(stderr,"** Error: -bmask and -omask are mutually exclusive.\n"); return(1);
            }            
            bmsk = argv[iarg];
            
            ++iarg;
            continue; 
         }
         
         if( strcmp(argv[iarg],"-cmask") == 0 ){  
            if( iarg+1 >= argc ){
               fprintf(stderr,"** Error: -cmask option requires a following argument!\n");
               exit(1) ;
            }
            cmask = EDT_calcmask( argv[++iarg] , &ncmask, 0 ) ;
            if( cmask == NULL ){
               fprintf(stderr,"** Error: Can't compute -cmask!\n"); exit(1);
            }
            iarg++ ; 
            continue ;
         }

         { /* bad news in tennis shoes */
            fprintf(stderr,"** Error: bad option %s\n", argv[iarg]);
            return 1;
         }
      
      } else {
         /* xyz format */
         if (nakedarg && !nakedland) {
            fprintf(stderr,"** Error: Keep x, y, z and output options together\n"
                           "argument %s not appropriate here.\n", argv[iarg]);
            return 1;
         }
         if (nakedarg == 0) { xi = atof(argv[iarg]); nakedland = 1; }
         else if (nakedarg == 1) yi = atof(argv[iarg]);
         else if (nakedarg == 2) zi = atof(argv[iarg]);
         else if (nakedarg == 3) output = atoi(argv[iarg]);
         ++nakedarg;
         ++iarg;
         continue;
      }
      
      fprintf(stderr,"** Error: Shouldn't be here Jim! (%s)\n", argv[iarg]);
      return 1;
   }
   
   if (b1) { 
      /* The secret option for Daniel,
      See NIH-5 labbook pp 46 for graph */
      int N_Neighb_Max = 5; /* max number of nieghbors a node can have*/
      int N_Node = 7;
      int N_Neighb[7];
      int **FirstNeighb=NULL;
      float **FirstNeighbDist=NULL; 
      int N_np=4;
      int np[4][2];
      int ii, kk;
      char prefix[] = {"puns.paths.1D"};
      int *nPath=NULL, N_n=0;
      float nDistance=0.0;
      FILE *fout=stdout, *fpout=NULL;
      
      /* fill number of neighbors for nodes 0 to 6 */
      ii=0;
      N_Neighb[ii++] = 4;      
      N_Neighb[ii++] = 2;
      N_Neighb[ii++] = 3;
      N_Neighb[ii++] = 2;
      N_Neighb[ii++] = 2;
      N_Neighb[ii++] = 0;
      N_Neighb[ii++] = 1;
      
      /* fill neighborhood (edges) and distances */
      FirstNeighb = (int **)calloc(N_Node, sizeof(int*));
      FirstNeighbDist = (float **)calloc(N_Node, sizeof(float*));
      for (ii=0; ii<7;++ii) {
         FirstNeighb[ii] = (int *)calloc(N_Neighb_Max, sizeof(int)); 
         FirstNeighbDist[ii] = (float *)calloc(N_Neighb_Max, sizeof(float)); 
      }
      FirstNeighb[0][0]=2;    FirstNeighbDist[0][0]=1.0; /*1st neighb of node 0*/
      FirstNeighb[0][1]=1;    FirstNeighbDist[0][1]=1.0; /*2nd neighb of node 0*/
      FirstNeighb[0][2]=4;    FirstNeighbDist[0][2]=5.0; /*3rd neighb of node 0*/
      FirstNeighb[0][3]=6;    FirstNeighbDist[0][3]=2.0; /* ... */

      FirstNeighb[1][0]=0;    FirstNeighbDist[1][0]=1.0; /*1st neighb of node 1*/
      FirstNeighb[1][1]=2;    FirstNeighbDist[1][1]=2.0; /*2nd neighb of node 1*/
      
      FirstNeighb[2][0]=1;    FirstNeighbDist[2][0]=2.0; 
      FirstNeighb[2][1]=3;    FirstNeighbDist[2][1]=1.0;
      FirstNeighb[2][2]=0;    FirstNeighbDist[2][2]=1.0;
   
      FirstNeighb[3][0]=4;    FirstNeighbDist[3][0]=2.0;
      FirstNeighb[3][1]=2;    FirstNeighbDist[3][1]=1.0;
   
      FirstNeighb[4][0]=3;    FirstNeighbDist[4][0]=2.0;
      FirstNeighb[4][1]=0;    FirstNeighbDist[4][1]=5.0;
      
      FirstNeighb[5][0]=-1;   /* not necessary, but to emphasize */
      
      FirstNeighb[6][0]=0;    FirstNeighbDist[6][0]=2.0;
      
      if (!(fpout = fopen(prefix,"w"))) {
         fprintf(stderr,"** Error: Failed to open %s for writing.\n", prefix);
         exit(1);
      }
      fprintf(fpout, 
               "#Paths between nodes\n"
               "#Col. 0: Total number of nodes in path\n"
               "#    0 for no path\n"
               "#Col. 1: Distance\n"
               "#    -1 for no path\n"
               "#Col. 2: First node\n"
               "#Col. last: Last node\n");
      
      /* Now get the shortest distance between some nodes pairs*/
      ii=0;
      np[ii][0] = 0; np[ii][1] = 4; ++ii;  /* from node 0 to node 4 */
      np[ii][0] = 6; np[ii][1] = 5; ++ii;  /* from node 6 to node 5 */
      np[ii][0] = 1; np[ii][1] = 2; ++ii;  /* from node 1 to node 2 */
      np[ii][0] = 3; np[ii][1] = 3; ++ii;  /* from node 1 to node 2 */
      /* work  the node pairs */
      fprintf(fout, "#Internodal distance along graph \n");
      fprintf(fout, "#%-6s %-6s %-6s\n",
                    "From" , "to", "Dist." );
      for (ii=0; ii < N_np; ++ii) {
         if ( !(nPath = SUMA_Dijkstra_generic ( 
                           7, 
                           NULL, -1, 0,
                           N_Neighb, FirstNeighb, FirstNeighbDist,
                           np[ii][0], np[ii][1], 
                           NULL, NULL, 
                           1, 
                           &nDistance, &N_n, 0)) ) {
            nDistance = -1.0;
            if (fpout) fprintf(fpout, "0 -1.0 %d %d\n", np[ii][0], np[ii][1]);
         } else {
            if (fpout) {
               fprintf(fpout, "%d %.2f ", N_n, nDistance);
               for(kk=0; kk<N_n; ++kk) 
                  fprintf(fpout, 
                           "%d ", 
                           nPath[kk]); 
               fprintf(fpout, "\n");
            }
            free(nPath); nPath = NULL;
         }
      
         fprintf(fout, " %-6d %-6d %-4.2f\n", 
                       np[ii][0], np[ii][1], nDistance);
      }
      
      fprintf(stderr,"See file %s for full output\n", prefix);
      
      if (fpout) fclose(fpout); fpout=NULL;
      exit(0);
      
   }
   if (nakedarg >= 3 && coord_file) {
      /* bad combo */
      fprintf(stderr,"** Error: Can't specify x, y, z coordinates on command line AND in coord_file.\n");
      return(1) ;
   }


   if (dicom == -1) {
      THD_coorder cord;
      /* try to set based on AFNI_ORIENT */
      THD_coorder_fill (my_getenv("AFNI_ORIENT"), &cord);
      if (strcmp(cord.orcode,"RAI") == 0) {
         fprintf(stdout,"++ Input coordinates orientation set by default rules to %s\n", cord.orcode); 
      }else if (strcmp(cord.orcode,"LPI") == 0) {
         fprintf(stdout,"++ Input coordinates orientation set by default rules to %s\n", cord.orcode); 
      }else {
         fprintf(stderr,"** Error: Only RAI or LPI orientations allowed\n"
                        "default setting returned %s\n"
                        "You need to override AFNI_ORIENT \n"
                        "and use either -lpi or -rai\n", cord.orcode);
         return 1;
      }
   } else {
      if (dicom == 1) fprintf(stdout,"++ Input coordinates orientation set by user to %s\n", "RAI"); 
      else if (dicom == 0) fprintf(stdout,"++ Input coordinates orientation set by user to %s\n", "LPI");
      else { fprintf(stderr,"** Error: Should not happen!\n"); return(1); } 
   }
   
   if (mni == -1) {
      fprintf(stdout,"++ Input coordinates space set by default rules to TLRC\n");
      mni = 0;
   } else if (mni == 0) {
      fprintf(stdout,"++ Input coordinates space set by user to TLRC\n");
   } else if (mni == 1) {
      fprintf(stdout,"++ Input coordinates space set by user to MNI\n");
   } else if (mni == 2) {
      fprintf(stdout,"++ Input coordinates space set by user to MNI_ANAT\n");
   } else {
      fprintf(stderr,"** Error: Should not happen!\n"); return(1);
   }
   
   
   if (N_atlaslist == 0) {
      /* use all */
      atlaslist[N_atlaslist] = AFNI_TLRC_ATLAS; ++N_atlaslist; isatlasused[AFNI_TLRC_ATLAS] = 1;
      atlaslist[N_atlaslist] = CA_EZ_N27_MPM_ATLAS; ++N_atlaslist; isatlasused[CA_EZ_N27_MPM_ATLAS] = 1;
      atlaslist[N_atlaslist] = CA_EZ_N27_ML_ATLAS; ++N_atlaslist; isatlasused[CA_EZ_N27_ML_ATLAS] = 1;
      atlaslist[N_atlaslist] = CA_EZ_N27_PMAPS_ATLAS; ++N_atlaslist; isatlasused[CA_EZ_N27_PMAPS_ATLAS] = 1;
   }
   
   if (nakedarg < 3 && !Show_Atlas_Code && !shar && !bmsk && !coord_file) {
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
                  if( THD_deathcon() && THD_is_file(DSET_HEADNAME(maskset)) ) {
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
   
   /* le bmask business */
   if (bmsk) {
      byte *bmask_vol = NULL, *ba = NULL;
      THD_3dim_dataset *mset=NULL, *mset_orig = NULL, *rset = NULL;
      ATLAS_DSET_HOLDER adh;
      int isb, nvox_in_mask=0, *count = NULL;
      int *ics=NULL, *unq=NULL, n_unq=0, iroi=0, nonzero;
      float frac=0.0, sum = 0.0;
      char tmps[20];
      
      /* load the mask dset */
      if (!(mset_orig = THD_open_dataset (bmsk))) {
         fprintf(stderr,"** Error: Failed to open mask set %s.\n", bmsk);
         return(1);
      } 
      
      /* are we in TLRC land? */
      if (mset_orig->view_type != VIEW_TALAIRACH_TYPE) {
         fprintf( stderr,
                  "** Error: Mask set %s is not of the Talairach persuasion.\n", 
                  bmsk);
         return(1);
      }
      
      if (cmask) {
         if (ncmask != DSET_NVOX(mset_orig)) {
            fprintf(stderr,
             "** Error: Voxel number mismatch between -bmask and -cmask input.\n"
             "Make sure both volumes have the same number of voxels.\n");
            
            return(1);
         }
      }
      
      if (dobin) { /* one pass, do all */
         fprintf(stdout,"++ In binary mode ...\n");
         n_unq = 1;
         unq = NULL;
      } else {
         fprintf(stdout,"++ In ordered mode ...\n");
         /* get unique values*/
         unq = THD_unique_vals( mset_orig , 0, &n_unq, cmask );
         if (unq) {
            fprintf(stdout,"++ Have %d unique values of:\n", n_unq );
            for (k=0; k<n_unq; ++k) fprintf (stdout, "   %d", unq [k]);
            fprintf (stdout, "\n");
         } else {
            fprintf(stdout,"++ Failed to find unique values.\n");
            return(1);   
         }
      }
      
      for (iroi=0; iroi<n_unq; ++iroi) {
         if (dobin) {
            mset = mset_orig;
          /* turn the mask dataset to zeros and 1s */
            if ((nonzero = THD_makedsetmask( mset , 0 , 1.0, 0.0 , cmask)) < 0) {  /* get all non-zero values */
                  fprintf(stderr,"** Error: No mask for you.\n");
                  return(1);
            }
         } else {
            if (unq[iroi] == 0) { /* skip nonesense */
               fprintf(stdout,"++ Skipping unique value of 0\n");
               continue;
            } else {
               fprintf(stdout,"++ Processing unique value of %d\n", unq[iroi]);
            }
            mset = EDIT_full_copy(mset_orig, "tmp_ccopy");
            /* turn the mask dataset to zeros and 1s */
            if ((nonzero = THD_makedsetmask( mset , 0 , (float)unq[iroi], (float)unq[iroi] , cmask)) < 0) {  /* get all non-zero values */
                  fprintf(stderr,"** Error: No mask for you either.\n");
                  return(1);
            }
         }
         fprintf(stdout,"++    %d voxels in ROI\n", nonzero);
         
         /* for each atlas */
         for (k=0; k < N_atlaslist; ++k) {
            adh = Atlas_With_Trimming (atlaslist[k], 0);
            if (!adh.dset) {
               fprintf(stderr,"** Warning: Atlas %s could not be loaded.\n", Atlas_Code_to_Atlas_Name(atlaslist[k]));
               continue;
            }
            if (adh.maxindexcode < 1) {
               if (LocalHead) fprintf(stderr,"** Warning: Atlas %s not suitable for this application.\n", Atlas_Code_to_Atlas_Name(atlaslist[k]));
               continue;
            }
            if (adh.maxindexcode > 255) {
               fprintf(stderr,"** Warning: Max index code (%d) higher than expected.\n"
                              "What's cracking?.\n", adh.maxindexcode);
            }  
            /* resample mask per atlas, use linear interpolation, cut-off at 0.5 */
            rset = r_new_resam_dset ( mset, adh.dset, 0, 0, 0, NULL, MRI_LINEAR, NULL, 1);
            if (!rset) {
               fprintf(stderr,"** Error: Failed to reslice!?\n"); return(1);
            }
           /* get byte mask of regions > 0.5 */
            if (!(bmask_vol = THD_makemask( rset , 0 , 0.5 , 2.0 ))) {  /* get all non-zero values */
               fprintf(stderr,"** Error: No byte for you.\n");
               return(1);
            }
            nvox_in_mask = 0;
            for (i=0; i<DSET_NVOX(adh.dset); ++i) {
               if (bmask_vol[i]) ++nvox_in_mask; 
            }
            fprintf(stdout,"++    %d voxels in atlas-resampled mask\n", nvox_in_mask);
            /* for each sub-brick sb */
            for (isb=0; isb< DSET_NVALS(adh.dset); ++isb) {
               ba = DSET_BRICK_ARRAY(adh.dset,isb); 
               if (!ba) { ERROR_message("Unexpected NULL array"); return(1); }
               /* Create count array for range of integral values in atlas */
               count = (int *)calloc(adh.maxindexcode+1, sizeof(int));
               switch (adh.atcode) {
                  case AFNI_TLRC_ATLAS:
                  case CA_EZ_N27_ML_ATLAS:
                  case CA_EZ_N27_LR_ATLAS:
                     for (i=0; i<DSET_NVOX(adh.dset); ++i) {
                        if (bmask_vol[i] && ba[i] ) ++count[ba[i]]; /* Can't use 0 values, even if used in atlas codes */
                                                                    /* such as for the AC/PC in TT_Daemon! They can't be*/
                                                                    /* differentiated with this algorithm from non-brain, areas*/
                     }
                     break;
                  case CA_EZ_N27_MPM_ATLAS:
                     for (i=0; i<DSET_NVOX(adh.dset); ++i) {
                        if (bmask_vol[i] && ba[i] >= CA_EZ_MPM_MIN ) ++count[ba[i]]; 
                     }
                     break;
                  case CA_EZ_N27_PMAPS_ATLAS: /* not appropriate */
                     break;
                  default:
                     fprintf(stderr,"** Error: What is this atlas code (%d)?\n", adh.atcode);
                     return(1);
               }
               /* Now form percentages */
               if (!unq) {
                  fprintf(stdout,"Intersection of ROI (all non-zero values) with atlas %s (sb%d):\n", Atlas_Code_to_Atlas_Name(atlaslist[k]), isb);
               } else {
                  fprintf(stdout,"Intersection of ROI (valued %d) with atlas %s (sb%d):\n", unq[iroi], Atlas_Code_to_Atlas_Name(atlaslist[k]), isb);
               }
               
               /* sort the count */
               if (!(ics = z_idqsort (count, (adh.maxindexcode+1) ))) {
                  fprintf(stderr,"** Error: Failed to sort!\n");
                  return(1);
               }

               sum = 0.0;
               for (i=0; i<=adh.maxindexcode; ++i) {
                  if (count[i]) {
                     frac = (float)count[i]/(float)nvox_in_mask;
                     sum += frac;
                     sprintf(tmps, "%3.1f", frac*100.0); 
                     fprintf(stdout, "   %-5s%% overlap with %s, code %d\n", 
                              tmps, STR_PRINT(Atlas_Val_to_Atlas_Name(adh, ics[i])), ics[i] );
                  }
               }
               sprintf(tmps, "%3.1f", sum*100.0);
               fprintf(stdout, "   -----\n"
                               "   %-5s%% of cluster accounted for.\n"
                               "\n", tmps);
               /* done with count */
               if (count) free(count); count = NULL;
               if (ics) free(ics); ics = NULL;
            }
            /* done with resampled mset */
            DSET_delete(rset); rset = NULL;
         }
         
         /* delete mset if not same as mset_orig */
         if (mset != mset_orig) DSET_delete(mset); mset = NULL;
      } /* iroi */
      
      /* free unique values list, nothing done */
      if (unq) free(unq); unq = NULL;

      /* done with mset_orig */
      DSET_delete(mset_orig); mset_orig = NULL;
           
   }
   
   if(cmask) free(cmask); cmask = NULL;   /* Should not be needed beyond here */

   
   if (nakedarg < 3 && !coord_file) { /* nothing left to do */
      return(0);
   }

   if (coord_file) { /* load the XYZ coordinates from a 1D file */
         MRI_IMAGE * XYZ_im=NULL;
         float *XYZv = NULL;
         
         XYZ_im = mri_read_1D( coord_file ) ;
         if( XYZ_im == NULL ){
            fprintf(stderr,"** Error: Can't read XYZ.1D file %s\n",coord_file);
            return(1) ;
         }
         if (XYZ_im->ny != 3) {
            fprintf(stderr,"** Error: Need three columns as input.\n   Found %d columns\n", XYZ_im->ny);
            return(1) ;
         }
         XYZv = MRI_FLOAT_PTR(XYZ_im) ;
         coord_list = (float *)calloc(3*XYZ_im->nx, sizeof(float));
         if (!coord_list) {
            fprintf(stderr,"** Error: Failed to allocate\n");
            return(1) ;
         }
         /* copy to me own vectors */
         nxyz = XYZ_im->nx;
         for (ixyz=0; ixyz<nxyz; ++ixyz) {
            coord_list[3*ixyz]   = XYZv[ixyz];
            coord_list[3*ixyz+1] = XYZv[ixyz+XYZ_im->nx];
            coord_list[3*ixyz+2] = XYZv[ixyz+XYZ_im->nx*2];
         }
         mri_free(XYZ_im); XYZ_im = NULL;
   } else {
      coord_list = (float *)calloc(3, sizeof(float));
      coord_list[0] = xi; coord_list[1] = yi; coord_list[2] = zi; 
      nxyz = 1;
   }
   
   if (!coord_list) {
      fprintf(stderr,"** Error: No coords!\n");
      return(1) ;
   }
   
   for (ixyz = 0; ixyz < nxyz; ++ixyz) {
      x = coord_list[3*ixyz];
      y = coord_list[3*ixyz+1];
      z = coord_list[3*ixyz+2];
      
      if (!dicom) {
         /* go from lpi to rai */
         x = -x;
         y = -y; 
      }

      /* coords here are now in RAI */
      
      if (mni == 1) { /* go from mni to tlrc */
         LOAD_FVEC3( tv , -x, -y, z ) ;   /* next call expects input in MNI, LPI*/
         m = THD_mni_to_tta( tv );  /* m units are in RAI */
         if (ixyz == 0) {
            fprintf(stdout,"++ Input coordinates being transformed from MNI  RAI ([%.2f %.2f %.2f]) \n"
                           "                                         to TLRC RAI ([%.2f %.2f %.2f]).\n", 
                                                               x, y, z, m.xyz[0],  m.xyz[1], m.xyz[2]);
         }
         x = m.xyz[0]; y = m.xyz[1]; z = m.xyz[2];
      }

      if (mni == 2) { /* go from mni anat to tlrc */
         LOAD_FVEC3( tv , -x, -y, z ) ; /* next call expects input in MNI, LPI*/
         tv.xyz[2] = z - 8; /* 8mm I-S shift relative to mni - to use mni xform */
         m = THD_mni_to_tta( tv ); /* m units are in RAI */
         if (ixyz == 0) {
            fprintf(stdout,"++ Input coordinates being transformed from MNI_ANAT RAI ([%.2f %.2f %.2f]) \n"
                           "                                         to TLRC RAI ([%.2f %.2f %.2f]).\n", 
                                                               x, y, z, m.xyz[0],  m.xyz[1], m.xyz[2]);
         }
         x = m.xyz[0]; y = m.xyz[1]; z = m.xyz[2];
      }
      
      if (OldMethod) {
        string = TT_whereami_old(x,y,z);
        if (string == NULL ) {                              /* 30 Apr 2005 [rickr] */
          fprintf(stderr,"** Error: whereami lookup failure: is TTatlas+tlrc/TTatlas.nii.gz available?\n");
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
               fprintf(stderr,"** Error: Failed to find 'Focus point' string.\n"
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
         string = TT_whereami(x,y,z, AFNI_TLRC_SPC);
         if (string) fprintf(stdout,"%s\n", string);
         else fprintf(stdout,"whereami NULL string out.\n");
         if (string) free(string); string = NULL;            
      }
   } /* ixyz */   
   
   if (coord_list) free(coord_list); coord_list = NULL; 
   
return 0;
}
