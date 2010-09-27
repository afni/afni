/*** Whereami.c modified 1/11/05 -- main function by Mike Angstadt of U Chicago ***/

#define MAIN

#include "mrilib.h"
#include "afni.h"
#include <stdio.h>
#include <stdlib.h>
#include "thd_ttatlas_query.h"
#include "matrix.h"
extern int * SUMA_Dijkstra_generic (int N_Node, 
                     float *NodeList, int NodeDim, int dist_metric,
                     int *N_Neighbv, int **FirstNeighb, float **FirstNeighbDist,
                     int Nx, int Ny, 
                     byte *isNodeInMeshp, 
                     int *N_isNodeInMesh, int Method_Number, 
                     float *Lfinal, int *N_Path,
                     int verb);


void atlas_read_all(void);
atlas_xform_list *report_xform_chain(char *src, char *dest);

int   init_space_structs(atlas_xform_list **atlas_xfl,
   atlas_list **atlas_alist,
   atlas_space_list **atlas_spaces,
   atlas_template_list **atlas_templates);
int read_space_niml(NI_stream space_niml, atlas_xform_list *atlas_xfl,
   atlas_list *atlas_alist,
   atlas_space_list *atlas_spaces,
   atlas_template_list *atlas_template);
int atlas_read_xform(NI_element *nel, atlas_xform *atlas_xf);
int atlas_read_template(NI_element *nel, ATLAS_TEMPLATE *atlas_tpl);
int atlas_read_atlas(NI_element *nel, ATLAS *atlas);
int atlas_read_atlas_space(NI_element *nel, ATLAS_SPACE *at_space);
int make_space_neighborhood(atlas_space_list *at_spl, atlas_xform_list *atlas_xfl);
atlas_xform *get_xform_neighbor(atlas_xform_list *atlas_xfl, ATLAS_SPACE *at_space, 
   ATLAS_SPACE *dest_space, int *inv_xf);
atlas_xform_list *
   pathlist_to_xform_list(int *nPath, int N_n, atlas_xform_list *atlas_xfl, 
   atlas_space_list *at_spl);
int copy_xform(atlas_xform *src_xform, atlas_xform *dest_xform);
atlas_xform_list *
get_xform_chain(ATLAS_SPACE *at_space, ATLAS_SPACE *dest_space);

atlas_xform_list *read_space_xforms(NI_stream space_niml);
atlas_xform_list *calc_xform_list(atlas_xform_list *xfl);
int find_atlas_space_index(char *spacename);
void report_available_spaces(char *src);
atlas_space_list *find_available_spaces(char *src_space_name);

void free_atlas_structs(void);

void free_xform_list(atlas_xform_list *xfl);
void free_xform(atlas_xform *xf);
void free_space_list(atlas_space_list *xsl);
void free_space(ATLAS_SPACE *xs);
void free_template_list(atlas_template_list *xtl);
void free_template(ATLAS_TEMPLATE *xt);
void free_atlas_list(atlas_list *xal);
void free_atlas(ATLAS *xa);
void print_xform_list(atlas_xform_list *xfl);
void print_space_list(atlas_space_list *xsl);
void print_atlas_list(atlas_list *xal);
void print_template_list(atlas_template_list *xtl);
void print_xform(atlas_xform *xf);
void print_all_xforms(atlas_xform_list *xfl);
void print_affine_xform_data(float *xfptr);

atlas_xform *calc_xf(atlas_xform *xf, atlas_xform *xf2);
int affine_mult(atlas_xform *xf, atlas_xform *xf2, atlas_xform *xf3);
int affine_2piece_mult(atlas_xform *xf,
   atlas_xform *xf2, atlas_xform *xf3, int dir);
int affine_12piece_mult(atlas_xform *xf,
   atlas_xform *xf2, atlas_xform *xf3, int dir);
int x2piece_2piece_mult(atlas_xform *xf,
   atlas_xform *xf2, atlas_xform *xf3);
int x2piece_12piece_mult(atlas_xform *xf,
   atlas_xform *xf2, atlas_xform *xf3, int dir);
int x12piece_12piece_mult(atlas_xform *xf,
   atlas_xform *xf2, atlas_xform *xf3);
   
int invert_xform(atlas_xform *xf);
int invert_affine(atlas_xform *xf);
int invert_12piece(atlas_xform *xf);
int invert_2piece(atlas_xform *xf);

atlas_xform *identity_xform(void);

int find_atlas_space(atlas_space_list *at_spl, ATLAS_SPACE *at_space);
int apply_xform_chain(atlas_xform_list *xfl, float x, float y, float z,
                  float *xout, float *yout, float *zout);

int apply_xform_affine(atlas_xform *xf, float x, float y, float z, \
    float *xout, float *yout, float *zout);
int apply_xform_2piece(atlas_xform *xf, float x, float y, float z, \
    float *xout, float *yout, float *zout);
int apply_xform_12piece(atlas_xform *xf, float x, float y, float z, \
    float *xout, float *yout, float *zout);
int apply_xform_brett_tt2mni(float x, float y, float z, \
    float *xout, float *yout, float *zout);
int apply_xform_brett_mni2tt(float x, float y, float z, \
    float *xout, float *yout, float *zout);

NI_element *NI_find_next_element(NI_stream ns, char *name);

static int debug_niml = 0;
static atlas_xform_list *atlas_xfl;
/*static atlas_space_list *atlas_spaces;*/
static atlas_list *atlas_alist;
static atlas_template_list *atlas_templates;

int **FirstNeighb=NULL;
float **FirstNeighbDist=NULL; 
int *N_Neighb;

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

" \n---------------\n"
" Show atlas NIML database options:\n"
" -show_atlases          : show all available atlases\n"
" -show_templates        : show all available templates\n"
" -show_spaces           : show all available template spaces\n"
" -show_xforms           : show all available xforms\n"
" -show_atlas_all        : show all the above\n"
"\n"
" -show_available_spaces srcspace : show spaces that are available from\n"
"             the source space\n"
" -show_chain srcspace destspace : show the chain of transformations\n"
"             needed to go from one space to another\n"
" -calc_chain srcspace destspace : compute the chain of transformations\n"
"             combining and inverting transformations where possible\n"
" -xform_xyz  used with calc_chain, takes the x,y,z coordinates and \n"
"             applies the combined chain of transformations to compute\n"
"             a new x,y,z coordinate\n"

"Note setting the environment variable AFNI_NIML_DEBUG will show detailed\n"
" as the program reads the Atlas NIML Database and computes transformations.\n"
" A Dijkstra search is used to find the shortest path between spaces. Each\n"
" transformation carry with it a distance attribute that is used for this\n"
" computation\n"
"---------------\n"

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
" -dset: Determine the template space to use from this reference dataset\n"
"        Space can be TLRC, MNI, MNI_ANAT. If the space is known,\n"
"        and a reference atlas can be found, the regions will be\n"
"        based on the coordinates from this template space.\n"
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
   THD_3dim_dataset *space_dset = NULL;
   int read_niml_atlas = 0, show_atlas = 0, show_atlas_spaces = 0;
   int show_atlas_templates = 0, show_atlas_xforms = 0;
   int show_xform_chain = 0, calc_xform_chain=0, show_avail_space=0;
   char *srcspace=NULL, *destspace=NULL;
   atlas_xform_list *xfl = NULL, *cxfl = NULL;
   atlas_xform *xf;
   float xout, yout, zout;
   int xform_xyz = 0;

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

         if (strcmp(argv[iarg],"-dset") == 0) { 
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -space\n"); return(1);
            }
            if (!(space_dset = THD_open_dataset (argv[iarg]))) {
               fprintf(stderr,"** Error: Failed to open data set %s.\n", bmsk);
               return(1);
            } 
            THD_get_space(space_dset); /* update space if necess*/

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
               isatlasused[CUSTOM_ATLAS] = 1;
               
#if 0
               fprintf(stderr,"** Error: Atlas name %s is not recognized\n", argv[iarg]);
               return(1);
#endif

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
         if( strcmp(argv[iarg],"-readniml") == 0) {
            atlas_xform_list  *xfl, *cxfl;
            /* atlas_xform *xf; */

            atlas_read_all();
            /* atlas testing */
            xfl = get_xform_chain(atlas_spaces->space+0,atlas_spaces->space+1);
            print_xform_list(xfl);
            cxfl = calc_xform_list(xfl);
            print_xform_list(cxfl);
            free_xform_list(xfl);
            free_xform_list(cxfl);
            free_atlas_structs();
            exit(0);
         }

        if( strcmp(argv[iarg],"-show_available_spaces") == 0) {
            read_niml_atlas = 1;
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
               "** Error: Need src space name after -show_available_spaces\n"); return(1);
            }
            srcspace = argv[iarg];
            show_avail_space = 1;
            ++iarg;
            continue; 
         }

         if( strcmp(argv[iarg],"-show_chain") == 0) {
            read_niml_atlas = 1;
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
               "** Error: Need src and dest spaces after -show_chain\n"); return(1);
            }
            srcspace = argv[iarg];
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
               "** Error: Need src and dest spaces after -show_chain\n"); return(1);
            }
            destspace = argv[iarg];
            show_xform_chain = 1;
            ++iarg;
            continue; 
         }
            
        if( strcmp(argv[iarg],"-calc_chain") == 0) {
            read_niml_atlas = 1;
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
               "** Error: Need src and dest spaces after -calc_chain\n"); return(1);
            }
            srcspace = argv[iarg];
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
               "** Error: Need src and dest spaces after -calc_chain\n"); return(1);
            }
            destspace = argv[iarg];
            show_xform_chain = 1;
            calc_xform_chain = 1;
            ++iarg;
            continue; 
         }

        if( strcmp(argv[iarg],"-xform_xyz") == 0){
            iarg++;
            read_niml_atlas = 1;
            xform_xyz = 1;
            continue ;
         }          

         if(strcmp(argv[iarg],"-show_atlases") == 0) {
            iarg++;
            read_niml_atlas = 1;
            show_atlas = 1;
            continue ;
         }          
         if(strcmp(argv[iarg],"-show_templates") == 0) {
            iarg++;
            read_niml_atlas = 1;
            show_atlas_templates = 1;
            continue ;
         }
         if(strcmp(argv[iarg],"-show_spaces") == 0) {
            iarg++;
            read_niml_atlas = 1;
            show_atlas_spaces = 1;
            continue ;
         }          
         if(strcmp(argv[iarg],"-show_xforms") == 0) {
            iarg++;
            read_niml_atlas = 1;
            show_atlas_xforms = 1;
            continue ;
         }          
         if(strcmp(argv[iarg],"-show_atlas_all") == 0) {
            iarg++;
            read_niml_atlas = 1;
            show_atlas = 1;
            show_atlas_spaces = 1;
            show_atlas_templates = 1;
            show_atlas_xforms = 1;
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

   /* user wants to see atlas database */
   if(read_niml_atlas) {

      atlas_read_all();
      if(show_avail_space)
         report_available_spaces(srcspace);
      if(show_xform_chain)
         xfl = report_xform_chain(srcspace, destspace);
      if(calc_xform_chain) {
         cxfl = calc_xform_list(xfl);
         print_xform_list(cxfl);  /* print the xforms briefly with names only */
         print_all_xforms(cxfl);  /* print the combined list transforms with data */
      }
      if(xform_xyz) {
         apply_xform_chain(cxfl, xi, yi, zi, &xout, &yout, &zout);
         printf("Coords in: %f, %f, %f -> Coords out: %f, %f, %f\n", xi,yi,zi,xout,yout,zout);
      }
      if(xfl)
        free_xform_list(xfl);
      if(cxfl)
        free_xform_list(cxfl);
      if(show_atlas)
         print_atlas_list(atlas_alist);
      if(show_atlas_templates)
         print_template_list(atlas_templates);
      if(show_atlas_spaces)
         print_space_list(atlas_spaces);
      if(show_atlas_xforms)
         print_all_xforms(atlas_xfl);
         
      free_atlas_structs(); 
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
   } else {
      fprintf(stderr,"** Error: Should not happen!\n"); return(1);
   }
   
   
   if (N_atlaslist == 0) {
#if 0
      if(dset->preferred_atlas){       /* check dataset structure for preferred status*/
          atlaslist[0] = CUSTOM_ATLAS; /* flag this is a custom atlas */
          ++N_atlaslist
      }
      else {
#endif
         /* use all */
         atlaslist[N_atlaslist] = AFNI_TLRC_ATLAS; ++N_atlaslist;
             isatlasused[AFNI_TLRC_ATLAS] = 1;
         atlaslist[N_atlaslist] = CA_EZ_N27_MPM_ATLAS; ++N_atlaslist;
             isatlasused[CA_EZ_N27_MPM_ATLAS] = 1;
         atlaslist[N_atlaslist] = CA_EZ_N27_ML_ATLAS; ++N_atlaslist;
             isatlasused[CA_EZ_N27_ML_ATLAS] = 1;
         atlaslist[N_atlaslist] = CA_EZ_N27_PMAPS_ATLAS; ++N_atlaslist;
             isatlasused[CA_EZ_N27_PMAPS_ATLAS] = 1;
/*      }*/
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
               fprintf(stdout,
               "++ ========================================================================\n") ;
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

         if(space_dset)
           string = TT_whereami(x,y,z, THD_space_code(space_dset->atlas_space));
         else
           string = TT_whereami(x,y,z, AFNI_TLRC_SPC);

         if (string) fprintf(stdout,"%s\n", string);
         else fprintf(stdout,"whereami NULL string out.\n");
         if (string) free(string); string = NULL;            
      }
   } /* ixyz */   
   
   if (coord_list) free(coord_list); coord_list = NULL; 
   
return 0;
}

/* report xform chain to go between src and dest spaces */
atlas_xform_list *report_xform_chain(char *src, char *dest)
{

   atlas_xform_list  *xfl;
   atlas_xform *xf;
   int srci, desti;
   
   /* atlas testing */
   srci = find_atlas_space_index(src);
   if(srci<0){
       INFO_message("Could not find source space %s in database", src);
   }    
   desti = find_atlas_space_index(dest);
   if(desti<0){
       INFO_message("Could not find destination space %s in database", dest);
   }
   /* check if we're going nowhere */
   if(srci==desti) {
      if(debug_niml)
         INFO_message("Chain is from and to same space");
      xf = identity_xform();      /* assign identity matrix */
      free(xf->source); free(xf->dest);
      xf->source = nifti_strdup(src);
      xf->dest = nifti_strdup(src);
      xfl = (atlas_xform_list *) calloc(1, sizeof(atlas_xform));
      xfl->xform = xf;
      xfl->nxforms = 1;
   }
   else {
     xfl =
      get_xform_chain(atlas_spaces->space+srci,atlas_spaces->space+desti);
   }   
   print_xform_list(xfl);
   return(xfl);
}

/* report which spaces are available to go between src and all other spaces */
void report_available_spaces(char *src)
{
   atlas_space_list *spl;
   
   spl = find_available_spaces(src);
   print_space_list(spl);
   free_space_list(spl);    
}

/* read various NIML files for atlas information*/
void atlas_read_all()
{
   NI_stream space_niml;
   int valid_space_niml;
   char * ept = NULL;
   char suppfilestr[261];
 
   ept = my_getenv("AFNI_NIML_DEBUG");
   if( ept ) debug_niml = atoi(ept);       /* adjust if set */

   if(debug_niml) 
      INFO_message("\nInitializing structures\n"); 
   if(!init_space_structs(&atlas_xfl, &atlas_alist,
                          &atlas_spaces, &atlas_templates))
      ERROR_message("Could not initialize structures for template spaces");


/*
atlas_xfl = (atlas_xform_list *) malloc(sizeof(atlas_xform_list));
atlas_xfl->nxforms = 0;
atlas_alist = (atlas_list *) malloc(sizeof(atlas_list));
atlas_alist->natlases = 0;
atlas_spaces = (atlas_space_list *) malloc(sizeof(atlas_space_list));
atlas_spaces->nspaces = 0;
atlas_templates = (atlas_template_list *) malloc(sizeof(atlas_template_list));
atlas_templates->ntemplates = 0;
*/
   if(debug_niml) 
      INFO_message("opening AFNI_atlas_spaces.niml");   

   space_niml = NI_stream_open("file:AFNI_atlas_spaces.niml","r");

   if(space_niml==NULL){
         WARNING_message("Could not open global AFNI_atlas_spaces_niml\n");
   }

   /* read atlas info from global atlas file */
   valid_space_niml = read_space_niml(space_niml, atlas_xfl,
          atlas_alist, atlas_spaces, atlas_templates);

   ept = my_getenv("AFNI_SUPP_ATLAS");
   if( ept ) {
      sprintf(suppfilestr, "file:%s", ept);
      if(debug_niml) 
         INFO_message("opening AFNI_supp_atlas_space.niml");   
      space_niml = NI_stream_open(suppfilestr,"r");
      if(space_niml==NULL){
            fprintf(stderr, "\nCould not open supplemental atlas niml file\n");
            return;
      }
      /* read atlas info from supplemental atlas file */
      /*  adding to existing structures */
      valid_space_niml = read_space_niml(space_niml, atlas_xfl,
             atlas_alist, atlas_spaces, atlas_templates);

   }


   /* read atlas info from local atlas file */
   ept = my_getenv("AFNI_LOCAL_ATLAS");
   if( ept ) {
      sprintf(suppfilestr, "file:%s", ept);
      if(debug_niml) 
         INFO_message("opening AFNI_local_atlas_space.niml");   
      space_niml = NI_stream_open(suppfilestr,"r");
      if(space_niml==NULL){
            fprintf(stderr, "\nCould not open supplemental atlas niml file\n");
            return;
      }
      /* read atlas info from local atlas file */
      /*  adding to existing structures */
      valid_space_niml = read_space_niml(space_niml, atlas_xfl,
             atlas_alist, atlas_spaces, atlas_templates);
   }

  
   /* set up the neighborhood for spaces */
   /*  how are the spaces related to each other */ 
   if(make_space_neighborhood(atlas_spaces, atlas_xfl)!=0)
     return;
   
}

/* initialize space structures for dealing with templates and atlases */
int   init_space_structs(atlas_xform_list **atlas_xfl,
   atlas_list **atlas_alist,
   atlas_space_list **atlas_spaces,
   atlas_template_list **atlas_templates)
{
   *atlas_alist = (atlas_list *) malloc(sizeof(atlas_list));
   *atlas_spaces = (atlas_space_list *) malloc(sizeof(atlas_space_list));
   *atlas_templates = (atlas_template_list *) malloc(sizeof(atlas_template_list));
   *atlas_xfl = (atlas_xform_list *) malloc(sizeof(atlas_xform_list));
   (*atlas_xfl)->nxforms = 0;
   (*atlas_xfl)->xform = NULL;
   (*atlas_alist)->natlases = 0;
   (*atlas_spaces)->nspaces = 0;
   (*atlas_templates)->ntemplates = 0;

   if(!(*atlas_alist) || !(*atlas_spaces) || !(*atlas_templates) || !(*atlas_xfl)) {
      return(0);
   }
   return(1);      
}


/* read niml file elements into C structures */
int   read_space_niml(NI_stream space_niml, atlas_xform_list *atlas_xfl,
   atlas_list *atlas_alist,
   atlas_space_list *atlas_spaces,
   atlas_template_list *atlas_templates)
{
   NI_element *nel;
   int found = 0; 

   nel = (NI_element *) 1;
   while(nel) {
      if(debug_niml) 
         INFO_message("reading elements\n");
      nel = NI_read_element(space_niml, 100);
      if(nel) {
         if(debug_niml) 
           INFO_message("nel name %s\n", nel->name);
         if (nel->type == NI_ELEMENT_TYPE) {
            if(strcmp(nel->name, "TEMPLATE_SPACE") == 0) {
               atlas_spaces->nspaces++;
               if(debug_niml){
                  INFO_message("Template space\n");
                  INFO_message("number of spaces now %d\n", atlas_spaces->nspaces);
               }
               if(atlas_spaces->nspaces==1){
                  if(debug_niml)
                     INFO_message("initial memory allocation for spaces");
                  atlas_spaces->space = (ATLAS_SPACE *) malloc(sizeof(ATLAS_SPACE));
               }
               else               
                  atlas_spaces->space = (ATLAS_SPACE *) realloc(
                      atlas_spaces->space, atlas_spaces->nspaces * sizeof(ATLAS_SPACE));
               atlas_read_atlas_space(
                    nel, &atlas_spaces->space[atlas_spaces->nspaces-1]);

               found = 1;
            }
            if(strcmp(nel->name, "XFORM") == 0) {
               atlas_xfl->nxforms++;
               if(debug_niml){
                  INFO_message("space XFORM\n");
                  INFO_message("number of xforms now %d\n", atlas_xfl->nxforms);
               }
               if(atlas_xfl->nxforms==1){
                  if(debug_niml)
                     INFO_message("initial memory allocation for xforms");
                  atlas_xfl->xform = (atlas_xform *) malloc(sizeof(atlas_xform));
               }
               else               
                  atlas_xfl->xform = (atlas_xform *) realloc(
                      atlas_xfl->xform, atlas_xfl->nxforms * sizeof(atlas_xform));
               atlas_read_xform(nel, &atlas_xfl->xform[atlas_xfl->nxforms-1]);
               found = 1;
            } 
            if(strcmp(nel->name, "atlas_dataset") == 0) {
              atlas_alist->natlases++;
              if(debug_niml){
                  INFO_message("Atlas dataset\n");
                  INFO_message("number of atlases now %d\n", atlas_alist->natlases);
               }
               if(atlas_alist->natlases==1){
                  if(debug_niml)
                     INFO_message("initial memory allocation for atlases");
                  atlas_alist->atlas = (ATLAS *) malloc(sizeof(ATLAS));
               }
               else               
                  atlas_alist->atlas = (ATLAS *) realloc(
                      atlas_alist->atlas, atlas_alist->natlases * sizeof(ATLAS));
               atlas_read_atlas(nel, &atlas_alist->atlas[atlas_alist->natlases-1]);

               found = 1;
            }

            if(strcmp(nel->name, "template_dataset") == 0) {
               if(debug_niml) 
                  INFO_message("template dataset\n");
               atlas_templates->ntemplates++;
               if(debug_niml){
                  INFO_message("Atlas template\n");
                  INFO_message("number of templates now %d\n", 
                      atlas_templates->ntemplates);
               }
               if(atlas_templates->ntemplates==1){
                  if(debug_niml)
                     INFO_message("initial memory allocation for templates");
                  atlas_templates->atlas_template = 
                      (ATLAS_TEMPLATE *) malloc(sizeof(ATLAS_TEMPLATE));
               }
               else               
                  atlas_templates->atlas_template = (ATLAS_TEMPLATE *) realloc(
                      atlas_templates->atlas_template, 
                      atlas_templates->ntemplates * sizeof(ATLAS_TEMPLATE));
               atlas_read_template(nel,
                   &atlas_templates->atlas_template[atlas_templates->ntemplates-1]);
               found = 1;
            }
         }      
         NI_free_element(nel);  /* don't need the NIML element anymore */
      }
   }


   return(found);
}

/* compute what spaces are neighbors of the others  */
/* in preparation for Dijkstra search */
int make_space_neighborhood(atlas_space_list *at_spl, atlas_xform_list *atlas_xfl)
{
      /* See Ziad's NIH-5 labbook pp 46 for graph */


      int i, j, nspaces, inv_xf, neighbor_i;
      ATLAS_SPACE *atlas_space, *dest_space;
      atlas_xform *xform;
      
      nspaces = at_spl->nspaces;
      FirstNeighb = (int **) malloc(nspaces * sizeof(int *));
      FirstNeighbDist = (float **) malloc(nspaces * sizeof(float *));
      N_Neighb = (int *) malloc(nspaces * sizeof(int));

      if(debug_niml)
         INFO_message("initial memory allocation for neighbors");
      if((FirstNeighb==NULL) || (FirstNeighbDist==NULL) || 
         (N_Neighb==NULL)) {
          WARNING_message("Could not allocate space for atlas neighborhood.");
          return(-1);
      }
      /* loop through all the spaces and see if they can be directly transformed
        (or inversely transformed) to any other space in the list of spaces */
      for(i=0;i<nspaces;i++){
         neighbor_i = 0;
         atlas_space = at_spl->space+i;
         for(j=0;j<nspaces;j++) {
            if(i==j) continue;  /* don't need to match the same space */
            dest_space = at_spl->space+j;
            xform = get_xform_neighbor(
                     atlas_xfl, atlas_space, dest_space, &inv_xf);
            if(xform!=NULL){
               if(neighbor_i==0){
                  FirstNeighb[i] = (int *) malloc(sizeof(int));
                  FirstNeighbDist[i] = (float *) malloc(sizeof(float));
               }
               else {
                  FirstNeighb[i] = (int *) realloc(FirstNeighb[i], 
                   (neighbor_i+1)*sizeof(int));
                  FirstNeighbDist[i] = (float *) realloc(FirstNeighbDist[i], 
                   (neighbor_i+1)*sizeof(float));
               }
               if((FirstNeighb[i]==NULL) || (FirstNeighbDist[i]==NULL)) {
                  WARNING_message("Could not allocate space for atlas neighborhood");
                  return(-1);
               }
               
               FirstNeighb[i][neighbor_i] = j;
               FirstNeighbDist[i][neighbor_i++] = xform->dist;
               if(debug_niml){
                  INFO_message("neighbor found for space %d with space %d",i,j);
                  INFO_message("xform %s with dist %f", xform->xform_name, xform->dist);
               }
            }
         }
         N_Neighb[i] = neighbor_i;
      }

    return(0);
}

/* search through list of transformations for direct or inverse transformation
   of source template space to destination template space */
atlas_xform *
get_xform_neighbor(atlas_xform_list *atlas_xfl, ATLAS_SPACE *at_space, 
   ATLAS_SPACE *dest_space, int *inv_xf)
{
    int i;
    char *srcstr, *deststr, *xfsrc, *xfdest;
    atlas_xform *xf;
       
    srcstr = at_space->atlas_space;
    deststr = dest_space->atlas_space;

    *inv_xf = 0;
    
    for(i=0;i<atlas_xfl->nxforms;i++) {
       xf = atlas_xfl->xform+i;
       xfsrc = xf->source;
       xfdest = xf->dest;
       if((strcmp(srcstr, xfsrc)==0) && (strcmp(deststr,xfdest)==0)) {
          return(xf);
       }
       /* check if inverse direction is available */
       if((strcmp(deststr, xfsrc)==0) && (strcmp(srcstr,xfdest)==0)) {
          *inv_xf = 1;
          return(xf);
       }
    }
    return(NULL);
}

/* find shortest path to go from one space to a destination space */
/* return the list of transformations needed to accomplish this */
atlas_xform_list *
get_xform_chain(ATLAS_SPACE *at_space, ATLAS_SPACE *dest_space)
{
   int srci, desti;
   int N_n, kk, *nPath;
   float nDistance;
   atlas_xform_list *xfl=NULL;

   /* find index for input spaces */
   srci = find_atlas_space(atlas_spaces, at_space);
   desti = find_atlas_space(atlas_spaces, dest_space);

   /* if src and dest are the same, should return identity right away */    
   if ( !(nPath = SUMA_Dijkstra_generic ( 
                          atlas_spaces->nspaces, 
                          NULL, -1, 0,
                          N_Neighb, FirstNeighb, FirstNeighbDist,
                          srci, desti, 
                          NULL, NULL, 
                          1, 
                          &nDistance, &N_n, 0)) ) {
          return(NULL); /* no path found */
        } 
   else {
      if(debug_niml){
         INFO_message("Number of spaces to traverse %d with distance %.2f ",
                     N_n, nDistance);
         fprintf(stderr, "spaces in chain by index: ");
         for(kk=0; kk<N_n; ++kk) 
            fprintf(stderr, "%d ", nPath[kk]); 
         fprintf(stderr, "\n");
      }

      xfl = pathlist_to_xform_list(nPath, N_n, atlas_xfl, atlas_spaces);
      free(nPath); nPath = NULL;
   }

   return(xfl);
}

/* find space index that matches space name */
int
find_atlas_space(atlas_space_list *at_spl, ATLAS_SPACE *at_space)
{
   int i;
   ATLAS_SPACE *sp1;
   
   for(i=0;i<at_spl->nspaces;i++) {
      sp1 = at_spl->space+i;
      if(strcmp(sp1->atlas_space, at_space->atlas_space)==0)
         return(i);
   }
   return(-1);   /* not found */ 
}

/* find space index in space list that matches string */
int
find_atlas_space_index(char *spacename)
{
   int i;
   ATLAS_SPACE *sp1;

   if((spacename==NULL) || (strcmp(spacename, "")==0))
      return(-1);
      
   for(i=0;i<atlas_spaces->nspaces;i++) {
      sp1 = atlas_spaces->space+i;
      if(strcmp(sp1->atlas_space, spacename)==0)
         return(i);
   }
   return(-1);   /* not found */ 
}

/* return the atlas_space structure corresponding to the atlas_space name
   in the dataset structure */
ATLAS_SPACE *
dset_space(THD_3dim_dataset *dset)
{
   int spacei;
   
   spacei = find_atlas_space_index(dset->atlas_space);
   if(spacei==-1)
      return(NULL);
   
   return(atlas_spaces->space+spacei);
}

/* find all available spaces that can be transformed to from some original space */
atlas_space_list *
find_available_spaces(char *src_space_name)
{
  int i, curr_i, nspaces=0;
  atlas_space_list *spl;
  atlas_xform_list *xfl;
  ATLAS_SPACE *xs, *spl_space, *src_space;
  
  spl = NULL;
  
  /* find index of current space string in static space list */
  curr_i = find_atlas_space_index(src_space_name);
  src_space = atlas_spaces->space+curr_i;
  
  /* search through all spaces */
  for(i=0;i<atlas_spaces->nspaces;i++){
     if(i==curr_i) continue;   /* don't count the current space */
     xs = atlas_spaces->space+i;   /* pointer to indexed space */
     xfl = get_xform_chain(src_space, xs);
     if(xfl){   /* found a transformation */
        if(debug_niml)
            INFO_message("Found an available space: %s", xs->atlas_space);
        free_xform_list(xfl);  /* don't actually need the xform list */
        
        if(spl==NULL) {
           spl = (atlas_space_list *) malloc(sizeof(atlas_space_list));
           spl->space = (ATLAS_SPACE *) malloc(sizeof(ATLAS_SPACE));
           nspaces = 1;
        }
        else {
           nspaces++;
           spl->space = (ATLAS_SPACE *) realloc(
               spl->space, nspaces * sizeof(ATLAS_SPACE));
        }       

        if((spl==NULL)||(spl->space==NULL)) {
            WARNING_message("Could not allocate available space transformation");
            return(NULL);
        }
        spl_space = spl->space+nspaces-1;
        spl_space->atlas_space = nifti_strdup(xs->atlas_space); 
        spl_space->generic_space = nifti_strdup(xs->generic_space);

        if((spl_space->atlas_space == NULL) || (spl_space->generic_space == NULL)) {
           WARNING_message("Could not allocate template space strings");
           return(NULL);
        }
        spl->nspaces = nspaces;


     }
  }
  if(spl)
      spl->nspaces = nspaces;
  else{
     if(debug_niml)
         INFO_message("no spaces reachable from source space: %s", src_space_name);   
  }
  if(debug_niml)
      INFO_message("There are %d spaces available", spl->nspaces);
  return(spl);
}

atlas_xform_list *
pathlist_to_xform_list(int *nPath, int N_n, atlas_xform_list *atlas_xfl, 
   atlas_space_list *at_spl)
{
   int kk, inv_xf;
   atlas_xform_list *xflc = NULL;
   atlas_xform *a_xform, *xxform, *xf;
   ATLAS_SPACE *sp1, *sp2;

   xflc = (atlas_xform_list *) malloc(sizeof(atlas_xform_list));
   xflc->xform =  (atlas_xform *)  malloc((N_n-1)*sizeof(atlas_xform));
   xflc->nxforms = N_n-1;
         
   for(kk=0;kk<N_n-1;++kk) {
       sp1 = at_spl->space+nPath[kk]; /* starting space */
       sp2 = at_spl->space+nPath[kk+1];   /* next space */
       a_xform = get_xform_neighbor(atlas_xfl,sp1, sp2, &inv_xf); 
       if(debug_niml){
         INFO_message("space%d %s to space%d %s using xform %s",
            kk, sp1->atlas_space, kk+1, sp2->atlas_space, a_xform->xform_name);
       }

       xxform = xflc->xform+kk;       
       if(copy_xform(a_xform, xxform)!=0){
          WARNING_message("Could not create copy of xform for path");
          return(NULL);
       }

           
       if(inv_xf)   /* if inverting xform, take inverse of inverse field */
           xxform->inverse = !(a_xform->inverse);

       if(debug_niml){
          print_xform(xxform);
       }

   }

   if(debug_niml){
      INFO_message("Number of xforms in chain is %d", xflc->nxforms);
   }

   return(xflc);
}


/* copy elements of xform structure, allocating space for each element */
/* dest_xform already exists as a structure, it just needs to be populated */
int
copy_xform(atlas_xform *src_xform, atlas_xform *dest_xform)
{
   memset(dest_xform, 0, sizeof(atlas_xform));
   dest_xform->xform_type = nifti_strdup(src_xform->xform_type);
   dest_xform->xform_name = nifti_strdup(src_xform->xform_name);
   dest_xform->source = nifti_strdup(src_xform->source);
   dest_xform->dest = nifti_strdup(src_xform->dest);
   dest_xform->coord_order = nifti_strdup(src_xform->coord_order);
   
   if((dest_xform->xform_type==NULL) ||(dest_xform->xform_name==NULL) ||
      (dest_xform->source==NULL) || (dest_xform->dest==NULL) ||
      (dest_xform->coord_order==NULL))
      return(1);
   dest_xform->dist = src_xform->dist;
   dest_xform->inverse = src_xform->inverse;
   dest_xform->prepost = src_xform->prepost;
   dest_xform->nelts = src_xform->nelts;
   if(dest_xform->nelts==0) return(0);
   dest_xform->xform = malloc(dest_xform->nelts*sizeof(float));
   if(dest_xform->xform==NULL) return(1);
   memcpy(dest_xform->xform, src_xform->xform, dest_xform->nelts*sizeof(float));
   return(0);
}

/* copy elements of xform structure, allocating space for each element */
/* dest_xform already exists as a structure, it just needs to be populated */
atlas_xform *
identity_xform()
{
   atlas_xform *dest_xform;
   float *fptr;

   dest_xform = (atlas_xform *) calloc(1,sizeof(atlas_xform));   
   dest_xform->xform_type = nifti_strdup("Identity");
   dest_xform->xform_name = nifti_strdup("Identity");
   dest_xform->source = nifti_strdup("");
   dest_xform->dest = nifti_strdup("");
   dest_xform->coord_order = nifti_strdup("rai");
   
   if((dest_xform->xform_type==NULL) ||(dest_xform->xform_name==NULL) ||
      (dest_xform->source==NULL) || (dest_xform->dest==NULL) ||
      (dest_xform->coord_order==NULL))
      return(NULL);
   dest_xform->dist = 0.01;
   dest_xform->inverse = 0;
   dest_xform->prepost = 1;

   dest_xform->nelts = 1;
   if(dest_xform->nelts==0) return(dest_xform);
   dest_xform->xform = malloc(dest_xform->nelts*sizeof(float));
   if(dest_xform->xform==NULL) return(NULL);
   fptr = (float *) dest_xform->xform;
   *fptr  = 1.0;
   return(dest_xform);
}

/* free all the static lists */
void free_atlas_structs()
{
   free_xform_list(atlas_xfl);
   free_atlas_list(atlas_alist);
   free_space_list(atlas_spaces);
   free_template_list(atlas_templates);
}

/* free list of xforms */
void
free_xform_list(atlas_xform_list *xfl)
{
   int i;
   
   if(xfl==NULL)
      return;
   for(i=(xfl->nxforms)-1;i>=0;i--){
      free_xform(xfl->xform+i);
   }
   free(xfl->xform);
   free(xfl);
}

/* free an atlas_xform structure */
void
free_xform(atlas_xform *xf)
{   
   if(xf==NULL)
      return;
   free(xf->xform);
   free(xf->xform_type); free(xf->xform_name); free(xf->source); free(xf->dest);
   free(xf->coord_order);
}    

/* free list of spaces */
void
free_space_list(atlas_space_list *xsl)
{
   int i;
   
   if(xsl==NULL)
      return;
   for(i=0;i<xsl->nspaces;i++)
      free_space(xsl->space+i);
   free(xsl->space);
   free(xsl);
}

/* free an atlas_space structure */
void
free_space(ATLAS_SPACE *xs)
{   
   if(xs==NULL)
      return;
   free(xs->atlas_space); free(xs->generic_space); 
}    

/* free list of spaces */
void
free_template_list(atlas_template_list *xtl)
{
   int i;
   
   if(xtl==NULL)
      return;
   for(i=0;i<xtl->ntemplates;i++)
      free_template(xtl->atlas_template+i);

   if(xtl->ntemplates >= 1)
      free(xtl->atlas_template); 
   free(xtl);
}

/* free an atlas_template structure */
void
free_template(ATLAS_TEMPLATE *xt)
{   
   if(xt==NULL)
      return;
   free(xt->atlas_space); free(xt->atlas_template); 
} 

/* free list of atlases */
void
free_atlas_list(atlas_list *xal)
{
   int i;
   
   if(xal==NULL)
      return;
   for(i=0;i<xal->natlases;i++)
      free_atlas(xal->atlas+i);
   if(xal->natlases >= 1)
      free(xal->atlas);
   free(xal);
}

/* free an atlas structure */
void
free_atlas(ATLAS *xa)
{   
   if(xa==NULL)
      return;
   free(xa->atlas_space); free(xa->atlas_dset); 
} 

/* print list of xforms - short form - as a chain */
void
print_xform_list(atlas_xform_list *xfl)
{
   int i;
   atlas_xform *xf;
   INFO_message("----- Transform list: -------");
   
   if(xfl==NULL)
      return;
   for(i=0;i<xfl->nxforms;i++) {
      xf = xfl->xform+i;
         fprintf(stderr,"%s ", xf->xform_name);
      if(xf->inverse)
         fprintf(stderr,"I");  
      if(i==(xfl->nxforms)-1)
         fprintf(stderr,"\n");
      else
         fprintf(stderr," -> ");
   }
   INFO_message("");
}

/* print list of xforms in long form */
void
print_all_xforms(atlas_xform_list *xfl)
{
   int i;
   atlas_xform *xf;
   INFO_message("----- Transform list: -------");
   
   if(xfl==NULL)
      return;
   for(i=0;i<xfl->nxforms;i++) {
      xf = xfl->xform+i;
      print_xform(xf);
      INFO_message("-------");
   }
  INFO_message("");
}

/* print the attributes of a transformation including its elements */
void
print_xform(atlas_xform *xf)
{

   int i;
   float *xfptr;
   
   fprintf(stderr, "xform: %s\n", xf->xform_name);
   fprintf(stderr, "xform_type: %s\n", xf->xform_type);
   fprintf(stderr, "xform source: %s   dest: %s\n", xf->source, xf->dest);
   fprintf(stderr, "coord order: %s\n", xf->coord_order);
   fprintf(stderr, "xform dist: %f  inverse: %d   prepost: %d   nelts: %d\n", 
           xf->dist, xf->inverse, xf->prepost, xf->nelts);
   xfptr = (float *) xf->xform;  /* for now use floating point values */
                               /* transformations may require different kinds of
                                       values in the future */
   if(strcmp(xf->xform_type,"Affine")==0)
      print_affine_xform_data(xfptr);
   else {
       for (i=0;i<xf->nelts;i++)
          fprintf(stderr, "%f ", *xfptr++);
       fprintf(stderr,"\n");
   }
}

/* print xform data for affine matrix in 4 columns */
void
print_affine_xform_data(float *xfptr)
{
   int i, j;

   for (i=0;i<3;i++){
      for (j=0;j<4;j++)
         fprintf(stderr, "%f ", *xfptr++);
      fprintf(stderr,"\n");
   } 
   fprintf(stderr,"\n");
}

/* print list of spaces */
void
print_space_list(atlas_space_list *xsl)
{
   int i;
   ATLAS_SPACE *xs;
   
   if(xsl==NULL)
      return;
   if(debug_niml)
      INFO_message("Space list has %d spaces\n",xsl->nspaces);
   INFO_message("----- List of available spaces: -------");
   for(i=0;i<xsl->nspaces;i++) {
      xs = xsl->space+i;
      INFO_message("%s", xs->atlas_space);
   }
   INFO_message("");
}

/* print list of atlases */
void
print_atlas_list(atlas_list *xal)
{
   int i;
   ATLAS *xa;
   
   INFO_message("----- Atlas list: -------");
   if(xal==NULL){
      INFO_message("** No atlases found **");
      return;
   }   
   for(i=0;i<xal->natlases;i++) {
      xa = xal->atlas+i;
      INFO_message("%s", xa->atlas_dset);
   }
}

/* print list of templates */
void
print_template_list(atlas_template_list *xtl)
{
   int i;
   ATLAS_TEMPLATE *xt;
   INFO_message("----- Template list: -------");
   if(xtl==NULL)
      return;
   for(i=0;i<xtl->ntemplates;i++) {
      xt = xtl->atlas_template+i;
      INFO_message("%s", xt->atlas_template);
   }
   INFO_message("");
}

/* calculate list of xforms */
atlas_xform_list *
calc_xform_list(atlas_xform_list *xfl)
{
   int i, nxf, sl1, sl2, cc;
   atlas_xform *xf, *xf2, *xf3, *oldxfptr=NULL;
   char *source, *dest;
   atlas_xform_list *cxfl;

   if(debug_niml)
      printf("calculating xform list\n");   
   if(xfl==NULL)
      return(NULL);
   nxf = (xfl->nxforms) - 1;

#if 0
   /* assign labels for overall list source and destination spaces, 
      checking if either first or last xforms is inverted */
   if(xfl->xform->inverse)
      source = nifti_strdup(xfl->xform->dest);
   else
      source = nifti_strdup(xfl->xform->source);

   xf2 = xfl->xform+nxf;
   if(xf2->inverse)
      dest = nifti_strdup(xf2->source);
   else
      dest = nifti_strdup(xf2->dest);
#endif

   /* make condensed transformation list structure */
   cxfl = (atlas_xform_list *)calloc(1,sizeof(atlas_xform_list));
   if(cxfl==NULL)
      ERROR_exit("Could not allocate space for condensed xform list\n");
   cxfl->xform =  (atlas_xform *)  malloc((xfl->nxforms)*sizeof(atlas_xform));
   if(cxfl->xform==NULL)
      ERROR_exit("Could not allocate space for condensed xform list xforms\n");

   cxfl->nxforms = 0;
   if(debug_niml)
      printf("starting to combine xforms\n");
   /* only one xform, just go home */
   if(xfl->nxforms==1){
      if(debug_niml)
         printf("only 1 xform\n");
      cxfl->nxforms = 1;
      cc = copy_xform(xfl->xform, cxfl->xform);
      if(cc!=0) {
          ERROR_exit("Could not copy only xform for condensed xform list");
      }
      if(cxfl->xform->inverse == 1){   /* check for inverse, even if only one */
         xf = cxfl->xform;
         invert_xform(cxfl->xform);
         source = nifti_strdup(xf->dest);
         dest = nifti_strdup(xf->source);
         free(xf->xform_name);
         free(xf->source); free(xf->dest);
         xf->source = source;
         xf->dest = dest;
         sl1 = strlen(xf->source); sl2 = strlen(xf->dest);
         xf->xform_name = (char *) malloc((sl1+sl2+3)*sizeof(char));
         sprintf(xf->xform_name, "%s::%s", xf->source, xf->dest);
      }
      return(cxfl);
   }
   /* do calculations on xforms a pair at a time */
   xf = xfl->xform;
   for(i=0;i<nxf;i++) {
      if(debug_niml)
         printf("xf %d with xf %d\n", i, i+1);

      xf2 = xfl->xform+i+1;

      if(xf2->inverse)
         dest = nifti_strdup(xf2->source);
      else
         dest = nifti_strdup(xf2->dest);

      if(xf->inverse)
         source = nifti_strdup(xf->dest);
      else
         source = nifti_strdup(xf->source);

      if(debug_niml)
        INFO_message("Multiplying %s type with %s type in chain\n", xf->xform_type,
          xf2->xform_type);

      xf3 = calc_xf(xf,xf2);   /* calculate xforms a pair of time */
      if(xf3) {
         free(xf3->xform_name);
         free(xf3->source); free(xf3->dest);

         xf3->source = source;
         xf3->dest = dest;
         sl1 = strlen(xf3->source); sl2 = strlen(xf3->dest);
         xf3->xform_name = (char *) malloc((sl1+sl2+3)*sizeof(char));
         sprintf(xf3->xform_name, "%s::%s", xf3->source, xf3->dest);

         if(i==(nxf-1)){
            if(debug_niml)
               printf(
                 "On last xform, copying last combined xform"
                 " to combined xform list\n");
            cc = copy_xform(xf3, cxfl->xform+(cxfl->nxforms));
            cxfl->nxforms++;
            if(debug_niml){
               print_xform(xf3);
               xf = xf3;
               print_xform(xf);
              }
            }
         else{
            if(debug_niml)
               printf("could combine xform %d with %d\n", i, i+1);
            xf = xf3; cc = 0;
/*            cc = copy_xform(xf3, xf); */ /* use combined xform for next pair */
            if(debug_niml)
               print_xform(xf);

         }

      }
      else {
         if(debug_niml)
            printf("could not calculate this combination of xforms - adding to chain\n");
         cc = copy_xform(xf, cxfl->xform+(cxfl->nxforms));
         cxfl->nxforms++;
         if((cc==0)&&(i<nxf-1))
             xf = xf2; cc = 0;
             /*copy_xform(xf2, xf); */ /* update start xform for next pair */
      }

      if(i>0)   /* free the temporary xform intermediate */
          free_xform(oldxfptr);
      oldxfptr = xf3;

      if(cc!=0) {
          ERROR_exit("Could not copy a xform for condensed xform list");
      }

   }

#if 0
   /* copy source name from 1st xform */
   nxf = cxfl->nxforms;
   xf = cxfl->xform;
   free(xf->source);
   xf->source = source;
   /* copy dest name from last xform */
   xf = cxfl->xform+nxf;
   free(xf->dest); 
   /* make combination name for 1st xform list that condense down to one */
   if(nxf==1){
      free(xf->xform_name);
      xf->dest = dest;
      sl1 = strlen(source); sl2 = strlen(dest);
      xf->xform_name = (char *) malloc((sl1+sl2+3)*sizeof(char));
      sprintf(xf->xform_name, "%s::%s", xf->source, xf->dest);
   }
#endif
   
   return(cxfl);
}

/* calculate product for pair of xforms */
/* return xform product - allocating space even if copy 
 If xf and xf2 are both affine transformations, 
   return xf2 * xf */
atlas_xform *
calc_xf(atlas_xform *xf, atlas_xform *xf2)
{
   atlas_xform *xf3;
   int cc;

/*   if(debug_niml)
      INFO_message("Multiplying %s type with %s type\n", xf->xform_type,
      xf2->xform_type);
*/
   
   xf3 = malloc(sizeof(atlas_xform));
   if(xf3==NULL)
      return(NULL);
   invert_xform(xf);   /* possibly need to invert transform */
   invert_xform(xf2);


   /* check for identity transformations - simplest */
   if(strcmp(xf->xform_type,"Identity")==0){
       cc = copy_xform(xf2,xf3);
       if(cc!=0) {
           return(NULL);
       }
       else return(xf3);
   }

   if(strcmp(xf2->xform_type,"Identity")==0){
       cc = copy_xform(xf,xf3);
       if(cc!=0) {
           return(NULL);
       }
       else return(xf3);
   }


   if(debug_niml)
      INFO_message("Multiplying %s type with %s type\n", xf->xform_type,
      xf2->xform_type);
   if(strcmp(xf->xform_type,"Affine")==0){
      if(strcmp(xf2->xform_type,"Affine")==0){
         cc = affine_mult(xf,xf2,xf3);
         if(debug_niml)
            INFO_message("combining two affine matrices\n");
         if(cc!=0) {
             if(debug_niml)
               INFO_message("could not combine two affine matrices\n");

             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"2-piece")==0){
         cc = affine_2piece_mult(xf,xf2,xf3,0);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"12-piece")==0){
         cc = affine_12piece_mult(xf,xf2,xf3,0);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }      
   }

   if(strcmp(xf->xform_type,"2-piece")==0){
      if(strcmp(xf2->xform_type,"Affine")==0){
         cc = affine_2piece_mult(xf,xf2,xf3,-1);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"2-piece")==0){
         cc = x2piece_2piece_mult(xf,xf2,xf3);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"12-piece")==0){
         cc = x2piece_12piece_mult(xf,xf2,xf3,0);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
   }

   if(strcmp(xf->xform_type,"12-piece")==0){
      if(strcmp(xf2->xform_type,"Affine")==0){
         cc = affine_12piece_mult(xf,xf2,xf3,-1);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"2-piece")==0){
         cc = x2piece_12piece_mult(xf,xf2,xf3,-1);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"12-piece")==0){
         cc = x12piece_12piece_mult(xf,xf2,xf3);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
   }

   INFO_message("AFNI doesn't know how to combine these transforms");
   return(NULL);
}

/* invert transformation - general form */
int
invert_xform(atlas_xform *xf)
{
   int cc =1;

   if(xf->inverse==0) return(0);
   if(strcmp(xf->xform_type,"Affine")==0){
      cc = invert_affine(xf);
   }
   if(strcmp(xf->xform_type,"12-piece")==0){
      cc = invert_12piece(xf);
   }
   if(strcmp(xf->xform_type,"2-piece")==0){
      cc = invert_2piece(xf);
   }

   xf->inverse = 0; /* should never be an inverse transform here */

   return(cc);
}

/* invert an affine matrix - do in place */
/* return error code - 0 for no error */
int
invert_affine(atlas_xform *xf)
{
   int i, j;
   matrix tempmat, invmat;
   float *xfptr;
   ENTRY("invert_affine");

   matrix_initialize (&tempmat);
   matrix_create(4,4,&tempmat);
   xfptr = (float *) xf->xform;
   for(i=0;i<3;i++)
      for(j=0;j<4;j++)
         tempmat.elts[i][j] = (double) *xfptr++; /* recast float to double*/
   tempmat.elts[3][0] =  tempmat.elts[3][1] = tempmat.elts[3][2] = 0.0;
   tempmat.elts[3][3] = 1.0;
   matrix_initialize (&invmat);
   matrix_inverse(tempmat, &invmat);

   xfptr = (float *) xf->xform;
   for(i=0;i<3;i++)
      for(j=0;j<4;j++)
        *xfptr++ = (float) invmat.elts[i][j];

   matrix_destroy(&invmat);
   matrix_destroy(&tempmat);

   return(0);
}

/* invert a 12 piece matrix - do in place */
/* return error code - 0 for no error */
int
invert_12piece(atlas_xform *xf)
{
   int cc;
   
   return(1);
}


/* invert a 2 piece matrix - do in place */
/* return error code - 0 for no error */
int
invert_2piece(atlas_xform *xf)
{
   int cc;
   
   return(1);
}

/* multiply affine transformations */
int
affine_mult(atlas_xform *xf, atlas_xform *xf2, atlas_xform *xf3)
{
   int cc, i, j;
   matrix sm1, sm2, sm3;
   float *xfptr, *xfptr2;
   
   cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);

   matrix_initialize(&sm1);
   matrix_initialize(&sm2);
   matrix_initialize(&sm3);
   matrix_create(4,4,&sm1);
   matrix_create(4,4,&sm2);

   xfptr = (float *) xf->xform;
   xfptr2 = (float *) xf2->xform;
   for(i=0;i<3;i++)
     for(j=0;j<4;j++) {
        sm1.elts[i][j] =  (double) *xfptr++;
        sm2.elts[i][j] =  (double) *xfptr2++;
     }      
   sm1.elts[3][0] =  sm1.elts[3][1] = sm1.elts[3][2] = 0.0;
   sm1.elts[3][3] = 1.0;
   sm2.elts[3][0] =  sm2.elts[3][1] = sm2.elts[3][2] = 0.0;
   sm2.elts[3][3] = 1.0;

   matrix_multiply(sm1, sm2, &sm3);
   
   xfptr = (float *) xf3->xform;
   for(i=0;i<3;i++)
     for(j=0;j<4;j++) {
        *xfptr++ = (float) sm3.elts[i][j];
     }      

   matrix_destroy(&sm1);
   matrix_destroy(&sm2);
   matrix_destroy(&sm3);
   
   if(xf->xform_type) free(xf->xform_type);
   xf->xform_type = nifti_strdup("Affine");

   return(0);
}

/* multiply affine transformation by 2-piece transform */
int
affine_2piece_mult(atlas_xform *xf, atlas_xform *xf2, atlas_xform *xf3, int dir)
{
   int cc;
   
   return(1);  /* can't do this yet */
   if(dir)
      cc = copy_xform(xf2,xf3);
   else   
      cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}


/* multiply affine transformation by 12-piece transform */
int
affine_12piece_mult(atlas_xform *xf, 
  atlas_xform *xf2, atlas_xform *xf3, int dir)
{
   int cc;
   
   return(1);
   
   if(dir)
      cc = copy_xform(xf2,xf3);
   else   
      cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}

/* multiply two 2-piece affine transformations */
int
x2piece_2piece_mult(atlas_xform *xf, 
  atlas_xform *xf2, atlas_xform *xf3)
{
   int cc;
   
   return(1);
   cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}

/* multiply a 2-piece and a 12-piece affine transformation */
int
x2piece_12piece_mult(atlas_xform *xf, 
  atlas_xform *xf2, atlas_xform *xf3, int dir)
{
   int cc;
   
   return(1);
   if(dir)
      cc = copy_xform(xf2,xf3);
   else   
      cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}


/* multiply two 12-piece affine transformations */
int
x12piece_12piece_mult(atlas_xform *xf, 
  atlas_xform *xf2, atlas_xform *xf3)
{
   int cc;
   
   return(1);
   cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}


/* apply xform to xyz */
int
apply_xform_general(atlas_xform *xf, float x,float y,float z, \
                        float *xout, float *yout, float *zout)
{
   int xgc = 1;
   if(strcmp(xf->xform_type,"Affine")==0){
      xgc = apply_xform_affine(xf, x, y, z, xout, yout, zout);
   }
   if(strcmp(xf->xform_type,"2-piece")==0){
      xgc = apply_xform_2piece(xf, x, y, z, xout, yout, zout);
   }
   
   if(strcmp(xf->xform_type,"brett_tt2mni")==0){
      xgc = apply_xform_brett_tt2mni(x, y, z, xout, yout, zout);
   }

   if(strcmp(xf->xform_type,"brett_mni2tt")==0){
      xgc = apply_xform_brett_mni2tt(x, y, z, xout, yout, zout);
   }
   
   if(strcmp(xf->xform_type,"12-piece")==0){
      xgc = apply_xform_12piece(xf, x, y, z, xout, yout, zout);
   }
   if(strcmp(xf->xform_type,"Identity")==0){
      *xout = x; *yout = y; *zout = z; xgc = 0;
   }
   return(xgc);
}


/* apply xform chain to xyz */
int
apply_xform_chain(atlas_xform_list *xfl, float x, float y, float z,
                  float *xout, float *yout, float *zout)
{

   int i, nxf, xgc;
   float xxout, yyout, zzout;
   atlas_xform * xf;
        
   nxf = xfl->nxforms;
   
   if(nxf==0) return(0);
   
   for(i=0;i<nxf;i++) {
      xf = xfl->xform+i;
      xgc = apply_xform_general(xf, x, y, z, &xxout, &yyout,  &zzout);
      if(xgc==0) {
        x = xxout;
        y = yyout;
        z = zzout;
      }
      else {
         WARNING_message("Could not transform between spaces");
         return(-1);
      }  
   } 

   *xout = xxout;
   *yout = yyout;
   *zout = zzout;

   return(0);
}

/* apply the forward affine transformation  to the xyz */
int
apply_xform_affine(atlas_xform *xf, float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
   float *xfptr;
   float x1, x2, x3, x4;
   
   if(xf->xform==NULL) return(1);

   xfptr = (float *) xf->xform;
   
   xfptr = (float *) xf->xform;
   
   *xout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *yout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *zout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   
    return(0);
}

/* apply the forward 2-piece transformation  to the xyz coordinate
  2-piece transformations apply one of two affine transformations
  depending on some dividing plane defined by x,y or z equal to 
  a particular value. This dividing plane  can be defined on the data
  before the transformation or it may be defined 'post' transformation.
  If a post transformation, the second xform applies an alternate
  xform to the coordinate transformed by the first affine xform.
*/
int
apply_xform_2piece(atlas_xform *xf, float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
   float *xfptr;
   float lx,ly,lz;
   int apply_post;
   
   /* brett transform - tta to mni
   1.0101  0        0         0
   0       1.02962 -0.05154   0
   0       0.05434  1.08554   0

   1.0101  0        0         0
   0       1.02962 -0.05154   0
   0       0.0595194  1.18892   0 
   */
   if(xf->xform==NULL) return(1);

   xfptr = xf->xform;
   /* change input coords to lpi if xform defined that way (RAI input)*/
   if(strcmp(xf->coord_order,"lpi") == 0){
      x = -x; y =-y;      
   }

   /* If this is a pre transformation, check the x,y,z limits. 
      If xyz > limit, use the second transformation */
   if(xf->prepost==0){
      lx = *xfptr++; ly = *xfptr++; lz = *xfptr++;
      if(lx > -9998) {
         if(x>lx)
            xfptr += 12;
      }
      if(ly > -9998) {
         if(y>ly)
            xfptr += 12;
      }
      if(lz > -9998) {
         if(z>lz)
            xfptr += 12;
      }
   }

   *xout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *yout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *zout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);   

   if(xf->prepost){
      apply_post = 0;
      lx = *xfptr++; ly = *xfptr++; lz = *xfptr++;
      if(lx > -9998) {
         if(x>lx)
            apply_post = 1;
       }
      if(ly > -9998) {
         if(y>ly)
            apply_post = 1;
      }
      if(lz > -9998) {
         if(z>lz)
            apply_post = 1;
      }
      if(apply_post) {
         x = *xout; y = *yout; z = *zout;
         xfptr += 4;
         *xout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
         xfptr += 4;
         *yout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
         xfptr += 4;
         *zout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);   
      }         
   }
   if(strcmp(xf->coord_order,"lpi") == 0){
      *xout = -(*xout); *yout= -(*yout);      
   }

   return(0);
}

/* apply the forward 12-piece transformation  to the xyz */
int
apply_xform_12piece(atlas_xform *xf, float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
   float *xfptr;
   static THD_talairach_12_warp ww;

   return(1);   /* doesn't work yet */

   if(xf->xform==NULL) return(1);
/*   LOAD_FVEC3( mv , x,y,z ) ;   
   tv = AFNI_forward_warp_vector(MNI_N27_to_TLRC_DSET->warp, mv);
*/   return(0);
}

/* apply Brett transform to transform from TT to MNI (Talairach to MNI)*/
/* special case of 2-piece transform and maybe the only one we will ever use */
int
apply_xform_brett_tt2mni(float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
   
   /* brett transform - tta to mni
   1.0101  0        0         0
   0       1.02962 -0.05154   0
   0       0.05434  1.08554   0

   1.0101  0        0         0
   0       1.02962 -0.05154   0
   0       0.0595194  1.18892   0 
   */

   THD_3tta_to_3mni(&x, &y, &z);      /* xform tt to mni space - results in lpi order */
   *xout = -x; *yout = -y; *zout = z; /* put coords back in RAI */

   return(0);
}



/* apply Brett transform to transform from MNI (MNI to Talairach)*/
/* special case of 2-piece transform and maybe the only one we will ever use */
int
apply_xform_brett_mni2tt(float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
   x = -x; y = -y;                   /* put coords in lpi from RAI first */   
   THD_3mni_to_3tta(&x, &y, &z);      /* xform mni to tt space - results in RAI order */

   return(0);
}

#if 0
      int N_Neighb_Max = 5; /* max number of neighbors a node can have*/
      int N_Node = 7;
      int N_Neighb[7];
      int N_np=4;
      int np[4][2];

      int *nPath=NULL, N_n=0;
      float nDistance=0.0;
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
      
    return(0);
}

#endif

/* read xform from NIML attributes into xform_struct */
int atlas_read_xform(NI_element *nel, atlas_xform *atlas_xf)
{
   float dist;
   int i;
   float *xfptr;
   char *sptr;

   if(debug_niml) {
      INFO_message("xform_name %s", NI_get_attribute(nel, "xform_name"));
      INFO_message("xform_type %s", NI_get_attribute(nel, "xform_type"));
      INFO_message("xform source %s", NI_get_attribute(nel, "source"));
      INFO_message("xform dest   %s", NI_get_attribute(nel, "dest"));
      INFO_message("xform number of elements %d", nel->vec_num);
      INFO_message("xform prepost %s", NI_get_attribute(nel, "prepost")); 
      INFO_message("xform coord_order %s", NI_get_attribute(nel, "coord_order")); 
   }   
   atlas_xf->xform_type = nifti_strdup(NI_get_attribute(nel, "xform_type")); 
   atlas_xf->xform_name = nifti_strdup(NI_get_attribute(nel, "xform_name")); 
   atlas_xf->source = nifti_strdup(NI_get_attribute(nel, "source")); 
   atlas_xf->dest = nifti_strdup(NI_get_attribute(nel, "dest")); 
   if(NI_get_attribute(nel, "distance")){
      dist = atof( NI_get_attribute(nel, "distance"));
      if(dist<=0) {
         WARNING_message("Distance less than or equal to 0 reset to 1");
         dist = 1;
      }
   }
   else
      dist = 1;
   atlas_xf->dist = dist;
   if(sptr = NI_get_attribute(nel, "prepost")){
       atlas_xf->prepost = atoi(sptr);
   }
   else
       atlas_xf->prepost = 0;   /*assume pre-xform (used for 2 and 12 part xforms */
       
   if(sptr = NI_get_attribute(nel, "coord_order")){
      atlas_xf->coord_order = nifti_strdup(sptr);
   }
   else
      atlas_xf->coord_order = nifti_strdup("rai");
   
   if((atlas_xf->xform_type == NULL) || (atlas_xf->source == NULL) ||
     (atlas_xf->dest == NULL) || (atlas_xf->xform_name==NULL) ||
     (atlas_xf->coord_order == NULL)) {
      WARNING_message("Could not allocate transformation type string");
      return(1);
   }
   
   atlas_xf->nelts = nel->vec_num;
   atlas_xf->inverse = 0;
   
   atlas_xf->xform = malloc(nel->vec_num * sizeof(float));
   if(atlas_xf->xform == NULL) {
      WARNING_message("Could not allocate transformation");
      return(1);
   }

  for(i=0;i<nel->vec_num;i++){
     memcpy((char *)(atlas_xf->xform)+i*sizeof(float), nel->vec[i], sizeof(float));
  }
  if(debug_niml)
      print_xform(atlas_xf);
   return(0);
}

/* read template info from NIML attributes into template structure */
int atlas_read_template(NI_element *nel, ATLAS_TEMPLATE *atlas_tpl)
{
   if(debug_niml) {
      INFO_message("atlas_template %s", NI_get_attribute(nel, "template_name"));
      INFO_message("templ_space %s", NI_get_attribute(nel, "atlas_space"));
   }

   atlas_tpl->atlas_template = nifti_strdup(NI_get_attribute(nel, "template_name")); 
   atlas_tpl->atlas_space = nifti_strdup(NI_get_attribute(nel, "atlas_space"));

   if((atlas_tpl->atlas_template == NULL) || (atlas_tpl->atlas_space == NULL)) {
      WARNING_message("Could not allocate template strings");
      return(1);
   }

   return(0);
}

/* read atlas info from NIML attributes into atlas structure */
int atlas_read_atlas(NI_element *nel, ATLAS *atlas)
{
   if(debug_niml) {
      INFO_message("atlas_name %s", NI_get_attribute(nel, "atlas_name"));
      INFO_message("atlas_space %s", NI_get_attribute(nel, "atlas_space"));
   }

   atlas->atlas_dset = nifti_strdup(NI_get_attribute(nel, "atlas_name")); 
   atlas->atlas_space = nifti_strdup(NI_get_attribute(nel, "atlas_space"));

   if((atlas->atlas_dset == NULL) || (atlas->atlas_space == NULL)) {
      WARNING_message("Could not allocate atlas strings");
      return(1);
   }

   return(0);
}

/* read template space info from NIML attributes into template space structure */
int atlas_read_atlas_space(NI_element *nel, ATLAS_SPACE *at_space)
{
   if(debug_niml) {
      INFO_message("space_name %s", NI_get_attribute(nel, "space_name"));
      INFO_message("generic_space %s", NI_get_attribute(nel, "generic_space"));
   }

   at_space->atlas_space = nifti_strdup(NI_get_attribute(nel, "space_name")); 
   at_space->generic_space = nifti_strdup(NI_get_attribute(nel, "generic_space"));

   if((at_space->atlas_space == NULL) || (at_space->generic_space == NULL)) {
      WARNING_message("Could not allocate template space strings");
      return(1);
   }

   return(0);
}



NI_element *NI_find_next_element(NI_stream ns, char *name)
{
   NI_element *nel;
 
   nel = (NI_element *) 1;
   while(nel) {
fprintf(stderr,"reading elements\n");   
      nel = NI_read_element(ns, 100);
      if(nel) {
         fprintf(stderr,"nel name %s\n", nel->name);
         if (nel->type == NI_ELEMENT_TYPE) {
            if(strcmp(name, nel->name) == 0) {
               fprintf(stderr, "name matches \n");
               return(nel);
            }
         }      
      }
   }
   return(NULL);
}
