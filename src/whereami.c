/*** Whereami.c modified 1/11/05 -- main function by Mike Angstadt of U Chicago ***/

#define MAIN
#define SUMA_noFunc

#include "mrilib.h"
#include "afni.h"
#include <stdio.h>
#include <stdlib.h>
#include "matrix.h"
#include "suma_suma.h"


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

int compute_overlap(char *bmsk, byte *cmask, int ncmask, int dobin,
  int N_atlas_names, char **atlas_names, ATLAS_LIST *atlas_alist);
static float *make_coord_list(char *coord_file, int *nxyz, int dicom);


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
   snprintf(strbuf+i, 490*sizeof(char), "\n            -> %s", pj);
   
   return(strbuf);
}

int print_atlas_reference(char *atname) 
{
   int i = 0, N_refs=0;
   char **refs=NULL;
   ATLAS *atlas;
   atlas = Atlas_With_Trimming(atname, 1, NULL);
   if (atlas && ATL_COMMENT(atlas)) {
     print_atlas_comment(atlas);
   }
   else {
     refs = atlas_reference_string_list(atname, &N_refs);
     while (i < N_refs) {
        printf("%s", PrettyRef(refs[i]));
        ++i;
     }
     if (refs) refs = free_names_list(refs, N_refs);
   }

   return(1);
}


void whereami_usage(ATLAS_LIST *atlas_alist, int detail) 
{
   /* print help message in three sections */
   printf(  
"Usage: whereami [x y z [output_format]] [-lpi/-spm] [-atlas ATLAS] \n"
"   ++ Reports brain areas located at x y z mm in some template space\n"
"   ++ according to atlases present with your AFNI installation.\n"
"   ++ Show the contents of available atlases\n"
"   ++ Extract ROIs for certain atlas regions using symbolic notation\n"
"   ++ Report on the overlap of ROIs with Atlas-defined regions.\n"
"\n%s", detail ? "":"use -h or -help for more help detail.\n");
   if (detail) {
      printf ( 
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
" -linkrbrain: get report from linkRbrain from list of coordinates\n"
"           only with -coord_file and -space or -dset_space\n"
" -linkr_type tasks/genes: report for correlation with tasks or genes\n"
"           Default is tasks\n"
" -lpi/-spm: Input coordinates' orientation is in LPI or SPM format. \n"
" -rai/-dicom: Input coordinates' orientation is in RAI or DICOM format.\n"
" NOTE: The default format for input coordinates' orientation is set by \n"
"       AFNI_ORIENT environment variable. If it is not set, then the default \n"
"       is RAI/DICOM\n"
" -space SPC: Space of input coordinates.\n"
"       SPC can be any template space name. Without a NIML table definition,\n"
"       the space name is limited to MNI, MNI_ANAT or TLRC (the default).\n"
" -classic: Classic output format (output_format = 0).\n"
" -tab: Tab delimited output (output_format = 1). \n"
"       Useful for spreadsheeting.\n"
" -atlas ATLAS: Use atlas ATLAS for the query.\n"
"               You can use this option repeatedly to specify\n"
"               more than one atlas. Default is all available atlases.\n");
   if (detail > 1) {
      printf("              ATLAS is one of:\n");
      print_atlas_table(atlas_alist);
   } else {
      printf("              Use whereami -help to see all available atlases.\n");
   }

  /* third section for usage help*/
  printf(
" -dset: Determine the template space to use from this reference dataset\n"
"        Space for human data is usually TLRC, MNI, MNI_ANAT.\n"
"        If the space is known and a reference atlas can be found, the\n"
"        regions will be based on the coordinates from this template space.\n"
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
" -min_prob MIN_PROB: set minimum probability to consider in probabilistic\n"
"             atlas output. This option will overrid the value set by the\n"
"             environment variable AFNI_WHEREAMI_PROB_MIN (default is 1E-10)\n"
" NOTE: You can turn off some of the whining by setting the environment \n"
"       variable  AFNI_WHEREAMI_NO_WARN\n"            
" -debug DEBUG: Debug flag\n"
" -verb VERB: Same as -debug DEBUG\n"
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
"        and must be at the same resolution as the bmask (binary mask) or\n"
"        the omask (the ordered mask) datasets.\n"
"        This option follows the style of 3dmaskdump (since the\n"
"        code for it was, uh, borrowed from there (thanks Bob!, thanks Rick!)).\n"
"        See '3dmaskdump -help' for more information.\n"
"\n"
"Note on the reported coordinates of the Focus Point:\n"
"----------------------------------------------------\n"
"Coordinates of the Focus Point are reported in available template spaces in\n"
"LPI coordinate order. The three principal spaces reported are Talairach \n"
" (TLRC), MNI, MNI Anatomical (MNI_ANAT).\n"
"  The TLRC coordinates follow the convention specified by the Talairach and \n"
"     Tournoux Atlas.\n"
"  The MNI coordinates are derived from the TLRC ones using an approximation \n"
"     equation.\n"
"  The MNI Anat. coordinates are a shifted version of the MNI coordinates \n"
"     (see Eickhoff et al. 05).\n"
"\n"
" For users who do not use the NIML table method of specifying template \n"
" and transformations, the MNI coordinates reported here are derived from TLRC\n"
" by an approximate function (the Brett transform). For transformations\n"
" between MNI_ANAT and TLRC coordinates, the 12 piece-wise linear transformation\n"
" that was used to transform the MNI_ANAT N27 brain to TLRC space is also\n"
" used to compute the coordinates in either direction.\n"
" For users who do use the NIML table method, the transformations among\n"
" the various Talairach, MNI and MNI_ANAT spaces may be performed a variety\n"
" of ways. The default method uses the Brett transform for TLRC to MNI, and\n"
" a simple shift for MNI to MNI_ANAT.\n"
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
"\n", Init_Whereami_Max_Find(), Init_Whereami_Max_Rad());
   if (detail > 1) {
      printf(
"Convenient Color maps For Atlas Datasets:\n"
"----------------------------------------\n"
"   Color maps (color scales) for atlas dataset should automatically be used\n"
"   when these datasets are viewed in the overlay. To manually select a\n"
"   a specific color scale in the AFNI GUI's overlay panel:\n"
"     o set the color map number chooser to '**' \n"
"     o right-click on the color map's color bar and select \n"
"       'Choose Colorscale'\n"
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
      );
   } else {
      printf("    Use whereami -help to see more information on:\n"
             "      Convenient Color maps For Atlas Datasets:\n"
             "         and \n"
             "      How To See Atlas regions overlaid in the AFNI GUI:\n"
      );
   }
   printf(
"Example 1:\n"
"----------\n"
"   To find a cluster center close to the top of the brain at -12,-26, 76 (LPI),\n"
"   whereami, assuming the coordinates are in Talairach space, would report:\n"
"    whereami -12 -26 76 -lpi\n"
"     ++ Input coordinates orientation set by user to LPI\n"
"     +++++++ nearby Atlas structures +++++++\n"
"\n"
"     Original input data coordinates in TLRC space\n"
"\n"
"     Focus point (LPI)=\n"
"        -12 mm [L], -26 mm [P],  76 mm [S] {TLRC}\n"
"        -12 mm [L], -31 mm [P],  81 mm [S] {MNI}\n"
"        -13 mm [L], -26 mm [P],  89 mm [S] {MNI_ANAT}\n"
"\n"
"     Atlas CA_N27_MPM: Cytoarch. Max. Prob. Maps (N27)\n"
"       Within 4 mm: Area 6\n"
"       Within 7 mm: Area 4a\n"
"\n"
"     Atlas CA_N27_ML: Macro Labels (N27)\n"
"       Within 1 mm: Left Paracentral Lobule\n"
"       Within 6 mm: Left Precentral Gyrus\n"
"          -AND- Left Postcentral Gyrus\n"
"\n"
"Example 2:\n"
"----------\n"
"   To create a mask dataset of both  left and right amygdala, you can do:\n"
"    whereami -prefix amymask -mask_atlas_region 'TT_Daemon::amygdala'\n\n"
"\n"
"   Note masks based on atlas regions can be specified \"on the fly\" in \n"
"   the same way with other afni commands as a dataset name (like 3dcalc,\n"
"   for instance), so a mask, very often, is not needed as a separate,\n"
"   explicit dataset on the disk.\n\n"
"\n"
"Example 3:\n"
"----------\n"
"   To create a mask from a FreeSurfer 'aparc' volume parcellation:\n"
"   (This assumes you have already run @SUMA_Make_Spec_FS, and your\n"
"    afni distribution is recent. Otherwise update afni then run:\n"
"    @MakeLabelTable -atlasize_labeled_dset aparc.a2009s+aseg_rank.nii\n"
"    from the SUMA/ directory for that subject.)\n"
"   To find the region's name, try something like:\n"
"    whereami -atlas aparc.a2009s+aseg_rank -show_atlas_code | grep -i insula\n"
"   Or you can try this search, assuming you screwed up the spelling:\n"
"   whereami -atlas aparc+aseg_rank -show_atlas_code | \\\n"
"                                  apsearch -word insola -stdin\n"
"   If you really screw up the spelling try:\n"
"   whereami -atlas aparc+aseg_rank -show_atlas_code | \\\n"
"                                  sed 's/[-_]/ /g'  | \\\n"
"                                  apsearch -word insolent -stdin\n"
"   Pick one area then run:\n"
"    whereami -atlas aparc.a2009s+aseg_rank \\\n"
"               -mask_atlas_region   \\\n"
"                     aparc.a2009s+aseg_rank::ctx_rh_S_circular_insula_sup\n"
"\n"
"\n");

printf(
" \n---------------\n"
" Atlas NIML tables:\n"
" Atlas, templates, template spaces and transforms may all now be specified\n"
" in a text file that follows an XML-like format, NIML. The specifications\n"
" for the NIML table files will be described more fully elsewhere, but an\n"
" overview is presented here. By default, and soon to be included with the\n"
" AFNI distributions, the file AFNI_atlas_spaces.niml contains entries for\n"
" each of the available atlases, template spaces, templates and \n"
" transformations. Two other additional files may be specified and changed\n"
" using the environment variables, AFNI_SUPP_ATLAS and AFNI_LOCAL_ATLAS.\n"
" It is best to examine the provided NIML table as an example for extending\n"
" and modifying the various atlas definitions.\n"
"\n"
" Show atlas NIML table options:\n"
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
"     examples: convert coordinates from TT_N27 to MNI or MNI anat space\n"
"             whereami -calc_chain TT_N27 MNI  -xform_xyz_quiet 10 20 30\n"
"             whereami -calc_chain TT_N27 MNI  -xform_xyz_quiet 0 0 0\n"
"             whereami -calc_chain TT_N27 MNIA -xform_xyz_quiet 0 0 0\n"
" -xform_xyz : used with calc_chain, takes the x,y,z coordinates and \n"
"             applies the combined chain of transformations to compute\n"
"             a new x,y,z coordinate\n"
" -xform_xyz_quiet : Same as -xform_xyz but only ouputs the final result\n"
" -coord_out  outfile : with -xform_xyz, -coord_file and -calc_chain, \n"
"             specifies an output file for transformed coordinates\n"
"             If not specified, coord_files will be transformed and printed\n"
"             to stdout \n"
"Note setting the environment variable AFNI_WAMI_DEBUG will show detailed\n"
" progress throughout the various functions called within whereami.\n"
" For spaces defined using a NIML table, a Dijkstra search is used to find\n"
" the shortest path between spaces. Each transformation carries with it a\n"
" distance attribute that is used for this computation. By modifying this\n"
" field, the user can control which transformations are preferred.\n\n"
" -web_atlas_type XML/browser/struct : report results from web-based atlases\n"
"            using XML output to screen, open a browser for output or just\n"
"            return the name of the structure at the coordinate\n"
" -html   :  put whereami output in html format for display in a browser\n"

" \n---------------\n"
" More information about Atlases in AFNI can be found here:\n"
"      https://afni.nimh.nih.gov/sscc/dglen/AFNIAtlases\n"
" Class document illustrating whereami usage:\n"
"      https://afni.nimh.nih.gov/pub/dist/edu/latest/afni11_roi/afni11_roi.pdf\n"
"---------------\n"
);

if (detail > 1) {
   printf(     
     "Global Options (available to all AFNI/SUMA programs)\n"
     "%s\n%s",
     SUMA_Offset_SLines(get_help_help(),2), get_gopt_help());
}
      
printf("Thanks to Kristina Simonyan for feedback and testing.\n");

}
   PRINT_COMPILE_DATE ;
   return;
}


/*----------------------------------------------------------------------------*/

int main(int argc, char **argv)
{
   float x, y, z, xi, yi, zi;
   char *string, *shar = NULL;
   int output = 0;
   int nakedland = 0, k = 0, Show_Atlas_Code=0;
   int iarg, dicom = 1, i, nakedarg, arglen, ixyz=0, nxyz=0;
   AFNI_ATLAS *aa = NULL;
   AFNI_ATLAS_REGION *aar= NULL;
   byte OldMethod = 0;
   char **atlas_names=NULL;
   int N_atlas_names = 0, nbest = 0;
   byte atlas_sort = 1, write_mask=0;
   ATLAS_SEARCH *as=NULL;
   char *mskpref= NULL, *bmsk = NULL;
   byte *cmask=NULL ; int ncmask=0 ;
   int dobin = 0, N_areas, mni;
   char *coord_file=NULL;
   float *coord_list = NULL, rad;
   THD_3dim_dataset *space_dset = NULL, *atlas_dset = NULL;
   int read_niml_atlas = 0, show_atlas = 0, show_atlas_spaces = 0;
   int show_atlas_templates = 0, show_atlas_xforms = 0;
   int show_xform_chain = 0, calc_xform_chain=0, show_avail_space=0;
   int show_atlas_point_lists = 0;
   int linkrbrain_output = 0;
   /* coordinates translated to send to linkrbrain */
   char *linkrbrain_xml = "__temp_linkrbrain.xml"; 
   /* unparsed XML response from linkrbrain */
   char *temp_linkrbrain_results = "__temp_linkrbrain_results.xml";
   char *linkrbrain_str = NULL;
   char *linkrbrain_corr_test = NULL;
   int linkr_corr_type = 0; /* linkrbrain correlation for tasks by default */
   char *srcspace=NULL, *destspace=NULL;
   char *coord_outfile=NULL;

   ATLAS_XFORM_LIST *xfl = NULL, *cxfl = NULL;
   float xout, yout, zout;
   int xform_xyz = 0, xform_xyz_quiet = 0;
   int atlas_writehard = 0, atlas_readhard = 0, alv=1, wv=1;
   ATLAS_LIST *atlas_alist=NULL, *atlas_list=NULL, *atlas_rlist=NULL;
   byte b1;
   int LocalHead = wami_lh();
   float minprob;

   mainENTRY("whereami main"); machdep(); AFNI_logger("whereami",argc,argv);
   
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
   OldMethod = 0; /* Leave at 0 */
   coord_file = NULL;
   alv=2; wv=2;
   xform_xyz_quiet = 0;
   iarg = 1 ; nakedarg = 0; Show_Atlas_Code = 0; shar = NULL;

   set_TT_whereami_version(alv,wv);
   if(alv<2)
      init_custom_atlas();   /* allow for custom atlas in old framework */
   xi = 0.0; yi=0.0, zi=0.0;
   set_wami_web_reqtype(WAMI_WEB_STRUCT); /* set web atlas output to simple structure */
   set_AFNI_wami_output_mode(0);   /* turn off HTML formatted output */   

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
            fprintf(stdout,"Anatomy Toolbox Version in AFNI is:\n%s\n", 
                           atlas_version_string("CA_N27_MPM"));  
            return(0);
         }
         
         if (strcmp(argv[iarg],"-ca_n27_reference") == 0) { 
            fprintf(stdout,"References for Anatomy Toolbox %s:\n", 
                           atlas_version_string("CA_N27_MPM"));  
            print_atlas_reference("CA_N27_MPM");
            fprintf(stdout,"\n");
            return(0);
         }
         
         if (  strcmp(argv[iarg],"-rai") == 0 || 
               strcmp(argv[iarg],"-dicom") == 0) { 
            dicom = 1; 
            ++iarg;
            continue; 
         }
         
         if (strcmp(argv[iarg],"-h") == 0 || strcmp(argv[iarg],"-help") == 0 ) { 
            atlas_alist = get_G_atlas_list();
            whereami_usage(atlas_alist, strlen(argv[iarg]) > 3 ? 2:1);
            return(0); 
            continue; 
         }
         if (strcmp(argv[iarg],"-old") == 0 ) { 
            fprintf( stderr,
                        "** Error: This option is no longer in use\n");
            return(1);
            OldMethod = 1; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-space") == 0) { 
            ++iarg;
            if (iarg >= argc) {
               fprintf( stderr,
                        "** Error: Need parameter after -space\n"); return(1);
            }
            if (srcspace) {
               fprintf( stderr,
               "** Error: Specify space of input (or output mask) with either -space or -dset, not both\n");
               return(1);
            }

            /* use srcspace as is on commandline */
            srcspace = argv[iarg];
            if ( strcmp(argv[iarg],"Paxinos_Rat_2007@Elsevier")==0 )
               srcspace = "paxinos_rat_2007@Elsevier";
            set_out_space(srcspace);   /* make output space for mask dset */

            ++iarg;
            continue; 
         }

         if (strcmp(argv[iarg],"-web_atlas_type") == 0) { 
            ++iarg;
            if (iarg >= argc) {
               fprintf( stderr,
                        "** Error: Need parameter after -web_atlas_type\n"); return(1);
            }
            if(strcmp(argv[iarg],"XML")==0) {
               set_wami_web_reqtype(WAMI_WEB_PRINT_XML);
            }
            else{
               if(strcmp(argv[iarg],"browser")==0) {
                  set_wami_web_reqtype(WAMI_WEB_BROWSER);
               }
               else {
                  if(strcmp(argv[iarg],"struct")==0) {
                     set_wami_web_reqtype(WAMI_WEB_STRUCT);
                  }
                  else {
                     fprintf( stderr, "** Error: option not value for web_atlas_type\n");
                     return(1);
                  }
               }
            }
            ++iarg;
            continue; 
         }

         if (strcmp(argv[iarg],"-dset") == 0) { 
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need dset after -dset\n"); 
               return(1);
            }
            if (srcspace) {
               fprintf( stderr,
               "** Error: Specify space of input (or output mask) with either -space or -dset, not both\n");
               return(1);
            }

            if (!(space_dset = THD_open_dataset (argv[iarg]))) {
               fprintf(stderr,"** Error: Failed to open data set %s.\n",
                       argv[iarg]);
               return(1);
            } 
            srcspace = THD_get_space(space_dset); /* update space if necess*/
            set_out_space(srcspace);   /* make output space for mask dset */
            ++iarg;
            continue; 
         }


         if (strcmp(argv[iarg],"-zone_sort") == 0 || 
             strcmp(argv[iarg],"-radius_sort") == 0) { 
            atlas_sort = 0; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-atlas_sort") == 0 ) { 
            atlas_sort = 1; 
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-alv2") == 0 ) { 
            alv = 2;
            set_TT_whereami_version(alv,wv);
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-wv2") == 0 ) { 
            wv = 2;
            set_TT_whereami_version(alv,wv);
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
               fprintf(stderr,
                        "** Error: Need parameter after -atlas\n"); return(1);
            }
            atlas_names = 
               add_to_names_list (atlas_names, &N_atlas_names, argv[iarg]);
            ++iarg;
            continue; 
         }
         
         if (strcmp(argv[iarg],"-show_atlas_code") == 0) {
            Show_Atlas_Code = 1;
            ++iarg;
            continue; 
         }
         
         if (strcmp(argv[iarg],"-show_atlas_region") == 0 || 
             strcmp(argv[iarg],"-mask_atlas_region") == 0) {
            if (strncmp(argv[iarg],"-mask", 4) == 0) write_mask = 1;
            ++iarg;
            if (iarg >= argc) {
               fprintf( stderr,
                        "** Error: Need parameter after" 
                        "-show_atlas_region/-mask_atlas_region\n"); 
               return(1);
            }            
            shar = argv[iarg];
            ++iarg;
            continue; 
         }
         
         if (strcmp(argv[iarg],"-coord_file") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
                "** Error: Need 1D file with coordinates after -coord_file\n"); 
               return(1);
            }
            coord_file = argv[iarg];
            ++iarg;
            continue;             
         }

         if (strcmp(argv[iarg],"-coord_out") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
                "** Error: Need 1D file with coordinates after -coord_out\n"); 
               return(1);
            }
            coord_outfile = argv[iarg];
            ++iarg;
            continue;             
         }

         if (strcmp(argv[iarg],"-linkrbrain") == 0) {
            ++iarg;
            linkrbrain_output = 1;
            set_AFNI_wami_output_mode(1);
            continue;             
         }

         if (strcmp(argv[iarg],"-linkr_type") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
                "** Error: Need the word \"tasks\" or \"genes\" after linkr_type\n"); 
               return(1);
            }
            if(strcmp(argv[iarg],"tasks")==0)
               linkr_corr_type = 0;
            else {
               if(strcmp(argv[iarg],"genes")==0)
                  linkr_corr_type = 1;
               else{
                  fprintf(stderr,
                   "** Error: Need the word \"tasks\" or \"genes\" after linkr_type\n"); 
                  return(1);
               }
            } 
            ++iarg;
            continue;             
         }

         if (strcmp(argv[iarg],"-linkrbrain_corr_test") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
             "** Error: Need filename for test XML to send"
             " after -linkrbrain_corr_test\n"); 
               return(1);
            }
            linkrbrain_output = 1;
            linkrbrain_corr_test = argv[iarg];
            ++iarg;
            continue;             
         }
         
         
         if (strcmp(argv[iarg],"-max_areas") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -max_areas\n"); 
               return(1);
            }
            N_areas = atoi(argv[iarg]);
            if (N_areas < 1 || N_areas > 50) {
               fprintf(stderr,
                  "** Error: -max_areas parameter must be between 1 and 50.\n"); 
               return(1);
            }
            Set_Whereami_Max_Find(N_areas);
            ++iarg;
            continue;             
         }
         
         if (strcmp(argv[iarg],"-max_search_radius") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
                        "** Error: Need parameter after -max_search_radius\n"); 
               return(1);
            }
            rad = atof(argv[iarg]);
            if (rad < 1.0 || rad > 9.5) {
               fprintf(stderr,
                     "** Error: -max_search_radius parameter must "
                     "be between 1.0 and 9.5.\n"); 
               return(1);
            }
            Set_Whereami_Max_Rad(rad);
            ++iarg;
            continue;             
         } 

         if (strcmp(argv[iarg],"-min_prob") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
                        "** Error: Need parameter after -min_prob\n"); 
               return(1);
            }
            minprob = atof(argv[iarg]);
            if (minprob <= 0.0 || minprob > 1.0) {
               fprintf(stderr,
               "** Error: -min_prob parameter must be greater than 0.0 and less than or equal to 1.0\n"); 
               return(1);
            }
            set_wami_minprob(minprob);
            ++iarg;
            continue;             
         } 

         if (strcmp(argv[iarg],"-dbg") == 0 || 
             strcmp(argv[iarg],"-debug") == 0 ||
             strcmp(argv[iarg],"-verb") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
                     "** Error: Need parameter after -debug|-dbg|-verb\n"); 
               return(1);
            }            
            set_wami_verb(MIN_PAIR(atoi(argv[iarg]), 4));
            ++iarg;
            continue; 
         }
         if (strcmp(argv[iarg],"-prefix") == 0) {
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -prefix\n"); 
               return(1);
            }            
            mskpref = argv[iarg];
            ++iarg;
            continue; 
         }
         
         if (strcmp(argv[iarg],"-bmask") == 0 || 
             strcmp(argv[iarg],"-omask") == 0 ) {
            if (strcmp(argv[iarg],"-bmask") == 0) dobin = 1;
            else dobin = 0;
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,"** Error: Need parameter after -bmask\n"); 
               return(1);
            }
            if (bmsk) {
               fprintf(stderr,
                  "** Error: -bmask and -omask are mutually exclusive.\n"); 
               return(1);
            }            
            bmsk = argv[iarg];
            
            ++iarg;
            continue; 
         }
         
         if( strcmp(argv[iarg],"-cmask") == 0 ){  
            if( iarg+1 >= argc ){
               fprintf(stderr,
                    "** Error: -cmask option requires a following argument!\n");
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
            ATLAS_XFORM_LIST  *xfl, *cxfl;
            ATLAS_SPACE_LIST *asl=NULL;
            /* atlas_xform *xf; */

            init_global_atlas_from_niml_files();
            /* atlas testing */
            xfl = get_xform_chain(asl->space+0,
                                  asl->space+1);
            print_xform_list(xfl);
            cxfl = calc_xform_list(xfl);
            print_xform_list(cxfl);
            free_xform_list(xfl);
            free_xform_list(cxfl);
            free_global_atlas_structs();
            exit(0);
         }

        if( strcmp(argv[iarg],"-show_available_spaces") == 0) {
            read_niml_atlas = 1;
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
                  "** Error: Need src space name after"
                  " -show_available_spaces\n"); 
               return(1);
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
                  "** Error: Need src and dest spaces after -show_chain\n"); 
               return(1);
            }
            srcspace = argv[iarg];
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
                  "** Error: Need src and dest spaces after -show_chain\n"); 
               return(1);
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
                  "** Error: Need src and dest spaces after -calc_chain\n"); 
               return(1);
            }
            srcspace = argv[iarg];
            ++iarg;
            if (iarg >= argc) {
               fprintf(stderr,
                  "** Error: Need src and dest spaces after -calc_chain\n"); 
               return(1);
            }
            destspace = argv[iarg];
            show_xform_chain = 1;
            calc_xform_chain = 1;
            ++iarg;
            continue; 
         }

        if( strcmp(argv[iarg],"-xform_xyz") == 0 ||
            strcmp(argv[iarg],"-xform_xyz_quiet") == 0){
            if (strlen(argv[iarg]) > 12) xform_xyz_quiet = 1;
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
         if(strcmp(argv[iarg],"-show_atlas_point_lists") == 0) {
            iarg++;
            read_niml_atlas = 1;
            show_atlas_point_lists = 1;
            continue ;
         }          
         if(strcmp(argv[iarg],"-show_atlas_all") == 0) {
            iarg++;
            read_niml_atlas = 1;
            show_atlas = 1;
            show_atlas_spaces = 1;
            show_atlas_templates = 1;
            show_atlas_xforms = 1;
            show_atlas_point_lists = 1;
            continue ;
         }          

         if (strcmp(argv[iarg],"-write_hardcode_atlas") ==0){ 
            atlas_writehard = 1; 
            ++iarg;
            continue; 
         }

         if (strcmp(argv[iarg],"-read_atlas_niml") ==0){ 
            atlas_readhard = 1; 
            ++iarg;
            if (!(atlas_dset = THD_open_dataset (argv[iarg]))) {
               fprintf(stderr,"** Error: Failed to open data set %s.\n",
                    argv[iarg]);
               return(1);
            } 
            continue; 
         }
         if (strcmp(argv[iarg],"-html") ==0){ 
            set_AFNI_wami_output_mode(1);
            iarg++;
            continue; 
         }

         { /* bad news in tennis shoes */
            fprintf(stderr,"** Error: bad option %s\n", argv[iarg]);
            suggest_best_prog_option(argv[0], argv[iarg]);
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
   
   /* set verbiage */
   LocalHead = wami_lh();

   /* user wants to see atlas database */
   if(read_niml_atlas) {
      init_global_atlas_from_niml_files();
      if(show_avail_space)
         report_available_spaces(srcspace);
      if(show_xform_chain)
         xfl = report_xform_chain(srcspace, destspace, !xform_xyz_quiet);
      if(calc_xform_chain) {
         cxfl = calc_xform_list(xfl);
         if (!xform_xyz_quiet) {
            print_xform_list(cxfl);/* print the xforms briefly with names only */
            print_all_xforms(cxfl);/* print combined list transforms with data */
         }
      }
      if(xform_xyz) {
         if(!cxfl)
            apply_xform_chain(xfl, xi, yi, zi, &xout, &yout, &zout);
         else
            apply_xform_chain(cxfl, xi, yi, zi, &xout, &yout, &zout);
/*            cxfl = calc_xform_list(xfl);*/
               
         if (xform_xyz_quiet) {
            printf("%f %f %f\n", xout,yout,zout);
         } else {   
            printf("Coords in: %f, %f, %f -> Coords out: %f, %f, %f\n", 
                  xi,yi,zi,xout,yout,zout);
         }
      }

      if (coord_file) { /* load the XYZ coordinates from a 1D file */
         coord_list = make_coord_list(coord_file, &nxyz, dicom);
         wami_xform_coords_print(coord_list, nxyz, srcspace, destspace, coord_outfile);
      }

      if(xfl)
        free_xform_list(xfl);
      if(cxfl)
        free_xform_list(cxfl);
      if(show_atlas)
         print_atlas_table(get_G_atlas_list());
/*         print_atlas_list(get_G_atlas_list());*/
      if(show_atlas_templates)
         print_template_list(get_G_templates_list());
      if(show_atlas_spaces)
         print_space_list(get_G_space_list());
      if(show_atlas_xforms)
         print_all_xforms(get_G_xform_list());
      if(show_atlas_point_lists)
         print_point_lists(get_G_atlas_list());
         
      free_global_atlas_structs(); 
      exit(0);
   }
   
   
   /* write out all the atlases that are hard-coded in AFNI to NIML files */
   if(atlas_writehard) {
      AFNI_atlas_list_to_niml();
      exit(0);
   } 

   /* read all the atlases that are hard-coded in AFNI to NIML files */
   if(atlas_readhard) {
      ATLAS_POINT_LIST *apl;
      apl = dset_niml_to_atlas_list(atlas_dset);
      print_atlas_point_list(apl);
      free_atlas_point_list(apl);
      exit(0);
   } 
   
   if (nakedarg >= 3 && coord_file) {
      /* bad combo */
      fprintf(stderr,"** Error: Can't specify x, y, z "
                     "coordinates on command line AND in coord_file.\n");
      return(1) ;
   }


   if (dicom == -1) {
      THD_coorder cord;
      /* try to set based on AFNI_ORIENT */
      THD_coorder_fill (my_getenv("AFNI_ORIENT"), &cord);
      if (strcmp(cord.orcode,"RAI") == 0) {
         if(!AFNI_wami_output_mode())   
            fprintf(stdout,
             "++ Input coordinates orientation set by default rules to %s\n",
             cord.orcode); 
      }else if (strcmp(cord.orcode,"LPI") == 0) {
         if(!AFNI_wami_output_mode())   
            fprintf(stdout,
             "++ Input coordinates orientation set by default rules to %s\n",
             cord.orcode); 
      }else {
         fprintf(stderr,"** Error: Only RAI or LPI orientations allowed\n"
                        "default setting returned %s\n"
                        "You need to override AFNI_ORIENT \n"
                        "and use either -lpi or -rai\n", cord.orcode);
         return 1;
      }
   } else {
      if(!AFNI_wami_output_mode()){
       if (dicom == 1) 
          fprintf(stdout,"++ Input coordinates orientation set by user to %s\n", 
                      "RAI"); 
       else if (dicom == 0) 
          fprintf(stdout,"++ Input coordinates orientation set by user to %s\n", 
                      "LPI");
       else { fprintf(stderr,"** Error: Should not happen!\n"); return(1); } 
      }
   }

   atlas_alist = get_G_atlas_list(); /* get the whole atlas list */
   if (N_atlas_names == 0) {
      /* use all atlases */
      for (k=0; k<atlas_alist->natlases; ++k) {
         atlas_names = add_to_names_list(atlas_names, &N_atlas_names, 
                                         Atlas_Name(&(atlas_alist->atlas[k])));
      }
   } else {
      /* check for missing atlases and stop in case of error */
      for (k=0; k < N_atlas_names; ++k) {
         if (!get_Atlas_Named(atlas_names[k], atlas_alist)) {
            ERROR_message("Atlas %s not found in list.\n", atlas_names[k]);
            string = suggest_Atlas_Named(atlas_names[k], atlas_alist);
            if (string[0] != '\0') {
               fprintf(stderr,"  Perhaps:      %s  is what you want?\n", string);
            }
            exit(1);
         }  
      }
      atlas_rlist = Atlas_Names_to_List(atlas_names, N_atlas_names);
      if(wami_verb()){
         INFO_message("reduced list of atlases");
         print_atlas_list(atlas_rlist);
      }
   } 
   
   if (!N_atlas_names) {
      ERROR_message("Found no atlases");
      exit(1);
   }
   
   if (nakedarg < 3 && !Show_Atlas_Code && !shar && !bmsk && !coord_file) {
      ERROR_message("Missing useful options. See full help or simple usage below");
      whereami_usage(atlas_alist, 0);
      return 1;
   }
   
   if (LocalHead) Set_Show_Atlas_Mode(LocalHead);

   if (Show_Atlas_Code) {
      for (k=0; k < N_atlas_names; ++k) {
         aa = Build_Atlas(atlas_names[k], atlas_alist);  
         Show_Atlas(aa); 
         aa = Free_Atlas(aa);
      }
   }
   
   if (shar) {
         Set_ROI_String_Decode_Verbosity(1); /* help the user */
         /* Do the match business */
         if (!(aar = ROI_String_Decode(shar, atlas_alist))) {
            ERROR_message("ROI string decoding failed.");
         } else {
            if (LocalHead) { 
               fprintf(stderr,"User seeks the following region in atlas %s:\n", 
                       aar->atlas_name);
               Show_Atlas_Region(aar);  
            }
            /* is this an OK atlas */
            if (!get_Atlas_Named(aar->atlas_name, atlas_alist)) {
               ERROR_message("Atlas %s not found", aar->atlas_name);
               exit(1);
            }
            if (aar->N_chnks < 1 && aar->id <= 0) {
               ERROR_message("bad or empty label");
               exit(1);
            }
            if (!(aa = Build_Atlas(aar->atlas_name, atlas_alist))) {
               ERROR_message("Failed to build atlas");
               exit(1);
            }
           
            if (wami_verb() > 2) Show_Atlas(aa); 
            as = Find_Atlas_Regions(aa,aar, NULL);
            /* analyze the matches, remember no left/right decisions made yet, 
               and even if labels are present, 
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

               if (!(maskset = Atlas_Region_Mask(aar, codes, n_codes,
                                                 atlas_alist))) {
                  ERROR_message("Failed to create mask");
                  exit(1);
               } else {
                  if(!equivalent_space(THD_get_space(maskset))){
                     ERROR_message("Atlas does not match space requested.");
                     exit(1);
                  }
                  tross_Make_History( "whereami" , argc, argv , maskset ) ;
                  if (mskpref) {
                        EDIT_dset_items(  maskset,
                          ADN_prefix    , mskpref,
                           ADN_none ) ;
                  }
                  if( THD_deathcon() && THD_is_file(DSET_HEADNAME(maskset)) ) {
                     ERROR_message("Output dataset %s already exists -- "
                                   "can't overwrite", DSET_HEADNAME(maskset)) ;
                     exit(1);
                  }

                  if (LocalHead) {
                     fprintf(stderr,"Writing ROI mask to %s...\n", 
                              DSET_HEADNAME(maskset));
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
      compute_overlap(bmsk, cmask, ncmask, dobin, N_atlas_names,
         atlas_names, atlas_alist);
   }
   
   if(cmask) free(cmask); cmask = NULL;   /* Should not be needed beyond here */

   
   if (nakedarg < 3 && !coord_file) { /* nothing left to do */
      return(0);
   }

   if (coord_file) { /* load the XYZ coordinates from a 1D file */
      coord_list = make_coord_list(coord_file, &nxyz, dicom);
   }
   else {
      coord_list = (float *)calloc(3, sizeof(float));
      if (!dicom) {
         coord_list[0] = -xi; coord_list[1] =-yi;
      }
      else {
         coord_list[0] = xi; coord_list[1] = yi; 
      }
      coord_list[2] = zi; 
      nxyz = 1;
   }

   
   if (!coord_list) {
      fprintf(stderr,"** Error: No coords!\n");
      return(1) ;
   }
   
#if 0
   /* linkRbrain output */
   if (linkrbrain_output){
     /* if just testing parsing linkrbrain output, just parse and exit*/
     /* for debugging only really */
     if(linkrbrain_corr_test){
        linkrbrain_XML_simple_report(linkrbrain_corr_test, 0);
        exit(0);  
     }

     if(srcspace==NULL) {
        ERROR_message("No source space specified. Use -space or -dset_space\n");
        exit(1);
     }
     if(coord_list==NULL) {
        ERROR_message("No coordinate list given. Use -coord_file\n");
        exit(1);
     }
     /* make XML input coordinate file for linkrbrain (convert to MNI if needed) */
     if(make_linkrbrain_xml(coord_list, nxyz, srcspace, "MNI", linkrbrain_xml,
         linkr_corr_type)!=0) {
         fprintf(stderr,"** Error: could not make XML file for linkrbrain\n");
         exit(1);
     }
     else {
        int lll = send_linkrbrain_xml(linkrbrain_xml, temp_linkrbrain_results) ;
        if( lll != 0 ){
           fprintf(stderr,"** Error: could not link to %s"
                        " -- Check web connections\n",get_linkrbrain_site());
           exit(1);
        }
        else {
        /* parse the output here. For now just say it's there */
           printf(
        "Query of coordinates to %s correlations\n\n",get_linkrbrain_site());
           linkrbrain_XML_simple_report(temp_linkrbrain_results,
              linkr_corr_type);
/*           linkrbrain_str = parse_linkrbrain("linkrbrain.xml");*/
        }
     } 
     exit(0);
   }
#endif

   for (ixyz = 0; ixyz < nxyz; ++ixyz) {
      x = coord_list[3*ixyz];
      y = coord_list[3*ixyz+1];
      z = coord_list[3*ixyz+2];
      
#if 0
      if (!dicom) {
         /* go from lpi to rai */
         x = -x;
         y = -y; 
      }
#endif
      if (!OldMethod) {
         /* the new whereami */
         if (atlas_sort) {
            if (output == 1) TT_whereami_set_outmode (TAB1_WAMI_ATLAS_SORT);
            else TT_whereami_set_outmode (CLASSIC_WAMI_ATLAS_SORT);
         } else {
            if (output == 1) TT_whereami_set_outmode (TAB1_WAMI_ZONE_SORT);
            else TT_whereami_set_outmode (CLASSIC_WAMI_ZONE_SORT);
         }

         set_TT_whereami_version(alv,wv);

         if(!atlas_rlist){
            atlas_list = env_atlas_list();
            if(!atlas_list) {
               atlas_list = atlas_alist;
            }
         }
         else {
            atlas_list = atlas_rlist; /* use reduced list */
            if (wami_verb() >= 2){
               INFO_message("Calling tt_whereami with this reduced"
                            " list of atlases");
               print_atlas_list(atlas_rlist);
            }
         }
         if(space_dset) {
           if (LocalHead) INFO_message("Calling tt_whereami with space_dset");
           string = TT_whereami(x,y,z, 
                                THD_get_space(space_dset), atlas_list);
         } else {
           if (!srcspace)
              srcspace = TT_whereami_default_spc_name();
           if (LocalHead) INFO_message("Calling tt_whereami with srcspace %s",
              srcspace);
           string = TT_whereami(x,y,z, srcspace, atlas_list);
         }
         if (string) fprintf(stdout,"%s\n", string);
         else{
            if(!get_wami_web_found())
               fprintf(stdout,"whereami NULL string out.\n");
         }
         if (string) free(string); string = NULL;            
      }
   } /* ixyz */   
   
   if (coord_list) free(coord_list); coord_list = NULL; 
   
   return 0;
}
/*----------------------------------------------------------------------------*/
/* End whereami main */
/*----------------------------------------------------------------------------*/

int
compute_overlap(char *bmsk, byte *cmask, int ncmask, int dobin,
  int N_atlas_names, char **atlas_names, ATLAS_LIST *atlas_alist)
{
      byte *bmask_vol = NULL, *bba = NULL;
      short *ba = NULL;
      THD_3dim_dataset *mset=NULL, *mset_orig = NULL, *rset = NULL;
      ATLAS *atlas=NULL;
      int isb, nvox_in_mask=0, *count = NULL, dset_kind;
      int *ics=NULL, *unq=NULL, n_unq=0, iroi=0, nonzero, i, k;
      float frac=0.0, sum = 0.0, *fba=NULL;
      char tmps[20];
      
      /* load the mask dset */
      if (!(mset_orig = THD_open_dataset (bmsk))) {
         fprintf(stderr,"** Error: Failed to open mask set %s.\n", bmsk);
         return(1);
      } 
      
      #if 0 /* No longer enforced here. See is_identity_xform_chain below*/
      /* are we in TLRC land? */ 
      if (mset_orig->view_type != VIEW_TALAIRACH_TYPE) {
         fprintf( stderr,
                  "** Error: Mask set %s is not of the Talairach persuasion.\n", 
                  bmsk);
         return(1);
      }
      #endif
      
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
            if ((nonzero = THD_makedsetmask( mset , 0 , 1.0, 0.0 , cmask)) < 0) {               /* get all non-zero values */
              fprintf(stderr,"** Error: No mask for you.\n");
              return(1);
            }
         } else {
            if (unq[iroi] == 0) { /* skip nonesense */
               fprintf(stdout,"++ Skipping unique value of 0\n");
               continue;
            } else {
               fprintf(stdout,
 "++ ========================================================================\n"
                        ) ;
               fprintf(stdout,"++ Processing unique value of %d\n", unq[iroi]);
            }
            mset = EDIT_full_copy(mset_orig, "tmp_ccopy");
            /* turn the mask dataset to zeros and 1s */
            if ((nonzero = 
                     THD_makedsetmask( mset , 0 , (float)unq[iroi], 
                                       (float)unq[iroi] , cmask)) < 0) {  
                  /* get all non-zero values */
                  fprintf(stderr,"** Error: No mask for you either.\n");
                  return(1);
            }
         }
         fprintf(stdout,"++    %d voxels in ROI\n", nonzero);
         
         /* for each atlas */
         for (k=0; k < N_atlas_names; ++k) {
            if (!(atlas = Atlas_With_Trimming(atlas_names[k], 0, atlas_alist))) {
               fprintf(stderr,"** Warning: Atlas %s could not be loaded.\n", 
                               atlas_names[k]);
               continue;
            }
            
            if (!is_identity_xform_chain(THD_get_space(mset_orig), 
                                                atlas->space)) {
               if (wami_verb()) {
                  fprintf(stderr,
            "** Error: Not ready to deal with non-Identity transform chain.\n"
            "Path from input in %s to atlas %s in %s is:\n" , 
                  THD_get_space(mset_orig), 
                  Atlas_Name(atlas), atlas->space);
                  print_xform_chain(THD_get_space(mset_orig), 
                  atlas->space);
               }
               continue;
            } 
            
            if (is_probabilistic_atlas(atlas)) {
               /* not appropriate, skip*/
               continue;
            }
            if (atlas->adh->maxkeyval < 1) {
               if (wami_verb()>=2) 
                  fprintf(stderr,
                     "** Warning: Atlas %s not suitable for this application.\n",
                     Atlas_Name(atlas));
               continue;
            }

            /* resample mask per atlas, use linear interpolation, 
               cut-off at 0.5 */
            rset = r_new_resam_dset (  mset, ATL_DSET(atlas), 0, 0, 0, NULL, 
                                       MRI_LINEAR, NULL, 1, 0);
            if (!rset) {
               fprintf(stderr,"** Error: Failed to reslice!?\n"); return(1);
            }
           /* get byte mask of regions > 0.5 */
            if (!(bmask_vol = THD_makemask( rset , 0 , 0.5 , 2.0 ))) {  
               /* get all non-zero values */
               fprintf(stderr,"** Error: No byte for you.\n");
               return(1);
            }
            nvox_in_mask = 0;
            for (i=0; i<DSET_NVOX(ATL_DSET(atlas)); ++i) {
               if (bmask_vol[i]) ++nvox_in_mask; 
            }
            fprintf( stdout,"++    %d voxels in atlas-resampled mask\n", 
                     nvox_in_mask);
            /* for each sub-brick sb */
            for (isb=0; isb< DSET_NVALS(ATL_DSET(atlas)); ++isb) {
               dset_kind = DSET_BRICK_TYPE(ATL_DSET(atlas),isb);
               if(dset_kind == MRI_short) {
                  ba = DSET_BRICK_ARRAY(ATL_DSET(atlas),isb); /* short type */
                  if (!ba) { 
                     ERROR_message("Unexpected NULL array");
                     free(bmask_vol); bmask_vol = NULL;
                     continue;
                  }
                 /* Create count array for range of integral values in atlas */
                  count = (int *)calloc(atlas->adh->maxkeyval+1, sizeof(int));
                  for (i=0; i<DSET_NVOX(ATL_DSET(atlas)); ++i) {
                     if (bmask_vol[i] && 
                         ba[i] >= atlas->adh->minkeyval) ++count[ba[i]]; 
                  }
               }
               else if(dset_kind == MRI_byte) {
                  bba = DSET_BRICK_ARRAY(ATL_DSET(atlas),isb); /* byte array */
                  if (!bba) { 
                     ERROR_message("Unexpected NULL array");
                     free(bmask_vol); bmask_vol = NULL;
                     continue;
                  }
                 /* Create count array for range of integral values in atlas */
                   count = (int *)calloc(atlas->adh->maxkeyval+1, sizeof(int));
                   for (i=0; i<DSET_NVOX(ATL_DSET(atlas)); ++i) {
                      if (bmask_vol[i] && 
                          bba[i] >= atlas->adh->minkeyval) ++count[bba[i]]; 
                   }
               }
               else if(dset_kind == MRI_float) {
                  fba = DSET_BRICK_ARRAY(ATL_DSET(atlas),isb); /* float array */
                  if (!fba) { 
                     ERROR_message("Unexpected NULL array");
                     free(bmask_vol); bmask_vol = NULL;
                     continue;
                  }
                 /* Create count array for range of integral values in atlas */
                   count = (int *)calloc(atlas->adh->maxkeyval+1, sizeof(int));
                   for (i=0; i<DSET_NVOX(ATL_DSET(atlas)); ++i) {
                      if (bmask_vol[i] && 
                          fba[i] >= atlas->adh->minkeyval) ++count[(int)fba[i]]; 
                   }
               }
               else {
                  ERROR_message(
                     "Atlas %s is not of type short, byte, or float.\n",
                     Atlas_Name(atlas));
                  continue;
               }

               /* Now form percentages */
               if (!unq) {
                  fprintf(stdout,
            "Intersection of ROI (all non-zero values) with atlas %s (sb%d):\n", 
                           Atlas_Name(atlas), isb);
               } else {
                  fprintf(stdout,
            "Intersection of ROI (valued %d) with atlas %s (sb%d):\n", 
                     unq[iroi],
                     Atlas_Name(atlas), isb);
               }
               
               /* sort the count */
               if (!(ics = z_idqsort (count, (atlas->adh->maxkeyval+1) ))) {
                  fprintf(stderr,"** Error: Failed to sort!\n");
                  return(1);
               }

               sum = 0.0;
               for (i=0; i<=atlas->adh->maxkeyval; ++i) {
                  if (count[i]) {
                     if(ics[i]==0) continue; /* don't count codes of 0 */
                     if(strcmp(
                         STR_PRINT(Atlas_Val_Key_to_Val_Name(atlas, ics[i])),
                         "NULL")==0)
                        continue; /* don't count unlabeled codes */
                     frac = (float)count[i]/(float)nvox_in_mask;
                     sum += frac;
                     sprintf(tmps, "%3.1f", frac*100.0); 
                     fprintf(stdout, "   %-5s%% overlap with %s, code %d\n", 
                             tmps, 
                             STR_PRINT(Atlas_Val_Key_to_Val_Name(atlas, ics[i])),
                             ics[i] );
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

      return(0);
}

/* make coordinate list from file */
static float *make_coord_list(char *coord_file, int *nxyz, int dicom)
{
      MRI_IMAGE * XYZ_im=NULL;
      float *XYZv = NULL;
      float *fcoord_list = NULL;
      int ixyz;


   XYZ_im = mri_read_1D( coord_file ) ;
   if( XYZ_im == NULL ){
      fprintf(stderr,"** Error: Can't read XYZ.1D file %s\n",coord_file);
      return(NULL) ;
   }
   if (XYZ_im->ny != 3) {
      fprintf(stderr,"** Error: Need three columns as input.\n"
                     "   Found %d columns\n", XYZ_im->ny);
      return(NULL) ;
   }
   XYZv = MRI_FLOAT_PTR(XYZ_im) ;
   fcoord_list = (float *)calloc(3*XYZ_im->nx, sizeof(float));
   if (!fcoord_list) {
      fprintf(stderr,"** Error: Failed to allocate\n");
      return(NULL) ;
   }
   /* copy to me own vectors */
   *nxyz = XYZ_im->nx;
   for (ixyz=0; ixyz<*nxyz; ++ixyz) {
      if (!dicom) {
         fcoord_list[3*ixyz]   = -XYZv[ixyz];
         fcoord_list[3*ixyz+1] = -XYZv[ixyz+XYZ_im->nx];
      }
      else {
         fcoord_list[3*ixyz]   = XYZv[ixyz];
         fcoord_list[3*ixyz+1] = XYZv[ixyz+XYZ_im->nx];
      } 
      fcoord_list[3*ixyz+2] = XYZv[ixyz+XYZ_im->nx*2];
   }
   mri_free(XYZ_im); XYZ_im = NULL;
 
   return(fcoord_list);
}
