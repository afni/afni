/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

int print_classic_label2index(THD_3dim_dataset * dset, char * labelname);
int print_classic_info       (THD_3dim_dataset * dset, char * dname,
                              int verb, int show_hist);
int validate_field_struct(int verb);

int Syntax(TFORM targ, int detail)
{
   sphinx_printf(targ,"\n"
"Prints out sort-of-useful information from a 3D dataset's header\n"
"Usage: 3dinfo [-verb OR -short] dataset [dataset ...] ~1~\n"
"  -verb means to print out lots of stuff\n"
"  -VERB means even more stuff [including slice time offsets]\n"
"  -short means to print out less stuff [now the default]\n"
"  -no_hist means to omit the HISTORY text\n"
"\n"
"%s"
"\n"
":SPX:"
"\n.. note::\n\n   This could be anything. Just for the demo.\n\n"
":SPX:"
"----------------------------------------------------------------------\n"
"Alternative Usage 1 (without either of the above options): ~1~\n"
"   Output a large block of text per dataset.  This has multiple options:\n"
"\n"
"   -label2index label dataset  : output index corresponding to label ~2~\n"
"\n"
"        example: 3dinfo -label2index aud#0_Coef stats.FT+tlrc\n"
"\n"
"        Prints to stdout the index corresponding to the sub-brick with\n"
"           the name label, or a blank line if label not found.\n"
"        The ONLY output is this sub-brick index.\n"
"        This is intended for used in a script, as in this tcsh fragment:LIT:\n"
"           set face = `3dinfo -label2index Face#0 AA_Decon+orig`\n"
"           set hous = `3dinfo -label2index House#0 AA_Decon+orig`\n"
"           3dcalc -a AA_Decon+orig\"[$face]\" -b AA_Decon+orig\"[$hous]\" ...:LR:\n"
"      * Added per the request and efforts of Colm Connolly.\n"
"\n"
"   -niml_hdr dataset           : output entire NIML-formatted header ~2~\n"
"\n"
"        example: 3dinfo -niml_hdr stats.FT+tlrc\n"
"\n"
"        Prints to stdout the NIML-formatted equivalent of the .HEAD file.\n"
"\n"
"   -subbrick_info dataset      : output only sub-brick part of info ~2~\n"
"\n"
"        example: 3dinfo -subbrick_info stats.FT+tlrc\n"
"\n"
"        Prints to stdout only the part of the full '3dinfo -VERB. output\n"
"        that includes sub-brick info.  The first such line might look like:\n"
"\n"
"           -- At sub-brick #0 'Full_Fstat' datum type is float:  0 to 971.2\n"
"\n"
"----------------------------------------------------------------------\n"
"Alternate Usage 2: ~1~\n"
"  3dinfo <OPTION> [OPTION ..] dataset [dataset ...]\n"
"  Outputs a specific piece of information depending on OPTION.\n"
"  This can form a table of outputs per dataset.\n"
"\n"
"  ==============================================================\n"
"  Options producing one value (string) ~2~\n"
"  ==============================================================\n"
"   -exists: 1 if dset is loadable, 0 otherwise\n"
"            This works on prefix also.\n"
"   -id: Idcodestring of dset\n"
"   -is_labeltable: 1 if dset has a labeltable attached.\n"
"   -is_atlas: 1 if dset is an atlas.\n"
"   -is_atlas_or_labeltable: 1 if dset has an atlas or has a labeltable.\n"
"   -is_nifti: 1 if dset is NIFTI format, 0 otherwise\n"
"   -is_slice_timing_nz: is there slice timing, and is it not uniformly 0\n"
"   -dset_extension: show filename extension for valid dataset (e.g. .nii.gz)\n"
"   -storage_mode: show internal storage mode of dataset (e.g. NIFTI)\n"
"   -space: dataset's space\n"
"   -gen_space: datasets generic space\n"
"   -av_space: AFNI format's view extension for the space\n"
"   -nifti_code: what AFNI would use for an output NIFTI (q)sform_code\n"
"   -is_oblique: 1 if dset is oblique\n"
"   -handedness: L if orientation is Left handed, R if it is right handed\n"
"   -obliquity: Angle from plumb direction.\n"
"               Angles of 0 (or close) are for cardinal orientations\n"
"\n"
"   -prefix: Return the prefix\n"
"   -prefix_noext: Return the prefix without extensions\n"
"   -ni: Return the number of voxels in i dimension\n"
"   -nj: Return the number of voxels in j dimension\n"
"   -nk: Return the number of voxels in k dimension\n"
"   -nijk: Return ni*nj*nk\n"
"   -nv: Return number of points in time or the number of sub-bricks\n"
"   -nt: same as -nv\n"
"   -n3: same as -ni -nj -nk\n"
"   -n4: same as -ni -nj -nk -nv\n"
"   -nvi: The maximum sub-brick index (= nv -1 )\n"
"   -nti: same as -nvi\n"
"   -ntimes: Return number of sub-bricks points in time\n"
"        This is an option for debugging use, stay away from it.\n"
"   -max_node: For a surface-based dset, return the maximum node index\n"
"   -di: Signed displacement per voxel along i direction, aka dx\n"
"   -dj: Signed displacement per voxel along j direction, aka dy\n"
"   -dk: Signed displacement per voxel along k direction, aka dz\n"
"   -d3: same as -di -dj -dk\n"
"   -adi: Voxel size along i direction (abs(di))\n"
"   -adj: Voxel size along j direction (abs(dj))\n"
"   -adk: Voxel size along k direction (abs(dk))\n"
"   -ad3: same as -adi -adj -adk\n"
"   -voxvol: Voxel volume in cubic millimeters\n"
"   -oi: Volume origin along the i direction\n"
"   -oj: Volume origin along the j direction\n"
"   -ok: Volume origin along the k direction\n"
"   -o3: same as -oi -oj -ok\n"
"   -dcx: volumetric center in x direction (DICOM coordinates)\n"
"   -dcy: volumetric center in y direction (DICOM coordinates)\n"
"   -dcz: volumetric center in z direction (DICOM coordinates)\n"
"   -dc3: same as -dcx -dcy -dcz\n"
"   -tr: The TR value in seconds.\n"
"   -dmin: The dataset's minimum value, scaled by fac\n"
"   -dmax: The dataset's maximum value, scaled by fac\n"
"   -dminus: The dataset's minimum value, unscaled.\n"
"   -dmaxus: The dataset's maximum value, unscaled.\n"
"   -smode: Dset storage mode string.\n"
"   -header_name: Value of dset structure (sub)field 'header_name'\n"
"   -brick_name: Value of dset structure (sub)field 'brick_name'\n"
"   -iname: Name of dset as input on the command line\n"
"   -orient: Value of orientation string.\n"
"            For example, LPI means:\n"
"               i direction grows from Left(negative) to Right(positive).\n"
"               j direction grows from Posterior (neg.) to Anterior (pos.)\n"     "               k direction grows from Inferior (neg.) to Superior (pos.)\n"
"   -extent: The spatial extent of the dataset along R, L, A, P, I and S\n"
"   -Rextent: Extent along R\n"
"   -Lextent: Extent along L\n"
"   -Aextent: Extent along P\n"
"   -Pextent: Extent along P\n"
"   -Iextent: Extent along I\n"
"   -Sextent: Extent along S\n"
"   -all_names: Value of various dset structures handling filenames.\n"
"\n"
"  ==============================================================\n"
"  Options producing one value per sub-brick ~2~\n"
"  ==============================================================\n"
"   -fac: Return the float scaling factor\n"
"   -label: The label of each sub-brick\n"
"   -datum: The data storage type\n"
"   -min: The minimum value, scaled by fac\n"
"   -max: The maximum value, scaled by fac\n"
"   -minus: The minimum value, unscaled.\n"
"   -maxus: The maximum value, unscaled.\n"
"\n"
"  ==============================================================\n"
"  Options producing multiple values (strings of multiple lines) ~2~\n"
"  ==============================================================\n"
"   You can specify the delimiter between sub-brick parameters with\n"
"       -sb_delim DELIM. Default DELIM is \"|\"\n"
"   -labeltable: Show label table, if any\n"
"   -labeltable_as_atlas_points: Show label table in atlas point format.\n"
"   -atlas_points: Show atlas points list, if any\n"
"   -history: History note. \n"
"   -slice_timing: Show slice timing. \n"
"\n"
"  ==============================================================\n"
"  Options affecting output format ~2~\n"
"  ==============================================================\n"
"   -header_line: Output as the first line the names of attributes\n"
"                 in each field (column)\n"
"   -hdr: Same as -header_line\n"
"   -sb_delim SB_DELIM: Delimiter string between sub-brick values\n"
"                       Default SB_DELIM is \"|\"\n"
"   -NA_flag NAFLAG: String to use when a field is not found or not\n"
"                    applicable. Default is \"NA\"\n"
"   -atr_delim ATR_DELIM: Delimiter string between attributes\n"
"                         Default ATR_DELIM is the tab character.\n"
"\n"
"  ==============================================================\n"
"  Options for displaying ijk_to_xyz matrices ~2~\n"
"  ==============================================================\n"
"   A set of functions for displaying the matrices that tell us where\n"
"   the data actually is in space!  These 4x4---well 3x4, in practice,\n"
"   because the bottom row of the matrix *must* be (0, 0, 0, 1)---\n"
"   can be related to the NIFTI sform and qform matrices (which are LPI\n"
"   native), but these aform_* matrices are RAI (DICOM) native.\n"
""
"   There are several types of matrices. Linear affine are the most general\n"
"   (containing translation, rotation, shear and scaling info), followed by\n"
"   orthogonal (no shear info; only translation, rotation and scale),\n"
"   followed by cardinal (no rotation info; only translation and scale).\n"
"   The 'scale' info is the voxel sizes. The 'translation' determines the\n"
"   origin location in space.  The 'rotation' describes a, well, rotation\n"
"   relative to the scanner coords---this is the dreaded 'obliquity'. The\n"
"   'shear'... well, that could also be present, but it is not common, at\n"
"   least to describe just-acquired data: it would tilt the axes away from\n"
"   being mutually 90 deg to each other (i.e., they wouldn't be\n"
"   orthogonal); this would likely just result from an alignment process.\n"
""
"   Note: the NIFTI sform can be linear affine, in general; in practice, it\n"
"   is often just orthogonal.  The NIFTI qform is a quaternion representation\n"
"   of the orthogonalized sform; if sform is orthogonal, then they contain\n"
"   the same information (common, but not required).\n"
""
"   The aform_real matrix is AFNI's equivalent of the NIFTI sform; it *can*\n"
"   encode general linear affine mappings. (In practice, it rarely does so.)\n"
"   The aform_orth is the orthogonalized aform_real, and thus equivalent\n"
"   to the NIFTI qform.  If aform_real is orthogonal (no shear info), then\n"
"   these two matrices are equal.  The aform_card is the cardinalized form of\n"
"   the aform_orth;  NIFTI does not have an equivalent.  AFNI typically uses\n"
"   this matrix to display your data on a rectangle that is parallel to your\n"
"   computer screen, without any need to regrid/resample the data (hence, no\n"
"   blurring introduced).  This can be though of displaying your dataset in\n"
"   a way that you *wish* your subject had been oriented.  Note that if\n"
"   there is no obliquity in the acquired data (that is, aform_orth does not\n"
"   contain any rotation relative to the scanner coords), then\n"
"    aform_card == aform_orth.\n"
""
"   The aform_card is an AFNI convenience (ha!) matrix, it does not have an\n"
"   equivalent in the NIFTI stable of matrices.\n"
""
"   -aform_real: Display full 3x4 'aform_real' matrix (AFNI's RAI equivalent\n"
"                of the sform matrix in NIFTI, may contain obliquity info),\n"
"                with comment line first.\n"
"   -aform_real_oneline: Display full 'aform_real' matrix (see '-aform_real')\n"
"                        as 1 row of 12 numbers. No additional comment.\n"
"   -aform_real_refit_ori XXX: Display full 3x4 'aform_real' matrix (see \n"
"                        '-aform_real')\n"
"                        *if* the dset were reoriented (via 3drefit) to\n"
"                        new orient XXX.  Includes comment line first.\n"
"   -is_aform_real_orth: if true, aform_real == aform_orth, which should be\n"
"                        a very common occurrence.\n"
"   -aform_orth: Display full 3x4 'aform_orth' matrix (AFNI's RAI matrix\n"
"                equivalent of the NIFTI quaternion, which may contain\n"
"                obliquity info), with comment line first.\n"
"                This matrix is the orthogonalized form of aform_real,\n"
"                and veeery often AFNI-produced dsets, we will have:\n"
"                aform_orth == aform_real.\n"
"  -perm_to_orient YYY: Display 3x3 permutation matrix to go from the\n"
"                       dset's current orientation to the YYY orient.\n"
"\n"
"  ==============================================================\n"
"  Options requiring dataset pairing at input ~2~\n"
"  ==============================================================\n"
"    3dinfo allows you to make some comparisons between dataset pairs.\n"
"    The comparison is always done in both directions whether or not\n"
"    the answer can be different. For example:\n"
"          3dinfo -same_grid dset1 dset2 \n"
"    will output two values, one comparing dset1 to dset2 and the second\n"
"    comparing dset2 to dset1. With -same_grid, the answers will always\n"
"    be identical, but this might be different for other queries.\n"
"    This behaviour allows you to mix options requiring dataset pairs\n"
"    with those that do not. For example:\n"
"          3dinfo -header_line -prefix -n4 -same_grid \\\n"
"                              DSET1+orig DSET2.nii DSET3.nii DSET4.nii\n"
"\n"
"   -same_grid: Output 1 if the grid is identical between two dsets\n"
"                      0 otherwise. \n"
"               For -same_grid to be 1, all of -same_dim, -same_delta,\n"
"               -same_orient, -same_center, and -same_obl must return 1\n"
"   -same_dim: 1 if dimensions (nx,ny,nz) are the same between dset pairs\n"
"   -same_delta: 1 if voxels sizes are the same between dset pairs\n"
"   -same_orient: 1 if orientation is the same between dset pairs\n"
"   -same_center: 1 if geometric center is the same between dset pairs\n"
"   -same_obl: 1 if obliquity is the same between dset pairs\n"
"   -same_all_grid: Equivalent to listing all of -same_dim -same_delta\n"
"                   -same_orient, -same_center, and -same_obl on the \n"
"                   command line.\n"
"   -val_diff: Output the sum of absolute differences of all voxels in the\n"
"              dataset pair. A -1.0 value indicates a grid mismatch between\n"
"              volume pairs.\n"
"   -sval_diff: Same as -val_diff, but the sum is divided (scaled) by the \n"
"               total number of voxels that are not zero in at least one\n"
"               of the two datasets.\n"
"\n"
"   -monog_pairs: Instead of pairing each dset with the first, pair each\n"
"                couple separately. This requires you to have an even\n"
"                number of dsets on the command line\n"
"\n"
" Examples with csh syntax using datasets in your afni binaries directory ~1~\n"
"\n"
"  0- First get some datasets with which we'll play\n"
"     set dsets = ( `apsearch -list_all_afni_P_dsets` )\n"
"\n"
"  1- The classic\n"
"     3dinfo $dsets[1]\n"
"\n"
"  2- Produce a table of results using 1-value-options for two datasets\n"
"     3dinfo  -echo_edu -prefix_noext -prefix -space -ni -nj -nk -nt  \\\n"
"               $dsets[1-2]\n"
"\n"
"  3- Use some of the options that operate on pairs, mix with other options\n"
"     3dinfo -echo_edu -header_line -prefix -n4 -same_grid $dsets[1-4]\n"
"\n"
"\n",
   SUMA_Offset_SLines(get_help_help(),2)) ;
   PRINT_COMPILE_DATE ; return(0) ;
}

THD_3dim_dataset *load_3dinfo_dataset(char *name)
{
   THD_3dim_dataset *dset = NULL;

   if( !name || name[0] == '\0' ) return(NULL);
   dset = THD_open_dataset( name ) ;

   if( dset == NULL ){  /* open failed */

       /* 23 Jan 2008: try again with +orig, +acpc, +tlrc appended */

       if( strchr(name,'+')==NULL && strstr(name,".nii")==NULL ){
         char str[THD_MAX_NAME] ; int vv , ll=strlen(name) ;
         for( vv=0 ; vv <= LAST_VIEW_TYPE && dset == NULL ; vv++ ){
           strcpy(str,name); if( str[ll-1] == '.' ) str[ll-1] = '\0';
           strcat(str,"+") ; strcat(str,VIEW_codestr[vv]) ;
           dset = THD_open_dataset(str) ;
         }
       }

   }
   return(dset);
}

/*
  [PT: Aug 6, 2024] Important note about adding 3dinfo opts:

  ** As noted below, keep enum INFO_FIELDS in sync with Field Names! **

  This note is probably mostly need for PT, who ignored this sage advice
  at least once in the past.

  Also, on this August date of clarification and correction (thanks,
  JKR for alerting us to the fact that things _had_ gotten out of
  sync), some cleaning of unused+unnecessary items here was done.  For
  example, at option reading time, '-d3' gets mapped to the effective
  triplet '-di -dj -dk', so we don't need a separate D3 or DIJK field
  in INFO_FIELDS (nor a corresponding string in Field_Names). This
  applies for what could be called N3, N4, O3, AD3, etc.
*/
typedef enum { 
   CLASSIC=0, DSET_SPACE, AV_DSET_SPACE, DSET_GEN_SPACE, INFO_NIFTI_CODE,
   IS_NIFTI, DSET_EXISTS,
   DSET_EXTENSION, STORAGE_MODE, /* 4 Jun 2019 [rickr] */
   IS_ATLAS, IS_LABELTABLE, IS_ATLAS_OR_LABELTABLE, 
   IS_OBLIQUE, OBLIQUITY, 
   AFORM_REAL, AFORM_REAL_ONELINE, AFORM_REAL_REFIT_ORI, // [PT: Nov 13, 2020]
   IS_AFORM_REAL_ORTH,
   AFORM_ORTH,                                          // [PT: Nov 14, 2020]
   PERM_TO_ORIENT,                                      // [PT: Nov 23, 2020]
   PREFIX , PREFIX_NOEXT,
   NI, NJ, NK, NT, NTI, NTIMES, MAX_NODE,
   NV, NVI, NIJK,
   DI, DJ, DK,
   OI, OJ, OK, 
   ADI, ADJ, ADK, 
   DCX, DCY, DCZ,
   LTABLE, LTABLE_AS_ATLAS_POINT_LIST, ATLAS_POINTS,
   SLICE_TIMING, IS_SLICE_TIMING_NZ,
   FAC, DATUM, LABEL,
   MIN, MAX, MINUS, MAXUS,
   DMIN, DMAX, DMINUS, DMAXUS,
   TR, HEADER_NAME, BRICK_NAME, ALL_NAMES,
   HISTORY, ORIENT,
   SAME_GRID, SAME_DIM, SAME_DELTA, SAME_ORIENT, SAME_CENTER,
   SAME_OBL, SVAL_DIFF, VAL_DIFF, SAME_ALL_GRID, 
   ID, SMODE,
   VOXVOL, INAME, HANDEDNESS,
   EXTENT_R, EXTENT_L, EXTENT_A, EXTENT_P, EXTENT_I, EXTENT_S, EXTENT,
   N_FIELDS } INFO_FIELDS; /* Keep synchronized with Field_Names
                              Leave N_FIELDS at the end */

char Field_Names[][32]={
   {"-classic-"}, {"space"}, {"AV_spc"}, {"gen_spc"}, {"nifti_code"},
   {"nifti?"}, {"exist?"}, 
   {"exten"}, {"smode"},
   {"atlas?"}, {"lt?"}, {"atlas_or_lt?"}, 
   {"oblq?"}, {"oblq"},
   {"aformr"}, {"aformr_line"}, {"aformr_refit"},
   {"aformr_orth?"}, 
   {"aform_orth"}, 
   {"perm2ori"},
   {"prefix"}, {"pref_nx"},
   {"Ni"}, {"Nj"}, {"Nk"}, {"Nt"}, {"Nti"}, {"Ntimes"}, {"MxNode"},
   {"Nv"}, {"Nvi"}, {"Nijk"},
   {"Di"}, {"Dj"}, {"Dk"}, 
   {"Oi"}, {"Oj"}, {"Ok"}, 
   {"ADi"}, {"ADj"}, {"ADk"}, 
   {"DCx"}, {"DCy"}, {"DCz"},
   {"label_table"}, {"LT_as_atlas_point_list"}, {"atlas_point_list"},
   {"slice_timing"}, {"is_slice_timing_nz"},
   {"factor"}, {"datum"}, {"label"},
   {"min"}, {"max"}, {"minus"}, {"maxus"},
   {"dmin"}, {"dmax"}, {"dminus"}, {"dmaxus"},
   {"TR"}, {"hdr_nm"}, {"brk_nm"}, {"all_nms"},
   {"hist"}, {"orient"},
   {"=grid?"}, {"=dim?"}, {"=delt?"}, {"=ornt?"}, {"=cent?"},
   {"=obl?"}, {"sDval"}, {"Dval"}, {"=dim_delta_orient_center_obl"},
   {"id"}, {"smode"},
   {"voxvol"}, {"iname"}, {"hand"},
   {"Rext"}, {"Lext"},{"Aext"}, {"Pext"}, {"Iext"}, {"Sext"}, {"RLAPIS_ext"},
   {"\0"} }; /* Keep synchronized with INFO_FIELDS */

char *PrintForm(INFO_FIELDS sing , int namelen, byte ForHead)
{
   static char form[5][15];
   static int iret=-1;
   ++iret; if (iret > 4) iret = 0;
   if (ForHead) {
      switch (sing) {
         case PREFIX:
         case PREFIX_NOEXT:
            sprintf(form[iret],"%%%ds", namelen);
            break;
         default:
            sprintf(form[iret],"%%s");
      }
   } else {
      ERROR_message("Not ready for non-header format of %d\n", sing);
      sprintf(form[iret],"F.ERROR:%%s");
   }

   return(form[iret]);
}

#define SPIT_DELIM(ii,N_ii, atrdelim) { \
   if (N_ii > 1 && ii < N_ii-1) fprintf(stdout,"%s",atrdelim);  \
   else fprintf(stdout,"\n"); \
}


int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset=NULL;
   int iarg , verbose = -1 ;
   char *stmp=NULL;
   char *classic_labelName = NULL;
   char *sbdelim = {"|"};
   char *NAflag = {"NA"};
   char *atrdelim = {"\t"}, *form=NULL;
   INFO_FIELDS sing[512];
   int iis=0, N_sing = 0, isb=0, withhead = 0, itmp=0;
   int ip=0, needpair = 0, namelen=0, monog_pairs = 0;
   int classic_niml_hdr = 0;    /* classic: show niml header */
   int classic_subb_info = 0;   /* classic: show sub-brick info */
   int classic_hist=1;          /* include HISTORY text [13 Oct 2022 rickr] */
   THD_3dim_dataset *tttdset=NULL, *dsetp=NULL;
   THD_fvec3 fv = {{-666.0, -666.0, -666.0}};
   char *tempstr = NULL;
   int extinit = 0;
   float RL_AP_IS[6];

   char *aform_real_pbase= "(aform_real)";
   char aform_real_pstr[100];
   char *ocharB_aform_real=NULL;        // new orient for aform_real disp
   mat44 dset_mat44_P;
   char *aform_orth_pbase= "(aform_orth)";
   char aform_orth_pstr[100];

   char *ochar_perm=NULL;        // new orient for perm calc
   char perm_pstr[100];

   mainENTRY("3dinfo main") ; machdep() ;

   strcpy(perm_pstr, ""); 
   strcpy(aform_real_pstr, aform_real_pbase);
   strcpy(aform_orth_pstr, aform_orth_pbase);

   if( argc < 2) { Syntax(TXT,1) ; RETURN(0); }

   iarg = 1 ;
   while (iarg < argc && argv[iarg][0] == '-') {
      CHECK_HELP(argv[iarg],Syntax);
           if( strncmp(argv[iarg],"-verb" ,5) == 0 ){
            verbose =  0; iarg++; continue; }
      else if( strncmp(argv[iarg],"-VERB" ,5) == 0 ){
            verbose =  1; iarg++; continue; }
      else if( strncmp(argv[iarg],"-short",5) == 0 ){
            verbose = -1; iarg++; continue; }
      else if( strncmp(argv[iarg],"-no_hist",5) == 0 ){
            classic_hist = 0; iarg++; continue; }
      else if( strcasecmp(argv[iarg],"-header_line") == 0 ||
               strcasecmp(argv[iarg],"-hdr") == 0 ){
            withhead = 1; iarg++; continue; }
      else if( strcasecmp(argv[iarg],"-monog_pairs") == 0 ){
            monog_pairs = 1; iarg++; continue; }
      /* long-format classic options */
      else if ( strncmp(argv[iarg],"-label2",7) == 0 )
      {
        iarg++;
        if (iarg >= argc)
           ERROR_exit( "3dinfo needs an argument after -label2number\n");
        classic_labelName = malloc(sizeof(char) * 2048);
        strcpy(classic_labelName, argv[iarg]);
        iarg++; continue;
      }
      else if( strcmp(argv[iarg],"-niml_hdr") == 0) {
        classic_niml_hdr = 1;  /* show niml header in classic case */
        iarg++; continue;
      }
      else if( strcmp(argv[iarg],"-subbrick_info") == 0) {
        classic_subb_info = 1;  /* show sub-brick part of info */
        iarg++; continue;
      }
      /* end: long-format classic options */
      else if( strcasecmp(argv[iarg],"-sb_delim") == 0) {
         iarg++;
         if (iarg >= argc)
           ERROR_exit( "3dinfo needs a string after -sb_delim\n");
         sbdelim = argv[iarg];
         iarg++; continue;
      }
      else if( strcasecmp(argv[iarg],"-NA_flag") == 0) {
         iarg++;
         if (iarg >= argc)
           ERROR_exit( "3dinfo needs a string after -NA_flag\n");
         NAflag = argv[iarg];
         iarg++; continue;
      }
      else if( strcasecmp(argv[iarg],"-atr_delim") == 0) {
         iarg++;
         if (iarg >= argc)
           ERROR_exit( "3dinfo needs a string after -atr_delim\n");
         atrdelim = argv[iarg];
         iarg++; continue;
      }
      else if( strcasecmp(argv[iarg],"-space") == 0) {
         sing[N_sing++] = DSET_SPACE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-av_space") == 0) {
         sing[N_sing++] = AV_DSET_SPACE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-gen_space") == 0) {
         sing[N_sing++] = DSET_GEN_SPACE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-nifti_code") == 0) {
         sing[N_sing++] = INFO_NIFTI_CODE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-is_nifti") == 0) {
         sing[N_sing++] = IS_NIFTI; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dset_extension") == 0) {
         sing[N_sing++] = DSET_EXTENSION; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-storage_mode") == 0) {
         sing[N_sing++] = STORAGE_MODE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-is_atlas") == 0) {
         sing[N_sing++] = IS_ATLAS; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-is_labeltable") == 0) {
         sing[N_sing++] = IS_LABELTABLE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-is_atlas_or_labeltable") == 0) {
         sing[N_sing++] = IS_ATLAS_OR_LABELTABLE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-exists") == 0) {
         sing[N_sing++] = DSET_EXISTS; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-is_oblique") == 0) {
         sing[N_sing++] = IS_OBLIQUE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-obliquity") == 0) {
         sing[N_sing++] = OBLIQUITY; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-aform_real") == 0) {
         sing[N_sing++] = AFORM_REAL; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-aform_real_oneline") == 0) {
         sing[N_sing++] = AFORM_REAL_ONELINE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-aform_real_refit_ori") == 0) {
         iarg++;
         if (iarg >= argc)
           ERROR_exit( "3dinfo needs a string after -aform_real_refit_ori\n");
         ocharB_aform_real = argv[iarg];
         sing[N_sing++] = AFORM_REAL_REFIT_ORI; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-is_aform_real_orth") == 0) {
         sing[N_sing++] = IS_AFORM_REAL_ORTH; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-aform_orth") == 0) {
         sing[N_sing++] = AFORM_ORTH; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-perm_to_orient") == 0) {
         iarg++;
         if (iarg >= argc)
           ERROR_exit( "3dinfo needs a string after -perm_to_orient\n");
         ochar_perm = argv[iarg];
         sing[N_sing++] = PERM_TO_ORIENT; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-handedness") == 0) {
         sing[N_sing++] = HANDEDNESS; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-prefix") == 0) {
         sing[N_sing++] = PREFIX; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-prefix_noext") == 0) {
         sing[N_sing++] = PREFIX_NOEXT; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-ni") == 0) {
         sing[N_sing++] = NI; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-nj") == 0) {
         sing[N_sing++] = NJ; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-nk") == 0) {
         sing[N_sing++] = NK; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-n3") == 0) {
         sing[N_sing++] = NI;
         sing[N_sing++] = NJ;
         sing[N_sing++] = NK; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-n4") == 0) {
         sing[N_sing++] = NI;
         sing[N_sing++] = NJ;
         sing[N_sing++] = NK;
         sing[N_sing++] = NV; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-Rextent") == 0) {
         sing[N_sing++] = EXTENT_R; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-Lextent") == 0) {
         sing[N_sing++] = EXTENT_L; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-Aextent") == 0) {
         sing[N_sing++] = EXTENT_A; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-Pextent") == 0) {
         sing[N_sing++] = EXTENT_P; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-Iextent") == 0) {
         sing[N_sing++] = EXTENT_I; iarg++;
         continue;
      }  else if( strcasecmp(argv[iarg],"-Sextent") == 0) {
         sing[N_sing++] = EXTENT_S; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-extent") == 0) {
         sing[N_sing++] = EXTENT_R;
         sing[N_sing++] = EXTENT_L;
         sing[N_sing++] = EXTENT_A;
         sing[N_sing++] = EXTENT_P;
         sing[N_sing++] = EXTENT_I;
         sing[N_sing++] = EXTENT_S;
         iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-di") == 0) {
         sing[N_sing++] = DI; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dj") == 0) {
         sing[N_sing++] = DJ; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dk") == 0) {
         sing[N_sing++] = DK; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-d3") == 0) {
         sing[N_sing++] = DI;
         sing[N_sing++] = DJ;
         sing[N_sing++] = DK; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-adi") == 0) {
         sing[N_sing++] = ADI; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-adj") == 0) {
         sing[N_sing++] = ADJ; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-adk") == 0) {
         sing[N_sing++] = ADK; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-ad3") == 0) {
         sing[N_sing++] = ADI;
         sing[N_sing++] = ADJ;
         sing[N_sing++] = ADK; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-dcx") == 0) {
         sing[N_sing++] = DCX; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dcy") == 0) {
         sing[N_sing++] = DCY; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dcz") == 0) {
         sing[N_sing++] = DCZ; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dc3") == 0) {
         sing[N_sing++] = DCX;
         sing[N_sing++] = DCY;
         sing[N_sing++] = DCZ; iarg++;
         continue;
      } else if( strcasecmp(argv[iarg],"-voxvol") == 0) {
         sing[N_sing++] = VOXVOL; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-iname") == 0) {
         sing[N_sing++] = INAME; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-oi") == 0) {
         sing[N_sing++] = OI; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-oj") == 0) {
         sing[N_sing++] = OJ; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-ok") == 0) {
         sing[N_sing++] = OK; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-o3") == 0) {
         sing[N_sing++] = OI;
         sing[N_sing++] = OJ;
         sing[N_sing++] = OK; iarg++;
         continue;
      }else if( strcasecmp(argv[iarg],"-nt") == 0) {
         sing[N_sing++] = NT; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-nti") == 0) {
         sing[N_sing++] = NTI; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-nv") == 0) {
         sing[N_sing++] = NV; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-nvi") == 0) {
         sing[N_sing++] = NVI; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-ntimes") == 0) {
         sing[N_sing++] = NTIMES; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-max_node") == 0) {
         sing[N_sing++] = MAX_NODE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-nijk") == 0) {
         sing[N_sing++] = NIJK; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-labeltable") == 0) {
         sing[N_sing++] = LTABLE; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-labeltable_as_atlas_points") == 0) {
         sing[N_sing++] = LTABLE_AS_ATLAS_POINT_LIST; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-atlas_points") == 0) {
         sing[N_sing++] = ATLAS_POINTS; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-fac") == 0) {
         sing[N_sing++] = FAC; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-datum") == 0) {
         sing[N_sing++] = DATUM; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-label") == 0) {
         sing[N_sing++] = LABEL; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-min") == 0) {
         sing[N_sing++] = MIN; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-max") == 0) {
         sing[N_sing++] = MAX; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-minus") == 0) {
         sing[N_sing++] = MINUS; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-maxus") == 0) {
         sing[N_sing++] = MAXUS; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dmin") == 0) {
         sing[N_sing++] = DMIN; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dmax") == 0) {
         sing[N_sing++] = DMAX; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dminus") == 0) {
         sing[N_sing++] = DMINUS; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-dmaxus") == 0) {
         sing[N_sing++] = DMAXUS; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-TR") == 0) {
         sing[N_sing++] = TR; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-header_name") == 0) {
         sing[N_sing++] = HEADER_NAME; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-brick_name") == 0) {
         sing[N_sing++] = BRICK_NAME; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-history") == 0) {
         sing[N_sing++] = HISTORY; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-all_names") == 0) {
         sing[N_sing++] = ALL_NAMES; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-orient") == 0) {
         sing[N_sing++] = ORIENT; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-same_grid") == 0) {
         sing[N_sing++] = SAME_GRID; needpair = 1; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-same_dim") == 0) {
         sing[N_sing++] = SAME_DIM; needpair = 1; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-same_delta") == 0) {
         sing[N_sing++] = SAME_DELTA; needpair = 1; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-same_orient") == 0) {
         sing[N_sing++] = SAME_ORIENT; needpair = 1; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-same_center") == 0) {
         sing[N_sing++] = SAME_CENTER; needpair = 1; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-same_obl") == 0) {
         sing[N_sing++] = SAME_OBL; needpair = 1; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-slice_timing") == 0) {
         sing[N_sing++] = SLICE_TIMING; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-is_slice_timing_nz") == 0) {
         sing[N_sing++] = IS_SLICE_TIMING_NZ; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-sval_diff") == 0) {
         sing[N_sing++] = SVAL_DIFF; needpair = 1; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-val_diff") == 0) {
         sing[N_sing++] = VAL_DIFF; needpair = 1; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-same_all_grid") == 0) {
         sing[N_sing++] = SAME_DIM;
         sing[N_sing++] = SAME_DELTA;
         sing[N_sing++] = SAME_ORIENT;
         sing[N_sing++] = SAME_CENTER;
         sing[N_sing++] = SAME_OBL; needpair = 1; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-id") == 0) {
         sing[N_sing++] = ID; iarg++; continue;
      } else if( strcasecmp(argv[iarg],"-smode") == 0) {
         sing[N_sing++] = SMODE; iarg++; continue;
      } else {
         ERROR_message("Option %s unknown", argv[iarg]);
         suggest_best_prog_option(argv[0], argv[iarg]);
         exit(1);
      }
   }

   if (N_sing == 0) {
      sing[N_sing++] = CLASSIC;
   }

   if (sing[iis] == CLASSIC) PRINT_VERSION("3dinfo") ;

   /* be sure field struct matches enum     [26 Jul 2021 rickr] */
   (void)validate_field_struct(verbose);

   THD_allow_empty_dataset(1) ;  /* 21 Mar 2007 */

   if (iarg == argc) {
      ERROR_message("No dsets on command line? I have nothing to do.\n");
      exit(1);
   }

   if (needpair && monog_pairs) needpair = 2; /* pair each couple separately */

   if (needpair==2 && (argc-iarg) % 2) {
      ERROR_message("Using options requiring dset pairs but have odd number\n"
                    "of dsets (%d) on command line.\n", (argc-iarg));
      exit (1);
   } else if (needpair==1 && (argc-iarg) < 2) {
      ERROR_message("Using options requiring dset pairs but have less than\n"
                    "two dsets (%d) on command line.\n", (argc-iarg));
      exit (1);
   }

   ip = 0;
   for( ; iarg < argc ; iarg++ ){
      if (ip == 0) {
         int kkk, nml; char *etr;
         namelen = 0;
         for (kkk=iarg; kkk<argc; ++kkk) {
            if ((etr = THD_trailname(argv[kkk],0))) {
               nml=strlen(etr);
               if (nml < 48 && nml > namelen) namelen = nml;
            }
         }
         if (namelen < 6) namelen = 6;
         if (withhead) {
            int havenew=0;
            for (iis = 0; iis < N_sing; ++iis) {
               if (sing[iis] != CLASSIC) {
                  ++havenew;
                  form = PrintForm(sing[iis], namelen, 1);
                  /*fprintf(stderr,"ZSS: %d %s >%s<\n",
                           sing[iis], Field_Names[sing[iis]], form);*/

                  fprintf(stdout, form, Field_Names[sing[iis]]);
               }
               if (havenew) {
                  if (N_sing > 1 && iis < N_sing-1)
                           fprintf(stdout,"%s",atrdelim);
                  else fprintf(stdout,"\n");
               }
            }
         }
      }
     if( argv[iarg][0] == '\0' ) continue ;  /* bad filename */

     set_obliquity_report(0); /* silence obliquity */

     if (!needpair) {
      if (!(dset = load_3dinfo_dataset(argv[iarg]))) {
        /* exit(1); */
      }
     } else {
      if (needpair == 2) { /* Crazy idea of comparing each pair separately */
         if (ip % 2 == 0) {
            if (!(dset = load_3dinfo_dataset(argv[iarg] ))) {
               /* exit(1); */
            }
            if (iarg+1==argc || argv[iarg+1][0] == '\0') {
               ERROR_message("Bad dset pair for %s\n", argv[iarg]);
               exit(1);
            }
            if (!(dsetp = load_3dinfo_dataset(argv[iarg+1] ))) {
               /* exit(1); */
            }
         } else { /* swap the pair - this allows non pair requiring functions
                     to work as before.*/
            tttdset = dsetp;
            dsetp = dset;
            dset = tttdset; tttdset=NULL;
         }
      } else { /* always compare to very first dset */
         if (ip==0) {
            if (!(dset = load_3dinfo_dataset(argv[iarg] ))) {
               /*exit(1);*/
            }
            if (!(dsetp = load_3dinfo_dataset(argv[iarg+1] ))) {
               /*exit(1);*/
            }
         } else if (ip==1) { /* switch order of first two */
            tttdset = dsetp;
            dsetp = dset; /* now dsetp is the very first dset */
            dset = tttdset; tttdset=NULL;
         } else { /* pair with very first, which is dsetp */
            if (!(dset = load_3dinfo_dataset(argv[iarg] ))) {
               /*exit(1);*/
            }
         }
      }
     }
     ++ip;

     if (0 && !dset) { /* allow for DSET_EXISTS option */
         ERROR_exit("Should not get here");
     }

     /* we should re-capture this per dataset   5 Feb 2019 [rickr] */
     extinit = 0;

     for (iis = 0; iis < N_sing; ++iis) {
        if (!dset) {
         if (sing[iis] == CLASSIC) {
            if( dset == NULL ){  /* still not open? */
               ERROR_exit("Can't open dataset %s\n", argv[iarg]) ;
            }
         } else if (sing[iis] != DSET_EXISTS && sing[iis] != INAME) {
            fprintf(stdout, "NO-DSET");
            SPIT_DELIM(iis, N_sing, atrdelim);
            continue;
         }
        }
        switch (sing[iis]) {
         case CLASSIC:
            /* check for multiple cases under classic unbrella (make
               new cases if this continues)      [10 Oct 2019 rickr] */

            if (classic_labelName != NULL ) { /*** get and output label ***/
               print_classic_label2index(dset, classic_labelName);
            } else if ( classic_niml_hdr ) {
               if( ! THD_write_niml_to_stream(dset, "stdout:", 0) )
                  ERROR_exit("Can't write NIML for dataset %s",argv[iarg]) ;
            } else if ( classic_subb_info ) {
               tempstr = THD_dset_subbrick_info(dset, 0);
               if( tempstr ) {
                  fputs(tempstr, stdout);
                  free(tempstr);
                  tempstr = NULL;
               } else {
                  ERROR_exit("failed subbrick_info for dset %s",argv[iarg]) ;
               }
                
            } else { /*** real CLASSIC: get and output general info ***/
               if( print_classic_info(dset, argv[iarg], verbose, classic_hist) )
                  exit(1);
            }

            THD_delete_3dim_dataset( dset , False ) ;
            break;
         case DSET_EXISTS:
            fprintf(stdout, "%d", dset ? 1:0);
            break;
         case DSET_SPACE:
            tempstr = THD_get_space(dset);
            if(tempstr==NULL)
                  fprintf(stdout, "-----");
            else
                  fprintf(stdout, "%s", tempstr);
            break;
         case DSET_GEN_SPACE:
            tempstr = THD_get_generic_space(dset);
            if(tempstr==NULL)
                  fprintf(stdout, "-----");
            else
                  fprintf(stdout, "%s", tempstr);
            break;
         case INFO_NIFTI_CODE:
            fprintf(stdout,"%d", space_to_NIFTI_code(dset));
            break;
         case AV_DSET_SPACE:
            /* don't allow anything but the three AFNI views */
            tempstr = THD_get_view_space(dset);
            if(tempstr==NULL)
                  fprintf(stdout, "+orig");
            else if (!strncasecmp(tempstr,"ORIG",4))
                  fprintf(stdout, "+orig");
            else if (!strncasecmp(tempstr,"ACPC",4))
                  fprintf(stdout, "+acpc");
            else if (!strncasecmp(tempstr,"TLRC",4))
                  fprintf(stdout, "+tlrc");
            else  /* shouldn't get here */
                  fprintf(stdout, "+orig");
            break;
         case IS_NIFTI:
            if (  dset->dblk->diskptr &&
                  dset->dblk->diskptr->storage_mode == STORAGE_BY_NIFTI ) {
               fprintf(stdout,"1");
            } else {
               fprintf(stdout,"0");
            }
            break;
         case DSET_EXTENSION:
            tempstr = find_filename_extension(argv[iarg]);
            if ( tempstr ) {
               fprintf(stdout,"%s", tempstr);
            } else {
               fprintf(stdout,"-----");
            }
            break;
         case STORAGE_MODE:
            tempstr = storage_mode_name(storage_mode_from_filename(argv[iarg]));
            /* equate UNDEFINED with NULL case */
            if ( tempstr && strcmp(tempstr, "UNDEFINED") ) {
               fprintf(stdout,"%s", tempstr);
            } else {
               fprintf(stdout,"-----");
            }
            break;
         case IS_ATLAS:
            if ( is_Dset_Atlasy(dset, NULL) ) {
               fprintf(stdout,"1");
            } else {
               fprintf(stdout,"0");
            }
            break;
         case IS_LABELTABLE:
            {
               char *str    = NULL;
               int iaol_val = 0;
               if ( (str = Dtable_to_nimlstring(DSET_Label_Dtable(dset),
                                                     "VALUE_LABEL_DTABLE")) ) {
                  // 'else if' for speed
                  iaol_val = 1;
                  free(str);
               } 
               fprintf(stdout,"%d", iaol_val);
            }
            break;
         case IS_ATLAS_OR_LABELTABLE:
            {
               char *str    = NULL;
               int iaol_val = 0;
               if ( is_Dset_Atlasy(dset, NULL) ) {
                  iaol_val = 1;
               }
               else if ( (str = Dtable_to_nimlstring(DSET_Label_Dtable(dset),
                                                     "VALUE_LABEL_DTABLE")) ) {
                  // 'else if' for speed
                  iaol_val = 1;
                  free(str);
               } 
               fprintf(stdout,"%d", iaol_val);
            }
            break;
         case IS_OBLIQUE:
            if (dset_obliquity(dset,NULL) > 0) {
               fprintf(stdout,"1");
            } else {
               fprintf(stdout,"0");
            }
            break;
         case HANDEDNESS:
            if (THD_handedness(dset) > 0) {
               fprintf(stdout,"R");
            } else {
               fprintf(stdout,"L");
            }
            break;
         case OBLIQUITY:
            fprintf(stdout,"%.3f",
                  THD_compute_oblique_angle(dset->daxes->ijk_to_dicom_real, 0));
            break;
         case AFORM_REAL:
            DUMP_MAT44(aform_real_pstr, dset->daxes->ijk_to_dicom_real);
            break;
         case AFORM_REAL_ONELINE:
            DUMP_MAT44_ONELINE(dset->daxes->ijk_to_dicom_real);
            break;
         case AFORM_REAL_REFIT_ORI:
            {
               char ostr[4];
               // the work: 
               dset_mat44_P = THD_refit_orient_ijk_to_dicom_real( dset, 
                                                         ocharB_aform_real ); 
               // display formatting/messaging:
               THD_fill_orient_str_3(dset->daxes, ostr);
               strcat(aform_real_pstr, " after reorienting from (current) ");
               strcat(aform_real_pstr, ostr);
               strcat(aform_real_pstr, " to ");
               strcat(aform_real_pstr, ocharB_aform_real);
               DUMP_MAT44(aform_real_pstr, dset_mat44_P);
               //mat44 TEST;
               //dset_mat44_P.m[0][2] = 1;
               //nifti_orthogonalize_mat44(dset_mat44_P, TEST);
            }
            break;
         case IS_AFORM_REAL_ORTH:
            fprintf(stdout,"%d",
                    is_mat44_orthogonal(dset->daxes->ijk_to_dicom_real));
            break;
         case AFORM_ORTH:
            {
               mat44 aform_orth;
               aform_orth = nifti_orthogonalize_mat44( 
                                             dset->daxes->ijk_to_dicom_real );
               DUMP_MAT44(aform_orth_pstr, aform_orth);
            break;
            }
         case PERM_TO_ORIENT:
            {
               mat33 P33;
               char ostr[4];
               // the work:
               P33 = THD_dset_reorient_perm_mat33(dset, ochar_perm);

               // display formatting/messaging:
               THD_fill_orient_str_3(dset->daxes, ostr);
               strcat(perm_pstr, "Perm from current ");
               strcat(perm_pstr, ostr);
               strcat(perm_pstr, " to ");
               strcat(perm_pstr, ochar_perm);
               DUMP_MAT33b(perm_pstr, P33);
            }
            break;
         case PREFIX:
            form = PrintForm(sing[iis], namelen, 1);
            fprintf(stdout,form, DSET_PREFIX(dset));
            break;
         case PREFIX_NOEXT:
            {
               form = PrintForm(sing[iis], namelen, 1);
               stmp=DSET_prefix_noext(dset);
               fprintf(stdout,form, stmp);
               free(stmp); stmp=NULL;
            }
            break;
         case HEADER_NAME:
            fprintf(stdout,"%s", dset->dblk->diskptr->header_name);
            break;
         case BRICK_NAME:
            fprintf(stdout,"%s", dset->dblk->diskptr->brick_name);
            break;
         case ALL_NAMES:
            THD_show_dataset_names(dset, "FOR_3DINFO", stdout);
            break;
         case HISTORY:
            stmp = tross_Get_History(dset);
            fprintf(stdout,"%s", stmp ? stmp:NAflag);
            if (stmp) free(stmp); stmp=NULL;
            break;
         case NI:
            fprintf(stdout,"%d", DSET_NX(dset));
            break;
         case NJ:
            fprintf(stdout,"%d", DSET_NY(dset));
            break;
         case NK:
            fprintf(stdout,"%d", DSET_NZ(dset));
            break;
         case NIJK:
            fprintf(stdout,"%d", DSET_NVOX(dset));
            break;
         case NTIMES:
            fprintf(stdout,"%d", DSET_NUM_TIMES(dset));
            break;
         case MAX_NODE:
            DSET_MAX_NODE(dset,itmp);
            fprintf(stdout,"%d", itmp);
            break;
         case NT:
         case NV:
            fprintf(stdout,"%d", DSET_NVALS(dset));
            break;
         case NTI:
         case NVI:
            fprintf(stdout,"%d", DSET_NVALS(dset)-1);
            break;
         case DI:
            fprintf(stdout,"%f", DSET_DX(dset));
            break;
         case DJ:
            fprintf(stdout,"%f", DSET_DY(dset));
            break;
         case DK:
            fprintf(stdout,"%f", DSET_DZ(dset));
            break;
         case OI:
            fprintf(stdout,"%f", DSET_XORG(dset));
            break;
         case OJ:
            fprintf(stdout,"%f", DSET_YORG(dset));
            break;
         case OK:
            fprintf(stdout,"%f", DSET_ZORG(dset));
            break;
         case ADI:
            fprintf(stdout,"%f", fabs(DSET_DX(dset)));
            break;
         case DCX:
            /* modular but inefficient, get C? each time */
            fv = THD_dataset_center(dset);
            fprintf(stdout,"%f", fv.xyz[0]);
            break;
         case DCY:
            /* modular but inefficient, get C? each time */
            fv = THD_dataset_center(dset);
            fprintf(stdout,"%f", fv.xyz[1]);
            break;
         case DCZ:
            /* modular but inefficient, get C? each time */
            fv = THD_dataset_center(dset);
            fprintf(stdout,"%f", fv.xyz[2]);
            break;
         case EXTENT_R:
         case EXTENT_L:
         case EXTENT_A:
         case EXTENT_P:
         case EXTENT_I:
         case EXTENT_S:
            {
               if (!extinit) {
                  THD_dset_extent(dset, '-', RL_AP_IS);
                  extinit = 1;
               }
               fprintf(stdout,"%f", RL_AP_IS[sing[iis]-EXTENT_R]);
            }
            break;

         case ADJ:
            fprintf(stdout,"%f", fabs(DSET_DY(dset)));
            break;
         case ADK:
            fprintf(stdout,"%f", fabs(DSET_DZ(dset)));
            break;
         case VOXVOL:
            fprintf(stdout,"%f", fabs(DSET_DX(dset))*
                                 fabs(DSET_DY(dset))*fabs(DSET_DZ(dset)));
            break;
         case INAME:
            fprintf(stdout,"%s", argv[iarg]);
            break;
         case LTABLE:
            {
               char *str;
               if ((str = Dtable_to_nimlstring(DSET_Label_Dtable(dset),                                                          "VALUE_LABEL_DTABLE"))) {
                  fprintf(stdout,"%s", str);
                  free(str);
               } else {
                  fprintf(stdout,"NO_LABEL_TABLE");
               }
            }
            break;
         case LTABLE_AS_ATLAS_POINT_LIST:
            {
               ATLAS_POINT_LIST *apl=NULL;
               if ((apl =
                     label_table_to_atlas_point_list(DSET_Label_Dtable(dset)))) {
                  atlas_list_to_niml(apl,NULL);
                  free_atlas_point_list(apl);
               } else {
                  fprintf(stdout,"NO_LABEL_TABLE");
               }
            }
            break;
         case  ATLAS_POINTS:
            {
               ATR_string *atr =
                  THD_find_string_atr( dset->dblk, "ATLAS_LABEL_TABLE");
               if (atr) {
                  fprintf(stdout,"%s", atr->ch);
               }  else {
                  fprintf(stdout,"NO_APL");
               }
            }
            break;
         case FAC:
            {
               for (isb=0; isb<DSET_NVALS(dset); ++isb) {
                  fprintf(stdout,"%f%s",
                        DSET_BRICK_FACTOR(dset,isb),
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
               }
               break;
            }
         case DATUM:
            {
               for (isb=0; isb<DSET_NVALS(dset); ++isb) {
                  fprintf(stdout,"%s%s",
                        MRI_TYPE_name[DSET_BRICK_TYPE(dset,isb)],
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
               }
               break;
            }
         case LABEL:
            {
               for (isb=0; isb<DSET_NVALS(dset); ++isb) {
                  fprintf(stdout,"%s%s",
               DSET_BRICK_LABEL(dset,isb) ? DSET_BRICK_LABEL(dset,isb):NAflag,
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
               }
               break;
            }
         case MIN:
         case MINUS:
         case MAX:
         case MAXUS:
            {
               float vv=0.0, min, max;
               for (isb=0; isb<DSET_NVALS(dset); ++isb) {
                  if (!THD_subbrick_minmax(dset, isb,
                        (sing[iis] == MINUS || sing[iis] == MAXUS) ? 0:1,
                        &min, &max)) {
                     fprintf(stdout,"%s%s",
                        NAflag,
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
                  } else {
                          if (sing[iis] == MINUS)
                        vv = min;
                     else if (sing[iis] == MAXUS)
                        vv = max;
                     else if (sing[iis] == MIN)
                        vv = min;
                     else if (sing[iis] == MAX)
                        vv = max;
                     fprintf(stdout,"%g%s",
                        vv,
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
                  }
               }
               break;
            }
         case DMIN:
         case DMINUS:
         case DMAX:
         case DMAXUS:
            {
               float vv=0.0, min, max;
               if (!THD_dset_minmax(dset,
                     (sing[iis] == DMINUS || sing[iis] == DMAXUS) ? 0:1,
                     &min, &max)) {
                  fprintf(stdout,"%s%s",
                     NAflag,
                     (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
               } else {
                       if (sing[iis] == DMINUS)
                     vv = min;
                  else if (sing[iis] == DMAXUS)
                     vv = max;
                  else if (sing[iis] == DMIN)
                     vv = min;
                  else if (sing[iis] == DMAX)
                     vv = max;
                  fprintf(stdout,"%g%s",
                     vv,
                     (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
               }
               break;
            }
         case TR:
#if 0
            fprintf(stdout,"%f", DSET_TR_SEC(dset));
#else
            fprintf(stdout,"%f", DSET_TR(dset));
#endif
            break;
         case ORIENT:
            {
               /* fprintf(stdout,"%c%c%c",
                *         ORIENT_typestr[dset->daxes->xxorient][0], ... ); */
               char ostr[4];    /* just to show        23 Jan 2013 [rickr] */
               THD_fill_orient_str_3(dset->daxes, ostr);
               fprintf(stdout,"%3s", ostr);
            }
            break;
         case SAME_GRID:
            fprintf(stdout,"%d",
               !THD_dataset_mismatch( dset , dsetp ));
            break;
         case SAME_DIM:
            fprintf(stdout,"%d",
               !(THD_dataset_mismatch( dset , dsetp ) & MISMATCH_DIMEN));
            break;
         case SAME_DELTA:
            fprintf(stdout,"%d",
               !(THD_dataset_mismatch( dset , dsetp ) & MISMATCH_DELTA));
            break;
         case SAME_ORIENT:
            fprintf(stdout,"%d",
               !(THD_dataset_mismatch( dset , dsetp ) & MISMATCH_ORIENT));
            break;
         case SAME_CENTER:
            fprintf(stdout,"%d",
               !(THD_dataset_mismatch( dset , dsetp ) & MISMATCH_CENTER));
            break;
         case SAME_OBL:
            fprintf(stdout,"%d",
               !(THD_dataset_mismatch( dset , dsetp ) & MISMATCH_OBLIQ));
            break;
         case SLICE_TIMING:     /* 6 May 2013 [rickr] */
            {
               if( DSET_HAS_SLICE_TIMING(dset) ) {
                  DSET_UNMSEC(dset); /* make sure times are in seconds */
                  for (isb=0; isb<dset->taxis->nsl; ++isb) {
                     fprintf(stdout,"%s%f",
                           (isb > 0) ? sbdelim : "",
                           dset->taxis->toff_sl[isb]);
                  }
               } else { /* all slices times are at t=0.0 */
                  for (isb=0; isb<DSET_NZ(dset); ++isb) {
                     fprintf(stdout,"%s%f", (isb > 0) ? sbdelim : "", 0.0);
                  }
               }
            }
            break;
         case IS_SLICE_TIMING_NZ:     /* 15 Aug 2024 [pt] */
            {
               int istnz = 0;
               // first, *is* there slice timing information?
               if( DSET_HAS_SLICE_TIMING(dset) ) {
                  DSET_UNMSEC(dset); /* make sure times are in seconds */
                  // then, verify having >=1 nonzero slice_timing value
                  for (isb=0; isb<dset->taxis->nsl; ++isb) 
                     if ( dset->taxis->toff_sl[isb] )
                        istnz = 1;
               } 
               fprintf(stdout,"%d", istnz);
            }
            break;
         case SVAL_DIFF:
            fprintf(stdout,"%f",THD_diff_vol_vals(dset, dsetp, 1));
            break;
         case VAL_DIFF:
            fprintf(stdout,"%f",THD_diff_vol_vals(dset, dsetp, 0));
            break;
         case ID:
            fprintf(stdout,"%s", DSET_IDCODE_STR(dset));
            break;
         case SMODE:
            fprintf(stdout,"%s", DSET_STORAGE_MODE_STR(dset));
            break;
         default:
            ERROR_message("Info field not set properly (%d)\n", sing[iis]);
            exit(1);
        }
        if (sing[iis] != CLASSIC) {
         SPIT_DELIM(iis, N_sing, atrdelim);
        }
      }
   }

   if( classic_labelName ) free(classic_labelName);

   exit(0) ;
}

/* validate Field_Names against INFO_FIELDS enum    [26 Jul 2021 rickr] */
int validate_field_struct(int verb)
{
   INFO_FIELDS   nfields = N_FIELDS;
   int           nfcount=-1, nfsize=-1, mismatch;

   ENTRY("validate_field_struct");

   /* only validate and warn in verbose mode, which defaults to -1 */
   if( verb < 0 )
      RETURN(0);

   /* compute nf by size, given that these are 32 char strings */
   /* (subtract 1 for the final empty string)                  */
   nfsize = (int)(sizeof(Field_Names)/sizeof(char[32])) - 1;


   for( nfcount = 0; Field_Names[nfcount][0]; nfcount++ )
      ;

   /* note mismatch before continuing */
   mismatch = nfields != nfcount || nfields != nfsize;

   /* possibly be verbose */
   if ( verb > 0 || mismatch ) {
      printf("++ checking INFO_FIELDS against Field_Names\n");
      printf("   N_FIELDS     = %d\n", nfields);
      printf("   nfcount      = %d\n", nfcount);
      printf("   nfsize       = %d\n", nfsize);
   }

   if( mismatch ) {
      fprintf(stderr,"** warning: INFO_FIELDS/Field_Names mismatch\n");
      RETURN(1);
   }

   RETURN(0);
}

/* try to print the index of the specified label */
int print_classic_label2index(THD_3dim_dataset * dset, char * labelname)
{
   int nval_per;
   int foundLabel = 0;
   int ival=0;

   ENTRY("print_classic_labels");

   if( ! dset || ! labelname ) {
      ERROR_message("classic label2index: missing dset or label");
      RETURN(1);
   }

   nval_per = DSET_NVALS(dset);

   for (ival=0 ; ival < nval_per && !foundLabel; ival++ ) {
      if (strcmp(DSET_BRICK_LAB(dset,ival), labelname) == 0) {
         printf("%d\n", ival); foundLabel = 1;
      }
   }
   if (!foundLabel) printf("\n");

   RETURN(0);
}

/* try to print the index of the specified label */
int print_classic_info(THD_3dim_dataset * dset, char * dname, int verb,
                       int show_hist)
{
   char * outbuf;
   ENTRY("print_classic_info");

   outbuf = THD_dataset_info( dset , verb , show_hist ) ;
   if( outbuf != NULL ){
      printf("\n") ;
      puts(outbuf) ;
      free(outbuf) ; outbuf = NULL ;
      RETURN(0);
   } else {
      ERROR_message("Can't get info for dataset %s", dname);
      RETURN(1);
   }
}

